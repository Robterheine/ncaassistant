# ============================================================================
# NCA Assistant — Bioequivalence model fit and verdict
# ============================================================================
# Shiny-free, so the BE statistics can be tested directly by
# validation/validation.R instead of through a hand-maintained copy.
# mod_path_be.R builds be_data with build_be_data() (one row per NCA profile,
# merged with the design columns) and calls fit_be_parameter() once per PK
# parameter.

#' Attach design columns to the NCA result, one row per profile
#'
#' The NCA result has one row per profile, identified by Subject, Treatment
#' and (when mapped) Period. The merge uses exactly those keys, so a replicate
#' design keeps one row per administration: merging on Subject + Treatment
#' alone would pair each administration with every period of that treatment
#' and enter every NCA value more than once.
#'
#' @param nca_res NCA result from run_nca() (Subject, Treatment[, Period])
#' @param pk_data The uploaded data used for the NCA
#' @param col_map Column mapping
#' @param reference The Reference treatment, as written in the data. When
#'   NULL, a level named "Reference" is used if present, otherwise the first
#'   level alphabetically (which may be the Test: callers should pass it).
#' @param covariates NULL, or the columns of pk_data to use as baseline
#'   covariates in a parallel-group analysis: a character vector of column
#'   names, or a data.frame(name, type, transform) with type "auto",
#'   "numeric" or "categorical" and transform "none" or "log". See
#'   be_covariate_prepare().
#' @return list(data, trt_col, subj_col, per_col, seq_col, covariates);
#'   Treatment is a factor with the reference as its first level. covariates
#'   is the resolved specification (be_covariate_prepare()) or NULL; its
#'   values sit in the .cov1, .cov2, ... columns of data.
build_be_data <- function(nca_res, pk_data, col_map, reference = NULL, exclusions = NULL,
                          covariates = NULL) {
  keys <- intersect(c("Subject", "Treatment", "Period"), names(nca_res))
  if (!all(c("Subject", "Treatment") %in% keys))
    stop("The NCA result has no Subject/Treatment columns; map the Treatment column.")
  src <- c(Subject = col_map$subject, Treatment = col_map$treatment,
           Period = if ("Period" %in% keys) col_map$period)

  design <- data.frame(lapply(src[keys], function(cc) as.character(pk_data[[cc]])),
                       stringsAsFactors = FALSE)
  names(design) <- keys
  seq_col <- NULL
  if (!is.null(col_map$sequence) && col_map$sequence %in% names(pk_data)) {
    seq_col <- if (col_map$sequence %in% c(keys, names(nca_res))) ".Sequence" else col_map$sequence
    design[[seq_col]] <- pk_data[[col_map$sequence]]
  }
  design <- unique(design)

  if (anyDuplicated(design[keys])) {
    stop("The Sequence column is not constant within a profile (",
         paste(keys, collapse = " x "), "). Check the Sequence column.")
  }
  issues <- design_identity_issues(pk_data, col_map)
  if (length(issues) > 0) stop(issues[[1]]$message, ". ", issues[[1]]$detail, " ", issues[[1]]$action)

  # Key columns are character on both sides: NCA keys are always character,
  # while uploaded Subject/Period columns are usually integer. A type mismatch
  # would leave Period all-NA after the merge and lm() would drop every row.
  nca_res[keys] <- lapply(nca_res[keys], as.character)
  # Every profile in the data gets a row. A profile without an NCA result
  # (no measurable concentration, e.g. a non-absorber) keeps a row
  # with missing parameters, so it is counted as missing instead of leaving
  # the comparison unseen (ICH M13A 2.2.1.1 allows that only as an exception).
  be <- merge(nca_res, design, by = keys, all = TRUE, sort = FALSE)
  if (nrow(be) != nrow(design))
    stop("Design merge changed the number of profiles (", nrow(design), " -> ",
         nrow(be), "). Check the Treatment, Period and Sequence columns.")

  be$Treatment <- factor(be$Treatment)
  if (!is.null(reference) && nzchar(reference)) {
    if (!reference %in% levels(be$Treatment))
      stop("The Reference treatment '", reference, "' is not in the Treatment column (found: ",
           paste(levels(be$Treatment), collapse = ", "), ").")
    be$Treatment <- relevel(be$Treatment, ref = reference)
  } else if ("Reference" %in% levels(be$Treatment)) {
    be$Treatment <- relevel(be$Treatment, ref = "Reference")
  }

  # Profiles the analyst excluded (EXCL = 1 in the NCA result) carry their
  # reason; fit_be_parameter() leaves them out and counts them separately
  be$EXCLUDED <- NA_character_
  if ("EXCL" %in% names(be)) {
    x <- suppressWarnings(as.numeric(be$EXCL)) %in% 1
    ex <- active_exclusions(exclusions); ex <- ex[ex$level == "profile", , drop = FALSE]
    reason <- vapply(which(x), function(i) {
      m <- ex$subject == be$Subject[i] &
        (if ("Treatment" %in% keys) ex$treatment %in% as.character(be$Treatment[i]) else TRUE) &
        (if ("Period" %in% keys) ex$period %in% be$Period[i] else TRUE)
      if (any(m)) paste(na.omit(c(ex$category[m][1], ex$detail[m][1])), collapse = ": ") else "excluded"
    }, character(1))
    be$EXCLUDED[x] <- reason
  }
  cov_spec <- NULL
  if (!is.null(covariates) && NROW(covariates) > 0) {
    cp <- be_covariate_prepare(pk_data, col_map, covariates)
    cov_spec <- cp$spec
    for (i in seq_len(nrow(cov_spec)))
      be[[cov_spec$col[i]]] <- cp$values[[cov_spec$col[i]]][match(as.character(be$Subject), cp$subject)]
  }
  list(data = be, trt_col = "Treatment", subj_col = "Subject",
       per_col = if ("Period" %in% keys) "Period" else NULL, seq_col = seq_col,
       covariates = cov_spec)
}

#' Read baseline covariates from the uploaded data, one value per subject
#'
#' Covariates are columns of the uploaded data that are not mapped to another
#' role. Numeric ones are used as they are (or as ln, when transform is
#' "log"); categorical ones become a factor. The function stops, in plain
#' words, when a column cannot be used as it stands: a role column, a column
#' that is missing, text that looks like numbers ("12 kg", "1,5"), two
#' different values for one subject, or a log of a value at or below zero.
#' Missing values are kept here and refused, per parameter, by
#' be_covariate_checks(): only the subjects who enter the comparison count.
#'
#' @param pk_data,col_map The uploaded data and its column mapping
#' @param covariates Character vector of column names, or data.frame(name,
#'   type, transform)
#' @return list(spec = data.frame(name, type, transform, col) with the
#'   resolved type, subject = character subject IDs, values = named list of
#'   per-subject vectors keyed by spec$col)
be_covariate_prepare <- function(pk_data, col_map, covariates) {
  spec <- if (is.data.frame(covariates)) covariates else data.frame(name = as.character(covariates))
  if (!"type" %in% names(spec)) spec$type <- "auto"
  if (!"transform" %in% names(spec)) spec$transform <- "none"
  spec$name <- as.character(spec$name); spec$type <- as.character(spec$type)
  spec$transform <- as.character(spec$transform)
  if (anyDuplicated(spec$name)) stop("A covariate is listed twice: ", spec$name[anyDuplicated(spec$name)], ".")
  if (is.null(col_map$subject) || !col_map$subject %in% names(pk_data))
    stop("Covariates need the Subject column to be mapped.")
  roles <- unlist(col_map[!vapply(col_map, is.null, logical(1))], use.names = FALSE)
  subj <- as.character(pk_data[[col_map$subject]])
  ids <- unique(subj)
  values <- list()
  for (i in seq_len(nrow(spec))) {
    nm <- spec$name[i]
    if (!nm %in% names(pk_data)) stop("The covariate column '", nm, "' is not in the data.")
    if (nm %in% roles)
      stop("The column '", nm, "' is mapped to another role (Subject, Treatment, Period, Sequence, time, ",
           "concentration or dose) and cannot be a covariate.")
    if (!spec$type[i] %in% c("auto", "numeric", "categorical"))
      stop("The type of covariate '", nm, "' must be auto, numeric or categorical.")
    if (!spec$transform[i] %in% c("none", "log"))
      stop("The transform of covariate '", nm, "' must be none or log.")
    x <- pk_data[[nm]]
    chr <- trimws(as.character(x)); chr[!nzchar(chr) | toupper(chr) %in% c("NA", "NAN")] <- NA
    type <- spec$type[i]
    if (is.numeric(x)) {
      if (type == "auto") type <- "numeric"
    } else if (type != "categorical") {
      nn <- chr[!is.na(chr)]
      num <- suppressWarnings(as.numeric(nn))
      if (any(grepl("^[-+]?[0-9]+,[0-9]+$", nn)))
        stop("The covariate '", nm, "' uses a decimal comma (for example ", nn[grepl(",", nn)][1],
             "). Write decimals with a point and upload again.")
      if (length(nn) > 0 && all(!is.na(num))) {
        if (type == "auto") type <- "numeric"
      } else if (length(nn) > 0 && (type == "numeric" || (type == "auto" && any(!is.na(num)) ||
                 type == "auto" && all(grepl("^[-+]?[0-9.]+ ?[A-Za-z%/]+", nn))))) {
        bad <- nn[is.na(num)]
        stop("The covariate '", nm, "' mixes numbers and text (for example '", bad[1], "'). ",
             "Keep the number only, and put the unit in the column name. To use it as groups, ",
             "set its type to categorical.")
      } else if (type == "auto") type <- "categorical"
    }
    v <- if (type == "numeric") suppressWarnings(as.numeric(if (is.numeric(x)) x else chr)) else chr
    if (type == "numeric" && any(is.infinite(v), na.rm = TRUE))
      stop("The covariate '", nm, "' has infinite values.")
    # One value per subject: a different value in another row is an error
    per <- tapply(seq_along(v), subj, function(j) unique(v[j][!is.na(v[j])]))
    multi <- names(per)[vapply(per, length, integer(1)) > 1]
    if (length(multi) > 0)
      stop("The covariate '", nm, "' has more than one value for subject(s) ", paste(head(multi, 5), collapse = ", "),
           if (length(multi) > 5) paste0(" and ", length(multi) - 5, " more") else "",
           ". A baseline covariate has one value per subject.")
    one <- vapply(ids, function(id) { u <- per[[id]]; if (length(u)) u[[1]] else NA }, v[NA_integer_[1]][1])
    if (type == "numeric" && spec$transform[i] == "log") {
      if (any(one <= 0, na.rm = TRUE))
        stop("The covariate '", nm, "' has values at or below zero, which have no logarithm. ",
             "Use it as it is, or leave the log transform off.")
      one <- log(one)
    }
    if (type == "categorical" && spec$transform[i] == "log")
      stop("The covariate '", nm, "' is categorical; the log transform applies to numbers only.")
    spec$type[i] <- type
    values[[paste0(".cov", i)]] <- if (type == "categorical") factor(unname(one)) else unname(one)
  }
  spec$col <- paste0(".cov", seq_len(nrow(spec)))
  list(spec = spec, subject = ids, values = values)
}

#' Limits on covariate adjustment
BE_COVARIATE_MAX <- 5L
BE_COVARIATE_MIN_DF <- 10L
BE_COVARIATE_MIN_PER_GROUP <- 12L

#' Can this covariate set be fitted for the subjects in the comparison?
#'
#' Run on the profiles that carry a value for the parameter (after
#' exclusions). Errors stop the run; warnings are shown with the result.
#' Rules: parallel design only; at most 5 covariates; no missing value (the
#' subjects are named: nobody is dropped silently); no constant covariate; no
#' covariate that is aliased with Treatment or with another covariate; at
#' least 12 subjects per group and residual df of at least 10; a warning for a
#' category with fewer than 3 subjects.
#'
#' @param be_data Data frame from build_be_data()$data
#' @param spec The resolved covariate specification (build_be_data()$covariates)
#' @param rows Logical vector: the profiles that enter the comparison
#' @return list(errors = character(), warnings = character())
be_covariate_checks <- function(be_data, spec, trt_col, subj_col, rows = rep(TRUE, nrow(be_data)),
                                design = "parallel") {
  errors <- character(0); warns <- character(0)
  if (is.null(spec) || nrow(spec) == 0) return(list(errors = errors, warnings = warns))
  if (be_design_model(design) != "parallel")
    return(list(errors = "Covariates are for parallel-group studies only. In a crossover the Subject term already absorbs any characteristic of the subject.",
                warnings = warns))
  if (nrow(spec) > BE_COVARIATE_MAX)
    errors <- c(errors, sprintf("At most %d covariates can be used (%d selected).", BE_COVARIATE_MAX, nrow(spec)))
  d <- be_data[rows, , drop = FALSE]
  trt <- droplevels(factor(d[[trt_col]]))
  for (i in seq_len(nrow(spec))) {
    nm <- spec$name[i]; v <- d[[spec$col[i]]]
    miss <- is.na(v)
    if (any(miss)) {
      who <- unique(as.character(d[[subj_col]][miss]))
      errors <- c(errors, paste0("The covariate '", nm, "' has no value for ", length(who), " subject(s): ",
        paste(head(who, 10), collapse = ", "), if (length(who) > 10) paste0(" and ", length(who) - 10, " more") else "",
        ". Complete the data or leave that covariate out. Nobody is dropped silently."))
      next
    }
    if (length(unique(v)) < 2) {
      errors <- c(errors, paste0("The covariate '", nm, "' has the same value for every subject, so it cannot adjust anything."))
    } else if (spec$type[i] == "categorical") {
      tab <- table(v)
      small <- names(tab)[tab < 3]
      if (length(small) > 0)
        warns <- c(warns, paste0("The covariate '", nm, "' has category ", paste(small, collapse = ", "),
                                 " with fewer than 3 subjects; its estimate is unstable."))
    }
  }
  n_grp <- table(trt)
  if (length(n_grp) == 2 && any(n_grp < BE_COVARIATE_MIN_PER_GROUP))
    errors <- c(errors, sprintf("Covariate adjustment needs at least %d subjects per group (found %s).",
                                BE_COVARIATE_MIN_PER_GROUP, paste(names(n_grp), n_grp, sep = ": ", collapse = ", ")))
  if (length(errors) == 0) {
    dd <- d[, spec$col, drop = FALSE]; dd$.trt <- trt
    X <- tryCatch(stats::model.matrix(stats::as.formula(paste("~ .trt +", paste(spec$col, collapse = " + "))), dd),
                  error = function(e) NULL)
    if (is.null(X)) {
      errors <- c(errors, "The covariates could not be turned into a model. Check for categories with a single subject.")
    } else {
      rk <- qr(X)$rank
      if (rk < ncol(X)) {
        no_trt <- X[, !grepl("^\\.trt", colnames(X)), drop = FALSE]
        errors <- c(errors, if (qr(no_trt)$rank == rk)
          "A covariate is identical to, or fully determined by, the treatment group. Remove it."
          else "Two covariates carry the same information (one is determined by the others). Remove one.")
      }
      dfe <- nrow(X) - ncol(X)
      if (rk == ncol(X) && dfe < BE_COVARIATE_MIN_DF)
        errors <- c(errors, sprintf("Only %d residual degrees of freedom would remain (at least %d needed). Use fewer covariates.",
                                    dfe, BE_COVARIATE_MIN_DF))
    }
  }
  list(errors = errors, warnings = warns)
}

#' The widened-limit scope chosen in the app, as fit_be_parameter() takes it
widened_scope_value <- function(x) if (isTRUE(x %in% c("all", "cmax_pauc"))) x else "cmax"

#' Suggest which treatment is the Reference from its name
#'
#' Only names that unambiguously mean "reference" are recognised (R, Ref,
#' Reference, Comparator, Innovator, Originator, RLD, in any capitals).
#' Anything else (A/B, New/Old) returns NULL: the user must choose.
suggest_reference_treatment <- function(levels) {
  hit <- levels[grepl("^(r|ref|reference|comparator|innovator|originator|rld)$",
                      trimws(levels), ignore.case = TRUE)]
  if (length(hit) == 1) hit else NULL
}

#' Within-subject CV (%) of a parameter from the BE confidence-interval table
#'
#' 100 * sqrt(exp(MSE) - 1), from the residual variance of the log-scale
#' model. NA when the parameter was not analysed on the log scale. For a
#' parallel design this is the total (between + within) CV, which is what a
#' parallel-design sample size needs. With covariates the MSE is the residual
#' after adjustment; unadjusted = TRUE gives the CV without adjustment.
within_cv_from_be <- function(ci_table, param = "CMAX", unadjusted = FALSE) {
  if (is.null(ci_table) || !all(c("Parameter", "Scale", "MSE") %in% names(ci_table))) return(NA_real_)
  i <- match(param, ci_table$Parameter)
  mse <- if (unadjusted && "Unadj_MSE" %in% names(ci_table)) ci_table$Unadj_MSE[i] else ci_table$MSE[i]
  if (is.na(i) || !grepl("^Ratio", ci_table$Scale[i]) || !is.finite(mse)) return(NA_real_)
  100 * sqrt(exp(mse) - 1)
}

#' Is a confidence interval within the acceptance limits?
#'
#' The limits are compared after rounding the CI to two decimals, as in FDA
#' "Statistical Approaches to Establishing Bioequivalence" (May 2026): "the
#' rounded confidence interval value should be at least 80.00 percent and not
#' more than 125.00 percent". The table shows the same rounded values.
be_limits_pass <- function(ci_lo, ci_hi, lower, upper) {
  round(ci_lo, 2) >= lower && round(ci_hi, 2) <= upper
}

#' Parameters compared as a ratio without a bioequivalence verdict
BE_NO_VERDICT_PARAMS <- c("LAMZHL")

#' Fit the BE model for one PK parameter and derive the CI and verdict
#'
#' @param be_data   Data frame at NCA-profile grain with the design columns
#' @param param     PK parameter column, e.g. "CMAX"
#' @param design    design code from BE_DESIGNS (R/designs.R); legacy codes
#'                  such as "crossover_fixed_order" are also accepted
#' @param model_type "fixed" or "mixed"
#' @param trt_col,subj_col Treatment and subject columns. Treatment must be a
#'                  factor whose first level is the reference.
#' @param per_col,seq_col Period and sequence columns, or NULL
#' @param log_transform Analyse log(param); never applied to TMAX
#' @param ci_level  Confidence level in percent
#' @param be_lower,be_upper Acceptance limits in percent
#' @param pe_constraint When the limits are wider than 80-125%, also require
#'                  the point estimate within 80.00-125.00% (as ABEL and RSABE
#'                  do). Ignored for limits within 80-125%, where the CI
#'                  already implies it.
#' @param widened_scope Which metrics widened limits (wider than 80-125%)
#'                  apply to: "cmax" (reference-scaled bioequivalence: EMA
#'                  1401/98 Rev.1 section 4.1.10 widens Cmax only; every other
#'                  metric is judged against 80.00-125.00%), "cmax_pauc" (Cmax
#'                  and the partial metrics, as the EMA modified-release
#'                  guideline allows for a partial AUC; AUC to last point and
#'                  to infinity stay at 80.00-125.00%) or "all" (e.g.
#'                  drug-interaction no-effect boundaries)
#' @param diff_unit Unit label for an untransformed difference, e.g. "h"
#' @param verdict   FALSE for a supportive metric: ratio and CI without a verdict
#' @param covariates Resolved covariate specification from build_be_data()
#'                  (parallel groups only): the model becomes
#'                  ln(PK) = Treatment + covariates, main effects, ordinary
#'                  least squares, pooled variance. The adjusted interval is
#'                  the primary result; the row gains Adjusted_for, Unadj_Lower
#'                  and Unadj_Upper (the unadjusted pooled interval), and
#'                  estimate gains unadjusted and covariate_coefs. A run the
#'                  covariates cannot support stops with an error of class
#'                  "be_covariate_error" (be_covariate_checks()). NULL leaves
#'                  every result as it was without the feature.
#' @return list(row      = one-row data frame for the CI table,
#'              anova    = ANOVA table or NULL,
#'              estimate = unrounded list(pe, ci_lo, ci_hi, dfe, mse) or NULL;
#'                         parallel designs add welch = list(lo, hi, df),
#'                         the unequal-variance interval (supplementary),
#'              reason   = character explanation when no estimate, else NULL)
fit_be_parameter <- function(be_data, param, design, model_type = "fixed",
                             trt_col, subj_col, per_col = NULL, seq_col = NULL,
                             log_transform = TRUE, ci_level = 90,
                             be_lower = 80, be_upper = 125,
                             pe_constraint = TRUE, widened_scope = "cmax",
                             diff_unit = NULL, verdict = TRUE, covariates = NULL) {

  out <- list(row = NULL, anova = NULL, estimate = NULL, reason = NULL)
  trt_levels <- levels(be_data[[trt_col]])
  alpha <- 1 - ci_level / 100

  # Only a log-transformed analysis yields a ratio that can be judged against
  # percentage limits. TMAX is never transformed, and neither is anything when
  # the user switches the transform off: those give a difference in the
  # parameter's own units and no verdict.
  is_ratio <- log_transform && param != "TMAX"
  # A single-sequence (fixed-order) study confounds period with treatment, so
  # it can report a paired ratio but cannot support a bioequivalence verdict.
  model_family <- be_design_model(design)
  is_paired <- model_family == "paired"
  # Only exposure parameters are bioequivalence endpoints. Half-life is
  # reported as a ratio with its confidence interval (useful in drug
  # interaction studies) but is not judged against acceptance limits.
  has_limits <- is_ratio && !is_paired && !param %in% BE_NO_VERDICT_PARAMS && isTRUE(verdict)
  scale_label <- if (is_ratio) "Ratio T/R (%)" else
    paste0("Difference T\u2212R", if (!is.null(diff_unit)) paste0(" (", diff_unit, ")") else "")
  if (!is.null(covariates) && nrow(covariates) > 0 && model_family != "parallel")
    stop(structure(class = c("be_covariate_error", "error", "condition"),
                   list(message = be_covariate_checks(be_data, covariates, trt_col, subj_col, design = design)$errors[1],
                        call = NULL)))
  widened <- be_lower < 80 || be_upper > 125
  widen_here <- identical(widened_scope, "all") || param == "CMAX" ||
    (identical(widened_scope, "cmax_pauc") && length(partial_auc_cols(param)) == 1)
  if (widened && !widen_here) {
    be_lower <- 80; be_upper <- 125; widened <- FALSE
  }

  # Every row carries the same columns, so rows for parameters that could not
  # be estimated bind with the rest instead of breaking rbind().
  model_label <- NA_character_
  # Counts of profiles that cannot enter this metric's comparison, so a reader
  # sees how much of the design is left (a subject missing one period drops out)
  n_zero_t <- NA_integer_; n_zero_r <- NA_integer_
  n_miss_t <- NA_integer_; n_miss_r <- NA_integer_
  n_excl_t <- 0L; n_excl_r <- 0L; n_flag_t <- 0L; n_flag_r <- 0L; n_incomplete <- 0L
  if (!is.null(covariates) && nrow(covariates) == 0) covariates <- NULL
  unadj_lo <- NA_real_; unadj_hi <- NA_real_; unadj_mse <- NA_real_
  make_row <- function(pe = NA, lo = NA, hi = NA, n_t = NA, n_r = NA, o_t = NA, o_r = NA,
                       pe_status = NA, verdict = NA, mse = NA, dfe = NA) {
    row <- data.frame(
      Parameter = param, Test = as.character(trt_levels[2]),
      Reference = as.character(trt_levels[1]),
      N_Test = n_t, N_Ref = n_r, Obs_Test = o_t, Obs_Ref = o_r, Scale = scale_label,
      Point_Est = pe, CI_Lower = lo, CI_Upper = hi,
      BE_Lower = if (has_limits) be_lower else NA,
      BE_Upper = if (has_limits) be_upper else NA,
      PE_Constraint = pe_status, Bioequivalent = verdict,
      Missing_Test = n_miss_t, Missing_Ref = n_miss_r,
      Zeros_Test = n_zero_t, Zeros_Ref = n_zero_r,
      Excluded_Test = n_excl_t, Excluded_Ref = n_excl_r,
      Flagged_Test = n_flag_t, Flagged_Ref = n_flag_r, Incomplete_Subjects = n_incomplete,
      MSE = mse, DF = dfe, Model = model_label, stringsAsFactors = FALSE)
    if (!is.null(covariates)) {
      row$Adjusted_for <- paste(covariates$name, collapse = ", ")
      row$Unadj_Lower <- unadj_lo; row$Unadj_Upper <- unadj_hi; row$Unadj_MSE <- unadj_mse
    }
    row
  }

  # A crossover without its Period column loses the period term: a period
  # effect then biases the ratio when the sequences are unbalanced, and widens
  # the CI when they are balanced. No estimate rather than a wrong one.
  if (model_family == "crossover" && (is.null(per_col) || !per_col %in% names(be_data))) {
    out$reason <- paste0("no verdict: a crossover needs the Period column. Map it on the Upload page ",
                         "(a column named Visit or Occasion is not recognised automatically) and run ",
                         "the analysis again.")
    out$row <- make_row(verdict = out$reason)
    return(out)
  }

  # Subject, Period and Sequence are classification factors in the ANOVA
  # model. Uploaded files usually code them as integers, and lm()/lme() would
  # then fit each as a single linear covariate: harmless with two periods,
  # but with three or four it is no longer the EMA Method A model and both
  # the CI and the degrees of freedom change.
  for (col in c(subj_col, per_col, seq_col)) {
    if (!is.null(col) && col %in% names(be_data) && !is.factor(be_data[[col]]))
      be_data[[col]] <- factor(as.character(be_data[[col]]))
  }

  vals <- as.numeric(be_data[[param]])
  trt <- as.character(be_data[[trt_col]])
  # A profile label: subject, and the period when the design has one
  prof_label <- function(i) paste0(as.character(be_data[[subj_col]][i]),
    if (!is.null(per_col) && per_col %in% names(be_data))
      paste0(" period ", as.character(be_data[[per_col]][i])) else "")
  # Profiles the analyst excluded (with a reason) leave the comparison and are
  # counted apart from missing ones
  excl <- if ("EXCLUDED" %in% names(be_data)) !is.na(be_data$EXCLUDED) & nzchar(be_data$EXCLUDED) else
    rep(FALSE, nrow(be_data))
  n_excl_t <- sum(excl & trt == trt_levels[2]); n_excl_r <- sum(excl & trt == trt_levels[1])
  vals[excl] <- NA
  # Values from half-life fits with a raised flag (informational; nothing is excluded)
  fc <- intersect(lz_flag_cols(param), names(be_data))
  if (length(fc) > 0) {
    fl <- !is.na(vals) & Reduce(`|`, lapply(fc, function(cc) suppressWarnings(as.numeric(be_data[[cc]])) %in% 1))
    n_flag_t <- sum(fl & trt == trt_levels[2]); n_flag_r <- sum(fl & trt == trt_levels[1])
  }
  missing <- is.na(vals) & !excl
  n_miss_t <- sum(missing & trt == trt_levels[2]); n_miss_r <- sum(missing & trt == trt_levels[1])
  # A partial AUC (or Cmax/Tmax within an interval) is not reported when the
  # interval reaches past the profile's last measurable concentration, while
  # the profile itself has an NCA result. Those are low-exposure profiles:
  # leaving them out would bias the ratio, as with zeros, so no estimate
  if (length(partial_auc_cols(param)) == 1 && "AUCLST" %in% names(be_data)) {
    beyond <- missing & !is.na(suppressWarnings(as.numeric(be_data$AUCLST)))
    if (any(beyond)) {
      who <- vapply(which(beyond), prof_label, character(1))
      out$reason <- paste0("no verdict: the interval reaches past the last measurable concentration in ",
                           sum(beyond), " profile(s): ", paste(head(who, 5), collapse = "; "),
                           if (length(who) > 5) paste0(" and ", length(who) - 5, " more") else "",
                           ". These values are not reported, and the profiles that would drop out are the ",
                           "low-exposure ones, so the remaining ratio would be biased. The interval and the ",
                           "BLQ rule belong in the protocol.")
      out$row <- make_row(verdict = out$reason)
      return(out)
    }
  }
  if (is_ratio) {
    # A zero (e.g. an early partial AUC with only BLQ samples) has no
    # logarithm. The profiles that would drop out are the low-exposure ones, so
    # the remaining ratio would be biased: no estimate and no verdict.
    zero <- !is.na(vals) & vals == 0
    n_zero_t <- sum(zero & trt == trt_levels[2]); n_zero_r <- sum(zero & trt == trt_levels[1])
    if (any(zero)) {
      who <- vapply(which(zero), prof_label, character(1))
      out$reason <- paste0("no verdict: ", sum(zero), " zero value(s) (", trt_levels[2], " ", n_zero_t,
                           ", ", trt_levels[1], " ", n_zero_r, "): ",
                           paste(head(who, 5), collapse = "; "),
                           if (length(who) > 5) paste0(" and ", length(who) - 5, " more") else "",
                           ". A zero cannot be log-transformed, and the profiles that would drop out ",
                           "are the low-exposure ones, so the remaining ratio would be biased. The ",
                           "interval and the BLQ rule belong in the protocol.")
      out$row <- make_row(verdict = out$reason)
      return(out)
    }
    vals <- log(vals); vals[!is.finite(vals)] <- NA
  } else {
    n_zero_t <- 0L; n_zero_r <- 0L
  }
  if (all(is.na(vals))) {
    out$reason <- paste0("no verdict: no profile has a value for this metric (", n_miss_t, " ",
                         trt_levels[2], " and ", n_miss_r, " ", trt_levels[1], " profiles missing)")
    out$row <- make_row(verdict = out$reason)
    return(out)
  }
  be_data$.response <- vals

  cov_warnings <- character(0)
  if (!is.null(covariates)) {
    chk <- be_covariate_checks(be_data, covariates, trt_col, subj_col,
                               rows = !is.na(be_data$.response), design = design)
    if (length(chk$errors) > 0)
      stop(structure(class = c("be_covariate_error", "error", "condition"),
                     list(message = paste(chk$errors, collapse = " "), call = NULL)))
    cov_warnings <- chk$warnings
  }

  # In a two-period crossover (and the paired comparison) a subject without a
  # value under both treatments adds nothing to the within-subject contrast,
  # and the EMA guideline leaves such subjects out. The fixed-effects model
  # ignores them anyway (the subject term absorbs a single value); the mixed
  # model would use their one value through the random effect, so they are
  # removed before either model is fitted. Replicate designs keep them: a
  # subject's repeated administrations of one treatment still inform the
  # within-subject variance.
  if (design %in% c("2x2x2", "paired")) {
    ok <- !is.na(be_data$.response)
    s_ref <- unique(as.character(be_data[[subj_col]][ok & trt == trt_levels[1]]))
    s_tst <- unique(as.character(be_data[[subj_col]][ok & trt == trt_levels[2]]))
    orphan <- ok & !as.character(be_data[[subj_col]]) %in% intersect(s_ref, s_tst)
    n_incomplete <- length(unique(as.character(be_data[[subj_col]][orphan])))
    be_data$.response[orphan] <- NA
  }

  # Guard: if Sequence has only 1 level, drop it (prevents lm() crash)
  if (!is.null(seq_col) && length(unique(be_data[[seq_col]])) < 2) {
    seq_col <- NULL
  }

  use_mixed <- model_type == "mixed" &&
               model_family == "crossover" &&
               requireNamespace("nlme", quietly = TRUE)

  # Build and fit model
  if (model_family == "parallel") {
    fit <- tryCatch(lm(as.formula(paste(".response ~", paste(c(trt_col, covariates$col), collapse = " + "))),
                       data = be_data, na.action = na.exclude), error = function(e) NULL)
  } else if (is_paired) {
    # Paired comparison (fixed order): all subjects received the same sequence.
    # Period and Treatment are confounded. Model: Subject + Treatment only.
    # Equivalent to a paired t-test on log-transformed parameters.
    fit <- tryCatch(
      lm(as.formula(paste(".response ~", subj_col, "+", trt_col)),
         data = be_data, na.action = na.exclude),
      error = function(e) NULL)
  } else if (use_mixed) {
    fixed_terms <- c()
    if (!is.null(seq_col)) fixed_terms <- c(fixed_terms, seq_col)
    if (!is.null(per_col)) fixed_terms <- c(fixed_terms, per_col)
    fixed_terms <- c(fixed_terms, trt_col)
    # Subject IDs are unique across sequences, so the random intercept is on
    # Subject alone. Nesting it in Sequence (~1|Sequence/Subject) also put a
    # random effect on Sequence, which is already a fixed effect, and left the
    # Sequence row of the ANOVA with zero denominator degrees of freedom.
    random_f <- paste0("~1|", subj_col)
    mixed_error <- NULL
    fit <- tryCatch(
      nlme::lme(fixed = as.formula(paste(".response~", paste(fixed_terms, collapse = "+"))),
                random = as.formula(random_f), data = be_data, na.action = na.exclude),
      error = function(e) {
        mixed_error <<- conditionMessage(e)
        tryCatch(lm(as.formula(paste(".response~", paste(c(fixed_terms, subj_col), collapse = "+"))),
                    data = be_data, na.action = na.exclude), error = function(e2) NULL)
      })
  } else {
    terms <- c()
    if (!is.null(seq_col)) terms <- c(terms, seq_col)
    terms <- c(terms, subj_col)
    if (!is.null(per_col)) terms <- c(terms, per_col)
    terms <- c(terms, trt_col)
    fit <- tryCatch(lm(as.formula(paste(".response~", paste(terms, collapse = "+"))),
                       data = be_data, na.action = na.exclude), error = function(e) NULL)
  }

  model_label <- if (inherits(fit, "lme")) "mixed effects" else
    if (use_mixed) paste0("fixed effects (mixed model failed to fit: ", mixed_error, ")") else "fixed effects"
  if (is.null(fit)) {
    out$reason <- paste0("no verdict: the model could not be fitted",
                         if (n_miss_t + n_miss_r > 0)
                           paste0("; ", n_miss_t + n_miss_r, " profile(s) have no value for this metric")
                         else "", ".")
    out$row <- make_row(verdict = out$reason)
    return(out)
  }

  out$anova <- tryCatch({
    if (inherits(fit, "lme")) {
      # Type III (marginal) SS for lme — order-independent, correct for
      # unbalanced data. anova.lme with type="marginal" uses Wald F-tests.
      anova(fit, type = "marginal")
    } else {
      # drop1 with F-test gives Type III SS for lm objects.
      tbl <- drop1(fit, test = "F")
      # Sequence is a between-subject effect and is aliased with Subject, so
      # drop1() reports it with 0 df. Test it the crossover way instead:
      # sequential SS for Sequence (entered first) against Subject(Sequence).
      if (!is.null(seq_col) && all(c(seq_col, subj_col) %in% rownames(tbl))) {
        a1 <- anova(fit)
        if (seq_col %in% rownames(a1) && identical(rownames(a1)[1], seq_col)) {
          df_seq  <- a1[seq_col, "Df"];   ss_seq  <- a1[seq_col, "Sum Sq"]
          df_subj <- tbl[subj_col, "Df"]; ss_subj <- tbl[subj_col, "Sum of Sq"]
          f_seq <- (ss_seq / df_seq) / (ss_subj / df_subj)
          tbl[seq_col, "Df"]        <- df_seq
          tbl[seq_col, "Sum of Sq"] <- ss_seq
          tbl[seq_col, "F value"]   <- f_seq
          tbl[seq_col, "Pr(>F)"]    <- pf(f_seq, df_seq, df_subj, lower.tail = FALSE)
        }
      }
      tbl
    }
  }, error = function(e) NULL)

  is_lme <- inherits(fit, "lme")
  trt_coef_name <- paste0(trt_col, trt_levels[2])

  # N counts subjects; Obs counts profiles (administrations). They differ in
  # replicate designs, where a subject receives a treatment more than once.
  has_ref <- be_data[[trt_col]] == trt_levels[1] & !is.na(be_data$.response)
  has_tst <- be_data[[trt_col]] == trt_levels[2] & !is.na(be_data$.response)
  o1 <- sum(has_ref); o2 <- sum(has_tst)
  subj_ref <- unique(as.character(be_data[[subj_col]][has_ref]))
  subj_tst <- unique(as.character(be_data[[subj_col]][has_tst]))
  if (model_family != "parallel" && !inherits(fit, "lme")) {
    # With subject as a fixed effect only subjects observed under both
    # treatments contribute to the comparison; count those
    both <- intersect(subj_ref, subj_tst); n1 <- length(both); n2 <- length(both)
  } else {
    n1 <- length(subj_ref); n2 <- length(subj_tst)
  }

  coef_result <- tryCatch({
    if (is_lme) {
      coefs <- nlme::fixef(fit); se_tbl <- summary(fit)$tTable; mse <- summary(fit)$sigma^2
      if (trt_coef_name %in% names(coefs) && !is.na(coefs[trt_coef_name])) {
        list(diff = coefs[trt_coef_name], se = se_tbl[trt_coef_name, "Std.Error"],
             dfe = se_tbl[trt_coef_name, "DF"], mse = mse)
      } else { NULL }
    } else {
      coefs <- coef(fit); mse <- summary(fit)$sigma^2; dfe <- fit$df.residual
      se_tbl <- summary(fit)$coefficients
      if (trt_coef_name %in% names(coefs) && !is.na(coefs[trt_coef_name])) {
        list(diff = coefs[trt_coef_name], se = se_tbl[trt_coef_name, "Std. Error"],
             dfe = dfe, mse = mse)
      } else { NULL }
    }
  }, error = function(e) NULL)

  if (is.null(coef_result)) {
    # Provide specific guidance based on the likely cause
    reason <- if (!is.null(seq_col) && length(unique(be_data[[seq_col]])) < 2) {
      "All subjects have the same sequence — try selecting 'Paired comparison' as the study design."
    } else if (!is.null(per_col) && length(unique(be_data[[per_col]])) < 2) {
      "Only one period found — try selecting 'Parallel groups' as the study design."
    } else {
      "The statistical model could not estimate the treatment effect. Check that the study design selection matches your data."
    }
    if (n_miss_t + n_miss_r > 0)
      reason <- paste0(reason, " ", n_miss_t + n_miss_r, " profile(s) have no value for this metric.")
    out$reason <- reason
    out$row <- make_row(verdict = reason)
    return(out)
  }

  diff <- coef_result$diff; se_diff <- coef_result$se
  dfe <- coef_result$dfe; mse <- coef_result$mse

  t_crit <- qt(1 - alpha / 2, dfe)
  ci_lo <- diff - t_crit * se_diff
  ci_hi <- diff + t_crit * se_diff

  if (is_ratio) {
    pe <- exp(diff) * 100; ci_lo_p <- exp(ci_lo) * 100; ci_hi_p <- exp(ci_hi) * 100
  } else { pe <- diff; ci_lo_p <- ci_lo; ci_hi_p <- ci_hi }
  pe <- unname(pe); ci_lo_p <- unname(ci_lo_p); ci_hi_p <- unname(ci_hi_p)

  # The model is pre-specified (ICH M13A 2.2.3.1: no data-driven changes to
  # the primary analysis). When the chosen mixed model cannot be fitted, the
  # fixed-effects fit is shown for information, without a verdict.
  mixed_failed <- use_mixed && !inherits(fit, "lme")
  # TOST at alpha = 0.05 is the 90% interval; an 80% interval would double the
  # type I error, so other levels give the interval without a verdict
  if (has_limits && !isTRUE(all.equal(as.numeric(ci_level), 90))) {
    pe_status <- "not applicable"
    verdict <- paste0("no verdict: a bioequivalence verdict uses the 90% confidence interval (this is ",
                      ci_level, "%)")
  } else if (has_limits && mixed_failed) {
    pe_status <- "not applicable"
    verdict <- paste0("no verdict: the pre-specified mixed model could not be fitted (", mixed_error,
                      "); the fixed-effects result is shown for information only")
  } else if (has_limits) {
    ci_pass <- be_limits_pass(ci_lo_p, ci_hi_p, be_lower, be_upper)
    # A CI inside 80-125% already puts the point estimate inside it. Wider
    # limits (ABEL-style, or fixed widened Cmax limits) do not, so the
    # constraint is applied unless the user has explicitly switched it off.
    if (!widened) {
      pe_status <- "not required"; be_pass <- ci_pass
    } else if (isTRUE(pe_constraint)) {
      pe_ok <- round(pe, 2) >= 80 && round(pe, 2) <= 125
      pe_status <- if (pe_ok) "YES" else "NO"; be_pass <- ci_pass && pe_ok
    } else {
      pe_status <- "not applied"; be_pass <- ci_pass
    }
    verdict <- if (be_pass) "YES" else "NO"
  } else {
    pe_status <- "not applicable"; verdict <- "no verdict"
  }

  # Parallel groups: the unequal-variance (Welch) interval as supplementary
  # information beside the pooled-variance interval, which stays the primary result
  welch <- if (model_family == "parallel") tryCatch({
    y <- be_data$.response; g <- be_data[[trt_col]]
    tt <- stats::t.test(y[g == trt_levels[2]], y[g == trt_levels[1]],
                        var.equal = FALSE, conf.level = 1 - alpha)
    ci <- as.numeric(tt$conf.int)
    if (is_ratio) ci <- exp(ci) * 100
    list(lo = ci[1], hi = ci[2], df = unname(tt$parameter))
  }, error = function(e) NULL)

  out$estimate <- list(pe = pe, ci_lo = ci_lo_p, ci_hi = ci_hi_p,
                       dfe = unname(dfe), mse = unname(mse), welch = welch)
  if (!is.null(covariates)) {
    # Supplementary: the same rows without covariates (pooled variance)
    fit0 <- lm(as.formula(paste(".response ~", trt_col)), data = be_data)
    ci0 <- confint(fit0, trt_coef_name, level = 1 - alpha)
    d0 <- unname(coef(fit0)[trt_coef_name]); ci0 <- as.numeric(ci0)
    if (is_ratio) { d0 <- exp(d0) * 100; ci0 <- exp(ci0) * 100 }
    unadj_lo <- ci0[1]; unadj_hi <- ci0[2]
    unadj_mse <- round(summary(fit0)$sigma^2, 6)
    out$estimate$unadjusted <- list(pe = d0, ci_lo = unadj_lo, ci_hi = unadj_hi,
                                    dfe = unname(fit0$df.residual), mse = unadj_mse)
    out$estimate$covariate_coefs <- be_covariate_coefs(fit, covariates, trt_col, alpha)
    out$warnings <- cov_warnings
    unadj_lo <- round(unadj_lo, 2); unadj_hi <- round(unadj_hi, 2)
  }
  out$row <- make_row(pe = round(pe, 2), lo = round(ci_lo_p, 2), hi = round(ci_hi_p, 2),
                      n_t = n2, n_r = n1, o_t = o2, o_r = o1,
                      pe_status = pe_status, verdict = verdict,
                      mse = round(mse, 6), dfe = unname(dfe))
  out
}


#' Baseline balance of the covariates between the two groups
#'
#' One row per numeric covariate (mean and SD per group) and per category
#' (n and % per group), with the standardized difference: the difference in
#' means over the pooled SD, or the difference in proportions over the pooled
#' binomial SD. It describes the groups; it is not a test, and covariates are
#' not to be chosen after looking at it. Excluded profiles are left out.
#' @return data.frame(Covariate, Level, <reference>, <test>, Std_Diff) or NULL
be_covariate_balance <- function(be_data, spec, trt_col, subj_col) {
  if (is.null(spec) || nrow(spec) == 0) return(NULL)
  keep <- if ("EXCLUDED" %in% names(be_data)) is.na(be_data$EXCLUDED) | !nzchar(be_data$EXCLUDED) else rep(TRUE, nrow(be_data))
  d <- be_data[keep, , drop = FALSE]
  d <- d[!duplicated(as.character(d[[subj_col]])), , drop = FALSE]
  lv <- levels(factor(d[[trt_col]])); if (length(lv) != 2) return(NULL)
  g <- as.character(d[[trt_col]])
  sd_diff <- function(a, b, den) if (is.finite(den) && den > 0) (a - b) / den else NA_real_
  rows <- list()
  for (i in seq_len(nrow(spec))) {
    v <- d[[spec$col[i]]]
    if (spec$type[i] == "numeric") {
      m <- tapply(v, g, mean, na.rm = TRUE)[lv]; sdv <- tapply(v, g, stats::sd, na.rm = TRUE)[lv]
      rows[[length(rows) + 1]] <- data.frame(Covariate = spec$name[i], Level = NA_character_,
        A = sprintf("%s (%s)", signif(m[1], 4), signif(sdv[1], 3)), B = sprintf("%s (%s)", signif(m[2], 4), signif(sdv[2], 3)),
        Std_Diff = sd_diff(m[2], m[1], sqrt((sdv[1]^2 + sdv[2]^2) / 2)), stringsAsFactors = FALSE)
    } else {
      for (l in levels(factor(v))) {
        n <- vapply(lv, function(x) sum(g == x & v %in% l, na.rm = TRUE), numeric(1))
        tot <- vapply(lv, function(x) sum(g == x & !is.na(v)), numeric(1))
        p <- n / tot
        rows[[length(rows) + 1]] <- data.frame(Covariate = spec$name[i], Level = l,
          A = sprintf("%d (%.0f%%)", n[1], 100 * p[1]), B = sprintf("%d (%.0f%%)", n[2], 100 * p[2]),
          Std_Diff = sd_diff(p[2], p[1], sqrt((p[1] * (1 - p[1]) + p[2] * (1 - p[2])) / 2)), stringsAsFactors = FALSE)
      }
    }
  }
  out <- do.call(rbind, rows)
  names(out)[3:4] <- lv
  out
}

#' Coefficients of the covariates in a fitted parallel-group model
#'
#' One row per model term (numeric covariate, or category against its
#' reference), with the estimate on the log scale (the analysis scale), its
#' standard error, the confidence interval at the analysis level and the
#' p-value. Supplementary: the treatment effect is the primary result.
be_covariate_coefs <- function(fit, spec, trt_col, alpha = 0.10) {
  sm <- summary(fit)$coefficients
  ci <- suppressWarnings(confint(fit, level = 1 - alpha))
  rows <- list()
  for (i in seq_len(nrow(spec))) {
    col <- spec$col[i]
    hit <- if (spec$type[i] == "numeric") intersect(col, rownames(sm)) else
      rownames(sm)[startsWith(rownames(sm), col)]
    for (h in hit) {
      lab <- if (spec$type[i] == "numeric") spec$name[i] else {
        lv <- levels(fit$model[[col]])
        paste0(spec$name[i], ": ", substring(h, nchar(col) + 1), " vs ", lv[1])
      }
      rows[[length(rows) + 1]] <- data.frame(Term = lab, Estimate = sm[h, 1], SE = sm[h, 2],
        CI_Lower = ci[h, 1], CI_Upper = ci[h, 2], P_Value = sm[h, 4], stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

#' Profiles per treatment whose partial AUC rests mainly on BLQ-derived values
#'
#' A ratio can be driven by such a profile without any value being exactly
#' zero, so the bioequivalence table reports these counts beside the zero and
#' missing counts. Cmax and Tmax within an interval inherit the flag of their
#' interval.
#'
#' @param blq_tab attr(run_nca(...), "partial_auc_blq"): the profile keys and
#'   one logical column per interval
#' @param be_data Data frame at NCA-profile grain (build_be_data()$data)
#' @param params PK parameters in the comparison
#' @return data.frame(Parameter, BLQ_Test, BLQ_Ref), or NULL without flags
partial_auc_blq_counts <- function(blq_tab, be_data, params, trt_col, trt_levels) {
  if (is.null(blq_tab) || length(params) == 0) return(NULL)
  keys <- intersect(c("Subject", "Treatment", "Period"), names(blq_tab))
  if (length(keys) == 0) return(NULL)
  bd <- merge(be_data[, unique(c(keys, trt_col)), drop = FALSE], blq_tab, by = keys,
              all.x = TRUE, sort = FALSE)
  count <- function(param, level) {
    col <- sub("^(CMAX|TMAX)_", "AUC_", param)
    if (!col %in% names(bd)) return(NA_integer_)
    sum(bd[[col]] %in% TRUE & as.character(bd[[trt_col]]) == level)
  }
  data.frame(Parameter = params,
             BLQ_Test = vapply(params, count, integer(1), level = trt_levels[2]),
             BLQ_Ref  = vapply(params, count, integer(1), level = trt_levels[1]),
             stringsAsFactors = FALSE)
}

#' Decide which BE design to analyse, given what the data show
#'
#' A study in which every subject received the treatments in the same order
#' is a paired comparison, whatever design was selected. It is detected from a
#' single Sequence level, or, when no Sequence column is mapped, from every
#' subject having the same treatment order by Period.
#'
#' @return list(design = design to analyse, note = explanation or NULL)
resolve_be_design <- function(design, be_data, subj_col, trt_col,
                              per_col = NULL, seq_col = NULL) {
  if (be_design_model(design) != "crossover") return(list(design = design, note = NULL))

  n_orders <- if (!is.null(seq_col) && seq_col %in% names(be_data)) {
    length(unique(be_data[[seq_col]]))
  } else if (!is.null(per_col) && per_col %in% names(be_data)) {
    per_num <- suppressWarnings(as.numeric(as.character(be_data[[per_col]])))
    ord <- if (anyNA(per_num)) order(as.character(be_data[[per_col]])) else order(per_num)
    d <- be_data[ord, , drop = FALSE]
    orders <- tapply(as.character(d[[trt_col]]), as.character(d[[subj_col]]),
                     paste, collapse = ">")
    length(unique(orders))
  } else {
    NA
  }

  if (!is.na(n_orders) && n_orders == 1) {
    list(design = "paired",
         note = paste0("All subjects received the treatments in the same order, so period ",
                       "and treatment cannot be separated. The data were analysed as a paired ",
                       "comparison, which reports the ratio and its confidence interval but ",
                       "gives no bioequivalence verdict."))
  } else {
    list(design = design, note = NULL)
  }
}


#' Within-subject SD and CV of one treatment in a replicate design
#'
#' Fits log(PK) ~ Sequence + Subject + Period to that treatment's data,
#' restricted to subjects who received it at least twice. The period term is
#' essential: without it period effects inflate the residual, and on real data
#' the naive subject-only model can move CVwR across the 30% threshold. Each
#' term is included only if it still has at least two levels after filtering
#' (a TRT/RTR design keeps a single sequence for the test treatment, for
#' example). This is the model used by replicateBE (EMA Method A data sets).
#'
#' @return list(sw, cv (percent), df, n_subjects) or NULL if not estimable
within_subject_variability <- function(be_data, param, level, trt_col, subj_col,
                                       per_col = NULL, seq_col = NULL) {
  d <- be_data[as.character(be_data[[trt_col]]) == level, , drop = FALSE]
  d$.y <- suppressWarnings(log(as.numeric(d[[param]])))
  d <- d[is.finite(d$.y), , drop = FALSE]
  subj <- as.character(d[[subj_col]])
  keep <- subj %in% names(which(table(subj) >= 2))
  d <- d[keep, , drop = FALSE]
  if (nrow(d) == 0) return(NULL)

  terms <- character(0)
  for (col in c(seq_col, subj_col, per_col)) {
    if (is.null(col) || !col %in% names(d)) next
    d[[col]] <- factor(as.character(d[[col]]))
    if (nlevels(d[[col]]) >= 2) terms <- c(terms, col)
  }
  if (!subj_col %in% terms) return(NULL)
  fit <- tryCatch(lm(as.formula(paste(".y ~", paste(terms, collapse = " + "))), data = d),
                  error = function(e) NULL)
  if (is.null(fit) || fit$df.residual < 1) return(NULL)
  s2 <- sum(residuals(fit)^2) / fit$df.residual
  list(sw = sqrt(s2), cv = 100 * sqrt(exp(s2) - 1), df = fit$df.residual,
       n_subjects = length(unique(as.character(d[[subj_col]]))))
}

#' EMA average bioequivalence with expanding limits (ABEL) for a given CVwR
#'
#' 80.00-125.00% up to CVwR 30%; exp(+/-0.76 * swR) above that; capped at
#' CVwR 50% (69.84-143.19%).
#' @param cv_pct Within-subject CV of the reference, in percent
#' @return c(lower, upper) in percent
abel_limits <- function(cv_pct) {
  if (is.na(cv_pct)) return(c(NA_real_, NA_real_))
  if (cv_pct <= 30) return(c(80, 125))
  sw <- sqrt(log((min(cv_pct, 50) / 100)^2 + 1))
  100 * exp(c(-1, 1) * 0.76 * sw)
}

#' Variability diagnostic for a replicate design (informational only)
#'
#' Reports the reference's within-subject variability (swR, CVwR), the test's
#' where the test was also replicated, their ratio, and the ABEL limits those
#' values would imply. It does not issue a scaled bioequivalence verdict: the assessment is be_assess_parameter() in R/be_scaled.R.
#'
#' @return one-row data frame, or NULL when the reference is not replicated
be_variability_diagnostic <- function(be_data, param, trt_col, subj_col,
                                      per_col = NULL, seq_col = NULL) {
  lv <- levels(factor(be_data[[trt_col]]))
  if (length(lv) != 2) return(NULL)
  ref_level <- lv[1]; test_level <- lv[2]
  r <- within_subject_variability(be_data, param, ref_level, trt_col, subj_col, per_col, seq_col)
  if (is.null(r)) return(NULL)
  t <- within_subject_variability(be_data, param, test_level, trt_col, subj_col, per_col, seq_col)
  t_replicated <- any(table(as.character(be_data[[subj_col]][
    as.character(be_data[[trt_col]]) == test_level])) >= 2)
  # EMA widens the limits for Cmax only
  lim <- if (param == "CMAX") abel_limits(r$cv) else c(NA_real_, NA_real_)
  data.frame(
    Parameter = param,
    swR = r$sw, CVwR = r$cv, df_R = r$df, n_R = r$n_subjects,
    swT = if (is.null(t)) NA_real_ else t$sw,
    CVwT = if (is.null(t)) NA_real_ else t$cv,
    CVwT_note = if (!is.null(t)) "" else if (!t_replicated)
      "not estimable: the test treatment was given only once per subject" else
      "not estimable from these data",
    sw_ratio = if (is.null(t)) NA_real_ else t$sw / r$sw,
    ABEL_lower = lim[1], ABEL_upper = lim[2],
    ABEL_widened = param == "CMAX" && r$cv > 30,
    stringsAsFactors = FALSE)
}

#' ICH M13A checks on a bioequivalence analysis (warnings; nothing is excluded)
#'
#' - pre-dose concentration above 5% of the profile's Cmax (single dose;
#'   M13A 2.2.3.3 excludes that period from the primary analysis)
#' - fewer than 12 evaluable subjects (M13A 2.2.3.1)
#' - profiles without an NCA result, and periods with AUC(0-t) below 5% of
#'   the treatment's geometric mean (M13A 2.2.1.1)
#' - AUC(0-t) covering less than 80% of AUC(0-inf) in more than 20% of the
#'   profiles (M13A 2.2.2.2: the validity of the study may need discussion)
#' @param ci_df CI table of the run (N_Test, N_Ref per parameter)
#' @return character vector of messages, empty when all checks pass
#' Single dose: profiles whose pre-dose concentration exceeds 5% of their Cmax
#'
#' ICH M13A (2.2.3.3) excludes such a period from the primary analysis. Used
#' by the bioequivalence checks and the batch NCA.
#' @param verdict TRUE adds that the verdicts shown are then not the primary analysis
#' @return one message, or NULL when no profile is affected
predose_above_5pct_note <- function(pk_data, col_map, verdict = TRUE) {
  pk <- profile_key(pk_data, col_map)
  t <- suppressWarnings(as.numeric(pk_data[[col_map$time]]))
  cc <- suppressWarnings(as.numeric(pk_data[[col_map$conc]]))
  pre <- tapply(ifelse(!is.na(t) & t <= 0, cc, NA), pk$key, function(v) suppressWarnings(max(v, na.rm = TRUE)))
  cmax <- tapply(cc, pk$key, function(v) suppressWarnings(max(v, na.rm = TRUE)))
  high <- names(pre)[is.finite(pre) & is.finite(cmax) & cmax > 0 & pre > 0.05 * cmax]
  if (length(high) == 0) return(NULL)
  lab <- profile_labels(pk$parts[match(high, pk$key), , drop = FALSE])
  paste0("Pre-dose concentration above 5% of Cmax in ", length(high), " profile(s): ",
         paste(head(lab, 5), collapse = "; "), if (length(lab) > 5) " and more" else "",
         ". ICH M13A (2.2.3.3) excludes such a period from the primary analysis of a bioequivalence study",
         if (verdict) paste0(". The verdicts shown include it, so they are not the M13A primary analysis: ",
                             "exclude the period as the protocol says, keep the unedited source file, and ",
                             "run the analysis again.") else
           ". A pre-dose concentration also points to carry-over or an endogenous level.")
}

be_m13a_checks <- function(pk_data, col_map, nca_res, ci_df, is_ss = FALSE) {
  out <- character(0)
  pk <- profile_key(pk_data, col_map)
  if (!isTRUE(is_ss)) out <- c(out, predose_above_5pct_note(pk_data, col_map))
  # Profiles without an NCA result, and periods with very low exposure: M13A
  # (2.2.1.1) accepts leaving such data out only as a documented exception,
  # in general for no more than one subject
  nk_cols <- intersect(c("Subject", "Treatment", "Period"), names(nca_res))
  if (identical(nk_cols, pk$cols)) {
    nk <- do.call(paste, c(lapply(nca_res[nk_cols], as.character), sep = "||"))
    gone <- setdiff(unique(pk$key), nk)
    if (length(gone) > 0) {
      lab <- profile_labels(pk$parts[match(gone, pk$key), , drop = FALSE])
      out <- c(out, paste0(length(gone), " profile(s) have no measurable concentration and no NCA ",
                           "result; they are counted as missing: ", paste(head(lab, 5), collapse = "; "),
                           if (length(lab) > 5) " and more" else "", ". Their subjects leave the comparison. ",
                           "ICH M13A (2.2.1.1) accepts this only as an exception planned in the protocol, in ",
                           "general for no more than one subject."))
    }
  }
  if (all(c("AUCLST", "Treatment", "Subject") %in% names(nca_res))) {
    a <- suppressWarnings(as.numeric(nca_res$AUCLST))
    tr <- as.character(nca_res$Treatment); sj <- as.character(nca_res$Subject)
    low <- vapply(seq_along(a), function(i) {
      o <- a[tr == tr[i] & sj != sj[i]]; o <- o[is.finite(o) & o > 0]
      is.finite(a[i]) && length(o) >= 2 && a[i] < 0.05 * exp(mean(log(o)))
    }, logical(1))
    if (any(low)) {
      lab <- profile_labels(nca_res[low, nk_cols, drop = FALSE])
      out <- c(out, paste0("AUC(0-t) below 5% of the geometric mean of that treatment (without the subject) in ",
                           sum(low), " profile(s): ", paste(head(lab, 5), collapse = "; "),
                           if (length(lab) > 5) " and more" else "", ". ICH M13A (2.2.1.1) calls this very low ",
                           "exposure; leaving it out is accepted only as a planned exception, in general for no ",
                           "more than one subject."))
    }
  }
  if (!is.null(ci_df) && all(c("N_Test", "N_Ref") %in% names(ci_df))) {
    n <- suppressWarnings(pmin(ci_df$N_Test, ci_df$N_Ref))
    if (any(!is.na(n) & n < 12))
      out <- c(out, paste0("Fewer than 12 evaluable subjects (smallest: ", min(n, na.rm = TRUE),
                           "). ICH M13A (2.2.3.1) does not accept a pivotal study with fewer than 12, so the verdict is a ",
                           "statistical result only."))
  }
  if (!isTRUE(is_ss) && "AUCPEO" %in% names(nca_res)) {
    pe <- suppressWarnings(as.numeric(nca_res$AUCPEO)); pe <- pe[!is.na(pe)]
    if (length(pe) > 0 && mean(pe > 20) > 0.2)
      out <- c(out, paste0("AUC(0-t) covers less than 80% of AUC(0-inf) in ", sum(pe > 20), " of ", length(pe),
                           " profiles (more than 20%). ICH M13A (2.2.2.2): the validity of the study may need ",
                           "to be discussed."))
  }
  out
}

#' What the study planner may take from a bioequivalence analysis
#'
#' The residual CV of a crossover or replicate analysis is a within-subject
#' CV; that of a parallel analysis is a total CV. Each is offered only to a
#' planner design that needs that kind. Scaled methods get CVwT and CVwR from
#' the replicate variability table (the pooled CV for both when the analysis
#' had no replicated reference).
#' @param be_results list with ci_table, cv_table and design (analysed code)
#' @return NULL when there is no CV for this metric; list(note) when the
#'   kind does not fit the planner design; otherwise list(cv, cv_wr, label)
planner_cv_offer <- function(be_results, analysis_type, planner_design, param = "CMAX") {
  # The planner assumes no adjustment, so it takes the unadjusted CV; the CV
  # left after adjustment is offered beside it (parallel groups with covariates)
  cv <- within_cv_from_be(be_results$ci_table, param, unadjusted = TRUE)
  if (is.na(cv)) return(NULL)
  be_parallel <- identical(be_design_model(if (is.null(be_results$design)) "2x2x2" else be_results$design), "parallel")
  pl_parallel <- identical(planner_design, "parallel")
  if (be_parallel && !pl_parallel)
    return(list(note = paste0("Your bioequivalence analysis had parallel groups, so its CV is a total CV. ",
                              "The selected design needs a within-subject CV.")))
  if (!be_parallel && pl_parallel)
    return(list(note = paste0("A parallel design needs the total CV. Your bioequivalence analysis was a ",
                              "crossover, which gives only the within-subject CV.")))
  scaled <- analysis_type %in% c("abel", "rsabe", "ntid")
  name <- if (exists("friendly_name")) friendly_name(param) else param
  if (!scaled) {
    out <- list(cv = cv, cv_wr = NA_real_,
                label = sprintf("Use the %s %s CV from my BE analysis (%.1f%%)", name,
                                if (pl_parallel) "total" else "within-subject", cv))
    cv_adj <- within_cv_from_be(be_results$ci_table, param)
    if (pl_parallel && "Unadj_MSE" %in% names(be_results$ci_table) && is.finite(cv_adj))
      out$adjusted <- list(cv = cv_adj,
                           label = sprintf("Use the %s residual CV after adjusting for %s (%.1f%%)", name,
                                           be_results$ci_table$Adjusted_for[1], cv_adj))
    return(out)
  }
  vt <- be_results$cv_table
  i <- if (is.null(vt)) NA else match(param, vt$Parameter)
  cvwr <- if (is.na(i)) cv else vt$CVwR[i]
  cvwt <- if (is.na(i) || is.na(vt$CVwT[i])) cv else vt$CVwT[i]
  list(cv = cvwt, cv_wr = cvwr,
       label = sprintf("Use %s CVwT %.1f%% and CVwR %.1f%% from my BE analysis%s", name, cvwt, cvwr,
                       if (is.na(i)) " (pooled: no replicated reference)" else ""))
}

#' Parallel design: the Welch interval as a sensitivity result
#'
#' The pooled-variance CI is anti-conservative when the smaller group is the
#' more variable one. When the Welch CI would give a different verdict, a
#' message reports it; the pooled result stays the primary one.
#' @return character vector of messages (empty when they agree)
parallel_welch_notes <- function(be_data, params, trt_col, be_lower = 80, be_upper = 125) {
  lv <- levels(factor(be_data[[trt_col]]))
  if (length(lv) != 2) return(character(0))
  out <- character(0)
  for (p in params) {
    y <- suppressWarnings(log(as.numeric(be_data[[p]])))
    ok <- is.finite(y)
    r <- y[ok & be_data[[trt_col]] == lv[1]]; t <- y[ok & be_data[[trt_col]] == lv[2]]
    if (length(r) < 2 || length(t) < 2) next
    ci <- function(var_equal) 100 * exp(stats::t.test(t, r, var.equal = var_equal, conf.level = 0.90)$conf.int)
    pooled <- ci(TRUE); welch <- ci(FALSE)
    if (be_limits_pass(pooled[1], pooled[2], be_lower, be_upper) != be_limits_pass(welch[1], welch[2], be_lower, be_upper))
      out <- c(out, sprintf(paste0("%s: the Welch interval (unequal variances), %.2f-%.2f%%, gives a different ",
                                   "conclusion from the pooled-variance interval, %.2f-%.2f%%. The groups differ in ",
                                   "size or variability; discuss this sensitivity result."),
                            if (exists("friendly_name")) friendly_name(p) else p, welch[1], welch[2], pooled[1], pooled[2]))
  }
  out
}


#' Worksheets that describe a covariate-adjusted analysis, for the downloads
#'
#' BE_Covariates: one row per parameter and model term, on the log scale of
#' the analysis. Covariate_Balance: the group summary. NULL without covariates.
#' @param be_results The BE result list of the app (covariates, covariate_coefs, covariate_balance)
#' @return named list of data frames, or NULL
be_covariate_sheets <- function(be_results) {
  if (is.null(be_results$covariates) || nrow(be_results$covariates) == 0) return(NULL)
  cc <- be_results$covariate_coefs
  coefs <- do.call(rbind, lapply(names(cc), function(p) {
    if (is.null(cc[[p]])) return(NULL)
    data.frame(Parameter = if (exists("friendly_name")) friendly_name(p) else p, cc[[p]], check.names = FALSE, row.names = NULL)
  }))
  bal <- be_results$covariate_balance
  if (!is.null(bal)) names(bal)[names(bal) == "Std_Diff"] <- "Standardized difference"
  Filter(Negate(is.null), list(BE_Covariates = coefs, Covariate_Balance = bal))
}
