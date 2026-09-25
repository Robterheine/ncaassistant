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
#' @return list(data, trt_col, subj_col, per_col, seq_col); Treatment is a
#'   factor with the reference as its first level.
build_be_data <- function(nca_res, pk_data, col_map, reference = NULL) {
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
  be <- merge(nca_res, design, by = keys, all.x = TRUE, sort = FALSE)
  if (nrow(be) != nrow(nca_res))
    stop("Design merge changed the number of profiles (", nrow(nca_res), " -> ",
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

  list(data = be, trt_col = "Treatment", subj_col = "Subject",
       per_col = if ("Period" %in% keys) "Period" else NULL, seq_col = seq_col)
}

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
#' parallel-design sample size needs.
within_cv_from_be <- function(ci_table, param = "CMAX") {
  if (is.null(ci_table) || !all(c("Parameter", "Scale", "MSE") %in% names(ci_table))) return(NA_real_)
  i <- match(param, ci_table$Parameter)
  if (is.na(i) || !grepl("^Ratio", ci_table$Scale[i]) || !is.finite(ci_table$MSE[i])) return(NA_real_)
  100 * sqrt(exp(ci_table$MSE[i]) - 1)
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
#'                  metric is judged against 80.00-125.00%) or "all" (e.g.
#'                  drug-interaction no-effect boundaries)
#' @param diff_unit Unit label for an untransformed difference, e.g. "h"
#' @param verdict   FALSE for a supportive metric: ratio and CI without a verdict
#' @return list(row      = one-row data frame for the CI table,
#'              anova    = ANOVA table or NULL,
#'              estimate = unrounded list(pe, ci_lo, ci_hi, dfe, mse) or NULL,
#'              reason   = character explanation when no estimate, else NULL)
fit_be_parameter <- function(be_data, param, design, model_type = "fixed",
                             trt_col, subj_col, per_col = NULL, seq_col = NULL,
                             log_transform = TRUE, ci_level = 90,
                             be_lower = 80, be_upper = 125,
                             pe_constraint = TRUE, widened_scope = "cmax",
                             diff_unit = NULL, verdict = TRUE) {

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
  widened <- be_lower < 80 || be_upper > 125
  if (widened && !identical(widened_scope, "all") && param != "CMAX") {
    be_lower <- 80; be_upper <- 125; widened <- FALSE
  }

  # Every row carries the same columns, so rows for parameters that could not
  # be estimated bind with the rest instead of breaking rbind().
  model_label <- NA_character_
  # Counts of profiles that cannot enter this metric's comparison, so a reader
  # sees how much of the design is left (a subject missing one period drops out)
  n_zero_t <- NA_integer_; n_zero_r <- NA_integer_
  n_miss_t <- NA_integer_; n_miss_r <- NA_integer_
  make_row <- function(pe = NA, lo = NA, hi = NA, n_t = NA, n_r = NA, o_t = NA, o_r = NA,
                       pe_status = NA, verdict = NA, mse = NA, dfe = NA) {
    data.frame(
      Parameter = param, Test = as.character(trt_levels[2]),
      Reference = as.character(trt_levels[1]),
      N_Test = n_t, N_Ref = n_r, Obs_Test = o_t, Obs_Ref = o_r, Scale = scale_label,
      Point_Est = pe, CI_Lower = lo, CI_Upper = hi,
      BE_Lower = if (has_limits) be_lower else NA,
      BE_Upper = if (has_limits) be_upper else NA,
      PE_Constraint = pe_status, Bioequivalent = verdict,
      Missing_Test = n_miss_t, Missing_Ref = n_miss_r,
      Zeros_Test = n_zero_t, Zeros_Ref = n_zero_r,
      MSE = mse, DF = dfe, Model = model_label, stringsAsFactors = FALSE)
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
  missing <- is.na(vals)
  n_miss_t <- sum(missing & trt == trt_levels[2]); n_miss_r <- sum(missing & trt == trt_levels[1])
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

  # Guard: if Sequence has only 1 level, drop it (prevents lm() crash)
  if (!is.null(seq_col) && length(unique(be_data[[seq_col]])) < 2) {
    seq_col <- NULL
  }

  use_mixed <- model_type == "mixed" &&
               model_family == "crossover" &&
               requireNamespace("nlme", quietly = TRUE)

  # Build and fit model
  if (model_family == "parallel") {
    fit <- tryCatch(lm(as.formula(paste(".response ~", trt_col)),
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

  out$estimate <- list(pe = pe, ci_lo = ci_lo_p, ci_hi = ci_hi_p,
                       dfe = unname(dfe), mse = unname(mse))
  out$row <- make_row(pe = round(pe, 2), lo = round(ci_lo_p, 2), hi = round(ci_hi_p, 2),
                      n_t = n2, n_r = n1, o_t = o2, o_r = o1,
                      pe_status = pe_status, verdict = verdict,
                      mse = round(mse, 6), dfe = unname(dfe))
  out
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
#' values would imply. It does not issue a scaled bioequivalence verdict.
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
#' - AUC(0-t) covering less than 80% of AUC(0-inf) in more than 20% of the
#'   profiles (M13A 2.2.2.2: the validity of the study may need discussion)
#' @param ci_df CI table of the run (N_Test, N_Ref per parameter)
#' @return character vector of messages, empty when all checks pass
be_m13a_checks <- function(pk_data, col_map, nca_res, ci_df, is_ss = FALSE) {
  out <- character(0)
  pk <- profile_key(pk_data, col_map)
  t <- suppressWarnings(as.numeric(pk_data[[col_map$time]]))
  cc <- suppressWarnings(as.numeric(pk_data[[col_map$conc]]))
  if (!isTRUE(is_ss)) {
    pre <- tapply(ifelse(!is.na(t) & t <= 0, cc, NA), pk$key, function(v) suppressWarnings(max(v, na.rm = TRUE)))
    cmax <- tapply(cc, pk$key, function(v) suppressWarnings(max(v, na.rm = TRUE)))
    high <- names(pre)[is.finite(pre) & is.finite(cmax) & cmax > 0 & pre > 0.05 * cmax]
    if (length(high) > 0) {
      lab <- profile_labels(pk$parts[match(high, pk$key), , drop = FALSE])
      out <- c(out, paste0("Pre-dose concentration above 5% of Cmax in ", length(high), " profile(s): ",
                           paste(head(lab, 5), collapse = "; "), if (length(lab) > 5) " and more" else "",
                           ". ICH M13A (2.2.3.3) excludes such a period from the primary analysis."))
    }
  }
  if (!is.null(ci_df) && all(c("N_Test", "N_Ref") %in% names(ci_df))) {
    n <- suppressWarnings(pmin(ci_df$N_Test, ci_df$N_Ref))
    if (any(!is.na(n) & n < 12))
      out <- c(out, paste0("Fewer than 12 evaluable subjects (smallest: ", min(n, na.rm = TRUE),
                           "). ICH M13A (2.2.3.1) does not accept a pivotal study with fewer than 12."))
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
  cv <- within_cv_from_be(be_results$ci_table, param)
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
  if (!scaled)
    return(list(cv = cv, cv_wr = NA_real_,
                label = sprintf("Use the %s %s CV from my BE analysis (%.1f%%)", name,
                                if (pl_parallel) "total" else "within-subject", cv)))
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
