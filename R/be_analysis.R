# ============================================================================
# NCA Assistant — Bioequivalence model fit and verdict
# ============================================================================
# Shiny-free, so the BE statistics can be tested directly by
# validation/validation.R instead of through a hand-maintained copy.
# mod_path_be.R prepares be_data (one row per NCA profile, merged with the
# design columns) and calls fit_be_parameter() once per PK parameter.

#' Fit the BE model for one PK parameter and derive the CI and verdict
#'
#' @param be_data   Data frame at NCA-profile grain with the design columns
#' @param param     PK parameter column, e.g. "CMAX"
#' @param design    "crossover_2x2", "crossover_fixed_order",
#'                  "crossover_3period", "parallel" or "replicate_2x2x4"
#' @param model_type "fixed" or "mixed"
#' @param trt_col,subj_col Treatment and subject columns. Treatment must be a
#'                  factor whose first level is the reference.
#' @param per_col,seq_col Period and sequence columns, or NULL
#' @param log_transform Analyse log(param); never applied to TMAX
#' @param ci_level  Confidence level in percent
#' @param be_lower,be_upper Acceptance limits in percent
#' @return list(row      = one-row data frame for the CI table,
#'              anova    = ANOVA table or NULL,
#'              estimate = unrounded list(pe, ci_lo, ci_hi, dfe, mse) or NULL,
#'              reason   = character explanation when no estimate, else NULL)
fit_be_parameter <- function(be_data, param, design, model_type = "fixed",
                             trt_col, subj_col, per_col = NULL, seq_col = NULL,
                             log_transform = TRUE, ci_level = 90,
                             be_lower = 80, be_upper = 125) {

  out <- list(row = NULL, anova = NULL, estimate = NULL, reason = NULL)
  trt_levels <- levels(be_data[[trt_col]])
  alpha <- 1 - ci_level / 100

  vals <- as.numeric(be_data[[param]])
  if (log_transform && param != "TMAX") {
    vals <- log(vals); vals[!is.finite(vals)] <- NA
  }
  be_data$.response <- vals

  # Guard: if Sequence has only 1 level, drop it (prevents lm() crash)
  if (!is.null(seq_col) && length(unique(be_data[[seq_col]])) < 2) {
    seq_col <- NULL
  }

  use_mixed <- model_type == "mixed" &&
               design != "parallel" &&
               design != "crossover_fixed_order" &&
               requireNamespace("nlme", quietly = TRUE)

  # Build and fit model
  if (design == "parallel") {
    fit <- tryCatch(lm(as.formula(paste(".response ~", trt_col)),
                       data = be_data, na.action = na.exclude), error = function(e) NULL)
  } else if (design == "crossover_fixed_order") {
    # Fixed-order crossover: all subjects received same sequence.
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
    random_f <- if (!is.null(seq_col)) paste0("~1|", seq_col, "/", subj_col) else paste0("~1|", subj_col)
    fit <- tryCatch(
      nlme::lme(fixed = as.formula(paste(".response~", paste(fixed_terms, collapse = "+"))),
                random = as.formula(random_f), data = be_data, na.action = na.exclude),
      error = function(e) {
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

  if (is.null(fit)) {
    out$row <- data.frame(Parameter = param, Point_Est = NA, CI_Lower = NA, CI_Upper = NA,
                          Bioequivalent = NA, stringsAsFactors = FALSE)
    return(out)
  }

  out$anova <- tryCatch({
    if (inherits(fit, "lme")) {
      # Type III (marginal) SS for lme — order-independent, correct for
      # unbalanced data. anova.lme with type="marginal" uses Wald F-tests.
      anova(fit, type = "marginal")
    } else {
      # drop1 with F-test gives Type III SS for lm objects.
      drop1(fit, test = "F")
    }
  }, error = function(e) NULL)

  is_lme <- inherits(fit, "lme")
  trt_coef_name <- paste0(trt_col, trt_levels[2])

  n1 <- sum(be_data[[trt_col]] == trt_levels[1] & !is.na(be_data$.response))
  n2 <- sum(be_data[[trt_col]] == trt_levels[2] & !is.na(be_data$.response))

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
      "All subjects have the same sequence — try selecting 'Fixed-order crossover' as the study design."
    } else if (!is.null(per_col) && length(unique(be_data[[per_col]])) < 2) {
      "Only one period found — try selecting 'Parallel groups' as the study design."
    } else {
      "The statistical model could not estimate the treatment effect. Check that the study design selection matches your data."
    }
    out$reason <- reason
    out$row <- data.frame(
      Parameter = param, Point_Est = NA, CI_Lower = NA, CI_Upper = NA,
      Bioequivalent = reason,
      stringsAsFactors = FALSE)
    return(out)
  }

  diff <- coef_result$diff; se_diff <- coef_result$se
  dfe <- coef_result$dfe; mse <- coef_result$mse

  t_crit <- qt(1 - alpha / 2, dfe)
  ci_lo <- diff - t_crit * se_diff
  ci_hi <- diff + t_crit * se_diff

  if (log_transform && param != "TMAX") {
    pe <- exp(diff) * 100; ci_lo_p <- exp(ci_lo) * 100; ci_hi_p <- exp(ci_hi) * 100
  } else { pe <- diff; ci_lo_p <- ci_lo; ci_hi_p <- ci_hi }

  be_pass <- ci_lo_p >= be_lower & ci_hi_p <= be_upper

  out$estimate <- list(pe = unname(pe), ci_lo = unname(ci_lo_p), ci_hi = unname(ci_hi_p),
                       dfe = unname(dfe), mse = unname(mse))
  out$row <- data.frame(
    Parameter = param, Test = as.character(trt_levels[2]),
    Reference = as.character(trt_levels[1]),
    N_Test = n2, N_Ref = n1,
    Point_Est = round(pe, 2),
    CI_Lower = round(ci_lo_p, 2), CI_Upper = round(ci_hi_p, 2),
    BE_Lower = be_lower, BE_Upper = be_upper,
    Bioequivalent = ifelse(be_pass, "YES", "NO"),
    MSE = round(mse, 6), DF = dfe, stringsAsFactors = FALSE
  )
  out
}
