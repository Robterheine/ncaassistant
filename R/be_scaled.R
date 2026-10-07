# ============================================================================
# NCA Assistant — Reference-scaled bioequivalence: FDA RSABE and EMA ABEL
# ============================================================================
# Shiny-free, like R/be_analysis.R, so validation/validation.R can test it.
#
# The analyst chooses the approach before the run (the choice is the
# prospective declaration); the app never switches by itself.
#   RSABE  FDA, "Statistical Approaches to Establishing Bioequivalence"
#          (May 2026), Appendix G. Designs 2x3x3 and 2x2x4. 2x2x3 is not
#          covered by the appendix and is refused.
#   ABEL   EMA CPMP/EWP/QWP/1401/98 Rev. 1, section 4.1.10. Cmax only; all
#          replicate designs, 2x2x3 included.
# Both build on fit_be_parameter(): the standard route is that fit.

RSABE_SIGMA_W0 <- 0.25                                  # FDA regulatory constant
RSABE_THETA    <- (log(1.25) / RSABE_SIGMA_W0)^2         # 0.7967
RSABE_SWITCH   <- 0.294                                 # s_WR at which scaling starts
RSABE_MIN_SUBJECTS <- 24L                               # FDA recommendation

#' Designs each approach can assess
BE_SCALED_DESIGNS <- list(rsabe = c("2x3x3", "2x2x4"), abel = c("2x2x3", "2x3x3", "2x2x4"))

#' Approaches offered for a design, as a named vector (label -> code)
be_approach_choices <- function(design) {
  ch <- c("Standard (average bioequivalence)" = "standard")
  if (design %in% BE_SCALED_DESIGNS$abel) ch <- c(ch, "EMA ABEL (Cmax, widened limits)" = "abel")
  if (design %in% BE_SCALED_DESIGNS$rsabe) ch <- c(ch, "FDA RSABE (reference-scaled)" = "rsabe")
  ch
}

#' Per-subject contrasts for the FDA RSABE assessment
#'
#' Complete cases only: a subject enters when every period of the design has
#' a value. Administrations are ordered by period. I is the test mean minus
#' the reference mean; D is the first minus the second reference value.
#' @return list(subjects = data.frame(subject, sequence, I, D), n_incomplete)
rsabe_contrasts <- function(be_data, param, design, trt_col, subj_col, per_col, seq_col) {
  n_per <- BE_DESIGNS$n_periods[BE_DESIGNS$code == design]
  y <- suppressWarnings(log(as.numeric(be_data[[param]])))
  excl <- if ("EXCLUDED" %in% names(be_data)) !is.na(be_data$EXCLUDED) & nzchar(be_data$EXCLUDED) else rep(FALSE, nrow(be_data))
  y[excl | !is.finite(y)] <- NA
  d <- data.frame(subj = as.character(be_data[[subj_col]]), per = as.numeric(as.character(be_data[[per_col]])),
                  trt = as.character(be_data[[trt_col]]), seq = as.character(be_data[[seq_col]]), y = y,
                  stringsAsFactors = FALSE)
  lv <- levels(factor(be_data[[trt_col]])); ref <- lv[1]; tst <- lv[2]
  rows <- lapply(split(d, d$subj), function(s) {
    s <- s[order(s$per), ]
    if (nrow(s) != n_per || anyNA(s$y) || anyDuplicated(s$per)) return(NULL)
    r <- s$y[s$trt == ref]; t <- s$y[s$trt == tst]
    if (length(r) != 2 || length(t) != n_per - 2) return(NULL)
    data.frame(subject = s$subj[1], sequence = s$seq[1], I = mean(t) - mean(r), D = r[1] - r[2],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  list(subjects = out, n_incomplete = length(unique(d$subj)) - if (is.null(out)) 0L else nrow(out))
}

#' FDA reference-scaled average bioequivalence (RSABE), Appendix G
#'
#' Steps: I ~ Sequence gives the equally weighted sequence-mean estimate, its
#' standard error and 90% limits (t, n - m df); x = estimate^2 - se^2; boundx
#' is the larger squared 90% limit; D ~ Sequence gives s2wr = MSE/2 and the
#' 95% upper bound boundy of -theta * s2wr (chi-square); the criterion bound
#' is (x + y) + sqrt((boundx - x)^2 + (boundy - y)^2). RSABE is met when it is
#' at or below 0 and the point estimate lies within 80.00-125.00%. Below the
#' switch (s_WR < 0.294) no scaling applies: the caller uses the standard
#' route (here only flagged).
#' @return list(ok, reason, n, n_incomplete, seqs, pe, ci_lo, ci_hi (percent), est, se, dfi,
#'   sWR, dfd, x, boundx, y, boundy, critbound, scaled (sWR >= switch), pe_ok, pass,
#'   limit_lo, limit_hi (implied scaled limits, percent))
rsabe_assess <- function(be_data, param, design, trt_col, subj_col, per_col, seq_col) {
  if (!design %in% BE_SCALED_DESIGNS$rsabe)
    return(list(ok = FALSE, reason = paste0("FDA RSABE is defined for the 2x3x3 partial replicate and the ",
                                            "2x2x4 full replicate. The 2x2x3 design is not covered by the FDA method; use ",
                                            "the standard approach or EMA ABEL.")))
  if (is.null(per_col) || is.null(seq_col))
    return(list(ok = FALSE, reason = "RSABE needs the Period and Sequence columns to be mapped."))
  if (anyNA(suppressWarnings(as.numeric(as.character(be_data[[per_col]])))))
    return(list(ok = FALSE, reason = "RSABE needs the Period values to be numbers (1, 2, 3, ...), so that the administrations can be put in order."))
  cc <- rsabe_contrasts(be_data, param, design, trt_col, subj_col, per_col, seq_col)
  s <- cc$subjects; n <- if (is.null(s)) 0L else nrow(s)
  seqs <- if (n > 0) length(unique(s$sequence)) else 0L
  if (n < 3 || seqs < 1 || n - seqs < 2)
    return(list(ok = FALSE, n = n, n_incomplete = cc$n_incomplete,
                reason = "Too few subjects with data in every period for the RSABE assessment."))
  if (seqs < 2)
    return(list(ok = FALSE, n = n, n_incomplete = cc$n_incomplete,
                reason = "RSABE needs at least two sequences; all subjects are in one."))
  s$sequence <- factor(s$sequence)
  fi <- lm(I ~ sequence, data = s); fd <- lm(D ~ sequence, data = s)
  m <- nlevels(s$sequence); dfi <- n - m
  mse_i <- sum(residuals(fi)^2) / dfi
  nk <- as.numeric(table(s$sequence))
  est <- mean(tapply(s$I, s$sequence, mean))
  se <- sqrt(mse_i * sum((1 / m)^2 / nk))
  tq <- qt(0.95, dfi); lcl <- est - tq * se; ucl <- est + tq * se
  x <- est^2 - se^2; boundx <- max(abs(lcl), abs(ucl))^2
  dfd <- n - m; s2wr <- sum(residuals(fd)^2) / dfd / 2; swr <- sqrt(s2wr)
  y <- -RSABE_THETA * s2wr; boundy <- y * dfd / qchisq(0.95, dfd)
  crit <- (x + y) + sqrt((boundx - x)^2 + (boundy - y)^2)
  pe <- 100 * exp(est)
  pe_ok <- pe_within_limits(pe)
  scaled <- swr >= RSABE_SWITCH
  list(ok = TRUE, n = n, n_incomplete = cc$n_incomplete, seqs = m,
       pe = pe, ci_lo = 100 * exp(lcl), ci_hi = 100 * exp(ucl), est = est, se = se, dfi = dfi, mse_i = mse_i,
       sWR = swr, dfd = dfd, x = x, boundx = boundx, y = y, boundy = boundy, critbound = crit,
       scaled = scaled, pe_ok = pe_ok, pass = scaled && crit <= 0 && pe_ok,
       limit_lo = 100 * exp(-log(1.25) * swr / RSABE_SIGMA_W0), limit_hi = 100 * exp(log(1.25) * swr / RSABE_SIGMA_W0))
}

#' Point-estimate constraint of RSABE and ABEL: the ratio within 80.00-125.00%
#'
#' Rounded to two decimals in percent (four in the ratio, as in FDA Appendix G
#' step 3b, [0.8000, 1.2500]), the same at both edges. Rounding to four
#' significant figures kept one decimal above 100% and so accepted 125.049%.
pe_within_limits <- function(pe) round(pe, 2) >= 80 && round(pe, 2) <= 125

#' Assess one parameter with the chosen approach
#'
#' Wraps fit_be_parameter(). Standard: the plain fit. ABEL: Cmax is judged
#' against limits computed from CVwR of this run (EMA model, replicateBE
#' Method A); other metrics stay at the entered limits. RSABE: every
#' log-transformed metric with s_WR at or above 0.294 is judged by the FDA
#' criterion, below it by the standard fit (Route "Standard"). Extra columns,
#' present only when a scaled approach was chosen: Approach, Route, s_WR,
#' Scaled_Lower, Scaled_Upper, Crit_Bound.
#'
#' @param approach "standard", "abel" or "rsabe"
#' @param ... arguments of fit_be_parameter()
#' @return like fit_be_parameter(), with `scaled` (details) when a scaled route was used
be_assess_parameter <- function(approach = "standard", be_data, param, design, ..., trt_col, subj_col,
                                per_col = NULL, seq_col = NULL) {
  args <- list(...)
  fit <- function(a = args) do.call(fit_be_parameter, c(list(be_data, param, design = design, trt_col = trt_col,
                                    subj_col = subj_col, per_col = per_col, seq_col = seq_col), a))
  if (identical(approach, "standard")) return(fit())
  if (!design %in% BE_SCALED_DESIGNS[[approach]])
    stop(structure(class = c("be_scaled_error", "error", "condition"),
                   list(message = paste0(if (approach == "rsabe") "FDA RSABE" else "EMA ABEL",
                                         " is not available for the design ", design, "."), call = NULL)))
  is_ratio <- isTRUE(if (is.null(args$log_transform)) TRUE else args$log_transform) && param != "TMAX" && !param %in% BE_NO_VERDICT_PARAMS
  add_cols <- function(out, route, swr, lo, hi, crit) {
    out$row$Approach <- if (approach == "rsabe") "FDA RSABE" else "EMA ABEL"
    out$row$Route <- route; out$row$s_WR <- swr; out$row$Scaled_Lower <- lo; out$row$Scaled_Upper <- hi
    out$row$Crit_Bound <- crit
    out
  }
  if (approach == "abel") {
    if (param != "CMAX" || !is_ratio) return(add_cols(fit(), "Standard", NA_real_, NA_real_, NA_real_, NA_real_))
    v <- within_subject_variability(be_data, param, levels(factor(be_data[[trt_col]]))[1], trt_col, subj_col, per_col, seq_col)
    if (is.null(v)) {
      out <- fit(); out$reason <- "no scaled verdict: the reference was not repeated in enough subjects to estimate CVwR."
      out$row$Bioequivalent <- out$reason
      return(add_cols(out, "Standard", NA_real_, NA_real_, NA_real_, NA_real_))
    }
    lim <- abel_limits(v$cv)
    # The EMA point-estimate condition is part of the method; the checkbox of the fixed-limits mode does not apply
    a <- args; a$be_lower <- lim[1]; a$be_upper <- lim[2]; a$widened_scope <- "cmax"; a$pe_constraint <- TRUE
    out <- fit(a)
    out$scaled <- list(approach = "abel", CVwR = v$cv, sWR = v$sw, df = v$df, lower = lim[1], upper = lim[2])
    return(add_cols(out, if (v$cv > 30 && is.null(out$reason)) "Scaled" else "Standard", v$sw, lim[1], lim[2], NA_real_))
  }
  # RSABE
  if (!is_ratio) return(add_cols(fit(), "Standard", NA_real_, NA_real_, NA_real_, NA_real_))
  r <- rsabe_assess(be_data, param, design, trt_col, subj_col, per_col, seq_col)
  std <- fit()
  # A metric the standard fit refused (zero values, an interval past the last concentration) stays refused
  if (!is.null(std$reason)) return(add_cols(std, "Standard", NA_real_, NA_real_, NA_real_, NA_real_))
  if (!isTRUE(r$ok)) {
    out <- std; out$reason <- paste0("no scaled verdict: ", r$reason); out$row$Bioequivalent <- out$reason
    return(add_cols(out, "Standard", NA_real_, NA_real_, NA_real_, NA_real_))
  }
  if (!r$scaled) {   # below the switch: the app's ABE model (fixed or mixed, as chosen)
    out <- std; out$scaled <- r
    return(add_cols(out, "Standard", r$sWR, NA_real_, NA_real_, r$critbound))
  }
  out <- std
  # The assessment is a 90% procedure. At another level the standard interval stays as it is, labelled
  # with its own level, and there is no scaled result
  ci_level <- if (is.null(args$ci_level)) 90 else args$ci_level
  if (!isTRUE(all.equal(as.numeric(ci_level), 90))) {
    out$row$Bioequivalent <- if (isFALSE(args$verdict)) "no verdict" else
      paste0("no verdict: the RSABE assessment uses the 90% confidence interval (this is ", ci_level, "%)")
    out$scaled <- r
    return(add_cols(out, "Standard", r$sWR, NA_real_, NA_real_, NA_real_))
  }
  # Scaled route: the estimate, 90% CI and verdict come from the FDA contrasts
  if (isFALSE(args$verdict)) {          # a supportive metric: ratio and interval, no conclusion
    out$row$Bioequivalent <- "no verdict"
  } else {
    out$row$Bioequivalent <- if (r$pass) "YES" else "NO"
  }
  out$row$Point_Est <- round(r$pe, 2); out$row$CI_Lower <- round(r$ci_lo, 2); out$row$CI_Upper <- round(r$ci_hi, 2)
  out$row$BE_Lower <- r$limit_lo; out$row$BE_Upper <- r$limit_hi
  out$row$PE_Constraint <- if (isFALSE(args$verdict)) "not applicable" else if (r$pe_ok) "YES" else "NO"
  out$row$N_Test <- r$n; out$row$N_Ref <- r$n; out$row$DF <- r$dfi; out$row$MSE <- round(r$mse_i, 6)
  out$row$Model <- "RSABE (intra-subject contrasts, FDA Appendix G)"
  out$estimate$pe <- r$pe; out$estimate$ci_lo <- r$ci_lo; out$estimate$ci_hi <- r$ci_hi
  out$scaled <- r
  add_cols(out, "Scaled", r$sWR, r$limit_lo, r$limit_hi, r$critbound)
}

#' One plain-language line per parameter for a scaled run
#'
#' Reads a row of the CI table (with the scaled columns) and says which route
#' was taken and why, for example "s_WR 0.41 at or above 0.294, scaled route;
#' criterion bound -0.12 at or below 0; point estimate 97.8% inside 80 to 125%".
#' @param row one-row data frame from be_assess_parameter()
#' @param name display name of the parameter
#' @param ci_level confidence level of the analysis, in percent
#' @return character(1), or NULL when the row has no verdict or no scaled columns
be_scaled_explain <- function(row, name, ci_level = 90) {
  if (!"Route" %in% names(row) || !row$Bioequivalent %in% c("YES", "NO")) return(NULL)
  pe <- row$Point_Est; inside <- function(v) if (isTRUE(v)) "inside" else "outside"
  pe_ok <- pe_within_limits(pe)
  near <- is.finite(row$s_WR) && abs(row$s_WR - RSABE_SWITCH) <= 0.02
  tail <- if (near) " s_WR is close to the switch, so a small change in the data can change the route." else ""
  met <- if (row$Bioequivalent == "YES") "Bioequivalence concluded." else "Bioequivalence not concluded."
  if (row$Approach == "FDA RSABE") {
    if (row$Route == "Scaled")
      return(sprintf("%s: s_WR %.3f at or above %.3f, scaled route; criterion bound %.4f %s 0; point estimate %.1f%% %s 80 to 125%%. %s%s",
                     name, row$s_WR, RSABE_SWITCH, row$Crit_Bound, if (row$Crit_Bound <= 0) "at or below" else "above",
                     pe, inside(pe_ok), met, tail))
    return(sprintf("%s: s_WR %.3f below %.3f, so no scaling: the standard test against 80 to 125%% with the model chosen under Statistical model, not the FDA replicate model (%s%% CI %.2f to %.2f%%). %s%s",
                   name, row$s_WR, RSABE_SWITCH, ci_level, row$CI_Lower, row$CI_Upper, met, tail))
  }
  if (!is.finite(row$s_WR))
    return(sprintf("%s: standard limits, 80 to 125%% (the EMA widens Cmax only). %s", name, met))
  cv <- 100 * sqrt(exp(row$s_WR^2) - 1)
  if (row$Route == "Scaled")
    return(sprintf("%s: CVwR %.1f%%, above 30%%; limits %.2f to %.2f%%; %s%% CI %.2f to %.2f%% %s the limits; point estimate %.1f%% %s 80 to 125%%. %s%s",
                   name, cv, row$Scaled_Lower, row$Scaled_Upper, ci_level, row$CI_Lower, row$CI_Upper,
                   inside(row$CI_Lower >= row$Scaled_Lower && row$CI_Upper <= row$Scaled_Upper), pe, inside(pe_ok), met, tail))
  sprintf("%s: CVwR %.1f%%, at or below 30%%, so the standard limits of 80 to 125%% apply. %s%s", name, cv, met, tail)
}

#' Notes on a scaled run: subject numbers and the switch
#'
#' The FDA recommends at least 24 evaluable subjects for RSABE; the assessment
#' is still computed. Subjects who lack a period are left out of the
#' contrasts, and their number is reported.
#' @param ci_df CI table of the run
#' @param details named list of the `scaled` elements of be_assess_parameter()
#' @return character vector
be_scaled_notes <- function(ci_df, details) {
  out <- character(0)
  if (is.null(ci_df) || !"Approach" %in% names(ci_df)) return(out)
  if (any(ci_df$Approach == "FDA RSABE", na.rm = TRUE)) {
    n <- suppressWarnings(min(unlist(lapply(details, function(d) if (isTRUE(d$ok)) d$n else NA)), na.rm = TRUE))
    if (is.finite(n) && n < RSABE_MIN_SUBJECTS)
      out <- c(out, sprintf(paste0("The FDA recommends at least %d evaluable subjects for RSABE (found %d in the contrast). ",
                                   "The assessment is computed anyway; discuss the sample size."), RSABE_MIN_SUBJECTS, n))
    lost <- unique(unlist(lapply(details, function(d) if (isTRUE(d$ok)) d$n_incomplete else NULL)))
    if (any(lost > 0))
      out <- c(out, sprintf(paste0("Subjects who lack a value in one or more periods are left out of the RSABE contrasts ",
                                   "(up to %d subject(s)); the numbers of subjects used are in the downloads."), max(lost)))
    below <- ci_df$Approach == "FDA RSABE" & ci_df$Route == "Standard" & is.finite(ci_df$s_WR) &
      ci_df$Bioequivalent %in% c("YES", "NO")
    if (any(below)) {
      nm <- as.character(ci_df$Parameter[below])
      if (exists("friendly_name")) nm <- vapply(nm, friendly_name, character(1), USE.NAMES = FALSE)
      out <- c(out, paste0("Below the switch (s_WR under 0.294), FDA RSABE falls back to the ordinary test, and the app ",
                           "runs it with the model chosen under Statistical model (EMA Method A or the mixed model). ",
                           "For replicate designs the FDA prescribes another model (Appendix C), with separate Test and ",
                           "Reference variances, a subject-by-formulation interaction and Satterthwaite degrees of freedom. ",
                           "The two can give different intervals, most of all with unbalanced sequences or unequal Test and ",
                           "Reference variability. For an FDA submission, check these rows with the FDA model: ",
                           paste(nm, collapse = ", "), "."))
    }
  }
  out
}

#' Constants and rules of the chosen approach, for the record
be_scaled_settings <- function(approach) {
  switch(approach,
    rsabe = list(analysis_approach = "FDA RSABE", source = "FDA, Statistical Approaches to Establishing Bioequivalence (May 2026), Appendix G",
                 sigma_w0 = RSABE_SIGMA_W0, theta = RSABE_THETA, switch_s_WR = RSABE_SWITCH,
                 point_estimate_limits_percent = c(80, 125), confidence_level_percent = 90, criterion_upper_bound_percent = 95,
                 minimum_subjects_recommended = RSABE_MIN_SUBJECTS),
    abel = list(analysis_approach = "EMA ABEL", source = "EMA CPMP/EWP/QWP/1401/98 Rev. 1, section 4.1.10",
                k = 0.76, cv_switch_percent = 30, cv_cap_percent = 50, widened_metrics = "Cmax",
                point_estimate_limits_percent = c(80, 125)),
    NULL)
}

#' Worksheet with the steps of the scaled assessment, one row per parameter
#' @param be_results BE result list of the app (ci_table, scaled_details)
#' @return data.frame or NULL
be_scaled_sheet <- function(be_results) {
  ci <- be_results$ci_table; det <- be_results$scaled_details
  if (is.null(ci) || !"Approach" %in% names(ci) || length(det) == 0) return(NULL)
  g <- function(d, k) if (!is.null(d[[k]]) && length(d[[k]]) == 1) d[[k]] else NA
  do.call(rbind, lapply(seq_len(nrow(ci)), function(i) {
    d <- det[[as.character(ci$Parameter[i])]]
    data.frame(Parameter = ci$Parameter[i], Approach = ci$Approach[i], Route = ci$Route[i], s_WR = ci$s_WR[i],
               DF_reference = g(d, "dfd"), x = g(d, "x"), boundx = g(d, "boundx"), y = g(d, "y"), boundy = g(d, "boundy"),
               Crit_Bound = ci$Crit_Bound[i], Point_Est = ci$Point_Est[i],
               Limit_Lower = if (ci$Route[i] == "Scaled") ci$Scaled_Lower[i] else NA_real_,
               Limit_Upper = if (ci$Route[i] == "Scaled") ci$Scaled_Upper[i] else NA_real_,
               Subjects_used = g(d, "n"), Subjects_left_out = g(d, "n_incomplete"),
               Verdict = ci$Bioequivalent[i], stringsAsFactors = FALSE)
  }))
}
