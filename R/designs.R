# ============================================================================
# NCA Assistant — Bioequivalence study design registry
# ============================================================================
# One table describes every design the app knows: which the planner offers
# for which method, which the analysis accepts, and how each is modelled.
# Plan a Study and Bioequivalence Testing both build their menus from it, and
# the About page renders it, so the scope statement cannot drift from the code.
#
# code            analysis design code (also recorded in the Analysis Record)
# powertost_code  design name in PowerTOST (planner)
# model           "parallel", "crossover" (EMA Method A ANOVA) or "paired"
# plan_*          offered by Plan a Study for ABE / ABEL+RSABE / FDA NTID
# verdict         the analysis issues a bioequivalence verdict

BE_DESIGNS <- data.frame(
  code           = c("2x2x2", "2x2x3", "2x3x3", "2x2x4", "parallel", "paired"),
  label          = c("2×2 standard crossover (TR | RT)",
                     "2×2×3 full replicate (TRT | RTR)",
                     "2×3×3 partial replicate (TRR | RTR | RRT)",
                     "2×2×4 full replicate (TRTR | RTRT)",
                     "Parallel groups",
                     "Paired comparison (single sequence — not a bioequivalence design)"),
  powertost_code = c("2x2", "2x2x3", "2x3x3", "2x2x4", "parallel", "paired"),
  model          = c("crossover", "crossover", "crossover", "crossover", "parallel", "paired"),
  n_periods      = c(2, 3, 3, 4, 1, 2),
  n_sequences    = c(2, 2, 3, 2, NA, 1),
  replicated     = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
  plan_abe       = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  plan_scaled    = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
  plan_ntid      = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  verdict        = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  analysis_note  = c("Standard ABE, EMA Method A model.",
                     "ABE with the same model as 2×2×3; CVwR and CVwT reported (informational).",
                     "ABE; CVwR reported, CVwT not estimable (test given once).",
                     "ABE; CVwR and CVwT reported (informational).",
                     "ABE, one-way model on treatment.",
                     "Ratio and CI only; period and treatment are confounded, so no verdict."),
  stringsAsFactors = FALSE
)

# Design codes used by earlier versions (and possibly in saved records)
LEGACY_DESIGN_CODES <- c(crossover_2x2 = "2x2x2", replicate_2x2x4 = "2x2x4",
                         crossover_fixed_order = "paired", parallel = "parallel")

#' Model family for a design code ("parallel", "crossover" or "paired")
#' Accepts current and legacy codes; the legacy "crossover_3period" was used
#' for both 2x2x3 and 2x3x3, which share the crossover model.
be_design_model <- function(code) {
  if (identical(code, "crossover_3period")) return("crossover")
  if (code %in% names(LEGACY_DESIGN_CODES)) code <- LEGACY_DESIGN_CODES[[code]]
  m <- BE_DESIGNS$model[BE_DESIGNS$code == code]
  if (length(m) == 1) m else "crossover"
}

#' Analysis menu choices: label -> code
be_analysis_choices <- function() {
  stats::setNames(BE_DESIGNS$code, BE_DESIGNS$label)
}

#' Planner choices for a method: named vector PowerTOST code -> label
#' @param method "abe", "abel", "rsabe" or "ntid"
planner_designs <- function(method) {
  keep <- switch(method,
                 abe = BE_DESIGNS$plan_abe,
                 abel = , rsabe = BE_DESIGNS$plan_scaled,
                 ntid = BE_DESIGNS$plan_ntid,
                 BE_DESIGNS$plan_abe)
  stats::setNames(BE_DESIGNS$label[keep], BE_DESIGNS$powertost_code[keep])
}

#' Compare the selected design with the structure detected in the data
#'
#' @param code Selected design code
#' @param detected Output of detect_study_design()
#' @return NULL when consistent (or not checkable), otherwise a message
check_design_against_data <- function(code, detected) {
  if (is.null(detected)) return(NULL)
  row <- BE_DESIGNS[BE_DESIGNS$code == code, ]
  if (nrow(row) != 1) return(NULL)
  if (row$model == "parallel") {
    if (isTRUE(detected$n_periods > 1))
      return(paste0("Parallel groups was selected, but the data have ",
                    detected$n_periods, " periods."))
    return(NULL)
  }
  issues <- character(0)
  if (!is.na(row$n_periods) && !is.null(detected$n_periods) && detected$n_periods > 1 &&
      detected$n_periods != row$n_periods)
    issues <- c(issues, paste0(detected$n_periods, " periods (expected ", row$n_periods, ")"))
  if (!is.na(row$n_sequences) && !is.null(detected$n_sequences) && detected$n_sequences > 1 &&
      detected$n_sequences != row$n_sequences)
    issues <- c(issues, paste0(detected$n_sequences, " sequences (expected ", row$n_sequences, ")"))
  if (length(issues) == 0) return(NULL)
  paste0("The selected design (", row$label, ") does not match the data: ",
         paste(issues, collapse = " and "), ". Check the design selection and the ",
         "Period and Sequence columns.")
}

#' The CV argument for PowerTOST, as a fraction
#'
#' Average bioequivalence uses one within-subject CV. The scaled methods
#' (ABEL, RSABE, NTID) take c(CVwT, CVwR): the Test CV changes the sample
#' size, sometimes several-fold, so it must not be replaced by the Reference CV.
#' @param analysis_type "abe", "abel", "rsabe" or "ntid"
#' @param cv_pct Within-subject CV (Test CV for scaled methods), percent
#' @param cv_wr_pct Within-subject CV of the Reference, percent
planner_cv <- function(analysis_type, cv_pct, cv_wr_pct = NULL) {
  if (analysis_type %in% c("abel", "rsabe", "ntid")) {
    wr <- if (is.null(cv_wr_pct) || is.na(cv_wr_pct)) cv_pct else cv_wr_pct
    c(cv_pct, wr) / 100
  } else {
    cv_pct / 100
  }
}

#' Default expected Test/Reference ratio (%) for a planning method
#'
#' 95% for average bioequivalence; 90% for the scaled methods and 97.5% for
#' NTID, the defaults of PowerTOST (highly variable products tend to deviate
#' more from the reference; NTID products have tightened content limits).
planner_default_theta0 <- function(analysis_type) {
  switch(if (is.null(analysis_type)) "abe" else analysis_type, abel = 90, rsabe = 90, ntid = 97.5, 95)
}

#' Label of the first CV input in Plan a Study
#'
#' A parallel design needs the total CV (between- plus within-subject);
#' scaled methods need the Test product's within-subject CV.
planner_cv_label <- function(analysis_type, design) {
  if (identical(design, "parallel")) return("Total CV, between + within subjects (CV %)")
  if (!is.null(analysis_type) && analysis_type %in% c("abel", "rsabe", "ntid")) return("Within-subject CV of the Test product (CV %)")
  "Within-subject variability (CV %)"
}

#' Sample size for a planner method: the PowerTOST call the app makes
#' @param cv_dec CV as a fraction (ABE); cv_arg planner_cv() result (scaled)
#' @return PowerTOST's result data frame (column "Sample size")
planner_sample_size <- function(atype, alpha, targetpower, theta0, theta1, theta2, cv_dec, cv_arg, design) {
  switch(atype,
    "abe"   = PowerTOST::sampleN.TOST(alpha = alpha, targetpower = targetpower, theta0 = theta0,
                                      theta1 = theta1, theta2 = theta2, CV = cv_dec, design = design,
                                      method = "exact", print = FALSE),
    "abel"  = PowerTOST::sampleN.scABEL(alpha = alpha, targetpower = targetpower, theta0 = theta0,
                                        CV = cv_arg, design = design, print = FALSE, details = FALSE,
                                        nsims = 1e5),
    "rsabe" = PowerTOST::sampleN.RSABE(alpha = alpha, targetpower = targetpower, theta0 = theta0,
                                       CV = cv_arg, design = design, print = FALSE, details = FALSE,
                                       nsims = 1e5),
    "ntid"  = {
      # Renamed between PowerTOST versions (sampleN.NTIDFDA, later sampleN.NTID)
      ns <- asNamespace("PowerTOST")
      f <- get0("sampleN.NTIDFDA", envir = ns, inherits = FALSE)
      if (is.null(f)) f <- get0("sampleN.NTID", envir = ns, inherits = FALSE)
      if (is.null(f)) stop("NTID sample size function not found in PowerTOST")
      f(alpha = alpha, targetpower = targetpower, theta0 = theta0, CV = cv_arg, design = design,
        print = FALSE, details = FALSE, nsims = 1e5)
    },
    stop("Unknown study type: ", atype))
}

#' Power for a planner method and sample size; NA when it cannot be computed
planner_power <- function(n, atype, alpha, theta0, theta1, theta2, cv_dec, cv_wr_dec, design, nsims = 1e5) {
  if (is.null(cv_wr_dec) || is.na(cv_wr_dec)) cv_wr_dec <- cv_dec
  # Scaled methods need c(CVwT, CVwR); see planner_cv()
  cv_arg <- planner_cv(atype, cv_dec * 100, cv_wr_dec * 100)
  tryCatch(
    switch(atype,
      "abe"   = PowerTOST::power.TOST(alpha = alpha, theta0 = theta0, theta1 = theta1, theta2 = theta2,
                                      CV = cv_dec, n = n, design = design, method = "exact"),
      "abel"  = PowerTOST::power.scABEL(alpha = alpha, theta0 = theta0, CV = cv_arg, n = n,
                                        design = design, nsims = nsims),
      "rsabe" = PowerTOST::power.RSABE(alpha = alpha, theta0 = theta0, CV = cv_arg, n = n,
                                       design = design, nsims = nsims),
      "ntid"  = {
        ns <- asNamespace("PowerTOST")
        f <- get0("power.NTIDFDA", envir = ns, inherits = FALSE)
        if (is.null(f)) f <- get0("power.NTID", envir = ns, inherits = FALSE)
        if (is.null(f)) stop("NTID power function not found in PowerTOST")
        f(alpha = alpha, theta0 = theta0, CV = cv_arg, n = n, design = design, nsims = nsims)
      },
      NA_real_),
    error = function(e) NA_real_)
}
