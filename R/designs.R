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
