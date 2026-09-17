# ============================================================================
# NCA Assistant — NCA Helper Functions
# ============================================================================
# BLQ handling, lambda_z management, dose normalization



#' Profile labels for an NCA result table, in row order
#' @param result NCA result with Subject and optionally Treatment, Period
result_profile_labels <- function(result) {
  cols <- intersect(c("Subject", "Treatment", "Period"), names(result))
  if (length(cols) == 0) return(as.character(result[[1]]))
  profile_labels(result[cols])
}



#' Row index in an NCA result for one profile label (integer(0) if absent)
profile_result_row <- function(result, label) {
  which(result_profile_labels(result) == label)
}

#' Subject / Treatment / Period of a profile label, for audit logs
profile_parts <- function(result, label) {
  i <- profile_result_row(result, label)
  if (length(i) != 1) return(list())
  cols <- intersect(c("Subject", "Treatment", "Period"), names(result))
  stats::setNames(lapply(cols, function(cc) as.character(result[[cc]][i])), tolower(cols))
}


#' Automatic lambda_z for one profile, as NonCompart computes it
#'
#' Uses NonCompart::BestSlope(), the slope selection behind the results table
#' (sNCA/tblNCA), so the half-life review shows exactly the fit that is used:
#' the same points, tolerance and exclusion of Cmax for extravascular dosing.
#' A fit below the adjusted R2 threshold is reported as not used, matching
#' run_nca(), which sets the half-life-based parameters to missing.
#'
#' @param time,conc Numeric vectors of one profile
#' @param r2adj_threshold Minimum adjusted R2 (default 0.7)
#' @param exclude_indices Unused; kept for compatibility
#' @param route "extravascular", "iv_bolus" or "iv_infusion"
#' @return list(lambda_z, half_life, r2adj, intercept, n_points, time_used,
#'   conc_used, all_time, all_conc, valid_mask, message)
estimate_lambda_z <- function(time, conc, r2adj_threshold = 0.7,
                              exclude_indices = NULL, route = "extravascular") {
  time <- suppressWarnings(as.numeric(as.character(time)))
  conc <- suppressWarnings(as.numeric(as.character(conc)))
  valid <- !is.na(conc) & conc > 0
  empty <- function(msg, r2 = NA) list(
    lambda_z = NA, half_life = NA, r2adj = r2, intercept = NA, n_points = 0,
    time_used = numeric(0), conc_used = numeric(0),
    all_time = time, all_conc = conc, valid_mask = valid, message = msg)

  keep <- !is.na(time) & !is.na(conc)
  x <- time[keep]; y <- conc[keep]
  ord <- order(x); x <- x[ord]; y <- y[ord]
  if (sum(y > 0) < 3) return(empty("Fewer than 3 non-zero points available"))

  adm <- switch(route, "iv_bolus" = "Bolus", "iv_infusion" = "Infusion", "Extravascular")
  bs <- tryCatch(NonCompart::BestSlope(x, y, adm = adm), error = function(e) NULL)
  if (is.null(bs) || is.na(bs["LAMZ"]) || bs["LAMZ"] <= 0)
    return(empty("No terminal phase could be fitted"))
  used <- attr(bs, "UsedPoints")
  r2adj <- unname(bs["R2ADJ"])
  if (!is.null(r2adj_threshold) && !is.na(r2adj_threshold) && r2adj < r2adj_threshold)
    return(empty(paste0("Best adj R\u00b2 = ", round(r2adj, 4), " < threshold ", r2adj_threshold,
                        ": half-life and the parameters derived from it are not reported"), r2adj))

  list(lambda_z = unname(bs["LAMZ"]), half_life = log(2) / unname(bs["LAMZ"]),
       r2adj = r2adj, intercept = unname(bs["b0"]), n_points = length(used),
       time_used = x[used], conc_used = y[used],
       all_time = time, all_conc = conc, valid_mask = valid, message = "OK")
}

#' Add dose-normalized parameters to NCA results
#' 
#' @param nca_result Data frame of NCA results (from tblNCA)
#' @param dose Numeric dose value (or vector per subject)
#' @return Data frame with additional _DN columns
add_dose_normalized <- function(nca_result, dose) {
  # A per-subject dose vector is matched to each profile by subject ID. A
  # crossover has several profiles per subject, and the result is sorted by
  # profile key, so dividing by position would use other subjects' doses.
  if (length(dose) > 1 && !is.null(names(dose))) {
    subj <- if ("Subject" %in% names(nca_result)) nca_result$Subject else nca_result[[1]]
    cols <- intersect(c("Subject", "Treatment", "Period"), names(nca_result))
    key <- if (length(cols) > 1) do.call(paste, c(lapply(nca_result[cols], as.character), sep = "||")) else as.character(subj)
    dose <- as.numeric(if (all(key %in% names(dose))) dose[key] else dose[as.character(subj)])
  }
  # Parameters that should be dose-normalized
  dn_params <- c("CMAX", "AUCLST", "AUCIFO", "AUCIFP",
                  "AUMCLST", "AUMCIFO", "AUMCIFP")
  
  for (p in dn_params) {
    if (p %in% names(nca_result)) {
      new_name <- paste0(p, "_DN")
      nca_result[[new_name]] <- as.numeric(nca_result[[p]]) / dose
    }
  }
  
  nca_result
}



# =============================================================================
# recalculate_lambda_z — shared helper for manual lambda-z override
# =============================================================================
# Called by all three Half-Life Review modules (single NCA, batch NCA, BE).
# Extracts the shared computation from what was previously triplicated code.
#
# @param time_vals  Numeric vector: all time values for this profile
# @param conc_vals  Numeric vector: all concentration values (same length)
# @param selected_idx Integer vector: indices of points the user selected
#
# @return Named list with three elements:
#   $result  — override list (lambda_z, half_life, intercept, r2adj, n_points,
#              time_used, message), or NULL on failure
#   $error   — character error message if validation failed, else NULL
#   $warning — character warning (2-point case), else NULL
#
# The $result list is structurally identical to the override list consumed by
# lz_state$override and local$lz_override in each module — do not change
# field names without updating all three modules.
recalculate_lambda_z <- function(time_vals, conc_vals, selected_idx) {

  out <- list(result = NULL, error = NULL, warning = NULL)

  # Guard 1: need at least 2 selected indices
  if (length(selected_idx) < 2) {
    out$error <- "Select at least 2 points for the regression."
    return(out)
  }

  # Extract selected time/conc — coerce to numeric defensively
  t_sel <- suppressWarnings(as.numeric(time_vals[selected_idx]))
  c_sel <- suppressWarnings(as.numeric(conc_vals[selected_idx]))

  # Guard 2: keep only positive, non-NA concentrations
  valid <- !is.na(c_sel) & !is.na(t_sel) & c_sel > 0
  t_sel <- t_sel[valid]
  c_sel <- c_sel[valid]

  if (length(t_sel) < 2) {
    out$error <- paste0("Need at least 2 points with positive concentration. ",
                        "The selected points include only ",
                        sum(valid), " positive value(s).")
    return(out)
  }

  # Fit log-linear regression: ln(C) = intercept - lambda_z * t
  fit <- tryCatch(
    lm(log(c_sel) ~ t_sel),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    out$error <- "Regression failed — check that selected points span a range of times."
    return(out)
  }

  cf          <- coef(fit)
  lambda_z    <- -cf[2]
  intercept   <- cf[1]
  n_pts       <- length(t_sel)

  # Guard 3: negative or zero lambda_z means ascending/flat slope
  if (is.na(lambda_z) || lambda_z <= 0) {
    out$error <- paste0("The selected points have an ascending or flat slope \u2014 ",
                        "they do not represent a terminal elimination phase. ",
                        "Select points from the descending part of the curve.")
    return(out)
  }

  half_life <- log(2) / lambda_z

  # R-squared adjusted (not meaningful with only 2 points)
  ss_res <- sum(residuals(fit)^2)
  ss_tot <- sum((log(c_sel) - mean(log(c_sel)))^2)
  r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  r2adj  <- if (n_pts >= 3 && !is.na(r2)) {
    1 - (1 - r2) * (n_pts - 1) / (n_pts - 2)
  } else {
    NA_real_
  }

  # Warning for 2-point case
  if (n_pts == 2) {
    out$warning <- paste0("Half-life computed from 2 points (R\u00B2 not available \u2014 ",
                          "at least 3 points needed for validation).")
  }

  out$result <- list(
    lambda_z  = as.numeric(lambda_z),
    half_life = as.numeric(half_life),
    intercept = as.numeric(intercept),
    r2adj     = as.numeric(r2adj),
    n_points  = n_pts,
    time_used = t_sel,
    message   = "User-selected"
  )

  out
}
