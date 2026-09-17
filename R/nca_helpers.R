# ============================================================================
# NCA Assistant — NCA Helper Functions
# ============================================================================
# BLQ handling, lambda_z management, dose normalization


#' Human-readable label for each profile, e.g. "12 | Test | P3"
#' @param parts data.frame with Subject and optionally Treatment, Period
profile_labels <- function(parts) {
  lab <- as.character(parts$Subject)
  if ("Treatment" %in% names(parts)) lab <- paste(lab, "|", parts$Treatment)
  if ("Period" %in% names(parts))    lab <- paste0(lab, " | P", parts$Period)
  lab
}

#' Profile labels for an NCA result table, in row order
#' @param result NCA result with Subject and optionally Treatment, Period
result_profile_labels <- function(result) {
  cols <- intersect(c("Subject", "Treatment", "Period"), names(result))
  if (length(cols) == 0) return(as.character(result[[1]]))
  profile_labels(result[cols])
}

#' Unique profiles in an uploaded data set, ordered subject -> treatment -> period
#' @return data.frame of parts with a `label` column
data_profiles <- function(data, col_map) {
  pk <- profile_key(data, col_map)
  u <- unique(pk$parts)
  subj_order <- match(u$Subject, unique(as.character(data[[col_map$subject]])))
  per_num <- if ("Period" %in% names(u)) suppressWarnings(as.numeric(u$Period)) else NULL
  ord_args <- list(subj_order)
  if ("Treatment" %in% names(u)) ord_args <- c(ord_args, list(u$Treatment))
  if ("Period" %in% names(u))
    ord_args <- c(ord_args, list(if (anyNA(per_num)) u$Period else per_num))
  u <- u[do.call(order, ord_args), , drop = FALSE]
  u$label <- profile_labels(u)
  rownames(u) <- NULL
  u
}

#' Rows of the uploaded data belonging to one profile label, in time order
profile_data_rows <- function(data, col_map, label) {
  labs <- profile_labels(profile_key(data, col_map)$parts)
  idx <- which(labs == label)
  idx[order(suppressWarnings(as.numeric(as.character(data[[col_map$time]][idx]))))]
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


#' Estimate lambda_z for a single subject's concentration-time profile
#' 
#' Uses the same adjusted R² criterion as WinNonlin and NonCompart.
#' Returns the regression details for interactive review.
#'
#' @param time Numeric vector of time points
#' @param conc Numeric vector of concentrations
#' @param r2adj_threshold Minimum adjusted R² to accept (default 0.7)
#' @param exclude_indices Integer vector of indices to exclude (manual override)
#' @return List with slope, intercept, r2adj, points_used, half_life, etc.
estimate_lambda_z <- function(time, conc, r2adj_threshold = 0.7,
                              exclude_indices = NULL) {
  
  # Remove NAs and zero concentrations (can't log-transform)
  valid <- !is.na(conc) & conc > 0
  t  <- time[valid]
  cv <- conc[valid]  # renamed from c to avoid shadowing base::c()
  
  if (length(t) < 3) {
    return(list(
      lambda_z    = NA, half_life = NA, r2adj = NA,
      intercept   = NA, n_points  = 0,
      time_used   = numeric(0), conc_used = numeric(0),
      all_time    = time, all_conc = conc,
      valid_mask  = valid,
      message     = "Fewer than 3 non-zero points available"
    ))
  }
  
  # Find Cmax index (in the valid subset)
  cmax_idx <- which.max(cv)
  
  # Only use points after Cmax
  terminal_mask <- seq_along(t) > cmax_idx
  if (!is.null(exclude_indices)) {
    # Map exclude_indices from original data to valid subset
    terminal_mask[exclude_indices] <- FALSE
  }
  
  t_term <- t[terminal_mask]
  cv_term <- cv[terminal_mask]
  
  if (length(t_term) < 3) {
    return(list(
      lambda_z    = NA, half_life = NA, r2adj = NA,
      intercept   = NA, n_points  = 0,
      time_used   = numeric(0), conc_used = numeric(0),
      all_time    = time, all_conc = conc,
      valid_mask  = valid,
      message     = "Fewer than 3 points in terminal phase"
    ))
  }
  
  log_c_term <- log(cv_term)
  
  # Best fit method: try last 3, 4, 5, ... n points
  # Pick regression with highest adjusted R²
  best_r2adj  <- -Inf
  best_result <- NULL
  
  n <- length(t_term)
  for (np in 3:n) {
    idx <- (n - np + 1):n
    tt  <- t_term[idx]
    lc  <- log_c_term[idx]
    
    fit <- lm(lc ~ tt)
    ss  <- summary(fit)
    
    r2adj <- 1 - (1 - ss$r.squared) * (np - 1) / (np - 2)
    
    if (r2adj > best_r2adj) {
      best_r2adj <- r2adj
      best_result <- list(
        lambda_z  = -coef(fit)[2],
        intercept = coef(fit)[1],
        r2adj     = r2adj,
        n_points  = np,
        time_used = tt,
        conc_used = exp(lc),
        fit       = fit
      )
    }
  }
  
  if (is.null(best_result) || best_r2adj < r2adj_threshold) {
    return(list(
      lambda_z    = NA, half_life = NA, r2adj = best_r2adj,
      intercept   = NA, n_points  = 0,
      time_used   = numeric(0), conc_used = numeric(0),
      all_time    = time, all_conc = conc,
      valid_mask  = valid,
      message     = paste0("Best adj R² = ", round(best_r2adj, 4),
                           " < threshold ", r2adj_threshold)
    ))
  }
  
  best_result$half_life <- log(2) / best_result$lambda_z
  best_result$all_time  <- time
  best_result$all_conc  <- conc
  best_result$valid_mask <- valid
  best_result$message   <- "OK"
  
  best_result
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
    dose <- as.numeric(dose[as.character(subj)])
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
