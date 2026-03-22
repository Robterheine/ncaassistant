# ============================================================================
# NCA Assistant — NCA Helper Functions
# ============================================================================
# BLQ handling, lambda_z management, dose normalization

#' Apply BLQ (Below Limit of Quantification) handling rules
#' 
#' Implements WinNonlin-compatible BLQ rules:
#'   Rule 1: Pre-first-quantifiable set to 0; post-last-quantifiable set to Missing
#'   Rule 2: All BLQ set to 0
#'   Rule 3: All BLQ set to Missing (NA)
#'   Rule 4: All BLQ set to LLOQ/2
#'   Rule 5: Pre-Cmax BLQ = 0; post-Cmax BLQ = Missing
#'
#' @param data Data frame with subject/time/concentration
#' @param col_map Column mapping list
#' @param rule Character: one of "rule1" through "rule5"
#' @param lloq Numeric: lower limit of quantification
#' @return Modified data frame
apply_blq_rules <- function(data, col_map, rule = "rule1", lloq = 0) {
  
  subj_col <- col_map$subject
  time_col <- col_map$time
  conc_col <- col_map$conc
  
  # Identify BLQ values
  data$.is_blq <- !is.na(data[[conc_col]]) & data[[conc_col]] < lloq
  
  if (rule == "rule2") {
    # All BLQ -> 0
    data[[conc_col]][data$.is_blq] <- 0
    
  } else if (rule == "rule3") {
    # All BLQ -> NA
    data[[conc_col]][data$.is_blq] <- NA
    
  } else if (rule == "rule4") {
    # All BLQ -> LLOQ/2
    data[[conc_col]][data$.is_blq] <- lloq / 2
    
  } else if (rule == "rule5") {
    # Pre-Cmax BLQ -> 0; post-Cmax BLQ -> NA
    subjects <- unique(data[[subj_col]])
    for (s in subjects) {
      idx <- which(data[[subj_col]] == s)
      sub <- data[idx, ]
      tmax_idx <- which.max(sub[[conc_col]])
      
      pre_cmax  <- idx[1:tmax_idx]
      post_cmax <- if (tmax_idx < length(idx)) idx[(tmax_idx + 1):length(idx)] else integer(0)
      
      data[[conc_col]][intersect(pre_cmax,  which(data$.is_blq))] <- 0
      data[[conc_col]][intersect(post_cmax, which(data$.is_blq))] <- NA
    }
    
  } else {
    # Rule 1 (default): pre-first-quantifiable -> 0, post-last-quantifiable -> NA
    subjects <- unique(data[[subj_col]])
    for (s in subjects) {
      idx <- which(data[[subj_col]] == s)
      sub <- data[idx, ]
      quant_idx <- which(!sub$.is_blq & !is.na(sub[[conc_col]]))
      
      if (length(quant_idx) == 0) {
        data[[conc_col]][idx[data$.is_blq[idx]]] <- NA
        next
      }
      
      first_quant <- min(quant_idx)
      last_quant  <- max(quant_idx)
      
      # Before first quantifiable: set BLQ to 0
      if (first_quant > 1) {
        pre <- idx[1:(first_quant - 1)]
        data[[conc_col]][intersect(pre, which(data$.is_blq))] <- 0
      }
      # After last quantifiable: set BLQ to NA
      if (last_quant < length(idx)) {
        post <- idx[(last_quant + 1):length(idx)]
        data[[conc_col]][intersect(post, which(data$.is_blq))] <- NA
      }
      # Between: BLQ to 0 (common convention)
      between <- idx[first_quant:last_quant]
      data[[conc_col]][intersect(between, which(data$.is_blq))] <- 0
    }
  }
  
  data$.is_blq <- NULL
  data
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
  t <- time[valid]
  c <- conc[valid]
  
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
  cmax_idx <- which.max(c)
  
  # Only use points after Cmax
  terminal_mask <- seq_along(t) > cmax_idx
  if (!is.null(exclude_indices)) {
    # Map exclude_indices from original data to valid subset
    terminal_mask[exclude_indices] <- FALSE
  }
  
  t_term <- t[terminal_mask]
  c_term <- c[terminal_mask]
  
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
  
  log_c_term <- log(c_term)
  
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

#' Run NCA for all subjects using NonCompart::tblNCA
#'
#' Wrapper that handles column mapping, options, and returns clean output.
#'
#' @param data Processed PK data
#' @param col_map Column mapping
#' @param settings List of NCA settings
#' @return Data frame of NCA results
run_nca <- function(data, col_map, settings) {
  
  # Build iAUC if specified
  iAUC_df <- ""
  if (!is.null(settings$partial_aucs) && nrow(settings$partial_aucs) > 0) {
    iAUC_df <- data.frame(
      Name  = settings$partial_aucs$name,
      Start = settings$partial_aucs$start,
      End   = settings$partial_aucs$end,
      stringsAsFactors = FALSE
    )
  }
  
  # Determine administration mode
  adm <- switch(settings$admin_route,
                "extravascular" = "Extravascular",
                "iv_bolus"      = "Bolus",
                "iv_infusion"   = "Infusion",
                "Extravascular")
  
  # Determine trapezoidal method
  down_method <- switch(settings$trap_method,
                        "linear"  = "Linear",
                        "log"     = "Log",
                        "Linear")
  
  # CRITICAL: For crossover studies (treatment column mapped), each subject
  # has multiple profiles. NonCompart::tblNCA groups by `key` — if we use
  # Subject alone, it sees non-monotonic time. Solution: create a composite
  # key (Subject + Treatment) so each profile is analyzed separately.
  
  use_composite_key <- !is.null(col_map$treatment) &&
    col_map$treatment %in% names(data)
  
  if (use_composite_key) {
    data$.nca_key <- paste(data[[col_map$subject]],
                           data[[col_map$treatment]], sep = "||")
    nca_key <- ".nca_key"
  } else {
    nca_key <- col_map$subject
  }
  
  # Ensure data is sorted by key and time
  data <- data[order(data[[nca_key]], data[[col_map$time]]), ]
  
  # Run NCA via NonCompart
  result <- tryCatch({
    tblNCA(
      concData  = data,
      key       = nca_key,
      colTime   = col_map$time,
      colConc   = col_map$conc,
      dose      = settings$dose,
      adm       = adm,
      dur       = settings$infusion_duration,
      doseUnit  = settings$dose_unit,
      timeUnit  = settings$time_unit,
      concUnit  = settings$conc_unit,
      down      = down_method,
      R2ADJ     = settings$r2adj_threshold,
      MW        = settings$mw,
      SS        = settings$is_steady_state,
      iAUC      = iAUC_df
    )
  }, error = function(e) {
    NULL
  })
  
  # If composite key was used, split it back into Subject and Treatment columns
  if (!is.null(result) && use_composite_key) {
    key_parts <- strsplit(result[[1]], "\\|\\|")
    result$Subject   <- sapply(key_parts, `[`, 1)
    result$Treatment <- sapply(key_parts, `[`, 2)
    
    # Move Subject and Treatment to front, drop the composite key
    first_col <- names(result)[1]
    result[[first_col]] <- NULL
    result <- result[, c("Subject", "Treatment",
                          setdiff(names(result), c("Subject", "Treatment")))]
  }
  
  result
}
