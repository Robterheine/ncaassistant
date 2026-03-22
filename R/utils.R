# ============================================================================
# NCA Assistant — Utility Functions
# ============================================================================

#' Detect study design from data structure
#' @param data Data frame with mapped columns
#' @param col_map Named list of column mappings
#' @return List with design type, number of periods, sequences, etc.
detect_study_design <- function(data, col_map) {
  design <- list(
    type        = "unknown",
    n_subjects  = length(unique(data[[col_map$subject]])),
    n_periods   = 1,
    n_sequences = 1,
    n_treatments = 1,
    is_crossover = FALSE,
    is_steady_state = FALSE
  )
  
  if (!is.null(col_map$period) && col_map$period %in% names(data)) {
    design$n_periods <- length(unique(data[[col_map$period]]))
  }
  if (!is.null(col_map$sequence) && col_map$sequence %in% names(data)) {
    design$n_sequences <- length(unique(data[[col_map$sequence]]))
  }
  if (!is.null(col_map$treatment) && col_map$treatment %in% names(data)) {
    design$n_treatments <- length(unique(data[[col_map$treatment]]))
  }
  
  # Crossover detection: >1 period or >1 sequence

  if (design$n_periods > 1 || design$n_sequences > 1) {
    design$is_crossover <- TRUE
    design$type <- paste0(design$n_treatments, "x",
                          design$n_sequences, "x",
                          design$n_periods)
  } else if (design$n_treatments > 1) {
    design$type <- "parallel"
  } else {
    design$type <- "single_arm"
  }
  
  design
}

#' Format numeric values for display with appropriate precision
#' @param x Numeric vector
#' @param digits Number of significant digits
#' @return Character vector
fmt_pk <- function(x, digits = 4) {
  ifelse(is.na(x), "—",
         ifelse(abs(x) >= 1,
                formatC(x, digits = digits, format = "fg"),
                formatC(x, digits = digits, format = "g")))
}

#' Create a summary statistics table for PK parameters
#' @param data Data frame of NCA results (one row per subject)
#' @param params Character vector of parameter names to summarize
#' @return Data frame with summary statistics
summarize_pk_params <- function(data, params) {
  results <- lapply(params, function(p) {
    vals <- as.numeric(data[[p]])
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    if (n == 0) {
      return(data.frame(
        Parameter = p, N = 0, Mean = NA, SD = NA, CV_pct = NA,
        Median = NA, Min = NA, Max = NA,
        Geo_Mean = NA, Geo_CV_pct = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    geo_mean <- exp(mean(log(vals[vals > 0])))
    geo_sd   <- exp(sd(log(vals[vals > 0])))
    geo_cv   <- sqrt(exp(sd(log(vals[vals > 0]))^2) - 1) * 100
    
    data.frame(
      Parameter  = p,
      N          = n,
      Mean       = mean(vals),
      SD         = sd(vals),
      CV_pct     = ifelse(mean(vals) != 0, sd(vals) / mean(vals) * 100, NA),
      Median     = median(vals),
      Min        = min(vals),
      Max        = max(vals),
      Geo_Mean   = ifelse(all(vals > 0), geo_mean, NA),
      Geo_CV_pct = ifelse(all(vals > 0), geo_cv, NA),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}

#' Get available PowerTOST designs as a named list for selectInput
get_powertost_designs <- function() {
  designs <- known.designs()
  design_names <- paste0(designs$design, " (", designs$df2, " df)")
  setNames(designs$design, design_names)
}

#' Validate column mapping completeness
#' @return List with $valid (logical) and $message (character)
validate_mapping <- function(col_map, required = c("subject", "time", "conc")) {
  missing <- required[!required %in% names(col_map) |
                        sapply(col_map[required], function(x) is.null(x) || x == "")]
  if (length(missing) > 0) {
    list(valid = FALSE,
         message = paste("Missing required mappings:",
                         paste(missing, collapse = ", ")))
  } else {
    list(valid = TRUE, message = "All required columns mapped.")
  }
}

#' CDISC SDTM PK parameter names lookup
cdisc_pk_names <- function() {
  data.frame(
    NonCompart = c("CMAX", "TMAX", "AUCLST", "AUCIFO", "AUCIFP",
                   "AUMCLST", "AUMCIFO", "LAMZ", "LAMZHL",
                   "CLFO", "CLFP", "VZFO", "VZFP",
                   "MRTEVLST", "MRTEVIFO"),
    CDISC      = c("CMAX", "TMAX", "AUCLST", "AUCIFO", "AUCIFP",
                   "AUMCLST", "AUMCIFO", "LAMZ", "LAMZHL",
                   "CLFO", "CLFP", "VZFO", "VZFP",
                   "MRTEVLST", "MRTEVIFO"),
    Description = c("Max observed concentration",
                    "Time of Cmax",
                    "AUC to last measurable conc",
                    "AUC extrapolated to inf (obs)",
                    "AUC extrapolated to inf (pred)",
                    "AUMC to last measurable conc",
                    "AUMC extrapolated to inf (obs)",
                    "Terminal rate constant",
                    "Terminal half-life",
                    "Clearance (obs, extravascular)",
                    "Clearance (pred, extravascular)",
                    "Vz (obs, extravascular)",
                    "Vz (pred, extravascular)",
                    "MRT extravascular (last)",
                    "MRT extravascular (inf, obs)"),
    stringsAsFactors = FALSE
  )
}
