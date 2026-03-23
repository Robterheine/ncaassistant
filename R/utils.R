# ============================================================================
# NCA Assistant — Utility Functions
# ============================================================================

# --- Parameter name dictionary ------------------------------------------------
# Maps NonCompart abbreviations to plain-language descriptions.

pk_param_labels <- c(
  # Subject/design
  "Subject"   = "Subject",
  "Treatment" = "Treatment",
  
  # Primary PK parameters
  "CMAX"     = "Peak Concentration (Cmax)",
  "CMAXD"    = "Dose-Normalised Cmax",
  "TMAX"     = "Time of Peak (Tmax)",
  "TLAG"     = "Lag Time",
  
  # Terminal phase
  "LAMZHL"   = "Half-Life (h)",
  "LAMZ"     = "Elimination Rate Constant",
  "LAMZLL"   = "Lambda_z Lower Time",
  "LAMZUL"   = "Lambda_z Upper Time",
  "LAMZNPT"  = "Points Used for Half-Life",
  
  # Goodness of fit
  "CORRXY"   = "Correlation (r)",
  "R2"       = "R-squared",
  "R2ADJ"    = "Adjusted R-squared",
  "b0"       = "Y-Intercept (ln scale)",
  
  # AUC — observed
  "AUCLST"   = "AUC to Last Point",
  "AUCALL"   = "AUC All (incl. trailing zero)",
  
  # AUC — extrapolated (observed Clast)
  "AUCIFO"   = "AUC to Infinity (observed)",
  "AUCIFOD"  = "Dose-Normalised AUC to Infinity",
  "AUCPEO"   = "AUC % Extrapolated (observed)",
  
  # AUC — extrapolated (predicted Clast)
  "AUCIFP"   = "AUC to Infinity (predicted)",
  "AUCIFPD"  = "Dose-Normalised AUC Inf (pred)",
  "AUCPEP"   = "AUC % Extrapolated (predicted)",
  
  # AUMC
  "AUMCLST"  = "AUMC to Last Point",
  "AUMCIFO"  = "AUMC to Infinity (observed)",
  "AUMCIFP"  = "AUMC to Infinity (predicted)",
  "AUMCPEO"  = "AUMC % Extrapolated (obs)",
  "AUMCPEP"  = "AUMC % Extrapolated (pred)",
  
  # Clearance and volume
  "CLFO"     = "Apparent Clearance (CL/F)",
  "CLFP"     = "Apparent Clearance (pred)",
  "VZFO"     = "Apparent Volume (Vz/F)",
  "VZFP"     = "Apparent Volume (pred)",
  "CLO"      = "Clearance (CL)",
  "VZO"      = "Volume of Distribution (Vz)",
  
  # MRT
  "MRTEVLST" = "Mean Residence Time (to last)",
  "MRTEVIFO" = "Mean Residence Time (to inf, obs)",
  "MRTEVIFP" = "Mean Residence Time (to inf, pred)",
  
  # Last observed
  "CLST"     = "Last Measurable Concentration",
  "CLSTP"    = "Predicted Last Concentration",
  "TLST"     = "Time of Last Measurable Conc",
  
  # Steady-state derived
  "AUCTAU"   = "AUC Within Dosing Interval",
  "TAU"      = "Dosing Interval (tau)",
  "CAVG"     = "Average Concentration (Cavg)",
  "CMIN_SS"  = "Trough Concentration (Cmin)",
  "FLUCTP"   = "Peak-Trough Fluctuation (%)",
  "SWING"    = "Swing ((Cmax-Cmin)/Cmin)",
  
  # Dose-normalised (from add_dose_normalized)
  "CMAX_DN"  = "Dose-Normalised Cmax",
  "AUCLST_DN"= "Dose-Normalised AUC Last",
  "AUCIFO_DN"= "Dose-Normalised AUC Inf"
)

#' Translate a NonCompart parameter name to plain English
#' @param name Character: the abbreviation
#' @return Character: the friendly name, or the original if unknown
friendly_name <- function(name) {
  label <- pk_param_labels[name]
  ifelse(is.na(label), name, label)
}

#' Rename columns of an NCA result data frame to friendly names
#' @param df Data frame with NonCompart column names
#' @return Data frame with renamed columns
rename_nca_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  nm <- names(df)
  for (i in seq_along(nm)) {
    label <- pk_param_labels[nm[i]]
    if (!is.na(label)) nm[i] <- label
  }
  names(df) <- nm
  df
}

#' Rename summary statistics columns to friendly names
#' @param df Summary data frame from summarize_pk_params
#' @return Data frame with renamed columns
rename_summary_columns <- function(df) {
  renames <- c(
    "Parameter" = "Parameter",
    "N"         = "N",
    "Mean"      = "Mean",
    "SD"        = "Std Dev",
    "CV_pct"    = "CV (%)",
    "Median"    = "Median",
    "Min"       = "Min",
    "Max"       = "Max",
    "Geo_Mean"  = "Geometric Mean",
    "Geo_CV_pct"= "Geometric CV (%)"
  )
  nm <- names(df)
  for (i in seq_along(nm)) {
    r <- renames[nm[i]]
    if (!is.na(r)) nm[i] <- r
  }
  names(df) <- nm
  # Also rename parameter values in the Parameter column
  if ("Parameter" %in% names(df)) {
    df$Parameter <- sapply(df$Parameter, friendly_name)
  }
  df
}

#' Rename BE CI table columns to friendly names
#' @param df CI result data frame
#' @return Data frame with renamed columns
rename_be_columns <- function(df) {
  renames <- c(
    "Parameter"     = "PK Parameter",
    "Test"          = "Test Formulation",
    "Reference"     = "Reference Formulation",
    "N_Test"        = "N (Test)",
    "N_Ref"         = "N (Reference)",
    "Point_Est"     = "Ratio (%)",
    "CI_Lower"      = "90% CI Lower",
    "CI_Upper"      = "90% CI Upper",
    "BE_Lower"      = "Accept. Lower",
    "BE_Upper"      = "Accept. Upper",
    "Bioequivalent" = "Bioequivalent?",
    "MSE"           = "Residual Variance",
    "DF"            = "Degrees of Freedom"
  )
  nm <- names(df)
  for (i in seq_along(nm)) {
    r <- renames[nm[i]]
    if (!is.na(r)) nm[i] <- r
  }
  names(df) <- nm
  # Also rename parameter values
  if ("PK Parameter" %in% names(df)) {
    df[["PK Parameter"]] <- sapply(df[["PK Parameter"]], friendly_name)
  }
  df
}

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
