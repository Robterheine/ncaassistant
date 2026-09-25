# ============================================================================
# NCA Assistant — Utility Functions
# ============================================================================

# --- NonCompart engine compatibility -----------------------------------------
# NonCompart 0.8.0 introduced a strict input-type check in sNCA() that hard-stops
# ("Check input types!") on non-numeric time/concentration/dose. run_nca() and the
# single-subject NCA coerce defensively so older and newer versions both work; this
# helper detects the installed version and reports a clear compatibility status so
# version issues surface plainly instead of as a cryptic "NCA failed".

NONCOMPART_MIN_VERSION    <- "0.7.0"   # floor we expect the tblNCA/sNCA API at
NONCOMPART_TESTED_VERSION <- "0.8.0"   # version the app is validated against

#' Verify the NCA engine's lazy-load database is intact, WITHOUT executing any
#' NCA. Force-reading every object from the package deserialises the functions/
#' data (so a corrupt or stale .rdb — e.g. "lazy-load database ... is corrupt",
#' which happens when NonCompart is updated while an R session has it loaded —
#' throws here), while never calling sNCA/tblNCA. This matters because sNCA can
#' fall into NonCompart's interactive base-graphics picker ("Choose points for
#' terminal slope", via DetSlope/identify()), which would block the app in an
#' interactive R session. packageVersion() alone cannot detect a stale handle
#' because it only reads the DESCRIPTION, not the lazy-load database.
#' @return NULL if the engine loads, else the error message string.
noncompart_engine_error <- function() {
  tryCatch({
    ns <- asNamespace("NonCompart")
    for (nm in ls(ns, all.names = TRUE)) force(get(nm, envir = ns))
    NULL
  }, error = function(e) conditionMessage(e))
}

#' Detect the installed NonCompart version and classify compatibility.
#' Includes a functional self-test so a corrupt or stale engine is reported
#' clearly up front, not as a cryptic "NCA failed" mid-analysis.
#' @return list(version, level = ok|info|warn|error, label, message)
noncompart_compat <- function() {
  v <- tryCatch(utils::packageVersion("NonCompart"), error = function(e) NULL)
  if (is.null(v)) {
    return(list(version = "(not installed)", level = "error", label = "Not installed",
                message = paste0("The NonCompart package is not installed. ",
                                 "Install it with install.packages(\"NonCompart\").")))
  }
  vs <- as.character(v)

  # Functional self-test — the engine must actually load and run, not merely
  # report a version. A long-lived R session that updated NonCompart while it was
  # loaded ends up with a stale lazy-load handle and a "database is corrupt" error
  # even though the on-disk package is fine.
  eng_err <- noncompart_engine_error()
  if (!is.null(eng_err)) {
    corrupt <- grepl("lazy-load|corrupt|database", eng_err, ignore.case = TRUE)
    return(list(version = vs, level = "error", label = "Engine error",
                message = paste0(
                  "NonCompart ", vs, " is installed but failed to run (", eng_err, "). ",
                  if (corrupt)
                    paste0("This usually means your R session has a stale package handle ",
                           "(e.g. NonCompart was updated while this session was running). ",
                           "Fix: restart R — in RStudio, Session → Restart R (Cmd/Ctrl+Shift+F10) — ",
                           "then re-launch the app. If it persists, run ",
                           "install.packages(\"NonCompart\") and restart R again.")
                  else
                    paste0("Try restarting R and reinstalling with ",
                           "install.packages(\"NonCompart\")."))))
  }

  if (v < package_version(NONCOMPART_MIN_VERSION)) {
    return(list(version = vs, level = "warn", label = "Update recommended",
                message = paste0("NonCompart ", vs, " is older than the tested minimum ",
                                 NONCOMPART_MIN_VERSION, ". If you encounter NCA errors, update with ",
                                 "install.packages(\"NonCompart\").")))
  }
  if (v == package_version(NONCOMPART_TESTED_VERSION)) {
    return(list(version = vs, level = "ok", label = "Tested version",
                message = paste0("NonCompart ", vs, ": the version the validation suite was run with.")))
  }
  list(version = vs, level = "info", label = "Compatible",
       message = paste0("NonCompart ", vs, " detected (validated against ",
                        NONCOMPART_TESTED_VERSION, "). Supported via defensive type handling; ",
                        "if you see NCA errors, please report this version."))
}

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
  "LAMZHL"   = "Half-Life",
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
  "AUCIFO_DN"= "Dose-Normalised AUC Inf",
  "AUCIFP_DN"= "Dose-Normalised AUC Inf (pred)",
  "AUMCLST_DN" = "Dose-Normalised AUMC Last",
  "AUMCIFO_DN" = "Dose-Normalised AUMC Inf",
  "AUMCIFP_DN" = "Dose-Normalised AUMC Inf (pred)"
)

#' NonCompart's own dose-normalised values, dropped from tables and exports
#' when the app's per-profile ones (..._DN) are present, so each appears once
drop_duplicate_dose_normalised <- function(df) {
  if (!any(grepl("_DN$", names(df)))) return(df)
  df[, setdiff(names(df), c("CMAXD", "AUCIFOD", "AUCIFPD")), drop = FALSE]
}

#' Translate a NonCompart parameter name to plain English
#' @param name Character: the abbreviation
#' @return Character: the friendly name, or the original if unknown
friendly_name <- function(name) {
  label <- pk_param_labels[name]
  label <- ifelse(is.na(label), partial_auc_label(name), label)
  ifelse(is.na(label), name, label)
}

#' Label of a partial AUC, Cmax-in-interval or Tmax-in-interval column
#'
#' "AUC_0_0.5" -> "Partial AUC 0\u20130.5"; "CMAX_168_t" -> "Cmax 168\u2013t";
#' NA for any other name. Times are in the time unit of the data.
partial_auc_label <- function(name) {
  dn <- grepl("_DN$", name)
  base <- sub("_DN$", "", name)
  m <- regmatches(base, regexec(PARTIAL_AUC_PATTERN, base))
  vapply(seq_along(m), function(i) {
    x <- m[[i]]
    if (length(x) == 0) return(NA_character_)
    lab <- paste0(switch(x[2], AUC = "Partial AUC ", CMAX = "Cmax ", TMAX = "Tmax "), x[3], "\u2013", x[4])
    # "Dose-Normalised ..." first, so no concentration unit is appended later
    if (dn[i]) paste0("Dose-Normalised ", lab) else lab
  }, character(1))
}

#' Rename columns of an NCA result data frame to friendly names
#' @param df Data frame with NonCompart column names
#' @return Data frame with renamed columns
#' @param units Optional list(dose, time, conc): appends each column's unit,
#'   as the exports need (the column labels themselves carry none)
rename_nca_columns <- function(df, units = NULL) {
  if (is.null(df) || nrow(df) == 0) return(df)
  nm <- names(df)
  for (i in seq_along(nm)) {
    label <- pk_param_labels[nm[i]]
    if (is.na(label)) label <- partial_auc_label(nm[i])
    if (!is.na(label)) nm[i] <- label
  }
  if (!is.null(units)) nm <- add_units_to_labels(nm, dose_unit = units$dose, time_unit = units$time,
                                                 conc_unit = units$conc)
  names(df) <- nm
  df
}

#' Rename summary statistics columns to friendly names
#' @param df Summary data frame from summarize_pk_params
#' @return Data frame with renamed columns
rename_summary_columns <- function(df) {
  renames <- c(
    "Treatment" = "Treatment",
    "Parameter" = "Parameter",
    "N"         = "N",
    "Geo_Mean"  = "Geometric Mean",
    "Geo_CV_pct"= "Geometric CV (%)",
    "Median"    = "Median",
    "Q25"       = "Q1 (25%)",
    "Q75"       = "Q3 (75%)",
    "Mean"      = "Arithmetic Mean",
    "SD"        = "Std Dev",
    "CV_pct"    = "CV (%)",
    "Min"       = "Min",
    "Max"       = "Max"
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
rename_be_columns <- function(df, ci_level = 90) {
  renames <- c(
    "Parameter"     = "PK Parameter",
    "Test"          = "Test Formulation",
    "Reference"     = "Reference Formulation",
    "N_Test"        = "Subjects (Test)",
    "N_Ref"         = "Subjects (Reference)",
    "Obs_Test"      = "Profiles (Test)",
    "Obs_Ref"       = "Profiles (Reference)",
    "Scale"         = "Scale",
    "Point_Est"     = "Estimate",
    "CI_Lower"      = paste0(ci_level, "% CI Lower"),
    "CI_Upper"      = paste0(ci_level, "% CI Upper"),
    "BE_Lower"      = "Accept. Lower",
    "BE_Upper"      = "Accept. Upper",
    "PE_Constraint" = "PE within 80\u2013125%",
    "Missing_Test"  = "Profiles missing (Test)",
    "Missing_Ref"   = "Profiles missing (Reference)",
    "Zeros_Test"    = "Zero values (Test)",
    "Zeros_Ref"     = "Zero values (Reference)",
    "BLQ_Test"      = "Mostly BLQ (Test)",
    "BLQ_Ref"       = "Mostly BLQ (Reference)",
    "Bioequivalent" = "Bioequivalent?",
    "MSE"           = "Residual Variance",
    "DF"            = "Degrees of Freedom",
    "Model"         = "Model"
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
summarize_pk_params <- function(data, params, group_col = NULL) {
  # If a grouping column is provided (e.g., Treatment), compute
  # statistics separately for each group level.
  if (!is.null(group_col) && group_col %in% names(data)) {
    groups <- sort(unique(as.character(data[[group_col]])))
    group_results <- lapply(groups, function(g) {
      sub <- data[as.character(data[[group_col]]) == g, , drop = FALSE]
      tbl <- summarize_pk_params(sub, params, group_col = NULL)
      tbl <- cbind(data.frame(Group = g, stringsAsFactors = FALSE), tbl)
      tbl
    })
    out <- do.call(rbind, group_results)
    names(out)[1] <- group_col
    return(out)
  }

  results <- lapply(params, function(p) {
    vals <- as.numeric(data[[p]])
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    if (n == 0) {
      return(data.frame(
        Parameter = p, N = 0,
        Geo_Mean = NA, Geo_CV_pct = NA,
        Median = NA, Q25 = NA, Q75 = NA,
        Mean = NA, SD = NA, CV_pct = NA,
        Min = NA, Max = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    pos <- vals[vals > 0]
    geo_mean <- if (length(pos) >= 2) exp(mean(log(pos))) else NA
    geo_cv   <- if (length(pos) >= 2) sqrt(exp(sd(log(pos))^2) - 1) * 100 else NA
    
    data.frame(
      Parameter  = p,
      N          = n,
      Geo_Mean   = ifelse(all(vals > 0), geo_mean, NA),
      Geo_CV_pct = ifelse(all(vals > 0), geo_cv, NA),
      Median     = median(vals),
      Q25        = unname(quantile(vals, 0.25)),
      Q75        = unname(quantile(vals, 0.75)),
      Mean       = mean(vals),
      SD         = sd(vals),
      CV_pct     = ifelse(mean(vals) != 0, sd(vals) / mean(vals) * 100, NA),
      Min        = min(vals),
      Max        = max(vals),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}

#' Append unit strings to friendly parameter labels
#'
#' @param labels Character vector of friendly parameter names
#' @param dose_unit Dose unit (e.g., "mg")
#' @param time_unit Time unit (e.g., "h")
#' @param conc_unit Concentration unit (e.g., "ng/mL")
#' @return Character vector with units appended where applicable
add_units_to_labels <- function(labels, dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL") {
  auc_unit  <- paste0(conc_unit, "\u00b7", time_unit)
  cl_unit   <- paste0("L/", time_unit)
  unit_map <- c(
    "Peak Concentration (Cmax)"          = conc_unit,
    "Time of Peak (Tmax)"                = time_unit,
    "AUC to Last Point"                  = auc_unit,
    "AUC to Infinity (observed)"         = auc_unit,
    "AUC Within Dosing Interval"         = auc_unit,
    "Average Concentration (Cavg)"       = conc_unit,
    "Trough Concentration (Cmin)"        = conc_unit,
    "Dosing Interval (tau)"              = time_unit,
    "Half-Life"                          = time_unit,
    "Elimination Rate Constant"          = paste0("1/", time_unit),
    "Apparent Clearance (CL/F)"          = cl_unit,
    "Apparent Volume (Vz/F)"             = "L",
    "Clearance (CL)"                     = cl_unit,
    "Volume of Distribution (Vz)"        = "L",
    "Lambda_z Lower Time"                = time_unit,
    "Lambda_z Upper Time"                = time_unit
  )
  for (i in seq_along(labels)) {
    u <- unit_map[labels[i]]
    if (is.na(u)) u <- if (startsWith(labels[i], "Partial AUC ")) auc_unit else
                       if (grepl("^Cmax [0-9.]+\u2013", labels[i])) conc_unit else
                       if (grepl("^Tmax [0-9.]+\u2013", labels[i])) time_unit else NA
    if (is.na(u)) next
    labels[i] <- paste0(labels[i], " (", u, ")")
  }
  labels
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
    shown <- c(subject = "Subject ID", time = "Time", conc = "Concentration")
    list(valid = FALSE,
         message = paste0("Choose a column for: ",
                          paste(ifelse(missing %in% names(shown), shown[missing], missing), collapse = ", "),
                          " (under Column Mapping)."))
  } else {
    list(valid = TRUE, message = "All required columns mapped.")
  }
}


# --- UNITS -----------------------------------------------------------------

# Units are not cosmetic. NonCompart::Unit() derives a numeric conversion
# factor from the dose and concentration units and applies it to clearance and
# volume, so CL/F and Vz/F change by orders of magnitude with the unit strings.
# Unrecognised spellings make Unit() fail outright, and because the NCA call is
# wrapped in tryCatch the user only ever saw the generic "NCA failed" message.
# These are the spellings NonCompart accepts; everything else is rejected up
# front with an explanation.
CONC_UNIT_CHOICES <- c("ng/mL", "ug/mL", "mg/mL", "pg/mL",
                       "ng/L",  "ug/L",  "mg/L",  "g/L",
                       "nmol/L", "umol/L", "mmol/L", "mol/L",
                       "nmol/mL", "umol/mL")
DOSE_UNIT_CHOICES <- c("mg", "ug", "g", "ng", "pg",
                       "mmol", "umol", "nmol", "mol")
TIME_UNIT_CHOICES <- c("h", "min", "day", "week", "s")

#' Check that a unit combination is one NonCompart can actually use
#'
#' Returns list(valid, message). `message` is user-facing when valid is FALSE.
validate_units <- function(dose_unit, time_unit, conc_unit, mw = 0) {
  fail <- function(msg) list(valid = FALSE, message = msg)
  blank <- function(x) is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))

  if (blank(dose_unit) || blank(time_unit) || blank(conc_unit)) {
    return(fail("Dose, time and concentration units must all be set."))
  }

  # A dose unit containing "/" (typically mg/kg) makes Unit() return NA rather
  # than error, which would silently blank the CL/F and Vz/F units.
  if (grepl("/", dose_unit, fixed = TRUE)) {
    return(fail(paste0("The dose unit must be a plain amount such as mg, not '", dose_unit,
                       "'. Body-weight-normalised dosing is not supported directly: enter ",
                       "the actual amount each subject received, or map a Dose column.")))
  }

  mw_num <- suppressWarnings(as.numeric(if (is.null(mw)) 0 else mw))
  if (length(mw_num) != 1 || is.na(mw_num) || mw_num < 0) mw_num <- 0

  molar_warning <- FALSE
  res <- withCallingHandlers(
    tryCatch(
      NonCompart::Unit(code = "CLFO", timeUnit = time_unit, concUnit = conc_unit,
                       doseUnit = dose_unit, MW = mw_num),
      error = function(e) NULL),
    warning = function(w) {
      if (grepl("[Mm]olecular weight", conditionMessage(w))) molar_warning <<- TRUE
      invokeRestart("muffleWarning")
    })

  if (is.null(res)) {
    return(fail(paste0("'", conc_unit, "' / '", dose_unit, "' is not a unit combination the ",
                       "NCA engine recognises. Use the listed units; note that the micro ",
                       "prefix must be typed as 'u' (ug/mL), not 'µ' or 'mcg'.")))
  }
  if (molar_warning) {
    return(fail(paste0("Molar and mass units are mixed ('", conc_unit, "' with '", dose_unit,
                       "'). Enter the molecular weight so clearance and volume can be ",
                       "converted, or use matching unit types.")))
  }
  if (is.na(res[["Unit"]]) || is.na(res[["Factor"]])) {
    return(fail(paste0("'", conc_unit, "' / '", dose_unit, "' does not give usable clearance ",
                       "and volume units. Check both against the listed units.")))
  }
  list(valid = TRUE, message = "")
}


#' Tell the user how the Analysis Record's reproduction check went
#' @param rec_out Return value of a create_*_record() function
notify_reproduction <- function(rec_out) {
  v <- attr(rec_out, "reproduction")
  if (is.null(v)) return(invisible(NULL))
  if (v %in% c("MATCH", "FIGURE CREATED")) {
    showNotification(paste0("Analysis Record ready. Reproduction check: ", v, "."),
                     type = "message", duration = 6)
  } else {
    showNotification(paste0("Analysis Record created, but the reproduction check says ", v,
                            ". See reproduction_check.txt in the record before relying on it."),
                     type = "error", duration = NULL)
  }
  invisible(v)
}


#' Collapsible list of the official CDISC codes for the parameters shown
#'
#' States the Controlled Terminology release used (see R/cdisc_terms.R).
cdisc_codes_ui <- function(params, admin_route, is_ss) {
  codes <- tryCatch(cdisc_pk_codes(params, admin_route, is_ss), error = function(e) NULL)
  if (is.null(codes) || nrow(codes) == 0) return(NULL)
  rel <- cdisc_ct_release()$Release
  tags$details(
    class = "mt-3 small",
    tags$summary(class = "fw-semibold",
                 icon("tags", class = "me-1"),
                 paste0("CDISC parameter codes (SDTM Controlled Terminology ", rel, ")")),
    tags$p(class = "text-muted mt-2 mb-1", cdisc_ct_statement(),
           " Code lookup only: these results are not an SDTM PP dataset, and no claim of ",
           "conformance to CDISC standards is made. Units are the app's labels, not CDISC PKUNIT terms."),
    tags$div(style = "max-height: 260px; overflow-y: auto;",
      tags$table(class = "table table-sm table-striped mb-0",
        tags$thead(tags$tr(tags$th("App parameter"), tags$th("PPTESTCD"), tags$th("PPTEST"),
                           tags$th("NCIt"), tags$th("Note"))),
        tags$tbody(lapply(seq_len(nrow(codes)), function(i) {
          x <- codes[i, ]
          tags$tr(tags$td(x$Parameter),
                  tags$td(if (nzchar(x$PPTESTCD)) tags$code(x$PPTESTCD) else tags$span(class = "text-muted", "\u2014")),
                  tags$td(x$PPTEST), tags$td(x$NCIt_code), tags$td(class = "text-muted", x$Note))
        }))))
  )
}

#' Clear a result when a setting that produced it changes
#'
#' A result shown next to settings that did not produce it (other units,
#' another method, another CV) is easy to misread, and a record built then
#' would pair the result with settings it was not computed with. So any change
#' clears the result and asks for a new run.
#' @param settings reactive returning the analysis settings (any value)
#' @param clear function that clears the result(s)
#' @param has_result function returning TRUE when there is a result to clear
#' @param id notification id
clear_result_on_change <- function(settings, has_result, clear, id) {
  observeEvent(settings(), {
    if (!isTRUE(has_result())) return()
    clear()
    showNotification("A setting changed after the analysis, so its results were cleared. Run it again.",
                     type = "message", duration = 6, id = id)
  }, ignoreInit = TRUE)
}

#' Units stated in the data, mapped onto the app's unit choices
#'
#' Read from CDISC unit variables (AVALU/PCSTRESU, RRLTU, DOSEU/EXDOSU) or
#' from flat-file unit columns named after the quantity (ConcUnit,
#' Time_Unit, DOSE.UNITS). A column with more than one unit is refused
#' elsewhere (interlock_mixed_units), so only single values are used.
#' @return list(conc, time, dose), each list(unit = app choice or NA,
#'   found = text in the data, column) or NULL when not stated
units_in_data <- function(data) {
  nm <- names(data)
  is_unit_col <- function(n) grepl("(^|[_. ])units?($|[_. ])", n, ignore.case = TRUE) | grepl("[a-z]Units?$", n)
  pick <- function(cdisc, word) {
    cols <- c(nm[toupper(nm) %in% cdisc], nm[is_unit_col(nm) & grepl(word, nm, ignore.case = TRUE)])
    for (cc in cols) {
      v <- unique(trimws(as.character(data[[cc]]))); v <- v[!is.na(v) & v != ""]
      if (length(v) == 1) return(list(found = v, column = cc))
    }
    NULL
  }
  # The micro sign (U+00B5 or Greek mu, U+03BC) is replaced by its UTF-8 bytes:
  # a pattern with the character itself fails or is mangled in a C locale
  norm <- function(u) {
    u <- as.character(u)
    for (m in c("\xc2\xb5", "\xce\xbc")) u <- gsub(m, "u", u, fixed = TRUE, useBytes = TRUE)
    # useBytes can mark the result as bytes, which match() treats as
    # different from the same plain text
    u <- vapply(u, function(z) rawToChar(charToRaw(z)), character(1), USE.NAMES = FALSE)
    gsub("mcg", "ug", tolower(gsub(" ", "", u, fixed = TRUE)), fixed = TRUE)
  }
  map <- function(hit, choices, aliases = character(0)) {
    if (is.null(hit)) return(NULL)
    u <- norm(hit$found)
    if (u %in% names(aliases)) u <- aliases[[u]]
    hit$unit <- choices[match(u, norm(choices))]
    hit
  }
  list(conc = map(pick(c("AVALU", "PCSTRESU", "PCORRESU", "CONCU"), "conc|aval|dv"), CONC_UNIT_CHOICES,
                  c("ng/ml" = "ng/ml", "ug/l" = "ug/l", "nm" = "nmol/l", "um" = "umol/l")),
       time = map(pick(c("RRLTU", "AFRLTU", "TIMEU"), "time|tad|tafd"), TIME_UNIT_CHOICES,
                  c("hr" = "h", "hrs" = "h", "hour" = "h", "hours" = "h", "minute" = "min", "minutes" = "min",
                    "mins" = "min", "d" = "day", "days" = "day", "weeks" = "week", "sec" = "s", "seconds" = "s")),
       dose = map(pick(c("DOSEU", "EXDOSU"), "dose|amt"), DOSE_UNIT_CHOICES))
}

#' Refuse a selected unit that contradicts a unit stated in the data
#' @param found units_in_data() result
#' @return NULL when consistent, otherwise a user-facing message
check_units_against_data <- function(found, dose_unit, time_unit, conc_unit) {
  sel <- list(conc = conc_unit, time = time_unit, dose = dose_unit)
  what <- c(conc = "concentrations", time = "times", dose = "doses")
  for (k in names(sel)) {
    f <- found[[k]]
    if (is.null(f) || is.na(f$unit) || identical(f$unit, sel[[k]])) next
    return(paste0("The data give ", what[[k]], " in ", f$found, " (column ", f$column, "), but the ",
                  k, " unit selected is ", sel[[k]], ". Select ", f$unit, ", or correct the file."))
  }
  NULL
}

#' Where uploaded data go (upload page and About page)
DATA_PROTECTION_NOTICE <- paste0(
  "On the public instance, uploads are processed on shinyapps.io servers run by Posit PBC (USA). ",
  "Upload only synthetic, example or anonymised data there. Pseudonymised trial data are still personal ",
  "data under the GDPR. Sending them to a third-party host needs agreements your organisation must have ",
  "in place, and may breach sponsor confidentiality. For real study data, run the app on your own computer.")

#' A file-reading error in words a user can act on
friendly_read_error <- function(msg) {
  hint <- if (grepl("more columns than column names|did not have|no lines available|incomplete final line|duplicate 'row.names'", msg))
    paste0(" Check the delimiter (comma, semicolon or tab) and the decimal mark under the file choice, and ",
           "that the first row holds the column names.") else ""
  paste0("The file could not be read (", msg, ").", hint)
}
