# ============================================================================
# NCA Assistant — Data Quality Check Engine
# ============================================================================
# Runs a comprehensive check on imported PK data and returns structured
# findings with severity levels and actionable guidance.
#
# Severity levels:
#   ERROR   — Must fix before proceeding (NCA will fail or produce wrong results)
#   WARNING — Should investigate (may affect results)
#   INFO    — Informational (good to know)
#   OK      — Check passed
# ============================================================================

#' Run comprehensive data quality checks on PK data
#'
#' @param data Data frame (raw, before BLQ processing)
#' @param col_map Named list with subject, time, conc, and optional columns
#' @param lloq Numeric LLOQ value (0 = not set)
#' @return List with $findings (data frame), $n_errors, $n_warnings, $n_info,
#'         $pass (logical: TRUE if no errors)
run_data_quality_check <- function(data, col_map, lloq = 0) {
  
  findings <- list()
  add <- function(severity, category, message, detail = "", action = "") {
    findings[[length(findings) + 1]] <<- data.frame(
      Severity = severity,
      Category = category,
      Message  = message,
      Detail   = detail,
      Action   = action,
      stringsAsFactors = FALSE
    )
  }
  
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # ===========================================================================
  # 1. BASIC STRUCTURE CHECKS
  # ===========================================================================
  
  if (n_rows == 0) {
    add("ERROR", "Structure", "Dataset has 0 rows",
        "The uploaded file contains no data rows.",
        "Check file format, delimiter, and header settings.")
    return(compile_findings(findings))
  }
  
  if (n_rows < 3) {
    add("WARNING", "Structure",
        paste("Dataset has only", n_rows, "rows"),
        "NCA requires at least 3 time points per subject for lambda_z estimation.",
        "Verify this is the correct file.")
  } else {
    add("OK", "Structure", paste(n_rows, "rows ×", n_cols, "columns loaded"))
  }
  
  # ===========================================================================
  # 2. COLUMN EXISTENCE & TYPE CHECKS
  # ===========================================================================
  
  # Check mapped columns actually exist
  for (field in c("subject", "time", "conc")) {
    col_name <- col_map[[field]]
    if (is.null(col_name) || col_name == "" || !col_name %in% names(data)) {
      add("ERROR", "Columns",
          paste0("Required column '", field, "' not found in data"),
          paste0("Mapped to '", col_name, "' but this column does not exist."),
          "Check column mapping in the sidebar.")
    }
  }
  
  # If required columns missing, stop here
  subj_col <- col_map$subject
  time_col <- col_map$time
  conc_col <- col_map$conc
  
  if (!all(c(subj_col, time_col, conc_col) %in% names(data))) {
    return(compile_findings(findings))
  }
  
  # ===========================================================================
  # 3. TIME COLUMN CHECKS
  # ===========================================================================
  
  time_raw <- data[[time_col]]
  time_num <- suppressWarnings(as.numeric(time_raw))
  n_time_na <- sum(is.na(time_num) & !is.na(time_raw))
  
  if (n_time_na > 0) {
    bad_vals <- unique(time_raw[is.na(time_num) & !is.na(time_raw)])
    add("ERROR", "Time",
        paste(n_time_na, "non-numeric values in Time column"),
        paste0("Examples: ", paste(head(bad_vals, 5), collapse = ", ")),
        "Remove or fix non-numeric time entries before analysis.")
  }
  
  n_time_missing <- sum(is.na(time_raw))
  if (n_time_missing > 0) {
    add("WARNING", "Time",
        paste(n_time_missing, "missing (NA/blank) time values"),
        paste0(round(n_time_missing/n_rows*100, 1), "% of rows"),
        "These rows will be excluded from analysis.")
  }
  
  # Negative times
  time_valid <- time_num[!is.na(time_num)]
  n_neg_time <- sum(time_valid < 0)
  if (n_neg_time > 0) {
    add("WARNING", "Time",
        paste(n_neg_time, "negative time values detected"),
        paste("Range:", min(time_valid), "to", max(time_valid)),
        "Pre-dose samples? Verify these are intentional. NCA uses time ≥ 0.")
  }
  
  if (length(time_valid) > 0 && all(!is.na(time_valid))) {
    add("OK", "Time",
        paste0("Time range: ", min(time_valid), " to ", max(time_valid),
               " (", length(unique(time_valid)), " unique time points)"))
  }
  
  # ===========================================================================
  # 4. CONCENTRATION COLUMN CHECKS
  # ===========================================================================
  
  conc_raw <- data[[conc_col]]
  
  # Check for BLQ strings
  if (is.character(conc_raw) || is.factor(conc_raw)) {
    conc_char <- as.character(conc_raw)
    blq_pattern <- grepl("^(BLQ|BQL|<|BLOQ|NS|ND|NQ|N/?A|MISSING)", conc_char,
                         ignore.case = TRUE)
    n_blq_str <- sum(blq_pattern)
    n_other_str <- sum(!blq_pattern & is.na(suppressWarnings(as.numeric(conc_char))) &
                         !is.na(conc_char) & conc_char != "")
    
    if (n_blq_str > 0) {
      blq_examples <- unique(conc_char[blq_pattern])
      add("WARNING", "Concentration",
          paste(n_blq_str, "BLQ/text entries detected"),
          paste0("Values found: ", paste(head(blq_examples, 5), collapse = ", "),
                 " (", round(n_blq_str/n_rows*100, 1), "% of data)"),
          "Set LLOQ > 0 and select a BLQ handling rule to process these.")
      
      # Try to auto-detect LLOQ from "<X" patterns
      lt_vals <- conc_char[grepl("^<", conc_char)]
      if (length(lt_vals) > 0) {
        lt_nums <- suppressWarnings(as.numeric(gsub("^<\\s*", "", lt_vals)))
        lt_nums <- lt_nums[!is.na(lt_nums)]
        if (length(lt_nums) > 0) {
          add("INFO", "Concentration",
              paste0("Auto-detected LLOQ candidate: ", min(lt_nums)),
              paste("From '<' entries:", paste(head(unique(lt_vals), 3), collapse=", ")),
              "Verify and set this value in the LLOQ field.")
        }
      }
    }
    
    if (n_other_str > 0) {
      bad_conc <- unique(conc_char[!blq_pattern &
                                     is.na(suppressWarnings(as.numeric(conc_char))) &
                                     !is.na(conc_char) & conc_char != ""])
      add("ERROR", "Concentration",
          paste(n_other_str, "unrecognized text values in Concentration column"),
          paste0("Examples: ", paste(head(bad_conc, 5), collapse = ", ")),
          "These are not recognized as BLQ. Fix or remove before analysis.")
    }
  }
  
  conc_num <- suppressWarnings(as.numeric(as.character(conc_raw)))
  n_conc_na <- sum(is.na(conc_num))
  
  # Negative concentrations
  n_neg_conc <- sum(conc_num < 0, na.rm = TRUE)
  if (n_neg_conc > 0) {
    add("WARNING", "Concentration",
        paste(n_neg_conc, "negative concentration values"),
        paste("Min value:", min(conc_num, na.rm = TRUE)),
        "Negative values are unusual. Check assay results. These will be excluded from log-scale calculations.")
  }
  
  # All concentrations missing
  conc_valid <- conc_num[!is.na(conc_num)]
  if (length(conc_valid) == 0) {
    add("ERROR", "Concentration",
        "No valid numeric concentration values",
        "All values are NA, text, or blank.",
        "Check column mapping — is this the correct concentration column?")
  } else {
    # Concentration range plausibility
    conc_range <- range(conc_valid)
    add("OK", "Concentration",
        paste0("Concentration range: ", signif(conc_range[1], 4), " to ",
               signif(conc_range[2], 4),
               " (", sum(!is.na(conc_num)), " valid values, ",
               n_conc_na, " missing)"))
    
    # Suspicious unit check: all values extremely small or extremely large
    if (max(conc_valid) < 0.001) {
      add("INFO", "Concentration",
          "All concentrations < 0.001",
          paste("Max:", signif(max(conc_valid), 3)),
          "Verify concentration units. Did you mean µg/L instead of mg/L?")
    }
    if (min(conc_valid[conc_valid > 0]) > 10000) {
      add("INFO", "Concentration",
          "All non-zero concentrations > 10,000",
          paste("Min non-zero:", signif(min(conc_valid[conc_valid > 0]), 3)),
          "Verify concentration units. Did you mean ng/mL instead of pg/mL?")
    }
  }
  
  # LLOQ check
  if (lloq > 0 && length(conc_valid) > 0) {
    n_below <- sum(conc_valid < lloq & conc_valid > 0)
    pct_below <- round(n_below / length(conc_valid) * 100, 1)
    if (pct_below > 30) {
      add("WARNING", "Concentration",
          paste0(pct_below, "% of non-zero values are below LLOQ (", lloq, ")"),
          paste(n_below, "values between 0 and LLOQ"),
          "High BLQ rate may indicate insufficient sensitivity or late sampling.")
    }
  }
  
  # ===========================================================================
  # 5. SUBJECT-LEVEL CHECKS
  # ===========================================================================
  
  subjects <- unique(data[[subj_col]])
  n_subj <- length(subjects)
  
  if (n_subj == 0) {
    add("ERROR", "Subjects", "No subjects found",
        "The Subject column contains no valid identifiers.",
        "Check column mapping.")
  } else {
    add("OK", "Subjects", paste(n_subj, "unique subjects detected"))
  }
  
  # Missing subject IDs
  n_missing_subj <- sum(is.na(data[[subj_col]]) | data[[subj_col]] == "")
  if (n_missing_subj > 0) {
    add("ERROR", "Subjects",
        paste(n_missing_subj, "rows with missing Subject ID"),
        paste0(round(n_missing_subj/n_rows*100, 1), "% of rows"),
        "Remove or fill in missing subject identifiers.")
  }
  
  # Observations per subject
  obs_per_subj <- table(data[[subj_col]])
  min_obs <- min(obs_per_subj)
  max_obs <- max(obs_per_subj)
  
  sparse_subjects <- names(obs_per_subj[obs_per_subj < 3])
  if (length(sparse_subjects) > 0) {
    add("WARNING", "Subjects",
        paste(length(sparse_subjects), "subjects with < 3 observations"),
        paste0("Subjects: ", paste(head(sparse_subjects, 5), collapse = ", "),
               if (length(sparse_subjects) > 5) paste0(" (+ ", length(sparse_subjects)-5, " more)")),
        "Lambda_z cannot be estimated with fewer than 3 points. These subjects will have incomplete NCA results.")
  }
  
  if (min_obs != max_obs) {
    add("INFO", "Subjects",
        paste0("Unequal observations per subject: ", min_obs, " to ", max_obs),
        paste("Median:", median(obs_per_subj)),
        "Normal for real-world data. Missing samples will be handled automatically.")
  }
  
  # Subjects with all-zero or all-NA concentrations
  for (s in subjects) {
    s_conc <- conc_num[data[[subj_col]] == s]
    if (all(is.na(s_conc))) {
      add("WARNING", "Subjects",
          paste0("Subject '", s, "' has no measurable concentrations"),
          "All values are NA/missing.",
          "This subject will have empty NCA results. Consider excluding.")
    } else if (all(s_conc == 0, na.rm = TRUE)) {
      add("WARNING", "Subjects",
          paste0("Subject '", s, "' has all-zero concentrations"),
          "No drug detected in any sample.",
          "May indicate non-compliance or dosing failure. Review source data.")
    }
  }
  
  # ===========================================================================
  # 6. TIME ORDERING & DUPLICATES
  # ===========================================================================
  
  # Check for duplicate time points per subject
  dup_count <- 0
  dup_subjects <- c()
  for (s in subjects) {
    s_times <- time_num[data[[subj_col]] == s & !is.na(time_num)]
    if (any(duplicated(s_times))) {
      dup_count <- dup_count + sum(duplicated(s_times))
      dup_subjects <- c(dup_subjects, s)
    }
  }
  
  if (dup_count > 0) {
    add("WARNING", "Time",
        paste(dup_count, "duplicate time points across",
              length(dup_subjects), "subjects"),
        paste0("Subjects: ", paste(head(dup_subjects, 5), collapse = ", ")),
        "Duplicate times may cause issues. Check for replicate samples or data entry errors.")
  }
  
  # ===========================================================================
  # 7. CROSSOVER DESIGN CHECKS (if treatment column mapped)
  # ===========================================================================
  
  if (!is.null(col_map$treatment) && col_map$treatment %in% names(data)) {
    trt_col <- col_map$treatment
    treatments <- unique(data[[trt_col]])
    n_trt <- length(treatments)
    
    add("OK", "Design",
        paste(n_trt, "treatments:", paste(treatments, collapse = ", ")))
    
    if (n_trt < 2) {
      add("WARNING", "Design",
          "Only 1 treatment level found",
          paste("Value:", treatments[1]),
          "BE analysis requires at least 2 treatments. Check column mapping.")
    }
    
    # Check each subject has data for each treatment (crossover completeness)
    if (n_trt >= 2) {
      incomplete <- c()
      for (s in subjects) {
        s_trts <- unique(data[[trt_col]][data[[subj_col]] == s])
        if (length(s_trts) < n_trt) {
          incomplete <- c(incomplete, s)
        }
      }
      if (length(incomplete) > 0) {
        add("WARNING", "Design",
            paste(length(incomplete), "subjects missing data for one or more treatments"),
            paste0("Subjects: ", paste(head(incomplete, 5), collapse = ", "),
                   if (length(incomplete) > 5) paste0(" (+ ", length(incomplete)-5, " more)")),
            "Dropouts or incomplete crossover. These subjects will be excluded from BE analysis.")
      } else {
        add("OK", "Design",
            "All subjects have data for all treatments (complete crossover)")
      }
    }
  }
  
  # Period/Sequence checks
  if (!is.null(col_map$period) && col_map$period %in% names(data)) {
    periods <- unique(data[[col_map$period]])
    add("OK", "Design",
        paste(length(periods), "periods:", paste(sort(periods), collapse = ", ")))
  }
  
  if (!is.null(col_map$sequence) && col_map$sequence %in% names(data)) {
    seqs <- unique(data[[col_map$sequence]])
    add("OK", "Design",
        paste(length(seqs), "sequences:", paste(sort(seqs), collapse = ", ")))
    
    # Check balanced sequences
    subj_per_seq <- table(
      unique(data[, c(subj_col, col_map$sequence)])[[ col_map$sequence ]]
    )
    if (length(unique(subj_per_seq)) > 1) {
      add("INFO", "Design",
          "Unbalanced sequences",
          paste("Subjects per sequence:",
                paste(names(subj_per_seq), "=", subj_per_seq, collapse = ", ")),
          "Unbalanced designs are handled but may reduce power.")
    }
  }
  
  # ===========================================================================
  # 8. DOSE CHECKS
  # ===========================================================================
  
  if (!is.null(col_map$dose) && col_map$dose %in% names(data)) {
    dose_vals <- suppressWarnings(as.numeric(data[[col_map$dose]]))
    
    if (all(is.na(dose_vals))) {
      add("ERROR", "Dose",
          "Dose column contains no valid numeric values",
          "All values are NA or non-numeric.",
          "Check dose column mapping or enter dose manually in the NCA settings.")
    } else {
      doses <- unique(dose_vals[!is.na(dose_vals)])
      if (any(dose_vals <= 0, na.rm = TRUE)) {
        add("ERROR", "Dose",
            "Zero or negative dose values detected",
            paste("Values:", paste(head(unique(dose_vals[dose_vals <= 0]), 3), collapse=", ")),
            "Dose must be positive for NCA calculations (CL, Vz).")
      }
      add("OK", "Dose",
          paste("Dose values:", paste(sort(doses), collapse = ", ")))
    }
  }
  
  compile_findings(findings)
}


#' Compile findings list into a structured result object
compile_findings <- function(findings) {
  if (length(findings) == 0) {
    df <- data.frame(
      Severity = character(0), Category = character(0),
      Message = character(0), Detail = character(0),
      Action = character(0), stringsAsFactors = FALSE
    )
  } else {
    df <- do.call(rbind, findings)
  }
  
  list(
    findings   = df,
    n_errors   = sum(df$Severity == "ERROR"),
    n_warnings = sum(df$Severity == "WARNING"),
    n_info     = sum(df$Severity == "INFO"),
    n_ok       = sum(df$Severity == "OK"),
    pass       = sum(df$Severity == "ERROR") == 0
  )
}


#' Render data quality findings as HTML
#' @param qc_result Output from run_data_quality_check()
#' @return Shiny HTML tags
render_quality_report <- function(qc_result) {
  
  if (nrow(qc_result$findings) == 0) {
    return(tags$p(class = "text-muted", "No checks performed yet."))
  }
  
  # Header with summary counts
  severity_badge <- function(sev, n) {
    cls <- switch(sev,
                  "ERROR" = "bg-danger",
                  "WARNING" = "bg-warning text-dark",
                  "INFO" = "bg-info",
                  "OK" = "bg-success")
    if (n > 0 || sev == "OK") {
      tags$span(class = paste("badge", cls, "me-2"),
                paste(n, sev))
    }
  }
  
  summary_bar <- tags$div(
    class = "d-flex align-items-center mb-3",
    if (qc_result$pass) {
      tags$span(class = "text-success fw-bold me-3",
                icon("circle-check"), " Data ready for analysis")
    } else {
      tags$span(class = "text-danger fw-bold me-3",
                icon("circle-xmark"), " Issues must be fixed before analysis")
    },
    severity_badge("ERROR", qc_result$n_errors),
    severity_badge("WARNING", qc_result$n_warnings),
    severity_badge("INFO", qc_result$n_info),
    severity_badge("OK", qc_result$n_ok)
  )
  
  # Build findings list grouped by severity
  finding_items <- lapply(seq_len(nrow(qc_result$findings)), function(i) {
    f <- qc_result$findings[i, ]
    
    icon_cls <- switch(f$Severity,
                       "ERROR"   = "circle-xmark text-danger",
                       "WARNING" = "triangle-exclamation text-warning",
                       "INFO"    = "circle-info text-info",
                       "OK"      = "circle-check text-success")
    
    border_cls <- switch(f$Severity,
                         "ERROR"   = "border-danger",
                         "WARNING" = "border-warning",
                         "INFO"    = "border-info",
                         "OK"      = "border-success")
    
    tags$div(
      class = paste("d-flex align-items-start mb-2 p-2 rounded border-start border-3",
                    border_cls),
      style = "background: rgba(0,0,0,0.02);",
      tags$div(
        class = "me-2 mt-1",
        style = "min-width: 20px;",
        icon(icon_cls)
      ),
      tags$div(
        class = "flex-grow-1",
        tags$div(
          class = "fw-semibold",
          style = "font-size: 0.9rem;",
          tags$span(class = "text-muted small me-2",
                    paste0("[", f$Category, "]")),
          f$Message
        ),
        if (f$Detail != "") {
          tags$div(class = "text-muted small", f$Detail)
        },
        if (f$Action != "") {
          tags$div(class = "small mt-1",
                   icon("lightbulb", class = "text-warning me-1"),
                   tags$em(f$Action))
        }
      )
    )
  })
  
  tags$div(summary_bar, tags$div(finding_items))
}
