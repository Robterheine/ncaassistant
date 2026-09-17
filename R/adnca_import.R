# ============================================================================
# NCA Assistant — ADNCA import (Shiny-free, standalone)
# ============================================================================
# Converts an analysis-ready, ADNCA-shaped dataset into the flat table the
# analysis uses. One implementation serves three users:
# - the app's "CDISC ADNCA dataset" upload (R/mod_data_upload.R);
# - the standalone converter converters/adnca_to_flat.R;
# - the reproduction script shipped in an Analysis Record (as adnca_import.R).
#
# Rules for this file: base R only (readxl for Excel, digest optional); no
# Shiny; no dependency on other app files, so it can be copied on its own.
#
# The conversion makes choices explicit and REFUSES what it cannot convert
# safely: the time variable is chosen by the user, record selection flags are
# applied and counted, derived records are refused, analytes are never
# averaged, and a missing value is passed on only when a BLQ result explains
# it. Variable names follow common ADNCA usage; they are not validated against
# a specific version of the ADNCA Implementation Guide. NCA Assistant is not
# affiliated with, endorsed by or certified by CDISC.
# ============================================================================

ADNCA_TIME_VARS <- c(NRRLT = "Nominal time since the dose of the profile",
                     ARRLT = "Actual time since the dose of the profile",
                     MRRLT = "Actual time since the dose, pre-dose samples at 0")

.adnca_refuse <- function(...) {
  stop(structure(class = c("adnca_refusal", "error", "condition"),
                 list(message = paste0("Refused: ", ...), call = NULL)))
}

.first_present <- function(d, cands) {
  hit <- intersect(cands, names(d)); if (length(hit)) hit[1] else NULL
}

#' Read an ADNCA-shaped file (.csv or .xlsx); variable names upper-cased
#' @param read_args list(sep, dec, sheet)
adnca_read <- function(path, read_args = list(), ext = tools::file_ext(path)) {
  if (tolower(ext) %in% c("xlsx", "xls")) {
    d <- as.data.frame(readxl::read_excel(path, sheet = if (is.null(read_args$sheet)) 1 else read_args$sheet))
  } else {
    d <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                         sep = if (is.null(read_args$sep)) "," else read_args$sep,
                         dec = if (is.null(read_args$dec)) "." else read_args$dec)
  }
  names(d) <- toupper(names(d))
  d
}

#' Summarise an ADNCA-shaped dataset before conversion (for the upload screen)
#' @return list describing analytes, matrices, time variables, units, LLOQ,
#'   record-selection counts and the design variables found
adnca_inspect <- function(d) {
  vals <- function(v) if (v %in% names(d)) sort(unique(as.character(d[[v]][!is.na(d[[v]]) & d[[v]] != ""]))) else character(0)
  analyte_var <- .first_present(d, c("PARAMCD", "PCTESTCD"))
  time_vars <- intersect(c(names(ADNCA_TIME_VARS), "AFRLT"), names(d))
  list(
    n_records   = nrow(d),
    is_adnca    = all(c("USUBJID", "AVAL") %in% names(d)),
    analyte_var = analyte_var,
    analytes    = if (is.null(analyte_var)) character(0) else vals(analyte_var),
    matrices    = vals("PCSPEC"),
    time_vars   = setdiff(time_vars, "AFRLT"),
    has_afrlt   = "AFRLT" %in% time_vars,
    negative_times = vapply(setdiff(time_vars, "AFRLT"), function(v)
      any(suppressWarnings(as.numeric(d[[v]])) < 0, na.rm = TRUE), logical(1)),
    datetime_vars = intersect(c("ADTM", "ADT", "PCDTC", "EXSTDTC"), names(d)),
    units       = lapply(stats::setNames(nm = intersect(c("AVALU", "PCSTRESU", "RRLTU", "DOSEU"), names(d))), vals),
    lloq        = if ("PCLLOQ" %in% names(d)) sort(unique(d$PCLLOQ[!is.na(d$PCLLOQ)])) else numeric(0),
    n_anl01fl_excluded = if ("ANL01FL" %in% names(d)) sum(is.na(d$ANL01FL) | d$ANL01FL != "Y") else NA_integer_,
    n_not_done  = if ("PCSTAT" %in% names(d)) sum(!is.na(d$PCSTAT) & toupper(d$PCSTAT) == "NOT DONE") else 0L,
    n_derived   = if ("DTYPE" %in% names(d)) sum(!is.na(d$DTYPE) & trimws(d$DTYPE) != "") else 0L,
    treatment_var = .first_present(d, c("TRTP", "TRTA", "TRT01P", "TRT01A")),
    period_var    = .first_present(d, "APERIOD"),
    sequence_var  = .first_present(d, c("TRTSEQP", "TRTSEQA")),
    dose_var      = .first_present(d, c("DOSEA", "DOSEP"))
  )
}

#' Convert an ADNCA-shaped data frame to the flat analysis table
#'
#' @param d Data frame from adnca_read() (upper-case variable names)
#' @param time "NRRLT", "ARRLT" or "MRRLT" (required)
#' @param paramcd,pcspec Analyte / matrix to keep when there is more than one
#' @param zero_predose With ARRLT, set negative (pre-dose) times to 0
#' @return list(flat, notes, col_map, lloq, options); signals a condition of
#'   class "adnca_refusal" when the dataset cannot be converted safely
adnca_convert <- function(d, time, paramcd = NULL, pcspec = NULL, zero_predose = FALSE) {
  refuse <- .adnca_refuse
  if (missing(time) || length(time) != 1 || is.na(time) || !time %in% names(ADNCA_TIME_VARS)) {
    if (!missing(time) && identical(time, "AFRLT"))
      refuse("AFRLT is time since the FIRST dose; NCA needs time since the dose of each ",
             "profile. Choose NRRLT (nominal), ARRLT or MRRLT (actual).")
    refuse("choose the time variable explicitly: time = \"NRRLT\" (nominal), \"ARRLT\" or ",
           "\"MRRLT\" (actual). The choice changes AUC and must be documented.")
  }
  names(d) <- toupper(names(d))
  if (identical(paramcd, "")) paramcd <- NULL
  if (identical(pcspec, "")) pcspec <- NULL
  notes <- character(0)

  # --- Required variables ---------------------------------------------------
  for (v in c("USUBJID", "AVAL")) if (!v %in% names(d)) refuse("variable ", v, " is missing.")
  if (!time %in% names(d)) {
    dt <- intersect(c("ADTM", "ADT", "PCDTC", "EXSTDTC"), names(d))
    refuse("time variable ", time, " is missing",
           if (length(dt)) paste0(" (only date-times found: ", paste(dt, collapse = ", "),
                                  "). Elapsed time must be derived upstream, where it can be QC'd") else "",
           ".")
  }

  # --- Record selection -----------------------------------------------------
  if ("ANL01FL" %in% names(d)) {
    keep <- !is.na(d$ANL01FL) & d$ANL01FL == "Y"
    notes <- c(notes, sprintf("ANL01FL: kept %d record(s) with ANL01FL = \"Y\"; dropped %d.",
                              sum(keep), sum(!keep)))
    d <- d[keep, , drop = FALSE]
  } else {
    notes <- c(notes, "ANL01FL: not present; no analysis-flag selection applied.")
  }
  if ("PCSTAT" %in% names(d)) {
    nd <- !is.na(d$PCSTAT) & toupper(d$PCSTAT) == "NOT DONE"
    notes <- c(notes, sprintf("PCSTAT: dropped %d record(s) with PCSTAT = \"NOT DONE\".", sum(nd)))
    d <- d[!nd, , drop = FALSE]
  }
  if ("DTYPE" %in% names(d)) {
    derived <- !is.na(d$DTYPE) & trimws(d$DTYPE) != ""
    if (any(derived))
      refuse(sum(derived), " record(s) have DTYPE populated (",
             paste(unique(d$DTYPE[derived]), collapse = ", "), "). These are derived records, ",
             "for example BLQ values already imputed upstream. Applying the app's BLQ rule on ",
             "top would impute twice. Supply the dataset without derived records.")
  }

  # --- One analyte, one matrix ------------------------------------------------
  select_one <- function(var, chosen, label) {
    if (!var %in% names(d)) return(invisible(NULL))
    vals <- unique(d[[var]][!is.na(d[[var]])])
    if (!is.null(chosen)) {
      if (!chosen %in% vals) refuse(label, " \"", chosen, "\" not found in ", var, ".")
      n0 <- nrow(d); d <<- d[!is.na(d[[var]]) & d[[var]] == chosen, , drop = FALSE]
      notes <<- c(notes, sprintf("%s: kept %s = \"%s\" (%d record(s)); dropped %d.", label, var,
                                 chosen, nrow(d), n0 - nrow(d)))
    } else if (length(vals) > 1) {
      refuse("more than one ", label, " in ", var, " (", paste(vals, collapse = ", "), "). ",
             "Analyse each separately: choose the ", tolower(label), " to analyse ",
             "(in R: ", tolower(var), " = \"...\"). Values are never averaged or combined.")
    } else if (length(vals) == 1) {
      notes <<- c(notes, sprintf("%s: single %s = \"%s\".", label, var, vals))
    }
  }
  select_one(if ("PARAMCD" %in% names(d)) "PARAMCD" else "PCTESTCD", paramcd, "Analyte")
  select_one("PCSPEC", pcspec, "Matrix")

  # --- Units and LLOQ -----------------------------------------------------------
  units <- list()
  for (v in intersect(c("AVALU", "PCSTRESU", "RRLTU", "DOSEU"), names(d))) {
    u <- unique(d[[v]][!is.na(d[[v]]) & d[[v]] != ""])
    if (length(u) > 1) refuse("more than one unit in ", v, " (", paste(u, collapse = ", "),
                              "). Convert to one unit upstream, even if the units are equivalent.")
    if (length(u) == 1) { notes <- c(notes, sprintf("Unit %s: %s", v, u)); units[[v]] <- u }
  }
  lloq <- NULL
  if ("PCLLOQ" %in% names(d)) {
    l <- unique(d$PCLLOQ[!is.na(d$PCLLOQ)])
    if (length(l) > 1) refuse("more than one LLOQ in PCLLOQ (", paste(l, collapse = ", "), ").")
    if (length(l) == 1) { lloq <- l; notes <- c(notes, sprintf("LLOQ (PCLLOQ): %s — use this as the LLOQ in the app.", l)) }
  }

  # --- Time ---------------------------------------------------------------------
  t_raw <- d[[time]]
  t <- suppressWarnings(as.numeric(t_raw))
  bad_t <- is.na(t) & !is.na(t_raw) & trimws(as.character(t_raw)) != ""
  if (any(bad_t)) refuse(time, " contains non-numeric values (for example \"",
                         as.character(t_raw[bad_t][1]), "\"); dates, date-times and ISO durations ",
                         "are not converted.")
  if (anyNA(t)) refuse(sum(is.na(t)), " record(s) have no ", time, ".")
  if (any(t < 0)) {
    if (time == "ARRLT" && isTRUE(zero_predose)) {
      notes <- c(notes, sprintf("ARRLT: %d negative pre-dose time(s) set to 0 (zero_predose = TRUE, as MRRLT).",
                                sum(t < 0)))
      t[t < 0] <- 0
    } else {
      refuse(sum(t < 0), " negative ", time, " value(s) (pre-dose samples). Either use MRRLT, or ",
             "choose to set pre-dose times to 0 (in R: zero_predose = TRUE). A pre-dose sample at ",
             "a negative time changes AUC.")
    }
  }
  notes <- c(notes, sprintf("Time: %s (%s).", time,
                            switch(time, NRRLT = "nominal", ARRLT = "actual", MRRLT = "actual, pre-dose at 0")))

  # --- Profile structure ----------------------------------------------------------
  trt_var <- .first_present(d, c("TRTP", "TRTA", "TRT01P", "TRT01A"))
  per_var <- .first_present(d, "APERIOD")
  seq_var <- .first_present(d, c("TRTSEQP", "TRTSEQA"))
  dose_var <- .first_present(d, c("DOSEA", "DOSEP"))
  prof <- paste(d$USUBJID, if (!is.null(trt_var)) d[[trt_var]], if (!is.null(per_var)) d[[per_var]], sep = "||")

  starts <- tapply(t, prof, min); spans <- tapply(t, prof, function(x) diff(range(x)))
  late <- which(starts > 0 & starts > 0.2 * spans)
  if (length(late) > 0)
    refuse(length(late), " profile(s) do not start near time zero (e.g. first time ",
           signif(as.vector(starts[late])[1], 6), "). ", time, " may hold time since the first dose; NCA ",
           "needs time since the dose of each profile.")
  if (anyDuplicated(data.frame(prof, t)))
    refuse("duplicate times within a profile (subject",
           if (!is.null(trt_var)) " x treatment", if (!is.null(per_var)) " x period",
           "). The file may still hold more than one analyte, matrix or record type.")

  if (!is.null(dose_var)) {
    n_dose <- tapply(d[[dose_var]], prof, function(x) length(unique(x[!is.na(x)])))
    if (any(n_dose > 1))
      refuse("more than one dose within a subject/period (", dose_var, ") in ", sum(n_dose > 1),
             " profile(s). Only single-dose profiles are converted.")
  }
  if (all(c("EXSTDTC", "EXENDTC") %in% names(d)) &&
      any(!is.na(d$EXENDTC) & !is.na(d$EXSTDTC) & d$EXENDTC != d$EXSTDTC))
    refuse("infusion or multi-day exposure records (EXENDTC differs from EXSTDTC).")
  if (!is.null(trt_var)) {
    n_trt <- length(unique(d[[trt_var]]))
    notes <- c(notes, sprintf("Treatment (%s): %d level(s)%s.", trt_var, n_trt,
                              if (n_trt > 2) " — the app's bioequivalence analysis needs exactly 2" else ""))
  }

  # --- Missing concentrations -----------------------------------------------------
  conc <- d$AVAL
  miss <- is.na(conc)
  if (any(miss)) {
    blq_text <- NULL
    for (v in intersect(c("PCORRES", "PCSTRESC", "AVALC"), names(d))) {
      txt <- as.character(d[[v]])
      if (any(grepl("^\\s*(<|(BLQ|BQL|BLOQ|ND|NQ)\\b)", txt[miss], ignore.case = TRUE, perl = TRUE))) { blq_text <- txt; break }
    }
    is_blq <- if (is.null(blq_text)) rep(FALSE, nrow(d)) else
      miss & grepl("^\\s*(<|(BLQ|BQL|BLOQ|ND|NQ)\\b)", blq_text, ignore.case = TRUE, perl = TRUE)
    if (any(miss & !is_blq))
      refuse(sum(miss & !is_blq), " record(s) have AVAL missing without a BLQ result. A missing ",
             "value can mean 'not taken' or 'below LLOQ'; resolve these upstream.")
    conc <- as.character(conc)
    conc[is_blq] <- trimws(blq_text[is_blq])
    notes <- c(notes, sprintf("AVAL missing with a BLQ result: %d record(s) passed on as text (e.g. \"%s\"); set the LLOQ and BLQ rule in the app.",
                              sum(is_blq), conc[is_blq][1]))
  }

  # --- Flat table ---------------------------------------------------------------------
  flat <- data.frame(Subject = d$USUBJID, Time = t, Conc = conc, stringsAsFactors = FALSE)
  if (!is.null(trt_var))  flat$Treatment <- d[[trt_var]]
  if (!is.null(per_var))  flat$Period    <- d[[per_var]]
  if (!is.null(seq_var))  flat$Sequence  <- d[[seq_var]]
  if (!is.null(dose_var)) flat$Dose      <- d[[dose_var]]
  flat <- flat[order(flat$Subject, if (!is.null(per_var)) flat$Period else 0, flat$Time), , drop = FALSE]
  rownames(flat) <- NULL

  col_map <- list(subject = "Subject", time = "Time", conc = "Conc")
  if (!is.null(trt_var))  col_map$treatment <- "Treatment"
  if (!is.null(per_var))  col_map$period    <- "Period"
  if (!is.null(seq_var))  col_map$sequence  <- "Sequence"
  if (!is.null(dose_var)) col_map$dose      <- "Dose"

  sources <- c(Subject = "USUBJID", Time = time, Conc = "AVAL", Treatment = trt_var,
               Period = per_var, Sequence = seq_var, Dose = dose_var)
  list(flat = flat, notes = notes, col_map = col_map, lloq = lloq, units = units,
       sources = sources,
       options = list(time = time, paramcd = paramcd, pcspec = pcspec,
                      zero_predose = isTRUE(zero_predose)))
}

#' Hash of a file for logs (SHA-256 with digest, otherwise MD5 from base R)
adnca_file_hash <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) paste0("SHA-256 ", digest::digest(file = path, algo = "sha256"))
  else paste0("MD5 ", unname(tools::md5sum(path)))
}

#' Conversion log lines
adnca_log_lines <- function(result, input, n_in, output = NULL, n_out = nrow(result$flat)) {
  c("ADNCA to flat file conversion log",
    "=================================",
    paste0("Date:      ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Converter: adnca_import.R (NCA Assistant), R ", getRversion()),
    paste0("Input:     ", basename(input), " (", n_in, " records; ", adnca_file_hash(input), ")"),
    if (!is.null(output)) paste0("Output:    ", basename(output), " (", n_out, " records; ", adnca_file_hash(output), ")")
    else paste0("Output:    ", n_out, " records (converted in the app)"),
    "", "Choices and record selection:", paste0("- ", result$notes), "",
    "Column mapping in NCA Assistant:",
    paste0("- ", names(result$sources), " = ", names(result$sources), " (from ", result$sources, ")"))
}

#' Convert an ADNCA-shaped file and write the flat CSV and a conversion log
#'
#' @param input,output Input (.csv/.xlsx) and output (.csv) paths
#' @param time "NRRLT", "ARRLT" or "MRRLT" (required)
#' @param paramcd,pcspec,zero_predose See adnca_convert()
#' @param log Log path (default: output name with _conversion_log.txt)
#' @return invisibly, list(data = flat data frame, log = log lines)
adnca_to_flat <- function(input, output, time, paramcd = NULL, pcspec = NULL,
                          zero_predose = FALSE, log = NULL) {
  if (missing(time)) time <- NA_character_
  d <- adnca_read(input)
  res <- adnca_convert(d, time = time, paramcd = paramcd, pcspec = pcspec, zero_predose = zero_predose)
  if (is.null(log)) log <- sub("\\.csv$", "", output, ignore.case = TRUE)
  if (!grepl("_conversion_log\\.txt$", log)) log <- paste0(log, "_conversion_log.txt")
  utils::write.csv(res$flat, output, row.names = FALSE)
  lines <- adnca_log_lines(res, input, nrow(d), output)
  writeLines(lines, log)
  message("Wrote ", output, " (", nrow(res$flat), " records) and ", log)
  invisible(list(data = res$flat, log = lines, result = res))
}
