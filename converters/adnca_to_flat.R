# ============================================================================
# adnca_to_flat.R — convert an ADNCA-shaped dataset to an NCA Assistant flat file
# ============================================================================
# Standalone: base R only (readxl for .xlsx input). Not part of the app.
#
# NCA Assistant accepts flat files: one row per sample with subject, time,
# concentration and optional treatment, period, sequence and dose. A CDISC
# ADaM ADNCA (or ADNCA-shaped) dataset carries more than that: record
# selection flags, derived records, several time variables and sometimes
# several analytes. This converter makes those choices explicit, applies the
# record selection, REFUSES anything it cannot convert safely, and writes a
# log of every choice and count so the conversion can be documented.
#
# Usage in R:
#   source("adnca_to_flat.R")
#   adnca_to_flat("adnca.csv", "flat.csv", time = "NRRLT")
#
# Usage from a terminal:
#   Rscript adnca_to_flat.R adnca.csv flat.csv NRRLT
#
# Variable names follow common ADNCA usage (USUBJID, PARAMCD, AVAL, AVALU,
# ARRLT, NRRLT, MRRLT, AFRLT, TRTP, APERIOD, TRTSEQP, DOSEA, ANL01FL, DTYPE,
# PCSPEC, PCLLOQ, PCSTAT). They have not been checked against a specific
# version of the ADNCA Implementation Guide; verify them for your data.
# This tool is not affiliated with, endorsed by or certified by CDISC.
# ============================================================================

#' Convert an ADNCA-shaped dataset to a flat NCA Assistant file
#'
#' @param input   Path to a .csv or .xlsx file
#' @param output  Path of the flat .csv to write
#' @param time    Time variable to use: "NRRLT" (nominal), "ARRLT" (actual) or
#'                "MRRLT" (actual, pre-dose set to 0). Required; there is no
#'                default because the choice changes AUC.
#' @param paramcd Analyte to keep when the file holds more than one PARAMCD
#' @param pcspec  Matrix to keep when the file holds more than one PCSPEC
#' @param zero_predose With time = "ARRLT", set negative (pre-dose) times to 0,
#'                as MRRLT does. Without it, negative times are refused.
#' @param log     Path of the conversion log (default: next to output)
#' @return invisibly, list(data = flat data frame, log = log lines)
adnca_to_flat <- function(input, output, time, paramcd = NULL, pcspec = NULL,
                          zero_predose = FALSE, log = NULL) {
  refuse <- function(...) stop(structure(class = c("adnca_refusal", "error", "condition"),
                                         list(message = paste0("Refused: ", ...), call = NULL)))
  if (missing(time) || length(time) != 1 || !time %in% c("NRRLT", "ARRLT", "MRRLT")) {
    if (!missing(time) && identical(time, "AFRLT"))
      refuse("AFRLT is time since the FIRST dose; NCA needs time since the dose of each ",
             "profile. Choose NRRLT (nominal), ARRLT or MRRLT (actual).")
    refuse("choose the time variable explicitly: time = \"NRRLT\" (nominal), \"ARRLT\" or ",
           "\"MRRLT\" (actual). The choice changes AUC and must be documented.")
  }
  if (is.null(log)) log <- sub("\\.csv$", "", output, ignore.case = TRUE)
  if (!grepl("_conversion_log\\.txt$", log)) log <- paste0(log, "_conversion_log.txt")

  ext <- tolower(tools::file_ext(input))
  d <- if (ext %in% c("xlsx", "xls")) as.data.frame(readxl::read_excel(input)) else
       utils::read.csv(input, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  names(d) <- toupper(names(d))
  n_in <- nrow(d)
  notes <- character(0)
  col <- function(nm) if (nm %in% names(d)) d[[nm]] else NULL
  first_present <- function(cands) { hit <- intersect(cands, names(d)); if (length(hit)) hit[1] else NULL }

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
             "Analyse each separately: choose one with ", tolower(var), " = \"...\".")
    } else if (length(vals) == 1) {
      notes <<- c(notes, sprintf("%s: single %s = \"%s\".", label, var, vals))
    }
  }
  select_one(if ("PARAMCD" %in% names(d)) "PARAMCD" else "PCTESTCD", paramcd, "Analyte")
  select_one("PCSPEC", pcspec, "Matrix")

  # --- Units and LLOQ -----------------------------------------------------------
  for (v in intersect(c("AVALU", "PCSTRESU", "RRLTU", "DOSEU"), names(d))) {
    u <- unique(d[[v]][!is.na(d[[v]]) & d[[v]] != ""])
    if (length(u) > 1) refuse("more than one unit in ", v, " (", paste(u, collapse = ", "),
                              "). Convert to one unit upstream, even if the units are equivalent.")
    if (length(u) == 1) notes <- c(notes, sprintf("Unit %s: %s", v, u))
  }
  if ("PCLLOQ" %in% names(d)) {
    l <- unique(d$PCLLOQ[!is.na(d$PCLLOQ)])
    if (length(l) > 1) refuse("more than one LLOQ in PCLLOQ (", paste(l, collapse = ", "), ").")
    if (length(l) == 1) notes <- c(notes, sprintf("LLOQ (PCLLOQ): %s — enter this value in the app.", l))
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
      refuse(sum(t < 0), " negative ", time, " value(s). With ARRLT, either use MRRLT or set ",
             "zero_predose = TRUE to set pre-dose times to 0; a pre-dose sample at a negative time ",
             "changes AUC.")
    }
  }
  notes <- c(notes, sprintf("Time: %s (%s).", time,
                            switch(time, NRRLT = "nominal", ARRLT = "actual", MRRLT = "actual, pre-dose at 0")))

  # --- Profile structure ----------------------------------------------------------
  trt_var <- first_present(c("TRTP", "TRTA", "TRT01P", "TRT01A"))
  per_var <- first_present(c("APERIOD"))
  seq_var <- first_present(c("TRTSEQP", "TRTSEQA"))
  dose_var <- first_present(c("DOSEA", "DOSEP"))
  prof <- paste(d$USUBJID, if (!is.null(trt_var)) d[[trt_var]], if (!is.null(per_var)) d[[per_var]], sep = "||")

  starts <- tapply(t, prof, min); spans <- tapply(t, prof, function(x) diff(range(x)))
  late <- names(starts)[starts > 0 & starts > 0.2 * spans]
  if (length(late) > 0)
    refuse(length(late), " profile(s) do not start near time zero (e.g. first time ",
           signif(starts[late][1], 6), "). ", time, " may hold time since the first dose; NCA ",
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
      if (any(grepl("^\\s*(<|BLQ|BQL|BLOQ)", txt[miss], ignore.case = TRUE))) { blq_text <- txt; break }
    }
    is_blq <- if (is.null(blq_text)) rep(FALSE, nrow(d)) else
      miss & grepl("^\\s*(<|BLQ|BQL|BLOQ)", blq_text, ignore.case = TRUE)
    if (any(miss & !is_blq))
      refuse(sum(miss & !is_blq), " record(s) have AVAL missing without a BLQ result. A missing ",
             "value can mean 'not taken' or 'below LLOQ'; resolve these upstream.")
    conc <- as.character(conc)
    conc[is_blq] <- trimws(blq_text[is_blq])
    notes <- c(notes, sprintf("AVAL missing with a BLQ result: %d record(s) passed on as text (e.g. \"%s\"); set the LLOQ and BLQ rule in the app.",
                              sum(is_blq), conc[is_blq][1]))
  }

  # --- Write ------------------------------------------------------------------------
  flat <- data.frame(Subject = d$USUBJID, Time = t, Conc = conc, stringsAsFactors = FALSE)
  if (!is.null(trt_var))  flat$Treatment <- d[[trt_var]]
  if (!is.null(per_var))  flat$Period    <- d[[per_var]]
  if (!is.null(seq_var))  flat$Sequence  <- d[[seq_var]]
  if (!is.null(dose_var)) flat$Dose      <- d[[dose_var]]
  flat <- flat[order(flat$Subject, if (!is.null(per_var)) flat$Period else 0, flat$Time), , drop = FALSE]
  rownames(flat) <- NULL
  utils::write.csv(flat, output, row.names = FALSE)

  hash <- function(f) {
    if (requireNamespace("digest", quietly = TRUE)) paste0("SHA-256 ", digest::digest(file = f, algo = "sha256"))
    else paste0("MD5 ", unname(tools::md5sum(f)))
  }
  lines <- c("ADNCA to flat file conversion log",
             "=================================",
             paste0("Date:      ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
             paste0("Converter: adnca_to_flat.R, R ", getRversion()),
             paste0("Input:     ", basename(input), " (", n_in, " records; ", hash(input), ")"),
             paste0("Output:    ", basename(output), " (", nrow(flat), " records; ", hash(output), ")"),
             "", "Choices and record selection:", paste0("- ", notes), "",
             "Column mapping in NCA Assistant:",
             "- Subject = USUBJID, Time = Time, Concentration = Conc",
             if (!is.null(trt_var)) paste0("- Treatment = Treatment (from ", trt_var, ")"),
             if (!is.null(per_var)) paste0("- Period = Period (from ", per_var, ")"),
             if (!is.null(seq_var)) paste0("- Sequence = Sequence (from ", seq_var, ")"),
             if (!is.null(dose_var)) paste0("- Dose = Dose (from ", dose_var, ")"))
  writeLines(lines, log)
  message("Wrote ", output, " (", nrow(flat), " records) and ", log)
  invisible(list(data = flat, log = lines))
}

# Command line: Rscript adnca_to_flat.R input.csv output.csv NRRLT [PARAMCD]
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3) {
    cat("Usage: Rscript adnca_to_flat.R input.csv output.csv NRRLT|ARRLT|MRRLT [PARAMCD]\n")
    quit(status = 1)
  }
  res <- tryCatch(adnca_to_flat(args[1], args[2], time = args[3],
                                paramcd = if (length(args) >= 4) args[4] else NULL),
                  adnca_refusal = function(e) { message(conditionMessage(e)); quit(status = 2) })
}
