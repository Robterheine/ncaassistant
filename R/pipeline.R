# ============================================================================
# NCA Assistant — Data pipeline (Shiny-free)
# ============================================================================
# Everything between "a file on disk" and "an NCA result table" lives here:
# reading, column detection, BLQ handling, profile definition and the NCA
# call. The app sources this file, the validation suite tests it directly,
# and every Analysis Record ships an exact copy as nca_pipeline.R, which the
# reproduction script sources. There is therefore one implementation of the
# pipeline, not an app version and a transcribed script version.
#
# Rules for this file:
# - No Shiny: no input$, no shared$, no showNotification().
# - Dependencies: base R, stats, NonCompart; readxl only for Excel files;
#   digest only for hashes (optional).
# - The canonical object is pk_dataset (see prepare_pk_dataset()). Existing
#   analysis code reads only $data and $col_map; every other field is
#   additive metadata.
# ============================================================================

#' Read an uploaded PK data file exactly as the app does
#'
#' @param path File path
#' @param read_args list(sep, dec, sheet) as chosen in the upload screen
#' @param ext File extension; defaults to the extension of `path`
read_pk_file <- function(path, read_args = list(), ext = tools::file_ext(path)) {
  sep   <- if (is.null(read_args$sep))   "," else read_args$sep
  dec   <- if (is.null(read_args$dec))   "." else read_args$dec
  sheet <- if (is.null(read_args$sheet)) 1   else read_args$sheet
  if (tolower(ext) %in% c("xlsx", "xls")) {
    # guess_max: readxl guesses a column's type from its first 1000 rows by
    # default, and text further down (e.g. "BLQ") then became missing without
    # a warning. Excel's row limit makes it look at every row.
    readxl::read_excel(path, sheet = sheet, guess_max = EXCEL_MAX_ROWS)
  } else {
    # Read as text first: a column with a leading zero ("001") stays text, so
    # IDs 001 and 01 are not both read as 1; the others are converted as
    # read.csv would. The bytes are read unchanged; when they are not valid
    # UTF-8 (Excel's "CSV" on Windows writes Windows-1252, e.g. a micro sign)
    # the file is read as Latin-1 and converted to UTF-8. (fileEncoding would
    # convert to the session's encoding and truncate the file in a C locale.)
    rd <- function(enc) utils::read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE,
                                        colClasses = "character", encoding = enc)
    raw <- rd("unknown")
    if (any(vapply(raw, function(v) any(!validUTF8(v)), logical(1))) || any(!validUTF8(names(raw)))) {
      raw <- rd("latin1")
      raw[] <- lapply(raw, function(v) iconv(v, "latin1", "UTF-8"))
      names(raw) <- iconv(names(raw), "latin1", "UTF-8")
    }
    as.data.frame(lapply(raw, function(v) {
      w <- trimws(v)
      if (any(grepl("^0[0-9]", w))) v else utils::type.convert(v, as.is = TRUE, dec = dec)
    }), stringsAsFactors = FALSE, check.names = FALSE)
  }
}

EXCEL_MAX_ROWS <- 1048576

#' Read decimal-comma numbers stored as text when the file uses a decimal comma
#'
#' read.csv(dec = ",") converts a column only when every value is a number.
#' A concentration column that also holds BLQ text ("<0,5") stays text, and
#' values such as "4,25" would become missing. When the upload's decimal mark
#' is a comma, values that are plain decimal-comma numbers are rewritten with
#' a point, and a point is read as a thousands separator ("12.500" = 12500,
#' "1.234,5" = 1234.5), as Excel writes it. Any other value with a point
#' ("0.25") contradicts the chosen decimal mark: it becomes missing here and
#' the data quality check refuses the file (comma_file_point_values()).
#' Anything else (BLQ text, other text) is left unchanged.
#' @param x Column as read
#' @param dec Decimal mark chosen at upload
normalise_decimal_comma <- function(x, dec) {
  if (!identical(dec, ",") || !(is.character(x) || is.factor(x))) return(x)
  x <- as.character(x)
  v <- trimws(x)
  num_comma <- grepl("^-?\\d+,\\d+$", v)
  x[num_comma] <- sub(",", ".", v[num_comma], fixed = TRUE)
  grouped <- grepl(THOUSANDS_PATTERN, v)
  x[grouped] <- sub(",", ".", gsub(".", "", v[grouped], fixed = TRUE), fixed = TRUE)
  x[v %in% comma_file_point_values(v, dec)] <- NA_character_
  x
}

#' A number with points as thousands separators and an optional decimal comma
THOUSANDS_PATTERN <- "^-?\\d{1,3}(\\.\\d{3})+(,\\d+)?$"

#' Values written with a decimal point in a file read with a decimal comma
#' @return the distinct offending values (empty when there are none)
comma_file_point_values <- function(x, dec) {
  if (!identical(dec, ",") || !(is.character(x) || is.factor(x))) return(character(0))
  v <- trimws(as.character(x))
  unique(v[grepl("^-?\\d*\\.\\d+$", v) & !grepl(THOUSANDS_PATTERN, v)])
}

#' Name of the column that marks values set by the BLQ rule
BLQ_FLAG_COLUMN <- "BLQ_flag"

#' Text that means "below the limit of quantification"
#'
#' "<x", BLQ, BQL, BLOQ, ND (not detected) and NQ (not quantifiable), in any
#' case. NS (no sample), N/A and MISSING are NOT BLQ: they stay missing.
BLQ_TEXT_PATTERN <- "^\\s*(<|(BLQ|BQL|BLOQ|ND|NQ)\\b)"
is_blq_text <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl(BLQ_TEXT_PATTERN, x, ignore.case = TRUE, perl = TRUE) &
    is.na(suppressWarnings(as.numeric(x)))
}

#' Count BLQ text entries and suggest an LLOQ from "<x" values
#'
#' @param conc_raw Concentration column as uploaded
#' @return list(n_blq_text, suggested_lloq (NULL if none))
blq_text_summary <- function(conc_raw) {
  conc_chr <- as.character(conc_raw)
  n_blq_text <- sum(is_blq_text(conc_chr))
  suggested <- NULL
  lt_vals <- conc_chr[grepl("^<", conc_chr)]
  if (length(lt_vals) > 0) {
    lt_nums <- suppressWarnings(as.numeric(gsub(",", ".", gsub("^<\\s*", "", lt_vals))))
    lt_nums <- lt_nums[!is.na(lt_nums)]
    if (length(lt_nums) > 0) suggested <- min(lt_nums)
  }
  list(n_blq_text = n_blq_text, suggested_lloq = suggested)
}

#' Turn an uploaded table into the canonical analysis dataset
#'
#' Converts time and concentration to numbers (rewriting "<x" BLQ text to a
#' placeholder below the LLOQ so the BLQ rule sees it), drops rows without a
#' time, sorts by subject and time, applies the BLQ rule, and detects the
#' study design. Quality checks run before this step in the app and are
#' attached through opts$qc.
#'
#' @param raw Data frame as read by read_pk_file()
#' @param col_map Column mapping (subject, time, conc and optional
#'   treatment, period, sequence, dose)
#' @param opts list(lloq, blq_rule, door, file_name, file_path, read_args,
#'   pipeline_sha256, qc, interlocks). read_args$dec = "," also reads
#'   decimal-comma numbers stored as text (normalise_decimal_comma()).
#' @return pk_dataset: list(data, col_map, design, provenance, analyte, units,
#'   time_basis, blq, flags, interlocks, qc)
prepare_pk_dataset <- function(raw, col_map, opts = list()) {
  lloq <- if (is.null(opts$lloq)) 0 else opts$lloq
  rule <- if (is.null(opts$blq_rule)) "rule1" else opts$blq_rule

  data <- raw
  # Leading/trailing spaces in IDs and design labels ("Test " vs "Test") would
  # otherwise create extra subjects, treatments or periods
  for (cc in unique(c(col_map$subject, col_map$treatment, col_map$period, col_map$sequence))) {
    if (!is.null(cc) && cc %in% names(data) && (is.character(data[[cc]]) || is.factor(data[[cc]])))
      data[[cc]] <- trimws(as.character(data[[cc]]))
  }
  dec <- opts$read_args$dec
  data[[col_map$time]] <- normalise_decimal_comma(data[[col_map$time]], dec)
  data[[col_map$conc]] <- normalise_decimal_comma(data[[col_map$conc]], dec)
  if (!is.null(col_map$dose) && col_map$dose %in% names(data))
    data[[col_map$dose]] <- normalise_decimal_comma(data[[col_map$dose]], dec)
  data[[col_map$time]] <- suppressWarnings(as.numeric(data[[col_map$time]]))

  # Pre-process BLQ text entries before numeric conversion. Text such as
  # "<0,195", "<0.1", "BLQ" or "ND" becomes NA under as.numeric(), so
  # apply_blq_rules would never see it as BLQ (it checks !is.na(x) & x <
  # lloq). Setting it to 0 ensures it is flagged, and the selected rule then
  # decides its value. "NS" (no sample) and similar stay missing.
  n_text <- 0L
  if (lloq > 0) {
    conc_raw_chr <- as.character(data[[col_map$conc]])
    blq_text_mask <- is_blq_text(conc_raw_chr)
    n_text <- sum(blq_text_mask)
    if (any(blq_text_mask)) {
      conc_raw_chr[blq_text_mask] <- "0"  # placeholder: 0 < lloq -> BLQ
    }
    data[[col_map$conc]] <- suppressWarnings(as.numeric(conc_raw_chr))
  } else {
    data[[col_map$conc]] <- suppressWarnings(as.numeric(data[[col_map$conc]]))
  }

  # Drop unusable rows and sort BEFORE applying the BLQ rules. Rules 1, 5 and
  # 6 are positional ("first quantifiable", "post-Cmax"), so running them on
  # file order rather than time order imputes the wrong samples whenever the
  # upload is not already sorted.
  n_before <- nrow(data)
  data <- data[!is.na(data[[col_map$time]]), ]
  data <- data[order(data[[col_map$subject]], data[[col_map$time]]), ]
  n_dropped <- n_before - nrow(data)

  if (lloq > 0) {
    data <- apply_blq_rules(data, col_map, rule = rule, lloq = lloq)
  } else {
    # Without an LLOQ no value is set by a BLQ rule; a column of this name in
    # the uploaded file must not feed the partial AUC notes
    data[[BLQ_FLAG_COLUMN]] <- NULL
  }

  sha <- if (!is.null(opts$file_path) && file.exists(opts$file_path) &&
             requireNamespace("digest", quietly = TRUE)) {
    digest::digest(file = opts$file_path, algo = "sha256")
  } else NA_character_

  list(
    data       = data,
    col_map    = col_map,
    design     = detect_study_design(data, col_map),
    provenance = list(door = if (is.null(opts$door)) "flat" else opts$door,
                      file_name = opts$file_name, file_path = opts$file_path,
                      sha256 = sha, read_args = opts$read_args,
                      pipeline_sha256 = opts$pipeline_sha256),
    analyte    = list(name = NA_character_, paramcd = NA_character_,
                      pctestcd = NA_character_, matrix = NA_character_),
    units      = list(conc = NA_character_, time = NA_character_,
                      dose = NA_character_, mw = NA_real_, source = "user"),
    time_basis = list(col = col_map$time, kind = NA_character_,
                      cdisc_var = NA_character_, user_confirmed = FALSE),
    blq        = list(lloq = lloq, rule = if (lloq > 0) rule else "none",
                      source = if (lloq > 0) "app_rule" else "none",
                      text_tokens_converted = n_text,
                      na_policy = "missing"),
    flags      = list(anl01fl_applied = FALSE, dtype_present = FALSE,
                      n_rows_dropped = n_dropped),
    interlocks = if (!is.null(opts$interlocks)) opts$interlocks else
                 data.frame(Severity = character(0), Category = character(0),
                            Message = character(0), Detail = character(0),
                            Action = character(0), stringsAsFactors = FALSE),
    qc         = opts$qc
  )
}

#' One dose per profile: the maximum of the Dose column within each profile
#' (subject x treatment x period, as mapped), named by profile key
#'
#' A subject can receive different doses in different periods (e.g. a
#' dose-proportionality crossover); each profile then uses its own dose.
#' run_nca() matches these names to its profile keys.
dose_by_profile <- function(data, col_map) {
  d <- suppressWarnings(as.numeric(data[[col_map$dose]]))
  key <- profile_key(data, col_map)$key
  v <- tapply(d, key, max, na.rm = TRUE)
  stats::setNames(as.numeric(v), names(v))
}

#' One dose per subject: the maximum of the Dose column, named by subject ID
#' (kept for records made before per-profile doses)
#'
#' run_nca() matches a multi-element dose vector by these names.
dose_by_subject <- function(data, col_map) {
  d <- suppressWarnings(as.numeric(data[[col_map$dose]]))
  v <- tapply(d, as.character(data[[col_map$subject]]), max, na.rm = TRUE)
  stats::setNames(as.numeric(v), names(v))
}

#' Suggest a column mapping from common column names
#'
#' A required field (subject, time, conc) that matches no known name falls
#' back to a column by position. Those fields are listed in
#' attr(result, "unmatched"), so the caller can warn that the suggestion is a
#' guess (for example a file without a Subject column, where the fallback
#' would pick the Time column).
auto_detect_columns <- function(cols) {
  cols_lower <- tolower(cols)
  unmatched <- character(0)

  detect <- function(patterns, fallback_idx = 1, field) {
    for (p in patterns) {
      match <- grep(p, cols_lower, value = FALSE)
      if (length(match) > 0) return(cols[match[1]])
    }
    unmatched <<- c(unmatched, field)
    return(cols[min(fallback_idx, length(cols))])
  }
  
  detect_optional <- function(patterns) {
    for (p in patterns) {
      match <- grep(p, cols_lower, value = FALSE)
      if (length(match) > 0) return(cols[match[1]])
    }
    return("")
  }
  
  out <- list(
    subject   = detect(c("^subj", "^id$", "^subject", "^usubjid", "^patid",
                          "^pat$", "^proband", "^teilnehmer"), 1, "subject"),
    # Time after dose before time since the first dose (NONMEM TAD vs TIME)
    time      = detect(c("^tad$", "^time", "^tpt", "^hours?$", "^hour", "^apts",
                          "^ntim", "^zeit", "^tid"), 2, "time"),
    conc      = detect(c("^conc", "^dv$", "^cp[^a-z]", "^cp$", "^concentration",
                          "^result", "^konz", "^plasma", "ug.l", "ng.ml"), 3, "conc"),
    treatment = detect_optional(c("^trt", "^treat", "^form", "^drug", "^arm",
                                   "^behandl")),
    period    = detect_optional(c("^per", "^period", "^prd", "^phase", "^aperiod$", "^occ")),
    sequence  = detect_optional(c("^seq", "^grp", "^sequence")),
    dose      = detect_optional(c("^dose", "^amt$", "^amount", "^dosis"))
  )
  attr(out, "unmatched") <- unmatched
  # Another column that also looks like the time or the concentration
  # (TimeNominal and TimeActual, TIME and TAD): the first match is used, and
  # the upload page says which other one exists
  alt <- function(pats, chosen) {
    hits <- unique(unlist(lapply(pats, function(p) cols[grepl(p, cols_lower)])))
    setdiff(hits, chosen)
  }
  attr(out, "alternatives") <- list(
    time = alt(c("^tad$", "^time", "^tpt", "^hours?$", "^ntim"), out$time),
    conc = alt(c("^conc", "^dv$", "^cp[^a-z]", "^cp$", "^concentration", "^result"), out$conc))
  out
}

#' Identify the concentration-time profile each row belongs to
#'
#' A profile is one subject, under one treatment, in one period. Treatment
#' and Period are included whenever they are mapped, so a replicate design
#' (the same treatment given in two periods) yields one profile per
#' administration instead of merging them. The same key is used by the BLQ
#' rules, the NCA and the generated reproduction script, so all three agree on
#' what a profile is. The key is applied unconditionally: its shape depends on
#' the mapping, never on the data.
#'
#' @param data Data frame
#' @param col_map Column mapping (subject, and optionally treatment/period)
#' @return list(key   = character profile key per row,
#'              parts = data.frame per row with Subject and, when mapped,
#'                      Treatment and Period (all character),
#'              cols  = names(parts))
profile_key <- function(data, col_map) {
  parts <- data.frame(Subject = as.character(data[[col_map$subject]]),
                      stringsAsFactors = FALSE)
  if (!is.null(col_map$treatment) && col_map$treatment %in% names(data))
    parts$Treatment <- as.character(data[[col_map$treatment]])
  if (!is.null(col_map$period) && col_map$period %in% names(data))
    parts$Period <- as.character(data[[col_map$period]])
  key <- if (ncol(parts) == 1) parts$Subject else do.call(paste, c(parts, sep = "||"))
  list(key = key, parts = parts, cols = names(parts))
}

#' Human-readable label for each profile, e.g. "12 | Test | P3"
#' @param parts data.frame with Subject and optionally Treatment, Period
profile_labels <- function(parts) {
  lab <- as.character(parts$Subject)
  if ("Treatment" %in% names(parts)) lab <- paste(lab, "|", parts$Treatment)
  if ("Period" %in% names(parts))    lab <- paste0(lab, " | P", parts$Period)
  lab
}

#' One line per profile in a concentration-time plot
#'
#' Lines grouped by subject alone join a crossover subject's Test and
#' Reference profiles into one zig-zag line. A profile is subject x treatment
#' x period, as in the NCA.
#' @return factor, one level per profile
profile_group <- function(data, col_map) {
  cols <- c(col_map$subject, col_map$treatment, col_map$period)
  cols <- cols[!is.null(cols) & nzchar(cols) & cols %in% names(data)]
  interaction(lapply(cols, function(cc) as.character(data[[cc]])), drop = TRUE, sep = " | ")
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

#' Apply BLQ (Below Limit of Quantification) handling rules
#' 
#' Implements the BLQ rules:
#'   Rule 1: Pre-first-quantifiable set to 0; post-last-quantifiable set to Missing
#'   Rule 2: All BLQ set to 0
#'   Rule 3: All BLQ set to Missing (NA)
#'   Rule 4: All BLQ set to LLOQ/2 (a pre-dose sample at time <= 0: 0)
#'   Rule 5: Pre-Cmax BLQ = 0; post-Cmax BLQ = Missing
#'   Rule 6: After dosing and before the first quantifiable value: LLOQ/2;
#'           all other BLQ (including a pre-dose sample) set to 0
#'
#' @param data Data frame with subject/time/concentration
#' @param col_map Column mapping list
#' @param rule Character: one of "rule1" through "rule6"
#' @param lloq Numeric: lower limit of quantification
#' @return Modified data frame
apply_blq_rules <- function(data, col_map, rule = "rule1", lloq = 0) {
  
  subj_col <- col_map$subject
  time_col <- col_map$time
  conc_col <- col_map$conc

  # Rules 1, 5 and 6 are positional: they depend on which samples come first
  # and last within ONE concentration-time profile. A profile is one subject
  # under one treatment in one period (profile_key()). Grouping more coarsely
  # concatenates profiles (Test and Reference periods, or both administrations
  # of a replicate) and then applies "first/last quantifiable" (rules 1, 6)
  # and "Cmax" (rule 5) across them at once, which imputes the wrong samples
  # and biases the ratio the BE analysis reports.
  prof_key <- profile_key(data, col_map)$key

  # These rules are also order-dependent, so each profile is visited in time
  # order regardless of how the rows happen to be arranged in the file.
  profile_idx <- function(k) {
    i <- which(prof_key == k)
    i[order(suppressWarnings(as.numeric(data[[time_col]][i])))]
  }

  # Identify BLQ values. The flag stays in the dataset under a readable name:
  # the partial AUC notes and the bioequivalence table report how much of an
  # interval rests on values this rule set. A column of this name in the
  # uploaded file is replaced.
  data[[BLQ_FLAG_COLUMN]] <- !is.na(data[[conc_col]]) & data[[conc_col]] < lloq
  predose <- !is.na(data[[time_col]]) & suppressWarnings(as.numeric(data[[time_col]])) <= 0
  
  if (rule == "rule2") {
    # All BLQ -> 0
    data[[conc_col]][data[[BLQ_FLAG_COLUMN]]] <- 0
    
  } else if (rule == "rule3") {
    # All BLQ -> NA
    data[[conc_col]][data[[BLQ_FLAG_COLUMN]]] <- NA
    
  } else if (rule == "rule4") {
    # All BLQ -> LLOQ/2, except before dosing: a BLQ pre-dose sample is 0, as
    # LLOQ/2 there would mean drug before the dose (and remove the lag time)
    data[[conc_col]][data[[BLQ_FLAG_COLUMN]]] <- lloq / 2
    data[[conc_col]][data[[BLQ_FLAG_COLUMN]] & predose] <- 0
    
  } else if (rule == "rule5") {
    # Pre-Cmax BLQ -> 0; post-Cmax BLQ -> NA
    for (s in unique(prof_key)) {
      idx <- profile_idx(s)
      sub <- data[idx, ]
      tmax_idx <- which.max(sub[[conc_col]])
      
      # Guard: which.max returns integer(0) when all concentrations are NA.
      # 1:integer(0) throws "argument of length 0" — skip this subject.
      if (length(tmax_idx) == 0) next
      
      pre_cmax  <- idx[1:tmax_idx]
      post_cmax <- if (tmax_idx < length(idx)) idx[(tmax_idx + 1):length(idx)] else integer(0)
      
      data[[conc_col]][intersect(pre_cmax,  which(data[[BLQ_FLAG_COLUMN]]))] <- 0
      data[[conc_col]][intersect(post_cmax, which(data[[BLQ_FLAG_COLUMN]]))] <- NA
    }
    
  } else if (rule == "rule6") {
    # Rule 6: BLQ after dosing and before the first quantifiable value ->
    # LLOQ/2; all other BLQ (including a pre-dose sample) -> 0
    for (s in unique(prof_key)) {
      idx <- profile_idx(s)
      sub <- data[idx, ]
      quant_idx <- which(!sub[[BLQ_FLAG_COLUMN]] & !is.na(sub[[conc_col]]))
      
      if (length(quant_idx) == 0) {
        data[[conc_col]][idx[data[[BLQ_FLAG_COLUMN]][idx]]] <- lloq / 2
        next
      }
      
      first_quant <- min(quant_idx)
      
      # Before first quantifiable: set BLQ to LLOQ/2
      if (first_quant > 1) {
        pre <- idx[1:(first_quant - 1)]
        data[[conc_col]][intersect(pre, which(data[[BLQ_FLAG_COLUMN]]))] <- lloq / 2
      }
      # All other BLQ (during and after quantifiable phase): set to 0
      from_quant <- idx[first_quant:length(idx)]
      data[[conc_col]][intersect(from_quant, which(data[[BLQ_FLAG_COLUMN]]))] <- 0
    }
    data[[conc_col]][data[[BLQ_FLAG_COLUMN]] & predose] <- 0
    
  } else {
    # Rule 1 (default): pre-first-quantifiable -> 0, post-last-quantifiable -> NA
    for (s in unique(prof_key)) {
      idx <- profile_idx(s)
      sub <- data[idx, ]
      quant_idx <- which(!sub[[BLQ_FLAG_COLUMN]] & !is.na(sub[[conc_col]]))
      
      if (length(quant_idx) == 0) {
        data[[conc_col]][idx[data[[BLQ_FLAG_COLUMN]][idx]]] <- NA
        next
      }
      
      first_quant <- min(quant_idx)
      last_quant  <- max(quant_idx)
      
      # Before first quantifiable: set BLQ to 0
      if (first_quant > 1) {
        pre <- idx[1:(first_quant - 1)]
        data[[conc_col]][intersect(pre, which(data[[BLQ_FLAG_COLUMN]]))] <- 0
      }
      # After last quantifiable: set BLQ to NA
      if (last_quant < length(idx)) {
        post <- idx[(last_quant + 1):length(idx)]
        data[[conc_col]][intersect(post, which(data[[BLQ_FLAG_COLUMN]]))] <- NA
      }
      # Between: BLQ to 0 (common convention)
      between <- idx[first_quant:last_quant]
      data[[conc_col]][intersect(between, which(data[[BLQ_FLAG_COLUMN]]))] <- 0
    }
  }
  
  data
}

#' Terminal-phase points for manual half-life overrides, as tblNCA UsePoints
#'
#' @param lz_overrides list of overrides, each with subject, optional
#'   treatment and period, and time_used (the chosen sampling times)
#' @return NULL when there are no applicable overrides, otherwise a list with
#'   one element per profile (NULL = automatic slope), aligned with final_keys.
#'   Indices refer to the profile's points after NA removal, as in sNCA().
override_use_points <- function(data, col_map, nca_key, final_keys, lz_overrides) {
  if (is.null(lz_overrides) || length(lz_overrides) == 0) return(NULL)
  pk <- profile_key(data, col_map)
  out <- vector("list", length(final_keys))
  any_set <- FALSE
  for (ov in lz_overrides) {
    if (is.null(ov$subject) || length(ov$time_used) < 2) next
    match_row <- pk$parts$Subject == as.character(ov$subject)
    if (!is.null(ov$treatment) && "Treatment" %in% pk$cols)
      match_row <- match_row & pk$parts$Treatment == as.character(ov$treatment)
    if (!is.null(ov$period) && "Period" %in% pk$cols)
      match_row <- match_row & pk$parts$Period == as.character(ov$period)
    k <- unique(data[[nca_key]][match_row])
    if (length(k) != 1) next
    i <- match(k, final_keys)
    if (is.na(i)) next
    rows <- data[data[[nca_key]] == k, , drop = FALSE]
    x <- rows[[col_map$time]]; y <- rows[[col_map$conc]]
    keep <- !(is.na(x) | is.na(y))
    x <- x[keep]; y <- y[keep]
    tu <- as.numeric(unlist(ov$time_used))
    chosen <- which(y > 0 & vapply(x, function(t) any(abs(t - tu) <= 1e-9 * max(1, abs(t))), logical(1)))
    if (length(chosen) >= 2) { out[[i]] <- chosen; any_set <- TRUE }
  }
  if (any_set) out else NULL
}

#' Terminal-phase fit without values set by a BLQ rule
#'
#' ICH M13A (2.2.2.2) leaves values below the LLOQ out of kel and t1/2. A BLQ
#' rule that sets them to LLOQ/2 (Rules 4 and 6) makes them positive, so
#' NonCompart's automatic search could fit a flat imputed tail. When its fit
#' would use such a value, the search is repeated on the measured values only.
#' @param time,conc One profile, in time order; is_blq flag per sample
#' @param adm NonCompart adm value
#' @return NULL when NonCompart's own fit uses no imputed value (nothing to
#'   change); otherwise list(points = indices after NA removal, as sNCA's
#'   UsePoints, or integer(0) when the measured values allow no fit; fit =
#'   NonCompart::BestSlope result or NULL)
blq_free_slope <- function(time, conc, is_blq, adm) {
  if (is.null(is_blq)) return(NULL)
  ok <- !is.na(time) & !is.na(conc)
  x <- time[ok]; y <- conc[ok]; f <- is_blq[ok] %in% TRUE
  if (!any(f & y > 0)) return(NULL)
  upto <- seq_len(max(which(y > 0)))
  pos <- upto[y[upto] > 0]
  auto <- tryCatch(NonCompart::BestSlope(x[pos], y[pos], adm = adm), error = function(e) NULL)
  if (is.null(auto) || !any(f[pos][attr(auto, "UsedPoints")])) return(NULL)
  cand <- pos[!f[pos]]
  fit <- if (length(cand) >= 3) tryCatch(NonCompart::BestSlope(x[cand], y[cand], adm = adm),
                                         error = function(e) NULL)
  if (is.null(fit) || is.na(fit["LAMZ"]) || fit["LAMZ"] <= 0 || length(attr(fit, "UsedPoints")) < 2)
    return(list(points = integer(0), fit = NULL))
  list(points = cand[attr(fit, "UsedPoints")], fit = fit)
}

#' Parameters that depend on the terminal slope (lambda-z)
#'
#' Set to missing when the automatic fit is below the analyst's adjusted R2
#' threshold. At steady state NonCompart computes CL from AUClast (AUC over
#' the dosing interval), so CL is kept there.
LAMZ_DEPENDENT <- c("LAMZ", "LAMZHL", "b0", "CLSTP", "AUCIFO", "AUCIFOD", "AUCIFP", "AUCIFPD",
                    "AUCPEO", "AUCPEP", "AUCPBEO", "AUCPBEP", "AUMCIFO", "AUMCIFP", "AUMCPEO",
                    "AUMCPEP", "MRTEVIFO", "MRTEVIFP", "MRTIVIFO", "MRTIVIFP", "VZFO", "VZFP",
                    "VZO", "VZP", "CLFO", "CLFP", "CLO", "CLP", "VSSO", "VSSP")
lamz_dependent_cols <- function(names_in, steady_state = FALSE) {
  cols <- intersect(LAMZ_DEPENDENT, names_in)
  if (isTRUE(steady_state)) cols <- setdiff(cols, c("CLFO", "CLO"))
  cols
}

#' Is an automatic terminal-phase fit below the adjusted R2 threshold?
#' @return logical per value; NA R2ADJ (no fit) is not flagged
below_r2_threshold <- function(r2adj, threshold) {
  thr <- suppressWarnings(as.numeric(threshold))
  if (length(thr) != 1 || is.na(thr) || thr <= 0) return(rep(FALSE, length(r2adj)))
  v <- suppressWarnings(as.numeric(r2adj))
  !is.na(v) & v < thr
}

#' AUC and AUMC, linear-up/log-down, linear on any segment that ends at zero
#'
#' Same rule as NonCompart's LogAUC(), which its IntAUC() uses.
log_down_auc <- function(x, y) {
  auc <- 0; aumc <- 0
  for (i in seq_along(x)[-1]) {
    dt <- x[i] - x[i - 1]
    if (y[i] < y[i - 1] && y[i] > 0) {
      k <- (log(y[i - 1]) - log(y[i])) / dt
      auc <- auc + (y[i - 1] - y[i]) / k
      aumc <- aumc + (x[i - 1] * y[i - 1] - x[i] * y[i]) / k + (y[i - 1] - y[i]) / k / k
    } else {
      auc <- auc + dt * (y[i] + y[i - 1]) / 2
      aumc <- aumc + dt * (y[i] * x[i] + y[i - 1] * x[i - 1]) / 2
    }
  }
  c(AUC = auc, AUMC = aumc)
}

#' Correct AUClast, AUMClast, AUCall and what depends on them under log-down
#'
#' NonCompart's sNCA() computes these with AUC(), which under log-down gives
#' a falling segment that ends at zero no area at all (log(0) = -Inf): a BLQ
#' value set to 0 between measurable samples then removes the area of the
#' preceding fall. Partial AUCs are not affected (IntAUC() goes linear there).
#' This rebuilds the affected parameters with sNCA()'s own formulas; clearance
#' and volumes are rescaled by the AUC ratio, which keeps NonCompart's unit
#' conversion. Profiles without such a segment are returned unchanged.
#' @param r Named NCA result (sNCA output, or one row of tblNCA as a list)
#' @param adm,down NonCompart's adm and down values; dur infusion duration
fix_log_down_zeros <- function(r, time, conc, adm, down, dur = 0, ss = FALSE) {
  if (toupper(down) != "LOG") return(r)
  ok <- !is.na(time) & !is.na(conc)
  x <- time[ok]; y <- conc[ok]
  if (length(x) < 2 || any(y < 0) || length(unique(y)) == 1 || !any(y > 0)) return(r)
  get <- function(n) if (n %in% names(r)) suppressWarnings(as.numeric(r[[n]])) else NA_real_
  put <- function(n, v) if (n %in% names(r)) r[[n]] <<- v
  last <- max(which(y > 0))
  x0 <- x[seq_len(last)]; y0 <- y[seq_len(last)]
  bolus <- toupper(adm) == "BOLUS"
  if (bolus) {
    c0 <- get("C0"); x2 <- c(0, x); y2 <- c(c0, y); x3 <- c(0, x0); y3 <- c(c0, y0)
  } else if (!any(x == 0)) {
    x2 <- c(0, x); y2 <- c(0, y); x3 <- c(0, x0); y3 <- c(0, y0)
  } else {
    x2 <- x; y2 <- y; x3 <- x0; y3 <- y0
  }
  falls_to_zero <- function(v) length(v) > 1 && any(v[-1] == 0 & v[-length(v)] > 0)
  if (!falls_to_zero(y2)) return(r)

  old <- list(lst = get("AUCLST"), ifo = get("AUCIFO"), ifp = get("AUCIFP"))
  a3 <- log_down_auc(x3, y3)
  lst <- unname(a3["AUC"]); aumc <- unname(a3["AUMC"])
  clst <- get("CLST"); clstp <- get("CLSTP"); lamz <- get("LAMZ"); tlst <- get("TLST")
  ifo <- lst + clst / lamz; ifp <- lst + clstp / lamz
  aumc_ifo <- aumc + clst * tlst / lamz + clst / lamz / lamz
  aumc_ifp <- aumc + clstp * tlst / lamz + clstp / lamz / lamz
  ratio <- function(new, o) if (is.finite(new) && is.finite(o) && new != 0) o / new else NA_real_
  put("AUCLST", lst); put("AUMCLST", aumc)
  put("AUCALL", unname(log_down_auc(x2, y2)["AUC"]))
  put("AUCIFO", ifo); put("AUCIFP", ifp)
  put("AUCPEO", (1 - lst / ifo) * 100); put("AUCPEP", (1 - lst / ifp) * 100)
  put("AUMCIFO", aumc_ifo); put("AUMCIFP", aumc_ifp)
  put("AUMCPEO", (1 - aumc / aumc_ifo) * 100); put("AUMCPEP", (1 - aumc / aumc_ifp) * 100)
  put("AUCIFOD", get("AUCIFOD") / ratio(ifo, old$ifo))
  put("AUCIFPD", get("AUCIFPD") / ratio(ifp, old$ifp))
  if (bolus) {
    first <- unname(log_down_auc(x3[1:2], y3[1:2])["AUC"])
    put("AUCPBEO", first / ifo * 100); put("AUCPBEP", first / ifp * 100)
  }
  # CL = dose / AUC and V = dose / (lambda-z x AUC): rescale by the AUC they use
  s_o <- if (ss) ratio(lst, old$lst) else ratio(ifo, old$ifo)
  s_p <- if (ss) NA_real_ else ratio(ifp, old$ifp)
  for (n in c("CLFO", "VZFO", "CLO", "VZO")) put(n, get(n) * s_o)
  for (n in c("CLFP", "VZFP", "CLP", "VZP")) put(n, get(n) * s_p)
  mrt_lst <- aumc / lst
  if (toupper(adm) == "EXTRAVASCULAR") {
    put("MRTEVLST", mrt_lst)
    put("MRTEVIFO", if (ss) NA_real_ else aumc_ifo / ifo)
    put("MRTEVIFP", if (ss) NA_real_ else aumc_ifp / ifp)
  } else {
    old_mrt_o <- get(if (ss) "MRTIVLST" else "MRTIVIFO"); old_mrt_p <- get("MRTIVIFP")
    put("MRTIVLST", mrt_lst - dur / 2)
    put("MRTIVIFO", if (ss) NA_real_ else aumc_ifo / ifo - dur / 2)
    put("MRTIVIFP", if (ss) NA_real_ else aumc_ifp / ifp - dur / 2)
    new_mrt_o <- get(if (ss) "MRTIVLST" else "MRTIVIFO")
    put("VSSO", get("VSSO") * s_o * new_mrt_o / old_mrt_o)
    if (!ss) put("VSSP", get("VSSP") * s_p * get("MRTIVIFP") / old_mrt_p)
  }
  r
}

#' Predicted Clast at Tlast for a fit on chosen points
#'
#' With UsePoints, NonCompart's sNCA() predicts Clast at the last sample time
#' (a logical vector is recycled over all times), not at Tlast, so with a zero
#' after Tlast every "...P" parameter is off. Its automatic fit predicts at
#' Tlast. This recomputes CLSTP = exp(b0 - lambda-z x Tlast) and the
#' parameters that use it, rescaling clearance and volumes by the AUC ratio.
fix_predicted_clast <- function(r, adm, dur = 0) {
  get <- function(n) if (n %in% names(r)) suppressWarnings(as.numeric(r[[n]])) else NA_real_
  put <- function(n, v) if (n %in% names(r)) r[[n]] <<- v
  lamz <- get("LAMZ"); b0 <- get("b0"); tlst <- get("TLST")
  new <- exp(b0 - lamz * tlst)
  if (!is.finite(new) || isTRUE(abs(new - get("CLSTP")) <= 1e-12 * max(1, abs(new)))) return(r)
  old_ifp <- get("AUCIFP"); lst <- get("AUCLST"); aumc <- get("AUMCLST")
  ifp <- lst + new / lamz
  aumc_ifp <- aumc + new * tlst / lamz + new / lamz / lamz
  ratio <- if (is.finite(ifp) && is.finite(old_ifp) && ifp != 0) old_ifp / ifp else NA_real_
  put("CLSTP", new); put("AUCIFP", ifp); put("AUCPEP", (1 - lst / ifp) * 100)
  put("AUMCIFP", aumc_ifp); put("AUMCPEP", (1 - aumc / aumc_ifp) * 100)
  put("AUCIFPD", get("AUCIFPD") / ratio); put("AUCPBEP", get("AUCPBEP") * ratio)
  for (n in c("CLFP", "VZFP", "CLP", "VZP")) put(n, get(n) * ratio)
  old_mrt <- get("MRTIVIFP")
  put("MRTEVIFP", if (is.na(get("MRTEVIFP"))) NA_real_ else aumc_ifp / ifp)
  put("MRTIVIFP", if (is.na(old_mrt)) NA_real_ else aumc_ifp / ifp - dur / 2)
  if (!is.na(old_mrt)) put("VSSP", get("VSSP") * ratio * get("MRTIVIFP") / old_mrt)
  r
}

#' Lag time: the last sample before the first measurable concentration
#'
#' NonCompart takes the last zero before Tlast, so a BLQ value set to 0
#' between measurable samples became the lag time. Not used for IV bolus.
fix_tlag <- function(r, time, conc, adm) {
  if (toupper(adm) == "BOLUS" || !"TLAG" %in% names(r)) return(r)
  ok <- !is.na(time) & !is.na(conc)
  x <- time[ok]; y <- conc[ok]
  first <- which(y > 0)[1]
  if (is.na(first)) return(r)
  r[["TLAG"]] <- if (first == 1) 0 else x[first - 1]
  r
}

#' NCA for one profile given as vectors (single-subject analysis)
#'
#' Same NonCompart call and options as run_nca(), so a profile analysed on
#' its own gives the same result as in the batch table.
#' @param time_used Optional sampling times for a manual half-life override
run_single_nca <- function(time, conc, settings, time_used = NULL, is_blq = NULL) {
  adm <- switch(settings$admin_route, "extravascular" = "Extravascular",
                "iv_bolus" = "Bolus", "iv_infusion" = "Infusion", "Extravascular")
  down <- switch(settings$trap_method, "linear" = "Linear", "log" = "Log", "Linear")
  t_num <- suppressWarnings(as.numeric(as.character(time)))
  c_num <- suppressWarnings(as.numeric(as.character(conc)))
  ord <- order(t_num); t_num <- t_num[ord]; c_num <- c_num[ord]
  if (!is.null(is_blq)) is_blq <- is_blq[ord]
  # The steady-state trough (Cmin) comes from the whole profile, including a
  # pre-dose sample that an IV bolus analysis sets aside below
  t_all <- t_num; c_all <- c_num
  if (adm == "Bolus") {
    post <- is.na(t_num) | t_num > 0
    t_num <- t_num[post]; c_num <- c_num[post]
    if (!is.null(is_blq)) is_blq <- is_blq[post]
  }
  use <- NULL
  if (length(time_used) >= 2) {
    keep <- !(is.na(t_num) | is.na(c_num))
    xf <- t_num[keep]; yf <- c_num[keep]
    tu <- as.numeric(unlist(time_used))
    use <- which(yf > 0 & vapply(xf, function(t) any(abs(t - tu) <= 1e-9 * max(1, abs(t))), logical(1)))
    if (length(use) < 2) use <- NULL
  }
  # Without a manual selection, keep values set by a BLQ rule out of the fit
  blq_fit <- if (is.null(use)) blq_free_slope(t_num, c_num, is_blq, adm)
  no_fit <- !is.null(blq_fit) && length(blq_fit$points) == 0
  nc_points <- if (!is.null(use)) use else if (!is.null(blq_fit) && !no_fit) blq_fit$points
  num0 <- function(v) { v <- suppressWarnings(as.numeric(v)); if (length(v) == 0 || is.na(v)) 0 else v }
  ss <- isTRUE(settings$is_steady_state)
  tau <- steady_state_tau(settings)
  if (ss && is.na(tau)) {
    warning("Steady state: enter the dosing interval (tau) in the analysis settings.")
    return(NULL)
  }
  pauc <- partial_auc_spec(settings$partial_aucs)
  pauc_err <- validate_partial_aucs(pauc, ss, tau)
  if (!is.null(pauc_err)) {
    warning(pauc_err)
    return(NULL)
  }
  iauc <- rbind(partial_auc_iauc(pauc),
                if (ss) data.frame(Name = "AUCTAU", Start = 0, End = tau, stringsAsFactors = FALSE))
  r <- NonCompart::sNCA(t_num, c_num, dose = num0(settings$dose), adm = adm, down = down,
                   dur = if (adm == "Infusion") num0(settings$infusion_duration) else 0,
                   doseUnit = settings$dose_unit, timeUnit = settings$time_unit,
                   concUnit = settings$conc_unit, SS = isTRUE(settings$is_steady_state),
                   # R2ADJ = 0 avoids NonCompart's interactive slope picker (see run_nca)
                   R2ADJ = 0, MW = num0(settings$mw), UsePoints = nc_points,
                   iAUC = if (is.null(iauc)) "" else iauc)
  r <- fix_log_down_zeros(r, t_num, c_num, adm, down,
                          dur = if (adm == "Infusion") num0(settings$infusion_duration) else 0, ss = ss)
  if (!is.null(nc_points))
    r <- fix_predicted_clast(r, adm, dur = if (adm == "Infusion") num0(settings$infusion_duration) else 0)
  r <- fix_tlag(r, t_num, c_num, adm)
  # The analyst's R2 threshold applies to the automatic fit, not to points
  # chosen by hand
  low <- is.null(use) && (no_fit || below_r2_threshold(r["R2ADJ"], settings$r2adj_threshold))
  if (low) r[lamz_dependent_cols(names(r), settings$is_steady_state)] <- NA
  if (ss) {
    r <- steady_state_parameters(r, t_all, c_all, tau, lamz_rejected = low)
    note <- steady_state_predose_note(list(t_all), list(c_all), "this profile", adm)
    if (!is.null(note)) warning(note)
  }
  if (!is.null(pauc)) {
    p <- partial_auc_profile(r, pauc, t_num, c_num, is_blq, partial_auc_blq_fraction(settings))
    r <- r[!grepl("^\\.PAUC", names(r))]
    r[names(p$values)] <- p$values
    for (msg in partial_auc_notes(pauc, list(p), "this profile", settings$trap_method,
                                  partial_auc_blq_fraction(settings))) warning(msg)
  }
  r
}

#' Steady-state warning for profiles without a measured pre-dose sample
#'
#' NonCompart then puts a concentration of 0 at time 0, which at steady state
#' underestimates AUCtau (by about Cmin x t1 / 2).
#' @param labels profile label per element of the lists
#' @return message, or NULL when every profile has a value at t <= 0
steady_state_predose_note <- function(times, concs, labels, adm) {
  if (toupper(adm) == "BOLUS") return(NULL)
  miss <- labels[vapply(seq_along(times), function(i)
    !any(!is.na(times[[i]]) & times[[i]] <= 0 & !is.na(concs[[i]])), logical(1))]
  if (length(miss) == 0) return(NULL)
  paste0("Steady state: ", length(miss), " profile(s) have no measured pre-dose sample at time 0 (",
         paste(head(miss, 5), collapse = ", "), if (length(miss) > 5) ", ..." else "",
         "). A concentration of 0 was assumed there, so AUC\u03C4 is too low by about Cmin x the time ",
         "of the first sample / 2. Add the pre-dose (trough) sample at time 0.")
}

#' The dosing interval entered for a steady-state analysis, or NA
steady_state_tau <- function(settings) {
  tau <- suppressWarnings(as.numeric(settings$tau))
  if (length(tau) != 1 || is.na(tau) || tau <= 0) NA_real_ else tau
}

#' Steady-state parameters for one profile, from the entered dosing interval
#'
#' AUCTAU is NonCompart's partial AUC from 0 to tau (interpolated within the
#' samples, extrapolated with lambda-z beyond the last one). It is missing when
#' that extrapolation would need a lambda-z fit rejected by the R2 rule.
#' CAVG = AUCTAU / tau; CMIN_SS is the lowest observed concentration within
#' 0-tau; fluctuation = (Cmax - Cmin) / Cavg x 100; swing = (Cmax - Cmin) / Cmin.
#' NonCompart computes CL/F and Vz/F at steady state from AUClast; they are
#' rescaled to AUCTAU, so they stay correct when sampling does not end at tau.
#' @param r Named NCA result (sNCA output, or one row of tblNCA as a list)
steady_state_parameters <- function(r, time, conc, tau, lamz_rejected = FALSE) {
  get <- function(n) if (n %in% names(r)) suppressWarnings(as.numeric(r[[n]])) else NA_real_
  auctau <- get("AUCTAU"); auclst <- get("AUCLST"); tlst <- get("TLST"); cmax <- get("CMAX")
  if (isTRUE(lamz_rejected) && !is.na(tlst) && tau > tlst + 1e-9) auctau <- NA_real_
  ok <- !is.na(time) & !is.na(conc) & time <= tau + 1e-9
  cmin  <- if (any(ok)) min(conc[ok]) else NA_real_
  cavg  <- if (!is.na(auctau)) auctau / tau else NA_real_
  fluct <- if (!is.na(cavg) && cavg > 0 && !is.na(cmin)) (cmax - cmin) / cavg * 100 else NA_real_
  swing <- if (!is.na(cmin) && cmin > 0) (cmax - cmin) / cmin else NA_real_
  scale <- if (!is.na(auctau) && auctau > 0 && !is.na(auclst)) auclst / auctau else NA_real_
  for (n in intersect(c("CLFO", "CLO", "VZFO", "VZO"), names(r))) r[[n]] <- get(n) * scale
  # NonCompart's Vss (AUMC/AUC over the samples x CL) and MRT to the last
  # sample are single-dose quantities; at steady state they are left empty
  for (n in intersect(c("VSSO", "VSSP", "MRTIVLST", "MRTEVLST"), names(r))) r[[n]] <- NA_real_
  r[["AUCTAU"]] <- auctau
  r[["TAU"]] <- tau
  r[["CAVG"]] <- cavg
  r[["CMIN_SS"]] <- cmin
  r[["FLUCTP"]] <- fluct
  r[["SWING"]] <- swing
  r
}

# ----------------------------------------------------------------------------
# Partial AUCs
# ----------------------------------------------------------------------------
# An interval has a start, an end (a time, or "t" = the last measurable
# concentration of each profile), whether to also report the observed Cmax and
# Tmax within it, and a role in bioequivalence (pivotal or supportive).
# Intervals come from the protocol; the app never chooses them.

#' Partial AUC intervals in one shape
#'
#' @param x NULL, a data.frame, or a list of records as read back from
#'   analysis_settings.json, with start, end and optionally cmax and role
#' @return NULL when there are no intervals, else data.frame(start numeric,
#'   end character: a number or "t", cmax logical, role character)
partial_auc_spec <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (!is.data.frame(x)) {
    field <- function(rec, f, default) if (is.null(rec[[f]]) || length(rec[[f]]) == 0) default else rec[[f]]
    x <- data.frame(start = vapply(x, function(r) as.character(field(r, "start", NA)), character(1)),
                    end   = vapply(x, function(r) as.character(field(r, "end", NA)), character(1)),
                    cmax  = vapply(x, function(r) isTRUE(as.logical(field(r, "cmax", FALSE))), logical(1)),
                    role  = vapply(x, function(r) as.character(field(r, "role", "pivotal")), character(1)),
                    stringsAsFactors = FALSE)
  }
  if (nrow(x) == 0) return(NULL)
  end_chr <- trimws(as.character(x$end))
  to_t <- !is.na(end_chr) & tolower(end_chr) == "t"
  end_num <- suppressWarnings(as.numeric(end_chr))
  data.frame(start = suppressWarnings(as.numeric(as.character(x$start))),
             end   = ifelse(to_t, "t", ifelse(is.na(end_num), NA_character_, .pauc_num(end_num))),
             cmax  = if (is.null(x$cmax)) FALSE else as.logical(x$cmax) %in% TRUE,
             role  = if (is.null(x$role)) "pivotal" else ifelse(is.na(x$role), "pivotal", as.character(x$role)),
             stringsAsFactors = FALSE)
}

#' Share of BLQ-derived samples above which an interval is flagged
PARTIAL_AUC_BLQ_FRACTION <- 0.5

#' The share in force for an analysis; recorded so a rerun keeps it
partial_auc_blq_fraction <- function(settings) {
  v <- suppressWarnings(as.numeric(settings$partial_auc_blq_fraction))
  if (length(v) != 1 || is.na(v) || v <= 0 || v >= 1) PARTIAL_AUC_BLQ_FRACTION else v
}

#' A number as it appears in a partial AUC column name
.pauc_num <- function(v) vapply(v, function(z) format(z, scientific = FALSE, trim = TRUE, digits = 15), character(1),
                                USE.NAMES = FALSE)

#' Check partial AUC intervals before the analysis
#' @return NULL when valid, otherwise a message for the user
validate_partial_aucs <- function(spec, is_steady_state = FALSE, tau = NA) {
  if (is.null(spec)) return(NULL)
  for (i in seq_len(nrow(spec))) {
    lab <- paste0("Partial AUC interval ", i, ": ")
    s <- spec$start[i]; e <- spec$end[i]
    if (is.na(s) || !is.finite(s) || s < 0) return(paste0(lab, "the start must be a time of 0 or later."))
    if (is.na(e)) return(paste0(lab, "the end must be a time, or 't' for the last measurable concentration."))
    if (e != "t" && as.numeric(e) <= s) return(paste0(lab, "the end must be later than the start."))
    if (!spec$role[i] %in% c("pivotal", "supportive"))
      return(paste0(lab, "the role must be pivotal or supportive."))
    if (isTRUE(is_steady_state)) {
      if (e == "t")
        return(paste0(lab, "at steady state the interval must lie within 0 to \u03C4; enter an end time ",
                      "instead of the last measurable concentration."))
      if (!is.na(tau) && as.numeric(e) > tau + 1e-9 * max(1, tau))
        return(paste0(lab, "at steady state the interval must lie within 0 to \u03C4 (", tau, ")."))
    }
  }
  if (anyDuplicated(paste(spec$start, spec$end)))
    return("The same partial AUC interval is entered twice.")
  NULL
}

#' Result column names for each interval, e.g. AUC_0_0.5, CMAX_168_t, TMAX_168_t
partial_auc_names <- function(spec) {
  iv <- paste0(.pauc_num(spec$start), "_", spec$end)
  data.frame(auc = paste0("AUC_", iv), cmax = paste0("CMAX_", iv), tmax = paste0("TMAX_", iv),
             stringsAsFactors = FALSE)
}

#' Pattern of partial AUC, Cmax-in-interval and Tmax-in-interval column names
PARTIAL_AUC_PATTERN <- "^(AUC|CMAX|TMAX)_([0-9.]+)_([0-9.]+|t)$"

#' Those columns among a set of names (used by the app, the exports and the
#' dose-normalisation helper, so it lives with the pattern, not in a module)
partial_auc_cols <- function(x) grep(PARTIAL_AUC_PATTERN, x, value = TRUE)

#' NonCompart iAUC rows for the intervals
#'
#' A fixed end is NonCompart's partial AUC from start to end. For an end at the
#' last measurable concentration, NonCompart gives AUC from 0 to start, and the
#' interval is AUClast minus that.
partial_auc_iauc <- function(spec) {
  if (is.null(spec)) return(NULL)
  need <- spec$end != "t" | spec$start > 0
  i <- which(need)
  if (length(i) == 0) return(NULL)
  data.frame(Name = paste0(".PAUC", i),
             Start = ifelse(spec$end[i] == "t", 0, spec$start[i]),
             End = ifelse(spec$end[i] == "t", spec$start[i], suppressWarnings(as.numeric(spec$end[i]))),
             stringsAsFactors = FALSE)
}

#' Partial AUCs of one profile
#'
#' A partial AUC is reported only when the whole interval lies within the
#' observed profile, from 0 to the last measurable concentration (Tlast). It is
#' never extrapolated with lambda-z. A cutoff between samples is interpolated
#' by NonCompart with the analysis's trapezoidal method. Cmax and Tmax in an
#' interval are the highest observed concentration from start to end and its
#' first time, without interpolation.
#'
#' @param r Named NCA result of the profile (sNCA output, or one row of tblNCA
#'   as a list) including the .PAUC columns from partial_auc_iauc()
#' @param time,conc The profile's samples
#' @param is_blq Optional flag per sample: value set by the BLQ rule
#' @param blq_fraction An interval is flagged when more than this fraction of
#'   the samples in it were set by the BLQ rule
#' @return list(values = named numeric, tlast = the profile's last measurable
#'   time, and one logical per interval for: beyond (not reported: past Tlast),
#'   zero, offgrid (a cutoff is not a sampling time), blq (more than
#'   blq_fraction of the samples used were set by the BLQ rule), sparse (fewer
#'   than three measurable concentrations in the interval))
partial_auc_profile <- function(r, spec, time, conc, is_blq = NULL,
                                blq_fraction = PARTIAL_AUC_BLQ_FRACTION) {
  get <- function(n) if (n %in% names(r)) suppressWarnings(as.numeric(r[[n]])) else NA_real_
  tl <- get("TLST"); auclst <- get("AUCLST")
  ok <- !is.na(time) & !is.na(conc)
  x <- time[ok]; y <- conc[ok]
  blq <- if (is.null(is_blq)) NULL else (is_blq[ok] %in% TRUE)
  tol <- function(v) 1e-9 * max(1, abs(v))
  nm <- partial_auc_names(spec)
  n <- nrow(spec)
  values <- c()
  flags <- list(beyond = logical(n), zero = logical(n), offgrid = logical(n),
                blq = logical(n), sparse = logical(n))
  for (i in seq_len(n)) {
    s <- spec$start[i]
    e <- if (spec$end[i] == "t") tl else as.numeric(spec$end[i])
    covered <- !is.na(tl) && !is.na(e) && s <= tl + tol(tl) && e <= tl + tol(tl)
    auc <- NA_real_
    if (covered) {
      auc <- if (spec$end[i] != "t") get(paste0(".PAUC", i)) else
             if (s == 0) auclst else if (abs(s - tl) <= tol(tl)) 0 else auclst - get(paste0(".PAUC", i))
      if (!is.na(auc) && abs(auc) <= 1e-12 * max(1, abs(auclst))) auc <- 0
    }
    values[nm$auc[i]] <- auc
    in_win <- covered & x >= s - tol(s) & x <= e + tol(e)
    if (isTRUE(spec$cmax[i])) {
      if (any(in_win)) {
        j <- which(in_win)[which.max(y[in_win])]
        values[nm$cmax[i]] <- y[j]; values[nm$tmax[i]] <- x[j]
      } else {
        values[nm$cmax[i]] <- NA_real_; values[nm$tmax[i]] <- NA_real_
      }
    }
    flags$beyond[i] <- !covered
    flags$zero[i] <- isTRUE(auc == 0)
    on_grid <- function(v) any(abs(x - v) <= tol(v))
    flags$offgrid[i] <- covered && ((s > 0 && !on_grid(s)) || (spec$end[i] != "t" && !on_grid(e)))
    flags$blq[i] <- covered && !is.null(blq) && any(in_win) && sum(blq[in_win]) > sum(in_win) * blq_fraction
    measurable <- in_win & y > 0 & (if (is.null(blq)) TRUE else !blq)
    flags$sparse[i] <- covered && sum(measurable) < 3
  }
  c(list(values = values, tlast = tl), flags)
}

#' Time ranges of partial AUC intervals, for shading a mean profile figure
#'
#' An end at the last measurable concentration differs per profile; on a mean
#' figure it is drawn to the last time shown. The height covers the plotted
#' values y (interactive plots cannot draw infinite rectangles).
partial_auc_shading <- function(spec, t_max, y, log = FALSE) {
  spec <- partial_auc_spec(spec)
  if (is.null(spec)) return(NULL)
  y <- y[is.finite(y) & (!log | y > 0)]
  data.frame(xmin = spec$start,
             xmax = ifelse(spec$end == "t", t_max, suppressWarnings(as.numeric(spec$end))),
             ymin = if (log) min(y) else min(0, y), ymax = max(y))
}

#' User-facing notes about partial AUCs across profiles
#' @param flags list per profile of partial_auc_profile() results
#' @param labels Profile label per element of flags
#' @return character vector of messages (empty when nothing to report)
partial_auc_notes <- function(spec, flags, labels, trap_method = "linear",
                              blq_fraction = PARTIAL_AUC_BLQ_FRACTION) {
  if (is.null(spec) || length(flags) == 0) return(character(0))
  single <- length(flags) == 1
  # Each profile ends at its own last measurable concentration, which is what
  # surprises users: a later sample that came back below the limit of
  # quantification does not extend the profile.
  last_meas <- vapply(flags, function(p) {
    if (is.null(p$tlast) || is.na(p$tlast)) "no measurable concentration at all" else
      paste0("no measurable concentration after ", .pauc_num(p$tlast))
  }, character(1))
  who <- function(f, i, detail = NULL, tail = "") {
    h <- which(vapply(flags, function(p) isTRUE(p[[f]][i]), logical(1)))
    if (length(h) == 0) return(NULL)
    txt <- if (is.null(detail)) labels[h] else paste0(labels[h], ": ", detail[h])
    if (single) return(paste0("this profile", tail, if (is.null(detail)) "" else paste0(": ", detail[h])))
    # The denominator keeps a note that applies to most profiles readable as a
    # property of the interval, not as a list of exceptions
    paste0(length(h), " of ", length(flags), " profiles", tail, ": ", paste(head(txt, 5), collapse = "; "),
           if (length(h) > 5) paste0(" and ", length(h) - 5, " more") else "")
  }
  lab <- paste0("Partial AUC ", .pauc_num(spec$start), "\u2013", spec$end)
  interp <- if (identical(trap_method, "log")) "linearly while concentrations rise and log-linearly while they fall" else "linearly"
  share <- if (isTRUE(all.equal(blq_fraction, 0.5))) "half" else paste0(round(blq_fraction * 100), "%")
  out <- character(0)
  for (i in seq_len(nrow(spec))) {
    w <- who("beyond", i, detail = last_meas)
    if (!is.null(w)) out <- c(out, paste0(lab[i], " is not reported for ", w,
      ". Partial AUCs are not extrapolated, and the interval reaches past that profile's last ",
      "measurable concentration. A later sample that came back below the limit of quantification ",
      "does not extend the profile."))
    w <- who("offgrid", i)
    if (!is.null(w)) out <- c(out, paste0(lab[i], ": a cutoff is not a sampling time in ", w,
      ". The concentration at the cutoff was interpolated ", interp, "."))
    w <- who("zero", i)
    if (!is.null(w)) out <- c(out, paste0(lab[i], " is zero in ", w,
      ". A zero cannot be log-transformed, so bioequivalence reports no result for this interval. ",
      "The interval and the BLQ rule are protocol choices."))
    w <- who("blq", i)
    if (!is.null(w)) out <- c(out, paste0(lab[i], ": more than ", share,
      " of the samples in this window were set by the BLQ rule, in ", w, "."))
    w <- who("sparse", i, tail = ", so the value is imprecise")
    if (!is.null(w)) out <- c(out, paste0(lab[i], " rests on fewer than three measurable concentrations in ",
      w, "."))
  }
  out
}

#' Run NCA for all subjects using NonCompart::tblNCA
#'
#' Wrapper that handles column mapping, options, and returns clean output.
#'
#' @param data Processed PK data
#' @param col_map Column mapping
#' @param settings List of NCA settings
#' @return Data frame of NCA results
run_nca <- function(data, col_map, settings, lz_overrides = NULL) {
  
  # Steady state: AUC over the entered dosing interval
  ss <- isTRUE(settings$is_steady_state)
  tau <- steady_state_tau(settings)
  if (ss && is.na(tau)) {
    warning("Steady state: enter the dosing interval (tau) in the analysis settings.")
    return(NULL)
  }
  pauc <- partial_auc_spec(settings$partial_aucs)
  pauc_err <- validate_partial_aucs(pauc, ss, tau)
  if (!is.null(pauc_err)) {
    warning(pauc_err)
    return(NULL)
  }
  iAUC_df <- rbind(partial_auc_iauc(pauc),
                   if (ss) data.frame(Name = "AUCTAU", Start = 0, End = tau, stringsAsFactors = FALSE))
  if (is.null(iAUC_df)) iAUC_df <- ""
  
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
  
  # CRITICAL: in crossover and replicate studies each subject has several
  # profiles. NonCompart::tblNCA groups by `key`, so the key must identify one
  # profile: subject + treatment + period (profile_key()). Without Period a
  # replicate design's two administrations of a treatment are merged into one
  # interleaved profile.
  pk <- profile_key(data, col_map)
  use_composite_key <- length(pk$cols) > 1

  if (use_composite_key) {
    data$.nca_key <- pk$key
    key_parts <- unique(cbind(.nca_key = pk$key, pk$parts))
    nca_key <- ".nca_key"
  } else {
    nca_key <- col_map$subject
  }
  
  # Force numeric time/concentration BEFORE sorting and filtering. As of
  # NonCompart 0.8.0, sNCA() hard-stops with "Check input types!" on non-numeric
  # input, and a character time column would also sort lexicographically
  # ("10" before "2"), breaking tblNCA's monotonic-time requirement. Coercing
  # here makes the NCA robust to a stray character column regardless of upload
  # path, locale, or NonCompart version. Numeric input is unaffected.
  data[[col_map$time]] <- suppressWarnings(as.numeric(as.character(data[[col_map$time]])))
  data[[col_map$conc]] <- suppressWarnings(as.numeric(as.character(data[[col_map$conc]])))

  # Ensure data is sorted by key and time
  data <- data[order(data[[nca_key]], data[[col_map$time]]), ]

  # IV bolus: NonCompart back-extrapolates C0 only when the first sample is
  # after the dose. A sample at or before time 0 (the pre-dose sample, often
  # 0 or BLQ) would instead start the curve from that value, so it is set
  # aside for the NCA. The steady-state trough still uses the whole profile.
  data_all <- data
  if (adm == "Bolus") data <- data[is.na(data[[col_map$time]]) | data[[col_map$time]] > 0, , drop = FALSE]
  
  # Degenerate profile filter: remove profiles with < 2 non-zero, non-NA
  # concentration values before passing to tblNCA. At least 2 positive
  # values are needed to compute Cmax, Tmax, and AUClast. Profiles with
  # exactly 2 positive values are valid sparse profiles — tblNCA handles
  # them correctly, returning NA for lambda-z-dependent parameters (half-life,
  # AUC∞, CL/F, Vz/F) which require ≥ 3 points for regression. Profiles with
  # only 1 positive value produce no meaningful NCA output and are excluded.
  # Excluded profiles are reported via warning so callers can surface them.
  all_keys   <- unique(data[[nca_key]])
  good_keys  <- character(0)
  bad_keys   <- character(0)
  for (k in all_keys) {
    k_conc <- data[[col_map$conc]][data[[nca_key]] == k]
    k_conc_num <- suppressWarnings(as.numeric(k_conc))
    n_valid <- sum(!is.na(k_conc_num) & k_conc_num > 0)
    if (n_valid >= 2) good_keys <- c(good_keys, k)
    else              bad_keys  <- c(bad_keys,  k)
  }
  
  if (length(bad_keys) > 0) {
    warning(paste0("Excluded ", length(bad_keys), " profile(s) with fewer than 2 ",
                   "positive concentration values (no meaningful NCA output possible): ",
                   paste(head(bad_keys, 5), collapse = ", "),
                   if (length(bad_keys) > 5) " ..." else ""))
    data <- data[data[[nca_key]] %in% good_keys, ]
  }
  
  # Abort cleanly if no valid profiles remain
  if (nrow(data) == 0 || length(good_keys) == 0) return(NULL)

  # Resolve one dose per profile, in the exact order tblNCA will see the
  # profiles. tblNCA indexes `dose` POSITIONALLY against unique(key), so a
  # vector built in any other order (e.g. grouped by subject, while the key is
  # sorted lexicographically) silently gives each subject someone else's dose:
  # CL/F, Vz/F and the dose-normalised parameters are then wrong while Cmax,
  # AUC and half-life look perfectly normal. Callers therefore pass either a
  # single dose or a vector NAMED by subject ID, and names are matched here.
  # Matching by name is also what makes per-subject dosing work in a crossover,
  # where one subject contributes several profiles.
  final_keys  <- unique(data[[nca_key]])
  subj_by_key <- as.character(data[[col_map$subject]][match(final_keys, data[[nca_key]])])
  dose_in <- settings$dose

  if (length(dose_in) <= 1) {
    dose_num <- suppressWarnings(as.numeric(dose_in))
  } else if (!is.null(names(dose_in))) {
    # Named by profile key (dose_by_profile) or by subject (dose_by_subject)
    by_profile <- all(as.character(final_keys) %in% names(dose_in))
    dose_num <- suppressWarnings(as.numeric(
      if (by_profile) dose_in[as.character(final_keys)] else dose_in[subj_by_key]))
    if (anyNA(dose_num)) {
      warning("No dose value for subject(s): ",
              paste(unique(subj_by_key[is.na(dose_num)]), collapse = ", "),
              ". Check the Dose column.")
      return(NULL)
    }
  } else {
    warning("Per-subject doses must be supplied as a vector named by subject ID. ",
            "The analysis was stopped rather than risk assigning doses to the ",
            "wrong subjects.")
    return(NULL)
  }
  dur_in   <- if (is.null(settings$infusion_duration)) 0 else settings$infusion_duration
  dur_num  <- suppressWarnings(as.numeric(dur_in))
  if (length(dur_num) == 0 || is.na(dur_num)) dur_num <- 0
  mw_in    <- if (is.null(settings$mw)) 0 else settings$mw
  mw_num   <- suppressWarnings(as.numeric(mw_in))
  if (length(mw_num) == 0 || is.na(mw_num)) mw_num <- 0

  # Manual half-life overrides: pass the chosen points to NonCompart
  # (UsePoints), so the slope and every parameter that depends on it (AUCinf,
  # extrapolated %, AUMC, MRT, CL, V, predicted-Clast variants) are computed by
  # NonCompart itself, with its unit conversions, exactly as for automatic fits.
  use_points <- override_use_points(data, col_map, nca_key, final_keys, lz_overrides)
  manual <- if (is.null(use_points)) rep(FALSE, length(final_keys)) else !vapply(use_points, is.null, logical(1))
  # Without a manual selection, keep values set by a BLQ rule out of the fit
  no_fit <- rep(FALSE, length(final_keys))
  if (BLQ_FLAG_COLUMN %in% names(data)) {
    for (i in which(!manual)) {
      rows <- data[[nca_key]] == final_keys[i]
      bf <- blq_free_slope(data[[col_map$time]][rows], data[[col_map$conc]][rows],
                           data[[BLQ_FLAG_COLUMN]][rows], adm)
      if (is.null(bf)) next
      if (length(bf$points) == 0) { no_fit[i] <- TRUE; next }
      if (is.null(use_points)) use_points <- vector("list", length(final_keys))
      use_points[[i]] <- bf$points
    }
  }

  # Run NCA via NonCompart
  result <- tryCatch({
    tblNCA(
      concData  = data,
      key       = nca_key,
      colTime   = col_map$time,
      colConc   = col_map$conc,
      dose      = dose_num,
      adm       = adm,
      dur       = dur_num,
      doseUnit  = settings$dose_unit,
      timeUnit  = settings$time_unit,
      concUnit  = settings$conc_unit,
      down      = down_method,
      # R2ADJ = 0 (NOT the user threshold). When R2ADJ > 0, NonCompart::sNCA
      # falls into the INTERACTIVE base-graphics picker DetSlope() ("Choose points
      # for terminal slope", via identify()) for any profile whose automatic fit
      # is below the threshold — which BLOCKS the whole app in an interactive R
      # session (e.g. RStudio runApp). With R2ADJ = 0 the automatic best-adjusted-R²
      # slope is always used; the user's R² threshold is applied by the app's own
      # half-life review (estimate_lambda_z) and the R²adj column is shown for
      # inspection. Verified: identical results to the threshold for well-behaved
      # profiles (DetSlope only ever fires below threshold).
      R2ADJ     = 0,
      MW        = mw_num,
      SS        = settings$is_steady_state,
      iAUC      = iAUC_df,
      UsePoints = use_points
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    # A "lazy-load database ... is corrupt" error is not an analysis problem: the
    # R session holds a stale handle to NonCompart (typically because the package
    # was updated while this session had it loaded). Tell the user how to fix it
    # instead of the misleading generic "check settings".
    if (grepl("lazy-load|lazy load", msg, ignore.case = TRUE)) {
      warning("The NonCompart engine could not load (", msg, "). This usually means ",
              "the R session has a stale package handle — restart R (in RStudio: ",
              "Session → Restart R) and relaunch the app. No reinstall is normally needed.")
    } else {
      warning("NCA calculation failed: ", msg)
    }
    NULL
  })
  
  if (!is.null(result)) {
    for (i in seq_len(nrow(result))) {
      rows <- data[[nca_key]] == result[[1]][i]
      row <- as.list(result[i, , drop = FALSE])
      fixed <- fix_log_down_zeros(row, data[[col_map$time]][rows], data[[col_map$conc]][rows],
                                  adm, down_method, dur = dur_num, ss = ss)
      k <- match(as.character(result[[1]][i]), as.character(final_keys))
      if (!is.null(use_points) && !is.na(k) && !is.null(use_points[[k]]))
        fixed <- fix_predicted_clast(fixed, adm, dur = if (adm == "Infusion") dur_num else 0)
      fixed <- fix_tlag(fixed, data[[col_map$time]][rows], data[[col_map$conc]][rows], adm)
      for (n in names(fixed)) if (!identical(fixed[[n]], row[[n]])) result[[n]][i] <- fixed[[n]]
    }
  }

  # Apply the analyst's adjusted R2 threshold. NonCompart is called with
  # R2ADJ = 0 (see above), so it always reports the best automatic fit; a fit
  # below the threshold is not reliable enough for half-life and everything
  # derived from it. Profiles with a manual selection are exempt.
  if (!is.null(result) && "R2ADJ" %in% names(result)) {
    k <- match(as.character(result[[1]]), as.character(final_keys))
    low <- (below_r2_threshold(result$R2ADJ, settings$r2adj_threshold) | no_fit[k]) & !manual[k]
    if (ss) {
      keys_ss <- as.character(result[[1]])
      note <- steady_state_predose_note(
        lapply(keys_ss, function(k) data_all[[col_map$time]][data_all[[nca_key]] == k]),
        lapply(keys_ss, function(k) data_all[[col_map$conc]][data_all[[nca_key]] == k]),
        if (use_composite_key) profile_labels(key_parts[match(keys_ss, key_parts$.nca_key), pk$cols, drop = FALSE])
        else keys_ss, adm)
      if (!is.null(note)) warning(note)
      # Steady-state parameters per profile, before any rows are blanked
      for (n in c("TAU", "CAVG", "CMIN_SS", "FLUCTP", "SWING")) if (!n %in% names(result)) result[[n]] <- NA_real_
      for (i in seq_len(nrow(result))) {
        rows <- data_all[[nca_key]] == result[[1]][i]
        rr <- steady_state_parameters(as.list(result[i, , drop = FALSE]),
                                      data_all[[col_map$time]][rows], data_all[[col_map$conc]][rows], tau,
                                      lamz_rejected = low[i])
        for (n in names(rr)) result[[n]][i] <- rr[[n]]
      }
    }
    if (any(low)) {
      for (cc in lamz_dependent_cols(names(result), settings$is_steady_state)) result[[cc]][low] <- NA
      warning("Half-life not reported for ", sum(low), " profile(s) with adjusted R\u00b2 below ",
              settings$r2adj_threshold, ": ",
              paste(head(if (use_composite_key)
                profile_labels(key_parts[match(as.character(result[[1]][low]), key_parts$.nca_key), pk$cols, drop = FALSE])
                else as.character(result[[1]][low]), 5), collapse = ", "),
              if (sum(low) > 5) " ..." else "",
              ". Half-life, AUC to infinity, CL/F, Vz/F and MRT are missing for these profiles; ",
              "review them in Half-Life Review.")
    }
  }

  if (!is.null(result) && !is.null(pauc)) {
    flags <- vector("list", nrow(result))
    vals <- NULL
    for (i in seq_len(nrow(result))) {
      rows <- data[[nca_key]] == result[[1]][i]
      flags[[i]] <- partial_auc_profile(as.list(result[i, , drop = FALSE]), pauc,
                                        data[[col_map$time]][rows], data[[col_map$conc]][rows],
                                        data[[BLQ_FLAG_COLUMN]][rows], partial_auc_blq_fraction(settings))
      vals <- rbind(vals, flags[[i]]$values)
    }
    result <- result[, !grepl("^\\.PAUC", names(result)), drop = FALSE]
    for (n in colnames(vals)) result[[n]] <- unname(vals[, n])
    # Which profiles rest mainly on BLQ-derived values, per interval. The
    # bioequivalence table reports these counts, because such a profile can
    # move a log-scale ratio without being a zero.
    blq_by_profile <- as.data.frame(do.call(rbind, lapply(flags, function(p) p$blq)))
    names(blq_by_profile) <- partial_auc_names(pauc)$auc
    keys <- as.character(result[[1]])
    labels <- if (use_composite_key)
      profile_labels(key_parts[match(keys, key_parts$.nca_key), pk$cols, drop = FALSE]) else keys
    for (msg in partial_auc_notes(pauc, flags, labels, settings$trap_method,
                                  partial_auc_blq_fraction(settings))) warning(msg)
    pauc_blq <- blq_by_profile
  }

  # If a composite key was used, restore its parts (Subject, Treatment,
  # Period) by matching the key, never by splitting the string: a separator
  # inside a treatment name can then not corrupt the columns.
  if (!is.null(result) && use_composite_key) {
    idx <- match(as.character(result[[1]]), key_parts$.nca_key)
    first_col <- names(result)[1]
    result[[first_col]] <- NULL
    for (cc in pk$cols) result[[cc]] <- key_parts[[cc]][idx]
    result <- result[, c(pk$cols, setdiff(names(result), pk$cols))]
  }

  if (!is.null(result) && exists("pauc_blq", inherits = FALSE)) {
    key_cols <- intersect(if (use_composite_key) pk$cols else names(result)[1], names(result))
    attr(result, "partial_auc_blq") <- cbind(result[, key_cols, drop = FALSE], pauc_blq)
  }

  result
}

#' Design problems that make one subject ID stand for more than one person
#'
#' A subject in two sequences, or with two treatments in the same period,
#' usually means subjects were numbered from 1 within each sequence.
#' @return list of list(message, detail, action); empty when the IDs are sound
design_identity_issues <- function(data, col_map) {
  has <- function(cc) !is.null(cc) && cc %in% names(data)
  subj <- col_map$subject
  if (!has(subj)) return(list())
  ids <- as.character(data[[subj]])
  shown <- function(x) paste0(paste(head(x, 5), collapse = ", "), if (length(x) > 5) paste0(" (+ ", length(x) - 5, " more)") else "")
  fix <- paste0("Give every subject a unique ID across sequences (for example 101-106 and ",
                "201-206 instead of 1-6 in each sequence), then upload the file again.")
  out <- list()
  if (has(col_map$sequence)) {
    n_seq <- tapply(as.character(data[[col_map$sequence]]), ids, function(v) length(unique(v[!is.na(v)])))
    multi <- names(n_seq)[n_seq > 1]
    if (length(multi) > 0)
      out[[length(out) + 1]] <- list(
        message = paste(length(multi), "subject ID(s) appear in more than one sequence"),
        detail = paste0("Subjects: ", shown(multi), "."), action = fix)
  }
  if (has(col_map$period) && has(col_map$treatment)) {
    sp <- paste(ids, as.character(data[[col_map$period]]), sep = "\r")
    n_trt <- tapply(as.character(data[[col_map$treatment]]), sp, function(v) length(unique(v[!is.na(v)])))
    multi <- unique(sub("\r.*$", "", names(n_trt)[n_trt > 1]))
    if (length(multi) > 0)
      out[[length(out) + 1]] <- list(
        message = paste(length(multi), "subject ID(s) received more than one treatment in the same period"),
        detail = paste0("Subjects: ", shown(multi), "."), action = fix)
  }
  out
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


# ============================================================================
# Reproduction from an Analysis Record
# ============================================================================
# Used by the reproduce_*.R scripts shipped in every record, and by the app
# when it runs those scripts at export time.

#' Compare a file's SHA-256 with the value recorded at analysis time
#' @return "MATCH", "MISMATCH" or "NOT CHECKED"; prints one line
verify_file_hash <- function(path, recorded, label) {
  if (is.null(recorded) || !nzchar(recorded) || !file.exists(path) ||
      !requireNamespace("digest", quietly = TRUE)) {
    cat(label, ": NOT CHECKED\n", sep = "")
    return(invisible("NOT CHECKED"))
  }
  status <- if (identical(digest::digest(file = path, algo = "sha256"), recorded)) "MATCH" else "MISMATCH"
  cat(label, ": ", status, if (status == "MISMATCH") " - this file differs from the one analysed!", "\n", sep = "")
  invisible(status)
}

#' Read the data of an Analysis Record the way the app read it
#'
#' A flat upload is read with its recorded separator/decimal mark. An ADNCA
#' import is converted again with the recorded choices; that needs
#' adnca_import.R (shipped in the record) to be sourced first.
#' @return list(raw = flat table, read_args = arguments for prepare_pk_dataset)
read_record_input <- function(rec) {
  if (identical(rec$door, "adnca")) {
    conv <- adnca_convert(adnca_read(rec$input_file, rec$read_args), time = rec$adnca$time,
                          paramcd = rec$adnca$paramcd, pcspec = rec$adnca$pcspec,
                          zero_predose = isTRUE(rec$adnca$zero_predose))
    list(raw = conv$flat, read_args = list())
  } else {
    list(raw = read_pk_file(rec$input_file, rec$read_args), read_args = rec$read_args)
  }
}

#' NCA settings as recorded in analysis_settings.json
#'
#' Per-subject doses are recomputed from the Dose column with
#' dose_by_subject(), exactly as the app computed them.
record_nca_settings <- function(rec, data, col_map) {
  dose <- if (identical(rec$dose_source, "per_profile")) dose_by_profile(data, col_map) else
          if (identical(rec$dose_source, "per_subject")) dose_by_subject(data, col_map) else rec$dose
  list(admin_route = rec$admin_route, dose = dose,
       infusion_duration = if (is.null(rec$infusion_dur)) 0 else rec$infusion_dur,
       is_steady_state = isTRUE(rec$steady_state), tau = rec$tau,
       dose_unit = rec$dose_unit, time_unit = rec$time_unit, conc_unit = rec$conc_unit,
       trap_method = rec$trap_method, r2adj_threshold = rec$r2adj_threshold,
       mw = if (is.null(rec$mw)) 0 else rec$mw, partial_aucs = partial_auc_spec(rec$partial_aucs),
       partial_auc_blq_fraction = rec$partial_auc_blq_fraction)
}

#' Compare reproduced results with the app's results shipped in the record
#'
#' @param result Reproduced NCA table (data frame) or single-profile vector
#' @param ref_file app_results_reference.csv
#' @return "MATCH" (max relative difference < 1e-6), "CLOSE" (< 1e-3),
#'   "DIFFERENT", or "NOT COMPARED"; prints a summary line
compare_with_reference <- function(result, ref_file = "app_results_reference.csv", integrity = NULL) {
  # A file that differs from the one analysed makes the record DIFFERENT, even
  # when the numbers happen to agree
  changed <- names(integrity)[integrity %in% "MISMATCH"]
  if (is.null(result) || length(result) == 0) {
    cat("Result: FAILED (the reproduction produced no NCA result)\n")
    return(invisible("FAILED"))
  }
  if (!file.exists(ref_file)) {
    cat("Result: NOT COMPARED (", ref_file, " not found)\n", sep = "")
    return(invisible("NOT COMPARED"))
  }
  ref <- read.csv(ref_file, stringsAsFactors = FALSE, check.names = FALSE)
  # A partial AUC column is named after its interval: one that is missing from
  # the reproduction means the recorded intervals differ from the app's
  ref_pauc <- grep(PARTIAL_AUC_PATTERN, if (is.data.frame(result)) names(ref) else ref$Parameter, value = TRUE)
  res_pauc <- grep(PARTIAL_AUC_PATTERN, names(result), value = TRUE)
  missing_pauc <- union(setdiff(ref_pauc, res_pauc), setdiff(res_pauc, ref_pauc))
  if (length(missing_pauc) > 0) {
    cat("Result: DIFFERENT (partial AUC columns differ: ", paste(missing_pauc, collapse = ", "), ")\n", sep = "")
    return(invisible("DIFFERENT"))
  }
  # Every parameter must be on both sides: a column deleted from the
  # reference would otherwise simply not be compared. Dose-normalised values
  # are added by the app after the NCA and are not reproduced.
  ref_names <- if (is.data.frame(result)) names(ref) else ref$Parameter
  res_names <- names(result)[vapply(names(result), function(n) is.numeric(result[[n]]), logical(1))]
  keys <- c("Subject", "Treatment", "Period")
  one_side <- union(setdiff(setdiff(ref_names, keys), c(names(result), grep("_DN$", ref_names, value = TRUE))),
                    setdiff(setdiff(res_names, keys), ref_names))
  if (length(one_side) > 0) {
    cat("Result: DIFFERENT (parameters present on one side only: ", paste(head(one_side, 10), collapse = ", "),
        ")\n", sep = "")
    return(invisible("DIFFERENT"))
  }
  if (is.data.frame(result)) {
    shared_cols <- intersect(names(result), names(ref))
    # Align rows on the profile key rather than trusting row order
    key_cols <- intersect(c("Subject", "Treatment", "Period"), shared_cols)
    if (nrow(result) != nrow(ref)) {
      cat("Result: DIFFERENT (", nrow(result), " reproduced rows vs ", nrow(ref), " in the app)\n", sep = "")
      return(invisible("DIFFERENT"))
    }
    if (length(key_cols) > 0) {
      k_res <- do.call(paste, c(lapply(result[key_cols], as.character), sep = "||"))
      k_ref <- do.call(paste, c(lapply(ref[key_cols], as.character), sep = "||"))
      ord <- match(k_res, k_ref)
      if (anyNA(ord) || anyDuplicated(ord)) {
        cat("Result: DIFFERENT (profiles do not match the app's profiles)\n")
        return(invisible("DIFFERENT"))
      }
      ref <- ref[ord, , drop = FALSE]
      shared_cols <- setdiff(shared_cols, key_cols)
    }
    a_list <- lapply(shared_cols, function(cn) suppressWarnings(as.numeric(as.character(result[[cn]]))))
    b_list <- lapply(shared_cols, function(cn) suppressWarnings(as.numeric(as.character(ref[[cn]]))))
    n_rows <- nrow(result)
  } else {
    shared_cols <- intersect(names(result), ref$Parameter)
    a_list <- lapply(shared_cols, function(cn) suppressWarnings(as.numeric(result[[cn]])))
    b_list <- lapply(shared_cols, function(cn) suppressWarnings(as.numeric(ref$Value[match(cn, ref$Parameter)])))
    n_rows <- 1
  }
  max_rel <- 0; n_cmp <- 0; n_par <- 0; na_mismatch <- 0
  for (i in seq_along(shared_cols)) {
    a <- a_list[[i]]; b <- b_list[[i]]
    na_mismatch <- na_mismatch + sum(is.finite(a) != is.finite(b))
    both <- is.finite(a) & is.finite(b)
    if (!any(both)) next
    n_par <- n_par + 1; n_cmp <- n_cmp + sum(both)
    max_rel <- max(max_rel, max(abs(a[both] - b[both]) / pmax(abs(b[both]), 1e-12)))
  }
  verdict <- if (na_mismatch > 0) "DIFFERENT" else if (max_rel < 1e-6) "MATCH" else
             if (max_rel < 1e-3) "CLOSE" else "DIFFERENT"
  cat(sprintf("Compared %d numeric values across %d parameters (%d rows). Max relative difference: %.3g -> %s\n",
              n_cmp, n_par, n_rows, max_rel, verdict))
  if (na_mismatch > 0) cat(na_mismatch, "value(s) are missing in one table but not the other.\n")
  if (length(changed) > 0) {
    cat("Result: DIFFERENT (", paste(changed, collapse = " and "), " not the one analysed; the numbers were ",
        verdict, ")\n", sep = "")
    return(invisible("DIFFERENT"))
  }
  cat("Result: ", verdict, "\n", sep = "")
  invisible(verdict)
}
