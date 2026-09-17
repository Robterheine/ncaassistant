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
    readxl::read_excel(path, sheet = sheet)
  } else {
    read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE)
  }
}

#' Read decimal-comma numbers stored as text when the file uses a decimal comma
#'
#' read.csv(dec = ",") converts a column only when every value is a number.
#' A concentration column that also holds BLQ text ("<0,5") stays text, and
#' values such as "4,25" would become missing. When the upload's decimal mark
#' is a comma, values that are plain decimal-comma numbers are rewritten with
#' a point; anything else (BLQ text, other text) is left unchanged.
#' @param x Column as read
#' @param dec Decimal mark chosen at upload
normalise_decimal_comma <- function(x, dec) {
  if (!identical(dec, ",") || !(is.character(x) || is.factor(x))) return(x)
  x <- as.character(x)
  num_comma <- grepl("^\\s*-?\\d+,\\d+\\s*$", x)
  x[num_comma] <- sub(",", ".", trimws(x[num_comma]), fixed = TRUE)
  x
}

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
    time      = detect(c("^time", "^tpt", "^hours?$", "^hour", "^apts",
                          "^ntim", "^zeit", "^tid"), 2, "time"),
    conc      = detect(c("^conc", "^dv$", "^cp[^a-z]", "^cp$", "^concentration",
                          "^result", "^konz", "^plasma", "ug.l", "ng.ml"), 3, "conc"),
    treatment = detect_optional(c("^trt", "^treat", "^form", "^drug", "^arm",
                                   "^behandl")),
    period    = detect_optional(c("^per", "^period", "^prd", "^phase")),
    sequence  = detect_optional(c("^seq", "^grp", "^sequence")),
    dose      = detect_optional(c("^dose", "^amt$", "^amount", "^dosis"))
  )
  attr(out, "unmatched") <- unmatched
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
#'   Rule 4: All BLQ set to LLOQ/2
#'   Rule 5: Pre-Cmax BLQ = 0; post-Cmax BLQ = Missing
#'   Rule 6: Pre-first-quantifiable set to LLOQ/2; all other BLQ set to 0
#'           (for drugs with absorption lag; used in ROSIE and similar studies)
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
    for (s in unique(prof_key)) {
      idx <- profile_idx(s)
      sub <- data[idx, ]
      tmax_idx <- which.max(sub[[conc_col]])
      
      # Guard: which.max returns integer(0) when all concentrations are NA.
      # 1:integer(0) throws "argument of length 0" — skip this subject.
      if (length(tmax_idx) == 0) next
      
      pre_cmax  <- idx[1:tmax_idx]
      post_cmax <- if (tmax_idx < length(idx)) idx[(tmax_idx + 1):length(idx)] else integer(0)
      
      data[[conc_col]][intersect(pre_cmax,  which(data$.is_blq))] <- 0
      data[[conc_col]][intersect(post_cmax, which(data$.is_blq))] <- NA
    }
    
  } else if (rule == "rule6") {
    # Rule 6: Pre-first-quantifiable BLQ -> LLOQ/2; all other BLQ -> 0
    # Appropriate for drugs with absorption lag where first samples may be BLQ
    for (s in unique(prof_key)) {
      idx <- profile_idx(s)
      sub <- data[idx, ]
      quant_idx <- which(!sub$.is_blq & !is.na(sub[[conc_col]]))
      
      if (length(quant_idx) == 0) {
        data[[conc_col]][idx[data$.is_blq[idx]]] <- lloq / 2
        next
      }
      
      first_quant <- min(quant_idx)
      
      # Before first quantifiable: set BLQ to LLOQ/2
      if (first_quant > 1) {
        pre <- idx[1:(first_quant - 1)]
        data[[conc_col]][intersect(pre, which(data$.is_blq))] <- lloq / 2
      }
      # All other BLQ (during and after quantifiable phase): set to 0
      from_quant <- idx[first_quant:length(idx)]
      data[[conc_col]][intersect(from_quant, which(data$.is_blq))] <- 0
    }
    
  } else {
    # Rule 1 (default): pre-first-quantifiable -> 0, post-last-quantifiable -> NA
    for (s in unique(prof_key)) {
      idx <- profile_idx(s)
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

#' NCA for one profile given as vectors (single-subject analysis)
#'
#' Same NonCompart call and options as run_nca(), so a profile analysed on
#' its own gives the same result as in the batch table.
#' @param time_used Optional sampling times for a manual half-life override
run_single_nca <- function(time, conc, settings, time_used = NULL) {
  adm <- switch(settings$admin_route, "extravascular" = "Extravascular",
                "iv_bolus" = "Bolus", "iv_infusion" = "Infusion", "Extravascular")
  down <- switch(settings$trap_method, "linear" = "Linear", "log" = "Log", "Linear")
  t_num <- suppressWarnings(as.numeric(as.character(time)))
  c_num <- suppressWarnings(as.numeric(as.character(conc)))
  ord <- order(t_num); t_num <- t_num[ord]; c_num <- c_num[ord]
  use <- NULL
  if (length(time_used) >= 2) {
    keep <- !(is.na(t_num) | is.na(c_num))
    xf <- t_num[keep]; yf <- c_num[keep]
    tu <- as.numeric(unlist(time_used))
    use <- which(yf > 0 & vapply(xf, function(t) any(abs(t - tu) <= 1e-9 * max(1, abs(t))), logical(1)))
    if (length(use) < 2) use <- NULL
  }
  num0 <- function(v) { v <- suppressWarnings(as.numeric(v)); if (length(v) == 0 || is.na(v)) 0 else v }
  r <- NonCompart::sNCA(t_num, c_num, dose = num0(settings$dose), adm = adm, down = down,
                   dur = if (adm == "Infusion") num0(settings$infusion_duration) else 0,
                   doseUnit = settings$dose_unit, timeUnit = settings$time_unit,
                   concUnit = settings$conc_unit, SS = isTRUE(settings$is_steady_state),
                   # R2ADJ = 0 avoids NonCompart's interactive slope picker (see run_nca)
                   R2ADJ = 0, MW = num0(settings$mw), UsePoints = use)
  # The analyst's R2 threshold applies to the automatic fit, not to points
  # chosen by hand
  if (is.null(use) && below_r2_threshold(r["R2ADJ"], settings$r2adj_threshold)) {
    r[lamz_dependent_cols(names(r), settings$is_steady_state)] <- NA
  }
  r
}

#' Steady-state summary parameters added to a single-profile NCA result
#'
#' tau is taken as the sampled interval (last minus first time); CAVG =
#' AUClast / tau; fluctuation and swing from Cmax and the minimum observed
#' concentration.
add_steady_state_parameters <- function(r, time, conc) {
  tau    <- max(time, na.rm = TRUE) - min(time, na.rm = TRUE)
  auclst <- as.numeric(r["AUCLST"])
  cmax   <- as.numeric(r["CMAX"])
  cmin   <- min(conc[!is.na(conc)], na.rm = TRUE)
  cavg   <- if (tau > 0 && !is.na(auclst)) auclst / tau else NA
  fluct  <- if (!is.na(cavg) && cavg > 0) (cmax - cmin) / cavg * 100 else NA
  swing  <- if (!is.na(cmin) && cmin > 0) (cmax - cmin) / cmin else NA
  c(r,
    AUCTAU  = auclst,
    TAU     = tau,
    CAVG    = if (is.finite(cavg)) cavg else NA,
    CMIN_SS = cmin,
    FLUCTP  = if (is.finite(fluct)) fluct else NA,
    SWING   = if (is.finite(swing)) swing else NA)
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
  
  # Apply the analyst's adjusted R2 threshold. NonCompart is called with
  # R2ADJ = 0 (see above), so it always reports the best automatic fit; a fit
  # below the threshold is not reliable enough for half-life and everything
  # derived from it. Profiles with a manual selection are exempt.
  if (!is.null(result) && "R2ADJ" %in% names(result)) {
    low <- below_r2_threshold(result$R2ADJ, settings$r2adj_threshold)
    if (!is.null(use_points)) {
      manual <- !vapply(use_points, is.null, logical(1))
      low <- low & !manual[match(as.character(result[[1]]), as.character(final_keys))]
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
  
  result
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
       is_steady_state = isTRUE(rec$steady_state),
       dose_unit = rec$dose_unit, time_unit = rec$time_unit, conc_unit = rec$conc_unit,
       trap_method = rec$trap_method, r2adj_threshold = rec$r2adj_threshold,
       mw = if (is.null(rec$mw)) 0 else rec$mw, partial_aucs = NULL)
}

#' Compare reproduced results with the app's results shipped in the record
#'
#' @param result Reproduced NCA table (data frame) or single-profile vector
#' @param ref_file app_results_reference.csv
#' @return "MATCH" (max relative difference < 1e-6), "CLOSE" (< 1e-3),
#'   "DIFFERENT", or "NOT COMPARED"; prints a summary line
compare_with_reference <- function(result, ref_file = "app_results_reference.csv") {
  if (is.null(result) || length(result) == 0) {
    cat("Result: FAILED (the reproduction produced no NCA result)\n")
    return(invisible("FAILED"))
  }
  if (!file.exists(ref_file)) {
    cat("Result: NOT COMPARED (", ref_file, " not found)\n", sep = "")
    return(invisible("NOT COMPARED"))
  }
  ref <- read.csv(ref_file, stringsAsFactors = FALSE, check.names = FALSE)
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
  cat("Result: ", verdict, "\n", sep = "")
  invisible(verdict)
}
