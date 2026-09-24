# ============================================================================
# NCA Assistant — Interlocks
# ============================================================================
# Checks that refuse data the app cannot analyse safely. Each interlock is a
# pure function of the uploaded table and the column mapping and returns
# findings in the data quality report shape (Severity / Category / Message /
# Detail / Action), so render_quality_report() shows them unchanged.
# run_data_quality_check() runs all of them; an ERROR blocks processing.
#
# They target failures that otherwise produce plausible numbers without any
# error: a CDISC dataset whose record-selection flags would be ignored, mixed
# units, date-times read as numbers, time measured from the first dose
# instead of the dose of each period, and several profiles stacked into one.
# ============================================================================

.finding <- function(severity, category, message, detail = "", action = "") {
  data.frame(Severity = severity, Category = category, Message = message,
             Detail = detail, Action = action, stringsAsFactors = FALSE)
}

.no_findings <- function() {
  data.frame(Severity = character(0), Category = character(0), Message = character(0),
             Detail = character(0), Action = character(0), stringsAsFactors = FALSE)
}

#' Run interlocks
#' @param scope "all", "file" (need no column mapping: file shape, units) or
#'   "mapped" (need the time/subject mapping)
#' @return data frame of findings (possibly empty)
run_interlocks <- function(data, col_map, scope = "all") {
  out <- list()
  if (scope %in% c("all", "file")) {
    out <- c(out, list(interlock_adnca_shape(data), interlock_mixed_units(data)))
  }
  if (scope %in% c("all", "mapped")) {
    out <- c(out, list(interlock_time_format(data, col_map),
                       interlock_profile_start(data, col_map),
                       interlock_stacked_profiles(data, col_map),
                       interlock_second_dose(data, col_map),
                       interlock_unmapped_period(data, col_map)))
  }
  do.call(rbind, c(list(.no_findings()), out))
}

#' Refuse files shaped like CDISC ADaM ADNCA or SDTM PC data
#'
#' Such files carry record-selection information (ANL01FL, DTYPE), several
#' time variables (ARRLT, NRRLT, AFRLT) and often several analytes in one
#' column. Read as a flat file, all of that is silently ignored.
interlock_adnca_shape <- function(data) {
  nm <- toupper(names(data))
  has_value   <- any(c("AVAL", "PCSTRESN") %in% nm)
  has_subject <- "USUBJID" %in% nm
  markers <- intersect(c("PARAMCD", "PCTESTCD", "ANL01FL", "DTYPE", "ARRLT", "NRRLT",
                         "AFRLT", "PCDTC", "AVALU", "PCSTRESU"), nm)
  if (!(has_value && has_subject && length(markers) > 0)) return(.no_findings())
  .finding("ERROR", "Format",
           "This looks like a CDISC ADNCA or SDTM PC dataset",
           paste0("CDISC variables found: ",
                  paste(intersect(c("USUBJID", "AVAL", "PCSTRESN", markers), nm), collapse = ", ")),
           paste0("The upload accepts flat files only (one row per sample: subject, time, ",
                  "concentration, ...). A CDISC dataset read this way would ignore its ",
                  "analysis flags (ANL01FL, DTYPE), may mix analytes, and needs a choice between ",
                  "actual and nominal time. Convert it to a flat file first: keep one analyte, ",
                  "apply ANL01FL and DTYPE, and use ARRLT or NRRLT as time. To do this in the ",
                  "app, set 'What kind of file?' to 'CDISC ADNCA dataset' at the top of this ",
                  "page and upload the file again."))
}

#' Refuse more than one unit in a unit column
#'
#' The app applies one concentration and one time unit to the whole file.
#' Different units are refused even when numerically equivalent (ng/mL and
#' ug/L), because the app cannot tell equivalent from non-equivalent.
interlock_mixed_units <- function(data) {
  nm <- names(data)
  # A unit column is named Unit(s), has Unit(s) as a separate word (Conc_Unit,
  # time.units) or as a CamelCase suffix (TimeUnit), or is a CDISC unit
  # variable. "Community" or "Opportunity" are not unit columns.
  unit_cols <- nm[grepl("(^|[_. ])units?($|[_. ])", nm, ignore.case = TRUE) |
                  grepl("[a-z]Units?$", nm) |
                  toupper(nm) %in% c("AVALU", "PCSTRESU", "PCORRESU", "RRLTU", "DOSEU", "EXDOSU",
                                     "CONCU", "TIMEU")]
  out <- list()
  for (cc in unit_cols) {
    v <- trimws(as.character(data[[cc]]))
    u <- unique(v[!is.na(v) & v != ""])
    if (length(u) > 1) {
      out[[cc]] <- .finding("ERROR", "Units",
        paste0("Column '", cc, "' contains more than one unit"),
        paste0("Units found: ", paste(head(u, 6), collapse = ", ")),
        paste0("The analysis uses one unit per quantity for the whole file. Convert the ",
               "values to a single unit before upload, even if the units are equivalent."))
    }
  }
  if (length(out) == 0) .no_findings() else do.call(rbind, out)
}

#' Refuse time columns that hold dates, date-times, durations or clock times
#'
#' Excel date-times arrive as date-time objects that convert silently to
#' seconds since 1970; the profile then looks normal, half-life is wrong, and
#' a Test/Reference ratio can still look right because both are scaled alike.
interlock_time_format <- function(data, col_map) {
  tc <- col_map$time
  if (is.null(tc) || !tc %in% names(data)) return(.no_findings())
  x <- data[[tc]]
  action <- paste0("The Time column must be numeric elapsed time since the dose of that ",
                   "profile (for example hours after dosing). Derive it before upload.")
  if (inherits(x, c("POSIXt", "Date", "difftime", "hms"))) {
    return(.finding("ERROR", "Time", "Time column contains dates or date-times, not elapsed time",
                    paste0("Column '", tc, "' is stored as ", class(x)[1],
                           " (for example an Excel date/time format)."), action))
  }
  v <- trimws(as.character(x)); v <- v[!is.na(v) & v != ""]
  if (length(v) == 0) return(.no_findings())
  kinds <- c(
    "ISO 8601 date or date-time" = "^\\d{4}-\\d{2}(-\\d{2})?([T ]\\d{1,2}(:\\d{2})?)?",
    "ISO 8601 duration"          = "^-?P(\\d+[YMWD])*(T(\\d+(\\.\\d+)?[HMS])+)?$",
    "clock time"                 = "^\\d{1,2}:\\d{2}(:\\d{2})?$",
    "date"                       = "^\\d{1,2}[/.-]\\d{1,2}[/.-]\\d{2,4}")
  hits <- vapply(kinds, function(p) sum(grepl(p, v, perl = TRUE)), numeric(1))
  hits <- hits[hits > 0]
  if (length(hits) == 0) return(.no_findings())
  kind <- names(hits)[which.max(hits)]
  ex <- head(v[grepl(kinds[[kind]], v, perl = TRUE)], 3)
  .finding("ERROR", "Time",
           paste0("Time column contains ", kind, " values (date/time format), not elapsed time"),
           paste0("Examples: ", paste(ex, collapse = ", ")), action)
}

#' Refuse profiles whose first sample is far from time zero
#'
#' Catches time measured from the first dose of the study (period 2 starting
#' at ~168 h), clock or calendar times read as numbers, and epoch seconds. A
#' profile is subject x treatment x period, as in the NCA. A profile with
#' fewer than three samples only warns, because its span says little.
interlock_profile_start <- function(data, col_map) {
  tc <- col_map$time; sc <- col_map$subject
  if (is.null(tc) || is.null(sc) || !all(c(tc, sc) %in% names(data))) return(.no_findings())
  t <- suppressWarnings(as.numeric(as.character(data[[tc]])))
  if (inherits(data[[tc]], c("POSIXt", "Date"))) return(.no_findings())  # reported by time format
  ok <- !is.na(t)
  if (!any(ok)) return(.no_findings())
  key <- profile_key(data, col_map)
  labels <- profile_labels(key$parts)[ok]
  starts <- tapply(t[ok], labels, min); ends <- tapply(t[ok], labels, max)
  n_pts <- tapply(t[ok], labels, length)
  far <- which(starts > 0 & starts > 0.2 * (ends - starts))
  if (length(far) == 0) return(.no_findings())
  # Index by position: a blank subject ID gives an empty label, and indexing a
  # named vector by "" returns NA
  bad <- names(starts)[far]
  sparse <- as.vector(n_pts[far]) < 3
  detail <- paste0("Profiles (first time): ",
                   paste(head(paste0(bad, " (", signif(as.vector(starts[far]), 6), ")"), 5), collapse = "; "),
                   if (length(bad) > 5) paste0("; + ", length(bad) - 5, " more") else "")
  action <- paste0("Time must be measured from the dose of each profile. In a crossover, ",
                   "use time since the dose of that period, not since the first dose of the study ",
                   "(in CDISC terms: ARRLT or NRRLT, not AFRLT). For steady-state data, use time ",
                   "since the most recent dose (time 0 = just before that dose), not since the first ",
                   "dose. Very large times may be dates or date-times stored as numbers.")
  out <- list()
  if (any(!sparse)) {
    out[[1]] <- .finding("ERROR", "Time",
      paste0(sum(!sparse), " profile(s) do not start near time zero (time since first dose?)"),
      detail, action)
  }
  if (any(sparse)) {
    out[[2]] <- .finding("WARNING", "Time",
      paste0(sum(sparse), " sparse profile(s) with fewer than 3 samples do not start near time zero"),
      paste0("Profiles: ", paste(head(bad[sparse], 5), collapse = "; ")), action)
  }
  do.call(rbind, out)
}

#' Refuse stacked profiles: duplicate times within one profile
#'
#' Several profiles in one column (more than one analyte or matrix, or
#' repeated rows) look like one profile with duplicated times.
interlock_stacked_profiles <- function(data, col_map) {
  tc <- col_map$time; sc <- col_map$subject
  if (is.null(tc) || is.null(sc) || !all(c(tc, sc) %in% names(data))) return(.no_findings())
  t <- suppressWarnings(as.numeric(as.character(data[[tc]])))
  ok <- !is.na(t)
  key <- profile_key(data, col_map)
  prof <- key$key[ok]
  dup <- duplicated(data.frame(prof, t[ok]))
  if (!any(dup)) return(.no_findings())
  dup_subjects <- unique(key$parts$Subject[ok][dup])
  unmapped <- c(if (!"Treatment" %in% key$cols) "Treatment", if (!"Period" %in% key$cols) "Period")
  .finding("ERROR", "Time",
    paste(sum(dup), "duplicate time points across", length(dup_subjects), "subjects"),
    paste0("Subjects: ", paste(head(dup_subjects, 5), collapse = ", ")),
    if (length(unmapped) == 0) {
      paste0("Duplicate times within one subject, treatment and period usually mean ",
             "several profiles are stacked in one column: more than one analyte or ",
             "matrix, or repeated rows. Filter the file so each subject contributes one ",
             "profile per treatment and period. Only average duplicates when they are ",
             "genuine replicate measurements of the same sample.")
    } else {
      paste0("Duplicate times usually mean several profiles are stacked in one ",
             "column: more than one analyte, matrix, period or treatment. If your file ",
             "has a ", paste(unmapped, collapse = " or "), " column, map it; otherwise ",
             "split or filter the file so each subject contributes one profile. Only ",
             "average duplicates when they are genuine replicate measurements of the ",
             "same sample.")
    })
}

#' Warn about a second rise after a long gap within one profile
#'
#' With time counted from the first dose and no Period column mapped, two
#' periods become one profile: the concentrations fall, sampling pauses (the
#' washout) and they rise again after the next dose. The first-sample check
#' cannot see this, because such a profile does start at time zero. A double
#' peak during absorption has no long gap before it, so it is not flagged.
interlock_second_dose <- function(data, col_map) {
  tc <- col_map$time; cc <- col_map$conc
  if (is.null(tc) || is.null(cc) || !all(c(tc, cc, col_map$subject) %in% names(data))) return(.no_findings())
  t <- suppressWarnings(as.numeric(as.character(data[[tc]])))
  y <- suppressWarnings(as.numeric(as.character(data[[cc]])))
  key <- profile_key(data, col_map)
  labels <- profile_labels(key$parts)
  hit <- character(0)
  for (lab in unique(labels)) {
    i <- which(labels == lab & !is.na(t) & !is.na(y)); i <- i[order(t[i])]
    if (length(i) < 5) next
    tt <- t[i]; yy <- y[i]; gaps <- diff(tt)
    g <- which.max(gaps)
    if (gaps[g] <= 3 * stats::median(gaps)) next
    after <- yy[(g + 1):length(yy)]
    if (max(after) > 2 * yy[g] && max(after) > 0.2 * max(yy) && which.max(after) > 1) hit <- c(hit, lab)
  }
  if (length(hit) == 0) return(.no_findings())
  .finding("WARNING", "Time",
    paste0(length(hit), " profile(s) rise again after a long sampling gap (a second dose in one profile?)"),
    paste0("Profiles: ", paste(head(hit, 5), collapse = "; "), if (length(hit) > 5) paste0("; + ", length(hit) - 5, " more") else ""),
    paste0("If subjects were dosed more than once (periods or occasions), map the Period column and use time ",
           "since the dose of each period. A column named Visit or Occasion is not recognised automatically. ",
           "Disregard this for a genuine double peak."))
}

#' Warn about an unmapped column that looks like a period or occasion
interlock_unmapped_period <- function(data, col_map) {
  if (!is.null(col_map$period) && nzchar(col_map$period)) return(.no_findings())
  sc <- col_map$subject
  if (is.null(sc) || !sc %in% names(data)) return(.no_findings())
  cand <- setdiff(names(data)[grepl("^(visit|avisit|occ|occasion|aperiod|period|trtseqp|day|aday)", names(data),
                                    ignore.case = TRUE)], unlist(col_map))
  multi <- cand[vapply(cand, function(cc) {
    n <- tapply(as.character(data[[cc]]), as.character(data[[sc]]), function(v) length(unique(v[!is.na(v)])))
    any(n > 1)
  }, logical(1))]
  if (length(multi) == 0) return(.no_findings())
  .finding("WARNING", "Design",
    paste0("Column '", multi[1], "' takes more than one value per subject, but no Period column is mapped"),
    if (length(multi) > 1) paste0("Also: ", paste(multi[-1], collapse = ", ")) else "",
    paste0("If it marks periods or dosing occasions, map it as Period; otherwise the profiles of different ",
           "periods are merged into one."))
}
