# ============================================================================
# NCA Assistant — CDISC PK parameter codes
# ============================================================================
# Official CDISC codes (PPTESTCD) and names (PPTEST) for the NCA parameters
# the app reports, from ONE pinned release of CDISC SDTM Controlled
# Terminology. The release, codelists and source are recorded in
# cdisc/ct_release.dcf and shown wherever codes appear (app, exports,
# Analysis Record), so a reader always knows which release was used.
#
# cdisc/pk_parameter_map.csv  app parameter -> PPTESTCD, per route and steady
#                             state (e.g. CL/F at steady state is CLFTAU)
# cdisc/pk_parameter_terms.csv PPTESTCD, PPTEST, NCIt code and definition,
#                             extracted by cdisc/extract_pk_terms.R
#
# This is a code lookup only. The app does not produce SDTM PP datasets and
# makes no claim of conformance to CDISC standards.
# ============================================================================

.cdisc_dir <- "cdisc"

#' Release metadata of the pinned Controlled Terminology
cdisc_ct_release <- function() {
  as.list(read.dcf(file.path(.cdisc_dir, "ct_release.dcf"))[1, ])
}

#' One-line statement of the standard and release used, for display
cdisc_ct_statement <- function() {
  r <- cdisc_ct_release()
  paste0("Parameter codes (PPTESTCD) and names (PPTEST): ", r$Standard, ", release ", r$Release,
         " (codelists C85839 PKPARMCD and C85493 PKPARM, NCI EVS).")
}

#' Official code and name for each app parameter
#'
#' @param params App parameter names (NCA result columns)
#' @param admin_route "extravascular", "iv_bolus" or "iv_infusion"
#' @param is_ss Steady-state analysis
#' @return data.frame(Parameter, PPTESTCD, PPTEST, NCIt_code, Note); PPTESTCD
#'   is "" when the release has no code for the parameter (Note says why)
cdisc_pk_codes <- function(params, admin_route = "extravascular", is_ss = FALSE) {
  map <- utils::read.csv(file.path(.cdisc_dir, "pk_parameter_map.csv"), stringsAsFactors = FALSE,
                         na.strings = character(0))
  terms <- utils::read.csv(file.path(.cdisc_dir, "pk_parameter_terms.csv"), stringsAsFactors = FALSE)
  ss <- if (isTRUE(is_ss)) "yes" else "no"
  rows <- lapply(params, function(p) {
    # Partial AUC columns share one map row per kind (AUC_T1_T2, CMAX_T1_T2, TMAX_T1_T2)
    iv <- regmatches(p, regexec(PARTIAL_AUC_PATTERN, p))[[1]]
    key <- if (length(iv) > 0) paste0(iv[2], "_T1_T2") else p
    cand <- map[map$app_parameter == key &
                map$route %in% c(admin_route, "any") &
                map$steady_state %in% c(ss, "any"), , drop = FALSE]
    if (nrow(cand) > 1) {   # prefer the most specific rule
      spec <- (cand$route != "any") + (cand$steady_state != "any")
      cand <- cand[which.max(spec), , drop = FALSE]
    }
    if (nrow(cand) == 0) {
      return(data.frame(Parameter = p, PPTESTCD = "", PPTEST = "", NCIt_code = "",
                        Note = if (key %in% map$app_parameter) "No code for this administration route" else
                          "Not mapped to a CDISC PK parameter code", stringsAsFactors = FALSE))
    }
    t <- terms[match(cand$PPTESTCD, terms$PPTESTCD), ]
    data.frame(Parameter = p, PPTESTCD = cand$PPTESTCD,
               PPTEST = if (nzchar(cand$PPTESTCD)) t$PPTEST else "",
               NCIt_code = if (nzchar(cand$PPTESTCD)) t$NCIt_code else "",
               Note = if (length(iv) > 0 && iv[2] == "AUC")
                 paste0(cand$note, ": PPSTINT ", iv[3], ", PPENINT ",
                        if (iv[4] == "t") "the time of the last measurable concentration" else iv[4],
                        " (time unit of the data)") else cand$note,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
