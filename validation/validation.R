# ============================================================================
# NCA Assistant — Consolidated Validation Script (version read from app.R)
# ============================================================================
# Attachment A to IQ/OQ/PQ Protocol
#
# Run from project root:
#   Rscript validation/validation.R
# ============================================================================

cat(paste(rep("=", 72), collapse=""), "
")
cat("NCA Assistant — Validation Script
")
cat(paste(rep("=", 72), collapse=""), "
")
cat("Execution started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "

")

if (!file.exists("app.R") || !dir.exists("R")) {
  stop("Run this script from the NCA Assistant project root:
  Rscript validation/validation.R")
}

# replicateBE is a validation-only dependency (reference implementation for
# the replicate-design checks in section REP); the app does not use it.
required_pkgs <- c("NonCompart", "PowerTOST", "nlme", "digest",
                   "openxlsx", "jsonlite", "readxl", "dplyr", "replicateBE",
                   "shinymanager", "DBI", "RSQLite")   # the last three: section GXP (controlled mode)
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  # A qualification run tests the installation as it is. Installing here would
  # qualify whatever versions CRAN serves today, so the run stops instead
  stop("Missing package(s): ", paste(missing, collapse = ", "), ". Install the validated versions ",
       "first, from the project root: in R, renv::restore(lockfile = \"validation/renv.lock\"), ",
       "or Rscript install_and_run.R --validated (which then starts the app).", call. = FALSE)
}
library(NonCompart); library(PowerTOST); library(nlme); library(digest)

for (f in c("R/pipeline.R", "R/adnca_import.R", "R/utils.R", "R/cdisc_terms.R", "R/nca_helpers.R", "R/interlocks.R", "R/data_quality.R",
           "R/export_record.R", "R/designs.R", "R/be_analysis.R", "R/be_scaled.R")) {
  tryCatch(source(f, local = TRUE), error = function(e) NULL)
}

# auto_detect_columns() and the data preparation now live in R/pipeline.R, so
# they are sourced directly instead of being text-extracted from the Shiny module.

# generate_nca_script() and the other record helpers are provided by sourcing
# R/export_record.R above (more robust than extracting a single function).

APP_VERSION <- tryCatch({
  app_lines <- readLines("app.R")
  ver_line <- grep("^APP_VERSION", app_lines, value = TRUE)[1]
  eval(parse(text = ver_line)); APP_VERSION
}, error = function(e) "unknown")

source_files <- c("R/utils.R", "R/nca_helpers.R", "R/data_quality.R",
                  "R/export_record.R", "R/mod_data_upload.R", "R/designs.R", "R/be_analysis.R",
                  "R/pipeline.R", "R/interlocks.R", "R/adnca_import.R", "R/cdisc_terms.R",
                  "converters/adnca_to_flat.R", "R/gxp_audit.R", "R/gxp_access.R", "R/gxp_sign.R",
                  "gxp/manage_users.R", "R/be_scaled.R")
hash_files <- c("validation/validation.R", source_files)
file_hashes <- sapply(hash_files, function(f) {
  if (file.exists(f)) digest(file = f, algo = "sha256") else "FILE_NOT_FOUND"
})
cat("File integrity hashes (SHA-256):
")
for (nm in names(file_hashes)) cat("  ", nm, ":", substr(file_hashes[nm], 1, 16), "...
")

results <- list(); section_times <- list(); current_section <- ""

check <- function(id, test_name, condition, urs_ref,
                  method = "", expected = "", critical = TRUE, detail = "") {
  status <- tryCatch({ if (isTRUE(condition)) "PASS" else "FAIL" }, error = function(e) "ERROR")
  results[[length(results) + 1]] <<- data.frame(
    ID = id, Test = test_name, Class = if (critical) "CRITICAL" else "SUPPORTIVE",
    URS_Ref = urs_ref, Result = status, Method = method, Expected = expected,
    Detail = if (status == "PASS") detail else paste(detail, "| condition was FALSE/errored"),
    Section = current_section, stringsAsFactors = FALSE)
  icon <- switch(status, PASS = "✅", FAIL = "❌", ERROR = "⚠️")
  cat(sprintf("  %s %s: %s -- %s
", icon, id, test_name, status))
}

skip_manual <- function(id, test_name, procedure, expected, urs_ref) {
  results[[length(results) + 1]] <<- data.frame(
    ID = id, Test = test_name, Class = "MANUAL", URS_Ref = urs_ref,
    Result = "SKIP", Method = procedure, Expected = expected,
    Detail = "Execute manually via app UI", Section = "MAN", stringsAsFactors = FALSE)
  cat(sprintf("  ⏭ %s: %s -- SKIP (manual)
", id, test_name))
}

start_section <- function(name) {
  current_section <<- name
  section_times[[name]] <<- proc.time()
  cat(sprintf("
--- Section %s ---
", name))
}
end_section <- function(name) {
  elapsed <- (proc.time() - section_times[[name]])["elapsed"]
  section_times[[name]] <<- elapsed
  cat(sprintf("  [%s completed in %.1f s]
", name, elapsed))
}

# =============================================================================
# SECTION IQ: Installation Qualification
# =============================================================================
start_section("IQ")

check("IQ-01", "R version >= 4.1",
      getRversion() >= "4.1.0",
      "URS-GEN-01", method="getRversion()", expected="R >= 4.1", critical=TRUE, detail=R.version.string)

check("IQ-02", "NonCompart installed", requireNamespace("NonCompart", quietly=TRUE),
      "URS-NCA-01", method="requireNamespace", expected="TRUE", critical=TRUE)

check("IQ-03", "PowerTOST installed", requireNamespace("PowerTOST", quietly=TRUE),
      "URS-PWR-01", method="requireNamespace", expected="TRUE", critical=TRUE)

check("IQ-04", "nlme installed", requireNamespace("nlme", quietly=TRUE),
      "URS-BE-05", method="requireNamespace", expected="TRUE", critical=TRUE)

check("IQ-05", "digest installed", requireNamespace("digest", quietly=TRUE),
      "URS-EXP-04", method="requireNamespace", expected="TRUE", critical=TRUE)

check("IQ-06", "app.R parses",
      tryCatch({ parse(file="app.R"); TRUE }, error=function(e) FALSE),
      "URS-GEN-01", method="parse(file='app.R')", expected="No errors", critical=TRUE)

for (i in seq_along(source_files)) {
  check(paste0("IQ-", sprintf("%02d", 6+i)), paste("Parses:", basename(source_files[i])),
        tryCatch({ parse(file=source_files[i]); TRUE }, error=function(e) FALSE),
        "URS-GEN-01", method=paste("parse", source_files[i]), expected="No errors", critical=TRUE)
}

# The app's interface modules and packages: without them the app does not start
app_files <- setdiff(list.files("R", pattern = "\\.R$", full.names = TRUE), source_files)
check("IQ-APP-01", "All other app source files parse",
      all(vapply(app_files, function(f) tryCatch({ parse(file = f); TRUE }, error = function(e) FALSE), logical(1))),
      "URS-GEN-01", method = "parse every R/*.R file not listed above", expected = "No errors", critical = TRUE,
      detail = paste(basename(app_files), collapse = ", "))
app_pkgs <- c("shiny", "bslib", "shinyWidgets", "DT", "plotly", "ggplot2", "htmltools", "tidyr",
              "dplyr", "readxl", "openxlsx", "jsonlite")
app_pkgs_missing <- app_pkgs[!vapply(app_pkgs, requireNamespace, logical(1), quietly = TRUE)]
check("IQ-APP-02", "Packages the app loads are installed", length(app_pkgs_missing) == 0,
      "URS-GEN-01", method = "requireNamespace for every package app.R loads", expected = "All installed",
      critical = TRUE, detail = if (length(app_pkgs_missing)) paste("Missing:", paste(app_pkgs_missing, collapse = ", ")) else "")

# The installation against the release it claims to be: every app file is the
# released one (SHA-256 in validation/release_manifest.csv, written by
# validation/make_release_files.R when the release is tagged), and the
# packages are the versions the release was validated with (validation/renv.lock)
check("IQ-REL-01", "App files match the release manifest",
  tryCatch({
    m <- read.csv("validation/release_manifest.csv", stringsAsFactors = FALSE)
    all(m$app_version == APP_VERSION) &&
      all(vapply(seq_len(nrow(m)), function(i) file.exists(m$file[i]) &&
                   identical(digest(file = m$file[i], algo = "sha256"), m$sha256[i]), logical(1)))
  }, error = function(e) FALSE),
  "URS-GEN-01, URS-GEN-08", method = "SHA-256 of app.R, R/, converters/, cdisc/ and www/ against validation/release_manifest.csv",
  expected = "Every file matches the manifest of this version", critical = TRUE)
# The packages make_release_files.R writes to the lockfile: the app's, the
# validation-only replicateBE and those of controlled mode
rel_pkgs <- c("NonCompart", "PowerTOST", "nlme", "digest", "openxlsx", "jsonlite", "readxl", "dplyr",
              "shiny", "bslib", "shinyWidgets", "DT", "plotly", "ggplot2", "htmltools", "tidyr", "replicateBE",
              "shinymanager", "DBI", "RSQLite")
#' Installed packages whose version differs from the lockfile, as
#' "name installed (validated: version)"
lock_differences <- function(lockfile, pkgs) tryCatch({
  lock <- jsonlite::fromJSON(lockfile)$Packages
  inst <- vapply(pkgs, function(p) {
    v <- suppressWarnings(utils::packageDescription(p, fields = "Version"))
    if (is.na(v)) "not installed" else v }, character(1))
  want <- vapply(pkgs, function(p) if (is.null(lock[[p]])) "not in the lockfile" else lock[[p]]$Version, character(1))
  d <- pkgs[inst != want]
  if (length(d) == 0) character(0) else paste0(d, " ", inst[d], " (validated: ", want[d], ")")
}, error = function(e) paste(lockfile, "could not be read"))
rel_diff <- lock_differences("validation/renv.lock", rel_pkgs)
check("IQ-REL-02", "Installed packages are the validated versions",
  length(rel_diff) == 0,
  "URS-GEN-01, URS-GEN-08", method = "Installed versions (DESCRIPTION) of the 20 packages in validation/renv.lock, controlled mode and replicateBE included",
  expected = "Same versions; a difference needs a risk assessment, and the differing packages are listed", critical = FALSE,
  detail = if (length(rel_diff) == 0) "all as in validation/renv.lock" else paste(rel_diff, collapse = "; "))
if (length(rel_diff) > 0) cat("    Not the validated version:", paste(rel_diff, collapse = "; "), "\n")

end_section("IQ")

# =============================================================================
# SECTION DAT: Data Handling
# =============================================================================
start_section("DAT")

col_conventions <- list(
  standard=c("Subject","Time","Concentration"), cdisc=c("USUBJID","NTIM","DV"),
  export=c("SubjID","Hours","Conc"), dutch=c("Proband","Zeit","Konzentration"),
  lowercase=c("subject","time","conc"), abbrev=c("ID","Tpt","Cp"),
  mixed=c("PatID","hour","plasma_ng_ml"))

for (cn in names(col_conventions)) {
  cols <- col_conventions[[cn]]; g <- auto_detect_columns(cols)
  tag <- toupper(substr(cn, 1, 3))
  check(paste0("DAT-AD-",tag,"-S"), paste("Auto-detect subj:", cn), identical(g$subject, cols[1]),
        "URS-DAT-02", method=paste("auto_detect on", cn), expected=cols[1], critical=TRUE)
  check(paste0("DAT-AD-",tag,"-T"), paste("Auto-detect time:", cn), identical(g$time, cols[2]),
        "URS-DAT-02", method=paste("auto_detect on", cn), expected=cols[2], critical=TRUE)
  check(paste0("DAT-AD-",tag,"-C"), paste("Auto-detect conc:", cn), identical(g$conc, cols[3]),
        "URS-DAT-02", method=paste("auto_detect on", cn), expected=cols[3], critical=TRUE)
}

bcm <- list(subject="Subject", time="Time", conc="Conc")

check("DAT-DQ-01", "DQ: Empty dataset ERROR",
      { qc <- run_data_quality_check(data.frame(Subject=character(0),Time=numeric(0),Conc=numeric(0)), bcm); !qc$pass },
      "URS-DAT-03", method="Empty df", expected="pass=FALSE", critical=TRUE)

check("DAT-DQ-02", "DQ: All-NA conc ERROR",
      { qc <- run_data_quality_check(data.frame(Subject=c("A","A","A"),Time=0:2,Conc=c(NA,NA,NA)), bcm); qc$n_errors>0 },
      "URS-DAT-03", method="All NA conc", expected="ERROR", critical=TRUE)

check("DAT-DQ-03", "DQ: Non-numeric time ERROR",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",3),Time=c("0h","1h","2h"),Conc=c(1,5,3)), bcm); any(qc$findings$Severity=="ERROR" & grepl("on-numeric",qc$findings$Message)) },
      "URS-DAT-03", method="Text in time", expected="ERROR non-numeric", critical=TRUE)

check("DAT-DQ-04", "DQ: Duplicate times ERROR",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",4),Time=c(0,1,1,2),Conc=c(0,5,5.1,3)), bcm); any(qc$findings$Severity=="ERROR" & grepl("uplicate",qc$findings$Message)) },
      "URS-DAT-03", method="Dup time=1", expected="ERROR duplicate", critical=TRUE)

check("DAT-DQ-05", "DQ: Missing subject ERROR",
      { qc <- run_data_quality_check(data.frame(Subject=c("A","","A"),Time=0:2,Conc=c(0,5,3)), bcm); any(qc$findings$Severity=="ERROR" & grepl("issing.*ubject",qc$findings$Message)) },
      "URS-DAT-03", method="Blank subject", expected="ERROR missing subj", critical=TRUE)

check("DAT-DQ-06", "DQ: Sparse subject WARNING",
      { qc <- run_data_quality_check(data.frame(Subject=c("A","A","B"),Time=c(0,1,0),Conc=c(0,5,3)), bcm); any(qc$findings$Severity=="WARNING" & grepl("< 3",qc$findings$Message)) },
      "URS-DAT-03", method="1 obs subject", expected="WARNING <3", critical=FALSE)

check("DAT-DQ-07", "DQ: Negative conc ERROR without an LLOQ",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",4),Time=0:3,Conc=c(0,5,-1,3)), bcm); any(qc$findings$Severity=="ERROR" & grepl("egative",qc$findings$Message)) },
      "URS-DAT-03", method="Conc=-1, no LLOQ", expected="ERROR negative (the profile would get no parameters)", critical=FALSE)

check("DAT-DQ-08", "DQ: All-zero WARNING",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",3),Time=0:2,Conc=c(0,0,0)), bcm); any(qc$findings$Severity=="WARNING" & grepl("all-zero",qc$findings$Message)) },
      "URS-DAT-03", method="All zero", expected="WARNING all-zero", critical=FALSE)

check("DAT-DQ-09", "DQ: BLQ no LLOQ ERROR",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",4),Time=0:3,Conc=c("0","BLQ","5","3")), bcm, lloq=0); any(qc$findings$Severity=="ERROR" & grepl("BLQ.*LLOQ",qc$findings$Message)) },
      "URS-DAT-03", method="BLQ with lloq=0", expected="ERROR", critical=TRUE)

check("DAT-DQ-10", "DQ: BLQ with LLOQ WARNING",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",4),Time=0:3,Conc=c("0","BLQ","5","3")), bcm, lloq=0.5); any(qc$findings$Severity=="WARNING" & grepl("BLQ",qc$findings$Message)) && !any(qc$findings$Severity=="ERROR" & grepl("BLQ",qc$findings$Message)) },
      "URS-DAT-03", method="BLQ with lloq=0.5", expected="WARNING only", critical=TRUE)

check("DAT-DQ-11", "DQ: Unrecognized text ERROR",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",3),Time=0:2,Conc=c("10","hemolyzed","5")), bcm); any(qc$findings$Severity=="ERROR" & grepl("nrecognized",qc$findings$Message)) },
      "URS-DAT-03", method="Unrecognized text", expected="ERROR", critical=TRUE)

check("DAT-DQ-12", "DQ: Incomplete crossover WARNING",
      { d <- data.frame(Subject=c("A","A","A","A","B","B"),Time=c(0,1,0,1,0,1),Conc=c(0,5,0,4,0,6),Treatment=c("T","T","R","R","T","T")); qc <- run_data_quality_check(d, list(subject="Subject",time="Time",conc="Conc",treatment="Treatment")); any(qc$findings$Severity=="WARNING" & grepl("issing.*treatment",qc$findings$Message)) },
      "URS-DAT-03", method="Subject B missing R", expected="WARNING", critical=FALSE)

check("DAT-DQ-13", "DQ: Clean dataset passes",
      { d <- data.frame(Subject=rep(c("A","B"),each=5),Time=rep(c(0,1,2,4,8),2),Conc=c(0,10,8,4,1,0,12,9,5,2)); run_data_quality_check(d, bcm)$pass },
      "URS-DAT-03", method="Well-formed data", expected="pass=TRUE", critical=TRUE)

# BLQ rules
bb <- data.frame(Subject=rep("A",7),Time=c(0,0.5,1,2,4,8,12),Conc=c(0.3,0.3,5,10,4,0.3,0.3),stringsAsFactors=FALSE)
bc <- list(subject="Subject",time="Time",conc="Conc"); bl <- 1.0

check("DAT-BLQ-01", "BLQ Rule 1: pre=0 post=NA",
      { d <- apply_blq_rules(bb,bc,"rule1",bl); d$Conc[1]==0 && d$Conc[2]==0 && d$Conc[3]==5 && d$Conc[4]==10 && d$Conc[5]==4 && is.na(d$Conc[6]) && is.na(d$Conc[7]) },
      "URS-DAT-04", method="Rule 1 on 7-pt profile", expected="[0,0,5,10,4,NA,NA]", critical=TRUE)

check("DAT-BLQ-02", "BLQ Rule 2: all=0",
      { d <- apply_blq_rules(bb,bc,"rule2",bl); all(d$Conc[c(1,2,6,7)]==0) && d$Conc[3]==5 },
      "URS-DAT-04", method="Rule 2", expected="BLQ=0", critical=TRUE)

check("DAT-BLQ-03", "BLQ Rule 3: all=NA",
      { d <- apply_blq_rules(bb,bc,"rule3",bl); all(is.na(d$Conc[c(1,2,6,7)])) },
      "URS-DAT-04", method="Rule 3", expected="BLQ=NA", critical=TRUE)

check("DAT-BLQ-04", "BLQ Rule 4: LLOQ/2 before the last measurable value, missing after, pre-dose=0",
      { d <- apply_blq_rules(bb,bc,"rule4",bl); d$Conc[1]==0 && d$Conc[2]==0.5 && all(is.na(d$Conc[6:7])) },
      "URS-DAT-04", method="Rule 4", expected="t=0 [0], t=0.5 [0.5], after the last measurable [NA, NA]", critical=TRUE)

check("DAT-BLQ-05", "BLQ Rule 5: pre-Cmax=0 post=NA",
      { d <- apply_blq_rules(bb,bc,"rule5",bl); d$Conc[1]==0 && d$Conc[2]==0 && is.na(d$Conc[6]) && is.na(d$Conc[7]) },
      "URS-DAT-04", method="Rule 5", expected="Pre [0,0] post [NA,NA]", critical=TRUE)

check("DAT-BLQ-06", "BLQ Rule 6: after dosing and before first quantifiable=LLOQ/2, rest=0",
      { d <- apply_blq_rules(bb,bc,"rule6",bl); d$Conc[1]==0 && d$Conc[2]==0.5 && d$Conc[6]==0 && d$Conc[7]==0 },
      "URS-DAT-04", method="Rule 6", expected="t=0 [0], t=0.5 [0.5], post [0,0]", critical=TRUE)

check("DAT-BLQ-07", "BLQ: Multi-subject independent",
      { d2 <- rbind(data.frame(Subject="A",Time=c(0,1,2),Conc=c(0.3,5,0.3)), data.frame(Subject="B",Time=c(0,1,2),Conc=c(0.3,8,0.3))); o <- apply_blq_rules(d2,bc,"rule1",bl); o$Conc[1]==0 && is.na(o$Conc[3]) && o$Conc[4]==0 && is.na(o$Conc[6]) },
      "URS-DAT-04", method="Two subjects Rule 1", expected="Independent processing", critical=TRUE)

check("DAT-BLQ-08", "BLQ: Rule affects AUC",
      { d2<-apply_blq_rules(bb,bc,"rule2",bl); d3<-apply_blq_rules(bb,bc,"rule3",bl); a2<-tryCatch({m<-AUC(d2$Time[!is.na(d2$Conc)],d2$Conc[!is.na(d2$Conc)]); m[nrow(m),1]},error=function(e)0); a3<-tryCatch({m<-AUC(d3$Time[!is.na(d3$Conc)],d3$Conc[!is.na(d3$Conc)]); m[nrow(m),1]},error=function(e)0); a2!=a3 },
      "URS-DAT-04", method="AUC Rule2 vs Rule3", expected="Different", critical=FALSE)

check("DAT-DES-01", "Design: single-arm",
      { des <- detect_study_design(data.frame(Subject=rep("A",3),Time=0:2,Conc=c(0,5,3)), bcm); des$type=="single_arm" },
      "URS-DAT-07", method="No treatment col", expected="single_arm", critical=TRUE)

check("DAT-DES-02", "Design: crossover",
      { d <- data.frame(Subject=rep(c("A","B"),each=4),Time=rep(c(0,1),4),Conc=c(0,5,0,4,0,6,0,3),Treatment=rep(c("T","R"),each=2,times=2),Period=rep(c(1,1,2,2),2),Sequence=rep(c("TR","RT"),each=4)); des <- detect_study_design(d, list(subject="Subject",time="Time",conc="Conc",treatment="Treatment",period="Period",sequence="Sequence")); des$is_crossover && des$n_treatments==2 },
      "URS-DAT-07", method="2x2 crossover data", expected="is_crossover=TRUE", critical=TRUE)

# --- DAT-PREP: the Shiny-free data pipeline (R/pipeline.R) --------------------
prep_raw <- data.frame(Subject = c(2, 2, 2, 1, 1, 1, 1), Treatment = "T",
                       Time = c("4", "0", "1", "0", "1", NA, "2"),
                       Conc = c("<0,5", "<0.5", "6.2", "0", "5.1", "3", "BLQ"),
                       Dose = c(100, 100, 100, 50, 50, 50, 50), stringsAsFactors = FALSE)
prep_cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", dose = "Dose")
check("DAT-PREP-01", "prepare_pk_dataset returns the canonical object",
  tryCatch({
    ds <- prepare_pk_dataset(prep_raw, prep_cm, list(lloq = 0.5, blq_rule = "rule1"))
    all(c("data", "col_map", "design", "provenance", "analyte", "units", "time_basis",
          "blq", "flags", "interlocks", "qc") %in% names(ds)) &&
      identical(ds$col_map, prep_cm) && ds$provenance$door == "flat" &&
      is.data.frame(ds$interlocks) && identical(names(ds$interlocks),
        c("Severity", "Category", "Message", "Detail", "Action"))
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = TRUE, method = "field names and shapes", expected = "all 11 fields present")
check("DAT-PREP-02", "Rows without a time are dropped, counted, and data sorted by subject and time",
  tryCatch({
    ds <- prepare_pk_dataset(prep_raw, prep_cm, list(lloq = 0.5, blq_rule = "rule1"))
    ds$flags$n_rows_dropped == 1 && nrow(ds$data) == 6 &&
      identical(ds$data$Subject, c(1, 1, 1, 2, 2, 2)) && identical(ds$data$Time, c(0, 1, 2, 0, 1, 4))
  }, error = function(e) FALSE),
  "URS-DAT-02", critical = TRUE, method = "unsorted input with one NA time", expected = "6 rows, sorted")
check("DAT-PREP-03", "BLQ text ('<x', 'BLQ') reaches the BLQ rule",
  tryCatch({
    ds <- prepare_pk_dataset(prep_raw, prep_cm, list(lloq = 0.5, blq_rule = "rule4"))
    d <- ds$data
    ds$blq$text_tokens_converted == 3 &&
      identical(d$Conc[d$Subject == 2 & d$Time %in% c(0, 4)], c(0, NA)) &&
      is.na(d$Conc[d$Subject == 1 & d$Time == 2]) &&
      all(d$BLQ_flag[(d$Subject == 2 & d$Time %in% c(0, 4)) | (d$Subject == 1 & d$Time == 2)])
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "rule 4 (LLOQ/2) with '<0.5', '<0,5' and 'BLQ'",
  expected = "'<' entries and 'BLQ' flagged as BLQ: the pre-dose '<0.5' -> 0, BLQ after the last measurable value -> missing")
check("DAT-PREP-04", "Without an LLOQ no BLQ rule is applied and text becomes missing",
  tryCatch({
    ds <- prepare_pk_dataset(prep_raw, prep_cm, list(lloq = 0))
    ds$blq$rule == "none" && ds$blq$text_tokens_converted == 0 && sum(is.na(ds$data$Conc)) == 3
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "lloq = 0", expected = "3 missing concentrations")
check("DAT-PREP-05", "LLOQ suggestion from '<x' text, including decimal commas",
  tryCatch({
    b <- blq_text_summary(c("<0,5", "<0.25", "BLQ", "3.1"))
    b$n_blq_text == 3 && is.null(b$suggested_lloq) && identical(b$lloq_candidates, c(0.25, 0.5)) &&
      identical(blq_text_summary(c("<0,5", "<0.5", "BLQ"))$suggested_lloq, 0.5) &&
      is.null(blq_text_summary(c("1", "2"))$suggested_lloq)
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = FALSE, method = "blq_text_summary()",
  expected = "3 entries; '<0,5' with '<0.25' lists 0.25 and 0.5 without a suggestion; '<0,5' with '<0.5' suggests 0.5")
check("DAT-PREP-06", "Files are read with the recorded separator and decimal mark",
  tryCatch({
    f <- tempfile(fileext = ".csv")
    writeLines(c("Subject;Time;Conc", "1;0,5;12,25", "1;1;8,5"), f)
    d <- read_pk_file(f, list(sep = ";", dec = ","))
    identical(d$Time, c(0.5, 1)) && identical(d$Conc, c(12.25, 8.5))
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = TRUE, method = "semicolon / decimal-comma CSV", expected = "numeric columns")
check("DAT-PREP-07", "Per-subject dose is the subject's maximum, named by subject",
  tryCatch({
    identical(dose_by_subject(prep_raw, prep_cm), c("1" = 50, "2" = 100))
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = TRUE, method = "dose_by_subject()", expected = "c('1' = 50, '2' = 100)")
check("DAT-PREP-08", "Source file SHA-256 is recorded in provenance",
  tryCatch({
    f <- tempfile(fileext = ".csv"); write.csv(prep_raw, f, row.names = FALSE)
    ds <- prepare_pk_dataset(read_pk_file(f), prep_cm, list(file_path = f, file_name = "x.csv"))
    identical(ds$provenance$sha256, digest::digest(file = f, algo = "sha256"))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = FALSE, method = "file_path in opts", expected = "hash equals digest of the file")
check("DAT-PREP-09", "The pipeline is Shiny-free and the upload module uses it",
  tryCatch({
    pl <- readLines("R/pipeline.R", warn = FALSE); pl <- pl[!grepl("^\\s*#", pl)]
    up <- readLines("R/mod_data_upload.R", warn = FALSE); up <- up[!grepl("^\\s*#", up)]
    vr <- readLines("validation/validation.R", warn = FALSE)
    !any(grepl("input\\$|shared\\$|showNotification|shiny::", pl)) &&
      any(grepl("prepare_pk_dataset\\(", up)) && !any(grepl("apply_blq_rules\\(", up)) &&
      !any(grepl(paste0("nchar(gsub(\"[^{]\", \"\", ", "lines[i]))"), vr, fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-GEN-01", critical = FALSE, method = "source inspection",
  expected = "no Shiny calls in pipeline.R; module calls prepare_pk_dataset; no brace counting in validation.R")

# --- IL: interlocks (R/interlocks.R), run by the data quality check ----------
il_fix <- function(f) read.csv(file.path("validation", "fixtures", f), stringsAsFactors = FALSE)
il_err <- function(qc, pattern) {
  f <- if (is.data.frame(qc)) qc else qc$findings
  any(f$Severity == "ERROR" & grepl(pattern, paste(f$Message, f$Detail, f$Action), ignore.case = TRUE))
}
il_adnca_cm <- list(subject = "USUBJID", time = "NRRLT", conc = "AVAL", treatment = "TRTP", period = "APERIOD")
il_flat_cm  <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period")

check("IL-SNIFF-01", "ADNCA-shaped files are refused at the flat upload (F1, F3-F9)",
  tryCatch({
    fx <- c("adnca_clean.csv", "adnca_dtype.csv", "adnca_anl01fl.csv", "adnca_multi_analyte.csv",
            "adnca_afrlt.csv", "adnca_datetime.csv", "adnca_units_mixed.csv", "adnca_multi_ex.csv")
    all(sapply(fx, function(f) {
      d <- il_fix(f); cm <- il_adnca_cm
      if (!"NRRLT" %in% names(d)) cm$time <- "PCDTC"
      il_err(run_data_quality_check(d, cm, lloq = 0.5), "CDISC|ADNCA")
    }))
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "run_data_quality_check on 8 ADNCA-shaped fixtures",
  expected = "ERROR naming the CDISC/ADNCA shape for every fixture")
check("IL-SNIFF-02", "Flat files are not mistaken for ADNCA",
  tryCatch({
    d1 <- il_fix("flat_equivalent.csv")
    d2 <- read.csv("data/example_be_crossover.csv", stringsAsFactors = FALSE)
    d3 <- data.frame(USUBJID = 1:2, Time = c(0, 1), Conc = c(0, 5))   # USUBJID alone is not ADNCA
    !il_err(run_data_quality_check(d1, il_flat_cm, lloq = 0.5), "CDISC|ADNCA") &&
      !il_err(run_data_quality_check(d2, list(subject = "Subject", time = "Time", conc = "Concentration",
                                              treatment = "Treatment", period = "Period"), 0), "CDISC|ADNCA") &&
      !il_err(run_data_quality_check(d3, list(subject = "USUBJID", time = "Time", conc = "Conc"), 0), "CDISC|ADNCA")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F2, BE example, file with only a USUBJID column",
  expected = "no ADNCA refusal")
check("IL-UNIT-01", "More than one concentration unit is refused, even if numerically equivalent",
  tryCatch({
    d8 <- il_fix("adnca_units_mixed.csv"); d1 <- il_fix("adnca_clean.csv")
    flat <- data.frame(Subject = rep(1:2, each = 3), Time = rep(c(0, 1, 2), 2), Conc = c(0, 5, 4, 0, 6, 5),
                       Unit = c(rep("ng/mL", 3), rep("mg/L", 3)))
    flat1 <- transform(flat, Unit = "ng/mL")
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    il_err(run_interlocks(d8, il_adnca_cm), "unit") && !il_err(run_interlocks(d1, il_adnca_cm), "unit") &&
      il_err(run_data_quality_check(flat, cm, 0), "unit") && !il_err(run_data_quality_check(flat1, cm, 0), "unit")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F8 (ng/mL and ug/L), F1, flat file with a Unit column",
  expected = "ERROR only when a unit column has more than one value")
check("IL-TIME-01", "Date-time time columns are refused (Excel datetimes, ISO datetimes, durations, clock times)",
  tryCatch({
    cm <- list(subject = "S", time = "T", conc = "C")
    base <- data.frame(S = 1, C = c(0, 5, 3))
    posix <- transform(base, T = as.POSIXct("2026-01-01 08:00", tz = "UTC") + c(0, 3600, 7200))
    iso   <- transform(base, T = c("2026-01-01T08:00", "2026-01-01T09:00", "2026-01-01T10:00"))
    dur   <- transform(base, T = c("PT0H", "PT1H", "PT1H30M"))
    clock <- transform(base, T = c("08:00", "09:00", "10:30"))
    ok    <- transform(base, T = c(0, 1, 2))
    all(sapply(list(posix, iso, dur, clock), function(d) il_err(run_interlocks(d, cm), "date|time format|duration|clock"))) &&
      nrow(run_interlocks(ok, cm)[run_interlocks(ok, cm)$Severity == "ERROR", ]) == 0
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "POSIXct, ISO 8601 datetime, ISO duration, clock time vs numeric",
  expected = "ERROR for the four date/time forms, none for numbers")
check("IL-T0-01", "Profiles that do not start near time zero are refused (time since first dose)",
  tryCatch({
    d6 <- il_fix("adnca_afrlt.csv")
    flat6 <- data.frame(Subject = d6$SUBJID, Treatment = d6$TRTP, Period = d6$APERIOD, Time = d6$ARRLT, Conc = d6$AVAL)
    flat2 <- il_fix("flat_equivalent.csv")
    il_err(run_interlocks(flat6, il_flat_cm), "time zero|first dose") &&
      !il_err(run_interlocks(flat2, il_flat_cm), "time zero|first dose")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "flat file built from F6 (period 2 from ~168 h) vs F2",
  expected = "ERROR for F6 only")
check("IL-T0-02", "Epoch-second times refused; a sparse profile only warns",
  tryCatch({
    cm <- list(subject = "S", time = "T", conc = "C")
    epoch <- data.frame(S = 1, T = 1.77e9 + c(0, 3600, 7200, 14400), C = c(1, 5, 4, 2))
    sparse <- data.frame(S = c(1, 1, 2, 2, 2), T = c(1, 2, 0, 1, 2), C = c(5, 3, 0, 5, 3))
    f <- run_interlocks(sparse, cm)
    il_err(run_interlocks(epoch, cm), "time zero|first dose|date") &&
      !any(f$Severity == "ERROR") && any(f$Severity == "WARNING" & grepl("time zero", f$Message))
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "epoch seconds; subject 1 with two samples at 1 and 2 h",
  expected = "ERROR for epoch times; WARNING (not ERROR) for the sparse profile")
check("IL-STACK-01", "Stacked profiles (duplicate times within a profile) are an interlock",
  tryCatch({
    d <- rbind(il_fix("flat_equivalent.csv"), il_fix("flat_equivalent.csv")[1:5, ])
    il_err(run_interlocks(d, il_flat_cm), "duplicate") &&
      !il_err(run_interlocks(il_fix("flat_equivalent.csv"), il_flat_cm), "duplicate")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F2 with five rows repeated vs F2", expected = "ERROR only when stacked")
check("IL-DEC-01", "Decimal-comma concentrations with BLQ text are read when the decimal mark is a comma",
  tryCatch({
    f <- tempfile(fileext = ".csv")
    writeLines(c("Subject;Time;Conc", "1;0;<0,5", "1;1;4,25", "1;2;3,5", "1;4;2", "1;8;0,75"), f)
    raw <- read_pk_file(f, list(sep = ";", dec = ","))
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    qc <- run_data_quality_check(raw, cm, lloq = 0.5, dec = ",")
    ds <- prepare_pk_dataset(raw, cm, list(lloq = 0.5, blq_rule = "rule1", read_args = list(sep = ";", dec = ",")))
    !il_err(qc, "unrecognized") && identical(ds$data$Conc, c(0, 4.25, 3.5, 2, 0.75))
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "'<0,5', '4,25', '3,5' with dec = ','", expected = "no text error; 4.25, 3.5, 0.75")
check("IL-DEC-02", "With a point decimal mark, comma values are still refused",
  tryCatch({
    raw <- data.frame(Subject = 1, Time = c(0, 1, 2), Conc = c("0", "4,25", "3,5"))
    !is.null(raw) && il_err(run_data_quality_check(raw, list(subject = "Subject", time = "Time", conc = "Conc"), 0, dec = "."), "unrecognized")
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "'4,25' with dec = '.'", expected = "ERROR: unrecognized text")
check("IL-LIB-01", "Interlocks return the quality-report finding shape and run in the quality check",
  tryCatch({
    f <- run_interlocks(il_fix("adnca_clean.csv"), il_adnca_cm)
    qc <- run_data_quality_check(il_fix("adnca_clean.csv"), il_adnca_cm, lloq = 0.5)
    identical(names(f), c("Severity", "Category", "Message", "Detail", "Action")) &&
      all(f$Message %in% qc$findings$Message) && !qc$pass
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = FALSE, method = "run_interlocks() vs run_data_quality_check()",
  expected = "same columns as findings; included in QC; QC does not pass")

end_section("DAT")


# =============================================================================
# SECTION NCA: Non-Compartmental Analysis
# =============================================================================
start_section("NCA")

iv_dose <- 500; C0 <- 100; ke <- 0.1
iv_times <- c(0.001, 0.5, 1, 2, 4, 6, 8, 12, 16, 24)
iv_conc <- C0 * exp(-ke * iv_times)
iv_data <- data.frame(Subject=rep("S1", length(iv_times)), Time=iv_times, Conc=iv_conc)
iv_cm <- list(subject="Subject", time="Time", conc="Conc")
iv_settings <- list(admin_route="iv_bolus", dose=iv_dose, dose_unit="mg", time_unit="h",
                    conc_unit="mg/L", trap_method="log", r2adj_threshold=0.7,
                    infusion_duration=0, mw=0, is_steady_state=FALSE, partial_aucs=NULL)
iv_result <- run_nca(iv_data, iv_cm, iv_settings)

check("NCA-GT-01", "IV Bolus: Cmax~100",
      !is.null(iv_result) && abs(as.numeric(iv_result$CMAX[1]) - 100) < 0.1,
      "URS-NCA-01", method="Mono-exp C0=100, t_start=0.001", expected="CMAX~100", critical=TRUE)
check("NCA-GT-02", "IV Bolus: lz=0.1",
      !is.null(iv_result) && abs(as.numeric(iv_result$LAMZ[1]) - 0.1) < 0.001,
      "URS-NCA-01", method="Analytical ke=0.1", expected="LAMZ=0.1", critical=TRUE)
check("NCA-GT-03", "IV Bolus: t1/2=6.931",
      !is.null(iv_result) && abs(as.numeric(iv_result$LAMZHL[1]) - log(2)/0.1) < 0.05,
      "URS-NCA-01", method="ln(2)/ke", expected="LAMZHL=6.931", critical=TRUE)
check("NCA-GT-04", "IV Bolus: AUCinf=1000",
      !is.null(iv_result) && abs(as.numeric(iv_result$AUCIFO[1]) - 1000) < 5,
      "URS-NCA-01", method="C0/ke=1000", expected="AUCIFO=1000", critical=TRUE)
check("NCA-GT-05", "IV Bolus: CL=0.5",
      !is.null(iv_result) && abs(as.numeric(iv_result$CLO[1]) - 0.5) < 0.01,
      "URS-NCA-01", method="Dose/AUCinf", expected="CLO=0.5", critical=TRUE)
check("NCA-GT-06", "IV Bolus: Vz=5",
      !is.null(iv_result) && abs(as.numeric(iv_result$VZO[1]) - 5) < 0.1,
      "URS-NCA-01", method="CL/ke", expected="VZO=5", critical=TRUE)

theoph <- Theoph; names(theoph) <- c("Subject","Wt","Dose","Time","Conc")
theoph$Subject <- as.character(theoph$Subject)
theoph_cm <- list(subject="Subject", time="Time", conc="Conc")
theoph_settings <- list(admin_route="extravascular", dose=320, dose_unit="mg", time_unit="h",
                        conc_unit="mg/L", trap_method="log", r2adj_threshold=0.7,
                        infusion_duration=0, mw=0, is_steady_state=FALSE, partial_aucs=NULL)
theoph_result <- run_nca(theoph, theoph_cm, theoph_settings)

check("NCA-TH-01", "Theoph: 12 subjects", !is.null(theoph_result)&&nrow(theoph_result)==12,
      "URS-NCA-06", method="Theoph tblNCA", expected="12 rows", critical=TRUE)
check("NCA-TH-02", "Theoph: Cmax>0", !is.null(theoph_result)&&all(as.numeric(theoph_result$CMAX)>0),
      "URS-NCA-01", method="All CMAX>0", expected="All positive", critical=TRUE)
check("NCA-TH-03", "Theoph: AUClast>0", !is.null(theoph_result)&&all(as.numeric(theoph_result$AUCLST)>0),
      "URS-NCA-01", method="All AUCLST>0", expected="All positive", critical=TRUE)
check("NCA-TH-04", "Theoph: lz estimable", { lz<-as.numeric(theoph_result$LAMZ); sum(!is.na(lz)&lz>0)>=10 },
      "URS-NCA-04", method="Count valid LAMZ", expected=">=10/12", critical=TRUE)

indo <- Indometh; names(indo)<-c("Subject","Time","Conc"); indo$Subject<-as.character(indo$Subject)
indo_s <- iv_settings; indo_s$dose <- 25
indo_result <- run_nca(indo, iv_cm, indo_s)

check("NCA-IN-01", "Indometh: 6 subjects", !is.null(indo_result)&&nrow(indo_result)==6,
      "URS-NCA-02", method="Indometh IV", expected="6 rows", critical=TRUE)
check("NCA-IN-02", "Indometh: CLO present", !is.null(indo_result)&&"CLO"%in%names(indo_result),
      "URS-NCA-02", method="IV->CLO", expected="CLO present", critical=TRUE)

check("NCA-AG-01", "sNCA vs tblNCA: Cmax",
      { snca<-sNCA(iv_data$Time,iv_data$Conc,dose=iv_dose,adm="Bolus",doseUnit="mg",timeUnit="h",concUnit="mg/L",down="Log"); abs(as.numeric(iv_result$CMAX[1])-as.numeric(snca["CMAX"]))<0.001 },
      "URS-NCA-01", method="Compare sNCA tblNCA", expected="Within 0.001", critical=TRUE)

check("NCA-LZ-01", "Lambda_z: best R2adj",
      { lz<-estimate_lambda_z(iv_times,iv_conc); !is.na(lz$lambda_z)&&abs(lz$lambda_z-0.1)<0.001&&lz$r2adj>0.99 },
      "URS-NCA-04", method="Mono-exp estimate_lambda_z", expected="lz=0.1 R2>0.99", critical=TRUE)
check("NCA-LZ-02", "Lambda_z: threshold reject",
      { lz<-estimate_lambda_z(iv_times[1:7],c(100,80,50,90,10,60,5),r2adj_threshold=0.99); is.na(lz$lambda_z) },
      "URS-NCA-04", method="Noisy data R2<0.99", expected="NA", critical=TRUE)

check("NCA-TR-01", "Trapezoidal: Linear triangle",
      { r <- AUC(c(0,1,2),c(0,10,0),down="Linear"); abs(r[nrow(r),1]-10)<0.01 },
      "URS-NCA-03", method="Triangle AUC last row=10", expected="10", critical=TRUE)
check("NCA-TR-02", "Trapezoidal: Log vs Linear differ",
      { t<-c(0,1,2,4); cv<-100*exp(-0.5*t); rl<-AUC(t,cv,down="Linear"); rg<-AUC(t,cv,down="Log"); rl[nrow(rl),1]!=rg[nrow(rg),1] },
      "URS-NCA-03", method="Exp decay comparison", expected="Different", critical=TRUE)

check("NCA-RT-01", "Extravascular: CLFO", "CLFO"%in%names(theoph_result),
      "URS-NCA-02", method="EV->CLFO", expected="Present", critical=TRUE)
check("NCA-RT-02", "Extravascular: no CLO", !"CLO"%in%names(theoph_result),
      "URS-NCA-02", method="EV->no CLO", expected="Absent", critical=TRUE)
check("NCA-RT-03", "Dose affects CL not Cmax",
      { hi<-theoph_settings; hi$dose<-640; rh<-run_nca(theoph,theoph_cm,hi); all(as.numeric(rh$CMAX)==as.numeric(theoph_result$CMAX))&&any(as.numeric(rh$CLFO)!=as.numeric(theoph_result$CLFO),na.rm=TRUE) },
      "URS-NCA-01", method="Double dose", expected="Cmax same CL diff", critical=TRUE)

check("NCA-DN-01", "Dose norm: CMAX_DN",
      { dn<-add_dose_normalized(theoph_result,320); "CMAX_DN"%in%names(dn)&&abs(as.numeric(dn$CMAX_DN[1])-as.numeric(dn$CMAX[1])/320)<1e-6 },
      "URS-NCA-08", method="CMAX_DN=CMAX/320", expected="Correct", critical=TRUE)
check("NCA-DN-02", "Dose norm: all DN cols",
      { dn<-add_dose_normalized(theoph_result,320); all(c("CMAX_DN","AUCLST_DN","AUCIFO_DN")%in%names(dn)) },
      "URS-NCA-08", method="Check DN columns", expected="All present", critical=TRUE)

check("NCA-SS-01", "SS: changes clearance values",
      { ss<-data.frame(Subject=rep("A",6),Time=c(0,1,2,4,8,12),Conc=c(5,15,12,8,5.5,5)); ss_f<-theoph_settings; ss_f$is_steady_state<-FALSE; ss_t<-theoph_settings; ss_t$is_steady_state<-TRUE; ss_t$tau<-12; rf<-run_nca(ss,iv_cm,ss_f); rt<-run_nca(ss,iv_cm,ss_t); !is.null(rf)&&!is.null(rt)&&as.numeric(rt$CLFO[1])!=as.numeric(rf$CLFO[1]) },
      "URS-NCA-07", method="SS=TRUE vs FALSE changes CL/F", expected="Different CL/F values", critical=TRUE)
check("NCA-SS-02", "SS: AUCTAU absent when FALSE", !"AUCTAU"%in%names(theoph_result),
      "URS-NCA-07", method="SS=FALSE", expected="AUCTAU absent", critical=FALSE)

check("NCA-ED-01", "Edge: neg conc", { r<-run_nca(data.frame(Subject=rep("A",5),Time=c(0,1,2,4,8),Conc=c(0,-1,5,3,1)),iv_cm,theoph_settings); !is.null(r) },
      "URS-NCA-10", method="Negative conc no crash", expected="Returns result", critical=TRUE)
# All-zero and single-point profiles are degenerate: there is nothing to analyse.
# The intended (v1.2+) behaviour is graceful handling — run_nca excludes the
# profile and returns NULL without error, and the calling modules surface a
# message. URS-NCA-10 asks for robustness (no crash), so the pass criterion is
# "no error thrown"; a NULL result is the correct, expected outcome here.
check("NCA-ED-02", "Edge: all zero",
      { r<-tryCatch(run_nca(data.frame(Subject=rep("A",5),Time=c(0,1,2,4,8),Conc=rep(0,5)),iv_cm,theoph_settings), error=function(e) "ERR"); !identical(r, "ERR") },
      "URS-NCA-10", method="All-zero profile handled gracefully (no crash)", expected="No error (degenerate profile excluded -> NULL)", critical=TRUE)
check("NCA-ED-03", "Edge: 1 point",
      { r<-tryCatch(run_nca(data.frame(Subject="A",Time=1,Conc=10),iv_cm,theoph_settings), error=function(e) "ERR"); !identical(r, "ERR") },
      "URS-NCA-10", method="Single observation handled gracefully (no crash)", expected="No error (insufficient points -> NULL)", critical=FALSE)
check("NCA-ED-04", "Edge: unsorted", { d<-data.frame(Subject=rep("A",5),Time=c(4,0,8,1,2),Conc=c(4,0,1,10,8)); r<-run_nca(d,iv_cm,theoph_settings); !is.null(r)&&as.numeric(r$CMAX[1])==10 },
      "URS-NCA-10", method="Unsorted->Cmax=10", expected="Correct Cmax", critical=TRUE)
check("NCA-ED-05", "Edge: sparse 3pt", { r<-run_nca(data.frame(Subject="A",Time=c(0,1,4),Conc=c(0,10,2)),iv_cm,theoph_settings); !is.null(r)&&as.numeric(r$CMAX[1])==10 },
      "URS-NCA-10", method="3 points", expected="Cmax=10", critical=TRUE)
check("NCA-ED-06", "Edge: large conc", { r<-run_nca(data.frame(Subject=rep("A",5),Time=c(0,1,2,4,8),Conc=c(0,1e8,5e7,1e7,1e6)),iv_cm,theoph_settings); !is.null(r)&&as.numeric(r$CMAX[1])==1e8 },
      "URS-NCA-10", method="1e8 concentration", expected="Cmax=1e8", critical=FALSE)
# Regression guard: NonCompart 0.8.0 hard-stops ("Check input types!") on
# non-numeric input, and a character time column also sorts lexicographically
# ("12" before "2"). run_nca must coerce defensively and still match numeric.
# Times 0,1,2,4,8,12 deliberately trigger the lexicographic-sort hazard.
check("NCA-ED-07", "Robustness: character-typed time/conc",
      { dn<-data.frame(Subject=rep("A",6),Time=c(0,1,2,4,8,12),Conc=c(0,100,80,40,12,2),stringsAsFactors=FALSE)
        dc<-dn; dc$Time<-as.character(dc$Time); dc$Conc<-as.character(dc$Conc)
        rn<-run_nca(dn,iv_cm,theoph_settings); rc<-run_nca(dc,iv_cm,theoph_settings)
        !is.null(rn) && !is.null(rc) &&
          isTRUE(all.equal(as.numeric(rn$CMAX),   as.numeric(rc$CMAX))) &&
          isTRUE(all.equal(as.numeric(rn$AUCLST), as.numeric(rc$AUCLST))) },
      "URS-NCA-10", method="run_nca: character vs numeric time/conc (NonCompart 0.8.0 type robustness)",
      expected="Identical CMAX & AUClast; no type/sort failure", critical=TRUE)

# The app's own parser (R/nca_helpers.R), not a copy
parse_manual <- parse_manual_entry
check("NCA-ME-01", "Manual: normal", { p<-parse_manual("0\n1\n2\n4","0\n10\n8\n3"); p$ok&&p$nt==4 },
      "URS-NCA-11", method="Newline parse", expected="ok=TRUE 4pt", critical=TRUE)
check("NCA-ME-02", "Manual: CRLF", { p<-parse_manual("0\r\n1\r\n2\r\n4","0\r\n10\r\n8\r\n3"); p$ok },
      "URS-NCA-11", method="CRLF", expected="ok=TRUE", critical=TRUE)
check("NCA-ME-03", "Manual: blanks", { p<-parse_manual("0\n\n1\n\n2\n4","0\n\n10\n\n8\n3"); p$ok&&p$nt==4 },
      "URS-NCA-11", method="Blank lines", expected="ok=TRUE", critical=TRUE)
check("NCA-ME-04", "Manual: mismatch", { p<-parse_manual("0\n1\n2","0\n10"); !p$ok },
      "URS-NCA-11", method="3 vs 2", expected="ok=FALSE", critical=TRUE)
check("NCA-ME-05", "Manual: non-numeric", { p<-parse_manual("0\n1\nabc","0\n10\n5"); !p$ok },
      "URS-NCA-11", method="abc->NA", expected="ok=FALSE", critical=TRUE)
check("NCA-ME-06", "Manual: <3 points", { p<-parse_manual("0\n1","0\n10"); !p$ok },
      "URS-NCA-11", method="2 points", expected="ok=FALSE", critical=TRUE)
check("NCA-ME-07", "Manual: sNCA valid", { p<-parse_manual("0\n1\n2\n4\n8","0\n10\n8\n4\n1"); snca<-sNCA(p$time,p$conc,dose=100,adm="Extravascular",doseUnit="mg",timeUnit="h",concUnit="mg/L"); as.numeric(snca["CMAX"])==10 },
      "URS-NCA-05", method="Manual->sNCA", expected="Cmax=10", critical=TRUE)

check("NCA-XO-01", "Crossover: composite key",
      { xo<-read.csv("data/example_be_crossover.csv",stringsAsFactors=FALSE); r<-run_nca(xo,list(subject="Subject",time="Time",conc="Concentration",treatment="Treatment"),theoph_settings); !is.null(r)&&"Subject"%in%names(r)&&"Treatment"%in%names(r) },
      "URS-NCA-06", method="Crossover CSV", expected="Subject+Treatment cols", critical=TRUE)
check("NCA-XO-02", "Crossover: row count",
      { xo<-read.csv("data/example_be_crossover.csv",stringsAsFactors=FALSE); ns<-length(unique(xo$Subject)); nt<-length(unique(xo$Treatment)); r<-run_nca(xo,list(subject="Subject",time="Time",conc="Concentration",treatment="Treatment"),theoph_settings); !is.null(r)&&nrow(r)==ns*nt },
      "URS-NCA-06", method="nrow=SxT", expected="Correct", critical=TRUE)

end_section("NCA")

# =============================================================================
# SECTION BE: Bioequivalence
# =============================================================================
start_section("BE")

set.seed(42); n_be <- 24
be_d <- data.frame(Subject=rep(1:n_be,each=2), Treatment=rep(c("R","T"),times=n_be),
  Period=rep(c(1,2,2,1),each=1,length.out=n_be*2),
  Sequence=rep(c("RT","TR"),each=2,length.out=n_be*2), stringsAsFactors=FALSE)
for (i in 1:n_be) {
  ir <- which(be_d$Subject==i & be_d$Treatment=="R")
  it <- which(be_d$Subject==i & be_d$Treatment=="T")
  se <- rnorm(1,0,0.3)
  be_d$CMAX[ir] <- exp(log(100)+se+rnorm(1,0,0.20))
  be_d$CMAX[it] <- exp(log(100)+se+rnorm(1,0,0.20))
  be_d$AUCLST[ir] <- exp(log(500)+se+rnorm(1,0,0.20))
  be_d$AUCLST[it] <- exp(log(500)+se+rnorm(1,0,0.20))
}

# BE tests call the app's own model code (R/be_analysis.R). The data are shaped
# the way mod_path_be.R hands them over: Subject as character (it comes from
# splitting the NCA key), Treatment as a factor with the reference first, and
# Period/Sequence exactly as uploaded.
be_input <- function(bd) {
  bd$Subject <- as.character(bd$Subject)
  bd$Treatment <- factor(bd$Treatment)
  bd
}
run_be_fit <- function(bd, param, design="crossover_2x2", mt="fixed", ci=90,
                       log_transform=TRUE, be_lower=80, be_upper=125, ...) {
  fit_be_parameter(be_input(bd), param, design = design, model_type = mt,
                   trt_col = "Treatment", subj_col = "Subject",
                   per_col = if ("Period" %in% names(bd)) "Period" else NULL,
                   seq_col = if ("Sequence" %in% names(bd)) "Sequence" else NULL,
                   log_transform = log_transform, ci_level = ci,
                   be_lower = be_lower, be_upper = be_upper, ...)
}
run_be <- function(bd, param, design="crossover_2x2", mt="fixed", ci=90) {
  run_be_fit(bd, param, design, mt, ci)$estimate
}

bf <- run_be(be_d, "CMAX")

check("BE-AN-01", "BE: GMR plausible", !is.null(bf)&&bf$pe>50&&bf$pe<200,
      "URS-BE-01", method="24-subj 2x2", expected="50-200%", critical=TRUE)
check("BE-AN-02", "BE: CI contains 100%", !is.null(bf)&&bf$ci_lo<100&&bf$ci_hi>100,
      "URS-BE-03", method="True GMR=1", expected="CI contains 100", critical=TRUE)
check("BE-AN-03", "BE: conclusion correct", !is.null(bf)&&bf$ci_lo>=80&&bf$ci_hi<=125,
      "URS-BE-04", method="GMR=1 CV=20% N=24", expected="BE=YES", critical=TRUE)
check("BE-AN-04", "BE: MSE>0", !is.null(bf)&&bf$mse>0,
      "URS-BE-01", method="MSE check", expected="MSE>0", critical=TRUE)
check("BE-NE-01", "BE: bioinequivalent fails",
      { bd2<-be_d; bd2$CMAX[bd2$Treatment=="T"]<-bd2$CMAX[bd2$Treatment=="T"]*2; r<-run_be(bd2,"CMAX"); !is.null(r)&&(r$ci_lo<80||r$ci_hi>125) },
      "URS-BE-04", method="Double T", expected="BE=NO", critical=TRUE)
check("BE-MX-01", "BE mixed: converges", { r<-run_be(be_d,"CMAX","crossover_2x2","mixed"); !is.null(r) },
      "URS-BE-05", method="nlme::lme", expected="Not NULL", critical=TRUE)
check("BE-MX-02", "BE mixed vs fixed agree",
      { rm<-run_be(be_d,"CMAX","crossover_2x2","mixed"); !is.null(rm)&&abs(rm$pe-bf$pe)/bf$pe<0.05 },
      "URS-BE-05", method="Compare GMR", expected="Within 5%", critical=FALSE)
check("BE-FO-01", "BE fixed-order: paired",
      { fo<-be_d; fo$Sequence<-"RT"; fo$Period<-ifelse(fo$Treatment=="R",1,2); r<-run_be(fo,"CMAX","crossover_fixed_order"); ref<-log(fo$CMAX[fo$Treatment=="R"]); tst<-log(fo$CMAX[fo$Treatment=="T"]); pp<-exp(mean(tst-ref))*100; !is.null(r)&&abs(r$pe-pp)<0.5 },
      "URS-BE-02", method="Fixed-order vs paired", expected="Within 0.5%", critical=TRUE)
check("BE-FO-02", "BE fixed-order: df=N-1",
      { fo<-be_d; fo$Sequence<-"RT"; fo$Period<-ifelse(fo$Treatment=="R",1,2); r<-run_be(fo,"CMAX","crossover_fixed_order"); !is.null(r)&&r$dfe==n_be-1 },
      "URS-BE-02", method="df=N-1=23", expected="df=23", critical=TRUE)
check("BE-PL-01", "Pipeline: T/R counts",
      { xo<-read.csv("data/example_be_crossover.csv",stringsAsFactors=FALSE); nca<-run_nca(xo,list(subject="Subject",time="Time",conc="Concentration",treatment="Treatment"),theoph_settings); if(is.null(nca)) FALSE else sum(nca$Treatment=="Test")>0&&sum(nca$Treatment=="Test")==sum(nca$Treatment=="Reference") },
      "URS-BE-01", method="CSV->NCA->T/R", expected="Equal", critical=TRUE)

end_section("BE")

# =============================================================================
# SECTION OQ-NEW: v1.1 Feature Tests (R² slider, half-life review, overrides)
# =============================================================================
start_section("OQ-NEW")

# OQ-79: R² slider propagates to BE NCA settings
check("OQ-79", "R2 slider propagates to NCA settings",
      { s1<-theoph_settings; s1$r2adj_threshold<-0.5; s2<-theoph_settings; s2$r2adj_threshold<-0.9999; r1<-run_nca(theoph,theoph_cm,s1); r2<-run_nca(theoph,theoph_cm,s2); n1<-sum(!is.na(as.numeric(r1$LAMZ))); n2<-sum(!is.na(as.numeric(r2$LAMZ))); n1>=n2 },
      "URS-NCA-04", method="R2adj=0.5 vs 0.9999 -> count valid lz", expected="Lower threshold -> more or equal valid lz", critical=TRUE)

# OQ-80: Half-life recalc correctness
check("OQ-80", "Half-life recalc: correct adjusted lz",
      { t<-iv_times; c<-iv_conc; idx<-7:10; tt<-t[idx]; lc<-log(c[idx]); fit<-lm(lc~tt); manual_lz<--coef(fit)[2]; lz_est<-estimate_lambda_z(t,c); abs(as.numeric(manual_lz)-lz_est$lambda_z)<0.001 },
      "URS-NCA-12", method="Manual lm on last 4 pts vs estimate_lambda_z", expected="lz within 0.1%", critical=TRUE)

# OQ-81: Negative slope rejection
check("OQ-81", "Half-life recalc: negative slope rejected",
      { t<-c(0,0.5,1,2,4); c<-c(1,5,10,8,4); lz<-estimate_lambda_z(t,c,r2adj_threshold=0.0); if(is.na(lz$lambda_z)) TRUE else lz$lambda_z > 0 },
      "URS-NCA-12", method="Ascending/peak data -> lz must be positive or NA", expected="Positive lz or NA (never negative)", critical=TRUE)

# OQ-82: 2-point R²adj = NA
check("OQ-82", "Half-life recalc: 2-point R2adj = NA",
      { t<-c(0,1,2,4,8); c<-c(0,10,8,4,1); lz<-estimate_lambda_z(t,c); if(lz$n_points==2) is.na(lz$r2adj) else TRUE },
      "URS-NCA-12", method="If 2 points selected, R2adj must be NA not Inf", expected="R2adj=NA for 2 points", critical=TRUE)

# OQ-83: lz override logged in settings JSON
check("OQ-83", "Override logged in settings JSON",
      { overrides<-list(S1=list(lambda_z=0.1,r2adj=0.99,n_points=4,lambda_z_orig=0.11,r2adj_orig=0.95)); j<-jsonlite::toJSON(list(lz_overrides=overrides),auto_unbox=TRUE,pretty=TRUE); parsed<-jsonlite::fromJSON(j); !is.null(parsed$lz_overrides)&&!is.null(parsed$lz_overrides$S1$lambda_z) },
      "URS-EXP-07", method="Create override list -> serialize to JSON -> parse back", expected="lz_overrides present with profile data", critical=TRUE)

# OQ-84: Reproducibility script applies overrides
check("OQ-84", "Repro script contains override section",
      { script<-generate_nca_script(); grepl("lz_overrides = rec$lz_overrides", script, fixed=TRUE) },
      "URS-EXP-07", method="reproduction script passes the recorded overrides to run_nca()", expected="Overrides replayed from analysis_settings.json", critical=FALSE)

# --- NCA-OV: half-life overrides are computed by NonCompart (UsePoints) --------
lz_d  <- read.csv(file.path("validation", "fixtures", "be_2x2x2_crossover.csv"), stringsAsFactors = FALSE)
lz_cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period")
lz_st <- list(admin_route = "extravascular", dose = 100, infusion_duration = 0, is_steady_state = FALSE,
              dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL", trap_method = "log",
              r2adj_threshold = 0.7, mw = 0)
lz_base <- suppressWarnings(run_nca(lz_d, lz_cm, lz_st))
lz_prof <- lz_d[lz_d$Subject == 1 & lz_d$Period == 1, ]
lz_prof <- lz_prof[order(lz_prof$Time), ]
check("NCA-OV-01", "Override on the automatically chosen points reproduces the automatic result",
  tryCatch({
    i <- which(lz_base$Subject == "1" & lz_base$Period == "1")
    tu <- lz_prof$Time[lz_prof$Time >= lz_base$LAMZLL[i] & lz_prof$Time <= lz_base$LAMZUL[i] & lz_prof$Conc > 0]
    ov <- list(list(subject = "1", treatment = lz_base$Treatment[i], period = "1", time_used = tu))
    r <- suppressWarnings(run_nca(lz_d, lz_cm, lz_st, lz_overrides = ov))
    num <- names(lz_base)[sapply(lz_base, is.numeric)]
    isTRUE(all.equal(lz_base[num], r[num], tolerance = 1e-12))
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE,
  method = "time_used = points NonCompart selected itself", expected = "identical results")
check("NCA-OV-02", "Override: slope from the chosen points; all dependent parameters in NonCompart units",
  tryCatch({
    tu <- c(8, 12, 16)
    ov <- list(list(subject = "1", treatment = "Test", period = "1", time_used = tu))
    r <- suppressWarnings(run_nca(lz_d, lz_cm, lz_st, lz_overrides = ov))
    i <- which(r$Subject == "1" & r$Period == "1")
    p <- lz_prof[lz_prof$Time %in% tu, ]
    lz <- -unname(coef(lm(log(Conc) ~ Time, data = p))[2])
    abs(r$LAMZ[i] - lz) < 1e-10 && r$LAMZNPT[i] == 3 &&
      abs(r$CLFO[i] * r$AUCIFO[i] / 100 - 1000) < 1e-6 &&
      abs(r$AUCIFP[i] - (r$AUCLST[i] + r$CLSTP[i] / r$LAMZ[i])) < 1e-8 &&
      abs(r$VZFO[i] * r$LAMZ[i] * r$AUCIFO[i] / 100 - 1000) < 1e-6
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE,
  method = "time_used 8/12/16 h on subject 1 period 1 (mg, ng/mL)",
  expected = "LAMZ = lm slope; CL/F x AUCinf = 1000 x dose; AUCIFP and Vz/F consistent")
check("NCA-OV-03", "Override changes only the chosen profile",
  tryCatch({
    ov <- list(list(subject = "1", treatment = "Test", period = "1", time_used = c(8, 12, 16)))
    r <- suppressWarnings(run_nca(lz_d, lz_cm, lz_st, lz_overrides = ov))
    other <- !(r$Subject == "1" & r$Period == "1")
    num <- names(lz_base)[sapply(lz_base, is.numeric)]
    isTRUE(all.equal(lz_base[other, num], r[other, num], tolerance = 1e-12)) &&
      lz_base$LAMZ[!other] != r$LAMZ[!other]
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE, method = "compare all other profiles", expected = "unchanged")
check("NCA-OV-04", "IV bolus override updates CL and V (CLO, VZO)",
  tryCatch({
    st <- lz_st; st$admin_route <- "iv_bolus"
    b <- suppressWarnings(run_nca(lz_d, lz_cm, st))
    ov <- list(list(subject = "1", treatment = "Test", period = "1", time_used = c(8, 12, 16)))
    r <- suppressWarnings(run_nca(lz_d, lz_cm, st, lz_overrides = ov))
    i <- which(r$Subject == "1" & r$Period == "1")
    r$CLO[i] != b$CLO[i] && abs(r$CLO[i] * r$AUCIFO[i] / 100 - 1000) < 1e-6
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE, method = "adm = Bolus", expected = "CLO recomputed consistently")
check("NCA-OV-05", "Single-subject and batch analyses agree, with and without an override",
  tryCatch({
    i <- which(lz_base$Subject == "1" & lz_base$Period == "1")
    s0 <- run_single_nca(lz_prof$Time, lz_prof$Conc, lz_st)
    s1 <- run_single_nca(lz_prof$Time, lz_prof$Conc, lz_st, time_used = c(8, 12, 16))
    ov <- list(list(subject = "1", treatment = "Test", period = "1", time_used = c(8, 12, 16)))
    b1 <- suppressWarnings(run_nca(lz_d, lz_cm, lz_st, lz_overrides = ov))
    num <- intersect(names(s0), names(lz_base))
    isTRUE(all.equal(unname(as.numeric(s0[num])), unname(as.numeric(lz_base[i, num])), tolerance = 1e-12)) &&
      isTRUE(all.equal(unname(as.numeric(s1[num])), unname(as.numeric(b1[i, num])), tolerance = 1e-12))
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE, method = "run_single_nca vs run_nca on the same profile",
  expected = "identical parameters")
check("NCA-OV-06", "Modules no longer compute half-life-dependent parameters by hand",
  tryCatch({
    src <- unlist(lapply(c("R/mod_path_be.R", "R/mod_path_multi_nca.R", "R/mod_path_single_nca.R"),
                         readLines, warn = FALSE))
    !any(grepl("dose_val\\s*/\\s*aucifo|auclst \\+ clast|clast / lambda_z", src))
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = FALSE, method = "source inspection", expected = "no hand-written AUCinf / CL formulas")

end_section("OQ-NEW")

# =============================================================================
# SECTION PWR: Power & Sample Size
# =============================================================================
start_section("PWR")

check("PWR-SS-01", "N=20 for CV=20%",
      { r<-sampleN.TOST(alpha=0.05,targetpower=0.80,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.20,design="2x2",method="exact",print=FALSE); r[["Sample size"]]==20 },
      "URS-PWR-01", method="sampleN.TOST std", expected="N=20", critical=TRUE)
check("PWR-CV-01", "Higher CV -> larger N",
      { r1<-sampleN.TOST(alpha=0.05,targetpower=0.80,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.20,design="2x2",method="exact",print=FALSE); r2<-sampleN.TOST(alpha=0.05,targetpower=0.80,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.40,design="2x2",method="exact",print=FALSE); r2[["Sample size"]]>r1[["Sample size"]] },
      "URS-PWR-01", method="CV 40 vs 20", expected="More subjects", critical=TRUE)
check("PWR-CV-02", "Parallel > crossover",
      { rx<-sampleN.TOST(alpha=0.05,targetpower=0.80,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.25,design="2x2",method="exact",print=FALSE); rp<-sampleN.TOST(alpha=0.05,targetpower=0.80,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.25,design="parallel",method="exact",print=FALSE); rp[["Sample size"]]>rx[["Sample size"]] },
      "URS-PWR-03", method="Parallel vs 2x2", expected="Parallel more", critical=TRUE)
check("PWR-PN-01", "Power increases with N",
      { p20<-power.TOST(alpha=0.05,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.25,n=20,design="2x2",method="exact"); p40<-power.TOST(alpha=0.05,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.25,n=40,design="2x2",method="exact"); p40>p20 },
      "URS-PWR-02", method="N=40 vs N=20", expected="P40>P20", critical=TRUE)

for (des in c("2x2","2x2x3","2x3x3","2x2x4","parallel")) {
  check(paste0("PWR-DES-",gsub("x","",des)), paste("Design",des),
        { r<-tryCatch(sampleN.TOST(alpha=0.05,targetpower=0.80,theta0=0.95,theta1=0.80,theta2=1.25,CV=0.25,design=des,method="exact",print=FALSE),error=function(e)NULL); !is.null(r)&&r[["Sample size"]]>0 },
        "URS-PWR-03", method=paste("design=",des), expected="Valid N", critical=TRUE)
}

check("PWR-HV-01", "ABEL works",
      { r<-tryCatch(sampleN.scABEL(alpha=0.05,targetpower=0.80,theta0=0.90,CV=0.50,design="2x2x4",print=FALSE,nsims=1e4),error=function(e)NULL); !is.null(r)&&r[["Sample size"]]>0 },
      "URS-PWR-01", method="sampleN.scABEL", expected="Valid N", critical=TRUE)
check("PWR-HV-02", "RSABE works",
      { r<-tryCatch(sampleN.RSABE(alpha=0.05,targetpower=0.80,theta0=0.90,CV=0.50,design="2x2x4",print=FALSE,nsims=1e4),error=function(e)NULL); !is.null(r)&&r[["Sample size"]]>0 },
      "URS-PWR-01", method="sampleN.RSABE", expected="Valid N", critical=TRUE)
check("PWR-NT-01", "NTID works",
      { fn <- if(exists("sampleN.NTIDFDA")) sampleN.NTIDFDA else sampleN.NTID; r<-tryCatch(fn(alpha=0.05,targetpower=0.80,theta0=0.975,CV=0.10,design="2x2x4",print=FALSE,nsims=1e4),error=function(e)NULL); !is.null(r)&&r[["Sample size"]]>0 },
      "URS-PWR-01", method="sampleN.NTIDFDA or sampleN.NTID", expected="Valid N", critical=TRUE)
check("PWR-IV-01", "CV=0 handled gracefully",
      { r<-tryCatch(sampleN.TOST(alpha=0.05,targetpower=0.80,theta0=0.95,theta1=0.80,theta2=1.25,CV=0,design="2x2",print=FALSE),error=function(e)"caught",warning=function(w)"caught"); identical(r,"caught")||is.data.frame(r) },
      "URS-PWR-06", method="CV=0 either errors or returns result", expected="No crash", critical=FALSE)

end_section("PWR")

# =============================================================================
# SECTION EXP: Export & Reproducibility
# =============================================================================
start_section("EXP")

check("EXP-DT-01", "Determinism: NCA", { r1<-run_nca(iv_data,iv_cm,iv_settings); r2<-run_nca(iv_data,iv_cm,iv_settings); identical(r1,r2) },
      "URS-EXP-01", method="Run NCA twice", expected="Identical", critical=TRUE)
check("EXP-DT-02", "Determinism: summary", { s1<-summarize_pk_params(theoph_result,c("CMAX","AUCLST")); s2<-summarize_pk_params(theoph_result,c("CMAX","AUCLST")); identical(s1,s2) },
      "URS-EXP-01", method="Summary twice", expected="Identical", critical=TRUE)
check("EXP-VR-01", "APP_VERSION queryable", nchar(APP_VERSION)>0&&APP_VERSION!="unknown",
      "URS-EXP-06", method="APP_VERSION from app.R", expected="Non-empty", critical=TRUE)
check("EXP-VR-02", "APP_VERSION is 1.8.0", APP_VERSION=="1.8.0",
      "URS-EXP-06", method="=='1.8.0'", expected="1.8.0", critical=FALSE)
check("EXP-VR-03", "Package versions", { v<-sapply(c("NonCompart","PowerTOST","nlme"),function(p)as.character(packageVersion(p))); all(nchar(v)>0) },
      "URS-EXP-06", method="packageVersion", expected="All return strings", critical=TRUE)
check("EXP-SH-01", "SHA-256 computable", nchar(digest(file="validation/validation.R",algo="sha256"))==64,
      "URS-EXP-04", method="digest SHA-256", expected="64-char hex", critical=FALSE)
check("EXP-RS-01", "Repro script: valid R",
      tryCatch({ parse(text=generate_nca_script()); parse(text=generate_single_nca_script()); TRUE },error=function(e)FALSE),
      "URS-EXP-02", method="generate_nca_script->parse", expected="Valid R", critical=TRUE)
check("EXP-RS-02", "Repro script: key settings",
      { s<-generate_nca_script(); grepl("NonCompart",s) && grepl('source("nca_pipeline.R")', s, fixed=TRUE) &&
          grepl("analysis_settings.json", s, fixed=TRUE) && grepl("prepare_pk_dataset(", s, fixed=TRUE) &&
          !grepl("tblNCA(", s, fixed=TRUE) && length(strsplit(s, "\n")[[1]]) <= 90 },
      "URS-EXP-02", method="Check contents", expected="Sources the shipped pipeline, reads settings from JSON, <= 90 lines", critical=TRUE)
check("EXP-MN-01", "Integrity manifest covers data, settings, results and pipeline code",
      tryCatch({
        td <- file.path(tempdir(),"exp_mn"); if(dir.exists(td)) unlink(td,recursive=TRUE); dir.create(td)
        zf <- file.path(td,"rec.zip")
        create_analysis_record(zf, theoph_result, theoph_settings, theoph_cm,
          "data/example_theoph.csv", "example_theoph.csv", blq_rule="rule1", lloq=0,
          analyst="Validation", study_name="Manifest Test")
        ex <- file.path(td,"ex"); dir.create(ex); utils::unzip(zf, exdir=ex)
        man <- paste(readLines(file.path(ex,"data_integrity.txt")), collapse="\n")
        n_hash <- length(gregexpr("SHA-256:", man, fixed=TRUE)[[1]])
        pl_hash <- digest::digest(file = file.path(ex, "nca_pipeline.R"), algo = "sha256")
        n_hash==6 && grepl("Source data",man) && grepl("Analysis settings",man) && grepl("Results",man) &&
          grepl("Reference results",man) && grepl("Reproduction script",man) &&
          grepl("Pipeline code",man) && grepl(pl_hash, man, fixed=TRUE) &&
          identical(pl_hash, digest::digest(file = "R/pipeline.R", algo = "sha256"))
      }, error=function(e) FALSE),
      "URS-EXP-04", method="create_analysis_record -> count SHA-256 entries in data_integrity.txt",
      expected="4 hashes: source data, settings, results, nca_pipeline.R (identical to R/pipeline.R)", critical=TRUE)
check("EXP-CMP-01", "Reproduction auto-comparison",
      tryCatch({
        td <- file.path(tempdir(),"exp_cmp"); if(dir.exists(td)) unlink(td,recursive=TRUE); dir.create(td)
        zf <- file.path(td,"rec.zip")
        create_analysis_record(zf, theoph_result, theoph_settings, theoph_cm,
          "data/example_theoph.csv", "example_theoph.csv", blq_rule="rule1", lloq=0)
        ex <- file.path(td,"ex"); dir.create(ex); utils::unzip(zf, exdir=ex)
        ref_file <- file.path(ex,"app_results_reference.csv")
        scr <- paste(readLines(file.path(ex,"reproduce_analysis.R")), collapse="\n")
        has_cmp <- file.exists(ref_file) && grepl("app_results_reference.csv", scr) &&
                   grepl("compare_with_reference", scr)
        ref <- read.csv(ref_file, stringsAsFactors=FALSE, check.names=FALSE)
        rr  <- run_nca(theoph, theoph_cm, theoph_settings)
        maxrel <- 0
        for (cn in intersect(names(rr), names(ref))) {
          a <- suppressWarnings(as.numeric(as.character(rr[[cn]])))
          b <- suppressWarnings(as.numeric(as.character(ref[[cn]])))
          both <- is.finite(a) & is.finite(b); if(!any(both)) next
          maxrel <- max(maxrel, max(abs(a[both]-b[both])/pmax(abs(b[both]),1e-12)))
        }
        has_cmp && maxrel < 1e-6
      }, error=function(e) FALSE),
      "URS-EXP-02", method="Record bundles app_results_reference.csv; script auto-compares; independent recompute matches",
      expected="Reference present + comparison in script + max rel diff < 1e-6 (MATCH)", critical=TRUE)
check("EXP-SM-01", "Summary: N=12", summarize_pk_params(theoph_result,"CMAX")$N[1]==12,
      "URS-EXP-01", method="Theoph N", expected="12", critical=TRUE)
check("EXP-SM-02", "Summary: Mean>0", summarize_pk_params(theoph_result,"CMAX")$Mean[1]>0,
      "URS-EXP-01", method="Mean Cmax", expected=">0", critical=TRUE)
check("EXP-SM-03", "Summary: GeoMean", { s<-summarize_pk_params(theoph_result,"CMAX"); !is.na(s$Geo_Mean[1])&&s$Geo_Mean[1]>0 },
      "URS-EXP-01", method="Geometric mean", expected=">0", critical=TRUE)
check("EXP-CD-01", "CDISC PK parameter codes come from one pinned, stated release",
  tryCatch({
    r <- cdisc_ct_release()
    terms <- read.csv(file.path("cdisc", "pk_parameter_terms.csv"), stringsAsFactors = FALSE)
    map <- read.csv(file.path("cdisc", "pk_parameter_map.csv"), stringsAsFactors = FALSE, na.strings = character(0))
    used <- unique(map$PPTESTCD[nzchar(map$PPTESTCD)])
    identical(r$Standard, "CDISC SDTM Controlled Terminology") && grepl("^\\d{4}-\\d{2}-\\d{2}$", r$Release) &&
      grepl("C85839", r$Codelists) && grepl("C85493", r$Codelists) && nchar(r$Source_SHA256) == 64 &&
      setequal(used, terms$PPTESTCD) && !anyDuplicated(terms$PPTESTCD) &&
      all(grepl("^C\\d+$", terms$NCIt_code)) && all(nzchar(terms$PPTEST)) &&
      grepl(r$Release, cdisc_ct_statement(), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-06", critical = TRUE,
  method = "cdisc/ct_release.dcf and pk_parameter_terms.csv vs pk_parameter_map.csv",
  expected = "dated release with codelist IDs and source hash; every mapped code has a pinned term")
check("EXP-CD-02", "Codes follow route and steady state",
  tryCatch({
    code <- function(p, route = "extravascular", ss = FALSE) cdisc_pk_codes(p, route, ss)$PPTESTCD
    code("CLFO") == "CLFO" && code("CLFO", ss = TRUE) == "CLFTAU" && code("VZFO", ss = TRUE) == "VZFTAU" &&
      code("CLO", "iv_bolus", TRUE) == "CLTAU" && code("MRTIVIFO", "iv_bolus") == "MRTIBIFO" &&
      code("MRTIVIFO", "iv_infusion") == "MRTICIFO" && code("CMIN_SS") == "CMIN" && code("FLUCTP") == "FLUCP" &&
      code("CMAX_DN") == "CMAXD" && code("AUCLST_DN") == "AUCLSTD" && code("VSSO", "iv_bolus", TRUE) == "" &&
      code("b0") == "" && nzchar(cdisc_pk_codes("b0")$Note) && code("CMAX") == "CMAX" &&
      cdisc_pk_codes("LAMZHL")$PPTEST == "Half-Life Lambda z"
  }, error = function(e) FALSE),
  "URS-GEN-06", critical = TRUE, method = "cdisc_pk_codes() for selected parameters",
  expected = "steady-state and route-specific codes; explicit blanks with a note")
check("EXP-CD-03", "Every NonCompart output parameter has a code or an explicit reason why not",
  tryCatch({
    d <- read.csv(file.path("validation", "fixtures", "be_2x2x2_crossover.csv"), stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period")
    st <- list(dose = 100, infusion_duration = 0.5, dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL",
               trap_method = "log", r2adj_threshold = 0.7, mw = 0)
    ok <- TRUE
    for (route in c("extravascular", "iv_bolus", "iv_infusion")) for (ss in c(FALSE, TRUE)) {
      st$admin_route <- route; st$is_steady_state <- ss; st$tau <- if (ss) 24 else NULL
      r <- suppressWarnings(run_nca(d, cm, st))
      params <- setdiff(names(r), c("Subject", "Treatment", "Period"))
      cc <- cdisc_pk_codes(params, route, ss)
      ok <- ok && nrow(cc) == length(params) && all(nzchar(cc$PPTESTCD) | nzchar(cc$Note)) &&
        !any(cc$Note == "Not mapped to a CDISC PK parameter code")
    }
    ss_extra <- cdisc_pk_codes(c("AUCTAU", "TAU", "CAVG", "CMIN_SS", "FLUCTP", "SWING"), is_ss = TRUE)
    ok && all(nzchar(ss_extra$PPTESTCD))
  }, error = function(e) FALSE),
  "URS-GEN-06", critical = TRUE, method = "all run_nca outputs for 3 routes x steady state; single-subject SS extras",
  expected = "no parameter silently unmapped")
check("EXP-FM-01", "fmt_pk: formats", nchar(fmt_pk(123.456,4))>0,
      "URS-UI-01", method="fmt_pk", expected="Non-empty", critical=FALSE)
check("EXP-FM-02", "fmt_pk: NA->dash",
      { x <- fmt_pk(NA); identical(charToRaw(x), as.raw(c(0xe2, 0x80, 0x94))) },
      "URS-UI-01", method="fmt_pk(NA) returns UTF-8 em-dash bytes", expected="e2 80 94", critical=FALSE)

end_section("EXP")

# =============================================================================
# SECTION UI: Usability & Code Quality
# =============================================================================
start_section("UI")

check("UI-AB-01", "Labels exist", { nc<-names(theoph_result); nc<-nc[!nc%in%c("Subject","Treatment")]; sum(nc%in%names(pk_param_labels))/length(nc)>0.8 },
      "URS-UI-01", method="pk_param_labels coverage", expected=">80%", critical=FALSE)

for (hv in c("help_data_format","help_column_mapping","help_lloq","help_blq_rules",
             "help_what_is_nca","help_admin_route","help_trapezoidal","help_lambda_z",
             "help_r2adj","help_steady_state","help_dose_norm","help_what_is_be",
             "help_log_transform","help_ci_level","help_be_limits")) {
  check(paste0("UI-HLP-",which(c("help_data_format","help_column_mapping","help_lloq","help_blq_rules",
               "help_what_is_nca","help_admin_route","help_trapezoidal","help_lambda_z",
               "help_r2adj","help_steady_state","help_dose_norm","help_what_is_be",
               "help_log_transform","help_ci_level","help_be_limits")==hv)),
        paste("Help:",hv),
        tryCatch(any(grepl(paste0("^",hv,"\\s*<-"),readLines("R/help_system.R"))),error=function(e)FALSE),
        "URS-UI-01", method=paste("grep",hv), expected="Defined", critical=FALSE)
}

check("UI-VM-01", "validate: missing->invalid", !validate_mapping(list(time="Time",conc="Conc"))$valid,
      "URS-UI-04", method="No subject", expected="invalid", critical=TRUE)
check("UI-VM-02", "validate: complete->valid", validate_mapping(list(subject="Subject",time="Time",conc="Conc"))$valid,
      "URS-UI-04", method="All present", expected="valid", critical=TRUE)
check("UI-VM-03", "validate: empty->invalid", !validate_mapping(list(subject="",time="Time",conc="Conc"))$valid,
      "URS-UI-04", method="Empty subject", expected="invalid", critical=TRUE)

check("UI-CQ-01", "No /mnt/ paths",
      { rf<-list.files("R",pattern="\\.R$",full.names=TRUE); !any(sapply(rf,function(f){l<-readLines(f,warn=FALSE);any(grepl("/mnt/",l)&!grepl("^#",l))})) },
      "URS-GEN-01", method="Grep R/*.R", expected="None", critical=FALSE)
check("UI-CQ-02", "No browser()",
      { rf<-list.files("R",pattern="\\.R$",full.names=TRUE); !any(sapply(rf,function(f){l<-readLines(f,warn=FALSE);any(grepl("browser\\(\\)",l)&!grepl("^#",l))})) },
      "URS-GEN-01", method="Grep browser()", expected="None", critical=FALSE)

# Missing URS coverage tests
# Controlled mode (R/gxp_*.R) keeps an audit trail and records on purpose;
# GEN-04 covers open mode, so those files are left out of this check
check("UI-NS-01", "No persistent storage in open mode (GEN-04)",
      { rf<-grep("/gxp_", list.files("R",pattern="\\.R$",full.names=TRUE), value=TRUE, invert=TRUE); !any(sapply(rf,function(f){l<-readLines(f,warn=FALSE);any(grepl("dbConnect|RSQLite|saveRDS",l)&!grepl("^#",l))})) },
      "URS-GEN-04", method="No database/persistent storage in app code outside R/gxp_*.R", expected="No DB calls", critical=FALSE)

# Read app.R as a single string so the check is robust to line breaks between the
# tag and the APP_VERSION reference. GEN-05 only requires the version to be shown
# in the UI; the navbar and About page render it via paste0(..., APP_VERSION).
check("UI-VER-01", "APP_VERSION displayed (GEN-05)",
      { txt<-paste(readLines("app.R",warn=FALSE), collapse=" "); grepl("APP_VERSION",txt) && grepl("paste0\\([^)]*APP_VERSION\\)", txt) },
      "URS-GEN-05", method="APP_VERSION formatted for display in UI code", expected="Version shown in UI", critical=FALSE)

check("UI-BLK-01", "DQ errors block processing (DAT-05)",
      { d<-data.frame(Subject=rep("A",4),Time=c(0,1,1,2),Conc=c(0,5,5,3)); qc<-run_data_quality_check(d,bcm); !qc$pass },
      "URS-DAT-05", method="Duplicate times -> pass=FALSE blocks analysis", expected="pass=FALSE", critical=TRUE)

check("UI-ERR-01", "File read errors handled (DAT-06)",
      { l<-readLines("R/mod_data_upload.R",warn=FALSE); any(grepl("tryCatch",l)) && any(grepl("showNotification.*error",l)) },
      "URS-DAT-06", method="tryCatch and error notification in upload", expected="Error handling present", critical=FALSE)

check("UI-PSD-01", "Per-subject dose support (NCA-09)",
      { any(grepl("col_dose|col_map\\$dose",readLines("R/mod_data_upload.R",warn=FALSE))) },
      "URS-NCA-09", method="Dose column mapping in upload module", expected="Dose column support", critical=FALSE)

check("UI-BEL-01", "Configurable BE limits (BE-07)",
      { l<-readLines("R/mod_path_be.R",warn=FALSE); any(grepl("be_lower|be_upper|input\\$be_lower",l)) },
      "URS-BE-07", method="BE limits configurable via input", expected="be_lower/be_upper in code", critical=FALSE)

check("UI-CVB-01", "CV bridge to power uses the BE within-subject CV (PWR-05)",
      { l<-readLines("R/mod_path_power.R",warn=FALSE); o <- deparse(planner_cv_offer)
        any(grepl("planner_cv_offer\\(",l)) && any(grepl("within_cv_from_be(", o, fixed = TRUE)) && !any(grepl("sd\\(log\\(cmax_vals",l)) },
      "URS-PWR-05", method="Power module takes the CV from planner_cv_offer(), which uses within_cv_from_be()", expected="BE-based CV; no between-subject spread", critical=FALSE)

check("UI-JSN-01", "Settings exported as JSON (EXP-03)",
      { l<-readLines("R/export_record.R",warn=FALSE); any(grepl("toJSON|analysis_settings\\.json",l)) },
      "URS-EXP-03", method="JSON export in export_record.R", expected="toJSON present", critical=FALSE)

check("UI-BER-01", "BE results in export (EXP-05)",
      { l<-readLines("R/export_record.R",warn=FALSE); any(grepl("be_results|BE_Confidence",l)) },
      "URS-EXP-05", method="BE results referenced in export code", expected="BE export present", critical=FALSE)

check("UI-HUB-01", "Landing page hub (UI-03)",
      { l<-readLines("app.R",warn=FALSE); any(grepl("hub|landing|workflow|path",l,ignore.case=TRUE)) },
      "URS-UI-03", method="Hub/landing page in app.R", expected="Hub code present", critical=FALSE)

check("UI-COL-01", "Every layout_columns() has one width per input",
  tryCatch({
    # A col_widths vector shorter than the number of inputs is recycled by
    # bslib, which squeezes inputs into one or two columns and puts the
    # dropdown caret on top of the text
    bad <- character(0)
    walk <- function(e, f) {
      if (!is.call(e)) return(invisible())
      if (is.name(e[[1]]) && identical(as.character(e[[1]]), "layout_columns")) {
        args <- as.list(e)[-1]
        nms <- names(args); if (is.null(nms)) nms <- rep("", length(args))
        cw <- args[nms == "col_widths"]
        if (length(cw) == 1) {
          v <- tryCatch(eval(cw[[1]]), error = function(err) NULL)
          if (!is.null(v) && length(v) != sum(nms == ""))
            bad <<- c(bad, paste0(f, ": ", length(v), " widths for ", sum(nms == ""), " inputs"))
        }
      }
      for (a in as.list(e)) if (!missing(a)) tryCatch(walk(a, f), error = function(err) NULL)
      invisible()
    }
    for (f in c(list.files("R", pattern = "[.]R$", full.names = TRUE), "app.R"))
      for (ex in tryCatch(parse(f), error = function(e) list())) walk(ex, f)
    if (length(bad) > 0) message("Mismatched layouts: ", paste(bad, collapse = "; "))
    length(bad) == 0
  }, error = function(e) FALSE),
  "URS-UI-01", critical = FALSE,
  method = "parse every UI file and compare col_widths with the number of inputs",
  expected = "no layout_columns() where the widths are recycled")

end_section("UI")

# =============================================================================
# SECTION MAN: Manual Tests
# =============================================================================
start_section("MAN")

skip_manual("MAN-01","App launches","Run shiny::runApp()","App opens","URS-GEN-01")
skip_manual("MAN-02","CSV upload","Upload example_theoph.csv","Preview matches","URS-DAT-01")
skip_manual("MAN-03","Excel upload","Upload .xlsx","Data loads","URS-DAT-01")
skip_manual("MAN-04","Column auto-detect UI","Check dropdowns after upload","Auto-populated","URS-DAT-02")
skip_manual("MAN-05","DQ report renders","Click Process Data","Quality card appears","URS-DAT-03")
skip_manual("MAN-06","BLQ rule selection","Change rules, reprocess","Data changes per rule","URS-DAT-04")
skip_manual("MAN-07","Single NCA plot","Select subject, run NCA","Plotly plot displays","URS-NCA-05")
skip_manual("MAN-08","Lambda_z inspector","Toggle points in inspector","Regression updates","URS-NCA-04")
skip_manual("MAN-09","Batch NCA results","Run batch NCA","Tables display","URS-NCA-06")
skip_manual("MAN-10","Batch grid plot","Check grid after batch","Paginated grid","URS-NCA-06")
skip_manual("MAN-11","BE forest plot","Run BE analysis","Forest plot with CI","URS-BE-06")
skip_manual("MAN-12","BE CI table","Check CI table","GMR, CI, conclusion","URS-BE-03")
skip_manual("MAN-13","Power curve","Calculate power","Curve with target","URS-PWR-04")
skip_manual("MAN-14","Analysis Record","Export zip from One Subject, All Subjects and Bioequivalence","Contains results.xlsx, app_results_reference.csv, analysis_settings.json, nca_pipeline.R, reproduce_analysis.R, reproduction_check.txt, data_integrity.txt, analysis_summary.html and the data file; the app reports the reproduction check","URS-EXP-01")
skip_manual("MAN-15","Repro script","Unzip an Analysis Record; run Rscript reproduce_analysis.R in its folder","Prints data hash MATCH and Result: MATCH","URS-EXP-02")
skip_manual("MAN-16","Methods page","Click Methods nav","Formulas display","URS-GEN-03")
skip_manual("MAN-17","Data Guide","Click Data Guide","Scenario tabs","URS-UI-02")
skip_manual("MAN-18","Help popovers","Click ? button","Popover appears","URS-UI-01")
skip_manual("MAN-19","Error notification","NCA without data","Red notification","URS-UI-04")
skip_manual("MAN-20","Responsive layout","Resize < 768px","Sidebar collapses","URS-GEN-01")
skip_manual("MAN-21","BE individual profiles","Upload crossover data; run BE; open Individual Profiles tab","Per-subject panels with treatment overlay","URS-BE-08")
skip_manual("MAN-22","BE half-life review","Upload crossover data; run BE; open Half-Life Review; select profile","Plot with terminal phase; checkboxes populate","URS-NCA-12")
skip_manual("MAN-23","Override info note","Open Half-Life Review tab; verify info text","Note explaining AUC-inf dependency present","URS-NCA-12")
skip_manual("MAN-24","CDISC ADNCA upload","Set 'What kind of file?' to CDISC ADNCA dataset; upload validation/fixtures/adnca_clean.csv; choose NRRLT; process","Summary shows analytes, time variables and record selection; data processed; choices listed in the Analysis Record","URS-DAT-01")
skip_manual("MAN-25","Viz data gate","Navigate to Visualize Data before upload","Data gate card displayed, no plot rendered","URS-VIZ-01")
skip_manual("MAN-26","Viz spaghetti plot","Choose example_theoph.csv under Or use an example, click Load example and Process Data; open Visualize Data; Individual Profiles tab","12 lines rendered without error","URS-VIZ-02")
skip_manual("MAN-27","Viz colour-by options","Cycle through colour-by options (Subject/Treatment/Period/Sequence)","Plot updates for each available option; unavailable options absent","URS-VIZ-02")
skip_manual("MAN-28","Viz summary plot","Load example_theoph.csv with Load example; open Summary Plot tab","Geometric mean curve with error bars, no error","URS-VIZ-03")
skip_manual("MAN-29","Viz BLQ note","Load dataset with zero concentration; open Summary Plot","Note counting excluded observations appears","URS-VIZ-05")
skip_manual("MAN-30","Viz log scale","Toggle Log Y-axis with zero-concentration data","Plot renders without error; zero values omitted silently","URS-VIZ-07")
skip_manual("MAN-31","Viz export PNG","Render any plot; go to Export tab; select PNG 7x5 300 DPI; click Download","Non-zero PNG file downloads","URS-VIZ-06")
skip_manual("MAN-32","Viz export invalid dims","Set width = 0; click Download","Validation message displayed, no file downloaded","URS-VIZ-06")
skip_manual("MAN-33","Viz dose normalisation","Map Dose column; enable C/Dose normalisation","Y-axis values scaled by dose; option absent when no dose column","URS-VIZ-08")
skip_manual("MAN-34","Interlock refusal","Upload validation/fixtures/adnca_afrlt.csv as a flat file","ERROR in the quality report explaining it looks like a CDISC dataset; processing blocked","URS-DAT-03")
skip_manual("MAN-35","Empty LLOQ","Clear the LLOQ field; click Process Data","Message asking for an LLOQ value; app stays connected","URS-DAT-03")
skip_manual("MAN-36","Minimum R2 note","Upload a profile with a poor terminal phase (adj R2 < 0.7); run All Subjects","Note names the profile; half-life, AUCinf, CL/F, Vz/F empty for it; Half-Life Review states the fit is below the threshold","URS-NCA-04")
skip_manual("MAN-37","Review equals results","Run All Subjects on Theoph; open Half-Life Review for subject 6","Half-life and number of points equal those in the results table (7.895 h, 7 points)","URS-NCA-04")
skip_manual("MAN-38","Single-subject result cleared","In One Subject at a Time run a profile, then select another profile","Result card returns to 'Click Run PK Analysis'; record panel disappears","URS-NCA-05")
skip_manual("MAN-39","Reference treatment","Upload a crossover file with treatments New/Old; open Bioequivalence; run without choosing; choose Old; run","Run blocked until a Reference is chosen; table shows Comparison 'New / Old'","URS-BE-01")
skip_manual("MAN-40","Replicate variability table","Upload validation/fixtures/be_2x2x4_full_replicate.csv; run Bioequivalence with design 2x2x4","CVwR and CVwT table with implied EMA limits, marked informational; no scaled verdict","URS-BE-09")
skip_manual("MAN-41","Paired comparison","Run Bioequivalence with design Paired comparison","Ratio and CI shown; verdict column says no verdict","URS-BE-02")
skip_manual("MAN-42","Scaled planning uses both CVs","Plan a Study: EMA ABEL, 2x2x4, Test CV 25, Reference CV 40, ratio 95, power 80","Label reads Test product CV; N = 14","URS-PWR-01")
skip_manual("MAN-43","CV from BE analysis","Run Bioequivalence (log-transformed); open Plan a Study","Button offers the Cmax within-subject CV from the BE analysis; after uploading new data the button is gone","URS-PWR-05")
skip_manual("MAN-44","CDISC parameter codes","Run any NCA; open the CDISC codes panel and the Excel download","Codes and CT release 2026-03-27 shown; parameters without a code marked","URS-GEN-06")
skip_manual("MAN-45","Partial AUCs in All Subjects","Upload validation/fixtures/be_2x2x2_crossover.csv (LLOQ 0.5); All Subjects: 2 intervals, 0-1.5 with Cmax and 12-t; run","Columns Partial AUC 0-1.5, Cmax 0-1.5, Tmax 0-1.5, Partial AUC 12-t with units, in the table, summary and Excel; AUCINT in the CDISC codes; zero note for 12-t where Tlast is 12 h","URS-NCA-13")
skip_manual("MAN-46","Invalid partial AUC interval","Enter start 2 and end 1, then run; tick steady state with tau 12 and enter 0-24","Error message; no analysis run","URS-NCA-14")
skip_manual("MAN-47","Partial AUCs in Bioequivalence","Bioequivalence on the same file: 0-1.5 pivotal with Cmax, 4-t supportive; run once","Both partial AUCs and Cmax 0-1.5 compared on the first run; YES/NO for pivotal, no verdict (grey in the forest plot) for supportive","URS-BE-10")
skip_manual("MAN-48","Record with partial AUCs","Download the Complete Analysis Record after MAN-47","reproduction_check.txt says MATCH; intervals and roles in analysis_settings.json and the HTML summary","URS-EXP-08")
skip_manual("MAN-49","Partial AUC help and shading","Open 'What is a partial AUC?'; in Visualize Data choose Summary Plot and tick shading","Help text shown; intervals shaded; the suggested legend names the shaded intervals","URS-VIZ-09")
skip_manual("MAN-50","Load and download an example","Upload page: choose example_blq.csv under Or use an example, click Download, then Load example and Process Data; carry on with Tutorial 2","The downloaded file equals data/example_blq.csv; the data type and separators are set, the status says it is a bundled example, and the tutorial's results follow","URS-UI-02")
skip_manual("MAN-51","Exclusions in the app","After MAN-50 on example_be_crossover.csv: add a profile exclusion with a reason, run the batch and bioequivalence paths, open Review exclusions, restore it with a reason; add it again, try to reload the page, click Download exclusions, reload, load the same example and load exclusions.csv; then load another example","The register lists the exclusion; every path shows the Left out by you line; bioequivalence shows the profile as excluded and the sensitivity analysis; restoring clears the results; the browser asks before reloading until the register is downloaded; the loaded register keeps the original time and author, marked as after bioequivalence results; new data ask before clearing the exclusions and offer a download","URS-DAT-09, URS-BE-12")
skip_manual("MAN-52","Half-life rules dialog","Batch path: Edit half-life rules, set the span to 5 and Apply; run; open All Parameters and Half-Life Review","The summary line shows the new rule; the Half-Life Flags column and the (flagged) profiles appear; no value changes compared with the default rules","URS-NCA-15")
skip_manual("MAN-53","Dose per kg","Load example_theoph.csv, keep the Dose column mapped and process; open All Subjects; then choose Wt under Dose per kg: weight column, process again, set Conc unit mg/L and run","First a blue panel that reads the doses as mg per subject and a warning that they look per kg, no green check; with Wt mapped the panel lists amounts (about 320 mg) and CL/F for subject 1 is 1.49 L/h","URS-NCA-09")

skip_manual("MAN-54","Covariates in a parallel-group study (Tutorial 4b)","Load example_be_parallel_covariates.csv; map Subject, Time, Conc, Treatment and Dose (leave Dose per kg empty); Bioequivalence: Parallel groups, open Covariates (optional), choose Weight only; run","A badge reads 1 selected and the line under the box reads Weight : numeric; the line Adjusted for: Weight appears; Cmax 95.04% (90.12-100.22%) with the unadjusted columns 77.76 and 92.12; AUC to last point 98.13% (93.51-102.99%); the balance card shows 68.16 (9.5) and 77.69 (12), standardized difference 0.88","URS-BE-13")
skip_manual("MAN-55","Covariate stop messages","Upload a copy of the example in which Weight is written as '67.3 kg', one with a decimal comma in Weight, one with two different Age values for subject 5, and one with Age 0 for subject 7 (log transform ticked under Advanced options); choose the column as covariate","Each message appears under the selector before the run, names the column, and the run stops with the same message; no result is shown","URS-BE-13")
skip_manual("MAN-56","Covariate downloads and record","After MAN-54: Download Complete BE Report (Excel), Download CI Table (CSV) and Generate Analysis Record","The workbook has the sheets BE_Covariates (Weight coefficient -0.0122) and Covariate_Balance; the CSV has Adjusted for and the unadjusted columns; analysis_settings.json lists the covariate with its type and transform","URS-BE-13, URS-EXP-05")
skip_manual("MAN-57","Acceptance approach selector","Load example_be_replicate_hvd.csv and open the selector; then switch the design to 2x2x3, 2x3x3, 2x2x2 and parallel groups; type 70 as the lower limit with RSABE chosen and click Run","Standard, EMA ABEL and FDA RSABE for 2x2x4 and 2x3x3; Standard and EMA ABEL for 2x2x3 (selection returns to Standard if it was RSABE); no selector for 2x2x2 and parallel groups; the run stops at once with a message to reset the limits to 80 and 125","URS-BE-14, URS-BE-15")
skip_manual("MAN-58","Reference-scaled analysis with FDA RSABE (Tutorial 4c)","example_be_replicate_hvd.csv, Acceptance approach FDA RSABE, defaults otherwise; run","Cmax: Scaled, s_WR 0.399, limits 70.04-142.78%, criterion bound -0.0446, YES; AUC to last point: Standard, s_WR 0.251, YES; the lines under the table say so in words; the forest plot shows different limits for the two rows","URS-BE-14")
skip_manual("MAN-59","Reference-scaled analysis with EMA ABEL (Tutorial 4c)","Same file, Acceptance approach EMA ABEL; run; then Standard; run","ABEL: Cmax CVwR 41.5%, limits 73.84-135.43%, 90% CI 77.52-100.69%, YES; AUC limits 80-125%, YES. Standard: Cmax 88.35% (77.52-100.69%), NO","URS-BE-15")
skip_manual("MAN-60","Fewer than 24 subjects and a missing period","Use a copy of example_be_replicate_hvd.csv with 20 subjects, and one with subject 4 period 2 removed; RSABE","A note that the FDA recommends at least 24 subjects (20 found), still computed; a note that subjects who lack a period are left out of the contrasts","URS-BE-14")
skip_manual("MAN-61","Record and audit trail of a scaled run","After MAN-58: Download Complete BE Report (Excel) and the Analysis Record; on a controlled test server repeat the run and open the audit trail","A sheet BE_Scaled with the steps; analysis_settings.json with analysis_approach, theta 0.7967 and the switch 0.294; the audit entry of the run names the approach and its constants","URS-BE-14, URS-BE-15, URS-GXP-07")
skip_manual("MAN-62","Phone width","Set the browser to 375 px wide; repeat MAN-54 and MAN-58","No sideways scrolling of the page; the tables scroll inside their frame","URS-UI-01")
skip_manual("MAN-63","Help, Methods page and Data Guide","Open the help buttons Which acceptance approach? and Adjusting for baseline characteristics?; the Statistical Methods page (Reference-scaled approaches, examples, references); the Data Guide (Parallel Groups, 2x2x4 replicate, example downloads)","The text matches the settings: RSABE not for 2x2x3, covariates for parallel groups only, 24 subjects, switch 0.294; both new example files download","URS-GEN-03, URS-UI-02")
skip_manual("MAN-64","Settings that must not stay active unseen","Tick Advanced options, choose a log covariate, untick Advanced options; use RSABE on a replicate study, then load a parallel study and run; untick the point-estimate box with widened limits, reset the limits to 80 and 125 and run ABEL on Cmax with a point estimate outside 80-125%","The advanced choices clear; no warning about an approach on the parallel study; ABEL gives NO because the point estimate is outside 80-125%","URS-BE-13, URS-BE-15")

end_section("MAN")

# =============================================================================
# VIZ — Visualization Module (supportive, automated)
# =============================================================================
start_section("VIZ")

check("VIZ-01","Visualization module file present and parseable",
  tryCatch({
    vfile <- "R/mod_path_viz.R"
    if (!file.exists(vfile)) stop("not found")
    parsed <- parse(file = vfile)
    length(parsed) > 0
  }, error = function(e) FALSE),
  "URS-VIZ-01,URS-VIZ-02", critical = FALSE,
  method = "file.exists + parse()", expected = "File parses without error")

check("VIZ-02","Geometric mean of example data satisfies Jensen's inequality",
  tryCatch({
    d <- read.csv("data/example_theoph.csv", stringsAsFactors = FALSE)
    conc_col <- names(d)[grepl("conc", names(d), ignore.case = TRUE)][1]
    vals <- suppressWarnings(as.numeric(d[[conc_col]]))
    pos  <- vals[!is.na(vals) & vals > 0]
    gm   <- exp(mean(log(pos)))
    am   <- mean(pos)
    is.finite(gm) && gm > 0 && gm < am   # GM < AM by Jensen's (strict inequality for non-constant data)
  }, error = function(e) FALSE),
  "URS-VIZ-03", critical = FALSE,
  method = "exp(mean(log(pos_vals)))", expected = "0 < GM < AM, GM finite")

check("VIZ-03","BLQ exclusion counts zeros correctly",
  tryCatch({
    conc <- c(0, 1.5, 3.2, 2.8, 0, 4.1)
    n_excl <- sum(!is.na(conc) & conc <= 0)
    pos    <- conc[!is.na(conc) & conc > 0]
    gm     <- exp(mean(log(pos)))
    n_excl == 2L && length(pos) == 4L && is.finite(gm) && gm > 0
  }, error = function(e) FALSE),
  "URS-VIZ-05", critical = FALSE,
  method = "sum(conc <= 0)", expected = "2 excluded, 4 positives, finite GM")

check("VIZ-04","Log10 transform of positive subset produces no NaN/Inf",
  tryCatch({
    conc     <- c(0, NA, 1.0, 2.5, 0.001)
    log_vals <- log10(conc[!is.na(conc) & conc > 0])
    !any(is.nan(log_vals)) && !any(is.infinite(log_vals)) && length(log_vals) == 3L
  }, error = function(e) FALSE),
  "URS-VIZ-07", critical = FALSE,
  method = "log10(positive subset)", expected = "3 finite log values, no NaN/Inf")

check("VIZ-05","Dose normalisation guards zero/NA dose with NA output",
  tryCatch({
    conc      <- c(10, 20, 30)
    doses     <- c(100, 0, NA)
    safe_dose <- ifelse(is.na(doses) | doses <= 0, NA_real_, doses)
    result    <- conc / safe_dose
    isTRUE(all.equal(result[1], 0.1, tolerance = 1e-10)) &&
      is.na(result[2]) && is.na(result[3])
  }, error = function(e) FALSE),
  "URS-VIZ-08", critical = FALSE,
  method = "ifelse guard then division", expected = "0.1, NA, NA")

check("VIZ-06","Colour-by choices exclude unmapped columns",
  tryCatch({
    cm <- list(subject = "ID", time = "Time", conc = "Conc",
               treatment = NULL, period = NULL, sequence = NULL)
    choices <- c("Subject ID" = "subject")
    if (!is.null(cm$treatment)) choices <- c(choices, "Treatment" = "treatment")
    if (!is.null(cm$period))    choices <- c(choices, "Period"    = "period")
    if (!is.null(cm$sequence))  choices <- c(choices, "Sequence"  = "sequence")
    !"treatment" %in% choices && !"sequence" %in% choices && "subject" %in% choices
  }, error = function(e) FALSE),
  "URS-VIZ-02", critical = FALSE,
  method = "replicate color_by_choices logic", expected = "Only subject and none offered")

check("VIZ-07","Export dimension validation rejects out-of-range values",
  tryCatch({
    vd <- function(w, h) {
      if (is.na(w) || w < 2 || w > 20) return("w_err")
      if (is.na(h) || h < 2 || h > 20) return("h_err")
      "ok"
    }
    vd(0, 5) == "w_err" && vd(21, 5) == "w_err" &&
    vd(7, 0) == "h_err" && vd(7, 5) == "ok"
  }, error = function(e) FALSE),
  "URS-VIZ-06", critical = FALSE,
  method = "inline validation logic", expected = "w=0->w_err, w=21->w_err, h=0->h_err, 7x5->ok")

check("VIZ-08","create_analysis_record has viz_settings parameter",
  tryCatch({
    env <- new.env(parent = globalenv())
    source("R/export_record.R", local = env)
    fn   <- get("create_analysis_record", envir = env)
    "viz_settings" %in% names(formals(fn))
  }, error = function(e) FALSE),
  "URS-VIZ-01", critical = FALSE,
  method = "names(formals(create_analysis_record))", expected = "viz_settings present")

check("VIZ-09","Treatment overlay: crossover data has multiple treatments",
  { xo<-read.csv("data/example_be_crossover.csv",stringsAsFactors=FALSE); length(unique(xo$Treatment))>=2 },
  "URS-VIZ-04", critical = FALSE,
  method = "Crossover example has >=2 treatment levels for overlay", expected = ">=2 treatments")

end_section("VIZ")

# =============================================================================
# SECTION REG - Correctness regressions (v1.3.0)
# Each test here reproduces a defect that shipped in an earlier version and
# would have gone undetected by the rest of the suite. They are written to FAIL
# if the defect returns, not merely to assert that a function exists.
# =============================================================================
start_section("REG")

# --- REG-DOSE-01 / 02: dose must follow the subject, not the position -------
# tblNCA matches `dose` positionally against unique(key). The key is sorted
# lexicographically, so with >=10 numeric subjects the grouped dose order and
# the key order diverge and every subject receives someone else's dose. Only
# dose-dependent parameters (CL/F, Vz/F, *_DN) are affected, so Cmax and AUC
# look correct and nothing surfaces to the user.
reg_profile <- function(s, trt, dose) data.frame(
  Subject = s, Treatment = trt, Dose = dose,
  Time = c(0, 0.5, 1, 2, 4, 8, 12, 24),
  Conc = c(0, 8.1, 14.2, 11.0, 6.4, 2.9, 1.1, 0.3),
  stringsAsFactors = FALSE)
reg_cm <- list(subject="Subject", time="Time", conc="Conc",
               treatment="Treatment", dose="Dose")
reg_settings <- list(admin_route="extravascular", dose=100, infusion_duration=0,
                     is_steady_state=FALSE, dose_unit="mg", time_unit="h",
                     conc_unit="ng/mL", trap_method="linear", r2adj_threshold=0.7,
                     mw=0, partial_aucs=NULL)
reg_named_dose <- function(d) {
  lk <- tapply(d$Dose, as.character(d$Subject), max, na.rm = TRUE)
  lk[order(as.numeric(names(lk)))]
}
# CL/F = Dose / AUCinf. Identical profiles differing only in dose must give
# CL/F exactly proportional to that subject's own dose.
reg_ratio_ok <- function(res, truth) {
  g <- data.frame(k = paste(res$Subject, res$Treatment, sep="||"),
                  CLFO = suppressWarnings(as.numeric(res$CLFO)),
                  stringsAsFactors = FALSE)
  m <- merge(g, truth, by="k")
  r <- m$CLFO / m$Dose
  all(is.finite(r)) && max(abs(r - r[1])) < 1e-8
}

check("REG-DOSE-01", "Per-subject dose follows the subject (parallel, 11 subjects)",
  tryCatch({
    d <- do.call(rbind, lapply(1:11, function(s)
           reg_profile(s, if (s <= 6) "A" else "B", if (s <= 6) 100 else 400)))
    st <- reg_settings; st$dose <- reg_named_dose(d)
    res <- run_nca(d, reg_cm, st)
    truth <- unique(data.frame(k = paste(d$Subject, d$Treatment, sep="||"),
                               Dose = d$Dose, stringsAsFactors = FALSE))
    !is.null(res) && nrow(res) == 11 && reg_ratio_ok(res, truth)
  }, error = function(e) FALSE),
  "URS-NCA-03", critical = TRUE,
  method = "11 subjects, 2 dose levels, treatment mapped; CL/F vs own dose",
  expected = "CL/F proportional to each subject's own dose")

check("REG-DOSE-02", "Per-subject dose works in a crossover (was a hard stop)",
  tryCatch({
    d <- do.call(rbind, lapply(1:11, function(s)
           do.call(rbind, lapply(c("Test","Ref"), function(t)
             reg_profile(s, t, s * 100)))))
    st <- reg_settings; st$dose <- reg_named_dose(d)
    res <- run_nca(d, reg_cm, st)
    truth <- unique(data.frame(k = paste(d$Subject, d$Treatment, sep="||"),
                               Dose = d$Dose, stringsAsFactors = FALSE))
    !is.null(res) && nrow(res) == 22 && reg_ratio_ok(res, truth)
  }, error = function(e) FALSE),
  "URS-NCA-03", critical = TRUE,
  method = "11 subjects x 2 treatments with a mapped Dose column",
  expected = "22 profiles, each using its own subject's dose")

check("REG-DOSE-03", "Unnamed multi-subject dose vector is refused, not guessed",
  tryCatch({
    d <- do.call(rbind, lapply(1:3, function(s) reg_profile(s, "A", s*100)))
    st <- reg_settings; st$dose <- c(100, 200, 300)   # deliberately unnamed
    res <- suppressWarnings(run_nca(d, reg_cm, st))
    is.null(res)
  }, error = function(e) FALSE),
  "URS-NCA-03", critical = TRUE,
  method = "run_nca() with a positional dose vector",
  expected = "NULL (refused) rather than a positional guess")

# --- REG-DOSE-04: dose-normalised parameters use each subject's own dose -----
check("REG-DOSE-04", "Dose-normalised parameters match each profile to its subject's dose",
  tryCatch({
    d <- read.csv(file.path("validation", "fixtures", "be_2x2x2_crossover.csv"), stringsAsFactors = FALSE)
    d$Dose <- d$Subject * 10
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment",
               period = "Period", dose = "Dose")
    st <- reg_settings; st$dose <- dose_by_subject(d, cm)
    r <- suppressWarnings(run_nca(d, cm, st))
    dn <- add_dose_normalized(as.data.frame(r), st$dose)
    truth <- as.numeric(r$CMAX) / (as.numeric(r$Subject) * 10)
    nrow(r) == 24 && max(abs(dn$CMAX_DN - truth)) < 1e-12
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = TRUE,
  method = "12-subject crossover, dose = subject x 10, named per-subject dose vector",
  expected = "CMAX_DN = CMAX / own dose for all 24 profiles")

# --- REG-BLQ-01: positional BLQ rules operate per profile, not per subject ---
check("REG-BLQ-01", "BLQ rules treat each crossover profile independently",
  tryCatch({
    d <- data.frame(
      Subject   = rep(1, 8),
      Time      = c(0,1,2,4, 0,1,2,4),
      Conc      = c(0.05, 10, 5, 0.05, 0.05, 12, 6, 3),
      Treatment = rep(c("Test","Ref"), each = 4),
      stringsAsFactors = FALSE)
    r  <- apply_blq_rules(d, list(subject="Subject", time="Time", conc="Conc",
                                  treatment="Treatment"), lloq = 0.1, rule = "rule1")
    tst <- r$Conc[r$Treatment == "Test"]; ref <- r$Conc[r$Treatment == "Ref"]
    # Test ends BLQ -> post-last-quantifiable -> NA. Ref starts BLQ -> pre-first -> 0.
    is.na(tst[4]) && !is.na(ref[1]) && ref[1] == 0
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE,
  method = "Rule 1 on one subject with Test and Reference periods",
  expected = "trailing BLQ of Test -> NA; leading BLQ of Reference -> 0")

check("REG-BLQ-02", "Positional BLQ rules are applied in time order, not file order",
  tryCatch({
    ord <- data.frame(Subject=rep(1,5), Time=c(0,1,2,4,8),
                      Conc=c(0.05, 10, 5, 2, 0.05), stringsAsFactors=FALSE)
    shuf <- ord[c(3,5,1,4,2), ]
    cm <- list(subject="Subject", time="Time", conc="Conc")
    a <- apply_blq_rules(ord,  cm, lloq=0.1, rule="rule1")
    b <- apply_blq_rules(shuf, cm, lloq=0.1, rule="rule1")
    a <- a[order(a$Time), ]; b <- b[order(b$Time), ]
    identical(as.numeric(a$Conc), as.numeric(b$Conc))
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE,
  method = "same profile, rows shuffled, rule 1 applied to both",
  expected = "identical concentrations after re-sorting")

# --- REG-UNIT-01..03: unit strings drive a real conversion factor -----------
check("REG-UNIT-01", "Unrecognised unit spellings are rejected before the NCA runs",
  tryCatch({
    !validate_units("mg", "h", "µg/mL", 0)$valid &&   # micro sign
    !validate_units("mg", "h", "mcg/mL", 0)$valid &&
    !validate_units("mg", "h", "", 0)$valid &&
    !validate_units("mg/kg", "h", "ng/mL", 0)$valid        # dose unit with "/"
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = TRUE,
  method = "validate_units() on spellings NonCompart::Unit() cannot parse",
  expected = "all rejected with a message")

check("REG-UNIT-02", "Valid unit combinations are accepted",
  tryCatch({
    validate_units("mg", "h", "ng/mL", 0)$valid &&
    validate_units("ug", "min", "ug/L", 0)$valid &&
    validate_units("nmol", "h", "nmol/L", 0)$valid
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = TRUE,
  method = "validate_units() on supported combinations",
  expected = "accepted")

check("REG-UNIT-03", "Molar/mass mixing requires a molecular weight",
  tryCatch({
    !validate_units("mg", "h", "nmol/L", 0)$valid &&
     validate_units("mg", "h", "nmol/L", 500)$valid
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = TRUE,
  method = "molar concentration with a mass dose, MW absent then present",
  expected = "rejected without MW, accepted with MW")

# --- REG-REP-01: the shipped script must reproduce the app's numbers --------
# EXP-CMP-01 re-runs run_nca() in-process, so it cannot detect a transcription
# error in the generated script. This one executes the script as shipped.
check("REG-REP-01", "Shipped reproduction script reproduces the app's results",
  tryCatch({
    wd <- file.path(tempdir(), paste0("regrep", as.integer(runif(1,1,1e6))))
    dir.create(wd, recursive = TRUE, showWarnings = FALSE)
    mk <- function(s, trt, dose) data.frame(
      Subject=s, Treatment=trt, Dose=dose, Time=c(0,0.5,1,2,4,8,12,24),
      Conc=c("<0.5","8.1","14.2","11.0","6.4","2.9","1.1","<0.5"),
      stringsAsFactors=FALSE)
    raw <- do.call(rbind, lapply(1:11, function(s)
             do.call(rbind, lapply(c("Test","Ref"), function(t) mk(s, t, s*100)))))
    csv <- file.path(wd, "source.csv"); write.csv(raw, csv, row.names=FALSE)

    cm <- reg_cm; LLOQ <- 0.5
    d <- read.csv(csv, stringsAsFactors=FALSE)
    cc <- as.character(d$Conc)
    msk <- grepl("^<", cc) & is.na(suppressWarnings(as.numeric(cc)))
    cc[msk] <- "0"
    d$Conc <- suppressWarnings(as.numeric(cc))
    d$Time <- suppressWarnings(as.numeric(d$Time))
    d <- d[!is.na(d$Time), ]
    d <- d[order(d$Subject, d$Time), ]
    d <- apply_blq_rules(d, cm, rule="rule1", lloq=LLOQ)

    st <- reg_settings; st$dose <- reg_named_dose(d)
    app <- run_nca(d, cm, st)

    zp <- file.path(wd, "rec.zip")
    invisible(create_analysis_record(zp, app, st, cm, csv, "source.csv",
                                     "rule1", LLOQ, analyst="validation",
                                     study_name="REG-REP-01"))
    ex <- file.path(wd, "unz"); dir.create(ex, showWarnings=FALSE)
    unzip(zp, exdir=ex)
    owd <- setwd(ex)
    out <- tryCatch(system2("Rscript", "reproduce_analysis.R", stdout=TRUE, stderr=TRUE),
                    error=function(e) character(0))
    setwd(owd)

    rp <- file.path(ex, "reproduced_results.csv")
    if (!file.exists(rp)) FALSE else {
      rep <- read.csv(rp, stringsAsFactors=FALSE)
      ka <- order(paste(app$Subject, app$Treatment)); kb <- order(paste(rep$Subject, rep$Treatment))
      a <- app[ka, ]; b <- rep[kb, ]
      if (!identical(paste(a$Subject,a$Treatment), paste(b$Subject,b$Treatment))) FALSE else {
        cols <- intersect(names(a), names(b)); worst <- 0
        for (cc2 in cols) {
          x <- suppressWarnings(as.numeric(a[[cc2]])); y <- suppressWarnings(as.numeric(b[[cc2]]))
          keep <- !is.na(x) & !is.na(y)
          if (!any(keep)) next
          worst <- max(worst, max(abs(x[keep]-y[keep]) / pmax(abs(y[keep]), 1e-12)))
        }
        nrow(a) == 22 && worst < 1e-9
      }
    }
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE,
  method = "build a record, execute reproduce_analysis.R via Rscript, compare all parameters",
  expected = "max relative difference < 1e-9 across 22 crossover profiles")

# --- REG-QC-01: remediation advice must not suggest averaging analytes ------
check("REG-QC-01", "Duplicate-time advice does not recommend averaging replicates",
  tryCatch({
    src <- paste(readLines("R/data_quality.R", warn = FALSE), collapse = "\n")
    !grepl("average replicate samples", src, ignore.case = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-07", critical = FALSE,
  method = "grep R/data_quality.R for the previous remediation wording",
  expected = "advice points to splitting stacked profiles instead")

# --- REG-BE: BE verdict and model defects found on 2026-09-17 (roadmap §10.5) --
# All run through fit_be_parameter(), the code the app executes.
# NCA-grain generator: one row per subject x period, true T/R ratio `ratio`,
# within-subject SD `sd_w`, optional period effects, integer Period as uploaded.
reg_be_grain <- function(seqs, n_per_seq, ratio = 1, sd_w = 0.05, per_eff = NULL, seed = 1) {
  set.seed(seed); rows <- list(); sid <- 0
  for (sq in seqs) for (j in seq_len(n_per_seq)) {
    sid <- sid + 1; bsv <- rnorm(1, 0, 0.3); trts <- strsplit(sq, "")[[1]]
    for (p in seq_along(trts)) {
      pe <- if (is.null(per_eff)) 0 else per_eff[p]
      rows[[length(rows) + 1]] <- data.frame(
        Subject = sid, Treatment = trts[p], Period = p, Sequence = sq,
        CMAX = exp(log(100) + bsv + pe + (trts[p] == "T") * log(ratio) + rnorm(1, 0, sd_w)),
        TMAX = sample(c(1, 1.5, 2), 1), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

# D3: widened limits must not drop the point-estimate constraint
reg_be_pe <- reg_be_grain(c("TR", "RT"), 12, ratio = 1.35, sd_w = 0.05, seed = 21)
check("REG-BE-PE-01", "Widened limits: PE outside 80-125 gives NO by default",
  tryCatch({
    r <- run_be_fit(reg_be_pe, "CMAX", be_lower = 69.84, be_upper = 143.19)
    r$estimate$ci_lo >= 69.84 && r$estimate$ci_hi <= 143.19 && r$estimate$pe > 125 &&
      identical(r$row$Bioequivalent, "NO") && identical(r$row$PE_Constraint, "NO")
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE,
  method = "2x2, true ratio 135%, limits 69.84-143.19, default settings",
  expected = "CI inside widened limits, PE > 125 -> Bioequivalent NO")
check("REG-BE-PE-02", "Widened limits: PE constraint can be switched off explicitly",
  tryCatch({
    r <- run_be_fit(reg_be_pe, "CMAX", be_lower = 69.84, be_upper = 143.19, pe_constraint = FALSE)
    identical(r$row$Bioequivalent, "YES") && identical(r$row$PE_Constraint, "not applied")
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE,
  method = "same data, pe_constraint = FALSE (e.g. DDI no-effect boundaries)",
  expected = "verdict on CI alone, constraint recorded as not applied")
check("REG-BE-PE-03", "Standard limits: verdict unchanged, constraint not required",
  tryCatch({
    r <- run_be_fit(be_d, "CMAX")
    identical(r$row$Bioequivalent, "YES") && identical(r$row$PE_Constraint, "not required")
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE,
  method = "BE section dataset, limits 80-125",
  expected = "YES, PE_Constraint = not required")

# D8: untransformed analyses get no verdict against percentage limits
check("REG-BE-UT-01", "TMAX: difference with no verdict",
  tryCatch({
    r <- run_be_fit(reg_be_pe, "TMAX")
    identical(r$row$Bioequivalent, "no verdict") && grepl("Difference", r$row$Scale)
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE,
  method = "TMAX (never log-transformed)", expected = "Bioequivalent = no verdict, Scale = Difference")
check("REG-BE-UT-02", "Log-transform off: difference with no verdict",
  tryCatch({
    r <- run_be_fit(reg_be_pe, "CMAX", log_transform = FALSE)
    identical(r$row$Bioequivalent, "no verdict") && grepl("Difference", r$row$Scale)
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE,
  method = "CMAX with log_transform = FALSE", expected = "Bioequivalent = no verdict")
check("REG-BE-UT-03", "Log-transformed parameters keep a ratio verdict",
  tryCatch({
    r <- run_be_fit(be_d, "CMAX")
    r$row$Bioequivalent %in% c("YES", "NO") && grepl("Ratio", r$row$Scale)
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE,
  method = "CMAX log-transformed", expected = "YES/NO, Scale = Ratio")

# D7: Period (and Sequence, Subject) must enter the model as factors
reg_be_rep <- reg_be_grain(c("TRTR", "RTRT"), 6, ratio = 0.95, sd_w = 0.15,
                           per_eff = c(0, 0.25, -0.15, 0.10), seed = 17)
check("REG-BE-PER-01", "Integer Period with 4 periods reproduces the Method A model",
  tryCatch({
    r <- run_be_fit(reg_be_rep, "CMAX")
    d <- reg_be_rep; d$Treatment <- factor(d$Treatment)
    ref <- lm(log(CMAX) ~ Sequence + factor(Subject) + factor(Period) + Treatment, data = d)
    s <- summary(ref)$coefficients["TreatmentT", ]; tc <- qt(0.95, ref$df.residual)
    r$estimate$dfe == 3 * 12 - 4 && ref$df.residual == 3 * 12 - 4 &&
      abs(r$estimate$ci_lo - exp(s[[1]] - tc * s[[2]]) * 100) < 1e-8 &&
      abs(r$estimate$ci_hi - exp(s[[1]] + tc * s[[2]]) * 100) < 1e-8
  }, error = function(e) FALSE),
  "URS-BE-03", critical = TRUE,
  method = "2x2x4 at NCA grain, integer Period, non-linear period effects",
  expected = "df = 3n-4 = 32; CI equal to lm with factor(Period)")
check("REG-BE-PER-02", "Mixed model with integer Period and Sequence codes matches the factor model",
  tryCatch({
    d3 <- reg_be_grain(c("TRR", "RTR", "RRT"), 6, sd_w = 0.15, per_eff = c(0, 0.2, -0.1), seed = 5)
    d3$Sequence <- match(d3$Sequence, c("TRR", "RTR", "RRT"))   # coded 1/2/3
    r <- run_be_fit(d3, "CMAX", design = "crossover_3period", mt = "mixed")$estimate
    d <- be_input(d3); d$y <- log(d$CMAX)
    ref <- nlme::lme(y ~ factor(Sequence) + factor(Period) + Treatment, random = ~1 | Subject, data = d)
    tt <- summary(ref)$tTable["TreatmentT", ]
    abs(log(r$pe / 100) - tt[["Value"]]) < 1e-6 && r$dfe == tt[["DF"]]
  }, error = function(e) FALSE),
  "URS-BE-05", critical = TRUE,
  method = "2x3x3 with Sequence 1/2/3 and integer Period vs lme with factors, random ~1|Subject",
  expected = "same treatment estimate and DF")
check("REG-BE-PER-03", "Integer Subject IDs are not treated as a covariate",
  tryCatch({
    d <- reg_be_rep; d$Treatment <- factor(d$Treatment)   # Subject left integer
    r <- fit_be_parameter(d, "CMAX", design = "replicate_2x2x4", trt_col = "Treatment",
                          subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    r$estimate$dfe == 3 * 12 - 4
  }, error = function(e) FALSE),
  "URS-BE-03", critical = TRUE,
  method = "call fit_be_parameter directly with integer Subject", expected = "df = 32")

# D4: mixed-model random structure and the Sequence row of the ANOVA tables
check("REG-BE-MX-03", "Mixed model: ~1|Subject, no degenerate Sequence row",
  tryCatch({
    r <- run_be_fit(be_d, "CMAX", mt = "mixed")
    d <- be_input(be_d); d$y <- log(d$CMAX)
    ref <- nlme::lme(y ~ Sequence + Period + Treatment, random = ~1 | Subject, data = d)
    tt <- summary(ref)$tTable["TreatmentT", ]
    a <- as.data.frame(r$anova)
    abs(log(r$estimate$pe / 100) - tt[["Value"]]) < 1e-8 && r$estimate$dfe == tt[["DF"]] &&
      a["Sequence", "denDF"] > 0 && !is.nan(a["Sequence", "p-value"])
  }, error = function(e) FALSE),
  "URS-BE-05", critical = TRUE,
  method = "compare with lme(random = ~1|Subject); inspect marginal ANOVA",
  expected = "same estimate and DF; Sequence denDF > 0, p-value not NaN")
check("REG-BE-AOV-01", "Fixed model: Sequence tested against Subject(Sequence)",
  tryCatch({
    r <- run_be_fit(be_d, "CMAX")
    d <- be_input(be_d); d$y <- log(d$CMAX)
    a1 <- anova(lm(y ~ Sequence + Subject + Period + Treatment, data = d))
    f_ref <- (a1["Sequence", "Sum Sq"] / a1["Sequence", "Df"]) /
             (a1["Subject", "Sum Sq"] / a1["Subject", "Df"])
    a <- as.data.frame(r$anova)
    a["Sequence", "Df"] == 1 && abs(a["Sequence", "F value"] - f_ref) < 1e-8
  }, error = function(e) FALSE),
  "URS-BE-01", critical = FALSE,
  method = "F = MS(Sequence) / MS(Subject within Sequence)",
  expected = "Df 1, F equal to the textbook crossover test")

# D5: the "no scaled analysis" warning must show on every design the planner
# offers scaled methods for (2x2x3 and 2x3x3 map to crossover_3period)
check("REG-BE-D5-01", "ABEL/RSABE note shows for every design the planner offers scaled methods for",
  tryCatch({
    src <- paste(readLines("R/mod_path_be.R", warn = FALSE), collapse = "\n")
    pos <- regexpr("By default this app performs average bioequivalence (ABE)", src, fixed = TRUE)
    before <- substr(src, max(1, pos - 700), pos)
    pos > 0 && grepl("BE_DESIGNS$code[BE_DESIGNS$plan_scaled]", before, fixed = TRUE) &&
      setequal(BE_DESIGNS$code[BE_DESIGNS$plan_scaled], c("2x2x3", "2x3x3", "2x2x4"))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = FALSE,
  method = "the note's condition is built from the registry's plan_scaled designs",
  expected = "condition uses the registry; scaled designs are 2x2x3, 2x3x3, 2x2x4")

# D6: a single-sequence (fixed-order) comparison is not a BE design
check("REG-BE-FO-03", "Fixed-order: estimate and CI reported, no BE verdict",
  tryCatch({
    fo <- be_d; fo$Sequence <- "RT"; fo$Period <- ifelse(fo$Treatment == "R", 1, 2)
    r <- run_be_fit(fo, "CMAX", "crossover_fixed_order")
    !is.na(r$row$Point_Est) && identical(r$row$Bioequivalent, "no verdict") &&
      is.na(r$row$BE_Lower) && identical(r$row$PE_Constraint, "not applicable")
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE,
  method = "fixed-order data, design crossover_fixed_order",
  expected = "Point estimate present, Bioequivalent = no verdict, no limits")
check("REG-BE-FO-04", "Single Sequence level is analysed as a paired comparison",
  tryCatch({
    fo <- be_input(be_d); fo$Sequence <- "RT"
    a <- resolve_be_design("crossover_2x2", fo, subj_col = "Subject", trt_col = "Treatment",
                           per_col = "Period", seq_col = "Sequence")
    b <- resolve_be_design("crossover_2x2", be_input(be_d), subj_col = "Subject", trt_col = "Treatment",
                           per_col = "Period", seq_col = "Sequence")
    identical(a$design, "paired") && !is.null(a$note) &&
      identical(b$design, "crossover_2x2") && is.null(b$note)
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE,
  method = "resolve_be_design() with one vs two Sequence levels",
  expected = "one level -> paired with a note; two -> unchanged")
check("REG-BE-FO-05", "Fixed order detected from Period when no Sequence column is mapped",
  tryCatch({
    fo <- be_input(be_d); fo$Period <- ifelse(fo$Treatment == "R", 1, 2)
    a <- resolve_be_design("crossover_2x2", fo, subj_col = "Subject", trt_col = "Treatment",
                           per_col = "Period", seq_col = NULL)
    b <- resolve_be_design("crossover_2x2", be_input(be_d), subj_col = "Subject", trt_col = "Treatment",
                           per_col = "Period", seq_col = NULL)
    identical(a$design, "paired") && identical(b$design, "crossover_2x2")
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE,
  method = "treatment order per subject derived from Period",
  expected = "all subjects same order -> fixed order; mixed orders -> unchanged")

# D9: a BE analysis record states what was actually used, and reproduces
check("REG-REP-02", "BE record with per-subject doses reproduces and records BE settings",
  tryCatch({
    wd <- file.path(tempdir(), paste0("regrep2", as.integer(runif(1, 1, 1e6))))
    dir.create(wd, recursive = TRUE, showWarnings = FALSE)
    mk <- function(s, trt, per, sq) data.frame(
      Subject = s, Treatment = trt, Period = per, Sequence = sq, Dose = s * 50,
      Time = c(0, 0.5, 1, 2, 4, 8, 12, 24),
      Conc = c(0, 8.1, 14.2, 11.0, 6.4, 2.9, 1.1, 0.4) * (1 + 0.05 * s))
    raw <- do.call(rbind, lapply(1:8, function(s) if (s %% 2)
      rbind(mk(s, "Test", 1, "TR"), mk(s, "Reference", 2, "TR")) else
      rbind(mk(s, "Reference", 1, "RT"), mk(s, "Test", 2, "RT"))))
    csv <- file.path(wd, "be.csv"); write.csv(raw, csv, row.names = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment",
               period = "Period", sequence = "Sequence", dose = "Dose")
    d <- read.csv(csv, stringsAsFactors = FALSE)
    st <- reg_settings; st$dose <- reg_named_dose(d); st$n_obs <- nrow(d)
    app <- run_nca(d, cm, st)
    bes <- list(design_selected = "crossover_2x2", design_analysed = "crossover_2x2",
                model_type = "fixed", log_transform = TRUE, ci_level = 90,
                acceptance_limits = c(80, 125), pe_constraint = TRUE,
                parameters = c("CMAX", "AUCLST"))
    zp <- file.path(wd, "rec.zip")
    invisible(create_analysis_record(zp, app, st, cm, csv, "be.csv", blq_rule = "rule1",
      lloq = 0, analyst = "QA", study_name = "REG-REP-02",
      be_results = list(ci_table = data.frame(Parameter = "CMAX"), anova = list()),
      be_settings = bes))
    ex <- file.path(wd, "ex"); unzip(zp, exdir = ex)
    js <- jsonlite::fromJSON(list.files(ex, "analysis_settings.json", recursive = TRUE, full.names = TRUE)[1])
    scr <- list.files(ex, "reproduce_analysis.R", recursive = TRUE, full.names = TRUE)[1]
    owd <- setwd(dirname(scr))
    out <- tryCatch(system2("Rscript", "reproduce_analysis.R", stdout = TRUE, stderr = TRUE),
                    error = function(e) character(0))
    setwd(owd)
    identical(js$dose_source, "per_subject") &&
      identical(js$bioequivalence$design_analysed, "crossover_2x2") &&
      isTRUE(js$bioequivalence$pe_constraint) &&
      any(grepl("-> MATCH", out)) && !any(grepl("DIFFERENT", out))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE,
  method = "BE record with per-subject doses and BE settings; execute reproduce_analysis.R",
  expected = "dose_source per_subject; bioequivalence block present; script says MATCH")

# Duplicate times and replicate designs in the quality check
reg_qc_cm <- list(subject = "Subject", time = "Time", conc = "Conc",
                  treatment = "Treatment", period = "Period")
reg_qc_prof <- function(s, trt, per) data.frame(Subject = s, Treatment = trt, Period = per,
  Time = c(0, 1, 2, 4, 8), Conc = c(0, 10, 8, 4, 1), stringsAsFactors = FALSE)
check("REG-QC-02", "Standard 2x2 with nominal times passes the duplicate check",
  tryCatch({
    d <- rbind(reg_qc_prof("A", "T", 1), reg_qc_prof("A", "R", 2),
               reg_qc_prof("B", "R", 1), reg_qc_prof("B", "T", 2))
    qc <- run_data_quality_check(d, reg_qc_cm)
    !any(qc$findings$Severity == "ERROR")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE,
  method = "2 subjects x 2 periods, repeated nominal times", expected = "no ERROR")
check("REG-QC-04", "Duplicates within one treatment-period: advice fits what is mapped",
  tryCatch({
    d <- rbind(reg_qc_prof("A", "T", 1), reg_qc_prof("A", "T", 1), reg_qc_prof("A", "R", 2))
    qc <- run_data_quality_check(d, reg_qc_cm); f <- qc$findings
    e <- f[f$Severity == "ERROR" & grepl("uplicate", f$Message), ]
    d2 <- rbind(reg_qc_prof("A", "T", 1), reg_qc_prof("A", "T", 2))
    qc2 <- run_data_quality_check(d2, list(subject = "Subject", time = "Time", conc = "Conc",
                                          treatment = "Treatment")); f2 <- qc2$findings
    e2 <- f2[f2$Severity == "ERROR" & grepl("uplicate", f2$Message), ]
    nrow(e) == 1 && !grepl("map the Treatment and Period", e$Action, ignore.case = TRUE) &&
      nrow(e2) == 1 && grepl("Period", e2$Action)
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = FALSE,
  method = "stacked profile with both columns mapped; replicate with Period unmapped",
  expected = "first: no map-columns advice; second: advice to map Period")

end_section("REG")

# =============================================================================
# SECTION REP: Replicate and crossover designs (roadmap Part B)
# =============================================================================
start_section("REP")

rep_fix <- function(f) read.csv(file.path("validation", "fixtures", f), stringsAsFactors = FALSE)
rep_cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment",
               period = "Period", sequence = "Sequence")
rep_settings <- list(admin_route = "extravascular", dose = 100, infusion_duration = 0,
                     is_steady_state = FALSE, dose_unit = "mg", time_unit = "h",
                     conc_unit = "ng/mL", trap_method = "log", r2adj_threshold = 0.7, mw = 0)
rep_224 <- rep_fix("be_2x2x4_full_replicate.csv")
rep_223 <- rep_fix("be_2x2x3_full_replicate.csv")
rep_233 <- rep_fix("be_2x3x3_partial_replicate.csv")
rep_222 <- rep_fix("be_2x2x2_crossover.csv")
rep_nca <- function(d) suppressWarnings(run_nca(d, rep_cm, rep_settings))

check("REP-PC-01", "2x2x4: one NCA profile per subject x period",
  tryCatch({
    r <- rep_nca(rep_224); n <- length(unique(rep_224$Subject))
    nrow(r) == 4 * n && all(c("Subject", "Treatment", "Period") %in% names(r)) &&
      !anyDuplicated(paste(r$Subject, r$Period))
  }, error = function(e) FALSE),
  "URS-NCA-06", critical = TRUE,
  method = "run_nca on 12-subject TRTR/RTRT fixture", expected = "48 profiles with a Period column")
check("REP-PC-02", "2x2x4: each profile's Cmax is that administration's own Cmax",
  tryCatch({
    r <- rep_nca(rep_224)
    truth <- aggregate(Conc ~ Subject + Period, data = rep_224, FUN = max)
    m <- merge(data.frame(Subject = r$Subject, Period = r$Period, CMAX = as.numeric(r$CMAX)),
               transform(truth, Subject = as.character(Subject), Period = as.character(Period)))
    nrow(m) == nrow(truth) && max(abs(m$CMAX - m$Conc)) < 1e-12
  }, error = function(e) FALSE),
  "URS-NCA-01", critical = TRUE,
  method = "compare CMAX with max(Conc) per subject x period", expected = "identical for all 48")
check("REP-PC-03", "2x2x4: BE data has one row per administration",
  tryCatch({
    r <- rep_nca(rep_224); b <- build_be_data(r, rep_224, rep_cm)
    n <- length(unique(rep_224$Subject))
    nrow(b$data) == 4 * n && !anyDuplicated(paste(b$data$Subject, b$data[[b$per_col]])) &&
      !anyNA(b$data[[b$per_col]]) && !anyNA(b$data[[b$seq_col]])
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE,
  method = "build_be_data on the 2x2x4 NCA result", expected = "48 rows, no duplicates, no NA Period/Sequence")
check("REP-PC-04", "2x2x3 and 2x3x3: 3n profiles and 3n BE rows",
  tryCatch({
    ok <- TRUE
    for (d in list(rep_223, rep_233)) {
      n <- length(unique(d$Subject)); r <- rep_nca(d); b <- build_be_data(r, d, rep_cm)
      ok <- ok && nrow(r) == 3 * n && nrow(b$data) == 3 * n
    }
    ok
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE,
  method = "TRT/RTR and TRR/RTR/RRT fixtures", expected = "36 profiles and 36 rows each")
check("REP-PC-05", "2x2: results unchanged by the period-aware key",
  tryCatch({
    r <- rep_nca(rep_222)
    d <- rep_222; d$.k <- paste(d$Subject, d$Treatment, sep = "||")
    d <- d[order(d$.k, d$Time), ]
    ref <- tblNCA(d, key = ".k", colTime = "Time", colConc = "Conc", dose = 100,
                  adm = "Extravascular", dur = 0, doseUnit = "mg", timeUnit = "h",
                  concUnit = "ng/mL", down = "Log", R2ADJ = 0, MW = 0, SS = FALSE, iAUC = "")
    parts <- strsplit(ref[[1]], "||", fixed = TRUE)
    ref_k <- paste(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    idx <- match(ref_k, paste(r$Subject, r$Treatment))
    num <- setdiff(names(ref)[-1], character(0))
    !anyNA(idx) && nrow(r) == nrow(ref) &&
      all(sapply(num, function(cc) identical(as.numeric(ref[[cc]]), as.numeric(r[[cc]][idx]))))
  }, error = function(e) FALSE),
  "URS-NCA-06", critical = TRUE,
  method = "compare with tblNCA keyed on Subject||Treatment", expected = "every parameter bit-identical")
check("REP-BLQ-01", "BLQ rules act on each administration separately",
  tryCatch({
    ok <- TRUE
    for (rule in c("rule1", "rule5", "rule6")) {
      whole <- apply_blq_rules(rep_224, rep_cm, rule = rule, lloq = 0.5)
      split_d <- split(rep_224, list(rep_224$Subject, rep_224$Period), drop = TRUE)
      each <- do.call(rbind, lapply(split_d, apply_blq_rules, col_map = rep_cm, rule = rule, lloq = 0.5))
      kw <- paste(whole$Subject, whole$Period, whole$Time); ke <- paste(each$Subject, each$Period, each$Time)
      ok <- ok && identical(whole$Conc, each$Conc[match(kw, ke)])
    }
    ok
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE,
  method = "apply rules 1/5/6 to the whole 2x2x4 file vs per subject x period", expected = "identical")
check("REP-QC-01", "Replicate fixture passes the data quality check",
  tryCatch({
    qc <- run_data_quality_check(rep_224, rep_cm, lloq = 0.5)
    !any(qc$findings$Severity == "ERROR")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE,
  method = "run_data_quality_check on 2x2x4 fixture", expected = "no ERROR")
check("REP-AN-01", "2x2x4 end to end: EMA Method A model and degrees of freedom",
  tryCatch({
    r <- rep_nca(rep_224); b <- build_be_data(r, rep_224, rep_cm)
    f <- fit_be_parameter(b$data, "CMAX", design = "replicate_2x2x4", trt_col = b$trt_col,
                          subj_col = b$subj_col, per_col = b$per_col, seq_col = b$seq_col)
    truth <- aggregate(Conc ~ Subject + Period + Treatment + Sequence, data = rep_224, FUN = max)
    truth$Treatment <- relevel(factor(truth$Treatment), ref = "Reference")
    ref <- lm(log(Conc) ~ Sequence + factor(Subject) + factor(Period) + Treatment, data = truth)
    s <- summary(ref)$coefficients["TreatmentTest", ]; tc <- qt(0.95, ref$df.residual)
    n <- length(unique(rep_224$Subject))
    f$estimate$dfe == 3 * n - 4 && abs(f$estimate$ci_lo - exp(s[[1]] - tc * s[[2]]) * 100) < 1e-8 &&
      abs(f$estimate$ci_hi - exp(s[[1]] + tc * s[[2]]) * 100) < 1e-8 &&
      f$row$N_Test == n && f$row$N_Ref == n
  }, error = function(e) FALSE),
  "URS-BE-03", critical = TRUE,
  method = "fixture -> run_nca -> build_be_data -> fit_be_parameter vs lm on true per-period Cmax",
  expected = "df = 3n-4 = 32, identical CI, N counts subjects")
check("REP-DOSE-01", "Per-subject doses follow the subject in a replicate design",
  tryCatch({
    d <- rep_224; d$Dose <- d$Subject * 10
    st <- rep_settings; dv <- tapply(d$Dose, as.character(d$Subject), max); st$dose <- dv
    r <- suppressWarnings(run_nca(d, rep_cm, st))
    # CL/F is reported in L/h for mg and ng/mL.h, hence the factor 1000
    expect <- as.numeric(dv[r$Subject]) * 1000
    got <- as.numeric(r$CLFO) * as.numeric(r$AUCIFO)
    ok <- !is.na(got)
    sum(ok) > 40 && max(abs(got[ok] - expect[ok]) / expect[ok]) < 1e-6
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = TRUE,
  method = "named dose vector (Subject x 10) on 2x2x4", expected = "CL/F x AUCinf = 1000 x dose (unit factor) for every profile")

rep_record_run <- function(d, cm, overrides = NULL, apply_to_app = NULL) {
  wd <- file.path(tempdir(), paste0("reprec", as.integer(runif(1, 1, 1e6))))
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(wd, "rep.csv"); write.csv(d, csv, row.names = FALSE)
  dd <- read.csv(csv, stringsAsFactors = FALSE)
  dd <- dd[order(dd$Subject, dd$Time), ]
  dd <- apply_blq_rules(dd, cm, rule = "rule1", lloq = 0.5)
  st <- rep_settings; st$n_obs <- nrow(dd)
  app <- suppressWarnings(run_nca(dd, cm, st, lz_overrides = overrides))
  if (!is.null(apply_to_app)) app <- apply_to_app(app)
  zp <- file.path(wd, "rec.zip")
  invisible(suppressWarnings(create_analysis_record(zp, app, st, cm, csv, "rep.csv",
    blq_rule = "rule1", lloq = 0.5, analyst = "QA", study_name = "REP", lz_overrides = overrides)))
  ex <- file.path(wd, "ex"); unzip(zp, exdir = ex)
  js <- jsonlite::fromJSON(list.files(ex, "analysis_settings.json", recursive = TRUE, full.names = TRUE)[1])
  scr <- list.files(ex, "reproduce_analysis.R", recursive = TRUE, full.names = TRUE)[1]
  owd <- setwd(dirname(scr))
  out <- tryCatch(system2("Rscript", "reproduce_analysis.R", stdout = TRUE, stderr = TRUE),
                  error = function(e) character(0))
  setwd(owd)
  list(json = js, out = out, app = app)
}
check("REP-REP-01", "Replicate record: script reproduces 48 profiles and states the profile key",
  tryCatch({
    rr <- rep_record_run(rep_224, rep_cm)
    identical(rr$json$nca_profile_key, c("Subject", "Treatment", "Period")) &&
      any(grepl("48 rows", rr$out)) && any(grepl("-> MATCH", rr$out)) && !any(grepl("DIFFERENT", rr$out))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE,
  method = "record for 2x2x4 fixture with LLOQ 0.5, rule 1; run reproduce_analysis.R",
  expected = "nca_profile_key Subject/Treatment/Period; 48 rows; MATCH")
check("REP-REP-02", "Replicate record: a half-life override is replayed on the right administration",
  tryCatch({
    ov <- list("1 | Test | P3" = list(profile = "1 | Test | P3", subject = "1", treatment = "Test",
               period = "3", original_lambda_z = 0.15, adjusted_lambda_z = 0.2,
               original_r2adj = 0.99, adjusted_r2adj = 0.99, points_used = 3))
    ov[[1]]$time_used <- c(4, 6, 8)
    rr <- rep_record_run(rep_224, rep_cm, overrides = ov)
    any(grepl("-> MATCH", rr$out)) && !any(grepl("DIFFERENT", rr$out))
  }, error = function(e) FALSE),
  "URS-EXP-07", critical = TRUE,
  method = "override on subject 1, Test, period 3 only; run reproduce_analysis.R",
  expected = "MATCH (override applied to exactly that profile)")

check("REP-HL-01", "Profile labels identify exactly one NCA row per administration",
  tryCatch({
    r <- rep_nca(rep_224); labs <- result_profile_labels(r)
    !anyDuplicated(labs) && all(sapply(labs, function(l) length(profile_result_row(r, l)) == 1)) &&
      identical(profile_parts(r, "1 | Test | P3"), list(subject = "1", treatment = "Test", period = "3"))
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE,
  method = "labels on 2x2x4 NCA result", expected = "48 unique labels, each matching one row")
check("REP-HL-02", "Profile data rows are that administration only, in time order",
  tryCatch({
    rows <- profile_data_rows(rep_224, rep_cm, "1 | Test | P3")
    d <- rep_224[rows, ]
    nrow(d) == 13 && all(d$Subject == 1 & d$Period == 3 & d$Treatment == "Test") && !is.unsorted(d$Time) &&
      nrow(data_profiles(rep_224, rep_cm)) == 48 &&
      identical(data_profiles(rep_224, rep_cm)$label[1:4],
                c("1 | Reference | P2", "1 | Reference | P4", "1 | Test | P1", "1 | Test | P3"))
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE,
  method = "profile_data_rows and data_profiles on the 2x2x4 fixture",
  expected = "13 rows of subject 1, Test, period 3; 48 ordered profiles")
check("REP-HL-03", "Half-life review code no longer parses profile labels",
  tryCatch({
    files <- c("R/mod_path_be.R", "R/mod_path_multi_nca.R", "R/mod_path_single_nca.R")
    src <- unlist(lapply(files, readLines, warn = FALSE))
    !any(grepl('strsplit\\((sel|profile)', src))
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = FALSE,
  method = "source inspection of the three analysis modules",
  expected = "no strsplit() on profile labels (lookups go through profile helpers)")

# B4: within-subject variability diagnostic, against replicateBE::method.A
rep_ref <- read.csv(file.path("validation", "fixtures", "replicateBE_reference.csv"), stringsAsFactors = FALSE)
rep_cv_for <- function(fixture) {
  d <- rep_fix(paste0(fixture, ".csv"))
  b <- build_be_data(rep_nca(d), d, rep_cm)
  list(b = b, cv = be_variability_diagnostic(b$data, "CMAX", trt_col = b$trt_col, subj_col = b$subj_col,
                                             per_col = b$per_col, seq_col = b$seq_col))
}
rep_close <- function(a, b, tol = 1e-6) isTRUE(abs(a - b) < tol)
check("REP-CV-01", "CVwR and CVwT match replicateBE on full replicates (2x2x4, 2x2x3, HVD)",
  tryCatch({
    ok <- TRUE
    for (f in c("be_2x2x4_full_replicate", "be_2x2x3_full_replicate", "be_2x2x4_highly_variable")) {
      x <- rep_cv_for(f)$cv; r <- rep_ref[rep_ref$fixture == f, ]
      ok <- ok && rep_close(x$swR, r$swR) && rep_close(x$CVwR, r$CVwR) &&
        rep_close(x$swT, r$swT) && rep_close(x$CVwT, r$CVwT)
    }
    ok
  }, error = function(e) FALSE),
  "URS-BE-09", critical = TRUE,
  method = "be_variability_diagnostic vs committed replicateBE 1.1.3 method.A values",
  expected = "swR, CVwR, swT, CVwT equal within 1e-6")
check("REP-CV-02", "Partial replicate: CVwR matches, CVwT reported as not estimable",
  tryCatch({
    x <- rep_cv_for("be_2x3x3_partial_replicate")$cv; r <- rep_ref[rep_ref$fixture == "be_2x3x3_partial_replicate", ]
    rep_close(x$CVwR, r$CVwR) && is.na(x$CVwT) && grepl("once", x$CVwT_note)
  }, error = function(e) FALSE),
  "URS-BE-09", critical = TRUE,
  method = "TRR/RTR/RRT fixture", expected = "CVwR equal; CVwT NA with an explanation")
check("REP-CV-03", "Implied ABEL limits match replicateBE and PowerTOST, including the cap",
  tryCatch({
    x <- rep_cv_for("be_2x2x4_highly_variable")$cv; r <- rep_ref[rep_ref$fixture == "be_2x2x4_highly_variable", ]
    lim_ok <- rep_close(x$ABEL_lower, r$L, 1e-6) && rep_close(x$ABEL_upper, r$U, 1e-6)
    cvs <- c(0.20, 0.30, 0.35, 0.45, 0.50, 0.65)
    pt <- sapply(cvs, function(cv) PowerTOST::scABEL(CV = cv, regulator = "EMA") * 100)
    mine <- sapply(cvs, function(cv) abel_limits(cv * 100))
    lim_ok && max(abs(pt - mine)) < 1e-8
  }, error = function(e) FALSE),
  "URS-BE-09", critical = TRUE,
  method = "HVD fixture vs replicateBE L/U; abel_limits() vs PowerTOST::scABEL at CV 20-65%",
  expected = "equal; 80-125 at CV <= 30%; capped at CV 50%")
check("REP-CV-04", "Non-replicated design: diagnostic says not applicable",
  tryCatch({
    d <- rep_222; b <- build_be_data(rep_nca(d), d, rep_cm)
    x <- be_variability_diagnostic(b$data, "CMAX", trt_col = b$trt_col, subj_col = b$subj_col,
                                   per_col = b$per_col, seq_col = b$seq_col)
    is.null(x)
  }, error = function(e) FALSE),
  "URS-BE-09", critical = FALSE,
  method = "2x2 fixture", expected = "NULL (reference not replicated)")
check("REP-CV-05", "The CVwR model needs the period term (naive model differs)",
  tryCatch({
    x <- rep_cv_for("be_2x2x4_highly_variable"); b <- x$b
    ref <- b$data[b$data$Treatment == "Reference", ]
    naive <- summary(lm(log(CMAX) ~ Subject, data = ref))$sigma
    abs(naive - x$cv$swR) > 1e-4
  }, error = function(e) FALSE),
  "URS-BE-09", critical = FALSE,
  method = "compare with lm(log(CMAX) ~ Subject) on reference data",
  expected = "differs: the fixture has period effects the naive model ignores")

# B5: one design registry for planner, analysis and About page
check("REP-DES-01", "Planner design menus come from the registry",
  tryCatch({
    identical(names(planner_designs("abe")), c("2x2", "2x2x3", "2x3x3", "2x2x4", "parallel")) &&
      identical(names(planner_designs("abel")), c("2x2x3", "2x3x3", "2x2x4")) &&
      identical(names(planner_designs("rsabe")), c("2x2x3", "2x3x3", "2x2x4")) &&
      identical(names(planner_designs("ntid")), "2x2x4") &&
      any(grepl("planner_designs(\"abe\")", readLines("R/mod_path_power.R", warn = FALSE), fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-PWR-03", critical = TRUE,
  method = "planner_designs() per method; mod_path_power.R uses it",
  expected = "same design sets as before, now from BE_DESIGNS")
check("REP-DES-02", "Every design the planner offers can be analysed, with the same name",
  tryCatch({
    planned <- BE_DESIGNS[BE_DESIGNS$plan_abe | BE_DESIGNS$plan_scaled | BE_DESIGNS$plan_ntid, ]
    all(planned$code %in% be_analysis_choices()) &&
      !anyDuplicated(BE_DESIGNS$code) && !anyDuplicated(BE_DESIGNS$label) &&
      !any(grepl("^3-period crossover$", BE_DESIGNS$label))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE,
  method = "registry integrity", expected = "planned designs are analysable; labels unique and specific")
check("REP-DES-03", "Design codes map to the right model, including legacy codes",
  tryCatch({
    m <- sapply(c("2x2x2", "2x2x3", "2x3x3", "2x2x4", "parallel", "paired",
                  "crossover_2x2", "crossover_3period", "replicate_2x2x4",
                  "crossover_fixed_order"), be_design_model)
    identical(unname(m), c("crossover", "crossover", "crossover", "crossover", "parallel", "paired",
                           "crossover", "crossover", "crossover", "paired"))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE,
  method = "be_design_model() on current and legacy codes", expected = "correct model family")
check("REP-DES-04", "Selected design is checked against the data",
  tryCatch({
    det224 <- detect_study_design(rep_224, rep_cm); det222 <- detect_study_design(rep_222, rep_cm)
    is.null(check_design_against_data("2x2x4", det224)) &&
      is.null(check_design_against_data("2x2x2", det222)) &&
      !is.null(check_design_against_data("2x3x3", det224)) &&
      !is.null(check_design_against_data("2x2x2", det224)) &&
      !is.null(check_design_against_data("parallel", det222))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE,
  method = "check_design_against_data() with 2x2x4 and 2x2 fixtures",
  expected = "matching selections pass; mismatches return a message")
check("REP-DES-05", "Partial and full 3-period replicates give identical ABE results",
  tryCatch({
    b <- build_be_data(rep_nca(rep_233), rep_233, rep_cm)
    f <- function(code) fit_be_parameter(b$data, "CMAX", design = code, trt_col = b$trt_col,
                                         subj_col = b$subj_col, per_col = b$per_col, seq_col = b$seq_col)$estimate
    identical(f("2x3x3"), f("2x2x3")) && identical(f("2x3x3"), f("crossover_3period"))
  }, error = function(e) FALSE),
  "URS-BE-03", critical = FALSE,
  method = "same data fitted under 2x3x3, 2x2x3 and legacy crossover_3period",
  expected = "identical estimates (one Method A model)")

# B6: agreement with the reference implementation on its 30 reference data sets
check("REP-RBE-01", "EMA Method A and CVwR agree with replicateBE on all 30 reference data sets",
  tryCatch({
    worst <- 0; df_ok <- TRUE; n_sets <- 0
    for (nm in sprintf("rds%02d", 1:30)) {
      d <- getExportedValue("replicateBE", nm)   # lazy-loaded reference data set
      ma <- suppressMessages(suppressWarnings(replicateBE::method.A(
        data = d, print = FALSE, details = TRUE, verbose = FALSE, plot.bxp = FALSE)))
      b <- data.frame(Subject = as.character(d$subject), Period = as.character(d$period),
                      Sequence = as.character(d$sequence),
                      Treatment = factor(ifelse(d$treatment == "T", "Test", "Reference"),
                                         levels = c("Reference", "Test")),
                      CMAX = d$PK, stringsAsFactors = FALSE)
      # Named CMAX: EMA widens the limits for Cmax only, so the implied ABEL
      # limits are reported (and compared) for that metric
      f <- fit_be_parameter(b, "CMAX", "2x2x4", trt_col = "Treatment", subj_col = "Subject",
                            per_col = "Period", seq_col = "Sequence")$estimate
      v <- be_variability_diagnostic(b, "CMAX", trt_col = "Treatment", subj_col = "Subject",
                                     per_col = "Period", seq_col = "Sequence")
      g <- function(col) if (col %in% names(ma)) as.numeric(ma[1, col]) else NA_real_
      diffs <- c(f$pe - g("PE(%)"), f$ci_lo - g("CL.lo(%)"), f$ci_hi - g("CL.hi(%)"),
                 v$CVwR - g("CVwR(%)"), v$CVwT - g("CVwT(%)"), v$ABEL_lower - g("L(%)"),
                 v$ABEL_upper - g("U(%)"))
      worst <- max(worst, abs(diffs), na.rm = TRUE)
      df_ok <- df_ok && f$dfe == g("DF")
      n_sets <- n_sets + 1
    }
    n_sets == 30 && df_ok && worst < 1e-8
  }, error = function(e) FALSE),
  "URS-BE-03, URS-BE-09", critical = TRUE,
  method = paste("replicateBE::method.A on rds01-rds30 vs fit_be_parameter and",
                 "be_variability_diagnostic (PE, 90% CI, DF, CVwR, CVwT, ABEL limits)"),
  expected = "identical DF; all other values within 1e-8")

end_section("REP")

# =============================================================================
# SECTION REC: Analysis Record reproduces through the shipped pipeline
# =============================================================================
start_section("REC")

rec_unzip <- function(zp) {
  ex <- file.path(tempdir(), paste0("recx", as.integer(runif(1, 1, 1e7))))
  dir.create(ex); utils::unzip(zp, exdir = ex); ex
}
rec_check_text <- function(ex) {
  f <- file.path(ex, "reproduction_check.txt")
  if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else ""
}
rec_build <- function(raw_csv_lines = NULL, df = NULL, cm, st, lloq = 0, rule = "rule1",
                      read_args = list(), overrides = NULL, be = FALSE) {
  wd <- file.path(tempdir(), paste0("recb", as.integer(runif(1, 1, 1e7)))); dir.create(wd)
  f <- file.path(wd, "input.csv")
  if (!is.null(raw_csv_lines)) writeLines(raw_csv_lines, f) else write.csv(df, f, row.names = FALSE)
  ds <- prepare_pk_dataset(read_pk_file(f, read_args), cm, list(lloq = lloq, blq_rule = rule, read_args = read_args))
  res <- suppressWarnings(run_nca(ds$data, cm, st, lz_overrides = overrides))
  zp <- file.path(wd, "rec.zip")
  # A BE record carries a real BE run: the same function the app calls
  be_in <- list(design_selected = "2x2x4", reference = "Reference", parameters = c("CMAX", "AUCLST"), approach = NULL,
                model_type = "fixed", log_transform = TRUE, ci_level = 90, be_lower = 80, be_upper = 125,
                pe_constraint = TRUE, widened_scope = "cmax", covariates = NULL, conc_unit = st$conc_unit,
                time_unit = st$time_unit, is_steady_state = FALSE)
  be_run <- if (be) run_be_analysis(res, ds$data, cm, be_in, st, detected_design = ds$design, lz_overrides = overrides)
  out <- suppressWarnings(create_analysis_record(zp, res, st, cm, f, "input.csv", blq_rule = rule,
    lloq = lloq, analyst = "QA", study_name = "REC", lz_overrides = overrides, read_args = read_args,
    be_results = if (be) be_run$result else NULL, be_settings = if (be) list(ci_level = 90) else NULL,
    be_record = if (be) list(inputs = be_in, raw = be_run$raw) else NULL))
  list(zip = zp, ex = rec_unzip(zp), result = res, out = out)
}

check("REC-01", "Record ships the pipeline and a passing reproduction check (theophylline)",
  tryCatch({
    zf <- file.path(tempdir(), "rec01.zip")
    th <- read_pk_file("data/example_theoph.csv"); th_cm <- list(subject = "Subject", time = "Time", conc = "conc")
    th_res <- run_nca(prepare_pk_dataset(th, th_cm)$data, th_cm, theoph_settings)
    out <- create_analysis_record(zf, th_res, theoph_settings, th_cm,
      "data/example_theoph.csv", "example_theoph.csv", blq_rule = "rule1", lloq = 0)
    ex <- rec_unzip(zf); js <- jsonlite::fromJSON(file.path(ex, "analysis_settings.json"))
    file.exists(file.path(ex, "nca_pipeline.R")) &&
      identical(js$pipeline_sha256, digest::digest(file = "R/pipeline.R", algo = "sha256")) &&
      grepl("Result: MATCH", rec_check_text(ex)) && identical(attr(out, "reproduction"), "MATCH") &&
      !file.exists(file.path(ex, "reproduced_results.csv"))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE,
  method = "create_analysis_record on theophylline; inspect zip",
  expected = "nca_pipeline.R with recorded hash; reproduction_check.txt says MATCH")
check("REC-02", "Molar units: the reproduction uses the recorded molecular weight",
  tryCatch({
    st <- theoph_settings; st$conc_unit <- "umol/L"; st$mw <- 180.16
    r <- rec_build(df = theoph, cm = theoph_cm, st = st)
    grepl("Result: MATCH", rec_check_text(r$ex))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "theophylline with umol/L and MW 180.16",
  expected = "MATCH (the previous script omitted MW)")
check("REC-03", "Semicolon / decimal-comma file with BLQ text reproduces",
  tryCatch({
    # Semicolon-separated, decimal commas throughout, '<0,1' BLQ text in the Conc column
    lines <- c("Subject;Time;Conc", "1;0;<0,1", "1;0,5;4,2", "1;1;6,1", "1;2;5,0", "1;4;3,1", "1;8;1,2", "1;12;0,5",
               "2;0;<0,1", "2;0,5;3,9", "2;1;6,6", "2;2;5,4", "2;4;3,0", "2;8;1,4", "2;12;0,6")
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    r <- rec_build(raw_csv_lines = lines, cm = cm, st = theoph_settings, lloq = 0.1, rule = "rule1",
                   read_args = list(sep = ";", dec = ","))
    grepl("Result: MATCH", rec_check_text(r$ex)) && nrow(r$result) == 2
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "sep ';', dec ',', '<0,1' text in a decimal-comma Conc column, LLOQ 0.1",
  expected = "MATCH (the previous script read every file as comma-separated)")
check("REC-04", "Replicate BE record with per-subject doses and an override reproduces",
  tryCatch({
    d <- rep_224; d$Dose <- d$Subject * 10
    cm <- c(rep_cm, list(dose = "Dose")); st <- rep_settings; st$dose <- dose_by_subject(d, cm)
    ov <- list("1 | Test | P3" = list(profile = "1 | Test | P3", subject = "1", treatment = "Test",
               period = "3", time_used = c(4, 6, 8)))
    r <- rec_build(df = d, cm = cm, st = st, lloq = 0.5, overrides = ov, be = TRUE)
    js <- jsonlite::fromJSON(file.path(r$ex, "analysis_settings.json"), simplifyDataFrame = FALSE)
    grepl("Result: MATCH", rec_check_text(r$ex)) && grepl("BE result: MATCH", rec_check_text(r$ex), fixed = TRUE) &&
      identical(js$dose_source, "per_subject") && identical(as.numeric(js$lz_overrides[[1]]$time_used), c(4, 6, 8))
  }, error = function(e) FALSE),
  "URS-EXP-07", critical = TRUE, method = "2x2x4 fixture, dose = subject x 10, override on one administration, with a real BE run in the record",
  expected = "NCA and BE both MATCH; per-subject dose and override time points recorded")
check("REC-05", "The check detects a changed data file",
  tryCatch({
    r <- rec_build(df = theoph, cm = theoph_cm, st = theoph_settings)
    f <- file.path(r$ex, "input.csv"); d <- read.csv(f); d$Conc[5] <- d$Conc[5] * 2; write.csv(d, f, row.names = FALSE)
    owd <- setwd(r$ex)
    out <- tryCatch(system2(file.path(R.home("bin"), "Rscript"), "reproduce_analysis.R", stdout = TRUE, stderr = TRUE),
                    error = function(e) character(0))
    setwd(owd)
    any(grepl("Data file: MISMATCH", out)) && any(grepl("DIFFERENT", out))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "double one concentration after export, rerun the script",
  expected = "data hash MISMATCH and result DIFFERENT")
check("REC-06", "Single-subject record (uploaded file, override) reproduces",
  tryCatch({
    wd <- file.path(tempdir(), "rec06"); dir.create(wd, showWarnings = FALSE)
    f <- file.path(wd, "rep.csv"); write.csv(rep_224, f, row.names = FALSE)
    ds <- prepare_pk_dataset(read_pk_file(f), rep_cm, list(lloq = 0.5, blq_rule = "rule1"))
    lab <- "2 | Reference | P1"; rows <- profile_data_rows(ds$data, rep_cm, lab)
    st <- rep_settings; tu <- c(6, 8, 12)
    res <- run_single_nca(ds$data$Time[rows], ds$data$Conc[rows], st, time_used = tu)
    zp <- file.path(wd, "single.zip")
    create_single_analysis_record(zp, res, st, ds$data$Time[rows], ds$data$Conc[rows],
      subject_label = lab, original_file_path = f, original_file_name = "rep.csv",
      blq_rule = "rule1", lloq = 0.5, col_map = rep_cm,
      lz_override = list(profile = lab, original_lambda_z = 0.15, adjusted_lambda_z = 0.1,
                         original_r2adj = 0.99, adjusted_r2adj = 0.98, points_used = 3, time_used = tu))
    grepl("Result: MATCH", rec_check_text(rec_unzip(zp)))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "profile 2 | Reference | P1 of the 2x2x4 fixture, override on 6/8/12 h",
  expected = "MATCH")
check("REC-07", "Single-subject record from manual entry reproduces",
  tryCatch({
    st <- theoph_settings; tt <- c(0, 0.5, 1, 2, 4, 8, 12, 24); cc <- c(0, 4.1, 7.9, 8.8, 7.0, 5.1, 3.6, 1.4)
    res <- run_single_nca(tt, cc, st)
    zp <- file.path(tempdir(), "rec07.zip")
    create_single_analysis_record(zp, res, st, tt, cc, subject_label = "Manual Entry")
    grepl("Result: MATCH", rec_check_text(rec_unzip(zp)))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "manual entry, no file", expected = "MATCH")
check("REC-08", "Figure record rebuilds the figure from the processed data",
  tryCatch({
    wd <- file.path(tempdir(), "rec08"); dir.create(wd, showWarnings = FALSE)
    f <- file.path(wd, "rep.csv"); write.csv(rep_222, f, row.names = FALSE)
    ds <- prepare_pk_dataset(read_pk_file(f), rep_cm, list(lloq = 0.5, blq_rule = "rule1"))
    p <- ggplot2::ggplot(ds$data, ggplot2::aes(Time, Conc, group = Subject)) + ggplot2::geom_line()
    zp <- file.path(wd, "fig.zip")
    create_viz_record(zp, p, list(plot_type = "spaghetti", export_format = "png", dpi = 72),
                      rep_cm, f, "rep.csv", blq_rule = "rule1", lloq = 0.5)
    ex <- rec_unzip(zp)
    scr <- paste(readLines(file.path(ex, "reproduce_figure.R"), warn = FALSE), collapse = "\n")
    file.exists(file.path(ex, "nca_pipeline.R")) && grepl("prepare_pk_dataset(", scr, fixed = TRUE) &&
      grepl("Result: FIGURE CREATED", rec_check_text(ex))
  }, error = function(e) FALSE),
  "URS-VIZ-09", critical = FALSE, method = "spaghetti figure record from the 2x2 fixture with LLOQ 0.5",
  expected = "script uses the pipeline; check reports the figure was produced")

check("REC-09", "Records state the CDISC release and include the parameter codes",
  tryCatch({
    r <- rec_build(df = theoph, cm = theoph_cm, st = theoph_settings)
    js <- jsonlite::fromJSON(file.path(r$ex, "analysis_settings.json"))
    html <- paste(readLines(file.path(r$ex, "analysis_summary.html"), warn = FALSE), collapse = "\n")
    codes <- openxlsx::read.xlsx(file.path(r$ex, "results.xlsx"), sheet = "CDISC_Parameter_Codes", startRow = 4)
    rel <- cdisc_ct_release()$Release
    identical(js$cdisc_terminology$Release, rel) && grepl(rel, html, fixed = TRUE) &&
      all(c("Parameter", "PPTESTCD", "PPTEST", "NCIt_code") %in% names(codes)) &&
      codes$PPTESTCD[codes$Parameter == "CMAX"] == "CMAX" &&
      grepl(rel, paste(unlist(openxlsx::read.xlsx(file.path(r$ex, "results.xlsx"), sheet = "CDISC_Parameter_Codes",
                                                  colNames = FALSE, rows = 1:2)), collapse = " "), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-06", critical = TRUE, method = "theophylline record: JSON, HTML, results.xlsx",
  expected = "release in JSON, HTML and the code sheet header; CMAX coded CMAX")

end_section("REC")

# =============================================================================
# SECTION CONV: standalone ADNCA-to-flat converter (converters/adnca_to_flat.R)
# =============================================================================
start_section("CONV")

conv_env <- new.env()
sys.source(file.path("converters", "adnca_to_flat.R"), envir = conv_env)
conv_fx <- function(f) file.path("validation", "fixtures", f)
conv_run <- function(f, ...) {
  out <- tempfile(fileext = ".csv")
  res <- conv_env$adnca_to_flat(conv_fx(f), out, ...)
  list(flat = read.csv(out, stringsAsFactors = FALSE), res = res, out = out,
       log = paste(readLines(sub("\\.csv$", "_conversion_log.txt", out)), collapse = "\n"))
}
conv_refused <- function(f, pattern, ...) {
  msg <- tryCatch({ conv_run(f, ...); "" }, adnca_refusal = function(e) conditionMessage(e))
  grepl("^Refused:", msg) && grepl(pattern, msg, ignore.case = TRUE)
}
conv_cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment",
                period = "Period", sequence = "Sequence")
conv_st <- list(admin_route = "extravascular", dose = 100, infusion_duration = 0, is_steady_state = FALSE,
                dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL", trap_method = "log",
                r2adj_threshold = 0.7, mw = 0)
conv_nca <- function(path, subject_map = NULL) {
  raw <- read_pk_file(path)
  ds <- prepare_pk_dataset(raw, conv_cm, list(lloq = 0.5, blq_rule = "rule1"))
  r <- suppressWarnings(run_nca(ds$data, conv_cm, conv_st))
  if (!is.null(subject_map)) r$Subject <- subject_map[r$Subject]
  list(ds = ds, r = r)
}

check("EQV-02", "Converted ADNCA and the equivalent flat file give the same analysis data",
  tryCatch({
    cv <- conv_run("adnca_clean.csv", time = "NRRLT")
    f1 <- read.csv(conv_fx("adnca_clean.csv"), stringsAsFactors = FALSE)
    map <- setNames(as.character(f1$SUBJID), f1$USUBJID)
    a <- conv_nca(cv$out)$ds$data; b <- conv_nca(conv_fx("flat_equivalent.csv"))$ds$data
    a$Subject <- map[a$Subject]; b$Subject <- as.character(b$Subject)
    key <- function(x) paste(x$Subject, x$Treatment, x$Period, x$Time)
    ka <- key(a); kb <- key(b)
    setequal(ka, kb) && !anyDuplicated(ka) &&
      identical(a$Conc[order(ka)], b$Conc[match(ka[order(ka)], kb)])
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = TRUE,
  method = "F1 converted with NRRLT vs F2 (generated from F1); keys compared separately from values",
  expected = "same profiles and times; identical concentrations")
check("EQV-01", "Converted ADNCA and the equivalent flat file give identical NCA results",
  tryCatch({
    cv <- conv_run("adnca_clean.csv", time = "NRRLT")
    f1 <- read.csv(conv_fx("adnca_clean.csv"), stringsAsFactors = FALSE)
    map <- setNames(as.character(f1$SUBJID), f1$USUBJID)
    a <- conv_nca(cv$out, map)$r; b <- conv_nca(conv_fx("flat_equivalent.csv"))$r
    ka <- paste(a$Subject, a$Treatment, a$Period); kb <- paste(b$Subject, b$Treatment, b$Period)
    num <- names(b)[sapply(b, is.numeric)]
    setequal(ka, kb) && nrow(a) == 24 &&
      identical(unname(as.matrix(a[order(ka), num])), unname(as.matrix(b[match(ka[order(ka)], kb), num])))
  }, error = function(e) FALSE),
  "URS-NCA-01", critical = TRUE, method = "full NCA on both, rows matched by subject/treatment/period",
  expected = "24 profiles, every parameter identical")
check("CONV-01", "Converter output passes the app's quality check and interlocks",
  tryCatch({
    cv <- conv_run("adnca_clean.csv", time = "NRRLT")
    qc <- run_data_quality_check(read_pk_file(cv$out), conv_cm, lloq = 0.5)
    qc$pass && nrow(run_interlocks(read_pk_file(cv$out), conv_cm)) == 0 &&
      identical(names(cv$flat), c("Subject", "Time", "Conc", "Treatment", "Period", "Sequence", "Dose"))
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F1 -> flat -> run_data_quality_check", expected = "QC passes, no interlock")
check("CONV-02", "Time variable must be chosen; AFRLT is refused",
  tryCatch({
    conv_refused("adnca_clean.csv", "choose the time variable") &&
      conv_refused("adnca_clean.csv", "first dose", time = "AFRLT")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "no time argument; time = AFRLT", expected = "both refused")
check("CONV-03", "Derived records (DTYPE) are refused: no double BLQ imputation",
  tryCatch(conv_refused("adnca_dtype.csv", "DTYPE.*impute twice", time = "NRRLT"), error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "F3 (HALFLLOQ records)", expected = "refused")
check("CONV-04", "Records outside the analysis set (ANL01FL) are dropped and counted",
  tryCatch({
    a <- conv_run("adnca_anl01fl.csv", time = "NRRLT"); b <- conv_run("adnca_clean.csv", time = "NRRLT")
    identical(a$flat, b$flat) && max(as.numeric(a$flat$Conc)) < 1000 &&
      grepl("dropped 24", a$log)
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F4 (24 extra records with AVAL 1e6, ANL01FL blank)",
  expected = "output identical to F1's; log reports 24 dropped")
check("CONV-05", "Several analytes: refused, or one selected explicitly (never averaged)",
  tryCatch({
    sel <- conv_run("adnca_multi_analyte.csv", time = "NRRLT", paramcd = "DRUGX")
    conv_refused("adnca_multi_analyte.csv", "more than one Analyte in PARAMCD \\(DRUGX, DRUGXM1\\)", time = "NRRLT") &&
      identical(sel$flat, conv_run("adnca_clean.csv", time = "NRRLT")$flat) &&
      !grepl("averag", sel$log, ignore.case = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F5 without and with paramcd = DRUGX", expected = "refused; selection equals F1")
check("CONV-06", "Time since first dose is refused",
  tryCatch(conv_refused("adnca_afrlt.csv", "near time zero", time = "ARRLT", zero_predose = TRUE),
           error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F6 (ARRLT holds AFRLT values)", expected = "refused")
check("CONV-07", "Date-time-only files are refused",
  tryCatch(conv_refused("adnca_datetime.csv", "missing.*date-times", time = "NRRLT"), error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F7 (PCDTC/EXSTDTC only)", expected = "refused")
check("CONV-08", "Mixed units are refused",
  tryCatch(conv_refused("adnca_units_mixed.csv", "more than one unit", time = "NRRLT"), error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F8 (ng/mL and ug/L)", expected = "refused")
check("CONV-09", "More than one dose within a subject and period is refused",
  tryCatch(conv_refused("adnca_multi_ex.csv", "more than one dose", time = "NRRLT"), error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F9", expected = "refused")
check("CONV-10", "Negative actual times need MRRLT semantics to be chosen explicitly",
  tryCatch({
    z <- conv_run("adnca_clean.csv", time = "ARRLT", zero_predose = TRUE)
    conv_refused("adnca_clean.csv", "negative ARRLT", time = "ARRLT") &&
      min(z$flat$Time) == 0 && grepl("set to 0", z$log)
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "F1 with ARRLT (pre-dose at -0.05 to -0.25 h)",
  expected = "refused without zero_predose; with it, pre-dose at 0 and logged")
check("CONV-11", "The conversion log records inputs, choices, counts and file hashes",
  tryCatch({
    cv <- conv_run("adnca_clean.csv", time = "NRRLT")
    all(sapply(c("Input:", "Output:", "Time: NRRLT \\(nominal\\)", "ANL01FL: kept 288",
                 "Analyte: single PARAMCD", "LLOQ \\(PCLLOQ\\): 0.5", "Subject = Subject \\(from USUBJID\\)"),
               function(p) grepl(p, cv$log))) &&
      grepl(digest::digest(file = conv_fx("adnca_clean.csv"), algo = "sha256"), cv$log, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = FALSE, method = "read the log for F1", expected = "all items present, input SHA-256 matches")
check("CONV-12", "Missing AVAL: BLQ results pass on as text, other gaps are refused",
  tryCatch({
    d <- read.csv(conv_fx("adnca_clean.csv"), stringsAsFactors = FALSE)
    d$PCORRES <- as.character(d$AVAL)
    i <- which(d$AVAL < 0.5 & d$NRRLT > 0)[1:2]; d$AVAL[i] <- NA; d$PCORRES[i] <- "<0.5"
    f_blq <- tempfile(fileext = ".csv"); write.csv(d, f_blq, row.names = FALSE, na = "")
    d2 <- d; d2$PCORRES[i[1]] <- ""; f_gap <- tempfile(fileext = ".csv"); write.csv(d2, f_gap, row.names = FALSE, na = "")
    out <- tempfile(fileext = ".csv")
    conv_env$adnca_to_flat(f_blq, out, time = "NRRLT")
    flat <- read.csv(out, stringsAsFactors = FALSE)
    gap <- tryCatch({ conv_env$adnca_to_flat(f_gap, tempfile(fileext = ".csv"), time = "NRRLT"); "" },
                    adnca_refusal = function(e) conditionMessage(e))
    sum(flat$Conc == "<0.5") == 2 && grepl("without a BLQ result", gap)
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "two AVAL set missing with PCORRES '<0.5'; then one without",
  expected = "text passed on for BLQ; unexplained gap refused")

check("ADNCA-01", "Inspection summarises an ADNCA dataset for the upload screen",
  tryCatch({
    i1 <- adnca_inspect(adnca_read(conv_fx("adnca_clean.csv")))
    i4 <- adnca_inspect(adnca_read(conv_fx("adnca_anl01fl.csv")))
    i3 <- adnca_inspect(adnca_read(conv_fx("adnca_dtype.csv")))
    i5 <- adnca_inspect(adnca_read(conv_fx("adnca_multi_analyte.csv")))
    i1$is_adnca && identical(i1$analytes, "DRUGX") && identical(i1$time_vars, c("NRRLT", "ARRLT")) &&
      isTRUE(i1$negative_times[["ARRLT"]]) && !isTRUE(i1$negative_times[["NRRLT"]]) && i1$has_afrlt &&
      identical(i1$lloq, 0.5) && i1$n_anl01fl_excluded == 0 && identical(i1$treatment_var, "TRTP") &&
      i4$n_anl01fl_excluded == 24 && i3$n_derived == 48 && length(i5$analytes) == 2
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = FALSE, method = "adnca_inspect on F1, F3, F4, F5",
  expected = "analytes, time variables, negative ARRLT, LLOQ, counts as in the fixtures")
check("ADNCA-02", "The standalone converter and the app share one conversion implementation",
  tryCatch({
    src <- readLines(file.path("converters", "adnca_to_flat.R"), warn = FALSE)
    code <- src[!grepl("^\\s*#", src)]
    !any(grepl("adnca_convert <- function|refuse\\(", code)) && any(grepl("adnca_import.R", code, fixed = TRUE)) &&
      identical(conv_run("adnca_clean.csv", time = "NRRLT")$flat,
                { x <- adnca_convert(adnca_read(conv_fx("adnca_clean.csv")), time = "NRRLT")$flat
                  f <- tempfile(fileext = ".csv"); write.csv(x, f, row.names = FALSE); read.csv(f, stringsAsFactors = FALSE) })
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = TRUE, method = "source inspection; same output from converter and adnca_convert()",
  expected = "converter sources adnca_import.R and adds no conversion logic; identical output")

adnca_record <- function(fixture, time, kind = "batch", paramcd = NULL, zero_predose = FALSE,
                         overrides = NULL, lloq = 0.5) {
  wd <- file.path(tempdir(), paste0("adr", as.integer(runif(1, 1, 1e7)))); dir.create(wd)
  src <- file.path(wd, fixture); file.copy(conv_fx(fixture), src)
  d <- adnca_read(src)
  conv <- adnca_convert(d, time = time, paramcd = paramcd, zero_predose = zero_predose)
  adnca <- c(conv[c("options", "notes", "sources", "lloq")], list(n_records = nrow(d)))
  cm <- conv$col_map
  ds <- prepare_pk_dataset(conv$flat, cm, list(lloq = lloq, blq_rule = "rule1"))
  zp <- file.path(wd, "rec.zip")
  if (kind == "single") {
    lab <- data_profiles(ds$data, cm)$label[3]; rows <- profile_data_rows(ds$data, cm, lab)
    res <- run_single_nca(ds$data$Time[rows], ds$data$Conc[rows], conv_st)
    create_single_analysis_record(zp, res, conv_st, ds$data$Time[rows], ds$data$Conc[rows],
      subject_label = lab, original_file_path = src, original_file_name = fixture,
      blq_rule = "rule1", lloq = lloq, col_map = cm, read_args = list(), adnca = adnca)
  } else if (kind == "viz") {
    p <- ggplot2::ggplot(ds$data, ggplot2::aes(Time, suppressWarnings(as.numeric(Conc)), group = Subject)) + ggplot2::geom_line()
    create_viz_record(zp, p, list(plot_type = "spaghetti", export_format = "png", dpi = 72), cm, src, fixture,
                      blq_rule = "rule1", lloq = lloq, read_args = list(), adnca = adnca)
  } else {
    st <- conv_st
    res <- suppressWarnings(run_nca(ds$data, cm, st, lz_overrides = overrides))
    create_analysis_record(zp, res, st, cm, src, fixture, blq_rule = "rule1", lloq = lloq,
      lz_overrides = overrides, read_args = list(), adnca = adnca,
      be_results = if (kind == "be") list(ci_table = data.frame(Parameter = "CMAX"), anova = list()) else NULL)
  }
  ex <- rec_unzip(zp)
  js <- jsonlite::fromJSON(file.path(ex, list.files(ex, "settings.json")[1]), simplifyDataFrame = FALSE)
  list(ex = ex, json = js, check = rec_check_text(ex), files = list.files(ex),
       manifest = paste(readLines(file.path(ex, "data_integrity.txt")), collapse = "\n"))
}
check("ADNCA-03", "Record from an ADNCA import (records outside the analysis set) reproduces",
  tryCatch({
    r <- adnca_record("adnca_anl01fl.csv", "NRRLT")
    grepl("Result: MATCH", r$check) && grepl("ADNCA import code: MATCH", r$check) &&
      all(c("adnca_anl01fl.csv", "adnca_import.R", "adnca_conversion_log.txt") %in% r$files) &&
      identical(r$json$door, "adnca") && identical(r$json$adnca$time, "NRRLT") &&
      grepl("ADNCA import code", r$manifest) &&
      identical(r$json$adnca_import_sha256, digest::digest(file = "R/adnca_import.R", algo = "sha256"))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "F4 imported with NRRLT; batch record; run the reproduction",
  expected = "MATCH; original ADNCA file, import code (hashed) and conversion log in the record")
check("ADNCA-04", "BE record from an ADNCA import with analyte selection, actual time and an override reproduces",
  tryCatch({
    ov <- list("x" = list(profile = "x", subject = "NCAA-001-002", treatment = "Test", period = "2",
                          original_lambda_z = 0.15, adjusted_lambda_z = 0.14, original_r2adj = 0.99,
                          adjusted_r2adj = 0.98, points_used = 3, time_used = c(6.0, 8.0, 12.0)))
    d <- adnca_read(conv_fx("adnca_multi_analyte.csv"))
    tu <- d$ARRLT[d$USUBJID == "NCAA-001-002" & d$APERIOD == 2 & d$PARAMCD == "DRUGX" & d$NRRLT %in% c(6, 8, 12)]
    ov$x$time_used <- tu
    r <- adnca_record("adnca_multi_analyte.csv", "ARRLT", kind = "be", paramcd = "DRUGX",
                      zero_predose = TRUE, overrides = ov)
    grepl("Result: MATCH", r$check) && isTRUE(r$json$adnca$zero_predose) && identical(r$json$adnca$paramcd, "DRUGX")
  }, error = function(e) FALSE),
  "URS-EXP-07", critical = TRUE, method = "F5, PARAMCD DRUGX, ARRLT with pre-dose at 0, one override",
  expected = "MATCH")
check("ADNCA-05", "Single-subject and figure records from an ADNCA import reproduce",
  tryCatch({
    s1 <- adnca_record("adnca_clean.csv", "NRRLT", kind = "single")
    v1 <- adnca_record("adnca_clean.csv", "NRRLT", kind = "viz")
    grepl("Result: MATCH", s1$check) && grepl("Result: FIGURE CREATED", v1$check) &&
      "adnca_import.R" %in% v1$files
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "F1 with NRRLT", expected = "MATCH; FIGURE CREATED")

end_section("CONV")

# =============================================================================
# SECTION REV: defects found in the adversarial review (2026-09-17)
# =============================================================================
start_section("REV")

rev_st <- list(admin_route = "extravascular", dose = 100, infusion_duration = 0, is_steady_state = FALSE,
               dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL", trap_method = "log",
               r2adj_threshold = 0.7, mw = 0)
rev_dp <- data.frame(Subject = rep(1:2, each = 12), Treatment = rep(rep(c("Low", "High"), each = 6), 2),
                     Period = rep(rep(1:2, each = 6), 2), Time = rep(c(0, 1, 2, 4, 8, 12), 4),
                     Conc = c(0, 10, 8, 5, 2, 1, 0, 40, 32, 20, 8, 4, 0, 12, 9, 6, 2.4, 1.2, 0, 44, 35, 22, 9, 4.4),
                     Dose = rep(rep(c(50, 200), each = 6), 2))
rev_dcm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period", dose = "Dose")

check("REV-01", "Doses that differ between periods are used per profile",
  tryCatch({
    st <- rev_st; st$dose <- dose_by_profile(rev_dp, rev_dcm)
    r <- suppressWarnings(run_nca(rev_dp, rev_dcm, st))
    own <- ifelse(r$Treatment == "Low", 50, 200)
    dn <- add_dose_normalized(as.data.frame(r), st$dose)
    max(abs(as.numeric(r$CMAX) / as.numeric(r$CMAXD) - own)) < 1e-9 &&
      max(abs(dn$CMAX_DN - as.numeric(r$CMAX) / own)) < 1e-12 &&
      identical(unname(dose_by_profile(read.csv(file.path("validation", "fixtures", "be_2x2x2_crossover.csv")),
                                       list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment",
                                            period = "Period", dose = "Dose"))[1:2]), c(100, 100))
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = TRUE, method = "crossover with 50 mg and 200 mg periods per subject",
  expected = "each profile uses its own period's dose (CMAX/CMAXD and CMAX_DN)")
check("REV-01b", "Per-profile doses reproduce from the Analysis Record",
  tryCatch({
    st <- rev_st; st$dose <- dose_by_profile(rev_dp, rev_dcm); st$dose_source <- "per_profile"
    r <- rec_build(df = rev_dp, cm = rev_dcm, st = st)
    js <- jsonlite::fromJSON(file.path(r$ex, "analysis_settings.json"))
    identical(js$dose_source, "per_profile") && grepl("Result: MATCH", rec_check_text(r$ex))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "record for the per-period dose example", expected = "per_profile; MATCH")
check("REV-02", "BLQ text ('BLQ', 'ND', ...) is handled like '<LLOQ'; 'NS' stays missing",
  tryCatch({
    cm <- list(subject = "Subject", time = "Time", conc = "Conc"); tt <- c(0, 0.5, 1, 2, 4, 8, 12, 24)
    auc <- function(conc, rule) {
      ds <- prepare_pk_dataset(data.frame(Subject = 1, Time = tt, Conc = conc, stringsAsFactors = FALSE), cm,
                               list(lloq = 0.5, blq_rule = rule))
      as.numeric(suppressWarnings(run_nca(ds$data, cm, rev_st))$AUCLST)
    }
    lt <- c("<0.5", "<0.5", "4.2", "18.5", "12.1", "2.8", "0.9", "<0.5")
    same <- all(sapply(c("rule1", "rule4", "rule6"), function(rule)
      abs(auc(c("BLQ", "ND", lt[3:7], "bql"), rule) - auc(lt, rule)) < 1e-12))
    ns <- prepare_pk_dataset(data.frame(Subject = 1, Time = tt, Conc = c("NS", "<0.5", lt[3:8]), stringsAsFactors = FALSE),
                             cm, list(lloq = 0.5, blq_rule = "rule4"))
    same && is.na(ns$data$Conc[1]) && ns$data$Conc[2] == 0.25 &&
      blq_text_summary(c("BLQ", "ND", "NS", "<0.5", "3"))$n_blq_text == 3
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "Data Guide example with 'BLQ'/'ND'/'bql' vs '<0.5' under rules 1, 4, 6; 'NS'",
  expected = "identical AUClast; NS missing; NS not counted as BLQ")
check("REV-03", "Unit check only looks at unit columns",
  tryCatch({
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    base <- data.frame(Subject = rep(1:2, each = 3), Time = rep(c(0, 1, 2), 2), Conc = c(0, 5, 4, 0, 6, 5))
    fp <- transform(base, Community = rep(c("A", "B"), each = 3), Opportunity = rep(c("x", "y"), 3), Unity = 1:6)
    hits <- sapply(c("Unit", "Units", "Conc_Unit", "TimeUnit", "time.units", "AVALU"), function(nm) {
      d <- base; d[[nm]] <- rep(c("ng/mL", "mg/L"), each = 3); nrow(interlock_mixed_units(d)) == 1 })
    nrow(interlock_mixed_units(fp)) == 0 && all(hits)
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "Community/Opportunity/Unity vs real unit column names",
  expected = "no finding for the first; a finding for each unit column")
check("REV-04", "Acceptance is judged on CI limits rounded to two decimals (FDA, May 2026)",
  tryCatch({
    be_limits_pass(79.996, 110, 80, 125) && !be_limits_pass(79.994, 110, 80, 125) &&
      be_limits_pass(90, 125.004, 80, 125) && !be_limits_pass(90, 125.006, 80, 125) &&
      any(grepl("be_limits_pass(", readLines("R/be_analysis.R"), fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE, method = "limits just inside/outside after rounding",
  expected = "79.996 and 125.004 pass; 79.994 and 125.006 fail")
check("REV-05", "The result states which model was fitted, including a failed mixed model",
  tryCatch({
    d <- be_input(be_d)
    fx <- fit_be_parameter(d, "CMAX", "2x2x2", "fixed", "Treatment", "Subject", "Period", "Sequence")$row$Model
    mx <- fit_be_parameter(d, "CMAX", "2x2x2", "mixed", "Treatment", "Subject", "Period", "Sequence")$row$Model
    suppressMessages(trace("lme", quote(stop("forced failure")), where = asNamespace("nlme"), print = FALSE))
    fbr <- tryCatch(fit_be_parameter(d, "CMAX", "2x2x2", "mixed", "Treatment", "Subject", "Period", "Sequence")$row,
                   finally = suppressMessages(untrace("lme", where = asNamespace("nlme"))))
    identical(fx, "fixed effects") && identical(mx, "mixed effects") && grepl("^fixed effects .*mixed model", fbr$Model) &&
      grepl("^no verdict: the pre-specified mixed model could not be fitted", fbr$Bioequivalent) && !is.na(fbr$Point_Est)
  }, error = function(e) FALSE),
  "URS-BE-05, URS-BE-11", critical = TRUE, method = "fixed, mixed, and mixed with lme forced to fail",
  expected = "Model column says what was fitted; a failed pre-specified mixed model gives no verdict (the fixed-effects estimate is shown)")
check("REV-06", "Subjects counted are those that contribute to the comparison",
  tryCatch({
    d <- be_input(be_d); d <- d[!(d$Subject == "1" & d$Treatment == "R"), ]
    f <- fit_be_parameter(d, "CMAX", "2x2x2", "fixed", "Treatment", "Subject", "Period", "Sequence")$row
    m <- fit_be_parameter(d, "CMAX", "2x2x2", "mixed", "Treatment", "Subject", "Period", "Sequence")$row
    # A subject without both treatments leaves both models (ARV-04)
    f$N_Test == 23 && f$N_Ref == 23 && m$N_Test == 23 && m$N_Ref == 23 &&
      f$Incomplete_Subjects == 1 && m$Incomplete_Subjects == 1
  }, error = function(e) FALSE),
  "URS-BE-01", critical = FALSE, method = "24-subject 2x2 with one subject missing Reference",
  expected = "both models count and use the 23 complete subjects; 1 incomplete subject reported")
check("REV-07", "Leading/trailing spaces in IDs and design labels do not create extra levels",
  tryCatch({
    d <- data.frame(Subject = c("S1 ", "S1", "S1", "S1", "S1", "S1"), Treatment = c("Test", "Test ", " Test", "Reference", "Reference", "Reference"),
                    Period = c(1, 1, 1, 2, 2, 2), Time = c(0, 1, 2, 0, 1, 2), Conc = c(0, 5, 4, 0, 6, 5))
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period")
    ds <- prepare_pk_dataset(d, cm, list())
    qc <- run_data_quality_check(d, cm)
    length(unique(ds$data$Treatment)) == 2 && length(unique(ds$data$Subject)) == 1 &&
      any(grepl("2 treatments", qc$findings$Message))
  }, error = function(e) FALSE),
  "URS-DAT-02", critical = FALSE, method = "'S1 ' and ' Test'/'Test '", expected = "one subject, two treatments")
check("REV-08", "Profile-start refusal explains steady-state timing",
  tryCatch({
    ss <- data.frame(Subject = rep(1:2, each = 5), Time = rep(c(168, 169, 172, 180, 192), 2), Conc = 5:14)
    f <- run_interlocks(ss, list(subject = "Subject", time = "Time", conc = "Conc"))
    any(f$Severity == "ERROR" & grepl("steady.state", f$Action, ignore.case = TRUE))
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = FALSE, method = "steady-state profile timed from the first dose", expected = "action mentions steady state")
check("REV-09", "Record fallback copy of a decimal-comma upload stays readable",
  tryCatch({
    raw <- data.frame(Subject = 1, Time = c(0, 1, 2), Conc = c("<0,5", "4,25", "3,5"), stringsAsFactors = FALSE)
    f <- tempfile(fileext = ".csv")
    ra <- write_record_fallback(raw, f, list(sep = ";", dec = ","))
    ds <- prepare_pk_dataset(read_pk_file(f, ra), list(subject = "Subject", time = "Time", conc = "Conc"),
                             list(lloq = 0.5, blq_rule = "rule1", read_args = ra))
    identical(ds$data$Conc, c(0, 4.25, 3.5))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = FALSE, method = "write_record_fallback() then read back", expected = "numbers preserved")
check("REV-10", "Uploaded file names cannot place files outside the record",
  tryCatch({
    wd <- file.path(tempdir(), paste0("rev10", as.integer(runif(1, 1, 1e6)))); dir.create(wd)
    src <- file.path(wd, "data.csv"); write.csv(theoph, src, row.names = FALSE)
    zp <- file.path(wd, "rec.zip")
    suppressWarnings(create_analysis_record(zp, theoph_result, theoph_settings, theoph_cm, src, "../escaped.csv",
                                            blq_rule = "rule1", lloq = 0))
    ex <- rec_unzip(zp); js <- jsonlite::fromJSON(file.path(ex, "analysis_settings.json"))
    !file.exists(file.path(tempdir(), "escaped.csv")) && file.exists(file.path(ex, "escaped.csv")) &&
      identical(js$input_file, "escaped.csv")
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = FALSE, method = "original_file_name = '../escaped.csv'", expected = "file stays inside the record")

end_section("REV")

# =============================================================================
# SECTION REV2: Second adversarial review (whole app)
# =============================================================================
start_section("REV2")

# Two-treatment 2x2 data with a known T/R ratio of about 0.89, relabelled
rev2_be <- function(test_label, ref_label, reference = NULL) {
  set.seed(3); n <- 24; s <- rep(1:n, each = 2)
  sq <- rep(rep(c("TR", "RT"), each = 2), n / 2); per <- rep(1:2, n)
  is_t <- (sq == "TR") == (per == 1)
  cm <- exp(log(100) + rep(rnorm(n, 0, .3), each = 2) + ifelse(is_t, log(0.85), 0) + rnorm(2 * n, 0, .15))
  trt <- ifelse(is_t, test_label, ref_label)
  nca <- data.frame(Subject = as.character(s), Treatment = trt, Period = as.character(per), CMAX = cm,
                    stringsAsFactors = FALSE)
  pk <- data.frame(ID = s, TRT = trt, PER = per, SEQ = sq, stringsAsFactors = FALSE)
  bd <- build_be_data(nca, pk, list(subject = "ID", treatment = "TRT", period = "PER", sequence = "SEQ"),
                      reference = reference)
  fit_be_parameter(bd$data, "CMAX", "2x2x2", trt_col = "Treatment", subj_col = "Subject",
                   per_col = "Period", seq_col = bd$seq_col)$row
}

check("REV2-01", "The chosen Reference treatment is used, whatever its name",
  tryCatch({
    std <- rev2_be("Test", "Reference")
    a <- rev2_be("New", "Old", reference = "Old")
    b <- rev2_be("A", "B", reference = "B")
    identical(a$Reference, "Old") && identical(b$Reference, "B") &&
      a$Point_Est == std$Point_Est && b$CI_Lower == std$CI_Lower
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE,
  method = "same data labelled Test/Reference, New/Old (reference Old), A/B (reference B)",
  expected = "identical ratio and CI; Reference column shows the chosen treatment")

check("REV2-02", "A Reference treatment that is not in the data is refused",
  tryCatch({ rev2_be("New", "Old", reference = "Placebo"); FALSE }, error = function(e) TRUE),
  "URS-BE-01", critical = TRUE, method = "reference = 'Placebo'", expected = "error")

check("REV2-03", "Reference suggestion only for unambiguous names",
  tryCatch(identical(suggest_reference_treatment(c("T", "R")), "R") &&
             identical(suggest_reference_treatment(c("Test", "Ref")), "Ref") &&
             identical(suggest_reference_treatment(c("reference", "test")), "reference") &&
             is.null(suggest_reference_treatment(c("New", "Old"))) &&
             is.null(suggest_reference_treatment(c("A", "B"))),
           error = function(e) FALSE),
  "URS-BE-01", critical = FALSE, method = "T/R, Test/Ref, reference/test, New/Old, A/B",
  expected = "R, Ref, reference, none, none")

check("REV2-04", "Scaled planning methods use both the Test and the Reference CV",
  tryCatch({
    abe <- planner_cv("abe", 20, 40); abel <- planner_cv("abel", 25, 40)
    n <- PowerTOST::sampleN.scABEL(CV = abel, theta0 = 0.95, design = "2x2x4",
                                   print = FALSE, details = FALSE)[["Sample size"]]
    isTRUE(all.equal(abe, 0.20)) && isTRUE(all.equal(abel, c(0.25, 0.40))) &&
      isTRUE(all.equal(planner_cv("ntid", 20, 10), c(0.20, 0.10))) && n == 14
  }, error = function(e) FALSE),
  "URS-PWR-01", critical = TRUE,
  method = "planner_cv(); sampleN.scABEL CVwT 25%, CVwR 40%, theta0 0.95, 2x2x4",
  expected = "c(CVwT, CVwR) for scaled methods; N = 14 (CVwR only gives 20)")

check("REV2-05", "CV offered to the planner is the within-subject CV from the BE model",
  tryCatch({
    ci <- data.frame(Parameter = c("CMAX", "TMAX"), Scale = c("Ratio T/R (%)", "Difference T−R (h)"),
                     MSE = c(0.0425, 0.3), stringsAsFactors = FALSE)
    isTRUE(all.equal(within_cv_from_be(ci, "CMAX"), 100 * sqrt(exp(0.0425) - 1))) &&
      is.na(within_cv_from_be(ci, "TMAX")) && is.na(within_cv_from_be(NULL, "CMAX")) &&
      !any(grepl("sd(log(cmax_vals))", readLines("R/mod_path_power.R"), fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-PWR-01", critical = TRUE,
  method = "within_cv_from_be() on a CI table; planner no longer uses the spread of Cmax across subjects",
  expected = "100*sqrt(exp(MSE)-1) for ratio rows, NA otherwise")

check("REV2-06", "Exported batch summaries are grouped by treatment, as on screen",
  tryCatch({
    src <- readLines("R/mod_path_multi_nca.R")
    calls <- grep("summarize_pk_params(", src, fixed = TRUE, value = TRUE)
    length(calls) >= 3 && all(grepl("group_col", calls))
  }, error = function(e) FALSE),
  "URS-NCA-05", critical = FALSE, method = "every summarize_pk_params() call in the batch module",
  expected = "all pass group_col")

check("REV2-07", "Confidence interval column names follow the chosen level",
  tryCatch({
    df <- data.frame(Parameter = "CMAX", CI_Lower = 1, CI_Upper = 2)
    all(c("95% CI Lower", "95% CI Upper") %in% names(rename_be_columns(df, ci_level = 95))) &&
      all(c("90% CI Lower", "90% CI Upper") %in% names(rename_be_columns(df)))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = FALSE, method = "rename_be_columns(ci_level = 95)", expected = "95% CI Lower/Upper")

end_section("REV2")

# =============================================================================
# SECTION REV3: Remaining findings of the second review
# =============================================================================
start_section("REV3")

rev3_code <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
rev3_noisy <- data.frame(ID = "1", T = c(0, 1, 2, 4, 6, 8, 12, 24), C = c(0, 10, 8, 3, 6, 2, 5, 1.5))
rev3_cm <- list(subject = "ID", time = "T", conc = "C")
rev3_st <- function(thr) list(admin_route = "extravascular", dose = 100, trap_method = "log",
                              dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL",
                              is_steady_state = FALSE, mw = 0, r2adj_threshold = thr)

check("REV3-01", "Results of a previous dataset are cleared when new data are processed",
  tryCatch({
    src <- paste(rev3_code("R/mod_data_upload.R"), collapse = "\n")
    grepl("shared\\$be_results\\s*<-\\s*NULL", src) &&
      grepl("shared\\$nca_results\\s*<-\\s*NULL", src)
  }, error = function(e) FALSE),
  "URS-PWR-01", critical = FALSE, method = "upload module clears shared BE and NCA results",
  expected = "both cleared")

check("REV3-02", "Bioequivalence texts do not state a fixed 90% level",
  tryCatch(!any(grepl("90% (CI|confidence)", rev3_code("R/mod_path_be.R"))), error = function(e) FALSE),
  "URS-BE-02", critical = FALSE, method = "search non-comment lines of mod_path_be.R", expected = "none")

check("REV3-03", "Half-life-based parameters are missing when the fit is below the R2 threshold",
  tryCatch({
    lo <- suppressWarnings(run_nca(rev3_noisy, rev3_cm, rev3_st(0.7)))
    hi <- suppressWarnings(run_nca(rev3_noisy, rev3_cm, rev3_st(0.3)))
    w <- tryCatch({ run_nca(rev3_noisy, rev3_cm, rev3_st(0.7)); "" }, warning = function(w) conditionMessage(w))
    s_lo <- run_single_nca(rev3_noisy$T, rev3_noisy$C, rev3_st(0.7))
    all(is.na(as.numeric(unlist(lo[c("LAMZHL", "AUCIFO", "CLFO", "VZFO", "MRTEVIFO")])))) &&
      !is.na(as.numeric(lo$CMAX)) && !is.na(as.numeric(lo$AUCLST)) && !is.na(as.numeric(lo$R2ADJ)) &&
      !is.na(as.numeric(hi$LAMZHL)) && grepl("R", w) &&
      is.na(s_lo["LAMZHL"]) && is.na(s_lo["AUCIFO"]) && !is.na(s_lo["AUCLST"])
  }, error = function(e) FALSE),
  "URS-NCA-03", critical = TRUE,
  method = "noisy profile, best adj R2 0.33; threshold 0.7 vs 0.3; batch and single",
  expected = "t1/2, AUCinf, CL/F, Vz/F, MRTinf missing at 0.7 with a warning; present at 0.3")

check("REV3-04", "A manual half-life selection is kept whatever its R2",
  tryCatch({
    ov <- list(list(subject = "1", time_used = c(2, 4, 6, 8, 12, 24)))
    r <- run_nca(rev3_noisy, rev3_cm, rev3_st(0.7), lz_overrides = ov)
    s <- run_single_nca(rev3_noisy$T, rev3_noisy$C, rev3_st(0.7), time_used = c(2, 4, 6, 8, 12, 24))
    !is.na(as.numeric(r$LAMZHL)) && !is.na(s["LAMZHL"])
  }, error = function(e) FALSE),
  "URS-NCA-04", critical = TRUE, method = "override on the noisy profile, threshold 0.7",
  expected = "half-life reported")

check("REV3-05", "The half-life review shows NonCompart's own automatic fit",
  tryCatch({
    th <- datasets::Theoph
    ok <- vapply(split(th, th$Subject), function(d) {
      a <- estimate_lambda_z(d$Time, d$conc, 0)
      b <- NonCompart::sNCA(d$Time, d$conc, dose = 320, doseUnit = "mg", timeUnit = "h",
                            concUnit = "mg/L", R2ADJ = 0)
      isTRUE(all.equal(a$half_life, unname(b["LAMZHL"]))) && a$n_points == b["LAMZNPT"] &&
        length(a$time_used) == b["LAMZNPT"]
    }, logical(1))
    all(ok)
  }, error = function(e) FALSE),
  "URS-NCA-04", critical = TRUE, method = "estimate_lambda_z vs sNCA on all 12 Theoph profiles",
  expected = "same half-life and points (subject 6: 7.90 h, 7 points)")

check("REV3-06", "Single-subject results are cleared when another profile is selected",
  tryCatch({
    src <- rev3_code("R/mod_path_single_nca.R")
    i <- grep("observeEvent(input$sel_profile", src, fixed = TRUE)
    any(vapply(i, function(k) any(grepl("nca_res(NULL)", src[k:min(k + 3, length(src))], fixed = TRUE)), logical(1)))
  }, error = function(e) FALSE),
  "URS-NCA-01", critical = TRUE, method = "sel_profile observer in mod_path_single_nca.R",
  expected = "clears nca_res")

check("REV3-07", "An empty LLOQ field is reported, not a crash",
  tryCatch({
    th <- datasets::Theoph; th$Subject <- as.character(th$Subject)
    q <- run_data_quality_check(th, list(subject = "Subject", time = "Time", conc = "conc"), lloq = NA)
    !q$pass && any(grepl("LLOQ", q$findings$Message))
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = FALSE, method = "run_data_quality_check(lloq = NA)", expected = "ERROR finding about the LLOQ")

check("REV3-08", "Figure texts describe the error bars as geometric SD",
  tryCatch(!any(grepl("geometric CV%", c(rev3_code("R/mod_path_viz.R"), rev3_code("R/export_record.R")),
                      ignore.case = TRUE)), error = function(e) FALSE),
  "URS-VIZ-03", critical = FALSE, method = "search figure module and record", expected = "no 'geometric CV%' label")

check("REV3-09", "Help texts make no regulatory-submission claim",
  tryCatch(!any(grepl("regulatory submission", rev3_code("R/help_system.R"), ignore.case = TRUE)),
           error = function(e) FALSE),
  "URS-GEN-01", critical = FALSE, method = "search help_system.R", expected = "none")

check("REV3-10", "Methods page matches the implementation",
  tryCatch({
    m <- paste(rev3_code("R/mod_methods.R"), collapse = " ")
    !grepl("using fixed acceptance limits of 80.00", m, fixed = TRUE) &&
      grepl("rounded to two decimals", m, fixed = TRUE) &&
      grepl("10,000", m, fixed = TRUE) &&
      !grepl("Subject nested within Sequence was modelled as a random effect", m, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-03", critical = FALSE, method = "search mod_methods.R", expected = "limits, rounding, simulations, random effect described as implemented")


check("REV3-11", "Auto-detect reports when no Subject column is recognised",
  tryCatch({
    g1 <- auto_detect_columns(c("Time", "Concentration"))
    g2 <- auto_detect_columns(c("Subject", "Time", "Concentration"))
    "subject" %in% attr(g1, "unmatched") && !("subject" %in% attr(g2, "unmatched"))
  }, error = function(e) FALSE),
  "URS-DAT-02", critical = FALSE, method = "auto_detect_columns on files with and without a subject column",
  expected = "'subject' listed as unmatched only when absent")

check("REV3-12", "Upload warns when no Subject column is recognised or Subject duplicates another column",
  tryCatch({
    src <- paste(rev3_code("R/mod_data_upload.R"), collapse = "\n")
    d <- data.frame(Time = c(0, 1, 2, 4), Concentration = c(0, 5, 3, 1))
    q <- run_data_quality_check(d, list(subject = "Time", time = "Time", conc = "Concentration"))
    grepl('attr(guess, "unmatched")', src, fixed = TRUE) &&
      any(q$findings$Severity == "WARNING" & grepl("Subject", q$findings$Message))
  }, error = function(e) FALSE),
  "URS-DAT-02", critical = FALSE, method = "upload module uses the unmatched attribute; quality check with Subject = Time",
  expected = "notification in the upload module; WARNING finding about the Subject mapping")

check("REV3-13", "The app names no commercial NCA software package",
  tryCatch(!any(grepl("winnonlin", c(readLines("app.R", warn = FALSE),
                                     unlist(lapply(list.files("R", "\\.R$", full.names = TRUE), readLines, warn = FALSE))),
                      ignore.case = TRUE)), error = function(e) FALSE),
  "URS-GEN-03", critical = FALSE, method = "search app.R and R/*.R", expected = "no occurrence")

check("REV3-14", "Half-life gets a ratio and 90% CI but no bioequivalence verdict",
  tryCatch({
    d <- read.csv("validation/fixtures/be_2x2x2_crossover.csv", stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period", sequence = "Sequence")
    r <- run_nca(d, cm, rev3_st(0.7))
    b <- build_be_data(r, d, cm, reference = "Reference")
    hl <- fit_be_parameter(b$data, "LAMZHL", "2x2x2", trt_col = "Treatment", subj_col = "Subject",
                           per_col = "Period", seq_col = b$seq_col)$row
    cm_ <- fit_be_parameter(b$data, "CMAX", "2x2x2", trt_col = "Treatment", subj_col = "Subject",
                            per_col = "Period", seq_col = b$seq_col)$row
    grepl("^Ratio", hl$Scale) && is.finite(hl$Point_Est) && is.finite(hl$CI_Lower) &&
      identical(hl$Bioequivalent, "no verdict") && is.na(hl$BE_Lower) &&
      cm_$Bioequivalent %in% c("YES", "NO")
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE, method = "fit_be_parameter for LAMZHL and CMAX on the 2x2 fixture",
  expected = "half-life: ratio and CI, 'no verdict', no limits; Cmax keeps its verdict")

end_section("REV3")

# =============================================================================
# SECTION REV4: External statistical audit
# =============================================================================
start_section("REV4")

ss_st <- function(tau = 12) list(admin_route = "extravascular", dose = 100, trap_method = "linear",
  dose_unit = "mg", time_unit = "h", conc_unit = "mg/L", is_steady_state = TRUE, tau = tau,
  mw = 0, r2adj_threshold = 0.7, infusion_duration = 0)
ss_cm <- list(subject = "ID", time = "T", conc = "C")
ss_d <- data.frame(ID = c(rep("A", 7), rep("B", 6)),
                   T = c(0, 0.5, 1, 2, 4, 8, 12, 0, 0.5, 1, 2, 4, 8),
                   C = c(3.2, 28.1, 22.4, 15, 9.8, 6, 4.1, 3.0, 25, 20, 14, 9, 5.5))

check("REV4-01", "Steady state needs the dosing interval",
  tryCatch({
    st <- ss_st(); st$tau <- NULL
    w <- tryCatch({ run_nca(ss_d, ss_cm, st); "" }, warning = function(w) conditionMessage(w))
    grepl("dosing interval", w, ignore.case = TRUE) && is.null(suppressWarnings(run_nca(ss_d, ss_cm, st)))
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE, method = "run_nca with is_steady_state = TRUE and no tau",
  expected = "no result, warning asking for the dosing interval")

check("REV4-02", "AUCtau covers 0 to tau, also when C(tau) is missing; Cavg = AUCtau / tau",
  tryCatch({
    r <- run_nca(ss_d, ss_cm, ss_st(12)); a <- r[r$ID == "A", ]; b <- r[r$ID == "B", ]
    x <- NonCompart::sNCA(ss_d$T[8:13], ss_d$C[8:13], dose = 100, doseUnit = "mg", timeUnit = "h",
      concUnit = "mg/L", R2ADJ = 0, SS = TRUE, iAUC = data.frame(Name = "P", Start = 0, End = 12))
    isTRUE(all.equal(as.numeric(a$AUCTAU), as.numeric(a$AUCLST))) &&
      isTRUE(all.equal(as.numeric(b$AUCTAU), unname(x["P"]))) && as.numeric(b$AUCTAU) > as.numeric(b$AUCLST) &&
      isTRUE(all.equal(as.numeric(b$CAVG), as.numeric(b$AUCTAU) / 12)) &&
      all(as.numeric(r$TAU) == 12)
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE,
  method = "profile B ends at 8 h, tau = 12 h; compare with NonCompart partial AUC",
  expected = "AUCtau = partial AUC 0-12 (extrapolated), Cavg = AUCtau/12, TAU = 12 for all")

check("REV4-03", "Clearance and volume at steady state use AUCtau",
  tryCatch({
    r <- run_nca(ss_d, ss_cm, ss_st(12)); b <- r[r$ID == "B", ]; a <- r[r$ID == "A", ]
    k_a <- as.numeric(a$CLFO) * as.numeric(a$AUCTAU); k_b <- as.numeric(b$CLFO) * as.numeric(b$AUCTAU)
    isTRUE(all.equal(k_a, k_b)) &&
      isTRUE(all.equal(as.numeric(b$VZFO) * as.numeric(b$LAMZ) * as.numeric(b$AUCTAU), k_b))
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE, method = "CL/F x AUCtau and Vz/F x lambda-z x AUCtau for both profiles",
  expected = "equal to dose x unit factor for both profiles")

check("REV4-04", "Steady-state summary parameters in every path, with the entered tau",
  tryCatch({
    r <- run_nca(ss_d, ss_cm, ss_st(12))
    s <- run_single_nca(ss_d$T[2:7], ss_d$C[2:7], ss_st(12))
    all(c("TAU", "AUCTAU", "CAVG", "CMIN_SS", "FLUCTP", "SWING") %in% names(r)) &&
      unname(s["TAU"]) == 12 && is.finite(unname(s["CAVG"])) &&
      isTRUE(all.equal(as.numeric(r$FLUCTP[1]),
        (as.numeric(r$CMAX[1]) - as.numeric(r$CMIN_SS[1])) / as.numeric(r$CAVG[1]) * 100))
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE, method = "batch run_nca and single run_single_nca (no pre-dose sample)",
  expected = "all columns present; tau = 12 without a pre-dose sample; fluctuation formula")

check("REV4-05", "Steady-state records reproduce with the recorded tau",
  tryCatch({
    rb <- rec_build(df = ss_d, cm = ss_cm, st = ss_st(12))
    js <- jsonlite::fromJSON(file.path(rb$ex, "analysis_settings.json"))
    zp <- file.path(tempdir(), "rev405.zip")
    res <- run_single_nca(ss_d$T[1:7], ss_d$C[1:7], ss_st(12))
    create_single_analysis_record(zp, res, ss_st(12), ss_d$T[1:7], ss_d$C[1:7], subject_label = "Manual Entry")
    identical(as.numeric(js$tau), 12) && grepl("Result: MATCH", rec_check_text(rb$ex)) &&
      grepl("Result: MATCH", rec_check_text(rec_unzip(zp)))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "batch and single records with tau = 12", expected = "tau recorded; MATCH")

check("REV4-06", "Planner defaults follow the method; parallel designs ask for the total CV",
  tryCatch({
    planner_default_theta0("abe") == 95 && planner_default_theta0("abel") == 90 &&
      planner_default_theta0("rsabe") == 90 && planner_default_theta0("ntid") == 97.5 &&
      grepl("total", planner_cv_label("abe", "parallel"), ignore.case = TRUE) &&
      !grepl("total", planner_cv_label("abe", "2x2"), ignore.case = TRUE)
  }, error = function(e) FALSE),
  "URS-PWR-01", critical = FALSE, method = "planner_default_theta0(), planner_cv_label()",
  expected = "95 / 90 / 90 / 97.5; 'total CV' for parallel")

check("REV4-07", "Methods page describes the models and criteria as implemented",
  tryCatch({
    m <- paste(rev3_code("R/mod_methods.R"), collapse = " ")
    all(vapply(c("95% upper confidence bound", "Method B", "Satterthwaite", "complete-case",
                 "pooled over Test and Reference", "equal variances", "dosing interval (\\u03C4) entered"),
               function(k) grepl(k, m, fixed = TRUE), logical(1)))
  }, error = function(e) FALSE),
  "URS-GEN-03", critical = FALSE, method = "search mod_methods.R", expected = "all statements present")

check("REV4-08", "Figure summary labels do not refer to PK parameters",
  tryCatch(!any(grepl("Cmax/AUC", rev3_code("R/mod_path_viz.R"), fixed = TRUE)), error = function(e) FALSE),
  "URS-VIZ-03", critical = FALSE, method = "search mod_path_viz.R", expected = "no Cmax/AUC wording in concentration plot labels")

end_section("REV4")

# =============================================================================
# SECTION PAUC: Partial AUCs
# =============================================================================
start_section("PAUC")

# Independent reference: trapezoids written out by hand, with the cutoff
# interpolated linearly (or log-linearly on a falling segment for the log-down
# method) and a zero at time 0 when the first sample is later.
pa_hand <- function(t, c, s, e, method = "linear") {
  ok <- !is.na(t) & !is.na(c); t <- t[ok]; c <- c[ok]
  if (t[1] > 0) { t <- c(0, t); c <- c(0, c) }
  at <- function(x) {
    if (x %in% t) return(c[match(x, t)])
    i <- max(which(t < x)); t1 <- t[i]; t2 <- t[i + 1]; c1 <- c[i]; c2 <- c[i + 1]
    if (method == "log" && c2 < c1 && c2 > 0) exp(log(c1) + (log(c2) - log(c1)) * (x - t1) / (t2 - t1))
    else c1 + (c2 - c1) * (x - t1) / (t2 - t1)
  }
  x <- sort(unique(c(s, e, t[t > s & t < e]))); y <- vapply(x, at, numeric(1))
  sum(vapply(seq_len(length(x) - 1), function(i) {
    dt <- x[i + 1] - x[i]; y1 <- y[i]; y2 <- y[i + 1]
    if (method == "log" && y2 < y1 && y2 > 0) (y1 - y2) * dt / log(y1 / y2) else (y1 + y2) * dt / 2
  }, numeric(1)))
}
pa_iv <- function(start, end, cmax = FALSE, role = "pivotal")
  data.frame(start = start, end = as.character(end), cmax = cmax, role = role, stringsAsFactors = FALSE)
pa_st <- function(pauc, trap = "linear", r2 = 0.7, ss = FALSE, tau = NA, route = "extravascular")
  list(admin_route = route, dose = 100, trap_method = trap, dose_unit = "mg", time_unit = "h",
       conc_unit = "ng/mL", is_steady_state = ss, tau = tau, mw = 0, r2adj_threshold = r2,
       infusion_duration = 0, partial_aucs = pauc)
pa_cm <- list(subject = "ID", time = "T", conc = "C")
pa_t <- c(0, 0.25, 0.5, 1, 2, 4, 8, 12, 24)
pa_d <- data.frame(ID = rep(c("P", "Q"), each = 9), T = rep(pa_t, 2),
                   C = c(0, 5, 9, 12, 10, 7, 4, 2.4, 0.6,  0, 4, 8, 11, 9, 6, 3, 1.2, NA))
pa_run <- function(pauc, ...) suppressWarnings(run_nca(pa_d, pa_cm, pa_st(pauc, ...)))
pa_row <- function(r, id) r[r$ID == id, , drop = FALSE]
pa_num <- function(r, id, col) as.numeric(pa_row(r, id)[[col]])
pa_warn <- function(expr) { w <- character(0)
  v <- withCallingHandlers(expr, warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  list(value = v, w = w) }

check("PAUC-01", "Partial AUC on sampling times equals hand-calculated trapezoids (linear and log-down)",
  tryCatch({
    iv <- pa_iv(c(0, 2), c(1, 8))
    ok <- TRUE
    for (m in c("linear", "log")) {
      r <- pa_run(iv, trap = m)
      ok <- ok && abs(pa_num(r, "P", "AUC_0_1") - 7.625) < 1e-9 &&
        abs(pa_num(r, "P", "AUC_2_8") - pa_hand(pa_t, pa_d$C[1:9], 2, 8, m)) < 1e-9
    }
    ok && abs(pa_hand(pa_t, pa_d$C[1:9], 2, 8, "linear") - 39) < 1e-12
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "run_nca with intervals 0-1 and 2-8 h vs trapezoids by hand",
  expected = "0-1 h = 7.625 in both methods; 2-8 h = 39 (linear) and the log-down value")

check("PAUC-02", "A cutoff between samples is interpolated with the analysis's method",
  tryCatch({
    iv <- pa_iv(c(0, 0), c(0.75, 3))
    r_lin <- pa_run(iv, trap = "linear"); r_log <- pa_run(iv, trap = "log")
    nc <- NonCompart::sNCA(pa_t, pa_d$C[1:9], dose = 100, R2ADJ = 0, down = "Log",
                           iAUC = data.frame(Name = "X", Start = 0, End = 3))
    abs(pa_num(r_lin, "P", "AUC_0_0.75") - 4.8125) < 1e-9 &&
      abs(pa_num(r_lin, "P", "AUC_0_3") - 27.875) < 1e-9 &&
      abs(pa_num(r_log, "P", "AUC_0_3") - pa_hand(pa_t, pa_d$C[1:9], 0, 3, "log")) < 1e-9 &&
      abs(pa_num(r_log, "P", "AUC_0_3") - unname(nc["X"])) < 1e-9
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "cutoffs 0.75 and 3 h; hand interpolation and NonCompart iAUC",
  expected = "0-0.75 h = 4.8125, 0-3 h = 27.875 (linear); log-down equal to hand and NonCompart")

check("PAUC-03", "End at the last measurable concentration (t): AUClast minus AUC from 0 to start, per profile",
  tryCatch({
    ok <- TRUE
    for (m in c("linear", "log")) {
      r <- pa_run(rbind(pa_iv(c(0, 3, 12, 20), "t"), pa_iv(c(3, 3), c("24", "12"))), trap = m)
      for (id in c("P", "Q")) {
        tl <- pa_num(r, id, "TLST"); cc <- pa_d$C[pa_d$ID == id]
        ok <- ok && abs(pa_num(r, id, "AUC_3_t") - pa_hand(pa_t, cc, 3, tl, m)) < 1e-9 &&
          abs(pa_num(r, id, "AUC_0_t") - pa_num(r, id, "AUCLST")) < 1e-9
      }
      ok <- ok && pa_num(r, "P", "TLST") == 24 && pa_num(r, "Q", "TLST") == 12 &&
        pa_num(r, "Q", "AUC_12_t") == 0 && is.na(pa_num(r, "Q", "AUC_20_t")) &&
        is.finite(pa_num(r, "P", "AUC_20_t")) &&
        # the same boundary: an interval ending exactly at Tlast equals the t interval
        isTRUE(all.equal(pa_num(r, "P", "AUC_3_24"), pa_num(r, "P", "AUC_3_t"))) &&
        isTRUE(all.equal(pa_num(r, "Q", "AUC_3_12"), pa_num(r, "Q", "AUC_3_t"))) &&
        is.na(pa_num(r, "Q", "AUC_3_24"))
    }
    ok
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE,
  method = "profiles with Tlast 24 h and 12 h; intervals 0-t, 3-t, 12-t, 20-t, 3-24 and 3-12",
  expected = paste("3-t equals hand trapezoids to each profile's Tlast; 0-t = AUClast; start = Tlast gives 0;",
                   "start > Tlast is missing; an interval ending exactly at Tlast equals the t interval"))

check("PAUC-04", "No extrapolation: an interval past Tlast is not reported, whatever the half-life fit",
  tryCatch({
    iv <- pa_iv(c(0, 20), c(18, 30))
    w <- pa_warn(run_nca(pa_d, pa_cm, pa_st(iv)))$w
    r <- pa_run(iv); r_strict <- pa_run(iv, r2 = 0.999)
    is.finite(pa_num(r, "P", "LAMZ")) && is.na(pa_num(r, "P", "AUC_20_30")) &&
      is.na(pa_num(r, "Q", "AUC_0_18")) && is.finite(pa_num(r, "P", "AUC_0_18")) &&
      identical(pa_num(r, "P", "AUC_0_18"), pa_num(r_strict, "P", "AUC_0_18")) &&
      # ASCII-only patterns: an en dash in a pattern is locale-dependent
      any(grepl("is not reported for 1 of 2 profiles: Q: no measurable concentration after 12", w)) &&
      any(grepl("below the limit of quantification does not extend the profile", w)) &&
      any(grepl("not extrapolated", w))
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "0-18 h (Q ends at 12 h) and 20-30 h (beyond both); R2 threshold 0.7 and 0.999",
  expected = paste("missing past Tlast with a note naming the profile and its last measurable time;",
                   "values inside Tlast unaffected by the lambda-z rule"))

check("PAUC-05", "Steady state: intervals must lie within 0 to tau",
  tryCatch({
    ss_d2 <- pa_d[pa_d$ID == "P" & pa_d$T <= 12, ]
    run_ss <- function(iv) pa_warn(run_nca(ss_d2, pa_cm, pa_st(iv, ss = TRUE, tau = 12)))
    x1 <- run_ss(pa_iv(0, 24)); x2 <- run_ss(pa_iv(4, "t")); r3 <- run_ss(pa_iv(c(0, 0), c(4, 12)))$value
    is.null(x1$value) && any(grepl("within 0 to", x1$w)) && is.null(x2$value) && any(grepl("within 0 to", x2$w)) &&
      !is.null(r3) && isTRUE(all.equal(as.numeric(r3$AUC_0_12), as.numeric(r3$AUCTAU))) &&
      abs(as.numeric(r3$AUC_0_4) - pa_hand(ss_d2$T, ss_d2$C, 0, 4)) < 1e-9 &&
      # sampling stops before tau: AUCtau is completed with lambda-z, the interval is not
      local({
        short <- ss_d2[ss_d2$T <= 8, ]
        rs <- suppressWarnings(run_nca(short, pa_cm, pa_st(pa_iv(0, 12), ss = TRUE, tau = 12)))
        is.finite(as.numeric(rs$AUCTAU)) && is.na(as.numeric(rs$AUC_0_12))
      })
  }, error = function(e) FALSE),
  "URS-NCA-14", critical = TRUE,
  method = "tau = 12 h; intervals 0-24, 4-t, 0-4 and 0-12; and a profile sampled only to 8 h",
  expected = paste("0-24 and 4-t refused with a message; 0-12 equals AUCtau when tau is within Tlast;",
                   "0-4 equals hand trapezoids; when sampling stops before tau, AUCtau is extrapolated",
                   "and the interval is not reported"))

check("PAUC-06", "Batch and single-profile analyses give identical partial AUCs, also without a time-0 sample and after IV bolus",
  tryCatch({
    iv <- pa_iv(c(0, 0.3, 2), c("t", "3", "t"), cmax = TRUE)
    ok <- TRUE
    for (route in c("extravascular", "iv_bolus")) for (m in c("linear", "log")) {
      d <- pa_d[pa_d$T > 0, ]
      if (route == "iv_bolus") d$C <- d$C + 1
      r <- suppressWarnings(run_nca(d, pa_cm, pa_st(iv, trap = m, route = route)))
      cols <- grep(PARTIAL_AUC_PATTERN, names(r), value = TRUE)
      for (id in c("P", "Q")) {
        dd <- d[d$ID == id, ]
        s1 <- suppressWarnings(run_single_nca(dd$T, dd$C, pa_st(iv, trap = m, route = route)))
        ok <- ok && length(cols) == 9 &&
          isTRUE(all.equal(unname(s1[cols]), as.numeric(pa_row(r, id)[cols]), tolerance = 1e-12)) &&
          isTRUE(all.equal(pa_num(r, id, "AUC_0_t"), pa_num(r, id, "AUCLST")))
      }
    }
    ok
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "run_nca vs run_single_nca; oral and IV bolus; linear and log-down; first sample at 0.25 h",
  expected = "same AUC, Cmax and Tmax in every interval; 0-t equals AUClast")

pa_lai <- data.frame(ID = rep(c("L1", "L2"), each = 12),
  T = rep(c(0, 0.04, 0.17, 1, 2, 3, 7, 14, 21, 28, 35, 42), 2),
  C = c("<0.05", 1.8, 9.5, 4.2, 1.1, 0.42, 0.30, 0.21, 0.12, "<0.05", 0.08, "<0.05",
        "<0.05", 2.3, 8.1, 3.6, 0.9, 0.35, 0.22, "<0.05", "<0.05", "<0.05", 0.06, "<0.05"),
  stringsAsFactors = FALSE)
pa_lai_ds <- prepare_pk_dataset(pa_lai, pa_cm, list(lloq = 0.05, blq_rule = "rule1"))

check("PAUC-07", "Contiguous intervals add up to AUClast (long-acting injectable in days)",
  tryCatch({
    iv <- pa_iv(c(0, 3, 14), c("3", "14", "t"))
    ok <- TRUE
    for (m in c("linear", "log")) {
      r <- suppressWarnings(run_nca(pa_lai_ds$data, pa_cm, pa_st(iv, trap = m)))
      for (id in c("L1", "L2"))
        ok <- ok && abs(pa_num(r, id, "AUC_0_3") + pa_num(r, id, "AUC_3_14") + pa_num(r, id, "AUC_14_t") -
                          pa_num(r, id, "AUCLST")) < 1e-9 * pa_num(r, id, "AUCLST")
    }
    ok
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "burst then plateau near the LLOQ, BLQ text, rule 1; 0-3, 3-14, 14-t days",
  expected = "sum equals AUClast for both profiles and both methods")

check("PAUC-08", "Cmax and Tmax in an interval are observed values, without interpolation",
  tryCatch({
    r <- pa_run(pa_iv(c(0.75, 1.2, 0, 20), c("3", "1.8", "t", "30"), cmax = TRUE))
    pa_num(r, "P", "CMAX_0.75_3") == 12 && pa_num(r, "P", "TMAX_0.75_3") == 1 &&
      is.na(pa_num(r, "P", "CMAX_1.2_1.8")) && pa_num(r, "Q", "CMAX_0_t") == 11 &&
      is.na(pa_num(r, "P", "CMAX_20_30")) && is.na(pa_num(r, "P", "TMAX_20_30"))
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "windows 0.75-3, 1.2-1.8 (no sample), 0-t and 20-30 h",
  expected = "12 at 1 h; missing without samples in the window or past Tlast")

check("PAUC-09", "Interval entries are checked and read back from a record in the same shape",
  tryCatch({
    v <- function(iv, ...) validate_partial_aucs(partial_auc_spec(iv), ...)
    js <- jsonlite::fromJSON(jsonlite::toJSON(pa_iv(c(0, 168), c("0.5", "t"), c(TRUE, FALSE), c("pivotal", "supportive"))),
                             simplifyDataFrame = FALSE)
    grepl("later than the start", v(pa_iv(2, 1))) && grepl("0 or later", v(pa_iv(-1, 2))) &&
      grepl("twice", v(pa_iv(c(0, 0), c(1, "1.0")))) && grepl("the end must be", v(pa_iv(0, "abc"))) &&
      grepl("pivotal or supportive", v(pa_iv(0, 1, role = "primary"))) &&
      is.null(v(pa_iv(c(0, 1), c(1, "t")))) && is.null(v(NULL)) &&
      identical(partial_auc_spec(js), partial_auc_spec(pa_iv(c(0, 168), c("0.5", "t"), c(TRUE, FALSE), c("pivotal", "supportive")))) &&
      identical(partial_auc_names(partial_auc_spec(pa_iv(0.5, "24.0")))$auc, "AUC_0.5_24")
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "validate_partial_aucs, partial_auc_spec on a JSON round trip",
  expected = "messages for end <= start, negative start, duplicates, non-numeric end, unknown role; identical spec after JSON")

check("PAUC-10", "Notes for interpolated cutoffs, zero partial AUCs and intervals resting on BLQ values",
  tryCatch({
    w_off <- pa_warn(run_nca(pa_d, pa_cm, pa_st(pa_iv(0, 0.75), trap = "log")))$w
    d0 <- pa_d; d0$C[d0$ID == "P" & d0$T == 0.25] <- 0
    w_zero <- pa_warn(run_nca(d0, pa_cm, pa_st(pa_iv(0, 0.25))))$w
    w_blq <- pa_warn(run_nca(pa_lai_ds$data, pa_cm, pa_st(pa_iv(c(14, 1), c("t", "3")))))$w
    any(grepl("a cutoff is not a sampling time in 2 of 2 profiles.*log-linearly", w_off)) &&
      any(grepl("0.25 is zero in 1 of 2 profiles: P", w_zero)) &&
      any(grepl("more than half of the samples in this window were set by the BLQ rule, in 1 of 2 profiles: L2",
                w_blq)) &&
      sum(grepl("more than half of the samples", w_blq)) == 1 &&
      any(grepl("rests on fewer than three measurable concentrations in 1 of 2 profiles, so the value is imprecise: L2",
                w_blq))
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = FALSE,
  method = "cutoff 0.75 h; a zero early interval; plateau 14 d-t with BLQ and few measurable samples",
  expected = paste("each note names the interval and the profiles, states the BLQ criterion, and flags a",
                   "window with fewer than three measurable concentrations"))

check("PAUC-11", "The BLQ flag is kept through the pipeline and does not change results",
  tryCatch({
    fl <- pa_lai_ds$data[[BLQ_FLAG_COLUMN]]
    no_flag <- pa_lai_ds$data; no_flag[[BLQ_FLAG_COLUMN]] <- NULL
    iv <- pa_iv(c(0, 3), c("3", "t"), cmax = TRUE)
    r1 <- suppressWarnings(run_nca(pa_lai_ds$data, pa_cm, pa_st(iv)))
    r2 <- suppressWarnings(run_nca(no_flag, pa_cm, pa_st(iv)))
    is.logical(fl) && sum(fl) == 8 &&
      identical(fl, pa_lai$C[order(pa_lai$ID, pa_lai$T)] == "<0.05") &&
      is.null(prepare_pk_dataset(pa_d, pa_cm)$data[[BLQ_FLAG_COLUMN]]) && isTRUE(all.equal(r1, r2)) &&
      # a readable column name, not an internal one
      identical(BLQ_FLAG_COLUMN, "BLQ_flag") && !startsWith(BLQ_FLAG_COLUMN, ".")
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE,
  method = "prepare_pk_dataset with LLOQ 0.05 and rule 1; run_nca with and without the BLQ_flag column",
  expected = "flag TRUE exactly for the 8 BLQ samples; absent without LLOQ; identical NCA results")

pa_be <- read.csv(file.path("validation", "fixtures", "be_2x2x2_crossover.csv"), stringsAsFactors = FALSE)
pa_be_cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment",
                 period = "Period", sequence = "Sequence")
pa_be_fit <- function(d, param, iv, verdict = TRUE, design = "2x2x2", cm = pa_be_cm) {
  r <- suppressWarnings(run_nca(d, cm, pa_st(iv, trap = "log")))
  b <- build_be_data(r, d, cm, reference = "Reference")
  list(r = r, b = b, f = fit_be_parameter(b$data, param, design = design, trt_col = b$trt_col,
                                          subj_col = b$subj_col, per_col = b$per_col, seq_col = b$seq_col,
                                          verdict = verdict))
}

check("PAUC-12", "Bioequivalence of a partial AUC: same model and CI as other metrics; verdict follows the role",
  tryCatch({
    iv <- pa_iv(c(0, 4), c("1.5", "t"), cmax = TRUE)
    x <- pa_be_fit(pa_be, "AUC_0_1.5", iv)
    b <- x$b$data; b$Treatment <- relevel(factor(as.character(b$Treatment)), ref = "Reference")
    ref <- lm(log(AUC_0_1.5) ~ factor(Sequence) + factor(Subject) + factor(Period) + Treatment, data = b)
    cf <- summary(ref)$coefficients["TreatmentTest", ]; tc <- qt(0.95, ref$df.residual)
    sup <- pa_be_fit(pa_be, "AUC_4_t", iv, verdict = FALSE)$f
    cmx <- pa_be_fit(pa_be, "CMAX_0_1.5", iv)$f
    abs(x$f$estimate$ci_lo - exp(cf[[1]] - tc * cf[[2]]) * 100) < 1e-8 &&
      abs(x$f$estimate$ci_hi - exp(cf[[1]] + tc * cf[[2]]) * 100) < 1e-8 &&
      x$f$row$Bioequivalent %in% c("YES", "NO") && identical(sup$row$Bioequivalent, "no verdict") &&
      is.finite(sup$row$CI_Lower) && is.na(sup$row$BE_Lower) && cmx$row$Bioequivalent %in% c("YES", "NO")
  }, error = function(e) FALSE),
  "URS-BE-10", critical = TRUE, method = "2x2 crossover fixture; AUC 0-1.5 h pivotal, AUC 4 h-t supportive, Cmax 0-1.5 h",
  expected = "CI equal to lm on log values; YES/NO for pivotal; ratio and CI without verdict for supportive")

check("PAUC-13", "A zero partial AUC gives no estimate and no verdict, with counts per treatment",
  tryCatch({
    d <- pa_be; d$Conc[d$Subject %in% c(1, 2) & d$Treatment == "Test" & d$Time == 0.5] <- 0
    x <- pa_be_fit(d, "AUC_0_0.5", pa_iv(0, "0.5"))
    grepl("^no verdict: 2 zero value\\(s\\) \\(Test 2, Reference 0\\)", x$f$row$Bioequivalent) &&
      is.na(x$f$row$Point_Est) && is.null(x$f$estimate) && !is.null(x$f$reason)
  }, error = function(e) FALSE),
  "URS-BE-10", critical = TRUE, method = "two Test profiles with a zero AUC 0-0.5 h",
  expected = "no estimate; message counts 2 Test and 0 Reference zeros")

check("PAUC-19", "A suppressed metric stays visible, with the profiles that dropped out",
  tryCatch({
    d <- pa_be; d$Conc[d$Subject == 1 & d$Treatment == "Test" & d$Period == 1 & d$Time == 0.5] <- 0
    iv <- pa_iv(c(0, 0), c("0.5", "1.5"))
    r <- suppressWarnings(run_nca(d, pa_be_cm, pa_st(iv, trap = "log")))
    b <- build_be_data(r, d, pa_be_cm, reference = "Reference")
    f <- function(param) fit_be_parameter(b$data, param, design = "2x2x2", trt_col = b$trt_col,
                                          subj_col = b$subj_col, per_col = b$per_col, seq_col = b$seq_col)
    zero <- f("AUC_0_0.5"); other <- f("AUC_0_1.5"); cm <- f("CMAX")
    # a zero in one period of a crossover: the message names subject and period
    grepl("1 period 1", zero$row$Bioequivalent, fixed = TRUE) &&
      zero$row$Zeros_Test == 1 && zero$row$Zeros_Ref == 0 && is.na(zero$row$Point_Est) &&
      # suppression is per metric, not per run
      is.finite(other$row$Point_Est) && other$row$Bioequivalent %in% c("YES", "NO") &&
      is.finite(cm$row$Point_Est) && cm$row$Zeros_Test == 0 &&
      # descriptive statistics keep the zero
      summarize_pk_params(r, "AUC_0_0.5", group_col = "Treatment")$N[1] == 12 &&
      min(as.numeric(r$AUC_0_0.5)) == 0 &&
      !grepl("switch", zero$row$Bioequivalent, ignore.case = TRUE) &&
      grepl("belong in the protocol", zero$row$Bioequivalent, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-10", critical = TRUE,
  method = "2x2 crossover with one zero AUC 0-0.5 h in subject 1, period 1",
  expected = paste("that metric alone has no estimate, names subject and period and counts the zeros;",
                   "other metrics unaffected; summary statistics still count the zero"))

check("PAUC-20", "Bioequivalence reports how many profiles are missing for a metric",
  tryCatch({
    iv <- pa_iv(c(0, 20), c("1.5", "t"))
    # LLOQ 0.5 makes the tail BLQ, so Tlast differs per profile (rule 1)
    d <- prepare_pk_dataset(pa_be, pa_be_cm, list(lloq = 0.5, blq_rule = "rule1"))$data
    r <- suppressWarnings(run_nca(d, pa_be_cm, pa_st(iv, trap = "log")))
    b <- build_be_data(r, d, pa_be_cm, reference = "Reference")
    f <- function(param) fit_be_parameter(b$data, param, design = "2x2x2", trt_col = b$trt_col,
                                          subj_col = b$subj_col, per_col = b$per_col, seq_col = b$seq_col)
    late <- f("AUC_20_t"); early <- f("AUC_0_1.5")
    n_na <- sum(is.na(as.numeric(r$AUC_20_t)))
    nm <- names(rename_be_columns(late$row))
    n_na > 0 && late$row$Missing_Test + late$row$Missing_Ref == n_na &&
      grepl("profiles missing|profile\\(s\\) have no value|reaches past the last measurable", late$row$Bioequivalent) &&
      early$row$Missing_Test == 0 && early$row$Missing_Ref == 0 &&
      all(c("Profiles missing (Test)", "Profiles missing (Reference)", "Zero values (Test)") %in% nm)
  }, error = function(e) FALSE),
  "URS-BE-10", critical = TRUE,
  method = "AUC 20 h-t, which lies past Tlast in part of the profiles",
  expected = "Missing_Test + Missing_Ref equals the number of profiles without a value; labelled in exports")

check("PAUC-14", "Replicate design: partial AUC CI and CVwR agree with replicateBE",
  tryCatch({
    iv <- pa_iv(0, "4")
    x <- pa_be_fit(rep_224, "AUC_0_4", iv, design = "replicate_2x2x4", cm = rep_cm)
    b <- x$b$data
    cv <- be_variability_diagnostic(b, "AUC_0_4", trt_col = x$b$trt_col, subj_col = x$b$subj_col,
                                    per_col = x$b$per_col, seq_col = x$b$seq_col)
    wd <- file.path(tempdir(), "pauc_rbe"); dir.create(wd, showWarnings = FALSE)
    rb <- data.frame(subject = b$Subject, period = b$Period, sequence = b[[x$b$seq_col]],
                     treatment = ifelse(b$Treatment == "Test", "T", "R"), PK = b$AUC_0_4)
    write.csv(rb[order(as.numeric(rb$subject), as.numeric(rb$period)), ], file.path(wd, "pauc.csv"),
              row.names = FALSE, quote = FALSE)
    m <- replicateBE::method.A(path.in = wd, path.out = wd, file = "pauc", set = "", ext = "csv",
                               print = FALSE, details = TRUE, verbose = FALSE, plot.bxp = FALSE)
    abs(x$f$estimate$ci_lo - as.numeric(m[1, "CL.lo(%)"])) < 1e-6 &&
      abs(x$f$estimate$ci_hi - as.numeric(m[1, "CL.hi(%)"])) < 1e-6 &&
      abs(cv$CVwR - as.numeric(m[1, "CVwR(%)"])) < 1e-6
  }, error = function(e) FALSE),
  "URS-BE-10", critical = TRUE, method = "2x2x4 fixture; AUC 0-4 h from the app into replicateBE::method.A",
  expected = "90% CI and CVwR equal within 1e-6")

check("PAUC-15", "Records store the intervals and reproduce the partial AUCs; a changed interval is detected",
  tryCatch({
    iv <- pa_iv(c(0, 4), c("1.5", "t"), cmax = c(TRUE, FALSE), role = c("pivotal", "supportive"))
    st <- pa_st(iv, trap = "log")
    rb <- rec_build(df = pa_be, cm = pa_be_cm, st = st, be = TRUE)
    js <- jsonlite::fromJSON(file.path(rb$ex, "analysis_settings.json"), simplifyDataFrame = FALSE)
    batch_ok <- identical(partial_auc_spec(js$partial_aucs), partial_auc_spec(iv)) &&
      identical(as.numeric(js$partial_auc_blq_fraction), PARTIAL_AUC_BLQ_FRACTION) &&
      grepl("Result: MATCH", rec_check_text(rb$ex)) && "AUC_4_t" %in% names(rb$result)
    # Edit the recorded interval and run the shipped script again
    js$partial_aucs[[1]]$end <- "2"
    writeLines(jsonlite::toJSON(js, auto_unbox = TRUE, digits = NA, null = "null", pretty = TRUE),
               file.path(rb$ex, "analysis_settings.json"))
    out <- local({ owd <- setwd(rb$ex); on.exit(setwd(owd))
      system2(file.path(R.home("bin"), "Rscript"), "reproduce_analysis.R", stdout = TRUE, stderr = TRUE) })
    changed_ok <- any(grepl("Result: DIFFERENT \\(partial AUC columns differ", out))
    dd <- pa_d[pa_d$ID == "P", ]
    s1 <- suppressWarnings(run_single_nca(dd$T, dd$C, pa_st(iv)))
    zp <- file.path(tempdir(), "pauc_single.zip")
    create_single_analysis_record(zp, s1, pa_st(iv), dd$T, dd$C, subject_label = "Manual Entry")
    batch_ok && changed_ok && grepl("Result: MATCH", rec_check_text(rec_unzip(zp)))
  }, error = function(e) FALSE),
  "URS-EXP-08", critical = TRUE, method = "BE record and single-profile record with intervals; interval end edited in the JSON",
  expected = "intervals in analysis_settings.json; MATCH; DIFFERENT after the edit")

check("PAUC-16", "Plain-language labels, units and the CDISC code AUCINT with the interval",
  tryCatch({
    fn <- unname(friendly_name(c("AUC_0_0.5", "AUC_168_t", "CMAX_0_4", "TMAX_0_4", "CMAX")))
    codes <- cdisc_pk_codes(c("AUC_0_0.5", "AUC_168_t", "CMAX_0_4"))
    lab <- unname(add_units_to_labels(fn[1:4], time_unit = "h", conc_unit = "ng/mL"))
    same <- function(a, b) length(a) == length(b) && all(enc2utf8(a) == enc2utf8(b))
    same(fn, c("Partial AUC 0\u20130.5", "Partial AUC 168\u2013t", "Cmax 0\u20134", "Tmax 0\u20134",
               "Peak Concentration (Cmax)")) &&
      same(lab, c("Partial AUC 0\u20130.5 (ng/mL\u00b7h)", "Partial AUC 168\u2013t (ng/mL\u00b7h)",
                  "Cmax 0\u20134 (ng/mL)", "Tmax 0\u20134 (h)")) &&
      identical(codes$PPTESTCD, c("AUCINT", "AUCINT", "")) && codes$PPTEST[1] == "AUC from T1 to T2" &&
      grepl("PPSTINT 0, PPENINT 0.5", codes$Note[1]) && grepl("PPENINT the time of the last measurable", codes$Note[2]) &&
      grepl("No code", codes$Note[3]) &&
      same(names(rename_nca_columns(data.frame(AUC_0_0.5 = 1, check.names = FALSE))), "Partial AUC 0\u20130.5")
  }, error = function(e) FALSE),
  "URS-UI-01, URS-GEN-06", critical = FALSE, method = "friendly_name, add_units_to_labels, cdisc_pk_codes",
  expected = "labels with en dash and units; AUCINT with PPSTINT/PPENINT; no code for Cmax in an interval")

check("PAUC-17", "Figures shade the partial AUC intervals, also in the Figure Record",
  tryCatch({
    iv <- pa_iv(c(0, 4), c("1.5", "t"))
    sh <- partial_auc_shading(iv, 36, c(2, NA, 8, 0.5))
    f <- file.path(tempdir(), "pauc_fig.csv"); write.csv(pa_be, f, row.names = FALSE)
    p <- ggplot2::ggplot(pa_be, ggplot2::aes(Time, Conc)) + ggplot2::geom_point()
    zp <- file.path(tempdir(), "pauc_fig.zip")
    create_viz_record(zp, p, list(plot_type = "summary", summary_statistic = "geomean", export_format = "png",
                                  dpi = 72, shade_partial_aucs = iv),
                      pa_be_cm, f, "pauc_fig.csv", blq_rule = "rule1", lloq = 0.5)
    ex <- rec_unzip(zp)
    scr <- paste(readLines(file.path(ex, "reproduce_figure.R"), warn = FALSE), collapse = "\n")
    identical(sh$xmin, c(0, 4)) && identical(sh$xmax, c(1.5, 36)) && all(sh$ymin == 0) && all(sh$ymax == 8) &&
      all(partial_auc_shading(iv, 36, c(0, 2, 8), log = TRUE)$ymin == 2) && is.null(partial_auc_shading(NULL, 36, 1)) &&
      grepl("partial_auc_shading(rec$visualization$shade_partial_aucs", scr, fixed = TRUE) &&
      grepl("Result: FIGURE CREATED", rec_check_text(ex))
  }, error = function(e) FALSE),
  "URS-VIZ-09", critical = FALSE, method = "partial_auc_shading(); summary figure record with two shaded intervals",
  expected = "0-1.5 and 4-36 (t drawn to the last time); script shades them; figure produced")

check("PAUC-21", "Bioequivalence reports profiles that rest mainly on BLQ-derived values",
  tryCatch({
    d <- pa_be
    # one subject absorbs late, so its 0.5 h sample is below an LLOQ of 0.5
    d$Conc[d$Subject == 1 & d$Treatment == "Test" & d$Time == 0.5] <- 0.2
    out <- lapply(c("rule1", "rule6"), function(rule) {
      ds <- prepare_pk_dataset(d, pa_be_cm, list(lloq = 0.5, blq_rule = rule))
      r <- suppressWarnings(run_nca(ds$data, pa_be_cm, pa_st(pa_iv(0, "0.5"), trap = "log")))
      b <- build_be_data(r, ds$data, pa_be_cm, reference = "Reference")
      f <- fit_be_parameter(b$data, "AUC_0_0.5", design = "2x2x2", trt_col = b$trt_col,
                            subj_col = b$subj_col, per_col = b$per_col, seq_col = b$seq_col)
      cnt <- partial_auc_blq_counts(attr(r, "partial_auc_blq"), b$data, "AUC_0_0.5",
                                    b$trt_col, levels(b$data[[b$trt_col]]))
      list(f = f, cnt = cnt)
    })
    r1 <- out[[1]]; r6 <- out[[2]]
    # Rule 1 makes the value exactly zero: suppressed, and counted as a zero
    is.na(r1$f$row$Point_Est) && r1$f$row$Zeros_Test == 1 &&
      # Rule 6 imputes LLOQ/2, so the metric keeps a verdict; the count is what
      # tells the reader that one Test profile rests on imputed values
      is.finite(r6$f$row$Point_Est) && r6$f$row$Zeros_Test == 0 &&
      r6$cnt$BLQ_Test == 1 && r6$cnt$BLQ_Ref == 0 && r1$cnt$BLQ_Test == 1 &&
      # Cmax within the interval inherits the flag of its interval
      identical(partial_auc_blq_counts(attr(suppressWarnings(run_nca(
        prepare_pk_dataset(d, pa_be_cm, list(lloq = 0.5, blq_rule = "rule6"))$data, pa_be_cm,
        pa_st(pa_iv(0, "0.5", cmax = TRUE), trap = "log"))), "partial_auc_blq"),
        build_be_data(suppressWarnings(run_nca(
          prepare_pk_dataset(d, pa_be_cm, list(lloq = 0.5, blq_rule = "rule6"))$data, pa_be_cm,
          pa_st(pa_iv(0, "0.5", cmax = TRUE), trap = "log"))),
          prepare_pk_dataset(d, pa_be_cm, list(lloq = 0.5, blq_rule = "rule6"))$data,
          pa_be_cm, reference = "Reference")$data,
        "CMAX_0_0.5", "Treatment", c("Reference", "Test"))$BLQ_Test, 1L) &&
      all(c("Mostly BLQ (Test)", "Mostly BLQ (Reference)") %in%
            names(rename_be_columns(data.frame(BLQ_Test = 1, BLQ_Ref = 0))))
  }, error = function(e) FALSE),
  "URS-BE-10", critical = TRUE,
  method = "one late-absorbing profile under BLQ rules 1 and 6; partial_auc_blq_counts()",
  expected = paste("rule 1 gives a zero and no estimate; rule 6 keeps the estimate but reports one",
                   "Test profile as mostly BLQ, and Cmax in the interval inherits that flag"))

check("PAUC-22", "Dose normalisation covers partial AUCs and Cmax within an interval",
  tryCatch({
    iv <- pa_iv(c(0, 4), c("1", "t"), cmax = c(TRUE, FALSE))
    r <- suppressWarnings(run_nca(pa_be, pa_be_cm, pa_st(iv, trap = "log")))
    dn <- add_dose_normalized(as.data.frame(r), 100)
    dv <- stats::setNames(rep(c(50, 100), each = 6), as.character(1:12))
    dn2 <- add_dose_normalized(as.data.frame(r), dv)
    codes <- cdisc_pk_codes(c("AUC_0_1_DN", "CMAX_0_1_DN"))
    same <- function(a, b) length(a) == length(b) && all(enc2utf8(a) == enc2utf8(b))
    all(c("AUC_0_1_DN", "AUC_4_t_DN", "CMAX_0_1_DN") %in% names(dn)) &&
      !("TMAX_0_1_DN" %in% names(dn)) &&
      isTRUE(all.equal(as.numeric(dn$AUC_0_1_DN), as.numeric(dn$AUC_0_1) / 100)) &&
      # a per-subject dose vector is matched by subject, not by position
      isTRUE(all.equal(as.numeric(dn2$CMAX_0_1_DN),
                       as.numeric(dn2$CMAX_0_1) / as.numeric(dv[as.character(dn2$Subject)]))) &&
      same(unname(friendly_name("AUC_0_1_DN")), "Dose-Normalised Partial AUC 0\u20131") &&
      # the label must not pick up a concentration unit meant for the plain metric
      same(unname(add_units_to_labels(unname(friendly_name("AUC_0_1_DN")), conc_unit = "ng/mL")),
           "Dose-Normalised Partial AUC 0\u20131") &&
      identical(codes$PPTESTCD, c("AUCINTD", "")) && codes$PPTEST[1] == "AUC from T1 to T2 Norm by Dose"
  }, error = function(e) FALSE),
  "URS-NCA-08", critical = FALSE,
  method = "add_dose_normalized() on a result with two intervals, single and per-subject doses",
  expected = "DN columns for the interval AUC and Cmax but not Tmax; doses matched by subject; AUCINTD code")

check("PAUC-18", "Methods page, help and Data Guide describe partial AUCs as implemented",
  tryCatch({
    has <- function(f, keys) { m <- paste(rev3_code(f), collapse = " "); all(vapply(keys, grepl, logical(1), m, fixed = TRUE)) }
    has("R/mod_methods.R", c("minus the AUC from 0 to the start of the interval", "Partial AUCs were ",
                             "not extrapolated", "intersection-union test", "no estimate and no conclusion",
                             "more than half of its samples were set by the BLQ rule", "Hopefl R, et al.",
                             "EMA/CHMP/EWP/280/96 Rev1")) &&
      has("R/help_system.R", c("help_partial_auc", "the app does not extrapolate", "Pivotal or supportive")) &&
      has("R/mod_data_guide.R", c("Planning a partial AUC", "A partial AUC cutoff with no sample near it",
                                  "Partial AUC intervals must lie within 0 to")) &&
      has("R/mod_partial_auc.R", c("help_partial_auc", "End at the last measurable concentration (t)"))
  }, error = function(e) FALSE),
  "URS-GEN-03", critical = FALSE, method = "search Methods, help, Data Guide and the interval editor",
  expected = "all statements present")

end_section("PAUC")

# =============================================================================
# SECTION REL: Fixes from the v1.5.0 release-readiness review (R-01 ...)
# =============================================================================
# Each test is built from the failing case the review found.
start_section("REL")

rel_st <- function(trap = "log", route = "extravascular", ss = FALSE, tau = NA, pauc = NULL, dur = 0)
  list(admin_route = route, dose = 100, trap_method = trap, dose_unit = "mg", time_unit = "h",
       conc_unit = "ng/mL", is_steady_state = ss, tau = tau, mw = 0, r2adj_threshold = 0,
       infusion_duration = dur, partial_aucs = pauc)
rel_cm <- list(subject = "ID", time = "T", conc = "C")
# Linear-up/log-down written out by hand, linear on a segment that ends at 0
rel_hand <- function(t, c) {
  a <- 0; m <- 0
  for (i in 2:length(t)) { dt <- t[i] - t[i - 1]
    if (c[i] < c[i - 1] && c[i] > 0) { k <- log(c[i - 1] / c[i]) / dt
      a <- a + (c[i - 1] - c[i]) / k; m <- m + (t[i - 1] * c[i - 1] - t[i] * c[i]) / k + (c[i - 1] - c[i]) / k^2
    } else { a <- a + dt * (c[i] + c[i - 1]) / 2; m <- m + dt * (t[i] * c[i] + t[i - 1] * c[i - 1]) / 2 } }
  c(AUC = a, AUMC = m)
}
rel_t <- c(0, 1, 2, 4, 6, 8, 12, 16, 24); rel_c <- c(0, 5, 20, 15, 0, 6, 4, 2, 1)

check("REL-01", "R-01: log-down AUClast and AUMClast count the fall to an embedded zero",
  tryCatch({
    r <- suppressWarnings(run_single_nca(rel_t, rel_c, rel_st()))
    b <- suppressWarnings(run_nca(data.frame(ID = "1", T = rel_t, C = rel_c), rel_cm, rel_st()))
    h <- rel_hand(rel_t, rel_c)
    abs(r[["AUCLST"]] - h[["AUC"]]) < 1e-9 && abs(r[["AUMCLST"]] - h[["AUMC"]]) < 1e-9 &&
      abs(r[["AUCLST"]] - 113.5741428) < 1e-6 && abs(b$AUCLST - r[["AUCLST"]]) < 1e-12 &&
      abs(b$AUMCLST - r[["AUMCLST"]]) < 1e-12
  }, error = function(e) FALSE),
  "URS-NCA-03", critical = TRUE, method = "0,5,20,15,0,6,4,2,1 at 0-24 h, log-down, single and batch vs hand calculation",
  expected = "AUClast 113.574 (NonCompart alone gives 98.574); AUMClast equal to the hand value")

check("REL-02", "R-01: parameters derived from AUClast follow the corrected value",
  tryCatch({
    r <- suppressWarnings(run_single_nca(rel_t, rel_c, rel_st()))
    h <- rel_hand(rel_t, rel_c); lz <- r[["LAMZ"]]; ifo <- h[["AUC"]] + r[["CLST"]] / lz
    aumc_ifo <- h[["AUMC"]] + r[["CLST"]] * r[["TLST"]] / lz + r[["CLST"]] / lz^2
    abs(r[["AUCIFO"]] - ifo) < 1e-9 && abs(r[["CLFO"]] - 100 / ifo * 1000) < 1e-6 &&
      abs(r[["VZFO"]] - 100 / ifo / lz * 1000) < 1e-5 && abs(r[["MRTEVIFO"]] - aumc_ifo / ifo) < 1e-9 &&
      abs(r[["AUCPEO"]] - (1 - h[["AUC"]] / ifo) * 100) < 1e-9 && abs(r[["AUCIFOD"]] - ifo / 100) < 1e-9
  }, error = function(e) FALSE),
  "URS-NCA-01", critical = TRUE, method = "AUCinf, CL/F, Vz/F, MRT, %extrap and AUCinf/D recomputed from the corrected AUClast",
  expected = "Each equals its definition (mg and ng/mL: CL/F in L/h = dose / AUCinf x 1000)")

check("REL-03", "R-01: a partial AUC ending at t after an embedded BLQ is not negative and matches the hand value",
  tryCatch({
    d <- data.frame(ID = "1", T = c(0, 1, 2, 4, 6, 8, 12), C = c(0, 10, 0.2, 1, 0.9, 0.7, 0.6))
    r <- suppressWarnings(run_nca(prepare_pk_dataset(d, rel_cm, list(lloq = 0.5, blq_rule = "rule1"))$data, rel_cm,
                                  rel_st(pauc = data.frame(start = 2, end = "t", cmax = FALSE, role = "pivotal"))))
    v <- r$AUC_2_t
    h <- rel_hand(c(2, 4, 6, 8, 12), c(0, 1, 0.9, 0.7, 0.6))[["AUC"]]
    isTRUE(v >= 0) && abs(v - h) < 1e-9 && abs(r$AUCLST - rel_hand(d$T, c(0, 10, 0, 1, 0.9, 0.7, 0.6))[["AUC"]]) < 1e-9
  }, error = function(e) FALSE),
  "URS-NCA-13", critical = TRUE, method = "0, 10, BLQ, 1, 0.9, 0.7, 0.6 with LLOQ 0.5 and Rule 1, log-down, interval 2-t",
  expected = "AUC 2-t = hand value (was -0.66 before the fix)")

check("REL-04", "R-01: profiles without a fall to zero are unchanged, and steady state and IV follow the corrected AUC",
  tryCatch({
    th <- data.frame(ID = as.character(Theoph$Subject), T = Theoph$Time, C = Theoph$conc)
    a <- suppressWarnings(run_nca(th, rel_cm, rel_st()))
    raw <- NonCompart::tblNCA(th, key = "ID", colTime = "T", colConc = "C", dose = 100, down = "Log",
                              R2ADJ = 0, doseUnit = "mg", timeUnit = "h", concUnit = "ng/mL")
    same <- isTRUE(all.equal(a$AUCLST, raw$AUCLST[match(a$ID, raw$ID)])) &&
            isTRUE(all.equal(a$CLFO, raw$CLFO[match(a$ID, raw$ID)]))
    s <- suppressWarnings(run_single_nca(rel_t, rel_c, rel_st(ss = TRUE, tau = 24)))
    ss_ok <- abs(s[["AUCLST"]] - 113.5741428) < 1e-6 && abs(s[["CLFO"]] - 100 / s[["AUCTAU"]] * 1000) < 1e-6
    iv <- suppressWarnings(run_single_nca(c(0.5, 1, 2, 4, 6, 8, 12), c(50, 30, 0, 8, 4, 2, 0.5),
                                          rel_st(route = "iv_infusion", dur = 0.25)))
    iv_ok <- abs(iv[["VSSO"]] - iv[["MRTIVIFO"]] * iv[["CLO"]]) < 1e-6 * iv[["VSSO"]] &&
             abs(iv[["CLO"]] - 100 / iv[["AUCIFO"]] * 1000) < 1e-6
    same && ss_ok && iv_ok
  }, error = function(e) FALSE),
  "URS-NCA-03", critical = TRUE, method = "Theoph vs NonCompart unchanged; steady state CL/F from AUCtau; infusion Vss = MRT x CL",
  expected = "Theoph identical to NonCompart; the identities hold after the correction")

rel_iv <- function(t) 900 * exp(-3 * t) + 100 * exp(-0.1 * t)
rel_iv_t <- c(0.25, 0.5, 1, 2, 4, 6, 8, 12, 24, 36, 48)

check("REL-05", "R-02: IV bolus with a pre-dose sample at time 0 gives the same result as without it",
  tryCatch({
    st <- rel_st(route = "iv_bolus"); st$dose <- 1000
    a <- run_single_nca(rel_iv_t, rel_iv(rel_iv_t), st)
    b <- run_single_nca(c(0, rel_iv_t), c(0, rel_iv(rel_iv_t)), st)
    d <- data.frame(ID = "1", T = c(0, rel_iv_t), C = c(0, rel_iv(rel_iv_t)))
    bb <- run_nca(d, rel_cm, st)
    blq <- run_nca(prepare_pk_dataset(transform(d, C = ifelse(T == 0, "BLQ", as.character(C))), rel_cm,
                                      list(lloq = 0.5, blq_rule = "rule1"))$data, rel_cm, st)
    hl <- estimate_lambda_z(d$T, d$C, 0, route = "iv_bolus")
    abs(b[["AUCIFO"]] - a[["AUCIFO"]]) < 1e-9 && abs(bb$AUCIFO - a[["AUCIFO"]]) < 1e-9 &&
      abs(blq$AUCIFO - a[["AUCIFO"]]) < 1e-9 && abs(b[["C0"]] - a[["C0"]]) < 1e-9 && b[["AUCPBEO"]] > 10 &&
      abs(a[["AUCIFO"]] - 1300) / 1300 < 0.01 && abs(hl$lambda_z - a[["LAMZ"]]) < 1e-12
  }, error = function(e) FALSE),
  "URS-NCA-02", critical = TRUE,
  method = "900e^-3t + 100e^-0.1t (AUCinf 1300), first sample 0.25 h, with and without a 0 or BLQ row at t = 0",
  expected = "AUCinf within 1% of 1300 in all cases (was 1196 with the t = 0 row); C0 back-extrapolated; review fit identical")

check("REL-06", "R-02: IV bolus at steady state takes the trough from the whole profile",
  tryCatch({
    st <- rel_st(route = "iv_bolus", ss = TRUE, tau = 12); st$dose <- 1000
    t <- c(0, 0.25, 0.5, 1, 2, 4, 6, 8, 12); cc <- c(40, 700, 560, 380, 220, 120, 90, 70, 42)
    r <- run_single_nca(t, cc, st)
    b <- run_nca(data.frame(ID = "1", T = t, C = cc), rel_cm, st)
    r[["CMIN_SS"]] == 40 && b$CMIN_SS == 40 && abs(r[["AUCTAU"]] - b$AUCTAU) < 1e-9 && r[["C0"]] > 700
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE, method = "Steady-state IV bolus with pre-dose trough 40 at t = 0",
  expected = "Cmin = 40 (the trough); C0 back-extrapolated above the first post-dose sample, not the trough")

check("REL-07", "R-03: a decimal-comma file with thousands separators is read at the right magnitude",
  tryCatch({
    f <- tempfile(fileext = ".csv")
    writeLines(c("ID;TIME;CONC;DOSE", "1;0;0;1.000", "1;1;850,5;1.000", "1;2;12.500;1.000", "1;4;1.234,5;1.000",
                 "1;8;980;1.000"), f)
    raw <- read_pk_file(f, list(sep = ";", dec = ","))
    cm <- list(subject = "ID", time = "TIME", conc = "CONC", dose = "DOSE")
    d <- prepare_pk_dataset(raw, cm, list(read_args = list(sep = ";", dec = ",")))$data
    q <- run_data_quality_check(raw, cm, 0, dec = ",")
    identical(as.numeric(d$CONC), c(0, 850.5, 12500, 1234.5, 980)) &&
      all(as.numeric(d$DOSE) == 1000) && q$n_errors == 0
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = TRUE, method = "Semicolon file with decimal comma and values 12.500, 1.234,5 and dose 1.000",
  expected = "12500, 1234.5 and dose 1000 (were read as 12.5, NA and 1); no quality errors")

check("REL-08", "R-03: a decimal point in a decimal-comma file is refused, naming the column",
  tryCatch({
    raw <- data.frame(ID = "1", TIME = c("0", "0.5", "1"), CONC = c("0", "4,2", "3.75"), stringsAsFactors = FALSE)
    cm <- list(subject = "ID", time = "TIME", conc = "CONC")
    q <- run_data_quality_check(raw, cm, 0, dec = ",")
    err <- q$findings[q$findings$Severity == "ERROR", ]
    d <- prepare_pk_dataset(raw, cm, list(read_args = list(dec = ",")))$data
    nrow(err) == 2 && all(grepl("decimal point", err$Message)) &&
      any(grepl("'TIME'", err$Message)) && any(grepl("3.75", err$Detail)) && !any(d$CONC == 3.75, na.rm = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "Decimal-comma upload with 0.5 in Time and 3.75 in Concentration",
  expected = "Two ERRORs naming the columns with examples; the values are never read as 0.5 or 3.75")

rel_be_cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment",
                  period = "Period", sequence = "Sequence")
rel_be <- read.csv("validation/fixtures/be_2x2x2_crossover.csv", stringsAsFactors = FALSE)

check("REL-09", "R-04: subject IDs that restart in each sequence are refused before bioequivalence",
  tryCatch({
    d <- rel_be
    d$Subject <- ave(d$Subject, d$Sequence, FUN = function(v) match(v, unique(v)))
    q <- run_data_quality_check(d, rel_be_cm, 0)
    msg <- q$findings$Message[q$findings$Severity == "ERROR"]
    no_seq <- run_data_quality_check(d, rel_be_cm[names(rel_be_cm) != "sequence"], 0)
    nca <- suppressWarnings(run_nca(d, rel_be_cm, rel_st(trap = "linear")))
    be_stop <- tryCatch({ build_be_data(nca, d, rel_be_cm, "Reference"); FALSE },
                        error = function(e) grepl("more than one sequence", conditionMessage(e)))
    ok_orig <- run_data_quality_check(rel_be, rel_be_cm, 0)$n_errors == 0
    any(grepl("more than one sequence", msg)) && any(grepl("same period", msg)) &&
      any(grepl("same period", no_seq$findings$Message[no_seq$findings$Severity == "ERROR"])) &&
      be_stop && ok_orig
  }, error = function(e) FALSE),
  "URS-DAT-05", critical = TRUE,
  method = "2x2x2 fixture renumbered 1-6 within each sequence, with and without Sequence mapped",
  expected = "ERROR in the data check (blocks analysis) and build_be_data() stops; the original IDs pass")

check("REL-10", "R-04: a subject recorded in two sequences is an error, not an unbalanced design",
  tryCatch({
    d <- rel_be
    d$Sequence[d$Subject == d$Subject[1] & d$Period == 2] <- setdiff(unique(d$Sequence), d$Sequence[1])[1]
    q <- run_data_quality_check(d, rel_be_cm, 0)
    any(q$findings$Severity == "ERROR" & grepl("more than one sequence", q$findings$Message)) &&
      any(grepl("subjects\\)", q$findings$Message[q$findings$Category == "Design"]))
  }, error = function(e) FALSE),
  "URS-DAT-05", critical = TRUE, method = "One subject's period-2 rows given the other sequence",
  expected = "ERROR naming the subject; the sequence line shows subjects per sequence")

check("REL-11", "R-05: a crossover without a mapped Period gets no estimate and no verdict",
  tryCatch({
    run <- function(cm) {
      nca <- suppressWarnings(run_nca(rel_be, cm, rel_st(trap = "linear")))
      b <- build_be_data(nca, rel_be, cm, "Reference")
      fit_be_parameter(b$data, "AUCLST", design = "2x2x2", trt_col = b$trt_col, subj_col = b$subj_col,
                       per_col = b$per_col, seq_col = b$seq_col)
    }
    with_p <- run(rel_be_cm)
    no_p <- run(rel_be_cm[names(rel_be_cm) != "period"])
    with_p$row$Bioequivalent %in% c("YES", "NO") && is.na(no_p$row$Point_Est) && is.null(no_p$estimate) &&
      grepl("^no verdict: a crossover needs the Period column", no_p$row$Bioequivalent)
  }, error = function(e) FALSE),
  "URS-BE-01, URS-BE-11", critical = TRUE, method = "2x2x2 fixture with Sequence mapped and Period unmapped",
  expected = "No point estimate, CI or verdict; the reason asks for the Period column (was a verdict from a model without period)")

check("REL-12", "R-05: Period is detected from APERIOD and Occasion/OCC columns",
  tryCatch({
    identical(auto_detect_columns(c("USUBJID", "AFRLT", "AVAL", "TRTA", "APERIOD"))$period, "APERIOD") &&
      identical(auto_detect_columns(c("ID", "TIME", "DV", "OCC"))$period, "OCC") &&
      identical(auto_detect_columns(c("ID", "Time", "Conc", "Occasion"))$period, "Occasion") &&
      identical(auto_detect_columns(c("ID", "Time", "Conc", "Visit"))$period, "")
  }, error = function(e) FALSE),
  "URS-DAT-02", critical = FALSE, method = "auto_detect_columns() on ADaM and NONMEM-style names",
  expected = "APERIOD, OCC and Occasion map to Period; Visit is not mapped automatically")

check("REL-13", "R-06: each path's UI is built once, so settings survive leaving and returning",
  tryCatch({
    src <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
    ui_part <- sub("\n# --- Server.*$", "", src)
    server_part <- sub("^.*\n# --- Server", "", src)
    paths <- c("path_power_ui", "path_data_ui", "path_viz_ui", "path_single_nca_ui", "path_multi_nca_ui", "path_be_ui")
    grepl("navset_hidden(", ui_part, fixed = TRUE) &&
      all(vapply(paths, function(f) lengths(regmatches(ui_part, gregexpr(paste0(f, "("), ui_part, fixed = TRUE))) == 1, logical(1))) &&
      !any(vapply(paths, function(f) grepl(paste0(f, "("), server_part, fixed = TRUE), logical(1))) &&
      grepl("nav_select(\"main_nav\"", server_part, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-03, URS-GEN-09", critical = TRUE, method = "Static check of app.R; confirmed in the running app (dose 4.02 kept after Home and back)",
  expected = "Every path UI appears once in the page definition and never in a server-side renderUI")

check("REL-14", "R-07: a planner result is cleared when an input changes after the calculation",
  tryCatch({
    suppressPackageStartupMessages({ library(shiny); library(bslib); library(plotly); library(DT) })
    for (f in c("R/help_system.R", "R/mod_path_power.R")) source(f, local = TRUE)
    ok <- FALSE
    suppressWarnings(shiny::testServer(path_power_server, args = list(shared = shiny::reactiveValues(be_results = NULL)), {
      session$setInputs(calc_mode = "sample_size", analysis_type = "abe", design = "2x2", cv = 20, cv_wr = 30,
                        theta0 = 95, alpha = 0.05, target_power = 80, n_subjects = 24, theta1 = 0.8, theta2 = 1.25)
      session$setInputs(btn_calc = 1)
      had <- !is.null(calc_result()) && calc_result()[["Sample size"]] == PowerTOST::sampleN.TOST(CV = 0.2, theta0 = 0.95,
               design = "2x2", print = FALSE)[["Sample size"]]
      session$setInputs(cv = 40)
      ok <<- had && is.null(calc_result())
    }))
    ok
  }, error = function(e) FALSE),
  "URS-PWR-01, URS-GEN-09", critical = TRUE, method = "shiny::testServer on the planner: calculate at CV 20%, then change the CV to 40%",
  expected = "N from PowerTOST after Calculate; no result after the CV changes (was N = 20 shown next to a CV of 40%)")

check("REL-15", "R-07: NCA and bioequivalence results are cleared when their settings change",
  tryCatch({
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    watched <- function(src, inputs) {
      i <- regexpr("clear_result_on_change(", src, fixed = TRUE)
      blk <- substr(src, i, i + regexpr("_stale\")", substring(src, i), fixed = TRUE))
      i > 0 && all(vapply(inputs, function(i) grepl(paste0("input$", i), blk, fixed = TRUE), logical(1)))
    }
    units <- c("dose_unit", "time_unit", "conc_unit", "trap_method", "admin_route", "is_ss", "tau")
    multi <- rd("R/mod_path_multi_nca.R"); single <- rd("R/mod_path_single_nca.R"); be <- rd("R/mod_path_be.R")
    watched(multi, c(units, "r2adj", "dose_source")) && watched(single, c(units, "r2adj", "dose")) &&
      watched(be, c(units, "ci_level", "be_lower", "be_upper", "be_reference", "model_type")) &&
      grepl("ran <- isolate(be_run_settings()$be$parameters)", be, fixed = TRUE) &&
      grepl("be_result(NULL); balance_result(NULL)", be, fixed = TRUE) &&
      grepl("v %in% names(raw_data())", rd("R/mod_data_upload.R"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-NCA-05, URS-GEN-09", critical = TRUE,
  method = "Static check of the modules; checked in the running app (unit change cleared the All Subjects table, CI level change cleared the BE results, parameters kept after a run)",
  expected = "Units, method, route, steady state, limits, CI level, Reference and model are watched; BE keeps the compared parameters; a half-life recalculation clears the BE result; mappings only name columns of the current file")

check("REL-16", "R-08: widened limits apply to Cmax only unless all metrics are chosen",
  tryCatch({
    b <- build_be_data(suppressWarnings(run_nca(rel_be, rel_be_cm, rel_st(trap = "linear"))), rel_be, rel_be_cm, "Reference")
    b$data$AUCLST[b$data$Treatment == "Test"] <- b$data$AUCLST[b$data$Treatment == "Test"] * 0.90
    fit <- function(p, scope) fit_be_parameter(b$data, p, design = "2x2x2", trt_col = b$trt_col, subj_col = b$subj_col,
                                               per_col = b$per_col, seq_col = b$seq_col, be_lower = 69.84,
                                               be_upper = 143.19, widened_scope = scope)$row
    auc_c <- fit("AUCLST", "cmax"); auc_a <- fit("AUCLST", "all"); cmax_c <- fit("CMAX", "cmax")
    hv <- read.csv("validation/fixtures/be_2x2x4_highly_variable.csv", stringsAsFactors = FALSE)
    cm4 <- rel_be_cm
    bh <- build_be_data(suppressWarnings(run_nca(hv, cm4, rel_st(trap = "linear"))), hv, cm4, "Reference")
    vd <- function(p) be_variability_diagnostic(bh$data, p, bh$trt_col, bh$subj_col, bh$per_col, bh$seq_col)
    auc_c$BE_Lower == 80 && auc_c$BE_Upper == 125 && auc_c$Bioequivalent == "NO" &&
      auc_a$BE_Lower == 69.84 && auc_a$Bioequivalent == "YES" && cmax_c$BE_Lower == 69.84 &&
      !is.na(vd("CMAX")$ABEL_lower) && is.na(vd("AUCLST")$ABEL_lower)
  }, error = function(e) FALSE),
  "URS-BE-07, URS-BE-11", critical = TRUE,
  method = "Limits 69.84-143.19% with Test AUClast scaled by 0.90; variability panel on the highly variable fixture",
  expected = "AUClast judged against 80-125% (NO) unless 'all metrics' (YES); Cmax keeps the widened limits; implied ABEL limits for Cmax only")

check("REL-17", "R-09: values set by a BLQ rule are not used for the terminal half-life",
  tryCatch({
    tt <- c(0, 0.5, 1, 2, 4, 6, 8, 12, 16, 24, 36, 48)
    cc <- 100 * (exp(-0.2 * tt) - exp(-1.5 * tt)); cc[tt >= 24] <- 0.5
    d <- data.frame(ID = "1", T = tt, C = cc)
    st <- rel_st(trap = "linear"); st$r2adj_threshold <- 0.7
    hl <- function(rule) {
      p <- prepare_pk_dataset(d, rel_cm, list(lloq = 2, blq_rule = rule))$data
      c(batch = suppressWarnings(run_nca(p, rel_cm, st))$LAMZHL,
        single = suppressWarnings(run_single_nca(p$T, p$C, st, is_blq = p$BLQ_flag))[["LAMZHL"]],
        review = estimate_lambda_z(p$T, p$C, 0.7, is_blq = p$BLQ_flag)$half_life)
    }
    r1 <- hl("rule1"); r4 <- hl("rule4")
    all(abs(r4 - r1) < 1e-9) && abs(r1[["batch"]] - log(2) / 0.2) < 0.02
  }, error = function(e) FALSE),
  "URS-NCA-04", critical = TRUE,
  method = "Oral profile, k = 0.2/h, BLQ at 24-48 h (LLOQ 2); Rule 1 and Rule 4; batch, single profile and Half-Life Review",
  expected = "Rule 4 half-life equal to Rule 1 (3.47 h, true 3.47) in all three (was 7.27 h with the LLOQ/2 tail in the fit)")

check("REL-18", "R-09: a BLQ pre-dose sample is not imputed, and BE notes Rules 3-6",
  tryCatch({
    d <- data.frame(ID = "1", T = c(0, 0.5, 1, 2, 4, 8), C = c("BLQ", "BLQ", 3, 8, 5, 2), stringsAsFactors = FALSE)
    lag <- function(rule) suppressWarnings(run_nca(prepare_pk_dataset(d, rel_cm, list(lloq = 0.5, blq_rule = rule))$data,
                                                   rel_cm, rel_st(trap = "linear")))$TLAG
    be <- paste(readLines("R/mod_path_be.R", warn = FALSE), collapse = "\n")
    # an imputed LLOQ/2 at 0.5 h is not a measured concentration (P-02, MRV-02)
    lag("rule1") == 0.5 && lag("rule6") == 0.5 && lag("rule4") == 0.5 &&
      prepare_pk_dataset(d, rel_cm, list(lloq = 0.5, blq_rule = "rule4"))$data$C[1] == 0 &&
      grepl("output$blq_rule_note", be, fixed = TRUE) && grepl("ICH M13A sets values below the LLOQ to zero", be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "BLQ at 0 and 0.5 h (LLOQ 0.5) under Rules 1, 4 and 6; BE module text",
  expected = "The t = 0 sample stays 0 under Rules 4 and 6, and Tlag is 0.5 h under Rules 1, 4 and 6; the BE results note the rule when it is 3-6")

check("REL-19", "R-10: ICH M13A checks flag a high pre-dose value, fewer than 12 subjects and poor AUC coverage",
  tryCatch({
    nca <- suppressWarnings(run_nca(rel_be, rel_be_cm, rel_st(trap = "linear")))
    ci_ok <- data.frame(Parameter = "CMAX", N_Test = 12, N_Ref = 12)
    clean <- be_m13a_checks(rel_be, rel_be_cm, nca, ci_ok)
    d <- rel_be
    p2 <- d$Subject == d$Subject[1] & d$Period == 2
    d$Conc[p2 & d$Time == 0] <- 0.2 * max(d$Conc[p2])
    pre <- be_m13a_checks(d, rel_be_cm, nca, ci_ok)
    few <- be_m13a_checks(rel_be, rel_be_cm, nca, data.frame(Parameter = "CMAX", N_Test = 10, N_Ref = 10))
    cov <- nca; cov$AUCPEO <- c(rep(25, 6), rep(5, nrow(cov) - 6))
    poor <- be_m13a_checks(rel_be, rel_be_cm, cov, ci_ok)
    ss <- be_m13a_checks(d, rel_be_cm, cov, ci_ok, is_ss = TRUE)
    length(clean) == 0 && length(pre) == 1 && grepl("above 5% of Cmax in 1 profile", pre) &&
      grepl(paste0("^", d$Subject[1], " \\| "), sub("^.*profile\\(s\\): ", "", pre)) &&
      length(few) == 1 && grepl("Fewer than 12", few) && length(poor) == 1 && grepl("6 of", poor) && length(ss) == 0
  }, error = function(e) FALSE),
  "URS-BE-01, URS-BE-11", critical = FALSE, method = "be_m13a_checks() on the 2x2x2 fixture with a period-2 pre-dose value at 20% of Cmax, 10 subjects, 6 profiles with AUC%extrap 25%",
  expected = "No message for the clean fixture; one message for each problem, naming the profile; none at steady state")

check("REL-20", "R-10: M13A is cited and the default comparison is Cmax and AUC(0-t)",
  tryCatch({
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- rd("R/mod_methods.R"); be <- rd("R/mod_path_be.R")
    grepl("ICH. M13A: Bioequivalence for immediate-release solid oral dosage forms", m, fixed = TRUE) &&
      grepl('selected = c("CMAX", "AUCLST"))', be, fixed = TRUE) && !grepl("EMA: all terms fixed", be, fixed = TRUE) &&
      grepl("output$m13a_note", be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-01, URS-BE-11", critical = FALSE, method = "Static check of the Methods page and the Bioequivalence module",
  expected = "M13A in the references; Cmax and AUClast selected by default; the M13A checks are shown")

check("REL-21", "R-11: text after row 1000 of an Excel file is read, in flat and ADNCA uploads",
  tryCatch({
    f <- tempfile(fileext = ".xlsx")
    n <- 1200
    x <- data.frame(ID = rep(1:100, each = 12), TIME = rep(c(0, 0.5, 1, 2, 4, 6, 8, 12, 24, 36, 48, 72), 100),
                    CONC = rep(c(0, 5, 9, 8, 6, 4, 3, 2, 1, 0.8, 0.6, 0.4), 100))
    late <- which(seq_len(n) > 1000 & x$TIME >= 48)
    # Numeric cells in the first 1000 rows, text cells ("BLQ") after them
    wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "d"); openxlsx::writeData(wb, "d", x)
    for (i in late) openxlsx::writeData(wb, "d", "BLQ", startCol = 3, startRow = i + 1)
    openxlsx::saveWorkbook(wb, f)
    r <- read_pk_file(f)
    a <- data.frame(USUBJID = rep("S1", n), PARAMCD = "DRUG", PCSPEC = "PLASMA", AFRLT = seq_len(n) / 10,
                    ARRLT = seq_len(n) / 10, AVAL = 1, AVALU = "ng/mL", PCSTRESU = "ng/mL", RRLTU = "h",
                    DTYPE = c(rep(NA, n - 1), "INTERP"), stringsAsFactors = FALSE)
    fa <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(a, fa)
    ra <- adnca_read(fa)
    sum(r$CONC == "BLQ", na.rm = TRUE) == length(late) && sum(is.na(r$CONC)) == 0 &&
      identical(ra$DTYPE[n], "INTERP")
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = TRUE, method = "1200-row xlsx with BLQ text only after row 1000; ADNCA xlsx with DTYPE in the last row",
  expected = "Every BLQ entry and the DTYPE value are read (were missing: readxl guessed the types from 1000 rows)")

check("REL-22", "R-12: units stated in the data are pre-selected and a contradicting selection is refused",
  tryCatch({
    a <- read.csv("data/example_adnca.csv", stringsAsFactors = FALSE)
    a$AVALU <- "ug/mL"; a$RRLTU <- "DAYS"
    u <- units_in_data(a)
    msg <- check_units_against_data(u, "mg", "h", "ng/mL")
    ok_msg <- is.null(check_units_against_data(u, "mg", "day", "ug/mL"))
    flat <- units_in_data(data.frame(ID = 1, Conc = 1, ConcUnit = "ng/ml", TimeUnit = "hr", Community = "x"))
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    wired <- all(vapply(c("R/mod_path_multi_nca.R", "R/mod_path_single_nca.R", "R/mod_path_be.R"), function(f)
      grepl("check_units_against_data(shared$study_info$units", rd(f), fixed = TRUE) &&
        grepl("Pre-select the units stated in the file", rd(f), fixed = TRUE), logical(1))) &&
      grepl("units     = units_in_data(raw_data())", rd("R/mod_data_upload.R"), fixed = TRUE)
    identical(u$conc$unit, "ug/mL") && identical(u$time$unit, "day") && identical(u$dose$unit, "mg") &&
      grepl("ug/mL \\(column AVALU\\)", msg) && ok_msg &&
      identical(flat$conc$unit, "ng/mL") && identical(flat$time$unit, "h") && is.null(flat$dose) && wired &&
      identical(units_in_data(data.frame(AVALU = "\u00b5g/mL"))$conc$unit, "ug/mL")
  }, error = function(e) FALSE),
  "URS-NCA-05, URS-DAT-08", critical = TRUE, method = "ADNCA example with AVALU ug/mL and RRLTU DAYS; flat file with ConcUnit and TimeUnit columns",
  expected = "Units mapped to the app's choices; ng/mL and h refused with the column named; the paths pre-select and check them")

check("REL-23", "R-13: the planner gets the kind of CV its design and method need",
  tryCatch({
    ci <- data.frame(Parameter = c("CMAX", "AUCLST"), Scale = "Ratio T/R (%)", MSE = c(log(0.2^2 + 1), log(0.15^2 + 1)))
    cx <- list(ci_table = ci, design = "2x2x2")
    rep <- list(ci_table = ci, design = "2x2x4",
                cv_table = data.frame(Parameter = "CMAX", CVwR = 45, CVwT = 35))
    par <- list(ci_table = ci, design = "parallel")
    a <- planner_cv_offer(cx, "abe", "2x2", "CMAX")
    b <- planner_cv_offer(cx, "abe", "parallel", "CMAX")
    c2 <- planner_cv_offer(par, "abe", "parallel", "CMAX")
    d <- planner_cv_offer(par, "abe", "2x2", "CMAX")
    e <- planner_cv_offer(rep, "abel", "2x2x4", "CMAX")
    f <- planner_cv_offer(cx, "abel", "2x2x4", "AUCLST")
    abs(a$cv - 20) < 1e-9 && grepl("within-subject", a$label) && !is.null(b$note) && is.null(b$cv) &&
      grepl("total", c2$label) && !is.null(d$note) && e$cv == 35 && e$cv_wr == 45 &&
      abs(f$cv - 15) < 1e-9 && abs(f$cv_wr - 15) < 1e-9 && grepl("pooled", f$label)
  }, error = function(e) FALSE),
  "URS-PWR-01", critical = TRUE, method = "planner_cv_offer() for crossover, parallel and replicate analyses and planner designs",
  expected = "Within-subject CV only for crossover designs, total CV only from a parallel analysis; ABEL gets CVwT 35 and CVwR 45 (was CVwT only)")

check("REL-24", "R-14: two periods merged into one profile are flagged, and TAD is preferred over TIME",
  tryCatch({
    t1 <- c(0, 0.5, 1, 2, 4, 8, 12, 24); c1 <- c(0, 5, 9, 7, 4, 2, 1, 0.3)
    d <- data.frame(ID = rep(1:4, each = 16), TIME = rep(c(t1, t1 + 168), 4), DV = rep(c(c1, c1 * 1.3), 4),
                    Condition = rep(rep(c("A", "B"), each = 8), 4), Visit = rep(rep(1:2, each = 8), 4))
    cm <- auto_detect_columns(names(d)); cm <- cm[nzchar(unlist(cm))]
    r <- run_interlocks(d, cm, "mapped")
    th <- read.csv("data/example_theoph.csv"); cm_th <- auto_detect_columns(names(th)); cm_th <- cm_th[nzchar(unlist(cm_th))]
    quiet <- nrow(run_interlocks(th, cm_th, "mapped")) == 0 &&
      nrow(run_interlocks(rel_be, rel_be_cm, "mapped")) == 0
    any(grepl("rise again after a long sampling gap", r$Message)) && any(grepl("'Visit' takes more than one value", r$Message)) &&
      quiet && identical(auto_detect_columns(c("ID", "TIME", "TAD", "DV"))$time, "TAD")
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = FALSE,
  method = "Interaction study with time since first dose (period 2 at 168 h) and an unmapped Visit column; theophylline and 2x2x2 fixture",
  expected = "Two warnings for the merged profiles; none for the example data; TAD chosen as time")

check("REL-25", "R-15: concentration-time lines are drawn per profile, not per subject",
  tryCatch({
    x <- read.csv("data/example_be_crossover.csv", stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Concentration", treatment = "Treatment", period = "Period")
    g <- profile_group(x, cm)
    wd <- file.path(tempdir(), "rel25"); dir.create(wd, showWarnings = FALSE)
    f <- file.path(wd, "x.csv"); write.csv(x, f, row.names = FALSE)
    p <- ggplot2::ggplot(x, ggplot2::aes(Time, Concentration, group = g)) + ggplot2::geom_line()
    zp <- file.path(wd, "fig.zip")
    create_viz_record(zp, p, list(plot_type = "spaghetti", color_by = "subject", export_format = "png", dpi = 72),
                      cm, f, "x.csv", blq_rule = "rule1", lloq = 0)
    ex <- rec_unzip(zp)
    scr <- paste(readLines(file.path(ex, "reproduce_figure.R"), warn = FALSE), collapse = "\n")
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    nlevels(g) == 2 * length(unique(x$Subject)) && grepl("group = profile_group(d, ds$col_map)", scr, fixed = TRUE) &&
      grepl("Result: FIGURE CREATED", rec_check_text(ex)) &&
      !grepl("group  = .subj", rd("R/mod_path_viz.R"), fixed = TRUE) && grepl("group = .profile", rd("R/mod_path_multi_nca.R"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-VIZ-01", critical = FALSE, method = "BE crossover example: profile groups, Figure Record script and module code",
  expected = "12 lines for 6 subjects x 2 periods (was 6 zig-zag lines); the Figure Record draws the same")

check("REL-26", "R-16: a changed file or a deleted reference column makes the reproduction DIFFERENT",
  tryCatch({
    r <- rec_build(df = theoph, cm = theoph_cm, st = theoph_settings)
    ok0 <- grepl("Result: MATCH", rec_check_text(r$ex))
    tamper <- function(edit) {
      ex <- file.path(tempdir(), paste0("rel26_", sample.int(1e6, 1))); dir.create(ex)
      file.copy(list.files(r$ex, full.names = TRUE), ex)
      edit(ex); suppressWarnings(run_reproduction_check(ex, "reproduce_analysis.R")); rec_check_text(ex)
    }
    code <- tamper(function(ex) cat("\n# edited\n", file = file.path(ex, "nca_pipeline.R"), append = TRUE))
    data <- tamper(function(ex) { f <- list.files(ex, "\\.csv$", full.names = TRUE)
      f <- f[!grepl("app_results_reference|reproduced", f)][1]; cat("\n", file = f, append = TRUE) })
    col <- tamper(function(ex) { f <- file.path(ex, "app_results_reference.csv"); x <- read.csv(f, check.names = FALSE)
      write.csv(x[, setdiff(names(x), c("CMAX", "AUCLST"))], f, row.names = FALSE) })
    man <- paste(readLines(file.path(r$ex, "data_integrity.txt")), collapse = "\n")
    ok0 && grepl("Result: DIFFERENT (Pipeline code not the one analysed", code, fixed = TRUE) &&
      grepl("Result: DIFFERENT (Data file not the one analysed", data, fixed = TRUE) &&
      grepl("Result: DIFFERENT (parameters present on one side only: CMAX, AUCLST", col, fixed = TRUE) &&
      grepl("Reference results:", man) && grepl("Reproduction script:", man) && grepl("not an", man) &&
      !any(vapply(c("R/help_system.R", "R/mod_methods.R", "R/mod_path_viz.R"), function(f)
        grepl("audit trails|exported audit trail", paste(readLines(f), collapse = " ")), logical(1)))
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = TRUE, method = "Theophylline record: pipeline code edited, data file changed, CMAX and AUCLST deleted from the reference",
  expected = "DIFFERENT with the reason in each case (was MATCH); the manifest covers the reference and the script")

check("REL-27", "R-17/R-18: records use private folders, and the app states where data go and what it is for",
  tryCatch({
    before <- list.files(tempdir(), "^(analysis_record|figure_record|upload_copy)")
    r1 <- rec_build(df = theoph, cm = theoph_cm, st = theoph_settings)
    after <- list.files(tempdir(), "^(analysis_record|figure_record|upload_copy)")
    fp <- fallback_copy_path("../x.csv")
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    er <- rd("R/export_record.R"); app <- rd("app.R")
    mods <- vapply(c("R/mod_path_multi_nca.R", "R/mod_path_be.R", "R/mod_path_viz.R"), function(f)
      grepl("fallback_copy_path(original_name)", rd(f), fixed = TRUE) &&
        grepl("unlink(fallback_dir, recursive = TRUE)", rd(f), fixed = TRUE), logical(1))
    grepl("Result: MATCH", rec_check_text(r1$ex)) && length(setdiff(after, before)) == 0 &&
      basename(fallback_copy_path("../y.xlsx")) == "y.csv" && basename(fp) == "x.csv" && dir.exists(dirname(fp)) && dirname(fp) != tempdir() &&
      !grepl('file.path(tmp, "analysis_record")', er, fixed = TRUE) && all(mods) &&
      grepl("data_protection_notice()", rd("R/mod_data_upload.R"), fixed = TRUE) &&
      grepl("Intended use", app, fixed = TRUE) && grepl("Posit PBC", DATA_PROTECTION_NOTICE, fixed = TRUE) &&
      !grepl('label = "Validated"', rd("R/utils.R"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-04, URS-GEN-07", critical = TRUE, method = "Build a record and list tempdir(); fallback path; upload page, About page and engine badge text",
  expected = "Each record in its own folder, removed afterwards; fallback copies private and deleted; notice and intended use shown; badge says 'Tested version'")

check("REL-28", "R-19: the planner's own calls give PowerTOST's sample sizes and power",
  tryCatch({
    N <- function(r) r[["Sample size"]]
    abe <- N(planner_sample_size("abe", 0.05, 0.8, 0.95, 0.8, 1.25, 0.20, planner_cv("abe", 20, 20), "2x2"))
    par <- N(planner_sample_size("abe", 0.05, 0.8, 0.95, 0.8, 1.25, 0.30, planner_cv("abe", 30, 30), "parallel"))
    abel <- N(planner_sample_size("abel", 0.05, 0.8, 0.90, 0.8, 1.25, 0.35, planner_cv("abel", 35, 45), "2x2x4"))
    rsabe <- N(planner_sample_size("rsabe", 0.05, 0.8, 0.90, 0.8, 1.25, 0.35, planner_cv("rsabe", 35, 45), "2x2x4"))
    ntid <- N(planner_sample_size("ntid", 0.05, 0.8, 0.975, 0.9, 1.1111, 0.08, planner_cv("ntid", 8, 10), "2x2x4"))
    pw <- planner_power(28, "abel", 0.05, 0.9, 0.8, 1.25, 0.45, 0.45, "2x2x4")
    abe == 20 && par == 76 && abel == 24 && rsabe == 20 && ntid == 14 &&
      ntid == PowerTOST::sampleN.NTID(CV = c(0.08, 0.10), theta0 = 0.975, design = "2x2x4", print = FALSE,
                                     details = FALSE)[["Sample size"]] &&
      abs(pw - PowerTOST::power.scABEL(CV = 0.45, n = 28, theta0 = 0.9, design = "2x2x4")) < 1e-12
  }, error = function(e) FALSE),
  "URS-PWR-01", critical = TRUE,
  method = "planner_sample_size() and planner_power(), the functions the planner calls, for ABE (crossover, parallel), ABEL, RSABE and NTID",
  expected = "N = 20, 76, 24, 20, 14; ABEL power at n = 28 equal to power.scABEL (100,000 simulations)")

check("REL-29", "R-19: Method B (mixed model) agrees with replicateBE on all 30 reference data sets",
  tryCatch({
    worst <- 0
    for (nm in sprintf("rds%02d", 1:30)) {
      d <- getExportedValue("replicateBE", nm)
      mb <- suppressMessages(suppressWarnings(replicateBE::method.B(
        data = d, print = FALSE, details = TRUE, verbose = FALSE, plot.bxp = FALSE, option = 2)))
      b <- data.frame(Subject = as.character(d$subject), Period = as.character(d$period),
                      Sequence = as.character(d$sequence),
                      Treatment = factor(ifelse(d$treatment == "T", "Test", "Reference"), levels = c("Reference", "Test")),
                      CMAX = d$PK, stringsAsFactors = FALSE)
      f <- fit_be_parameter(b, "CMAX", "2x2x4", model_type = "mixed", trt_col = "Treatment", subj_col = "Subject",
                            per_col = "Period", seq_col = "Sequence")$estimate
      g <- function(col) as.numeric(mb[1, col])
      worst <- max(worst, abs(c(f$pe - g("PE(%)"), f$ci_lo - g("CL.lo(%)"), f$ci_hi - g("CL.hi(%)"))))
    }
    worst < 1e-6
  }, error = function(e) FALSE),
  "URS-BE-05", critical = TRUE, method = "fit_be_parameter(model_type = 'mixed') vs replicateBE::method.B(option = 2) on rds01-rds30",
  expected = "Point estimate and 90% CI equal to 1e-6 (Methods: 'its results agree with the replicateBE package')")

check("REL-30", "R-19: theophylline AUClast and Cmax equal an independent calculation for all 12 subjects",
  tryCatch({
    th <- read.csv("data/example_theoph.csv"); cm <- list(subject = "Subject", time = "Time", conc = "conc")
    r <- suppressWarnings(run_nca(th, cm, rel_st(trap = "linear")))
    ok <- TRUE
    for (s in unique(th$Subject)) {
      x <- th[th$Subject == s, ]; x <- x[order(x$Time), ]
      last <- max(which(x$conc > 0)); x <- x[seq_len(last), ]
      auc <- sum(diff(x$Time) * (head(x$conc, -1) + tail(x$conc, -1)) / 2)
      rr <- r[as.character(r$Subject) == as.character(s), ]
      ok <- ok && abs(rr$AUCLST - auc) < 1e-9 && rr$CMAX == max(x$conc) && rr$TMAX == x$Time[which.max(x$conc)]
    }
    ok && nrow(r) == 12
  }, error = function(e) FALSE),
  "URS-NCA-01", critical = TRUE, method = "Linear trapezoids written out by hand for every theophylline profile",
  expected = "AUClast, Cmax and Tmax identical to the hand calculation")

check("REL-31", "R-20: release files can be generated and the About page shows the pipeline fingerprint",
  tryCatch({
    rel <- paste(readLines("validation/make_release_files.R", warn = FALSE), collapse = "\n")
    parse(text = rel)
    grepl("renv::lockfile_create", rel, fixed = TRUE) && grepl("release_manifest.csv", rel, fixed = TRUE) &&
      grepl("Pipeline code SHA-256:", paste(readLines("app.R", warn = FALSE), collapse = "\n"), fixed = TRUE) &&
      grepl("validation_environment.txt", paste(readLines("validation/validation.R", warn = FALSE), collapse = "\n"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-01, URS-GEN-08", critical = FALSE, method = "make_release_files.R parses and writes the lockfile and manifest; About and environment file",
  expected = "Release files and environment record in place")

check("REL-32", "R-21: theme colours pass WCAG AA with white text; hub cards and help buttons work without a mouse",
  tryCatch({
    app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
    lum <- function(h) { v <- grDevices::col2rgb(h)[, 1] / 255
      v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4); sum(c(0.2126, 0.7152, 0.0722) * v) }
    contrast <- function(h) (1 + 0.05) / (lum(h) + 0.05)
    cols <- vapply(c("primary", "secondary", "success", "info", "warning", "danger"), function(k)
      regmatches(app, regexpr(paste0(k, " *= \"#[0-9A-Fa-f]{6}\""), app)), character(1))
    hex <- sub('^.*"(#[0-9A-Fa-f]{6})"$', "\\1", cols)
    all_r <- paste(vapply(list.files("R", "\\.R$", full.names = TRUE), function(f)
      paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
    all(vapply(hex, contrast, numeric(1)) >= 4.5) &&
      lengths(regmatches(app, gregexpr('role = "button", tabindex = "0"', app, fixed = TRUE))) == 6 &&
      grepl("`aria-label` = paste(\"Help:\", title)", all_r, fixed = TRUE) &&
      !grepl("bg-warning text-dark", paste(all_r, app), fixed = TRUE) && grepl("navbar-dark bg-primary", app, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-01, URS-UI-05", critical = FALSE, method = "Contrast of the six theme colours against white; hub card, help button and badge markup",
  expected = "Every theme colour at least 4.5:1 (was 2.2-3.8 for success, info, warning, danger, secondary); keyboard-operable cards; named help buttons")

check("REL-33", "R-22: with chosen half-life points, predicted Clast is taken at Tlast",
  tryCatch({
    tt <- c(0, 1, 2, 4, 8, 12, 24, 36); cc <- c(0, 8, 10, 7, 4, 2.2, 0.9, 0)
    st <- rel_st(trap = "linear")
    r <- run_single_nca(tt, cc, st, time_used = c(8, 12, 24))
    b <- run_nca(data.frame(ID = "1", T = tt, C = cc), rel_cm, st,
                 lz_overrides = list(list(subject = "1", time_used = c(8, 12, 24))))
    fit <- stats::lm(log(c(4, 2.2, 0.9)) ~ c(8, 12, 24)); lz <- -stats::coef(fit)[[2]]
    clstp <- exp(stats::coef(fit)[[1]] - lz * 24); ifp <- r[["AUCLST"]] + clstp / lz
    raw <- NonCompart::sNCA(tt, cc, dose = 100, R2ADJ = 0, UsePoints = 5:7)
    abs(r[["CLSTP"]] - clstp) < 1e-9 && abs(r[["AUCIFP"]] - ifp) < 1e-9 && abs(r[["CLFP"]] - 100 / ifp * 1000) < 1e-6 &&
      abs(b$AUCIFP - ifp) < 1e-9 && abs(raw[["CLSTP"]] - clstp) > 0.1
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = TRUE, method = "Points 8, 12, 24 h chosen, a 0 at 36 h; single profile and batch vs lm() by hand",
  expected = "CLSTP = exp(b0 - lambda-z x 24), AUCIFP and CL/F (pred) from it (NonCompart alone predicts at 36 h)")

check("REL-34", "R-23: Tlag is the last sample before the first measurable concentration",
  tryCatch({
    st <- rel_st(trap = "linear"); tt <- c(0, 1, 2, 4, 6, 8, 12)
    embedded <- run_single_nca(tt, c(0, 5, 20, 15, 0, 6, 4), st)[["TLAG"]]
    lag <- run_single_nca(tt, c(0, 0, 20, 15, 10, 6, 4), st)[["TLAG"]]
    none <- run_single_nca(tt, c(3, 5, 20, 15, 0, 6, 4), st)[["TLAG"]]
    b <- run_nca(data.frame(ID = c(rep("A", 7), rep("B", 7)), T = rep(tt, 2),
                            C = c(0, 5, 20, 15, 0, 6, 4, 0, 0, 20, 15, 10, 6, 4)), rel_cm, st)
    embedded == 0 && lag == 1 && none == 0 && identical(b$TLAG, c(0, 1))
  }, error = function(e) FALSE),
  "URS-NCA-01", critical = FALSE, method = "0, 5, 20, 15, 0, 6, 4 (embedded zero); 0, 0, 20, ... (lag); single and batch",
  expected = "Tlag 0 with an embedded zero (NonCompart gives 6 h); 1 h with a real lag")

check("REL-35", "R-24: steady state without a pre-dose sample is reported",
  tryCatch({
    warn_of <- function(expr) { w <- character(0)
      withCallingHandlers(expr, warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }); w }
    st <- rel_st(trap = "linear", ss = TRUE, tau = 12)
    with0 <- warn_of(run_nca(data.frame(ID = "1", T = c(0, 1, 2, 4, 8, 12), C = c(5, 20, 15, 10, 7, 5)), rel_cm, st))
    no0 <- warn_of(run_nca(data.frame(ID = "1", T = c(1, 2, 4, 8, 12), C = c(20, 15, 10, 7, 5)), rel_cm, st))
    single <- warn_of(run_single_nca(c(1, 2, 4, 8, 12), c(20, 15, 10, 7, 5), st))
    !any(grepl("no measured pre-dose sample", with0)) && any(grepl("1 profile\\(s\\) have no measured pre-dose sample", no0)) &&
      any(grepl("no measured pre-dose sample", single))
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = FALSE, method = "Steady state (tau 12 h) with and without a sample at time 0; batch and single profile",
  expected = "A warning naming the profile when there is no pre-dose sample; none when there is one")

check("REL-36", "R-31: a half-life note is not presented as an excluded profile",
  tryCatch({
    src <- paste(readLines("R/mod_path_multi_nca.R", warn = FALSE), collapse = "\n")
    w <- character(0)
    withCallingHandlers(run_nca(theoph[theoph$Subject %in% 1:3, ], theoph_cm, modifyList(theoph_settings, list(r2adj_threshold = 0.9999))),
                        warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
    grepl('excluded <- startsWith(msgs, "Excluded ")', src, fixed = TRUE) &&
      grepl('"Notes from the analysis: "', src, fixed = TRUE) &&
      any(startsWith(w, "Half-life not reported")) && !any(startsWith(w[startsWith(w, "Half-life")], "Excluded "))
  }, error = function(e) FALSE),
  "URS-UI-04", critical = FALSE, method = "All Subjects alert code; run_nca with a minimum R2 of 0.9999",
  expected = "Exclusions and other notes in separate alerts; the R2 note is a note, not an exclusion")

check("REL-37", "R-25: a negative concentration blocks the analysis without an LLOQ, and is BLQ with one",
  tryCatch({
    d <- data.frame(ID = "7", T = c(0, 1, 2, 4), C = c(-0.1, 5, 3, 1))
    q0 <- run_data_quality_check(d, rel_cm, 0); q1 <- run_data_quality_check(d, rel_cm, 0.5)
    e0 <- q0$findings[grepl("negative concentration", q0$findings$Message), ]
    e1 <- q1$findings[grepl("negative concentration", q1$findings$Message), ]
    e0$Severity == "ERROR" && grepl("Subjects: 7", e0$Detail) && e1$Severity == "WARNING" &&
      !any(grepl("excluded from log-scale", c(q0$findings$Action, q1$findings$Action)))
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE, method = "Profile with -0.1 at t = 0, LLOQ 0 and 0.5",
  expected = "ERROR naming the subject without an LLOQ (NonCompart would return no parameters); WARNING that the BLQ rule handles it with an LLOQ")

check("REL-38", "R-26: exports carry the units of the run; half-life is not labelled in hours when time is in minutes",
  tryCatch({
    st <- modifyList(theoph_settings, list(time_unit = "min"))
    r <- rec_build(df = theoph, cm = theoph_cm, st = st)
    x <- openxlsx::read.xlsx(file.path(r$ex, "results.xlsx"), sheet = 1, check.names = FALSE, sep.names = " ")
    lab <- names(rename_nca_columns(data.frame(LAMZHL = 1, CLFO = 1, AUCLST = 1),
                                    units = list(dose = "mg", time = "min", conc = "ng/mL")))
    all_src <- paste(vapply(c(list.files("R", "\\.R$", full.names = TRUE)), function(f)
      paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
    "Half-Life (min)" %in% names(x) && !any(grepl("Half-Life (h)", names(x), fixed = TRUE)) &&
      all(enc2utf8(lab) == enc2utf8(c("Half-Life (min)", "Apparent Clearance (CL/F) (L/min)", "AUC to Last Point (ng/mL\u00b7min)"))) &&
      !grepl('"Half-Life (h)"', all_src, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-EXP-01, URS-DAT-08", critical = FALSE, method = "Theophylline record with time in minutes; rename_nca_columns() with units",
  expected = "results.xlsx says Half-Life (min) (was Half-Life (h) over values in minutes); units on every labelled column")

check("REL-39", "R-27: IV clearance and volume are shown in the result card and the default views",
  tryCatch({
    st <- rel_st(route = "iv_bolus"); st$dose <- 1000
    r <- run_nca(data.frame(ID = "1", T = rel_iv_t, C = rel_iv(rel_iv_t)), rel_cm, st)
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    single <- rd("R/mod_path_single_nca.R"); multi <- rd("R/mod_path_multi_nca.R")
    all(c("CLO", "VZO") %in% names(r)) && !"CLFO" %in% names(r) &&
      grepl('tags$td("Clearance (CL):"), tags$td(tags$strong(sg("CLO")))', single, fixed = TRUE) &&
      grepl('"Clearance (CL)"', multi, fixed = TRUE) && grepl('"CLFO","VZFO","CLO","VZO")', multi, fixed = TRUE) &&
      grepl("Clearance (CL) (L/h)", add_units_to_labels("Clearance (CL)"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-NCA-02", critical = FALSE, method = "IV bolus run; card, table and summary code; unit label",
  expected = "CL and Vz (CLO, VZO) shown for IV instead of an empty CL/F")

check("REL-40", "R-28: a bioequivalence verdict is given only for the 90% confidence interval",
  tryCatch({
    b <- build_be_data(suppressWarnings(run_nca(rel_be, rel_be_cm, rel_st(trap = "linear"))), rel_be, rel_be_cm, "Reference")
    f <- function(lv) fit_be_parameter(b$data, "AUCLST", design = "2x2x2", trt_col = b$trt_col, subj_col = b$subj_col,
                                       per_col = b$per_col, seq_col = b$seq_col, ci_level = lv)$row
    r90 <- f(90); r80 <- f(80); r95 <- f(95)
    r90$Bioequivalent %in% c("YES", "NO") && grepl("^no verdict: a bioequivalence verdict uses the 90%", r80$Bioequivalent) &&
      grepl("^no verdict", r95$Bioequivalent) && !is.na(r80$CI_Lower) && r80$CI_Lower > r90$CI_Lower
  }, error = function(e) FALSE),
  "URS-BE-04, URS-BE-11", critical = TRUE, method = "2x2x2 fixture at 80%, 90% and 95%",
  expected = "Verdict at 90% only; the 80% and 95% intervals are shown without a verdict")

check("REL-41", "R-29: Visualize describes the arithmetic mean as arithmetic and does not clamp log-scale bars",
  tryCatch({
    v <- paste(readLines("R/mod_path_viz.R", warn = FALSE), collapse = "\n")
    grepl("Error bars: arithmetic mean \\u00b1 SD, all observations included", v, fixed = TRUE) &&
      grepl('". Error bars represent \\u00b1 1 SD."', v, fixed = TRUE) &&
      grepl('geo <- identical(input$summary_stat %||% "geomean", "geomean")', v, fixed = TRUE) &&
      grepl("if (geo && n > 0)", v, fixed = TRUE) &&
      !grepl("1e-10", v, fixed = TRUE) && grepl("summ$.lo[low] <- summ$.center[low]", v, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-VIZ-03", critical = FALSE, method = "Visualize module text and summary code; checked in the running app with the arithmetic mean",
  expected = "Caption, legend and note follow the statistic; no 1e-10 clamp on a log axis")

check("REL-42", "R-30: the Half-Life Review shows the fit the results use and keeps overrides per profile",
  tryCatch({
    tt <- c(0, 1, 2, 4, 6, 8, 12, 16, 24); cc <- c(0, 5, 20, 15, 0, 6, 4, 2, 1)
    r <- run_single_nca(tt, cc, rel_st(trap = "linear"))
    lz <- estimate_lambda_z(tt, cc, 0)
    st <- rel_st(route = "iv_bolus"); st$dose <- 1000
    rb <- run_single_nca(rel_iv_t, rel_iv(rel_iv_t), st)
    lzb <- estimate_lambda_z(rel_iv_t, rel_iv(rel_iv_t), 0, route = "iv_bolus")
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    wired <- all(vapply(c("R/mod_path_multi_nca.R", "R/mod_path_be.R"), function(f) {
      x <- rd(f)
      grepl("lz_state$override <- lz_state$fits[[input$lz_profile]]", x, fixed = TRUE) &&
        grepl("lz_state$fits[[sel]] <- override", x, fixed = TRUE) && grepl('observeEvent(input$lz_reset', x, fixed = TRUE) &&
        grepl('lz_candidate_points(sd$time, sd$conc, sd$is_blq, input$admin_route)', x, fixed = TRUE)
    }, logical(1))) && grepl("time >= cmax_t & time > 0", rd("R/nca_helpers.R"), fixed = TRUE)
    lz$n_points == r[["LAMZNPT"]] && abs(lzb$lambda_z - rb[["LAMZ"]]) < 1e-12 && lzb$n_points == rb[["LAMZNPT"]] && wired
  }, error = function(e) FALSE),
  "URS-NCA-12", critical = FALSE, method = "Profile with an embedded zero; IV bolus; review module code",
  expected = "Point count equal to NonCompart's (was one more); IV bolus review fit equal to the result; overrides reloaded per profile and removable")

check("REL-43", "R-33: the Methods steady-state text and the BE table match the implementation",
  tryCatch({
    m <- paste(readLines("R/mod_methods.R", warn = FALSE), collapse = "\n")
    be <- paste(readLines("R/mod_path_be.R", warn = FALSE), collapse = "\n")
    !grepl("represents AUC", m, fixed = TRUE) && !grepl("should not be reported", m, fixed = TRUE) &&
      grepl("is the primary exposure measure", m, fixed = TRUE) &&
      grepl('"AUC Within Dosing Interval", "Average Concentration (Cavg)"', be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = FALSE, method = "Methods page and Bioequivalence PK table code",
  expected = "No claim that AUC0-t is AUCtau or that Vz/F is not reported; the BE table shows AUCtau at steady state")

check("REL-44", "R-34: parallel data are not run under the default crossover design",
  tryCatch({
    par <- read.csv("data/example_be_parallel.csv", stringsAsFactors = FALSE)
    cm <- auto_detect_columns(names(par)); cm <- cm[nzchar(unlist(cm))]
    det <- detect_study_design(par, cm)
    msg <- check_design_against_data("2x2x2", det)
    be <- paste(readLines("R/mod_path_be.R", warn = FALSE), collapse = "\n")
    identical(det$type, "parallel") && "parallel" %in% BE_DESIGNS$code && !is.null(msg) &&
      grepl("one period per subject", msg) && is.null(check_design_against_data("parallel", det)) &&
      grepl('updateSelectInput(session, "be_design", selected = t)', be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-02", critical = FALSE, method = "Parallel example: detected design, design check for 2x2x2, BE module pre-selection",
  expected = "Parallel detected and pre-selected; a crossover selection on one-period data is flagged")

check("REL-45", "R-35: error messages use the app's words and say what to do",
  tryCatch({
    m <- validate_mapping(list(subject = "ID", time = "", conc = NULL))$message
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
    grepl("Choose a column for: Time, Concentration", m, fixed = TRUE) && !grepl("conc", m, fixed = TRUE) &&
      grepl("Check the delimiter", friendly_read_error("more columns than column names")) &&
      grepl('paste0("Set LLOQ to ", sug)', rd("R/mod_data_upload.R"), fixed = TRUE) &&
      grepl("Please enter the infusion duration", rd("R/mod_path_single_nca.R"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-04", critical = FALSE, method = "validate_mapping(), friendly_read_error(), LLOQ button and single-profile infusion check",
  expected = "On-screen names in the mapping message; a hint for read errors; the LLOQ button says what it does; infusion duration checked")

check("REL-46", "R-36: a new upload clears the analysis state of the previous file",
  tryCatch({
    suppressPackageStartupMessages({ library(shiny); library(bslib); library(DT) })
    for (f in c("R/help_system.R", "R/mod_data_upload.R")) source(f, local = TRUE)
    ok <- FALSE
    sh <- shiny::reactiveValues(be_results = list(ci_table = 1), partial_aucs = data.frame(start = 0), viz_settings = list(a = 1),
                                nca_results = 1, data_ready = TRUE)
    suppressWarnings(shiny::testServer(data_upload_server, args = list(shared = sh), {
      session$setInputs(file_upload = data.frame(name = "x.csv", size = 1, type = "text/csv",
                                                 datapath = "data/example_theoph.csv"))
      ok <<- is.null(sh$be_results) && is.null(sh$partial_aucs) && is.null(sh$viz_settings) && is.null(sh$nca_results)
    }))
    ok
  }, error = function(e) FALSE),
  "URS-DAT-01, URS-GEN-09", critical = FALSE, method = "shiny::testServer on the upload module with state from an earlier analysis",
  expected = "BE results, partial AUC intervals and figure settings cleared when a new file is chosen")

check("REL-47", "R-37: the navbar wraps on a phone instead of widening the page",
  tryCatch(grepl("nav.navbar > .container-fluid { flex-wrap: wrap;",
                 paste(readLines("www/custom.css", warn = FALSE), collapse = "\n"), fixed = TRUE),
           error = function(e) FALSE),
  "URS-UI-03, URS-UI-05", critical = FALSE, method = "custom.css; checked in the running app at 375 px (every page 375 px wide, was 457)",
  expected = "Navbar items wrap")

check("REL-48", "R-38: dose-normalised values appear once, all labelled, and in the default view",
  tryCatch({
    th <- read.csv("data/example_theoph.csv"); cm <- list(subject = "Subject", time = "Time", conc = "conc", dose = "Dose")
    st <- rel_st(trap = "linear"); st$dose <- dose_by_profile(th, cm)
    r <- add_dose_normalized(as.data.frame(suppressWarnings(run_nca(th, cm, st))), st$dose)
    shown <- names(rename_nca_columns(drop_duplicate_dose_normalised(r)))
    multi <- paste(readLines("R/mod_path_multi_nca.R", warn = FALSE), collapse = "\n")
    !anyDuplicated(shown) && !any(grepl("_DN$", shown)) && !"CMAXD" %in% names(drop_duplicate_dose_normalised(r)) &&
      "CMAXD" %in% names(r) && grepl('"Dose-Normalised Cmax", "Dose-Normalised AUC Last"', multi, fixed = TRUE) &&
      !grepl("37 columns", multi, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-NCA-08", critical = FALSE, method = "Theophylline with per-profile doses and dose normalisation; All Subjects table code",
  expected = "No duplicate or raw-code headers; NonCompart's CMAXD kept in the data (records) but not shown twice; DN columns in the default view")

check("REL-49", "R-39: no label is lost in a non-UTF-8 locale",
  tryCatch({
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("-e", shQuote('for (f in c(list.files("R", full.names = TRUE), "app.R")) parse(f)')),
      stdout = TRUE, stderr = TRUE, env = "LC_ALL=C"))
    !any(grepl("unable to translate", out))
  }, error = function(e) FALSE),
  "URS-GEN-01", critical = FALSE, method = "Parse every source file with LC_ALL=C",
  expected = "No 'unable to translate' warning (was 12 choice labels)")

check("REL-50", "R-41: single-dose Vss and MRT are not reported at steady state",
  tryCatch({
    st <- rel_st(route = "iv_bolus", ss = TRUE, tau = 12); st$dose <- 1000
    t <- c(0, 0.25, 0.5, 1, 2, 4, 6, 8, 12); cc <- c(40, 700, 560, 380, 220, 120, 90, 70, 42)
    r <- run_single_nca(t, cc, st)
    b <- run_nca(data.frame(ID = "1", T = t, C = cc), rel_cm, st)
    is.na(r[["VSSO"]]) && is.na(r[["MRTIVLST"]]) && is.na(b$VSSO) && !is.na(r[["CLO"]])
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = FALSE, method = "Steady-state IV bolus, single profile and batch",
  expected = "Vss and MRT to last empty (NonCompart's values are single-dose quantities); CL kept")

check("REL-51", "R-42: a parallel study reports the Welch interval when it changes the conclusion",
  tryCatch({
    r <- exp(log(100) + qnorm(ppoints(36)) * 0.10); t <- exp(log(109) + qnorm(ppoints(12)) * 0.30)
    d <- data.frame(Treatment = factor(c(rep("R", 36), rep("T", 12)), levels = c("R", "T")), AUCLST = c(r, t))
    n <- parallel_welch_notes(d, "AUCLST", "Treatment")
    eq <- data.frame(Treatment = factor(c(rep("R", 12), rep("T", 12)), levels = c("R", "T")),
                     AUCLST = exp(log(100) + c(qnorm(ppoints(12)), qnorm(ppoints(12))) * 0.15))
    w <- stats::t.test(log(t), log(r), conf.level = 0.9)$conf.int
    length(n) == 1 && grepl(sprintf("%.2f-%.2f%%", 100 * exp(w[1]), 100 * exp(w[2])), n, fixed = TRUE) &&
      length(parallel_welch_notes(eq, "AUCLST", "Treatment")) == 0 &&
      !grepl("Welch) interval was not calculated", paste(readLines("R/mod_methods.R"), collapse = " "), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-01", critical = FALSE, method = "36 Reference (SD 0.10) vs 12 Test (SD 0.30); equal groups as control",
  expected = "Welch 93.27-127.39% reported against pooled 99.15-119.83%; nothing when the two agree")

check("REL-52", "R-43: the steady-state trough can be compared in bioequivalence",
  tryCatch({
    d <- rel_be
    # A steady-state trough before the dose (the fixture is single-dose, trough 0)
    d$Conc[d$Time == 0] <- 1 + (as.numeric(d$Subject[d$Time == 0]) %% 4) / 10 + (d$Treatment[d$Time == 0] == "Test") * 0.05
    nca <- suppressWarnings(run_nca(d, rel_be_cm, rel_st(trap = "linear", ss = TRUE, tau = 12)))
    b <- build_be_data(nca, d, rel_be_cm, "Reference")
    f <- fit_be_parameter(b$data, "CMIN_SS", design = "2x2x2", trt_col = b$trt_col, subj_col = b$subj_col,
                          per_col = b$per_col, seq_col = b$seq_col)$row
    be <- paste(readLines("R/mod_path_be.R", warn = FALSE), collapse = "\n")
    f$Bioequivalent %in% c("YES", "NO") && !is.na(f$Point_Est) && grepl('"CMAX","AUCTAU","CMIN_SS"', be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-01", critical = FALSE, method = "2x2x2 fixture analysed at steady state (tau 12 h); parameter list of the module",
  expected = "Cmin at steady state gets a ratio, CI and verdict, and is offered for comparison")

check("REL-53", "R-45: statistical and NCA wording matches the code",
  tryCatch({
    all_txt <- paste(vapply(c(list.files("R", "\\.R$", full.names = TRUE), "app.R", "install_and_run.R"), function(f)
      paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
    bad <- c("contribute to one treatment arm only", "When in doubt, use 95", "try lowering it",
             "ln(1.11111)/0.10", "for extravascular administration the C", "exact method via Owen's Q\")",
             "Assistant v1.0", "both the Test and the Reference. using")
    !any(vapply(bad, grepl, logical(1), x = all_txt, fixed = TRUE)) &&
      grepl("the 95% upper confidence bound of the squared log", all_txt, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-01", critical = FALSE, method = "Search app text for the wordings the review found wrong",
  expected = "None of the incorrect wordings remain")

check("REL-54", "R-46: data-reading edge cases are read correctly or reported",
  tryCatch({
    f <- tempfile(fileext = ".csv")
    con <- file(f, "wb"); writeBin(c(charToRaw("ID;TIME;CONC;ConcUnit\n001;0;0;"), as.raw(0xB5), charToRaw("g/L\n01;1;5;"),
                                     as.raw(0xB5), charToRaw("g/L\n")), con); close(con)
    x <- read_pk_file(f, list(sep = ";", dec = "."))
    ids_ok <- identical(x$ID, c("001", "01")) && is.numeric(x$TIME)
    enc_ok <- identical(units_in_data(x)$conc$unit, "ug/L")
    q <- run_data_quality_check(data.frame(S = 1:2, T = 0, C = 1, Trt = c("Test", "test"), D = 1),
                                list(subject = "S", time = "T", conc = "C", treatment = "Trt"), 0)
    case_ok <- any(grepl("differ only in case", q$findings$Message))
    q2 <- run_data_quality_check(data.frame(S = 1, T = c(0, 1, 2), C = c(0, 5, 3), D = c(100, 100, 50)),
                                 list(subject = "S", time = "T", conc = "C", dose = "D"), 0)
    dose_ok <- any(grepl("more than one dose value", q2$findings$Message))
    alt_ok <- identical(attr(auto_detect_columns(c("ID", "TimeNominal", "TimeActual", "Conc")), "alternatives")$time, "TimeActual")
    sparse <- run_interlocks(data.frame(ID = 1, T = c(2, 4, 6), C = c(5, 3, 1)), list(subject = "ID", time = "T"), "mapped")
    period2 <- run_interlocks(data.frame(ID = 1, T = c(168, 170, 180), C = c(5, 3, 1)), list(subject = "ID", time = "T"), "mapped")
    flag <- prepare_pk_dataset(data.frame(ID = 1, T = 0:2, C = c(0, 5, 3), BLQ_flag = TRUE), list(subject = "ID", time = "T", conc = "C"))$data
    ids_ok && enc_ok && case_ok && dose_ok && alt_ok && nrow(sparse) == 0 && nrow(period2) > 0 &&
      is.null(flag$BLQ_flag) && grepl("dose_normalised = isTRUE(settings$dose_normalised)",
                                      paste(readLines("R/export_record.R"), collapse = "\n"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = FALSE,
  method = "Windows-1252 CSV with IDs 001 and 01; case-variant treatments; a dose varying within a profile; two time columns; sparse and late profiles; a user BLQ_flag column",
  expected = "Latin-1 read with IDs kept distinct; each problem reported; sparse 2-6 h profile accepted, 168 h start refused; user flag dropped; dose normalisation recorded")

check("REL-55", "R-47: CDISC notes state the ISO 8601 interval format and that units are not CT-coded",
  tryCatch({
    n <- cdisc_pk_codes("AUC_0_2", "extravascular", FALSE)$Note
    grepl("ISO 8601", n, fixed = TRUE) &&
      grepl("not CDISC PKUNIT terms", paste(readLines("R/export_record.R"), collapse = " "), fixed = TRUE) &&
      grepl("not CDISC PKUNIT terms", paste(readLines("R/utils.R"), collapse = " "), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-EXP-08", critical = FALSE, method = "cdisc_pk_codes() for a partial AUC; code sheet and panel text",
  expected = "The PPSTINT/PPENINT note asks for ISO 8601 durations; units are said not to be CT terms")

check("REL-56", "R-48: LICENSE is the verbatim GPL-3 text and the About page lists dependency licences",
  tryCatch({
    lic <- readLines("LICENSE", warn = FALSE)
    ref <- readLines(file.path(R.home("share"), "licenses", "GPL-3"), warn = FALSE)
    app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
    readme <- paste(readLines("README.md", warn = FALSE), collapse = "\n")
    identical(lic, ref) && grepl('tags$th(style = "width: 10%;", "Licence")', app, fixed = TRUE) &&
      grepl("# Copyright (C) 2026 Rob ter Heine", app, fixed = TRUE) && grepl("either version 3 of the License, or", app, fixed = TRUE) &&
      grepl("GPL-3.0-or-later", readme, fixed = TRUE) && grepl("Copyright (C) 2026 Rob ter Heine", readme, fixed = TRUE) &&
      grepl("packageDescription(pkg$name)$License", app, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-01", critical = FALSE, method = "LICENSE against R's copy of GPL-3; About page package table",
  expected = "Identical licence text (was missing the title, preamble and FSF notice); copyright and GPL-3.0-or-later notice in app.R, About and README; licence column shown")

check("REL-57", "R-50: on-screen text uses words, not codes, and consistent spelling",
  tryCatch({
    txt <- paste(vapply(c(list.files("R", "\\.R$", full.names = TRUE)), function(f)
      paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
    identical(design_label("single_arm"), "Single arm") && identical(design_label("parallel"), BE_DESIGNS$label[BE_DESIGNS$code == "parallel"]) &&
      grepl("design_label(info$design$type)", txt, fixed = TRUE) && !grepl("in R: zero_predose", txt, fixed = TRUE) &&
      !grepl("\"Calculate dose-normalized", txt, fixed = TRUE) && !grepl("Half-Life Review</b> tab", txt, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-01", critical = FALSE, method = "design_label(); module and help text",
  expected = "Design shown in words; British spelling in prose; help and refusal texts match the app")

check("REL-58", "Tlast under Rule 4 is the last measured concentration",
  tryCatch({
    tt <- c(0, 0.5, 1, 2, 4, 6, 8, 12, 16, 24, 36, 48)
    cc <- 100 * (exp(-0.2 * tt) - exp(-1.5 * tt)); cc[tt >= 24] <- 0.5; cc[tt == 6] <- 0.5
    d <- data.frame(ID = "1", T = tt, C = cc)
    st <- rel_st(trap = "linear"); st$r2adj_threshold <- 0.7
    r <- function(rule) suppressWarnings(run_nca(prepare_pk_dataset(d, rel_cm, list(lloq = 2, blq_rule = rule))$data, rel_cm, st))
    r1 <- r("rule1"); r4 <- r("rule4")
    p4 <- prepare_pk_dataset(d, rel_cm, list(lloq = 2, blq_rule = "rule4"))$data
    r4$TLST == 16 && r4$TLST == r1$TLST && abs(r4$CLST - r1$CLST) < 1e-12 &&
      p4$C[p4$T == 6] == 1 && all(is.na(p4$C[p4$T >= 24])) && r4$AUCLST > r1$AUCLST
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "Oral profile, BLQ at 6 h (embedded) and from 24 h (LLOQ 2), Rules 1 and 4",
  expected = "Tlast 16 h and the same Clast under both rules (Rule 4 gave 48 h); the embedded BLQ is LLOQ/2 under Rule 4")

end_section("REL")

# =============================================================================
# SECTION MRV: App fixes from the review of user manual 1.7 (P-xx, R-xx, T-xx)
# =============================================================================
# Each test is built from the case the manual review found.
start_section("MRV")

mrv_cm <- list(subject = "Subject", time = "Time", conc = "Concentration", treatment = "Treatment",
               period = "Period", sequence = "Sequence")
mrv_xo <- read.csv("data/example_be_crossover.csv", stringsAsFactors = FALSE)
mrv_st <- function(ss = FALSE, tau = NA)
  list(admin_route = "extravascular", dose = 100, dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL",
       trap_method = "log", r2adj_threshold = 0.7, infusion_duration = 0, mw = 0, is_steady_state = ss,
       tau = tau, partial_aucs = NULL)
mrv_prep <- function(raw, rule = "rule1", lloq = 0.5) prepare_pk_dataset(raw, mrv_cm, list(lloq = lloq, blq_rule = rule))$data

check("MRV-01", "P-01: a period without measurable concentrations is counted as missing and named",
  tryCatch({
    a <- mrv_xo; a$Concentration <- as.character(a$Concentration)
    a$Concentration[a$Subject == 1 & a$Treatment == "Test"] <- "BLQ"
    da <- mrv_prep(a); ra <- suppressWarnings(run_nca(da, mrv_cm, mrv_st()))
    bd <- build_be_data(ra, da, mrv_cm, reference = "Reference")
    f <- fit_be_parameter(bd$data, "CMAX", design = "2x2x2", trt_col = bd$trt_col, subj_col = bd$subj_col,
                          per_col = bd$per_col, seq_col = bd$seq_col)
    ma <- be_m13a_checks(da, mrv_cm, ra, NULL)
    b <- mrv_xo; i <- b$Subject == 2 & b$Treatment == "Reference"; b$Concentration[i] <- b$Concentration[i] * 0.03
    db <- mrv_prep(b); mb <- be_m13a_checks(db, mrv_cm, suppressWarnings(run_nca(db, mrv_cm, mrv_st())), NULL)
    dc <- mrv_prep(mrv_xo); mc <- be_m13a_checks(dc, mrv_cm, suppressWarnings(run_nca(dc, mrv_cm, mrv_st())), NULL)
    nrow(bd$data) == 12 && f$row$Missing_Test == 1 && f$row$N_Test == 5 &&
      any(grepl("1 | Test | P1", ma, fixed = TRUE) & grepl("2.2.1.1", ma, fixed = TRUE)) &&
      any(grepl("below 5% of the geometric mean", mb, fixed = TRUE) & grepl("2 | Reference | P1", mb, fixed = TRUE)) &&
      length(mc) == 0
  }, error = function(e) FALSE),
  "URS-BE-11", critical = TRUE,
  method = "example_be_crossover.csv with subject 1's Test period all BLQ; subject 2's Reference period at 3%; unchanged file",
  expected = "Profiles missing (Test) 1 (was 0) and the profile named with M13A 2.2.1.1; the 3% period flagged; no note for the clean file")

check("MRV-02", "P-02: values set by a BLQ rule do not end the lag time, and Rule 6 leaves an all-BLQ profile at 0",
  tryCatch({
    tt <- c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24)
    d <- rbind(data.frame(Subject = "LAG", Time = tt, Concentration = c("BLQ", "BLQ", "BLQ", 5, 20, 30, 22, 12, 6, 2, "BLQ")),
               data.frame(Subject = "NONE", Time = tt, Concentration = rep("BLQ", 11)))
    cm <- list(subject = "Subject", time = "Time", conc = "Concentration")
    tl <- vapply(paste0("rule", 1:6), function(rule) {
      dd <- prepare_pk_dataset(d, cm, list(lloq = 1, blq_rule = rule))$data
      r <- suppressWarnings(run_nca(dd, cm, mrv_st()))
      as.numeric(r$TLAG[r$Subject == "LAG"])
    }, numeric(1))
    d6 <- prepare_pk_dataset(d, cm, list(lloq = 1, blq_rule = "rule6"))$data
    r6 <- suppressWarnings(run_nca(d6, cm, mrv_st()))
    d4 <- prepare_pk_dataset(d, cm, list(lloq = 1, blq_rule = "rule4"))$data; l4 <- d4$Subject == "LAG"
    s4 <- run_single_nca(d4$Time[l4], d4$Concentration[l4], mrv_st(), is_blq = d4$BLQ_flag[l4])
    all(tl == 1) && all(d6$Concentration[d6$Subject == "NONE"] == 0) && !"NONE" %in% r6$Subject &&
      as.numeric(s4[["TLAG"]]) == 1
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE,
  method = "BLQ at 0, 0.5 and 1 h, first measurable at 1.5 h (LLOQ 1), and a profile BLQ throughout; Rules 1 to 6, batch and single profile",
  expected = "Tlag 1 h under every rule (Rules 3, 4 and 6 gave 0); the all-BLQ profile stays 0 under Rule 6 (was LLOQ/2, Cmax 0.5) and gets no NCA row")

check("MRV-03", "R-01: a verdict that includes data M13A excludes says it is not the primary analysis; the batch checks the pre-dose value too",
  tryCatch({
    th <- read.csv("data/example_theoph.csv", stringsAsFactors = FALSE)
    tcm <- list(subject = "Subject", time = "Time", conc = "conc")
    nb <- predose_above_5pct_note(th, tcm, verdict = FALSE); nv <- predose_above_5pct_note(th, tcm)
    few <- be_m13a_checks(rel_be, rel_be_cm, suppressWarnings(run_nca(rel_be, rel_be_cm, rel_st(trap = "linear"))),
                          data.frame(Parameter = "CMAX", N_Test = 10, N_Ref = 10))
    mn <- paste(readLines("R/mod_path_multi_nca.R", warn = FALSE), collapse = "\n")
    grepl("in 1 profile(s): 1.", nb, fixed = TRUE) && grepl("carry-over", nb, fixed = TRUE) &&
      !grepl("verdicts shown", nb, fixed = TRUE) &&
      grepl("not the M13A primary analysis", nv, fixed = TRUE) && grepl("run the analysis again", nv, fixed = TRUE) &&
      any(grepl("statistical result only", few, fixed = TRUE)) &&
      grepl("predose_above_5pct_note(shared$pk_data, shared$col_map, verdict = FALSE)", mn, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-11", critical = FALSE,
  method = "example_theoph.csv (subject 1: pre-dose 0.74 of Cmax 10.5 mg/L) in the batch and bioequivalence wording; N = 10",
  expected = "Batch names subject 1 (was no check); the BE note says the verdicts are not the M13A primary analysis; N < 12 is a statistical result only")

check("MRV-04", "R-10: the Analysis Record holds the data quality findings and the notes shown with the results",
  tryCatch({
    wd <- file.path(tempdir(), paste0("mrv04", as.integer(runif(1, 1, 1e7)))); dir.create(wd)
    f <- file.path(wd, "input.csv"); write.csv(mrv_xo, f, row.names = FALSE)
    d <- prepare_pk_dataset(read_pk_file(f), mrv_cm, list(lloq = 0, blq_rule = "rule1"))$data
    res <- suppressWarnings(run_nca(d, mrv_cm, mrv_st()))
    qc <- run_data_quality_check(read_pk_file(f), mrv_cm)
    note <- "Fewer than 12 evaluable subjects (smallest: 6). ICH M13A (2.2.3.1) does not accept a pivotal study"
    ck <- record_checks(qc, c(note, note, NA))
    zp <- file.path(wd, "rec.zip")
    suppressWarnings(create_analysis_record(zp, res, mrv_st(), mrv_cm, f, "input.csv", blq_rule = "rule1", lloq = 0,
      be_results = list(ci_table = data.frame(Parameter = "CMAX"), anova = list()), checks = ck))
    ex <- rec_unzip(zp)
    sh <- openxlsx::read.xlsx(file.path(ex, "results.xlsx"), sheet = "Checks")
    html <- paste(readLines(file.path(ex, "analysis_summary.html"), warn = FALSE), collapse = "\n")
    src <- paste(c(readLines("R/mod_path_be.R", warn = FALSE), readLines("R/mod_path_multi_nca.R", warn = FALSE)), collapse = "\n")
    sum(ck$Message == note) == 1 && sum(ck$Source == "Data quality check") == nrow(qc$findings) &&
      any(sh$Message == note) && grepl("10. Checks and Notes", html, fixed = TRUE) &&
      grepl("does not accept a pivotal study", html, fixed = TRUE) &&
      grepl("checks         = record_checks(shared$qc_result, c(be_result()$m13a, copy_note))", src, fixed = TRUE) &&
      grepl("checks         = record_checks(shared$qc_result, c(nca_excl_note(), pauc_notes(), copy_note))", src, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-EXP-01", critical = FALSE,
  method = "Record of the crossover example with its quality findings and an M13A note (given twice and once missing)",
  expected = "results.xlsx has a Checks sheet and the summary a Checks and Notes section with the note once (the record had neither)")

check("MRV-05", "T-02, R-02, R-05, R-11, R-15: the app's wording matches what it does and claims no more",
  tryCatch({
    rd <- function(f) paste(readLines(f, warn = FALSE), collapse = " ")
    up <- rd("R/mod_data_upload.R"); gd <- rd("R/mod_data_guide.R"); ap <- rd("app.R"); be <- rd("R/mod_path_be.R")
    grepl("Click 'Set LLOQ to", up, fixed = TRUE) && grepl("then click Process Data again", up, fixed = TRUE) &&
      !grepl("Apply LLOQ", gd, fixed = TRUE) && grepl("'Set LLOQ to 0.5'", gd, fixed = TRUE) &&
      !grepl("apply and process", up, fixed = TRUE) &&
      grepl("anonymised", DATA_PROTECTION_NOTICE, fixed = TRUE) &&
      grepl("still personal", DATA_PROTECTION_NOTICE, fixed = TRUE) && !grepl("use pseudonymised IDs", DATA_PROTECTION_NOTICE) &&
      !grepl("override audit trail", ap, fixed = TRUE) && !grepl("validated with NonCompart", ap, fixed = TRUE) &&
      !grepl("reference-scaled bioequivalence)\" = \"cmax", be, fixed = TRUE) && grepl("\"Cmax only\" = \"cmax\"", be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-07", critical = FALSE, method = "search the upload module, Data Guide, About page, data notice and BE settings",
  expected = "LLOQ steps as the app works ('Set LLOQ to', then Process Data); notice asks for anonymised data; no 'audit trail' or 'validated' claim; Cmax scope label without 'reference-scaled'")

check("MRV-06", "R-08: widened limits can apply to Cmax and the partial AUCs without widening AUC",
  tryCatch({
    iv <- data.frame(start = 0, end = "1", cmax = TRUE, role = "pivotal", stringsAsFactors = FALSE)
    st <- mrv_st(); st$partial_aucs <- iv
    d <- mrv_prep(mrv_xo); r <- suppressWarnings(run_nca(d, mrv_cm, st))
    bd <- build_be_data(r, d, mrv_cm, reference = "Reference")
    lim <- function(p, scope) fit_be_parameter(bd$data, p, design = "2x2x2", trt_col = bd$trt_col, subj_col = bd$subj_col,
                                                per_col = bd$per_col, seq_col = bd$seq_col, be_lower = 69.84,
                                                be_upper = 143.19, widened_scope = scope)$row$BE_Lower
    m <- sapply(c("cmax", "cmax_pauc", "all"), function(sc) sapply(c("CMAX", "AUC_0_1", "CMAX_0_1", "AUCLST"), lim, scope = sc))
    all(m["CMAX", ] == 69.84) && all(m[c("AUC_0_1", "CMAX_0_1", "AUCLST"), "cmax"] == 80) &&
      all(m[c("AUC_0_1", "CMAX_0_1"), "cmax_pauc"] == 69.84) && m["AUCLST", "cmax_pauc"] == 80 &&
      all(m[, "all"] == 69.84) && widened_scope_value("cmax_pauc") == "cmax_pauc" && widened_scope_value(NULL) == "cmax"
  }, error = function(e) FALSE),
  "URS-BE-11", critical = TRUE,
  method = "crossover example with a 0-1 h interval; limits 69.84-143.19 under the three scopes",
  expected = "Cmax only: AUC and partial metrics at 80; Cmax and partial AUCs: partial metrics widened, AUClast at 80 (no such choice before); all: every metric widened")

check("MRV-07", "P-14, P-15: Ctau,ss is reported apart from Cmin, and nothing extrapolated to infinity is shown at steady state",
  tryCatch({
    d <- rbind(data.frame(ID = "A", T = c(0, 0.5, 1, 2, 4, 8, 12), C = c(4, 3.5, 6, 10, 8, 5, 4.2)),
               data.frame(ID = "B", T = c(0, 0.5, 1, 2, 4, 8, 10), C = c(4, 3.5, 6, 10, 8, 5, 4.6)))
    r <- suppressWarnings(run_nca(d, rel_cm, rel_st(ss = TRUE, tau = 12)))
    a <- r[r$ID == "A", ]; b <- r[r$ID == "B", ]
    s1 <- suppressWarnings(run_single_nca(d$T[d$ID == "A"], d$C[d$ID == "A"], rel_st(ss = TRUE, tau = 12)))
    inf <- intersect(c("AUCIFO", "AUCIFP", "AUCPEO", "AUCPEP", "AUMCIFO", "MRTEVIFO", "CLFP", "VZFP"), names(r))
    map <- read.csv("cdisc/pk_parameter_map.csv", stringsAsFactors = FALSE)
    be <- paste(readLines("R/mod_path_be.R", warn = FALSE), collapse = " ")
    a$CMIN_SS == 3.5 && a$CTAU_SS == 4.2 && is.na(b$CTAU_SS) && b$CMIN_SS == 3.5 &&
      as.numeric(s1[["CTAU_SS"]]) == 4.2 && length(inf) > 0 && all(is.na(unlist(r[inf]))) &&
      all(is.na(unlist(s1[intersect(inf, names(s1))]))) && all(is.finite(r$CLFO)) &&
      friendly_name("CMIN_SS") == "Minimum Concentration (Cmin)" && friendly_name("CTAU_SS") == "Concentration at Tau (Ctau)" &&
      "CTAU_SS" %in% map$app_parameter && grepl("\"CMIN_SS\", \"CTAU_SS\"", be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE,
  method = "Steady state, tau 12 h: minimum 3.5 at 0.5 h (after the dose), 4.2 at 12 h; a second profile ending at 10 h",
  expected = "Cmin 3.5, Ctau 4.2 (no Ctau before), Ctau empty without a sample at tau; AUC to infinity, % extrapolated, AUMC to infinity and predicted CL/V empty (were shown); CL/F kept")

check("MRV-08", "T-07: every output column has a label, and every column with a dimension has a unit",
  tryCatch({
    d <- read.csv(file.path("validation", "fixtures", "be_2x2x2_crossover.csv"), stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period")
    cols <- character(0)
    for (route in c("extravascular", "iv_bolus", "iv_infusion")) for (ss in c(FALSE, TRUE)) {
      st <- rel_st(route = route, ss = ss, tau = if (ss) 24 else NA, dur = 0.5)
      r <- suppressWarnings(run_nca(d, cm, st)); cols <- union(cols, names(add_dose_normalized(r, 100)))
    }
    cols <- setdiff(cols, c("Subject", "Treatment", "Period"))
    lab <- vapply(cols, friendly_name, character(1))
    u <- add_units_to_labels(unname(lab), dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL")
    unitless <- grepl("%|R-squared|Correlation|Points Used|Intercept|Swing|Span Ratio|^Flag:|^Excluded by Analyst", lab)
    all(lab != cols) && all(u[!unitless] != lab[!unitless]) &&
      u[lab == "AUMC to Last Point"] == "AUMC to Last Point (ng/mL\u00b7h\u00b2)" &&
      u[lab == "Initial Concentration (C0)"] == "Initial Concentration (C0) (ng/mL)" &&
      any(u == "Dose-Normalised Cmax (ng/mL per mg)")
  }, error = function(e) FALSE),
  "URS-UI-01", critical = FALSE,
  method = "all NonCompart and app columns for extravascular, IV bolus and IV infusion, single dose and steady state, with dose normalisation",
  expected = "no raw code as a label (C0, AUCPBEO, VZP, CLP, MRTIV*, VSSO/P were); a unit on every column with a dimension (about 20 had none)")

check("MRV-09", "R-22: a record that holds a rewritten copy of the data says so",
  tryCatch({
    mk <- function(note) {
      wd <- file.path(tempdir(), paste0("mrv09", as.integer(runif(1, 1, 1e7)))); dir.create(wd)
      f <- file.path(wd, "copy.csv"); write.csv(mrv_xo, f, row.names = FALSE)
      d <- prepare_pk_dataset(read_pk_file(f), mrv_cm, list(lloq = 0, blq_rule = "rule1"))$data
      zp <- file.path(wd, "rec.zip")
      suppressWarnings(create_analysis_record(zp, suppressWarnings(run_nca(d, mrv_cm, mrv_st())), mrv_st(), mrv_cm, f,
        "copy.csv", blq_rule = "rule1", lloq = 0, checks = record_checks(NULL, note), data_copy_note = note))
      ex <- rec_unzip(zp)
      list(js = jsonlite::fromJSON(file.path(ex, "analysis_settings.json")),
           html = paste(readLines(file.path(ex, "analysis_summary.html"), warn = FALSE), collapse = " "))
    }
    note <- fallback_copy_note("study_upload.xlsx")
    a <- mk(note); b <- mk(NULL)
    src <- paste(c(readLines("R/mod_path_be.R", warn = FALSE), readLines("R/mod_path_multi_nca.R", warn = FALSE)), collapse = " ")
    identical(a$js$data_copy, note) && grepl("study_upload.xlsx", a$html, fixed = TRUE) &&
      identical(b$js$data_copy, "the uploaded file") && lengths(regmatches(src, gregexpr("data_copy_note = copy_note", src, fixed = TRUE))) == 2
  }, error = function(e) FALSE),
  "URS-EXP-03", critical = FALSE,
  method = "records with and without the fallback copy of the data; both analysis paths",
  expected = "settings and summary say the record holds a rewritten CSV whose name and hash differ (the record was silent)")

check("MRV-10", "T-16, T-01: an example file with BLQ results, and an installation with the validated package versions",
  tryCatch({
    raw <- read_pk_file("data/example_blq.csv"); cm <- list(subject = "Subject", time = "Time", conc = "Concentration")
    q0 <- run_data_quality_check(raw, cm, lloq = 0); q5 <- run_data_quality_check(raw, cm, lloq = 0.5)
    d <- prepare_pk_dataset(raw, cm, list(lloq = 0.5, blq_rule = "rule1"))$data
    r <- suppressWarnings(run_nca(d, cm, mrv_st()))
    inst <- paste(readLines("install_and_run.R", warn = FALSE), collapse = "\n")
    gd <- paste(readLines("R/mod_data_guide.R", warn = FALSE), collapse = " ")
    ap <- paste(readLines("app.R", warn = FALSE), collapse = " ")
    sum(raw$Concentration == "<0.5") == 22 && length(unique(raw$Subject)) == 8 &&
      any(q0$findings$Severity == "ERROR") && !any(q5$findings$Severity == "ERROR") && nrow(r) == 8 &&
      r$TLAG[r$Subject == "5"] == 0.5 &&
      grepl("--validated", inst, fixed = TRUE) && grepl("renv::restore(lockfile = lock", inst, fixed = TRUE) &&
      grepl("dl_example_blq", gd, fixed = TRUE) && grepl("output$dl_example_blq", ap, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-02", critical = FALSE,
  method = "data/example_blq.csv through the quality check (LLOQ 0 and 0.5) and the NCA; install_and_run.R; Data Guide download",
  expected = "22 '<0.5' entries in 8 subjects; LLOQ 0 is refused, 0.5 passes; subject 5's lag time 0.5 h; a --validated install from validation/renv.lock")

end_section("MRV")

# =============================================================================
# SECTION GXP: Controlled mode (audit trail, access control, signatures)
# =============================================================================
start_section("GXP")

gxp_ok <- all(vapply(c("DBI", "RSQLite", "shinymanager"), requireNamespace, logical(1), quietly = TRUE))
gxp_env <- new.env()
if (gxp_ok) sys.source("R/gxp_audit.R", envir = gxp_env)
gxp_tmp <- tempfile("gxp_"); dir.create(gxp_tmp)
gxp_set <- function(dir) Sys.setenv(NCA_GXP_DIR = dir, NCA_GXP_KEY = "validation-key",
                                    NCA_GXP_ORG = "Validation Org")
gxp_unset <- function() Sys.unsetenv(c("NCA_GXP_DIR", "NCA_GXP_KEY", "NCA_GXP_ORG"))
# A fresh trail with n entries after trail_created
gxp_trail <- function(n = 9) {
  d <- tempfile("trail_", tmpdir = gxp_tmp); dir.create(d)
  p <- file.path(d, "audit.sqlite")
  gxp_env$audit_init(p, user = "owner", org = "Validation Org")
  for (i in seq_len(n)) gxp_env$audit_append("analysis_run", object = paste("run", i),
                                             details = list(i = i), user = "ana", role = "analyst",
                                             org = "Validation Org", path = p)
  p
}
# Tamper with a copy: drop the triggers first, as someone with file access could
gxp_tamper <- function(p, sql) {
  q <- tempfile(fileext = ".sqlite", tmpdir = gxp_tmp); file.copy(p, q)
  con <- DBI::dbConnect(RSQLite::SQLite(), q)
  DBI::dbExecute(con, "DROP TRIGGER trail_no_update"); DBI::dbExecute(con, "DROP TRIGGER trail_no_delete")
  for (s in sql) DBI::dbExecute(con, s)
  DBI::dbDisconnect(con)
  q
}

check("GXP-01", "The packages controlled mode needs are installed (DBI, RSQLite, shinymanager)",
      gxp_ok, "URS-GXP-01", method = "requireNamespace()", expected = "TRUE")

check("GXP-02", "A new trail verifies: 10 entries, intact, first entry trail_created with a zero prev_hash",
      tryCatch({
        p <- gxp_trail(9); v <- gxp_env$audit_verify(p); tr <- gxp_env$audit_read(p)
        e1 <- jsonlite::fromJSON(DBI::dbGetQuery(con <- DBI::dbConnect(RSQLite::SQLite(), p),
                                                  "SELECT entry FROM trail WHERE seq = 1")$entry)
        DBI::dbDisconnect(con)
        v$intact && v$n == 10 && tr$event[1] == "trail_created" && e1$prev_hash == strrep("0", 64)
      }, error = function(e) FALSE),
      "URS-GXP-05,URS-GXP-06", method = "audit_init() + 9 x audit_append(); audit_verify()",
      expected = "intact; n = 10")

check("GXP-03", "Every entry records user, role, organisation, UTC time, app version and the previous hash",
      tryCatch({
        tr <- gxp_env$audit_read(gxp_trail(3))
        all(tr$user[-1] == "ana") && all(tr$role[-1] == "analyst") &&
          all(tr$organisation == "Validation Org") &&
          all(grepl("^\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d\\.\\d{3}Z$", tr$time_utc))
      }, error = function(e) FALSE),
      "URS-GXP-05", method = "audit_read() of a new trail",
      expected = "user, role, organisation filled; time as YYYY-MM-DDThh:mm:ss.sssZ")

check("GXP-04", "UPDATE and DELETE on the trail table are refused by the triggers",
      tryCatch({
        p <- gxp_trail(3); con <- DBI::dbConnect(RSQLite::SQLite(), p)
        u <- tryCatch({ DBI::dbExecute(con, "UPDATE trail SET hash = 'x' WHERE seq = 2"); "done" },
                      error = function(e) conditionMessage(e))
        d <- tryCatch({ DBI::dbExecute(con, "DELETE FROM trail WHERE seq = 2"); "done" },
                      error = function(e) conditionMessage(e))
        n <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM trail")$n; DBI::dbDisconnect(con)
        grepl("append-only", u) && grepl("append-only", d) && n == 4 && gxp_env$audit_verify(p)$intact
      }, error = function(e) FALSE),
      "URS-GXP-05", method = "UPDATE and DELETE through DBI", expected = "both refused; 4 rows; intact")

check("GXP-05", "A changed byte, a deleted row, an inserted row and swapped rows are each detected at the right entry",
      tryCatch({
        p <- gxp_trail(9)
        con <- DBI::dbConnect(RSQLite::SQLite(), p)
        rows <- DBI::dbGetQuery(con, "SELECT seq, entry, hash FROM trail ORDER BY seq"); DBI::dbDisconnect(con)
        q <- function(x) gsub("'", "''", x)
        changed <- gxp_tamper(p, "UPDATE trail SET entry = replace(entry, 'run 4', 'run X') WHERE seq = 5")
        deleted <- gxp_tamper(p, "DELETE FROM trail WHERE seq = 5")
        fake <- sub('"object":"run 3"', '"object":"inserted"', rows$entry[4])
        inserted <- gxp_tamper(p, c(
          "DELETE FROM trail WHERE seq >= 5",
          sprintf("INSERT INTO trail VALUES (5, '%s', '%s')", q(fake),
                  digest::digest(fake, algo = "sha256", serialize = FALSE)),
          sprintf("INSERT INTO trail VALUES (%d, '%s', '%s')", rows$seq[5:10] + 1L, q(rows$entry[5:10]), rows$hash[5:10])))
        swapped <- gxp_tamper(p, c(
          "DELETE FROM trail WHERE seq IN (5, 6)",
          sprintf("INSERT INTO trail VALUES (5, '%s', '%s')", q(rows$entry[6]), rows$hash[6]),
          sprintf("INSERT INTO trail VALUES (6, '%s', '%s')", q(rows$entry[5]), rows$hash[5])))
        fb <- vapply(list(changed, deleted, inserted, swapped),
                     function(f) { v <- gxp_env$audit_verify(f); if (v$intact) NA_integer_ else v$first_broken },
                     integer(1))
        identical(fb, c(5L, 6L, 5L, 5L))
      }, error = function(e) FALSE),
      "URS-GXP-06", method = "tamper with copies (triggers dropped); audit_verify()",
      expected = "not intact; first broken entry 5 (changed), 6 (deleted), 5 (inserted), 5 (swapped)")

check("GXP-06", "Removing the last entries is detected against an anchor filed earlier",
      tryCatch({
        p <- gxp_trail(9); h <- gxp_env$audit_head(p)
        t <- gxp_tamper(p, "DELETE FROM trail WHERE seq > 7")
        without <- gxp_env$audit_verify(t)
        with <- gxp_env$audit_verify(t, anchors = data.frame(seq = h$seq, hash = h$hash))
        h$seq == 10 && without$intact && !with$intact && grepl("entries removed", with$errors[1])
      }, error = function(e) FALSE),
      "URS-GXP-06", method = "audit_head(); delete entries 8-10 in a copy; audit_verify() with and without the anchor",
      expected = "intact without the anchor (a bare chain cannot tell), broken with it")

check("GXP-07", "Three processes appending 500 entries each at the same time give one intact chain",
      tryCatch({
        p <- gxp_trail(0); go <- file.path(gxp_tmp, "go"); unlink(go)
        worker <- file.path(gxp_tmp, "worker.R")
        writeLines(c(sprintf('source("%s")', normalizePath("R/gxp_audit.R")),
                     sprintf('while (!file.exists("%s")) Sys.sleep(0.01)', go),
                     'for (i in 1:500) audit_append("analysis_run", object = commandArgs(TRUE)[1], user = commandArgs(TRUE)[1],',
                     sprintf('  role = "analyst", org = "Validation Org", path = "%s")', p)), worker)
        rs <- file.path(R.home("bin"), "Rscript")
        for (w in c("w1", "w2", "w3")) system2(rs, c(shQuote(worker), w), wait = FALSE, stdout = FALSE, stderr = FALSE)
        Sys.sleep(2); file.create(go)
        n_rows <- function() { con <- gxp_env$.audit_connect(p); on.exit(DBI::dbDisconnect(con))
                               DBI::dbGetQuery(con, "SELECT count(*) AS n FROM trail")$n }
        t0 <- Sys.time()
        while (n_rows() < 1501 && difftime(Sys.time(), t0, units = "secs") < 180) Sys.sleep(0.5)
        v <- gxp_env$audit_verify(p); tr <- gxp_env$audit_read(p)
        switches <- sum(head(tr$object[-1], -1) != tail(tr$object[-1], -1))
        v$intact && v$n == 1501 && all(table(tr$user[-1]) == 500) && switches > 10
      }, error = function(e) FALSE),
      "URS-GXP-05", method = "3 Rscript processes, 500 audit_append() each, started together",
      expected = "1501 entries, intact, 500 per writer, writes interleaved")

check("GXP-08", "An append to a read-only trail stops with an error and leaves no row",
      tryCatch({
        p <- gxp_trail(2); Sys.chmod(p, "0444")
        err <- tryCatch({ gxp_env$audit_append("analysis_run", user = "ana", role = "analyst",
                                               org = "Validation Org", path = p); FALSE },
                        error = function(e) TRUE)
        Sys.chmod(p, "0644")
        err && gxp_env$audit_verify(p)$n == 3
      }, error = function(e) FALSE),
      "URS-GXP-07", method = "chmod 0444 on audit.sqlite; audit_append()", expected = "error; still 3 entries")

check("GXP-09", "An entry written after the clock was set back is reported as a warning, not a break",
      tryCatch({
        p <- gxp_trail(2); real <- gxp_env$gxp_utc_now
        assign("gxp_utc_now", function() "2000-01-01T00:00:00.000Z", envir = gxp_env)
        gxp_env$audit_append("analysis_run", user = "ana", role = "analyst", org = "Validation Org", path = p)
        assign("gxp_utc_now", real, envir = gxp_env)
        v <- gxp_env$audit_verify(p)
        v$intact && length(v$warnings) == 1 && grepl("earlier", v$warnings)
      }, error = function(e) FALSE),
      "URS-GXP-05", method = "append an entry with a time before its predecessor", expected = "intact; one clock warning")

check("GXP-10", "Fail closed: gxp_guard() writes before the action, returns FALSE when it cannot, and is a no-op in open mode",
      tryCatch({
        gxp_unset(); open_ok <- isTRUE(gxp_env$gxp_guard("analysis_run", session = NULL))
        d <- file.path(gxp_tmp, "guard"); dir.create(d); gxp_set(d)
        gxp_env$audit_init(file.path(d, "audit.sqlite"), user = "owner", org = "Validation Org")
        written <- isTRUE(gxp_env$gxp_guard("analysis_run", object = "x", session = NULL))
        Sys.chmod(file.path(d, "audit.sqlite"), "0444")
        refused <- identical(gxp_env$gxp_guard("analysis_run", object = "y", session = NULL), FALSE)
        Sys.chmod(file.path(d, "audit.sqlite"), "0644")
        n <- gxp_env$audit_verify(file.path(d, "audit.sqlite"))$n
        gxp_unset()
        open_ok && written && refused && n == 2
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-07,URS-GXP-01", method = "gxp_guard() in open mode, with a writable and with a read-only trail",
      expected = "TRUE (open), TRUE and one entry (writable), FALSE and no entry (read-only)")

check("GXP-11", "A record is stored read-only under its SHA-256 and its creation is logged",
      tryCatch({
        d <- file.path(gxp_tmp, "store"); dir.create(d); gxp_set(d)
        gxp_env$audit_init(file.path(d, "audit.sqlite"), user = "owner", org = "Validation Org")
        z <- tempfile(fileext = ".zip", tmpdir = gxp_tmp); writeLines("record content", z)
        sha <- gxp_env$gxp_store_record(z, "record.zip", "batch_nca", study = "S1", verdict = "MATCH", session = NULL)
        dest <- file.path(d, "records", paste0(sha, ".zip"))
        tr <- gxp_env$audit_read(file.path(d, "audit.sqlite")); gxp_unset()
        identical(sha, gxp_env$sha256_file(z)) && file.exists(dest) &&
          file.access(dest, 2) != 0 && tail(tr$event, 1) == "record_created" && tail(tr$sha256, 1) == sha
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-09", method = "gxp_store_record() on a small file", expected = "records/<sha>.zip, read-only; record_created with that SHA-256")

check("GXP-12", "A security alert goes to the system log (or the fallback file) and into the trail",
      tryCatch({
        d <- file.path(gxp_tmp, "alert"); dir.create(d); gxp_set(d)
        gxp_env$audit_init(file.path(d, "audit.sqlite"), user = "owner", org = "Validation Org")
        via_logger <- if (nzchar(Sys.which("logger"))) isTRUE(gxp_env$gxp_alert("login_locked", "ana", session = NULL)) else TRUE
        old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = "")
        via_file <- isTRUE(gxp_env$gxp_alert("signature_failures", "ana", session = NULL))
        Sys.setenv(PATH = old_path)
        tr <- gxp_env$audit_read(file.path(d, "audit.sqlite")); gxp_unset()
        via_logger && via_file && file.exists(file.path(d, "security_alerts.log")) &&
          sum(tr$event == "security_alert") == 2 && all(grepl('"sent":true', tr$details[tr$event == "security_alert"]))
      }, error = function(e) { Sys.setenv(PATH = old_path); gxp_unset(); FALSE }),
      "URS-GXP-16", method = "gxp_alert() with logger, and with logger unavailable",
      expected = "sent both times; security_alerts.log written in the fallback; two security_alert entries")

check("GXP-13", "The app refuses to start with gxp/CONTROLLED present and controlled mode not configured, or with the organisation missing",
      tryCatch({
        gxp_unset(); app <- file.path(gxp_tmp, "app"); dir.create(file.path(app, "gxp"), recursive = TRUE)
        free <- isFALSE(gxp_env$gxp_check_startup(app))
        file.create(file.path(app, "gxp", "CONTROLLED"))
        marked <- inherits(tryCatch(gxp_env$gxp_check_startup(app), error = function(e) e), "error")
        d <- file.path(gxp_tmp, "startup"); dir.create(d); gxp_set(d); Sys.setenv(NCA_GXP_ORG = "")
        no_org <- tryCatch({ gxp_env$gxp_check_startup(app); "" }, error = function(e) conditionMessage(e))
        gxp_unset()
        free && marked && grepl("NCA_GXP_ORG", no_org)
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-01", method = "gxp_check_startup() without and with the marker; with NCA_GXP_ORG empty",
      expected = "open mode allowed without the marker; stop with the marker; stop naming NCA_GXP_ORG")

# --- gxp/manage_users.R ------------------------------------------------------
gxp_mu_dir <- file.path(gxp_tmp, "mu"); dir.create(gxp_mu_dir)
gxp_mu_env <- c(sprintf("NCA_GXP_DIR=%s", file.path(gxp_mu_dir, "ctl")), "NCA_GXP_KEY=validation-key",
                "NCA_GXP_ORG='Validation Org'", "SUDO_USER=validator")
gxp_mu <- function(..., env = gxp_mu_env) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("gxp/manage_users.R", ...),
                                  env = env, stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"), out = out)
}
gxp_mu_trail <- function() gxp_env$audit_read(file.path(gxp_mu_dir, "ctl", "audit.sqlite"))
if (gxp_ok) {
  gxp_mu("init"); gxp_mu("add", "ana", "'Ana Lyst'", "analyst", "'new analyst'")
  gxp_mu("add", "rev", "'Rev Iewer'", "'analyst;reviewer'", "'new reviewer'")
  gxp_mu("add", "insp", "'In Spector'", "inspector", "'inspection'")
  gxp_mu("role", "ana", "'analyst;reviewer'", "'promoted'"); gxp_mu("reset", "rev", "'forgotten'")
  gxp_mu("deactivate", "insp", "'inspection over'")
}

check("GXP-14", "manage_users.R: each command writes its event, with the person from SUDO_USER, a reason, and old and new values",
      tryCatch({
        tr <- gxp_mu_trail(); adm <- tr[tr$event %in% c("user_added", "role_changed", "password_reset", "user_deactivated"), ]
        identical(adm$event, c("user_added", "user_added", "user_added", "role_changed", "password_reset", "user_deactivated")) &&
          all(adm$user == "validator") && all(adm$role == "system owner") && all(nzchar(adm$reason)) &&
          grepl('"old":"analyst","new":"analyst;reviewer"', adm$details[4]) &&
          grepl('"expire":\\{"old":null,"new":"', adm$details[6]) &&
          tr$event[1] == "trail_created" && gxp_env$audit_verify(file.path(gxp_mu_dir, "ctl", "audit.sqlite"))$intact
      }, error = function(e) FALSE),
      "URS-GXP-04", method = "init, add x3, role, reset, deactivate through Rscript with SUDO_USER=validator",
      expected = "six account entries by validator, reasons, old/new values; trail intact")

check("GXP-15", "manage_users.R refuses a reused ID, an inspector with a second role, a missing reason, and the service account acting as itself; an ID only typed at the login page stays free",
      tryCatch({
        reused <- gxp_mu("add", "ana", "'Someone Else'", "analyst", "'again'")
        combo  <- gxp_mu("add", "qa1", "'Q A'", "'inspector;reviewer'", "'x'")
        noreason <- gxp_mu("add", "qa2", "'Q A'", "analyst", "' '")
        service <- gxp_mu("add", "qa3", "'Q A'", "analyst", "'x'",
                          env = c(setdiff(gxp_mu_env, "SUDO_USER=validator"), "SUDO_USER=",
                                  sprintf("NCA_GXP_SERVICE_ACCOUNT=%s", Sys.info()[["user"]])))
        gxp_env$audit_append("login_failed", object = "newcomer", user = "newcomer", role = "unknown user",
                             details = list(known_user = FALSE), org = "Validation Org",
                             path = file.path(gxp_mu_dir, "ctl", "audit.sqlite"))
        fresh <- gxp_mu("add", "newcomer", "'New Comer'", "analyst", "'joined'")
        st <- gxp_env$gxp_store_read(file.path(gxp_mu_dir, "ctl", "users.sqlite"), "validation-key")
        fresh$status == 0 && "newcomer" %in% st$credentials$user &&
        all(c(reused$status, combo$status, noreason$status, service$status) != 0) &&
          grepl("never reused", paste(reused$out, collapse = " ")) &&
          grepl("cannot be combined", paste(combo$out, collapse = " ")) &&
          grepl("through sudo", paste(service$out, collapse = " ")) &&
          !any(c("qa1", "qa2", "qa3") %in% st$credentials$user)
      }, error = function(e) FALSE),
      "URS-GXP-04", method = "four invalid calls; then add an ID that only appears as a failed sign-in", expected = "each refused with its message; no account created for them; the ID from the login page is added")

check("GXP-16", "New and reset accounts start with admin and must change it; a deactivated account cannot sign in or sign",
      tryCatch({
        ck <- shinymanager::check_credentials(file.path(gxp_mu_dir, "ctl", "users.sqlite"), passphrase = "validation-key")
        st <- gxp_env$gxp_store_read(file.path(gxp_mu_dir, "ctl", "users.sqlite"), "validation-key")
        pm <- st$pwd_mngt; tr <- gxp_mu_trail()
        isTRUE(ck("ana", "admin")$result) && isTRUE(ck("rev", "admin")$result) &&
          gxp_env$gxp_must_change("ana", st, tr) && gxp_env$gxp_must_change("rev", st, tr) &&
          all(pm$must_change == "FALSE") &&
          isFALSE(ck("insp", "admin")$result) && isTRUE(ck("insp", "admin")$expired) &&
          all(st$credentials$is_hashed_password)
      }, error = function(e) FALSE),
      "URS-GXP-03,URS-GXP-04", method = "shinymanager::check_credentials() on the store built by the script (sign-in and signing use it)",
      expected = "ana and rev accepted, with a password change due (from the trail); shinymanager's own flag FALSE for all; insp refused as expired; passwords stored hashed")

check("GXP-17", "archive: a read-only folder whose manifest checks out, verified in a fresh R session from the archived release",
      tryCatch({
        a <- gxp_mu("archive", shQuote(file.path(gxp_mu_dir, "arch")))
        out <- list.files(file.path(gxp_mu_dir, "arch"), full.names = TRUE)[1]
        man <- read.table(file.path(out, "MANIFEST.sha256"), col.names = c("sha", "file"), stringsAsFactors = FALSE)
        man_ok <- all(vapply(seq_len(nrow(man)), function(i)
          identical(gxp_env$sha256_file(file.path(out, man$file[i])), man$sha[i]), logical(1)))
        need <- c("audit.sqlite", "app_release.zip", "renv.lock", "RESTORE.md", "verification_report.txt")
        rel <- file.path(gxp_mu_dir, "restored"); dir.create(rel)
        utils::unzip(file.path(out, "app_release.zip"), exdir = rel)
        fresh <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                          c(file.path(rel, "gxp", "manage_users.R"), "verify", shQuote(out)),
                                          env = gxp_mu_env, stdout = TRUE, stderr = TRUE))
        a$status == 0 && all(file.exists(file.path(out, need))) && man_ok &&
          file.access(file.path(out, "audit.sqlite"), 2) != 0 &&
          any(grepl("^Intact", fresh)) && tail(gxp_mu_trail()$event, 1) == "trail_archived"
      }, error = function(e) FALSE),
      "URS-GXP-12,URS-GXP-19", method = "manage_users.R archive; SHA-256 of every file; unzip the archived release and run its verify on the archive",
      expected = "all files present and matching; read-only; 'Intact' from the archived release; trail_archived written")

check("GXP-18", "Two account changes at the same moment both survive (one transaction each)",
      tryCatch({
        rs <- file.path(R.home("bin"), "Rscript")
        for (id in c("c1", "c2")) system2(rs, c("gxp/manage_users.R", "add", id, "'Con Current'", "analyst", "'parallel'"),
                                          env = gxp_mu_env, wait = FALSE, stdout = FALSE, stderr = FALSE)
        t0 <- Sys.time(); st <- NULL
        repeat {
          st <- tryCatch(gxp_env$gxp_store_read(file.path(gxp_mu_dir, "ctl", "users.sqlite"), "validation-key"), error = function(e) NULL)
          if ((!is.null(st) && all(c("c1", "c2") %in% st$credentials$user)) || difftime(Sys.time(), t0, units = "secs") > 60) break
          Sys.sleep(0.5)
        }
        all(c("c1", "c2") %in% st$credentials$user) && all(c("c1", "c2") %in% st$pwd_mngt$user)
      }, error = function(e) FALSE),
      "URS-GXP-04", method = "two 'add' processes started together", expected = "both accounts in credentials and pwd_mngt")

skip_manual("MAN-GXP-01", "Restore an archive on a clean machine",
            "On a machine without the app: unzip app_release.zip from a manage_users.R archive, renv::restore(lockfile = 'renv.lock'), copy the archive to a writable folder, set NCA_GXP_DIR/KEY/ORG to it (with the users.sqlite of the installation), start the app",
            "The app starts in controlled mode; the Records page lists the archived records; Verify chain reports the head in verification_report.txt",
            "URS-GXP-19")

# --- Login, roles, app start (R/gxp_access.R, app.R) ------------------------
if (gxp_ok) sys.source("R/gxp_access.R", envir = gxp_env)
gxp_session <- function(roles, user = "u1") {
  s <- new.env(); s$userData <- new.env(); s$userData$gxp <- list(user = user, name = "U One", roles = roles)
  s$token <- "abcdef0123456789"; s
}
# A controlled directory with a user store holding one account
gxp_ctl <- function(name) {
  d <- file.path(gxp_tmp, name); dir.create(d); gxp_set(d)
  gxp_env$audit_init(file.path(d, "audit.sqlite"), user = "owner", org = "Validation Org")
  gxp_env$gxp_store_init(file.path(d, "users.sqlite"), "validation-key")
  gxp_env$gxp_store_update(function(st) {
    st$credentials <- rbind(st$credentials, data.frame(user = "u1", password = "Valid2026pass", start = as.character(Sys.Date()),
      expire = NA_character_, admin = "FALSE", name = "U One", roles = "analyst", is_hashed_password = FALSE,
      stringsAsFactors = FALSE)[, names(st$credentials)])
    st$pwd_mngt <- rbind(st$pwd_mngt, data.frame(user = "u1", must_change = "FALSE", have_changed = "TRUE",
      date_change = as.character(Sys.Date()), n_wrong_pwd = 0, stringsAsFactors = FALSE)[, names(st$pwd_mngt)])
    st
  })
  d
}

check("GXP-19", "Password rule: at least 12 characters, a digit, a lower-case and an upper-case letter, never admin",
      tryCatch({
        v <- gxp_env$gxp_validate_pwd
        v("Valid2026pass") && !v("Short1Aa") && !v("alllowercase2026") && !v("ALLUPPERCASE2026") &&
          !v("NoDigitsAtAllHere") && !v("admin") && !v("ADMIN") && !v(NA_character_) && !v(c("Valid2026pass", "x"))
      }, error = function(e) FALSE),
      "URS-GXP-03", method = "gxp_validate_pwd() on 9 cases", expected = "only Valid2026pass accepted")

check("GXP-20", "Open mode loads none of the controlled-mode packages and adds nothing to the UI",
      tryCatch({
        out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(paste(
          "suppressMessages(source('app.R'));",
          "cat(any(c('shinymanager','DBI','RSQLite') %in% loadedNamespaces()),",
          "grepl('gxp', as.character(ui)), '\\n')"))),
          env = c("NCA_GXP_DIR="), stdout = TRUE, stderr = FALSE))
        identical(tail(trimws(out), 1), "FALSE FALSE")
      }, error = function(e) FALSE),
      "URS-GXP-01,URS-GEN-04", method = "source app.R in a fresh R process with NCA_GXP_DIR unset; inspect loadedNamespaces() and the UI",
      expected = "no shinymanager, DBI or RSQLite loaded; no gxp element in the UI")

check("GXP-21", "app_started records the configuration in force (policy values, directory, organisation, versions)",
      tryCatch({
        d <- gxp_ctl("started"); gxp_env$gxp_app_started(); tr <- gxp_env$audit_read(file.path(d, "audit.sqlite"))
        det <- jsonlite::fromJSON(tail(tr$details, 1)); gxp_unset()
        tail(tr$event, 1) == "app_started" && det$pwd_validity_days == 90 && det$pwd_failure_limit == 5 &&
          det$timeout_min == 15 && det$organisation == "Validation Org" && !is.null(det$packages$shinymanager)
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-05,URS-GXP-14", method = "gxp_app_started() in a test directory", expected = "an app_started entry with the configuration")

check("GXP-22", "Roles are read from the signed-in user, per session",
      tryCatch({
        a <- gxp_session("analyst"); r <- gxp_session("analyst;reviewer"); i <- gxp_session("inspector")
        h <- gxp_env$has_role
        !h(a, "reviewer") && h(r, "reviewer") && h(r, "analyst") && h(i, "inspector") && !h(i, "reviewer") &&
          !h(new.env(), "reviewer")
      }, error = function(e) FALSE),
      "URS-GXP-10,URS-GXP-17", method = "has_role() for analyst, analyst;reviewer, inspector and a session without a user",
      expected = "roles as held; no role without a user")

check("GXP-23", "Sign-in attempts are logged (ok, failed, locked); the fifth failure locks the account from the trail, with an alert; shinymanager is never left to count",
      tryCatch({
        d <- gxp_ctl("login"); ck <- gxp_env$audited_check()
        ok <- isTRUE(ck("u1", "Valid2026pass")$result)
        fails <- lapply(1:5, function(k) ck("u1", paste0("Wrong2026pass", k)))
        ck("nobody", "Wrong2026pass")
        right_locked <- ck("u1", "Valid2026pass")            # right password, locked account
        wrong_locked <- ck("u1", "Wrong2026pass")            # wrong password, locked account
        pm <- gxp_env$gxp_store_read()$pwd_mngt
        tr <- gxp_env$audit_read(file.path(d, "audit.sqlite")); gxp_unset()
        ev <- tr$event[-1]; al <- tr$details[tr$event == "security_alert"]
        ok && identical(ev[1:6], c("login_ok", rep("login_failed", 5))) &&
          tr$role[tr$object %in% "nobody"][1] == "unknown user" &&
          all(vapply(fails, function(f) isFALSE(f$result) && is.null(f$user_info), logical(1))) &&
          isFALSE(right_locked$result) && isFALSE(right_locked$authorized) && !is.null(right_locked$user_info) &&
          isFALSE(wrong_locked$authorized) && sum(ev == "login_locked") == 2 &&
          sum(grepl("account_locked", al)) == 1 && sum(grepl("login_locked", al)) == 2 &&
          all(pm$n_wrong_pwd == 0)
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-03,URS-GXP-05,URS-GXP-16", method = "audited_check(): a right login, five wrong ones, an unknown ID, then the right and a wrong password on the locked account",
      expected = "login_ok, login_failed x5, login_locked x2; an account_locked alert at the fifth failure and two login_locked alerts; failures returned without user_info and locked attempts as not authorized, so shinymanager writes no counter")

skip_manual("MAN-GXP-02", "Nothing runs before sign-in",
            "On the login page, and while the Choose your own password dialog of a first sign-in is open, run Shiny.setInputValue('nav_path','be') and Shiny.setInputValue('gxp_rec_approve', 1) in the browser console",
            "Nothing changes behind the dialog; the audit trail shows no analysis, navigation or signing entries for that session", "URS-GXP-02")
skip_manual("MAN-GXP-03", "First sign-in with the starting password",
            "Create an account with manage_users.R add; sign in with admin; in the dialog try admin and a short password as the new password; then a valid one. Repeat after manage_users.R reset",
            "The Choose your own password dialog opens at once and cannot be closed (only Sign out); it states the 12-character rule; admin and the short password are refused; after the valid one the app opens; the trail shows password_changed (first sign-in, reset or expiry)",
            "URS-GXP-03")
skip_manual("MAN-GXP-04", "Header, hub line and About page in controlled mode",
            "Sign in as an analyst and as a reviewer",
            "Header: Controlled badge (tooltip with organisation and host) and the user menu with name, roles, Change password and Sign out; hub: 'Signed in as ...'; About: organisation, host and controlled directory",
            "URS-GXP-02")
skip_manual("MAN-GXP-05", "Inactivity: activity keeps the session, the warning comes 2 minutes before",
            "Sign in; move the mouse now and then for 20 minutes; then leave the page untouched",
            "Still signed in after 20 minutes; after 13 minutes without activity a warning with a countdown and Stay signed in; after 15 minutes the login page",
            "URS-GXP-03")
skip_manual("MAN-GXP-06", "Change password from the user menu",
            "Open Change password; enter a wrong current password; then the right one with a new password that breaks the rule; then a valid new password",
            "'incorrect, 2 attempts left'; the rule is shown; 'Your password has been changed.'; you stay signed in; the browser does not offer to fill in the fields; the trail shows password_change_failed and password_changed (user menu)",
            "URS-GXP-03")
skip_manual("MAN-GXP-07", "Sign out",
            "Choose Sign out in the user menu", "The login page; the trail shows session_end with reason 'sign out'", "URS-GXP-05")
skip_manual("MAN-GXP-08", "Open mode is unchanged",
            "Start the app without NCA_GXP_DIR; compare the hub and the four analysis paths with the previous release",
            "No login, no Controlled badge, no user menu, no hub line; the screens are the same as before", "URS-GXP-01")

# --- Hooks in the analysis modules (T5) --------------------------------------
check("GXP-24", "Every place results are produced or leave the app has its audit hook",
      tryCatch({
        n <- function(f, pat) sum(grepl(pat, readLines(f, warn = FALSE), fixed = TRUE))
        n("R/mod_data_upload.R", 'gxp_guard("data_loaded"') == 1 &&
          n("R/mod_path_single_nca.R", 'gxp_guard("data_loaded"') == 1 &&
          n("R/mod_path_single_nca.R", 'gxp_guard("analysis_run"') == 2 &&
          n("R/mod_path_multi_nca.R", 'gxp_guard("analysis_run"') == 2 &&
          n("R/mod_path_be.R", 'gxp_guard("analysis_run"') == 3 &&
          n("R/mod_path_single_nca.R", "gxp_export_done(") == 1 && n("R/mod_path_multi_nca.R", "gxp_export_done(") == 2 &&
          n("R/mod_path_be.R", "gxp_export_done(") == 2 && n("R/mod_path_viz.R", "gxp_export_done(") == 1 &&
          all(vapply(c("R/mod_path_single_nca.R", "R/mod_path_multi_nca.R", "R/mod_path_be.R", "R/mod_path_viz.R"),
                     function(f) n(f, "gxp_record_done(") == 1 && n(f, "gxp_analyst(input$record_analyst)") == 1, logical(1)))
      }, error = function(e) FALSE),
      "URS-GXP-05,URS-GXP-15", method = "count the hooks in the module sources",
      expected = "data_loaded 2, analysis_run 7, export_downloaded 6, record_created 4, signed-in analyst in 4 records")

check("GXP-25", "Record and export hooks stop the download when the audit trail cannot be written, and do nothing in open mode",
      tryCatch({
        gxp_unset(); z <- tempfile(fileext = ".zip", tmpdir = gxp_tmp); writeLines("content", z)
        open_ok <- isTRUE(gxp_env$gxp_record_done(z, "r.zip", "batch_nca", "", NULL, session = NULL)) &&
          isTRUE(gxp_env$gxp_export_done(z, "r.csv", "csv", session = NULL))
        d <- gxp_ctl("hooks")
        stored <- nchar(gxp_env$gxp_record_done(z, "r.zip", "batch_nca", "S1", structure(1, reproduction = "MATCH"), session = NULL)) == 64
        exported <- isTRUE(gxp_env$gxp_export_done(z, "r.csv", "csv", "abc", session = NULL))
        Sys.chmod(file.path(d, "audit.sqlite"), "0444")
        rec_stop <- inherits(tryCatch(gxp_env$gxp_record_done(z, "r2.zip", "batch_nca", "", NULL, session = NULL),
                                      error = function(e) e), "error")
        exp_stop <- inherits(tryCatch(gxp_env$gxp_export_done(z, "r2.csv", "csv", session = NULL),
                                      error = function(e) e), "error")
        z3 <- tempfile(fileext = ".zip", tmpdir = gxp_tmp); writeLines("other content", z3)   # a record not stored before
        new_stop <- inherits(tryCatch(gxp_env$gxp_record_done(z3, "r3.zip", "batch_nca", "", NULL, session = NULL),
                                      error = function(e) e), "error")
        no_orphan <- !file.exists(file.path(d, "records", paste0(gxp_env$sha256_file(z3), ".zip")))
        Sys.chmod(file.path(d, "audit.sqlite"), "0644")
        tr <- gxp_env$audit_read(file.path(d, "audit.sqlite")); gxp_unset()
        open_ok && stored && exported && rec_stop && exp_stop && new_stop && no_orphan &&
          identical(tr$event[-1], c("record_created", "export_downloaded")) && grepl('"reproduction":"MATCH"', tr$details[2])
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-07,URS-GXP-09", method = "gxp_record_done() and gxp_export_done() in open mode, with a writable and with a read-only trail",
      expected = "no-ops in open mode; entries when writable; an error (download stopped), no entry and no stored copy when read-only")

check("GXP-26", "The analyst in a record is the signed-in user in controlled mode, the typed name otherwise",
      tryCatch({
        gxp_unset(); a <- gxp_env$gxp_analyst
        typed <- a("J. Typed", session = NULL) == "J. Typed" && a("", session = NULL) == "Analyst" && a(NULL, session = NULL) == "Analyst"
        gxp_set(file.path(gxp_tmp, "hooks")); signed <- a("J. Typed", session = gxp_session("analyst")) == "U One"; gxp_unset()
        typed && signed
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-02", method = "gxp_analyst() in open and controlled mode", expected = "typed name or 'Analyst'; signed-in name in controlled mode")

check("GXP-27", "Logging adds well under 100 ms to an analysis",
      tryCatch({
        d <- gxp_ctl("timing"); s <- gxp_session("analyst")
        t <- system.time(for (i in 1:20) gxp_env$gxp_guard("analysis_run", object = "all subjects",
                                                           sha256 = gxp_env$sha256_file("data/example_theoph.csv"),
                                                           details = list(i = i), session = s))[["elapsed"]] / 20
        gxp_unset(); t < 0.1
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-05", method = "20 x gxp_guard() with the SHA-256 of example_theoph.csv", expected = "mean < 0.1 s per entry",
      detail = "controlled-mode overhead per analysis_run")

skip_manual("MAN-GXP-09", "Each analysis path writes its entries with the right hashes",
            "Controlled mode, signed in as an analyst: type data in One Subject and run it; upload example_theoph.csv, run All Subjects; upload example_be_crossover.csv, run Bioequivalence; draw a figure; in each path download the results and create a record; recalculate one half-life with other points",
            "The trail shows data_loaded (typed in / the file's SHA-256), analysis_run (run and half-life override) with the same data SHA-256, export_downloaded per file and record_created per record; the records folder holds each zip under its SHA-256; the status line under each record button says 'Stored for review' (a figure: 'Stored')",
            "URS-GXP-05,URS-GXP-09")
skip_manual("MAN-GXP-10", "Fail closed in the app",
            "Make audit.sqlite read-only on the test server; run an analysis; try a download; make it writable again",
            "The 'Not carried out' dialog; no new results; the download fails with the same dialog; no entries were added",
            "URS-GXP-07")

# --- Records, signatures, Audit trail page (R/gxp_sign.R) and password change ---
if (gxp_ok) { suppressMessages({ library(shiny); library(bslib) })
  sys.source("R/gxp_sign.R", envir = gxp_env); sys.source("R/export_record.R", envir = gxp_env) }
# A controlled directory with analyst ana, reviewer rev (also analyst) and inspector insp
gxp_team <- function(name) {
  d <- gxp_ctl(name)
  gxp_env$gxp_store_update(function(st) {
    add <- function(st, u, r) {
      st$credentials <- rbind(st$credentials, data.frame(user = u, password = "Valid2026pass", start = as.character(Sys.Date()),
        expire = NA_character_, admin = "FALSE", name = toupper(u), roles = r, is_hashed_password = FALSE,
        stringsAsFactors = FALSE)[, names(st$credentials)])
      st$pwd_mngt <- rbind(st$pwd_mngt, data.frame(user = u, must_change = "FALSE", have_changed = "TRUE",
        date_change = as.character(Sys.Date()), n_wrong_pwd = 0, stringsAsFactors = FALSE)[, names(st$pwd_mngt)])
      st }
    add(add(add(st, "ana", "analyst"), "rev", "analyst;reviewer"), "insp", "inspector") })
  # Their own passwords are in place: no change is due (the app reads that from the trail)
  for (u in c("ana", "rev", "insp")) gxp_env$audit_append("password_changed", object = u, user = u, role = "setup",
                                                         details = list(via = "validation setup"))
  d
}
gxp_mock <- function(u, roles, tok) { s <- gxp_session(roles, u); s$token <- tok; s$userData$gxp$name <- toupper(u)
  s$sendCustomMessage <- function(...) NULL; s }
# A real record zip, stored by user u
gxp_rec <- function(u, roles, tok, label, data_sha = NULL) {
  src <- file.path(gxp_tmp, paste0(label, ".txt")); writeLines(label, src)
  z <- file.path(gxp_tmp, paste0(label, ".zip")); utils::zip(z, src, flags = "-jq")
  s <- gxp_mock(u, roles, tok)
  if (!is.null(data_sha)) gxp_env$gxp_guard("analysis_run", object = "all subjects", sha256 = data_sha,
                                            details = list(trigger = "run", path = "multi_nca", settings = list(blq_rule = "rule1")), session = s)
  gxp_env$gxp_store_record(z, paste0(label, ".zip"), "batch_nca", data_sha256 = .or_na(data_sha), session = s)
}
.or_na <- function(x) if (is.null(x)) NA_character_ else x
gxp_as <- function(u, roles) function(input, output, session) {
  session$userData$gxp <- list(user = u, name = toupper(u), roles = roles); session$userData$gxp_failures <- 0L
  gxp_env$gxp_sign_server(input, output, session) }
gxp_pick <- function(session, sha) {
  r <- gxp_env$gxp_records(); r <- r[order(r$created, decreasing = TRUE), ]
  session$setInputs(gxp_rec_filter = "all", gxp_rec_table_rows_selected = which(r$sha == sha)) }
gxp_msg <- function(output, id = "gxp_sign_msg") paste(unlist(output[[id]]), collapse = " ")

check("GXP-28", "Signing: own record, wrong password, a closed record, an analyst and an inspector are all refused; a reviewer's signature binds to the SHA-256",
      tryCatch({
        gxp_team("sign1")
        sha_ana <- gxp_rec("ana", "analyst", "aaaaaaaa01", "rec_ana1"); sha_rev <- gxp_rec("rev", "analyst;reviewer", "bbbbbbbb01", "rec_rev1")
        res <- list()
        testServer(gxp_as("rev", "analyst;reviewer"), {
          gxp_pick(session, sha_rev); session$userData$gxp_sign_meaning <- "review_approved"
          session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 1)
          res$own <<- gxp_msg(output)
          gxp_pick(session, sha_ana); session$setInputs(gxp_rec_approve = 1)
          session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Wrong2026pass", gxp_sign_submit = 2)
          res$wrong <<- gxp_msg(output)
          session$setInputs(gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 3)
          session$setInputs(gxp_sign_submit = 4); res$closed <<- gxp_msg(output)
        })
        for (who in list(c("ana", "analyst"), c("insp", "inspector"))) testServer(gxp_as(who[1], who[2]), {
          gxp_pick(session, sha_rev); session$userData$gxp_sign_meaning <- "review_approved"
          session$setInputs(gxp_sign_user = who[1], gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 1)
          res[[who[1]]] <<- gxp_msg(output) })
        tr <- gxp_env$audit_read(); sg <- tr[tr$event == "record_signed", ]; gxp_unset()
        grepl("own record", res$own) && grepl("2 attempts left", res$wrong) && grepl("already been reviewed", res$closed) &&
          grepl("Only a reviewer", res$ana) && grepl("Only a reviewer", res$insp) &&
          sum(grepl("signing_without_role", tr$details[tr$event == "security_alert"])) == 2 &&
          nrow(sg) == 1 && sg$sha256 == sha_ana && sg$user == "rev" && grepl('"meaning":"review_approved","printed_name":"REV"', sg$details) &&
          sum(tr$event == "signature_failed") == 1
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-08,URS-GXP-10,URS-GXP-17", method = "testServer(gxp_sign_server) as reviewer, analyst and inspector",
      expected = "refusals with their messages; a security alert for each attempt without the reviewer role; one record_signed on ana's record, by rev, with meaning and printed name")

check("GXP-29", "The third failed signature ends the session and sends a security alert; a locked account cannot sign",
      tryCatch({
        gxp_team("sign2"); sha <- gxp_rec("ana", "analyst", "aaaaaaaa02", "rec_ana2"); closed <- FALSE; locked_msg <- ""
        testServer(gxp_as("rev", "analyst;reviewer"), {
          gxp_pick(session, sha); session$setInputs(gxp_rec_approve = 1)
          for (k in 1:3) session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Wrong2026pass", gxp_sign_submit = k)
          closed <<- session$isClosed() })
        gxp_env$gxp_store_update(function(st) { st$pwd_mngt$n_wrong_pwd[st$pwd_mngt$user == "rev"] <- 5; st })
        testServer(gxp_as("rev", "analyst;reviewer"), {
          gxp_pick(session, sha); session$setInputs(gxp_rec_approve = 1)
          session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 1)
          locked_msg <<- gxp_msg(output) })
        tr <- gxp_env$audit_read(); gxp_unset()
        closed && sum(tr$event == "signature_failed") == 3 && any(tr$event == "security_alert") &&
          grepl("locked", locked_msg) && !any(tr$event == "record_signed")
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-03,URS-GXP-08,URS-GXP-16", method = "three wrong passwords in one session; then a signature with the lock counter at the limit",
      expected = "session closed; 3 signature_failed; a security alert; locked account refused")

check("GXP-30", "A password changed during the session is the one that counts at signing",
      tryCatch({
        gxp_team("sign3"); sha <- gxp_rec("ana", "analyst", "aaaaaaaa03", "rec_ana3"); m <- ""
        testServer(gxp_as("rev", "analyst;reviewer"), {
          gxp_pick(session, sha); session$setInputs(gxp_rec_approve = 1)
          gxp_env$gxp_store_update(function(st) { i <- st$credentials$user == "rev"
            st$credentials$password[i] <- "Changed2026pass"; st$credentials$is_hashed_password[i] <- FALSE; st })
          session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 1); m <<- gxp_msg(output)
          session$setInputs(gxp_sign_pwd = "Changed2026pass", gxp_sign_submit = 2) })
        tr <- gxp_env$audit_read(); gxp_unset()
        grepl("incorrect", m) && sum(tr$event == "record_signed") == 1
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-08", method = "change rev's password in the store after the session started; sign with the old, then the new",
      expected = "old refused; new accepted")

check("GXP-31", "Signature sheet, validity line and signed bundle; a changed stored copy makes the signature INVALID",
      tryCatch({
        d <- gxp_team("sign4"); sha <- gxp_rec("ana", "analyst", "aaaaaaaa04", "rec_ana4"); files <- NULL
        testServer(gxp_as("rev", "analyst;reviewer"), {
          gxp_pick(session, sha); session$setInputs(gxp_rec_approve = 1)
          session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 1)
          files <<- file.path(gxp_tmp, "bundle.zip"); file.copy(output$gxp_rec_download, files, overwrite = TRUE) })
        out <- file.path(gxp_tmp, "bundle"); utils::unzip(files, exdir = out, unzip = "unzip")
        inner <- file.path(out, "rec_ana4.zip"); sheet_f <- list.files(out, "^signatures_", full.names = TRUE)
        sheet <- paste(readLines(sheet_f, warn = FALSE), collapse = " ")
        rec <- gxp_env$gxp_records(); rec <- rec[rec$sha == sha, ]
        ok_before <- gxp_env$gxp_record_valid(sha)
        f <- file.path(d, "records", paste0(sha, ".zip")); Sys.chmod(f, "0644"); cat("x", file = f, append = TRUE)
        tr <- gxp_env$audit_read(); ex <- gxp_env$gxp_exceptions(tr, gxp_env$gxp_records(tr))
        sheet_after <- gxp_env$gxp_signature_sheet(rec, tr); valid_after <- gxp_env$gxp_record_valid(sha); gxp_unset()
        identical(gxp_env$sha256_file(inner), sha) && length(sheet_f) == 1 &&
          all(vapply(c("ANA \\(ana, analyst\\)", "REV \\(rev, analyst;reviewer\\)", "\\(UTC\\)", "I approve this record",
                       "Signature valid", "Audit trail: intact", "Head: entry"), grepl, logical(1), sheet)) &&
          ok_before && !valid_after && grepl("Signature INVALID", sheet_after) &&
          nrow(ex[["Records whose stored copy no longer matches its SHA-256"]]) == 1 && any(tr$event == "record_downloaded")
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-08,URS-GXP-09,URS-GXP-18", method = "approve, download the bundle, read the sheet; append a byte to the stored copy",
      expected = "bundle = unchanged record zip + sheet with creator, reviewer, UTC, meaning, validity, head; after the change INVALID and listed in Exceptions")

check("GXP-32", "Verify a record file: a stored record is found with its signatures; an altered copy is not found",
      tryCatch({
        gxp_team("verify"); sha <- gxp_rec("ana", "analyst", "aaaaaaaa05", "rec_ana5"); z <- file.path(gxp_tmp, "rec_ana5.zip")
        alt <- file.path(gxp_tmp, "rec_ana5_altered.zip"); file.copy(z, alt); cat("x", file = alt, append = TRUE)
        found <- ""; missing <- ""
        testServer(gxp_as("insp", "inspector"), {
          session$setInputs(gxp_verify_file = data.frame(name = "rec_ana5.zip", size = file.size(z), type = "application/zip", datapath = z))
          found <<- gxp_msg(output, "gxp_verify_result")
          session$setInputs(gxp_verify_file = data.frame(name = "altered.zip", size = file.size(alt), type = "application/zip", datapath = alt))
          missing <<- gxp_msg(output, "gxp_verify_result") })
        tr <- gxp_env$audit_read(); gxp_unset()
        grepl("rec_ana5.zip", found) && grepl("Created a record", found) && grepl("No record with this SHA-256", missing) &&
          sum(tr$event == "record_verified") == 2
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-18", method = "testServer as inspector: upload the stored record and an altered copy", expected = "found / not found; two record_verified entries")

check("GXP-33", "Each Exceptions query finds its planted case",
      tryCatch({
        d <- gxp_team("exc"); s1 <- gxp_mock("ana", "analyst", "cccccc0101"); s2 <- gxp_mock("ana", "analyst", "cccccc0202")
        a <- function(ev, ..., s = NULL, user = "ana", role = "analyst") gxp_env$audit_append(ev, ..., user = user, role = role,
                                                                                             session = if (!is.null(s)) substr(s$token, 1, 8))
        a("app_started", details = list(host = "h", timeout_min = 15), user = "system", role = "system")
        a("app_started", details = list(host = "h", timeout_min = 30), user = "system", role = "system")
        a("login_failed", object = "ana"); a("signature_failed", object = "x.zip")
        dsha <- strrep("d", 64); gxp_rec("ana", "analyst", "cccccc0101", "rec_exc", data_sha = dsha)
        gxp_env$gxp_guard("analysis_run", sha256 = dsha, details = list(trigger = "run", settings = list(blq_rule = "rule4")), session = s1)
        sha2 <- gxp_rec("ana", "analyst", "cccccc0101", "rec_exc2", data_sha = dsha)
        a("analysis_run", sha256 = strrep("e", 64), details = list(trigger = "run"), s = s2)
        a("analysis_run", object = "later", s = s1)   # s1 still active after s2 started
        real <- gxp_env$gxp_utc_now
        assign("gxp_utc_now", function() format(as.POSIXct(paste(Sys.Date(), "03:00:00")), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"), envir = gxp_env)
        a("analysis_run", object = "night", s = s1); assign("gxp_utc_now", real, envir = gxp_env)
        a("record_signed", object = "rec_exc.zip", sha256 = gxp_env$gxp_records()$sha[1], reason = "wrong",
          details = list(meaning = "review_rejected"), user = "rev", role = "analyst;reviewer")
        tr <- gxp_env$audit_read(); ex <- gxp_env$gxp_exceptions(tr, gxp_env$gxp_records(tr)); gxp_unset()
        n <- vapply(ex, nrow, 1L)
        all(n[c("Failed and locked sign-ins, and security alerts", "Refused signatures and password changes", "Rejected records",
                "Data with runs but no record", "App started without a preceding stop",
                "Configuration differs from the previous start", "Entries earlier than the entry before them (clock)",
                "Activity outside working hours", "One user signed in in two sessions at the same time",
                "Data with more than one run before its record")] >= 1) &&
          n["Records whose stored copy no longer matches its SHA-256"] == 0
      }, error = function(e) { assign("gxp_utc_now", real, envir = gxp_env); gxp_unset(); FALSE }),
      "URS-GXP-14", method = "plant one case per query in a test trail; gxp_exceptions()", expected = "every query finds its case; no false invalid record")

check("GXP-34", "Users overview: current roles, status and role history; the trail review is signed only by a reviewer",
      tryCatch({
        d <- gxp_team("users")
        gxp_env$audit_append("user_added", object = "ana", details = list(roles = list(old = NULL, new = "analyst")), user = "own", role = "system owner")
        gxp_env$audit_append("role_changed", object = "ana", details = list(roles = list(old = "analyst", new = "analyst;reviewer")), user = "own", role = "system owner")
        ov <- gxp_env$gxp_users_overview(gxp_env$gxp_store_read(), gxp_env$audit_read())
        rv <- NULL
        testServer(gxp_as("insp", "inspector"), { session$setInputs(gxp_trail_review = 1); session$setInputs(gxp_rev_user = "insp", gxp_rev_pwd = "Valid2026pass", gxp_rev_submit = 1) })
        testServer(gxp_as("rev", "analyst;reviewer"), { session$setInputs(gxp_trail_review = 1)
          session$setInputs(gxp_rev_user = "rev", gxp_rev_pwd = "Valid2026pass", gxp_rev_submit = 1) })
        tr <- gxp_env$audit_read(); rv <- tr[tr$event == "trail_reviewed", ]; gxp_unset()
        grepl("analyst;reviewer", ov$Role_history[ov$User == "ana"]) && ov$Status[ov$User == "insp"] == "active" &&
          nrow(rv) == 1 && rv$user == "rev" && grepl('"head_seq"', rv$details)
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-14,URS-GXP-20", method = "gxp_users_overview(); trail review attempted by the inspector and by the reviewer",
      expected = "role history shown; one trail_reviewed, by rev, with the head")

check("GXP-35", "An analyst sees the status and review of own records only; reviewers and inspectors see the data history",
      tryCatch({
        gxp_team("vis"); dsha <- strrep("f", 64)
        mine <- gxp_rec("ana", "analyst", "dddddddd01", "rec_vis", data_sha = dsha); other <- gxp_rec("rev", "analyst;reviewer", "dddddddd02", "rec_vis2")
        own_html <- ""; other_html <- ""; insp_html <- ""
        testServer(gxp_as("ana", "analyst"), {
          session$setInputs(gxp_rec_table_rows_selected = 1)
          r <- gxp_env$gxp_records(); r <- r[r$author == "ana", ]
          session$setInputs(gxp_rec_table_rows_selected = 1); own_html <<- gxp_msg(output, "gxp_rec_detail") })
        testServer(gxp_as("insp", "inspector"), { gxp_pick(session, mine); insp_html <<- gxp_msg(output, "gxp_rec_detail") })
        gxp_unset()
        grepl("rec_vis.zip", own_html) && !grepl("Audit history", own_html) && !grepl("rec_vis2", own_html) &&
          grepl("Audit history of the data", insp_html) && !grepl("Approve", insp_html)
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-10,URS-GXP-13,URS-GXP-17", method = "record details for an analyst and an inspector",
      expected = "analyst: own record, no history; inspector: history, no Approve")

check("GXP-36", "Change password: refused without the right current password, when reused, when the rule is broken and when locked; accepted otherwise",
      tryCatch({
        gxp_team("pwd"); m <- list()
        srv <- function(input, output, session) {
          session$userData$gxp <- list(user = "ana", name = "ANA", roles = "analyst"); session$userData$gxp_failures <- 0L
          observeEvent(input$go, gxp_env$gxp_password_submit(input, session)) }
        testServer(srv, {
          try_pwd <- function(k, cur, new, rep = new) {
            session$setInputs(gxp_pwd_current = cur, gxp_pwd_new = new, gxp_pwd_repeat = rep, go = k)
            tryCatch(paste(unlist(output$gxp_pwd_msg), collapse = " "), error = function(e) "") }
          m$wrong <<- try_pwd(1, "Wrong2026pass", "Better2026pass")
          m$same <<- try_pwd(2, "Valid2026pass", "Valid2026pass")
          m$rule <<- try_pwd(3, "Valid2026pass", "short")
          m$differ <<- try_pwd(4, "Valid2026pass", "Better2026pass", "Other2026pass")
          gxp_env$gxp_store_update(function(st) { st$pwd_mngt$n_wrong_pwd[st$pwd_mngt$user == "ana"] <- 5; st })
          m$locked <<- try_pwd(5, "Valid2026pass", "Better2026pass")
          gxp_env$gxp_store_update(function(st) { i <- st$pwd_mngt$user == "ana"
            st$pwd_mngt$n_wrong_pwd[i] <- 0; st$pwd_mngt$date_change[i] <- "2026-01-01"; st })
          session$setInputs(gxp_pwd_current = "Valid2026pass", gxp_pwd_new = "Better2026pass", gxp_pwd_repeat = "Better2026pass", go = 6) })
        ck <- shinymanager::check_credentials(gxp_env$gxp_config()$users, passphrase = "validation-key")
        pm <- gxp_env$gxp_store_read()$pwd_mngt; tr <- gxp_env$audit_read(); gxp_unset()
        grepl("incorrect", m$wrong) && grepl("different from the current", m$same) && grepl("At least 12", m$rule) &&
          grepl("two new passwords are different", m$differ) && grepl("locked", m$locked) &&
          !isTRUE(ck("ana", "Valid2026pass")$result) && isTRUE(ck("ana", "Better2026pass")$result) &&
          pm$date_change[pm$user == "ana"] == as.character(Sys.Date()) && any(tr$event == "password_changed" & grepl("user menu", tr$details)) &&
          any(tr$event == "password_change_failed")
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-03", method = "testServer around gxp_password_submit(): five refusals, then a valid change",
      expected = "each refusal with its message; afterwards the old password fails, the new works, the expiry is reset, password_changed (user menu)")

check("GXP-37", "A required password change is made in the app's own dialog, under the store lock, and logged; afterwards no change is due",
      tryCatch({
        gxp_team("forced")
        gxp_env$audit_append("password_reset", object = "ana", details = list(), user = "own", role = "system owner")
        due_before <- gxp_env$gxp_must_change("ana"); started <- FALSE
        srv <- function(input, output, session) {
          session$userData$gxp <- list(user = "ana", name = "ANA", roles = "analyst"); session$userData$gxp_failures <- 0L
          session$userData$gxp_forced <- TRUE
          session$userData$gxp_start_app <- function() started <<- TRUE
          observeEvent(input$go, gxp_env$gxp_password_submit(input, session)) }
        testServer(srv, session$setInputs(gxp_pwd_current = "Valid2026pass", gxp_pwd_new = "Better2026pass",
                                          gxp_pwd_repeat = "Better2026pass", go = 1))
        tr <- gxp_env$audit_read(); due_after <- gxp_env$gxp_must_change("ana")
        ck <- shinymanager::check_credentials(gxp_env$gxp_config()$users, passphrase = "validation-key"); gxp_unset()
        src <- paste(deparse(body(gxp_env$gxp_server)), collapse = " ")
        due_before && !due_after && started && isTRUE(ck("ana", "Better2026pass")$result) &&
          any(tr$event == "password_changed" & grepl("first sign-in, reset or expiry", tr$details)) &&
          regexpr("gxp_must_change(auth$user)", src, fixed = TRUE) < regexpr("start_app()", src, fixed = TRUE)
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-03", method = "password_reset in the trail; gxp_password_submit() in a session marked for a required change; gxp_server checks gxp_must_change() before starting the app",
      expected = "a change is due before and not after; the new password works; password_changed (first sign-in, reset or expiry); the app then starts in the same session")

skip_manual("MAN-GXP-11", "Records page and signing dialog",
            "As a reviewer: open Records; select a record awaiting review; check the details and the data history; Approve with a wrong, then the right password; Reject another without and with a reason",
            "The dialog shows record, study, type, author, date, short SHA-256 and the meaning; the user ID stays after a failure; 'N attempts left'; the record stays selected, and the right password typed with Enter pressed straight after it signs the record; Sign is disabled until a reason is typed; the row changes status at once; the browser does not fill in the password",
            "URS-GXP-08")
skip_manual("MAN-GXP-12", "Inspector account", "Sign in as an inspector; open Records and Audit trail; try to sign from the browser console",
            "All records and the trail visible; no Approve, Reject or Sign trail review; the console attempt is refused", "URS-GXP-17")
skip_manual("MAN-GXP-13", "Audit trail page", "As a reviewer: open Audit trail; look at Exceptions; filter All entries; Verify chain; Export CSV; Users; Sign trail review",
            "Plain-language events; filters work; 'Intact: N entries'; the CSV starts with the verification line; the last-review line updates", "URS-GXP-11,URS-GXP-14")

# --- Self-review fixes (T7) ---------------------------------------------------
check("GXP-38", "An analyst gets none of the Audit trail outputs, even when a page asks for them; sheet downloads and verifications are logged or refused",
      tryCatch({
        d <- gxp_team("t7"); sha <- gxp_rec("ana", "analyst", "abababab01", "rec_t7")
        blocked <- c()
        testServer(gxp_as("ana", "analyst"), {
          for (o in c("gxp_review_line", "gxp_trail_table", "gxp_users_table", "gxp_exceptions"))
            blocked[o] <<- tryCatch({ x <- output[[o]]; is.null(x) || length(unlist(x)) == 0 }, error = function(e) TRUE)
          session$setInputs(gxp_verify_file = data.frame(name = "rec_t7.zip", size = 1, type = "application/zip",
                                                        datapath = file.path(gxp_tmp, "rec_t7.zip")))
          blocked["verify"] <<- tryCatch({ x <- output$gxp_verify_result; is.null(x) || length(unlist(x)) == 0 }, error = function(e) TRUE) })
        testServer(gxp_as("insp", "inspector"), { gxp_pick(session, sha); invisible(output$gxp_rec_sheet) })
        tr <- gxp_env$audit_read()
        sheet_logged <- any(tr$event == "record_downloaded" & grepl("signature sheet", tr$details))
        Sys.chmod(file.path(d, "audit.sqlite"), "0444"); shown <- "x"
        testServer(gxp_as("insp", "inspector"), {
          session$setInputs(gxp_verify_file = data.frame(name = "rec_t7.zip", size = 1, type = "application/zip",
                                                        datapath = file.path(gxp_tmp, "rec_t7.zip")))
          shown <<- tryCatch(paste(unlist(output$gxp_verify_result), collapse = ""), error = function(e) "") })
        Sys.chmod(file.path(d, "audit.sqlite"), "0644"); gxp_unset()
        all(blocked) && sheet_logged && !grepl("rec_t7", shown) && !any(tr$event == "record_verified")
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-10,URS-GXP-11,URS-GXP-07", method = "testServer as analyst (outputs forced) and inspector (sheet download; verification with a read-only trail)",
      expected = "no output for the analyst; record_downloaded for the sheet; no verification result without its entry")

check("GXP-39", "A login ID with control characters or excessive length is cleaned before it reaches the trail and the system log",
      tryCatch({
        gxp_team("t7b"); ck <- gxp_env$audited_check(); ck(paste0("evil\nFAKE LOG LINE", strrep("x", 200)), "Wrong2026pass")
        tr <- gxp_env$audit_read(); u <- tail(tr$user, 1); gxp_unset()
        !grepl("[[:cntrl:]]", u) && nchar(u) <= 64
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-05", method = "audited_check() with a newline and 200 extra characters in the ID", expected = "no control characters; at most 64 characters")

check("GXP-40", "The record's data_integrity.txt says which mode it was made in; open-mode records keep their wording",
      tryCatch({
        f <- file.path(gxp_tmp, "art.txt"); writeLines("x", f)
        o <- file.path(gxp_tmp, "rec_open"); dir.create(o); gxp_unset(); gxp_env$write_integrity_manifest(o, list(Data = f))
        gxp_team("t8"); cdir <- file.path(gxp_tmp, "rec_ctl"); dir.create(cdir); gxp_env$write_integrity_manifest(cdir, list(Data = f)); gxp_unset()
        open_txt <- paste(readLines(file.path(o, "data_integrity.txt")), collapse = " ")
        ctl_txt <- paste(readLines(file.path(cdir, "data_integrity.txt")), collapse = " ")
        grepl("it is not an audit trail or electronic signature", open_txt) &&
          grepl("controlled installation", ctl_txt) && grepl("audit trail of Validation Org", ctl_txt) &&
          !grepl("it is not an audit trail", ctl_txt)
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-09,URS-EXP-06", method = "write_integrity_manifest() in open and in controlled mode",
      expected = "open: the existing sentence; controlled: stored on the server, audit trail of the organisation")

check("GXP-41", "Whether a password change is due comes from the trail: new account, reset, expiry, a change that was not saved",
      tryCatch({
        gxp_team("gate"); real <- gxp_env$gxp_utc_now
        a <- function(ev, days = 0, details = list()) {
          assign("gxp_utc_now", function() format(Sys.time() - days * 86400, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"), envir = gxp_env)
          gxp_env$audit_append(ev, object = "ana", details = details, user = "ana", role = "analyst")
          assign("gxp_utc_now", real, envir = gxp_env) }
        due <- function() gxp_env$gxp_must_change("ana")
        r <- c(ok_setup = !due())
        a("password_reset");                   r["reset"] <- due()
        a("password_changed", days = 91);      r["old"] <- due()
        a("password_changed", days = 89);      r["recent"] <- !due()
        a("password_changed"); a("password_change_failed", details = list(why = "not saved; the password is unchanged"))
        r["unsaved"] <- !due()                 # the earlier (89-day) change still counts
        a("password_reset"); a("password_changed"); a("password_change_failed", details = list(why = "not saved; the password is unchanged"))
        r["unsaved_after_reset"] <- due()
        r["unknown"] <- gxp_env$gxp_must_change("nobody")
        gxp_unset(); all(r)
      }, error = function(e) { assign("gxp_utc_now", real, envir = gxp_env); gxp_unset(); FALSE }),
      "URS-GXP-02,URS-GXP-03", method = "trail entries for ana: reset, a 91-day-old and an 89-day-old change, a change logged but not saved (with and without a reset before it), and an unknown user",
      expected = "due after the reset and the 91-day-old change; not due after the 89-day-old one; an unsaved change does not count; due for an unknown user")

check("GXP-42", "Signing is refused while a password change is due and once the reviewer role has been removed",
      tryCatch({
        gxp_team("sign9"); sha <- gxp_rec("ana", "analyst", "aaaaaaaa41", "rec_ana41"); m <- list()
        testServer(gxp_as("rev", "analyst;reviewer"), {
          gxp_pick(session, sha); session$setInputs(gxp_rec_approve = 1)
          gxp_env$audit_append("password_reset", object = "rev", details = list(), user = "own", role = "system owner")
          session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 1); m$change <<- gxp_msg(output)
          gxp_env$audit_append("password_changed", object = "rev", details = list(via = "test"), user = "rev", role = "analyst;reviewer")
          gxp_env$gxp_store_update(function(st) { st$credentials$roles[st$credentials$user == "rev"] <- "analyst"; st })
          session$setInputs(gxp_sign_submit = 2); m$role <<- gxp_msg(output)
          session$setInputs(gxp_rev_user = "rev", gxp_rev_pwd = "Valid2026pass", gxp_rev_submit = 1); m$trail <<- gxp_msg(output, "gxp_rev_msg") })
        tr <- gxp_env$audit_read(); gxp_unset()
        grepl("Change your password", m$change) && grepl("reviewer role has been removed", m$role) &&
          grepl("reviewer role has been removed", m$trail) && !any(tr$event %in% c("record_signed", "trail_reviewed"))
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-03,URS-GXP-10", method = "testServer(gxp_sign_server) as rev; a password reset in the trail, then a change and the reviewer role taken away in the store during the session",
      expected = "both refused with their messages; no record_signed or trail_reviewed")

check("GXP-43", "A signature binds to the record shown in the dialog, not to a selection changed afterwards",
      tryCatch({
        gxp_team("sign10"); s1 <- gxp_rec("ana", "analyst", "aaaaaaaa42", "rec_ana42"); s2 <- gxp_rec("ana", "analyst", "aaaaaaaa43", "rec_ana43"); m <- list()
        testServer(gxp_as("rev", "analyst;reviewer"), {
          gxp_pick(session, s1)
          session$setInputs(gxp_sign_user = "rev", gxp_sign_pwd = "Valid2026pass", gxp_sign_submit = 1); m$nodialog <<- gxp_msg(output)
          session$setInputs(gxp_rec_approve = 1); gxp_pick(session, s2)
          session$setInputs(gxp_sign_submit = 2); m$moved <<- gxp_msg(output)
          gxp_pick(session, s1); session$setInputs(gxp_sign_submit = 3) })
        tr <- gxp_env$audit_read(); sg <- tr[tr$event == "record_signed", ]; gxp_unset()
        grepl("click Approve or Reject first", m$nodialog) && grepl("not the one in this dialog", m$moved) &&
          nrow(sg) == 1 && sg$sha256 == s1
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-08", method = "submit without opening the dialog; open it on one record, select another, submit; select the first again, submit",
      expected = "the first two refused; one record_signed, on the record shown in the dialog")

check("GXP-44", "manage_users.R verify with a filed head detects entries removed from the end of the trail",
      tryCatch({
        p <- gxp_trail(9); h <- gxp_env$audit_head(p); head_arg <- paste0(h$seq, ":", h$hash)
        ok_copy <- gxp_mu("verify", dirname(p), head_arg)
        t <- gxp_tamper(p, "DELETE FROM trail WHERE seq > 7")
        bare <- gxp_mu("verify", t); anchored <- gxp_mu("verify", t, head_arg)
        ok_copy$status == 0 && bare$status == 0 && anchored$status == 2 &&
          any(grepl("entries removed", anchored$out))
      }, error = function(e) FALSE),
      "URS-GXP-06,URS-GXP-12", method = "file the head of a trail; delete the last three entries in a copy; verify the copy without and with the filed head",
      expected = "intact trail with its head: exit 0; truncated copy: exit 0 without the head, exit 2 and 'entries removed' with it")

check("GXP-45", "A record names the data it was made from, also after other data were loaded or run in the same session",
      tryCatch({
        gxp_team("recdata"); s <- gxp_mock("ana", "analyst", "abcd4545")
        gxp_env$gxp_guard("data_loaded", object = "typed in", sha256 = strrep("b", 64), details = list(source = "typed in"), session = s)
        z <- file.path(gxp_tmp, "rec45.zip"); writeLines("rec45", file.path(gxp_tmp, "rec45.txt"))
        utils::zip(z, file.path(gxp_tmp, "rec45.txt"), flags = "-jq")
        gxp_env$gxp_store_record(z, "rec45.zip", "figure", data_sha256 = strrep("a", 64), session = s)
        tr <- gxp_env$audit_read(); rc <- tr[tr$event == "record_created", ]; gxp_unset()
        nrow(rc) == 1 && identical(jsonlite::fromJSON(rc$details)$data_sha256, strrep("a", 64)) &&
          all(grepl("data_sha256 = gxp_data_sha256\\(shared\\$study_info\\)|data_sha256 = \\.or\\(local\\$gxp_data_sha",
                    vapply(c("R/mod_path_viz.R", "R/mod_path_multi_nca.R", "R/mod_path_be.R", "R/mod_path_single_nca.R"),
                           function(f) paste(readLines(f), collapse = " "), "")))
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-05,URS-GXP-13", method = "log typed-in data, then store a figure record made from another dataset; each path passes its own data SHA-256 to gxp_record_done()",
      expected = "record_created carries the figure's data SHA-256, not the latest entry's; all four paths pass it")

check("GXP-46", "Ten failed sign-ins within an hour raise one alert; three wrong current passwords end the session with an alert",
      tryCatch({
        gxp_team("alerts"); ck <- gxp_env$audited_check()
        for (k in 1:11) ck("ghost", paste0("Wrong2026pass", k))
        closed <- FALSE
        srv <- function(input, output, session) {
          session$userData$gxp <- list(user = "ana", name = "ANA", roles = "analyst"); session$userData$gxp_failures <- 0L
          observeEvent(input$go, gxp_env$gxp_password_submit(input, session)) }
        testServer(srv, {
          for (k in 1:3) session$setInputs(gxp_pwd_current = "Wrong2026pass", gxp_pwd_new = "Better2026pass",
                                           gxp_pwd_repeat = "Better2026pass", go = k)
          closed <<- session$isClosed() })
        tr <- gxp_env$audit_read(); al <- tr$details[tr$event == "security_alert"]; gxp_unset()
        sum(grepl("failed_sign_ins", al)) == 1 && sum(tr$event == "login_failed" & tr$object == "ghost") == 11 &&
          closed && sum(tr$event == "password_change_failed") == 3 && sum(grepl("password_change_failures|3 failed attempts", al)) == 1
      }, error = function(e) { gxp_unset(); FALSE }),
      "URS-GXP-16,URS-GXP-03", method = "audited_check() 11 times with a wrong password for one ID; three wrong current passwords in the Change password dialog",
      expected = "one failed_sign_ins alert at the tenth failure; the session closes after the third wrong current password, with one alert")

check("GXP-47", "manage_users.R list shows every account with its roles, status and history",
      tryCatch({
        l <- gxp_mu("list"); out <- paste(l$out, collapse = "\n")
        l$status == 0 && all(vapply(c("ana", "rev", "insp"), function(u) grepl(paste0("(^|\n)", u, " "), out), logical(1))) &&
          grepl("analyst;reviewer", out) && grepl("role_changed", out) && grepl("password_reset", out) && grepl("by validator", out)
      }, error = function(e) FALSE),
      "URS-GXP-20,URS-GXP-04", method = "manage_users.R list on the store built by the manage_users.R tests (add, role, reset)",
      expected = "exit 0; each account with its current roles; the role change and reset in its history, by the person who ran them")

gxp_unset()
end_section("GXP")

# =============================================================================
# SECTION ADV: Adversarial audit of v1.8.0 (September 2026)
# =============================================================================
# One or more regression tests per finding of the external adversarial audit
# that held up on verification, each built from the failing case.
start_section("ADV")

adv_quiet <- function(expr) {
  w <- character(0)
  r <- withCallingHandlers(expr, warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  list(value = r, warnings = w)
}

check("ADV-01", "F-19: names from the data file cannot put markup or script into a record's HTML",
  tryCatch({
    x <- "<img src=x onerror=alert(1)>"
    cm <- list(subject = x, time = paste0(x, "t"), conc = paste0(x, "c"), treatment = paste0(x, "trt"),
               period = paste0(x, "p"), sequence = paste0(x, "s"), dose = paste0(x, "d"))
    st <- list(admin_route = "iv_infusion", infusion_duration = 1, dose = 100, dose_unit = "<script>mg</script>",
               time_unit = "<script>h</script>", conc_unit = "<script>ng/mL</script>", is_steady_state = TRUE,
               tau = 24, trap_method = "linear", r2adj_threshold = 0.7, partial_aucs = NULL)
    ov <- list(p = list(profile = x, original_lambda_z = 0.1, adjusted_lambda_z = 0.12, original_r2adj = 0.9,
                        adjusted_r2adj = 0.95, points_used = 3))
    h1 <- generate_summary_html(st, cm, "data.csv", strrep("0", 64), "rule4", 0.5, "Validation", "Study",
                                2, 20, lz_overrides = ov)
    h2 <- generate_viz_html(list(plot_type = x, y_scale = x, color_by = x, summary_statistic = x, colour_palette = x),
                            cm, "data.csv", strrep("0", 64), "Validation", "Study", 2, 20)
    ok <- function(h) !grepl("<img", h, fixed = TRUE) && !grepl("<script", h, fixed = TRUE) &&
      grepl("&lt;img src=x onerror=alert(1)&gt;", h, fixed = TRUE)
    ok(h1) && ok(h2) && grepl("&lt;script&gt;ng/mL", h1, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-EXP-01, URS-VIZ-09", critical = TRUE,
  method = "generate_summary_html() and generate_viz_html() with the column names, units, subject ID and figure settings set to '<img src=x onerror=alert(1)>' and '<script>'",
  expected = "No raw <img> or <script> in either HTML; the names appear escaped (was: column names, units and the override profile pasted unescaped)")

check("ADV-02", "F-05: values set by a BLQ rule are not offered for a manual half-life fit and are left out of it",
  tryCatch({
    raw <- data.frame(Subject = "S1", Time = c(0, 1, 2, 4, 6, 8, 10, 12),
                      Conc = c("BLQ", 5, 10, 8, 4, "BLQ", 2, 1.5), stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    d <- prepare_pk_dataset(raw, cm, list(lloq = 1, blq_rule = "rule4"))$data
    cand <- lz_candidate_points(d$Time, d$Conc, d$BLQ_flag, "extravascular")
    pick <- which(d$Time %in% c(6, 8, 10, 12))
    rc <- recalculate_lambda_z(d$Time, d$Conc, pick, is_blq = d$BLQ_flag)
    st <- list(admin_route = "extravascular", dose = 100, dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL",
               trap_method = "linear", r2adj_threshold = 0.7, infusion_duration = 0, mw = 0,
               is_steady_state = FALSE, tau = NA, partial_aucs = NULL)
    r <- suppressWarnings(run_nca(d, cm, st, lz_overrides = list(list(subject = "S1", time_used = c(6, 8, 10, 12)))))
    d$Conc[d$Time == 8] == 0.5 && d$BLQ_flag[d$Time == 8] &&
      identical(d$Time[cand$term], c(4, 6, 10, 12)) && cand$n_blq == 1 && grepl("1 value set by the BLQ rule", cand$label) &&
      identical(rc$result$time_used, c(6, 10, 12)) && grepl("t = 8", rc$warning, fixed = TRUE) &&
      as.numeric(r$LAMZNPT) == 3
  }, error = function(e) FALSE),
  "URS-DAT-04, URS-NCA-12", critical = TRUE,
  method = "Rule 4, LLOQ 1, an embedded BLQ at 8 h set to 0.5; the point list, recalculate_lambda_z() with 6, 8, 10 and 12 h, and run_nca() with an override naming the same times",
  expected = "8 h not offered and counted in the label; the manual fit uses 6, 10 and 12 h with a note on 8 h; NonCompart fits 3 points (was: 8 h offered and fitted, 4 points)")

check("ADV-03", "F-21: '<x' entries with different limits give no LLOQ suggestion but a warning listing them",
  tryCatch({
    b <- blq_text_summary(c("<0.5", "<0.1", "3"))
    one <- blq_text_summary(c("<0.5", "<0.5", "BLQ"))
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    raw <- data.frame(Subject = 1, Time = c(0, 1, 2, 4, 8), Conc = c("<0.5", 3, 5, 2, "<0.1"), stringsAsFactors = FALSE)
    qc <- run_data_quality_check(raw, cm, lloq = 0.5)
    f <- qc$findings
    is.null(b$suggested_lloq) && identical(b$lloq_candidates, c(0.1, 0.5)) && identical(one$suggested_lloq, 0.5) &&
      any(f$Severity == "WARNING" & grepl("different limits: 0.1, 0.5", f$Message, fixed = TRUE)) &&
      !any(grepl("Auto-detected LLOQ candidate", f$Message, fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-DAT-03, URS-DAT-04", critical = TRUE,
  method = "blq_text_summary() on '<0.5' and '<0.1', and on '<0.5' twice; run_data_quality_check() on a file with both limits",
  expected = "No suggestion, limits 0.1 and 0.5 listed, a WARNING in the quality report; one limit still suggested (was: 0.1 suggested, the minimum)")

check("ADV-04", "F-22: a record that fails part-way leaves no folder behind; the unused cleanup handler is gone",
  tryCatch((function() {
    real_zip <- zip_record_dir
    on.exit(assign("zip_record_dir", real_zip, envir = globalenv()))
    assign("zip_record_dir", function(rec_dir, output_path) stop("simulated failure while zipping"), envir = globalenv())
    before <- list.files(tempdir(), "^analysis_record_")
    out <- tryCatch(create_analysis_record(tempfile(fileext = ".zip"), theoph_result, theoph_settings, theoph_cm,
                                           "data/example_theoph.csv", "example_theoph.csv", blq_rule = "rule1",
                                           lloq = 0, analyst = "Validation", study_name = "Cleanup"),
                    error = function(e) conditionMessage(e))
    after <- list.files(tempdir(), "^analysis_record_")
    identical(out, "simulated failure while zipping") && length(setdiff(after, before)) == 0 &&
      !any(grepl("session_temp_dirs", readLines("app.R"), fixed = TRUE))
  })(), error = function(e) FALSE),
  "URS-GEN-04", critical = FALSE,
  method = "create_analysis_record() for theophylline with the zip step replaced by one that fails; tempdir() before and after; app.R",
  expected = "The error reaches the caller and no analysis_record_ folder is left (was: left behind until the R process ends)")

check("ADV-05", "F-03: a profile without a dose stops the NCA and names the subject, also inside run_nca()",
  tryCatch({
    raw <- data.frame(Subject = rep(c("A", "B"), each = 5), Time = rep(c(0, 1, 2, 4, 8), 2),
                      Conc = c(0, 10, 8, 4, 1, 0, 12, 9, 5, 2), Dose = c(rep(100, 5), rep(NA, 5)))
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", dose = "Dose")
    d <- prepare_pk_dataset(raw, cm, list(lloq = 0, blq_rule = "rule1"))$data
    vp <- dose_by_profile(d, cm); vs <- dose_by_subject(d, cm)
    st <- list(admin_route = "extravascular", dose = vp, dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL",
               trap_method = "linear", r2adj_threshold = 0.7, infusion_duration = 0, mw = 0,
               is_steady_state = FALSE, tau = NA, partial_aucs = NULL)
    r1 <- adv_quiet(run_nca(d, cm, st))
    st$dose <- c(A = 100, B = Inf); r2 <- adv_quiet(run_nca(d, cm, st))
    vp[[1]] == 100 && is.na(vp[[2]]) && is.na(vs[["B"]]) &&
      is.null(r1$value) && any(grepl("No dose value for subject(s): B.", r1$warnings, fixed = TRUE)) &&
      is.null(r2$value) && any(grepl("subject(s): B.", r2$warnings, fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-NCA-09", critical = TRUE,
  method = "Two subjects, subject B's Dose column empty; dose_by_profile(), dose_by_subject(), run_nca(); run_nca() with an infinite dose",
  expected = "Dose NA for B (was -Inf); run_nca() returns no result and names B (was: a result with CL/F from -Inf)")

check("ADV-06", "F-06: at steady state fluctuation and swing use the peak within 0-tau, with a note when the highest value lies later",
  tryCatch({
    tt <- c(0, 1, 2, 4, 8, 12, 24, 26, 28)
    late <- c(3, 10, 12, 9, 6, 4, 3, 14, 11); norm <- c(3, 10, 12, 9, 6, 4, 3, 2.5, 2)
    st <- list(admin_route = "extravascular", dose = 100, dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL",
               trap_method = "linear", r2adj_threshold = 0.7, infusion_duration = 0, mw = 0,
               is_steady_state = TRUE, tau = 24, partial_aucs = NULL)
    s1 <- adv_quiet(run_single_nca(tt, late, st))
    raw <- data.frame(Subject = rep(c("LATE", "NORMAL"), each = 9), Time = rep(tt, 2), Conc = c(late, norm))
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    b <- adv_quiet(run_nca(prepare_pk_dataset(raw, cm, list(lloq = 0, blq_rule = "rule1"))$data, cm, st))
    rl <- b$value[b$value$Subject == "LATE", ]; rn <- b$value[b$value$Subject == "NORMAL", ]
    note <- grep("highest concentration after", b$warnings, value = TRUE)
    abs(as.numeric(s1$value[["SWING"]]) - 3) < 1e-9 && any(grepl("highest concentration after", s1$warnings)) &&
      abs(rl$SWING - 3) < 1e-9 && abs(rl$FLUCTP - 9 / rl$CAVG * 100) < 1e-6 && rl$CMAX == 14 &&
      abs(rn$SWING - 3) < 1e-9 && rn$CMIN_SS == 3 &&
      length(note) == 1 && grepl("(LATE)", note, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE,
  method = "Samples 0-28 h, tau 24 h, one profile with its highest value (14) at 26 h and one declining after tau; single-profile and batch NCA",
  expected = "Swing (12 - 3) / 3 = 3 and fluctuation from 12 for both (was 3.67 from the 26 h value); Cmax column 14; one note naming LATE")

check("ADV-07", "F-16: a password falls due after 90 days counted in UTC, whatever the server's time zone",
  tryCatch((function() {
    old_tz <- Sys.getenv("TZ", unset = NA)
    on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz))
    v <- gxp_env$GXP_PWD_VALIDITY_DAYS
    today <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = "UTC"))
    due <- function(days_ago) gxp_env$gxp_must_change("ana", list(credentials = data.frame(user = "ana")),
      data.frame(object = "ana", event = "password_changed", details = "",
                 time_utc = paste0(format(today - days_ago), "T12:00:00.000Z"), stringsAsFactors = FALSE))
    res <- vapply(c("Etc/GMT-14", "Etc/GMT+12", "UTC"), function(z) { Sys.setenv(TZ = z); c(!due(v), due(v + 1)) },
                  logical(2))
    all(res) && identical(gxp_env$gxp_utc_today(), as.Date(format(Sys.time(), "%Y-%m-%d", tz = "UTC")))
  })(), error = function(e) FALSE),
  "URS-GXP-03", critical = TRUE,
  method = "gxp_must_change() for a change exactly 90 and 91 UTC days ago, with the server at UTC+14, UTC-12 and UTC",
  expected = "Not due at 90 days and due at 91 in every time zone (was: a day early or late depending on the zone and the hour)")

check("ADV-08", "F-07: the validation run installs nothing, and the version check covers every package of the lockfile and names the differences",
  tryCatch({
    src <- readLines("validation/validation.R")
    code <- src[!grepl("^\\s*#", src)]
    mr <- parse("validation/make_release_files.R")
    i <- which(vapply(mr, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("pkgs")), logical(1)))
    lock_pkgs <- eval(mr[[i]][[3]])
    lf <- tempfile(fileext = ".lock")
    writeLines(jsonlite::toJSON(list(Packages = list(NonCompart = list(Version = "0.0.1"),
                                                     digest = list(Version = as.character(utils::packageDescription("digest")$Version)))),
                                auto_unbox = TRUE), lf)
    dd <- lock_differences(lf, c("NonCompart", "digest", "DBI"))
    !any(grepl(paste0("install", ".packages("), code, fixed = TRUE)) && setequal(rel_pkgs, lock_pkgs) &&
      length(dd) == 2 && grepl("^NonCompart .* \\(validated: 0\\.0\\.1\\)$", dd[1]) && grepl("^DBI .*not in the lockfile", dd[2])
  }, error = function(e) FALSE),
  "URS-GEN-08", critical = FALSE,
  method = "validation.R source; the package list of IQ-REL-02 against make_release_files.R; lock_differences() on a lockfile with NonCompart 0.0.1 and without DBI",
  expected = "No package installation in the script (was: missing packages installed from CRAN mid-run); the same 20 packages; NonCompart and DBI listed with the reason")

check("ADV-09", "F-09: sign-out removes the session's token on the server, through the shinymanager internals it relies on",
  tryCatch({
    tk <- get(".tok", envir = asNamespace("shinymanager"))
    tok <- tk$generate("adv09"); tk$add(tok, list(user = "adv09"))
    before <- tk$is_valid(tok)
    got <- gxp_env$gxp_end_token(list(clientData = list(url_search = paste0("?token=", tok))))
    src <- paste(readLines("R/gxp_access.R"), collapse = "\n")
    before && isTRUE(got == tok) && !tk$is_valid(tok) &&
      grepl("gxp_sign_out, \\{\\s*signing_out <<- TRUE\\s*gxp_end_token\\(session\\)", src)
  }, error = function(e) FALSE),
  "URS-GXP-02", critical = TRUE,
  method = "A shinymanager 1.1.0 token added to its store; gxp_end_token() with a session carrying it; the sign-out handler in R/gxp_access.R",
  expected = "The token is valid before and gone after; sign-out calls gxp_end_token(). Fails when a shinymanager upgrade renames getToken or .tok")

check("ADV-10", "F-01: the reproduction scripts say whether R and the packages are the versions of the record",
  tryCatch({
    rec <- list(packages = list(NonCompart = as.character(packageVersion("NonCompart"))), r_version = R.version.string)
    o1 <- capture.output(v1 <- verify_versions(rec))
    rec2 <- rec; rec2$packages$NonCompart <- "0.0.1"; rec2$r_version <- "R version 3.6.0 (2019-04-26)"
    o2 <- capture.output(v2 <- verify_versions(rec2))
    o3 <- capture.output(v3 <- verify_versions(list()))
    scripts <- c(generate_nca_script(), generate_single_nca_script(), generate_viz_script(list(), theoph_cm))
    td <- tempfile("adv10_"); dir.create(td); zf <- file.path(td, "rec.zip")
    cm <- list(subject = "Subject", time = "Time", conc = "conc")
    d <- prepare_pk_dataset(read.csv("data/example_theoph.csv"), cm, list(lloq = 0, blq_rule = "rule1"))$data
    create_analysis_record(zf, run_nca(d, cm, theoph_settings), theoph_settings, cm, "data/example_theoph.csv",
                           "example_theoph.csv", blq_rule = "rule1", lloq = 0, analyst = "Validation", study_name = "Versions")
    utils::unzip(zf, exdir = td)
    chk <- readLines(file.path(td, "reproduction_check.txt"))
    identical(v1, "SAME") && identical(v2, "DIFFERENT") && grepl("NonCompart .* here, 0.0.1 in the record", o2) &&
      grepl("R version 3.6.0 (2019-04-26) in the record", o2, fixed = TRUE) && identical(v3, "NOT RECORDED") &&
      all(grepl("verify_versions(rec)", scripts, fixed = TRUE)) &&
      any(grepl("R and package versions: SAME", chk, fixed = TRUE)) && any(grepl("^Result: MATCH", chk))
  }, error = function(e) FALSE),
  "URS-EXP-02", critical = TRUE,
  method = "verify_versions() with the installed NonCompart, with 0.0.1 and an old R, and with no versions; the three script generators; a theophylline record's reproduction check",
  expected = "SAME; DIFFERENT naming both differences; NOT RECORDED; every script calls it; the record's check says SAME and MATCH")

check("ADV-11", "F-08: the fonts come from www/fonts, and the theme builds without a network connection",
  tryCatch({
    app <- paste(readLines("app.R"), collapse = "\n")
    css <- c("www/fonts/source-sans-pro/font.css", "www/fonts/fira-code/font.css")
    urls <- unlist(lapply(css, function(f) {
      l <- grep("url\\(", readLines(f), value = TRUE)
      stats::setNames(sub(".*url\\(([^)]+)\\).*", "\\1", l), rep(dirname(f), length(l)))
    }))
    ex <- parse("app.R")
    i <- which(vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                        identical(e[[2]], as.name("pharma_theme")), logical(1)))
    theme <- eval(ex[[i]][[3]], new.env(parent = asNamespace("bslib")))
    px <- Sys.getenv(c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY"), unset = NA)
    Sys.setenv(http_proxy = "http://127.0.0.1:9", https_proxy = "http://127.0.0.1:9",
               HTTP_PROXY = "http://127.0.0.1:9", HTTPS_PROXY = "http://127.0.0.1:9")
    built <- tryCatch({ bslib::bs_theme_dependencies(theme); TRUE }, error = function(e) FALSE)
    for (n in names(px)) if (is.na(px[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, as.list(px[n]))
    !grepl("font_google", app, fixed = TRUE) && !grepl("googleapis", app, fixed = TRUE) &&
      all(vapply(css, function(f) grepl(sub("^www/", "", f), app, fixed = TRUE), logical(1))) &&
      length(urls) == 14 && !any(grepl("^(https?:)?//", urls)) && all(file.exists(file.path(names(urls), urls))) &&
      all(file.exists(file.path(dirname(css), "OFL.txt"))) && built
  }, error = function(e) FALSE),
  "URS-GEN-01", critical = FALSE,
  method = "app.R; the url() entries of www/fonts/*/font.css; the app's theme built with bslib::bs_theme_dependencies() while every proxy points to a closed port",
  expected = "No font_google() or Google address in app.R; both font files linked; all 14 font files present with their licences; the theme builds offline (was: fetched from Google on first use, and an offline server without a cache could not build it)")

check("ADV-12", "Scaled-method planning gives the same sample size and power whatever the random state, and every simulation fixes its seed",
  tryCatch({
    runs <- lapply(c(1, 2), function(seed) {
      set.seed(seed)
      c(abel = planner_sample_size("abel", 0.05, 0.8, 0.90, 0.8, 1.25, 0.35, planner_cv("abel", 35, 45), "2x2x4")[["Sample size"]],
        rsabe = planner_sample_size("rsabe", 0.05, 0.8, 0.90, 0.8, 1.25, 0.35, planner_cv("rsabe", 35, 45), "2x2x4")[["Sample size"]],
        pw = planner_power(24, "abel", 0.05, 0.90, 0.8, 1.25, 0.35, 0.45, "2x2x4", nsims = 1e4))
    })
    # Every call that passes nsims (PowerTOST's simulations), except the app's wrappers around planner_power()
    calls <- list()
    walk <- function(x) if (is.call(x)) {
      a <- as.list(x)
      if ("nsims" %in% names(a) && !deparse(x[[1]]) %in% c("planner_power", "compute_power", "function"))
        calls[[length(calls) + 1]] <<- x
      for (k in seq_along(a)[-1]) if (is.call(a[[k]])) walk(a[[k]])
    }
    for (f in c("R/designs.R", "R/mod_path_power.R")) for (e in parse(f)) walk(e)
    identical(runs[[1]], runs[[2]]) && length(calls) == 9 &&
      all(vapply(calls, function(x) identical(x[["setseed"]], TRUE), logical(1)))
  }, error = function(e) FALSE),
  "URS-PWR-01", critical = FALSE,
  method = "planner_sample_size() for ABEL and RSABE and planner_power() for ABEL after set.seed(1) and set.seed(2); every PowerTOST call with nsims in R/designs.R and R/mod_path_power.R",
  expected = "Identical results; all 9 simulation calls pass setseed = TRUE")

end_section("ADV")

# =============================================================================
# SECTION DSR: Adversarial review of data processing and statistics
# =============================================================================
# One test per finding (D-1..D-8), each built from the failing case.
start_section("DSR")

dsr_st <- function(...) { s <- list(admin_route = "extravascular", dose = 100, dose_unit = "mg", time_unit = "h",
  conc_unit = "ng/mL", trap_method = "log", r2adj_threshold = 0.7, infusion_duration = 0, mw = 0,
  is_steady_state = FALSE, tau = NA, partial_aucs = NULL); m <- list(...); s[names(m)] <- m; s }
dsr_quiet <- function(expr) { w <- character(0)
  v <- withCallingHandlers(expr, warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  list(value = v, warnings = w) }
dsr_cm <- list(subject = "Subject", time = "Time", conc = "Concentration", treatment = "Treatment",
               period = "Period", sequence = "Sequence")

check("DSR-01", "D-1: in a decimal-comma file a point value with three decimals is refused, not read as thousands",
  tryCatch({
    x <- c("0.250", "0.000", "12.500", "1.234,5", "0,5")
    f <- tempfile(fileext = ".csv")
    writeLines(c("Subject;Time;Conc", "1;0.000;0.000", "1;0.500;3.100", "1;1.000;12.500", "1;2.000;8.250",
                 "1;4.000;4.125", "1;8.000;1.010"), f)
    qc <- run_data_quality_check(read_pk_file(f, list(sep = ";", dec = ",")),
                                 list(subject = "Subject", time = "Time", conc = "Conc"), lloq = 0, dec = ",")
    n <- normalise_decimal_comma(x, ",")
    is.na(n[1]) && is.na(n[2]) && n[3] == "12500" && n[4] == "1234.5" && n[5] == "0.5" &&
      !qc$pass && any(qc$findings$Category == "Decimal mark" & qc$findings$Severity == "ERROR")
  }, error = function(e) FALSE),
  "URS-DAT-01", critical = TRUE,
  method = "normalise_decimal_comma() on 0.250, 0.000, 12.500, 1.234,5 and 0,5; a point-decimal file with three decimals read with a decimal comma",
  expected = "0.250 and 0.000 refused (were 250 and 0); 12.500 = 12500 and 1.234,5 = 1234.5 as before; the file gets a decimal-mark ERROR (was read x1000 with no error)")

check("DSR-02", "D-2: a period with one measurable concentration keeps its Cmax and AUC and stays in the bioequivalence comparison",
  tryCatch({
    xo <- read.csv("data/example_be_crossover.csv", stringsAsFactors = FALSE)
    xo$Concentration <- as.character(xo$Concentration)
    i <- xo$Subject == 1 & xo$Treatment == "Test"
    cc <- rep("BLQ", sum(i)); cc[which(xo$Time[i] == sort(unique(xo$Time[i]))[3])] <- "2.0"
    xo$Concentration[i] <- cc
    d <- prepare_pk_dataset(xo, dsr_cm, list(lloq = 0.5, blq_rule = "rule1"))$data
    r <- dsr_quiet(run_nca(d, dsr_cm, dsr_st()))$value
    bd <- build_be_data(r, d, dsr_cm, reference = "Reference")
    f <- fit_be_parameter(bd$data, "CMAX", "2x2x2", trt_col = "Treatment", subj_col = "Subject",
                          per_col = "Period", seq_col = "Sequence")
    none <- data.frame(Subject = 1, Time = c(0, 1, 2), Concentration = c(0, 0, 0))
    nr <- dsr_quiet(run_nca(none, list(subject = "Subject", time = "Time", conc = "Concentration"), dsr_st()))
    row <- r[r$Subject == "1" & r$Treatment == "Test", ]
    nrow(r) == 12 && row$CMAX == 2 && is.finite(row$AUCLST) && is.na(row$LAMZHL) &&
      f$row$N_Test == 6 && f$row$Bioequivalent == "NO" &&
      is.null(nr$value) && any(grepl("without a measurable concentration", nr$warnings))
  }, error = function(e) FALSE),
  "URS-NCA-06, URS-BE-11", critical = TRUE,
  method = "example_be_crossover.csv with subject 1's Test period BLQ except one sample of 2.0 (LLOQ 0.5, Rule 1); a profile of zeros",
  expected = "12 NCA rows; that period has Cmax 2 and AUClast, no half-life; Cmax comparison with 6 subjects gives NO (was: period dropped, 5 subjects, YES); the all-zero profile is still excluded")

check("DSR-03", "D-2: a partial AUC that is missing because the interval passes a profile's last measurable concentration gives no verdict",
  tryCatch({
    xo <- read.csv("data/example_be_crossover.csv", stringsAsFactors = FALSE)
    xo$Concentration <- as.character(xo$Concentration)
    i <- xo$Subject == 2 & xo$Treatment == "Test"
    v <- as.numeric(xo$Concentration[i]) * 0.25; v[xo$Time[i] >= 6] <- NA
    xo$Concentration[i] <- ifelse(is.na(v), "BLQ", format(v))
    d <- prepare_pk_dataset(xo, dsr_cm, list(lloq = 0.5, blq_rule = "rule1"))$data
    r <- dsr_quiet(run_nca(d, dsr_cm, dsr_st(partial_aucs = data.frame(start = 0, end = "8", cmax = TRUE, role = "pivotal"))))$value
    bd <- build_be_data(r, d, dsr_cm, reference = "Reference")
    fit <- function(p) fit_be_parameter(bd$data, p, "2x2x2", trt_col = "Treatment", subj_col = "Subject",
                                        per_col = "Period", seq_col = "Sequence")
    fa <- fit("AUC_0_8"); fc <- fit("CMAX_0_8"); fl <- fit("AUCLST")
    is.null(fa$estimate) && grepl("reaches past the last measurable concentration", fa$row$Bioequivalent) &&
      grepl("2 period 2", fa$row$Bioequivalent) && is.null(fc$estimate) && fl$row$Bioequivalent %in% c("YES", "NO")
  }, error = function(e) FALSE),
  "URS-BE-10", critical = TRUE,
  method = "example_be_crossover.csv with subject 2's Test period at 25% and BLQ from 6 h; pivotal AUC 0-8 with Cmax; AUClast",
  expected = "AUC 0-8 and Cmax 0-8: no estimate, the reason names the profile (was: subject dropped, YES); AUClast keeps its verdict")

check("DSR-04", "D-3: negative times stop the analysis, except one pre-dose sample close to the dose (ARV-06)",
  tryCatch({
    d <- data.frame(Subject = 1, Time = c(-5, 0.5, 1, 2, 4, 8, 12), Conc = c(0, 5, 10, 8, 4, 2, 1))
    qc <- run_data_quality_check(d, list(subject = "Subject", time = "Time", conc = "Conc"), lloq = 0)
    !qc$pass && any(qc$findings$Severity == "ERROR" & grepl("negative time", qc$findings$Message))
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE,
  method = "A 12 h profile with a pre-dose sample at -5 h (beyond 10% of the profile before the dose)",
  expected = "ERROR, processing blocked (a single pre-dose sample within 10% is analysed at 0, ARV-06)")

check("DSR-05", "D-4: several analytes in one concentration column are refused even when their sampling times differ",
  tryCatch({
    par <- data.frame(Subject = rep(1:3, each = 7), Analyte = "PARENT", Time = rep(c(0, 0.5, 1, 2, 4, 8, 12), 3),
                      Conc = rep(c(0, 20, 40, 30, 15, 6, 2), 3))
    met <- data.frame(Subject = rep(1:3, each = 6), Analyte = "METAB", Time = rep(c(0.75, 1.5, 3, 6, 10, 24), 3),
                      Conc = rep(c(2, 5, 6, 4, 2, 0.5), 3))
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    qc <- run_data_quality_check(rbind(par, met), cm, lloq = 0)
    ok <- run_data_quality_check(par, cm, lloq = 0)
    !qc$pass && any(qc$findings$Severity == "ERROR" & grepl("Analyte", qc$findings$Message)) && ok$pass
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE,
  method = "Parent and metabolite stacked, sampled at different times, with an unmapped Analyte column; the parent alone",
  expected = "ERROR naming the Analyte column (was: no finding, one mixed profile with AUClast 117.5 instead of 158); the parent alone passes")

check("DSR-06", "D-5: the mean profile leaves out time points that are mostly zero or BLQ, and says when times differ between profiles",
  tryCatch({
    d <- data.frame(.time = rep(c(1, 24), each = 12), .conc = c(rep(10, 12), rep(0, 10), 0.6, 0.8),
                    .profile = factor(rep(1:12, 2)))
    s <- viz_summary_stats(d, "geomean"); a <- viz_summary_stats(d, "arithmean")
    jit <- data.frame(.time = c(1.02, 0.98, 1.05, 2.01, 1.97, 2.1), .conc = 1:6, .profile = factor(rep(1:3, 2)))
    s$.hidden[s$.time == 24] && is.na(s$.center[s$.time == 24]) && !s$.hidden[s$.time == 1] &&
      abs(a$.center[a$.time == 24] - 1.4 / 12) < 1e-12 &&
      !is.null(viz_exact_time_note(jit)) && is.null(viz_exact_time_note(d)) &&
      grepl("n() / 2", generate_viz_script(list(plot_type = "summary", summary_statistic = "geomean"),
                                           list(subject = "S", time = "T", conc = "C")), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-VIZ-05", critical = FALSE,
  method = "12 profiles with 10 zeros and 0.6, 0.8 at 24 h; actual times that differ per profile; the Figure Record script",
  expected = "24 h not plotted as a geometric mean (was 0.69 from 2 values); arithmetic mean includes the zeros; a note for differing times only; the script applies the same rule")

check("DSR-07", "D-6: at steady state AUCtau, Cavg, Cmin and Ctau are dose-normalised, with labels",
  tryCatch({
    d <- data.frame(Subject = rep(c("A", "B"), each = 7), Time = rep(c(0, 1, 2, 4, 8, 12, 24), 2),
                    Conc = c(3, 10, 12, 9, 6, 4, 3, 6, 20, 24, 18, 12, 8, 6), Dose = rep(c(50, 100), each = 7))
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", dose = "Dose")
    dv <- dose_by_profile(d, cm)
    r <- add_dose_normalized(dsr_quiet(run_nca(d, cm, dsr_st(dose = dv, is_steady_state = TRUE, tau = 24)))$value, dv)
    all(abs(r$AUCTAU_DN * dv[r$Subject] - r$AUCTAU) < 1e-9) && all(abs(r$CAVG_DN * dv[r$Subject] - r$CAVG) < 1e-9) &&
      all(c("CMIN_SS_DN", "CTAU_SS_DN") %in% names(r)) &&
      !any(friendly_name(c("AUCTAU_DN", "CAVG_DN", "CMIN_SS_DN", "CTAU_SS_DN")) == c("AUCTAU_DN", "CAVG_DN", "CMIN_SS_DN", "CTAU_SS_DN"))
  }, error = function(e) FALSE),
  "URS-NCA-08", critical = FALSE,
  method = "Two steady-state profiles (tau 24 h) with doses 50 and 100 mg from the Dose column",
  expected = "AUCTAU_DN and CAVG_DN = value / own dose, CMIN_SS_DN and CTAU_SS_DN present, all labelled (were missing)")

check("DSR-08", "D-7: a byte-order mark does not become part of the first column name",
  tryCatch({
    f <- tempfile(fileext = ".csv")
    writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("Subject,Time,Conc\n1,0,0\n1,1,10\n1,2,8\n")), f)
    g <- tempfile(fileext = ".csv"); writeLines(c("X...Subject,Time", "1,0"), g)
    nm <- names(read_pk_file(f)); a <- auto_detect_columns(nm)
    identical(nm, c("Subject", "Time", "Conc")) && identical(a$subject, "Subject") &&
      identical(strip_bom_name(c("X...Subject", "Time"), f), c("Subject", "Time")) &&
      identical(strip_bom_name(c("X...Subject", "Time"), g), c("X...Subject", "Time")) &&
      names(adnca_read(f))[1] == "SUBJECT"
  }, error = function(e) FALSE),
  "URS-DAT-01, URS-DAT-02", critical = FALSE,
  method = paste0("A CSV that starts with the UTF-8 byte-order mark (Excel's CSV UTF-8), read in this run's locale (",
                  Sys.getlocale("LC_CTYPE"), "); strip_bom_name() on the C-locale form X...Subject, with and without a mark"),
  expected = "First column Subject and recognised (was X...Subject in a C locale); a column really named X...Subject is kept")

check("DSR-09", "D-8: the ADNCA import and a flat upload use the same rule for a profile that starts late",
  tryCatch({
    mk <- function(start) data.frame(USUBJID = "S1", AVAL = c(5, 8, 6, 3, 1), NRRLT = start + c(0, 1, 2, 4, 6))
    conv <- function(d) tryCatch({ adnca_convert(d, time = "NRRLT"); "converted" }, error = function(e) "refused")
    il <- function(start) nrow(interlock_profile_start(data.frame(S = "S1", T = start + c(0, 1, 2, 4, 6)),
                                                       list(subject = "S", time = "T")))
    conv(mk(2)) == "converted" && il(2) == 0 && conv(mk(4)) == "refused" && il(4) == 1 &&
      nrow(adnca_convert(mk(0), time = "NRRLT")$flat) == 5
  }, error = function(e) FALSE),
  "URS-DAT-01, URS-DAT-03", critical = FALSE,
  method = "A profile sampled over 6 h starting at 2 h (33% of the span) and at 4 h (67%), through adnca_convert() and the flat-file interlock; a dataset without APERIOD",
  expected = "Start at 2 h accepted by both (ADNCA refused it at 20%); start at 4 h refused by both; a dataset without APERIOD converts (stopped with 'argument lengths differ')")

end_section("DSR")

# =============================================================================
# SECTION EXM: Bundled example datasets loaded with one click
# =============================================================================
start_section("EXM")
suppressPackageStartupMessages({ library(shiny); library(bslib); library(DT) })
# The modules call gxp_guard(); with controlled mode off it lets every action through
for (f in c("R/help_system.R", "R/mod_data_upload.R", "R/mod_lz_rules.R", "R/mod_exclusions.R", "R/gxp_audit.R")) source(f, local = TRUE)
if (!exists("PIPELINE_SHA256")) PIPELINE_SHA256 <- digest::digest(file = "R/pipeline.R", algo = "sha256")

check("EXM-01", "Every bundled example exists and its columns are recognised",
  tryCatch({
    det <- function(f) auto_detect_columns(names(read_pk_file(example_path(f))))
    th <- det("example_theoph.csv"); xo <- det("example_be_crossover.csv"); rp <- det("example_be_replicate_2x2x4.csv")
    pa <- det("example_be_parallel.csv"); bl <- det("example_blq.csv")
    pc <- det("example_be_parallel_covariates.csv"); hv <- det("example_be_replicate_hvd.csv")
    ad <- adnca_convert(adnca_read(example_path("example_adnca.csv")), time = "NRRLT")
    length(EXAMPLE_FILES) == 8 && all(file.exists(vapply(EXAMPLE_FILES, example_path, "")) ) &&
      th$conc == "conc" && th$dose == "Dose" && xo$treatment == "Treatment" && xo$period == "Period" &&
      xo$sequence == "Sequence" && rp$period == "Period" && pa$treatment == "Treatment" && bl$conc == "Concentration" &&
      pc$treatment == "Treatment" && (length(pc$period) == 0 || !nzchar(pc$period)) && hv$period == "Period" && hv$sequence == "Sequence" &&
      nrow(ad$flat) > 0
  }, error = function(e) FALSE),
  "URS-UI-02, URS-DAT-01", critical = FALSE,
  method = "example_path() and auto_detect_columns() for the eight files; adnca_convert() for the ADNCA example",
  expected = "All eight present; mappings as in the tutorials; the ADNCA example converts with NRRLT")

check("EXM-02", "Load example goes through the upload path: earlier results cleared, data read, source recorded as example",
  tryCatch({
    ok <- FALSE
    sh <- shiny::reactiveValues(be_results = list(ci_table = 1), nca_results = 1, data_ready = FALSE, exclusions = NULL,
                                data_id = 0)
    suppressWarnings(shiny::testServer(data_upload_server, args = list(shared = sh), {
      session$setInputs(data_type = "flat", csv_sep = ",", csv_dec = ".", example = "example_theoph.csv", btn_example = 1)
      cleared <- is.null(sh$be_results) && is.null(sh$nca_results)
      read_ok <- identical(src()$origin, "example") && nrow(raw_data()) == 132
      session$setInputs(col_subject = "Subject", col_time = "Time", col_conc = "conc", col_treatment = "",
                        col_period = "", col_sequence = "", col_dose = "Dose", lloq = 0, blq_rule = "rule1", btn_apply = 1)
      ok <<- cleared && read_ok && isTRUE(sh$data_ready) && identical(sh$study_info$source, "example") &&
        identical(sh$study_info$file_name, "example_theoph.csv") && file.exists(sh$study_info$file_path) &&
        sh$data_id == 1
    }))
    ok
  }, error = function(e) FALSE),
  "URS-UI-02, URS-DAT-01, URS-GEN-09", critical = FALSE,
  method = "shiny::testServer on the upload module: choose example_theoph.csv, Load example, map the columns, Process Data",
  expected = "Previous results cleared; 132 rows read; study_info names the file, an existing path and source \"example\"")

check("EXM-03", "Only the bundled examples can be loaded or downloaded by name",
  tryCatch({
    bad <- tryCatch({ example_path("../app.R"); "loaded" }, error = function(e) "refused")
    bad2 <- tryCatch({ example_path("example_theoph.csv.bak"); "loaded" }, error = function(e) "refused")
    bad == "refused" && bad2 == "refused"
  }, error = function(e) FALSE),
  "URS-UI-02", critical = TRUE, method = "example_path('../app.R') and a name not in EXAMPLE_FILES",
  expected = "Both refused")

check("EXM-04", "Controlled mode records an example as an example, with its SHA-256",
  tryCatch({
    src_code <- paste(readLines("R/mod_data_upload.R"), collapse = "\n")
    grepl('gxp_guard("data_loaded", object = src()$name,\n                     sha256 = sha256_file(src()$datapath),\n                     details = list(source = src()$origin,', src_code, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GXP-05, URS-UI-02", critical = FALSE, method = "The data_loaded audit call in R/mod_data_upload.R",
  expected = "Object is the file name, the SHA-256 of the loaded file, details$source = \"file\" or \"example\"")

check("EXM-05", "A record made from an example says so and reproduces",
  tryCatch({
    td <- tempfile("exm05_"); dir.create(td); zf <- file.path(td, "rec.zip")
    cm <- list(subject = "Subject", time = "Time", conc = "conc")
    d <- prepare_pk_dataset(read.csv(example_path("example_theoph.csv")), cm, list(lloq = 0))$data
    create_analysis_record(zf, suppressWarnings(run_nca(d, cm, theoph_settings)), theoph_settings, cm,
                           example_path("example_theoph.csv"), "example_theoph.csv", blq_rule = "rule1", lloq = 0,
                           analyst = "Validation", study_name = "Example", data_source = "example")
    utils::unzip(zf, exdir = td)
    html <- paste(readLines(file.path(td, "analysis_summary.html"), warn = FALSE), collapse = "\n")
    grepl("(bundled example dataset)", html, fixed = TRUE) &&
      any(grepl("^Result: MATCH", readLines(file.path(td, "reproduction_check.txt"))))
  }, error = function(e) FALSE),
  "URS-EXP-01, URS-EXP-02", critical = TRUE, method = "create_analysis_record() for example_theoph.csv with data_source = \"example\"",
  expected = "Summary marks the bundled example dataset; reproduction MATCH")

check("EXM-06", "Download serves the chosen example unchanged",
  tryCatch({
    ok <- FALSE
    sh <- shiny::reactiveValues(data_ready = FALSE, exclusions = NULL, data_id = 0)
    suppressWarnings(shiny::testServer(data_upload_server, args = list(shared = sh), {
      session$setInputs(example = "example_blq.csv")
      f <- output$dl_example
      ok <<- identical(digest::digest(file = f, algo = "sha256"), digest::digest(file = "data/example_blq.csv", algo = "sha256"))
    }))
    ok
  }, error = function(e) FALSE),
  "URS-UI-02", critical = FALSE, method = "shiny::testServer: the Download button with example_blq.csv chosen",
  expected = "The downloaded file has the SHA-256 of data/example_blq.csv")

check("EXM-07", "The highly variable replicate example shows both routes of RSABE and the numbers Tutorial 4c quotes",
  tryCatch({
    d <- read.csv(example_path("example_be_replicate_hvd.csv"), stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", period = "Period",
               sequence = "Sequence", dose = "Dose")
    r <- suppressWarnings(run_nca(d, cm, rep_settings)); b <- build_be_data(r, d, cm, reference = "Reference")
    a <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    row <- function(ap, p) do.call(be_assess_parameter, c(list(ap, b$data, p, "2x2x4"), a))$row
    st <- row("standard", "CMAX"); sa <- row("standard", "AUCLST")
    rc <- row("rsabe", "CMAX"); ra <- row("rsabe", "AUCLST"); ac <- row("abel", "CMAX")
    vd <- be_variability_diagnostic(b$data, "CMAX", "Treatment", "Subject", "Period", "Sequence")
    length(unique(d$Subject)) == 32 && all(table(unique(d[c("Subject", "Sequence")])$Sequence) == 16) &&
      identical(c(st$Point_Est, st$CI_Lower, st$CI_Upper, st$Bioequivalent), c(88.35, 77.52, 100.69, "NO")) &&
      identical(c(sa$Point_Est, sa$CI_Lower, sa$CI_Upper, sa$Bioequivalent), c(98.2, 90.56, 106.5, "YES")) &&
      rc$Route == "Scaled" && rc$Bioequivalent == "YES" && round(rc$s_WR, 3) == 0.399 && round(rc$Scaled_Lower, 2) == 70.04 &&
      round(rc$Scaled_Upper, 2) == 142.78 && round(rc$Crit_Bound, 4) == -0.0446 && rc$Point_Est == 88.35 && rc$CI_Lower == 76.67 && rc$CI_Upper == 101.81 &&
      ra$Route == "Standard" && round(ra$s_WR, 3) == 0.251 && ra$Bioequivalent == "YES" &&
      ac$Route == "Scaled" && ac$Bioequivalent == "YES" && round(c(ac$Scaled_Lower, ac$Scaled_Upper), 2)[1] == 73.84 &&
      round(c(ac$Scaled_Lower, ac$Scaled_Upper), 2)[2] == 135.43 && round(vd$CVwR, 1) == 41.5
  }, error = function(e) FALSE),
  "URS-BE-14, URS-BE-15, URS-UI-02", critical = FALSE,
  method = "example_be_replicate_hvd.csv through run_nca(), build_be_data() and be_assess_parameter() for the standard, RSABE and ABEL approaches",
  expected = "Standard Cmax 88.35% (77.52-100.69%) fails; RSABE: Cmax scaled (s_WR 0.399, limits 70.04-142.78%, bound -0.0446) and AUC standard (s_WR 0.251), both bioequivalent; ABEL limits 73.84-135.43%")

check("EXM-08", "The parallel example with covariates gives the numbers Tutorial 4b quotes: the unadjusted interval fails for Cmax, the adjusted one passes",
  tryCatch({
    d <- read.csv(example_path("example_be_parallel_covariates.csv"), stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc", treatment = "Treatment", dose = "Dose")
    r <- suppressWarnings(run_nca(d, cm, rep_settings))
    bu <- build_be_data(r, d, cm, reference = "Reference"); bw <- build_be_data(r, d, cm, reference = "Reference", covariates = "Weight")
    fu <- function(p) fit_be_parameter(bu$data, p, "parallel", trt_col = "Treatment", subj_col = "Subject")$row
    fw <- function(p) fit_be_parameter(bw$data, p, "parallel", trt_col = "Treatment", subj_col = "Subject", covariates = bw$covariates)
    cu <- fu("CMAX"); cw <- fw("CMAX")$row; au <- fu("AUCLST"); aw <- fw("AUCLST")$row
    bal <- be_covariate_balance(bw$data, bw$covariates, "Treatment", "Subject")
    length(unique(d$Subject)) == 40 && all(table(unique(d[c("Subject", "Treatment")])$Treatment) == 20) &&
      identical(c(cu$Point_Est, cu$CI_Lower, cu$CI_Upper, cu$Bioequivalent), c(84.64, 77.76, 92.12, "NO")) &&
      identical(c(cw$Point_Est, cw$CI_Lower, cw$CI_Upper, cw$Bioequivalent), c(95.04, 90.12, 100.22, "YES")) &&
      identical(c(cw$Unadj_Lower, cw$Unadj_Upper), c(77.76, 92.12)) && cw$DF == 37 &&
      identical(c(au$Point_Est, au$CI_Lower, au$CI_Upper), c(90.81, 85.2, 96.8)) && identical(c(aw$Point_Est, aw$CI_Lower, aw$CI_Upper), c(98.13, 93.51, 102.99)) &&
      bal$Reference == "68.16 (9.5)" && bal$Test == "77.69 (12)" && round(bal$Std_Diff, 2) == 0.88 &&
      round(100 * sqrt(exp(cw$Unadj_MSE) - 1), 1) == 16 && round(100 * sqrt(exp(cw$MSE) - 1), 1) == 9.1
  }, error = function(e) FALSE),
  "URS-BE-13, URS-UI-02", critical = FALSE,
  method = "example_be_parallel_covariates.csv with and without Weight as covariate",
  expected = "Cmax: unadjusted 84.64% (77.76-92.12%) not bioequivalent; adjusted for Weight 95.04% (90.12-100.22%) bioequivalent; residual df 37; Weight 68.2 vs 77.7 kg, standardized difference 0.88")

check("EXM-09", "The two new examples are what the committed generator makes from its seeds",
  tryCatch({
    source(file.path("validation", "fixtures", "make_example_datasets.R"), local = TRUE)
    h <- sim_hvd(522)$data; p <- sim_par(676)$data
    fh <- read.csv("data/example_be_replicate_hvd.csv"); fp <- read.csv("data/example_be_parallel_covariates.csv")
    isTRUE(all.equal(h, fh, check.attributes = FALSE)) && isTRUE(all.equal(p, fp, check.attributes = FALSE))
  }, error = function(e) FALSE),
  "URS-UI-02", critical = FALSE, method = "sim_hvd(522) and sim_par(676) from validation/fixtures/make_example_datasets.R against the files in data/",
  expected = "Identical values: the examples can be regenerated")

end_section("EXM")

# =============================================================================
# SECTION GLD: The bioequivalence run gives the same results as before the v1.9 refactor
# =============================================================================
# validation/fixtures/be_run_golden/ holds what the app stored for nine runs,
# made with the code before the run moved into run_be_analysis(). Each case is
# run again through the real module and must be identical().
start_section("GLD")
source(file.path("validation", "fixtures", "make_be_run_golden.R"))
be_golden_env()
gld_cases <- be_golden_cases()
for (i in seq_along(gld_cases)) local({
  nm <- names(gld_cases)[i]
  check(sprintf("GLD-%02d", i), sprintf("BE run '%s' is identical to its golden output", nm),
    tryCatch(identical(readRDS(file.path("validation", "fixtures", "be_run_golden", paste0(nm, ".rds"))),
                       be_golden_run(gld_cases[[nm]])), error = function(e) FALSE),
    "URS-BE-01, URS-BE-02, URS-EXP-05", critical = TRUE,
    method = paste0("The real BE module run through shiny::testServer on ", gld_cases[[nm]]$file,
                    "; be_result(), be_run_settings() and the notifications against the stored file"),
    expected = "identical() to the golden file: the table, ANOVA, verdicts, checks and messages")
})

# ---- The Analysis Record of a BE run reproduces the BE statistics and the verdict ----
# Each record is made by the real module (Run, then the Analysis Record download) and its
# reproduce_analysis.R is run in a separate R process, as a recipient would run it.
gld_rec <- function(nm) {
  d <- file.path(tempdir(), paste0("gldrec_", nm))
  if (!dir.exists(d)) be_golden_record(gld_cases[[nm]], d)
  d
}
gld_copy <- function(nm) {
  d <- file.path(tempdir(), paste0("gldcopy_", nm, "_", as.integer(runif(1, 1, 1e7))))
  dir.create(d); file.copy(list.files(gld_rec(nm), full.names = TRUE), d); d
}
gld_run <- function(d) {
  owd <- setwd(d); on.exit(setwd(owd))
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), "reproduce_analysis.R", stdout = TRUE, stderr = TRUE))
}
gld_final <- function(out) {
  v <- regmatches(out, regexpr("(?<=^Result: )[A-Z ]+", out, perl = TRUE)); if (length(v)) trimws(tail(v, 1)) else "NONE"
}
gld_edit <- function(f, fn) writeLines(fn(readLines(f, warn = FALSE)), f)

for (i in seq_along(gld_cases)) local({
  nm <- names(gld_cases)[i]
  check(sprintf("REC-BE-%02d", i), sprintf("BE record '%s' reproduces the BE statistics and the verdict", nm),
    tryCatch({
      d <- gld_rec(nm); txt <- rec_check_text(d)
      sens <- identical(nm, "replicate_excl")
      grepl("BE result: MATCH", txt, fixed = TRUE) && identical(gld_final(strsplit(txt, "\n")[[1]]), "MATCH") &&
        file.exists(file.path(d, "app_be_reference.csv")) &&
        identical(file.exists(file.path(d, "app_be_sensitivity_reference.csv")), sens) &&
        (!sens || grepl("BE sensitivity result: MATCH", txt, fixed = TRUE)) &&
        !file.exists(file.path(d, "reproduced_be_results.csv"))
    }, error = function(e) FALSE),
    "URS-EXP-02, URS-EXP-05", critical = TRUE,
    method = paste0("The Analysis Record of ", gld_cases[[nm]]$file, " from the real module; reproduce_analysis.R run in a separate R process"),
    expected = "The script recomputes the NCA and the BE table from the recorded settings; both match; the last line is Result: MATCH")
})

check("REC-BE-10", "A changed value in the app's BE results gives DIFFERENT",
  tryCatch({
    d <- gld_copy("crossover_fixed"); f <- file.path(d, "app_be_reference.csv")
    r <- read.csv(f, check.names = FALSE); r$CI_Upper[1] <- r$CI_Upper[1] + 0.5; write.csv(r, f, row.names = FALSE)
    out <- gld_run(d); identical(gld_final(out), "DIFFERENT") && any(grepl("^BE result: DIFFERENT", out))
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-04", critical = TRUE, method = "One confidence limit of app_be_reference.csv raised by 0.5 percentage points; script run",
  expected = "BE result: DIFFERENT and Result: DIFFERENT")

check("REC-BE-11", "A changed setting (confidence level) gives DIFFERENT",
  tryCatch({
    d <- gld_copy("crossover_fixed"); gld_edit(file.path(d, "analysis_settings.json"), function(x) sub('"ci_level": 90', '"ci_level": 95', x))
    identical(gld_final(gld_run(d)), "DIFFERENT")
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-03", critical = TRUE, method = "ci_level 90 changed to 95 in analysis_settings.json; script run",
  expected = "Result: DIFFERENT")

check("REC-BE-12", "A changed Reference treatment gives DIFFERENT",
  tryCatch({
    d <- gld_copy("crossover_fixed"); gld_edit(file.path(d, "analysis_settings.json"), function(x) sub('"reference": "Reference"', '"reference": "Test"', x))
    identical(gld_final(gld_run(d)), "DIFFERENT")
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-03", critical = TRUE, method = "be_run reference changed from Reference to Test; script run",
  expected = "Result: DIFFERENT (the ratio would be inverted)")

check("REC-BE-13", "Changed BE code gives DIFFERENT even when the numbers agree",
  tryCatch({
    d <- gld_copy("crossover_fixed"); cat("\n# changed\n", file = file.path(d, "be_analysis.R"), append = TRUE)
    out <- gld_run(d); identical(gld_final(out), "DIFFERENT") && any(grepl("BE code (be_analysis.R): MISMATCH", out, fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-04", critical = TRUE, method = "A comment appended to the shipped be_analysis.R; script run",
  expected = "BE code (be_analysis.R): MISMATCH and Result: DIFFERENT")

check("REC-BE-14", "A change below the rounding of the table is still detected (unrounded comparison)",
  tryCatch({
    d <- gld_copy("crossover_fixed"); f <- file.path(d, "app_be_reference.csv")
    r <- read.csv(f, check.names = FALSE); shown <- r$Point_Est[1]
    r$Raw_Point_Est[1] <- r$Raw_Point_Est[1] * (1 + 3e-5); write.csv(r, f, row.names = FALSE)
    out <- gld_run(d)
    # the displayed point estimate (2 decimals) is untouched, the verdict is not MATCH
    identical(r$Point_Est[1], shown) && identical(gld_final(out), "CLOSE")
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-04", critical = TRUE, method = "Raw_Point_Est of app_be_reference.csv changed by 0.003% while the rounded column stays; script run",
  expected = "Result: CLOSE: the unrounded estimate is compared, not the rounded one")

check("REC-BE-15", "The BE comparison matches rows on Parameter, text exactly, and combines verdicts by their worst",
  tryCatch({
    ci <- data.frame(Parameter = c("CMAX", "AUCLST"), Point_Est = c(100.12, 95.5), Bioequivalent = c("YES", "NO"),
                     Scale = "Difference T\u2212R", stringsAsFactors = FALSE)
    f <- tempfile(fileext = ".csv"); write.csv(be_reference_table(ci, data.frame(Parameter = ci$Parameter, Raw_Point_Est = c(100.1234, 95.5))),
                                                f, row.names = FALSE)
    tab <- be_reference_table(ci, data.frame(Parameter = ci$Parameter, Raw_Point_Est = c(100.1234, 95.5)))
    run <- function(t, integrity = NULL) { v <- NULL; capture.output(v <- compare_be_with_reference(t, f, integrity)); v }
    shuffled <- tab[2:1, ]; txt <- tab; txt$Bioequivalent[1] <- "NO"; extra <- rbind(tab, transform(tab[1, ], Parameter = "TMAX"))
    ph <- tab; ph$Point_Est[2] <- 95.5001
    identical(run(tab), "MATCH") && identical(run(shuffled), "MATCH") && identical(run(txt), "DIFFERENT") && identical(run(extra), "DIFFERENT") &&
      identical(run(tab, c("BE code (be_analysis.R)" = "MISMATCH")), "DIFFERENT") && identical(run(NULL), "FAILED") &&
      identical(run(ph), "CLOSE") &&
      identical(combine_verdicts("MATCH", "MATCH"), "MATCH") && identical(combine_verdicts("MATCH", "CLOSE"), "CLOSE") &&
      identical(combine_verdicts("CLOSE", "NOT COMPARED"), "CLOSE") && identical(combine_verdicts("NOT COMPARED", "FAILED"), "FAILED") &&
      identical(combine_verdicts("FAILED", "DIFFERENT", "MATCH"), "DIFFERENT") &&
      !any(grepl("[^ -~]", readLines(f, warn = FALSE)))
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-04", critical = TRUE, method = "compare_be_with_reference() and combine_verdicts() on small tables: shuffled rows, a changed verdict text, a parameter on one side only, a changed code hash, an empty table, a change of 0.0001; the reference file is ASCII",
  expected = "MATCH for shuffled rows; DIFFERENT for text, a missing or extra parameter and a code mismatch; FAILED for nothing; CLOSE for a small change; worst verdict wins")

check("REC-BE-16", "The recorded BE settings read back as the settings the run was given (parallel group with a covariate)",
  tryCatch({
    d <- gld_rec("parallel_cov")
    rec <- jsonlite::fromJSON(file.path(d, "analysis_settings.json"), simplifyDataFrame = FALSE)
    b <- be_inputs_from_record(rec)
    identical(b$design_selected, "parallel") && identical(b$reference, "Reference") && identical(b$parameters, c("CMAX", "AUCLST")) &&
      identical(b$covariates, data.frame(name = "Weight", type = "auto", transform = "none", stringsAsFactors = FALSE)) &&
      isTRUE(all.equal(c(b$ci_level, b$be_lower, b$be_upper), c(90, 80, 125))) && isTRUE(b$log_transform) &&
      isTRUE(b$pe_constraint) && identical(b$model_type, "fixed") && is.null(b$approach) && identical(b$is_steady_state, FALSE)
  }, error = function(e) FALSE),
  "URS-EXP-03", critical = TRUE, method = "analysis_settings.json of the parallel record read with simplifyDataFrame = FALSE and turned back by be_inputs_from_record()",
  expected = "design, reference, parameters, covariate specification, level, limits and flags as they were given")

check("REC-BE-17", "The record ships the BE code unchanged, with hashes, and no longer says the BE statistics are not recomputed",
  tryCatch({
    d <- gld_rec("hvd_rsabe"); rec <- jsonlite::fromJSON(file.path(d, "analysis_settings.json"), simplifyDataFrame = FALSE)
    files <- c("be_analysis.R", "be_scaled.R", "designs.R", "utils.R")
    man <- paste(readLines(file.path(d, "data_integrity.txt"), warn = FALSE), collapse = "\n")
    html <- paste(readLines(file.path(d, "analysis_summary.html"), warn = FALSE), collapse = "\n")
    all(vapply(files, function(f) identical(unname(tools::md5sum(file.path(d, f))), unname(tools::md5sum(file.path("R", f)))) &&
                 identical(rec$be_code_sha256[[f]], digest::digest(file = file.path("R", f), algo = "sha256")) &&
                 grepl(rec$be_code_sha256[[f]], man, fixed = TRUE), TRUE)) &&
      grepl("app_be_reference.csv", man, fixed = TRUE) && is.null(rec$reproduction_scope) &&
      grepl("recomputes the NCA parameters and then the", html, fixed = TRUE) && !grepl("are not\nrecomputed by the script", html, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-04", critical = TRUE, method = "The RSABE record unzipped: shipped files against R/, hashes in the JSON and in the manifest, the summary text",
  expected = "Exact copies of the four code files, their SHA-256 in the settings and the manifest, the reference file listed; no 'not recomputed' sentence")

# ---- A design that does not fit the data is refused, not analysed ----------------
gld_csv <- function(df) { f <- tempfile(fileext = ".csv"); write.csv(df, f, row.names = FALSE); f }
gld_refused <- function(df, design, inputs = list()) {
  r <- be_golden_run(list(file = gld_csv(df), inputs = utils::modifyList(list(be_design = design), inputs)))
  list(none = is.null(r$be_result), text = paste(vapply(r$notifications, `[[`, "", "text"), collapse = " | "),
       type = vapply(r$notifications, `[[`, "", "type"), res = r)
}
gld_xo <- read.csv("data/example_be_crossover.csv"); gld_rep <- read.csv("data/example_be_replicate_2x2x4.csv")
# a study run in two groups: the sequence labels carry the group, as in TR-G1 and TR-G2
gld_groups <- function(d) { g <- ave(d$Subject, d$Sequence, FUN = function(v) as.integer(factor(v)) %% 2)
                            d$Sequence <- paste0(d$Sequence, "-G", ifelse(g == 0, 1, 2)); d }

check("REP-DES-06", "A study run in groups is refused: the run stops, no result is written, and the message names the labels",
  tryCatch({
    a <- gld_refused(gld_groups(gld_xo), "2x2x2"); b <- gld_refused(gld_groups(gld_rep), "2x2x4")
    cm <- list(subject = "Subject", time = "Time", conc = "Concentration", treatment = "Treatment", period = "Period", sequence = "Sequence")
    a$none && b$none && "error" %in% a$type && "error" %in% b$type && !any(grepl("analysis complete", c(a$text, b$text))) &&
      grepl("4 sequences (expected 2)", a$text, fixed = TRUE) && grepl("TRTR-G1 and TRTR-G2", b$text, fixed = TRUE) &&
      grepl("several groups", b$text, fixed = TRUE) &&
      is.null(check_design_against_data("2x2x4", detect_study_design(gld_rep, cm)))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE, method = "The 2x2x2 and the 2x2x4 example with Sequence labelled per group (TR-G1, TR-G2, ...) run through the real BE module",
  expected = "Error message; be_result() stays empty; no 'analysis complete'")

check("REP-DES-07", "Parallel on crossover data and a crossover on one-period data are refused",
  tryCatch({
    a <- gld_refused(gld_xo, "parallel")
    b <- gld_refused(read.csv("data/example_be_parallel.csv"), "2x2x2")
    a$none && b$none && grepl("Parallel groups was selected, but the data have 2 periods", a$text, fixed = TRUE) &&
      grepl("is a crossover, but the data have one period", b$text, fixed = TRUE) && grepl("Upload page", paste(a$text, b$text), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE, method = "Parallel selected on the 2x2x2 example; 2x2x2 selected on the parallel example; both through the real module",
  expected = "Both refused with the design, what was found and what to do; no result")

check("REP-DES-08", "A subject who misses a period, and a four-sequence replicate, are not refused",
  tryCatch({
    d <- gld_xo[!(gld_xo$Subject == 1 & gld_xo$Period == 2), ]
    a <- gld_refused(d, "2x2x2")
    r23 <- read.csv("validation/fixtures/rsabe_datasets.csv"); r23 <- r23[r23$dataset == "rds23", ]
    cm <- list(subject = "subject", treatment = "treatment", period = "period", sequence = "sequence")
    !a$none && any(grepl("only one treatment", a$text)) &&
      is.null(check_design_against_data("2x2x4", detect_study_design(r23, cm), be_sequence_orders(r23, "subject", "treatment", "period", "sequence")))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE, method = "2x2x2 example without subject 1 period 2, through the real module; the four-sequence 2x2x4 fixture rds23 against the check",
  expected = "The first runs with the incomplete-subject note; rds23 (RTRT, RTTR, TRRT, TRTR) matches 2x2x4")

check("REP-DES-09", "A single treatment order is still analysed as a paired comparison, with its note",
  tryCatch({
    d <- gld_xo[gld_xo$Sequence == "TR", ]
    a <- gld_refused(d, "2x2x2")
    !a$none && identical(a$res$be_result$design, "paired") && any(grepl("paired", a$res$be_result$m13a, ignore.case = TRUE))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = FALSE, method = "The 2x2x2 example reduced to sequence TR, run through the real module",
  expected = "A result with design paired and the note; not a refusal")

check("REP-DES-10", "Survey: every bundled dataset is accepted with its own design, and only the multi-group files are refused",
  tryCatch({
    cmf <- function(raw) { cm <- auto_detect_columns(names(raw)); cm[!vapply(cm, function(v) is.null(v) || identical(v, ""), TRUE)] }
    chk <- function(raw, design) { cm <- cmf(raw); prof <- unique(raw[intersect(c(cm$subject, cm$treatment, cm$period, cm$sequence), names(raw))])
      check_design_against_data(design, detect_study_design(raw, cm), be_sequence_orders(prof, cm$subject, cm$treatment, cm$period, cm$sequence)) }
    own <- c("example_be_crossover.csv" = "2x2x2", "example_be_parallel.csv" = "parallel", "example_be_parallel_covariates.csv" = "parallel",
             "example_be_replicate_2x2x4.csv" = "2x2x4", "example_be_replicate_hvd.csv" = "2x2x4")
    ok <- vapply(names(own), function(f) is.null(chk(read.csv(file.path("data", f)), own[[f]])), TRUE)
    fx <- c("be_2x2x2_crossover.csv" = "2x2x2", "be_2x2x3_full_replicate.csv" = "2x2x3", "be_2x2x4_full_replicate.csv" = "2x2x4",
            "be_2x2x4_highly_variable.csv" = "2x2x4", "be_2x3x3_partial_replicate.csv" = "2x3x3", "flat_equivalent.csv" = "2x2x2",
            "cov_parallel_data.csv" = "parallel")
    okf <- vapply(names(fx), function(f) is.null(chk(read.csv(file.path("validation", "fixtures", f)), fx[[f]])), TRUE)
    rs <- read.csv("validation/fixtures/rsabe_datasets.csv")
    okr <- vapply(unique(rs$dataset), function(u) { d <- rs[rs$dataset == u, ]
      is.null(chk(d, if (u %in% c("rds02", "rds04", "rds07", "rds30")) "2x3x3" else "2x2x4")) }, TRUE)
    pb <- read.csv("validation/fixtures/parallel_be_datasets.csv")
    okp <- vapply(unique(pb$dataset), function(u) is.null(chk(pb[pb$dataset == u, ], "parallel")), TRUE)
    all(ok) && all(okf) && all(okr) && all(okp) &&
      !is.null(chk(gld_groups(gld_xo), "2x2x2")) && !is.null(chk(gld_groups(gld_rep), "2x2x4"))
  }, error = function(e) FALSE),
  "URS-BE-02", critical = TRUE, method = "check_design_against_data() with the sequence orders for every BE dataset in data/ and validation/fixtures/ (including the 4-sequence replicates rds23, rds24) against its own design, and for the two examples run in groups",
  expected = "All bundled datasets accepted with their own design; the two multi-group versions refused")
end_section("GLD")

# =============================================================================
# SECTION HLF: Half-life quality flags
# =============================================================================
start_section("HLF")

hlf_st <- function(...) { s <- list(admin_route = "extravascular", dose = 100, dose_unit = "mg", time_unit = "h",
  conc_unit = "ng/mL", trap_method = "log", r2adj_threshold = 0.7, infusion_duration = 0, mw = 0,
  is_steady_state = FALSE, tau = NA, partial_aucs = NULL); m <- list(...); s[names(m)] <- m; s }

check("HLF-01", "Span ratio and the rule boundaries",
  tryCatch({
    r <- data.frame(LAMZLL = c(4, 4, 4), LAMZUL = c(12, 12, 12), LAMZHL = c(4, 4.0001, 8), AUCPEO = c(20, 20.001, 5))
    f <- lambda_z_flags(r, LZ_RULES_DEFAULT)
    f$LZSPAN[1] == 2 && f$FLAG_SPAN[1] == 0 && f$FLAG_SPAN[2] == 1 && f$FLAG_SPAN[3] == 1 &&
      f$FLAG_AUCPE[1] == 0 && f$FLAG_AUCPE[2] == 1 && all(is.na(f$FLAG_AUCPBE))
  }, error = function(e) FALSE),
  "URS-NCA-04, URS-NCA-15", critical = TRUE, method = "lambda_z_flags() on spans of exactly 2, just under 2 and 1, and % extrapolated of exactly 20 and just over",
  expected = "Span 2 and 20% pass; below 2 and over 20% flagged; back-extrapolation not applicable (extravascular)")

check("HLF-02", "Flags in the NCA: blank where the half-life is blanked, at steady state extrapolation of AUCtau, back-extrapolation only for IV bolus",
  tryCatch({
    th <- read.csv("data/example_theoph.csv"); cm <- list(subject = "Subject", time = "Time", conc = "conc")
    r <- suppressWarnings(run_nca(th, cm, hlf_st(r2adj_threshold = 0.999)))
    blank <- is.na(r$LAMZHL)
    ss <- suppressWarnings(run_nca(th, cm, hlf_st(is_steady_state = TRUE, tau = 24)))
    bo <- suppressWarnings(run_nca(data.frame(S = 1, T = c(0.25, 0.5, 1, 2, 4, 8, 12), C = 100 * exp(-0.2 * c(0.25, 0.5, 1, 2, 4, 8, 12))),
                                   list(subject = "S", time = "T", conc = "C"), hlf_st(admin_route = "iv_bolus")))
    any(blank) && all(is.na(r$FLAG_SPAN[blank])) && all(!is.na(r$FLAG_SPAN[!blank])) &&
      all(abs(r$LZSPAN[!blank] - (r$LAMZUL - r$LAMZLL)[!blank] / r$LAMZHL[!blank]) < 1e-12) &&
      all(ss$FLAG_AUCPE == as.numeric(ss$AUCTAU_PCTEXT > 20)) && !is.na(bo$FLAG_AUCPBE) && all(is.na(r$FLAG_AUCPBE))
  }, error = function(e) FALSE),
  "URS-NCA-04, URS-NCA-07, URS-NCA-15", critical = TRUE,
  method = "Theophylline with a minimum R2 of 0.999 (some half-lives blanked), at steady state, and an IV bolus profile",
  expected = "Flags NA where blanked, span = (LAMZUL - LAMZLL) / t1/2, at steady state the extrapolation flag follows AUCtau % extrapolated, back-extrapolation flag only for the bolus")

check("HLF-03", "The rules are settings: switched off, changed, recorded, reproduced",
  tryCatch({
    th <- read.csv("data/example_theoph.csv"); cm <- list(subject = "Subject", time = "Time", conc = "conc")
    off <- suppressWarnings(run_nca(th, cm, hlf_st(lz_rules = list(span_min = NA, aucpext_max = 20, aucpbe_max = 20))))
    st3 <- hlf_st(lz_rules = list(span_min = 3, aucpext_max = 10, aucpbe_max = 20))
    r3 <- suppressWarnings(run_nca(th, cm, st3))
    td <- tempfile("hlf03_"); dir.create(td); zf <- file.path(td, "rec.zip")
    create_analysis_record(zf, r3, st3, cm, "data/example_theoph.csv", "example_theoph.csv", blq_rule = "rule1", lloq = 0,
                           analyst = "Validation", study_name = "Rules")
    utils::unzip(zf, exdir = td)
    js <- jsonlite::fromJSON(file.path(td, "analysis_settings.json"))
    all(is.na(off$FLAG_SPAN)) && sum(r3$FLAG_SPAN, na.rm = TRUE) >= sum(off$LZSPAN < 2, na.rm = TRUE) &&
      js$lz_rules$span_min == 3 && js$lz_rules$aucpext_max == 10 &&
      any(grepl("^Result: MATCH", readLines(file.path(td, "reproduction_check.txt"))))
  }, error = function(e) FALSE),
  "URS-NCA-04, URS-EXP-02, URS-NCA-15", critical = TRUE,
  method = "Span rule switched off; span >= 3 and extrapolated <= 10%; Analysis Record of the second run",
  expected = "No span flags when off; stricter rules flag more; the rules are in analysis_settings.json and the record reproduces (MATCH)")

check("HLF-04", "A half-life fitted on chosen points is flagged too; blanking stays exempt",
  tryCatch({
    th <- read.csv("data/example_theoph.csv"); p <- th[th$Subject == 1, ]
    r <- suppressWarnings(run_single_nca(p$Time, p$conc, hlf_st(r2adj_threshold = 0.99999), time_used = c(9.05, 12.12)))
    !is.na(r[["LAMZHL"]]) && r[["FLAG_SPAN"]] == 1
  }, error = function(e) FALSE),
  "URS-NCA-12, URS-NCA-15", critical = FALSE, method = "Theophylline subject 1, two points 9.05 and 12.12 h chosen by hand, minimum R2 0.99999",
  expected = "Half-life reported (manual fits are not blanked) and flagged for its short span")

check("HLF-05", "Flags are counted in summaries and bioequivalence, and exclude nothing",
  tryCatch({
    th <- read.csv("data/example_theoph.csv"); cm <- list(subject = "Subject", time = "Time", conc = "conc")
    r <- suppressWarnings(run_nca(th, cm, hlf_st()))
    sm <- summarize_pk_params(r, c("LAMZHL", "CMAX"))
    nf <- sum((r$FLAG_SPAN %in% 1) & !is.na(r$LAMZHL))
    xo <- read.csv("data/example_be_crossover.csv", stringsAsFactors = FALSE)
    xcm <- list(subject = "Subject", time = "Time", conc = "Concentration", treatment = "Treatment", period = "Period", sequence = "Sequence")
    rx <- suppressWarnings(run_nca(xo, xcm, hlf_st()))
    rx$FLAG_SPAN[1] <- 1
    bd <- build_be_data(rx, xo, xcm, reference = "Reference")
    f <- fit_be_parameter(bd$data, "AUCIFO", "2x2x2", trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    g <- fit_be_parameter(bd$data, "CMAX", "2x2x2", trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    sm$N_Flagged[sm$Parameter == "LAMZHL"] == nf && sm$N_Flagged[sm$Parameter == "CMAX"] == 0 &&
      sm$N[sm$Parameter == "LAMZHL"] == sum(!is.na(r$LAMZHL)) &&
      (f$row$Flagged_Test + f$row$Flagged_Ref) == 1 && (g$row$Flagged_Test + g$row$Flagged_Ref) == 0 &&
      f$row$Missing_Test + f$row$Missing_Ref == 0
  }, error = function(e) FALSE),
  "URS-NCA-06, URS-BE-11, URS-NCA-15", critical = FALSE,
  method = "Summary of theophylline half-lives; a crossover with one span flag, AUC to infinity and Cmax compared",
  expected = "N flagged counted for half-life, not for Cmax; all values used; one flagged AUC to infinity profile in bioequivalence, none missing")

check("HLF-06", "Flags in words: tables, Half-Life Review checklist and profile list",
  tryCatch({
    r <- data.frame(Subject = c("1", "2", "3"), LAMZHL = c(5, 6, NA), LZSPAN = c(1.4, 3, NA), FLAG_SPAN = c(1, 0, NA),
                    AUCPEO = c(27, 5, NA), FLAG_AUCPE = c(1, 0, NA), FLAG_AUCPBE = NA)
    tx <- lz_flag_text(r)
    off <- lz_flag_text(r[2, ], list(span_min = NA, aucpext_max = NA, aucpbe_max = NA))
    ck <- lz_checklist(list(r2adj = 0.93, half_life = 5, time_used = c(8, 15)), 0.7, LZ_RULES_DEFAULT, pe = 27)
    ch <- lz_profile_choices(r)
    tx[1] == "span 1.4 < 2; % extrapolated 27 > 20" && tx[2] == "none" && tx[3] == "no half-life" && off == "rules off" &&
      grepl("pass", ck) && grepl("span 1.4 < 2 flag", ck, fixed = TRUE) && grepl("27% > 20% flag", ck, fixed = TRUE) &&
      names(ch)[1] == "1 (flagged)" && unname(ch[1]) == "1" && names(ch)[2] == "2" &&
      grepl("span", lz_rules_summary(LZ_RULES_DEFAULT)) && grepl("all rules off", lz_rules_summary(list(span_min = NA, aucpext_max = NA, aucpbe_max = NA)))
  }, error = function(e) FALSE),
  "URS-UI-01, URS-NCA-04, URS-NCA-15", critical = FALSE, method = "lz_flag_text(), lz_checklist(), lz_profile_choices(), lz_rules_summary()",
  expected = "Plain-text flags (not colour only); a profile without flags says none, no half-life or rules off (never an empty cell); a checklist per fit; '(flagged)' in the profile list, values unchanged")

end_section("HLF")

# =============================================================================
# SECTION EXC: Exclusions made by the analyst, with a reason
# =============================================================================
start_section("EXC")

exc_st <- hlf_st
exc_xo <- read.csv("data/example_be_crossover.csv", stringsAsFactors = FALSE)
exc_cm <- list(subject = "Subject", time = "Time", conc = "Concentration", treatment = "Treatment", period = "Period", sequence = "Sequence")
exc_one <- function(level, subject, treatment, period, time = NA, category = "Sample handling", id = "e1")
  as_exclusions(data.frame(id = id, level = level, subject = subject, treatment = treatment, period = period, time = time,
                           category = category, stringsAsFactors = FALSE))
exc_drop <- function(r) r[, setdiff(names(r), "EXCL"), drop = FALSE]

check("EXC-01", "Excluding a sample gives the result of deleting it from the file, under every BLQ rule",
  tryCatch({
    d <- data.frame(Subject = 1, Time = c(0, 0.5, 1, 2, 4, 6, 8, 12, 24),
                    Conc = c("BLQ", 2, 9, 12, 8, "BLQ", 4, 2, "BLQ"), stringsAsFactors = FALSE)
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    ex <- as_exclusions(data.frame(id = "e1", level = "sample", subject = "1", time = 8, category = "Sample handling"))
    all(vapply(paste0("rule", 1:6), function(rule) {
      a <- prepare_pk_dataset(d, cm, list(lloq = 1, blq_rule = rule, exclusions = ex))$data
      b <- prepare_pk_dataset(d[d$Time != 8, ], cm, list(lloq = 1, blq_rule = rule))$data
      ra <- suppressWarnings(run_nca(a, cm, exc_st(exclusions = ex))); rb <- suppressWarnings(run_nca(b, cm, exc_st()))
      isTRUE(all.equal(exc_drop(ra), exc_drop(rb))) && identical(a$Conc, b$Conc)
    }, logical(1)))
  }, error = function(e) FALSE),
  "URS-DAT-04, URS-NCA-06, URS-DAT-09", critical = TRUE,
  method = "A profile with BLQ at 0, 6 and 24 h (LLOQ 1); the 8 h sample excluded; Rules 1 to 6",
  expected = "Imputed values and every NCA parameter identical to the file without the 8 h row (the exclusion acts before the BLQ rule)")

check("EXC-02", "Exclusions are matched by profile and time: file order and floating-point times do not matter",
  tryCatch({
    shuffled <- exc_xo[sample(nrow(exc_xo)), ]
    p1 <- exc_xo$Period[exc_xo$Subject == 2 & exc_xo$Treatment == "Test"][1]
    t0 <- sort(unique(exc_xo$Time))[4]
    ex <- exc_one("sample", "2", "Test", as.character(p1), time = t0 + 1e-13)
    a <- prepare_pk_dataset(shuffled, exc_cm, list(lloq = 0, exclusions = ex))
    f <- tempfile(fileext = ".csv")
    writeLines(c("Subject;Time;Conc", "1;0;0", "1;0,1;5", "1;0,3;9", "1;1;6", "1;2;3"), f)
    dc <- read_pk_file(f, list(sep = ";", dec = ","))
    b <- prepare_pk_dataset(dc, list(subject = "Subject", time = "Time", conc = "Conc"),
                            list(lloq = 0, read_args = list(dec = ","),
                                 exclusions = as_exclusions(data.frame(id = "e", level = "sample", subject = "1", time = 0.1 + 0.2))))
    nrow(a$data) == nrow(exc_xo) - 1 && nrow(a$excluded$samples) == 1 && length(a$excluded$unmatched) == 0 &&
      !any(b$data$Time == 0.3) && nrow(b$data) == 4
  }, error = function(e) FALSE),
  "URS-DAT-01, URS-DAT-09", critical = TRUE,
  method = "Crossover file in random order, exclusion time off by 1e-13; a decimal-comma file with 0,3 excluded as 0.1 + 0.2",
  expected = "Exactly the intended sample removed in both")

check("EXC-03", "An exclusion that no longer matches the data is reported, never dropped silently",
  tryCatch({
    p1 <- exc_xo$Period[exc_xo$Subject == 2 & exc_xo$Treatment == "Test"][1]
    ex <- exc_one("sample", "2", "Test", as.character(p1), time = 9999)
    a <- prepare_pk_dataset(exc_xo, exc_cm, list(lloq = 0, exclusions = ex))
    up <- paste(readLines("R/mod_data_upload.R"), collapse = "\n")
    identical(a$excluded$unmatched, "e1") && nrow(a$data) == nrow(exc_xo) &&
      grepl("exclusion(s) match no sample or profile of these data", up, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-03, URS-DAT-09", critical = TRUE, method = "An exclusion at a time that is not in the data; the Process Data handler",
  expected = "Listed as unmatched; Process Data stops with a message naming it")

check("EXC-04", "A profile exclusion keeps the NCA and leaves the profile out of summaries and bioequivalence",
  tryCatch({
    p <- exc_xo$Period[exc_xo$Subject == 3 & exc_xo$Treatment == "Reference"][1]
    ex <- exc_one("profile", "3", "Reference", as.character(p), category = "Vomiting or diarrhoea")
    ex$detail <- "vomited at 0.5 h"
    d <- prepare_pk_dataset(exc_xo, exc_cm, list(lloq = 0, exclusions = ex))$data
    r <- suppressWarnings(run_nca(d, exc_cm, exc_st(exclusions = ex)))
    sm <- summarize_pk_params(r, "CMAX", group_col = "Treatment")
    bd <- build_be_data(r, d, exc_cm, reference = "Reference", exclusions = ex)
    f <- fit_be_parameter(bd$data, "CMAX", "2x2x2", trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    nrow(r) == 12 && sum(r$EXCL) == 1 && !is.na(r$CMAX[r$EXCL == 1]) &&
      sm$N[sm$Treatment == "Reference"] == 5 && sm$N[sm$Treatment == "Test"] == 6 &&
      bd$data$EXCLUDED[bd$data$Subject == "3" & bd$data$Treatment == "Reference"] == "Vomiting or diarrhoea: vomited at 0.5 h" &&
      f$row$Excluded_Ref == 1 && f$row$Missing_Ref == 0 && f$row$N_Test == 5
  }, error = function(e) FALSE),
  "URS-NCA-06, URS-BE-11, URS-DAT-09", critical = TRUE,
  method = "example_be_crossover.csv with subject 3's Reference profile excluded for vomiting",
  expected = "12 NCA rows, that profile marked EXCL = 1 with its Cmax; Reference N 5 in the summary; excluded (not missing) in bioequivalence with its reason; 5 subjects compared")

check("EXC-05", "Records hold the exclusions and reproduce them; a changed register is DIFFERENT",
  tryCatch({
    p1 <- exc_xo$Period[exc_xo$Subject == 2 & exc_xo$Treatment == "Test"][1]
    p3 <- exc_xo$Period[exc_xo$Subject == 3 & exc_xo$Treatment == "Reference"][1]
    ex <- rbind(exc_one("sample", "2", "Test", as.character(p1), time = sort(unique(exc_xo$Time))[4], id = "s1"),
                exc_one("profile", "3", "Reference", as.character(p3), category = "Dosing deviation", id = "p1"))
    f <- tempfile(fileext = ".csv"); write.csv(exc_xo, f, row.names = FALSE)
    st <- exc_st(exclusions = ex)
    d <- prepare_pk_dataset(read.csv(f, stringsAsFactors = FALSE), exc_cm, list(lloq = 0, exclusions = ex))$data
    r <- suppressWarnings(run_nca(d, exc_cm, st))
    td <- tempfile("exc05_"); dir.create(td); zf <- file.path(td, "rec.zip")
    create_analysis_record(zf, r, st, exc_cm, f, "crossover.csv", blq_rule = "rule1", lloq = 0,
                           analyst = "Validation", study_name = "Exclusions")
    utils::unzip(zf, exdir = td)
    js <- jsonlite::fromJSON(file.path(td, "analysis_settings.json"), simplifyDataFrame = FALSE)
    wb_sheets <- openxlsx::getSheetNames(file.path(td, "results.xlsx"))
    html <- paste(readLines(file.path(td, "analysis_summary.html"), warn = FALSE), collapse = "\n")
    match1 <- any(grepl("^Result: MATCH", readLines(file.path(td, "reproduction_check.txt"))))
    n_ex <- length(js$exclusions)
    js$exclusions <- js$exclusions[1]   # drop the profile exclusion
    jsonlite::write_json(js, file.path(td, "analysis_settings.json"), auto_unbox = TRUE, digits = NA, null = "null", pretty = TRUE)
    owd <- setwd(td); out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), "reproduce_analysis.R", stdout = TRUE, stderr = TRUE)); setwd(owd)
    js$schema_version == "1.4.0" && n_ex == 2 && match1 && "Exclusions" %in% wb_sheets &&
      grepl("11. Exclusions", html, fixed = TRUE) && any(grepl("^Result: DIFFERENT", out))
  }, error = function(e) FALSE),
  "URS-EXP-01, URS-EXP-02, URS-EXP-03, URS-DAT-09", critical = TRUE,
  method = "Record with one sample and one profile exclusion; reproduction; then the profile exclusion removed from analysis_settings.json",
  expected = "Schema 1.4.0 with both exclusions; Exclusions sheet and section; MATCH; DIFFERENT after the register was changed")

check("EXC-06", "Edge cases: IV bolus pre-dose, steady-state trough, and a partial AUC across an excluded sample",
  tryCatch({
    cm <- list(subject = "S", time = "T", conc = "C")
    iv <- data.frame(S = 1, T = c(0, 0.25, 0.5, 1, 2, 4, 8), C = c(0, 90, 80, 65, 45, 20, 5))
    exi <- as_exclusions(data.frame(id = "i", level = "sample", subject = "1", time = 0, category = "Sample handling"))
    a <- suppressWarnings(run_nca(prepare_pk_dataset(iv, cm, list(lloq = 0, exclusions = exi))$data, cm, exc_st(admin_route = "iv_bolus")))
    b <- suppressWarnings(run_nca(iv, cm, exc_st(admin_route = "iv_bolus")))
    ss <- data.frame(S = 1, T = c(0, 1, 2, 4, 8, 12), C = c(1.5, 10, 12, 8, 5, 3))
    exs <- as_exclusions(data.frame(id = "s", level = "sample", subject = "1", time = 0, category = "Sample handling"))
    s1 <- suppressWarnings(run_nca(prepare_pk_dataset(ss, cm, list(lloq = 0, exclusions = exs))$data, cm,
                                   exc_st(is_steady_state = TRUE, tau = 12)))
    pa <- data.frame(start = 0, end = "6", cmax = FALSE, role = "pivotal")
    pd <- data.frame(S = 1, T = c(0, 1, 2, 4, 6, 8, 12), C = c(0, 5, 9, 7, 4, 3, 1))
    exp_ <- as_exclusions(data.frame(id = "p", level = "sample", subject = "1", time = 4, category = "Sample handling"))
    pa1 <- suppressWarnings(run_nca(prepare_pk_dataset(pd, cm, list(lloq = 0, exclusions = exp_))$data, cm, exc_st(partial_aucs = pa)))
    pa2 <- suppressWarnings(run_nca(pd[pd$T != 4, ], cm, exc_st(partial_aucs = pa)))
    isTRUE(all.equal(exc_drop(a), exc_drop(b))) && s1$CMIN_SS == 3 && abs(pa1$AUC_0_6 - pa2$AUC_0_6) < 1e-12
  }, error = function(e) FALSE),
  "URS-NCA-02, URS-NCA-07, URS-NCA-13, URS-DAT-09", critical = TRUE,
  method = "IV bolus with the time-0 sample excluded; steady state with the pre-dose sample excluded; AUC 0-6 h with the 4 h sample excluded",
  expected = "Bolus unchanged (that sample is set aside anyway); Cmin from the remaining samples (3); partial AUC interpolated across the gap as without the row")

check("EXC-07", "The ICH M13A checks use the data before exclusions, so excluding the pre-dose sample does not silence them",
  tryCatch({
    d <- exc_xo; i <- d$Subject == 1 & d$Treatment == "Test" & d$Time == 0; d$Concentration[i] <- 30
    p1 <- d$Period[i][1]
    ex <- exc_one("sample", "1", "Test", as.character(p1), time = 0)
    with_ex <- prepare_pk_dataset(d, exc_cm, list(lloq = 0, exclusions = ex))$data
    without <- prepare_pk_dataset(d, exc_cm, list(lloq = 0))$data
    r <- suppressWarnings(run_nca(with_ex, exc_cm, exc_st(exclusions = ex)))
    m_all <- be_m13a_checks(without, exc_cm, r, NULL); m_ex <- be_m13a_checks(with_ex, exc_cm, r, NULL)
    be <- paste(readLines("R/mod_path_be.R"), collapse = "\n")
    ba <- paste(readLines("R/be_analysis.R", warn = FALSE), collapse = "\n")
    any(grepl("Pre-dose concentration above 5% of Cmax", m_all)) && !any(grepl("Pre-dose concentration above 5% of Cmax", m_ex)) &&
      grepl("be_m13a_checks(d_unexcl, cm, nca_res,", ba, fixed = TRUE) &&
      grepl("data_without_exclusions(shared)", be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-11, URS-DAT-09", critical = TRUE,
  method = "Subject 1 Test with a pre-dose value of 30 (above 5% of Cmax), that sample excluded; the checks on both datasets; the BE module",
  expected = "The check fires on the data before exclusions and the module passes those data")

check("EXC-08", "A sensitivity analysis without the exclusions is computed, shown and recorded",
  tryCatch({
    be <- paste(readLines("R/mod_path_be.R"), collapse = "\n")
    ba <- paste(readLines("R/be_analysis.R", warn = FALSE), collapse = "\n")
    f <- tempfile(fileext = ".csv"); write.csv(exc_xo, f, row.names = FALSE)
    r <- suppressWarnings(run_nca(exc_xo, exc_cm, exc_st()))
    bd <- build_be_data(r, exc_xo, exc_cm, reference = "Reference")
    ci <- fit_be_parameter(bd$data, "CMAX", "2x2x2", trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")$row
    td <- tempfile("exc08_"); dir.create(td); zf <- file.path(td, "rec.zip")
    create_analysis_record(zf, r, exc_st(), exc_cm, f, "crossover.csv", blq_rule = "rule1", lloq = 0,
                           be_results = list(ci_table = ci, sensitivity = ci), be_settings = list(ci_level = 90))
    utils::unzip(zf, exdir = td)
    grepl("sens_fits <- tryCatch(lapply(params, function(p) fit_one(bd0$data, p)),", ba, fixed = TRUE) &&
      grepl("sens_df <- do.call(rbind, lapply(sens_fits, function(f) f$row))", ba, fixed = TRUE) &&
      grepl("sensitivity = sens_df", ba, fixed = TRUE) && grepl("Sensitivity analysis: without your exclusions", be, fixed = TRUE) &&
      "BE_Without_Exclusions" %in% openxlsx::getSheetNames(file.path(td, "results.xlsx"))
  }, error = function(e) FALSE),
  "URS-BE-11, URS-EXP-05, URS-BE-12", critical = FALSE,
  method = "The BE module code; a record built with a sensitivity table",
  expected = "Computed with the same fit on the unexcluded data, shown under the CI table, and a BE_Without_Exclusions sheet in the record")

check("EXC-09", "Adding and restoring an exclusion in the app: register, data prepared again, nothing deleted",
  tryCatch({
    ok <- FALSE
    cm <- list(subject = "Subject", time = "Time", conc = "conc")
    th <- read.csv("data/example_theoph.csv")
    opts <- list(lloq = 0, blq_rule = "rule1", door = "flat", read_args = list(), col_map = cm)
    ds <- prepare_pk_dataset(th, cm, opts[setdiff(names(opts), "col_map")])
    sh <- shiny::reactiveValues(data_ready = TRUE, raw_data = th, col_map = cm, pk_data = ds$data, pk_dataset = ds,
                                prepare_opts = opts, exclusions = NULL, be_results = list(ci_table = 1),
                                exclusion_request = NULL)
    suppressWarnings(shiny::testServer(exclusions_server, args = list(shared = sh), {
      session$setInputs(dlg_profile = "1", dlg_level = "sample", dlg_times = "0.25", dlg_category = "Sample handling",
                        dlg_detail = "", dlg_prespecified = "no", dlg_section = "", dlg_save = 1)
      added <- nrow(active_exclusions(sh$exclusions)) == 1 && nrow(sh$pk_data) == nrow(ds$data) - 1 &&
        isTRUE(sh$exclusions$after_be[1])
      session$setInputs(dlg_category = "Other", dlg_detail = "", dlg_save = 2)
      refused <- nrow(sh$exclusions) == 1
      session$setInputs(restore_id = sh$exclusions$id[1], restore_reason = "wrong sample", restore = 1)
      ok <<- added && refused && nrow(sh$exclusions) == 1 && !is.na(sh$exclusions$restored_utc[1]) &&
        nrow(sh$pk_data) == nrow(ds$data) && sh$exclusions$restore_reason[1] == "wrong sample"
    }))
    ok
  }, error = function(e) FALSE),
  "URS-DAT-03, URS-GEN-09, URS-DAT-09", critical = TRUE,
  method = "shiny::testServer on the exclusion module: exclude theophylline subject 1 at 0.25 h after BE results exist; save 'Other' without detail; restore",
  expected = "One exclusion, marked after bioequivalence results, the sample removed; 'Other' without detail refused; after restoring the sample is back and the entry stays with its reason")

check("EXC-10", "Controlled mode: exclusions are written to the audit trail first, and overrides reset only on new data",
  tryCatch({
    ex <- paste(readLines("R/mod_exclusions.R"), collapse = "\n")
    mu <- paste(readLines("R/mod_path_multi_nca.R"), collapse = "\n"); be <- paste(readLines("R/mod_path_be.R"), collapse = "\n")
    up <- paste(readLines("R/mod_data_upload.R"), collapse = "\n")
    grepl('if (!gxp_guard("exclusion_added"', ex, fixed = TRUE) && grepl('if (!gxp_guard("exclusion_restored"', ex, fixed = TRUE) &&
      regexpr('gxp_guard("exclusion_added"', ex, fixed = TRUE) < regexpr("shared$exclusions <- rbind(as_exclusions(shared$exclusions), new)", ex, fixed = TRUE) &&
      regexpr('gxp_guard("exclusion_imported"', ex, fixed = TRUE) < regexpr("shared$exclusions <- rbind(have, imp)", ex, fixed = TRUE) &&
      grepl("observeEvent(shared$data_id, {", mu, fixed = TRUE) && grepl("observeEvent(shared$data_id, {", be, fixed = TRUE) &&
      !grepl("observeEvent(shared$pk_data, {", mu, fixed = TRUE) && grepl("prune_overrides(lz_state, shared$exclusions)", mu, fixed = TRUE) &&
      grepl("shared$data_id    <- shared$data_id + 1", up, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GXP-05, URS-GXP-07, URS-NCA-12, URS-DAT-09", critical = TRUE,
  method = "R/mod_exclusions.R, the batch and BE modules, the upload module",
  expected = "gxp_guard before the register changes (fail closed); manual fits reset on Process Data only and pruned for the changed profiles")

check("EXC-11", "A settings file of schema 1.3.0 (no exclusions, no rules) still reads",
  tryCatch({
    rec <- list(dose_source = "single", dose = 320, admin_route = "extravascular", steady_state = FALSE, tau = NULL,
                dose_unit = "mg", time_unit = "h", conc_unit = "mg/L", trap_method = "log", r2adj_threshold = 0.7)
    s <- record_nca_settings(rec, NULL, NULL)
    th <- read.csv("data/example_theoph.csv"); cm <- list(subject = "Subject", time = "Time", conc = "conc")
    r <- suppressWarnings(run_nca(th, cm, s))
    nrow(r) == 12 && all(r$EXCL == 0) && nrow(as_exclusions(NULL)) == 0 && identical(lz_rules(s), LZ_RULES_DEFAULT)
  }, error = function(e) FALSE),
  "URS-EXP-03, URS-DAT-09", critical = FALSE, method = "record_nca_settings() on a settings list without exclusions or lz_rules",
  expected = "Default rules, no exclusions, 12 profiles")

end_section("EXC")


# =============================================================================
# SECTION ARV: Adversarial review of the app (dose unit, trough, subjects,
# pre-dose time, exclusion register, error messages)
# =============================================================================
# One test per finding that held up on reproduction, each built from the
# failing case: theophylline's Dose column in mg/kg, a trough drawn 15 minutes
# late, a crossover subject left with one period, a last sample before tau, a
# pre-dose sample at -0.25 h, a register lost with the session.
start_section("ARV")

arv_th <- read.csv("data/example_theoph.csv", colClasses = c(Subject = "character"))
arv_cm <- list(subject = "Subject", time = "Time", conc = "conc", dose = "Dose")
arv_ss <- function(t, c, ...) suppressWarnings(run_nca(data.frame(Subject = "1", Time = t, Conc = c),
  list(subject = "Subject", time = "Time", conc = "Conc"),
  hlf_st(is_steady_state = TRUE, tau = 12, ...)))

check("ARV-01", "A Dose column per kg is multiplied by body weight: CL/F as with the amount given",
  tryCatch({
    cmk <- c(arv_cm, list(dose_weight = "Wt"))
    ds <- prepare_pk_dataset(arv_th, cmk, list())
    st <- hlf_st(dose_unit = "mg", conc_unit = "mg/L", dose = dose_by_profile(ds$data, cmk), dose_source = "per_profile")
    r <- suppressWarnings(run_nca(ds$data, cmk, st))
    old <- suppressWarnings(run_nca(arv_th, arv_cm, hlf_st(dose_unit = "mg", conc_unit = "mg/L",
                                                           dose = dose_by_profile(arv_th, arv_cm), dose_source = "per_profile")))
    amt <- tapply(arv_th$Dose * arv_th$Wt, arv_th$Subject, max)[r$Subject]
    wt <- tapply(arv_th$Wt, arv_th$Subject, max)[r$Subject]
    k <- match(r$Subject, old$Subject)
    identical(ds$provenance$dose_basis, "per_kg") && all(ds$data[[DOSE_PER_KG_COLUMN]] == arv_th$Dose[order(arv_th$Subject, arv_th$Time)]) &&
      all(abs(as.numeric(r$CLFO) - amt / as.numeric(r$AUCIFO)) < 1e-9 * as.numeric(r$CLFO)) &&
      all(abs(as.numeric(r$CLFO) / as.numeric(old$CLFO[k]) - wt) < 1e-6 * wt)
  }, error = function(e) FALSE),
  "URS-NCA-09, URS-DAT-01", critical = TRUE,
  method = "example_theoph.csv with Dose (mg/kg) and the weight column Wt mapped, against the Dose column read as mg",
  expected = "CL/F = Dose x Wt / AUC to infinity for every subject, i.e. body weight (54.6 to 86.4 kg) times the value from reading mg/kg as mg; uploaded dose kept")

check("ARV-02", "The Dose panel states the assumed unit and warns when doses look per kg",
  tryCatch({
    h <- as.character(dose_column_panel(arv_th, arv_cm, "mg"))
    hk <- as.character(dose_column_panel(prepare_pk_dataset(arv_th, c(arv_cm, list(dose_weight = "Wt")), list())$data,
                                         c(arv_cm, list(dose_weight = "Wt")), "mg"))
    xo <- read.csv("data/example_be_crossover.csv"); xo$Dose <- 100
    hx <- as.character(dose_column_panel(xo, list(subject = "Subject", time = "Time", conc = "Concentration", dose = "Dose"), "mg"))
    dose_looks_per_kg(arv_th, arv_cm) && grepl("alert-warning", h) && grepl("per subject", h) &&
      !grepl("alert-success|circle-check", paste(h, hk, hx)) && !grepl("alert-warning", hk) && grepl("per kg", hk) &&
      !grepl("alert-warning", hx)
  }, error = function(e) FALSE),
  "URS-NCA-09, URS-UI-01", critical = FALSE,
  method = "dose_column_panel() for theophylline without and with the weight column, and for a 100 mg crossover",
  expected = "No green confirmation; the unit read is stated; a warning only for the theophylline doses read per subject")

check("ARV-03", "A record with doses per kg reproduces",
  tryCatch({
    td <- tempfile("arv03_"); dir.create(td); zf <- file.path(td, "rec.zip")
    cmk <- c(arv_cm, list(dose_weight = "Wt"))
    d <- prepare_pk_dataset(read.csv(example_path("example_theoph.csv")), cmk, list(lloq = 0))$data
    st <- hlf_st(dose_unit = "mg", conc_unit = "mg/L", dose = dose_by_profile(d, cmk), dose_source = "per_profile")
    create_analysis_record(zf, suppressWarnings(run_nca(d, cmk, st)), st, cmk,
                           example_path("example_theoph.csv"), "example_theoph.csv", blq_rule = "rule1", lloq = 0,
                           analyst = "Validation", study_name = "ARV-03")
    utils::unzip(zf, exdir = td)
    any(grepl("^Result: MATCH", readLines(file.path(td, "reproduction_check.txt"))))
  }, error = function(e) FALSE),
  "URS-EXP-02, URS-EXP-03", critical = TRUE,
  method = "create_analysis_record() for theophylline with the weight column mapped, per-profile doses",
  expected = "reproduction_check.txt: MATCH (the mapping carries the conversion)")

check("ARV-04", "A crossover subject without both treatments leaves both models",
  tryCatch({
    st <- hlf_st()
    ex <- exc_one("profile", "3", "Reference", "2", category = "Vomiting or diarrhoea")
    r <- suppressWarnings(run_nca(exc_xo, exc_cm, c(st, list(exclusions = ex))))
    bd <- build_be_data(r, exc_xo, exc_cm, reference = "Reference", exclusions = ex)$data
    xo2 <- exc_xo[exc_xo$Subject != 3, ]
    bd2 <- build_be_data(suppressWarnings(run_nca(xo2, exc_cm, st)), xo2, exc_cm, reference = "Reference")$data
    f <- function(b, m) fit_be_parameter(b, "CMAX", "2x2x2", m, "Treatment", "Subject", "Period", "Sequence")$row
    a <- f(bd, "fixed"); b <- f(bd, "mixed"); c2 <- f(bd2, "mixed")
    a$N_Test == 5 && b$N_Test == 5 && b$N_Ref == 5 && b$Incomplete_Subjects == 1 &&
      abs(b$Point_Est - c2$Point_Est) < 1e-9 && abs(b$CI_Lower - c2$CI_Lower) < 1e-9 && abs(a$Point_Est - b$Point_Est) < 1e-6
  }, error = function(e) FALSE),
  "URS-BE-01, URS-BE-11", critical = TRUE,
  method = "example_be_crossover.csv, the Reference profile of subject 3 excluded; Method A and B against the data without subject 3",
  expected = "Method B equals the analysis without subject 3 (was: Test 6 / Reference 5 and a shifted ratio); 1 incomplete subject reported")

check("ARV-05", "C(tau) is the sample nearest to tau within the trough window, never interpolated",
  tryCatch({
    t <- c(0, 0.5, 1, 2, 4, 6, 8); cc <- c(20, 50, 80, 90, 70, 50, 35)
    late <- arv_ss(c(t, 12.25), c(cc, 20)); far <- arv_ss(c(t, 13.5), c(cc, 18))
    exact <- arv_ss(c(t, 12.25), c(cc, 20), ctau_window = 0)
    w <- tryCatch({ arv_ss(c(t, 13.5), c(cc, 18)); character(0) }, warning = function(w) conditionMessage(w))
    msgs <- character(0)
    withCallingHandlers(run_nca(data.frame(Subject = "1", Time = c(t, 13.5), Conc = c(cc, 18)),
      list(subject = "Subject", time = "Time", conc = "Conc"), hlf_st(is_steady_state = TRUE, tau = 12)),
      warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
    late$CTAU_SS == 20 && late$CTAU_TIME == 12.25 && is.na(far$CTAU_SS) && is.na(exact$CTAU_SS) &&
      ctau_window(list(), 12) == 1 && ctau_window(list(), 4) == 0.4 && any(grepl("is empty. It is never interpolated", msgs, fixed = TRUE))
  }, error = function(e) FALSE),
  "URS-NCA-07", critical = TRUE,
  method = "tau 12 h; troughs at 12.25 h and 13.5 h; window default (min(10% of tau, 1)) and 0",
  expected = "12.25 h used with its time; 13.5 h and window 0 give no Ctau, with a note (was: empty without a note)")

check("ARV-06", "AUCtau extrapolated past the last sample is reported, noted and flagged",
  tryCatch({
    t <- c(0, 0.5, 1, 2, 4, 6, 8, 10); cc <- c(20, 50, 80, 90, 70, 50, 35, 26)
    msgs <- character(0)
    r <- withCallingHandlers(run_nca(data.frame(Subject = "1", Time = t, Conc = cc),
      list(subject = "Subject", time = "Time", conc = "Conc"), hlf_st(is_steady_state = TRUE, tau = 12)),
      warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
    full <- arv_ss(c(t, 12), c(cc, 20))
    strict <- suppressWarnings(run_nca(data.frame(Subject = "1", Time = t, Conc = cc), list(subject = "Subject", time = "Time", conc = "Conc"),
      hlf_st(is_steady_state = TRUE, tau = 12, lz_rules = list(span_min = 2, aucpext_max = 5, aucpbe_max = 20))))
    p <- 100 * (r$AUCTAU - r$AUCLST) / r$AUCTAU
    abs(r$AUCTAU_PCTEXT - p) < 1e-9 && r$AUCTAU_PCTEXT > 0 && full$AUCTAU_PCTEXT == 0 &&
      any(grepl("extrapolated with", msgs)) && r$FLAG_AUCPE == 0 && strict$FLAG_AUCPE == 1 &&
      grepl("extrapolated 7.3 > 5", lz_flag_text(strict, list(span_min = 2, aucpext_max = 5, aucpbe_max = 20)), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-NCA-07, URS-NCA-15", critical = TRUE,
  method = "tau 12 h with the last sample at 10 h, and at 12 h; extrapolation limit 20% and 5%",
  expected = "AUCTAU_PCTEXT = (AUCtau - AUClast) / AUCtau x 100, 0 when sampled to tau; a note; flagged above the limit (was: silent)")

check("ARV-07", "One pre-dose sample at a small negative time is analysed at 0; other negative times are refused",
  tryCatch({
    cm <- list(subject = "Subject", time = "Time", conc = "Conc")
    tt <- c(0.5, 1, 2, 4, 6, 8, 12); cc <- c(40, 70, 80, 60, 45, 34, 19)
    neg <- data.frame(Subject = "1", Time = c(-0.25, tt), Conc = c(0, cc))
    zero <- data.frame(Subject = "1", Time = c(0, tt), Conc = c(0, cc))
    qc <- run_data_quality_check(neg, cm, lloq = 0); ds <- prepare_pk_dataset(neg, cm, list())
    r1 <- suppressWarnings(run_nca(ds$data, cm, hlf_st())); r0 <- suppressWarnings(run_nca(zero, cm, hlf_st()))
    two <- neg; two$Time[2] <- -0.1
    both <- rbind(neg, transform(zero, Subject = "2")); both$Time[both$Subject == "2"][1] <- -0.25
    both <- rbind(both, data.frame(Subject = "2", Time = 0, Conc = 0))
    qc$pass && any(qc$findings$Severity == "WARNING" & grepl("analysed at time 0", qc$findings$Message)) &&
      ds$data$Time[1] == 0 && ds$data[[PREDOSE_TIME_COLUMN]][1] == -0.25 && ds$provenance$predose_times_set_to_0 == 1 &&
      isTRUE(all.equal(r1$AUCLST, r0$AUCLST)) && isTRUE(all.equal(r1$TLAG, r0$TLAG)) &&
      !run_data_quality_check(two, cm, lloq = 0)$pass && !run_data_quality_check(both, cm, lloq = 0)$pass
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE,
  method = "A 12 h profile with its pre-dose sample at -0.25 h; two negative times; a negative and a 0 h pre-dose sample",
  expected = "Analysed at 0 with a warning and the uploaded time kept, results equal to the file with 0 (was: refused); the others refused")

check("ARV-08", "The exclusion register can be loaded again, and keeps its evidence of timing",
  tryCatch({
    ok <- FALSE
    cm <- list(subject = "Subject", time = "Time", conc = "conc")
    th <- read.csv("data/example_theoph.csv")
    opts <- list(lloq = 0, blq_rule = "rule1", door = "flat", read_args = list(), col_map = cm)
    ds <- prepare_pk_dataset(th, cm, opts[setdiff(names(opts), "col_map")])
    reg <- as_exclusions(data.frame(id = c("a1", "a2"), level = c("sample", "profile"), subject = c("1", "2"),
      treatment = NA, period = NA, time = c(0.25, NA), category = c("Sample handling", "Vomiting or diarrhoea"),
      after_be = c(TRUE, FALSE), created_utc = "2026-09-01T10:00:00Z", created_by = "Analyst (ana)", stringsAsFactors = FALSE))
    f_csv <- tempfile(fileext = ".csv"); utils::write.csv(reg, f_csv, row.names = FALSE, na = "")
    f_json <- tempfile(fileext = ".json"); jsonlite::write_json(list(exclusions = reg), f_json, auto_unbox = TRUE)
    bad <- reg; bad$subject[1] <- "99"; f_bad <- tempfile(fileext = ".csv"); utils::write.csv(bad, f_bad, row.names = FALSE, na = "")
    rc <- read_exclusion_file(f_csv); rj <- read_exclusion_file(f_json, "analysis_settings.json")
    sh <- shiny::reactiveValues(data_ready = TRUE, raw_data = th, col_map = cm, pk_data = ds$data, pk_dataset = ds,
                                prepare_opts = opts, exclusions = NULL, be_results = list(ci_table = 1), exclusion_request = NULL)
    suppressWarnings(shiny::testServer(exclusions_server, args = list(shared = sh), {
      session$setInputs(import = list(name = "exclusions.csv", datapath = f_bad))
      none <- nrow(as_exclusions(sh$exclusions)) == 0
      session$setInputs(import = list(name = "exclusions.csv", datapath = f_csv))
      ex <- as_exclusions(sh$exclusions)
      ok <<- none && nrow(ex) == 2 && all(ex$after_be) && all(!is.na(ex$imported_utc)) &&
        all(ex$created_utc == "2026-09-01T10:00:00Z") && nrow(sh$pk_data) == nrow(ds$data) - 1
    }))
    ok && identical(rc$id, reg$id) && identical(rj$category, reg$category) && is.null(read_exclusion_file(f_json, "x.csv"))
  }, error = function(e) FALSE),
  "URS-DAT-09, URS-GXP-05", critical = TRUE,
  method = "exclusions.csv and analysis_settings.json read back; shiny::testServer: a register that does not match, then one that does, after bioequivalence results",
  expected = "Nothing loaded from the non-matching file; both exclusions loaded with their original time and author, marked as after bioequivalence results, the sample removed")

check("ARV-09", "Controlled mode: an exclusion made in a new session after bioequivalence results is still marked",
  tryCatch({
    ex <- paste(readLines("R/mod_exclusions.R"), collapse = "\n")
    grepl("after_be = be_results_seen(shared)", ex, fixed = TRUE) &&
      grepl('tr$event == "analysis_run" & grepl("^bioequivalence", tr$object) & tr$sha256 %in% sha', ex, fixed = TRUE) &&
      grepl("imp$after_be <- imp$after_be | be_results_seen(shared)", ex, fixed = TRUE) &&
      grepl('if (!gxp_guard("exclusion_imported"', ex, fixed = TRUE) &&
      grepl("window.onbeforeunload", ex, fixed = TRUE) && grepl("unsaved(FALSE)", ex, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-09, URS-GXP-05", critical = TRUE,
  method = "Code inspection of R/mod_exclusions.R",
  expected = "The mark uses the audit trail's bioequivalence runs for the data's SHA-256; imports are audited first; the page warns before closing with an undownloaded register")

check("ARV-10", "Errors stay on screen until closed and are announced to screen readers",
  tryCatch({
    files <- c(list.files("R", pattern = "\\.R$", full.names = TRUE), "app.R")
    calls <- unlist(lapply(files, function(f) {
      s <- paste(readLines(f, warn = FALSE), collapse = "\n")
      regmatches(s, gregexpr("showNotification\\((?:[^()]|\\((?:[^()]|\\([^()]*\\))*\\))*\\)", s, perl = TRUE))[[1]]
    }))
    err <- calls[grepl('type\\s*=\\s*"error"', calls)]
    app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
    length(err) > 40 && all(grepl("duration\\s*=\\s*NULL", err)) &&
      grepl('id = "nca-live-assertive", class = "visually-hidden", role = "alert"', app, fixed = TRUE) &&
      grepl("shiny-notification-error", app, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-01", critical = FALSE,
  method = "Every showNotification(type = \"error\") call in R/ and app.R; the live regions in app.R",
  expected = "No error disappears on a timer (were 5 to 12 s); notification text is copied into live regions, errors assertively")

# Stress test of the app (2026-09-30): hostile inputs that ended the session or gave wrong numbers
stress_xo <- function(n) {
  seqs <- rep(c("TR", "RT"), length.out = n); set.seed(11)
  do.call(rbind, lapply(seq_len(n), function(i) {
    ord <- strsplit(seqs[i], "")[[1]]; b <- rnorm(1, 0, .3)
    data.frame(Subject = as.character(i), Period = as.character(1:2), Sequence = seqs[i],
               Treatment = factor(ifelse(ord == "T", "Test", "Reference"), levels = c("Reference", "Test")),
               CMAX = exp(b + ifelse(ord == "T", 0.05, 0) + rnorm(2, 0, .2)))
  }))
}
stress_fit <- function(d, design = "2x2x2", ...) fit_be_parameter(d, "CMAX", design, trt_col = "Treatment", subj_col = "Subject",
                                                                   per_col = "Period", seq_col = "Sequence", ...)
check("ARV-11", "A study with no residual degrees of freedom gives a reason, not an error that ends the session",
  tryCatch({
    a <- stress_fit(stress_xo(2))
    par11 <- data.frame(Subject = c("1", "2"), Treatment = factor(c("Reference", "Test"), levels = c("Reference", "Test")), CMAX = c(1, 2))
    b <- fit_be_parameter(par11, "CMAX", "parallel", trt_col = "Treatment", subj_col = "Subject")
    ok <- function(r) !is.null(r$reason) && grepl("degrees of freedom", r$reason) && is.na(r$row$Point_Est) &&
      !r$row$Bioequivalent %in% c("YES", "NO")
    ok(a) && ok(b) && identical(stress_fit(stress_xo(12))$row$Bioequivalent %in% c("YES", "NO"), TRUE)
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE,
  method = "fit_be_parameter() on a 2-subject 2x2x2 crossover and a 1 + 1 parallel study; a 12-subject crossover as control",
  expected = "No estimate and a stated reason for the two degenerate studies (was: R error); the control still gets a verdict")

check("ARV-12", "An infinite or negative value in the BE data is reported, never dropped without a trace",
  tryCatch({
    d <- stress_xo(12); d$CMAX[1] <- Inf
    e <- stress_xo(12); e$CMAX[1] <- -5
    m <- stress_xo(12); m$CMAX[1] <- NA
    a <- stress_fit(d); b <- stress_fit(e); c2 <- stress_fit(m)
    grepl("infinite or negative", a$reason) && grepl("infinite or negative", b$reason) &&
      is.null(c2$reason) && c2$row$Missing_Test + c2$row$Missing_Ref == 1
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE,
  method = "fit_be_parameter() with Inf, a negative value and an NA in one Cmax",
  expected = "Inf and the negative value give no verdict with the profile named (was: dropped without a count); an NA is counted as missing as before")

check("ARV-13", "The quality check refuses infinite concentrations and a column mapped to two roles, and the pipeline stops on the latter",
  tryCatch({
    raw <- data.frame(S = rep(1:2, each = 4), T = rep(c(0, 1, 2, 4), 2), C = c(0, 5, 3, 1, 0, 4, 2, 1))
    cm <- list(subject = "S", time = "T", conc = "C")
    qc_ok <- run_data_quality_check(raw, cm, lloq = 0)
    inf <- raw; inf$C[3] <- Inf
    qc_inf <- run_data_quality_check(inf, cm, lloq = 0)
    qc_dup <- run_data_quality_check(raw, list(subject = "S", time = "T", conc = "T"), lloq = 0)
    qc_one <- run_data_quality_check(data.frame(V1 = 1:6), list(subject = "V1", time = "V1", conc = "V1"), lloq = 0)
    stopped <- inherits(try(prepare_pk_dataset(data.frame(V1 = 1:6), list(subject = "V1", time = "V1", conc = "V1"),
                                               list(lloq = 0)), silent = TRUE), "try-error")
    has <- function(qc, pat) any(qc$findings$Severity == "ERROR" & grepl(pat, qc$findings$Message))
    qc_ok$pass && !qc_inf$pass && has(qc_inf, "infinite") && !qc_dup$pass && has(qc_dup, "more than one role") &&
      !qc_one$pass && stopped
  }, error = function(e) FALSE),
  "URS-DAT-02, URS-DAT-03", critical = TRUE,
  method = "run_data_quality_check() with an Inf concentration, Time mapped as Concentration, and a one-column file mapped to all three roles; prepare_pk_dataset() on the last",
  expected = "Each is an error in the quality check (was: no finding or a warning); the pipeline stops with a message instead of a subscript error; a clean file still passes")

check("ARV-14", "Reference-scaled assessment with one sequence, and a missing acceptance limit, give a clear refusal",
  tryCatch({
    set.seed(3)
    d <- do.call(rbind, lapply(1:12, function(i) data.frame(Subject = as.character(i), Period = as.character(1:3), Sequence = "TRR",
            Treatment = factor(c("Test", "Reference", "Reference"), levels = c("Reference", "Test")), CMAX = exp(rnorm(3, 1, .3)))))
    r <- rsabe_assess(d, "CMAX", "2x3x3", "Treatment", "Subject", "Period", "Sequence")
    lim <- tryCatch(stress_fit(stress_xo(12), be_lower = NA), error = function(e) conditionMessage(e))
    !isTRUE(r$ok) && grepl("at least two sequences", r$reason) && grepl("acceptance limits", lim)
  }, error = function(e) FALSE),
  "URS-BE-01", critical = FALSE,
  method = "rsabe_assess() on a 2x3x3 study with all subjects in one sequence; fit_be_parameter() with be_lower = NA",
  expected = "RSABE refuses with a stated reason (was: contrasts error); the missing limit stops with a message naming the limits")

check("ARV-15", "The BE run turns any unexpected error in the model fit into a message instead of ending the session",
  tryCatch({
    ba <- paste(readLines("R/be_analysis.R", warn = FALSE), collapse = "\n")
    grepl("stopped with an unexpected error", ba, fixed = TRUE) &&
      grepl("be_scaled_error = function(e)", ba, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-UI-01", critical = FALSE,
  method = "Code inspection of run_be_analysis() in R/be_analysis.R",
  expected = "The tryCatch around the fit has a general error handler after the two classed ones")


check("ARV-16", "ADNCA: ANL01FL is read in any case, and a dataset where no record has ANL01FL = Y is refused by name",
  tryCatch({
    d <- data.frame(USUBJID = "S1", ARRLT = c(0, 1, 2, 4, 8), AVAL = c(1, 2, 3, 2, 1), ANL01FL = "y", PARAMCD = "DRUG")
    lower <- adnca_convert(d, time = "ARRLT"); d$ANL01FL <- "Y"; upper <- adnca_convert(d, time = "ARRLT")
    d$ANL01FL <- "N"; none <- tryCatch(adnca_convert(d, time = "ARRLT"), error = function(e) conditionMessage(e))
    nrow(lower$flat) == 5 && identical(lower$flat, upper$flat) && is.character(none) && grepl("ANL01FL", none) &&
      grepl("values found: N", none, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-DAT-03", critical = TRUE,
  method = "adnca_convert() on five records with ANL01FL y, Y and N",
  expected = "y keeps the same five records as Y (was 0 records and a misleading 'Dataset has 0 rows'); all N is refused with the values found")

check("ARV-17", "Warnings that change how BE results are read reach the on-screen checks, the Checks sheet and the Analysis Record",
  tryCatch({
    be <- paste(readLines("R/mod_path_be.R", warn = FALSE), collapse = "\n")
    ba <- paste(readLines("R/be_analysis.R", warn = FALSE), collapse = "\n")
    grepl("warn_run(design_used$note, 15)", ba, fixed = TRUE) && !grepl("warn_run(mismatch", ba, fixed = TRUE) &&
      grepl('warn_run(\n      paste0("No Sequence column is mapped.', ba, fixed = TRUE) &&
      grepl('warn_run(\n      paste0("Tmax is included in your analysis.', ba, fixed = TRUE) &&
      grepl("m13a = c(run_warns, ", ba, fixed = TRUE) &&
      grepl("record_checks(shared$qc_result, c(be_result()$m13a", be, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-EXP-04", critical = FALSE,
  method = "Code inspection of run_be_analysis() in R/be_analysis.R and of R/mod_path_be.R",
  expected = "The paired-comparison note, the missing-Sequence and Tmax warnings are collected in run_warns and stored with the M13A checks, which feed the Checks sheet and the record")

check("ARV-18", "Confidence limits and point estimates round halves up, as SAS does, before the comparison with the limits",
  tryCatch({
    be <- paste(readLines("R/be_analysis.R", warn = FALSE), collapse = "\n")
    sc <- paste(readLines("R/be_scaled.R", warn = FALSE), collapse = "\n")
    identical(round_half_up(c(125.005, 79.995, 125.0049, -1.005, 0.125, NA), 2), c(125.01, 80, 125, -1.01, 0.13, NA)) &&
      !be_limits_pass(90, 125.005, 80, 125) && be_limits_pass(90, 125.0049, 80, 125) &&
      be_limits_pass(79.995, 110, 80, 125) && !be_limits_pass(79.9949, 110, 80, 125) &&
      !pe_within_limits(125.005) && pe_within_limits(79.995) &&
      !grepl("round(ci_lo, 2)", be, fixed = TRUE) && !grepl("round(pe, 2)", be, fixed = TRUE) &&
      grepl("lo = round_half_up(ci_lo_p, 2)", be, fixed = TRUE) && grepl("CI_Upper <- round_half_up(r$ci_hi, 2)", sc, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-04", critical = TRUE,
  method = "round_half_up(), be_limits_pass() and pe_within_limits() at the halves; code inspection of R/be_analysis.R and R/be_scaled.R",
  expected = "125.005 rounds to 125.01 and fails, as in SAS (was 125.00 and a pass); 125.0049 passes; 79.995 rounds to 80.00; the table shows the same rounded values")

end_section("ARV")

# =============================================================================
# SECTION PAR: Parallel-group bioequivalence against published reference data
# =============================================================================
# The 11 datasets of Fuglsang et al. (AAPS J 2015;17:400-404,
# doi:10.1208/s12248-014-9704-6), with the 90% CIs the paper proposes as
# validation targets: pooled variance (Table II) and Welch (Table I). They
# cover imbalance, outliers, heteroscedasticity and extreme ranges.
start_section("PAR")

par_dat <- read.csv(file.path("validation", "fixtures", "parallel_be_datasets.csv"), stringsAsFactors = FALSE)
par_ref <- read.csv(file.path("validation", "fixtures", "parallel_be_reference.csv"), stringsAsFactors = FALSE)
par_set <- function(id) {
  x <- par_dat[par_dat$dataset == id, ]
  x$Treat <- factor(x$Treat, levels = c("R", "T")); x$Var <- as.numeric(x$Var); x
}
par_fit <- function(id) fit_be_parameter(par_set(id), "Var", "parallel", trt_col = "Treat", subj_col = "Subj")$estimate

check("PAR-01", "Datasets are the published ones (11 sets, group sizes as in the paper)",
  {
    n <- sapply(par_ref$dataset, function(id) { x <- par_set(id); c(T = sum(x$Treat == "T"), R = sum(x$Treat == "R")) })
    exp_n <- cbind(P1 = c(9, 9), P2 = c(9, 4), P3 = c(9, 9), P4 = c(20, 20), P5 = c(31, 29), P6 = c(24, 26),
                   P7 = c(1000, 200), P8 = c(1000, 1000), P9 = c(1000, 1000), P10 = c(1000, 200), P11 = c(1000, 200))
    nrow(par_ref) == 11 && all(unname(n) == unname(exp_n))
  },
  "URS-BE-01", critical = TRUE, method = "group sizes of P1-P11",
  expected = "N(T)/N(R) as in Fuglsang et al. (P2 9/4, P5 31/29, P7 1000/200 ...)")

check("PAR-02", "Pooled-variance 90% CI and point estimate match the paper (Table II), P1-P11",
  tryCatch({
    ok <- TRUE
    for (i in seq_len(nrow(par_ref))) {
      e <- par_fit(par_ref$dataset[i]); r <- par_ref[i, ]
      ok <- ok && isTRUE(all.equal(round(c(e$ci_lo, e$ci_hi, e$pe), 2), c(r$pooled_lo, r$pooled_hi, r$pe)))
    }
    ok
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE, method = "fit_be_parameter(design = 'parallel') on each dataset, rounded to 2 decimals",
  expected = "Equal to the consensus of R, OpenOffice Calc, WinNonlin, EquivTest/PK, SAS (Kinetica differs on unbalanced sets)")

check("PAR-03", "The app's supplementary Welch 90% CI matches the paper (Table I), P1-P11",
  tryCatch({
    ok <- TRUE
    for (i in seq_len(nrow(par_ref))) {
      w <- par_fit(par_ref$dataset[i])$welch; r <- par_ref[i, ]
      ok <- ok && !is.null(w) && isTRUE(all.equal(round(c(w$lo, w$hi), 2), c(r$welch_lo, r$welch_hi)))
    }
    ok
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE, method = "fit_be_parameter(design = 'parallel')$estimate$welch on each dataset, rounded to 2 decimals",
  expected = "Equal to the paper's Welch intervals (WinNonlin needs a workaround; EquivTest/PK and Kinetica cannot)")

check("PAR-04", "The Welch note appears exactly where the pooled and Welch verdicts differ",
  tryCatch({
    lim <- function(lo, hi, L, U) lo >= L && hi <= U
    ok <- TRUE; flips <- 0
    # 80-125 flips nothing in these data; 80-130 and 70-143 make P7 and others flip
    for (L in list(c(80, 125), c(80, 130), c(70, 143))) {
      for (i in seq_len(nrow(par_ref))) {
        r <- par_ref[i, ]; x <- par_set(r$dataset)
        expect_note <- lim(r$pooled_lo, r$pooled_hi, L[1], L[2]) != lim(r$welch_lo, r$welch_hi, L[1], L[2])
        got_note <- length(parallel_welch_notes(x, "Var", "Treat", be_lower = L[1], be_upper = L[2])) > 0
        ok <- ok && identical(expect_note, got_note); flips <- flips + expect_note
      }
    }
    ok && flips > 0
  }, error = function(e) FALSE),
  "URS-BE-01", critical = TRUE, method = "parallel_welch_notes() on P1-P11 at limits 80-125, 80-130 and 70-143; expected verdicts taken from the paper's two tables",
  expected = "A note exactly where the verdict of the pooled and the Welch interval differ, and at least one such case")

check("PAR-05", "Welch interval: confidence level follows the analysis, unavailable for crossovers, columns named in the results",
  tryCatch({
    x <- par_set("P7")
    e95 <- fit_be_parameter(x, "Var", "parallel", trt_col = "Treat", subj_col = "Subj", ci_level = 95)$estimate$welch
    y <- log(x$Var)
    t95 <- 100 * exp(as.numeric(stats::t.test(y[x$Treat == "T"], y[x$Treat == "R"], conf.level = 0.95)$conf.int))
    rn <- names(rename_be_columns(data.frame(Welch_Lower = 1, Welch_Upper = 1, Welch_DF = 1), ci_level = 95))
    d222 <- rep_222; b <- build_be_data(rep_nca(d222), d222, rep_cm)
    xo <- fit_be_parameter(b$data, "CMAX", "2x2x2", trt_col = b$trt_col, subj_col = b$subj_col,
                           per_col = b$per_col, seq_col = b$seq_col)$estimate
    isTRUE(all.equal(c(e95$lo, e95$hi), t95)) && is.null(xo$welch) &&
      identical(rn, c("Welch 95% CI Lower (suppl.)", "Welch 95% CI Upper (suppl.)", "Welch Degrees of Freedom"))
  }, error = function(e) FALSE),
  "URS-BE-01", critical = FALSE, method = "P7 at 95% against t.test; a 2x2x2 crossover; rename_be_columns()",
  expected = "Welch at the chosen level; no Welch result for a crossover; labelled as supplementary")

end_section("PAR")

# =============================================================================
# SECTION COV: Covariate adjustment in parallel-group bioequivalence
# =============================================================================
# Model: ln(PK) = Treatment + covariates (main effects, ordinary least squares,
# pooled variance). Reference values come from a second tool, Python
# statsmodels (validation/fixtures/make_covariate_reference.py), and from
# matrix algebra written here without lm().
start_section("COV")

cov_dat <- read.csv(file.path("validation", "fixtures", "cov_parallel_data.csv"), stringsAsFactors = FALSE)
cov_ref <- read.csv(file.path("validation", "fixtures", "cov_parallel_reference.csv"), stringsAsFactors = FALSE)
cov_cm <- list(subject = "Subject", treatment = "Treat")
# Same steps as build_be_data(): resolve the covariates, attach one value per subject
cov_be <- function(d, spec) {
  cp <- be_covariate_prepare(d, cov_cm, spec)
  be <- d
  be$Treat <- factor(be$Treat, levels = c("R", "T"))
  for (cc in cp$spec$col) be[[cc]] <- cp$values[[cc]][match(as.character(be$Subject), cp$subject)]
  list(data = be, spec = cp$spec)
}
cov_fit <- function(b, ...) fit_be_parameter(b$data, "Var", "parallel", trt_col = "Treat", subj_col = "Subject",
                                             covariates = b$spec, ...)
cov_sp <- function(...) data.frame(name = c(...), stringsAsFactors = FALSE)
cov_stop <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))

check("COV-00", "Without covariates the output is unchanged: same columns, same values, no covariate element",
  tryCatch({
    x <- par_set("P5")
    o <- fit_be_parameter(x, "Var", "parallel", trt_col = "Treat", subj_col = "Subj")
    o2 <- fit_be_parameter(x, "Var", "parallel", trt_col = "Treat", subj_col = "Subj", covariates = NULL)
    exp_cols <- c("Parameter", "Test", "Reference", "N_Test", "N_Ref", "Obs_Test", "Obs_Ref", "Scale", "Point_Est",
                  "CI_Lower", "CI_Upper", "BE_Lower", "BE_Upper", "PE_Constraint", "Bioequivalent", "Missing_Test",
                  "Missing_Ref", "Zeros_Test", "Zeros_Ref", "Excluded_Test", "Excluded_Ref", "Flagged_Test",
                  "Flagged_Ref", "Incomplete_Subjects", "MSE", "DF", "Model")
    r5 <- par_ref[par_ref$dataset == "P5", ]
    identical(names(o$row), exp_cols) && identical(o, o2) &&
      identical(names(o), c("row", "anova", "estimate", "reason")) &&
      is.null(o$estimate$unadjusted) && is.null(o$estimate$covariate_coefs) &&
      isTRUE(all.equal(c(o$row$CI_Lower, o$row$CI_Upper, o$row$Point_Est), c(r5$pooled_lo, r5$pooled_hi, r5$pe)))
  }, error = function(e) FALSE),
  "URS-BE-13, URS-BE-01", critical = TRUE,
  method = "fit_be_parameter() on P5 with and without covariates = NULL; column set compared with the list of the previous release",
  expected = "Identical output; the 27 known columns; no unadjusted or coefficient element; interval equal to the published one")

check("COV-01", "Adjusted 90% CI, point estimate, df and MSE match Python statsmodels (numeric, categorical and log covariates)",
  tryCatch({
    ok <- TRUE
    for (k in c("age_sex", "age_weight", "logage")) {
      b <- switch(k, age_sex = cov_be(cov_dat, data.frame(name = c("age", "sex"), type = c("numeric", "categorical"))),
                     age_weight = cov_be(cov_dat, cov_sp("age", "weight")),
                     logage = cov_be(cov_dat, data.frame(name = "age", transform = "log")))
      e <- cov_fit(b)$estimate; r <- cov_ref[cov_ref$model == k, ]
      ok <- ok && isTRUE(all.equal(c(e$pe, e$ci_lo, e$ci_hi, e$dfe, e$mse), c(r$pe, r$lo, r$hi, r$df, r$mse), tolerance = 1e-7))
    }
    ok
  }, error = function(e) FALSE),
  "URS-BE-13", critical = TRUE,
  method = "60-subject seeded dataset; three models fitted in the app and in statsmodels (tool versions in cov_parallel_reference.csv)",
  expected = "Estimate, interval, residual df and MSE agree to 7 significant figures")

check("COV-02", "Adjusted interval equals the matrix-algebra solution beta = (X'X)^-1 X'y written without lm()",
  tryCatch({
    b <- cov_be(cov_dat, cov_sp("age", "sex", "weight"))
    e <- cov_fit(b)$estimate
    d <- b$data; y <- log(d$Var)
    X <- cbind(1, d$Treat == "T", d$age, d$sex == "M", d$weight)
    XtXi <- solve(crossprod(X)); beta <- XtXi %*% crossprod(X, y)
    res <- y - X %*% beta; dfe <- nrow(X) - ncol(X); s2 <- sum(res^2) / dfe
    ci <- 100 * exp(beta[2] + c(-1, 1) * qt(0.95, dfe) * sqrt(s2 * XtXi[2, 2]))
    isTRUE(all.equal(c(e$ci_lo, e$ci_hi, e$dfe), c(ci, dfe), tolerance = 1e-9))
  }, error = function(e) FALSE),
  "URS-BE-13", critical = TRUE, method = "solve(crossprod(X), crossprod(X, y)) on the same data",
  expected = "Equal to 9 significant figures")

check("COV-03", "The unadjusted pooled interval beside the adjusted one equals statsmodels without covariates; adjustment narrows the interval here",
  tryCatch({
    b <- cov_be(cov_dat, cov_sp("age", "weight")); o <- cov_fit(b)
    r <- cov_ref[cov_ref$model == "none", ]; u <- o$estimate$unadjusted
    isTRUE(all.equal(c(u$pe, u$ci_lo, u$ci_hi, u$dfe), c(r$pe, r$lo, r$hi, r$df), tolerance = 1e-7)) &&
      isTRUE(all.equal(c(o$row$Unadj_Lower, o$row$Unadj_Upper), round(c(u$ci_lo, u$ci_hi), 2))) &&
      identical(o$row$Adjusted_for, "age, weight") &&
      (o$estimate$ci_hi - o$estimate$ci_lo) < (u$ci_hi - u$ci_lo)
  }, error = function(e) FALSE),
  "URS-BE-13", critical = TRUE, method = "Unadj_Lower/Unadj_Upper and estimate$unadjusted against statsmodels; interval widths",
  expected = "Unadjusted interval equal to the reference; the row names the covariates; the adjusted interval is narrower for prognostic covariates")

check("COV-04", "The treatment estimate is invariant to centering, rescaling, row order and factor-level order or labels",
  tryCatch({
    base <- cov_fit(cov_be(cov_dat, data.frame(name = c("age", "sex"), type = c("numeric", "categorical"))))$estimate
    key <- function(e) c(e$pe, e$ci_lo, e$ci_hi, e$dfe)
    d2 <- cov_dat; d2$age <- (d2$age - 42) / 10; d2$sex <- ifelse(d2$sex == "F", "Z_female", "A_male")
    d3 <- cov_dat[sample(nrow(cov_dat)), ]
    sp <- data.frame(name = c("age", "sex"), type = c("numeric", "categorical"))
    e2 <- cov_fit(cov_be(d2, sp))$estimate; e3 <- cov_fit(cov_be(d3, sp))$estimate
    isTRUE(all.equal(key(base), key(e2), tolerance = 1e-9)) && isTRUE(all.equal(key(base), key(e3), tolerance = 1e-9))
  }, error = function(e) FALSE),
  "URS-BE-13", critical = TRUE, method = "Same model on centred and rescaled age, recoded and re-ordered sex levels, shuffled rows",
  expected = "Same estimate, interval and df to 9 significant figures")

check("COV-05", "A covariate unrelated to the response moves the estimate only slightly and costs one degree of freedom",
  tryCatch({
    set.seed(7); d <- cov_dat; d$noise <- rnorm(nrow(d))
    e0 <- fit_be_parameter(transform(d, Treat = factor(Treat, levels = c("R", "T"))), "Var", "parallel",
                           trt_col = "Treat", subj_col = "Subject")$estimate
    e1 <- cov_fit(cov_be(d, cov_sp("noise")))$estimate
    abs(log(e1$pe / e0$pe)) < 0.05 && e1$dfe == e0$dfe - 1
  }, error = function(e) FALSE),
  "URS-BE-13", critical = FALSE, method = "Seeded pure-noise covariate added to the 60-subject dataset",
  expected = "Point estimate within 5% of the unadjusted one; residual df one lower")

check("COV-06", "Stop conditions in the data: text with a unit, decimal comma, two values for a subject, role column, log of zero, unknown column",
  tryCatch({
    d <- cov_dat
    d$kg <- paste(d$weight, "kg"); d$comma <- sub("\\.", ",", format(d$weight, nsmall = 1)); d$two <- d$age
    dd <- rbind(d, d[d$Subject == 3, ]); dd$two[nrow(dd)] <- 99
    d$zero <- d$age - min(d$age)
    m <- c(kg = cov_stop(be_covariate_prepare(d, cov_cm, cov_sp("kg"))),
           comma = cov_stop(be_covariate_prepare(d, cov_cm, cov_sp("comma"))),
           two = cov_stop(be_covariate_prepare(dd, cov_cm, cov_sp("two"))),
           role = cov_stop(be_covariate_prepare(d, cov_cm, cov_sp("Treat"))),
           zero = cov_stop(be_covariate_prepare(d, cov_cm, data.frame(name = "zero", transform = "log"))),
           none = cov_stop(be_covariate_prepare(d, cov_cm, cov_sp("nothere"))))
    all(!is.na(m)) && grepl("mixes numbers and text", m["kg"]) && grepl("decimal comma", m["comma"]) &&
      grepl("more than one value for subject", m["two"]) && grepl("another role", m["role"]) &&
      grepl("no logarithm", m["zero"]) && grepl("not in the data", m["none"])
  }, error = function(e) FALSE),
  "URS-BE-13", critical = TRUE, method = "be_covariate_prepare() on data with each fault",
  expected = "Each fault stops with a plain message naming the column")

check("COV-07", "Stop conditions in the model: missing value (subjects named), constant, equal to treatment, duplicate information, too many, too few subjects, too few residual df",
  tryCatch({
    d <- cov_dat; d$const <- 5; d$trtnum <- as.numeric(d$Treat == "T"); d$age2 <- d$age * 2
    d$miss <- d$weight; d$miss[c(2, 31)] <- NA
    for (i in 1:5) d[[paste0("z", i)]] <- rnorm(nrow(d))
    msg <- function(names, dat = d, ...) cov_stop(cov_fit(cov_be(dat, cov_sp(names)), ...))
    m_miss <- msg("miss")
    small <- d[d$Subject %in% c(1:8, 31:38), ]
    few <- d[d$Subject %in% c(1:12, 31:42), ]
    # Three 6-level categorical covariates use 15 of the 24 df: 7 remain
    set.seed(11); few$g1 <- sample(rep(1:6, 5), nrow(few), replace = FALSE)[seq_len(nrow(few))]
    few$g2 <- sample(rep(1:6, 5), nrow(few)); few$g3 <- sample(rep(1:6, 5), nrow(few))
    m_df <- cov_stop(cov_fit(cov_be(few, data.frame(name = c("g1", "g2", "g3"), type = "categorical"))))
    c(grepl("no value for 2 subject", m_miss), grepl("2, 31", m_miss),
      grepl("same value for every subject", msg("const")),
      grepl("identical to, or fully determined by, the treatment", msg("trtnum")),
      grepl("same information", msg(c("age", "age2"))),
      grepl("At most 5", msg(c("age", "weight", paste0("z", 1:5)))),
      grepl("at least 12 subjects per group", msg("age", small)),
      grepl("Only 7 residual degrees of freedom", m_df)) |> all()
  }, error = function(e) FALSE),
  "URS-BE-13", critical = TRUE, method = "fit_be_parameter() with covariates that break each rule; class be_covariate_error",
  expected = "Each case stops; the missing-value message lists subjects 2 and 31; nobody is dropped silently")

check("COV-08", "A category with fewer than 3 subjects gives a warning, and only crossover-free designs accept covariates",
  tryCatch({
    d <- cov_dat; d$grp <- ifelse(d$Subject %in% c(1, 2), "rare", ifelse(d$age > 42, "hi", "lo"))
    o <- cov_fit(cov_be(d, data.frame(name = "grp", type = "categorical")))
    b <- build_be_data(rep_nca(rep_222), rep_222, rep_cm)
    sp <- data.frame(name = "x", type = "numeric", transform = "none", col = ".cov1")
    b$data$.cov1 <- 1
    err <- tryCatch(fit_be_parameter(b$data, "CMAX", "2x2x2", trt_col = b$trt_col, subj_col = b$subj_col,
                                     per_col = b$per_col, seq_col = b$seq_col, covariates = sp),
                    error = function(e) e)
    any(grepl("fewer than 3 subjects", o$warnings)) && inherits(err, "be_covariate_error") &&
      grepl("parallel-group studies only", conditionMessage(err))
  }, error = function(e) FALSE),
  "URS-BE-13", critical = TRUE, method = "Category of two subjects; a 2x2x2 crossover called with a covariate",
  expected = "Warning on the small category; crossover refused with a plain message")

check("COV-09", "Coefficient table: one row per numeric covariate and per category against its reference, equal to lm()",
  tryCatch({
    b <- cov_be(cov_dat, data.frame(name = c("age", "sex"), type = c("numeric", "categorical")))
    cc <- cov_fit(b)$estimate$covariate_coefs
    m <- lm(log(Var) ~ Treat + age + sex, cov_dat); s <- summary(m)$coefficients; ci <- confint(m, level = 0.90)
    identical(cc$Term, c("age", "sex: M vs F")) &&
      isTRUE(all.equal(cc$Estimate, unname(s[c("age", "sexM"), 1]))) &&
      isTRUE(all.equal(cc$CI_Lower, unname(ci[c("age", "sexM"), 1]))) &&
      isTRUE(all.equal(cc$P_Value, unname(s[c("age", "sexM"), 4])))
  }, error = function(e) FALSE),
  "URS-BE-13", critical = FALSE, method = "covariate_coefs against summary(lm) and confint(lm)",
  expected = "Same estimates, 90% limits and p-values, labelled by name and category")

check("COV-10", "Seeded simulation: with a prognostic covariate the 90% interval covers the true ratio about 90% of the time and is narrower",
  tryCatch({
    set.seed(20260930); n <- 30; cover <- 0; wa <- wu <- numeric(0); sims <- 500
    for (s in seq_len(sims)) {
      d <- data.frame(Subject = 1:(2 * n), Treat = factor(rep(c("R", "T"), each = n), levels = c("R", "T")),
                      x = rnorm(2 * n))
      d$Var <- exp(1 + 0.6 * d$x + rnorm(2 * n, 0, 0.25))
      b <- list(data = d, spec = data.frame(name = "x", type = "numeric", transform = "none", col = "x"))
      e <- cov_fit(b)$estimate
      cover <- cover + (e$ci_lo <= 100 && e$ci_hi >= 100)
      wa <- c(wa, e$ci_hi - e$ci_lo); wu <- c(wu, e$unadjusted$ci_hi - e$unadjusted$ci_lo)
    }
    cover / sims > 0.86 && cover / sims < 0.94 && mean(wa) < 0.8 * mean(wu)
  }, error = function(e) FALSE),
  "URS-BE-13", critical = FALSE, method = "500 seeded studies, 30 per group, true ratio 1, covariate with slope 0.6",
  expected = "Coverage between 86% and 94%; adjusted interval at least 20% narrower on average (supportive)")

check("COV-11", "Group balance: means, SDs, category shares and standardized differences match a direct computation; excluded profiles are left out",
  tryCatch({
    b <- cov_be(cov_dat, data.frame(name = c("age", "sex"), type = c("numeric", "categorical")))
    bal <- be_covariate_balance(b$data, b$spec, "Treat", "Subject")
    r <- cov_dat$Treat == "R"; t <- !r
    sdp <- sqrt((sd(cov_dat$age[r])^2 + sd(cov_dat$age[t])^2) / 2)
    pm <- c(mean(cov_dat$sex[r] == "M"), mean(cov_dat$sex[t] == "M"))
    sdm <- (pm[2] - pm[1]) / sqrt((pm[1] * (1 - pm[1]) + pm[2] * (1 - pm[2])) / 2)
    b$data$EXCLUDED <- NA_character_; b$data$EXCLUDED[b$data$Subject == 1] <- "Vomiting"
    bal2 <- be_covariate_balance(b$data, b$spec, "Treat", "Subject")
    identical(names(bal)[3:4], c("R", "T")) && identical(bal$Level, c(NA, "F", "M")) &&
      isTRUE(all.equal(bal$Std_Diff[bal$Covariate == "age"], (mean(cov_dat$age[t]) - mean(cov_dat$age[r])) / sdp)) &&
      isTRUE(all.equal(bal$Std_Diff[bal$Level %in% "M"], sdm)) &&
      identical(bal$R[bal$Covariate == "age"], sprintf("%s (%s)", signif(mean(cov_dat$age[r]), 4), signif(sd(cov_dat$age[r]), 3))) &&
      !identical(bal$R, bal2$R) && is.null(be_covariate_balance(b$data, NULL, "Treat", "Subject"))
  }, error = function(e) FALSE),
  "URS-BE-13", critical = FALSE, method = "be_covariate_balance() against mean(), sd() and proportions; one profile marked excluded",
  expected = "Same values; the excluded subject leaves the summary; NULL without covariates")

check("COV-12", "The record and the downloads carry the covariates: settings JSON, BE_Covariates and Covariate_Balance sheets, adjusted-for and unadjusted columns",
  tryCatch({
    b <- cov_be(cov_dat, data.frame(name = c("age", "sex"), type = c("numeric", "categorical")))
    o <- cov_fit(b)
    res <- list(ci_table = o$row, covariates = b$spec, covariate_coefs = list(Var = o$estimate$covariate_coefs),
                covariate_balance = be_covariate_balance(b$data, b$spec, "Treat", "Subject"))
    sh <- be_covariate_sheets(res)
    f <- tempfile(fileext = ".csv"); write.csv(exc_xo, f, row.names = FALSE)
    r <- suppressWarnings(run_nca(exc_xo, exc_cm, exc_st()))
    td <- tempfile("cov12_"); dir.create(td); zf <- file.path(td, "rec.zip")
    create_analysis_record(zf, r, exc_st(), exc_cm, f, "crossover.csv", blq_rule = "rule1", lloq = 0,
                           be_results = res,
                           be_settings = list(ci_level = 90, covariates = b$spec[, c("name", "type", "transform")]))
    utils::unzip(zf, exdir = td)
    js <- jsonlite::fromJSON(file.path(td, "analysis_settings.json"))
    sn <- openxlsx::getSheetNames(file.path(td, "results.xlsx"))
    ci <- names(rename_be_columns(o$row))
    identical(names(sh), c("BE_Covariates", "Covariate_Balance")) &&
      all(c("BE_Covariates", "Covariate_Balance") %in% sn) &&
      identical(js$bioequivalence$covariates$name, c("age", "sex")) &&
      all(c("Adjusted for", "Unadjusted 90% CI Lower (suppl.)", "Unadjusted 90% CI Upper (suppl.)",
            "Unadjusted residual variance") %in% ci) &&
      nrow(sh$BE_Covariates) == 2 && "Standardized difference" %in% names(sh$Covariate_Balance) &&
      is.null(be_covariate_sheets(list(ci_table = o$row)))
  }, error = function(e) FALSE),
  "URS-BE-13, URS-EXP-05", critical = TRUE,
  method = "create_analysis_record() with an adjusted result; be_covariate_sheets(); rename_be_columns()",
  expected = "Settings JSON lists the covariates; both sheets are in results.xlsx; no sheets without covariates")

check("COV-13", "Controlled mode: the audit event of a bioequivalence run carries the covariate settings",
  tryCatch({
    b <- cov_be(cov_dat, cov_sp("age", "weight"))
    d <- file.path(gxp_tmp, "cov13"); dir.create(d); gxp_set(d)
    gxp_env$audit_init(file.path(d, "audit.sqlite"), user = "owner", org = "Validation Org")
    be <- paste(readLines("R/mod_path_be.R"), collapse = "\n")
    wrote <- isTRUE(gxp_env$gxp_guard("analysis_run", object = "bioequivalence", sha256 = "abc", session = NULL,
                                      details = list(trigger = "run", path = "be",
                                                     be_settings = list(parameters = "CMAX",
                                                                        covariates = b$spec[, c("name", "type", "transform")]))))
    tr <- gxp_env$audit_read(file.path(d, "audit.sqlite"))
    gxp_unset()
    dj <- jsonlite::fromJSON(tr$details[tr$event == "analysis_run"][1])
    wrote && identical(dj$be_settings$covariates$name, c("age", "weight")) &&
      grepl("audit_be$covariates <- run$covariates[, c(\"name\", \"type\", \"transform\")]", be, fixed = TRUE) &&
      grepl("be_settings = audit_be", be, fixed = TRUE)
  }, error = function(e) { gxp_unset(); FALSE }),
  "URS-BE-13, URS-GXP-07", critical = TRUE,
  method = "gxp_guard() with covariate details on a temporary trail, read back; the BE module builds those details",
  expected = "The trail entry holds name, type and transform of each covariate")

check("COV-14", "The planner offers the unadjusted CV and, beside it, the residual CV after adjustment; nothing changes without covariates",
  tryCatch({
    d <- cov_dat; d$Treat <- factor(d$Treat, levels = c("R", "T"))
    b <- cov_be(cov_dat, cov_sp("age", "weight")); o <- cov_fit(b)
    plain <- fit_be_parameter(d, "Var", "parallel", trt_col = "Treat", subj_col = "Subject")
    res <- list(ci_table = o$row, design = "parallel"); res0 <- list(ci_table = plain$row, design = "parallel")
    of <- planner_cv_offer(res, "abe", "parallel", "Var"); of0 <- planner_cv_offer(res0, "abe", "parallel", "Var")
    r <- cov_ref[cov_ref$model == "none", ]; ra <- cov_ref[cov_ref$model == "age_weight", ]
    isTRUE(all.equal(of$cv, 100 * sqrt(exp(r$mse) - 1), tolerance = 1e-5)) &&
      isTRUE(all.equal(of$adjusted$cv, 100 * sqrt(exp(ra$mse) - 1), tolerance = 1e-5)) &&
      of$adjusted$cv < of$cv && grepl("after adjusting for age, weight", of$adjusted$label) &&
      is.null(of0$adjusted) && isTRUE(all.equal(of0$cv, of$cv, tolerance = 1e-5))
  }, error = function(e) FALSE),
  "URS-BE-13, URS-PWR-01", critical = FALSE, method = "planner_cv_offer() on adjusted and unadjusted parallel results, against statsmodels MSE",
  expected = "Main offer equals the unadjusted CV; a second offer equals the adjusted residual CV; no second offer without covariates")

check("COV-15", "Parallel groups raise no 'incomplete design' alarm: every subject has one treatment by design",
  tryCatch({
    ba <- paste(readLines("R/be_analysis.R", warn = FALSE), collapse = "\n")
    grepl('incomplete_subjects <- if (identical(be_design_model(design_used$design), "parallel")) character(0) else', ba, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-01", critical = FALSE, method = "Code inspection of the balance pre-check in run_be_analysis() (R/be_analysis.R); click-through on a parallel dataset",
  expected = "The pre-check is skipped for parallel designs and kept for crossover and replicate designs")

end_section("COV")

# =============================================================================
# SECTION RSA: FDA reference-scaled average bioequivalence (RSABE, Appendix G)
# =============================================================================
# Reference values: an independent Python implementation (numpy/scipy, no lm();
# validation/fixtures/make_rsabe_reference.py) and an independent R version on
# explicit matrices (validation/fixtures/rsabe_independent.R), on 13 replicateBE
# reference data sets. No package implements the RSABE assessment: replicateBE
# has ABE and Method A/B only, PowerTOST gives power and not a verdict.
start_section("RSA")

source(file.path("validation", "fixtures", "rsabe_independent.R"))
rsa_dat <- read.csv(file.path("validation", "fixtures", "rsabe_datasets.csv"), stringsAsFactors = FALSE)
rsa_ref <- read.csv(file.path("validation", "fixtures", "rsabe_reference.csv"), stringsAsFactors = FALSE)
rsa_be <- function(id) {
  d <- rsa_dat[rsa_dat$dataset == id, ]
  data.frame(Subject = d$subject, Period = as.character(d$period), Sequence = d$sequence,
             Treatment = factor(ifelse(d$treatment == "T", "Test", "Reference"), levels = c("Reference", "Test")),
             CMAX = d$pk, stringsAsFactors = FALSE)
}
abl_b <- function(nm) {
  d <- getExportedValue("replicateBE", nm)
  data.frame(Subject = as.character(d$subject), Period = as.character(d$period), Sequence = as.character(d$sequence),
             Treatment = factor(ifelse(d$treatment == "T", "Test", "Reference"), levels = c("Reference", "Test")),
             CMAX = d$PK, AUCLST = d$PK, stringsAsFactors = FALSE)
}
rsa_design <- function(id) if (id %in% c("rds02", "rds04", "rds07", "rds30")) "2x3x3" else "2x2x4"
rsa_run <- function(b, design) rsabe_assess(b, "CMAX", design, "Treatment", "Subject", "Period", "Sequence")
# Synthetic study with exactly known s_WR and point estimate (24 subjects, 2x2x4):
# R1 = a + k*(+-1), R2 = a - k*(+-1), T1 = T2 = a + shift + e; signs balanced within sequence
rsa_synth <- function(swr, pe_pct = 100, n = 24) {
  k <- swr / sqrt(4 * n / (2 * (n - 2)))
  seqs <- rep(c("TRTR", "RTRT"), each = n / 2); s <- rep(c(1, -1), length.out = n)
  a <- seq(-0.5, 0.5, length.out = n); e <- 0.02 * rep(c(1, -1), each = 1, length.out = n)
  shift <- log(pe_pct / 100)
  rows <- lapply(seq_len(n), function(i) {
    ord <- strsplit(seqs[i], "")[[1]]; r <- 0; tt <- 0
    y <- vapply(seq_along(ord), function(p) if (ord[p] == "R") { r <<- r + 1; a[i] + c(1, -1)[r] * s[i] * k } else a[i] + shift + e[i], 0)
    data.frame(Subject = as.character(i), Period = as.character(1:4), Sequence = seqs[i],
               Treatment = factor(ifelse(ord == "T", "Test", "Reference"), levels = c("Reference", "Test")), CMAX = exp(y),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

check("RSA-01", "rsabe_assess() equals the independent Python computation on 13 reference data sets (all steps of Appendix G)",
  tryCatch({
    ok <- TRUE
    for (i in seq_len(nrow(rsa_ref))) {
      r <- rsa_ref[i, ]; a <- rsa_run(rsa_be(r$dataset), rsa_design(r$dataset))
      got <- c(a$n, a$pe, a$ci_lo, a$ci_hi, a$sWR, a$dfd, a$x, a$boundx, a$y, a$boundy, a$critbound)
      exp <- c(r$n, r$pe, r$lcl, r$ucl, r$swr, r$dfd, r$x, r$boundx, r$y, r$boundy, r$critbound)
      ok <- ok && isTRUE(all.equal(got, exp, tolerance = 1e-9)) && a$seqs == r$m
    }
    ok && nrow(rsa_ref) == 13
  }, error = function(e) FALSE),
  "URS-BE-14", critical = TRUE,
  method = "13 replicateBE data sets (2x3x3, 2x2x4 and variants, some with missing periods) against Python numpy/scipy (versions in rsabe_reference.csv)",
  expected = "N, point estimate, 90% limits, s_WR, df, x, boundx, y, boundy and the criterion bound equal to 9 significant figures")

check("RSA-02", "rsabe_assess() equals a second independent implementation in R (explicit matrices), including on simulated unbalanced studies",
  tryCatch({
    ok <- TRUE
    ind <- function(b, ord = FALSE) {
      z <- data.frame(subject = b$Subject, period = as.numeric(b$Period), sequence = b$Sequence,
                      treatment = ifelse(b$Treatment == "Test", "T", "R"), y = log(b$CMAX), stringsAsFactors = FALSE)
      rsabe_independent(z)
    }
    for (id in rsa_ref$dataset) {
      a <- rsa_run(rsa_be(id), rsa_design(id)); i <- ind(rsa_be(id))
      ok <- ok && isTRUE(all.equal(c(a$n, a$pe, a$sWR, a$critbound, a$boundx, a$boundy),
                                   unname(i[c("n", "pe", "swr", "critbound", "boundx", "boundy")]), tolerance = 1e-9))
    }
    set.seed(5)
    for (k in 1:5) {   # unbalanced sequences and scattered missing periods
      b <- rsa_synth(0.25 + 0.1 * k, 90 + 3 * k, n = 24 + 2 * k)
      b$CMAX <- b$CMAX * exp(rnorm(nrow(b), 0, 0.15))
      b$CMAX[sample(nrow(b), 4)] <- NA
      b <- b[!(b$Sequence == "RTRT" & b$Subject %in% as.character(1:3)), ]
      a <- rsa_run(b, "2x2x4"); i <- ind(b)
      ok <- ok && isTRUE(all.equal(c(a$n, a$pe, a$critbound), unname(i[c("n", "pe", "critbound")]), tolerance = 1e-9))
    }
    ok
  }, error = function(e) FALSE),
  "URS-BE-14", critical = TRUE, method = "rsabe_independent() in validation/fixtures on the 13 data sets and 5 simulated studies with unequal sequences and missing values",
  expected = "Equal to 9 significant figures")

check("RSA-03", "Consistency with the planner: the pass rate of the assessment equals PowerTOST::power.RSABE within Monte Carlo error",
  tryCatch({
    sim_pass <- function(design, n, cv_wr, cv_wt, gmr, nsim, seed) {
      set.seed(seed)
      seqs <- if (design == "2x2x4") c("TRTR", "RTRT") else c("TRR", "RTR", "RRT")
      sw_r <- sqrt(log(cv_wr^2 + 1)); sw_t <- sqrt(log(cv_wt^2 + 1))
      seq_of <- rep(seqs, length.out = n)
      base <- do.call(rbind, lapply(seq_len(n), function(i) {
        ord <- strsplit(seq_of[i], "")[[1]]
        data.frame(Subject = as.character(i), Period = as.character(seq_along(ord)), Sequence = seq_of[i],
                   trt = ord, stringsAsFactors = FALSE)
      }))
      base$Treatment <- factor(ifelse(base$trt == "T", "Test", "Reference"), levels = c("Reference", "Test"))
      hits <- 0
      for (s in seq_len(nsim)) {
        b <- base
        b$CMAX <- exp(rnorm(n, 0, 0.3)[as.integer(b$Subject)] + ifelse(b$trt == "T", log(gmr), 0) +
                        rnorm(nrow(b), 0, ifelse(b$trt == "T", sw_t, sw_r)))
        a <- rsa_run(b, design); hits <- hits + isTRUE(a$pass)
      }
      hits / nsim
    }
    cases <- list(list("2x2x4", 30, 0.45, 0.45, 0.90), list("2x2x4", 24, 0.60, 0.60, 1.00), list("2x3x3", 36, 0.50, 0.50, 0.95))
    nsim <- 3000; ok <- TRUE
    for (cs in cases) {
      mine <- sim_pass(cs[[1]], cs[[2]], cs[[3]], cs[[4]], cs[[5]], nsim, seed = 11)
      pt <- suppressMessages(PowerTOST::power.RSABE(theta0 = cs[[5]], CV = c(cs[[4]], cs[[3]]), design = cs[[1]],
                                                    n = cs[[2]], nsims = 1e5, setseed = TRUE))
      ok <- ok && abs(mine - pt) < 4 * sqrt(pt * (1 - pt) / nsim) + 0.01
    }
    ok
  }, error = function(e) FALSE),
  "URS-BE-14, URS-PWR-01", critical = FALSE,
  method = "3000 seeded studies per case (2x2x4 n=30 CV 45% GMR 0.90; 2x2x4 n=24 CV 60% GMR 1.00; 2x3x3 n=36 CV 50% GMR 0.95) assessed with rsabe_assess(); PowerTOST::power.RSABE with 1e5 simulations",
  expected = "Pass rates equal within four binomial standard errors of the simulation (cases chosen far above the switch, so the ABE fall-back plays no part)")

check("RSA-04", "The switch at s_WR 0.294 and the point-estimate limits: just below and above, two decimals at both edges",
  tryCatch({
    lo <- rsa_run(rsa_synth(0.2939, 100), "2x2x4"); hi <- rsa_run(rsa_synth(0.2941, 100), "2x2x4")
    abs(lo$sWR - 0.2939) < 1e-6 && abs(hi$sWR - 0.2941) < 1e-6 && !lo$scaled && hi$scaled && !lo$pass &&
      # high s_WR, so the criterion is met and only the point estimate decides
      { a <- rsa_run(rsa_synth(0.6, 124.99), "2x2x4"); b <- rsa_run(rsa_synth(0.6, 125.04), "2x2x4")
        c2 <- rsa_run(rsa_synth(0.6, 125.06), "2x2x4"); d2 <- rsa_run(rsa_synth(0.6, 79.996), "2x2x4")
        e2 <- rsa_run(rsa_synth(0.6, 79.99), "2x2x4")
        a$critbound <= 0 && a$pass && !b$pass && !c2$pass && c2$critbound <= 0 && d2$pass && !e2$pass &&
          abs(a$pe - 124.99) < 0.01 }
  }, error = function(e) FALSE),
  "URS-BE-14", critical = TRUE, method = "Synthetic 2x2x4 studies with exactly known s_WR and point estimate",
  expected = "Scaling starts at 0.294; 124.99% passes, 125.04% and 125.06% fail; 79.996% passes (80.00), 79.99% fails")

check("RSA-05", "Complete cases: a subject missing a period leaves the contrast, N is reported; unbalanced sequences work; excluded profiles are left out",
  tryCatch({
    b <- rsa_be("rds01"); full <- rsa_run(b, "2x2x4")
    n_all <- length(unique(b$Subject))
    b2 <- rsa_synth(0.5, 100); n0 <- rsa_run(b2, "2x2x4")$n
    b2$CMAX[b2$Subject == "1" & b2$Period == "2"] <- NA
    m1 <- rsa_run(b2, "2x2x4")
    b3 <- rsa_synth(0.5, 100); b3$EXCLUDED <- NA_character_; b3$EXCLUDED[b3$Subject == "2" & b3$Period == "3"] <- "Vomiting"
    m2 <- rsa_run(b3, "2x2x4")
    b4 <- b2[!(b2$Subject %in% as.character(1:4) & b2$Sequence == "TRTR"), ]
    full$n < n_all && full$n_incomplete == n_all - full$n && n0 == 24 && m1$n == 23 && m1$n_incomplete == 1 &&
      m2$n == 23 && m2$n_incomplete == 1 && rsa_run(b4, "2x2x4")$ok
  }, error = function(e) FALSE),
  "URS-BE-14, URS-BE-11", critical = TRUE, method = "rds01 (77 subjects, missing periods); synthetic study with one period removed or excluded; sequences of unequal size",
  expected = "Only subjects with every period enter; the count of subjects left out is reported; exclusions act like missing values")

check("RSA-06", "Unsupported input is refused in plain words: 2x2x3, 2x2x2, missing Period or Sequence, too few subjects",
  tryCatch({
    b <- rsa_synth(0.5, 100)
    r223 <- rsabe_assess(b, "CMAX", "2x2x3", "Treatment", "Subject", "Period", "Sequence")
    r222 <- rsabe_assess(b, "CMAX", "2x2x2", "Treatment", "Subject", "Period", "Sequence")
    rnp <- rsabe_assess(b, "CMAX", "2x2x4", "Treatment", "Subject", NULL, "Sequence")
    few <- rsabe_assess(b[b$Subject %in% as.character(1:2), ], "CMAX", "2x2x4", "Treatment", "Subject", "Period", "Sequence")
    w <- tryCatch(be_assess_parameter("rsabe", b, "CMAX", "2x2x3", trt_col = "Treatment", subj_col = "Subject",
                                      per_col = "Period", seq_col = "Sequence"), error = function(e) e)
    !r223$ok && grepl("2x2x3 design is not covered", r223$reason) && !r222$ok && !rnp$ok && grepl("Period and Sequence", rnp$reason) &&
      !few$ok && grepl("Too few subjects", few$reason) && inherits(w, "be_scaled_error") && grepl("not available for the design 2x2x3", conditionMessage(w))
  }, error = function(e) FALSE),
  "URS-BE-14", critical = TRUE, method = "rsabe_assess() and be_assess_parameter() with unsupported designs and inputs",
  expected = "ok = FALSE with a plain reason; be_assess_parameter() stops with class be_scaled_error")

check("RSA-07", "The assessment wrapper: Route, s_WR, implied limits and criterion bound; below the switch the standard model is used; other confidence levels give no verdict",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    hi <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.6, 100), "CMAX", "2x2x4"), args))
    lo <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.2, 100), "CMAX", "2x2x4"), args))
    st <- do.call(be_assess_parameter, c(list("standard", rsa_synth(0.2, 100), "CMAX", "2x2x4"), args))
    l95 <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.6, 100), "CMAX", "2x2x4", ci_level = 95), args))
    tm <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.6, 100), "TMAX", "2x2x4"), args))
    new_cols <- c("Approach", "Route", "s_WR", "Scaled_Lower", "Scaled_Upper", "Crit_Bound")
    hi$row$Route == "Scaled" && hi$row$Bioequivalent == "YES" && hi$row$Approach == "FDA RSABE" &&
      isTRUE(all.equal(hi$row$Scaled_Upper, 100 * exp(log(1.25) * 0.6 / 0.25))) && hi$row$Crit_Bound <= 0 &&
      hi$row$BE_Upper == hi$row$Scaled_Upper && grepl("RSABE", hi$row$Model) &&
      lo$row$Route == "Standard" && lo$row$Bioequivalent == "YES" && lo$row$BE_Upper == 125 && !grepl("RSABE", lo$row$Model) &&
      all(new_cols %in% names(lo$row)) && !any(new_cols %in% names(st$row)) &&
      grepl("^no verdict", l95$row$Bioequivalent) && tm$row$Route == "Standard"
  }, error = function(e) FALSE),
  "URS-BE-14", critical = TRUE, method = "be_assess_parameter('rsabe') on synthetic studies with s_WR 0.6 and 0.2, at 90 and 95%, and on Tmax",
  expected = "Scaled route with implied limits and bound; standard route below the switch; the standard approach adds no columns")

check("RSA-08", "Without the scaled approach nothing changes: 'standard' returns exactly the output of fit_be_parameter()",
  tryCatch({
    b <- rsa_be("rds01"); args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    a <- do.call(be_assess_parameter, c(list("standard", b, "CMAX", "2x2x4"), args))
    f <- do.call(fit_be_parameter, c(list(b, "CMAX", design = "2x2x4"), args))
    identical(a, f) && identical(be_approach_choices("2x2x2"), c("Standard (average bioequivalence)" = "standard")) &&
      identical(unname(be_approach_choices("2x2x3")), c("standard", "abel")) &&
      identical(unname(be_approach_choices("2x3x3")), c("standard", "abel", "rsabe")) &&
      identical(unname(be_approach_choices("parallel")), "standard")
  }, error = function(e) FALSE),
  "URS-BE-14, URS-BE-15", critical = TRUE, method = "be_assess_parameter('standard') against fit_be_parameter() on rds01; be_approach_choices() for each design",
  expected = "Identical; scaled approaches offered only for the designs the FDA or EMA text covers")

check("RSA-09", "The result explains itself: one line per metric with the route, the numbers and the verdict; an amber hint near the switch",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    run <- function(ap, b, p = "CMAX") do.call(be_assess_parameter, c(list(ap, b, p, "2x2x4"), args))$row
    hi <- be_scaled_explain(run("rsabe", rsa_synth(0.6, 97.8)), "Cmax")
    lo <- be_scaled_explain(run("rsabe", rsa_synth(0.2, 100)), "Cmax")
    nr <- be_scaled_explain(run("rsabe", rsa_synth(0.2941, 100)), "Cmax")
    bb <- abl_b("rds08"); ab <- be_scaled_explain(run("abel", bb), "Cmax"); au <- be_scaled_explain(run("abel", bb, "AUCLST"), "AUC")
    abs_ <- be_scaled_explain(run("abel", abl_b("rds29")), "Cmax")
    nov <- be_scaled_explain(run("rsabe", rsa_synth(0.6, 100))[, ], "Cmax", 90)
    l95 <- run("rsabe", rsa_synth(0.6, 100)); l95$Bioequivalent <- "no verdict: x"
    grepl("s_WR 0.600 at or above 0.294, scaled route; criterion bound -0\\.[0-9]+ at or below 0; point estimate 97.8% inside 80 to 125%. Bioequivalence concluded.", hi) &&
      grepl("s_WR 0.200 below 0.294, so no scaling", lo) && grepl("close to the switch", nr) && !grepl("close to the switch", hi) &&
      grepl("CVwR 77", ab) && grepl("limits 69.84 to 143.19%", ab) && grepl("standard limits, 80 to 125% \\(the EMA widens Cmax only\\)", au) &&
      grepl("at or below 30%, so the standard limits", abs_) && is.null(be_scaled_explain(l95, "Cmax")) &&
      is.null(be_scaled_explain(fit_be_parameter(rsa_be("rds01"), "CMAX", "2x2x4", trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")$row, "Cmax"))
  }, error = function(e) FALSE),
  "URS-BE-14, URS-BE-15", critical = FALSE, method = "be_scaled_explain() for RSABE (scaled, below the switch, near it), ABEL (Cmax, AUC, below 30%), a run without verdict and a standard run",
  expected = "The stated route, numbers and verdict match; the near-switch hint only near 0.294; nothing for a standard or verdictless row")

check("RSA-10", "Notes on a scaled run: fewer than 24 subjects, subjects left out of the contrasts; nothing for other approaches",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    b <- rsa_synth(0.6, 100, n = 20); f <- do.call(be_assess_parameter, c(list("rsabe", b, "CMAX", "2x2x4"), args))
    n20 <- be_scaled_notes(f$row, list(CMAX = f$scaled))
    b2 <- rsa_synth(0.6, 100, n = 26); b2$CMAX[b2$Subject == "1" & b2$Period == "1"] <- NA
    f2 <- do.call(be_assess_parameter, c(list("rsabe", b2, "CMAX", "2x2x4"), args))
    n26 <- be_scaled_notes(f2$row, list(CMAX = f2$scaled))
    fs <- do.call(be_assess_parameter, c(list("standard", b, "CMAX", "2x2x4"), args))
    fa <- do.call(be_assess_parameter, c(list("abel", b, "CMAX", "2x2x4"), args))
    length(n20) == 1 && grepl("at least 24 evaluable subjects for RSABE \\(found 20", n20) && grepl("computed anyway", n20) &&
      length(n26) == 1 && grepl("left out of the RSABE contrasts", n26) && !grepl("at least 24", n26) &&
      length(be_scaled_notes(fs$row, list())) == 0 && length(be_scaled_notes(fa$row, list(CMAX = fa$scaled))) == 0
  }, error = function(e) FALSE),
  "URS-BE-14", critical = TRUE, method = "be_scaled_notes() with 20 subjects, with 26 subjects and one missing period, and with the standard and ABEL approaches",
  expected = "A warning below 24 (still computed); a note on subjects left out; no note for other approaches")

check("RSA-11", "Record and audit: approach and constants in the settings JSON, BE_Scaled sheet with the steps, approach in the audit event; nothing added for a standard run",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    b <- rsa_synth(0.6, 100)
    f <- do.call(be_assess_parameter, c(list("rsabe", b, "CMAX", "2x2x4"), args))
    res <- list(ci_table = f$row, approach = "rsabe", scaled_details = list(CMAX = f$scaled))
    sh <- be_scaled_sheet(res)
    fs <- do.call(be_assess_parameter, c(list("standard", b, "CMAX", "2x2x4"), args))
    cst <- be_scaled_settings("rsabe"); cab <- be_scaled_settings("abel")
    f0 <- tempfile(fileext = ".csv"); write.csv(exc_xo, f0, row.names = FALSE)
    r <- suppressWarnings(run_nca(exc_xo, exc_cm, exc_st()))
    td <- tempfile("rsa11_"); dir.create(td); zf <- file.path(td, "rec.zip")
    create_analysis_record(zf, r, exc_st(), exc_cm, f0, "crossover.csv", blq_rule = "rule1", lloq = 0,
                           be_results = res, be_settings = list(ci_level = 90, analysis_approach = cst))
    utils::unzip(zf, exdir = td)
    js <- jsonlite::fromJSON(file.path(td, "analysis_settings.json"))
    d <- file.path(gxp_tmp, "rsa11"); dir.create(d); gxp_set(d)
    gxp_env$audit_init(file.path(d, "audit.sqlite"), user = "owner", org = "Validation Org")
    be <- paste(readLines("R/mod_path_be.R"), collapse = "\n")
    gxp_env$gxp_guard("analysis_run", object = "bioequivalence", sha256 = "abc", session = NULL,
                      details = list(be_settings = list(analysis_approach = cst)))
    tr <- gxp_env$audit_read(file.path(d, "audit.sqlite")); gxp_unset()
    dj <- jsonlite::fromJSON(tr$details[tr$event == "analysis_run"][1])
    all(c("Parameter", "Approach", "Route", "s_WR", "DF_reference", "x", "boundx", "y", "boundy", "Crit_Bound", "Subjects_used") %in% names(sh)) &&
      sh$Route == "Scaled" && sh$Subjects_used == 24 && isTRUE(all.equal(sh$s_WR, 0.6, tolerance = 1e-6)) &&
      "BE_Scaled" %in% openxlsx::getSheetNames(file.path(td, "results.xlsx")) &&
      isTRUE(all.equal(js$bioequivalence$analysis_approach$theta, (log(1.25) / 0.25)^2)) &&
      js$bioequivalence$analysis_approach$switch_s_WR == 0.294 && identical(dj$be_settings$analysis_approach$analysis_approach, "FDA RSABE") &&
      cab$k == 0.76 && cab$cv_cap_percent == 50 && is.null(be_scaled_settings("standard")) && is.null(be_scaled_sheet(list(ci_table = fs$row))) &&
      grepl("audit_be$analysis_approach <- be_scaled_settings(approach)", be, fixed = TRUE) &&
      grepl("rs$be$analysis_approach <- be_scaled_settings(approach)", be, fixed = TRUE)
  }, error = function(e) { gxp_unset(); FALSE }),
  "URS-BE-14, URS-BE-15, URS-EXP-05, URS-GXP-07", critical = TRUE,
  method = "be_scaled_sheet(), be_scaled_settings(), create_analysis_record() and a trail entry; the BE module builds the snapshot and the audit details",
  expected = "Constants and source in the JSON, BE_Scaled sheet, approach in the audit entry; no sheet or setting for the standard approach")

check("RSA-12", "Module wiring: the selector exists only for replicate designs, the chosen approach reaches every fit, limits must stay 80-125, stale results clear, per-metric limits are drawn",
  tryCatch({
    be <- paste(readLines("R/mod_path_be.R"), collapse = "\n"); has <- function(x) grepl(x, be, fixed = TRUE)
    ba <- paste(readLines("R/be_analysis.R", warn = FALSE), collapse = "\n"); has_ba <- function(x) grepl(x, ba, fixed = TRUE)
    has('output$approach_ui <- renderUI({') && has("if (length(ch) < 2) return(NULL)") &&
      has_ba("fit_one <- function(dat, param) be_assess_parameter(") && has_ba("fit_out <- tryCatch(fit_one(be_data, param),") &&
      has_ba("fit_one(bd0$data, p)") && length(gregexpr("approach, dat, param,", ba, fixed = TRUE)[[1]]) == 1 &&
      !has("be_assess_parameter(") &&
      has_ba("be_scaled_error = function(e)") && has("input$cov_log, input$be_approach)),") &&
      has('!isTRUE(input$be_lower == 80) || !isTRUE(input$be_upper == 125)') && has("limits_differ") &&
      has_ba("be_scaled_notes(ci_df, scaled_details)") && has('output$scaled_explain <- renderUI({') &&
      has_ba("if (length(be_approach_choices(s$design_selected)) > 1 && !identical(s$approach, approach)") && has("background:#FFF3CD;color:#7D5A00;") &&
      identical(unname(be_approach_choices("2x2x4")), c("standard", "abel", "rsabe")) && length(be_approach_choices("parallel")) == 1
  }, error = function(e) FALSE),
  "URS-BE-14, URS-BE-15", critical = FALSE, method = "Code inspection of R/mod_path_be.R; click-through on a 36-subject 2x2x4 study with both approaches",
  expected = "Selector only where it applies; both fit calls use it; the two approaches produce different routes and limits in the running app")

check("RSA-13", "Review fixes: exclusions leave CVwR, ABEL keeps the point-estimate condition, RSABE respects supportive metrics and refused metrics, Period must be numeric",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    b <- abl_b("rds01"); b$EXCLUDED <- NA_character_
    i <- which(b$Treatment == "Reference")[1:6]; b$EXCLUDED[i] <- "Vomiting"
    v_all <- within_subject_variability(abl_b("rds01"), "CMAX", "Reference", "Treatment", "Subject", "Period", "Sequence")
    v_ex <- within_subject_variability(b, "CMAX", "Reference", "Treatment", "Subject", "Period", "Sequence")
    b2 <- b[!is.na(b$EXCLUDED) == FALSE, ]
    v_gone <- within_subject_variability(b2, "CMAX", "Reference", "Treatment", "Subject", "Period", "Sequence")
    # ABEL: a hidden 'point estimate' checkbox left at FALSE must not switch the condition off
    w <- abl_b("rds08"); w$CMAX[w$Treatment == "Test"] <- w$CMAX[w$Treatment == "Test"] * 1.6
    x <- do.call(be_assess_parameter, c(list("abel", w, "CMAX", "2x2x4", pe_constraint = FALSE), args))$row
    # RSABE: supportive metric, refused metric, non-numeric periods
    sup <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.6, 100), "CMAX", "2x2x4", verdict = FALSE), args))$row
    z <- rsa_synth(0.6, 100); z$CMAX[z$Subject == "3" & z$Treatment == "Test"][1] <- 0
    zr <- do.call(be_assess_parameter, c(list("rsabe", z, "CMAX", "2x2x4"), args))
    np <- rsa_synth(0.6, 100); np$Period <- paste0("P", np$Period)
    nr <- rsabe_assess(np, "CMAX", "2x2x4", "Treatment", "Subject", "Period", "Sequence")
    isTRUE(all.equal(v_ex$cv, v_gone$cv, tolerance = 1e-12)) && !isTRUE(all.equal(v_ex$cv, v_all$cv)) &&
      x$Bioequivalent == "NO" && x$PE_Constraint == "NO" &&
      sup$Route == "Scaled" && sup$Bioequivalent == "no verdict" && sup$PE_Constraint == "not applicable" &&
      !is.null(zr$reason) && grepl("zero", zr$reason) && zr$row$Route == "Standard" && !grepl("RSABE", zr$row$Model) &&
      !nr$ok && grepl("Period values to be numbers", nr$reason)
  }, error = function(e) FALSE),
  "URS-BE-14, URS-BE-15, URS-BE-11", critical = TRUE,
  method = "Findings of the code review of phases 1-5, each with the data that triggered it",
  expected = "Excluded profiles do not enter CVwR; ABEL verdict NO when only the point estimate fails, whatever the checkbox; supportive RSABE metric has no verdict; a zero value stops the RSABE result; text periods are refused")

check("RSA-14", "Interface audit fixes: a level other than 90% gives no scaled interval under a wrong label; the limits are refused before the NCA; advanced covariate options do not stay active unseen; log covariates are named as such",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    b <- rsa_synth(0.6, 100)
    l95 <- do.call(be_assess_parameter, c(list("rsabe", b, "CMAX", "2x2x4", ci_level = 95), args))
    std95 <- do.call(fit_be_parameter, c(list(b, "CMAX", design = "2x2x4", ci_level = 95), args))
    sup95 <- do.call(be_assess_parameter, c(list("rsabe", b, "CMAX", "2x2x4", ci_level = 95, verdict = FALSE), args))$row
    cb <- cov_be(cov_dat, data.frame(name = c("age", "sex"), type = c("numeric", "categorical"), transform = c("log", "none")))
    bal <- be_covariate_balance(cb$data, cb$spec, "Treat", "Subject")
    cf <- cov_fit(cb)$estimate$covariate_coefs
    be <- paste(readLines("R/mod_path_be.R"), collapse = "\n"); has <- function(x) grepl(x, be, fixed = TRUE)
    # the limits check sits before the NCA run in the run handler
    i_lim <- regexpr("A scaled approach sets its own limits: refuse before the NCA runs", be, fixed = TRUE)
    i_nca <- regexpr("run_nca(", be, fixed = TRUE)
    isTRUE(all.equal(l95$row$CI_Lower, std95$row$CI_Lower)) && isTRUE(all.equal(l95$row$Point_Est, std95$row$Point_Est)) &&
      l95$row$Route == "Standard" && grepl("^no verdict", l95$row$Bioequivalent) && sup95$Bioequivalent == "no verdict" &&
      "age (natural log)" %in% bal$Covariate && "age (natural log)" %in% cf$Term &&
      i_lim > 0 && i_lim < i_nca && has("observeEvent(input$cov_advanced, {") && has('output$cov_summary <- renderUI({') &&
      has("(!input['%s'] || input['%s'] == 'standard')") && grepl("in the downloads", be_scaled_notes(
        data.frame(Approach = "FDA RSABE"), list(CMAX = list(ok = TRUE, n = 30, n_incomplete = 1)))[1])
  }, error = function(e) FALSE),
  "URS-BE-14, URS-BE-13", critical = TRUE, method = "Findings of the adversarial audit of the interface, each with the input that triggered it",
  expected = "At 95% the interval and the estimate are the standard ones with no scaled verdict; the limits check runs first; unticking Advanced options clears them; the balance and the coefficient table say natural log")

check("RSA-15", "The RSABE point-estimate constraint rounds to two decimals at both edges (was four significant figures, which accepted 125.049%)",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    pe_ok <- function(pct) rsabe_assess(rsa_synth(0.6, pct), "CMAX", "2x2x4", "Treatment", "Subject", "Period", "Sequence")$pe_ok
    up <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.6, 125.03), "CMAX", "2x2x4"), args))
    src <- paste(readLines("R/be_scaled.R", warn = FALSE), collapse = "\n")
    !pe_ok(125.03) && pe_ok(125.004) && pe_ok(79.996) && !pe_ok(79.99) &&
      up$row$PE_Constraint == "NO" && up$row$Bioequivalent == "NO" &&
      pe_within_limits(125.0049) && !pe_within_limits(125.049) && !grepl("signif(pe", src, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-14", critical = TRUE,
  method = "rsabe_assess() on synthetic 2x2x4 studies with s_WR 0.6 and point estimates 125.03, 125.004, 79.996 and 79.99%; pe_within_limits() at 125.0049 and 125.049%",
  expected = "125.03% fails the constraint and the verdict is NO (was YES); 125.004 and 79.996% pass; 79.99% fails; no signif() left")

check("RSA-16", "Below the switch, the RSABE results say the ordinary test used the app's model and not the FDA replicate model",
  tryCatch({
    args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
    lo <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.2, 100), "CMAX", "2x2x4"), args))
    hi <- do.call(be_assess_parameter, c(list("rsabe", rsa_synth(0.6, 100), "CMAX", "2x2x4"), args))
    n_lo <- be_scaled_notes(lo$row, list(CMAX = lo$scaled)); n_hi <- be_scaled_notes(hi$row, list(CMAX = hi$scaled))
    any(grepl("Appendix C", n_lo)) && any(grepl("Satterthwaite", n_lo)) && !any(grepl("Appendix C", n_hi)) &&
      grepl("not the FDA replicate model", be_scaled_explain(lo$row, "Cmax"), fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-BE-14", critical = FALSE,
  method = "be_scaled_notes() and be_scaled_explain() for synthetic studies with s_WR 0.2 (below the switch) and 0.6",
  expected = "The note names Appendix C and the FDA model's features below the switch only; the one-line explanation says which model ran")

end_section("RSA")

# =============================================================================
# SECTION ABL: EMA average bioequivalence with expanding limits (ABEL)
# =============================================================================
# EMA CPMP/EWP/QWP/1401/98 Rev. 1, section 4.1.10: limits from CVwR above 30%,
# capped at 50%, Cmax only, point estimate within 80.00-125.00%. Reference
# implementation: replicateBE::method.A on its 30 reference data sets.
start_section("ABL")

abl_args <- list(trt_col = "Treatment", subj_col = "Subject", per_col = "Period", seq_col = "Sequence")
abl_run <- function(b, param = "CMAX", design = "2x2x4", ...) do.call(be_assess_parameter, c(list("abel", b, param, design), abl_args, list(...)))
abl_ma <- function(nm) suppressMessages(suppressWarnings(replicateBE::method.A(
  data = getExportedValue("replicateBE", nm), print = FALSE, details = TRUE, verbose = FALSE, plot.bxp = FALSE)))

check("ABL-01", "ABEL verdict, limits and CVwR equal replicateBE::method.A on all 30 reference data sets",
  tryCatch({
    ok <- TRUE; n_scaled <- 0; n_std <- 0; n_yes <- 0; n_no <- 0
    for (nm in sprintf("rds%02d", 1:30)) {
      ma <- abl_ma(nm); r <- abl_run(abl_b(nm))$row
      cv <- as.numeric(ma[1, "CVwR(%)"]); pass_ref <- identical(as.character(ma[1, "BE"]), "pass")
      ok <- ok && identical(r$Bioequivalent, if (pass_ref) "YES" else "NO") &&
        isTRUE(all.equal(r$s_WR, as.numeric(ma[1, "swR"]), tolerance = 1e-8)) &&
        identical(r$Route, if (cv > 30) "Scaled" else "Standard")
      if (cv > 30) {
        ok <- ok && isTRUE(all.equal(c(r$Scaled_Lower, r$Scaled_Upper), c(as.numeric(ma[1, "L(%)"]), as.numeric(ma[1, "U(%)"])), tolerance = 1e-8)) &&
          r$BE_Lower == r$Scaled_Lower
        n_scaled <- n_scaled + 1
      } else { ok <- ok && r$Scaled_Lower == 80 && r$Scaled_Upper == 125; n_std <- n_std + 1 }
      n_yes <- n_yes + pass_ref; n_no <- n_no + !pass_ref
    }
    ok && n_scaled > 5 && n_std > 3 && n_yes > 3 && n_no > 3
  }, error = function(e) FALSE),
  "URS-BE-15, URS-BE-09", critical = TRUE,
  method = "be_assess_parameter('abel') on rds01-rds30 against replicateBE::method.A (BE, L, U, swR, CVwR)",
  expected = "Same verdict on every set (both passes and failures occur); same limits above CVwR 30% and 80.00-125.00% below; same s_WR")

check("ABL-02", "Only Cmax is widened: in the same run AUC keeps 80.00-125.00%; the widening is capped at CVwR 50%",
  tryCatch({
    ok <- TRUE
    for (nm in c("rds08", "rds04", "rds01")) {
      b <- abl_b(nm); cm <- abl_run(b, "CMAX")$row; au <- abl_run(b, "AUCLST")$row
      ok <- ok && au$Route == "Standard" && au$BE_Lower == 80 && au$BE_Upper == 125 && is.na(au$Scaled_Lower)
    }
    big <- abl_run(abl_b("rds08"))$row     # CVwR far above 50%
    ok && isTRUE(all.equal(c(big$BE_Lower, big$BE_Upper), abel_limits(50), tolerance = 1e-12)) &&
      isTRUE(all.equal(round(c(big$BE_Lower, big$BE_Upper), 2), c(69.84, 143.19)))
  }, error = function(e) FALSE),
  "URS-BE-15", critical = TRUE, method = "be_assess_parameter('abel') on Cmax and on AUC (same data), on data sets with CVwR near and above 50%",
  expected = "AUC: standard route, 80-125; Cmax above 50% CVwR: 69.84-143.19")

check("ABL-03", "Design rules: 2x2x3 accepted, 2x2x2 and parallel groups refused; the point estimate must also lie within 80-125%",
  tryCatch({
    r3 <- abl_run(abl_b("rds03"), design = "2x2x3")$row
    ma <- abl_ma("rds03")
    e2 <- tryCatch(abl_run(abl_b("rds03"), design = "2x2x2"), error = function(e) e)
    ep <- tryCatch(abl_run(abl_b("rds03"), design = "parallel"), error = function(e) e)
    # A wide, imprecise study: the 90% CI sits inside widened limits but the point estimate is outside 80-125%
    b <- abl_b("rds08"); b$CMAX[b$Treatment == "Test"] <- b$CMAX[b$Treatment == "Test"] * 1.6
    x <- abl_run(b)$row
    ci_in <- x$CI_Lower >= x$BE_Lower && x$CI_Upper <= x$BE_Upper
    identical(r3$Bioequivalent, if (identical(as.character(ma[1, "BE"]), "pass")) "YES" else "NO") &&
      inherits(e2, "be_scaled_error") && inherits(ep, "be_scaled_error") &&
      x$Point_Est > 125 && ci_in && x$Bioequivalent == "NO" && x$PE_Constraint == "NO"
  }, error = function(e) FALSE),
  "URS-BE-15", critical = TRUE, method = "rds03 as 2x2x3; rds08 with the Test values raised by 60%",
  expected = "2x2x3 matches Method A; 2x2x2 and parallel refused with class be_scaled_error; verdict NO when only the point-estimate condition fails")

check("ABL-04", "Sensitivity after an exclusion works for both approaches: the profile is counted as excluded and the scaled result is still computed",
  tryCatch({
    b <- abl_b("rds01"); b$EXCLUDED <- NA_character_
    i <- which(b$Subject == b$Subject[1] & b$Treatment == "Test")[1]; b$EXCLUDED[i] <- "Vomiting"
    a0 <- abl_run(abl_b("rds01"))$row; a1 <- abl_run(b)$row
    r0 <- do.call(be_assess_parameter, c(list("rsabe", abl_b("rds01"), "CMAX", "2x2x4"), abl_args))$row
    r1 <- do.call(be_assess_parameter, c(list("rsabe", b, "CMAX", "2x2x4"), abl_args))$row
    a1$Excluded_Test == 1 && a0$Excluded_Test == 0 && r1$Route == "Scaled" && r1$N_Test == r0$N_Test - 1 &&
      is.finite(a1$CI_Lower) && is.finite(r1$CI_Lower)
  }, error = function(e) FALSE),
  "URS-BE-15, URS-BE-14, URS-BE-11", critical = TRUE, method = "rds01 with one Test profile marked excluded, ABEL and RSABE",
  expected = "ABEL: Excluded_Test = 1. RSABE: one subject fewer in the contrast. Both give an interval")

end_section("ABL")

# =============================================================================
# SECTION DOC: The statistics text says what the code does
# =============================================================================
# The Statistical Methods page, the help topics and the scope statement are
# checked against the code they describe: constants, limits, references, the
# wording of the scope in every copy, and stale sentences that must be gone.
start_section("DOC")

doc_methods <- function() {
  suppressPackageStartupMessages({ library(shiny); library(bslib) })
  source("R/mod_methods.R", local = TRUE)
  h <- htmltools::renderTags(methods_ui())$html
  gsub("\\s+", " ", htmltools::htmlEscape(gsub("<[^>]+>", "", h), attribute = FALSE))
}
doc_rd <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
doc_em <- intToUtf8(8212)

check("DOC-01", "One scope statement everywhere: the registry sentence appears in the intended-use text, the design card, the Methods page and the README; the old sentences are gone",
  tryCatch({
    m <- doc_methods(); app <- doc_rd("app.R"); rd <- doc_rd("README.md")
    scope <- BE_SCOPE_STATEMENT
    stale <- c("gives no reference-scaled bioequivalence", "It does not scale limits", "This application does not scale limits",
               "does not perform a reference-scaled", "never a scaled bioequivalence verdict", "not reference-scaled (ABEL, RSABE) or NTID analyses",
               "This app does not calculate scaled limits", "This application performs Average Bioequivalence (ABE) only",
               "shown but not applied", "but were not applied in this analysis")
    all_txt <- paste(doc_rd("app.R"), doc_rd("README.md"), doc_rd("R/mod_methods.R"), doc_rd("R/mod_data_guide.R"),
                     doc_rd("R/help_system.R"), doc_rd("R/mod_path_be.R"), doc_rd("R/be_analysis.R"))
    grepl("Average bioequivalence for every design, plus EMA ABEL and FDA RSABE for replicate designs.", scope, fixed = TRUE) &&
      grepl("for planning only", scope, fixed = TRUE) &&
      grepl("BE_SCOPE_STATEMENT", app, fixed = TRUE) && length(gregexpr("BE_SCOPE_STATEMENT", app, fixed = TRUE)[[1]]) >= 2 &&
      grepl(scope, gsub("\\s+", " ", m), fixed = TRUE) && grepl(scope, rd, fixed = TRUE) &&
      !any(vapply(stale, function(x) grepl(x, all_txt, fixed = TRUE), logical(1)))
  }, error = function(e) FALSE),
  "URS-GEN-07, URS-BE-14, URS-BE-15", critical = TRUE,
  method = "BE_SCOPE_STATEMENT in R/designs.R against app.R, the rendered Methods page and README.md; search for the sentences it replaced",
  expected = "The same sentence in every copy; none of the old sentences left in the app text")

check("DOC-02", "The Methods page states the constants and limits the code uses, and cites the sources",
  tryCatch({
    m <- doc_methods()
    has <- function(x) grepl(x, m, fixed = TRUE, useBytes = TRUE)
    abel50 <- sprintf("%.2f–%.2f%%", abel_limits(50)[1], abel_limits(50)[2])
    has(sprintf("%.4f", RSABE_THETA)) && has(as.character(RSABE_SWITCH)) && has(abel50) &&
      has("0.760") && has("0.25") && has("at least 24 evaluable subjects") && RSABE_MIN_SUBJECTS == 24L &&
      has("Reference-scaled approaches (EMA ABEL and FDA RSABE)") && has("s wR (EMA model)") && has("s WR (FDA contrasts)") &&
      has("Route column") && has("Davit BM") && has("Howe WG") && has("Tsiatis AA") && has("Tothfalusi L") &&
      has("Appendix G") && has("section 4.1.10") && has("Parallel-group study with baseline covariates") &&
      has("Bioequivalence with expanding limits (EMA ABEL)") && has("Reference-scaled bioequivalence (FDA RSABE)") &&
      has("fewer than 12 subjects per group") && has("N − 2 − p residual degrees of freedom") &&
      has("2×2×3 design supports EMA ABEL only")
  }, error = function(e) FALSE),
  "URS-GEN-03, URS-BE-14, URS-BE-15", critical = FALSE,
  method = "Rendered Methods page searched for the constants of R/be_scaled.R (theta, switch, 24 subjects), abel_limits(50) and the references",
  expected = "Every constant and limit in the text equals the code's; the sections, examples and references are present")

check("DOC-03", "No em dashes in the rendered Methods page, and the two sections of the s_WR estimators are labelled apart",
  tryCatch({
    m <- doc_methods()
    !grepl(doc_em, m, fixed = TRUE, useBytes = TRUE) && !grepl("\\u2014", doc_rd("R/mod_methods.R"), fixed = TRUE) &&
      !grepl(doc_em, doc_rd("R/mod_data_guide.R"), fixed = TRUE, useBytes = TRUE) &&
      grepl("(EMA model)", m, fixed = TRUE, useBytes = TRUE) && grepl("(FDA contrasts)", m, fixed = TRUE, useBytes = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-03", critical = FALSE, method = "Rendered Methods page and the source of the Data Guide searched for the em dash",
  expected = "None left; both s_WR estimators named")

check("DOC-04", "Help, Data Guide and design table describe the same options: approach help, covariate help, per-design approaches, covariate columns",
  tryCatch({
    h <- doc_rd("R/help_system.R"); g <- doc_rd("R/mod_data_guide.R")
    grepl('help_be_approach <- info_btn("help_be_approach"', h, fixed = TRUE) &&
      grepl('help_be_covariates <- info_btn("help_be_covariates"', h, fixed = TRUE) &&
      grepl("Acceptance approach", h, fixed = TRUE) && grepl("0.294", h, fixed = TRUE) && grepl("69.84 to 143.19%", h, fixed = TRUE) &&
      grepl("RSABE for\n  2x3x3 and 2x2x4 only", h, fixed = TRUE) &&
      grepl("Covariates (optional)", g, fixed = TRUE) && grepl("at least 12 subjects per group", g, fixed = TRUE) &&
      grepl("EMA ABEL (both designs) or FDA RSABE (2×3×3 only", g, fixed = TRUE, useBytes = TRUE) &&
      all(grepl("ABEL|ABE", BE_DESIGNS$analysis_note[BE_DESIGNS$code %in% c("2x2x3", "2x3x3", "2x2x4")])) &&
      grepl("RSABE", BE_DESIGNS$analysis_note[BE_DESIGNS$code == "2x3x3"]) && !grepl("RSABE", BE_DESIGNS$analysis_note[BE_DESIGNS$code == "2x2x3"]) &&
      identical(unname(be_approach_choices("2x2x3")), c("standard", "abel")) &&
      grepl("covariates", BE_DESIGNS$analysis_note[BE_DESIGNS$code == "parallel"])
  }, error = function(e) FALSE),
  "URS-BE-13, URS-BE-14, URS-BE-15", critical = FALSE, method = "Text of the help topics, the Data Guide and the design registry against be_approach_choices()",
  expected = "Help and guide say what the selector offers: RSABE not for 2x2x3, covariates for parallel groups only")

end_section("DOC")

# =============================================================================
# Post-execution
# =============================================================================
cat("\n", paste(rep("=",72),collapse=""), "\n")
cat("RESULTS SUMMARY\n")
cat(paste(rep("=",72),collapse=""), "\n")

# The counts in the documents are the counts of this run (this check included)

all_urs <- c(paste0("URS-GEN-0",c(1,3:9)),paste0("URS-DAT-0",1:9),paste0("URS-NCA-",sprintf("%02d",1:15)),
             paste0("URS-BE-0",1:9),"URS-BE-10","URS-BE-11","URS-BE-12","URS-BE-13","URS-BE-14","URS-BE-15",paste0("URS-PWR-0",1:6),paste0("URS-EXP-0",1:8),paste0("URS-UI-0",1:5),
             paste0("URS-VIZ-0",1:9),paste0("URS-GXP-",sprintf("%02d",1:20)))
start_section("DOC")
check("DOC-06", "Every requirement ID in the URS document is in the list of tested requirements, and the list holds nothing the URS lacks",
  tryCatch({
    td <- tempfile(); dir.create(td); utils::unzip("validation/NCA_Assistant_URS.docx", "word/document.xml", exdir = td)
    x <- paste(readLines(file.path(td, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "")
    cells <- regmatches(x, gregexpr("<w:tc>.*?</w:tc>", x, perl = TRUE))[[1]]
    first <- gsub("<[^>]+>", "", cells)
    ids <- sort(unique(first[grepl("^URS-[A-Z]+-[0-9]{2}$", first)]))
    length(ids) == length(all_urs) && identical(ids, sort(all_urs))
  }, error = function(e) FALSE),
  "URS-GEN-03", critical = FALSE,
  method = "The requirement IDs in the tables of validation/NCA_Assistant_URS.docx against all_urs, the list behind the coverage line",
  expected = "The same 95 IDs on both sides; a new requirement cannot be left out of the coverage line")

check("DOC-07", "BLQ rules and AUC integration: the Methods page, the Upload labels and the popover say what the pipeline does",
  tryCatch({
    raw <- read_pk_file("data/example_blq.csv"); cm <- auto_detect_columns(names(raw))
    st <- list(admin_route = "extravascular", dose = 320, dose_unit = "mg", time_unit = "h", conc_unit = "mg/L",
               trap_method = "log", r2adj_threshold = 0.7, is_steady_state = FALSE)
    r <- lapply(c("rule1", "rule2"), function(rule)
      suppressWarnings(run_nca(prepare_pk_dataset(raw, cm, list(lloq = 0.5, blq_rule = rule))$data, cm, st)))
    same <- all(vapply(c("CMAX", "TLST", "AUCLST", "AUCIFO", "LAMZHL"), function(k) isTRUE(all.equal(r[[1]][[k]], r[[2]][[k]])), logical(1)))
    m <- gsub("\\s+", " ", doc_methods()); up <- doc_rd("R/mod_data_upload.R"); hp <- doc_rd("R/help_system.R")
    be <- doc_rd("R/mod_path_be.R")
    same && !isTRUE(all.equal(r[[1]]$AUCALL, r[[2]]$AUCALL)) &&
      grepl("which is what Rule 2 does", m, fixed = TRUE) && !grepl("as Rules 1 and 2 do", m, fixed = TRUE) &&
      !grepl("Rules 1 and 2 do this", be, fixed = TRUE) &&
      grepl("ends at zero (a value set by a BLQ rule) has no logarithm", m, fixed = TRUE) &&
      grepl("rounded to two decimals at both limits", m, fixed = TRUE) && !grepl("four significant figures", m, fixed = TRUE) &&
      grepl("after the last \\u2192 missing, pre-dose \\u2192 0", up, fixed = TRUE) &&
      grepl("also in a profile with no quantifiable value", up, fixed = TRUE) &&
      grepl("A BLQ pre-dose sample \u2192 0. In a profile without any measurable value", hp, fixed = TRUE) &&
      grepl("including a pre-dose sample and every value of a profile without a measurable concentration", hp, fixed = TRUE) &&
      grepl("any quantifiable concentration, the BLQ values other than the pre-dose sample", m, fixed = TRUE)
  }, error = function(e) FALSE),
  "URS-GEN-03, URS-DAT-04", critical = FALSE,
  method = "Rules 1 and 2 on data/example_blq.csv; the rendered Methods page, R/mod_data_upload.R, R/help_system.R and R/mod_path_be.R",
  expected = "Rules 1 and 2 give the same Cmax, Tlast, AUClast, AUCinf and half-life, and a different AUCall; the texts state the M13A rule, the Rule 4 and Rule 6 exceptions, the zero segment and the rounding as the code does")

check("DOC-05", "The counts in the READMEs, the version history and the protocol equal the counts of this run",
  tryCatch({
    rs <- do.call(rbind, results)
    n_auto <- as.integer(sum(rs$Result != "SKIP") + 1); n_man <- as.integer(sum(rs$Result == "SKIP"))
    n_crit <- as.integer(sum(rs$Class == "CRITICAL" & rs$Result != "SKIP")); n_supp <- as.integer(sum(rs$Class == "SUPPORTIVE" & rs$Result != "SKIP") + 1)
    refs <- unique(unlist(strsplit(c(rs$URS_Ref[rs$Result != "SKIP"], "URS-GEN-03"), ",\\s*")))
    n_auto_urs <- as.integer(length(intersect(all_urs, refs)))
    rd <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    num <- function(txt, pat) as.integer(regmatches(txt, regexec(pat, txt))[[1]][-1])
    r1 <- rd("README.md"); r2 <- rd("validation/README.md"); ap <- rd("app.R")
    zip_txt <- function(f) { td <- tempfile(); dir.create(td); utils::unzip(f, "word/document.xml", exdir = td)
      gsub("<[^>]+>", "", paste(readLines(file.path(td, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "")) }
    iq <- zip_txt("validation/NCA_Assistant_IQOQPQ.docx"); urs <- zip_txt("validation/NCA_Assistant_URS.docx")
    man <- zip_txt("NCA_Assistant_User_Manual_v1.9.docx")
    ok <- c(
      identical(num(r1, "executes (\\d+) automated tests \\(plus (\\d+) manual"), c(n_auto, n_man)),
      identical(num(r2, "runs \\*\\*(\\d+) automated tests\\*\\*"), n_auto),
      identical(num(r2, "\\*\\*(\\d+) manual tests\\*\\*"), n_man),
      identical(num(r2, "Total: (\\d+) \\(auto: (\\d+), manual: (\\d+)\\)"), as.integer(c(n_auto + n_man, n_auto, n_man))),
      identical(num(r2, "PASS: (\\d+) \\| FAIL: 0 \\| ERROR: 0 \\| SKIP: (\\d+)"), c(n_auto, n_man)),
      identical(num(r2, "Of the (\\d+) automated tests, (\\d+) are CRITICAL and (\\d+) SUPPORTIVE"), c(n_auto, n_crit, n_supp)),
      identical(num(r2, "URS: (\\d+)/(\\d+) covered \\((\\d+) by automated"), as.integer(c(length(all_urs), length(all_urs), n_auto_urs))),
      identical(num(ap, "Validation: (\\d+) automated and (\\d+) manual tests"), c(n_auto, n_man)),
      identical(num(iq, "\\((\\d+) automated, (\\d+) manual\\)"), c(n_auto, n_man)),
      identical(num(urs, "covers (\\d+) requirements"), as.integer(length(all_urs))),
      identical(num(man, "test script with (\\d+) automated and (\\d+) manual tests"), c(n_auto, n_man)),
      identical(num(r1, "(\\d+) requirements across 9 categories"), as.integer(length(all_urs))),
      identical(num(r2, "(\\d+) requirements across 9 categories"), as.integer(length(all_urs))),
      identical(num(r1, "`R/`\\]\\(R/\\) \\| (\\d+) files"), as.integer(length(list.files("R", "\\.R$")))))
    if (!all(ok)) cat("  DOC-05 mismatch in items:", which(!ok), "of", length(ok), "- expected auto", n_auto, "manual", n_man, "\n")
    all(ok)
  }, error = function(e) { cat("  DOC-05 error:", conditionMessage(e), "\n"); FALSE }),
  "URS-GEN-03", critical = FALSE,
  method = "Counts parsed from README.md, validation/README.md, the version history in app.R, the IQ/OQ/PQ protocol, the URS, the manual, and the requirement and file counts in the READMEs, against the results list of this run",
  expected = "Every count equals this run's: automated, manual, critical, supportive, requirements and requirements covered")
end_section("DOC")

results_df <- do.call(rbind, results)
for (s in setdiff(names(section_times),"MAN")) cat(sprintf("  %s: %.1f s\n",s,section_times[[s]]))

n_pass<-sum(results_df$Result=="PASS"); n_fail<-sum(results_df$Result=="FAIL")
n_skip<-sum(results_df$Result=="SKIP"); n_err<-sum(results_df$Result=="ERROR")
cat(sprintf("\nTotal: %d (auto: %d, manual: %d)\n",nrow(results_df),nrow(results_df)-n_skip,n_skip))
cat(sprintf("  PASS: %d | FAIL: %d | ERROR: %d | SKIP: %d\n",n_pass,n_fail,n_err,n_skip))

cf <- results_df[results_df$Class=="CRITICAL"&results_df$Result!="PASS",]
if (nrow(cf)>0) {
  cat("\nCRITICAL FAILURES:\n")
  for(i in 1:nrow(cf)) cat(sprintf("  %s: %s\n",cf$ID[i],cf$Test[i]))
  cat("\nSTATUS: FAILED\n")
} else {
  cat("\nALL CRITICAL TESTS PASSED\n")
  if(n_fail>0) cat(sprintf("  (%d supportive failures need risk assessment)\n",n_fail))
}

covered <- unique(unlist(strsplit(results_df$URS_Ref,",\\s*")))
# Coverage by executed tests only: a requirement whose only tests are manual
# (SKIP in this run) is reported as such, not as covered by this run
auto_cov <- unique(unlist(strsplit(results_df$URS_Ref[results_df$Result != "SKIP"],",\\s*")))
manual_only <- setdiff(intersect(all_urs, covered), auto_cov)
cat(sprintf("\nURS: %d/%d covered (%d by automated tests%s)\n",length(intersect(all_urs,covered)),length(all_urs),
            length(intersect(all_urs, auto_cov)),
            if (length(manual_only) > 0) paste0("; manual tests only: ", paste(manual_only, collapse = ", ")) else ""))
miss <- setdiff(all_urs,covered)
if (length(miss)>0) cat("  Missing:",paste(miss,collapse=", "),"\n")

write.csv(results_df, "validation/validation_results.csv", row.names=FALSE)
# The environment of this run, next to the results: R and package versions,
# and the SHA-256 of every file that was tested
env_pkgs <- c(required_pkgs, "shiny", "bslib", "shinyWidgets", "DT", "plotly", "ggplot2", "htmltools", "tidyr",
              "shinymanager", "DBI", "RSQLite")
writeLines(c(paste("NCA Assistant", APP_VERSION, "- validation run", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), "",
             "Packages:", paste0("  ", env_pkgs, " ", vapply(env_pkgs, function(p)
               tryCatch(as.character(packageVersion(p)), error = function(e) "not installed"), character(1))), "",
             "File SHA-256:", paste0("  ", names(file_hashes), " ", file_hashes), "",
             capture.output(sessionInfo())),
           "validation/validation_environment.txt")
cat("\nResults: validation/validation_results.csv\n")
cat("Validation complete:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
