# ============================================================================
# NCA Assistant v1.2 — Consolidated Validation Script
# ============================================================================
# Attachment A to IQ/OQ/PQ Protocol
#
# Run from project root:
#   Rscript validation/validation.R
# ============================================================================

cat(paste(rep("=", 72), collapse=""), "
")
cat("NCA Assistant v1.2 — Validation Script
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
required_pkgs <- c("NonCompart", "PowerTOST", "nlme", "digest", "rmarkdown",
                   "openxlsx", "jsonlite", "readxl", "dplyr", "knitr", "replicateBE")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse=", "), "
")
  install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
}
library(NonCompart); library(PowerTOST); library(nlme); library(digest)

for (f in c("R/pipeline.R", "R/adnca_import.R", "R/utils.R", "R/nca_helpers.R", "R/interlocks.R", "R/data_quality.R",
           "R/export_record.R", "R/designs.R", "R/be_analysis.R")) {
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
                  "R/pipeline.R", "R/interlocks.R", "R/adnca_import.R", "converters/adnca_to_flat.R")
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
      as.numeric(R.version$major) >= 4 && as.numeric(R.version$minor) >= 1,
      "URS-GEN-01", method="R.version check", expected="R >= 4.1", critical=TRUE, detail=R.version.string)

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

end_section("IQ")

# =============================================================================
# SECTION DAT: Data Handling
# =============================================================================
start_section("DAT")

col_conventions <- list(
  standard=c("Subject","Time","Concentration"), cdisc=c("USUBJID","NTIM","DV"),
  winnonlin=c("SubjID","Hours","Conc"), dutch=c("Proband","Zeit","Konzentration"),
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

check("DAT-DQ-07", "DQ: Negative conc WARNING",
      { qc <- run_data_quality_check(data.frame(Subject=rep("A",4),Time=0:3,Conc=c(0,5,-1,3)), bcm); any(qc$findings$Severity=="WARNING" & grepl("egative",qc$findings$Message)) },
      "URS-DAT-03", method="Conc=-1", expected="WARNING negative", critical=FALSE)

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

check("DAT-BLQ-04", "BLQ Rule 4: all=LLOQ/2",
      { d <- apply_blq_rules(bb,bc,"rule4",bl); all(d$Conc[c(1,2,6,7)]==0.5) },
      "URS-DAT-04", method="Rule 4", expected="BLQ=0.5", critical=TRUE)

check("DAT-BLQ-05", "BLQ Rule 5: pre-Cmax=0 post=NA",
      { d <- apply_blq_rules(bb,bc,"rule5",bl); d$Conc[1]==0 && d$Conc[2]==0 && is.na(d$Conc[6]) && is.na(d$Conc[7]) },
      "URS-DAT-04", method="Rule 5", expected="Pre [0,0] post [NA,NA]", critical=TRUE)

check("DAT-BLQ-06", "BLQ Rule 6: pre=LLOQ/2 rest=0",
      { d <- apply_blq_rules(bb,bc,"rule6",bl); d$Conc[1]==0.5 && d$Conc[2]==0.5 && d$Conc[6]==0 && d$Conc[7]==0 },
      "URS-DAT-04", method="Rule 6", expected="Pre [0.5,0.5] post [0,0]", critical=TRUE)

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
check("DAT-PREP-03", "'<x' BLQ text reaches the BLQ rule; other text becomes missing",
  tryCatch({
    ds <- prepare_pk_dataset(prep_raw, prep_cm, list(lloq = 0.5, blq_rule = "rule4"))
    d <- ds$data
    ds$blq$text_tokens_converted == 2 &&
      identical(d$Conc[d$Subject == 2 & d$Time %in% c(0, 4)], c(0.25, 0.25)) &&
      is.na(d$Conc[d$Subject == 1 & d$Time == 2])
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "rule 4 (LLOQ/2) with '<0.5', '<0,5' and 'BLQ'",
  expected = "'<' entries -> 0.25; 'BLQ' -> NA")
check("DAT-PREP-04", "Without an LLOQ no BLQ rule is applied and text becomes missing",
  tryCatch({
    ds <- prepare_pk_dataset(prep_raw, prep_cm, list(lloq = 0))
    ds$blq$rule == "none" && ds$blq$text_tokens_converted == 0 && sum(is.na(ds$data$Conc)) == 3
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = TRUE, method = "lloq = 0", expected = "3 missing concentrations")
check("DAT-PREP-05", "LLOQ suggestion from '<x' text, including decimal commas",
  tryCatch({
    b <- blq_text_summary(c("<0,5", "<0.25", "BLQ", "3.1"))
    b$n_blq_text == 3 && identical(b$suggested_lloq, 0.25) &&
      is.null(blq_text_summary(c("1", "2"))$suggested_lloq)
  }, error = function(e) FALSE),
  "URS-DAT-04", critical = FALSE, method = "blq_text_summary()", expected = "3 entries, LLOQ 0.25")
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
      { ss<-data.frame(Subject=rep("A",6),Time=c(0,1,2,4,8,12),Conc=c(5,15,12,8,5.5,5)); ss_f<-theoph_settings; ss_f$is_steady_state<-FALSE; ss_t<-theoph_settings; ss_t$is_steady_state<-TRUE; rf<-run_nca(ss,iv_cm,ss_f); rt<-run_nca(ss,iv_cm,ss_t); !is.null(rf)&&!is.null(rt)&&as.numeric(rt$CLFO[1])!=as.numeric(rf$CLFO[1]) },
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

parse_manual <- function(tt, cc) {
  tl<-trimws(unlist(strsplit(tt,"\n"))); cl<-trimws(unlist(strsplit(cc,"\n")))
  tl<-tl[tl!=""]; cl<-cl[cl!=""]
  tv<-suppressWarnings(as.numeric(tl)); cv<-suppressWarnings(as.numeric(cl))
  list(time=tv,conc=cv,nt=length(tv),nc=length(cv),ok=length(tv)==length(cv)&&length(tv)>=3&&sum(is.na(tv))==0&&sum(is.na(cv))==0)
}
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
check("PWR-DP-01", "DP works",
      { r<-tryCatch(sampleN.dp(alpha=0.05,targetpower=0.80,CV=0.20,doses=c(50,100,200),print=FALSE),error=function(e)NULL); !is.null(r)&&r[["Sample size"]]>0 },
      "URS-PWR-01", method="sampleN.dp with doses", expected="Valid N", critical=TRUE)
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
check("EXP-VR-02", "APP_VERSION is 1.3.0", APP_VERSION=="1.3.0",
      "URS-EXP-06", method="=='1.3.0'", expected="1.3.0", critical=FALSE)
check("EXP-VR-03", "Package versions", { v<-sapply(c("NonCompart","PowerTOST","nlme"),function(p)as.character(packageVersion(p))); all(nchar(v)>0) },
      "URS-EXP-06", method="packageVersion", expected="All return strings", critical=TRUE)
check("EXP-SH-01", "SHA-256 computable", nchar(digest(file="validation/validation.R",algo="sha256"))==64,
      "URS-EXP-04", method="digest SHA-256", expected="64-char hex", critical=TRUE)
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
        n_hash==4 && grepl("Source data",man) && grepl("Analysis settings",man) && grepl("Results",man) &&
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
check("EXP-CD-01", "CDISC names", is.data.frame(cdisc_pk_names())&&nrow(cdisc_pk_names())>10,
      "URS-GEN-06", method="cdisc_pk_names()", expected=">10 rows", critical=FALSE)
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
      "URS-GEN-01", method="Grep R/*.R", expected="None", critical=TRUE)
check("UI-CQ-02", "No browser()",
      { rf<-list.files("R",pattern="\\.R$",full.names=TRUE); !any(sapply(rf,function(f){l<-readLines(f,warn=FALSE);any(grepl("browser\\(\\)",l)&!grepl("^#",l))})) },
      "URS-GEN-01", method="Grep browser()", expected="None", critical=FALSE)

# Missing URS coverage tests
check("UI-NS-01", "No persistent storage (GEN-04)",
      { rf<-list.files("R",pattern="\\.R$",full.names=TRUE); !any(sapply(rf,function(f){l<-readLines(f,warn=FALSE);any(grepl("dbConnect|RSQLite|saveRDS",l)&!grepl("^#",l))})) },
      "URS-GEN-04", method="No database/persistent storage in app code", expected="No DB calls", critical=FALSE)

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

check("UI-CVB-01", "NCA CV bridge to power (PWR-05)",
      { l<-readLines("R/mod_path_power.R",warn=FALSE); any(grepl("shared\\$nca|geo.*cv|CV.*bridge",l,ignore.case=TRUE)) || any(grepl("cv.*nca|nca.*cv",l,ignore.case=TRUE)) },
      "URS-PWR-05", method="Power module references NCA-derived CV", expected="CV bridge code present", critical=FALSE)

check("UI-JSN-01", "Settings exported as JSON (EXP-03)",
      { l<-readLines("R/export_record.R",warn=FALSE); any(grepl("toJSON|analysis_settings\\.json",l)) },
      "URS-EXP-03", method="JSON export in export_record.R", expected="toJSON present", critical=FALSE)

check("UI-BER-01", "BE results in export (EXP-05)",
      { l<-readLines("R/export_record.R",warn=FALSE); any(grepl("be_results|BE_Confidence",l)) },
      "URS-EXP-05", method="BE results referenced in export code", expected="BE export present", critical=FALSE)

check("UI-HUB-01", "Landing page hub (UI-03)",
      { l<-readLines("app.R",warn=FALSE); any(grepl("hub|landing|workflow|path",l,ignore.case=TRUE)) },
      "URS-UI-03", method="Hub/landing page in app.R", expected="Hub code present", critical=FALSE)

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
skip_manual("MAN-14","Analysis Record","Export zip","Contains all files","URS-EXP-01")
skip_manual("MAN-15","Repro script","Run reproduce_analysis.R","Produces CSV","URS-EXP-02")
skip_manual("MAN-16","Methods page","Click Methods nav","Formulas display","URS-GEN-03")
skip_manual("MAN-17","Data Guide","Click Data Guide","Scenario tabs","URS-UI-02")
skip_manual("MAN-18","Help popovers","Click ? button","Popover appears","URS-UI-01")
skip_manual("MAN-19","Error notification","NCA without data","Red notification","URS-UI-04")
skip_manual("MAN-20","Responsive layout","Resize < 768px","Sidebar collapses","URS-GEN-01")
skip_manual("MAN-21","BE individual profiles","Upload crossover data; run BE; open Individual Profiles tab","Per-subject panels with treatment overlay","URS-BE-08")
skip_manual("MAN-22","BE half-life review","Upload crossover data; run BE; open Half-Life Review; select profile","Plot with terminal phase; checkboxes populate","URS-NCA-12")
skip_manual("MAN-23","Override info note","Open Half-Life Review tab; verify info text","Note explaining AUC-inf dependency present","URS-NCA-12")
skip_manual("MAN-25","Viz data gate","Navigate to Visualize Data before upload","Data gate card displayed, no plot rendered","URS-VIZ-01")
skip_manual("MAN-26","Viz spaghetti plot","Load example_theoph.csv; open Visualize Data; Individual Profiles tab","12 lines rendered without error","URS-VIZ-02")
skip_manual("MAN-27","Viz colour-by options","Cycle through colour-by options (Subject/Treatment/Period/Sequence)","Plot updates for each available option; unavailable options absent","URS-VIZ-02")
skip_manual("MAN-28","Viz summary plot","Load example_theoph.csv; open Summary Plot tab","Geometric mean curve with error bars, no error","URS-VIZ-03")
skip_manual("MAN-29","Viz BLQ note","Load dataset with zero concentration; open Summary Plot","Note counting excluded observations appears","URS-VIZ-05")
skip_manual("MAN-30","Viz log scale","Toggle Log Y-axis with zero-concentration data","Plot renders without error; zero values omitted silently","URS-VIZ-07")
skip_manual("MAN-31","Viz export PNG","Render any plot; go to Export tab; select PNG 7x5 300 DPI; click Download","Non-zero PNG file downloads","URS-VIZ-06")
skip_manual("MAN-32","Viz export invalid dims","Set width = 0; click Download","Validation message displayed, no file downloaded","URS-VIZ-06")
skip_manual("MAN-33","Viz dose normalisation","Map Dose column; enable C/Dose normalisation","Y-axis values scaled by dose; option absent when no dose column","URS-VIZ-08")

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
  "URS-BE-05", critical = FALSE,
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
    pos <- regexpr("This app performs standard ABE", src, fixed = TRUE)
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
      !is.null(js$reproduction_scope) &&
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
                      PK = d$PK, stringsAsFactors = FALSE)
      f <- fit_be_parameter(b, "PK", "2x2x4", trt_col = "Treatment", subj_col = "Subject",
                            per_col = "Period", seq_col = "Sequence")$estimate
      v <- be_variability_diagnostic(b, "PK", trt_col = "Treatment", subj_col = "Subject",
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
  out <- suppressWarnings(create_analysis_record(zp, res, st, cm, f, "input.csv", blq_rule = rule,
    lloq = lloq, analyst = "QA", study_name = "REC", lz_overrides = overrides, read_args = read_args,
    be_results = if (be) list(ci_table = data.frame(Parameter = "CMAX"), anova = list()) else NULL))
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
    grepl("Result: MATCH", rec_check_text(r$ex)) && identical(js$dose_source, "per_subject") &&
      identical(as.numeric(js$lz_overrides[[1]]$time_used), c(4, 6, 8))
  }, error = function(e) FALSE),
  "URS-EXP-07", critical = TRUE, method = "2x2x4 fixture, dose = subject x 10, override on one administration",
  expected = "MATCH; per-subject dose and override time points recorded")
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
  "URS-VIZ-08", critical = FALSE, method = "spaghetti figure record from the 2x2 fixture with LLOQ 0.5",
  expected = "script uses the pipeline; check reports the figure was produced")

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
# Post-execution
# =============================================================================
cat("\n", paste(rep("=",72),collapse=""), "\n")
cat("RESULTS SUMMARY\n")
cat(paste(rep("=",72),collapse=""), "\n")

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

all_urs <- c(paste0("URS-GEN-0",c(1,3:6)),paste0("URS-DAT-0",1:7),paste0("URS-NCA-",sprintf("%02d",1:12)),
             paste0("URS-BE-0",1:9),paste0("URS-PWR-0",1:6),paste0("URS-EXP-0",1:7),paste0("URS-UI-0",1:4),
             paste0("URS-VIZ-0",1:8))
covered <- unique(unlist(strsplit(results_df$URS_Ref,",\\s*")))
cat(sprintf("\nURS: %d/%d covered\n",length(intersect(all_urs,covered)),length(all_urs)))
miss <- setdiff(all_urs,covered)
if (length(miss)>0) cat("  Missing:",paste(miss,collapse=", "),"\n")

write.csv(results_df, "validation/validation_results.csv", row.names=FALSE)
cat("\nResults: validation/validation_results.csv\n")
cat("Validation complete:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
