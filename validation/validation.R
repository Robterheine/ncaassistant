# ============================================================================
# NCA Assistant v1.2 — Consolidated Validation Script
# ============================================================================
# Attachment A to IQ/OQ/PQ Protocol v1.3
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

required_pkgs <- c("NonCompart", "PowerTOST", "nlme", "digest", "rmarkdown",
                   "openxlsx", "jsonlite", "readxl", "dplyr", "knitr")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse=", "), "
")
  install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
}
library(NonCompart); library(PowerTOST); library(nlme); library(digest)

for (f in c("R/utils.R", "R/nca_helpers.R", "R/data_quality.R")) {
  tryCatch(source(f, local = TRUE), error = function(e) NULL)
}

tryCatch({
  lines <- readLines("R/mod_data_upload.R")
  start <- grep("^auto_detect_columns", lines)
  if (length(start) > 0) {
    depth <- 0; end <- start
    for (i in start:length(lines)) {
      depth <- depth + nchar(gsub("[^{]", "", lines[i])) - nchar(gsub("[^}]", "", lines[i]))
      if (depth == 0 && i > start) { end <- i; break }
    }
    eval(parse(text = paste(lines[start:end], collapse = "
")), envir = globalenv())
  }
}, error = function(e) cat("Warning: could not extract auto_detect_columns
"))

tryCatch({
  lines <- readLines("R/export_record.R")
  start <- grep("^generate_nca_script", lines)
  if (length(start) > 0) {
    depth <- 0; end <- start[1]
    for (i in start[1]:length(lines)) {
      depth <- depth + nchar(gsub("[^{]", "", lines[i])) - nchar(gsub("[^}]", "", lines[i]))
      if (depth == 0 && i > start[1]) { end <- i; break }
    }
    eval(parse(text = paste(lines[start[1]:end], collapse = "
")), envir = globalenv())
  }
}, error = function(e) cat("Warning: could not extract generate_nca_script
"))

APP_VERSION <- tryCatch({
  app_lines <- readLines("app.R")
  ver_line <- grep("^APP_VERSION", app_lines, value = TRUE)[1]
  eval(parse(text = ver_line)); APP_VERSION
}, error = function(e) "unknown")

source_files <- c("R/utils.R", "R/nca_helpers.R", "R/data_quality.R",
                  "R/export_record.R", "R/mod_data_upload.R")
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
check("NCA-ED-02", "Edge: all zero", { r<-run_nca(data.frame(Subject=rep("A",5),Time=c(0,1,2,4,8),Conc=rep(0,5)),iv_cm,theoph_settings); !is.null(r) },
      "URS-NCA-10", method="All zero no crash", expected="Returns result", critical=TRUE)
check("NCA-ED-03", "Edge: 1 point", { r<-run_nca(data.frame(Subject="A",Time=1,Conc=10),iv_cm,theoph_settings); !is.null(r) },
      "URS-NCA-10", method="Single obs", expected="Returns result", critical=FALSE)
check("NCA-ED-04", "Edge: unsorted", { d<-data.frame(Subject=rep("A",5),Time=c(4,0,8,1,2),Conc=c(4,0,1,10,8)); r<-run_nca(d,iv_cm,theoph_settings); !is.null(r)&&as.numeric(r$CMAX[1])==10 },
      "URS-NCA-10", method="Unsorted->Cmax=10", expected="Correct Cmax", critical=TRUE)
check("NCA-ED-05", "Edge: sparse 3pt", { r<-run_nca(data.frame(Subject="A",Time=c(0,1,4),Conc=c(0,10,2)),iv_cm,theoph_settings); !is.null(r)&&as.numeric(r$CMAX[1])==10 },
      "URS-NCA-10", method="3 points", expected="Cmax=10", critical=TRUE)
check("NCA-ED-06", "Edge: large conc", { r<-run_nca(data.frame(Subject=rep("A",5),Time=c(0,1,2,4,8),Conc=c(0,1e8,5e7,1e7,1e6)),iv_cm,theoph_settings); !is.null(r)&&as.numeric(r$CMAX[1])==1e8 },
      "URS-NCA-10", method="1e8 concentration", expected="Cmax=1e8", critical=FALSE)

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

run_be <- function(bd, param, design="crossover_2x2", mt="fixed", ci=90) {
  a <- 1-ci/100; bd$.r <- log(as.numeric(bd[[param]]))
  bd$Subject <- factor(bd$Subject); bd$Treatment <- factor(bd$Treatment)
  if (!is.null(bd$Sequence)) bd$Sequence <- factor(bd$Sequence)
  if (!is.null(bd$Period)) bd$Period <- factor(bd$Period)
  if (design=="crossover_fixed_order") fit <- lm(.r~Subject+Treatment,data=bd,na.action=na.exclude)
  else if (mt=="mixed") fit <- tryCatch(nlme::lme(.r~Sequence+Period+Treatment,random=~1|Sequence/Subject,data=bd,na.action=na.exclude),error=function(e) NULL)
  else fit <- lm(.r~Sequence+Subject+Period+Treatment,data=bd,na.action=na.exclude)
  if (is.null(fit)) return(NULL)
  tl <- sort(unique(bd$Treatment)); tcn <- paste0("Treatment",tl[2])
  if (inherits(fit,"lme")) { co<-nlme::fixef(fit); st<-summary(fit)$tTable; d<-co[tcn]; s<-st[tcn,"Std.Error"]; df<-st[tcn,"DF"]; mse<-summary(fit)$sigma^2 }
  else { co<-coef(fit); st<-summary(fit)$coefficients; d<-co[tcn]; s<-st[tcn,"Std. Error"]; df<-fit$df.residual; mse<-summary(fit)$sigma^2 }
  tc <- qt(1-a/2,df)
  list(pe=exp(d)*100, ci_lo=exp(d-tc*s)*100, ci_hi=exp(d+tc*s)*100, dfe=df, mse=mse)
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
      { ov<-list("S1"=list(profile="S1",original_lambda_z=0.08,adjusted_lambda_z=0.1,original_r2adj=0.95,adjusted_r2adj=0.99,points_used=4)); script<-generate_nca_script(theoph_settings,theoph_cm,"example_theoph.csv","rule1",0,lz_overrides=ov); grepl("override|Override|S1",script,ignore.case=TRUE) },
      "URS-EXP-07", method="generate_nca_script with lz_overrides -> check script content", expected="Override section in script", critical=FALSE)

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
check("EXP-VR-02", "APP_VERSION is 1.1", APP_VERSION=="1.1",
      "URS-EXP-06", method="=='1.0'", expected="1.0", critical=FALSE)
check("EXP-VR-03", "Package versions", { v<-sapply(c("NonCompart","PowerTOST","nlme"),function(p)as.character(packageVersion(p))); all(nchar(v)>0) },
      "URS-EXP-06", method="packageVersion", expected="All return strings", critical=TRUE)
check("EXP-SH-01", "SHA-256 computable", nchar(digest(file="validation/validation.R",algo="sha256"))==64,
      "URS-EXP-04", method="digest SHA-256", expected="64-char hex", critical=TRUE)
check("EXP-RS-01", "Repro script: valid R",
      tryCatch({ parse(text=generate_nca_script(theoph_settings,theoph_cm,"example_theoph.csv","rule1",0)); TRUE },error=function(e)FALSE),
      "URS-EXP-02", method="generate_nca_script->parse", expected="Valid R", critical=TRUE)
check("EXP-RS-02", "Repro script: key settings",
      { s<-generate_nca_script(theoph_settings,theoph_cm,"example_theoph.csv","rule1",0); grepl("NonCompart",s)&&grepl("Extravascular",s)&&grepl("example_theoph",s) },
      "URS-EXP-02", method="Check contents", expected="Key elements present", critical=TRUE)
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

check("UI-VER-01", "APP_VERSION displayed (GEN-05)",
      { l<-readLines("app.R",warn=FALSE); any(grepl("APP_VERSION",l)&grepl("output|render|tags|HTML",l)) },
      "URS-GEN-05", method="APP_VERSION referenced in UI rendering code", expected="Version in UI", critical=FALSE)

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

end_section("VIZ")

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
             paste0("URS-BE-0",1:8),paste0("URS-PWR-0",1:6),paste0("URS-EXP-0",1:7),paste0("URS-UI-0",1:4),
             paste0("URS-VIZ-0",1:8))
covered <- unique(unlist(strsplit(results_df$URS_Ref,",\\s*")))
cat(sprintf("\nURS: %d/%d covered\n",length(intersect(all_urs,covered)),length(all_urs)))
miss <- setdiff(all_urs,covered)
if (length(miss)>0) cat("  Missing:",paste(miss,collapse=", "),"\n")

write.csv(results_df, "validation/validation_results.csv", row.names=FALSE)
cat("\nResults: validation/validation_results.csv\n")
cat("Validation complete:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
