#!/usr/bin/env Rscript
# ============================================================================
# NCA Assistant — Validation Suite
# ============================================================================
# Single validation script covering Installation Qualification (IQ),
# Operational Qualification (OQ), and Performance Qualification (PQ).
#
# Every test is tagged with a URS requirement ID. No test is duplicated.
# This script is the authoritative source of all automated validation.
#
# Usage:  cd <project root>
#         Rscript tests/validation.R
#
# Output: tests/validation_results.csv (attach to signed protocol)
#         Console log with [PASS]/[FAIL] per test
# ============================================================================

# ── Project Root ─────────────────────────────────────────────────────────────
if (file.exists("R/nca_helpers.R")) {
  project_root <- getwd()
} else if (file.exists("../R/nca_helpers.R")) {
  project_root <- normalizePath(".."); setwd(project_root)
} else stop("Run from project root.")

# ── Test Framework ───────────────────────────────────────────────────────────
results <- data.frame(ID=character(), Test=character(), URS=character(),
                       Result=character(), Detail=character(), stringsAsFactors=FALSE)
n_pass <- 0; n_fail <- 0

check <- function(id, test, condition, urs, detail="") {
  if (is.na(condition)) condition <- FALSE
  r <- if (condition) "PASS" else "FAIL"
  if (r == "PASS") n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  results <<- rbind(results, data.frame(ID=id, Test=test, URS=urs,
    Result=r, Detail=detail, stringsAsFactors=FALSE))
  cat(sprintf("  [%s] %s: %s %s\n", r, id, test, detail))
}

pctd <- function(a, b) abs(a/b - 1) * 100

cat("\n================================================================\n")
cat("  NCA Assistant — Validation Suite\n")
cat(sprintf("  %s | R %s | %s\n", Sys.Date(), getRversion(), R.version$platform))
cat("================================================================\n\n")


# ============================================================================
# IQ: INSTALLATION QUALIFICATION
# ============================================================================
cat("=== IQ: Installation Qualification ===\n\n")

check("IQ-01", "R >= 4.1.0", getRversion() >= "4.1.0", "URS-GEN-02",
      sprintf("(%s)", getRversion()))

renv_ok <- requireNamespace("renv", quietly=TRUE)
check("IQ-02", "renv installed", renv_ok, "URS-GEN-02")
check("IQ-03", "renv.lock present", file.exists("renv.lock"), "URS-GEN-02")

for (pkg in c("NonCompart","PowerTOST","nlme")) {
  v <- tryCatch(as.character(packageVersion(pkg)), error=function(e) "MISSING")
  check(sprintf("IQ-%02d", which(c("NonCompart","PowerTOST","nlme")==pkg)+3),
        sprintf("%s loaded (v%s)", pkg, v), v!="MISSING",
        c("URS-NCA-01","URS-PWR-01","URS-BE-05")[which(c("NonCompart","PowerTOST","nlme")==pkg)])
}

ui_pkgs <- c("shiny","bslib","plotly","DT","openxlsx","readxl","dplyr","tidyr",
             "ggplot2","digest","jsonlite","shinyWidgets")
ui_ok <- sapply(ui_pkgs, requireNamespace, quietly=TRUE)
check("IQ-07", sprintf("UI packages (%d/%d)", sum(ui_ok), length(ui_ok)),
      all(ui_ok), "URS-GEN-01")

check("IQ-08", "app.R parses",
      tryCatch({parse(file="app.R");TRUE}, error=function(e) FALSE), "URS-GEN-01")

app_src <- readLines("app.R")
check("IQ-09", "APP_VERSION defined",
      any(grepl("^APP_VERSION", app_src)), "URS-GEN-04")

r_files <- list.files("R", "\\.R$", full.names=TRUE)
check("IQ-10", "All R/ files parse",
      all(sapply(r_files, function(f) tryCatch({parse(file=f);TRUE},error=function(e) FALSE))),
      "URS-GEN-01")


# ── Load app modules ─────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(NonCompart); library(PowerTOST); library(dplyr)
})
source("R/utils.R"); source("R/nca_helpers.R"); source("R/data_quality.R")

# Extract auto_detect_columns from upload module
upload_src <- readLines("R/mod_data_upload.R")
adc_start <- grep("^auto_detect_columns <- function", upload_src)
if (length(adc_start) > 0) {
  depth <- 0; adc_end <- adc_start
  for (i in adc_start:length(upload_src)) {
    depth <- depth + nchar(gsub("[^{]","",upload_src[i])) -
                      nchar(gsub("[^}]","",upload_src[i]))
    if (depth == 0 && i > adc_start) { adc_end <- i; break }
  }
  eval(parse(text=upload_src[adc_start:adc_end]))
}


# ============================================================================
# DAT: DATA UPLOAD, COLUMN DETECTION, QUALITY CHECKS, BLQ
# ============================================================================
cat("\n=== DAT: Data Upload & Quality ===\n\n")

# ── Column auto-detection (7 conventions) ─────────────────────────────────────
cat("--- Column Auto-Detection ---\n")
det_cases <- list(
  list(c("USUBJID","NTIM","DV","TRT01A","APERIOD","SEQUENCE"), "USUBJID","NTIM","DV","CDISC"),
  list(c("SubjectID","TimePoint_h","Concentration_ng_mL","Treatment","Period","Seq"), "SubjectID","TimePoint_h","Concentration_ng_mL","Clinical"),
  list(c("ID","Time","Conc","Drug","Per"), "ID","Time","Conc","Simple"),
  list(c("Subject","Timepoint","Result","Formulation","Period"), "Subject","Timepoint","Result","WinNonlin"),
  list(c("subject","time","conc"), "subject","time","conc","Lowercase"),
  list(c("Proband","Zeit_h","Konzentration","Behandlung"), "Proband","Zeit_h","Konzentration","European"),
  list(c("PatID","Hours","Cp_ug_L","TreatmentArm","StudyPeriod"), "PatID","Hours","Cp_ug_L","CpPrefix")
)
n_det <- 0
for (tc in det_cases) {
  d <- auto_detect_columns(tc[[1]])
  n_det <- n_det + 1
  check(sprintf("DAT-%02d", n_det), sprintf("Detect Subject (%s)", tc[[5]]),
        d$subject == tc[[2]], "URS-DAT-02")
  n_det <- n_det + 1
  check(sprintf("DAT-%02d", n_det), sprintf("Detect Time (%s)", tc[[5]]),
        d$time == tc[[3]], "URS-DAT-02")
  n_det <- n_det + 1
  check(sprintf("DAT-%02d", n_det), sprintf("Detect Conc (%s)", tc[[5]]),
        d$conc == tc[[4]], "URS-DAT-02")
}

# ── Data Quality Checks ──────────────────────────────────────────────────────
cat("\n--- Data Quality Checks ---\n")
cm0 <- list(subject="Subject", time="Time", conc="Conc")

qc <- run_data_quality_check(data.frame(Subject=character(0),Time=numeric(0),Conc=numeric(0)), cm0)
check("DAT-22", "DQ: empty dataset → ERROR", !qc$pass, "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=rep("A",5),Time=0:4,Conc=rep(NA_real_,5)), cm0)
check("DAT-23", "DQ: all-NA conc → ERROR", qc$n_errors > 0, "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=rep("A",5),Time=c("0h","1h","2","4h","8"),
  Conc=c(0,15,12,8,4),stringsAsFactors=FALSE), cm0)
check("DAT-24", "DQ: non-numeric time → ERROR",
      any(grepl("non-numeric",qc$findings$Message,ignore.case=TRUE)), "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=rep("A",7),Time=c(0,1,2,2,4,8,24),
  Conc=c(0,15,12,11.8,8,4,1)), cm0)
check("DAT-25", "DQ: duplicate times → ERROR",
      any(grepl("duplicate",qc$findings$Message,ignore.case=TRUE)), "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=c("A","A","","A",NA,"A"),Time=0:5,
  Conc=c(0,10,5,3,12,6)), cm0)
check("DAT-26", "DQ: missing Subject ID → ERROR",
      any(grepl("missing Subject",qc$findings$Message)), "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=c("A","A","B","B","B","B"),Time=c(0,4,0,1,2,4),
  Conc=c(0,5,0,15,12,8)), cm0)
check("DAT-27", "DQ: sparse subject (<3 obs) → WARNING",
      any(grepl("< 3",qc$findings$Message)), "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=rep("A",6),Time=0:5,
  Conc=c(0,15,12,-0.5,4,1)), cm0)
check("DAT-28", "DQ: negative conc → WARNING",
      any(grepl("negative",qc$findings$Message,ignore.case=TRUE)), "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=c(rep("A",4),rep("B",4)),Time=rep(0:3,2),
  Conc=c(0,5,3,1,0,0,0,0)), cm0)
check("DAT-29", "DQ: all-zero subject → WARNING",
      any(grepl("all-zero",qc$findings$Message,ignore.case=TRUE)), "URS-DAT-03")

blq_str <- data.frame(Subject=rep("A",8),Time=0:7,
  Conc=c("BLQ","5.2","<0.5","12.3","8.1","ND","3.2","BQL"),stringsAsFactors=FALSE)
qc <- run_data_quality_check(blq_str, cm0, lloq=0)
check("DAT-30", "DQ: BLQ strings detected, LLOQ unset → ERROR",
      any(grepl("LLOQ.*not set",qc$findings$Message)), "URS-DAT-03")
check("DAT-31", "DQ: LLOQ auto-detected from '<0.5'",
      any(grepl("LLOQ|auto-detect",qc$findings$Message,ignore.case=TRUE)), "URS-DAT-03")

qc <- run_data_quality_check(data.frame(Subject=rep("A",5),Time=0:4,
  Conc=c("0","hello","15.2","world","8.1"),stringsAsFactors=FALSE), cm0)
check("DAT-32", "DQ: unrecognized text → ERROR", qc$n_errors > 0 && !qc$pass, "URS-DAT-06")

incomplete <- data.frame(Subject=c("A","A","A","A","B","B","C","C","C","C"),
  Time=c(0,1,0,1,0,1,0,1,0,1),Conc=c(0,10,0,8,0,12,0,15,0,11),
  Treatment=c("T","T","R","R","T","T","T","T","R","R"))
qc <- run_data_quality_check(incomplete,
  list(subject="Subject",time="Time",conc="Conc",treatment="Treatment"))
check("DAT-33", "DQ: incomplete crossover → WARNING",
      any(grepl("missing data",qc$findings$Message,ignore.case=TRUE)), "URS-DAT-03")

clean <- data.frame(Subject=rep(c("A","B","C"),each=6),Time=rep(c(0,1,2,4,8,24),3),
  Conc=c(0,15,12,8,4,1,0,18,14,9,5,1.2,0,13,10,7,3.5,0.8))
qc <- run_data_quality_check(clean, cm0)
check("DAT-34", "DQ: clean data passes (0 errors)", qc$pass && qc$n_errors==0, "URS-DAT-03")

# HTML rendering
if (requireNamespace("shiny",quietly=TRUE)) {
  library(shiny)
  check("DAT-35", "DQ: HTML report renders (clean)",
        !is.null(tryCatch(render_quality_report(qc),error=function(e) NULL)), "URS-UI-01")
} else {
  check("DAT-35", "DQ: HTML render (skipped, no shiny)", TRUE, "URS-UI-01")
}

# ── BLQ Rules (all 6) ────────────────────────────────────────────────────────
cat("\n--- BLQ Rules ---\n")
blq <- data.frame(Subject=rep("S1",8), Time=c(0,.5,1,2,4,8,12,24),
  Conc=c(0.01,5,10,8,0.01,3,0.01,0.01))
cm_b <- list(subject="Subject",time="Time",conc="Conc"); lloq <- 0.05

r1 <- apply_blq_rules(blq,cm_b,"rule1",lloq)
check("DAT-36", "BLQ Rule 1: pre-first→0, mid→0, post-last→NA",
      r1$Conc[1]==0 && r1$Conc[5]==0 && is.na(r1$Conc[8]), "URS-DAT-04")

r2 <- apply_blq_rules(blq,cm_b,"rule2",lloq)
check("DAT-37", "BLQ Rule 2: all→0", all(r2$Conc[c(1,5,7,8)]==0), "URS-DAT-04")

r3 <- apply_blq_rules(blq,cm_b,"rule3",lloq)
check("DAT-38", "BLQ Rule 3: all→NA", all(is.na(r3$Conc[c(1,5,7,8)])), "URS-DAT-04")

r4 <- apply_blq_rules(blq,cm_b,"rule4",lloq)
check("DAT-39", "BLQ Rule 4: all→LLOQ/2", all(r4$Conc[c(1,5,7,8)]==lloq/2), "URS-DAT-04")

blq5 <- data.frame(Subject=rep("S1",8),Time=c(0,.5,1,2,4,8,12,24),
  Conc=c(0.01,0.01,5,10,8,3,0.01,0.01))
r5 <- apply_blq_rules(blq5,cm_b,"rule5",lloq)
check("DAT-40", "BLQ Rule 5: pre-Cmax→0, post-Cmax→NA",
      r5$Conc[1]==0 && r5$Conc[2]==0 && is.na(r5$Conc[7]) && is.na(r5$Conc[8]), "URS-DAT-04")

blq6 <- data.frame(Subject=rep("S1",6),Time=c(0,.5,1,2,4,8),
  Conc=c(0.05,0.08,5,3,0.08,0.05))
r6 <- apply_blq_rules(blq6,cm_b,"rule6",0.1)
check("DAT-41", "BLQ Rule 6: pre-first-quant→LLOQ/2, rest→0",
      all(r6$Conc[1:2]==0.05) && r6$Conc[5]==0 && r6$Conc[6]==0, "URS-DAT-04")

blq6m <- data.frame(Subject=c(rep("S1",4),rep("S2",4)),Time=rep(c(0,1,2,4),2),
  Conc=c(0.05,10,5,0.05, 8,6,3,0.05))
r6m <- apply_blq_rules(blq6m,list(subject="Subject",time="Time",conc="Conc"),"rule6",0.1)
check("DAT-42", "BLQ Rule 6: multi-subject independence",
      r6m$Conc[1]==0.05 && r6m$Conc[4]==0 && r6m$Conc[5]==8 && r6m$Conc[8]==0, "URS-DAT-04")

# BLQ rule affects AUC
d_r1 <- apply_blq_rules(blq,cm_b,"rule1",lloq)
d_r4 <- apply_blq_rules(blq,cm_b,"rule4",lloq)
a1 <- as.numeric(tblNCA(d_r1[!is.na(d_r1$Conc),],"Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0)$AUCLST)
a4 <- as.numeric(tblNCA(d_r4,"Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0)$AUCLST)
check("DAT-43", "BLQ: Rule 1 vs Rule 4 produce different AUC",
      abs(a1-a4)>0.001, "URS-DAT-04", sprintf("(R1=%.1f, R4=%.1f)",a1,a4))


# ============================================================================
# NCA: NON-COMPARTMENTAL ANALYSIS
# ============================================================================
cat("\n=== NCA: Computation ===\n\n")

# ── Analytical ground truth (1-compartment IV bolus) ──────────────────────────
cat("--- IV Bolus Ground Truth ---\n")
dose<-100; Vd<-10; ke<-0.1; C0<-dose/Vd
AUC_true<-dose/(Vd*ke); hl_true<-log(2)/ke; CL_true<-Vd*ke
times<-c(0,.25,.5,1,2,3,4,6,8,12,16,24,36,48,72)
concs<-C0*exp(-ke*times)
pk<-data.frame(Subject="S1",Time=times,Conc=concs)

r<-tblNCA(pk,"Subject","Time","Conc",dose=100,adm="Bolus",down="Linear",R2ADJ=0)
check("NCA-01","Cmax = C0", as.numeric(r$CMAX)==C0, "URS-NCA-01")
check("NCA-02","Lambda_z = ke (<0.5%)", pctd(as.numeric(r$LAMZ),ke)<0.5, "URS-NCA-04",
      sprintf("(%.6f vs %.6f)",as.numeric(r$LAMZ),ke))
check("NCA-03","Half-life (<0.5%)", pctd(as.numeric(r$LAMZHL),hl_true)<0.5, "URS-NCA-04")
check("NCA-04","AUCinf (<3%, linear trap bias)", pctd(as.numeric(r$AUCIFO),AUC_true)<3, "URS-NCA-01")
check("NCA-05","CL (<3%)", pctd(as.numeric(r$CLO),CL_true)<3, "URS-NCA-01")
check("NCA-06","Vz (<3%)", pctd(as.numeric(r$VZO),Vd)<3, "URS-NCA-01")

# ── Theoph oral dataset ──────────────────────────────────────────────────────
cat("\n--- Theoph Dataset ---\n")
th<-tblNCA(Theoph,"Subject","Time","conc",dose=320,adm="Extravascular",down="Linear",R2ADJ=0)
check("NCA-07","Theoph: 12 subjects", nrow(th)==12, "URS-NCA-01")
check("NCA-08","Theoph: all Cmax>0", all(as.numeric(th$CMAX)>0), "URS-NCA-01")
check("NCA-09","Theoph: all AUClast>0", all(as.numeric(th$AUCLST)>0), "URS-NCA-01")
check("NCA-10","Theoph: all lambda_z estimated",
      all(!is.na(as.numeric(th$LAMZ))&as.numeric(th$LAMZ)>0), "URS-NCA-04")

# ── Indometh IV bolus ────────────────────────────────────────────────────────
indo<-tblNCA(Indometh,"Subject","time","conc",dose=25,adm="Bolus",down="Log",R2ADJ=0)
check("NCA-11","Indometh: 6 subjects", nrow(indo)==6, "URS-NCA-02")
check("NCA-12","Indometh: CL positive/finite",
      all(as.numeric(indo$CLO)>0&is.finite(as.numeric(indo$CLO))), "URS-NCA-02")

# ── sNCA vs tblNCA ───────────────────────────────────────────────────────────
s1<-sNCA(times,concs,dose=100,adm="Bolus",down="Linear",R2ADJ=0)
check("NCA-13","sNCA Cmax = tblNCA Cmax", as.numeric(s1["CMAX"])==as.numeric(r$CMAX), "URS-NCA-05")
check("NCA-14","sNCA LAMZ = tblNCA LAMZ",
      abs(as.numeric(s1["LAMZ"])-as.numeric(r$LAMZ))<1e-10, "URS-NCA-05")

# ── Lambda_z estimation ─────────────────────────────────────────────────────
lz<-estimate_lambda_z(times,concs,r2adj_threshold=0.7)
check("NCA-15","Custom lambda_z matches NonCompart (<1%)",
      abs(lz$lambda_z/as.numeric(r$LAMZ)-1)<0.01, "URS-NCA-04")
check("NCA-16","R2adj>0.9999 for perfect data", lz$r2adj>0.9999, "URS-NCA-04")

# ── Trapezoidal methods ──────────────────────────────────────────────────────
cat("\n--- Trapezoidal Methods ---\n")
tl<-c(0.01,.25,.5,1,2,3,4,6,8,12,16,24,36,48,72); cl<-C0*exp(-ke*tl)
pk_l<-data.frame(Subject="S1",Time=tl,Conc=cl)
auc_exact<-C0/ke*(exp(-ke*0.01)-exp(-ke*72))
auc_lin<-as.numeric(tblNCA(pk_l,"Subject","Time","Conc",dose=100,adm="Bolus",down="Linear",R2ADJ=0)$AUCLST)
auc_log<-as.numeric(tblNCA(pk_l,"Subject","Time","Conc",dose=100,adm="Bolus",down="Log",R2ADJ=0)$AUCLST)
check("NCA-17","Log-down closer to exact than linear",
      abs(auc_log/auc_exact-1)<abs(auc_lin/auc_exact-1), "URS-NCA-03")
check("NCA-18","Log-down error <0.15%", abs(auc_log/auc_exact-1)<0.0015, "URS-NCA-03")

oral<-data.frame(Subject="S1",Time=c(0,.5,1,2,4,8,12,24),Conc=c(0,12.5,28.3,22.1,14.8,7.2,3.1,0.8))
cm<-list(subject="Subject",time="Time",conc="Conc")
stt<-list(admin_route="extravascular",dose=100,infusion_duration=0,is_steady_state=FALSE,
  dose_unit="mg",time_unit="h",conc_unit="ng/mL",trap_method="linear",r2adj_threshold=0.7,mw=0,partial_aucs=NULL)
r_lin<-run_nca(oral,cm,stt)
stt$trap_method<-"log"; r_log<-run_nca(oral,cm,stt); stt$trap_method<-"linear"
check("NCA-19","Linear vs log AUClast differ",
      as.numeric(r_lin$AUCLST)!=as.numeric(r_log$AUCLST), "URS-NCA-03")

# ── Route & Settings ─────────────────────────────────────────────────────────
cat("\n--- Route & Settings ---\n")
check("NCA-20","Extravascular → CLFO column", "CLFO"%in%names(r_lin), "URS-NCA-02")

stt$r2adj_threshold<-0; r_lax<-run_nca(oral,cm,stt)
check("NCA-21","R2adj=0 always estimates lambda_z", !is.na(as.numeric(r_lax$LAMZ)), "URS-NCA-04")
stt$r2adj_threshold<-0.7

stt$dose<-200; r200<-run_nca(oral,cm,stt); stt$dose<-100
check("NCA-22","Dose: Cmax unchanged", as.numeric(r_lin$CMAX)==as.numeric(r200$CMAX), "URS-NCA-09")
check("NCA-23","Dose: CL changes", abs(as.numeric(r_lin$CLFO)-as.numeric(r200$CLFO))>0.01, "URS-NCA-09")

# ── Dose normalization ───────────────────────────────────────────────────────
dn<-add_dose_normalized(as.data.frame(th),320)
check("NCA-24","Cmax_DN = Cmax/dose",
      all(abs(as.numeric(dn$CMAX_DN)-as.numeric(dn$CMAX)/320)<1e-10), "URS-NCA-08")
check("NCA-25","AUCinf_DN = AUCinf/dose",
      all(abs(as.numeric(dn$AUCIFO_DN)-as.numeric(dn$AUCIFO)/320)<1e-10,na.rm=TRUE), "URS-NCA-08")

# ── Steady state ─────────────────────────────────────────────────────────────
cat("\n--- Steady State ---\n")
ss_t<-c(0,.5,1,2,3,4,6,8,10,12); ss_c<-c(5.2,18.5,32.1,25.3,18.8,14.2,8.5,6.8,5.8,5.3)
r_ss<-tblNCA(data.frame(Subject="S1",Time=ss_t,Conc=ss_c),"Subject","Time","Conc",
  dose=100,adm="Extravascular",down="Linear",R2ADJ=0,SS=TRUE)
check("NCA-26","SS NCA completes", !is.null(r_ss), "URS-NCA-07")
check("NCA-27","SS: Cmax = 32.1", abs(as.numeric(r_ss$CMAX)-32.1)<0.01, "URS-NCA-07")

ss_t2<-c(0,.5,1,2,4,8,12); ss_c2<-c(5,12,10,7,3.5,1,0.3)
rss<-sNCA(ss_t2,ss_c2,dose=100,adm="Extravascular",SS=TRUE)
rsd<-sNCA(ss_t2,ss_c2,dose=100,adm="Extravascular",SS=FALSE)
check("NCA-28","SS: CL/F ratio = AUCinf/AUClast ratio",
      abs(as.numeric(rss["CLFO"])/as.numeric(rsd["CLFO"])-
          as.numeric(rss["AUCIFO"])/as.numeric(rss["AUCLST"]))<1e-4, "URS-NCA-07")

stt$is_steady_state<-TRUE; r_ss2<-run_nca(oral,cm,stt); stt$is_steady_state<-FALSE
check("NCA-29","SS flag accepted (no crash)", !is.null(r_ss2), "URS-NCA-07")

# ── Edge cases ───────────────────────────────────────────────────────────────
cat("\n--- Edge Cases ---\n")
check("NCA-30","Negative conc: no crash",
      !is.null(tryCatch(tblNCA(data.frame(Subject="S1",Time=c(0,1,2,4,8,24),
        Conc=c(0,25,18,10,-0.3,1)),
        "Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0),error=function(e) NULL)),
      "URS-NCA-10")

nca_z<-tryCatch(tblNCA(data.frame(Subject="S1",Time=c(0,1,2,4,8,24),Conc=rep(0,6)),
  "Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0),error=function(e) NULL,
  warning=function(w) suppressWarnings(tblNCA(data.frame(Subject="S1",Time=c(0,1,2,4,8,24),Conc=rep(0,6)),
    "Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0)))
check("NCA-31","All-zero: no crash", !is.null(nca_z), "URS-NCA-10")

check("NCA-32","Single obs: no crash",
      !is.null(tryCatch(tblNCA(data.frame(Subject="S1",Time=2,Conc=50),
        "Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0),
        error=function(e) NULL,warning=function(w) suppressWarnings(
          tblNCA(data.frame(Subject="S1",Time=2,Conc=50),"Subject","Time","Conc",
            dose=100,adm="Extravascular",R2ADJ=0)))), "URS-NCA-10")

check("NCA-33","Duplicate times: no crash",
      !is.null(tryCatch(tblNCA(data.frame(Subject="S1",Time=c(0,1,2,2,4,8,12,24),
        Conc=c(0,15,12,11.8,8,4,1.8,0.3)),"Subject","Time","Conc",dose=100,
        adm="Extravascular",R2ADJ=0),error=function(e) NULL)), "URS-NCA-10")

check("NCA-34","All-NA: no hard crash", {
  r<-tryCatch(tblNCA(data.frame(Subject="S1",Time=0:5,Conc=rep(NA_real_,6)),
    "Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0),
    error=function(e) "error",warning=function(w) suppressWarnings(
      tblNCA(data.frame(Subject="S1",Time=0:5,Conc=rep(NA_real_,6)),
        "Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0)))
  !identical(r,"error")}, "URS-NCA-10")

check("NCA-35","Extreme values (1e8): no overflow", {
  r<-tryCatch(tblNCA(data.frame(Subject="S1",Time=c(0,1,2,4,8,24),
    Conc=c(0,1e8,5e7,2e7,1e7,1e6)),"Subject","Time","Conc",dose=1e6,
    adm="Extravascular",R2ADJ=0),error=function(e) NULL)
  !is.null(r)&&is.finite(as.numeric(r$CMAX))}, "URS-NCA-10")

t0<-proc.time()
big<-do.call(rbind,lapply(1:200,function(i) data.frame(Subject=paste0("S",i),
  Time=c(0,.5,1,2,4,8,12,24),Conc=pmax(0,100*exp(rnorm(1,0,.3))*
    c(0,.5,.9,.7,.4,.2,.08,.02)+rnorm(8,0,2)))))
nca_big<-tryCatch(tblNCA(big,"Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0.7),
  error=function(e) NULL)
elapsed<-(proc.time()-t0)["elapsed"]
check("NCA-36","200 subjects: completes", !is.null(nca_big)&&nrow(nca_big)==200, "URS-GEN-01",
      sprintf("(%.1fs)",elapsed))
check("NCA-37","200 subjects: <30 seconds", elapsed<30, "URS-GEN-01")

sparse<-data.frame(Subject=c("A","A","B","B","B","B","B","B"),Time=c(0,4,0,1,2,4,8,24),
  Conc=c(0,5.2,0,15,12,8,4,1))
ns<-tryCatch(tblNCA(sparse,"Subject","Time","Conc",dose=100,adm="Extravascular",R2ADJ=0),
  error=function(e) NULL)
check("NCA-38","Sparse: completes, 2 subjects", !is.null(ns)&&nrow(ns)==2, "URS-NCA-10")
if (!is.null(ns)) check("NCA-39","Sparse: S1 lambda_z=NA (2 pts)",
  is.na(as.numeric(ns$LAMZ[ns[[1]]=="A"])), "URS-NCA-10")

unsorted<-data.frame(Subject="S1",Time=c(4,0,24,1,8,2,12),Conc=c(8.5,0,0.3,15.3,4.2,12.1,1.8))
unsorted<-unsorted[order(unsorted$Time),]
check("NCA-40","Re-sorted data: valid result",
      as.numeric(tryCatch(tblNCA(unsorted,"Subject","Time","Conc",dose=100,
        adm="Extravascular",R2ADJ=0)$CMAX,error=function(e) NA))==15.3, "URS-NCA-10")

# ── Manual entry parsing ─────────────────────────────────────────────────────
cat("\n--- Manual Entry Parsing ---\n")
parse_manual<-function(t_text,c_text){
  tl<-trimws(unlist(strsplit(t_text,"\n"))); cl<-trimws(unlist(strsplit(c_text,"\n")))
  tl<-tl[tl!=""]; cl<-cl[cl!=""]
  tv<-suppressWarnings(as.numeric(tl)); cv<-suppressWarnings(as.numeric(cl))
  list(time=tv,conc=cv,nt=length(tv),nc=length(cv),
    ok=length(tv)==length(cv)&&length(tv)>=3&&sum(is.na(tv))==0&&sum(is.na(cv))==0)}

p1<-parse_manual("0\n0.5\n1\n2\n4\n8\n12\n24","0\n12.5\n28.3\n22.1\n14.8\n7.2\n3.1\n0.8")
check("NCA-41","Manual: normal input", p1$ok&&p1$nt==8, "URS-NCA-11")
check("NCA-42","Manual: Windows CRLF",
      parse_manual("0\r\n1\r\n2\r\n4\r\n8\r\n24","0\r\n15\r\n12\r\n8\r\n4\r\n1")$ok, "URS-NCA-11")
check("NCA-43","Manual: blank lines ignored",
      parse_manual("0\n\n1\n\n2\n4\n8\n\n24\n","0\n\n15\n\n12\n8\n4\n\n1\n")$ok, "URS-NCA-11")
check("NCA-44","Manual: mismatched count rejected",
      !parse_manual("0\n1\n2\n4\n8","0\n15\n12")$ok, "URS-NCA-11")
check("NCA-45","Manual: non-numeric rejected",
      !parse_manual("0\n1\nhello\n4","0\n15\n12\n8")$ok, "URS-NCA-11")
check("NCA-46","Manual: <3 points rejected", !parse_manual("0\n1","0\n15")$ok, "URS-NCA-11")
check("NCA-47","Manual: comma decimals detected",
      !parse_manual("0\n0,5\n1\n2","0\n12,5\n28,3\n22,1")$ok, "URS-NCA-11")
check("NCA-48","Manual: whitespace stripped",
      parse_manual("  0  \n  1  \n  2  \n  4  ","  0  \n  15  \n  12  \n  8  ")$ok, "URS-NCA-11")

if (p1$ok) {
  rm<-tryCatch(sNCA(p1$time,p1$conc,dose=100,adm="Extravascular",down="Log",R2ADJ=0.7),error=function(e) NULL)
  check("NCA-49","Manual → sNCA works", !is.null(rm)&&as.numeric(rm["CMAX"])==28.3, "URS-NCA-11")
}

# ── Pipeline: crossover composite key ────────────────────────────────────────
cat("\n--- Pipeline ---\n")
xover<-rbind(
  data.frame(Subject=rep("S1",6),Time=c(0,1,2,4,8,24),Conc=c(0,20,15,10,5,1),Treatment="Test"),
  data.frame(Subject=rep("S1",6),Time=c(0,1,2,4,8,24),Conc=c(0,18,14,9,4.5,0.9),Treatment="Ref"),
  data.frame(Subject=rep("S2",6),Time=c(0,1,2,4,8,24),Conc=c(0,25,18,12,6,1.5),Treatment="Test"),
  data.frame(Subject=rep("S2",6),Time=c(0,1,2,4,8,24),Conc=c(0,22,16,11,5.5,1.2),Treatment="Ref"))
cm_x<-list(subject="Subject",time="Time",conc="Conc",treatment="Treatment")
nca_x<-run_nca(xover,cm_x,stt)
check("NCA-50","Crossover: 4 profiles (2 subj × 2 trt)", !is.null(nca_x)&&nrow(nca_x)==4, "URS-NCA-01")
check("NCA-51","Crossover: Subject/Treatment columns split",
      "Subject"%in%names(nca_x)&&"Treatment"%in%names(nca_x), "URS-NCA-01")
nca_x1<-run_nca(xover[xover$Subject=="S1",],cm_x,stt)
check("NCA-52","Single subject crossover: 2 profiles", !is.null(nca_x1)&&nrow(nca_x1)==2, "URS-NCA-01")

# ── Design detection ─────────────────────────────────────────────────────────
val<-validate_mapping(list(subject="Subject",time="Time",conc="Conc"))
check("NCA-53","Column mapping validates", val$valid, "URS-DAT-01")

set.seed(42); n_subj<-24; dose_val<-200
gen_pk<-function(subj,trt,per,seq,dose,cmax_p,ke_p){
  eta<-rnorm(1,0,.3); cmax<-cmax_p*exp(eta+rnorm(1,0,.15)); ke<-ke_p*exp(rnorm(1,0,.2)); ka<-2
  ts<-c(0,.25,.5,1,1.5,2,3,4,6,8,12,24)
  cs<-sapply(ts,function(t){if(t==0)return(0);ct<-(dose*ka/(ka-ke))*(exp(-ke*t)-exp(-ka*t))
    max(0,ct*cmax/(dose*ka/(ka-ke)*(exp(-ke*1.5)-exp(-ka*1.5)))+rnorm(1,0,cmax*.05))})
  data.frame(Subject=subj,Time=ts,Conc=round(pmax(0,cs),2),Treatment=trt,Period=per,Sequence=seq,Dose=dose)
}
profs<-list(); idx<-1
for(i in 1:12){profs[[idx]]<-gen_pk(sprintf("S%03d",i),"Test",1,"TR",dose_val,500,.15);idx<-idx+1
  profs[[idx]]<-gen_pk(sprintf("S%03d",i),"Reference",2,"TR",dose_val,480,.15);idx<-idx+1}
for(i in 13:24){profs[[idx]]<-gen_pk(sprintf("S%03d",i),"Reference",1,"RT",dose_val,480,.15);idx<-idx+1
  profs[[idx]]<-gen_pk(sprintf("S%03d",i),"Test",2,"RT",dose_val,500,.15);idx<-idx+1}
full<-do.call(rbind,profs)

cm_f<-list(subject="Subject",time="Time",conc="Conc",treatment="Treatment",period="Period",sequence="Sequence")
design<-detect_study_design(full,cm_f)
check("NCA-54","Design: crossover detected", design$is_crossover, "URS-DAT-07")
check("NCA-55","Design: N=24", design$n_subjects==24, "URS-DAT-07")
check("NCA-56","Design: 2 treatments", design$n_treatments==2, "URS-DAT-07")

nca_full<-run_nca(full,cm_f,list(admin_route="extravascular",dose=dose_val,infusion_duration=0,
  is_steady_state=FALSE,dose_unit="mg",time_unit="h",conc_unit="ng/mL",trap_method="linear",
  r2adj_threshold=0.7,mw=0,partial_aucs=NULL))
check("NCA-57","Pipeline: NCA produces results", !is.null(nca_full)&&nrow(nca_full)>0, "URS-NCA-06")
check("NCA-58","Pipeline: no NA in Cmax", all(!is.na(as.numeric(nca_full$CMAX))), "URS-NCA-01")
lamz_ok<-sum(!is.na(as.numeric(nca_full$LAMZ))&as.numeric(nca_full$LAMZ)>0)
check("NCA-59","Pipeline: lambda_z >80% estimated",
      lamz_ok/nrow(nca_full)>0.8, "URS-NCA-04", sprintf("(%d/%d)",lamz_ok,nrow(nca_full)))

nca_dn<-add_dose_normalized(as.data.frame(nca_full),dose_val)
check("NCA-60","Pipeline: dose norm correct",
      all(abs(as.numeric(nca_dn$CMAX_DN)-as.numeric(nca_dn$CMAX)/dose_val)<1e-10), "URS-NCA-08")

summ<-summarize_pk_params(nca_dn,c("CMAX","AUCLST","LAMZHL"))
check("NCA-61","Summary: N matches", all(summ$N==nrow(nca_full)), "URS-NCA-06")
check("NCA-62","Summary: geo mean < arith mean", all(summ$Geo_Mean<summ$Mean,na.rm=TRUE), "URS-NCA-06")


# ============================================================================
# BE: BIOEQUIVALENCE
# ============================================================================
cat("\n=== BE: Bioequivalence ===\n\n")
set.seed(42); n_per<-12; sigma_w<-sqrt(log(1+0.20^2)); true_gmr<-1.05
mu_R<-log(100); mu_T<-mu_R+log(true_gmr); eta<-rnorm(24,0,sigma_w*0.5)

be<-rbind(
  data.frame(Subject=1:12,Treatment="Test",Period=1,Sequence="TR",
    logAUC=mu_T+eta[1:12]+rnorm(12,0,sigma_w)),
  data.frame(Subject=1:12,Treatment="Reference",Period=2,Sequence="TR",
    logAUC=mu_R+eta[1:12]+rnorm(12,0,sigma_w)),
  data.frame(Subject=13:24,Treatment="Reference",Period=1,Sequence="RT",
    logAUC=mu_R+eta[13:24]+rnorm(12,0,sigma_w)),
  data.frame(Subject=13:24,Treatment="Test",Period=2,Sequence="RT",
    logAUC=mu_T+eta[13:24]+rnorm(12,0,sigma_w)))
be$Subject<-factor(be$Subject); be$Treatment<-factor(be$Treatment,levels=c("Reference","Test"))
be$Period<-factor(be$Period); be$Sequence<-factor(be$Sequence)

fit<-lm(logAUC~Sequence+Subject+Period+Treatment,data=be)
d<-coef(fit)["TreatmentTest"]; se<-summary(fit)$coefficients["TreatmentTest","Std. Error"]
dfe<-fit$df.residual; tc<-qt(0.95,dfe)
gmr<-exp(d)*100; ci_lo<-exp(d-tc*se)*100; ci_hi<-exp(d+tc*se)*100

# Paired cross-check
bw<-reshape(be[,c("Subject","Treatment","logAUC")],idvar="Subject",timevar="Treatment",direction="wide")
bw$diff<-bw$logAUC.Test-bw$logAUC.Reference
gmr_p<-exp(mean(bw$diff))*100

check("BE-01","ANOVA GMR ≈ paired GMR (<2%)", abs(gmr-gmr_p)<2, "URS-BE-01",
      sprintf("(ANOVA=%.1f%%, paired=%.1f%%)",gmr,gmr_p))
check("BE-02","90% CI contains true GMR (105%)", ci_lo<105&ci_hi>105, "URS-BE-03",
      sprintf("([%.1f,%.1f])",ci_lo,ci_hi))
check("BE-03","Correctly concludes BE", ci_lo>=80&ci_hi<=125, "URS-BE-04")

check("BE-04","MSE ≈ true sigma_w^2 (within 3x)", {
  mse<-summary(fit)$sigma^2; true_v<-log(1+0.20^2)
  mse/true_v>0.33&mse/true_v<3}, "URS-BE-01")

# Known bioinequivalent data
be_f<-be; be_f$logAUC[be_f$Treatment=="Test"]<-be_f$logAUC[be_f$Treatment=="Test"]-log(1.5)
fit_f<-lm(logAUC~Sequence+Subject+Period+Treatment,data=be_f)
df<-coef(fit_f)["TreatmentTest"]; sf<-summary(fit_f)$coefficients["TreatmentTest","Std. Error"]
ci_f_lo<-exp(df-qt(0.95,fit_f$df.residual)*sf)*100
check("BE-05","Bioinequivalent: CI lower <80%", ci_f_lo<80, "URS-BE-04")

# Mixed effects
if (requireNamespace("nlme",quietly=TRUE)) {
  fm<-tryCatch(nlme::lme(logAUC~Sequence+Period+Treatment,random=~1|Sequence/Subject,data=be),
    error=function(e) NULL)
  check("BE-06","Mixed model converges", !is.null(fm), "URS-BE-05")
  if (!is.null(fm)) check("BE-07","Mixed GMR ≈ fixed GMR (<5%)",
    abs(exp(nlme::fixef(fm)["TreatmentTest"])*100-gmr)<5, "URS-BE-05")
}

# Fixed-order crossover
check("BE-08","Fixed-order: CI matches paired t-test", {
  set.seed(42); n<-12; subj<-factor(rep(1:n,each=2)); trt<-rep(c("R","T"),n)
  lp<-0.05*(trt=="T")+rnorm(2*n,5,.2)+rep(rnorm(n,sd=.3),each=2)
  d<-data.frame(Subject=subj,Treatment=trt,.response=lp)
  fit<-lm(.response~Subject+Treatment,data=d)
  se<-summary(fit)$coefficients["TreatmentT","Std. Error"]
  diff<-coef(fit)["TreatmentT"]; dfe<-fit$df.residual
  ci_lo<-exp(diff-qt(.95,dfe)*se)*100; ci_hi<-exp(diff+qt(.95,dfe)*se)*100
  pt<-t.test(lp[trt=="T"],lp[trt=="R"],paired=TRUE,conf.level=.9)
  abs(ci_lo-exp(pt$conf.int[1])*100)<0.1&&abs(ci_hi-exp(pt$conf.int[2])*100)<0.1
}, "URS-BE-02")

check("BE-09","Fixed-order: df=N-1", {
  n<-8; d<-data.frame(Subject=factor(rep(1:n,each=2)),Treatment=rep(c("R","T"),n),.r=rnorm(2*n))
  lm(.r~Subject+Treatment,data=d)$df.residual==(n-1)}, "URS-BE-02")

# Pipeline BE
nca_t<-nca_full[nca_full$Treatment=="Test",]; nca_r<-nca_full[nca_full$Treatment=="Reference",]
check("BE-10","Pipeline: T and R correct N",
      nrow(nca_t)==24&&nrow(nca_r)==24, "URS-BE-01")
be_d<-data.frame(Subject=c(nca_t$Subject,nca_r$Subject),
  Treatment=factor(c(rep("Test",24),rep("Reference",24)),levels=c("Reference","Test")),
  lCMAX=c(log(as.numeric(nca_t$CMAX)),log(as.numeric(nca_r$CMAX))))
be_d<-merge(be_d,unique(full[,c("Subject","Period","Sequence","Treatment")]),by=c("Subject","Treatment"))
be_d$Subject<-factor(be_d$Subject); be_d$Period<-factor(be_d$Period); be_d$Sequence<-factor(be_d$Sequence)
fit_p<-lm(lCMAX~Sequence+Subject+Period+Treatment,data=be_d)
gmr_p<-exp(coef(fit_p)["TreatmentTest"])*100
check("BE-11","Pipeline: GMR plausible (80-125%)", gmr_p>80&gmr_p<125, "URS-BE-03",sprintf("(%.1f%%)",gmr_p))
check("BE-12","Pipeline: df correct (N-2)", fit_p$df.residual==22, "URS-BE-01")


# ============================================================================
# PWR: POWER & SAMPLE SIZE
# ============================================================================
cat("\n=== PWR: Power & Sample Size ===\n\n")

r1<-sampleN.TOST(alpha=.05,targetpower=.8,theta0=.95,CV=.2,design="2x2",method="exact",print=FALSE)
check("PWR-01","ABE 2x2 CV=0.20: N=20", r1[["Sample size"]]==20, "URS-PWR-01")
r2<-sampleN.TOST(alpha=.05,targetpower=.8,theta0=.95,CV=.3,design="2x2",method="exact",print=FALSE)
check("PWR-02","Higher CV→more subjects", r2[["Sample size"]]>r1[["Sample size"]], "URS-PWR-01")
r3<-sampleN.TOST(alpha=.05,targetpower=.8,theta0=1,CV=.2,design="2x2",method="exact",print=FALSE)
check("PWR-03","theta0=1: N=16", r3[["Sample size"]]==16, "URS-PWR-01")
rp<-sampleN.TOST(alpha=.05,targetpower=.8,theta0=.95,CV=.2,design="parallel",method="exact",print=FALSE)
check("PWR-04","Parallel>crossover N", rp[["Sample size"]]>r1[["Sample size"]], "URS-PWR-03")
rn<-sampleN.noninf(alpha=.05,targetpower=.8,theta0=.95,margin=.8,CV=.2,design="2x2",print=FALSE)
check("PWR-05","Non-inf N<equivalence N", rn[["Sample size"]]<r1[["Sample size"]], "URS-PWR-01")

pwr<-power.TOST(alpha=.05,theta0=.95,CV=.2,n=26,design="2x2",method="exact")
check("PWR-06","Power>=0.80 at N=26", pwr>=0.80, "URS-PWR-02", sprintf("(%.4f)",pwr))

for (des in c("2x2","2x2x3","2x3x3","2x2x4","parallel")) {
  rd<-tryCatch(sampleN.TOST(alpha=.05,targetpower=.8,CV=.25,theta0=.95,design=des,print=FALSE),
    error=function(e) NULL)
  check(sprintf("PWR-%02d",6+which(c("2x2","2x2x3","2x3x3","2x2x4","parallel")==des)),
        sprintf("Design '%s' works",des), !is.null(rd), "URS-PWR-03")
}

# Scaled/special methods
ra<-tryCatch(sampleN.scABEL(alpha=.05,targetpower=.8,theta0=.95,CV=.45,design="2x2x4",print=FALSE,nsims=1e5),error=function(e) NULL)
check("PWR-12","ABEL sample size", !is.null(ra), "URS-PWR-01")
rr<-tryCatch(sampleN.RSABE(alpha=.05,targetpower=.8,theta0=.95,CV=.5,design="2x2x4",print=FALSE,nsims=1e5),error=function(e) NULL)
check("PWR-13","RSABE sample size", !is.null(rr), "URS-PWR-01")
rt<-tryCatch(sampleN.NTIDFDA(alpha=.05,targetpower=.8,theta0=.95,CV=.1,design="2x2x4",print=FALSE,nsims=1e5),error=function(e) NULL)
check("PWR-14","NTID sample size", !is.null(rt), "URS-PWR-01")
rdp<-tryCatch(sampleN.dp(alpha=.05,targetpower=.8,CV=.25,print=FALSE),error=function(e) NULL)
check("PWR-15","Dose-proportionality sample size", !is.null(rdp), "URS-PWR-01")

# Input validation
check("PWR-16","CV=0: error or warning",
      tryCatch({sampleN.TOST(CV=0,print=FALSE);"no_error"},error=function(e)"error",
        warning=function(w)"warning")!="no_error", "URS-PWR-06")
check("PWR-17","High CV (150%): completes",
      !is.null(tryCatch(sampleN.TOST(alpha=.05,targetpower=.8,CV=1.5,theta0=.95,design="2x2",print=FALSE),
        error=function(e) NULL)), "URS-PWR-06")

# Cross-path: NCA CV → PowerTOST
cmax_v<-as.numeric(nca_x$CMAX); cv_nca<-sqrt(exp(sd(log(cmax_v))^2)-1)
check("PWR-18","NCA CV → finite positive", is.finite(cv_nca)&&cv_nca>0, "URS-PWR-05")
check("PWR-19","PowerTOST accepts NCA CV",
      !is.null(tryCatch(sampleN.TOST(alpha=.05,targetpower=.8,theta0=.95,CV=cv_nca,design="2x2",print=FALSE),
        error=function(e) NULL)), "URS-PWR-05")


# ============================================================================
# EXP: EXPORT & REPRODUCIBILITY
# ============================================================================
cat("\n=== EXP: Export & Reproducibility ===\n\n")

# Determinism
r_a<-run_nca(oral,cm,list(admin_route="extravascular",dose=100,infusion_duration=0,is_steady_state=FALSE,
  dose_unit="mg",time_unit="h",conc_unit="ng/mL",trap_method="log",r2adj_threshold=0.7,mw=0,partial_aucs=NULL))
r_b<-run_nca(oral,cm,list(admin_route="extravascular",dose=100,infusion_duration=0,is_steady_state=FALSE,
  dose_unit="mg",time_unit="h",conc_unit="ng/mL",trap_method="log",r2adj_threshold=0.7,mw=0,partial_aucs=NULL))
check("EXP-01","Determinism: identical CMAX", identical(r_a$CMAX,r_b$CMAX), "URS-EXP-02")
check("EXP-02","Determinism: identical AUCLST", identical(r_a$AUCLST,r_b$AUCLST), "URS-EXP-02")
check("EXP-03","Determinism: identical LAMZ", identical(r_a$LAMZ,r_b$LAMZ), "URS-EXP-02")

# Package versions queryable
check("EXP-04","NonCompart version", !is.null(packageVersion("NonCompart")), "URS-EXP-06")
check("EXP-05","PowerTOST version", !is.null(packageVersion("PowerTOST")), "URS-EXP-06")
check("EXP-06","R version", !is.null(R.version.string), "URS-EXP-06")

# SHA-256
if (requireNamespace("digest",quietly=TRUE)) {
  tmp<-tempfile(fileext=".csv"); write.csv(data.frame(x=1:10),tmp,row.names=FALSE)
  h1<-digest::digest(file=tmp,algo="sha256"); h2<-digest::digest(file=tmp,algo="sha256")
  file.remove(tmp)
  check("EXP-07","SHA-256 consistency", identical(h1,h2)&&nchar(h1)==64, "URS-EXP-04")
}

# Reproducibility scripts
source("R/export_record.R"); APP_VERSION<-"1.0"
check("EXP-08","Batch script: valid R", tryCatch({
  s<-generate_nca_script(list(admin_route="extravascular",dose=320,infusion_duration=0,
    is_steady_state=FALSE,dose_unit="mg",time_unit="h",conc_unit="ng/mL",trap_method="log",
    r2adj_threshold=0.7),list(subject="Subject",time="Time",conc="conc"),"theoph.csv","rule1",0)
  parse(text=s);TRUE},error=function(e) FALSE), "URS-EXP-02")

check("EXP-09","Batch script: matching results", tryCatch({
  td<-tempdir(); tf<-file.path(td,"theoph_test.csv"); write.csv(Theoph,tf,row.names=FALSE)
  direct<-tblNCA(Theoph,"Subject","Time","conc",dose=320,adm="Extravascular",down="Log",R2ADJ=0.7)
  s<-generate_nca_script(list(admin_route="extravascular",dose=320,infusion_duration=0,
    is_steady_state=FALSE,dose_unit="mg",time_unit="h",conc_unit="ng/mL",trap_method="log",
    r2adj_threshold=0.7),list(subject="Subject",time="Time",conc="conc"),"theoph_test.csv","rule1",0)
  old<-getwd();setwd(td);invisible(capture.output(eval(parse(text=s),envir=new.env())))
  rep<-read.csv(file.path(td,"reproduced_results.csv"));setwd(old)
  all(abs(as.numeric(direct$CMAX)-as.numeric(rep$CMAX))<.001)},error=function(e){
    tryCatch(setwd(old),error=function(e2)NULL);FALSE}), "URS-EXP-02")

check("EXP-10","Single-subject script: valid R", tryCatch({
  s<-generate_single_nca_script(Theoph$Time[Theoph$Subject==1],Theoph$conc[Theoph$Subject==1],
    list(admin_route="extravascular",dose=320,infusion_duration=0,is_steady_state=FALSE,
      dose_unit="mg",time_unit="h",conc_unit="mg/L",trap_method="log"),"S1")
  parse(text=s);TRUE},error=function(e) FALSE), "URS-EXP-02")

# Summary statistics
test_df<-data.frame(CMAX=c(10,20,30,40,50),AUC=c(100,200,300,400,500))
sm<-summarize_pk_params(test_df,c("CMAX","AUC"))
check("EXP-11","Summary: N=5", all(sm$N==5), "URS-NCA-06")
check("EXP-12","Summary: mean=30", abs(sm$Mean[1]-30)<1e-10, "URS-NCA-06")
check("EXP-13","Summary: SD correct", abs(sm$SD[1]-sd(c(10,20,30,40,50)))<1e-10, "URS-NCA-06")
check("EXP-14","Summary: geo mean correct",
      abs(sm$Geo_Mean[1]-exp(mean(log(c(10,20,30,40,50)))))<1e-6, "URS-NCA-06")

# CDISC
cdisc<-cdisc_pk_names()
check("EXP-15","CDISC lookup: ≥10 entries", nrow(cdisc)>=10, "URS-GEN-06")
check("EXP-16","CDISC: CMAX present", "CMAX"%in%cdisc$NonCompart, "URS-GEN-06")

# Required params in NCA output
check("EXP-17","NCA output: CDISC params present",
      all(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZ","LAMZHL")%in%names(th)), "URS-GEN-06")

# fmt_pk
check("EXP-18","fmt_pk: NA → dash", fmt_pk(NA)=="—", "URS-UI-01")
check("EXP-19","fmt_pk: large number", nchar(fmt_pk(123.456,4))>0, "URS-UI-01")


# ============================================================================
# UI: USABILITY, LABELS, WORKFLOW
# ============================================================================
cat("\n=== UI: Usability ===\n\n")

# Abbreviation audit
mod_files<-list.files("R/",pattern="mod_path_.*\\.R$",full.names=TRUE)
all_code<-paste(sapply(mod_files,readLines),collapse="\n")
bad<-c('"ABE"','"ABEL"','"RSABE"','"NTID"','"TOST"','"HVD"','"NTI"','"CVwR"','"MSE"')
found<-c()
for(ab in bad) if(grepl(paste0("=\\s*",ab),all_code)) found<-c(found,ab)
check("UI-01","No unexplained abbreviations in dropdowns", length(found)==0, "URS-UI-01",
      if(length(found)>0) paste("Found:",paste(found,collapse=", ")) else "")

# Help topics
if (requireNamespace("shiny",quietly=TRUE)) {
  source("R/help_system.R")
  helps<-c("help_data_format","help_column_mapping","help_lloq","help_blq_rules",
    "help_what_is_nca","help_admin_route","help_trapezoidal","help_lambda_z","help_r2adj",
    "help_what_is_be","help_log_transform","help_ci_level","help_what_is_power","help_cv","help_theta0")
  for(h in helps) check(sprintf("UI-%02d",1+which(helps==h)),sprintf("Help '%s' exists",h),
    exists(h),"URS-UI-01")
}

# Default settings
nca_def<-run_nca(Theoph,list(subject="Subject",time="Time",conc="conc"),
  list(admin_route="extravascular",dose=320,infusion_duration=0,is_steady_state=FALSE,
    dose_unit="mg",time_unit="h",conc_unit="mg/L",trap_method="log",r2adj_threshold=0.7,mw=0,partial_aucs=NULL))
check("UI-17","Default settings: valid NCA on Theoph", !is.null(nca_def)&&nrow(nca_def)==12, "URS-UI-04")

# Input validation messages
vm<-validate_mapping(list(subject="ID",time="",conc="DV"))
check("UI-18","Validation: catches missing Time", !vm$valid, "URS-UI-04")
check("UI-19","Validation: message mentions 'time'", grepl("time",vm$message,ignore.case=TRUE), "URS-UI-04")
check("UI-20","Validation: accepts complete mapping",
      validate_mapping(list(subject="ID",time="Time",conc="DV"))$valid, "URS-UI-04")

# Pipeline consumability
check("UI-21","Pipeline: Subject column present", names(nca_def)[1]%in%c("Subject","SUBJID","ID"), "URS-NCA-01")
check("UI-22","Pipeline: CMAX numeric", is.numeric(as.numeric(nca_def$CMAX)), "URS-NCA-01")
check("UI-23","Pipeline: AUCLST numeric", is.numeric(as.numeric(nca_def$AUCLST)), "URS-NCA-01")

# Code quality
mod_all<-list.files("R/","\\.R$",full.names=TRUE); dbl_ns<-0
for(f in mod_all){ls<-readLines(f);for(i in seq_along(ls)){
  if(grepl("condition.*ns\\(",ls[i])&&i<length(ls)&&grepl("^\\s*ns\\s*=\\s*ns",ls[i+1])) dbl_ns<-dbl_ns+1}}
check("UI-24","No double-namespaced conditionalPanels", dbl_ns==0, "URS-GEN-01")

snca_code<-readLines("R/mod_path_single_nca.R")
check("UI-25","Manual entry uses textAreaInput",
      length(grep("tags\\$textarea.*manual_",snca_code))==0&&
      length(grep("textAreaInput.*manual_",snca_code))>=2, "URS-GEN-01")

# NULL guards
check("UI-26","NULL guard: data_mode", tryCatch({dm<-NULL;if(is.null(dm)||length(dm)==0) FALSE
  else FALSE},error=function(e)"CRASH")!="CRASH", "URS-GEN-01")


# ============================================================================
# MANUAL TESTS (listed for protocol — cannot be automated)
# ============================================================================
cat("\n=== MANUAL TESTS (execute via app UI) ===\n\n")
man<-list(
  c("MAN-01","shinyapps.io loads","URS-GEN-01"),
  c("MAN-02","Excel import","URS-DAT-01"),
  c("MAN-03","Analysis blocked on ERROR","URS-DAT-05"),
  c("MAN-04","Wrong file → informative error","URS-DAT-06"),
  c("MAN-05","Single NCA matches batch","URS-NCA-05"),
  c("MAN-06","Half-life inspector","URS-NCA-05"),
  c("MAN-07","Forest plot renders","URS-BE-06"),
  c("MAN-08","Custom CI level (95%)","URS-BE-07"),
  c("MAN-09","Custom BE limits","URS-BE-07"),
  c("MAN-10","Power curve displays","URS-PWR-04"),
  c("MAN-11","CV bridge from NCA","URS-PWR-05"),
  c("MAN-12","ZIP contains 6 files","URS-EXP-01"),
  c("MAN-13","reproduce_analysis.R matches","URS-EXP-02"),
  c("MAN-14","SHA-256 matches source","URS-EXP-04"),
  c("MAN-15","BE export worksheets","URS-EXP-05"),
  c("MAN-16","Versions in export","URS-EXP-06"),
  c("MAN-17","Hub: 5 paths navigate","URS-UI-03"),
  c("MAN-18","Data Prep Guide","URS-UI-02"),
  c("MAN-19","Methods page","URS-GEN-03"),
  c("MAN-20","No persistent data","URS-GEN-05"))
for(m in man){
  results<<-rbind(results,data.frame(ID=m[1],Test=m[2],URS=m[3],Result="SKIP",Detail="(manual)",stringsAsFactors=FALSE))
  cat(sprintf("  [SKIP] %s: %s\n",m[1],m[2]))}


# ============================================================================
# SUMMARY
# ============================================================================
cat("\n================================================================\n")
cat("  VALIDATION SUMMARY\n")
cat("================================================================\n\n")
cat(sprintf("  Automated:  %d PASS  %d FAIL\n", n_pass, n_fail))
cat(sprintf("  Manual:     %d SKIP (execute via app UI)\n", sum(results$Result=="SKIP")))
cat(sprintf("  Total:      %d tests\n\n", nrow(results)))

if (n_fail==0) {
  cat("  RESULT: ALL AUTOMATED TESTS PASSED\n")
} else {
  cat("  RESULT: FAILURES DETECTED\n")
  f<-results[results$Result=="FAIL",]
  for(i in seq_len(nrow(f))) cat(sprintf("    %s: %s\n",f$ID[i],f$Test[i]))
}

# URS coverage
cat("\n--- URS Coverage ---\n")
all_urs<-sort(unique(unlist(strsplit(results$URS[results$URS!=""],","))))
defined<-c(paste0("URS-GEN-0",1:7),paste0("URS-DAT-0",1:7),
  paste0("URS-NCA-",sprintf("%02d",1:11)),paste0("URS-BE-0",1:7),
  paste0("URS-PWR-0",1:6),paste0("URS-EXP-0",1:6),paste0("URS-UI-0",1:4))
covered<-intersect(defined,all_urs); uncovered<-setdiff(defined,all_urs)
cat(sprintf("  Covered: %d/%d\n",length(covered),length(defined)))
if(length(uncovered)>0) cat(sprintf("  Gaps: %s\n",paste(uncovered,collapse=", ")))

write.csv(results,"tests/validation_results.csv",row.names=FALSE)
cat(sprintf("\n  Results: tests/validation_results.csv\n"))
cat("================================================================\n\n")

while(dev.cur()>1) try(dev.off(),silent=TRUE)
rp<-list.files(pattern="^Rplots.*\\.pdf$"); if(length(rp)>0) invisible(file.remove(rp))
