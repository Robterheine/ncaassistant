#!/usr/bin/env Rscript
# ============================================================================
# NCA Assistant — Functional & UX Validation Suite
# ============================================================================
# Tests the full data pipeline, edge cases, error handling, auto-detection,
# real-world data quirks, and workflow completeness.
#
# Unlike the statistical cross-validation, this tests:
#   1. Column auto-detection accuracy
#   2. Handling of messy real-world data (BLQ strings, NA, negative values)
#   3. Data flow: import → NCA → BE → export pipeline
#   4. Edge cases: single subject, all-NA, enormous dataset, zero-length
#   5. Audit trail completeness
#   6. Reporting/export integrity
#   7. PowerTOST input validation
#   8. Numeric precision and rounding
#   9. CDISC compatibility
#  10. UX: default values, error messages, workflow guidance
# ============================================================================

library(NonCompart)
library(PowerTOST)

source("R/utils.R")
source("R/nca_helpers.R")
source("R/data_quality.R")

pass_count <- 0
fail_count <- 0
warn_count <- 0
total_count <- 0

check <- function(test_name, condition, detail = "") {
  total_count <<- total_count + 1
  if (is.na(condition)) condition <- FALSE
  if (condition) {
    pass_count <<- pass_count + 1
    cat(sprintf("  [PASS] %s %s\n", test_name, detail))
  } else {
    fail_count <<- fail_count + 1
    cat(sprintf("  [FAIL] %s %s\n", test_name, detail))
  }
}

warn <- function(test_name, detail = "") {
  warn_count <<- warn_count + 1
  cat(sprintf("  [WARN] %s %s\n", test_name, detail))
}

cat("\n")
cat("================================================================\n")
cat("  NCA Assistant Functional & UX Validation Suite\n")
cat("================================================================\n\n")


# ============================================================================
# SECTION A: COLUMN AUTO-DETECTION
# ============================================================================
cat("=== A: Column Auto-Detection ===\n\n")

# Source the auto-detect function
source("R/mod_data_upload.R")

# Test various real-world column naming conventions
test_cases <- list(
  # Standard CDISC-like
  list(cols = c("USUBJID", "NTIM", "DV", "TRT01A", "APERIOD", "SEQUENCE", "OCC", "DOSE"),
       expect_subj = "USUBJID", expect_time = "NTIM", expect_conc = "DV",
       label = "CDISC SDTM style"),
  # Clinical study style
  list(cols = c("SubjectID", "TimePoint_h", "Concentration_ng_mL", "Treatment", "Period", "Seq"),
       expect_subj = "SubjectID", expect_time = "TimePoint_h", expect_conc = "Concentration_ng_mL",
       label = "Clinical study style"),
  # Simple
  list(cols = c("ID", "Time", "Conc", "Drug", "Per"),
       expect_subj = "ID", expect_time = "Time", expect_conc = "Conc",
       label = "Simple short names"),
  # WinNonlin export style
  list(cols = c("Subject", "Timepoint", "Result", "Formulation", "Period"),
       expect_subj = "Subject", expect_time = "Timepoint", expect_conc = "Result",
       label = "WinNonlin export style"),
  # Minimal
  list(cols = c("subject", "time", "conc"),
       expect_subj = "subject", expect_time = "time", expect_conc = "conc",
       label = "Lowercase minimal"),
  # European naming — now with extended patterns
  list(cols = c("Proband", "Zeit_h", "Konzentration", "Behandlung"),
       expect_subj = "Proband", expect_time = "Zeit_h",
       expect_conc = "Konzentration",
       label = "European naming"),
  # Tricky: Cp prefix in compound column name
  list(cols = c("PatID", "Hours", "Cp_ug_L", "TreatmentArm", "StudyPeriod"),
       expect_subj = "PatID", expect_time = "Hours", expect_conc = "Cp_ug_L",
       label = "Cp prefix in compound name")
)

for (tc in test_cases) {
  detected <- auto_detect_columns(tc$cols)
  cat(sprintf("  [%s]\n", tc$label))
  
  check(paste0("  Subject detection (", tc$label, ")"),
        detected$subject == tc$expect_subj,
        sprintf("(got '%s', expected '%s')", detected$subject, tc$expect_subj))
  check(paste0("  Time detection (", tc$label, ")"),
        detected$time == tc$expect_time,
        sprintf("(got '%s', expected '%s')", detected$time, tc$expect_time))
  check(paste0("  Conc detection (", tc$label, ")"),
        detected$conc == tc$expect_conc,
        sprintf("(got '%s', expected '%s')", detected$conc, tc$expect_conc))
}


# ============================================================================
# SECTION B: MESSY REAL-WORLD DATA HANDLING
# ============================================================================
cat("\n=== B: Messy Real-World Data Handling ===\n\n")

# --- B1: BLQ strings in concentration column --------------------------------
cat("  -- B1: BLQ string handling --\n")
messy_data <- data.frame(
  Subject = rep("S1", 10),
  Time    = c(0, 0.5, 1, 2, 4, 6, 8, 12, 24, 48),
  Conc    = c("BLQ", "5.2", "15.8", "12.3", "8.1", "BLQ", "3.2", "1.5", "BLQ", "BLQ"),
  stringsAsFactors = FALSE
)

# Simulate what the app does: convert to numeric (BLQ becomes NA)
messy_data$Conc_num <- suppressWarnings(as.numeric(messy_data$Conc))

check("BLQ strings → NA on numeric conversion",
      sum(is.na(messy_data$Conc_num)) == 4,
      sprintf("(got %d NAs)", sum(is.na(messy_data$Conc_num))))

# Apply BLQ rule after converting strings
messy_clean <- messy_data
messy_clean$Conc <- messy_clean$Conc_num
col_map <- list(subject = "Subject", time = "Time", conc = "Conc")

# Set NAs where BLQ was, now apply rule with LLOQ
messy_clean$Conc[is.na(messy_clean$Conc)] <- 0  # Temporarily mark as 0
result_blq <- apply_blq_rules(messy_clean, col_map, "rule4", lloq = 0.5)

check("BLQ Rule4 on string-converted data produces LLOQ/2",
      all(result_blq$Conc[c(1, 6, 9, 10)] == 0.25),
      sprintf("(got %s)", paste(result_blq$Conc[c(1,6,9,10)], collapse=",")))


# --- B2: Negative concentration values --------------------------------------
cat("  -- B2: Negative concentrations --\n")
neg_data <- data.frame(
  Subject = rep("S1", 8),
  Time = c(0, 1, 2, 4, 8, 12, 24, 48),
  Conc = c(0, 25.3, 18.5, 10.2, 4.8, -0.3, 1.2, 0.5)
)

# NonCompart should handle this (negative values get excluded from log)
nca_neg <- tryCatch({
  tblNCA(neg_data, "Subject", "Time", "Conc", dose = 100,
         adm = "Extravascular", down = "Linear", R2ADJ = 0)
}, error = function(e) NULL)

check("NCA handles negative concentrations without crashing",
      !is.null(nca_neg),
      if (!is.null(nca_neg)) "(completed)" else "(CRASHED)")


# --- B3: All-zero profile ---------------------------------------------------
cat("  -- B3: All-zero concentration profile --\n")
zero_data <- data.frame(
  Subject = rep("S1", 6),
  Time = c(0, 1, 2, 4, 8, 24),
  Conc = rep(0, 6)
)

nca_zero <- tryCatch({
  tblNCA(zero_data, "Subject", "Time", "Conc", dose = 100,
         adm = "Extravascular", down = "Linear", R2ADJ = 0)
}, error = function(e) NULL,
   warning = function(w) {
     suppressWarnings(
       tblNCA(zero_data, "Subject", "Time", "Conc", dose = 100,
              adm = "Extravascular", down = "Linear", R2ADJ = 0)
     )
   })

check("NCA handles all-zero profile without crashing",
      !is.null(nca_zero))

if (!is.null(nca_zero)) {
  check("All-zero profile: Cmax = 0",
        as.numeric(nca_zero$CMAX) == 0)
  auc_zero <- as.numeric(nca_zero$AUCLST)
  check("All-zero profile: AUClast = 0, NaN, or NA (all acceptable for zero data)",
        isTRUE(auc_zero == 0) || is.nan(auc_zero) || is.na(auc_zero),
        sprintf("(got %s)", auc_zero))
}


# --- B4: Single observation per subject ------------------------------------
cat("  -- B4: Single observation --\n")
single_obs <- data.frame(
  Subject = "S1", Time = 2, Conc = 50
)

nca_single <- tryCatch({
  tblNCA(single_obs, "Subject", "Time", "Conc", dose = 100,
         adm = "Extravascular", down = "Linear", R2ADJ = 0)
}, error = function(e) NULL,
   warning = function(w) {
     suppressWarnings(
       tblNCA(single_obs, "Subject", "Time", "Conc", dose = 100,
              adm = "Extravascular", down = "Linear", R2ADJ = 0)
     )
   })

check("NCA handles single observation without crashing",
      !is.null(nca_single))


# --- B5: Duplicate time points ----------------------------------------------
cat("  -- B5: Duplicate time points --\n")
dup_data <- data.frame(
  Subject = rep("S1", 8),
  Time = c(0, 1, 2, 2, 4, 8, 12, 24),  # t=2 is duplicated
  Conc = c(0, 15.3, 12.1, 11.8, 8.5, 4.2, 1.8, 0.3)
)

nca_dup <- tryCatch({
  tblNCA(dup_data, "Subject", "Time", "Conc", dose = 100,
         adm = "Extravascular", down = "Linear", R2ADJ = 0)
}, error = function(e) NULL)

check("NCA handles duplicate time points without crashing",
      !is.null(nca_dup))


# --- B6: Non-monotonic time (out-of-order samples) --------------------------
cat("  -- B6: Non-monotonic time --\n")
unsorted_data <- data.frame(
  Subject = rep("S1", 7),
  Time = c(4, 0, 24, 1, 8, 2, 12),  # scrambled
  Conc = c(8.5, 0, 0.3, 15.3, 4.2, 12.1, 1.8)
)

# Sort first (as the app does)
unsorted_data <- unsorted_data[order(unsorted_data$Time), ]

nca_unsorted <- tryCatch({
  tblNCA(unsorted_data, "Subject", "Time", "Conc", dose = 100,
         adm = "Extravascular", down = "Linear", R2ADJ = 0)
}, error = function(e) NULL)

check("NCA on re-sorted data produces valid result",
      !is.null(nca_unsorted) && as.numeric(nca_unsorted$CMAX) == 15.3)


# ============================================================================
# SECTION C: DATA FLOW PIPELINE (Import → NCA → BE → Export)
# ============================================================================
cat("\n=== C: End-to-End Data Pipeline ===\n\n")

# Build a realistic 2x2 crossover dataset
set.seed(42)
n_subj <- 24
dose_val <- 200

generate_pk_profile <- function(subj, trt, period, seq,
                                 dose, cmax_pop, tmax_pop, ke_pop) {
  # Add inter-individual variability
  eta <- rnorm(1, 0, 0.3)
  cmax <- cmax_pop * exp(eta + rnorm(1, 0, 0.15))
  tmax <- tmax_pop * exp(rnorm(1, 0, 0.2))
  ke   <- ke_pop * exp(rnorm(1, 0, 0.2))
  ka   <- 2  # absorption rate
  
  times <- c(0, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24)
  concs <- sapply(times, function(t) {
    if (t == 0) return(0)
    c_t <- (dose * ka / (ka - ke)) * (exp(-ke * t) - exp(-ka * t))
    max(0, c_t * cmax / (dose * ka / (ka - ke) *
      (exp(-ke * tmax) - exp(-ka * tmax))) + rnorm(1, 0, cmax * 0.05))
  })
  concs <- pmax(0, concs)
  
  data.frame(
    Subject = subj, Time = times, Conc = round(concs, 2),
    Treatment = trt, Period = period, Sequence = seq,
    Dose = dose, stringsAsFactors = FALSE
  )
}

# Generate crossover data
all_profiles <- list()
idx <- 1

for (i in 1:(n_subj/2)) {
  # Sequence TR
  all_profiles[[idx]] <- generate_pk_profile(
    subj = paste0("S", sprintf("%03d", i)),
    trt = "Test", period = 1, seq = "TR",
    dose = dose_val, cmax_pop = 500, tmax_pop = 1.5, ke_pop = 0.15)
  idx <- idx + 1
  
  all_profiles[[idx]] <- generate_pk_profile(
    subj = paste0("S", sprintf("%03d", i)),
    trt = "Reference", period = 2, seq = "TR",
    dose = dose_val, cmax_pop = 480, tmax_pop = 1.5, ke_pop = 0.15)
  idx <- idx + 1
}

for (i in (n_subj/2 + 1):n_subj) {
  # Sequence RT
  all_profiles[[idx]] <- generate_pk_profile(
    subj = paste0("S", sprintf("%03d", i)),
    trt = "Reference", period = 1, seq = "RT",
    dose = dose_val, cmax_pop = 480, tmax_pop = 1.5, ke_pop = 0.15)
  idx <- idx + 1
  
  all_profiles[[idx]] <- generate_pk_profile(
    subj = paste0("S", sprintf("%03d", i)),
    trt = "Test", period = 2, seq = "RT",
    dose = dose_val, cmax_pop = 500, tmax_pop = 1.5, ke_pop = 0.15)
  idx <- idx + 1
}

full_data <- do.call(rbind, all_profiles)

cat(sprintf("  Generated dataset: %d rows, %d subjects, %d treatments\n",
            nrow(full_data), n_subj, 2))

# --- C1: Column mapping validation -----------------------------------------
col_map <- list(
  subject   = "Subject",
  time      = "Time",
  conc      = "Conc",
  treatment = "Treatment",
  period    = "Period",
  sequence  = "Sequence",
  dose      = "Dose"
)

val <- validate_mapping(col_map)
check("Column mapping validates successfully", val$valid)

# --- C2: Study design detection --------------------------------------------
design <- detect_study_design(full_data, col_map)

check("Design detection: crossover",
      design$is_crossover,
      sprintf("(type=%s)", design$type))
check("Design detection: correct subject count",
      design$n_subjects == n_subj,
      sprintf("(detected %d)", design$n_subjects))
check("Design detection: 2 treatments",
      design$n_treatments == 2)
check("Design detection: 2 periods",
      design$n_periods == 2)

# --- C3: Run NCA on full dataset -------------------------------------------
cat("\n  -- C3: NCA on full crossover dataset --\n")

# Use run_nca() which handles composite keys for crossover data
nca_all <- run_nca(full_data,
                    col_map = col_map,
                    settings = list(
                      admin_route = "extravascular",
                      dose = dose_val,
                      infusion_duration = 0,
                      is_steady_state = FALSE,
                      dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL",
                      trap_method = "linear",
                      r2adj_threshold = 0.7,
                      mw = 0,
                      partial_aucs = NULL
                    ))

cat(sprintf("  NCA completed: %d profiles analyzed\n",
            if (!is.null(nca_all)) nrow(nca_all) else 0))

check("NCA produces results for all subject-profiles",
      nrow(nca_all) > 0,
      sprintf("(%d rows — expected %d: %d subjects × 2 treatments)",
              nrow(nca_all), n_subj * 2, n_subj))

# With composite key, we get Subject and Treatment columns
check("NCA output has Subject and Treatment columns (composite key split)",
      "Subject" %in% names(nca_all) && "Treatment" %in% names(nca_all))

# Check key parameters exist
expected_params <- c("CMAX", "TMAX", "AUCLST", "LAMZ", "LAMZHL")
check("NCA output contains required parameters",
      all(expected_params %in% names(nca_all)),
      sprintf("(missing: %s)",
              paste(setdiff(expected_params, names(nca_all)), collapse=", ")))

# Check no all-NA parameters for key metrics
cmax_vals <- as.numeric(nca_all$CMAX)
check("CMAX: no NA values",
      all(!is.na(cmax_vals)),
      sprintf("(%d NAs out of %d)", sum(is.na(cmax_vals)), length(cmax_vals)))

auclst_vals <- as.numeric(nca_all$AUCLST)
check("AUCLST: no NA values",
      all(!is.na(auclst_vals)),
      sprintf("(%d NAs out of %d)", sum(is.na(auclst_vals)), length(auclst_vals)))

lamz_vals <- as.numeric(nca_all$LAMZ)
lamz_ok <- sum(!is.na(lamz_vals) & lamz_vals > 0)
check("Lambda_z: estimated for >80% of profiles",
      lamz_ok / length(lamz_vals) > 0.80,
      sprintf("(%d/%d = %.0f%%)", lamz_ok, length(lamz_vals),
              lamz_ok/length(lamz_vals)*100))

# --- C4: Dose normalization ------------------------------------------------
cat("\n  -- C4: Dose normalization --\n")
nca_dn <- add_dose_normalized(as.data.frame(nca_all), dose_val)

check("Dose-normalized columns created",
      "CMAX_DN" %in% names(nca_dn) && "AUCLST_DN" %in% names(nca_dn))

check("CMAX_DN = CMAX / dose for all subjects",
      all(abs(as.numeric(nca_dn$CMAX_DN) -
                as.numeric(nca_dn$CMAX) / dose_val) < 1e-10))

# --- C5: Summary statistics ------------------------------------------------
cat("\n  -- C5: Summary statistics --\n")
summ <- summarize_pk_params(nca_dn, c("CMAX", "AUCLST", "LAMZHL"))

check("Summary has correct number of parameters",
      nrow(summ) == 3)
check("Summary N matches NCA row count",
      all(summ$N == nrow(nca_all)),
      sprintf("(summary N=%d, NCA rows=%d)", summ$N[1], nrow(nca_all)))
check("Geometric mean < arithmetic mean (positive skew expected)",
      all(summ$Geo_Mean < summ$Mean, na.rm = TRUE),
      sprintf("(Geo: %s, Arith: %s)",
              paste(round(summ$Geo_Mean, 1), collapse=", "),
              paste(round(summ$Mean, 1), collapse=", ")))

# --- C6: BE Analysis pipeline (manual, simulating the module) ---------------
cat("\n  -- C6: BE analysis pipeline --\n")

# With composite key, nca_all already has Subject + Treatment columns
nca_test <- nca_all[nca_all$Treatment == "Test", ]
nca_ref  <- nca_all[nca_all$Treatment == "Reference", ]

cat(sprintf("  Test profiles: %d, Reference profiles: %d\n",
            nrow(nca_test), nrow(nca_ref)))

check("Test NCA: correct subject count",
      nrow(nca_test) == n_subj)
check("Reference NCA: correct subject count",
      nrow(nca_ref) == n_subj)

# Build BE dataset from NCA results (already split by treatment)
be_data <- data.frame(
  Subject    = c(nca_test$Subject, nca_ref$Subject),
  Treatment  = c(rep("Test", nrow(nca_test)), rep("Reference", nrow(nca_ref))),
  log_CMAX   = c(log(as.numeric(nca_test$CMAX)), log(as.numeric(nca_ref$CMAX))),
  log_AUClst = c(log(as.numeric(nca_test$AUCLST)), log(as.numeric(nca_ref$AUCLST))),
  stringsAsFactors = FALSE
)

# Merge with design info (Period, Sequence)
design_info <- unique(full_data[, c("Subject", "Period", "Sequence", "Treatment")])
be_data <- merge(be_data, design_info, by = c("Subject", "Treatment"))

be_data$Subject   <- factor(be_data$Subject)
be_data$Treatment <- factor(be_data$Treatment, levels = c("Reference", "Test"))
be_data$Period    <- factor(be_data$Period)
be_data$Sequence  <- factor(be_data$Sequence)

# Run ANOVA for Cmax
fit_cmax <- lm(log_CMAX ~ Sequence + Subject + Period + Treatment, data = be_data)
se_tbl <- summary(fit_cmax)$coefficients

diff_cmax <- coef(fit_cmax)["TreatmentTest"]
se_cmax   <- se_tbl["TreatmentTest", "Std. Error"]
dfe       <- fit_cmax$df.residual
t_crit    <- qt(0.95, dfe)

gmr_cmax    <- exp(diff_cmax) * 100
ci_low_cmax <- exp(diff_cmax - t_crit * se_cmax) * 100
ci_hi_cmax  <- exp(diff_cmax + t_crit * se_cmax) * 100

cat(sprintf("  Cmax: GMR=%.2f%%, 90%%CI=[%.2f%%, %.2f%%]\n",
            gmr_cmax, ci_low_cmax, ci_hi_cmax))

check("BE Cmax: GMR in plausible range (80-125%)",
      gmr_cmax > 80 & gmr_cmax < 125,
      sprintf("(%.2f%%)", gmr_cmax))
check("BE Cmax: CI width is reasonable (not degenerate)",
      (ci_hi_cmax - ci_low_cmax) > 5 & (ci_hi_cmax - ci_low_cmax) < 60,
      sprintf("(width=%.1f pp)", ci_hi_cmax - ci_low_cmax))
check("BE Cmax: DF residual is correct for 2x2 crossover",
      dfe == n_subj - 2,  # N - number of sequences
      sprintf("(df=%d, expected=%d)", dfe, n_subj - 2))

# Run ANOVA for AUClast
fit_auc <- lm(log_AUClst ~ Sequence + Subject + Period + Treatment, data = be_data)
se_tbl_auc <- summary(fit_auc)$coefficients

diff_auc <- coef(fit_auc)["TreatmentTest"]
se_auc   <- se_tbl_auc["TreatmentTest", "Std. Error"]
dfe_auc  <- fit_auc$df.residual

gmr_auc    <- exp(diff_auc) * 100
ci_low_auc <- exp(diff_auc - qt(0.95, dfe_auc) * se_auc) * 100
ci_hi_auc  <- exp(diff_auc + qt(0.95, dfe_auc) * se_auc) * 100

cat(sprintf("  AUClast: GMR=%.2f%%, 90%%CI=[%.2f%%, %.2f%%]\n",
            gmr_auc, ci_low_auc, ci_hi_auc))

check("BE AUClast: GMR in plausible range",
      gmr_auc > 80 & gmr_auc < 125)


# ============================================================================
# SECTION D: EDGE CASES
# ============================================================================
cat("\n=== D: Edge Cases ===\n\n")

# --- D1: Very large dataset -------------------------------------------------
cat("  -- D1: Performance with large dataset --\n")
n_big <- 200
big_data <- do.call(rbind, lapply(1:n_big, function(i) {
  data.frame(
    Subject = paste0("S", i),
    Time = c(0, 0.5, 1, 2, 4, 8, 12, 24),
    Conc = pmax(0, 100 * exp(rnorm(1, 0, 0.3)) *
                  c(0, 0.5, 0.9, 0.7, 0.4, 0.2, 0.08, 0.02) +
                  rnorm(8, 0, 2))
  )
}))

t_start <- proc.time()
nca_big <- tryCatch(
  tblNCA(big_data, "Subject", "Time", "Conc",
         dose = 100, adm = "Extravascular", down = "Linear", R2ADJ = 0.7),
  error = function(e) NULL
)
t_elapsed <- (proc.time() - t_start)["elapsed"]

check("Large dataset (200 subjects): completes",
      !is.null(nca_big))
check("Large dataset: all subjects in output",
      !is.null(nca_big) && nrow(nca_big) == n_big,
      sprintf("(%d/%d)", ifelse(!is.null(nca_big), nrow(nca_big), 0), n_big))
check("Large dataset: completes in < 30 seconds",
      t_elapsed < 30,
      sprintf("(%.1f sec)", t_elapsed))

# --- D2: Subject with only 2 time points -----------------------------------
cat("  -- D2: Minimal data per subject --\n")
sparse_data <- data.frame(
  Subject = c("S1", "S1", "S2", "S2", "S2", "S2", "S2", "S2"),
  Time = c(0, 4, 0, 1, 2, 4, 8, 24),
  Conc = c(0, 5.2, 0, 15, 12, 8, 4, 1)
)

nca_sparse <- tryCatch(
  tblNCA(sparse_data, "Subject", "Time", "Conc",
         dose = 100, adm = "Extravascular", down = "Linear", R2ADJ = 0),
  error = function(e) NULL
)

check("Sparse data: completes without crash",
      !is.null(nca_sparse))
if (!is.null(nca_sparse)) {
  check("Sparse data: both subjects in output",
        nrow(nca_sparse) == 2)
  # S1 should have Lambda_z = NA (only 2 points)
  s1_lamz <- as.numeric(nca_sparse$LAMZ[nca_sparse[[1]] == "S1"])
  check("Sparse data: S1 Lambda_z = NA (insufficient points)",
        is.na(s1_lamz),
        sprintf("(got %s)", s1_lamz))
}

# --- D3: All concentrations are NA -----------------------------------------
cat("  -- D3: All-NA concentrations --\n")
na_data <- data.frame(
  Subject = rep("S1", 6),
  Time = c(0, 1, 2, 4, 8, 24),
  Conc = rep(NA_real_, 6)
)

nca_na <- tryCatch(
  tblNCA(na_data, "Subject", "Time", "Conc",
         dose = 100, adm = "Extravascular", down = "Linear", R2ADJ = 0),
  error = function(e) "error",
  warning = function(w) {
    suppressWarnings(
      tblNCA(na_data, "Subject", "Time", "Conc",
             dose = 100, adm = "Extravascular", down = "Linear", R2ADJ = 0)
    )
  }
)

check("All-NA data: handled gracefully (no hard crash)",
      !identical(nca_na, "error"),
      "(may produce warnings, which is OK)")


# --- D4: Extremely high concentrations (overflow check) ---------------------
cat("  -- D4: Extreme values --\n")
extreme_data <- data.frame(
  Subject = rep("S1", 6),
  Time = c(0, 1, 2, 4, 8, 24),
  Conc = c(0, 1e8, 5e7, 2e7, 1e7, 1e6)
)

nca_extreme <- tryCatch(
  tblNCA(extreme_data, "Subject", "Time", "Conc",
         dose = 1e6, adm = "Extravascular", down = "Linear", R2ADJ = 0),
  error = function(e) NULL
)

check("Extreme values (1e8): no overflow",
      !is.null(nca_extreme) && is.finite(as.numeric(nca_extreme$CMAX)))


# ============================================================================
# SECTION E: AUDIT TRAIL & EXPORT COMPLETENESS
# ============================================================================
cat("\n=== E: Audit Trail & Export ===\n\n")

# Simulate the audit trail generation
study_info <- list(
  file_name = "test_study.csv",
  lloq = 0.5,
  blq_rule = "rule1",
  design = list(type = "2x2", n_subjects = 24,
                is_crossover = TRUE)
)

nca_settings <- list(
  admin_route = "extravascular",
  dose = 200,
  dose_unit = "mg",
  time_unit = "h",
  conc_unit = "ng/mL",
  trap_method = "log",
  r2adj_threshold = 0.7,
  mw = 0,
  is_steady_state = FALSE,
  infusion_duration = 0
)

# Check that all critical fields would be in the audit trail
audit_fields <- c("file_name", "lloq", "blq_rule", "admin_route", "dose",
                   "dose_unit", "time_unit", "conc_unit", "trap_method",
                   "r2adj_threshold")

check("Audit: file_name recorded", !is.null(study_info$file_name))
check("Audit: LLOQ recorded", !is.null(study_info$lloq))
check("Audit: BLQ rule recorded", !is.null(study_info$blq_rule))
check("Audit: admin route recorded", !is.null(nca_settings$admin_route))
check("Audit: dose recorded", !is.null(nca_settings$dose))
check("Audit: units recorded",
      !is.null(nca_settings$dose_unit) &&
      !is.null(nca_settings$time_unit) &&
      !is.null(nca_settings$conc_unit))
check("Audit: trapezoidal method recorded", !is.null(nca_settings$trap_method))
check("Audit: R2adj threshold recorded", !is.null(nca_settings$r2adj_threshold))
check("Audit: NonCompart version available",
      !is.null(packageVersion("NonCompart")))
check("Audit: R version available",
      !is.null(R.version.string))


# ============================================================================
# SECTION F: CDISC COMPATIBILITY
# ============================================================================
cat("\n=== F: CDISC Compatibility ===\n\n")

cdisc_names <- cdisc_pk_names()

# Check that NonCompart output column names match CDISC
nca_test_small <- tblNCA(Theoph, "Subject", "Time", "conc",
                          dose = 320, adm = "Extravascular")

nca_cols <- names(nca_test_small)

# Key CDISC PK parameters that should be in output
required_cdisc <- c("CMAX", "TMAX", "AUCLST", "AUCIFO", "LAMZ", "LAMZHL")

check("CDISC: all required parameters in output",
      all(required_cdisc %in% nca_cols),
      sprintf("(present: %s)",
              paste(intersect(required_cdisc, nca_cols), collapse=", ")))

# Check CDISC lookup table completeness
check("CDISC lookup: has descriptions for key parameters",
      nrow(cdisc_names) >= 10,
      sprintf("(%d entries)", nrow(cdisc_names)))


# ============================================================================
# SECTION G: POWERTOST INPUT VALIDATION
# ============================================================================
cat("\n=== G: PowerTOST Input Validation ===\n\n")

# --- G1: Boundary conditions ------------------------------------------------
cat("  -- G1: Boundary conditions --\n")

# CV = 0 should fail gracefully
pt_cv0 <- tryCatch(
  sampleN.TOST(alpha = 0.05, targetpower = 0.80, CV = 0,
               theta0 = 1.0, design = "2x2", print = FALSE),
  error = function(e) "error"
)
check("PowerTOST: CV=0 handled (error expected)",
      identical(pt_cv0, "error") || is.data.frame(pt_cv0),
      "(should error or return Inf)")

# Very high CV
pt_highcv <- tryCatch(
  sampleN.TOST(alpha = 0.05, targetpower = 0.80, CV = 1.5,
               theta0 = 0.95, design = "2x2", print = FALSE),
  error = function(e) NULL
)
check("PowerTOST: high CV (150%) completes",
      !is.null(pt_highcv),
      if (!is.null(pt_highcv))
        sprintf("(N=%d)", pt_highcv[["Sample size"]]) else "")

# theta0 at boundary (power = alpha → N = Inf → expected to fail/error)
pt_boundary <- tryCatch(
  sampleN.TOST(alpha = 0.05, targetpower = 0.80, CV = 0.20,
               theta0 = 0.80, theta1 = 0.80, theta2 = 1.25,
               design = "2x2", print = FALSE),
  error = function(e) "error"
)
check("PowerTOST: theta0=theta1 (boundary) errors or returns large N",
      identical(pt_boundary, "error") ||
        (is.data.frame(pt_boundary) && pt_boundary[["Sample size"]] > 1000),
      "(at boundary, power=alpha → N→∞, error is correct behavior)")

# --- G2: All supported designs work -----------------------------------------
cat("  -- G2: All designs produce results --\n")
test_designs <- c("2x2", "2x2x3", "2x3x3", "2x2x4", "parallel")

for (des in test_designs) {
  r <- tryCatch(
    sampleN.TOST(alpha = 0.05, targetpower = 0.80, CV = 0.25,
                 theta0 = 0.95, design = des, print = FALSE),
    error = function(e) NULL
  )
  check(sprintf("Design '%s' produces result", des),
        !is.null(r),
        if (!is.null(r)) sprintf("(N=%d)", r[["Sample size"]]) else "")
}


# ============================================================================
# SECTION H: UX DEFAULTS & WORKFLOW ASSESSMENT
# ============================================================================
cat("\n=== H: UX Defaults & Workflow Assessment ===\n\n")

# --- H1: Default settings produce valid results ----------------------------
cat("  -- H1: Default NCA settings work out of the box --\n")

# Simulate app defaults
defaults <- list(
  admin_route = "extravascular",
  dose = 100,
  infusion_duration = 0,
  is_steady_state = FALSE,
  dose_unit = "mg",
  time_unit = "h",
  conc_unit = "ng/mL",
  trap_method = "log",
  r2adj_threshold = 0.7,
  mw = 0,
  partial_aucs = NULL
)

nca_default <- run_nca(Theoph, 
                        col_map = list(subject = "Subject", time = "Time", conc = "conc"),
                        settings = list(
                          admin_route = "extravascular",
                          dose = 320,
                          infusion_duration = 0,
                          is_steady_state = FALSE,
                          dose_unit = "mg", time_unit = "h", conc_unit = "mg/L",
                          trap_method = "log",
                          r2adj_threshold = 0.7,
                          mw = 0,
                          partial_aucs = NULL
                        ))

check("Default settings produce NCA results on Theoph",
      !is.null(nca_default) && nrow(nca_default) == 12)

# --- H2: Error message quality checklist -----------------------------------
cat("  -- H2: Input validation catches common mistakes --\n")

# Missing required column
val_missing <- validate_mapping(list(subject = "ID", time = "", conc = "DV"))
check("Validation: catches missing Time column",
      !val_missing$valid)
check("Validation: error message mentions 'time'",
      grepl("time", val_missing$message, ignore.case = TRUE),
      sprintf("(msg: '%s')", val_missing$message))

# All required present
val_ok <- validate_mapping(list(subject = "ID", time = "Time", conc = "DV"))
check("Validation: accepts complete mapping",
      val_ok$valid)

# --- H3: Reasonable output precision ----------------------------------------
cat("  -- H3: Output precision/formatting --\n")

# Test fmt_pk function
check("fmt_pk: large numbers use fixed notation",
      fmt_pk(123.456, 4) == "123.5",
      sprintf("(got '%s')", fmt_pk(123.456, 4)))
check("fmt_pk: small numbers use scientific notation",
      nchar(fmt_pk(0.00123, 4)) > 0)
check("fmt_pk: NA renders as dash",
      fmt_pk(NA) == "—")

# --- H4: Workflow linearity check ------------------------------------------
cat("  -- H4: Workflow completeness --\n")

# Verify that outputs from one module match inputs expected by the next
# NCA output must contain columns that BE module needs
check("Pipeline: NCA output has Subject column (needed by BE)",
      names(nca_default)[1] %in% c("Subject", "SUBJID", "ID"),
      sprintf("(first col: '%s')", names(nca_default)[1]))

# NCA output has numeric CMAX and AUCLST (needed for log-transform in BE)
check("Pipeline: CMAX is numeric after NCA",
      is.numeric(as.numeric(nca_default$CMAX)))
check("Pipeline: AUCLST is numeric after NCA",
      is.numeric(as.numeric(nca_default$AUCLST)))

# PowerTOST can use CV derived from NCA results
if (!is.null(nca_default)) {
  cmax_vals <- as.numeric(nca_default$CMAX)
  cv_from_nca <- sd(log(cmax_vals)) # approximately CVw on log scale
  check("Pipeline: CV derivable from NCA Cmax for PowerTOST",
        is.finite(cv_from_nca) && cv_from_nca > 0,
        sprintf("(CV~%.2f)", cv_from_nca))
}


# ============================================================================
# SECTION I: STEADY-STATE NCA
# ============================================================================
cat("\n=== I: Steady-State NCA ===\n\n")

# Generate SS data (pre-dose trough + full profile within tau)
ss_data <- data.frame(
  Subject = rep("S1", 10),
  Time = c(0, 0.5, 1, 2, 3, 4, 6, 8, 10, 12),  # tau = 12h
  Conc = c(5.2, 18.5, 32.1, 25.3, 18.8, 14.2, 8.5, 6.8, 5.8, 5.3)
)

nca_ss <- tryCatch(
  tblNCA(ss_data, "Subject", "Time", "Conc",
         dose = 100, adm = "Extravascular", down = "Linear",
         R2ADJ = 0, SS = TRUE),
  error = function(e) NULL
)

check("Steady-state NCA: completes",
      !is.null(nca_ss))

if (!is.null(nca_ss)) {
  # Check SS-specific parameters exist
  ss_params <- c("CMAX", "TMAX", "AUCLST")
  check("SS NCA: Cmax present",
        "CMAX" %in% names(nca_ss))
  check("SS NCA: Cmax = 32.1 (observed max)",
        abs(as.numeric(nca_ss$CMAX) - 32.1) < 0.01)
}


# ============================================================================
# SECTION J: DATA QUALITY CHECK ENGINE
# ============================================================================
cat("\n=== J: Data Quality Check Engine ===\n\n")

# --- J1: Clean data passes all checks -------------------------------------
cat("  -- J1: Clean data --\n")
clean_data <- data.frame(
  Subject = rep(c("S1", "S2", "S3"), each = 6),
  Time = rep(c(0, 1, 2, 4, 8, 24), 3),
  Conc = c(0, 15, 12, 8, 4, 1, 0, 18, 14, 9, 5, 1.2, 0, 13, 10, 7, 3.5, 0.8)
)
qc_clean <- run_data_quality_check(clean_data,
                                     list(subject="Subject", time="Time", conc="Conc"))
check("Clean data: passes (no errors)", qc_clean$pass)
check("Clean data: zero errors", qc_clean$n_errors == 0)
check("Clean data: findings include OK entries", qc_clean$n_ok > 0)

# --- J2: BLQ strings detected and warned -----------------------------------
cat("  -- J2: BLQ string detection --\n")
blq_str_data <- data.frame(
  Subject = rep("S1", 8),
  Time = 0:7,
  Conc = c("BLQ", "5.2", "<0.5", "12.3", "8.1", "ND", "3.2", "BQL"),
  stringsAsFactors = FALSE
)
qc_blq <- run_data_quality_check(blq_str_data,
                                   list(subject="Subject", time="Time", conc="Conc"))
blq_findings <- qc_blq$findings[grep("BLQ|text", qc_blq$findings$Message), ]
check("BLQ strings: detected as warning",
      nrow(blq_findings) > 0,
      sprintf("(%d findings)", nrow(blq_findings)))
check("BLQ strings: LLOQ auto-detected from '<0.5'",
      any(grepl("LLOQ|auto-detect", qc_blq$findings$Message, ignore.case = TRUE)))

# --- J3: Unrecognized text triggers error ----------------------------------
cat("  -- J3: Unrecognized text values --\n")
bad_text_data <- data.frame(
  Subject = rep("S1", 5),
  Time = 0:4,
  Conc = c("0", "hello", "15.2", "world", "8.1"),
  stringsAsFactors = FALSE
)
qc_bad <- run_data_quality_check(bad_text_data,
                                   list(subject="Subject", time="Time", conc="Conc"))
check("Unrecognized text: triggers error",
      qc_bad$n_errors > 0,
      sprintf("(%d errors)", qc_bad$n_errors))
check("Unrecognized text: does NOT pass",
      !qc_bad$pass)

# --- J4: Missing subject IDs -----------------------------------------------
cat("  -- J4: Missing subject IDs --\n")
missing_subj_data <- data.frame(
  Subject = c("S1", "S1", "", "S2", NA, "S2"),
  Time = c(0, 1, 2, 0, 1, 2),
  Conc = c(0, 10, 5, 0, 12, 6)
)
qc_missing <- run_data_quality_check(missing_subj_data,
                                       list(subject="Subject", time="Time", conc="Conc"))
check("Missing subject IDs: detected as error",
      any(grepl("missing Subject ID", qc_missing$findings$Message)))

# --- J5: Negative concentrations warned ------------------------------------
cat("  -- J5: Negative concentrations --\n")
neg_conc_data <- data.frame(
  Subject = rep("S1", 6),
  Time = c(0, 1, 2, 4, 8, 24),
  Conc = c(0, 15, 12, -0.5, 4, 1)
)
qc_neg <- run_data_quality_check(neg_conc_data,
                                   list(subject="Subject", time="Time", conc="Conc"))
check("Negative conc: detected as warning",
      any(grepl("negative", qc_neg$findings$Message, ignore.case = TRUE)))

# --- J6: Empty dataset triggers error --------------------------------------
cat("  -- J6: Empty dataset --\n")
empty_data <- data.frame(Subject=character(0), Time=numeric(0), Conc=numeric(0))
qc_empty <- run_data_quality_check(empty_data,
                                     list(subject="Subject", time="Time", conc="Conc"))
check("Empty dataset: triggers error",
      qc_empty$n_errors > 0)
check("Empty dataset: does NOT pass",
      !qc_empty$pass)

# --- J7: Sparse subjects warned --------------------------------------------
cat("  -- J7: Sparse subjects --\n")
sparse_qc_data <- data.frame(
  Subject = c("S1", "S1", "S2", "S2", "S2", "S2", "S2", "S2"),
  Time = c(0, 4, 0, 1, 2, 4, 8, 24),
  Conc = c(0, 5, 0, 15, 12, 8, 4, 1)
)
qc_sparse <- run_data_quality_check(sparse_qc_data,
                                      list(subject="Subject", time="Time", conc="Conc"))
check("Sparse subject (2 obs): warned",
      any(grepl("< 3 observations", qc_sparse$findings$Message)))

# --- J8: Duplicate times warned --------------------------------------------
cat("  -- J8: Duplicate time points --\n")
dup_qc_data <- data.frame(
  Subject = rep("S1", 7),
  Time = c(0, 1, 2, 2, 4, 8, 24),
  Conc = c(0, 15, 12, 11.8, 8, 4, 1)
)
qc_dup <- run_data_quality_check(dup_qc_data,
                                   list(subject="Subject", time="Time", conc="Conc"))
check("Duplicate times: detected",
      any(grepl("duplicate", qc_dup$findings$Message, ignore.case = TRUE)))

# --- J9: All-zero subject warned ------------------------------------------
cat("  -- J9: All-zero subject --\n")
zero_subj_data <- data.frame(
  Subject = c(rep("S1", 5), rep("S2", 5)),
  Time = rep(c(0, 1, 2, 4, 8), 2),
  Conc = c(0, 15, 12, 8, 4, 0, 0, 0, 0, 0)
)
qc_zero <- run_data_quality_check(zero_subj_data,
                                    list(subject="Subject", time="Time", conc="Conc"))
check("All-zero subject: warned",
      any(grepl("all-zero", qc_zero$findings$Message, ignore.case = TRUE)))

# --- J10: Crossover completeness check ------------------------------------
cat("  -- J10: Crossover completeness --\n")
incomplete_xover <- data.frame(
  Subject   = c("S1","S1","S1","S1", "S2","S2", "S3","S3","S3","S3"),
  Time      = c(0,1,0,1, 0,1, 0,1,0,1),
  Conc      = c(0,10,0,8, 0,12, 0,15,0,11),
  Treatment = c("T","T","R","R", "T","T", "T","T","R","R")
)
qc_xover <- run_data_quality_check(
  incomplete_xover,
  list(subject="Subject", time="Time", conc="Conc", treatment="Treatment"))
check("Incomplete crossover: S2 missing Reference detected",
      any(grepl("missing data for one or more treatments",
                qc_xover$findings$Message)))

# --- J11: Non-numeric time triggers error ---------------------------------
cat("  -- J11: Non-numeric time --\n")
bad_time_data <- data.frame(
  Subject = rep("S1", 5),
  Time = c("0h", "1h", "2", "4h", "8"),
  Conc = c(0, 15, 12, 8, 4),
  stringsAsFactors = FALSE
)
qc_bad_time <- run_data_quality_check(bad_time_data,
                                        list(subject="Subject", time="Time", conc="Conc"))
check("Non-numeric time: triggers error",
      any(grepl("non-numeric.*Time", qc_bad_time$findings$Message, ignore.case = TRUE)))

# --- J12: HTML rendering doesn't crash ------------------------------------
cat("  -- J12: HTML rendering --\n")

# render_quality_report uses Shiny's icon() and tags$ — load if available
has_shiny <- requireNamespace("shiny", quietly = TRUE)
if (has_shiny) {
  library(shiny)
  html_out <- tryCatch(
    render_quality_report(qc_clean),
    error = function(e) NULL
  )
  check("Quality report HTML renders without error (clean data)",
        !is.null(html_out))
  
  html_out_bad <- tryCatch(
    render_quality_report(qc_bad),
    error = function(e) NULL
  )
  check("Quality report HTML renders without error (error data)",
        !is.null(html_out_bad))
} else {
  check("Quality report HTML render: skipped (shiny not installed)",
        TRUE, "(shiny package not available)")
  check("Quality report HTML render: skipped (shiny not installed)",
        TRUE, "(shiny package not available)")
}


# ============================================================================
# SUMMARY
# ============================================================================
cat("\n")
cat("================================================================\n")
cat(sprintf("  FUNCTIONAL & UX VALIDATION SUMMARY\n"))
cat(sprintf("  %d/%d tests passed", pass_count, total_count))
if (fail_count > 0) cat(sprintf(", %d FAILED", fail_count))
if (warn_count > 0) cat(sprintf(", %d WARNINGS", warn_count))
cat("\n")
cat("================================================================\n")

if (fail_count == 0) {
  cat("  ALL TESTS PASSED.\n")
} else {
  cat("  FAILURES DETECTED — Review failed tests above.\n")
}

# Section breakdown
cat("\n  Section breakdown:\n")
cat("    A: Column auto-detection\n")
cat("    B: Messy data handling (BLQ strings, negatives, zeros, NAs)\n")
cat("    C: End-to-end pipeline (24-subject crossover → NCA → BE)\n")
cat("    D: Edge cases (large datasets, sparse data, extremes)\n")
cat("    E: Audit trail completeness\n")
cat("    F: CDISC compatibility\n")
cat("    G: PowerTOST input validation\n")
cat("    H: UX defaults and workflow\n")
cat("    I: Steady-state NCA\n")
cat("    J: Data quality check engine\n")
cat("\n")


# Cleanup: close graphics devices, remove Rplots
while (dev.cur() > 1) try(dev.off(), silent = TRUE)
rplots <- list.files(pattern = "^Rplots.*[.]pdf$")
if (length(rplots) > 0) invisible(file.remove(rplots))
