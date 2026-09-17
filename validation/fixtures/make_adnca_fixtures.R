# ============================================================================
# NCA Assistant — ADNCA-shaped validation fixtures (roadmap §5, F1–F9)
# ============================================================================
# Deterministic generator. Run from the repository root:
#   Rscript validation/fixtures/make_adnca_fixtures.R
# The CSVs are committed. They are test data only (never in data/).
#
# Shape: an analysis-ready PK dataset with CDISC ADaM ADNCA-style variable
# names (USUBJID, PARAMCD, AVAL, AVALU, ARRLT, NRRLT, AFRLT, ANL01FL, DTYPE,
# ...). Variable names follow common ADNCA usage; they are not validated
# against the Implementation Guide and must not be cited as conformant.
#
# Study: 12 subjects, 2x2 crossover (TR | RT), one analyte, oral 100 mg,
# one-compartment profiles (ka 1.2 /h, ke 0.15 /h), 7 days washout.

set.seed(20260917)
nominal <- c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24)
lloq <- 0.5

base_rows <- list()
for (s in 1:12) {
  seq <- if (s %% 2 == 1) "TR" else "RT"
  subj_eff <- rnorm(1, 0, 0.25)
  for (per in 1:2) {
    trt <- if (substr(seq, per, per) == "T") "Test" else "Reference"
    f <- exp(subj_eff + rnorm(1, 0, 0.15)) * if (trt == "Test") 0.95 else 1
    # actual times: pre-dose slightly before the dose, later samples slightly late
    actual <- nominal + ifelse(nominal == 0, -round(runif(1, 0.05, 0.25), 2),
                               round(runif(length(nominal), 0, 0.08), 2))
    tt <- pmax(actual, 0)
    conc <- signif(100 * f * 1.2 / (1.2 - 0.15) * (exp(-0.15 * tt) - exp(-1.2 * tt)) / 10, 5)
    base_rows[[length(base_rows) + 1]] <- data.frame(
      STUDYID = "NCAA-001", USUBJID = sprintf("NCAA-001-%03d", s), SUBJID = s,
      PARAMCD = "DRUGX", PARAM = "Drug X concentration", PCSPEC = "PLASMA",
      AVAL = conc, AVALU = "ng/mL", PCLLOQ = lloq,
      NRRLT = nominal, ARRLT = actual, AFRLT = actual + (per - 1) * 168, RRLTU = "h",
      TRTP = trt, APERIOD = per, TRTSEQP = seq,
      DOSEA = 100, DOSEU = "mg", ANL01FL = "Y", DTYPE = "",
      stringsAsFactors = FALSE)
  }
}
f1 <- do.call(rbind, base_rows)

out <- function(d, name) {
  write.csv(d, file.path("validation", "fixtures", name), row.names = FALSE, na = "")
  cat("wrote", name, nrow(d), "rows\n")
}

# F1: clean, one analyte, all records in the analysis set, no derived records
out(f1, "adnca_clean.csv")

# F2: the same study as a flat file, generated FROM F1 so identity holds
f2 <- data.frame(Subject = f1$SUBJID, Treatment = f1$TRTP, Period = f1$APERIOD,
                 Sequence = f1$TRTSEQP, Time = f1$NRRLT, Conc = f1$AVAL, Dose = f1$DOSEA)
out(f2, "flat_equivalent.csv")

# F3: BLQ already imputed upstream (DTYPE = HALFLLOQ)
f3 <- f1
blq <- f3$AVAL < lloq
f3$AVAL[blq] <- lloq / 2
f3$DTYPE[blq] <- "HALFLLOQ"
out(f3, "adnca_dtype.csv")

# F4: records outside the analysis set, with absurd values that change Cmax
f4 <- f1
extra <- f1[f1$NRRLT == 2, ]
extra$AVAL <- 1e6
extra$ANL01FL <- ""
out(rbind(f4, extra), "adnca_anl01fl.csv")

# F5: parent and metabolite stacked
met <- f1
met$PARAMCD <- "DRUGXM1"; met$PARAM <- "Drug X metabolite M1 concentration"
met$AVAL <- signif(met$AVAL * 0.3, 5)
out(rbind(f1, met), "adnca_multi_analyte.csv")

# F6: time since FIRST dose used as the time variable (period 2 starts at ~168 h)
f6 <- f1
f6$ARRLT <- f6$AFRLT
out(f6, "adnca_afrlt.csv")

# F7: only datetimes, no relative time variables
f7 <- f1[, setdiff(names(f1), c("NRRLT", "ARRLT", "AFRLT", "RRLTU"))]
dose_dt <- as.POSIXct("2026-03-02 08:00:00", tz = "UTC") + (f1$APERIOD - 1) * 7 * 86400
f7$EXSTDTC <- format(dose_dt, "%Y-%m-%dT%H:%M:%S")
f7$PCDTC <- format(dose_dt + f1$ARRLT * 3600, "%Y-%m-%dT%H:%M:%S")
out(f7, "adnca_datetime.csv")

# F8: two concentration units that are numerically equivalent (ng/mL = ug/L)
f8 <- f1
f8$AVALU[f8$SUBJID > 6] <- "ug/L"
out(f8, "adnca_units_mixed.csv")

# F9: two different doses recorded within one subject and period
f9 <- f1
f9$DOSEA[f9$SUBJID == 3 & f9$APERIOD == 1 & f9$NRRLT >= 12] <- 50
out(f9, "adnca_multi_ex.csv")
