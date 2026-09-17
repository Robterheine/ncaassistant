# ============================================================================
# NCA Assistant — validation fixtures for crossover and replicate BE designs
# ============================================================================
# Deterministic generator. Run from the repository root:
#   Rscript validation/fixtures/make_fixtures.R
# The generated CSVs are committed as well, because tests that hash or
# reproduce data need stable bytes. These files are test data only: they live
# here, not in data/, so they never appear among the user-facing examples.
#
# Model: one-compartment oral absorption, ka = 1.2 /h, ke = 0.15 /h.
# Each administration gets its own bioavailability factor
#   F = exp(subject effect + within-subject noise) * (ratio if Test)
# so every administration has a distinct profile and a per-period Cmax that
# can be checked directly against the data. Nominal times repeat in every
# period (pre-dose at t = 0), which is the layout of most real files.
# Concentrations below 0.5 are reported as 0.4 so that BLQ rules have work to
# do when LLOQ = 0.5 is applied.

make_design <- function(sequences, n_per_seq, ratio = 0.95, cv_w = 0.25,
                        seed = 20260917) {
  set.seed(seed)
  times <- c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24, 36)
  ka <- 1.2; ke <- 0.15; dose <- 100
  sw <- sqrt(log(cv_w^2 + 1))
  rows <- list(); sid <- 0
  for (sq in sequences) for (j in seq_len(n_per_seq)) {
    sid <- sid + 1
    subj_eff <- rnorm(1, 0, 0.3)
    trts <- strsplit(sq, "")[[1]]
    for (p in seq_along(trts)) {
      f <- exp(subj_eff + rnorm(1, 0, sw)) * if (trts[p] == "T") ratio else 1
      conc <- dose * f * ka / (ka - ke) * (exp(-ke * times) - exp(-ka * times)) / 10
      conc <- signif(conc, 5)
      conc[times > 0 & conc < 0.5] <- 0.4
      rows[[length(rows) + 1]] <- data.frame(
        Subject = sid, Sequence = sq, Period = p,
        Treatment = if (trts[p] == "T") "Test" else "Reference",
        Time = times, Conc = conc, Dose = dose, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

out_dir <- file.path("validation", "fixtures")
fixtures <- list(
  "be_2x2x2_crossover.csv"      = make_design(c("TR", "RT"),         6, seed = 1),
  "be_2x2x4_full_replicate.csv" = make_design(c("TRTR", "RTRT"),     6, seed = 2),
  "be_2x2x3_full_replicate.csv" = make_design(c("TRT", "RTR"),       6, seed = 3),
  "be_2x3x3_partial_replicate.csv" = make_design(c("TRR", "RTR", "RRT"), 4, seed = 4),
  # Within-subject CV around 45%, so reference scaling would widen the limits
  "be_2x2x4_highly_variable.csv" = make_design(c("TRTR", "RTRT"), 12, cv_w = 0.45, seed = 5)
)
for (nm in names(fixtures)) {
  write.csv(fixtures[[nm]], file.path(out_dir, nm), row.names = FALSE)
  cat("wrote", nm, nrow(fixtures[[nm]]), "rows\n")
}
