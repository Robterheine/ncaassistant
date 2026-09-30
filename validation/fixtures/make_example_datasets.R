# Generates the two bundled example datasets added in v1.8.0. Synthetic data,
# seeded; run from the project root:
#   Rscript validation/fixtures/make_example_datasets.R
#
# example_be_replicate_hvd.csv  2x2x4 full replicate, 32 subjects. The Reference
#   is highly variable for Cmax (within-subject SD about 0.40, driven by the
#   absorption rate) and moderately variable for AUC (about 0.25, driven by
#   clearance), so one run shows both routes of FDA RSABE: scaled for Cmax,
#   standard for AUC. The Test Cmax is about 13% lower: the standard 80-125%
#   test misses on Cmax, while ABEL and RSABE conclude bioequivalence.
# example_be_parallel_covariates.csv  parallel groups, 20 per group, with the
#   baseline columns Age, Weight and Sex. Weight drives volume and clearance, so
#   adjusting for it narrows the interval.
#
# The seeds were chosen by searching for data that show what the tutorials teach
# (see the criteria in each block); the search itself is in the last lines.

times <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 12, 24, 36, 48)
conc_1cpt <- function(dose, ka, ke, v, t) dose * ka / (v * (ka - ke)) * (exp(-ke * t) - exp(-ka * t))
auc_lin <- function(t, c) sum(diff(t) * (head(c, -1) + tail(c, -1)) / 2)

# ---- replicate, highly variable Cmax ----------------------------------------
sim_hvd <- function(seed, n = 32) {
  set.seed(seed)
  seqs <- rep(c("TRTR", "RTRT"), each = n / 2)
  rows <- list(); adm <- list()
  for (i in seq_len(n)) {
    b_ka <- rnorm(1, 0, 0.30); b_cl <- rnorm(1, 0, 0.25)          # between-subject
    for (p in 1:4) {
      trt <- strsplit(seqs[i], "")[[1]][p]
      ka <- 1.1 * exp(b_ka + rnorm(1, 0, 0.30))
      v <- 55 * exp(rnorm(1, 0, 0.50)) * (if (trt == "T") 1.15 else 1)   # drives Cmax, not AUC; Test Cmax about 13% lower
      cl <- 6 * exp(b_cl + rnorm(1, 0, 0.30)) * (if (trt == "T") 1 / 1.04 else 1)  # drives AUC
      ke <- cl / v
      cc <- conc_1cpt(100, ka, ke, v, times) * exp(rnorm(length(times), 0, 0.04))
      cc[1] <- 0
      rows[[length(rows) + 1]] <- data.frame(Subject = i, Sequence = seqs[i], Period = p,
        Treatment = ifelse(trt == "T", "Test", "Reference"), Time = times, Conc = round(cc, 4), Dose = 100)
      adm[[length(adm) + 1]] <- data.frame(Subject = i, Period = p, trt = trt, cmax = max(cc), auc = auc_lin(times, cc))
    }
  }
  list(data = do.call(rbind, rows), adm = do.call(rbind, adm))
}
swr <- function(adm, col) {   # within-subject SD of the Reference from the two R administrations (FDA contrast D)
  r <- adm[adm$trt == "R", ]; r <- r[order(r$Subject, r$Period), ]
  d <- tapply(log(r[[col]]), r$Subject, function(v) v[1] - v[2])
  sqrt(sum((d - ave(d, ifelse(as.integer(names(d)) <= 16, "a", "b")))^2) / (length(d) - 2) / 2)
}
find_hvd <- function() {
  for (s in 1:2000) {
    z <- sim_hvd(s); sc <- swr(z$adm, "cmax"); sa <- swr(z$adm, "auc")
    gm <- function(col) exp(mean(log(z$adm[[col]][z$adm$trt == "T"])) - mean(log(z$adm[[col]][z$adm$trt == "R"])))
    if (abs(sc - 0.40) < 0.015 && abs(sa - 0.25) < 0.015 && gm("cmax") > 0.84 && gm("cmax") < 0.90 && gm("auc") > 0.97 && gm("auc") < 1.10) {
      # the standard 80-125% test (EMA Method A model) should just miss for Cmax
      m <- transform(z$adm, lc = log(cmax), Subject = factor(Subject), Period = factor(Period))
      m$Seq <- factor(ifelse(as.integer(m$Subject) <= 16, "TRTR", "RTRT"))
      ci <- 100 * exp(confint(lm(lc ~ Seq + Subject + Period + trt, m), "trtT", level = 0.90))
      if (ci[1] > 77.5 && ci[1] < 79.5) return(s)
    }
  }
  stop("no seed found")
}

# ---- parallel groups with covariates ----------------------------------------
sim_par <- function(seed, n_per = 20) {
  set.seed(seed)
  n <- 2 * n_per
  d <- data.frame(Subject = seq_len(n), Treatment = rep(c("Reference", "Test"), each = n_per),
                  Age = round(rnorm(n, 41, 10)), Weight = round(rnorm(n, 72, 11), 1),
                  Sex = sample(c("F", "M"), n, TRUE), stringsAsFactors = FALSE)
  rows <- list(); adm <- list()
  for (i in seq_len(n)) {
    w <- d$Weight[i]
    v <- 55 * (w / 72) * exp(rnorm(1, 0, 0.07)); cl <- 6 * (w / 72)^0.75 * (if (d$Sex[i] == "F") 0.94 else 1) *
      exp(rnorm(1, 0, 0.10)) / (if (d$Treatment[i] == "Test") 1.03 else 1)
    ka <- 1.2 * exp(rnorm(1, 0, 0.15)); ke <- cl / v
    cc <- conc_1cpt(100, ka, ke, v, times) * exp(rnorm(length(times), 0, 0.04)); cc[1] <- 0
    rows[[i]] <- data.frame(Subject = i, Treatment = d$Treatment[i], Time = times, Conc = round(cc, 4), Dose = 100,
                            Age = d$Age[i], Weight = d$Weight[i], Sex = d$Sex[i])
    adm[[i]] <- data.frame(Subject = i, trt = d$Treatment[i], cmax = max(cc), auc = auc_lin(times, cc), Weight = w, Age = d$Age[i], Sex = d$Sex[i])
  }
  list(data = do.call(rbind, rows), adm = do.call(rbind, adm))
}
find_par <- function() {
  for (s in 1:3000) {
    z <- sim_par(s); a <- z$adm; a$lc <- log(a$cmax); a$tt <- a$trt == "Test"
    fu <- lm(lc ~ tt, a); fa <- lm(lc ~ tt + Weight + Age + Sex, a)
    ci <- function(f) 100 * exp(confint(f, "ttTRUE", level = 0.90))
    # Test group heavier by chance, so the unadjusted ratio is pulled down and its interval fails the lower limit,
    # while the adjusted interval sits inside 80-125
    if (ci(fu)[1] < 79 && ci(fa)[1] > 86 && ci(fa)[2] < 118 && diff(tapply(a$Weight, a$trt, mean)) > 4)
      return(s)
  }
  stop("no seed found")
}

if (sys.nframe() == 0) {
  s1 <- find_hvd(); s2 <- find_par()
  cat("seed replicate:", s1, " seed parallel:", s2, "\n")
  write.csv(sim_hvd(s1)$data, file.path("data", "example_be_replicate_hvd.csv"), row.names = FALSE)
  write.csv(sim_par(s2)$data, file.path("data", "example_be_parallel_covariates.csv"), row.names = FALSE)
}
