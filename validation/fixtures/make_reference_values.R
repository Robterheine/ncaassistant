# ============================================================================
# NCA Assistant — reference values from replicateBE for the replicate fixtures
# ============================================================================
# Run from the repository root (requires the replicateBE package):
#   Rscript validation/fixtures/make_reference_values.R
# Writes validation/fixtures/replicateBE_reference.csv, which is committed so
# the validation suite does not need replicateBE installed.
#
# PK input for replicateBE is each administration's observed Cmax, taken
# straight from the fixture (max concentration per subject x period), so the
# reference does not depend on the app's NCA code.

if (!requireNamespace("replicateBE", quietly = TRUE))
  stop("replicateBE is required to regenerate the reference values.")

fixture_dir <- file.path("validation", "fixtures")
work <- file.path(tempdir(), "replicateBE_ref")
dir.create(work, showWarnings = FALSE, recursive = TRUE)

fixtures <- c("be_2x2x4_full_replicate", "be_2x2x3_full_replicate",
              "be_2x3x3_partial_replicate", "be_2x2x4_highly_variable")

rows <- lapply(fixtures, function(f) {
  d <- read.csv(file.path(fixture_dir, paste0(f, ".csv")), stringsAsFactors = FALSE)
  pk <- aggregate(Conc ~ Subject + Period + Sequence + Treatment, data = d, FUN = max)
  x <- data.frame(subject = pk$Subject, period = pk$Period, sequence = pk$Sequence,
                  treatment = ifelse(pk$Treatment == "Test", "T", "R"), PK = pk$Conc)
  x <- x[order(x$subject, x$period), ]
  write.csv(x, file.path(work, paste0(f, ".csv")), row.names = FALSE, quote = FALSE)
  m <- replicateBE::method.A(path.in = work, path.out = work, file = f, set = "",
                             ext = "csv", print = FALSE, details = TRUE,
                             verbose = FALSE, plot.bxp = FALSE)
  num <- function(col) if (col %in% names(m)) as.numeric(m[1, col]) else NA_real_
  data.frame(fixture = f, design = as.character(m$Design[1]), n = num("n"),
             DF = num("DF"), PE = num("PE(%)"), CL_lo = num("CL.lo(%)"),
             CL_hi = num("CL.hi(%)"), swR = num("swR"), CVwR = num("CVwR(%)"),
             swT = num("swT"), CVwT = num("CVwT(%)"), L = num("L(%)"), U = num("U(%)"),
             stringsAsFactors = FALSE)
})
ref <- do.call(rbind, rows)
ref$replicateBE_version <- as.character(packageVersion("replicateBE"))
write.csv(ref, file.path(fixture_dir, "replicateBE_reference.csv"), row.names = FALSE)
print(ref)
