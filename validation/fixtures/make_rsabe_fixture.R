# Per-administration Cmax of replicateBE reference data sets, as input for the
# RSABE and ABEL validation (sections RSA and ABL). Run from the project root:
#   Rscript validation/fixtures/make_rsabe_fixture.R
# Partial replicates (2x3x3) and full replicates (2x2x4 and variants) with and
# without missing periods. replicateBE is a validation-only dependency.
sets <- c(paste0("rds", c("02", "04", "07", "30")),                          # TRR|RTR|RRT
          paste0("rds", c("01", "05", "06", "08", "09", "11", "23", "24", "29")))   # two T and two R per subject
out <- do.call(rbind, lapply(sets, function(nm) {
  d <- getExportedValue("replicateBE", nm)
  data.frame(dataset = nm, subject = d$subject, period = d$period, sequence = d$sequence,
             treatment = d$treatment, pk = d$PK, stringsAsFactors = FALSE)
}))
write.csv(out, file.path("validation", "fixtures", "rsabe_datasets.csv"), row.names = FALSE)
cat("replicateBE", as.character(packageVersion("replicateBE")), "\n")
