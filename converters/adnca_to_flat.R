# ============================================================================
# adnca_to_flat.R — convert an ADNCA-shaped dataset to an NCA Assistant flat file
# ============================================================================
# Standalone converter. The conversion itself is in adnca_import.R, the same
# code the app uses for its "CDISC ADNCA dataset" upload, so a file converted
# here and a file imported in the app are converted identically.
#
# Keep adnca_import.R next to this script (or run it from the NCA Assistant
# repository, where it is in R/). Base R only; readxl for .xlsx input.
#
# Usage in R:
#   source("adnca_to_flat.R")
#   adnca_to_flat("adnca.csv", "flat.csv", time = "NRRLT")
#
# Usage from a terminal (exit status 2 when the dataset is refused):
#   Rscript adnca_to_flat.R adnca.csv flat.csv NRRLT [PARAMCD]
#
# See ADNCA_TO_FLAT.md for the choices, the refusals and how to load the
# result. This tool is not affiliated with, endorsed by or certified by CDISC.
# ============================================================================

local({
  script_dir <- tryCatch({
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(f) == 1) dirname(normalizePath(f)) else {
      of <- sys.frames()[[1]]$ofile
      if (!is.null(of)) dirname(normalizePath(of)) else getwd()
    }
  }, error = function(e) getwd())
  candidates <- unique(c(file.path(script_dir, "adnca_import.R"),
                         file.path(script_dir, "..", "R", "adnca_import.R"),
                         "adnca_import.R", file.path("R", "adnca_import.R"),
                         file.path("..", "R", "adnca_import.R")))
  core <- candidates[file.exists(candidates)][1]
  if (is.na(core)) stop("adnca_import.R not found. Place it next to adnca_to_flat.R.")
  sys.source(core, envir = parent.frame(2))
})

# Command line: Rscript adnca_to_flat.R input.csv output.csv NRRLT [PARAMCD]
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3) {
    cat("Usage: Rscript adnca_to_flat.R input.csv output.csv NRRLT|ARRLT|MRRLT [PARAMCD]\n")
    quit(status = 1)
  }
  res <- tryCatch(adnca_to_flat(args[1], args[2], time = args[3],
                                paramcd = if (length(args) >= 4) args[4] else NULL),
                  adnca_refusal = function(e) { message(conditionMessage(e)); quit(status = 2) })
}
