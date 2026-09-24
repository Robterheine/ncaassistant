# ============================================================================
# NCA Assistant - release files for installation qualification
# ============================================================================
# Run from the project root when a release is tagged, after the validation
# run that goes into the IQ/OQ/PQ protocol:
#   Rscript validation/make_release_files.R
# Writes
#   validation/renv.lock           the package versions the release was validated
#                                  with; renv::restore(lockfile = "validation/renv.lock")
#                                  rebuilds that library. It is kept out of the
#                                  project root, where rsconnect would use it
#                                  when deploying.
#   validation/release_manifest.csv  SHA-256 of every file the app runs on,
#                                  with the app version; the IQ section of
#                                  validation.R compares an installation with it
# ============================================================================

if (!file.exists("app.R") || !dir.exists("R")) stop("Run from the project root.")
app_version <- sub('^APP_VERSION <- "([^"]+)".*$', "\\1", grep("^APP_VERSION", readLines("app.R"), value = TRUE)[1])

pkgs <- c("NonCompart", "PowerTOST", "nlme", "digest", "openxlsx", "jsonlite", "readxl", "dplyr",
          "shiny", "bslib", "shinyWidgets", "DT", "plotly", "ggplot2", "htmltools", "tidyr", "replicateBE")
lock <- renv::lockfile_create(type = "custom", packages = pkgs)
renv::lockfile_write(lock, file = "validation/renv.lock")

files <- c("app.R", sort(list.files("R", "\\.R$", full.names = TRUE)),
           sort(list.files("converters", "\\.R$", full.names = TRUE)),
           sort(list.files("cdisc", full.names = TRUE)), sort(list.files("www", full.names = TRUE)))
manifest <- data.frame(app_version = app_version, file = files,
                       sha256 = vapply(files, function(f) digest::digest(file = f, algo = "sha256"), character(1)),
                       stringsAsFactors = FALSE)
write.csv(manifest, "validation/release_manifest.csv", row.names = FALSE)
cat("Wrote validation/renv.lock and validation/release_manifest.csv for NCA Assistant", app_version, "\n")
