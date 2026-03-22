#!/usr/bin/env Rscript
# ============================================================================
# NCA Assistant — Setup & Launch Script
# ============================================================================
# Run this script once to install all dependencies, then launch the app.
# Usage: Rscript install_and_run.R
#    or: source("install_and_run.R") from within R/RStudio

cat("
======================================================
  Non-Compartmental Analysis Assistant v0.1
  Radboud Applied Pharmacometrics — Radboudumc
======================================================
\n")

# --- Check R version --------------------------------------------------------
if (getRversion() < "4.1.0") {
  stop("NCA Assistant requires R >= 4.1.0. You have ", getRversion())
}

# --- Required packages -------------------------------------------------------
required_packages <- c(
  # Core Shiny
  "shiny", "bslib", "htmltools", "sass",
  # UI components
  "shinyWidgets", "DT", "plotly",
  # Data handling
  "readxl", "openxlsx", "dplyr", "tidyr",
  # Plotting
  "ggplot2", "scales",
  # PK analysis engines
  "NonCompart", "ncar", "PowerTOST",
  # Mixed effects for BE
  "nlme"
)

# --- Install missing packages -----------------------------------------------
cat("Checking package dependencies...\n")

missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  cat("Installing", length(missing), "missing packages:\n")
  cat("  ", paste(missing, collapse = ", "), "\n\n")
  
  install.packages(missing, repos = "https://cloud.r-project.org",
                   dependencies = TRUE, quiet = FALSE)
  
  # Verify installation
  still_missing <- missing[!missing %in% installed.packages()[, "Package"]]
  if (length(still_missing) > 0) {
    stop("Failed to install: ", paste(still_missing, collapse = ", "),
         "\nTry installing them manually.")
  }
  cat("\nAll packages installed successfully.\n\n")
} else {
  cat("All", length(required_packages), "packages already installed.\n\n")
}

# --- Print version info ------------------------------------------------------
cat("--- Engine Versions ---\n")
cat("R:          ", R.version.string, "\n")
cat("NonCompart: ", as.character(packageVersion("NonCompart")), "\n")
cat("ncar:       ", as.character(packageVersion("ncar")), "\n")
cat("PowerTOST:  ", as.character(packageVersion("PowerTOST")), "\n")
cat("Shiny:      ", as.character(packageVersion("shiny")), "\n")
cat("bslib:      ", as.character(packageVersion("bslib")), "\n")
cat("\n")

# --- Launch ------------------------------------------------------------------
cat("Launching NCA Assistant...\n")
cat("The app will open in your default browser.\n")
cat("Press Ctrl+C (or Esc in RStudio) to stop the server.\n\n")

# Determine app directory (same directory as this script)
app_dir <- if (interactive()) {
  # If sourced from RStudio, use the script's location
  tryCatch(
    dirname(rstudioapi::getSourceEditorContext()$path),
    error = function(e) getwd()
  )
} else {
  # If run via Rscript, use the script's location
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    getwd()
  }
}

shiny::runApp(app_dir, launch.browser = TRUE)
