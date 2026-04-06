#!/usr/bin/env Rscript
# ============================================================================
# NCA Assistant — Package Risk Assessment using {riskmetric}
# ============================================================================
# Applies the R Validation Hub's risk-based framework to all contributed
# package dependencies of NCA Assistant.
#
# Reference: pharmaR White Paper — "A Risk-based Approach for Assessing
# R Package Accuracy within a Validated Infrastructure" (2020)
# https://pharmar.org/white-paper/
#
# Usage:
#   cd <project root>
#   Rscript validation/package_risk_assessment.R
#
# Output:
#   validation/package_risk_scores.csv
#   Console summary
# ============================================================================

if (!requireNamespace("riskmetric", quietly = TRUE)) {
  cat("Installing riskmetric...\n")
  install.packages("riskmetric", repos = "https://cloud.r-project.org")
}

library(riskmetric)

# ── Package classification ───────────────────────────────────────────────────
# Following pharmaR framework: "Intended for Use" vs. "Imports"
#
# NCA Assistant is the only "Intended for Use" application. Users interact
# with the Shiny app, never calling package functions directly. All packages
# below are therefore classified as IMPORTS — their accuracy is verified
# indirectly through the app's 239 automated tests and IQ/OQ/PQ protocol.
#
# nlme is an R Recommended Package (ships with R). Per the pharmaR white
# paper, the R Foundation qualifies as a Trusted Resource, making all
# base and recommended packages low risk without further assessment.

# Contributed packages used by NCA Assistant (non-base, non-recommended)
contributed_packages <- c(
  # PK computation engines
  "NonCompart",      # NCA engine — core computational dependency
  "ncar",            # NCA reporting companion to NonCompart
  "PowerTOST",       # Power and sample size for BE studies
  # Shiny framework
  "shiny",           # Web application framework
  "bslib",           # Bootstrap theming
  "htmltools",       # HTML generation
  "shinyWidgets",    # Enhanced UI widgets
  # Interactive display
  "DT",              # DataTables for R
  "plotly",          # Interactive plots
  # Data manipulation
  "dplyr",           # Data wrangling
  "tidyr",           # Data tidying
  # Plotting
  "ggplot2",         # Static plots
  "scales",          # Scale functions for ggplot2
  # File I/O
  "readxl",          # Excel import
  "openxlsx",        # Excel export
  # Utilities
  "digest",          # SHA-256 hashing for data integrity
  "jsonlite"         # JSON for analysis settings
)

# R Recommended packages (Trusted Resource — low risk by definition)
recommended_packages <- c(
  "nlme"             # Mixed-effects models for BE analysis
)

cat("\n")
cat("================================================================\n")
cat("  NCA Assistant — Package Risk Assessment\n")
cat("  Framework: R Validation Hub / pharmaR White Paper (2020)\n")
cat(sprintf("  Date: %s\n", Sys.Date()))
cat(sprintf("  R: %s\n", R.version.string))
cat("================================================================\n\n")

# ── Assess recommended packages (Trusted Resource) ──────────────────────────
cat("--- R Recommended Packages (Trusted Resource: R Foundation) ---\n\n")
for (pkg in recommended_packages) {
  ver <- tryCatch(as.character(packageVersion(pkg)), error = function(e) "NOT INSTALLED")
  cat(sprintf("  %-15s v%-12s  Risk: LOW (Trusted Resource)\n", pkg, ver))
}

# ── Assess contributed packages via riskmetric ───────────────────────────────
cat("\n--- Contributed Packages (riskmetric assessment) ---\n\n")

# Run riskmetric
cat("  Running riskmetric assessment (this may take a moment)...\n\n")

refs <- tryCatch(
  pkg_ref(contributed_packages),
  error = function(e) {
    cat("  Warning: Could not create package references.\n")
    cat("  Ensure all packages are installed: renv::restore()\n")
    cat("  Error:", e$message, "\n")
    return(NULL)
  }
)

if (!is.null(refs)) {
  assessments <- pkg_assess(refs)
  scores <- pkg_score(assessments)

  # Extract summary
  score_df <- summary(scores)

  # Add classification and purpose
  purpose <- ifelse(
    score_df$package %in% c("NonCompart", "ncar", "PowerTOST"),
    "Statistical",
    "Non-statistical"
  )

  classification <- rep("Import", nrow(score_df))

  result <- data.frame(
    Package = score_df$package,
    Version = sapply(score_df$package, function(p)
      tryCatch(as.character(packageVersion(p)), error = function(e) "?")),
    Classification = classification,
    Purpose = purpose,
    Risk_Score = round(score_df$pkg_score, 3),
    stringsAsFactors = FALSE
  )

  # Determine risk level from score
  # riskmetric scores: 0 = lowest risk, 1 = highest risk
  result$Risk_Level <- ifelse(result$Risk_Score < 0.3, "LOW",
                       ifelse(result$Risk_Score < 0.6, "MEDIUM", "HIGH"))

  # Sort by risk score descending
  result <- result[order(-result$Risk_Score), ]

  # Print summary table
  for (i in seq_len(nrow(result))) {
    r <- result[i, ]
    cat(sprintf("  %-15s v%-12s  %-14s  %-16s  Score: %.3f  Risk: %s\n",
                r$Package, r$Version, r$Classification, r$Purpose,
                r$Risk_Score, r$Risk_Level))
  }

  # Add recommended packages to output
  rec_df <- data.frame(
    Package = recommended_packages,
    Version = sapply(recommended_packages, function(p)
      tryCatch(as.character(packageVersion(p)), error = function(e) "?")),
    Classification = "Import (Recommended)",
    Purpose = "Statistical",
    Risk_Score = 0,
    Risk_Level = "LOW (Trusted)",
    stringsAsFactors = FALSE
  )
  full_result <- rbind(result, rec_df)

  # Save to CSV
  out_dir <- if (dir.exists("validation")) "validation" else "."
  out_path <- file.path(out_dir, "package_risk_scores.csv")
  write.csv(full_result, out_path, row.names = FALSE)
  cat(sprintf("\n  Results saved to: %s\n", out_path))

  # Summary statistics
  cat("\n  --- Summary ---\n")
  cat(sprintf("  Total packages assessed:  %d contributed + %d recommended\n",
              nrow(result), length(recommended_packages)))
  cat(sprintf("  Statistical packages:     %d\n", sum(result$Purpose == "Statistical")))
  cat(sprintf("  Non-statistical packages: %d\n", sum(result$Purpose == "Non-statistical")))
  cat(sprintf("  LOW risk:    %d\n", sum(result$Risk_Level == "LOW")))
  cat(sprintf("  MEDIUM risk: %d\n", sum(result$Risk_Level == "MEDIUM")))
  cat(sprintf("  HIGH risk:   %d\n", sum(result$Risk_Level == "HIGH")))

  # Flag statistical packages for attention
  stat_pkgs <- result[result$Purpose == "Statistical", ]
  if (any(stat_pkgs$Risk_Level != "LOW")) {
    cat("\n  Note: Statistical packages with elevated risk scores may\n")
    cat("  warrant additional testing. NCA Assistant's cross-validation\n")
    cat("  suite (52 tests) validates NonCompart and PowerTOST against\n")
    cat("  analytical ground truth and published reference values.\n")
  }
} else {
  cat("  Skipped — riskmetric assessment failed.\n")
  cat("  Install packages first: renv::restore()\n")
}

cat("\n================================================================\n\n")
