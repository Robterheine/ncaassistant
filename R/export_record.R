# ============================================================================
# NCA Assistant — Complete Analysis Record Export
# ============================================================================
# Generates a self-contained zip with results, settings, reproducibility
# script, data integrity hash, and summary document.

# Schema version for analysis_settings.json / figure_settings.json. Increment
# when the settings structure changes so downstream tooling can branch on it.
RECORD_SCHEMA_VERSION <- "1.3.0"

#' Zip the contents of a directory into output_path without changing the global
#' working directory (session-safe in multi-session Shiny deployments).
#'
#' Primary: system2("zip", "-j") with absolute paths (junk paths, no setwd).
#' Fallback: withr::with_dir() if available, else a scoped setwd with guaranteed
#' on.exit restore.
#'
#' @param rec_dir Directory whose files should be zipped (non-recursive)
#' @param output_path Destination zip path
#' @return invisibly, output_path
zip_record_dir <- function(rec_dir, output_path) {
  files_to_zip <- list.files(rec_dir, full.names = TRUE)
  abs_output   <- normalizePath(output_path, mustWork = FALSE)

  tryCatch({
    res <- system2("zip",
                   args   = c("-j", shQuote(abs_output), shQuote(files_to_zip)),
                   stdout = FALSE, stderr = FALSE)
    if (!is.null(res) && res != 0) stop("system zip returned non-zero exit")
    res
  }, error = function(e) {
    tryCatch({
      if (requireNamespace("withr", quietly = TRUE)) {
        withr::with_dir(rec_dir, {
          utils::zip(abs_output, files = basename(files_to_zip), flags = "-j")
        })
      } else {
        old_wd <- setwd(rec_dir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(abs_output, files = basename(files_to_zip), flags = "-j")
      }
    }, error = function(e2) {
      warning("Could not create zip archive: ", e2$message)
    })
  })

  invisible(output_path)
}

#' SHA-256 hash of a file, or a placeholder when it can't be computed.
sha256_or_na <- function(path) {
  if (!is.null(path) && file.exists(path) &&
      requireNamespace("digest", quietly = TRUE)) {
    digest::digest(file = path, algo = "sha256")
  } else {
    "not computed"
  }
}

#' Write a three-way data-integrity manifest (data_integrity.txt)
#'
#' Records a SHA-256 hash for each artefact in the record (source data, analysis
#' settings, and results — plus the figure for visualisation records) so the
#' whole package is independently verifiable. This is the traceability anchor:
#' recompute any hash and confirm it matches, linking source data → settings →
#' results.
#'
#' @param rec_dir Record directory to write data_integrity.txt into
#' @param artifacts Named list: label -> absolute file path (order preserved)
#' @return invisibly, a named character vector of label -> hash
write_integrity_manifest <- function(rec_dir, artifacts) {
  hashes <- character(0)
  lines <- c(
    "Data Integrity Verification (SHA-256)",
    "=====================================",
    "",
    "This manifest fingerprints every artefact in this analysis record so the",
    "package is independently verifiable and traceable end to end:",
    "source data → analysis settings → results.",
    "Recompute any hash below and confirm it matches; a match proves that file",
    "has not been altered since the analysis was performed.",
    "")
  for (lab in names(artifacts)) {
    p <- artifacts[[lab]]
    h <- sha256_or_na(p)
    hashes[[lab]] <- h
    sz <- if (!is.null(p) && file.exists(p)) as.character(file.info(p)$size) else "NA"
    lines <- c(lines,
               paste0(lab, ":"),
               paste0("  file:    ", if (!is.null(p)) basename(p) else "(none)"),
               paste0("  size:    ", sz, " bytes"),
               paste0("  SHA-256: ", h),
               "")
  }
  lines <- c(lines,
             "To verify any file, in R run:",
             "  digest::digest(file = \"<filename>\", algo = \"sha256\")",
             "and confirm the result matches the hash above.",
             "",
             paste0("Computed at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
  writeLines(lines, file.path(rec_dir, "data_integrity.txt"))
  invisible(hashes)
}






#' Generate the analysis summary HTML document
#' @return Character string containing HTML
generate_summary_html <- function(settings, col_map, file_name, file_hash,
                                   blq_rule, lloq, analyst, study_name,
                                   n_subjects, n_obs, analysis_type = "NCA",
                                   lz_overrides = NULL, reproduction = NULL) {
  
  ver <- tryCatch(get("APP_VERSION", envir = globalenv()), error = function(e) "?")
  r_ver <- tryCatch(R.version.string, error = function(e) "R")
  
  pkg_ver <- function(pkg) {
    tryCatch(as.character(packageVersion(pkg)), error = function(e) "?")
  }
  
  blq_desc <- switch(blq_rule,
    "rule1" = "Rule 1 (Standard): Pre-first-quantifiable = 0, post-last-quantifiable = missing",
    "rule2" = "Rule 2: All BLQ = 0",
    "rule3" = "Rule 3: All BLQ excluded",
    "rule4" = "Rule 4: All BLQ = LLOQ/2",
    "rule5" = "Rule 5: Pre-Cmax = 0, post-Cmax = missing",
    "rule6" = "Rule 6: BLQ before first quantifiable = LLOQ/2, other BLQ = 0",
    blq_rule
  )
  
  route_desc <- switch(settings$admin_route,
    "extravascular" = "Extravascular (oral / IM / SC)",
    "iv_bolus" = "Intravenous bolus",
    "iv_infusion" = "Intravenous infusion",
    settings$admin_route
  )
  
  trap_desc <- if (settings$trap_method == "log") "Linear-up / Log-down" else "Linear"
  pauc <- partial_auc_spec(settings$partial_aucs)
  
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Analysis Record — ', htmltools::htmlEscape(study_name), '</title>
<style>
  body { font-family: "Segoe UI", Calibri, Arial, sans-serif; max-width: 800px;
         margin: 2rem auto; padding: 0 1rem; color: #2C3E50; font-size: 14px; line-height: 1.6; }
  h1 { color: #2C3E50; border-bottom: 3px solid #3498DB; padding-bottom: 0.5rem; }
  h2 { color: #3498DB; margin-top: 2rem; }
  h3 { color: #2C3E50; }
  table { border-collapse: collapse; width: 100%; margin: 1rem 0; }
  th, td { border: 1px solid #ddd; padding: 6px 10px; text-align: left; }
  th { background: #f5f5f5; }
  .info-box { background: #EBF5FB; border-left: 4px solid #3498DB;
              padding: 12px 16px; margin: 1rem 0; border-radius: 4px; }
  .hash { font-family: monospace; background: #f5f5f5; padding: 2px 6px; border-radius: 3px; word-break: break-all; }
  .footer { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid #ddd;
            font-size: 12px; color: #7f8c8d; }
</style>
</head>
<body>

<h1>Complete Analysis Record</h1>

<div class="info-box">
<strong>What is this document?</strong><br>
This package contains everything needed to reproduce the pharmacokinetic analysis
without the NCA Assistant app. <code>reproduce_analysis.R</code> runs the app\'s own
pipeline code (<code>nca_pipeline.R</code>, shipped in this package) with the settings in
<code>analysis_settings.json</code> and compares the result with the app\'s. SHA-256 hashes
verify that the data file and the pipeline code are the ones used.
', if (!is.null(reproduction)) paste0('<br><br>\n<strong>Reproduction check at export:</strong> ',
  htmltools::htmlEscape(reproduction), ' (details in <code>reproduction_check.txt</code>). ',
  'This demonstrates reproducibility, not independent verification: the same algorithms and ',
  'packages are re-executed.') else '', '
<br><br>
<strong>CDISC parameter codes:</strong> ', htmltools::htmlEscape(cdisc_ct_statement()),
' The codes are listed in the sheet <code>CDISC_Parameter_Codes</code> of <code>results.xlsx</code>.
This is a code lookup only; the results are not an SDTM PP dataset and no claim of conformance to
CDISC standards is made.

', if (identical(analysis_type, "Bioequivalence")) paste0('<br><br>
<strong>Scope:</strong> the script recomputes the NCA parameters. The bioequivalence
statistics (ANOVA, confidence intervals and verdict) are recorded in <code>results.xlsx</code>
and <code>analysis_settings.json</code> but are not recomputed by the script.
') else '', '
</div>

<h2>1. Study Information</h2>
<table>
<tr><th>Study name</th><td>', htmltools::htmlEscape(study_name), '</td></tr>
<tr><th>Analyst</th><td>', htmltools::htmlEscape(analyst), '</td></tr>
<tr><th>Analysis type</th><td>', analysis_type, '</td></tr>
<tr><th>Date &amp; time</th><td>', format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), '</td></tr>
<tr><th>Subjects</th><td>', n_subjects, '</td></tr>
<tr><th>Observations</th><td>', n_obs, '</td></tr>
</table>

<h2>2. Data File</h2>
<table>
<tr><th>File name</th><td><code>', htmltools::htmlEscape(file_name), '</code></td></tr>
<tr><th>SHA-256 hash</th><td><span class="hash">', file_hash, '</span></td></tr>
</table>
<p style="font-size:12px; color:#7f8c8d;">
The SHA-256 hash is a digital fingerprint of the data file. If you recalculate the hash
of your data file and it matches the value above, the file has not been modified since
this analysis was performed.
</p>

<h2>3. Analysis Settings</h2>
<table>
<tr><th>Route of administration</th><td>', route_desc, '</td></tr>',
if (settings$admin_route == "iv_infusion")
  paste0('<tr><th>Infusion duration</th><td>', settings$infusion_duration, ' ', settings$time_unit, '</td></tr>') else "",
'<tr><th>Dose</th><td>', if (length(settings$dose) > 1) paste(unique(settings$dose), collapse=", ") else settings$dose,
  ' ', settings$dose_unit, if (length(settings$dose) > 1) " (per profile, from the Dose column)" else "", '</td></tr>
<tr><th>Steady state</th><td>', if (settings$is_steady_state) "Yes" else "No", '</td></tr>',
if (!is.null(pauc)) paste0('<tr><th>Partial AUC intervals</th><td>',
  paste(htmltools::htmlEscape(paste0(.pauc_num(pauc$start), "\u2013", pauc$end, " ", settings$time_unit,
    ifelse(pauc$cmax, " (with Cmax and Tmax)", ""),
    if (identical(analysis_type, "Bioequivalence")) paste0(", ", pauc$role) else "")), collapse = "<br>"),
  '</td></tr>') else "", ' 
<tr><th>Trapezoidal method</th><td>', trap_desc, '</td></tr>
<tr><th>Min R&sup2; for half-life</th><td>', settings$r2adj_threshold, '</td></tr>
<tr><th>BLQ handling</th><td>', blq_desc, '</td></tr>
<tr><th>LLOQ</th><td>', lloq, ' ', settings$conc_unit, '</td></tr>
<tr><th>Units</th><td>Dose: ', settings$dose_unit, ' | Time: ', settings$time_unit,
  ' | Conc: ', settings$conc_unit, '</td></tr>
</table>

<h2>4. Column Mapping</h2>
<table>
<tr><th>Subject</th><td>', col_map$subject, '</td></tr>
<tr><th>Time</th><td>', col_map$time, '</td></tr>
<tr><th>Concentration</th><td>', col_map$conc, '</td></tr>',
if (!is.null(col_map$treatment)) paste0('<tr><th>Treatment</th><td>', col_map$treatment, '</td></tr>') else "",
if (!is.null(col_map$period)) paste0('<tr><th>Period</th><td>', col_map$period, '</td></tr>') else "",
if (!is.null(col_map$sequence)) paste0('<tr><th>Sequence</th><td>', col_map$sequence, '</td></tr>') else "",
if (!is.null(col_map$dose)) paste0('<tr><th>Dose</th><td>', col_map$dose, '</td></tr>') else "",
'</table>

<h2>5. Statistical Methods</h2>
<h3>Non-Compartmental Analysis</h3>
<p>Pharmacokinetic parameters were estimated by non-compartmental analysis
using the NonCompart R package (version ', pkg_ver("NonCompart"),
'; Kim et al., 2018). The area under the concentration&ndash;time curve
(AUC<sub>0&ndash;t</sub>) was calculated using the ', tolower(trap_desc),
' trapezoidal method. The terminal elimination rate constant (&lambda;<sub>z</sub>)
was estimated by log-linear regression of the terminal phase, selecting the
combination of points with the highest adjusted R&sup2; (&ge; ',
settings$r2adj_threshold, '). The terminal half-life was calculated as
ln(2)/&lambda;<sub>z</sub>. C<sub>max</sub> and T<sub>max</sub> were obtained
directly from the observed data.</p>',
if (isTRUE(settings$is_steady_state))
paste0('<p>Steady-state analysis with a dosing interval &tau; = ', settings$tau, ' ', settings$time_unit,
'. AUC<sub>&tau;</sub> is the AUC from 0 to &tau; (interpolated between samples, extrapolated with
&lambda;<sub>z</sub> beyond the last sample). Clearance and volume were calculated from AUC<sub>&tau;</sub>;
C<sub>avg</sub> = AUC<sub>&tau;</sub>/&tau;.</p>') else "",
if (!is.null(pauc)) paste0('<p>Partial AUCs were calculated over the intervals in section 3, with the
same trapezoidal method as AUC<sub>0&ndash;t</sub>. An interval ending at t ended at the last measurable
concentration of each profile; its partial AUC was AUC<sub>0&ndash;t</sub> minus the AUC from 0 to the
start. At a cutoff between two samples the concentration was interpolated. Partial AUCs were not
extrapolated: when an interval reached beyond the last measurable concentration, no value was reported.',
if (identical(analysis_type, "Bioequivalence")) ' In the bioequivalence analysis partial AUCs were
analysed with the same model as the other metrics. Pivotal intervals received a verdict, supportive
intervals only a ratio and confidence interval. A metric with a value of zero in any profile received no
estimate and no verdict, because zero cannot be log-transformed.' else "",
' The intervals are recorded as entered; the app cannot check that they were pre-specified in the
protocol.</p>') else "",
if (lloq > 0) paste0('<p>Concentrations below the LLOQ (', lloq, ' ', settings$conc_unit,
  ') were handled using ', blq_desc, '.</p>') else "",
'

<h2>6. Software Environment</h2>
<table>
<tr><th>NCA Assistant</th><td>v', ver, '</td></tr>
<tr><th>R</th><td>', r_ver, '</td></tr>
<tr><th>NonCompart</th><td>', pkg_ver("NonCompart"), '</td></tr>
<tr><th>nlme</th><td>', pkg_ver("nlme"), '</td></tr>
<tr><th>PowerTOST</th><td>', pkg_ver("PowerTOST"), '</td></tr>
<tr><th>Operating system</th><td>', sessionInfo()$running, '</td></tr>
</table>

<h2>7. How to Reproduce</h2>
<div class="info-box">
<strong>Instructions:</strong>
<ol>
<li>Place <code>reproduce_analysis.R</code> and <code>', htmltools::htmlEscape(file_name),
'</code> in the same folder.</li>
<li>Open R or RStudio.</li>
<li>Set the working directory to that folder:<br>
<code>setwd("path/to/your/folder")</code></li>
<li>Run the script:<br>
<code>source("reproduce_analysis.R")</code></li>
<li>The script installs any missing packages automatically, produces
<code>reproduced_results.csv</code>, and <strong>automatically compares</strong> its
output against the app\'s results (<code>app_results_reference.csv</code>, included),
printing a <code>MATCH</code> / <code>DIFFERENT</code> verdict. It also re-checks the
data file\'s SHA-256 against the recorded value.</li>
</ol>
<p style="font-size:12px; color:#7f8c8d; margin-bottom:0;">
Independent integrity: <code>data_integrity.txt</code> lists SHA-256 hashes for the
source data, the analysis settings, and the results, so every artefact in this
package can be verified separately.</p>
</div>

<h2>8. References</h2>
<p>Kim H, Han S, Cho YS, Yoon SK, Bae KS. Development of R packages: &lsquo;NonCompart&rsquo; and &lsquo;ncar&rsquo;
for noncompartmental analysis (NCA). <em>Transl Clin Pharmacol</em>. 2018;26(1):10&ndash;15.</p>
<p>R Core Team. R: A Language and Environment for Statistical Computing.
R Foundation for Statistical Computing, Vienna, Austria.</p>

', if (!is.null(lz_overrides) && length(lz_overrides) > 0) {
  rows <- sapply(lz_overrides, function(ov) {
    paste0('<tr><td>', ov$profile, '</td>',
           '<td>', if (!is.na(ov$original_lambda_z)) signif(ov$original_lambda_z, 5) else "N/A", '</td>',
           '<td>', signif(ov$adjusted_lambda_z, 5), '</td>',
           '<td>', if (!is.na(ov$original_r2adj)) signif(ov$original_r2adj, 4) else "N/A", '</td>',
           '<td>', if (!is.na(ov$adjusted_r2adj)) signif(ov$adjusted_r2adj, 4) else "N/A", '</td>',
           '<td>', ov$points_used, '</td></tr>')
  })
  paste0('<h2>9. Manual Lambda-z Overrides</h2>\n',
         '<p>', length(lz_overrides), ' profile(s) had manual terminal phase adjustments:</p>\n',
         '<table border="1" cellpadding="4" style="border-collapse: collapse; font-size: 13px;">\n',
         '<tr style="background:#eee;"><th>Profile</th><th>Original &lambda;z</th>',
         '<th>Adjusted &lambda;z</th><th>Original R&sup2;</th>',
         '<th>Adjusted R&sup2;</th><th>Points</th></tr>\n',
         paste(rows, collapse = '\n'), '\n</table>\n',
         '<p style="font-size:12px;color:#666;">These adjustments are reflected in the results Excel ',
         'and are applied by the reproducibility script (reproduce_analysis.R).</p>\n')
} else '', '

<div class="footer">
Generated by NCA Assistant v', ver, ' on ', format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
'<br>This document is part of the Complete Analysis Record and should be kept
together with the other files in this package.
</div>

</body>
</html>')
}


#' Create the complete analysis record zip file
#' @param output_path Path to write the zip file
#' @param results Data frame of NCA results
#' @param settings List of analysis settings
#' @param col_map Column mapping
#' @param original_file_path Path to the original uploaded data file
#' @param original_file_name Name of the original file
#' @param blq_rule BLQ rule used
#' @param lloq LLOQ value
#' @param analyst Analyst name
#' @param study_name Study name
#' @param summary_stats Optional summary statistics data frame
#' @param be_results Optional BE results list
#' @param be_settings Optional list of the BE settings used (design, model,
#'   CI level, limits, point-estimate constraint, parameters)
create_analysis_record <- function(output_path, results, settings, col_map,
                                    original_file_path, original_file_name,
                                    blq_rule, lloq, analyst = "Analyst",
                                    study_name = "Untitled Study",
                                    summary_stats = NULL,
                                    be_results = NULL,
                                    be_settings = NULL,
                                    lz_overrides = NULL,
                                    viz_settings = NULL,
                                    read_args = NULL,
                                    adnca = NULL) {
  
  # The name comes from the browser: keep only the file name, never a path
  original_file_name <- basename(original_file_name)

  # Create temp directory
  tmp <- tempdir()
  rec_dir <- file.path(tmp, "analysis_record")
  if (dir.exists(rec_dir)) unlink(rec_dir, recursive = TRUE)
  dir.create(rec_dir, recursive = TRUE)
  
  n_subjects <- if ("Subject" %in% names(results)) length(unique(results$Subject))
                else nrow(results)
  n_obs <- if (!is.null(settings$n_obs)) settings$n_obs else "?"
  analysis_type <- if (!is.null(be_results)) "Bioequivalence" else "Non-Compartmental Analysis"

  # SHA-256 of the source data file — recorded in the settings (so the
  # reproduction can confirm the data is unchanged) and in the manifest.
  data_sha256 <- sha256_or_na(original_file_path)

  # The data file and the pipeline code travel with the record
  tryCatch({
    file.copy(original_file_path, file.path(rec_dir, original_file_name), overwrite = TRUE)
  }, error = function(e) warning("Could not copy data file: ", e$message))
  pipeline_sha256 <- .ship_pipeline(rec_dir)
  shipped_adnca <- .ship_adnca(rec_dir, adnca, original_file_path)

  # 1. Results Excel
  tryCatch({
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Individual_Parameters")
    openxlsx::writeData(wb, 1, rename_nca_columns(results))
    if (!is.null(summary_stats)) {
      openxlsx::addWorksheet(wb, "Summary_Statistics")
      openxlsx::writeData(wb, 2, rename_summary_columns(summary_stats))
    }
    if (!is.null(be_results)) {
      openxlsx::addWorksheet(wb, "BE_Confidence_Intervals")
      openxlsx::writeData(wb, "BE_Confidence_Intervals", rename_be_columns(be_results$ci_table, ci_level = if (is.null(be_settings$ci_level)) 90 else be_settings$ci_level))
      if (!is.null(be_results$cv_table)) {
        openxlsx::addWorksheet(wb, "Within_Subject_Variability")
        openxlsx::writeData(wb, "Within_Subject_Variability", be_results$cv_table)
      }
      for (p in names(be_results$anova)) {
        sn <- substr(paste0("ANOVA_", friendly_name(p)), 1, 31)
        openxlsx::addWorksheet(wb, sn)
        df <- as.data.frame(be_results$anova[[p]])
        df$Source <- rownames(df)
        openxlsx::writeData(wb, sn, df)
      }
    }
    add_cdisc_code_sheet(wb, names(results)[vapply(results, is.numeric, logical(1))],
                         settings$admin_route, isTRUE(settings$is_steady_state))
    openxlsx::saveWorkbook(wb, file.path(rec_dir, "results.xlsx"), overwrite = TRUE)
  }, error = function(e) warning("Could not create results.xlsx: ", e$message))

  # 1b. Machine-readable reference results (raw NonCompart columns) so the
  #     reproduce script can compare its output against the app automatically.
  tryCatch({
    write.csv(results, file.path(rec_dir, "app_results_reference.csv"),
              row.names = FALSE)
  }, error = function(e) warning("Could not write app_results_reference.csv: ", e$message))

  # 2. Settings JSON
  tryCatch({
    settings_export <- list(
      schema_version  = RECORD_SCHEMA_VERSION,
      app_version     = tryCatch(get("APP_VERSION", envir = globalenv()), error = function(e) "?"),
      r_version       = R.version.string,
      timestamp       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      analyst         = analyst,
      study_name      = study_name,
      analysis_type   = analysis_type,
      input_file      = original_file_name,
      data_sha256     = data_sha256,
      pipeline_sha256 = pipeline_sha256,
      read_args       = if (is.null(read_args)) list() else read_args,
      column_mapping  = col_map,
      nca_profile_key = c("Subject",
                          if (!is.null(col_map$treatment)) "Treatment",
                          if (!is.null(col_map$period)) "Period"),
      admin_route     = settings$admin_route,
      dose            = settings$dose,
      dose_source     = if (!is.null(settings$dose_source)) settings$dose_source else
                          if (length(settings$dose) > 1) "per_subject" else "single",
      dose_unit       = settings$dose_unit,
      time_unit       = settings$time_unit,
      conc_unit       = settings$conc_unit,
      infusion_dur    = settings$infusion_duration,
      steady_state    = settings$is_steady_state,
      tau             = settings$tau,
      trap_method     = settings$trap_method,
      r2adj_threshold = settings$r2adj_threshold,
      mw              = if (is.null(settings$mw)) 0 else settings$mw,
      blq_rule        = blq_rule,
      lloq            = lloq,
      packages = list(
        NonCompart = tryCatch(as.character(packageVersion("NonCompart")), error = function(e) "?"),
        nlme       = tryCatch(as.character(packageVersion("nlme")), error = function(e) "?"),
        PowerTOST  = tryCatch(as.character(packageVersion("PowerTOST")), error = function(e) "?")
      )
    )
    settings_export <- c(settings_export, shipped_adnca$json,
                         list(cdisc_terminology = .cdisc_json()))
    pauc <- partial_auc_spec(settings$partial_aucs)
    if (!is.null(pauc)) settings_export$partial_aucs <- pauc
    if (!is.null(be_results)) {
      settings_export$bioequivalence <- be_settings
      settings_export$reproduction_scope <- paste(
        "reproduce_analysis.R recomputes the NCA parameters only. The bioequivalence",
        "statistics (ANOVA, confidence intervals, verdict) are recorded in results.xlsx",
        "and are not recomputed by the script.")
    }
    if (!is.null(lz_overrides) && length(lz_overrides) > 0) {
      settings_export$lz_overrides <- lz_overrides
    }
    if (!is.null(viz_settings) && length(viz_settings) > 0) {
      settings_export$visualization <- viz_settings
    }
    .write_json(settings_export, file.path(rec_dir, "analysis_settings.json"))
  }, error = function(e) warning("Could not create settings JSON: ", e$message))
  
  # 3. Reproducibility R script (generic: sources nca_pipeline.R, reads the JSON)
  tryCatch({
    writeLines(generate_nca_script(), file.path(rec_dir, "reproduce_analysis.R"))
  }, error = function(e) warning("Could not create R script: ", e$message))

  # 4. Data integrity manifest (source data, settings, results, pipeline code)
  tryCatch({
    write_integrity_manifest(rec_dir, c(list(
      "Source data"       = original_file_path,
      "Analysis settings" = file.path(rec_dir, "analysis_settings.json"),
      "Results (Excel)"   = file.path(rec_dir, "results.xlsx"),
      "Pipeline code"     = file.path(rec_dir, "nca_pipeline.R")
    ), shipped_adnca$manifest))
  }, error = function(e) warning("Could not create integrity file: ", e$message))

  # 5. Run the reproduction now, so the user knows before download
  verdict <- run_reproduction_check(rec_dir, "reproduce_analysis.R")

  # 6. Summary HTML
  tryCatch({
    html <- generate_summary_html(settings, col_map, original_file_name,
                                   data_sha256, blq_rule, lloq, analyst,
                                   study_name, n_subjects, n_obs, analysis_type,
                                   lz_overrides, reproduction = verdict)
    writeLines(html, file.path(rec_dir, "analysis_summary.html"))
  }, error = function(e) warning("Could not create summary HTML: ", e$message))

  # Create zip — session-safe, no global setwd (see zip_record_dir).
  zip_record_dir(rec_dir, output_path)
  unlink(rec_dir, recursive = TRUE)

  attr(output_path, "reproduction") <- verdict
  invisible(output_path)
}


#' Create the Complete Analysis Record zip for a single-subject NCA
#'
#' Single-subject results are a named vector from NonCompart::sNCA rather than
#' a per-subject data frame, so this is a dedicated entry point. It produces the
#' same six-file record as create_analysis_record() and reuses the shared script,
#' HTML, and zip helpers, so the single-subject record is now on par with the
#' batch and BE records (full package list, BLQ rule, LLOQ, schema version, and
#' lambda-z override capture).
#'
#' @param output_path Path to write the zip file
#' @param result Named vector of NCA parameters (from sNCA, override applied)
#' @param settings List of NCA settings (admin_route, dose, units, trap_method,
#'   r2adj_threshold, infusion_duration, is_steady_state, n_obs)
#' @param time_vec,conc_vec The analysed profile (for the reproducibility script)
#' @param subject_label Label for the subject / profile
#' @param original_file_path,original_file_name Source data file (NULL = manual)
#' @param blq_rule,lloq BLQ handling actually applied ("none"/0 for manual entry)
#' @param analyst,study_name Free-text metadata
#' @param lz_override Optional single override entry with $time_used plus the
#'   audit fields (profile, original_lambda_z, adjusted_lambda_z, original_r2adj,
#'   adjusted_r2adj, points_used); NULL when no manual adjustment was made
create_single_analysis_record <- function(output_path, result, settings,
                                           time_vec, conc_vec,
                                           subject_label = "Subject",
                                           original_file_path = NULL,
                                           original_file_name = "manual_entry.csv",
                                           blq_rule = "none", lloq = 0,
                                           analyst = "Analyst",
                                           study_name = "Untitled Study",
                                           lz_override = NULL,
                                           col_map = NULL,
                                           read_args = NULL,
                                           adnca = NULL) {

  original_file_name <- basename(original_file_name)
  tmp <- tempdir()
  rec_dir <- file.path(tmp, "analysis_record")
  if (dir.exists(rec_dir)) unlink(rec_dir, recursive = TRUE)
  dir.create(rec_dir, recursive = TRUE)

  has_file <- !is.null(original_file_path) && file.exists(original_file_path) && !is.null(col_map)

  # Wrap the single override entry into the named-list shape that the shared
  # HTML/JSON helpers expect (keyed by the profile label).
  lz_overrides <- if (!is.null(lz_override)) {
    stats::setNames(list(lz_override), subject_label)
  } else NULL

  # 0. The data travel with the record: the uploaded file, or the typed values
  if (has_file) {
    input_file <- original_file_name
    file.copy(original_file_path, file.path(rec_dir, input_file), overwrite = TRUE)
  } else {
    input_file <- "manual_entry.csv"
    write.csv(data.frame(Time = time_vec, Concentration = conc_vec),
              file.path(rec_dir, input_file), row.names = FALSE)
  }
  source_path <- file.path(rec_dir, input_file)
  data_sha256 <- sha256_or_na(source_path)
  pipeline_sha256 <- .ship_pipeline(rec_dir)
  shipped_adnca <- .ship_adnca(rec_dir, if (has_file) adnca else NULL, source_path)

  # 1. Results Excel (friendly Parameter / Abbreviation / Value layout)
  tryCatch({
    df <- data.frame(
      Parameter    = vapply(names(result), friendly_name, character(1)),
      Abbreviation = names(result),
      Value        = as.character(result),
      stringsAsFactors = FALSE
    )
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "NCA_Parameters")
    openxlsx::writeData(wb, 1, df)
    add_cdisc_code_sheet(wb, names(result), settings$admin_route, isTRUE(settings$is_steady_state))
    openxlsx::saveWorkbook(wb, file.path(rec_dir, "results.xlsx"), overwrite = TRUE)
  }, error = function(e) warning("Could not create results.xlsx: ", e$message))

  # 1b. Machine-readable reference results for the automatic comparison
  tryCatch({
    write.csv(data.frame(Parameter = names(result), Value = as.character(result),
                         stringsAsFactors = FALSE),
              file.path(rec_dir, "app_results_reference.csv"), row.names = FALSE)
  }, error = function(e) warning("Could not write app_results_reference.csv: ", e$message))

  # 2. Settings JSON — the single source of parameters for the reproduction
  tryCatch({
    settings_export <- list(
      schema_version  = RECORD_SCHEMA_VERSION,
      app_version     = tryCatch(get("APP_VERSION", envir = globalenv()), error = function(e) "?"),
      r_version       = R.version.string,
      timestamp       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      analyst         = analyst,
      study_name      = study_name,
      analysis_type   = "Single-Subject NCA",
      subject         = subject_label,
      input_file      = input_file,
      data_source     = if (has_file) "uploaded_file" else "manual_entry",
      data_sha256     = data_sha256,
      pipeline_sha256 = pipeline_sha256,
      read_args       = if (is.null(read_args)) list() else read_args,
      column_mapping  = if (has_file) col_map else list(),
      admin_route     = settings$admin_route,
      dose            = settings$dose,
      dose_unit       = settings$dose_unit,
      time_unit       = settings$time_unit,
      conc_unit       = settings$conc_unit,
      infusion_dur    = if (is.null(settings$infusion_duration)) 0 else settings$infusion_duration,
      steady_state    = isTRUE(settings$is_steady_state),
      tau             = settings$tau,
      trap_method     = settings$trap_method,
      r2adj_threshold = if (is.null(settings$r2adj_threshold)) 0.7 else settings$r2adj_threshold,
      mw              = if (is.null(settings$mw)) 0 else settings$mw,
      blq_rule        = if (has_file) blq_rule else "none",
      lloq            = if (has_file) lloq else 0,
      packages = list(
        NonCompart = tryCatch(as.character(packageVersion("NonCompart")), error = function(e) "?"),
        nlme       = tryCatch(as.character(packageVersion("nlme")), error = function(e) "?"),
        PowerTOST  = tryCatch(as.character(packageVersion("PowerTOST")), error = function(e) "?")
      )
    )
    settings_export <- c(settings_export, shipped_adnca$json,
                         list(cdisc_terminology = .cdisc_json()))
    pauc <- partial_auc_spec(settings$partial_aucs)
    if (!is.null(pauc)) settings_export$partial_aucs <- pauc
    if (!is.null(lz_overrides)) settings_export$lz_overrides <- lz_overrides
    .write_json(settings_export, file.path(rec_dir, "analysis_settings.json"))
  }, error = function(e) warning("Could not create settings JSON: ", e$message))

  # 3. Reproducibility R script (generic)
  tryCatch({
    writeLines(generate_single_nca_script(), file.path(rec_dir, "reproduce_analysis.R"))
  }, error = function(e) warning("Could not create R script: ", e$message))

  # 4. Integrity manifest
  tryCatch({
    write_integrity_manifest(rec_dir, c(list(
      "Source data"       = source_path,
      "Analysis settings" = file.path(rec_dir, "analysis_settings.json"),
      "Results (Excel)"   = file.path(rec_dir, "results.xlsx"),
      "Pipeline code"     = file.path(rec_dir, "nca_pipeline.R")
    ), shipped_adnca$manifest))
  }, error = function(e) warning("Could not create integrity file: ", e$message))

  # 5. Run the reproduction now, so the user knows before download
  verdict <- run_reproduction_check(rec_dir, "reproduce_analysis.R")

  # 6. Summary HTML
  tryCatch({
    cm <- list(subject = "Subject", time = "Time", conc = "Concentration")
    html <- generate_summary_html(settings, cm, input_file,
                                   data_sha256, blq_rule, lloq, analyst,
                                   study_name, 1, length(time_vec),
                                   "Single-Subject NCA", lz_overrides, reproduction = verdict)
    writeLines(html, file.path(rec_dir, "analysis_summary.html"))
  }, error = function(e) warning("Could not create summary HTML: ", e$message))

  zip_record_dir(rec_dir, output_path)
  unlink(rec_dir, recursive = TRUE)
  attr(output_path, "reproduction") <- verdict
  invisible(output_path)
}




#' Generate the figure-provenance HTML document for a Visualize-tab record
generate_viz_html <- function(viz_settings, col_map, file_name, file_hash,
                              analyst, study_name, n_subjects, n_obs) {
  ver   <- tryCatch(get("APP_VERSION", envir = globalenv()), error = function(e) "?")
  r_ver <- tryCatch(R.version.string, error = function(e) "R")
  pkg_ver <- function(pkg) tryCatch(as.character(packageVersion(pkg)), error = function(e) "?")
  vs <- viz_settings
  yn <- function(x) if (isTRUE(x)) "Yes" else "No"

  paste0('<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8">
<title>Figure Record &mdash; ', htmltools::htmlEscape(study_name), '</title>
<style>
  body { font-family: "Segoe UI", Calibri, Arial, sans-serif; max-width: 800px;
         margin: 2rem auto; padding: 0 1rem; color: #2C3E50; font-size: 14px; line-height: 1.6; }
  h1 { color: #2C3E50; border-bottom: 3px solid #8E44AD; padding-bottom: 0.5rem; }
  h2 { color: #8E44AD; margin-top: 2rem; }
  table { border-collapse: collapse; width: 100%; margin: 1rem 0; }
  th, td { border: 1px solid #ddd; padding: 6px 10px; text-align: left; }
  th { background: #f5f5f5; }
  .info-box { background: #F4ECF7; border-left: 4px solid #8E44AD;
              padding: 12px 16px; margin: 1rem 0; border-radius: 4px; }
  .hash { font-family: monospace; background: #f5f5f5; padding: 2px 6px; border-radius: 3px; word-break: break-all; }
  .footer { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid #ddd; font-size: 12px; color: #7f8c8d; }
</style></head><body>

<h1>Figure Record</h1>
<div class="info-box"><strong>What is this document?</strong><br>
This package contains everything needed to independently reproduce the
concentration&ndash;time figure. The R script (<code>reproduce_figure.R</code>)
rebuilds the figure from the original data using the settings below, without the
NCA Assistant app. The SHA-256 hash verifies the data file is unchanged.</div>

<h2>1. Study Information</h2>
<table>
<tr><th>Study name</th><td>', htmltools::htmlEscape(study_name), '</td></tr>
<tr><th>Analyst</th><td>', htmltools::htmlEscape(analyst), '</td></tr>
<tr><th>Analysis type</th><td>Figure / Visualization</td></tr>
<tr><th>Date &amp; time</th><td>', format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), '</td></tr>
<tr><th>Subjects</th><td>', n_subjects, '</td></tr>
<tr><th>Observations</th><td>', n_obs, '</td></tr>
</table>

<h2>2. Data File</h2>
<table>
<tr><th>File name</th><td><code>', htmltools::htmlEscape(file_name), '</code></td></tr>
<tr><th>SHA-256 hash</th><td><span class="hash">', file_hash, '</span></td></tr>
</table>

<h2>3. Figure Settings</h2>
<table>
<tr><th>Plot type</th><td>', vs$plot_type %||% "spaghetti", '</td></tr>
<tr><th>Y-axis scale</th><td>', vs$y_scale %||% "linear", '</td></tr>
<tr><th>Colour by</th><td>', vs$color_by %||% "subject", '</td></tr>
<tr><th>Summary statistic</th><td>', vs$summary_statistic %||% "geomean", '</td></tr>
<tr><th>Dose-normalized</th><td>', yn(vs$dose_normalized), '</td></tr>
<tr><th>Colour palette</th><td>', vs$colour_palette %||% "default", '</td></tr>
<tr><th>Figure size</th><td>', vs$figure_width_in %||% 7, ' &times; ',
  vs$figure_height_in %||% 5, ' in @ ', vs$dpi %||% 300, ' DPI</td></tr>
<tr><th>Export format</th><td>', toupper(vs$export_format %||% "png"), '</td></tr>
<tr><th>Zero/BLQ excluded from geometric mean</th><td>', vs$blq_excluded_n %||% 0, ' observation(s)</td></tr>
</table>

<h2>4. Software Environment</h2>
<table>
<tr><th>NCA Assistant</th><td>v', ver, '</td></tr>
<tr><th>R</th><td>', r_ver, '</td></tr>
<tr><th>ggplot2</th><td>', pkg_ver("ggplot2"), '</td></tr>
<tr><th>dplyr</th><td>', pkg_ver("dplyr"), '</td></tr>
<tr><th>Operating system</th><td>', sessionInfo()$running, '</td></tr>
</table>

<h2>5. How to Reproduce</h2>
<div class="info-box"><ol>
<li>Place <code>reproduce_figure.R</code> and <code>', htmltools::htmlEscape(file_name), '</code> in the same folder.</li>
<li>Open R or RStudio and <code>setwd()</code> to that folder.</li>
<li>Run <code>source("reproduce_figure.R")</code>.</li>
<li>Compare the generated figure with the one in this package.</li>
</ol></div>

<div class="footer">Generated by NCA Assistant v', ver, ' on ',
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), '<br>
Part of the Figure Record &mdash; keep these files together.</div>
</body></html>')
}


#' Create the Complete Analysis Record zip for a Visualize-tab figure
#'
#' @param output_path Path to write the zip file
#' @param plot_obj A ggplot object (the figure to export)
#' @param viz_settings The shared$viz_settings list
#' @param col_map Column mapping
#' @param original_file_path,original_file_name Source data file
#' @param blq_rule,lloq BLQ handling applied upstream (for provenance)
#' @param analyst,study_name Free-text metadata
#' @param n_subjects,n_obs Counts for the summary
create_viz_record <- function(output_path, plot_obj, viz_settings, col_map,
                              original_file_path, original_file_name,
                              blq_rule = "none", lloq = 0,
                              analyst = "Analyst", study_name = "Untitled Study",
                              n_subjects = NA, n_obs = NA, read_args = NULL, adnca = NULL) {

  original_file_name <- basename(original_file_name)
  tmp <- tempdir()
  rec_dir <- file.path(tmp, "figure_record")
  if (dir.exists(rec_dir)) unlink(rec_dir, recursive = TRUE)
  dir.create(rec_dir, recursive = TRUE)

  fmt <- viz_settings$export_format %||% "png"
  w   <- viz_settings$figure_width_in  %||% 7
  h   <- viz_settings$figure_height_in %||% 5
  dpi <- viz_settings$dpi %||% 300

  # 1. Figure file
  tryCatch({
    ggplot2::ggsave(filename = file.path(rec_dir, paste0("figure.", fmt)),
                    plot = plot_obj, device = fmt,
                    width = w, height = h, dpi = dpi, units = "in")
  }, error = function(e) warning("Could not save figure: ", e$message))

  # Data file and pipeline code travel with the record
  file_hash <- sha256_or_na(original_file_path)
  if (!is.null(original_file_path) && file.exists(original_file_path)) {
    file.copy(original_file_path, file.path(rec_dir, original_file_name), overwrite = TRUE)
  }
  pipeline_sha256 <- .ship_pipeline(rec_dir)
  shipped_adnca <- .ship_adnca(rec_dir, adnca, original_file_path)

  # 2. Figure settings JSON
  tryCatch({
    settings_export <- list(
      schema_version = RECORD_SCHEMA_VERSION,
      app_version    = tryCatch(get("APP_VERSION", envir = globalenv()), error = function(e) "?"),
      r_version      = R.version.string,
      timestamp      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      analyst        = analyst,
      study_name     = study_name,
      analysis_type  = "Figure / Visualization",
      input_file     = original_file_name,
      data_sha256    = file_hash,
      pipeline_sha256 = pipeline_sha256,
      read_args      = if (is.null(read_args)) list() else read_args,
      column_mapping = col_map,
      blq_rule       = blq_rule,
      lloq           = lloq,
      visualization  = viz_settings,
      packages = list(
        ggplot2 = tryCatch(as.character(packageVersion("ggplot2")), error = function(e) "?"),
        dplyr   = tryCatch(as.character(packageVersion("dplyr")), error = function(e) "?")
      )
    )
    settings_export <- c(settings_export, shipped_adnca$json)
    .write_json(settings_export, file.path(rec_dir, "figure_settings.json"))
  }, error = function(e) warning("Could not create figure settings JSON: ", e$message))

  # 3. Reproducibility R script
  tryCatch({
    script <- generate_viz_script(viz_settings, col_map)
    writeLines(script, file.path(rec_dir, "reproduce_figure.R"))
  }, error = function(e) warning("Could not create figure R script: ", e$message))

  # 4. Integrity manifest (source data, figure settings, figure, pipeline code)
  tryCatch({
    write_integrity_manifest(rec_dir, c(list(
      "Source data"     = original_file_path,
      "Figure settings" = file.path(rec_dir, "figure_settings.json"),
      "Figure"          = file.path(rec_dir, paste0("figure.", fmt)),
      "Pipeline code"   = file.path(rec_dir, "nca_pipeline.R")
    ), shipped_adnca$manifest))
  }, error = function(e) warning("Could not create integrity file: ", e$message))

  # 5. Rebuild the figure now, so the user knows before download that it works
  verdict <- run_reproduction_check(rec_dir, "reproduce_figure.R", outputs = character(0),
                                    figure = paste0("reproduced_figure.", fmt))

  # 5. Provenance HTML
  tryCatch({
    html <- generate_viz_html(viz_settings, col_map, original_file_name,
                              file_hash, analyst, study_name, n_subjects, n_obs)
    writeLines(html, file.path(rec_dir, "figure_provenance.html"))
  }, error = function(e) warning("Could not create provenance HTML: ", e$message))

  zip_record_dir(rec_dir, output_path)
  unlink(rec_dir, recursive = TRUE)
  attr(output_path, "reproduction") <- verdict
  invisible(output_path)
}


# ============================================================================
# Reproduction scripts
# ============================================================================
# Every record ships nca_pipeline.R (an exact copy of R/pipeline.R) and a
# settings JSON. The scripts below contain no analysis logic of their own:
# they verify hashes, source the pipeline and call the same functions the
# app called, with the recorded settings. There is therefore nothing to
# transcribe and nothing that can drift from the app.

.script_header <- function(title, what, script_name) {
  paste0(
'# ============================================================================
# ', title, '
# ============================================================================
# Generated by NCA Assistant ', tryCatch(get("APP_VERSION", envir = globalenv()), error = function(e) ""),
' on ', format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), '
#
# ', what, '
# All analysis code is in nca_pipeline.R, the app\'s own pipeline, shipped
# with this record; all settings are read from the JSON file.
#
# To run: unzip the record, set the working directory to that folder, then
#   source("', script_name, '")
# ============================================================================
')
}

#' Reproduction script for batch and bioequivalence NCA records
generate_nca_script <- function() {
  paste0(.script_header("NCA Analysis Reproducibility Script",
                        "Re-runs the NCA and compares every parameter with the app's results.",
                        "reproduce_analysis.R"), r"---(
for (pkg in c("NonCompart", "jsonlite", "digest", "readxl")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(NonCompart)

rec <- jsonlite::fromJSON("analysis_settings.json", simplifyDataFrame = FALSE)

# 1. Integrity: the pipeline code and the data file must be the ones analysed
cat("Pipeline code:", if (identical(digest::digest(file = "nca_pipeline.R", algo = "sha256"),
                                    rec$pipeline_sha256)) "MATCH" else "MISMATCH", "\n")
source("nca_pipeline.R")
verify_file_hash(rec$input_file, rec$data_sha256, "Data file")

# 2. Read and prepare the data exactly as the app did
if (identical(rec$door, "adnca")) {
  # CDISC ADNCA import: convert again with the recorded choices (adnca_import.R)
  cat("ADNCA import code:", if (identical(digest::digest(file = "adnca_import.R", algo = "sha256"),
                                        rec$adnca_import_sha256)) "MATCH" else "MISMATCH", "\n")
  source("adnca_import.R")
}
inp <- read_record_input(rec)
ds  <- prepare_pk_dataset(inp$raw, rec$column_mapping,
                          list(lloq = rec$lloq, blq_rule = rec$blq_rule, read_args = inp$read_args))

# 3. NCA with the recorded settings and half-life overrides
result <- run_nca(ds$data, ds$col_map, record_nca_settings(rec, ds$data, ds$col_map),
                  lz_overrides = rec$lz_overrides)
write.csv(result, "reproduced_results.csv", row.names = FALSE)
cat("Profiles analysed:", nrow(result), "\n")

# 4. Compare with the app's results shipped in this record
compare_with_reference(result, "app_results_reference.csv")
if (!is.null(rec$reproduction_scope)) cat("Scope:", rec$reproduction_scope, "\n")
)---")
}

#' Reproduction script for single-subject NCA records
generate_single_nca_script <- function() {
  paste0(.script_header("Single-Subject NCA Reproducibility Script",
                        "Re-runs the single-profile NCA and compares it with the app's result.",
                        "reproduce_analysis.R"), r"---(
for (pkg in c("NonCompart", "jsonlite", "digest", "readxl")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(NonCompart)

rec <- jsonlite::fromJSON("analysis_settings.json", simplifyDataFrame = FALSE)

cat("Pipeline code:", if (identical(digest::digest(file = "nca_pipeline.R", algo = "sha256"),
                                    rec$pipeline_sha256)) "MATCH" else "MISMATCH", "\n")
source("nca_pipeline.R")
verify_file_hash(rec$input_file, rec$data_sha256, "Data file")

# The profile: from the uploaded file (prepared as in the app) or manual entry
if (identical(rec$data_source, "uploaded_file")) {
  if (identical(rec$door, "adnca")) {
    cat("ADNCA import code:", if (identical(digest::digest(file = "adnca_import.R", algo = "sha256"),
                                          rec$adnca_import_sha256)) "MATCH" else "MISMATCH", "\n")
    source("adnca_import.R")
  }
  inp  <- read_record_input(rec)
  ds   <- prepare_pk_dataset(inp$raw, rec$column_mapping,
                             list(lloq = rec$lloq, blq_rule = rec$blq_rule, read_args = inp$read_args))
  rows <- profile_data_rows(ds$data, ds$col_map, rec$subject)
  time <- ds$data[[ds$col_map$time]][rows]
  conc <- ds$data[[ds$col_map$conc]][rows]
} else {
  manual <- read.csv(rec$input_file)
  time <- manual$Time; conc <- manual$Concentration
}
cat("Profile:", rec$subject, "-", length(time), "time points\n")

settings <- list(admin_route = rec$admin_route, dose = rec$dose, infusion_duration = rec$infusion_dur,
                 is_steady_state = isTRUE(rec$steady_state), tau = rec$tau, dose_unit = rec$dose_unit,
                 time_unit = rec$time_unit, conc_unit = rec$conc_unit,
                 trap_method = rec$trap_method, mw = rec$mw,
                 r2adj_threshold = rec$r2adj_threshold, partial_aucs = partial_auc_spec(rec$partial_aucs))
time_used <- if (length(rec$lz_overrides) > 0) rec$lz_overrides[[1]]$time_used else NULL
result <- run_single_nca(time, conc, settings, time_used = time_used)
write.csv(data.frame(Parameter = names(result), Value = as.character(result)),
          "reproduced_results.csv", row.names = FALSE)

compare_with_reference(result, "app_results_reference.csv")
)---")
}

#' Run a record's reproduction script and store the outcome in the record
#'
#' Executed at export time so the user learns before download whether the
#' record reproduces. The script runs in a separate R process, in the record
#' folder, exactly as a recipient would run it; its output is written to
#' reproduction_check.txt and its own output files are removed again.
#'
#' @return verdict: "MATCH", "CLOSE", "DIFFERENT", "NOT COMPARED",
#'   "FIGURE CREATED" or "FAILED"
run_reproduction_check <- function(rec_dir, script, outputs = "reproduced_results.csv",
                                   figure = NULL) {
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- tryCatch({
    owd <- setwd(rec_dir); on.exit(setwd(owd), add = TRUE)
    suppressWarnings(system2(rscript, script, stdout = TRUE, stderr = TRUE, timeout = 300))
  }, error = function(e) paste("Could not run the script:", conditionMessage(e)))
  verdict <- if (!is.null(figure)) {
    if (file.exists(file.path(rec_dir, figure))) "FIGURE CREATED" else "FAILED"
  } else {
    v <- regmatches(out, regexpr("(?<=^Result: )[A-Z ]+", out, perl = TRUE))
    if (length(v) > 0) trimws(tail(v, 1)) else "FAILED"
  }
  lines <- c("Reproduction check", "==================", "",
             paste0("Performed: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                    ", by the app at export, running ", script, " in a separate R process."),
             paste0("Result: ", verdict), "",
             "This shows that the record reproduces the analysis with the shipped pipeline",
             "code and the recorded settings. It demonstrates reproducibility, not",
             "independent verification: the same algorithms and packages are re-executed.", "",
             "Script output:", "--------------", out)
  writeLines(lines, file.path(rec_dir, "reproduction_check.txt"))
  unlink(file.path(rec_dir, c(outputs, figure)))
  verdict
}

#' Write the uploaded table when the original upload file is no longer available
#'
#' The copy is a standard CSV (comma separator, point decimal mark). When the
#' upload used a decimal comma, decimal-comma numbers stored as text are
#' rewritten with a point first, so the copy reads back to the same values.
#' @return read arguments for the copy (always the defaults)
write_record_fallback <- function(raw, path, read_args = list()) {
  if (identical(read_args$dec, ",")) {
    for (cc in names(raw)) raw[[cc]] <- normalise_decimal_comma(raw[[cc]], ",")
  }
  utils::write.csv(raw, path, row.names = FALSE)
  list()
}

#' Copy the pipeline code into a record folder as nca_pipeline.R
#' @return SHA-256 of the copy
.ship_pipeline <- function(rec_dir) {
  src <- "R/pipeline.R"
  if (!file.exists(src)) stop("R/pipeline.R not found; the record cannot be made reproducible.")
  file.copy(src, file.path(rec_dir, "nca_pipeline.R"), overwrite = TRUE)
  sha256_or_na(file.path(rec_dir, "nca_pipeline.R"))
}

#' Ship the ADNCA import with a record: code, choices and conversion log
#'
#' @param adnca list(options, notes, sources, lloq) from the app's ADNCA upload,
#'   or NULL for a flat upload
#' @return list of JSON fields (door, adnca, adnca_import_sha256) and the
#'   manifest entries to add
.ship_adnca <- function(rec_dir, adnca, input_path) {
  if (is.null(adnca)) return(list(json = list(door = "flat"), manifest = list()))
  src <- "R/adnca_import.R"
  if (!file.exists(src)) stop("R/adnca_import.R not found; the ADNCA import cannot be reproduced.")
  file.copy(src, file.path(rec_dir, "adnca_import.R"), overwrite = TRUE)
  log_path <- file.path(rec_dir, "adnca_conversion_log.txt")
  writeLines(adnca_log_lines(adnca, input_path, adnca$n_records), log_path)
  list(json = list(door = "adnca", adnca = adnca$options,
                   adnca_import_sha256 = sha256_or_na(file.path(rec_dir, "adnca_import.R"))),
       manifest = list("ADNCA import code" = file.path(rec_dir, "adnca_import.R"),
                       "ADNCA conversion log" = log_path))
}

#' Add a sheet with the official CDISC code and name of each parameter
#'
#' The first rows state the Controlled Terminology release used and that this
#' is a code lookup, not an SDTM PP dataset.
add_cdisc_code_sheet <- function(wb, params, admin_route, is_ss, sheet = "CDISC_Parameter_Codes") {
  codes <- cdisc_pk_codes(params, admin_route, is_ss)
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, cdisc_ct_statement(), startRow = 1)
  openxlsx::writeData(wb, sheet, paste("Code lookup only: these results are not an SDTM PP dataset and",
                                       "no claim of conformance to CDISC standards is made."), startRow = 2)
  openxlsx::writeData(wb, sheet, codes, startRow = 4)
  invisible(codes)
}

#' The pinned CDISC release, for settings JSON
.cdisc_json <- function() {
  r <- cdisc_ct_release()
  r[c("Standard", "Release", "Codelists", "Source", "Source_SHA256")]
}

#' Write a settings JSON; digits = NA keeps full precision (doses, LLOQ,
#' half-life override times), which the reproduction needs
.write_json <- function(x, path) {
  writeLines(jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null"), path)
}

#' Generate a standalone R script that reproduces a Visualize-tab figure
#'
#' Rebuilds the concentration-time figure with ggplot2 from the original data
#' file, processed by the shipped copy of the app's pipeline (nca_pipeline.R)
#' with the recorded reading and BLQ settings, then saves it at the recorded
#' dimensions/resolution. Faithful to the core plot (geometry, grouping, scale,
#' dose-normalization, summary statistic); cosmetic theming is approximated.
#'
#' @param viz_settings The shared$viz_settings list
#' @param col_map Column mapping list
#' @return Character string containing the complete R script
generate_viz_script <- function(viz_settings, col_map) {

  vs <- viz_settings
  plot_type  <- vs$plot_type        %||% "spaghetti"
  y_scale    <- vs$y_scale          %||% "linear"
  color_by   <- vs$color_by         %||% "subject"
  summary_st <- vs$summary_statistic %||% "geomean"
  do_norm    <- isTRUE(vs$dose_normalized)
  width_in   <- vs$figure_width_in  %||% 7
  height_in  <- vs$figure_height_in %||% 5
  dpi        <- vs$dpi              %||% 300
  fmt        <- vs$export_format    %||% "png"

  has_treat <- !is.null(col_map$treatment)
  has_dose  <- !is.null(col_map$dose)

  # Map the colour-by choice to its source column
  color_col <- switch(color_by,
    "subject"   = col_map$subject,
    "treatment" = col_map$treatment %||% col_map$subject,
    "period"    = col_map$period    %||% col_map$subject,
    "sequence"  = col_map$sequence  %||% col_map$subject,
    col_map$subject)

  norm_section <- if (do_norm && has_dose) {
    paste0(
'# Dose-normalize concentration (C / Dose); non-positive doses become NA
dose_vals <- suppressWarnings(as.numeric(d[[', deparse(col_map$dose), ']]))
dose_vals[is.na(dose_vals) | dose_vals <= 0] <- NA
d$.conc <- d$.conc / dose_vals
y_label <- "Dose-normalized concentration (C/Dose)"
')
  } else {
    '\ny_label <- "Concentration"\n'
  }

  plot_section <- if (plot_type == "summary") {
    grp <- if (has_treat) paste0('c(".time", ', deparse(col_map$treatment), ')') else 'c(".time")'
    stat_code <- if (summary_st == "geomean") {
'# Geometric mean multiplied/divided by the geometric SD (positive concentrations only)
summ <- d[!is.na(d$.conc) & d$.conc > 0, ]
summ <- summ |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
  dplyr::summarise(
    .gm  = exp(mean(log(.conc))),
    .gcv = sqrt(exp(stats::var(log(.conc))) - 1) * 100,
    .y   = exp(mean(log(.conc))),
    .lo  = exp(mean(log(.conc)) - stats::sd(log(.conc))),
    .hi  = exp(mean(log(.conc)) + stats::sd(log(.conc))),
    .groups = "drop")
'
    } else {
'# Arithmetic mean +/- SD
summ <- d[!is.na(d$.conc), ] |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
  dplyr::summarise(
    .y  = mean(.conc),
    .lo = mean(.conc) - stats::sd(.conc),
    .hi = mean(.conc) + stats::sd(.conc),
    .groups = "drop")
'
    }
    paste0(
'grp_cols <- ', grp, '
', stat_code, '
p <- ggplot2::ggplot(summ, ggplot2::aes(x = .time, y = .y',
      if (has_treat) paste0(', colour = ', deparse(col_map$treatment),
                            ', group = ', deparse(col_map$treatment)) else "",
      ')) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = .lo, ymax = .hi), width = 0) +
  ggplot2::labs(x = "Time", y = y_label)
', if (!is.null(partial_auc_spec(vs$shade_partial_aucs))) paste0('
# Shaded partial AUC intervals (an end at t is drawn to the last time shown)
iv <- partial_auc_shading(rec$visualization$shade_partial_aucs, max(summ$.time, na.rm = TRUE),
                          c(summ$.lo, summ$.hi, summ$.y), log = ', identical(y_scale, "log"), ')
p$layers <- c(ggplot2::annotate("rect", xmin = iv$xmin, xmax = iv$xmax, ymin = iv$ymin, ymax = iv$ymax,
                                fill = "grey50", alpha = 0.15), p$layers)
') else "")
  } else {
    paste0(
'p <- ggplot2::ggplot(d, ggplot2::aes(x = .time, y = .conc,
       group = ', deparse(col_map$subject), ', colour = factor(', deparse(color_col), '))) +
  ggplot2::geom_line(alpha = 0.7) +
  ggplot2::geom_point(size = 1) +
  ggplot2::labs(x = "Time", y = y_label, colour = ', deparse(color_by), ')
')
  }

  scale_section <- if (y_scale == "log") {
    '\np <- p + ggplot2::scale_y_log10()  # semi-log; non-positive values dropped\n'
  } else ""

  paste0(
'# ============================================================================
# Figure Reproducibility Script
# ============================================================================
# Generated by NCA Assistant
# Date: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), '
#
# This script reproduces the concentration-time figure created in the
# Visualize Data tab, from the original data file, using the recorded settings.
#
# INSTRUCTIONS:
#   1. Keep this script, nca_pipeline.R, figure_settings.json and the data file
#      together in one folder (as unzipped)
#   2. setwd("path/to/your/folder")
#   3. source("reproduce_figure.R")  -> writes "reproduced_figure.', fmt, '"
# ============================================================================


# --- Step 1: Packages -------------------------------------------------------
for (pkg in c("ggplot2", "dplyr", "jsonlite", "digest")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}


# --- Step 2: Read and process the data with the app\'s own pipeline ---------
rec <- jsonlite::fromJSON("figure_settings.json", simplifyDataFrame = FALSE)
cat("Pipeline code:", if (identical(digest::digest(file = "nca_pipeline.R", algo = "sha256"),
                                    rec$pipeline_sha256)) "MATCH" else "MISMATCH", "\\n")
source("nca_pipeline.R")
verify_file_hash(rec$input_file, rec$data_sha256, "Data file")
if (identical(rec$door, "adnca")) {
  cat("ADNCA import code:", if (identical(digest::digest(file = "adnca_import.R", algo = "sha256"),
                                        rec$adnca_import_sha256)) "MATCH" else "MISMATCH", "\\n")
  source("adnca_import.R")
}
inp <- read_record_input(rec)
ds  <- prepare_pk_dataset(inp$raw, rec$column_mapping,
                          list(lloq = rec$lloq, blq_rule = rec$blq_rule, read_args = inp$read_args))


# --- Step 3: Assemble plotting frame ----------------------------------------
d <- ds$data
d$.time <- suppressWarnings(as.numeric(d[[', deparse(col_map$time), ']]))
d$.conc <- suppressWarnings(as.numeric(d[[', deparse(col_map$conc), ']]))
', norm_section, '
d <- d[!is.na(d$.time), ]


# --- Step 4: Build the figure -----------------------------------------------
# Plot type: ', plot_type, ' | Y-axis: ', y_scale, '
', plot_section, scale_section, '
p <- p + ggplot2::theme_bw()


# --- Step 5: Save -----------------------------------------------------------
ggplot2::ggsave("reproduced_figure.', fmt, '", plot = p, device = "', fmt, '",
                width = ', width_in, ', height = ', height_in, ', dpi = ', dpi, ', units = "in")
cat("Saved reproduced_figure.', fmt, '\\n")

# --- Step 6: Compare with the app figure ------------------------------------
# A figure is compared visually rather than numerically: open
# "reproduced_figure.', fmt, '" next to "figure.', fmt, '" (shipped in this record)
# and confirm they match. The data file SHA-256 in data_integrity.txt confirms
# the underlying data is identical.
if (file.exists("figure.', fmt, '")) {
  cat("Compare reproduced_figure.', fmt, ' with figure.', fmt, ' (bundled) - they should match.\\n")
} else {
  cat("Bundled figure.', fmt, ' not found alongside the script; compare against the record copy.\\n")
}
')
}
