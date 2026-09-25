# NCA Assistant

**Version 1.7.0** | Designed by Rob ter Heine

A free, open-source Shiny application for non-compartmental pharmacokinetic analysis (NCA), bioequivalence testing, study planning and figures. It is written for people who run such an analysis now and then rather than daily: parameters carry plain-language names, the app says what it did and what it refused to do, and any analysis can be exported as a package that re-runs itself. Built by the [Radboud Applied Pharmacometrics](https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics) research group at Radboudumc, Nijmegen, The Netherlands.


---

## What It Does

Six workflow paths, each usable on its own, from one hub screen:

**1. Plan a Study.** Sample size and power, computed with PowerTOST: average bioequivalence, the scaled methods for highly variable drugs (EMA ABEL, FDA RSABE), and the FDA method for narrow therapeutic index drugs. The scaled methods use the within-subject CV of the Test as well as the Reference. Every design offered here can also be analysed in Bioequivalence Testing, under the same name; the reverse does not hold, because ABEL and RSABE need a replicate design, NTID needs the 4-period replicate, and a fixed-order paired comparison cannot be planned as a bioequivalence study at all. The CV can be taken from a bioequivalence analysis of your own data. Power curves and a CV sensitivity plot are drawn interactively.

**2. Upload & Check Data.** Reads CSV and Excel files, comma or semicolon separated, with a point or a decimal comma, and recognises the usual column names, including export conventions and non-English ones. CDISC ADNCA datasets have their own upload option (see below). You set the LLOQ and one of six BLQ rules, applied per profile; text such as `<0.5`, BLQ, BQL, BLOQ, ND and NQ counts as below the limit. More than 30 data quality checks then run, among them safety checks that refuse data the app cannot analyse safely: mixed units, dates or clock times in the time column, time counted from the first dose instead of the dose of each period, several profiles stacked in one column, and subject IDs that restart in each sequence. A second dose inside one profile gets a warning. A crossover without a Period column is analysed but gets no verdict. Units stated in the file are pre-selected, and a unit choice that contradicts them is refused.

**3. Visualize Data.** Concentration-time figures straight from the uploaded data: individual profiles with a choice of colour grouping, and geometric mean ×/÷ geometric SD curves with treatment overlays for crossover data. Export as PNG, PDF or SVG at up to 600 DPI. The app drafts a figure legend to paste into a manuscript, and can shade the partial AUC intervals of the last analysis on the summary plot.

**4. Analyze One Subject at a Time.** Step through the profiles with Previous and Next, or type the data in by hand. The half-life inspector lets you choose the terminal-phase points yourself, the dose is filled in from the data for each profile, and partial AUCs over intervals you enter appear beside the other parameters.

**5. Analyze All Subjects (Batch).** One run over every profile, a profile being one subject, treatment and period, so the two administrations of a replicate design stay apart. You get summary statistics per treatment, a profile grid, spaghetti and mean ± SD plots, a half-life review, and steady-state analysis from the dosing interval you enter (AUCτ, average concentration, fluctuation and swing). Where the automatic terminal fit falls below the minimum adjusted R² (0.70 by default), that profile gets no half-life and none of the parameters derived from it, unless you pick the points yourself. Partial AUCs from your protocol, ending at a time or at the last measurable concentration, join the table, the summary statistics and the downloads, with the highest observed concentration in each interval and its time when you ask for them.

**6. Bioequivalence Testing.** NCA, then the ANOVA, then the confidence interval (90% by default), a forest plot and the conclusion. The model is EMA Method A, with sequence, subject, period and treatment as fixed effects; Method B, with subject as a random effect, is offered as well. You choose which treatment is the Reference. Cmax and AUC0–t are compared by default, following ICH M13A. The conclusion uses confidence limits rounded to two decimals, and for limits wider than 80–125% it also asks for the point estimate inside 80.00–125.00%. Wider limits apply to Cmax only, unless you choose Cmax and the partial AUCs, or all metrics. A verdict is given only for a 90% confidence interval, and not when the pre-specified mixed model cannot be fitted. Designs: 2×2 crossover, 2×2×3 and 2×2×4 full replicate, 2×3×3 partial replicate, parallel groups, and the paired comparison for a fixed order, which yields a ratio but no verdict. For replicate designs the within-subject variability of Reference and Test, and the EMA limits it would imply, are shown for information only: this app does average bioequivalence, not reference-scaled (ABEL, RSABE) or NTID analyses. Partial AUCs and the maximum concentration within an interval can be compared too, with a verdict for the intervals you mark pivotal and a ratio with its confidence interval for the supportive ones. The app also checks a few ICH M13A points: a pre-dose concentration above 5% of Cmax, fewer than 12 subjects, and low AUC coverage. For parallel groups a Welch interval is shown as a sensitivity analysis. Results agree with the replicateBE package on all 30 of its reference data sets.

Alongside the paths: a **Statistical Methods** page with wording to adapt for a manuscript, a **Data Preparation Guide** of 12 tabs (one per study type, plus real laboratory data and common mistakes), and an **About** page with the package list and the version history.

---

## Intended use

NCA Assistant is for pharmacokineticists doing non-compartmental analysis, average bioequivalence testing and study planning. It gives no reference-scaled bioequivalence verdict (ABEL, RSABE), and it has no audit trail, electronic signature or access control. The public instance on shinyapps.io is for evaluation and training, with synthetic or pseudonymised data. For regulated work, install a tagged release on your own system and qualify it there with the validation package. Responsibility for the analysis and its conclusions stays with the user. Not for dosing decisions for individual patients.

**Your data.** On the public instance, uploads are processed on shinyapps.io servers run by Posit PBC (USA). Upload only synthetic, example or anonymised data there. Pseudonymised trial data are still personal data under the GDPR. Sending them to a third-party host needs agreements your organisation must have in place, and may breach sponsor confidentiality. For real study data, run the app on your own computer.

---

## CDISC data and terminology

- **ADNCA datasets:** set *What kind of file?* to **CDISC ADNCA dataset** on the Upload page. The app shows a summary, asks which time variable (NRRLT, ARRLT or MRRLT) and analyte to use, applies ANL01FL, refuses derived records (DTYPE) and other data it cannot convert safely, and stores the choices in the Analysis Record. The standalone converter [`converters/adnca_to_flat.R`](converters/adnca_to_flat.R) does the same outside the app; see [`converters/ADNCA_TO_FLAT.md`](converters/ADNCA_TO_FLAT.md), which also gives a recipe for SAS transport (`.xpt`) files, which are not read directly.
- **Parameter codes:** results, downloads and Analysis Records list the official CDISC PK parameter code of each parameter from CDISC SDTM Controlled Terminology release 2026-03-27 ([`cdisc/`](cdisc/)). A partial AUC maps to AUCINT, with the interval start and end named for PPSTINT and PPENINT. This is a code lookup; the results are not SDTM PP datasets.

NCA Assistant has not been checked against a specific version of the ADNCA Implementation Guide and is not affiliated with, endorsed by, or certified by CDISC.

---

## Complete Analysis Record

**One Subject at a Time**, **All Subjects (Batch)** and **Bioequivalence** can generate a **Complete Analysis Record**: a self-contained zip file for archiving, publication supplements, and inclusion in a sponsor's study documentation. It documents one analysis; it is not an audit trail, and it is not signed. A consistently placed *Generate Analysis Record* panel (with an explanatory tooltip) appears on each tab once results exist.

For the NCA and bioequivalence paths the record contains:

- **results.xlsx** — Individual NCA parameters, summary statistics, and (for BE) confidence intervals and ANOVA tables
- **app_results_reference.csv** — The app's computed results in machine-readable form, used by the reproduction script for an automated comparison
- **analysis_settings.json** — Every setting that affects the analysis (including per-profile doses and, for bioequivalence, the design, Reference treatment, model, confidence level, limits and point-estimate constraint), with package versions, schema version, timestamp, the partial AUC intervals and their roles, and (if used in the same session) visualization settings
- **nca_pipeline.R** — The app's own data-processing code, so the reproduction runs exactly the code the app used
- **reproduce_analysis.R** — Standalone R script that reproduces the exact analysis without the app. It re-checks the source-data SHA-256 against the recorded value, and **automatically compares** its output against `app_results_reference.csv`, printing a `MATCH` / `DIFFERENT` verdict. A changed source file or a parameter present on one side only also counts as `DIFFERENT`. The app runs this script when it creates the record and stores the outcome in `reproduction_check.txt`
- **data_integrity.txt** — Three-way SHA-256 manifest fingerprinting the **source data, the analysis settings, and the results**, so every artefact is independently verifiable (source data → settings → results)
- **analysis_summary.html** — Self-contained summary with statistical methods, software environment, and instructions
- **Original data file** — Copy included so the package is self-contained

**Visualize Data** produces an equivalent **Figure Record**: the exported figure, `figure_settings.json`, a `reproduce_figure.R` script that rebuilds the plot from the data, a three-way integrity manifest (source data, figure settings, figure), an HTML provenance summary, and a copy of the original data.

---

## Requirements

- R ≥ 4.1.0
- Required packages: NonCompart, PowerTOST, nlme, shiny, bslib, shinyWidgets, htmltools, plotly, DT, readxl, dplyr, tidyr, ggplot2, openxlsx, jsonlite, digest
- For validation only: replicateBE (reference implementation for the replicate-design checks)

---

## Quick Start

### Option A: Run locally

Clone the repository and install dependencies:

```r
source("install_and_run.R")
```

Or manually:

```r
shiny::runApp()
```

### Option B: shinyapps.io

Available at [robterheine.shinyapps.io/NCAassistant](https://robterheine.shinyapps.io/NCAassistant/), for evaluation and training. This instance is outside the scope of the validation package: its package versions are set by the deployment, not by `validation/renv.lock`.

---

## Repository Layout

| Path | What is in it | What it is for |
|---|---|---|
| [`app.R`](app.R) | The Shiny app: UI shell, navigation, the About page and `APP_VERSION` | Entry point. `shiny::runApp()` starts here, and it sources everything in `R/` |
| [`R/`](R/) | 21 files: one module per workflow path (`mod_path_*.R`), the Shiny-free analysis pipeline (`pipeline.R`), bioequivalence statistics (`be_analysis.R`), Analysis Records (`export_record.R`), data checks, help text and the Statistical Methods page | All application code. `pipeline.R` is deliberately free of Shiny, so the validation suite and every Analysis Record can run it outside the app |
| [`data/`](data/) | Six small example datasets (theophylline, crossover, parallel, replicate, ADNCA, BLQ results) | The example files the Data Preparation Guide offers for download, and the datasets the worked examples in the manual use |
| [`cdisc/`](cdisc/) | One pinned release of CDISC SDTM Controlled Terminology: the release metadata, the extracted PK parameter terms, the map from app parameters to PPTESTCD, and the extractor script | Lets results, downloads and records state the official CDISC code of each parameter, from one stated release. A code lookup only: the app produces no SDTM PP datasets |
| [`converters/`](converters/) | `adnca_to_flat.R` and its documentation | Converts a CDISC ADNCA dataset to a flat CSV outside the app, for scripted use. It calls the same conversion code as the app's ADNCA upload, so both give the same result. The app itself never loads this folder |
| [`validation/`](validation/) | The validation package: the test script, the URS and IQ/OQ/PQ documents, the protocol generator, the release manifest and package lockfile, and `fixtures/` with committed test data and their deterministic generators | Qualification evidence. `fixtures/` is required to run the suite; see [`validation/README.md`](validation/README.md) |
| [`www/`](www/) | The user manual PDF, the stylesheet and the logo | Files the app serves to the browser. The manual link in the header points here |
| [`install_and_run.R`](install_and_run.R) | Dependency installation and launch | One-step setup for a new machine |
| `NCA_Assistant_User_Manual_v1.8.docx` | The manual source | Edited in Word; the PDF in `www/` is exported from it |

---

## Validation

A consolidated validation package is available in [`validation/`](validation/), following a risk-based approach consistent with ICH Q9 and GAMP 5 Category 5 principles.

**Single validation script** — run from the project root:

```bash
Rscript validation/validation.R
```

This executes 431 automated tests (plus 49 manual tests defined for a running app) and writes a results CSV with per-section results and URS traceability, and an environment file with the R and package versions and the SHA-256 of every tested file. Each release also ships a manifest of file hashes and a package lockfile (`validation/release_manifest.csv`, `validation/renv.lock`), which the installation checks compare against.

**Validation deliverables:**

- **User Requirement Specification** ([`validation/NCA_Assistant_URS.docx`](validation/NCA_Assistant_URS.docx)) — 69 requirements across 8 categories (GEN, DAT, NCA, BE, PWR, EXP, UI, VIZ), with a hazard-based FMEA, supplier assessment, and change control procedures
- **IQ/OQ/PQ Protocol** ([`validation/NCA_Assistant_IQOQPQ.docx`](validation/NCA_Assistant_IQOQPQ.docx)) — every automated and manual test listed individually with method, expected result, URS cross-reference, and criticality classification
- **Consolidated test script** ([`validation/validation.R`](validation/validation.R)) — automated tests + manual test definitions, covering IQ, data handling, NCA accuracy, bioequivalence, power/sample size, export/reproducibility, usability, and visualization (URS-VIZ)

NCA accuracy is checked against analytical ground truth (mono-exponential IV bolus) and R's built-in Theoph and Indometh datasets; bioequivalence results against `replicateBE` (30 reference data sets) and sample sizes against PowerTOST; every Analysis Record type is checked to reproduce. Every test is classified CRITICAL (failure blocks qualification) or SUPPORTIVE (failure requires risk assessment). Visualization tests are classified SUPPORTIVE (non-critical: graphical output does not affect NCA parameters or regulatory conclusions).

See [`validation/README.md`](validation/README.md) for detailed instructions on running the validation and adapting it for your organisation.

---

## Citation

> ter Heine R. NCA Assistant (v1.7.0). Radboud Applied Pharmacometrics, Radboudumc, Nijmegen, The Netherlands. https://github.com/robterheine/ncaassistant

> Kim H, Han S, Cho YS, Yoon SK, Bae KS. Development of R packages: 'NonCompart' and 'ncar' for noncompartmental analysis (NCA). *Transl Clin Pharmacol*. 2018;26(1):10-15.

---

## License

Copyright (C) 2026 Rob ter Heine.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version (GPL-3.0-or-later). It is distributed without any warranty. See [LICENSE](LICENSE) for the full text.

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen*
