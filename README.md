# NCA Assistant

**Version 1.8.0** | Designed by Rob ter Heine

A free, open-source Shiny application for non-compartmental pharmacokinetic analysis (NCA), bioequivalence testing, study planning and figures. It is written for people who run such an analysis now and then rather than daily: parameters carry plain-language names, the app says what it did and what it refused to do, and any analysis can be exported as a package that re-runs itself. Built by the [Radboud Applied Pharmacometrics](https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics) research group at Radboudumc, Nijmegen, The Netherlands.


---

## What It Does

Six workflow paths, each usable on its own, from one hub screen:

**1. Plan a Study.** Sample size and power, computed with PowerTOST: average bioequivalence, the scaled methods for highly variable drugs (EMA ABEL, FDA RSABE), and the FDA method for narrow therapeutic index drugs. The scaled methods use the within-subject CV of the Test as well as the Reference. Every design offered here can also be analysed in Bioequivalence Testing, under the same name; the reverse does not hold, because ABEL and RSABE need a replicate design, NTID needs the 4-period replicate, and a fixed-order paired comparison cannot be planned as a bioequivalence study at all. The CV can be taken from a bioequivalence analysis of your own data. Power curves and a CV sensitivity plot are drawn interactively.

**2. Upload & Check Data.** Reads CSV and Excel files, comma or semicolon separated, with a point or a decimal comma, and recognises the usual column names, including export conventions and non-English ones. CDISC ADNCA datasets have their own upload option (see below). You set the LLOQ and one of six BLQ rules, applied per profile; text such as `<0.5`, BLQ, BQL, BLOQ, ND and NQ counts as below the limit. More than 30 data quality checks then run, among them safety checks that refuse data the app cannot analyse safely: mixed units, dates or clock times in the time column, time counted from the first dose instead of the dose of each period, several profiles stacked in one column, subject IDs that restart in each sequence, and a decimal point in a decimal-comma file. A second dose inside one profile gets a warning. A crossover without a Period column is analysed but gets no verdict. Units stated in the file are pre-selected, and a unit choice that contradicts them is refused. The six example datasets load with one click, and each can be downloaded to see how the file is laid out. After processing you can leave out a sample or a whole profile, but only with a reason (vomiting, a dosing or protocol deviation, an invalid bioanalytical result and the like). Exclusions can be restored, are shown in every path and are listed in the downloads and the Analysis Record.

**3. Visualize Data.** Concentration-time figures straight from the uploaded data: individual profiles with a choice of colour grouping, and geometric mean ×/÷ geometric SD curves with treatment overlays for crossover data. Export as PNG, PDF or SVG at up to 600 DPI. The app drafts a figure legend to paste into a manuscript, and can shade the partial AUC intervals of the last analysis on the summary plot.

**4. Analyze One Subject at a Time.** Step through the profiles one by one, or type the data in by hand. You can choose the terminal-phase points yourself, the dose is filled in from the data for each profile, and partial AUCs over intervals you enter appear beside the other parameters.

**5. Analyze All Subjects (Batch).** One run over every profile, a profile being one subject, treatment and period, so the two administrations of a replicate design stay apart. You get summary statistics per treatment, a profile grid, spaghetti and mean ± SD plots, a half-life review, and steady-state analysis from the dosing interval you enter (AUCτ, average concentration, Cmin, the concentration at τ, fluctuation and swing; parameters extrapolated to infinity are left empty at steady state). Where the automatic terminal fit falls below the minimum adjusted R² (0.70 by default), that profile gets no half-life and none of the parameters derived from it, unless you pick the points yourself. Partial AUCs from your protocol, ending at a time or at the last measurable concentration, join the table, the summary statistics and the downloads, with the highest observed concentration in each interval and its time when you ask for them. Every output column has a plain-language label, and a unit where it has one. Each half-life fit is also checked against three rules you can edit: the fitted points should span at least two half-lives, and AUC to infinity should be at most 20% extrapolated (and, after an IV bolus, at most 20% back-extrapolated). A fit that breaks a rule is flagged; no value changes.

**6. Bioequivalence Testing.** NCA, then the ANOVA, then the confidence interval (90% by default), a forest plot and the conclusion. The model is EMA Method A, with sequence, subject, period and treatment as fixed effects; Method B, with subject as a random effect, is offered as well. You choose which treatment is the Reference. Cmax and AUC0–t are compared by default, following ICH M13A. The conclusion uses confidence limits rounded to two decimals, and for limits wider than 80–125% it also asks for the point estimate inside 80.00–125.00%. Wider limits apply to Cmax only, unless you choose Cmax and the partial AUCs, or all metrics. A verdict is given only for a 90% confidence interval, and not when the pre-specified mixed model cannot be fitted. 

Designs: 2×2 crossover, 2×2×3 and 2×2×4 full replicate, 2×3×3 partial replicate, parallel groups, and the paired comparison for a fixed order, which yields a ratio but no verdict. For replicate designs the within-subject variability of Reference and Test, and the EMA limits it would imply, are shown for information only: this app does average bioequivalence, not reference-scaled (ABEL, RSABE) or NTID analyses. Partial AUCs and the maximum concentration within an interval can be compared too, with a verdict for the intervals you mark pivotal and a ratio with its confidence interval for the supportive ones. The app also checks a few ICH M13A points: a pre-dose concentration above 5% of Cmax, fewer than 12 subjects, low AUC coverage, a period without measurable concentrations (counted as missing, not dropped silently) and a period with very low exposure. When the data include a period M13A excludes, the app says the verdict is not the M13A primary analysis. For parallel groups a Welch interval is shown as a sensitivity analysis. When you have excluded data, the comparison is repeated without the exclusions and shown next to the primary result. Results agree with the replicateBE package on all 30 of its reference data sets.

Alongside the paths: a **Statistical Methods** page with wording to adapt for a manuscript, a **Data Preparation Guide** of 12 tabs (one per study type, plus real laboratory data and common mistakes) with six example files, an **About** page with the package list and the version history, and the **User Manual** (PDF, in the navigation bar), with seven tutorials, the study types, how to read the results, and a chapter on regulated use.

---

## Intended use

NCA Assistant is for pharmacokineticists doing non-compartmental analysis, average bioequivalence testing and study planning. It gives no reference-scaled bioequivalence verdict (ABEL, RSABE). The public instance and a standard installation have no audit trail, electronic signature or access control; a controlled installation on your own server adds them (see [Controlled mode](#controlled-mode)). The public instance on shinyapps.io is for evaluation and training, with synthetic or pseudonymised data. For regulated work, install a tagged release on your own system and qualify it there with the validation package. Responsibility for the analysis and its conclusions stays with the user. Not for dosing decisions for individual patients.

**Your data.** On the public instance, uploads are processed on shinyapps.io servers run by Posit PBC (USA). Upload only synthetic, example or anonymised data there. Pseudonymised trial data are still personal data under the GDPR. Sending them to a third-party host needs agreements your organisation must have in place, and may breach sponsor confidentiality. For real study data, run the app on your own computer.

---

## CDISC data and terminology

- **ADNCA datasets:** set *What kind of file?* to **CDISC ADNCA dataset** on the Upload page. The app shows a summary, asks which time variable (NRRLT, ARRLT or MRRLT) and analyte to use, applies ANL01FL, refuses derived records (DTYPE) and other data it cannot convert safely, and stores the choices in the Analysis Record. The standalone converter [`converters/adnca_to_flat.R`](converters/adnca_to_flat.R) does the same outside the app; see [`converters/ADNCA_TO_FLAT.md`](converters/ADNCA_TO_FLAT.md), which also gives a recipe for SAS transport (`.xpt`) files, which are not read directly.
- **Parameter codes:** results, downloads and Analysis Records list the official CDISC PK parameter code of each parameter from CDISC SDTM Controlled Terminology release 2026-03-27 ([`cdisc/`](cdisc/)). A partial AUC maps to AUCINT, with the interval start and end named for PPSTINT and PPENINT. This is a code lookup; the results are not SDTM PP datasets.

NCA Assistant has not been checked against a specific version of the ADNCA Implementation Guide and is not affiliated with, endorsed by, or certified by CDISC.

---

## Complete Analysis Record

**One Subject at a Time**, **All Subjects (Batch)** and **Bioequivalence** can generate a **Complete Analysis Record**: a self-contained zip file for archiving, publication supplements, and inclusion in a sponsor's study documentation. It documents one analysis. On the public instance and in a standard installation it is not an audit trail, and it is not signed; in controlled mode the record is also stored on the server and reviewed there (see [Controlled mode](#controlled-mode)). The *Generate Analysis Record* panel appears once there are results.

For the NCA and bioequivalence paths the record contains:

- **results.xlsx**: individual NCA parameters, summary statistics, (for BE) confidence intervals and ANOVA tables, and a Checks sheet with the data quality findings and the notes shown with the results, and an Exclusions sheet (for BE with exclusions also the comparison without them)
- **app_results_reference.csv**: the app's computed results in machine-readable form, used by the reproduction script for an automated comparison
- **analysis_settings.json**: every setting that affects the analysis (including per-profile doses and, for bioequivalence, the design, Reference treatment, model, confidence level, limits and point-estimate constraint), with package versions, schema version, timestamp, the partial AUC intervals and their roles, the half-life rules, the exclusions with their reasons, and (if used in the same session) visualization settings
- **nca_pipeline.R**: the app's own data-processing code, so the reproduction runs exactly the code the app used
- **reproduce_analysis.R**: a standalone R script that reproduces the analysis without the app. It re-checks the source-data SHA-256 against the recorded value and **compares** its output with `app_results_reference.csv`, printing `MATCH`, `CLOSE`, `DIFFERENT` or `NOT COMPARED`. A changed source file or a parameter present on one side only also counts as `DIFFERENT`. The app runs this script when it creates the record and stores the outcome in `reproduction_check.txt`. For a bioequivalence record it recomputes the NCA parameters; the ANOVA, confidence intervals and verdict are recorded in results.xlsx but not recomputed
- **data_integrity.txt**: SHA-256 hashes of the source data, the analysis settings, the results, the reference results, the pipeline code and the reproduction script. The manifest is not signed: store the zip, or its hash, in a controlled system. In controlled mode the note says where the record's audit trail and review signature are kept
- **analysis_summary.html**: a self-contained summary with statistical methods, software environment, checks and notes, and instructions
- **Original data file**: a copy, so the package is self-contained. When the uploaded file was no longer available, the record holds the table as the app read it, written as CSV, and says so

**Visualize Data** produces an equivalent **Figure Record**: the exported figure, `figure_settings.json`, a `reproduce_figure.R` script that rebuilds the plot from the data, an integrity manifest (source data, figure settings, figure, pipeline code and script), an HTML provenance summary, and a copy of the original data.

---

## Controlled mode

For regulated work, NCA Assistant can run in **controlled mode** on a server your organisation runs (Shiny Server behind HTTPS, in an environment where two-factor sign-in is standard). Controlled mode is off unless the environment variable `NCA_GXP_DIR` points to a controlled directory; without it, the app behaves exactly as described above. In controlled mode:

- **Access control.** Everyone signs in with a personal account (via `shinymanager`): *analyst*, *reviewer* or read-only *inspector*. Passwords follow a policy (12 characters, expiry, lockout, inactivity timeout); new accounts start with the one-time password `admin`, which must be changed at first sign-in.
- **Audit trail.** Every sign-in, data load, analysis run, download, record and signature is written to a hash-chained SQLite trail that refuses changes, with user, role, organisation and UTC time. If an entry cannot be written, the action does not happen. Attempted misuse is also reported to the server's system log.
- **Records and signatures.** Every Analysis Record is stored read-only under its SHA-256. A reviewer approves or rejects it on the **Records** page with user ID and password, after seeing the history of the data; signed records download together with a signature sheet.
- **Review.** The **Audit trail** page offers exceptions, filters, chain verification, CSV export, a users overview and a signed trail review.
- **Administration.** `gxp/manage_users.R` adds, changes, resets and deactivates accounts, and archives the trail and records together with the software to restore them.

Setting up a controlled installation, and what stays the organisation's responsibility, is described in the user manual (chapter *Working on a Controlled Installation*, and the appendix on setting up and administering a controlled installation). A local installation can run controlled mode for training, but is not a qualified setup.

---

## Requirements

- R ≥ 4.1.0
- Required packages: NonCompart, PowerTOST, nlme, shiny, bslib, shinyWidgets, htmltools, plotly, DT, readxl, dplyr, tidyr, ggplot2, openxlsx, jsonlite, digest
- For validation only: replicateBE (reference implementation for the replicate-design checks)
- For controlled mode only: shinymanager, DBI, RSQLite, and on the server the system programs `zip` and `logger`

---

## Quick Start

### Option A: Run locally

Download a tagged release from [Releases](https://github.com/Robterheine/NCAassistant/releases) (or clone the repository), then install the dependencies and start the app from its folder:

```r
source("install_and_run.R")
```

For a qualified installation, install the package versions the release was validated with (`validation/renv.lock`) instead of the latest ones from CRAN:

```bash
Rscript install_and_run.R --validated
```

With the packages in place, `shiny::runApp()` starts the app. The user manual's Getting Started chapter gives the steps in more detail.

### Option B: shinyapps.io

Available at [robterheine.shinyapps.io/NCAassistant](https://robterheine.shinyapps.io/NCAassistant/), for evaluation and training. This instance is outside the scope of the validation package: its package versions are set by the deployment, not by `validation/renv.lock`.

### Option C: Your own server (controlled mode)

For regulated work, install a tagged release on Shiny Server behind nginx with HTTPS, with `--validated` package versions, and switch on [controlled mode](#controlled-mode) through the settings in the service account's `.Renviron`. Appendix E of the user manual gives the steps and example settings for Shiny Server, nginx (WebSocket forwarding, a one-hour read timeout, uploads up to 50 MB), `.Renviron` and sudoers, and the account commands (`gxp/manage_users.R`). Qualify the installation with the IQ/OQ/PQ protocol, including its checks on the server (section 2.1).

To try controlled mode without a server, for example for training or Tutorial 8, run it on your own computer from a separate copy of the app folder with its own `.Renviron`. The appendix's *A Training Installation* has the steps for macOS, Linux and Windows. Such an installation works the same way but is not qualified.

---

## Repository Layout

| Path | What is in it | What it is for |
|---|---|---|
| [`app.R`](app.R) | The Shiny app: UI shell, navigation, the About page and `APP_VERSION` | Entry point. `shiny::runApp()` starts here, and it sources everything in `R/` |
| [`R/`](R/) | 24 files: one module per workflow path (`mod_path_*.R`), the Shiny-free analysis pipeline (`pipeline.R`), bioequivalence statistics (`be_analysis.R`), Analysis Records (`export_record.R`), data checks, help text, the Statistical Methods page, and controlled mode (`gxp_audit.R`, `gxp_access.R`, `gxp_sign.R`) | All application code. `pipeline.R` is deliberately free of Shiny, so the validation suite and every Analysis Record can run it outside the app |
| [`data/`](data/) | Six small example datasets (theophylline, crossover, parallel, replicate, ADNCA, BLQ results) | The example files the Data Preparation Guide offers for download, and the datasets the worked examples in the manual use |
| [`cdisc/`](cdisc/) | One pinned release of CDISC SDTM Controlled Terminology: the release metadata, the extracted PK parameter terms, the map from app parameters to PPTESTCD, and the extractor script | Lets results, downloads and records state the official CDISC code of each parameter, from one stated release. A code lookup only: the app produces no SDTM PP datasets |
| [`converters/`](converters/) | `adnca_to_flat.R` and its documentation | Converts a CDISC ADNCA dataset to a flat CSV outside the app, for scripted use. It calls the same conversion code as the app's ADNCA upload, so both give the same result. The app itself never loads this folder |
| [`validation/`](validation/) | The validation package: the test script, the URS and IQ/OQ/PQ documents, the protocol generator, the release manifest and package lockfile, and `fixtures/` with committed test data and their deterministic generators | Qualification evidence. `fixtures/` is required to run the suite; see [`validation/README.md`](validation/README.md) |
| [`gxp/`](gxp/) | `manage_users.R` | Account administration and archiving for controlled mode, run by the system owner on the server |
| [`www/`](www/) | The user manual PDF, the stylesheet, the logo and `gxp_activity.js` (controlled mode only) | Files the app serves to the browser. The manual link in the header points here |
| [`install_and_run.R`](install_and_run.R) | Dependency installation and launch | One-step setup for a new machine |
| `NCA_Assistant_User_Manual_v1.9.docx` | The manual source | Edited in Word; the PDF in `www/` is exported from it |

---

## Validation

The validation package in [`validation/`](validation/) follows a risk-based approach in line with ICH Q9 and GAMP 5 Category 5.

**Single validation script**, run from the project root:

```bash
Rscript validation/validation.R
```

This executes 526 automated tests (plus 65 manual tests defined for a running app) and writes a results CSV with per-section results and URS traceability, and an environment file with the R and package versions and the SHA-256 of every tested file. Each release also ships a manifest of file hashes and a package lockfile (`validation/release_manifest.csv`, `validation/renv.lock`), which the installation checks compare against.

**Validation deliverables:**

- **User Requirement Specification** ([`validation/NCA_Assistant_URS.docx`](validation/NCA_Assistant_URS.docx)): 92 requirements across 9 categories (GEN, DAT, NCA, BE, PWR, EXP, UI, VIZ, GXP), with a hazard-based FMEA, supplier assessment, and change control procedures
- **IQ/OQ/PQ Protocol** ([`validation/NCA_Assistant_IQOQPQ.docx`](validation/NCA_Assistant_IQOQPQ.docx)): approval before execution, a checklist for adopting organisations, every automated and manual test listed individually with method, expected result, URS cross-reference and criticality, and a template for the user's own PQ
- **Consolidated test script** ([`validation/validation.R`](validation/validation.R)): automated tests and manual test definitions, covering IQ, data handling, NCA accuracy, bioequivalence, power/sample size, export/reproducibility, usability, visualization (URS-VIZ) and controlled mode (URS-GXP)

NCA accuracy is checked against analytical ground truth (mono-exponential IV bolus) and R's built-in Theoph and Indometh datasets; bioequivalence results against `replicateBE` (30 reference data sets) and sample sizes against PowerTOST; every Analysis Record type is checked to reproduce. Every test is CRITICAL (a failure blocks qualification) or SUPPORTIVE (a failure needs a risk assessment). The visualization tests are SUPPORTIVE, because a figure does not change NCA parameters or conclusions.

See [`validation/README.md`](validation/README.md) for detailed instructions on running the validation and adapting it for your organisation.

---

## Citation

> ter Heine R. NCA Assistant (v1.8.0). Radboud Applied Pharmacometrics, Radboudumc, Nijmegen, The Netherlands. https://github.com/Robterheine/NCAassistant

> Kim H, Han S, Cho YS, Yoon SK, Bae KS. Development of R packages: 'NonCompart' and 'ncar' for noncompartmental analysis (NCA). *Transl Clin Pharmacol*. 2018;26(1):10-15.

---

## License

Copyright (C) 2026 Rob ter Heine.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version (GPL-3.0-or-later). It is distributed without any warranty. See [LICENSE](LICENSE) for the full text.

*Radboud Applied Pharmacometrics, Radboudumc, Nijmegen*
