# NCA Assistant

**Version 1.4.0** | Designed by Rob ter Heine

A freely available, open-source Shiny application for pharmacokinetic non-compartmental analysis (NCA), bioequivalence testing, study planning, and data visualization. Developed by the [Radboud Applied Pharmacometrics](https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics) research group at Radboudumc, Nijmegen, The Netherlands.


---

## What It Does

The app offers six self-contained workflow paths accessible from a central hub:

**1. Plan a Study** — Calculate sample size or power for bioequivalence studies with PowerTOST: standard average bioequivalence, highly variable drugs (EMA ABEL, FDA RSABE) and narrow therapeutic index drugs (FDA NTID). The scaled methods use the within-subject CVs of both Test and Reference. Offers the same study designs, under the same names, as the Bioequivalence analysis, and can take the within-subject CV from a bioequivalence analysis of your own data. Interactive power curves and a CV sensitivity plot.

**2. Upload & Check Data** — Import CSV or Excel files (comma or semicolon separator, point or decimal comma) and auto-detect common column names (including common export and European naming conventions). CDISC ADNCA datasets are read through a separate *CDISC ADNCA dataset* option (see below). Set the LLOQ and one of 6 BLQ rules, applied per profile; text such as `<0.5`, BLQ, BQL, BLOQ, ND and NQ is handled as below the LLOQ. 20+ automated data quality checks, including safety checks that refuse data the app cannot analyse safely: mixed units, dates or clock times as time, time since the first dose instead of the dose of each period, and several profiles stacked in one column.

**3. Visualize Data** — Create publication-ready concentration-time plots directly from uploaded data. Individual profiles (spaghetti plot) with flexible colour grouping, and geometric mean ×/÷ geometric SD summary curves with treatment overlays for crossover data. Export to PNG, PDF, or SVG at up to 600 DPI. Includes an auto-generated figure legend ready to paste into a manuscript.

**4. Analyze One Subject at a Time** — Step through individual profiles with Previous/Next navigation. Includes manual data entry, an interactive half-life inspector with point-by-point adjustment, and dose auto-fill from the data for each profile.

**5. Analyze All Subjects (Batch)** — Run NCA on all profiles at once (one profile per subject, treatment and period, so replicate administrations stay separate). Summary statistics per treatment, individual profile grid, spaghetti and mean±SD plots, half-life review, and steady-state analysis with an entered dosing interval (AUCτ, average concentration, fluctuation and swing). When the automatic terminal fit is below the minimum adjusted R² (default 0.70), half-life and the parameters derived from it are not reported for that profile, unless you select the points yourself.

**6. Bioequivalence Testing** — NCA → ANOVA (EMA Method A: Sequence, Subject, Period and Treatment as factors) → confidence interval (90% by default) → forest plot → bioequivalence conclusion. You choose the Reference treatment; the conclusion uses confidence limits rounded to two decimals and, for limits wider than 80–125%, also requires the point estimate within 80.00–125.00% by default. Designs: 2×2 crossover, 2×2×3 and 2×2×4 full replicate, 2×3×3 partial replicate, parallel groups, and paired comparison (all subjects in the same order; ratio without a verdict). For replicate designs the within-subject variability of Reference and Test and the EMA expanded limits they would imply are shown for information; the app performs average bioequivalence only, not reference-scaled (ABEL/RSABE) or NTID analyses. Results agree with the replicateBE package on its 30 reference data sets.

Plus: **Statistical Methods** page (text for a manuscript's Methods section), **Data Preparation Guide** (10 scenario tabs), and **About & Packages** with version history.

---

## CDISC data and terminology

- **ADNCA datasets:** set *What kind of file?* to **CDISC ADNCA dataset** on the Upload page. The app shows a summary, asks which time variable (NRRLT, ARRLT or MRRLT) and analyte to use, applies ANL01FL, refuses derived records (DTYPE) and other data it cannot convert safely, and stores the choices in the Analysis Record. The standalone converter [`converters/adnca_to_flat.R`](converters/adnca_to_flat.R) does the same outside the app; see [`converters/ADNCA_TO_FLAT.md`](converters/ADNCA_TO_FLAT.md), which also gives a recipe for SAS transport (`.xpt`) files, which are not read directly.
- **Parameter codes:** results, downloads and Analysis Records list the official CDISC PK parameter code of each parameter from CDISC SDTM Controlled Terminology release 2026-03-27 ([`cdisc/`](cdisc/)). This is a code lookup; the results are not SDTM PP datasets.

NCA Assistant has not been checked against a specific version of the ADNCA Implementation Guide and is not affiliated with, endorsed by, or certified by CDISC.

---

## Complete Analysis Record

**One Subject at a Time**, **All Subjects (Batch)** and **Bioequivalence** can generate a **Complete Analysis Record**: a self-contained zip file for archiving, audit trails, publication supplements, and inclusion in a sponsor's study documentation. A consistently placed *Generate Analysis Record* panel (with an explanatory tooltip) appears on each tab once results exist.

For the NCA and bioequivalence paths the record contains:

- **results.xlsx** — Individual NCA parameters, summary statistics, and (for BE) confidence intervals and ANOVA tables
- **app_results_reference.csv** — The app's computed results in machine-readable form, used by the reproduction script for an automated comparison
- **analysis_settings.json** — Every setting that affects the analysis (including per-profile doses and, for bioequivalence, the design, Reference treatment, model, confidence level, limits and point-estimate constraint), with package versions, schema version, timestamp, and (if used in the same session) visualization settings
- **nca_pipeline.R** — The app's own data-processing code, so the reproduction runs exactly the code the app used
- **reproduce_analysis.R** — Standalone R script that reproduces the exact analysis without the app. It re-checks the source-data SHA-256 against the recorded value, and **automatically compares** its output against `app_results_reference.csv`, printing a `MATCH` / `DIFFERENT` verdict. The app runs this script when it creates the record and stores the outcome in `reproduction_check.txt`
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

Available at [robterheine.shinyapps.io/NCAassistant](https://robterheine.shinyapps.io/NCAassistant/).

---

## Validation

A consolidated validation package is available in [`validation/`](validation/), following a risk-based approach consistent with ICH Q9 and GAMP 5 Category 5 principles.

**Single validation script** — run from the project root:

```bash
Rscript validation/validation.R
```

This executes 338 automated tests (plus 44 manual tests defined for a running app) and generates a results CSV with environment details, per-section results, and URS traceability.

**Validation deliverables:**

- **User Requirement Specification** ([`validation/NCA_Assistant_URS.docx`](validation/NCA_Assistant_URS.docx)) — 58 requirements across 8 categories (GEN, DAT, NCA, BE, PWR, EXP, UI, VIZ), with FMEA risk assessment, supplier assessment, and change control procedures
- **IQ/OQ/PQ Protocol** ([`validation/NCA_Assistant_IQOQPQ.docx`](validation/NCA_Assistant_IQOQPQ.docx)) — every automated and manual test listed individually with method, expected result, URS cross-reference, and criticality classification
- **Consolidated test script** ([`validation/validation.R`](validation/validation.R)) — automated tests + manual test definitions, covering IQ, data handling, NCA accuracy, bioequivalence, power/sample size, export/reproducibility, usability, and visualization (URS-VIZ)

NCA accuracy is checked against analytical ground truth (mono-exponential IV bolus) and R's built-in Theoph and Indometh datasets; bioequivalence results against `replicateBE` (30 reference data sets) and sample sizes against PowerTOST; every Analysis Record type is checked to reproduce. Every test is classified CRITICAL (failure blocks qualification) or SUPPORTIVE (failure requires risk assessment). Visualization tests are classified SUPPORTIVE (non-critical: graphical output does not affect NCA parameters or regulatory conclusions).

See [`validation/README.md`](validation/README.md) for detailed instructions on running the validation and adapting it for your organisation.

---

## Citation

> ter Heine R. NCA Assistant (v1.4.0). Radboud Applied Pharmacometrics, Radboudumc, Nijmegen, The Netherlands. https://github.com/robterheine/ncaassistant

> Kim H, Han S, Cho YS, Yoon SK, Bae KS. Development of R packages: 'NonCompart' and 'ncar' for noncompartmental analysis (NCA). *Transl Clin Pharmacol*. 2018;26(1):10-15.

---

## License

GPL-3.

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen*
