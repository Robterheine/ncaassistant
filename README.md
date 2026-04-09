# NCA Assistant

**Version 1.2** | Designed by Rob ter Heine

A freely available, open-source Shiny application for pharmacokinetic non-compartmental analysis (NCA), bioequivalence testing, study planning, and data visualization. Developed by the [Radboud Applied Pharmacometrics](https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics) research group at Radboudumc, Nijmegen, The Netherlands.

Built for pharmacokinetic scientists at the start of their career. Every setting has a plain-language explanation. No abbreviations without context.

---

## What It Does

The app offers six self-contained workflow paths accessible from a central hub:

**1. Plan a Study** — Calculate sample size or power for bioequivalence studies. Supports standard ABE, highly variable drug designs (ABEL/EMA, RSABE/FDA), narrow therapeutic index (NTID/FDA), dose-proportionality, and non-inferiority. Interactive power curves and sensitivity analyses for all design types.

**2. Upload & Check Data** — Import CSV or Excel files, auto-detect column names (including CDISC, WinNonlin export, and European naming conventions), set BLQ handling rules (6 rules including LLOQ/2 for drugs with absorption lag), and run 20+ automated data quality checks.

**3. Visualize Data** *(new in v1.2)* — Create publication-ready concentration-time plots directly from uploaded data. Individual profiles (spaghetti plot) with flexible colour grouping, and geometric mean ± geometric CV% summary curves with treatment overlays for crossover data. Export to PNG, PDF, or SVG at journal-submission resolution (up to 600 DPI). Includes an auto-generated figure legend ready to paste into a manuscript.

**4. Analyze One Subject at a Time** — Step through individual profiles with Previous/Next navigation. Includes manual data entry, interactive half-life inspector with point-by-point adjustment, and per-subject dose auto-fill from data.

**5. Analyze All Subjects (Batch)** — Run NCA on all subjects at once. Population summary statistics, individual profile grid, spaghetti and mean±SD plots, half-life review with recalculate, and steady-state display with AUCτ as the primary parameter.

**6. Bioequivalence Testing** — Complete BE workflow: NCA → ANOVA → 90% confidence intervals → forest plot → regulatory conclusion. Supports 2-period crossover, fixed-order crossover, 3-period crossover, replicate crossover, and parallel designs.

Plus: **Statistical Methods** page (publication-ready, expert-reviewed), **Data Preparation Guide** (10 scenario tabs), and **About & Packages** with version history.

---

## Complete Analysis Record

Both the Batch NCA and Bioequivalence paths can generate a **Complete Analysis Record** — a self-contained zip file for regulatory submissions, publication supplements, or audit trails:

- **results.xlsx** — Individual NCA parameters, summary statistics, and (for BE) confidence intervals and ANOVA tables
- **analysis_settings.json** — Every setting that affects the analysis, with package versions, timestamp, and (if used in the same session) visualization settings
- **reproduce_analysis.R** — Standalone R script that reproduces the exact analysis without the app, with step-by-step comments
- **data_integrity.txt** — SHA-256 hash of the original data file
- **analysis_summary.html** — Self-contained summary with statistical methods, software environment, and instructions
- **Original data file** — Copy included so the package is self-contained

---

## Requirements

- R ≥ 4.1.0
- Required packages: NonCompart, PowerTOST, nlme, shiny, bslib, plotly, DT, readxl, dplyr, tidyr, ggplot2, openxlsx, jsonlite, digest

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

This executes automated tests and generates a results CSV with environment details, per-section results, and URS traceability.

**Validation deliverables:**

- **User Requirement Specification** ([`validation/NCA_Assistant_URS.docx`](validation/NCA_Assistant_URS.docx)) — requirements across 8 categories (GEN, DAT, NCA, BE, PWR, EXP, UI, VIZ), with FMEA risk assessment, supplier assessment, ALCOA+ data integrity framework, and change control procedures
- **IQ/OQ/PQ Protocol** ([`validation/NCA_Assistant_IQOQPQ.docx`](validation/NCA_Assistant_IQOQPQ.docx)) — every automated and manual test listed individually with method, expected result, URS cross-reference, and criticality classification
- **Consolidated test script** ([`validation/validation.R`](validation/validation.R)) — automated tests + manual test definitions, covering IQ, data handling, NCA accuracy, bioequivalence, power/sample size, export/reproducibility, usability, and visualization (URS-VIZ)

NCA accuracy validated against analytical ground truth (mono-exponential IV bolus) and R's built-in Theoph and Indometh datasets. Every test is classified CRITICAL (failure blocks qualification) or SUPPORTIVE (failure requires risk assessment). Visualization tests are classified SUPPORTIVE (non-critical: graphical output does not affect NCA parameters or regulatory conclusions).

See [`validation/README.md`](validation/README.md) for detailed instructions on running the validation and adapting it for your organisation.

---

## Citation

> ter Heine R. NCA Assistant (v1.2). Radboud Applied Pharmacometrics, Radboudumc, Nijmegen, The Netherlands. https://github.com/robterheine/ncaassistant

> Kim H, et al. NonCompart: Noncompartmental Analysis for Pharmacokinetic Data. *Transl Clin Pharmacol*. 2018;26(1):10-15.

---

## License

GPL-3.

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen*
