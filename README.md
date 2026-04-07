# NCA Assistant

**Version 1.0**

A freely available, open-source Shiny application for pharmacokinetic non-compartmental analysis (NCA), bioequivalence testing, and study planning. Developed by the [Radboud Applied Pharmacometrics](https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics) research group at Radboudumc, Nijmegen, The Netherlands.

Built for pharmacokinetic scientists at the start of their career. Every setting has a plain-language explanation. No abbreviations without context.

---

## What It Does

The app offers five self-contained workflow paths accessible from a central hub:

**1. Plan a Study** — Calculate sample size or power for bioequivalence studies. Supports standard ABE, highly variable drug designs (ABEL/EMA, RSABE/FDA), narrow therapeutic index (NTID/FDA), dose-proportionality, and non-inferiority. Interactive power curves and sensitivity analyses for all design types.

**2. Upload & Check Data** — Import CSV or Excel files, auto-detect column names (including CDISC, WinNonlin export, and European naming conventions), set BLQ handling rules (6 rules including LLOQ/2 for drugs with absorption lag), and run 20+ automated data quality checks.

**3. Analyze One Subject at a Time** — Step through individual profiles with Previous/Next navigation. Includes manual data entry, interactive half-life inspector with point-by-point adjustment, and per-subject dose auto-fill from data.

**4. Analyze All Subjects (Batch)** — Run NCA on all subjects at once. Population summary statistics, individual profile grid, spaghetti and mean±SD plots, half-life review with recalculate, and steady-state display with AUCτ as the primary parameter.

**5. Bioequivalence Testing** — Complete BE workflow: NCA → ANOVA → 90% confidence intervals → forest plot → regulatory conclusion. Supports 2-period crossover, fixed-order crossover, 3-period crossover, replicate crossover, and parallel designs.

Plus: **Statistical Methods** page (publication-ready, expert-reviewed), **Data Preparation Guide** (10 scenario tabs), and **About & Packages** with version history.

---

## Complete Analysis Record

Both the Batch NCA and Bioequivalence paths can generate a **Complete Analysis Record** — a self-contained zip file for regulatory submissions, publication supplements, or audit trails:

- **results.xlsx** — Individual NCA parameters, summary statistics, and (for BE) confidence intervals and ANOVA tables
- **analysis_settings.json** — Every setting that affects the analysis, with package versions and timestamp
- **reproduce_analysis.R** — Standalone R script that reproduces the exact analysis without the app, with step-by-step comments
- **data_integrity.txt** — SHA-256 hash of the original data file
- **analysis_summary.html** — Self-contained summary with statistical methods, software environment, and instructions
- **Original data file** — Copy included so the package is self-contained

---

## Requirements

- R ≥ 4.1.0
- [renv](https://rstudio.github.io/renv/) (installed automatically on first launch)

---

## Quick Start

### Option A: Run locally

Clone the repository and restore the package environment:

```r
# install.packages("renv")  # only if not already installed
renv::restore()
shiny::runApp()
```

`renv::restore()` reads the lockfile and installs the exact package versions used in development. This runs once; subsequent launches only need `shiny::runApp()`.

### Option B: shinyapps.io

Available at [robterheine.shinyapps.io/NCAassistant](https://robterheine.shinyapps.io/NCAassistant/).

---

## Validation

250+ automated tests across 3 suites. NCA engine validated against Certara Phoenix WinNonlin® (Kim et al., 2018). Reproducibility script tested by generating, executing, and comparing results against direct NonCompart output.

A draft validation package is available in [`validation/`](validation/), following a risk-based approach consistent with ICH Q9 and GAMP 5 Category 5 principles:

- **User Requirement Specification (URS)** — 44 requirements across 7 categories, with FMEA risk assessment, supplier assessment for key packages, ALCOA+ data integrity framework, and change control procedures
- **IQ/OQ/PQ Protocol** — 94 pre-filled test cases (11 IQ, 78 OQ, 5 PQ) with sign-off pages and deviation handling
- **Automated IQ/OQ/PQ test script** ([`tests/iq_oq_pq_validation.R`](tests/iq_oq_pq_validation.R)) — executes all programmatically verifiable test cases, keyed to protocol IDs

These documents are provided as drafts. Organisations adopting NCA Assistant for regulated use should review, adapt, and sign off the documents according to their own quality management system. See [`validation/README.md`](validation/README.md) for instructions.

Full documentation of every individual test (what it verifies, how it works, and why it matters) is available in [`tests/TEST_DOCUMENTATION.md`](tests/TEST_DOCUMENTATION.md).

---

## Citation

> NCA Assistant (v1.0). Radboud Applied Pharmacometrics, Radboudumc, Nijmegen, The Netherlands. https://github.com/Robterheine/NCAassistant

> Kim H, et al. NonCompart: Noncompartmental Analysis for Pharmacokinetic Data. *Transl Clin Pharmacol*. 2018;26(1):10-15.

---

## License

GPL-3.

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen*
