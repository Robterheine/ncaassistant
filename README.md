# Non-Compartmental Analysis Assistant

**Version 1.0 Release Candidate 1 (1.0 RC1)**

A freely available, open-source Shiny application for pharmacokinetic non-compartmental analysis (NCA), bioequivalence testing, and study planning. Developed by the [Radboud Applied Pharmacometrics](https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics) research group at Radboudumc, Nijmegen, The Netherlands.

Built for pharmacokinetic scientists at the start of their career. Every setting has a plain-language explanation. No abbreviations without context.

---

## What It Does

The app offers five self-contained workflow paths accessible from a central hub:

**1. Plan a Study** — Calculate sample size or power for bioequivalence studies. Supports standard ABE, highly variable drug designs (ABEL/EMA, RSABE/FDA), narrow therapeutic index (NTID/FDA), dose-proportionality, and non-inferiority. All major regulatory presets (EMA, FDA, PMDA, Health Canada, WHO, CDE). Interactive power curves and sensitivity analyses for all design types.

**2. Upload & Check Data** — Import CSV or Excel files, auto-detect column names (including CDISC, WinNonlin export, and European naming conventions), set BLQ handling rules, and run a 20+ point automated data quality check before analysis.

**3. Analyze One Subject at a Time** — Step through individual concentration-time profiles with Previous/Next navigation. Includes manual data entry (type time and concentration values directly — no file needed), interactive half-life inspector with point-by-point adjustment, and per-subject NCA results. Supports single-dose and steady-state analysis.

**4. Analyze All Subjects (Batch)** — Run NCA on all subjects at once. Population summary statistics (mean, SD, CV%, geometric mean), individual profile grid, spaghetti and mean±SD plots, and a half-life review tab where you can adjust the terminal slope for any subject and propagate the change through all derived parameters.

**5. Bioequivalence Testing** — Complete BE workflow: NCA → ANOVA → 90% confidence intervals → forest plot → regulatory conclusion. Supports 2-period crossover, replicate crossover, and parallel designs. Fixed-effects and mixed-effects (nlme) models. Exports regulatory-ready Excel reports.

---

## Quick Start

### Option A: Run locally in RStudio

```r
source("install_and_run.R")
```

This checks your R version, installs any missing packages, and launches the app. Requires R ≥ 4.1.0.

### Option B: Manual launch

```r
# Install dependencies (first time only)
install.packages(c(
  "shiny", "bslib", "NonCompart", "ncar", "PowerTOST",
  "plotly", "DT", "readxl", "dplyr", "tidyr", "ggplot2",
  "shinyWidgets", "htmltools", "openxlsx", "nlme"
))

# Launch
shiny::runApp()
```

### Option C: shinyapps.io

A hosted version is available at [robterheine.shinyapps.io/NCAassistant](https://robterheine.shinyapps.io/NCAassistant/).

---

## Data Preparation

The app includes a built-in **Data Preparation Guide** (accessible from the top navigation bar) with example datasets for every study type:

| Study type | Required columns |
|---|---|
| Single subject | Time, Concentration |
| Multiple subjects | Subject, Time, Concentration |
| 2-period crossover (BE) | Subject, Treatment, Period, Sequence, Time, Concentration |
| Replicate crossover (BE) | Same as above (Period goes up to 3 or 4) |
| Parallel-group BE | Subject, Treatment, Time, Concentration |
| Steady-state | Subject, Time, Concentration (tick steady-state in settings) |

Column names are auto-detected. The app recognises common conventions: `USUBJID`, `NTIM`, `DV`, `Trt`, `Konz`, `Zeit`, `Proband`, and many others.

---

## Validation

The app ships with three automated test suites (238 tests total):

| Suite | Tests | What it covers |
|---|---|---|
| `tests/cross_validation.R` | 41 | NCA engine accuracy against analytical solutions, Theoph/Indometh datasets, BLQ rules, BE confidence intervals, PowerTOST self-consistency |
| `tests/functional_ux_validation.R` | 112 | Column auto-detection (21 naming conventions), messy data handling, end-to-end crossover pipeline, edge cases (200 subjects, sparse data, extreme values), CDISC compatibility |
| `tests/comprehensive_validation.R` | 85 | Settings propagation (every UI control reaches the engine), manual entry parsing, cross-path data flow, NCA sensitivity to settings changes, BE mixed-effects validation, power calculations for all designs, audit trail reproducibility, label consistency, data quality gatekeeper |

Run all tests:

```r
setwd("path/to/ncaassistant")
source("tests/cross_validation.R")
source("tests/functional_ux_validation.R")
source("tests/comprehensive_validation.R")
```

### NCA Engine

All NCA calculations are performed by the [NonCompart](https://cran.r-project.org/package=NonCompart) R package (Kim et al., *Translational and Clinical Pharmacology*, 2018;26(1):10-15), validated against Certara Phoenix WinNonlin®. The app wraps NonCompart's `tblNCA` (batch) and `sNCA` (single profile) functions.

### Bioequivalence

The ANOVA-based 90% confidence interval uses R's `lm()` for fixed-effects models and `nlme::lme()` for mixed-effects models. Treatment effect estimation uses the coefficient and standard error directly from the model summary. Results have been cross-validated against paired t-test approaches and manual calculations.

### Power & Sample Size

All power and sample size calculations use the [PowerTOST](https://cran.r-project.org/package=PowerTOST) package (Labes, Schütz & Lang), which implements exact methods via Owen's Q function.

---

## File Structure

```
ncaassistant/
├── app.R                           Main app: hub, navigation, about screen
├── install_and_run.R               One-click setup and launch
├── R/
│   ├── utils.R                     Design detection, summary stats, CDISC names
│   ├── nca_helpers.R               BLQ rules, lambda_z estimation, run_nca()
│   ├── data_quality.R              20+ automated quality checks
│   ├── help_system.R               25+ contextual help popovers
│   ├── mod_data_upload.R           Shared upload + column mapping module
│   ├── mod_data_guide.R            Data preparation guide (7 scenarios)
│   ├── mod_path_power.R            Path 1: Power & sample size
│   ├── mod_path_data.R             Path 2: Upload & check data
│   ├── mod_path_single_nca.R       Path 3: Single-subject NCA
│   ├── mod_path_multi_nca.R        Path 4: Batch NCA (all subjects)
│   └── mod_path_be.R               Path 5: Bioequivalence testing
├── data/
│   └── example_be_crossover.csv    6-subject 2×2 crossover test dataset
├── tests/
│   ├── cross_validation.R          41 statistical accuracy tests
│   ├── functional_ux_validation.R  112 functional/UX tests
│   └── comprehensive_validation.R  85 comprehensive tests
└── www/
    ├── custom.css                  Bootstrap 5 theme refinements
    └── logo.svg                    App icon
```

---

## R Package Dependencies

| Package | Role |
|---|---|
| [NonCompart](https://cran.r-project.org/package=NonCompart) | NCA engine (Cmax, AUC, half-life, CL, Vd) |
| [ncar](https://cran.r-project.org/package=ncar) | NCA reporting utilities |
| [PowerTOST](https://cran.r-project.org/package=PowerTOST) | Power/sample size for BE studies |
| [nlme](https://cran.r-project.org/package=nlme) | Mixed-effects ANOVA for BE |
| [shiny](https://cran.r-project.org/package=shiny) | Web application framework |
| [bslib](https://cran.r-project.org/package=bslib) | Bootstrap 5 theming |
| [plotly](https://cran.r-project.org/package=plotly) | Interactive plots |
| [ggplot2](https://cran.r-project.org/package=ggplot2) | Static publication-quality plots |
| [DT](https://cran.r-project.org/package=DT) | Interactive data tables |
| [readxl](https://cran.r-project.org/package=readxl) | Excel file import |
| [openxlsx](https://cran.r-project.org/package=openxlsx) | Excel file export |
| [dplyr](https://cran.r-project.org/package=dplyr) | Data manipulation |
| [tidyr](https://cran.r-project.org/package=tidyr) | Data reshaping |
| [shinyWidgets](https://cran.r-project.org/package=shinyWidgets) | Extended UI components |

Full version details and references are available in the app under **About & Packages**.

---

## Known Limitations

- **Log-down trapezoidal + noiseless IV bolus data**: produces NaN when back-extrapolated C0 equals observed C(0) exactly. Only occurs with simulated data. The app defaults to log-down and documents this edge case.
- **No automated browser-level testing**: the 238 tests validate statistical accuracy, data pipelines, and code structure, but do not test UI interactions in a real browser. Browser-level validation (e.g., with shinytest2) is planned.
- **Single-session state**: lambda_z overrides and analysis results are not persisted between sessions.

---

## License

GPL-3. All R packages retain their original licenses.

---

## Citation

If you use this software in published work, please cite:

> NCA Assistant (v1.0 RC1). Radboud Applied Pharmacometrics, Radboudumc, Nijmegen, The Netherlands. Available at: https://github.com/Robterheine/NCAassistant

And the underlying NCA engine:

> Kim H, Yun H, Cho S, et al. NonCompart: Noncompartmental Analysis for Pharmacokinetic Data. *Translational and Clinical Pharmacology*. 2018;26(1):10-15.

---

## Contributing

This is a release candidate. Bug reports, feature requests, and pull requests are welcome via [GitHub Issues](https://github.com/Robterheine/NCAassistant/issues).

---

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen*
