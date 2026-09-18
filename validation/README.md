# NCA Assistant — Validation Package

This folder contains the validation package for NCA Assistant v1.5.0. It follows a risk-based approach consistent with ICH Q9 and GAMP 5 Category 5 principles for custom software used in a regulated pharmaceutical environment.

---

## Contents

| File | Description |
|------|-------------|
| `validation.R` | Consolidated validation script (Attachment A to the IQ/OQ/PQ protocol) |
| `make_iqoqpq.py` | Regenerates the test tables, traceability matrix and counts of the IQ/OQ/PQ protocol from `validation_results.csv` (needs python-docx) |
| `NCA_Assistant_URS.docx` | User Requirement Specification — 63 requirements across 8 categories |
| `NCA_Assistant_IQOQPQ.docx` | IQ/OQ/PQ protocol — every test listed individually with method, expected result, URS cross-reference, and criticality |
| `fixtures/` | Committed test data (crossover, replicate and ADNCA-shaped files, plus reference values from `replicateBE`) and the deterministic scripts that generate them |
| `validation_results.csv` | Generated on each run: pass/fail record with timestamps and environment details. Not committed, see below |

---

## Running the Validation Script

Run from the **project root** (not from inside the `validation/` folder):

```bash
Rscript validation/validation.R
```

Or from within R:

```r
source("validation/validation.R")
```

### What the script needs

**R.** Version 4.1.0 or newer (checked by IQ-01).

**R packages.** The script installs any that are missing:

| Package | Used for |
|---|---|
| `NonCompart` | the NCA itself |
| `PowerTOST` | sample size and power |
| `nlme` | mixed-effects bioequivalence models |
| `digest` | SHA-256 hashes |
| `openxlsx`, `jsonlite`, `readxl` | reading and writing record files |
| `dplyr` | data handling in the figure checks |
| `replicateBE` | validation only: the reference implementation the replicate-design and partial AUC checks compare against (sections REP and PAUC). The app never uses it |

The interface packages the app loads (shiny, bslib, shinyWidgets, DT, plotly, ggplot2, htmltools, tidyr) are checked but not installed by the script.

**Files.** Run the script from the project root: it reads the repository by relative path and does not copy anything into a temporary folder first. It needs

- `R/*.R` and `app.R` — the code under test, sourced directly, plus `APP_VERSION`;
- `converters/adnca_to_flat.R` — the standalone ADNCA converter (section CONV);
- `cdisc/ct_release.dcf` and `cdisc/pk_parameter_terms.csv` — the pinned CDISC release the parameter codes come from (checks EXP-CD-01..03 and REC-09);
- `data/example_theoph.csv` and `data/example_be_crossover.csv` — example datasets used by the NCA and record checks;
- **`validation/fixtures/`** — required. Crossover, replicate and ADNCA-shaped test data, plus `replicateBE_reference.csv` with the committed `replicateBE::method.A` values. These files are read at the top level of the script, so a missing fixture stops the run with `cannot open file ...` rather than failing a single test: without the folder the run aborts partway and produces no results file.

Test data for crossover and replicate designs live in `validation/fixtures/`, not in `data/`. `make_fixtures.R` (crossover and replicate designs) and `make_adnca_fixtures.R` (ADNCA-shaped data) generate them deterministically, and `make_reference_values.R` records the matching `replicateBE::method.A` results; the generators and their outputs are both committed, so the suite runs without regenerating anything.

**Nothing else is needed to run the suite.** `make_iqoqpq.py` is only for regenerating the IQ/OQ/PQ protocol afterwards and needs Python with `python-docx`; the app's own runtime (a browser, a Shiny server) is not involved, because the script tests the code, not a running app.

On completion the script prints a results summary to the console and writes `validation/validation_results.csv`. That file is deliberately not committed: it is regenerated on every run and records the machine and environment of that run. The IQ/OQ/PQ protocol holds the committed record of a passing run.

---

## What the Script Tests

The script runs **361 automated tests** in eighteen sections, each mapped to a URS requirement:

| Section | Code | Tests | Tests cover |
|---------|------|------:|-------------|
| Installation Qualification | IQ | 20 | R version, package availability (analysis and interface packages), every source file parses, file integrity (SHA-256 hashes) |
| Data Handling | DAT | 63 | Column auto-detection, data quality checks, BLQ rules 1–6 per profile, BLQ text, study design detection, the shared data pipeline, interlocks (IL: CDISC-shaped flat files, mixed units, date/clock time, time since first dose, stacked profiles) and decimal-comma reading |
| NCA Accuracy | NCA | 40 | Analytical ground truth (mono-exponential IV bolus), Theoph and Indometh datasets, lambda-z, routes, trapezoid methods, dose normalisation, steady state, edge cases, manual data entry, crossover profiles |
| Bioequivalence | BE | 10 | CI construction, TOST logic, crossover ANOVA, mixed model, paired and parallel designs |
| Half-life overrides | OQ-NEW | 12 | R² propagation, recalculation by NonCompart, negative slope rejection, 2-point edge case, override audit trail |
| Power & Sample Size | PWR | 13 | ABE, ABEL, RSABE, NTID and the planner designs via PowerTOST |
| Export & Reproducibility | EXP | 18 | Determinism, summary statistics, R script generation, SHA-256 integrity, app and package versions, CDISC parameter codes from the pinned release |
| Usability & Code Quality | UI | 32 | Parameter labels and help topics, column-mapping validation, defensive coding checks (including one that fails when a layout gives fewer column widths than inputs), requirement spot checks |
| Visualisation | VIZ | 9 | Plot data construction, dose normalisation, colour palette handling |
| Correctness regressions | REG | 29 | Dose matching, BLQ rule scoping and ordering, unit validation, bioequivalence model and verdict (point-estimate constraint, rounding, factor coding), execution of the shipped reproduction script |
| Replicate designs | REP | 25 | Profiles per administration, design merge, CVwR/CVwT diagnostic, design registry, agreement with `replicateBE` method A on its 30 reference data sets |
| Analysis Records | REC | 9 | Shipped pipeline and hashes; reproduction MATCH for theophylline, molar units, a semicolon/decimal-comma file with BLQ text, a replicate BE study with overrides, single-subject and manual entry; tampered data detected; figure rebuilt |
| ADNCA conversion | CONV | 19 | The ADNCA import shared by the app and `converters/adnca_to_flat.R`: time variable choice, ANL01FL, DTYPE, analyte/matrix selection, units, LLOQ, refusals, conversion log |
| First adversarial review | REV | 11 | Per-profile doses, BLQ text, unit-column detection, rounded CI limits, model column, subject counts, whitespace in IDs, steady-state message, record fallback copy, record file names |
| Second review (1) | REV2 | 7 | Reference treatment chosen by the user, Test and Reference CV in scaled planning, within-subject CV for the planner, grouped exports, CI labels |
| Second review (2) | REV3 | 14 | Minimum R² applied to results, half-life review equal to NonCompart’s fit, results cleared on new data or profile, empty LLOQ, figure legend, help and Methods wording, warning when no Subject column is recognised, no references to commercial NCA software, half-life without a verdict |
| Statistical audit | REV4 | 8 | Steady state with an entered dosing interval (AUCτ from 0 to τ, CL/F and Vz/F from AUCτ, Cavg, fluctuation and swing in all paths, records), planner defaults per method and total CV for parallel designs, Methods page statements, figure labels |
| Partial AUC | PAUC | 22 | Intervals with a fixed end or an end at the last measurable concentration (t), hand-calculated trapezoids, interpolated cutoffs, no extrapolation past Tlast, steady-state limits, Cmax and Tmax within an interval, notes for zeros and for BLQ-dependent or sparse windows, bioequivalence with pivotal and supportive roles (agreement with `replicateBE`), records, labels, the CDISC code AUCINT, figure shading and the app text |

In addition, **49 manual tests** are defined in the script (Section MAN). These require a running app instance and cover interactive features such as file upload (flat and CDISC ADNCA), column mapping, interlock messages, the half-life review and minimum-R² note, choosing the Reference treatment, the replicate variability table, planning with both CVs, CDISC parameter codes, partial AUC intervals in the batch and bioequivalence paths (including an invalid interval, a suppressed metric and the shaded figure), the Complete Analysis Record download and its reproduction check, and the Visualize Figure Record. They are included in the script for traceability but are marked SKIP in automated runs.

The test tables in `NCA_Assistant_IQOQPQ.docx` (IQ, automated OQ/PQ sections, manual tests, traceability matrix and totals) are generated from `validation_results.csv` of a passing reference run, so they list exactly the tests the script defines.

---

## Test Classification

Every test is classified as either:

- **CRITICAL** — failure blocks qualification. The app must not be used for regulated analyses until the failure is resolved.
- **SUPPORTIVE** — failure requires risk assessment. The app may continue to be used while the issue is investigated, provided a documented justification exists.

Visualisation tests (URS-VIZ) are classified SUPPORTIVE because graphical output does not affect NCA parameters or regulatory conclusions.

---

## Interpreting Results

A passing run produces:

```
Total: 410 (auto: 361, manual: 49)
  PASS: 361 | FAIL: 0 | ERROR: 0 | SKIP: 49

ALL CRITICAL TESTS PASSED

URS: 62/62 covered

Results: validation/validation_results.csv
```

Of the 361 automated tests, 268 are CRITICAL and 93 SUPPORTIVE.

If any critical test fails, the script lists the affected test IDs under `CRITICAL FAILURES` and prints `STATUS: FAILED`. Supportive failures are counted separately and require a written risk assessment before the system can be signed off.

The `validation_results.csv` file records each test's ID, name, section, classification, result, URS reference, and any error message. This file is the primary evidence document for the qualification record.

---

## Adapting for Your Organisation

The validation package is provided as a starting point. Before use in a regulated environment:

1. **Execute the validation script** in your target environment and retain the console output and `validation_results.csv` as evidence.
2. **Complete the manual tests** in `NCA_Assistant_IQOQPQ.docx` using a running app instance. Record the actual results and tester signatures in the protocol.
3. **Review the URS** (`NCA_Assistant_URS.docx`) against your organisation's requirements. Add or remove requirements as appropriate and re-run the validation script to confirm coverage.
4. **Perform a risk assessment** for any SUPPORTIVE test failures or requirements not applicable to your use case.
5. **Retain all documents** (URS, IQ/OQ/PQ protocol, validation results, risk assessments) in your quality management system.

The documents name the application version they were produced for, but carry no document version of their own, in the file name or elsewhere, so they can go into your document management system under your own versioning scheme.

---

## File Integrity

The validation script computes SHA-256 hashes of `validation.R` itself and the core R source files it tests (`R/utils.R`, `R/nca_helpers.R`, `R/data_quality.R`, `R/export_record.R`, `R/mod_data_upload.R`, `R/designs.R`, `R/be_analysis.R`, `R/pipeline.R`, `R/interlocks.R`, `R/adnca_import.R`, `R/cdisc_terms.R`, `converters/adnca_to_flat.R`). These hashes are printed at the start of each run and recorded in `validation_results.csv`. Retain these alongside the results as evidence that the validated source files were not modified between qualification and use.

---

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen, The Netherlands*
