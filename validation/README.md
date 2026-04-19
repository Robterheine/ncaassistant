# NCA Assistant — Validation Package

This folder contains the validation package for NCA Assistant v1.2. It follows a risk-based approach consistent with ICH Q9 and GAMP 5 Category 5 principles for custom software used in a regulated pharmaceutical environment.

---

## Contents

| File | Description |
|------|-------------|
| `validation.R` | Consolidated validation script (Attachment A to the IQ/OQ/PQ protocol) |
| `NCA_Assistant_URS.docx` | User Requirement Specification — 49 requirements across 8 categories |
| `NCA_Assistant_IQOQPQ.docx` | IQ/OQ/PQ protocol — every test listed individually with method, expected result, URS cross-reference, and criticality |
| `validation_results.csv` | Generated on each run — pass/fail record with timestamps and environment details |

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

The script installs any missing packages automatically. Required packages: `NonCompart`, `PowerTOST`, `nlme`, `digest`, `openxlsx`, `jsonlite`, `readxl`, `dplyr`, `knitr`.

On completion the script prints a results summary to the console and writes `validation/validation_results.csv`.

---

## What the Script Tests

The script runs **169 automated tests** across eight sections, each mapped to a URS requirement:

| Section | Code | Tests cover |
|---------|------|-------------|
| Installation Qualification | IQ | R version, package availability, file integrity (SHA-256 hashes) |
| Data Handling | DAT | Column auto-detection, BLQ rules 1–6, LLOQ validation, crossover data |
| NCA Accuracy | NCA | Analytical ground truth (mono-exponential IV bolus), Theoph dataset, lambda-z estimation, R²adj, steady-state |
| Bioequivalence | BE | CI construction, TOST logic, crossover ANOVA, forest plot data |
| v1.1 Features | OQ-NEW | R² slider propagation, half-life recalculation, negative slope rejection, 2-point edge case, override audit trail |
| Power & Sample Size | PWR | ABE, ABEL, RSABE, NTID designs via PowerTOST |
| Export & Reproducibility | EXP | JSON settings structure, R script generation, SHA-256 integrity, schema version |
| Usability & Code Quality | UI | Module loading, defensive coding checks |
| Visualisation | VIZ | Plot data construction, dose normalisation, colour palette handling |

In addition, **24 manual tests** are defined in the script (Section MAN). These require a running app instance and cover interactive features such as file upload, column mapping, the half-life review inspector, and the Complete Analysis Record download. The manual test definitions are included in the script for traceability but are marked SKIP in automated runs.

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
ALL CRITICAL TESTS PASSED
URS: 49/49 covered
Results: validation/validation_results.csv
```

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

The documents are provided without version numbers in headers or filenames so that they can be incorporated directly into your local document management system with your own versioning scheme.

---

## File Integrity

The validation script computes SHA-256 hashes of `validation.R` itself and the five core R source files it sources (`R/utils.R`, `R/nca_helpers.R`, `R/data_quality.R`, `R/export_record.R`, `R/mod_data_upload.R`). These hashes are printed at the start of each run and recorded in `validation_results.csv`. Retain these alongside the results as evidence that the validated source files were not modified between qualification and use.

---

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen, The Netherlands*
