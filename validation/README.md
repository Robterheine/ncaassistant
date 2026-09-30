# NCA Assistant — Validation Package

This folder contains the validation package for NCA Assistant v1.8.0. It follows a risk-based approach consistent with ICH Q9 and GAMP 5 Category 5 principles for custom software used in a regulated pharmaceutical environment.

---

## Contents

| File | Description |
|------|-------------|
| `validation.R` | Consolidated validation script (Attachment A to the IQ/OQ/PQ protocol) |
| `make_iqoqpq.py` | Regenerates the test tables, traceability matrix and counts of the IQ/OQ/PQ protocol from `validation_results.csv` (needs python-docx) |
| `NCA_Assistant_URS.docx` | User Requirement Specification — 95 requirements across 9 categories, with a hazard-based FMEA |
| `NCA_Assistant_IQOQPQ.docx` | IQ/OQ/PQ protocol — approval before execution, a checklist for adopting organisations, every test listed individually with method, expected result, URS cross-reference, and criticality, and a template for the user's own PQ |
| `fixtures/` | Committed test data (crossover, replicate and ADNCA-shaped files, plus reference values from `replicateBE` the published parallel-group datasets, reference values for covariate adjustment and RSABE from independent Python implementations) and the deterministic scripts that generate them |
| `make_release_files.R` | Writes `renv.lock` and `release_manifest.csv` when a release is tagged |
| `renv.lock` | The package versions the release was validated with. `renv::restore(lockfile = "validation/renv.lock")` rebuilds that library. It sits here rather than in the project root, where rsconnect would pick it up when deploying |
| `release_manifest.csv` | SHA-256 of every file the app runs on (`app.R`, `R/`, `converters/`, `cdisc/`, `www/`, `gxp/`), with the app version. Checks IQ-REL-01 and IQ-REL-02 compare an installation with this file and with `renv.lock` |
| `validation_results.csv` | Generated on each run: pass/fail record with timestamps and environment details. Not committed, see below |
| `validation_environment.txt` | Generated on each run: R and package versions, the SHA-256 of every tested file, and `sessionInfo()`. Not committed |

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

**R packages.** The script installs nothing: when one of these is missing it stops. Install the validated versions first, from the project root, with `renv::restore(lockfile = "validation/renv.lock")` in R or `Rscript install_and_run.R --validated`.

| Package | Used for |
|---|---|
| `NonCompart` | the NCA itself |
| `PowerTOST` | sample size and power |
| `nlme` | mixed-effects bioequivalence models |
| `digest` | SHA-256 hashes |
| `openxlsx`, `jsonlite`, `readxl` | reading and writing record files |
| `dplyr` | data handling in the figure checks |
| `replicateBE` | validation only: the reference implementation the replicate-design and partial AUC checks compare against (sections REP and PAUC). The app never uses it |
| `shinymanager`, `DBI`, `RSQLite` | section GXP: login, the user store and the audit trail of controlled mode. The app loads them only in controlled mode |

The interface packages the app loads (shiny, bslib, shinyWidgets, DT, plotly, ggplot2, htmltools, tidyr) are checked but not installed by the script.

**Files.** Run the script from the project root: it reads the repository by relative path and does not copy anything into a temporary folder first. It needs

- `R/*.R` and `app.R` — the code under test, sourced directly, plus `APP_VERSION`;
- `converters/adnca_to_flat.R` — the standalone ADNCA converter (section CONV);
- `cdisc/ct_release.dcf` and `cdisc/pk_parameter_terms.csv` — the pinned CDISC release the parameter codes come from (checks EXP-CD-01..03 and REC-09);
- `data/example_theoph.csv`, `data/example_be_crossover.csv` and `data/example_blq.csv` — example datasets used by the NCA, record and BLQ checks;
- **`validation/fixtures/`** — required. Crossover, replicate and ADNCA-shaped test data, plus `replicateBE_reference.csv` with the committed `replicateBE::method.A` values, and `parallel_be_datasets.csv` and `parallel_be_reference.csv` with the 11 parallel-group datasets and the 90% confidence intervals published by Fuglsang et al. (doi:10.1208/s12248-014-9704-6). For covariates and RSABE: `cov_parallel_data.csv` and `cov_parallel_reference.csv` (Python statsmodels, made by `make_covariate_reference.py`), `rsabe_datasets.csv` (per-administration Cmax of replicateBE data sets, made by `make_rsabe_fixture.R`), `rsabe_reference.csv` (an independent numpy/scipy computation of Appendix G, made by `make_rsabe_reference.py`) and `rsabe_independent.R` (a second, matrix-based implementation in R). `make_example_datasets.R` generates the two example files of v1.8.0 in `data/` from fixed seeds. These files are read at the top level of the script, so a missing fixture stops the run with `cannot open file ...` rather than failing a single test: without the folder the run aborts partway and produces no results file.

Test data for crossover and replicate designs live in `validation/fixtures/`, not in `data/`. `make_fixtures.R` (crossover and replicate designs) and `make_adnca_fixtures.R` (ADNCA-shaped data) generate them deterministically, and `make_reference_values.R` records the matching `replicateBE::method.A` results; the generators and their outputs are both committed, so the suite runs without regenerating anything.

**Nothing else is needed to run the suite.** `make_iqoqpq.py` is only for regenerating the IQ/OQ/PQ protocol afterwards and needs Python with `python-docx`; the app's own runtime (a browser, a Shiny server) is not involved, because the script tests the code, not a running app.

On completion the script prints a results summary to the console and writes `validation/validation_results.csv`. That file is deliberately not committed: it is regenerated on every run and records the machine and environment of that run. The IQ/OQ/PQ protocol holds the committed record of a passing run.

---

## What the Script Tests

The script runs **585 automated tests** in thirty-two sections, each mapped to a URS requirement:

| Section | Code | Tests | Tests cover |
|---------|------|------:|-------------|
| Installation Qualification | IQ | 27 | R version, package availability (analysis and interface packages), every source file parses, file integrity (SHA-256 hashes), the installed files and package versions against the release manifest and lockfile |
| Data Handling | DAT | 63 | Column auto-detection, data quality checks, BLQ rules 1–6 per profile, BLQ text, study design detection, the shared data pipeline, interlocks (IL: CDISC-shaped flat files, mixed units, date/clock time, time since first dose, stacked profiles) and decimal-comma reading |
| NCA Accuracy | NCA | 40 | Analytical ground truth (mono-exponential IV bolus), Theoph and Indometh datasets, lambda-z, routes, trapezoid methods, dose normalisation, steady state, edge cases, manual data entry, crossover profiles |
| Bioequivalence | BE | 10 | CI construction, TOST logic, crossover ANOVA, mixed model, paired and parallel designs |
| Half-life overrides | OQ-NEW | 12 | R² propagation, recalculation by NonCompart, negative slope rejection, 2-point edge case, override log |
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
| Release review v1.5.0 | REL | 58 | One or more regression tests per finding of the five-reviewer review of v1.5.0 (R-01 to R-50), each built from the failing case: log-down AUC with an embedded zero, IV bolus with a time-0 sample, thousands separators in decimal-comma files, subject IDs per sequence, crossover without Period, settings kept across pages, results cleared on changed settings, widened limits for Cmax only, BLQ values kept out of the half-life, ICH M13A checks, units from the data, reproduction verdict with file integrity, Method B against `replicateBE`, an independent AUC calculation, colour contrast and keyboard access, locale-safe labels, Rule 4 Tlast, and more |
| Manual review 1.7 | MRV | 10 | App fixes from the review of user manual 1.7, each built from its case: a period without measurable concentrations counted as missing, BLQ-rule values and the lag time, Rule 6 on an all-BLQ profile, the M13A verdict notes and the batch pre-dose check, checks and data-copy notes in the Analysis Record, widened limits for Cmax and partial AUCs, wording, Ctau and steady-state blanks, labels and units of every column, the BLQ example file and the validated installation |
| Controlled mode | GXP | 47 | The audit trail (hash chain, triggers, tampering, truncation against an anchor and against a filed head in manage_users.R verify, three writers at once, fail-closed, clock warnings), manage_users.R (every command, refusals, archive and verification of an archived copy in a fresh R session, concurrent changes), login and roles (password rule, attempts logged, lockout alert, lockout and required password changes read from the audit trail, the required change made in the app's own dialog before the app starts), the audit hooks in every path, record storage with the data each record was made from, review signatures (every refusal, binding to the SHA-256 of the record shown in the dialog, a password change due or a reviewer role removed during the session, three failures end the session, a password changed during the session), signature sheet, signed bundle and validity, Verify a record file, the Exceptions queries, the users overview and manage_users.R list, the alerts for repeated failures, the signed trail review, role visibility, the password change, and the texts that depend on the mode |
| Adversarial audit v1.8.0 | ADV | 12 | One or more regression tests per finding of the external adversarial audit that held up on verification, each built from the failing case: names from the data file escaped in record summaries, values set by a BLQ rule kept out of a manual half-life fit, different '<x' limits listed instead of the lowest suggested, record folders removed after an error, a profile without a dose, steady-state fluctuation and swing within 0–τ, password expiry in UTC, a validation run that installs nothing, removal of the sign-in token at sign-out, R and package versions in the reproduction scripts, fonts served by the app, and fixed seeds for scaled-method planning |
| Data and statistics review | DSR | 9 | One test per finding of the review of data processing and statistics (D-1 to D-8), each built from the failing case: 0.250 in a decimal-comma file, a period with one measurable concentration, partial AUCs past the last measurable concentration in bioequivalence, negative pre-dose times, several analytes in one file, mostly-BLQ time points in the mean profile, dose-normalised steady-state metrics, a byte-order mark, and the profile-start rule of the ADNCA import (with a dataset without APERIOD) |
| Example datasets | EXM | 9 | Every bundled example loads through the upload path and is recorded as an example (also in controlled mode, with its SHA-256); only bundled files can be loaded or downloaded by name; a record made from an example says so and reproduces; the download serves the file unchanged; the two examples of v1.8.0 (a highly variable replicate study and a parallel study with covariates) give the numbers the tutorials quote and are what the committed generator makes from its seeds |
| Half-life quality flags | HLF | 6 | The span ratio and the rule limits at their boundaries; flags blank where the half-life is blanked, off at steady state for extrapolation, back-extrapolation for IV bolus only; rules switched off, changed, recorded and reproduced; manual fits flagged; flags counted in summaries and bioequivalence, excluding nothing; flags in words |
| Exclusions with a reason | EXC | 11 | An excluded sample equals deleting it from the file under every BLQ rule; matching by profile and time; an exclusion that no longer matches is reported; a profile exclusion keeps its NCA and leaves summaries and bioequivalence; records hold and reproduce the register; IV bolus, trough and partial AUC edge cases; ICH M13A checks on the data before exclusions; the sensitivity analysis; adding and restoring in the app; audit entries first; schema 1.3.0 settings still read |
| Adversarial review of the app | ARV | 10 | One test per finding that held up, each built from the failing case: a Dose column per kg (converted with the weight column, the Dose panel, a reproducing record), Cτ within a trough window, AUCτ extrapolated past the last sample, a 2×2 subject without both treatments in Method B, a pre-dose sample at a negative time, loading the exclusion register again with its timing, errors that stay on screen |
| Parallel-group bioequivalence | PAR | 5 | The 11 published datasets of Fuglsang et al. (AAPS J 2015): group sizes, the pooled-variance 90% CI and point estimate against the paper's consensus (Table II), the app's supplementary Welch CI against Table I, the Welch note appearing exactly where the two verdicts differ, and the Welch interval at the chosen confidence level and absent for crossovers |
| Covariate adjustment | COV | 16 | The parallel-group model with baseline covariates against Python statsmodels and matrix algebra, invariance to centering, rescaling, row order and level order, no change without covariates, every stop rule, the coefficient table, a seeded type I error and precision simulation, the group balance, the record, the audit event and the planner offer |
| FDA reference-scaled bioequivalence | RSA | 14 | Appendix G of the FDA guidance against two independent implementations (Python and R) on 13 replicateBE data sets, consistency with `PowerTOST::power.RSABE`, the switch at 0.294 and the point-estimate limits, complete cases, refused input, the assessment wrapper, the explanation lines, notes, record and audit, module wiring, and the fixes of the code review and the interface audit |
| EMA expanding limits | ABL | 4 | ABEL verdict, limits and sWR against `replicateBE::method.A` on its 30 reference data sets, widening for Cmax only with the cap at CVwR 50%, design rules and the point-estimate condition, exclusions |
| Text matches the code | DOC | 6 | The scope statement in every copy, the constants, limits and references on the Statistical Methods page, no em dashes, help and Data Guide against the approach selector, the requirement IDs of the URS document against the list behind the coverage line, and the counts in the READMEs, the version history, the protocol, the URS and the manual against this run |

In addition, **77 manual tests** are defined in the script (Section MAN). These require a running app instance and cover interactive features such as file upload (flat and CDISC ADNCA), column mapping, interlock messages, the half-life review and minimum-R² note, choosing the Reference treatment, the replicate variability table, planning with both CVs, CDISC parameter codes, partial AUC intervals in the batch and bioequivalence paths (including an invalid interval, a suppressed metric and the shaded figure), the Complete Analysis Record download and its reproduction check, the Visualize Figure Record, loading and downloading an example, exclusions in the app (including download and loading back), the half-life rules dialog and a Dose column per kg. They are included in the script for traceability but are marked SKIP in automated runs. The 13 MAN-GXP tests cover controlled mode: nothing runs before sign-in, the first sign-in, the header, the inactivity warning, the password change, sign-out, open mode unchanged, every path's audit entries, fail-closed behaviour, the Records page and signing dialog, the inspector account, the Audit trail page and restoring an archive on a clean machine. They need a test server set up as described in the user manual's appendix on controlled installations, not a laptop.

The execution record of the manual tests MAN-54 to MAN-64 of v1.8.0, run by an AI agent (the product owner waived personal execution and initials), is `MANUAL_TESTS_MAN-54-64_v1.8.0.md`; two screenshots are in `manual_evidence/`.

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
Total: 662 (auto: 585, manual: 77)
  PASS: 585 | FAIL: 0 | ERROR: 0 | SKIP: 77

ALL CRITICAL TESTS PASSED

URS: 95/95 covered (92 by automated tests; manual tests only: URS-BE-06, URS-BE-08, URS-PWR-04)

Results: validation/validation_results.csv
```

Of the 585 automated tests, 419 are CRITICAL and 166 SUPPORTIVE. The coverage line separates requirements covered by automated tests from those covered by manual tests only; the latter are met only once the manual tests have been carried out and recorded.

IQ-REL-01 and IQ-REL-02 pass only on an unchanged release: after any edit to a file listed in the manifest, IQ-REL-01 fails until `make_release_files.R` is run again for a new release. When IQ-REL-02 fails, its Detail column names each package whose version differs from `renv.lock`.

If any critical test fails, the script lists the affected test IDs under `CRITICAL FAILURES` and prints `STATUS: FAILED`. Supportive failures are counted separately and require a written risk assessment before the system can be signed off.

The `validation_results.csv` file records each test's ID, name, section, classification, result, URS reference, and any error message. This file is the primary evidence document for the qualification record.

---

## Adapting for Your Organisation

The validation package is provided as a starting point. Section 1.3 of `NCA_Assistant_IQOQPQ.docx` holds the full checklist. Before use in a regulated environment:

1. **Execute the validation script** in your target environment and retain the console output, `validation_results.csv` and `validation_environment.txt` as evidence. Install from a tagged release, so that IQ-REL-01 and IQ-REL-02 can confirm the files and package versions are the validated ones.
2. **Complete the manual OQ tests** in `NCA_Assistant_IQOQPQ.docx` using a running app instance. Record the actual results and tester signatures in the protocol.
3. **Carry out the user PQ** (section 5 of the protocol): analyse datasets like your own studies and compare the results with an independent reference.
4. **Review the URS** (`NCA_Assistant_URS.docx`) against your organisation's requirements. Add or remove requirements as appropriate and re-run the validation script to confirm coverage.
5. **Perform a risk assessment** for any SUPPORTIVE test failures or requirements not applicable to your use case.
6. **Retain all documents** (URS, IQ/OQ/PQ protocol, validation results, risk assessments) in your quality management system.

The documents name the application version they were produced for, but carry no document version of their own, in the file name or elsewhere, so they can go into your document management system under your own versioning scheme.

---

## File Integrity

The validation script computes SHA-256 hashes of `validation.R` itself and the core R source files it tests (`R/utils.R`, `R/nca_helpers.R`, `R/data_quality.R`, `R/export_record.R`, `R/mod_data_upload.R`, `R/designs.R`, `R/be_analysis.R`, `R/pipeline.R`, `R/interlocks.R`, `R/adnca_import.R`, `R/cdisc_terms.R`, `converters/adnca_to_flat.R`). These hashes are printed at the start of each run and written to `validation_environment.txt`. Check IQ-REL-01 compares every file the app runs on, including `www/`, with `release_manifest.csv`. Retain these alongside the results as evidence that the validated source files were not modified between qualification and use.

---

*Radboud Applied Pharmacometrics — Radboudumc, Nijmegen, The Netherlands*
