# Test Documentation

Complete documentation of all 239 automated tests across three validation suites. Each test is described by what it verifies, how it works, and why it matters.

**Logical location:** `tests/TEST_DOCUMENTATION.md` — lives alongside the test scripts it documents, renders natively on GitHub.

---

## Suite 1: Cross-Validation (52 tests)

**File:** `tests/cross_validation.R`

**Purpose:** Validates computational accuracy by comparing app output against known analytical solutions, reference datasets, published values, and independent manual calculations. This is the primary evidence that the NCA and BE engines produce correct results.

### Test 1: IV Bolus 1-Compartment — Analytical Ground Truth (6 tests)

A synthetic 1-compartment IV bolus dataset is generated with known pharmacokinetic parameters (dose=100 mg, Vd=10 L, ke=0.1 h⁻¹). The exact analytical solutions are: C₀=10 mg/L, AUC∞=100 mg·h/L, t½=6.931 h, CL=1 L/h, MRT=10 h. NCA is run via NonCompart and each parameter is compared to the true value.

| # | What | How | Why |
|---|------|-----|-----|
| 1 | Cmax matches C₀ | Compare NCA Cmax to dose/Vd; tolerance <0.5% | Verifies the engine correctly identifies the maximum concentration |
| 2 | AUCinf matches analytical | Compare to dose/(Vd·ke); tolerance <3% | Linear trapezoidal method overestimates AUC for declining curves; 3% tolerance accounts for this known bias |
| 3 | Lambda_z matches ke | Compare to 0.1; tolerance <0.5% | Verifies terminal phase regression recovers the true elimination rate |
| 4 | Half-life matches analytical | Compare to ln(2)/ke; tolerance <0.5% | Derived from λz; verifies the ln(2)/λz calculation |
| 5 | CL matches analytical | Compare to Vd·ke; tolerance <3% | CL = dose/AUCinf; inherits AUC tolerance |
| 6 | Vz matches analytical | Compare to Vd=10 L; tolerance <3% | Vz = dose/(λz·AUCinf); verifies the volume of distribution calculation |

### Test 2: Theoph Dataset — Oral NCA (4 tests)

Runs NCA on R's built-in Theoph dataset (12 subjects, oral administration). NonCompart was validated against WinNonlin in Kim et al. (2018); this test verifies the app produces the expected output structure and plausible values.

| # | What | How | Why |
|---|------|-----|-----|
| 7 | All 12 subjects analyzed | Check nrow(result) == 12 | Verifies no subjects are silently dropped |
| 8 | All Cmax positive | Check all CMAX > 0 | Sanity check on a well-known dataset |
| 9 | All AUClast positive | Check all AUCLST > 0 | Ensures AUC computation succeeds for all subjects |
| 10 | Lambda_z estimated for all | Check no NA values in LAMZ | Dense Theoph data should allow λz estimation for every subject |

### Test 3: Indometh Dataset — IV Bolus NCA (2 tests)

Runs NCA on R's Indometh dataset (6 subjects, IV bolus) with log-down trapezoidal method.

| # | What | How | Why |
|---|------|-----|-----|
| 11 | All 6 subjects analyzed | Check nrow == 6 | Validates IV bolus route with a second reference dataset |
| 12 | CL values positive and finite | Check all CLO > 0 and finite | CL = dose/AUCinf; validates the IV bolus clearance pathway |

### Test 4: Lambda Z Estimation — Custom vs NonCompart (2 tests)

Compares the app's custom `estimate_lambda_z()` function against NonCompart's internal λz estimation using the same IV bolus data.

| # | What | How | Why |
|---|------|-----|-----|
| 13 | Custom λz matches NonCompart | Compare values; tolerance <1% | The custom function is used in the interactive half-life inspector; must agree with the batch engine |
| 14 | R² near 1.0 for perfect data | Check R²adj > 0.9999 | Noise-free mono-exponential data should produce near-perfect regression |

### Test 5: BLQ Handling Rules (4 tests)

Tests four of the six BLQ rules using a dataset with known BLQ positions (concentrations below LLOQ=0.05). Each rule is applied and the resulting substitutions checked against expected values.

| # | What | How | Why |
|---|------|-----|-----|
| 15 | Rule 1: pre-first → 0 | Check first BLQ sample = 0 | WinNonlin default; most commonly used rule |
| 16 | Rule 1: post-last → NA | Check last BLQ sample = NA | Post-last-quantifiable samples should be excluded |
| 17 | Rule 1: mid BLQ → 0 | Check between-quantifiable BLQ = 0 | Between-phase BLQ samples set to zero |
| 18 | Rule 2: all BLQ → 0 | Check all BLQ positions = 0 | Conservative approach; maximises AUC |
| 19 | Rule 3: all BLQ → NA | Check all BLQ positions = NA | Exclusion approach; drops all BLQ samples |
| 20 | Rule 4: all BLQ → LLOQ/2 | Check all BLQ = 0.025 | Imputation approach; common in early-phase studies |

### Test 6: BE 90% CI — Crossover ANOVA vs Manual (4 tests)

Generates a 2×2 crossover dataset with known GMR=1.05 and CV=20%. Fits the standard ANOVA model (Sequence + Subject(Sequence) + Period + Treatment), computes the 90% CI, and cross-checks against an independent paired t-test calculation.

| # | What | How | Why |
|---|------|-----|-----|
| 21 | ANOVA GMR close to paired GMR | Compare two independent calculation methods; tolerance <2% | Cross-validates the ANOVA treatment coefficient extraction against paired differences |
| 22 | Estimated GMR within 10% of true | Compare to known 105% | With n=24 and CV=20%, simulation noise should keep the estimate close |
| 23 | 90% CI contains true GMR | Check 105% falls within CI bounds | CI coverage verification for a correctly specified model |
| 24 | Correctly concludes bioequivalence | Check CI within [80%, 125%] | True GMR=1.05 with adequate power should pass BE |

### Test 7: ANOVA MSE vs Known Variance (1 test)

| # | What | How | Why |
|---|------|-----|-----|
| 25 | MSE within 3× of true σ²w | Compare MSE to log(1+CV²) | The ANOVA MSE estimates within-subject variance; with n=24, some variability around the true value is expected |

### Test 8: PowerTOST — Published Reference Values (6 tests)

Compares PowerTOST sample size calculations against known correct answers for standard BE designs.

| # | What | How | Why |
|---|------|-----|-----|
| 26 | ABE 2×2 CV=0.20 θ₀=0.95 → N=20 | sampleN.TOST with exact method | Standard reference case; N=20 is the published answer |
| 27 | ABE 2×2 CV=0.30 θ₀=0.95 → N=40 | Same function, higher CV | Verifies sample size increases with CV |
| 28 | Power ≥ 0.80 at N=26 | power.TOST at fixed N | Verifies power calculation consistency with sample size calculation |
| 29 | ABE 2×2 CV=0.20 θ₀=1.00 → N=16 | No T/R difference | θ₀=1.0 requires fewer subjects; verifies sensitivity to GMR |
| 30 | Parallel N > crossover N | Compare parallel vs 2×2 results | Parallel designs require more subjects; validates design parameter |
| 31 | Non-inferiority N < equivalence N | Compare noninf vs TOST | One-sided test requires fewer subjects |

### Test 9: Trapezoidal Method Accuracy (2 tests)

Compares linear and log-down trapezoidal AUC against the exact analytical integral for a mono-exponential curve.

| # | What | How | Why |
|---|------|-----|-----|
| 32 | Log-down closer to exact than linear | Compare both errors | Log-down is theoretically exact for mono-exponential decay |
| 33 | Log-down error < 0.15% | Check relative error | For mono-exponential data, log-down should be nearly exact |

### Test 10: Dose Normalization (2 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 34 | Cmax_DN = Cmax/Dose | Check arithmetic to machine precision | Dose normalization is simple division; must be exact |
| 35 | AUCinf_DN = AUCinf/Dose | Same check for AUC | Consistency across all dose-normalized parameters |

### Test 11: Known Bioinequivalent Data (2 tests)

Generates crossover data with true GMR=0.70 (clearly bioinequivalent) and verifies the analysis correctly fails BE.

| # | What | How | Why |
|---|------|-----|-----|
| 36 | CI lower bound < 80% | Check lower CI < 80 | GMR=70% must fail the lower BE limit |
| 37 | Correctly fails BE criterion | Full [80%, 125%] check fails | Validates the app's ability to detect bioinequivalence |

### Test 12: Summary Statistics (4 tests)

Tests the `summarize_pk_params()` function with a known dataset (CMAX = 10, 20, 30, 40, 50).

| # | What | How | Why |
|---|------|-----|-----|
| 38 | N correct | Check N = 5 | Basic count |
| 39 | Mean = 30 | Arithmetic mean of known values | Validates arithmetic mean calculation |
| 40 | SD correct | Compare to R's sd() | Validates standard deviation |
| 41 | Geometric mean correct | Compare to exp(mean(log(x))) | Geometric mean is used in BE; must be accurate |

### Additional Tests: BLQ Rule 6, Fixed-Order, Steady-State, Reproducibility (11 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 42 | BLQ Rule 6: pre-first-quant → LLOQ/2, post → 0 | Check substitution values | Rule 6 for absorption-lag drugs; different from other rules |
| 43 | BLQ Rule 6: multi-subject independence | Two subjects with different BLQ patterns | Each subject's BLQ positions must be handled independently |
| 44 | Fixed-order crossover CI matches paired t-test | Compare ANOVA CI to t.test() | Fixed-order model (Subject + Treatment) is equivalent to paired t |
| 45 | Fixed-order crossover df = N−1 | Check residual df | Verifies correct degrees of freedom for the paired model |
| 46 | Steady-state: CL/F ratio matches AUC ratio | Compare SS vs SD CLFO and AUCinf/AUClast | At SS, CL/F = Dose/AUCτ instead of Dose/AUCinf |
| 47 | Steady-state: Vz/F ratio matches AUC ratio | Same comparison for volume | Consistent SS-specific parameter handling |
| 48 | Batch reproducibility script generates valid R | Parse generated script text | The exported R script must be syntactically valid |
| 49 | Batch reproducibility script produces matching results | Execute script; compare to direct NCA | The exported script must reproduce the app's results exactly |
| 50 | SHA-256 hash consistency | Hash the same file twice | digest::digest must produce identical hashes for identical files |
| 51 | Single-subject script generates valid R | Parse generated script | Single-subject export path must produce valid R code |
| 52 | Single-subject script produces matching results | Execute and compare | Single-subject exported results must match the app's direct output |

---

## Suite 2: Comprehensive Validation (84 tests)

**File:** `tests/comprehensive_validation.R`

**Purpose:** Tests settings propagation, data pipeline robustness, cross-path data flow, engine sensitivity, BE statistics, power calculations, audit trail integrity, label consistency, data quality gating, and export integrity. Covers the app's integration logic rather than just the computation engines.

### Section A: Statistical Accuracy (10 tests)

Re-validates the core NCA engine using the same 1-compartment IV bolus approach as the cross-validation suite, plus sNCA vs tblNCA consistency.

| # | What | How | Why |
|---|------|-----|-----|
| 1–4 | λz, t½, Cmax, AUCinf match analytical | Same as cross-validation Tests 1–6 | Redundant check in a separate suite for independent confirmation |
| 5–6 | Theoph: 12 subjects, all Cmax positive | tblNCA on Theoph | Dataset-level sanity check |
| 7–8 | All λz estimated for Theoph | Check for NAs | Ensures estimation succeeds with real-world data |
| 9–10 | sNCA matches tblNCA for single subject | Compare Cmax and LAMZ | Single-subject function must agree with batch function |

### Section B: Settings Propagation (5 tests)

Verifies that each UI setting actually reaches the NCA engine and changes the result.

| # | What | How | Why |
|---|------|-----|-----|
| 11 | Linear vs log-down AUC differ | Run NCA twice with different methods; compare AUClast | Proves the trapezoidal method setting propagates to NonCompart |
| 12 | Extravascular output has CLFO | Check column names | Route selection must produce correct parameter set |
| 13 | R²adj threshold=0 always estimates λz | Run with threshold=0 | Proves the threshold setting reaches the engine |
| 14 | Dose normalization divides by dose | Check CMAX_DN = CMAX/dose | Proves the dose normalization path is functional |
| 15 | Steady-state flag accepted | Run with SS=TRUE and SS=FALSE | Proves the SS flag doesn't crash the engine |

### Section C: Data Pipeline Robustness (14 tests)

Throws messy, edge-case, and extreme inputs at the NCA engine and data quality checker.

| # | What | How | Why |
|---|------|-----|-----|
| 16 | BLQ strings detected in QC | Data with "BLQ", "<0.5", "BQL" | Ensures the quality checker catches non-numeric concentration values |
| 17 | Negative concentrations warned | Data with conc = −0.5 | Negative values are unusual; should produce a warning |
| 18 | Empty dataset blocked | Zero-row data.frame | Prevents downstream crashes on empty input |
| 19 | All-NA concentrations caught | Subject with all NA conc values | Catches datasets with no usable data |
| 20 | Non-numeric time caught | Time column with "0h", "1h" | Catches unit strings accidentally left in time values |
| 21 | Duplicate times don't crash NCA | Data with repeated time=2 | NonCompart should handle this gracefully |
| 22 | Single observation doesn't crash | One row per subject | Edge case: NCA can't compute most parameters but shouldn't crash |
| 23–24 | 200 subjects completes in <30s | Generate and time batch NCA | Performance check: the engine must scale to large studies |
| 25 | Extreme values (1e8) no overflow | Cmax = 100,000,000 | Numerical stability with very large concentrations |
| 26–27 | Crossover composite key produces 4 profiles | 2 subjects × 2 treatments via run_nca | Validates the Subject||Treatment composite key mechanism |
| 28 | Single subject in crossover → 2 profiles | 1 subject, 2 treatments | Edge case for the composite key splitter |
| 29 | NULL input guards (startup) | Simulate NULL input$data_mode | Prevents crash when Shiny module runs before UI renders |

### Section D: Manual Entry Text Parsing (10 tests)

Simulates the text parsing logic used when users type concentration-time data directly into the app.

| # | What | How | Why |
|---|------|-----|-----|
| 30 | Normal input parsed correctly | 8 time/conc pairs, newline-separated | Baseline: normal input works |
| 31 | Windows line endings (\\r\\n) | CRLF line endings | Users on Windows may paste CRLF-terminated text |
| 32 | Extra blank lines ignored | Double newlines between values | Copy-paste from spreadsheets often introduces blank lines |
| 33 | Mismatched count detected | 5 times, 3 concentrations | Must reject when lengths don't match |
| 34 | Non-numeric values detected | "hello" in time column | Must reject non-numeric input |
| 35 | Fewer than 3 points rejected | Only 2 time/conc pairs | λz estimation requires ≥3 points; warn early |
| 36 | European decimal commas detected | "0,5" instead of "0.5" | Common European formatting error; must not silently misparse |
| 37 | Leading/trailing whitespace stripped | "  0  " and "  15  " | Copy-paste from formatted text includes whitespace |
| 38 | Trailing tabs stripped | "15\\t" | Copy-paste from Excel adds trailing tabs |
| 39–40 | Manual data → sNCA produces correct result | Parse then run sNCA | End-to-end manual entry path: text → numbers → NCA |

### Section E: Cross-Path Data Flow (4 tests)

Verifies that output from one module can be consumed by the next module in the workflow.

| # | What | How | Why |
|---|------|-----|-----|
| 41–42 | NCA output has Subject and Treatment columns | Check column names in crossover NCA output | The BE module merges on these columns; they must exist |
| 43 | CV computable from NCA Cmax | sd(log(Cmax)) is finite and positive | The power module needs within-subject CV from NCA results |
| 44 | PowerTOST accepts NCA-derived CV | Pass NCA CV to sampleN.TOST | End-to-end: NCA → Power bridge |

### Section F: NCA Engine Sensitivity (4 tests)

Verifies that changing a setting actually changes the result in the expected direction.

| # | What | How | Why |
|---|------|-----|-----|
| 45 | Log-down AUC < linear AUC for declining curve | Compare AUClast values | Log-down is tighter; validates the method produces the expected relationship |
| 46 | R²adj threshold changes λz estimation | Strict vs lax threshold | Proves the threshold isn't ignored |
| 47 | BLQ Rule 1 vs Rule 4 give different AUC | Compare AUClast after different BLQ handling | Different rules should produce measurably different exposure estimates |
| 48 | Dose changes CL but not Cmax | Compare NCA at dose=100 vs dose=200 | CL = dose/AUC, so CL changes; Cmax is observed, so it shouldn't change |

### Section G: BE Statistical Validation (5 tests)

Independent validation of the ANOVA and 90% CI computation, including mixed-effects models.

| # | What | How | Why |
|---|------|-----|-----|
| 49 | ANOVA GMR matches paired GMR | Two independent calculation methods | Cross-validates the treatment coefficient extraction |
| 50 | 90% CI contains true GMR (105%) | Check CI coverage | CI calibration check |
| 51 | Bioinequivalent data fails BE | GMR shifted down by factor 1.5 | Validates correct failure detection |
| 52–53 | Mixed-effects model converges and agrees with fixed | nlme::lme vs lm | Mixed model produces similar but not identical results; must converge |

### Section H: Power Calculations (7 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 54 | ABE 2×2 achieves ≥80% power | sampleN.TOST, check achieved power | Basic sample size functionality |
| 55 | Higher CV needs more subjects | Compare CV=0.30 vs CV=0.20 | Power decreases with CV; N must increase |
| 56 | Parallel needs more subjects | Compare parallel vs crossover | Crossover design removes between-subject variability |
| 57 | Power at calculated N ≥ target | power.TOST at the N from sampleN.TOST | Internal consistency: the calculated N must achieve the target |
| 58–62 | All 5 designs work | Loop over 2×2, 2×2×3, 2×3×3, 2×2×4, parallel | Every design the app offers must produce a valid result |

### Section I: Audit Trail & Reproducibility (5 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 63–65 | Same input → identical output (3 params) | Run NCA twice; compare CMAX, AUCLST, LAMZ | NCA must be deterministic; no randomness or state dependency |
| 66–68 | Package versions available | Check packageVersion() for NonCompart, PowerTOST, R | Version recording requires these to be queryable |

### Section J: Label Consistency (16 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 69 | No unexplained abbreviations in dropdowns | Scan module source for raw abbreviations as selectInput values | Users should never see "ABE", "NTID", etc. without explanation |
| 70–84 | Help topics exist | Check that 15 named help objects are defined | Every help tooltip referenced in the UI must have content |

### Section K: Data Quality Gatekeeper (4 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 85 | Clean data passes QC | 3 subjects, 6 timepoints each | Baseline: good data should not be flagged |
| 86 | Incomplete crossover detected | Subject S2 missing Reference treatment | Crossover completeness check protects BE analysis |
| 87 | Missing subject IDs caught | Blank and NA subject values | Missing IDs make NCA and BE grouping impossible |
| 88 | Sparse subjects warned | Subject S1 with only 2 observations | λz can't be estimated with <3 points |

### Section L: Export Integrity (6 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 89–92 | Summary statistics correct | Mean, SD, N, geometric mean for known data | Summary table in exports must be arithmetically correct |
| 93 | CDISC lookup has entries | Check cdisc_pk_names() | The CDISC parameter name mapping must be populated |
| 94 | fmt_pk handles NA | fmt_pk(NA) returns "—" | Formatted output must not show "NA" to users |

---

## Suite 3: Functional & UX Validation (103 tests)

**File:** `tests/functional_ux_validation.R`

**Purpose:** Tests the full user experience: column auto-detection across naming conventions, handling of real-world messy data, end-to-end pipeline from import through BE analysis, edge cases (large datasets, sparse data, extreme values), audit trail completeness, CDISC compatibility, PowerTOST input validation, UX defaults, steady-state NCA, and the data quality check engine.

### Section A: Column Auto-Detection (21 tests)

Tests the `auto_detect_columns()` function with seven different naming conventions: CDISC SDTM, clinical study, simple short names, WinNonlin export, lowercase minimal, European (German), and Cp-prefixed compound names. For each convention, Subject, Time, and Concentration detection are verified.

| # | What | How | Why |
|---|------|-----|-----|
| 1–3 | CDISC: USUBJID→Subject, NTIM→Time, DV→Conc | Pass CDISC column names to auto_detect | Regulatory-standard naming must be recognised |
| 4–6 | Clinical: SubjectID, TimePoint_h, Concentration_ng_mL | Common clinical study export format | Real-world naming with underscores and units |
| 7–9 | Simple: ID, Time, Conc | Minimal short names | Most basic column naming |
| 10–12 | WinNonlin: Subject, Timepoint, Result | WinNonlin export convention | Users migrating from WinNonlin expect this to work |
| 13–15 | Lowercase: subject, time, conc | All lowercase | Case-insensitive detection |
| 16–18 | European: Proband, Zeit_h, Konzentration | German naming (common in EU pharma) | Supports European users without column renaming |
| 19–21 | Cp prefix: PatID, Hours, Cp_ug_L | Compound concentration naming | "Cp" prefix shouldn't confuse the detector |

### Section B: Messy Real-World Data Handling (13 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 22–23 | BLQ strings → NA on conversion | Convert "BLQ" to numeric; count NAs | Validates the string-to-numeric pathway |
| 24 | BLQ Rule 4 on string-converted data | Apply LLOQ/2 after string cleanup | Tests the combined string-conversion + BLQ-handling pipeline |
| 25 | Negative concentrations don't crash NCA | tblNCA with conc = −0.3 | NonCompart must handle this gracefully |
| 26–28 | All-zero profile: no crash, Cmax=0 | tblNCA on all-zero data | Edge case: non-compliance or dosing failure |
| 29 | Single observation: no crash | One row per subject | Extreme sparse data |
| 30 | Duplicate time points: no crash | Repeated t=2 | Common data error |
| 31 | Non-monotonic time → valid after sort | Scrambled times, then sort and run NCA | The app sorts data before analysis; sorted data must work |

### Section C: End-to-End Data Pipeline (24 tests)

Generates a realistic 24-subject 2×2 crossover dataset with simulated PK profiles, then runs the full pipeline: column mapping → design detection → NCA → dose normalization → summary statistics → BE analysis.

| # | What | How | Why |
|---|------|-----|-----|
| 32 | Column mapping validates | validate_mapping() | First step in the pipeline |
| 33–36 | Design detection: crossover, N=24, 2 treatments, 2 periods | detect_study_design() | The app auto-detects study design from the data |
| 37–43 | NCA produces results for all profiles | run_nca() with composite key; check counts, columns, parameters | The crossover NCA engine must handle the Subject||Treatment composite key |
| 44–45 | No NA values in Cmax and AUClast | Check for NAs in key parameters | Simulated data is clean; all parameters should be estimable |
| 46 | λz estimated for >80% of profiles | Count non-NA LAMZ | Some profiles may have poor terminal phase; >80% is acceptable |
| 47–48 | Dose normalization columns created and correct | Check CMAX_DN = CMAX/dose | Verifies dose normalization works in the pipeline context |
| 49–51 | Summary statistics: N, geometric mean < arithmetic mean | summarize_pk_params() | Geometric mean < arithmetic mean for lognormal data |
| 52–55 | BE analysis: GMR plausible, CI width reasonable, df correct | lm() ANOVA on NCA results | Full NCA → BE pipeline on realistic data; df = N−2 for 2-sequence crossover |

### Section D: Edge Cases (8 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 56–58 | 200 subjects: completes, all in output, <30 seconds | Generate large dataset; time it | Performance and scalability |
| 59–61 | Sparse data: completes, both subjects present, S1 λz = NA | Subject S1 has only 2 time points | Verifies graceful degradation with insufficient data |
| 62 | All-NA concentrations: no hard crash | tblNCA on all-NA data | Must handle degenerate input |
| 63 | Extreme values (1e8): no overflow | Cmax = 100 million | Numerical stability |

### Section E: Audit Trail & Export (10 tests)

Verifies that all critical settings and metadata would be recorded in the analysis record.

| # | What | How | Why |
|---|------|-----|-----|
| 64–73 | Each audit field is non-null | Check file_name, LLOQ, BLQ rule, route, dose, units, method, R²adj, NonCompart version, R version | The analysis record must capture every setting that affects results |

### Section F: CDISC Compatibility (2 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 74 | All required CDISC parameters in NCA output | Check for CMAX, TMAX, AUCLST, AUCIFO, LAMZ, LAMZHL | Output must map to CDISC PK parameter names |
| 75 | CDISC lookup table has ≥10 entries | Check cdisc_pk_names() | The lookup table must be sufficiently populated |

### Section G: PowerTOST Input Validation (8 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 76 | CV=0 handled | sampleN.TOST with CV=0 | Invalid input must not crash the app |
| 77 | High CV (150%) completes | sampleN.TOST with CV=1.5 | Highly variable drugs have CVs above 100% |
| 78 | θ₀=θ₁ (boundary) errors or returns large N | θ₀=0.80 = lower limit | At the boundary, power=α; N→∞ is correct |
| 79–83 | All 5 designs produce results | Loop over design strings | Every design the app offers must work with PowerTOST |

### Section H: UX Defaults & Workflow (8 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 84 | Default settings produce valid NCA | run_nca() with app defaults on Theoph | A user who changes nothing should still get results |
| 85–86 | Missing Time column caught; error mentions "time" | validate_mapping() with empty time | Error messages must identify the problem specifically |
| 87 | Complete mapping accepted | All three required columns present | Positive test: valid input passes |
| 88–90 | fmt_pk: large numbers, small numbers, NA | Format various values | User-facing numbers must be formatted consistently |
| 91–93 | Pipeline: columns and types correct for downstream modules | Check Subject column, numeric CMAX and AUCLST | NCA output must be consumable by the BE module |

### Section I: Steady-State NCA (3 tests)

| # | What | How | Why |
|---|------|-----|-----|
| 94 | Steady-state NCA completes | tblNCA with SS=TRUE | SS mode must not crash |
| 95 | Cmax present | Check column exists | Basic parameter must still be computed |
| 96 | Cmax = 32.1 (observed max) | Compare to known maximum in test data | Cmax is always the observed maximum regardless of SS flag |

### Section J: Data Quality Check Engine (15 tests)

Comprehensive testing of the `run_data_quality_check()` function with various problematic datasets.

| # | What | How | Why |
|---|------|-----|-----|
| 97–99 | Clean data: passes, zero errors, has OK entries | 3 subjects, clean data | Positive test: good data should not be flagged |
| 100–101 | BLQ strings detected; LLOQ auto-detected from "<0.5" | Data with BLQ/BQL/ND and <0.5 | The checker must detect BLQ patterns and suggest LLOQ |
| 102–103 | Unrecognized text triggers error, fails QC | "hello" and "world" in conc column | Non-BLQ text is an error, not a warning |
| 104 | Missing subject IDs detected | Blank and NA values in Subject | Missing IDs prevent analysis |
| 105 | Negative concentrations warned | conc = −0.5 | Unusual but not fatal; warrants investigation |
| 106–107 | Empty dataset: error, fails QC | Zero-row dataframe | Must catch at the earliest possible point |
| 108 | Sparse subjects warned | Subject with 2 observations | λz requires ≥3 points |
| 109 | Duplicate times detected | Repeated t=2 | Can cause averaging errors or incorrect AUC |
| 110 | All-zero subject warned | Subject with all conc = 0 | May indicate non-compliance |
| 111 | Incomplete crossover detected | Subject S2 missing Reference | BE analysis requires data for both treatments |
| 112 | Non-numeric time triggers error | "0h", "1h" in Time column | Non-numeric values cannot be used in NCA |
| 113–114 | HTML quality report renders for clean and error data | render_quality_report() | The visual report must not crash regardless of input quality |
