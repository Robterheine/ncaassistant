# Validation

This folder contains the validation package for NCA Assistant v1.0, following a risk-based approach consistent with ICH Q9 (Quality Risk Management) and GAMP 5 Category 5 principles.

## Documents

| File | Description |
|------|-------------|
| `NCA_Assistant_URS_v1.0.docx` | User Requirement Specification — 44 requirements across 7 categories, with FMEA risk assessment, supplier assessment, and change control procedures |
| `NCA_Assistant_IQOQPQ_v1.0.docx` | Installation, Operational, and Performance Qualification protocol — 94 pre-filled test cases with sign-off pages |
| `iq_oq_pq_validation.R` | Automated test script that executes all programmatically verifiable IQ/OQ/PQ test cases |
| `package_risk_assessment.md` | Package risk assessment following the R Validation Hub (pharmaR) framework — classifies all dependencies, documents risk rationale |
| `package_risk_assessment.R` | Script to generate quantitative risk scores using `{riskmetric}` |

## How to Validate

### Step 1: Run the automated tests

From the project root directory:

```r
Rscript tests/iq_oq_pq_validation.R
```

This executes all test cases that can be verified without the Shiny UI. Each test is keyed to a protocol ID (IQ-01 through PQ-05). The script produces:

- Console output with PASS/FAIL/SKIP per test
- `tests/iq_oq_pq_results.csv` — attach this to the signed protocol as evidence

Tests marked SKIP require manual execution through the application interface (see Step 2).

### Step 2: Execute manual test cases

Open the application (`shiny::runApp()`) and work through the SKIP test cases listed in the IQ/OQ/PQ protocol. These cover:

- **UI tests**: central hub navigation (OQ-76), forest plot rendering (OQ-51), power curve display (OQ-62)
- **File handling**: Excel import (OQ-02), wrong file format error (OQ-22)
- **Export verification**: download the Complete Analysis Record ZIP and verify its contents (OQ-67 to OQ-72)
- **Visual inspections**: help text audit (OQ-74), Data Preparation Guide (OQ-75), Methods page (OQ-78)
- **Custom settings**: custom CI level (OQ-52), custom BE limits (OQ-53)

Record the observed result and mark PASS or FAIL in the protocol document.

### Step 3: Sign off

Complete the approval tables in both documents. If any test fails, follow the deviation handling procedure described in Section 1.4 of the IQ/OQ/PQ protocol.

## Adapting for Your Environment

The URS and IQ/OQ/PQ are provided as editable Word documents. If your organisation has specific requirements, you may:

- Add organisation-specific document control headers and approval workflows
- Adjust risk levels in the FMEA (URS Section 4) based on your intended use
- Add or remove test cases in the IQ/OQ/PQ to match your risk assessment
- Add SOPs for electronic signature management if required by your regulatory framework

If you modify the URS requirements, update the corresponding OQ/PQ test cases and the automated test script to maintain traceability.

## R Validation Hub (pharmaR) Framework

The `package_risk_assessment.md` document applies the industry-standard R Validation Hub framework to NCA Assistant's dependencies. This framework, described in the [pharmaR white paper](https://pharmar.org/white-paper/), is the consensus approach used by pharmaceutical companies to assess R packages for regulated use.

Key findings:

- NCA Assistant itself is the only "Intended for Use" system — all R packages are Imports whose accuracy is verified through the app's test suites
- nlme qualifies as low risk (R Recommended package, Trusted Resource)
- Tidyverse packages qualify as low risk (commercially backed by Posit, Trusted Resource candidate)
- NonCompart and PowerTOST are validated through 52 cross-validation tests against analytical ground truth and published reference values

To generate quantitative risk scores using `{riskmetric}`:

```r
Rscript validation/package_risk_assessment.R
```

This produces `validation/package_risk_scores.csv` for inclusion in audit documentation.

## Revalidation

Revalidation is required when:

1. R is upgraded to a new major or minor version
2. Any computational package (NonCompart, PowerTOST, nlme) is updated
3. Application code affecting computations, data handling, or export logic is modified
4. The application is deployed to a new hosting environment

For minor changes (UI text, help content), revalidation is not required. See URS Section 7 for the full change control procedure.

## Existing Test Suites

In addition to the IQ/OQ/PQ script, the application includes three automated validation suites in `tests/`:

| Suite | Tests | Coverage |
|-------|-------|----------|
| `cross_validation.R` | 52 | NCA vs. analytical ground truth, Theoph, BLQ rules, BE statistics, PowerTOST |
| `comprehensive_validation.R` | 84 | Settings propagation, pipeline robustness, sensitivity, audit trail, export integrity |
| `functional_ux_validation.R` | 103 | Column detection, messy data, end-to-end pipeline, numeric precision, CDISC |

These suites are referenced by the OQ test cases where applicable. Running all three suites (`Rscript tests/cross_validation.R`, etc.) provides additional confidence beyond the IQ/OQ/PQ protocol.
