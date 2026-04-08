# NCA Assistant — Validation Package

## Contents

| File | Description |
|------|-------------|
| `validation.R` | Consolidated validation script (~169 automated tests + 24 manual test definitions) |
| `NCA_Assistant_URS_v1.1.docx` | User Requirement Specification (49 requirements) |
| `NCA_Assistant_IQOQPQ_v1.3.docx` | IQ/OQ/PQ Protocol with every test listed individually |
| `package_risk_assessment.R` | Supplier package risk assessment script |
| `package_risk_assessment.md` | Supplier package risk assessment report |

## How to Validate

### 1. Automated tests

From the project root:

```bash
Rscript validation/validation.R
```

This runs all automated tests and generates:
- `validation/validation_results.csv` — machine-readable test results
- `validation/validation_report.pdf` — self-contained PDF report (HTML fallback if tinytex unavailable)

The PDF report contains:
- Execution timestamp and environment details
- SHA-256 hashes of the validation script and all sourced files
- Per-section results with risk impact statements
- URS traceability summary
- Signature block
- Full source code of validation.R as Appendix A

### 2. Manual tests

20 tests require execution via the application UI. These are listed in:
- The IQ/OQ/PQ Protocol (Section 4)
- The validation script output (listed as SKIP)

For each manual test: launch the app, follow the procedure, record observations, and mark PASS/FAIL in the protocol.

### 3. Complete the protocol

1. Fill in the Test Environment table in the IQ/OQ/PQ Protocol
2. Attach `validation_report.pdf` as Attachment A
3. Record manual test results in Section 4
4. Document any deviations in Section 6
5. Sign the approval section

## Adapting for Your Organisation

- **Add your logo/header**: Edit the Word documents or modify the generation scripts
- **Organisational SOPs**: Add cross-references to your validation SOP and change control procedures
- **Additional tests**: Add check() calls to validation.R following the existing pattern
- **Revalidation**: Re-run validation.R after any code change, R upgrade, or package update
- **Regulatory context**: The URS references FDA, EMA, and ICH guidance. Adjust regulatory references to your jurisdiction.

## Acceptance Criteria

- **CRITICAL tests**: Must ALL pass for qualification
- **SUPPORTIVE tests**: Failures require documented risk assessment but do not block qualification
- **Manual tests**: Recorded in the protocol; failures are deviations

## Prerequisites

R >= 4.1.0 with packages: NonCompart, PowerTOST, nlme, digest, rmarkdown, knitr, openxlsx, jsonlite, readxl, dplyr.

For PDF report generation: tinytex or a LaTeX distribution. If unavailable, the script falls back to HTML.
