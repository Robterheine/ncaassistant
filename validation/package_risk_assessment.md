# Package Risk Assessment

Risk-based assessment of NCA Assistant's R package dependencies, following the R Validation Hub's framework described in:

> Nicholls A, Bargo PR, Sims J. *A Risk-based Approach for Assessing R package Accuracy within a Validated Infrastructure.* R Validation Hub / R Consortium, 2020.  
> https://pharmar.org/white-paper/

## Framework Summary

The pharmaR white paper recommends classifying packages within an installation as either **Intended for Use** (loaded directly by users) or **Imports** (required by other packages but not called directly). Risk assessment focuses on Intended for Use packages and evaluates four criteria: purpose, maintenance practices, community usage, and testing.

## Package Classification

NCA Assistant is a Shiny web application. Users interact exclusively with the app's browser interface — they never call R package functions directly. From the pharmaR framework's perspective:

| Classification | Packages |
|---|---|
| **Intended for Use** | NCA Assistant itself (the Shiny app) |
| **Imports** | All R package dependencies listed below |

This is a strong validation position. The accuracy of every Import is verified indirectly through the app's 239 automated tests and 94-case IQ/OQ/PQ protocol, which test the app's exposed functionality end-to-end.

## Trusted Resources

The pharmaR white paper recognises the R Foundation as a Trusted Resource. All base and recommended packages qualify as low risk without further assessment.

| Package | Type | Role in NCA Assistant |
|---|---|---|
| nlme | R Recommended | Mixed-effects models for bioequivalence ANOVA |
| stats | Base R | Statistical functions (lm, anova, qt, etc.) |
| tools | Base R | File utilities |
| utils | Base R | General utilities |

**nlme** is the only recommended package with a direct computational role. It implements the linear mixed-effects models used in the BE module when the user selects "mixed-effects" as the model type. Because it is maintained by the R Foundation and ships with every R installation, it carries low risk per the pharmaR framework.

## Contributed Package Assessment

The table below classifies all contributed (non-base, non-recommended) package dependencies by purpose and documents the rationale for each risk assessment.

### Statistical Packages

Statistical packages implement algorithms whose errors are difficult to detect. The pharmaR white paper recommends that all statistical modelling functions within Intended for Use statistical packages be tested regardless of risk score. Since these are Imports (not Intended for Use), their accuracy is verified through the app's own test suites rather than through direct package testing.

| Package | Version | Purpose | pharmaR Risk Factors | Validation Evidence | Risk |
|---|---|---|---|---|---|
| NonCompart | 0.7.1 | NCA computation engine | Peer-reviewed (Kim et al., 2018, *Transl Clin Pharmacol*); CRAN-hosted; maintained by original authors; 6+ years on CRAN | Validated against analytical ground truth (1-compartment IV bolus); cross-validated against WinNonlin reference values; 52 cross-validation tests | LOW–MEDIUM |
| ncar | 0.5.0 | NCA reporting companion | Same author as NonCompart; CRAN-hosted; depends on NonCompart | Tested indirectly through NonCompart validation | LOW |
| PowerTOST | 1.5-7 | Power and sample size for BE | Peer-reviewed; CRAN-hosted; 10+ years; used by EMA/FDA reviewers; comprehensive vignettes; active bug tracker | Validated against published reference tables; 6 PowerTOST functions tested in cross-validation suite | LOW |

### Non-Statistical Packages

Non-statistical packages present lower risk because errors in data manipulation, display, or I/O are more likely to produce visibly incorrect output and be caught during routine use.

| Package | Version | Category | pharmaR Risk Factors | Risk |
|---|---|---|---|---|
| shiny | 1.13.0 | Application framework | Commercially backed (Posit); 10+ years; millions of downloads; extensive testing | LOW |
| bslib | 0.10.0 | UI theming | Posit-maintained; active development | LOW |
| htmltools | 0.5.9 | HTML generation | Posit-maintained; foundational package | LOW |
| shinyWidgets | 0.9.1 | UI widgets | CRAN; 5+ years; widely used | LOW |
| DT | 0.34.0 | Interactive tables | Posit-maintained; wrapper for DataTables.js | LOW |
| plotly | 4.12.0 | Interactive plots | Commercially backed (Plotly); CRAN; widely used | LOW |
| dplyr | 1.2.0 | Data manipulation | Posit/tidyverse; Trusted Resource candidate per pharmaR | LOW |
| tidyr | 1.3.2 | Data tidying | Posit/tidyverse | LOW |
| ggplot2 | 4.0.2 | Static plotting | Posit/tidyverse; most downloaded R package | LOW |
| scales | 1.4.0 | Scale functions | Posit/tidyverse | LOW |
| readxl | 1.4.5 | Excel import | Posit/tidyverse; no external dependencies | LOW |
| openxlsx | 4.2.8.1 | Excel export | CRAN; 8+ years; widely used in pharma | LOW |
| digest | 0.6.39 | SHA-256 hashing | CRAN; 20+ years; implements FIPS 180-4 standard | LOW |
| jsonlite | 2.0.0 | JSON serialisation | CRAN; 10+ years; foundational R JSON package | LOW |

### Tidyverse as Trusted Resource

The pharmaR white paper specifically discusses the tidyverse as a candidate for Trusted Resource status, citing its commercial backing by Posit (formerly RStudio), published design principles, consistent development practices, and popularity. NCA Assistant uses six tidyverse packages (dplyr, tidyr, ggplot2, scales, readxl, and — via dependency — rlang, vctrs, tibble). Following the white paper's guidance, these are assessed as low risk.

## Quantitative Risk Scores

The R Validation Hub provides the `{riskmetric}` package to compute quantitative risk scores based on the four assessment criteria. To generate scores for NCA Assistant's dependencies:

```r
Rscript validation/package_risk_assessment.R
```

This produces `validation/package_risk_scores.csv` with per-package scores. The script classifies each package by purpose (statistical vs. non-statistical) and computes the riskmetric composite score (0 = lowest risk, 1 = highest risk).

## Responding to Risk

Following the pharmaR framework's guidance on risk response:

**Low-risk packages** (all non-statistical packages, nlme, PowerTOST): No additional remediation beyond the existing system qualification tests. The pharmaR white paper states that "additional remediation for low risk packages is unlikely to yield any significant reduction in risk."

**Statistical packages with elevated risk** (NonCompart, if scored above LOW): Additional remediation is provided through:
- 52 cross-validation tests comparing against analytical ground truth and WinNonlin
- Reproducibility script (reproduce_analysis.R) that independently verifies results
- Interactive half-life inspector allowing users to visually verify λz estimation

## Reproducibility

The pharmaR white paper identifies reproducibility as a key challenge for R-based systems. NCA Assistant addresses this through:

- **renv** — all package versions locked in `renv.lock`; `renv::restore()` recreates the exact environment
- **Git** — source code version-controlled with tagged releases
- **Complete Analysis Record** — every analysis export includes the R script, settings, data hash, and package versions needed for independent reproduction

## Traceability

The white paper recommends logging loaded packages to ensure traceability. NCA Assistant's Complete Analysis Record captures:

- `analysis_settings.json` with package versions and timestamps
- `analysis_summary.html` with the full software environment description
- `reproduce_analysis.R` that explicitly loads the required packages

## Summary

| Criterion | NCA Assistant Status |
|---|---|
| Package classification | All dependencies are Imports; app is the only Intended for Use system |
| Trusted Resources | nlme (R Recommended); tidyverse packages (Posit-backed) |
| Statistical package testing | NonCompart and PowerTOST validated via 52 cross-validation tests |
| Quantitative risk scores | Available via `{riskmetric}` script |
| Reproducibility | renv lockfile; Git version control; tagged releases |
| Traceability | Analysis record with package versions, settings, data hash |
| System qualification | 239 automated tests + 94-case IQ/OQ/PQ protocol |

## References

- Nicholls A, Bargo PR, Sims J. A Risk-based Approach for Assessing R package Accuracy within a Validated Infrastructure. R Validation Hub, 2020. https://pharmar.org/white-paper/
- R Foundation. R: Regulatory Compliance and Validation Issues. A Guidance Document for the Use of R in Regulated Clinical Trial Environments. https://www.r-project.org/doc/R-FDA.pdf
- Kim H et al. NonCompart: Noncompartmental Analysis for Pharmacokinetic Data. *Transl Clin Pharmacol*. 2018;26(1):10-15.
- Labes D, Schütz H, Lang B. PowerTOST: An R Package for Bioequivalence Study Design. R package.
- Pinheiro JC, Bates DM. Mixed-Effects Models in S and S-PLUS. Springer, 2000. ISBN 0-387-98957-9.
