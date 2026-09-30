# Manual tests MAN-54 to MAN-64: execution record

Software: NCA Assistant v1.8.0, working tree at commit 38def2a (validation run: 584 automated tests passed, 77 manual skipped).
Date: 30 September 2026. Environment: macOS, R 4.6, the app run from the project folder on localhost, in the built-in browser of the Claude desktop app.
Tester: an AI agent (Claude Sonnet 5.5) following the procedure of each test in `validation/validation.R`. An agent is not an independent tester; see section 1.4 of the IQ/OQ/PQ protocol.
Evidence: the numbers below were read from the running app (tables, notes, downloads and the audit trail). Two screenshots are kept in `validation/manual_evidence/`. Downloads were fetched from the running session and opened in R.

| ID | Result | What was observed |
|---|---|---|
| MAN-54 | Pass | Badge "1 selected", line "Weight : numeric", "Adjusted for: Weight". Cmax 95.04% (90.12 to 100.22%), unadjusted columns 77.76 and 92.12; AUC to last point 98.13% (93.51 to 102.99%). Balance card: 68.16 (9.5) and 77.69 (12), standardized difference 0.88. Mapping offered by the app: Subject, Time, Conc, Treatment, Dose. Screenshot: MAN-54_covariates.jpg. |
| MAN-55 | Pass | Each of the four files gave its message under the selector before the run and again when Run was clicked, and no result was shown: "mixes numbers and text (for example '75.7 kg')", "uses a decimal comma (for example 75,7)", "more than one value for subject(s) 5", "values at or below zero, which have no logarithm". |
| MAN-56 | Pass | The report has the sheets BE_Covariates (Weight coefficient −0.01216 for Cmax) and Covariate_Balance. The CSV has Adjusted for, the two unadjusted columns and the unadjusted residual variance. analysis_settings.json lists Weight, numeric, none. The record's reproduction check says MATCH. |
| MAN-57 | Pass | 2×2×4 and 2×3×3: Standard, ABEL, RSABE. 2×2×3: Standard, ABEL, and an RSABE choice returned to Standard. No selector for 2×2×2 and parallel groups. With 70 as lower limit and RSABE chosen, the run stopped at once with the message to reset the limits to 80 and 125. |
| MAN-58 | Pass | Cmax: Scaled, s_WR 0.399, limits 70.04–142.78%, criterion bound −0.0446, YES. AUC: Standard, s_WR 0.251, YES. Explanation lines as in the tutorial. Forest plot limits: 70.04 and 142.78 for Cmax, 80 and 125 for AUC. The BE_Scaled sheet has both rows. Screenshot: MAN-58_rsabe.jpg. |
| MAN-59 | Pass | ABEL: Cmax CVwR 41.5%, limits 73.84–135.43%, 90% CI 77.52–100.69%, YES; AUC standard limits, YES. Standard approach: Cmax 88.35% (77.52–100.69%), NO. |
| MAN-60 | Pass | 20 subjects: "at least 24 evaluable subjects for RSABE (found 20 in the contrast)", result still computed. Subject 4 without period 2: the note about subjects left out of the contrasts. |
| MAN-61 | Pass | Open mode: BE_Scaled sheet and analysis_settings.json with analysis_approach "FDA RSABE", sigma_w0 0.25, theta 0.7967, switch 0.294, 90% and 95%, 24 subjects. Controlled mode on a local training server (not a qualified setup): the audit entry of the run carries the same approach object; the trail of 8 entries verifies as intact. |
| MAN-62 | Pass | At 375 px wide the page did not scroll sideways for the covariate run or the RSABE run; the tables stay inside their frame. |
| MAN-63 | Pass, with a limit | The text of both help buttons, the Methods page (sections, constants, references, scope statement) and the Data Guide (covariates note, approach text, both example downloads, identical to the files in data/) is as the test expects. The help popovers were read from their content attribute; they were not opened by hand. |
| MAN-64 | Pass, with a limit | Unticking Advanced options cleared the natural-log choice. A choice of ABEL left over from a replicate study gave no warning on a parallel study. The widened-limits panel is hidden under ABEL, and with the point-estimate box unticked earlier the ABEL table still shows PE within 80–125% = YES (applied). A study whose point estimate lies outside 80–125% with its interval inside the ABEL limits could not be built from the example data; that case is tested in RSA-13. |

Observation (not a defect): a typed acceptance limit stays in the field when new data are loaded, so the lower limit of 70 from MAN-57 was still there for the next study; the run then stopped with the clear message.

## Critical tests for the product owner

The product owner runs these four personally and initials them (decision 7 of the plan):

| ID | What to do | Initials and date |
|---|---|---|
| MAN-54 | Tutorial 4b with the covariate Weight | |
| MAN-58 | Tutorial 4c with FDA RSABE | |
| MAN-59 | Tutorial 4c with EMA ABEL and with the standard approach | |
| MAN-61 | The record and the audit trail of the RSABE run, on your own controlled test server | |
