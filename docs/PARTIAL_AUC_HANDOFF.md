# NCA Assistant: partial AUC (pAUC), assessment and handoff

**Audience:** the owner (decision) and a future session or maintainer (implementation).
**Written:** 17 September 2026, against app v1.4.0 (`main` at `cbbd4c6`).
**Status:** decisions made (2026-09-18, owner). Nothing is built yet. Section 5 lists the decisions and their resolutions.

Review team: statistician, clinical pharmacologist, R/Shiny engineer.

**Sources:**
- Hopefl R et al. A 2024 update on US FDA implementation of partial area under the curve into bioavailability and bioequivalence assessments. *Clin Pharmacol Ther* 2025;117:1185–93 (FDA white paper; cited below as Hopefl 2025).
- Periyasamy M, Ravichandran M. Partial area under the curve: a revelatory story in pharmacokinetics. *Clin Pharmacokinet* 2026;65:943–51 (review; cited below as Periyasamy 2026).
- Tsakiridou G et al. Partial AUCs in long-acting injectables: rationale, challenges, variability, usefulness, and clinical relevance. *Pharmaceutics* 2025;17:21 (review of six long-acting injectable case studies, FDA and EMA; cited below as Tsakiridou 2025). Its consequences are in section 4.6.

---

## 1. Short answer

**Can users calculate partial AUCs with the app today? No.**

The engine can, but the app does not expose it:

- **Engine.** `run_nca()` in `R/pipeline.R` accepts `settings$partial_aucs` (name, start, end) and passes it to NonCompart as `iAUC`. The steady-state AUCτ added in v1.4.0 uses exactly this route.
- **No way in.** Every module sets `partial_aucs = NULL`. There is no user interface, and `run_single_nca()` has no partial-AUC argument.
- **Not recorded.** The Analysis Record does not store intervals; `record_nca_settings()` also sets `partial_aucs = NULL`. A partial AUC would therefore not reproduce.
- **Not in Bioequivalence.** The parameter list is fixed (Cmax, AUCτ, AUClast, AUC∞, Tmax, half-life), so a partial AUC cannot be compared.
- **Missing pieces:**
  - no CDISC code;
  - nothing in the Methods page, manual, URS or validation.
- **Engine behaviour that needs handling (section 3):**
  - intervals ending at "t", the last measurable time;
  - extrapolation beyond the last sample;
  - log-transforming a partial AUC of zero.

**Recommendation.** Build it, as a focused feature (sections 4 and 6).

Partial AUCs are now standard BE metrics in FDA product-specific guidances (PSGs). Hopefl 2025 lists 18 new PSGs between August 2020 and August 2024, covering long-acting injectables, nasal sprays, inhalation powder, extended-release tablets and transdermal systems. Partial AUCs also appear in EMA product-specific and modified-release guidance.

The bioequivalence model in the app does not change. The work is in:
- defining the intervals correctly;
- guarding interpolation and extrapolation;
- handling zeros;
- recording and documenting everything.

---

## 2. What partial AUCs are used for (clinical pharmacology)

A partial AUC is the AUC over a time window that matters clinically, not over the whole profile. Regulators use it when Cmax and total AUC can be equivalent while the shape of the profile differs in a way that matters.

**How regulators choose the interval (Hopefl 2025, Table 1):**

| Rationale | Example | Metric | Role |
|---|---|---|---|
| Quick onset | Loxapine inhalation powder | AUC0–30min | Pivotal (Cmax supportive) |
| Quick onset | Loxapine inhalation powder | AUC0–10min, AUC30min–2h | Supportive |
| Quick onset | Naloxone and nalmefene nasal sprays | AUC0–4min, AUC0–10min, AUC10–30min | Supportive |
| Known PK/PD relationship | Amphetamine ER tablet | AUC0–4h, AUC4h–t | Pivotal |
| Known PK/PD relationship | Dextroamphetamine transdermal | AUC2–9h | Pivotal |
| Formulation (long-acting injectable) | Leuprolide, goserelin | AUC7days–t | Pivotal |
| Formulation (long-acting injectable) | Exenatide ER | AUCWeek4–t | Supportive |
| Formulation (long-acting injectable) | Afamelanotide implant | AUC0–96h (pivotal), AUC0–14days (supportive) | Pivotal and supportive |
| GI locally acting | Budesonide DR capsule | AUC4h–t | Pivotal |
| GI locally acting | Budesonide DR capsule | AUC0–4h | Supportive (fasting) |
| Abuse deterrent | Oxycodone/naloxone ER | AUC0–3h, AUC0–4h | Supportive |

**Things the app must respect:**

- **The intervals come from the protocol or PSG, never from the data.**
  - FDA intervals are fixed clock times, or end at "t", the last measurable time point (Hopefl 2025, Table 1 footnote).
  - For a single-dose study of a modified-release product, EMA's default is an early and a terminal partial AUC split at half the dosing interval, unless otherwise justified (Hopefl 2025, citing the EMA modified-release guideline).
  - Periyasamy 2026 also mentions a data-driven choice of the time point with the highest R² from a regression. The app should **not** offer that: choosing the metric from the same data inflates false conclusions of equivalence.
- **Pivotal or supportive.**
  - Pivotal metrics must meet 80.00–125.00%.
  - Supportive metrics are reported without a verdict. The loxapine PSG asks for Cmax, Tmax and early partial AUCs as supportive data.
- **Sampling at the cutoff.** PSGs ask for samples at the cutoff times; the loxapine PSG, for example, asks for enough samples to compare exposure in the first 10 minutes, 30 minutes and 30 minutes to 2 hours. When a cutoff falls between samples, the value is interpolated and depends on the interpolation method. The app should say so.
- **Early partial AUCs are variable.** For loxapine, the within-subject CV was above 30% for Cmax and AUC0–10min, but below 30% for AUC0–30min (Hopefl 2025). This is why the pivotal metric was AUC0–30min. Users planning such studies need the CV of the partial AUC, not of total AUC.
- **Early zeros and BLQ.**
  - Early partial AUCs are often zero or based on BLQ values.
  - The BLQ rule has a large effect: Rule 1 sets them to 0, Rule 6 to LLOQ/2.
- **Late partial AUCs near the LLOQ.** Long-acting injectables can have long plateaus close to the LLOQ. Tsakiridou 2025 (Table 5) reports that the between-subject CV of leuprolide 3.75 mg rises from 18% for AUC 72 h–t to 61–62% for AUC 5 d–t and AUC 7 d–t. That table is internally inconsistent, though: AUC 5 d–t has a mean of 254 and SD of 15, which is a CV of 6%, not 61%. Use it only as an illustration, not as a number to quote. Rule 1 sets BLQ values between quantifiable samples to 0, so a single BLQ result inside a low plateau creates a dip in the curve.
  - A partial AUC of exactly zero cannot be log-transformed.
- **Steady state.** Partial AUCs within τ (for example AUC0–4h at steady state) are legitimate. They must lie within 0–τ.
- **Replicate designs.** One partial AUC per administration, like every other parameter.

---

## 3. What the engine does now (verified 2026-09-17)

**Test.** NonCompart 0.8.0 `sNCA(..., iAUC = data.frame(Name, Start, End))`, with sampling times 0, 0.25, 0.5, 1, 2, 4, 8, 12 and 24 h, and concentrations 0, 5, 9, 12, 10, 7, 4, 2.4 and 0.6.

| Interval | Linear-down | Log-down | Hand check | Behaviour |
|---|---|---|---|---|
| 0–1 h (on sampling times) | 7.625 | 7.625 | 7.625 | Trapezoids on the samples |
| 0–0.75 h (between samples) | 4.812 | 4.812 | – | Cutoff value interpolated (linear while rising) |
| 0–3 h | 27.875 | 27.754 | – | Interpolation follows the `down` method while falling |
| 3–24 h (24 = Tlast) | 60.55 | 57.22 | AUClast − AUC0–3 = 60.55 | Correct |
| 20–30 h (end beyond Tlast) | 6.174 | 5.625 | – | **Extrapolated with λz** |

**Other behaviour** (`NonCompart::IntAUC`):

- **Start beyond Tlast:** the result is NA.
- **End beyond the last sample with no λz:** the result is NA.
- **Every concentration zero within the sampled range:** the result is 0.
- **Extrapolation uses NonCompart's automatic λz.** The app's minimum-R² rule blanks λz only afterwards, so a partial AUC can currently use a fit the app has rejected. This is the same issue that was handled for AUCτ in v1.4.0.
- **An interval ending at "t" cannot be passed.** `End` is one number for all profiles, and Tlast differs per profile.

---

## 4. Proposed design

### 4.1 Interval definition

The user enters one or more intervals. Each interval has:

| Field | Values |
|---|---|
| Start | Number ≥ 0, in the Time unit |
| End | A number > Start, or **"t (last measurable)"** |
| Role in Bioequivalence | Pivotal (verdict) or Supportive (no verdict) |

The column name is generated from the interval, for example `AUC_0_0.5` or `AUC_168_t`. The label reads "AUC 0–0.5 h".

No automatic cutoffs. One optional helper could be considered (**D3**): for a single-dose modified-release study, "split at half the dosing interval" fills in 0–τ/2 and τ/2–t from an entered τ. Its label would cite the EMA default and remind the user to follow the protocol.

Two design constraints come from section 4.6:
- The End (and Start) fields must accept per-profile values, not just one number, even if the first release only offers "t". Reference-Tmax cutoffs (D8) can then be added without redesigning the settings or the record format.
- Enter times in the dataset's time unit. For a long-acting injectable recorded in days, "24 h" is entered as 1. The label shows the unit; the help text gives this example.

### 4.2 Calculation (statistician and engineer)

- **Fixed end:** NonCompart `iAUC`, using the same trapezoidal method as the rest of the analysis. It is already wired through `run_nca()`.
- **End = t:** calculated per profile as AUClast − AUC0–Start, taking AUC0–Start from `iAUC`. The result is NA when Start > Tlast. This matches the definition "t = last measurable time" and never extrapolates.
- **End beyond Tlast.** Default proposal (**D1**): **not reported**, with a note naming the profiles. A partial AUC is meant to be an observed exposure metric. The alternative, allowed only if the owner decides so, is to extrapolate with λz, and then only with a fit that passes the minimum-R² rule.
- **Start before the first sample:** handled as for AUC (after extravascular dosing NonCompart assumes 0 at time 0).
- **Steady state:** intervals must lie within 0–τ, otherwise the analysis is refused with a message.
- **Warnings, not refusals:**
  - a cutoff that is not a sampling time in some profiles (the value is interpolated; say which method);
  - a partial AUC that is zero in some profiles.

### 4.3 Bioequivalence (statistician)

- **Model.** Partial AUCs join the existing parameter list and are analysed with the same model: log-transformed, EMA Method A or B, 90% CI, confidence limits rounded before the verdict. No new statistics are needed.
- **Verdict by role.**
  - Pivotal partial AUCs get a YES/NO verdict.
  - Supportive ones get the ratio and CI without a verdict, generalising `BE_NO_VERDICT_PARAMS`.
- **Several pivotal metrics:** each must pass on its own at the 90% level. This is an intersection-union test, so no multiplicity adjustment is needed. The Methods page should say so.
- **Zeros on the log scale (D2).** A partial AUC of 0 has no logarithm, and quietly dropping those profiles would bias the ratio. Proposal:
  - report the number of zero values per treatment;
  - give **no verdict** when any profile in the comparison has a zero partial AUC, with a message;
  - suggest checking the BLQ rule and the interval.

  Alternatives would need a documented justification: a small constant, a non-parametric comparison, or an untransformed difference. None is standard in the guidances reviewed.
- **Widened limits:** not applied to partial AUCs unless the protocol says so. The existing warning about pre-specified widened limits covers this.
- **Within-subject variability table** (replicate designs): partial AUCs can be included, for information.
- **Plan a Study:** no change needed; partial AUCs use the same TOST calculation. Add help text saying to plan with the partial AUC's CV, which is often higher than total AUC's (loxapine example).

### 4.4 User interface (engineer)

- **All Subjects, One Subject at a Time and Bioequivalence settings:** a collapsible "Partial AUCs" section with a small interval table.
  - Add and remove rows.
  - Start, End (number, or tick "to last measurable").
  - A Role column, shown in Bioequivalence only.
  - Interval validation: End > Start, no duplicates, within τ at steady state.
- **Results:**
  - partial AUC columns in the parameter table and the Excel/CSV downloads;
  - summary statistics per treatment;
  - Bioequivalence parameter choices built from the defined intervals.
- **Visualize Data (optional, D5):** shade the intervals on the mean plot.
- **Help popover:** what a partial AUC is, where cutoffs come from (protocol/PSG), the "t" option, interpolation and extrapolation, and zeros. Written in the app's beginner-friendly style.

### 4.5 Records, reproduction and CDISC (engineer)

- **Pre-specification.** Post-hoc partial AUCs failed in 41% of 117 passing ANVISA studies and in 47% of 53 passing modified-release studies (Tsakiridou 2025, citing Soares and Boily). The HTML report should state that the intervals are recorded as entered and that the app cannot check they were pre-specified in the protocol.

- **Settings:** `analysis_settings.json` gains `partial_aucs` (start, end or "t", role, column name). `record_nca_settings()` passes the intervals, so `reproduce_analysis.R` recalculates them and `app_results_reference.csv` includes them. The existing MATCH check then covers partial AUCs automatically.
- **Pipeline:** the pipeline changes (`R/pipeline.R`) ship in the record as `nca_pipeline.R`.
- **CDISC (D4):**
  - SDTM PP represents a partial AUC as PPTESTCD **AUCINT** ("AUC from T1 to T2"), with the interval in PPSTINT/PPENINT (PharmaSUG 2012 DS10; pkpdutils).
  - **Verify** that AUCINT is in the pinned CT release 2026-03-27 by re-running `cdisc/extract_pk_terms.R` with `AUCINT` added to `cdisc/pk_parameter_map.csv`.
  - The code sheet should list the interval next to the code; an "End = t" interval gives PPENINT as the last measurable time.
  - Scope stays as decided in `CDISC_ROADMAP.md` §4.4: a code lookup, not a PP dataset.

### 4.6 Consequences of Tsakiridou 2025 (long-acting injectables)

The paper reviews six long-acting injectables: buprenorphine, naltrexone, octreotide, lanreotide, exenatide and leuprolide. It does not change the core design (sections 4.1–4.5). It does widen the metrics and warnings needed.

| # | Finding in the paper | Consequence for the app |
|---|---|---|
| 1 | **Cutoffs based on Tmax.** Three approaches are in use: the reference product's population median Tmax (FDA 2003); each subject's own reference Tmax (Health Canada, AUCRefTmax); and the earlier of the subject's two Tmax values. | The first is a fixed number the user can type. The other two differ per subject and need the Reference treatment, so they only make sense in Bioequivalence. Proposal **D8**: not in the first release, but design the interval fields for per-profile ends (section 4.1). |
| 2 | **Cmax per release phase.** EMA asks for Cmax,1 and Cmax,2 for exenatide, and Cmax within each partial AUC region as secondary metrics for octreotide. | A partial AUC alone does not cover this. Proposal **D9**: an optional "Cmax in interval" column per interval (observed maximum, no interpolation, with its time), compared in Bioequivalence like other metrics. |
| 3 | **Cτ after a single dose.** EMA proposes Cτ, the concentration at the end of the dosing interval, for octreotide (study lasting 2τ). | Not a partial AUC. Proposal: out of the first release (section 8). A later option could give the observed concentration at a stated time, with no interpolation. |
| 4 | **Intervals beyond τ after a single dose.** Octreotide needs AUC 0–28 d and AUC 28–56 d; lanreotide (EMA) AUC 0–7 d, 7–28 d and 28 d–t. | Already covered: the 0–τ check applies only at steady state. Add a validation test that contiguous partial AUCs (0–a, a–b, b–t) add up to AUClast. |
| 5 | **High variability.** Between-subject CVs of 37–109% for partial AUCs (buprenorphine 0–3 d 60%; lanreotide 28 d–t 109%; leuprolide 7 d–t 62%). The paper's sample sizes (Table 6: 37 and 79 in a 2×2 crossover at CV 20% and 30%) are odd numbers, which a balanced 2×2 cannot have. PowerTOST gives 38 and 80 (θ0 0.90, power 80%). Quote the PowerTOST figures, not the paper's. | Plan a Study help text: use the partial AUC's own CV; above 30% within-subject, consider a replicate design. The replicate-design variability table should include partial AUCs. The earlier plan already said this; the paper adds numbers for the help text and manual. |
| 6 | **Widened limits for highly variable partial AUCs.** The paper says regulators allow widening for highly variable partial AUCs, but cites only the general FDA 2021 draft ANDA guidance and EMA guidance, without the exact clause. | The app does average bioequivalence only. Proposal **D10**: show the informational expanded EMA limits for a partial AUC only after checking the EMA modified-release guideline wording (section 9). Until then, show CVwR without implied limits. |
| 7 | **Plateaus near the LLOQ** make late partial AUCs variable (leuprolide). | Proposal **D11**: per interval and profile, count the concentrations set by the BLQ rule. Warn when an interval depends mainly on them. `apply_blq_rules()` now drops the BLQ flag (`data$.is_blq <- NULL`), so the flag must be kept for this. |
| 8 | **Post-hoc cutoffs fail often** (41% and 47%; see section 4.5). | Confirms D7 (no data-driven cutoffs). Add the pre-specification sentence to the report, help and manual. |
| 9 | **Time scales of days to months** (dosing intervals of 1, 3 and 6 months). | Enter intervals in the dataset's time unit (section 4.1). Add a validation fixture shaped like a long-acting injectable (burst, then a plateau near the LLOQ, time in days). |
| 10 | **Paper's own method:** arithmetic CV and trapezoids on digitised mean curves. | No consequence: the app works on individual data. Do not copy the paper's arithmetic CVs into the app as defaults. |

---

## 5. Decisions

Decided by the owner on 2026-09-18.

| # | Decision | Resolution |
|---|---|---|
| D1 | End time beyond the last sample | **Not reported**, with a note naming the affected profiles (no extrapolation) |
| D2 | Partial AUC of zero in a Bioequivalence comparison | **No verdict** for that metric when any profile has a zero; report the count per treatment, with a message |
| D3 | EMA "split at τ/2" helper for single-dose modified-release studies | **Leave out.** Users enter intervals from the protocol/PSG only; a default-filling helper is one step from "the app chose my cutoff" (the risk D7 exists to avoid) |
| D4 | CDISC code AUCINT | **Add**, after verifying it is in the pinned CT release |
| D5 | Shading intervals in Visualize Data | **Include**, phase 5 (last, after the core feature is stable) |
| D6 | Paths that get partial AUCs | **All Subjects, One Subject at a Time, Bioequivalence** (not Plan a Study, which only needs a CV) |
| D7 | Data-driven cutoff selection (Periyasamy 2026) | **Do not offer** (Tsakiridou 2025 supports this, section 4.6 #8) |
| D8 | Per-subject Reference-Tmax cutoffs (Health Canada) | **Not in the first release**; design the interval fields so it can be added later without a redesign |
| D9 | "Cmax in interval" per interval (EMA exenatide, octreotide) | **Include**, as an optional column: observed maximum in the interval and its time, no interpolation, compared in Bioequivalence like other metrics. This is a second per-interval metric, not just pAUC — plan for it in Phase 1 (calculation) and Phase 3 (BE comparison), not as a bolt-on |
| D10 | Expanded EMA limits for highly variable partial AUCs (information only) | **Defer.** Show CVwR without implied widened limits in v1; revisit only once someone verifies the EMA modified-release guideline wording (section 9). Do not block Phase 1 on this research |
| D11 | Warning when an interval depends mainly on BLQ-derived values | **Include.** Remove the `data$.is_blq <- NULL` line in `apply_blq_rules()` so the BLQ flag survives through the pipeline. This touches shared code used by every analysis path, not just pAUC — treat it as its own small, tested change within Phase 1 |

---

## 6. Implementation phases

Each phase follows the project's working method:
1. write failing tests first;
2. fix;
3. run the full validation suite;
4. check the change in the running app;
5. commit.

**Phase 1: engine (3–4 days; includes D9 and D11)**
- Validate an interval specification in `R/pipeline.R` (`validate_partial_aucs()`).
- Calculate fixed-end intervals with `iAUC` and "t" intervals as AUClast − AUC0–Start.
- Apply the D1 rule beyond Tlast; make sure a rejected λz is never used.
- Enforce the steady-state 0–τ check.
- Wire the same logic into `run_single_nca()`.
- Emit warnings for off-grid cutoffs and zeros.
- **D9:** calculate the observed Cmax and its time within each interval (no interpolation).
- **D11:** remove the `data$.is_blq <- NULL` line in `apply_blq_rules()` so the flag survives; count BLQ-derived concentrations per interval/profile and warn when an interval depends mainly on them.
- **Tests:**
  - hand-calculated trapezoids (linear and log-down);
  - interpolated cutoffs against NonCompart;
  - "t" intervals, including Tlast < Start;
  - end beyond Tlast;
  - intervals across steady state;
  - identical results in batch and single paths;
  - Cmax-in-interval against a hand check;
  - BLQ flag survives `apply_blq_rules()` and the dependency warning fires on a constructed plateau.

**Phase 2: records and reproduction (1 day)**
- Record the intervals in the settings JSON and in `record_nca_settings()`.
- **Tests:** batch, single and Bioequivalence records with partial AUCs reproduce (MATCH); a changed interval is detected.

**Phase 3: Bioequivalence (2 days; includes D9)**
- Build the parameter choices from the defined intervals.
- Give each metric its role: pivotal gets a verdict, supportive does not.
- Apply the D2 zero handling.
- Include partial AUCs in the variability table.
- **D9:** compare Cmax-in-interval the same way as other metrics (log-transformed, 90% CI), following each interval's pivotal/supportive role.
- **Tests:**
  - partial AUC CI equals `lm` on the log values;
  - supportive metrics get "no verdict";
  - zeros give no verdict, with a count;
  - replicate design agrees with replicateBE on an interval (compute replicateBE's `PK` from the same partial AUCs);
  - Cmax-in-interval comparison matches a hand-calculated CI.

**Phase 4: user interface (2 days)**
- Interval editor in the three paths, results columns, downloads, summary statistics.
- Help popover and the optional D3 helper.
- Browser check: define intervals, run, check the table and downloads, generate a record.

**Phase 5: CDISC and figures (0.5–1 day, optional)**
- Verify AUCINT and add it to the map and code sheet (D4).
- Shading in Visualize Data (D5).

**Phase 6: documentation and app text (2–3 days)**
Section 7 has the full list. Two rules:
- Do this pass once, after phases 1–5 are merged and the validation run is green. Documents quote test counts and IDs, so writing them earlier means redoing them.
- Use the humanizer skill on new prose, as for manual v1.5.

**Total estimate: 11–13 working days** (D9 and D11 are in scope; the range reflects UI and documentation effort).

---

## 7. Documentation and app text: what changes where

**Terminology rule (all documents).** The app already uses "partial" for the 2×3×3 *partial replicate* design. Always write "partial AUC" in full, never "partial" alone. In tables, write "pAUC" only after defining it once. Write intervals as "AUC 0–30 min" and "AUC 168 h–t", with an en dash and the unit.

### 7.1 Statistical Methods page (`R/mod_methods.R`)

| Tab | Change |
|---|---|
| Non-Compartmental Analysis | New subsection **"Partial AUC"** after "Area Under the Curve". Cover five points (below). |
| Steady-State | One sentence: partial AUCs at steady state lie within 0–τ. AUCτ is itself a partial AUC and uses the same calculation. |
| BLQ Handling | Two sentences: the BLQ rule strongly affects early partial AUCs (Rule 1 zeros compared with Rule 6 LLOQ/2), and late partial AUCs over plateaus near the LLOQ (Rule 1 sets BLQ values between quantifiable samples to 0). Choose and justify the rule in the protocol. |
| Bioequivalence | New subsection **"Partial AUCs"** before "Bioequivalence Conclusion". Cover four points (below). Add Hopefl 2025 and the EMA modified-release guideline to the regulatory references. |
| Power | One sentence: plan with the CV of the pivotal partial AUC, which can be much higher than for total AUC. Cite the loxapine example. |
| Example Paragraph | Add a **"Partial AUC"** paragraph with [bracketed] interval, role and BLQ rule, as in the other examples. Add the references to "References for your manuscript". |

**Points for the NCA subsection:**
1. Definition.
2. Intervals come from the protocol or product-specific guidance.
3. Interpolation at cutoffs that are not sampling times, following the chosen trapezoidal method.
4. The "t" end (calculated as AUClast − AUC0–start).
5. The D1 rule beyond the last sample.

**Points for the Bioequivalence subsection:**
1. Same model and CI as the other metrics.
2. Pivotal gets a verdict, supportive gets none.
3. Several pivotal metrics each must pass (intersection-union test; no multiplicity adjustment).
4. The D2 zero rule and why (log of 0 is undefined; dropping zeros biases the ratio).

The tests guarding the Methods page (REV3-10, REV3-13, REV4-07) check specific wording. Extend them with anchors for the new subsections.

### 7.2 Data Preparation Guide (`R/mod_data_guide.R`)

- **"Several Subjects" and "Standard 2×2 Crossover":** a short box, "Planning a partial AUC". Samples must be taken at the cutoff times; otherwise the value is interpolated and depends on the method.
- **"Multiple Dosing at Steady State":** partial AUC intervals must lie within τ.
- **"Working with Real Bioanalytical Data":** use actual sampling times. Planned times at the cutoff are fine only if the protocol says so.
- **"Common Data Preparation Mistakes":** add two mistakes:
  - a cutoff with no sample near it;
  - early BLQ values left as text, so they become missing instead of the protocol's BLQ rule.
- **No new example dataset needed.** `data/example_be_crossover.csv` samples at 0, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12 and 24 h. It can demonstrate an early interval on sampling times (AUC 0–1 h) and a terminal interval ending at t (AUC 4 h–t).

### 7.3 In-app help and labels

| File | Change |
|---|---|
| `R/help_system.R` | New popover **`help_partial_auc`**, "What is a partial AUC?". Cover: what it is, where cutoffs come from, the "t" option, interpolation, what happens past the last sample, zeros, pivotal and supportive. A popover of that name was removed earlier as dead; reuse the name. |
| `R/help_system.R` | `help_cv`: one sentence on using the partial AUC's own CV. |
| `R/utils.R` `pk_param_labels` / `friendly_name()` | Labels are a fixed lookup, so generated names like `AUC_0_0.5` need a rule. `friendly_name()` should turn them into "Partial AUC 0–0.5 h" (URS-UI-01 requires plain-language labels). |
| `R/utils.R` `cdisc_codes_ui()` and `R/export_record.R` `add_cdisc_code_sheet()` | Map every partial AUC column to AUCINT, and show the interval (PPSTINT/PPENINT) in the code table (D4). |
| `cdisc/pk_parameter_map.csv` | A pattern row for AUCINT (or handle it in code), with a note on the interval columns. |
| `R/mod_path_be.R` | Units: partial AUCs get the AUC unit in the result table. Help text: supportive metrics are "compared without a verdict", as for half-life. |
| `R/data_quality.R` | No new checks at upload. Interval warnings belong to the analysis step (Phase 1), because the intervals are not known at upload. |
| `R/export_record.R` HTML report | One paragraph listing the intervals, their role and the D1/D2 rules, like the existing steady-state paragraph. |
| `app.R` | Version bump (1.5.0: new feature), version history entry, and the About page feature list. |

### 7.4 URS (`validation/NCA_Assistant_URS.docx`)

**New requirements:**

| ID | Requirement (draft) | Risk |
|---|---|---|
| URS-NCA-13 | The system shall calculate partial AUCs over user-defined intervals. End is a time or the last measurable concentration (t), with the trapezoidal method of the analysis. A partial AUC that needs data beyond the last sample shall not be reported. | High (Sev 10, like NCA-01) |
| URS-NCA-14 | At steady state, the system shall refuse partial AUC intervals outside 0–τ. | Medium |
| URS-BE-10 | The system shall compare partial AUCs in the bioequivalence analysis. It shall give a verdict only for intervals marked pivotal, and no verdict when any profile in the comparison has a partial AUC of zero. | High (Sev 10, like BE-04) |
| URS-EXP-08 | The Analysis Record shall store partial AUC intervals and roles, and the reproducibility script shall recalculate the partial AUCs. | High |

**Existing requirements to widen:**
- URS-GEN-03 (Methods page covers partial AUCs)
- URS-GEN-06 (AUCINT code)
- URS-UI-01 (labels for generated names)
- URS-PWR-01 help text (optional)

**Other URS sections:**
- **FMEA (section 4):** add FM-NCA-13, FM-NCA-14, FM-BE-10 and FM-EXP-08, with the same scoring convention.
- **Traceability (section 6):** add the new IDs.
- **Regulatory references (section 8):** Hopefl 2025; EMA Guideline on the pharmacokinetic and clinical evaluation of modified release dosage forms (EMA/CPMP/EWP/280/96 Rev 1); FDA product-specific guidances as a class.
- **Glossary (section 9):** "Partial AUC", "Pivotal metric", "Supportive metric".
- **Totals:** the requirement count goes from 58 to 62.

### 7.5 Validation script and IQ/OQ/PQ protocol

**`validation/validation.R`: a new section `PAUC`.** Suggested tests (IDs PAUC-01…):

| Area | Tests |
|---|---|
| Calculation | Hand-calculated trapezoids on sampling times, linear and log-down. Cutoff between samples, compared with NonCompart and with a hand interpolation. |
| "t" end | Equals AUClast − AUC0–start. NA when start > Tlast. Differs per profile when Tlast differs. |
| Beyond Tlast | Not reported (D1). A rejected λz is never used. |
| Steady state | An interval outside 0–τ is refused. 0–τ equals AUCτ. |
| Consistency | Batch, single-subject and BE paths give identical values. |
| Bioequivalence | CI equals `lm` on log values. Pivotal gives YES/NO; supportive gives no verdict. Zeros give no verdict with a count. Replicate design agrees with replicateBE on the same partial AUC values. |
| Records | Settings JSON holds intervals. The reproducibility script gives MATCH. A changed interval is detected. |
| Labels and codes | `friendly_name()` label. AUCINT code with interval. |
| Text | Methods page and help popover anchors (like REV3-10). |

**New manual tests (MAN-45 onward):**
- define intervals in the batch path and check the table and download;
- enter an invalid interval (End ≤ Start, or outside τ) and check the error message;
- BE with one pivotal and one supportive partial AUC: forest plot and verdict column;
- generate a record and run the script (MATCH);
- read the help popover.

**`NCA_Assistant_IQOQPQ.docx`:**
- regenerate with the `iqoqpq2.py` generator from the new `validation_results.csv`;
- add heading 3.17, "Partial AUC (OQ/PQ)";
- update the traceability matrix (section 5) and the summary counts in section 7 (tests, automated, manual, URS coverage 62/62).

The generator script lived in the session scratchpad. Copy it into `validation/` this time so it is not lost.

**`validation/README.md`:** new section name, new counts, and which PSG-style cases are covered.

### 7.6 User manual (`NCA_Assistant_User_Manual_v1.5.docx` → v1.6)

| Part | Change |
|---|---|
| Document Control | v1.6, change summary |
| Part I, "Key Pharmacokinetic Parameters" | Short entry: partial AUC, with the loxapine and long-acting injectable examples as illustrations |
| Part II, "Scenario 4" and "Scenario 7" | Sampling at cutoff times; steady-state intervals within τ. "BLQ Handling": effect on early partial AUCs. "Common Mistakes": the two new mistakes. |
| Part III, "Path 4", "Path 5", "Path 6" | How to add intervals, the "t" option, the role column in BE, what "no verdict" means, the zero and past-last-sample messages. "Complete Analysis Record": intervals are recorded. |
| Part III, "Path 1: Plan a Study" | Use the partial AUC's CV |
| Part IV | **Exercise 4: Partial AUC**, using the example crossover data. Include an interval on sampling times and the hand-checked answer (one profile worked by hand), then a BE run with one pivotal and one supportive metric. |
| Part V | New section, **"Partial AUCs: Interpolation and the End of the Interval"**. Cover: why the interpolation method matters; why no extrapolation past the last sample; the "t" definition. |
| Part VI | New section, **"Partial AUCs in Bioequivalence"**: pivotal and supportive; intersection-union; zeros; power with the right CV; FDA product-specific guidance and the EMA half-τ default |
| Appendix A (Glossary) | Partial AUC, pivotal, supportive, t (last measurable) |
| Appendix C (Troubleshooting) | "My partial AUC is empty" (past the last sample, start after Tlast); "No verdict for my partial AUC" (supportive, or zeros) |
| Appendix D (References) | Hopefl 2025; Periyasamy 2026; EMA modified-release guideline |

**Mechanics:**
- rename the files to v1.6 (`.docx` and `www/…pdf`);
- update the manual link in `app.R`;
- update the table of contents in Word, re-export the PDF, and check that the page header shows v1.6.

### 7.7 READMEs and other files

- **`README.md`:**
  - Path 4, 5 and 6 descriptions: partial AUCs.
  - The CDISC sentence: AUCINT.
  - Do not claim "FDA-compliant partial AUCs". Say the intervals follow the user's protocol or product-specific guidance.
- **`docs/CDISC_ROADMAP.md`:** note that AUCINT was added and where.
- **Memory:** after merging, record the feature and any open items (as for the earlier reviews).

### 7.8 Order of the documentation pass

The order matters: each later document quotes numbers from an earlier one.

1. Methods page, help popovers, labels and codes (app text)
2. Data Preparation Guide
3. Run the validation, which produces the test counts
4. URS (requirement IDs)
5. IQ/OQ/PQ, generated from steps 3 and 4
6. Manual, which refers to all of the above
7. READMEs, version history, About page

Then a factual audit of all new text against the code, as before the v1.4.0 merge.

---

## 8. Out of scope

- **Partial areas under the effect curve (pAUEC)** and pharmacodynamic endpoints.
- **Model-based partial AUCs** (population PK, NONMEM), mentioned in Periyasamy 2026.
- **Sparse-sampling designs** where FDA computes partial AUCs from mean profiles with bootstrap or Bailer's method (FDA 2026 statistical guidance): a different analysis altogether.
- **Automatic or data-driven cutoff selection** (D7).
- **Tmax-based cutoffs computed by the app** (D8): the reference product's median Tmax, per-subject reference Tmax, or the earlier Tmax. A median Tmax can already be typed as a fixed number.
- **Cτ after a single dose** and concentrations at other stated times (section 4.6 #3).
- **AUC from a cutoff to infinity** (for example AUC τ/2–∞ in Boily's post-hoc analysis): this needs extrapolation, which conflicts with D1.
- **PK/PD-model-based selection of sensitive windows** (exenatide, paliperidone in Tsakiridou 2025).

---

## 9. Facts to verify during implementation

- **AUCINT:** present in CDISC SDTM CT release 2026-03-27 (codelists C85839/C85493), and the exact PPSTINT/PPENINT conventions.
- **EMA modified-release guideline:** check the current wording of the τ/2 early/terminal split, and whether widened limits are ever allowed for partial AUCs (for example Cmax-like widening in highly variable products).
- **FDA PSGs:** confirm "t" is the last measurable time point and not the last sampling time, in a sample of the PSGs listed in Hopefl 2025 (leuprolide PSG_021731, budesonide PSG_215935).
- **EMA modified-release guideline (EMA/CPMP/EWP/280/96 Rev 1), for D10 and D9:** for which metrics widening is allowed (Cmax, Cτ, partial AUCs?), and the exact wording for Cmax per phase.
- **Health Canada:** whether current guidance still requires AUCRefTmax (Tsakiridou 2025 cites a 1992 report). Only needed if D8 is taken up.
- **NonCompart `IntAUC` behaviour** at a cutoff before the first sample after IV bolus (C0 back-extrapolation), and whether Rule 1's pre-first-quantifiable zeros give the expected early partial AUC.
