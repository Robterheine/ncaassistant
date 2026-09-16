# CDISC compatibility — roadmap and handoff (Phases 1–5)

**Status:** Phase 0 complete and released as v1.3.0 (commit `58d6f5b`).
**Audience:** a fresh session, or a future maintainer, picking this up cold.
**Written:** September 2026, against app v1.3.0.

This document is the design record for making NCA Assistant interoperate with
CDISC-structured pharmacokinetic data. It exists so that the reasoning behind
the scope decisions is not lost, because several of those decisions are
deliberate refusals that look like omissions.

---

## 1. Why this work started, and what changed

The original question was "how do we make NCA Assistant CDISC (SDTM) compatible,
for input and output files, while keeping the same packages?"

Three specialists (regulatory/CDISC, clinical pharmacology, R engineering)
reviewed the app against that question. They converged on a scope that is much
narrower than "support SDTM", and in the process found four correctness defects
that had nothing to do with CDISC. Those were fixed first, in Phase 0.

**The single most important conclusion:** the mapping layer is not the feature.
Mapping `AVAL` to a concentration column is a lookup table and buys almost
nothing. The substance of CDISC support is *record selection and refusal* —
knowing which rows are in the analysis set and rejecting files the app cannot
safely analyse. If only the mapping is built, the app becomes **more** dangerous,
because users will believe they are on a standards-compliant path while the
interlocks that make that path safe are missing.

---

## 2. What Phase 0 already fixed (v1.3.0)

All four were silent: they produced plausible numbers, not errors. All are
covered by new regression tests in section `REG` of `validation/validation.R`.

| ID | Defect | Fix |
|---|---|---|
| `REG-DOSE-01/02/03` | `tblNCA` matches `dose` positionally against `unique(key)`. The dose vector was built grouped by subject while the key sorts lexicographically, so with ≥10 numeric subjects (or unpadded character IDs) plus a Treatment column, each subject got another subject's dose. Wrong CL/F, Vz/F, `*_DN`; Cmax/AUC/t½ unaffected. | Doses are passed as a vector **named by subject** and matched by name inside `run_nca()` (`R/nca_helpers.R`). Unnamed multi-subject vectors are refused. Also made per-subject dosing work in crossovers for the first time. |
| `REG-REP-01` | The shipped `reproduce_analysis.R` did not reproduce the app: the generated `dplyr::summarise()` referenced the whole frame rather than the group (every subject got the file's maximum dose), and the script lacked the app's `"<0.5"` text-BLQ rescue. | Both mirrored in `generate_nca_script()` (`R/export_record.R`). `REG-REP-01` now executes the script via `Rscript` and compares every parameter. |
| `REG-BLQ-01/02` | Positional BLQ rules (1, 5, 6) grouped by subject only, so in a crossover they ran across both periods at once; and they ran in file order, not time order. | `apply_blq_rules()` now groups by profile (subject × treatment) and visits each profile in time order. The upload module sorts before applying rules. |
| `REG-UNIT-01/02/03` | Units drive a real conversion factor for CL and V inside NonCompart. Free text allowed `µg/mL`, `mcg/mL`, `mg/kg`, which failed inside a `tryCatch` and surfaced as "NCA failed". | Units are chosen from validated lists (`CONC_UNIT_CHOICES` etc. in `R/utils.R`), checked by `validate_units()` before the run, and a molecular-weight input was added so molar assays return CL and V. |

**Consequence for later phases:** `validate_units()` and the named-dose contract
already exist and should be reused, not reinvented.

---

## 3. Scope decisions — read this before designing anything

These were argued out and should not be silently reversed. If you reverse one,
record why.

### 3.1 Build an ADNCA importer, not an SDTM merger

Nobody in industry feeds raw SDTM into an NCA tool. The real flow is:

```
SDTM PC + EX + DM (+VS)  ->  merged analysis-ready dataset  ->  NCA  ->  PP domain
                             (historically a "PKC" file;
                              now CDISC ADaM ADNCA)
```

The merge is a data-management function performed and QC'd upstream. Building it
inside a Shiny app means owning a clinical data-management pipeline alone.

**Therefore:** accept an already-merged, analysis-ready **ADNCA-shaped file**.

### 3.2 Refuse to derive elapsed time from datetimes

Require a numeric elapsed-time column. Offer the user the choice between the
nominal (`NRRLT`) and actual (`ARRLT` / `MRRLT`) basis, and record which was
used in the analysis record.

Reason: deriving elapsed time from `PCDTC` / `EXSTDTC` means handling partial
ISO 8601 datetimes, timezones and DST, ambiguous reference doses, and sign
conventions. Those failure modes are exactly where AUC is most sensitive, and
the derivation belongs upstream where it is QC'd.

Note `MRRLT` semantics: it is `ARRLT` with negative pre-dose times set to zero.
If you accept `ARRLT` directly you must apply that zeroing or refuse the file —
a pre-dose sample at t = −0.25 instead of t = 0 measurably shifts AUC.

### 3.3 Output: a draft PP-shaped table, never a PP domain

NonCompart already emits CDISC PP parameter codes (`CMAX`, `TMAX`, `AUCLST`,
`AUCIFO`, `LAMZHL`, `AUCPEO`, `VZFO`, `CLFO`, `MRTEVLST`, …). That is the easy
20%. A conformant PP record also needs `STUDYID`, `DOMAIN`, `PPSEQ`, `PPTEST`,
`PPCAT` (analyte), `PPORRES`/`PPORRESU`, `PPSTRESC`/`PPSTRESN`/`PPSTRESU`,
`PPSPEC` (matrix), timing, and a `RELREC` linkage back to PC via `PCSEQ`.

The app currently has **no study ID, no analyte, no matrix, no dates, and no
`PCSEQ`**. Without `PCSEQ` carried through the input, the PC↔PP linkage cannot
be constructed at all. That is a structural limit, not a to-do.

Two app-added parameter names are non-standard and should be corrected in any
export: `CMIN_SS` → `CMIN`, `FLUCTP` → `FLUCP`.

### 3.4 Explicit refusals

- **No `define.xml`.** It is a study-level submission deliverable authored by the
  sponsor's standards group, validated as a unit against a declared IG version
  and CT release. A fragment emitted by a third-party tool for one table is
  unusable by sponsors and creates a false impression of submission-readiness.
  Emit a plain dataset specification (variable, label, type, origin, codelist)
  instead — that genuinely helps whoever writes the real define.xml.
- **No `.xpt` output.** An `.xpt` file looks like a submission dataset and
  invites someone to drop it into a submission folder; a `.csv` does not. Keep
  the output format as a barrier against misuse and say so in the docs. Anyone
  who needs XPT has SAS.
- **No multi-analyte support.** Detect and refuse. Do not average, do not pivot.
- **No BE results in CDISC form.** `PP` holds subject-level parameters, not
  treatment comparisons. Geometric mean ratios, 90% CIs and ANOVA tables belong
  in ADaM / the Analysis Results Standard. Export them as CSV/HTML and state
  that BE output is out of CDISC scope. A fabricated PP-like structure
  containing CIs is worse than no export.

### 3.5 Keep both input paths, as two separate doors

The existing flat-file path stays. Most users (hospital pharmacists, students,
academic groups) have an Excel file from the bioanalytical lab and will never
see an SDTM domain. Removing that path would also remove validated
functionality, invalidating existing users' qualification.

But: **two doors, one core.** Both converge on a single canonical representation
before any NCA code runs, and the door used is recorded in the settings JSON.
Two independent pipelines would be unacceptable.

**Do not add ADNCA patterns to `auto_detect_columns()`** (`R/mod_data_upload.R`).
If the flat door auto-detects `AVAL`/`USUBJID`, users will push ADNCA files
through it and bypass every interlock. Add a *sniffer* that refuses instead —
see Phase 2.

---

## 4. The phases

Effort is in **focused days** for one part-time maintainer. At ~4–6 h/week,
one focused day ≈ 1.5–2 calendar weeks.

### Phase 1 — Extract the pipeline (4–5 days) — **do this next**

Independently shippable. No visible change; validated by "all existing tests
still pass, plus new ones".

**Problem it solves.** The ingestion logic is ~45 lines inside an `observeEvent`
in `R/mod_data_upload.R`. It cannot be called, tested or reused by a second
door. The proof: `validation/validation.R` cannot source the module, so it
*text-extracts* `auto_detect_columns()` with a brace-counting loop. The code
that processes every uploaded file has no direct test coverage.

It also solves the reproduce-script divergence *structurally*. Phase 0 fixed the
two known divergences by mirroring logic into the generated script, but there
are still two copies of the pipeline. With two doors, interlocks and unit
conversion, hand-transcription will fail again within a release.

**Work:**

1. Create `R/pipeline.R` — Shiny-free (no `input$`, no `shared$`, no
   `showNotification`). Move `apply_blq_rules()` and `run_nca()` there
   unchanged. Extract the observer body into `prepare_pk_dataset(raw, col_map, opts)`.
2. Define the canonical object (below). `R/mod_data_upload.R` becomes a thin caller.
3. **Ship `R/pipeline.R` inside the record zip** as `nca_pipeline.R`, and add its
   SHA-256 as a fourth entry in the integrity manifest (`write_integrity_manifest()`
   already handles three).
4. Shrink `reproduce_analysis.R` to ~60 lines: install → read → verify hash →
   `source("nca_pipeline.R")` → `prepare_pk_dataset()` → `run_nca()` → write →
   compare. The settings JSON becomes the single source of parameters instead
   of string-interpolated constants. This deletes ~250 lines of
   `R/export_record.R` (the BLQ switch, `dose_code`, `composite_key_code`,
   `split_code` all collapse into calls).
5. Same treatment for `generate_single_nca_script()` and `generate_viz_script()`.
6. **Run the generated script at export time** and write `reproduction_check.txt`
   into the zip, so the user is told before they download if it says DIFFERENT.

**Canonical object.** The governing rule: `data` and `col_map` are the only
fields any pre-existing code reads. Everything else is additive metadata for new
consumers (validators, export layer, future PP writer).

```r
pk_dataset <- list(
  data       = <data.frame, user's original headers preserved>,
  col_map    = <the existing 7 keys, unchanged semantics>,

  design     = <detect_study_design() output, unchanged>,
  provenance = list(door = "flat" | "adnca", file_name, file_path, sha256,
                    read_args, pipeline_sha256),
  analyte    = list(name, paramcd, pctestcd, matrix),
  units      = list(conc, time, dose, mw, source = "user" | "AVALU"),
  time_basis = list(col, kind = "actual" | "nominal",
                    cdisc_var = "ARRLT" | "NRRLT" | NA, user_confirmed),
  blq        = list(lloq, rule, source = "app_rule" | "upstream_dtype" | "none",
                    text_tokens_converted, na_policy),
  flags      = list(anl01fl_applied, dtype_present, n_rows_dropped),
  interlocks = <data.frame, same shape as data_quality findings>,
  qc         = <run_data_quality_check() output, unchanged>
)
```

**Design test:** if a CDISC feature forces an edit to `mod_path_multi_nca.R`,
`mod_path_be.R`, `mod_path_single_nca.R` or `mod_path_viz.R`, the object is
wrong. Go back and fix the object.

**Do not** introduce a rename/normalisation step. `col_map` is consumed at ~200
sites across 9 files; rewriting them all is ~200 chances to reintroduce exactly
the silent-wrong-number class of bug that Phase 0 just removed. The app also
deliberately shows users *their own* column names, which is a real pedagogical
virtue. `col_map` *is* the normalisation, applied lazily.

**Acceptance:** all 191 existing tests pass; `prepare_pk_dataset()` is directly
callable from `validation.R` without brace-counting; the record zip contains a
hashed `nca_pipeline.R`; `reproduction_check.txt` is present and says MATCH.

---

### Phase 2 — Interlocks as a library (3–4 days)

Independently shippable, and several interlocks improve the **flat** door too,
so this has value even if Phase 3 never happens.

Create `R/interlocks.R`. Each interlock is a pure predicate returning the
`data_quality.R` finding shape (`Severity` / `Category` / `Message` / `Detail` /
`Action`) so `render_quality_report()` renders them unchanged.

**Ship these on both doors:**

- More than one distinct concentration unit, or time unit.
- Minimum time per profile is not near zero. *(This single check kills the
  `AFRLT`-instead-of-`ARRLT` trap, where period 2 starts at ~168 h, the profile
  looks monotonic and normal, and every period-2 parameter is wrong.)*
- Time column containing datetimes, ISO 8601 durations (`PT1H30M`), partial
  dates, or values implausible for the stated unit. *(An Excel datetime read as
  POSIXct becomes epoch seconds; the profile passes QC, half-life is nonsense,
  and because both T and R scale by 3600 the **BE ratio still looks right**.)*
- Stacked-file detection (duplicate times within subject × treatment).
- **The ADNCA sniffer:** if the header shows `USUBJID`/`PARAMCD`/`AVAL`/`ANL01FL`
  signatures, the flat door refuses and points at the CDISC door.

**Acceptance:** each interlock has a positive and a negative test; the ADNCA
sniffer fires on the Phase 3 fixtures.

---

### Phase 3 — The ADNCA door (6–8 days). Requires Phase 2.

**Hard rule: this ships complete or not at all.** A CDISC door that exists, is
discoverable, and has three of seven interlocks is more dangerous than none,
because the app's whole value proposition is that it refuses to produce a wrong
number quietly. Do not ship it labelled "beta" — users do not read beta labels.

Create `R/ingest_adnca.R`. CSV/XLSX only (XPT is Phase 5, and is discouraged).
Ends by calling `prepare_pk_dataset()`.

**Column recognition** (auto-fill the mapping; this is most of the day-one value):

| Need | ADNCA | Fallback |
|---|---|---|
| Subject | `USUBJID` | — |
| Concentration | `AVAL` | `PCSTRESN` |
| Analyte | `PARAMCD` / `PARAM` | `PCTESTCD` |
| Time (actual) | `ARRLT` or `MRRLT` | — |
| Time (nominal) | `NRRLT` | `PCTPTNUM` |
| Treatment | `TRTP` / `TRT01P` | `EXTRT` |
| Period | `APERIOD` | — |
| Sequence | `TRTSEQP` / `ACTARM` | — |
| Dose | `DOSEA` | scalar |
| Units | `AVALU` / `PCSTRESU` | — |
| Analysis flag | `ANL01FL` | — |
| Derived-record flag | `DTYPE` | — |
| BLQ | `ABLFL` + LLOQ | `PCORRES` text |

**Blocking refusals** — each with a message naming the offending variable and value:

1. More than one `PARAMCD` / `PCTESTCD`. Message must **not** suggest averaging.
2. More than one `PCSPEC` (plasma + urine).
3. More than one distinct unit, or more than one distinct `PCLLOQ`.
4. Any record with `DTYPE` populated (`HALFLLOQ`, `COPY`), **and** the app's own
   BLQ rules must be **disabled in the UI** when they are present.
5. `ANL01FL != "Y"` when `ANL01FL` exists → drop, with a visible count.
6. `PCSTAT = "NOT DONE"` → drop, with a visible count.
7. More than one EX record per subject per period, or any infusion
   (`EXENDTC != EXSTDTC`).
8. More than two treatment levels for the BE module.
9. Datetime / ISO-duration / partial-date time columns.
10. Negative times when using `ARRLT` without consenting to `MRRLT` zeroing.
11. More than one distinct dose per subject × period.

**The #1 predicted defect is double imputation:** an ADNCA arrives with BLQ
already handled (`DTYPE = "HALFLLOQ"`) and the app applies its own LLOQ/2 rule on
top. Interlock 4 exists specifically to prevent this. Classify it CRITICAL.

**Also unresolved and needed here:** BLQ detection is threshold-based
(`!is.na(conc) & conc < lloq`). SDTM encodes BLQ as `PCSTRESN`/`AVAL` = `NA`
with the text in `PCORRES`, so the rules would never fire. Do **not** silently
treat all `NA` as BLQ — `NA` also means "sample not taken", and conflating the
two is its own wrong-number bug. This must become a recorded user choice
(`blq$na_policy` in the canonical object).

**Acceptance:** every refusal has a fixture that triggers it and a fixture that
does not; `EQV-01` passes (below).

---

### Phase 4 — Draft PP export (5–7 days)

The engineering view is that this is the **first thing to cut**: least leverage,
most ongoing maintenance (CT releases are dated and move, so you are signing up
to re-pin annually), and it is where being 90% right is worse than being absent.
The clinical view is that it is genuinely useful to a group without SAS. Decide
after Phase 3, with real users.

If built:

- Emit CSV/XLSX, never `.xpt`.
- **Pin to a dated CT release.** Put the CT release identifier and date in one
  data file and print it in the export, the settings JSON and the HTML summary.
  A user in 2029 then sees "CT release 2026-03-27" and knows to check, instead
  of trusting a claim that quietly rotted. **The pinning is the whole mitigation.**
- Label "draft — not for submission" in the file itself and in the filename.
- Requires: study ID, analyte name and matrix carried through the data model
  (introduce these in Phase 3 even for single-analyte studies).
- Units: derive `PPSTRESU` from a unit-algebra table — `CMAX` takes the
  concentration unit, `AUCLST` conc·time, `CLFO` dose/(conc·time), `VZFO`
  dose/conc, `LAMZ` 1/time. **Do not re-implement the algebra**: `NonCompart::Unit(code = "CMAX", ...)`
  already returns it, and `tblNCA` returns `attr(result, "units")` which the app
  currently discards and re-derives approximately in `add_units_to_labels()`.
- `PPORRES` == `PPSTRESN` for this app (it computes in the user's units); say so
  rather than faking a conversion.
- Replace the circular `cdisc_pk_names()` stub (`R/utils.R`) — its `NonCompart`
  and `CDISC` columns are character-for-character identical, and its test
  `EXP-CD-01` asserts only `nrow() > 10`, so it **cannot fail for any reason
  connected to CDISC**. Rewrite the acceptance criterion against a dated CT
  release.

---

### Phase 5 — XPT input (2–3 days). Recommended against.

`haven::read_xpt` makes this easy, which is the problem: it is the step that
invites "so it reads submission data" claims. Build it only after Phase 3 is
validated and only if real users ask. Read-only. Never write XPT.

---

## 5. Testing and fixtures

`validation/validation.R` is a flat ~1100-line script with a `check()`
accumulator. Phase 1's extraction is also the testability fix.

**Expected growth:** `DAT-PREP` (~15), `ING-CD` (~25, each interlock plus its
negative case), `EQV` (~8), `PP` (~12), `UNIT` (~10) → roughly **+70 checks**.
Split into `validation/tests/*.R` with `validation.R` as the runner; the existing
`check()` accumulator supports this with no redesign.

**Risk classification:** most of these are **CRITICAL**, not SUPPORTIVE. A
record-selection or unit-conversion defect produces a plausible-looking wrong
number with no error — the same severity as NCA accuracy. CDISC I/O is a new
critical computational path, not a file-format convenience.

**Fixtures: generate, do not collect.** ADNCA is a shape, not a secret. Write
`validation/fixtures/make_fixtures.R` — committed, deterministic (`set.seed`) —
and **also commit the generated CSVs**, because the reproduce-script tests need
stable SHA-256s.

Put them in `validation/fixtures/`, **not** `data/`. `data/` is user-facing
example data surfaced in the Data Preparation Guide; a user finding an ADNCA
fixture in the examples dropdown will try to analyse it.

| ID | Fixture | Must |
|---|---|---|
| F1 | `adnca_clean.csv` — 12 subj × 2 trt × 2 per, one PARAMCD, ANL01FL all Y, DTYPE empty | happy path |
| F2 | `flat_equivalent.csv` — **generated from F1** | equivalence pair |
| F3 | `adnca_dtype.csv` — HALFLLOQ rows | refuse + disable app BLQ rules |
| F4 | `adnca_anl01fl.csv` — extra `ANL01FL=""` rows with absurd values | must be dropped; failure visibly changes Cmax |
| F5 | `adnca_multi_analyte.csv` — parent + metabolite stacked | refuse |
| F6 | `adnca_afrlt.csv` — AFRLT substituted in period 2 (min time ~168 h) | **highest-value fixture** |
| F7 | `adnca_datetime.csv` — PCDTC/EXSTDTC only | refuse |
| F8 | `adnca_units_mixed.csv` — ng/mL and ug/L mixed | refuse even though numerically equivalent |
| F9 | `adnca_multi_ex.csv` | refuse |
| F10 | `blq_text_rule4.csv` | guards the Phase 0 script divergence |
| F11 | `dose_multi.csv` — 3 dose levels, 11 **unpadded numeric** IDs, treatment column | guards the Phase 0 dose bug; **must include subjects 10 and 11** or it vanishes |

**The equivalence test** (`EQV-01`) is the entire traceability argument for
having two doors, made empirically. Same study, both doors, identical numbers to
full precision. Three traps, all of which will bite:

- **Row order.** The doors produce different orders (`USUBJID` strings vs numeric
  IDs sort differently — the same lexicographic issue as the Phase 0 dose bug).
  Sort both, and assert key-set equality *separately*, or a mismatch presents as
  a numeric difference and you chase the wrong thing.
- **Subject ID type.** `STUDY-001-010` vs `10`. Generate F2 **from** F1 so
  identity holds by construction; never maintain them as two hand-written files.
- **Do not compare `pk_dataset` objects** — they differ by design. Compare the
  canonical triple as `EQV-02` and the NCA output as `EQV-01`, so a failure tells
  you whether ingestion or analysis diverged.

---

## 6. Claims and documentation — separate, outstanding work

These are independent of the phases and should be done regardless.

### Outstanding overclaims found during this review

| Where | Problem |
|---|---|
| `README.md`, `validation/README.md` | Both claim the URS contains an **"ALCOA+ data integrity framework"**. The string "ALCOA" does not appear anywhere in `NCA_Assistant_URS.docx` (sections are Purpose, System Description, Supplier Assessment, FMEA, Requirements, Traceability, Configuration Management, Regulatory References, Glossary). Either add a real section — honestly marking *Attributable* as **not met**, since `analyst` is free text with no authentication — or delete the claim. |
| `app.R` (About page, package list) | Describes `ncar` as "CDISC SDTM compatible". That is an unsourced claim about a third-party package. Quote its own wording or drop it. Separately, `library(ncar)` is loaded but **never used anywhere in `R/`** — drop it or use it. |
| `README.md` (Complete Analysis Record section) | "a self-contained zip file **for regulatory submissions**" reads as submission material. Prefer "for archiving, audit trails, publication supplements, and inclusion in a sponsor's study documentation". |
| `README.md` (feature list) | Claims column auto-detection "including CDISC … naming conventions". Only `USUBJID` matches; `AVAL` and `PCSTRESN` fall through to the positional fallback. Fix the claim or (after Phase 2) point at the sniffer. |
| `URS-GEN-06` + `EXP-CD-01` | Circular: the requirement says parameter names "follow CDISC conventions", the acceptance criterion is that `cdisc_pk_names()` maps them, and that function's two columns are identical. The test asserts only `nrow() > 10` and **cannot fail**. Rewrite against a dated CT release. |

### Safe vs dangerous wording

**Never use:** "CDISC compliant", "SDTM compliant", "CDISC certified/validated",
"generates submission-ready datasets", "21 CFR Part 11 compliant", "GAMP 5
validated", "FDA/EMA accepted". "Compliant" is a term of art meaning conformance
verified by a rules engine (Pinnacle 21 / CDISC CORE) against a declared IG
version and CT release. CDISC, SDTM and ADaM are registered trademarks; there is
a certification programme this project has not been through.

**Defensible shape** — describe the structure you read, the codes you write, the
version you did it against, and what you did not verify:

> Accepts analysis-ready PK datasets structured according to the CDISC ADaM
> Implementation Guide for Non-Compartmental Analysis Input Data (ADNCA) v1.0.
> Record selection flags (`ANL01FL`, `DTYPE`, `ABLFL`) are read and applied;
> users confirm the resulting analysis set before running NCA.

> Exports NCA parameters as a PP-shaped parameter table using `PPTESTCD` codes
> drawn from CDISC controlled terminology release YYYY-MM-DD. This is a draft
> mapping intended as input to a sponsor's SDTM PP derivation. It is not a
> conformant SDTM PP dataset, has not been checked against CDISC conformance
> rules, and must not be submitted without re-derivation by the sponsor.

> NCA Assistant is not affiliated with, endorsed by, or certified by CDISC.
> CDISC, SDTM and ADaM are trademarks of the Clinical Data Interchange Standards
> Consortium.

### One sentence worth adding to the reproducibility documentation

The reproduce-script comparison demonstrates **reproducibility, not independent
verification** — it re-executes the same algorithms from the same packages. A QA
reviewer will make that distinction immediately, and blurring it costs
credibility across the whole validation package.

> The reproduction script re-executes the recorded analysis using the same
> packages; it demonstrates reproducibility, not independent verification, and
> does not substitute for re-derivation in a qualified system.

### Suggested process fix

Add a **claims register** to the repo: one row per regulatory or standards claim
made anywhere (UI, README, manuscript, validation documents), with the exact
claim text, its location, the supporting source (document, version, date,
section), and the date last verified. Any claim with an empty source column gets
deleted, not softened. This would have caught every item in the table above.

---

## 7. Strategic context — decide this before Phase 3

[`pharmaverse/aNCA`](https://github.com/pharmaverse/aNCA) is an existing Shiny
NCA app built on `PKNCA`, backed by Roche, Appsilon and Human Predictions, which
already ingests ADNCA and emits PP and ADPP. It will win the sponsor-scale ADNCA
use case. Competing there from a solo academic position means chasing a
quarterly-moving controlled-terminology target indefinitely.

NCA Assistant's genuine differentiators are elsewhere: the **bioequivalence half**
(PowerTOST planning + ANOVA + CI alongside the NCA in one tool), the
**teaching-oriented QC layer** that explains *why* a file is wrong, and the
**Complete Analysis Record**. None of those are CDISC.

**The cheaper alternative to Phase 3**, worth trying first: ship a small,
well-tested standalone `adnca_to_flat.R` helper plus a one-page recipe (**1–2
days**). It applies the same refusals — filter `ANL01FL`, reject `DTYPE`, pick
`ARRLT`/`NRRLT` — and emits the flat 7-column CSV. Same interlocks, ~10% of the
effort, no new door, no new validation surface, and the user's choices land in a
file they can attach to their own documentation.

**If it gets used, that is the evidence the in-app door is worth building. If it
does not, a year has been saved.**

Say plainly in the README what this app is for and what aNCA is for. Pointing
users to the better tool for CDISC work costs nothing, buys credibility, and
inoculates against the "why not just use aNCA?" review comment.

---

## 8. Facts to verify before any of this reaches a manuscript

Do not write these from memory.

| Claim | Confidence | Action |
|---|---|---|
| ADNCA v1.0 exact title, version, publication date | high that it exists | verify citation on cdisc.org |
| CDISC trademark status and certification programme names | high / medium | verify on cdisc.org |
| FDA Study Data Technical Conformance Guide and Data Standards Catalog versions | high on substance | verify current version and date |
| EMA does not mandate CDISC for MAA submissions | medium-high | **verify** — it materially affects the value argument for a European user base |
| Every NonCompart parameter name is a current `PPTESTCD` | **unverified** | check each against a dated CT release. Do not write "verbatim"; some outputs (`CORRXY`, `R2ADJ`, `b0`) are regression diagnostics that may have no PP equivalent |
| `SWING` and `TAU` exist as `PPTESTCD` values | unverified | check NCI EVS codelist C85839 |
| SDTMIG 3.4 PC↔PP linkage convention (`PPGRPID`/`PCGRPID`, RELREC) | unverified | most likely to have moved |
| Regulatory expectation that **actual** sampling times are used for BE, with deviations tabulated | high on practice | verify exact wording in the current EMA BE guideline and FDA BE-with-PK-endpoints guidance |
| Maturity/adoption of the CDISC Analysis Results Standard | moderate | verify before promising a home for BE output |
| GAMP 5 2nd ed. supplier-vs-user validation responsibility; Part 11 / Annex 11 gaps | high | safe to state |

---

## 9. Suggested order

1. **Phase 1** — extract the pipeline. Highest leverage; makes everything else
   testable and turns "reproducible" into a verified property.
2. **Fix the documentation overclaims** (§6). Cheap, and one of them is live in
   a manuscript under review.
3. **Phase 2** — interlocks. Improves the existing flat door on its own.
4. **The standalone `adnca_to_flat.R` converter** (§7). Two days, and it
   generates the evidence for the next decision.
5. **Then decide** on Phase 3 based on whether anyone used the converter.
