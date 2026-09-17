# NCA Assistant — roadmap and handoff

**Audience:** a fresh session, or a future maintainer, picking this up cold.
**Written:** September 2026, against app v1.3.0. Last updated 2026-09-17
(Part B verified against the code; see §10.5).

This document is the design record for two separate workstreams, both arising
from peer review of the manuscript. It exists so that the reasoning behind the
scope decisions is not lost, because several of those decisions are deliberate
refusals that look like omissions.

| Part | Workstream | Status |
|---|---|---|
| **A** (§1–9) | CDISC / SDTM interoperability | Phase 0 shipped in v1.3.0 (`58d6f5b`). **Phases 1, 2 and 3 shipped** (see §4.1, §4.2, §4.3). Phases 4–5 pending (decisions). |
| **B** (§10–15) | Bioequivalence design coverage — replicate designs | Reviewed and decided. **This is the next version.** Verified against the code on 2026-09-17 (§10.5). **Tier 0 and B1–B6 shipped** (Tier 0: `a85936b`, `7faf9bf`, `5a73794`; Part B: `0ad844b`, `138ea3d`, `e277f37`, `37a2a12`, `c47dbfc`, `193da3d`). README and user manual update pending. |

The two interact: Part B's implementation is cheaper and cleaner if Part A's
Phase 1 (extract `R/pipeline.R`) is done first. See §14.

---

# PART A — CDISC interoperability

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

### Phase 1 — Extract the pipeline (4–5 days) — first within Part A, after Part B (see §16)

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

### 4.1 Phase 1 implementation record (2026-09-17)

| Step | Commit | Delivered |
|---|---|---|
| 1 | `a840a59` | `R/pipeline.R` (Shiny-free): `read_pk_file()`, `auto_detect_columns()`, `blq_text_summary()`, `prepare_pk_dataset()` returning the canonical `pk_dataset`, `dose_by_subject()`, profile key and lookups, `apply_blq_rules()`, `run_nca()`, `detect_study_design()`. Moved code is verbatim; `prepare_pk_dataset()` output is `identical()` to the old observer on 10 golden cases. Upload module is a thin caller and records read arguments. `validation.R` sources the pipeline; brace counting removed. |
| 2 | `e043870` | Half-life overrides go through NonCompart (`UsePoints`) in all three paths via `run_nca(..., lz_overrides)` / `run_single_nca(..., time_used)`. |
| 3 | `e5d05a5` | Records ship `nca_pipeline.R` (hashed; fourth manifest entry), a generic reproduction script (41 lines) driven by the settings JSON, and `reproduction_check.txt` produced by running that script at export. Same for single-subject and figure records. |

Validation: 246 → 261 automated checks (DAT-PREP-01..09, NCA-OV-01..06,
REG-DOSE-04, REC-01..08). All acceptance criteria above are met.

**Defects found and fixed during Phase 1** (all silent unless noted):

| Defect | Effect |
|---|---|
| Override set CL/F and Vz/F to dose/AUCinf without NonCompart's unit factor | Off by 1000 for mg with ng/mL after any half-life adjustment |
| Override did not update IV CL/V (`CLO`, `VZO`), predicted-Clast parameters, or AUCPEO/AUMC/MRT (batch, BE) | Stale values inconsistent with the displayed half-life |
| Re-running an analysis dropped overrides while the record still listed them | Record claimed adjustments absent from its results |
| Dose normalisation divided by the per-subject dose vector by position | 22 of 24 `*_DN` values wrong in a 12-subject crossover |
| Reproduction script omitted MW; comparison skipped values missing on one side | Molar-unit records reported MATCH although CL/F and Vz/F were not reproduced |
| Reproduction script ignored the upload's separator/decimal mark | Script failed for such files |
| Figure script plotted raw data | BLQ handling used by the app not reproduced |
| Batch parameter table selected default columns after appending units (loud: visible) | Cmax, AUC, half-life, CL/F, Vz/F missing from the default view; "Half-Life (h) (h)" |
| Single-subject record in manual-entry mode labelled as uploaded file | Reproduction would read the wrong data |
| `PIPELINE_SHA256` defined in `app.R`'s environment (caught in the app, loud) | Upload failed with "object not found" |

**Carried to Phase 2 (not fixed):** a concentration column that uses a decimal
comma *and* contains BLQ text is read as text, so values such as `"3,5"`
become missing. The quality check reports it as unrecognised text (loud), but
the recommended fix — converting decimal commas in character concentration
columns inside `prepare_pk_dataset()` — changes results for existing files and
belongs with the interlocks.

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

### 4.2 Phase 2 implementation record (2026-09-17)

`R/interlocks.R` holds five interlocks, each a pure function returning
quality-report findings; `run_data_quality_check()` runs them all, so both the
flat door and any future door get them. Fixtures F1–F9 are generated by
`validation/fixtures/make_adnca_fixtures.R` (F10/F11 are covered by the existing
REG-BLQ/REG-DOSE checks).

| Interlock | Severity | Catches |
|---|---|---|
| `interlock_adnca_shape` | ERROR | USUBJID + AVAL/PCSTRESN + any ADNCA/PC marker (PARAMCD, ANL01FL, DTYPE, ARRLT, NRRLT, AFRLT, PCDTC, AVALU, ...). Fires on F1, F3–F9; not on F2 or a file with USUBJID alone. Before Phase 2, F1 passed the quality check and would have been analysed with its flags ignored. |
| `interlock_mixed_units` | ERROR | A unit column with more than one value, equivalent units included (F8). |
| `interlock_time_format` | ERROR | Time stored as date/date-time objects (Excel), ISO 8601 date-times or durations, clock times, dates. |
| `interlock_profile_start` | ERROR (WARNING if < 3 samples) | A profile's first time > 0 and > 20% of its span: time since first dose (F6: period 2 from ~168 h), epoch seconds. |
| `interlock_stacked_profiles` | ERROR | Duplicate times within subject × treatment × period (moved from `data_quality.R`). |

The Phase 1 carry-over is resolved: with the decimal mark set to comma,
`normalise_decimal_comma()` reads decimal-comma numbers stored as text (a
concentration column that also contains `"<0,5"`), in the quality check, the
pipeline and the reproduction. Files that previously passed are unaffected:
such files failed the quality check before, and nothing changes when the
decimal mark is a point.

Tests: IL-SNIFF-01/02, IL-UNIT-01, IL-TIME-01, IL-T0-01/02, IL-STACK-01,
IL-DEC-01/02, IL-LIB-01; REC-03 now uses decimal commas throughout. No
interlock fires on the example data or any flat fixture. Validation 271/271.

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

### 4.3 Phase 3 implementation record (2026-09-17)

**Decision (with the maintainer):** build the ADNCA door so that people who work
with ADNCA datasets can use the app, and keep the standalone converter. The
door reuses the converter as its engine, so the §7 "converter first" route and
the door are one implementation:

- `R/adnca_import.R` (base R, Shiny-free, standalone) holds `adnca_read()`,
  `adnca_inspect()`, `adnca_convert()` (all refusals from the Phase 3 list) and
  the conversion log. `converters/adnca_to_flat.R` is now a thin wrapper that
  sources it.
- Upload page: a toggle **What kind of file?** — *Simple table* / *CDISC ADNCA
  dataset* — with a beginner tooltip (what each option is, how to recognise an
  ADNCA dataset, the four steps). In ADNCA mode the app shows a summary
  (records, analytes, matrices, time variables, units, LLOQ, ANL01FL and DTYPE
  counts), asks for the time variable (tooltip), the analyte/matrix when there
  are several (tooltip) and pre-dose zeroing for ARRLT, then converts or shows
  the refusal reason. The converted table enters the same mapping, quality
  check, interlocks and analyses as any flat upload; mapping and LLOQ are
  filled in.
- Analysis Records from an ADNCA import ship the original ADNCA file,
  `adnca_import.R` (hashed, in the manifest), the choices (`door`, `adnca` in
  the settings JSON) and `adnca_conversion_log.txt`; the reproduction scripts
  re-run the conversion before the analysis.
- The flat upload's ADNCA refusal now points to the toggle.

The "ships complete" rule is met because every refusal was already
implemented and mutation-tested in the converter. Out of scope, as decided in
§3: XPT input, SDTM merging, PP output, conformance claims.

Tests: ADNCA-01 (inspection), ADNCA-02 (converter and app share one
implementation; identical output), ADNCA-03..05 (batch, BE with analyte
selection + actual time + override, single-subject and figure records from an
ADNCA import reproduce). Validation 293/293. Verified in the app: summary,
refusals (two analytes; negative ARRLT; DTYPE), conversion with analyte and
pre-dose choice, automatic mapping and LLOQ, BE analysis, and an Analysis
Record whose reproduction check says MATCH.

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
| `README.md:91` | Claims the URS contains an **"ALCOA+ data integrity framework"**. (`validation/README.md` does not repeat it — verified 2026-09-17.) The string "ALCOA" does not appear anywhere in `NCA_Assistant_URS.docx` (sections are Purpose, System Description, Supplier Assessment, FMEA, Requirements, Traceability, Configuration Management, Regulatory References, Glossary). Either add a real section — honestly marking *Attributable* as **not met**, since `analyst` is free text with no authentication — or delete the claim. |
| `app.R:501` (About page, package list) | Describes `ncar` as "NCA report generation. Produces formatted PDF and RTF reports from NonCompart output. CDISC SDTM compatible." The last sentence is an unsourced claim about a third-party package — quote its own wording or drop it — and the first two describe a role `ncar` does not play in this app. Separately, `library(ncar)` is loaded but **never used anywhere in `R/`** — drop it or use it. |
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

> **Built (2026-09-17).** `converters/adnca_to_flat.R` (base R; readxl for
> Excel) and the recipe `converters/ADNCA_TO_FLAT.md`. It applies the Phase 3
> refusal list: explicit time variable (NRRLT/ARRLT/MRRLT; AFRLT refused;
> negative ARRLT only with `zero_predose = TRUE`), ANL01FL selection and
> PCSTAT "NOT DONE" drops with counts, DTYPE refused, one analyte/matrix
> (refused or selected explicitly, never averaged), one unit and LLOQ,
> profiles starting near zero, no duplicate times, one dose per subject and
> period, no infusions, and missing AVAL only when a BLQ result explains it.
> It writes the flat CSV and a conversion log with SHA-256 hashes. The app's
> ADNCA refusal message points to it. Tests: EQV-01/02 (converted F1 and flat
> F2 give identical analysis data and identical NCA results) and CONV-01..12,
> each guarded refusal confirmed by mutation (disabling it fails its test).
> **The Phase 3 decision is now open:** it depends on whether the converter
> is used.

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

## 9. Suggested order within Part A

1. **Phase 1** — extract the pipeline. Highest leverage; makes everything else
   testable and turns "reproducible" into a verified property.
2. **Fix the documentation overclaims** (§6). Cheap, and one of them is live in
   a manuscript under review.
3. **Phase 2** — interlocks. Improves the existing flat door on its own.
4. **The standalone `adnca_to_flat.R` converter** (§7). Two days, and it
   generates the evidence for the next decision.
5. **Then decide** on Phase 3 based on whether anyone used the converter.

This order is now subordinate to Part B, which is the next version. See the
combined order at the end of §15.

---

# PART B — Bioequivalence design coverage (replicate designs)

**Decision: this is the next version.** Reviewed September 2026 by a senior
biostatistician, a senior clinical pharmacologist and an R/Shiny engineer.

> **Status (2026-09-17): built.** Tier 0 and B1–B6 are implemented; see
> §12.1 for what was done and the decisions taken while implementing.
> Outstanding: the README and the PDF user manual (deliberately left to one
> pass at the end), the URS (new requirement URS-BE-09), and the §6
> documentation overclaims.

---

## 10. Why this work started, and what the review actually found

A peer reviewer observed that the analysis module does not cover every design the
planning module can plan for:

| | Designs offered |
|---|---|
| **Plan a Study** (`R/mod_path_power.R` ~216) | ABE: `2x2`, `2x2x3`, `2x3x3`, `2x2x4`, `parallel`; ABEL/RSABE: `2x2x3`, `2x3x3`, `2x2x4`; NTID: `2x2x4` |
| **BE Testing** (`R/mod_path_be.R` ~86) | `crossover_2x2`, `crossover_fixed_order`, `crossover_3period`, `parallel`, `replicate_2x2x4` |

**The reviewer's premise about the statistical model is wrong, and the real
problem is worse than the one they identified.**

### 10.1 The model is already correct

The statistician verified that the app's fixed-effects model reproduces
`replicateBE::method.A` — the EMA reference implementation — **exactly** (point
estimate, confidence interval and degrees of freedom to three decimal places) on
14 of the EMA reference datasets, including unbalanced ones with real dropouts.

Residual degrees of freedom match PowerTOST's formula for every planner design,
in both the `lme` and `lm` variants, balanced and unbalanced:

| Design | Sequences used | PowerTOST df | App df |
|---|---|---|---|
| `parallel` | 2 groups | n−2 = 22 | 22 |
| `2x2` | TR \| RT | n−2 = 22 | 22 |
| `2x2x3` | TRT \| RTR (and TRR \| RTT) | 2n−3 = 45 | 45 |
| `2x3x3` | TRR \| RTR \| RRT | 2n−3 = 45 | 45 |
| `2x2x4` | TRTR \| RTRT (and TRRT \| RTTR) | 3n−4 = 68 | 68 |

So there is **no model gap to close**, and no degrees-of-freedom problem. The
generic "3-period crossover" option is statistically correct for both `2x2x3`
and `2x3x3` in the EMA Method A sense. This is worth stating in the manuscript:
the app's BE analysis *is* EMA Method A, demonstrably.

> **Correction (2026-09-17): true of the model formula, not of the shipped code.**
> The agreement above holds when Period enters the model as a factor. The app
> never coerces Period, and an uploaded Period column is almost always integer,
> so with more than two periods it is fitted as a 1-df linear trend. See **D7**
> in §10.5. The manuscript claim must wait until D7 is fixed and the agreement
> is re-demonstrated through the app's own code path.

### 10.2 But a shipping design is silently broken

Two independent defects, both reproduced directly against the app's own code.

**D1 — Replicate profiles are merged before NCA.**
`run_nca()` (`R/nca_helpers.R` ~307) keys profiles as `paste(subject, treatment, sep="||")`.
Period is not in the key. In any replicate design a subject receives the same
treatment more than once, so **both administrations collapse into one key** and
are analysed as a single interleaved profile.

Measured on a 2×2×4 (TRTR \| RTRT), 6 subjects × 4 periods:

```
expected NCA profiles: 24        returned: 12   (2 per subject)
```

One subject, true per-period values vs what the app reports:

```
truth  period 1 (T): CMAX 63.748   AUCLST 295.564
       period 2 (R): CMAX 73.975   AUCLST 313.341
       period 3 (T): CMAX 71.863   AUCLST 311.744
       period 4 (R): CMAX 65.703   AUCLST 296.164

app    T: CMAX 71.863  AUCLST 304.300     <- max across both T administrations
       R: CMAX 73.975  AUCLST 303.786     <- max across both R administrations
```

Cmax becomes the **maximum across both administrations** (systematic upward
bias); AUC is a trapezoid computed over an interleaved zig-zag.

The failure is data-dependent and mostly silent:

| Time column | Behaviour |
|---|---|
| Nominal times repeated each period | `data_quality.R:421` raises a duplicate-time ERROR and blocks the upload — but its remediation text tells the user to map Treatment and Period, which they already did. Dead end. |
| Actual/elapsed times (no exact ties) | **24 → 12 profiles, silently.** Warning count is dataset-dependent; on one run there were none at all. |
| An internal tie in the interleaved profile | `run_nca()` returns NULL, "the condition has length > 1" → generic "NCA failed." **This is the common case**: a pre-dose sample recorded at exactly t = 0 in every period is already a tie. The silent 24 → 12 path needs untied pre-dose times (e.g. actual times like −0.05 h). |

Where warnings do appear they are cryptic R internals ("numerical expression has
2 elements: only the first used"), which `mod_path_be.R` catches and renders as a
bland `"Note:"`. **Do not rely on warnings as the detection mechanism.**

In a partial replicate (`2x3x3`, only R replicated) the bias is one-sided: the
clinical pharmacologist simulated a true 100 % ratio reading out at **84.4 % Cmax
at CV 45 %** — precisely the CV range where a partial replicate would be run.

This same keying is *why* `mod_path_be.R:507` hard-refuses more than two treatment
levels. That restriction is forced by the architecture, not a scope decision.

**D2 — The design merge then duplicates every row.**
`mod_path_be.R` ~476 builds `design_df` with
`select(Subject, Treatment, Period, Sequence) %>% distinct()`, which in a
replicate yields **two rows per (Subject, Treatment)**, then merges on
`Subject` + `Treatment` only. Each NCA value is entered twice.

```
6 subjects, 2x2x4:   NCA 12 rows  ->  design_df 24 rows  ->  be_data 24 rows
                     each NCA value entered 2x
                     CI table reports N_Test = 12 for a 6-subject study
```

Measured harm on a clean 12-subject dataset (duplication alone, no NCA collapse):

```
honest (12 subjects)   rows 24 | PE 94.51% | 90% CI 86.73 - 102.99 | width 16.27 | df 10
as the app fits it     rows 48 | PE 94.51% | 90% CI 90.36 -  98.85 | width  8.49 | df 32

app CI width is 52% of honest -> too narrow, i.e. biased toward declaring BE
```

Point estimate unchanged; confidence interval systematically too narrow and
degrees of freedom inflated. **This is the false-PASS direction.**

**D1 and D2 must be fixed together.** Verified: making the key period-aware while
leaving the merge on Subject + Treatment takes 24 NCA rows to **48** `be_data`
rows and makes the interval worse still. Nobody should touch `R/nca_helpers.R`
before the replicate fixture test exists and fails for the right reason.

### 10.3 Four smaller defects found in passing

| ID | Defect | Location |
|---|---|---|
| **D3** | **No point-estimate constraint.** Acceptance is `be_pass <- ci_lo >= be_lower & ci_hi <= be_upper`, and the limits are free numeric inputs. A user who plans ABEL, reads off 69.84/143.19 and types them in gets "Bioequivalent: YES" with a point estimate of 135 % — which is not an ABEL verdict, since ABEL also requires the PE inside 80–125. | `mod_path_be.R:715` |
| **D4** | **`random = ~1\|Sequence/Subject` is the wrong nesting.** Subject IDs are already unique, so this puts a random intercept on Sequence while Sequence is also a fixed effect. Produces `DF = 0` / `F = NaN` for the Sequence row in the ANOVA table and the `pt(...): NaNs produced` warnings on every mixed-model run. Treatment inference is unaffected (verified: identical estimate, SE and DF). Fix: drop `Sequence/`. **Does not fix the fixed-effects ANOVA**, where `drop1()` also reports Sequence with `Df 0` because Sequence is aliased with Subject; Method A tests Sequence against Subject(Sequence), which `drop1()` cannot do. | `mod_path_be.R:631`, `:658` |
| **D5** | **The "no ABEL/RSABE" warning is on the wrong design.** It sits inside `conditionalPanel(be_design == 'replicate_2x2x4')`, but the planner offers scaled methods on `2x2x3` and `2x3x3` too — which map to `crossover_3period`, which shows nothing. Two of the three designs that need the warning do not get it. | `mod_path_be.R:110` |
| **D6** | **`crossover_fixed_order` is numerically identical to a paired t-test** (verified: same estimate, SE, df = n−1) — i.e. PowerTOST's `paired`. Period and treatment are fully confounded, so it cannot support a BE conclusion, yet the results pane still prints a BE verdict against 80–125 %. The warning and the output contradict each other, and the output wins. | `mod_path_be.R:618` |

### 10.4 Three more places with the same period-blind grain assumption

These must move in lockstep with D1 or the fix is only half done:

- `nca_helpers.R:36-40` — `apply_blq_rules()` uses the identical period-blind
  profile key. Positional BLQ rules (1, 5, 6) therefore run "first quantifiable"
  and "post-Cmax" across both administrations at once. This is the same class of
  bug the v1.3.0 `REG-BLQ-01/02` fix addressed; it was fixed one level short.
- `data_quality.R:385-400` — the duplicate-time check branches on `has_trt`
  *before* `has_per`, so with both mapped it groups by Treatment, not
  Treatment × Period.
- `mod_path_be.R:1172-1178` and `mod_path_multi_nca.R:779-781` — the half-life
  override does `which(Subject == x & Treatment == y)` then
  `if (length(row_idx) == 1)`. With replicates that is length 2 → **silent
  no-op**: the user adjusts a terminal slope, sees the "Recalculated"
  notification, and nothing is written back.
- The half-life **profile selector labels** are built as `Subject | Treatment` at
  `mod_path_be.R:1028` and `mod_path_multi_nca.R:395`; with replicates two
  profiles share one label. `lz_sub_data` (`mod_path_be.R:1039-1049`, and its
  twin in `mod_path_multi_nca.R`) subsets by Subject + Treatment only, so the
  plotted profile is the interleaved one.
- `export_record.R:453-456` — the generated script's λz override replay uses the
  same `Subject | Treatment` match and `if (length(idx) == 1)` guard, so the
  **reproduction script would silently skip overrides** for replicates.

### 10.5 Found during pre-implementation verification (2026-09-17)

Every claim in §10.1–10.4 was re-checked against the code at v1.3.0 before any
implementation began: cited lines read, the validation suite run (191 automated
checks pass, 32 manual), defects reproduced by driving the real `run_nca()` with
the BE merge and model mirrored from `mod_path_be.R`, and the model compared
against `replicateBE::method.A` (1.1.3) on its reference datasets. D1–D6 are
confirmed. Five further defects were found.

| ID | Defect | Location |
|---|---|---|
| **D7** | **Period enters the model as a number, not a factor.** Only Treatment is coerced (`:501`). With > 2 periods, Period is fitted as a 1-df linear trend instead of Method A's period factor. Sequence is also uncoerced: harmless in `lm` (aliased with Subject), but wrong in `lme` if sequences are coded 1/2/3. Masked today because every 2-treatment design with > 2 periods also hits D1/D2, and invisible to the suite because its model copy coerces (D10). No effect on 2-period designs. | `mod_path_be.R:498-501` |
| **D8** | **Untransformed parameters get a verdict against percentage limits.** When `log_transform` is off, and **always for TMAX**, `pe`/`ci` are raw differences (h, ng/mL) and are compared with `be_lower`/`be_upper` as if they were percentages. Every TMAX row carries a meaningless YES/NO. | `mod_path_be.R:711-715` |
| **D9** | **BE Analysis Records with per-subject doses do not reproduce.** The record is built with `dose = input$dose` (a scalar) even when doses came from the Dose column, so `analysis_settings.json` says `dose_source: single` and the shipped script recomputes CL/F and Vz/F with the wrong dose. Also: BE settings (design, model type, CI level, limits, log-transform, parameters) are not written to the settings JSON, and the script does not reproduce the BE statistics at all. | `mod_path_be.R:1262-1273`; `export_record.R:933-966` |
| **D10** | **The BE validation tests exercise a copy, not the app.** `validation.R:449` defines its own `run_be()`, which coerces Period and Sequence to factors and uses its own formula. BE-AN, BE-MX and BE-NE pass or fail independently of `mod_path_be.R`, so no Tier 0 fix would be regression-tested. | `validation/validation.R:449-462` |
| **D11** | **Planner uses the generic label "3-period crossover" for `2x2x3`**, which is a full replicate. Fold into B5. | `mod_path_power.R:218, 222, 225` |

**D7, measured on `replicateBE` reference data** (app formula, fixed effects):

```
         method.A                          Period factor       Period integer (as shipped)
rds17    PE 134.18  CI 116.02-155.19 df 34  identical           PE 136.41  CI 118.78-156.67 df 35
rds01    PE 115.66  CI 107.11-124.89 df 217 identical           PE 115.74  CI 107.20-124.96 df 219
rds03    PE 124.19  CI 113.05-136.43 df 143 identical           PE 124.22  CI 113.11-136.42 df 144
```

**D8, measured:** 2×2, 24 subjects, true ratio 1, Cmax untransformed →
"PE" −1.61 (ng/mL difference), CI −7.60 to 4.39, verdict "NO" against 80–125.

**D9, measured:** 8-subject 2×2 with per-subject doses, record built exactly as
`mod_path_be.R` builds it, shipped script executed →
`Max relative difference: 3 -> DIFFERENT`.

---

## 11. Decisions taken

These were argued out with the three specialists. If you reverse one, record why.

### 11.1 Build full replicate support — Option B

The alternative considered was to detect repeated (Subject, Treatment) pairs,
refuse the file, and remove the `replicate_2x2x4` option since it cannot pass its
own check (~1 day). **Rejected** in favour of building it properly, because the
planning module can size these designs and the modules should agree.

### 11.2 Report CVwR as a diagnostic, but issue no scaled verdict

Both specialists independently argued that correct *unscaled* ABE on a replicate
design is a door returning a number users should not rely on — nobody runs a
replicate design to get 80–125 % limits. The agreed middle path:

Compute and display, on any design where the reference is replicated:

- **s_wR and CVwR**, from the **period-adjusted reference-only model**
- **CVwT and the s_wT : s_wR ratio** where estimable (full replicates only; the
  partial replicate `2x3x3` administers T once, so CVwT is **not estimable** —
  say so explicitly rather than showing a blank)
- the **ABEL limits those CVwR values would imply**, with the 50 % cap applied,
  clearly marked informational
- the **point estimate against the 80–125 % constraint**
- the conventional ABE result, as now

and then state plainly: *"These are the inputs to a scaled assessment. This app
does not issue a scaled bioequivalence verdict. For a regulatory decision use
`replicateBE` or validated commercial software."*

This closes the perceived gap at low regulatory exposure, and the CVwR estimate
feeds back into the planning module — closing the loop between the two modules in
the direction that actually matters.

> **Trap, verified by the statistician.** The CVwR model **must include a period
> term**. A naive `lm(log(PK) ~ subject)` on reference-only data disagreed with
> `replicateBE` on all ten datasets tested. On one it gave CVwR 312 % vs the
> correct 222 %. On another it **flipped the regulatory decision**:
> ```
> rds17  naive  swR 0.2847  CVwR 29.05%  -> RSABE applies = FALSE
>        adj    swR 0.2972  CVwR 30.39%  -> RSABE applies = TRUE
> ```
> The correct model is `lm(log(PK) ~ sequence + subject + period)` restricted to
> subjects with ≥ 2 reference observations, with every term added **conditionally
> on still having ≥ 2 levels after filtering** — a `2x2x3` TRT\|RTR design loses a
> whole sequence at that filter and `lm` otherwise errors.

### 11.3 Do NOT implement ABEL, RSABE or NTID verdicts

Unanimous across all three specialists. The mathematics is roughly 200 lines and
the statistician wrote a working prototype in an hour — that is not the problem.
The problem is that **every failure mode in this domain is silent and produces a
plausible number**, and the bias is asymmetric in the dangerous direction: scaled
methods widen the acceptance region, so a bug that widens further, or that drops
the point-estimate constraint, causes false BE claims.

The statistician's own prototype disagreed with the reference implementation on
all ten datasets at first attempt, and flipped a regulatory decision on one.

Named traps, all silent: the period term in the CVwR model (§11.2); degenerate
subsets after reference-only filtering; the chi-square tail direction in the
Hyslop bound (`qchisq(0.05, df)`, not `0.95` — reverse it and *every* study
passes); reference-outlier handling; and the point-estimate constraint and 50 %
cap, both trivial to code and trivial to forget.

`replicateBE` (Helmut Schütz) is peer-reviewed, actively maintained, and
qualified against the EMA reference datasets with published expected values.
Point at it.

**If this is ever revisited**, the non-negotiable minimum is: (i) a regression
suite running all 30 `replicateBE` datasets with tolerances, in CI, failing the
build on drift; (ii) the point-estimate constraint implemented *before* any
scaled limit is offered anywhere; (iii) a validation report in `validation/`.

### 11.4 Keep `fixed_order`, but reframe it

It is not a BE design — period and treatment are fully confounded — but it is
probably used more by this app's actual audience than any replicate design: DDI
studies (you cannot un-induce CYP3A4), hospital PK before and after a switch,
teaching datasets, food-effect and dose-proportionality pilots.

- Rename to **"Paired comparison (single sequence — not a bioequivalence design)"**.
- **Suppress the BE verdict entirely** for this design. Report point estimate and
  CI, and state that period and treatment are confounded.
- Detect it (one sequence level) rather than relying on the user to select it;
  the detection logic already exists at `mod_path_be.R:538`.
- **Do not** add `paired` to the planning module as a peer of `2x2`. If it goes in
  at all, it goes in a visually separated non-regulatory group.

### 11.5 Do not add designs with more than two treatments

`3x3`, `3x6x3`, `4x4`, `2x4x4`, `2x4x2` (Balaam's), `2x2x2r` (Liu's) — the
planning module does not offer them either, so this is **not** a sync gap.

Noted for the future: the clinical pharmacologist ranked multi-treatment support
(3 formulations, or test/reference/food-effect in one study) as a **bigger real
gap for this app's academic and hospital audience than reference scaling will
ever be** — and it shares the same root fix, period-aware keying. Worth
revisiting after Part B, not during.

### 11.6 Do not "fix" the type I error inflation

The statistician's simulations found α inflated above nominal in two situations:
a subject-by-formulation interaction on replicate designs (α ≈ 0.089 at
CVd = 0.30), and heteroscedasticity on the partial replicate `2x3x3`
(α ≈ 0.078 at CVwT 40 % / CVwR 25 %, because r_T = 1 while r_R = 2 makes the
pooled variance asymmetric).

**This is a known property of the regulatory method, not a defect to fix.** EMA
Method A *is* the pooled all-fixed ANOVA. Switching to a heteroscedastic or
subject-by-formulation model would diverge from Method A and break the exact
agreement demonstrated in §10.1. **Document the limitation; do not change the
model.** Surface a note on `2x3x3` that CVwT is not estimable and the pooled-
variance interval assumes CVwT ≈ CVwR.

---

## 12. Implementation plan

### Tier 0 — Safety fixes, ship first and independently (~1.5 days)

> **Status (2026-09-17): done.** Steps 1–9 implemented in `a85936b` (extraction),
> `7faf9bf` (D3, D4, D7, D8) and `5a73794` (D5, D6, D9, QC). 19 regression checks
> added; validation 211/211. Decisions taken while implementing:
> - **D3:** the point-estimate constraint is on by default for limits wider than
>   80–125% and can be switched off explicitly (for comparisons with no such
>   rule, e.g. DDI no-effect boundaries); the choice is recorded per row and in
>   the settings JSON.
> - **D8:** untransformed results are shown as a difference in their own units
>   with "no verdict", not hidden.
> - **Interim replicate refusal.** The data quality check now raises an ERROR
>   when a subject receives the same treatment in more than one period (Treatment
>   and Period mapped). Until B1/B2 land this turns D1's silent merge into a
>   visible refusal. **Remove this check as part of B2.** It cannot catch a
>   replicate file uploaded without a Period column mapped.

None of these depend on the replicate work, and D3 is a live false-pass risk.
Revised 2026-09-17 after the verification in §10.5: step 1 is new and must come
first, otherwise none of the fixes below can be regression-tested (D10).

1. **D10 — extract the BE fit and verdict** from the `observeEvent` in
   `mod_path_be.R` (`:597-727`) into a Shiny-free function in `R/` (model
   formula, coefficient extraction, CI, verdict). Point BE-AN / BE-MX / BE-NE at
   it and delete `run_be()` from `validation.R`. ~2 h. This is a narrow slice of
   Part A Phase 1, not a substitute for it.
2. **D3 — point-estimate constraint** (`mod_path_be.R:715`). ~1 h.
3. **D8 — no verdict for untransformed analyses** (`:711-715`): TMAX always, and
   any parameter when log-transform is off. Report the difference and CI in the
   parameter's units, labelled as such. ~1 h.
4. **D7 — coerce Period and Sequence to factors** before fitting (`:498-501`).
   ~30 min. No numeric change for any design that currently works (all have ≤ 2
   periods); required for B2 to reproduce Method A.
5. **D4 — `~1|Sequence/Subject` → `~1|Subject`** (`:631`). ~30 min. Removes the
   `NaN` warnings and the `DF = 0` row in the mixed-model ANOVA; treatment
   inference unaffected. The fixed-effects `drop1()` table still shows Sequence
   with `Df 0` — either test Sequence against Subject(Sequence) or omit that row.
6. **D5 — relocate the ABEL/RSABE warning** so it shows on `crossover_3period`
   as well as `replicate_2x2x4`.
7. **D6 — reframe `fixed_order`** and suppress its BE verdict (§11.4). ~2 h.
8. **D9 — record what was actually used**: the named per-subject dose vector
   (`mod_path_be.R:1264`), and the BE settings (design, model type, CI level,
   limits, log-transform, parameters) in `analysis_settings.json`. Add a
   regression test that a per-subject-dose BE record reproduces (MATCH).
   Reproducing the BE statistics in the script is a separate, larger item;
   until then, state in the record that the script reproduces the NCA only.
9. **Fix the misdirecting duplicate-time remediation text** (`data_quality.R:425-432`),
   which currently tells users to map columns they already mapped.

Each of 2–8 gets a regression test through the function from step 1, written to
fail before the fix.

### 12.1 Implementation record (2026-09-17)

Validation grew from 191 to 236 automated checks; every new check was
written to fail before its fix, except guards that protect existing behaviour.

| Phase | Commit | Delivered |
|---|---|---|
| B1 + B2 | `0ad844b` | `profile_key()` (Subject × Treatment × Period) used by `run_nca()`, `apply_blq_rules()` and — embedded verbatim — the reproduction script; key parts restored by `match()`. `build_be_data()` merges on all three keys, coerces types, and stops if the row count would change. CI table distinguishes subjects (N) from profiles (Obs). `nca_profile_key` in the settings JSON; schema 1.3.0. Interim replicate refusal removed. Fixtures in `validation/fixtures/`. |
| B3 | `138ea3d` | Profile helpers replace every `strsplit()` label lookup in the BE, batch and single-subject modules; half-life overrides write back to exactly one administration and are replayed per period by the script; replicate note on batch summary statistics. |
| B4 | `e277f37` | `be_variability_diagnostic()`: swR/CVwR, swT/CVwT (or "not estimable" for partial replicates), ratio, EMA ABEL limits with cap, PE vs 80–125%; shown under the CI table with the no-verdict statement and a pooled-variance note for 2×3×3; exported to Excel and the record. |
| B5 | `37a2a12` | `R/designs.R` registry drives the planner menus, the analysis menu, the scaled-analysis note and the About page; data-vs-selection check; legacy codes still accepted. Fixed "Williams design" mislabels in the Data Guide and Methods page. |
| B6 | `c47dbfc` | `REP-RBE-01`: agreement with `replicateBE::method.A` on all 30 reference data sets (DF identical; PE, CI, CVwR, CVwT, ABEL limits within 1e-8; observed ≤ 2e-12). |
| — | `193da3d` | Visualize legend notes pooled administrations for replicate designs. |

**Decisions taken while implementing** (reversals of §11–§13 recorded here):

- **Period column only when Period is mapped.** §13 proposed emitting it
  always. A column of NA for files without a period adds nothing; the grain is
  stated instead by `nca_profile_key` in every record.
- **Committed reference values plus a live check.** `make_reference_values.R`
  stores `replicateBE` results for the fixtures, so those checks need no extra
  package; `REP-RBE-01` additionally runs `replicateBE` on its 30 data sets.
  `replicateBE` is therefore a validation-only dependency, auto-installed by
  `validation.R` and never loaded by the app.
- **ABEL limits only (EMA).** The diagnostic shows EMA expanded limits; FDA
  RSABE quantities are not shown. No scaled verdict of any kind (§11.3 holds).
- **Design codes changed** to the registry codes (`2x2x2`, `2x2x3`, `2x3x3`,
  `2x2x4`, `parallel`, `paired`). Records written by the Tier 0 build use the
  old codes; `be_design_model()` still maps them.

### Phase B1 — Period-aware profile key (0.5 d)

Add one helper to `R/nca_helpers.R`:

```r
profile_key <- function(data, col_map)
  # -> list(key = <chr>, parts = data.frame(Subject, Treatment, Period))
```

Build from Subject + Treatment + Period when Period is mapped. **Split back by
`match()` against `parts`, not `strsplit()`** — this also retires the
"separator appears in a treatment name" hazard permanently.

Apply it **unconditionally**, not only when a repeat is detected: a key shape
that changes silently with the data is exactly the invisible behaviour this app
is otherwise good at avoiding, and it would make the generated reproduction
script data-dependent too.

`apply_blq_rules()` must use the same helper, or BLQ and NCA disagree about what
a profile is.

**Dose matching is unaffected.** The v1.3.0 named-dose lookup
(`match(final_keys, data[[nca_key]])`) is key-shape-agnostic. Verified.

### Phase B2 — The merge (1.5 d) — the hard one

`mod_path_be.R:476-490` must merge on `c("Subject","Treatment","Period")`.

> **Type-coercion trap.** `nca_res$Period` arrives as character from the key
> split; the uploaded Period column is usually integer. The merge will not match,
> Period becomes all-NA, and `lm` drops every row. Coerce both — exactly as the
> existing Subject coercion at `:480-483` already does, which exists because this
> class of bug was hit once before.
>
> This is a separate problem from D7. Coercing Period to character for the
> *merge* does not make it a factor in the *model*; Tier 0 step 4 handles that.

Also in this phase: `n1`/`n2` at `:665-666` count **rows**, and the results table
labels them "N (Test)". With replicates that reports 2n where every reader will
read subjects. Same in the design summary at `:770-822`. Fix the labelling to
distinguish subjects from administrations.

### Phase B3 — Downstream grain assumptions (2 d)

- **Half-life override across three modules** (`mod_path_be.R:1172`,
  `mod_path_multi_nca.R:781`, the `lz_sub_data` subsets at `mod_path_be.R:1039-1049` and in
  `mod_path_multi_nca.R`, the selector labels at `mod_path_be.R:1028` and
  `mod_path_multi_nca.R:395`, and the script's override replay at `export_record.R:453-456`):
  profile labels become `Subject | Treatment | Period`, which makes the
  `length == 1` guard work again. Three near-copies in the modules plus one string-built copy in the script generator — a candidate for sharing.
- **Summary statistics** (`utils.R:305-319` via `mod_path_multi_nca.R:549`):
  `summarize_pk_params()` pools both administrations into one geometric mean and
  labels the result "Geometric CV (%)", which readers take as between-subject CV.
  Decide and document what that column means for replicates.
- **NCA results table** gains a Period column (`mod_path_be.R:894-909`).
- **Forest plot is unaffected** — it works off `ci_table`, one row per parameter.
- **Exports are mostly free** — they write the result frame wholesale and pick up
  Period automatically.
- **Viz module is unaffected.** It works off `shared$pk_data` directly and never
  touches NCA results. One soft spot: the geometric-mean curve groups by
  `(.time, .treatment)` (`mod_path_viz.R:576`), averaging both administrations at
  each timepoint — defensible for a mean plot, worth a footnote.

### Phase B4 — CVwR diagnostic (1 d)

Per §11.2. Period-adjusted reference-only model, conditional term inclusion,
CVwT and the ratio where estimable, implied ABEL limits with the cap, marked
informational, no verdict. Validate against `replicateBE` on the EMA reference
datasets — the statistician confirmed the period-adjusted model matches exactly
on 10/10.

### Phase B5 — Design menu and shared registry (1 d)

Split the analysis menu to mirror the planner's vocabulary, with the design code
visible: `2x2` standard crossover; `2x2x3` 2-sequence 3-period full replicate;
`2x3x3` 3-sequence partial replicate; `2x2x4` 4-period full replicate;
`parallel`; `paired (fixed order — not a BE design)`.

`2x2x3` and `2x3x3` map to the **same fitted model** — correct, per §10.1 — but
get separate entries, separate help text, and the design code goes into the
export record.

Then a small shared registry, `R/designs.R`, ~40 lines, a plain data frame:

```r
BE_DESIGNS <- data.frame(
  code           = c("parallel","2x2x2","2x2x3","2x3x3","2x2x4","fixed_order"),
  label          = c(...),
  powertost_code = c("parallel","2x2","2x2x3","2x3x3","2x2x4","paired"),
  n_periods      = c(1,2,3,3,4,2),
  n_sequences    = c(NA,2,2,3,2,1),
  replicated     = c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE),
  plan_abe = ..., plan_scaled = ..., analyse_abe = ..., analyse_scaled = FALSE,
  analysis_note  = c(...),
  stringsAsFactors = FALSE)
```

The value is **not** DRY — two hardcoded lists is not a maintenance burden at
this size. The value is that **the registry is the scope table, and it is
executable, so the table cannot drift from the code**. Both module UIs consume it
via `subset()`; the About page and README render it with `kable()`. That is what
makes the reviewer's complaint structurally unable to recur.

Best payoff: `detect_study_design()` (`utils.R:253-286`) already emits an
`n_treatments × n_sequences × n_periods` code. Add `normalize_design_code()` and
the app can finally check **what the data is** against **what the user selected**
— roughly 10 lines once the vocabulary is shared.

### Phase B6 — Validation (1.5 d)

- **Fixtures** (`validation/fixtures/`, *not* `data/`): 2×2×4 TRTR\|RTRT,
  2×2×3 TRT\|RTR, 2×3×3 TRR\|RTR\|RRT, plus a 2×2 control. Generate them from a
  known PK model so expected values are analytic; commit both the generator and
  the generated CSVs.
- **Numeric agreement against `replicateBE::method.A`** on an EMA reference
  dataset. This is the single most persuasive artefact the project can produce
  and it is worth more than any number of extra dropdown entries.
- **Profile-count tests**: a 2×2×4 with n subjects must yield 4n NCA profiles and
  4n `be_data` rows — the test that would have caught both D1 and D2.
- **Regression tests for D3–D9**, through the extracted BE function (Tier 0 step 1).
- **Method A agreement through the app's code path**, with Period supplied as
  integer (as real files have it), so D7 cannot silently return.
- Four join keys in `validation.R` need updating: three use
  `paste(Subject, Treatment, sep="||")` (`:907, 921, 936`) and one uses
  `paste(Subject, Treatment)` (`:1066`).

> **Note on the existing suite.** `validation.R` cannot source the Shiny modules,
> so it text-extracts functions with brace counting (`:40`), and several checks
> are `grepl()` over source text — they assert a string exists, not that
> behaviour is correct. That is precisely how D1 survived 191 tests. This is the
> strongest argument for doing Part A Phase 1 first (§14).

---

## 13. Backward compatibility

**Verified: non-replicate results do not change.** A standard 2×2 with 12
subjects gives 24 unique keys under both the old `Subject||Treatment` and the new
`Subject||Treatment||Period` — same partition, same membership. Every derived
number is bit-identical.

What does move:

- **A new `Period` column** in the NCA results table and every export, for
  everyone. Emit it always rather than conditionally: a results table that states
  its own grain is worth the column.
- **Row order** can change in one corner case. The sort is lexicographic on the
  key, so treatments named `"A"` and `"AB"` reorder (`'B'` = 0x42 sorts before
  `'|'` = 0x7C). No reordering occurs with Test/Reference labels. Row order
  changes the bytes of `results.xlsx` and therefore its SHA-256.
- **Existing analysis records are not retroactively invalidated.** The manifest
  hashes the frozen source data, settings and results, and `generate_nca_script()`
  writes a self-contained copy of the keying logic at export time, so an old
  record reproduces itself forever. What changes is the cross-version claim.
- **Bump `RECORD_SCHEMA_VERSION`** (currently `"1.2.2"`, `export_record.R:9`) and
  — more valuable — **add `nca_profile_key: ["Subject","Treatment","Period"]` to
  `analysis_settings.json`** so every record states its own grain. That single
  line is what makes this change auditable years later.

---

## 14. Sequencing against Part A

The engineer's advice, recorded because it is easy to get wrong:

**Part A Phase 1 (extract `R/pipeline.R`) should come before Phase B1.**
`export_record.R` contains three separate string-built copies of the
composite-key and split-back logic (`:223-241`, `:356-360`). Phase 1 deletes all
three by shipping the pipeline into the record zip. Doing B first means writing
the keying fix three times and later deleting two — roughly one wasted focused
day, plus three chances to introduce a divergence.

Part A Phase 2 also already lists "stacked-file detection (duplicate times within
subject × treatment)" as an interlock, which is closely related to the
period-blind grain problem.

**Decision taken: build Part B now, but write the profile-key logic once** as a
helper in `nca_helpers.R` and have the script generators call it rather than
string-building their own copies. This captures most of Phase 1's benefit for
this specific change without doing all of Phase 1 first, and gets replicate
support out months sooner. Phase 1 can then collapse the remainder.

---

## 15. Effort, risk, and facts to verify

### Effort

| Phase | Work | Focused days |
|---|---|---|
| Tier 0 | Extract BE fit + safety fixes (D3–D10) — **independently shippable** | 1.5 |
| B1 | Period-aware profile key | 0.5 |
| B2 | Merge + type coercion + n-vs-rows labelling | 1.5 |
| B3 | Half-life review, summary stats, table columns | 2 |
| B4 | CVwR diagnostic | 1 |
| B5 | Design menu split + shared registry | 1 |
| B6 | Fixtures, replicateBE agreement, regression tests | 1.5 |
| — | Contingency (merge and half-life both have silent failure modes) | +2 |
| | **Total** | **~11** |

At 4–6 h/week that is roughly **4–6 calendar months**. Tier 0 alone is 1.5 days
and should not wait.

### Risks

- **Shipping B1 without B2.** Verified to make the confidence interval *worse*
  (24 → 48 rows). Write the profile-count fixture test first and watch it fail
  for the right reason.
- **Period type coercion in the merge** silently producing NA Periods that `lm`
  drops.
- **The half-life override** silently no-opping — it already does today, and the
  fix touches three near-identical copies.
- **Scope creep toward scaled verdicts.** The first user with a 2×2×4 will ask.
  §11.3 is the answer; write it into the URS rather than into a reply.

### Facts to verify before any of this reaches the manuscript

The statistical results in §10 are reproducible from the app's own code and need
no further verification. The regulatory claims do:

| Claim | Confidence | Action |
|---|---|---|
| EMA BE guideline specifies ANOVA with sequence, subject-within-sequence, period, formulation, all fixed | high on substance, medium on the section number | verify and cite the section |
| EMA Q&A defines Method A (preferred) and Method B | high — reproduced to 3 dp on 14 datasets | pin the Q&A revision number |
| **ICH M13A** (adopted 2024) and its treatment of the statistical model; M13B/M13C in development | medium — recent and moving | **verify the current text; this is the claim most likely to be out of date** |
| ABEL mechanics: CVwR > 30 % trigger, 50 % cap, PE constraint 80–125 %, Cmax-only scope | medium-high on mechanics, medium on the scope restriction | verify against the current guideline |
| FDA RSABE: linearised criterion, θ = (ln 1.25 / 0.25)², no upper cap | high on mechanics, medium on draft status | verify current HVD guidance version |
| FDA NTID: full replicate + s_wT/s_wR ≤ 2.5 + unscaled criterion | medium-high | verify the bound and the CI method |
| Fixed-order unacceptable for regulatory BE | high (structural) | cite the randomised-sequence requirement rather than asserting |
| Tmax non-parametric requirement — the app's current warning (`mod_path_be.R:592`) is slightly stronger than the guidance, which ties it to a clinically relevant rapid-onset claim | moderate | soften the wording or cite precisely |
| `replicateBE` is the de facto reference implementation | moderate-high | verify CRAN status and maintenance before depending on it |


---

## 16. Combined order across both workstreams

Reconciling Part A and Part B. Part B is the next version; Part A resumes after.

1. **Tier 0 safety fixes** (§12). About 1.5 days, independently shippable, and D3 (the
   missing point-estimate constraint) is a live false-pass risk that should not
   wait for anything else. Start by extracting the BE fit (D10) so every fix is
   regression-tested.
2. **The documentation overclaims** (§6) — the unsupported ALCOA+ claim, the
   `ncar` "CDISC SDTM compatible" attribution, `README.md:24`'s replicate claim,
   and the circular `URS-GEN-06` / `EXP-CD-01` pair. Cheap, and some are live in
   a manuscript under review.
3. **Part B, Phases B1–B6** (§12) — replicate design support with the CVwR
   diagnostic. ~10 focused days. Write the profile-key helper once (§14).
4. **Part A Phase 1** — extract `R/pipeline.R`. It collapses whatever key-building
   duplication Part B leaves behind, and it is the fix for the fact that
   `validation.R` cannot source the modules (§12, Phase B6 note).
5. **Part A Phase 2** — interlocks, folding in the stacked-file detection that
   overlaps Part B's grain checks.
6. **The standalone `adnca_to_flat.R` converter** (§7), then decide on Phase 3.
7. **Revisit multi-treatment designs** (§11.5) — ranked by the clinical
   pharmacologist as a bigger real gap for this audience than reference scaling,
   and it shares Part B's period-aware keying as its root fix.
