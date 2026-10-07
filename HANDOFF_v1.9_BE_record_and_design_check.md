# Handoff for v1.9: a BE record that reproduces its verdict, and a design check that refuses

Written 7 October 2026 against commit `a7fda2c` (local `main`, v1.8.0 untagged, 596 automated and 77 manual tests).
Line numbers below are from that commit and will drift; the function names will not.

This file describes two pieces of work the product owner decided to do in v1.9, after the adversarial review of
7 October 2026 (`NCA_Assistant_adversarial_review_2026-10-07.md`, untracked, in the main folder):

- **Part A (review finding F1).** The Analysis Record of a bioequivalence run recomputes the NCA table but not the
  bioequivalence statistics. In v1.9 it recomputes the ANOVA, the confidence intervals and the verdict too, and says
  MATCH or DIFFERENT for them.
- **Part B (review finding F9, second half).** When the selected design does not fit the data, the app warns and
  analyses anyway. In v1.9 it refuses.

Do Part A first. Part B then goes into the function that Part A creates.

---

## 0. Before you start

Read `CLAUDE.md`. The rules that matter most here:

- **Surgical changes, verified.** Every changed line traces to this plan. Define the check before you change code.
- **Humanizer.** Every new or changed word a user or reader sees (UI text, messages, Methods page, help, manual,
  README, URS, protocol prose) goes through the `humanizer` skill, with no em dashes. Identifiers, constants and
  button labels that tests or the manual quote are exempt.
- **Git.** Commit locally only. Never push, tag or deploy without the owner's explicit consent. Merge into local
  `main` by fast-forward. Commit messages end with the `Co-Authored-By` line the harness gives you.
- **One document pass at the end** of the feature, not per step (README, manual, URS, validation README, protocol,
  version history, version bump).

How the app is put together, in one paragraph: `app.R` loads the modules in `R/mod_*.R` (Shiny). The statistics live
in Shiny-free files: `R/pipeline.R` (reading, BLQ rules, NCA, the record helpers), `R/be_analysis.R` (the BE model,
`build_be_data()`, M13A checks), `R/be_scaled.R` (ABEL and RSABE) and `R/designs.R` (the design registry). The
validation suite is one script, `validation/validation.R`, run from the project root; it sources those files
directly and takes about 11 minutes.

---

## Part A. The BE record reproduces the BE statistics

### A.1 What happens now

- The **run** happens in one large `observeEvent(input$run_be, ...)` in `R/mod_path_be.R` (about lines 545 to
  1028). Inside `withProgress()` it runs the NCA (`run_nca()`), builds the BE dataset (`build_be_data()`), picks the
  parameters, resolves the design (`resolve_be_design()`), loops over the parameters calling
  `be_assess_parameter()`, adds the Welch columns, runs the sensitivity analysis without exclusions, counts BLQ-based
  profiles, computes within-subject variability, and finally stores everything with `be_result(list(...))` and
  `be_run_settings(list(...))`.
- The **record** is built by `create_analysis_record()` in `R/export_record.R`, called from the download handler
  near line 1890 of `R/mod_path_be.R` with `be_results = be_result()` and `be_settings = run$be`.
  - It ships `nca_pipeline.R`, an exact copy of `R/pipeline.R` (`.ship_pipeline()`), plus `adnca_import.R` for
    ADNCA uploads, and writes `analysis_settings.json` (the BE settings go under `bioequivalence`).
  - It writes `results.xlsx` (the BE sheets) and `app_results_reference.csv` (the NCA table).
  - It generates `reproduce_analysis.R` with `generate_nca_script()`, then runs it in a separate R process at export
    time (`run_reproduction_check()`). The verdict comes from the script's last `Result: ...` line, which
    `compare_with_reference()` in `R/pipeline.R` prints.
  - It writes `reproduction_scope` into the JSON ("recomputes the NCA parameters only ...").

So the script stops after the NCA. The BE logic lives partly in the observer, which the record cannot run.

### A.2 What it should do

A reviewer unzips the record, runs `source("reproduce_analysis.R")` in a plain R session without the app, and gets:

1. the NCA table recomputed and compared, as today;
2. the BE table (point estimate, CI limits, PE constraint, verdict, and for ABEL and RSABE the route, s_WR, limits
   and criterion bound) recomputed from that NCA table with the recorded BE settings, and compared with what the app
   reported;
3. one final line `Result: MATCH` only when both match. A changed data file, changed code, a changed setting or a
   changed reference value gives `Result: DIFFERENT`.

The same rule as for the NCA applies: **the script contains no analysis logic of its own.** It sources the app's
files and calls the same function the app called. That is the only way it cannot drift from the app (see the comment
above `.script_header()` in `R/export_record.R`).

### A.3 Step 1: move the BE run into one Shiny-free function

Create `run_be_analysis()` in `R/be_analysis.R` (or a new `R/be_run.R`, Shiny-free, sourced by `app.R` and by the
validation suite). It does what the observer does between `run_nca()` and `be_result(...)`, with no `input$`, no
`shared$`, no `showNotification()`, no `gxp_guard()` and no reactive writes.

Suggested signature (adjust the names, keep the separation):

```r
run_be_analysis(nca_res, pk_data, col_map, be_settings, nca_settings,
                exclusions = NULL, data_unexcluded = NULL, lz_overrides = NULL)
```

- `be_settings` is a plain list holding everything the observer now reads from `input$`: `design_selected`,
  `reference`, `parameters` (as selected, before the steady-state swap), `approach` (the code: `"standard"`,
  `"abel"` or `"rsabe"`, after `approach_for()`), `model_type`, `log_transform`, `ci_level`, `be_lower`,
  `be_upper`, `pe_constraint`, `widened_scope`, `covariates` (the full spec from `cov_spec_input()`, including the
  categorical and log choices, not only the three columns stored today), `conc_unit` and `time_unit` (for
  `diff_unit_for()`), and `is_steady_state`.
- `data_unexcluded` replaces `data_without_exclusions(shared)` (that helper is in Shiny code, `R/mod_exclusions.R`).
  The observer computes it as now and passes it in; the record script recomputes it with `prepare_pk_dataset()`
  without the exclusions, as `data_without_exclusions()` already does.
- Return a list with:
  - the exact content of today's `be_result(...)`: `ci_table`, `anova`, `cv_table`, `design`, `sensitivity`,
    `covariates`, `covariate_coefs`, `approach`, `scaled_details`, `covariate_balance` and `m13a`;
  - `balance_info`, `params` and `design_used`;
  - `messages`: a list of `list(text, type, duration)` that the observer shows with `showNotification()`. This
    covers the steady-state swap, the design note, the mismatch, the missing Sequence, the incomplete subjects, the
    unavailable approach, the Tmax note, the mixed-model fallback and the per-parameter reasons;
  - `stop`: `NULL`, or the message of a classed condition (`be_covariate_error`, `be_scaled_error`, or the unexpected
    error) when the loop stopped. The observer then shows it and returns, as it does now with `cov_stopped`.

The observer keeps only Shiny work: validating inputs, running the NCA, `gxp_guard()`, calling `run_be_analysis()`,
showing `messages`, writing `be_result()`, `be_run_settings()` and `balance_result()`.

**Keep it identical.** This is a refactor of the most critical path; the numbers must not change by a single digit.

1. **Before touching the observer,** capture golden outputs. Write a `shiny::testServer()` test on
   `path_be_server` (the suite already uses `testServer` for the upload module; search for `testServer` in
   `validation/validation.R`). Load each case, set the inputs, trigger `run_be`, and save `be_result()` with
   `saveRDS()` into `validation/fixtures/be_run_golden/`. Cases:
   - `data/example_be_crossover.csv`, standard, fixed effects, and again with the mixed model;
   - `data/example_be_parallel_covariates.csv` with the Weight covariate;
   - `data/example_be_replicate_hvd.csv` with ABEL, and with RSABE;
   - `data/example_be_replicate_2x2x4.csv` with one exclusion, so the sensitivity table exists;
   - one steady-state case and one case with a supportive partial AUC.

   Commit the golden files together with the script that made them, so they can be regenerated.
2. Move the code, then assert that `run_be_analysis()` gives `identical()` results to the golden files, and that the
   `testServer` run of the refactored observer still gives the same `be_result()`.
3. Watch the code-inspection tests. About 30 tests read `R/mod_path_be.R` and anchor on exact strings, and some will
   fail when the code moves. At `a7fda2c` they are:

   `ARV-15 ARV-17 COV-13 COV-15 DOC-01 DOC-07 EXC-07 EXC-08 EXC-10 GXP-24 GXP-45 MRV-04 MRV-05 MRV-07 MRV-09
   NCA-OV-06 NCA-XO-02 REG-BE-D5-01 REL-15 REL-18 REL-20 REL-22 REL-27 REL-42 REL-43 REL-44 REL-52 REP-HL-03 REV3-02
   RSA-11 RSA-12 RSA-14 UI-BEL-01`

   Run the suite after the move. For each failure, find the string it looks for, move the anchor to the file where
   that code now lives, and keep the assertion as strict as it was. A test that only passes because its anchor was
   loosened has stopped testing anything.

### A.4 Step 2: record everything the function needs

In the download handler (`R/mod_path_be.R`, near line 1890) and in `create_analysis_record()`:

- Store the full `be_settings` list from A.3 in `analysis_settings.json` under `bioequivalence`. Check that it can be
  read back with `jsonlite::fromJSON(..., simplifyDataFrame = FALSE)` and gives the same values. Watch numbers that
  JSON turns into integers, factor levels, and the covariate data frame.
- Write the app's BE table as `app_be_reference.csv`, from `be_result()$ci_table`, unrounded columns as the app holds
  them. Add the sensitivity table when it exists (`app_be_sensitivity_reference.csv`). Put both in the integrity
  manifest (`write_integrity_manifest()`).
- Ship the code the function needs, the way `.ship_pipeline()` ships `nca_pipeline.R`: `be_analysis.R`,
  `be_scaled.R`, `designs.R`, and from `utils.R` whatever `be_analysis.R` calls (`friendly_name()`,
  `lz_flag_cols()`; check with a clean R session which functions are missing). Prefer shipping whole files over
  copying functions.
  - Record each file's SHA-256 in the JSON, and have the script compare them, as it already does for
    `nca_pipeline.R` and `adnca_import.R`.
  - `utils.R` holds a few `showNotification()` calls inside functions. Sourcing it without Shiny works as long as
    none of them runs. Confirm it in a session without Shiny loaded.
- Remove the `reproduction_scope` sentence, and change the record summary text in `generate_summary_html()` (around
  line 208, "runs the app's own pipeline ...") so it says the BE statistics are recomputed too.

### A.5 Step 3: the script and the comparison

- Give BE records their own script generator, `generate_be_script()`, beside `generate_nca_script()`, and choose it
  in `create_analysis_record()` when `be_results` is not `NULL`. It does the NCA part unchanged, then:
  1. checks the hashes of the shipped BE files;
  2. sources them;
  3. rebuilds `data_unexcluded` when exclusions are recorded;
  4. calls `run_be_analysis()` with the recorded settings;
  5. compares the result with `app_be_reference.csv`.
- Add `compare_be_with_reference(ci_table, ref_file, integrity)` next to `compare_with_reference()` in
  `R/pipeline.R` (it must be in the shipped `nca_pipeline.R`):
  - Compare numbers with the same tolerance rule: relative difference below 1e-6 is MATCH, below 1e-3 is CLOSE.
  - Compare `Bioequivalent`, `PE_Constraint`, `Route` and `Model` as text, exactly.
  - Match rows on `Parameter`, never on row order. A parameter on one side only is DIFFERENT.
  - Print `BE result: ...`.
- The last line the script prints must stay `Result: <verdict>`, because `run_reproduction_check()` parses exactly
  that. Combine the two: MATCH only when the NCA and the BE comparison both say MATCH; otherwise the worse of the two,
  in the order DIFFERENT, FAILED, CLOSE, NOT COMPARED.
- The script runs inside `run_reproduction_check()` with a 300-second timeout. RSABE and the mixed model are slower
  than the NCA, so time the largest example and raise the timeout if needed.

### A.6 Step 4: tests

Add them to the `REC` section of `validation/validation.R`. Use `rec_build(..., be = TRUE)` and
`rec_check_text()`, which `REC-04` already uses.

| Test | Builds | Expects |
|---|---|---|
| BE record, 2x2x2 crossover, standard | `example_be_crossover.csv` | `Result: MATCH`; the script printed `BE result: MATCH` |
| Mixed model | same, `model_type = "mixed"` | MATCH |
| Parallel with covariates | `example_be_parallel_covariates.csv`, Weight | MATCH, including the unadjusted columns |
| ABEL | `example_be_replicate_hvd.csv` | MATCH, including the scaled limits and the route |
| RSABE | same data, RSABE | MATCH, including `Crit_Bound` |
| Exclusions | 2x2x4 with one profile excluded | MATCH for the main and the sensitivity table |
| Tamper: reference | any of the above, then one CI limit changed in `app_be_reference.csv` | DIFFERENT |
| Tamper: setting | change `ci_level` or the reference treatment in the JSON | DIFFERENT |
| Tamper: code | change one character in the shipped `be_analysis.R` | DIFFERENT (hash) |
| Golden equivalence | `run_be_analysis()` against `validation/fixtures/be_run_golden/` | `identical()` for every case |

Update `REC-04`, which today expects MATCH from the NCA part only. The new tests are critical: they cover the
verdict.

### A.7 Done means

- Every test above passes, the full suite is green, and the golden files are unchanged.
- In the running app, a BE record downloaded for each example dataset shows "Reproduction check: MATCH" (from
  `notify_reproduction()`).
- Unzipping one record and running `source("reproduce_analysis.R")` in a fresh R session prints the NCA and BE
  comparisons and `Result: MATCH`.

---

## Part B. Refuse when the selected design does not fit the data

### B.1 What happens now

- `check_design_against_data(code, detected)` in `R/designs.R` (line 82) compares the selected design with
  `shared$study_info$design`, which `detect_study_design()` in `R/pipeline.R` (line 1790) computes at Process Data.
  It returns a message, or `NULL`, when:
  - Parallel is selected and the data have more than one period;
  - a crossover is selected and the data have one period;
  - the number of periods or sequences differs from the design's (`BE_DESIGNS$n_periods`, `n_sequences`).
- The observer shows the message as a warning and analyses anyway (`R/mod_path_be.R`, line 744). Since v1.8.0 the
  warning is also stored in `m13a`, so it reaches the Checks sheet and the record.
- `resolve_be_design()` (`R/be_analysis.R`, line 898) separately turns a crossover with a single treatment order
  into a paired comparison, with a note. That note is not a mismatch and stays a warning.

### B.2 What it should do

When `check_design_against_data()` returns a message, the BE run stops with an error notification that says what does
not fit and what to do: choose the design that matches, or check the Period and Sequence columns on the Upload page.
No results are written. The audit trail logs the refusal like other refused runs, so check how `gxp_guard()` is used
for those.

### B.3 Check first which data would be refused

The counts are numbers of distinct labels over the whole dataset, not per subject. A subject who misses a period does
not change them, so incomplete data are not refused. Two things would be:

- a period label that no subject has at all;
- sequence labels that carry more than the sequence, for example `TR-G1` and `TR-G2` in a study run in two groups.
  This is the multi-group case the change is meant to stop.

Before switching to a refusal, run `check_design_against_data()` for every dataset in `data/` and
`validation/fixtures/` against every design the suite or the manual's tutorials analyse it with. Search
`validation/validation.R` for each file name to find the designs. Any message that appears is either an intended
refusal, which needs a test, or a sign the check is too strict, which needs fixing before the switch. List the outcome
in the commit message.

### B.4 Change

- After Part A, the check lives in `run_be_analysis()`. Return it as `stop` with the message, instead of adding it to
  `messages`, and remove it from `m13a`.
- Rewrite the message text so it reads as a refusal (humanizer). Keep the facts it names: the selected design, what
  was found, what was expected.
- Update the anchors in `ARV-17` (`warn_run(mismatch, 15)` goes away) and the test `REP-DES-04`
  (`check_design_against_data()` itself does not change, so its assertions should still hold).

### B.5 Tests

- A 2x2x2 run on data with four sequences (two sequences in each of two groups, labelled per group) is refused, and
  no `be_result` is written. Use `testServer`, or `run_be_analysis()` after Part A.
- Parallel selected on crossover data is refused; a crossover selected on one-period data is refused.
- A 2x2x2 dataset where one subject misses period 2 is not refused, and analyses as today.
- The single-order case still becomes a paired comparison with its note.

### B.6 Done means

The tests pass. The datasets of B.3 behave as listed in the commit message. The scope sentence added in v1.8.0
(`BE_SCOPE_STATEMENT`: studies run in several groups are not modelled) stays.

---

## C. The document pass, once, at the end

Run these in this order. Each step depends on the one before.

1. **Version.** Set `APP_VERSION` in `app.R` to `1.9.0`.
   - The manual file name is a separate decision for the owner (review decision 5: the manual is v1.9 while the app
     is v1.8.0).
   - If the manual becomes v2.0, update its file name everywhere it is referenced: the `href` in the header of
     `app.R`, `DOC-05`, `validation/make_release_files.R` and the README.
2. **Text, through the humanizer:**
   - the v1.9.0 version-history entry in `app.R` (`about_ui()`);
   - the record chapter and the BE chapter of the manual (`NCA_Assistant_User_Manual_v1.9.docx`);
   - the README section on reproducibility;
   - `validation/README.md` (counts, section table rows for `REC` and the new tests).
3. **URS** (`validation/NCA_Assistant_URS.docx`):
   - Reword `URS-EXP-02` and `URS-EXP-05`, or add `URS-EXP-09` ("the reproducibility script shall recompute the
     bioequivalence statistics and verdict and compare them with the app's"), and state the refusal under
     `URS-BE-02`.
   - A new ID must be added in three places, or the coverage checks fail: the URS document, `all_urs` in
     `validation/validation.R` (line 7714, checked by `DOC-06`), and `urs_all` in `validation/make_iqoqpq.py`.
   - Add the matching FMEA entries if the URS document carries them for the record.
4. **Manual PDF.** Edit the `.docx` with python-docx, replacing text inside runs to keep the formatting. Export it with
   Word through AppleScript: copy the docx into `~/Library/Containers/com.microsoft.Word/Data/Documents/`, open it,
   update the tables of contents, `save as ... file format format PDF`, then copy the PDF to
   `www/NCA_Assistant_User_Manual_v1.9.pdf`. The export can take several minutes. If it hangs, Word is showing a
   dialog the owner has to close.
5. **Release files and protocol:**

   ```bash
   Rscript validation/make_release_files.R      # manifest + lockfile (IQ-REL-01 needs this after any app change)
   Rscript validation/validation.R              # DOC-05 fails here only on the protocol's count (item 9)
   /usr/bin/python3 validation/make_iqoqpq.py   # rebuilds the protocol from validation_results.csv
   Rscript validation/make_release_files.R      # the protocol changed
   Rscript validation/validation.R              # must end with FAIL: 0 and ALL CRITICAL TESTS PASSED
   ```

   `DOC-05` compares 14 counts in the README, `validation/README.md`, `app.R`, the protocol, the URS and the manual
   with the run. When it fails, it prints which items differ. `make_iqoqpq.py` ignores `DOC-05` in its all-pass check,
   for exactly this reason.
6. **Manual tests.** Add `MAN-` entries for anything only a running app can show: the reproduction notification on a
   BE record, and the refusal message. Record their execution as the owner decides; for v1.8.0 the owner waived
   personal execution and initials.

---

## D. Facts that are easy to get wrong

- `round_half_up()` (v1.8.0, finding F8) rounds the CI and point estimate the SAS way, and the BE table shows those
  rounded values. The record comparison must compare the unrounded values the app holds, or a MATCH can hide a
  difference at the third decimal.
- `be_scaled_notes()`, `be_m13a_checks()` and `parallel_welch_notes()` produce the `m13a` text. They belong in
  `run_be_analysis()`, so the record and the app agree on the checks too. Don't compare the text in the record check;
  compare the tables.
- The ANOVA list (`be_result()$anova`) holds `drop1()` and `anova.lme` objects. Keep them as they are; the Excel export
  turns them into data frames.
- `approach_for()` reads `input$be_approach` and falls back to `"standard"` when the approach is not available for
  the analysed design. Resolve that in the observer and pass the resolved code in `be_settings`. The fallback message
  stays a notification.
- The steady-state swap (AUCLST and AUCIFO become AUCTAU) is part of the analysis. Do it inside `run_be_analysis()`,
  so the record repeats it.
- Partial AUC roles (pivotal or supportive) come from `nca_settings$partial_aucs$role`. The NCA settings are already
  in the JSON (`partial_aucs`), but check that the role survives the round trip.
