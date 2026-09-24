# ============================================================================
# NCA Assistant — Contextual Help System
# ============================================================================
# Provides info_btn() — a small ℹ️ button that shows a popover with
# plain-language explanation. Every technical term, every decision point.
# Written for first-year PhD students who've never run NCA before.
# ============================================================================

#' Create an info button with a popover explanation
#'
#' @param id Unique identifier
#' @param title Short title shown in popover header
#' @param content Plain-language explanation (HTML allowed)
#' @return Shiny tag
info_btn <- function(id, title, content) {
  tags$button(
    type = "button",
    class = "btn btn-link btn-sm p-0 ms-1 info-help-btn",
    style = "font-size: 0.85rem; vertical-align: middle; color: #1F6FAE; border: none; background: none;",
    `aria-label` = paste("Help:", title),
    `data-bs-toggle` = "popover",
    `data-bs-trigger` = "focus",
    `data-bs-placement` = "auto",
    `data-bs-html` = "true",
    `data-bs-title` = title,
    `data-bs-content` = content,
    tabindex = "0",
    icon("circle-info")
  )
}

#' JavaScript to initialize all Bootstrap popovers
#' Call this once in the app UI header
help_init_js <- function() {
  tags$script(HTML("
    $(document).ready(function() {
      // Initialize popovers
      var popoverTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"popover\"]'));
      popoverTriggerList.map(function(el) {
        el.classList.add('popover-initialized');
        return new bootstrap.Popover(el, { sanitize: false });
      });
      
      // Re-initialize after Shiny updates DOM
      $(document).on('shiny:value', function() {
        setTimeout(function() {
          var newPopovers = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"popover\"]:not(.popover-initialized)'));
          newPopovers.map(function(el) {
            el.classList.add('popover-initialized');
            return new bootstrap.Popover(el, { sanitize: false });
          });
        }, 200);
      });
    });
  "))
}

# ============================================================================
# HELP CONTENT — Organized by module
# ============================================================================
# Every piece of text here is written for someone who has never done PK
# analysis before. No undefined jargon. Short sentences.

# --- DATA IMPORT -----------------------------------------------------------

help_data_format <- info_btn("help_data_format", "What should my data look like?",
  "Your data should be a table (CSV or Excel) where each row is one blood sample.
  <br><br>
  You need at minimum three columns:
  <ul>
  <li><b>Subject</b> — who the sample came from (e.g., 'S001', '1', 'Patient_A')</li>
  <li><b>Time</b> — when the sample was taken, as a number (e.g., 0, 0.5, 1, 2, 4, 8, 24)</li>
  <li><b>Concentration</b> — how much drug was measured (e.g., 12.5, 0, BLQ)</li>
  </ul>
  For crossover studies (e.g., bioequivalence), you also need Treatment and Period columns, and preferably Sequence.")

help_data_type <- info_btn("help_data_type", "What kind of file do I have?",
  "<b>Simple table</b> (most users): one row per blood sample, with columns such as
  Subject, Time and Concentration, and for crossover studies Treatment, Period and
  Sequence. This is what a lab spreadsheet usually looks like.
  <em>Not sure? Choose Simple table.</em>
  <br><br>
  <b>CDISC ADNCA dataset</b>: a standardised dataset from a pharmaceutical company
  or CRO. You can recognise it by column names such as <code>USUBJID</code>,
  <code>AVAL</code>, <code>PARAMCD</code>, <code>ARRLT</code>/<code>NRRLT</code> and
  <code>ANL01FL</code>.
  <br><br>
  <b>How the ADNCA option works:</b>
  <ol class='ps-3 mb-0'>
  <li>Upload the file (.csv or .xlsx).</li>
  <li>Check the summary and choose the time to use (and the analyte if there are several).</li>
  <li>Click <b>Convert dataset</b>. The app keeps only rows flagged for analysis
  (<code>ANL01FL = Y</code>) and refuses files it cannot use safely, with the reason.</li>
  <li>Continue as usual: the columns and LLOQ are filled in for you.</li>
  </ol>
  The choices and a conversion log are saved in the Analysis Record.
  <br><br>
  <b>Received a .xpt (SAS transport) file?</b> The app does not read XPT. Convert it
  to CSV first, for example in R:<br>
  <code>d &lt;- haven::read_xpt(\"adnca.xpt\")</code><br>
  <code>write.csv(haven::zap_labels(d), \"adnca.csv\", row.names = FALSE, na = \"\")</code>")

help_adnca_time <- info_btn("help_adnca_time", "Which time should I use?",
  "NCA needs the time since the dose of each profile.
  <br><br>
  <b>Nominal (NRRLT)</b>: the planned sampling times from the protocol (0, 0.5, 1, 2 h, ...).
  <br><br>
  <b>Actual (ARRLT)</b>: the times the samples were really taken. Pre-dose samples
  have small negative times; tick <em>Set pre-dose times to 0</em> to use them.
  <br><br>
  <b>Actual, pre-dose at 0 (MRRLT)</b>: actual times with pre-dose already at 0.
  <br><br>
  The choice changes AUC. Use what your analysis plan specifies; regulatory
  bioequivalence analyses usually use actual times.
  <br><br>
  <em>Time since the first dose (AFRLT) is not offered: in a crossover it would
  start period 2 at about one week.</em>")

help_adnca_analyte <- info_btn("help_adnca_analyte", "Why choose one analyte or matrix?",
  "The dataset contains more than one analyte (for example a drug and its metabolite)
  or matrix (for example plasma and urine) in the same column.
  <br><br>
  NCA is done for one analyte in one matrix at a time. Choose the one to analyse
  now; repeat the upload for the others. Values are never averaged or combined.")

help_column_mapping <- info_btn("help_col_mapping", "What is column mapping?",
  "The app needs to know which column in your data contains what information.
  <br><br>
  It tries to guess automatically. If it guesses wrong, just select the correct 
  column from the dropdown. The <b>required</b> fields are Subject, Time, and Concentration.
  <br><br>
  The optional fields (Treatment, Period, Sequence) are needed for crossover studies and 
  bioequivalence analysis; map Dose when doses differ between subjects or periods.")

help_lloq <- info_btn("help_lloq", "What is LLOQ?",
  "LLOQ = <b>Lower Limit of Quantification</b>. It's the lowest concentration your 
  lab assay can reliably measure.
  <br><br>
  Any measured value below the LLOQ is unreliable. These are called <b>BLQ</b> 
  (Below Limit of Quantification) values.
  <br><br>
  If your data has BLQ entries, set the LLOQ value here. The app will handle 
  them according to the BLQ rule you choose.
  <br><br>
  <b>If you don't know your LLOQ</b>, check the bioanalytical report from your lab.
  Leave at 0 if no BLQ handling is needed.")

help_blq_rules <- info_btn("help_blq_rules", "Which BLQ rule should I use?",
  "BLQ values are drug concentrations too low to measure accurately. 
  Different rules handle them differently:
  <br><br>
  <b>Rule 1</b> (default): Before the first measurable value → set to 0. 
  After the last measurable value → treat as missing. Between → set to 0.
  <br><br>
  <b>Rule 2</b>: Set all BLQ to 0. Late BLQ samples then add a declining tail to AUC.
  <br><br>
  <b>Rule 3</b>: Treat all BLQ as missing. Early BLQ samples no longer anchor the start of the curve.
  <br><br>
  <b>Rule 4</b>: Set all BLQ after dosing to half the LLOQ (LLOQ/2). These values are not used for the half-life.
  <br><br>
  <b>Rule 5</b>: Before Cmax → 0; after Cmax → missing.
  <br><br>
  <b>Rule 6</b>: Between dosing and the first measurable value → LLOQ/2; all other BLQ → 0.
  <br><br>
  <em>Rule 1 is the usual choice in NCA. Follow your analysis plan if it specifies a rule.</em>")

# --- NCA SETTINGS ----------------------------------------------------------

help_what_is_nca <- info_btn("help_what_is_nca", "What is NCA?",
  "<b>Non-Compartmental Analysis</b> is the simplest and most common way to 
  analyze drug concentration data.
  <br><br>
  It calculates key parameters like:
  <ul>
  <li><b>Cmax</b> — the highest concentration reached</li>
  <li><b>Tmax</b> — the time when Cmax occurs</li>
  <li><b>AUC</b> — the total drug exposure (area under the curve)</li>
  <li><b>Half-life</b> — how long it takes for the concentration to drop by half</li>
  <li><b>Clearance</b> — the volume of blood cleared of drug per unit of time</li>
  </ul>
  No mathematical model is assumed — it works directly from the observed data.")

help_admin_route <- info_btn("help_admin_route", "Which route should I choose?",
  "<b>Extravascular</b> — the drug was given by mouth (oral), injection into muscle (IM), 
  or under the skin (SC). The drug has to be absorbed first. <em>This is the most common choice.</em>
  <br><br>
  <b>IV Bolus</b> — the drug was injected directly into a vein all at once. 
  There is no absorption phase.
  <br><br>
  <b>IV Infusion</b> — the drug was given into a vein slowly, over a period of time 
  (e.g., a 30-minute drip). You'll need to specify how long the infusion lasted.")

help_trapezoidal <- info_btn("help_trapezoidal", "What trapezoidal method should I use?",
  "The trapezoidal method is how the app calculates the area under the curve (AUC).
  <br><br>
  <b>Linear-up / Log-down</b> (recommended): Uses linear interpolation while concentrations 
  are rising and logarithmic interpolation while they're declining. This is more accurate 
  for the typical drug profile shape and is widely used.
  <br><br>
  <b>Linear-up / Linear-down</b>: Uses straight lines everywhere. Simpler but slightly 
  overestimates AUC during the elimination phase.
  <br><br>
  <em>Use Linear-up/Log-down unless you have a specific reason not to.</em>")

help_lambda_z <- info_btn("help_lambda_z", "What is Lambda Z?",
  "<b>Lambda Z (λz)</b> is the rate at which the drug is eliminated from the body 
  during the terminal phase — the tail end of the concentration-time curve.
  <br><br>
  It's calculated by fitting a straight line through the last few points on a 
  log-concentration plot. The slope of that line is Lambda Z.
  <br><br>
  From Lambda Z, the app calculates:
  <ul>
  <li><b>Half-life</b> = ln(2) / λz</li>
  <li><b>AUC extrapolated to infinity</b></li>
  <li><b>Clearance and Volume of distribution</b></li>
  </ul>
  The <b>Half-Life Review</b> tab lets you see which points were used and 
  change them if needed.")

help_r2adj <- info_btn("help_r2adj", "What is Adjusted R²?",
  "<b>Adjusted R²</b> measures how well a straight line fits the terminal phase 
  of the concentration data (on a log scale).
  <br><br>
  A value of 1.0 = perfect fit. The threshold (default 0.7) means: only accept 
  the Lambda Z estimate if the fit quality is above this level.
  <br><br>
  <b>Higher threshold</b> (e.g., 0.9) = stricter, may reject more profiles.
  <br><br>
  <b>Lower threshold</b> (e.g., 0.5) = more permissive, accepts noisier fits.
  <br><br>
  <em>0.7 is a reasonable default. If many subjects have missing Lambda Z, try lowering it.</em>")

help_steady_state <- info_btn("help_steady_state", "When is data 'steady-state'?",
  "<b>Steady-state</b> means the drug has been given repeatedly (e.g., once daily for 
  a week) until the body reaches an equilibrium where the amount going in equals 
  the amount going out each dosing interval.
  <br><br>
  If your data comes from a <b>single dose</b> study (one-time administration), 
  leave this unchecked.
  <br><br>
  If the drug was given <b>multiple times</b> and you're looking at the profile 
  after several days of dosing, check this box and enter the dosing interval (τ), 
  for example 12 h for twice-daily dosing. The app then calculates AUCτ (AUC from 0 to τ), 
  average concentration, fluctuation and swing, and clearance from AUCτ.")

help_partial_auc <- info_btn("help_partial_auc", "What is a partial AUC?",
  "A <b>partial AUC</b> is the area under the curve over part of the profile, for
  example the first 30 minutes (AUC 0&ndash;0.5 h), or from 4 h to the last measurable
  concentration (AUC 4 h&ndash;t). It shows whether two products give the same early or
  late exposure, even when Cmax and total AUC agree.
  <br><br>
  <b>Where the times come from:</b> the protocol, or the product-specific guidance for
  the drug. Do not pick them after looking at the data.
  <br><br>
  <b>End at t:</b> the interval runs to each profile's last measurable concentration.
  <br><br>
  <b>Cutoff between samples:</b> the concentration at that time is interpolated with
  the trapezoidal method you chose, so the result depends on that choice. Sample at
  the cutoff times when you can.
  <br><br>
  <b>After the last measurable concentration:</b> the app does not extrapolate. A
  partial AUC that would need data after that point is left empty.
  <br><br>
  <b>Zero:</b> an early interval with only BLQ samples can be 0. A zero cannot be
  log-transformed, so bioequivalence gives no result for that interval.
  <br><br>
  <b>Pivotal or supportive:</b> in bioequivalence, a pivotal partial AUC gets a
  verdict against the acceptance limits; a supportive one is shown with its ratio and
  confidence interval only.
  <br><br>
  <b>Cmax and Tmax in an interval:</b> both are reported in the results table, and Cmax
  can be compared in bioequivalence when your protocol asks for the peak inside a window.
  Tmax within an interval is not offered there, because a window holds only a few
  sampling times, so a confidence interval on it would not mean much. Tmax over the whole
  profile can be compared, with the warning that it needs a non-parametric analysis.")

help_dose_norm <- info_btn("help_dose_norm", "What is dose normalization?",
  "Dose normalization divides PK parameters (like Cmax and AUC) by the dose.
  <br><br>
  This is useful when comparing different dose levels. For example, if 100 mg 
  gives Cmax = 50 ng/mL and 200 mg gives Cmax = 95 ng/mL, the dose-normalized 
  values (0.50 and 0.475) let you check whether the drug behaves proportionally 
  to dose.
  <br><br>
  <em>Enable this if your study includes multiple dose levels.</em>
  <br><br>
  Partial AUCs and the Cmax within an interval are normalised too; Tmax is not,
  because it is a time.")


# --- BE / STATISTICS -------------------------------------------------------

help_what_is_be <- info_btn("help_what_is_be", "What is bioequivalence?",
  "<b>Bioequivalence (BE)</b> testing determines whether two formulations of the 
  same drug (e.g., a generic vs. the original) produce the same drug levels 
  in the body.
  <br><br>
  The standard approach:
  <ol>
  <li>Give subjects both formulations (crossover design)</li>
  <li>Measure Cmax and AUC for each</li>
  <li>Calculate the ratio (Test/Reference) and its 90% confidence interval</li>
  <li>If the 90% CI falls within <b>80% to 125%</b>, the formulations are bioequivalent</li>
  </ol>
  This is required by regulatory agencies (FDA, EMA) for generic drug approval.")

help_log_transform <- info_btn("help_log_transform", "Why log-transform?",
  "PK parameters like Cmax and AUC follow a <b>log-normal distribution</b> — 
  they're skewed to the right (a few high values pull the average up).
  <br><br>
  Log-transformation makes the data approximately normal, which is required 
  for the ANOVA to work correctly. The results are then back-transformed to 
  give you a <b>geometric mean ratio</b> (%) and confidence interval.
  <br><br>
  This is the <b>standard method</b> recommended by all regulatory agencies. 
  Keep this checked unless you have a specific reason not to.
  <br><br>
  <em>Tmax is the exception — it's not log-transformed because it's ordinal.</em>")

help_ci_level <- info_btn("help_ci_level", "Why 90% and not 95%?",
  "Bioequivalence uses a <b>90% confidence interval</b>, not the usual 95%.
  <br><br>
  This is because the BE test is a <b>two one-sided test (TOST)</b> at the 
  5% significance level. The 90% CI for the ratio is mathematically equivalent 
  to two one-sided tests each at α = 0.05.
  <br><br>
  <em>Always use 90% for standard bioequivalence. Only change this if your 
  regulatory guidance specifies otherwise.</em>")

help_be_limits <- info_btn("help_be_limits", "What are the BE limits?",
  "The standard bioequivalence limits are <b>80% to 125%</b>.
  <br><br>
  This means: if the 90% confidence interval for the geometric mean ratio 
  (Test/Reference) falls entirely within 80–125%, the formulations are 
  considered bioequivalent.
  <br><br>
  <b>Narrow therapeutic index drugs</b> (e.g., warfarin, cyclosporine): the EMA 
  uses tighter limits of <b>90.00% to 111.11%</b>; the FDA uses a reference-scaled method.
  <br><br>
  <b>Highly variable drugs</b> (within-subject CV of the Reference above 30%) may use wider limits under 
  scaled approaches (ABEL, RSABE). Those approaches also require the
  <b>point estimate</b> to lie within 80–125%. When you enter limits wider
  than 80–125%, the app applies that constraint by default. This app does not
  calculate scaled limits; they must come from your protocol.
  <br><br>
  Under the EMA guideline wider limits apply to <b>Cmax only</b>; AUC is still judged
  against 80–125%. The app does the same unless you choose <i>All compared metrics</i>,
  for example for drug-interaction no-effect boundaries.
  <br><br>
  <em>Use 80–125% unless your regulatory guidance says otherwise.</em>")

help_be_design <- info_btn("help_be_design", "What design is my study?",
  "The study design describes how subjects received the treatments:
  <br><br>
  <b>2×2 Crossover</b> (most common): Each subject receives both Test and 
  Reference in two periods. Half get T first, half get R first.
  <br><br>
  <b>Parallel</b>: One group gets Test, another gets Reference. Subjects 
  only receive one treatment. Requires more subjects.
  <br><br>
  <b>Replicate designs</b>: a subject receives a treatment more than once. 
  2×2×4 (TRTR | RTRT) and 2×2×3 (TRT | RTR) are full replicates; 2×3×3 
  (TRR | RTR | RRT) is a partial replicate, where only the Reference is repeated. 
  Used for highly variable drugs.
  <br><br>
  <b>Paired comparison</b>: all subjects received the treatments in the same 
  order. Period and treatment cannot be separated, so no bioequivalence 
  verdict is given.
  <br><br>
  <em>Check your study protocol to confirm the design.</em>")

help_mixed_effects <- info_btn("help_mixed_effects", "Fixed vs. Mixed effects?",
  "<b>Fixed effects</b>: Treats Subject as a fixed factor. Standard for balanced 
  crossover studies. Simpler.
  <br><br>
  <b>Mixed effects</b>: Treats Subject as a <b>random</b> effect. Uses subjects 
  with only one treatment as well, which can help when there are dropouts 
  or unbalanced data.
  <br><br>
  For a balanced 2×2 crossover with no dropouts, both give the same result.
  <br><br>
  <em>Use the model your protocol or statistical analysis plan specifies.</em>")

# --- POWER & SAMPLE SIZE ---------------------------------------------------

help_what_is_power <- info_btn("help_what_is_power", "What is power & sample size?",
  "Before running a study, you need to know: <b>how many subjects do I need?</b>
  <br><br>
  <b>Power</b> is the probability that your study concludes bioequivalence 
  when the true Test/Reference ratio equals the ratio you expect. 
  The standard target is <b>80%</b> (meaning 80% chance of success).
  <br><br>
  The sample size depends on:
  <ul>
  <li><b>CV</b> — variability of the drug (higher CV = more subjects needed); for a parallel design the total CV</li>
  <li><b>T/R ratio</b> — how similar you expect the formulations to be</li>
  <li><b>Study design</b> — crossover needs fewer subjects than parallel</li>
  </ul>")

help_cv <- info_btn("help_cv", "What CV should I use?",
  "<b>CV (Coefficient of Variation)</b> measures how variable the drug is 
  between occasions within the same person.
  <br><br>
  <b>Enter as a percentage</b> — for example, type <b>20</b> for 20% variability.
  <br><br>
  <b>Where to find it:</b>
  <ul>
  <li>From a pilot study or previous BE study with the same drug</li>
  <li>From published literature</li>
  <li>From a bioequivalence analysis in this app (use the button that appears below after a log-transformed analysis)</li>
  </ul>
  <b>Typical ranges:</b>
  <ul>
  <li>Low variability: CV &lt; 15%</li>
  <li>Moderate: 15–30%</li>
  <li>Highly variable: &gt; 30% (a scaled approach, ABEL or RSABE, may then be allowed)</li>
  </ul>")

help_cv_wr <- info_btn("help_cv_wr", "What is the Reference CV?",
  "For scaled bioequivalence approaches (EMA ABEL, FDA RSABE, NTID), 
  you need the within-subject variability of the <b>Reference product specifically</b>.
  <br><br>
  This comes from a replicate design study where the Reference is given twice, 
  allowing its variability to be estimated separately from the Test.
  <br><br>
  <b>If you only have one CV estimate</b> (e.g., from a standard 2-period study), 
  enter the same value here as above. This assumes Test and Reference are equally variable.
  <br><br>
  <b>Enter as a percentage</b> — for example, type <b>35</b> for 35%.")

help_theta0 <- info_btn("help_theta0", "What is the expected T/R ratio?",
  "<b>T/R ratio</b> is your best guess for how similar the Test and Reference 
  formulations are.
  <br><br>
  <b>Enter as a percentage</b> — for example, type <b>95</b> if you expect the 
  Test to deliver 95% of the Reference exposure.
  <br><br>
  <ul>
  <li>Type <b>100</b> if you believe the formulations are identical.</li>
  <li>Type <b>95</b> as a standard assumption.</li>
  <li>Type <b>105</b> if you expect the Test to be slightly higher than Reference.</li>
  </ul>
  <b>When in doubt, use 95.</b> This is the conventional planning assumption.")

# --- LAMBDA Z INSPECTOR ----------------------------------------------------


# --- VISUALIZATION ---------------------------------------------------------



# --- ANALYSIS RECORD -------------------------------------------------------

help_analysis_record <- info_btn("help_analysis_record",
  "What is the Analysis Record?",
  "A <b>self-contained package</b> (a single zip file) that captures everything
  needed to reproduce and verify this analysis later.
  <br><br>
  It contains:
  <ul>
  <li><b>Results</b> — an Excel file with the parameters / figure</li>
  <li><b>Settings (JSON)</b> — every choice that affects the output, with package versions</li>
  <li><b>Reproducibility R script</b> — re-runs the exact analysis without this app</li>
  <li><b>Data integrity hash (SHA-256)</b> — shows whether the data file has changed since</li>
  <li><b>HTML summary</b> — a human-readable record of methods and environment</li>
  <li><b>A copy of your original data</b> — so the package stands on its own</li>
  </ul>
  <b>When to use it:</b> archiving, publication supplements, and a sponsor's
  study documentation, any time someone needs to see how the result was
  produced. It is not an audit trail or electronic signature (21 CFR Part 11,
  EU GMP Annex 11).")

# ============================================================================
# SHARED UI — Analysis Record panel (used by every analysis tab)
# ============================================================================

#' Render the consistent "Generate Analysis Record" panel
#'
#' One reusable component so the record action looks and behaves identically in
#' the Visualize, Single-Subject, Batch, and Bioequivalence tabs. Secondary
#' styling keeps it subordinate to each tab's primary action; the info popover
#' explains it; the inline analyst/study fields keep the main view uncluttered.
#'
#' Fixed input ids (dl_record, record_analyst, record_study) match the existing
#' server handlers in every module, so wiring is unchanged.
#'
#' @param ns The module namespace function
#' @param intro Optional short description of what THIS tab's record contains
#' @param button_label Label for the download button
#' @return A bslib card
analysis_record_ui <- function(ns, intro = NULL,
                               button_label = "Generate Analysis Record") {
  default_intro <- paste0(
    "Download a self-contained package — results, settings, a standalone R ",
    "script that reproduces this analysis, a SHA-256 data-integrity hash, and an ",
    "HTML summary. For archiving, publication supplements, and study documentation.")

  card(
    class = "mt-3 border-secondary-subtle",
    card_header(
      class = "bg-light d-flex align-items-center py-2",
      icon("file-zipper", class = "me-2 text-primary"),
      tags$span(class = "fw-semibold", "Analysis Record"),
      help_analysis_record,
      tags$span(class = "badge bg-secondary ms-2", style = "font-weight: 500;",
                "reproducibility")
    ),
    card_body(
      class = "py-3",
      tags$p(class = "text-muted small mb-2", intro %||% default_intro),
      layout_columns(
        col_widths = c(6, 6),
        textInput(ns("record_analyst"), "Analyst name (optional)",
                  value = "", placeholder = "Your name"),
        textInput(ns("record_study"), "Study name (optional)",
                  value = "", placeholder = "e.g., Study XYZ")
      ),
      downloadButton(ns("dl_record"), button_label,
                     class = "btn-outline-primary btn-sm",
                     icon = icon("file-zipper"))
    )
  )
}
