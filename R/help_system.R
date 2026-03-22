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
    style = "font-size: 0.85rem; vertical-align: middle; color: #3498DB; border: none; background: none;",
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
  For crossover studies (e.g., bioequivalence), you also need Treatment, Period, and Sequence columns.")

help_column_mapping <- info_btn("help_col_mapping", "What is column mapping?",
  "The app needs to know which column in your data contains what information.
  <br><br>
  It tries to guess automatically. If it guesses wrong, just select the correct 
  column from the dropdown. The <b>required</b> fields are Subject, Time, and Concentration.
  <br><br>
  The optional fields (Treatment, Period, Sequence) are only needed if you're doing 
  a crossover study or bioequivalence analysis.")

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
  <b>Rule 1</b> (recommended, WinNonlin default): Before the first measurable value → set to 0. 
  After the last measurable value → treat as missing. Between → set to 0.
  <br><br>
  <b>Rule 2</b>: Set all BLQ to 0. Simple but may overestimate AUC slightly.
  <br><br>
  <b>Rule 3</b>: Exclude all BLQ (treat as missing). Removes data, which may underestimate AUC.
  <br><br>
  <b>Rule 4</b>: Set all BLQ to half the LLOQ (LLOQ/2). A common compromise.
  <br><br>
  <b>Rule 5</b>: Before Cmax → 0; after Cmax → missing. Focuses on accurate terminal phase.
  <br><br>
  <em>When in doubt, use Rule 1. It's the industry standard.</em>")

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
  <li><b>Clearance</b> — how fast the body removes the drug</li>
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
  for the typical drug profile shape and is the <b>industry standard</b> (WinNonlin default).
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
  The <b>Lambda Z Inspector</b> tab lets you see which points were used and 
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
  after several days of dosing, check this box. The app will calculate 
  steady-state-specific parameters like AUCtau (AUC over one dosing interval).")

help_dose_norm <- info_btn("help_dose_norm", "What is dose normalization?",
  "Dose normalization divides PK parameters (like Cmax and AUC) by the dose.
  <br><br>
  This is useful when comparing different dose levels. For example, if 100 mg 
  gives Cmax = 50 ng/mL and 200 mg gives Cmax = 95 ng/mL, the dose-normalized 
  values (0.50 and 0.475) let you check whether the drug behaves proportionally 
  to dose.
  <br><br>
  <em>Enable this if your study includes multiple dose levels.</em>")

help_partial_auc <- info_btn("help_partial_auc", "What are partial AUCs?",
  "A partial AUC measures drug exposure over a specific time window, 
  not the entire curve.
  <br><br>
  For example, <b>AUC(0-4h)</b> measures early exposure (important for 
  onset of action), while <b>AUC(4-12h)</b> might capture sustained exposure.
  <br><br>
  Some regulatory guidelines require specific partial AUCs for 
  modified-release formulations.
  <br><br>
  <em>Leave empty for standard NCA. Add intervals if your protocol requires them.</em>")

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
  <b>Narrow therapeutic index drugs</b> (e.g., warfarin, cyclosporine) may 
  use tighter limits: <b>90% to 111%</b>.
  <br><br>
  <b>Highly variable drugs</b> (CV > 30%) may use wider limits under 
  scaled approaches (ABEL, RSABE).
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
  <b>Replicate Crossover</b> (e.g., 2×2×4): Each subject receives each 
  treatment twice. Used for highly variable drugs.
  <br><br>
  <em>Check your study protocol to confirm the design.</em>")

help_mixed_effects <- info_btn("help_mixed_effects", "Fixed vs. Mixed effects?",
  "<b>Fixed effects</b>: Treats Subject as a fixed factor. Standard for balanced 
  crossover studies. Simpler.
  <br><br>
  <b>Mixed effects</b> (recommended by FDA): Treats Subject as a <b>random</b> 
  effect nested within Sequence. More appropriate when there are dropouts 
  or unbalanced data.
  <br><br>
  For a balanced 2×2 crossover with no dropouts, both give the same result.
  <br><br>
  <em>Use mixed effects if you have missing data or your regulatory submission 
  targets the FDA.</em>")

# --- POWER & SAMPLE SIZE ---------------------------------------------------

help_what_is_power <- info_btn("help_what_is_power", "What is power & sample size?",
  "Before running a study, you need to know: <b>how many subjects do I need?</b>
  <br><br>
  <b>Power</b> is the probability that your study will correctly conclude 
  bioequivalence if the two formulations truly are equivalent. 
  The standard target is <b>80%</b> (meaning 80% chance of success).
  <br><br>
  The sample size depends on:
  <ul>
  <li><b>CV</b> — variability of the drug (higher CV = more subjects needed)</li>
  <li><b>T/R ratio</b> — how similar you expect the formulations to be</li>
  <li><b>Study design</b> — crossover needs fewer subjects than parallel</li>
  </ul>")

help_cv <- info_btn("help_cv", "What CV should I use?",
  "<b>CV (Coefficient of Variation)</b> measures how variable the drug is 
  between occasions within the same person.
  <br><br>
  <b>Where to find it:</b>
  <ul>
  <li>From a pilot study or previous BE study with the same drug</li>
  <li>From published literature</li>
  <li>From NCA results in this app (see summary statistics for Cmax and AUC)</li>
  </ul>
  <b>Typical ranges:</b>
  <ul>
  <li>Low variability: CV < 15%</li>
  <li>Moderate: 15–30%</li>
  <li>Highly variable: > 30% (may need scaled approaches like ABEL or RSABE)</li>
  </ul>
  <em>Enter as a decimal: 20% = 0.20</em>")

help_theta0 <- info_btn("help_theta0", "What is the expected T/R ratio?",
  "<b>θ₀ (Theta-zero)</b> is your best guess for the true ratio of Test to Reference.
  <br><br>
  If you believe the formulations are identical → θ₀ = 1.0 (ratio of 100%).
  <br><br>
  If you expect a small difference → θ₀ = 0.95 (Test is 5% lower than Reference).
  <br><br>
  <b>Conservative approach:</b> Use 0.95 (the FDA recommends this for sample size 
  planning). It builds in a safety margin.
  <br><br>
  <b>Optimistic approach:</b> Use 1.0 if you're confident the formulations are 
  very similar. This gives a smaller sample size but less margin for error.
  <br><br>
  <em>When in doubt, use 0.95.</em>")

# --- LAMBDA Z INSPECTOR ----------------------------------------------------

help_lz_inspector <- info_btn("help_lz_inspector", "How to use the Lambda Z Inspector",
  "This plot shows the <b>terminal elimination phase</b> of each subject's 
  concentration profile on a log scale.
  <br><br>
  <b>Red points</b> = currently included in the Lambda Z regression.
  <br>
  <b>Open circles</b> = excluded.
  <br>
  <b>Dashed red line</b> = the fitted regression line.
  <br><br>
  <b>To override the automatic selection:</b>
  <ol>
  <li>Click on a point to toggle it in/out of the regression</li>
  <li>The Lambda Z, half-life, and R² update automatically</li>
  <li>Look for a straight line on the log scale — that's a good fit</li>
  </ol>
  <b>When to override:</b>
  <ul>
  <li>If an early point bends the line (distribution phase not yet complete)</li>
  <li>If a late point is noisy and pulls the fit off</li>
  <li>If the automatic R² criterion excluded too many or too few points</li>
  </ul>")

# --- VISUALIZATION ---------------------------------------------------------

help_plot_types <- info_btn("help_plot_types", "Which plot type should I use?",
  "<b>Spaghetti plot</b>: Shows all subjects at once. Good for getting an overview 
  and spotting outliers. If you see one line far from the others, investigate.
  <br><br>
  <b>Mean ± SD/SEM</b>: Shows the average profile with error bars. Good for 
  presentations and reports. SD shows total spread, SEM shows precision of the mean.
  <br><br>
  <b>Individual browser</b>: Look at one subject at a time. Essential for quality 
  checking each profile before NCA.
  <br><br>
  <b>Box plots by time</b>: Shows the distribution at each time point. Good for 
  spotting time points with high variability.
  <br><br>
  <em>Start with the spaghetti plot on log scale to check your data, then 
  use mean ± SD for your report.</em>")

help_log_scale <- info_btn("help_log_scale", "Linear vs. log scale?",
  "<b>Linear scale</b>: What you'd normally expect. Good for seeing the actual 
  concentration values and peak height.
  <br><br>
  <b>Log scale</b>: Compresses high values and expands low values. 
  <b>Essential for PK analysis</b> because:
  <ul>
  <li>The terminal elimination phase appears as a straight line</li>
  <li>You can spot multi-phasic elimination (the line bends)</li>
  <li>Low concentrations become visible instead of being squashed at the bottom</li>
  </ul>
  <em>Always check both. Use log scale to assess the terminal phase.</em>")
