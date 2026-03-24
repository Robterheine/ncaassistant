# ============================================================================
# NCA Assistant — Statistical Methods Description
# ============================================================================
# Publication-ready descriptions of every statistical method used in the app.
# Reviewed by: medical writer, biostatistician, pharmacokineticist.

methods_ui <- function() {
  
  pkg_ver <- function(pkg) {
    tryCatch(as.character(packageVersion(pkg)), error = function(e) "?")
  }
  ver <- tryCatch(get("APP_VERSION", envir = globalenv()), error = function(e) "?")
  r_ver <- tryCatch(R.version.string, error = function(e) "R")
  r_year <- tryCatch(R.version$year, error = function(e) format(Sys.Date(), "%Y"))
  
  eq <- function(...) {
    tags$div(
      class = "bg-light rounded px-3 py-2 my-2 text-center",
      style = "font-family: 'Fira Code', monospace; font-size: 0.9rem; overflow-x: auto;",
      ...
    )
  }
  
  ref_box <- function(...) {
    tags$div(
      class = "border-start border-4 border-info ps-3 my-3 py-1",
      style = "font-size: 0.85rem;",
      ...
    )
  }
  
  tryCatch({
  tags$div(
    class = "container-fluid py-4",
    style = "max-width: 900px; margin: 0 auto;",
    
    tags$div(
      class = "text-center mb-4 p-4 rounded",
      style = "background: linear-gradient(135deg, #3498DB 0%, #2C3E50 100%);",
      tags$h3(class = "text-white fw-bold mb-2",
              icon("flask-vial", class = "me-2"),
              "Description of Statistical Methods"),
      tags$p(class = "text-white opacity-75 mb-0",
             "Publication-ready descriptions for the Methods section of your manuscript. ",
             "Copy the relevant sections and cite the R packages listed.")
    ),
    
    tags$p(class = "text-muted small",
           "All analyses were performed using the NCA Assistant application (v",
           ver, "), built in R (R Core Team, ", r_year, "). ",
           "The following sections describe each statistical method. ",
           "Adapt and combine the sections relevant to your study."),
    
    navset_card_pill(
      title = "Select Analysis Type",
      
      # ================================================================
      # NCA
      # ================================================================
      nav_panel(
        "Non-Compartmental Analysis",
        icon = icon("flask"),
        
        tags$h5(class = "fw-bold mt-3", "Non-Compartmental Analysis (NCA)"),
        
        tags$p(
          "Pharmacokinetic parameters were estimated by non-compartmental analysis ",
          "using the NonCompart R package (version ", pkg_ver("NonCompart"),
          "; Kim et al., 2018). The NonCompart package has been validated against ",
          "Certara Phoenix WinNonlin\u00AE by its authors using multiple reference datasets, ",
          "with results matching to at least four significant digits (Kim et al., 2018)."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Peak Exposure"),
        tags$p(
          "The maximum observed concentration (C", tags$sub("max"), ") and the time at which it ",
          "occurred (T", tags$sub("max"), ") were obtained directly from the observed data ",
          "without interpolation. T", tags$sub("max"),
          " is a discrete variable determined by the sampling schedule and was summarised ",
          "as median (range), not as arithmetic mean."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Pre-Dose Concentration at Time Zero"),
        tags$p(class = "small",
          "For extravascular administration, if no observation was recorded at time zero, ",
          "the concentration at time zero was assumed to be zero. For intravenous bolus ",
          "administration, the concentration at time zero (C", tags$sub("0"),
          ") was back-extrapolated from the log-linear regression of the first two ",
          "measurable concentrations to time zero. This back-extrapolated C", tags$sub("0"),
          " was used in the calculation of AUC", tags$sub("0\u2013t"), " and AUC", tags$sub("0\u2013\u221E"), "."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Area Under the Curve (AUC)"),
        tags$p(
          "The area under the plasma concentration\u2013time curve from time zero to the last ",
          "measurable concentration (AUC", tags$sub("0\u2013t"), ") was calculated using the ",
          "trapezoidal rule. Two methods are available:"
        ),
        
        tags$p(class = "small",
               tags$strong("Linear trapezoidal method: "),
               "The area of each trapezoid between consecutive time points ",
               "t", tags$sub("i"), " and t", tags$sub("i+1"), " was computed as:"),
        eq("AUC", tags$sub("i"), " = (t", tags$sub("i+1"),
           " \u2212 t", tags$sub("i"), ") \u00D7 (C", tags$sub("i"),
           " + C", tags$sub("i+1"), ") / 2"),
        
        tags$p(class = "small",
               tags$strong("Linear-up / log-down method (recommended): "),
               "The linear trapezoidal formula was applied during the ascending phase ",
               "(C", tags$sub("i+1"), " \u2265 C", tags$sub("i"), "). ",
               "During the descending phase (C", tags$sub("i+1"), " < C", tags$sub("i"), "), ",
               "the logarithmic trapezoidal formula was applied:"),
        eq("AUC", tags$sub("i"), " = (t", tags$sub("i+1"),
           " \u2212 t", tags$sub("i"), ") \u00D7 (C", tags$sub("i"),
           " \u2212 C", tags$sub("i+1"), ") / (ln C", tags$sub("i"),
           " \u2212 ln C", tags$sub("i+1"), ")"),
        tags$p(class = "small",
               "The log-down method provides a more accurate estimate during the ",
               "mono-exponential elimination phase (Yeh and Kwan, 1978) and is the default ",
               "in this application."),
        
        tags$h6(class = "fw-semibold mt-3", "Terminal Elimination Rate Constant (\u03BB", tags$sub("z"), ")"),
        tags$p(
          "The terminal elimination rate constant (\u03BB", tags$sub("z"), ") was estimated ",
          "by ordinary least squares linear regression of ln(concentration) versus time in ",
          "the terminal log-linear phase. ",
          "The regression was fitted to the last n data points after C", tags$sub("max"),
          " (where n \u2265 3), and the combination yielding the highest adjusted coefficient ",
          "of determination (R\u00B2", tags$sub("adj"), ") was selected automatically. ",
          "Only fits with R\u00B2", tags$sub("adj"), " \u2265 0.70 were accepted (configurable by the analyst). ",
          "The slope of the regression equals \u2212\u03BB", tags$sub("z"), ":"
        ),
        eq("\u03BB", tags$sub("z"), " = \u2212slope of the ln(C) vs. time regression"),
        
        tags$p(class = "small",
               "The analyst may manually include or exclude individual time points from ",
               "the terminal phase regression, after which \u03BB", tags$sub("z"),
               " and all dependent parameters (t", tags$sub("\u00BD"),
               ", AUC", tags$sub("0\u2013\u221E"), ", CL/F, V", tags$sub("z"),
               "/F, MRT) are recalculated accordingly."),
        
        tags$h6(class = "fw-semibold mt-3", "Terminal Half-Life"),
        tags$p("The terminal elimination half-life was calculated as:"),
        eq("t", tags$sub("\u00BD"), " = ln(2) / \u03BB", tags$sub("z")),
        
        tags$h6(class = "fw-semibold mt-3", "Extrapolation to Infinity"),
        tags$p(
          "AUC from time zero to infinity (AUC", tags$sub("0\u2013\u221E"), ") was calculated as:"
        ),
        eq("AUC", tags$sub("0\u2013\u221E"), " = AUC", tags$sub("0\u2013t"),
           " + C", tags$sub("last"), " / \u03BB", tags$sub("z")),
        tags$p(class = "small",
               "where C", tags$sub("last"), " is the last measurable concentration. ",
               "The percentage of AUC extrapolated beyond the last observation was reported ",
               "as AUC", tags$sub("%extrap"), ". ",
               "An AUC", tags$sub("0\u2013\u221E"), " estimate with > 20% extrapolation ",
               "should be interpreted with caution."),
        
        tags$h6(class = "fw-semibold mt-3", "Clearance and Volume of Distribution"),
        tags$p("For extravascular administration, apparent clearance and apparent volume of distribution were calculated as:"),
        eq("CL/F = Dose / AUC", tags$sub("0\u2013\u221E")),
        eq("V", tags$sub("z"), "/F = Dose / (\u03BB", tags$sub("z"),
           " \u00D7 AUC", tags$sub("0\u2013\u221E"), ")"),
        tags$p(class = "small",
               "where F denotes the (unknown) absolute bioavailability. For intravenous ",
               "administration, F = 1 by definition and these parameters reduce to CL and V", tags$sub("z"),
               ". The V", tags$sub("z"),
               " estimate reflects the volume of distribution during the terminal phase and assumes ",
               "complete distribution equilibrium."),
        
        tags$h6(class = "fw-semibold mt-3", "Mean Residence Time"),
        eq("MRT = AUMC", tags$sub("0\u2013\u221E"), " / AUC", tags$sub("0\u2013\u221E")),
        tags$p(class = "small",
               "where AUMC is the area under the first moment curve (concentration \u00D7 time ",
               "vs. time), computed by the same trapezoidal method as AUC. ",
               "For extravascular administration, this value represents MRT",
               tags$sub("oral"), ", which includes the mean absorption time (MAT): ",
               "MRT", tags$sub("oral"), " = MRT", tags$sub("iv"),
               " + MAT. MRT should therefore be interpreted in the context of the route of administration."),
        
        tags$h6(class = "fw-semibold mt-3", "Dose Normalization"),
        tags$p(class = "small",
               "When subjects received different doses, dose-normalized parameters were ",
               "computed by dividing the parameter value by the individual dose ",
               "(e.g., C", tags$sub("max"), "/Dose, AUC/Dose). ",
               "Dose normalization assumes dose-proportional (linear) pharmacokinetics. ",
               "If exposure does not scale proportionally with dose, dose-normalized ",
               "parameters should be interpreted with caution."),
        
        tags$h6(class = "fw-semibold mt-3", "Descriptive Statistics"),
        tags$p(class = "small",
               "Summary statistics were computed for each PK parameter across subjects. ",
               "Arithmetic mean, standard deviation (SD), and coefficient of variation ",
               "(CV% = SD/mean \u00D7 100) were reported. For log-normally distributed parameters ",
               "(C", tags$sub("max"), ", AUC), the geometric mean was calculated as ",
               "exp(mean(ln(x))) and the geometric CV% as sqrt(exp(var(ln(x))) \u2212 1) \u00D7 100. ",
               "T", tags$sub("max"), " was summarised as median and range. ",
               "All concentrations below the limit of quantification were handled ",
               "prior to summary computation (see BLQ Handling tab)."),
        
        ref_box(
          tags$strong("Software reference: "),
          "Kim H, Yun H, Cho S, et al. NonCompart: Noncompartmental Analysis for ",
          "Pharmacokinetic Data. ", tags$em("Transl Clin Pharmacol"), ". 2018;26(1):10\u201315.",
          tags$br(), tags$br(),
          tags$strong("Methodological reference: "),
          "Yeh KC, Kwan KC. A comparison of numerical integrating algorithms by trapezoidal, ",
          "Lagrange, and spline approximation. ",
          tags$em("J Pharmacokinet Biopharm"), ". 1978;6(1):79\u201398."
        )
      ),
      
      # ================================================================
      # STEADY-STATE
      # ================================================================
      nav_panel(
        "Steady-State NCA",
        icon = icon("rotate"),
        
        tags$h5(class = "fw-bold mt-3", "Steady-State Pharmacokinetic Analysis"),
        
        tags$p(
          "For multiple-dose studies at pharmacokinetic steady state, NCA was performed ",
          "on concentration data collected during a single dosing interval (\u03C4). ",
          "The attainment of steady state was assumed based on the study design ",
          "(i.e., sufficient dosing duration relative to the terminal half-life) ",
          "and was not formally verified by this application. Verification of steady state ",
          "(e.g., by comparing pre-dose trough concentrations across consecutive dosing ",
          "intervals) should be performed and reported separately."
        ),
        
        tags$p("The following additional parameters were derived:"),
        
        tags$h6(class = "fw-semibold mt-3", "AUC Within the Dosing Interval"),
        eq("AUC", tags$sub("\u03C4"), " = AUC", tags$sub("0\u2013\u03C4")),
        tags$p(class = "small",
               "Computed by the trapezoidal rule over the dosing interval, where \u03C4 is ",
               "the time between consecutive doses (e.g., 24 h for once-daily, ",
               "12 h for twice-daily dosing)."),
        
        tags$h6(class = "fw-semibold mt-3", "Average Concentration"),
        eq("C", tags$sub("avg"), " = AUC", tags$sub("\u03C4"), " / \u03C4"),
        
        tags$h6(class = "fw-semibold mt-3", "Trough Concentration"),
        tags$p(class = "small",
               "C", tags$sub("min"), " was defined as the minimum observed concentration ",
               "during the dosing interval, typically the pre-dose sample."),
        
        tags$h6(class = "fw-semibold mt-3", "Peak-Trough Fluctuation"),
        eq("Fluctuation (%) = (C", tags$sub("max"), " \u2212 C", tags$sub("min"),
           ") / C", tags$sub("avg"), " \u00D7 100"),
        
        tags$h6(class = "fw-semibold mt-3", "Swing"),
        eq("Swing = (C", tags$sub("max"), " \u2212 C", tags$sub("min"),
           ") / C", tags$sub("min")),
        
        tags$p(class = "small text-muted",
               "Note: C", tags$sub("avg"), ", Fluctuation, and Swing were derived by the application ",
               "from AUC", tags$sub("\u03C4"), " and the observed concentration data. ",
               "These parameters are not computed by the NonCompart package directly.")
      ),
      
      # ================================================================
      # BLQ
      # ================================================================
      nav_panel(
        "BLQ Handling",
        icon = icon("filter"),
        
        tags$h5(class = "fw-bold mt-3", "Handling of Concentrations Below the Limit of Quantification"),
        
        tags$p(
          "Concentrations below the lower limit of quantification (LLOQ) were handled ",
          "according to one of the following rules, selected prior to analysis. ",
          "These rules follow the recommendations described by Beal (2001) and are ",
          "consistent with the approaches implemented in Certara Phoenix WinNonlin\u00AE."
        ),
        
        tags$table(
          class = "table table-sm",
          style = "font-size: 0.85rem;",
          tags$thead(class = "table-light",
            tags$tr(tags$th("Rule"), tags$th("Description"))
          ),
          tags$tbody(
            tags$tr(tags$td(tags$strong("Rule 1"), " (default)"),
                    tags$td("BLQ concentrations occurring before the first quantifiable concentration ",
                            "were set to zero. BLQ concentrations after the last quantifiable concentration ",
                            "were treated as missing (excluded from analysis). BLQ values between quantifiable ",
                            "concentrations were set to zero.")),
            tags$tr(tags$td(tags$strong("Rule 2")),
                    tags$td("All BLQ concentrations were set to zero.")),
            tags$tr(tags$td(tags$strong("Rule 3")),
                    tags$td("All BLQ concentrations were excluded from the analysis (treated as missing).")),
            tags$tr(tags$td(tags$strong("Rule 4")),
                    tags$td("All BLQ concentrations were replaced by LLOQ / 2.")),
            tags$tr(tags$td(tags$strong("Rule 5")),
                    tags$td("BLQ concentrations before C", tags$sub("max"),
                            " were set to zero. BLQ concentrations after C", tags$sub("max"),
                            " were treated as missing (excluded from analysis)."))
          )
        ),
        
        tags$p(class = "small",
               "The LLOQ value and the rule applied were recorded and are included in the ",
               "exported audit trail and analysis report."),
        
        ref_box(
          tags$strong("Reference: "),
          "Beal SL. Ways to fit a PK model with some data below the quantification limit. ",
          tags$em("J Pharmacokinet Pharmacodyn"), ". 2001;28(5):481\u2013504."
        )
      ),
      
      # ================================================================
      # BIOEQUIVALENCE
      # ================================================================
      nav_panel(
        "Bioequivalence",
        icon = icon("arrows-left-right"),
        
        tags$h5(class = "fw-bold mt-3", "Bioequivalence Analysis"),
        
        tags$h6(class = "fw-semibold mt-3", "Log-Transformation"),
        tags$p(
          "In accordance with regulatory guidance (EMA, 2010; FDA, 2003), the pharmacokinetic ",
          "parameters C", tags$sub("max"), " and AUC were log-transformed (natural logarithm) ",
          "prior to statistical analysis. Bioequivalence was assessed on the log scale; results ",
          "were back-transformed by exponentiation and are presented as geometric mean ratios ",
          "(Test/Reference) with 90% confidence intervals."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Analysis of Variance (ANOVA)"),
        
        tags$p(tags$strong("2-period, 2-sequence crossover design (2\u00D72\u00D72):")),
        tags$p(class = "small",
               "A linear model was fitted to the log-transformed PK parameter:"),
        eq("ln(PK) = \u03BC + Sequence + Subject(Sequence) + Period + Treatment + \u03B5"),
        tags$p(class = "small",
               "where Sequence, Period, and Treatment are fixed effects, ",
               "Subject nested within Sequence accounts for between-subject variability, ",
               "and \u03B5 represents the within-subject residual error. ",
               "The treatment effect (the difference between Test and Reference on the log scale) ",
               "and its standard error were obtained from the model coefficients."),
        
        tags$p(tags$strong("Fixed-effects model"), " (R base function ", tags$code("lm()"), "):"),
        tags$p(class = "small",
               "Treatment, Period, Sequence, and Subject were entered as fixed effects. ",
               "The residual mean square error provided the estimate of within-subject variance. ",
               "The residual degrees of freedom from the model were used for inference. ",
               "This approach is appropriate for balanced designs (all subjects complete both periods)."),
        
        tags$p(tags$strong("Mixed-effects model"), " (nlme package, version ",
               pkg_ver("nlme"), "; Pinheiro & Bates, 2000):"),
        tags$p(class = "small",
               "Sequence, Period, and Treatment were modelled as fixed effects. ",
               "Subject nested within Sequence was modelled as a random effect:"),
        eq("ln(PK) = (\u03B2", tags$sub("0"), " + \u03B2", tags$sub("Seq"),
           " + \u03B2", tags$sub("Per"), " + \u03B2", tags$sub("Trt"),
           ") + (b", tags$sub("Subj(Seq)"), ") + \u03B5"),
        tags$p(class = "small",
               "The denominator degrees of freedom were determined by the containment method ",
               "as implemented in nlme (not the Satterthwaite or Kenward-Roger approximation). ",
               "For a balanced 2\u00D72\u00D72 crossover with N subjects, this yields N \u2212 2 ",
               "denominator degrees of freedom for the treatment effect. ",
               "The mixed-effects approach is recommended by the FDA for unbalanced designs ",
               "(e.g., when subjects drop out after period 1) and provides valid inference ",
               "regardless of balance."),
        
        tags$p(tags$strong("Parallel-group design:")),
        tags$p(class = "small",
               "A linear model with Treatment as the only fixed effect was fitted:"),
        eq("ln(PK) = \u03BC + Treatment + \u03B5"),
        tags$p(class = "small",
               "The residual degrees of freedom (N", tags$sub("T"), " + N", tags$sub("R"),
               " \u2212 2) were used for the confidence interval construction."),
        
        tags$h6(class = "fw-semibold mt-3", "90% Confidence Interval and TOST Equivalence"),
        tags$p(
          "The 90% confidence interval for the difference in log-transformed means ",
          "(\u03B4 = \u03BC", tags$sub("T"), " \u2212 \u03BC", tags$sub("R"),
          ") was constructed as:"
        ),
        eq("\u03B4 \u00B1 t", tags$sub("(0.95, df)"), " \u00D7 SE(\u03B4)"),
        tags$p(class = "small",
               "where SE(\u03B4) is the standard error of the treatment coefficient from the model, ",
               "df is the denominator degrees of freedom (residual for fixed-effects; ",
               "containment for mixed-effects), and t", tags$sub("(0.95, df)"),
               " is the 95th percentile of the t-distribution with df degrees of freedom. ",
               "The confidence interval was back-transformed by exponentiation and expressed ",
               "as a percentage:"),
        eq("90% CI = [exp(\u03B4 \u2212 t \u00D7 SE) \u00D7 100%, exp(\u03B4 + t \u00D7 SE) \u00D7 100%]"),
        
        tags$p(class = "small",
               "This 90% confidence interval approach is equivalent to performing two one-sided ",
               "tests (TOST; Schuirmann, 1987) at the 5% significance level each. Bioequivalence ",
               "is concluded if and only if both one-sided null hypotheses (H", tags$sub("01"),
               ": \u03BC", tags$sub("T"), "/\u03BC", tags$sub("R"),
               " \u2264 0.80 and H", tags$sub("02"), ": \u03BC", tags$sub("T"),
               "/\u03BC", tags$sub("R"),
               " \u2265 1.25) are rejected, which occurs precisely when the 90% CI ",
               "falls entirely within [80.00%, 125.00%]."),
        
        tags$h6(class = "fw-semibold mt-3", "Bioequivalence Conclusion"),
        tags$p(
          "Bioequivalence was concluded if the 90% confidence interval for the geometric ",
          "mean ratio (Test/Reference) fell entirely within the pre-specified acceptance ",
          "limits (default: 80.00\u2013125.00%), in accordance with EMA (2010) and FDA (2003) guidance."
        ),
        
        ref_box(
          tags$strong("Software references:"),
          tags$br(),
          "R Core Team. R: A Language and Environment for Statistical Computing. ",
          "R Foundation for Statistical Computing, Vienna, Austria, ", r_year, ".",
          tags$br(),
          "Pinheiro JC, Bates DM. ", tags$em("Mixed-Effects Models in S and S-PLUS"),
          ". Springer, New York, 2000.",
          tags$br(), tags$br(),
          tags$strong("Methodological reference:"),
          tags$br(),
          "Schuirmann DJ. A comparison of the two one-sided tests procedure and the power approach for ",
          "assessing the equivalence of average bioavailability. ",
          tags$em("J Pharmacokinet Biopharm"), ". 1987;15(6):657\u2013680.",
          tags$br(), tags$br(),
          tags$strong("Regulatory guidance:"),
          tags$br(),
          "EMA. Guideline on the Investigation of Bioequivalence. CPMP/EWP/QWP/1401/98 Rev. 1. 2010.",
          tags$br(),
          "FDA. Guidance for Industry: Bioavailability and Bioequivalence Studies Submitted in NDAs or ",
          "INDs \u2014 General Considerations. 2003."
        )
      ),
      
      # ================================================================
      # POWER & SAMPLE SIZE
      # ================================================================
      nav_panel(
        "Power & Sample Size",
        icon = icon("calculator"),
        
        tags$h5(class = "fw-bold mt-3", "Power Analysis and Sample Size Estimation"),
        
        tags$p(
          "Power and sample size calculations were performed using the PowerTOST R package ",
          "(version ", pkg_ver("PowerTOST"),
          "; Labes et al.), which implements exact methods based on Owen\u2019s Q function ",
          "(Owen, 1965) for average bioequivalence and simulation-based approaches for ",
          "reference-scaled methods."
        ),
        
        tags$h6(class = "fw-semibold mt-3",
                "Average Bioequivalence (ABE)"),
        tags$p(class = "small",
               "Sample size was determined using the exact power function for the ",
               "two one-sided tests (TOST) procedure (Schuirmann, 1987). ",
               "The required inputs were: the within-subject coefficient of variation (CV), ",
               "the expected geometric mean ratio (Test/Reference, denoted \u03B8", tags$sub("0"),
               "), the significance level (\u03B1 = 0.05), the target power (typically 0.80 or 0.90), ",
               "and the bioequivalence limits (\u03B8", tags$sub("1"),
               " = 0.80, \u03B8", tags$sub("2"), " = 1.25). ",
               "Power was computed as the probability of rejecting both one-sided null hypotheses ",
               "simultaneously, evaluated exactly via Owen\u2019s Q function."),
        
        tags$h6(class = "fw-semibold mt-3",
                "Highly Variable Drugs \u2014 EMA (ABEL)"),
        tags$p(class = "small",
               "For highly variable drug products (within-subject CV", tags$sub("wR"),
               " of the reference > 30%), ",
               "the EMA allows widening of the acceptance limits based on the within-subject ",
               "variability of the reference product (Average Bioequivalence with Expanding ",
               "Limits; ABEL). Power was estimated by simulation (default: 100,000 studies) ",
               "using the method implemented in PowerTOST. The acceptance limits were scaled as:"),
        eq("[exp(\u2212k \u00D7 s", tags$sub("wR"), "), exp(+k \u00D7 s", tags$sub("wR"),
           ")]  where s", tags$sub("wR"), " = sqrt(MSE", tags$sub("R"),
           ") and k = 0.760"),
        tags$p(class = "small",
               "The limits were capped at [69.84%, 143.19%]. A point estimate constraint ",
               "of 80.00\u2013125.00% additionally applied."),
        
        tags$h6(class = "fw-semibold mt-3",
                "Highly Variable Drugs \u2014 FDA (RSABE)"),
        tags$p(class = "small",
               "The FDA uses Reference-Scaled Average Bioequivalence (RSABE) for HVDs ",
               "(CV", tags$sub("wR"), " > 30%). The criterion was based on a linearized ",
               "scaled statistic:"),
        eq("(\u03BC", tags$sub("T"), " \u2212 \u03BC", tags$sub("R"),
           ")\u00B2 \u2212 \u03B8 \u00D7 \u03C3\u00B2", tags$sub("wR"),
           " \u2264 0"),
        tags$p(class = "small",
               "where \u03B8 = (ln 1.25 / \u03C3", tags$sub("0"),
               ")\u00B2 and \u03C3", tags$sub("0"),
               " = 0.25. A point estimate constraint of 80.00\u2013125.00% additionally applied. ",
               "Power was estimated by simulation using PowerTOST."),
        
        tags$h6(class = "fw-semibold mt-3",
                "Narrow Therapeutic Index Drugs (NTID)"),
        tags$p(class = "small",
               "For NTI drugs, the FDA requires tightened limits using a scaled approach ",
               "that constrains the ratio of within-subject variabilities: ",
               "\u03C3", tags$sub("wT"), " / \u03C3", tags$sub("wR"), " must not substantially ",
               "exceed 1. Power was estimated by simulation using PowerTOST."),
        
        tags$h6(class = "fw-semibold mt-3", "Supported Study Designs"),
        tags$table(
          class = "table table-sm small",
          tags$thead(class = "table-light",
            tags$tr(tags$th("Design"), tags$th("Description"))
          ),
          tags$tbody(
            tags$tr(tags$td("2\u00D72\u00D72"), tags$td("Standard 2-sequence, 2-period crossover (TR|RT)")),
            tags$tr(tags$td("2\u00D72\u00D73"), tags$td("2-sequence, 3-period crossover (TRT|RTR)")),
            tags$tr(tags$td("2\u00D73\u00D73"), tags$td("3-sequence, 3-period Williams design (TRR|RTR|RRT)")),
            tags$tr(tags$td("2\u00D72\u00D74"), tags$td("2-sequence, 4-period replicate crossover (TRTR|RTRT)")),
            tags$tr(tags$td("Parallel"), tags$td("Two parallel groups (each subject receives one treatment)"))
          )
        ),
        
        ref_box(
          tags$strong("Software reference:"),
          tags$br(),
          "Labes D, Sch\u00FCtz H, Lang B. PowerTOST: Power and Sample Size for (Bio)Equivalence Studies. ",
          "R package version ", pkg_ver("PowerTOST"), ".",
          tags$br(), tags$br(),
          tags$strong("Methodological references:"),
          tags$br(),
          "Schuirmann DJ. A comparison of the two one-sided tests procedure and the power approach for ",
          "assessing the equivalence of average bioavailability. ",
          tags$em("J Pharmacokinet Biopharm"), ". 1987;15(6):657\u2013680.",
          tags$br(),
          "Owen DB. A special case of a bivariate non-central t-distribution. ",
          tags$em("Biometrika"), ". 1965;52(3/4):437\u2013446."
        )
      ),
      
      # ================================================================
      # EXAMPLE PARAGRAPHS
      # ================================================================
      nav_panel(
        "Example Paragraph",
        icon = icon("copy"),
        
        tags$h5(class = "fw-bold mt-3", "Example Methods Paragraphs"),
        tags$p(class = "text-muted small",
               "Copy and adapt these texts for your manuscript. Replace items in [brackets] with your study details. ",
               "All text is written in past tense as appropriate for a Methods section."),
        
        tags$h6(class = "fw-semibold mt-3", "Standard PK study"),
        tags$div(
          class = "border rounded p-3 bg-light mb-3",
          style = "font-size: 0.88rem;",
          tags$p(
            "Pharmacokinetic parameters were determined by non-compartmental analysis. ",
            "The area under the plasma concentration\u2013time curve from time zero to the ",
            "last measurable concentration (AUC", tags$sub("0\u2013t"), ") was calculated using the ",
            "linear-up/log-down trapezoidal method. AUC from zero to infinity ",
            "(AUC", tags$sub("0\u2013\u221E"), ") was obtained by adding C", tags$sub("last"),
            "/\u03BB", tags$sub("z"), " to AUC", tags$sub("0\u2013t"),
            ". The terminal elimination rate constant (\u03BB", tags$sub("z"),
            ") was estimated by log-linear regression of the terminal phase, ",
            "selecting the number of points yielding the highest adjusted R\u00B2 ",
            "(\u2265 0.70). The terminal half-life was calculated as ln(2)/\u03BB", tags$sub("z"),
            ". C", tags$sub("max"), " and T", tags$sub("max"),
            " were obtained directly from the observed data. ",
            "T", tags$sub("max"), " was reported as median (range). ",
            "Apparent oral clearance (CL/F) was calculated as Dose/AUC", tags$sub("0\u2013\u221E"),
            " and apparent volume of distribution (V", tags$sub("z"),
            "/F) as Dose/(\u03BB", tags$sub("z"), " \u00D7 AUC", tags$sub("0\u2013\u221E"),
            "). Concentrations below the LLOQ ([state value and unit]) were handled ",
            "according to [state rule, e.g., Rule 1: pre-first-quantifiable set to zero, ",
            "post-last-quantifiable treated as missing (Beal, 2001)]. ",
            "Descriptive statistics included arithmetic mean, SD, CV%, geometric mean, ",
            "and geometric CV% for C", tags$sub("max"), " and AUC parameters. ",
            "All NCA computations were performed using the NonCompart R package (version ",
            pkg_ver("NonCompart"), "; Kim et al., 2018)."
          )
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Bioequivalence study"),
        tags$div(
          class = "border rounded p-3 bg-light mb-3",
          style = "font-size: 0.88rem;",
          tags$p(
            "Bioequivalence was assessed in a [2-period, 2-sequence crossover / ",
            "4-period replicate crossover / parallel-group] study with [N] [healthy subjects / ",
            "patients]. PK parameters (C", tags$sub("max"),
            ", AUC", tags$sub("0\u2013t"), ", AUC", tags$sub("0\u2013\u221E"),
            ") were determined by non-compartmental analysis as described above. ",
            "Log-transformed PK parameters were analysed using a linear ",
            "[fixed-effects / mixed-effects] model with Sequence, Period, and Treatment ",
            "as fixed effects [and Subject nested within Sequence as a random effect]. ",
            "[For the mixed-effects model, denominator degrees of freedom were determined ",
            "by the containment method as implemented in the nlme R package.] ",
            "The 90% confidence interval for the geometric mean ratio (Test/Reference) ",
            "was constructed from the treatment coefficient and its standard error. ",
            "This 90% CI approach is equivalent to two one-sided tests at the 5% level ",
            "(Schuirmann, 1987). ",
            "Bioequivalence was concluded if the 90% CI fell entirely within the ",
            "acceptance limits of 80.00\u2013125.00% (EMA, 2010; FDA, 2003). ",
            "The NCA was performed using NonCompart (version ",
            pkg_ver("NonCompart"),
            "; Kim et al., 2018)",
            "[, the mixed-effects model using nlme (version ",
            pkg_ver("nlme"),
            "; Pinheiro & Bates, 2000)], ",
            "and sample size estimation using PowerTOST (version ",
            pkg_ver("PowerTOST"),
            "; Labes et al.). All analyses were conducted in R (",
            r_ver, "; R Core Team, ", r_year, ")."
          )
        ),
        
        tags$h6(class = "fw-semibold mt-3", "References for your manuscript"),
        tags$div(
          class = "border rounded p-3 bg-light small",
          tags$p("Beal SL. Ways to fit a PK model with some data below the quantification limit. ",
                 tags$em("J Pharmacokinet Pharmacodyn"), ". 2001;28(5):481\u2013504."),
          tags$p("Kim H, Yun H, Cho S, et al. NonCompart: Noncompartmental Analysis for ",
                 "Pharmacokinetic Data. ", tags$em("Transl Clin Pharmacol"), ". 2018;26(1):10\u201315."),
          tags$p("Labes D, Sch\u00FCtz H, Lang B. PowerTOST: Power and Sample Size for ",
                 "(Bio)Equivalence Studies. R package version ",
                 pkg_ver("PowerTOST"), "."),
          tags$p("Owen DB. A special case of a bivariate non-central t-distribution. ",
                 tags$em("Biometrika"), ". 1965;52(3/4):437\u2013446."),
          tags$p("Pinheiro JC, Bates DM. ", tags$em("Mixed-Effects Models in S and S-PLUS"),
                 ". Springer, New York, 2000."),
          tags$p("R Core Team. R: A Language and Environment for Statistical Computing. ",
                 "R Foundation for Statistical Computing, Vienna, Austria, ", r_year, "."),
          tags$p("Schuirmann DJ. A comparison of the two one-sided tests procedure and ",
                 "the power approach for assessing the equivalence of average bioavailability. ",
                 tags$em("J Pharmacokinet Biopharm"), ". 1987;15(6):657\u2013680."),
          tags$p("Yeh KC, Kwan KC. A comparison of numerical integrating algorithms by trapezoidal, ",
                 "Lagrange, and spline approximation. ",
                 tags$em("J Pharmacokinet Biopharm"), ". 1978;6(1):79\u201398."),
          tags$p("EMA. Guideline on the Investigation of Bioequivalence. ",
                 "CPMP/EWP/QWP/1401/98 Rev. 1. 2010."),
          tags$p("FDA. Guidance for Industry: Bioavailability and Bioequivalence Studies ",
                 "Submitted in NDAs or INDs \u2014 General Considerations. 2003.")
        )
      )
    )
  )
  }, error = function(e) {
    tags$div(
      class = "container-fluid py-4",
      style = "max-width: 900px; margin: 0 auto;",
      tags$div(class = "alert alert-danger",
               tags$h5("Error loading Statistical Methods page"),
               tags$p("Details: ", e$message))
    )
  })
}
