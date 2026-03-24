# ============================================================================
# NCA Assistant — Statistical Methods Description
# ============================================================================
# Publication-ready descriptions of every statistical method used in the app.
# Written for the Methods section of a scientific manuscript.

methods_ui <- function() {
  
  # Helper: equation block
  eq <- function(...) {
    tags$div(
      class = "bg-light rounded px-3 py-2 my-2 text-center",
      style = "font-family: 'Fira Code', monospace; font-size: 0.9rem; overflow-x: auto;",
      ...
    )
  }
  
  # Helper: citation
  cite <- function(text) {
    tags$span(class = "text-muted fst-italic", text)
  }
  
  # Helper: section reference box
  ref_box <- function(...) {
    tags$div(
      class = "border-start border-4 border-info ps-3 my-3 py-1",
      style = "font-size: 0.85rem;",
      ...
    )
  }
  
  tags$div(
    class = "container-fluid py-4",
    style = "max-width: 900px; margin: 0 auto;",
    
    # Header
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
           APP_VERSION, "), built in R (R Core Team, 2024). ",
           "The following sections describe each statistical method. ",
           "Adapt and combine the sections relevant to your study."),
    
    # ====================================================================
    navset_card_pill(
      title = "Select Analysis Type",
      
      # ----------------------------------------------------------------
      # NCA
      # ----------------------------------------------------------------
      nav_panel(
        "Non-Compartmental Analysis",
        icon = icon("flask"),
        
        tags$h5(class = "fw-bold mt-3", "Non-Compartmental Analysis (NCA)"),
        
        tags$p(
          "Pharmacokinetic parameters were estimated by non-compartmental analysis ",
          "using the NonCompart R package (version ", as.character(packageVersion("NonCompart")),
          "; Kim et al., 2018), validated against Certara Phoenix WinNonlin\u00AE."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Peak Exposure"),
        tags$p(
          "The maximum observed concentration (C", tags$sub("max"), ") and the time at which it ",
          "occurred (T", tags$sub("max"), ") were obtained directly from the observed data ",
          "without interpolation."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Area Under the Curve (AUC)"),
        tags$p(
          "The area under the concentration\u2013time curve from time zero to the last ",
          "measurable concentration (AUC", tags$sub("0\u2013t"), ") was calculated using the ",
          "trapezoidal rule. Two methods are available:"
        ),
        
        tags$p(class = "small",
               tags$strong("Linear trapezoidal method: "),
               "The area of each trapezoid between consecutive time points ",
               "t", tags$sub("i"), " and t", tags$sub("i+1"), " is computed as:"),
        eq("AUC", tags$sub("i"), " = (t", tags$sub("i+1"),
           " \u2212 t", tags$sub("i"), ") \u00D7 (C", tags$sub("i"),
           " + C", tags$sub("i+1"), ") / 2"),
        
        tags$p(class = "small",
               tags$strong("Linear-up / log-down method (recommended): "),
               "The linear trapezoidal formula is used during the ascending phase ",
               "(C", tags$sub("i+1"), " \u2265 C", tags$sub("i"), "). ",
               "During the descending phase (C", tags$sub("i+1"), " < C", tags$sub("i"), "), ",
               "the logarithmic trapezoidal formula is applied:"),
        eq("AUC", tags$sub("i"), " = (t", tags$sub("i+1"),
           " \u2212 t", tags$sub("i"), ") \u00D7 (C", tags$sub("i"),
           " \u2212 C", tags$sub("i+1"), ") / (ln C", tags$sub("i"),
           " \u2212 ln C", tags$sub("i+1"), ")"),
        tags$p(class = "small",
               "The log-down method provides a more accurate estimate during the ",
               "mono-exponential elimination phase and is the default in this application."),
        
        tags$h6(class = "fw-semibold mt-3", "Terminal Elimination Rate Constant (\u03BB", tags$sub("z"), ")"),
        tags$p(
          "The terminal elimination rate constant (\u03BB", tags$sub("z"), ") was estimated ",
          "by linear regression of ln(concentration) versus time in the terminal phase. ",
          "The regression was fitted to the last n data points after C", tags$sub("max"),
          " (where n \u2265 3), and the combination yielding the highest adjusted coefficient ",
          "of determination (R\u00B2", tags$sub("adj"), ") was selected automatically. ",
          "Only fits with R\u00B2", tags$sub("adj"), " \u2265 0.7 were accepted (configurable). ",
          "The slope of the regression equals \u2212\u03BB", tags$sub("z"), "."
        ),
        eq("\u03BB", tags$sub("z"), " = \u2212slope of ln(C) vs. t regression"),
        
        tags$p(class = "small",
               "Manual override: individual time points can be included or excluded from ",
               "the regression by the analyst, after which all dependent parameters are recalculated."),
        
        tags$h6(class = "fw-semibold mt-3", "Terminal Half-Life"),
        eq("t", tags$sub("\u00BD"), " = ln(2) / \u03BB", tags$sub("z")),
        
        tags$h6(class = "fw-semibold mt-3", "Extrapolation to Infinity"),
        tags$p(
          "AUC from time zero to infinity (AUC", tags$sub("0\u2013\u221E"), ") was calculated as:"
        ),
        eq("AUC", tags$sub("0\u2013\u221E"), " = AUC", tags$sub("0\u2013t"),
           " + C", tags$sub("last"), " / \u03BB", tags$sub("z")),
        tags$p(class = "small",
               "where C", tags$sub("last"), " is the last measurable concentration. ",
               "The percentage of AUC extrapolated beyond the last observation is reported ",
               "as AUC", tags$sub("extrap"), "%."),
        
        tags$h6(class = "fw-semibold mt-3", "Clearance and Volume of Distribution"),
        tags$p("For extravascular administration:"),
        eq("CL/F = Dose / AUC", tags$sub("0\u2013\u221E")),
        eq("V", tags$sub("z"), "/F = Dose / (\u03BB", tags$sub("z"),
           " \u00D7 AUC", tags$sub("0\u2013\u221E"), ")"),
        tags$p(class = "small",
               "where F denotes the unknown oral bioavailability. For intravenous ",
               "administration, F = 1 and these reduce to CL and V", tags$sub("z"), "."),
        
        tags$h6(class = "fw-semibold mt-3", "Mean Residence Time"),
        eq("MRT = AUMC", tags$sub("0\u2013\u221E"), " / AUC", tags$sub("0\u2013\u221E")),
        tags$p(class = "small",
               "where AUMC is the area under the first moment curve (concentration \u00D7 time vs. time)."),
        
        tags$h6(class = "fw-semibold mt-3", "Dose Normalization"),
        tags$p(class = "small",
               "When subjects received different doses, dose-normalized parameters were ",
               "computed by dividing the parameter value by the individual dose ",
               "(e.g., C", tags$sub("max"), "/Dose, AUC/Dose)."),
        
        ref_box(
          tags$strong("Software reference: "),
          "Kim H, Yun H, Cho S, et al. NonCompart: Noncompartmental Analysis for ",
          "Pharmacokinetic Data. ", tags$em("Transl Clin Pharmacol"), ". 2018;26(1):10\u201315."
        )
      ),
      
      # ----------------------------------------------------------------
      # STEADY-STATE
      # ----------------------------------------------------------------
      nav_panel(
        "Steady-State NCA",
        icon = icon("rotate"),
        
        tags$h5(class = "fw-bold mt-3", "Steady-State Pharmacokinetic Analysis"),
        
        tags$p(
          "For multiple-dose studies at steady state, NCA was performed on concentration data ",
          "collected during a single dosing interval (\u03C4). The following additional parameters ",
          "were derived:"
        ),
        
        tags$h6(class = "fw-semibold mt-3", "AUC Within the Dosing Interval"),
        eq("AUC", tags$sub("\u03C4"), " = AUC", tags$sub("0\u2013\u03C4")),
        tags$p(class = "small",
               "Computed by the trapezoidal rule over the dosing interval, where \u03C4 is ",
               "the time between consecutive doses (e.g., 24 h for once-daily dosing)."),
        
        tags$h6(class = "fw-semibold mt-3", "Average Concentration"),
        eq("C", tags$sub("avg"), " = AUC", tags$sub("\u03C4"), " / \u03C4"),
        
        tags$h6(class = "fw-semibold mt-3", "Trough Concentration"),
        tags$p(class = "small",
               "C", tags$sub("min"), " is the minimum observed concentration during the dosing interval, ",
               "typically the pre-dose sample."),
        
        tags$h6(class = "fw-semibold mt-3", "Peak-Trough Fluctuation"),
        eq("Fluctuation (%) = (C", tags$sub("max"), " \u2212 C", tags$sub("min"),
           ") / C", tags$sub("avg"), " \u00D7 100"),
        
        tags$h6(class = "fw-semibold mt-3", "Swing"),
        eq("Swing = (C", tags$sub("max"), " \u2212 C", tags$sub("min"),
           ") / C", tags$sub("min")),
        
        tags$p(class = "small text-muted",
               "Note: C", tags$sub("avg"), ", Fluctuation, and Swing are derived in the application ",
               "from the NCA output and the observed data. They are not computed by NonCompart directly.")
      ),
      
      # ----------------------------------------------------------------
      # BLQ
      # ----------------------------------------------------------------
      nav_panel(
        "BLQ Handling",
        icon = icon("filter"),
        
        tags$h5(class = "fw-bold mt-3", "Handling of Concentrations Below the Limit of Quantification"),
        
        tags$p(
          "Concentrations below the lower limit of quantification (LLOQ) were handled ",
          "according to one of the following rules, selected by the analyst. ",
          "These rules are consistent with the recommendations of Shah et al. (1991) and ",
          "the approaches implemented in Certara Phoenix WinNonlin\u00AE."
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
                            "are set to zero. BLQ concentrations after the last quantifiable concentration ",
                            "are treated as missing (excluded). BLQ values between quantifiable ",
                            "concentrations are set to zero.")),
            tags$tr(tags$td(tags$strong("Rule 2")),
                    tags$td("All BLQ concentrations are set to zero.")),
            tags$tr(tags$td(tags$strong("Rule 3")),
                    tags$td("All BLQ concentrations are excluded from the analysis (treated as missing).")),
            tags$tr(tags$td(tags$strong("Rule 4")),
                    tags$td("All BLQ concentrations are replaced by LLOQ / 2.")),
            tags$tr(tags$td(tags$strong("Rule 5")),
                    tags$td("BLQ concentrations before C", tags$sub("max"),
                            " are set to zero. BLQ concentrations after C", tags$sub("max"),
                            " are treated as missing (excluded)."))
          )
        ),
        
        ref_box(
          tags$strong("Reference: "),
          "Shah VP, Midha KK, Dighe SV, et al. Analytical methods validation: bioavailability, ",
          "bioequivalence, and pharmacokinetic studies. ", tags$em("J Pharm Sci"), ". 1991;81(3):309\u2013312."
        )
      ),
      
      # ----------------------------------------------------------------
      # BIOEQUIVALENCE
      # ----------------------------------------------------------------
      nav_panel(
        "Bioequivalence",
        icon = icon("arrows-left-right"),
        
        tags$h5(class = "fw-bold mt-3", "Bioequivalence Analysis"),
        
        tags$h6(class = "fw-semibold mt-3", "Log-Transformation"),
        tags$p(
          "In accordance with regulatory guidance (EMA, 2010; FDA, 2003), the pharmacokinetic ",
          "parameters C", tags$sub("max"), " and AUC were log-transformed (natural logarithm) ",
          "prior to analysis. Bioequivalence was assessed on the log scale; results are ",
          "back-transformed and presented as geometric mean ratios (Test/Reference) with ",
          "90% confidence intervals."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Analysis of Variance (ANOVA)"),
        
        tags$p(tags$strong("2-period, 2-sequence crossover design (2\u00D72\u00D72):")),
        tags$p(class = "small",
               "A linear model was fitted to the log-transformed PK parameter:"),
        eq("ln(PK) = \u03BC + Sequence + Subject(Sequence) + Period + Treatment + \u03B5"),
        tags$p(class = "small",
               "where Sequence, Period, and Treatment are fixed effects and ",
               "Subject nested within Sequence accounts for between-subject variability. ",
               "The treatment effect (Test \u2212 Reference on the log scale) and its standard error ",
               "are obtained from the model coefficients."),
        
        tags$p(tags$strong("Fixed-effects model"), " (R function ", tags$code("lm()"), "):"),
        tags$p(class = "small",
               "Treatment, Period, Sequence, and Subject are all entered as fixed effects. ",
               "The residual mean square error provides the estimate of within-subject variance. ",
               "This is the standard approach for balanced crossover studies."),
        
        tags$p(tags$strong("Mixed-effects model"), " (R package nlme, version ",
               as.character(packageVersion("nlme")), "; Pinheiro & Bates, 2000):"),
        tags$p(class = "small",
               "Sequence, Period, and Treatment are modelled as fixed effects. ",
               "Subject nested within Sequence is modelled as a random effect:"),
        eq("ln(PK) = (\u03B2", tags$sub("0"), " + \u03B2", tags$sub("Seq"),
           " + \u03B2", tags$sub("Per"), " + \u03B2", tags$sub("Trt"),
           ") + (b", tags$sub("Subj(Seq)"), ") + \u03B5"),
        tags$p(class = "small",
               "The mixed-effects approach is recommended by the FDA for unbalanced designs ",
               "(e.g., when subjects drop out after period 1) and provides appropriate degrees of ",
               "freedom via the containment method."),
        
        tags$p(tags$strong("Parallel-group design:")),
        tags$p(class = "small",
               "A simple linear model with Treatment as the only fixed effect was fitted:"),
        eq("ln(PK) = \u03BC + Treatment + \u03B5"),
        
        tags$h6(class = "fw-semibold mt-3", "90% Confidence Interval"),
        tags$p(
          "The 90% confidence interval for the difference in log-transformed means ",
          "(\u03B4 = \u03BC", tags$sub("T"), " \u2212 \u03BC", tags$sub("R"),
          ") was constructed as:"
        ),
        eq("\u03B4 \u00B1 t", tags$sub("(1\u2212\u03B1/2, df)"),
           " \u00D7 SE(\u03B4)"),
        tags$p(class = "small",
               "where SE(\u03B4) is the standard error of the treatment coefficient from the model, ",
               "df is the residual (or Satterthwaite) degrees of freedom, and ",
               "\u03B1 = 0.10 for a 90% CI (two one-sided tests at the 5% level). ",
               "The confidence interval is back-transformed by exponentiation and expressed ",
               "as a percentage:"),
        eq("90% CI = [exp(\u03B4 \u2212 t \u00D7 SE) \u00D7 100% , exp(\u03B4 + t \u00D7 SE) \u00D7 100%]"),
        
        tags$h6(class = "fw-semibold mt-3", "Bioequivalence Conclusion"),
        tags$p(
          "Bioequivalence was concluded if the 90% confidence interval for the geometric ",
          "mean ratio (Test/Reference) fell entirely within the pre-specified acceptance ",
          "limits (default: 80.00\u2013125.00%), in accordance with EMA and FDA guidance."
        ),
        
        ref_box(
          tags$strong("Software references:"),
          tags$br(),
          "R Core Team. R: A Language and Environment for Statistical Computing. Vienna, Austria, 2024.",
          tags$br(),
          "Pinheiro JC, Bates DM. Mixed-Effects Models in S and S-PLUS. Springer, New York, 2000.",
          tags$br(), tags$br(),
          tags$strong("Regulatory guidance:"),
          tags$br(),
          "EMA. Guideline on the Investigation of Bioequivalence. CPMP/EWP/QWP/1401/98 Rev. 1. 2010.",
          tags$br(),
          "FDA. Guidance for Industry: Bioavailability and Bioequivalence Studies Submitted in NDAs or ",
          "INDs \u2014 General Considerations. 2003."
        )
      ),
      
      # ----------------------------------------------------------------
      # POWER & SAMPLE SIZE
      # ----------------------------------------------------------------
      nav_panel(
        "Power & Sample Size",
        icon = icon("calculator"),
        
        tags$h5(class = "fw-bold mt-3", "Power Analysis and Sample Size Estimation"),
        
        tags$p(
          "Power and sample size calculations were performed using the PowerTOST R package ",
          "(version ", as.character(packageVersion("PowerTOST")),
          "; Labes et al.), which implements exact methods based on Owen\u2019s Q function ",
          "for average bioequivalence and simulation-based approaches for ",
          "reference-scaled methods."
        ),
        
        tags$h6(class = "fw-semibold mt-3",
                "Average Bioequivalence (ABE)"),
        tags$p(class = "small",
               "Sample size was determined using the exact power function for the ",
               "two one-sided tests (TOST) procedure (Schuirmann, 1987). ",
               "The required inputs are: the within-subject coefficient of variation (CV), ",
               "the expected geometric mean ratio (Test/Reference), the significance level ",
               "(\u03B1 = 0.05), the target power (typically 0.80 or 0.90), and the ",
               "bioequivalence limits (default: 0.80\u20131.25). Power is computed as:"),
        eq("Power = P(reject H", tags$sub("01"), " and reject H", tags$sub("02"),
           " | \u03B8", tags$sub("0"), ", CV, n, design)"),
        tags$p(class = "small",
               "where H", tags$sub("01"), ": \u03BC", tags$sub("T"), "/\u03BC", tags$sub("R"),
               " \u2264 \u03B8", tags$sub("1"), " and H", tags$sub("02"),
               ": \u03BC", tags$sub("T"), "/\u03BC", tags$sub("R"),
               " \u2265 \u03B8", tags$sub("2"),
               " are the null hypotheses of the TOST procedure."),
        
        tags$h6(class = "fw-semibold mt-3",
                "Highly Variable Drugs \u2014 EMA (ABEL)"),
        tags$p(class = "small",
               "For highly variable drug products (within-subject CV of the reference > 30%), ",
               "the EMA allows widening of the acceptance limits based on the within-subject ",
               "variability of the reference product (Average Bioequivalence with Expanding ",
               "Limits; ABEL). Power was estimated by simulation (default: 100,000 studies) ",
               "using the method of Labes et al. The acceptance limits are scaled as:"),
        eq("[exp(\u2212k \u00D7 s", tags$sub("wR"), "), exp(+k \u00D7 s", tags$sub("wR"),
           ")]  where s", tags$sub("wR"), " = sqrt(MSE", tags$sub("R"),
           ") and k = 0.760"),
        tags$p(class = "small",
               "The limits are capped at 69.84\u2013143.19%. A point estimate constraint ",
               "of 80.00\u2013125.00% additionally applies."),
        
        tags$h6(class = "fw-semibold mt-3",
                "Highly Variable Drugs \u2014 FDA (RSABE)"),
        tags$p(class = "small",
               "The FDA uses Reference-Scaled Average Bioequivalence (RSABE) for HVDs. ",
               "The criterion is based on a linearized scaled statistic. ",
               "Power was estimated by simulation using PowerTOST."),
        
        tags$h6(class = "fw-semibold mt-3",
                "Narrow Therapeutic Index Drugs (NTID)"),
        tags$p(class = "small",
               "For NTI drugs, the FDA requires tightened limits using a scaled approach that ",
               "compares within-subject variability of Test and Reference. ",
               "Power was estimated by simulation using PowerTOST."),
        
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
          "R package version ", as.character(packageVersion("PowerTOST")), ".",
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
      
      # ----------------------------------------------------------------
      # EXAMPLE METHODS PARAGRAPH
      # ----------------------------------------------------------------
      nav_panel(
        "Example Paragraph",
        icon = icon("copy"),
        
        tags$h5(class = "fw-bold mt-3", "Example Methods Paragraph"),
        tags$p(class = "text-muted small",
               "Copy and adapt this text for your manuscript. Replace bracketed items with your study details."),
        
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
            " were determined directly from the observed data. ",
            "Apparent oral clearance (CL/F) was calculated as Dose/AUC", tags$sub("0\u2013\u221E"),
            " and apparent volume of distribution (V", tags$sub("z"),
            "/F) as Dose/(\u03BB", tags$sub("z"), " \u00D7 AUC", tags$sub("0\u2013\u221E"),
            "). Concentrations below the LLOQ [state value] were handled as follows: ",
            "pre-first-quantifiable set to zero, post-last-quantifiable treated as missing. ",
            "All NCA computations were performed using the NonCompart R package (version ",
            as.character(packageVersion("NonCompart")), "; Kim et al., 2018)."
          )
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Bioequivalence study"),
        tags$div(
          class = "border rounded p-3 bg-light mb-3",
          style = "font-size: 0.88rem;",
          tags$p(
            "Bioequivalence was assessed in a [2-period, 2-sequence, crossover] study ",
            "with [N] healthy subjects. PK parameters (C", tags$sub("max"),
            ", AUC", tags$sub("0\u2013t"), ", AUC", tags$sub("0\u2013\u221E"),
            ") were determined by non-compartmental analysis as described above. ",
            "Log-transformed PK parameters were analysed using a linear [mixed-effects] model ",
            "with Sequence, Period, and Treatment as fixed effects ",
            "[and Subject nested within Sequence as a random effect]. ",
            "The 90% confidence interval for the geometric mean ratio (Test/Reference) ",
            "was constructed from the treatment coefficient and its standard error using ",
            "the residual [or containment] degrees of freedom. ",
            "Bioequivalence was concluded if the 90% CI fell entirely within the ",
            "acceptance limits of 80.00\u2013125.00%. ",
            "The NCA was performed using NonCompart (version ",
            as.character(packageVersion("NonCompart")),
            "; Kim et al., 2018), the mixed-effects model using nlme (version ",
            as.character(packageVersion("nlme")),
            "; Pinheiro & Bates, 2000), and sample size estimation using PowerTOST (version ",
            as.character(packageVersion("PowerTOST")),
            "; Labes et al.). All analyses were conducted in R (version ",
            R.version.string, ")."
          )
        ),
        
        tags$h6(class = "fw-semibold mt-3", "References for your manuscript"),
        tags$div(
          class = "border rounded p-3 bg-light small",
          tags$p("Kim H, Yun H, Cho S, et al. NonCompart: Noncompartmental Analysis for ",
                 "Pharmacokinetic Data. ", tags$em("Transl Clin Pharmacol"), ". 2018;26(1):10\u201315."),
          tags$p("Pinheiro JC, Bates DM. ", tags$em("Mixed-Effects Models in S and S-PLUS"),
                 ". Springer, New York, 2000."),
          tags$p("Labes D, Sch\u00FCtz H, Lang B. PowerTOST: Power and Sample Size for ",
                 "(Bio)Equivalence Studies. R package version ",
                 as.character(packageVersion("PowerTOST")), "."),
          tags$p("R Core Team. R: A Language and Environment for Statistical Computing. ",
                 "R Foundation for Statistical Computing, Vienna, Austria, 2024."),
          tags$p("Schuirmann DJ. A comparison of the two one-sided tests procedure and ",
                 "the power approach for assessing the equivalence of average bioavailability. ",
                 tags$em("J Pharmacokinet Biopharm"), ". 1987;15(6):657\u2013680."),
          tags$p("Shah VP, Midha KK, Dighe SV, et al. Analytical methods validation: ",
                 "bioavailability, bioequivalence, and pharmacokinetic studies. ",
                 tags$em("J Pharm Sci"), ". 1991;81(3):309\u2013312."),
          tags$p("EMA. Guideline on the Investigation of Bioequivalence. ",
                 "CPMP/EWP/QWP/1401/98 Rev. 1. 2010."),
          tags$p("FDA. Guidance for Industry: Bioavailability and Bioequivalence Studies ",
                 "Submitted in NDAs or INDs \u2014 General Considerations. 2003.")
        )
      )
    )
  )
}
