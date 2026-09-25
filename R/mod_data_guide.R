# ============================================================================
# NCA Assistant: Data Preparation Guide
# ============================================================================
# Scenario-based manual for preparing PK datasets.
# Target audience: first-year PhD students in clinical pharmacology.

data_guide_ui <- function() {
  
  # Helper: renders a small example table from a data.frame
  ex_table <- function(df) {
    tags$div(
      class = "table-responsive mb-3",
      tags$table(
        class = "table table-sm table-bordered",
        style = "font-size: 0.82rem; max-width: 700px;",
        tags$thead(
          class = "table-dark",
          tags$tr(lapply(names(df), function(n) tags$th(n)))
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(lapply(df[i, ], function(v) tags$td(as.character(v))))
          })
        )
      )
    )
  }
  
  # Helper: "do / don't" block
  do_dont <- function(do_items, dont_items) {
    layout_columns(
      col_widths = c(6, 6),
      tags$div(
        class = "border-start border-4 border-success ps-3 mb-3",
        tags$h6(class = "text-success fw-bold", icon("check"), " Do"),
        tags$ul(class = "small mb-0",
                lapply(do_items, function(x) tags$li(x)))
      ),
      tags$div(
        class = "border-start border-4 border-danger ps-3 mb-3",
        tags$h6(class = "text-danger fw-bold", icon("xmark"), " Don't"),
        tags$ul(class = "small mb-0",
                lapply(dont_items, function(x) tags$li(x)))
      )
    )
  }
  
  # Helper: checklist
  checklist <- function(items) {
    tags$div(
      class = "bg-light rounded p-3 mb-3",
      tags$h6(class = "fw-bold", icon("clipboard-check"), " Pre-upload checklist"),
      tags$ul(class = "list-unstyled small mb-0",
              lapply(items, function(x) tags$li(icon("square-check", class="text-success me-1"), x)))
    )
  }
  
  # Helper: a sorted example table (rows ordered as they would be in a file)
  code <- function(x) tags$code(x)

  # ======================================================================
  # PAGE LAYOUT
  # ======================================================================
  tags$div(
    class = "container-fluid py-4",
    style = "max-width: 1000px; margin: 0 auto;",

    # Header
    tags$div(
      class = "text-center mb-4 p-4 rounded",
      style = "background: linear-gradient(135deg, #F39C12 0%, #E74C3C 100%);",
      tags$h3(class = "text-white fw-bold mb-2",
              icon("book-open", class = "me-2"),
              "Data Preparation Guide"),
      tags$p(class = "text-white opacity-75 mb-0",
             "How to lay out concentration-time data for the app. ",
             "Find your study type below and follow the example.")
    ),

    # Example datasets
    card(
      class = "mb-3",
      card_header(icon("download"), " Example Datasets"),
      card_body(
        tags$p(class = "small",
               "Practice files for the app and for the worked examples in the user manual."),
        layout_columns(
          col_widths = c(4, 4, 4),
          tags$div(
            tags$h6(class = "fw-semibold", "Theophylline PK study"),
            tags$p(class = "small text-muted",
                   "12 subjects, one oral dose of about 320 mg (the Dose column is in mg/kg), ",
                   "11 samples per subject. Use with All Subjects (Batch)."),
            downloadButton("dl_example_theoph", "example_theoph.csv",
                           class = "btn-outline-primary btn-sm mb-3")
          ),
          tags$div(
            tags$h6(class = "fw-semibold", "2×2 crossover"),
            tags$p(class = "small text-muted",
                   "6 subjects, sequences TR and RT, 12 samples per period. ",
                   "Use with Bioequivalence (2×2 standard crossover)."),
            downloadButton("dl_example_be", "example_be_crossover.csv",
                           class = "btn-outline-primary btn-sm mb-3")
          ),
          tags$div(
            tags$h6(class = "fw-semibold", "Parallel groups"),
            tags$p(class = "small text-muted",
                   "12 subjects, 6 on Test and 6 on Reference, one period each. ",
                   "Use with Bioequivalence (Parallel groups)."),
            downloadButton("dl_example_parallel", "example_be_parallel.csv",
                           class = "btn-outline-primary btn-sm mb-3")
          )
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          tags$div(
            tags$h6(class = "fw-semibold", "2×2×4 full replicate"),
            tags$p(class = "small text-muted",
                   "12 subjects, sequences TRTR and RTRT. Use with Bioequivalence ",
                   "(2×2×4 full replicate); shows the within-subject variability table."),
            downloadButton("dl_example_replicate", "example_be_replicate_2x2x4.csv",
                           class = "btn-outline-primary btn-sm")
          ),
          tags$div(
            tags$h6(class = "fw-semibold", "CDISC ADNCA dataset"),
            tags$p(class = "small text-muted",
                   "12 subjects, 2×2 crossover in ADNCA layout (USUBJID, AVAL, NRRLT, ARRLT, ",
                   "ANL01FL, ...). Upload with 'What kind of file?' set to CDISC ADNCA dataset."),
            downloadButton("dl_example_adnca", "example_adnca.csv",
                           class = "btn-outline-primary btn-sm")
          ),
          tags$div(
            tags$h6(class = "fw-semibold", "BLQ results"),
            tags$p(class = "small text-muted",
                   "8 subjects, one oral dose of 100 mg, with results reported as '<0.5' (LLOQ 0.5 ng/mL), ",
                   "some lag times and one BLQ result between measurable samples. Use it to practise setting ",
                   "the LLOQ and choosing a BLQ rule."),
            downloadButton("dl_example_blq", "example_blq.csv",
                           class = "btn-outline-primary btn-sm")
          )
        )
      )
    ),

    # Quick reference card
    card(
      card_header(icon("table-list"), " Quick Reference: Which Columns Do I Need?"),
      card_body(
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm",
            style = "font-size: 0.85rem;",
            tags$thead(class = "table-light",
              tags$tr(tags$th("Study type"), tags$th("Columns"), tags$th("Where in the app"))
            ),
            tags$tbody(
              tags$tr(tags$td("One subject"),
                      tags$td(code("Time"), ", ", code("Concentration"), " (add ", code("Subject"), " to upload a file)"),
                      tags$td("One Subject at a Time")),
              tags$tr(tags$td("Several subjects, same dose"),
                      tags$td(code("Subject"), ", ", code("Time"), ", ", code("Concentration")),
                      tags$td("All Subjects (Batch)")),
              tags$tr(tags$td("Several subjects, different doses"),
                      tags$td(code("Subject"), ", ", code("Time"), ", ", code("Concentration"), ", ", code("Dose")),
                      tags$td("All Subjects: 'Doses differ by subject or period'")),
              tags$tr(tags$td("2×2 crossover"),
                      tags$td(code("Subject"), ", ", code("Treatment"), ", ", code("Period"), ", ", code("Sequence"), ", ", code("Time"), ", ", code("Concentration")),
                      tags$td("Bioequivalence: 2×2 standard crossover")),
              tags$tr(tags$td("Paired comparison (everyone in the same order)"),
                      tags$td("Same as 2×2; Sequence optional"),
                      tags$td("Bioequivalence: Paired comparison (no verdict)")),
              tags$tr(tags$td("2×2×3 full or 2×3×3 partial replicate"),
                      tags$td("Same as 2×2, Period 1 to 3"),
                      tags$td("Bioequivalence: 2×2×3 or 2×3×3")),
              tags$tr(tags$td("2×2×4 full replicate"),
                      tags$td("Same as 2×2, Period 1 to 4"),
                      tags$td("Bioequivalence: 2×2×4")),
              tags$tr(tags$td("Parallel groups"),
                      tags$td(code("Subject"), ", ", code("Treatment"), ", ", code("Time"), ", ", code("Concentration")),
                      tags$td("Bioequivalence: Parallel groups")),
              tags$tr(tags$td("Steady state"),
                      tags$td(code("Subject"), ", ", code("Time"), ", ", code("Concentration"), " (one dosing interval)"),
                      tags$td("All Subjects or Bioequivalence: tick 'Steady-state'")),
              tags$tr(tags$td("Drug interaction study"),
                      tags$td("As a crossover or paired comparison"),
                      tags$td("Bioequivalence (victim drug alone = Reference)")),
              tags$tr(tags$td("CDISC ADNCA dataset"),
                      tags$td("As delivered (USUBJID, AVAL, NRRLT or ARRLT, ...)"),
                      tags$td("Upload: 'What kind of file?' = CDISC ADNCA dataset"))
            )
          )
        ),
        tags$p(class = "text-muted small mt-2",
               tags$strong("Dose column: "),
               "not needed when everyone received the same dose; you enter it in the app. ",
               "When doses differ between subjects, or between periods of one subject, add a Dose ",
               "column. The app uses the dose of each profile (subject, treatment and period).")
      )
    ),

    # ====================================================================
    # SCENARIO TABS
    # ====================================================================
    navset_card_pill(
      title = "Choose Your Study Type",

      # ----------------------------------------------------------------
      # Single subject
      # ----------------------------------------------------------------
      nav_panel(
        "Single Subject",
        icon = icon("user"),
        tags$h5(class = "fw-bold mt-2", "One Subject, One Dose"),
        tags$p("You gave a drug to one person (or animal) and took blood samples over time. ",
               "You can type the data straight into One Subject at a Time, or prepare a file."),
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "For manual entry: the sampling times (hours after the dose) and the concentrations. ",
               "To upload a file, also add a Subject column. It can hold the same value on every row, ",
               "such as 'Patient1'. Without it the upload cannot tell which rows belong together."),
        ex_table(data.frame(
          Subject = rep("Patient1", 9),
          Time  = c(0, 0.25, 0.5, 1, 2, 4, 8, 12, 24),
          Concentration = c(0, 4.2, 12.8, 25.6, 18.3, 9.1, 3.4, 1.2, 0.15)
        )),
        do_dont(
          do_items = c(
            "Give time in hours since the dose",
            "Include the pre-dose sample at time 0",
            "Sample the terminal phase well: at least 3 declining points after the peak",
            "If the file uses decimal commas, choose 'Comma (,)' as decimal point when uploading"
          ),
          dont_items = c(
            "Use clock times ('08:30', '09:00'); the app refuses them",
            "Mix decimal points into a decimal-comma file: there a point only separates thousands (12.500 = 12500)",
            "Leave out the pre-dose sample",
            "Mix units (some values in ng/mL, others in µg/L)"
          )
        ),
        checklist(c(
          "Time is in hours since the dose (not clock time)",
          "One concentration unit throughout",
          "A time 0 sample is present",
          "At least 3 declining points after the peak for the half-life"
        ))
      ),

      # ----------------------------------------------------------------
      # Multiple subjects
      # ----------------------------------------------------------------
      nav_panel(
        "Multiple Subjects",
        icon = icon("users"),
        tags$h5(class = "fw-bold mt-2", "Several Subjects"),
        tags$p("Several subjects received the drug and were sampled at the protocol times. ",
               "This covers single-dose PK studies and dose-escalation studies."),

        tags$h6(class = "fw-semibold mt-3", "Case A: everyone received the same dose"),
        tags$p(class = "small",
               "Three columns: subject ID, time and concentration. Enter the dose in the app."),
        ex_table(data.frame(
          Subject = c("S001","S001","S001","S001","S002","S002","S002","S002","S003","S003","S003","S003"),
          Time = c(0,1,4,24, 0,1,4,24, 0,1,4,24),
          Concentration = c(0,22.5,8.3,0.4, 0,28.1,10.2,0.6, 0,19.8,7.1,0.3)
        )),

        tags$h6(class = "fw-semibold mt-4", "Case B: subjects received different doses"),
        tags$p(class = "small",
               "Dose escalation (cohort 1 gets 50 mg, cohort 2 gets 100 mg) or weight-based dosing. ",
               "Add a ", tags$strong("Dose"), " column with each subject's actual dose, in the dose unit you select in the app."),
        ex_table(data.frame(
          Subject = c("S001","S001","S001","S002","S002","S002","S003","S003","S003"),
          Dose    = c(50,50,50, 50,50,50, 100,100,100),
          Time    = c(0,1,24, 0,1,24, 0,1,24),
          Concentration = c(0,10.2,0.2, 0,12.8,0.3, 0,25.1,0.5)
        )),
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("In the app: "),
          "map the Dose column during upload, then choose ",
          "'Doses differ by subject or period (from Dose column in data)' in the analysis settings. ",
          "Clearance and volume then use each subject's own dose. The Dose column should hold one ",
          "value per profile; if it varies within a profile, the app uses the largest value."
        ),

        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("Planning a partial AUC: "),
          "if the protocol asks for a partial AUC, take samples at the cutoff times (for example at 30 min ",
          "for AUC 0\u201330 min). A cutoff between two samples is interpolated, and the result then depends on ",
          "the trapezoidal method. A late interval that ends after the last measurable concentration gets no ",
          "value, because the app does not extrapolate."
        ),

        tags$h6(class = "fw-semibold mt-4", "Actual or nominal times?"),
        tags$p(class = "small",
               "Use the actual sampling times for the NCA (0.48 h rather than the protocol's 0.5 h); ",
               "they give the more accurate AUC, and subjects do not need identical times. ",
               "The mean curve in Visualize Data averages samples with the same time value, so for ",
               "that plot a file with nominal times gives a cleaner curve."),

        do_dont(
          do_items = c(
            "Give every subject a unique ID across the whole study",
            "Stack all subjects in one long table",
            "Keep subjects with incomplete profiles",
            "Add a Dose column when doses differ"
          ),
          dont_items = c(
            "Put each subject in a separate sheet or file",
            "Use wide format (one column per subject)",
            "Reuse IDs in different groups (two people called '1' become one subject)",
            "Leave out pre-dose samples"
          )
        ),

        tags$h6(class = "fw-semibold", "What is long format?"),
        tags$p(class = "small",
               "One row per sample, with subject ID, time and concentration. ",
               "10 subjects with 12 samples each gives 120 rows."),
        layout_columns(
          col_widths = c(6, 6),
          tags$div(
            tags$p(class = "small text-success fw-bold", icon("check"), " Correct: long format"),
            ex_table(data.frame(Subject = c("S1","S1","S2","S2"), Time = c(0,1,0,1), Conc = c(0,25,0,30)))
          ),
          tags$div(
            tags$p(class = "small text-danger fw-bold", icon("xmark"), " Wrong: wide format"),
            ex_table(data.frame(Time = c(0, 1), S1 = c(0, 25), S2 = c(0, 30)))
          )
        ),
        checklist(c(
          "One row per sample (long format)",
          "Subject IDs unique and written the same way on every row ('S01', not sometimes 'S1')",
          "Same dose for all: no Dose column needed",
          "Different doses: Dose column, mapped during upload",
          "Time in hours since each subject's dose",
          "A time 0 sample for each subject"
        ))
      ),

      # ----------------------------------------------------------------
      # 2x2 crossover
      # ----------------------------------------------------------------
      nav_panel(
        "Crossover (2×2)",
        icon = icon("arrows-left-right"),
        tags$h5(class = "fw-bold mt-2", "Standard 2×2 Crossover"),
        tags$p("Each subject takes both formulations (Test and Reference) in two periods with a ",
               "washout in between. Half the subjects take Test first (sequence TR), half take ",
               "Reference first (RT). This is the usual design for bioequivalence studies."),
        tags$h6(class = "fw-semibold", "What you need"),
        tags$div(
          class = "bg-light rounded p-3 mb-3 small",
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$tr(tags$td(class="fw-bold", "Subject"), tags$td("A unique ID for each person.")),
            tags$tr(tags$td(class="fw-bold", "Treatment"), tags$td("Any two labels, such as 'Test' and 'Reference', 'T' and 'R', or two product names. In the app you choose which one is the Reference; it is pre-selected only for R, Ref, Reference, Comparator, Innovator, Originator or RLD.")),
            tags$tr(tags$td(class="fw-bold", "Period"), tags$td("1 or 2: the first or second dosing occasion.")),
            tags$tr(tags$td(class="fw-bold", "Sequence"), tags$td("'TR' or 'RT': the order in which the subject received the treatments.")),
            tags$tr(tags$td(class="fw-bold", "Time"), tags$td("Hours since the dose of that period. Starts again at 0 in every period.")),
            tags$tr(tags$td(class="fw-bold", "Concentration"), tags$td("The measured concentration."))
          )
        ),
        ex_table(data.frame(
          Subject   = c(rep("S01", 8), rep("S02", 8)),
          Treatment = c(rep("Test", 4), rep("Reference", 4), rep("Reference", 4), rep("Test", 4)),
          Period    = rep(c(1,1,1,1,2,2,2,2), 2),
          Sequence  = c(rep("TR", 8), rep("RT", 8)),
          Time      = rep(c(0,1,4,24), 4),
          Conc      = c(0,24.5,9.1,0.5, 0,22.8,8.7,0.4, 0,20.9,8.1,0.4, 0,23.6,8.8,0.5)
        )),
        tags$p(class = "small text-muted",
               "S01 is in sequence TR (Test in period 1, Reference in period 2); S02 is in RT."),
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("Time starts at 0 in every period. "),
          "If period 2 starts on day 8, its pre-dose sample is time 0, not 192 h. ",
          "The app refuses profiles that do not start near time zero."
        ),

        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("Planning a partial AUC: "),
          "if the protocol asks for a partial AUC, take samples at the cutoff times (for example at 30 min ",
          "for AUC 0\u201330 min). A cutoff between two samples is interpolated, and the result then depends on ",
          "the trapezoidal method. A late interval that ends after the last measurable concentration gets no ",
          "value, because the app does not extrapolate."
        ),

        tags$h6(class = "fw-semibold mt-3", "Why Period and Sequence?"),
        tags$p(class = "small",
               "Period lets the model separate the treatment effect from changes between visits. ",
               "Sequence is part of the standard ANOVA table (sequence effect tested against ",
               "subjects within sequence), so include it. Because every subject belongs to one sequence, ",
               "the ratio and its confidence interval come out the same with or without the Sequence column."),

        tags$h6(class = "fw-semibold mt-3", "Different doses in different periods"),
        tags$p(class = "small",
               "In a dose-proportionality crossover a subject receives, for example, 50 mg in period 1 and ",
               "100 mg in period 2. Add a Dose column; each period is analysed with its own dose (clearance, volume and dose-normalised values)."),
        ex_table(data.frame(
          Subject = rep("S01", 6), Treatment = c(rep("Low", 3), rep("High", 3)),
          Period = c(1,1,1,2,2,2), Sequence = rep("LH", 6), Dose = c(50,50,50,100,100,100),
          Time = rep(c(0,2,24), 2), Conc = c(0,11.2,0.3, 0,21.9,0.7)
        )),

        do_dont(
          do_items = c(
            "Start time at 0 in each period",
            "Keep subjects who dropped out after period 1; the mixed model can use their data",
            "Use (approximately) the same sampling times in both periods",
            "Choose the Reference treatment in the app before running"
          ),
          dont_items = c(
            "Continue time from period 1 into period 2",
            "Measure time from the first dose of the study",
            "Give one person different IDs in the two periods",
            "Put several analytes in one Concentration column (duplicate times are refused)"
          )
        ),
        checklist(c(
          "Columns: Subject, Treatment, Period, Sequence, Time, Concentration",
          "Treatment has exactly two values",
          "Period is 1 or 2 (number or text)",
          "Sequence matches the order of treatments",
          "Time starts at 0 in each period",
          "Dose column if doses differ between subjects or periods"
        ))
      ),

      # ----------------------------------------------------------------
      # Paired comparison (fixed order)
      # ----------------------------------------------------------------
      nav_panel(
        "Paired (Fixed Order)",
        icon = icon("arrow-right"),
        tags$h5(class = "fw-bold mt-2", "Paired Comparison: Everyone in the Same Order"),
        tags$p("All subjects receive the treatments in the same order, without randomisation. ",
               "Typical examples are drug interaction studies (the victim drug alone first, then with ",
               "the interacting drug) and some food-effect studies."),
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("Limitation: "),
          "with one order for everybody, a period effect cannot be separated from the treatment effect. ",
          "The app therefore uses a paired analysis (equivalent to a paired t-test on the log scale) and ",
          "reports the ratio and confidence interval without a bioequivalence verdict."
        ),
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "The same columns as a 2×2 crossover. Sequence has one value for everybody, or can be left out: ",
               "the app also recognises a single order from the Period column."),
        ex_table(data.frame(
          Subject   = c(rep("S01", 8), rep("S02", 8)),
          Treatment = rep(c(rep("Reference", 4), rep("Test", 4)), 2),
          Period    = rep(c(1,1,1,1,2,2,2,2), 2),
          Sequence  = rep("RT", 16),
          Time      = rep(c(0,1,4,24), 4),
          Conc      = c(0,22.8,8.7,0.4, 0,24.5,9.1,0.5, 0,20.1,7.9,0.3, 0,23.2,8.5,0.6)
        )),
        tags$p(class = "small text-muted",
               "Everyone received Reference in period 1 and Test in period 2. In the app select ",
               "'Paired comparison'. If you select a crossover design by mistake, the app detects the single ",
               "order and switches to the paired analysis."),
        checklist(c(
          "Columns: Subject, Treatment, Period (Sequence optional)",
          "The same treatment order for every subject",
          "Treatment has exactly two values",
          "Time starts at 0 in each period",
          "Design in the app: Paired comparison"
        ))
      ),

      # ----------------------------------------------------------------
      # 3-period replicates
      # ----------------------------------------------------------------
      nav_panel(
        "3-Period Replicates",
        icon = icon("rotate"),
        tags$h5(class = "fw-bold mt-2", "3-Period Replicate Designs"),
        tags$div(
          class = "bg-light rounded p-3 mb-3 small",
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$tr(tags$td(class = "fw-bold", "2×2×3 full replicate"),
                    tags$td("Sequences TRT and RTR. Subjects in TRT get Test twice, subjects in RTR get Reference twice.")),
            tags$tr(tags$td(class = "fw-bold", "2×3×3 partial replicate"),
                    tags$td("Sequences TRR, RTR and RRT. Every subject gets Test once and Reference twice."))
          )
        ),
        tags$p(class = "small",
               "The columns are the same as for a 2×2 crossover; Period runs from 1 to 3. ",
               "Each administration is analysed as its own profile. Two samples per period are shown to keep the examples short."),
        tags$h6(class = "fw-semibold mt-3", "Example: 2×2×3 (TRT | RTR)"),
        ex_table(data.frame(
          Subject   = c(rep("S01", 6), rep("S02", 6)),
          Treatment = c("Test","Test","Reference","Reference","Test","Test",
                        "Reference","Reference","Test","Test","Reference","Reference"),
          Period    = rep(c(1,1,2,2,3,3), 2),
          Sequence  = c(rep("TRT", 6), rep("RTR", 6)),
          Time      = rep(c(0, 4), 6),
          Conc      = c(0,8.8, 0,9.5, 0,10.1, 0,9.2, 0,8.6, 0,9.9)
        )),
        tags$h6(class = "fw-semibold mt-3", "Example: 2×3×3 (TRR | RTR | RRT)"),
        ex_table(data.frame(
          Subject   = c(rep("S01", 6), rep("S02", 6), rep("S03", 6)),
          Treatment = c("Test","Test","Reference","Reference","Reference","Reference",
                        "Reference","Reference","Test","Test","Reference","Reference",
                        "Reference","Reference","Reference","Reference","Test","Test"),
          Period    = rep(c(1,1,2,2,3,3), 3),
          Sequence  = c(rep("TRR", 6), rep("RTR", 6), rep("RRT", 6)),
          Time      = rep(c(0, 4), 9),
          Conc      = c(0,9.8, 0,8.5, 0,8.3, 0,9.0, 0,9.4, 0,8.7, 0,8.1, 0,8.9, 0,9.6)
        )),
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("In the app: "),
          "select '2×2×3 full replicate' or '2×3×3 partial replicate'. Besides the ",
          "average bioequivalence result, the app shows the within-subject variability of the Reference ",
          "(and of Test when it was given twice) for information. It does not give a scaled verdict."
        ),
        checklist(c(
          "Columns as for a 2×2 crossover",
          "Period runs from 1 to 3",
          "Sequence matches the order of treatments over all periods",
          "Time starts at 0 in each period"
        ))
      ),

      # ----------------------------------------------------------------
      # 4-period replicate
      # ----------------------------------------------------------------
      nav_panel(
        "4-Period Replicate",
        icon = icon("repeat"),
        tags$h5(class = "fw-bold mt-2", "2×2×4 Full Replicate"),
        tags$p("Often used for highly variable drugs. Each subject receives each formulation twice, ",
               "over four periods, usually in sequences TRTR and RTRT. The design estimates the ",
               "within-subject variability of both products."),
        tags$p(class = "small",
               "The columns are the same as for a 2×2 crossover; Period runs from 1 to 4. ",
               "Two samples per period are shown; a real study has 10 to 15."),
        ex_table(data.frame(
          Subject   = c(rep("S01", 8), rep("S02", 8)),
          Treatment = c("Test","Test","Reference","Reference","Test","Test","Reference","Reference",
                        "Reference","Reference","Test","Test","Reference","Reference","Test","Test"),
          Period    = rep(c(1,1,2,2,3,3,4,4), 2),
          Sequence  = c(rep("TRTR", 8), rep("RTRT", 8)),
          Time      = rep(c(0, 4), 8),
          Conc      = c(0,9.5, 0,8.8, 0,10.1, 0,9.0, 0,8.4, 0,9.7, 0,8.9, 0,10.3)
        )),
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("In the app: "),
          "select '2×2×4 full replicate'. The app reports average bioequivalence and, for ",
          "information, the within-subject variability of Test and Reference with the EMA widened limits ",
          "they would imply. It does not perform a reference-scaled (ABEL or RSABE) analysis. ",
          "Download example_be_replicate_2x2x4.csv above to try it."
        ),
        checklist(c(
          "Columns as for a 2×2 crossover",
          "Period runs from 1 to 4; each treatment appears twice per subject",
          "Sequence matches the order of treatments over all periods",
          "Time starts at 0 in each period"
        ))
      ),

      # ----------------------------------------------------------------
      # Parallel groups
      # ----------------------------------------------------------------
      nav_panel(
        "Parallel Groups",
        icon = icon("arrow-right-arrow-left"),
        tags$h5(class = "fw-bold mt-2", "Parallel-Group Comparison"),
        tags$p("Each subject receives one formulation only. Used when a crossover is impractical: ",
               "very long half-lives, long-acting formulations, or effects that do not wash out."),
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "Four columns: Subject, Treatment, Time and Concentration. No Period or Sequence. ",
               "Subject IDs must be unique across both groups."),
        ex_table(data.frame(
          Subject   = c("S01","S01","S01","S02","S02","S02","S03","S03","S03","S04","S04","S04"),
          Treatment = c(rep("Test", 6), rep("Reference", 6)),
          Time      = rep(c(0,4,24), 4),
          Conc      = c(0,18.3,1.1, 0,21.5,1.3, 0,19.8,0.9, 0,17.6,1.0)
        )),
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("Keep in mind: "),
          "the groups contain different people, so differences between subjects end up in the comparison. ",
          "A parallel study therefore needs more subjects than a crossover for the same power. ",
          "In the app, select 'Parallel groups'."
        ),
        checklist(c(
          "Each subject appears under one treatment only",
          "Treatment column with two values",
          "No Period or Sequence column needed",
          "Unique subject IDs across groups",
          "Dose column if doses differ between subjects"
        ))
      ),

      # ----------------------------------------------------------------
      # Steady state
      # ----------------------------------------------------------------
      nav_panel(
        "Steady State",
        icon = icon("rotate"),
        tags$h5(class = "fw-bold mt-2", "Multiple Dosing at Steady State"),
        tags$p("The drug has been given repeatedly (for example once daily for 7 days) and you sample ",
               "over one dosing interval at steady state. The pre-dose concentration is above zero ",
               "because the drug has accumulated."),
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "The same columns as for several subjects. Time 0 is just before the dose of the sampled ",
               "interval, not the first dose of the treatment. Include the pre-dose sample at time 0 and ",
               "sample up to the end of the dosing interval (\u03C4). In the app you enter \u03C4; AUC\u03C4 is ",
               "calculated from 0 to \u03C4, and extrapolated when the last sample is missing (for example BLQ). ",
               "Partial AUC intervals must lie within 0 to \u03C4."),
        ex_table(data.frame(
          Subject = c("S01","S01","S01","S01","S01","S02","S02","S02","S02","S02"),
          Time    = c(0, 0.5, 1, 4, 12, 0, 0.5, 1, 4, 12),
          Conc    = c(3.2, 28.1, 22.4, 9.8, 4.1, 2.8, 25.6, 20.1, 8.5, 3.5)
        )),
        tags$p(class = "small text-muted",
               "A twice-daily drug: sampling from 0 to 12 h. The pre-dose values (3.2 and 2.8) are left over from earlier doses."),
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("In the app: "),
          "tick 'Steady-state (drug given repeatedly)' and enter the dosing interval \u03C4. The app reports ",
          "AUC\u03C4, average concentration, trough, fluctuation and swing, and calculates CL/F from AUC\u03C4; ",
          "AUC to infinity has no meaning during repeated dosing."
        ),
        checklist(c(
          "Time 0 is just before the dose of the sampled interval",
          "Pre-dose sample present (above zero is normal)",
          "Sampling covers one dosing interval (0 to 12 h for twice daily, 0 to 24 h for once daily)",
          "'Steady-state' ticked and the dosing interval entered in the app"
        ))
      ),

      # ----------------------------------------------------------------
      # Drug interaction study
      # ----------------------------------------------------------------
      nav_panel(
        "Drug Interaction",
        icon = icon("pills"),
        tags$h5(class = "fw-bold mt-2", "Drug Interaction (DDI) Study"),
        tags$p("A DDI study measures how one drug (the perpetrator, or precipitant) changes the exposure of ",
               "another (the victim, or substrate). Subjects receive the victim drug alone and together with ",
               "the perpetrator, in a randomised crossover or in a fixed order."),
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "The layout of a 2×2 crossover, or of a paired comparison when everyone had the same order. ",
               "Concentrations are those of the victim drug."),
        ex_table(data.frame(
          Subject   = rep("S01", 8),
          Treatment = c(rep("Alone", 4), rep("With inhibitor", 4)),
          Period    = c(1,1,1,1,2,2,2,2),
          Time      = rep(c(0,1,4,24), 2),
          Conc      = c(0,12.4,6.1,0.4, 0,19.8,13.5,2.2)
        )),
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("In the app: "),
          "in Bioequivalence, choose the victim drug alone ('Alone') as the Reference treatment. ",
          "The ratio is then exposure with the perpetrator divided by exposure alone: 200% means the ",
          "exposure doubled. Select 'Paired comparison' for a fixed-order study."
        ),
        tags$h6(class = "fw-semibold mt-3", "Interpreting the result"),
        tags$p(class = "small",
               "ICH M12 (section 5.2) asks for the geometric mean ratio with its 90% confidence interval, for ",
               "AUC and Cmax. A 90% interval within 80–125% is an accepted default no-effect boundary, although ",
               "M12 calls it overly conservative for most drugs; boundaries based on exposure-response are preferred. ",
               "The labels strong, moderate and weak inhibitor or inducer describe the precipitant's effect on a ",
               "sensitive index substrate; they do not classify the interaction for any other victim drug."),
        checklist(c(
          "Crossover or paired layout, victim drug concentrations",
          "The victim drug alone chosen as Reference in the app",
          "Paired comparison selected for a fixed-order study"
        ))
      ),

      # ----------------------------------------------------------------
      # CDISC ADNCA
      # ----------------------------------------------------------------
      nav_panel(
        "CDISC ADNCA",
        icon = icon("database"),
        tags$h5(class = "fw-bold mt-2", "CDISC ADNCA Datasets"),
        tags$p("Pharmaceutical companies and CROs often deliver PK data as a CDISC ADNCA dataset. ",
               "You recognise it by variables such as USUBJID, AVAL, PARAMCD, NRRLT, ARRLT and ANL01FL. ",
               "Do not convert it by hand."),
        tags$h6(class = "fw-semibold", "How to upload"),
        tags$ol(class = "small",
          tags$li("On Upload & Check Data, set 'What kind of file?' to 'CDISC ADNCA dataset'."),
          tags$li("Upload the .csv or .xlsx file. The app shows the analytes, time variables, units, LLOQ and record counts."),
          tags$li("Choose the time: NRRLT (nominal), ARRLT (actual; pre-dose samples have negative times, which you can set to 0) or MRRLT (actual, pre-dose at 0). AFRLT, time since the first dose, is refused."),
          tags$li("Choose the analyte if there is more than one, then process the data. The choices are stored in the Analysis Record.")
        ),
        tags$h6(class = "fw-semibold", "What the app does with it"),
        tags$ul(class = "small",
          tags$li("Keeps records with ANL01FL = 'Y' and drops PCSTAT = 'NOT DONE'."),
          tags$li("Refuses derived records (DTYPE filled in), more than one unit or LLOQ, more than one analyte or matrix without a choice, duplicate times within a profile, and AVAL missing without a BLQ result."),
          tags$li("Passes BLQ results (for example '<0.5' in PCORRES) to the BLQ rule you choose.")
        ),
        tags$p(class = "small",
               "SAS transport (.xpt) files are not read. Convert them to CSV first; ",
               code("converters/ADNCA_TO_FLAT.md"), " in the repository gives a two-line R recipe. ",
               "The same conversion is available outside the app as ", code("converters/adnca_to_flat.R"), ". ",
               "Download example_adnca.csv above to try it."),
        tags$p(class = "small text-muted",
               "Variable names follow common ADNCA usage; the app has not been checked against a specific ",
               "version of the ADNCA Implementation Guide and is not affiliated with CDISC.")
      ),

      # ----------------------------------------------------------------
      # Real lab data
      # ----------------------------------------------------------------
      nav_panel(
        "Real Lab Data",
        icon = icon("flask-vial"),
        tags$h5(class = "fw-bold mt-2", "Working with Real Bioanalytical Data"),
        tags$p("Laboratory data rarely arrive in exactly the layout the app needs. These are the usual issues."),

        tags$h6(class = "fw-semibold mt-3", "Below the limit of quantification (BLQ)"),
        tags$p(class = "small",
               "Every assay has a lower limit of quantification (LLOQ): the lowest concentration it measures ",
               "reliably. Results below it are reported as '<0.5', 'BLQ', 'BQL', 'BLOQ', 'ND' (not detected) ",
               "or 'NQ' (not quantifiable). Leave them as text. Once you set the LLOQ, the app recognises them ",
               "in any capitals and applies the BLQ rule you choose. 'NS' (no sample), 'N/A' and 'missing' ",
               "count as missing values, not as BLQ."),
        tags$p(class = "small",
               "In the processed data the app adds a ", code("BLQ_flag"), " column that marks every value the ",
               "rule set. You see it in the data preview, and the partial AUC notes and the bioequivalence table ",
               "use it to say how much of a result rests on such values. A column of that name in your file is ",
               "replaced."),
        tags$p(class = "small",
               "The default rule (Rule 1) sets BLQ samples before the first measurable concentration to 0, ",
               "ignores BLQ samples after the last measurable concentration, and sets BLQ samples between ",
               "measurable ones to 0. The rules work per profile, in time order."),
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("Set the LLOQ when the data contain BLQ text. "),
          "Use the value from the bioanalytical validation report (for example 0.5 ng/mL). With the LLOQ ",
          "left at 0 the app stops with an error when it finds BLQ text. If the entries look like '<0.5' ",
          "(or '<0,5'), it then shows the value it found and a button 'Set LLOQ to 0.5'. The button only ",
          "fills in the field: check the value, then click Process Data again."
        ),
        ex_table(data.frame(
          Subject = rep("S01", 8),
          Time = c(0, 0.25, 0.5, 1, 2, 8, 12, 24),
          Conc = c("BLQ", "BLQ", "4.2", "18.5", "12.1", "2.8", "0.9", "<0.5")
        )),

        tags$h6(class = "fw-semibold mt-4", "Units"),
        tags$p(class = "small",
               "Use one concentration unit, one time unit and one dose unit for the whole file, and select ",
               "them in the app: they set the conversion factor for clearance and volume. A unit column ",
               "with more than one unit (even ng/mL and µg/L) is refused. Molar concentration units ",
               "(nmol/L) with a mass dose (mg) need the molecular weight."),

        tags$h6(class = "fw-semibold mt-4", "Missing samples"),
        tags$p(class = "small",
               "A lost sample (haemolysed, broken tube) can be left out, left blank, or marked 'NS'. ",
               "Do not enter 0: that says the concentration was measured and was zero."),

        tags$h6(class = "fw-semibold mt-4", "What the app refuses"),
        tags$ul(class = "small",
          tags$li("Dates or clock times in the Time column."),
          tags$li("Profiles that do not start near time 0, such as period 2 starting at 168 h (time since the first dose)."),
          tags$li("Duplicate times within a profile: usually several analytes, matrices or periods stacked in one column."),
          tags$li("A unit column with more than one unit."),
          tags$li("A CDISC-style file uploaded as a simple table.")
        ),

        do_dont(
          do_items = c(
            "Leave BLQ entries as text ('BLQ', '<0.5', 'ND')",
            "Use actual sampling times for the NCA",
            "Leave missing samples blank, write NS, or leave the row out",
            "Keep all subjects, also with incomplete data"
          ),
          dont_items = c(
            "Replace BLQ with 0 yourself",
            "Enter 0 for a missing sample",
            "Mix units within the file",
            "Remove subjects with a few missing samples"
          )
        ),

        tags$h6(class = "fw-semibold mt-4", "Data quality messages you may see"),
        tags$div(
          class = "small",
          tags$p(tags$strong("Large sampling gap.")),
          tags$p(class = "text-muted",
                 "Two consecutive samples in a profile are far apart. The threshold is 24 h, or 48 h when the ",
                 "profile lasts longer than 48 h, so daily trough samples are not flagged. A gap just above ",
                 "24 h is usually normal timing; a much larger gap often means a missed sample."),
          tags$p(tags$strong("BLQ entries detected but LLOQ is not set.")),
          tags$p(class = "text-muted",
                 "Set the LLOQ and click Process Data again. When the app found '<X' entries, the button ",
                 "'Set LLOQ to X' below the LLOQ field fills in the value for you."),
          tags$p(tags$strong("Tmax = 0 in one or more subjects.")),
          tags$p(class = "text-muted",
                 "The highest concentration is at time 0. After an oral dose this usually means a mislabelled ",
                 "pre-dose sample. Check the raw data."),
          tags$p(tags$strong("Subjects with < 3 observations.")),
          tags$p(class = "text-muted",
                 "The half-life needs at least 3 points after the peak. A profile with 2 positive concentrations ",
                 "still gives Cmax, Tmax and AUClast; a profile with fewer than 2 is left out of the analysis."),
          tags$p(tags$strong("Unequal observations per subject.")),
          tags$p(class = "text-muted",
                 "For information only. Missing samples are common and the app handles them.")
        ),

        tags$h6(class = "fw-semibold mt-4", "Column names the app recognises"),
        tags$p(class = "small",
               "The app suggests a mapping from common names (not case-sensitive). Always check it: when no name ",
               "matches, it falls back to a column by position."),
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm small",
            style = "max-width: 650px;",
            tags$thead(class = "table-light",
              tags$tr(tags$th("Meaning"), tags$th("Recognised names"))
            ),
            tags$tbody(
              tags$tr(tags$td("Subject"), tags$td("Subject, SubjectID, USUBJID, ID, Pat, PatID, Proband, Teilnehmer")),
              tags$tr(tags$td("Time"), tags$td("Time, Hours, Hour, Tpt, NTIM, APTS, Zeit, Tid")),
              tags$tr(tags$td("Concentration"), tags$td("Conc, Concentration, DV, Cp, Result, Konz, Plasma, or a name containing ng/mL or ug/L")),
              tags$tr(tags$td("Treatment"), tags$td("Treatment, Trt, Formulation, Drug, Arm, Behandlung")),
              tags$tr(tags$td("Period"), tags$td("Period, Per, Prd, Phase")),
              tags$tr(tags$td("Sequence"), tags$td("Sequence, Seq, Grp")),
              tags$tr(tags$td("Dose"), tags$td("Dose, Amt, Amount, Dosis"))
            )
          )
        )
      ),

      # ----------------------------------------------------------------
      # Common mistakes
      # ----------------------------------------------------------------
      nav_panel(
        "Common Mistakes",
        icon = icon("triangle-exclamation"),
        tags$h5(class = "fw-bold mt-2", "Common Data Preparation Mistakes"),
        tags$p("Check your file against this list before uploading."),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "1. Clock time instead of hours since the dose"),
          tags$p(class = "small",
                 "A lab report lists '08:30', '09:30'. The app needs hours since the dose: with the dose at ",
                 "08:30, 09:30 is 1.0 h. The app refuses clock times.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "2. Wide format"),
          tags$p(class = "small",
                 "One column per subject does not work. Use one row per sample; see the Multiple Subjects tab.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "3. Time not restarted in each period"),
          tags$p(class = "small",
                 "In a crossover, the pre-dose sample of period 2 is time 0, not 168 h. The app refuses profiles ",
                 "that do not start near zero.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "4. BLQ replaced by 0 before uploading"),
          tags$p(class = "small",
                 "The BLQ rules treat a BLQ sample according to where it sits in the profile: before the first ",
                 "or after the last measurable concentration (Rules 1, 4 and 6), or before or after Cmax (Rule 5). ",
                 "Once you have typed 0, the app cannot tell a BLQ result from a real zero. Leave BLQ as text.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "5. Several analytes or matrices in one column"),
          tags$p(class = "small",
                 "Parent drug and metabolite, or plasma and urine, stacked in one Concentration column give ",
                 "duplicate times per profile. The app refuses this. Make one file per analyte and matrix.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "6. Subject IDs that are not unique or not consistent"),
          tags$p(class = "small",
                 "'S01' in period 1 and 'S1' in period 2 become two people. Numbering that restarts in each group ",
                 "or sequence turns two people into one subject. Use one ID per person for the whole study; the ",
                 "app refuses an ID that appears in two sequences or with two treatments in one period.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "7. 0 entered for a missing sample"),
          tags$p(class = "small",
                 "Zero means the sample was measured and nothing was found. For a missing sample leave the cell ",
                 "blank, write NS, or leave the row out.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "8. Mixed or wrong units"),
          tags$p(class = "small",
                 "All concentrations need the same unit, and the units selected in the app must match the file: ",
                 "they determine the conversion factor for clearance and volume. Units stated in the file (a ",
                 "ConcUnit column, or AVALU in ADNCA) are pre-selected, and a different selection is refused.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "9. A partial AUC cutoff with no sample near it"),
          tags$p(class = "small",
                 "The protocol asks for AUC 0\u201330 min, but samples were taken at 15 and 60 min. The app ",
                 "interpolates the concentration at 30 min, so the value depends on the trapezoidal method and can ",
                 "differ from what a sample at 30 min would have shown. Plan samples at the cutoff times.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "10. A crossover without a Period column"),
          tags$p(class = "small",
                 "A column named Visit or Occasion is not recognised as the period, so map it on the Upload ",
                 "page. Without a Period column a crossover gets no bioequivalence verdict.")
        ),
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "11. Excluding a subject by editing the file"),
          tags$p(class = "small",
                 "The app has no control to exclude a subject or period, so an exclusion means editing the file, ",
                 "and the Analysis Record then holds only the edited file. Archive the unedited source file with ",
                 "the reason for each exclusion, as ICH M13A asks.")
        )
      )
    ),

    # Ready to go
    tags$div(
      class = "text-center mt-4 mb-3",
      tags$p(class = "text-muted",
             "Ready to upload? Go to ",
             tags$a(href = "#",
                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'}); return false;",
                    tags$strong("Upload & Check Data")),
             ". The app checks your data and flags remaining issues."
      )
    )
  )
}
