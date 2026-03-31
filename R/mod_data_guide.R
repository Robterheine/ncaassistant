# ============================================================================
# NCA Assistant — Data Preparation Guide
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
             "How to prepare your concentration-time data for analysis. ",
             "Find your study type below and follow the example.")
    ),
    
    # Quick reference card
    card(
      card_header(icon("table-list"), " Quick Reference: Which Columns Do I Need?"),
      card_body(
        tags$table(
          class = "table table-sm",
          style = "font-size: 0.85rem;",
          tags$thead(class = "table-light",
            tags$tr(tags$th("Study type"),
                    tags$th("Required columns"),
                    tags$th("App path"))
          ),
          tags$tbody(
            tags$tr(tags$td("Single subject, single dose"),
                    tags$td(tags$code("Time"), ", ", tags$code("Concentration")),
                    tags$td("One Subject at a Time (manual entry)")),
            tags$tr(tags$td("Multiple subjects, same dose"),
                    tags$td(tags$code("Subject"), ", ", tags$code("Time"), ", ", tags$code("Concentration")),
                    tags$td("All Subjects (Batch)")),
            tags$tr(tags$td("Multiple subjects, different doses"),
                    tags$td(tags$code("Subject"), ", ", tags$code("Time"), ", ", tags$code("Concentration"), ", ", tags$code("Dose")),
                    tags$td("All Subjects (select 'from Dose column')")),
            tags$tr(tags$td("2-period crossover (BE)"),
                    tags$td(tags$code("Subject"), ", ", tags$code("Period"), ", ", tags$code("Sequence"), ", ", tags$code("Treatment"), ", ", tags$code("Time"), ", ", tags$code("Concentration")),
                    tags$td("Bioequivalence")),
            tags$tr(tags$td("Fixed-order crossover (BE)"),
                    tags$td(tags$code("Subject"), ", ", tags$code("Period"), ", ", tags$code("Sequence"), ", ", tags$code("Treatment"), ", ", tags$code("Time"), ", ", tags$code("Concentration")),
                    tags$td("Bioequivalence (fixed-order)")),
            tags$tr(tags$td("3-period crossover (BE)"),
                    tags$td(tags$code("Subject"), ", ", tags$code("Period"), ", ", tags$code("Sequence"), ", ", tags$code("Treatment"), ", ", tags$code("Time"), ", ", tags$code("Concentration")),
                    tags$td("Bioequivalence")),
            tags$tr(tags$td("Replicate crossover (BE)"),
                    tags$td("Same as above"),
                    tags$td("Bioequivalence")),
            tags$tr(tags$td("Parallel-group BE"),
                    tags$td(tags$code("Subject"), ", ", tags$code("Treatment"), ", ", tags$code("Time"), ", ", tags$code("Concentration")),
                    tags$td("Bioequivalence")),
            tags$tr(tags$td("Multiple-dose / steady-state"),
                    tags$td(tags$code("Subject"), ", ", tags$code("Time"), ", ", tags$code("Concentration")),
                    tags$td("All Subjects (tick 'steady-state')"))
          )
        ),
        tags$p(class = "text-muted small mt-2",
               tags$strong("Note on the Dose column: "),
               "If all subjects received the same dose, you don't need a Dose column — ",
               "just enter the dose in the app settings. If subjects received different doses ",
               "(dose escalation, weight-based dosing), include a Dose column in your data and ",
               "select 'Each subject has a different dose' in the analysis settings.")
      )
    ),
    
    # ====================================================================
    # SCENARIO TABS
    # ====================================================================
    navset_card_pill(
      title = "Choose Your Study Type",
      
      # ----------------------------------------------------------------
      # SCENARIO 1: Single subject
      # ----------------------------------------------------------------
      nav_panel(
        "Single Subject",
        icon = icon("user"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 1: One Subject, One Dose"),
        tags$p("You gave a drug to one person (or animal) and collected blood samples over time. ",
               "This is the simplest case. You can either type the data directly into the app ",
               "or prepare a small file."),
        
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "Two columns: the time each sample was drawn (in hours after the dose) and the ",
               "measured drug concentration in that sample."),
        
        ex_table(data.frame(
          Time  = c(0, 0.25, 0.5, 1, 2, 4, 8, 12, 24),
          Concentration = c(0, 4.2, 12.8, 25.6, 18.3, 9.1, 3.4, 1.2, 0.15)
        )),
        
        tags$p(class = "small text-muted",
               "If you upload a file instead of using manual entry, add a Subject column ",
               "(it can be the same value for every row, like 'Patient1')."),
        
        do_dont(
          do_items = c(
            "Express time in hours since the dose was given",
            "Include a pre-dose sample at time 0 (concentration should be 0 or near 0)",
            "Include enough points in the terminal phase (the tail end, where concentrations decline) — at least 3 points",
            "Use a period (.) as decimal separator"
          ),
          dont_items = c(
            "Use clock time ('08:30', '09:00') — convert to hours since dosing",
            "Leave out the pre-dose sample",
            "Mix units (some concentrations in ng/mL, others in \u00B5g/L)",
            "Use commas as decimal separators (use periods)"
          )
        ),
        
        checklist(c(
          "Time is in hours since the dose (not clock time)",
          "Concentration units are consistent throughout",
          "There is a time = 0 observation",
          "At least 3 declining points after the peak for half-life estimation"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 2: Multiple subjects
      # ----------------------------------------------------------------
      nav_panel(
        "Multiple Subjects",
        icon = icon("users"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 2: Multiple Subjects"),
        tags$p("Several subjects each received the drug and blood samples were collected ",
               "at protocol-specified times. This covers both single-dose PK studies ",
               "and dose-escalation studies."),
        
        # --- Same dose ---
        tags$h6(class = "fw-semibold mt-3",
                "Case A: Everyone received the same dose"),
        tags$p(class = "small",
               "Three columns: subject ID, time, and concentration. ",
               "You enter the dose once in the app settings — no Dose column needed."),
        
        ex_table(data.frame(
          Subject = c("S001","S001","S001","S001","S002","S002","S002","S002","S003","S003","S003","S003"),
          Time = c(0,1,4,24, 0,1,4,24, 0,1,4,24),
          Concentration = c(0,22.5,8.3,0.4, 0,28.1,10.2,0.6, 0,19.8,7.1,0.3)
        )),
        
        # --- Different doses ---
        tags$h6(class = "fw-semibold mt-4",
                "Case B: Subjects received different doses"),
        tags$p(class = "small",
               "This happens in dose-escalation studies (Phase I: cohort 1 gets 50 mg, ",
               "cohort 2 gets 100 mg, etc.) and with weight-based dosing (each subject ",
               "gets mg/kg \u00D7 their weight). Add a ",
               tags$strong("Dose"), " column with each subject's actual dose."),
        
        ex_table(data.frame(
          Subject = c("S001","S001","S001","S002","S002","S002","S003","S003","S003"),
          Dose    = c(50,50,50, 50,50,50, 100,100,100),
          Time    = c(0,1,24, 0,1,24, 0,1,24),
          Concentration = c(0,10.2,0.2, 0,12.8,0.3, 0,25.1,0.5)
        )),
        
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("How it works in the app: "),
          "During data upload, map the Dose column. In the analysis settings, select ",
          "'Each subject has a different dose (from Dose column in data)'. ",
          "The app reads each subject's dose from the data and uses it to calculate ",
          "dose-dependent parameters like clearance (CL = Dose / AUC) and volume of distribution."
        ),
        
        tags$p(class = "small text-muted",
               "The Dose column should contain a single value per subject (the same value ",
               "on every row for that subject). If doses vary within a subject's rows, ",
               "the app uses the maximum value."),
        
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("Tip: "),
          "Time points don't need to be identical across subjects. If one subject was ",
          "sampled at 0.48h instead of the protocol's 0.5h, use the actual time (0.48). ",
          "The app handles unequal time points automatically."
        ),
        
        do_dont(
          do_items = c(
            "Use a unique ID for each subject (S001, S002, ... or 1, 2, 3, ...)",
            "Stack all subjects into one long table, not separate sheets",
            "Use actual sampling times if available (not just nominal protocol times)",
            "Include all subjects, even those with incomplete profiles",
            "Include a Dose column if subjects received different doses"
          ),
          dont_items = c(
            "Put each subject in a separate sheet or file",
            "Use wide format (subjects as columns) — use long format (subjects as rows)",
            "Remove subjects with missing data points — let the app handle them",
            "Exclude pre-dose samples",
            "Leave out the Dose column for dose-escalation studies — the app can't guess doses"
          )
        ),
        
        tags$h6(class = "fw-semibold", "What is long format?"),
        tags$p(class = "small",
               "Your data should have one row per observation (one sample = one row). ",
               "Each row has the subject ID, the time, and the concentration. ",
               "If you have 10 subjects with 12 samples each, you should have 120 rows."),
        
        layout_columns(
          col_widths = c(6, 6),
          tags$div(
            tags$p(class = "small text-success fw-bold", icon("check"), " Correct: long format"),
            ex_table(data.frame(
              Subject = c("S1","S1","S2","S2"),
              Time = c(0,1,0,1),
              Conc = c(0,25,0,30)
            ))
          ),
          tags$div(
            tags$p(class = "small text-danger fw-bold", icon("xmark"), " Wrong: wide format"),
            ex_table(data.frame(
              Time = c(0, 1),
              S1 = c(0, 25),
              S2 = c(0, 30)
            ))
          )
        ),
        
        checklist(c(
          "One row per observation (long format)",
          "Subject IDs are unique and consistent (no 'S01' vs 'S1' mix-up)",
          "If same dose for all: enter the dose in the app (no Dose column needed)",
          "If different doses: include a Dose column and map it during upload",
          "Time is in hours since the individual subject's dose",
          "Pre-dose sample (time 0) is present for each subject"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 3: 2-period crossover BE
      # ----------------------------------------------------------------
      nav_panel(
        "Crossover BE (2-period)",
        icon = icon("arrows-left-right"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 3: Standard 2-Period Crossover (BE)"),
        tags$p("The classic bioequivalence design: each subject takes two formulations ",
               "(Test and Reference) in two separate periods, with a washout between them. ",
               "Half the subjects take Test first (TR sequence), half take Reference first (RT sequence). ",
               "This is the most common design for generic drug approval."),
        
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "Six columns. The extra three (compared to a simple PK study) tell the statistical ",
               "model which treatment each observation belongs to, and which period and sequence."),
        
        tags$div(
          class = "bg-light rounded p-3 mb-3 small",
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$tr(tags$td(class="fw-bold", "Subject"), tags$td("A unique ID for each person.")),
            tags$tr(tags$td(class="fw-bold", "Treatment"), tags$td("'Test' or 'Reference'. These exact words are recommended, but any two labels work.")),
            tags$tr(tags$td(class="fw-bold", "Period"), tags$td("1 or 2. The first dosing occasion is period 1, the second (after washout) is period 2.")),
            tags$tr(tags$td(class="fw-bold", "Sequence"), tags$td("'TR' or 'RT'. Tells the model the order in which each subject received the treatments.")),
            tags$tr(tags$td(class="fw-bold", "Time"), tags$td("Hours since dosing in that period. Resets to 0 at the start of each period.")),
            tags$tr(tags$td(class="fw-bold", "Concentration"), tags$td("The measured drug concentration."))
          )
        ),
        
        ex_table(data.frame(
          Subject   = c("S01","S01","S01","S01","S01","S01","S01","S01"),
          Treatment = c("Test","Test","Test","Test","Reference","Reference","Reference","Reference"),
          Period    = c(1,1,1,1,2,2,2,2),
          Sequence  = c("TR","TR","TR","TR","TR","TR","TR","TR"),
          Time      = c(0,1,4,24, 0,1,4,24),
          Conc      = c(0,24.5,9.1,0.5, 0,22.8,8.7,0.4)
        )),
        
        tags$p(class = "small text-muted",
               "Subject S01 is in the TR sequence: they received Test in period 1, ",
               "Reference in period 2. A subject in the RT sequence would have ",
               "Reference in period 1 and Test in period 2."),
        
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("Critical: "), "Time resets to 0 at the start of each period. ",
          "If period 2 starts on day 8, the first sample of period 2 is still time = 0, ",
          "not time = 192 (8 \u00D7 24)."
        ),
        
        tags$h6(class = "fw-semibold mt-3", "Why do I need Sequence and Period?"),
        tags$p(class = "small",
               "The statistical model for bioequivalence must separate the treatment effect ",
               "(the thing you're testing) from the period effect (did something change between visits?) ",
               "and the sequence effect (is there carryover from the first treatment?). ",
               "Without these columns, the model can't do this separation, and the result is unreliable."),
        
        do_dont(
          do_items = c(
            "Use 'Test' and 'Reference' (or 'T' and 'R') as treatment labels",
            "Reset time to 0 at the start of each period",
            "Include all subjects, even if they dropped out after period 1",
            "Use the same time points (approximately) in both periods"
          ),
          dont_items = c(
            "Use brand names as treatment labels (use 'Test' / 'Reference')",
            "Continue time from period 1 into period 2 without resetting",
            "Forget the Sequence column (the ANOVA model needs it)",
            "Exclude dropouts — include their available data"
          )
        ),
        
        checklist(c(
          "Six columns present: Subject, Treatment, Period, Sequence, Time, Concentration",
          "Treatment has exactly two levels (Test and Reference)",
          "Period is 1 or 2 (numeric or text, both work)",
          "Sequence matches the treatment order (TR means Test-first, RT means Reference-first)",
          "Time resets to 0 at the start of each period",
          "Roughly equal numbers of subjects in each sequence group",
          "Every subject appears in both periods (or is flagged as a dropout)",
          "If doses differ between subjects (e.g., weight-based dosing), add a Dose column"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 3b: Fixed-order crossover
      # ----------------------------------------------------------------
      nav_panel(
        "Fixed-Order Crossover",
        icon = icon("arrow-right"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 3b: Fixed-Order Crossover"),
        tags$p("All subjects receive the treatments in the same fixed order ",
               "(e.g., everyone gets Reference first, then Test after washout). ",
               "This is used in some relative bioavailability studies where randomisation ",
               "is not possible, for example when the test formulation was developed after ",
               "the reference study was completed."),
        
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("Statistical limitation: "),
          "Because all subjects follow the same order, the period effect ",
          "(things that change between visits) cannot be separated from the treatment effect. ",
          "The app uses a paired analysis (equivalent to a paired t-test). ",
          "Results should be interpreted with this limitation in mind."
        ),
        
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "The same six columns as a standard crossover. The only difference is that the ",
               "Sequence column has a single value for all subjects (e.g., 'RT' if everyone ",
               "received Reference first and Test second)."),
        
        ex_table(data.frame(
          Subject   = c("S01","S01","S01","S01","S01","S01","S01","S01",
                         "S02","S02","S02","S02","S02","S02","S02","S02"),
          Treatment = c("Reference","Reference","Reference","Reference",
                         "Test","Test","Test","Test",
                         "Reference","Reference","Reference","Reference",
                         "Test","Test","Test","Test"),
          Period    = c(1,1,1,1,2,2,2,2, 1,1,1,1,2,2,2,2),
          Sequence  = rep("RT", 16),
          Time      = rep(c(0,1,4,24), 4),
          Conc      = c(0,22.8,8.7,0.4, 0,24.5,9.1,0.5,
                         0,20.1,7.9,0.3, 0,23.2,8.5,0.6)
        )),
        
        tags$p(class = "small text-muted",
               "Notice: all subjects have Sequence = 'RT'. Everyone received Reference in ",
               "period 1 and Test in period 2. In the app, select ",
               "'Fixed-order crossover (all subjects same sequence)' as the study design."),
        
        tags$h6(class = "fw-semibold mt-3", "Do I still need the Sequence column?"),
        tags$p(class = "small",
               "Yes. Include it even though it has only one value. The app uses it to detect ",
               "that this is a fixed-order design and applies the correct paired analysis ",
               "instead of the standard crossover ANOVA."),
        
        checklist(c(
          "Six columns present: Subject, Treatment, Period, Sequence, Time, Concentration",
          "Sequence has ONE value for all subjects (e.g., 'RT')",
          "Treatment has exactly two levels (Test and Reference)",
          "Time resets to 0 at the start of each period",
          "In the app, select 'Fixed-order crossover' as the study design",
          "If doses differ between subjects, add a Dose column"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 3c: 3-period crossover
      # ----------------------------------------------------------------
      nav_panel(
        "3-Period Crossover",
        icon = icon("rotate"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 3c: 3-Period Crossover"),
        tags$p("Each subject receives treatments across three periods. Two common variants exist:"),
        
        tags$div(
          class = "bg-light rounded p-3 mb-3 small",
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$tr(tags$td(class = "fw-bold", "2-sequence, 3-period"),
                    tags$td("Sequences TRT and RTR. One treatment is given twice, ",
                            "the other once. Provides extra within-subject replication.")),
            tags$tr(tags$td(class = "fw-bold", "Williams design (3\u00D73\u00D73)"),
                    tags$td("Sequences TRR, RTR, RRT (three sequences, three periods). ",
                            "Both treatments appear at least once per subject. ",
                            "Used for partial replicate designs."))
          )
        ),
        
        tags$h6(class = "fw-semibold", "Same six columns as the 2-period design"),
        tags$p(class = "small",
               "The data format is identical to Scenario 3, except Period goes up to 3."),
        
        tags$h6(class = "fw-semibold mt-3", "Example: TRT|RTR design"),
        ex_table(data.frame(
          Subject   = rep("S01", 6),
          Treatment = c("Test","Ref","Test", "Test","Ref","Test"),
          Period    = c(1,2,3, 1,2,3),
          Sequence  = rep("TRT", 6),
          Time      = rep(c(0, 4), 3),
          Conc      = c(0,9.5, 0,8.8, 0,10.1)
        )),
        
        tags$p(class = "small text-muted",
               "Subject S01 is in the TRT sequence: Test in period 1, Reference in period 2, ",
               "Test again in period 3. Shown with 2 time points per period for brevity."),
        
        tags$h6(class = "fw-semibold mt-3", "Example: Williams design (TRR|RTR|RRT)"),
        ex_table(data.frame(
          Subject   = c(rep("S01", 6), rep("S04", 6)),
          Treatment = c("Test","Ref","Ref", "Test","Ref","Ref",
                         "Ref","Test","Ref", "Ref","Test","Ref"),
          Period    = rep(c(1,2,3), 4),
          Sequence  = c(rep("TRR", 6), rep("RTR", 6)),
          Time      = rep(c(0, 4), 6),
          Conc      = c(0,9.5, 0,8.8, 0,9.0,
                         0,8.5, 0,9.8, 0,8.3)
        )),
        
        tags$p(class = "small text-muted",
               "S01 is in sequence TRR, S04 in RTR. A third group would have sequence RRT."),
        
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("In the app: "),
          "Select '3-period crossover' as the study design in the Bioequivalence settings."
        ),
        
        checklist(c(
          "Six columns: Subject, Treatment, Period, Sequence, Time, Concentration",
          "Period goes up to 3",
          "Sequence accurately reflects the treatment order across all 3 periods",
          "Time resets to 0 at the start of each period",
          "If doses differ between subjects, add a Dose column"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 4: Replicate crossover
      # ----------------------------------------------------------------
      nav_panel(
        "Replicate Crossover",
        icon = icon("repeat"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 4: Replicate Design (4-Period Crossover)"),
        tags$p("Used for highly variable drugs. Each subject receives each formulation twice, in four periods. ",
               "Common sequences are TRTR and RTRT (2-sequence), or more complex Williams designs with ",
               "3 or 4 sequences. This design lets you estimate within-subject variability for the ",
               "Reference product, which is needed for widened acceptance limits."),
        
        tags$h6(class = "fw-semibold", "Same six columns as the 2-period design"),
        tags$p(class = "small",
               "The data format is identical to the 2-period crossover, except Period goes up to 4 ",
               "and each treatment appears twice per subject."),
        
        ex_table(data.frame(
          Subject   = rep("S01", 8),
          Treatment = c("Test","Ref","Test","Ref", "Test","Ref","Test","Ref"),
          Period    = c(1,1,2,2,3,3,4,4),
          Sequence  = rep("TRTR", 8),
          Time      = rep(c(0, 4), 4),
          Conc      = c(0,9.5, 0,8.8, 0,10.1, 0,9.0)
        )),
        
        tags$p(class = "small text-muted",
               "Shown with only 2 time points per period for brevity. ",
               "In practice, you would have 10-15 time points per period."),
        
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("Note: "),
          "If your study has 3 periods (e.g., Williams design with sequences TRR, RTR, RRT), ",
          "the same format applies — just adjust the Period values (1, 2, 3) and the Sequence accordingly. ",
          "In these designs, one treatment is given once and the other twice."
        ),
        
        checklist(c(
          "Same six columns as 2-period design",
          "Period goes up to 3 or 4 (depending on the design)",
          "Each treatment appears at least twice per subject (for 4-period designs)",
          "Sequence accurately reflects the order across all periods",
          "If doses differ between subjects, add a Dose column"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 5: Parallel-group BE
      # ----------------------------------------------------------------
      nav_panel(
        "Parallel BE",
        icon = icon("arrow-right-arrow-left"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 5: Parallel-Group Bioequivalence"),
        tags$p("Each subject receives only one formulation (either Test or Reference, never both). ",
               "Used when crossover is impractical: long-acting drugs, irreversible effects, ",
               "or drugs with very long half-lives where washout would take too long."),
        
        tags$h6(class = "fw-semibold", "What you need"),
        tags$p(class = "small",
               "Four columns. No Period or Sequence column is needed because ",
               "each subject only takes one treatment."),
        
        ex_table(data.frame(
          Subject   = c("S01","S01","S01","S02","S02","S02","S03","S03","S03"),
          Treatment = c("Test","Test","Test","Test","Test","Test","Reference","Reference","Reference"),
          Time      = c(0,4,24, 0,4,24, 0,4,24),
          Conc      = c(0,18.3,1.1, 0,21.5,1.3, 0,19.8,0.9)
        )),
        
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("Keep in mind: "),
          "Parallel designs have more between-subject variability than crossover designs ",
          "because different people receive different treatments. You'll need more subjects ",
          "to achieve the same statistical power."
        ),
        
        checklist(c(
          "Each subject appears under only one treatment",
          "Treatment column present (Test or Reference)",
          "Period and Sequence columns are NOT needed",
          "Approximately equal numbers of subjects per treatment group",
          "If doses differ between subjects, add a Dose column"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 6: Steady-state
      # ----------------------------------------------------------------
      nav_panel(
        "Steady-State",
        icon = icon("rotate"),
        
        tags$h5(class = "fw-bold mt-2", "Scenario 6: Multiple-Dose / Steady-State PK"),
        tags$p("The drug has been given repeatedly (e.g., once daily for 7 days), and you are ",
               "sampling during one dosing interval at steady state. The drug has accumulated ",
               "to a stable level, so the pre-dose concentration is no longer zero."),
        
        tags$h6(class = "fw-semibold", "Same columns as a single-dose study"),
        tags$p(class = "small",
               "The data format is the same as Scenario 2 (multiple subjects). The key difference ",
               "is that time = 0 now means 'just before the steady-state dose', not 'before the first-ever dose'. ",
               "The pre-dose concentration will be above zero because the drug has accumulated."),
        
        ex_table(data.frame(
          Subject = c("S01","S01","S01","S01","S01","S02","S02","S02","S02","S02"),
          Time    = c(0, 0.5, 1, 4, 12, 0, 0.5, 1, 4, 12),
          Conc    = c(3.2, 28.1, 22.4, 9.8, 4.1, 2.8, 25.6, 20.1, 8.5, 3.5)
        )),
        
        tags$p(class = "small text-muted",
               "Notice the pre-dose concentration (time 0) is 3.2 and 2.8, not zero — ",
               "that's the remaining drug from previous doses."),
        
        tags$div(
          class = "alert alert-info py-2 small",
          tags$strong("In the app: "),
          "When analyzing steady-state data, tick the 'Steady-state' checkbox in the analysis settings. ",
          "This tells the NCA engine to calculate AUC within the dosing interval (AUC\u03C4) ",
          "instead of AUC extrapolated to infinity."
        ),
        
        checklist(c(
          "Time is relative to the steady-state dose (time 0 = just before the dose)",
          "Pre-dose sample is present (it will NOT be zero — that's correct)",
          "Sampling covers one full dosing interval (e.g., 0 to 12h for a twice-daily drug, 0 to 24h for once-daily)",
          "Tick 'Steady-state' in the app before running the analysis",
          "If doses differ between subjects (e.g., weight-based dosing), add a Dose column"
        ))
      ),
      
      # ----------------------------------------------------------------
      # SCENARIO 7: Working with real lab data
      # ----------------------------------------------------------------
      nav_panel(
        "Handling Real Lab Data",
        icon = icon("flask-vial"),
        
        tags$h5(class = "fw-bold mt-2", "Working with Real Bioanalytical Data"),
        tags$p("The data from the bioanalytical laboratory rarely arrives in the exact format ",
               "the app needs. This section covers the most common issues you'll encounter."),
        
        # BLQ
        tags$h6(class = "fw-semibold mt-3",
                "Below the limit of quantification (BLQ)"),
        tags$p(class = "small",
               "Every analytical method has a lower limit of quantification (LLOQ): the lowest ",
               "concentration it can reliably measure. Samples below this limit are reported as ",
               "'BLQ', '<0.5', 'ND' (not detected), or similar text. The app recognizes these ",
               "automatically — you don't need to change them before uploading."),
        
        tags$p(class = "small",
               "In the upload step, you set the LLOQ value and choose a BLQ handling rule. ",
               "The most common rule (and the default) is: BLQ samples before the first measurable ",
               "concentration are set to zero, and BLQ samples after the last measurable concentration ",
               "are excluded from the analysis. This matches what WinNonlin does by default."),
        
        tags$div(
          class = "alert alert-warning py-2 small",
          tags$strong("You must set LLOQ > 0 when your data contains BLQ entries. "),
          "The LLOQ value is the number from your bioanalytical validation report ",
          "(e.g., 0.5 ng/mL). If you leave LLOQ at 0, the app cannot process BLQ text entries ",
          "and will show an error. The app tries to auto-detect the LLOQ from entries like ",
          "'<0.5' and will pre-fill it for you — verify the value and click Process Data."
        ),
        
        ex_table(data.frame(
          Subject = rep("S01", 8),
          Time = c(0, 0.25, 0.5, 1, 2, 8, 12, 24),
          Conc = c("BLQ", "BLQ", "4.2", "18.5", "12.1", "2.8", "0.9", "<0.5")
        )),
        
        tags$p(class = "small text-muted",
               "The app auto-detects BLQ strings and the LLOQ value (0.5 in this case, ",
               "from the '<0.5' entry). Leave them as-is in your file."),
        
        # Actual vs nominal time
        tags$h6(class = "fw-semibold mt-4",
                "Actual vs. nominal sampling times"),
        tags$p(class = "small",
               "Your protocol specifies nominal times (0, 0.5, 1, 2, 4, 8, 12, 24 hours). ",
               "The actual blood draws happen close to these times but not exactly: ",
               "the 0.5h sample might be drawn at 0.52h, the 4h sample at 3.95h."),
        
        tags$p(class = "small fw-semibold",
               "Use actual times if you have them. They give more accurate AUC calculations. ",
               "Use nominal times only if actual times are not recorded."),
        
        # Missing samples
        tags$h6(class = "fw-semibold mt-4", "Missing samples"),
        tags$p(class = "small",
               "Sometimes a sample is lost (haemolysed, tube broke, subject vomited). ",
               "You can either leave the row out entirely, or include it with an empty ",
               "concentration cell (the app treats both the same way). Do NOT enter 0 for ",
               "a missing sample — that tells the app the concentration was actually zero."),
        
        do_dont(
          do_items = c(
            "Leave BLQ entries as text ('BLQ', '<0.5', 'ND') — the app handles them",
            "Use actual sampling times when available",
            "Leave missing samples blank or omit the row",
            "Include all subjects, even with incomplete data"
          ),
          dont_items = c(
            "Replace BLQ with 0 yourself (let the app apply the correct rule)",
            "Replace missing samples with 0 (that means 'concentration was zero')",
            "Round actual times to nominal times",
            "Remove subjects with 1-2 missing samples (the app works with partial profiles)"
          )
        ),
        
        # Common column name variations
        tags$h6(class = "fw-semibold mt-4", "Common column name variations the app recognizes"),
        tags$p(class = "small",
               "You don't need to rename your columns. The app auto-detects common names:"),
        
        tags$table(
          class = "table table-sm small",
          style = "max-width: 600px;",
          tags$thead(class = "table-light",
            tags$tr(tags$th("Meaning"), tags$th("Recognized names (case-insensitive)"))
          ),
          tags$tbody(
            tags$tr(tags$td("Subject"),
                    tags$td("Subject, SubjectID, USUBJID, ID, Pat, PatID, Proband")),
            tags$tr(tags$td("Time"),
                    tags$td("Time, Hours, Hour, Tpt, NTIM, APTS, Zeit")),
            tags$tr(tags$td("Concentration"),
                    tags$td("Conc, Concentration, DV, Cp, Result, Konz, Plasma")),
            tags$tr(tags$td("Treatment"),
                    tags$td("Treatment, Trt, Formulation, Drug, Arm")),
            tags$tr(tags$td("Period"),
                    tags$td("Period, Per, Prd, Phase")),
            tags$tr(tags$td("Sequence"),
                    tags$td("Sequence, Seq, Grp")),
            tags$tr(tags$td("Dose"),
                    tags$td("Dose, Amt, Amount")))
        )
      ),
      
      # ----------------------------------------------------------------
      # COMMON MISTAKES
      # ----------------------------------------------------------------
      nav_panel(
        "Common Mistakes",
        icon = icon("triangle-exclamation"),
        
        tags$h5(class = "fw-bold mt-2", "The Most Common Data Preparation Mistakes"),
        tags$p("These are the issues we see most often. Check your data against this list ",
               "before uploading."),
        
        # Mistake 1
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "1. Clock time instead of hours since dosing"),
          tags$p(class = "small",
                 "Your lab report might have '08:30', '09:00', '09:30'. ",
                 "The app needs hours since the dose: 0, 0.5, 1.0. ",
                 "You need to convert. If the dose was given at 08:30, then 09:30 = 1.0 hour.")
        ),
        
        # Mistake 2
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "2. Wide format instead of long format"),
          tags$p(class = "small",
                 "Some people create a table with one column per subject. ",
                 "The app expects one ROW per observation: Subject, Time, Concentration. ",
                 "See the 'Multiple Subjects' tab for a visual example.")
        ),
        
        # Mistake 3
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "3. Not resetting time in crossover studies"),
          tags$p(class = "small",
                 "In a crossover study, time must reset to 0 at the start of each period. ",
                 "Period 2 does not continue from where period 1 ended. ",
                 "The 'pre-dose' sample in period 2 is time = 0, not time = 168 or 336.")
        ),
        
        # Mistake 4
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "4. Replacing BLQ with 0 before uploading"),
          tags$p(class = "small",
                 "Don't do this. The app has specific rules for handling BLQ values that depend on ",
                 "where they fall in the profile (before Cmax vs. after Cmax). ",
                 "If you replace them with 0 yourself, you take away the app's ability ",
                 "to apply the correct rule. Leave BLQ as text.")
        ),
        
        # Mistake 5
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "5. Missing the Sequence column in BE studies"),
          tags$p(class = "small",
                 "Treatment and Period alone are not enough. Without Sequence, the statistical model ",
                 "cannot estimate or test for carryover effects. Many investigators forget this column, ",
                 "but it is essential for the ANOVA to give valid results.")
        ),
        
        # Mistake 6
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "6. Inconsistent subject IDs"),
          tags$p(class = "small",
                 "If subject 1 is called 'S01' in period 1 and 'S1' in period 2, ",
                 "the app treats them as two different people. Check that IDs are ",
                 "exactly the same across all rows for each subject.")
        ),
        
        # Mistake 7
        tags$div(
          class = "border-start border-4 border-danger ps-3 mb-4",
          tags$h6(class = "fw-bold text-danger", "7. Entering 0 for a missing sample"),
          tags$p(class = "small",
                 "A missing sample and a sample with concentration = 0 are completely different things. ",
                 "Zero means 'we measured and found nothing.' Missing means 'we don't have a measurement.' ",
                 "Leave the cell blank for missing data, or omit the row entirely.")
        )
      )
    ),
    
    # Ready to go
    tags$div(
      class = "text-center mt-4 mb-3",
      tags$p(class = "text-muted",
             "Ready to upload? Head to ",
             tags$a(href = "#",
                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'}); return false;",
                    tags$strong("Upload & Check Data")),
             ". The app will verify your data and flag any remaining issues."
      )
    )
  )
}
