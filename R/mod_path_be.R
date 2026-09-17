# ============================================================================
# NCA Assistant — Path 5: Bioequivalence Testing
# ============================================================================
# Complete BE workflow: verify crossover data → run NCA → configure BE →
# compute 90% CI → forest plot → export.

path_be_ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container-fluid py-3",
    style = "max-width: 1300px; margin: 0 auto;",
    
    tags$h4(class = "fw-bold mb-1",
            icon("arrows-left-right", class = "text-danger me-2"),
            "Bioequivalence Testing"),
    tags$p(class = "text-muted mb-3",
           "Compare two formulations (Test vs. Reference). This path runs PK analysis ",
           "on your crossover data, then computes the confidence interval (90% by default) to determine ",
           "if the formulations are bioequivalent."),
    
    uiOutput(ns("data_gate")),
    
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("data_ok")),
      
      layout_sidebar(
        fillable = FALSE,
        
        sidebar = sidebar(
          title = tagList("BE Settings", help_what_is_be),
          width = 380, open = TRUE,
          
          # Step 1: NCA settings
          card(
            card_header(class = "bg-primary text-white",
                        "Step 1: PK Analysis Settings"),
            card_body(
              selectInput(ns("admin_route"),
                          tagList("Route of administration", help_admin_route),
                          choices = c("Oral / IM / SC (extravascular)" = "extravascular",
                                      "IV Bolus (injected into vein at once)" = "iv_bolus")),
              # Note: IV Infusion is not supported in BE analysis (no paired
              # infusion duration per treatment period).
              
              radioButtons(ns("dose_source"), "Dose information",
                           choices = c(
                             "Same dose for all subjects" = "single",
                             "Different doses (from Dose column)" = "from_data"
                           ), selected = "single"),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'single'", ns("dose_source")),
                numericInput(ns("dose"), "Dose", value = 100, min = 0)
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'from_data'", ns("dose_source")),
                uiOutput(ns("dose_column_status"))
              ),
              
              layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("dose_unit"), "Dose", choices = DOSE_UNIT_CHOICES, selected = "mg"),
                selectInput(ns("time_unit"), "Time", choices = TIME_UNIT_CHOICES, selected = "h"),
                selectInput(ns("conc_unit"), "Conc", choices = CONC_UNIT_CHOICES, selected = "ng/mL")
              ),
              numericInput(ns("mw"), "Molecular weight (only for molar units)",
                           value = 0, min = 0, step = 1),
              selectInput(ns("trap_method"),
                          tagList("Trapezoidal method", help_trapezoidal),
                          choices = c("Linear-up / Log-down" = "log",
                                      "Linear-up / Linear-down" = "linear")),
              sliderInput(ns("r2adj_be"),
                          tagList("Minimum R\u00B2 for half-life estimation", help_r2adj),
                          min = 0,   max = 1, value = 0.7, step = 0.05),
              checkboxInput(ns("is_ss"),
                            tagList("Steady-state (drug given repeatedly)", help_steady_state),
                            value = FALSE),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("is_ss")),
                numericInput(ns("tau"), "Dosing interval \u03C4 (same unit as Time)", value = NA, min = 0))
            )
          ),
          
          # Step 2: BE settings
          card(
            card_header(class = "bg-primary text-white",
                        "Step 2: Bioequivalence Settings"),
            card_body(
              selectInput(ns("be_design"),
                          tagList("Study design", help_be_design),
                          choices = be_analysis_choices(), selected = "2x2x2"),
              uiOutput(ns("reference_ui")),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'paired'", ns("be_design")),
                tags$div(
                  class = "alert alert-warning py-2 small mb-2",
                  icon("triangle-exclamation", class = "me-1"),
                  tags$strong("Not a bioequivalence design: "),
                  "All subjects received treatments in the same order, so period and ",
                  "treatment effects cannot be separated. A paired analysis (equivalent to a ",
                  "paired t-test on the log-transformed parameters) reports the ratio and its ",
                  "confidence interval, but no bioequivalence verdict is given. Suitable for ",
                  "before/after comparisons such as drug-interaction or switch studies."
                )
              ),
              
              # Shown for every design the planner offers scaled methods for
              conditionalPanel(
                condition = sprintf("[%s].indexOf(input['%s']) >= 0",
                                    paste0("'", BE_DESIGNS$code[BE_DESIGNS$plan_scaled], "'", collapse = ", "),
                                    ns("be_design")),
                tags$div(
                  class = "alert alert-info py-2 small mb-2",
                  icon("circle-info", class = "me-1"),
                  tags$strong("Note: "),
                  "This app performs average bioequivalence (ABE) with the acceptance limits you enter, for all designs. ",
                  "It does not perform reference-scaled analysis (ABEL/RSABE). ",
                  "When the reference is replicated, the results show CV", tags$sub("wR"),
                  " and the limits it would imply, for information. If your drug requires ",
                  "widened or scaled limits (CV", tags$sub("wR"), " > 30%), use dedicated ",
                  "software for the scaled analysis."
                )
              ),
              
              selectInput(ns("model_type"),
                          tagList("Statistical model", help_mixed_effects),
                          choices = c(
                            "Fixed effects (EMA: all terms fixed)" = "fixed",
                            "Mixed effects (subject random; uses dropouts)" = "mixed"
                          )),
              
              checkboxGroupInput(ns("be_params"), "Parameters to compare",
                                 choiceNames = unname(sapply(c("CMAX", "AUCLST", "AUCIFO", "TMAX", "LAMZHL"), friendly_name)),
                                 choiceValues = c("CMAX", "AUCLST", "AUCIFO", "TMAX", "LAMZHL"),
                                 selected = c("CMAX", "AUCLST", "AUCIFO")),
              
              checkboxInput(ns("log_transform"),
                            tagList("Log-transform (recommended)", help_log_transform),
                            TRUE),
              
              tags$h6("Confidence interval", help_ci_level),
              sliderInput(ns("ci_level"), NULL,
                          min = 80, max = 99, value = 90, step = 1, post = "%"),
              
              tags$h6("Acceptance limits", help_be_limits),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("be_lower"), "Lower (%)", value = 80),
                numericInput(ns("be_upper"), "Upper (%)", value = 125)
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] < 80 || input['%s'] > 125",
                                    ns("be_lower"), ns("be_upper")),
                checkboxInput(ns("pe_constraint"),
                              "Also require the point estimate within 80.00\u2013125.00%",
                              value = TRUE),
                tags$div(
                  class = "alert alert-warning py-2 small mb-2",
                  icon("triangle-exclamation", class = "me-1"),
                  "Widened limits must be pre-specified in the protocol. This app does not ",
                  "derive them or check them against CV", tags$sub("wR"), ". Reference-scaled ",
                  "methods (ABEL, RSABE) require the point-estimate constraint; untick it only ",
                  "for comparisons that have none, such as drug-interaction no-effect boundaries."
                )
              )
            )
          ),
          
          partial_auc_ui(ns("pauc"), show_role = TRUE),

          hr(),
          
          actionButton(ns("run_be"), "Run Complete BE Analysis",
                       class = "btn-success btn-lg w-100",
                       icon = icon("play")),
          tags$p(class = "text-muted small mt-2 text-center",
                 "This runs NCA on all profiles, then performs the ",
                 "bioequivalence comparison.")
        ),
        
        # --- Results ---------------------------------------------------------
        tagList(
          uiOutput(ns("be_status")),
          uiOutput(ns("ss_note")),
          uiOutput(ns("pauc_note")),
          uiOutput(ns("balance_note")),
          uiOutput(ns("design_summary")),
          
          navset_card_tab(
            title = "Bioequivalence Results",
            
            nav_panel(
              "Test vs. Reference Comparison",
              icon = icon("arrows-left-right"),
              tags$p(class = "text-muted small",
                     "The table shows the geometric mean ratio (Test ÷ Reference) ",
                     "and its confidence interval (90% by default). If the CI falls entirely within ",
                     "the acceptance limits (usually 80–125%), the formulations are bioequivalent. ",
                     "With limits wider than 80–125%, the point estimate must also lie within ",
                     "80–125% unless that constraint is switched off. Half-life is shown as a ratio ",
                     "with its confidence interval but has no verdict, because it is not a ",
                     "bioequivalence endpoint. A partial AUC marked as supportive is shown the same way. Tmax, and any parameter analysed without ",
                     "log-transformation, is shown as a difference in its own units and has no verdict."),
              DTOutput(ns("ci_table")),
              tags$p(class = "text-muted small mt-2",
                     icon("circle-info", class = "me-1"),
                     "Full details (N per group, acceptance limits, residual variance, ",
                     "degrees of freedom) are included in the Excel and CSV downloads."),
              hr(),
              tags$p(class = "text-muted small",
                     "Forest plot: dot = point estimate, bar = confidence interval, ",
                     "dashed lines = acceptance limits."),
              plotlyOutput(ns("forest_plot"), height = "350px"),
              uiOutput(ns("variability_panel"))
            ),
            
            nav_panel(
              "PK Parameter Table",
              icon = icon("table"),
              tags$p(class = "text-muted small",
                     "Individual NCA results for all subject-treatment profiles."),
              checkboxInput(ns("nca_show_all"), "Show all parameters (37 columns)", FALSE),
              DTOutput(ns("nca_table")),
              uiOutput(ns("cdisc_codes"))
            ),
            
            nav_panel(
              "Statistical Details (ANOVA)",
              icon = icon("table"),
              uiOutput(ns("anova_output"))
            ),
            
            nav_panel(
              "Concentration-Time Profiles",
              icon = icon("chart-line"),
              plotlyOutput(ns("profile_plot"), height = "450px")
            ),
            
            nav_panel(
              "Individual Profiles",
              icon = icon("grip"),
              tags$p(class = "text-muted small",
                     "One panel per subject. Test and Reference overlaid in different colours."),
              layout_columns(
                col_widths = c(4, 8),
                selectInput(ns("be_grid_page"), "Subjects shown",
                            choices = "All", selected = "All"),
                tags$span()
              ),
              plotlyOutput(ns("be_grid_plot"), height = "700px")
            ),
            
            nav_panel(
              tagList("Half-Life Review", help_lambda_z),
              icon = icon("magnifying-glass-chart"),
              tags$p(class = "text-muted small",
                     "Review and adjust the terminal phase regression for individual profiles. ",
                     "Adjustments affect AUC\u221E, CL/F, and Vz/F. If your BE comparison uses ",
                     "only Cmax and AUClast, changes here will not affect the confidence intervals."),
              layout_columns(
                col_widths = c(6, 6),
                selectInput(ns("lz_profile"), "Profile:", choices = NULL),
                tags$div(
                  style = "padding-top: 1.7rem;",
                  uiOutput(ns("lz_status"))
                )
              ),
              plotlyOutput(ns("lz_plot"), height = "400px"),
              tags$div(
                class = "mt-2",
                tags$h6("Select terminal phase points:"),
                checkboxGroupInput(ns("lz_points"), NULL, choices = NULL, inline = TRUE),
                actionButton(ns("lz_recalc"), "Recalculate",
                             class = "btn-warning btn-sm",
                             icon = icon("refresh"))
              )
            )
          ),
          
          # Downloads
          tags$div(
            class = "mt-3",
            downloadButton(ns("dl_be_xlsx"), "Download Complete BE Report (Excel)",
                           class = "btn-outline-success"),
            downloadButton(ns("dl_ci_csv"), "Download CI Table (CSV)",
                           class = "btn-outline-primary ms-2")
          ),
          
          # Analysis Record — consistent, discoverable panel (appears once
          # the BE analysis has been run).
          uiOutput(ns("record_panel"))
        )
      )
    )
  )
}

path_be_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$data_ok <- reactive({ shared$data_ready })
    outputOptions(output, "data_ok", suspendWhenHidden = FALSE)
    
    # Auto-select "from_data" when Dose column is mapped
    observe({
      if (shared$data_ready && !is.null(shared$col_map$dose)) {
        updateRadioButtons(session, "dose_source", selected = "from_data")
      }
    })
    
    # Dose column status
    output$dose_column_status <- renderUI({
      if (!shared$data_ready || is.null(shared$col_map$dose)) {
        return(tags$div(class = "alert alert-warning py-2 small",
                        icon("triangle-exclamation"),
                        " No Dose column mapped. Map it in Upload & Check Data, ",
                        "or switch to 'Same dose for all'."))
      }
      dose_vals <- shared$pk_data[[shared$col_map$dose]]
      dose_by_subj <- tapply(dose_vals, shared$pk_data[[shared$col_map$subject]],
                             function(x) max(x, na.rm = TRUE))
      unique_doses <- sort(unique(dose_by_subj))
      tags$div(class = "alert alert-success py-2 small",
               icon("circle-check"),
               paste0(" Dose column '", shared$col_map$dose, "': ",
                      length(unique_doses), " dose level(s): ",
                      paste(unique_doses, collapse = ", "), " ",
                      input$dose_unit, "."))
    })
    
    output$data_gate <- renderUI({
      if (!shared$data_ready) {
        card(class = "border-warning",
             card_body(class = "text-center py-4",
                       icon("triangle-exclamation", class = "fa-2x text-warning mb-2"),
                       tags$h5("No data loaded yet"),
                       tags$p("Bioequivalence requires crossover data with Treatment, ",
                              "Period, and Sequence columns."),
                       actionButton(ns("goto_upload"), "Go to Upload & Check Data", class = "btn-warning",
                                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});")))
      } else if (is.null(shared$col_map$treatment)) {
        card(class = "border-warning",
             card_body(class = "text-center py-4",
                       icon("triangle-exclamation", class = "fa-2x text-warning mb-2"),
                       tags$h5("Treatment column not mapped"),
                       tags$p("Go to Upload & Check Data and map your Treatment/Formulation column."),
                       actionButton(ns("goto_upload2"), "Go to Upload & Check Data", class = "btn-warning",
                                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});")))
      }
    })
    
    # Update BE parameter choices when NCA results exist
    observe({
      req(be_nca_result())
      r <- be_nca_result()
      # Partial AUCs and Cmax in an interval; Tmax in an interval is not compared
      pauc_params <- grep("^(AUC|CMAX)_", partial_auc_cols(names(r)), value = TRUE)
      available <- c(intersect(
        c("CMAX","AUCTAU","AUCLST","AUCIFO","AUCIFP","TMAX","LAMZHL"), names(r)), pauc_params)
      # At steady state AUCTAU (AUC from 0 to tau) is the primary exposure
      # parameter; AUC to infinity has no meaning during repeated dosing.
      default <- if (isTRUE(input$is_ss)) {
        intersect(c("CMAX","AUCTAU"), available)
      } else {
        intersect(c("CMAX","AUCLST","AUCIFO"), available)
      }
      default <- c(default, pauc_params)
      updateCheckboxGroupInput(session, "be_params",
                               choiceNames = unname(sapply(available, friendly_name)),
                               choiceValues = available,
                               selected = default)
    })
    
    # NCA results (run as part of BE)
    be_nca_result  <- reactiveVal(NULL)
    be_result      <- reactiveVal(NULL)
    balance_result <- reactiveVal(NULL)   # stores imbalance info for persistent alert
    # Settings exactly as used by the last completed run. The Analysis Record is
    # built from this snapshot, not from the inputs at download time, which may
    # have changed since (and which never held the per-subject dose vector).
    be_run_settings <- reactiveVal(NULL)
    be_nca_settings <- reactiveVal(NULL)   # NCA settings of the last run (for recalculation)
    pauc_spec  <- partial_auc_server("pauc", show_role = TRUE)
    pauc_notes <- reactiveVal(character(0))

    # Offer (and select) the interval metrics as soon as the intervals are
    # valid, so the first run already compares them
    observeEvent(pauc_spec(), {
      spec <- pauc_spec()
      new <- if (is.null(spec) || !is.null(validate_partial_aucs(spec))) character(0) else {
        nm <- partial_auc_names(spec)
        unlist(lapply(seq_len(nrow(spec)), function(i) c(nm$auc[i], if (spec$cmax[i]) nm$cmax[i])))
      }
      r <- isolate(be_nca_result())
      base <- if (is.null(r)) c("CMAX", "AUCLST", "AUCIFO", "TMAX", "LAMZHL") else
        intersect(c("CMAX", "AUCTAU", "AUCLST", "AUCIFO", "AUCIFP", "TMAX", "LAMZHL"), names(r))
      updateCheckboxGroupInput(session, "be_params",
                               choiceNames = unname(sapply(c(base, new), friendly_name)),
                               choiceValues = c(base, new),
                               selected = union(intersect(isolate(input$be_params), base), new))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$run_be, {
      req(shared$pk_data, shared$col_map, shared$col_map$treatment)
      
      # Dataset size guard
      nr <- nrow(shared$pk_data)
      if (nr > 50000) {
        showNotification("Dataset too large for BE analysis (>50,000 rows). Consider subsetting.",
                         type = "error", duration = 8); return()
      }
      if (nr > 10000) {
        showNotification("Large dataset — analysis may take a moment.", type = "warning", duration = 5)
      }
      
      cm <- shared$col_map
      use_data_dose <- (input$dose_source == "from_data" && !is.null(cm$dose))
      
      if (!use_data_dose) {
        if (is.null(input$dose) || is.na(input$dose) || input$dose <= 0) {
          showNotification("Please enter a valid dose (greater than 0).",
                           type = "error", duration = 5)
          return()
        }
      }
      
      # Validate acceptance limits
      if (is.null(input$be_lower) || is.na(input$be_lower) ||
          is.null(input$be_upper) || is.na(input$be_upper)) {
        showNotification("Please enter both acceptance limits (lower and upper).",
                         type = "error", duration = 5)
        return()
      }
      if (input$be_lower >= input$be_upper) {
        showNotification("Lower acceptance limit must be less than upper (e.g., 80 and 125).",
                         type = "error", duration = 5)
        return()
      }
      
      # Validate IV infusion duration
      if (input$admin_route == "iv_infusion") {
        showNotification("IV infusion is not supported in the BE module. Select Oral/IM/SC or IV Bolus.",
                         type = "error", duration = 8)
        return()
      }
      
      if (is.null(input$be_reference) || !nzchar(input$be_reference)) {
        showNotification("Choose the Reference treatment in Step 2 (the ratio is Test / Reference).",
                         type = "error", duration = 8)
        return()
      }

      if (isTRUE(input$is_ss) && (is.null(input$tau) || is.na(input$tau) || input$tau <= 0)) {
        showNotification("Steady state: enter the dosing interval \u03C4 (for example 12 or 24 h).",
                         type = "error", duration = 8)
        return()
      }
      pauc_err <- validate_partial_aucs(pauc_spec(), isTRUE(input$is_ss), input$tau)
      if (!is.null(pauc_err)) {
        showNotification(pauc_err, type = "error", duration = 10)
        return()
      }
      withProgress(message = "Step 1: Running NCA...", value = 0.3, {
        
        # Run NCA
        settings <- list(
          admin_route = input$admin_route,
          dose = if (use_data_dose) NA else input$dose,
          infusion_duration = 0,
          is_steady_state = isTRUE(input$is_ss), tau = input$tau,
          dose_unit = input$dose_unit,
          time_unit = input$time_unit,
          conc_unit = input$conc_unit,
          trap_method = input$trap_method,
          r2adj_threshold = input$r2adj_be,
          mw = input$mw, partial_aucs = pauc_spec()
        )
        
        # Units drive a real conversion factor for CL/F and Vz/F inside NonCompart,
        # and an unrecognised spelling makes the NCA fail with an opaque message.
        # Check the combination before running so the user gets a usable error.
        uchk <- validate_units(input$dose_unit, input$time_unit, input$conc_unit, input$mw)
        if (!uchk$valid) {
          showNotification(uchk$message, type = "error", duration = 12)
          return()
        }

        if (use_data_dose) {
          dose_vec <- suppressWarnings(dose_by_profile(shared$pk_data, cm))
          if (any(!is.finite(dose_vec) | dose_vec <= 0)) {
            showNotification(
              "Some subjects have missing or zero dose values. Check the Dose column in your data.",
              type = "error", duration = 8)
            return()
          }
          # One dose per profile (subject x treatment x period), matched by profile
          # key in run_nca(); a subject may get different doses in different periods
          settings$dose <- dose_vec
          settings$dose_source <- "per_profile"
        }
        
        nca_warnings_be <- character(0)
        nca_res <- withCallingHandlers(
          run_nca(shared$pk_data, cm, settings, lz_overrides = lz_state$overrides_log),
          warning = function(w) {
            nca_warnings_be <<- c(nca_warnings_be, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        
        if (is.null(nca_res)) {
          detail <- if (length(nca_warnings_be) > 0) paste(nca_warnings_be, collapse = " ")
                    else "Check the data and column mapping."
          showNotification(paste0("NCA failed. ", detail), type = "error", duration = NULL)
          return()
        }
        
        is_pauc <- startsWith(nca_warnings_be, "Partial AUC")
        pauc_notes(nca_warnings_be[is_pauc])
        nca_warnings_be <- nca_warnings_be[!is_pauc]
        if (any(is_pauc))
          showNotification("Partial AUCs: see the notes above the results.", type = "warning", duration = 8)
        if (length(nca_warnings_be) > 0) {
          showNotification(
            paste0("Note: ", paste(nca_warnings_be, collapse = "; ")),
            type = "warning", duration = 12)
        }
        
        be_nca_result(nca_res)
        be_nca_settings(settings)
        shared$nca_results <- nca_res
        shared$partial_aucs <- settings$partial_aucs
        gc()  # Free NCA intermediates before BE analysis
        
        setProgress(0.5, message = "Step 2: Running BE analysis...")
        
        # Merge with design info: one row per NCA profile (subject x treatment
        # x period), with the Sequence column attached. See build_be_data().
        bd <- tryCatch(build_be_data(nca_res, shared$pk_data, cm, reference = input$be_reference),
                       error = function(e) {
                         showNotification(conditionMessage(e), type = "error", duration = NULL)
                         NULL
                       })
        if (is.null(bd)) return()
        be_data     <- bd$data
        trt_col_be  <- bd$trt_col
        subj_col_be <- bd$subj_col
        per_col     <- bd$per_col
        seq_col     <- bd$seq_col
        trt_levels  <- levels(be_data[[trt_col_be]])
        
        if (length(trt_levels) != 2) {
          showNotification(
            paste0("Treatment column must have exactly 2 levels (found ",
                   length(trt_levels),
                   if (length(trt_levels) > 0) paste0(": ", paste(trt_levels, collapse = ", ")) else "",
                   "). Please ensure your Treatment column contains exactly two values ",
                   "(e.g., Test and Reference, or Drug A and Drug B)."),
            type = "error", duration = 10)
          return()
        }
        
        
        params <- input$be_params
        if (is.null(params) || length(params) == 0)
          params <- c("CMAX","AUCLST","AUCIFO")
        if (isTRUE(input$is_ss)) {
          # At steady state the exposure parameter is AUC from 0 to tau
          swapped <- intersect(c("AUCLST","AUCIFO"), params)
          params <- unique(c(setdiff(params, c("AUCLST","AUCIFO")), "AUCTAU"))
          if (length(swapped) > 0)
            showNotification(paste0("Steady state: AUC over the dosing interval (AUC\u03C4) is compared ",
                                    "instead of ", paste(sapply(swapped, friendly_name), collapse = " and "), "."),
                             type = "message", duration = 10)
        }
        params <- intersect(params, names(nca_res))
        
        # A single treatment order is a paired comparison whatever was selected;
        # analyse it as one rather than fitting a confounded crossover model.
        design_used <- resolve_be_design(input$be_design, be_data,
                                         subj_col = subj_col_be, trt_col = trt_col_be,
                                         per_col = per_col, seq_col = seq_col)
        if (!is.null(design_used$note)) {
          showNotification(design_used$note, type = "warning", duration = 15)
        } else {
          mismatch <- check_design_against_data(input$be_design, shared$study_info$design)
          if (!is.null(mismatch)) showNotification(mismatch, type = "warning", duration = 15)
        }
        
        # Warn when no Sequence column is mapped for crossover designs
        if (is.null(seq_col) &&
            be_design_model(design_used$design) == "crossover") {
          showNotification(
            paste0("No Sequence column is mapped. For a ", design_used$design,
                   " design the Sequence term is part of the standard ANOVA table ",
                   "(ln(PK) = Sequence + Subject(Sequence) + Period + Treatment). ",
                   "With subject as a fixed effect the ratio and confidence interval are the same ",
                   "without it, because each subject belongs to one sequence; only the test of the ",
                   "sequence effect is missing. If your data have a Sequence column, map it in the Upload step."),
            type = "warning", duration = 15)
        }

        # ---- Balanced design pre-check ------------------------------------
        # Check each subject appears in both treatment levels in the NCA result.
        # be_data is at the NCA result grain (one row per subject-treatment),
        # so this is design-agnostic — valid for 2x2, 3-period, and replicate.
        # Missing subjects are listed by name and shown as a persistent alert.
        # Analysis still proceeds (na.exclude handles missing data in lm/lme),
        # but the user must be aware of the imbalance.
        balance_subjects <- unique(be_data[[subj_col_be]])
        incomplete_subjects <- Filter(function(s) {
          trts <- be_data[[trt_col_be]][be_data[[subj_col_be]] == s]
          !all(trt_levels %in% as.character(trts))
        }, balance_subjects)
        
        if (length(incomplete_subjects) > 0) {
          n_total <- length(balance_subjects)
          n_incomplete <- length(incomplete_subjects)
          subj_list <- paste(head(incomplete_subjects, 10), collapse = ", ")
          if (n_incomplete > 10) subj_list <- paste0(subj_list, ", ...")
          showNotification(
            paste0(n_incomplete, " of ", n_total,
                   " subject(s) have data for only one treatment: ", subj_list,
                   ". These subjects contribute to one treatment arm only. ",
                   "Degrees of freedom are reduced accordingly."),
            type = "warning", duration = 15)
          # Store for persistent alert (written below after be_result is set)
          balance_info <- list(n_incomplete = n_incomplete, n_total = n_total,
                               subjects = incomplete_subjects)
        } else {
          balance_info <- NULL
        }
        # -------------------------------------------------------------------

        ci_results <- list()
        anova_results <- list()
        
        # Warn once if Tmax is among the selected parameters
        if ("TMAX" %in% params) {
          showNotification(
            paste0("Tmax is included in your analysis. Note: Tmax is a discrete ",
                   "variable that takes only values present in the sampling schedule. ",
                   "A parametric ANOVA model is not the usual analysis for Tmax. ",
                   "When a Tmax comparison is relevant (for example a claim of rapid release), ",
                   "the EMA guideline asks for a non-parametric analysis. ",
                   "The parametric CI shown here is provided for completeness only, ",
                   "carries no bioequivalence verdict, ",
                   "and should not be used as the primary Tmax analysis in a ",
                   "regulatory submission."),
            type = "warning", duration = 20)
        }

        # Unit of an untransformed difference (TMAX, or any parameter when the
        # log-transform is off), so the table never presents it as a ratio.
        pauc_names <- if (is.null(settings$partial_aucs)) NULL else partial_auc_names(settings$partial_aucs)
        supportive <- if (is.null(pauc_names)) character(0) else
          unlist(pauc_names[settings$partial_aucs$role == "supportive", c("auc", "cmax")])
        diff_unit_for <- function(param) {
          if (startsWith(param, "AUC_")) return(paste0(input$conc_unit, "\u00B7", input$time_unit))
          if (startsWith(param, "CMAX_")) return(input$conc_unit)
          switch(param,
                 TMAX = , LAMZHL = input$time_unit,
                 CMAX = input$conc_unit,
                 AUCLST = , AUCTAU = , AUCIFO = , AUCIFP = paste0(input$conc_unit, "\u00B7", input$time_unit),
                 NULL)
        }

        for (param in params) {
          fit_out <- fit_be_parameter(
            be_data, param,
            design        = design_used$design,
            model_type    = input$model_type,
            trt_col       = trt_col_be,
            subj_col      = subj_col_be,
            per_col       = per_col,
            seq_col       = seq_col,
            log_transform = input$log_transform,
            ci_level      = input$ci_level,
            be_lower      = input$be_lower,
            be_upper      = input$be_upper,
            pe_constraint = !identical(input$pe_constraint, FALSE),
            diff_unit     = diff_unit_for(param),
            verdict       = !param %in% supportive)
          if (!is.na(fit_out$row$Model) && grepl("mixed model failed", fit_out$row$Model)) {
            showNotification(paste0(friendly_name(param), ": the mixed model could not be fitted; ",
                                    "fixed effects were used instead. See the Model column in the downloads."),
                             type = "warning", duration = 12)
          }
          if (!is.null(fit_out$reason)) {
            showNotification(
              paste0("Could not compute BE results for ", friendly_name(param), ": ", fit_out$reason),
              type = "error", duration = 12)
          }
          if (!is.null(fit_out$anova)) anova_results[[param]] <- fit_out$anova
          ci_results[[param]] <- fit_out$row
        }
        
        ci_df <- do.call(rbind, ci_results)

        # Within-subject variability (replicate designs only; informational).
        # Only for log-transformed ratio parameters, never TMAX.
        cv_rows <- list()
        if (isTRUE(input$log_transform)) {
          for (param in setdiff(params, c("TMAX", BE_NO_VERDICT_PARAMS))) {
            # No log-scale variability with zero values (see fit_be_parameter)
            if (any(as.numeric(be_data[[param]]) == 0, na.rm = TRUE)) next
            cv_rows[[param]] <- tryCatch(
              be_variability_diagnostic(be_data, param, trt_col = trt_col_be,
                                        subj_col = subj_col_be, per_col = per_col,
                                        seq_col = seq_col),
              error = function(e) NULL)
          }
        }
        cv_df <- if (length(cv_rows) > 0) do.call(rbind, cv_rows) else NULL
        if (!is.null(cv_df)) {
          pe <- ci_df$Point_Est[match(cv_df$Parameter, ci_df$Parameter)]
          cv_df$PE_within_80_125 <- ifelse(is.na(pe), NA,
                                           ifelse(pe >= 80 & pe <= 125, "YES", "NO"))
        }

        be_result(list(ci_table = ci_df, anova = anova_results, cv_table = cv_df))
        be_run_settings(list(
          nca = settings,
          be  = list(
            design_selected   = input$be_design,
            design_analysed   = design_used$design,
            reference         = input$be_reference,
            model_type        = input$model_type,
            log_transform     = isTRUE(input$log_transform),
            ci_level          = input$ci_level,
            acceptance_limits = c(input$be_lower, input$be_upper),
            pe_constraint     = !identical(input$pe_constraint, FALSE),
            parameters        = params)))
        shared$be_results <- be_result()
        balance_result(balance_info)  # persist for the alert panel
        
        setProgress(1, message = "Done!")
        showNotification("Bioequivalence analysis complete.", type = "message")
      })
    })
    
    # Confidence level of the analysis that was run (the slider may have moved since)
    run_ci_level <- function() {
      lv <- be_run_settings()$be$ci_level
      if (is.null(lv)) 90 else lv
    }

    # Reference treatment: suggested only from an unambiguous name (R, Ref,
    # Reference, ...); otherwise the user must choose, because the ratio is
    # Test/Reference and alphabetical order says nothing about which is which.
    output$reference_ui <- renderUI({
      cm <- shared$col_map; dat <- shared$pk_data
      if (is.null(cm) || is.null(dat) || is.null(cm$treatment) || !cm$treatment %in% names(dat))
        return(NULL)
      lv <- sort(unique(trimws(as.character(dat[[cm$treatment]]))))
      lv <- lv[!is.na(lv) & nzchar(lv)]
      sug <- suggest_reference_treatment(lv)
      tagList(
        selectInput(ns("be_reference"),
                    tagList("Reference treatment",
                            tags$span(class = "text-muted small",
                                      " (the ratio is Test / Reference)")),
                    choices = c("Choose\u2026" = "", lv),
                    selected = if (is.null(sug)) "" else sug),
        if (is.null(sug))
          tags$p(class = "text-muted small mt-n2",
                 "The app cannot tell from the names which treatment is the Reference. Choose it.")
      )
    })

    # Status
    output$be_status <- renderUI({
      if (is.null(be_result())) {
        card(card_body(class = "text-center py-4 text-muted",
                       icon("arrows-left-right", class = "fa-2x mb-2"),
                       tags$h5("Configure settings and click 'Run Complete BE Analysis'")))
      }
    })

    # Balance note — persistent alert when subjects have incomplete treatment data
    output$balance_note <- renderUI({
      bi <- balance_result()
      if (is.null(bi) || bi$n_incomplete == 0) return(NULL)
      subj_list <- paste(head(bi$subjects, 10), collapse = ", ")
      if (bi$n_incomplete > 10) subj_list <- paste0(subj_list, " ...")
      tags$div(
        class = "alert alert-warning py-2 small mb-2",
        icon("triangle-exclamation", class = "me-1"),
        tags$strong("Incomplete design detected: "),
        paste0(bi$n_incomplete, " of ", bi$n_total,
               " subject(s) have data for only one treatment level."),
        tags$br(),
        tags$span(class = "text-muted",
                  paste0("Affected subjects: ", subj_list, ". "),
                  "These subjects contribute to one arm only. Degrees of freedom ",
                  "are reduced and confidence intervals may be wider than expected. ",
                  "Verify that missing profiles are not due to a data preparation error.")
      )
    })

    # Design summary — shown after NCA runs to let user verify their model setup
    output$design_summary <- renderUI({
      req(be_nca_result())
      r   <- be_nca_result()
      cm  <- shared$col_map
      dat <- shared$pk_data
      if (is.null(dat) || is.null(cm)) return(NULL)
      
      # Counts from the NCA result (post-filtering)
      n_profiles  <- nrow(r)
      trt_col     <- if ("Treatment" %in% names(r)) "Treatment" else cm$treatment
      treatments  <- if (!is.null(trt_col) && trt_col %in% names(r))
                       sort(unique(r[[trt_col]])) else character(0)
      n_trt       <- length(treatments)
      
      # Subjects per treatment from the result table
      subj_col    <- if ("Subject" %in% names(r)) "Subject" else names(r)[1]
      # Subjects and profiles differ in replicate designs, where a subject
      # receives the same treatment in more than one period.
      per_trt     <- if (n_trt > 0 && trt_col %in% names(r)) {
        sapply(treatments, function(t)
          length(unique(r[[subj_col]][r[[trt_col]] == t])))
      } else integer(0)
      prof_trt    <- if (n_trt > 0 && trt_col %in% names(r)) {
        sapply(treatments, function(t) sum(r[[trt_col]] == t, na.rm = TRUE))
      } else integer(0)
      
      # Design info from shared study_info (set at upload time)
      si     <- shared$study_info
      design <- if (!is.null(si)) si$design else NULL
      n_periods   <- if (!is.null(design)) design$n_periods   else "?"
      n_sequences <- if (!is.null(design)) design$n_sequences else "?"
      design_type <- if (!is.null(design) && !is.null(design$type))
                       design$type else input$be_design
      
      tags$div(
        class = "alert alert-light py-2 small mb-2",
        style = "border-left: 3px solid #3498DB;",
        icon("info-circle", class = "text-info me-1"),
        tags$strong("Design detected from data: "),
        tags$span(design_type),
        tags$span(class = "text-muted ms-3",
                  paste0(n_profiles, " profile(s)")),
        if (n_trt > 0) {
          trt_str <- paste(
            mapply(function(t, n, k) paste0(t, " (n=", n, if (k != n) paste0(", ", k, " profiles"), ")"),
                   treatments, per_trt, prof_trt),
            collapse = ", ")
          tags$span(class = "text-muted ms-3",
                    paste0("Treatments: ", trt_str))
        },
        tags$span(class = "text-muted ms-3",
                  paste0(n_periods, " period(s), ",
                         n_sequences, " sequence(s)")),
        tags$br(),
        tags$span(class = "text-muted",
                  "Verify this matches your intended design before interpreting the confidence intervals.")
      )
    })

    output$pauc_note <- renderUI({
      req(be_result())
      partial_auc_notes_ui(pauc_notes())
    })

    # Steady-state note — shown in results area when SS is active
    output$ss_note <- renderUI({
      if (!isTRUE(input$is_ss) || is.null(be_result())) return(NULL)
      tags$div(
        class = "alert alert-info py-2 small mb-2",
        icon("circle-info", class = "me-1"),
        tags$strong("Steady-state analysis: "),
        "AUC\u03C4 is the AUC from 0 to the dosing interval you entered (extrapolated with ",
        "\u03BBz when the last sample is before \u03C4). The exposure parameters compared are ",
        "Cmax and AUC\u03C4; AUC to infinity is not meaningful during repeated dosing."
      )
    })

    # CI table
    output$ci_table <- renderDT({
      req(be_result())
      ci_lab <- paste0(run_ci_level(), "% CI ")
      display_ci <- rename_be_columns(be_result()$ci_table, ci_level = run_ci_level())
      # Show which treatments were compared, so a wrong Reference is visible
      display_ci$Comparison <- paste(display_ci[["Test Formulation"]], "/",
                                     display_ci[["Reference Formulation"]])
      be_col <- if ("Bioequivalent?" %in% names(display_ci)) "Bioequivalent?" else "Bioequivalent"
      
      # Show only key columns — the rest are in the Excel export
      key_cols <- intersect(c("PK Parameter", "Comparison", "Scale", "Estimate", paste0(ci_lab, "Lower"),
                              paste0(ci_lab, "Upper"), "PE within 80\u2013125%", "Bioequivalent?"),
                            names(display_ci))
      # Profiles that could not enter a comparison are part of the result
      for (cc in c("Profiles missing (Test)", "Profiles missing (Reference)",
                   "Zero values (Test)", "Zero values (Reference)")) {
        v <- suppressWarnings(as.numeric(display_ci[[cc]]))
        if (!is.null(v) && any(v > 0, na.rm = TRUE))
          key_cols <- append(key_cols, cc, after = match("Bioequivalent?", key_cols) - 1)
      }
      display_ci <- display_ci[, key_cols, drop = FALSE]
      
      # Fixed 2 decimal places for ratio and CI columns (regulatory standard)
      num_cols <- intersect(c("Estimate", paste0(ci_lab, "Lower"), paste0(ci_lab, "Upper")),
                            names(display_ci))
      dt <- datatable(display_ci,
                options = list(scrollX = TRUE, dom = "t", ordering = FALSE),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatStyle(be_col,
                    backgroundColor = styleEqual(c("YES","NO"), c("#d4edda","#f8d7da")),
                    fontWeight = "bold")
      if (length(num_cols) > 0)
        dt <- dt %>% formatRound(columns = num_cols, digits = 2)
      dt
    })
    
    # Within-subject variability: inputs to a scaled assessment, no verdict
    output$variability_panel <- renderUI({
      req(be_result())
      cv <- be_result()$cv_table
      if (is.null(cv) || nrow(cv) == 0) return(NULL)
      fmt <- function(x, d = 2) ifelse(is.na(x), "\u2014", formatC(x, format = "f", digits = d))
      rows <- lapply(seq_len(nrow(cv)), function(i) {
        x <- cv[i, ]
        tags$tr(
          tags$td(friendly_name(x$Parameter)),
          tags$td(paste0(fmt(x$CVwR, 1), "%")),
          tags$td(fmt(x$swR, 4)),
          tags$td(if (is.na(x$CVwT)) tags$span(class = "text-muted", x$CVwT_note)
                  else paste0(fmt(x$CVwT, 1), "%")),
          tags$td(fmt(x$sw_ratio, 3)),
          tags$td(paste0(fmt(x$ABEL_lower), "\u2013", fmt(x$ABEL_upper), "%",
                         if (isTRUE(x$ABEL_widened)) "" else " (not widened)")),
          tags$td(ifelse(is.na(x$PE_within_80_125), "\u2014", x$PE_within_80_125)))
      })
      tags$div(
        class = "mt-3",
        tags$h6(class = "fw-bold", "Within-subject variability (replicate design)"),
        tags$table(
          class = "table table-sm table-striped small",
          tags$thead(tags$tr(
            tags$th("PK Parameter"), tags$th(HTML("CV<sub>wR</sub>")), tags$th(HTML("s<sub>wR</sub>")),
            tags$th(HTML("CV<sub>wT</sub>")), tags$th(HTML("s<sub>wT</sub> / s<sub>wR</sub>")),
            tags$th("Implied ABEL limits (EMA)"), tags$th("PE within 80\u2013125%"))),
          tags$tbody(rows)),
        if (any(is.na(cv$CVwT) & grepl("only once", cv$CVwT_note))) tags$div(
          class = "alert alert-warning py-2 small",
          icon("triangle-exclamation", class = "me-1"),
          tags$strong("Partial replicate: "),
          "the test treatment was given once, so its within-subject variability cannot be ",
          "estimated. The confidence interval above uses a pooled residual variance and ",
          "assumes CV", tags$sub("wT"), " \u2248 CV", tags$sub("wR"), "; if the test ",
          "formulation is more variable, the interval can be too narrow."),
        tags$div(
          class = "alert alert-secondary py-2 small",
          icon("circle-info", class = "me-1"),
          "These are the inputs to a reference-scaled assessment, estimated with the ",
          "period-adjusted reference-only model (as in the replicateBE package). The ",
          "implied limits are informational. ",
          tags$strong("This app does not issue a scaled bioequivalence verdict."),
          " For a regulatory decision use replicateBE or validated commercial software.")
      )
    })

    # Forest plot
    output$forest_plot <- renderPlotly({
      req(be_result())
      ci <- be_result()$ci_table
      # Only ratios belong on this axis; differences (TMAX, untransformed
      # parameters) are in their own units. Ratios without a verdict
      # (half-life, paired comparisons) are drawn in grey.
      ci <- ci[grepl("^Ratio", ci$Scale), ]
      if (nrow(ci) == 0) return(plotly_empty())
      ci$Label <- sapply(ci$Parameter, friendly_name)
      ci$Label <- factor(ci$Label, levels = rev(ci$Label))
      ci$Bioequivalent[!ci$Bioequivalent %in% c("YES", "NO")] <- "no verdict"
      lims <- ci[!is.na(ci$BE_Lower), c("BE_Lower", "BE_Upper")]
      # A metric without an estimate (zero values) keeps its row and says why,
      # instead of disappearing from the figure
      none <- ci[is.na(ci$Point_Est), , drop = FALSE]
      if (nrow(none) > 0) {
        nz <- rowSums(cbind(none$Zeros_Test, none$Zeros_Ref), na.rm = TRUE)
        none$note <- ifelse(nz > 0, paste0("no estimate: ", nz, " zero value(s)"), "no estimate")
      }
      ci <- ci[!is.na(ci$Point_Est), , drop = FALSE]

      p <- ggplot(ci, aes(x = Point_Est, y = Label)) +
        geom_vline(xintercept = 100, color = "grey50") +
        { if (nrow(lims) > 0) geom_vline(xintercept = c(lims$BE_Lower[1], lims$BE_Upper[1]),
                                         color = "#E74C3C", linetype = "dashed") } +
        geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper),
                      width = 0.25, linewidth = 0.8,
                      orientation = "y") +
        geom_point(aes(color = Bioequivalent), size = 4) +
        scale_color_manual(values = c("YES" = "#18BC9C", "NO" = "#E74C3C", "no verdict" = "#95A5A6")) +
        scale_y_discrete(drop = FALSE) +
        { if (nrow(none) > 0) geom_text(data = none, aes(x = 100, y = Label, label = note),
                                        inherit.aes = FALSE, size = 3.2, colour = "#7f8c8d") } +
        labs(x = paste0("Geometric Mean Ratio (%) with ", run_ci_level(), "% CI"),
             y = NULL, color = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none", panel.grid.major.y = element_blank())
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(b = 60),
               xaxis = list(title = paste0("Geometric Mean Ratio (%) with ",
                                           run_ci_level(), "% CI")))
    })
    
    # Official CDISC codes for the parameters in the table, with the release used
    output$cdisc_codes <- renderUI({
      req(be_nca_result())
      r <- be_nca_result()
      cdisc_codes_ui(names(r)[vapply(r, is.numeric, logical(1))], input$admin_route, isTRUE(input$is_ss))
    })

    # NCA table
    output$nca_table <- renderDT({
      req(be_nca_result())
      display_nca <- rename_nca_columns(be_nca_result())
      
      if (!isTRUE(input$nca_show_all)) {
        # AUCPEO included so >20% extrapolation is visible in default view
        key_cols <- intersect(
          c("Subject", "Treatment", "Period",
            "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
            "AUC to Last Point", "AUC to Infinity (observed)",
            "AUC % Extrapolated (observed)",
            "Half-Life (h)", "Apparent Clearance (CL/F)",
            "Apparent Volume (Vz/F)", "Adjusted R-squared"),
          names(display_nca))
        key_cols <- c(key_cols, unname(friendly_name(partial_auc_cols(names(be_nca_result())))))
        display_nca <- display_nca[, key_cols, drop = FALSE]
      }
      
      aucpeo_col <- "AUC % Extrapolated (observed)"
      has_aucpeo <- aucpeo_col %in% names(display_nca)
      
      dt <- datatable(display_nca, options = list(scrollX = TRUE, scrollY = "400px",
                                            pageLength = 50, dom = "frtip"),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatSignif(columns = which(sapply(display_nca, is.numeric)), digits = 4)
      
      # Amber flag when AUC extrapolation > 20% (0-100 scale from NonCompart)
      if (has_aucpeo) {
        dt <- dt %>%
          formatStyle(
            aucpeo_col,
            backgroundColor = styleInterval(20, c("transparent", "#FFF3CD")),
            color            = styleInterval(20, c("inherit",      "#7D5A00")),
            fontWeight       = styleInterval(20, c("normal",       "bold"))
          )
      }
      dt
    })
    
    # ANOVA
    output$anova_output <- renderUI({
      req(be_result())
      aov_list <- be_result()$anova
      if (length(aov_list) == 0) return(tags$p("No ANOVA tables available."))
      tagList(lapply(names(aov_list), function(param) {
        card(card_header(paste("ANOVA for", friendly_name(param))),
             card_body(renderTable({
               df <- as.data.frame(aov_list[[param]])
               df$Source <- rownames(df)
               # Rename ANOVA columns
               nm <- names(df)
               nm[nm == "Df"]      <- "df"
               nm[nm == "Sum Sq"]  <- "Sum of Squares"
               nm[nm == "Mean Sq"] <- "Mean Square"
               nm[nm == "F value"] <- "F statistic"
               nm[nm == "Pr(>F)"]  <- "p-value"
               names(df) <- nm
               df[, c("Source", setdiff(nm, "Source"))]
             }, digits = 4, striped = TRUE, hover = TRUE)))
      }))
    })
    
    # Profile plot
    output$profile_plot <- renderPlotly({
      req(shared$pk_data, shared$col_map, shared$col_map$treatment)
      d <- shared$pk_data; cm <- shared$col_map
      d <- d[!is.na(d[[cm$conc]]) & d[[cm$conc]] > 0, ]
      if (nrow(d) == 0) return(plotly_empty())
      p <- ggplot(d, aes(x = .data[[cm$time]], y = .data[[cm$conc]],
                         color = factor(.data[[cm$treatment]]),
                         group = interaction(.data[[cm$subject]], .data[[cm$treatment]]))) +
        geom_line(alpha = 0.4) + geom_point(alpha = 0.5, size = 1.5) +
        scale_y_log10() + scale_color_brewer(palette = "Set1") +
        labs(x = "Time", y = "Concentration (log)", color = "Treatment") +
        theme_minimal(base_size = 11) + theme(legend.position = "bottom")
      ggplotly(p) %>% layout(legend = list(orientation="h", y=-0.15))
    })
    
    # --- Individual Profiles Grid (per subject, coloured by treatment) ---
    observe({
      req(shared$pk_data, shared$col_map)
      subjects <- sort(unique(shared$pk_data[[shared$col_map$subject]]))
      n <- length(subjects)
      if (n <= 12) {
        choices <- list("All subjects" = "all")
      } else {
        pages <- split(subjects, ceiling(seq_along(subjects) / 12))
        choices <- c(list("All subjects (may be slow)" = "all"),
                     setNames(seq_along(pages),
                              sapply(pages, function(pg) paste0(pg[1], " \u2013 ", pg[length(pg)]))))
      }
      updateSelectInput(session, "be_grid_page", choices = choices,
                        selected = if (n <= 12) "all" else "1")
    })
    
    output$be_grid_plot <- renderPlotly({
      req(shared$pk_data, shared$col_map, shared$col_map$treatment)
      d <- shared$pk_data; cm <- shared$col_map
      d <- d[!is.na(d[[cm$conc]]) & d[[cm$conc]] > 0, ]
      if (nrow(d) == 0) return(plotly_empty())
      subjects <- sort(unique(d[[cm$subject]]))
      
      sel <- input$be_grid_page
      if (!is.null(sel) && sel != "all") {
        pages <- split(subjects, ceiling(seq_along(subjects) / 12))
        idx <- as.integer(sel)
        if (!is.na(idx) && idx <= length(pages)) subjects <- pages[[idx]]
      } else if (length(subjects) > 36) {
        subjects <- subjects[1:36]
      }
      
      sub_d <- d[d[[cm$subject]] %in% subjects, ]
      tryCatch({
        p <- ggplot(sub_d, aes(x = .data[[cm$time]], y = .data[[cm$conc]],
                               color = factor(.data[[cm$treatment]]))) +
          geom_line(linewidth = 0.5) +
          geom_point(size = 1.5) +
          facet_wrap(reformulate(cm$subject), scales = "free_y") +
          scale_y_log10() +
          scale_color_brewer(palette = "Set1") +
          theme_minimal(base_size = 9) +
          labs(x = "Time", y = "Concentration (log)", color = "Treatment") +
          theme(legend.position = "bottom")
        ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.1))
      }, error = function(e) plotly_empty())
    })
    
    # --- Half-Life Review for BE ---
    lz_state <- reactiveValues(override = NULL, overrides_log = list())
    # Overrides belong to one data set: clear them when new data are processed,
    # so they can never be applied to matching profiles of a different file.
    observeEvent(shared$pk_data, {
      lz_state$overrides_log <- list()
      lz_state$override <- NULL
      # Results of the previous file must not be shown or exported with the new one
      be_result(NULL); be_nca_result(NULL); be_run_settings(NULL); balance_result(NULL)
    }, ignoreNULL = FALSE)
    
    # Update profile selector after NCA runs
    observe({
      req(be_nca_result())
      r <- be_nca_result()
      # One entry per profile: subject | treatment | period (as mapped)
      updateSelectInput(session, "lz_profile", choices = result_profile_labels(r))
    })
    
    # Reset override when profile changes
    observeEvent(input$lz_profile, { lz_state$override <- NULL }, ignoreInit = TRUE)
    
    # Get data for selected profile
    lz_sub_data <- reactive({
      req(input$lz_profile, shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map; sel <- input$lz_profile
      sub_d <- d[profile_data_rows(d, cm, sel), ]
      list(time = sub_d[[cm$time]], conc = sub_d[[cm$conc]])
    })
    
    # Half-life status
    output$lz_status <- renderUI({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      lz <- if (!is.null(lz_state$override)) lz_state$override
            else estimate_lambda_z(sd$time, sd$conc, input$r2adj_be, route = input$admin_route)
      if (is.na(lz$lambda_z)) {
        badge <- tags$span(class = "badge bg-warning", "Not estimable")
        return(tags$div(tags$small("Half-life could not be estimated"), badge))
      }
      badge <- if (!is.null(lz_state$override))
        tags$span(class = "badge bg-info ms-1", "User-adjusted") else NULL
      tags$div(
        tags$small(paste0("Half-life: ", signif(lz$half_life, 4), " h | R\u00B2: ",
                          if (!is.na(lz$r2adj)) signif(lz$r2adj, 4) else "N/A",
                          " | ", lz$n_points, " points")),
        badge)
    })
    
    # Half-life plot
    output$lz_plot <- renderPlotly({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      tryCatch({
        lz <- if (!is.null(lz_state$override)) lz_state$override
              else estimate_lambda_z(sd$time, sd$conc, input$r2adj_be, route = input$admin_route)
        df <- data.frame(Time = sd$time,
                         ln_Conc = ifelse(sd$conc > 0, log(sd$conc), NA),
                         Conc = sd$conc, used = FALSE)
        if (length(lz$time_used) > 0) {
          for (i in seq_along(lz$time_used)) {
            m <- which(abs(sd$time - lz$time_used[i]) < 1e-10)
            if (length(m) > 0) df$used[m[1]] <- TRUE
          }
        }
        df$Status <- ifelse(df$used, "Used for half-life", "Not used")
        df <- df[!is.na(df$ln_Conc), ]
        df$tooltip <- paste0("Time: ", round(df$Time, 2), " h\n",
                             "Conc: ", signif(df$Conc, 4), "\n",
                             "ln(Conc): ", round(df$ln_Conc, 3))
        p <- ggplot(df, aes(x = Time, y = ln_Conc, color = Status, text = tooltip)) +
          geom_point(size = 3.5, alpha = 0.85) +
          scale_color_manual(values = c("Used for half-life" = "#E74C3C",
                                        "Not used" = "#BDC3C7")) +
          theme_minimal(base_size = 11) +
          labs(x = "Time", y = "ln(Concentration)") +
          theme(legend.position = "none")
        if (!is.na(lz$lambda_z)) {
          tr <- range(lz$time_used)
          tp <- seq(tr[1], tr[2] * 1.05, length.out = 30)
          line_df <- data.frame(Time = tp, ln_Conc = lz$intercept - lz$lambda_z * tp)
          p <- p + geom_line(data = line_df, aes(x = Time, y = ln_Conc),
                             inherit.aes = FALSE, color = "#E74C3C",
                             linetype = "dashed", linewidth = 0.7)
        }
        ggplotly(p, tooltip = "text") %>% layout(margin = list(b = 40))
      }, error = function(e) plotly_empty())
    })
    
    # Populate checkboxes
    observe({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      valid <- !is.na(sd$conc) & sd$conc > 0
      cmax_t <- sd$time[which.max(sd$conc)]
      term <- valid & sd$time > cmax_t
      if (any(term)) {
        term_idx <- which(term)
        ch <- setNames(
          as.character(term_idx),
          paste0("t=", round(sd$time[term_idx], 2), "  C=", round(sd$conc[term_idx], 3))
        )
        lz <- if (!is.null(lz_state$override)) lz_state$override
              else estimate_lambda_z(sd$time, sd$conc, input$r2adj_be, route = input$admin_route)
        sel <- if (length(lz$time_used) > 0) {
          as.character(term_idx[sd$time[term_idx] %in% lz$time_used])
        } else NULL
        updateCheckboxGroupInput(session, "lz_points", choices = ch,
                                 selected = sel, inline = TRUE)
      }
    })
    
    # Recalculate handler
    observeEvent(input$lz_recalc, {
      sd <- lz_sub_data(); req(length(sd$time) >= 2)
      sel_idx <- as.integer(input$lz_points)
      
      # Shared computation via helper
      lz_calc <- recalculate_lambda_z(sd$time, sd$conc, sel_idx)
      
      if (!is.null(lz_calc$error)) {
        showNotification(lz_calc$error, type = "error", duration = 10)
        return()
      }
      if (!is.null(lz_calc$warning)) {
        showNotification(lz_calc$warning, type = "warning", duration = 8)
      }
      
      override <- lz_calc$result
      lz_new  <- override$lambda_z
      hl_new  <- override$half_life
      r2adj   <- override$r2adj
      n_pts   <- override$n_points
      t_sel   <- override$time_used
      
      # Get original λz for audit logging
      orig_lz <- estimate_lambda_z(sd$time, sd$conc, input$r2adj_be, route = input$admin_route)
      
      lz_state$override <- override
      
      # Log the override for audit trail
      sel <- input$lz_profile
      lz_state$overrides_log[[sel]] <- c(list(
        profile = sel),
        # Subject / treatment / period, so the reproduction script can replay
        # the override on exactly this administration
        profile_parts(be_nca_result(), sel), list(
        original_lambda_z = if (!is.na(orig_lz$lambda_z)) as.numeric(orig_lz$lambda_z) else NA,
        adjusted_lambda_z = as.numeric(lz_new),
        original_r2adj = if (!is.na(orig_lz$r2adj)) as.numeric(orig_lz$r2adj) else NA,
        adjusted_r2adj = if (!is.na(r2adj)) as.numeric(r2adj) else NA,
        points_used = length(t_sel),
        time_used = as.numeric(t_sel)
      ))
      
      # Recompute with every logged override applied. NonCompart fits the chosen
      # points and derives all dependent parameters (AUCinf, CL, V, MRT, ...)
      # with its own unit conversions, exactly as the reproduction script does.
      settings <- be_nca_settings()
      if (!is.null(settings)) {
        r <- suppressWarnings(run_nca(shared$pk_data, shared$col_map, settings,
                                      lz_overrides = lz_state$overrides_log))
        if (!is.null(r)) {
          be_nca_result(r)
          shared$nca_results <- r
        }
      }
      
      showNotification(
        paste0("Recalculated: t\u00BD = ", signif(hl_new, 4), " h (",
               if (!is.na(r2adj)) paste0("R\u00B2 = ", signif(r2adj, 4)) else "R\u00B2 = N/A",
               ", ", n_pts, " pts). Click 'Run Complete BE Analysis' again to update confidence intervals."),
        type = "message", duration = 10)
    })
    
    # Downloads
    output$dl_be_xlsx <- downloadHandler(
      filename = function() paste0("BE_report_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(be_result())
        wb <- createWorkbook()
        addWorksheet(wb, "Confidence_Intervals")
        writeData(wb, 1, rename_be_columns(be_result()$ci_table, ci_level = run_ci_level()))
        if (!is.null(be_nca_result())) {
          addWorksheet(wb, "NCA_Parameters")
          writeData(wb, 2, rename_nca_columns(be_nca_result()))
        }
        if (!is.null(be_nca_result())) {
          r <- be_nca_result()
          add_cdisc_code_sheet(wb, names(r)[vapply(r, is.numeric, logical(1))],
                               input$admin_route, isTRUE(input$is_ss))
        }
        if (!is.null(be_result()$cv_table)) {
          addWorksheet(wb, "Within_Subject_Variability")
          writeData(wb, "Within_Subject_Variability", be_result()$cv_table)
        }
        for (p in names(be_result()$anova)) {
          sn <- substr(paste0("ANOVA_", friendly_name(p)), 1, 31)
          addWorksheet(wb, sn)
          df <- as.data.frame(be_result()$anova[[p]])
          df$Source <- rownames(df); writeData(wb, sn, df)
        }
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    output$dl_ci_csv <- downloadHandler(
      filename = function() paste0("BE_CI_table_", Sys.Date(), ".csv"),
      content = function(file) {
        req(be_result())
        write.csv(rename_be_columns(be_result()$ci_table, ci_level = run_ci_level()), file, row.names=FALSE)
      }
    )
    
    # Analysis Record panel — appears once the BE comparison has run
    output$record_panel <- renderUI({
      req(be_result())
      analysis_record_ui(
        session$ns,
        intro = paste0(
          "Self-contained package with NCA results, the bioequivalence results ",
          "(ANOVA tables and confidence intervals), every setting, a standalone ",
          "R reproducibility script, a SHA-256 data-integrity hash, and an HTML summary."))
    })

    # Complete Analysis Record
    output$dl_record <- downloadHandler(
      filename = function() {
        study <- if (nchar(input$record_study) > 0)
          gsub("[^A-Za-z0-9_-]", "_", input$record_study) else "BE"
        paste0("Analysis_Record_", study, "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(be_nca_result(), be_result(), be_run_settings(), shared$col_map, shared$study_info)
        
        withProgress(message = "Generating analysis record...", value = 0.3, {
          r <- be_nca_result()
          
          run <- be_run_settings()
          settings <- run$nca
          settings$n_obs <- nrow(shared$pk_data)
          
          si <- shared$study_info
          original_name <- si$file_name
          original_path <- si$file_path
          read_args <- si$read_args
          adnca_rec <- if (identical(si$door, "adnca")) si$adnca else NULL
          
          if (is.null(original_path) || !file.exists(original_path)) {
            original_path <- file.path(tempdir(), original_name)
            if (!is.null(shared$raw_data))
              read_args <- write_record_fallback(shared$raw_data, original_path, read_args)
            else read_args <- list()
            adnca_rec <- NULL
          }
          
          setProgress(0.6, message = "Building R script and summary...")
          
          rec_out <- create_analysis_record(
            output_path    = file,
            results        = r,
            settings       = settings,
            col_map        = shared$col_map,
            original_file_path = original_path,
            original_file_name = original_name,
            blq_rule       = si$blq_rule,
            lloq           = si$lloq,
            analyst        = if (nchar(input$record_analyst) > 0) input$record_analyst else "Analyst",
            study_name     = if (nchar(input$record_study) > 0) input$record_study else "Untitled Study",
            be_results     = be_result(),
            be_settings    = run$be,
            lz_overrides   = if (length(lz_state$overrides_log) > 0) lz_state$overrides_log else NULL,
            viz_settings   = shared$viz_settings,
            read_args      = read_args,
            adnca          = adnca_rec
          )
          notify_reproduction(rec_out)
        })
      }
    )
  })
}
