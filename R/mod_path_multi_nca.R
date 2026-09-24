# ============================================================================
# NCA Assistant — Path 4: Analyze All Subjects (Batch NCA)
# ============================================================================

path_multi_nca_ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container-fluid py-3",
    style = "max-width: 1300px; margin: 0 auto;",
    
    tags$h4(class = "fw-bold mb-1",
            icon("users", class = "text-primary me-2"),
            "Analyze All Subjects — Batch NCA"),
    tags$p(class = "text-muted mb-3",
           "Run PK analysis on all subjects at once. Get individual parameters, ",
           "population summary statistics, and exportable results."),
    
    uiOutput(ns("data_gate")),
    
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("data_ok")),
      
      layout_sidebar(
        fillable = FALSE,
        
        sidebar = sidebar(
          title = tagList("NCA Settings", help_what_is_nca),
          width = 340, open = TRUE,
          
          card(
            card_header(class = "bg-primary text-white", "Administration"),
            card_body(
              selectInput(ns("admin_route"),
                          tagList("How was the drug given?", help_admin_route),
                          choices = c("Oral / IM / SC (extravascular)" = "extravascular",
                                      "IV Bolus (injected into vein at once)" = "iv_bolus",
                                      "IV Infusion (drip over time)" = "iv_infusion")),
              
              # Dose source selector
              radioButtons(ns("dose_source"), "Dose information",
                           choices = c(
                             "Same dose for all subjects" = "single",
                             "Doses differ by subject or period (from Dose column in data)" = "from_data"
                           ), selected = "single"),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'single'", ns("dose_source")),
                numericInput(ns("dose"), "Dose given to each subject",
                             value = 100, min = 0)
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'from_data'", ns("dose_source")),
                uiOutput(ns("dose_column_status"))
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'iv_infusion'", ns("admin_route")),
                numericInput(ns("inf_dur"), "Infusion duration", value = 0, min = 0)),
              checkboxInput(ns("is_ss"),
                            tagList("Steady-state (drug given repeatedly)", help_steady_state),
                            value = FALSE),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("is_ss")),
                numericInput(ns("tau"), "Dosing interval \u03C4 (same unit as Time)", value = NA, min = 0))
            )
          ),
          
          card(
            card_header(class = "bg-primary text-white", "Units"),
            card_body(
              layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("dose_unit"), "Dose unit", choices = DOSE_UNIT_CHOICES, selected = "mg"),
                selectInput(ns("time_unit"), "Time unit", choices = TIME_UNIT_CHOICES, selected = "h"),
                selectInput(ns("conc_unit"), "Conc unit", choices = CONC_UNIT_CHOICES, selected = "ng/mL")
              ),
              numericInput(ns("mw"), "Molecular weight (only for molar units)",
                           value = 0, min = 0, step = 1)
            )
          ),
          
          card(
            card_header(class = "bg-primary text-white", "Calculation Method"),
            card_body(
              selectInput(ns("trap_method"),
                          tagList("Trapezoidal method", help_trapezoidal),
                          choices = c("Linear-up / Log-down" = "log",
                                      "Linear-up / Linear-down" = "linear")),
              sliderInput(ns("r2adj"),
                          tagList("Minimum R² for half-life estimation", help_r2adj),
                          min = 0, max = 1, value = 0.7, step = 0.05),
              checkboxInput(ns("dose_norm"),
                            tagList("Calculate dose-normalized parameters", help_dose_norm),
                            FALSE)
            )
          ),

          partial_auc_ui(ns("pauc")),
          
          hr(),
          actionButton(ns("run_nca"), "Run Analysis on All Subjects",
                       class = "btn-success btn-lg w-100",
                       icon = icon("play"))
        ),
        
        # --- Main content ----------------------------------------------------
        tagList(
          uiOutput(ns("result_status")),
          uiOutput(ns("excl_note")),
          uiOutput(ns("pauc_note")),
          
          navset_card_tab(
            title = "Results",
            
            # Profiles
            nav_panel(
              "Concentration-Time Profiles",
              icon = icon("chart-line"),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header("All Subjects (spaghetti plot)"),
                  plotlyOutput(ns("spaghetti_plot"), height = "380px")
                ),
                card(
                  card_header("Mean ± SD"),
                  plotlyOutput(ns("mean_plot"), height = "380px")
                )
              )
            ),
            
            # Parameter table
            nav_panel(
              "All Parameters (per profile)",
              icon = icon("table"),
              tags$p(class = "text-muted small",
                     "One row per profile (subject, and treatment and period when mapped). ",
                     "Key PK parameters shown. Tick the box below for all 37 parameters, ",
                     "or download the full table as Excel."),
              checkboxInput(ns("show_all_params"),
                            "Show all parameters (37 columns)", FALSE),
              DTOutput(ns("param_table")),
              uiOutput(ns("cdisc_codes")),
              downloadButton(ns("dl_params_csv"), "Download Results (CSV)",
                             class = "btn-outline-primary btn-sm mt-2"),
              downloadButton(ns("dl_params_xlsx"), "Download Results (Excel)",
                             class = "btn-outline-success btn-sm mt-2")
            ),
            
            # Summary stats
            nav_panel(
              "Summary Statistics",
              icon = icon("chart-bar"),
              tags$p(class = "text-muted small",
                     "Mean, SD, CV%, median, range, geometric mean, and geometric CV% ",
                     "for key PK parameters, per treatment when a Treatment column is mapped."),
              uiOutput(ns("ss_note")),
              uiOutput(ns("replicate_note")),
              DTOutput(ns("summary_table")),
              hr(),
              plotlyOutput(ns("boxplot"), height = "350px")
            ),
            
            # Individual grid
            nav_panel(
              "Individual Profiles (grid view)",
              icon = icon("grip"),
              tags$p(class = "text-muted small",
                     "One panel per subject on log scale. Check for consistent ",
                     "terminal slopes and unusual profiles."),
              layout_columns(
                col_widths = c(4, 8),
                selectInput(ns("grid_page"), "Subjects shown",
                            choices = "All", selected = "All"),
                tags$span()
              ),
              plotlyOutput(ns("grid_plot"), height = "700px")
            ),
            
            # Half-life review
            nav_panel(
              tagList("Half-Life Review", help_lambda_z),
              icon = icon("magnifying-glass-chart"),
              layout_columns(
                col_widths = c(4, 8),
                card(
                  card_header("Select a Profile"),
                  card_body(
                    selectInput(ns("lz_profile"), "Profile:", choices = NULL),
                    uiOutput(ns("lz_info")),
                    hr(),
                    tags$p(class = "text-muted small",
                           tags$span(class = "fw-bold", style = "color: #E74C3C;", "\u25CF"),
                           " = used for half-life \u00A0\u00A0",
                           tags$span(class = "fw-bold", style = "color: #BDC3C7;", "\u25CF"),
                           " = not used"),
                    tags$p(class = "text-muted small",
                           "Uncheck points that don't belong on the straight line."),
                    checkboxGroupInput(ns("lz_points"), "Points for half-life:",
                                       choices = NULL),
                    actionButton(ns("lz_recalc"), "Recalculate",
                                 class = "btn-outline-primary btn-sm w-100",
                                 icon = icon("refresh"))
                  )
                ),
                card(
                  card_header("Terminal Slope (log scale)"),
                  plotlyOutput(ns("lz_plot"), height = "400px")
                )
              )
            )
          ),

          # Analysis Record — consistent panel below the results, visible
          # regardless of which results sub-tab is open (appears once NCA runs).
          uiOutput(ns("record_panel"))
        )
      )
    )
  )
}

path_multi_nca_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Pre-select the units stated in the file
    observeEvent(shared$study_info, {
      u <- shared$study_info$units
      for (k in c("conc", "time", "dose")) {
        v <- u[[k]]$unit
        if (length(v) == 1 && !is.na(v)) updateSelectInput(session, paste0(k, "_unit"), selected = v)
      }
    })
    
    lz_state <- reactiveValues(override = NULL, overrides_log = list())
    # Overrides belong to one data set: clear them when new data are processed,
    # so they can never be applied to matching profiles of a different file.
    observeEvent(shared$pk_data, {
      lz_state$overrides_log <- list()
      lz_state$override <- NULL
      # Results of the previous file must not be shown or exported with the new one
      nca_result(NULL)
    }, ignoreNULL = FALSE)
    
    # Reset override when profile changes
    observeEvent(input$lz_profile, { lz_state$override <- NULL }, ignoreInit = TRUE)
    
    output$data_ok <- reactive({ shared$data_ready })
    outputOptions(output, "data_ok", suspendWhenHidden = FALSE)
    
    # Auto-select "from_data" when a Dose column is mapped
    observe({
      if (shared$data_ready && !is.null(shared$col_map$dose)) {
        updateRadioButtons(session, "dose_source", selected = "from_data")
      }
    })
    
    # Dose column status — shows summary when "from data" is selected
    output$dose_column_status <- renderUI({
      if (!shared$data_ready) {
        return(tags$div(class = "alert alert-warning py-2 small",
                        icon("triangle-exclamation"),
                        " Upload data first and map a Dose column."))
      }
      
      if (is.null(shared$col_map$dose)) {
        return(tags$div(class = "alert alert-warning py-2 small",
                        icon("triangle-exclamation"),
                        " No Dose column mapped. Go to Upload & Check Data ",
                        "and map the Dose column, or switch to 'Same dose for all'."))
      }
      
      # Show dose summary from data
      dose_vals <- shared$pk_data[[shared$col_map$dose]]
      dose_by_subj <- tapply(dose_vals, shared$pk_data[[shared$col_map$subject]],
                             function(x) max(x, na.rm = TRUE))
      unique_doses <- sort(unique(dose_by_subj))
      
      tags$div(
        class = "alert alert-success py-2 small",
        icon("circle-check"),
        paste0(" Dose column '", shared$col_map$dose, "' found. "),
        if (length(unique_doses) == 1) {
          paste0("All subjects received ", unique_doses, " ",
                 input$dose_unit, ".")
        } else {
          paste0(length(unique_doses), " different doses: ",
                 paste(unique_doses, collapse = ", "), " ", input$dose_unit,
                 " (", length(dose_by_subj), " subjects).")
        }
      )
    })
    
    output$data_gate <- renderUI({
      if (!shared$data_ready) {
        card(class = "border-warning",
             card_body(class = "text-center py-4",
                       icon("triangle-exclamation", class = "fa-2x text-warning mb-2"),
                       tags$h5("No data loaded yet"),
                       actionButton(ns("goto_upload"), "Go to Upload & Check Data",
                                    class = "btn-warning",
                                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});")))
      }
    })
    
    nca_result    <- reactiveVal(NULL)
    nca_excl_note <- reactiveVal(NULL)  # persists degenerate-profile exclusion warnings
    pauc_spec     <- partial_auc_server("pauc")
    pauc_notes    <- reactiveVal(character(0))

    clear_result_on_change(
      reactive(list(input$admin_route, input$dose, input$inf_dur, input$is_ss, input$tau,
                    input$dose_unit, input$time_unit, input$conc_unit, input$trap_method,
                    input$r2adj, input$mw, input$dose_norm, input$dose_source, pauc_spec())),
      has_result = function() !is.null(nca_result()), clear = function() nca_result(NULL),
      id = "multi_nca_stale")
    
    # Run NCA
    observeEvent(input$run_nca, {
      req(shared$pk_data, shared$col_map)
      
      # Determine dose
      use_data_dose <- (input$dose_source == "from_data" &&
                        !is.null(shared$col_map$dose))
      
      if (!use_data_dose) {
        if (is.null(input$dose) || is.na(input$dose) || input$dose <= 0) {
          showNotification("Please enter a valid dose (greater than 0).",
                           type = "error", duration = 5)
          return()
        }
      }
      
      if (input$admin_route == "iv_infusion" &&
          (is.null(input$inf_dur) || is.na(input$inf_dur) || input$inf_dur <= 0)) {
        showNotification("Please enter the infusion duration (greater than 0) for IV infusion.",
                         type = "error", duration = 5)
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
      settings <- list(
        admin_route       = input$admin_route,
        dose              = if (use_data_dose) NA else input$dose,
        infusion_duration = ifelse(input$admin_route == "iv_infusion",
                                   input$inf_dur, 0),
        is_steady_state   = input$is_ss,
        tau               = input$tau,
        dose_unit         = input$dose_unit,
        time_unit         = input$time_unit,
        conc_unit         = input$conc_unit,
        trap_method       = input$trap_method,
        r2adj_threshold   = input$r2adj,
        mw = input$mw,
        partial_aucs = pauc_spec()
      )
      
      # Units drive a real conversion factor for CL/F and Vz/F inside NonCompart,
      # and an unrecognised spelling makes the NCA fail with an opaque message.
      # Check the combination before running so the user gets a usable error.
      uchk <- validate_units(input$dose_unit, input$time_unit, input$conc_unit, input$mw)
      if (!uchk$valid) {
        showNotification(uchk$message, type = "error", duration = 12)
        return()
      }
      umsg <- check_units_against_data(shared$study_info$units, input$dose_unit, input$time_unit, input$conc_unit)
      if (!is.null(umsg)) {
        showNotification(umsg, type = "error", duration = 12)
        return()
      }

      if (use_data_dose) {
        dose_vec <- suppressWarnings(dose_by_profile(shared$pk_data, shared$col_map))
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
      
      withProgress(message = "Running NCA on all subjects...", value = 0.5, {
        # Capture any warnings from run_nca (e.g. degenerate profiles excluded)
        nca_warnings <- character(0)
        result <- withCallingHandlers(
          run_nca(shared$pk_data, shared$col_map, settings,
                  lz_overrides = lz_state$overrides_log),
          warning = function(w) {
            nca_warnings <<- c(nca_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        
        if (is.null(result)) {
          detail <- if (length(nca_warnings) > 0) paste(nca_warnings, collapse = " ")
                    else "Check the data and column mapping."
          showNotification(paste0("NCA failed. ", detail), type = "error", duration = NULL)
          return()
        }
        
        # Partial AUC notes have their own alert
        is_pauc <- startsWith(nca_warnings, "Partial AUC")
        pauc_notes(nca_warnings[is_pauc])
        nca_warnings <- nca_warnings[!is_pauc]
        if (any(is_pauc))
          showNotification("Partial AUCs: see the notes above the results.", type = "warning", duration = 8)

        # Store and surface any degenerate-profile exclusions
        if (length(nca_warnings) > 0) {
          nca_excl_note(nca_warnings)  # persists as alert in results panel
          showNotification(
            paste0("Note: ", paste(nca_warnings, collapse = "; ")),
            type = "warning", duration = 12)
        } else {
          nca_excl_note(NULL)  # clear from previous run
        }
        
        if (input$dose_norm) {
          # add_dose_normalized() matches a per-subject dose by subject ID
          result <- add_dose_normalized(as.data.frame(result), settings$dose)
        }
        
        nca_result(result)
        shared$nca_results  <- result
        shared$nca_settings <- settings
        shared$partial_aucs <- settings$partial_aucs
        gc()  # Free NCA intermediates
      })
      
      # Update half-life profile selector
      # One entry per profile: subject | treatment | period (as mapped)
      updateSelectInput(session, "lz_profile", choices = result_profile_labels(result))
      
      showNotification(paste("NCA complete:", nrow(result), "profiles analyzed."),
                       type = "message")
    })
    
    # Steady-state note
    output$ss_note <- renderUI({
      if (isTRUE(input$is_ss) && !is.null(nca_result())) {
        tags$div(
          class = "alert alert-info py-2 small",
          icon("circle-info", class = "me-1"),
          tags$strong("Steady-state analysis: "),
          "AUC\u03C4 is the AUC from 0 to the dosing interval you entered (extrapolated with ",
          "\u03BBz when the last sample is before \u03C4). CL/F and Vz/F are calculated from AUC\u03C4; ",
          "average concentration is AUC\u03C4/\u03C4. AUC to infinity is not meaningful during ",
          "repeated dosing and is hidden from the default view (available under 'Show all parameters')."
        )
      }
    })
    
    # Status
    # Persistent excluded-profiles alert
    output$excl_note <- renderUI({
      msgs <- nca_excl_note()
      if (is.null(msgs) || length(msgs) == 0) return(NULL)
      tags$div(
        class = "alert alert-warning py-2 small mb-2",
        icon("triangle-exclamation", class = "me-1"),
        tags$strong("Profiles excluded from analysis: "),
        paste(msgs, collapse = " | "),
        tags$br(),
        tags$span(class = "text-muted",
                  "These profiles had fewer than 2 positive concentration values (no meaningful NCA output possible). ",
                  "Check the raw data for these subjects/treatments.")
      )
    })

    output$pauc_note <- renderUI({
      req(nca_result())
      partial_auc_notes_ui(pauc_notes())
    })

    output$result_status <- renderUI({
      if (is.null(nca_result())) {
        card(card_body(class = "text-center py-4 text-muted",
                       icon("flask", class = "fa-2x mb-2"),
                       tags$h5("Set your analysis parameters and click 'Run Analysis'")))
      }
    })
    
    # Spaghetti plot
    output$spaghetti_plot <- renderPlotly({
      req(shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map
      d <- d[!is.na(d[[cm$conc]]) & d[[cm$conc]] > 0, ]
      if (nrow(d) == 0) return(plotly_empty())
      d$.profile <- profile_group(d, cm)
      p <- ggplot(d, aes(x = .data[[cm$time]], y = .data[[cm$conc]], group = .profile)) +
        geom_line(alpha = 0.4, color = "#3498DB") +
        scale_y_log10() +
        labs(x = "Time", y = "Concentration (log)") +
        theme_minimal(base_size = 11)
      ggplotly(p)
    })
    
    # Mean plot
    output$mean_plot <- renderPlotly({
      req(shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map
      d <- d[!is.na(d[[cm$conc]]) & !is.na(d[[cm$time]]), ]
      if (nrow(d) == 0) return(plotly_empty())
      # Treatments are averaged separately, never pooled
      d$.trt <- if (!is.null(cm$treatment) && cm$treatment %in% names(d)) as.character(d[[cm$treatment]]) else "All"
      summ <- d %>%
        group_by(.trt, .data[[cm$time]]) %>%
        summarize(mean_c = mean(.data[[cm$conc]], na.rm = TRUE),
                  sd_c = sd(.data[[cm$conc]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(summ, aes(x = .data[[cm$time]], y = mean_c, colour = .trt, group = .trt)) +
        geom_errorbar(aes(ymin = pmax(0, mean_c - sd_c), ymax = mean_c + sd_c),
                      width = 0.3, alpha = 0.5) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2.5) +
        labs(colour = NULL) +
        labs(x = "Time", y = "Mean ± SD") +
        theme_minimal(base_size = 11)
      ggplotly(p)
    })
    
    # Parameter table
    output$param_table <- renderDT({
      req(nca_result())
      display_df <- rename_nca_columns(nca_result())
      
      if (!isTRUE(input$show_all_params)) {
        if (isTRUE(input$is_ss)) {
          key_cols <- intersect(
            c("Subject", "Treatment", "Period",
              "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
              "AUC Within Dosing Interval", "Average Concentration (Cavg)",
              "Trough Concentration (Cmin)", "Peak-Trough Fluctuation (%)",
              "Half-Life (h)", "Apparent Clearance (CL/F)",
              "Adjusted R-squared"),
            names(display_df))
        } else {
          # AUCPEO included so the >20% extrapolation flag is always visible
          key_cols <- intersect(
            c("Subject", "Treatment", "Period",
              "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
              "AUC to Last Point", "AUC to Infinity (observed)",
              "AUC % Extrapolated (observed)",
              "Half-Life (h)", "Elimination Rate Constant",
              "Points Used for Half-Life",
              "Apparent Clearance (CL/F)",
              "Apparent Volume (Vz/F)", "Adjusted R-squared"),
            names(display_df))
        }
        key_cols <- c(key_cols, unname(friendly_name(partial_auc_cols(names(nca_result())))))
        display_df <- display_df[, key_cols, drop = FALSE]
      }
      # Append units to parameter column headers. This must follow the column
      # selection above, which matches on the plain labels.
      names(display_df) <- add_units_to_labels(names(display_df),
        dose_unit = input$dose_unit, time_unit = input$time_unit,
        conc_unit = input$conc_unit)
      
      aucpeo_col <- "AUC % Extrapolated (observed)"
      has_aucpeo <- aucpeo_col %in% names(display_df)
      
      dt <- datatable(display_df,
                options = list(scrollX = TRUE, scrollY = "400px",
                               pageLength = 50, dom = "frtip"),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatSignif(columns = which(sapply(display_df, is.numeric)), digits = 4)
      
      # Highlight AUCPEO > 20% in amber. Values are on 0-100 scale (percentage).
      # Amber flag prompts lambda-z review but does not block analysis.
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
    
    # Official CDISC codes for the parameters in the table, with the release used
    output$cdisc_codes <- renderUI({
      req(nca_result())
      r <- nca_result()
      cdisc_codes_ui(names(r)[vapply(r, is.numeric, logical(1))], input$admin_route, isTRUE(input$is_ss))
    })

    # Replicate designs: a subject contributes more than one profile per
    # treatment, so the summary pools administrations. Say so, because readers
    # take N as subjects and geometric CV as between-subject variability.
    output$replicate_note <- renderUI({
      req(nca_result())
      r <- nca_result()
      if (!all(c("Subject", "Treatment", "Period") %in% names(r))) return(NULL)
      if (!anyDuplicated(paste(r$Subject, r$Treatment, sep = "||"))) return(NULL)
      tags$div(
        class = "alert alert-info py-2 small mb-2",
        icon("circle-info", class = "me-1"),
        tags$strong("Replicate design: "),
        "subjects received a treatment more than once. These statistics pool all ",
        "administrations: N counts profiles, not subjects, and the geometric CV combines ",
        "within- and between-subject variability. For within-subject variability of the ",
        "reference (CV", tags$sub("wR"), "), use the Bioequivalence path.")
    })

    # Summary stats
    output$summary_table <- renderDT({
      req(nca_result())
      r <- nca_result()
      
      if (isTRUE(input$is_ss)) {
        # At steady state: AUC over the dosing interval and its derived parameters
        key <- intersect(c("CMAX","TMAX","AUCTAU","CAVG","CMIN_SS","FLUCTP","LAMZHL","CLFO"), names(r))
      } else {
        key <- intersect(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","LAMZ","CLFO","VZFO"), names(r))
      }
      key <- c(key, partial_auc_cols(names(r)))
      if (length(key) == 0) return(NULL)
      group <- if ("Treatment" %in% names(r)) "Treatment" else NULL
      summ <- summarize_pk_params(r, key, group_col = group)
      summ <- rename_summary_columns(summ)
      
      
      datatable(summ, options = list(scrollX = TRUE, dom = "t"),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatSignif(columns = 3:ncol(summ), digits = 4)
    })
    
    # Boxplot
    output$boxplot <- renderPlotly({
      req(nca_result())
      r <- nca_result()
      key <- intersect(c("CMAX","AUCLST","AUCIFO","LAMZHL"), names(r))
      if (length(key) == 0) return(plotly_empty())
      has_trt <- "Treatment" %in% names(r)
      id_cols <- if (has_trt) c("Treatment") else character(0)
      long <- r %>%
        select(all_of(c(id_cols, key))) %>%
        pivot_longer(all_of(key), names_to = "Parameter", values_to = "Value") %>%
        mutate(Value = as.numeric(Value),
               Parameter = sapply(Parameter, friendly_name)) %>%
        filter(!is.na(Value))
      if (has_trt) {
        p <- ggplot(long, aes(x = Treatment, y = Value, fill = Treatment)) +
          geom_boxplot(alpha = 0.6) +
          geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
          facet_wrap(~Parameter, scales = "free", nrow = 1) +
          theme_minimal(base_size = 11)
      } else {
        p <- ggplot(long, aes(x = Parameter, y = Value)) +
          geom_boxplot(fill = "#3498DB", alpha = 0.6) +
          geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
          facet_wrap(~Parameter, scales = "free", nrow = 1) +
          theme_minimal(base_size = 11) +
          theme(axis.text.x = element_blank())
      }
      ggplotly(p)
    })
    
    # Individual grid — update page selector when data changes
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
      updateSelectInput(session, "grid_page", choices = choices,
                        selected = if (n <= 12) "all" else "1")
    })
    
    # Individual grid — plotly
    output$grid_plot <- renderPlotly({
      req(shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map
      d <- d[!is.na(d[[cm$conc]]) & d[[cm$conc]] > 0, ]
      if (nrow(d) == 0) return(plotly_empty())
      subjects <- sort(unique(d[[cm$subject]]))
      
      # Paginate
      sel <- input$grid_page
      if (!is.null(sel) && sel != "all") {
        pages <- split(subjects, ceiling(seq_along(subjects) / 12))
        idx <- as.integer(sel)
        if (!is.na(idx) && idx <= length(pages)) subjects <- pages[[idx]]
      } else if (length(subjects) > 36) {
        subjects <- subjects[1:36]
      }
      
      sub_d <- d[d[[cm$subject]] %in% subjects, ]
      tryCatch({
        sub_d$.profile <- profile_group(sub_d, cm)
        p <- ggplot(sub_d, aes(x = .data[[cm$time]], y = .data[[cm$conc]], group = .profile)) +
          geom_line(color = "#2C3E50", linewidth = 0.5) +
          geom_point(size = 1.5, color = "#3498DB") +
          facet_wrap(reformulate(cm$subject), scales = "free_y") +
          scale_y_log10() +
          theme_minimal(base_size = 9) +
          labs(x = "Time", y = "Concentration (log)")
        ggplotly(p)
      }, error = function(e) plotly_empty())
    })
    
    # Half-life inspector — helper to get selected profile's data
    lz_sub_data <- reactive({
      req(input$lz_profile, shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map; sel <- input$lz_profile
      sub_d <- d[profile_data_rows(d, cm, sel), ]
      list(time = sub_d[[cm$time]], conc = sub_d[[cm$conc]], is_blq = sub_d[[BLQ_FLAG_COLUMN]])
    })
    
    output$lz_info <- renderUI({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      lz <- if (!is.null(lz_state$override)) lz_state$override
            else estimate_lambda_z(sd$time, sd$conc, input$r2adj, route = input$admin_route, is_blq = sd$is_blq)
      if (is.na(lz$lambda_z)) {
        tags$div(class="alert alert-warning py-2", tags$small(lz$message))
      } else {
        badge <- if (!is.null(lz_state$override))
          tags$span(class = "badge bg-info ms-2", "manually adjusted") else NULL
        tags$div(class="alert alert-success py-2",
                 tags$small(paste0("Half-life: ", signif(lz$half_life,4), " h | R\u00B2: ",
                                   signif(lz$r2adj,4), " | ", lz$n_points, " pts")),
                 badge)
      }
    })
    
    output$lz_plot <- renderPlotly({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      tryCatch({
      lz <- if (!is.null(lz_state$override)) lz_state$override
            else estimate_lambda_z(sd$time, sd$conc, input$r2adj, route = input$admin_route, is_blq = sd$is_blq)
      df <- data.frame(
        Time = sd$time,
        ln_Conc = ifelse(sd$conc > 0, log(sd$conc), NA),
        Conc = sd$conc,
        used = FALSE
      )
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
        theme(legend.position = "none", plot.margin = margin(5, 10, 5, 5))
      
      if (!is.na(lz$lambda_z)) {
        tr <- range(lz$time_used)
        tp <- seq(tr[1], tr[2] * 1.05, length.out = 30)
        line_df <- data.frame(Time = tp,
                              ln_Conc = lz$intercept - lz$lambda_z * tp)
        p <- p + geom_line(data = line_df, aes(x = Time, y = ln_Conc),
                           inherit.aes = FALSE, color = "#E74C3C",
                           linetype = "dashed", linewidth = 0.7)
      }
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(b = 40))
    }, error = function(e) plotly_empty())
    })
    
    # Populate checkboxes when profile changes
    observe({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      valid <- !is.na(sd$conc) & sd$conc > 0
      cmax_t <- sd$time[which.max(sd$conc)]
      term <- valid & sd$time > cmax_t
      if (any(term)) {
        ch <- paste0("t=", sd$time[term], "  C=", round(sd$conc[term], 3))
        names(ch) <- which(term)
        if (!is.null(lz_state$override)) {
          sel <- as.character(which(term)[sd$time[term] %in% lz_state$override$time_used])
        } else {
          lz <- estimate_lambda_z(sd$time, sd$conc, input$r2adj, route = input$admin_route, is_blq = sd$is_blq)
          sel <- if (length(lz$time_used) > 0)
            as.character(which(term)[sd$time[term] %in% lz$time_used]) else NULL
        }
        updateCheckboxGroupInput(session, "lz_points",
                                 choices = setNames(names(ch), ch), selected = sel)
      }
    })
    
    # Recalculate from user-selected points
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
      
      lz_state$override <- override
      
      # Log the override for audit trail
      sel <- input$lz_profile
      orig_lz <- estimate_lambda_z(sd$time, sd$conc, input$r2adj, route = input$admin_route, is_blq = sd$is_blq)
      lz_state$overrides_log[[sel]] <- c(list(
        profile = sel),
        # Subject / treatment / period, so the reproduction script can replay
        # the override on exactly this administration
        profile_parts(nca_result(), sel), list(
        original_lambda_z = if (!is.na(orig_lz$lambda_z)) as.numeric(orig_lz$lambda_z) else NA,
        adjusted_lambda_z = as.numeric(lz_new),
        original_r2adj = if (!is.na(orig_lz$r2adj)) as.numeric(orig_lz$r2adj) else NA,
        adjusted_r2adj = if (!is.na(r2adj)) as.numeric(r2adj) else NA,
        points_used = n_pts,
        time_used = as.numeric(t_sel)
      ))
      
      # Recompute with every logged override applied. NonCompart fits the chosen
      # points and derives all dependent parameters (AUCinf, CL, V, MRT, ...)
      # with its own unit conversions, exactly as the reproduction script does.
      settings <- shared$nca_settings
      if (!is.null(settings)) {
        r <- suppressWarnings(run_nca(shared$pk_data, shared$col_map, settings,
                                      lz_overrides = lz_state$overrides_log))
        if (!is.null(r)) {
          if (isTRUE(input$dose_norm)) r <- add_dose_normalized(as.data.frame(r), settings$dose)
          nca_result(r)
          shared$nca_results <- r
        }
      }
      
      showNotification(
        sprintf("Recalculated: t\u00BD = %.3f h (%s, %d pts)",
                hl_new,
                if (!is.na(r2adj)) sprintf("R\u00B2 = %.4f", r2adj) else "R\u00B2 = N/A",
                n_pts),
        type = "message", duration = 5)
    })
    
    # Downloads
    output$dl_params_csv <- downloadHandler(
      filename = function() paste0("NCA_results_", Sys.Date(), ".csv"),
      content = function(file) {
        req(nca_result())
        write.csv(rename_nca_columns(nca_result()), file, row.names=FALSE)
      }
    )
    output$dl_params_xlsx <- downloadHandler(
      filename = function() paste0("NCA_results_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(nca_result())
        wb <- createWorkbook()
        addWorksheet(wb, "Individual_Parameters")
        writeData(wb, 1, rename_nca_columns(nca_result()))
        r <- nca_result()
        add_cdisc_code_sheet(wb, names(r)[vapply(r, is.numeric, logical(1))],
                             input$admin_route, isTRUE(input$is_ss))
        key <- c(intersect(if (isTRUE(input$is_ss)) c("CMAX","TMAX","AUCTAU","CAVG","CMIN_SS","FLUCTP","LAMZHL","CLFO") else c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"), names(r)),
                 partial_auc_cols(names(r)))
        if (length(key)>0) {
          addWorksheet(wb, "Summary_Statistics")
          writeData(wb, "Summary_Statistics", rename_summary_columns(summarize_pk_params(r, key, group_col = if ("Treatment" %in% names(r)) "Treatment" else NULL)))
        }
        saveWorkbook(wb, file, overwrite=TRUE)
      }
    )
    
    # Analysis Record panel — appears once NCA has been run
    output$record_panel <- renderUI({
      req(nca_result())
      analysis_record_ui(
        session$ns,
        intro = paste0(
          "Self-contained package with individual NCA parameters, summary statistics, ",
          "every setting, a standalone R reproducibility script, a SHA-256 ",
          "data-integrity hash, and an HTML summary."))
    })

    # Complete Analysis Record
    output$dl_record <- downloadHandler(
      filename = function() {
        study <- if (nchar(input$record_study) > 0)
          gsub("[^A-Za-z0-9_-]", "_", input$record_study) else "NCA"
        paste0("Analysis_Record_", study, "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(nca_result(), shared$col_map, shared$study_info)
        
        withProgress(message = "Generating analysis record...", value = 0.3, {
          r <- nca_result()
          key <- c(intersect(if (isTRUE(input$is_ss)) c("CMAX","TMAX","AUCTAU","CAVG","CMIN_SS","FLUCTP","LAMZHL","CLFO") else c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"), names(r)),
                   partial_auc_cols(names(r)))
          summ <- if (length(key) > 0) summarize_pk_params(r, key, group_col = if ("Treatment" %in% names(r)) "Treatment" else NULL) else NULL
          
          settings <- shared$nca_settings
          if (is.null(settings)) {
            settings <- list(
              admin_route = input$admin_route, dose = input$dose,
              infusion_duration = 0, is_steady_state = isTRUE(input$is_ss), tau = input$tau,
              dose_unit = input$dose_unit, time_unit = input$time_unit,
              conc_unit = input$conc_unit, trap_method = input$trap_method,
              r2adj_threshold = input$r2adj, n_obs = nrow(shared$pk_data)
            )
          }
          settings$n_obs <- nrow(shared$pk_data)
          
          si <- shared$study_info
          original_name <- si$file_name
          original_path <- si$file_path
          read_args <- si$read_args
          adnca_rec <- if (identical(si$door, "adnca")) si$adnca else NULL
          
          # Fallback: save shared$raw_data to temp file
          fallback_dir <- NULL
          if (is.null(original_path) || !file.exists(original_path)) {
            original_path <- fallback_copy_path(original_name)
            fallback_dir <- dirname(original_path)
            read_args <- if (!is.null(shared$raw_data))
              write_record_fallback(shared$raw_data, original_path, read_args) else list()
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
            summary_stats  = summ,
            lz_overrides   = if (length(lz_state$overrides_log) > 0) lz_state$overrides_log else NULL,
            viz_settings   = shared$viz_settings,
            read_args      = read_args,
            adnca          = adnca_rec
          )
          notify_reproduction(rec_out)
          if (!is.null(fallback_dir)) unlink(fallback_dir, recursive = TRUE)
        })
      }
    )
  })
}
