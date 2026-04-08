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
                             "Each subject has a different dose (from Dose column in data)" = "from_data"
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
                            value = FALSE)
            )
          ),
          
          card(
            card_header(class = "bg-primary text-white", "Units"),
            card_body(
              layout_columns(
                col_widths = c(4, 4, 4),
                textInput(ns("dose_unit"), "Dose", value = "mg"),
                textInput(ns("time_unit"), "Time", value = "h"),
                textInput(ns("conc_unit"), "Conc", value = "ng/mL")
              )
            )
          ),
          
          card(
            card_header(class = "bg-primary text-white", "Calculation Method"),
            card_body(
              selectInput(ns("trap_method"),
                          tagList("Trapezoidal method", help_trapezoidal),
                          choices = c("Linear-up / Log-down (recommended)" = "log",
                                      "Linear-up / Linear-down" = "linear")),
              sliderInput(ns("r2adj"),
                          tagList("Minimum R² for half-life estimation", help_r2adj),
                          min = 0, max = 1, value = 0.7, step = 0.05),
              checkboxInput(ns("dose_norm"),
                            tagList("Calculate dose-normalized parameters", help_dose_norm),
                            FALSE)
            )
          ),
          
          hr(),
          actionButton(ns("run_nca"), "Run Analysis on All Subjects",
                       class = "btn-success btn-lg w-100",
                       icon = icon("play"))
        ),
        
        # --- Main content ----------------------------------------------------
        tagList(
          uiOutput(ns("result_status")),
          
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
              "All Parameters (per subject)",
              icon = icon("table"),
              tags$p(class = "text-muted small",
                     "One row per subject (or per subject-treatment for crossover data). ",
                     "Key PK parameters shown. Tick the box below for all 37 parameters, ",
                     "or download the full table as Excel."),
              checkboxInput(ns("show_all_params"),
                            "Show all parameters (37 columns)", FALSE),
              DTOutput(ns("param_table")),
              downloadButton(ns("dl_params_csv"), "Download as CSV",
                             class = "btn-outline-primary btn-sm mt-2"),
              downloadButton(ns("dl_params_xlsx"), "Download as Excel",
                             class = "btn-outline-success btn-sm mt-2"),
              hr(),
              tags$details(
                tags$summary(
                  class = "fw-semibold small",
                  style = "cursor: pointer;",
                  icon("file-zipper", class = "me-1 text-primary"),
                  "Download Complete Analysis Record"
                ),
                tags$div(
                  class = "mt-2 small",
                  tags$p(class = "text-muted",
                         "Download a self-contained package with results, settings, ",
                         "a standalone R reproducibility script, data integrity hash, ",
                         "and analysis summary. Useful for regulatory submissions, ",
                         "publication supplements, and audit trails."),
                  layout_columns(
                    col_widths = c(6, 6),
                    textInput(ns("record_analyst"), "Analyst name (optional)",
                              value = "", placeholder = "Your name"),
                    textInput(ns("record_study"), "Study name (optional)",
                              value = "", placeholder = "e.g., Phase I PK Study")
                  ),
                  downloadButton(ns("dl_record"), "Generate Analysis Record",
                                 class = "btn-primary btn-sm w-100",
                                 icon = icon("file-zipper"))
                )
              )
            ),
            
            # Summary stats
            nav_panel(
              "Summary Statistics (across subjects)",
              icon = icon("chart-bar"),
              tags$p(class = "text-muted small",
                     "Mean, SD, CV%, median, range, geometric mean, and geometric CV% ",
                     "for key PK parameters across all subjects."),
              uiOutput(ns("ss_note")),
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
          )
        )
      )
    )
  )
}

path_multi_nca_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    lz_state <- reactiveValues(override = NULL, overrides_log = list())
    
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
                       actionButton(ns("goto_upload"), "Upload Data",
                                    class = "btn-warning",
                                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});")))
      }
    })
    
    nca_result <- reactiveVal(NULL)
    
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
      
      settings <- list(
        admin_route       = input$admin_route,
        dose              = if (use_data_dose) NA else input$dose,
        infusion_duration = ifelse(input$admin_route == "iv_infusion",
                                   input$inf_dur, 0),
        is_steady_state   = input$is_ss,
        dose_unit         = input$dose_unit,
        time_unit         = input$time_unit,
        conc_unit         = input$conc_unit,
        trap_method       = input$trap_method,
        r2adj_threshold   = input$r2adj,
        mw = 0,
        partial_aucs      = NULL
      )
      
      if (use_data_dose) {
        dose_df <- shared$pk_data %>%
          group_by(.data[[shared$col_map$subject]]) %>%
          summarize(dose = max(.data[[shared$col_map$dose]], na.rm = TRUE),
                    .groups = "drop")
        settings$dose <- dose_df$dose
      }
      
      withProgress(message = "Running NCA on all subjects...", value = 0.5, {
        result <- run_nca(shared$pk_data, shared$col_map, settings)
        
        if (is.null(result)) {
          showNotification("NCA failed. Check settings.", type = "error")
          return()
        }
        
        if (input$dose_norm) {
          dose_val <- if (length(settings$dose) > 1) settings$dose else
            rep(settings$dose, nrow(result))
          result <- add_dose_normalized(as.data.frame(result), dose_val)
        }
        
        nca_result(result)
        shared$nca_results  <- result
        shared$nca_settings <- settings
        gc()  # Free NCA intermediates
      })
      
      # Update half-life profile selector
      if ("Subject" %in% names(result) && "Treatment" %in% names(result)) {
        choices <- paste(result$Subject, "|", result$Treatment)
      } else {
        choices <- result[[1]]
      }
      updateSelectInput(session, "lz_profile", choices = choices)
      
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
          "AUC to Last Point represents the AUC within the dosing interval (AUC\u03C4). ",
          "CL/F is calculated as Dose/AUC\u03C4, which is the correct formula at steady state. ",
          "AUC to infinity is not pharmacokinetically meaningful during repeated dosing and is ",
          "hidden from the default view (available under 'Show all parameters')."
        )
      }
    })
    
    # Status
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
      p <- ggplot(d, aes(x = .data[[cm$time]], y = .data[[cm$conc]],
                         group = .data[[cm$subject]])) +
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
      summ <- d %>%
        group_by(.data[[cm$time]]) %>%
        summarize(mean_c = mean(.data[[cm$conc]], na.rm = TRUE),
                  sd_c = sd(.data[[cm$conc]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(summ, aes(x = .data[[cm$time]], y = mean_c)) +
        geom_errorbar(aes(ymin = pmax(0, mean_c - sd_c), ymax = mean_c + sd_c),
                      width = 0.3, alpha = 0.5) +
        geom_line(linewidth = 0.8, color = "#2C3E50") +
        geom_point(size = 2.5, color = "#E74C3C") +
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
          # Steady-state: show tau-relevant parameters, de-emphasise AUC∞
          key_cols <- intersect(
            c("Subject", "Treatment",
              "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
              "AUC to Last Point",
              "Half-Life (h)", "Apparent Clearance (CL/F)",
              "Adjusted R-squared"),
            names(display_df))
        } else {
          key_cols <- intersect(
            c("Subject", "Treatment",
              "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
              "AUC to Last Point", "AUC to Infinity (observed)",
              "Half-Life (h)", "Apparent Clearance (CL/F)",
              "Apparent Volume (Vz/F)", "Adjusted R-squared",
              "Points Used for Half-Life"),
            names(display_df))
        }
        display_df <- display_df[, key_cols, drop = FALSE]
      }
      
      datatable(display_df,
                options = list(scrollX = TRUE, scrollY = "400px",
                               pageLength = 50, dom = "frtip"),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatSignif(columns = which(sapply(display_df, is.numeric)), digits = 4)
    })
    
    # Summary stats
    output$summary_table <- renderDT({
      req(nca_result())
      r <- nca_result()
      
      if (isTRUE(input$is_ss)) {
        # At steady state: AUC0-t = AUCtau, show CL/F (derived from AUCtau by NonCompart)
        key <- intersect(c("CMAX","TMAX","AUCLST","LAMZHL","CLFO"), names(r))
      } else {
        key <- intersect(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"), names(r))
      }
      if (length(key) == 0) return(NULL)
      summ <- summarize_pk_params(r, key)
      summ <- rename_summary_columns(summ)
      
      # At steady state, relabel AUC to Last Point as AUC within dosing interval
      if (isTRUE(input$is_ss) && "Parameter" %in% names(summ)) {
        summ$Parameter[summ$Parameter == "AUC to Last Point"] <-
          "AUC Within Dosing Interval (AUC\u03C4)"
      }
      
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
      long <- r %>%
        select(all_of(key)) %>%
        pivot_longer(everything(), names_to = "Parameter", values_to = "Value") %>%
        mutate(Value = as.numeric(Value),
               Parameter = sapply(Parameter, friendly_name)) %>%
        filter(!is.na(Value))
      p <- ggplot(long, aes(x = Parameter, y = Value)) +
        geom_boxplot(fill = "#3498DB", alpha = 0.6) +
        geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
        facet_wrap(~Parameter, scales = "free", nrow = 1) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_blank())
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
        p <- ggplot(sub_d, aes(x = .data[[cm$time]], y = .data[[cm$conc]])) +
          geom_line(color = "#2C3E50", linewidth = 0.5) +
          geom_point(size = 1.5, color = "#3498DB") +
          facet_wrap(as.formula(paste("~", cm$subject)), scales = "free_y") +
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
      if (grepl(" \\| ", sel)) {
        parts <- strsplit(sel, " \\| ")[[1]]
        sub_d <- d[d[[cm$subject]] == trimws(parts[1]) &
                     d[[cm$treatment]] == trimws(parts[2]), ]
      } else { sub_d <- d[d[[cm$subject]] == sel, ] }
      sub_d <- sub_d[order(sub_d[[cm$time]]), ]
      list(time = sub_d[[cm$time]], conc = sub_d[[cm$conc]])
    })
    
    output$lz_info <- renderUI({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      lz <- if (!is.null(lz_state$override)) lz_state$override
            else estimate_lambda_z(sd$time, sd$conc, 0.7)
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
            else estimate_lambda_z(sd$time, sd$conc, 0.7)
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
          lz <- estimate_lambda_z(sd$time, sd$conc, 0.7)
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
      if (length(sel_idx) < 2) {
        showNotification("Select at least 2 points.", type = "warning"); return()
      }
      t_sel <- sd$time[sel_idx]; c_sel <- sd$conc[sel_idx]
      valid <- c_sel > 0 & !is.na(c_sel)
      t_sel <- t_sel[valid]; c_sel <- c_sel[valid]
      if (length(t_sel) < 2) {
        showNotification("Need 2+ points with positive concentration.", type = "warning"); return()
      }
      fit <- lm(log(c_sel) ~ t_sel)
      lz_new <- -coef(fit)[2]; int_new <- coef(fit)[1]
      n_pts <- length(t_sel)
      
      # Guard: negative lambda_z
      if (is.na(lz_new) || lz_new <= 0) {
        showNotification(
          "The selected points have an ascending or flat slope \u2014 select points from the descending part of the curve.",
          type = "error", duration = 10)
        return()
      }
      
      hl_new <- log(2) / lz_new
      ss_res <- sum(residuals(fit)^2)
      ss_tot <- sum((log(c_sel) - mean(log(c_sel)))^2)
      r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA
      r2adj <- if (n_pts >= 3 && !is.na(r2)) {
        1 - (1 - r2) * (n_pts - 1) / (n_pts - 2)
      } else { NA }
      if (n_pts == 2) {
        showNotification("Half-life computed from 2 points (R\u00B2 not available).",
                         type = "warning", duration = 8)
      }
      
      lz_state$override <- list(
        lambda_z = as.numeric(lz_new), half_life = as.numeric(hl_new),
        intercept = as.numeric(int_new), r2adj = as.numeric(r2adj),
        n_points = n_pts, time_used = t_sel, message = "User-selected"
      )
      
      # Log the override for audit trail
      sel <- input$lz_profile
      orig_lz <- estimate_lambda_z(sd$time, sd$conc, input$r2adj)
      lz_state$overrides_log[[sel]] <- list(
        profile = sel,
        original_lambda_z = if (!is.na(orig_lz$lambda_z)) as.numeric(orig_lz$lambda_z) else NA,
        adjusted_lambda_z = as.numeric(lz_new),
        original_r2adj = if (!is.na(orig_lz$r2adj)) as.numeric(orig_lz$r2adj) else NA,
        adjusted_r2adj = if (!is.na(r2adj)) as.numeric(r2adj) else NA,
        points_used = n_pts
      )
      
      # Update this profile's row in the NCA results table
      r <- nca_result()
      if (!is.null(r)) {
        sel <- input$lz_profile
        if (grepl(" \\| ", sel)) {
          parts <- strsplit(sel, " \\| ")[[1]]
          row_idx <- which(r$Subject == trimws(parts[1]) & r$Treatment == trimws(parts[2]))
        } else {
          row_idx <- which(r[[1]] == sel)
        }
        if (length(row_idx) == 1) {
          r$LAMZ[row_idx]    <- lz_new
          r$LAMZHL[row_idx]  <- hl_new
          r$R2ADJ[row_idx]   <- r2adj
          r$LAMZNPT[row_idx] <- n_pts
          auclst <- as.numeric(r$AUCLST[row_idx])
          clast  <- as.numeric(r$CLST[row_idx])
          if (!is.na(clast) && lz_new > 0) {
            aucifo <- auclst + clast / lz_new
            r$AUCIFO[row_idx] <- aucifo
            if ("CLFO" %in% names(r)) {
              dose_col <- if ("DOSE" %in% names(r)) as.numeric(r$DOSE[row_idx]) else NA
              dose_val <- if (!is.na(dose_col)) dose_col else input$dose
              if (is.null(dose_val)) dose_val <- 100
              r$CLFO[row_idx] <- dose_val / aucifo
              r$VZFO[row_idx] <- dose_val / (aucifo * lz_new)
            }
          }
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
        key <- intersect(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"), names(r))
        if (length(key)>0) {
          addWorksheet(wb, "Summary_Statistics")
          writeData(wb, 2, rename_summary_columns(summarize_pk_params(r, key)))
        }
        saveWorkbook(wb, file, overwrite=TRUE)
      }
    )
    
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
          key <- intersect(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"), names(r))
          summ <- if (length(key) > 0) summarize_pk_params(r, key) else NULL
          
          settings <- shared$nca_settings
          if (is.null(settings)) {
            settings <- list(
              admin_route = input$admin_route, dose = input$dose,
              infusion_duration = 0, is_steady_state = isTRUE(input$is_ss),
              dose_unit = input$dose_unit, time_unit = input$time_unit,
              conc_unit = input$conc_unit, trap_method = input$trap_method,
              r2adj_threshold = input$r2adj, n_obs = nrow(shared$pk_data)
            )
          }
          settings$n_obs <- nrow(shared$pk_data)
          
          si <- shared$study_info
          original_name <- si$file_name
          original_path <- si$file_path
          
          # Fallback: save shared$raw_data to temp file
          if (is.null(original_path) || !file.exists(original_path)) {
            original_path <- file.path(tempdir(), original_name)
            if (!is.null(shared$raw_data)) {
              write.csv(shared$raw_data, original_path, row.names = FALSE)
            }
          }
          
          setProgress(0.6, message = "Building R script and summary...")
          
          create_analysis_record(
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
            lz_overrides   = if (length(lz_state$overrides_log) > 0) lz_state$overrides_log else NULL
          )
        })
      }
    )
  })
}
