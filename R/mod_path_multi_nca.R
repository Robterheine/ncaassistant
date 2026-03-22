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
              numericInput(ns("dose"), "Dose given to each subject", value = 100, min = 0),
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
                     "Each column is a PK parameter."),
              DTOutput(ns("param_table")),
              downloadButton(ns("dl_params_csv"), "Download as CSV",
                             class = "btn-outline-primary btn-sm mt-2"),
              downloadButton(ns("dl_params_xlsx"), "Download as Excel",
                             class = "btn-outline-success btn-sm mt-2")
            ),
            
            # Summary stats
            nav_panel(
              "Summary Statistics (across subjects)",
              icon = icon("chart-bar"),
              tags$p(class = "text-muted small",
                     "Mean, SD, CV%, median, range, geometric mean, and geometric CV% ",
                     "for key PK parameters across all subjects."),
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
              plotOutput(ns("grid_plot"), height = "800px")
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
                    uiOutput(ns("lz_info"))
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
    
    output$data_ok <- reactive({ shared$data_ready })
    outputOptions(output, "data_ok", suspendWhenHidden = FALSE)
    
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
      
      settings <- list(
        admin_route       = input$admin_route,
        dose              = input$dose,
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
      
      if (!is.null(shared$col_map$dose)) {
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
      datatable(nca_result(),
                options = list(scrollX = TRUE, scrollY = "400px",
                               pageLength = 50, dom = "frtip"),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatSignif(columns = which(sapply(nca_result(), is.numeric)), digits = 4)
    })
    
    # Summary stats
    output$summary_table <- renderDT({
      req(nca_result())
      r <- nca_result()
      key <- intersect(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"),
                       names(r))
      if (length(key) == 0) return(NULL)
      summ <- summarize_pk_params(r, key)
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
        mutate(Value = as.numeric(Value)) %>%
        filter(!is.na(Value))
      p <- ggplot(long, aes(x = Parameter, y = Value)) +
        geom_boxplot(fill = "#3498DB", alpha = 0.6) +
        geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
        facet_wrap(~Parameter, scales = "free", nrow = 1) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_blank())
      ggplotly(p)
    })
    
    # Individual grid
    output$grid_plot <- renderPlot({
      req(shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map
      subjects <- sort(unique(d[[cm$subject]]))
      if (length(subjects) > 36) subjects <- subjects[1:36]
      sub_d <- d[d[[cm$subject]] %in% subjects, ]
      ggplot(sub_d, aes(x = .data[[cm$time]], y = .data[[cm$conc]])) +
        geom_line(color = "#2C3E50", linewidth = 0.5) +
        geom_point(size = 1, color = "#3498DB") +
        facet_wrap(as.formula(paste("~", cm$subject)), scales = "free_y") +
        scale_y_log10() +
        theme_minimal(base_size = 9) +
        labs(x = "Time", y = "Concentration (log)")
    }, res = 100)
    
    # Half-life inspector
    output$lz_info <- renderUI({
      req(input$lz_profile, shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map; sel <- input$lz_profile
      if (grepl(" \\| ", sel)) {
        parts <- strsplit(sel, " \\| ")[[1]]
        sub_d <- d[d[[cm$subject]] == trimws(parts[1]) &
                     d[[cm$treatment]] == trimws(parts[2]), ]
      } else { sub_d <- d[d[[cm$subject]] == sel, ] }
      lz <- estimate_lambda_z(sub_d[[cm$time]], sub_d[[cm$conc]], 0.7)
      if (is.na(lz$lambda_z)) {
        tags$div(class="alert alert-warning py-2", tags$small(lz$message))
      } else {
        tags$div(class="alert alert-success py-2",
                 tags$small(paste0("Half-life: ", signif(lz$half_life,4), "h | R²: ",
                                   signif(lz$r2adj,4), " | ", lz$n_points, " pts")))
      }
    })
    
    output$lz_plot <- renderPlotly({
      req(input$lz_profile, shared$pk_data, shared$col_map)
      d <- shared$pk_data; cm <- shared$col_map; sel <- input$lz_profile
      if (grepl(" \\| ", sel)) {
        parts <- strsplit(sel, " \\| ")[[1]]
        sub_d <- d[d[[cm$subject]] == trimws(parts[1]) &
                     d[[cm$treatment]] == trimws(parts[2]), ]
      } else { sub_d <- d[d[[cm$subject]] == sel, ] }
      time <- sub_d[[cm$time]]; conc <- sub_d[[cm$conc]]
      lz <- estimate_lambda_z(time, conc, 0.7)
      df <- data.frame(time = time, lc = ifelse(conc > 0, log(conc), NA), used = FALSE)
      if (length(lz$time_used) > 0) {
        for (i in seq_along(lz$time_used))
          { m <- which(abs(time - lz$time_used[i]) < 1e-10)
            if (length(m)>0) df$used[m[1]] <- TRUE }
      }
      df$type <- ifelse(df$used, "Included", "Excluded")
      df <- df[!is.na(df$lc), ]
      p <- ggplot(df, aes(x=time, y=lc, color=type)) + geom_point(size=3) +
        scale_color_manual(values=c("Included"="#E74C3C","Excluded"="#BDC3C7")) +
        theme_minimal(base_size=11) + labs(x="Time",y="ln(Conc)",color=NULL)
      if (!is.na(lz$lambda_z)) {
        tr <- range(lz$time_used)
        tp <- seq(tr[1], tr[2]*1.1, length.out=30)
        p <- p + geom_line(data=data.frame(time=tp, lc=lz$intercept-lz$lambda_z*tp),
                           aes(x=time,y=lc), inherit.aes=FALSE,
                           color="#E74C3C", linetype="dashed")
      }
      ggplotly(p)
    })
    
    # Downloads
    output$dl_params_csv <- downloadHandler(
      filename = function() paste0("NCA_results_", Sys.Date(), ".csv"),
      content = function(file) { req(nca_result()); write.csv(nca_result(), file, row.names=FALSE) }
    )
    output$dl_params_xlsx <- downloadHandler(
      filename = function() paste0("NCA_results_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(nca_result())
        wb <- createWorkbook()
        addWorksheet(wb, "Individual_Parameters"); writeData(wb, 1, nca_result())
        r <- nca_result()
        key <- intersect(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"), names(r))
        if (length(key)>0) {
          addWorksheet(wb, "Summary_Statistics")
          writeData(wb, 2, summarize_pk_params(r, key))
        }
        saveWorkbook(wb, file, overwrite=TRUE)
      }
    )
  })
}
