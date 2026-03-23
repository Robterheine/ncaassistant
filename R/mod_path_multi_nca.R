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
                     "Key PK parameters shown. Tick the box below for all 37 parameters, ",
                     "or download the full table as Excel."),
              checkboxInput(ns("show_all_params"),
                            "Show all parameters (37 columns)", FALSE),
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
    
    lz_state <- reactiveValues(override = NULL)
    
    # Reset override when profile changes
    observeEvent(input$lz_profile, { lz_state$override <- NULL }, ignoreInit = TRUE)
    
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
      display_df <- rename_nca_columns(nca_result())
      
      if (!isTRUE(input$show_all_params)) {
        key_cols <- intersect(
          c("Subject", "Treatment",
            "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
            "AUC to Last Point", "AUC to Infinity (observed)",
            "Half-Life (h)", "Apparent Clearance (CL/F)",
            "Apparent Volume (Vz/F)", "Adjusted R-squared",
            "Points Used for Half-Life"),
          names(display_df))
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
      key <- intersect(c("CMAX","TMAX","AUCLST","AUCIFO","LAMZHL","CLFO","VZFO"),
                       names(r))
      if (length(key) == 0) return(NULL)
      summ <- summarize_pk_params(r, key)
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
      hl_new <- log(2) / lz_new; n_pts <- length(t_sel)
      ss_res <- sum(residuals(fit)^2)
      ss_tot <- sum((log(c_sel) - mean(log(c_sel)))^2)
      r2adj <- 1 - (1 - (1 - ss_res/ss_tot)) * (n_pts - 1) / (n_pts - 2)
      
      lz_state$override <- list(
        lambda_z = as.numeric(lz_new), half_life = as.numeric(hl_new),
        intercept = as.numeric(int_new), r2adj = as.numeric(r2adj),
        n_points = n_pts, time_used = t_sel, message = "User-selected"
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
        sprintf("Recalculated: t\u00BD = %.3f h (R\u00B2 = %.4f, %d pts)",
                hl_new, r2adj, n_pts),
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
  })
}
