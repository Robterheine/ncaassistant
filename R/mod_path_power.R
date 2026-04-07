# ============================================================================
# NCA Assistant — Path 1: Plan a Study (Power & Sample Size)
# ============================================================================
# Fully standalone. Uses PowerTOST. Optional bridge to NCA results for CV.

path_power_ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container-fluid py-3",
    style = "max-width: 1200px; margin: 0 auto;",
    
    tags$h4(class = "fw-bold mb-1",
            icon("calculator", class = "text-info me-2"),
            "Plan a Study — Power & Sample Size"),
    tags$p(class = "text-muted mb-3",
           "Calculate how many subjects you need, or check the probability of success ",
           "for a given sample size. No data upload required."),
    
    layout_sidebar(
      fillable = FALSE,
      
      sidebar = sidebar(
        title = tagList("Settings", help_what_is_power),
        width = 400,
        open = TRUE,
        
        # What to calculate
        selectInput(ns("calc_mode"), "What do you want to calculate?",
                    choices = c(
                      "How many subjects do I need? (sample size)" = "sample_size",
                      "What is the chance of success? (power)" = "power"
                    )),
        
        # Analysis type
        selectInput(ns("analysis_type"), "Type of study",
                    choices = c(
                      "Standard Bioequivalence (80–125% limits)" = "abe",
                      "Highly Variable Drugs — EMA/WHO widened limits" = "abel",
                      "Highly Variable Drugs — US FDA scaled approach" = "rsabe",
                      "Narrow Therapeutic Index Drugs — US FDA" = "ntid",
                      "Dose-Proportionality (does exposure scale with dose?)" = "dp",
                      "Non-Inferiority (is the new treatment not worse?)" = "noninf"
                    )),
        
        uiOutput(ns("type_desc")),
        
        hr(),
        
        # Study design
        selectInput(ns("design"), "Study design",
                    choices = c(
                      "Standard 2-period crossover" = "2x2",
                      "3-period crossover" = "2x2x3",
                      "Williams design (3-sequence, 3-period)" = "2x3x3",
                      "4-period replicate crossover" = "2x2x4",
                      "Parallel groups" = "parallel"
                    )),
        
        # Regulatory preset
        selectInput(ns("reg_preset"), "Regulatory agency",
                    choices = c(
                      "Custom settings" = "custom",
                      "European Medicines Agency (EMA)" = "ema",
                      "US Food & Drug Administration (FDA)" = "fda",
                      "Health Canada" = "hc",
                      "World Health Organization (WHO)" = "who",
                      "Japan (PMDA)" = "pmda",
                      "China (CDE)" = "cde"
                    )),
        
        hr(),
        
        # Key parameters
        numericInput(ns("alpha"), "Significance level (usually 0.05)",
                     value = 0.05, min = 0.001, max = 0.1, step = 0.005),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sample_size'", ns("calc_mode")),
          numericInput(ns("target_power"),
                       "Target power (e.g., 0.80 = 80% chance of success)",
                       value = 0.80, min = 0.5, max = 0.999, step = 0.01)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'power'", ns("calc_mode")),
          numericInput(ns("n_subjects"), "Total number of subjects",
                       value = 24, min = 4, step = 2)
        ),
        
        numericInput(ns("theta0"),
                     tagList("Expected Test/Reference ratio", help_theta0),
                     value = 0.95, min = 0.5, max = 2, step = 0.01),
        
        numericInput(ns("cv"),
                     tagList("Within-subject variability (CV as decimal: 20% = 0.20)",
                             help_cv),
                     value = 0.20, min = 0.01, max = 2, step = 0.01),
        
        # CV bridge from NCA
        uiOutput(ns("cv_from_nca")),
        
        conditionalPanel(
          condition = sprintf(
            "['abel', 'rsabe', 'ntid'].includes(input['%s'])",
            ns("analysis_type")),
          numericInput(ns("cv_wr"),
                       "Within-subject CV of the Reference product",
                       value = 0.30, min = 0.01, max = 2, step = 0.01)
        ),
        
        hr(),
        
        tags$h6("Equivalence acceptance limits"),
        tags$p(class = "text-muted small",
               "Standard: 0.80 to 1.25 (Test within 80–125% of Reference)."),
        layout_columns(
          col_widths = c(6, 6),
          numericInput(ns("theta1"), "Lower limit", value = 0.80),
          numericInput(ns("theta2"), "Upper limit", value = 1.25)
        ),
        
        hr(),
        
        actionButton(ns("btn_calc"), "Calculate",
                     class = "btn-success btn-lg w-100",
                     icon = icon("calculator"))
      ),
      
      # --- Results -----------------------------------------------------------
      tagList(
        uiOutput(ns("result_cards")),
        
        navset_card_tab(
          title = "Detailed Results",
          
          nav_panel(
            "Power vs. Number of Subjects",
            icon = icon("chart-line"),
            tags$p(class = "text-muted small",
                   "Shows how the probability of concluding equivalence ",
                   "increases with more subjects."),
            plotlyOutput(ns("power_curve"), height = "420px")
          ),
          
          nav_panel(
            "Sample Size vs. Variability",
            icon = icon("table-cells"),
            tags$p(class = "text-muted small",
                   "Shows how the required number of subjects changes across ",
                   "different variability levels."),
            plotlyOutput(ns("sensitivity_plot"), height = "420px")
          ),
          
          nav_panel(
            "Calculation Summary",
            icon = icon("info-circle"),
            verbatimTextOutput(ns("result_text"))
          )
        )
      )
    )
  )
}

path_power_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Type description
    output$type_desc <- renderUI({
      desc <- switch(input$analysis_type,
        "abe"   = "Tests whether the 90% CI of the Test/Reference ratio falls within 80–125%.",
        "abel"  = "For highly variable drugs (CV > 30%). Acceptance limits widen based on Reference variability. EMA/WHO approach.",
        "rsabe" = "For highly variable drugs (CV > 30%). US FDA scaled approach.",
        "ntid"  = "For narrow therapeutic index drugs. Tighter limits. US FDA method.",
        "dp"    = "Tests whether drug exposure increases proportionally with dose.",
        "noninf"= "Tests whether a new treatment is not worse than the reference."
      )
      tags$p(class = "text-muted small fst-italic", desc)
    })
    
    # CV bridge from NCA results
    output$cv_from_nca <- renderUI({
      if (!is.null(shared$nca_results)) {
        cmax_vals <- as.numeric(shared$nca_results$CMAX)
        cmax_vals <- cmax_vals[!is.na(cmax_vals) & cmax_vals > 0]
        if (length(cmax_vals) > 2) {
          cv_est <- sqrt(exp(sd(log(cmax_vals))^2) - 1)
          actionButton(ns("use_nca_cv"),
                       paste0("Use CV from my PK analysis (≈ ",
                              round(cv_est * 100, 1), "%)"),
                       class = "btn-outline-info btn-sm w-100 mt-1",
                       icon = icon("arrow-right"))
        }
      }
    })
    
    observeEvent(input$use_nca_cv, {
      cmax_vals <- as.numeric(shared$nca_results$CMAX)
      cmax_vals <- cmax_vals[!is.na(cmax_vals) & cmax_vals > 0]
      cv_est <- sqrt(exp(sd(log(cmax_vals))^2) - 1)
      updateNumericInput(session, "cv", value = round(cv_est, 3))
    })
    
    # Regulatory presets
    observeEvent(input$reg_preset, {
      if (input$reg_preset == "custom") return()
      updateNumericInput(session, "alpha", value = 0.05)
      updateNumericInput(session, "theta1", value = 0.80)
      updateNumericInput(session, "theta2", value = 1.25)
    })
    
    # Calculation
    calc_result <- reactiveVal(NULL)
    
    observeEvent(input$btn_calc, {
      # Validate inputs
      cv <- input$cv
      if (is.null(cv) || is.na(cv) || cv <= 0) {
        showNotification("CV must be greater than 0.", type = "error", duration = 5); return()
      }
      if (is.null(input$theta0) || is.na(input$theta0) || input$theta0 <= 0) {
        showNotification("Expected GMR (theta0) must be greater than 0.", type = "error", duration = 5); return()
      }
      if (is.null(input$alpha) || is.na(input$alpha) || input$alpha <= 0 || input$alpha >= 1) {
        showNotification("Alpha must be between 0 and 1.", type = "error", duration = 5); return()
      }
      
      withProgress(message = "Calculating...", value = 0.5, {
        cv_wr <- if (!is.null(input$cv_wr)) input$cv_wr else input$cv
        result <- tryCatch({
          if (input$calc_mode == "sample_size") {
            switch(input$analysis_type,
              "abe" = sampleN.TOST(alpha = input$alpha, targetpower = input$target_power,
                                   theta0 = input$theta0, theta1 = input$theta1,
                                   theta2 = input$theta2, CV = input$cv,
                                   design = input$design, method = "exact", print = FALSE),
              "abel" = sampleN.scABEL(alpha = input$alpha, targetpower = input$target_power,
                                      theta0 = input$theta0, CV = cv_wr,
                                      design = input$design, print = FALSE, nsims = 1e5),
              "rsabe" = sampleN.RSABE(alpha = input$alpha, targetpower = input$target_power,
                                      theta0 = input$theta0, CV = cv_wr,
                                      design = input$design, print = FALSE, nsims = 1e5),
              "ntid" = sampleN.NTIDFDA(alpha = input$alpha, targetpower = input$target_power,
                                       theta0 = input$theta0, CV = cv_wr,
                                       design = input$design, print = FALSE, nsims = 1e5),
              "noninf" = sampleN.noninf(alpha = input$alpha, targetpower = input$target_power,
                                        theta0 = input$theta0, margin = input$theta1,
                                        CV = input$cv, design = input$design, print = FALSE),
              "dp" = sampleN.dp(alpha = input$alpha, targetpower = input$target_power,
                                CV = input$cv, print = FALSE)
            )
          } else {
            # Power calculation — compute power at given N
            if (is.null(input$n_subjects) || is.na(input$n_subjects) || input$n_subjects < 4) {
              showNotification("At least 4 subjects are needed for a power calculation.",
                               type = "error", duration = 5)
              NULL
            } else {
            pwr <- compute_power(input$n_subjects, input$analysis_type,
                                 input$alpha, input$theta0, input$theta1,
                                 input$theta2, input$cv, cv_wr, input$design)
            if (is.na(pwr)) {
              showNotification("Power calculation not available for this analysis type with these settings.",
                               type = "warning")
              NULL
            } else {
              data.frame(Design = input$design, n = input$n_subjects,
                         power = pwr, CV = input$cv)
            }
            }  # end n_subjects guard
          }
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
          NULL
        })
        calc_result(result)
      })
    })
    
    # Result cards
    output$result_cards <- renderUI({
      req(calc_result())
      r <- calc_result()
      
      if (input$calc_mode == "sample_size") {
        n_val <- r[["Sample size"]]
        pwr_val <- r[["Achieved power"]]
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(title = "Total Subjects Needed", value = tags$strong(n_val),
                    theme = "success", showcase = icon("users")),
          value_box(title = "Achieved Power",
                    value = paste0(round(pwr_val * 100, 1), "%"),
                    theme = "info", showcase = icon("bullseye")),
          value_box(title = "Study Design", value = input$design,
                    theme = "secondary", showcase = icon("sitemap"))
        )
      } else {
        pwr_val <- r$power
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(title = "Power (Chance of Success)",
                    value = paste0(round(pwr_val * 100, 1), "%"),
                    theme = "success", showcase = icon("bullseye")),
          value_box(title = "Sample Size", value = input$n_subjects,
                    theme = "info", showcase = icon("users")),
          value_box(title = "Design", value = input$design,
                    theme = "secondary", showcase = icon("sitemap"))
        )
      }
    })
    
    # Power curve
    # Helper: compute power for any analysis type at given n
    compute_power <- function(n, type, alpha, theta0, theta1, theta2, cv, cv_wr, design) {
      if (is.null(cv_wr) || is.na(cv_wr)) cv_wr <- cv  # fallback
      tryCatch({
        switch(type,
          "abe" = power.TOST(alpha = alpha, theta0 = theta0,
                             theta1 = theta1, theta2 = theta2,
                             CV = cv, n = n, design = design, method = "exact"),
          "noninf" = power.noninf(alpha = alpha, theta0 = theta0,
                                  margin = theta1, CV = cv,
                                  n = n, design = design),
          "abel" = power.scABEL(alpha = alpha, theta0 = theta0,
                                CV = cv_wr, n = n, design = design, nsims = 1e4),
          "rsabe" = power.RSABE(alpha = alpha, theta0 = theta0,
                                CV = cv_wr, n = n, design = design, nsims = 1e4),
          "ntid" = power.NTIDFDA(alpha = alpha, theta0 = theta0,
                                 CV = cv_wr, n = n, design = design, nsims = 1e4),
          NA  # dp and others — no power function available
        )
      }, error = function(e) NA)
    }
    
    output$power_curve <- renderPlotly({
      req(calc_result())
      
      if (input$analysis_type == "dp") {
        return(plotly_empty() %>%
          layout(title = list(text = "Power curve not available for dose-proportionality",
                              font = list(size = 12))))
      }
      
      n_range <- seq(4, 200, by = 2)
      powers <- sapply(n_range, function(n) {
        compute_power(n, input$analysis_type, input$alpha, input$theta0,
                      input$theta1, input$theta2, input$cv,
                      input$cv_wr,
                      input$design)
      })
      df <- data.frame(N = n_range, Power = powers)
      df <- df[!is.na(df$Power), ]
      if (nrow(df) == 0) return(plotly_empty())
      
      target <- if (input$calc_mode == "sample_size") input$target_power else 0.80
      
      p <- ggplot(df, aes(x = N, y = Power)) +
        geom_line(color = "#2C3E50", linewidth = 1) +
        geom_hline(yintercept = target, linetype = "dashed", color = "#E74C3C") +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
        labs(x = "Total Number of Subjects", y = "Power (Probability of Success)") +
        theme_minimal(base_size = 13)
      
      r <- calc_result()
      if (input$calc_mode == "sample_size") {
        n_res <- r[["Sample size"]]
        if (!is.null(n_res)) {
          p <- p + geom_vline(xintercept = n_res, linetype = "dotted", color = "#3498DB")
        }
      }
      ggplotly(p)
    })
    
    # Sensitivity plot
    output$sensitivity_plot <- renderPlotly({
      req(calc_result())
      
      if (input$analysis_type == "dp") {
        return(plotly_empty() %>%
          layout(title = list(text = "Sensitivity not available for dose-proportionality",
                              font = list(size = 12))))
      }
      
      cv_seq <- seq(0.10, 0.60, length.out = 11)
      target_pwr <- if (input$calc_mode == "sample_size") input$target_power else 0.80
      
      results <- lapply(cv_seq, function(cv_val) {
        tryCatch({
          r <- switch(input$analysis_type,
            "abe" = sampleN.TOST(alpha = input$alpha, targetpower = target_pwr,
                                 theta0 = input$theta0, theta1 = input$theta1,
                                 theta2 = input$theta2, CV = cv_val,
                                 design = input$design, print = FALSE),
            "noninf" = sampleN.noninf(alpha = input$alpha, targetpower = target_pwr,
                                      theta0 = input$theta0, margin = input$theta1,
                                      CV = cv_val, design = input$design, print = FALSE),
            "abel" = sampleN.scABEL(alpha = input$alpha, targetpower = target_pwr,
                                    theta0 = input$theta0, CV = cv_val,
                                    design = input$design, print = FALSE, nsims = 1e4),
            "rsabe" = sampleN.RSABE(alpha = input$alpha, targetpower = target_pwr,
                                    theta0 = input$theta0, CV = cv_val,
                                    design = input$design, print = FALSE, nsims = 1e4),
            "ntid" = sampleN.NTIDFDA(alpha = input$alpha, targetpower = target_pwr,
                                     theta0 = input$theta0, CV = cv_val,
                                     design = input$design, print = FALSE, nsims = 1e4),
            NULL
          )
          if (!is.null(r)) data.frame(CV = cv_val, N = r[["Sample size"]])
          else NULL
        }, error = function(e) NULL)
      })
      df <- do.call(rbind, results[!sapply(results, is.null)])
      if (is.null(df) || nrow(df) == 0) return(plotly_empty())
      
      p <- ggplot(df, aes(x = CV * 100, y = N)) +
        geom_line(color = "#2C3E50", linewidth = 1) +
        geom_point(color = "#E74C3C", size = 3) +
        labs(x = "Within-Subject Variability (CV %)",
             y = "Required Number of Subjects") +
        theme_minimal(base_size = 13)
      ggplotly(p)
    })
    
    # Text summary
    output$result_text <- renderPrint({
      req(calc_result())
      cat("NCA Assistant — Power & Sample Size Calculation\n")
      cat(paste(rep("=", 50), collapse = ""), "\n\n")
      print(calc_result())
      cat("\nSettings:\n")
      cat("  Analysis:", input$analysis_type, "\n")
      cat("  Design:  ", input$design, "\n")
      cat("  CV:      ", input$cv, "\n")
      cat("  T/R ratio:", input$theta0, "\n")
      cat("  Limits:  [", input$theta1, ",", input$theta2, "]\n")
    })
  })
}
