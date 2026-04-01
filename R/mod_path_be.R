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
           "on your crossover data, then computes the 90% confidence interval to determine ",
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
                          choices = c("Oral / IM / SC" = "extravascular",
                                      "IV Bolus" = "iv_bolus",
                                      "IV Infusion" = "iv_infusion")),
              
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
                textInput(ns("dose_unit"), "Dose unit", value = "mg"),
                textInput(ns("time_unit"), "Time unit", value = "h"),
                textInput(ns("conc_unit"), "Conc unit", value = "ng/mL")
              ),
              selectInput(ns("trap_method"),
                          tagList("Trapezoidal method", help_trapezoidal),
                          choices = c("Linear-up / Log-down (recommended)" = "log",
                                      "Linear-up / Linear-down" = "linear"))
            )
          ),
          
          # Step 2: BE settings
          card(
            card_header(class = "bg-primary text-white",
                        "Step 2: Bioequivalence Settings"),
            card_body(
              selectInput(ns("be_design"),
                          tagList("Study design", help_be_design),
                          choices = c(
                            "Standard 2-period crossover" = "crossover_2x2",
                            "Fixed-order crossover (all subjects same sequence)" = "crossover_fixed_order",
                            "3-period crossover" = "crossover_3period",
                            "Parallel groups" = "parallel",
                            "4-period replicate crossover" = "replicate_2x2x4"
                          )),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'crossover_fixed_order'", ns("be_design")),
                tags$div(
                  class = "alert alert-warning py-2 small mb-2",
                  icon("triangle-exclamation", class = "me-1"),
                  tags$strong("Limitation: "),
                  "All subjects received treatments in the same order. ",
                  "Period and treatment effects are fully confounded and cannot be separated. ",
                  "A paired analysis (equivalent to a paired t-test on the log-transformed ",
                  "parameters) is used. Results should be interpreted with caution."
                )
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'replicate_2x2x4'", ns("be_design")),
                tags$div(
                  class = "alert alert-info py-2 small mb-2",
                  icon("circle-info", class = "me-1"),
                  tags$strong("Note: "),
                  "This app performs standard ABE (fixed 80\u2013125% limits) for all designs. ",
                  "It does not perform reference-scaled analysis (ABEL/RSABE). ",
                  "If your drug requires widened or scaled limits (CV", tags$sub("wR"),
                  " > 30%), use dedicated software for the scaled analysis."
                )
              ),
              
              selectInput(ns("model_type"),
                          tagList("Statistical model", help_mixed_effects),
                          choices = c(
                            "Fixed effects — standard for balanced data" = "fixed",
                            "Mixed effects — handles dropouts (FDA recommended)" = "mixed"
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
              )
            )
          ),
          
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
          
          navset_card_tab(
            title = "Bioequivalence Results",
            
            nav_panel(
              "Test vs. Reference Comparison",
              icon = icon("arrows-left-right"),
              tags$p(class = "text-muted small",
                     "The table shows the geometric mean ratio (Test ÷ Reference) ",
                     "and its 90% confidence interval. If the CI falls entirely within ",
                     "the acceptance limits (usually 80–125%), the formulations are bioequivalent."),
              DTOutput(ns("ci_table")),
              tags$p(class = "text-muted small mt-2",
                     icon("circle-info", class = "me-1"),
                     "Full details (N per group, acceptance limits, residual variance, ",
                     "degrees of freedom) are included in the Excel and CSV downloads."),
              hr(),
              tags$p(class = "text-muted small",
                     "Forest plot: dot = point estimate, bar = 90% CI, ",
                     "dashed lines = acceptance limits."),
              plotlyOutput(ns("forest_plot"), height = "350px")
            ),
            
            nav_panel(
              "PK Parameter Table",
              icon = icon("table"),
              tags$p(class = "text-muted small",
                     "Individual NCA results for all subject-treatment profiles."),
              checkboxInput(ns("nca_show_all"), "Show all parameters (37 columns)", FALSE),
              DTOutput(ns("nca_table"))
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
                     "Self-contained package with NCA results, BE results, settings, ",
                     "R reproducibility script, data integrity hash, and analysis summary."),
              layout_columns(
                col_widths = c(6, 6),
                textInput(ns("record_analyst"), "Analyst name (optional)",
                          value = "", placeholder = "Your name"),
                textInput(ns("record_study"), "Study name (optional)",
                          value = "", placeholder = "e.g., BE Study XYZ")
              ),
              downloadButton(ns("dl_record"), "Generate Analysis Record",
                             class = "btn-primary btn-sm w-100",
                             icon = icon("file-zipper"))
            )
          )
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
                       tags$h5("No data loaded"),
                       tags$p("Bioequivalence requires crossover data with Treatment, ",
                              "Period, and Sequence columns."),
                       actionButton(ns("goto_upload"), "Upload Data", class = "btn-warning",
                                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});")))
      } else if (is.null(shared$col_map$treatment)) {
        card(class = "border-warning",
             card_body(class = "text-center py-4",
                       icon("triangle-exclamation", class = "fa-2x text-warning mb-2"),
                       tags$h5("Treatment column not mapped"),
                       tags$p("Go to Upload & Check Data and map your Treatment/Formulation column."),
                       actionButton(ns("goto_upload2"), "Go to Data Upload", class = "btn-warning",
                                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});")))
      }
    })
    
    # Update BE parameter choices when NCA results exist
    observe({
      req(be_nca_result())
      r <- be_nca_result()
      available <- intersect(
        c("CMAX","AUCLST","AUCIFO","AUCIFP","TMAX","LAMZHL"), names(r))
      default <- intersect(c("CMAX","AUCLST","AUCIFO"), available)
      updateCheckboxGroupInput(session, "be_params",
                               choiceNames = unname(sapply(available, friendly_name)),
                               choiceValues = available,
                               selected = default)
    })
    
    # NCA results (run as part of BE)
    be_nca_result <- reactiveVal(NULL)
    be_result     <- reactiveVal(NULL)
    
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
      
      withProgress(message = "Step 1: Running NCA...", value = 0.3, {
        
        # Run NCA
        settings <- list(
          admin_route = input$admin_route,
          dose = if (use_data_dose) NA else input$dose,
          infusion_duration = 0,
          is_steady_state = FALSE,
          dose_unit = input$dose_unit,
          time_unit = input$time_unit,
          conc_unit = input$conc_unit,
          trap_method = input$trap_method,
          r2adj_threshold = 0.7,
          mw = 0, partial_aucs = NULL
        )
        
        if (use_data_dose) {
          dose_df <- shared$pk_data %>%
            group_by(.data[[cm$subject]]) %>%
            summarize(dose = max(.data[[cm$dose]], na.rm = TRUE),
                      .groups = "drop")
          settings$dose <- dose_df$dose
        }
        
        nca_res <- run_nca(shared$pk_data, cm, settings)
        
        if (is.null(nca_res)) {
          showNotification("NCA failed.", type = "error"); return()
        }
        
        be_nca_result(nca_res)
        shared$nca_results <- nca_res
        gc()  # Free NCA intermediates before BE analysis
        
        setProgress(0.5, message = "Step 2: Running BE analysis...")
        
        # Merge with design info
        trt_col <- if ("Treatment" %in% names(nca_res)) "Treatment" else cm$treatment
        subj_col <- if ("Subject" %in% names(nca_res)) "Subject" else names(nca_res)[1]
        
        merge_cols <- c(cm$subject)
        if (!is.null(cm$treatment)) merge_cols <- c(merge_cols, cm$treatment)
        extra <- c()
        if (!is.null(cm$period)) extra <- c(extra, cm$period)
        if (!is.null(cm$sequence)) extra <- c(extra, cm$sequence)
        
        if (length(extra) > 0) {
          design_df <- shared$pk_data %>%
            select(all_of(c(merge_cols, extra))) %>% distinct()
          
          if ("Subject" %in% names(nca_res) && "Treatment" %in% names(nca_res)) {
            be_data <- merge(nca_res, design_df,
                             by.x = c("Subject","Treatment"),
                             by.y = c(cm$subject, cm$treatment), all.x = TRUE)
          } else {
            be_data <- merge(nca_res, design_df,
                             by.x = names(nca_res)[1], by.y = cm$subject, all.x = TRUE)
          }
        } else { be_data <- nca_res }
        
        # BE analysis
        trt_col_be <- if ("Treatment" %in% names(be_data)) "Treatment" else cm$treatment
        subj_col_be <- if ("Subject" %in% names(be_data)) "Subject" else cm$subject
        per_col <- if (!is.null(cm$period) && cm$period %in% names(be_data)) cm$period else NULL
        seq_col <- if (!is.null(cm$sequence) && cm$sequence %in% names(be_data)) cm$sequence else NULL
        
        be_data[[trt_col_be]] <- factor(be_data[[trt_col_be]])
        trt_levels <- levels(be_data[[trt_col_be]])
        if ("Reference" %in% trt_levels)
          be_data[[trt_col_be]] <- relevel(be_data[[trt_col_be]], ref = "Reference")
        trt_levels <- levels(be_data[[trt_col_be]])
        
        params <- input$be_params
        if (is.null(params) || length(params) == 0)
          params <- intersect(c("CMAX","AUCLST","AUCIFO"), names(nca_res))
        
        alpha <- 1 - input$ci_level / 100
        ci_results <- list()
        anova_results <- list()
        
        for (param in params) {
          vals <- as.numeric(be_data[[param]])
          if (input$log_transform && param != "TMAX") {
            vals <- log(vals); vals[!is.finite(vals)] <- NA
          }
          be_data$.response <- vals
          
          # Guard: if Sequence has only 1 level, drop it (prevents lm() crash)
          if (!is.null(seq_col) && length(unique(be_data[[seq_col]])) < 2) {
            seq_col <- NULL
          }
          
          use_mixed <- input$model_type == "mixed" &&
                       input$be_design != "parallel" &&
                       input$be_design != "crossover_fixed_order" &&
                       requireNamespace("nlme", quietly = TRUE)
          
          # Build and fit model
          if (input$be_design == "parallel") {
            fit <- tryCatch(lm(as.formula(paste(".response ~", trt_col_be)),
                               data = be_data, na.action = na.exclude), error = function(e) NULL)
          } else if (input$be_design == "crossover_fixed_order") {
            # Fixed-order crossover: all subjects received same sequence.
            # Period and Treatment are confounded. Model: Subject + Treatment only.
            # Equivalent to a paired t-test on log-transformed parameters.
            fit <- tryCatch(
              lm(as.formula(paste(".response ~", subj_col_be, "+", trt_col_be)),
                 data = be_data, na.action = na.exclude),
              error = function(e) NULL)
          } else if (use_mixed) {
            fixed_terms <- c()
            if (!is.null(seq_col)) fixed_terms <- c(fixed_terms, seq_col)
            if (!is.null(per_col)) fixed_terms <- c(fixed_terms, per_col)
            fixed_terms <- c(fixed_terms, trt_col_be)
            random_f <- if (!is.null(seq_col)) paste0("~1|",seq_col,"/",subj_col_be) else paste0("~1|",subj_col_be)
            fit <- tryCatch(
              nlme::lme(fixed = as.formula(paste(".response~", paste(fixed_terms, collapse="+"))),
                        random = as.formula(random_f), data = be_data, na.action = na.exclude),
              error = function(e) {
                tryCatch(lm(as.formula(paste(".response~", paste(c(fixed_terms, subj_col_be), collapse="+"))),
                            data = be_data, na.action = na.exclude), error = function(e2) NULL)
              })
          } else {
            terms <- c()
            if (!is.null(seq_col)) terms <- c(terms, seq_col)
            terms <- c(terms, subj_col_be)
            if (!is.null(per_col)) terms <- c(terms, per_col)
            terms <- c(terms, trt_col_be)
            fit <- tryCatch(lm(as.formula(paste(".response~", paste(terms, collapse="+"))),
                               data = be_data, na.action = na.exclude), error = function(e) NULL)
          }
          
          if (is.null(fit)) { ci_results[[param]] <- data.frame(Parameter=param, Point_Est=NA, CI_Lower=NA, CI_Upper=NA, Bioequivalent=NA, stringsAsFactors=FALSE); next }
          
          anova_results[[param]] <- anova(fit)
          
          is_lme <- inherits(fit, "lme")
          trt_coef_name <- paste0(trt_col_be, trt_levels[2])
          
          n1 <- sum(be_data[[trt_col_be]] == trt_levels[1] & !is.na(be_data$.response))
          n2 <- sum(be_data[[trt_col_be]] == trt_levels[2] & !is.na(be_data$.response))
          
          if (is_lme) {
            coefs <- nlme::fixef(fit); se_tbl <- summary(fit)$tTable; mse <- summary(fit)$sigma^2
            if (trt_coef_name %in% names(coefs)) {
              diff <- coefs[trt_coef_name]; se_diff <- se_tbl[trt_coef_name,"Std.Error"]; dfe <- se_tbl[trt_coef_name,"DF"]
            } else { diff <- NA; se_diff <- NA; dfe <- NA }
          } else {
            coefs <- coef(fit); mse <- summary(fit)$sigma^2; dfe <- fit$df.residual; se_tbl <- summary(fit)$coefficients
            if (trt_coef_name %in% names(coefs)) {
              diff <- coefs[trt_coef_name]; se_diff <- se_tbl[trt_coef_name,"Std. Error"]
            } else { diff <- NA; se_diff <- NA }
          }
          
          if (is.na(diff)) { ci_results[[param]] <- data.frame(Parameter=param, Point_Est=NA, CI_Lower=NA, CI_Upper=NA, Bioequivalent=NA, stringsAsFactors=FALSE); next }
          
          t_crit <- qt(1 - alpha/2, dfe)
          ci_lo <- diff - t_crit * se_diff
          ci_hi <- diff + t_crit * se_diff
          
          if (input$log_transform && param != "TMAX") {
            pe <- exp(diff)*100; ci_lo_p <- exp(ci_lo)*100; ci_hi_p <- exp(ci_hi)*100
          } else { pe <- diff; ci_lo_p <- ci_lo; ci_hi_p <- ci_hi }
          
          be_pass <- ci_lo_p >= input$be_lower & ci_hi_p <= input$be_upper
          
          ci_results[[param]] <- data.frame(
            Parameter = param, Test = as.character(trt_levels[2]),
            Reference = as.character(trt_levels[1]),
            N_Test = n2, N_Ref = n1,
            Point_Est = round(pe, 2),
            CI_Lower = round(ci_lo_p, 2), CI_Upper = round(ci_hi_p, 2),
            BE_Lower = input$be_lower, BE_Upper = input$be_upper,
            Bioequivalent = ifelse(be_pass, "YES", "NO"),
            MSE = round(mse, 6), DF = dfe, stringsAsFactors = FALSE
          )
        }
        
        ci_df <- do.call(rbind, ci_results)
        be_result(list(ci_table = ci_df, anova = anova_results))
        shared$be_results <- be_result()
        
        setProgress(1, message = "Done!")
      })
      
      showNotification("Bioequivalence analysis complete.", type = "message")
    })
    
    # Status
    output$be_status <- renderUI({
      if (is.null(be_result())) {
        card(card_body(class = "text-center py-4 text-muted",
                       icon("arrows-left-right", class = "fa-2x mb-2"),
                       tags$h5("Configure settings and click 'Run Complete BE Analysis'")))
      }
    })
    
    # CI table
    output$ci_table <- renderDT({
      req(be_result())
      display_ci <- rename_be_columns(be_result()$ci_table)
      be_col <- if ("Bioequivalent?" %in% names(display_ci)) "Bioequivalent?" else "Bioequivalent"
      
      # Show only key columns — the rest are in the Excel export
      key_cols <- intersect(c("PK Parameter", "Ratio (%)", "90% CI Lower",
                              "90% CI Upper", "Bioequivalent?"),
                            names(display_ci))
      display_ci <- display_ci[, key_cols, drop = FALSE]
      
      datatable(display_ci,
                options = list(scrollX = TRUE, dom = "t", ordering = FALSE),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatStyle(be_col,
                    backgroundColor = styleEqual(c("YES","NO"), c("#d4edda","#f8d7da")),
                    fontWeight = "bold")
    })
    
    # Forest plot
    output$forest_plot <- renderPlotly({
      req(be_result())
      ci <- be_result()$ci_table
      ci <- ci[!is.na(ci$Point_Est), ]
      if (nrow(ci) == 0) return(plotly_empty())
      ci$Label <- sapply(ci$Parameter, friendly_name)
      ci$Label <- factor(ci$Label, levels = rev(ci$Label))
      
      p <- ggplot(ci, aes(x = Point_Est, y = Label)) +
        geom_vline(xintercept = 100, color = "grey50") +
        geom_vline(xintercept = c(ci$BE_Lower[1], ci$BE_Upper[1]),
                   color = "#E74C3C", linetype = "dashed") +
        geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper),
                       height = 0.25, linewidth = 0.8) +
        geom_point(aes(color = Bioequivalent), size = 4) +
        scale_color_manual(values = c("YES" = "#18BC9C", "NO" = "#E74C3C")) +
        labs(x = paste0("Geometric Mean Ratio (%) with ", input$ci_level, "% CI"),
             y = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom", panel.grid.major.y = element_blank())
      ggplotly(p)
    })
    
    # NCA table
    output$nca_table <- renderDT({
      req(be_nca_result())
      display_nca <- rename_nca_columns(be_nca_result())
      
      if (!isTRUE(input$nca_show_all)) {
        key_cols <- intersect(
          c("Subject", "Treatment",
            "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
            "AUC to Last Point", "AUC to Infinity (observed)",
            "Half-Life (h)", "Apparent Clearance (CL/F)",
            "Apparent Volume (Vz/F)", "Adjusted R-squared"),
          names(display_nca))
        display_nca <- display_nca[, key_cols, drop = FALSE]
      }
      
      datatable(display_nca, options = list(scrollX = TRUE, scrollY = "400px",
                                            pageLength = 50, dom = "frtip"),
                rownames = FALSE, class = "compact stripe hover") %>%
        formatSignif(columns = which(sapply(display_nca, is.numeric)), digits = 4)
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
    
    # Downloads
    output$dl_be_xlsx <- downloadHandler(
      filename = function() paste0("BE_report_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(be_result())
        wb <- createWorkbook()
        addWorksheet(wb, "Confidence_Intervals")
        writeData(wb, 1, rename_be_columns(be_result()$ci_table))
        if (!is.null(be_nca_result())) {
          addWorksheet(wb, "NCA_Parameters")
          writeData(wb, 2, rename_nca_columns(be_nca_result()))
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
        write.csv(rename_be_columns(be_result()$ci_table), file, row.names=FALSE)
      }
    )
    
    # Complete Analysis Record
    output$dl_record <- downloadHandler(
      filename = function() {
        study <- if (nchar(input$record_study) > 0)
          gsub("[^A-Za-z0-9_-]", "_", input$record_study) else "BE"
        paste0("Analysis_Record_", study, "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(be_nca_result(), be_result(), shared$col_map, shared$study_info)
        
        withProgress(message = "Generating analysis record...", value = 0.3, {
          r <- be_nca_result()
          
          settings <- list(
            admin_route     = input$admin_route,
            dose            = input$dose,
            infusion_duration = 0,
            is_steady_state = FALSE,
            dose_unit       = input$dose_unit,
            time_unit       = input$time_unit,
            conc_unit       = input$conc_unit,
            trap_method     = input$trap_method,
            r2adj_threshold = 0.7,
            n_obs           = nrow(shared$pk_data)
          )
          
          si <- shared$study_info
          original_path <- NULL
          original_name <- si$file_name
          
          uploads <- list.files("/mnt/user-data/uploads", full.names = TRUE)
          if (length(uploads) > 0) {
            match <- grep(tools::file_path_sans_ext(original_name), uploads, value = TRUE)
            original_path <- if (length(match) > 0) match[1] else uploads[length(uploads)]
          }
          if (is.null(original_path) || !file.exists(original_path)) {
            original_path <- file.path(tempdir(), original_name)
            if (!is.null(shared$raw_data))
              write.csv(shared$raw_data, original_path, row.names = FALSE)
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
            be_results     = be_result()
          )
        })
      }
    )
  })
}
