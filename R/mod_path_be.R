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
                          choices = c("Oral / IM / SC (extravascular)" = "extravascular",
                                      "IV Bolus (injected into vein at once)" = "iv_bolus",
                                      "IV Infusion (drip over time)" = "iv_infusion")),
              
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
                textInput(ns("dose_unit"), "Dose", value = "mg"),
                textInput(ns("time_unit"), "Time", value = "h"),
                textInput(ns("conc_unit"), "Conc", value = "ng/mL")
              ),
              selectInput(ns("trap_method"),
                          tagList("Trapezoidal method", help_trapezoidal),
                          choices = c("Linear-up / Log-down" = "log",
                                      "Linear-up / Linear-down" = "linear")),
              sliderInput(ns("r2adj_be"),
                          tagList("Minimum R\u00B2 for half-life estimation", help_r2adj),
                          min = 0.5, max = 1, value = 0.7, step = 0.05),
              checkboxInput(ns("is_ss"),
                            tagList("Steady-state (drug given repeatedly)", help_steady_state),
                            value = FALSE)
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
          uiOutput(ns("ss_note")),
          uiOutput(ns("balance_note")),
          
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
      available <- intersect(
        c("CMAX","AUCLST","AUCIFO","AUCIFP","TMAX","LAMZHL"), names(r))
      # At steady state, AUCLST = AUCτ and is the correct primary parameter.
      # AUCIFO is not pharmacokinetically meaningful at steady state,
      # so exclude it from the default selection.
      default <- if (isTRUE(input$is_ss)) {
        intersect(c("CMAX","AUCLST"), available)
      } else {
        intersect(c("CMAX","AUCLST","AUCIFO"), available)
      }
      updateCheckboxGroupInput(session, "be_params",
                               choiceNames = unname(sapply(available, friendly_name)),
                               choiceValues = available,
                               selected = default)
    })
    
    # NCA results (run as part of BE)
    be_nca_result  <- reactiveVal(NULL)
    be_result      <- reactiveVal(NULL)
    balance_result <- reactiveVal(NULL)   # stores imbalance info for persistent alert
    
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
      
      withProgress(message = "Step 1: Running NCA...", value = 0.3, {
        
        # Run NCA
        settings <- list(
          admin_route = input$admin_route,
          dose = if (use_data_dose) NA else input$dose,
          infusion_duration = 0,
          is_steady_state = isTRUE(input$is_ss),
          dose_unit = input$dose_unit,
          time_unit = input$time_unit,
          conc_unit = input$conc_unit,
          trap_method = input$trap_method,
          r2adj_threshold = input$r2adj_be,
          mw = 0, partial_aucs = NULL
        )
        
        if (use_data_dose) {
          dose_df <- shared$pk_data %>%
            group_by(.data[[cm$subject]]) %>%
            summarize(dose = max(.data[[cm$dose]], na.rm = TRUE),
                      .groups = "drop")
          if (any(!is.finite(dose_df$dose) | dose_df$dose <= 0)) {
            showNotification(
              "Some subjects have missing or zero dose values. Check the Dose column in your data.",
              type = "error", duration = 8)
            return()
          }
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
        
        if (length(trt_levels) != 2) {
          showNotification(
            paste0("Treatment column must have exactly 2 levels (found ",
                   length(trt_levels), ": ", paste(trt_levels, collapse = ", "),
                   "). Bioequivalence compares two formulations."),
            type = "error", duration = 10)
          return()
        }
        
        if ("Reference" %in% trt_levels)
          be_data[[trt_col_be]] <- relevel(be_data[[trt_col_be]], ref = "Reference")
        trt_levels <- levels(be_data[[trt_col_be]])
        
        params <- input$be_params
        if (is.null(params) || length(params) == 0)
          params <- intersect(c("CMAX","AUCLST","AUCIFO"), names(nca_res))
        
        # Warn if design selection may not match the data
        if (!is.null(seq_col) && length(unique(be_data[[seq_col]])) < 2 &&
            input$be_design %in% c("crossover_2x2", "crossover_3period", "replicate_2x2x4")) {
          showNotification(
            paste0("Your data has only one sequence level ('",
                   unique(be_data[[seq_col]])[1],
                   "'). This looks like a fixed-order crossover. ",
                   "Consider selecting 'Fixed-order crossover' as the study design."),
            type = "warning", duration = 10)
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
          
          anova_results[[param]] <- tryCatch(anova(fit), error = function(e) NULL)
          
          is_lme <- inherits(fit, "lme")
          trt_coef_name <- paste0(trt_col_be, trt_levels[2])
          
          n1 <- sum(be_data[[trt_col_be]] == trt_levels[1] & !is.na(be_data$.response))
          n2 <- sum(be_data[[trt_col_be]] == trt_levels[2] & !is.na(be_data$.response))
          
          coef_result <- tryCatch({
            if (is_lme) {
              coefs <- nlme::fixef(fit); se_tbl <- summary(fit)$tTable; mse <- summary(fit)$sigma^2
              if (trt_coef_name %in% names(coefs) && !is.na(coefs[trt_coef_name])) {
                list(diff = coefs[trt_coef_name], se = se_tbl[trt_coef_name,"Std.Error"],
                     dfe = se_tbl[trt_coef_name,"DF"], mse = mse)
              } else { NULL }
            } else {
              coefs <- coef(fit); mse <- summary(fit)$sigma^2; dfe <- fit$df.residual
              se_tbl <- summary(fit)$coefficients
              if (trt_coef_name %in% names(coefs) && !is.na(coefs[trt_coef_name])) {
                list(diff = coefs[trt_coef_name], se = se_tbl[trt_coef_name,"Std. Error"],
                     dfe = dfe, mse = mse)
              } else { NULL }
            }
          }, error = function(e) NULL)
          
          if (is.null(coef_result)) {
            # Provide specific guidance based on the likely cause
            reason <- if (!is.null(seq_col) && length(unique(be_data[[seq_col]])) < 2) {
              "All subjects have the same sequence \u2014 try selecting 'Fixed-order crossover' as the study design."
            } else if (!is.null(per_col) && length(unique(be_data[[per_col]])) < 2) {
              "Only one period found \u2014 try selecting 'Parallel groups' as the study design."
            } else {
              "The statistical model could not estimate the treatment effect. Check that the study design selection matches your data."
            }
            showNotification(
              paste0("Could not compute BE results for ", friendly_name(param), ": ", reason),
              type = "error", duration = 12)
            ci_results[[param]] <- data.frame(
              Parameter=param, Point_Est=NA, CI_Lower=NA, CI_Upper=NA,
              Bioequivalent=reason,
              stringsAsFactors=FALSE)
            next
          }
          
          diff <- coef_result$diff; se_diff <- coef_result$se
          dfe <- coef_result$dfe; mse <- coef_result$mse
          
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
        balance_result(balance_info)  # persist for the alert panel
        
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

    # Steady-state note — shown in results area when SS is active
    output$ss_note <- renderUI({
      if (!isTRUE(input$is_ss) || is.null(be_result())) return(NULL)
      tags$div(
        class = "alert alert-info py-2 small mb-2",
        icon("circle-info", class = "me-1"),
        tags$strong("Steady-state analysis: "),
        "AUC to Last Point (AUCLST) represents the AUC within the dosing interval (AUC\u03C4). ",
        "CL/F is calculated as Dose/AUC\u03C4, which is the correct formula at steady state. ",
        "AUC to infinity is not pharmacokinetically meaningful during repeated dosing and has ",
        "been removed from the default parameter selection. ",
        "The primary BE parameters are Cmax and AUC\u03C4 (shown as AUC to Last Point)."
      )
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
        geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper),
                      width = 0.25, linewidth = 0.8,
                      orientation = "y") +
        geom_point(aes(color = Bioequivalent), size = 4) +
        scale_color_manual(values = c("YES" = "#18BC9C", "NO" = "#E74C3C")) +
        labs(x = paste0("Geometric Mean Ratio (%) with ", input$ci_level, "% CI"),
             y = NULL, color = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none", panel.grid.major.y = element_blank())
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(b = 60),
               xaxis = list(title = paste0("Geometric Mean Ratio (%) with ",
                                           input$ci_level, "% CI")))
    })
    
    # NCA table
    output$nca_table <- renderDT({
      req(be_nca_result())
      display_nca <- rename_nca_columns(be_nca_result())
      
      if (!isTRUE(input$nca_show_all)) {
        # AUCPEO included so >20% extrapolation is visible in default view
        key_cols <- intersect(
          c("Subject", "Treatment",
            "Peak Concentration (Cmax)", "Time of Peak (Tmax)",
            "AUC to Last Point", "AUC to Infinity (observed)",
            "AUC % Extrapolated (observed)",
            "Half-Life (h)", "Apparent Clearance (CL/F)",
            "Apparent Volume (Vz/F)", "Adjusted R-squared"),
          names(display_nca))
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
          facet_wrap(as.formula(paste("~", cm$subject)), scales = "free_y") +
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
    
    # Update profile selector after NCA runs
    observe({
      req(be_nca_result())
      r <- be_nca_result()
      if ("Subject" %in% names(r) && "Treatment" %in% names(r)) {
        choices <- paste(r$Subject, "|", r$Treatment)
      } else {
        choices <- r[[1]]
      }
      updateSelectInput(session, "lz_profile", choices = choices)
    })
    
    # Reset override when profile changes
    observeEvent(input$lz_profile, { lz_state$override <- NULL }, ignoreInit = TRUE)
    
    # Get data for selected profile
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
    
    # Half-life status
    output$lz_status <- renderUI({
      sd <- lz_sub_data(); req(length(sd$time) >= 3)
      lz <- if (!is.null(lz_state$override)) lz_state$override
            else estimate_lambda_z(sd$time, sd$conc, input$r2adj_be)
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
              else estimate_lambda_z(sd$time, sd$conc, input$r2adj_be)
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
              else estimate_lambda_z(sd$time, sd$conc, input$r2adj_be)
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
      
      if (is.na(lz_new) || lz_new <= 0) {
        showNotification("The selected points have an ascending or flat slope \u2014 select points from the descending part of the curve.",
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
      
      # Get original λz for audit logging
      orig_lz <- estimate_lambda_z(sd$time, sd$conc, input$r2adj_be)
      
      lz_state$override <- list(
        lambda_z = as.numeric(lz_new), half_life = as.numeric(hl_new),
        intercept = as.numeric(int_new), r2adj = as.numeric(r2adj),
        n_points = n_pts, time_used = t_sel, message = "User-selected"
      )
      
      # Log the override for audit trail
      sel <- input$lz_profile
      lz_state$overrides_log[[sel]] <- list(
        profile = sel,
        original_lambda_z = if (!is.na(orig_lz$lambda_z)) as.numeric(orig_lz$lambda_z) else NA,
        adjusted_lambda_z = as.numeric(lz_new),
        original_r2adj = if (!is.na(orig_lz$r2adj)) as.numeric(orig_lz$r2adj) else NA,
        adjusted_r2adj = if (!is.na(r2adj)) as.numeric(r2adj) else NA,
        points_used = length(t_sel)
      )
      
      # Update NCA results
      r <- be_nca_result()
      if (!is.null(r)) {
        if (grepl(" \\| ", sel)) {
          parts <- strsplit(sel, " \\| ")[[1]]
          row_idx <- which(r$Subject == trimws(parts[1]) & r$Treatment == trimws(parts[2]))
        } else {
          row_idx <- which(r[[1]] == sel)
        }
        if (length(row_idx) == 1) {
          r$LAMZ[row_idx]    <- lz_new
          r$LAMZHL[row_idx]  <- hl_new
          if ("R2ADJ" %in% names(r)) r$R2ADJ[row_idx] <- r2adj
          if ("LAMZNPT" %in% names(r)) r$LAMZNPT[row_idx] <- n_pts
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
            is_steady_state = isTRUE(input$is_ss),
            dose_unit       = input$dose_unit,
            time_unit       = input$time_unit,
            conc_unit       = input$conc_unit,
            trap_method     = input$trap_method,
            r2adj_threshold = input$r2adj_be,
            n_obs           = nrow(shared$pk_data)
          )
          
          si <- shared$study_info
          original_name <- si$file_name
          original_path <- si$file_path
          
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
            be_results     = be_result(),
            lz_overrides   = if (length(lz_state$overrides_log) > 0) lz_state$overrides_log else NULL,
            viz_settings   = shared$viz_settings
          )
        })
      }
    )
  })
}
