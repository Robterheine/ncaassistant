# ============================================================================
# NCA Assistant — Path 1: Plan a Study (Power & Sample Size)  v1.2
# ============================================================================
# Redesigned with a guided three-step flow.
# Study type embeds regulatory agency context (no separate agency selector).
# CV is entered as a percentage (e.g., 20 for 20%) — converted internally.
# Invalid design/type combinations are prevented, not caught after the fact.
# Dose-proportionality and non-inferiority removed (out of scope).

path_power_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "container-fluid py-3",
    style = "max-width: 1200px; margin: 0 auto;",

    tags$h4(class = "fw-bold mb-1",
            icon("calculator", class = "text-info me-2"),
            "Plan a Study \u2014 Power & Sample Size"),
    tags$p(class = "text-muted mb-3",
           "Calculate how many subjects you need, or check the probability of ",
           "success for a planned study. No data upload required."),

    layout_sidebar(
      fillable = FALSE,

      # ======= SIDEBAR =======================================================
      sidebar = sidebar(
        title = tagList("Settings", help_what_is_power),
        width = 420,
        open  = TRUE,

        # ---------------------------------------------------------------
        # STEP 1: What do you want to calculate?
        # ---------------------------------------------------------------
        card(
          card_header(class = "bg-primary text-white",
                      "Step 1: What do you want to calculate?"),
          card_body(
            radioButtons(
              ns("calc_mode"), label = NULL,
              choices = c(
                "How many subjects do I need? (find sample size)" = "sample_size",
                "What is the chance of success for my planned study? (calculate power)" = "power"
              ),
              selected = "sample_size"
            )
          )
        ),

        # ---------------------------------------------------------------
        # STEP 2: What kind of study?
        # ---------------------------------------------------------------
        card(
          card_header(class = "bg-primary text-white",
                      "Step 2: What kind of study?"),
          card_body(
            radioButtons(
              ns("analysis_type"), label = NULL,
              choices = c(
                "Standard bioequivalence  (80\u2013125% limits, any region)" = "abe",
                "Highly variable drug \u2014 EMA / WHO  (widened limits, replicate design)" = "abel",
                "Highly variable drug \u2014 US FDA  (scaled approach, replicate design)" = "rsabe",
                "Narrow therapeutic index drug \u2014 US FDA  (tighter limits, 4-period replicate)" = "ntid"
              ),
              selected = "abe"
            ),
            uiOutput(ns("type_desc"))
          )
        ),

        # ---------------------------------------------------------------
        # STEP 3: Study design (filtered to valid options)
        # ---------------------------------------------------------------
        card(
          card_header(class = "bg-primary text-white",
                      "Step 3: Study design"),
          card_body(
            uiOutput(ns("design_ui"))
          )
        ),

        # ---------------------------------------------------------------
        # Parameters
        # ---------------------------------------------------------------
        card(
          card_header(class = "bg-primary text-white", "Parameters"),
          card_body(

            # Sample size mode: target power
            conditionalPanel(
              condition = sprintf("input['%s'] == 'sample_size'", ns("calc_mode")),
              numericInput(ns("target_power"),
                           "Target power — e.g., 80 means 80% chance of success (%)",
                           value = 80, min = 50, max = 99.9, step = 1)
            ),

            # Power mode: number of subjects
            conditionalPanel(
              condition = sprintf("input['%s'] == 'power'", ns("calc_mode")),
              numericInput(ns("n_subjects"),
                           "Total number of subjects in your study",
                           value = 24, min = 4, step = 2)
            ),

            # Expected T/R ratio
            numericInput(ns("theta0"),
                         tagList("Expected Test/Reference ratio (%)",
                                 help_theta0),
                         value = 95, min = 50, max = 200, step = 1),
            tags$p(class = "text-muted small mt-n2 mb-2",
                   "95 means you expect the Test to be 95% of the Reference. ",
                   "Use 95 if unsure \u2014 this is the conservative standard assumption."),

            # Main CV
            numericInput(ns("cv"),
                         tagList("Within-subject variability (CV %)", help_cv),
                         value = 20, min = 1, max = 200, step = 1),
            tags$p(class = "text-muted small mt-n2 mb-2",
                   "Enter as a percentage: 20 means 20% variability. ",
                   "Find this from a pilot study, literature, or your NCA results."),

            # CV from NCA bridge
            uiOutput(ns("cv_from_nca")),

            # CVwr — only for HVD/NTID
            conditionalPanel(
              condition = sprintf(
                "['abel','rsabe','ntid'].includes(input['%s'])",
                ns("analysis_type")),
              numericInput(ns("cv_wr"),
                           tagList("Within-subject CV of the Reference product (%)",
                                   help_cv_wr),
                           value = 30, min = 1, max = 200, step = 1),
              tags$p(class = "text-muted small mt-n2 mb-2",
                     "For scaled approaches, you also need the variability of the ",
                     "Reference formulation specifically. ",
                     "If unknown, use the same value as CV above.")
            ),

            # Advanced: limits + alpha — content adapts to study type
            tags$details(
              tags$summary(
                class  = "fw-semibold small mb-1",
                style  = "cursor: pointer;",
                icon("sliders", class = "me-1"),
                "Advanced: acceptance limits & significance level"
              ),
              uiOutput(ns("limits_ui"))
            )
          )
        ),

        # Calculate button
        actionButton(ns("btn_calc"), "Calculate",
                     class = "btn-success btn-lg w-100",
                     icon  = icon("calculator"))
      ),

      # ======= RESULTS =======================================================
      tagList(
        uiOutput(ns("result_cards")),

        navset_card_tab(
          title = "Detailed Results",

          nav_panel(
            "Power vs. Number of Subjects",
            icon = icon("chart-line"),
            tags$p(class = "text-muted small",
                   "Shows how the probability of concluding bioequivalence ",
                   "increases with more subjects. The red dashed line is your target power."),
            plotlyOutput(ns("power_curve"), height = "420px")
          ),

          nav_panel(
            "Sample Size vs. Variability",
            icon = icon("table-cells"),
            tags$p(class = "text-muted small",
                   "Shows how the required number of subjects changes across ",
                   "different variability levels. A wider CV requires more subjects."),
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


# =============================================================================
# SERVER
# =============================================================================

path_power_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Resolve NTID function names (renamed between PowerTOST versions) ---
    # Older versions: sampleN.NTIDFDA / power.NTIDFDA
    # Newer versions: sampleN.NTID   / power.NTID
    ntid_sampleN <- if (existsFunction("sampleN.NTIDFDA")) sampleN.NTIDFDA
                    else if (existsFunction("sampleN.NTID")) sampleN.NTID
                    else NULL
    ntid_power   <- if (existsFunction("power.NTIDFDA"))  power.NTIDFDA
                    else if (existsFunction("power.NTID"))  power.NTID
                    else NULL

    # ---- Valid designs per analysis type ------------------------------------
    valid_designs <- list(
      abe    = c("2x2"    = "Standard 2-period crossover",
                 "2x2x3"  = "3-period crossover",
                 "2x3x3"  = "Williams design (3-sequence, 3-period)",
                 "2x2x4"  = "4-period replicate crossover",
                 "parallel" = "Parallel groups"),
      abel   = c("2x2x3"  = "3-period crossover",
                 "2x3x3"  = "Williams design (3-sequence, 3-period)",
                 "2x2x4"  = "4-period replicate crossover"),
      rsabe  = c("2x2x3"  = "3-period crossover",
                 "2x3x3"  = "Williams design (3-sequence, 3-period)",
                 "2x2x4"  = "4-period replicate crossover"),
      ntid   = c("2x2x4"  = "4-period replicate crossover")
    )

    default_design <- list(
      abe  = "2x2",
      abel = "2x2x4",
      rsabe = "2x2x4",
      ntid = "2x2x4"
    )

    # ---- Design selector (filtered to valid options) ------------------------
    output$design_ui <- renderUI({
      atype   <- input$analysis_type %||% "abe"
      choices <- valid_designs[[atype]]
      def     <- default_design[[atype]]
      tagList(
        selectizeInput(ns("design"), label = NULL,
                       choices  = setNames(names(choices), choices),
                       selected = def,
                       options  = list(dropdownParent = "body")),
        if (atype == "ntid") {
          tags$p(class = "text-muted small fst-italic",
                 "The FDA NTID method requires a 4-period replicate design.")
        } else if (atype %in% c("abel", "rsabe")) {
          tags$p(class = "text-muted small fst-italic",
                 "Scaled approaches require a replicate or 3-period design so that ",
                 "within-subject variability of the Reference can be estimated.")
        } else {
          tags$p(class = "text-muted small fst-italic",
                 "For standard bioequivalence, all designs are available. ",
                 "The standard 2-period crossover is the most commonly used.")
        }
      )
    })

    # ---- Study type description (with limits) --------------------------------
    output$type_desc <- renderUI({
      atype <- input$analysis_type %||% "abe"
      switch(atype,
        "abe" = tagList(
          tags$p(class = "text-muted small fst-italic mt-1 mb-1",
                 "The standard approach used by all regulatory agencies. ",
                 "Tests whether the 90% confidence interval of the Test/Reference ",
                 "ratio falls entirely within fixed limits."),
          tags$div(
            class = "alert alert-light py-2 small mb-0",
            style = "border-left: 3px solid #3498DB;",
            icon("info-circle", class = "text-info me-1"),
            tags$strong("Acceptance limits: "), "80\u2013125%",
            tags$span(class = "text-muted ms-2",
                      "(both the lower and upper CI bound must fall within this range)")
          )
        ),
        "abel" = tagList(
          tags$p(class = "text-muted small fst-italic mt-1 mb-1",
                 "For drugs with high within-subject variability (CV > 30%). ",
                 "The EMA and WHO allow the acceptance limits to widen based on ",
                 "the measured variability of the Reference product. ",
                 "Requires a replicate or 3-period design."),
          tags$div(
            class = "alert alert-light py-2 small mb-0",
            style = "border-left: 3px solid #F39C12;",
            icon("info-circle", class = "text-warning me-1"),
            tags$strong("Acceptance limits: "), "variable \u2014 widen with Reference CV",
            tags$br(),
            tags$span(class = "text-muted",
                      "At CVwr = 30%: approx. 74.6\u2013134.2% \u2022 ",
                      "At CVwr = 40%: approx. 71.2\u2013140.4% \u2022 ",
                      "At CVwr \u2265 50%: maximum 69.8\u2013143.2%"),
            tags$br(),
            tags$span(class = "text-muted",
                      "Computed automatically from your Reference CV input. ",
                      "You do not set these manually.")
          )
        ),
        "rsabe" = tagList(
          tags$p(class = "text-muted small fst-italic mt-1 mb-1",
                 "The US FDA\u2019s approach for highly variable drugs (CV > 30%). ",
                 "Uses a reference-scaling criterion rather than fixed limits. ",
                 "Requires a replicate or 3-period design."),
          tags$div(
            class = "alert alert-light py-2 small mb-0",
            style = "border-left: 3px solid #F39C12;",
            icon("info-circle", class = "text-warning me-1"),
            tags$strong("Acceptance limits: "), "reference-scaled \u2014 no fixed bounds",
            tags$br(),
            tags$span(class = "text-muted",
                      "The FDA uses a scaled criterion: the test-to-reference variance ratio ",
                      "must be within a regulatory constant (\u03b8 = 2.494) times the ",
                      "Reference variance. Additionally, the point estimate must fall ",
                      "within 80\u2013125%. Limits are computed internally by the simulation.")
          )
        ),
        "ntid" = tagList(
          tags$p(class = "text-muted small fst-italic mt-1 mb-1",
                 "For drugs with a narrow margin between therapeutic and toxic doses ",
                 "(e.g., warfarin, cyclosporine, tacrolimus). ",
                 "The FDA requires tighter limits and a 4-period replicate design."),
          tags$div(
            class = "alert alert-light py-2 small mb-0",
            style = "border-left: 3px solid #E74C3C;",
            icon("info-circle", class = "text-danger me-1"),
            tags$strong("Acceptance limits: "), "90\u2013111.11%",
            tags$br(),
            tags$span(class = "text-muted",
                      "Tighter than standard BE (80\u2013125%). The FDA also requires that ",
                      "the within-subject variance of the Test is not greater than that ",
                      "of the Reference (variance ratio test). Both criteria are evaluated ",
                      "together in the simulation.")
          )
        )
      )
    })

    # ---- Limits UI (adapts to study type) ------------------------------------
    # ABE: editable limits (user may have a protocol with different limits)
    # ABEL/RSABE/NTID: explanation only — limits are computed, not set manually
    output$limits_ui <- renderUI({
      atype <- input$analysis_type %||% "abe"
      tags$div(
        class = "mt-2",
        if (atype == "abe") {
          tagList(
            tags$p(class = "text-muted small",
                   "Standard limits are 80\u2013125% (0.80\u20131.25). ",
                   "Only change if your protocol specifies different limits."),
            layout_columns(
              col_widths = c(6, 6),
              numericInput(ns("theta1"), "Lower limit", value = 0.80,
                           min = 0.5, max = 1.0, step = 0.01),
              numericInput(ns("theta2"), "Upper limit", value = 1.25,
                           min = 1.0, max = 2.0, step = 0.01)
            )
          )
        } else if (atype == "ntid") {
          tagList(
            tags$p(class = "text-muted small",
                   "The FDA NTID method uses fixed limits of 90\u2013111.11% (0.90\u20131.111). ",
                   "These are set by regulation and cannot be changed."),
            layout_columns(
              col_widths = c(6, 6),
              tags$div(
                tags$label(class = "form-label", "Lower limit"),
                tags$input(class = "form-control form-control-sm",
                           value = "0.90", disabled = NA)
              ),
              tags$div(
                tags$label(class = "form-label", "Upper limit"),
                tags$input(class = "form-control form-control-sm",
                           value = "1.111", disabled = NA)
              )
            ),
            # Still need theta1/theta2 inputs in DOM for server validation —
            # hidden, pre-set to NTID values
            tags$div(style = "display:none;",
                     numericInput(ns("theta1"), NULL, value = 0.90),
                     numericInput(ns("theta2"), NULL, value = 1.111))
          )
        } else {
          # ABEL / RSABE: limits are dynamic / reference-scaled
          tags$p(class = "text-muted small",
                 "For this study type the acceptance limits are computed during ",
                 "the simulation from your Reference CV. They are not set manually. ",
                 "See the description above for typical ranges.",
                 tags$br(),
                 # Hidden inputs with ABE defaults so server validation
                 # doesn't trip on missing theta1/theta2
                 tags$div(style = "display:none;",
                          numericInput(ns("theta1"), NULL, value = 0.80),
                          numericInput(ns("theta2"), NULL, value = 1.25)))
        },
        # Significance level always shown
        tags$div(
          class = "mt-2",
          numericInput(ns("alpha"),
                       "Significance level (almost always 0.05)",
                       value = 0.05, min = 0.001, max = 0.2, step = 0.005)
        )
      )
    })

    # ---- CV bridge from NCA results -----------------------------------------
    output$cv_from_nca <- renderUI({
      if (is.null(shared$nca_results)) return(NULL)
      cmax_vals <- suppressWarnings(as.numeric(shared$nca_results$CMAX))
      cmax_vals <- cmax_vals[!is.na(cmax_vals) & cmax_vals > 0]
      if (length(cmax_vals) < 3) return(NULL)
      # Geometric CV as percentage
      cv_est_pct <- round(sqrt(exp(sd(log(cmax_vals))^2) - 1) * 100, 1)
      actionButton(
        ns("use_nca_cv"),
        paste0("Use CV from my PK analysis (\u2248 ", cv_est_pct, "%)"),
        class = "btn-outline-info btn-sm w-100 mt-1",
        icon  = icon("arrow-right")
      )
    })

    observeEvent(input$use_nca_cv, {
      cmax_vals <- suppressWarnings(as.numeric(shared$nca_results$CMAX))
      cmax_vals <- cmax_vals[!is.na(cmax_vals) & cmax_vals > 0]
      cv_est_pct <- round(sqrt(exp(sd(log(cmax_vals))^2) - 1) * 100, 1)
      updateNumericInput(session, "cv", value = cv_est_pct)
    })

    # ---- Input validation helper --------------------------------------------
    validate_inputs <- function() {
      # CV
      cv_pct <- input$cv
      if (is.null(cv_pct) || is.na(cv_pct) || cv_pct <= 0) {
        showNotification("CV must be greater than 0%.", type = "error", duration = 6)
        return(FALSE)
      }
      if (cv_pct > 150) {
        showNotification(
          paste0("CV of ", cv_pct, "% is unusually high. ",
                 "Please check — CV is entered as a percentage (e.g., 20 for 20%)."),
          type = "warning", duration = 8)
        # Allow but warn — don't block
      }

      # CVwr
      atype <- input$analysis_type %||% "abe"
      if (atype %in% c("abel", "rsabe", "ntid")) {
        cv_wr_pct <- input$cv_wr
        if (is.null(cv_wr_pct) || is.na(cv_wr_pct) || cv_wr_pct <= 0) {
          showNotification("Reference CV must be greater than 0%.",
                           type = "error", duration = 6)
          return(FALSE)
        }
      }

      # theta0 (entered as %)
      t0 <- input$theta0
      if (is.null(t0) || is.na(t0) || t0 <= 0) {
        showNotification("Expected T/R ratio must be greater than 0%.",
                         type = "error", duration = 6)
        return(FALSE)
      }

      # alpha
      alp <- input$alpha
      if (is.null(alp) || is.na(alp) || alp <= 0 || alp >= 1) {
        showNotification("Significance level must be between 0 and 1.",
                         type = "error", duration = 6)
        return(FALSE)
      }

      # Limits
      t1 <- input$theta1; t2 <- input$theta2
      if (is.null(t1) || is.null(t2) || is.na(t1) || is.na(t2)) {
        showNotification("Please set both acceptance limits.",
                         type = "error", duration = 6)
        return(FALSE)
      }
      if (t1 >= t2) {
        showNotification("Lower limit must be less than upper limit.",
                         type = "error", duration = 6)
        return(FALSE)
      }

      # Power mode: N
      if (input$calc_mode == "power") {
        n <- input$n_subjects
        if (is.null(n) || is.na(n) || n < 4) {
          showNotification("At least 4 subjects are needed.",
                           type = "error", duration = 6)
          return(FALSE)
        }
      }

      # Sample size mode: target power
      if (input$calc_mode == "sample_size") {
        tp <- input$target_power
        if (is.null(tp) || is.na(tp) || tp <= 50 || tp >= 100) {
          showNotification("Target power must be between 50% and 100%.",
                           type = "error", duration = 6)
          return(FALSE)
        }
      }

      TRUE
    }

    # ---- Convert % inputs to decimals for PowerTOST -------------------------
    pct_to_dec <- function(pct) pct / 100

    # ---- Compute power for a given N ----------------------------------------
    compute_power <- function(n, atype, alpha, theta0_dec, theta1, theta2,
                              cv_dec, cv_wr_dec, design) {
      if (is.null(cv_wr_dec) || is.na(cv_wr_dec)) cv_wr_dec <- cv_dec
      tryCatch(
        switch(atype,
          "abe"   = power.TOST(alpha = alpha, theta0 = theta0_dec,
                               theta1 = theta1, theta2 = theta2,
                               CV = cv_dec, n = n, design = design,
                               method = "exact"),
          "abel"  = power.scABEL(alpha = alpha, theta0 = theta0_dec,
                                 CV = cv_wr_dec, n = n, design = design,
                                 nsims = 1e4),
          "rsabe" = power.RSABE(alpha = alpha, theta0 = theta0_dec,
                                CV = cv_wr_dec, n = n, design = design,
                                nsims = 1e4),
          "ntid"  = {
            if (is.null(ntid_power)) stop("NTID power function not found in PowerTOST")
            ntid_power(alpha = alpha, theta0 = theta0_dec,
                       CV = cv_wr_dec, n = n, design = design,
                       nsims = 1e4)
          },
          NA_real_
        ),
        error = function(e) NA_real_
      )
    }

    # ---- Main calculation ---------------------------------------------------
    calc_result <- reactiveVal(NULL)

    # ---- Resolve correct limits per study type ------------------------------
    # For NTID the limits are fixed by regulation regardless of user input.
    # For ABEL/RSABE the theta1/theta2 inputs are hidden placeholders — the
    # limits are not used by those PowerTOST functions directly.
    resolve_limits <- function(atype) {
      switch(atype,
        "ntid" = list(theta1 = 0.90, theta2 = 1.111),
        list(theta1 = input$theta1 %||% 0.80,
             theta2 = input$theta2 %||% 1.25)
      )
    }

    observeEvent(input$btn_calc, {
      if (!validate_inputs()) return()

      atype      <- input$analysis_type %||% "abe"
      design     <- input$design        %||% "2x2"
      alpha      <- input$alpha
      theta0_dec <- pct_to_dec(input$theta0)
      lims       <- resolve_limits(atype)
      theta1     <- lims$theta1
      theta2     <- lims$theta2
      cv_dec     <- pct_to_dec(input$cv)
      cv_wr_dec  <- if (atype %in% c("abel", "rsabe", "ntid"))
                      pct_to_dec(input$cv_wr) else cv_dec

      # Final design guard — belt and suspenders even though UI restricts
      allowed <- names(valid_designs[[atype]])
      if (!design %in% allowed) {
        showNotification(
          paste0("The selected design is not valid for this study type. ",
                 "Please choose: ", paste(valid_designs[[atype]], collapse = ", ")),
          type = "error", duration = 8)
        return()
      }

      withProgress(message = "Calculating\u2026", value = 0.5, {
        result <- tryCatch({
          if (input$calc_mode == "sample_size") {
            tp <- pct_to_dec(input$target_power)
            switch(atype,
              "abe"   = sampleN.TOST(alpha = alpha, targetpower = tp,
                                     theta0 = theta0_dec,
                                     theta1 = theta1, theta2 = theta2,
                                     CV = cv_dec, design = design,
                                     method = "exact", print = FALSE),
              "abel"  = sampleN.scABEL(alpha = alpha, targetpower = tp,
                                       theta0 = theta0_dec, CV = cv_wr_dec,
                                       design = design, print = FALSE,
                                       nsims = 1e5),
              "rsabe" = sampleN.RSABE(alpha = alpha, targetpower = tp,
                                      theta0 = theta0_dec, CV = cv_wr_dec,
                                      design = design, print = FALSE,
                                      nsims = 1e5),
              "ntid"  = {
                if (is.null(ntid_sampleN)) stop("NTID sample size function not found in PowerTOST")
                ntid_sampleN(alpha = alpha, targetpower = tp,
                             theta0 = theta0_dec, CV = cv_wr_dec,
                             design = design, print = FALSE,
                             nsims = 1e5)
              }
            )
          } else {
            # Power mode
            n   <- input$n_subjects
            pwr <- compute_power(n, atype, alpha, theta0_dec, theta1, theta2,
                                 cv_dec, cv_wr_dec, design)
            if (is.na(pwr)) {
              showNotification(
                "Power could not be computed with these settings. Check your inputs.",
                type = "warning", duration = 8)
              return(NULL)
            }
            data.frame(Design = design, n = n, power = pwr,
                       CV = input$cv, stringsAsFactors = FALSE)
          }
        }, error = function(e) {
          showNotification(paste("Calculation error:", conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        })

        calc_result(result)
      })
    })

    # ---- Result cards -------------------------------------------------------
    output$result_cards <- renderUI({
      req(calc_result())
      r <- calc_result()

      if (input$calc_mode == "sample_size") {
        n_val   <- r[["Sample size"]]
        pwr_val <- r[["Achieved power"]]
        if (is.null(n_val) || is.null(pwr_val)) return(NULL)
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(title = "Total Subjects Needed",
                    value = tags$strong(n_val),
                    theme = "success", showcase = icon("users")),
          value_box(title = "Achieved Power",
                    value = paste0(round(pwr_val * 100, 1), "%"),
                    theme = "info", showcase = icon("bullseye")),
          value_box(title = "Study Design",
                    value = valid_designs[[input$analysis_type %||% "abe"]][input$design %||% "2x2"],
                    theme = "secondary", showcase = icon("sitemap"))
        )
      } else {
        pwr_val <- r$power
        if (is.null(pwr_val)) return(NULL)
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(title = "Power (Chance of Success)",
                    value = paste0(round(pwr_val * 100, 1), "%"),
                    theme = if (pwr_val >= 0.80) "success" else "warning",
                    showcase = icon("bullseye")),
          value_box(title = "Sample Size",
                    value = input$n_subjects,
                    theme = "info", showcase = icon("users")),
          value_box(title = "Study Design",
                    value = valid_designs[[input$analysis_type %||% "abe"]][input$design %||% "2x2"],
                    theme = "secondary", showcase = icon("sitemap"))
        )
      }
    })

    # ---- Power curve --------------------------------------------------------
    output$power_curve <- renderPlotly({
      req(calc_result())

      atype      <- input$analysis_type %||% "abe"
      design     <- input$design        %||% "2x2"
      alpha      <- input$alpha
      theta0_dec <- pct_to_dec(input$theta0)
      lims       <- resolve_limits(atype)
      theta1     <- lims$theta1
      theta2     <- lims$theta2
      cv_dec     <- pct_to_dec(input$cv)
      cv_wr_dec  <- if (atype %in% c("abel", "rsabe", "ntid"))
                      pct_to_dec(input$cv_wr) else cv_dec

      n_range <- seq(4, 200, by = 2)
      powers  <- sapply(n_range, function(n) {
        compute_power(n, atype, alpha, theta0_dec, theta1, theta2,
                      cv_dec, cv_wr_dec, design)
      })

      df <- data.frame(N = n_range, Power = powers)
      df <- df[!is.na(df$Power), ]
      if (nrow(df) == 0) {
        return(plotly_empty() %>%
          layout(title = list(
            text = "Power curve not available for these settings",
            font = list(size = 12))))
      }

      target <- if (input$calc_mode == "sample_size")
        pct_to_dec(input$target_power) else 0.80

      p <- ggplot(df, aes(x = N, y = Power)) +
        geom_line(color = "#2C3E50", linewidth = 1) +
        geom_hline(yintercept = target, linetype = "dashed",
                   color = "#E74C3C") +
        scale_y_continuous(limits = c(0, 1),
                           labels = scales::percent_format()) +
        labs(x = "Total Number of Subjects",
             y = "Power (Probability of Success)") +
        theme_minimal(base_size = 13)

      if (input$calc_mode == "sample_size") {
        r     <- calc_result()
        n_res <- r[["Sample size"]]
        if (!is.null(n_res) && !is.na(n_res))
          p <- p + geom_vline(xintercept = n_res, linetype = "dotted",
                              color = "#3498DB")
      } else {
        p <- p + geom_vline(xintercept = input$n_subjects,
                            linetype = "dotted", color = "#3498DB")
      }

      ggplotly(p)
    })

    # ---- Sensitivity plot ---------------------------------------------------
    output$sensitivity_plot <- renderPlotly({
      req(calc_result())

      atype      <- input$analysis_type %||% "abe"
      design     <- input$design        %||% "2x2"
      alpha      <- input$alpha
      theta0_dec <- pct_to_dec(input$theta0)
      lims       <- resolve_limits(atype)
      theta1     <- lims$theta1
      theta2     <- lims$theta2

      target_pwr <- if (input$calc_mode == "sample_size")
        pct_to_dec(input$target_power) else 0.80

      # CV range: 10% to 60% in steps, entered internally as decimals
      cv_seq <- seq(0.10, 0.60, length.out = 13)

      results <- lapply(cv_seq, function(cv_val) {
        tryCatch({
          r <- switch(atype,
            "abe"   = sampleN.TOST(alpha = alpha, targetpower = target_pwr,
                                   theta0 = theta0_dec,
                                   theta1 = theta1, theta2 = theta2,
                                   CV = cv_val, design = design,
                                   method = "exact", print = FALSE),
            "abel"  = sampleN.scABEL(alpha = alpha, targetpower = target_pwr,
                                     theta0 = theta0_dec, CV = cv_val,
                                     design = design, print = FALSE,
                                     nsims = 1e4),
            "rsabe" = sampleN.RSABE(alpha = alpha, targetpower = target_pwr,
                                    theta0 = theta0_dec, CV = cv_val,
                                    design = design, print = FALSE,
                                    nsims = 1e4),
            "ntid"  = {
              if (is.null(ntid_sampleN)) return(NULL)
              ntid_sampleN(alpha = alpha, targetpower = target_pwr,
                           theta0 = theta0_dec, CV = cv_val,
                           design = design, print = FALSE,
                           nsims = 1e4)
            },
            NULL
          )
          if (!is.null(r)) data.frame(CV = cv_val * 100, N = r[["Sample size"]])
          else NULL
        }, error = function(e) NULL)
      })

      df <- do.call(rbind, Filter(Negate(is.null), results))
      if (is.null(df) || nrow(df) == 0) return(plotly_empty())

      p <- ggplot(df, aes(x = CV, y = N)) +
        geom_line(color = "#2C3E50", linewidth = 1) +
        geom_point(color = "#E74C3C", size = 3) +
        labs(x = "Within-Subject Variability (CV %)",
             y = "Required Number of Subjects") +
        theme_minimal(base_size = 13)

      ggplotly(p)
    })

    # ---- Text summary -------------------------------------------------------
    output$result_text <- renderPrint({
      req(calc_result())
      cat("NCA Assistant \u2014 Power & Sample Size Calculation\n")
      cat(paste(rep("=", 50), collapse = ""), "\n\n")
      print(calc_result())
      cat("\nSettings used:\n")
      cat("  Study type:  ", input$analysis_type, "\n")
      cat("  Design:      ", input$design, "\n")
      cat("  CV:          ", input$cv, "%\n")
      cat("  T/R ratio:   ", input$theta0, "%\n")
      cat("  Limits:      [", input$theta1, ",", input$theta2, "]\n")
      cat("  Alpha:       ", input$alpha, "\n")
    })

  })
}


# ---------------------------------------------------------------------------
# Null-coalescing helper (local)
# ---------------------------------------------------------------------------
`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) > 0 && !is.na(x[1])) x else y
}
