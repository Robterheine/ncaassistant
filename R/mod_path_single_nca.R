# ============================================================================
# NCA Assistant — Path 3: Analyze One Subject at a Time
# ============================================================================
# Two entry modes:
#   A) Use data already uploaded in Path 2 → navigate between profiles
#   B) Enter data manually (type time and concentration values directly)

path_single_nca_ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container-fluid py-3",
    style = "max-width: 1200px; margin: 0 auto;",
    
    tags$h4(class = "fw-bold mb-1",
            icon("user", class = "text-warning me-2"),
            "Analyze One Subject at a Time"),
    tags$p(class = "text-muted mb-3",
           "Inspect a single concentration-time profile, check the data, ",
           "adjust the half-life calculation, and review all PK parameters."),
    
    # Mode selector
    card(
      card_body(
        class = "py-3",
        layout_columns(
          col_widths = c(6, 6),
          radioButtons(ns("data_mode"), "Where is your data?",
                       choices = c(
                         "Use my uploaded dataset (from step 2)" = "uploaded",
                         "Enter data manually (type values directly)" = "manual"
                       ),
                       selected = "uploaded"),
          uiOutput(ns("mode_info"))
        )
      )
    ),
    
    # --- MANUAL ENTRY MODE --------------------------------------------------
    conditionalPanel(
      condition = sprintf("input['%s'] == 'manual'", ns("data_mode")),
      card(
        card_header(class = "bg-warning text-white",
                    icon("keyboard"), " Enter Concentration-Time Data"),
        card_body(
          tags$p(class = "text-muted small",
                 "Type your time points and matching concentrations below, ",
                 "one value per line. Both columns must have the same number of lines. ",
                 "Example values are shown \u2014 replace with your own data."),
          layout_columns(
            col_widths = c(5, 5, 2),
            textAreaInput(ns("manual_time"), "Time points",
                          value = "0\n0.5\n1\n2\n4\n8\n12\n24",
                          rows = 10,
                          placeholder = "One number per line"),
            textAreaInput(ns("manual_conc"), "Concentrations",
                          value = "0\n12.5\n28.3\n22.1\n14.8\n7.2\n3.1\n0.8",
                          rows = 10,
                          placeholder = "One number per line"),
            tags$div(style = "padding-top: 1.7rem;",
                     uiOutput(ns("manual_validation")),
                     actionButton(ns("btn_use_manual"), "Use This Data",
                                  class = "btn-warning w-100 mt-2",
                                  icon = icon("check")),
                     actionButton(ns("btn_clear_manual"), "Clear",
                                  class = "btn-outline-secondary w-100 mt-1",
                                  icon = icon("eraser")))
          )
        )
      )
    ),
    
    # --- UPLOADED DATA GATE ------------------------------------------------
    conditionalPanel(
      condition = sprintf("input['%s'] == 'uploaded'", ns("data_mode")),
      uiOutput(ns("data_gate"))
    ),
    
    # --- ANALYSIS AREA (shared by both modes) ------------------------------
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("ready")),
      
      card(
        card_header("Analysis Settings"),
        card_body(
          layout_columns(
            # One width per input: a shorter vector is recycled, which squeezed
            # the concentration unit and the trapezoidal method into one or two
            # columns and pushed their carets onto the text
            col_widths = c(4, 2, 2, 2, 2,
                           4, 4, 4),
            selectInput(ns("admin_route"), tagList("Route of administration", help_admin_route),
                        choices = c("Oral / IM / SC (extravascular)" = "extravascular",
                                    "IV Bolus (injected into vein at once)" = "iv_bolus",
                                    "IV Infusion (drip over time)" = "iv_infusion")),
            numericInput(ns("dose"), "Dose", value = 100, min = 0),
            tags$div(id = ns("dose_hint_container"),
                     uiOutput(ns("dose_hint"))),
            selectInput(ns("dose_unit"), "Dose unit", choices = DOSE_UNIT_CHOICES, selected = "mg"),
            selectInput(ns("time_unit"), "Time unit", choices = TIME_UNIT_CHOICES, selected = "h"),
            selectInput(ns("conc_unit"), "Conc unit", choices = CONC_UNIT_CHOICES, selected = "ng/mL"),
            numericInput(ns("mw"), "Molecular weight (only for molar units)",
                         value = 0, min = 0, step = 1),
            selectInput(ns("trap_method"), tagList("Trapezoidal method", help_trapezoidal),
                        choices = c("Linear-up / Log-down" = "log",
                                    "Linear-up / Linear-down" = "linear"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'iv_infusion'", ns("admin_route")),
            numericInput(ns("inf_dur"),
                         "Infusion duration (same unit as Time)",
                         value = 1, min = 0)
          ),
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput(ns("is_ss"),
                          tagList("Steady-state", help_steady_state), FALSE),
            sliderInput(ns("r2adj"),
                        tagList("Min R\u00B2 for half-life", help_r2adj),
                        min = 0, max = 1, value = 0.7, step = 0.05)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("is_ss")),
            numericInput(ns("tau"), "Dosing interval \u03C4 (same unit as Time)", value = NA, min = 0),
            tags$p(class = "text-muted small",
                   "The pre-dose concentration is not zero because the drug has accumulated. ",
                   "The app calculates AUC from 0 to \u03C4 (AUC\u03C4), average concentration and ",
                   "fluctuation instead of AUC to infinity. Sample from just before the dose to \u03C4.")
          )
        )
      ),
      
      partial_auc_ui(ns("pauc")),

      # Profile navigator (uploaded mode, multiple profiles only)
      uiOutput(ns("navigator")),
      
      actionButton(ns("run_nca"), "Run PK Analysis",
                   class = "btn-success btn-lg mb-3", icon = icon("play")),
      
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Concentration-Time Profile"),
          card_body(
            radioButtons(ns("y_scale"), NULL,
                         choices = c("Linear" = "linear", "Log scale" = "log"),
                         inline = TRUE),
            plotlyOutput(ns("profile_plot"), height = "380px")
          )
        ),
        tagList(
          card(
            card_header(tagList("PK Parameters", help_what_is_nca)),
            card_body(uiOutput(ns("nca_card")), uiOutput(ns("cdisc_codes")))
          ),
          card(
            card_header(tagList("Half-Life Calculation", help_lambda_z)),
            card_body(
              uiOutput(ns("lz_info")),
              tags$p(class = "text-muted small mb-1",
                     tags$span(class = "fw-bold", style = "color: #E74C3C;",
                               "\u25CF"),
                     " = used for half-life \u00A0\u00A0\u00A0",
                     tags$span(class = "fw-bold", style = "color: #BDC3C7;",
                               "\u25CF"),
                     " = not used"),
              plotlyOutput(ns("lz_plot"), height = "260px"),
              hr(),
              tags$p(class = "text-muted small",
                     "Uncheck points that don't fall on the straight line."),
              checkboxGroupInput(ns("lz_points"), "Points for half-life:",
                                 choices = NULL),
              actionButton(ns("lz_recalc"), "Recalculate",
                           class = "btn-outline-primary btn-sm w-100",
                           icon = icon("refresh"))
            )
          )
        )
      ),
      downloadButton(ns("dl_csv"), "Download Results (CSV)",
                     class = "btn-outline-primary btn-sm mt-2"),

      # Analysis Record — consistent panel (appears once PK analysis has run)
      uiOutput(ns("record_panel"))
    )
  )
}

path_single_nca_server <- function(id, shared) {
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
    
    local <- reactiveValues(time = NULL, conc = NULL,
                            label = "Manual Entry", manual_ready = FALSE,
                            lz_override = NULL)  # manual lambda_z from recalculate
    
    # Reset lambda_z override when profile changes or new NCA runs
    # A result belongs to the profile it was computed for: clear it when the
    # profile or the data source changes, so results, CSV and Analysis Record
    # can never pair one profile's parameters with another profile's data
    observeEvent(input$sel_profile, {
      nca_res(NULL)
      local$lz_override <- NULL
    }, ignoreInit = TRUE)
    observeEvent(input$data_mode, { nca_res(NULL); local$lz_override <- NULL }, ignoreInit = TRUE)
    observeEvent(shared$pk_data, { nca_res(NULL); local$lz_override <- NULL }, ignoreInit = TRUE)
    observeEvent(input$btn_use_manual, { local$lz_override <- NULL }, ignoreInit = TRUE)
    
    # Auto-fill dose from data when profile changes
    observeEvent(input$sel_profile, {
      if (!shared$data_ready || is.null(shared$col_map$dose)) return()
      d <- shared$pk_data; cm <- shared$col_map; sel <- input$sel_profile
      sub_d <- d[profile_data_rows(d, cm, sel), ]
      dose_val <- max(sub_d[[cm$dose]], na.rm = TRUE)
      if (is.finite(dose_val)) {
        updateNumericInput(session, "dose", value = dose_val)
      }
    }, ignoreInit = TRUE)
    
    # Dose hint
    output$dose_hint <- renderUI({
      if (shared$data_ready && !is.null(shared$col_map$dose)) {
        tags$span(class = "text-muted", style = "font-size: 0.75rem;",
                  icon("circle-info", class = "me-1"),
                  "Auto-filled from your data's Dose column.")
      }
    })
    
    # Mode info
    output$mode_info <- renderUI({
      dm <- input$data_mode
      if (is.null(dm)) return(NULL)
      if (dm == "uploaded") {
        if (shared$data_ready) {
          n <- shared$study_info$design$n_subjects
          tags$div(class = "alert alert-success py-2 mb-0",
                   icon("circle-check"),
                   paste0(" Dataset loaded: ", n, " subjects."))
        } else {
          tags$div(class = "alert alert-warning py-2 mb-0",
                   icon("triangle-exclamation"),
                   " No dataset uploaded yet.")
        }
      } else {
        tags$div(class = "alert alert-info py-2 mb-0",
                 icon("keyboard"), " Type your data below.")
      }
    })
    
    # Manual validation
    manual_parsed <- reactive(parse_manual_entry(input$manual_time, input$manual_conc))
    
    output$manual_validation <- renderUI({
      p <- manual_parsed()
      if (p$nt == 0 && p$nc == 0)
        return(tags$span(class = "text-muted small", "Enter values..."))
      if (p$nt == 0)
        return(tags$span(class = "text-danger small", "No time values found"))
      if (p$nc == 0)
        return(tags$span(class = "text-danger small", "No concentration values found"))
      if (p$nt != p$nc)
        return(tags$span(class = "text-danger small",
                         paste0(p$nt, " times vs ", p$nc, " concs — must match")))
      if (p$nt < 3)
        return(tags$span(class = "text-warning small", "Need at least 3 points"))
      if (sum(is.na(p$time)) > 0)
        return(tags$span(class = "text-danger small",
                         paste0(sum(is.na(p$time)), " non-numeric time value(s)")))
      if (sum(is.na(p$conc)) > 0)
        return(tags$span(class = "text-danger small",
                         paste0(sum(is.na(p$conc)), " non-numeric conc value(s)")))
      if (p$ok)
        tags$span(class = "text-success small",
                  icon("check"), paste0(" ", p$nt, " points ready"))
      else
        tags$span(class = "text-danger small", "Data not valid — check values")
    })
    
    observeEvent(input$btn_use_manual, {
      p <- manual_parsed()
      if (!p$ok) { showNotification("Fix data issues first.", type = "error"); return() }
      local$time <- p$time; local$conc <- p$conc
      local$manual_ready <- TRUE
      showNotification(paste0(p$nt, " points loaded."), type = "message")
    })
    
    # Clear manual data
    observeEvent(input$btn_clear_manual, {
      updateTextAreaInput(session, "manual_time", value = "")
      updateTextAreaInput(session, "manual_conc", value = "")
      local$time <- NULL
      local$conc <- NULL
      local$manual_ready <- FALSE
      local$lz_override <- NULL
      nca_res(NULL)
    })
    
    # Data gate (uploaded mode)
    output$data_gate <- renderUI({
      if (!shared$data_ready)
        card(class = "border-warning",
             card_body(class = "text-center py-4",
                       icon("triangle-exclamation", class = "fa-2x text-warning mb-2"),
                       tags$h5("No data loaded yet"),
                       tags$p("Upload data first, or switch to manual entry."),
                       actionButton(ns("goto_upload"), "Go to Upload & Check Data", class = "btn-warning",
                                    onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});")))
    })
    
    # Ready flag
    output$ready <- reactive({
      dm <- input$data_mode
      if (is.null(dm) || length(dm) == 0) return(FALSE)
      if (dm == "manual") local$manual_ready
      else shared$data_ready
    })
    outputOptions(output, "ready", suspendWhenHidden = FALSE)
    
    # Profile navigator
    profiles <- reactive({
      dm <- input$data_mode
      if (is.null(dm) || dm != "uploaded" || !shared$data_ready) return(NULL)
      d <- shared$pk_data; cm <- shared$col_map
      # One entry per profile: subject | treatment | period (as mapped)
      data_profiles(d, cm)$label
    })
    
    output$navigator <- renderUI({
      profs <- profiles()
      dm <- input$data_mode
      if (is.null(profs) || is.null(dm) || dm == "manual") return(NULL)
      multi <- length(profs) > 1
      card(card_body(class = "py-2",
        layout_columns(
          col_widths = if (multi) c(2, 6, 2, 2) else c(12),
          if (multi) actionButton(ns("prev"), icon("arrow-left"), class = "btn-outline-secondary"),
          selectInput(ns("sel_profile"),
                      paste0("Profile", if (multi) paste0(" (", length(profs), " available)") else ""),
                      choices = profs),
          if (multi) actionButton(ns("next_s"), icon("arrow-right"), class = "btn-outline-secondary"),
          if (multi) tags$span(class = "text-muted small pt-3", textOutput(ns("counter"), inline = TRUE))
        )
      ))
    })
    
    output$counter <- renderText({
      profs <- profiles(); cur <- which(profs == input$sel_profile)
      if (length(cur) > 0) paste0(cur, " / ", length(profs))
    })
    
    observeEvent(input$prev, {
      p <- profiles(); i <- which(p == input$sel_profile)
      if (length(i) > 0 && i > 1) updateSelectInput(session, "sel_profile", selected = p[i-1])
    })
    observeEvent(input$next_s, {
      p <- profiles(); i <- which(p == input$sel_profile)
      if (length(i) > 0 && i < length(p)) updateSelectInput(session, "sel_profile", selected = p[i+1])
    })
    
    # Current time/conc
    tc <- reactive({
      req(input$data_mode)
      if (input$data_mode == "manual") {
        req(local$manual_ready)
        list(time = local$time, conc = local$conc, label = "Manual Entry")
      } else {
        req(shared$pk_data, shared$col_map, input$sel_profile)
        d <- shared$pk_data; cm <- shared$col_map; sel <- input$sel_profile
        sub_d <- d[profile_data_rows(d, cm, sel), ]
        # Coerce to numeric BEFORE ordering: a character time column would sort
        # lexicographically ("10" before "2"), producing a non-monotonic profile
        # that NonCompart rejects. Numeric input is unaffected.
        sub_t <- suppressWarnings(as.numeric(as.character(sub_d[[cm$time]])))
        sub_c <- suppressWarnings(as.numeric(as.character(sub_d[[cm$conc]])))
        ord <- order(sub_t)
        list(time = sub_t[ord], conc = sub_c[ord], label = sel, is_blq = sub_d[[BLQ_FLAG_COLUMN]][ord])
      }
    })
    
    # Profile plot
    output$profile_plot <- renderPlotly({
      d <- tc(); req(length(d$time) > 0)
      df <- data.frame(Time = d$time, Conc = d$conc)
      if (input$y_scale == "log") {
        df <- df[!is.na(df$Conc) & df$Conc > 0, ]
        if (nrow(df) == 0) return(plotly_empty())
      }
      tryCatch({
        p <- ggplot(df, aes(Time, Conc)) +
          geom_line(color = "#2C3E50", linewidth = 0.8) +
          geom_point(color = "#E74C3C", size = 3) +
          labs(x = paste0("Time (", input$time_unit, ")"),
               y = paste0("Concentration (", input$conc_unit, ")"),
               title = d$label) + theme_minimal(base_size = 12)
        if (input$y_scale == "log") p <- p + scale_y_log10()
        ggplotly(p, dynamicTicks = TRUE)
      }, error = function(e) plotly_empty())
    })
    
    # NCA
    nca_res <- reactiveVal(NULL)
    pauc_spec <- partial_auc_server("pauc")
    clear_result_on_change(
      reactive(list(input$admin_route, input$dose, input$inf_dur, input$is_ss, input$tau,
                    input$dose_unit, input$time_unit, input$conc_unit, input$trap_method,
                    input$r2adj, input$mw, pauc_spec())),
      has_result = function() !is.null(nca_res()), clear = function() nca_res(NULL),
      id = "single_nca_stale")
    pauc_notes <- reactiveVal(character(0))
    # NCA settings from the inputs, in the shape run_single_nca() expects
    single_settings <- function() {
      list(admin_route = input$admin_route,
           dose = suppressWarnings(as.numeric(input$dose)),
           infusion_duration = if (input$admin_route == "iv_infusion") input$inf_dur else 0,
           is_steady_state = isTRUE(input$is_ss), tau = input$tau,
           dose_unit = input$dose_unit, time_unit = input$time_unit, conc_unit = input$conc_unit,
           trap_method = input$trap_method, r2adj_threshold = input$r2adj,
           mw = if (is.null(input$mw) || is.na(input$mw)) 0 else input$mw,
           partial_aucs = pauc_spec())
    }
    observeEvent(input$run_nca, {
      local$lz_override <- NULL  # reset manual override on fresh NCA
      d <- tc(); req(length(d$time) >= 2)
      
      if (is.null(input$dose) || is.na(input$dose) || input$dose <= 0) {
        showNotification("Please enter a valid dose (greater than 0).",
                         type = "error", duration = 5)
        return()
      }
      
      # NonCompart 0.8.0 hard-stops ("Check input types!") unless time, conc, and
      # dose are numeric. Coerce defensively so a stray character value can't fail NCA.
      t_num <- suppressWarnings(as.numeric(as.character(d$time)))
      c_num <- suppressWarnings(as.numeric(as.character(d$conc)))

      # Units drive a real conversion factor for CL/F and Vz/F, and an
      # unrecognised spelling makes sNCA fail with an opaque error. Check first.
      uchk <- validate_units(input$dose_unit, input$time_unit, input$conc_unit, input$mw)
      if (!uchk$valid) {
        showNotification(uchk$message, type = "error", duration = 12)
        return(NULL)
      }
      umsg <- if (identical(input$data_mode, "uploaded")) check_units_against_data(shared$study_info$units, input$dose_unit, input$time_unit, input$conc_unit)
      if (!is.null(umsg)) {
        showNotification(umsg, type = "error", duration = 12)
        return(NULL)
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
      settings <- single_settings()
      notes <- character(0)
      r <- tryCatch(withCallingHandlers(run_single_nca(t_num, c_num, settings, is_blq = d$is_blq),
                                        warning = function(w) {
                                          if (startsWith(conditionMessage(w), "Partial AUC")) {
                                            notes <<- c(notes, conditionMessage(w))
                                            invokeRestart("muffleWarning")
                                          }
                                        }),
                     error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL })
      pauc_notes(notes)
      if (length(notes) > 0)
        showNotification("Partial AUCs: see the notes under the PK parameters.", type = "warning", duration = 8)
      shared$partial_aucs <- settings$partial_aucs
      
      if (!is.null(r) && below_r2_threshold(r["R2ADJ"], input$r2adj)) {
        showNotification(paste0("Adjusted R\u00b2 of the terminal fit (", signif(as.numeric(r["R2ADJ"]), 3),
                                ") is below ", input$r2adj, ": half-life, AUC to infinity, CL/F, Vz/F ",
                                "and MRT are not reported. Review the fit in Half-Life Review."),
                         type = "warning", duration = 12)
      }
      
      nca_res(r)
    })
    
    # Official CDISC codes for the parameters computed, with the release used
    output$cdisc_codes <- renderUI({
      r <- nca_res(); req(r)
      cdisc_codes_ui(names(r), input$admin_route, isTRUE(input$is_ss))
    })

    output$nca_card <- renderUI({
      r <- nca_res()
      if (is.null(r)) return(tags$p(class="text-muted", "Click 'Run PK Analysis'."))
      sg <- function(n) { v <- r[n]; if (is.na(v)) "\u2014" else signif(as.numeric(v), 4) }
      
      # Common rows
      rows <- tagList(
        tags$tr(tags$td("Peak concentration (Cmax):"), tags$td(tags$strong(sg("CMAX")))),
        tags$tr(tags$td("Time of peak (Tmax):"), tags$td(tags$strong(sg("TMAX")))),
        tags$tr(tags$td("AUC to last point:"), tags$td(tags$strong(sg("AUCLST"))))
      )
      
      if (isTRUE(input$is_ss)) {
        # Steady-state specific
        rows <- tagList(rows,
          tags$tr(tags$td("AUC within dosing interval (AUC\u03C4):"),
                  tags$td(tags$strong(sg("AUCTAU")))),
          tags$tr(tags$td("Dosing interval (\u03C4):"),
                  tags$td(tags$strong(sg("TAU")))),
          tags$tr(tags$td("Average concentration (Cavg = AUC\u03C4 / \u03C4):"),
                  tags$td(tags$strong(sg("CAVG")))),
          tags$tr(tags$td("Trough concentration (Cmin):"),
                  tags$td(tags$strong(sg("CMIN_SS")))),
          tags$tr(tags$td("Peak-trough fluctuation (%):"),
                  tags$td(tags$strong(sg("FLUCTP")))),
          tags$tr(tags$td("Swing ((Cmax \u2212 Cmin) / Cmin):"),
                  tags$td(tags$strong(sg("SWING"))))
        )
      } else {
        # Single-dose specific
        rows <- tagList(rows,
          tags$tr(tags$td("AUC to infinity:"), tags$td(tags$strong(sg("AUCIFO"))))
        )
      }
      
      # Always show these
      rows <- tagList(rows,
        tags$tr(tags$td("Terminal half-life:"), tags$td(tags$strong(sg("LAMZHL")))),
        tags$tr(tags$td("Clearance (CL/F):"), tags$td(tags$strong(sg("CLFO")))),
        tags$tr(tags$td("Volume of distribution (Vz/F):"), tags$td(tags$strong(sg("VZFO")))),
        tags$tr(tags$td("R\u00B2 of terminal fit:"), tags$td(tags$strong(sg("R2ADJ"))))
      )
      
      # AUCPEO flag: show % extrapolated when lambda-z was estimated.
      # Values from NonCompart are on 0-100 scale. Flag amber when > 20%.
      aucpeo_val <- suppressWarnings(as.numeric(r["AUCPEO"]))
      if (!is.null(r) && "AUCPEO" %in% names(r) && !is.na(aucpeo_val)) {
        aucpeo_display <- paste0(round(aucpeo_val, 1), "%")
        if (aucpeo_val > 20) {
          aucpeo_cell <- tags$td(
            tags$span(style = "color: #7D5A00; font-weight: bold;", aucpeo_display),
            tags$span(class = "badge ms-2",
                      style = "background: #FFF3CD; color: #7D5A00; font-size: 0.75em;",
                      "Review lambda-z")
          )
        } else {
          aucpeo_cell <- tags$td(tags$strong(aucpeo_display))
        }
        rows <- tagList(rows,
          tags$tr(tags$td("AUC % extrapolated:"), aucpeo_cell)
        )
      }
      
      for (p in partial_auc_cols(names(r)))
        rows <- tagList(rows, tags$tr(tags$td(paste0(friendly_name(p), ":")), tags$td(tags$strong(sg(p)))))

      tagList(tags$table(class = "table table-sm table-borderless",
                         style = "font-size: 0.85rem;", rows),
              partial_auc_notes_ui(pauc_notes()))
    })
    
    # Lambda Z
    output$lz_info <- renderUI({
      d <- tc(); req(length(d$time) >= 3)
      lz <- if (!is.null(local$lz_override)) local$lz_override
            else estimate_lambda_z(d$time, d$conc, input$r2adj, route = input$admin_route, is_blq = d$is_blq)
      if (is.na(lz$lambda_z))
        tags$div(class="alert alert-warning py-2", tags$small(tags$strong("Not estimable. "), lz$message))
      else {
        badge <- if (!is.null(local$lz_override))
          tags$span(class = "badge bg-info ms-2", "manually adjusted") else NULL
        tags$div(class="alert alert-success py-2",
                 tags$small(paste0("Half-life: ", signif(lz$half_life,4), " ", input$time_unit, " | R\u00B2: ",
                                   signif(lz$r2adj,4), " | ", lz$n_points, " points")),
                 badge)
      }
    })
    
    output$lz_plot <- renderPlotly({
      d <- tc(); req(length(d$time) >= 3)
      tryCatch({
      lz <- if (!is.null(local$lz_override)) local$lz_override
            else estimate_lambda_z(d$time, d$conc, input$r2adj, route = input$admin_route, is_blq = d$is_blq)
      df <- data.frame(
        Time = d$time,
        ln_Conc = ifelse(d$conc > 0, log(d$conc), NA),
        Conc = d$conc,
        used = FALSE
      )
      if (length(lz$time_used) > 0) {
        for (i in seq_along(lz$time_used)) {
          m <- which(abs(d$time - lz$time_used[i]) < 1e-10)
          if (length(m) > 0) df$used[m[1]] <- TRUE
        }
      }
      df$Status <- ifelse(df$used, "Used for half-life", "Not used")
      df <- df[!is.na(df$ln_Conc), ]
      
      df$tooltip <- paste0(
        "Time: ", round(df$Time, 2), " ", input$time_unit, "\n",
        "Conc: ", signif(df$Conc, 4), "\n",
        "ln(Conc): ", round(df$ln_Conc, 3)
      )
      
      p <- ggplot(df, aes(x = Time, y = ln_Conc, color = Status, text = tooltip)) +
        geom_point(size = 3.5, alpha = 0.85) +
        scale_color_manual(values = c("Used for half-life" = "#E74C3C",
                                      "Not used" = "#BDC3C7")) +
        theme_minimal(base_size = 11) +
        labs(x = paste0("Time (", input$time_unit, ")"),
             y = "ln(Concentration)") +
        theme(legend.position = "none",
              plot.margin = margin(5, 10, 5, 5))
      
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
    
    observe({
      d <- tc(); req(length(d$time) >= 3)
      valid <- !is.na(d$conc) & d$conc > 0
      cmax_t <- d$time[which.max(d$conc)]
      term <- valid & d$time > cmax_t
      if (any(term)) {
        ch <- paste0("t=", d$time[term], "  C=", round(d$conc[term], 3))
        names(ch) <- which(term)
        # Use override selection if available, otherwise auto-detect
        if (!is.null(local$lz_override)) {
          sel <- as.character(which(term)[d$time[term] %in% local$lz_override$time_used])
        } else {
          lz <- estimate_lambda_z(d$time, d$conc, input$r2adj, route = input$admin_route, is_blq = d$is_blq)
          sel <- if (length(lz$time_used) > 0)
            as.character(which(term)[d$time[term] %in% lz$time_used]) else NULL
        }
        updateCheckboxGroupInput(session, "lz_points",
                                 choices = setNames(names(ch), ch), selected = sel)
      }
    })
    
    # === RECALCULATE LAMBDA Z FROM USER-SELECTED POINTS ===================
    observeEvent(input$lz_recalc, {
      d <- tc(); req(length(d$time) >= 2)
      sel_idx <- as.integer(input$lz_points)
      
      # Shared computation via helper — validation, regression, R²
      lz_calc <- recalculate_lambda_z(d$time, d$conc, sel_idx)
      
      if (!is.null(lz_calc$error)) {
        showNotification(lz_calc$error, type = "error", duration = 10)
        return()
      }
      if (!is.null(lz_calc$warning)) {
        showNotification(lz_calc$warning, type = "warning", duration = 8)
      }
      
      override <- lz_calc$result
      lambda_z_new  <- override$lambda_z
      half_life_new <- override$half_life
      r2adj         <- override$r2adj
      n_pts         <- override$n_points
      t_sel         <- override$time_used
      
      # Store override (field names must match what lz_plot/lz_status consume)
      local$lz_override <- override
      
      # Recompute with NonCompart on the chosen points: the slope and every
      # dependent parameter (AUCinf, CL, V, MRT, ...) with NonCompart's units,
      # identical to the batch analysis and the reproduction script.
      if (!is.null(nca_res())) {
        t_num <- suppressWarnings(as.numeric(as.character(d$time)))
        c_num <- suppressWarnings(as.numeric(as.character(d$conc)))
        r <- tryCatch(suppressWarnings(run_single_nca(t_num, c_num, single_settings(), time_used = t_sel,
                                                      is_blq = d$is_blq)),
                      error = function(e) NULL)
        if (!is.null(r)) {
          nca_res(r)
        }
      }
      
      showNotification(
        sprintf(paste0("Recalculated: t\u00BD = %.3f ", input$time_unit, " (%s, %d points)"),
                half_life_new,
                if (!is.na(r2adj)) sprintf("R\u00B2 = %.4f", r2adj) else "R\u00B2 = N/A",
                n_pts),
        type = "message", duration = 5)
    })
    
    output$dl_csv <- downloadHandler(
      filename = function() paste0("NCA_single_", Sys.Date(), ".csv"),
      content = function(file) {
        req(nca_res())
        r <- nca_res()
        if (!is.null(r)) {
          df <- data.frame(
            Parameter = sapply(names(r), friendly_name),
            Abbreviation = names(r),
            Value = as.character(r)
          )
          write.csv(df, file, row.names = FALSE)
        }
      }
    )
    
    # Analysis Record panel — appears once a PK analysis has been run
    output$record_panel <- renderUI({
      req(nca_res())
      analysis_record_ui(
        session$ns,
        intro = paste0(
          "Self-contained package for this single profile: results, every setting, ",
          "a standalone R reproducibility script (including any manual half-life ",
          "adjustment), a SHA-256 data-integrity hash, and an HTML summary."))
    })

    # Complete Analysis Record for single subject — shared engine
    output$dl_record <- downloadHandler(
      filename = function() {
        study <- if (nchar(input$record_study) > 0)
          gsub("[^A-Za-z0-9_-]", "_", input$record_study) else "Single_NCA"
        paste0("Analysis_Record_", study, "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(nca_res())

        withProgress(message = "Generating analysis record...", value = 0.4, {
          r <- nca_res()
          d <- tc()

          settings <- single_settings()
          settings$r2adj_threshold <- input$r2adj
          settings$n_obs <- length(d$time)

          subject_label <- if (identical(input$data_mode, "uploaded") &&
                               !is.null(shared$pk_data) && !is.null(shared$col_map)) {
            input$sel_profile %||% "Subject"
          } else { "Manual Entry" }

          # File source: uploaded dataset vs. manual entry
          # Uploaded only when this analysis used the uploaded data (not manual entry)
          has_file <- identical(input$data_mode, "uploaded") &&
            !is.null(shared$study_info) && !is.null(shared$study_info$file_name)
          original_name <- if (has_file) shared$study_info$file_name else "manual_entry.csv"
          original_path <- if (has_file) shared$study_info$file_path else NULL

          # BLQ handling actually applied (from the upload step), else none
          blq_rule <- if (has_file && !is.null(shared$study_info$blq_rule))
            shared$study_info$blq_rule else "none"
          lloq <- if (has_file && !is.null(shared$study_info$lloq))
            shared$study_info$lloq else 0

          # Capture a manual lambda-z override as an audit entry (compare against
          # the automatic fit at the current R-squared threshold).
          lz_override <- NULL
          if (!is.null(local$lz_override) && length(d$time) >= 3) {
            auto <- tryCatch(estimate_lambda_z(d$time, d$conc, input$r2adj, route = input$admin_route, is_blq = d$is_blq),
                             error = function(e) NULL)
            lz_override <- list(
              profile            = subject_label,
              original_lambda_z  = if (!is.null(auto)) auto$lambda_z else NA,
              adjusted_lambda_z  = local$lz_override$lambda_z,
              original_r2adj     = if (!is.null(auto)) auto$r2adj else NA,
              adjusted_r2adj     = local$lz_override$r2adj,
              points_used        = local$lz_override$n_points,
              time_used          = local$lz_override$time_used
            )
          }

          setProgress(0.7, message = "Building record...")

          rec_out <- create_single_analysis_record(
            output_path        = file,
            result             = r,
            settings           = settings,
            time_vec           = d$time,
            conc_vec           = d$conc,
            subject_label      = subject_label,
            original_file_path = original_path,
            original_file_name = original_name,
            blq_rule           = blq_rule,
            lloq               = lloq,
            analyst            = if (nchar(input$record_analyst) > 0) input$record_analyst else "Analyst",
            study_name         = if (nchar(input$record_study) > 0) input$record_study else "Untitled Study",
            lz_override        = lz_override,
            col_map            = if (has_file) shared$col_map else NULL,
            read_args          = if (has_file) shared$study_info$read_args else NULL,
            adnca              = if (has_file && identical(shared$study_info$door, "adnca"))
                                   shared$study_info$adnca else NULL
          )
          notify_reproduction(rec_out)
        })
      }
    )
  })
}
