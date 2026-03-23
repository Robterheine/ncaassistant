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
        card_header(class = "bg-warning text-dark",
                    icon("keyboard"), " Enter Concentration-Time Data"),
        card_body(
          tags$p(class = "text-muted small",
                 "Type your time points and matching concentrations below, ",
                 "one value per line. Both columns must have the same number of lines."),
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
                                  icon = icon("check")))
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
            col_widths = c(3, 2, 2, 2, 2, 1),
            selectInput(ns("admin_route"), tagList("Route", help_admin_route),
                        choices = c("Oral / IM / SC" = "extravascular",
                                    "IV Bolus" = "iv_bolus",
                                    "IV Infusion" = "iv_infusion")),
            numericInput(ns("dose"), "Dose", value = 100, min = 0),
            textInput(ns("dose_unit"), "Dose unit", value = "mg"),
            textInput(ns("time_unit"), "Time unit", value = "h"),
            textInput(ns("conc_unit"), "Conc unit", value = "ng/mL"),
            tags$div(style = "padding-top: 1.7rem;",
                     selectInput(ns("trap_method"), NULL,
                                 choices = c("Log-down" = "log", "Linear" = "linear")))
          ),
          layout_columns(
            col_widths = c(4, 8),
            checkboxInput(ns("is_ss"),
                          tagList("Steady-state analysis", help_steady_state),
                          FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("is_ss")),
              tags$p(class = "text-muted small",
                     "The pre-dose concentration is not zero because the drug has accumulated. ",
                     "The app will calculate AUC within the dosing interval (AUC\u03C4) ",
                     "instead of AUC to infinity.")
            )
          )
        )
      ),
      
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
            card_body(uiOutput(ns("nca_card")))
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
      downloadButton(ns("dl_csv"), "Download results (CSV)",
                     class = "btn-outline-primary btn-sm mt-2")
    )
  )
}

path_single_nca_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    local <- reactiveValues(time = NULL, conc = NULL,
                            label = "Manual Entry", manual_ready = FALSE,
                            lz_override = NULL)  # manual lambda_z from recalculate
    
    # Reset lambda_z override when profile changes or new NCA runs
    observeEvent(input$sel_profile, { local$lz_override <- NULL }, ignoreInit = TRUE)
    observeEvent(input$btn_use_manual, { local$lz_override <- NULL }, ignoreInit = TRUE)
    
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
    manual_parsed <- reactive({
      t_lines <- trimws(unlist(strsplit(input$manual_time %||% "", "\n")))
      c_lines <- trimws(unlist(strsplit(input$manual_conc %||% "", "\n")))
      t_lines <- t_lines[t_lines != ""]; c_lines <- c_lines[c_lines != ""]
      t_vals <- suppressWarnings(as.numeric(t_lines))
      c_vals <- suppressWarnings(as.numeric(c_lines))
      list(time = t_vals, conc = c_vals,
           nt = length(t_vals), nc = length(c_vals),
           ok = length(t_vals) == length(c_vals) && length(t_vals) >= 3 &&
                sum(is.na(t_vals)) == 0 && sum(is.na(c_vals)) == 0)
    })
    
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
    
    # Data gate (uploaded mode)
    output$data_gate <- renderUI({
      if (!shared$data_ready)
        card(class = "border-warning",
             card_body(class = "text-center py-4",
                       icon("triangle-exclamation", class = "fa-2x text-warning mb-2"),
                       tags$h5("No data loaded"),
                       tags$p("Upload data first, or switch to manual entry."),
                       actionButton(ns("goto_upload"), "Upload Data", class = "btn-warning",
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
      if (!is.null(cm$treatment))
        paste(unique(d[, c(cm$subject, cm$treatment)])[[1]], "|",
              unique(d[, c(cm$subject, cm$treatment)])[[2]])
      else sort(unique(d[[cm$subject]]))
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
        if (grepl(" \\| ", sel)) {
          parts <- strsplit(sel, " \\| ")[[1]]
          sub_d <- d[d[[cm$subject]] == trimws(parts[1]) &
                       d[[cm$treatment]] == trimws(parts[2]), ]
        } else sub_d <- d[d[[cm$subject]] == sel, ]
        sub_d <- sub_d[order(sub_d[[cm$time]]), ]
        list(time = sub_d[[cm$time]], conc = sub_d[[cm$conc]], label = sel)
      }
    })
    
    # Profile plot
    output$profile_plot <- renderPlotly({
      d <- tc(); req(length(d$time) > 0)
      df <- data.frame(Time = d$time, Conc = d$conc)
      p <- ggplot(df, aes(Time, Conc)) +
        geom_line(color = "#2C3E50", linewidth = 0.8) +
        geom_point(color = "#E74C3C", size = 3) +
        labs(x = paste0("Time (", input$time_unit, ")"),
             y = paste0("Concentration (", input$conc_unit, ")"),
             title = d$label) + theme_minimal(base_size = 12)
      if (input$y_scale == "log") p <- p + scale_y_log10()
      ggplotly(p, dynamicTicks = TRUE)
    })
    
    # NCA
    nca_res <- reactiveVal(NULL)
    observeEvent(input$run_nca, {
      local$lz_override <- NULL  # reset manual override on fresh NCA
      d <- tc(); req(length(d$time) >= 2)
      adm <- switch(input$admin_route, "extravascular"="Extravascular",
                     "iv_bolus"="Bolus", "iv_infusion"="Infusion")
      down <- switch(input$trap_method, "linear"="Linear", "log"="Log")
      r <- tryCatch(NonCompart::sNCA(d$time, d$conc, dose = input$dose,
                                      adm = adm, down = down,
                                      doseUnit = input$dose_unit,
                                      timeUnit = input$time_unit,
                                      concUnit = input$conc_unit,
                                      SS = isTRUE(input$is_ss),
                                      R2ADJ = 0.7),
                     error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL })
      
      # For steady-state: derive tau-based parameters
      if (!is.null(r) && isTRUE(input$is_ss)) {
        tau   <- max(d$time, na.rm = TRUE) - min(d$time, na.rm = TRUE)
        auclst <- as.numeric(r["AUCLST"])
        cmax   <- as.numeric(r["CMAX"])
        cmin   <- min(d$conc[!is.na(d$conc)], na.rm = TRUE)
        cavg   <- if (tau > 0 && !is.na(auclst)) auclst / tau else NA
        fluct  <- if (!is.na(cavg) && cavg > 0) (cmax - cmin) / cavg * 100 else NA
        swing  <- if (!is.na(cmin) && cmin > 0) (cmax - cmin) / cmin else NA
        
        r <- c(r,
               AUCTAU  = auclst,
               TAU     = tau,
               CAVG    = if (is.finite(cavg)) cavg else NA,
               CMIN_SS = cmin,
               FLUCTP  = if (is.finite(fluct)) fluct else NA,
               SWING   = if (is.finite(swing)) swing else NA)
      }
      
      nca_res(r)
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
      
      tags$table(class = "table table-sm table-borderless",
                 style = "font-size: 0.85rem;", rows)
    })
    
    # Lambda Z
    output$lz_info <- renderUI({
      d <- tc(); req(length(d$time) >= 3)
      lz <- if (!is.null(local$lz_override)) local$lz_override
            else estimate_lambda_z(d$time, d$conc, 0.7)
      if (is.na(lz$lambda_z))
        tags$div(class="alert alert-warning py-2", tags$small(tags$strong("Not estimable. "), lz$message))
      else {
        badge <- if (!is.null(local$lz_override))
          tags$span(class = "badge bg-info ms-2", "manually adjusted") else NULL
        tags$div(class="alert alert-success py-2",
                 tags$small(paste0("Half-life: ", signif(lz$half_life,4), " h | R\u00B2: ",
                                   signif(lz$r2adj,4), " | ", lz$n_points, " points")),
                 badge)
      }
    })
    
    output$lz_plot <- renderPlotly({
      d <- tc(); req(length(d$time) >= 3)
      lz <- if (!is.null(local$lz_override)) local$lz_override
            else estimate_lambda_z(d$time, d$conc, 0.7)
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
        "Time: ", round(df$Time, 2), " h\n",
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
          lz <- estimate_lambda_z(d$time, d$conc, 0.7)
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
      
      if (length(sel_idx) < 2) {
        showNotification("Select at least 2 points for the regression.",
                         type = "warning")
        return()
      }
      
      # Get time/conc at selected indices
      t_sel <- d$time[sel_idx]
      c_sel <- d$conc[sel_idx]
      
      # Remove any non-positive concentrations
      valid <- c_sel > 0 & !is.na(c_sel)
      t_sel <- t_sel[valid]; c_sel <- c_sel[valid]
      
      if (length(t_sel) < 2) {
        showNotification("Need at least 2 points with positive concentration.",
                         type = "warning")
        return()
      }
      
      # Fit linear regression: ln(conc) = intercept - lambda_z * time
      fit <- lm(log(c_sel) ~ t_sel)
      lambda_z_new <- -coef(fit)[2]
      intercept_new <- coef(fit)[1]
      half_life_new <- log(2) / lambda_z_new
      n_pts <- length(t_sel)
      
      # R² adjusted
      ss_res <- sum(residuals(fit)^2)
      ss_tot <- sum((log(c_sel) - mean(log(c_sel)))^2)
      r2 <- 1 - ss_res / ss_tot
      r2adj <- 1 - (1 - r2) * (n_pts - 1) / (n_pts - 2)
      
      # Store override
      local$lz_override <- list(
        lambda_z  = as.numeric(lambda_z_new),
        half_life = as.numeric(half_life_new),
        intercept = as.numeric(intercept_new),
        r2adj     = as.numeric(r2adj),
        n_points  = n_pts,
        time_used = t_sel,
        message   = "User-selected points"
      )
      
      # Update NCA results with new lambda_z-dependent parameters
      r <- nca_res()
      if (!is.null(r)) {
        auclst <- as.numeric(r["AUCLST"])
        clast  <- as.numeric(r["CLST"])
        dose_val <- input$dose
        
        # Recalculate derived parameters
        r["LAMZ"]    <- lambda_z_new
        r["LAMZHL"]  <- half_life_new
        r["R2ADJ"]   <- r2adj
        r["LAMZNPT"] <- n_pts
        
        # AUC extrapolated: AUClast + Clast / lambda_z
        if (!is.na(clast) && lambda_z_new > 0) {
          auc_extrap <- clast / lambda_z_new
          aucifo <- auclst + auc_extrap
          r["AUCIFO"]  <- aucifo
          r["AUCPEO"]  <- auc_extrap / aucifo * 100
          
          # CL/F and Vz/F
          if (dose_val > 0) {
            r["CLFO"] <- dose_val / aucifo
            r["VZFO"] <- dose_val / (aucifo * lambda_z_new)
          }
          
          # MRT
          aumclst <- as.numeric(r["AUMCLST"])
          if (!is.na(aumclst)) {
            aumc_extrap <- clast * max(t_sel) / lambda_z_new + clast / lambda_z_new^2
            aumcifo <- aumclst + aumc_extrap
            r["AUMCIFO"] <- aumcifo
            r["MRTEVLST"] <- aumclst / auclst
            r["MRTEVIFO"] <- aumcifo / aucifo
          }
        }
        
        # Update steady-state derived params if applicable
        if (isTRUE(input$is_ss) && !is.null(r["AUCTAU"])) {
          tau <- as.numeric(r["TAU"])
          if (!is.na(tau) && tau > 0) {
            r["CAVG"] <- auclst / tau
          }
        }
        
        nca_res(r)
      }
      
      showNotification(
        sprintf("Recalculated: t\u00BD = %.3f h (R\u00B2 = %.4f, %d points)",
                half_life_new, r2adj, n_pts),
        type = "message", duration = 5)
    })
    
    output$dl_csv <- downloadHandler(
      filename = function() paste0("NCA_single_", Sys.Date(), ".csv"),
      content = function(file) {
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
  })
}
