# ============================================================================
# NCA Assistant — Path 3: Visualize Data  (v1.2)
# ============================================================================
# Layout  : settings sidebar (left) + stacked plots (right, no tabs).
# Plot type radio ("Individual Profiles" / "Summary Plot" / "Both") drives
# which plot(s) are visible in the main panel.
# Export controls live in the sidebar so the user never needs to hunt.
#
# Shared reactive dependencies (read-only):
#   shared$data_ready, shared$pk_data, shared$col_map, shared$study_info
#
# Writes shared$viz_settings for optional inclusion in Complete Analysis Record.
# ============================================================================

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

path_viz_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "container-fluid py-3",
    style = "max-width: 1300px; margin: 0 auto;",

    tags$h4(
      class = "fw-bold mb-1",
      icon("chart-line", class = "me-2", style = "color: #8E44AD;"),
      "Visualize Data"
    ),
    tags$p(
      class = "text-muted mb-3",
      "Create publication-ready concentration-time plots from your uploaded data. ",
      "Configure the plot in the sidebar. Export controls are at the bottom of the sidebar."
    ),

    # Data gate (shown when no data loaded)
    uiOutput(ns("data_gate")),

    # Main layout (shown only when data is ready)
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("data_ok")),

      layout_sidebar(
        fillable = FALSE,

        # ==== SIDEBAR ========================================================
        sidebar = sidebar(
          title = "Plot Settings",
          width = 310,
          open  = TRUE,

          # --- Plot type ---
          card(
            card_header(class = "bg-primary text-white", "Plot Type"),
            card_body(
              radioButtons(
                ns("plot_type"),
                label    = NULL,
                choices  = c(
                  "Individual Profiles (spaghetti plot)"      = "spaghetti",
                  "Summary Plot (geometric mean \u00b1 gCV%)" = "summary",
                  "Both plots"                                = "both"
                ),
                selected = "spaghetti"
              )
            )
          ),

          # --- Data options ---
          card(
            card_header(class = "bg-primary text-white", "Data Options"),
            card_body(

              # Y scale — prominent, first control
              tags$div(
                class = "mb-2",
                tags$label(class = "form-label fw-semibold", "Y-axis scale"),
                radioButtons(
                  ns("y_scale"),
                  label    = NULL,
                  choices  = c("Linear" = "linear",
                               "Log\u2081\u2080 (semi-log)" = "log"),
                  selected = "linear",
                  inline   = TRUE
                )
              ),

              # Colour-by (spaghetti / both)
              conditionalPanel(
                condition = sprintf(
                  "input['%s'] == 'spaghetti' || input['%s'] == 'both'",
                  ns("plot_type"), ns("plot_type")),
                uiOutput(ns("color_by_ui")),
                checkboxInput(ns("show_points_spaghetti"),
                              "Show data points on lines", value = TRUE)
              ),

              # Summary options (summary / both)
              conditionalPanel(
                condition = sprintf(
                  "input['%s'] == 'summary' || input['%s'] == 'both'",
                  ns("plot_type"), ns("plot_type")),
                selectizeInput(
                  ns("summary_stat"),
                  label = tags$span(
                    "Summary statistic",
                    tags$span(
                      class = "ms-1",
                      style = "font-size: 0.75rem; font-weight: normal; color: #6c757d;",
                      "(geometric mean recommended)"
                    )
                  ),
                  choices = c(
                    "Geometric mean \u00b1 Geometric CV%" = "geomean",
                    "Arithmetic mean \u00b1 SD (not recommended for Cmax/AUC)" = "arithmean"
                  ),
                  selected = "geomean",
                  # Attach dropdown to <body> so card overflow never clips it
                  options = list(dropdownParent = "body")
                ),
                checkboxInput(ns("show_points_summary"),
                              "Overlay individual observations", value = FALSE)
              ),

              # Dose normalisation (only when Dose column is mapped)
              uiOutput(ns("dose_norm_ui"))
            )
          ),

          # --- Appearance ---
          card(
            card_header(class = "bg-primary text-white", "Appearance"),
            card_body(
              textInput(ns("x_label"),    "X-axis label", value = "Time (h)"),
              textInput(ns("y_label"),    "Y-axis label", value = "Concentration"),
              textInput(ns("plot_title"), "Plot title",   value = ""),

              selectInput(
                ns("color_palette"), "Colour palette",
                choices = c(
                  "Default (app palette)"       = "default",
                  "Colorblind-safe (Okabe-Ito)" = "okabe",
                  "Grayscale"                   = "gray"
                ),
                selected = "default"
              ),

              selectInput(
                ns("plot_theme"), "Theme",
                choices = c(
                  "Clean (white background)" = "bw",
                  "Minimal"                  = "minimal",
                  "Classic"                  = "classic"
                ),
                selected = "bw"
              ),

              numericInput(ns("base_size"), "Base font size (pt)",
                           value = 11, min = 8, max = 18, step = 1)
            )
          ),

          # --- Export (in sidebar — no tab confusion) -----------------------
          card(
            card_header(
              class = "bg-primary text-white",
              icon("download", class = "me-1"), "Export Figure"
            ),
            card_body(

              # Which plot to export — shown only when "Both" is selected
              uiOutput(ns("export_which_ui")),

              # Clear label telling the user exactly what will be exported
              uiOutput(ns("export_which_note")),

              radioButtons(
                ns("export_format"), "Format",
                choices  = c("PNG" = "png", "PDF" = "pdf",
                             "SVG (vector)" = "svg"),
                selected = "png",
                inline   = TRUE
              ),

              layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("export_width"),  "Width (in)",
                             value = 7, min = 2, max = 20),
                numericInput(ns("export_height"), "Height (in)",
                             value = 5, min = 2, max = 20)
              ),

              selectInput(
                ns("export_dpi"), "Resolution (DPI)",
                choices  = c(
                  "150 \u2014 screen"           = "150",
                  "300 \u2014 print (default)"  = "300",
                  "600 \u2014 journal high-res" = "600"
                ),
                selected = "300"
              ),

              uiOutput(ns("download_btn_ui"))
            )
          )
        ),

        # ==== MAIN PANEL — stacked plots, no tabs ============================
        tags$div(
          class = "pt-2",

          # Alerts
          uiOutput(ns("blq_note_log")),
          uiOutput(ns("blq_note_geomean")),
          uiOutput(ns("arithmean_warning")),

          # Auto-generated figure legend for manuscripts
          uiOutput(ns("figure_legend_ui")),

          # Individual Profiles
          conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'spaghetti' || input['%s'] == 'both'",
              ns("plot_type"), ns("plot_type")),
            tags$div(
              class = "mb-1",
              tags$p(class = "fw-semibold text-muted small mb-1 mt-2",
                     "Individual Profiles"),
              plotlyOutput(ns("spaghetti_plotly"), height = "440px"),
              tags$div(
                class = "text-muted small mt-1 mb-4",
                icon("circle-info", class = "me-1"),
                "Interactive: hover for values, scroll to zoom, drag to pan."
              )
            )
          ),

          # Summary plot
          conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'summary' || input['%s'] == 'both'",
              ns("plot_type"), ns("plot_type")),
            tags$div(
              tags$p(class = "fw-semibold text-muted small mb-1 mt-2",
                     "Summary Plot"),
              plotlyOutput(ns("summary_plotly"), height = "440px"),
              tags$div(
                class = "text-muted small mt-1",
                icon("circle-info", class = "me-1"),
                "Error bars: geometric mean \u00d7\u00f7 geometric SD ",
                "(equivalent to \u00b1 1 geometric SD on the log scale). ",
                "Observations with concentration \u2264 0 excluded from geometric mean."
              )
            )
          )
        )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

path_viz_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Colour palettes ---------------------------------------------------
    # Always returns exactly n colours; interpolates if n > palette length.
    palette_colours <- function(n, palette_name) {
      base_cols <- switch(palette_name,
        "okabe" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                    "#0072B2", "#D55E00", "#CC79A7", "#000000"),
        "gray"  = return(gray.colors(max(n, 2), start = 0.15, end = 0.70)),
        # Default: app palette
        c("#2C3E50", "#E74C3C", "#3498DB", "#18BC9C",
          "#F39C12", "#8E44AD", "#1ABC9C", "#E67E22")
      )
      if (n <= length(base_cols)) {
        base_cols[seq_len(n)]
      } else {
        # Interpolate smoothly when more colours are needed than the palette has
        colorRampPalette(base_cols)(n)
      }
    }

    # ---- Data gate ---------------------------------------------------------
    output$data_ok <- reactive({ isTRUE(shared$data_ready) })
    outputOptions(output, "data_ok", suspendWhenHidden = FALSE)

    output$data_gate <- renderUI({
      if (isTRUE(shared$data_ready)) return(NULL)
      tags$div(
        class = "card border-warning",
        tags$div(
          class = "card-body text-center py-5",
          icon("file-import", class = "fa-3x text-warning mb-3"),
          tags$h5("No data loaded yet"),
          tags$p(class = "text-muted",
                 "Upload your concentration-time data first, then return here."),
          actionButton(
            ns("go_upload"), "Go to Upload & Check Data",
            class   = "btn-warning",
            onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});"
          )
        )
      )
    })

    # ---- Colour-by choices — only offer mapped columns ---------------------
    color_by_choices <- reactive({
      req(shared$data_ready, shared$col_map)
      cm <- shared$col_map
      d  <- shared$pk_data
      ch <- c("Subject ID" = "subject")
      if (!is.null(cm$treatment) && cm$treatment %in% names(d))
        ch <- c(ch, "Treatment" = "treatment")
      if (!is.null(cm$period)    && cm$period    %in% names(d))
        ch <- c(ch, "Period"    = "period")
      if (!is.null(cm$sequence)  && cm$sequence  %in% names(d))
        ch <- c(ch, "Sequence"  = "sequence")
      c(ch, "None (all same colour)" = "none")
    })

    output$color_by_ui <- renderUI({
      ch  <- color_by_choices()
      def <- if ("treatment" %in% ch) "treatment" else "subject"
      selectInput(ns("color_by"), "Colour lines by",
                  choices = ch, selected = def)
    })

    # ---- Dose normalisation UI (conditional on dose column) ----------------
    output$dose_norm_ui <- renderUI({
      req(shared$data_ready, shared$col_map)
      cm <- shared$col_map
      if (!is.null(cm$dose) && cm$dose %in% names(shared$pk_data))
        checkboxInput(ns("dose_norm"),
                      "Normalize to dose (show C/Dose)", value = FALSE)
    })

    # ---- Export: which-plot selector (only visible when "both") ------------
    output$export_which_ui <- renderUI({
      req(shared$data_ready)
      if ((input$plot_type %||% "spaghetti") != "both") return(NULL)
      radioButtons(
        ns("export_which"), "Which plot to export?",
        choices  = c("Individual Profiles" = "spaghetti",
                     "Summary Plot"        = "summary"),
        selected = "spaghetti",
        inline   = TRUE
      )
    })

    # Explain to user exactly what will download
    output$export_which_note <- renderUI({
      req(shared$data_ready)
      pt <- input$plot_type %||% "spaghetti"
      lbl <- if (pt == "both") {
        if ((input$export_which %||% "spaghetti") == "summary")
          "Summary Plot" else "Individual Profiles"
      } else if (pt == "summary") "Summary Plot" else "Individual Profiles"

      tags$div(
        class = "alert alert-light py-2 small mb-2",
        style = "border: 1px solid #dee2e6;",
        icon("download", class = "me-1 text-success"),
        tags$strong("Will export: "), lbl,
        tags$span(class = "text-muted ms-1",
                  "\u2014 one static file (PNG / PDF / SVG)")
      )
    })

    # ---- Theme helper ------------------------------------------------------
    apply_theme <- function(p, theme_name, base_size) {
      switch(theme_name,
        "minimal" = p + theme_minimal(base_size = base_size),
        "classic" = p + theme_classic(base_size = base_size),
        p + theme_bw(base_size = base_size)
      )
    }

    # ---- Processed / dose-normalised data reactive -------------------------
    plot_data <- reactive({
      req(shared$data_ready, shared$pk_data, shared$col_map)
      d  <- shared$pk_data
      cm <- shared$col_map

      do_norm <- isTRUE(input$dose_norm) &&
        !is.null(cm$dose) && cm$dose %in% names(d)

      if (do_norm) {
        dose_vals <- suppressWarnings(as.numeric(d[[cm$dose]]))
        safe_dose <- ifelse(is.na(dose_vals) | dose_vals <= 0, NA_real_, dose_vals)
        conc_vals <- suppressWarnings(as.numeric(d[[cm$conc]])) / safe_dose
      } else {
        conc_vals <- suppressWarnings(as.numeric(d[[cm$conc]]))
      }

      out <- data.frame(
        .subj = d[[cm$subject]],
        .time = suppressWarnings(as.numeric(d[[cm$time]])),
        .conc = conc_vals,
        stringsAsFactors = FALSE
      )

      if (!is.null(cm$treatment) && cm$treatment %in% names(d))
        out$.treatment <- d[[cm$treatment]]
      if (!is.null(cm$period) && cm$period %in% names(d))
        out$.period    <- d[[cm$period]]
      if (!is.null(cm$sequence) && cm$sequence %in% names(d))
        out$.sequence  <- d[[cm$sequence]]

      out <- out[!is.na(out$.time), ]
      out[order(out$.subj, out$.time), ]
    })

    # ---- BLQ counts --------------------------------------------------------
    blq_n_raw <- reactive({
      req(shared$data_ready, shared$pk_data, shared$col_map)
      vals <- suppressWarnings(
        as.numeric(shared$pk_data[[shared$col_map$conc]]))
      sum(!is.na(vals) & vals <= 0)
    })

    blq_n_summary <- reactive({
      req(plot_data())
      d <- plot_data()
      sum(!is.na(d$.conc) & d$.conc <= 0)
    })

    # Alerts
    output$blq_note_log <- renderUI({
      req(shared$data_ready)
      if (!isTRUE(input$y_scale == "log")) return(NULL)
      n <- blq_n_raw()
      if (n == 0) return(NULL)
      tags$div(class = "alert alert-info py-2 small mb-2",
               icon("triangle-exclamation", class = "me-1"),
               n, " observation(s) with concentration \u2264 0 omitted from log scale.")
    })

    output$blq_note_geomean <- renderUI({
      req(shared$data_ready)
      if (!input$plot_type %in% c("summary", "both")) return(NULL)
      n <- blq_n_summary()
      if (n == 0) return(NULL)
      tags$div(class = "alert alert-info py-2 small mb-2",
               icon("triangle-exclamation", class = "me-1"),
               n, " observation(s) with concentration \u2264 0 excluded from geometric mean.")
    })

    output$arithmean_warning <- renderUI({
      req(shared$data_ready)
      if (!input$plot_type %in% c("summary", "both")) return(NULL)
      if (!isTRUE(input$summary_stat == "arithmean")) return(NULL)
      tags$div(class = "alert alert-warning py-2 small mb-2",
               icon("triangle-exclamation", class = "me-1"),
               tags$strong("Note: "),
               "Arithmetic mean \u00b1 SD is not recommended for log-normally ",
               "distributed PK parameters (Cmax, AUC). ",
               "Geometric mean is the appropriate default.")
    })

    # ---- Auto-generated figure legend for manuscripts ----------------------
    output$figure_legend_ui <- renderUI({
      req(shared$data_ready, shared$pk_data, shared$col_map)
      d     <- shared$pk_data
      cm    <- shared$col_map
      pt    <- input$plot_type    %||% "spaghetti"
      stat  <- input$summary_stat %||% "geomean"
      sc    <- input$y_scale      %||% "linear"
      cb    <- input$color_by     %||% "subject"
      norm  <- isTRUE(input$dose_norm) &&
                 !is.null(cm$dose) && cm$dose %in% names(d)

      n_subj    <- length(unique(d[[cm$subject]]))
      n_obs     <- nrow(d)
      blq_excl  <- blq_n_summary()

      has_trt  <- !is.null(cm$treatment) && cm$treatment %in% names(d)
      trt_lvls <- if (has_trt)
        sort(unique(as.character(d[[cm$treatment]])))
      else character(0)

      y_desc     <- if (norm) "dose-normalised concentration (C/Dose)" else "concentration"
      scale_desc <- if (sc == "log") "semi-logarithmic" else "linear"
      blq_sent   <- if (blq_excl > 0)
        paste0(" ", blq_excl,
               " observation(s) with concentration \u2264 0 were excluded from the geometric mean.")
      else ""
      trt_sent <- if (length(trt_lvls) >= 2)
        paste0(" Treatment groups (",
               paste(trt_lvls, collapse = " vs "),
               ") are shown as distinct coloured series.")
      else ""

      stat_desc <- if (stat == "geomean")
        paste0("geometric mean \u00b1 geometric CV% (n\u2009=\u2009", n_subj, ")")
      else
        paste0("arithmetic mean \u00b1 SD (n\u2009=\u2009", n_subj, ")")

      colour_desc <- switch(cb,
        "subject"   = "colour-coded by subject",
        "treatment" = "colour-coded by treatment group",
        "period"    = "colour-coded by period",
        "sequence"  = "colour-coded by sequence",
        "plotted in a single colour"
      )

      spag_legend <- paste0(
        "Figure. Individual ", y_desc, "-time profiles for ",
        n_subj, " subjects (", n_obs, " observations total), ",
        colour_desc, ". The Y-axis uses a ", scale_desc, " scale."
      )

      summ_legend <- paste0(
        "Figure. ", tools::toTitleCase(y_desc), "-time profile showing ",
        stat_desc, ". Error bars represent the geometric standard deviation ",
        "(geometric mean \u00d7\u00f7 geometric SD on the log scale).",
        trt_sent, blq_sent,
        " The Y-axis uses a ", scale_desc, " scale."
      )

      legend_txt <- switch(pt,
        "spaghetti" = spag_legend,
        "summary"   = summ_legend,
        "both"      = paste0(spag_legend, "\n\n", summ_legend)
      )

      card(
        card_header(
          class = "py-2",
          style = "background: #f8f4ff; border-left: 3px solid #8E44AD;",
          tags$span(
            style = "color: #8E44AD; font-weight: 600; font-size: 0.85rem;",
            icon("quote-left", class = "me-1"),
            "Suggested figure legend"
          ),
          tags$span(
            class = "text-muted ms-2",
            style = "font-size: 0.75rem; font-weight: normal;",
            "Copy and adapt as needed"
          )
        ),
        card_body(
          class = "py-2",
          tags$pre(
            style = paste0("white-space: pre-wrap; word-wrap: break-word;",
                           "font-family: inherit; font-size: 0.82rem;",
                           "background: transparent; border: none;",
                           "margin: 0; padding: 0; line-height: 1.55;"),
            legend_txt
          )
        )
      )
    })

    # ---- Summary computation -----------------------------------------------
    compute_summary_df <- function(d, stat_type) {
      has_treatment <- ".treatment" %in% names(d)
      group_vars    <- if (has_treatment) c(".time", ".treatment") else ".time"
      grp_combos    <- unique(d[, group_vars, drop = FALSE])
      out_list      <- vector("list", nrow(grp_combos))

      for (i in seq_len(nrow(grp_combos))) {
        mask <- rep(TRUE, nrow(d))
        for (gv in group_vars)
          mask <- mask & (d[[gv]] == grp_combos[[gv]][i])
        vals <- d$.conc[mask]

        if (stat_type == "geomean") {
          pos <- vals[!is.na(vals) & vals > 0]
          if (length(pos) >= 2) {
            log_v <- log(pos)
            gm    <- exp(mean(log_v))
            gsd   <- exp(sd(log_v))      # multiplicative geometric SD
            lo    <- gm / gsd
            hi    <- gm * gsd
          } else if (length(pos) == 1) {
            gm <- pos[1]; lo <- NA_real_; hi <- NA_real_
          } else {
            gm <- NA_real_; lo <- NA_real_; hi <- NA_real_
          }
          row <- data.frame(.time = grp_combos$.time[i],
                            .center = gm, .lo = lo, .hi = hi,
                            stringsAsFactors = FALSE)
        } else {
          pos <- vals[!is.na(vals)]
          am  <- if (length(pos) > 0) mean(pos) else NA_real_
          s   <- if (length(pos) > 1) sd(pos)   else NA_real_
          lo_raw <- if (!is.na(am) && !is.na(s)) am - s else NA_real_
          row <- data.frame(.time = grp_combos$.time[i],
                            .center = am,
                            # Clamp lower bar to a small positive value so
                            # log scale doesn't crash when SD > mean
                            .lo     = if (!is.na(lo_raw)) max(lo_raw, 1e-10) else NA_real_,
                            .hi     = if (!is.na(am) && !is.na(s)) am + s else NA_real_,
                            stringsAsFactors = FALSE)
        }
        if (has_treatment) row$.treatment <- grp_combos$.treatment[i]
        out_list[[i]] <- row
      }
      do.call(rbind, out_list)
    }

    # ---- Build ggplot2: spaghetti ------------------------------------------
    build_spaghetti_gg <- reactive({
      req(plot_data(), shared$col_map)
      d            <- plot_data()
      color_by_val <- input$color_by      %||% "subject"
      palette_name <- input$color_palette %||% "default"
      show_pts     <- isTRUE(input$show_points_spaghetti)
      base_sz      <- max(8L, min(18L, as.integer(input$base_size %||% 11L)))
      theme_name   <- input$plot_theme    %||% "bw"
      x_lab        <- if (nzchar(input$x_label %||% "")) input$x_label else "Time"
      y_lab        <- if (nzchar(input$y_label %||% "")) input$y_label else "Concentration"
      title_txt    <- input$plot_title %||% ""

      col_internal <- switch(color_by_val,
        "subject"   = ".subj",
        "treatment" = if (".treatment" %in% names(d)) ".treatment" else NULL,
        "period"    = if (".period"    %in% names(d)) ".period"    else NULL,
        "sequence"  = if (".sequence"  %in% names(d)) ".sequence"  else NULL,
        NULL
      )

      if (!is.null(col_internal)) {
        d[[col_internal]] <- factor(d[[col_internal]])
        n_lev       <- nlevels(d[[col_internal]])
        cols        <- palette_colours(n_lev, palette_name)
        legend_name <- switch(color_by_val,
          "subject"   = "Subject",
          "treatment" = "Treatment",
          "period"    = "Period",
          "sequence"  = "Sequence",
          "")

        p <- ggplot(d, aes(x = .time, y = .conc,
                           group  = .subj,
                           colour = .data[[col_internal]])) +
          geom_line(alpha = 0.65, linewidth = 0.55) +
          scale_color_manual(values = cols, name = legend_name)
        if (show_pts)
          p <- p + geom_point(alpha = 0.80, size = 1.6)
      } else {
        p <- ggplot(d, aes(x = .time, y = .conc, group = .subj)) +
          geom_line(colour = "#2C3E50", alpha = 0.45, linewidth = 0.55)
        if (show_pts)
          p <- p + geom_point(colour = "#2C3E50", alpha = 0.70, size = 1.6)
      }

      p <- p + labs(x = x_lab, y = y_lab,
                    title = if (nzchar(title_txt)) title_txt else NULL)
      if (isTRUE(input$y_scale == "log")) p <- p + scale_y_log10()
      apply_theme(p, theme_name, base_sz)
    })

    # ---- Build ggplot2: summary --------------------------------------------
    # Visual fixes vs first version:
    #   - error bars drawn BEFORE points (z-order) so they are not hidden
    #   - wider caps (width = 0.40), thicker bars (linewidth = 0.95, alpha = 0.90)
    #   - hollow points (shape 21, white fill, size 2.0) so bar ends show through
    #   - legend swatches forced via guide_legend(override.aes) — coloured squares
    build_summary_gg <- reactive({
      req(plot_data(), shared$col_map)
      d            <- plot_data()
      stat_type    <- input$summary_stat  %||% "geomean"
      palette_name <- input$color_palette %||% "default"
      show_pts_ind <- isTRUE(input$show_points_summary)
      base_sz      <- max(8L, min(18L, as.integer(input$base_size %||% 11L)))
      theme_name   <- input$plot_theme    %||% "bw"
      x_lab        <- if (nzchar(input$x_label %||% "")) input$x_label else "Time"
      y_lab        <- if (nzchar(input$y_label %||% "")) input$y_label else "Concentration"
      title_txt    <- input$plot_title %||% ""

      has_treatment <- ".treatment" %in% names(d)

      summ <- tryCatch(compute_summary_df(d, stat_type), error = function(e) NULL)
      req(!is.null(summ), nrow(summ) > 0)

      caption_txt <- if (stat_type == "geomean")
        "Error bars: geometric mean \u00d7\u00f7 geometric SD"
      else
        "Error bars: mean \u00b1 SD"

      if (has_treatment) {
        summ$.treatment <- factor(summ$.treatment)
        n_lev <- nlevels(summ$.treatment)
        cols  <- palette_colours(n_lev, palette_name)

        p <- ggplot(summ, aes(x = .time, y = .center,
                              colour = .treatment,
                              group  = .treatment))

        # Individual obs — drawn first (bottom layer)
        if (show_pts_ind) {
          d_pos <- d[!is.na(d$.conc) & d$.conc > 0, ]
          d_pos$.treatment <- factor(d_pos$.treatment,
                                     levels = levels(summ$.treatment))
          p <- p + geom_point(
            data        = d_pos,
            aes(x = .time, y = .conc, colour = .treatment),
            alpha       = 0.18, size = 0.9,
            inherit.aes = FALSE
          )
        }

        p <- p +
          # Error bars drawn BEFORE (below) the mean points
          geom_errorbar(
            aes(ymin = .lo, ymax = .hi),
            width     = 0.40,
            linewidth = 0.95,
            alpha     = 0.90
          ) +
          geom_line(linewidth = 1.0) +
          # Hollow points so error bar caps remain visible
          geom_point(shape = 21, size = 2.0, stroke = 1.1, fill = "white") +
          scale_color_manual(
            values = cols,
            name   = "Treatment",
            # Force clearly coloured filled squares in the legend
            guide  = guide_legend(
              override.aes = list(
                shape    = 22,
                fill     = cols,
                colour   = cols,
                size     = 4,
                linetype = 0
              )
            )
          )

      } else {
        col1 <- palette_colours(1L, palette_name)[1]

        p <- ggplot(summ, aes(x = .time, y = .center))

        if (show_pts_ind) {
          d_pos <- d[!is.na(d$.conc) & d$.conc > 0, ]
          p <- p + geom_point(
            data        = d_pos,
            aes(x = .time, y = .conc),
            colour      = col1, alpha = 0.18, size = 0.9,
            inherit.aes = FALSE
          )
        }

        p <- p +
          geom_errorbar(
            aes(ymin = .lo, ymax = .hi),
            colour    = col1,
            width     = 0.40,
            linewidth = 0.95,
            alpha     = 0.90
          ) +
          geom_line(colour = col1, linewidth = 1.0) +
          geom_point(shape = 21, size = 2.0, stroke = 1.1,
                     colour = col1, fill = "white")
      }

      p <- p + labs(x = x_lab, y = y_lab,
                    title   = if (nzchar(title_txt)) title_txt else NULL,
                    caption = caption_txt)

      if (isTRUE(input$y_scale == "log")) p <- p + scale_y_log10()
      apply_theme(p, theme_name, base_sz)
    })

    # ---- Track first successful render ------------------------------------
    plot_rendered <- reactiveVal(FALSE)

    # ---- Plotly: spaghetti -------------------------------------------------
    output$spaghetti_plotly <- renderPlotly({
      req(shared$data_ready)
      tryCatch({
        p  <- build_spaghetti_gg()
        # suppressWarnings: log10 of zero/negative produces expected Inf warning
        pl <- suppressWarnings(
          ggplotly(p, tooltip = c("x", "y", "colour")) %>%
            layout(legend = list(
              orientation = "v",
              bgcolor     = "rgba(255,255,255,0.85)",
              bordercolor = "#dee2e6",
              borderwidth = 1
            ))
        )
        plot_rendered(TRUE)
        pl
      }, error = function(e) {
        plotly_empty() %>%
          layout(title = list(
            text = paste("Plot error:", conditionMessage(e)),
            font = list(color = "#E74C3C")))
      })
    })

    # ---- Plotly: summary ---------------------------------------------------
    output$summary_plotly <- renderPlotly({
      req(shared$data_ready)
      tryCatch({
        p  <- build_summary_gg()
        pl <- suppressWarnings(
          ggplotly(p, tooltip = c("x", "y", "colour")) %>%
            layout(legend = list(
              orientation = "v",
              bgcolor     = "rgba(255,255,255,0.85)",
              bordercolor = "#dee2e6",
              borderwidth = 1
            ))
        )
        # Ensure every named trace shows a legend entry with its line colour
        for (i in seq_along(pl$x$data)) {
          nm <- pl$x$data[[i]]$name
          if (!is.null(nm) && nzchar(nm)) {
            pl$x$data[[i]]$showlegend  <- TRUE
            pl$x$data[[i]]$legendgroup <- nm
          }
        }
        plot_rendered(TRUE)
        pl
      }, error = function(e) {
        plotly_empty() %>%
          layout(title = list(
            text = paste("Plot error:", conditionMessage(e)),
            font = list(color = "#E74C3C")))
      })
    })

    # ---- Download button (greyed out until first render) ------------------
    output$download_btn_ui <- renderUI({
      if (isTRUE(plot_rendered())) {
        downloadButton(ns("dl_plot"), "Download Figure",
                       class = "btn-success btn-lg w-100 mt-1")
      } else {
        tags$button(
          class = "btn btn-secondary btn-lg w-100 mt-1 disabled",
          icon("download", class = "me-1"),
          "Download Figure",
          tags$br(),
          tags$small("(render a plot above first)")
        )
      }
    })

    # ---- Resolve which ggplot to pass to ggsave ---------------------------
    resolve_export_plot <- function() {
      pt <- input$plot_type %||% "spaghetti"
      which_plot <- if (pt == "both")
        input$export_which %||% "spaghetti"
      else
        pt
      if (which_plot == "summary") build_summary_gg() else build_spaghetti_gg()
    }

    # ---- Download handler --------------------------------------------------
    output$dl_plot <- downloadHandler(
      filename = function() {
        fmt <- input$export_format %||% "png"
        pt  <- input$plot_type     %||% "spaghetti"
        pfx <- if (pt == "both") {
          if ((input$export_which %||% "spaghetti") == "summary")
            "summary" else "spaghetti"
        } else pt
        paste0(pfx, "_plot.", fmt)
      },
      content = function(file) {
        w   <- as.numeric(input$export_width  %||% 7)
        h   <- as.numeric(input$export_height %||% 5)
        r   <- as.integer(input$export_dpi    %||% 300)
        fmt <- input$export_format %||% "png"

        if (is.na(w) || w < 2 || w > 20) {
          showNotification("Width must be between 2 and 20 inches.",
                           type = "error")
          return(NULL)
        }
        if (is.na(h) || h < 2 || h > 20) {
          showNotification("Height must be between 2 and 20 inches.",
                           type = "error")
          return(NULL)
        }

        p <- tryCatch(
          resolve_export_plot(),
          error = function(e) {
            showNotification(paste("Figure build failed:", conditionMessage(e)),
                             type = "error", duration = 8)
            NULL
          }
        )
        req(!is.null(p))

        tryCatch(
          ggplot2::ggsave(filename = file, plot = p, device = fmt,
                          width = w, height = h, dpi = r, units = "in"),
          error = function(e)
            showNotification(paste("Export failed:", conditionMessage(e)),
                             type = "error", duration = 8)
        )
      }
    )

    # ---- Write viz settings back to shared (for analysis record) ----------
    observe({
      req(shared$data_ready, plot_rendered())
      shared$viz_settings <- list(
        plot_type         = input$plot_type      %||% "spaghetti",
        color_by          = input$color_by       %||% "subject",
        y_scale           = input$y_scale        %||% "linear",
        summary_statistic = input$summary_stat   %||% "geomean",
        dose_normalized   = isTRUE(input$dose_norm),
        theme             = input$plot_theme     %||% "bw",
        colour_palette    = input$color_palette  %||% "default",
        figure_width_in   = as.numeric(input$export_width  %||% 7),
        figure_height_in  = as.numeric(input$export_height %||% 5),
        dpi               = as.integer(input$export_dpi    %||% 300),
        export_format     = input$export_format  %||% "png",
        blq_excluded_n    = blq_n_summary()
      )
    })

  })
}


# ---------------------------------------------------------------------------
# Null-coalescing helper (file-scope so it is available inside server closure)
# ---------------------------------------------------------------------------
`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) > 0 && !is.na(x[1])) x else y
}
