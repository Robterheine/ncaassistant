# ============================================================================
# NCA Assistant — Half-life quality rules (shared by the analysis paths)
# ============================================================================
# One summary line under the minimum R2 setting, and a dialog to edit the
# rules. The rules only flag (lambda_z_flags() in R/pipeline.R); the minimum
# R2 keeps its own control, which blanks. One set of rules for the whole app,
# kept in shared$lz_rules, so every path flags the same way.

#' The rules in plain words, e.g. for the sidebar
lz_rules_summary <- function(rules) {
  part <- function(v, txt) if (is.na(v)) NULL else sprintf(txt, format(v))
  on <- c(part(rules$span_min, "span ≥ %s half-lives"),
          part(rules$aucpext_max, "extrapolated ≤ %s%%"),
          part(rules$aucpbe_max, "back-extrapolated ≤ %s%% (IV bolus)"))
  paste0("Half-life flags: ", if (length(on) == 0) "all rules off" else paste(on, collapse = " · "))
}

lz_rules_ui <- function(id) {
  ns <- NS(id)
  tags$div(class = "mb-2",
    tags$p(class = "small text-muted mb-1", textOutput(ns("line"), inline = TRUE)),
    actionLink(ns("edit"), "Edit half-life rules…", class = "small"))
}

#' @return reactive: the rules in force (LZ_RULES_DEFAULT until edited)
lz_rules_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rules <- reactive(if (is.null(shared$lz_rules)) LZ_RULES_DEFAULT else shared$lz_rules)
    output$line <- renderText(lz_rules_summary(rules()))
    row <- function(key, label, value, default, help) {
      on <- !is.na(value)
      tags$div(class = "mb-3",
        checkboxInput(ns(paste0(key, "_on")), label, value = on),
        numericInput(ns(key), NULL, value = if (on) value else default, min = 0, step = 0.5, width = "140px"),
        tags$p(class = "small text-muted mb-0", help))
    }
    observeEvent(input$edit, {
      r <- rules()
      showModal(modalDialog(
        title = "Half-life rules", easyClose = TRUE,
        tags$p(class = "small",
               "These rules only flag a half-life fit and never change a value. Whether the half-life ",
               "is reported still depends on the minimum R² in the settings. Fits from points you ",
               "chose in Half-Life Review are flagged too."),
        row("span_min", "Span of the fitted points of at least (half-lives)", r$span_min, 2,
            "Span = time between the first and last point of the fit, divided by the half-life. Informational at steady state."),
        row("aucpext_max", "AUC to infinity extrapolated at most (%)", r$aucpext_max, 20,
            "Flags AUC to infinity and what is derived from it. Not used at steady state."),
        row("aucpbe_max", "IV bolus: AUC back-extrapolated to time 0 at most (%)", r$aucpbe_max, 20,
            "Flags AUC to infinity and what is derived from it."),
        footer = tagList(modalButton("Cancel"), actionButton(ns("apply"), "Apply", class = "btn-primary"))))
    })
    observeEvent(input$apply, {
      val <- function(key) {
        v <- suppressWarnings(as.numeric(input[[key]]))
        if (!isTRUE(input[[paste0(key, "_on")]]) || length(v) != 1 || is.na(v) || v < 0) NA_real_ else v
      }
      shared$lz_rules <- list(span_min = val("span_min"), aucpext_max = val("aucpext_max"),
                              aucpbe_max = val("aucpbe_max"))
      removeModal()
    })
    rules
  })
}
