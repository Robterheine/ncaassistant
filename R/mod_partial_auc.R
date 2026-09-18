# ============================================================================
# NCA Assistant — Partial AUC interval editor
# ============================================================================
# Used by All Subjects, One Subject at a Time and Bioequivalence. The rows are
# static inputs shown by count, so entered values survive adding or removing
# intervals.

PARTIAL_AUC_MAX <- 6

partial_auc_ui <- function(id, show_role = FALSE) {
  ns <- NS(id)
  row_ui <- function(i) {
    conditionalPanel(
      condition = sprintf("Number(input['%s']) >= %d", ns("n"), i),
      tags$div(
        class = "border rounded p-2 mb-2",
        tags$div(class = "small fw-semibold mb-1", paste("Interval", i)),
        layout_columns(
          col_widths = c(6, 6),
          numericInput(ns(paste0("start", i)), "Start", value = NA, min = 0),
          conditionalPanel(
            condition = sprintf("!input['%s']", ns(paste0("to_t", i))),
            numericInput(ns(paste0("end", i)), "End", value = NA, min = 0))
        ),
        checkboxInput(ns(paste0("to_t", i)), "End at the last measurable concentration (t)", FALSE),
        checkboxInput(ns(paste0("cmax", i)), "Also report Cmax and Tmax in this interval", FALSE),
        if (show_role)
          radioButtons(ns(paste0("role", i)), "Role in bioequivalence",
                       choices = c("Pivotal (verdict)" = "pivotal", "Supportive (no verdict)" = "supportive"),
                       inline = TRUE)
      )
    )
  }
  card(
    card_header(class = "bg-primary text-white", tagList("Partial AUCs (optional)", help_partial_auc)),
    card_body(
      # dropdownParent = "body": a card clips a dropdown that opens inside it
      selectizeInput(ns("n"), "Number of intervals", choices = 0:PARTIAL_AUC_MAX, selected = 0,
                     options = list(dropdownParent = "body")),
      conditionalPanel(
        condition = sprintf("Number(input['%s']) > 0", ns("n")),
        tags$p(class = "text-muted small mb-2",
               "Enter the intervals from the protocol, in the time unit of your data.")),
      lapply(seq_len(PARTIAL_AUC_MAX), row_ui)
    )
  )
}

#' @return reactive: interval specification (partial_auc_spec) or NULL
partial_auc_server <- function(id, show_role = FALSE) {
  moduleServer(id, function(input, output, session) {
    reactive({
      n <- suppressWarnings(as.integer(input$n))
      if (length(n) != 1 || is.na(n) || n == 0) return(NULL)
      rows <- lapply(seq_len(n), function(i) {
        num <- function(v) if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v))
        data.frame(start = num(input[[paste0("start", i)]]),
                   end = if (isTRUE(input[[paste0("to_t", i)]])) "t" else as.character(num(input[[paste0("end", i)]])),
                   cmax = isTRUE(input[[paste0("cmax", i)]]),
                   role = if (show_role && !is.null(input[[paste0("role", i)]])) input[[paste0("role", i)]] else "pivotal",
                   stringsAsFactors = FALSE)
      })
      partial_auc_spec(do.call(rbind, rows))
    })
  })
}

#' Alert listing the notes about partial AUCs from the last run
partial_auc_notes_ui <- function(notes) {
  if (length(notes) == 0) return(NULL)
  tags$div(class = "alert alert-info py-2 small mb-2",
           icon("circle-info", class = "me-1"),
           tags$strong("Partial AUCs: "),
           tags$ul(class = "mb-0", lapply(notes, tags$li)))
}
