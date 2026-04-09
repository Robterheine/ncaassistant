# ============================================================================
# NCA Assistant — Path 2: Upload & Check Data
# ============================================================================

path_data_ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container-fluid py-3",
    style = "max-width: 1100px; margin: 0 auto;",
    
    tags$h4(class = "fw-bold mb-3",
            icon("file-import", class = "text-success me-2"),
            "Upload & Check Your Data"),
    tags$p(class = "text-muted",
           "Upload your concentration-time data, map the columns, and run a quality check. ",
           "Once processed, your data is available for all analysis paths."),
    
    # Shared upload module
    data_upload_ui(ns("upload")),
    
    # After processing: data preview + navigation
    uiOutput(ns("post_upload"))
  )
}

path_data_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Embed the shared upload module
    data_upload_server("upload", shared)
    
    # Post-upload: preview + next-step navigation
    output$post_upload <- renderUI({
      req(shared$data_ready)
      
      d  <- shared$pk_data
      cm <- shared$col_map
      info <- shared$study_info
      
      tagList(
        # Summary cards
        card(
          card_header("Processed Data Summary"),
          card_body(
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              value_box(title = "Subjects", value = info$design$n_subjects,
                        theme = "primary", showcase = icon("users")),
              value_box(title = "Observations", value = nrow(d),
                        theme = "info", showcase = icon("vials")),
              value_box(title = "Missing Conc",
                        value = sum(is.na(d[[cm$conc]])),
                        theme = if (sum(is.na(d[[cm$conc]])) > 0) "warning" else "success",
                        showcase = icon("triangle-exclamation")),
              value_box(title = "Study Design", value = info$design$type,
                        theme = "secondary", showcase = icon("sitemap"))
            )
          )
        ),
        
        # Data preview
        card(
          card_header("Data Preview (first 200 rows)"),
          card_body(
            DTOutput(ns("preview_table"))
          )
        ),
        
        # What to do next
        card(
          card_header(class = "bg-success text-white",
                      icon("route"), " Your data is ready — what's next?"),
          card_body(
            class = "py-3",
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              
              tags$div(
                class = "text-center p-2",
                tags$div(class = "mb-2",
                         icon("chart-line", class = "fa-2x",
                              style = "color: #8E44AD;")),
                tags$h6("Visualize Data"),
                tags$p(class = "text-muted small",
                       "Plot individual profiles and geometric mean curves. Export for manuscripts."),
                actionButton(ns("goto_viz"), "Visualize Data",
                             class = "btn-outline-secondary btn-sm",
                             style = "border-color: #8E44AD; color: #8E44AD;",
                             onclick = "Shiny.setInputValue('nav_path', 'viz', {priority: 'event'});")
              ),
              
              tags$div(
                class = "text-center p-2",
                tags$div(class = "mb-2",
                         icon("user", class = "fa-2x text-warning")),
                tags$h6("Analyze One Subject"),
                tags$p(class = "text-muted small",
                       "Review profiles one at a time. Good for QC and dose-escalation."),
                actionButton(ns("goto_single"), "Single Subject",
                             class = "btn-outline-warning btn-sm",
                             onclick = "Shiny.setInputValue('nav_path', 'single_nca', {priority: 'event'});")
              ),
              
              tags$div(
                class = "text-center p-2",
                tags$div(class = "mb-2",
                         icon("users", class = "fa-2x text-primary")),
                tags$h6("Analyze All Subjects"),
                tags$p(class = "text-muted small",
                       "Batch NCA on all subjects. Get summary statistics and export."),
                actionButton(ns("goto_multi"), "Batch Analysis",
                             class = "btn-outline-primary btn-sm",
                             onclick = "Shiny.setInputValue('nav_path', 'multi_nca', {priority: 'event'});")
              ),
              
              tags$div(
                class = "text-center p-2",
                tags$div(class = "mb-2",
                         icon("arrows-left-right", class = "fa-2x text-danger")),
                tags$h6("Bioequivalence"),
                tags$p(class = "text-muted small",
                       "Compare Test vs. Reference formulations with 90% CI."),
                actionButton(ns("goto_be"), "Bioequivalence",
                             class = "btn-outline-danger btn-sm",
                             onclick = "Shiny.setInputValue('nav_path', 'be', {priority: 'event'});")
              )
            )
          )
        )
      )
    })
    
    output$preview_table <- renderDT({
      req(shared$pk_data)
      datatable(head(shared$pk_data, 200),
                options = list(scrollX = TRUE, pageLength = 10, dom = "frtip"),
                rownames = FALSE, class = "compact stripe hover")
    })
  })
}
