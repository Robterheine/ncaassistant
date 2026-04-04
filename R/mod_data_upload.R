# ============================================================================
# NCA Assistant — Shared Data Upload Module
# ============================================================================
# Reusable data upload + column mapping + BLQ + quality check.
# Embedded within Path 2 (standalone) and referenced by Paths 3, 4, 5.

data_upload_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        class = "bg-primary text-white",
        "Upload PK Data", help_data_format
      ),
      card_body(
        tags$p(class = "small text-muted mb-3",
               icon("book-open", class = "me-1"),
               "Not sure if your file is ready? Check the ",
               tags$a(href = "#",
                      onclick = "Shiny.setInputValue('nav_path', 'guide', {priority: 'event'}); return false;",
                      "Data Preparation Guide"),
               " for format instructions and example datasets."),
        layout_columns(
          col_widths = c(4, 8),
          
          # Left: Upload + settings
          tagList(
            fileInput(ns("file_upload"), NULL,
                      accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv"),
                      placeholder = "Choose CSV or Excel file"),
            
            conditionalPanel(
              condition = sprintf("output['%s'] == true", ns("is_csv")),
              layout_columns(
                col_widths = c(6, 6),
                selectInput(ns("csv_sep"), "Delimiter",
                            choices = c("Comma" = ",", "Semicolon" = ";",
                                        "Tab" = "\t"), selected = ","),
                selectInput(ns("csv_dec"), "Decimal point",
                            choices = c("Period (.)" = ".", "Comma (,)" = ","))
              )
            ),
            
            conditionalPanel(
              condition = sprintf("output['%s'] == true", ns("is_excel")),
              numericInput(ns("excel_sheet"), "Sheet number", value = 1, min = 1)
            )
          ),
          
          # Right: Status
          uiOutput(ns("upload_status"))
        )
      )
    ),
    
    # Column mapping (only shown after upload)
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("has_data")),
      
      card(
        card_header(
          class = "bg-primary text-white",
          "Map Your Columns", help_column_mapping
        ),
        card_body(
          tags$p(class = "text-muted small",
                 "Tell the app which column contains what. It guessed below — ",
                 "correct any that are wrong."),
          
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput(ns("col_subject"), "Subject ID", choices = NULL),
            selectInput(ns("col_time"), "Time", choices = NULL),
            selectInput(ns("col_conc"), "Concentration", choices = NULL)
          ),
          
          tags$details(
            tags$summary(class = "fw-semibold small mb-2",
                         "Additional columns (for crossover / BE studies)"),
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              selectInput(ns("col_treatment"), "Treatment / Formulation",
                          choices = NULL),
              selectInput(ns("col_period"), "Period", choices = NULL),
              selectInput(ns("col_sequence"), "Sequence", choices = NULL),
              selectInput(ns("col_dose"), "Dose (per subject)", choices = NULL)
            )
          ),
          
          hr(),
          
          layout_columns(
            col_widths = c(4, 8),
            numericInput(ns("lloq"),
                         tagList("LLOQ value", help_lloq),
                         value = 0, min = 0, step = 0.01),
            tags$div(
              radioButtons(ns("blq_rule"),
                           tagList("BLQ handling rule", help_blq_rules),
                           choiceNames = list(
                             tags$span("Standard", tags$span(class="text-muted", " \u2014 pre-first = 0, post-last = missing (Rule 1, WinNonlin default)")),
                             tags$span("All BLQ \u2192 zero", tags$span(class="text-muted", " (Rule 2)")),
                             tags$span("All BLQ \u2192 excluded", tags$span(class="text-muted", " (Rule 3)")),
                             tags$span("All BLQ \u2192 LLOQ/2", tags$span(class="text-muted", " (Rule 4)")),
                             tags$span("Before peak \u2192 zero, after peak \u2192 excluded", tags$span(class="text-muted", " (Rule 5)")),
                             tags$span("First BLQ \u2192 LLOQ/2, rest \u2192 zero", tags$span(class="text-muted", " (Rule 6, for drugs with lag time)"))
                           ),
                           choiceValues = c("rule1", "rule2", "rule3", "rule4", "rule5", "rule6"),
                           selected = "rule1")
            )
          ),
          
          tags$div(
            class = "text-end mt-2",
            actionButton(ns("btn_apply"), "Process Data",
                         class = "btn-success",
                         icon = icon("check"))
          )
        )
      ),
      
      # Quality report
      uiOutput(ns("quality_report_card"))
    )
  )
}

data_upload_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # File type detection
    file_ext <- reactive({
      req(input$file_upload)
      tools::file_ext(input$file_upload$name)
    })
    
    output$is_csv <- reactive({ file_ext() %in% c("csv", "txt", "tsv") })
    outputOptions(output, "is_csv", suspendWhenHidden = FALSE)
    
    output$is_excel <- reactive({ file_ext() %in% c("xlsx", "xls") })
    outputOptions(output, "is_excel", suspendWhenHidden = FALSE)
    
    output$has_data <- reactive({ !is.null(raw_data()) })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    
    # Reset all shared state when a new file is uploaded
    observeEvent(input$file_upload, {
      shared$qc_result   <- NULL
      shared$pk_data      <- NULL
      shared$col_map      <- NULL
      shared$data_ready   <- FALSE
      shared$nca_results  <- NULL
      shared$nca_settings <- NULL
      shared$raw_data     <- NULL
      shared$study_info   <- NULL
    }, priority = 10)  # high priority: runs before raw_data() updates
    
    # Read raw data
    raw_data <- reactive({
      req(input$file_upload)
      ext <- file_ext()
      path <- input$file_upload$datapath
      tryCatch({
        if (ext %in% c("xlsx", "xls")) {
          readxl::read_excel(path, sheet = input$excel_sheet)
        } else {
          read.csv(path, sep = input$csv_sep, dec = input$csv_dec,
                   stringsAsFactors = FALSE)
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message),
                         type = "error", duration = 8)
        NULL
      })
    })
    
    # Upload status
    output$upload_status <- renderUI({
      if (is.null(raw_data())) {
        tags$div(
          class = "text-center py-4 text-muted",
          icon("cloud-arrow-up", class = "fa-2x mb-2"),
          tags$p("Upload a CSV or Excel file to begin.")
        )
      } else {
        d <- raw_data()
        tags$div(
          class = "py-2",
          tags$div(class = "d-flex align-items-center mb-2",
                   icon("circle-check", class = "text-success me-2"),
                   tags$strong(input$file_upload$name)),
          tags$p(class = "text-muted small mb-0",
                 paste(nrow(d), "rows,", ncol(d), "columns. ",
                       "Columns: ", paste(head(names(d), 6), collapse = ", "),
                       if (ncol(d) > 6) "..." else ""))
        )
      }
    })
    
    # Auto-detect columns
    observe({
      req(raw_data())
      cols <- names(raw_data())
      guess <- auto_detect_columns(cols)
      none_choice <- c("(none)" = "")
      
      updateSelectInput(session, "col_subject", choices = cols, selected = guess$subject)
      updateSelectInput(session, "col_time",    choices = cols, selected = guess$time)
      updateSelectInput(session, "col_conc",    choices = cols, selected = guess$conc)
      updateSelectInput(session, "col_treatment",
                        choices = c(none_choice, cols), selected = guess$treatment)
      updateSelectInput(session, "col_period",
                        choices = c(none_choice, cols), selected = guess$period)
      updateSelectInput(session, "col_sequence",
                        choices = c(none_choice, cols), selected = guess$sequence)
      updateSelectInput(session, "col_dose",
                        choices = c(none_choice, cols), selected = guess$dose)
    })
    
    # Process data
    observeEvent(input$btn_apply, {
      req(raw_data())
      
      col_map <- list(
        subject = input$col_subject,
        time    = input$col_time,
        conc    = input$col_conc
      )
      if (input$col_treatment != "") col_map$treatment <- input$col_treatment
      if (input$col_period != "")    col_map$period    <- input$col_period
      if (input$col_sequence != "")  col_map$sequence  <- input$col_sequence
      if (input$col_dose != "")      col_map$dose      <- input$col_dose
      
      # Validate
      val <- validate_mapping(col_map)
      if (!val$valid) {
        showNotification(val$message, type = "error", duration = 5)
        return()
      }
      
      # Quality check
      qc <- run_data_quality_check(raw_data(), col_map, lloq = input$lloq)
      shared$qc_result <- qc
      
      # Auto-detect LLOQ from BLQ text entries if not set
      if (input$lloq <= 0) {
        conc_raw <- as.character(raw_data()[[col_map$conc]])
        lt_vals <- conc_raw[grepl("^<", conc_raw)]
        if (length(lt_vals) > 0) {
          lt_nums <- suppressWarnings(as.numeric(gsub("^<\\s*", "", lt_vals)))
          lt_nums <- lt_nums[!is.na(lt_nums)]
          if (length(lt_nums) > 0) {
            detected_lloq <- min(lt_nums)
            updateNumericInput(session, "lloq", value = detected_lloq)
            showNotification(
              paste0("LLOQ auto-detected as ", detected_lloq,
                     " from your BLQ entries. Click Process Data again to apply."),
              type = "message", duration = 8)
          }
        }
      }
      
      if (!qc$pass) {
        showNotification(
          paste0(qc$n_errors, " error(s) found. Fix them before proceeding."),
          type = "error", duration = 8)
        return()
      }
      
      # Process
      data <- raw_data()
      data[[col_map$time]] <- suppressWarnings(as.numeric(data[[col_map$time]]))
      data[[col_map$conc]] <- suppressWarnings(as.numeric(data[[col_map$conc]]))
      
      if (input$lloq > 0) {
        data <- apply_blq_rules(data, col_map, rule = input$blq_rule,
                                lloq = input$lloq)
      }
      
      data <- data[!is.na(data[[col_map$time]]), ]
      data <- data[order(data[[col_map$subject]], data[[col_map$time]]), ]
      
      design <- detect_study_design(data, col_map)
      
      shared$raw_data   <- raw_data()
      shared$pk_data    <- data
      shared$col_map    <- col_map
      shared$data_ready <- TRUE
      shared$study_info <- list(
        design    = design,
        lloq      = input$lloq,
        blq_rule  = input$blq_rule,
        file_name = input$file_upload$name,
        file_path = input$file_upload$datapath
      )
      
      showNotification(
        paste0("Data ready: ", design$n_subjects, " subjects, ",
               nrow(data), " observations."),
        type = "message", duration = 4)
    })
    
    # Quality report
    output$quality_report_card <- renderUI({
      req(shared$qc_result)
      qc <- shared$qc_result
      
      header_cls <- if (!qc$pass) "bg-danger text-white"
                    else if (qc$n_warnings > 0) "bg-warning text-dark"
                    else "bg-success text-white"
      header_text <- if (!qc$pass) "Data Quality — Issues Found"
                     else if (qc$n_warnings > 0) "Data Quality — Warnings"
                     else "Data Quality — All Clear"
      
      card(
        card_header(class = header_cls,
                    icon("clipboard-check"), " ", header_text),
        card_body(
          style = "max-height: 350px; overflow-y: auto;",
          render_quality_report(qc)
        )
      )
    })
  })
}

# Column auto-detection (same as v1, with extended patterns)
auto_detect_columns <- function(cols) {
  cols_lower <- tolower(cols)
  
  detect <- function(patterns, fallback_idx = 1) {
    for (p in patterns) {
      match <- grep(p, cols_lower, value = FALSE)
      if (length(match) > 0) return(cols[match[1]])
    }
    return(cols[min(fallback_idx, length(cols))])
  }
  
  detect_optional <- function(patterns) {
    for (p in patterns) {
      match <- grep(p, cols_lower, value = FALSE)
      if (length(match) > 0) return(cols[match[1]])
    }
    return("")
  }
  
  list(
    subject   = detect(c("^subj", "^id$", "^subject", "^usubjid", "^patid",
                          "^pat$", "^proband", "^teilnehmer"), 1),
    time      = detect(c("^time", "^tpt", "^hours?$", "^hour", "^apts",
                          "^ntim", "^zeit", "^tid"), 2),
    conc      = detect(c("^conc", "^dv$", "^cp[^a-z]", "^cp$", "^concentration",
                          "^result", "^konz", "^plasma", "ug.l", "ng.ml"), 3),
    treatment = detect_optional(c("^trt", "^treat", "^form", "^drug", "^arm",
                                   "^behandl")),
    period    = detect_optional(c("^per", "^period", "^prd", "^phase")),
    sequence  = detect_optional(c("^seq", "^grp", "^sequence")),
    dose      = detect_optional(c("^dose", "^amt$", "^amount", "^dosis"))
  )
}
