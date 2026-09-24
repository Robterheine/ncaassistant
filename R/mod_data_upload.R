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
        radioButtons(ns("data_type"),
                     tagList("What kind of file?", help_data_type),
                     choices = c("Simple table (one row per sample)" = "flat",
                                 "CDISC ADNCA dataset" = "adnca"),
                     selected = "flat", inline = TRUE),
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

    # ADNCA import: summary, choices and conversion (only in ADNCA mode)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'adnca' && output['%s'] == true",
                          ns("data_type"), ns("has_file")),
      uiOutput(ns("adnca_panel"))
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
              selectInput(ns("col_dose"), "Dose", choices = NULL)
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
                             tags$span("Standard", tags$span(class="text-muted", " \u2014 pre-first = 0, post-last = missing (Rule 1, default)")),
                             tags$span("All BLQ \u2192 zero", tags$span(class="text-muted", " (Rule 2)")),
                             tags$span("All BLQ \u2192 excluded", tags$span(class="text-muted", " (Rule 3)")),
                             tags$span("All BLQ \u2192 LLOQ/2", tags$span(class="text-muted", " (Rule 4)")),
                             tags$span("Before peak \u2192 zero, after peak \u2192 excluded", tags$span(class="text-muted", " (Rule 5)")),
                             tags$span("Before first quantifiable \u2192 LLOQ/2, rest \u2192 zero", tags$span(class="text-muted", " (Rule 6)"))
                           ),
                           choiceValues = c("rule1", "rule2", "rule3", "rule4", "rule5", "rule6"),
                           selected = "rule1")
            )
          ),
          
          uiOutput(ns("lloq_apply_ui")),
          
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
    lloq_suggestion <- reactiveVal(NULL)  # suggested LLOQ from auto-detection

    # Render "Apply suggested LLOQ and process" button when suggestion is available
    output$lloq_apply_ui <- renderUI({
      sug <- lloq_suggestion()
      if (is.null(sug)) return(NULL)
      tags$div(
        class = "alert alert-info py-2 small mt-2",
        icon("circle-info", class = "me-1"),
        tags$strong(paste0("LLOQ auto-detected: ", sug)),
        tags$br(),
        "A value of ", tags$strong(sug), " was inferred from your BLQ entries. ",
        actionButton(ns("btn_apply_lloq"),
                     paste0("Apply LLOQ = ", sug, " and process"),
                     class = "btn-primary btn-sm mt-1",
                     icon = icon("check"))
      )
    })

    # "Apply suggested LLOQ" handler.
    # Sets the LLOQ field to the auto-detected value and clears the suggestion
    # banner. The user then clicks "Process Data" once more to complete upload.
    # This two-step keeps the flow transparent without adding package dependencies.
    observeEvent(input$btn_apply_lloq, {
      sug <- lloq_suggestion()
      req(!is.null(sug), sug > 0)
      updateNumericInput(session, "lloq", value = sug)
      lloq_suggestion(NULL)
      showNotification(
        paste0("LLOQ set to ", sug, ". Click ‘Process Data’ to continue."),
        type = "message", duration = 6)
    })
    
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

    output$has_file <- reactive({ !is.null(input$file_upload) })
    outputOptions(output, "has_file", suspendWhenHidden = FALSE)
    
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
      shared$pk_dataset   <- NULL
    }, priority = 10)  # high priority: runs before raw_data() updates
    
    # How the file is read; recorded so the reproduction reads it the same way
    read_args <- reactive({
      if (file_ext() %in% c("xlsx", "xls")) list(sheet = input$excel_sheet)
      else list(sep = input$csv_sep, dec = input$csv_dec)
    })

    # ---- CDISC ADNCA import (R/adnca_import.R) ------------------------------
    adnca_conv  <- reactiveVal(NULL)   # successful conversion
    adnca_error <- reactiveVal(NULL)   # refusal message

    adnca_file <- reactive({
      req(input$file_upload, input$data_type == "adnca", !identical(tolower(file_ext()), "xpt"))
      tryCatch(adnca_read(input$file_upload$datapath, read_args(), ext = file_ext()),
               error = function(e) {
                 showNotification(paste("Error reading file:", e$message), type = "error", duration = 8)
                 NULL
               })
    })
    adnca_info <- reactive({ req(adnca_file()); adnca_inspect(adnca_file()) })

    # Any change of file, file type or choice invalidates a previous conversion
    observeEvent(list(input$file_upload, input$data_type, input$csv_sep, input$csv_dec,
                      input$excel_sheet, input$adnca_time, input$adnca_paramcd,
                      input$adnca_pcspec, input$adnca_zero_predose), {
      adnca_conv(NULL); adnca_error(NULL)
    }, ignoreInit = TRUE)

    output$adnca_panel <- renderUI({
      if (identical(tolower(file_ext()), "xpt")) {
        return(card(card_body(class = "alert alert-warning mb-0",
          icon("triangle-exclamation", class = "me-1"),
          tags$strong("XPT files are not read. "),
          "Convert the file to CSV first, for example in R: ",
          tags$code('d <- haven::read_xpt("adnca.xpt")'), " then ",
          tags$code('write.csv(haven::zap_labels(d), "adnca.csv", row.names = FALSE, na = "")'),
          ", and upload the CSV.")))
      }
      info <- adnca_info()
      if (!info$is_adnca) {
        return(card(card_body(class = "alert alert-warning mb-0",
          icon("triangle-exclamation", class = "me-1"),
          "This file does not look like an ADNCA dataset (USUBJID and AVAL are required). ",
          "If it is a simple table, choose 'Simple table' above.")))
      }
      fmt_vals <- function(x) if (length(x) == 0) "\u2014" else paste(x, collapse = ", ")
      units_txt <- if (length(info$units) == 0) "\u2014" else
        paste(paste0(names(info$units), ": ", vapply(info$units, fmt_vals, "")), collapse = "; ")
      time_choices <- stats::setNames(info$time_vars,
        paste0(c(NRRLT = "Nominal time (NRRLT)", ARRLT = "Actual time (ARRLT)",
                 MRRLT = "Actual time, pre-dose at 0 (MRRLT)")[info$time_vars]))
      card(
        card_header(class = "bg-primary text-white", "CDISC ADNCA dataset: check and convert",
                    help_data_type),
        card_body(
          tags$table(class = "table table-sm small mb-3",
            tags$tbody(
              tags$tr(tags$th("Records"), tags$td(info$n_records)),
              tags$tr(tags$th("Analytes"), tags$td(paste0(fmt_vals(info$analytes),
                                                          if (!is.null(info$analyte_var)) paste0(" (", info$analyte_var, ")")))),
              tags$tr(tags$th("Matrices (PCSPEC)"), tags$td(fmt_vals(info$matrices))),
              tags$tr(tags$th("Time variables"), tags$td(fmt_vals(info$time_vars),
                                                         if (info$has_afrlt) " (AFRLT present, not used)")),
              tags$tr(tags$th("Units"), tags$td(units_txt)),
              tags$tr(tags$th("LLOQ (PCLLOQ)"), tags$td(fmt_vals(info$lloq))),
              tags$tr(tags$th("Not in analysis set (ANL01FL \u2260 Y)"),
                      tags$td(if (is.na(info$n_anl01fl_excluded)) "ANL01FL not present"
                              else paste(info$n_anl01fl_excluded, "record(s), will be dropped"))),
              tags$tr(tags$th("Derived records (DTYPE)"),
                      tags$td(if (info$n_derived > 0) tags$span(class = "text-danger fw-semibold",
                                paste(info$n_derived, "record(s): the dataset will be refused"))
                              else "none"))
            )
          ),
          if (length(info$time_vars) == 0) {
            tags$div(class = "alert alert-danger small",
                     "No usable time variable (NRRLT, ARRLT or MRRLT). ",
                     if (length(info$datetime_vars)) paste0("Only date-times were found (",
                       paste(info$datetime_vars, collapse = ", "), "). "),
                     "Elapsed time must be derived before upload.")
          } else tagList(
            radioButtons(ns("adnca_time"), tagList("Time to use", help_adnca_time),
                         choices = time_choices, selected = character(0)),
            if (isTRUE(info$negative_times[["ARRLT"]]))
              conditionalPanel(
                condition = sprintf("input['%s'] == 'ARRLT'", ns("adnca_time")),
                checkboxInput(ns("adnca_zero_predose"),
                              "Set pre-dose times (negative ARRLT) to 0", value = FALSE)),
            if (length(info$analytes) > 1)
              selectInput(ns("adnca_paramcd"), tagList("Analyte to analyse", help_adnca_analyte),
                          choices = c("Choose..." = "", info$analytes)),
            if (length(info$matrices) > 1)
              selectInput(ns("adnca_pcspec"), tagList("Matrix to analyse", help_adnca_analyte),
                          choices = c("Choose..." = "", info$matrices)),
            actionButton(ns("btn_adnca_convert"), "Convert dataset", class = "btn-primary",
                         icon = icon("right-left"))
          ),
          uiOutput(ns("adnca_result"))
        )
      )
    })

    observeEvent(input$btn_adnca_convert, {
      d <- adnca_file(); req(d)
      if (is.null(input$adnca_time) || !nzchar(input$adnca_time)) {
        adnca_error("Choose the time to use."); adnca_conv(NULL); return()
      }
      res <- tryCatch(
        adnca_convert(d, time = input$adnca_time, paramcd = input$adnca_paramcd,
                      pcspec = input$adnca_pcspec, zero_predose = isTRUE(input$adnca_zero_predose)),
        adnca_refusal = function(e) e,
        error = function(e) e)
      if (inherits(res, "condition")) {
        adnca_error(sub("^Refused: ", "", conditionMessage(res))); adnca_conv(NULL)
      } else {
        adnca_error(NULL)
        adnca_conv(c(res, list(n_records = nrow(d))))
        if (!is.null(res$lloq)) updateNumericInput(session, "lloq", value = res$lloq)
      }
    })

    output$adnca_result <- renderUI({
      if (!is.null(adnca_error())) {
        return(tags$div(class = "alert alert-danger small mt-3",
                        icon("ban", class = "me-1"), tags$strong("Not converted: "), adnca_error()))
      }
      conv <- adnca_conv(); if (is.null(conv)) return(NULL)
      tags$div(class = "alert alert-success small mt-3",
               icon("circle-check", class = "me-1"),
               tags$strong(paste0("Converted: ", nrow(conv$flat), " samples ready. ")),
               "Check the column mapping and LLOQ below, then click Process Data.",
               tags$ul(class = "mb-0 mt-1", lapply(conv$notes, tags$li)))
    })

    # Read raw data: the uploaded table, or the converted ADNCA dataset
    raw_data <- reactive({
      req(input$file_upload)
      if (identical(input$data_type, "adnca")) {
        conv <- adnca_conv()
        req(conv)
        return(conv$flat)
      }
      ext <- file_ext()
      path <- input$file_upload$datapath
      tryCatch({
        read_pk_file(path, read_args(), ext = ext)
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message),
                         type = "error", duration = 8)
        NULL
      })
    })
    
    # Upload status
    output$upload_status <- renderUI({
      if (identical(input$data_type, "adnca") && !is.null(input$file_upload) && is.null(adnca_conv())) {
        info <- tryCatch(adnca_info(), error = function(e) NULL)
        return(tags$div(class = "py-2",
          tags$div(class = "d-flex align-items-center mb-2",
                   icon("file-medical", class = "text-primary me-2"),
                   tags$strong(input$file_upload$name)),
          tags$p(class = "text-muted small mb-0",
                 if (!is.null(info)) paste0(info$n_records, " records. ") else "",
                 "Check the summary below, choose the time to use and click Convert dataset.")))
      }
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

      # Required columns that matched no known name were filled in by position
      unmatched <- attr(guess, "unmatched")
      if ("subject" %in% unmatched) {
        showNotification(
          paste0("No Subject column recognised. The app pre-selected '", guess$subject,
                 "' as Subject, which is probably wrong. Choose the Subject column; ",
                 "for a single profile, add a Subject column to the file."),
          type = "warning", duration = NULL)
      }
      other <- setdiff(unmatched, "subject")
      if (length(other) > 0) {
        showNotification(
          paste0("No column name recognised for: ",
                 paste(c(time = "Time", conc = "Concentration")[other], collapse = ", "),
                 ". The pre-selected columns are a guess; check the mapping."),
          type = "warning", duration = 15)
      }
    })
    
    # Process data
    observeEvent(input$btn_apply, {
      req(raw_data())
      
      col_map <- list(
        subject = input$col_subject,
        time    = input$col_time,
        conc    = input$col_conc
      )
      # A dropdown can still hold a column of the previous file (a selectize
      # input keeps its value when the new choices have no match), so only
      # columns of this file are accepted
      in_file <- function(v) length(v) == 1 && nzchar(v) && v %in% names(raw_data())
      col_map <- col_map[vapply(col_map, in_file, logical(1))]
      if (in_file(input$col_treatment)) col_map$treatment <- input$col_treatment
      if (in_file(input$col_period))    col_map$period    <- input$col_period
      if (in_file(input$col_sequence))  col_map$sequence  <- input$col_sequence
      if (in_file(input$col_dose))      col_map$dose      <- input$col_dose
      
      # Validate
      val <- validate_mapping(col_map)
      if (!val$valid) {
        showNotification(val$message, type = "error", duration = 5)
        return()
      }
      
      if (is.null(input$lloq) || is.na(input$lloq) || input$lloq < 0) {
        showNotification("Enter an LLOQ value (0 if no BLQ handling is needed), then click Process Data again.",
                         type = "error", duration = 8)
        return()
      }

      # Quality check
      is_adnca <- identical(input$data_type, "adnca")
      qc <- run_data_quality_check(raw_data(), col_map, lloq = input$lloq,
                                   dec = if (is_adnca || is.null(read_args()$dec)) "." else read_args()$dec)
      shared$qc_result <- qc
      
      # Auto-detect LLOQ from BLQ text entries if not set
      blq_txt <- blq_text_summary(raw_data()[[col_map$conc]])
      n_blq_text <- blq_txt$n_blq_text

      if (input$lloq <= 0 && n_blq_text > 0) {
        detected_lloq <- blq_txt$suggested_lloq
        # Store suggestion so the "Apply and continue" button can use it
        lloq_suggestion(detected_lloq)
        # Hard stop: BLQ text present but no LLOQ — show persistent apply button
        showNotification(
          paste0(n_blq_text, " BLQ entr", if (n_blq_text == 1) "y" else "ies",
                 " detected but LLOQ is not set. ",
                 if (!is.null(detected_lloq))
                   paste0("LLOQ auto-detected as ", detected_lloq,
                          ". Use the button below the LLOQ field to apply and process.")
                 else
                   "Set an LLOQ value above 0 and click Process Data again."),
          type = "error", duration = 12)
        return()
      }
      lloq_suggestion(NULL)  # clear suggestion once LLOQ is properly set
      
      if (!qc$pass) {
        showNotification(
          paste0(qc$n_errors, " error(s) found. Fix them before proceeding."),
          type = "error", duration = 8)
        return()
      }
      
      # Process: one Shiny-free implementation (R/pipeline.R), shared with the
      # validation suite and the reproduction script
      ds <- prepare_pk_dataset(raw_data(), col_map, list(
        lloq = input$lloq, blq_rule = input$blq_rule, door = if (is_adnca) "adnca" else "flat",
        file_name = input$file_upload$name, file_path = input$file_upload$datapath,
        read_args = if (is_adnca) list() else read_args(),
        pipeline_sha256 = PIPELINE_SHA256, qc = qc,
        interlocks = run_interlocks(raw_data(), col_map)))
      data   <- ds$data
      design <- ds$design
      
      shared$raw_data   <- raw_data()
      shared$pk_dataset <- ds
      shared$pk_data    <- data
      shared$col_map    <- col_map
      shared$data_ready <- TRUE
      # Results belong to the data they were computed from; Plan a Study must not
      # offer a CV from the previous file
      shared$nca_results  <- NULL
      shared$nca_settings <- NULL
      shared$be_results   <- NULL
      shared$study_info <- list(
        design    = design,
        lloq      = input$lloq,
        blq_rule  = input$blq_rule,
        file_name = input$file_upload$name,
        file_path = input$file_upload$datapath,
        read_args = read_args(),
        door      = if (is_adnca) "adnca" else "flat",
        # ADNCA import: the choices, notes and sources, for the Analysis Record
        adnca     = if (is_adnca) adnca_conv()[c("options", "notes", "sources", "lloq", "n_records")] else NULL
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

