# ============================================================================
# Non-Compartmental Analysis Assistant v1.2
# ============================================================================
# Radboud Applied Pharmacometrics — Radboudumc, Nijmegen
# Designed by Rob ter Heine
# https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics
#
# 6 workflow paths:
#   1. Plan a Study (Power & Sample Size)
#   2. Upload & Check Data
#   3. Visualize Data (NEW in v1.2)
#   4. Analyze One Subject at a Time (Single-Subject NCA)
#   5. Analyze All Subjects (Batch NCA)
#   6. Bioequivalence Testing
# ============================================================================

APP_VERSION <- "1.2"
APP_NAME    <- "Non-Compartmental Analysis Assistant"

library(shiny)
library(bslib)
library(NonCompart)
library(ncar)
library(PowerTOST)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(htmltools)
library(openxlsx)
library(nlme)

# --- Source all modules ------------------------------------------------------
source("R/utils.R")
source("R/nca_helpers.R")
source("R/data_quality.R")
source("R/export_record.R")
source("R/help_system.R")
source("R/mod_data_upload.R")
source("R/mod_path_power.R")
source("R/mod_path_data.R")
source("R/mod_path_single_nca.R")
source("R/mod_path_multi_nca.R")
source("R/mod_path_be.R")
source("R/mod_path_viz.R")
source("R/mod_data_guide.R")
source("R/mod_methods.R")

# --- Theme -------------------------------------------------------------------
pharma_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#2C3E50",
  secondary  = "#95A5A6",
  success    = "#18BC9C",
  info       = "#3498DB",
  warning    = "#F39C12",
  danger     = "#E74C3C",
  base_font  = font_google("Source Sans Pro"),
  heading_font = font_google("Source Sans Pro"),
  code_font  = font_google("Fira Code"),
  "navbar-bg" = "#2C3E50",
  font_scale = 0.95
)

# --- UI ----------------------------------------------------------------------
ui <- page_fluid(
  theme = pharma_theme,
  
  # Global header
  tags$nav(
    class = "navbar navbar-dark bg-dark px-3 py-2 mb-0",
    tags$div(
      class = "container-fluid",
      tags$a(
        class = "navbar-brand d-flex align-items-center",
        href = "#", onclick = "Shiny.setInputValue('nav_path', 'home', {priority: 'event'}); return false;",
        tags$img(src = "logo.svg", height = "26px", class = "me-2"),
        tags$span("NCA Assistant", style = "font-weight: 700;")
      ),
      tags$div(
        class = "d-flex align-items-center",
        uiOutput("global_data_badge"),
        tags$a(
          href = "#",
          onclick = "Shiny.setInputValue('nav_path', 'methods', {priority: 'event'}); return false;",
          class = "btn btn-info btn-sm ms-3",
          style = "font-size: 0.7rem; padding: 2px 8px; color: #fff;",
          icon("flask-vial", class = "me-1"), "Statistical Methods"
        ),
        tags$a(
          href = "#",
          onclick = "Shiny.setInputValue('nav_path', 'guide', {priority: 'event'}); return false;",
          class = "btn btn-outline-warning btn-sm ms-2",
          style = "font-size: 0.7rem; padding: 2px 8px;",
          icon("book-open", class = "me-1"), "Data Guide"
        ),
        tags$a(
          href = "#",
          onclick = "Shiny.setInputValue('nav_path', 'about', {priority: 'event'}); return false;",
          class = "btn btn-outline-light btn-sm ms-2",
          style = "font-size: 0.7rem; padding: 2px 8px;",
          icon("circle-info", class = "me-1"), "About"
        ),
        tags$a(
          href = "NCA_Assistant_User_Manual_v1.4.pdf",
          target = "_blank",
          class = "btn btn-outline-success btn-sm ms-2",
          style = "font-size: 0.7rem; padding: 2px 8px;",
          icon("file-pdf", class = "me-1"), "User Manual"
        ),
        tags$span(class = "text-muted ms-2", style = "font-size: 0.7rem;",
                  paste0("v", APP_VERSION))
      )
    )
  ),
  
  # Back-to-home breadcrumb (hidden on home page)
  uiOutput("breadcrumb"),
  
  # Popover initialization
  help_init_js(),
  
  # Main content area — switches between paths
  uiOutput("main_content")
)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # === GLOBAL STATE ==========================================================
  # Shared across all paths
  shared <- reactiveValues(
    # Navigation
    current_path = "home",
    
    # Data (shared gateway)
    raw_data     = NULL,
    pk_data      = NULL,
    col_map      = NULL,
    study_info   = NULL,
    qc_result    = NULL,
    data_ready   = FALSE,
    
    # NCA results (from batch or single-subject)
    nca_results  = NULL,
    nca_settings = NULL,
    
    # BE results
    be_results   = NULL,
    
    # Visualization settings (written by viz module, read by export_record)
    viz_settings = NULL
  )
  
  # === NAVIGATION ============================================================
  observeEvent(input$nav_path, {
    shared$current_path <- input$nav_path
  })
  
  # Breadcrumb / back button
  output$breadcrumb <- renderUI({
    if (shared$current_path == "home") return(NULL)
    
    path_names <- c(
      power      = "Plan a Study (Power & Sample Size)",
      data       = "Upload & Check Data",
      viz        = "Visualize Data",
      single_nca = "Analyze One Subject at a Time",
      multi_nca  = "Analyze All Subjects",
      be         = "Bioequivalence Testing",
      about      = "About & R Packages",
      guide      = "Data Preparation Guide",
      methods    = "Statistical Methods"
    )
    
    tags$div(
      class = "bg-light border-bottom px-3 py-2 d-flex align-items-center",
      tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('nav_path', 'home', {priority: 'event'}); return false;",
        class = "text-decoration-none me-2",
        icon("house"), " Home"
      ),
      tags$span(class = "text-muted mx-2", "/"),
      tags$span(class = "fw-semibold",
                path_names[shared$current_path])
    )
  })
  
  # Global data badge
  output$global_data_badge <- renderUI({
    if (!shared$data_ready) return(NULL)
    
    n_subj <- shared$study_info$design$n_subjects
    n_obs  <- nrow(shared$pk_data)
    
    tags$span(
      class = "badge bg-success ms-2",
      style = "font-size: 0.75rem;",
      icon("database", class = "me-1"),
      paste0(n_subj, " subjects, ", n_obs, " obs")
    )
  })
  
  # === MAIN CONTENT ROUTER ===================================================
  output$main_content <- renderUI({
    switch(shared$current_path,
      "home"       = hub_ui(),
      "power"      = path_power_ui("path_power"),
      "data"       = path_data_ui("path_data"),
      "viz"        = path_viz_ui("path_viz"),
      "single_nca" = path_single_nca_ui("path_single_nca"),
      "multi_nca"  = path_multi_nca_ui("path_multi_nca"),
      "be"         = path_be_ui("path_be"),
      "about"      = about_ui(),
      "guide"      = data_guide_ui(),
      "methods"    = methods_ui(),
      hub_ui()
    )
  })
  
  # === HUB (HOME) UI =========================================================
  hub_ui <- function() {
    tags$div(
      class = "container-fluid py-4",
      style = "max-width: 1100px; margin: 0 auto;",
      
      # Welcome header
      tags$div(
        class = "text-center mb-4",
        style = "background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%); border-radius: 12px; padding: 2.5rem;",
        tags$h2(class = "text-white fw-bold mb-2",
                "Non-Compartmental Analysis Assistant"),
        tags$p(class = "text-white opacity-75 mb-1",
               "Pharmacokinetic analysis made accessible."),
        tags$p(class = "text-white-50 mb-0", style = "font-size: 0.8rem;",
               "Rob ter Heine, Radboud Applied Pharmacometrics")
      ),
      
      # ---- Row 1: Plan | Upload | Visualize --------------------------------
      tags$div(
        class = "row g-3 mt-2",
        
        # --- Card 1: Power & Sample Size ---
        tags$div(
          class = "col-md-4",
          tags$div(
            class = "card h-100 border-0 shadow-sm hub-card",
            style = "cursor: pointer; transition: transform 0.15s;",
            onclick = "Shiny.setInputValue('nav_path', 'power', {priority: 'event'});",
            tags$div(
              class = "card-body p-4",
              tags$div(
                class = "d-flex align-items-center mb-3",
                tags$div(
                  class = "rounded-circle bg-info bg-opacity-10 d-flex align-items-center justify-content-center me-3",
                  style = "width: 52px; height: 52px;",
                  icon("calculator", class = "fa-lg text-info")
                ),
                tags$div(
                  tags$h5(class = "fw-bold mb-1", "1. Plan a Study"),
                  tags$span(class = "badge bg-info", "No data needed")
                )
              ),
              tags$p(class = "text-muted mb-2",
                     "Calculate how many subjects you need for a bioequivalence ",
                     "or pharmacokinetic study, and estimate the probability of ",
                     "success (power)."),
              tags$p(class = "small text-muted mb-0",
                     icon("check-circle", class = "text-success me-1"),
                     "Supports crossover, parallel, and replicate designs for all major regulatory agencies.")
            )
          )
        ),
        
        # --- Card 2: Upload & Check Data ---
        tags$div(
          class = "col-md-4",
          tags$div(
            class = "card h-100 border-0 shadow-sm hub-card",
            style = "cursor: pointer; transition: transform 0.15s;",
            onclick = "Shiny.setInputValue('nav_path', 'data', {priority: 'event'});",
            tags$div(
              class = "card-body p-4",
              tags$div(
                class = "d-flex align-items-center mb-3",
                tags$div(
                  class = "rounded-circle bg-success bg-opacity-10 d-flex align-items-center justify-content-center me-3",
                  style = "width: 52px; height: 52px;",
                  icon("file-import", class = "fa-lg text-success")
                ),
                tags$div(
                  tags$h5(class = "fw-bold mb-1", "2. Upload & Check Data"),
                  tags$span(class = "badge bg-success", "Start here if you have data")
                )
              ),
              tags$p(class = "text-muted mb-2",
                     "Upload your concentration-time data (CSV or Excel), ",
                     "map the columns, handle values below the detection limit, ",
                     "and run an automatic quality check."),
              tags$p(class = "small text-muted mb-0",
                     icon("check-circle", class = "text-success me-1"),
                     "Auto-detects column names, study design, and data problems.")
            )
          )
        ),
        
        # --- Card 3: Visualize Data (NEW v1.2) ---
        tags$div(
          class = "col-md-4",
          tags$div(
            class = "card h-100 border-0 shadow-sm hub-card viz-hub-card",
            style = "cursor: pointer; transition: transform 0.15s;",
            onclick = "Shiny.setInputValue('nav_path', 'viz', {priority: 'event'});",
            tags$div(
              class = "card-body p-4",
              tags$div(
                class = "d-flex align-items-center mb-3",
                tags$div(
                  class = "rounded-circle d-flex align-items-center justify-content-center me-3",
                  style = "width: 52px; height: 52px; background: rgba(142,68,173,0.1);",
                  icon("chart-line", class = "fa-lg", style = "color: #8E44AD;")
                ),
                tags$div(
                  tags$h5(class = "fw-bold mb-1", "3. Visualize Data"),
                  tags$span(class = "badge", style = "background: #8E44AD;", "Explore your data")
                )
              ),
              tags$p(class = "text-muted mb-2",
                     "Create publication-ready concentration-time plots: ",
                     "individual spaghetti plots and geometric mean summary curves ",
                     "with error bars."),
              tags$p(class = "small text-muted mb-0",
                     icon("check-circle", class = "text-success me-1"),
                     "Export to PNG, PDF, or SVG at journal-submission resolution.")
            )
          )
        )
      ),
      
      # ---- Row 2: Single NCA | Batch NCA | BE ------------------------------
      tags$div(
        class = "row g-3 mt-1",
        
        # --- Card 4: Single Subject NCA ---
        tags$div(
          class = "col-md-4",
          tags$div(
            class = "card h-100 border-0 shadow-sm hub-card",
            style = "cursor: pointer; transition: transform 0.15s;",
            onclick = "Shiny.setInputValue('nav_path', 'single_nca', {priority: 'event'});",
            tags$div(
              class = "card-body p-4",
              tags$div(
                class = "d-flex align-items-center mb-3",
                tags$div(
                  class = "rounded-circle bg-warning bg-opacity-10 d-flex align-items-center justify-content-center me-3",
                  style = "width: 48px; height: 48px;",
                  icon("user", class = "fa-lg text-warning")
                ),
                tags$h5(class = "fw-bold mb-0", "4. One Subject at a Time")
              ),
              tags$p(class = "text-muted mb-2",
                     "Inspect and analyze PK profiles subject by subject. ",
                     "Ideal for dose-escalation studies and quality review."),
              tags$p(class = "small text-muted mb-0",
                     "Interactive half-life adjustment for each profile.")
            )
          )
        ),
        
        # --- Card 5: Batch NCA ---
        tags$div(
          class = "col-md-4",
          tags$div(
            class = "card h-100 border-0 shadow-sm hub-card",
            style = "cursor: pointer; transition: transform 0.15s;",
            onclick = "Shiny.setInputValue('nav_path', 'multi_nca', {priority: 'event'});",
            tags$div(
              class = "card-body p-4",
              tags$div(
                class = "d-flex align-items-center mb-3",
                tags$div(
                  class = "rounded-circle bg-primary bg-opacity-10 d-flex align-items-center justify-content-center me-3",
                  style = "width: 48px; height: 48px;",
                  icon("users", class = "fa-lg text-primary")
                ),
                tags$h5(class = "fw-bold mb-0", "5. All Subjects (Batch)")
              ),
              tags$p(class = "text-muted mb-2",
                     "Run PK analysis on all subjects at once. ",
                     "Get summary statistics, parameter tables, and export results."),
              tags$p(class = "small text-muted mb-0",
                     "Includes population summary and individual review.")
            )
          )
        ),
        
        # --- Card 6: Bioequivalence ---
        tags$div(
          class = "col-md-4",
          tags$div(
            class = "card h-100 border-0 shadow-sm hub-card",
            style = "cursor: pointer; transition: transform 0.15s;",
            onclick = "Shiny.setInputValue('nav_path', 'be', {priority: 'event'});",
            tags$div(
              class = "card-body p-4",
              tags$div(
                class = "d-flex align-items-center mb-3",
                tags$div(
                  class = "rounded-circle bg-danger bg-opacity-10 d-flex align-items-center justify-content-center me-3",
                  style = "width: 48px; height: 48px;",
                  icon("arrows-left-right", class = "fa-lg text-danger")
                ),
                tags$h5(class = "fw-bold mb-0", "6. Bioequivalence")
              ),
              tags$p(class = "text-muted mb-2",
                     "Compare two formulations (Test vs. Reference). ",
                     "Runs NCA, computes 90% confidence intervals, and concludes equivalence."),
              tags$p(class = "small text-muted mb-0",
                     "Complete BE workflow from data to regulatory-ready results.")
            )
          )
        )
      ),
      
      # Quick help
      tags$div(
        class = "text-center mt-4 text-muted small",
        tags$p(
          icon("circle-info", class = "me-1"),
          "New to PK analysis? Check the ",
          tags$a(href = "#", onclick = "Shiny.setInputValue('nav_path', 'guide', {priority: 'event'}); return false;",
                 style = "text-decoration: underline; cursor: pointer;",
                 "Data Preparation Guide"),
          " first, then start with ",
          tags$strong("Upload & Check Data"),
          "."
        ),
        tags$p(
          "Look for the ",
          icon("circle-info", class = "text-info"),
          " buttons throughout the app \u2014 they explain every setting in plain language."
        )
      )
    )
  }
  
  # === ABOUT SCREEN ==========================================================
  about_ui <- function() {
    
    # Collect package versions dynamically
    pkg_info <- list(
      list(
        name    = "NonCompart",
        version = as.character(packageVersion("NonCompart")),
        role    = "Non-compartmental analysis engine. Calculates Cmax, AUC, half-life, clearance, and all standard NCA parameters. Validated against WinNonlin\u00AE.",
        url     = "https://cran.r-project.org/package=NonCompart",
        ref     = "Kim H et al. Transl Clin Pharmacol. 2018;26(1):10-15"
      ),
      list(
        name    = "ncar",
        version = as.character(packageVersion("ncar")),
        role    = "NCA report generation. Produces formatted PDF and RTF reports from NonCompart output. CDISC SDTM compatible.",
        url     = "https://cran.r-project.org/package=ncar",
        ref     = "Kim H et al. Transl Clin Pharmacol. 2018;26(1):10-15"
      ),
      list(
        name    = "PowerTOST",
        version = as.character(packageVersion("PowerTOST")),
        role    = "Power analysis and sample size estimation for bioequivalence studies. Supports ABE, ABEL, RSABE, NTID, dose-proportionality, and non-inferiority designs.",
        url     = "https://cran.r-project.org/package=PowerTOST",
        ref     = "Labes D, Sch\u00FCtz H, Lang B. PowerTOST: Power and Sample Size for (Bio)Equivalence Studies."
      ),
      list(
        name    = "nlme",
        version = as.character(packageVersion("nlme")),
        role    = "Linear and nonlinear mixed-effects models. Used for the mixed-effects ANOVA in bioequivalence analysis (FDA-recommended approach for crossover studies).",
        url     = "https://cran.r-project.org/package=nlme",
        ref     = "Pinheiro J, Bates D. Mixed-Effects Models in S and S-PLUS. Springer, 2000."
      ),
      list(
        name    = "shiny",
        version = as.character(packageVersion("shiny")),
        role    = "Web application framework. Provides the interactive user interface.",
        url     = "https://cran.r-project.org/package=shiny",
        ref     = "Chang W et al. shiny: Web Application Framework for R."
      ),
      list(
        name    = "bslib",
        version = as.character(packageVersion("bslib")),
        role    = "Bootstrap 5 theming for Shiny. Provides the modern visual design of the interface.",
        url     = "https://cran.r-project.org/package=bslib",
        ref     = "Sievert C, Cheng J. bslib: Custom Bootstrap Sass Themes for shiny and rmarkdown."
      ),
      list(
        name    = "plotly",
        version = as.character(packageVersion("plotly")),
        role    = "Interactive plots with zoom, hover, and pan. Used for all concentration-time profiles and forest plots.",
        url     = "https://cran.r-project.org/package=plotly",
        ref     = "Sievert C. Interactive Web-Based Data Visualization with R, plotly, and shiny. CRC Press, 2020."
      ),
      list(
        name    = "ggplot2",
        version = as.character(packageVersion("ggplot2")),
        role    = "Grammar of graphics plotting engine. Creates all static and publication-quality figures.",
        url     = "https://cran.r-project.org/package=ggplot2",
        ref     = "Wickham H. ggplot2: Elegant Graphics for Data Analysis. Springer, 2016."
      ),
      list(
        name    = "DT",
        version = as.character(packageVersion("DT")),
        role    = "Interactive data tables with sorting, filtering, and pagination.",
        url     = "https://cran.r-project.org/package=DT",
        ref     = "Xie Y et al. DT: A Wrapper of the JavaScript Library DataTables."
      ),
      list(
        name    = "readxl",
        version = as.character(packageVersion("readxl")),
        role    = "Reading Excel files (.xlsx, .xls) for data import.",
        url     = "https://cran.r-project.org/package=readxl",
        ref     = "Wickham H, Bryan J. readxl: Read Excel Files."
      ),
      list(
        name    = "openxlsx",
        version = as.character(packageVersion("openxlsx")),
        role    = "Writing Excel files for result export (multi-sheet workbooks).",
        url     = "https://cran.r-project.org/package=openxlsx",
        ref     = "Schauberger P, Walker A. openxlsx: Read, Write and Edit xlsx Files."
      ),
      list(
        name    = "dplyr",
        version = as.character(packageVersion("dplyr")),
        role    = "Data manipulation: filtering, grouping, summarizing concentration data.",
        url     = "https://cran.r-project.org/package=dplyr",
        ref     = "Wickham H et al. dplyr: A Grammar of Data Manipulation."
      ),
      list(
        name    = "tidyr",
        version = as.character(packageVersion("tidyr")),
        role    = "Data reshaping for summary statistics and plot preparation.",
        url     = "https://cran.r-project.org/package=tidyr",
        ref     = "Wickham H, Girlich M. tidyr: Tidy Messy Data."
      ),
      list(
        name    = "shinyWidgets",
        version = as.character(packageVersion("shinyWidgets")),
        role    = "Extended UI components for improved interactivity.",
        url     = "https://cran.r-project.org/package=shinyWidgets",
        ref     = "Perrier V, Meyer F, Granjon D. shinyWidgets: Custom Inputs Widgets for Shiny."
      )
    )
    
    # Build the package table rows
    pkg_rows <- lapply(pkg_info, function(pkg) {
      tags$tr(
        tags$td(tags$a(href = pkg$url, target = "_blank",
                       class = "fw-bold text-decoration-none",
                       pkg$name)),
        tags$td(tags$code(pkg$version)),
        tags$td(class = "small", pkg$role),
        tags$td(class = "small text-muted", pkg$ref)
      )
    })
    
    tags$div(
      class = "container-fluid py-4",
      style = "max-width: 1100px; margin: 0 auto;",
      
      # App info card
      card(
        card_body(
          class = "text-center py-4",
          style = "background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%); border-radius: 8px;",
          tags$h3(class = "text-white fw-bold", APP_NAME),
          tags$p(class = "text-white opacity-75 mb-1",
                 paste0("Version ", APP_VERSION)),
          tags$p(class = "text-white-50 small mb-2",
                 "A freely available, open-source tool for pharmacokinetic ",
                 "non-compartmental analysis, bioequivalence testing, and ",
                 "study planning."),
          tags$a(
            href = "https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics",
            target = "_blank",
            class = "btn btn-outline-light btn-sm",
            "Radboud Applied Pharmacometrics — Radboudumc, Nijmegen"
          )
        )
      ),
      
      # R packages table
      card(
        card_header(
          icon("box-open"), " R Packages Used in This Application"
        ),
        card_body(
          tags$p(class = "text-muted small",
                 "This application is built entirely in R. All statistical computations ",
                 "are performed by the packages listed below. Click a package name to ",
                 "visit its CRAN page."),
          
          tags$table(
            class = "table table-sm table-hover",
            tags$thead(
              class = "table-light",
              tags$tr(
                tags$th(style = "width: 12%;", "Package"),
                tags$th(style = "width: 8%;", "Version"),
                tags$th(style = "width: 45%;", "Role in this application"),
                tags$th(style = "width: 35%;", "Reference")
              )
            ),
            tags$tbody(pkg_rows)
          )
        )
      ),
      
      # Technical details
      card(
        card_header(icon("microchip"), " Runtime Environment"),
        card_body(
          tags$table(
            class = "table table-sm table-borderless small",
            tags$tr(tags$td(class = "fw-bold", "R version:"),
                    tags$td(R.version.string)),
            tags$tr(tags$td(class = "fw-bold", "Platform:"),
                    tags$td(R.version$platform)),
            tags$tr(tags$td(class = "fw-bold", "OS:"),
                    tags$td(sessionInfo()$running)),
            tags$tr(tags$td(class = "fw-bold", "NCA engine:"),
                    tags$td("NonCompart (validated against WinNonlin\u00AE, ",
                            "Kim et al. 2018)")),
            tags$tr(tags$td(class = "fw-bold", "BE engine:"),
                    tags$td("Base R lm() / nlme::lme() with ANOVA-based ",
                            "90% confidence intervals")),
            tags$tr(tags$td(class = "fw-bold", "Power engine:"),
                    tags$td("PowerTOST (exact method via Owen's Q)"))
          )
        )
      ),
      
      # License & credits
      card(
        card_header(icon("scale-balanced"), " License & Credits"),
        card_body(
          tags$p(
            "This application was designed and developed by ",
            tags$strong("Rob ter Heine"),
            " (hospital pharmacist-clinical pharmacologist, Radboudumc) and the ",
            tags$a(href = "https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics",
                   target = "_blank",
                   "Radboud Applied Pharmacometrics"),
            " research group at Radboudumc, Nijmegen, The Netherlands."
          ),
          tags$p("The application and its source code are provided under the ",
                 tags$strong("GPL-3 license"), ". All R packages used retain ",
                 "their original licenses. Source code and documentation are available on ",
                 tags$a(href = "https://github.com/robterheine/ncaassistant",
                        target = "_blank", "GitHub"), "."),
          tags$p(class = "text-muted small",
                 "This is version ", APP_VERSION, ". The software is provided ",
                 "as-is for research and educational purposes. Results should be ",
                 "independently verified before use in regulatory submissions.")
        )
      ),
      
      # Version history
      card(
        card_header(icon("clock-rotate-left"), " Version History"),
        card_body(
          class = "small",
          
          tags$div(
            class = "border-start border-3 border-primary ps-3 mb-3",
            tags$h6(class = "fw-bold mb-1", "v1.2",
                    tags$span(class = "badge bg-primary ms-2", "current")),
            tags$p(class = "text-muted mb-1", "April 2026"),
            tags$ul(class = "mb-0",
              tags$li("New workflow path: Visualize Data (path 3) \u2014 publication-ready concentration-time plots"),
              tags$li("Spaghetti plot: individual PK profiles with flexible colour-by options (Subject, Treatment, Period, Sequence)"),
              tags$li("Summary plot: geometric mean \u00b1 geometric CV% with optional arithmetic mean \u00b1 SD; treatment overlay for crossover/BE data"),
              tags$li("Export to PNG, PDF, or SVG with user-defined dimensions (2\u201320 inches) and resolution (150/300/600 DPI)"),
              tags$li("Log\u2081\u2080 Y-axis toggle on all plots; zero/BLQ observations excluded from geometric mean with visible count note"),
              tags$li("Dose-normalized display (C/Dose) when a Dose column is mapped"),
              tags$li("Three colour palettes: default (app palette), Okabe-Ito colorblind-safe, grayscale"),
              tags$li("Visualization settings appended to Complete Analysis Record JSON when downloaded in the same session"),
              tags$li("Hub restructured from 2+3 to 3+3 card grid; workflow paths renumbered 1\u20136"),
              tags$li("App designed by Rob ter Heine, Radboud Applied Pharmacometrics"),
              tags$li("Version bump to 1.2; validation suite updated with URS-VIZ-01 through URS-VIZ-08 (supportive class)"),
              tags$li("Bug fixes: BLQ Rule 5 crash when all concentrations are NA (which.max guard); LLOQ hard-stop when BLQ text present and LLOQ = 0; degenerate profile filter in run_nca (<3 positive values excluded with persistent amber note); R\u00B2adj division-by-zero in 2-point lambda-z; BLQ Rule 6 label corrected to \u2018before first quantifiable\u2019 throughout"),
              tags$li("Export fixes: composite key column names wrapped in deparse() in generated R script; zip archive no longer calls setwd() globally (system2 primary, withr::with_dir fallback); schema_version field added to analysis_settings.json"),
              tags$li("Lambda-z refactor: shared recalculate_lambda_z() helper in nca_helpers.R; all three modules (single NCA, batch NCA, BE) use identical regression, R\u00B2adj, and validation logic; R\u00B2 threshold slider now wired to all estimate_lambda_z() calls in the half-life review inspector"),
              tags$li("Data quality: large sampling gap warning (>24 h within a profile, crossover-aware); time gap check per subject and per treatment period"),
              tags$li("Upload: one-click \u2018Apply LLOQ = X and process\u2019 button when LLOQ is auto-detected from BLQ text entries; reformulate() replaces as.formula(paste()) for safe column name handling"),
              tags$li("Batch NCA: AUCPEO >20% flagged in amber in parameter table; excluded profiles displayed as persistent amber note above results tabs"),
              tags$li("Bioequivalence: steady-state checkbox added (AUC\u03C4 as primary parameter); balanced design pre-check with persistent amber warning; design summary card after NCA run; warnings from run_nca surfaced via withCallingHandlers"),
              tags$li("Plan a Study: 3-step guided flow; CV, GMR, and power entered as percentages; valid design list prevents invalid combinations; NTID locked to 4-period replicate"),
              tags$li("UI: User Manual PDF downloadable from navigation bar; PharmaKinEx references removed from codebase")
            )
          ),
          
          tags$div(
            class = "border-start border-3 border-secondary ps-3 mb-3",
            tags$h6(class = "fw-bold mb-1", "v1.1"),
            tags$p(class = "text-muted mb-1", "April 2026"),
            tags$ul(class = "mb-0",
              tags$li("Bioequivalence module: configurable R\u00B2 threshold for half-life estimation (was hardcoded to 0.70)"),
              tags$li("Bioequivalence module: half-life review with interactive point selection, \u03BBz plot, and recalculation"),
              tags$li("Bioequivalence module: individual profiles tab (per-subject panels with Test vs Reference overlay, paginated)"),
              tags$li("Lambda-z override audit trail in Complete Analysis Record (both batch NCA and BE): overrides logged in settings JSON, applied in reproducibility script, and displayed in HTML summary"),
              tags$li("Design mismatch detection: warns when single-sequence data is used with a multi-sequence crossover design"),
              tags$li("Defensive fixes: log-scale plot crash with zero concentrations, R\u00B2adj division by zero with 2 points, negative \u03BBz rejection, infusion duration validation"),
              tags$li("Forest plot rendering fix (overlapping axis labels)"),
              tags$li("Crossover-aware duplicate time check (no longer flags time = 0 across periods)"),
              tags$li("Three original test suites (249 tests) replaced by consolidated validation script (183 tests) aligned with IQ/OQ/PQ protocol"),
              tags$li("Draft validation package available on GitHub: URS, IQ/OQ/PQ protocol, automated validation script, package risk assessment per pharmaR framework")
            )
          ),
          
          tags$div(
            class = "border-start border-3 border-secondary ps-3 mb-3",
            tags$h6(class = "fw-bold mb-1", "v1.0 \u2014 First Stable Release"),
            tags$p(class = "text-muted mb-1", "April 2026"),
            tags$ul(class = "mb-0",
              tags$li("Complete Analysis Record export in all three analysis paths: self-contained zip with results, standalone R reproducibility script, data integrity hash (SHA-256), analysis settings (JSON), and HTML summary document"),
              tags$li("BLQ Rule 6 added (first BLQ \u2192 LLOQ/2, rest \u2192 zero) for drugs with absorption lag"),
              tags$li("Fixed-order crossover design (all subjects same sequence) with paired analysis"),
              tags$li("Steady-state display: AUC\u03C4 as primary parameter, AUC\u221E hidden, CL/F correctly derived from AUC\u03C4"),
              tags$li("Drug\u2013drug interaction studies: analysable via the BE path with customisable acceptance limits"),
              tags$li("Individual profiles grid converted to interactive plotly with pagination (12 subjects per page)"),
              tags$li("Example datasets (theophylline PK, crossover BE) downloadable from the Data Preparation Guide"),
              tags$li("Clear button for manual data entry in single-subject analysis"),
              tags$li("Data Guide discoverability: nudge on hub screen and in the upload tab"),
              tags$li("BLQ text entries with LLOQ=0 now block processing with auto-detection of LLOQ from '<X' entries"),
              tags$li("Duplicate time points upgraded from warning to error"),
              tags$li("Input validation: dose > 0 in all paths, CV/GMR/alpha guards in power module, treatment must have exactly 2 levels for BE"),
              tags$li("State reset on new file upload; memory cleanup (gc) after NCA"),
              tags$li("Platform-independent file handling (no hardcoded upload paths)"),
              tags$li("Data guide expanded: 10 scenario tabs covering all supported study designs"),
              tags$li("Expert-reviewed statistical methods page (medical writer, biostatistician, pharmacokineticist)"),
              tags$li("User manual v1.2 with theoretical background, statistical methods, DDI guidance, and regulatory references"),
              tags$li("249 automated tests across 3 validation suites")
            )
          ),
          
          tags$div(
            class = "border-start border-3 border-secondary ps-3 mb-3",
            tags$h6(class = "fw-bold mb-1", "v0.9 \u2014 Feature Complete"),
            tags$p(class = "text-muted mb-1", "March 2026"),
            tags$ul(class = "mb-0",
              tags$li("Built-in Data Preparation Guide with 8 scenario tabs and example datasets"),
              tags$li("BE parameter checkboxes with friendly names and pre-populated defaults"),
              tags$li("Improved half-life plot: clean tooltips, inline colour legend, no label overlap"),
              tags$li("Fixed startup crash caused by NULL input in module server evaluation"),
              tags$li("Fixed conditionalPanel double-namespacing across all modules"),
              tags$li("Fixed manual data entry using proper Shiny textAreaInput binding")
            )
          ),
          
          tags$div(
            class = "border-start border-3 border-secondary ps-3 mb-3",
            tags$h6(class = "fw-bold mb-1", "v0.5 \u2014 Hub Architecture"),
            tags$p(class = "text-muted mb-1", "March 2026"),
            tags$ul(class = "mb-0",
              tags$li("Redesigned from linear 6-tab layout to 5 independent workflow paths with central hub"),
              tags$li("Path 1: Plan a Study (Power & Sample Size) \u2014 standalone, no data needed"),
              tags$li("Path 2: Upload & Check Data \u2014 shared data gateway with 20+ quality checks"),
              tags$li("Path 3: Analyze One Subject at a Time \u2014 with manual data entry and Previous/Next navigation"),
              tags$li("Path 4: Analyze All Subjects (Batch NCA) \u2014 summary statistics, individual grid, half-life review"),
              tags$li("Path 5: Bioequivalence Testing \u2014 complete workflow from NCA to 90% CI to conclusion"),
              tags$li("About & Packages page with runtime environment and all R package references")
            )
          ),
          
          tags$div(
            class = "border-start border-3 border-secondary ps-3 mb-3",
            tags$h6(class = "fw-bold mb-1", "v0.1 \u2014 Initial Release"),
            tags$p(class = "text-muted mb-1", "March 2026"),
            tags$ul(class = "mb-0",
              tags$li("Non-compartmental analysis engine (NonCompart) validated against WinNonlin"),
              tags$li("5 BLQ handling rules (WinNonlin-compatible)"),
              tags$li("Bioequivalence ANOVA with fixed and mixed-effects models"),
              tags$li("PowerTOST integration for sample size and power"),
              tags$li("Crossover data handling with composite Subject||Treatment key"),
              tags$li("Contextual help system with 25+ plain-language popovers"),
              tags$li("153 automated validation tests")
            )
          )
        )
      )
    )
  }
  
  # === EXAMPLE DATASET DOWNLOADS ================================================
  output$dl_example_theoph <- downloadHandler(
    filename = function() "example_theoph.csv",
    content = function(file) file.copy("data/example_theoph.csv", file)
  )
  output$dl_example_be <- downloadHandler(
    filename = function() "example_be_crossover.csv",
    content = function(file) file.copy("data/example_be_crossover.csv", file)
  )
  
  # === MODULE SERVERS ========================================================
  path_power_server("path_power", shared)
  path_data_server("path_data", shared)
  path_single_nca_server("path_single_nca", shared)
  path_multi_nca_server("path_multi_nca", shared)
  path_be_server("path_be", shared)
  path_viz_server("path_viz", shared)
}

shinyApp(ui = ui, server = server)
