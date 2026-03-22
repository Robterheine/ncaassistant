# ============================================================================
# Non-Compartmental Analysis Assistant v0.1 (test release)
# ============================================================================
# Radboud Applied Pharmacometrics — Radboudumc, Nijmegen
# https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics
#
# 5 workflow paths:
#   1. Plan a Study (Power & Sample Size)
#   2. Upload & Check Data
#   3. Analyze One Subject at a Time (Single-Subject NCA)
#   4. Analyze All Subjects (Batch NCA)
#   5. Bioequivalence Testing
# ============================================================================

APP_VERSION <- "0.1 (test release)"
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
source("R/help_system.R")
source("R/mod_data_upload.R")
source("R/mod_path_power.R")
source("R/mod_path_data.R")
source("R/mod_path_single_nca.R")
source("R/mod_path_multi_nca.R")
source("R/mod_path_be.R")
source("R/mod_data_guide.R")

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
          onclick = "Shiny.setInputValue('nav_path', 'guide', {priority: 'event'}); return false;",
          class = "btn btn-outline-warning btn-sm ms-3",
          style = "font-size: 0.7rem; padding: 2px 8px;",
          icon("book-open", class = "me-1"), "Data Preparation Guide"
        ),
        tags$a(
          href = "#",
          onclick = "Shiny.setInputValue('nav_path', 'about', {priority: 'event'}); return false;",
          class = "btn btn-outline-light btn-sm ms-2",
          style = "font-size: 0.7rem; padding: 2px 8px;",
          icon("circle-info", class = "me-1"), "About"
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
    be_results   = NULL
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
      single_nca = "Analyze One Subject at a Time",
      multi_nca  = "Analyze All Subjects",
      be         = "Bioequivalence Testing",
      about      = "About & R Packages",
      guide      = "Data Preparation Guide"
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
      "single_nca" = path_single_nca_ui("path_single_nca"),
      "multi_nca"  = path_multi_nca_ui("path_multi_nca"),
      "be"         = path_be_ui("path_be"),
      "about"      = about_ui(),
      "guide"      = data_guide_ui(),
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
               "Pharmacokinetic analysis made accessible. ",
               "Choose what you'd like to do."),
        tags$p(class = "text-white-50 mb-0", style = "font-size: 0.8rem;",
               "Radboud Applied Pharmacometrics — Radboudumc, Nijmegen",
               tags$span(class = "ms-2", paste0("v", APP_VERSION)))
      ),
      
      # Pathway cards
      tags$div(
        class = "row g-3 mt-2",
        
        # --- Card 1: Power & Sample Size ---
        tags$div(
          class = "col-md-6",
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
                  style = "width: 56px; height: 56px;",
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
          class = "col-md-6",
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
                  style = "width: 56px; height: 56px;",
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
        
        # --- Card 3: Single Subject NCA ---
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
                tags$h5(class = "fw-bold mb-0", "3. One Subject at a Time")
              ),
              tags$p(class = "text-muted mb-2",
                     "Inspect and analyze PK profiles subject by subject. ",
                     "Ideal for dose-escalation studies and quality review."),
              tags$p(class = "small text-muted mb-0",
                     "Interactive half-life adjustment for each profile.")
            )
          )
        ),
        
        # --- Card 4: Batch NCA ---
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
                tags$h5(class = "fw-bold mb-0", "4. All Subjects (Batch)")
              ),
              tags$p(class = "text-muted mb-2",
                     "Run PK analysis on all subjects at once. ",
                     "Get summary statistics, parameter tables, and export results."),
              tags$p(class = "small text-muted mb-0",
                     "Includes population summary and individual review.")
            )
          )
        ),
        
        # --- Card 5: Bioequivalence ---
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
                tags$h5(class = "fw-bold mb-0", "5. Bioequivalence")
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
          "New to PK analysis? Start with ",
          tags$strong("Upload & Check Data"),
          ", then go to ",
          tags$strong("All Subjects"),
          " or ",
          tags$strong("One Subject at a Time"),
          "."
        ),
        tags$p(
          "Look for the ",
          icon("circle-info", class = "text-info"),
          " buttons throughout the app — they explain every setting in plain language."
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
            "This application was developed by the ",
            tags$a(href = "https://www.radboudumc.nl/en/research/research-groups/radboud-applied-pharmacometrics",
                   target = "_blank",
                   "Radboud Applied Pharmacometrics"),
            " research group at Radboudumc, Nijmegen, The Netherlands."
          ),
          tags$p("The application and its source code are provided under the ",
                 tags$strong("GPL-3 license"), ". All R packages used retain ",
                 "their original licenses."),
          tags$p(class = "text-muted small",
                 "This is version ", APP_VERSION, ". The software is provided ",
                 "as-is for research and educational purposes. Results should be ",
                 "independently verified before use in regulatory submissions.")
        )
      )
    )
  }
  
  # === MODULE SERVERS ========================================================
  path_power_server("path_power", shared)
  path_data_server("path_data", shared)
  path_single_nca_server("path_single_nca", shared)
  path_multi_nca_server("path_multi_nca", shared)
  path_be_server("path_be", shared)
}

shinyApp(ui = ui, server = server)
