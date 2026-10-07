# Golden outputs of the bioequivalence run (Part A of the v1.9 plan).
#
# Drives the real path_be_server() with shiny::testServer(), the way a user
# would (Process Data, then Run), and saves what the app stored after the run:
# be_result(), be_run_settings() and the notifications shown. The refactor that
# moves the run into run_be_analysis() must reproduce these files exactly
# (identical()). Never regenerate them to make a test pass.
#
# Run from the project root:
#   Rscript validation/fixtures/make_be_run_golden.R          # write the files
#   Rscript validation/fixtures/make_be_run_golden.R check    # compare, write nothing
#
# be_golden_cases() and be_golden_run() are also used by the validation suite.

be_golden_env <- function() {
  suppressPackageStartupMessages({
    library(shiny); library(bslib); library(NonCompart); library(PowerTOST)
    library(plotly); library(DT); library(dplyr); library(tidyr); library(ggplot2)
    library(shinyWidgets); library(htmltools); library(openxlsx); library(nlme)
  })
  for (f in c("R/pipeline.R", "R/utils.R", "R/cdisc_terms.R", "R/nca_helpers.R", "R/interlocks.R",
              "R/data_quality.R", "R/designs.R", "R/be_analysis.R", "R/be_scaled.R", "R/help_system.R",
              "R/mod_partial_auc.R", "R/mod_lz_rules.R", "R/mod_exclusions.R", "R/gxp_audit.R",
              "R/export_record.R", "R/mod_path_be.R"))
    source(f, local = FALSE)
  # The record helpers read these two from the global environment (app.R sets them)
  if (!exists("APP_VERSION", envir = globalenv())) assign("APP_VERSION", "golden", envir = globalenv())
  if (!exists("PIPELINE_SHA256", envir = globalenv()))
    assign("PIPELINE_SHA256", digest::digest(file = "R/pipeline.R", algo = "sha256"), envir = globalenv())
  invisible(TRUE)
}

# The settings a user leaves at their defaults; a case overrides what it changes
be_golden_defaults <- list(
  admin_route = "extravascular", dose_source = "single", dose = 100,
  dose_unit = "mg", time_unit = "h", conc_unit = "ng/mL", mw = 0, trap_method = "log",
  r2adj_be = 0.7, is_ss = FALSE, tau = NA, ctau_window = NA,
  be_design = "2x2x2", model_type = "fixed", be_params = c("CMAX", "AUCLST"),
  log_transform = TRUE, ci_level = 90, be_lower = 80, be_upper = 125,
  widened_scope = "cmax", pe_constraint = TRUE, be_reference = "Reference"
)

be_golden_cases <- function() list(
  crossover_fixed  = list(file = "data/example_be_crossover.csv", inputs = list()),
  crossover_mixed  = list(file = "data/example_be_crossover.csv", inputs = list(model_type = "mixed")),
  parallel_cov     = list(file = "data/example_be_parallel_covariates.csv",
                          inputs = list(be_design = "parallel", be_covariates = "Weight")),
  hvd_abel         = list(file = "data/example_be_replicate_hvd.csv",
                          inputs = list(be_design = "2x2x4", be_approach = "abel", be_params = c("CMAX", "AUCLST"))),
  hvd_rsabe        = list(file = "data/example_be_replicate_hvd.csv",
                          inputs = list(be_design = "2x2x4", be_approach = "rsabe", be_params = c("CMAX", "AUCLST"))),
  replicate_excl   = list(file = "data/example_be_replicate_2x2x4.csv",
                          inputs = list(be_design = "2x2x4"), exclude_first_profile = TRUE),
  tmax_note        = list(file = "data/example_be_crossover.csv",
                          inputs = list(be_params = c("CMAX", "AUCLST", "TMAX"))),
  steady_state     = list(file = "data/example_be_crossover.csv",
                          inputs = list(is_ss = TRUE, tau = 24, be_params = c("CMAX", "AUCLST"))),
  partial_auc      = list(file = "data/example_be_crossover.csv",
                          inputs = list(be_params = c("CMAX", "AUCLST", "AUC_0_4")),
                          partial_aucs = data.frame(start = 0, end = 4, cmax = FALSE, role = "supportive",
                                                    stringsAsFactors = FALSE))
)

# Process Data as the upload module does it, without the interface
be_golden_shared <- function(file, exclusions = NULL) {
  raw <- read_pk_file(file, list())
  cm <- auto_detect_columns(names(raw))
  cm <- cm[!vapply(cm, function(v) is.null(v) || identical(v, ""), logical(1))]
  qc <- tryCatch(run_data_quality(raw, cm, lloq = 0), error = function(e) NULL)
  ds <- prepare_pk_dataset(raw, cm, list(lloq = 0, blq_rule = "rule1", door = "flat", file_name = basename(file),
                                         file_path = file, read_args = list(), qc = qc,
                                         interlocks = run_interlocks(raw, cm), exclusions = exclusions))
  shiny::reactiveValues(
    raw_data = raw, pk_data = ds$data, col_map = cm, study_info = list(design = ds$design, lloq = 0, blq_rule = "rule1",
      file_name = basename(file), file_path = file, source = "example", read_args = list(), door = "flat",
      units = units_in_data(raw), adnca = NULL),
    data_ready = TRUE, nca_results = NULL, nca_settings = NULL, partial_aucs = NULL, be_results = NULL,
    viz_settings = NULL, exclusions = exclusions, prepare_opts = NULL, data_id = 1, lz_rules = NULL,
    exclusion_request = NULL)
}

#' The exclusion register of a case: the first profile of the file, or none
be_golden_exclusions <- function(case) {
  if (!isTRUE(case$exclude_first_profile)) return(NULL)
  raw <- read_pk_file(case$file, list())
  as_exclusions(data.frame(id = "ex1", level = "profile", subject = as.character(raw[[1]][1]),
                           treatment = as.character(raw$Treatment[1]), period = as.character(raw$Period[1]),
                           time = NA_real_, category = "protocol deviation", detail = "golden case",
                           protocol_section = "", after_be = FALSE, created_utc = "2026-01-01 00:00:00",
                           created_by = "golden", stringsAsFactors = FALSE))
}

#' Run one case through the real module; returns what the app stored
be_golden_run <- function(case) {
  excl <- be_golden_exclusions(case)
  shared <- be_golden_shared(case$file, excl)
  notes <- list()
  assign("showNotification", function(ui, ..., type = "default", duration = 5, id = NULL)
    notes[[length(notes) + 1]] <<- list(text = paste(as.character(ui), collapse = " "), type = type, duration = duration),
    envir = globalenv())
  on.exit(rm("showNotification", envir = globalenv()), add = TRUE)
  out <- NULL
  suppressWarnings(shiny::testServer(path_be_server, args = list(shared = shared), {
    inp <- utils::modifyList(be_golden_defaults, case$inputs)
    do.call(session$setInputs, inp)
    if (!is.null(case$partial_aucs)) {
      p <- case$partial_aucs
      session$setInputs(`pauc-n` = nrow(p))
      for (i in seq_len(nrow(p))) {
        do.call(session$setInputs, stats::setNames(list(p$start[i], p$end[i], p$cmax[i], p$role[i]),
                paste0("pauc-", c("start", "end", "cmax", "role"), i)))
      }
    }
    session$setInputs(run_be = 1)
    out <<- list(be_result = be_result(), be_run_settings = be_run_settings(), notifications = notes)
  }))
  out
}

#' Run one case, click Run and download the real Analysis Record into `dir`
#' @return the folder with the unzipped record
be_golden_record <- function(case, dir) {
  shared <- be_golden_shared(case$file, be_golden_exclusions(case))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  suppressWarnings(shiny::testServer(path_be_server, args = list(shared = shared), {
    do.call(session$setInputs, utils::modifyList(be_golden_defaults, case$inputs))
    if (!is.null(case$partial_aucs)) {
      p <- case$partial_aucs
      session$setInputs(`pauc-n` = nrow(p))
      for (i in seq_len(nrow(p)))
        do.call(session$setInputs, stats::setNames(list(p$start[i], p$end[i], p$cmax[i], p$role[i]),
                paste0("pauc-", c("start", "end", "cmax", "role"), i)))
    }
    session$setInputs(run_be = 1, record_study = "GLD", record_analyst = "QA")
    utils::unzip(output$dl_record, exdir = dir)
  }))
  dir
}

if (sys.nframe() == 0) {
  be_golden_env()
  check <- identical(commandArgs(TRUE)[1], "check")
  dir <- "validation/fixtures/be_run_golden"
  if (!check) dir.create(dir, showWarnings = FALSE)
  bad <- character(0)
  for (nm in names(be_golden_cases())) {
    r <- be_golden_run(be_golden_cases()[[nm]])
    if (is.null(r$be_result)) { cat(sprintf("%-16s NO RESULT (notifications: %s)\n", nm,
                                            paste(vapply(r$notifications, `[[`, "", "text"), collapse = " | "))); bad <- c(bad, nm); next }
    f <- file.path(dir, paste0(nm, ".rds"))
    if (check) {
      same <- identical(readRDS(f), r)
      cat(sprintf("%-16s %s\n", nm, if (same) "identical" else "DIFFERENT")); if (!same) bad <- c(bad, nm)
    } else {
      saveRDS(r, f, version = 2)
      cat(sprintf("%-16s written: %d parameter row(s), %d notification(s)\n", nm, nrow(r$be_result$ci_table), length(r$notifications)))
    }
  }
  if (length(bad)) quit(status = 1)
}
