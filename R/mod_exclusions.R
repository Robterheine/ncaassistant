# ============================================================================
# NCA Assistant — Exclusions made by the analyst, with a reason
# ============================================================================
# One register (shared$exclusions, see as_exclusions() in R/pipeline.R), shown
# in Upload & Check Data and reachable from every analysis page. The analyst
# decides what is left out; the app never excludes on its own initiative.
# A sample exclusion treats the sample as never collected (removed before the
# BLQ rule). A profile exclusion keeps the NCA and leaves the profile out of
# summaries, mean curves and bioequivalence. Nothing is deleted: a restored
# exclusion stays in the register, and in controlled mode every change is
# written to the audit trail first (fail closed).

#' Reason categories per level. Excluding a concentration because it looks
#' like an outlier is not a reason ICH M13A or EMA accept, so there is none.
EXCLUSION_CATEGORIES <- list(
  sample  = c("Bioanalytical report (sample invalid)", "Sample handling", "Sampling-time deviation", "Other"),
  profile = c("Vomiting or diarrhoea", "Pre-dose concentration > 5% of Cmax (ICH M13A 2.2.3.3)",
              "Very low exposure: AUC < 5% of the geometric mean of the product (ICH M13A)",
              "Dosing deviation", "Adverse event or concomitant medication", "Protocol deviation", "Other"))

.nz <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else x

#' Categories whose detail field is required
EXCLUSION_DETAIL_REQUIRED <- c("Other", "Protocol deviation", "Dosing deviation",
                               "Adverse event or concomitant medication")

#' Short labels, e.g. "1 | Test | P1, t = 2" or "3 | Reference | P2 (whole profile)"
exclusion_labels <- function(ex) {
  ex <- as_exclusions(ex)
  if (nrow(ex) == 0) return(character(0))
  lab <- ex$subject
  has_t <- !is.na(ex$treatment) & nzchar(ex$treatment); lab[has_t] <- paste(lab[has_t], "|", ex$treatment[has_t])
  has_p <- !is.na(ex$period) & nzchar(ex$period); lab[has_p] <- paste0(lab[has_p], " | P", ex$period[has_p])
  ifelse(ex$level == "sample", paste0(lab, ", t = ", format(ex$time, trim = TRUE)), paste0(lab, " (whole profile)"))
}

#' Profile label (as profile_labels()) of each exclusion
exclusion_profile_labels <- function(ex) {
  ex <- as_exclusions(ex)
  if (nrow(ex) == 0) return(character(0))
  parts <- data.frame(Subject = ex$subject, stringsAsFactors = FALSE)
  if (any(!is.na(ex$treatment))) parts$Treatment <- ex$treatment
  if (any(!is.na(ex$period))) parts$Period <- ex$period
  profile_labels(parts)
}

#' Drop the manual half-life fits of profiles an exclusion changed
#' @return labels of the profiles whose fit was dropped
prune_overrides <- function(lz_state, exclusions) {
  labs <- unique(exclusion_profile_labels(active_exclusions(exclusions)))
  gone <- intersect(names(lz_state$overrides_log), labs)
  for (g in gone) { lz_state$overrides_log[[g]] <- NULL; lz_state$fits[[g]] <- NULL }
  if (length(gone) > 0) lz_state$override <- NULL
  gone
}

#' The data with every sample, as Process Data prepared them without exclusions
data_without_exclusions <- function(shared) {
  o <- shared$prepare_opts
  if (is.null(o) || is.null(shared$raw_data)) return(shared$pk_data)
  prepare_pk_dataset(shared$raw_data, o$col_map, o[setdiff(names(o), "col_map")])$data
}

exclusions_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(icon("filter-circle-xmark", class = "me-1", `aria-hidden` = "true"), "Exclusions",
                tags$span(class = "text-muted small ms-2", "samples or profiles you leave out, each with a reason")),
    card_body(
      uiOutput(ns("register")),
      tags$div(class = "d-flex gap-2 flex-wrap mt-2",
        actionButton(ns("add"), "Add an exclusion", class = "btn-outline-primary btn-sm", icon = icon("plus")),
        checkboxInput(ns("show_restored"), "Show restored exclusions", FALSE))))
}

#' Summary line above results: what is left out, by the analyst and by the app
exclusion_strip_ui <- function(id) uiOutput(NS(id, "strip"))

exclusion_strip_server <- function(id, shared, app_note = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    output$strip <- renderUI({
      ex <- active_exclusions(shared$exclusions)
      app <- app_note()
      if (nrow(ex) == 0 && length(app) == 0) return(NULL)
      tags$div(class = "alert alert-secondary py-2 small mb-2", role = "status",
        icon("filter-circle-xmark", class = "me-1", `aria-hidden` = "true"),
        if (nrow(ex) > 0) paste0("Left out by you: ", sum(ex$level == "sample"), " sample(s), ",
                                 sum(ex$level == "profile"), " profile(s). "),
        if (length(app) > 0) paste0("Left out by the app: ", paste(app, collapse = "; "), " "),
        actionLink(session$ns("review"), "Review exclusions…"))
    })
    observeEvent(input$review, shared$exclusion_request <- list(review = TRUE, nonce = Sys.time()))
  })
}

exclusions_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Every sample, including excluded ones, to choose from
    all_data <- reactive({ req(shared$data_ready); data_without_exclusions(shared) })
    profiles <- reactive({ req(all_data()); data_profiles(all_data(), shared$col_map) })

    register_table <- function(show_restored) {
      ex <- as_exclusions(shared$exclusions)
      if (!isTRUE(show_restored)) ex <- ex[is.na(ex$restored_utc) | !nzchar(ex$restored_utc), , drop = FALSE]
      if (nrow(ex) == 0) return(tags$p(class = "text-muted small mb-0", "No exclusions. All samples and profiles are used."))
      tbl <- exclusion_sheet(ex)
      tags$div(class = "table-responsive",
        tags$table(class = "table table-sm small mb-1",
          tags$thead(tags$tr(lapply(c("Excluded", names(tbl)[c(6, 7, 8, 9, 10, 11, 12)]), tags$th))),
          tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
            restored <- tbl$Status[i] != "in force"
            tags$tr(class = if (restored) "text-muted",
                    style = if (restored) "text-decoration: line-through;",
                    tags$td(exclusion_labels(ex[i, , drop = FALSE])),
                    lapply(tbl[i, c(6, 7, 8, 9, 10, 11, 12)], function(v) tags$td(ifelse(is.na(v), "", as.character(v)))))
          }))))
    }
    output$register <- renderUI({
      req(shared$data_ready)
      act <- active_exclusions(shared$exclusions)
      tagList(register_table(input$show_restored),
        if (nrow(act) > 0) tags$div(class = "d-flex gap-2 align-items-end flex-wrap",
          selectInput(ns("restore_id"), "Restore an exclusion",
                      choices = stats::setNames(act$id, exclusion_labels(act)), width = "260px"),
          textInput(ns("restore_reason"), "Reason for restoring", width = "260px"),
          actionButton(ns("restore"), "Restore", class = "btn-outline-secondary btn-sm mb-3")))
    })

    # ---- The dialog ---------------------------------------------------------
    open_dialog <- function(label = NULL) {
      req(shared$data_ready)
      pr <- profiles()
      showModal(modalDialog(
        title = "Exclude samples or a profile", size = "l", easyClose = FALSE,
        tags$p(class = "small", "Leave out data for a reason that is not pharmacokinetic, preferably one ",
               "defined in the protocol. A value that only looks unusual is not a reason to exclude it."),
        selectInput(ns("dlg_profile"), "Profile", choices = pr$label,
                    selected = if (!is.null(label) && label %in% pr$label) label else pr$label[1]),
        radioButtons(ns("dlg_level"), "What to leave out",
                     choices = c("Samples of this profile (treated as never collected)" = "sample",
                                 "The whole profile (kept in the NCA listing, left out of summaries, mean curves and bioequivalence)" = "profile")),
        uiOutput(ns("dlg_samples")),
        selectInput(ns("dlg_category"), "Reason", choices = NULL),
        textInput(ns("dlg_detail"), "Detail (required for Other, protocol, dosing and adverse-event reasons)"),
        tags$div(class = "d-flex gap-3",
          radioButtons(ns("dlg_prespecified"), "Pre-specified in the protocol?", c("Yes" = "yes", "No" = "no"),
                       selected = "no", inline = TRUE),
          textInput(ns("dlg_section"), "Protocol section", width = "200px")),
        uiOutput(ns("dlg_consequence")),
        footer = tagList(modalButton("Cancel"), actionButton(ns("dlg_save"), "Exclude", class = "btn-danger"))))
    }
    observeEvent(input$dlg_level, {
      updateSelectInput(session, "dlg_category", choices = EXCLUSION_CATEGORIES[[input$dlg_level]])
    })
    profile_rows <- reactive({
      req(input$dlg_profile, all_data())
      d <- all_data(); d[profile_data_rows(d, shared$col_map, input$dlg_profile), , drop = FALSE]
    })
    output$dlg_samples <- renderUI({
      req(identical(input$dlg_level, "sample"))
      d <- profile_rows(); cm <- shared$col_map
      checkboxGroupInput(ns("dlg_times"), "Samples to leave out",
        choices = stats::setNames(as.character(d[[cm$time]]),
                                  paste0("t = ", d[[cm$time]], "   C = ", signif(d[[cm$conc]], 4))),
        inline = TRUE)
    })
    output$dlg_consequence <- renderUI({
      req(input$dlg_profile, input$dlg_level)
      d <- profile_rows(); cm <- shared$col_map
      tt <- suppressWarnings(as.numeric(d[[cm$time]])); cc <- suppressWarnings(as.numeric(d[[cm$conc]]))
      warn <- character(0)
      if (identical(input$dlg_level, "sample")) {
        sel <- suppressWarnings(as.numeric(input$dlg_times))
        if (length(sel) == 0) return(tags$p(class = "small text-muted", "Choose the samples to leave out."))
        meas <- !is.na(cc) & cc > 0 & !(d[[BLQ_FLAG_COLUMN]] %in% TRUE)
        if (any(meas) && any(same_time(sel, tt[which.max(replace(cc, is.na(cc), -Inf))]))) warn <- c(warn, "This is the Cmax sample.")
        if (any(meas) && any(vapply(sel, function(x) same_time(x, max(tt[meas])), logical(1)))) warn <- c(warn, "This is the last measurable sample (Tlast).")
        pre <- any(sel <= 0)
        txt <- paste0(length(sel), " sample(s) of ", input$dlg_profile, " will be treated as never collected. ",
                      "Cmax, Tmax, AUC and the half-life of this profile are calculated again without them.")
        tagList(tags$div(class = "alert alert-warning py-2 small", role = "alert", txt,
                         if (length(warn) > 0) tags$div(tags$strong(paste(warn, collapse = " ")))),
                if (pre) checkboxInput(ns("dlg_confirm_predose"),
                  "I confirm leaving out the pre-dose (or trough) sample: the ICH M13A pre-dose check still uses it.", FALSE))
      } else {
        tags$div(class = "alert alert-warning py-2 small", role = "alert",
                 paste0(input$dlg_profile, " keeps its NCA result but is left out of summary statistics, mean curves ",
                        "and the bioequivalence comparison for all parameters."))
      }
    })
    observeEvent(input$add, open_dialog())
    observeEvent(shared$exclusion_request, {
      r <- shared$exclusion_request
      if (isTRUE(r$review)) {
        showModal(modalDialog(title = "Exclusions", size = "l", easyClose = TRUE, register_table(TRUE),
                              tags$p(class = "small text-muted mb-0", "Add or restore exclusions in Upload & Check Data."),
                              footer = modalButton("Close")))
      } else open_dialog(r$label)
    }, ignoreInit = TRUE)

    # Prepare the data again when the samples in force change
    reprepare <- function() {
      o <- shared$prepare_opts; req(o, shared$raw_data)
      ds <- prepare_pk_dataset(shared$raw_data, o$col_map,
                               c(o[setdiff(names(o), "col_map")], list(exclusions = shared$exclusions)))
      ds$qc <- shared$pk_dataset$qc; ds$interlocks <- shared$pk_dataset$interlocks
      shared$pk_dataset <- ds
      shared$pk_data <- ds$data
    }
    who <- function() { u <- gxp_user(session); if (is.null(u)) "" else paste0(u$name, " (", u$user, ")") }

    observeEvent(input$dlg_save, {
      lvl <- input$dlg_level; cat <- input$dlg_category; det <- trimws(.nz(input$dlg_detail))
      if (is.null(cat) || !nzchar(cat)) { showNotification("Choose a reason.", type = "error"); return() }
      if (cat %in% EXCLUSION_DETAIL_REQUIRED && !nzchar(det)) {
        showNotification("Describe the reason in the detail field (for a protocol deviation, its ID).", type = "error")
        return()
      }
      pr <- profiles(); p <- pr[pr$label == input$dlg_profile, , drop = FALSE]; req(nrow(p) == 1)
      times <- if (lvl == "sample") suppressWarnings(as.numeric(input$dlg_times)) else NA_real_
      if (lvl == "sample" && length(times) == 0) { showNotification("Choose the samples to leave out.", type = "error"); return() }
      if (lvl == "sample" && any(times <= 0) && !isTRUE(input$dlg_confirm_predose)) {
        showNotification("Confirm that the pre-dose sample is left out.", type = "error"); return()
      }
      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      new <- as_exclusions(data.frame(
        id = paste0("x", format(as.numeric(Sys.time()) * 1000, scientific = FALSE), "_", seq_along(times)),
        level = lvl, subject = p$Subject,
        treatment = if ("Treatment" %in% names(p)) p$Treatment else NA,
        period = if ("Period" %in% names(p)) p$Period else NA,
        time = times, category = cat, detail = if (nzchar(det)) det else NA,
        protocol_section = if (identical(input$dlg_prespecified, "yes"))
          paste0("yes", if (nzchar(.nz(input$dlg_section))) paste0(", ", input$dlg_section)) else "no",
        after_be = !is.null(shared$be_results), created_utc = now, created_by = who(),
        restored_utc = NA, restore_reason = NA, stringsAsFactors = FALSE))
      for (i in seq_len(nrow(new)))
        if (!gxp_guard("exclusion_added", object = exclusion_labels(new[i, , drop = FALSE]),
                       details = as.list(new[i, c("level", "time", "category", "detail", "protocol_section", "after_be")]))) return()
      shared$exclusions <- rbind(as_exclusions(shared$exclusions), new)
      if (lvl == "sample") reprepare()
      removeModal()
      showNotification(paste0("Excluded: ", paste(exclusion_labels(new), collapse = "; "),
                              if (isTRUE(new$after_be[1])) ". Marked as made after bioequivalence results were shown." else "."),
                       type = "message", duration = 8)
    })

    observeEvent(input$restore, {
      ex <- as_exclusions(shared$exclusions); i <- which(ex$id == input$restore_id); req(length(i) == 1)
      why <- trimws(.nz(input$restore_reason))
      if (gxp_enabled() && !nzchar(why)) { showNotification("Give a reason for restoring.", type = "error"); return() }
      if (!gxp_guard("exclusion_restored", object = exclusion_labels(ex[i, , drop = FALSE]),
                     details = list(id = ex$id[i], reason = why))) return()
      ex$restored_utc[i] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ex$restore_reason[i] <- if (nzchar(why)) why else NA
      shared$exclusions <- ex
      if (ex$level[i] == "sample") reprepare()
      showNotification(paste0("Restored: ", exclusion_labels(ex[i, , drop = FALSE]), "."), type = "message", duration = 6)
    })
  })
}
