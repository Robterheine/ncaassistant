# ============================================================================
# Controlled mode: Records page, review signatures, Audit trail page
# ============================================================================
# Everything is derived from the audit trail each time; there is no second
# database. Roles are checked on the server for every action.

GXP_MEANING <- c(
  review_approved = paste("I have checked the settings of this analysis against the protocol, its results",
                          "and its audit trail, and I approve this record."),
  review_rejected = "I have reviewed this record and I reject it.")
GXP_TYPE_LABEL <- c(single_nca = "One subject", batch_nca = "All subjects", be = "Bioequivalence",
                    figure = "Figure")
GXP_EVENT_LABEL <- c(
  trail_created = "Audit trail created", app_started = "App started", app_stopped = "App stopped",
  login_ok = "Signed in", login_failed = "Sign-in failed", login_locked = "Sign-in refused: account locked",
  session_end = "Signed out", data_loaded = "Loaded data", analysis_run = "Ran an analysis",
  record_created = "Created a record", export_downloaded = "Downloaded results",
  record_downloaded = "Downloaded a stored record", record_verified = "Checked a record file",
  record_signed = "Signed a record", signature_failed = "Signature refused",
  password_changed = "Changed password", password_change_failed = "Password change refused",
  security_alert = "Security alert sent", trail_verified = "Checked the audit trail",
  trail_exported = "Exported the audit trail", trail_reviewed = "Signed a review of the audit trail",
  trail_archived = "Archived the audit trail", user_added = "Added a user",
  user_deactivated = "Deactivated a user", role_changed = "Changed a user's roles",
  password_reset = "Reset a user's password")

# Plain-language label of one entry (the codes stay in the CSV export)
gxp_event_label <- function(event, object, details) {
  d <- tryCatch(jsonlite::fromJSON(details), error = function(e) list())
  base <- .or(GXP_EVENT_LABEL[event], event)
  if (identical(event, "data_loaded") && identical(d$source, "typed in")) return("Typed in data")
  if (identical(event, "record_signed"))
    return(if (identical(d$meaning, "review_rejected")) "Rejected a record" else "Approved a record")
  if (identical(event, "analysis_run")) {
    what <- c(single_nca = "one subject", multi_nca = "all subjects", be_nca = "bioequivalence, NCA step",
              be = "bioequivalence")[.or(d$path, "")]
    return(paste0("Ran an analysis (", .or(what, .or(object, "")),
                  if (identical(d$trigger, "half-life override")) ", half-life override" else "", ")"))
  }
  if (identical(event, "export_downloaded")) return(paste0("Downloaded results (", toupper(.or(d$format, "")), ")"))
  unname(base)
}

# --- Records, derived from the trail -------------------------------------------

#' One row per stored record, with its creator, review and status
gxp_records <- function(tr = audit_read(), store = tryCatch(gxp_store_read(), error = function(e) NULL)) {
  cr <- tr[tr$event == "record_created" & !is.na(tr$sha256), ]
  cr <- cr[!duplicated(cr$sha256), ]
  if (nrow(cr) == 0) return(data.frame(sha = character(0)))
  printed <- function(u) {
    n <- if (!is.null(store)) store$credentials$name[match(u, store$credentials$user)] else NA
    ifelse(is.na(n), u, n)
  }
  det <- lapply(cr$details, jsonlite::fromJSON)
  rows <- lapply(seq_len(nrow(cr)), function(i) {
    sg <- tr[tr$event == "record_signed" & tr$sha256 %in% cr$sha256[i], ]
    d <- det[[i]]
    rev <- if (nrow(sg) > 0) sg[nrow(sg), ] else NULL
    meaning <- if (!is.null(rev)) jsonlite::fromJSON(rev$details)$meaning else NA
    status <- if (!isTRUE(d$queued)) "Stored" else if (is.null(rev)) "Awaiting review" else
      if (identical(meaning, "review_rejected")) "Rejected" else "Approved"
    data.frame(sha = cr$sha256[i], name = cr$object[i], type = .or(d$record_type, NA),
               study = .or(d$study, NA), created = cr$time_utc[i], author = cr$user[i],
               author_name = printed(cr$user[i]), author_role = cr$role[i],
               reproduction = .or(d$reproduction, NA), queued = isTRUE(d$queued),
               data_sha = .or(d$data_sha256, NA), status = status,
               reviewer = if (!is.null(rev)) rev$user else NA,
               reviewer_name = if (!is.null(rev)) printed(rev$user) else NA,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Does the stored copy still match the hash it was signed under?
gxp_record_valid <- function(sha) {
  f <- file.path(gxp_config()$records, paste0(sha, ".zip"))
  file.exists(f) && identical(sha256_file(f), sha)
}

gxp_validity_line <- function(sha) {
  if (gxp_record_valid(sha))
    tags$p(class = "small text-success mb-1", icon("circle-check", class = "me-1"),
           paste0("Signature valid: the stored record matches SHA-256 ", substr(sha, 1, 8), "."))
  else tags$p(class = "small text-danger fw-bold mb-1", icon("triangle-exclamation", class = "me-1"),
              "Signature INVALID: the stored record has changed since it was signed.")
}

#' Earlier runs on the same data, with the settings that differed
gxp_data_history <- function(rec, tr) {
  if (is.na(rec$data_sha)) return(NULL)
  runs <- tr[tr$event == "analysis_run" & tr$sha256 %in% rec$data_sha & tr$time_utc <= rec$created, ]
  if (nrow(runs) == 0) return(NULL)
  flat <- lapply(runs$details, function(x) {
    d <- jsonlite::fromJSON(x, simplifyVector = FALSE)
    v <- unlist(c(d$settings, d$nca_settings, d$be_settings))
    if (is.null(v)) character(0) else vapply(v, function(z) paste(z, collapse = ","), "")
  })
  diffs <- vapply(seq_along(flat), function(i) {
    if (i == 1) return("")
    a <- flat[[i - 1]]; b <- flat[[i]]; k <- union(names(a), names(b))
    ch <- k[vapply(k, function(n) !identical(a[n], b[n]), logical(1))]
    if (length(ch) == 0) "" else paste(sprintf("%s: %s \u2192 %s", ch, a[ch], b[ch]), collapse = "; ")
  }, "")
  typed <- any(tr$event == "data_loaded" & tr$sha256 %in% rec$data_sha & grepl('"source":"typed in"', tr$details))
  list(runs = data.frame(Time_UTC = runs$time_utc, User = runs$user,
                         Analysis = mapply(gxp_event_label, runs$event, runs$object, runs$details),
                         Settings_changed = diffs, stringsAsFactors = FALSE),
       typed = typed)
}

#' The signature sheet: static HTML, no scripts
gxp_signature_sheet <- function(rec, tr) {
  e <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x)))
  sg <- tr[tr$event == "record_signed" & tr$sha256 %in% rec$sha, ]
  v <- tryCatch(audit_verify(), error = function(err) list(intact = FALSE, n = NA))
  h <- audit_head()
  rows <- if (nrow(sg) == 0) "<tr><td colspan='2'>Not reviewed.</td></tr>" else
    paste(vapply(seq_len(nrow(sg)), function(i) {
      d <- jsonlite::fromJSON(sg$details[i])
      paste0("<tr><th>Reviewed by</th><td>", e(d$printed_name), " (", e(sg$user[i]), ", ", e(sg$role[i]), ")</td></tr>",
             "<tr><th>Date and time</th><td>", e(sg$time_utc[i]), " (UTC)</td></tr>",
             "<tr><th>Meaning</th><td>", e(GXP_MEANING[d$meaning]), "</td></tr>",
             if (!is.na(sg$reason[i])) paste0("<tr><th>Reason</th><td>", e(sg$reason[i]), "</td></tr>") else "",
             "<tr><th>Audit entry</th><td>", e(sg$seq[i]), " / ", e(sg$hash[i]), "</td></tr>")
    }, ""), collapse = "")
  valid <- gxp_record_valid(rec$sha)
  paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><title>Signature sheet ', e(substr(rec$sha, 1, 8)),
         '</title><style>body{font-family:Arial,sans-serif;margin:2em;max-width:52em}th{text-align:left;padding-right:1em;',
         'vertical-align:top;width:12em}td,th{padding:3px 6px;border-bottom:1px solid #ddd}.ok{color:#0E7C66}.bad{color:#C0392B;font-weight:bold}',
         '</style></head><body><h2>Signature sheet</h2><p>NCA Assistant ', e(get0("APP_VERSION", ifnotfound = "")),
         ' &middot; ', e(gxp_config()$org), '</p><table>',
         '<tr><th>Record</th><td>', e(rec$name), '</td></tr>',
         '<tr><th>SHA-256</th><td>', e(rec$sha), '</td></tr>',
         '<tr><th>Type</th><td>', e(.or(GXP_TYPE_LABEL[rec$type], rec$type)), '</td></tr>',
         '<tr><th>Study</th><td>', e(rec$study), '</td></tr>',
         '<tr><th>Created by</th><td>', e(rec$author_name), ' (', e(rec$author), ', ', e(rec$author_role), ')</td></tr>',
         '<tr><th>Created</th><td>', e(rec$created), ' (UTC)</td></tr>', rows, '</table>',
         '<p class="', if (valid) 'ok' else 'bad', '">', if (valid) paste0('Signature valid: the stored record matches SHA-256 ', e(substr(rec$sha, 1, 8)), '.')
         else 'Signature INVALID: the stored record has changed since it was signed.', '</p>',
         '<p>Audit trail: ', if (isTRUE(v$intact)) 'intact' else 'NOT INTACT', ', ', e(v$n), ' entries. Head: entry ', e(h$seq),
         ', ', e(h$hash), '.</p><p>Generated ', e(gxp_utc_now()), ' (UTC).</p></body></html>')
}

# --- Server ---------------------------------------------------------------------

gxp_nav_links <- function(session) {
  btn <- "font-size: 0.7rem; padding: 2px 8px;"
  go <- function(p) sprintf("Shiny.setInputValue('nav_path', '%s', {priority: 'event'}); return false;", p)
  n <- session$userData$gxp_awaiting
  tagList(
    tags$a(href = "#", onclick = go("records"), class = "btn btn-outline-light btn-sm ms-2", style = btn,
           icon("folder-open", class = "me-1"), "Records",
           if (has_role(session, "reviewer") && !is.null(n) && n > 0) tags$span(class = "badge bg-warning ms-1", n)),
    if (has_role(session, c("reviewer", "inspector")))
      tags$a(href = "#", onclick = go("audit"), class = "btn btn-outline-light btn-sm ms-2", style = btn,
             icon("list-check", class = "me-1"), "Audit trail"))
}

#' Everything behind the Records and Audit trail pages
gxp_sign_server <- function(input, output, session) {
  me <- gxp_user(session)$user
  can_review <- function() has_role(session, "reviewer")
  can_see_all <- function() has_role(session, c("reviewer", "inspector"))
  seq_now <- reactivePoll(3000, session, checkFunc = function() audit_head()$seq,
                          valueFunc = function() audit_head()$seq)
  trail <- reactive({ seq_now(); audit_read() })
  store <- reactive({ seq_now(); tryCatch(gxp_store_read(), error = function(e) NULL) })
  records <- reactive(gxp_records(trail(), store()))
  observe({
    r <- records()
    session$userData$gxp_awaiting <- if (nrow(r) == 0) 0L else sum(r$status == "Awaiting review" & r$author != me)
    output$gxp_header <- renderUI(gxp_header_ui(session))
  })
  selected <- reactiveVal(NULL)

  # ---- Records page ----
  output$gxp_records_page <- renderUI({
    tags$div(class = "container-fluid py-3",
      tags$h4("Records"),
      tags$p(class = "text-muted small", if (can_see_all())
        "Every record stored on this installation. Select a record to see its details."
        else "Your records and their review. Select a record to see its details."),
      layout_columns(col_widths = c(7, 5),
        card(card_header(
               if (can_see_all()) selectInput("gxp_rec_filter", NULL, width = "240px",
                 choices = if (can_review()) c("Awaiting review, not mine" = "open", "All records" = "all", "My records" = "mine")
                           else c("All records" = "all")),
               class = "py-1"),
             DT::DTOutput("gxp_rec_table")),
        card(card_header("Details"), card_body(uiOutput("gxp_rec_detail")))),
      if (can_see_all()) card(class = "mt-3", card_header("Verify a record file"), card_body(
        tags$p(class = "small text-muted", "Upload any record zip to see whether this audit trail has it, and its signatures."),
        fileInput("gxp_verify_file", NULL, accept = ".zip"), uiOutput("gxp_verify_result"))))
  })
  shown <- reactive({
    r <- records(); if (nrow(r) == 0) return(r)
    f <- .or(input$gxp_rec_filter, if (can_review()) "open" else "all")
    if (!can_see_all() || f == "mine") r <- r[r$author == me, ]
    else if (f == "open") r <- r[r$status == "Awaiting review" & r$author != me, ]
    r[order(r$created, decreasing = TRUE), ]
  })
  output$gxp_rec_table <- DT::renderDT({
    r <- shown()
    df <- if (nrow(r) == 0) data.frame(Created = character(0), Study = character(0), Type = character(0),
                                       Author = character(0), Status = character(0))
          else data.frame(Created = substr(r$created, 1, 16), Study = r$study,
                          Type = unname(.or(GXP_TYPE_LABEL[r$type], r$type)), Author = r$author_name,
                          Status = r$status, stringsAsFactors = FALSE)
    DT::datatable(df, selection = "single", rownames = FALSE,
                  options = list(pageLength = 15, dom = "tip", language = list(emptyTable = "No records.")))
  })
  observeEvent(input$gxp_rec_table_rows_selected, {
    i <- input$gxp_rec_table_rows_selected
    selected(if (length(i) == 1) shown()$sha[i] else NULL)
  }, ignoreNULL = FALSE)
  rec <- reactive({ s <- selected(); r <- records(); if (is.null(s) || !s %in% r$sha) NULL else r[r$sha == s, ] })
  # The selected record as the trail has it now: actions must not rely on the
  # page's list, which refreshes every few seconds (a second click could sign twice)
  rec_now <- function() {
    s <- selected(); if (is.null(s)) return(NULL)
    r <- gxp_records(audit_read(), tryCatch(gxp_store_read(), error = function(e) NULL))
    if (!s %in% r$sha) NULL else r[r$sha == s, ]
  }

  output$gxp_rec_detail <- renderUI({
    r <- rec()
    if (is.null(r)) return(tags$p(class = "text-muted small", "Select a record."))
    if (!can_see_all() && r$author != me) return(NULL)
    hist <- if (can_see_all()) gxp_data_history(r, trail()) else NULL
    signed <- r$status %in% c("Approved", "Rejected")
    status_class <- c("Awaiting review" = "bg-warning", Approved = "bg-success", Rejected = "bg-danger",
                      Stored = "bg-secondary")[[r$status]]
    tagList(
      tags$p(tags$strong(r$name), tags$br(), tags$span(class = paste("badge", status_class), r$status)),
      tags$table(class = "table table-sm small", tags$tbody(
        tags$tr(tags$th("Type"), tags$td(.or(GXP_TYPE_LABEL[r$type], r$type))),
        tags$tr(tags$th("Study"), tags$td(ifelse(is.na(r$study), "", r$study))),
        tags$tr(tags$th("Created"), tags$td(paste0(r$created, " (UTC), ", r$author_name, " (", r$author, ")"))),
        tags$tr(tags$th("Reproduction"), tags$td(ifelse(is.na(r$reproduction), "", r$reproduction))),
        tags$tr(tags$th("SHA-256"), tags$td(tags$code(substr(r$sha, 1, 16)), "\u2026")),
        if (signed) tags$tr(tags$th("Review"), tags$td(paste0(r$status, " by ", r$reviewer_name, " (", r$reviewer, ")"))))),
      if (signed) gxp_validity_line(r$sha)
      else if (!gxp_record_valid(r$sha)) tags$p(class = "small text-danger fw-bold", "The stored copy no longer matches its SHA-256."),
      if (!is.null(hist) && isTRUE(hist$typed))
        tags$div(class = "alert alert-warning py-1 small", "Data typed in by hand: check every value against the source document."),
      if (!is.null(hist)) tagList(
        tags$p(class = "small fw-semibold mb-1 mt-2",
               "Audit history of the data",
               if (nrow(hist$runs) > 1) tags$span(class = "badge bg-warning ms-1",
                                                  paste(nrow(hist$runs), "runs on this data before the record"))),
        tags$table(class = "table table-sm small", tags$thead(tags$tr(tags$th("Time (UTC)"), tags$th("By"),
                                                                       tags$th("Analysis"), tags$th("Settings changed"))),
                   tags$tbody(lapply(seq_len(nrow(hist$runs)), function(k) tags$tr(
                     tags$td(substr(hist$runs$Time_UTC[k], 1, 19)), tags$td(hist$runs$User[k]),
                     tags$td(hist$runs$Analysis[k]), tags$td(hist$runs$Settings_changed[k])))))),
      tags$div(class = "d-flex flex-wrap gap-2 mt-2",
        actionButton("gxp_rec_summary", "Open summary", class = "btn-outline-secondary btn-sm"),
        downloadButton("gxp_rec_download", if (signed) "Download (with signature sheet)" else "Download",
                       class = "btn-outline-primary btn-sm"),
        if (signed) downloadButton("gxp_rec_sheet", "Signature sheet", class = "btn-outline-primary btn-sm"),
        if (can_review() && r$status == "Awaiting review" && r$author != me) tags$div(class = "ms-auto",
          actionButton("gxp_rec_reject", "Reject", class = "btn-outline-danger btn-sm"),
          actionButton("gxp_rec_approve", "Approve", class = "btn-success btn-sm"))))
  })

  observeEvent(input$gxp_rec_summary, {
    r <- rec(); req(r, can_see_all() || r$author == me)
    f <- file.path(gxp_config()$records, paste0(r$sha, ".zip"))
    d <- tempfile("rec_")
    html <- tryCatch({
      utils::unzip(f, exdir = d)
      s <- list.files(d, pattern = "summary\\.html$", recursive = TRUE, full.names = TRUE)[1]
      paste(readLines(s, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }, error = function(e) "<p>The summary cannot be read from this record.</p>")
    unlink(d, recursive = TRUE)
    showModal(modalDialog(title = r$name, size = "xl", easyClose = TRUE,
                          # sandbox: the summary is shown, never run with the app's rights
                          tags$iframe(srcdoc = html, sandbox = "", style = "width: 100%; height: 70vh; border: 0;"),
                          footer = modalButton("Close")))
  })

  output$gxp_rec_download <- downloadHandler(
    filename = function() { r <- rec_now(); if (r$status %in% c("Approved", "Rejected")) sub("\\.zip$", "_signed.zip", r$name) else r$name },
    content = function(file) {
      r <- rec_now(); req(r, can_see_all() || r$author == me)
      src <- file.path(gxp_config()$records, paste0(r$sha, ".zip"))
      signed <- r$status %in% c("Approved", "Rejected")
      if (!gxp_guard("record_downloaded", object = r$name, sha256 = r$sha, details = list(bundle = signed)))
        stop("The download was stopped because it could not be recorded in the audit trail.", call. = FALSE)
      if (!signed) { file.copy(src, file); return(invisible()) }
      d <- tempfile("bundle_"); dir.create(d)
      file.copy(src, file.path(d, r$name))
      writeLines(gxp_signature_sheet(r, audit_read()), file.path(d, paste0("signatures_", substr(r$sha, 1, 8), ".html")))
      zip_record_dir(d, file)
    })
  output$gxp_rec_sheet <- downloadHandler(
    filename = function() paste0("signatures_", substr(rec()$sha, 1, 8), ".html"),
    content = function(file) {
      r <- rec_now(); req(r, can_see_all() || r$author == me)
      if (!gxp_guard("record_downloaded", object = paste0("signatures_", substr(r$sha, 1, 8), ".html"), sha256 = r$sha,
                     details = list(what = "signature sheet")))
        stop("The download was stopped because it could not be recorded in the audit trail.", call. = FALSE)
      writeLines(gxp_signature_sheet(r, audit_read()), file)
    })

  # ---- Signing ----
  sign_dialog <- function(meaning) {
    r <- rec(); req(r, can_review(), r$author != me, r$status == "Awaiting review")
    showModal(modalDialog(
      title = if (meaning == "review_approved") "Approve this record" else "Reject this record",
      tags$table(class = "table table-sm small", tags$tbody(
        tags$tr(tags$th("Record"), tags$td(r$name)), tags$tr(tags$th("Study"), tags$td(ifelse(is.na(r$study), "", r$study))),
        tags$tr(tags$th("Type"), tags$td(.or(GXP_TYPE_LABEL[r$type], r$type))),
        tags$tr(tags$th("Created"), tags$td(paste0(r$created, " (UTC) by ", r$author_name))),
        tags$tr(tags$th("SHA-256"), tags$td(tags$code(substr(r$sha, 1, 8)))))),
      tags$p(tags$strong(GXP_MEANING[[meaning]])),
      if (meaning == "review_rejected") textAreaInput("gxp_sign_reason", "Reason (required)", width = "100%", rows = 3),
      tags$div(class = "mb-2", tags$label(`for` = "gxp_sign_user", class = "form-label", "User ID"),
               tags$input(id = "gxp_sign_user", type = "text", class = "form-control", autocomplete = "off", autofocus = NA)),
      gxp_password_field("gxp_sign_pwd", "Password"),
      uiOutput("gxp_sign_msg"),
      tags$script(HTML(paste0("$('#gxp_sign_pwd').on('keydown', function(e){ if (e.key === 'Enter') $('#gxp_sign_submit').click(); });",
        if (meaning == "review_rejected") paste0(
          "setTimeout(function(){ $('#gxp_sign_submit').prop('disabled', true);",
          " $('#gxp_sign_reason').on('input', function(){ $('#gxp_sign_submit').prop('disabled', !this.value.trim()); }); }, 0);")))),
      footer = tagList(modalButton("Cancel"),
                       actionButton("gxp_sign_submit", "Sign", class = if (meaning == "review_approved") "btn-success" else "btn-danger"))))
    session$userData$gxp_sign_meaning <- meaning
    session$userData$gxp_sign_sha <- r$sha  # the signature binds to the record shown in the dialog
    output$gxp_sign_msg <- renderUI(NULL)
  }
  observeEvent(input$gxp_rec_approve, sign_dialog("review_approved"))
  observeEvent(input$gxp_rec_reject, sign_dialog("review_rejected"))
  observeEvent(input$gxp_sign_submit, {
    msg <- function(t) output$gxp_sign_msg <- renderUI(tags$div(class = "alert alert-danger py-2 small", t))
    meaning <- session$userData$gxp_sign_meaning
    r <- rec_now()
    # Checks, in the order of the handover (Part A, A.6)
    if (!can_review()) return(msg("Only a reviewer can sign a record."))
    if (is.null(meaning)) return(msg("Open the record and click Approve or Reject first."))
    if (is.null(r)) return(msg("Select a record first."))
    if (r$author == me) return(msg("You cannot review your own record."))
    if (r$status != "Awaiting review") return(msg("This record has already been reviewed."))
    if (!gxp_record_valid(r$sha)) return(msg("The stored record no longer matches its SHA-256; it cannot be signed."))
    reason <- if (meaning == "review_rejected") trimws(.or(input$gxp_sign_reason, "")) else ""
    if (meaning == "review_rejected" && !nzchar(reason)) return(msg("Give the reason for the rejection."))
    if (!identical(r$sha, session$userData$gxp_sign_sha))
      return(msg("The selected record is not the one in this dialog. Close it and open the record again."))
    fail <- function(why) {
      gxp_guard("signature_failed", object = r$name, sha256 = r$sha,
                details = list(attempt = .or(session$userData$gxp_failures, 0L) + 1L, why = why))
      left <- gxp_count_failure(session, "signature_failures")
      msg(sprintf("User ID or password is incorrect. %d attempts left before you are signed out.", left))
    }
    if (!identical(input$gxp_sign_user, me)) return(fail("user ID"))
    st <- tryCatch(gxp_store_read(), error = function(e) NULL)
    if (is.null(st) || gxp_is_locked(me, st)) return(msg("Your account is locked. Please contact the system owner."))
    if (gxp_must_change(me, st)) return(msg("Change your password before you sign."))
    if (!gxp_has_role_now(me, "reviewer", st)) return(msg("Only a reviewer can sign; your reviewer role has been removed."))
    cfg <- gxp_config()
    if (!isTRUE(shinymanager::check_credentials(cfg$users, passphrase = cfg$key)(me, input$gxp_sign_pwd)$result))
      return(fail("password"))
    if (!gxp_guard("record_signed", object = r$name, sha256 = r$sha,
                   reason = if (nzchar(reason)) reason else NULL,
                   details = list(meaning = meaning, printed_name = gxp_user(session)$name))) return()
    removeModal()
    showNotification(if (meaning == "review_approved") "Record approved. The signature sheet is available."
                     else "Record rejected. The signature sheet is available.", type = "message")
  })

  # ---- Verify a record file ----
  output$gxp_verify_result <- renderUI({
    f <- input$gxp_verify_file; req(f); req(can_see_all())
    sha <- sha256_file(f$datapath)
    tr <- trail(); hits <- tr[tr$sha256 %in% sha & tr$event %in% c("record_created", "record_signed"), ]
    if (!gxp_guard("record_verified", object = f$name, sha256 = sha, details = list(found = nrow(hits) > 0))) return(NULL)
    if (nrow(hits) == 0) return(tags$p(class = "small text-danger", "No record with this SHA-256 exists in this audit trail.",
                                       tags$br(), tags$code(sha)))
    r <- gxp_records(tr, store()); r <- r[r$sha == sha, ]
    tagList(tags$p(class = "small", tags$strong(r$name), " \u2014 ", r$status, tags$br(), tags$code(sha)),
            if (r$status %in% c("Approved", "Rejected")) gxp_validity_line(sha),
            tags$ul(class = "small", lapply(seq_len(nrow(hits)), function(k)
              tags$li(paste0(hits$time_utc[k], " (UTC) ",
                             gxp_event_label(hits$event[k], hits$object[k], hits$details[k]), " by ", hits$user[k])))))
  })

  # ---- Audit trail page ----
  output$gxp_audit_page <- renderUI({
    if (!can_see_all()) return(tags$div(class = "container py-4", tags$p("The audit trail is available to reviewers and inspectors.")))
    tags$div(class = "container-fluid py-3",
      tags$h4("Audit trail"),
      uiOutput("gxp_review_line"),
      tags$div(class = "d-flex gap-2 mb-2",
        actionButton("gxp_verify_chain", "Verify chain", class = "btn-outline-secondary btn-sm"),
        downloadButton("gxp_trail_csv", "Export CSV", class = "btn-outline-primary btn-sm"),
        if (can_review()) actionButton("gxp_trail_review", "Sign trail review", class = "btn-primary btn-sm")),
      uiOutput("gxp_verify_msg"),
      navset_tab(id = "gxp_audit_tabs",
        nav_panel("Exceptions", uiOutput("gxp_exceptions")),
        nav_panel("All entries", tags$div(class = "pt-2",
          layout_columns(col_widths = c(3, 3, 3, 3),
            dateRangeInput("gxp_f_dates", "Dates (UTC)", start = Sys.Date() - 30, end = Sys.Date()),
            selectInput("gxp_f_user", "User", choices = c("All" = "")),
            selectInput("gxp_f_event", "Event", choices = c("All" = "", stats::setNames(names(GXP_EVENT_LABEL), GXP_EVENT_LABEL))),
            textInput("gxp_f_search", "Search (text or SHA-256)")),
          DT::DTOutput("gxp_trail_table"), uiOutput("gxp_entry_detail"))),
        nav_panel("Users", tags$div(class = "pt-2", DT::DTOutput("gxp_users_table")))))
  })
  observe({
    tr <- trail(); req(can_see_all())
    updateSelectInput(session, "gxp_f_user", choices = c("All" = "", sort(unique(stats::na.omit(tr$user)))),
                      selected = isolate(input$gxp_f_user))
  })
  last_review <- reactive({
    tr <- trail(); rv <- tr[tr$event == "trail_reviewed", ]
    if (nrow(rv) == 0) NULL else { x <- rv[nrow(rv), ]; x$d <- list(jsonlite::fromJSON(x$details)); x }
  })
  output$gxp_review_line <- renderUI({
    req(can_see_all())
    lr <- last_review(); n <- nrow(trail())
    tags$p(class = "small text-muted", if (is.null(lr)) paste0("The audit trail has not been reviewed yet (", n, " entries).")
      else { k <- n - as.integer(lr$d[[1]]$head_seq)
        sprintf("Last review: %s (UTC) by %s, up to entry %s. %d new %s since.",
                substr(lr$time_utc, 1, 16), lr$user, lr$d[[1]]$head_seq, k, if (k == 1) "entry" else "entries") })
  })
  filtered <- reactive({
    tr <- trail(); req(can_see_all())
    tr <- tr[order(tr$seq, decreasing = TRUE), ]
    dr <- input$gxp_f_dates
    if (length(dr) == 2 && !any(is.na(dr))) tr <- tr[substr(tr$time_utc, 1, 10) >= as.character(dr[1]) &
                                                    substr(tr$time_utc, 1, 10) <= as.character(dr[2]), ]
    if (nzchar(.or(input$gxp_f_user, ""))) tr <- tr[tr$user %in% input$gxp_f_user, ]
    if (nzchar(.or(input$gxp_f_event, ""))) tr <- tr[tr$event %in% input$gxp_f_event, ]
    q <- trimws(.or(input$gxp_f_search, ""))
    if (nzchar(q)) tr <- tr[grepl(q, paste(tr$object, tr$sha256, tr$details, tr$reason), fixed = TRUE), ]
    tr
  })
  output$gxp_trail_table <- DT::renderDT({
    tr <- filtered()
    df <- data.frame(Seq = tr$seq, Time_UTC = substr(tr$time_utc, 1, 19), User = tr$user, Role = tr$role,
                     Event = if (nrow(tr)) mapply(gxp_event_label, tr$event, tr$object, tr$details) else character(0),
                     Object = tr$object, SHA256 = substr(tr$sha256, 1, 8), Reason = tr$reason, stringsAsFactors = FALSE)
    DT::datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 100, dom = "tip"))
  }, server = TRUE)
  output$gxp_entry_detail <- renderUI({
    i <- input$gxp_trail_table_rows_selected; req(length(i) == 1)
    e <- filtered()[i, ]
    tags$pre(class = "small bg-light p-2", jsonlite::prettify(e$details), "\nentry hash: ", e$hash)
  })
  observeEvent(input$gxp_verify_chain, {
    req(can_see_all())
    v <- audit_verify()
    if (!gxp_guard("trail_verified", details = list(intact = v$intact, n = v$n, warnings = length(v$warnings)))) return()
    output$gxp_verify_msg <- renderUI(tags$div(class = paste("alert py-2 small", if (v$intact) "alert-success" else "alert-danger"),
      if (v$intact) sprintf("Intact: %d entries.", v$n) else sprintf("NOT INTACT: first broken entry %s.", v$first_broken),
      if (length(c(v$errors, v$warnings))) tags$ul(lapply(c(v$errors, v$warnings), tags$li))))
  })
  output$gxp_trail_csv <- downloadHandler(
    filename = function() paste0("audit_trail_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content = function(file) {
      req(can_see_all())
      tr <- filtered(); v <- audit_verify(); h <- audit_head()
      if (!gxp_guard("trail_exported", details = list(rows = nrow(tr), intact = v$intact, head_seq = h$seq)))
        stop("The export was stopped because it could not be recorded in the audit trail.", call. = FALSE)
      con <- file(file, "w", encoding = "UTF-8")
      writeLines(c(sprintf("# NCA Assistant audit trail, %s; exported %s (UTC) by %s", gxp_config()$org, gxp_utc_now(), me),
                   sprintf("# Verification: %s, %d entries; head %s %s", if (v$intact) "intact" else "NOT INTACT", v$n, h$seq, h$hash)), con)
      utils::write.csv(tr[order(tr$seq), ], con, row.names = FALSE)
      close(con)
    })

  # ---- Signed trail review (reviewers) ----
  observeEvent(input$gxp_trail_review, {
    req(can_review())
    lr <- last_review(); tr <- trail()
    from <- if (is.null(lr)) tr$time_utc[1] else lr$time_utc
    session$userData$gxp_review_period <- c(from = from, to = gxp_utc_now())
    showModal(modalDialog(title = "Sign trail review",
      tags$p(tags$strong(sprintf("I have reviewed the audit trail from %s to %s.", substr(from, 1, 16),
                                 substr(session$userData$gxp_review_period[["to"]], 1, 16)))),
      tags$div(class = "mb-2", tags$label(`for` = "gxp_rev_user", class = "form-label", "User ID"),
               tags$input(id = "gxp_rev_user", type = "text", class = "form-control", autocomplete = "off")),
      gxp_password_field("gxp_rev_pwd", "Password"), uiOutput("gxp_rev_msg"),
      footer = tagList(modalButton("Cancel"), actionButton("gxp_rev_submit", "Sign", class = "btn-primary"))))
    output$gxp_rev_msg <- renderUI(NULL)
  })
  observeEvent(input$gxp_rev_submit, {
    req(can_review())
    msg <- function(t) output$gxp_rev_msg <- renderUI(tags$div(class = "alert alert-danger py-2 small", t))
    fail <- function() {
      gxp_guard("signature_failed", object = "audit trail review",
                details = list(attempt = .or(session$userData$gxp_failures, 0L) + 1L))
      msg(sprintf("User ID or password is incorrect. %d attempts left before you are signed out.",
                  gxp_count_failure(session, "signature_failures")))
    }
    if (!identical(input$gxp_rev_user, me)) return(fail())
    st <- tryCatch(gxp_store_read(), error = function(e) NULL)
    if (is.null(st) || gxp_is_locked(me, st)) return(msg("Your account is locked. Please contact the system owner."))
    if (gxp_must_change(me, st)) return(msg("Change your password before you sign."))
    if (!gxp_has_role_now(me, "reviewer", st)) return(msg("Only a reviewer can sign; your reviewer role has been removed."))
    cfg <- gxp_config()
    if (!isTRUE(shinymanager::check_credentials(cfg$users, passphrase = cfg$key)(me, input$gxp_rev_pwd)$result)) return(fail())
    p <- session$userData$gxp_review_period; h <- audit_head()
    if (!gxp_guard("trail_reviewed", details = list(from = p[["from"]], to = p[["to"]], head_seq = h$seq, head_hash = h$hash,
                                                   printed_name = gxp_user(session)$name))) return()
    removeModal(); showNotification("Your review of the audit trail has been recorded.", type = "message")
  })

  # ---- Users and exceptions ----
  output$gxp_users_table <- DT::renderDT({
    req(can_see_all())
    DT::datatable(gxp_users_overview(store(), trail()), rownames = FALSE, options = list(pageLength = 50, dom = "tip"))
  })
  output$gxp_exceptions <- renderUI({
    req(can_see_all())
    ex <- gxp_exceptions(trail(), records())
    tagList(lapply(names(ex), function(k) {
      x <- ex[[k]]
      tags$div(class = "mt-3",
        tags$p(class = "fw-semibold mb-1", k, tags$span(class = paste("badge ms-1", if (nrow(x) > 0) "bg-warning" else "bg-light text-dark"), nrow(x))),
        if (nrow(x) > 0) tags$table(class = "table table-sm small", tags$thead(tags$tr(lapply(names(x), tags$th))),
                                    tags$tbody(lapply(seq_len(min(nrow(x), 50)), function(i) tags$tr(lapply(x[i, ], function(v) tags$td(as.character(v))))))))
    }))
  })
}

#' One row per account, from the user store and the trail
gxp_users_overview <- function(store, tr) {
  if (is.null(store) || nrow(store$credentials) == 0) return(data.frame(User = character(0)))
  cr <- store$credentials
  status <- vapply(cr$user, function(u) {
    e <- cr$expire[cr$user == u]
    if (length(e) == 1 && !is.na(e) && nzchar(e) && as.Date(e) < Sys.Date()) "deactivated"
    else if (gxp_is_locked(u, store)) "locked" else "active"
  }, "")
  when <- function(u, ev) { x <- tr$time_utc[tr$object %in% u & tr$event == ev]; if (length(x)) substr(x[length(x)], 1, 16) else "" }
  history <- vapply(cr$user, function(u) {
    x <- tr[tr$object %in% u & tr$event %in% c("user_added", "role_changed"), ]
    if (nrow(x) == 0) return("")
    paste(vapply(seq_len(nrow(x)), function(i) {
      r <- tryCatch(jsonlite::fromJSON(x$details[i])$roles$new, error = function(e) "")
      paste0(substr(x$time_utc[i], 1, 10), ": ", .or(r, ""))
    }, ""), collapse = "; ")
  }, "")
  data.frame(User = cr$user, Name = cr$name, Roles = gsub(";", ", ", cr$roles), Status = unname(status),
             Created = vapply(cr$user, when, "", "user_added"), Deactivated = vapply(cr$user, when, "", "user_deactivated"),
             Role_history = unname(history), stringsAsFactors = FALSE, row.names = NULL)
}

#' The fixed exception queries of the handover (Part A, A.6)
gxp_exceptions <- function(tr, recs = gxp_records(tr)) {
  pick <- function(x) data.frame(Seq = x$seq, Time_UTC = substr(x$time_utc, 1, 19), User = x$user,
                                 Event = x$event, Object = x$object, Details = substr(x$details, 1, 120),
                                 stringsAsFactors = FALSE)
  runs <- tr[tr$event == "analysis_run" & !is.na(tr$sha256), ]
  rec_data <- if (nrow(recs) > 0) recs$data_sha else character(0)
  # Per record: the runs on its data before it was created (as the review panel shows them)
  multi <- if (nrow(recs) > 0) do.call(rbind, lapply(which(!is.na(recs$data_sha)), function(i) {
    n <- sum(runs$sha256 == recs$data_sha[i] & runs$time_utc <= recs$created[i])
    if (n > 1) data.frame(Record = recs$name[i], Data_SHA256 = substr(recs$data_sha[i], 1, 16),
                          Runs_before_record = n, stringsAsFactors = FALSE)
  })) else NULL
  norec <- setdiff(unique(runs$sha256), rec_data)
  starts <- which(tr$event == "app_started")
  unclean <- starts[vapply(starts, function(i) {
    prev <- tr$event[seq_len(i - 1)]; prev <- prev[prev %in% c("app_started", "app_stopped")]
    length(prev) > 0 && prev[length(prev)] == "app_started"
  }, logical(1))]
  cfg_diff <- do.call(rbind, lapply(seq_along(starts)[-1], function(k) {
    strip <- function(x) { d <- jsonlite::fromJSON(x); d$host <- NULL; jsonlite::toJSON(d, auto_unbox = TRUE) }
    if (!identical(strip(tr$details[starts[k - 1]]), strip(tr$details[starts[k]]))) pick(tr[starts[k], ])
  }))
  clock <- which(c(FALSE, tr$time_utc[-1] < tr$time_utc[-nrow(tr)]))
  wh <- as.integer(strsplit(gsub(":\\d\\d", "", .or(gxp_config()$work_hours, "07:00-19:00")), "-")[[1]])
  local_hour <- as.integer(format(as.POSIXct(tr$time_utc, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), "%H", tz = ""))
  human <- !tr$role %in% c("system")
  outside <- which(human & !is.na(local_hour) & (local_hour < wh[1] | local_hour >= wh[2]))
  ses <- tr[!is.na(tr$session) & !tr$role %in% "system", ]
  span <- if (nrow(ses)) stats::aggregate(time_utc ~ user + session, ses, function(x) paste(min(x), max(x), sep = "|")) else NULL
  overlap <- if (!is.null(span) && nrow(span) > 1) do.call(rbind, lapply(split(span, span$user), function(s) {
    if (nrow(s) < 2) return(NULL)
    se <- do.call(rbind, strsplit(s$time_utc, "|", fixed = TRUE))
    o <- outer(se[, 1], se[, 2], "<=") & t(outer(se[, 1], se[, 2], "<=")); diag(o) <- FALSE
    if (any(o)) data.frame(User = s$user[1], Sessions = paste(s$session, collapse = ", "), stringsAsFactors = FALSE)
  })) else NULL
  empty <- data.frame()
  list(
    "Failed and locked sign-ins, and security alerts" = pick(tr[tr$event %in% c("login_failed", "login_locked", "security_alert"), ]),
    "Refused signatures and password changes" = pick(tr[tr$event %in% c("signature_failed", "password_change_failed"), ]),
    "Rejected records" = pick(tr[tr$event == "record_signed" & grepl("review_rejected", tr$details), ]),
    "Records whose stored copy no longer matches its SHA-256" =
      if (nrow(recs)) { bad <- recs[!vapply(recs$sha, gxp_record_valid, logical(1)), ]
        data.frame(Record = bad$name, SHA256 = substr(bad$sha, 1, 16), stringsAsFactors = FALSE) } else empty,
    "Data with more than one run before its record" = .or(multi, empty),
    "Data with runs but no record" = if (length(norec)) data.frame(Data_SHA256 = substr(norec, 1, 16),
                                                                   Runs = vapply(norec, function(s) sum(runs$sha256 == s), 1L)) else empty,
    "App started without a preceding stop" = pick(tr[unclean, ]),
    "Configuration differs from the previous start" = .or(cfg_diff, empty),
    "Entries earlier than the entry before them (clock)" = pick(tr[clock, ]),
    "Activity outside working hours" = pick(tr[outside, ]),
    "One user signed in in two sessions at the same time" = .or(overlap, empty))
}
