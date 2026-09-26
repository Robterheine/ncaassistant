# ============================================================================
# Controlled mode: login, roles, header menu, password change
# ============================================================================
# Only reached when gxp_enabled(); shinymanager is called through `::` so that
# open mode does not need it. The real app server starts after the login, and
# after a forced password change, never before.

#' Our password rule: at least 12 characters, a digit, a lower-case and an
#' upper-case letter, and never the starting password
gxp_validate_pwd <- function(pwd) {
  is.character(pwd) && length(pwd) == 1 && !is.na(pwd) && nchar(pwd) >= 12 &&
    grepl("[0-9]", pwd) && grepl("[a-z]", pwd) && grepl("[A-Z]", pwd) &&
    tolower(pwd) != "admin"
}

GXP_PWD_RULE <- "At least 12 characters, with a digit, a lower-case and an upper-case letter. Not admin."

gxp_roles <- function(session = shiny::getDefaultReactiveDomain()) {
  r <- gxp_user(session)$roles
  if (is.null(r) || is.na(r)) character(0) else strsplit(r, ";", fixed = TRUE)[[1]]
}

#' Does the signed-in user hold this role? Checked on the server for every
#' capability; hiding a button is never the only check.
has_role <- function(session, role) any(role %in% gxp_roles(session))

#' The login page, around the app UI
gxp_secure_ui <- function(ui) {
  cfg <- gxp_config()
  options(shinymanager.pwd_validity = GXP_PWD_VALIDITY_DAYS,
          shinymanager.pwd_failure_limit = GXP_PWD_FAILURE_LIMIT)
  do.call(shinymanager::set_labels, c(list(language = "en"), stats::setNames(list(GXP_PWD_RULE),
    "Password must contain at least one number, one lowercase, one uppercase and must be at least length 6.")))
  shinymanager::secure_app(
    ui, enable_admin = FALSE, fab_position = "none",
    tags_top = tags$div(
      style = "text-align: center;",
      tags$img(src = "logo.svg", height = "44px"),
      tags$h4("NCA Assistant", style = "margin-top: 8px;"),
      tags$p(class = "text-muted", style = "font-size: 0.85rem;",
             "Controlled installation \u00B7 ", cfg$org, " \u00B7 ", Sys.info()[["nodename"]])),
    tags_bottom = tags$p(class = "text-muted", style = "font-size: 0.85rem; text-align: center;",
                         "Forgot your password? Ask the system owner to reset it."))
}

#' The credential check behind the login page, with audit entries and alerts
audited_check <- function() {
  function(user, password) {
    cfg <- gxp_config()
    res <- shinymanager::check_credentials(cfg$users, passphrase = cfg$key)(user, password)
    # The typed ID goes into the trail and the system log: bounded, no control characters
    user <- substr(gsub("[[:cntrl:]]", " ", .or(user, "")), 1, 64)
    store <- tryCatch(gxp_store_read(), error = function(e) NULL)
    locked <- !is.null(store) && gxp_is_locked(user, store)
    known <- !is.null(store) && user %in% store$credentials$user
    event <- if (isTRUE(res$result) && !locked) "login_ok" else if (isTRUE(res$result)) "login_locked" else "login_failed"
    role <- if (known) store$credentials$roles[store$credentials$user == user] else "unknown user"
    tryCatch(audit_append(event, object = user, user = user, role = role,
                          details = list(expired = isTRUE(res$expired), known_user = known)),
             error = function(e) NULL)
    if (event == "login_locked") gxp_alert("login_locked", user, session = NULL)
    if (event == "login_failed" && known) {
      pm <- store$pwd_mngt
      n <- suppressWarnings(as.numeric(pm$n_wrong_pwd[pm$user == user]))
      # shinymanager counts this failure after we return: this one locks the account
      if (length(n) == 1 && !is.na(n) && n + 1 == GXP_PWD_FAILURE_LIMIT)
        gxp_alert("account_locked", user, sprintf("after %d failed sign-ins", GXP_PWD_FAILURE_LIMIT), session = NULL)
    }
    if (event == "login_failed") {
      tr <- tryCatch(audit_read(), error = function(e) NULL)
      if (!is.null(tr)) {
        since <- format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
        n_hour <- sum(tr$event == "login_failed" & tr$object %in% user & tr$time_utc >= since)
        if (n_hour == 10) gxp_alert("failed_sign_ins", user, "10 failed sign-ins within 60 minutes", session = NULL)
      }
    }
    res
  }
}

#' Log a password change made on shinymanager's own screen (first login or
#' expiry), which has no hook of its own
gxp_detect_forced_change <- function(session) {
  u <- gxp_user(session)$user
  store <- tryCatch(gxp_store_read(), error = function(e) NULL)
  tr <- tryCatch(audit_read(), error = function(e) NULL)
  if (is.null(store) || is.null(tr)) return(invisible())
  pm <- store$pwd_mngt[store$pwd_mngt$user == u, ]
  if (nrow(pm) != 1 || !identical(pm$have_changed, "TRUE")) return(invisible())
  pw <- tr[tr$object %in% u & tr$event %in% c("user_added", "password_reset", "password_changed"), ]
  if (nrow(pw) == 0) return(invisible())
  last <- pw[nrow(pw), ]
  changed <- last$event %in% c("user_added", "password_reset") ||
    as.Date(pm$date_change) > as.Date(substr(last$time_utc, 1, 10))
  if (changed)
    gxp_guard("password_changed", object = u, details = list(via = "first login or expiry"), session = session)
  invisible()
}

#' May the app start for this user? shinymanager_where comes from the browser,
#' so check on the server that the required password change has been done
gxp_app_allowed <- function(user) {
  st <- tryCatch(gxp_store_read(), error = function(e) NULL)
  if (!is.null(st) && !gxp_must_change(user, st)) return(TRUE)
  gxp_alert("forced_change_skipped", user, "app requested before the required password change", session = NULL)
  FALSE
}

#' The server: shinymanager first, the real app once the user is in
gxp_server <- function(server) {
  function(input, output, session) {
    cfg <- gxp_config()
    shinymanager::check_credentials(cfg$users, passphrase = cfg$key)  # tells shinymanager where the store is
    auth <- shinymanager::secure_server(check_credentials = audited_check(),
                                        timeout = GXP_TIMEOUT_MIN, validate_pwd = gxp_validate_pwd)
    started <- FALSE
    signing_out <- FALSE
    observe({
      req(!started, auth$user, identical(input$shinymanager_where, "application"))
      if (!gxp_app_allowed(auth$user)) return()
      started <<- TRUE
      session$userData$gxp <- list(user = auth$user, name = auth$name, roles = auth$roles)
      session$userData$gxp_failures <- 0L
      gxp_detect_forced_change(session)
      who <- isolate(list(user = auth$user, roles = auth$roles))
      session$onSessionEnded(function() {
        tryCatch(audit_append("session_end", object = who$user, user = who$user, role = who$roles,
                              details = list(reason = if (signing_out) "sign out" else "closed or timed out"),
                              session = substr(session$token, 1, 8)),
                 error = function(e) NULL)
      })
      # shinymanager signs out on its own input; the browser sets it (gxp_activity.js)
      observeEvent(input$gxp_sign_out, {
        signing_out <<- TRUE
        session$sendCustomMessage("gxp_logout", TRUE)
      })
      observeEvent(input$gxp_change_password, gxp_password_dialog(session))
      observeEvent(input$gxp_pwd_submit, gxp_password_submit(input, session))
      output$gxp_header <- renderUI(gxp_header_ui(session))
      isolate({
        gxp_sign_server(input, output, session)
        server(input, output, session)
      })
    })
  }
}

#' Controlled-mode items for the header: badge, user menu (Records and Audit
#' trail are added by gxp_sign.R)
gxp_header_ui <- function(session) {
  u <- gxp_user(session); cfg <- gxp_config()
  btn <- "font-size: 0.7rem; padding: 2px 8px;"
  tagList(
    if (exists("gxp_nav_links", mode = "function")) gxp_nav_links(session),
    tags$span(class = "badge border border-light text-light ms-2", style = "font-size: 0.7rem; font-weight: 500;",
              title = paste0("Controlled installation: ", cfg$org, " \u00B7 ", Sys.info()[["nodename"]]),
              icon("shield-halved"), tags$span(class = "visually-hidden", "Controlled installation")),
    tags$div(class = "dropdown ms-2 d-inline-block",
      tags$button(class = "btn btn-outline-light btn-sm dropdown-toggle", style = btn, type = "button",
                  `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                  icon("user", class = "me-1"), u$name),
      tags$ul(class = "dropdown-menu dropdown-menu-end",
        tags$li(tags$span(class = "dropdown-item-text small text-muted",
                          paste0(u$user, " \u00B7 ", gsub(";", ", ", u$roles)))),
        tags$li(tags$hr(class = "dropdown-divider")),
        tags$li(tags$a(class = "dropdown-item", href = "#",
                       onclick = "Shiny.setInputValue('gxp_change_password', Date.now(), {priority: 'event'}); return false;",
                       "Change password")),
        tags$li(tags$a(class = "dropdown-item", href = "#",
                       onclick = "Shiny.setInputValue('gxp_sign_out', Date.now(), {priority: 'event'}); return false;",
                       "Sign out")))))
}

#' One line on the hub: who is signed in, and that work is recorded
gxp_hub_line <- function(session = shiny::getDefaultReactiveDomain()) {
  u <- gxp_user(session)
  if (is.null(u)) return(NULL)
  tags$p(class = "text-muted text-center small mb-0 mt-2",
         sprintf("Signed in as %s (%s). Your analyses and downloads are recorded in the audit trail.",
                 u$name, gsub(";", ", ", u$roles)))
}

# A password field the browser's password manager does not fill in
gxp_password_field <- function(id, label) {
  tags$div(class = "mb-2",
           tags$label(`for` = id, class = "form-label", label),
           tags$input(id = id, type = "password", class = "form-control", autocomplete = "new-password"))
}

gxp_password_dialog <- function(session) {
  shiny::showModal(shiny::modalDialog(
    title = "Change password",
    gxp_password_field("gxp_pwd_current", "Current password"),
    gxp_password_field("gxp_pwd_new", "New password"),
    gxp_password_field("gxp_pwd_repeat", "Repeat the new password"),
    tags$p(class = "text-muted small", GXP_PWD_RULE),
    uiOutput("gxp_pwd_msg"),
    footer = tagList(shiny::modalButton("Cancel"),
                     shiny::actionButton("gxp_pwd_submit", "Change password", class = "btn-primary"))),
    session = session)
  session$output$gxp_pwd_msg <- renderUI(NULL)
}

#' Count a failed signature or password change; the third in a session ends it
gxp_count_failure <- function(session, trigger) {
  n <- .or(session$userData$gxp_failures, 0L) + 1L
  session$userData$gxp_failures <- n
  if (n >= GXP_MAX_ATTEMPTS) {
    gxp_alert(trigger, gxp_user(session)$user, sprintf("%d failed attempts in one session", n), session = session)
    session$close()
  }
  GXP_MAX_ATTEMPTS - n
}

gxp_password_submit <- function(input, session) {
  u <- gxp_user(session)$user
  msg <- function(text) session$output$gxp_pwd_msg <- renderUI(tags$div(class = "alert alert-danger py-2 small", text))
  store <- tryCatch(gxp_store_read(), error = function(e) NULL)
  if (is.null(store)) return(msg("The user store cannot be read. Please contact the system owner."))
  if (gxp_is_locked(u, store)) return(msg("Your account is locked. Please contact the system owner."))
  cfg <- gxp_config()
  current_ok <- isTRUE(shinymanager::check_credentials(cfg$users, passphrase = cfg$key)(u, input$gxp_pwd_current)$result)
  if (!current_ok) {
    gxp_guard("password_change_failed", object = u,
              details = list(attempt = .or(session$userData$gxp_failures, 0L) + 1L), session = session)
    left <- gxp_count_failure(session, "password_change_failures")
    return(msg(sprintf("The current password is incorrect. %d %s left before you are signed out.", left, if (left == 1) "attempt" else "attempts")))
  }
  if (!identical(input$gxp_pwd_new, input$gxp_pwd_repeat)) return(msg("The two new passwords are different."))
  if (identical(input$gxp_pwd_new, input$gxp_pwd_current)) return(msg("The new password must be different from the current one."))
  if (!gxp_validate_pwd(input$gxp_pwd_new)) return(msg(GXP_PWD_RULE))
  if (!gxp_guard("password_changed", object = u, details = list(via = "user menu"), session = session)) return()
  done <- tryCatch({
    gxp_store_update(function(store) {
      i <- store$credentials$user == u
      store$credentials$password[i] <- input$gxp_pwd_new
      store$credentials$is_hashed_password[i] <- FALSE
      j <- store$pwd_mngt$user == u
      store$pwd_mngt$must_change[j] <- "FALSE"; store$pwd_mngt$have_changed[j] <- "TRUE"
      store$pwd_mngt$date_change[j] <- as.character(Sys.Date())
      store
    })
    TRUE
  }, error = function(e) FALSE)
  if (!done) return(msg("The password could not be saved. Please contact the system owner."))
  shiny::removeModal(session)
  shiny::showNotification("Your password has been changed.", type = "message", session = session)
}
