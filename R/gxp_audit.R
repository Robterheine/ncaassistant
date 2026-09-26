# ============================================================================
# Controlled mode: configuration, audit trail, stored records, security alerts
# ============================================================================
# Controlled mode is off unless NCA_GXP_DIR is set. Every function here is a
# no-op in open mode, and the packages DBI and RSQLite are only reached through
# `pkg::` calls on controlled-mode paths, so a standard installation does not
# need them.
#
# The audit trail is one SQLite table, `trail(seq, entry, hash)`. `entry` is the
# JSON text of an event and `hash` the SHA-256 of exactly that text; each entry
# holds the hash of the one before it (`prev_hash`), so any change, insertion or
# deletion breaks the chain. Two triggers refuse UPDATE and DELETE.

# Private default operator. The path modules define `%||%` with other semantics
# (NA and empty count as missing); a second global definition would change them.
.or <- function(x, y) if (is.null(x)) y else x

#' Controlled-mode configuration, or NULL in open mode
gxp_config <- function() {
  dir <- Sys.getenv("NCA_GXP_DIR")
  if (!nzchar(dir)) return(NULL)
  list(dir        = dir,
       key        = Sys.getenv("NCA_GXP_KEY"),
       org        = Sys.getenv("NCA_GXP_ORG"),
       work_hours = Sys.getenv("NCA_GXP_WORK_HOURS", "07:00-19:00"),
       users      = file.path(dir, "users.sqlite"),
       trail      = file.path(dir, "audit.sqlite"),
       records    = file.path(dir, "records"))
}

gxp_enabled <- function() !is.null(gxp_config())

# Policy values (Part A of the handover); recorded in app_started
GXP_PWD_VALIDITY_DAYS <- 90
GXP_PWD_FAILURE_LIMIT <- 5
GXP_TIMEOUT_MIN       <- 15
GXP_MAX_ATTEMPTS      <- 3    # failed signatures and password changes per session

#' Refuse to start in a half-configured state
#'
#' With `gxp/CONTROLLED` present (a qualified server), controlled mode is
#' mandatory. With NCA_GXP_DIR set, everything it needs must be in place: the
#' app never falls back to open mode.
#' @param app_dir Folder of app.R
gxp_check_startup <- function(app_dir = ".") {
  cfg <- gxp_config()
  if (is.null(cfg)) {
    if (file.exists(file.path(app_dir, "gxp", "CONTROLLED")))
      stop("This installation must run in controlled mode (gxp/CONTROLLED exists), ",
           "but NCA_GXP_DIR is not set. The app does not start.", call. = FALSE)
    return(invisible(FALSE))
  }
  problems <- c(
    if (!dir.exists(cfg$dir)) paste("the directory", cfg$dir, "does not exist"),
    if (dir.exists(cfg$dir) && file.access(cfg$dir, 2) != 0) paste("the directory", cfg$dir, "is not writable"),
    if (!nzchar(cfg$key)) "NCA_GXP_KEY is not set",
    if (!nzchar(cfg$org)) "NCA_GXP_ORG is not set",
    if (!file.exists(cfg$users)) "users.sqlite is missing (run gxp/manage_users.R init)",
    if (!file.exists(cfg$trail)) "audit.sqlite is missing (run gxp/manage_users.R init)",
    unlist(lapply(c("shinymanager", "DBI", "RSQLite"), function(p)
      if (!requireNamespace(p, quietly = TRUE)) paste("package", p, "is not installed"))))
  if (length(problems) > 0)
    stop("Controlled mode is configured (NCA_GXP_DIR) but cannot start: ",
         paste(problems, collapse = "; "), ".", call. = FALSE)
  invisible(TRUE)
}

# --- Audit trail -------------------------------------------------------------

gxp_utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

# RSQLite sets `synchronous = off` on connect by default: that fails while
# another process holds the lock, and could lose committed entries in a power
# cut. So: no pragma on connect, wait up to 10 s for a lock, then full sync.
.audit_connect <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path, synchronous = NULL)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000")
  DBI::dbExecute(con, "PRAGMA synchronous = FULL")
  con
}

#' Create the trail table, its triggers and the first entry
#' @param path audit.sqlite (must not exist yet)
audit_init <- function(path, user, role = "system owner", org = "", details = list()) {
  if (file.exists(path)) stop("The audit trail already exists: ", path, call. = FALSE)
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE trail (seq INTEGER PRIMARY KEY, entry TEXT NOT NULL, hash TEXT NOT NULL)")
  DBI::dbExecute(con, paste("CREATE TRIGGER trail_no_update BEFORE UPDATE ON trail",
                            "BEGIN SELECT RAISE(ABORT, 'audit trail is append-only'); END"))
  DBI::dbExecute(con, paste("CREATE TRIGGER trail_no_delete BEFORE DELETE ON trail",
                            "BEGIN SELECT RAISE(ABORT, 'audit trail is append-only'); END"))
  DBI::dbDisconnect(con); on.exit()
  audit_append("trail_created", details = details, user = user, role = role, org = org, path = path)
}

# JSON text of one entry, fields in the fixed order of the handover
.audit_entry <- function(seq, user, role, org, event, object, sha256, reason,
                         details, session, prev_hash) {
  if (length(details) == 0) details <- structure(list(), names = character(0))
  e <- list(seq = seq, time_utc = gxp_utc_now(), user = user,
            role = paste(role, collapse = ";"), organisation = org, event = event,
            object = object, sha256 = sha256, reason = reason, details = details,
            app_version = get0("APP_VERSION", envir = globalenv(), ifnotfound = NA_character_),
            pipeline_sha256 = get0("PIPELINE_SHA256", envir = globalenv(), ifnotfound = NA_character_),
            session = session, prev_hash = prev_hash)
  as.character(jsonlite::toJSON(e, auto_unbox = TRUE, null = "null", na = "null",
                                digits = NA, force = TRUE))
}

#' Append one entry; stops on any failure and leaves no partial row
#' @return list(seq, hash), invisibly
audit_append <- function(event, object = NULL, sha256 = NULL, details = list(),
                         reason = NULL, user, role, session = NULL,
                         org = gxp_config()$org, path = gxp_config()$trail) {
  if (is.null(path) || !file.exists(path)) stop("The audit trail is not available.", call. = FALSE)
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  ok <- FALSE
  on.exit(if (!ok) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE), add = TRUE, after = FALSE)
  last <- DBI::dbGetQuery(con, "SELECT seq, hash FROM trail ORDER BY seq DESC LIMIT 1")
  seq  <- if (nrow(last) > 0) last$seq[1] + 1L else 1L
  prev <- if (nrow(last) > 0) last$hash[1] else strrep("0", 64)
  entry <- .audit_entry(seq, user, role, .or(org, ""), event, object, sha256, reason,
                        details, session, prev)
  hash <- digest::digest(entry, algo = "sha256", serialize = FALSE)
  DBI::dbExecute(con, "INSERT INTO trail (seq, entry, hash) VALUES (?, ?, ?)",
                 params = list(seq, entry, hash))
  DBI::dbExecute(con, "COMMIT")
  ok <- TRUE
  invisible(list(seq = seq, hash = hash))
}

#' Last entry of the trail: the anchor to file elsewhere
audit_head <- function(path = gxp_config()$trail) {
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  h <- DBI::dbGetQuery(con, "SELECT seq, hash FROM trail ORDER BY seq DESC LIMIT 1")
  list(seq = if (nrow(h)) h$seq[1] else 0L, hash = if (nrow(h)) h$hash[1] else NA_character_)
}

#' The trail as a data frame, one row per entry
#' @param newest_first Order of the rows
audit_read <- function(path = gxp_config()$trail, newest_first = FALSE) {
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con, paste("SELECT seq, entry, hash FROM trail ORDER BY seq",
                                     if (newest_first) "DESC" else "ASC"))
  if (nrow(rows) == 0)
    return(data.frame(seq = integer(0), time_utc = character(0), user = character(0),
                      role = character(0), organisation = character(0), event = character(0),
                      object = character(0), sha256 = character(0), reason = character(0),
                      details = character(0), session = character(0), hash = character(0),
                      stringsAsFactors = FALSE))
  fld <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v)[1] }
  parsed <- lapply(rows$entry, jsonlite::fromJSON, simplifyVector = FALSE)
  data.frame(
    seq = rows$seq,
    time_utc = vapply(parsed, fld, "", "time_utc"),
    user = vapply(parsed, fld, "", "user"),
    role = vapply(parsed, fld, "", "role"),
    organisation = vapply(parsed, fld, "", "organisation"),
    event = vapply(parsed, fld, "", "event"),
    object = vapply(parsed, fld, "", "object"),
    sha256 = vapply(parsed, fld, "", "sha256"),
    reason = vapply(parsed, fld, "", "reason"),
    details = vapply(parsed, function(e) as.character(jsonlite::toJSON(e$details, auto_unbox = TRUE,
                                                                        null = "null")), ""),
    session = vapply(parsed, fld, "", "session"),
    hash = rows$hash, stringsAsFactors = FALSE)
}

#' Check the whole chain
#'
#' Recomputes every hash and link and checks that `seq` has no gaps. An entry
#' whose time is earlier than the one before it is a warning (a clock set back),
#' not a break. Anchors (seq and hash filed elsewhere) reveal a truncated or
#' rebuilt trail.
#' @param anchors data.frame(seq, hash), or NULL
#' @return list(intact, n, first_broken, errors, warnings)
audit_verify <- function(path = gxp_config()$trail, anchors = NULL) {
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con, "SELECT seq, entry, hash FROM trail ORDER BY seq")
  errors <- character(0); warnings <- character(0); first_broken <- NA_integer_
  flag <- function(s, msg) {
    errors <<- c(errors, sprintf("Entry %s: %s", s, msg))
    if (is.na(first_broken)) first_broken <<- as.integer(s)
  }
  prev <- strrep("0", 64); prev_time <- ""
  for (i in seq_len(nrow(rows))) {
    s <- rows$seq[i]
    e <- tryCatch(jsonlite::fromJSON(rows$entry[i], simplifyVector = FALSE), error = function(err) NULL)
    if (is.null(e)) { flag(s, "the entry cannot be read"); prev <- rows$hash[i]; next }
    if (!identical(digest::digest(rows$entry[i], algo = "sha256", serialize = FALSE), rows$hash[i]))
      flag(s, "the content does not match its hash (changed)")
    if (!identical(e$prev_hash, prev))
      flag(s, "the link to the previous entry is broken (entries inserted, deleted or reordered)")
    if (s != i || !identical(as.integer(e$seq), as.integer(s)))
      flag(s, "the sequence has a gap or does not match")
    if (!is.null(e$time_utc) && nzchar(prev_time) && e$time_utc < prev_time)
      warnings <- c(warnings, sprintf("Entry %s: time %s is earlier than the entry before it (%s)",
                                      s, e$time_utc, prev_time))
    prev <- rows$hash[i]; prev_time <- .or(e$time_utc, prev_time)
  }
  if (!is.null(anchors) && nrow(anchors) > 0) {
    for (k in seq_len(nrow(anchors))) {
      a <- anchors[k, ]
      if (a$seq > nrow(rows)) {
        errors <- c(errors, sprintf("Anchor %s: the trail ends at entry %d (entries removed)", a$seq, nrow(rows)))
        if (is.na(first_broken)) first_broken <- as.integer(nrow(rows) + 1L)
      } else if (!identical(rows$hash[rows$seq == a$seq], a$hash)) {
        errors <- c(errors, sprintf("Anchor %s: the hash differs from the one filed (trail rebuilt)", a$seq))
        if (is.na(first_broken)) first_broken <- as.integer(a$seq)
      }
    }
  }
  list(intact = length(errors) == 0, n = nrow(rows), first_broken = first_broken,
       errors = errors, warnings = warnings)
}

# --- Use in the app ------------------------------------------------------------

#' Who is acting in this session (NULL outside a signed-in controlled session)
gxp_user <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(NULL)
  session$userData$gxp
}

gxp_failure_modal <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(invisible())
  shiny::showModal(shiny::modalDialog(
    title = "Not carried out",
    paste("This action was not carried out because it could not be recorded in the",
          "audit trail. Your data and settings are unchanged. Please contact the system owner."),
    easyClose = TRUE, footer = shiny::modalButton("Close")), session = session)
}

#' Write an audit entry before the action it belongs to (fail closed)
#'
#' Returns TRUE when the action may go ahead: always in open mode, and in
#' controlled mode only when the entry was written. On failure it shows the
#' failure dialog and returns FALSE; the caller then does nothing.
#' @param ... Passed to audit_append() (event, object, sha256, details, reason)
gxp_guard <- function(..., session = shiny::getDefaultReactiveDomain()) {
  if (!gxp_enabled()) return(TRUE)
  u <- gxp_user(session)
  ok <- tryCatch({
    audit_append(..., user = .or(u$user, "unknown"), role = .or(u$roles, "unknown"),
                 session = if (!is.null(session)) substr(session$token, 1, 8) else NULL)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) gxp_failure_modal(session)
  ok
}

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

#' SHA-256 of values typed in by hand (times and concentrations)
sha256_values <- function(...) {
  digest::digest(paste(vapply(list(...), function(v) paste(format(v, digits = 15), collapse = ","), ""),
                       collapse = "|"), algo = "sha256", serialize = FALSE)
}

#' Keep a read-only copy of a record and log its creation
#'
#' @param file The zip just built for download
#' @return The record's SHA-256, or FALSE when it could not be stored or logged
#'   (the failure dialog has then been shown). In open mode: TRUE.
gxp_store_record <- function(file, record_name, record_type, study = NA,
                             verdict = NA, queued = TRUE,
                             session = shiny::getDefaultReactiveDomain()) {
  if (!gxp_enabled()) return(TRUE)
  cfg <- gxp_config()
  sha <- tryCatch({
    s <- sha256_file(file)
    dir.create(cfg$records, showWarnings = FALSE)
    dest <- file.path(cfg$records, paste0(s, ".zip"))
    if (!file.exists(dest)) {
      if (!file.copy(file, dest)) stop("copy failed")
      Sys.chmod(dest, "0444")
    }
    if (!identical(sha256_file(dest), s)) stop("stored copy differs")
    s
  }, error = function(e) NULL)
  if (is.null(sha)) { gxp_failure_modal(session); return(FALSE) }
  # The data behind the record: the latest run (or data load) of this session
  data_sha <- tryCatch({
    tr <- audit_read(cfg$trail)
    mine <- tr[tr$session %in% substr(.or(session$token, ""), 1, 8) & tr$event %in% c("analysis_run", "data_loaded"), ]
    if (nrow(mine) > 0) mine$sha256[nrow(mine)] else NA_character_
  }, error = function(e) NA_character_)
  ok <- gxp_guard("record_created", object = record_name, sha256 = sha,
                  details = list(record_type = record_type, study = study,
                                 reproduction = verdict, queued = queued, data_sha256 = data_sha),
                  session = session)
  if (ok) sha else FALSE
}

#' Report attempted misuse to the system log at once (Part 11 11.300(d))
#'
#' Writes one line to the system log (`logger`, for IT's monitoring), or to
#' security_alerts.log in the controlled directory where `logger` is missing,
#' and an audit entry `security_alert`. A failure never blocks the user.
gxp_alert <- function(trigger, target_user, detail = "",
                      session = shiny::getDefaultReactiveDomain()) {
  if (!gxp_enabled()) return(invisible(FALSE))
  cfg <- gxp_config()
  msg <- sprintf("NCA Assistant security alert [%s] trigger=%s user=%s %s",
                 cfg$org, trigger, target_user, detail)
  sent <- tryCatch({
    if (nzchar(Sys.which("logger"))) {
      system2("logger", c("-t", "nca-assistant", "-p", "auth.warning", shQuote(msg)),
              stdout = FALSE, stderr = FALSE) == 0
    } else {
      cat(gxp_utc_now(), msg, "\n", file = file.path(cfg$dir, "security_alerts.log"), append = TRUE)
      TRUE
    }
  }, error = function(e) FALSE)
  u <- gxp_user(session)
  tryCatch(audit_append("security_alert", object = target_user,
                        details = list(trigger = trigger, detail = detail, sent = isTRUE(sent)),
                        user = .or(u$user, "system"), role = .or(u$roles, "system"),
                        session = if (!is.null(session)) substr(session$token, 1, 8) else NULL),
           error = function(e) NULL)
  invisible(isTRUE(sent))
}

#' Record that the app started, with the configuration in force
gxp_app_started <- function() {
  if (!gxp_enabled()) return(invisible())
  cfg <- gxp_config()
  pk <- c("shiny", "shinymanager", "DBI", "RSQLite", "NonCompart", "PowerTOST", "nlme")
  audit_append("app_started", user = "system", role = "system", details = list(
    host = Sys.info()[["nodename"]], directory = cfg$dir, organisation = cfg$org,
    work_hours = cfg$work_hours, pwd_validity_days = GXP_PWD_VALIDITY_DAYS,
    pwd_failure_limit = GXP_PWD_FAILURE_LIMIT, timeout_min = GXP_TIMEOUT_MIN,
    controlled_marker = file.exists(file.path("gxp", "CONTROLLED")),
    r_version = R.version.string,
    packages = as.list(vapply(pk, function(p) tryCatch(as.character(utils::packageVersion(p)),
                                                       error = function(e) NA_character_), ""))))
}

gxp_app_stopped <- function() {
  if (!gxp_enabled()) return(invisible())
  tryCatch(audit_append("app_stopped", user = "system", role = "system",
                        details = list(host = Sys.info()[["nodename"]])),
           error = function(e) NULL)
}

# --- User store (shinymanager's encrypted users.sqlite) -----------------------
# Read and written only inside one SQLite transaction, so that the app (a
# password change) and gxp/manage_users.R (an account change) never overwrite
# each other. Only exported shinymanager functions are used.

GXP_ROLES <- c("analyst", "reviewer", "inspector")

#' Create an empty user store (shinymanager's create_db() needs at least one user)
gxp_store_init <- function(path, key) {
  if (file.exists(path)) stop("The user store already exists: ", path, call. = FALSE)
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  shinymanager::write_db_encrypt(con, name = "credentials", passphrase = key, value = data.frame(
    user = character(0), password = character(0), start = character(0), expire = character(0),
    admin = character(0), name = character(0), roles = character(0),
    is_hashed_password = logical(0), stringsAsFactors = FALSE))
  shinymanager::write_db_encrypt(con, name = "pwd_mngt", passphrase = key, value = data.frame(
    user = character(0), must_change = character(0), have_changed = character(0),
    date_change = character(0), n_wrong_pwd = numeric(0), stringsAsFactors = FALSE))
  shinymanager::write_db_encrypt(con, name = "logs", passphrase = key, value = data.frame(
    user = character(0), server_connected = character(0), token = character(0),
    logout = character(0), app = character(0), stringsAsFactors = FALSE))
  invisible(TRUE)
}

#' Read both tables of the user store
#' @return list(credentials, pwd_mngt)
gxp_store_read <- function(path = gxp_config()$users, key = gxp_config()$key) {
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  list(credentials = shinymanager::read_db_decrypt(con, name = "credentials", passphrase = key),
       pwd_mngt    = shinymanager::read_db_decrypt(con, name = "pwd_mngt", passphrase = key))
}

#' Change the user store in one transaction
#'
#' @param fun function(store) returning the changed list(credentials, pwd_mngt).
#'   A password set in `credentials` with is_hashed_password = FALSE is hashed
#'   (scrypt) by write_db_encrypt().
gxp_store_update <- function(fun, path = gxp_config()$users, key = gxp_config()$key) {
  con <- .audit_connect(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  ok <- FALSE
  on.exit(if (!ok) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE), add = TRUE, after = FALSE)
  store <- list(credentials = shinymanager::read_db_decrypt(con, name = "credentials", passphrase = key),
                pwd_mngt    = shinymanager::read_db_decrypt(con, name = "pwd_mngt", passphrase = key))
  store <- fun(store)
  shinymanager::write_db_encrypt(con, value = store$credentials, name = "credentials", passphrase = key)
  shinymanager::write_db_encrypt(con, value = store$pwd_mngt, name = "pwd_mngt", passphrase = key)
  DBI::dbExecute(con, "COMMIT")
  ok <- TRUE
  invisible(store)
}

#' Is the account locked (failed sign-ins at or above the limit)?
gxp_is_locked <- function(user, store = gxp_store_read()) {
  pm <- store$pwd_mngt
  n <- suppressWarnings(as.numeric(pm$n_wrong_pwd[pm$user == user]))
  length(n) == 1 && !is.na(n) && n >= GXP_PWD_FAILURE_LIMIT
}

# --- Hooks for the analysis modules --------------------------------------------
# One call each in the modules; all are no-ops in open mode, and their
# arguments are not even evaluated there.

#' The analyst named in a record: the signed-in user in controlled mode, the
#' typed name (or "Analyst") otherwise
gxp_analyst <- function(typed, session = shiny::getDefaultReactiveDomain()) {
  u <- if (gxp_enabled()) gxp_user(session) else NULL
  if (!is.null(u)) return(u$name)
  if (!is.null(typed) && nchar(typed) > 0) typed else "Analyst"
}

#' SHA-256 of the uploaded data file behind the current analysis
gxp_data_sha256 <- function(study_info) {
  p <- study_info$file_path
  if (!is.null(p) && file.exists(p)) sha256_file(p) else NA_character_
}

#' After a record zip has been built: store it, log it, show its status
#'
#' Stops (so the download fails) when the record cannot be stored and logged.
gxp_record_done <- function(file, record_name, record_type, study, rec_out, queued = TRUE,
                            session = shiny::getDefaultReactiveDomain()) {
  if (!gxp_enabled()) return(invisible(TRUE))
  sha <- gxp_store_record(file, record_name, record_type,
                          study = if (!is.null(study) && nzchar(study)) study else NA_character_,
                          verdict = .or(attr(rec_out, "reproduction"), NA_character_),
                          queued = queued, session = session)
  if (isFALSE(sha)) stop("The record was not stored because it could not be recorded in the audit trail.", call. = FALSE)
  # The status line under the record button. A download does not flush outputs,
  # so it goes to the browser as a message, which is delivered at once.
  panel <- c(single_nca = "path_single_nca", batch_nca = "path_multi_nca", be = "path_be", figure = "path_viz")
  if (!is.null(session) && record_type %in% names(panel)) session$sendCustomMessage("gxp_record_status", list(
    id = paste0(panel[[record_type]], "-gxp_record_status"),
    html = as.character(tags$p(
      class = "small mt-2 mb-0",
      icon("shield-halved", class = "me-1 text-success"),
      if (queued) paste0("Stored for review \u00B7 ", substr(sha, 1, 8), " \u00B7 Awaiting review \u00B7 ")
      else paste0("Stored \u00B7 ", substr(sha, 1, 8), " "),
      if (queued) tags$a(href = "#", onclick = "Shiny.setInputValue('nav_path', 'records', {priority: 'event'}); return false;",
                         "Open in Records")))))
  invisible(sha)
}

#' Before a results file is handed over: log it with its SHA-256
gxp_export_done <- function(file, file_name, format, data_sha256 = NA_character_,
                            session = shiny::getDefaultReactiveDomain()) {
  if (!gxp_enabled()) return(invisible(TRUE))
  if (!gxp_guard("export_downloaded", object = file_name, sha256 = sha256_file(file),
                 details = list(format = format, data_sha256 = data_sha256), session = session))
    stop("The download was stopped because it could not be recorded in the audit trail.", call. = FALSE)
  invisible(TRUE)
}
