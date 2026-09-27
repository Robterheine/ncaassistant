#!/usr/bin/env Rscript
# ============================================================================
# NCA Assistant: user administration for controlled mode
# ============================================================================
# Run by the system owner on the server, as the app's service account:
#   sudo -H -u shiny Rscript /srv/shiny-server/nca/gxp/manage_users.R <command> ...
# (-H: R then reads the service account's .Renviron, where the settings are)
# Every change is written to the audit trail under the person who ran it
# (SUDO_USER), with a reason. There is no delete: user IDs are never reused.
#
# Commands:
#   init                                   create the user store and the audit trail
#   add <id> "<printed name>" <roles> "<reason>"
#   role <id> <roles> "<reason>"           roles: analyst, reviewer, analyst;reviewer, inspector
#   reset <id> "<reason>"                  starting password admin, change at next login
#   deactivate <id> "<reason>"
#   list                                   accounts, roles and their history
#   verify [copy] [seq:hash ...]           check the trail (or an archived copy), and
#                                          that the heads you filed are still there
#   head                                   print the anchor (last seq and hash)
#   archive <folder>                       read-only archive that can be restored
#
# Needs NCA_GXP_DIR, NCA_GXP_KEY and NCA_GXP_ORG, as the app does.
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
app_dir <- normalizePath(file.path(dirname(script), ".."))
source(file.path(app_dir, "R", "gxp_audit.R"))
APP_VERSION <- tryCatch({
  l <- grep("^APP_VERSION", readLines(file.path(app_dir, "app.R")), value = TRUE)[1]
  eval(parse(text = sub("^APP_VERSION\\s*<-\\s*", "", l)))
}, error = function(e) NA_character_)

fail <- function(...) { message("Error: ", ...); quit(status = 1) }
say  <- function(...) cat(..., "\n", sep = "")

cmd <- if (length(args) > 0) args[1] else ""
cfg <- gxp_config()
if (is.null(cfg)) fail("NCA_GXP_DIR is not set.")
if (!nzchar(cfg$key) || !nzchar(cfg$org)) fail("NCA_GXP_KEY and NCA_GXP_ORG must be set.")

# The person acting: the one who ran sudo, never the service account itself
acting <- if (nzchar(Sys.getenv("SUDO_USER"))) Sys.getenv("SUDO_USER") else Sys.info()[["user"]]
service <- Sys.getenv("NCA_GXP_SERVICE_ACCOUNT", "shiny")
if (!cmd %in% c("verify", "head") && identical(acting, service))
  fail("Run this as yourself through sudo (sudo -H -u ", service, " Rscript <full path>/gxp/manage_users.R ...), so that the audit trail names you.")

need <- function(n, usage) if (length(args) < n + 1) fail("Usage: manage_users.R ", usage)
log_admin <- function(event, object, details, reason = NULL)
  audit_append(event, object = object, details = details, reason = reason,
               user = acting, role = "system owner")
check_roles <- function(r) {
  parts <- strsplit(r, ";", fixed = TRUE)[[1]]
  if (length(parts) == 0 || !all(parts %in% GXP_ROLES) || anyDuplicated(parts))
    fail("Roles must be analyst, reviewer, analyst;reviewer or inspector.")
  if ("inspector" %in% parts && length(parts) > 1) fail("The inspector role cannot be combined with another role.")
  paste(parts, collapse = ";")
}
check_reason <- function(r) if (!nzchar(trimws(r))) fail("A reason is required.") else r
user_row <- function(store, id) {
  i <- which(store$credentials$user == id)
  if (length(i) != 1) fail("No account ", id, ".")
  i
}
starting_password <- function(store, id) {
  store$credentials$password[store$credentials$user == id] <- "admin"
  store$credentials$is_hashed_password[store$credentials$user == id] <- FALSE
  j <- store$pwd_mngt$user == id
  # The app asks for the new password itself (the password_reset entry makes it due);
  # shinymanager's own flag stays FALSE, so that it never rewrites the store
  store$pwd_mngt$must_change[j] <- "FALSE"; store$pwd_mngt$have_changed[j] <- "FALSE"
  store$pwd_mngt$date_change[j] <- as.character(Sys.Date()); store$pwd_mngt$n_wrong_pwd[j] <- 0
  store
}
is_deactivated <- function(store, id) {
  e <- store$credentials$expire[store$credentials$user == id]
  length(e) == 1 && !is.na(e) && nzchar(e) && as.Date(e) < Sys.Date()
}

switch(cmd,

init = {
  if (file.exists(cfg$users) || file.exists(cfg$trail)) fail("A user store or audit trail already exists in ", cfg$dir, ".")
  dir.create(cfg$dir, showWarnings = FALSE, recursive = TRUE, mode = "0700")
  dir.create(cfg$records, showWarnings = FALSE, mode = "0700")
  audit_init(cfg$trail, user = acting, org = cfg$org,
             details = list(app_version = APP_VERSION, host = Sys.info()[["nodename"]]))
  gxp_store_init(cfg$users, cfg$key)
  say("Created the user store and the audit trail in ", cfg$dir, ".")
},

add = {
  need(4, 'add <id> "<printed name>" <roles> "<reason>"')
  id <- args[2]; name <- trimws(args[3]); roles <- check_roles(args[4]); reason <- check_reason(args[5])
  if (!grepl("^[A-Za-z0-9._-]{2,64}$", id)) fail("A user ID has 2 to 64 letters, digits, dots, hyphens or underscores.")
  if (!nzchar(name)) fail("A printed name is required.")
  tr <- audit_read()
  # IDs typed at the login page that were never accounts ("unknown user") do not count
  if (id %in% c(tr$user[!tr$role %in% "unknown user"], tr$object[grepl("^user_|^password_", tr$event)]))
    fail("The ID ", id, " has been used before; IDs are never reused.")
  gxp_store_update(function(store) {
    if (id %in% store$credentials$user) fail("The ID ", id, " already exists.")
    store$credentials <- rbind(store$credentials, data.frame(
      user = id, password = "admin", start = as.character(Sys.Date()), expire = NA_character_,
      admin = "FALSE", name = name, roles = roles, is_hashed_password = FALSE,
      stringsAsFactors = FALSE)[, names(store$credentials)])
    store$pwd_mngt <- rbind(store$pwd_mngt, data.frame(
      user = id, must_change = "FALSE", have_changed = "FALSE", date_change = as.character(Sys.Date()),
      n_wrong_pwd = 0, stringsAsFactors = FALSE)[, names(store$pwd_mngt)])
    store
  })
  log_admin("user_added", id, list(name = name, roles = list(old = NULL, new = roles)), reason)
  say("Added ", id, " (", name, ", ", roles, ") with the starting password admin.")
  say("The user must sign in now and choose a new password. If admin does not work for them, reset the account.")
},

role = {
  need(3, 'role <id> <roles> "<reason>"')
  id <- args[2]; roles <- check_roles(args[3]); reason <- check_reason(args[4]); old <- NA
  gxp_store_update(function(store) {
    i <- user_row(store, id)
    old <<- store$credentials$roles[i]
    store$credentials$roles[i] <- roles
    store
  })
  log_admin("role_changed", id, list(roles = list(old = old, new = roles)), reason)
  say("Roles of ", id, ": ", old, " -> ", roles, ".")
},

reset = {
  need(2, 'reset <id> "<reason>"')
  id <- args[2]; reason <- check_reason(args[3])
  gxp_store_update(function(store) {
    user_row(store, id)
    if (is_deactivated(store, id)) fail("The account ", id, " is deactivated and stays so.")
    starting_password(store, id)
  })
  log_admin("password_reset", id, list(must_change = list(old = NULL, new = TRUE), lock_counter = list(new = 0)), reason)
  say("Reset ", id, " to the starting password admin. The user must sign in now and choose a new password.")
},

deactivate = {
  need(2, 'deactivate <id> "<reason>"')
  id <- args[2]; reason <- check_reason(args[3]); old <- NA
  new <- as.character(Sys.Date() - 1)
  gxp_store_update(function(store) {
    i <- user_row(store, id)
    old <<- store$credentials$expire[i]
    store$credentials$expire[i] <- new
    store
  })
  log_admin("user_deactivated", id, list(expire = list(old = old, new = new)), reason)
  say("Deactivated ", id, ". The account is kept; the ID cannot be used again.")
},

list = {
  store <- gxp_store_read(); tr <- audit_read()
  cr <- store$credentials
  if (nrow(cr) == 0) { say("No accounts."); quit(status = 0) }
  for (i in seq_len(nrow(cr))) {
    id <- cr$user[i]
    status <- if (is_deactivated(store, id)) "deactivated" else if (gxp_is_locked(id, store, tr)) "locked" else
      if (gxp_must_change(id, store, tr)) "active (password change due)" else "active"
    hist <- tr[tr$object %in% id & tr$event %in% c("user_added", "role_changed", "user_deactivated", "password_reset"), ]
    say(sprintf("%-16s %-24s %-18s %s", id, cr$name[i], cr$roles[i], status))
    for (k in seq_len(nrow(hist)))
      say(sprintf("    %s  %-17s by %-12s %s", hist$time_utc[k], hist$event[k], hist$user[k], hist$details[k]))
  }
},

verify = {
  rest <- args[-1]
  is_head <- grepl("^[0-9]+:[0-9a-f]{64}$", rest)
  if (sum(!is_head) > 1) fail("Usage: manage_users.R verify [copy] [seq:hash ...]")
  path <- if (any(!is_head)) rest[!is_head] else cfg$trail
  if (dir.exists(path)) path <- file.path(path, "audit.sqlite")
  anchors <- if (any(is_head)) data.frame(seq = as.integer(sub(":.*", "", rest[is_head])),
                                          hash = sub(".*:", "", rest[is_head]), stringsAsFactors = FALSE)
  v <- audit_verify(path, anchors = anchors)
  say(if (v$intact) sprintf("Intact: %d entries.", v$n) else sprintf("NOT INTACT: first broken entry %s.", v$first_broken))
  for (e in c(v$errors, v$warnings)) say("  ", e)
  if (!any(!is_head)) log_admin("trail_verified", NULL, list(intact = v$intact, n = v$n, warnings = length(v$warnings),
                                                          heads_checked = sum(is_head)))
  quit(status = if (v$intact) 0 else 2)
},

head = {
  h <- audit_head(cfg$trail)
  say("Trail head: entry ", h$seq, ", hash ", h$hash, " (", gxp_utc_now(), ")")
  say("To file, and to give to verify later: ", h$seq, ":", h$hash)
},

archive = {
  need(1, "archive <folder>")
  out <- file.path(args[2], paste0("NCA_archive_", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")))
  if (!suppressWarnings(dir.create(out, recursive = TRUE)))
    fail("Cannot create ", out, ". The folder must exist and be writable by the service account (", service, ").")
  # A consistent copy of the trail while the app may be writing
  con <- .audit_connect(cfg$trail)
  DBI::dbExecute(con, sprintf("VACUUM INTO '%s'", gsub("'", "''", file.path(out, "audit.sqlite"))))
  DBI::dbDisconnect(con)
  dir.create(file.path(out, "records"))
  file.copy(list.files(cfg$records, full.names = TRUE), file.path(out, "records"), copy.date = TRUE)
  # The software that reads and verifies them: the files that are running,
  # not a git commit, which may differ from them
  release <- file.path(out, "app_release.zip")
  git <- function(...) tryCatch(suppressWarnings(system2("git", c("-C", shQuote(app_dir), ...), stdout = TRUE, stderr = FALSE)),
                                error = function(e) character(0))
  tag <- git("describe", "--tags", "--always")
  tag <- if (length(tag) == 1 && nzchar(tag)) {
    paste0("git ", tag, if (length(git("status", "--porcelain", "--untracked-files=no")) > 0) " with local changes" else "")
  } else "not a git checkout"
  tag <- paste0(tag, "; app version ", APP_VERSION)
  old <- setwd(app_dir)
  utils::zip(release, intersect(c("app.R", "R", "www", "gxp", "cdisc", "converters", "data", "validation"), list.files()),
             flags = "-r9Xq")
  setwd(old)
  for (f in c("renv.lock", "release_manifest.csv", "validation_results.csv", "validation_environment.txt",
              "NCA_Assistant_URS.docx", "NCA_Assistant_IQOQPQ.docx")) {
    src <- file.path(app_dir, "validation", f)
    if (file.exists(src)) file.copy(src, file.path(out, f))
  }
  v <- audit_verify(file.path(out, "audit.sqlite")); h <- audit_head(file.path(out, "audit.sqlite"))
  writeLines(c(sprintf("Verification of the archived trail, %s", gxp_utc_now()),
               sprintf("Organisation: %s; source: %s", cfg$org, cfg$trail),
               sprintf("Result: %s; %d entries; head %s %s", if (v$intact) "intact" else "NOT INTACT", v$n, h$seq, h$hash),
               v$errors, v$warnings,
               sprintf("Records: %d files", length(list.files(file.path(out, "records"))))),
             file.path(out, "verification_report.txt"))
  writeLines(c(
    "# Restoring this archive", "",
    sprintf("Archived %s by %s from %s (%s). App release: %s.", gxp_utc_now(), acting, Sys.info()[["nodename"]], cfg$org, tag), "",
    "1. Unzip `app_release.zip` into an empty folder.",
    "2. Install the package versions of the release: `renv::restore(lockfile = \"renv.lock\")`. On Linux, install",
    "   the system libraries first (user manual, appendix on controlled installations, step 1).",
    "3. Copy this folder (not the original) to a writable location, and copy the user store `users.sqlite` of the",
    "   installation into it if accounts are needed; without it, the trail and records can still be verified.",
    "4. Set NCA_GXP_DIR to that copy, NCA_GXP_KEY and NCA_GXP_ORG, and start the app with `shiny::runApp()`.",
    "5. Check the trail: `Rscript gxp/manage_users.R verify <copy>`, and compare the head with verification_report.txt.",
    "", "MANIFEST.sha256 lists the SHA-256 of every file in this archive."), file.path(out, "RESTORE.md"))
  files <- list.files(out, recursive = TRUE)
  writeLines(paste(vapply(file.path(out, files), sha256_file, ""), files, sep = "  "), file.path(out, "MANIFEST.sha256"))
  manifest_sha <- sha256_file(file.path(out, "MANIFEST.sha256"))
  Sys.chmod(list.files(out, recursive = TRUE, full.names = TRUE), "0444")
  Sys.chmod(c(file.path(out, "records"), out), "0555")
  log_admin("trail_archived", out, list(head = h, intact = v$intact, manifest_sha256 = manifest_sha, release = tag))
  say("Archived to ", out, " (", if (v$intact) "trail intact" else "TRAIL NOT INTACT", ", head ", h$seq, ").")
},

fail("Unknown command. Commands: init, add, role, reset, deactivate, list, verify, head, archive.")
)
