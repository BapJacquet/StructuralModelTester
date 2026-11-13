#' Locate the StructuralModelTester package / source root
#' @keywords internal
smt_find_pkg_root <- function() {
  # Explicit overrides first
  env_pkg <- Sys.getenv("SMT_PKG_ROOT", unset = "")
  if (nzchar(env_pkg) && file.exists(file.path(env_pkg, "DESCRIPTION"))) {
    return(normalizePath(env_pkg, winslash = "/", mustWork = FALSE))
  }
  # pkgload / installed namespace
  root <- tryCatch(get_dev_pkg_root(), error = function(e) "")
  if (nzchar(root) && file.exists(file.path(root, "DESCRIPTION"))) return(root)
  # Walk up from getwd() (Shiny often sets wd to inst/app)
  walk <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 1:6) {
    if (file.exists(file.path(walk, "DESCRIPTION"))) {
      pkg <- tryCatch(
        as.character(read.dcf(file.path(walk, "DESCRIPTION"), fields = "Package")[1, 1]),
        error = function(e) NA_character_
      )
      if (identical(pkg, "StructuralModelTester")) return(walk)
    }
    parent <- dirname(walk)
    if (identical(parent, walk)) break
    walk <- parent
  }
  # find.package (load_all or installed)
  p <- tryCatch(find.package("StructuralModelTester", quiet = TRUE), error = function(e) "")
  if (is.character(p) && length(p) >= 1L && nzchar(p[[1]])) {
    p <- normalizePath(p[[1]], winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(p, "DESCRIPTION"))) return(p)
    parent <- dirname(p)
    if (file.exists(file.path(parent, "DESCRIPTION"))) return(parent)
  }
  ""
}

#' Root directory for live app command inbox / results
#'
#' Uses option `smt.cmd.root`, else env `SMT_CMD_ROOT`, else `<pkg>/.smt`.
#' @export
smt_cmd_root <- function() {
  opt <- getOption("smt.cmd.root", NULL)
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    return(normalizePath(opt, winslash = "/", mustWork = FALSE))
  }
  env <- Sys.getenv("SMT_CMD_ROOT", unset = "")
  if (nzchar(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  root <- smt_find_pkg_root()
  if (!nzchar(root)) {
    root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
  file.path(root, ".smt")
}

#' @keywords internal
smt_cmd_inbox_dir <- function(root = smt_cmd_root()) file.path(root, "inbox")

#' @keywords internal
smt_cmd_results_dir <- function(root = smt_cmd_root()) file.path(root, "results")

#' @keywords internal
smt_cmd_owner_path <- function(root = smt_cmd_root()) file.path(root, "owner.json")

#' Ensure `.smt/inbox` and `.smt/results` exist
#' @keywords internal
smt_cmd_ensure_dirs <- function(root = smt_cmd_root()) {
  dir.create(smt_cmd_inbox_dir(root), recursive = TRUE, showWarnings = FALSE)
  dir.create(smt_cmd_results_dir(root), recursive = TRUE, showWarnings = FALSE)
  root
}

#' Read current command-bus owner (session token + heartbeat)
#' @keywords internal
smt_cmd_read_owner <- function(root = smt_cmd_root()) {
  path <- smt_cmd_owner_path(root)
  if (!file.exists(path)) return(NULL)
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!nzchar(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
}

#' Write command-bus ownership for this Shiny session
#' @keywords internal
smt_cmd_claim_owner <- function(session_token, root = smt_cmd_root()) {
  session_token <- as.character(session_token %||% "")[[1]]
  if (!nzchar(session_token)) return(FALSE)
  root <- smt_cmd_ensure_dirs(root)
  path <- smt_cmd_owner_path(root)
  payload <- list(token = session_token, ts = as.numeric(Sys.time()))
  tmp <- paste0(path, ".tmp")
  writeLines(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
    tmp,
    useBytes = TRUE
  )
  file.rename(tmp, path)
  TRUE
}

#' TRUE if this session currently owns the command bus (refresh heartbeat)
#'
#' Only one browser session should consume `.smt/inbox`. A stale heartbeat
#' (>2s) is treated as abandoned so a replacement tab can take over.
#' @keywords internal
smt_cmd_am_owner <- function(session_token, root = smt_cmd_root(), max_age_sec = 2) {
  session_token <- as.character(session_token %||% "")[[1]]
  if (!nzchar(session_token)) return(FALSE)
  cur <- smt_cmd_read_owner(root)
  now <- as.numeric(Sys.time())
  if (is.null(cur)) return(smt_cmd_claim_owner(session_token, root = root))
  tok <- as.character(cur$token %||% "")[[1]]
  ts <- as.numeric(cur$ts %||% 0)[[1]]
  age <- if (is.finite(ts)) now - ts else Inf
  if (identical(tok, session_token)) {
    return(smt_cmd_claim_owner(session_token, root = root))
  }
  if (!is.finite(age) || age > max_age_sec) {
    return(smt_cmd_claim_owner(session_token, root = root))
  }
  FALSE
}

#' @keywords internal
smt_cmd_new_id <- function() {
  sprintf(
    "%s-%04d",
    format(Sys.time(), "%Y%m%d-%H%M%OS3"),
    sample.int(9999L, 1L)
  )
}

#' Send a command to a running Structural Model Tester app
#'
#' The Shiny session polls `.smt/inbox/` and writes a matching result under
#' `.smt/results/`. Use this from a second R process (or the agent) while
#' `run_app()` is live.
#'
#' @param action Character action name (see [smt_cmd_actions()]).
#' @param ... Named arguments for the action (JSON-serializable).
#' @param wait Wait for a result from the app.
#' @param timeout_sec Max seconds to wait when `wait = TRUE`.
#' @param root Command directory root (default [smt_cmd_root()]).
#' @return A list with at least `id`, `ok`, `action`, and usually `message`.
#'   If `wait = FALSE`, returns `list(id = ..., queued = TRUE)`.
#' @export
#' @examples
#' \dontrun{
#' smt_cmd("ping")
#' smt_cmd("add_node", label = "X", kind = "observed")
#' smt_cmd("simulate", n = 200, seed = 1)
#' }
smt_cmd <- function(action, ..., wait = TRUE, timeout_sec = 60, root = smt_cmd_root()) {
  action <- as.character(action)[[1]]
  if (!nzchar(action)) stop("action is required.", call. = FALSE)
  args <- list(...)
  if (length(args) && (is.null(names(args)) || any(!nzchar(names(args))))) {
    stop("All command arguments after action must be named.", call. = FALSE)
  }
  root <- smt_cmd_ensure_dirs(root)
  id <- smt_cmd_new_id()
  payload <- list(
    id = id,
    action = action,
    args = args,
    ts = as.numeric(Sys.time())
  )
  inbox <- file.path(smt_cmd_inbox_dir(root), paste0(id, ".json"))
  tmp <- paste0(inbox, ".tmp")
  writeLines(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", dataframe = "rows"),
    tmp,
    useBytes = TRUE
  )
  file.rename(tmp, inbox)

  if (!isTRUE(wait)) {
    return(list(id = id, queued = TRUE, action = action))
  }
  smt_cmd_await(id, timeout_sec = timeout_sec, root = root)
}

#' Wait for a previously queued command result
#' @param id Command id from [smt_cmd()].
#' @param timeout_sec Max wait.
#' @param root Command directory root.
#' @return Result list.
#' @export
smt_cmd_await <- function(id, timeout_sec = 60, root = smt_cmd_root()) {
  id <- as.character(id)[[1]]
  path <- file.path(smt_cmd_results_dir(root), paste0(id, ".json"))
  t0 <- Sys.time()
  timeout_sec <- as.numeric(timeout_sec)[[1]]
  if (!is.finite(timeout_sec) || timeout_sec < 0.5) timeout_sec <- 0.5
  repeat {
    if (file.exists(path)) {
      txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      res <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
      if (is.list(res)) return(res)
      return(list(id = id, ok = FALSE, message = "Invalid result payload."))
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_sec) {
      stop(
        "Timed out waiting for smt_cmd result id=", id,
        " (is run_app() running and polling .smt/inbox?).",
        call. = FALSE
      )
    }
    Sys.sleep(0.15)
  }
}

#' Supported live command actions
#' @return Character vector of action names.
#' @export
smt_cmd_actions <- function() {
  c(
    "ping",
    "status",
    "get_model",
    "load_project",
    "set_model",
    "add_node",
    "add_latent",
    "add_interaction",
    "remove_node",
    "add_edge",
    "remove_edge",
    "map_column",
    "set_sim_var_kind",
    "set_node_sim",
    "set_edge_prior",
    "set_edge_rel_kind",
    "clear_fit",
    "open_data",
    "simulate",
    "run",
    "cancel_power"
  )
}

#' Write a command result file (app-side)
#' @keywords internal
smt_cmd_write_result <- function(id, result, root = smt_cmd_root()) {
  smt_cmd_ensure_dirs(root)
  path <- file.path(smt_cmd_results_dir(root), paste0(id, ".json"))
  tmp <- paste0(path, ".tmp")
  if (!is.list(result)) result <- list(ok = FALSE, message = as.character(result))
  result$id <- id
  writeLines(
    jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", dataframe = "rows"),
    tmp,
    useBytes = TRUE
  )
  file.rename(tmp, path)
  invisible(path)
}

#' List pending inbox command files (oldest first)
#' @keywords internal
smt_cmd_list_inbox <- function(root = smt_cmd_root()) {
  d <- smt_cmd_inbox_dir(root)
  if (!dir.exists(d)) return(character())
  files <- list.files(d, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) return(character())
  info <- file.info(files)
  files[order(info$mtime)]
}

#' Read one inbox command (NULL if unreadable)
#' @keywords internal
smt_cmd_read_file <- function(path) {
  tryCatch({
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }, error = function(e) NULL)
}
