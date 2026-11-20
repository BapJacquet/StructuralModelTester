#' Normalize optional seed data for [run_app()]
#'
#' Accepts a data.frame / tibble; returns a plain data.frame copy.
#' @keywords internal
smt_normalize_seed_data <- function(data) {
  if (is.null(data)) return(NULL)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame (or tibble).", call. = FALSE)
  }
  if (!nrow(data) || !ncol(data)) {
    stop("`data` must have at least one row and one column.", call. = FALSE)
  }
  # Plain copy; keep factors / numeric types (do not stringify).
  as.data.frame(data, optional = TRUE, stringsAsFactors = FALSE)
}

#' Open a URL without aborting when no browser is configured
#'
#' Soft-fails so [shiny::runApp()] keeps running even if `options("browser")`
#' is unset or the opener is missing. Always prints the URL for manual open.
#' @keywords internal
smt_launch_browser <- function(url) {
  url <- if (length(url)) as.character(url[[1]]) else ""
  if (!nzchar(url)) return(invisible(FALSE))
  message("Structural Model Tester: open ", url, " in your browser.")

  opened <- FALSE
  # Prefer R's configured browser when it is usable
  br <- getOption("browser")
  if (is.function(br) || (is.character(br) && length(br) == 1L && nzchar(br) &&
        !identical(br, "false"))) {
    opened <- isTRUE(tryCatch({
      utils::browseURL(url)
      TRUE
    }, error = function(e) FALSE))
  }

  if (!opened) {
    opened <- isTRUE(tryCatch(
      smt_launch_browser_fallback(url),
      error = function(e) FALSE
    ))
  }

  if (!opened) {
    warning(
      "Could not open a system browser. Paste the URL above into any browser, ",
      "or set options(browser = \"firefox\") / the BROWSER env var.",
      call. = FALSE
    )
  }
  invisible(opened)
}

#' Platform fallbacks when utils::browseURL cannot run
#' @keywords internal
smt_launch_browser_fallback <- function(url) {
  if (.Platform$OS.type == "windows") {
    ok <- tryCatch({
      utils::shell.exec(url)
      TRUE
    }, error = function(e) FALSE)
    return(isTRUE(ok))
  }

  candidates <- character()
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    candidates <- c("open")
  } else {
    candidates <- c(
      "xdg-open",
      "firefox",
      "google-chrome",
      "chromium",
      "chromium-browser",
      "microsoft-edge"
    )
  }

  for (cmd in candidates) {
    path <- Sys.which(cmd)
    if (!nzchar(path)) next
    status <- tryCatch(
      system2(path, shQuote(url), wait = FALSE, stdout = FALSE, stderr = FALSE),
      error = function(e) 1L
    )
    # system2(..., wait = FALSE) returns 0 when the process was spawned
    if (is.null(status) || identical(as.integer(status)[[1]], 0L)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Launch the Structural Model Tester app
#'
#' Opens the Structural Model Tester Shiny interface in the browser.
#'
#' @param launch.browser Logical; open in the system browser. When `TRUE`
#'   (default), a soft-fail launcher is used so a missing default browser does
#'   not abort startup — the app URL is always printed. Pass `FALSE` to skip
#'   opening, or a function(url) for a custom opener.
#' @param data Optional data.frame (or tibble) to seed the Data tab. When
#'   supplied, the welcome examples modal is skipped and the app starts with
#'   this dataset already loaded (blank model).
#' @param debug Logical; if `TRUE`, echo `[smt-debug]` lines to the R console.
#'   The gitignored `smt-debug.log` file is still written either way.
#' @param ... Passed to [shiny::runApp()].
#' @export
#' @examples
#' \dontrun{
#' StructuralModelTester::run_app()
#'
#' # Prep in R, then open the app with that frame (no examples modal):
#' dat <- subset(iris, Species != "setosa")
#' StructuralModelTester::run_app(data = dat)
#'
#' # Verbose console tracing while developing UI/plots:
#' StructuralModelTester::run_app(debug = TRUE)
#' }
run_app <- function(launch.browser = TRUE, data = NULL, debug = FALSE, ...) {
  requireNamespace("StructuralModelTester", quietly = TRUE)
  # Prefer the source-tree app when developing (pkgload::load_all). The installed
  # copy under system.file() can lag behind and serve stale JS/CSS that breaks
  # plots and canvas updates.
  pkg_root <- Sys.getenv("SMT_PKG_ROOT", unset = "")
  if (!nzchar(pkg_root)) {
    pkg_root <- tryCatch(get_dev_pkg_root(), error = function(e) "")
  }
  app_dir <- ""
  if (nzchar(pkg_root)) {
    cand <- file.path(pkg_root, "inst", "app")
    if (dir.exists(cand)) app_dir <- cand
  }
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    app_dir <- system.file("app", package = "StructuralModelTester")
  }
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "Cannot find Shiny app directory. Install the package with ",
      "devtools::install() or remotes::install_local(), or run from the ",
      "package root after pkgload::load_all().",
      call. = FALSE
    )
  }
  # ragg can be ABI-incompatible after R upgrades ("Version de l'API graphiques incompatible")
  options(shiny.useragg = FALSE)
  # Live smt_cmd() inbox lives under <pkg>/.smt — pin it before Shiny chdirs to inst/app
  if (nzchar(pkg_root)) {
    options(smt.cmd.root = file.path(normalizePath(pkg_root, winslash = "/", mustWork = FALSE), ".smt"))
  } else {
    found <- tryCatch(smt_find_pkg_root(), error = function(e) "")
    if (nzchar(found)) {
      options(smt.cmd.root = file.path(found, ".smt"))
    }
  }
  # Seed analysis data for this launch (NULL clears a previous seed in-process)
  options(smt.seed_data = smt_normalize_seed_data(data))
  # Console debug echo (file log is independent)
  options(smt.debug.console = isTRUE(debug))
  message(
    "Structural Model Tester: app is running. ",
    "Stop it from this R console with Esc (RStudio / Positron) or Ctrl+C (terminal)."
  )
  # TRUE → soft-fail launcher (missing browser must not abort shiny::runApp)
  lb <- if (isTRUE(launch.browser)) smt_launch_browser else launch.browser
  shiny::runApp(app_dir, launch.browser = lb, ...)
}

#' @keywords internal
get_dev_pkg_root <- function() {
  if (file.exists(file.path(getwd(), "DESCRIPTION"))) {
    desc <- tryCatch(read.dcf("DESCRIPTION")[, "Package"], error = function(e) "")
    if (identical(as.character(desc), "StructuralModelTester")) {
      return(normalizePath(getwd()))
    }
  }
  stop(
    "Package not installed. From the package root run: ",
    "devtools::load_all(); run_app()",
    call. = FALSE
  )
}
