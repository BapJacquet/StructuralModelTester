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

#' Launch the Structural Model Tester app
#'
#' Opens the Structural Model Tester Shiny interface in the browser.
#'
#' @param launch.browser Logical; open in the system browser.
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
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
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
