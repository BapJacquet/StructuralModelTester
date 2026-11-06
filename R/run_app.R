#' Launch the Structural Model Tester app
#'
#' Opens a Jamovi-inspired Shiny interface in the browser.
#'
#' @param launch.browser Logical; open in the system browser.
#' @param ... Passed to [shiny::runApp()].
#' @export
#' @examples
#' \dontrun{
#' StructuralModelTester::run_app()
#' }
run_app <- function(launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "StructuralModelTester")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    pkg_root <- Sys.getenv("SMT_PKG_ROOT", unset = "")
    if (!nzchar(pkg_root)) {
      pkg_root <- tryCatch(get_dev_pkg_root(), error = function(e) "")
    }
    app_dir <- file.path(pkg_root, "inst", "app")
  }
  if (!dir.exists(app_dir)) {
    stop(
      "Cannot find Shiny app directory. Install the package with ",
      "devtools::install() or remotes::install_local().",
      call. = FALSE
    )
  }
  # Ensure namespace is loaded so app.R can call package functions
  requireNamespace("StructuralModelTester", quietly = TRUE)
  # ragg can be ABI-incompatible after R upgrades ("Version de l'API graphiques incompatible")
  options(shiny.useragg = FALSE)
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}

#' @keywords internal
get_dev_pkg_root <- function() {
  if (file.exists(file.path(getwd(), "DESCRIPTION"))) {
    desc <- tryCatch(read.dcf("DESCRIPTION")[, "Package"], error = function(e) "")
    if (identical(as.character(desc), "StructuralModelTester")) return(normalizePath(getwd()))
  }
  # Walk up from this file when sourced via load_all
  path <- getwd()
  stop("Package not installed. From the package root run: devtools::load_all(); run_app()", call. = FALSE)
}
