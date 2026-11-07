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
