# Shiny entry point for StructuralModelTester
suppressPackageStartupMessages({
  if (!requireNamespace("StructuralModelTester", quietly = TRUE)) {
    stop(
      "Package StructuralModelTester is not installed. ",
      "From the package root run: remotes::install_local('.', force = TRUE)",
      call. = FALSE
    )
  }
})

# Avoid ragg when its graphics ABI mismatches the running R
options(shiny.useragg = FALSE)

shiny::shinyApp(
  ui = StructuralModelTester:::smt_ui(),
  server = StructuralModelTester:::smt_server
)
