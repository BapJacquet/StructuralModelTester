#' Resolve package sample_data directory (installed or source tree)
#' @keywords internal
smt_sample_data_dir <- function() {
  sys <- system.file("sample_data", package = "StructuralModelTester")
  if (nzchar(sys) && dir.exists(sys)) return(sys)
  pkg_root <- tryCatch(get_dev_pkg_root(), error = function(e) "")
  if (!nzchar(pkg_root)) {
    pkg_root <- tryCatch(smt_find_pkg_root(), error = function(e) "")
  }
  if (nzchar(pkg_root)) {
    cand <- file.path(pkg_root, "inst", "sample_data")
    if (dir.exists(cand)) return(cand)
  }
  ""
}

#' Path under sample_data, or "" if missing
#' @keywords internal
smt_sample_path <- function(...) {
  root <- smt_sample_data_dir()
  if (!nzchar(root)) return("")
  path <- file.path(root, ...)
  if (file.exists(path)) path else ""
}

#' Built-in example catalog for the welcome modal
#'
#' Each entry: `id`, `title`, `blurb`, `tags`, optional `data` / `model` filenames
#' under `inst/sample_data/`.
#' @keywords internal
smt_examples_catalog <- function() {
  list(
    list(
      id = "path",
      title = "Path model",
      blurb = "Continuous mediation with two exploratory covariates (X → M → Y).",
      tags = c("path", "mediation", "exploratory"),
      data = "path_demo.csv",
      model = "path_demo_model.json"
    ),
    list(
      id = "causal",
      title = "Causal chain",
      blurb = "X → A → B → C → Y with skip paths and exploratory distractors (true zeros).",
      tags = c("causal", "mediation", "exploratory"),
      data = "chain_skip_demo.csv",
      model = "chain_skip_demo_model.json"
    ),
    list(
      id = "interaction",
      title = "Interaction",
      blurb = "Gender × Method on Score, with Age as a confirmatory covariate.",
      tags = c("interaction", "binary factors"),
      data = "interaction_demo.csv",
      model = "interaction_demo_model.json"
    ),
    list(
      id = "repeated",
      title = "Repeated measures",
      blurb = "Within-subject Condition (5 trials each) → Score with lmer (1+Condition|subject), plus Age.",
      tags = c("lmer", "intra", "random slopes", "exact counts"),
      data = "repeated_demo.csv",
      model = "repeated_demo_model.json"
    ),
    list(
      id = "measurement",
      title = "Latent measurement",
      blurb = "Two latents predicting z — one unidimensional scale, one multi-factor mix (PCA screen).",
      tags = c("CFA", "factor scores", "unidimensionality"),
      data = "measurement_demo.csv",
      model = "measurement_demo_model.json"
    )
  )
}

#' Resolve one catalog entry to absolute paths (drops missing files)
#' @keywords internal
smt_example_resolved <- function(id) {
  cat <- smt_examples_catalog()
  ids <- vapply(cat, function(x) x$id, character(1))
  hit <- match(as.character(id), ids)
  if (is.na(hit)) return(NULL)
  ex <- cat[[hit]]
  data_path <- if (!is.null(ex$data)) smt_sample_path(ex$data) else ""
  model_path <- if (!is.null(ex$model)) smt_sample_path(ex$model) else ""
  ex$data_path <- data_path
  ex$model_path <- model_path
  ex
}

#' Welcome / examples modal UI
#' @keywords internal
smt_examples_modal <- function() {
  cards <- lapply(smt_examples_catalog(), function(ex) {
    tags <- paste(ex$tags, collapse = " · ")
    shiny::div(
      class = "smt-example-card",
      shiny::div(
        class = "smt-example-card-body",
        shiny::div(class = "smt-example-card-title", ex$title),
        shiny::div(class = "smt-example-card-blurb", ex$blurb),
        shiny::div(class = "smt-example-card-tags", tags)
      ),
      shiny::actionButton(
        paste0("smt_example_", ex$id),
        "Start with this",
        class = "smt-btn smt-btn-secondary smt-example-card-btn"
      )
    )
  })

  shiny::modalDialog(
    title = NULL,
    easyClose = TRUE,
    fade = TRUE,
    footer = shiny::tagList(
      shiny::actionButton(
        "smt_example_blank",
        "Start blank",
        class = "smt-btn smt-btn-ghost"
      ),
      shiny::modalButton("Close")
    ),
    size = "l",
    shiny::div(
      class = "smt-welcome-modal",
      shiny::div(
        class = "smt-welcome-hero",
        shiny::div(class = "smt-welcome-kicker", "Structural Model Tester"),
        shiny::h3("Choose a starting example"),
        shiny::p(
          class = "smt-welcome-lead",
          "No data is loaded until you pick an example — or start blank and open your own file."
        )
      ),
      shiny::div(class = "smt-example-grid", cards)
    )
  )
}
