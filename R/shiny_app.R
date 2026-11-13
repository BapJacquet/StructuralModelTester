#' Package version string for the UI chrome
#' @keywords internal
smt_package_version <- function() {
  v <- tryCatch(
    as.character(utils::packageVersion("StructuralModelTester")),
    error = function(e) NA_character_
  )
  if (is.na(v) || !nzchar(v)) {
    desc <- tryCatch({
      root <- get_dev_pkg_root()
      as.character(read.dcf(file.path(root, "DESCRIPTION"))[1, "Version"])
    }, error = function(e) NA_character_)
    if (!is.na(desc) && nzchar(desc)) v <- desc
  }
  if (is.na(v) || !nzchar(v)) v <- "?"
  v
}

#' Cache-bust query for www assets (mtime); avoids stale canvas JS in the browser
#' @keywords internal
smt_www_asset_query <- function(filename) {
  roots <- character()
  pkg_root <- tryCatch(get_dev_pkg_root(), error = function(e) "")
  if (nzchar(pkg_root)) {
    roots <- c(roots, file.path(pkg_root, "inst", "app", "www"))
  }
  sys <- system.file("app", "www", package = "StructuralModelTester")
  if (nzchar(sys)) roots <- c(roots, sys)
  for (root in roots) {
    path <- file.path(root, filename)
    if (file.exists(path)) {
      mt <- file.info(path)$mtime
      if (!is.na(mt)) {
        return(sprintf("?v=%s", format(as.numeric(mt), scientific = FALSE)))
      }
    }
  }
  sprintf("?v=%s", smt_package_version())
}

#' @keywords internal
smt_ui <- function() {
  shiny::fluidPage(
    title = "Structural Model Tester",
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet", type = "text/css",
        href = paste0("smt-theme.css", smt_www_asset_query("smt-theme.css"))
      ),
      shiny::tags$script(src = paste0("smt-panels.js", smt_www_asset_query("smt-panels.js"))),
      shiny::tags$script(src = paste0("smt-canvas.js", smt_www_asset_query("smt-canvas.js"))),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap"
      )
    ),
    shiny::div(
      class = "smt-app",
      shiny::div(
        class = "smt-topbar",
        shiny::div(
          class = "smt-topbar-left",
          shiny::div(
            class = "smt-brand",
            shiny::span(class = "smt-brand-mark", "SMT"),
            shiny::span(class = "smt-brand-name", "Structural Model Tester")
          ),
          shiny::div(
            class = "smt-view-toggle",
            shiny::radioButtons(
              "center_view", NULL,
              choiceNames = list("Data", "Prepare", "Model"),
              choiceValues = list("data", "prepare", "model"),
              selected = "model", inline = TRUE
            ),
            shiny::span(
              class = "smt-version",
              title = "StructuralModelTester package version",
              paste0("v", smt_package_version())
            )
          )
        ),
        shiny::div(
          class = "smt-top-actions",
          shiny::fileInput(
            "data_file", label = NULL, buttonLabel = "Open data",
            accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds"),
            placeholder = "CSV / Excel / RDS"
          ),
          shiny::fileInput("project_file", label = NULL, buttonLabel = "Open project",
                           accept = ".json", placeholder = "Project JSON"),
          shiny::downloadButton("save_project", "Save project", class = "smt-btn smt-btn-ghost"),
          shiny::actionButton("run_analysis", "Run", class = "smt-btn smt-btn-primary")
        )
      ),
      shiny::div(
        class = "smt-main",
        shiny::div(
          class = "smt-left",
          shiny::div(
            class = "smt-panel",
            shiny::h3("Variables"),
            shiny::uiOutput("variable_list"),
            shiny::hr(class = "smt-hr"),
            shiny::h3("Model tools"),
            shiny::textInput("new_node_label", "New node label", placeholder = "e.g. LatentF"),
            shiny::div(
              class = "smt-btn-row",
              shiny::actionButton("add_node", "Add node", class = "smt-btn smt-btn-secondary"),
              shiny::actionButton("add_latent", "Add latent", class = "smt-btn smt-btn-ghost")
            ),
            shiny::hr(class = "smt-hr"),
            shiny::h3("Analysis"),
            shiny::radioButtons(
              "inference_mode", "Framework",
              choices = c(
                "p-values" = "pvalue",
                "Model comparison (AIC/BIC)" = "search"
              ),
              selected = "pvalue"
            ),
            shiny::conditionalPanel(
              condition = "input.inference_mode == 'pvalue'",
              shiny::numericInput("alpha", "α for exploratory marking", value = 0.05, min = 0.001, max = 0.2, step = 0.01),
              shiny::checkboxInput("mark_nonsig", "Mark non-significant exploratory links", TRUE),
              shiny::selectInput(
                "conf_p_adjust",
                "Confirmatory multiplicity correction",
                choices = c(
                  "None" = "none",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Benjamini–Hochberg" = "BH"
                ),
                selected = "none",
                width = "100%"
              ),
              shiny::helpText(
                "Adjusts confirmatory link p-values across all confirmatory paths ",
                "(canvas / coefficients / power detection)."
              )
            ),
            shiny::conditionalPanel(
              condition = "input.inference_mode == 'search'",
              shiny::radioButtons("ic_criterion", "Criterion", choices = c("AIC" = "aic", "BIC" = "bic"), selected = "bic", inline = TRUE),
              shiny::numericInput(
                "min_weight",
                "Min Σw to keep",
                value = 0.95,
                min = 0,
                max = 1,
                step = 0.05
              ),
              shiny::checkboxInput(
                "mark_low_weight",
                "Reject links with Σw below threshold",
                value = TRUE
              ),
              shiny::helpText(
                "Sum of Akaike/BIC weights across models that include the link. ",
                "Below the threshold, confirmatory and exploratory links are dashed (✕) ",
                "and do not count as detected in power analysis. Default 0.95 is strict; ",
                "lower toward 0.50 for more permissive retention."
              ),
              shiny::checkboxInput(
                "model_average",
                "Model averaging (full)",
                value = FALSE
              ),
              shiny::helpText("Full averaging sets missing predictors to 0 across models; canvas shows averaged estimates.")
            ),
            shiny::hr(class = "smt-hr"),
            shiny::selectInput(
              "uncertainty_method",
              "Final-model uncertainty",
              choices = c(
                "None" = "none",
                "Bootstrap" = "bootstrap",
                "Plausible values" = "plausible"
              ),
              selected = "none",
              width = "100%"
            ),
            shiny::conditionalPanel(
              condition = "input.uncertainty_method != 'none'",
              shiny::numericInput(
                "uncertainty_draws",
                "Draws",
                value = 199,
                min = 20,
                max = 2000,
                step = 1
              ),
              shiny::helpText(
                "Applied only after the final model is chosen (not during AIC/BIC search). ",
                "Bootstrap resamples people; plausible values redraw latent scores then refit paths. ",
                "Inference is conditional on the selected model."
              )
            ),
            shiny::uiOutput("latent_score_method_ui"),
            shiny::hr(class = "smt-hr"),
            shiny::h3("Simulation"),
            shiny::numericInput(
              "sim_seed",
              "Seed",
              value = 1,
              min = -1,
              step = 1,
              width = "100%"
            ),
            shiny::helpText("Use −1 for a new random seed on each Simulate."),
            shiny::numericInput(
              "sim_n",
              "Samples",
              value = 500,
              min = 10,
              max = 100000,
              step = 10,
              width = "100%"
            ),
            shiny::helpText(
              "Rows when all factors are Inter. Subjects when any factor is Intra ",
              "(rows = subjects \u00d7 within-subject cells)."
            ),
            shiny::div(
              class = "smt-btn-row",
              shiny::actionButton(
                "run_simulation",
                "Simulate",
                class = "smt-btn smt-btn-secondary"
              )
            ),
            shiny::numericInput(
              "power_reps",
              "Power replications",
              value = 100,
              min = 10,
              max = 500,
              step = 10,
              width = "100%"
            ),
            shiny::numericInput(
              "power_interp_steps",
              "Interpolation steps",
              value = 3,
              min = 0,
              max = 20,
              step = 1,
              width = "100%"
            ),
            shiny::numericInput(
              "power_max_n",
              "Max sample (N cap)",
              value = 1000,
              min = 10,
              max = 100000,
              step = 50,
              width = "100%"
            ),
            shiny::helpText(
              "Power search evaluates several low N anchors plus min(Samples, Max sample), ",
              "then repeatedly curve-fits N* and resamples that point. Low-N coverage ",
              "stabilizes N* (especially for AIC/BIC). Evaluations never exceed Max sample."
            ),
            shiny::div(
              class = "smt-btn-row",
              shiny::actionButton(
                "run_power_analysis",
                "Run Power Analysis",
                class = "smt-btn smt-btn-secondary"
              ),
              shiny::actionButton(
                "cancel_power_analysis",
                "Cancel",
                class = "smt-btn smt-btn-danger",
                title = "Stop a running power analysis"
              )
            ),
            shiny::helpText(
              "Uses edge expected effects, exogenous mean/SD, and DV residual SD (\u03b5). ",
              "Simulate fits one sample; Power Analysis estimates P(all confirmatory links detected) ",
              "and, with confirmatory expected effects set to 0, the rate of detecting exactly k of them ",
              "(p < \u03b1, or in the best AIC/BIC model with Σw ≥ Min Σw). ",
              "Cancel stops after the current replication."
            )
          )
        ),
        shiny::div(class = "smt-splitter", `data-side` = "left", title = "Drag to resize"),
        shiny::div(
          class = "smt-center",
          shiny::conditionalPanel(
            condition = "input.center_view == 'data'",
            shiny::div(
              class = "smt-center-inner smt-data-wrap",
              shiny::h3(class = "smt-center-title", "Data"),
              shiny::helpText(
                "Raw loaded data (immutable). Long cells are truncated; hover for the full value. ",
                "Use Prepare to filter or mutate before modeling."
              ),
              shiny::div(
                class = "smt-data-load-opts",
                shiny::numericInput(
                  "data_skip_after_header",
                  "Skip rows after header",
                  value = 0, min = 0, max = 50, step = 1, width = "160px"
                ),
                shiny::textInput(
                  "data_excel_sheet",
                  "Excel sheet",
                  value = "1",
                  placeholder = "1 or sheet name",
                  width = "160px"
                ),
                shiny::actionButton(
                  "data_reload",
                  "Reload file",
                  class = "smt-btn smt-btn-secondary"
                )
              ),
              shiny::helpText(
                "SoSci Survey Excel/CSV exports: set skip to 1 to drop the column-description row. ",
                "Excel sheet accepts a 1-based index or sheet name."
              ),
              DT::DTOutput("data_table")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.center_view == 'prepare'",
            shiny::div(
              class = "smt-center-inner smt-prepare-wrap",
              shiny::h3(class = "smt-center-title", "Prepare"),
              shiny::div(
                class = "smt-prepare-toolbar",
                shiny::radioButtons(
                  "prepare_mode", NULL,
                  choiceNames = list("Steps", "Script"),
                  choiceValues = list("steps", "script"),
                  selected = "steps", inline = TRUE
                ),
                shiny::actionButton(
                  "prepare_apply", "Apply",
                  class = "smt-btn smt-btn-primary"
                ),
                shiny::span(class = "smt-prepare-status", shiny::textOutput("prepare_status", inline = TRUE))
              ),
              shiny::conditionalPanel(
                condition = "input.prepare_mode == 'steps'",
                shiny::div(
                  class = "smt-prepare-steps-panel",
                  shiny::div(
                    class = "smt-prepare-steps-actions",
                    shiny::actionButton(
                      "prepare_add_filter", "Add filter",
                      class = "smt-btn smt-btn-secondary"
                    ),
                    shiny::actionButton(
                      "prepare_add_mutate", "Add mutate",
                      class = "smt-btn smt-btn-secondary"
                    )
                  ),
                  shiny::helpText(
                    "Filter: logical expression (e.g. age > 18). ",
                    "Mutate: new_col = expression (e.g. log_y = log(Y)). Base R only."
                  ),
                  shiny::uiOutput("prepare_steps_ui")
                )
              ),
              shiny::conditionalPanel(
                condition = "input.prepare_mode == 'script'",
                shiny::div(
                  class = "smt-prepare-script-panel",
                  shiny::helpText(
                    "Script must return a data frame. Use `data` or `.` for the raw table. Base R only."
                  ),
                  shiny::textAreaInput(
                    "prepare_script",
                    label = NULL,
                    value = "",
                    rows = 10,
                    width = "100%",
                    placeholder = "subset(data, complete.cases(data))\n# or:\n# transform(data, log_y = log(Y))"
                  )
                )
              ),
              shiny::h4("Analysis preview"),
              DT::DTOutput("prepare_preview_table")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.center_view == 'model'",
            shiny::div(
              class = "smt-center-inner smt-canvas-wrap",
              shiny::div(
                class = "smt-canvas-toolbar",
                shiny::h3(class = "smt-center-title smt-canvas-toolbar-title", "Model"),
                shiny::div(
                  class = "smt-canvas-toolbar-actions",
                  shiny::actionButton(
                    "add_interaction", "Interaction",
                    class = "smt-btn smt-btn-secondary smt-canvas-ix-btn",
                    title = "Add interaction (×) node. Wire two predictors into ×, then drag × to a DV."
                  ),
                  shiny::div(
                    class = "smt-view-toggle smt-link-type-toggle",
                    shiny::radioButtons(
                      "link_type", NULL,
                      choiceNames = list(
                        "Fixed",
                        "Confirmatory",
                        "Exploratory",
                        "Measurement"
                      ),
                      choiceValues = list(
                        "fixed",
                        "confirmatory",
                        "exploratory",
                        "measurement"
                      ),
                      selected = "confirmatory",
                      inline = TRUE
                    )
                  )
                )
              ),
              shiny::div(
                class = "smt-graph-host",
                shiny::div(id = "smt_graph", class = "smt-graph")
              ),
              shiny::div(
                class = "smt-hsplitter",
                title = "Drag to resize plot panel"
              ),
              shiny::div(
                class = "smt-edge-plot-panel",
                shiny::div(
                  class = "smt-edge-plot-header",
                  shiny::h3(class = "smt-center-title smt-edge-plot-title", "Conditional / marginal effect"),
                  shiny::selectInput(
                    "edge_rel_kind",
                    label = NULL,
                    choices = c(
                      "Linear" = "linear",
                      "Quadratic" = "quadratic",
                      "Cubic" = "cubic",
                      "Smooth (cubic in model)" = "loess"
                    ),
                    selected = "linear",
                    width = "160px"
                  ),
                  shiny::div(
                    class = "smt-edge-prior-inline",
                    title = paste(
                      "Normal prior on the unstandardized coefficient b",
                      "(continuous: slope on the predictor's units; factors: contrast vs base).",
                      "Used for simulation; ignored by frequentist Run."
                    ),
                    shiny::numericInput(
                      "prior_mean_selected",
                      "Expected b",
                      value = 0,
                      step = 0.1,
                      width = "110px"
                    ),
                    shiny::numericInput(
                      "prior_sd_selected",
                      "Uncertainty (SD)",
                      value = 1,
                      min = 0.01,
                      step = 0.1,
                      width = "110px"
                    )
                  )
                ),
                shiny::div(
                  class = "smt-edge-plot-body",
                  shiny::plotOutput("edge_plot", height = "240px")
                )
              )
            )
          )
        ),
        shiny::div(class = "smt-splitter", `data-side` = "right", title = "Drag to resize"),
        shiny::div(
          class = "smt-right",
          shiny::div(
            class = "smt-panel smt-results",
            shiny::h3(shiny::textOutput("right_panel_title", inline = TRUE)),
            shiny::uiOutput("sim_results_banner"),
            shiny::uiOutput("status_msg"),
            shiny::uiOutput("inspector_panel"),
            # Residual plot must stay outside renderUI — remounting plotOutput
            # leaves a blank panel even when renderPlot succeeds.
            shiny::uiOutput("dv_resid_controls"),
            # Keep plotOutput in the DOM (static) so remounts do not blank it;
            # hide the host when there is nothing plottable for the selection.
            shiny::conditionalPanel(
              condition = "output.dv_resid_has_plot === true",
              shiny::div(
                class = "smt-resid-plot-wrap",
                id = "smt_resid_plot_host",
                shiny::plotOutput("dv_resid_plot", height = "320px")
              )
            ),
            shiny::h4("Power analysis"),
            shiny::uiOutput("power_recommend_msg"),
            shiny::conditionalPanel(
              condition = "output.power_has_plot === true",
              shiny::div(
                class = "smt-power-plot-wrap",
                shiny::plotOutput("power_plot", height = "280px")
              ),
              shiny::div(
                class = "smt-power-plot-wrap",
                shiny::plotOutput("power_fdr_plot", height = "280px")
              )
            ),
            shiny::h4("Path coefficients"),
            DT::DTOutput("coef_table"),
            shiny::h4("Best vs confirmatory"),
            shiny::helpText(
              "Confirmatory = best ∪ all confirmatory paths. ",
              "Confirmatory only = all confirmatory, no exploratory. ",
              "Null = no free paths; full = every free path."
            ),
            DT::DTOutput("best_vs_conf_table"),
            shiny::h4("All models"),
            DT::DTOutput("comparison_table"),
            shiny::h4("Model syntax"),
            shiny::verbatimTextOutput("syntax_out")
          )
        )
      )
    )
  )
}

#' @keywords internal
smt_server <- function(input, output, session) {
  state <- shiny::reactiveVal(empty_model_state())
  data_rv <- shiny::reactiveVal(NULL)
  last_data_file <- shiny::reactiveVal(NULL)
  prepare_rv <- shiny::reactiveVal(empty_prepare_state())
  analysis_rv <- shiny::reactiveVal(NULL)
  # Simulated generative data — plots only; never shown in Data / Prepare.
  sim_data_rv <- shiny::reactiveVal(NULL)
  # TRUE when last_result was fitted on simulated data (banner + plot fit gating).
  results_are_simulated <- shiny::reactiveVal(FALSE)
  # Monte Carlo confirmatory power curve (does not replace last_result).
  power_result_rv <- shiny::reactiveVal(NULL)
  prepare_status <- shiny::reactiveVal("")
  last_result <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("Load data, build a model, then Run.")
  skip_prepare_mode_write <- shiny::reactiveVal(FALSE)

  sample_path <- system.file("sample_data", "demo.csv", package = "StructuralModelTester")
  if (!nzchar(sample_path) || !file.exists(sample_path)) {
    dev_root <- tryCatch(get_dev_pkg_root(), error = function(e) "")
    if (nzchar(dev_root)) {
      sample_path <- file.path(dev_root, "inst", "sample_data", "demo.csv")
    }
  }

  set_raw_data <- function(dat, reset_prepare = TRUE) {
    data_rv(dat)
    if (isTRUE(reset_prepare)) {
      prepare_rv(empty_prepare_state())
      skip_prepare_mode_write(TRUE)
      shiny::updateRadioButtons(session, "prepare_mode", selected = "steps")
      shiny::updateTextAreaInput(session, "prepare_script", value = "")
    }
    analysis_rv(if (is.null(dat)) NULL else as.data.frame(dat, stringsAsFactors = FALSE))
    prepare_status(
      if (is.null(dat)) "" else sprintf("Using raw data (%d × %d).", nrow(dat), ncol(dat))
    )
  }

  invalidate_fit <- function(msg = NULL) {
    st <- clear_fit_results(state())
    state(st)
    if (!is.null(last_result())) last_result(NULL)
    results_are_simulated(FALSE)
    if (!is.null(msg)) status(msg)
  }

  # Collect current Prepare UI into prepare state (does not apply)
  sync_prepare_from_inputs <- function() {
    prep <- ensure_prepare_state(prepare_rv())
    mode <- input$prepare_mode %||% prep$mode
    if (!mode %in% c("steps", "script")) mode <- "steps"
    prep$mode <- mode
    prep$script <- as.character(input$prepare_script %||% prep$script %||% "")
    if (nrow(prep$steps)) {
      for (i in seq_len(nrow(prep$steps))) {
        sid <- prep$steps$id[i]
        typ <- input[[paste0("prep_type_", sid)]]
        expr <- input[[paste0("prep_expr_", sid)]]
        en <- input[[paste0("prep_en_", sid)]]
        if (!is.null(typ) && typ %in% c("filter", "mutate")) {
          prep$steps$type[i] <- typ
        }
        if (!is.null(expr)) prep$steps$expr[i] <- as.character(expr)
        if (!is.null(en)) prep$steps$enabled[i] <- isTRUE(en)
      }
    }
    prepare_rv(prep)
    prep
  }

  apply_prepare_now <- function() {
    raw <- data_rv()
    if (is.null(raw)) {
      prepare_status("Load data first.")
      return(invisible(FALSE))
    }
    prep <- sync_prepare_from_inputs()
    res <- try_apply_prepare(raw, prep)
    prepare_rv(res$prepare)
    if (!is.na(res$error) && nzchar(res$error)) {
      prepare_status(paste("Apply failed:", res$error))
      status(paste("Prepare error:", res$error))
      return(invisible(FALSE))
    }
    analysis_rv(res$data)
    n_raw <- nrow(raw)
    n_out <- nrow(res$data)
    n_cols <- ncol(res$data)
    prepare_status(sprintf(
      "Applied · %d → %d rows · %d columns.",
      n_raw, n_out, n_cols
    ))
    invalidate_fit("Prepare applied — re-run analysis to refresh estimates.")
    invisible(TRUE)
  }

  # Load demo data on startup if present
  shiny::observe({
    if (file.exists(sample_path) && is.null(data_rv())) {
      dat <- read_data_file(sample_path, "demo.csv")
      set_raw_data(dat, reset_prepare = TRUE)
      status("Demo data loaded. Map columns to nodes or open your own file.")
    }
  })

  load_uploaded_data <- function(f, announce = TRUE) {
    shiny::req(f)
    skip <- input$data_skip_after_header %||% 0
    sheet <- input$data_excel_sheet %||% "1"
    dat <- read_data_file(
      f$datapath, f$name,
      skip_after_header = skip,
      sheet = sheet
    )
    set_raw_data(dat, reset_prepare = TRUE)
    invalidate_fit()
    last_data_file(list(
      datapath = f$datapath,
      name = f$name
    ))
    if (isTRUE(announce)) {
      skip_n <- suppressWarnings(as.integer(skip))
      skip_txt <- if (is.finite(skip_n) && skip_n > 0L) {
        sprintf(" · skipped %d row(s) after header", skip_n)
      } else {
        ""
      }
      status(sprintf(
        "Loaded %s (%d rows × %d columns%s).",
        f$name, nrow(dat), ncol(dat), skip_txt
      ))
      shiny::updateRadioButtons(session, "center_view", selected = "data")
    }
    invisible(dat)
  }

  shiny::observeEvent(input$data_file, {
    f <- input$data_file
    shiny::req(f)
    tryCatch(
      load_uploaded_data(f, announce = TRUE),
      error = function(e) status(paste("Data error:", e$message))
    )
  })

  shiny::observeEvent(input$data_reload, {
    f <- last_data_file()
    if (is.null(f) || is.null(f$datapath) || !file.exists(f$datapath)) {
      status("Open a data file first, then use Reload file.")
      return()
    }
    tryCatch(
      load_uploaded_data(f, announce = TRUE),
      error = function(e) status(paste("Data error:", e$message))
    )
  })

  shiny::observeEvent(input$project_file, {
    f <- input$project_file
    shiny::req(f)
    tryCatch({
      smt_debug("project_file load start", list(
        name = f$name %||% NA_character_,
        size = f$size %||% NA_real_
      ))
      txt <- paste(readLines(f$datapath, warn = FALSE), collapse = "\n")
      st <- model_from_json(txt)
      prep <- attr(st, "prepare")
      if (is.null(prep)) prep <- prepare_from_project_json(txt)
      attr(st, "prepare") <- NULL
      state(st)
      prepare_rv(ensure_prepare_state(prep))
      skip_prepare_mode_write(TRUE)
      shiny::updateRadioButtons(session, "prepare_mode", selected = prep$mode %||% "steps")
      shiny::updateTextAreaInput(session, "prepare_script", value = prep$script %||% "")
      raw <- data_rv()
      if (!is.null(raw)) {
        res <- try_apply_prepare(raw, prep)
        prepare_rv(res$prepare)
        if (!is.na(res$error) && nzchar(res$error)) {
          analysis_rv(as.data.frame(raw, stringsAsFactors = FALSE))
          prepare_status(paste("Prepare restore failed:", res$error))
          status(paste("Project loaded; prepare error:", res$error))
        } else {
          analysis_rv(res$data)
          prepare_status(sprintf(
            "Prepare restored · %d → %d rows.",
            nrow(raw), nrow(res$data)
          ))
          status("Project loaded (prepare applied).")
        }
      } else {
        prepare_status("Project prepare loaded — open data to apply.")
        status("Project loaded.")
      }
      shiny::updateRadioButtons(session, "center_view", selected = "model")
      smt_debug("project_file load OK", list(
        n_nodes = nrow(st$nodes), n_edges = nrow(st$edges)
      ))
    }, error = function(e) {
      smt_debug("project_file load ERROR", list(message = e$message))
      status(paste("Project error:", e$message))
    })
  })

  flush_sim_var_kind_from_inputs <- function(st, nid, apply_levels = FALSE) {
    if (is.null(nid) || is_interaction_node(st, nid)) return(st)
    col <- st$nodes$observed_column[match(nid, st$nodes$id)]
    mapped <- !is.na(col) && nzchar(col) && !is.null(analysis_rv()) &&
      col %in% names(analysis_rv())
    if (mapped) return(st)
    kind_in <- input$sim_var_kind
    if (!is.null(kind_in) && nzchar(as.character(kind_in)) &&
          as.character(kind_in) %in% c("continuous", "categorical", "binary")) {
      st <- set_node_sim_var_kind(st, nid, as.character(kind_in))
    }
    kind <- node_sim_var_kind(st, nid, data = NULL)
    # Level renames are committed only via Apply (or Simulate/Run flush), not
    # on every keystroke — live updates remount ports and destabilize the UI.
    if (isTRUE(apply_levels) && kind %in% c("categorical", "binary")) {
      lev_txt <- input$sim_factor_levels
      if (!is.null(lev_txt) && nzchar(trimws(as.character(lev_txt)))) {
        levs <- decode_factor_levels(as.character(lev_txt))
        if (length(levs)) {
          st <- set_node_factor_coding(
            st, nid, base = levs[[1]], levels = levs, data = NULL
          )
        }
      }
    }
    st
  }

  flush_sim_params_from_inputs <- function(st, apply_levels = TRUE) {
    st <- ensure_node_sim_columns(st)
    nid <- shiny::isolate(selected_node_id())
    dv <- shiny::isolate(selected_dv_id())
    # Declared type for unmapped nodes (exogenous or DV) — shared inspector control
    if (!is.null(nid)) {
      st <- flush_sim_var_kind_from_inputs(st, nid, apply_levels = apply_levels)
    }
    if (!is.null(nid) && node_is_structural_exogenous(st, nid)) {
      kind <- node_sim_var_kind(st, nid, data = analysis_rv())
      if (kind %in% c("categorical", "binary")) {
        dm <- input$sim_design_mode
        if (!is.null(dm) && nzchar(as.character(dm)) &&
              as.character(dm) %in% c("inter", "intra")) {
          st <- set_node_sim_design_mode(st, nid, as.character(dm))
        }
        am <- input$sim_alloc_mode
        if (!is.null(am) && nzchar(as.character(am)) &&
              as.character(am) %in% c("probability", "exact")) {
          st <- set_node_sim_alloc_mode(st, nid, as.character(am))
        }
        levs <- node_sim_factor_levels(st, nid, data = analysis_rv())
        alloc <- node_sim_alloc_mode(st, nid)
        if (identical(alloc, "exact")) {
          counts <- integer(length(levs))
          names(counts) <- levs
          for (i in seq_along(levs)) {
            v <- input[[paste0("sim_count_", i)]]
            if (!is.null(v) && length(v) == 1L && is.finite(v)) {
              counts[[i]] <- as.integer(max(0L, round(as.numeric(v))))
            } else {
              counts[[i]] <- 1L
            }
          }
          if (length(counts)) st <- set_node_sim_level_counts(st, nid, counts)
        } else {
          props <- numeric(length(levs))
          names(props) <- levs
          for (i in seq_along(levs)) {
            v <- input[[paste0("sim_prop_", i)]]
            if (!is.null(v) && length(v) == 1L && is.finite(v)) props[[i]] <- as.numeric(v)
          }
          if (length(props)) st <- set_node_sim_level_props(st, nid, props)
        }
      } else {
        m <- input$sim_exog_mean
        s <- input$sim_exog_sd
        if (!is.null(m) && !is.null(s) && length(m) == 1L && length(s) == 1L &&
              is.finite(m) && is.finite(s) && s > 0) {
          st <- set_node_sim_exogenous(st, nid, mean = m, sd = s)
        }
        lo <- input$sim_exog_min
        hi <- input$sim_exog_max
        # NULL = input not mounted; leave stored bounds. NA/blank clears a bound.
        if (!is.null(lo) || !is.null(hi)) {
          cur <- node_sim_params(st, nid)
          lo_v <- if (is.null(lo) || length(lo) != 1L || is.na(lo) || !is.finite(lo)) {
            NA_real_
          } else {
            as.numeric(lo)
          }
          hi_v <- if (is.null(hi) || length(hi) != 1L || is.na(hi) || !is.finite(hi)) {
            NA_real_
          } else {
            as.numeric(hi)
          }
          # If only one input is present this flush, keep the other from state
          if (is.null(lo)) lo_v <- cur$min
          if (is.null(hi)) hi_v <- cur$max
          st <- set_node_sim_bounds(st, nid, min = lo_v, max = hi_v)
        }
      }
    }
    if (!is.null(dv)) {
      icept <- input$sim_dv_intercept
      if (!is.null(icept) && length(icept) == 1L && is.finite(icept)) {
        st <- set_node_sim_intercept(st, dv, intercept = icept)
      }
      r <- input$sim_resid_sd
      if (!is.null(r) && length(r) == 1L && is.finite(r) && r > 0) {
        st <- set_node_sim_resid_sd(st, dv, resid_sd = r)
      }
      lo <- input$sim_dv_min
      hi <- input$sim_dv_max
      if (!is.null(lo) || !is.null(hi)) {
        cur <- node_sim_params(st, dv)
        lo_v <- if (is.null(lo) || length(lo) != 1L || is.na(lo) || !is.finite(lo)) {
          NA_real_
        } else {
          as.numeric(lo)
        }
        hi_v <- if (is.null(hi) || length(hi) != 1L || is.na(hi) || !is.finite(hi)) {
          NA_real_
        } else {
          as.numeric(hi)
        }
        if (is.null(lo)) lo_v <- cur$min
        if (is.null(hi)) hi_v <- cur$max
        st <- set_node_sim_bounds(st, dv, min = lo_v, max = hi_v)
      }
    }
    st
  }

  output$save_project <- shiny::downloadHandler(
    filename = function() paste0("structural-model-", Sys.Date(), ".json"),
    content = function(file) {
      prep <- tryCatch(sync_prepare_from_inputs(), error = function(e) prepare_rv())
      st <- merge_edge_priors_from_inputs(state())$state
      st <- flush_sim_params_from_inputs(st)
      state(st)
      writeLines(model_to_json(st, prepare = prep), file)
    }
  )

  shiny::observeEvent(input$prepare_mode, {
    if (isTRUE(skip_prepare_mode_write())) {
      skip_prepare_mode_write(FALSE)
      return()
    }
    prep <- ensure_prepare_state(prepare_rv())
    mode <- input$prepare_mode %||% "steps"
    if (!identical(prep$mode, mode)) {
      prep$mode <- mode
      prepare_rv(prep)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$prepare_add_filter, {
    sync_prepare_from_inputs()
    prepare_rv(add_prepare_step(prepare_rv(), type = "filter", expr = ""))
  })

  shiny::observeEvent(input$prepare_add_mutate, {
    sync_prepare_from_inputs()
    prepare_rv(add_prepare_step(prepare_rv(), type = "mutate", expr = ""))
  })

  shiny::observeEvent(input$prep_remove, {
    sid <- input$prep_remove
    shiny::req(sid)
    sync_prepare_from_inputs()
    prepare_rv(remove_prepare_step(prepare_rv(), sid))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$prep_move_up, {
    sid <- input$prep_move_up
    shiny::req(sid)
    sync_prepare_from_inputs()
    prepare_rv(move_prepare_step(prepare_rv(), sid, "up"))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$prep_move_down, {
    sid <- input$prep_move_down
    shiny::req(sid)
    sync_prepare_from_inputs()
    prepare_rv(move_prepare_step(prepare_rv(), sid, "down"))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$prepare_apply, {
    apply_prepare_now()
  })

  output$prepare_status <- shiny::renderText(prepare_status())

  output$prepare_steps_ui <- shiny::renderUI({
    prep <- ensure_prepare_state(prepare_rv())
    if (!nrow(prep$steps)) {
      return(shiny::div(class = "smt-muted", "No steps yet. Add a filter or mutate."))
    }
    shiny::tagList(lapply(seq_len(nrow(prep$steps)), function(i) {
      sid <- prep$steps$id[i]
      shiny::div(
        class = "smt-prepare-step",
        shiny::checkboxInput(
          paste0("prep_en_", sid),
          label = NULL,
          value = isTRUE(prep$steps$enabled[i])
        ),
        shiny::selectInput(
          paste0("prep_type_", sid),
          label = NULL,
          choices = c("Filter" = "filter", "Mutate" = "mutate"),
          selected = prep$steps$type[i],
          width = "110px"
        ),
        shiny::textInput(
          paste0("prep_expr_", sid),
          label = NULL,
          value = prep$steps$expr[i],
          width = "100%",
          placeholder = if (identical(prep$steps$type[i], "mutate")) {
            "new_col = expression"
          } else {
            "logical expression"
          }
        ),
        shiny::div(
          class = "smt-prepare-step-btns",
          shiny::tags$button(
            type = "button", class = "smt-btn smt-btn-ghost smt-prepare-icon-btn",
            title = "Move up", "\u2191",
            onclick = sprintf(
              "Shiny.setInputValue('prep_move_up', '%s', {priority: 'event'});",
              sid
            )
          ),
          shiny::tags$button(
            type = "button", class = "smt-btn smt-btn-ghost smt-prepare-icon-btn",
            title = "Move down", "\u2193",
            onclick = sprintf(
              "Shiny.setInputValue('prep_move_down', '%s', {priority: 'event'});",
              sid
            )
          ),
          shiny::tags$button(
            type = "button", class = "smt-btn smt-btn-danger smt-prepare-icon-btn",
            title = "Remove", "\u00D7",
            onclick = sprintf(
              "Shiny.setInputValue('prep_remove', '%s', {priority: 'event'});",
              sid
            )
          )
        )
      )
    }))
  })

  output$prepare_preview_table <- DT::renderDT({
    dat <- analysis_rv()
    shiny::req(dat)
    smt_browse_datatable(dat, page_length = 12L, max_chars = 24L)
  })

  output$variable_list <- shiny::renderUI({
    dat <- analysis_rv()
    st <- state()
    vars <- variable_list_from_data(dat, st)
    if (!nrow(vars)) {
      return(shiny::div(class = "smt-muted", "No data loaded."))
    }
    shiny::tags$ul(
      class = "smt-var-list",
      lapply(seq_len(nrow(vars)), function(i) {
        col <- vars$column[i]
        mapped <- vars$mapped_to[i]
        icon <- if (vars$type[i] == "continuous") "●" else "■"
        badge <- if (!is.na(mapped)) shiny::span(class = "smt-badge", "mapped") else NULL
        shiny::tags$li(
          class = "smt-var-item",
          shiny::span(class = "smt-var-icon", icon),
          shiny::span(class = "smt-var-name", col),
          badge,
          shiny::actionLink(
            paste0("add_var_", i),
            label = if (is.na(mapped)) "＋" else "↻",
            class = "smt-var-add",
            onclick = sprintf(
              "Shiny.setInputValue('add_variable_col', '%s', {priority: 'event'}); return false;",
              col
            )
          )
        )
      })
    )
  })

  output$latent_score_method_ui <- shiny::renderUI({
    st <- state()
    if (n_latent_nodes(st) < 2L) return(NULL)
    cur <- shiny::isolate(input$latent_score_method)
    if (is.null(cur) || !cur %in% c("own", "joint")) cur <- "own"
    shiny::tagList(
      shiny::hr(class = "smt-hr"),
      shiny::radioButtons(
        "latent_score_method",
        "Latent scores",
        choices = c(
          "Own indicators" = "own",
          "Joint (Thomson)" = "joint"
        ),
        selected = cur,
        width = "100%"
      ),
      shiny::helpText(
        "Analysis-wide. Own indicators (default) score each latent from its ",
        "measurement links only. Joint Thomson/regression scores use the ",
        "multi-factor CFA and can weight items from other constructs when ",
        "factors correlate. With one latent both coincide."
      )
    )
  })

  shiny::observeEvent(input$add_variable_col, {
    col <- input$add_variable_col
    shiny::req(col)
    st <- state()
    # If a node already mapped to this column exists, skip; else add observed node
    existing <- st$nodes$id[!is.na(st$nodes$observed_column) & st$nodes$observed_column == col]
    if (length(existing)) {
      status(sprintf("'%s' is already a node.", col))
      return()
    }
    st <- add_node(st, label = col, observed_column = col)
    state(st)
    status(sprintf("Added observed node '%s'.", col))
    shiny::updateRadioButtons(session, "center_view", selected = "model")
  })

  shiny::observeEvent(input$add_node, {
    label <- input$new_node_label
    if (is.null(label) || !nzchar(trimws(label))) label <- paste0("Node", state()$next_node)
    st <- add_node(
      state(),
      label = trimws(label),
      observed_column = NA_character_,
      kind = "observed"
    )
    state(st)
    shiny::updateTextInput(session, "new_node_label", value = "")
    status(sprintf("Added observed node '%s' (map a data column when ready).", trimws(label)))
    shiny::updateRadioButtons(session, "center_view", selected = "model")
  })

  shiny::observeEvent(input$add_latent, {
    label <- input$new_node_label
    if (is.null(label) || !nzchar(trimws(label))) label <- paste0("F", state()$next_node)
    st <- add_node(
      state(),
      label = trimws(label),
      observed_column = NA_character_,
      kind = "latent"
    )
    state(st)
    shiny::updateTextInput(session, "new_node_label", value = "")
    status(sprintf("Added latent '%s'. Connect measurement links to indicators.", trimws(label)))
    shiny::updateRadioButtons(session, "center_view", selected = "model")
  })

  shiny::observeEvent(input$add_interaction, {
    st <- add_interaction_node(state())
    state(clear_fit_results(st))
    status("Added × node. Wire two predictors into its inputs, then connect × to a DV.")
    shiny::updateRadioButtons(session, "center_view", selected = "model")
  })

  # Drag-to-create link (custom JS → smt_new_edge)
  shiny::observeEvent(input$smt_new_edge, {
    edge <- input$smt_new_edge
    shiny::req(edge)
    from <- as.character(edge$from)
    to <- as.character(edge$to)
    level <- edge$level
    if (is.null(level) || !nzchar(as.character(level))) level <- NA_character_
    level <- as.character(level)
    to_port <- edge$toPort
    if (is.null(to_port) || !nzchar(as.character(to_port))) to_port <- NA_character_
    to_port <- as.character(to_port)
    to_level <- edge$toLevel
    if (is.null(to_level) || !nzchar(as.character(to_level))) to_level <- NA_character_
    to_level <- as.character(to_level)
    if (!nzchar(from) || !nzchar(to) || identical(from, to)) return()
    link_type <- input$link_type %||% "confirmatory"
    before <- nrow(state()$edges)
    st <- add_edge(
      state(), from, to, type = link_type, level = level,
      to_port = to_port, to_level = to_level
    )
    state(clear_fit_results(st))
    if (nrow(st$edges) > before) {
      if (!is.na(to_port) && nzchar(to_port)) {
        status(sprintf("Wired %s → × port %s.", from, to_port))
      } else {
        lvl_txt <- if (!is.na(level) && nzchar(level)) paste0(" [", level, "]") else ""
        status(sprintf("Added %s link %s%s → %s.", link_type, from, lvl_txt, to))
      }
    } else {
      status("Link already exists (or invalid).")
    }
  }, ignoreInit = TRUE)

  # Persist node coordinates after move without rebuilding the HTML graph.
  # Top/left canvas growth may shift every node, so prefer the full positions list.
  # Layout-only updates must not clear fit results: remounting the inspector can
  # re-fire mapping/coding inputs, so freeze those values for this flush.
  skip_graph_render <- shiny::reactiveVal(FALSE)
  # Remounted inspector inputs arrive after freezeReactiveValue expires; ignore
  # writebacks briefly after selection / move / fit so they cannot clear results.
  suppress_inspector_writes <- shiny::reactiveVal(FALSE)
  suppress_deadline <- shiny::reactiveVal(0)
  begin_inspector_suppress <- function(ms = 700) {
    suppress_inspector_writes(TRUE)
    suppress_deadline(as.numeric(Sys.time()) + ms / 1000)
  }
  inspector_writes_allowed <- function() {
    !isTRUE(shiny::isolate(suppress_inspector_writes()))
  }
  shiny::observe({
    if (!isTRUE(suppress_inspector_writes())) return()
    shiny::invalidateLater(100, session)
    if (as.numeric(Sys.time()) >= shiny::isolate(suppress_deadline())) {
      suppress_inspector_writes(FALSE)
    }
  })
  freeze_inspector_inputs <- function() {
    for (nm in c(
      "inspector_map_column",
      "dv_transform",
      "dv_repeated_measures",
      "dv_cluster_column",
      "cat_factor_base",
      "cat_factor_ordered",
      "cat_factor_levels",
      "sim_exog_mean",
      "sim_exog_sd",
      "sim_exog_min",
      "sim_exog_max",
      "sim_resid_sd",
      "sim_dv_intercept",
      "sim_dv_min",
      "sim_dv_max",
      "sim_var_kind",
      "sim_factor_levels",
      "sim_design_mode",
      "sim_alloc_mode",
      paste0("sim_prop_", seq_len(12L)),
      paste0("sim_count_", seq_len(12L))
      # Do NOT freeze edge_rel_kind or dv_diag_equation — residual / edge
      # renderPlot reads them; freezing silently aborts the plot flush and
      # leaves a blank panel after a node or edge click.
    )) {
      try(shiny::freezeReactiveValue(input, nm), silent = TRUE)
    }
  }

  # Bumped on every canvas selection so edge_plot always re-executes.
  edge_plot_tick <- shiny::reactiveVal(0L)
  resid_plot_tick <- shiny::reactiveVal(0L)

  selected_structural_edge_id <- shiny::reactive({
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (is.null(eid) || !nzchar(eid)) return(NULL)
    prior_ids <- structural_prior_edges(state())$id
    if (!eid %in% prior_ids) return(NULL)
    eid
  })

  # Flush Link-relationship prior fields into the selected structural edge.
  # Never clears fit results — priors are storage-only for later simulation / Bayes.
  merge_edge_priors_from_inputs <- function(st, edge_id = NULL) {
    st <- ensure_edge_priors(st)
    if (!nrow(st$edges)) return(list(state = st, changed = FALSE))
    eid <- edge_id
    if (is.null(eid)) eid <- shiny::isolate(selected_structural_edge_id())
    if (is.null(eid)) return(list(state = st, changed = FALSE))
    m <- input$prior_mean_selected
    s <- input$prior_sd_selected
    if (is.null(m) || is.null(s) || length(m) != 1L || length(s) != 1L) {
      return(list(state = st, changed = FALSE))
    }
    if (is.na(m) || is.na(s)) return(list(state = st, changed = FALSE))
    cur <- edge_prior(st, eid)
    if (isTRUE(all.equal(cur$mean, as.numeric(m), tolerance = 1e-9)) &&
          isTRUE(all.equal(cur$sd, as.numeric(s), tolerance = 1e-9))) {
      return(list(state = st, changed = FALSE))
    }
    st <- set_edge_prior(st, eid, mean = m, sd = s)
    list(state = st, changed = TRUE)
  }

  # How many programmatic prior updates to ignore (mean + sd = 2).
  skip_prior_writes_left <- shiny::reactiveVal(0L)
  last_prior_edge_id <- shiny::reactiveVal(NULL)

  shiny::observeEvent(selected_structural_edge_id(), {
    new_eid <- selected_structural_edge_id()
    old_eid <- shiny::isolate(last_prior_edge_id())
    # Persist edits for the edge that was shown before this selection change
    if (!is.null(old_eid) && !identical(old_eid, new_eid)) {
      m <- input$prior_mean_selected
      s <- input$prior_sd_selected
      if (!is.null(m) && !is.null(s) && length(m) == 1L && length(s) == 1L &&
            is.finite(m) && is.finite(s)) {
        st0 <- state()
        cur <- edge_prior(st0, old_eid)
        if (!isTRUE(all.equal(cur$mean, as.numeric(m), tolerance = 1e-9)) ||
              !isTRUE(all.equal(cur$sd, as.numeric(s), tolerance = 1e-9))) {
          skip_graph_render(TRUE)
          state(set_edge_prior(st0, old_eid, mean = m, sd = s))
        }
      }
    }
    last_prior_edge_id(new_eid)
    if (is.null(new_eid)) {
      skip_prior_writes_left(2L)
      shiny::updateNumericInput(session, "prior_mean_selected", value = 0)
      shiny::updateNumericInput(session, "prior_sd_selected", value = 1)
      return()
    }
    pr <- edge_prior(state(), new_eid)
    skip_prior_writes_left(2L)
    shiny::updateNumericInput(session, "prior_mean_selected", value = pr$mean)
    shiny::updateNumericInput(session, "prior_sd_selected", value = pr$sd)
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$prior_mean_selected, {
    left <- shiny::isolate(skip_prior_writes_left())
    if (left > 0L) {
      skip_prior_writes_left(left - 1L)
      return()
    }
    if (!inspector_writes_allowed()) return()
    eid <- selected_structural_edge_id()
    if (is.null(eid)) return()
    merged <- merge_edge_priors_from_inputs(state(), edge_id = eid)
    if (!isTRUE(merged$changed)) return()
    skip_graph_render(TRUE)
    state(merged$state)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$prior_sd_selected, {
    left <- shiny::isolate(skip_prior_writes_left())
    if (left > 0L) {
      skip_prior_writes_left(left - 1L)
      return()
    }
    if (!inspector_writes_allowed()) return()
    eid <- selected_structural_edge_id()
    if (is.null(eid)) return()
    merged <- merge_edge_priors_from_inputs(state(), edge_id = eid)
    if (!isTRUE(merged$changed)) return()
    skip_graph_render(TRUE)
    state(merged$state)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$smt_node_moved, {
    mv <- input$smt_node_moved
    shiny::req(mv)
    st <- state()
    if (!is.null(mv$positions) && length(mv$positions)) {
      for (p in mv$positions) {
        if (is.null(p$id)) next
        st <- apply_node_position(st, p$id, p$x, p$y)
      }
    } else {
      shiny::req(mv$id)
      st <- apply_node_position(st, mv$id, mv$x, mv$y)
    }
    begin_inspector_suppress()
    freeze_inspector_inputs()
    skip_graph_render(TRUE)
    state(st)
  }, ignoreInit = TRUE)

  # Push graph to HTML editor when model structure / results / data change.
  # Read/clear skip with isolate so toggling it does not schedule a second render
  # (which would rebuild the canvas and remount the inspector after a drag).
  # If skip was set for a layout/prior write but structure also changed in the
  # same flush (new edge, binary ports, …), force a redraw anyway.
  last_graph_struct_sig <- shiny::reactiveVal("")
  shiny::observe({
    st <- state()
    dat <- analysis_rv()
    payload <- graph_payload(st, dat)
    sig <- graph_structure_signature(payload)
    skip <- isTRUE(shiny::isolate(skip_graph_render()))
    prev_sig <- shiny::isolate(last_graph_struct_sig())
    struct_changed <- !identical(sig, prev_sig)
    if (skip && !struct_changed) {
      smt_debug(
        "R graph observe SKIPPED render (layout-only) | n_edges=", nrow(st$edges),
        " has_result=", !is.null(shiny::isolate(last_result()))
      )
      shiny::isolate(skip_graph_render(FALSE))
      return()
    }
    if (skip && struct_changed) {
      smt_debug(
        "R graph observe FORCE render (structure changed despite skip) | n_nodes=",
        nrow(st$nodes), " n_edges=", nrow(st$edges)
      )
      shiny::isolate(skip_graph_render(FALSE))
    } else {
      smt_debug(
        "R graph observe SEND render | n_nodes=", nrow(st$nodes),
        " n_edges=", nrow(st$edges)
      )
    }
    last_graph_struct_sig(sig)
    session$sendCustomMessage("smt_render_graph", payload)
  })

  delete_current_selection <- function() {
    sel <- input$graph_selected
    st <- state()
    n_edges <- if (!is.null(sel$edges)) length(sel$edges) else 0L
    n_nodes <- if (!is.null(sel$nodes)) length(sel$nodes) else 0L
    if (!n_edges && !n_nodes) return()
    if (n_edges) {
      for (eid in sel$edges) st <- remove_edge(st, eid)
    }
    if (n_nodes) {
      for (nid in sel$nodes) st <- remove_node(st, nid)
    }
    state(clear_fit_results(st))
    status("Deleted selection.")
  }

  shiny::observeEvent(input$smt_delete_selection, {
    delete_current_selection()
  }, ignoreInit = TRUE)

  # Sync relationship-kind control from the selected edge.
  # Selecting a node remounts the inspector; freeze its inputs for this flush so
  # spurious select/checkbox events cannot clear last_result / fit estimates.
  skip_rel_kind_write <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$graph_selected, {
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    nids <- if (!is.null(sel$nodes) && length(sel$nodes)) {
      paste(as.character(unlist(sel$nodes)), collapse = ",")
    } else {
      ""
    }
    tick <- shiny::isolate(edge_plot_tick()) + 1L
    edge_plot_tick(tick)
    resid_plot_tick(shiny::isolate(resid_plot_tick()) + 1L)
    smt_debug(
      "R graph_selected | edges=", smt_debug_fmt(sel$edges),
      " eid=", eid %||% "NULL",
      " nodes=[", nids, "]",
      " has_result=", !is.null(last_result()),
      " suppress=", isTRUE(shiny::isolate(suppress_inspector_writes())),
      " edge_plot_tick=", tick
    )
    begin_inspector_suppress()
    freeze_inspector_inputs()
    # Only sync the relationship dropdown when a structural edge is selected.
    # Syncing on node clicks (defaulting to "linear") can re-fire the writer and
    # clear fit results / remount the canvas so the edge plot never appears.
    if (is.null(eid) || !nzchar(eid)) return()
    st <- ensure_edge_to_port(state())
    if (!eid %in% st$edges$id) {
      smt_debug("R graph_selected | eid not in state$edges$id")
      return()
    }
    rk <- st$edges$rel_kind[match(eid, st$edges$id)]
    if (is.na(rk) || !nzchar(rk)) rk <- "linear"
    current <- as.character(input$edge_rel_kind %||% "")
    if (!identical(current, as.character(rk))) {
      smt_debug("R graph_selected | sync edge_rel_kind ", current, " -> ", rk)
      skip_rel_kind_write(TRUE)
      shiny::updateSelectInput(session, "edge_rel_kind", selected = rk)
    }
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$smt_debug_event, {
    ev <- input$smt_debug_event
    if (is.null(ev)) return()
    smt_debug(
      "JS ", as.character(ev$kind %||% "?"),
      " | ", smt_debug_fmt(ev)
    )
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$edge_rel_kind, {
    if (isTRUE(skip_rel_kind_write())) {
      skip_rel_kind_write(FALSE)
      return()
    }
    if (!inspector_writes_allowed()) return()
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (is.null(eid) || !nzchar(eid)) return()
    rk <- as.character(input$edge_rel_kind %||% "linear")
    st0 <- ensure_edge_to_port(state())
    if (!eid %in% st0$edges$id) return()
    cur <- st0$edges$rel_kind[match(eid, st0$edges$id)]
    if (is.na(cur) || !nzchar(cur)) cur <- "linear"
    if (identical(as.character(cur), rk)) return()
    skip_graph_render(TRUE)
    st <- set_edge_rel_kind(st0, eid, rk)
    state(clear_fit_results(st))
    if (!is.null(last_result())) {
      last_result(NULL)
      status("Relationship kind updated — re-run analysis to refresh coefficients.")
    }
  }, ignoreInit = TRUE)

  # Categorical split/omnibus toggle on the variable node (canvas)
  shiny::observeEvent(input$smt_node_cat_mode, {
    msg <- input$smt_node_cat_mode
    shiny::req(msg, msg$id, msg$mode)
    mode <- as.character(msg$mode)
    if (!mode %in% c("split", "omnibus")) return()
    st <- set_node_cat_mode(state(), as.character(msg$id), mode, data = analysis_rv())
    state(clear_fit_results(st))
    if (!is.null(last_result())) {
      last_result(NULL)
      status("Categorical coding updated — re-run analysis to refresh coefficients.")
    } else {
      status(
        if (identical(mode, "omnibus")) {
          "Omnibus: one joint test for all levels."
        } else {
          "Split: one effect per level."
        }
      )
    }
  }, ignoreInit = TRUE)

  output$edge_plot <- shiny::renderPlot({
    tick <- edge_plot_tick()
    sel <- shiny::isolate(input$graph_selected)
    eid <- NULL
    if (!is.null(sel$edges) && length(sel$edges)) {
      eid <- as.character(sel$edges[[1]])
    }
    smt_debug(
      "R edge_plot render | tick=", tick,
      " eid=", eid %||% "NULL",
      " sel_edges=", smt_debug_fmt(sel$edges),
      " has_data=", !is.null(shiny::isolate(analysis_rv())),
      " has_result=", !is.null(shiny::isolate(last_result())),
      " rel_kind_input=", as.character(shiny::isolate(input$edge_rel_kind) %||% "NULL")
    )
    if (is.null(eid) || !nzchar(eid)) {
      smt_debug("R edge_plot -> placeholder (no edge selected)")
      return(edge_plot_placeholder())
    }
    analysis_dat <- shiny::isolate(analysis_rv())
    sim_dat <- sim_data_rv()
    st <- ensure_edge_to_port(shiny::isolate(state()))
    idx <- match(eid, st$edges$id)
    if (is.na(idx)) {
      smt_debug("R edge_plot -> placeholder (eid not in edges: ", eid, ")")
      return(edge_plot_placeholder())
    }
    from_id <- st$edges$from[idx]
    to_id <- st$edges$to[idx]
    need_ids <- if (is_interaction_node(st, from_id)) {
      fac <- interaction_factors(st, from_id)
      c(fac$a, fac$b, to_id)
    } else {
      c(from_id, to_id)
    }
    has_result <- !is.null(shiny::isolate(last_result()))
    picked <- pick_plot_data_for_nodes(
      st, analysis_dat, sim_dat, need_ids, has_result = has_result
    )
    if (is.null(picked$data)) {
      smt_debug(
        "R edge_plot -> no data/sim | has_result=", has_result,
        " | ", picked$reason %||% ""
      )
      return(edge_plot_message(
        picked$title %||% "No data",
        picked$reason %||% "Load data or run Simulate to plot this link."
      ))
    }
    dat <- picked$data
    simulated <- isTRUE(picked$simulated)
    rk <- "linear"
    stored <- st$edges$rel_kind[idx]
    if (!is.na(stored) && nzchar(stored)) rk <- stored
    rk_in <- shiny::isolate(input$edge_rel_kind)
    if (!is.null(rk_in) && nzchar(as.character(rk_in))) {
      rk <- as.character(rk_in)
    }
    # Fit overlay only when plot data source matches the fitted result source
    res_sim <- isTRUE(shiny::isolate(results_are_simulated()))
    show_fit <- !is.null(shiny::isolate(last_result())) &&
      identical(isTRUE(simulated), res_sim)
    # Live Expected effect / Uncertainty from the link panel
    pr <- edge_prior(st, eid)
    prior_mean <- pr$mean
    prior_sd <- pr$sd
    pm_in <- input$prior_mean_selected
    ps_in <- input$prior_sd_selected
    if (!is.null(pm_in) && length(pm_in) == 1L && is.finite(pm_in)) {
      prior_mean <- as.numeric(pm_in)
    }
    if (!is.null(ps_in) && length(ps_in) == 1L && is.finite(ps_in) && ps_in > 0) {
      prior_sd <- as.numeric(ps_in)
    }
    smt_debug(
      "R edge_plot building | from=", from_id,
      " to=", to_id,
      " type=", st$edges$type[idx],
      " rk=", rk, " show_fit=", show_fit,
      " simulated=", simulated,
      " prior_mean=", prior_mean, " prior_sd=", prior_sd,
      " nrow_data=", nrow(dat)
    )
    score_meth <- shiny::isolate({
      res0 <- last_result()
      normalize_latent_score_method(
        res0$latent_score_method %||% input$latent_score_method %||% "own",
        st
      )
    })
    p <- tryCatch(
      edge_relationship_plot(
        st, dat, eid,
        show_fit = show_fit,
        rel_kind = rk,
        prior_mean = prior_mean,
        prior_sd = prior_sd,
        simulated = simulated,
        latent_score_method = score_meth
      ),
      error = function(e) {
        smt_debug("R edge_plot ERROR: ", e$message)
        edge_plot_message("Plot error", e$message)
      }
    )
    if (is.null(p)) {
      smt_debug("R edge_plot -> placeholder (plot fn returned NULL)")
      return(edge_plot_placeholder())
    }
    smt_debug("R edge_plot -> ggplot OK | class=", paste(class(p), collapse = ","))
    p
  })
  shiny::outputOptions(output, "edge_plot", suspendWhenHidden = FALSE)

  # Shared fit pipeline for Run (analysis data) and Simulate (generated data).
  run_fit_pipeline <- function(st, dat, simulated = FALSE,
                               progress_message = "Running analysis…",
                               sim_seed_used = NULL) {
    if (is.null(dat) || !nrow(dat)) {
      stop("No data to fit.", call. = FALSE)
    }
    fit_st <- st
    if (isTRUE(simulated)) {
      # Fit against sim columns without permanently mapping the canvas nodes.
      fit_st <- bind_sim_columns_to_state(st, dat)
    }
    unc <- input$uncertainty_method %||% "none"
    if (!unc %in% c("none", "bootstrap", "plausible")) unc <- "none"
    draws <- input$uncertainty_draws
    if (identical(unc, "plausible") && (is.null(draws) || !is.finite(draws))) draws <- 20
    if (identical(unc, "bootstrap") && (is.null(draws) || !is.finite(draws))) draws <- 199
    score_meth <- normalize_latent_score_method(
      input$latent_score_method %||% "own",
      fit_st
    )
    progress_cb <- if (!identical(unc, "none")) {
      function(i, total) {
        shiny::setProgress(
          value = 0.55 + 0.4 * (i / max(total, 1)),
          detail = sprintf("%s %d / %d", unc, i, total)
        )
      }
    } else {
      NULL
    }
    shiny::withProgress(message = progress_message, value = 0.1, {
      status(progress_message)
      shiny::setProgress(value = 0.2, detail = "Fitting model")
      res <- if (identical(input$inference_mode, "pvalue")) {
        fit_pvalue_model(
          fit_st, dat,
          alpha = input$alpha,
          mark_nonsig = isTRUE(input$mark_nonsig),
          conf_adjust = input$conf_p_adjust %||% "none",
          uncertainty = unc,
          uncertainty_draws = draws,
          uncertainty_progress = progress_cb,
          latent_score_method = score_meth
        )
      } else {
        mw <- as.numeric(input$min_weight %||% 0.95)
        if (!is.finite(mw) || mw < 0) mw <- 0
        if (mw > 1) mw <- 1
        fit_aic_bic_search(
          fit_st, dat,
          criterion = input$ic_criterion,
          uncertainty = unc,
          uncertainty_draws = draws,
          uncertainty_progress = progress_cb,
          min_weight = mw,
          mark_low_weight = isTRUE(input$mark_low_weight),
          latent_score_method = score_meth
        )
      }
      if (isTRUE(simulated)) {
        if (!is.null(res$state)) {
          res$state <- restore_column_mappings(res$state, st)
        }
        if (!is.null(res$state_averaged)) {
          res$state_averaged <- restore_column_mappings(res$state_averaged, st)
        }
      }
      shiny::setProgress(value = 0.95, detail = "Updating canvas")
      begin_inspector_suppress()
      freeze_inspector_inputs()
      # Never skip this redraw — a leftover skip_graph_render from move/rel-kind
      # would leave coefficient labels stuck on the canvas.
      skip_graph_render(FALSE)
      if (!is.null(res$comparison) && isTRUE(input$model_average) &&
            !is.null(res$state_averaged)) {
        res$model_averaged <- TRUE
        state(res$state_averaged)
      } else {
        if (!is.null(res$comparison)) res$model_averaged <- FALSE
        state(res$state)
      }
      res$from_simulation <- isTRUE(simulated)
      last_result(res)
      results_are_simulated(isTRUE(simulated))
      resid_plot_tick(shiny::isolate(resid_plot_tick()) + 1L)
      edge_plot_tick(shiny::isolate(edge_plot_tick()) + 1L)
      session$sendCustomMessage(
        "smt_render_graph",
        graph_payload(shiny::isolate(state()), dat)
      )
      unc_info <- res$uncertainty
      unc_txt <- if (!is.null(unc_info) && !identical(unc_info$method %||% "none", "none")) {
        sprintf(
          " · %s uncertainty (%d/%d draws ok)",
          unc_info$method, as.integer(unc_info$ok %||% 0L), as.integer(unc_info$draws %||% 0L)
        )
      } else {
        ""
      }
      if (isTRUE(simulated)) {
        seed_txt <- if (!is.null(sim_seed_used) && is.finite(sim_seed_used)) {
          sprintf(" · seed %d", as.integer(sim_seed_used))
        } else {
          ""
        }
        status(paste0(
          "Simulated analysis complete (",
          nrow(dat), " rows). Results are from simulated data.",
          seed_txt,
          unc_txt
        ))
      } else if (isTRUE(res$model_averaged)) {
        status(paste0("Analysis complete (full model averaging on canvas).", unc_txt))
      } else {
        status(paste0("Analysis complete.", unc_txt))
      }
    })
    shiny::updateRadioButtons(session, "center_view", selected = "model")
    invisible(TRUE)
  }

  shiny::observeEvent(input$run_analysis, {
    st <- ensure_edge_to_port(state())
    # Flush expected-effect prior from the link panel before fitting / persisting
    st <- merge_edge_priors_from_inputs(st)$state
    st <- flush_sim_params_from_inputs(st)
    dat <- analysis_rv()
    # Flush UI relationship kind onto the selected edge before fitting
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (!is.null(eid) && nzchar(eid) && !is.null(input$edge_rel_kind) &&
          nzchar(as.character(input$edge_rel_kind))) {
      st <- set_edge_rel_kind(st, eid, as.character(input$edge_rel_kind))
    }
    state(st)
    if (is.null(dat) || !nrow(dat)) {
      status("Load data before running the analysis.")
      return()
    }
    tryCatch({
      run_fit_pipeline(st, dat, simulated = FALSE)
    }, error = function(e) {
      status(paste("Error:", e$message))
      last_result(NULL)
      results_are_simulated(FALSE)
    })
  })

  shiny::observeEvent(input$run_simulation, {
    seed <- input$sim_seed
    n <- input$sim_n
    if (is.null(seed) || !is.finite(seed)) seed <- 1
    if (is.null(n) || !is.finite(n) || n < 10) {
      status("Simulation needs at least 10 samples.")
      return()
    }
    st <- ensure_edge_to_port(state())
    st <- merge_edge_priors_from_inputs(st)$state
    st <- flush_sim_params_from_inputs(st)
    st <- sync_sim_kinds_from_data(st, analysis_rv())
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (!is.null(eid) && nzchar(eid) && !is.null(input$edge_rel_kind) &&
          nzchar(as.character(input$edge_rel_kind))) {
      st <- set_edge_rel_kind(st, eid, as.character(input$edge_rel_kind))
    }
    state(st)
    tryCatch({
      sim <- simulate_path_data(st, n = as.integer(n), seed = as.integer(seed))
      sim_data_rv(sim)
      used_seed <- attr(sim, "sim_seed")
      if (isTRUE(attr(sim, "sim_has_intra")) && "subject" %in% names(sim)) {
        st <- enable_repeated_measures_for_intra_sim(st)
        state(st)
      }
      run_fit_pipeline(
        st, sim,
        simulated = TRUE,
        progress_message = "Simulating and fitting…",
        sim_seed_used = used_seed
      )
    }, error = function(e) {
      status(paste("Simulation error:", e$message))
      last_result(NULL)
      results_are_simulated(FALSE)
    })
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$run_power_analysis, {
    seed <- input$sim_seed
    n_max <- input$sim_n
    n_reps <- input$power_reps
    n_steps <- input$power_interp_steps
    n_cap <- input$power_max_n
    if (is.null(seed) || !is.finite(seed)) seed <- 1
    if (is.null(n_max) || !is.finite(n_max) || n_max < 10) {
      status("Power analysis needs Samples of at least 10.")
      return()
    }
    if (is.null(n_reps) || !is.finite(n_reps) || n_reps < 10) {
      status("Power analysis needs at least 10 replications.")
      return()
    }
    if (n_reps > 500) n_reps <- 500
    if (is.null(n_steps) || !is.finite(n_steps) || n_steps < 0) n_steps <- 0
    if (n_steps > 20) n_steps <- 20
    if (is.null(n_cap) || !is.finite(n_cap) || n_cap < 10) n_cap <- 1000
    if (n_cap > 100000) n_cap <- 100000
    st <- ensure_edge_to_port(state())
    st <- merge_edge_priors_from_inputs(st)$state
    st <- flush_sim_params_from_inputs(st)
    st <- sync_sim_kinds_from_data(st, analysis_rv())
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (!is.null(eid) && nzchar(eid) && !is.null(input$edge_rel_kind) &&
          nzchar(as.character(input$edge_rel_kind))) {
      st <- set_edge_rel_kind(st, eid, as.character(input$edge_rel_kind))
    }
    st <- enable_repeated_measures_for_intra_sim(st)
    state(st)
    conf_ids <- confirmatory_power_edge_ids(st)
    if (!length(conf_ids)) {
      status("Power analysis error: add at least one confirmatory structural link.")
      return()
    }
    zero_ids <- confirmatory_zero_prior_edge_ids(st, conf_ids)
    if (length(zero_ids)) {
      labs <- confirmatory_edge_warning_labels(st, zero_ids)
      status(paste0(
        "Power analysis error: set Expected effect (b ≠ 0) on confirmatory link",
        if (length(zero_ids) > 1L) "s" else "",
        " currently at 0: ",
        paste(labs, collapse = "; "),
        ". Auto-added interaction mains default to 0 until you set them."
      ))
      return()
    }
    mode <- if (identical(input$inference_mode, "pvalue")) "pvalue" else "search"
    criterion <- input$ic_criterion %||% "bic"
    if (!criterion %in% c("aic", "bic")) criterion <- "bic"
    alpha <- input$alpha %||% 0.05
    if (!is.finite(alpha)) alpha <- 0.05
    conf_adjust <- input$conf_p_adjust %||% "none"
    if (!conf_adjust %in% c("none", "bonferroni", "holm", "BH")) conf_adjust <- "none"
    min_weight <- as.numeric(input$min_weight %||% 0)
    if (!is.finite(min_weight) || min_weight < 0) min_weight <- 0
    if (min_weight > 1) min_weight <- 1
    # Only apply Σw rejection in search mode when the mark checkbox is on
    if (!identical(mode, "search") || !isTRUE(input$mark_low_weight)) {
      min_weight <- 0
    }
    score_meth <- normalize_latent_score_method(
      input$latent_score_method %||% "own",
      st
    )
    session$userData$power_cancel <- FALSE
    cancel_baseline <- shiny::isolate({
      v <- input$cancel_power_analysis
      if (is.null(v) || !is.finite(v)) 0L else as.integer(v)
    })
    tryCatch({
      shiny::withProgress(
        message = "Running power analysis…",
        value = 0,
        {
          status(sprintf(
            "Running power analysis (low-N grid + %d refine steps · %d reps)…",
            as.integer(n_steps), as.integer(n_reps)
          ))
          pw <- run_confirmatory_power(
            st,
            n_max = as.integer(n_max),
            n_reps = as.integer(n_reps),
            seed = as.integer(seed),
            mode = mode,
            alpha = as.numeric(alpha),
            criterion = criterion,
            conf_adjust = conf_adjust,
            min_weight = min_weight,
            latent_score_method = score_meth,
            n_steps = as.integer(n_steps),
            n_cap = as.integer(n_cap),
            progress = function(done, tot) {
              # Do not httpuv::service()/later::run_now() here: yielding from
              # inside this observeEvent deadlocks the session (UI stuck at
              # 1/N, R at ~0% CPU). Progress text still updates; Cancel is
              # cooperative via session$userData$power_cancel / smt_cmd only
              # between runs until power is moved off the reactive thread.
              shiny::setProgress(
                value = done / max(tot, 1L),
                detail = sprintf("%d / %d", done, tot)
              )
              if (isTRUE(session$userData$power_cancel)) {
                smt_cancel_power()
              }
            }
          )
          power_result_rv(pw)
          rec <- pw$recommended_n
          nstar_rule <- if (identical(mode, "pvalue")) {
            "power ≥ 80%"
          } else {
            "power ≥ 80% and P(0 false detections) ≥ 95%"
          }
          rec_txt <- if (!is.null(rec) && length(rec) == 1L && is.finite(rec)) {
            metrics <- format_power_metrics_at_n(pw$power, pw$fdr, rec)
            if (nzchar(metrics)) {
              sprintf(" · recommended N* = %d (%s)", as.integer(rec), metrics)
            } else {
              sprintf(" · recommended N* = %d", as.integer(rec))
            }
          } else if (isTRUE(pw$n_cap_hit)) {
            raw <- pw$recommended_n_raw
            sprintf(
              " · N* ≈ %s exceeds Max sample (%d)",
              if (is.finite(raw)) format(as.integer(raw), big.mark = ",") else "?",
              as.integer(pw$n_cap %||% n_cap)
            )
          } else {
            paste0(" · no N meets ", nstar_rule)
          }
          status(sprintf(
            "Power analysis complete (%d N evaluated · %d steps · %d reps · seed %d%s).",
            nrow(pw$power),
            as.integer(pw$n_steps %||% n_steps),
            as.integer(n_reps),
            as.integer(attr(pw$power, "seed") %||% seed),
            rec_txt
          ))
        }
      )
    }, error = function(e) {
      if (smt_is_power_cancelled(e)) {
        status("Power analysis cancelled.")
      } else {
        status(paste("Power analysis error:", e$message))
      }
    })
    session$userData$power_cancel <- FALSE
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$cancel_power_analysis, {
    session$userData$power_cancel <- TRUE
    status("Cancelling power analysis…")
  }, ignoreInit = TRUE)

  # Live R command bus: second process calls smt_cmd() → .smt/inbox → here.
  smt_cmd_resolve_node <- function(st, ref) {
    ref <- as.character(ref %||% "")[[1]]
    if (!nzchar(ref)) return(NA_character_)
    if (ref %in% st$nodes$id) return(ref)
    hit <- st$nodes$id[st$nodes$label == ref]
    if (length(hit)) return(hit[[1]])
    NA_character_
  }
  smt_cmd_resolve_edge <- function(st, args) {
    eid <- as.character(smt_cmd_arg(args, "id", "")[[1]])
    if (nzchar(eid) && eid %in% st$edges$id) return(eid)
    from <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "from"))
    to <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "to"))
    if (is.na(from) || is.na(to)) return(NA_character_)
    level <- smt_cmd_arg(args, "level", NULL)
    to_level <- smt_cmd_arg(args, "to_level", NULL)
    hit <- st$edges$id[st$edges$from == from & st$edges$to == to]
    if (!length(hit)) return(NA_character_)
    if (!is.null(level) && "level" %in% names(st$edges)) {
      lv <- as.character(level)[[1]]
      sub <- hit[as.character(st$edges$level[match(hit, st$edges$id)]) == lv]
      if (length(sub)) hit <- sub
    }
    if (!is.null(to_level) && "to_level" %in% names(st$edges)) {
      tlv <- as.character(to_level)[[1]]
      sub <- hit[as.character(st$edges$to_level[match(hit, st$edges$id)]) == tlv]
      if (length(sub)) hit <- sub
    }
    hit[[1]]
  }
  smt_cmd_arg <- function(args, name, default = NULL) {
    if (is.null(args) || !length(args) || !(name %in% names(args))) return(default)
    args[[name]]
  }
  smt_cmd_dispatch <- function(cmd) {
    action <- as.character(cmd$action %||% "")[[1]]
    args <- cmd$args %||% list()
    if (!is.list(args)) args <- as.list(args)

    if (identical(action, "ping")) {
      return(list(ok = TRUE, message = "pong", ts = as.numeric(Sys.time())))
    }
    if (identical(action, "status")) {
      return(list(
        ok = TRUE,
        message = as.character(status()),
        n_nodes = nrow(state()$nodes),
        n_edges = nrow(state()$edges),
        has_data = !is.null(analysis_rv()),
        has_result = !is.null(last_result()),
        simulated = isTRUE(results_are_simulated())
      ))
    }
    if (identical(action, "get_model")) {
      st <- state()
      return(list(
        ok = TRUE,
        message = "model",
        json = model_to_json(st, prepare = prepare_rv()),
        n_nodes = nrow(st$nodes),
        n_edges = nrow(st$edges)
      ))
    }
    if (identical(action, "cancel_power")) {
      session$userData$power_cancel <- TRUE
      status("Cancelling power analysis…")
      return(list(ok = TRUE, message = "cancel requested"))
    }
    if (identical(action, "clear_fit")) {
      st <- clear_fit_results(state())
      state(st)
      last_result(NULL)
      results_are_simulated(FALSE)
      power_result_rv(NULL)
      status("Fit cleared.")
      return(list(ok = TRUE, message = "fit cleared"))
    }
    if (identical(action, "load_project")) {
      path <- as.character(smt_cmd_arg(args, "path", "")[[1]])
      if (!nzchar(path) || !file.exists(path)) {
        stop("load_project requires an existing path=…", call. = FALSE)
      }
      txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      st <- model_from_json(txt)
      prep <- attr(st, "prepare")
      state(st)
      prepare_rv(ensure_prepare_state(prep))
      last_result(NULL)
      results_are_simulated(FALSE)
      power_result_rv(NULL)
      status(sprintf("Loaded project via smt_cmd (%d nodes).", nrow(st$nodes)))
      return(list(ok = TRUE, message = "project loaded", n_nodes = nrow(st$nodes)))
    }
    if (identical(action, "set_model")) {
      json <- smt_cmd_arg(args, "json", NULL)
      path <- as.character(smt_cmd_arg(args, "path", "")[[1]])
      if (is.null(json) && nzchar(path) && file.exists(path)) {
        json <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      }
      if (is.null(json) || !nzchar(as.character(json)[[1]])) {
        stop("set_model requires json=… or path=…", call. = FALSE)
      }
      st <- model_from_json(as.character(json)[[1]])
      prep <- attr(st, "prepare")
      state(st)
      if (!is.null(prep)) prepare_rv(ensure_prepare_state(prep))
      last_result(NULL)
      results_are_simulated(FALSE)
      power_result_rv(NULL)
      status(sprintf("Model replaced via smt_cmd (%d nodes).", nrow(st$nodes)))
      return(list(ok = TRUE, message = "model set", n_nodes = nrow(st$nodes)))
    }
    if (identical(action, "open_data")) {
      path <- as.character(smt_cmd_arg(args, "path", "")[[1]])
      if (!nzchar(path) || !file.exists(path)) {
        stop("open_data requires an existing path=…", call. = FALSE)
      }
      dat <- read_data_file(path, basename(path))
      set_raw_data(dat, reset_prepare = TRUE)
      last_data_file(path)
      status(sprintf("Data opened via smt_cmd (%d × %d).", nrow(dat), ncol(dat)))
      return(list(ok = TRUE, message = "data opened", nrow = nrow(dat), ncol = ncol(dat)))
    }
    if (identical(action, "add_node") || identical(action, "add_latent")) {
      label <- as.character(smt_cmd_arg(args, "label", "Node")[[1]])
      col <- smt_cmd_arg(args, "observed_column", NA_character_)
      kind <- if (identical(action, "add_latent")) {
        "latent"
      } else {
        as.character(smt_cmd_arg(args, "kind", "observed")[[1]])
      }
      st <- add_node(
        state(),
        label = label,
        observed_column = if (is.null(col) || (length(col) == 1L && is.na(col))) {
          NA_character_
        } else {
          as.character(col)[[1]]
        },
        kind = kind
      )
      state(st)
      nid <- st$nodes$id[[nrow(st$nodes)]]
      status(sprintf("Added node '%s' via smt_cmd.", label))
      return(list(ok = TRUE, message = "node added", id = nid, label = label))
    }
    if (identical(action, "add_interaction")) {
      st <- add_interaction_node(state())
      state(clear_fit_results(st))
      nid <- st$nodes$id[st$nodes$node_kind == "interaction"]
      nid <- nid[[length(nid)]]
      status("Added × node via smt_cmd.")
      return(list(ok = TRUE, message = "interaction added", id = nid))
    }
    if (identical(action, "remove_node")) {
      st <- state()
      nid <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "id", smt_cmd_arg(args, "node")))
      if (is.na(nid)) stop("remove_node: unknown id/label.", call. = FALSE)
      state(clear_fit_results(remove_node(st, nid)))
      status(sprintf("Removed node '%s' via smt_cmd.", nid))
      return(list(ok = TRUE, message = "node removed", id = nid))
    }
    if (identical(action, "add_edge")) {
      st <- state()
      from <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "from"))
      to <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "to"))
      if (is.na(from) || is.na(to)) stop("add_edge: unknown from/to.", call. = FALSE)
      typ <- as.character(smt_cmd_arg(args, "type", "confirmatory")[[1]])
      level <- smt_cmd_arg(args, "level", NA_character_)
      to_port <- smt_cmd_arg(args, "to_port", NA_character_)
      to_level <- smt_cmd_arg(args, "to_level", NA_character_)
      before <- nrow(st$edges)
      st <- add_edge(
        st, from, to, type = typ,
        level = if (is.null(level)) NA_character_ else as.character(level)[[1]],
        to_port = if (is.null(to_port)) NA_character_ else as.character(to_port)[[1]],
        to_level = if (is.null(to_level)) NA_character_ else as.character(to_level)[[1]]
      )
      if (nrow(st$edges) <= before) {
        stop("add_edge: link not added (exists or invalid).", call. = FALSE)
      }
      state(clear_fit_results(st))
      eid <- st$edges$id[[nrow(st$edges)]]
      status(sprintf("Added edge %s → %s via smt_cmd.", from, to))
      return(list(ok = TRUE, message = "edge added", id = eid, from = from, to = to))
    }
    if (identical(action, "remove_edge")) {
      st <- state()
      eid <- as.character(smt_cmd_arg(args, "id", "")[[1]])
      if (!nzchar(eid) || !eid %in% st$edges$id) {
        stop("remove_edge: unknown id=…", call. = FALSE)
      }
      st$edges <- st$edges[st$edges$id != eid, , drop = FALSE]
      state(clear_fit_results(st))
      status(sprintf("Removed edge '%s' via smt_cmd.", eid))
      return(list(ok = TRUE, message = "edge removed", id = eid))
    }
    if (identical(action, "map_column")) {
      st <- state()
      nid <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "id", smt_cmd_arg(args, "node")))
      if (is.na(nid)) stop("map_column: unknown id/label.", call. = FALSE)
      col <- smt_cmd_arg(args, "column", NA_character_)
      if (is.null(col) || (length(col) == 1L && !nzchar(as.character(col)))) {
        col <- NA_character_
      } else {
        col <- as.character(col)[[1]]
      }
      state(clear_fit_results(map_column_to_node(st, nid, col)))
      status(sprintf("Mapped '%s' via smt_cmd.", nid))
      return(list(ok = TRUE, message = "mapped", id = nid, column = col))
    }
    if (identical(action, "set_sim_var_kind")) {
      st <- state()
      nid <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "id", smt_cmd_arg(args, "node")))
      if (is.na(nid)) stop("set_sim_var_kind: unknown id/label.", call. = FALSE)
      kind <- as.character(smt_cmd_arg(args, "kind", "continuous")[[1]])
      st <- set_node_sim_var_kind(st, nid, kind)
      levs <- smt_cmd_arg(args, "levels", NULL)
      if (!is.null(levs)) {
        levs <- as.character(unlist(levs, use.names = FALSE))
        levs <- levs[nzchar(levs)]
        if (length(levs)) {
          st <- set_node_factor_coding(st, nid, base = levs[[1]], levels = levs, data = NULL)
        }
      }
      state(st)
      status(sprintf("Set %s sim kind=%s via smt_cmd.", nid, kind))
      return(list(ok = TRUE, message = "sim kind set", id = nid, kind = kind))
    }
    if (identical(action, "set_node_sim")) {
      st <- state()
      nid <- smt_cmd_resolve_node(st, smt_cmd_arg(args, "id", smt_cmd_arg(args, "node")))
      if (is.na(nid)) stop("set_node_sim: unknown id/label.", call. = FALSE)
      if (!is.null(smt_cmd_arg(args, "mean", NULL)) || !is.null(smt_cmd_arg(args, "sd", NULL))) {
        sp <- node_sim_params(st, nid)
        mean <- as.numeric(smt_cmd_arg(args, "mean", sp$mean)[[1]])
        sd <- as.numeric(smt_cmd_arg(args, "sd", sp$sd)[[1]])
        st <- set_node_sim_exogenous(st, nid, mean = mean, sd = sd)
      }
      if (!is.null(smt_cmd_arg(args, "intercept", NULL))) {
        st <- set_node_sim_intercept(
          st, nid, intercept = as.numeric(smt_cmd_arg(args, "intercept")[[1]])
        )
      }
      if (!is.null(smt_cmd_arg(args, "resid_sd", NULL))) {
        st <- set_node_sim_resid_sd(
          st, nid, resid_sd = as.numeric(smt_cmd_arg(args, "resid_sd")[[1]])
        )
      }
      if (!is.null(smt_cmd_arg(args, "min", NULL)) || !is.null(smt_cmd_arg(args, "max", NULL))) {
        sp <- node_sim_params(st, nid)
        lo <- smt_cmd_arg(args, "min", sp$min)
        hi <- smt_cmd_arg(args, "max", sp$max)
        st <- set_node_sim_bounds(st, nid, min = lo, max = hi)
      }
      state(st)
      sp <- node_sim_params(st, nid)
      status(sprintf("Updated sim knobs on '%s' via smt_cmd.", nid))
      return(list(
        ok = TRUE, message = "node sim set", id = nid,
        mean = sp$mean, sd = sp$sd, intercept = sp$intercept, resid_sd = sp$resid_sd
      ))
    }
    if (identical(action, "set_edge_prior")) {
      st <- state()
      eid <- smt_cmd_resolve_edge(st, args)
      if (is.na(eid)) stop("set_edge_prior: unknown id=… or from=/to=…", call. = FALSE)
      mean <- as.numeric(smt_cmd_arg(args, "mean", 0)[[1]])
      sd <- as.numeric(smt_cmd_arg(args, "sd", 1)[[1]])
      family <- as.character(smt_cmd_arg(args, "family", "normal")[[1]])
      st <- set_edge_prior(st, eid, mean = mean, sd = sd, family = family)
      state(st)
      sel_eid <- shiny::isolate(selected_structural_edge_id())
      if (!is.null(sel_eid) && identical(sel_eid, eid)) {
        skip_prior_writes_left(2L)
        shiny::updateNumericInput(session, "prior_mean_selected", value = mean)
        shiny::updateNumericInput(session, "prior_sd_selected", value = sd)
        edge_plot_tick(shiny::isolate(edge_plot_tick()) + 1L)
      }
      status(sprintf("Updated prior on %s via smt_cmd.", eid))
      return(list(ok = TRUE, message = "prior set", id = eid, mean = mean, sd = sd))
    }
    if (identical(action, "set_edge_rel_kind")) {
      st <- state()
      eid <- smt_cmd_resolve_edge(st, args)
      if (is.na(eid)) stop("set_edge_rel_kind: unknown id=… or from=/to=…", call. = FALSE)
      rk <- as.character(smt_cmd_arg(args, "rel_kind", "linear")[[1]])
      state(set_edge_rel_kind(st, eid, rk))
      return(list(ok = TRUE, message = "rel_kind set", id = eid, rel_kind = rk))
    }
    if (identical(action, "simulate")) {
      n <- as.integer(smt_cmd_arg(args, "n", input$sim_n %||% 200)[[1]])
      seed <- as.integer(smt_cmd_arg(args, "seed", input$sim_seed %||% 1)[[1]])
      if (!is.finite(n) || n < 10L) stop("simulate: n must be ≥ 10.", call. = FALSE)
      if (!is.finite(seed)) seed <- 1L
      st <- ensure_edge_to_port(state())
      st <- flush_sim_params_from_inputs(st)
      st <- sync_sim_kinds_from_data(st, analysis_rv())
      state(st)
      sim <- simulate_path_data(st, n = n, seed = seed)
      sim_data_rv(sim)
      used_seed <- attr(sim, "sim_seed")
      if (isTRUE(attr(sim, "sim_has_intra")) && "subject" %in% names(sim)) {
        st <- enable_repeated_measures_for_intra_sim(st)
        state(st)
      }
      run_fit_pipeline(
        st, sim,
        simulated = TRUE,
        progress_message = "Simulating and fitting…",
        sim_seed_used = used_seed
      )
      return(list(ok = TRUE, message = "simulate done", n = n, seed = used_seed %||% seed))
    }
    if (identical(action, "run")) {
      dat <- analysis_rv()
      if (is.null(dat) || !nrow(dat)) stop("run: no analysis data loaded.", call. = FALSE)
      st <- ensure_edge_to_port(state())
      st <- flush_sim_params_from_inputs(st)
      st <- sync_sim_kinds_from_data(st, dat)
      state(st)
      run_fit_pipeline(st, dat, simulated = FALSE, progress_message = "Running analysis…")
      return(list(ok = TRUE, message = "run done"))
    }
    stop("Unknown smt_cmd action: ", action, call. = FALSE)
  }

  # Steal command-bus ownership for the browser tab the user (or agent) is on.
  shiny::observeEvent(session$clientData$url_pathname, {
    root <- tryCatch(smt_cmd_root(), error = function(e) "")
    if (!nzchar(root)) return()
    smt_cmd_claim_owner(session$token, root = root)
  }, ignoreNULL = FALSE, once = TRUE)

  shiny::observe({
    shiny::invalidateLater(400, session)
    root <- tryCatch(smt_cmd_root(), error = function(e) "")
    if (!nzchar(root)) return()
    # Only the owning session consumes inbox commands (avoids split state).
    if (!isTRUE(smt_cmd_am_owner(session$token, root = root, max_age_sec = 2))) {
      return()
    }
    files <- smt_cmd_list_inbox(root)
    if (!length(files)) return()
    # One command per tick to keep the UI responsive
    path <- files[[1]]
    cmd <- smt_cmd_read_file(path)
    unlink(path)
    if (is.null(cmd) || is.null(cmd$id)) return()
    id <- as.character(cmd$id)[[1]]
    res <- tryCatch(
      smt_cmd_dispatch(cmd),
      error = function(e) list(ok = FALSE, message = conditionMessage(e))
    )
    if (is.null(res$ok)) res$ok <- TRUE
    res$action <- as.character(cmd$action %||% "")
    smt_cmd_write_result(id, res, root = root)
    smt_debug("smt_cmd | action=", res$action, " ok=", isTRUE(res$ok), " id=", id)
  })

  output$power_has_plot <- shiny::reactive({
    pw <- power_result_rv()
    !is.null(pw) && !is.null(pw$power) && nrow(pw$power) > 0
  })
  shiny::outputOptions(output, "power_has_plot", suspendWhenHidden = FALSE)

  output$power_recommend_msg <- shiny::renderUI({
    pw <- power_result_rv()
    if (is.null(pw) || is.null(pw$power) || !nrow(pw$power)) return(NULL)
    rec <- pw$recommended_n
    use_fdr <- isTRUE(attr(pw$power, "require_zero_null"))
    rule_txt <- if (use_fdr) {
      "power ≥ 80% and P(0 false detections) ≥ 95%"
    } else {
      "power ≥ 80% (false-detection rate ignored for N* under p-values)"
    }
    n_cap <- pw$n_cap %||% attr(pw$power, "n_cap")
    if (!is.null(rec) && length(rec) == 1L && is.finite(rec)) {
      metrics <- format_power_metrics_at_n(pw$power, pw$fdr, rec)
      detail <- if (nzchar(metrics)) {
        paste0(" — ", metrics, ".")
      } else {
        ""
      }
      shiny::div(
        class = "smt-power-recommend",
        shiny::tags$strong(sprintf("Recommended N* = %d", as.integer(rec))),
        shiny::span(detail)
      )
    } else if (isTRUE(pw$n_cap_hit) || isTRUE(attr(pw$power, "n_cap_hit"))) {
      raw <- pw$recommended_n_raw %||% attr(pw$power, "recommended_n_raw")
      shiny::div(
        class = "smt-power-recommend smt-power-recommend-miss",
        shiny::tags$strong("N* exceeds Max sample."),
        shiny::span(sprintf(
          " Curve-fit estimate ≈ %s but search is capped at %s. Raise Max sample or effect sizes for %s.",
          if (is.finite(raw)) format(as.integer(raw), big.mark = ",") else "?",
          if (is.finite(n_cap)) format(as.integer(n_cap), big.mark = ",") else "the cap",
          rule_txt
        ))
      )
    } else {
      shiny::div(
        class = "smt-power-recommend smt-power-recommend-miss",
        shiny::tags$strong("No recommended N."),
        shiny::span(sprintf(
          " Raise Max sample, Samples, or effect sizes until the curves reach %s.",
          rule_txt
        ))
      )
    }
  })

  output$power_plot <- shiny::renderPlot({
    pw <- power_result_rv()
    if (is.null(pw)) return(power_analysis_plot(NULL))
    power_analysis_plot(pw$power, recommended_n = pw$recommended_n)
  })

  output$power_fdr_plot <- shiny::renderPlot({
    pw <- power_result_rv()
    if (is.null(pw)) return(power_fdr_plot(NULL))
    power_fdr_plot(pw$fdr, recommended_n = pw$recommended_n)
  })

  output$sim_results_banner <- shiny::renderUI({
    if (!isTRUE(results_are_simulated()) || is.null(last_result())) return(NULL)
    sim <- sim_data_rv()
    n_txt <- if (!is.null(sim)) sprintf("%d rows", nrow(sim)) else "simulated sample"
    seed_used <- if (!is.null(sim)) attr(sim, "sim_seed") else NULL
    seed_txt <- if (!is.null(seed_used) && is.finite(seed_used)) {
      sprintf(" Seed used: %d.", as.integer(seed_used))
    } else {
      ""
    }
    shiny::div(
      class = "smt-sim-results-banner",
      role = "status",
      shiny::tags$strong("Simulated results"),
      shiny::span(
        sprintf(
          " — estimates below are from a generated dataset (%s), not your loaded data.",
          n_txt
        )
      ),
      if (nzchar(seed_txt)) shiny::span(seed_txt),
      shiny::span(" Use Run on mapped data for empirical analysis.")
    )
  })

  shiny::observeEvent(input$uncertainty_method, {
    m <- input$uncertainty_method %||% "none"
    if (identical(m, "plausible")) {
      shiny::updateNumericInput(session, "uncertainty_draws", value = 20, min = 5, max = 200)
    } else if (identical(m, "bootstrap")) {
      shiny::updateNumericInput(session, "uncertainty_draws", value = 199, min = 20, max = 2000)
    }
  }, ignoreInit = TRUE)

  # Toggle model averaging on an existing search result (updates canvas)
  shiny::observeEvent(input$model_average, {
    res <- last_result()
    if (is.null(res) || is.null(res$comparison) || is.null(res$state_averaged)) return()
    if (isTRUE(input$model_average)) {
      res$model_averaged <- TRUE
      st <- res$state_averaged
      # preserve node positions from current state
      cur <- state()
      if (nrow(st$nodes) && nrow(cur$nodes)) {
        m <- match(st$nodes$id, cur$nodes$id)
        hit <- !is.na(m)
        st$nodes$x[hit] <- cur$nodes$x[m[hit]]
        st$nodes$y[hit] <- cur$nodes$y[m[hit]]
      }
      state(st)
      status("Full model averaging enabled — canvas updated.")
    } else {
      res$model_averaged <- FALSE
      st <- apply_best_estimates_from_result(res, state())
      cur <- state()
      if (nrow(st$nodes) && nrow(cur$nodes)) {
        m <- match(st$nodes$id, cur$nodes$id)
        hit <- !is.na(m)
        st$nodes$x[hit] <- cur$nodes$x[m[hit]]
        st$nodes$y[hit] <- cur$nodes$y[m[hit]]
      }
      state(st)
      status("Showing best-model estimates on canvas.")
    }
    last_result(res)
  }, ignoreInit = TRUE)

  output$data_table <- DT::renderDT({
    dat <- data_rv()
    shiny::req(dat)
    smt_browse_datatable(dat, page_length = 15L, max_chars = 24L)
  })

  output$status_msg <- shiny::renderUI({
    msg <- status()
    if (is.null(msg) || !nzchar(msg)) return(NULL)
    if (startsWith(msg, "Analysis complete") ||
          startsWith(msg, "Simulated analysis complete") ||
          startsWith(msg, "Power analysis complete")) {
      shiny::div(
        class = paste(
          "smt-status",
          if (startsWith(msg, "Simulated")) {
            "smt-status-sim"
          } else if (startsWith(msg, "Power")) {
            "smt-status-sim"
          } else {
            "smt-status-ok"
          }
        ),
        shiny::tags$i(class = "fas fa-check smt-status-icon", `aria-hidden` = "true"),
        shiny::span(msg)
      )
    } else if (startsWith(msg, "Error:") || startsWith(msg, "Prepare error:") ||
                 startsWith(msg, "Simulation error:") ||
                 startsWith(msg, "Power analysis error:")) {
      shiny::div(
        class = "smt-status smt-status-error",
        shiny::tags$i(class = "fas fa-exclamation-circle smt-status-icon", `aria-hidden` = "true"),
        shiny::span(msg)
      )
    } else {
      shiny::div(class = "smt-status", shiny::span(msg))
    }
  })

  # First selected node id (if any)
  selected_node_id <- shiny::reactive({
    sel <- input$graph_selected
    if (is.null(sel$nodes) || !length(sel$nodes)) return(NULL)
    as.character(sel$nodes[[1]])
  })

  # Selected node that is currently a structural DV
  selected_dv_id <- shiny::reactive({
    nid <- selected_node_id()
    if (is.null(nid)) return(NULL)
    st <- state()
    dvs <- structural_dv_ids(st)
    if (!nid %in% dvs) return(NULL)
    nid
  })

  output$right_panel_title <- shiny::renderText({
    dv <- selected_dv_id()
    if (!is.null(dv)) {
      paste0("Inspector · ", node_display_name(state(), dv))
    } else if (!is.null(selected_node_id())) {
      paste0("Inspector · ", node_display_name(state(), selected_node_id()))
    } else {
      "Results"
    }
  })

  shiny::observeEvent(input$dv_repeated_measures, {
    if (!inspector_writes_allowed()) return()
    if (is.null(input$dv_repeated_measures)) return()
    dv <- selected_dv_id()
    if (is.null(dv)) return()
    en <- isTRUE(input$dv_repeated_measures)
    cl <- input$dv_cluster_column %||% NA_character_
    if (en && (is.null(cl) || is.na(cl) || !nzchar(cl))) {
      dat <- analysis_rv()
      if (!is.null(dat) && length(names(dat))) cl <- names(dat)[[1]]
    }
    cur <- node_repeated_measures(state(), dv)
    same_cl <- identical(
      as.character(cur$cluster %||% NA_character_),
      as.character(cl %||% NA_character_)
    )
    if (identical(isTRUE(cur$enabled), en) && same_cl) return()
    st <- set_node_repeated_measures(state(), dv, enabled = en, cluster_column = cl)
    state(clear_fit_results(st))
    if (!is.null(last_result())) last_result(NULL)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$dv_cluster_column, {
    if (!inspector_writes_allowed()) return()
    if (is.null(input$dv_cluster_column)) return()
    dv <- selected_dv_id()
    if (is.null(dv)) return()
    if (!isTRUE(input$dv_repeated_measures)) return()
    cl <- input$dv_cluster_column %||% NA_character_
    cur <- node_repeated_measures(state(), dv)
    if (identical(as.character(cur$cluster %||% NA_character_), as.character(cl))) {
      return()
    }
    st <- set_node_repeated_measures(state(), dv, enabled = TRUE, cluster_column = cl)
    state(clear_fit_results(st))
    if (!is.null(last_result())) last_result(NULL)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$dv_transform, {
    if (!inspector_writes_allowed()) return()
    if (is.null(input$dv_transform)) return()
    dv <- selected_dv_id()
    if (is.null(dv)) return()
    tr <- as.character(input$dv_transform %||% "none")
    if (!tr %in% names(dv_transform_choices()) &&
          !tr %in% unname(dv_transform_choices())) {
      return()
    }
    if (identical(node_dv_transform(state(), dv), tr)) return()
    st <- set_node_dv_transform(state(), dv, tr)
    state(clear_fit_results(st))
    if (!is.null(last_result())) last_result(NULL)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Selected observed node that can edit factor coding (categorical / binary factor)
  selected_factor_node_id <- shiny::reactive({
    nid <- selected_node_id()
    if (is.null(nid)) return(NULL)
    st <- state()
    if (is_interaction_node(st, nid) || is_latent_node(st, nid)) return(NULL)
    dat <- analysis_rv()
    col <- st$nodes$observed_column[match(nid, st$nodes$id)]
    if (is.na(col) || !nzchar(col) || is.null(dat) || !col %in% names(dat)) return(NULL)
    kind <- classify_variable(dat[[col]])
    if (!kind %in% c("categorical", "binary")) return(NULL)
    if (is.numeric(dat[[col]]) && !is.factor(dat[[col]])) return(NULL)
    nid
  })

  apply_factor_coding_from_inputs <- function(include_levels = FALSE) {
    if (!inspector_writes_allowed()) return()
    nid <- selected_factor_node_id()
    if (is.null(nid)) return()
    # Inspector remount can briefly leave selects NULL — ignore those flushes
    if (is.null(input$cat_factor_base)) return()
    dat <- analysis_rv()
    base <- input$cat_factor_base %||% NA_character_
    ordered <- isTRUE(input$cat_factor_ordered)
    cur <- node_factor_coding(state(), nid)
    coding <- variable_coding_for_node(state(), nid, dat)
    # Level-order text commits only via Apply — keystrokes must not remount ports.
    if (isTRUE(include_levels)) {
      lev_txt <- input$cat_factor_levels %||% ""
      levels <- if (nzchar(trimws(lev_txt))) {
        decode_factor_levels(lev_txt)
      } else {
        character()
      }
    } else if (length(cur$levels)) {
      levels <- cur$levels
    } else {
      levels <- coding$all_levels
    }
    # If order omitted, keep existing / data order with base first
    if (!length(levels)) {
      levels <- coding$all_levels
    }
    if (!is.na(base) && nzchar(base) && length(levels)) {
      levels <- c(base, setdiff(levels, base))
    }
    same <- identical(as.character(cur$base %||% NA_character_), as.character(base %||% NA_character_)) &&
      identical(isTRUE(cur$ordered), ordered) &&
      identical(cur$levels, levels)
    if (same) return()
    st <- set_node_factor_coding(
      state(), nid,
      base = base, ordered = ordered, levels = levels, data = dat
    )
    state(clear_fit_results(st))
    if (!is.null(last_result())) last_result(NULL)
    status("Factor coding updated — re-run analysis to refresh estimates.")
  }

  shiny::observeEvent(input$cat_factor_base, {
    apply_factor_coding_from_inputs(include_levels = FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$cat_factor_ordered, {
    apply_factor_coding_from_inputs(include_levels = FALSE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$apply_cat_factor_levels, {
    apply_factor_coding_from_inputs(include_levels = TRUE)
  }, ignoreInit = TRUE)

  node_mapping_inspector_ui <- function(st, dat, nid) {
    if (is.null(nid) || is_interaction_node(st, nid)) return(NULL)
    col <- st$nodes$observed_column[match(nid, st$nodes$id)]
    if (is.na(col)) col <- ""
    cols <- if (is.null(dat)) {
      c("(latent — none)" = "")
    } else {
      c("(latent — none)" = "", stats::setNames(names(dat), names(dat)))
    }
    selected <- if (nzchar(col) && !is.null(dat) && col %in% names(dat)) col else ""
    mapped_txt <- if (nzchar(selected)) {
      sprintf("Currently mapped to « %s ».", selected)
    } else if (is_latent_node(st, nid)) {
      "Currently latent (no data column)."
    } else {
      "Observed node — not mapped to a data column yet."
    }
    shiny::tagList(
      shiny::h4("Data mapping"),
      shiny::selectInput(
        "inspector_map_column",
        "Mapped column",
        choices = cols,
        selected = selected,
        width = "100%"
      ),
      shiny::helpText(
        mapped_txt, " Clear to make this node latent. ",
        "If the column is mapped elsewhere, it moves here."
      )
    )
  }

  node_is_unmapped_observed <- function(st, nid) {
    if (is.null(nid) || is_interaction_node(st, nid) || is_latent_node(st, nid)) {
      return(FALSE)
    }
    col <- st$nodes$observed_column[match(nid, st$nodes$id)]
    is.na(col) || !nzchar(as.character(col))
  }

  node_sim_exogenous_ui <- function(st, dat, nid) {
    if (is.null(nid)) return(NULL)
    if (!node_is_structural_exogenous(st, nid)) return(NULL)
    if (is_interaction_node(st, nid)) return(NULL)
    st <- ensure_node_sim_columns(st)
    unmapped <- node_is_unmapped_observed(st, nid)
    kind <- node_sim_var_kind(st, nid, data = dat)
    declared <- st$nodes$sim_var_kind[match(nid, st$nodes$id)]
    if (is.na(declared) || !nzchar(declared)) declared <- "continuous"

    type_ui <- if (unmapped || is_latent_node(st, nid)) {
      shiny::radioButtons(
        "sim_var_kind",
        "Variable type",
        choices = c(
          Continuous = "continuous",
          Categorical = "categorical",
          Binary = "binary"
        ),
        selected = declared,
        inline = TRUE
      )
    } else {
      shiny::helpText(sprintf("Type from mapped data: %s.", kind))
    }

    if (kind %in% c("categorical", "binary")) {
      levs <- node_sim_factor_levels(st, nid, data = dat)
      design_mode <- node_sim_design_mode(st, nid)
      alloc_mode <- node_sim_alloc_mode(st, nid)
      design_ui <- shiny::radioButtons(
        "sim_design_mode",
        "Design",
        choiceNames = list("Inter (between)", "Intra (within)"),
        choiceValues = list("inter", "intra"),
        selected = design_mode,
        inline = TRUE
      )
      alloc_ui <- shiny::radioButtons(
        "sim_alloc_mode",
        "Level allocation",
        choiceNames = list("Probability", "Exact counts"),
        choiceValues = list("probability", "exact"),
        selected = alloc_mode,
        inline = TRUE
      )
      level_inputs <- if (identical(alloc_mode, "exact")) {
        counts <- node_sim_level_counts(st, nid, data = dat)
        lapply(seq_along(levs), function(i) {
          shiny::numericInput(
            paste0("sim_count_", i),
            sprintf("n(%s)", levs[[i]]),
            value = as.integer(counts[[levs[[i]]]] %||% 1L),
            min = 0,
            step = 1,
            width = "100%"
          )
        })
      } else {
        props <- node_sim_level_props(st, nid, data = dat)
        lapply(seq_along(levs), function(i) {
          shiny::numericInput(
            paste0("sim_prop_", i),
            sprintf("P(%s)", levs[[i]]),
            value = as.numeric(props[[levs[[i]]]] %||% (1 / length(levs))),
            min = 0,
            max = 1,
            step = 0.05,
            width = "100%"
          )
        })
      }
      levels_ui <- if (unmapped) {
        shiny::tagList(
          shiny::textAreaInput(
            "sim_factor_levels",
            "Levels (one per line; first = reference)",
            value = encode_factor_levels(levs),
            rows = max(3L, length(levs)),
            width = "100%"
          ),
          shiny::actionButton(
            "apply_sim_factor_levels",
            "Apply levels",
            class = "btn-sm btn-default"
          ),
          shiny::helpText("Canvas ports update when you Apply (not while typing).")
        )
      } else {
        NULL
      }
      help_txt <- if (identical(alloc_mode, "exact")) {
        if (identical(design_mode, "intra")) {
          "Exact counts per level within each subject. Several Intra factors expand their Cartesian product (cell weight = product of counts). Prefer 1 of each for a balanced repeated-measures factorial."
        } else {
          "Exact counts tile a balanced between-subject assignment (recycled to Samples)."
        }
      } else if (identical(design_mode, "intra")) {
        "Probability mode on Intra still expands every level combination once per subject (balanced cells). Use Exact counts for unequal within-subject repetitions."
      } else {
        "Expected proportions when simulating this factor (renormalized to sum 1)."
      }
      return(shiny::tagList(
        shiny::h4("Simulation (exogenous)"),
        type_ui,
        levels_ui,
        design_ui,
        alloc_ui,
        shiny::div(class = "smt-sim-fields", level_inputs),
        shiny::helpText(help_txt, " Ignored by frequentist Run.")
      ))
    }

    sp <- node_sim_params(st, nid)
    shiny::tagList(
      shiny::h4("Simulation (exogenous)"),
      type_ui,
      shiny::div(
        class = "smt-sim-fields",
        shiny::numericInput(
          "sim_exog_mean",
          "Expected mean",
          value = sp$mean,
          step = 0.1,
          width = "100%"
        ),
        shiny::numericInput(
          "sim_exog_sd",
          "SD",
          value = sp$sd,
          min = 0.01,
          step = 0.1,
          width = "100%"
        ),
        shiny::numericInput(
          "sim_exog_min",
          "Min (optional)",
          value = if (is.finite(sp$min)) sp$min else NA,
          step = 0.1,
          width = "100%"
        ),
        shiny::numericInput(
          "sim_exog_max",
          "Max (optional)",
          value = if (is.finite(sp$max)) sp$max else NA,
          step = 0.1,
          width = "100%"
        )
      ),
      shiny::helpText(
        "Generative Normal(mean, SD) when simulating this exogenous continuous ",
        "variable. Draws outside Min/Max are clamped. Leave Min/Max blank for ",
        "no bound. Defaults 0 / 1. Ignored by frequentist Run."
      )
    )
  }

  node_sim_resid_ui <- function(st, nid) {
    if (is.null(nid)) return(NULL)
    if (!nid %in% structural_dv_ids(st)) return(NULL)
    st <- ensure_node_sim_columns(st)
    sp <- node_sim_params(st, nid)
    unmapped <- node_is_unmapped_observed(st, nid) || is_latent_node(st, nid)
    declared <- st$nodes$sim_var_kind[match(nid, st$nodes$id)]
    if (is.na(declared) || !nzchar(declared)) declared <- "continuous"
    kind <- node_sim_var_kind(st, nid, data = analysis_rv())

    type_ui <- if (unmapped) {
      shiny::tagList(
        shiny::radioButtons(
          "sim_var_kind",
          "Variable type",
          choices = c(
            Continuous = "continuous",
            Categorical = "categorical",
            Binary = "binary"
          ),
          selected = declared,
          inline = TRUE
        ),
        if (kind %in% c("categorical", "binary")) {
          levs <- node_sim_factor_levels(st, nid, data = NULL)
          shiny::tagList(
            shiny::textAreaInput(
              "sim_factor_levels",
              "Levels (one per line; first = reference)",
              value = encode_factor_levels(levs),
              rows = max(2L, length(levs)),
              width = "100%"
            ),
            shiny::actionButton(
              "apply_sim_factor_levels",
              "Apply levels",
              class = "btn-sm btn-default"
            ),
            shiny::helpText("Canvas ports update when you Apply (not while typing).")
          )
        },
        shiny::helpText(
          "Sets the canvas engine (lm vs glm · binomial) and how Simulate draws ",
          "this outcome. Binary uses a logistic / Bernoulli draw (intercept on the ",
          "logit scale); residual SD is ignored."
        )
      )
    } else {
      shiny::helpText(sprintf("Type from mapped data: %s.", kind))
    }

    cont_params <- if (identical(kind, "binary")) {
      shiny::tagList(
        shiny::numericInput(
          "sim_dv_intercept",
          "Intercept (logit)",
          value = if (is.finite(sp$intercept)) sp$intercept else 0,
          step = 0.1,
          width = "100%"
        ),
        shiny::helpText(
          "Baseline logit when predictors are at reference / centered 0. ",
          "Simulate draws Bernoulli outcomes; Residual SD and Min/Max are unused."
        )
      )
    } else if (identical(kind, "categorical")) {
      shiny::helpText(
        "Categorical outcomes are not simulated yet. Use Binary (logistic) or ",
        "Continuous, or map a data column and Run."
      )
    } else {
      shiny::tagList(
        shiny::numericInput(
          "sim_dv_intercept",
          "Intercept",
          value = if (is.finite(sp$intercept)) sp$intercept else 0,
          step = 0.1,
          width = "100%"
        ),
        shiny::numericInput(
          "sim_resid_sd",
          "Residual SD (\u03b5)",
          value = sp$resid_sd,
          min = 0.01,
          step = 0.1,
          width = "100%"
        ),
        shiny::numericInput(
          "sim_dv_min",
          "Min (optional)",
          value = if (is.finite(sp$min)) sp$min else NA,
          step = 0.1,
          width = "100%"
        ),
        shiny::numericInput(
          "sim_dv_max",
          "Max (optional)",
          value = if (is.finite(sp$max)) sp$max else NA,
          step = 0.1,
          width = "100%"
        ),
        shiny::helpText(
          "Intercept is the baseline outcome mean when predictors are at reference ",
          "(factors) or centered 0 (continuous). Residual SD adds noise on the same ",
          "scale; outcomes outside Min/Max are clamped. Defaults intercept 0, resid 1. ",
          "Ignored by frequentist Run."
        )
      )
    }

    shiny::tagList(
      shiny::h4("Simulation (outcome)"),
      type_ui,
      cont_params
    )
  }

  apply_sim_exogenous_from_inputs <- function(apply_levels = FALSE) {
    if (!inspector_writes_allowed()) return()
    nid <- selected_node_id()
    if (is.null(nid)) return()
    st0 <- ensure_node_sim_columns(state())
    # Kind/levels apply to unmapped DVs too (same sim_var_kind control)
    is_exog <- node_is_structural_exogenous(st0, nid)
    is_dv <- nid %in% structural_dv_ids(st0)
    if (!is_exog && !is_dv) return()
    kind0 <- node_sim_var_kind(st0, nid, data = analysis_rv())
    lev0 <- node_sim_factor_levels(st0, nid, data = analysis_rv())
    dm0 <- if (is_exog) node_sim_design_mode(st0, nid) else NA_character_
    st1 <- flush_sim_params_from_inputs(st0, apply_levels = apply_levels)
    kind1 <- node_sim_var_kind(st1, nid, data = analysis_rv())
    lev1 <- node_sim_factor_levels(st1, nid, data = analysis_rv())
    dm1 <- if (is_exog) node_sim_design_mode(st1, nid) else NA_character_
    # Kind/level/design changes alter canvas ports/badges — never skip that redraw.
    visual_changed <- !identical(kind0, kind1) || !identical(lev0, lev1) ||
      !identical(dm0, dm1)
    if (!visual_changed) skip_graph_render(TRUE)
    state(st1)
    if (isTRUE(apply_levels) && visual_changed) {
      status("Levels applied.")
    }
  }

  apply_sim_resid_from_inputs <- function() {
    if (!inspector_writes_allowed()) return()
    dv <- selected_dv_id()
    if (is.null(dv)) return()
    st0 <- ensure_node_sim_columns(state())
    cur <- node_sim_params(st0, dv)
    st1 <- st0
    changed <- FALSE
    icept <- input$sim_dv_intercept
    if (!is.null(icept) && length(icept) == 1L && is.finite(icept) &&
          !isTRUE(all.equal(cur$intercept, as.numeric(icept), tolerance = 1e-9))) {
      st1 <- set_node_sim_intercept(st1, dv, intercept = icept)
      changed <- TRUE
    }
    s <- input$sim_resid_sd
    if (!is.null(s) && length(s) == 1L && is.finite(s) && s > 0 &&
          !isTRUE(all.equal(cur$resid_sd, as.numeric(s), tolerance = 1e-9))) {
      st1 <- set_node_sim_resid_sd(st1, dv, resid_sd = s)
      changed <- TRUE
    }
    lo <- input$sim_dv_min
    hi <- input$sim_dv_max
    if (!is.null(lo) || !is.null(hi)) {
      lo_v <- if (is.null(lo) || length(lo) != 1L || is.na(lo) || !is.finite(lo)) {
        NA_real_
      } else {
        as.numeric(lo)
      }
      hi_v <- if (is.null(hi) || length(hi) != 1L || is.na(hi) || !is.finite(hi)) {
        NA_real_
      } else {
        as.numeric(hi)
      }
      if (is.null(lo)) lo_v <- cur$min
      if (is.null(hi)) hi_v <- cur$max
      same_lo <- (is.na(cur$min) && is.na(lo_v)) ||
        (is.finite(cur$min) && is.finite(lo_v) &&
           isTRUE(all.equal(cur$min, lo_v, tolerance = 1e-9)))
      same_hi <- (is.na(cur$max) && is.na(hi_v)) ||
        (is.finite(cur$max) && is.finite(hi_v) &&
           isTRUE(all.equal(cur$max, hi_v, tolerance = 1e-9)))
      if (!same_lo || !same_hi) {
        st1 <- set_node_sim_bounds(st1, dv, min = lo_v, max = hi_v)
        changed <- TRUE
      }
    }
    if (!changed) return()
    skip_graph_render(TRUE)
    state(st1)
  }

  shiny::observeEvent(input$sim_exog_mean, {
    apply_sim_exogenous_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  shiny::observeEvent(input$sim_exog_sd, {
    apply_sim_exogenous_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  shiny::observeEvent(input$sim_exog_min, {
    apply_sim_exogenous_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(input$sim_exog_max, {
    apply_sim_exogenous_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(input$sim_var_kind, {
    apply_sim_exogenous_from_inputs(apply_levels = FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  shiny::observeEvent(input$apply_sim_factor_levels, {
    apply_sim_exogenous_from_inputs(apply_levels = TRUE)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$sim_design_mode, {
    apply_sim_exogenous_from_inputs(apply_levels = FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  shiny::observeEvent(input$sim_alloc_mode, {
    apply_sim_exogenous_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  lapply(seq_len(12L), function(i) {
    local({
      ii <- i
      shiny::observeEvent(input[[paste0("sim_prop_", ii)]], {
        apply_sim_exogenous_from_inputs()
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      shiny::observeEvent(input[[paste0("sim_count_", ii)]], {
        apply_sim_exogenous_from_inputs()
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })
  shiny::observeEvent(input$sim_dv_intercept, {
    apply_sim_resid_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  shiny::observeEvent(input$sim_resid_sd, {
    apply_sim_resid_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  shiny::observeEvent(input$sim_dv_min, {
    apply_sim_resid_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(input$sim_dv_max, {
    apply_sim_resid_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$inspector_map_column, {
    if (!inspector_writes_allowed()) return()
    # NULL = input not ready / inspector remounting — never treat as "clear mapping"
    col <- input$inspector_map_column
    if (is.null(col)) return()
    nid <- selected_node_id()
    if (is.null(nid)) return()
    st0 <- state()
    if (is_interaction_node(st0, nid)) return()
    if (!nzchar(col)) col <- NA_character_
    cur <- st0$nodes$observed_column[match(nid, st0$nodes$id)]
    cur_chr <- if (is.na(cur) || !nzchar(cur)) "" else as.character(cur)
    new_chr <- if (is.na(col) || !nzchar(col)) "" else as.character(col)
    if (identical(cur_chr, new_chr)) return()
    st <- map_column_to_node(st0, nid, col)
    state(clear_fit_results(st))
    if (!is.null(last_result())) last_result(NULL)
    status("Mapping updated.")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  factor_coding_inspector_ui <- function(st, dat, nid) {
    coding <- variable_coding_for_node(st, nid, dat)
    meta <- node_factor_coding(st, nid)
    levs <- coding$all_levels
    if (!length(levs)) return(NULL)
    base_sel <- if (!is.na(meta$base) && meta$base %in% levs) meta$base else coding$base
    order_txt <- if (length(meta$levels)) {
      encode_factor_levels(meta$levels)
    } else {
      encode_factor_levels(levs)
    }
    if (is.na(order_txt) || !nzchar(order_txt)) {
      order_txt <- paste(levs, collapse = "\n")
    }
    shiny::tagList(
      shiny::h4("Factor coding"),
      shiny::selectInput(
        "cat_factor_base",
        "Base (reference) level",
        choices = levs,
        selected = base_sel,
        width = "100%"
      ),
      shiny::checkboxInput(
        "cat_factor_ordered",
        "Ordered factor",
        value = isTRUE(meta$ordered)
      ),
      shiny::conditionalPanel(
        condition = "input.cat_factor_ordered === true",
        shiny::textAreaInput(
          "cat_factor_levels",
          "Level order (one per line)",
          value = order_txt,
          rows = min(8L, max(3L, length(levs))),
          width = "100%"
        ),
        shiny::actionButton(
          "apply_cat_factor_levels",
          "Apply level order",
          class = "btn-sm btn-default"
        ),
        shiny::helpText(
          "Top to bottom = factor order. Press Apply to commit — the canvas does not ",
          "update while typing. The base level is moved first for contrasts when you ",
          "pick it above. Split ports follow non-base levels in this order."
        )
      ),
      if (!isTRUE(meta$ordered)) {
        shiny::helpText(
          "Unordered: choose the reference level. Enable Ordered to set a custom level sequence."
        )
      }
    )
  }

  output$inspector_panel <- shiny::renderUI({
    st <- state()
    dat <- analysis_rv()
    nid <- selected_node_id()
    dv <- selected_dv_id()
    res <- last_result()
    factor_nid <- selected_factor_node_id()

    # Selected variable / latent node: mapping (+ DV / factor extras)
    if (!is.null(nid) && !is_interaction_node(st, nid)) {
      map_ui <- node_mapping_inspector_ui(st, dat, nid)

      if (!is.null(dv)) {
        eng <- dv_model_engine(st, dat, dv)
        badge <- dv_model_badge(eng %||% "lm")
        r2 <- st$nodes$r_squared[match(dv, st$nodes$id)]
        r2k <- st$nodes$r_squared_kind[match(dv, st$nodes$id)]
        r2_txt <- if (is.finite(r2)) {
          if (identical(r2k, "mcfadden")) sprintf("McFadden R\u00B2 = %.3f", r2)
          else sprintf("R\u00B2 = %.3f", r2)
        } else {
          NULL
        }
        rm <- node_repeated_measures(st, dv)
        y_tr <- node_dv_transform(st, dv)
        cluster_choices <- if (!is.null(dat)) names(dat) else character()
        latents <- model_uses_latents(st)
        col <- st$nodes$observed_column[match(dv, st$nodes$id)]
        y_kind <- if (!is.null(dat) && !is.na(col) && nzchar(col) && col %in% names(dat)) {
          classify_variable(dat[[col]])
        } else {
          node_sim_var_kind(st, dv, data = NULL)
        }
        can_transform <- identical(y_kind, "continuous") && !isTRUE(latents)
        eq_html <- NULL
        fits_dv <- dv_result_fits(res, dv)
        if (length(fits_dv)) {
          res_one <- res
          res_one$fit <- fits_dv
          eq_html <- format_best_equations_html(res_one, st)
        }

        return(shiny::tagList(
          shiny::div(
            class = "smt-inspector",
            shiny::div(
              class = "smt-inspector-meta",
              shiny::span(class = "smt-inspector-badge", badge),
              if (!is.null(r2_txt)) shiny::span(class = "smt-inspector-r2", r2_txt)
            ),
            map_ui,
            node_sim_resid_ui(st, dv),
            if (can_transform) {
              shiny::selectInput(
                "dv_transform",
                "Outcome transform",
                choices = dv_transform_choices(),
                selected = y_tr,
                width = "100%"
              )
            } else if (!identical(y_kind, "continuous")) {
              shiny::helpText("Outcome transforms apply to continuous DVs only.")
            },
            if (latents) {
              meth <- shiny::isolate({
                normalize_latent_score_method(
                  last_result()$latent_score_method %||%
                    input$latent_score_method %||% "own",
                  st
                )
              })
              score_txt <- if (identical(meth, "joint")) {
                "joint Thomson/regression scores from the multi-factor CFA"
              } else {
                "scores from each latent's own indicators only"
              }
              shiny::helpText(
                "Latents: confirmatory CFA (no rotation) for loadings on the canvas; ",
                "path stage uses ", score_txt, " (Analysis → Latent scores when ≥2 latents). ",
                "Structural links then use the usual path engine (lm/glm, AIC/BIC, ",
                "split logistic). Score weights appear in Model fit after analysis."
              )
            },
            shiny::tagList(
              shiny::checkboxInput(
                "dv_repeated_measures",
                "Repeated measures (random intercept)",
                value = isTRUE(rm$enabled)
              ),
              shiny::conditionalPanel(
                condition = "input.dv_repeated_measures === true",
                shiny::selectInput(
                  "dv_cluster_column",
                  "Clustering variable",
                  choices = cluster_choices,
                  selected = if (!is.na(rm$cluster) && nzchar(rm$cluster) &&
                                   rm$cluster %in% cluster_choices) {
                    rm$cluster
                  } else if (length(cluster_choices)) {
                    cluster_choices[[1]]
                  } else {
                    NULL
                  },
                  width = "100%"
                ),
                shiny::helpText("Fits lmer / glmer with (1 | cluster). No random slopes yet.")
              )
            ),
            if (!is.null(factor_nid) && identical(factor_nid, dv)) {
              factor_coding_inspector_ui(st, dat, dv)
            },
            shiny::h4("Equation"),
            if (!is.null(eq_html)) {
              eq_html
            } else {
              shiny::helpText("Run the analysis to see this DV's fitted equation.")
            }
          )
        ))
      }

      if (!is.null(factor_nid)) {
        kind_lab <- if (identical(
          classify_variable(dat[[st$nodes$observed_column[match(factor_nid, st$nodes$id)]]]),
          "categorical"
        )) {
          "categorical"
        } else {
          "binary"
        }
        return(shiny::tagList(
          shiny::div(
            class = "smt-inspector",
            shiny::div(
              class = "smt-inspector-meta",
              shiny::span(class = "smt-inspector-badge", kind_lab)
            ),
            map_ui,
            node_sim_exogenous_ui(st, dat, nid),
            factor_coding_inspector_ui(st, dat, factor_nid)
          )
        ))
      }

      # Observed or latent node that is not a DV
      kind_lab <- if (is_latent_node(st, nid)) "latent" else "observed"
      return(shiny::tagList(
        shiny::div(
          class = "smt-inspector",
          shiny::div(
            class = "smt-inspector-meta",
            shiny::span(class = "smt-inspector-badge", kind_lab)
          ),
          map_ui,
          node_sim_exogenous_ui(st, dat, nid)
        )
      ))
    }

    if (!is.null(nid) && is_interaction_node(st, nid)) {
      return(shiny::tagList(
        shiny::div(
          class = "smt-inspector",
          shiny::div(
            class = "smt-inspector-meta",
            shiny::span(class = "smt-inspector-badge", "interaction")
          ),
          shiny::helpText(
            "Wire two predictors into the × ports, then drag × to a DV. ",
            "Interaction nodes are not mapped to a data column."
          )
        )
      ))
    }

    # Default: global model-fit equations
    shiny::tagList(
      shiny::h4("Model fit"),
      if (is.null(res)) {
        shiny::helpText("Run the analysis to see fitted equations.")
      } else if (isTRUE(res$model_averaged)) {
        format_averaged_equations_html(res, st)
      } else {
        format_best_equations_html(res, st)
      }
    )
  })

  output$dv_resid_has_plot <- shiny::reactive({
    resid_plot_tick()
    dv <- selected_dv_id()
    fits_dv <- dv_result_fits(last_result(), dv)
    if (!length(fits_dv)) return(FALSE)
    keys <- names(fits_dv)
    if (is.null(keys) || !length(keys)) return(FALSE)
    key <- if (length(keys) > 1L) {
      input$dv_diag_equation %||% keys[[1]]
    } else {
      keys[[1]]
    }
    if (is.null(key) || !key %in% keys) key <- keys[[1]]
    !is.null(path_fit_residual_info(fits_dv[[key]]))
  })
  shiny::outputOptions(output, "dv_resid_has_plot", suspendWhenHidden = FALSE)

  output$dv_resid_controls <- shiny::renderUI({
    resid_plot_tick()
    dv <- selected_dv_id()
    res <- last_result()
    if (is.null(dv)) return(NULL)
    fits_dv <- dv_result_fits(res, dv)
    if (!length(fits_dv)) {
      return(shiny::tagList(
        shiny::h4("Residual diagnostics"),
        shiny::helpText("Run the analysis to see residual diagnostics for this DV.")
      ))
    }
    keys <- names(fits_dv)
    if (is.null(keys) || !length(keys)) return(NULL)
    # Only show the section when at least one equation has extractable residuals
    has_any <- any(vapply(
      fits_dv,
      function(f) !is.null(path_fit_residual_info(f)),
      logical(1)
    ))
    if (!has_any) return(NULL)
    shiny::tagList(
      shiny::h4("Residual diagnostics"),
      if (length(keys) > 1L) {
        shiny::selectInput(
          "dv_diag_equation",
          "Equation",
          choices = stats::setNames(
            keys,
            vapply(keys, function(k) dv_fit_key_label(state(), k), character(1))
          ),
          selected = keys[[1]],
          width = "100%"
        )
      },
      shiny::helpText(
        "QQ checks residual normality; residuals vs fitted checks ",
        "non-linearity / heteroscedasticity. For glm/glmer, deviance ",
        "residuals are shown."
      )
    )
  })

  output$dv_resid_plot <- shiny::renderPlot({
    tick <- resid_plot_tick()
    # Also refresh after Run without a new click
    res <- last_result()
    dv <- selected_dv_id()
    fits_dv <- dv_result_fits(res, dv)
    smt_debug(
      "R dv_resid_plot | tick=", tick,
      " dv=", dv %||% "NULL",
      " n_fits=", length(fits_dv),
      " has_result=", !is.null(res)
    )
    if (is.null(dv) || !length(fits_dv)) {
      smt_debug("R dv_resid_plot -> blank (nothing to show)")
      return(residual_diagnostics_plot(NULL, title = NULL))
    }
    keys <- names(fits_dv)
    if (is.null(keys) || !length(keys)) {
      smt_debug("R dv_resid_plot -> blank (unnamed fits)")
      return(residual_diagnostics_plot(NULL, title = NULL))
    }
    # Avoid reading dv_diag_equation when the selectInput is absent — a frozen
    # or missing input used to abort this renderPlot mid-flush (blank panel).
    key <- if (length(keys) > 1L) {
      input$dv_diag_equation %||% keys[[1]]
    } else {
      keys[[1]]
    }
    if (is.null(key) || !key %in% keys) key <- keys[[1]]
    fit_obj <- fits_dv[[key]]
    smt_debug(
      "R dv_resid_plot -> plotting key=", key,
      " class=", paste(class(fit_obj), collapse = "/")
    )
    tryCatch(
      residual_diagnostics_plot(
        fit_obj,
        title = dv_fit_key_label(state(), key)
      ),
      error = function(e) {
        smt_debug("R dv_resid_plot ERROR: ", e$message)
        residual_diagnostics_plot(NULL, title = NULL)
      }
    )
  })
  shiny::outputOptions(output, "dv_resid_plot", suspendWhenHidden = FALSE)

  output$coef_table <- DT::renderDT({
    res <- last_result()
    # When averaging, coefficients live on state edges already
    est <- if (!is.null(res) && !isTRUE(res$model_averaged)) res$estimates else NULL
    tab <- estimates_table(state(), estimates = est)
    dv <- selected_dv_id()
    if (!is.null(dv) && nrow(tab)) {
      dv_lab <- node_display_name(state(), dv)
      tr_lab <- dv_transform_lhs(dv_lab, node_dv_transform(state(), dv))
      # Match Y, transform log(Y), and split outcomes Y[level]
      is_to <- tab$To %in% c(dv_lab, tr_lab) |
        startsWith(as.character(tab$To), paste0(dv_lab, "["))
      keep <- is_to | (tab$Type == "indirect" & (tab$From == dv_lab | is_to))
      tab <- tab[keep, , drop = FALSE]
    }
    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE),
      class = "compact"
    )
  })

  output$best_vs_conf_table <- DT::renderDT({
    res <- last_result()
    if (is.null(res) || is.null(res$best_vs_confirmatory)) return(NULL)
    tab <- format_best_vs_confirmatory_display(res$best_vs_confirmatory, state())
    if (is.null(tab) || !nrow(tab)) return(NULL)
    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE),
      class = "compact"
    )
  })

  output$comparison_table <- DT::renderDT({
    res <- last_result()
    if (is.null(res) || is.null(res$comparison)) return(NULL)
    DT::datatable(
      format_comparison_display(res$comparison, state()),
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip"),
      class = "compact"
    )
  })

  output$syntax_out <- shiny::renderText({
    res <- last_result()
    if (is.null(res)) return("")
    res$syntax
  })
}

#' Build the Shiny application object
#' @keywords internal
smt_shiny_app <- function() {
  shiny::shinyApp(ui = smt_ui(), server = smt_server)
}
