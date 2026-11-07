#' @keywords internal
smt_ui <- function() {
  shiny::fluidPage(
    title = "Structural Model Tester",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "jamovi-theme.css"),
      shiny::tags$script(src = "smt-panels.js"),
      shiny::tags$script(src = "smt-canvas.js"),
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
            )
          )
        ),
        shiny::div(
          class = "smt-top-actions",
          shiny::fileInput("data_file", label = NULL, buttonLabel = "Open data",
                           accept = c(".csv", ".tsv", ".txt", ".rds"),
                           placeholder = "CSV / TSV / RDS"),
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
              shiny::checkboxInput("mark_nonsig", "Mark non-significant exploratory links", TRUE)
            ),
            shiny::conditionalPanel(
              condition = "input.inference_mode == 'search'",
              shiny::radioButtons("ic_criterion", "Criterion", choices = c("AIC" = "aic", "BIC" = "bic"), selected = "bic", inline = TRUE),
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
            )
          )
        ),
        shiny::div(class = "smt-splitter", `data-side` = "left", title = "Drag to resize"),
        shiny::div(
          class = "smt-center",
          shiny::conditionalPanel(
            condition = "input.center_view == 'data'",
            shiny::div(
              class = "smt-center-inner",
              shiny::h3(class = "smt-center-title", "Data"),
              shiny::helpText("Raw loaded data (immutable). Use Prepare to filter or mutate before modeling."),
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
                  shiny::h3(class = "smt-center-title smt-edge-plot-title", "Link relationship"),
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
                  )
                ),
                shiny::div(
                  class = "smt-edge-plot-body",
                  shiny::plotOutput("edge_plot", height = "100%")
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
            shiny::uiOutput("status_msg"),
            shiny::uiOutput("inspector_panel"),
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
  prepare_rv <- shiny::reactiveVal(empty_prepare_state())
  analysis_rv <- shiny::reactiveVal(NULL)
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

  shiny::observeEvent(input$data_file, {
    f <- input$data_file
    shiny::req(f)
    tryCatch({
      dat <- read_data_file(f$datapath, f$name)
      set_raw_data(dat, reset_prepare = TRUE)
      invalidate_fit()
      status(sprintf("Loaded %s (%d rows × %d columns).", f$name, nrow(dat), ncol(dat)))
      shiny::updateRadioButtons(session, "center_view", selected = "data")
    }, error = function(e) status(paste("Data error:", e$message)))
  })

  shiny::observeEvent(input$project_file, {
    f <- input$project_file
    shiny::req(f)
    tryCatch({
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
    }, error = function(e) status(paste("Project error:", e$message)))
  })

  output$save_project <- shiny::downloadHandler(
    filename = function() paste0("structural-model-", Sys.Date(), ".json"),
    content = function(file) {
      prep <- tryCatch(sync_prepare_from_inputs(), error = function(e) prepare_rv())
      writeLines(model_to_json(state(), prepare = prep), file)
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
    DT::datatable(
      dat,
      rownames = FALSE,
      options = list(pageLength = 12, scrollX = TRUE, dom = "tip"),
      class = "compact stripe hover"
    )
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
    st <- add_node(state(), label = trimws(label), observed_column = NA_character_)
    state(st)
    shiny::updateTextInput(session, "new_node_label", value = "")
    shiny::updateRadioButtons(session, "center_view", selected = "model")
  })

  shiny::observeEvent(input$add_latent, {
    label <- input$new_node_label
    if (is.null(label) || !nzchar(trimws(label))) label <- paste0("F", state()$next_node)
    st <- add_node(state(), label = trimws(label), observed_column = NA_character_)
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
  freeze_inspector_inputs <- function() {
    for (nm in c(
      "inspector_map_column",
      "dv_transform",
      "dv_repeated_measures",
      "dv_cluster_column",
      "cat_factor_base",
      "cat_factor_ordered",
      "cat_factor_levels",
      "dv_diag_equation",
      "edge_rel_kind"
    )) {
      try(shiny::freezeReactiveValue(input, nm), silent = TRUE)
    }
  }
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
    freeze_inspector_inputs()
    skip_graph_render(TRUE)
    state(st)
  }, ignoreInit = TRUE)

  # Push graph to HTML editor when model structure / results / data change.
  # Read/clear skip with isolate so toggling it does not schedule a second render
  # (which would rebuild the canvas and remount the inspector after a drag).
  shiny::observe({
    st <- state()
    dat <- analysis_rv()
    if (isTRUE(shiny::isolate(skip_graph_render()))) {
      shiny::isolate(skip_graph_render(FALSE))
      return()
    }
    session$sendCustomMessage("smt_render_graph", graph_payload(st, dat))
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
    freeze_inspector_inputs()
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    st <- ensure_edge_to_port(state())
    rk <- "linear"
    if (!is.null(eid) && eid %in% st$edges$id) {
      rk <- st$edges$rel_kind[match(eid, st$edges$id)]
      if (is.na(rk) || !nzchar(rk)) rk <- "linear"
    }
    current <- as.character(input$edge_rel_kind %||% "")
    if (!identical(current, as.character(rk))) {
      skip_rel_kind_write(TRUE)
      shiny::updateSelectInput(session, "edge_rel_kind", selected = rk)
    } else {
      skip_rel_kind_write(FALSE)
    }
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$edge_rel_kind, {
    if (isTRUE(skip_rel_kind_write())) {
      skip_rel_kind_write(FALSE)
      return()
    }
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (is.null(eid) || !nzchar(eid)) return()
    rk <- input$edge_rel_kind %||% "linear"
    skip_graph_render(TRUE)
    st <- set_edge_rel_kind(state(), eid, rk)
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
    sel <- input$graph_selected
    eid <- NULL
    if (!is.null(sel$edges) && length(sel$edges)) {
      eid <- as.character(sel$edges[[1]])
    }
    if (is.null(eid) || !nzchar(eid)) {
      return(edge_plot_placeholder())
    }
    dat <- analysis_rv()
    if (is.null(dat)) {
      return(edge_plot_message("No data", "Load a dataset to plot this link."))
    }
    st <- ensure_edge_to_port(state())
    idx <- match(eid, st$edges$id)
    rk <- "linear"
    if (!is.na(idx)) {
      stored <- st$edges$rel_kind[idx]
      if (!is.na(stored) && nzchar(stored)) rk <- stored
    }
    if (!is.null(input$edge_rel_kind) && nzchar(as.character(input$edge_rel_kind))) {
      rk <- as.character(input$edge_rel_kind)
    }
    show_fit <- !is.null(last_result())
    p <- edge_relationship_plot(
      st, dat, eid,
      show_fit = show_fit,
      rel_kind = rk
    )
    if (is.null(p)) edge_plot_placeholder() else p
  })

  shiny::observeEvent(input$run_analysis, {
    st <- ensure_edge_to_port(state())
    dat <- analysis_rv()
    # Flush UI relationship kind onto the selected edge before fitting
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (!is.null(eid) && nzchar(eid) && !is.null(input$edge_rel_kind) &&
          nzchar(as.character(input$edge_rel_kind))) {
      st <- set_edge_rel_kind(st, eid, as.character(input$edge_rel_kind))
      state(st)
    }
    tryCatch({
      unc <- input$uncertainty_method %||% "none"
      if (!unc %in% c("none", "bootstrap", "plausible")) unc <- "none"
      draws <- input$uncertainty_draws
      if (identical(unc, "plausible") && (is.null(draws) || !is.finite(draws))) draws <- 20
      if (identical(unc, "bootstrap") && (is.null(draws) || !is.finite(draws))) draws <- 199
      # Sensible defaults when user leaves Draws at 199 but switches to PV
      if (identical(unc, "plausible") && isTRUE(draws >= 100)) {
        # keep user value if intentional; no auto-clamp
      }
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
      shiny::withProgress(message = "Running analysis…", value = 0.1, {
        status("Running analysis…")
        shiny::setProgress(value = 0.2, detail = "Fitting model")
        res <- if (identical(input$inference_mode, "pvalue")) {
          fit_pvalue_model(
            st, dat,
            alpha = input$alpha,
            mark_nonsig = isTRUE(input$mark_nonsig),
            uncertainty = unc,
            uncertainty_draws = draws,
            uncertainty_progress = progress_cb
          )
        } else {
          fit_aic_bic_search(
            st, dat,
            criterion = input$ic_criterion,
            uncertainty = unc,
            uncertainty_draws = draws,
            uncertainty_progress = progress_cb
          )
        }
        shiny::setProgress(value = 0.95, detail = "Updating canvas")
        if (!is.null(res$comparison) && isTRUE(input$model_average) &&
              !is.null(res$state_averaged)) {
          res$model_averaged <- TRUE
          state(res$state_averaged)
        } else {
          if (!is.null(res$comparison)) res$model_averaged <- FALSE
          state(res$state)
        }
        last_result(res)
        unc_info <- res$uncertainty
        unc_txt <- if (!is.null(unc_info) && !identical(unc_info$method %||% "none", "none")) {
          sprintf(
            " · %s uncertainty (%d/%d draws ok)",
            unc_info$method, as.integer(unc_info$ok %||% 0L), as.integer(unc_info$draws %||% 0L)
          )
        } else {
          ""
        }
        status(
          if (isTRUE(res$model_averaged)) {
            paste0("Analysis complete (full model averaging on canvas).", unc_txt)
          } else {
            paste0("Analysis complete.", unc_txt)
          }
        )
      })
      shiny::updateRadioButtons(session, "center_view", selected = "model")
    }, error = function(e) {
      status(paste("Error:", e$message))
      last_result(NULL)
    })
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
    DT::datatable(
      dat,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
      class = "compact stripe hover"
    )
  })

  output$status_msg <- shiny::renderUI({
    msg <- status()
    if (is.null(msg) || !nzchar(msg)) return(NULL)
    if (startsWith(msg, "Analysis complete")) {
      shiny::div(
        class = "smt-status smt-status-ok",
        shiny::tags$i(class = "fas fa-check smt-status-icon", `aria-hidden` = "true"),
        shiny::span(msg)
      )
    } else if (startsWith(msg, "Error:") || startsWith(msg, "Prepare error:")) {
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

  apply_factor_coding_from_inputs <- function() {
    nid <- selected_factor_node_id()
    if (is.null(nid)) return()
    # Inspector remount can briefly leave selects NULL — ignore those flushes
    if (is.null(input$cat_factor_base)) return()
    dat <- analysis_rv()
    base <- input$cat_factor_base %||% NA_character_
    ordered <- isTRUE(input$cat_factor_ordered)
    lev_txt <- input$cat_factor_levels %||% ""
    levels <- if (nzchar(trimws(lev_txt))) {
      decode_factor_levels(lev_txt)
    } else {
      character()
    }
    # If order omitted, keep existing / data order with base first
    if (!length(levels)) {
      coding <- variable_coding_for_node(state(), nid, dat)
      levels <- coding$all_levels
    }
    if (!is.na(base) && nzchar(base) && length(levels)) {
      levels <- c(base, setdiff(levels, base))
    }
    cur <- node_factor_coding(state(), nid)
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
    apply_factor_coding_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$cat_factor_ordered, {
    apply_factor_coding_from_inputs()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$cat_factor_levels, {
    apply_factor_coding_from_inputs()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

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
    } else {
      "Currently latent (no data column)."
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

  shiny::observeEvent(input$inspector_map_column, {
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
        shiny::helpText(
          "Top to bottom = factor order. The base level is moved first for contrasts ",
          "when you pick it above. Split ports follow non-base levels in this order."
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
          "continuous"
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
              shiny::helpText(
                "Latents: measurement links build factor scores first, then structural ",
                "links use the usual path engine (lm/glm, AIC/BIC, split logistic)."
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
            },
            shiny::h4("Residual diagnostics"),
            if (!length(fits_dv)) {
              shiny::helpText("Run the analysis to see residual diagnostics for this DV.")
            } else {
              keys <- names(fits_dv)
              shiny::tagList(
                if (length(keys) > 1L) {
                  shiny::selectInput(
                    "dv_diag_equation",
                    "Equation",
                    choices = stats::setNames(
                      keys,
                      vapply(keys, function(k) dv_fit_key_label(st, k), character(1))
                    ),
                    selected = keys[[1]],
                    width = "100%"
                  )
                },
                shiny::div(
                  class = "smt-resid-plot-wrap",
                  shiny::plotOutput("dv_resid_plot", height = "320px")
                ),
                shiny::helpText(
                  "QQ checks residual normality; residuals vs fitted checks ",
                  "non-linearity / heteroscedasticity. For glm/glmer, deviance ",
                  "residuals are shown."
                )
              )
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
          map_ui
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

  output$dv_resid_plot <- shiny::renderPlot({
    dv <- selected_dv_id()
    if (is.null(dv)) {
      return(residual_diagnostics_plot(NULL, title = NULL))
    }
    res <- last_result()
    fits_dv <- dv_result_fits(res, dv)
    if (!length(fits_dv)) {
      return(residual_diagnostics_plot(NULL, title = NULL))
    }
    keys <- names(fits_dv)
    key <- input$dv_diag_equation %||% keys[[1]]
    if (is.null(key) || !key %in% keys) key <- keys[[1]]
    residual_diagnostics_plot(
      fits_dv[[key]],
      title = dv_fit_key_label(state(), key)
    )
  })
  # plotOutput is created dynamically in the inspector; keep it ready when hidden
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
