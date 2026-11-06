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
          class = "smt-brand",
          shiny::span(class = "smt-brand-mark", "SMT"),
          shiny::span(class = "smt-brand-name", "Structural Model Tester")
        ),
        shiny::div(
          class = "smt-top-actions",
          shiny::fileInput("data_file", label = NULL, buttonLabel = "Open data",
                           accept = c(".csv", ".tsv", ".txt", ".rds"),
                           placeholder = "CSV / TSV / RDS"),
          shiny::fileInput("project_file", label = NULL, buttonLabel = "Open project",
                           accept = ".json", placeholder = "Project JSON"),
          shiny::downloadButton("save_project", "Save project", class = "smt-btn smt-btn-ghost"),
          shiny::actionButton("run_analysis", "Run", class = "smt-btn smt-btn-primary"),
          shiny::div(
            class = "smt-view-toggle",
            shiny::radioButtons(
              "center_view", NULL,
              choiceNames = list("Data", "Model"),
              choiceValues = list("data", "model"),
              selected = "model", inline = TRUE
            )
          )
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
            shiny::actionButton("add_node", "Add node", class = "smt-btn smt-btn-secondary smt-btn-block"),
            shiny::actionButton("add_latent", "Add latent", class = "smt-btn smt-btn-ghost smt-btn-block"),
            shiny::hr(class = "smt-hr"),
            shiny::h4("Interaction"),
            shiny::actionButton("add_interaction", "Add × node", class = "smt-btn smt-btn-secondary smt-btn-block"),
            shiny::helpText("Wire two predictors into ×, then drag × to a DV. Main-effect links are added automatically (same type as × → DV)."),
            shiny::selectInput(
              "link_type", "Link type for new connections",
              choices = c(
                "Fixed" = "fixed",
                "Confirmatory" = "confirmatory",
                "Exploratory" = "exploratory",
                "Measurement (latent → indicator)" = "measurement"
              ),
              selected = "confirmatory"
            ),
            shiny::helpText("Drag from a right-hand port to a left-hand input. Categorical nodes: split = one level (incl. into ×); omnibus = whole factor (all interactions)."),
            shiny::actionButton("delete_selected", "Delete selected", class = "smt-btn smt-btn-danger smt-btn-block"),
            shiny::helpText("Or press Delete / Backspace."),
            shiny::hr(class = "smt-hr"),
            shiny::h3("Map column → node"),
            shiny::uiOutput("mapping_controls"),
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
              DT::DTOutput("data_table")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.center_view == 'model'",
            shiny::div(
              class = "smt-center-inner smt-canvas-wrap",
              shiny::h3(class = "smt-center-title", "Model"),
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
            shiny::h3("Results"),
            shiny::verbatimTextOutput("status_msg", placeholder = TRUE),
            shiny::h4("Model fit"),
            shiny::uiOutput("fit_equations"),
            DT::DTOutput("fit_table"),
            shiny::h4("Path coefficients"),
            DT::DTOutput("coef_table"),
            shiny::h4("Model comparison"),
            DT::DTOutput("comparison_table"),
            shiny::h4("lavaan syntax"),
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
  last_result <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("Load data, build a model, then Run.")

  sample_path <- system.file("sample_data", "demo.csv", package = "StructuralModelTester")
  if (!nzchar(sample_path) || !file.exists(sample_path)) {
    dev_root <- tryCatch(get_dev_pkg_root(), error = function(e) "")
    if (nzchar(dev_root)) {
      sample_path <- file.path(dev_root, "inst", "sample_data", "demo.csv")
    }
  }

  # Load demo data on startup if present
  shiny::observe({
    if (file.exists(sample_path) && is.null(data_rv())) {
      dat <- read_data_file(sample_path, "demo.csv")
      data_rv(dat)
      status("Demo data loaded. Map columns to nodes or open your own file.")
    }
  })

  shiny::observeEvent(input$data_file, {
    f <- input$data_file
    shiny::req(f)
    tryCatch({
      dat <- read_data_file(f$datapath, f$name)
      data_rv(dat)
      status(sprintf("Loaded %s (%d rows × %d columns).", f$name, nrow(dat), ncol(dat)))
      shiny::updateRadioButtons(session, "center_view", selected = "data")
    }, error = function(e) status(paste("Data error:", e$message)))
  })

  shiny::observeEvent(input$project_file, {
    f <- input$project_file
    shiny::req(f)
    tryCatch({
      txt <- paste(readLines(f$datapath, warn = FALSE), collapse = "\n")
      state(model_from_json(txt))
      status("Project loaded.")
      shiny::updateRadioButtons(session, "center_view", selected = "model")
    }, error = function(e) status(paste("Project error:", e$message)))
  })

  output$save_project <- shiny::downloadHandler(
    filename = function() paste0("structural-model-", Sys.Date(), ".json"),
    content = function(file) {
      writeLines(model_to_json(state()), file)
    }
  )

  output$variable_list <- shiny::renderUI({
    dat <- data_rv()
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

  output$mapping_controls <- shiny::renderUI({
    st <- ensure_node_kind_columns(state())
    dat <- data_rv()
    mappable <- st$nodes[st$nodes$node_kind != "interaction", , drop = FALSE]
    if (!nrow(mappable)) return(shiny::div(class = "smt-muted", "Add nodes first."))
    cols <- if (is.null(dat)) character() else c("(latent — none)" = "", names(dat))
    shiny::tagList(
      shiny::selectInput("map_node", "Node", choices = setNames(mappable$id, mappable$label)),
      shiny::selectInput("map_column", "Data column", choices = cols),
      shiny::actionButton("apply_map", "Apply mapping", class = "smt-btn smt-btn-secondary smt-btn-block")
    )
  })

  shiny::observeEvent(input$apply_map, {
    shiny::req(input$map_node)
    col <- input$map_column
    if (is.null(col) || col == "") col <- NA_character_
    st <- map_column_to_node(state(), input$map_node, col)
    state(clear_fit_results(st))
    status("Mapping updated.")
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
    if (!nzchar(from) || !nzchar(to) || identical(from, to)) return()
    link_type <- input$link_type %||% "confirmatory"
    before <- nrow(state()$edges)
    st <- add_edge(state(), from, to, type = link_type, level = level, to_port = to_port)
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

  # Persist node coordinates after move without rebuilding the HTML graph
  skip_graph_render <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$smt_node_moved, {
    mv <- input$smt_node_moved
    shiny::req(mv, mv$id)
    st <- apply_node_position(state(), mv$id, mv$x, mv$y)
    skip_graph_render(TRUE)
    state(st)
  }, ignoreInit = TRUE)

  # Push graph to HTML editor when model structure / results / data change
  shiny::observe({
    payload <- graph_payload(state(), data_rv())
    if (isTRUE(skip_graph_render())) {
      skip_graph_render(FALSE)
      return()
    }
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

  shiny::observeEvent(input$delete_selected, {
    delete_current_selection()
  })

  shiny::observeEvent(input$smt_delete_selection, {
    delete_current_selection()
  }, ignoreInit = TRUE)

  # Sync relationship-kind control from the selected edge.
  skip_rel_kind_write <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$graph_selected, {
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
    st <- set_node_cat_mode(state(), as.character(msg$id), mode, data = data_rv())
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
    dat <- data_rv()
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
    dat <- data_rv()
    # Flush UI relationship kind onto the selected edge before fitting
    sel <- input$graph_selected
    eid <- if (!is.null(sel$edges) && length(sel$edges)) as.character(sel$edges[[1]]) else NULL
    if (!is.null(eid) && nzchar(eid) && !is.null(input$edge_rel_kind) &&
          nzchar(as.character(input$edge_rel_kind))) {
      st <- set_edge_rel_kind(st, eid, as.character(input$edge_rel_kind))
      state(st)
    }
    tryCatch({
      status("Running analysis…")
      res <- if (identical(input$inference_mode, "pvalue")) {
        fit_pvalue_model(st, dat, alpha = input$alpha, mark_nonsig = isTRUE(input$mark_nonsig))
      } else {
        fit_aic_bic_search(st, dat, criterion = input$ic_criterion)
      }
      if (!is.null(res$comparison) && isTRUE(input$model_average) &&
            !is.null(res$state_averaged)) {
        res$model_averaged <- TRUE
        state(res$state_averaged)
      } else {
        if (!is.null(res$comparison)) res$model_averaged <- FALSE
        state(res$state)
      }
      last_result(res)
      status(
        if (isTRUE(res$model_averaged)) {
          "Analysis complete (full model averaging on canvas)."
        } else {
          "Analysis complete."
        }
      )
      shiny::updateRadioButtons(session, "center_view", selected = "model")
    }, error = function(e) {
      status(paste("Error:", e$message))
      last_result(NULL)
    })
  })

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

  output$status_msg <- shiny::renderText(status())

  output$fit_equations <- shiny::renderUI({
    res <- last_result()
    if (is.null(res)) return(NULL)
    if (isTRUE(res$model_averaged)) {
      format_averaged_equations_html(res, state())
    } else {
      format_best_equations_html(res, state())
    }
  })

  output$fit_table <- DT::renderDT({
    res <- last_result()
    if (is.null(res)) return(NULL)
    DT::datatable(
      format_fit_measures_display(res$fit_measures),
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE),
      class = "compact"
    )
  })

  output$coef_table <- DT::renderDT({
    res <- last_result()
    # When averaging, coefficients live on state edges already
    est <- if (!is.null(res) && !isTRUE(res$model_averaged)) res$estimates else NULL
    DT::datatable(
      estimates_table(state(), estimates = est),
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
