sample_raw <- function(n = 6L) {
  data.frame(
    Age = c(20, 25, 30, 35, 40, 45),
    Gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
    Method = c("T", "T", "I", "I", "T", "I"),
    Score = c(50, 55, 60, 65, 70, 75),
    stringsAsFactors = FALSE
  )
}

#' Load one built-in example (data + model state), or skip if files missing
smt_example_bundle <- function(id) {
  ex <- StructuralModelTester:::smt_example_resolved(id)
  testthat::skip_if(
    is.null(ex) || !nzchar(ex$data_path %||% "") || !nzchar(ex$model_path %||% ""),
    paste0("example '", id, "' files missing")
  )
  list(
    id = id,
    title = ex$title %||% id,
    dat = utils::read.csv(ex$data_path, stringsAsFactors = FALSE),
    state = StructuralModelTester:::model_from_json(
      paste(readLines(ex$model_path, warn = FALSE), collapse = "\n")
    )
  )
}

#' Text labels drawn via annotate()/geom_text (message plots, interaction deltas)
smt_ggplot_text_labels <- function(p) {
  if (is.null(p) || !inherits(p, "ggplot")) return(character())
  built <- tryCatch(
    suppressWarnings(ggplot2::ggplot_build(p)),
    error = function(e) NULL
  )
  if (is.null(built)) return(character())
  out <- character()
  for (d in built$data) {
    if (is.data.frame(d) && "label" %in% names(d)) {
      out <- c(out, as.character(d$label))
    }
  }
  out[!is.na(out) & nzchar(out)]
}

#' Assert an edge link plot rendered (real ggplot or intentional message panel)
expect_smt_edge_plot_ok <- function(p, info = NULL) {
  testthat::expect_false(is.null(p), info = info)
  testthat::expect_true(inherits(p, "ggplot"), info = info)
  texts <- smt_ggplot_text_labels(p)
  title <- as.character(p$labels$title %||% "")
  bad <- any(texts == "Plot error") || identical(title, "Plot error") ||
    any(grepl("^pas de composante|^object .+ not found|^Error in", texts))
  testthat::expect_false(
    bad,
    info = paste(c(info, title, texts), collapse = " | ")
  )
}

#' Framework-specific path-coefficients table checks
expect_smt_estimates_table <- function(tab, framework, info = NULL) {
  testthat::expect_true(is.data.frame(tab), info = info)
  testthat::expect_true(nrow(tab) >= 1L, info = info)
  testthat::expect_true(
    all(c("Path", "Estimate") %in% names(tab)),
    info = paste(info, paste(names(tab), collapse = ","))
  )
  framework <- as.character(framework)[[1]]
  if (identical(framework, "pvalue")) {
    testthat::expect_true(
      "p" %in% names(tab) || "SE" %in% names(tab),
      info = info
    )
  } else if (identical(framework, "search")) {
    testthat::expect_true("Σw" %in% names(tab), info = info)
  } else if (identical(framework, "bayes")) {
    testthat::expect_true(
      any(grepl("% HDI$", names(tab))) || "pd" %in% names(tab),
      info = paste(info, paste(names(tab), collapse = ","))
    )
  }
}

#' Residual diagnostics for classical / search path fits
expect_smt_residual_diagnostics <- function(result, info = NULL) {
  testthat::expect_true(is.list(result$fit) && length(result$fit) >= 1L, info = info)
  for (nm in names(result$fit)) {
    fit <- result$fit[[nm]]
    tag <- paste(info, "dv=", nm)
    ri <- StructuralModelTester:::path_fit_residual_info(fit)
    testthat::expect_false(is.null(ri), info = tag)
    testthat::expect_true(sum(is.finite(ri$resid)) >= 3L, info = tag)
    p <- StructuralModelTester:::residual_diagnostics_plot(fit, title = nm)
    testthat::expect_true(inherits(p, "ggplot"), info = tag)
  }
}

#' MCMC strip + per-DV MCMC/PPC for Bayesian results
expect_smt_bayes_diagnostics <- function(result, state, info = NULL) {
  testthat::expect_identical(result$sampler, "brms", info = info)
  # Short MCMC in tests can trip ESS-capping warnings from posterior::
  agg <- suppressWarnings(
    StructuralModelTester:::brms_mcmc_diagnostics_aggregate(result, state)
  )
  testthat::expect_true(agg$n_models >= 1L, info = info)
  testthat::expect_true(agg$status %in% c("ok", "warn", "bad"), info = info)
  html <- StructuralModelTester:::format_mcmc_health_html(agg)
  testthat::expect_true(
    inherits(html, "shiny.tag") || inherits(html, "html"),
    info = info
  )
  for (nm in names(result$fit)) {
    fit <- result$fit[[nm]]
    tag <- paste(info, "dv=", nm)
    testthat::expect_true(inherits(fit, "brmsfit"), info = tag)
    d <- suppressWarnings(StructuralModelTester:::brms_mcmc_diagnostics(fit))
    testthat::expect_true(d$status %in% c("ok", "warn", "bad"), info = tag)
    p <- suppressWarnings(
      StructuralModelTester:::mcmc_diagnostics_plot(fit, title = nm)
    )
    testthat::expect_true(inherits(p, "ggplot"), info = tag)
    ppc <- suppressWarnings(
      StructuralModelTester:::brms_ppc_plot(fit, title = nm, ndraws = 20L)
    )
    testthat::expect_true(inherits(ppc, "ggplot"), info = tag)
  }
}

#' Plot every edge: before analysis, or after with optional fit overlay / Bayes strip
expect_smt_all_edge_plots <- function(state, dat, result = NULL, info = NULL) {
  st <- StructuralModelTester:::ensure_edge_to_port(state)
  n <- nrow(st$edges)
  testthat::expect_true(n >= 1L, info = info)
  show_fit <- !is.null(result)
  for (i in seq_len(n)) {
    eid <- st$edges$id[[i]]
    from_id <- st$edges$from[[i]]
    to_id <- st$edges$to[[i]]
    etype <- st$edges$type[[i]]
    tag <- paste0(
      info, " edge=", eid, " ", from_id, "→", to_id, " (", etype, ")"
    )
    to_level <- if ("to_level" %in% names(st$edges)) {
      st$edges$to_level[[i]]
    } else {
      NA_character_
    }
    edge_fit <- if (isTRUE(show_fit)) {
      StructuralModelTester:::edge_fit_for_plot(result, to_id, to_level = to_level)
    } else {
      NULL
    }
    built <- tryCatch(
      list(
        ok = TRUE,
        plot = suppressWarnings(
          StructuralModelTester:::edge_relationship_plot(
            st, dat, eid,
            show_fit = isTRUE(show_fit) && !is.null(edge_fit),
            fit = edge_fit
          )
        ),
        error = NULL
      ),
      error = function(e) {
        list(ok = FALSE, plot = NULL, error = conditionMessage(e))
      }
    )
    testthat::expect_true(
      isTRUE(built$ok),
      info = paste(tag, built$error %||% "edge_relationship_plot failed")
    )
    p <- built$plot
    if (!isTRUE(built$ok)) next
    if (is.null(p)) {
      testthat::expect_true(
        FALSE,
        info = paste(tag, "edge_relationship_plot returned NULL")
      )
      next
    }
    # Fail fast on error panels — do not let a Bayes strip compose over them
    # (compose_edge_bayes_plot can replace a "Plot error" message plot).
    expect_smt_edge_plot_ok(p, info = tag)
    if (identical(result$sampler %||% "", "brms") && isTRUE(built$ok)) {
      info_b <- tryCatch(
        StructuralModelTester:::edge_bayes_param_info(
          result, st, eid, degree = 1L
        ),
        error = function(e) NULL
      )
      if (!is.null(info_b)) {
        p2 <- tryCatch(
          suppressWarnings(
            StructuralModelTester:::compose_edge_bayes_plot(p, info_b)
          ),
          error = function(e) {
            testthat::expect_true(
              FALSE,
              info = paste(tag, "compose_edge_bayes_plot:", conditionMessage(e))
            )
            p
          }
        )
        expect_smt_edge_plot_ok(p2, info = paste(tag, "bayes-strip"))
      }
    }
  }
}

binary_node_state <- function(label = "Method",
                              base = "T",
                              levels = c("T", "I"),
                              column = label) {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st,
    label = label,
    observed_column = column,
    kind = "observed"
  )
  nid <- st$nodes$id[[1]]
  StructuralModelTester:::set_node_factor_coding(
    st,
    nid,
    base = base,
    levels = levels,
    data = NULL
  )
}
