#' Topological order of structural DV node ids
#' @keywords internal
structural_dv_topo_order <- function(state) {
  state <- ensure_edge_to_port(state)
  dvs <- structural_dv_ids(state)
  if (!length(dvs)) return(character())
  struct <- state$edges[
    !state$edges$type %in% c("measurement", "definitional") &
      state$edges$to %in% dvs,
    ,
    drop = FALSE
  ]
  if (!nrow(struct)) return(dvs)

  # Predecessors that are themselves structural DVs (mediators)
  pred_dvs <- lapply(dvs, function(dv) {
    fr <- unique(as.character(struct$from[struct$to == dv]))
    intersect(fr, dvs)
  })
  names(pred_dvs) <- dvs

  remaining <- dvs
  ordered <- character()
  while (length(remaining)) {
    ready <- remaining[
      vapply(remaining, function(dv) {
        all(pred_dvs[[dv]] %in% ordered)
      }, logical(1))
    ]
    if (!length(ready)) {
      # Cycle or incomplete — append rest in current order
      return(c(ordered, remaining))
    }
    ordered <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }
  ordered
}

#' Column name used when writing simulated variables
#' @keywords internal
node_sim_column <- function(state, node_id) {
  lavaan_var_name(state, node_id)
}

#' Mean-center a numeric vector (0 if empty / all non-finite)
#'
#' Used for continuous predictors in simulation so edge prior means are
#' unstandardized slopes `b` (no intercept in the sim equation).
#' @keywords internal
sim_center <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ok <- is.finite(x)
  out <- rep(0, length(x))
  if (!any(ok)) return(out)
  out[ok] <- x[ok] - mean(x[ok])
  out
}

#' Draw one exogenous simulation column for a node
#' @keywords internal
sim_draw_node_column <- function(state, node_id, n) {
  kind <- node_sim_var_kind(state, node_id, data = NULL)
  if (kind %in% c("categorical", "binary")) {
    levs <- node_sim_factor_levels(state, node_id, data = NULL)
    props <- node_sim_level_props(state, node_id, data = NULL)
    if (!length(levs)) {
      sp <- node_sim_params(state, node_id)
      return(stats::rnorm(n, mean = sp$mean, sd = sp$sd))
    }
    props <- normalize_sim_props(props, levs)
    drawn <- sample(levs, size = n, replace = TRUE, prob = as.numeric(props[levs]))
    return(factor(drawn, levels = levs))
  }
  sp <- node_sim_params(state, node_id)
  stats::rnorm(n, mean = sp$mean, sd = sp$sd)
}

#' Numeric predictor contribution for a simulated edge
#' @keywords internal
sim_edge_predictor <- function(state, dat, from_id, level = NA_character_) {
  from_nm <- node_sim_column(state, from_id)
  x <- dat[[from_nm]]
  kind <- node_sim_var_kind(state, from_id, data = NULL)
  if (kind %in% c("categorical", "binary") || is.factor(x) || is.character(x)) {
    levs <- node_sim_factor_levels(state, from_id, data = NULL)
    base <- levs[[1]]
    x_chr <- as.character(x)
    if (!is.na(level) && nzchar(as.character(level))) {
      return(as.numeric(x_chr == as.character(level)))
    }
    # Omnibus / unleveled: equal prior applied to every non-base level later
    # by returning a matrix of indicators (list) — caller handles vectors.
    non_base <- setdiff(levs, base)
    if (!length(non_base)) return(rep(0, length(x_chr)))
    inds <- lapply(non_base, function(lv) as.numeric(x_chr == lv))
    names(inds) <- non_base
    return(inds)
  }
  # Continuous: unstandardized expected b on mean-centered X
  sim_center(x)
}

#' Numeric series for one side of a simulated interaction (match fit coding)
#'
#' Mirrors `interaction_factor_series` / `factor_numeric_for_product`:
#' binary → 0/1 (`as_binary01`), continuous → mean-centered, categorical →
#' non-base (or level-restricted) indicators. Returns a list of numeric
#' vectors (length 1 for binary/continuous/split; one per non-base level for
#' omnibus categorical).
#' @keywords internal
sim_interaction_factor_series <- function(state, dat, node_id,
                                         level = NA_character_) {
  nm <- node_sim_column(state, node_id)
  x <- dat[[nm]]
  n <- length(x)
  kind <- node_sim_var_kind(state, node_id, data = NULL)
  level <- if (is.null(level) || is.na(level) || !nzchar(as.character(level))) {
    NA_character_
  } else {
    as.character(level)
  }

  if (identical(kind, "categorical")) {
    levs <- node_sim_factor_levels(state, node_id, data = NULL)
    if (!length(levs)) return(list(rep(0, n)))
    base <- levs[[1]]
    non_base <- setdiff(levs, base)
    cm <- node_cat_mode(state, node_id)
    if (!is.na(level)) {
      return(list(as.numeric(as.character(x) == level)))
    }
    if (identical(cm, "omnibus") || !length(non_base)) {
      if (!length(non_base)) return(list(rep(0, n)))
      return(lapply(non_base, function(lv) {
        as.numeric(as.character(x) == lv)
      }))
    }
    # Split categorical without a level: no well-defined product term
    return(list(rep(0, n)))
  }

  if (identical(kind, "binary")) {
    # Match fit: as_binary01 on factor uses level order (base first → 0)
    if (is.factor(x)) {
      return(list(as.numeric(as.integer(droplevels(x)) - 1L)))
    }
    levs <- node_sim_factor_levels(state, node_id, data = NULL)
    if (length(levs) >= 2L) {
      return(list(as.numeric(as.character(x) == levs[[length(levs)]])))
    }
    return(list(as.numeric(as_binary01(x))))
  }

  # Continuous: mean-centered product coding (same as factor_numeric_for_product)
  xv <- suppressWarnings(as.numeric(x))
  ok <- is.finite(xv)
  out <- rep(0, n)
  if (any(ok)) out[ok] <- xv[ok] - mean(xv[ok])
  list(out)
}

#' Interaction product contribution matching the analysis coding
#'
#' For omnibus categorical sides, applies the same prior mean to every
#' non-base × partner product (sum of products), matching how confirmatory
#' omnibus mains reuse one prior across indicators.
#' @keywords internal
sim_interaction_predictor <- function(state, dat, ix_id) {
  fac <- interaction_factors(state, ix_id)
  if (is.na(fac$a) || is.na(fac$b) || !nzchar(fac$a) || !nzchar(fac$b)) {
    return(NULL)
  }
  series_a <- sim_interaction_factor_series(state, dat, fac$a, fac$level_a)
  series_b <- sim_interaction_factor_series(state, dat, fac$b, fac$level_b)
  if (!length(series_a) || !length(series_b)) return(NULL)
  n <- length(series_a[[1]])
  prod <- rep(0, n)
  for (va in series_a) {
    for (vb in series_b) {
      prod <- prod + as.numeric(va) * as.numeric(vb)
    }
  }
  prod
}

#' Simulate a path dataset from priors + node sim knobs
#'
#' Exogenous continuous nodes: Normal(sim_mean, sim_sd).
#' Exogenous categorical/binary: multinomial draws with `sim_level_props`.
#' Structural DVs (topo order): edge prior means × predictors — unstandardized
#' continuous mains (`b` on mean-centered X), treatment-coded factor mains, and
#' interaction products coded like the fit (binary 0/1, continuous centered) —
#' plus N(0, sim_resid_sd) on the outcome scale.
#'
#' @return A data frame with `n` rows.
#' @keywords internal
simulate_path_data <- function(state, n = 500L, seed = 1L) {
  n <- as.integer(n)
  if (!is.finite(n) || n < 2L) {
    stop("Simulation needs at least 2 samples.", call. = FALSE)
  }
  seed <- as.integer(seed)
  if (!is.finite(seed)) seed <- 1L
  # -1 (or any negative): draw a fresh seed for this run
  if (seed < 0L) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  state <- ensure_edge_to_port(state)
  state <- ensure_node_sim_columns(state)
  state <- ensure_node_factor_coding_columns(state)
  state <- ensure_edge_priors(state)

  set.seed(seed)
  dat <- list()

  for (id in state$nodes$id) {
    if (!node_is_structural_exogenous(state, id)) next
    if (is_interaction_node(state, id)) next
    nm <- node_sim_column(state, id)
    dat[[nm]] <- sim_draw_node_column(state, id, n)
  }

  ensure_col <- function(id) {
    if (is_interaction_node(state, id)) return(invisible(NULL))
    nm <- node_sim_column(state, id)
    if (nm %in% names(dat)) return(invisible(NULL))
    dat[[nm]] <<- sim_draw_node_column(state, id, n)
    invisible(NULL)
  }

  dv_order <- structural_dv_topo_order(state)
  struct <- state$edges[
    !state$edges$type %in% c("measurement", "definitional"),
    ,
    drop = FALSE
  ]

  for (dv in dv_order) {
    dv_nm <- node_sim_column(state, dv)
    inc <- struct[struct$to == dv, , drop = FALSE]
    eta <- rep(0, n)

    if (nrow(inc)) {
      for (i in seq_len(nrow(inc))) {
        eid <- inc$id[[i]]
        from_id <- inc$from[[i]]
        beta <- edge_prior(state, eid)$mean
        if (!is.finite(beta)) beta <- 0

        if (is_interaction_node(state, from_id)) {
          fac <- interaction_factors(state, from_id)
          if (is.na(fac$a) || is.na(fac$b) || !nzchar(fac$a) || !nzchar(fac$b)) next
          ensure_col(fac$a)
          ensure_col(fac$b)
          prod <- sim_interaction_predictor(state, dat, from_id)
          if (is.null(prod)) next
          eta <- eta + beta * prod
          next
        }

        ensure_col(from_id)
        lv <- if ("level" %in% names(inc)) inc$level[[i]] else NA_character_
        pred <- sim_edge_predictor(state, dat, from_id, level = lv)
        if (is.list(pred)) {
          # Omnibus: same prior mean on each non-base indicator
          for (ind in pred) {
            eta <- eta + beta * ind
          }
        } else {
          eta <- eta + beta * pred
        }
      }
    }

    resid_sd <- node_sim_params(state, dv)$resid_sd
    if (!is.finite(resid_sd) || resid_sd <= 0) resid_sd <- 1
    # Factor DVs are not simulated yet — always continuous outcome scale
    dat[[dv_nm]] <- eta + stats::rnorm(n, mean = 0, sd = resid_sd)
  }

  if (!length(dat)) {
    stop(
      "Nothing to simulate. Add exogenous continuous nodes and/or structural links.",
      call. = FALSE
    )
  }
  out <- as.data.frame(dat, optional = TRUE, stringsAsFactors = FALSE)
  # Preserve factor columns (as.data.frame may strip with optional=TRUE in some cases)
  for (nm in names(dat)) {
    if (is.factor(dat[[nm]])) out[[nm]] <- dat[[nm]]
  }
  attr(out, "sim_seed") <- as.integer(seed)
  out
}

#' Temporarily map unmapped observed nodes onto simulated column names for fitting
#'
#' Does not invent mappings for true latents (those still need measurement links).
#' Safe to use only on a copy of state passed into the fit engine — restore
#' original `observed_column` / `node_kind` before writing state back to the UI.
#'
#' @keywords internal
bind_sim_columns_to_state <- function(state, data) {
  if (is.null(data) || !ncol(data)) return(state)
  state <- ensure_node_kind_columns(state)
  for (i in seq_len(nrow(state$nodes))) {
    id <- state$nodes$id[[i]]
    if (is_interaction_node(state, id)) next
    if (is_latent_node(state, id)) next
    col <- state$nodes$observed_column[[i]]
    if (!is.na(col) && nzchar(col) && col %in% names(data)) next
    nm <- node_sim_column(state, id)
    if (!nm %in% names(data)) next
    state$nodes$observed_column[[i]] <- nm
    state$nodes$node_kind[[i]] <- "observed"
  }
  state
}

#' Restore observed mappings after a simulated fit
#' @keywords internal
restore_column_mappings <- function(fitted_state, source_state) {
  if (is.null(fitted_state) || is.null(source_state)) return(fitted_state)
  fitted_state <- ensure_node_kind_columns(fitted_state)
  source_state <- ensure_node_kind_columns(source_state)
  idx <- match(fitted_state$nodes$id, source_state$nodes$id)
  ok <- !is.na(idx)
  if (!any(ok)) return(fitted_state)
  fitted_state$nodes$observed_column[ok] <- source_state$nodes$observed_column[idx[ok]]
  fitted_state$nodes$node_kind[ok] <- source_state$nodes$node_kind[idx[ok]]
  fitted_state
}

#' Prefer analysis data only when nodes are mapped; else simulated data
#'
#' Analysis columns are used only via `observed_column` mappings — never by
#' matching an unmapped node id/label to a dataset column.
#'
#' @param node_ids Character vector of endpoint node ids (and interaction factors).
#' @return list(data, simulated) or list(data=NULL, simulated=FALSE, reason=...)
#' @keywords internal
pick_plot_data_for_nodes <- function(state, analysis, sim, node_ids) {
  node_ids <- unique(as.character(node_ids))
  node_ids <- node_ids[!is.na(node_ids) & nzchar(node_ids)]
  node_ids <- node_ids[
    !vapply(node_ids, function(id) is_interaction_node(state, id), logical(1))
  ]
  col_ok <- function(dat, col) {
    !is.null(col) && !is.na(col) && nzchar(col) &&
      !is.null(dat) && col %in% names(dat) && sum(!is.na(dat[[col]])) > 0L
  }

  mapped <- vapply(node_ids, function(id) node_analysis_column(state, id), character(1))
  all_mapped <- length(mapped) > 0L && all(!is.na(mapped) & nzchar(mapped))
  if (all_mapped && all(vapply(mapped, function(c) col_ok(analysis, c), logical(1)))) {
    return(list(data = analysis, simulated = FALSE))
  }

  plot_cols <- vapply(node_ids, function(id) node_plot_column(state, id), character(1))
  if (length(plot_cols) && all(vapply(plot_cols, function(c) col_ok(sim, c), logical(1)))) {
    return(list(data = sim, simulated = TRUE))
  }

  missing <- character()
  if (!all_mapped) {
    missing <- c(missing, node_ids[is.na(mapped) | !nzchar(mapped)])
  } else {
    missing <- c(missing, mapped[!vapply(mapped, function(c) col_ok(analysis, c), logical(1))])
  }
  missing_sim <- plot_cols[!vapply(plot_cols, function(c) col_ok(sim, c), logical(1))]
  list(
    data = NULL,
    simulated = FALSE,
    reason = paste0(
      "No plottable data. ",
      if (length(missing_sim)) {
        paste0(
          "Run Simulate (missing sim columns: ",
          paste(unique(missing_sim), collapse = ", "),
          "). "
        )
      } else {
        ""
      },
      "Map nodes to columns to use the analysis dataset."
    )
  )
}

#' Prefer analysis columns when present; else simulated data for an edge pair
#' @keywords internal
pick_edge_plot_data <- function(analysis, sim, from_col, to_col) {
  # Legacy helper: column names only (no mapping check). Prefer pick_plot_data_for_nodes.
  cols <- c(from_col, to_col)
  cols <- unique(as.character(cols))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  col_ok <- function(dat, col) {
    !is.null(dat) && col %in% names(dat) && sum(!is.na(dat[[col]])) > 0L
  }
  if (length(cols) && all(vapply(cols, function(c) col_ok(analysis, c), logical(1)))) {
    return(list(data = analysis, simulated = FALSE))
  }
  if (length(cols) && all(vapply(cols, function(c) col_ok(sim, c), logical(1)))) {
    return(list(data = sim, simulated = TRUE))
  }
  list(
    data = NULL,
    simulated = FALSE,
    reason = "Load data or run Simulate to plot this link."
  )
}
