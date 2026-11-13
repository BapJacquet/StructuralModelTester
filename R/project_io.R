#' Serialize model state to JSON
#'
#' @param state Model state list.
#' @return JSON string.
#' @export
model_to_json <- function(state, prepare = NULL) {
  payload <- list(
    version = 1L,
    nodes = state$nodes,
    edges = state$edges,
    next_node = state$next_node,
    next_edge = state$next_edge
  )
  if (!is.null(prepare)) {
    payload$prepare <- prepare_for_json(prepare)
  }
  jsonlite::toJSON(payload, dataframe = "rows", null = "null", auto_unbox = TRUE, pretty = TRUE)
}

#' Restore model state from JSON
#'
#' @param json JSON string or path handled by caller as text.
#' @return Model state list.
#' @export
model_from_json <- function(json) {
  payload <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)
  state <- empty_model_state()

  nodes <- as.data.frame(payload$nodes, stringsAsFactors = FALSE)
  edges <- as.data.frame(payload$edges, stringsAsFactors = FALSE)

  # Ensure columns exist
  need_n <- c(
    "id", "label", "observed_column", "shape", "x", "y",
    "r_squared", "r_squared_kind", "node_kind", "factor_a", "factor_b", "cat_mode",
    "repeated_measures", "cluster_column", "dv_transform",
    "factor_base", "factor_ordered", "factor_levels",
    "sim_mean", "sim_sd", "sim_min", "sim_max", "sim_intercept", "sim_resid_sd",
    "sim_var_kind", "sim_level_props", "sim_level_counts", "sim_design_mode",
    "sim_alloc_mode"
  )
  for (nm in need_n) {
    if (!nm %in% names(nodes)) {
      nodes[[nm]] <- if (nm == "shape") {
        "ellipse"
      } else if (nm %in% c("x", "y", "r_squared", "sim_min", "sim_max")) {
        NA_real_
      } else if (nm %in% c("sim_mean", "sim_intercept")) {
        0
      } else if (nm %in% c("sim_sd", "sim_resid_sd")) {
        1
      } else if (nm == "cat_mode") {
        "split"
      } else if (nm == "sim_var_kind") {
        "continuous"
      } else if (nm == "sim_design_mode") {
        "inter"
      } else if (nm == "sim_alloc_mode") {
        "probability"
      } else if (nm %in% c("repeated_measures", "factor_ordered")) {
        FALSE
      } else if (nm == "dv_transform") {
        "none"
      } else {
        NA_character_
      }
    }
  }
  if (!nrow(nodes)) {
    nodes <- state$nodes
  } else {
    nodes$x <- as.numeric(nodes$x)
    nodes$y <- as.numeric(nodes$y)
    nodes$r_squared <- as.numeric(nodes$r_squared)
    nodes$r_squared_kind <- as.character(nodes$r_squared_kind)
    nodes$node_kind <- as.character(nodes$node_kind)
    nodes$factor_a <- as.character(nodes$factor_a)
    nodes$factor_b <- as.character(nodes$factor_b)
    nodes$cat_mode <- as.character(nodes$cat_mode)
    miss_cm <- is.na(nodes$cat_mode) | !nzchar(nodes$cat_mode)
    if (any(miss_cm)) nodes$cat_mode[miss_cm] <- "split"
    nodes$repeated_measures <- as.logical(nodes$repeated_measures)
    miss_rm <- is.na(nodes$repeated_measures)
    if (any(miss_rm)) nodes$repeated_measures[miss_rm] <- FALSE
    nodes$cluster_column <- as.character(nodes$cluster_column)
    nodes$dv_transform <- as.character(nodes$dv_transform)
    miss_tr <- is.na(nodes$dv_transform) | !nzchar(nodes$dv_transform)
    if (any(miss_tr)) nodes$dv_transform[miss_tr] <- "none"
    allowed_tr <- unname(dv_transform_choices())
    bad_tr <- !nodes$dv_transform %in% allowed_tr
    if (any(bad_tr)) nodes$dv_transform[bad_tr] <- "none"
    nodes$factor_base <- as.character(nodes$factor_base)
    nodes$factor_ordered <- as.logical(nodes$factor_ordered)
    miss_fo <- is.na(nodes$factor_ordered)
    if (any(miss_fo)) nodes$factor_ordered[miss_fo] <- FALSE
    nodes$factor_levels <- as.character(nodes$factor_levels)
    nodes$sim_mean <- as.numeric(nodes$sim_mean)
    nodes$sim_sd <- as.numeric(nodes$sim_sd)
    nodes$sim_min <- as.numeric(nodes$sim_min)
    nodes$sim_max <- as.numeric(nodes$sim_max)
    nodes$sim_intercept <- as.numeric(nodes$sim_intercept)
    nodes$sim_resid_sd <- as.numeric(nodes$sim_resid_sd)
    nodes$sim_var_kind <- as.character(nodes$sim_var_kind)
    miss_sk <- is.na(nodes$sim_var_kind) | !nzchar(nodes$sim_var_kind)
    if (any(miss_sk)) nodes$sim_var_kind[miss_sk] <- "continuous"
    bad_sk <- !nodes$sim_var_kind %in% c("continuous", "categorical", "binary")
    if (any(bad_sk)) nodes$sim_var_kind[bad_sk] <- "continuous"
    nodes$sim_level_props <- as.character(nodes$sim_level_props)
    nodes$sim_level_counts <- as.character(nodes$sim_level_counts)
    nodes$sim_design_mode <- as.character(nodes$sim_design_mode)
    miss_dm <- is.na(nodes$sim_design_mode) | !nzchar(nodes$sim_design_mode)
    if (any(miss_dm)) nodes$sim_design_mode[miss_dm] <- "inter"
    bad_dm <- !nodes$sim_design_mode %in% c("inter", "intra")
    if (any(bad_dm)) nodes$sim_design_mode[bad_dm] <- "inter"
    nodes$sim_alloc_mode <- as.character(nodes$sim_alloc_mode)
    miss_am <- is.na(nodes$sim_alloc_mode) | !nzchar(nodes$sim_alloc_mode)
    if (any(miss_am)) nodes$sim_alloc_mode[miss_am] <- "probability"
    bad_am <- !nodes$sim_alloc_mode %in% c("probability", "exact")
    if (any(bad_am)) nodes$sim_alloc_mode[bad_am] <- "probability"
    # Infer node_kind when missing from older projects
    miss_kind <- is.na(nodes$node_kind) | !nzchar(nodes$node_kind)
    if (any(miss_kind)) {
      ix <- !is.na(nodes$factor_a) & nzchar(nodes$factor_a) &
        !is.na(nodes$factor_b) & nzchar(nodes$factor_b)
      obs <- !is.na(nodes$observed_column) & nzchar(nodes$observed_column)
      nodes$node_kind[miss_kind] <- ifelse(
        ix[miss_kind], "interaction",
        ifelse(obs[miss_kind], "observed", "latent")
      )
    }
    # Migrate legacy edge cat_mode → node before dropping edge column
    if ("cat_mode" %in% names(edges) && nrow(edges)) {
      om_from <- unique(as.character(edges$from[
        !is.na(edges$cat_mode) & edges$cat_mode == "omnibus"
      ]))
      if (length(om_from)) {
        nodes$cat_mode[nodes$id %in% om_from] <- "omnibus"
      }
    }
    nodes <- nodes[, need_n, drop = FALSE]
  }

  need_e <- c(
    "id", "from", "to", "type", "level", "to_port", "to_level", "rel_kind", "status",
    "estimate", "se", "pvalue", "estimate_std", "label",
    "prior_family", "prior_mean", "prior_sd"
  )
  if (!nrow(edges) || is.null(payload$edges)) {
    edges <- state$edges
  } else {
    for (nm in need_e) {
      if (!nm %in% names(edges)) {
        edges[[nm]] <- if (nm %in% c("estimate", "se", "pvalue", "estimate_std")) {
          NA_real_
        } else if (nm == "prior_mean") {
          0
        } else if (nm == "prior_sd") {
          1
        } else if (nm == "prior_family") {
          "normal"
        } else if (nm == "status") {
          "pending"
        } else if (nm == "rel_kind") {
          "linear"
        } else {
          NA_character_
        }
      }
    }
    edges <- edges[, need_e, drop = FALSE]
    edges$estimate <- as.numeric(edges$estimate)
    edges$se <- as.numeric(edges$se)
    edges$pvalue <- as.numeric(edges$pvalue)
    edges$estimate_std <- as.numeric(edges$estimate_std)
    edges$prior_mean <- as.numeric(edges$prior_mean)
    edges$prior_sd <- as.numeric(edges$prior_sd)
    edges$prior_family <- as.character(edges$prior_family)
    edges$level <- as.character(edges$level)
    edges$to_port <- as.character(edges$to_port)
    edges$to_level <- as.character(edges$to_level)
    edges$rel_kind <- as.character(edges$rel_kind)
    miss_rk <- is.na(edges$rel_kind) | !nzchar(edges$rel_kind)
    if (any(miss_rk)) edges$rel_kind[miss_rk] <- "linear"
  }

  state$nodes <- nodes
  state$edges <- edges
  state$next_node <- as.integer(payload$next_node %||% 1L)
  state$next_edge <- as.integer(payload$next_edge %||% 1L)
  state <- ensure_node_kind_columns(state)
  state <- ensure_node_sim_columns(state)
  state <- ensure_edge_priors(state)
  attr(state, "prepare") <- prepare_from_list(payload$prepare)
  state
}

#' Extract prepare recipe from a project JSON string
#' @keywords internal
prepare_from_project_json <- function(json) {
  payload <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)
  prepare_from_list(payload$prepare)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
