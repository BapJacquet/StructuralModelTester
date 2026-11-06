#' Serialize model state to JSON
#'
#' @param state Model state list.
#' @return JSON string.
#' @export
model_to_json <- function(state) {
  payload <- list(
    version = 1L,
    nodes = state$nodes,
    edges = state$edges,
    next_node = state$next_node,
    next_edge = state$next_edge
  )
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
    "r_squared", "r_squared_kind", "node_kind", "factor_a", "factor_b", "cat_mode"
  )
  for (nm in need_n) {
    if (!nm %in% names(nodes)) {
      nodes[[nm]] <- if (nm == "shape") {
        "ellipse"
      } else if (nm %in% c("x", "y", "r_squared")) {
        NA_real_
      } else if (nm == "cat_mode") {
        "split"
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
    "id", "from", "to", "type", "level", "to_port", "rel_kind", "status",
    "estimate", "se", "pvalue", "estimate_std", "label"
  )
  if (!nrow(edges) || is.null(payload$edges)) {
    edges <- state$edges
  } else {
    for (nm in need_e) {
      if (!nm %in% names(edges)) {
        edges[[nm]] <- if (nm %in% c("estimate", "se", "pvalue", "estimate_std")) {
          NA_real_
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
    edges$level <- as.character(edges$level)
    edges$to_port <- as.character(edges$to_port)
    edges$rel_kind <- as.character(edges$rel_kind)
    miss_rk <- is.na(edges$rel_kind) | !nzchar(edges$rel_kind)
    if (any(miss_rk)) edges$rel_kind[miss_rk] <- "linear"
  }

  state$nodes <- nodes
  state$edges <- edges
  state$next_node <- as.integer(payload$next_node %||% 1L)
  state$next_edge <- as.integer(payload$next_edge %||% 1L)
  ensure_node_kind_columns(state)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
