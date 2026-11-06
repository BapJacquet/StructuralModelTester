#' Serialize model state for the HTML graph editor
#' @keywords internal
graph_payload <- function(state, data = NULL) {
  state <- ensure_node_kind_columns(state)
  state <- ensure_edge_to_port(state)
  state <- refresh_interaction_nodes(state)
  nodes <- state$nodes
  edges <- state$edges
  if (nrow(edges) && !"level" %in% names(edges)) {
    edges$level <- NA_character_
  }
  engines <- dv_engines_map(state, data, edges)

  node_list <- list()
  if (nrow(nodes)) {
    for (i in seq_len(nrow(nodes))) {
      nid <- nodes$id[i]
      col <- nodes$observed_column[i]
      is_ix <- identical(nodes$node_kind[i], "interaction")
      latent <- !is_ix && (is.na(col) || !nzchar(col))
      label <- if (is_ix) {
        nodes$label[i]
      } else if (!latent) {
        col
      } else {
        nodes$label[i]
      }
      eng <- engines[[nid]]
      base <- ""
      levels <- character()
      var_kind <- ""
      if (!latent && !is_ix && !is.null(data) && col %in% names(data)) {
        coding <- variable_coding(data[[col]])
        var_kind <- coding$kind
        if (identical(coding$kind, "categorical")) {
          base <- coding$base
          levels <- coding$levels
        }
      }
      r2 <- if ("r_squared" %in% names(nodes)) nodes$r_squared[i] else NA_real_
      r2_kind <- if ("r_squared_kind" %in% names(nodes)) nodes$r_squared_kind[i] else NA_character_
      r2_label <- ""
      if (is.finite(r2) && !is_ix) {
        r2_label <- if (identical(r2_kind, "mcfadden")) {
          sprintf("R\u00B2m = %.2f", r2)
        } else {
          sprintf("R\u00B2 = %.2f", r2)
        }
      }
      cm <- if ("cat_mode" %in% names(nodes)) nodes$cat_mode[i] else "split"
      if (is.na(cm) || !nzchar(cm)) cm <- "split"
      node_list[[i]] <- list(
        id = nid,
        label = label,
        x = if (is.na(nodes$x[i])) 80 + ((i - 1) %% 4) * 160 else nodes$x[i],
        y = if (is.na(nodes$y[i])) 80 + ((i - 1) %/% 4) * 110 else nodes$y[i],
        latent = latent,
        interaction = is_ix,
        model = if (is.null(eng) || is_ix) "" else dv_model_badge(eng),
        r2 = r2_label,
        varKind = var_kind,
        base = base,
        levels = as.list(levels),
        catMode = if (identical(var_kind, "categorical")) cm else "split"
      )
    }
  }

  edge_list <- list()
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      lv <- edges$level[i]
      tp <- if ("to_port" %in% names(edges)) edges$to_port[i] else NA_character_
      edge_list[[i]] <- list(
        id = edges$id[i],
        from = edges$from[i],
        to = edges$to[i],
        level = if (is.na(lv) || !nzchar(lv)) "" else lv,
        toPort = if (is.na(tp) || !nzchar(tp)) "" else tp,
        label = edges$label[i],
        color = edge_color(edges[i, ]),
        width = edge_width(edges[i, ]),
        dashes = isTRUE(edges$status[i] == "excluded") ||
          (edges$type[i] == "exploratory" && edges$status[i] == "pending"),
        status = edges$status[i],
        type = edges$type[i]
      )
    }
  }

  list(nodes = node_list, edges = edge_list)
}

edge_color <- function(edge) {
  if (!is.null(edge$status) && identical(edge$status, "excluded")) return("#B0B0B0")
  if (identical(edge$type, "definitional")) return("#9B8FD4")
  if (!is.na(edge$estimate)) {
    if (edge$estimate < 0) return("#C0392B")
    if (edge$estimate > 0) return("#1F7A4D")
  }
  switch(edge$type,
    fixed = "#3D3D3D",
    confirmatory = "#5B2C8B",
    exploratory = "#8E7CC3",
    measurement = "#2E86AB",
    definitional = "#9B8FD4",
    "#666666"
  )
}

edge_width <- function(edge) {
  if (!is.null(edge$status) && identical(edge$status, "excluded")) return(1)
  if (identical(edge$type, "definitional")) return(1.5)
  # Prefer standardized |β|; fall back to raw |β| if std missing
  val <- if (!is.null(edge$estimate_std) && !is.na(edge$estimate_std)) {
    edge$estimate_std
  } else {
    edge$estimate
  }
  if (is.na(val)) {
    return(switch(edge$type, fixed = 3, confirmatory = 2.5, exploratory = 2, measurement = 2, definitional = 1.5, 2))
  }
  # Standardized effects are typically in ~[-1, 1]; cap at 1 for mapping
  cap <- if (!is.null(edge$estimate_std) && !is.na(edge$estimate_std)) 1 else 1.5
  w <- 1.5 + 4 * min(abs(as.numeric(val)), cap) / cap
  as.numeric(w)
}

#' Apply one node position update from the HTML editor
#' @keywords internal
apply_node_position <- function(state, id, x, y) {
  idx <- match(id, state$nodes$id)
  if (is.na(idx)) return(state)
  state$nodes$x[idx] <- as.numeric(x)
  state$nodes$y[idx] <- as.numeric(y)
  state
}

#' @keywords internal
apply_node_positions <- function(state, positions) {
  if (is.null(positions) || !length(positions)) return(state)
  for (id in names(positions)) {
    pos <- positions[[id]]
    state <- apply_node_position(state, id, pos$x, pos$y)
  }
  state
}
