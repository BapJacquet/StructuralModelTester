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
      # Respect node_kind: observed nodes stay observed even when unmapped.
      latent <- is_latent_node(state, nid)
      mapped <- !is.na(col) && nzchar(as.character(col))
      # Canvas always shows the node label (editable in the inspector).
      # Mapped column is data-only and may differ after a rename.
      label <- {
        lab <- nodes$label[i]
        if (!is.na(lab) && nzchar(as.character(lab))) {
          as.character(lab)
        } else if (mapped) {
          as.character(col)
        } else {
          as.character(nid)
        }
      }
      eng <- engines[[nid]]
      base <- ""
      levels <- character()
      var_kind <- ""
      if (!latent && !is_ix && mapped && !is.null(data) && col %in% names(data)) {
        coding <- variable_coding_for_node(state, nid, data)
        var_kind <- coding$kind
        if (identical(coding$kind, "categorical")) {
          base <- coding$base
          # Non-base levels drive split / omnibus ports
          levels <- coding$levels
        } else if (identical(coding$kind, "binary")) {
          base <- coding$base
          levels <- character()
        }
      } else if (!latent && !is_ix && !mapped) {
        # Unmapped observed: declared sim kind / factor coding owns the canvas ports.
        # Binary uses treatment coding (single body port) — no split/omnibus UI.
        var_kind <- node_sim_var_kind(state, nid, data = NULL)
        if (identical(var_kind, "categorical")) {
          all_levs <- node_sim_factor_levels(state, nid, data = NULL)
          meta <- node_factor_coding(state, nid)
          base <- if (!is.na(meta$base) && nzchar(meta$base)) {
            as.character(meta$base)
          } else if (length(all_levs)) {
            as.character(all_levs[[1]])
          } else {
            ""
          }
          levels <- if (nzchar(base)) {
            setdiff(as.character(all_levs), base)
          } else {
            as.character(all_levs)
          }
        } else if (identical(var_kind, "binary")) {
          all_levs <- node_sim_factor_levels(state, nid, data = NULL)
          meta <- node_factor_coding(state, nid)
          base <- if (!is.na(meta$base) && nzchar(meta$base)) {
            as.character(meta$base)
          } else if (length(all_levs)) {
            as.character(all_levs[[1]])
          } else {
            ""
          }
          levels <- character()
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
      uni_status <- ""
      uni_label <- ""
      uni_title <- ""
      if (latent && !is_ix) {
        ust <- if ("uni_status" %in% names(nodes)) {
          as.character(nodes$uni_status[i])
        } else {
          NA_character_
        }
        ulab <- if ("uni_label" %in% names(nodes)) {
          as.character(nodes$uni_label[i])
        } else {
          NA_character_
        }
        uratio <- if ("uni_ratio" %in% names(nodes)) {
          suppressWarnings(as.numeric(nodes$uni_ratio[i]))
        } else {
          NA_real_
        }
        upct1 <- if ("uni_pct1" %in% names(nodes)) {
          suppressWarnings(as.numeric(nodes$uni_pct1[i]))
        } else {
          NA_real_
        }
        if (!is.na(ust) && nzchar(ust)) {
          uni_status <- ust
          uni_label <- if (!is.na(ulab) && nzchar(ulab)) ulab else ust
          bits <- character()
          if (is.finite(uratio)) {
            bits <- c(bits, sprintf("\u03bb\u2081/\u03bb\u2082 = %s", formatC(uratio, digits = 2, format = "f")))
          }
          if (is.finite(upct1)) {
            bits <- c(bits, sprintf("PC1 = %s%%", formatC(100 * upct1, digits = 0, format = "f")))
          }
          uni_title <- if (length(bits)) {
            paste0("Unidimensionality (PCA): ", paste(bits, collapse = "; "))
          } else {
            "Unidimensionality (PCA)"
          }
        }
      }
      cm <- if ("cat_mode" %in% names(nodes)) nodes$cat_mode[i] else "split"
      if (is.na(cm) || !nzchar(cm)) cm <- "split"
      design_mode <- ""
      if (!latent && !is_ix && var_kind %in% c("categorical", "binary")) {
        design_mode <- node_sim_design_mode(state, nid)
        if (!design_mode %in% c("inter", "intra")) design_mode <- "inter"
      }
      node_list[[i]] <- list(
        id = nid,
        label = label,
        x = if (is.na(nodes$x[i])) 80 + ((i - 1) %% 4) * 160 else nodes$x[i],
        y = if (is.na(nodes$y[i])) 80 + ((i - 1) %/% 4) * 110 else nodes$y[i],
        latent = latent,
        interaction = is_ix,
        # Observed (non-latent) with no data column — canvas shows a broken-link mark
        unmapped = !latent && !is_ix && !mapped,
        model = if (is.null(eng) || is_ix) "" else dv_model_badge(eng),
        r2 = r2_label,
        uniStatus = uni_status,
        uniLabel = uni_label,
        uniTitle = uni_title,
        varKind = var_kind,
        base = base,
        levels = as.list(levels),
        catMode = if (identical(var_kind, "categorical")) cm else "split",
        designMode = design_mode
      )
    }
  }

  edge_list <- list()
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      lv <- edges$level[i]
      tp <- if ("to_port" %in% names(edges)) edges$to_port[i] else NA_character_
      tl <- if ("to_level" %in% names(edges)) edges$to_level[i] else NA_character_
      edge_list[[i]] <- list(
        id = edges$id[i],
        from = edges$from[i],
        to = edges$to[i],
        level = if (is.na(lv) || !nzchar(lv)) "" else lv,
        toPort = if (is.na(tp) || !nzchar(tp)) "" else tp,
        toLevel = if (is.na(tl) || !nzchar(tl)) "" else tl,
        label = edges$label[i],
        color = edge_color(edges[i, ], alpha = state$pvalue_alpha),
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

#' Compact signature of graph structure (ignore positions / fit cosmetics)
#'
#' Used so layout-only `skip_graph_render` updates do not suppress redraws when
#' edges, ports, or variable kinds actually changed in the same flush.
#' @keywords internal
graph_structure_signature <- function(payload) {
  nodes <- if (is.null(payload)) list() else (payload$nodes %||% list())
  edges <- if (is.null(payload)) list() else (payload$edges %||% list())
  nsig <- vapply(nodes, function(n) {
    levs <- paste(unlist(n$levels %||% list()), collapse = ",")
    paste(
      n$id %||% "",
      isTRUE(n$latent),
      isTRUE(n$interaction),
      isTRUE(n$unmapped),
      n$varKind %||% "",
      n$base %||% "",
      n$catMode %||% "",
      n$designMode %||% "",
      levs,
      sep = "\t"
    )
  }, character(1))
  esig <- vapply(edges, function(e) {
    paste(
      e$id %||% "",
      e$from %||% "",
      e$to %||% "",
      e$type %||% "",
      e$level %||% "",
      e$toPort %||% "",
      e$toLevel %||% "",
      sep = "\t"
    )
  }, character(1))
  paste(c(sort(nsig), "||", sort(esig)), collapse = "\n")
}

#' Signature of model content that Results / Inspector care about (ignore x/y)
#'
#' Layout-only node moves still update `state()`, but Results UI should not
#' remount (MathJax / DT) when only canvas positions change.
#' @keywords internal
results_ui_signature <- function(state) {
  if (is.null(state)) return("")
  nodes <- state$nodes
  if (is.data.frame(nodes) && nrow(nodes)) {
    drop <- intersect(c("x", "y"), names(nodes))
    if (length(drop)) {
      nodes <- nodes[, setdiff(names(nodes), drop), drop = FALSE]
    }
  }
  as.character(jsonlite::toJSON(
    list(
      nodes = nodes,
      edges = state$edges,
      pvalue_alpha = state$pvalue_alpha,
      next_node = state$next_node,
      next_edge = state$next_edge
    ),
    dataframe = "columns",
    auto_unbox = TRUE,
    null = "null"
  ))
}

edge_color <- function(edge, alpha = NULL) {
  if (!is.null(edge$status) && identical(edge$status, "excluded")) return("#B0B0B0")
  if (identical(as.character(edge$type), "confirmatory")) {
    # Bayesian HDI+ROPE: mute when the null is not rejected. Link stays in the model.
    if (!is.null(edge$rope_decision) && length(edge$rope_decision)) {
      dec <- as.character(edge$rope_decision[[1]])
      if (!is.na(dec) && nzchar(dec) && !identical(dec, "reject")) {
        return("#B0B0B0")
      }
    } else {
      inc <- if (!is.null(edge$includes_zero)) edge$includes_zero else NULL
      if (!is.null(inc) && length(inc) && !is.na(inc[[1]]) && isTRUE(as.logical(inc[[1]]))) {
        return("#B0B0B0")
      }
    }
    # P-value analysis: mute when p > α
    if (!is.null(alpha) && is.finite(alpha)) {
      pv <- suppressWarnings(as.numeric(edge$pvalue))
      if (length(pv) && is.finite(pv[[1]]) && pv[[1]] > alpha) return("#B0B0B0")
    }
  }
  if (identical(edge$type, "definitional")) return("#9B8FD4")
  est <- suppressWarnings(as.numeric(edge$estimate))
  if (length(est) && is.finite(est[[1]]) && !is.na(est[[1]])) {
    if (est[[1]] < 0) return("#C0392B")
    if (est[[1]] > 0) return("#1F7A4D")
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
