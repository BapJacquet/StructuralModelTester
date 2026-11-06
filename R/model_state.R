#' Create an empty structural model state
#'
#' @return A list with nodes, edges, and meta fields.
#' @export
empty_model_state <- function() {
  list(
    nodes = data.frame(
      id = character(),
      label = character(),
      observed_column = character(),
      shape = character(),
      x = numeric(),
      y = numeric(),
      r_squared = numeric(),
      r_squared_kind = character(),
      node_kind = character(),
      factor_a = character(),
      factor_b = character(),
      cat_mode = character(),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = character(),
      from = character(),
      to = character(),
      type = character(),
      level = character(),
      to_port = character(),
      rel_kind = character(),
      status = character(),
      estimate = numeric(),
      se = numeric(),
      pvalue = numeric(),
      estimate_std = numeric(),
      label = character(),
      stringsAsFactors = FALSE
    ),
    next_node = 1L,
    next_edge = 1L
  )
}

sanitize_id <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  if (!nzchar(x)) x <- "var"
  if (grepl("^[0-9]", x)) x <- paste0("v_", x)
  x
}

unique_node_id <- function(state, base) {
  id <- sanitize_id(base)
  existing <- state$nodes$id
  if (!id %in% existing) return(id)
  i <- 1L
  while (paste0(id, "_", i) %in% existing) i <- i + 1L
  paste0(id, "_", i)
}

#' Ensure interaction-related node columns exist
#' @keywords internal
ensure_node_kind_columns <- function(state) {
  if (!"factor_a" %in% names(state$nodes)) state$nodes$factor_a <- NA_character_
  if (!"factor_b" %in% names(state$nodes)) state$nodes$factor_b <- NA_character_
  if (!"node_kind" %in% names(state$nodes)) {
    ix <- !is.na(state$nodes$factor_a) & nzchar(state$nodes$factor_a) &
      !is.na(state$nodes$factor_b) & nzchar(state$nodes$factor_b)
    obs <- !is.na(state$nodes$observed_column) & nzchar(state$nodes$observed_column)
    state$nodes$node_kind <- ifelse(ix, "interaction", ifelse(obs, "observed", "latent"))
  }
  if (!"cat_mode" %in% names(state$nodes)) state$nodes$cat_mode <- "split"
  miss_cm <- is.na(state$nodes$cat_mode) | !nzchar(state$nodes$cat_mode)
  if (any(miss_cm)) state$nodes$cat_mode[miss_cm] <- "split"
  bad_cm <- !state$nodes$cat_mode %in% c("split", "omnibus")
  if (any(bad_cm)) state$nodes$cat_mode[bad_cm] <- "split"
  # Migrate legacy edge-level cat_mode → node
  if ("cat_mode" %in% names(state$edges) && nrow(state$edges) && nrow(state$nodes)) {
    om_from <- unique(state$edges$from[
      !is.na(state$edges$cat_mode) & state$edges$cat_mode == "omnibus"
    ])
    if (length(om_from)) {
      hit <- state$nodes$id %in% om_from
      state$nodes$cat_mode[hit] <- "omnibus"
    }
  }
  state
}

#' Categorical coding mode for a predictor node (split | omnibus)
#' @keywords internal
node_cat_mode <- function(state, node_id) {
  state <- ensure_node_kind_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return("split")
  cm <- state$nodes$cat_mode[idx]
  if (is.na(cm) || !nzchar(cm) || !cm %in% c("split", "omnibus")) return("split")
  cm
}

#' True if node is an interaction (product) operator
#' @keywords internal
is_interaction_node <- function(state, node_id) {
  state <- ensure_node_kind_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(FALSE)
  identical(state$nodes$node_kind[idx], "interaction")
}

#' True if node is a latent (unmapped, not an interaction)
#' @keywords internal
is_latent_node <- function(state, node_id) {
  !is_observed_node(state, node_id) && !is_interaction_node(state, node_id)
}

#' Format one interaction factor for display (optionally with split level)
#' @keywords internal
interaction_factor_label <- function(state, node_id, level = NA_character_) {
  if (is.na(node_id) || !nzchar(node_id)) return("?")
  nm <- node_display_name(state, node_id)
  if (!is.na(level) && nzchar(level)) sprintf("%s[%s]", nm, level) else nm
}

#' Display label for an interaction node (A×B, with optional split levels)
#' @keywords internal
interaction_label <- function(state, factor_a = NA_character_, factor_b = NA_character_,
                              level_a = NA_character_, level_b = NA_character_) {
  a_ok <- !is.na(factor_a) && nzchar(factor_a)
  b_ok <- !is.na(factor_b) && nzchar(factor_b)
  if (a_ok && b_ok) {
    return(paste0(
      interaction_factor_label(state, factor_a, level_a),
      "\u00D7",
      interaction_factor_label(state, factor_b, level_b)
    ))
  }
  if (a_ok) return(paste0(interaction_factor_label(state, factor_a, level_a), "\u00D7?"))
  if (b_ok) return(paste0("?\u00D7", interaction_factor_label(state, factor_b, level_b)))
  "\u00D7"
}

#' Ensure edge to_port / rel_kind columns exist
#' @keywords internal
ensure_edge_to_port <- function(state) {
  state <- ensure_node_kind_columns(state)
  if (!"to_port" %in% names(state$edges)) state$edges$to_port <- NA_character_
  if (!"rel_kind" %in% names(state$edges)) state$edges$rel_kind <- "linear"
  miss <- is.na(state$edges$rel_kind) | !nzchar(state$edges$rel_kind)
  if (any(miss)) state$edges$rel_kind[miss] <- "linear"
  # Drop legacy "none" (points-only) → linear
  none_idx <- which(state$edges$rel_kind == "none")
  if (length(none_idx)) state$edges$rel_kind[none_idx] <- "linear"
  state
}

#' Set inspect-panel relationship kind for one edge
#' @keywords internal
set_edge_rel_kind <- function(state, edge_id, rel_kind) {
  state <- ensure_edge_to_port(state)
  allowed <- unname(edge_rel_kind_choices())
  if (!rel_kind %in% allowed) rel_kind <- "linear"
  idx <- match(edge_id, state$edges$id)
  if (is.na(idx)) return(state)
  state$edges$rel_kind[idx] <- rel_kind
  state
}

#' Set categorical coding mode on a predictor node (split | omnibus)
#'
#' Updates outgoing structural edges: omnibus collapses to one unleveled link
#' per target; split expands unleveled links into one edge per non-base level.
#' @keywords internal
set_node_cat_mode <- function(state, node_id, cat_mode = c("split", "omnibus"), data = NULL) {
  cat_mode <- match.arg(cat_mode)
  state <- ensure_node_kind_columns(state)
  state <- ensure_edge_to_port(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  if (is_interaction_node(state, node_id) || is_latent_node(state, node_id)) {
    return(state)
  }
  cur <- node_cat_mode(state, node_id)
  state$nodes$cat_mode[idx] <- cat_mode

  # Drop definitional wires into × — user must re-wire (level ports vs omnibus port)
  if (!identical(cur, cat_mode) && nrow(state$edges)) {
    def_drop <- which(
      state$edges$from == node_id &
        state$edges$type == "definitional" &
        vapply(state$edges$to, function(id) is_interaction_node(state, id), logical(1))
    )
    if (length(def_drop)) {
      state$edges <- state$edges[-def_drop, , drop = FALSE]
      state <- refresh_interaction_nodes(state)
    }
  }

  is_struct <- function(i) {
    !state$edges$type[i] %in% c("measurement", "definitional")
  }
  outs <- which(state$edges$from == node_id & vapply(seq_len(nrow(state$edges)), is_struct, logical(1)))
  if (!length(outs)) return(state)

  tos <- unique(state$edges$to[outs])
  if (identical(cat_mode, "omnibus")) {
    for (to in tos) {
      hits <- which(
        state$edges$from == node_id & state$edges$to == to &
          !state$edges$type %in% c("measurement", "definitional")
      )
      if (!length(hits)) next
      keep <- hits[1]
      etype <- state$edges$type[keep]
      drop <- setdiff(hits, keep)
      if (length(drop)) state$edges <- state$edges[-drop, , drop = FALSE]
      keep <- match(state$edges$id[keep], state$edges$id)
      # after drop, re-find
      keep <- which(
        state$edges$from == node_id & state$edges$to == to &
          !state$edges$type %in% c("measurement", "definitional")
      )[1]
      if (is.na(keep)) next
      state$edges$level[keep] <- NA_character_
      state$edges$label[keep] <- paste0(type_short_label(etype), "·omnibus")
    }
    return(state)
  }

  # split: expand each unleveled from→to into per-level edges
  if (is.null(data)) return(state)
  col <- tryCatch(lavaan_var_name(state, node_id), error = function(e) NA_character_)
  if (is.na(col) || !col %in% names(data)) return(state)
  coding <- variable_coding(data[[col]])
  if (!identical(coding$kind, "categorical") || !length(coding$levels)) return(state)

  for (to in tos) {
    hits <- which(
      state$edges$from == node_id & state$edges$to == to &
        !state$edges$type %in% c("measurement", "definitional")
    )
    if (!length(hits)) next
    levels_present <- state$edges$level[hits]
    if (any(!is.na(levels_present) & nzchar(levels_present))) {
      # Already split — just refresh labels
      for (h in hits) {
        lv <- state$edges$level[h]
        if (!is.na(lv) && nzchar(lv)) {
          state$edges$label[h] <- paste0(type_short_label(state$edges$type[h]), "·", lv)
        }
      }
      next
    }
    etype <- state$edges$type[hits[1]]
    # Drop unleveled, add one per level
    state$edges <- state$edges[-hits, , drop = FALSE]
    for (lev in coding$levels) {
      state <- add_edge(state, node_id, to, type = etype, level = lev)
    }
  }
  state
}

#' Resolve interaction factors from inbound wiring (ports a / b)
#' @return list(a, b, level_a, level_b) — levels set for split categorical wires
#' @keywords internal
interaction_factors <- function(state, ix_id) {
  state <- ensure_edge_to_port(state)
  empty <- list(
    a = NA_character_, b = NA_character_,
    level_a = NA_character_, level_b = NA_character_
  )
  if (!is_interaction_node(state, ix_id) || !nrow(state$edges)) return(empty)
  inc <- state$edges[state$edges$to == ix_id & state$edges$type != "measurement", , drop = FALSE]
  if (!nrow(inc)) return(empty)
  a_hit <- which(!is.na(inc$to_port) & inc$to_port == "a")
  b_hit <- which(!is.na(inc$to_port) & inc$to_port == "b")
  lvl_of <- function(hits) {
    if (!length(hits)) return(NA_character_)
    lv <- if ("level" %in% names(inc)) inc$level[hits[1]] else NA_character_
    if (is.na(lv) || !nzchar(lv)) NA_character_ else as.character(lv)
  }
  list(
    a = if (length(a_hit)) inc$from[a_hit[1]] else NA_character_,
    b = if (length(b_hit)) inc$from[b_hit[1]] else NA_character_,
    level_a = lvl_of(a_hit),
    level_b = lvl_of(b_hit)
  )
}

#' Refresh interaction node labels / cached factor ids from wiring
#' @keywords internal
refresh_interaction_nodes <- function(state) {
  state <- ensure_node_kind_columns(state)
  state <- ensure_edge_to_port(state)
  ix <- which(state$nodes$node_kind == "interaction")
  for (i in ix) {
    fac <- interaction_factors(state, state$nodes$id[i])
    state$nodes$factor_a[i] <- fac$a
    state$nodes$factor_b[i] <- fac$b
    state$nodes$label[i] <- interaction_label(
      state, fac$a, fac$b, fac$level_a, fac$level_b
    )
  }
  state
}

#' True if edge wires a predictor into an interaction operator
#' @keywords internal
is_definitional_edge <- function(state, edge_id) {
  idx <- match(edge_id, state$edges$id)
  if (is.na(idx)) return(FALSE)
  is_interaction_node(state, state$edges$to[idx])
}

#' True if an unleveled structural edge from→to already exists
#' @keywords internal
has_plain_edge <- function(state, from, to) {
  if (!nrow(state$edges)) return(FALSE)
  hit <- state$edges$from == from & state$edges$to == to &
    !state$edges$type %in% c("measurement", "definitional")
  if (!any(hit)) return(FALSE)
  lv <- state$edges$level[hit]
  any(is.na(lv) | !nzchar(lv))
}

#' True if a leveled structural edge from→to already exists
#' @keywords internal
has_leveled_edge <- function(state, from, to, level) {
  if (!nrow(state$edges) || is.na(level) || !nzchar(level)) return(FALSE)
  hit <- state$edges$from == from & state$edges$to == to &
    !state$edges$type %in% c("measurement", "definitional")
  if (!any(hit)) return(FALSE)
  any(!is.na(state$edges$level[hit]) & state$edges$level[hit] == level)
}

#' Auto-add main-effect edges for an interaction → DV link (hierarchy)
#'
#' When `ix → dv` exists, ensure each wired factor also has `factor → dv`,
#' using the same link type as the interaction → DV edge.
#' Split categorical factors get the wired level only; omnibus get the
#' whole-factor (unleveled) main effect.
#' @param dv_id If NULL, apply to every DV currently targeted by the interaction.
#' @keywords internal
ensure_interaction_main_effects <- function(state, ix_id, dv_id = NULL) {
  if (!is_interaction_node(state, ix_id)) return(state)
  fac <- interaction_factors(state, ix_id)
  slots <- list(
    list(id = fac$a, level = fac$level_a),
    list(id = fac$b, level = fac$level_b)
  )
  slots <- Filter(function(s) !is.na(s$id) && nzchar(s$id), slots)
  if (!length(slots)) return(state)

  outs <- state$edges$from == ix_id & state$edges$type != "measurement"
  out_edges <- state$edges[outs, , drop = FALSE]
  if (!nrow(out_edges)) return(state)

  if (is.null(dv_id)) {
    dv_id <- unique(out_edges$to)
  }
  dv_id <- unique(as.character(dv_id))
  dv_id <- dv_id[!is.na(dv_id) & nzchar(dv_id)]
  dv_id <- dv_id[!vapply(dv_id, function(id) is_interaction_node(state, id), logical(1))]
  if (!length(dv_id)) return(state)

  for (dv in dv_id) {
    ix_hit <- which(out_edges$to == dv)
    link_type <- if (length(ix_hit)) {
      as.character(out_edges$type[ix_hit[1]])
    } else {
      "confirmatory"
    }
    if (!link_type %in% c("fixed", "confirmatory", "exploratory", "measurement")) {
      link_type <- "confirmatory"
    }
    if (identical(link_type, "measurement")) link_type <- "confirmatory"

    for (s in slots) {
      f <- s$id
      if (identical(f, dv)) next
      cm <- node_cat_mode(state, f)
      lvl <- s$level
      if (identical(cm, "omnibus") || is.na(lvl) || !nzchar(lvl)) {
        if (has_plain_edge(state, f, dv)) next
        state <- add_edge(state, from = f, to = dv, type = link_type, level = NA_character_)
      } else {
        if (has_leveled_edge(state, f, dv, lvl) || has_plain_edge(state, f, dv)) next
        state <- add_edge(state, from = f, to = dv, type = link_type, level = lvl)
      }
    }
  }
  state
}

#' Add a node to the model
#' @keywords internal
add_node <- function(state, label, observed_column = NA_character_) {
  state <- ensure_node_r2_columns(state)
  state <- ensure_node_kind_columns(state)
  id <- unique_node_id(state, label)
  is_obs <- !is.na(observed_column) && nzchar(observed_column)
  shape <- if (is_obs) "box" else "ellipse"
  n <- nrow(state$nodes)
  # Stagger new nodes so they are not stacked
  new_row <- data.frame(
    id = id,
    label = label,
    observed_column = if (is_obs) observed_column else NA_character_,
    shape = shape,
    x = 80 + (n %% 4) * 160,
    y = 80 + (n %/% 4) * 100,
    r_squared = NA_real_,
    r_squared_kind = NA_character_,
    node_kind = if (is_obs) "observed" else "latent",
    factor_a = NA_character_,
    factor_b = NA_character_,
    cat_mode = "split",
    stringsAsFactors = FALSE
  )
  state$nodes <- rbind(state$nodes, new_row)
  state$next_node <- state$next_node + 1L
  state
}

#' Add an empty interaction (product) operator node
#'
#' Wire two observed predictors into ports a/b, then link the output to a DV.
#' @keywords internal
add_interaction_node <- function(state) {
  state <- ensure_node_kind_columns(state)
  id <- unique_node_id(state, "ix")
  n <- nrow(state$nodes)
  new_row <- data.frame(
    id = id,
    label = "\u00D7",
    observed_column = NA_character_,
    shape = "operator",
    x = 80 + (n %% 4) * 160,
    y = 80 + (n %/% 4) * 100,
    r_squared = NA_real_,
    r_squared_kind = NA_character_,
    node_kind = "interaction",
    factor_a = NA_character_,
    factor_b = NA_character_,
    cat_mode = "split",
    stringsAsFactors = FALSE
  )
  state$nodes <- rbind(state$nodes, new_row)
  state$next_node <- state$next_node + 1L
  state
}

#' Remove a node and its incident edges
#' @keywords internal
remove_node <- function(state, node_id) {
  state$nodes <- state$nodes[state$nodes$id != node_id, , drop = FALSE]
  state$edges <- state$edges[
    state$edges$from != node_id & state$edges$to != node_id,
    ,
    drop = FALSE
  ]
  state
}

#' Map a data column onto a node (observed) or clear mapping (latent)
#' @keywords internal
map_column_to_node <- function(state, node_id, column = NA_character_) {
  state <- ensure_node_kind_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  if (is_interaction_node(state, node_id)) return(state)
  if (is.null(column) || is.na(column) || !nzchar(column)) {
    state$nodes$observed_column[idx] <- NA_character_
    state$nodes$shape[idx] <- "ellipse"
    state$nodes$node_kind[idx] <- "latent"
  } else {
    # One column -> one node
    taken <- which(state$nodes$observed_column == column & state$nodes$id != node_id)
    if (length(taken)) {
      state$nodes$observed_column[taken] <- NA_character_
      state$nodes$shape[taken] <- "ellipse"
      state$nodes$node_kind[taken] <- "latent"
    }
    state$nodes$observed_column[idx] <- column
    state$nodes$label[idx] <- column
    state$nodes$shape[idx] <- "box"
    state$nodes$node_kind[idx] <- "observed"
  }
  state
}

#' Add a directed link
#'
#' @param level For categorical predictors: non-reference level this edge
#'   represents. `NA` for continuous/binary/measurement links.
#' @param to_port For links into an interaction node: `"a"` or `"b"`.
#' @keywords internal
add_edge <- function(state, from, to,
                     type = c("fixed", "confirmatory", "exploratory", "measurement"),
                     level = NA_character_,
                     to_port = NA_character_) {
  type <- match.arg(type)
  state <- ensure_edge_to_port(state)
  if (!from %in% state$nodes$id || !to %in% state$nodes$id) return(state)
  if (from == to) return(state)
  if (identical(type, "measurement") &&
        (is_interaction_node(state, from) || is_interaction_node(state, to))) {
    return(state)
  }
  if (is.null(level) || !nzchar(as.character(level))) level <- NA_character_
  level <- as.character(level)
  if (is.null(to_port) || !nzchar(as.character(to_port))) to_port <- NA_character_
  to_port <- as.character(to_port)

  # Wire into interaction operator (definitional; not a regression path)
  if (is_interaction_node(state, to)) {
    if (!to_port %in% c("a", "b")) return(state)
    if (!is_observed_node(state, from)) return(state)
    if (is_interaction_node(state, from)) return(state)
    cm <- node_cat_mode(state, from)
    # Omnibus factor: whole-variable wire only (no level)
    if (identical(cm, "omnibus") && !is.na(level) && nzchar(level)) {
      return(state)
    }
    # Split categorical: one category only — level must be set (enforced at fit
    # if the column is categorical; continuous/binary omit level).
    # One wire per input port
    if (any(state$edges$to == to & !is.na(state$edges$to_port) & state$edges$to_port == to_port)) {
      return(state)
    }
    # Same predictor cannot feed both ports
    other <- interaction_factors(state, to)
    if ((!is.na(other$a) && identical(from, other$a)) ||
          (!is.na(other$b) && identical(from, other$b))) {
      return(state)
    }
    # Wiring only — never a regression path / search parameter
    type <- "definitional"
    eid <- paste0("e", state$next_edge)
    lab <- paste0("\u2192\u00D7", to_port)
    if (!is.na(level) && nzchar(level)) lab <- paste0(lab, "\u00B7", level)
    new_row <- data.frame(
      id = eid,
      from = from,
      to = to,
      type = type,
      level = level,
      to_port = to_port,
      rel_kind = "linear",
      status = "pending",
      estimate = NA_real_,
      se = NA_real_,
      pvalue = NA_real_,
      estimate_std = NA_real_,
      label = lab,
      stringsAsFactors = FALSE
    )
    state$edges <- rbind(state$edges, new_row)
    state$next_edge <- state$next_edge + 1L
    state <- refresh_interaction_nodes(state)
    # If × already targets DVs, attach main effects for this newly wired factor
    return(ensure_interaction_main_effects(state, to))
  }

  # Normal structural / measurement edge
  to_port <- NA_character_
  from_omni <- identical(node_cat_mode(state, from), "omnibus")
  if (from_omni) {
    # Omnibus predictor: only one unleveled structural edge per target
    if (!is.na(level) && nzchar(level)) level <- NA_character_
  }
  same <- state$edges$from == from & state$edges$to == to &
    !state$edges$type %in% c("measurement", "definitional")
  if (any(same)) {
    if (from_omni) return(state)
    existing_levels <- state$edges$level[same]
    if (is.na(level)) {
      if (all(is.na(existing_levels) | !nzchar(existing_levels))) return(state)
      return(state)
    }
    if (any(!is.na(existing_levels) & existing_levels == level)) return(state)
    if (any(is.na(existing_levels) | !nzchar(existing_levels))) return(state)
  }

  eid <- paste0("e", state$next_edge)
  lab <- type_short_label(type)
  if (from_omni) {
    lab <- paste0(lab, "·omnibus")
  } else if (!is.na(level) && nzchar(level)) {
    lab <- paste0(lab, "·", level)
  }
  new_row <- data.frame(
    id = eid,
    from = from,
    to = to,
    type = type,
    level = level,
    to_port = to_port,
    rel_kind = "linear",
    status = "pending",
    estimate = NA_real_,
    se = NA_real_,
    pvalue = NA_real_,
    estimate_std = NA_real_,
    label = lab,
    stringsAsFactors = FALSE
  )
  state$edges <- rbind(state$edges, new_row)
  state$next_edge <- state$next_edge + 1L

  # Hierarchy: × → DV also creates fixed main-effect edges from wired factors
  if (is_interaction_node(state, from) && !identical(type, "measurement")) {
    state <- ensure_interaction_main_effects(state, from, dv_id = to)
  }
  state
}

type_short_label <- function(type) {
  switch(type,
    fixed = "Fix",
    confirmatory = "C",
    exploratory = "E",
    measurement = "M",
    definitional = "\u00D7",
    type
  )
}

#' Edge ids that are analysis paths (exclude wiring into ×)
#' @keywords internal
analyzable_edge_ids <- function(state, edge_ids = NULL) {
  if (is.null(edge_ids)) edge_ids <- state$edges$id
  if (!length(edge_ids) || !nrow(state$edges)) return(character())
  keep <- vapply(edge_ids, function(eid) {
    idx <- match(eid, state$edges$id)
    if (is.na(idx)) return(FALSE)
    if (identical(state$edges$type[idx], "definitional")) return(FALSE)
    !is_interaction_node(state, state$edges$to[idx])
  }, logical(1))
  as.character(edge_ids[keep])
}

#' Format a p-value for display (never show 0; use <0.001)
#' @keywords internal
format_pvalue <- function(p, digits = 3) {
  if (length(p) != 1L || is.na(p)) return(NA_character_)
  thresh <- 10^(-digits)
  if (p < thresh) {
    paste0("<", formatC(thresh, digits = digits, format = "f"))
  } else {
    formatC(p, digits = digits, format = "f")
  }
}

#' Remove an edge by id
#' @keywords internal
remove_edge <- function(state, edge_id) {
  state$edges <- state$edges[state$edges$id != edge_id, , drop = FALSE]
  refresh_interaction_nodes(state)
}

#' Ensure node R² columns exist
#' @keywords internal
ensure_node_r2_columns <- function(state) {
  if (!"r_squared" %in% names(state$nodes)) state$nodes$r_squared <- NA_real_
  if (!"r_squared_kind" %in% names(state$nodes)) state$nodes$r_squared_kind <- NA_character_
  state
}

#' Apply R² map (node id -> list(value, kind)) onto nodes
#' @keywords internal
apply_r_squared_to_nodes <- function(state, r2_map) {
  state <- ensure_node_r2_columns(state)
  state$nodes$r_squared <- NA_real_
  state$nodes$r_squared_kind <- NA_character_
  if (is.null(r2_map) || !length(r2_map) || !nrow(state$nodes)) return(state)
  for (nid in names(r2_map)) {
    idx <- match(nid, state$nodes$id)
    if (is.na(idx)) next
    slot <- r2_map[[nid]]
    if (is.null(slot)) next
    val <- if (is.list(slot)) slot$value else slot
    kind <- if (is.list(slot) && !is.null(slot$kind)) slot$kind else "r2"
    if (!is.finite(val)) next
    state$nodes$r_squared[idx] <- as.numeric(val)
    state$nodes$r_squared_kind[idx] <- as.character(kind)
  }
  state
}

#' Reset fit results on edges
#' @keywords internal
clear_fit_results <- function(state) {
  state <- ensure_node_r2_columns(state)
  state <- ensure_edge_to_port(state)
  state <- refresh_interaction_nodes(state)
  if (nrow(state$nodes)) {
    state$nodes$r_squared <- NA_real_
    state$nodes$r_squared_kind <- NA_character_
  }
  if (!nrow(state$edges)) return(state)
  if (!"level" %in% names(state$edges)) state$edges$level <- NA_character_
  state$edges$status <- "pending"
  state$edges$estimate <- NA_real_
  state$edges$se <- NA_real_
  state$edges$pvalue <- NA_real_
  if (!"estimate_std" %in% names(state$edges)) state$edges$estimate_std <- NA_real_
  state$edges$estimate_std <- NA_real_
  state$edges$label <- vapply(seq_len(nrow(state$edges)), function(i) {
    if (is_interaction_node(state, state$edges$to[i])) {
      tp <- state$edges$to_port[i]
      lv <- state$edges$level[i]
      lab <- paste0("\u2192\u00D7", if (!is.na(tp) && nzchar(tp)) tp else "")
      if (!is.na(lv) && nzchar(lv)) lab <- paste0(lab, "\u00B7", lv)
      return(lab)
    }
    lab <- type_short_label(state$edges$type[i])
    lv <- state$edges$level[i]
    if (identical(node_cat_mode(state, state$edges$from[i]), "omnibus") &&
          (is.na(lv) || !nzchar(lv))) {
      paste0(lab, "·omnibus")
    } else if (!is.na(lv) && nzchar(lv)) {
      paste0(lab, "·", lv)
    } else {
      lab
    }
  }, character(1))
  state
}

#' Apply parameter estimates back onto edges
#' @keywords internal
apply_estimates_to_edges <- function(state, estimates, included_edge_ids = NULL) {
  state <- clear_fit_results(state)
  if (!nrow(state$edges)) return(state)

  if (is.null(included_edge_ids)) {
    included_edge_ids <- state$edges$id
  }

  for (i in seq_len(nrow(state$edges))) {
    eid <- state$edges$id[i]
    if (!eid %in% included_edge_ids) {
      state$edges$status[i] <- "excluded"
      state$edges$label[i] <- paste0(type_short_label(state$edges$type[i]), " ✕")
      next
    }
    state$edges$status[i] <- "included"
    from <- state$edges$from[i]
    to <- state$edges$to[i]
    etype <- state$edges$type[i]

    # Definitional wires into × — no regression coefficient
    if (identical(etype, "definitional") || is_interaction_node(state, to)) {
      tp <- if ("to_port" %in% names(state$edges)) state$edges$to_port[i] else NA_character_
      state$edges$estimate[i] <- NA_real_
      state$edges$se[i] <- NA_real_
      state$edges$pvalue[i] <- NA_real_
      state$edges$estimate_std[i] <- NA_real_
      state$edges$status[i] <- "included"
      state$edges$label[i] <- paste0("\u2192\u00D7", if (!is.na(tp) && nzchar(tp)) tp else "")
      next
    }

    from_nm <- lavaan_var_name(state, from)
    to_nm <- lavaan_var_name(state, to)
    edge_level <- if ("level" %in% names(state$edges)) state$edges$level[i] else NA_character_
    is_omni_edge <- identical(node_cat_mode(state, from), "omnibus")
    if (is_interaction_node(state, from)) {
      fac_ix <- interaction_factors(state, from)
      is_omni_edge <- identical(node_cat_mode(state, fac_ix$a), "omnibus") ||
        identical(node_cat_mode(state, fac_ix$b), "omnibus")
    }

    if (etype == "measurement") {
      hit <- estimates$lhs == from_nm & estimates$op == "=~" & estimates$rhs == to_nm
    } else {
      hit <- estimates$lhs == to_nm & estimates$op == "~" & estimates$rhs == from_nm
      if ("kind" %in% names(estimates) && any(hit)) {
        if (is_omni_edge) {
          om <- hit & estimates$kind == "omnibus"
          if (any(om)) hit <- om
        } else if (is_interaction_node(state, from)) {
          # Split interaction: one (or more) product terms; keep effects
          hit <- hit & estimates$kind == "effect"
        } else if (!is.na(edge_level) && nzchar(edge_level)) {
          hit <- hit & estimates$kind == "effect" &
            !is.na(estimates$level) & estimates$level == edge_level
        } else {
          # Primary continuous / binary term: exclude poly + omnibus
          hit <- hit & estimates$kind == "effect" &
            (is.na(estimates$level) | !nzchar(as.character(estimates$level)) |
               !vapply(estimates$level, is_poly_level, logical(1)))
        }
      } else if ("level" %in% names(estimates)) {
        if (!is.na(edge_level) && nzchar(edge_level)) {
          hit <- hit & !is.na(estimates$level) & estimates$level == edge_level
        } else {
          hit <- hit & (is.na(estimates$level) | !nzchar(as.character(estimates$level)) |
            !vapply(estimates$level, is_poly_level, logical(1)))
        }
      }
    }

    if (any(hit)) {
      row <- estimates[which(hit)[1], ]
      est <- as.numeric(row$est)
      se <- as.numeric(row$se)
      pv <- as.numeric(row$pvalue)
      est_std <- if ("est_std" %in% names(row)) as.numeric(row$est_std) else NA_real_
      df_om <- if ("df" %in% names(row)) as.numeric(row$df) else NA_real_
      state$edges$estimate[i] <- est
      state$edges$se[i] <- se
      state$edges$pvalue[i] <- pv
      state$edges$estimate_std[i] <- est_std
      prefix <- type_short_label(etype)
      if (is_omni_edge) {
        prefix <- paste0(prefix, "·omnibus")
      } else if (!is.na(edge_level) && nzchar(edge_level)) {
        prefix <- paste0(prefix, "·", edge_level)
      }
      # Append polynomial terms for this predictor on the same DV
      poly_bits <- character()
      if (etype != "measurement" && !is_omni_edge && "level" %in% names(estimates)) {
        poly_hit <- estimates$lhs == to_nm & estimates$op == "~" &
          estimates$rhs == from_nm &
          vapply(estimates$level, is_poly_level, logical(1))
        if (any(poly_hit)) {
          prows <- estimates[poly_hit, , drop = FALSE]
          prows <- prows[order(prows$level), , drop = FALSE]
          for (pi in seq_len(nrow(prows))) {
            deg <- sub("^\\^", "", as.character(prows$level[pi]))
            pow <- if (identical(deg, "2")) "\u00B2" else if (identical(deg, "3")) "\u00B3" else paste0("^", deg)
            poly_bits <- c(
              poly_bits,
              sprintf("%+.2f·%s%s", as.numeric(prows$est[pi]), from_nm, pow)
            )
          }
        }
      }
      poly_txt <- if (length(poly_bits)) paste0(" ", paste(poly_bits, collapse = " ")) else ""
      if (is_omni_edge && is.finite(pv)) {
        df_txt <- if (is.finite(df_om)) sprintf("df=%s, ", formatC(df_om, digits = 0, format = "f")) else ""
        state$edges$label[i] <- sprintf("%s (%sp=%s)", prefix, df_txt, format_pvalue(pv))
      } else if (!is.na(est) && !is.na(se) && !is.na(pv)) {
        state$edges$label[i] <- sprintf(
          "%s %.2f%s (SE=%.2f, p=%s)", prefix, est, poly_txt, se, format_pvalue(pv)
        )
      } else if (!is.na(est) && !is.na(pv)) {
        state$edges$label[i] <- sprintf(
          "%s %.2f%s (p=%s)", prefix, est, poly_txt, format_pvalue(pv)
        )
      } else if (!is.na(est)) {
        state$edges$label[i] <- sprintf("%s %.2f%s", prefix, est, poly_txt)
      } else {
        state$edges$label[i] <- prefix
      }
    } else {
      lab <- type_short_label(etype)
      if (is_omni_edge) {
        lab <- paste0(lab, "·omnibus")
      } else if (!is.na(edge_level) && nzchar(edge_level)) {
        lab <- paste0(lab, "·", edge_level)
      }
      state$edges$label[i] <- lab
    }
  }
  state
}

node_display_name <- function(state, node_id) {
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(node_id)
  col <- state$nodes$observed_column[idx]
  if (!is.na(col) && nzchar(col)) return(col)
  state$nodes$label[idx]
}

is_observed_node <- function(state, node_id) {
  if (is_interaction_node(state, node_id)) return(FALSE)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(FALSE)
  col <- state$nodes$observed_column[idx]
  !is.na(col) && nzchar(col)
}
