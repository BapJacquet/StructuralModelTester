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
      repeated_measures = logical(),
      cluster_column = character(),
      dv_transform = character(),
      factor_base = character(),
      factor_ordered = logical(),
      factor_levels = character(),
      sim_mean = numeric(),
      sim_sd = numeric(),
      sim_resid_sd = numeric(),
      sim_var_kind = character(),
      sim_level_props = character(),
      sim_level_counts = character(),
      sim_design_mode = character(),
      sim_alloc_mode = character(),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = character(),
      from = character(),
      to = character(),
      type = character(),
      level = character(),
      to_port = character(),
      to_level = character(),
      rel_kind = character(),
      status = character(),
      estimate = numeric(),
      se = numeric(),
      pvalue = numeric(),
      estimate_std = numeric(),
      label = character(),
      prior_family = character(),
      prior_mean = numeric(),
      prior_sd = numeric(),
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

#' True if node is a latent (explicit kind, or legacy unmapped non-observed)
#' @keywords internal
is_latent_node <- function(state, node_id) {
  state <- ensure_node_kind_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(FALSE)
  if (identical(state$nodes$node_kind[idx], "interaction")) return(FALSE)
  if (identical(state$nodes$node_kind[idx], "latent")) return(TRUE)
  if (identical(state$nodes$node_kind[idx], "observed")) return(FALSE)
  !is_observed_node(state, node_id)
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
  if (!"to_level" %in% names(state$edges)) state$edges$to_level <- NA_character_
  if (!"rel_kind" %in% names(state$edges)) state$edges$rel_kind <- "linear"
  miss <- is.na(state$edges$rel_kind) | !nzchar(state$edges$rel_kind)
  if (any(miss)) state$edges$rel_kind[miss] <- "linear"
  # Drop legacy "none" (points-only) → linear
  none_idx <- which(state$edges$rel_kind == "none")
  if (length(none_idx)) state$edges$rel_kind[none_idx] <- "linear"
  ensure_edge_priors(state)
}

#' Default / migrate Normal priors on edges (unstandardized coefficient `b`)
#'
#' Stored as Normal(mean, sd) for simulation / later Bayesian work.
#' Continuous links use mean-centered X so `mean` is a raw slope; factor links
#' use treatment contrasts. UI: Expected b + Uncertainty. Defaults 0 / 1.
#' @keywords internal
ensure_edge_priors <- function(state) {
  n <- nrow(state$edges)
  if (!"prior_family" %in% names(state$edges)) {
    state$edges$prior_family <- rep("normal", n)
  }
  if (!"prior_mean" %in% names(state$edges)) {
    state$edges$prior_mean <- rep(0, n)
  }
  if (!"prior_sd" %in% names(state$edges)) {
    state$edges$prior_sd <- rep(1, n)
  }
  if (!n) return(state)
  miss_f <- is.na(state$edges$prior_family) | !nzchar(as.character(state$edges$prior_family))
  if (any(miss_f)) state$edges$prior_family[miss_f] <- "normal"
  bad_f <- !state$edges$prior_family %in% "normal"
  if (any(bad_f)) state$edges$prior_family[bad_f] <- "normal"
  miss_m <- is.na(state$edges$prior_mean) | !is.finite(state$edges$prior_mean)
  if (any(miss_m)) state$edges$prior_mean[miss_m] <- 0
  miss_s <- is.na(state$edges$prior_sd) | !is.finite(state$edges$prior_sd) |
    state$edges$prior_sd <= 0
  if (any(miss_s)) state$edges$prior_sd[miss_s] <- 1
  state
}

#' Read prior for one edge
#' @keywords internal
edge_prior <- function(state, edge_id) {
  state <- ensure_edge_priors(state)
  idx <- match(edge_id, state$edges$id)
  if (is.na(idx)) {
    return(list(family = "normal", mean = 0, sd = 1))
  }
  list(
    family = as.character(state$edges$prior_family[idx]),
    mean = as.numeric(state$edges$prior_mean[idx]),
    sd = as.numeric(state$edges$prior_sd[idx])
  )
}

#' Set Normal prior parameters on one edge (does not clear fit estimates)
#' @keywords internal
set_edge_prior <- function(state, edge_id, mean = 0, sd = 1, family = "normal") {
  state <- ensure_edge_priors(state)
  idx <- match(edge_id, state$edges$id)
  if (is.na(idx)) return(state)
  if (!identical(family, "normal")) family <- "normal"
  mean <- as.numeric(mean)
  sd <- as.numeric(sd)
  if (!is.finite(mean)) mean <- 0
  if (!is.finite(sd) || sd <= 0) sd <- 1
  state$edges$prior_family[idx] <- family
  state$edges$prior_mean[idx] <- mean
  state$edges$prior_sd[idx] <- sd
  state
}

#' Structural edges that carry expected-effect priors (excl. measurement / × wires)
#' @keywords internal
structural_prior_edges <- function(state) {
  state <- ensure_edge_to_port(state)
  if (!nrow(state$edges)) return(state$edges[0, , drop = FALSE])
  hit <- !state$edges$type %in% c("measurement", "definitional")
  if (any(hit)) {
    into_x <- vapply(
      state$edges$to[hit],
      function(id) is_interaction_node(state, id),
      logical(1)
    )
    hit[which(hit)] <- hit[which(hit)] & !into_x
  }
  state$edges[hit, , drop = FALSE]
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
  coding <- variable_coding_for_node(state, node_id, data)
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

#' Match edges on optional response level (categorical DV split)
#' @keywords internal
edge_to_level_match <- function(edges, to_level = NA_character_) {
  if (!"to_level" %in% names(edges)) {
    return(rep(is.na(to_level) || !nzchar(as.character(to_level)), nrow(edges)))
  }
  tl <- edges$to_level
  want <- if (is.null(to_level) || is.na(to_level) || !nzchar(as.character(to_level))) {
    is.na(tl) | !nzchar(as.character(tl))
  } else {
    !is.na(tl) & as.character(tl) == as.character(to_level)
  }
  want
}

#' True if an unleveled structural edge from→to already exists
#' @keywords internal
has_plain_edge <- function(state, from, to, to_level = NA_character_) {
  if (!nrow(state$edges)) return(FALSE)
  state <- ensure_edge_to_port(state)
  hit <- state$edges$from == from & state$edges$to == to &
    !state$edges$type %in% c("measurement", "definitional") &
    edge_to_level_match(state$edges, to_level)
  if (!any(hit)) return(FALSE)
  lv <- state$edges$level[hit]
  any(is.na(lv) | !nzchar(lv))
}

#' True if a leveled structural edge from→to already exists
#' @keywords internal
has_leveled_edge <- function(state, from, to, level, to_level = NA_character_) {
  if (!nrow(state$edges) || is.na(level) || !nzchar(level)) return(FALSE)
  state <- ensure_edge_to_port(state)
  hit <- state$edges$from == from & state$edges$to == to &
    !state$edges$type %in% c("measurement", "definitional") &
    edge_to_level_match(state$edges, to_level)
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
    if (!length(ix_hit)) next
    # One main-effect set per response level targeted by × → DV
    resp_levels <- if ("to_level" %in% names(out_edges)) {
      unique(as.character(out_edges$to_level[ix_hit]))
    } else {
      NA_character_
    }
    if (!length(resp_levels)) resp_levels <- NA_character_

    for (tl in resp_levels) {
      hit_tl <- ix_hit
      if ("to_level" %in% names(out_edges)) {
        if (is.na(tl) || !nzchar(tl)) {
          hit_tl <- ix_hit[is.na(out_edges$to_level[ix_hit]) |
                             !nzchar(as.character(out_edges$to_level[ix_hit]))]
        } else {
          hit_tl <- ix_hit[!is.na(out_edges$to_level[ix_hit]) &
                             out_edges$to_level[ix_hit] == tl]
        }
      }
      if (!length(hit_tl)) next
      link_type <- as.character(out_edges$type[hit_tl[1]])
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
          if (has_plain_edge(state, f, dv, to_level = tl)) next
          state <- add_edge(
            state, from = f, to = dv, type = link_type,
            level = NA_character_, to_level = tl
          )
        } else {
          if (has_leveled_edge(state, f, dv, lvl, to_level = tl) ||
                has_plain_edge(state, f, dv, to_level = tl)) next
          state <- add_edge(
            state, from = f, to = dv, type = link_type,
            level = lvl, to_level = tl
          )
        }
      }
    }
  }
  state
}

#' Add a node to the model
#'
#' @param kind `"observed"` (default when a column is mapped, or for Add node)
#'   or `"latent"` (Add latent). Observed nodes may be unmapped (`observed_column`
#'   empty) without becoming latents.
#' @keywords internal
add_node <- function(state, label, observed_column = NA_character_,
                     kind = NULL) {
  state <- ensure_node_r2_columns(state)
  state <- ensure_node_kind_columns(state)
  state <- ensure_node_rm_columns(state)
  state <- ensure_node_transform_columns(state)
  state <- ensure_node_factor_coding_columns(state)
  state <- ensure_node_sim_columns(state)
  id <- unique_node_id(state, label)
  is_obs_col <- !is.na(observed_column) && nzchar(observed_column)
  if (is.null(kind) || !nzchar(as.character(kind))) {
    kind <- if (is_obs_col) "observed" else "latent"
  }
  kind <- as.character(kind)
  if (!kind %in% c("observed", "latent")) {
    kind <- if (is_obs_col) "observed" else "latent"
  }
  shape <- if (identical(kind, "observed")) "box" else "ellipse"
  n <- nrow(state$nodes)
  # Stagger new nodes so they are not stacked
  new_row <- data.frame(
    id = id,
    label = label,
    observed_column = if (is_obs_col) as.character(observed_column) else NA_character_,
    shape = shape,
    x = 80 + (n %% 4) * 160,
    y = 80 + (n %/% 4) * 100,
    r_squared = NA_real_,
    r_squared_kind = NA_character_,
    node_kind = kind,
    factor_a = NA_character_,
    factor_b = NA_character_,
    cat_mode = "split",
    repeated_measures = FALSE,
    cluster_column = NA_character_,
    dv_transform = "none",
    factor_base = NA_character_,
    factor_ordered = FALSE,
    factor_levels = NA_character_,
    sim_mean = 0,
    sim_sd = 1,
    sim_resid_sd = 1,
    sim_var_kind = "continuous",
    sim_level_props = NA_character_,
    sim_level_counts = NA_character_,
    sim_design_mode = "inter",
    sim_alloc_mode = "probability",
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
  state <- ensure_node_rm_columns(state)
  state <- ensure_node_transform_columns(state)
  state <- ensure_node_factor_coding_columns(state)
  state <- ensure_node_sim_columns(state)
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
    repeated_measures = FALSE,
    cluster_column = NA_character_,
    dv_transform = "none",
    factor_base = NA_character_,
    factor_ordered = FALSE,
    factor_levels = NA_character_,
    sim_mean = 0,
    sim_sd = 1,
    sim_resid_sd = 1,
    sim_var_kind = "continuous",
    sim_level_props = NA_character_,
    sim_level_counts = NA_character_,
    sim_design_mode = "inter",
    sim_alloc_mode = "probability",
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
#' @param to_level For categorical DV in split mode: response category
#'   (one-vs-rest). `NA` otherwise.
#' @keywords internal
add_edge <- function(state, from, to,
                     type = c("fixed", "confirmatory", "exploratory", "measurement"),
                     level = NA_character_,
                     to_port = NA_character_,
                     to_level = NA_character_) {
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
  if (is.null(to_level) || !nzchar(as.character(to_level))) to_level <- NA_character_
  to_level <- as.character(to_level)

  # Wire into interaction operator (definitional; not a regression path)
  if (is_interaction_node(state, to)) {
    if (!to_port %in% c("a", "b")) return(state)
    # Observed or latent (latents are scored in the two-step path stage)
    if (!is_observed_node(state, from) && !is_latent_node(state, from)) return(state)
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
      to_level = NA_character_,
      rel_kind = "linear",
      status = "pending",
      estimate = NA_real_,
      se = NA_real_,
      pvalue = NA_real_,
      estimate_std = NA_real_,
      label = lab,
      prior_family = "normal",
      prior_mean = 0,
      prior_sd = 1,
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
    !state$edges$type %in% c("measurement", "definitional") &
    edge_to_level_match(state$edges, to_level)
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
  if (!is.na(to_level) && nzchar(to_level)) {
    lab <- paste0(lab, "\u2192", to_level)
  }
  new_row <- data.frame(
    id = eid,
    from = from,
    to = to,
    type = type,
    level = level,
    to_port = to_port,
    to_level = to_level,
    rel_kind = "linear",
    status = "pending",
    estimate = NA_real_,
    se = NA_real_,
    pvalue = NA_real_,
    estimate_std = NA_real_,
    label = lab,
    prior_family = "normal",
    prior_mean = 0,
    prior_sd = 1,
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

#' Ensure simulation generative columns on nodes
#'
#' - `sim_mean` / `sim_sd`: exogenous continuous draws (defaults 0 / 1).
#' - `sim_resid_sd`: structural-DV residual SD on the outcome scale (default 1).
#' - `sim_var_kind`: declared type for unmapped nodes
#'   (`continuous` / `categorical` / `binary`).
#' - `sim_level_props`: encoded level=proportion map for factor simulation.
#' - `sim_level_counts`: encoded level=count map for exact allocation.
#' - `sim_design_mode`: `"inter"` (between) or `"intra"` (within / repeated).
#' - `sim_alloc_mode`: `"probability"` or `"exact"` level allocation.
#' @keywords internal
ensure_node_sim_columns <- function(state) {
  n <- nrow(state$nodes)
  if (!"sim_mean" %in% names(state$nodes)) {
    state$nodes$sim_mean <- rep(0, n)
  }
  if (!"sim_sd" %in% names(state$nodes)) {
    state$nodes$sim_sd <- rep(1, n)
  }
  if (!"sim_resid_sd" %in% names(state$nodes)) {
    state$nodes$sim_resid_sd <- rep(1, n)
  }
  if (!"sim_var_kind" %in% names(state$nodes)) {
    state$nodes$sim_var_kind <- rep("continuous", n)
  }
  if (!"sim_level_props" %in% names(state$nodes)) {
    state$nodes$sim_level_props <- rep(NA_character_, n)
  }
  if (!"sim_level_counts" %in% names(state$nodes)) {
    state$nodes$sim_level_counts <- rep(NA_character_, n)
  }
  if (!"sim_design_mode" %in% names(state$nodes)) {
    state$nodes$sim_design_mode <- rep("inter", n)
  }
  if (!"sim_alloc_mode" %in% names(state$nodes)) {
    state$nodes$sim_alloc_mode <- rep("probability", n)
  }
  if (!n) return(state)
  miss_m <- is.na(state$nodes$sim_mean) | !is.finite(state$nodes$sim_mean)
  if (any(miss_m)) state$nodes$sim_mean[miss_m] <- 0
  miss_s <- is.na(state$nodes$sim_sd) | !is.finite(state$nodes$sim_sd) |
    state$nodes$sim_sd <= 0
  if (any(miss_s)) state$nodes$sim_sd[miss_s] <- 1
  miss_r <- is.na(state$nodes$sim_resid_sd) | !is.finite(state$nodes$sim_resid_sd) |
    state$nodes$sim_resid_sd <= 0
  if (any(miss_r)) state$nodes$sim_resid_sd[miss_r] <- 1
  miss_k <- is.na(state$nodes$sim_var_kind) | !nzchar(state$nodes$sim_var_kind)
  if (any(miss_k)) state$nodes$sim_var_kind[miss_k] <- "continuous"
  bad_k <- !state$nodes$sim_var_kind %in% c("continuous", "categorical", "binary")
  if (any(bad_k)) state$nodes$sim_var_kind[bad_k] <- "continuous"
  miss_dm <- is.na(state$nodes$sim_design_mode) | !nzchar(state$nodes$sim_design_mode)
  if (any(miss_dm)) state$nodes$sim_design_mode[miss_dm] <- "inter"
  bad_dm <- !state$nodes$sim_design_mode %in% c("inter", "intra")
  if (any(bad_dm)) state$nodes$sim_design_mode[bad_dm] <- "inter"
  miss_am <- is.na(state$nodes$sim_alloc_mode) | !nzchar(state$nodes$sim_alloc_mode)
  if (any(miss_am)) state$nodes$sim_alloc_mode[miss_am] <- "probability"
  bad_am <- !state$nodes$sim_alloc_mode %in% c("probability", "exact")
  if (any(bad_am)) state$nodes$sim_alloc_mode[bad_am] <- "probability"
  state
}

#' TRUE if node has no incoming structural paths (exogenous for simulation)
#' @keywords internal
node_is_structural_exogenous <- function(state, node_id) {
  state <- ensure_edge_to_port(state)
  if (is.na(node_id) || !nzchar(as.character(node_id))) return(FALSE)
  if (is_interaction_node(state, node_id)) return(FALSE)
  if (!nrow(state$edges)) return(TRUE)
  hit <- state$edges$to == node_id &
    !state$edges$type %in% c("measurement", "definitional")
  !any(hit)
}

#' Simulation knobs for one node
#' @return list(mean, sd, resid_sd)
#' @keywords internal
node_sim_params <- function(state, node_id) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) {
    return(list(mean = 0, sd = 1, resid_sd = 1))
  }
  list(
    mean = as.numeric(state$nodes$sim_mean[idx]),
    sd = as.numeric(state$nodes$sim_sd[idx]),
    resid_sd = as.numeric(state$nodes$sim_resid_sd[idx])
  )
}

#' Set exogenous continuous simulation mean/SD (does not clear fits)
#' @keywords internal
set_node_sim_exogenous <- function(state, node_id, mean = 0, sd = 1) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  mean <- as.numeric(mean)
  sd <- as.numeric(sd)
  if (!is.finite(mean)) mean <- 0
  if (!is.finite(sd) || sd <= 0) sd <- 1
  state$nodes$sim_mean[idx] <- mean
  state$nodes$sim_sd[idx] <- sd
  state
}

#' Set structural-DV residual SD on the outcome scale (does not clear fits)
#' @keywords internal
set_node_sim_resid_sd <- function(state, node_id, resid_sd = 1) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  resid_sd <- as.numeric(resid_sd)
  if (!is.finite(resid_sd) || resid_sd <= 0) resid_sd <- 1
  state$nodes$sim_resid_sd[idx] <- resid_sd
  state
}

#' Encode level → proportion map for simulation storage
#' @keywords internal
encode_sim_level_props <- function(props) {
  if (is.null(props) || !length(props)) return(NA_character_)
  nms <- names(props)
  if (is.null(nms)) return(NA_character_)
  nms <- as.character(nms)
  vals <- as.numeric(props)
  keep <- !is.na(nms) & nzchar(nms) & is.finite(vals)
  if (!any(keep)) return(NA_character_)
  paste(
    sprintf("%s=%s", nms[keep], format(vals[keep], scientific = FALSE, trim = TRUE)),
    collapse = "|"
  )
}

#' Decode stored level=prop string to a named numeric vector
#' @keywords internal
decode_sim_level_props <- function(text) {
  if (is.null(text) || length(text) != 1L || is.na(text) || !nzchar(text)) {
    return(numeric())
  }
  parts <- strsplit(as.character(text), "|", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(numeric())
  out <- numeric(length(parts))
  nms <- character(length(parts))
  for (i in seq_along(parts)) {
    kv <- strsplit(parts[[i]], "=", fixed = TRUE)[[1]]
    if (length(kv) < 2L) {
      nms[[i]] <- trimws(kv[[1]])
      out[[i]] <- NA_real_
    } else {
      nms[[i]] <- trimws(kv[[1]])
      out[[i]] <- suppressWarnings(as.numeric(trimws(kv[[2]])))
    }
  }
  keep <- nzchar(nms)
  stats::setNames(out[keep], nms[keep])
}

#' Clip and renormalize proportions (equal weights if empty / all zero)
#' @keywords internal
normalize_sim_props <- function(props, levels) {
  levels <- unique(as.character(levels))
  levels <- levels[!is.na(levels) & nzchar(levels)]
  if (!length(levels)) return(numeric())
  raw <- rep(NA_real_, length(levels))
  names(raw) <- levels
  if (!is.null(props) && length(props)) {
    hit <- intersect(names(props), levels)
    if (length(hit)) raw[hit] <- as.numeric(props[hit])
  }
  raw[!is.finite(raw) | raw < 0] <- 0
  s <- sum(raw)
  if (!is.finite(s) || s <= 0) {
    return(stats::setNames(rep(1 / length(levels), length(levels)), levels))
  }
  raw / s
}

#' Declared or data-inferred variable kind for simulation
#'
#' Mapped columns win via `classify_variable`. Unmapped use `sim_var_kind`.
#' @keywords internal
node_sim_var_kind <- function(state, node_id, data = NULL) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return("continuous")
  col <- state$nodes$observed_column[idx]
  if (!is.na(col) && nzchar(col) && !is.null(data) && col %in% names(data)) {
    kind <- classify_variable(data[[col]])
    if (kind %in% c("continuous", "categorical", "binary")) return(kind)
  }
  kind <- as.character(state$nodes$sim_var_kind[idx])
  if (!kind %in% c("continuous", "categorical", "binary")) kind <- "continuous"
  kind
}

#' Set declared simulation variable kind (unmapped authority)
#' @keywords internal
set_node_sim_var_kind <- function(state, node_id,
                                  kind = c("continuous", "categorical", "binary")) {
  kind <- match.arg(kind)
  state <- ensure_node_sim_columns(state)
  state <- ensure_node_factor_coding_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  state$nodes$sim_var_kind[idx] <- kind
  if (identical(kind, "binary")) {
    meta <- node_factor_coding(state, node_id)
    levs <- meta$levels
    if (!length(levs)) {
      base <- if (!is.na(meta$base) && nzchar(meta$base)) meta$base else "0"
      other <- if (identical(base, "1")) "0" else "1"
      levs <- c(base, other)
      state <- set_node_factor_coding(
        state, node_id,
        base = base, levels = levs, data = NULL
      )
    }
  }
  if (identical(kind, "categorical")) {
    meta <- node_factor_coding(state, node_id)
    if (!length(meta$levels)) {
      state <- set_node_factor_coding(
        state, node_id,
        base = "A", levels = c("A", "B", "C"), data = NULL
      )
    }
  }
  state
}

#' Level names used when simulating a factor node
#' @keywords internal
node_sim_factor_levels <- function(state, node_id, data = NULL) {
  state <- ensure_node_factor_coding_columns(state)
  meta <- node_factor_coding(state, node_id)
  kind <- node_sim_var_kind(state, node_id, data = data)
  col <- state$nodes$observed_column[match(node_id, state$nodes$id)]
  data_levs <- character()
  if (!is.na(col) && nzchar(col) && !is.null(data) && col %in% names(data)) {
    coding <- variable_coding_for_node(state, node_id, data)
    data_levs <- as.character(coding$all_levels %||% character())
  }
  levs <- meta$levels
  if (!length(levs) && length(data_levs)) levs <- data_levs
  if (!length(levs) && identical(kind, "binary")) levs <- c("0", "1")
  if (!length(levs) && identical(kind, "categorical")) levs <- c("A", "B", "C")
  base <- meta$base
  if (is.na(base) || !nzchar(base)) {
    base <- if (length(data_levs)) data_levs[[1]] else if (length(levs)) levs[[1]] else NA_character_
  }
  if (!is.na(base) && nzchar(base)) {
    levs <- unique(c(base, setdiff(levs, base)))
  }
  levs[!is.na(levs) & nzchar(levs)]
}

#' Named proportions for simulating a factor node
#' @keywords internal
node_sim_level_props <- function(state, node_id, data = NULL) {
  state <- ensure_node_sim_columns(state)
  levs <- node_sim_factor_levels(state, node_id, data = data)
  idx <- match(node_id, state$nodes$id)
  raw <- if (is.na(idx)) numeric() else decode_sim_level_props(state$nodes$sim_level_props[idx])
  normalize_sim_props(raw, levs)
}

#' Store level proportions for simulation (does not clear fits)
#' @keywords internal
set_node_sim_level_props <- function(state, node_id, props) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  levs <- names(props)
  if (is.null(levs)) levs <- character()
  norm <- normalize_sim_props(props, levs)
  state$nodes$sim_level_props[idx] <- encode_sim_level_props(norm)
  state
}

#' Normalize exact per-level counts (default 1; clip negatives / non-finite to 0)
#' @keywords internal
normalize_sim_counts <- function(counts, levels) {
  levels <- unique(as.character(levels))
  levels <- levels[!is.na(levels) & nzchar(levels)]
  if (!length(levels)) return(integer())
  raw <- rep(1L, length(levels))
  names(raw) <- levels
  if (!is.null(counts) && length(counts)) {
    hit <- intersect(names(counts), levels)
    if (length(hit)) {
      vals <- suppressWarnings(as.integer(round(as.numeric(counts[hit]))))
      vals[!is.finite(vals) | is.na(vals) | vals < 0L] <- 0L
      raw[hit] <- vals
    }
  }
  raw
}

#' Encode level → count map for simulation storage
#' @keywords internal
encode_sim_level_counts <- function(counts) {
  if (is.null(counts) || !length(counts)) return(NA_character_)
  nms <- names(counts)
  if (is.null(nms)) return(NA_character_)
  nms <- as.character(nms)
  vals <- suppressWarnings(as.integer(round(as.numeric(counts))))
  keep <- !is.na(nms) & nzchar(nms) & is.finite(vals)
  if (!any(keep)) return(NA_character_)
  paste(sprintf("%s=%d", nms[keep], vals[keep]), collapse = "|")
}

#' Decode stored level=count string to a named integer vector
#' @keywords internal
decode_sim_level_counts <- function(text) {
  raw <- decode_sim_level_props(text)
  if (!length(raw)) return(integer())
  vals <- suppressWarnings(as.integer(round(as.numeric(raw))))
  vals[!is.finite(vals) | is.na(vals) | vals < 0L] <- 0L
  stats::setNames(vals, names(raw))
}

#' Named exact counts for simulating a factor node
#' @keywords internal
node_sim_level_counts <- function(state, node_id, data = NULL) {
  state <- ensure_node_sim_columns(state)
  levs <- node_sim_factor_levels(state, node_id, data = data)
  idx <- match(node_id, state$nodes$id)
  raw <- if (is.na(idx)) integer() else decode_sim_level_counts(state$nodes$sim_level_counts[idx])
  normalize_sim_counts(raw, levs)
}

#' Store exact level counts for simulation (does not clear fits)
#' @keywords internal
set_node_sim_level_counts <- function(state, node_id, counts) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  levs <- names(counts)
  if (is.null(levs)) levs <- character()
  norm <- normalize_sim_counts(counts, levs)
  state$nodes$sim_level_counts[idx] <- encode_sim_level_counts(norm)
  state
}

#' Simulation design mode for a factor node: inter (between) or intra (within)
#' @keywords internal
node_sim_design_mode <- function(state, node_id) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return("inter")
  dm <- as.character(state$nodes$sim_design_mode[idx])
  if (!dm %in% c("inter", "intra")) dm <- "inter"
  dm
}

#' Set inter/intra simulation design mode (does not clear fits)
#' @keywords internal
set_node_sim_design_mode <- function(state, node_id,
                                     mode = c("inter", "intra")) {
  mode <- match.arg(mode)
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  state$nodes$sim_design_mode[idx] <- mode
  state
}

#' Simulation allocation mode: probability or exact counts
#' @keywords internal
node_sim_alloc_mode <- function(state, node_id) {
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return("probability")
  am <- as.character(state$nodes$sim_alloc_mode[idx])
  if (!am %in% c("probability", "exact")) am <- "probability"
  am
}

#' Set probability/exact simulation allocation mode (does not clear fits)
#' @keywords internal
set_node_sim_alloc_mode <- function(state, node_id,
                                    mode = c("probability", "exact")) {
  mode <- match.arg(mode)
  state <- ensure_node_sim_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  # When switching to exact with empty counts, seed 1 per known level
  if (identical(mode, "exact")) {
    levs <- node_sim_factor_levels(state, node_id, data = NULL)
    raw <- decode_sim_level_counts(state$nodes$sim_level_counts[idx])
    if (!length(raw) && length(levs)) {
      state <- set_node_sim_level_counts(
        state, node_id,
        stats::setNames(rep(1L, length(levs)), levs)
      )
    }
  }
  state$nodes$sim_alloc_mode[idx] <- mode
  state
}

#' TRUE if any exogenous categorical/binary node is marked intra
#' @keywords internal
state_has_intra_sim_factor <- function(state) {
  state <- ensure_node_sim_columns(state)
  if (!nrow(state$nodes)) return(FALSE)
  for (id in state$nodes$id) {
    if (is_interaction_node(state, id)) next
    if (!node_is_structural_exogenous(state, id)) next
    kind <- node_sim_var_kind(state, id, data = NULL)
    if (!kind %in% c("categorical", "binary")) next
    if (identical(node_sim_design_mode(state, id), "intra")) return(TRUE)
  }
  FALSE
}

#' Ensure repeated-measures columns exist on nodes
#' @keywords internal
ensure_node_rm_columns <- function(state) {
  if (!"repeated_measures" %in% names(state$nodes)) {
    state$nodes$repeated_measures <- FALSE
  }
  miss_rm <- is.na(state$nodes$repeated_measures)
  if (any(miss_rm)) state$nodes$repeated_measures[miss_rm] <- FALSE
  if (!"cluster_column" %in% names(state$nodes)) {
    state$nodes$cluster_column <- NA_character_
  }
  state
}

#' Repeated-measures settings for a DV node
#' @return list(enabled, cluster)
#' @keywords internal
node_repeated_measures <- function(state, node_id) {
  state <- ensure_node_rm_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(list(enabled = FALSE, cluster = NA_character_))
  en <- isTRUE(state$nodes$repeated_measures[idx])
  cl <- state$nodes$cluster_column[idx]
  if (is.null(cl) || is.na(cl) || !nzchar(cl)) cl <- NA_character_
  list(enabled = en, cluster = as.character(cl))
}

#' Set repeated-measures flag and optional cluster column on a node
#' @keywords internal
set_node_repeated_measures <- function(state, node_id, enabled = FALSE,
                                       cluster_column = NA_character_) {
  state <- ensure_node_rm_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  state$nodes$repeated_measures[idx] <- isTRUE(enabled)
  if (isTRUE(enabled)) {
    if (is.null(cluster_column) || is.na(cluster_column) || !nzchar(cluster_column)) {
      state$nodes$cluster_column[idx] <- NA_character_
    } else {
      state$nodes$cluster_column[idx] <- as.character(cluster_column)
    }
  } else {
    state$nodes$cluster_column[idx] <- NA_character_
  }
  state
}

#' Ensure dv_transform column exists on nodes
#' @keywords internal
ensure_node_transform_columns <- function(state) {
  if (!"dv_transform" %in% names(state$nodes)) {
    state$nodes$dv_transform <- "none"
  }
  miss <- is.na(state$nodes$dv_transform) | !nzchar(state$nodes$dv_transform)
  if (any(miss)) state$nodes$dv_transform[miss] <- "none"
  allowed <- unname(dv_transform_choices())
  bad <- !state$nodes$dv_transform %in% allowed
  if (any(bad)) state$nodes$dv_transform[bad] <- "none"
  state
}

#' Outcome transform for a DV node (none | log | log10 | log1p | sqrt)
#' @keywords internal
node_dv_transform <- function(state, node_id) {
  state <- ensure_node_transform_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return("none")
  tr <- state$nodes$dv_transform[idx]
  if (is.na(tr) || !nzchar(tr) || !tr %in% unname(dv_transform_choices())) {
    return("none")
  }
  tr
}

#' Set outcome transform on a node
#' @keywords internal
set_node_dv_transform <- function(state, node_id, transform = "none") {
  state <- ensure_node_transform_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  allowed <- unname(dv_transform_choices())
  if (is.null(transform) || is.na(transform) || !nzchar(transform) ||
        !transform %in% allowed) {
    transform <- "none"
  }
  state$nodes$dv_transform[idx] <- transform
  state
}

#' Encode / decode factor level order for node storage
#' @keywords internal
encode_factor_levels <- function(levels) {
  levels <- as.character(levels %||% character())
  levels <- levels[!is.na(levels) & nzchar(levels)]
  if (!length(levels)) return(NA_character_)
  paste(levels, collapse = "\n")
}

#' @keywords internal
decode_factor_levels <- function(text) {
  if (is.null(text) || length(text) != 1L || is.na(text) || !nzchar(text)) {
    return(character())
  }
  parts <- strsplit(as.character(text), "\n", fixed = TRUE)[[1]]
  trimws(parts[nzchar(trimws(parts))])
}

#' Ensure factor coding columns exist on nodes
#' @keywords internal
ensure_node_factor_coding_columns <- function(state) {
  if (!"factor_base" %in% names(state$nodes)) state$nodes$factor_base <- NA_character_
  if (!"factor_ordered" %in% names(state$nodes)) state$nodes$factor_ordered <- FALSE
  if (!"factor_levels" %in% names(state$nodes)) state$nodes$factor_levels <- NA_character_
  miss_ord <- is.na(state$nodes$factor_ordered)
  if (any(miss_ord)) state$nodes$factor_ordered[miss_ord] <- FALSE
  state
}

#' Factor coding overrides for a node
#' @return list(base, ordered, levels)
#' @keywords internal
node_factor_coding <- function(state, node_id) {
  state <- ensure_node_factor_coding_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) {
    return(list(base = NA_character_, ordered = FALSE, levels = character()))
  }
  base <- state$nodes$factor_base[idx]
  if (is.null(base) || is.na(base) || !nzchar(base)) base <- NA_character_
  list(
    base = as.character(base),
    ordered = isTRUE(state$nodes$factor_ordered[idx]),
    levels = decode_factor_levels(state$nodes$factor_levels[idx])
  )
}

#' variable_coding using node overrides when present
#' @keywords internal
variable_coding_for_node <- function(state, node_id, data) {
  col <- tryCatch(lavaan_var_name(state, node_id), error = function(e) NA_character_)
  if (is.na(col) || is.null(data) || !col %in% names(data)) {
    return(list(
      kind = "continuous", base = NA_character_, levels = character(),
      all_levels = character(), ordered = FALSE
    ))
  }
  meta <- node_factor_coding(state, node_id)
  coding <- variable_coding(
    data[[col]],
    base = meta$base,
    levels_order = meta$levels
  )
  coding$ordered <- isTRUE(meta$ordered)
  coding
}

#' Drop split edges that conflict with a new base / level set
#' @keywords internal
prune_edges_for_factor_coding <- function(state, node_id, coding) {
  if (!nrow(state$edges)) return(state)
  base <- coding$base
  allowed <- coding$levels
  drop <- integer()
  for (i in seq_len(nrow(state$edges))) {
    if (!identical(state$edges$from[i], node_id)) next
    if (state$edges$type[i] %in% c("measurement", "definitional")) next
    lv <- if ("level" %in% names(state$edges)) state$edges$level[i] else NA_character_
    if (is.na(lv) || !nzchar(lv)) next
    if ((!is.na(base) && identical(lv, base)) || !lv %in% allowed) {
      drop <- c(drop, i)
    }
  }
  # Also drop definitional × wires with a now-invalid level
  for (i in seq_len(nrow(state$edges))) {
    if (!identical(state$edges$from[i], node_id)) next
    if (!identical(state$edges$type[i], "definitional")) next
    lv <- state$edges$level[i]
    if (is.na(lv) || !nzchar(lv)) next
    if ((!is.na(base) && identical(lv, base)) ||
          (length(allowed) && !lv %in% allowed && identical(coding$kind, "categorical"))) {
      drop <- c(drop, i)
    }
  }
  drop <- unique(drop)
  if (length(drop)) {
    state$edges <- state$edges[-drop, , drop = FALSE]
    state <- refresh_interaction_nodes(state)
  }
  state
}

#' Set factor base / ordered flag / level order on a node
#' @keywords internal
set_node_factor_coding <- function(state, node_id, base = NULL, ordered = NULL,
                                  levels = NULL, data = NULL) {
  state <- ensure_node_factor_coding_columns(state)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(state)
  if (is_interaction_node(state, node_id) || is_latent_node(state, node_id)) {
    return(state)
  }
  if (!is.null(base)) {
    base <- as.character(base)
    if (!nzchar(base)) base <- NA_character_
    state$nodes$factor_base[idx] <- base
  }
  if (!is.null(ordered)) {
    state$nodes$factor_ordered[idx] <- isTRUE(ordered)
  }
  if (!is.null(levels)) {
    state$nodes$factor_levels[idx] <- encode_factor_levels(levels)
  }
  if (!is.null(data)) {
    coding <- variable_coding_for_node(state, node_id, data)
    state <- prune_edges_for_factor_coding(state, node_id, coding)
  }
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
      lab <- paste0(lab, "·omnibus")
    } else if (!is.na(lv) && nzchar(lv)) {
      lab <- paste0(lab, "·", lv)
    }
    tl <- if ("to_level" %in% names(state$edges)) state$edges$to_level[i] else NA_character_
    if (!is.na(tl) && nzchar(tl)) lab <- paste0(lab, "\u2192", tl)
    lab
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
    edge_to_level <- if ("to_level" %in% names(state$edges)) state$edges$to_level[i] else NA_character_
    to_lhs <- dv_outcome_lhs(to_nm, edge_to_level)
    if (identical(to_lhs, to_nm)) {
      to_lhs <- dv_transform_lhs(to_nm, node_dv_transform(state, to))
    }
    is_omni_edge <- identical(node_cat_mode(state, from), "omnibus")
    if (is_interaction_node(state, from)) {
      fac_ix <- interaction_factors(state, from)
      is_omni_edge <- identical(node_cat_mode(state, fac_ix$a), "omnibus") ||
        identical(node_cat_mode(state, fac_ix$b), "omnibus")
    }

    if (etype == "measurement") {
      hit <- estimates$lhs == from_nm & estimates$op == "=~" & estimates$rhs == to_nm
    } else {
      hit <- estimates$lhs == to_lhs & estimates$op == "~" & estimates$rhs == from_nm
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
      if (!is.na(edge_to_level) && nzchar(edge_to_level)) {
        prefix <- paste0(prefix, "\u2192", edge_to_level)
      }
      # Measurement canvas: signed standardized loading (fallback: raw λ)
      if (identical(etype, "measurement")) {
        show <- if (is.finite(est_std)) est_std else est
        if (is.finite(show)) {
          state$edges$label[i] <- sprintf("%s %.2f", prefix, show)
        } else {
          state$edges$label[i] <- prefix
        }
        next
      }
      # Append polynomial terms for this predictor on the same DV
      poly_bits <- character()
      if (!is_omni_edge && "level" %in% names(estimates)) {
        poly_hit <- estimates$lhs == to_lhs & estimates$op == "~" &
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
  state <- ensure_node_kind_columns(state)
  if (is_interaction_node(state, node_id)) return(FALSE)
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) return(FALSE)
  if (identical(state$nodes$node_kind[idx], "observed")) return(TRUE)
  col <- state$nodes$observed_column[idx]
  !is.na(col) && nzchar(col)
}
