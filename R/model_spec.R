#' Build lavaan model syntax from an internal model state
#'
#' @param state Model state list (nodes / edges).
#' @param include_edge_ids Character vector of edge ids to include.
#'   If NULL, includes all non-exploratory edges plus exploratory ones
#'   (caller decides filtering).
#' @return A character string of lavaan syntax.
#' @export
build_lavaan_syntax <- function(state, include_edge_ids = NULL) {
  edges <- state$edges
  if (!is.null(include_edge_ids)) {
    edges <- edges[edges$id %in% include_edge_ids, , drop = FALSE]
  }
  if (!nrow(edges)) {
    stop("No edges selected for the model.", call. = FALSE)
  }

  validate_model_state(state, edges)

  # Measurement: from = latent, to = indicator
  meas <- edges[edges$type == "measurement", , drop = FALSE]
  struct <- edges[edges$type != "measurement", , drop = FALSE]

  lines <- character()

  if (nrow(meas)) {
    latents <- unique(meas$from)
    for (lat in latents) {
      inds <- meas$to[meas$from == lat]
      ind_names <- vapply(inds, function(id) lavaan_var_name(state, id), character(1))
      lat_name <- lavaan_var_name(state, lat)
      lines <- c(lines, paste0(lat_name, " =~ ", paste(ind_names, collapse = " + ")))
    }
  }

  if (nrow(struct)) {
    # Skip definitional wires into interaction operators (product is precomputed)
    is_def <- vapply(struct$to, function(id) is_interaction_node(state, id), logical(1))
    struct_reg <- struct[!is_def, , drop = FALSE]
    if (nrow(struct_reg)) {
      state <- ensure_edge_to_port(state)
      dvs <- unique(struct_reg$to)
      for (dv in dvs) {
        preds <- struct_reg[struct_reg$to == dv, , drop = FALSE]
        rhs_parts <- character()
        for (j in seq_len(nrow(preds))) {
          xn <- lavaan_var_name(state, preds$from[j])
          rk <- if ("rel_kind" %in% names(preds)) preds$rel_kind[j] else "linear"
          if ("id" %in% names(preds)) {
            eidx <- match(preds$id[j], state$edges$id)
            if (!is.na(eidx) && "rel_kind" %in% names(state$edges)) {
              stored <- state$edges$rel_kind[eidx]
              if (!is.na(stored) && nzchar(stored)) rk <- stored
            }
          }
          deg <- rel_kind_degree(rk)
          rhs_parts <- c(rhs_parts, xn)
          if (deg >= 2L) {
            for (d in 2:deg) rhs_parts <- c(rhs_parts, poly_col_name(xn, d))
          }
        }
        lines <- c(
          lines,
          paste0(lavaan_var_name(state, dv), " ~ ", paste(unique(rhs_parts), collapse = " + "))
        )
      }
    }
  }

  paste(lines, collapse = "\n")
}

lavaan_var_name <- function(state, node_id) {
  idx <- match(node_id, state$nodes$id)
  if (is.na(idx)) stop("Unknown node: ", node_id, call. = FALSE)
  if (is_interaction_node(state, node_id)) {
    return(sanitize_id(state$nodes$id[idx]))
  }
  col <- state$nodes$observed_column[idx]
  if (!is.na(col) && nzchar(col)) return(col)
  # latent: use node id (sanitized label)
  sanitize_id(state$nodes$id[idx])
}

#' Numeric coding of a continuous/binary factor for a product term
#' @keywords internal
factor_numeric_for_product <- function(x, center = TRUE) {
  kind <- classify_variable(x)
  if (identical(kind, "categorical")) {
    stop(
      "Internal error: categorical factors must be expanded via dummies, ",
      "not factor_numeric_for_product().",
      call. = FALSE
    )
  }
  if (identical(kind, "binary")) {
    return(as.numeric(as_binary01(x)))
  }
  x <- as.numeric(x)
  if (isTRUE(center)) x <- x - mean(x, na.rm = TRUE)
  x
}

#' True if either interaction factor is multi-level categorical
#' @keywords internal
interaction_involves_categorical <- function(state, data, ix_id) {
  fac <- interaction_factors(state, ix_id)
  for (id in c(fac$a, fac$b)) {
    if (is.na(id) || !nzchar(id)) next
    col <- state$nodes$observed_column[match(id, state$nodes$id)]
    if (is.na(col) || !nzchar(col) || !col %in% names(data)) next
    if (identical(classify_variable(data[[col]]), "categorical")) return(TRUE)
  }
  FALSE
}

#' Validate model before fitting
#' @keywords internal
validate_model_state <- function(state, edges = state$edges) {
  if (!nrow(state$nodes)) stop("Add at least one node.", call. = FALSE)
  if (!nrow(edges)) stop("Add at least one link.", call. = FALSE)

  latent_ids <- state$nodes$id[vapply(state$nodes$id, function(id) {
    is_latent_node(state, id)
  }, logical(1))]
  meas <- edges[edges$type == "measurement", , drop = FALSE]

  for (lat in latent_ids) {
    # Latent must appear as measurement LHS or be unused (warn if used structurally without indicators)
    used <- any(edges$from == lat | edges$to == lat)
    if (!used) next
    n_ind <- sum(meas$from == lat)
    if (n_ind < 1) {
      lab <- state$nodes$label[match(lat, state$nodes$id)]
      stop(
        sprintf(
          "Latent node '%s' needs at least one measurement link to an observed indicator.",
          lab
        ),
        call. = FALSE
      )
    }
  }

  # Interaction operators used as predictors must have both inputs wired
  struct_chk <- edges[edges$type != "measurement", , drop = FALSE]
  if (nrow(struct_chk)) {
    ix_preds <- unique(struct_chk$from[vapply(struct_chk$from, function(id) {
      is_interaction_node(state, id)
    }, logical(1))])
    for (ix in ix_preds) {
      fac <- interaction_factors(state, ix)
      if (is.na(fac$a) || is.na(fac$b) || !nzchar(fac$a) || !nzchar(fac$b)) {
        stop(
          sprintf(
            "Interaction '%s' needs two predictors wired into its input ports.",
            state$nodes$label[match(ix, state$nodes$id)]
          ),
          call. = FALSE
        )
      }
    }
  }

  # Measurement edges must go latent -> observed
  if (nrow(meas)) {
    for (i in seq_len(nrow(meas))) {
      if (is_observed_node(state, meas$from[i])) {
        stop("Measurement links must start from a latent (unmapped) node.", call. = FALSE)
      }
      if (!is_observed_node(state, meas$to[i])) {
        stop("Measurement links must end at an observed (mapped) node.", call. = FALSE)
      }
    }
  }

  # Observed nodes used in structural model need column mapping
  struct <- edges[edges$type != "measurement", , drop = FALSE]
  used_ids <- unique(c(struct$from, struct$to))
  for (id in used_ids) {
    if (!is_observed_node(state, id) && !id %in% meas$from) {
      # latent endogenous/exogenous OK if it has measurement model
      if (!id %in% latent_ids) next
      if (!id %in% meas$from) {
        lab <- state$nodes$label[match(id, state$nodes$id)]
        stop(sprintf("Node '%s' is latent but has no indicators.", lab), call. = FALSE)
      }
    }
  }

  invisible
}

#' Prepare analysis data frame (rename columns to lavaan names where needed)
#' @keywords internal
prepare_analysis_data <- function(state, data) {
  if (is.null(data) || !nrow(data)) {
    stop("Load a data file before running the analysis.", call. = FALSE)
  }
  state <- ensure_node_kind_columns(state)
  mapped <- state$nodes[!is.na(state$nodes$observed_column) & nzchar(state$nodes$observed_column), , drop = FALSE]
  if (!nrow(mapped)) {
    stop("Map at least one data column to a node.", call. = FALSE)
  }
  missing_cols <- setdiff(mapped$observed_column, names(data))
  if (length(missing_cols)) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  out <- data[, mapped$observed_column, drop = FALSE]
  # Drop rows with any NA in used columns (listwise)
  out <- out[stats::complete.cases(out), , drop = FALSE]
  if (!nrow(out)) stop("No complete cases after listwise deletion.", call. = FALSE)

  # Materialize centered products for continuous/binary × continuous/binary only.
  # Categorical interactions are expanded into dummy products in build_path_terms.
  state <- refresh_interaction_nodes(state)
  ix <- which(state$nodes$node_kind == "interaction")
  for (i in ix) {
    ix_id <- state$nodes$id[i]
    fac <- interaction_factors(state, ix_id)
    fa <- fac$a
    fb <- fac$b
    if (is.na(fa) || is.na(fb) || !nzchar(fa) || !nzchar(fb)) next
    col_a <- state$nodes$observed_column[match(fa, state$nodes$id)]
    col_b <- state$nodes$observed_column[match(fb, state$nodes$id)]
    if (is.na(col_a) || !nzchar(col_a) || is.na(col_b) || !nzchar(col_b)) {
      stop(
        sprintf(
          "Interaction '%s' needs both factors mapped to data columns.",
          state$nodes$label[i]
        ),
        call. = FALSE
      )
    }
    if (!col_a %in% names(out) || !col_b %in% names(out)) {
      stop(
        sprintf(
          "Interaction '%s' factors are not in the analysis data.",
          state$nodes$label[i]
        ),
        call. = FALSE
      )
    }
    if (interaction_involves_categorical(state, out, ix_id)) next
    pname <- lavaan_var_name(state, ix_id)
    out[[pname]] <- factor_numeric_for_product(out[[col_a]], center = TRUE) *
      factor_numeric_for_product(out[[col_b]], center = TRUE)
  }

  # Polynomial powers for continuous predictors / interaction products
  state <- ensure_edge_to_port(state)
  struct <- state$edges[
    state$edges$type != "measurement" & state$edges$type != "definitional",
    ,
    drop = FALSE
  ]
  if (nrow(struct)) {
    for (i in seq_len(nrow(struct))) {
      if (is_interaction_node(state, struct$to[i])) next
      from_id <- struct$from[i]
      xn <- lavaan_var_name(state, from_id)
      if (!xn %in% names(out)) next
      # Skip categorical / binary (poly only for continuous-like columns)
      if (!is_interaction_node(state, from_id)) {
        kind <- classify_variable(out[[xn]])
        if (!identical(kind, "continuous")) next
      }
      rk <- struct$rel_kind[i]
      deg <- rel_kind_degree(rk)
      if (deg < 2L) next
      expanded <- expand_continuous_predictor(out, xn, out[[xn]], max_degree = deg)
      out <- expanded$data
    }
  }
  out
}
