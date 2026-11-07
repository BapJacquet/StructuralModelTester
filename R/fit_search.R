#' AIC/BIC search over non-fixed edges
#'
#' Only **fixed** (+ measurement) edges are always included. Confirmatory and
#' exploratory edges are searched over (all subsets, including the empty /
#' null set). The best model by AIC or BIC is kept.
#'
#' After search, the result always summarizes **best**, **confirmatory**
#' (best ∪ all confirmatory paths), **confirmatory only** (all confirmatory,
#' no exploratory), **null** (no free paths), and **full** (all free paths).
#' These are tagged in `comparison` and listed in `best_vs_confirmatory`.
#'
#' Observed-only path models sum equation-wise lm/glm AIC or BIC.
#' Latents use two-step factor scores then path IC (measurement links always kept).
#'
#' @param state Model state.
#' @param data Data frame.
#' @param criterion Either "aic" or "bic".
#' @param max_exploratory Maximum free (non-fixed) edges for exhaustive search (default 12).
#' @param uncertainty Post-selection uncertainty on the **best** model only.
#' @param uncertainty_draws Bootstrap/PV draws (defaults by method).
#' @param uncertainty_progress Optional function(i, total) callback.
#' @return A list with updated `state`, `fit`, `estimates`, `fit_measures`, `syntax`, `comparison`.
#' @export
fit_aic_bic_search <- function(state, data, criterion = c("aic", "bic"), max_exploratory = 12L,
                               uncertainty = c("none", "bootstrap", "plausible"),
                               uncertainty_draws = NULL,
                               uncertainty_progress = NULL,
                               min_weight = 0,
                               mark_low_weight = TRUE) {
  criterion <- match.arg(criterion)
  uncertainty <- match.arg(uncertainty)
  min_weight <- as.numeric(min_weight)
  if (!is.finite(min_weight) || min_weight < 0) min_weight <- 0
  if (min_weight > 1) min_weight <- 1
  mark_low_weight <- isTRUE(mark_low_weight)

  # Definitional wires into × are never searched or estimated.
  # Measurement links define factor scores (two-step) and are always kept — not searched.
  always <- analyzable_edge_ids(state, state$edges$id[state$edges$type == "fixed"])
  meas_ids <- analyzable_edge_ids(
    state,
    state$edges$id[state$edges$type == "measurement"]
  )
  always <- unique(c(always, meas_ids))
  conf_free <- analyzable_edge_ids(
    state,
    state$edges$id[state$edges$type == "confirmatory"]
  )
  free <- analyzable_edge_ids(
    state,
    state$edges$id[state$edges$type %in% c("confirmatory", "exploratory")]
  )

  if (!length(always) && !length(free)) {
    stop("No edges to fit.", call. = FALSE)
  }

  if (length(free) > max_exploratory) {
    stop(
      sprintf(
        "Too many free links (%d). Maximum for exhaustive search is %d. Mark more links as fixed or remove some.",
        length(free), max_exploratory
      ),
      call. = FALSE
    )
  }

  n <- length(free)
  if (n == 0) {
    subsets <- list(character())
  } else {
    subsets <- unlist(lapply(0:n, function(k) {
      if (k == 0) return(list(character()))
      utils::combn(free, k, simplify = FALSE)
    }), recursive = FALSE)
  }

  rows <- list()
  coef_by_model <- list()
  best <- NULL
  best_score <- Inf
  best_subset <- character()

  for (i in seq_along(subsets)) {
    subset <- subsets[[i]]
    ids <- c(always, subset)
    res <- tryCatch(
      {
        fitted <- fit_structural_model(state, data, include_edge_ids = ids)
        fm <- fitted$fit_measures
        aic <- fm$value[match("aic", fm$measure)]
        bic <- fm$value[match("bic", fm$measure)]
        df <- fm$value[match("df", fm$measure)]
        if (length(aic) != 1L || is.na(aic) || length(bic) != 1L || is.na(bic)) {
          stop(
            "AIC/BIC are not available for this model engine ",
            "(e.g. categorical lavaan / WLSMV). Use continuous DVs or an observed-only path model.",
            call. = FALSE
          )
        }
        list(
          ok = TRUE,
          fitted = fitted,
          aic = as.numeric(aic),
          bic = as.numeric(bic),
          df = if (length(df) == 1L && !is.na(df)) as.numeric(df) else NA_real_
        )
      },
      error = function(e) list(ok = FALSE, error = e$message)
    )

    if (!isTRUE(res$ok)) {
      rows[[i]] <- data.frame(
        model = i,
        n_free = length(subset),
        edges = paste(subset, collapse = ","),
        aic = NA_real_,
        bic = NA_real_,
        df = NA_real_,
        status = paste("failed:", res$error),
        stringsAsFactors = FALSE
      )
      next
    }

    coef_by_model[[as.character(i)]] <- list(
      edges = estimates_by_edge_id(state, res$fitted$estimates, ids),
      intercepts = intercepts_from_fits(res$fitted$fit),
      r_squared = res$fitted$r_squared %||% list()
    )

    score <- res[[criterion]]
    rows[[i]] <- data.frame(
      model = i,
      n_free = length(subset),
      edges = paste(subset, collapse = ","),
      aic = res$aic,
      bic = res$bic,
      df = res$df,
      status = "ok",
      stringsAsFactors = FALSE
    )

    if (!is.na(score) && score < best_score) {
      best_score <- score
      best <- res$fitted
      best_subset <- subset
    }
  }

  comparison <- do.call(rbind, rows)
  if (is.null(best)) {
    stop("All candidate models failed to fit. Check the model and data.", call. = FALSE)
  }

  # Preserve `free` order so edges keys match combn output
  conf_subset <- free[free %in% unique(c(best_subset, conf_free))]
  conf_only_subset <- free[free %in% conf_free]
  null_subset <- character()
  full_subset <- free
  best_key <- paste(best_subset, collapse = ",")
  conf_key <- paste(conf_subset, collapse = ",")
  conf_only_key <- paste(conf_only_subset, collapse = ",")
  null_key <- paste(null_subset, collapse = ",")
  full_key <- paste(full_subset, collapse = ",")

  if (!is.null(comparison) && nrow(comparison)) {
    edge_keys <- ifelse(is.na(comparison$edges), "", comparison$edges)
    comparison$role <- ""
    ok_rows <- which(comparison$status == "ok")
    for (i in ok_rows) {
      comparison$role[i] <- search_model_role(
        edge_keys[i],
        best_key = best_key,
        conf_key = conf_key,
        conf_only_key = conf_only_key,
        null_key = null_key,
        full_key = full_key
      )
    }

    comparison <- comparison[order(comparison[[criterion]], na.last = TRUE), , drop = FALSE]
    best_ic <- min(comparison[[criterion]], na.rm = TRUE)
    comparison$delta <- comparison[[criterion]] - best_ic
    ok <- is.finite(comparison$delta)
    w <- rep(NA_real_, nrow(comparison))
    if (any(ok)) {
      rel <- exp(-0.5 * comparison$delta[ok])
      w[ok] <- rel / sum(rel)
    }
    comparison$weight <- w
    rownames(comparison) <- NULL
  }

  best_vs_confirmatory <- build_best_vs_confirmatory(
    comparison,
    criterion = criterion,
    best_key = best_key,
    conf_key = conf_key,
    conf_only_key = conf_only_key,
    null_key = null_key,
    full_key = full_key
  )

  included <- c(always, best_subset)
  # Uncertainty only on the selected (best) model — not on every candidate
  best <- apply_postselection_uncertainty(
    state, data, included, best,
    method = uncertainty,
    draws = uncertainty_draws,
    progress = uncertainty_progress
  )

  all_edge_ids <- unique(c(always, free))
  wsums <- edge_weight_sums(comparison, always_ids = always, free_ids = free)
  averaged_edges <- full_average_edge_coefs(comparison, coef_by_model, all_edge_ids)
  averaged_intercepts <- full_average_intercepts(comparison, coef_by_model)

  def_ids <- state$edges$id[
    state$edges$type == "definitional" |
      vapply(state$edges$to, function(id) is_interaction_node(state, id), logical(1))
  ]
  included_display <- unique(c(included, def_ids))
  state_best <- apply_estimates_to_edges(state, best$estimates, included_edge_ids = included_display)
  state_best <- apply_r_squared_to_nodes(state_best, best$r_squared)
  state_best <- apply_weight_labels_to_edges(state_best, wsums)
  state_best <- mark_edges_below_weight(
    state_best, wsums,
    min_weight = min_weight,
    mark = mark_low_weight
  )
  averaged_r2 <- full_average_r_squared(comparison, coef_by_model)
  state_avg <- apply_averaged_estimates_to_state(
    state, averaged_edges, wsums,
    min_weight = if (mark_low_weight) min_weight else 0
  )
  state_avg <- apply_r_squared_to_nodes(state_avg, averaged_r2)

  list(
    state = state_best,
    state_averaged = state_avg,
    fit = best$fit,
    estimates = best$estimates,
    best_estimates = best$estimates,
    r_squared = best$r_squared,
    averaged_r_squared = averaged_r2,
    averaged_edges = averaged_edges,
    averaged_intercepts = averaged_intercepts,
    coef_by_model = coef_by_model,
    fit_measures = best$fit_measures,
    syntax = best$syntax,
    comparison = comparison,
    best_vs_confirmatory = best_vs_confirmatory,
    criterion = criterion,
    best_exploratory = best_subset,
    confirmatory_subset = conf_subset,
    confirmatory_only_subset = conf_only_subset,
    confirmatory_edges = conf_free,
    always = always,
    free = free,
    edge_weights = wsums,
    min_weight = min_weight,
    mark_low_weight = mark_low_weight,
    engine = best$engine,
    model_averaged = FALSE,
    uncertainty = best$uncertainty %||% list(method = "none")
  )
}

#' Role label(s) for a free-edge set key in AIC/BIC search
#' @keywords internal
search_model_role <- function(edge_key, best_key, conf_key, conf_only_key,
                              null_key = "", full_key = "") {
  labs <- character()
  if (identical(edge_key, best_key)) labs <- c(labs, "best")
  if (identical(edge_key, conf_key)) labs <- c(labs, "confirmatory")
  if (identical(edge_key, conf_only_key)) labs <- c(labs, "confirmatory only")
  if (identical(edge_key, null_key)) labs <- c(labs, "null")
  if (identical(edge_key, full_key)) labs <- c(labs, "full")
  if (!length(labs)) return("")
  paste(labs, collapse = " = ")
}

#' Focal model summary from a search comparison table
#'
#' Always includes best, confirmatory, confirmatory only, null, and full
#' (collapsing identical edge sets into a combined role label).
#' @keywords internal
build_best_vs_confirmatory <- function(comparison, criterion = "bic",
                                       best_key = "", conf_key = "",
                                       conf_only_key = "",
                                       null_key = "", full_key = "") {
  empty <- data.frame(
    role = character(),
    edges = character(),
    aic = numeric(),
    bic = numeric(),
    df = numeric(),
    delta = numeric(),
    weight = numeric(),
    status = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(comparison) || !nrow(comparison)) return(empty)

  # Stable focal order; unique() drops duplicates when models coincide
  wanted <- unique(c(best_key, conf_key, conf_only_key, null_key, full_key))
  edge_keys <- ifelse(is.na(comparison$edges), "", comparison$edges)
  rows <- list()
  for (k in wanted) {
    hit <- comparison[comparison$status == "ok" & edge_keys == k, , drop = FALSE]
    if (!nrow(hit)) next
    row <- hit[1, , drop = FALSE]
    row$role <- search_model_role(
      k, best_key, conf_key, conf_only_key, null_key, full_key
    )
    rows[[length(rows) + 1L]] <- row
  }
  if (!length(rows)) return(empty)
  hit <- do.call(rbind, rows)

  # Delta relative to the best model in this summary
  best_row <- hit[vapply(hit$role, function(r) grepl("\\bbest\\b", r), logical(1)), , drop = FALSE]
  ref <- if (nrow(best_row)) {
    best_row[[criterion]][1]
  } else {
    min(hit[[criterion]], na.rm = TRUE)
  }
  hit$delta <- hit[[criterion]] - ref
  rownames(hit) <- NULL
  hit
}

#' Sum of Akaike/BIC weights over models that include each edge
#' @keywords internal
edge_weight_sums <- function(comparison, always_ids = character(), free_ids = character(),
                             explor_ids = NULL) {
  # explor_ids kept as deprecated alias
  if (!is.null(explor_ids) && !length(free_ids)) free_ids <- explor_ids
  ids <- unique(c(always_ids, free_ids))
  sums <- setNames(rep(0, length(ids)), ids)
  if (is.null(comparison) || !nrow(comparison) || !length(ids)) return(sums)

  for (i in seq_len(nrow(comparison))) {
    w <- comparison$weight[i]
    if (!is.finite(w)) next
    free_in <- comparison$edges[i]
    subset_ids <- if (is.na(free_in) || !nzchar(free_in)) {
      character()
    } else {
      strsplit(free_in, ",", fixed = TRUE)[[1]]
    }
    for (eid in c(always_ids, subset_ids)) {
      if (eid %in% names(sums)) sums[[eid]] <- sums[[eid]] + w
    }
  }
  sums
}

#' Replace edge labels with estimate + Σw (model-comparison display)
#' @keywords internal
apply_weight_labels_to_edges <- function(state, weight_sums) {
  if (!nrow(state$edges)) return(state)
  for (i in seq_len(nrow(state$edges))) {
    eid <- state$edges$id[i]
    # Keep definitional × wiring and measurement (std loadings) labels unchanged
    if (identical(state$edges$type[i], "definitional") ||
          identical(state$edges$type[i], "measurement") ||
          is_interaction_node(state, state$edges$to[i])) {
      next
    }
    ws <- if (eid %in% names(weight_sums)) as.numeric(weight_sums[[eid]]) else 0
    est <- state$edges$estimate[i]
    pv <- state$edges$pvalue[i]
    short <- type_short_label(state$edges$type[i])
    lab0 <- state$edges$label[i]
    lv <- if ("level" %in% names(state$edges)) state$edges$level[i] else NA_character_
    if (!is.na(lv) && nzchar(lv)) short <- paste0(short, "·", lv)
    is_cat <- is.character(lab0) && grepl("^cat\\.", lab0)
    if (!is.na(est)) {
      state$edges$label[i] <- sprintf("%.2f (Σw=%.2f)", est, ws)
    } else if (is_cat && !is.na(pv)) {
      state$edges$label[i] <- sprintf("%s · Σw=%.2f", lab0, ws)
    } else if (identical(state$edges$status[i], "excluded")) {
      state$edges$label[i] <- sprintf("%s Σw=%.2f", short, ws)
    } else {
      state$edges$label[i] <- sprintf("%s (Σw=%.2f)", short, ws)
    }
  }
  state
}

#' Mark confirmatory/exploratory edges with Σw below a threshold as excluded
#'
#' Fixed / measurement / definitional wires are never rejected by weight.
#' @keywords internal
mark_edges_below_weight <- function(state, weight_sums, min_weight = 0,
                                    mark = TRUE) {
  if (!isTRUE(mark) || is.null(state) || !nrow(state$edges)) return(state)
  min_weight <- as.numeric(min_weight)
  if (!is.finite(min_weight) || min_weight <= 0) return(state)
  if (min_weight > 1) min_weight <- 1
  if (is.null(weight_sums)) weight_sums <- list()

  for (i in seq_len(nrow(state$edges))) {
    etype <- state$edges$type[[i]]
    if (!etype %in% c("confirmatory", "exploratory")) next
    if (is_interaction_node(state, state$edges$to[[i]])) next
    eid <- state$edges$id[[i]]
    ws <- if (eid %in% names(weight_sums)) as.numeric(weight_sums[[eid]]) else 0
    if (!is.finite(ws)) ws <- 0
    if (ws >= min_weight) next

    state$edges$status[i] <- "excluded"
    est_val <- state$edges$estimate[i]
    short <- type_short_label(etype)
    lv <- if ("level" %in% names(state$edges)) state$edges$level[i] else NA_character_
    if (!is.na(lv) && nzchar(lv)) short <- paste0(short, "·", lv)
    state$edges$label[i] <- if (is.finite(est_val)) {
      sprintf("%s %.2f Σw=%.2f ✕", short, est_val, ws)
    } else {
      sprintf("%s Σw=%.2f ✕", short, ws)
    }
  }
  state
}
