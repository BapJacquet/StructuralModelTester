#' AIC/BIC search over non-fixed edges
#'
#' Only **fixed** edges are always included. Confirmatory, exploratory, and
#' measurement edges are searched over (all subsets, including the empty /
#' null set). The best model by AIC or BIC is kept.
#' Observed-only path models sum equation-wise lm/glm AIC or BIC.
#' Latent models use lavaan IC when available.
#'
#' @param state Model state.
#' @param data Data frame.
#' @param criterion Either "aic" or "bic".
#' @param max_exploratory Maximum free (non-fixed) edges for exhaustive search (default 12).
#' @return A list with updated `state`, `fit`, `estimates`, `fit_measures`, `syntax`, `comparison`.
#' @export
fit_aic_bic_search <- function(state, data, criterion = c("aic", "bic"), max_exploratory = 12L) {
  criterion <- match.arg(criterion)

  # Definitional wires into × are never searched or estimated
  always <- analyzable_edge_ids(state, state$edges$id[state$edges$type == "fixed"])
  free <- analyzable_edge_ids(
    state,
    state$edges$id[state$edges$type %in% c("confirmatory", "exploratory", "measurement")]
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
  if (!is.null(comparison) && nrow(comparison)) {
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

  if (is.null(best)) {
    stop("All candidate models failed to fit. Check the model and data.", call. = FALSE)
  }

  all_edge_ids <- unique(c(always, free))
  wsums <- edge_weight_sums(comparison, always_ids = always, free_ids = free)
  averaged_edges <- full_average_edge_coefs(comparison, coef_by_model, all_edge_ids)
  averaged_intercepts <- full_average_intercepts(comparison, coef_by_model)

  included <- c(always, best_subset)
  def_ids <- state$edges$id[
    state$edges$type == "definitional" |
      vapply(state$edges$to, function(id) is_interaction_node(state, id), logical(1))
  ]
  included_display <- unique(c(included, def_ids))
  state_best <- apply_estimates_to_edges(state, best$estimates, included_edge_ids = included_display)
  state_best <- apply_r_squared_to_nodes(state_best, best$r_squared)
  state_best <- apply_weight_labels_to_edges(state_best, wsums)
  averaged_r2 <- full_average_r_squared(comparison, coef_by_model)
  state_avg <- apply_averaged_estimates_to_state(state, averaged_edges, wsums)
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
    criterion = criterion,
    best_exploratory = best_subset,
    always = always,
    free = free,
    edge_weights = wsums,
    engine = best$engine,
    model_averaged = FALSE
  )
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
    # Keep definitional × wiring labels unchanged
    if (identical(state$edges$type[i], "definitional") ||
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
