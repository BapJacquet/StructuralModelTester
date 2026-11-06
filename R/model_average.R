#' Match structural estimate rows to edge ids
#' @keywords internal
estimates_by_edge_id <- function(state, estimates, included_ids) {
  out <- list()
  if (is.null(estimates) || !nrow(estimates) || !length(included_ids)) return(out)
  for (eid in included_ids) {
    idx <- match(eid, state$edges$id)
    if (is.na(idx)) next
    if (identical(state$edges$type[idx], "definitional") ||
          is_interaction_node(state, state$edges$to[idx])) {
      next
    }
    from_nm <- lavaan_var_name(state, state$edges$from[idx])
    to_nm <- lavaan_var_name(state, state$edges$to[idx])
    etype <- state$edges$type[idx]
    lv <- if ("level" %in% names(state$edges)) state$edges$level[idx] else NA_character_
    from_id <- state$edges$from[idx]
    cm <- node_cat_mode(state, from_id)
    if (is_interaction_node(state, from_id)) {
      fac_ix <- interaction_factors(state, from_id)
      if (identical(node_cat_mode(state, fac_ix$a), "omnibus") ||
            identical(node_cat_mode(state, fac_ix$b), "omnibus")) {
        cm <- "omnibus"
      }
    }

    if (identical(etype, "measurement")) {
      hit <- estimates$lhs == from_nm & estimates$op == "=~" & estimates$rhs == to_nm
    } else {
      hit <- estimates$lhs == to_nm & estimates$op == "~" & estimates$rhs == from_nm
      if ("kind" %in% names(estimates) && identical(cm, "omnibus")) {
        om <- hit & estimates$kind == "omnibus"
        if (any(om)) hit <- om
      } else if ("level" %in% names(estimates)) {
        if (!is.na(lv) && nzchar(lv)) {
          hit <- hit & !is.na(estimates$level) & estimates$level == lv
          if ("kind" %in% names(estimates)) {
            ef <- hit & estimates$kind == "effect"
            if (any(ef)) hit <- ef
          }
        } else {
          hit <- hit & (is.na(estimates$level) | !nzchar(as.character(estimates$level)) |
            !vapply(estimates$level, is_poly_level, logical(1)))
          if ("kind" %in% names(estimates)) {
            ef <- hit & estimates$kind == "effect"
            if (any(ef)) hit <- ef
          }
        }
      }
    }
    if (!any(hit)) next
    row <- estimates[which(hit)[1], ]
    out[[eid]] <- list(
      est = as.numeric(row$est[1]),
      se = as.numeric(row$se[1]),
      est_std = if ("est_std" %in% names(row)) as.numeric(row$est_std[1]) else NA_real_
    )
  }
  out
}

#' Intercept estimates from path lm/glm fits
#' @keywords internal
intercepts_from_fits <- function(fits) {
  out <- list()
  if (is.null(fits) || !is.list(fits) || inherits(fits, "lavaan")) return(out)
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    if (is.null(fit)) next
    cf <- stats::coef(fit)
    if (!("(Intercept)" %in% names(cf))) next
    se <- tryCatch(sqrt(diag(stats::vcov(fit)))[["(Intercept)"]], error = function(e) NA_real_)
    yname <- tryCatch(as.character(stats::formula(fit)[[2]]), error = function(e) nm)
    out[[yname]] <- list(est = unname(cf[["(Intercept)"]]), se = unname(se))
  }
  out
}

#' Full model averaging for edge coefficients (absent → 0)
#'
#' @return data.frame with id, estimate, se, weight_sum
#' @keywords internal
full_average_edge_coefs <- function(comparison, coef_by_model, edge_ids) {
  empty <- data.frame(
    id = character(), estimate = numeric(), se = numeric(),
    weight_sum = numeric(), stringsAsFactors = FALSE
  )
  if (!length(edge_ids) || is.null(comparison) || !nrow(comparison)) return(empty)

  ok <- comparison$status == "ok" & is.finite(comparison$weight)
  comp <- comparison[ok, , drop = FALSE]
  if (!nrow(comp)) return(empty)

  rows <- lapply(edge_ids, function(eid) {
    ests <- numeric(nrow(comp))
    ses <- numeric(nrow(comp))
    stds <- numeric(nrow(comp))
    present <- logical(nrow(comp))
    for (i in seq_len(nrow(comp))) {
      mid <- as.character(comp$model[i])
      slot <- coef_by_model[[mid]]
      if (!is.null(slot) && !is.null(slot$edges[[eid]])) {
        ests[i] <- slot$edges[[eid]]$est
        ses[i] <- slot$edges[[eid]]$se
        stds[i] <- if (is.finite(slot$edges[[eid]]$est_std)) slot$edges[[eid]]$est_std else 0
        present[i] <- TRUE
      } else {
        ests[i] <- 0
        ses[i] <- 0
        stds[i] <- 0
        present[i] <- FALSE
      }
    }
    w <- comp$weight
    # Full average: missing parameters contribute 0
    beta_bar <- sum(w * ests)
    std_bar <- sum(w * stds)
    # Unconditional SE (Burnham & Anderson)
    se_bar <- sum(w * sqrt(ses^2 + (ests - beta_bar)^2))
    data.frame(
      id = eid,
      estimate = beta_bar,
      se = se_bar,
      estimate_std = std_bar,
      weight_sum = sum(w[present]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Full model-average of per-DV R² (missing -> 0)
#' @keywords internal
full_average_r_squared <- function(comparison, coef_by_model) {
  out <- list()
  if (is.null(comparison) || !nrow(comparison)) return(out)
  ok <- comparison$status == "ok" & is.finite(comparison$weight)
  if (!any(ok)) return(out)
  comp <- comparison[ok, , drop = FALSE]
  node_ids <- unique(unlist(lapply(coef_by_model, function(slot) names(slot$r_squared %||% list()))))
  if (!length(node_ids)) return(out)

  for (nid in node_ids) {
    vals <- numeric(nrow(comp))
    kind <- NA_character_
    for (i in seq_len(nrow(comp))) {
      mid <- as.character(comp$model[i])
      slot <- coef_by_model[[mid]]
      r2 <- if (!is.null(slot)) slot$r_squared[[nid]] else NULL
      if (is.null(r2)) {
        vals[i] <- 0
      } else {
        vals[i] <- if (is.list(r2)) as.numeric(r2$value) else as.numeric(r2)
        if (is.na(kind) && is.list(r2) && !is.null(r2$kind)) kind <- r2$kind
      }
    }
    bar <- sum(comp$weight * vals)
    if (!is.finite(bar)) next
    out[[nid]] <- list(value = bar, kind = if (is.na(kind)) "r2" else kind)
  }
  out
}

#' Apply model-averaged edge estimates onto state for canvas / tables
#' @keywords internal
apply_averaged_estimates_to_state <- function(state, avg_edges, weight_sums) {
  state <- clear_fit_results(state)
  if (!nrow(state$edges) || is.null(avg_edges) || !nrow(avg_edges)) return(state)

  for (i in seq_len(nrow(state$edges))) {
    eid <- state$edges$id[i]
    if (is_interaction_node(state, state$edges$to[i])) {
      tp <- if ("to_port" %in% names(state$edges)) state$edges$to_port[i] else NA_character_
      state$edges$status[i] <- "included"
      state$edges$label[i] <- paste0("\u2192\u00D7", if (!is.na(tp) && nzchar(tp)) tp else "")
      next
    }
    hit <- match(eid, avg_edges$id)
    ws <- if (eid %in% names(weight_sums)) as.numeric(weight_sums[[eid]]) else 0
    short <- type_short_label(state$edges$type[i])
    lv <- if ("level" %in% names(state$edges)) state$edges$level[i] else NA_character_
    if (!is.na(lv) && nzchar(lv)) short <- paste0(short, "·", lv)

    if (is.na(hit)) {
      state$edges$status[i] <- if (ws > 0) "included" else "excluded"
      state$edges$label[i] <- sprintf("%s Σw=%.2f", short, ws)
      next
    }

    est <- avg_edges$estimate[hit]
    se <- avg_edges$se[hit]
    state$edges$estimate[i] <- est
    state$edges$se[i] <- se
    state$edges$pvalue[i] <- NA_real_
    if ("estimate_std" %in% names(avg_edges)) {
      state$edges$estimate_std[i] <- avg_edges$estimate_std[hit]
    }
    # Keep links with any support visible; mark tiny Σw as excluded for dashes
    state$edges$status[i] <- if (is.finite(ws) && ws >= 0.05) "included" else "excluded"
    state$edges$label[i] <- sprintf(
      "%.2f (SE=%.2f, Σw=%.2f)",
      est, se, ws
    )
  }
  state
}

#' Rebuild best-model state display from a stored search result
#' @keywords internal
apply_best_estimates_from_result <- function(result, state_template) {
  always <- result$always %||% character()
  best_subset <- result$best_exploratory %||% character()
  included <- c(always, best_subset)
  def_ids <- state_template$edges$id[
    state_template$edges$type == "definitional" |
      vapply(state_template$edges$to, function(id) {
        is_interaction_node(state_template, id)
      }, logical(1))
  ]
  st <- apply_estimates_to_edges(
    state_template,
    result$best_estimates %||% result$estimates,
    included_edge_ids = unique(c(included, def_ids))
  )
  r2 <- result$r_squared
  if (is.null(r2) && !is.null(result$fit)) {
    r2 <- r_squared_from_fitted(result, state_template)
  }
  st <- apply_r_squared_to_nodes(st, r2)
  apply_weight_labels_to_edges(st, result$edge_weights %||% list())
}

#' HTML equations using full model-averaged coefficients
#' @keywords internal
format_averaged_equations_html <- function(result, state) {
  if (is.null(result) || is.null(result$averaged_edges)) return(NULL)
  avg <- result$averaged_edges
  if (!nrow(avg)) return(NULL)

  # Group by DV using current graph edges
  edges <- state$edges
  if (!nrow(edges)) return(NULL)
  dvs <- unique(edges$to[edges$type != "measurement"])
  if (!length(dvs)) return(NULL)

  blocks <- lapply(dvs, function(dv) {
    eidx <- which(edges$to == dv & edges$type != "measurement")
    if (!length(eidx)) return(NULL)
    yname <- node_display_name(state, dv)
    # Infer link from engine badge / last fit if available
    wrap_tag <- "model-averaged (full)"
    lhs <- sprintf("E(%s)", htmltools::htmlEscape(yname))
    wrap_l <- ""
    wrap_r <- ""
    if (identical(result$engine, "path") && is.list(result$fit) && !inherits(result$fit, "lavaan")) {
      fit0 <- result$fit[[dv]]
      if (!is.null(fit0)) {
        w <- equation_link_wrapper(fit0, yname)
        wrap_tag <- paste(w$tag, "· averaged")
        lhs <- w$lhs
        wrap_l <- w$wrap_left
        wrap_r <- w$wrap_right
      }
    }

    # Intercept: average if available
    parts <- character()
    y_key <- lavaan_var_name(state, dv)
    if (!is.null(result$averaged_intercepts) && y_key %in% names(result$averaged_intercepts)) {
      ai <- result$averaged_intercepts[[y_key]]
      lo <- ai$est - 1.96 * ai$se
      hi <- ai$est + 1.96 * ai$se
      parts <- c(parts, format_est_ci_html(ai$est, lo, hi))
    }

    for (i in eidx) {
      eid <- edges$id[i]
      hit <- match(eid, avg$id)
      if (is.na(hit)) next
      est <- avg$estimate[hit]
      se <- avg$se[hit]
      lo <- est - 1.96 * se
      hi <- est + 1.96 * se
      pred <- edge_predictor_label(state, eid)
      parts <- c(parts, paste0(format_est_ci_html(est, lo, hi), "&middot;", htmltools::htmlEscape(pred)))

      # Polynomial powers (from best-model estimates; not yet fully averaged)
      est_src <- result$best_estimates %||% result$estimates
      if (!is.null(est_src) && nrow(est_src) && "level" %in% names(est_src)) {
        from_nm <- lavaan_var_name(state, edges$from[i])
        to_nm <- lavaan_var_name(state, dv)
        poly_hit <- est_src$lhs == to_nm & est_src$op == "~" &
          est_src$rhs == from_nm &
          vapply(est_src$level, is_poly_level, logical(1))
        if (any(poly_hit)) {
          prows <- est_src[poly_hit, , drop = FALSE]
          prows <- prows[order(prows$level), , drop = FALSE]
          parts <- c(parts, format_eta_from_estimates(prows))
        }
      }
    }
    if (!length(parts)) return(NULL)
    eta <- paste(parts, collapse = " + ")
    sprintf(
      paste0(
        "<div class=\"smt-eq-block\">",
        "<div class=\"smt-eq-tag\">%s</div>",
        "<div class=\"smt-eq-line\">",
        "<span class=\"smt-eq-lhs\">%s</span> = %s%s%s",
        "</div></div>"
      ),
      htmltools::htmlEscape(wrap_tag),
      lhs,
      wrap_l,
      eta,
      wrap_r
    )
  })
  blocks <- Filter(Negate(is.null), blocks)
  if (!length(blocks)) return(NULL)
  htmltools::HTML(paste0(
    "<div class=\"smt-equations\">",
    "<div class=\"smt-eq-caption\">Model-averaged (full; absent = 0) &middot; estimate<sup class=\"smt-ci\">[95% CI]</sup></div>",
    paste(blocks, collapse = ""),
    "<div class=\"smt-eq-note\">Full model averaging: coefficients are 0 in models that omit the predictor.</div>",
    "</div>"
  ))
}

#' Average intercepts across candidate models (full: always present)
#' @keywords internal
full_average_intercepts <- function(comparison, coef_by_model) {
  ok <- comparison$status == "ok" & is.finite(comparison$weight)
  comp <- comparison[ok, , drop = FALSE]
  if (!nrow(comp)) return(list())

  # Collect all intercept names
  ynames <- unique(unlist(lapply(coef_by_model, function(m) names(m$intercepts))))
  if (!length(ynames)) return(list())

  out <- list()
  for (y in ynames) {
    ests <- numeric(nrow(comp))
    ses <- numeric(nrow(comp))
    for (i in seq_len(nrow(comp))) {
      mid <- as.character(comp$model[i])
      slot <- coef_by_model[[mid]]$intercepts[[y]]
      if (is.null(slot)) {
        ests[i] <- 0
        ses[i] <- 0
      } else {
        ests[i] <- slot$est
        ses[i] <- if (is.finite(slot$se)) slot$se else 0
      }
    }
    w <- comp$weight
    beta_bar <- sum(w * ests)
    se_bar <- sum(w * sqrt(ses^2 + (ests - beta_bar)^2))
    out[[y]] <- list(est = beta_bar, se = se_bar)
  }
  out
}
