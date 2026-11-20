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
    to_id <- state$edges$to[idx]
    to_nm <- lavaan_var_name(state, to_id)
    etype <- state$edges$type[idx]
    lv <- if ("level" %in% names(state$edges)) state$edges$level[idx] else NA_character_
    tl <- if ("to_level" %in% names(state$edges)) state$edges$to_level[idx] else NA_character_
    to_lhs <- dv_outcome_lhs(to_nm, tl)
    if (identical(to_lhs, to_nm)) {
      to_lhs <- dv_transform_lhs(to_nm, node_dv_transform(state, to_id))
    }
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
      hit <- estimates$lhs == to_lhs & estimates$op == "~" & estimates$rhs == from_nm
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

#' Free-edge id set stored on one AIC/BIC comparison row (+ always-in edges)
#' @keywords internal
search_model_edge_ids <- function(free_edges_str, always_ids = character()) {
  free <- if (is.null(free_edges_str) || length(free_edges_str) < 1L ||
               is.na(free_edges_str[[1]]) || !nzchar(as.character(free_edges_str[[1]]))) {
    character()
  } else {
    strsplit(as.character(free_edges_str[[1]]), ",", fixed = TRUE)[[1]]
  }
  unique(c(as.character(always_ids %||% character()), free))
}

#' Does a candidate model include every requested edge?
#' @keywords internal
search_model_includes_edges <- function(free_edges_str, need_ids,
                                        always_ids = character()) {
  need_ids <- unique(as.character(need_ids %||% character()))
  need_ids <- need_ids[!is.na(need_ids) & nzchar(need_ids)]
  if (!length(need_ids)) return(FALSE)
  have <- search_model_edge_ids(free_edges_str, always_ids)
  all(need_ids %in% have)
}

#' Joint Σw: sum of model weights where all listed edges are present
#' @keywords internal
joint_edge_weight_sum <- function(comparison, need_ids, always_ids = character()) {
  need_ids <- unique(as.character(need_ids %||% character()))
  need_ids <- need_ids[!is.na(need_ids) & nzchar(need_ids)]
  if (!length(need_ids) || is.null(comparison) || !nrow(comparison)) {
    return(NA_real_)
  }
  ok <- comparison$status == "ok" & is.finite(comparison$weight)
  if (!any(ok)) return(NA_real_)
  s <- 0
  for (i in which(ok)) {
    if (search_model_includes_edges(comparison$edges[i], need_ids, always_ids)) {
      s <- s + as.numeric(comparison$weight[i])
    }
  }
  as.numeric(s)
}

#' Full model-average of a mediation product a×b (absent → 0)
#'
#' @return list(est, se, weight_sum) with joint Σw for both edges
#' @keywords internal
full_average_mediation_product <- function(comparison, coef_by_model,
                                           edge_a, edge_b,
                                           always_ids = character()) {
  empty <- list(est = NA_real_, se = NA_real_, weight_sum = NA_real_)
  edge_a <- as.character(edge_a %||% "")[[1]]
  edge_b <- as.character(edge_b %||% "")[[1]]
  if (!nzchar(edge_a) || !nzchar(edge_b) || is.null(comparison) || !nrow(comparison)) {
    return(empty)
  }
  ok <- comparison$status == "ok" & is.finite(comparison$weight)
  comp <- comparison[ok, , drop = FALSE]
  if (!nrow(comp)) return(empty)

  abs <- numeric(nrow(comp))
  ses <- numeric(nrow(comp))
  for (i in seq_len(nrow(comp))) {
    mid <- as.character(comp$model[i])
    slot <- coef_by_model[[mid]]
    ea <- if (!is.null(slot)) slot$edges[[edge_a]] else NULL
    eb <- if (!is.null(slot)) slot$edges[[edge_b]] else NULL
    if (is.null(ea) || is.null(eb)) {
      abs[i] <- 0
      ses[i] <- 0
    } else {
      a <- as.numeric(ea$est); b <- as.numeric(eb$est)
      sa <- as.numeric(ea$se); sb <- as.numeric(eb$se)
      if (!is.finite(a)) a <- 0
      if (!is.finite(b)) b <- 0
      if (!is.finite(sa) || sa < 0) sa <- 0
      if (!is.finite(sb) || sb < 0) sb <- 0
      abs[i] <- a * b
      ses[i] <- sqrt(b^2 * sa^2 + a^2 * sb^2)
    }
  }
  w <- as.numeric(comp$weight)
  beta_bar <- sum(w * abs)
  se_bar <- sum(w * sqrt(ses^2 + (abs - beta_bar)^2))
  list(
    est = beta_bar,
    se = se_bar,
    weight_sum = joint_edge_weight_sum(
      comparison, c(edge_a, edge_b), always_ids = always_ids
    )
  )
}

#' Full model-average of path-analytic total (direct + Σ via products)
#'
#' Missing direct / incomplete vias contribute 0 in each candidate (full MA).
#' `weight_sum` = Σw of models with the direct edge or any complete via chain.
#' @param via_pairs list of length-2 character vectors c(edge_a, edge_b)
#' @keywords internal
full_average_mediation_total <- function(comparison, coef_by_model,
                                         edge_direct = NA_character_,
                                         via_pairs = list(),
                                         always_ids = character()) {
  empty <- list(est = NA_real_, se = NA_real_, weight_sum = NA_real_)
  if (is.null(comparison) || !nrow(comparison)) return(empty)
  ok <- comparison$status == "ok" & is.finite(comparison$weight)
  comp <- comparison[ok, , drop = FALSE]
  if (!nrow(comp)) return(empty)

  edge_direct <- as.character(edge_direct %||% "")[[1]]
  if (is.na(edge_direct)) edge_direct <- ""

  tots <- numeric(nrow(comp))
  ses <- numeric(nrow(comp))
  has_path <- logical(nrow(comp))
  for (i in seq_len(nrow(comp))) {
    mid <- as.character(comp$model[i])
    slot <- coef_by_model[[mid]]
    t <- 0
    v_t <- 0
    if (nzchar(edge_direct) && !is.null(slot) && !is.null(slot$edges[[edge_direct]])) {
      ed <- slot$edges[[edge_direct]]
      c0 <- as.numeric(ed$est); sc <- as.numeric(ed$se)
      if (!is.finite(c0)) c0 <- 0
      if (!is.finite(sc) || sc < 0) sc <- 0
      t <- t + c0
      v_t <- v_t + sc^2
      has_path[i] <- TRUE
    }
    for (pr in via_pairs) {
      ea_id <- as.character(pr[[1]] %||% "")
      eb_id <- as.character(pr[[2]] %||% "")
      if (!nzchar(ea_id) || !nzchar(eb_id) || is.null(slot)) next
      ea <- slot$edges[[ea_id]]
      eb <- slot$edges[[eb_id]]
      if (is.null(ea) || is.null(eb)) next
      a <- as.numeric(ea$est); b <- as.numeric(eb$est)
      sa <- as.numeric(ea$se); sb <- as.numeric(eb$se)
      if (!is.finite(a)) a <- 0
      if (!is.finite(b)) b <- 0
      if (!is.finite(sa) || sa < 0) sa <- 0
      if (!is.finite(sb) || sb < 0) sb <- 0
      t <- t + a * b
      v_t <- v_t + b^2 * sa^2 + a^2 * sb^2
      has_path[i] <- TRUE
    }
    tots[i] <- t
    ses[i] <- sqrt(max(0, v_t))
  }
  w <- as.numeric(comp$weight)
  beta_bar <- sum(w * tots)
  se_bar <- sum(w * sqrt(ses^2 + (tots - beta_bar)^2))
  list(
    est = beta_bar,
    se = se_bar,
    weight_sum = sum(w[has_path])
  )
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
apply_averaged_estimates_to_state <- function(state, avg_edges, weight_sums,
                                             min_weight = 0.05) {
  state <- clear_fit_results(state)
  if (!nrow(state$edges) || is.null(avg_edges) || !nrow(avg_edges)) return(state)
  min_weight <- as.numeric(min_weight)
  if (!is.finite(min_weight) || min_weight < 0) min_weight <- 0
  if (min_weight > 1) min_weight <- 1
  # Tiny default floor so Σw≈0 links stay dashed when threshold is 0
  keep_floor <- if (min_weight > 0) min_weight else 0.05

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
    is_meas <- identical(state$edges$type[i], "measurement")

    if (is.na(hit)) {
      state$edges$status[i] <- if (ws > 0 || is_meas) "included" else "excluded"
      if (is_meas) {
        state$edges$label[i] <- short
      } else {
        state$edges$label[i] <- sprintf("%s Σw=%.2f", short, ws)
      }
      next
    }

    est <- avg_edges$estimate[hit]
    se <- avg_edges$se[hit]
    est_std <- if ("estimate_std" %in% names(avg_edges)) {
      as.numeric(avg_edges$estimate_std[hit])
    } else {
      NA_real_
    }
    state$edges$estimate[i] <- est
    state$edges$se[i] <- se
    state$edges$pvalue[i] <- NA_real_
    if ("estimate_std" %in% names(state$edges)) {
      state$edges$estimate_std[i] <- est_std
    }
    if (is_meas) {
      # Always-kept measurement: show standardized loading only (no Σw)
      state$edges$status[i] <- "included"
      show <- if (is.finite(est_std)) est_std else est
      state$edges$label[i] <- if (is.finite(show)) {
        sprintf("%s %.2f", short, show)
      } else {
        short
      }
      next
    }
    keep <- is.finite(ws) && ws >= keep_floor
    state$edges$status[i] <- if (keep) "included" else "excluded"
    state$edges$label[i] <- if (keep) {
      sprintf("%.2f (SE=%.2f, Σw=%.2f)", est, se, ws)
    } else {
      sprintf("%.2f (SE=%.2f, Σw=%.2f) ✕", est, se, ws)
    }
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
  st <- apply_unidimensionality_to_nodes(
    st, result$measurement_unidimensionality
  )
  st <- apply_weight_labels_to_edges(st, result$edge_weights %||% list())
  mark_edges_below_weight(
    st,
    result$edge_weights %||% list(),
    min_weight = result$min_weight %||% 0,
    mark = isTRUE(result$mark_low_weight %||% FALSE) ||
      (is.finite(result$min_weight %||% 0) && (result$min_weight %||% 0) > 0)
  )
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

  abbrev_map <- equation_abbrev_map(result, state)
  meas_html <- format_measurement_equations_html(result, state, abbrev_map)

  blocks <- lapply(dvs, function(dv) {
    eidx <- which(edges$to == dv & edges$type != "measurement")
    if (!length(eidx)) return(NULL)
    yname <- dv_transform_lhs(
      node_display_name(state, dv),
      node_dv_transform(state, dv)
    )
    y_disp <- abbreviate_lhs_display(yname, abbrev_map)
    wrap <- list(
      tag = "model-averaged (full)",
      lhs = sprintf("\\mathrm{E}(%s)", latex_lhs_display(y_disp)),
      wrap_left = "",
      wrap_right = "",
      generative = FALSE,
      sigma = NA_real_,
      note = NULL
    )
    if (identical(result$engine, "path") && is.list(result$fit) && !inherits(result$fit, "lavaan")) {
      fit0 <- result$fit[[dv]]
      if (!is.null(fit0)) {
        wrap <- equation_link_wrapper(fit0, y_disp)
        wrap$tag <- paste(wrap$tag, "· averaged")
      }
    }

    u_map <- wrap$re_u_map
    term_lines <- character()
    y_key <- lavaan_var_name(state, dv)
    if (!is.null(result$averaged_intercepts) && y_key %in% names(result$averaged_intercepts)) {
      ai <- result$averaged_intercepts[[y_key]]
      term_lines <- c(
        term_lines,
        format_coef_with_re(
          format_est_se_tex(ai$est, ai$se),
          match_re_u_symbol("(Intercept)", u_map)
        )
      )
    }

    for (i in eidx) {
      eid <- edges$id[i]
      hit <- match(eid, avg$id)
      if (is.na(hit)) next
      est <- avg$estimate[hit]
      se <- avg$se[hit]
      pred <- edge_predictor_label(state, eid, abbrev_map = abbrev_map)
      from_id <- edges$from[i]
      from_nm <- lavaan_var_name(state, from_id)
      lv <- if ("level" %in% names(edges)) edges$level[i] else NA_character_
      fixed_key <- if (!is.na(lv) && nzchar(as.character(lv)) &&
                         !is_poly_level(lv)) {
        dummy_col_name(from_nm, lv)
      } else {
        from_nm
      }
      term_lines <- c(term_lines, paste0(
        format_coef_with_re(
          format_est_se_tex(est, se),
          match_re_u_symbol(fixed_key, u_map)
        ),
        " \\cdot ",
        latex_lhs_display(pred)
      ))

      est_src <- result$best_estimates %||% result$estimates
      if (!is.null(est_src) && nrow(est_src) && "level" %in% names(est_src)) {
        to_nm <- lavaan_var_name(state, dv)
        to_lhs <- dv_transform_lhs(to_nm, node_dv_transform(state, dv))
        poly_hit <- est_src$lhs == to_lhs & est_src$op == "~" &
          est_src$rhs == from_nm &
          vapply(est_src$level, is_poly_level, logical(1))
        if (any(poly_hit)) {
          prows <- est_src[poly_hit, , drop = FALSE]
          prows <- prows[order(prows$level), , drop = FALSE]
          # Poly terms: fixed only (no RE match for powers)
          poly_bits <- format_eta_from_estimates(
            prows, abbrev_map = abbrev_map, state = state
          )
          if (nzchar(poly_bits)) term_lines <- c(term_lines, poly_bits)
        }
      }
    }
    if (!length(term_lines)) return(NULL)
    sprintf(
      paste0(
        "<div class=\"smt-eq-block\">",
        "<div class=\"smt-eq-tag\">%s</div>",
        "%s",
        "</div>"
      ),
      htmltools::htmlEscape(wrap$tag),
      smt_eq_outcome_html(wrap, term_lines)
    )
  })
  blocks <- Filter(Negate(is.null), blocks)
  if (!length(blocks) && is.null(meas_html)) return(NULL)
  htmltools::HTML(paste0(
    "<div class=\"smt-equations\">",
    meas_html %||% "",
    if (length(blocks)) {
      paste0(
        "<div class=\"smt-eq-caption\">Model-averaged (full; absent = 0) · ",
        "estimate\\(^{\\pm\\mathrm{SE}}\\)</div>",
        paste(blocks, collapse = ""),
        "<div class=\"smt-eq-note\">Full model averaging: coefficients are 0 in models that omit the predictor.</div>"
      )
    } else {
      ""
    },
    "</div>"
  ))
}

#' LaTeX equations using full model-averaged coefficients
#' @keywords internal
format_averaged_equations_latex <- function(result, state) {
  html <- format_averaged_equations_html(result, state)
  if (is.null(html)) return(NULL)
  smt_eq_html_to_latex(html)
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
