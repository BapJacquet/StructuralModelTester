#' Signal cooperative cancellation of a power analysis
#' @keywords internal
smt_cancel_power <- function(message = "Power analysis cancelled.") {
  e <- simpleError(message)
  class(e) <- c("smt_power_cancelled", class(e))
  stop(e)
}

#' TRUE when an error is a cooperative power cancel
#' @keywords internal
smt_is_power_cancelled <- function(e) {
  inherits(e, "smt_power_cancelled") ||
    grepl("Power analysis cancelled", conditionMessage(e), fixed = TRUE)
}

#' Invoke a power progress callback; propagate cancel, swallow UI errors
#' @keywords internal
smt_power_progress <- function(progress, done, total) {
  if (!is.function(progress)) return(invisible(NULL))
  tryCatch(
    progress(done, total),
    error = function(e) {
      if (smt_is_power_cancelled(e)) stop(e)
      invisible(NULL)
    }
  )
}

#' Run n_reps detection counts serially (progress callback may cancel)
#' @keywords internal
power_run_replications <- function(state, n, n_reps, seed, conf_ids,
                                   mode, alpha, criterion, conf_adjust,
                                   min_weight,
                                   latent_score_method = "own",
                                   progress = NULL,
                                   progress_offset = 0L,
                                   progress_total = NULL) {
  n_reps <- as.integer(n_reps)
  conf_ids <- as.character(conf_ids)
  seeds <- as.integer(as.integer(seed) + seq_len(n_reps))
  counts <- integer(n_reps)
  hit_mat <- matrix(0L, nrow = n_reps, ncol = length(conf_ids))
  colnames(hit_mat) <- conf_ids
  done <- as.integer(progress_offset)
  n_i <- as.integer(n)
  for (j in seq_len(n_reps)) {
    # Advance the counter before the (possibly slow) fit so the UI does not
    # sit on the previous tick while this replication is still running.
    smt_power_progress(progress, done + 1L, progress_total)
    hits <- power_one_replication_hits(
      state,
      n = n_i,
      seed = seeds[[j]],
      conf_ids = conf_ids,
      mode = mode,
      alpha = alpha,
      criterion = criterion,
      conf_adjust = conf_adjust,
      min_weight = min_weight,
      latent_score_method = latent_score_method
    )
    # Align in case of missing names
    h <- as.integer(hits[conf_ids])
    h[is.na(h)] <- 0L
    hit_mat[j, ] <- h
    counts[[j]] <- as.integer(sum(hit_mat[j, ], na.rm = TRUE))
    done <- done + 1L
  }
  list(counts = counts, hits = hit_mat, done = done)
}

#' Starting sample sizes for iterative power search
#'
#' Several low-N anchors (log-spaced) plus the high end. Low-N coverage
#' stabilizes the logistic-in-log(N) curve fit for N* (especially BIC, where
#' the null P(k=0) curve also matters).
#'
#' @param n_max Maximum sample size (Samples / Max sample).
#' @param n_low Number of low-N anchor points (before the high end).
#' @return Sorted unique integer vector of N values (≥ 1).
#' @keywords internal
power_n_bounds <- function(n_max, n_low = 4L) {
  n_max <- as.integer(n_max)
  if (!is.finite(n_max) || n_max < 10L) {
    stop("Samples must be at least 10 for power analysis.", call. = FALSE)
  }
  n_low <- as.integer(n_low)
  if (!is.finite(n_low) || n_low < 1L) n_low <- 1L
  if (n_low > 8L) n_low <- 8L

  n_floor <- 20L
  if (n_max <= n_floor) {
    return(as.integer(n_max))
  }

  # Span the lower quarter of the searchable range (at least a bit above floor)
  n_low_hi <- min(
    n_max - 1L,
    max(n_floor + 10L, as.integer(round(n_max / 4)))
  )
  if (n_low_hi <= n_floor) {
    lows <- as.integer(n_floor)
  } else if (n_low == 1L) {
    lows <- as.integer(n_floor)
  } else {
    lows <- as.integer(round(exp(seq(
      log(n_floor), log(n_low_hi), length.out = n_low
    ))))
  }
  lows <- lows[is.finite(lows) & lows >= 10L & lows < n_max]
  sort(unique(as.integer(c(lows, n_max))))
}

#' @rdname power_n_bounds
#' @keywords internal
power_n_grid <- function(n_max, n_points = 7L) {
  # Compatibility alias: prefer low-anchor grid over a dense uniform grid.
  n_low <- max(1L, as.integer(n_points) - 1L)
  power_n_bounds(n_max, n_low = n_low)
}

#' Confirmatory analyzable edge ids used as the power target
#'
#' Measurement links are never power targets (they only feed the sim DGP /
#' CFA stage). Only structural confirmatory paths are counted.
#' @keywords internal
confirmatory_power_edge_ids <- function(state) {
  analyzable_edge_ids(
    state,
    state$edges$id[state$edges$type == "confirmatory"]
  )
}

#' Confirmatory power targets whose expected effect is still 0
#' @return Character vector of edge ids (possibly empty).
#' @keywords internal
confirmatory_zero_prior_edge_ids <- function(state, conf_ids = NULL) {
  if (is.null(conf_ids)) conf_ids <- confirmatory_power_edge_ids(state)
  if (!length(conf_ids)) return(character())
  state <- ensure_edge_priors(state)
  conf_ids[vapply(conf_ids, function(eid) {
    m <- edge_prior(state, eid)$mean
    !is.finite(m) || abs(as.numeric(m)) <= 1e-12
  }, logical(1))]
}

#' Snapshot of generative residual SD for confirmatory-target DVs
#'
#' @return data.frame with columns `id`, `label`, `resid_sd`, `kind`
#'   (possibly 0 rows).
#' @keywords internal
power_dv_residuals_snapshot <- function(state, conf_ids = NULL) {
  empty <- data.frame(
    id = character(),
    label = character(),
    resid_sd = numeric(),
    kind = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(state) || is.null(state$edges) || !nrow(state$edges)) {
    return(empty)
  }
  state <- ensure_node_sim_columns(state)
  if (is.null(conf_ids)) conf_ids <- confirmatory_power_edge_ids(state)
  if (!length(conf_ids)) return(empty)
  idx <- match(conf_ids, state$edges$id)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) return(empty)
  dv_ids <- unique(as.character(state$edges$to[idx]))
  dv_ids <- dv_ids[nzchar(dv_ids)]
  if (!length(dv_ids)) return(empty)

  rows <- lapply(dv_ids, function(id) {
    sp <- node_sim_params(state, id)
    kind <- node_sim_var_kind(state, id, data = NULL)
    lab <- tryCatch(node_display_name(state, id), error = function(e) id)
    data.frame(
      id = id,
      label = as.character(lab),
      resid_sd = as.numeric(sp$resid_sd),
      kind = as.character(kind),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Short labels for confirmatory links (for power warnings)
#' @keywords internal
confirmatory_edge_warning_labels <- function(state, edge_ids) {
  edge_ids <- as.character(edge_ids)
  if (!length(edge_ids) || is.null(state) || !nrow(state$edges)) {
    return(character())
  }
  vapply(edge_ids, function(eid) {
    idx <- match(eid, state$edges$id)
    if (is.na(idx)) return(eid)
    from_lab <- tryCatch(
      node_display_name(state, state$edges$from[[idx]]),
      error = function(e) state$edges$from[[idx]]
    )
    to_lab <- tryCatch(
      node_display_name(state, state$edges$to[[idx]]),
      error = function(e) state$edges$to[[idx]]
    )
    lv <- if ("level" %in% names(state$edges)) state$edges$level[[idx]] else NA_character_
    tl <- if ("to_level" %in% names(state$edges)) state$edges$to_level[[idx]] else NA_character_
    lab <- sprintf("%s \u2192 %s", from_lab, to_lab)
    if (!is.na(lv) && nzchar(as.character(lv))) {
      lab <- sprintf("%s[%s] \u2192 %s", from_lab, lv, to_lab)
    }
    if (!is.na(tl) && nzchar(as.character(tl))) {
      lab <- paste0(lab, "[", tl, "]")
    }
    lab
  }, character(1), USE.NAMES = FALSE)
}

#' Copy of state with confirmatory expected effects set to 0
#' @keywords internal
state_with_null_confirmatory <- function(state, conf_ids = NULL) {
  state <- ensure_edge_priors(state)
  if (is.null(conf_ids)) conf_ids <- confirmatory_power_edge_ids(state)
  for (eid in conf_ids) {
    pr <- edge_prior(state, eid)
    state <- set_edge_prior(state, eid, mean = 0, sd = pr$sd, family = pr$family)
  }
  state
}

#' Per-link detection hits under a p-value fit (0/1, named by edge id)
#' @keywords internal
confirmatory_detection_hits_pvalue <- function(fit_res, conf_ids, alpha = 0.05) {
  conf_ids <- as.character(conf_ids)
  hits <- stats::setNames(integer(length(conf_ids)), conf_ids)
  if (!length(conf_ids)) return(hits)
  if (is.null(fit_res) || is.null(fit_res$state) || !nrow(fit_res$state$edges)) {
    return(hits)
  }
  edges <- fit_res$state$edges
  for (eid in conf_ids) {
    idx <- match(eid, edges$id)
    if (is.na(idx)) next
    pv <- edges$pvalue[[idx]]
    if (is.finite(pv) && pv < alpha) hits[[eid]] <- 1L
  }
  hits
}

#' Count confirmatory links detected under a p-value fit
#' @keywords internal
confirmatory_detection_count_pvalue <- function(fit_res, conf_ids, alpha = 0.05) {
  as.integer(sum(confirmatory_detection_hits_pvalue(fit_res, conf_ids, alpha = alpha)))
}

#' Per-link detection hits under AIC/BIC search (0/1, named by edge id)
#'
#' When `min_weight` > 0, an included confirmatory link also needs
#' Σw ≥ `min_weight` (same rejection rule as the canvas).
#' @keywords internal
confirmatory_detection_hits_search <- function(fit_res, conf_ids,
                                              min_weight = 0) {
  conf_ids <- as.character(conf_ids)
  hits <- stats::setNames(integer(length(conf_ids)), conf_ids)
  if (!length(conf_ids) || is.null(fit_res)) return(hits)
  best <- fit_res$best_exploratory
  if (is.null(best)) best <- character()
  always <- fit_res$always %||% character()
  included <- unique(c(as.character(always), as.character(best)))
  min_weight <- as.numeric(min_weight)
  wsums <- fit_res$edge_weights %||% list()
  for (eid in conf_ids) {
    if (!(eid %in% included)) next
    if (!is.finite(min_weight) || min_weight <= 0) {
      hits[[eid]] <- 1L
      next
    }
    ws <- if (eid %in% names(wsums)) as.numeric(wsums[[eid]]) else 0
    if (is.finite(ws) && ws >= min_weight) hits[[eid]] <- 1L
  }
  hits
}

#' Count confirmatory links included in the AIC/BIC best model
#' @keywords internal
confirmatory_detection_count_search <- function(fit_res, conf_ids,
                                               min_weight = 0) {
  as.integer(sum(confirmatory_detection_hits_search(
    fit_res, conf_ids, min_weight = min_weight
  )))
}

#' One Monte Carlo replication: simulate, fit, per-link detection hits
#'
#' Fit/simulate errors count as no detections.
#' @return Named integer vector (0/1) aligned with `conf_ids`.
#' @keywords internal
power_one_replication_hits <- function(state, n, seed, conf_ids,
                                       mode = c("pvalue", "search"),
                                       alpha = 0.05,
                                       criterion = c("aic", "bic"),
                                       conf_adjust = c("holm", "none", "bonferroni", "BH"),
                                       min_weight = 0,
                                       latent_score_method = "own") {
  mode <- match.arg(mode)
  criterion <- match.arg(criterion)
  conf_adjust <- match.arg(conf_adjust)
  conf_ids <- as.character(conf_ids)
  empty <- stats::setNames(integer(length(conf_ids)), conf_ids)
  min_weight <- as.numeric(min_weight)
  if (!is.finite(min_weight) || min_weight < 0) min_weight <- 0
  latent_score_method <- normalize_latent_score_method(latent_score_method, state)
  tryCatch({
    sim <- simulate_path_data(state, n = as.integer(n), seed = as.integer(seed))
    fit_st <- bind_sim_columns_to_state(state, sim)
    res <- if (identical(mode, "pvalue")) {
      fit_pvalue_model(
        fit_st, sim,
        alpha = alpha,
        mark_nonsig = FALSE,
        conf_adjust = conf_adjust,
        uncertainty = "none",
        latent_score_method = latent_score_method
      )
    } else {
      fit_aic_bic_search(
        fit_st, sim,
        criterion = criterion,
        uncertainty = "none",
        min_weight = min_weight,
        mark_low_weight = is.finite(min_weight) && min_weight > 0,
        latent_score_method = latent_score_method
      )
    }
    if (identical(mode, "pvalue")) {
      confirmatory_detection_hits_pvalue(res, conf_ids, alpha = alpha)
    } else {
      confirmatory_detection_hits_search(res, conf_ids, min_weight = min_weight)
    }
  }, error = function(e) {
    if (exists("smt_debug", mode = "function")) {
      smt_debug(
        "power rep error | n=", n, " seed=", seed,
        " | ", conditionMessage(e)
      )
    }
    empty
  })
}

#' One Monte Carlo replication: count of confirmatory detections
#' @keywords internal
power_one_replication_count <- function(state, n, seed, conf_ids,
                                        mode = c("pvalue", "search"),
                                        alpha = 0.05,
                                        criterion = c("aic", "bic"),
                                        conf_adjust = c("holm", "none", "bonferroni", "BH"),
                                        min_weight = 0,
                                        latent_score_method = "own") {
  as.integer(sum(power_one_replication_hits(
    state, n = n, seed = seed, conf_ids = conf_ids,
    mode = mode, alpha = alpha, criterion = criterion,
    conf_adjust = conf_adjust, min_weight = min_weight,
    latent_score_method = latent_score_method
  )))
}

#' Wilson score CI for a Monte Carlo binomial rate
#'
#' Prefer Wilson over the normal `p ± z√(p(1-p)/n)` approximation, which is
#' poor near 0/1 and with modest replication counts. `se` is still the usual
#' binomial SE (for diagnostics); `lower`/`upper` are Wilson.
#' @keywords internal
binomial_rate_band <- function(n_ok, n_reps, z = 1.96) {
  n_ok <- as.numeric(n_ok)
  n_reps <- as.numeric(n_reps)
  z <- as.numeric(z)
  if (!is.finite(z) || z <= 0) z <- 1.96
  p <- ifelse(n_reps > 0, n_ok / n_reps, NA_real_)
  se <- ifelse(n_reps > 0, sqrt(pmax(0, p * (1 - p) / n_reps)), NA_real_)
  # Wilson score interval (see Newcombe 1998)
  z2 <- z * z
  den <- ifelse(n_reps > 0, 1 + z2 / n_reps, NA_real_)
  center <- ifelse(
    n_reps > 0,
    (p + z2 / (2 * n_reps)) / den,
    NA_real_
  )
  half <- ifelse(
    n_reps > 0,
    z * sqrt(pmax(0, p * (1 - p) / n_reps + z2 / (4 * n_reps * n_reps))) / den,
    NA_real_
  )
  list(
    rate = p,
    se = se,
    lower = pmax(0, center - half),
    upper = pmin(1, center + half)
  )
}

#' Run detection counts over an N × replication grid
#' @keywords internal
power_count_grid <- function(state, n_grid, n_reps, seed, conf_ids,
                             mode, alpha, criterion, conf_adjust = "none",
                             min_weight = 0,
                             latent_score_method = "own",
                             progress = NULL,
                             progress_offset = 0L, progress_total = NULL) {
  conf_ids <- as.character(conf_ids)
  n_conf <- length(conf_ids)
  rows_power <- vector("list", length(n_grid))
  rows_edge <- vector("list", length(n_grid) * max(1L, n_conf))
  rows_fdr <- vector("list", length(n_grid) * (n_conf + 1L))
  fdr_i <- 0L
  edge_i <- 0L
  done <- as.integer(progress_offset)
  if (is.null(progress_total)) {
    progress_total <- progress_offset + length(n_grid) * n_reps
  }

  for (i in seq_along(n_grid)) {
    n <- n_grid[[i]]
    seed_i <- as.integer(seed + (i - 1L) * n_reps)
    run <- power_run_replications(
      state, n = n, n_reps = n_reps, seed = seed_i, conf_ids = conf_ids,
      mode = mode, alpha = alpha, criterion = criterion,
      conf_adjust = conf_adjust, min_weight = min_weight,
      latent_score_method = latent_score_method,
      progress = progress,
      progress_offset = done, progress_total = progress_total
    )
    counts <- run$counts
    hit_mat <- run$hits
    done <- run$done
    n_all <- sum(counts == n_conf)
    band_all <- binomial_rate_band(n_all, n_reps)
    rows_power[[i]] <- data.frame(
      n = n,
      power = band_all$rate,
      n_ok = n_all,
      n_reps = n_reps,
      se = band_all$se,
      lower = band_all$lower,
      upper = band_all$upper,
      stringsAsFactors = FALSE
    )
    if (!is.null(hit_mat) && ncol(hit_mat) == n_conf && n_conf > 0L) {
      for (k in seq_len(n_conf)) {
        n_ok_e <- as.integer(sum(hit_mat[, k], na.rm = TRUE))
        band_e <- binomial_rate_band(n_ok_e, n_reps)
        edge_i <- edge_i + 1L
        rows_edge[[edge_i]] <- data.frame(
          n = n,
          edge_id = conf_ids[[k]],
          power = band_e$rate,
          n_ok = n_ok_e,
          n_reps = n_reps,
          se = band_e$se,
          lower = band_e$lower,
          upper = band_e$upper,
          stringsAsFactors = FALSE
        )
      }
    }
    for (k in 0:n_conf) {
      n_ok_k <- sum(counts == k)
      band_k <- binomial_rate_band(n_ok_k, n_reps)
      fdr_i <- fdr_i + 1L
      rows_fdr[[fdr_i]] <- data.frame(
        n = n,
        n_detected = k,
        rate = band_k$rate,
        n_ok = n_ok_k,
        n_reps = n_reps,
        se = band_k$se,
        lower = band_k$lower,
        upper = band_k$upper,
        stringsAsFactors = FALSE
      )
    }
  }

  edge_power <- if (edge_i > 0L) {
    do.call(rbind, rows_edge[seq_len(edge_i)])
  } else {
    data.frame(
      n = integer(),
      edge_id = character(),
      power = numeric(),
      n_ok = integer(),
      n_reps = integer(),
      se = numeric(),
      lower = numeric(),
      upper = numeric(),
      stringsAsFactors = FALSE
    )
  }

  list(
    power = do.call(rbind, rows_power),
    edge_power = edge_power,
    counts = do.call(rbind, rows_fdr[seq_len(fdr_i)])
  )
}

#' Merge per-edge power rows; refine overwrites same (n, edge_id)
#' @keywords internal
merge_edge_power_rows <- function(coarse, fine) {
  if (is.null(fine) || !nrow(fine)) return(coarse)
  if (is.null(coarse) || !nrow(coarse)) return(fine)
  all <- rbind(coarse, fine)
  key <- paste(all$n, all$edge_id, sep = "\r")
  all[!duplicated(key, fromLast = TRUE), , drop = FALSE]
}

#' Deduplicate and order Monte Carlo rate points by N
#' @keywords internal
power_rate_points <- function(n, rate, n_ok = NULL, n_reps = NULL) {
  n <- as.numeric(n)
  rate <- as.numeric(rate)
  n_ok <- if (is.null(n_ok)) rep(NA_real_, length(n)) else as.numeric(n_ok)
  n_reps <- if (is.null(n_reps)) rep(NA_real_, length(n)) else as.numeric(n_reps)
  len <- max(length(n), length(rate), length(n_ok), length(n_reps))
  if (length(n) == 1L && len > 1L) n <- rep(n, len)
  if (length(rate) == 1L && len > 1L) rate <- rep(rate, len)
  if (length(n_ok) == 1L && len > 1L) n_ok <- rep(n_ok, len)
  if (length(n_reps) == 1L && len > 1L) n_reps <- rep(n_reps, len)
  ok <- is.finite(n) & n > 0 & is.finite(rate)
  n <- n[ok]
  rate <- rate[ok]
  n_ok <- n_ok[ok]
  n_reps <- n_reps[ok]
  if (!length(n)) {
    return(data.frame(
      n = numeric(), rate = numeric(),
      n_ok = numeric(), n_reps = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  ord <- order(n)
  n <- n[ord]
  rate <- rate[ord]
  n_ok <- n_ok[ord]
  n_reps <- n_reps[ord]
  keep <- !duplicated(n, fromLast = TRUE)
  data.frame(
    n = n[keep],
    rate = rate[keep],
    n_ok = n_ok[keep],
    n_reps = n_reps[keep],
    stringsAsFactors = FALSE
  )
}

#' Binomial logistic fit of rate vs log(N)
#'
#' Prefers `cbind(n_ok, n_reps - n_ok) ~ log(n)`. Falls back to quasibinomial
#' on the empirical rate when counts are unavailable. Returns `NULL` if the
#' fit is unusable (too few points, non-finite coeffs). When
#' `require_positive_slope` is TRUE (N* solves), a non-positive log(N) slope
#' is also rejected.
#' @keywords internal
fit_rate_logn_glm <- function(n, rate, n_ok = NULL, n_reps = NULL,
                              require_positive_slope = TRUE) {
  pts <- power_rate_points(n, rate, n_ok = n_ok, n_reps = n_reps)
  if (nrow(pts) < 2L) return(NULL)

  use_counts <- all(is.finite(pts$n_ok) & is.finite(pts$n_reps) &
                      pts$n_reps > 0 &
                      pts$n_ok >= 0 & pts$n_ok <= pts$n_reps)
  fit <- tryCatch({
    if (use_counts) {
      suppressWarnings(
        stats::glm(
          cbind(n_ok, n_reps - n_ok) ~ log(n),
          data = pts,
          family = stats::binomial()
        )
      )
    } else {
      suppressWarnings(
        stats::glm(
          rate ~ log(n),
          data = pts,
          family = stats::quasibinomial()
        )
      )
    }
  }, error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  coefs <- stats::coef(fit)
  if (length(coefs) < 2L) return(NULL)
  b0 <- unname(coefs[[1L]])
  b1 <- unname(coefs[[2L]])
  if (!is.finite(b0) || !is.finite(b1)) return(NULL)
  if (isTRUE(require_positive_slope) && b1 <= 0) return(NULL)

  list(
    b0 = b0,
    b1 = b1,
    n = pts$n,
    rate = pts$rate,
    n_ok = pts$n_ok,
    n_reps = pts$n_reps,
    fit = fit
  )
}

#' Predict rate from a `fit_rate_logn_glm` object
#' @keywords internal
predict_rate_logn <- function(fit_obj, n) {
  if (is.null(fit_obj)) return(rep(NA_real_, length(n)))
  stats::plogis(fit_obj$b0 + fit_obj$b1 * log(pmax(as.numeric(n), .Machine$double.eps)))
}

#' Predict rate ± z·SE on the response scale from a fitted log-N GLM
#'
#' Degenerate series (all 0 / all 1 / separation) produce enormous link SEs that
#' map to a full [0, 1] ribbon — those bands are collapsed to the point estimate.
#' @keywords internal
predict_rate_logn_band <- function(fit_obj, n, z = 1.96,
                                   max_width = 0.85) {
  n <- as.numeric(n)
  empty <- data.frame(
    n = n,
    rate = rep(NA_real_, length(n)),
    lower = rep(NA_real_, length(n)),
    upper = rep(NA_real_, length(n)),
    stringsAsFactors = FALSE
  )
  if (is.null(fit_obj) || !length(n)) return(empty)
  z <- as.numeric(z)
  if (!is.finite(z) || z <= 0) z <- 1.96
  max_width <- as.numeric(max_width)
  if (!is.finite(max_width) || max_width <= 0) max_width <- 0.85

  # Flat 0/1 Monte Carlo series: no estimable uncertainty from the GLM
  obs_rate <- fit_obj$rate
  if (length(obs_rate) && all(is.finite(obs_rate))) {
    if (all(obs_rate <= 0) || all(obs_rate >= 1) ||
          diff(range(obs_rate)) < 1e-12) {
      y0 <- mean(obs_rate)
      return(data.frame(
        n = n,
        rate = rep(y0, length(n)),
        lower = rep(y0, length(n)),
        upper = rep(y0, length(n)),
        stringsAsFactors = FALSE
      ))
    }
  }

  if (!is.null(fit_obj$fit)) {
    pr <- tryCatch(
      stats::predict(
        fit_obj$fit,
        newdata = data.frame(n = n),
        type = "link",
        se.fit = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(pr)) {
      fit <- as.numeric(pr$fit)
      se <- as.numeric(pr$se.fit)
      # Cap absurd link SEs (complete separation / all-zero fits)
      se[!is.finite(se) | se > 10] <- NA_real_
      rate <- stats::plogis(fit)
      lower <- stats::plogis(fit - z * se)
      upper <- stats::plogis(fit + z * se)
      width <- upper - lower
      bad <- !is.finite(width) | width > max_width | is.na(se)
      lower[bad] <- rate[bad]
      upper[bad] <- rate[bad]
      return(data.frame(
        n = n,
        rate = rate,
        lower = lower,
        upper = upper,
        stringsAsFactors = FALSE
      ))
    }
  }
  y <- predict_rate_logn(fit_obj, n)
  data.frame(n = n, rate = y, lower = y, upper = y, stringsAsFactors = FALSE)
}

#' Linear N where a monotone rate first reaches `target` (fallback)
#'
#' Returns continuous N (not rounded). Interpolates between points when the
#' target is crossed; if the highest evaluated N is still below target but the
#' last segment is rising, extrapolates beyond that N. `Inf` if unreachable
#' (flat/decreasing last segment). Smallest `n` if already at/above target on
#' the first point.
#' @keywords internal
interpolate_rate_threshold_n <- function(n, rate, target) {
  pts <- power_rate_points(n, rate)
  n <- pts$n
  rate <- pts$rate
  if (!length(n)) return(Inf)
  if (rate[[1]] >= target) return(n[[1]])
  for (i in seq_len(length(n) - 1L)) {
    r0 <- rate[[i]]
    r1 <- rate[[i + 1L]]
    if (r0 < target && r1 >= target) {
      if (!is.finite(r1 - r0) || abs(r1 - r0) < .Machine$double.eps) {
        return(n[[i + 1L]])
      }
      w <- (target - r0) / (r1 - r0)
      w <- max(0, min(1, w))
      return(n[[i]] + w * (n[[i + 1L]] - n[[i]]))
    }
  }
  # Still below target at the largest N: extrapolate if the last segment rises
  if (length(n) >= 2L && rate[[length(rate)]] < target) {
    i <- length(n) - 1L
    r0 <- rate[[i]]
    r1 <- rate[[i + 1L]]
    n0 <- n[[i]]
    n1 <- n[[i + 1L]]
    dr <- r1 - r0
    if (is.finite(dr) && dr > .Machine$double.eps && n1 > n0) {
      return(n1 + (n1 - n0) * (target - r1) / dr)
    }
  }
  Inf
}

#' Curve-fit N where a rate reaches `target` (logistic in log N)
#'
#' Fits a binomial GLM of detection rate vs `log(n)` and solves for the N at
#' which the fitted curve hits `target`. Falls back to linear segment
#' interpolate/extrapolate when the GLM is unusable. Returns continuous N
#' (not rounded); `Inf` if unreachable.
#' @keywords internal
fit_rate_threshold_n <- function(n, rate, target, n_ok = NULL, n_reps = NULL) {
  target <- as.numeric(target)
  if (!is.finite(target) || target <= 0 || target >= 1) {
    return(interpolate_rate_threshold_n(n, rate, target))
  }
  pts <- power_rate_points(n, rate, n_ok = n_ok, n_reps = n_reps)
  if (!nrow(pts)) return(Inf)
  if (pts$rate[[1]] >= target) return(pts$n[[1]])

  fit_obj <- fit_rate_logn_glm(pts$n, pts$rate, n_ok = pts$n_ok, n_reps = pts$n_reps)
  if (is.null(fit_obj)) {
    return(interpolate_rate_threshold_n(pts$n, pts$rate, target))
  }

  pred_min <- predict_rate_logn(fit_obj, pts$n[[1]])
  if (is.finite(pred_min) && pred_min >= target) return(pts$n[[1]])

  # plogis(b0 + b1 * log(n)) = target  =>  n = exp((logit(target) - b0) / b1)
  n_star <- exp((stats::qlogis(target) - fit_obj$b0) / fit_obj$b1)
  if (!is.finite(n_star) || n_star <= 0) {
    return(interpolate_rate_threshold_n(pts$n, pts$rate, target))
  }
  max(10, n_star)
}

#' Smooth curve predictions for plotting a fitted rate-vs-N model
#'
#' Includes `lower` / `upper` (±1.96 SE on the link scale, mapped to response)
#' for `geom_ribbon`.
#' @keywords internal
power_fitted_curve_df <- function(n, rate, n_ok = NULL, n_reps = NULL,
                                  n_star = NULL, n_guide = 80L,
                                  require_positive_slope = TRUE,
                                  y_name = "power") {
  fit_obj <- fit_rate_logn_glm(
    n, rate, n_ok = n_ok, n_reps = n_reps,
    require_positive_slope = require_positive_slope
  )
  if (is.null(fit_obj)) return(NULL)
  n_obs <- fit_obj$n
  n_lo <- min(n_obs)
  n_hi <- max(n_obs)
  if (is.finite(n_star) && n_star > n_hi) n_hi <- as.numeric(n_star)
  # Small pad so the guide extends slightly past the last evaluated N
  if (n_hi <= n_lo) {
    n_seq <- n_lo
  } else {
    n_seq <- exp(seq(log(n_lo), log(n_hi), length.out = max(2L, as.integer(n_guide))))
  }
  band <- predict_rate_logn_band(fit_obj, n_seq)
  out <- data.frame(
    n = band$n,
    lower = band$lower,
    upper = band$upper,
    stringsAsFactors = FALSE
  )
  out[[y_name]] <- band$rate
  out
}

#' Per-k logistic curve guides for the false-detection plot
#' @keywords internal
fdr_fitted_curves_df <- function(fdr_df, recommended_n = NULL, n_guide = 80L) {
  if (is.null(fdr_df) || !nrow(fdr_df)) return(NULL)
  ks <- sort(unique(as.integer(fdr_df$n_detected)))
  rows <- vector("list", length(ks))
  for (i in seq_along(ks)) {
    k <- ks[[i]]
    sub <- fdr_df[as.integer(fdr_df$n_detected) == k, , drop = FALSE]
    n_ok <- if ("n_ok" %in% names(sub)) sub$n_ok else NULL
    n_reps <- if ("n_reps" %in% names(sub)) sub$n_reps else NULL
    cdf <- power_fitted_curve_df(
      sub$n, sub$rate,
      n_ok = n_ok, n_reps = n_reps,
      n_star = recommended_n,
      n_guide = n_guide,
      require_positive_slope = FALSE,
      y_name = "rate"
    )
    if (is.null(cdf) || !nrow(cdf)) next
    cdf$n_detected <- k
    rows[[i]] <- cdf
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Power and P(≥1 false detection) at a sample size
#'
#' Always uses the same logistic-in-log(N) curve as N* (linear approx only if
#' the GLM is unusable). Monte Carlo points feed the fit; they are not reported
#' raw even when `n` was evaluated.
#' @keywords internal
power_metrics_at_n <- function(power_df, fdr_df, n) {
  n <- as.numeric(n)
  out <- list(power = NA_real_, fdr_ge1 = NA_real_)
  if (!is.finite(n) || n < 1) return(out)

  rate_at <- function(n_vec, rate, n_ok = NULL, n_reps = NULL, n_query) {
    n_vec <- as.numeric(n_vec)
    rate <- as.numeric(rate)
    fit_obj <- fit_rate_logn_glm(n_vec, rate, n_ok = n_ok, n_reps = n_reps)
    pred <- predict_rate_logn(fit_obj, n_query)
    if (is.finite(pred)) return(as.numeric(pred))
    if (length(n_vec) >= 2L) {
      return(as.numeric(stats::approx(n_vec, rate, xout = n_query, rule = 2)$y))
    }
    # Last resort: single evaluated point
    hit <- which(as.integer(round(n_vec)) == as.integer(round(n_query)))
    if (length(hit)) return(as.numeric(rate[[hit[[1]]]]))
    NA_real_
  }

  if (!is.null(power_df) && nrow(power_df)) {
    out$power <- rate_at(
      power_df$n, power_df$power,
      n_ok = if ("n_ok" %in% names(power_df)) power_df$n_ok else NULL,
      n_reps = if ("n_reps" %in% names(power_df)) power_df$n_reps else NULL,
      n_query = n
    )
  }
  if (!is.null(fdr_df) && nrow(fdr_df)) {
    zero <- fdr_df[as.integer(fdr_df$n_detected) == 0L, , drop = FALSE]
    if (nrow(zero)) {
      p0 <- rate_at(
        zero$n, zero$rate,
        n_ok = if ("n_ok" %in% names(zero)) zero$n_ok else NULL,
        n_reps = if ("n_reps" %in% names(zero)) zero$n_reps else NULL,
        n_query = n
      )
      if (is.finite(p0)) out$fdr_ge1 <- max(0, min(1, 1 - p0))
    }
  }
  out
}

#' Format power / false-detection metrics for N* messages
#' @keywords internal
format_power_metrics_at_n <- function(power_df, fdr_df, n) {
  m <- power_metrics_at_n(power_df, fdr_df, n)
  parts <- character()
  if (is.finite(m$power)) {
    parts <- c(parts, sprintf("power %.0f%%", 100 * m$power))
  }
  if (is.finite(m$fdr_ge1)) {
    parts <- c(parts, sprintf("P(≥1 false detection) %.0f%%", 100 * m$fdr_ge1))
  }
  if (!length(parts)) return("")
  paste(parts, collapse = ", ")
}

#' Curve-fit N* with power ≥ 80% (and optionally P(0 false detections) ≥ 95%)
#'
#' Uses a logistic-in-log(N) binomial GLM on the Monte Carlo rates (linear
#' fallback if the fit fails). For AIC/BIC search, takes the more demanding of
#' the two threshold crossings. For p-value mode, false-detection rates under
#' the complete null stay near `(1-α)^K` without multiplicity correction, so
#' N* uses power only.
#'
#' @keywords internal
recommend_power_sample_size <- function(power_df, fdr_df,
                                        min_power = 0.8,
                                        min_zero_null = 0.95,
                                        require_zero_null = TRUE) {
  if (is.null(power_df) || !nrow(power_df)) {
    return(NA_integer_)
  }
  n_ok_p <- if ("n_ok" %in% names(power_df)) power_df$n_ok else NULL
  n_reps_p <- if ("n_reps" %in% names(power_df)) power_df$n_reps else NULL
  n_power <- fit_rate_threshold_n(
    power_df$n, power_df$power, min_power,
    n_ok = n_ok_p, n_reps = n_reps_p
  )
  if (!isTRUE(require_zero_null)) {
    if (!is.finite(n_power)) return(NA_integer_)
    return(as.integer(max(10L, as.integer(ceiling(n_power - 1e-9)))))
  }
  if (is.null(fdr_df) || !nrow(fdr_df)) return(NA_integer_)
  zero <- fdr_df[fdr_df$n_detected == 0L, , drop = FALSE]
  if (!nrow(zero)) return(NA_integer_)
  n_ok_z <- if ("n_ok" %in% names(zero)) zero$n_ok else NULL
  n_reps_z <- if ("n_reps" %in% names(zero)) zero$n_reps else NULL
  n_zero <- fit_rate_threshold_n(
    zero$n, zero$rate, min_zero_null,
    n_ok = n_ok_z, n_reps = n_reps_z
  )
  n_star <- max(n_power, n_zero)
  if (!is.finite(n_star)) return(NA_integer_)
  as.integer(max(10L, as.integer(ceiling(n_star - 1e-9))))
}

#' Integer refine N at the curve-fit / extrapolated crossing
#'
#' May exceed the Simulation Samples (`n_max`) starting high end, but never
#' exceeds `hard_cap` (UI Max sample).
#' @keywords internal
power_refine_n_point <- function(n_star, n_max = NULL, hard_cap = NULL) {
  n_star <- as.numeric(n_star)
  if (!is.finite(n_star) || n_star < 10) return(integer())
  n_star <- as.integer(ceiling(n_star - 1e-9))
  n_star <- max(10L, n_star)
  if (is.null(hard_cap) || !is.finite(hard_cap) || hard_cap < 10L) {
    n_ref <- if (!is.null(n_max) && is.finite(n_max)) as.integer(n_max) else n_star
    hard_cap <- max(1000L, as.integer(n_ref))
  }
  hard_cap <- as.integer(hard_cap)
  n_star <- min(n_star, hard_cap)
  as.integer(n_star)
}

#' Merge coarse + refine Monte Carlo rows (refine overwrites same N)
#' @keywords internal
merge_power_curve_rows <- function(coarse, fine) {
  if (is.null(fine) || !nrow(fine)) return(coarse)
  if (is.null(coarse) || !nrow(coarse)) return(fine)
  all <- rbind(coarse, fine)
  # Prefer refine rows when N collides
  all[!duplicated(all$n, fromLast = TRUE), , drop = FALSE]
}

#' Merge FDR count curves by (n, n_detected); refine overwrites
#' @keywords internal
merge_fdr_curve_rows <- function(coarse, fine) {
  if (is.null(fine) || !nrow(fine)) return(coarse)
  if (is.null(coarse) || !nrow(coarse)) return(fine)
  all <- rbind(coarse, fine)
  key <- paste(all$n, all$n_detected, sep = "\r")
  all[!duplicated(key, fromLast = TRUE), , drop = FALSE]
}

#' Shared vertical marker for the recommended sample size
#' @keywords internal
power_recommended_n_layer <- function(recommended_n) {
  if (is.null(recommended_n) || length(recommended_n) != 1L || !is.finite(recommended_n)) {
    return(list())
  }
  n <- as.numeric(recommended_n)
  list(
    ggplot2::geom_vline(
      xintercept = n,
      linetype = "solid",
      color = "#166534",
      linewidth = 0.8
    ),
    ggplot2::annotate(
      "text",
      x = n,
      y = 1,
      label = sprintf("N* = %s", format(as.integer(n), big.mark = ",")),
      hjust = -0.08,
      vjust = 1.2,
      size = 3.2,
      color = "#166534",
      fontface = "bold"
    )
  )
}

#' Evaluate power + null FDR at one or more N values and merge into curves
#' @keywords internal
power_eval_and_merge <- function(state, null_state, n_vals, n_reps, seed,
                                 conf_ids, mode, alpha, criterion,
                                 power, fdr, edge_power = NULL,
                                 progress = NULL,
                                 progress_offset = 0L, progress_total = NULL,
                                 conf_adjust = "none",
                                 min_weight = 0,
                                 latent_score_method = "own") {
  n_vals <- sort(unique(as.integer(n_vals)))
  n_vals <- n_vals[is.finite(n_vals) & n_vals >= 10L]
  if (!length(n_vals)) {
    return(list(
      power = power, fdr = fdr, edge_power = edge_power, done = progress_offset
    ))
  }
  units_half <- length(n_vals) * n_reps
  alt <- power_count_grid(
    state, n_vals, n_reps, seed, conf_ids,
    mode = mode, alpha = alpha, criterion = criterion,
    conf_adjust = conf_adjust, min_weight = min_weight,
    latent_score_method = latent_score_method,
    progress = progress,
    progress_offset = progress_offset,
    progress_total = progress_total
  )
  null_seed <- as.integer(seed + units_half + 1L)
  null <- power_count_grid(
    null_state, n_vals, n_reps, null_seed, conf_ids,
    mode = mode, alpha = alpha, criterion = criterion,
    conf_adjust = conf_adjust, min_weight = min_weight,
    latent_score_method = latent_score_method,
    progress = progress,
    progress_offset = progress_offset + units_half,
    progress_total = progress_total
  )
  list(
    power = merge_power_curve_rows(power, alt$power),
    fdr = merge_fdr_curve_rows(fdr, null$counts),
    edge_power = merge_edge_power_rows(edge_power, alt$edge_power),
    done = progress_offset + 2L * units_half,
    null_seed = null_seed
  )
}

#' Monte Carlo power + null false-detection rates for confirmatory links
#'
#' Starts from a low N and `n_max`, then repeatedly curve-fits (logistic in
#' log N) the N* that jointly reaches power ≥ 80% and P(0 false detections)
#' ≥ 95%, resamples that single N, and refits for `n_steps` iterations.
#'
#' @param n_max Maximum sample size (Simulation Samples) used as the starting
#'   high endpoint (also clamped by `n_cap`).
#' @param n_cap Hard ceiling for evaluated / recommended N (UI Max sample).
#' @param n_steps Number of curve-fit → resample cycles after the start grid.
#' @param n_grid Optional explicit starting N values (defaults to several low
#'   anchors plus `n_max`).
#' @return A list with `power` and `fdr` data frames and shared attributes.
#' @keywords internal
run_confirmatory_power <- function(state, n_max = NULL, n_reps = 100L, seed = 1L,
                                   mode = c("pvalue", "search"),
                                   alpha = 0.05,
                                   criterion = c("aic", "bic"),
                                   conf_adjust = c("holm", "none", "bonferroni", "BH"),
                                   min_weight = 0,
                                   latent_score_method = "own",
                                   progress = NULL,
                                   n_steps = 3L,
                                   n_grid = NULL,
                                   n_cap = 1000L) {
  mode <- match.arg(mode)
  criterion <- match.arg(criterion)
  conf_adjust <- match.arg(conf_adjust)
  min_weight <- as.numeric(min_weight)
  if (!is.finite(min_weight) || min_weight < 0) min_weight <- 0
  if (min_weight > 1) min_weight <- 1
  latent_score_method <- normalize_latent_score_method(latent_score_method, state)
  n_reps <- as.integer(n_reps)
  if (!is.finite(n_reps) || n_reps < 1L) {
    stop("Power replications must be at least 1.", call. = FALSE)
  }
  n_steps <- as.integer(n_steps)
  if (!is.finite(n_steps) || n_steps < 0L) n_steps <- 0L
  if (n_steps > 30L) n_steps <- 30L

  n_cap <- as.integer(n_cap)
  if (!is.finite(n_cap) || n_cap < 10L) n_cap <- 1000L

  if (is.null(n_max)) {
    if (is.null(n_grid)) stop("Provide n_max or n_grid.", call. = FALSE)
    n_max <- max(as.integer(n_grid), na.rm = TRUE)
  }
  n_max <- as.integer(n_max)
  # Never search above the hard cap (guards AIC/BIC runaway N*)
  n_max_eff <- min(n_max, n_cap)
  if (!is.finite(n_max_eff) || n_max_eff < 10L) {
    stop("Max sample / Samples must be at least 10 for power analysis.", call. = FALSE)
  }

  if (is.null(n_grid)) {
    n_grid <- power_n_bounds(n_max_eff)
  } else {
    n_grid <- as.integer(n_grid)
    n_grid <- sort(unique(n_grid[is.finite(n_grid) & n_grid >= 10L & n_grid <= n_cap]))
  }
  if (!length(n_grid)) {
    stop("No valid sample sizes for power analysis within Max sample.", call. = FALSE)
  }

  seed <- as.integer(seed)
  if (!is.finite(seed)) seed <- 1L
  if (seed < 0L) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  conf_ids <- confirmatory_power_edge_ids(state)
  if (!length(conf_ids)) {
    stop(
      "Add at least one confirmatory structural link before running power analysis.",
      call. = FALSE
    )
  }
  zero_ids <- confirmatory_zero_prior_edge_ids(state, conf_ids)
  if (length(zero_ids)) {
    labs <- confirmatory_edge_warning_labels(state, zero_ids)
    stop(
      "Set Expected effect (b ≠ 0) on confirmatory link",
      if (length(zero_ids) > 1L) "s" else "",
      " currently at 0: ",
      paste(labs, collapse = "; "),
      ". Auto-added interaction mains default to 0 until you set them.",
      call. = FALSE
    )
  }

  # Intra sim expands rows per subject; fit with (1|subject) like Simulate.
  state <- enable_repeated_measures_for_intra_sim(state)

  # Endpoints + up to n_steps single-N resamples (power + null each)
  total <- 2L * (length(n_grid) + n_steps) * n_reps
  null_state <- state_with_null_confirmatory(state, conf_ids)

  # Hot path: each replication simulates + fits. sim_debug lines would be
  # thousands of file writes (e.g. 1600 × ~4) and freeze the progress UI.
  quiet_prev <- getOption("smt.quiet", FALSE)
  options(smt.quiet = TRUE)
  on.exit(options(smt.quiet = quiet_prev), add = TRUE)

  power <- NULL
  fdr <- NULL
  edge_power <- NULL
  done <- 0L
  require_zero_null <- !identical(mode, "pvalue")
  smt_power_progress(progress, 0L, total)
  ev <- power_eval_and_merge(
    state, null_state, n_grid, n_reps, seed, conf_ids,
    mode, alpha, criterion, power, fdr, edge_power = edge_power,
    progress = progress,
    progress_offset = 0L, progress_total = total,
    conf_adjust = conf_adjust, min_weight = min_weight,
    latent_score_method = latent_score_method
  )
  power <- ev$power
  fdr <- ev$fdr
  edge_power <- ev$edge_power
  done <- ev$done
  null_seed <- ev$null_seed

  recommended_coarse_raw <- recommend_power_sample_size(
    power, fdr, require_zero_null = require_zero_null
  )
  recommended_n_raw <- recommended_coarse_raw
  refine_grid <- integer()
  n_history <- recommended_coarse_raw

  for (step in seq_len(n_steps)) {
    if (!is.finite(recommended_n_raw)) break
    n_next <- power_refine_n_point(
      recommended_n_raw, n_max = n_max_eff, hard_cap = n_cap
    )
    if (!length(n_next)) break
    # Already evaluated this exact N — stop (converged on the integer grid)
    if (n_next %in% as.integer(power$n)) break
    refine_grid <- c(refine_grid, n_next)
    step_seed <- as.integer(seed + done + 17L * step)
    ev <- power_eval_and_merge(
      state, null_state, n_next, n_reps, step_seed, conf_ids,
      mode, alpha, criterion, power, fdr, edge_power = edge_power,
      progress = progress,
      progress_offset = done, progress_total = total,
      conf_adjust = conf_adjust, min_weight = min_weight,
      latent_score_method = latent_score_method
    )
    power <- ev$power
    fdr <- ev$fdr
    edge_power <- ev$edge_power
    done <- ev$done
    recommended_n_raw <- recommend_power_sample_size(
      power, fdr, require_zero_null = require_zero_null
    )
    n_history <- c(n_history, recommended_n_raw)
  }

  # If the curve-fit N* lies beyond the cap, do not recommend that huge N —
  # report uncapped estimate separately and leave recommended_n missing.
  n_cap_hit <- is.finite(recommended_n_raw) && recommended_n_raw > n_cap
  recommended_n <- if (isTRUE(n_cap_hit)) NA_integer_ else recommended_n_raw
  recommended_coarse <- if (is.finite(recommended_coarse_raw) &&
                              recommended_coarse_raw > n_cap) {
    NA_integer_
  } else {
    recommended_coarse_raw
  }

  power <- power[order(power$n), , drop = FALSE]
  rownames(power) <- NULL
  fdr <- fdr[order(fdr$n, fdr$n_detected), , drop = FALSE]
  rownames(fdr) <- NULL
  if (!is.null(edge_power) && nrow(edge_power)) {
    edge_power <- edge_power[order(edge_power$n, edge_power$edge_id), , drop = FALSE]
    rownames(edge_power) <- NULL
    labs <- confirmatory_edge_warning_labels(state, edge_power$edge_id)
    edge_power$label <- labs
    state <- ensure_edge_priors(state)
    edge_power$prior_mean <- vapply(edge_power$edge_id, function(eid) {
      m <- edge_prior(state, eid)$mean
      if (is.finite(m)) as.numeric(m) else NA_real_
    }, numeric(1))
  }

  smt_power_progress(progress, total, total)

  meta <- list(
    mode = mode,
    alpha = as.numeric(alpha),
    criterion = criterion,
    conf_adjust = conf_adjust,
    min_weight = as.numeric(min_weight),
    latent_score_method = latent_score_method,
    require_zero_null = require_zero_null,
    seed = as.integer(seed),
    null_seed = as.integer(null_seed),
    n_confirmatory = length(conf_ids),
    conf_ids = conf_ids,
    n_cap = n_cap,
    n_max = n_max,
    n_max_eff = n_max_eff,
    n_cap_hit = isTRUE(n_cap_hit),
    recommended_n = recommended_n,
    recommended_n_raw = recommended_n_raw,
    recommended_n_coarse = recommended_coarse,
    recommended_n_history = n_history,
    refine_grid = refine_grid,
    n_steps = n_steps
  )
  for (nm in names(meta)) {
    attr(power, nm) <- meta[[nm]]
    attr(fdr, nm) <- meta[[nm]]
  }

  list(
    power = power,
    fdr = fdr,
    edge_power = edge_power,
    recommended_n = recommended_n,
    recommended_n_raw = recommended_n_raw,
    recommended_n_coarse = recommended_coarse,
    recommended_n_history = n_history,
    refine_grid = refine_grid,
    n_steps = n_steps,
    n_cap = n_cap,
    n_cap_hit = isTRUE(n_cap_hit),
    dv_residuals = power_dv_residuals_snapshot(state, conf_ids = conf_ids)
  )
}

#' ggplot power curve for confirmatory detection
#' @keywords internal
power_analysis_plot <- function(power_df, recommended_n = NULL) {
  if (is.null(power_df) || !nrow(power_df)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text", x = 0.5, y = 0.5,
          label = "Run Power Analysis to estimate detection power.",
          size = 4, color = "#4A5568"
        ) +
        ggplot2::theme_void()
    )
  }
  if (is.null(recommended_n)) recommended_n <- attr(power_df, "recommended_n")
  mode <- attr(power_df, "mode") %||% "pvalue"
  alpha <- attr(power_df, "alpha") %||% 0.05
  criterion <- attr(power_df, "criterion") %||% "bic"
  conf_adjust <- attr(power_df, "conf_adjust") %||% "none"
  min_weight <- attr(power_df, "min_weight") %||% 0
  n_reps <- power_df$n_reps[[1]]
  n_conf <- attr(power_df, "n_confirmatory") %||% NA_integer_
  rule <- if (identical(mode, "pvalue")) {
    adj_lab <- if (identical(conf_adjust, "none")) {
      sprintf("raw p < %g", alpha)
    } else {
      sprintf("adjusted p < %g [%s]", alpha, conf_adjust)
    }
    sprintf("all confirmatory links significant (%s)", adj_lab)
  } else if (is.finite(min_weight) && min_weight > 0) {
    sprintf(
      "all confirmatory links in best %s model with Σw ≥ %g",
      toupper(criterion), min_weight
    )
  } else {
    sprintf("all confirmatory links in best %s model", toupper(criterion))
  }
  subtitle <- sprintf(
    "P(%s) · %d confirmatory · %d reps / N",
    rule, as.integer(n_conf), as.integer(n_reps)
  )

  n_ok_p <- if ("n_ok" %in% names(power_df)) power_df$n_ok else NULL
  n_reps_p <- if ("n_reps" %in% names(power_df)) power_df$n_reps else NULL
  curve_df <- power_fitted_curve_df(
    power_df$n, power_df$power,
    n_ok = n_ok_p, n_reps = n_reps_p,
    n_star = recommended_n
  )

  p <- ggplot2::ggplot(power_df, ggplot2::aes(x = .data$n, y = .data$power)) +
    ggplot2::geom_hline(
      yintercept = 0.8, linetype = "dashed", color = "#9a3412", linewidth = 0.5
    )
  p <- p + power_recommended_n_layer(recommended_n)
  # Fitted curve + SE ribbon (GLM link-scale ±1.96 SE → response)
  if (!is.null(curve_df) && nrow(curve_df) >= 2L) {
    if (all(c("lower", "upper") %in% names(curve_df))) {
      p <- p + ggplot2::geom_ribbon(
        data = curve_df,
        ggplot2::aes(x = .data$n, ymin = .data$lower, ymax = .data$upper),
        inherit.aes = FALSE,
        fill = "#2E86AB",
        alpha = 0.22,
        color = NA
      )
    }
    p <- p + ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = .data$n, y = .data$power),
      color = "#2E86AB",
      linewidth = 1.1,
      linetype = "solid",
      inherit.aes = FALSE
    )
  }
  p +
    ggplot2::geom_point(color = "#2E86AB", size = 2.4) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      labels = scales_percent_safe
    ) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(c(
        as.numeric(power_df$n),
        if (is.finite(recommended_n)) as.numeric(recommended_n) else numeric()
      )))
    ) +
    ggplot2::labs(
      title = "Power to detect all confirmatory links",
      subtitle = paste0(subtitle, " · curve ±1.96 SE ribbon"),
      x = "Sample size (N)",
      y = "Power"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#4A5568"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' ggplot false-detection rates under null confirmatory effects
#'
#' One line per exact number of confirmatory links detected (0 … K).
#' @keywords internal
power_fdr_plot <- function(fdr_df, recommended_n = NULL) {
  if (is.null(fdr_df) || !nrow(fdr_df)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text", x = 0.5, y = 0.5,
          label = "Run Power Analysis to estimate false detections under null effects.",
          size = 4, color = "#4A5568"
        ) +
        ggplot2::theme_void()
    )
  }
  if (is.null(recommended_n)) recommended_n <- attr(fdr_df, "recommended_n")
  mode <- attr(fdr_df, "mode") %||% "pvalue"
  alpha <- attr(fdr_df, "alpha") %||% 0.05
  criterion <- attr(fdr_df, "criterion") %||% "bic"
  conf_adjust <- attr(fdr_df, "conf_adjust") %||% "none"
  min_weight <- attr(fdr_df, "min_weight") %||% 0
  n_reps <- fdr_df$n_reps[[1]]
  rule <- if (identical(mode, "pvalue")) {
    adj_lab <- if (identical(conf_adjust, "none")) {
      sprintf("raw p < %g", alpha)
    } else {
      sprintf("adjusted p < %g [%s]", alpha, conf_adjust)
    }
    sprintf("detected if %s", adj_lab)
  } else if (is.finite(min_weight) && min_weight > 0) {
    sprintf("in best %s · Σw ≥ %g", toupper(criterion), min_weight)
  } else {
    sprintf("kept in best %s model", toupper(criterion))
  }
  subtitle <- sprintf(
    "Confirmatory expected effects = 0 · P(exactly k detected | %s) · %d reps / N · curve ±1.96 SE ribbon",
    rule, as.integer(n_reps)
  )

  plot_df <- fdr_df
  k_levels <- sort(unique(plot_df$n_detected))
  k_labels <- sprintf("%d detected", k_levels)
  plot_df$n_detected_f <- factor(
    plot_df$n_detected,
    levels = k_levels,
    labels = k_labels
  )

  curve_df <- fdr_fitted_curves_df(fdr_df, recommended_n = recommended_n)
  if (!is.null(curve_df) && nrow(curve_df)) {
    curve_df$n_detected_f <- factor(
      curve_df$n_detected,
      levels = k_levels,
      labels = k_labels
    )
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$n,
      y = .data$rate,
      color = .data$n_detected_f,
      fill = .data$n_detected_f,
      group = .data$n_detected_f
    )
  ) +
    ggplot2::geom_hline(
      yintercept = c(0.05, 0.95),
      linetype = "dashed",
      color = "#9a3412",
      linewidth = 0.5
    )
  p <- p + power_recommended_n_layer(recommended_n)
  if (!is.null(curve_df) && nrow(curve_df) >= 2L) {
    if (all(c("lower", "upper") %in% names(curve_df))) {
      # Skip collapsed / full-plot bands (zero width after SE sanitizing)
      ribbon_df <- curve_df[
        is.finite(curve_df$lower) & is.finite(curve_df$upper) &
          (curve_df$upper - curve_df$lower) > 1e-4,
        , drop = FALSE
      ]
      if (nrow(ribbon_df)) {
        p <- p + ggplot2::geom_ribbon(
          data = ribbon_df,
          ggplot2::aes(
            x = .data$n,
            ymin = .data$lower,
            ymax = .data$upper,
            fill = .data$n_detected_f,
            group = .data$n_detected_f
          ),
          alpha = 0.18,
          color = NA,
          show.legend = FALSE
        )
      }
    }
    p <- p + ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(
        x = .data$n,
        y = .data$rate,
        color = .data$n_detected_f,
        group = .data$n_detected_f
      ),
      linewidth = 1.05,
      show.legend = FALSE
    )
  }
  p +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      labels = scales_percent_safe
    ) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(c(
        as.numeric(plot_df$n),
        if (is.finite(recommended_n)) as.numeric(recommended_n) else numeric()
      )))
    ) +
    ggplot2::labs(
      title = "False detections under null confirmatory effects",
      subtitle = subtitle,
      x = "Sample size (N)",
      y = "Rate",
      color = NULL,
      fill = NULL
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#4A5568"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Percent labels without requiring scales package
#' @keywords internal
scales_percent_safe <- function(x) {
  paste0(formatC(100 * x, digits = 0, format = "f"), "%")
}

#' Per-link curve N* (power ≥ threshold) and curve power at a reference N
#'
#' `n_ref` defaults to joint recommended N* when available, else the largest
#' evaluated sample size. Rates and N* always come from the logistic-in-log(N)
#' curve (same family as the joint N*).
#' @keywords internal
smt_power_edge_summary <- function(pw, threshold = 0.8, n_ref = NULL) {
  ep <- pw$edge_power %||% NULL
  if (is.null(ep) || !nrow(ep)) return(NULL)
  threshold <- as.numeric(threshold)
  if (!is.finite(threshold) || threshold <= 0 || threshold >= 1) threshold <- 0.8

  if (is.null(n_ref) || !is.finite(n_ref)) {
    n_ref <- pw$recommended_n %||% attr(pw$power, "recommended_n")
  }
  if (is.null(n_ref) || !is.finite(n_ref)) {
    n_ref <- max(as.numeric(ep$n), na.rm = TRUE)
  }
  n_ref <- as.numeric(n_ref)
  n_cap <- pw$n_cap %||% attr(pw$power, "n_cap")

  ids <- unique(as.character(ep$edge_id))
  rows <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    eid <- ids[[i]]
    sub <- ep[as.character(ep$edge_id) == eid, , drop = FALSE]
    if (!nrow(sub)) next
    n_ok <- if ("n_ok" %in% names(sub)) sub$n_ok else NULL
    n_reps <- if ("n_reps" %in% names(sub)) sub$n_reps else NULL
    n_star_raw <- fit_rate_threshold_n(
      sub$n, sub$power, threshold,
      n_ok = n_ok, n_reps = n_reps
    )
    n_star <- if (is.finite(n_star_raw)) {
      as.integer(max(10L, as.integer(ceiling(n_star_raw - 1e-9))))
    } else {
      NA_integer_
    }
    fit_obj <- fit_rate_logn_glm(
      sub$n, sub$power, n_ok = n_ok, n_reps = n_reps
    )
    power_at <- predict_rate_logn(fit_obj, n_ref)
    if (!is.finite(power_at) && nrow(sub) >= 2L) {
      power_at <- as.numeric(stats::approx(
        sub$n, sub$power, xout = n_ref, rule = 2
      )$y)
    }
    lab <- if ("label" %in% names(sub) && any(nzchar(as.character(sub$label)))) {
      as.character(sub$label[[1]])
    } else {
      eid
    }
    pm <- if ("prior_mean" %in% names(sub)) as.numeric(sub$prior_mean[[1]]) else NA_real_
    rows[[i]] <- data.frame(
      edge_id = eid,
      label = lab,
      prior_mean = pm,
      n_ref = n_ref,
      power = as.numeric(power_at),
      n_star = n_star,
      n_star_raw = if (is.finite(n_star_raw)) as.numeric(n_star_raw) else NA_real_,
      n_star_capped = is.finite(n_star) && is.finite(n_cap) && n_star > n_cap,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(NULL)
  # Hardest first: missing / huge N*, then low power, then label
  n_star_ord <- ifelse(is.finite(out$n_star), out$n_star, .Machine$integer.max)
  out <- out[order(-n_star_ord, out$power, out$label), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "n_ref") <- n_ref
  attr(out, "threshold") <- threshold
  attr(out, "n_cap") <- n_cap
  out
}

#' Per-edge detection rates at one N (default: largest evaluated)
#'
#' Kept for callers that want the raw Monte Carlo slice; UI prefers
#' [smt_power_edge_summary()].
#' @keywords internal
smt_power_edge_rates_at_n <- function(pw, n = NULL) {
  ep <- pw$edge_power %||% NULL
  if (is.null(ep) || !nrow(ep)) return(NULL)
  if (is.null(n) || !is.finite(n)) {
    n <- max(as.numeric(ep$n), na.rm = TRUE)
  }
  n <- as.integer(n)
  ns <- sort(unique(as.integer(ep$n)))
  if (!length(ns)) return(NULL)
  if (!(n %in% ns)) {
    below <- ns[ns <= n]
    n <- if (length(below)) max(below) else ns[[which.min(abs(ns - n))]]
  }
  sub <- ep[as.integer(ep$n) == n, , drop = FALSE]
  if (!nrow(sub)) return(NULL)
  lab_ord <- if ("label" %in% names(sub)) {
    as.character(sub$label)
  } else {
    as.character(sub$edge_id)
  }
  sub <- sub[order(sub$power, lab_ord), , drop = FALSE]
  rownames(sub) <- NULL
  attr(sub, "n") <- n
  sub
}

#' Links with per-edge curve power below a threshold at the reference N
#' @keywords internal
smt_power_hard_edges <- function(pw, threshold = 0.8, n = NULL) {
  sub <- smt_power_edge_summary(pw, threshold = threshold, n_ref = n)
  if (is.null(sub) || !nrow(sub)) return(NULL)
  hard <- sub[is.finite(sub$power) & sub$power < threshold, , drop = FALSE]
  if (!nrow(hard)) return(NULL)
  attr(hard, "n") <- attr(sub, "n_ref")
  attr(hard, "threshold") <- threshold
  hard
}

#' Format a per-link N* for display
#' @keywords internal
smt_format_edge_nstar <- function(n_star, n_cap = NULL, capped = FALSE) {
  if (isTRUE(capped) || (is.finite(n_star) && is.finite(n_cap) && n_star > n_cap)) {
    return(sprintf(
      "N* > %s",
      format(as.integer(n_cap), big.mark = ",")
    ))
  }
  if (!is.finite(n_star)) return("N* —")
  sprintf("N* = %s", format(as.integer(n_star), big.mark = ","))
}

#' UI listing per-edge curve power + per-link N*
#' @keywords internal
smt_power_edge_rates_ui <- function(pw, compact = FALSE, threshold = 0.8) {
  sub <- smt_power_edge_summary(pw, threshold = threshold)
  if (is.null(sub) || !nrow(sub)) return(NULL)
  n_ref <- attr(sub, "n_ref") %||% sub$n_ref[[1]]
  n_cap <- attr(sub, "n_cap") %||% pw$n_cap
  hard <- sub[
    (is.finite(sub$power) & sub$power < threshold) |
      !is.finite(sub$n_star) |
      isTRUE(sub$n_star_capped),
    , drop = FALSE
  ]
  show <- if (isTRUE(compact)) {
    if (nrow(hard)) hard else utils::head(sub, 3L)
  } else {
    sub
  }
  items <- lapply(seq_len(nrow(show)), function(i) {
    row <- show[i, , drop = FALSE]
    lab <- row$label[[1]]
    if (is.null(lab) || is.na(lab) || !nzchar(as.character(lab))) {
      lab <- as.character(row$edge_id[[1]])
    }
    pct <- if (is.finite(row$power[[1]])) {
      sprintf("%.0f%%", 100 * row$power[[1]])
    } else {
      "?"
    }
    nstar_txt <- smt_format_edge_nstar(
      row$n_star[[1]], n_cap = n_cap, capped = isTRUE(row$n_star_capped[[1]])
    )
    b_txt <- if ("prior_mean" %in% names(row) && is.finite(row$prior_mean[[1]])) {
      sprintf("b = %.2f", row$prior_mean[[1]])
    } else {
      NULL
    }
    is_hard <- (is.finite(row$power[[1]]) && row$power[[1]] < threshold) ||
      !is.finite(row$n_star[[1]]) ||
      isTRUE(row$n_star_capped[[1]])
    shiny::tags$li(
      class = if (is_hard) "smt-power-edge-hard" else NULL,
      shiny::span(class = "smt-power-target-lab", lab),
      shiny::span(class = "smt-power-edge-pct", pct),
      shiny::span(class = "smt-power-edge-nstar", nstar_txt),
      if (!is.null(b_txt)) shiny::span(class = "smt-power-target-b", b_txt)
    )
  })

  head_txt <- if (nrow(hard)) {
    sprintf(
      "Per-link power at N = %s · %d / %d need more N for %.0f%%",
      format(as.integer(round(n_ref)), big.mark = ","),
      nrow(hard),
      nrow(sub),
      100 * threshold
    )
  } else {
    sprintf(
      "Per-link power at N = %s (all ≥ %.0f%%)",
      format(as.integer(round(n_ref)), big.mark = ","),
      100 * threshold
    )
  }

  if (isTRUE(compact)) {
    return(shiny::div(
      class = "smt-power-edge-rates smt-power-edge-rates-compact",
      shiny::div(
        class = "smt-power-targets-head",
        shiny::tags$strong(if (nrow(hard)) {
          sprintf("%d hard link%s", nrow(hard), if (nrow(hard) == 1L) "" else "s")
        } else {
          "Links detectable"
        }),
        shiny::span(
          class = "smt-muted",
          sprintf(" at N = %s", format(as.integer(round(n_ref)), big.mark = ","))
        )
      ),
      shiny::tags$ul(class = "smt-power-targets-list", items)
    ))
  }

  shiny::div(
    class = paste(
      "smt-power-edge-rates",
      if (nrow(hard)) "smt-power-edge-rates-hard" else NULL
    ),
    shiny::div(
      class = "smt-power-targets-head",
      shiny::tags$strong(head_txt)
    ),
    shiny::tags$ul(class = "smt-power-targets-list", items),
    shiny::div(
      class = "smt-muted smt-power-edge-hint",
      "Power % and per-link N* from the fitted curves (power ≥ 80% per link).",
      if (nrow(hard)) {
        " Raise expected |b|, reduce residual noise, or increase Max sample / Samples."
      }
    )
  )
}

#' UI listing DV generative residual SDs for power
#'
#' Prefers a run snapshot (`pw$dv_residuals`); otherwise builds from `state`.
#' @keywords internal
smt_power_dv_resid_ui <- function(pw = NULL, state = NULL, compact = FALSE) {
  df <- NULL
  if (!is.null(pw) && !is.null(pw$dv_residuals) &&
        is.data.frame(pw$dv_residuals) && nrow(pw$dv_residuals)) {
    df <- pw$dv_residuals
  } else if (!is.null(state)) {
    df <- power_dv_residuals_snapshot(state)
  }
  if (is.null(df) || !nrow(df)) return(NULL)

  items <- lapply(seq_len(nrow(df)), function(i) {
    lab <- df$label[[i]]
    if (is.null(lab) || is.na(lab) || !nzchar(as.character(lab))) {
      lab <- as.character(df$id[[i]])
    }
    kind <- as.character(df$kind[[i]] %||% "continuous")
    sd_val <- as.numeric(df$resid_sd[[i]])
    if (identical(kind, "binary")) {
      sd_txt <- "SD(\u03b5) unused (logistic)"
    } else if (identical(kind, "categorical")) {
      sd_txt <- "SD(\u03b5) unused (categorical)"
    } else if (is.finite(sd_val)) {
      sd_txt <- sprintf("SD(\u03b5) = %.2f", sd_val)
    } else {
      sd_txt <- "SD(\u03b5) unset"
    }
    shiny::tags$li(
      shiny::span(class = "smt-power-target-lab", lab),
      shiny::span(class = "smt-power-target-b", sd_txt)
    )
  })

  head_lab <- if (isTRUE(compact)) {
    "DV residual SD"
  } else {
    sprintf("DV residual SD (%d)", nrow(df))
  }
  shiny::div(
    class = paste(
      "smt-power-targets smt-power-dv-resid",
      if (isTRUE(compact)) "smt-power-dv-resid-compact" else NULL
    ),
    shiny::div(
      class = "smt-power-targets-head",
      shiny::tags$strong(head_lab)
    ),
    shiny::tags$ul(class = "smt-power-targets-list", items),
    if (!isTRUE(compact)) {
      shiny::div(
        class = "smt-muted smt-power-edge-hint",
        "Generative residual noise on confirmatory-target DVs (inspector SD(\u03b5))."
      )
    }
  )
}

#' Recommend / miss message for a power result
#' @keywords internal
smt_power_recommend_ui <- function(pw, compact = FALSE) {
  if (is.null(pw) || is.null(pw$power) || !nrow(pw$power)) return(NULL)
  rec <- pw$recommended_n
  use_fdr <- isTRUE(attr(pw$power, "require_zero_null"))
  rule_txt <- if (use_fdr) {
    "power ≥ 80% and P(0 false detections) ≥ 95%"
  } else {
    "power ≥ 80% (false-detection rate ignored for N* under p-values)"
  }
  n_cap <- pw$n_cap %||% attr(pw$power, "n_cap")
  edge_ui <- smt_power_edge_rates_ui(pw, compact = compact)
  resid_ui <- smt_power_dv_resid_ui(pw = pw, compact = compact)
  if (!is.null(rec) && length(rec) == 1L && is.finite(rec)) {
    metrics <- format_power_metrics_at_n(pw$power, pw$fdr, rec)
    detail <- if (nzchar(metrics)) paste0(" — ", metrics, ".") else ""
    if (isTRUE(compact)) {
      return(shiny::tagList(
        shiny::div(
          class = "smt-power-recommend",
          shiny::tags$strong(sprintf("N* = %d", as.integer(rec))),
          shiny::span(if (nzchar(metrics)) paste0(" · ", metrics) else ""),
          shiny::span(class = "smt-muted", " · open Run power… for plots")
        ),
        edge_ui,
        resid_ui
      ))
    }
    return(shiny::tagList(
      shiny::div(
        class = "smt-power-recommend",
        shiny::tags$strong(sprintf("Recommended N* = %d", as.integer(rec))),
        shiny::span(detail)
      ),
      edge_ui,
      resid_ui
    ))
  }
  if (isTRUE(pw$n_cap_hit) || isTRUE(attr(pw$power, "n_cap_hit"))) {
    raw <- pw$recommended_n_raw %||% attr(pw$power, "recommended_n_raw")
    msg <- sprintf(
      " Curve-fit estimate ≈ %s but search is capped at %s.",
      if (is.finite(raw)) format(as.integer(raw), big.mark = ",") else "?",
      if (is.finite(n_cap)) format(as.integer(n_cap), big.mark = ",") else "the cap"
    )
    if (isTRUE(compact)) {
      return(shiny::tagList(
        shiny::div(
          class = "smt-power-recommend smt-power-recommend-miss",
          shiny::tags$strong("N* exceeds Max sample"),
          shiny::span(class = "smt-muted", " · open Run power… for details")
        ),
        edge_ui,
        resid_ui
      ))
    }
    return(shiny::tagList(
      shiny::div(
        class = "smt-power-recommend smt-power-recommend-miss",
        shiny::tags$strong("N* exceeds Max sample."),
        shiny::span(paste0(msg, " Raise Max sample or effect sizes for ", rule_txt, "."))
      ),
      edge_ui,
      resid_ui
    ))
  }
  if (isTRUE(compact)) {
    return(shiny::tagList(
      shiny::div(
        class = "smt-power-recommend smt-power-recommend-miss",
        shiny::tags$strong("No recommended N"),
        shiny::span(class = "smt-muted", " · open Run power… for details")
      ),
      edge_ui,
      resid_ui
    ))
  }
  shiny::tagList(
    shiny::div(
      class = "smt-power-recommend smt-power-recommend-miss",
      shiny::tags$strong("No recommended N."),
      shiny::span(sprintf(
        " Raise Max sample, Samples, or effect sizes until the curves reach %s.",
        rule_txt
      ))
    ),
    edge_ui,
    resid_ui
  )
}

#' Summary list of confirmatory edges used as power targets
#' @keywords internal
smt_power_targets_ui <- function(state) {
  if (is.null(state)) {
    return(shiny::div(
      class = "smt-power-targets smt-power-targets-empty",
      "No model loaded."
    ))
  }
  state <- ensure_edge_priors(state)
  conf_ids <- confirmatory_power_edge_ids(state)
  if (!length(conf_ids)) {
    return(shiny::div(
      class = "smt-power-targets smt-power-targets-empty",
      shiny::tags$strong("No confirmatory structural links."),
      shiny::span(" Add at least one confirmatory path before running power.")
    ))
  }
  zero_ids <- confirmatory_zero_prior_edge_ids(state, conf_ids)
  labs <- confirmatory_edge_warning_labels(state, conf_ids)
  items <- lapply(seq_along(conf_ids), function(i) {
    eid <- conf_ids[[i]]
    pr <- edge_prior(state, eid)
    m <- pr$mean
    s <- pr$sd
    is_zero <- eid %in% zero_ids
    b_txt <- if (is.finite(m) && is.finite(s)) {
      sprintf("expected b = %.2f \u00b1 %.2f", m, s)
    } else if (is.finite(m)) {
      sprintf("expected b = %.2f", m)
    } else {
      "expected b unset"
    }
    shiny::tags$li(
      class = if (is_zero) "smt-power-target-zero" else NULL,
      shiny::span(class = "smt-power-target-lab", labs[[i]]),
      shiny::span(class = "smt-power-target-b", b_txt),
      if (is_zero) {
        shiny::span(class = "smt-power-target-warn", "needs b \u2260 0")
      }
    )
  })
  shiny::div(
    class = "smt-power-targets",
    shiny::div(
      class = "smt-power-targets-head",
      shiny::tags$strong(
        sprintf(
          "Confirmatory targets (%d)",
          length(conf_ids)
        )
      ),
      if (length(zero_ids)) {
        shiny::span(
          class = "smt-power-targets-warn",
          sprintf(" · %d with b = 0", length(zero_ids))
        )
      }
    ),
    shiny::tags$ul(class = "smt-power-targets-list", items)
  )
}

#' Power analysis configure / run / results modal
#' @keywords internal
smt_power_modal <- function(reps = 100L,
                            steps = 3L,
                            n_cap = 1000L,
                            sim_n = 500L,
                            targets_ui = NULL,
                            dv_resid_ui = NULL) {
  reps <- as.integer(reps %||% 100L)
  steps <- as.integer(steps %||% 3L)
  n_cap <- as.integer(n_cap %||% 1000L)
  sim_n <- as.integer(sim_n %||% 500L)
  if (!is.finite(reps) || reps < 10L) reps <- 100L
  if (!is.finite(steps) || steps < 0L) steps <- 3L
  if (!is.finite(n_cap) || n_cap < 10L) n_cap <- 1000L
  if (!is.finite(sim_n) || sim_n < 10L) sim_n <- 500L

  shiny::modalDialog(
    title = NULL,
    easyClose = TRUE,
    fade = TRUE,
    size = "l",
    footer = shiny::tagList(
      shiny::actionButton(
        "cancel_power_analysis",
        "Cancel",
        class = "smt-btn smt-btn-danger",
        title = "Stop after the current replication"
      ),
      shiny::modalButton("Close")
    ),
    shiny::div(
      class = "smt-power-modal",
      shiny::div(
        class = "smt-power-modal-hero",
        shiny::div(class = "smt-welcome-kicker", "Simulation"),
        shiny::h3("Power analysis"),
        shiny::p(
          class = "smt-power-modal-lead",
          sprintf(
            "Monte Carlo detection curve for confirmatory links. Uses Samples = %s from the Simulation panel (seed, expected effects, residual SD).",
            format(sim_n, big.mark = ",")
          )
        )
      ),
      targets_ui %||% NULL,
      dv_resid_ui %||% NULL,
      shiny::div(
        class = "smt-power-modal-fields",
        shiny::numericInput(
          "power_reps",
          "Replications",
          value = reps,
          min = 10,
          max = 500,
          step = 10,
          width = "100%"
        ),
        shiny::numericInput(
          "power_interp_steps",
          "Interpolation steps",
          value = steps,
          min = 0,
          max = 20,
          step = 1,
          width = "100%"
        ),
        shiny::numericInput(
          "power_max_n",
          "Max sample (N cap)",
          value = n_cap,
          min = 10,
          max = 100000,
          step = 50,
          width = "100%"
        )
      ),
      shiny::helpText(
        "Search evaluates low-N anchors plus min(Samples, Max sample), then curve-fits N* ",
        "and resamples that point. Evaluations never exceed Max sample."
      ),
      shiny::div(
        class = "smt-btn-row",
        shiny::actionButton(
          "run_power_analysis",
          "Run simulations",
          class = "smt-btn smt-btn-primary"
        )
      ),
      shiny::uiOutput("power_modal_status"),
      shiny::uiOutput("power_recommend_msg"),
      shiny::conditionalPanel(
        condition = "output.power_has_plot === true",
        shiny::div(
          class = "smt-power-plot-wrap",
          shiny::plotOutput("power_plot", height = "260px")
        ),
        shiny::div(
          class = "smt-power-plot-wrap",
          shiny::plotOutput("power_fdr_plot", height = "260px")
        )
      )
    )
  )
}
