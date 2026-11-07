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
#' @keywords internal
confirmatory_power_edge_ids <- function(state) {
  analyzable_edge_ids(
    state,
    state$edges$id[state$edges$type == "confirmatory"]
  )
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

#' Count confirmatory links detected under a p-value fit
#' @keywords internal
confirmatory_detection_count_pvalue <- function(fit_res, conf_ids, alpha = 0.05) {
  if (!length(conf_ids)) return(0L)
  if (is.null(fit_res) || is.null(fit_res$state) || !nrow(fit_res$state$edges)) {
    return(0L)
  }
  edges <- fit_res$state$edges
  n_hit <- 0L
  for (eid in conf_ids) {
    idx <- match(eid, edges$id)
    if (is.na(idx)) next
    pv <- edges$pvalue[[idx]]
    if (is.finite(pv) && pv < alpha) n_hit <- n_hit + 1L
  }
  n_hit
}

#' Count confirmatory links included in the AIC/BIC best model
#'
#' When `min_weight` > 0, an included confirmatory link also needs
#' Σw ≥ `min_weight` (same rejection rule as the canvas).
#' @keywords internal
confirmatory_detection_count_search <- function(fit_res, conf_ids,
                                               min_weight = 0) {
  if (!length(conf_ids) || is.null(fit_res)) return(0L)
  best <- fit_res$best_exploratory
  if (is.null(best)) best <- character()
  always <- fit_res$always %||% character()
  included <- unique(c(as.character(always), as.character(best)))
  min_weight <- as.numeric(min_weight)
  if (!is.finite(min_weight) || min_weight <= 0) {
    return(as.integer(sum(conf_ids %in% included)))
  }
  wsums <- fit_res$edge_weights %||% list()
  n_hit <- 0L
  for (eid in conf_ids) {
    if (!(eid %in% included)) next
    ws <- if (eid %in% names(wsums)) as.numeric(wsums[[eid]]) else 0
    if (is.finite(ws) && ws >= min_weight) n_hit <- n_hit + 1L
  }
  as.integer(n_hit)
}

#' One Monte Carlo replication: simulate, fit, count confirmatory detections
#'
#' Fit/simulate errors count as 0 detections.
#' @keywords internal
power_one_replication_count <- function(state, n, seed, conf_ids,
                                        mode = c("pvalue", "search"),
                                        alpha = 0.05,
                                        criterion = c("aic", "bic"),
                                        conf_adjust = c("none", "bonferroni", "holm", "BH"),
                                        min_weight = 0) {
  mode <- match.arg(mode)
  criterion <- match.arg(criterion)
  conf_adjust <- match.arg(conf_adjust)
  min_weight <- as.numeric(min_weight)
  if (!is.finite(min_weight) || min_weight < 0) min_weight <- 0
  n_hit <- tryCatch({
    sim <- simulate_path_data(state, n = as.integer(n), seed = as.integer(seed))
    fit_st <- bind_sim_columns_to_state(state, sim)
    res <- if (identical(mode, "pvalue")) {
      fit_pvalue_model(
        fit_st, sim,
        alpha = alpha,
        mark_nonsig = FALSE,
        conf_adjust = conf_adjust,
        uncertainty = "none"
      )
    } else {
      fit_aic_bic_search(
        fit_st, sim,
        criterion = criterion,
        uncertainty = "none",
        min_weight = min_weight,
        mark_low_weight = is.finite(min_weight) && min_weight > 0
      )
    }
    if (identical(mode, "pvalue")) {
      confirmatory_detection_count_pvalue(res, conf_ids, alpha = alpha)
    } else {
      confirmatory_detection_count_search(res, conf_ids, min_weight = min_weight)
    }
  }, error = function(e) 0L)
  as.integer(n_hit)
}

#' Normal-approximation CI for a Monte Carlo binomial rate
#' @keywords internal
binomial_rate_band <- function(n_ok, n_reps, z = 1.96) {
  n_ok <- as.numeric(n_ok)
  n_reps <- as.numeric(n_reps)
  p <- ifelse(n_reps > 0, n_ok / n_reps, NA_real_)
  se <- ifelse(n_reps > 0, sqrt(pmax(0, p * (1 - p) / n_reps)), NA_real_)
  list(
    rate = p,
    se = se,
    lower = pmax(0, p - z * se),
    upper = pmin(1, p + z * se)
  )
}

#' Run detection counts over an N × replication grid
#' @keywords internal
power_count_grid <- function(state, n_grid, n_reps, seed, conf_ids,
                             mode, alpha, criterion, conf_adjust = "none",
                             min_weight = 0,
                             progress = NULL,
                             progress_offset = 0L, progress_total = NULL) {
  n_conf <- length(conf_ids)
  rows_power <- vector("list", length(n_grid))
  rows_fdr <- vector("list", length(n_grid) * (n_conf + 1L))
  fdr_i <- 0L
  done <- as.integer(progress_offset)
  if (is.null(progress_total)) {
    progress_total <- progress_offset + length(n_grid) * n_reps
  }

  for (i in seq_along(n_grid)) {
    n <- n_grid[[i]]
    counts <- integer(n_reps)
    for (r in seq_len(n_reps)) {
      rep_seed <- as.integer(seed + (i - 1L) * n_reps + r)
      counts[[r]] <- power_one_replication_count(
        state, n = n, seed = rep_seed, conf_ids = conf_ids,
        mode = mode, alpha = alpha, criterion = criterion,
        conf_adjust = conf_adjust, min_weight = min_weight
      )
      done <- done + 1L
      if (is.function(progress)) {
        try(progress(done, progress_total), silent = TRUE)
      }
    }
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

  list(
    power = do.call(rbind, rows_power),
    counts = do.call(rbind, rows_fdr[seq_len(fdr_i)])
  )
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
  out <- data.frame(n = n_seq, stringsAsFactors = FALSE)
  out[[y_name]] <- predict_rate_logn(fit_obj, n_seq)
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
#' Uses the Monte Carlo point when `n` was evaluated; otherwise predicts from
#' the same logistic-in-log(N) curve used for N*.
#' @keywords internal
power_metrics_at_n <- function(power_df, fdr_df, n) {
  n <- as.numeric(n)
  out <- list(power = NA_real_, fdr_ge1 = NA_real_)
  if (!is.finite(n) || n < 1) return(out)

  rate_at <- function(n_vec, rate, n_ok = NULL, n_reps = NULL, n_query) {
    n_vec <- as.numeric(n_vec)
    rate <- as.numeric(rate)
    hit <- which(as.integer(round(n_vec)) == as.integer(round(n_query)))
    if (length(hit)) return(as.numeric(rate[[hit[[1]]]]))
    fit_obj <- fit_rate_logn_glm(n_vec, rate, n_ok = n_ok, n_reps = n_reps)
    pred <- predict_rate_logn(fit_obj, n_query)
    if (is.finite(pred)) return(as.numeric(pred))
    if (length(n_vec) >= 2L) {
      return(as.numeric(stats::approx(n_vec, rate, xout = n_query, rule = 2)$y))
    }
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
                                 power, fdr, progress = NULL,
                                 progress_offset = 0L, progress_total = NULL,
                                 conf_adjust = "none",
                                 min_weight = 0) {
  n_vals <- sort(unique(as.integer(n_vals)))
  n_vals <- n_vals[is.finite(n_vals) & n_vals >= 10L]
  if (!length(n_vals)) {
    return(list(power = power, fdr = fdr, done = progress_offset))
  }
  units_half <- length(n_vals) * n_reps
  alt <- power_count_grid(
    state, n_vals, n_reps, seed, conf_ids,
    mode = mode, alpha = alpha, criterion = criterion,
    conf_adjust = conf_adjust, min_weight = min_weight,
    progress = progress,
    progress_offset = progress_offset,
    progress_total = progress_total
  )
  null_seed <- as.integer(seed + units_half + 1L)
  null <- power_count_grid(
    null_state, n_vals, n_reps, null_seed, conf_ids,
    mode = mode, alpha = alpha, criterion = criterion,
    conf_adjust = conf_adjust, min_weight = min_weight,
    progress = progress,
    progress_offset = progress_offset + units_half,
    progress_total = progress_total
  )
  list(
    power = merge_power_curve_rows(power, alt$power),
    fdr = merge_fdr_curve_rows(fdr, null$counts),
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
                                   conf_adjust = c("none", "bonferroni", "holm", "BH"),
                                   min_weight = 0,
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

  # Endpoints + up to n_steps single-N resamples (power + null each)
  total <- 2L * (length(n_grid) + n_steps) * n_reps
  null_state <- state_with_null_confirmatory(state, conf_ids)

  power <- NULL
  fdr <- NULL
  done <- 0L
  require_zero_null <- !identical(mode, "pvalue")
  ev <- power_eval_and_merge(
    state, null_state, n_grid, n_reps, seed, conf_ids,
    mode, alpha, criterion, power, fdr, progress,
    progress_offset = 0L, progress_total = total,
    conf_adjust = conf_adjust, min_weight = min_weight
  )
  power <- ev$power
  fdr <- ev$fdr
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
      mode, alpha, criterion, power, fdr, progress,
      progress_offset = done, progress_total = total,
      conf_adjust = conf_adjust, min_weight = min_weight
    )
    power <- ev$power
    fdr <- ev$fdr
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

  if (is.function(progress)) {
    try(progress(total, total), silent = TRUE)
  }

  meta <- list(
    mode = mode,
    alpha = as.numeric(alpha),
    criterion = criterion,
    conf_adjust = conf_adjust,
    min_weight = as.numeric(min_weight),
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
    recommended_n = recommended_n,
    recommended_n_raw = recommended_n_raw,
    recommended_n_coarse = recommended_coarse,
    recommended_n_history = n_history,
    refine_grid = refine_grid,
    n_steps = n_steps,
    n_cap = n_cap,
    n_cap_hit = isTRUE(n_cap_hit)
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
    "P(%s) · %d confirmatory · %d reps / N · ribbon ±1.96 SE",
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
  # Fitted curve only (no empirical polyline). Uncertainty as per-N intervals
  # so sparse Monte Carlo points do not draw polygonal “segment” bands.
  if (!is.null(curve_df) && nrow(curve_df) >= 2L) {
    p <- p + ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = .data$n, y = .data$power),
      color = "#2E86AB",
      linewidth = 1.1,
      linetype = "solid",
      inherit.aes = FALSE
    )
  }
  if (all(c("lower", "upper") %in% names(power_df))) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0,
      color = "#2E86AB",
      alpha = 0.55,
      linewidth = 0.55
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
      subtitle = paste0(subtitle, " · curve = logistic(log N)"),
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
  min_weight <- attr(fdr_df, "min_weight") %||% 0
  n_reps <- fdr_df$n_reps[[1]]
  rule <- if (identical(mode, "pvalue")) {
    sprintf("significant at p < %g", alpha)
  } else if (is.finite(min_weight) && min_weight > 0) {
    sprintf("in best %s · Σw ≥ %g", toupper(criterion), min_weight)
  } else {
    sprintf("kept in best %s model", toupper(criterion))
  }
  subtitle <- sprintf(
    "Confirmatory expected effects = 0 · P(exactly k detected | %s) · %d reps / N · ribbon ±1.96 SE",
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
  if (all(c("lower", "upper") %in% names(plot_df))) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0,
      alpha = 0.55,
      linewidth = 0.55,
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
      subtitle = paste0(subtitle, " · curves = logistic(log N)"),
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
