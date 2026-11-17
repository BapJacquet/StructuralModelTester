#' Resolve posterior draws + prior for one selected structural edge
#'
#' Defaults to the linear term (`degree = 1`). Returns NULL when the result is
#' not a Bayesian path fit or the edge has no matching coefficient.
#' @keywords internal
edge_bayes_param_info <- function(result, state, edge_id, degree = 1L) {
  if (is.null(result) || !identical(result$sampler, "brms")) return(NULL)
  if (!identical(result$engine, "path") || !is.list(result$fit)) return(NULL)
  state <- ensure_edge_to_port(state)
  idx <- match(as.character(edge_id), state$edges$id)
  if (is.na(idx)) return(NULL)
  from_id <- state$edges$from[[idx]]
  to_id <- state$edges$to[[idx]]
  if (identical(state$edges$type[[idx]], "definitional") ||
        is_interaction_node(state, to_id)) {
    return(NULL)
  }
  to_level <- if ("to_level" %in% names(state$edges)) {
    state$edges$to_level[[idx]]
  } else {
    NA_character_
  }
  fit <- edge_fit_for_plot(result, to_id, to_level = to_level)
  if (is.null(fit) || !inherits(fit, "brmsfit")) return(NULL)

  degree <- as.integer(degree)
  if (!is.finite(degree) || degree < 1L) degree <- 1L
  from_nm <- tryCatch(lavaan_var_name(state, from_id), error = function(e) NA_character_)
  if (is.na(from_nm) || !nzchar(from_nm)) return(NULL)
  term <- if (degree <= 1L) from_nm else poly_col_name(from_nm, degree)
  # Fall back to any estimate-row term for this edge
  if (!is.null(result$estimates) && nrow(result$estimates)) {
    er <- result$estimates
    hit <- er$op == "~" & er$rhs == from_nm & er$kind == "effect"
    if ("level" %in% names(er) && degree >= 2L) {
      hit <- hit & er$level == poly_level_tag(degree)
    } else if ("level" %in% names(er)) {
      hit <- hit & (is.na(er$level) | !nzchar(as.character(er$level)) |
                      !vapply(er$level, is_poly_level, logical(1)))
    }
    if (any(hit) && "term" %in% names(er)) {
      tn <- as.character(er$term[which(hit)[[1]]])
      if (nzchar(tn) && !is.na(tn)) term <- tn
    }
  }
  draws <- brms_term_draws(fit, term)
  pr <- edge_prior(state, edge_id)
  prior_mean <- if (degree <= 1L) pr$mean else if (degree == 2L) pr$mean2 else pr$mean3
  prior_sd <- if (degree <= 1L) pr$sd else if (degree == 2L) pr$sd2 else pr$sd3
  rope_k <- as.numeric(result$rope_k %||% fit$smt_rope_k %||% 0.1)
  rope <- rope_bounds_from_prior(prior_sd, rope_k)
  ci <- as.numeric(result$ci_level %||% fit$smt_ci %||% 0.95)
  sm <- bayes_term_summary(
    draws, rope = rope, ci = ci,
    prior_mean = as.numeric(prior_mean),
    prior_sd = as.numeric(prior_sd)
  )
  list(
    term = term,
    degree = degree,
    draws = draws,
    prior_mean = as.numeric(prior_mean),
    prior_sd = as.numeric(prior_sd),
    rope = rope,
    rope_k = rope_k,
    ci = ci,
    summary = sm
  )
}

#' Density panel for a prior or posterior with ROPE (and optional HDI)
#'
#' The area under the curve inside the ROPE is filled separately from the
#' area outside it (ROPE mass in amber, remainder in the series color).
#' @keywords internal
bayes_density_panel <- function(kind = c("prior", "posterior"),
                                prior_mean = 0, prior_sd = 1,
                                draws = NULL, rope = c(-0.1, 0.1),
                                hdi = NULL, pd = NA_real_, rope_pct = NA_real_,
                                log_bf10 = NA_real_, ci = 0.95, xlim = NULL) {
  kind <- match.arg(kind)
  rope <- as.numeric(rope)
  if (length(rope) < 2L || !all(is.finite(rope))) rope <- c(-0.1, 0.1)
  r_lo <- min(rope[[1]], rope[[2]])
  r_hi <- max(rope[[1]], rope[[2]])
  if (is.null(xlim) || length(xlim) < 2L || !all(is.finite(xlim))) {
    if (identical(kind, "posterior") && length(draws)) {
      xlim <- range(c(draws, rope, prior_mean + c(-3, 3) * prior_sd), finite = TRUE)
    } else {
      xlim <- range(c(prior_mean + c(-3.5, 3.5) * prior_sd, rope), finite = TRUE)
    }
  }
  if (!all(is.finite(xlim)) || diff(xlim) <= 0) xlim <- c(-1, 1)
  pad <- 0.05 * diff(xlim)
  xlim <- c(xlim[[1]] - pad, xlim[[2]] + pad)
  xs <- seq(xlim[[1]], xlim[[2]], length.out = 200L)

  col_out <- if (identical(kind, "prior")) "#5B2C8B" else "#1F7A4D"
  col_in <- "#D4A017"

  if (identical(kind, "prior")) {
    dens <- stats::dnorm(xs, mean = prior_mean, sd = max(prior_sd, 1e-6))
    df <- data.frame(x = xs, y = dens)
    cap <- sprintf("Prior N(%.2f, %.2f\u00B2)", prior_mean, prior_sd)
  } else {
    draws <- as.numeric(draws)
    draws <- draws[is.finite(draws)]
    if (length(draws) >= 2L) {
      d <- stats::density(draws, from = xlim[[1]], to = xlim[[2]], n = 200L)
      df <- data.frame(x = d$x, y = d$y)
    } else {
      df <- data.frame(x = xs, y = rep(0, length(xs)))
    }
    pd_txt <- if (is.finite(pd)) sprintf("pd=%.0f%%", 100 * pd) else "pd=—"
    rp_txt <- if (is.finite(rope_pct)) sprintf("ROPE=%.0f%%", 100 * rope_pct) else "ROPE=—"
    bf_txt <- if (is.infinite(log_bf10) && log_bf10 > 0) {
      "log BF\u2081\u2080=\u221E"
    } else if (is.infinite(log_bf10) && log_bf10 < 0) {
      "log BF\u2081\u2080=-\u221E"
    } else if (is.finite(log_bf10)) {
      # Natural log Savage–Dickey BF10; >0 favors H1
      sprintf("log BF\u2081\u2080=%+.2f", log_bf10)
    } else {
      NULL
    }
    cap <- paste(c(sprintf("Posterior · %s · %s", pd_txt, rp_txt), bf_txt), collapse = " · ")
  }

  # Split the density so ROPE mass and non-ROPE mass get different fills.
  # Insert boundary knots so the areas meet cleanly at the ROPE edges.
  xs_all <- sort(unique(c(df$x, r_lo, r_hi)))
  ys_all <- stats::approx(df$x, df$y, xout = xs_all, rule = 2)$y
  df_all <- data.frame(x = xs_all, y = pmax(0, ys_all))
  df_in <- df_all[df_all$x >= r_lo & df_all$x <= r_hi, , drop = FALSE]
  df_out <- df_all
  df_out$y_out <- ifelse(df_out$x >= r_lo & df_out$x <= r_hi, 0, df_out$y)

  ymax <- max(df_all$y, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1
  p <- ggplot2::ggplot(df_all, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_area(
      data = df_out,
      ggplot2::aes(x = x, y = y_out),
      fill = col_out, alpha = 0.22, color = NA
    ) +
    ggplot2::geom_area(
      data = df_in,
      ggplot2::aes(x = x, y = y),
      fill = col_in, alpha = 0.55, color = NA
    ) +
    ggplot2::geom_line(color = col_out, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 0, color = "#999999", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = c(r_lo, r_hi), color = col_in,
                        linewidth = 0.35, linetype = "dotted") +
    ggplot2::coord_cartesian(xlim = xlim, ylim = c(0, ymax * 1.15), expand = FALSE) +
    ggplot2::labs(title = cap, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "plain", margin = ggplot2::margin(b = 2)),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(2, 4, 2, 4)
    )

  if (identical(kind, "posterior") && !is.null(hdi) && length(hdi) >= 2L &&
        all(is.finite(hdi))) {
    p <- p +
      ggplot2::annotate(
        "segment",
        x = hdi[[1]], xend = hdi[[2]], y = ymax * 1.05, yend = ymax * 1.05,
        color = "#1F7A4D", linewidth = 1.1
      ) +
      ggplot2::annotate(
        "text",
        x = mean(hdi), y = ymax * 1.12,
        label = sprintf("%d%% HDI", round(100 * ci)),
        size = 2.4, color = "#1F7A4D"
      )
  }
  p
}

#' Compose effect plot (2×2) with prior / posterior strip (1×2)
#' @keywords internal
compose_edge_bayes_plot <- function(effect_plot, info) {
  if (is.null(effect_plot)) return(NULL)
  if (is.null(info)) return(effect_plot)
  if (!requireNamespace("patchwork", quietly = TRUE)) return(effect_plot)

  xlim <- range(
    c(
      info$prior_mean + c(-3.5, 3.5) * info$prior_sd,
      info$rope,
      if (length(info$draws)) info$draws else NULL,
      info$summary$hdi_low, info$summary$hdi_high
    ),
    finite = TRUE
  )
  p_prior <- bayes_density_panel(
    "prior",
    prior_mean = info$prior_mean,
    prior_sd = info$prior_sd,
    rope = info$rope,
    xlim = xlim
  )
  p_post <- bayes_density_panel(
    "posterior",
    prior_mean = info$prior_mean,
    prior_sd = info$prior_sd,
    draws = info$draws,
    rope = info$rope,
    hdi = c(info$summary$hdi_low, info$summary$hdi_high),
    pd = info$summary$pd,
    rope_pct = info$summary$rope_pct,
    log_bf10 = info$summary$log_bf10,
    ci = info$ci,
    xlim = xlim
  )
  strip <- patchwork::wrap_plots(p_prior, p_post, ncol = 1L)
  patchwork::wrap_plots(effect_plot, strip, widths = c(2, 1))
}
