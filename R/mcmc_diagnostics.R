# MCMC sampling health for Bayesian (brms) path fits.
#
# Results shows a one-line strip; the DV inspector shows the same metrics for
# the selected equation plus a small Rhat / ESS plot.

#' Thresholds for MCMC attention (Rhat, ESS ratio, divergences)
#' @keywords internal
mcmc_diag_thresholds <- function() {
  list(
    rhat_warn = 1.01,
    rhat_bad = 1.05,
    neff_ratio_warn = 0.1,
    neff_ratio_bad = 0.05,
    ess_abs_warn = 100
  )
}

#' Unique fingerprint for a brmsfit (dedupe joint equation views)
#' @keywords internal
brms_fit_fingerprint <- function(fit) {
  if (!inherits(fit, "brmsfit")) return(NA_character_)
  rh <- tryCatch(brms::rhat(fit), error = function(e) NULL)
  if (is.null(rh) || !length(rh)) return(NA_character_)
  rh <- rh[is.finite(rh)]
  if (!length(rh)) return(NA_character_)
  sprintf(
    "n%d_max%.6f_mean%.6f",
    length(rh), max(rh), mean(rh)
  )
}

#' Collect unique brms fits from a path result (label = equation key)
#' @return Named list of brmsfit; names are primary equation keys.
#' @keywords internal
brms_unique_fits_from_result <- function(result) {
  if (is.null(result) || !identical(result$sampler, "brms") ||
        !is.list(result$fit) || !length(result$fit)) {
    return(list())
  }
  out <- list()
  seen <- character()
  for (nm in names(result$fit)) {
    f <- result$fit[[nm]]
    if (!inherits(f, "brmsfit")) next
    fp <- brms_fit_fingerprint(f)
    if (is.na(fp) || fp %in% seen) next
    seen <- c(seen, fp)
    out[[nm]] <- f
  }
  out
}

#' MCMC diagnostics for one brmsfit
#'
#' @return list with numeric summaries and `status` in `ok` / `warn` / `bad`.
#' @keywords internal
brms_mcmc_diagnostics <- function(fit) {
  empty <- list(
    ok = FALSE,
    status = "unavailable",
    max_rhat = NA_real_,
    min_neff_ratio = NA_real_,
    min_ess_bulk = NA_real_,
    n_divergent = NA_integer_,
    n_max_treedepth = NA_integer_,
    n_chains = NA_integer_,
    n_iter = NA_integer_,
    n_warmup = NA_integer_,
    n_params = 0L,
    message = "No MCMC diagnostics"
  )
  if (!inherits(fit, "brmsfit")) return(empty)
  if (!requireNamespace("brms", quietly = TRUE)) return(empty)

  thr <- mcmc_diag_thresholds()
  rh <- tryCatch(brms::rhat(fit), error = function(e) NULL)
  rh <- if (is.null(rh)) numeric() else as.numeric(rh[is.finite(rh)])
  max_rhat <- if (length(rh)) max(rh) else NA_real_

  neff <- tryCatch(brms::neff_ratio(fit), error = function(e) NULL)
  neff <- if (is.null(neff)) numeric() else as.numeric(neff[is.finite(neff)])
  min_neff <- if (length(neff)) min(neff) else NA_real_

  # Absolute bulk ESS when the posterior package is available
  min_ess <- NA_real_
  if (requireNamespace("posterior", quietly = TRUE)) {
    sm <- tryCatch(posterior::summarise_draws(fit), error = function(e) NULL)
    if (is.data.frame(sm) && "ess_bulk" %in% names(sm)) {
      eb <- as.numeric(sm$ess_bulk)
      eb <- eb[is.finite(eb)]
      if (length(eb)) min_ess <- min(eb)
    }
  }

  n_div <- NA_integer_
  n_td <- NA_integer_
  np <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  if (is.data.frame(np) && all(c("Parameter", "Value") %in% names(np))) {
    div <- np[as.character(np$Parameter) == "divergent__", , drop = FALSE]
    if (nrow(div)) n_div <- as.integer(sum(as.numeric(div$Value) > 0, na.rm = TRUE))
    td <- np[as.character(np$Parameter) == "treedepth__", , drop = FALSE]
    # Count iterations that hit the default max treedepth (10)
    if (nrow(td)) {
      n_td <- as.integer(sum(as.numeric(td$Value) >= 10, na.rm = TRUE))
    }
  }

  n_chains <- tryCatch(fit$fit@sim$chains, error = function(e) NA_integer_)
  if (!is.finite(n_chains)) {
    n_chains <- tryCatch(
      length(unique(as.data.frame(brms::as_draws_df(fit))$.chain)),
      error = function(e) NA_integer_
    )
  }
  n_iter <- tryCatch(fit$fit@sim$iter, error = function(e) NA_integer_)
  n_warmup <- tryCatch(fit$fit@sim$warmup, error = function(e) NA_integer_)

  status <- "ok"
  reasons <- character()
  if (is.finite(max_rhat) && max_rhat >= thr$rhat_bad) {
    status <- "bad"
    reasons <- c(reasons, sprintf("max Rhat = %.3f", max_rhat))
  } else if (is.finite(max_rhat) && max_rhat >= thr$rhat_warn) {
    status <- "warn"
    reasons <- c(reasons, sprintf("max Rhat = %.3f", max_rhat))
  }
  if (is.finite(min_neff) && min_neff < thr$neff_ratio_bad) {
    status <- "bad"
    reasons <- c(reasons, sprintf("min ESS ratio = %.3f", min_neff))
  } else if (is.finite(min_neff) && min_neff < thr$neff_ratio_warn) {
    if (!identical(status, "bad")) status <- "warn"
    reasons <- c(reasons, sprintf("min ESS ratio = %.3f", min_neff))
  }
  if (is.finite(min_ess) && min_ess < thr$ess_abs_warn) {
    if (!identical(status, "bad")) status <- "warn"
    reasons <- c(reasons, sprintf("min bulk ESS = %.0f", min_ess))
  }
  if (is.finite(n_div) && n_div > 0L) {
    status <- if (n_div >= 10L) "bad" else if (!identical(status, "bad")) "warn" else status
    reasons <- c(reasons, sprintf("%d divergent transition%s", n_div, if (n_div == 1L) "" else "s"))
  }
  if (is.finite(n_td) && n_td > 0L) {
    if (!identical(status, "bad")) status <- "warn"
    reasons <- c(reasons, sprintf("%d max-treedepth hit%s", n_td, if (n_td == 1L) "" else "s"))
  }

  msg <- if (!length(reasons)) {
    if (is.finite(max_rhat)) {
      sprintf("Sampling looks fine (max Rhat = %.3f)", max_rhat)
    } else {
      "Sampling diagnostics unavailable"
    }
  } else {
    paste(reasons, collapse = " · ")
  }

  list(
    ok = identical(status, "ok"),
    status = status,
    max_rhat = max_rhat,
    min_neff_ratio = min_neff,
    min_ess_bulk = min_ess,
    n_divergent = n_div,
    n_max_treedepth = n_td,
    n_chains = as.integer(n_chains),
    n_iter = as.integer(n_iter),
    n_warmup = as.integer(n_warmup),
    n_params = length(rh),
    message = msg
  )
}

#' Aggregate MCMC diagnostics across equations in a result
#' @keywords internal
brms_mcmc_diagnostics_aggregate <- function(result, state = NULL) {
  fits <- brms_unique_fits_from_result(result)
  empty <- list(
    ok = FALSE,
    status = "unavailable",
    n_models = 0L,
    max_rhat = NA_real_,
    min_neff_ratio = NA_real_,
    min_ess_bulk = NA_real_,
    n_divergent = 0L,
    n_max_treedepth = 0L,
    worst_key = NA_character_,
    worst_label = NA_character_,
    message = "No Bayesian MCMC fits",
    by_key = list()
  )
  if (!length(fits)) return(empty)

  by_key <- lapply(fits, brms_mcmc_diagnostics)
  names(by_key) <- names(fits)

  statuses <- vapply(by_key, function(d) d$status, character(1))
  status <- if (any(statuses == "bad")) {
    "bad"
  } else if (any(statuses == "warn")) {
    "warn"
  } else if (any(statuses == "ok")) {
    "ok"
  } else {
    "unavailable"
  }

  max_rhat <- max(vapply(by_key, function(d) d$max_rhat %||% NA_real_, numeric(1)), na.rm = TRUE)
  if (!is.finite(max_rhat)) max_rhat <- NA_real_
  min_neff <- min(vapply(by_key, function(d) d$min_neff_ratio %||% NA_real_, numeric(1)), na.rm = TRUE)
  if (!is.finite(min_neff)) min_neff <- NA_real_
  min_ess <- min(vapply(by_key, function(d) d$min_ess_bulk %||% NA_real_, numeric(1)), na.rm = TRUE)
  if (!is.finite(min_ess)) min_ess <- NA_real_
  n_div <- sum(vapply(by_key, function(d) {
    v <- d$n_divergent
    if (!is.finite(v)) 0L else as.integer(v)
  }, integer(1)))
  n_td <- sum(vapply(by_key, function(d) {
    v <- d$n_max_treedepth
    if (!is.finite(v)) 0L else as.integer(v)
  }, integer(1)))

  # Worst equation: prefer bad, then highest Rhat
  score <- vapply(by_key, function(d) {
    s <- if (identical(d$status, "bad")) 2 else if (identical(d$status, "warn")) 1 else 0
    r <- d$max_rhat %||% 1
    if (!is.finite(r)) r <- 1
    s * 10 + r
  }, numeric(1))
  worst_key <- names(by_key)[[which.max(score)]]
  worst_label <- if (!is.null(state)) {
    dv_fit_key_label(state, worst_key)
  } else {
    worst_key
  }

  bits <- character()
  if (is.finite(max_rhat)) bits <- c(bits, sprintf("max Rhat %.3f", max_rhat))
  if (is.finite(min_neff)) bits <- c(bits, sprintf("min ESS ratio %.3f", min_neff))
  if (n_div > 0L) bits <- c(bits, sprintf("%d divergent", n_div))
  if (n_td > 0L) bits <- c(bits, sprintf("%d treedepth", n_td))
  if (length(fits) > 1L && !identical(status, "ok")) {
    bits <- c(bits, paste0("worst: ", worst_label))
  }
  msg <- if (identical(status, "ok")) {
    if (is.finite(max_rhat)) {
      sprintf("MCMC OK across %d model%s (max Rhat = %.3f)",
              length(fits), if (length(fits) == 1L) "" else "s", max_rhat)
    } else {
      "MCMC OK"
    }
  } else if (length(bits)) {
    paste(bits, collapse = " · ")
  } else {
    "MCMC diagnostics unavailable"
  }

  list(
    ok = identical(status, "ok"),
    status = status,
    n_models = length(fits),
    max_rhat = max_rhat,
    min_neff_ratio = min_neff,
    min_ess_bulk = min_ess,
    n_divergent = n_div,
    n_max_treedepth = n_td,
    worst_key = worst_key,
    worst_label = worst_label,
    message = msg,
    by_key = by_key
  )
}

#' HTML strip for Results: global Bayesian sampling health
#' @keywords internal
format_mcmc_health_html <- function(agg) {
  if (is.null(agg) || identical(agg$status, "unavailable")) return(NULL)
  cls <- switch(
    as.character(agg$status),
    ok = "smt-mcmc-health smt-mcmc-health-ok",
    warn = "smt-mcmc-health smt-mcmc-health-warn",
    bad = "smt-mcmc-health smt-mcmc-health-bad",
    "smt-mcmc-health"
  )
  icon <- switch(
    as.character(agg$status),
    ok = "fas fa-check",
    warn = "fas fa-exclamation-triangle",
    bad = "fas fa-exclamation-circle",
    "fas fa-info-circle"
  )
  hint <- if (!identical(agg$status, "ok")) {
    " Select the DV in Model view for per-equation diagnostics."
  } else {
    ""
  }
  shiny::div(
    class = cls,
    shiny::tags$i(class = paste(icon, "smt-status-icon"), `aria-hidden` = "true"),
    shiny::div(
      class = "smt-mcmc-health-body",
      shiny::div(
        class = "smt-mcmc-health-title-row",
        shiny::tags$strong("MCMC"),
        smt_help_button("bayes_diagnostics")
      ),
      shiny::span(paste0(agg$message, hint))
    )
  )
}

#' Compact HTML summary for the DV inspector
#' @keywords internal
format_mcmc_dv_summary_html <- function(diag) {
  if (is.null(diag) || identical(diag$status, "unavailable")) {
    return(htmltools::p(
      class = "help-block",
      "Run a Bayesian analysis to see MCMC diagnostics."
    ))
  }
  cls <- switch(
    as.character(diag$status),
    ok = "smt-mcmc-dv-summary smt-mcmc-health-ok",
    warn = "smt-mcmc-dv-summary smt-mcmc-health-warn",
    bad = "smt-mcmc-dv-summary smt-mcmc-health-bad",
    "smt-mcmc-dv-summary"
  )
  rows <- c(
    if (is.finite(diag$max_rhat)) sprintf("max Rhat = %.3f", diag$max_rhat),
    if (is.finite(diag$min_neff_ratio)) sprintf("min ESS ratio = %.3f", diag$min_neff_ratio),
    if (is.finite(diag$min_ess_bulk)) sprintf("min bulk ESS = %.0f", diag$min_ess_bulk),
    if (is.finite(diag$n_divergent)) sprintf("divergences = %d", as.integer(diag$n_divergent)),
    if (is.finite(diag$n_max_treedepth) && diag$n_max_treedepth > 0) {
      sprintf("max treedepth hits = %d", as.integer(diag$n_max_treedepth))
    },
    if (is.finite(diag$n_chains) && is.finite(diag$n_iter)) {
      sprintf(
        "%d chain%s × %d iterations (warmup %s)",
        as.integer(diag$n_chains),
        if (as.integer(diag$n_chains) == 1L) "" else "s",
        as.integer(diag$n_iter),
        if (is.finite(diag$n_warmup)) as.integer(diag$n_warmup) else "?"
      )
    }
  )
  htmltools::div(
    class = cls,
    htmltools::tags$p(
      class = "smt-mcmc-dv-msg",
      diag$message
    ),
    if (length(rows)) {
      htmltools::tags$ul(
        lapply(rows, function(x) htmltools::tags$li(x))
      )
    }
  )
}

#' Rhat + ESS-ratio scatter for one brmsfit (DV inspector)
#' @keywords internal
mcmc_diagnostics_plot <- function(fit, title = NULL) {
  empty <- function(msg = NULL) {
    p <- ggplot2::ggplot() + ggplot2::theme_void()
    if (!is.null(msg) && nzchar(msg)) {
      p <- p +
        ggplot2::annotate("text", x = 0, y = 0, label = msg, color = "#6e6e73")
    }
    p
  }
  if (!inherits(fit, "brmsfit")) return(empty())
  rh <- tryCatch(brms::rhat(fit), error = function(e) NULL)
  ne <- tryCatch(brms::neff_ratio(fit), error = function(e) NULL)
  if (is.null(rh) || is.null(ne) || !length(rh) || !length(ne)) {
    return(empty("No Rhat / ESS diagnostics"))
  }
  nms <- intersect(names(rh), names(ne))
  if (!length(nms)) {
    # Positional fallback
    n <- min(length(rh), length(ne))
    nms <- paste0("p", seq_len(n))
    rh <- rh[seq_len(n)]
    ne <- ne[seq_len(n)]
    names(rh) <- nms
    names(ne) <- nms
  }
  df <- data.frame(
    param = nms,
    rhat = as.numeric(rh[nms]),
    neff = as.numeric(ne[nms]),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$rhat) & is.finite(df$neff), , drop = FALSE]
  if (!nrow(df)) return(empty("No finite Rhat / ESS values"))

  thr <- mcmc_diag_thresholds()
  df$flag <- df$rhat >= thr$rhat_warn | df$neff < thr$neff_ratio_warn

  # Keep plot readable: at most ~40 params (prefer flagged + random sample)
  if (nrow(df) > 40L) {
    flagged <- which(df$flag)
    keep <- unique(c(flagged, utils::head(order(df$rhat, decreasing = TRUE), 20L)))
    if (length(keep) < 40L) {
      rest <- setdiff(seq_len(nrow(df)), keep)
      keep <- c(keep, utils::head(rest, 40L - length(keep)))
    }
    df <- df[keep, , drop = FALSE]
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$neff, y = .data$rhat)) +
    ggplot2::geom_hline(
      yintercept = thr$rhat_warn, linetype = "dashed", color = "#c2410c",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = thr$neff_ratio_warn, linetype = "dashed", color = "#c2410c",
      linewidth = 0.4
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$flag),
      size = 2, alpha = 0.85
    ) +
    ggplot2::scale_color_manual(
      values = c(`FALSE` = "#5b5b63", `TRUE` = "#c2410c"),
      guide = "none"
    ) +
    ggplot2::labs(
      title = title,
      x = "ESS ratio (effective / total draws)",
      y = "Rhat"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Observed outcome vector for a (possibly joint) brmsfit view
#' @keywords internal
brms_observed_y <- function(fit) {
  if (!inherits(fit, "brmsfit")) return(numeric())
  resp <- as.character(fit$smt_response %||% "")[[1]]
  y <- tryCatch({
    if (!is.na(resp) && nzchar(resp)) {
      brms::get_y(fit, resp = resp)
    } else {
      brms::get_y(fit)
    }
  }, error = function(e) numeric())
  as.numeric(y)
}

#' Posterior predictive draws for a (possibly joint) brmsfit view
#' @keywords internal
brms_posterior_predict_y <- function(fit, ndraws = 50L) {
  if (!inherits(fit, "brmsfit")) return(NULL)
  ndraws <- as.integer(ndraws %||% 50L)
  if (!is.finite(ndraws) || ndraws < 5L) ndraws <- 50L
  if (ndraws > 200L) ndraws <- 200L
  resp <- as.character(fit$smt_response %||% "")[[1]]
  tryCatch({
    if (!is.na(resp) && nzchar(resp)) {
      # Translate SMT response name to Stan-safe name when needed
      resp_safe <- brms_translate_term(fit, resp, "to_safe")
      brms::posterior_predict(fit, resp = resp_safe, ndraws = ndraws)
    } else {
      brms::posterior_predict(fit, ndraws = ndraws)
    }
  }, error = function(e) {
    # Fallback: omit resp (univariate)
    tryCatch(
      brms::posterior_predict(fit, ndraws = ndraws),
      error = function(e2) NULL
    )
  })
}

#' Posterior predictive check plot (dens overlay or binary mean check)
#'
#' Continuous: light densities of `y_rep` draws with the observed `y` density
#' overlaid. Binary / Bernoulli: histogram of replicated means vs observed mean.
#' @keywords internal
brms_ppc_plot <- function(fit, title = NULL, ndraws = 50L) {
  empty <- function(msg = NULL) {
    p <- ggplot2::ggplot() + ggplot2::theme_void()
    if (!is.null(msg) && nzchar(msg)) {
      p <- p +
        ggplot2::annotate("text", x = 0, y = 0, label = msg, color = "#6e6e73")
    }
    p
  }
  if (!inherits(fit, "brmsfit")) return(empty())
  y <- brms_observed_y(fit)
  y <- y[is.finite(y)]
  if (length(y) < 5L) return(empty("Too few observations for PPC"))

  yrep <- brms_posterior_predict_y(fit, ndraws = ndraws)
  if (is.null(yrep) || !length(yrep)) return(empty("posterior_predict failed"))
  yrep <- as.matrix(yrep)
  if (!ncol(yrep) || !nrow(yrep)) return(empty("Empty y_rep"))

  fl <- tryCatch(brms_view_family(fit), error = function(e) list(family = "gaussian"))
  fam <- as.character(fl$family %||% "gaussian")
  binary <- identical(fam, "bernoulli") || identical(fam, "binomial") ||
    (all(y %in% c(0, 1)) && length(unique(y)) <= 2L)

  if (binary) {
    obs_mean <- mean(y)
    rep_means <- rowMeans(yrep, na.rm = TRUE)
    rep_means <- rep_means[is.finite(rep_means)]
    if (!length(rep_means)) return(empty("No finite replicated means"))
    df <- data.frame(mean = rep_means, stringsAsFactors = FALSE)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$mean)) +
        ggplot2::geom_histogram(
          bins = min(30L, max(10L, length(rep_means) %/% 3L)),
          fill = "#c7c7cc", color = NA, alpha = 0.9
        ) +
        ggplot2::geom_vline(
          xintercept = obs_mean, color = "#1d1d1f", linewidth = 0.9
        ) +
        ggplot2::labs(
          title = title,
          subtitle = sprintf(
            "PPC: replicated mean(y) vs observed mean = %.3f", obs_mean
          ),
          x = "Mean of replicated outcomes",
          y = "Count"
        ) +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 11, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 10, color = "#6e6e73"),
          panel.grid.minor = ggplot2::element_blank()
        )
    )
  }

  # Continuous dens overlay (subset of draws for readability)
  n_show <- min(nrow(yrep), as.integer(ndraws))
  idx <- seq_len(n_show)
  dens_list <- lapply(idx, function(i) {
    d <- tryCatch(
      stats::density(yrep[i, ], na.rm = TRUE, n = 128L),
      error = function(e) NULL
    )
    if (is.null(d)) return(NULL)
    data.frame(
      x = d$x, y = d$y, draw = i,
      stringsAsFactors = FALSE
    )
  })
  dens_list <- Filter(Negate(is.null), dens_list)
  if (!length(dens_list)) return(empty("Could not build y_rep densities"))
  dens_df <- do.call(rbind, dens_list)
  dens_df$draw <- factor(dens_df$draw)

  d_obs <- tryCatch(
    stats::density(y, na.rm = TRUE, n = 128L),
    error = function(e) NULL
  )
  if (is.null(d_obs)) return(empty("Could not build observed density"))
  obs_df <- data.frame(x = d_obs$x, y = d_obs$y, stringsAsFactors = FALSE)

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dens_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$draw),
      color = "#b0b0b5", linewidth = 0.35, alpha = 0.55
    ) +
    ggplot2::geom_line(
      data = obs_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "#1d1d1f", linewidth = 1.05
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "PPC dens overlay: %d draws of y_rep (grey) vs observed y (black)",
        n_show
      ),
      x = "Outcome",
      y = "Density"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#6e6e73"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
