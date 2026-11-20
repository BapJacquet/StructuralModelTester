# Posterior indices for Bayesian path links: HDI, pd, ROPE, and log BF10.
# Follows Kruschke (HDI + ROPE) and Makowski et al. (pd / % in ROPE / BF).

#' Highest-density interval from draws
#' @keywords internal
hdi_of <- function(x, ci = 0.95) {
  x <- as.numeric(x)
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (!n) return(c(low = NA_real_, high = NA_real_))
  ci <- as.numeric(ci)
  if (!is.finite(ci) || ci <= 0 || ci >= 1) ci <- 0.95
  kn <- max(1L, as.integer(floor(ci * n)))
  if (kn >= n) return(c(low = x[[1]], high = x[[n]]))
  widths <- x[kn:n] - x[seq_len(n - kn + 1L)]
  i <- which.min(widths)[[1]]
  c(low = x[[i]], high = x[[i + kn - 1L]])
}

#' Probability of Direction (Makowski et al.)
#'
#' Proportion of posterior mass on the side of the median. Ranges from 0.5 to 1
#' for continuous parameters.
#' @keywords internal
pd_of <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  med <- stats::median(x)
  if (!is.finite(med) || identical(med, 0)) {
    return(max(mean(x > 0), mean(x < 0)))
  }
  mean(sign(x) == sign(med))
}

#' ROPE half-width from a link's prior SD
#'
#' Default: `rope_k * prior_sd` around 0. With the usual Expected SD of 1 and
#' `rope_k = 0.1`, the ROPE is [-0.1, 0.1].
#' @keywords internal
rope_bounds_from_prior <- function(prior_sd, rope_k = 0.1) {
  sd <- as.numeric(prior_sd)
  k <- as.numeric(rope_k)
  if (!is.finite(sd) || sd <= 0) sd <- 1
  if (!is.finite(k) || k <= 0) k <- 0.1
  hw <- k * sd
  c(low = -hw, high = hw)
}

#' Share of draws inside the ROPE
#' @keywords internal
rope_pct_of <- function(x, rope) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x) || length(rope) < 2L) return(NA_real_)
  lo <- as.numeric(rope[[1]])
  hi <- as.numeric(rope[[2]])
  if (!is.finite(lo) || !is.finite(hi)) return(NA_real_)
  mean(x >= lo & x <= hi)
}

#' Prior probability mass inside the ROPE (normal prior)
#' @keywords internal
rope_prior_mass <- function(prior_mean = 0, prior_sd = 1, rope = c(-0.1, 0.1)) {
  mu <- as.numeric(prior_mean)
  sd <- as.numeric(prior_sd)
  if (!is.finite(mu)) mu <- 0
  if (!is.finite(sd) || sd <= 0) sd <- 1
  if (length(rope) < 2L) return(NA_real_)
  lo <- as.numeric(rope[[1]])
  hi <- as.numeric(rope[[2]])
  if (!is.finite(lo) || !is.finite(hi)) return(NA_real_)
  if (hi < lo) {
    tmp <- lo
    lo <- hi
    hi <- tmp
  }
  stats::pnorm(hi, mean = mu, sd = sd) - stats::pnorm(lo, mean = mu, sd = sd)
}

#' Bayes factor BF10 for “outside ROPE” vs “inside ROPE”
#'
#' Makowski et al. (2019): prior vs posterior odds of lying outside the ROPE.
#' BF10 > 1 favors a non-negligible effect. Less useful when the Expected prior
#' already puts near-zero mass in the ROPE (ratio ≈ 1).
#' @keywords internal
bf10_rope_of <- function(x, rope, prior_mean = 0, prior_sd = 1) {
  p_post <- rope_pct_of(x, rope)
  p_prior <- rope_prior_mass(prior_mean, prior_sd, rope)
  if (!is.finite(p_post) || !is.finite(p_prior)) return(NA_real_)
  # Avoid 0/1 odds blowing up from Monte Carlo / tiny tails
  eps <- 1e-10
  p_post <- min(max(p_post, eps), 1 - eps)
  p_prior <- min(max(p_prior, eps), 1 - eps)
  odds_out_post <- (1 - p_post) / p_post
  odds_out_prior <- (1 - p_prior) / p_prior
  as.numeric(odds_out_post / odds_out_prior)
}

#' Savage–Dickey log BF10 for a point null at 0
#'
#' Natural log of BF₁₀ = log f_prior(0) − log f_post(0) (Wagenmakers et al.):
#' evidence for H1: θ ≠ 0 vs H0: θ = 0. Positive favors H1, negative favors H0.
#'
#' Returns `NA` when the Expected prior has negligible density at the null
#' (e.g. N(1.5, 0.05)): Savage–Dickey is then not a meaningful test against 0.
#' @keywords internal
log_bf10_point_null_of <- function(x, prior_mean = 0, prior_sd = 1, null = 0) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  mu <- as.numeric(prior_mean)
  sd <- as.numeric(prior_sd)
  null <- as.numeric(null)
  if (!is.finite(mu)) mu <- 0
  if (!is.finite(sd) || sd <= 0) sd <- 1
  if (!is.finite(null)) null <- 0
  f_prior <- stats::dnorm(null, mean = mu, sd = sd)
  # Prior must place real mass at the null for SD-BF to be interpretable
  if (!is.finite(f_prior) || f_prior < 1e-6) return(NA_real_)
  d <- tryCatch(
    stats::density(x, n = 512L, from = min(c(x, null)), to = max(c(x, null))),
    error = function(e) NULL
  )
  if (is.null(d)) return(NA_real_)
  f_post <- stats::approx(d$x, d$y, xout = null, rule = 2)$y
  if (!is.finite(f_post) || f_post <= 0) return(Inf)
  as.numeric(log(f_prior) - log(f_post))
}

#' @keywords internal
bf10_point_null_of <- function(x, prior_mean = 0, prior_sd = 1, null = 0) {
  lb <- log_bf10_point_null_of(x, prior_mean = prior_mean, prior_sd = prior_sd, null = null)
  if (is.na(lb)) return(NA_real_)
  exp(lb)
}

#' Prior→posterior information gain (KL divergence, nats)
#'
#' \(D_{\mathrm{KL}}(\pi(\theta\mid data)\,\|\,\pi(\theta))
#' = \mathbb{E}_{\mathrm{post}}[\log \pi(\theta\mid data) - \log \pi(\theta)]\).
#' Zero when the data leave beliefs unchanged; larger when the posterior shifts
#' and/or concentrates relative to the Expected normal prior.
#' @keywords internal
kl_prior_post_of <- function(x, prior_mean = 0, prior_sd = 1) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  mu <- as.numeric(prior_mean)
  sd <- as.numeric(prior_sd)
  if (!is.finite(mu)) mu <- 0
  if (!is.finite(sd) || sd <= 0) sd <- 1
  # Monte Carlo KL using a kernel density for the posterior
  d <- tryCatch(
    stats::density(x, n = 512L, from = min(x), to = max(x)),
    error = function(e) NULL
  )
  if (is.null(d)) return(NA_real_)
  f_post <- stats::approx(d$x, d$y, xout = x, rule = 2)$y
  f_post <- pmax(as.numeric(f_post), .Machine$double.xmin)
  f_prior <- stats::dnorm(x, mean = mu, sd = sd)
  f_prior <- pmax(as.numeric(f_prior), .Machine$double.xmin)
  kl <- mean(log(f_post) - log(f_prior))
  if (!is.finite(kl) || kl < 0) {
    # Tiny negatives are numerical artifacts when post ≈ prior
    if (is.finite(kl) && kl > -1e-6) return(0)
    return(NA_real_)
  }
  as.numeric(kl)
}

#' HDI + ROPE decision (Kruschke)
#'
#' @return `"reject"`, `"accept"`, or `"undecided"`
#' @keywords internal
rope_hdi_decision <- function(hdi, rope) {
  if (length(hdi) < 2L || length(rope) < 2L) return(NA_character_)
  hl <- as.numeric(hdi[[1]])
  hh <- as.numeric(hdi[[2]])
  rl <- as.numeric(rope[[1]])
  rh <- as.numeric(rope[[2]])
  if (!all(is.finite(c(hl, hh, rl, rh)))) return(NA_character_)
  if (hh < rl || hl > rh) return("reject")
  if (hl >= rl && hh <= rh) return("accept")
  "undecided"
}

#' Posterior draws for one population-level brms coefficient
#' @keywords internal
brms_term_draws <- function(fit, term) {
  if (!inherits(fit, "brmsfit")) return(numeric())
  term <- as.character(term)
  if (!nzchar(term)) return(numeric())
  # Estimates use SMT names; Stan parameters use brms-safe names
  term <- brms_translate_term(fit, term, "to_safe")
  dm <- tryCatch(
    as.matrix(brms::as_draws_matrix(fit)),
    error = function(e) NULL
  )
  if (is.null(dm) || !ncol(dm)) return(numeric())
  resp <- fit$smt_response %||% ""
  resp <- as.character(resp)
  if (length(resp) != 1L || is.na(resp)) resp <- ""
  # Joint fits tag smt_response with the SMT outcome name; Stan uses the safe name
  if (nzchar(resp)) resp <- brms_translate_term(fit, resp, "to_safe")
  base <- if (identical(term, "(Intercept)") || identical(term, "Intercept")) {
    "Intercept"
  } else {
    term
  }
  col <- if (nzchar(resp)) {
    paste0("b_", resp, "_", base)
  } else {
    paste0("b_", base)
  }
  if (!col %in% colnames(dm)) return(numeric())
  as.numeric(dm[, col])
}

#' Build per-term ROPE bounds from Expected coefficients
#' @keywords internal
brms_rope_by_term <- function(state, preds, term_meta, rope_k = 0.1) {
  term_meta <- term_meta %||% list()
  out <- list()
  for (tn in names(term_meta)) {
    sp <- edge_normal_prior(state, preds, term_meta[[tn]])
    out[[tn]] <- rope_bounds_from_prior(sp$sd, rope_k)
  }
  out
}

#' Posterior narrowing: prior SD / posterior SD
#'
#' Values near 1 mean the data barely tightened the Expected prior; 2 means the
#' posterior is half as wide as the prior. Complements KL, which also mixes in
#' location shift away from the prior mean.
#' @keywords internal
post_narrow_of <- function(post_sd, prior_sd = 1) {
  post_sd <- as.numeric(post_sd)
  prior_sd <- as.numeric(prior_sd)
  if (!is.finite(post_sd) || post_sd <= 0) return(NA_real_)
  if (!is.finite(prior_sd) || prior_sd <= 0) prior_sd <- 1
  as.numeric(prior_sd / post_sd)
}

#' Flag KL below this (nats) as weak prior→posterior learning in Results
#' @keywords internal
bayes_kl_attention_max <- function() 0.1

#' Flag SD↓ below this as little posterior narrowing in Results
#' @keywords internal
bayes_sd_narrow_attention_max <- function() 1.5

#' Wrap a table cell when a Bayesian learning diagnostic is weak
#' @keywords internal
bayes_attention_html <- function(txt, title) {
  txt <- as.character(txt %||% "")
  if (!nzchar(txt)) return("")
  sprintf(
    '<span class="smt-bayes-weak" title="%s">%s</span>',
    htmltools::htmlEscape(as.character(title %||% ""), attribute = TRUE),
    htmltools::htmlEscape(txt)
  )
}

#' Format KL for path-coef tables; highlight low learning
#' @keywords internal
format_bayes_kl <- function(k) {
  k <- as.numeric(k)
  if (!is.finite(k)) return("")
  txt <- if (k >= 100) sprintf("%.0f", k)
  else if (k >= 10) sprintf("%.1f", k)
  else sprintf("%.2f", k)
  if (k < bayes_kl_attention_max()) {
    return(bayes_attention_html(
      txt,
      sprintf(
        "Low prior\u2192posterior learning (KL < %.2f nats)",
        bayes_kl_attention_max()
      )
    ))
  }
  txt
}

#' Format SD↓ for path-coef tables; highlight little narrowing
#' @keywords internal
format_bayes_sd_narrow <- function(r) {
  r <- as.numeric(r)
  if (!is.finite(r) || r <= 0) return("")
  txt <- if (r >= 100) sprintf("%.0f", r)
  else if (r >= 10) sprintf("%.1f", r)
  else sprintf("%.2f", r)
  if (r < bayes_sd_narrow_attention_max()) {
    return(bayes_attention_html(
      txt,
      sprintf(
        "Little posterior narrowing (SD\u2193 < %.1f)",
        bayes_sd_narrow_attention_max()
      )
    ))
  }
  txt
}

#' Summaries for one coefficient: mean, SD, HDI, pd, ROPE, log BF10, KL, narrowing
#' @keywords internal
bayes_term_summary <- function(draws, rope = c(-0.1, 0.1), ci = 0.95,
                               prior_mean = 0, prior_sd = 1) {
  draws <- as.numeric(draws)
  draws <- draws[is.finite(draws)]
  empty <- list(
    est = NA_real_, se = NA_real_,
    hdi_low = NA_real_, hdi_high = NA_real_,
    pd = NA_real_, rope_low = as.numeric(rope[[1]] %||% NA_real_),
    rope_high = as.numeric(rope[[2]] %||% NA_real_),
    rope_pct = NA_real_, rope_decision = NA_character_,
    log_bf10 = NA_real_,
    kl_prior_post = NA_real_,
    post_narrow = NA_real_,
    includes_null = NA
  )
  if (!length(draws)) return(empty)
  hdi <- hdi_of(draws, ci)
  decision <- rope_hdi_decision(hdi, rope)
  post_sd <- stats::sd(draws)
  list(
    est = mean(draws),
    se = post_sd,
    hdi_low = unname(hdi[["low"]] %||% hdi[[1]]),
    hdi_high = unname(hdi[["high"]] %||% hdi[[2]]),
    pd = pd_of(draws),
    rope_low = as.numeric(rope[[1]]),
    rope_high = as.numeric(rope[[2]]),
    rope_pct = rope_pct_of(draws, rope),
    rope_decision = decision,
    log_bf10 = log_bf10_point_null_of(
      draws, prior_mean = prior_mean, prior_sd = prior_sd
    ),
    kl_prior_post = kl_prior_post_of(
      draws, prior_mean = prior_mean, prior_sd = prior_sd
    ),
    post_narrow = post_narrow_of(post_sd, prior_sd),
    includes_null = !identical(decision, "reject")
  )
}


#' Highest-density interval from a sample
#' @keywords internal
hdi_bounds <- function(x, ci = 0.95) {
  x <- sort(as.numeric(x[is.finite(x)]))
  n <- length(x)
  if (!n) return(c(low = NA_real_, high = NA_real_))
  ci <- as.numeric(ci)
  if (!is.finite(ci) || ci <= 0 || ci >= 1) ci <- 0.95
  kn <- max(1L, floor(ci * n))
  if (kn >= n) return(c(low = x[[1]], high = x[[n]]))
  widths <- x[kn:n] - x[seq_len(n - kn + 1L)]
  i <- which.min(widths)
  c(low = x[[i]], high = x[[i + kn - 1L]])
}

#' Probability of Direction (pd): mass on the median's side of 0
#' @keywords internal
posterior_pd <- function(x) {
  x <- as.numeric(x[is.finite(x)])
  if (!length(x)) return(NA_real_)
  med <- stats::median(x)
  if (!is.finite(med) || med == 0) {
    return(max(mean(x > 0), mean(x < 0)))
  }
  mean(sign(x) == sign(med))
}

#' Default ROPE half-width: `rope_k * prior_sd` around 0
#' @keywords internal
rope_from_prior_sd <- function(prior_sd, rope_k = 0.1) {
  sd <- as.numeric(prior_sd)
  k <- as.numeric(rope_k)
  if (!is.finite(sd) || sd <= 0) sd <- 1
  if (!is.finite(k) || k <= 0) k <- 0.1
  half <- k * sd
  c(low = -half, high = half)
}

#' Share of draws inside the ROPE
#' @keywords internal
rope_percentage <- function(x, rope_low, rope_high) {
  x <- as.numeric(x[is.finite(x)])
  if (!length(x) || !is.finite(rope_low) || !is.finite(rope_high)) return(NA_real_)
  if (rope_low > rope_high) {
    tmp <- rope_low
    rope_low <- rope_high
    rope_high <- tmp
  }
  mean(x >= rope_low & x <= rope_high)
}

#' Kruschke HDI+ROPE decision
#'
#' @return `"reject"`, `"accept"`, or `"undecided"`
#' @keywords internal
hdi_rope_decision <- function(hdi_low, hdi_high, rope_low, rope_high) {
  if (!all(is.finite(c(hdi_low, hdi_high, rope_low, rope_high)))) {
    return(NA_character_)
  }
  if (rope_low > rope_high) {
    tmp <- rope_low
    rope_low <- rope_high
    rope_high <- tmp
  }
  if (hdi_high < rope_low || hdi_low > rope_high) return("reject")
  if (hdi_low >= rope_low && hdi_high <= rope_high) return("accept")
  "undecided"
}

#' Posterior / prior summaries for one coefficient sample
#' @keywords internal
bayes_coef_indices <- function(draws, ci = 0.95, prior_mean = 0, prior_sd = 1,
                               rope_k = 0.1, rope_low = NULL, rope_high = NULL) {
  draws <- as.numeric(draws[is.finite(draws)])
  rope <- if (is.finite(rope_low %||% NA_real_) && is.finite(rope_high %||% NA_real_)) {
    c(low = as.numeric(rope_low), high = as.numeric(rope_high))
  } else {
    rope_from_prior_sd(prior_sd, rope_k = rope_k)
  }
  hdi <- hdi_bounds(draws, ci = ci)
  pd <- posterior_pd(draws)
  rp <- rope_percentage(draws, rope[["low"]], rope[["high"]])
  decision <- hdi_rope_decision(hdi[["low"]], hdi[["high"]], rope[["low"]], rope[["high"]])
  list(
    est = if (length(draws)) mean(draws) else NA_real_,
    se = if (length(draws)) stats::sd(draws) else NA_real_,
    ci_low = hdi[["low"]],
    ci_high = hdi[["high"]],
    ci_level = as.numeric(ci),
    pd = pd,
    rope_low = unname(rope[["low"]]),
    rope_high = unname(rope[["high"]]),
    rope_pct = rp,
    rope_decision = decision,
    includes_zero = if (is.na(decision)) NA else !identical(decision, "reject"),
    prior_mean = as.numeric(prior_mean %||% 0),
    prior_sd = as.numeric(prior_sd %||% 1)
  )
}

#' Draw matrix of population-level slopes from a brms fit
#' @keywords internal
brms_slope_draws <- function(fit) {
  if (!inherits(fit, "brmsfit")) return(NULL)
  dm <- tryCatch(as.matrix(brms::as_draws_matrix(fit)), error = function(e) NULL)
  if (is.null(dm) || !ncol(dm)) return(NULL)
  keep <- grepl("^b_", colnames(dm))
  if (!any(keep)) return(NULL)
  out <- dm[, keep, drop = FALSE]
  colnames(out) <- sub("^b_", "", colnames(out))
  colnames(out)[colnames(out) == "Intercept"] <- "(Intercept)"
  out
}

#' Enrich a brms fixef table with HDI, pd, and ROPE columns
#'
#' `term_priors` is an optional named list of lists with `mean`, `sd` for ROPE.
#' @keywords internal
brms_coef_table <- function(fit, ci = 0.95, rope_k = 0.1, term_priors = NULL) {
  ci <- as.numeric(ci)
  if (!is.finite(ci) || ci <= 0 || ci >= 1) ci <- 0.95
  rope_k <- as.numeric(rope_k %||% 0.1)
  if (!is.finite(rope_k) || rope_k <= 0) rope_k <- 0.1
  fx <- as.data.frame(brms::fixef(fit))
  draws <- brms_slope_draws(fit)
  n <- nrow(fx)
  fx$ci_low <- rep(NA_real_, n)
  fx$ci_high <- rep(NA_real_, n)
  fx$pd <- rep(NA_real_, n)
  fx$rope_low <- rep(NA_real_, n)
  fx$rope_high <- rep(NA_real_, n)
  fx$rope_pct <- rep(NA_real_, n)
  fx$rope_decision <- rep(NA_character_, n)
  fx$includes_zero <- rep(NA, n)
  fx$prior_mean <- rep(NA_real_, n)
  fx$prior_sd <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    nm <- rownames(fx)[[i]]
    d <- if (!is.null(draws) && nm %in% colnames(draws)) draws[, nm] else numeric()
    pr <- if (!is.null(term_priors) && nm %in% names(term_priors)) {
      term_priors[[nm]]
    } else {
      list(mean = 0, sd = 1)
    }
    idx <- bayes_coef_indices(
      d, ci = ci,
      prior_mean = pr$mean %||% 0,
      prior_sd = pr$sd %||% 1,
      rope_k = rope_k
    )
    fx$ci_low[[i]] <- idx$ci_low
    fx$ci_high[[i]] <- idx$ci_high
    fx$pd[[i]] <- idx$pd
    fx$rope_low[[i]] <- idx$rope_low
    fx$rope_high[[i]] <- idx$rope_high
    fx$rope_pct[[i]] <- idx$rope_pct
    fx$rope_decision[[i]] <- idx$rope_decision
    fx$includes_zero[[i]] <- idx$includes_zero
    fx$prior_mean[[i]] <- idx$prior_mean
    fx$prior_sd[[i]] <- idx$prior_sd
  }
  fx
}

#' Pull optional Bayesian columns from a coef table row
#' @keywords internal
path_coef_bayes_fields <- function(ct, term) {
  empty <- list(
    pd = NA_real_, rope_low = NA_real_, rope_high = NA_real_,
    rope_pct = NA_real_, rope_decision = NA_character_,
    prior_mean = NA_real_, prior_sd = NA_real_, includes_zero = NA
  )
  if (is.null(ct) || !term %in% rownames(ct)) return(empty)
  getn <- function(col) {
    if (!col %in% names(ct)) return(NA_real_)
    as.numeric(ct[term, col])
  }
  getc <- function(col) {
    if (!col %in% names(ct)) return(NA_character_)
    as.character(ct[term, col])
  }
  inc <- if ("includes_zero" %in% names(ct)) {
    as.logical(ct[term, "includes_zero"])
  } else {
    NA
  }
  list(
    pd = getn("pd"),
    rope_low = getn("rope_low"),
    rope_high = getn("rope_high"),
    rope_pct = getn("rope_pct"),
    rope_decision = getc("rope_decision"),
    prior_mean = getn("prior_mean"),
    prior_sd = getn("prior_sd"),
    includes_zero = inc
  )
}
