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
  dm <- tryCatch(
    as.matrix(brms::as_draws_matrix(fit)),
    error = function(e) NULL
  )
  if (is.null(dm) || !ncol(dm)) return(numeric())
  col <- if (identical(term, "(Intercept)") || identical(term, "Intercept")) {
    "b_Intercept"
  } else {
    paste0("b_", term)
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

#' Summaries for one coefficient: mean, SD, HDI, pd, ROPE, log BF10
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
    includes_null = NA
  )
  if (!length(draws)) return(empty)
  hdi <- hdi_of(draws, ci)
  decision <- rope_hdi_decision(hdi, rope)
  list(
    est = mean(draws),
    se = stats::sd(draws),
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
    includes_null = !identical(decision, "reject")
  )
}
