# Bayesian path fits via brms.
#
# Slope priors are the normal Expected coefficients already stored on each
# structural link (prior_mean / prior_sd, and the quadratic and cubic slots).
# Intercept and residual SD keep brms' weakly informative defaults, which are
# centered on the observed outcome. Measurement models stay on the existing
# two-step factor-score path; the path stage is what brms samples.
#
# The estimate table stores the posterior mean, posterior SD, HDI, Probability of
# Direction (pd), and ROPE coverage. Exploratory links are unsupported when the
# HDI is not entirely outside the ROPE (Kruschke HDI+ROPE). No p-value is used.

#' Whether brms and a Stan backend can run
#' @keywords internal
bayes_backend_available <- function() {
  if (!requireNamespace("brms", quietly = TRUE)) return(FALSE)
  !is.na(brms_stan_backend(must = FALSE))
}

#' cmdstanr when CmdStan is installed, otherwise rstan
#' @keywords internal
brms_stan_backend <- function(must = TRUE) {
  if (requireNamespace("cmdstanr", quietly = TRUE)) {
    ok <- tryCatch({
      v <- cmdstanr::cmdstan_version()
      !is.null(v) && nzchar(as.character(v))
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return("cmdstanr")
  }
  if (requireNamespace("rstan", quietly = TRUE)) return("rstan")
  if (isTRUE(must)) {
    stop(
      "Bayesian Run needs the brms package and a Stan backend (cmdstanr or rstan).",
      call. = FALSE
    )
  }
  NA_character_
}

#' MCMC settings for one brms equation
#' @keywords internal
normalize_brms_args <- function(args = NULL) {
  args <- args %||% list()
  chains <- as.integer(args$chains %||% 2L)
  iter <- as.integer(args$iter %||% 2000L)
  warmup <- as.integer(args$warmup %||% 1000L)
  seed <- as.integer(args$seed %||% 1L)
  if (!is.finite(chains) || chains < 1L) chains <- 2L
  if (chains > 8L) chains <- 8L
  if (!is.finite(iter) || iter < 200L) iter <- 2000L
  if (!is.finite(warmup) || warmup < 50L) warmup <- min(1000L, iter %/% 2L)
  if (warmup >= iter) warmup <- max(50L, iter %/% 2L)
  if (!is.finite(seed)) seed <- 1L
  ci <- as.numeric(args$ci %||% 0.95)
  if (!is.finite(ci) || ci < 0.5 || ci >= 1) ci <- 0.95
  rope_k <- as.numeric(args$rope_k %||% 0.1)
  if (!is.finite(rope_k) || rope_k <= 0) rope_k <- 0.1
  if (rope_k > 2) rope_k <- 2
  list(
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    ci = ci,
    rope_k = rope_k,
    cores = 1L,
    refresh = 0L
  )
}

#' Whether the joint credible region for a block of coefficients excludes 0
#'
#' Uses the posterior mean and posterior covariance. The origin is outside the
#' region of probability `ci` when the Wald statistic exceeds the chi-square
#' quantile at that probability.
#' @keywords internal
cri_region_excludes_zero <- function(fit, term_names, ci = 0.95) {
  if (!length(term_names)) return(NA)
  cf <- path_fixed_coefs(fit)
  V <- path_vcov(fit)
  if (is.null(V)) return(NA)
  idx <- match(term_names, names(cf))
  idx <- idx[!is.na(idx) & is.finite(cf[idx])]
  if (!length(idx)) return(NA)
  b <- cf[idx]
  Vb <- V[idx, idx, drop = FALSE]
  W <- tryCatch(as.numeric(t(b) %*% solve(Vb, b)), error = function(e) NA_real_)
  if (!is.finite(W)) return(NA)
  W > stats::qchisq(as.numeric(ci), df = length(idx))
}

#' Edge row whose Expected coefficient is the prior for one path term
#' @keywords internal
match_prior_edge <- function(state, preds, meta) {
  if (is.null(state) || is.null(preds) || !nrow(preds) || is.null(meta)) {
    return(NULL)
  }
  rhs <- as.character(meta$rhs %||% "")
  if (!nzchar(rhs)) return(NULL)
  meta_lv <- as.character(meta$level %||% NA_character_)
  want_level <- !is.na(meta_lv) && nzchar(meta_lv) && !is_poly_level(meta_lv)
  fallback <- NULL
  for (i in seq_len(nrow(preds))) {
    from_nm <- tryCatch(
      lavaan_var_name(state, preds$from[i]),
      error = function(e) NA_character_
    )
    if (!identical(from_nm, rhs)) next
    eid <- preds$id[i]
    idx <- match(eid, state$edges$id)
    if (is.na(idx)) next
    erow <- state$edges[idx, , drop = FALSE]
    lv <- if ("level" %in% names(erow)) as.character(erow$level[[1]]) else NA_character_
    leveled <- !is.na(lv) && nzchar(lv)
    if (want_level && identical(lv, meta_lv)) return(erow)
    if (!want_level && !leveled) return(erow)
    if (is.null(fallback)) fallback <- erow
  }
  fallback
}

#' Normal(mean, sd) for one designed term, from the link's Expected coefficients
#'
#' Degree 1 uses `prior_mean` / `prior_sd`. Degrees 2 and 3 use the quadratic
#' and cubic slots. Missing links fall back to normal(0, 1).
#' @keywords internal
edge_normal_prior <- function(state, preds, meta) {
  degree <- 1L
  if (!is.null(meta$degree) && is.finite(as.numeric(meta$degree))) {
    degree <- as.integer(meta$degree)
  } else if (is_poly_level(meta$level %||% NA_character_)) {
    degree <- as.integer(sub("^\\^", "", as.character(meta$level)))
  }
  if (!is.finite(degree) || degree < 1L) degree <- 1L
  if (degree > 3L) degree <- 3L
  erow <- match_prior_edge(state, preds, meta)
  pull <- function(mean_col, sd_col) {
    m <- if (!is.null(erow) && mean_col %in% names(erow)) as.numeric(erow[[mean_col]][[1]]) else 0
    s <- if (!is.null(erow) && sd_col %in% names(erow)) as.numeric(erow[[sd_col]][[1]]) else 1
    if (!is.finite(m)) m <- 0
    if (!is.finite(s) || s <= 0) s <- 1
    list(mean = m, sd = s)
  }
  if (degree <= 1L) return(pull("prior_mean", "prior_sd"))
  if (degree == 2L) return(pull("prior_mean2", "prior_sd2"))
  pull("prior_mean3", "prior_sd3")
}

#' Write normal slope priors into a `get_prior()` table
#'
#' Class-`b` rows with an empty coef become normal(0, 1). Named coefs use the
#' matching link. Intercept, sigma, and random-effect rows are left as brms
#' set them.
#' @keywords internal
fill_brms_slope_priors <- function(prior_df, state, preds, term_meta) {
  if (is.null(prior_df) || !nrow(prior_df) || !"class" %in% names(prior_df)) {
    return(prior_df)
  }
  term_meta <- term_meta %||% list()
  for (i in seq_len(nrow(prior_df))) {
    if (!identical(as.character(prior_df$class[[i]]), "b")) next
    coef <- if ("coef" %in% names(prior_df)) {
      as.character(prior_df$coef[[i]] %||% "")
    } else {
      ""
    }
    if (is.na(coef)) coef <- ""
    if (!nzchar(coef)) next
    meta <- term_meta[[coef]]
    sp <- if (is.null(meta)) {
      list(mean = 0, sd = 1)
    } else {
      edge_normal_prior(state, preds, meta)
    }
    prior_df$prior[[i]] <- sprintf("normal(%.8g, %.8g)", sp$mean, sp$sd)
    if ("source" %in% names(prior_df)) prior_df$source[[i]] <- "user"
  }
  # A class-level `b` row is unused once every coefficient has its own prior,
  # and brms warns about it. Drop it when named slope rows exist.
  if ("coef" %in% names(prior_df)) {
    coefs <- ifelse(is.na(prior_df$coef), "", as.character(prior_df$coef))
    named_b <- prior_df$class == "b" & nzchar(coefs)
    if (any(named_b)) {
      prior_df <- prior_df[!(prior_df$class == "b" & !nzchar(coefs)), , drop = FALSE]
    }
  }
  prior_df
}

#' Named prior list for designed terms (for ROPE width)
#' @keywords internal
term_priors_from_path <- function(state, preds, term_meta) {
  out <- list()
  term_meta <- term_meta %||% list()
  for (nm in names(term_meta)) {
    sp <- edge_normal_prior(state, preds, term_meta[[nm]])
    out[[nm]] <- list(mean = sp$mean, sd = sp$sd)
  }
  out
}

#' Fit one path equation with brms
#' @keywords internal
fit_one_brms_equation <- function(yname, y_kind, formula, data, rhs, re = NULL,
                                  brms_args = NULL, state = NULL, preds = NULL,
                                  term_meta = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(
      "Bayesian Run needs the brms package (and cmdstanr or rstan).",
      call. = FALSE
    )
  }
  args <- normalize_brms_args(brms_args)
  backend <- brms_stan_backend(must = TRUE)
  binary <- identical(y_kind, "binary")
  family <- if (binary) brms::bernoulli(link = "logit") else stats::gaussian()
  fam_lab <- if (binary) "bernoulli" else "gaussian"
  prior_df <- brms::get_prior(formula, data = data, family = family)
  prior_df <- fill_brms_slope_priors(prior_df, state, preds, term_meta)
  fit <- tryCatch(
    brms::brm(
      formula = formula,
      data = data,
      family = family,
      prior = prior_df,
      chains = args$chains,
      iter = args$iter,
      warmup = args$warmup,
      cores = args$cores,
      seed = args$seed,
      refresh = args$refresh,
      backend = backend,
      silent = 2
    ),
    error = function(e) {
      stop("brms error: ", e$message, call. = FALSE)
    }
  )
  fit$smt_ci <- args$ci
  fit$smt_rope_k <- args$rope_k
  fit$smt_term_priors <- term_priors_from_path(state, preds, term_meta)
  rhs_txt <- if (!is.null(re) && nzchar(re)) {
    paste(rhs, re, sep = " + ")
  } else {
    rhs
  }
  prior_bits <- character()
  if (!is.null(prior_df) && nrow(prior_df)) {
    for (i in seq_len(nrow(prior_df))) {
      if (!identical(as.character(prior_df$class[[i]]), "b")) next
      coef <- as.character(prior_df$coef[[i]] %||% "")
      if (is.na(coef) || !nzchar(coef)) next
      prior_bits <- c(prior_bits, paste0(coef, " ~ ", prior_df$prior[[i]]))
    }
  }
  syntax <- paste0("brms(", fam_lab, "): ", yname, " ~ ", rhs_txt)
  if (length(prior_bits)) {
    syntax <- paste0(syntax, "\n# priors: ", paste(prior_bits, collapse = "; "))
  }
  rh <- tryCatch(brms::rhat(fit), error = function(e) NULL)
  if (!is.null(rh)) {
    rh <- rh[is.finite(rh)]
    if (length(rh) && max(rh) > 1.05) {
      syntax <- paste0(
        syntax,
        sprintf("\n# warning: max Rhat %.3f (chains may not have mixed)", max(rh))
      )
    }
  }
  list(fit = fit, syntax = syntax, engine = "brms")
}

#' Fit the structural model and mark links by HDI + ROPE
#'
#' Same graph and data contract as [fit_pvalue_model()]. `ci` is the HDI mass
#' (default 0.95). The ROPE for each slope is
#' `[-rope_k * prior_sd, +rope_k * prior_sd]` from the link's Expected SD.
#' Exploratory links are excluded when the HDI is not entirely outside the
#' ROPE. Confirmatory links stay in the model and are muted in that case.
#'
#' @param chains,iter,warmup,seed MCMC settings passed to brms. One core is
#'   used so sampling stays inside the Shiny process.
#' @export
fit_bayes_model <- function(state, data, ci = 0.95, rope_k = 0.1,
                            mark_nonsig = TRUE,
                            chains = 2L, iter = 2000L, warmup = 1000L, seed = 1L,
                            latent_score_method = "own") {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(
      "Bayesian Run needs the brms package (and cmdstanr or rstan).",
      call. = FALSE
    )
  }
  brms_stan_backend(must = TRUE)
  latent_score_method <- normalize_latent_score_method(latent_score_method, state)
  edge_ids <- analyzable_edge_ids(state)
  all_display_ids <- unique(c(edge_ids, state$edges$id[
    state$edges$type == "definitional" |
      vapply(state$edges$to, function(id) is_interaction_node(state, id), logical(1))
  ]))
  brms_args <- normalize_brms_args(list(
    chains = chains, iter = iter, warmup = warmup, seed = seed,
    ci = ci, rope_k = rope_k
  ))
  fitted <- fit_structural_model(
    state, data,
    include_edge_ids = edge_ids,
    latent_score_method = latent_score_method,
    backend = "brms",
    brms_args = brms_args
  )
  state2 <- apply_estimates_to_edges(
    state, fitted$estimates, included_edge_ids = all_display_ids
  )
  state2 <- apply_r_squared_to_nodes(state2, fitted$r_squared)
  state2 <- apply_unidimensionality_to_nodes(
    state2, fitted$measurement_unidimensionality
  )
  if (mark_nonsig && nrow(state2$edges)) {
    for (i in seq_len(nrow(state2$edges))) {
      if (state2$edges$type[i] != "exploratory") next
      unsupported <- if ("rope_decision" %in% names(state2$edges)) {
        !identical(as.character(state2$edges$rope_decision[i]), "reject") &&
          !is.na(state2$edges$rope_decision[i])
      } else {
        isTRUE(state2$edges$includes_zero[i])
      }
      if (!unsupported) next
      state2$edges$status[i] <- "excluded"
      est_val <- state2$edges$estimate[i]
      lo <- if ("ci_low" %in% names(state2$edges)) state2$edges$ci_low[i] else NA_real_
      hi <- if ("ci_high" %in% names(state2$edges)) state2$edges$ci_high[i] else NA_real_
      pd <- if ("pd" %in% names(state2$edges)) state2$edges$pd[i] else NA_real_
      lv <- if ("level" %in% names(state2$edges)) state2$edges$level[i] else NA_character_
      prefix <- "E"
      if (!is.na(lv) && nzchar(lv)) prefix <- paste0(prefix, "·", lv)
      pd_txt <- if (is.finite(pd)) sprintf(" pd=%.0f%%", 100 * pd) else ""
      state2$edges$label[i] <- if (is.finite(est_val) && is.finite(lo) && is.finite(hi)) {
        sprintf("%s %.2f [%.2f, %.2f]%s \u2715", prefix, est_val, lo, hi, pd_txt)
      } else if (is.finite(est_val)) {
        sprintf("%s %.2f%s \u2715", prefix, est_val, pd_txt)
      } else {
        paste0(prefix, " \u2715")
      }
    }
  }

  list(
    state = state2,
    fit = fitted$fit,
    estimates = fitted$estimates,
    fit_measures = fitted$fit_measures,
    syntax = fitted$syntax,
    comparison = NULL,
    engine = fitted$engine,
    sampler = "brms",
    pvalue_kind = "hdi_rope",
    ci_level = brms_args$ci,
    rope_k = brms_args$rope_k,
    uncertainty = list(method = "none"),
    conf_adjust = "none",
    two_step = isTRUE(fitted$two_step),
    measurement_fit = fitted$measurement_fit,
    measurement_score_weights = fitted$measurement_score_weights,
    measurement_score_fits = fitted$measurement_score_fits,
    measurement_cronbach_alpha = fitted$measurement_cronbach_alpha,
    measurement_unidimensionality = fitted$measurement_unidimensionality,
    latent_score_method = fitted$latent_score_method %||% latent_score_method
  )
}
