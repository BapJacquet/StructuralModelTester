#' Coefficient row key for aligning draws
#' @keywords internal
estimate_row_key <- function(lhs, op, rhs, level = NA_character_, kind = "effect") {
  if (is.null(level) || length(level) != 1L || is.na(level)) level <- ""
  if (is.null(kind) || length(kind) != 1L || is.na(kind) || !nzchar(kind)) kind <- "effect"
  paste(lhs, op, rhs, as.character(level), as.character(kind), sep = "\1")
}

#' Keys for an estimates data frame
#' @keywords internal
estimates_keys <- function(est) {
  if (is.null(est) || !nrow(est)) return(character())
  lv <- if ("level" %in% names(est)) est$level else rep(NA_character_, nrow(est))
  kd <- if ("kind" %in% names(est)) est$kind else rep("effect", nrow(est))
  vapply(seq_len(nrow(est)), function(i) {
    estimate_row_key(est$lhs[i], est$op[i], est$rhs[i], lv[i], kd[i])
  }, character(1))
}

#' Collect numeric estimates vector aligned to keys
#' @keywords internal
estimates_to_named <- function(est) {
  if (is.null(est) || !nrow(est)) return(numeric())
  keys <- estimates_keys(est)
  vals <- as.numeric(est$est)
  # Prefer effect rows; if duplicate keys, first wins
  keep <- !duplicated(keys)
  setNames(vals[keep], keys[keep])
}

#' Wald p-value from estimate and SE
#' @keywords internal
wald_pvalue <- function(est, se) {
  if (!is.finite(est) || !is.finite(se) || se <= 0) return(NA_real_)
  2 * stats::pnorm(-abs(est / se))
}

#' Replace SEs (and Wald p) from a matrix of draws; keep point estimates
#'
#' @param base_est Estimates from the final model
#' @param draw_mat Matrix draws × coefficient keys (colnames = keys)
#' @param se_mat Optional draws × keys of within-draw SEs (for Rubin's rules)
#' @keywords internal
refine_estimates_from_draws <- function(base_est, draw_mat, method = "bootstrap",
                                        se_mat = NULL) {
  if (is.null(base_est) || !nrow(base_est)) return(base_est)
  if (is.null(draw_mat) || !nrow(draw_mat) || !ncol(draw_mat)) return(base_est)
  keys <- estimates_keys(base_est)
  M <- nrow(draw_mat)
  for (i in seq_len(nrow(base_est))) {
    k <- keys[[i]]
    if (!k %in% colnames(draw_mat)) next
    xs_all <- as.numeric(draw_mat[, k])
    ok <- is.finite(xs_all)
    xs <- xs_all[ok]
    if (length(xs) < 2L) next
    B <- stats::var(xs)
    if (!is.finite(B) || B < 0) B <- 0
    if (!is.null(se_mat) && k %in% colnames(se_mat)) {
      # Rubin's rules: T = W + (1 + 1/M) B
      ses <- as.numeric(se_mat[, k])[ok]
      ses <- ses[is.finite(ses) & ses >= 0]
      if (length(ses) >= 1L) {
        W <- mean(ses^2)
        Tvar <- W + (1 + 1 / length(xs)) * B
        se <- sqrt(max(Tvar, 0))
      } else {
        se <- sqrt(B)
      }
    } else {
      # Bootstrap: SE = sd of replicate estimates
      se <- stats::sd(xs)
    }
    if (!is.finite(se) || se < 0) next
    base_est$se[i] <- se
    if (identical(base_est$op[i], "~") || identical(base_est$op[i], "=~")) {
      if (is.finite(base_est$est[i])) {
        base_est$pvalue[i] <- wald_pvalue(base_est$est[i], se)
      } else if ("kind" %in% names(base_est) && identical(base_est$kind[i], "omnibus")) {
        next
      }
    }
  }
  attr(base_est, "uncertainty_method") <- method
  attr(base_est, "uncertainty_draws") <- M
  base_est
}

#' Named vector of SEs from an estimates table
#' @keywords internal
estimates_se_to_named <- function(est) {
  if (is.null(est) || !nrow(est)) return(numeric())
  keys <- estimates_keys(est)
  vals <- as.numeric(est$se)
  keep <- !duplicated(keys)
  setNames(vals[keep], keys[keep])
}

#' Default draw counts
#' @keywords internal
uncertainty_default_draws <- function(method) {
  switch(
    as.character(method %||% "none"),
    bootstrap = 199L,
    plausible = 20L,
    0L
  )
}

#' Append uncertainty note to syntax
#' @keywords internal
append_uncertainty_syntax_note <- function(syntax, method, draws, ok_draws) {
  note <- switch(
    method,
    bootstrap = sprintf(
      "# post-selection uncertainty: nonparametric bootstrap (R=%d, ok=%d)\n",
      as.integer(draws), as.integer(ok_draws)
    ),
    plausible = sprintf(
      "# post-selection uncertainty: plausible factor-score values (M=%d, ok=%d)\n",
      as.integer(draws), as.integer(ok_draws)
    ),
    ""
  )
  paste0(note, syntax)
}

#' Nonparametric bootstrap of the final structural model (post-selection)
#' @keywords internal
bootstrap_final_model <- function(state, data, include_edge_ids, base_fitted,
                                  draws = 199L, progress = NULL,
                                  latent_score_method = "own") {
  draws <- as.integer(draws)
  if (!is.finite(draws) || draws < 2L) {
    stop("Bootstrap needs at least 2 draws.", call. = FALSE)
  }
  latent_score_method <- normalize_latent_score_method(
    base_fitted$latent_score_method %||% latent_score_method,
    state
  )
  keys0 <- names(estimates_to_named(base_fitted$estimates))
  if (!length(keys0)) {
    return(list(estimates = base_fitted$estimates, ok = 0L, draws = draws))
  }
  n <- nrow(data)
  if (is.null(n) || n < 2L) {
    stop("Bootstrap needs at least 2 rows of data.", call. = FALSE)
  }
  mat <- matrix(NA_real_, nrow = draws, ncol = length(keys0))
  colnames(mat) <- keys0
  ok <- 0L
  for (b in seq_len(draws)) {
    if (is.function(progress)) progress(b, draws)
    idx <- sample.int(n, n, replace = TRUE)
    boot_dat <- data[idx, , drop = FALSE]
    res <- tryCatch(
      fit_structural_model(
        state, boot_dat,
        include_edge_ids = include_edge_ids,
        latent_score_method = latent_score_method
      ),
      error = function(e) NULL
    )
    if (is.null(res) || is.null(res$estimates)) next
    named <- estimates_to_named(res$estimates)
    hit <- intersect(names(named), keys0)
    if (!length(hit)) next
    ok <- ok + 1L
    mat[ok, hit] <- named[hit]
  }
  if (ok < 2L) {
    stop(
      "Bootstrap failed: fewer than 2 successful draws. Check the model/data.",
      call. = FALSE
    )
  }
  mat <- mat[seq_len(ok), , drop = FALSE]
  est2 <- refine_estimates_from_draws(base_fitted$estimates, mat, method = "bootstrap")
  list(estimates = est2, ok = ok, draws = draws, draw_matrix = mat)
}

#' Draw one plausible factor-score matrix (n × nlat) given lavPredict se/acov
#' @keywords internal
draw_plausible_score_matrix <- function(score_mean, acov) {
  n <- nrow(score_mean)
  nlat <- ncol(score_mean)
  if (is.null(acov) || !is.matrix(acov)) {
    # Fallback: small ridge on residual variance
    acov <- diag(nlat) * 0.01
  }
  if (!all(dim(acov) == c(nlat, nlat))) {
    acov <- diag(nlat) * mean(diag(as.matrix(acov)), na.rm = TRUE)
  }
  # Stabilize
  acov <- as.matrix(acov)
  acov <- (acov + t(acov)) / 2
  eg <- eigen(acov, symmetric = TRUE)
  eg$values <- pmax(eg$values, 1e-8)
  chol_like <- eg$vectors %*% diag(sqrt(eg$values), nrow = nlat) %*% t(eg$vectors)
  noise <- matrix(stats::rnorm(n * nlat), n, nlat) %*% t(chol_like)
  out <- as.matrix(score_mean) + noise
  colnames(out) <- colnames(score_mean)
  out
}

#' Fit path stage only with supplied latent score columns already in dat
#' @keywords internal
fit_path_with_given_scores <- function(state, dat_with_scores, include_edge_ids,
                                       latent_ids, measurement_estimates = NULL,
                                       measurement_syntax = "") {
  state_path <- state_with_latent_as_scores(state, latent_ids)
  edges <- state$edges[state$edges$id %in% include_edge_ids, , drop = FALSE]
  struct_edges <- edges[edges$type != "measurement", , drop = FALSE]
  dv_ids <- structural_dv_ids(state, state$edges)
  dat_path <- prepare_analysis_data(state_path, dat_with_scores)
  path_res <- fit_path_equations(
    state_path, dat_path, struct_edges, dv_ids = dv_ids
  )
  est <- path_res$estimates
  if (!is.null(measurement_estimates) && nrow(measurement_estimates)) {
    est <- rbind(measurement_estimates, est)
  }
  path_res$estimates <- est
  path_res$syntax <- paste0(
    "# path with plausible / fixed measurement scores\n",
    if (nzchar(measurement_syntax)) paste0("# measurement\n", measurement_syntax, "\n") else "",
    path_res$syntax
  )
  path_res
}

#' Plausible values for latents, then path engine (post-selection)
#'
#' CFA is fixed from the original sample; factor scores are redrawn from
#' N(Fhat, prediction-error cov); structural paths re-fit each time.
#' @keywords internal
plausible_values_final_model <- function(state, data, include_edge_ids, base_fitted,
                                         draws = 20L, progress = NULL,
                                         latent_score_method = "own") {
  draws <- as.integer(draws)
  if (!is.finite(draws) || draws < 2L) {
    stop("Plausible values need at least 2 draws.", call. = FALSE)
  }
  if (!model_uses_latents(state, state$edges)) {
    stop(
      "Plausible values require latents / measurement links. Use bootstrap instead.",
      call. = FALSE
    )
  }
  latent_score_method <- normalize_latent_score_method(
    base_fitted$latent_score_method %||% latent_score_method,
    state
  )
  dat0 <- prepare_analysis_data(state, data)
  attached <- attach_latent_factor_scores(
    state, dat0, score_method = latent_score_method
  )
  base_dat <- attached$data
  score_cols <- vapply(
    attached$latent_ids,
    function(id) lavaan_var_name(state, id),
    character(1)
  )
  if (!all(score_cols %in% names(base_dat))) {
    stop("Plausible values: attached factor-score columns are missing.", call. = FALSE)
  }
  score_mean <- as.matrix(base_dat[, score_cols, drop = FALSE])
  colnames(score_mean) <- score_cols

  # Prediction-error SEs from the score fit(s). Off-diagonals stay 0 for own
  # scores; joint Thomson still uses a diagonal PV draw (conservative).
  se_diag <- rep(0.05, length(score_cols))
  names(se_diag) <- score_cols
  fits <- attached$measurement_score_fits
  if (is.list(fits) && length(fits)) {
    for (j in seq_along(score_cols)) {
      nm <- score_cols[[j]]
      f1 <- fits[[nm]]
      if (!inherits(f1, "lavaan")) next
      sc1 <- tryCatch(
        lavaan::lavPredict(f1, type = "lv", method = "regression", se = TRUE),
        error = function(e) NULL
      )
      if (is.null(sc1)) next
      se1 <- tryCatch({
        se_attr <- attr(sc1, "se")[[1]]
        if (is.null(se_attr)) {
          NULL
        } else if (is.matrix(se_attr) || is.data.frame(se_attr)) {
          cn <- colnames(se_attr)
          if (!is.null(cn) && nm %in% cn) se_attr[, nm] else se_attr[, min(j, ncol(se_attr))]
        } else if (is.list(se_attr) && !is.null(names(se_attr)) && nm %in% names(se_attr)) {
          se_attr[[nm]]
        } else {
          se_attr
        }
      }, error = function(e) NULL)
      if (!is.null(se1) && length(se1) && is.finite(mean(as.numeric(se1), na.rm = TRUE))) {
        se_diag[[nm]] <- mean(as.numeric(se1), na.rm = TRUE)
      }
    }
  }
  acov <- diag(as.numeric(se_diag)^2, nrow = length(score_cols))
  dimnames(acov) <- list(score_cols, score_cols)

  keys0 <- names(estimates_to_named(base_fitted$estimates))
  # Structural keys only for PV path refits (measurement stay fixed)
  struct_keys <- keys0[vapply(keys0, function(k) {
    parts <- strsplit(k, "\1", fixed = TRUE)[[1]]
    length(parts) >= 2L && identical(parts[[2]], "~")
  }, logical(1))]
  if (!length(struct_keys)) struct_keys <- keys0

  mat <- matrix(NA_real_, nrow = draws, ncol = length(struct_keys))
  se_mat <- matrix(NA_real_, nrow = draws, ncol = length(struct_keys))
  colnames(mat) <- struct_keys
  colnames(se_mat) <- struct_keys
  ok <- 0L
  meas_est <- attached$measurement_estimates

  for (m in seq_len(draws)) {
    if (is.function(progress)) progress(m, draws)
    drawn <- draw_plausible_score_matrix(score_mean, acov)
    dat_m <- base_dat
    for (j in seq_along(score_cols)) {
      dat_m[[score_cols[j]]] <- as.numeric(drawn[, j])
    }
    res <- tryCatch(
      fit_path_with_given_scores(
        state, dat_m, include_edge_ids,
        latent_ids = attached$latent_ids,
        measurement_estimates = meas_est,
        measurement_syntax = attached$measurement_syntax
      ),
      error = function(e) NULL
    )
    if (is.null(res) || is.null(res$estimates)) next
    named <- estimates_to_named(res$estimates)
    named_se <- estimates_se_to_named(res$estimates)
    hit <- intersect(names(named), struct_keys)
    if (!length(hit)) next
    ok <- ok + 1L
    mat[ok, hit] <- named[hit]
    hit_se <- intersect(names(named_se), hit)
    if (length(hit_se)) se_mat[ok, hit_se] <- named_se[hit_se]
  }
  if (ok < 2L) {
    stop(
      "Plausible values failed: fewer than 2 successful path draws.",
      call. = FALSE
    )
  }
  mat <- mat[seq_len(ok), , drop = FALSE]
  se_mat <- se_mat[seq_len(ok), , drop = FALSE]
  est2 <- refine_estimates_from_draws(
    base_fitted$estimates, mat, method = "plausible", se_mat = se_mat
  )
  list(estimates = est2, ok = ok, draws = draws, draw_matrix = mat)
}

#' Apply post-selection uncertainty to a fitted result list
#' @keywords internal
apply_postselection_uncertainty <- function(state, data, include_edge_ids, fitted,
                                            method = c("none", "bootstrap", "plausible"),
                                            draws = NULL, progress = NULL,
                                            latent_score_method = "own") {
  method <- match.arg(method)
  latent_score_method <- normalize_latent_score_method(
    fitted$latent_score_method %||% latent_score_method,
    state
  )
  if (identical(method, "none")) {
    fitted$uncertainty <- list(method = "none", draws = 0L, ok = 0L)
    return(fitted)
  }
  if (is.null(draws) || !is.finite(as.numeric(draws))) {
    draws <- uncertainty_default_draws(method)
  }
  draws <- as.integer(draws)

  out <- if (identical(method, "bootstrap")) {
    bootstrap_final_model(
      state, data, include_edge_ids, fitted,
      draws = draws, progress = progress,
      latent_score_method = latent_score_method
    )
  } else {
    plausible_values_final_model(
      state, data, include_edge_ids, fitted,
      draws = draws, progress = progress,
      latent_score_method = latent_score_method
    )
  }
  fitted$estimates <- out$estimates
  fitted$syntax <- append_uncertainty_syntax_note(
    fitted$syntax %||% "", method, draws, out$ok
  )
  fitted$uncertainty <- list(
    method = method,
    draws = draws,
    ok = out$ok
  )
  fitted
}
