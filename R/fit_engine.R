#' Classify an observed variable for model choice
#' @return "binary", "categorical", or "continuous"
#' @keywords internal
classify_variable <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("continuous")
  if (is.logical(x)) return("binary")
  if (is.factor(x) || is.character(x)) {
    n <- length(unique(as.character(x)))
    if (n <= 1L) return("continuous")
    if (n == 2L) return("binary")
    return("categorical")
  }
  u <- unique(x)
  if (length(u) == 2L) return("binary")
  "continuous"
}

#' Unique levels present in data (character)
#' @keywords internal
data_levels_chr <- function(x) {
  if (is.factor(x)) {
    return(levels(droplevels(x[!is.na(x)])))
  }
  sort(unique(as.character(stats::na.omit(x))))
}

#' Merge a preferred level order with levels observed in data
#' @keywords internal
merge_level_order <- function(preferred, observed) {
  preferred <- as.character(preferred %||% character())
  preferred <- preferred[nzchar(preferred)]
  observed <- as.character(observed %||% character())
  observed <- observed[nzchar(observed)]
  keep <- preferred[preferred %in% observed]
  c(keep, setdiff(observed, keep))
}

#' Reference coding for a variable (base + non-reference levels)
#'
#' Categorical (3+ levels): base = first level after applying optional overrides.
#' Binary/continuous: no per-level ports (`levels` empty) unless
#' `force_factor = TRUE` (binary factor with chosen base).
#'
#' @param base Preferred reference level (must appear in data).
#' @param levels_order Preferred full level order.
#' @keywords internal
variable_coding <- function(x, base = NULL, levels_order = NULL, force_factor = FALSE) {
  kind <- classify_variable(x)
  if (!identical(kind, "categorical") && !(isTRUE(force_factor) && identical(kind, "binary"))) {
    return(list(
      kind = kind, base = NA_character_, levels = character(),
      all_levels = character(), ordered = FALSE
    ))
  }
  observed <- data_levels_chr(x)
  if (length(observed) < 2L) {
    return(list(
      kind = "continuous", base = NA_character_, levels = character(),
      all_levels = character(), ordered = FALSE
    ))
  }
  levs <- merge_level_order(levels_order, observed)
  base <- as.character(base %||% NA_character_)
  if (length(base) != 1L || is.na(base) || !nzchar(base) || !base %in% levs) {
    base <- levs[[1]]
  }
  levs <- c(base, setdiff(levs, base))
  # Ports / dummies: categorical (3+) always; binary only when force_factor
  # (split mode / leveled wires). Otherwise binary keeps empty `levels`.
  if (identical(kind, "binary") && !isTRUE(force_factor)) {
    return(list(
      kind = kind, base = levs[[1]], levels = character(),
      all_levels = levs, ordered = FALSE
    ))
  }
  if (identical(kind, "binary")) {
    return(list(
      kind = "binary", base = levs[[1]], levels = levs[-1],
      all_levels = levs, ordered = FALSE
    ))
  }
  list(
    kind = "categorical",
    base = levs[[1]],
    levels = levs[-1],
    all_levels = levs,
    ordered = FALSE
  )
}

#' Coerce a vector to a factor using coding overrides
#' @keywords internal
coerce_with_factor_coding <- function(x, base = NULL, levels_order = NULL, ordered = FALSE) {
  observed <- data_levels_chr(x)
  if (length(observed) < 2L) return(x)
  levs <- merge_level_order(levels_order, observed)
  base <- as.character(base %||% NA_character_)
  if (length(base) != 1L || is.na(base) || !nzchar(base) || !base %in% levs) {
    base <- levs[[1]]
  }
  levs <- c(base, setdiff(levs, base))
  factor(as.character(x), levels = levs, ordered = isTRUE(ordered))
}

#' Safe dummy column name for a contrast level
#' @keywords internal
dummy_col_name <- function(var, level) {
  paste0(sanitize_id(var), "__", sanitize_id(level))
}

#' Integer scores 1..K for an ordered factor (level order)
#'
#' Used when an ordered categorical predictor has a polynomial `rel_kind`
#' (quadratic / cubic): SMT expands linear + centered powers of these scores.
#'
#' Scores follow `levels_order` as the ordinal sequence (e.g. None < Low <
#' High). Unlike treatment coding, the reference `base` is **not** moved to
#' position 1 — that would scramble polynomial scores.
#' @keywords internal
ordered_level_scores <- function(x, levels_order = NULL, base = NULL) {
  observed <- data_levels_chr(x)
  if (length(observed) < 2L) {
    return(suppressWarnings(as.numeric(x)))
  }
  levs <- merge_level_order(levels_order, observed)
  if (!length(levs)) {
    # No declared order: fall back to base-first coerce (legacy)
    f <- coerce_with_factor_coding(
      x,
      base = base,
      levels_order = levels_order,
      ordered = TRUE
    )
    return(as.numeric(f))
  }
  as.numeric(factor(as.character(x), levels = levs, ordered = TRUE))
}

#' True if the model involves latent / measurement structure
#' @keywords internal
model_uses_latents <- function(state, edges = state$edges) {
  if (!nrow(state$nodes) || !nrow(edges)) return(FALSE)
  if (any(edges$type == "measurement")) return(TRUE)
  used <- unique(c(edges$from, edges$to))
  any(vapply(used, function(id) is_latent_node(state, id), logical(1)))
}

#' Count latent nodes in the graph
#' @keywords internal
n_latent_nodes <- function(state) {
  state <- ensure_node_kind_columns(state)
  if (!nrow(state$nodes)) return(0L)
  sum(vapply(state$nodes$id, function(id) is_latent_node(state, id), logical(1)))
}

#' Normalize latent scoring method
#'
#' `"own"` (default): each latent scored from its own indicators only.
#' `"joint"`: Thomson/regression scores from the joint CFA (may weight other
#' constructs' items when factors correlate). With fewer than 2 latents, both
#' coincide and `"own"` is used.
#' @keywords internal
normalize_latent_score_method <- function(method, state = NULL) {
  method <- as.character(method %||% "own")[[1]]
  if (!method %in% c("own", "joint")) method <- "own"
  if (!is.null(state) && n_latent_nodes(state) < 2L) return("own")
  method
}

#' Cronbach's alpha for a numeric item matrix
#'
#' Classical formula on the item covariance matrix (complete cases among the
#' supplied columns). Needs ≥ 2 items with finite variances; otherwise `NA`.
#' @keywords internal
cronbach_alpha <- function(items) {
  if (is.null(items)) return(NA_real_)
  if (!is.data.frame(items) && !is.matrix(items)) {
    items <- as.data.frame(items, stringsAsFactors = FALSE)
  }
  items <- as.data.frame(lapply(items, function(x) suppressWarnings(as.numeric(x))),
                         stringsAsFactors = FALSE)
  items <- items[stats::complete.cases(items), , drop = FALSE]
  k <- ncol(items)
  if (k < 2L || nrow(items) < 2L) return(NA_real_)
  S <- tryCatch(stats::cov(items), error = function(e) NULL)
  if (is.null(S) || !is.matrix(S) || nrow(S) != k) return(NA_real_)
  tot <- sum(S)
  item_var <- sum(diag(S))
  if (!is.finite(tot) || !is.finite(item_var) || abs(tot) < .Machine$double.eps) {
    return(NA_real_)
  }
  as.numeric((k / (k - 1)) * (1 - item_var / tot))
}

#' PCA unidimensionality screen for a numeric item matrix
#'
#' Correlation-matrix PCA on complete cases. Returns the first/second
#' eigenvalue ratio and a coarse status for canvas / Model-fit indicators:
#' - `"ok"`: λ₁/λ₂ ≥ 3 (essentially unidimensional)
#' - `"warn"`: 2 ≤ λ₁/λ₂ < 3 (weak / borderline)
#' - `"fail"`: λ₁/λ₂ < 2 with ≥ 3 items (possible second dimension)
#' - `NA` status when incomputable (< 2 items, etc.)
#' @keywords internal
pca_unidimensionality <- function(items) {
  empty <- list(
    status = NA_character_,
    label = "",
    ratio = NA_real_,
    pct1 = NA_real_,
    n_items = 0L,
    n = 0L,
    lambda1 = NA_real_,
    lambda2 = NA_real_,
    eigenvalues = numeric(),
    loadings = NULL,
    loadings_rotated = NULL,
    n_rotated = 0L
  )
  if (is.null(items)) return(empty)
  if (!is.data.frame(items) && !is.matrix(items)) {
    items <- as.data.frame(items, stringsAsFactors = FALSE)
  }
  items <- as.data.frame(
    lapply(items, function(x) suppressWarnings(as.numeric(x))),
    stringsAsFactors = FALSE
  )
  items <- items[stats::complete.cases(items), , drop = FALSE]
  k <- ncol(items)
  n <- nrow(items)
  empty$n_items <- as.integer(k)
  empty$n <- as.integer(n)
  if (k < 2L || n < 3L) return(empty)

  R <- tryCatch(stats::cor(items), error = function(e) NULL)
  if (is.null(R) || !is.matrix(R) || nrow(R) != k) return(empty)
  if (any(!is.finite(R))) return(empty)
  eg <- tryCatch(
    eigen(R, symmetric = TRUE, only.values = FALSE),
    error = function(e) NULL
  )
  if (is.null(eg) || length(eg$values) < 2L) return(empty)
  ev <- as.numeric(eg$values)
  if (!all(is.finite(ev[1:2])) || ev[[2]] <= .Machine$double.eps) return(empty)
  # Component loadings = eigenvector × √eigenvalue (correlation PCA)
  vec <- eg$vectors
  if (is.null(dim(vec))) vec <- matrix(vec, ncol = 1L)
  loadings <- vec
  for (j in seq_len(ncol(vec))) {
    s <- if (is.finite(ev[[j]]) && ev[[j]] > 0) sqrt(ev[[j]]) else 0
    loadings[, j] <- vec[, j] * s
    # Flip sign so the component loads mostly positive (readable)
    if (sum(loadings[, j], na.rm = TRUE) < 0) {
      loadings[, j] <- -loadings[, j]
    }
  }
  item_nms <- names(items)
  if (is.null(item_nms) || length(item_nms) != k) {
    item_nms <- colnames(R)
  }
  if (is.null(item_nms) || length(item_nms) != k) {
    item_nms <- paste0("item", seq_len(k))
  }
  rownames(loadings) <- item_nms
  colnames(loadings) <- paste0("PC", seq_len(ncol(loadings)))

  # Varimax only when Kaiser keeps ≥ 2 components (λ > 1). A single
  # component makes rotation irrelevant / misleading.
  n_rot <- as.integer(sum(ev > 1))
  n_rot <- max(0L, min(n_rot, k, ncol(loadings)))
  loadings_rot <- NULL
  if (n_rot >= 2L) {
    rot <- tryCatch(
      stats::varimax(loadings[, seq_len(n_rot), drop = FALSE], normalize = TRUE),
      error = function(e) NULL
    )
    if (!is.null(rot) && !is.null(rot$loadings)) {
      loadings_rot <- matrix(
        as.numeric(rot$loadings),
        nrow = nrow(loadings),
        ncol = n_rot,
        dimnames = list(item_nms, paste0("RC", seq_len(n_rot)))
      )
      for (j in seq_len(n_rot)) {
        if (sum(loadings_rot[, j], na.rm = TRUE) < 0) {
          loadings_rot[, j] <- -loadings_rot[, j]
        }
      }
    } else {
      n_rot <- 0L
    }
  } else {
    n_rot <- 0L
  }

  ratio <- ev[[1]] / ev[[2]]
  pct1 <- ev[[1]] / sum(ev)
  status <- if (ratio >= 3) {
    "ok"
  } else if (ratio >= 2) {
    "warn"
  } else if (k >= 3L) {
    "fail"
  } else {
    # 2 items with weak correlation — borderline, not a clear second factor
    "warn"
  }
  label <- switch(
    status,
    ok = "uni \u2713",
    warn = "weak 1D",
    fail = "multi ?",
    ""
  )
  list(
    status = status,
    label = label,
    ratio = as.numeric(ratio),
    pct1 = as.numeric(pct1),
    n_items = as.integer(k),
    n = as.integer(n),
    lambda1 = as.numeric(ev[[1]]),
    lambda2 = as.numeric(ev[[2]]),
    eigenvalues = ev,
    loadings = loadings,
    loadings_rotated = loadings_rot,
    n_rotated = as.integer(n_rot)
  )
}

#' Cronbach's alpha for each latent from its own measurement indicators
#'
#' @return Named numeric vector (lavaan latent names). Missing / incomputable → NA.
#' @keywords internal
cronbach_alpha_for_latents <- function(state, dat, latent_ids = NULL, meas = NULL) {
  state <- ensure_node_kind_columns(state)
  if (is.null(meas)) {
    meas <- state$edges[state$edges$type == "measurement", , drop = FALSE]
  }
  if (is.null(latent_ids)) {
    latent_ids <- unique(as.character(meas$from))
  }
  latent_ids <- unique(as.character(latent_ids))
  out <- setNames(rep(NA_real_, length(latent_ids)), character(0))
  if (!length(latent_ids) || is.null(dat) || !NROW(dat) || !nrow(meas)) {
    return(setNames(numeric(0), character(0)))
  }
  alphas <- numeric(length(latent_ids))
  names(alphas) <- vapply(
    latent_ids,
    function(id) lavaan_var_name(state, id),
    character(1)
  )
  for (i in seq_along(latent_ids)) {
    lat <- latent_ids[[i]]
    lat_nm <- names(alphas)[[i]]
    ind_ids <- unique(as.character(meas$to[meas$from == lat]))
    ind_cols <- vapply(ind_ids, function(id) lavaan_var_name(state, id), character(1))
    ind_cols <- ind_cols[ind_cols %in% names(dat)]
    if (length(ind_cols) < 2L) {
      alphas[[lat_nm]] <- NA_real_
      next
    }
    alphas[[lat_nm]] <- cronbach_alpha(dat[, ind_cols, drop = FALSE])
  }
  alphas
}

#' PCA unidimensionality screen for each latent (own indicators)
#'
#' @return Named list keyed by **node id** (for canvas) with slots
#'   `status`, `label`, `ratio`, `pct1`, `n_items`, plus `lavaan_name`.
#' @keywords internal
unidimensionality_for_latents <- function(state, dat, latent_ids = NULL,
                                          meas = NULL) {
  state <- ensure_node_kind_columns(state)
  if (is.null(meas)) {
    meas <- state$edges[state$edges$type == "measurement", , drop = FALSE]
  }
  if (is.null(latent_ids)) {
    latent_ids <- unique(as.character(meas$from))
  }
  latent_ids <- unique(as.character(latent_ids))
  out <- list()
  if (!length(latent_ids) || is.null(dat) || !NROW(dat) || !nrow(meas)) {
    return(out)
  }
  for (lat in latent_ids) {
    ind_ids <- unique(as.character(meas$to[meas$from == lat]))
    ind_cols <- vapply(ind_ids, function(id) lavaan_var_name(state, id), character(1))
    ind_cols <- ind_cols[ind_cols %in% names(dat)]
    slot <- pca_unidimensionality(
      if (length(ind_cols)) dat[, ind_cols, drop = FALSE] else NULL
    )
    slot$lavaan_name <- lavaan_var_name(state, lat)
    out[[lat]] <- slot
  }
  out
}

#' Structural dependent-variable node ids (targets of non-measurement links)
#' @keywords internal
structural_dv_ids <- function(state, edges = state$edges) {
  struct <- edges[edges$type != "measurement", , drop = FALSE]
  if (!nrow(struct)) return(character())
  tos <- unique(struct$to)
  tos[!vapply(tos, function(id) is_interaction_node(state, id), logical(1))]
}

#' Coerce a binary vector to 0/1 for glm / path predictors
#' @keywords internal
as_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) return(as.integer(droplevels(x)) - 1L)
  u <- sort(unique(stats::na.omit(as.character(x))))
  if (length(u) != 2L) stop("Expected a binary variable.", call. = FALSE)
  as.integer(as.character(x) == u[2L])
}

#' Empty estimates table with optional contrast metadata
#' @keywords internal
empty_estimates <- function() {
  data.frame(
    lhs = character(),
    op = character(),
    rhs = character(),
    est = numeric(),
    se = numeric(),
    pvalue = numeric(),
    est_std = numeric(),
    df = numeric(),
    level = character(),
    kind = character(),
    ci_low = numeric(),
    ci_high = numeric(),
    ci_level = numeric(),
    includes_zero = logical(),
    pd = numeric(),
    rope_low = numeric(),
    rope_high = numeric(),
    rope_pct = numeric(),
    rope_decision = character(),
    log_bf10 = numeric(),
    kl_prior_post = numeric(),
    post_narrow = numeric(),
    term = character(),
    stringsAsFactors = FALSE
  )
}

#' Bind estimate tables with a shared column schema
#'
#' Older CFA / lavaan builders sometimes omit CI / Bayes columns. Align both
#' sides to [empty_estimates()] before `rbind` so two-step latent fits stay safe.
#' @keywords internal
bind_estimates <- function(...) {
  tabs <- list(...)
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]
  if (!length(tabs)) return(empty_estimates())
  proto <- empty_estimates()
  na_like <- function(template, n) {
    if (is.logical(template)) return(rep(NA, n))
    if (is.character(template)) return(rep(NA_character_, n))
    if (is.integer(template)) return(rep(NA_integer_, n))
    rep(NA_real_, n)
  }
  aligned <- lapply(tabs, function(x) {
    if (!is.data.frame(x) || !nrow(x)) return(proto[0, , drop = FALSE])
    for (nm in names(proto)) {
      if (!nm %in% names(x)) x[[nm]] <- na_like(proto[[nm]], nrow(x))
    }
    x[, names(proto), drop = FALSE]
  })
  nonempty <- aligned[vapply(aligned, nrow, integer(1)) > 0L]
  if (!length(nonempty)) return(proto[0, , drop = FALSE])
  do.call(rbind, nonempty)
}

#' Append one estimate row
#' @keywords internal
estimate_row <- function(lhs, op, rhs, est, se, pvalue,
                         est_std = NA_real_,
                         df = NA_real_, level = NA_character_, kind = "effect",
                         ci_low = NA_real_, ci_high = NA_real_,
                         ci_level = NA_real_, includes_zero = NA,
                         pd = NA_real_, rope_low = NA_real_, rope_high = NA_real_,
                         rope_pct = NA_real_, rope_decision = NA_character_,
                         log_bf10 = NA_real_,
                         kl_prior_post = NA_real_,
                         post_narrow = NA_real_,
                         term = NA_character_) {
  ci_low <- as.numeric(ci_low)
  ci_high <- as.numeric(ci_high)
  includes_zero <- as.logical(includes_zero)
  rope_decision <- as.character(rope_decision %||% NA_character_)
  if (length(includes_zero) != 1L || is.na(includes_zero)) {
    if (!is.na(rope_decision) && nzchar(rope_decision)) {
      includes_zero <- !identical(rope_decision, "reject")
    } else {
      includes_zero <- if (is.finite(ci_low) && is.finite(ci_high)) {
        ci_low <= 0 && ci_high >= 0
      } else {
        NA
      }
    }
  }
  data.frame(
    lhs = lhs,
    op = op,
    rhs = rhs,
    est = as.numeric(est),
    se = as.numeric(se),
    pvalue = as.numeric(pvalue),
    est_std = as.numeric(est_std),
    df = as.numeric(df),
    level = as.character(level),
    kind = as.character(kind),
    ci_low = ci_low,
    ci_high = ci_high,
    ci_level = as.numeric(ci_level),
    includes_zero = includes_zero,
    pd = as.numeric(pd),
    rope_low = as.numeric(rope_low),
    rope_high = as.numeric(rope_high),
    rope_pct = as.numeric(rope_pct),
    rope_decision = rope_decision,
    log_bf10 = as.numeric(log_bf10),
    kl_prior_post = as.numeric(kl_prior_post),
    post_narrow = as.numeric(post_narrow),
    term = as.character(term %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

#' Fixed-effect coefficients (works for lm/glm and merMod)
#' @keywords internal
path_fixed_coefs <- function(fit) {
  if (inherits(fit, "brmsfit")) {
    fx <- brms::fixef(fit)
    est <- as.numeric(fx[, "Estimate"])
    nms <- rownames(fx)
    resp <- as.character(fit$smt_response %||% "")
    if (!is.na(resp) && nzchar(resp)) {
      pref <- paste0(resp, "_")
      keep <- startsWith(nms, pref)
      if (!any(keep) && any(nms == "Intercept" | !grepl("_", nms))) {
        # Univariate naming even on a tagged fit
        keep <- rep(TRUE, length(nms))
      } else {
        nms <- substring(nms[keep], nchar(pref) + 1L)
        est <- est[keep]
      }
    }
    nms[nms == "Intercept"] <- "(Intercept)"
    # Restore SMT dummy names (Var__Level) after brms-safe rename
    nms <- vapply(
      nms, function(nm) brms_translate_term(fit, nm, "to_orig"),
      character(1), USE.NAMES = FALSE
    )
    names(est) <- nms
    return(est)
  }
  if (inherits(fit, "merMod")) {
    return(lme4::fixef(fit))
  }
  stats::coef(fit)
}

#' Covariance of fixed effects, with brms intercept renamed to `(Intercept)`
#' @keywords internal
path_vcov <- function(fit) {
  V <- tryCatch(as.matrix(stats::vcov(fit)), error = function(e) NULL)
  if (is.null(V) || !nrow(V)) return(NULL)
  if (inherits(fit, "brmsfit")) {
    nms <- colnames(V)
    resp <- as.character(fit$smt_response %||% "")
    if (!is.na(resp) && nzchar(resp)) {
      pref <- paste0(resp, "_")
      keep <- startsWith(nms, pref)
      if (any(keep)) {
        V <- V[keep, keep, drop = FALSE]
        nms <- substring(colnames(V), nchar(pref) + 1L)
      }
    }
    nms[nms == "Intercept"] <- "(Intercept)"
    nms <- vapply(
      nms, function(nm) brms_translate_term(fit, nm, "to_orig"),
      character(1), USE.NAMES = FALSE
    )
    colnames(V) <- nms
    rownames(V) <- nms
  }
  V
}

#' Coefficient table for path engines (Estimate / SE / p)
#' @return matrix/data.frame with rownames = term names
#' @keywords internal
path_coef_table <- function(fit) {
  if (inherits(fit, "brmsfit")) {
    return(brms_coef_table(
      fit,
      ci = fit$smt_ci %||% 0.95,
      rope_k = fit$smt_rope_k %||% 0.1,
      term_priors = fit$smt_term_priors
    ))
  }
  ct <- tryCatch(stats::coef(summary(fit)), error = function(e) NULL)
  if (is.null(ct)) return(NULL)
  as.data.frame(ct)
}

#' P-value column from a coef summary table
#' @keywords internal
path_coef_pvalue <- function(ct, term) {
  if (is.null(ct) || !term %in% rownames(ct)) return(NA_real_)
  pcols <- intersect(
    c("Pr(>|t|)", "Pr(>|z|)", "Pr(>|Chisq|)"),
    colnames(ct)
  )
  if (length(pcols)) return(as.numeric(ct[term, pcols[[1]]]))
  # Fallback: normal approx from |Estimate|/SE
  if (ncol(ct) >= 2) {
    est <- as.numeric(ct[term, 1L])
    se <- as.numeric(ct[term, 2L])
    if (is.finite(est) && is.finite(se) && se > 0) {
      return(2 * stats::pnorm(-abs(est / se)))
    }
  }
  NA_real_
}

#' HDI bounds stored on a brms coefficient table
#' @keywords internal
path_coef_bounds <- function(ct, term) {
  if (is.null(ct) || !term %in% rownames(ct)) return(c(NA_real_, NA_real_))
  if (!all(c("ci_low", "ci_high") %in% names(ct))) return(c(NA_real_, NA_real_))
  c(as.numeric(ct[term, "ci_low"]), as.numeric(ct[term, "ci_high"]))
}

#' pd / ROPE fields stored on a brms coefficient table
#' @keywords internal
path_coef_bayes_fields <- function(ct, term) {
  empty <- list(
    pd = NA_real_,
    rope_low = NA_real_,
    rope_high = NA_real_,
    rope_pct = NA_real_,
    rope_decision = NA_character_,
    log_bf10 = NA_real_,
    kl_prior_post = NA_real_,
    post_narrow = NA_real_,
    includes_zero = NA
  )
  if (is.null(ct) || !term %in% rownames(ct)) return(empty)
  pull <- function(nm) {
    if (!nm %in% names(ct)) return(NA)
    ct[term, nm][[1]]
  }
  list(
    pd = as.numeric(pull("pd")),
    rope_low = as.numeric(pull("rope_low")),
    rope_high = as.numeric(pull("rope_high")),
    rope_pct = as.numeric(pull("rope_pct")),
    rope_decision = {
      rd <- pull("rope_decision")
      if (length(rd) != 1L || is.null(rd) || is.na(rd)) NA_character_ else as.character(rd)
    },
    log_bf10 = as.numeric(pull("log_bf10")),
    kl_prior_post = as.numeric(pull("kl_prior_post")),
    post_narrow = as.numeric(pull("post_narrow")),
    includes_zero = as.logical(pull("includes_zero"))
  )
}

#' Posterior mean, SD, HDI, pd, and ROPE for brms population-level coefficients
#'
#' For joint multivariate fits tagged with `smt_response`, only that response's
#' coefficients are returned, with response prefixes stripped so term names match
#' the univariate path helpers.
#' @keywords internal
brms_coef_table <- function(fit, ci = 0.95, rope_k = 0.1, term_priors = NULL) {
  ci <- as.numeric(ci %||% 0.95)
  if (!is.finite(ci) || ci <= 0 || ci >= 1) ci <- 0.95
  rope_k <- as.numeric(rope_k %||% 0.1)
  if (!is.finite(rope_k) || rope_k <= 0) rope_k <- 0.1
  cf <- path_fixed_coefs(fit)
  nms <- names(cf)
  if (!length(nms)) {
    return(data.frame(
      Estimate = numeric(), Est.Error = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  term_priors <- term_priors %||% list()
  fx <- data.frame(
    Estimate = as.numeric(cf),
    Est.Error = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    pd = NA_real_,
    rope_low = NA_real_,
    rope_high = NA_real_,
    rope_pct = NA_real_,
    rope_decision = NA_character_,
    log_bf10 = NA_real_,
    kl_prior_post = NA_real_,
    post_narrow = NA_real_,
    includes_zero = NA,
    row.names = nms,
    stringsAsFactors = FALSE
  )
  for (nm in nms) {
    draws <- brms_term_draws(fit, nm)
    prior_mean <- if (!is.null(term_priors[[nm]]$mean)) {
      as.numeric(term_priors[[nm]]$mean)
    } else {
      0
    }
    prior_sd <- if (!is.null(term_priors[[nm]]$sd)) {
      as.numeric(term_priors[[nm]]$sd)
    } else {
      1
    }
    rope <- rope_bounds_from_prior(prior_sd, rope_k)
    sm <- bayes_term_summary(
      draws, rope = rope, ci = ci,
      prior_mean = prior_mean, prior_sd = prior_sd
    )
    fx[nm, "Estimate"] <- sm$est
    fx[nm, "Est.Error"] <- sm$se
    fx[nm, "ci_low"] <- sm$hdi_low
    fx[nm, "ci_high"] <- sm$hdi_high
    fx[nm, "pd"] <- sm$pd
    fx[nm, "rope_low"] <- sm$rope_low
    fx[nm, "rope_high"] <- sm$rope_high
    fx[nm, "rope_pct"] <- sm$rope_pct
    fx[nm, "rope_decision"] <- sm$rope_decision
    fx[nm, "log_bf10"] <- sm$log_bf10
    fx[nm, "kl_prior_post"] <- sm$kl_prior_post
    fx[nm, "post_narrow"] <- sm$post_narrow
    fx[nm, "includes_zero"] <- sm$includes_null
  }
  fx
}

#' Design matrix used to compute predictor SDs for standardization
#' @keywords internal
path_design_matrix <- function(fit) {
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (!is.null(mm)) return(mm)
  if (inherits(fit, "brmsfit") && is.data.frame(fit$data)) {
    fml <- path_fixed_formula(fit)
    if (!is.null(fml)) {
      mm <- tryCatch(
        stats::model.matrix(fml, data = fit$data),
        error = function(e) NULL
      )
    }
  }
  mm
}

#' Resolve a coef name to a column in the design matrix (brms-safe aliases)
#' @keywords internal
path_design_col_for_term <- function(fit, term, mm_names) {
  term <- as.character(term %||% "")[[1]]
  if (!nzchar(term) || identical(term, "(Intercept)")) return(NA_character_)
  if (term %in% mm_names) return(term)
  safe <- tryCatch(
    brms_translate_term(fit, term, "to_safe"),
    error = function(e) term
  )
  if (nzchar(safe) && safe %in% mm_names) return(safe)
  # Fallback: collapse SMT __ dummies the same way brms_safe_name_one does
  collapsed <- gsub("__+", "_", term, perl = TRUE)
  collapsed <- sub("_+$", "", collapsed)
  if (nzchar(collapsed) && collapsed %in% mm_names) return(collapsed)
  NA_character_
}

#' Standardized coefficients for lm / glm / merMod / brms terms (excl. intercept)
#'
#' Continuous / dummy predictors: `b * sd(x) / sd(y)` on the design matrix.
#' glm / glmer: `b * sd(x) / sqrt(var(η) + π²/3)` (latent-y style).
#' Mixed models use the same observed-y (or latent-η) scaling as the fixed part.
#' @keywords internal
standardized_term_coefs <- function(fit) {
  cf <- path_fixed_coefs(fit)
  out <- setNames(rep(NA_real_, length(cf)), names(cf))
  mm <- path_design_matrix(fit)
  if (is.null(mm) || !ncol(mm)) return(out)
  mm_names <- colnames(mm)

  if (inherits(fit, "glmerMod")) {
    eta <- tryCatch(
      stats::predict(fit, type = "link", re.form = NULL),
      error = function(e) NULL
    )
    sy <- if (!is.null(eta) && length(eta)) {
      sqrt(stats::var(as.numeric(eta), na.rm = TRUE) + (pi^2) / 3)
    } else {
      NA_real_
    }
  } else if (inherits(fit, "glm")) {
    eta <- fit$linear.predictors
    sy <- sqrt(stats::var(eta) + (pi^2) / 3)
  } else if (inherits(fit, "brmsfit")) {
    y <- brms_observed_y(fit)
    sy <- if (length(y)) stats::sd(as.numeric(y), na.rm = TRUE) else NA_real_
    # Bernoulli / binomial: latent-scale standardization when link is logit/probit
    fam <- tryCatch(brms_view_family(fit), error = function(e) NULL)
    fam_nm <- as.character(fam$family %||% "")[[1]]
    if (identical(fam_nm, "bernoulli") || identical(fam_nm, "binomial")) {
      pe <- tryCatch(
        as.numeric(colMeans(brms::posterior_linpred(fit), na.rm = TRUE)),
        error = function(e) NULL
      )
      if (!is.null(pe) && length(pe)) {
        sy <- sqrt(stats::var(pe, na.rm = TRUE) + (pi^2) / 3)
      }
    }
  } else {
    y <- tryCatch(
      stats::model.response(stats::model.frame(fit)),
      error = function(e) NULL
    )
    sy <- if (is.null(y)) NA_real_ else stats::sd(as.numeric(y), na.rm = TRUE)
  }
  if (!is.finite(sy) || sy <= 0) sy <- 1

  for (nm in names(cf)) {
    if (identical(nm, "(Intercept)")) next
    col <- path_design_col_for_term(fit, nm, mm_names)
    if (is.na(col)) next
    sx <- stats::sd(mm[, col], na.rm = TRUE)
    if (!is.finite(sx) || sx <= 0) next
    out[[nm]] <- unname(cf[[nm]]) * sx / sy
  }
  out
}

#' Omnibus term test (drop1) for a predictor coded as a factor term
#' @keywords internal
term_omnibus <- function(fit, xn, engine) {
  test <- if (identical(engine, "glm")) "Chisq" else "F"
  d1 <- tryCatch(
    stats::drop1(fit, test = test),
    error = function(e) NULL
  )
  if (is.null(d1) || !xn %in% rownames(d1)) {
    return(list(df = NA_real_, pvalue = NA_real_))
  }
  row <- d1[xn, , drop = FALSE]
  df <- if ("Df" %in% names(row)) as.numeric(row$Df[1]) else NA_real_
  pcol <- intersect(c("Pr(>Chi)", "Pr(>F)"), names(row))
  pv <- if (length(pcol)) as.numeric(row[[pcol[1]]][1]) else NA_real_
  list(df = df, pvalue = pv)
}

#' Joint Wald χ² test for a block of coefficients (omnibus over dummies)
#' @keywords internal
joint_wald_test <- function(fit, term_names) {
  empty <- list(df = NA_real_, pvalue = NA_real_, statistic = NA_real_)
  if (!length(term_names)) return(empty)
  cf <- path_fixed_coefs(fit)
  V <- path_vcov(fit)
  if (is.null(V)) return(empty)
  idx <- match(term_names, names(cf))
  keep <- !is.na(idx) & is.finite(cf[idx])
  idx <- idx[keep]
  if (!length(idx)) return(empty)
  b <- cf[idx]
  Vb <- V[idx, idx, drop = FALSE]
  W <- tryCatch(as.numeric(t(b) %*% solve(Vb, b)), error = function(e) NA_real_)
  df <- as.numeric(length(idx))
  pv <- if (is.finite(W)) stats::pchisq(W, df = df, lower.tail = FALSE) else NA_real_
  list(df = df, pvalue = pv, statistic = W)
}

#' Resolve cat_mode for a predictor in an edge row (from the from-node)
#' @keywords internal
edge_cat_mode <- function(preds, i, state) {
  from_id <- preds$from[i]
  node_cat_mode(state, from_id)
}

#' Expand one interaction factor into numeric series (dummies or continuous)
#'
#' @return list of lists with `tag`, `values`, `level` (NA for continuous)
#' @keywords internal
interaction_factor_series <- function(state, dat, node_id, level = NA_character_) {
  col <- lavaan_var_name(state, node_id)
  if (!col %in% names(dat)) {
    stop("Predictor '", col, "' not found in analysis data.", call. = FALSE)
  }
  x <- dat[[col]]
  kind <- classify_variable(x)
  coding <- variable_coding_for_node(state, node_id, dat)
  # Declared categorical (inspector) can promote binary→categorical coding.
  if (identical(coding$kind, "categorical")) {
    kind <- "categorical"
  } else if (!is.na(level) && nzchar(as.character(level)) && identical(kind, "binary")) {
    # Level-wired binary: same split indicator as a 2-level categorical factor
    levs <- coding$all_levels
    if (!length(levs)) levs <- data_levels_chr(x)
    if (length(levs) >= 2L) {
      base <- if (!is.na(coding$base) && nzchar(coding$base) && coding$base %in% levs) {
        coding$base
      } else {
        levs[[1]]
      }
      levs <- c(base, setdiff(levs, base))
      coding <- list(
        kind = "categorical",
        base = levs[[1]],
        levels = levs[-1],
        all_levels = levs,
        ordered = isTRUE(coding$ordered)
      )
      kind <- "categorical"
    }
  }
  cm <- node_cat_mode(state, node_id)

  if (identical(kind, "categorical")) {
    if (!length(coding$levels)) {
      stop("Categorical predictor '", col, "' has no non-base levels.", call. = FALSE)
    }
    if (identical(cm, "omnibus")) {
      if (!is.na(level) && nzchar(level)) {
        stop(
          "Omnibus factor '", col, "' must be wired into × without a level port.",
          call. = FALSE
        )
      }
      out <- lapply(coding$levels, function(lv) {
        list(
          tag = sanitize_id(lv),
          level = as.character(lv),
          values = as.numeric(as.character(x) == as.character(lv))
        )
      })
      return(out)
    }
    # split: exactly one category
    if (is.na(level) || !nzchar(level)) {
      stop(
        "Split categorical '", col, "' must wire one level into the × node ",
        "(use a level port, not the whole factor).",
        call. = FALSE
      )
    }
    if (identical(level, coding$base)) {
      stop(
        "Level '", level, "' is the reference base for '", col, "'. ",
        "Wire a non-base level into ×.",
        call. = FALSE
      )
    }
    if (!level %in% coding$levels) {
      stop("Unknown level '", level, "' for '", col, "'.", call. = FALSE)
    }
    return(list(list(
      tag = sanitize_id(level),
      level = as.character(level),
      values = as.numeric(as.character(x) == as.character(level))
    )))
  }

  if (!is.na(level) && nzchar(level)) {
    stop(
      "Level '", level, "' was wired into × but '", col, "' is not categorical.",
      call. = FALSE
    )
  }
  list(list(
    tag = "num",
    level = NA_character_,
    values = factor_numeric_for_product(x, center = TRUE)
  ))
}

#' Expand an interaction node into product term columns
#'
#' Omnibus categorical sides produce one product per non-base level (jointly
#' tested). Split categorical sides produce a single product for that level.
#' @return list(data, term_names, term_meta, omnibus_terms)
#' @keywords internal
expand_interaction_terms <- function(state, dat, ix_id, local_dat) {
  fac <- interaction_factors(state, ix_id)
  if (is.na(fac$a) || is.na(fac$b) || !nzchar(fac$a) || !nzchar(fac$b)) {
    stop(
      "Interaction '", node_display_name(state, ix_id),
      "' needs two predictors wired into its input ports.",
      call. = FALSE
    )
  }
  series_a <- interaction_factor_series(state, dat, fac$a, fac$level_a)
  series_b <- interaction_factor_series(state, dat, fac$b, fac$level_b)
  ix_nm <- lavaan_var_name(state, ix_id)
  a_coding <- variable_coding_for_node(state, fac$a, dat)
  b_coding <- variable_coding_for_node(state, fac$b, dat)
  a_omni <- identical(node_cat_mode(state, fac$a), "omnibus") &&
    identical(a_coding$kind, "categorical")
  b_omni <- identical(node_cat_mode(state, fac$b), "omnibus") &&
    identical(b_coding$kind, "categorical")

  term_names <- character()
  term_meta <- list()
  for (sa in series_a) {
    for (sb in series_b) {
      nm <- if (length(series_a) == 1L && length(series_b) == 1L &&
                 is.na(sa$level) && is.na(sb$level)) {
        ix_nm
      } else {
        paste0(sanitize_id(ix_nm), "__", sa$tag, "_x_", sb$tag)
      }
      local_dat[[nm]] <- as.numeric(sa$values) * as.numeric(sb$values)
      # Level tag for table / omnibus contrast rows
      lv <- if (!is.na(sa$level) && !is.na(sb$level)) {
        paste0(sa$level, "\u00D7", sb$level)
      } else if (!is.na(sa$level)) {
        as.character(sa$level)
      } else if (!is.na(sb$level)) {
        as.character(sb$level)
      } else {
        NA_character_
      }
      term_names <- c(term_names, nm)
      term_meta[[nm]] <- list(rhs = ix_nm, level = lv, kind = "effect")
    }
  }
  omnibus_terms <- if ((a_omni || b_omni) && length(term_names) > 0L) {
    unique(term_names)
  } else {
    character()
  }
  list(
    data = local_dat,
    term_names = unique(term_names),
    term_meta = term_meta,
    omnibus_terms = omnibus_terms
  )
}

#' Coefficient rows belonging to a factor predictor
#' @keywords internal
contrast_coef_rows <- function(ct, xn) {
  if (is.null(ct) || !nrow(ct)) return(ct[FALSE, , drop = FALSE])
  rn <- rownames(ct)
  plain <- gsub("`", "", rn, fixed = TRUE)
  hits <- which(plain != xn & startsWith(plain, xn))
  if (!length(hits)) return(ct[FALSE, , drop = FALSE])
  ct[hits, , drop = FALSE]
}

#' Engine label for a DV node given current model + data
#'
#' @return NULL if not a DV; otherwise "lm", "glm", "lmer", "glmer",
#'   "lavaan", or "unsupported"
#' @keywords internal
dv_model_engine <- function(state, data, node_id, edges = state$edges) {
  dvs <- structural_dv_ids(state, edges)
  if (!node_id %in% dvs) return(NULL)

  rm <- node_repeated_measures(state, node_id)
  use_rm <- isTRUE(rm$enabled) && !is.na(rm$cluster) && nzchar(rm$cluster)

  # True measurement latents stay continuous in the path stage
  if (is_latent_node(state, node_id)) {
    return(if (use_rm) "lmer" else "lm")
  }

  col <- state$nodes$observed_column[match(node_id, state$nodes$id)]
  mapped <- !is.na(col) && nzchar(as.character(col))
  kind <- NULL
  if (mapped && !is.null(data) && col %in% names(data)) {
    kind <- classify_variable(data[[col]])
  } else if (!mapped) {
    # Unmapped: declared sim kind only — do not classify a coincidental
    # same-named column from the open data file.
    kind <- node_sim_var_kind(state, node_id, data = NULL)
  } else if (!is.null(data)) {
    # Mapped but column missing from this data frame (e.g. mid-sim restore):
    # try lavaan name, else declared kind.
    nm <- tryCatch(lavaan_var_name(state, node_id), error = function(e) NA_character_)
    if (!is.na(nm) && nzchar(nm) && nm %in% names(data)) {
      kind <- classify_variable(data[[nm]])
    }
    if (is.null(kind) || !kind %in% c("continuous", "categorical", "binary")) {
      kind <- node_sim_var_kind(state, node_id, data = NULL)
    }
  }
  if (is.null(kind) || !kind %in% c("continuous", "categorical", "binary")) {
    kind <- node_sim_var_kind(state, node_id, data = NULL)
  }
  if (identical(kind, "categorical")) {
    cm <- node_cat_mode(state, node_id)
    if (!identical(cm, "split")) return("unsupported")
    # Split categorical DV → one-vs-rest glm/glmer
    return(if (use_rm) "glmer" else "glm")
  }
  if (identical(kind, "binary")) {
    return(if (use_rm) "glmer" else "glm")
  }
  if (use_rm) "lmer" else "lm"
}

#' Display label for canvas badge
#' @keywords internal
dv_model_badge <- function(engine) {
  switch(engine,
    lm = "lm",
    glm = "glm · binomial",
    lmer = "lmer · (1|id)",
    glmer = "glmer · binomial · (1|id)",
    lavaan = "lavaan",
    unsupported = "categorical DV: use split",
    engine
  )
}

#' Map of node_id -> engine for all current DVs
#' @keywords internal
dv_engines_map <- function(state, data, edges = state$edges) {
  ids <- structural_dv_ids(state, edges)
  if (!length(ids)) return(list())
  out <- lapply(ids, function(id) dv_model_engine(state, data, id, edges))
  names(out) <- ids
  out
}

#' Map latent nodes onto score columns for the path stage
#' @keywords internal
state_with_latent_as_scores <- function(state, latent_ids) {
  state <- ensure_node_kind_columns(state)
  for (lat in latent_ids) {
    idx <- match(lat, state$nodes$id)
    if (is.na(idx)) next
    nm <- lavaan_var_name(state, lat)
    state$nodes$observed_column[idx] <- nm
    state$nodes$node_kind[idx] <- "observed"
  }
  state
}

#' Regression factor-score weights from a (typically single-factor) CFA fit
#'
#' Uses the Thomson/regression formula `W = Ψ Λ' Σ⁻¹` with `Σ = ΛΨΛ' + Θ`,
#' matching `lavPredict(..., method = "regression")`.
#'
#' @return Named list of numeric weight vectors (one per latent), or NULL.
#' @keywords internal
cfa_regression_score_weights <- function(fit) {
  if (!inherits(fit, "lavaan")) return(NULL)
  est <- tryCatch(lavaan::lavInspect(fit, "est"), error = function(e) NULL)
  if (is.null(est)) return(NULL)
  lam <- est$lambda
  th <- est$theta
  psi <- est$psi
  if (is.null(lam) || is.null(th) || is.null(psi)) return(NULL)
  lam <- as.matrix(lam)
  th <- as.matrix(th)
  psi <- as.matrix(psi)
  if (!nrow(lam) || !ncol(lam)) return(NULL)
  sig <- tryCatch(
    lam %*% psi %*% t(lam) + th,
    error = function(e) NULL
  )
  if (is.null(sig)) return(NULL)
  w_mat <- tryCatch(
    psi %*% t(lam) %*% solve(sig),
    error = function(e) NULL
  )
  if (is.null(w_mat)) return(NULL)
  w_mat <- as.matrix(w_mat)
  out <- list()
  for (i in seq_len(nrow(w_mat))) {
    lat <- rownames(w_mat)[[i]]
    if (is.null(lat) || !nzchar(lat)) lat <- colnames(lam)[[i]]
    ww <- as.numeric(w_mat[i, ])
    names(ww) <- colnames(w_mat)
    out[[lat]] <- ww
  }
  out
}

#' Score one latent from its own measurement indicators only
#'
#' Fits a single-factor CFA on the indicators linked to `lat` and returns
#' regression factor scores. Other constructs' items are never used — matching
#' the graph (and the intuition that separate questionnaires stay separate).
#' @keywords internal
score_latent_own_indicators <- function(state, dat, lat, meas_edges) {
  lat <- as.character(lat)[[1]]
  rows <- meas_edges[meas_edges$from == lat, , drop = FALSE]
  if (!nrow(rows)) {
    stop(
      "Latent '", node_display_name(state, lat),
      "' has no measurement links.",
      call. = FALSE
    )
  }
  syn <- build_measurement_syntax(state, rows)
  fit <- tryCatch(
    lavaan::cfa(syn, data = dat, meanstructure = FALSE, warn = FALSE),
    error = function(e) {
      stop(
        "Factor-score CFA error for '",
        node_display_name(state, lat), "': ", e$message,
        call. = FALSE
      )
    }
  )
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
    stop(
      "Factor-score CFA did not converge for '",
      node_display_name(state, lat), "'.",
      call. = FALSE
    )
  }
  sc <- tryCatch(
    lavaan::lavPredict(fit, type = "lv", method = "regression"),
    error = function(e) {
      stop(
        "Could not compute factor scores for '",
        node_display_name(state, lat), "': ", e$message,
        call. = FALSE
      )
    }
  )
  sc <- as.data.frame(sc, stringsAsFactors = FALSE)
  if (!nrow(sc) || !ncol(sc)) {
    stop(
      "Factor-score prediction returned no scores for '",
      node_display_name(state, lat), "'.",
      call. = FALSE
    )
  }
  lat_nm <- lavaan_var_name(state, lat)
  # lavPredict column may be the latent name
  score_col <- if (lat_nm %in% names(sc)) {
    lat_nm
  } else {
    names(sc)[[1]]
  }
  used <- tryCatch(
    lavaan::lavInspect(fit, "case.idx"),
    error = function(e) NULL
  )
  if (is.list(used)) used <- used[[1]]
  w <- cfa_regression_score_weights(fit)
  ww <- if (!is.null(w) && length(w)) w[[1]] else NULL
  list(
    fit = fit,
    scores = as.numeric(sc[[score_col]]),
    used = used,
    weights = ww,
    latent_name = lat_nm
  )
}

#' Stage 1: build factor scores from measurement links (graph unchanged)
#'
#' Joint CFA always supplies confirmatory loadings for the report.
#' Path-stage scores depend on `score_method`:
#' - `"own"` (default): each latent from its own indicators only
#' - `"joint"`: Thomson/regression scores from the joint CFA (may use other
#'   constructs' items when factors correlate; only meaningful with ≥2 latents)
#'
#' Returns analysis data with score columns, a path-ready state (latents mapped
#' to those columns), CFA fit/estimates/syntax, and latent ids.
#' @keywords internal
attach_latent_factor_scores <- function(state, dat, score_method = "own") {
  validate_model_state(state, state$edges)
  meas <- state$edges[state$edges$type == "measurement", , drop = FALSE]
  if (!nrow(meas)) {
    stop(
      "Latent nodes need measurement links to observed indicators before fitting.",
      call. = FALSE
    )
  }
  latent_ids <- unique(as.character(meas$from))
  score_method <- normalize_latent_score_method(score_method, state)
  syntax <- build_measurement_syntax(state, meas)

  # Ensure indicator columns exist
  for (i in seq_len(nrow(meas))) {
    ind <- lavaan_var_name(state, meas$to[i])
    if (!ind %in% names(dat)) {
      stop(
        "Indicator '", ind, "' for latent '",
        node_display_name(state, meas$from[i]),
        "' is missing from the analysis data.",
        call. = FALSE
      )
    }
  }

  # Joint CFA: loadings for canvas / Model-fit report (structure as drawn)
  fit <- tryCatch(
    lavaan::cfa(syntax, data = dat, meanstructure = FALSE, warn = FALSE),
    error = function(e) stop("Factor-score CFA error: ", e$message, call. = FALSE)
  )
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
    stop("Factor-score CFA did not converge.", call. = FALSE)
  }

  score_weights <- list()
  score_fits <- list()

  if (identical(score_method, "joint")) {
    sc <- tryCatch(
      lavaan::lavPredict(fit, type = "lv", method = "regression"),
      error = function(e) stop("Could not compute factor scores: ", e$message, call. = FALSE)
    )
    sc <- as.data.frame(sc, stringsAsFactors = FALSE)
    if (!nrow(sc) || !ncol(sc)) {
      stop("Factor-score prediction returned no scores.", call. = FALSE)
    }
    used <- tryCatch(
      lavaan::lavInspect(fit, "case.idx"),
      error = function(e) NULL
    )
    if (is.list(used)) used <- used[[1]]
    w_all <- cfa_regression_score_weights(fit)
    for (lat in latent_ids) {
      lat_nm <- lavaan_var_name(state, lat)
      score_fits[[lat_nm]] <- fit
      if (!is.null(w_all) && lat_nm %in% names(w_all)) {
        score_weights[[lat_nm]] <- w_all[[lat_nm]]
      } else if (!is.null(w_all) && length(w_all) == length(latent_ids)) {
        score_weights[[lat_nm]] <- w_all[[match(lat, latent_ids)]]
      }
      col <- if (lat_nm %in% names(sc)) lat_nm else {
        # fall back to column order matching latent_ids
        idx <- match(lat, latent_ids)
        if (is.finite(idx) && idx <= ncol(sc)) names(sc)[[idx]] else names(sc)[[1]]
      }
      dat[[lat_nm]] <- NA_real_
      vals <- as.numeric(sc[[col]])
      if (!is.null(used) && length(used) == length(vals)) {
        dat[[lat_nm]][used] <- vals
      } else if (length(vals) == nrow(dat)) {
        dat[[lat_nm]] <- vals
      } else {
        stop(
          "Factor-score rows for '", node_display_name(state, lat),
          "' (", length(vals), ") do not match analysis data (", nrow(dat), ").",
          call. = FALSE
        )
      }
    }
  } else {
    # Own-indicator scores (never borrow items from other latents)
    for (lat in latent_ids) {
      one <- score_latent_own_indicators(state, dat, lat, meas)
      lat_nm <- one$latent_name
      score_fits[[lat_nm]] <- one$fit
      if (!is.null(one$weights)) score_weights[[lat_nm]] <- one$weights
      dat[[lat_nm]] <- NA_real_
      if (!is.null(one$used) && length(one$used) == length(one$scores)) {
        dat[[lat_nm]][one$used] <- one$scores
      } else if (length(one$scores) == nrow(dat)) {
        dat[[lat_nm]] <- one$scores
      } else {
        stop(
          "Factor-score rows for '", node_display_name(state, lat),
          "' (", length(one$scores), ") do not match analysis data (", nrow(dat), ").",
          call. = FALSE
        )
      }
    }
  }

  # Reliability / unidimensionality on raw indicators (before score row filter)
  cronbach <- cronbach_alpha_for_latents(state, dat, latent_ids, meas = meas)
  uni <- unidimensionality_for_latents(state, dat, latent_ids, meas = meas)

  score_cols <- vapply(
    latent_ids,
    function(id) lavaan_var_name(state, id),
    character(1)
  )
  dat <- dat[stats::complete.cases(dat[, score_cols, drop = FALSE]), , drop = FALSE]
  if (!nrow(dat)) {
    stop("No complete cases after attaching factor scores.", call. = FALSE)
  }

  pe <- as.data.frame(lavaan::parameterEstimates(fit, standardized = TRUE))
  meas_est <- empty_estimates()
  if (nrow(pe)) {
    pe <- pe[pe$op == "=~", , drop = FALSE]
    if (nrow(pe)) {
      std_col <- if ("std.all" %in% names(pe)) {
        "std.all"
      } else if ("std.lv" %in% names(pe)) {
        "std.lv"
      } else {
        NA_character_
      }
      # Use estimate_row so CFA loadings share the path-estimate schema
      # (CI / Bayes columns); raw data.frame rbind breaks otherwise.
      rows <- lapply(seq_len(nrow(pe)), function(i) {
        estimate_row(
          lhs = as.character(pe$lhs[[i]]),
          op = as.character(pe$op[[i]]),
          rhs = as.character(pe$rhs[[i]]),
          est = as.numeric(pe$est[[i]]),
          se = as.numeric(pe$se[[i]]),
          pvalue = as.numeric(pe$pvalue[[i]]),
          est_std = if (is.na(std_col)) NA_real_ else as.numeric(pe[[std_col]][[i]]),
          df = NA_real_,
          level = NA_character_,
          kind = "effect"
        )
      })
      meas_est <- do.call(rbind, rows)
    }
  }

  state_path <- state_with_latent_as_scores(state, latent_ids)
  list(
    data = dat,
    state = state_path,
    latent_ids = latent_ids,
    measurement_fit = fit,
    measurement_estimates = meas_est,
    measurement_syntax = syntax,
    measurement_score_weights = score_weights,
    measurement_score_fits = score_fits,
    measurement_cronbach_alpha = cronbach,
    measurement_unidimensionality = uni,
    latent_score_method = score_method
  )
}

#' Fit the structural model with a context-aware engine
#'
#' Observed-only path models use equation-wise lm / glm (binomial) based on
#' each dependent variable's type. Multi-level categorical predictors are kept
#' as factors (k−1 contrasts + omnibus test).
#'
#' When the graph includes latents, a **two-step** procedure runs: (1) CFA /
#' factor scores from measurement links, (2) the usual path engine on structural
#' links with latents replaced by those scores (AIC/BIC, split logistic, etc.).
#'
#' @keywords internal
fit_structural_model <- function(state, data, include_edge_ids = NULL,
                                 latent_score_method = "own",
                                 backend = c("freq", "brms"),
                                 brms_args = NULL) {
  backend <- match.arg(backend)
  if (is.null(include_edge_ids)) include_edge_ids <- state$edges$id
  edges <- state$edges[state$edges$id %in% include_edge_ids, , drop = FALSE]
  # DVs from the full graph so null / reduced candidates still get intercept-only equations
  dv_ids <- structural_dv_ids(state, state$edges)
  dat <- prepare_analysis_data(state, data)
  latent_score_method <- normalize_latent_score_method(latent_score_method, state)

  # Two-step whenever the *full* graph has latents (scores from all measurement links)
  if (model_uses_latents(state, state$edges)) {
    attached <- attach_latent_factor_scores(
      state, dat, score_method = latent_score_method
    )
    # Rematerialize interactions / polys with latents treated as observed scores
    dat_path <- prepare_analysis_data(attached$state, attached$data)
    struct_edges <- edges[edges$type != "measurement", , drop = FALSE]
    if (!length(dv_ids) && !nrow(struct_edges)) {
      stop("No structural dependent variables to fit.", call. = FALSE)
    }
    path_res <- fit_path_equations(
      attached$state, dat_path, struct_edges, dv_ids = dv_ids,
      backend = backend, brms_args = brms_args
    )
    # Keep measurement loadings on the canvas; structural coeffs from path stage
    est <- path_res$estimates
    if (nrow(attached$measurement_estimates)) {
      est <- bind_estimates(attached$measurement_estimates, est)
    }
    path_res$estimates <- est
    score_note <- if (identical(attached$latent_score_method, "joint")) {
      "# scores: joint Thomson/regression (multi-factor)\n"
    } else {
      "# scores: own indicators only\n"
    }
    path_stage <- if (identical(path_res$sampler, "brms")) "brms" else "lm/glm"
    path_res$syntax <- paste0(
      "# two-step: factor scores (CFA) then path (", path_stage, ")\n",
      score_note,
      "# measurement\n",
      attached$measurement_syntax,
      "\n# path\n",
      path_res$syntax
    )
    path_res$two_step <- TRUE
    path_res$measurement_fit <- attached$measurement_fit
    path_res$measurement_score_weights <- attached$measurement_score_weights
    path_res$measurement_score_fits <- attached$measurement_score_fits
    path_res$measurement_cronbach_alpha <- attached$measurement_cronbach_alpha
    path_res$measurement_unidimensionality <- attached$measurement_unidimensionality
    path_res$latent_score_method <- attached$latent_score_method
    path_res$engine <- "path"
    return(path_res)
  }

  if (!nrow(edges)) {
    if (!length(dv_ids)) stop("No dependent variables to fit.", call. = FALSE)
    return(fit_path_equations(
      state, dat, edges, dv_ids = dv_ids,
      backend = backend, brms_args = brms_args
    ))
  }

  fit_path_equations(
    state, dat, edges, dv_ids = dv_ids,
    backend = backend, brms_args = brms_args
  )
}

#' @keywords internal
fit_lavaan_model <- function(state, dat, edges) {
  syntax <- build_lavaan_syntax(state, include_edge_ids = edges$id)
  struct <- edges[edges$type != "measurement", , drop = FALSE]
  ordered_vars <- character()
  if (nrow(struct)) {
    for (dv in unique(struct$to)) {
      if (!is_observed_node(state, dv)) next
      col <- lavaan_var_name(state, dv)
      if (!col %in% names(dat)) next
      if (identical(classify_variable(dat[[col]]), "binary")) {
        ordered_vars <- c(ordered_vars, col)
      }
    }
    ordered_vars <- unique(ordered_vars)
  }

  fit <- tryCatch(
    {
      if (length(ordered_vars)) {
        lavaan::sem(
          syntax,
          data = dat,
          ordered = ordered_vars,
          estimator = "WLSMV",
          meanstructure = FALSE
        )
      } else {
        lavaan::sem(syntax, data = dat, meanstructure = FALSE)
      }
    },
    error = function(e) stop("lavaan error: ", e$message, call. = FALSE)
  )

  if (!lavaan::lavInspect(fit, "converged")) {
    stop("lavaan model did not converge.", call. = FALSE)
  }

  pe <- as.data.frame(lavaan::parameterEstimates(fit, standardized = TRUE))
  estimates <- empty_estimates()
  if (nrow(pe)) {
    keep <- pe$op %in% c("~", "=~")
    pe <- pe[keep, , drop = FALSE]
    if (nrow(pe)) {
      std_col <- if ("std.all" %in% names(pe)) "std.all" else if ("std.lv" %in% names(pe)) "std.lv" else NA_character_
      estimates <- data.frame(
        lhs = as.character(pe$lhs),
        op = as.character(pe$op),
        rhs = as.character(pe$rhs),
        est = as.numeric(pe$est),
        se = as.numeric(pe$se),
        pvalue = as.numeric(pe$pvalue),
        est_std = if (is.na(std_col)) NA_real_ else as.numeric(pe[[std_col]]),
        df = NA_real_,
        level = NA_character_,
        kind = "effect",
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    estimates = estimates,
    fit = fit,
    fit_measures = extract_fit_measures(fit),
    r_squared = r_squared_from_fitted(list(fit = fit, engine = "lavaan"), state),
    syntax = if (length(ordered_vars)) {
      paste0(
        "# lavaan (ordered: ", paste(ordered_vars, collapse = ", "), "; WLSMV)\n",
        syntax
      )
    } else {
      paste0("# lavaan (ML)\n", syntax)
    },
    engine = "lavaan",
    ordered = ordered_vars
  )
}

#' R² (lm / lavaan) or McFadden pseudo-R² (glm) from one fitted object
#' @return list(value, kind) with kind in c("r2", "mcfadden") or NULL
#' @keywords internal
r_squared_one_fit <- function(fit) {
  if (inherits(fit, "brmsfit")) {
    r2 <- tryCatch(
      suppressWarnings(brms::bayes_R2(fit)),
      error = function(e) NULL
    )
    if (is.null(r2)) return(NULL)
    resp <- as.character(fit$smt_response %||% "")
    val <- NA_real_
    if (is.matrix(r2) && "Estimate" %in% colnames(r2)) {
      rn <- rownames(r2)
      if (!is.na(resp) && nzchar(resp) && length(rn)) {
        hit <- which(rn == resp | rn == paste0("R2", resp) | endsWith(rn, resp))
        if (length(hit)) {
          val <- as.numeric(r2[hit[[1]], "Estimate"])
        }
      }
      if (!is.finite(val)) val <- as.numeric(r2[1L, "Estimate"])
    } else {
      val <- stats::median(as.numeric(r2), na.rm = TRUE)
    }
    if (is.finite(val)) return(list(value = val, kind = "r2"))
    return(NULL)
  }
  if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    r2 <- tryCatch(summary(fit)$r.squared, error = function(e) NA_real_)
    if (is.finite(r2)) return(list(value = as.numeric(r2), kind = "r2"))
    return(NULL)
  }
  if (inherits(fit, "glm")) {
    ll_m <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
    # Avoid update(): formula env may reference a vanished local `data`
    ll_0 <- tryCatch({
      y <- fit$y
      w <- fit$prior.weights
      if (is.null(y) || !length(y)) return(NA_real_)
      if (is.null(w) || !length(w)) w <- rep(1, length(y))
      p <- stats::weighted.mean(y, w)
      if (!is.finite(p) || p <= 0 || p >= 1) return(NA_real_)
      sum(w * (y * log(p) + (1 - y) * log(1 - p)))
    }, error = function(e) NA_real_)
    if (is.finite(ll_m) && is.finite(ll_0) && abs(ll_0) > .Machine$double.eps) {
      r2 <- 1 - (ll_m / ll_0)
      if (is.finite(r2)) return(list(value = as.numeric(r2), kind = "mcfadden"))
    }
    return(NULL)
  }
  NULL
}

#' Named R² map (node id -> list(value, kind)) from a fitted result
#' @keywords internal
r_squared_from_fitted <- function(fitted, state) {
  out <- list()
  if (is.null(fitted)) return(out)

  if (identical(fitted$engine, "path") && is.list(fitted$fit) &&
        !inherits(fitted$fit, "lavaan")) {
    for (key in names(fitted$fit)) {
      # Keys may be "nodeId" or "nodeId::responseLevel"
      dv <- sub("::.*$", "", key)
      slot <- r_squared_one_fit(fitted$fit[[key]])
      if (!is.null(slot)) out[[dv]] <- slot
    }
    return(out)
  }

  fit <- fitted$fit
  if (!inherits(fit, "lavaan")) return(out)
  rsq <- tryCatch(lavaan::lavInspect(fit, "rsquare"), error = function(e) NULL)
  if (is.null(rsq) || !length(rsq)) return(out)
  # lavInspect may return a list (multi-group) or named vector
  if (is.list(rsq) && !is.null(names(rsq)) && !is.numeric(rsq)) {
    rsq <- rsq[[1]]
  }
  rsq <- as.numeric(rsq)
  if (is.null(names(rsq)) || !length(rsq)) return(out)

  for (nm in names(rsq)) {
    val <- unname(rsq[[nm]])
    if (!is.finite(val)) next
    # Map lavaan name -> node id
    hit <- which(vapply(state$nodes$id, function(id) {
      identical(lavaan_var_name(state, id), nm)
    }, logical(1)))
    if (!length(hit)) next
    # Ordered / categorical endogenous: skip classical R²
    ordered <- tryCatch(lavaan::lavInspect(fit, "ordered"), error = function(e) character())
    if (is.null(ordered)) ordered <- character()
    if (nm %in% ordered) next
    out[[state$nodes$id[hit[1]]]] <- list(value = val, kind = "r2")
  }
  out
}

#' Max polynomial degree implied by inspect-panel relationship kind
#'
#' `loess` is represented parametrically as a cubic polynomial in the model.
#' @keywords internal
rel_kind_degree <- function(rel_kind) {
  switch(
    as.character(rel_kind %||% "linear"),
    linear = 1L,
    none = 1L, # legacy projects
    quadratic = 2L,
    cubic = 3L,
    loess = 3L,
    1L
  )
}

#' Column name for a polynomial power of a predictor
#' @keywords internal
poly_col_name <- function(var, degree) {
  if (as.integer(degree) <= 1L) return(as.character(var))
  paste0(sanitize_id(var), "_p", as.integer(degree))
}

#' Level tag stored on estimate rows for poly powers (^2, ^3, …)
#' @keywords internal
poly_level_tag <- function(degree) {
  d <- as.integer(degree)
  if (is.na(d) || d <= 1L) return(NA_character_)
  paste0("^", d)
}

#' True if an estimate level encodes a polynomial power
#' @keywords internal
is_poly_level <- function(level) {
  !is.na(level) && grepl("^\\^[0-9]+$", as.character(level))
}

#' Add polynomial powers of a continuous predictor to a data frame
#'
#' Degree 1 uses the raw variable. Higher powers use centered x for stability.
#' @return list(data, term_names, term_meta)
#' @keywords internal
expand_continuous_predictor <- function(local_dat, xn, x_num, max_degree = 1L) {
  max_degree <- as.integer(max_degree)
  if (!is.finite(max_degree) || max_degree < 1L) max_degree <- 1L
  x_num <- as.numeric(x_num)
  local_dat[[xn]] <- x_num
  term_names <- xn
  term_meta <- list()
  term_meta[[xn]] <- list(
    rhs = xn, level = NA_character_, kind = "effect", degree = 1L
  )
  if (max_degree >= 2L) {
    xc <- x_num - mean(x_num, na.rm = TRUE)
    for (d in 2:max_degree) {
      nm <- poly_col_name(xn, d)
      local_dat[[nm]] <- xc^d
      term_names <- c(term_names, nm)
      term_meta[[nm]] <- list(
        rhs = xn, level = poly_level_tag(d), kind = "effect", degree = d
      )
    }
  }
  list(data = local_dat, term_names = term_names, term_meta = term_meta)
}

#' Outcome display / estimate lhs for a DV equation
#' @keywords internal
dv_outcome_lhs <- function(yname, response_level = NA_character_) {
  if (is.null(response_level) || is.na(response_level) || !nzchar(response_level)) {
    return(as.character(yname))
  }
  paste0(yname, "[", response_level, "]")
}

#' Allowed DV outcome transforms (continuous only)
#' @keywords internal
dv_transform_choices <- function() {
  c(
    "None" = "none",
    "log (natural)" = "log",
    "log10" = "log10",
    "log(1 + x)" = "log1p",
    "sqrt" = "sqrt"
  )
}

#' Display label for a transformed outcome, e.g. log(Y)
#' @keywords internal
dv_transform_lhs <- function(yname, transform = "none") {
  yname <- as.character(yname)
  if (is.null(transform) || is.na(transform) || !nzchar(transform) ||
        identical(transform, "none")) {
    return(yname)
  }
  switch(
    as.character(transform),
    log = paste0("log(", yname, ")"),
    log10 = paste0("log10(", yname, ")"),
    log1p = paste0("log1p(", yname, ")"),
    sqrt = paste0("sqrt(", yname, ")"),
    yname
  )
}

#' Apply a numeric outcome transform; errors on invalid domain
#' @keywords internal
apply_dv_transform <- function(x, transform = "none", yname = "y") {
  if (is.null(transform) || is.na(transform) || !nzchar(transform) ||
        identical(transform, "none")) {
    return(x)
  }
  x <- as.numeric(x)
  switch(
    as.character(transform),
    log = {
      if (any(is.finite(x) & x <= 0)) {
        stop(
          "Cannot take log of '", yname, "': values must be > 0.",
          call. = FALSE
        )
      }
      log(x)
    },
    log10 = {
      if (any(is.finite(x) & x <= 0)) {
        stop(
          "Cannot take log10 of '", yname, "': values must be > 0.",
          call. = FALSE
        )
      }
      log10(x)
    },
    log1p = {
      if (any(is.finite(x) & x < -1)) {
        stop(
          "Cannot take log1p of '", yname, "': values must be >= -1.",
          call. = FALSE
        )
      }
      log1p(x)
    },
    sqrt = {
      if (any(is.finite(x) & x < 0)) {
        stop(
          "Cannot take sqrt of '", yname, "': values must be >= 0.",
          call. = FALSE
        )
      }
      sqrt(x)
    },
    x
  )
}

#' Build design columns for one DV equation from structural edges
#'
#' @param response_level For categorical DV split: category coded 1 vs rest.
#' @keywords internal
build_path_terms <- function(state, dat, preds, response_level = NA_character_) {
  state <- ensure_edge_to_port(state)
  y_id <- preds$to[1]
  yname <- lavaan_var_name(state, y_id)
  if (!yname %in% names(dat)) {
    stop("Dependent variable '", yname, "' not found in analysis data.", call. = FALSE)
  }
  y_kind <- classify_variable(dat[[yname]])
  local_dat <- dat
  lhs_name <- yname
  y_transform <- node_dv_transform(state, y_id)

  if (identical(y_kind, "categorical")) {
    if (is.null(response_level) || is.na(response_level) || !nzchar(response_level)) {
      stop(
        "Dependent variable '", yname, "' is multi-level categorical. ",
        "In split mode, wire predictors into a specific response level ",
        "(one-vs-rest). Omnibus categorical DVs are not supported yet.",
        call. = FALSE
      )
    }
    # One-vs-rest binary outcome (transforms do not apply)
    local_dat[[yname]] <- as.integer(
      as.character(dat[[yname]]) == as.character(response_level)
    )
    y_kind <- "binary"
    lhs_name <- dv_outcome_lhs(yname, response_level)
  } else if (identical(y_kind, "binary")) {
    local_dat[[yname]] <- as_binary01(local_dat[[yname]])
  } else if (!identical(y_transform, "none")) {
    local_dat[[yname]] <- apply_dv_transform(
      local_dat[[yname]], y_transform, yname = yname
    )
    lhs_name <- dv_transform_lhs(yname, y_transform)
  }

  term_names <- character()
  term_meta <- list()
  omnibus_groups <- list()
  processed_cat <- character()

  for (i in seq_len(nrow(preds))) {
    from_id <- preds$from[i]

    # Interaction product(s) — may expand to multiple terms when categorical
    if (is_interaction_node(state, from_id)) {
      if (interaction_involves_categorical(state, dat, from_id)) {
        ix_exp <- expand_interaction_terms(state, dat, from_id, local_dat)
        local_dat <- ix_exp$data
        for (nm in ix_exp$term_names) {
          if (!nm %in% term_names) {
            term_names <- c(term_names, nm)
            term_meta[[nm]] <- ix_exp$term_meta[[nm]]
          }
        }
        if (length(ix_exp$omnibus_terms)) {
          omnibus_groups[[lavaan_var_name(state, from_id)]] <- ix_exp$omnibus_terms
        }
        next
      }
      fac <- interaction_factors(state, from_id)
      if (is.na(fac$a) || is.na(fac$b) || !nzchar(fac$a) || !nzchar(fac$b)) {
        stop(
          "Interaction '", node_display_name(state, from_id),
          "' needs two predictors wired into its input ports.",
          call. = FALSE
        )
      }
    }

    xn <- lavaan_var_name(state, from_id)
    if (!xn %in% names(dat) && !xn %in% names(local_dat)) {
      stop("Predictor '", xn, "' not found in analysis data.", call. = FALSE)
    }
    if (!xn %in% names(dat) && xn %in% names(local_dat)) {
      # Product already in local_dat (continuous×continuous from prepare)
      dat_xn <- local_dat[[xn]]
    } else {
      dat_xn <- dat[[xn]]
    }
    lvl <- if ("level" %in% names(preds)) preds$level[i] else NA_character_
    coding <- variable_coding_for_node(state, from_id, local_dat)
    kind <- classify_variable(dat_xn)
    if (identical(coding$kind, "categorical")) kind <- "categorical"
    rel_kind <- if ("rel_kind" %in% names(preds)) preds$rel_kind[i] else "linear"
    if (is.na(rel_kind) || !nzchar(rel_kind)) rel_kind <- "linear"
    # Prefer rel_kind from full state (preds may be a filtered copy)
    if ("id" %in% names(preds)) {
      eidx <- match(preds$id[i], state$edges$id)
      if (!is.na(eidx) && "rel_kind" %in% names(state$edges)) {
        rk <- state$edges$rel_kind[eidx]
        if (!is.na(rk) && nzchar(rk)) rel_kind <- rk
      }
    }
    cat_mode <- edge_cat_mode(preds, i, state)

    if (identical(kind, "categorical")) {
      if (from_id %in% processed_cat) next
      processed_cat <- c(processed_cat, from_id)
      # Ordered factor + polynomial link → integer scores + poly expansion
      # (keeps qualitative labels in the data; linear/quadratic in the model).
      deg_ord <- rel_kind_degree(rel_kind)
      if (isTRUE(coding$ordered) && deg_ord >= 2L) {
        # Ordinal scores must match Simulate / prior overlay: use declared
        # factor_levels order, not treatment-coding all_levels (base-first).
        meta_ord <- node_factor_coding(state, from_id)
        levs <- meta_ord$levels
        if (!length(levs)) levs <- coding$all_levels
        if (!length(levs)) {
          stop("Ordered predictor '", xn, "' has no levels.", call. = FALSE)
        }
        x_num <- ordered_level_scores(
          dat[[xn]],
          levels_order = levs,
          base = meta_ord$base %||% coding$base
        )
        expanded <- expand_continuous_predictor(
          local_dat, xn, x_num, max_degree = deg_ord
        )
        local_dat <- expanded$data
        for (nm in expanded$term_names) {
          if (!nm %in% term_names) {
            term_names <- c(term_names, nm)
            term_meta[[nm]] <- expanded$term_meta[[nm]]
          }
        }
        next
      }
      idxs <- which(preds$from == from_id)
      modes <- vapply(idxs, function(j) edge_cat_mode(preds, j, state), character(1))
      levels_present <- if ("level" %in% names(preds)) {
        preds$level[idxs]
      } else {
        rep(NA_character_, length(idxs))
      }
      is_omni_mode <- any(modes == "omnibus")
      unleveled <- all(is.na(levels_present) | !nzchar(as.character(levels_present)))
      # Omnibus or a single unleveled edge → all non-base dummies
      use_all_levels <- is_omni_mode || unleveled
      if (!length(coding$levels)) {
        stop("Categorical predictor '", xn, "' has no non-base levels.", call. = FALSE)
      }
      dnames <- character()
      levels_to_use <- if (use_all_levels) {
        coding$levels
      } else {
        unique(as.character(levels_present[!is.na(levels_present) & nzchar(levels_present)]))
      }
      for (lv in levels_to_use) {
        if (identical(lv, coding$base)) {
          stop(
            "Level '", lv, "' is the reference base for '", xn, "'. ",
            "Connect a non-base level instead.",
            call. = FALSE
          )
        }
        dname <- dummy_col_name(xn, lv)
        local_dat[[dname]] <- as.integer(as.character(dat[[xn]]) == as.character(lv))
        if (!dname %in% term_names) {
          term_names <- c(term_names, dname)
          term_meta[[dname]] <- list(rhs = xn, level = as.character(lv), kind = "effect")
        }
        dnames <- c(dnames, dname)
      }
      if (is_omni_mode && length(dnames)) {
        omnibus_groups[[xn]] <- unique(dnames)
      }
      next
    }

    if (!is.na(lvl) && nzchar(lvl)) {
      # Explicit contrast for binary-as-level or misc
      dname <- dummy_col_name(xn, lvl)
      local_dat[[dname]] <- as.integer(as.character(dat_xn) == as.character(lvl))
      term_names <- c(term_names, dname)
      term_meta[[dname]] <- list(rhs = xn, level = as.character(lvl), kind = "effect")
    } else if (identical(kind, "binary")) {
      local_dat[[xn]] <- as_binary01(dat_xn)
      term_names <- c(term_names, xn)
      term_meta[[xn]] <- list(rhs = xn, level = NA_character_, kind = "effect")
    } else {
      # Continuous (or interaction product already numeric): poly expansion
      expanded <- expand_continuous_predictor(
        local_dat, xn, dat_xn, max_degree = rel_kind_degree(rel_kind)
      )
      local_dat <- expanded$data
      for (nm in expanded$term_names) {
        if (!nm %in% term_names) {
          term_names <- c(term_names, nm)
          term_meta[[nm]] <- expanded$term_meta[[nm]]
        }
      }
    }
  }

  list(
    yname = yname,
    lhs_name = lhs_name,
    y_kind = y_kind,
    response_level = if (is.na(response_level) || !nzchar(as.character(response_level))) {
      NA_character_
    } else {
      as.character(response_level)
    },
    data = local_dat,
    term_names = unique(term_names),
    term_meta = term_meta,
    omnibus_groups = omnibus_groups
  )
}

#' Resolve cluster column for a DV (or NULL if RM not active)
#' @keywords internal
dv_cluster_column <- function(state, dv_id, dat) {
  rm <- node_repeated_measures(state, dv_id)
  if (!isTRUE(rm$enabled)) return(NULL)
  cl <- rm$cluster
  if (is.na(cl) || !nzchar(cl)) {
    stop(
      "Node '", node_display_name(state, dv_id),
      "' has repeated measures enabled but no clustering variable.",
      call. = FALSE
    )
  }
  if (is.null(dat) || !cl %in% names(dat)) {
    stop(
      "Clustering variable '", cl, "' for DV '",
      node_display_name(state, dv_id), "' not found in the dataset.",
      call. = FALSE
    )
  }
  cl
}

#' Whether a within-cluster factor has enough trials for random slopes
#'
#' Requires within-cluster variation and median rows per `(cluster × level) ≥ min_trials`.
#' @keywords internal
factor_slope_cell_ok <- function(x, cluster, min_trials = 2) {
  ok <- !is.na(x) & !is.na(cluster) & nzchar(as.character(x)) &
    nzchar(as.character(cluster))
  if (!any(ok)) return(FALSE)
  x <- as.character(x[ok])
  cl <- as.character(cluster[ok])
  nlev <- tapply(x, cl, function(z) length(unique(z)))
  if (!any(as.integer(nlev) > 1L, na.rm = TRUE)) return(FALSE)
  cell_n <- as.numeric(table(cl, x))
  cell_n <- cell_n[cell_n > 0]
  if (!length(cell_n)) return(FALSE)
  stats::median(cell_n) >= min_trials
}

#' Predictor column names eligible for per-level cluster random slopes
#'
#' Categorical/binary predictors that are Intra by design or vary within
#' cluster, with median ≥2 trials per `(cluster × level)`.
#' @keywords internal
dv_random_slope_terms <- function(state, dv, dat, cluster, pred_edges,
                                  min_trials = 2) {
  if (is.null(dat) || is.null(cluster) || !nzchar(as.character(cluster)) ||
        !cluster %in% names(dat) || is.null(pred_edges) || !nrow(pred_edges)) {
    return(character())
  }
  from_ids <- unique(as.character(pred_edges$from))
  out <- character()
  for (from_id in from_ids) {
    if (is_interaction_node(state, from_id)) next
    xn <- tryCatch(lavaan_var_name(state, from_id), error = function(e) NA_character_)
    if (is.na(xn) || !nzchar(xn) || !xn %in% names(dat)) next
    kind <- classify_variable(dat[[xn]])
    coding <- tryCatch(
      variable_coding_for_node(state, from_id, dat),
      error = function(e) NULL
    )
    if (!is.null(coding) && identical(coding$kind, "categorical")) {
      kind <- "categorical"
    }
    if (!kind %in% c("categorical", "binary")) next
    # Intra design or observed within-cluster variation, with ≥min_trials/cell
    if (!factor_slope_cell_ok(dat[[xn]], dat[[cluster]], min_trials)) next
    if (!xn %in% out) out <- c(out, xn)
  }
  out
}

#' Build lmer/glmer random-effects term string
#' @keywords internal
rm_re_term <- function(cluster, slope_terms = NULL) {
  slope_terms <- unique(as.character(slope_terms %||% character()))
  slope_terms <- slope_terms[nzchar(slope_terms)]
  if (!length(slope_terms)) {
    return(paste0("(1|", cluster, ")"))
  }
  paste0("(1 + ", paste(slope_terms, collapse = " + "), "|", cluster, ")")
}

#' Ensure slope factor columns exist on local data (from original dat)
#' @keywords internal
attach_slope_factors <- function(local_dat, dat, slope_terms) {
  slope_terms <- unique(as.character(slope_terms %||% character()))
  for (nm in slope_terms) {
    if (!nm %in% names(dat)) next
    local_dat[[nm]] <- as.factor(dat[[nm]])
  }
  local_dat
}

#' Fit one path equation (lm/glm or lmer/glmer random intercept / slopes)
#' @keywords internal
fit_one_path_equation <- function(yname, y_kind, rhs_terms, local_dat,
                                  cluster = NULL, slope_terms = NULL,
                                  backend = c("freq", "brms"),
                                  brms_args = NULL,
                                  state = NULL, preds = NULL,
                                  term_meta = NULL) {
  backend <- match.arg(backend)
  rhs <- if (!length(rhs_terms)) "1" else paste(rhs_terms, collapse = " + ")
  binary <- identical(y_kind, "binary")
  re <- NULL
  if (!is.null(cluster)) {
    local_dat[[cluster]] <- as.factor(local_dat[[cluster]])
    re <- rm_re_term(cluster, slope_terms)
    fml <- stats::as.formula(paste(yname, "~", rhs, "+", re))
  } else {
    fml <- stats::as.formula(paste(yname, "~", rhs))
  }
  if (identical(backend, "brms")) {
    prep <- brms_prepare_path_input(
      data = local_dat,
      yname = yname,
      rhs_terms = rhs_terms,
      slope_terms = slope_terms,
      cluster = cluster,
      term_meta = term_meta
    )
    rhs <- if (!length(prep$rhs_terms)) "1" else paste(prep$rhs_terms, collapse = " + ")
    re <- NULL
    if (!is.null(prep$cluster)) {
      prep$data[[prep$cluster]] <- as.factor(prep$data[[prep$cluster]])
      re <- rm_re_term(prep$cluster, prep$slope_terms)
      fml <- stats::as.formula(paste(prep$yname, "~", rhs, "+", re))
    } else {
      fml <- stats::as.formula(paste(prep$yname, "~", rhs))
    }
    out <- fit_one_brms_equation(
      yname = prep$yname,
      y_kind = y_kind,
      formula = fml,
      data = prep$data,
      rhs = rhs,
      re = re,
      brms_args = brms_args,
      state = state,
      preds = preds,
      term_meta = prep$term_meta
    )
    out$fit <- brms_tag_name_maps(out$fit, prep$to_safe, prep$from_safe)
    # Prefer SMT dummy names (Var__Level) in the printed syntax
    syn <- out$syntax
    orgs <- names(prep$to_safe)
    orgs <- orgs[order(nchar(unname(prep$to_safe[orgs])), decreasing = TRUE)]
    for (orig in orgs) {
      safe <- unname(prep$to_safe[[orig]])
      if (!identical(orig, safe)) syn <- gsub(safe, orig, syn, fixed = TRUE)
    }
    out$syntax <- syn
    return(out)
  }
  if (!is.null(cluster)) {
    if (binary) {
      fit <- lme4::glmer(fml, data = local_dat, family = stats::binomial())
      syntax <- paste0("glmer(binomial): ", yname, " ~ ", rhs, " + ", re)
      eng <- "glmer"
    } else {
      fit <- lmerTest::lmer(fml, data = local_dat)
      syntax <- paste0("lmer: ", yname, " ~ ", rhs, " + ", re)
      eng <- "lmer"
    }
  } else if (binary) {
    fit <- stats::glm(fml, data = local_dat, family = stats::binomial())
    syntax <- paste0("glm(binomial): ", yname, " ~ ", rhs)
    eng <- "glm"
  } else {
    fit <- stats::lm(fml, data = local_dat)
    syntax <- paste0("lm: ", yname, " ~ ", rhs)
    eng <- "lm"
  }
  list(fit = fit, syntax = syntax, engine = eng)
}

#' Collect path-equation jobs (data + terms) without fitting
#' @keywords internal
collect_path_equation_jobs <- function(state, dat, edges, dv_ids = NULL) {
  state <- ensure_node_rm_columns(state)
  struct <- edges[edges$type != "measurement", , drop = FALSE]
  if (!"level" %in% names(struct)) struct$level <- NA_character_
  if (!"to_level" %in% names(struct)) struct$to_level <- NA_character_
  if (is.null(dv_ids) || !length(dv_ids)) {
    if (!nrow(struct)) stop("No structural links to fit.", call. = FALSE)
    dv_ids <- unique(struct$to)
  }

  jobs <- list()
  for (dv in dv_ids) {
    preds_all <- if (nrow(struct)) {
      struct[struct$to == dv, , drop = FALSE]
    } else {
      struct[0, , drop = FALSE]
    }
    cluster <- dv_cluster_column(state, dv, dat)
    rm_info <- node_repeated_measures(state, dv)
    yname <- lavaan_var_name(state, dv)
    if (!yname %in% names(dat)) {
      stop("Dependent variable '", yname, "' not found in analysis data.", call. = FALSE)
    }
    y_kind0 <- classify_variable(dat[[yname]])

    if (identical(y_kind0, "categorical")) {
      if (!identical(node_cat_mode(state, dv), "split")) {
        stop(
          "Dependent variable '", yname, "' is categorical in omnibus mode. ",
          "Switch the node to split and wire predictors into a response level ",
          "(one-vs-rest).",
          call. = FALSE
        )
      }
      if (!nrow(preds_all)) {
        stop(
          "Categorical DV '", yname, "' needs predictors wired into a response level.",
          call. = FALSE
        )
      }
      resp_levels <- unique(as.character(preds_all$to_level))
      resp_levels <- resp_levels[!is.na(resp_levels) & nzchar(resp_levels)]
      if (!length(resp_levels)) {
        stop(
          "Categorical DV '", yname, "': drop links on a specific left-hand ",
          "response port (split), not the node body.",
          call. = FALSE
        )
      }
    } else {
      resp_levels <- NA_character_
    }

    for (tl in resp_levels) {
      preds <- if (is.na(tl) || !nzchar(tl)) {
        preds_all
      } else {
        preds_all[!is.na(preds_all$to_level) & preds_all$to_level == tl, , drop = FALSE]
      }
      fit_key <- if (is.na(tl) || !nzchar(tl)) dv else paste0(dv, "::", tl)

      slope_terms <- character()
      if (!is.null(cluster) && isTRUE(rm_info$slopes) && nrow(preds)) {
        slope_terms <- dv_random_slope_terms(state, dv, dat, cluster, preds)
      }

      if (!nrow(preds)) {
        if (identical(y_kind0, "categorical")) next
        local_dat <- dat
        y_kind <- y_kind0
        lhs_disp <- yname
        if (identical(y_kind, "binary")) {
          local_dat[[yname]] <- as_binary01(local_dat[[yname]])
        } else {
          y_tr <- node_dv_transform(state, dv)
          if (!identical(y_tr, "none")) {
            local_dat[[yname]] <- apply_dv_transform(
              local_dat[[yname]], y_tr, yname = yname
            )
            lhs_disp <- dv_transform_lhs(yname, y_tr)
          }
        }
        jobs[[length(jobs) + 1L]] <- list(
          key = fit_key,
          dv = dv,
          yname = yname,
          y_kind = y_kind,
          term_names = character(),
          data = local_dat,
          cluster = cluster,
          slope_terms = character(),
          preds = preds,
          term_meta = list(),
          lhs_name = lhs_disp,
          omnibus_groups = list()
        )
        next
      }

      built <- build_path_terms(state, dat, preds, response_level = tl)
      if (!length(built$term_names)) {
        stop("No predictors for dependent variable '", built$lhs_name, "'.", call. = FALSE)
      }
      local_fit_dat <- attach_slope_factors(built$data, dat, slope_terms)
      jobs[[length(jobs) + 1L]] <- list(
        key = fit_key,
        dv = dv,
        yname = built$yname,
        y_kind = built$y_kind,
        term_names = built$term_names,
        data = local_fit_dat,
        cluster = cluster,
        slope_terms = slope_terms,
        preds = preds,
        term_meta = built$term_meta,
        lhs_name = built$lhs_name,
        omnibus_groups = built$omnibus_groups %||% list()
      )
    }
  }
  jobs
}

#' Append estimate rows for one fitted path equation job
#' @keywords internal
append_path_job_estimates <- function(est_rows, job, fit) {
  if (!length(job$term_names) && !length(job$omnibus_groups)) {
    return(est_rows)
  }
  ct <- path_coef_table(fit)
  bstd <- standardized_term_coefs(fit)
  lhs <- job$lhs_name
  omni <- job$omnibus_groups %||% list()
  ci_level <- if (inherits(fit, "brmsfit")) as.numeric(fit$smt_ci %||% 0.95) else NA_real_
  for (xn in names(omni)) {
    if (inherits(fit, "brmsfit")) {
      excl <- cri_region_excludes_zero(fit, omni[[xn]], ci = ci_level)
      est_rows[[length(est_rows) + 1L]] <- estimate_row(
        lhs = lhs, op = "~", rhs = xn,
        est = NA_real_, se = NA_real_, pvalue = NA_real_,
        est_std = NA_real_, df = length(omni[[xn]]),
        level = NA_character_, kind = "omnibus",
        ci_level = ci_level,
        includes_zero = if (is.na(excl)) NA else !isTRUE(excl)
      )
    } else {
      wt <- joint_wald_test(fit, omni[[xn]])
      est_rows[[length(est_rows) + 1L]] <- estimate_row(
        lhs = lhs, op = "~", rhs = xn,
        est = NA_real_, se = NA_real_, pvalue = wt$pvalue,
        est_std = NA_real_, df = wt$df,
        level = NA_character_, kind = "omnibus"
      )
    }
  }
  for (tn in job$term_names) {
    if (is.null(ct) || !tn %in% rownames(ct)) next
    meta <- job$term_meta[[tn]]
    bounds <- path_coef_bounds(ct, tn)
    bay <- path_coef_bayes_fields(ct, tn)
    est_rows[[length(est_rows) + 1L]] <- estimate_row(
      lhs = lhs, op = "~", rhs = meta$rhs,
      est = as.numeric(ct[tn, 1L]),
      se = as.numeric(ct[tn, 2L]),
      pvalue = if (is.finite(bounds[[1]])) NA_real_ else path_coef_pvalue(ct, tn),
      est_std = if (tn %in% names(bstd)) as.numeric(bstd[[tn]]) else NA_real_,
      df = 1,
      level = meta$level,
      kind = "effect",
      ci_low = bounds[[1]],
      ci_high = bounds[[2]],
      ci_level = if (is.finite(bounds[[1]])) ci_level else NA_real_,
      includes_zero = bay$includes_zero,
      pd = bay$pd,
      rope_low = bay$rope_low,
      rope_high = bay$rope_high,
      rope_pct = bay$rope_pct,
      rope_decision = bay$rope_decision,
      log_bf10 = bay$log_bf10,
      kl_prior_post = bay$kl_prior_post,
      post_narrow = bay$post_narrow,
      term = tn
    )
  }
  est_rows
}

#' Equation-wise lm / glm / lmer / glmer path models (observed variables only)
#'
#' Bayesian backend: `brms_args$structure` may be `"independent"` (default) or
#' `"joint"` (one multivariate gaussian model; residual correlations when
#' outcomes do not predict each other — mediation turns rescor off;
#' otherwise falls back to independent equations).
#'
#' @param dv_ids Optional DV node ids (from the full graph). Ensures intercept-only
#'   equations when a candidate omits all predictors for a DV.
#' @keywords internal
fit_path_equations <- function(state, dat, edges, dv_ids = NULL,
                             backend = c("freq", "brms"),
                             brms_args = NULL) {
  backend <- match.arg(backend)
  if (identical(backend, "brms")) {
    brms_args <- normalize_brms_args(brms_args)
  }
  jobs <- collect_path_equation_jobs(state, dat, edges, dv_ids)

  est_rows <- list()
  syntax_lines <- character()
  fits <- list()
  aic_sum <- 0
  bic_sum <- 0
  npar_sum <- 0L
  bayes_structure <- if (identical(backend, "brms")) brms_args$structure else NA_character_
  joint_note <- NULL

  want_joint <- identical(backend, "brms") &&
    identical(brms_args$structure %||% "independent", "joint")
  if (want_joint) {
    joint <- fit_joint_brms_path(jobs, state, brms_args)
    if (isTRUE(joint$ok)) {
      bayes_structure <- "joint"
      syntax_lines <- c(syntax_lines, joint$syntax)
      for (job in jobs) {
        tp <- term_priors_from_path(state, job$preds, job$term_meta)
        fit_view <- tag_brms_equation_view(
          joint$fit, job$yname, brms_args$ci, brms_args$rope_k, tp,
          structure = "joint"
        )
        fits[[job$key]] <- fit_view
        if (length(job$term_names) || length(job$omnibus_groups)) {
          est_rows <- append_path_job_estimates(est_rows, job, fit_view)
        }
      }
      npar_sum <- nrow(as.data.frame(brms::fixef(joint$fit)))
    } else {
      bayes_structure <- "independent"
      joint_note <- paste0(
        "# joint structure requested but unavailable (",
        joint$reason %||% "unknown",
        "); using independent equations"
      )
    }
  }

  if (!length(fits)) {
    for (job in jobs) {
      fitted_one <- fit_one_path_equation(
        job$yname, job$y_kind, job$term_names, job$data, job$cluster,
        slope_terms = job$slope_terms,
        backend = backend, brms_args = brms_args,
        state = state, preds = job$preds, term_meta = job$term_meta
      )
      syn <- fitted_one$syntax
      if (!identical(job$lhs_name, job$yname)) {
        syn <- sub(
          paste0("^((?:brms|glmer|glm|lmer|lm)[^:]*): ", job$yname),
          paste0("\\1: ", job$lhs_name),
          syn
        )
      }
      fit <- fitted_one$fit
      if (inherits(fit, "brmsfit")) {
        fit <- tag_brms_equation_view(
          fit, NULL, brms_args$ci, brms_args$rope_k,
          term_priors_from_path(state, job$preds, job$term_meta),
          structure = "independent"
        )
      }
      fits[[job$key]] <- fit
      syntax_lines <- c(syntax_lines, syn)
      if (!identical(backend, "brms")) {
        aic_sum <- aic_sum + stats::AIC(fit)
        bic_sum <- bic_sum + stats::BIC(fit)
      }
      npar_sum <- npar_sum + length(path_fixed_coefs(fit))
      if (length(job$term_names) || length(job$omnibus_groups)) {
        est_rows <- append_path_job_estimates(est_rows, job, fit)
      }
    }
  }

  estimates <- if (length(est_rows)) {
    do.call(rbind, est_rows)
  } else {
    empty_estimates()
  }

  if (identical(backend, "brms")) {
    fit_measures <- data.frame(
      measure = c(
        "equations", "npar", "chains", "iter", "warmup", "ci", "rope_k",
        "structure_joint"
      ),
      value = c(
        as.numeric(length(fits)),
        as.numeric(npar_sum),
        as.numeric(brms_args$chains),
        as.numeric(brms_args$iter),
        as.numeric(brms_args$warmup),
        as.numeric(brms_args$ci),
        as.numeric(brms_args$rope_k),
        as.numeric(identical(bayes_structure, "joint"))
      ),
      stringsAsFactors = FALSE
    )
    header <- sprintf(
      "# path models (brms · %s · %d%% HDI · ROPE k=%.2f · %d chains · %d iter · warmup %d · seed %d)",
      bayes_structure,
      round(100 * brms_args$ci), brms_args$rope_k,
      brms_args$chains, brms_args$iter, brms_args$warmup, brms_args$seed
    )
  } else {
    fit_measures <- data.frame(
      measure = c("equations", "npar", "aic", "bic"),
      value = c(as.numeric(length(fits)), as.numeric(npar_sum), aic_sum, bic_sum),
      stringsAsFactors = FALSE
    )
    header <- "# path models (equation-wise)"
  }

  syntax_body <- syntax_lines
  if (!is.null(joint_note)) syntax_body <- c(joint_note, syntax_body)
  result <- list(
    estimates = estimates,
    fit = fits,
    fit_measures = fit_measures,
    syntax = paste(c(header, syntax_body), collapse = "\n"),
    engine = "path",
    sampler = if (identical(backend, "brms")) "brms" else "freq",
    bayes_structure = bayes_structure,
    ordered = character()
  )
  result$r_squared <- r_squared_from_fitted(result, state)
  result
}
