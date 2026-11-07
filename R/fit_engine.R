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
  # Ports / dummies: categorical only (3+). Binary keeps empty `levels`.
  if (identical(kind, "binary") && !isTRUE(force_factor)) {
    return(list(
      kind = kind, base = levs[[1]], levels = character(),
      all_levels = levs, ordered = FALSE
    ))
  }
  if (identical(kind, "binary")) {
    return(list(
      kind = "binary", base = levs[[1]], levels = character(),
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

#' True if the model involves latent / measurement structure
#' @keywords internal
model_uses_latents <- function(state, edges = state$edges) {
  if (!nrow(state$nodes) || !nrow(edges)) return(FALSE)
  if (any(edges$type == "measurement")) return(TRUE)
  used <- unique(c(edges$from, edges$to))
  any(vapply(used, function(id) is_latent_node(state, id), logical(1)))
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
    stringsAsFactors = FALSE
  )
}

#' Append one estimate row
#' @keywords internal
estimate_row <- function(lhs, op, rhs, est, se, pvalue,
                         est_std = NA_real_,
                         df = NA_real_, level = NA_character_, kind = "effect") {
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
    stringsAsFactors = FALSE
  )
}

#' Fixed-effect coefficients (works for lm/glm and merMod)
#' @keywords internal
path_fixed_coefs <- function(fit) {
  if (inherits(fit, "merMod")) {
    return(lme4::fixef(fit))
  }
  stats::coef(fit)
}

#' Coefficient table for path engines (Estimate / SE / p)
#' @return matrix/data.frame with rownames = term names
#' @keywords internal
path_coef_table <- function(fit) {
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

#' Standardized coefficients for lm / glm terms (excl. intercept)
#'
#' lm: b * sd(x) / sd(y).
#' glm: b * sd(x) / sqrt(var(η) + π²/3) (latent-y style, logit/probit-friendly).
#' Mixed models: left NA for now.
#' @keywords internal
standardized_term_coefs <- function(fit) {
  cf <- path_fixed_coefs(fit)
  out <- setNames(rep(NA_real_, length(cf)), names(cf))
  if (inherits(fit, "merMod")) return(out)
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(mm)) return(out)

  if (inherits(fit, "glm")) {
    eta <- fit$linear.predictors
    sy <- sqrt(stats::var(eta) + (pi^2) / 3)
  } else {
    y <- tryCatch(stats::model.response(stats::model.frame(fit)), error = function(e) NULL)
    sy <- if (is.null(y)) NA_real_ else stats::sd(as.numeric(y))
  }
  if (!is.finite(sy) || sy <= 0) sy <- 1

  for (nm in names(cf)) {
    if (identical(nm, "(Intercept)")) next
    if (!nm %in% colnames(mm)) next
    sx <- stats::sd(mm[, nm])
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
  V <- tryCatch(as.matrix(stats::vcov(fit)), error = function(e) NULL)
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
  cm <- node_cat_mode(state, node_id)

  if (identical(kind, "categorical")) {
    coding <- variable_coding_for_node(state, node_id, dat)
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
  a_omni <- identical(node_cat_mode(state, fac$a), "omnibus") &&
    identical(classify_variable(dat[[lavaan_var_name(state, fac$a)]]), "categorical")
  b_omni <- identical(node_cat_mode(state, fac$b), "omnibus") &&
    identical(classify_variable(dat[[lavaan_var_name(state, fac$b)]]), "categorical")

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

  # Latents are scored then treated as continuous in the path stage
  if (is_latent_node(state, node_id)) {
    return(if (use_rm) "lmer" else "lm")
  }

  col <- state$nodes$observed_column[match(node_id, state$nodes$id)]
  if (is.na(col) || !nzchar(col)) {
    return(if (use_rm) "lmer" else "lm")
  }
  if (is.null(data) || !col %in% names(data)) {
    return(if (use_rm) "lmer" else "lm")
  }
  kind <- classify_variable(data[[col]])
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

#' Stage 1: build factor scores from measurement links (graph unchanged)
#'
#' Returns analysis data with score columns, a path-ready state (latents mapped
#' to those columns), CFA fit/estimates/syntax, and latent ids.
#' @keywords internal
attach_latent_factor_scores <- function(state, dat) {
  validate_model_state(state, state$edges)
  meas <- state$edges[state$edges$type == "measurement", , drop = FALSE]
  if (!nrow(meas)) {
    stop(
      "Latent nodes need measurement links to observed indicators before fitting.",
      call. = FALSE
    )
  }
  latent_ids <- unique(as.character(meas$from))
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

  fit <- tryCatch(
    lavaan::cfa(syntax, data = dat, meanstructure = FALSE, warn = FALSE),
    error = function(e) stop("Factor-score CFA error: ", e$message, call. = FALSE)
  )
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
    stop("Factor-score CFA did not converge.", call. = FALSE)
  }

  sc <- tryCatch(
    lavaan::lavPredict(fit, type = "lv", method = "regression"),
    error = function(e) stop("Could not compute factor scores: ", e$message, call. = FALSE)
  )
  sc <- as.data.frame(sc, stringsAsFactors = FALSE)
  if (!nrow(sc) || !ncol(sc)) {
    stop("Factor-score prediction returned no scores.", call. = FALSE)
  }
  # Align to cases used by lavaan
  used <- tryCatch(
    lavaan::lavInspect(fit, "case.idx"),
    error = function(e) NULL
  )
  if (is.list(used)) used <- used[[1]]
  if (!is.null(used) && length(used) == nrow(sc)) {
    for (nm in names(sc)) {
      dat[[nm]] <- NA_real_
      dat[[nm]][used] <- as.numeric(sc[[nm]])
    }
  } else if (nrow(sc) == nrow(dat)) {
    for (nm in names(sc)) dat[[nm]] <- as.numeric(sc[[nm]])
  } else {
    stop(
      "Factor-score rows (", nrow(sc), ") do not match analysis data (", nrow(dat), ").",
      call. = FALSE
    )
  }
  # Drop rows without scores (should be rare after listwise prepare)
  score_cols <- names(sc)
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
      meas_est <- data.frame(
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

  state_path <- state_with_latent_as_scores(state, latent_ids)
  list(
    data = dat,
    state = state_path,
    latent_ids = latent_ids,
    measurement_fit = fit,
    measurement_estimates = meas_est,
    measurement_syntax = syntax
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
fit_structural_model <- function(state, data, include_edge_ids = NULL) {
  if (is.null(include_edge_ids)) include_edge_ids <- state$edges$id
  edges <- state$edges[state$edges$id %in% include_edge_ids, , drop = FALSE]
  # DVs from the full graph so null / reduced candidates still get intercept-only equations
  dv_ids <- structural_dv_ids(state, state$edges)
  dat <- prepare_analysis_data(state, data)

  # Two-step whenever the *full* graph has latents (scores from all measurement links)
  if (model_uses_latents(state, state$edges)) {
    attached <- attach_latent_factor_scores(state, dat)
    # Rematerialize interactions / polys with latents treated as observed scores
    dat_path <- prepare_analysis_data(attached$state, attached$data)
    struct_edges <- edges[edges$type != "measurement", , drop = FALSE]
    if (!length(dv_ids) && !nrow(struct_edges)) {
      stop("No structural dependent variables to fit.", call. = FALSE)
    }
    path_res <- fit_path_equations(
      attached$state, dat_path, struct_edges, dv_ids = dv_ids
    )
    # Keep measurement loadings on the canvas; structural coeffs from path stage
    est <- path_res$estimates
    if (nrow(attached$measurement_estimates)) {
      est <- rbind(attached$measurement_estimates, est)
    }
    path_res$estimates <- est
    path_res$syntax <- paste0(
      "# two-step: factor scores (CFA) then path (lm/glm)\n",
      "# measurement\n",
      attached$measurement_syntax,
      "\n# path\n",
      path_res$syntax
    )
    path_res$two_step <- TRUE
    path_res$measurement_fit <- attached$measurement_fit
    path_res$engine <- "path"
    return(path_res)
  }

  if (!nrow(edges)) {
    if (!length(dv_ids)) stop("No dependent variables to fit.", call. = FALSE)
    return(fit_path_equations(state, dat, edges, dv_ids = dv_ids))
  }

  fit_path_equations(state, dat, edges, dv_ids = dv_ids)
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

#' Fit one path equation (lm/glm or lmer/glmer random intercept)
#' @keywords internal
fit_one_path_equation <- function(yname, y_kind, rhs_terms, local_dat,
                                  cluster = NULL) {
  rhs <- if (!length(rhs_terms)) "1" else paste(rhs_terms, collapse = " + ")
  binary <- identical(y_kind, "binary")
  if (!is.null(cluster)) {
    # Ensure grouping factor
    local_dat[[cluster]] <- as.factor(local_dat[[cluster]])
    re <- paste0("(1|", cluster, ")")
    fml <- stats::as.formula(paste(yname, "~", rhs, "+", re))
    if (binary) {
      fit <- lme4::glmer(fml, data = local_dat, family = stats::binomial())
      # Promote to lmerTest-compatible summary when possible
      syntax <- paste0("glmer(binomial): ", yname, " ~ ", rhs, " + ", re)
      eng <- "glmer"
    } else {
      fit <- lmerTest::lmer(fml, data = local_dat)
      syntax <- paste0("lmer: ", yname, " ~ ", rhs, " + ", re)
      eng <- "lmer"
    }
  } else if (binary) {
    fit <- stats::glm(
      stats::as.formula(paste(yname, "~", rhs)),
      data = local_dat,
      family = stats::binomial()
    )
    syntax <- paste0("glm(binomial): ", yname, " ~ ", rhs)
    eng <- "glm"
  } else {
    fit <- stats::lm(
      stats::as.formula(paste(yname, "~", rhs)),
      data = local_dat
    )
    syntax <- paste0("lm: ", yname, " ~ ", rhs)
    eng <- "lm"
  }
  list(fit = fit, syntax = syntax, engine = eng)
}

#' Equation-wise lm / glm / lmer / glmer path models (observed variables only)
#'
#' @param dv_ids Optional DV node ids (from the full graph). Ensures intercept-only
#'   equations when a candidate omits all predictors for a DV.
#' @keywords internal
fit_path_equations <- function(state, dat, edges, dv_ids = NULL) {
  state <- ensure_node_rm_columns(state)
  struct <- edges[edges$type != "measurement", , drop = FALSE]
  if (!"level" %in% names(struct)) {
    struct$level <- NA_character_
  }
  if (is.null(dv_ids) || !length(dv_ids)) {
    if (!nrow(struct)) stop("No structural links to fit.", call. = FALSE)
    dv_ids <- unique(struct$to)
  }

  est_rows <- list()
  syntax_lines <- character()
  fits <- list()
  aic_sum <- 0
  bic_sum <- 0
  npar_sum <- 0L
  row_i <- 0L

  if (!"to_level" %in% names(struct)) struct$to_level <- NA_character_

  for (dv in dv_ids) {
    preds_all <- if (nrow(struct)) {
      struct[struct$to == dv, , drop = FALSE]
    } else {
      struct[0, , drop = FALSE]
    }
    cluster <- dv_cluster_column(state, dv, dat)
    yname <- lavaan_var_name(state, dv)
    if (!yname %in% names(dat)) {
      stop("Dependent variable '", yname, "' not found in analysis data.", call. = FALSE)
    }
    y_kind0 <- classify_variable(dat[[yname]])

    # Response slices: categorical split → one equation per to_level
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

      if (!nrow(preds)) {
        # Intercept-only (null / reduced) — not used for categorical slices
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
        fitted_one <- fit_one_path_equation(yname, y_kind, character(), local_dat, cluster)
        syn <- fitted_one$syntax
        if (!identical(lhs_disp, yname)) {
          syn <- sub(
            paste0("^((?:glm|glmer|lm|lmer)[^:]*): ", yname),
            paste0("\\1: ", lhs_disp),
            syn
          )
        }
        fits[[fit_key]] <- fitted_one$fit
        syntax_lines <- c(syntax_lines, syn)
        aic_sum <- aic_sum + stats::AIC(fitted_one$fit)
        bic_sum <- bic_sum + stats::BIC(fitted_one$fit)
        npar_sum <- npar_sum + length(path_fixed_coefs(fitted_one$fit))
        next
      }

      built <- build_path_terms(state, dat, preds, response_level = tl)
      if (!length(built$term_names)) {
        stop("No predictors for dependent variable '", built$lhs_name, "'.", call. = FALSE)
      }

      fitted_one <- fit_one_path_equation(
        built$yname, built$y_kind, built$term_names, built$data, cluster
      )
      # Tag syntax with response level / transform when display lhs differs
      syn <- fitted_one$syntax
      if (!identical(built$lhs_name, built$yname)) {
        syn <- sub(
          paste0("^((?:glm|glmer|lm|lmer)[^:]*): ", built$yname),
          paste0("\\1: ", built$lhs_name),
          syn
        )
      }
      fit <- fitted_one$fit
      fits[[fit_key]] <- fit
      syntax_lines <- c(syntax_lines, syn)
      aic_sum <- aic_sum + stats::AIC(fit)
      bic_sum <- bic_sum + stats::BIC(fit)
      npar_sum <- npar_sum + length(path_fixed_coefs(fit))

      ct <- path_coef_table(fit)
      bstd <- standardized_term_coefs(fit)
      lhs <- built$lhs_name

      omni <- built$omnibus_groups %||% list()
      for (xn in names(omni)) {
        wt <- joint_wald_test(fit, omni[[xn]])
        row_i <- row_i + 1L
        est_rows[[row_i]] <- estimate_row(
          lhs = lhs,
          op = "~",
          rhs = xn,
          est = NA_real_,
          se = NA_real_,
          pvalue = wt$pvalue,
          est_std = NA_real_,
          df = wt$df,
          level = NA_character_,
          kind = "omnibus"
        )
      }

      for (tn in built$term_names) {
        if (is.null(ct) || !tn %in% rownames(ct)) next
        meta <- built$term_meta[[tn]]
        row_i <- row_i + 1L
        est_rows[[row_i]] <- estimate_row(
          lhs = lhs,
          op = "~",
          rhs = meta$rhs,
          est = as.numeric(ct[tn, 1L]),
          se = as.numeric(ct[tn, 2L]),
          pvalue = path_coef_pvalue(ct, tn),
          est_std = if (tn %in% names(bstd)) as.numeric(bstd[[tn]]) else NA_real_,
          df = 1,
          level = meta$level,
          kind = "effect"
        )
      }
    }
  }

  estimates <- if (length(est_rows)) {
    do.call(rbind, est_rows)
  } else {
    empty_estimates()
  }

  fit_measures <- data.frame(
    measure = c("equations", "npar", "aic", "bic"),
    value = c(as.numeric(length(fits)), as.numeric(npar_sum), aic_sum, bic_sum),
    stringsAsFactors = FALSE
  )

  result <- list(
    estimates = estimates,
    fit = fits,
    fit_measures = fit_measures,
    syntax = paste(c("# path models (equation-wise)", syntax_lines), collapse = "\n"),
    engine = "path",
    ordered = character()
  )
  result$r_squared <- r_squared_from_fitted(result, state)
  result
}
