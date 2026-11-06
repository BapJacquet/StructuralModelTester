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

#' Reference coding for a variable (base + non-reference levels)
#'
#' Categorical (3+ levels): base = first factor level / sorted unique.
#' Binary/continuous: no per-level ports (`levels` empty).
#' @keywords internal
variable_coding <- function(x) {
  kind <- classify_variable(x)
  if (!identical(kind, "categorical")) {
    return(list(kind = kind, base = NA_character_, levels = character()))
  }
  if (is.factor(x)) {
    levs <- levels(droplevels(x[!is.na(x)]))
  } else {
    levs <- sort(unique(as.character(stats::na.omit(x))))
  }
  if (length(levs) < 2L) {
    return(list(kind = "continuous", base = NA_character_, levels = character()))
  }
  list(kind = "categorical", base = levs[[1]], levels = levs[-1])
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

#' Standardized coefficients for lm / glm terms (excl. intercept)
#'
#' lm: b * sd(x) / sd(y).
#' glm: b * sd(x) / sqrt(var(η) + π²/3) (latent-y style, logit/probit-friendly).
#' @keywords internal
standardized_term_coefs <- function(fit) {
  cf <- stats::coef(fit)
  out <- setNames(rep(NA_real_, length(cf)), names(cf))
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
  cf <- stats::coef(fit)
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
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
    coding <- variable_coding(x)
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
#' @return NULL if not a DV; otherwise "lm", "glm", "lavaan", or "unsupported"
#' @keywords internal
dv_model_engine <- function(state, data, node_id, edges = state$edges) {
  dvs <- structural_dv_ids(state, edges)
  if (!node_id %in% dvs) return(NULL)
  if (model_uses_latents(state, edges)) return("lavaan")

  col <- state$nodes$observed_column[match(node_id, state$nodes$id)]
  if (is.na(col) || !nzchar(col)) return("lavaan")
  if (is.null(data) || !col %in% names(data)) return("lm")
  kind <- classify_variable(data[[col]])
  if (identical(kind, "binary")) return("glm")
  if (identical(kind, "categorical")) return("unsupported")
  "lm"
}

#' Display label for canvas badge
#' @keywords internal
dv_model_badge <- function(engine) {
  switch(engine,
    lm = "lm",
    glm = "glm · binomial",
    lavaan = "lavaan",
    unsupported = "categorical DV (unsupported)",
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

#' Fit the structural model with a context-aware engine
#'
#' Observed-only path models use equation-wise lm / glm (binomial) based on
#' each dependent variable's type. Multi-level categorical predictors are kept
#' as factors (k−1 contrasts + omnibus test). Models with latents use lavaan.
#'
#' @keywords internal
fit_structural_model <- function(state, data, include_edge_ids = NULL) {
  if (is.null(include_edge_ids)) include_edge_ids <- state$edges$id
  edges <- state$edges[state$edges$id %in% include_edge_ids, , drop = FALSE]
  # DVs from the full graph so null / reduced candidates still get intercept-only equations
  dv_ids <- structural_dv_ids(state, state$edges)
  dat <- prepare_analysis_data(state, data)

  if (!nrow(edges)) {
    if (model_uses_latents(state, state$edges)) {
      stop(
        "Cannot fit a null model while the graph has latent/measurement structure. ",
        "Keep measurement links fixed or include them in the candidate.",
        call. = FALSE
      )
    }
    if (!length(dv_ids)) stop("No dependent variables to fit.", call. = FALSE)
    return(fit_path_equations(state, dat, edges, dv_ids = dv_ids))
  }

  if (model_uses_latents(state, edges)) {
    validate_model_state(state, edges)
    fit_lavaan_model(state, dat, edges)
  } else {
    # Reduced path candidates may omit predictors; skip full latent validation
    if (any(edges$type == "measurement")) validate_model_state(state, edges)
    fit_path_equations(state, dat, edges, dv_ids = dv_ids)
  }
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
    for (dv in names(fitted$fit)) {
      slot <- r_squared_one_fit(fitted$fit[[dv]])
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

#' Build design columns for one DV equation from structural edges
#' @keywords internal
build_path_terms <- function(state, dat, preds) {
  state <- ensure_edge_to_port(state)
  y_id <- preds$to[1]
  yname <- lavaan_var_name(state, y_id)
  if (!yname %in% names(dat)) {
    stop("Dependent variable '", yname, "' not found in analysis data.", call. = FALSE)
  }
  y_kind <- classify_variable(dat[[yname]])
  if (identical(y_kind, "categorical")) {
    stop(
      "Dependent variable '", yname, "' is multi-level categorical. ",
      "Only binary (glm) or continuous (lm) outcomes are supported in path mode.",
      call. = FALSE
    )
  }

  local_dat <- dat
  if (identical(y_kind, "binary")) {
    local_dat[[yname]] <- as_binary01(local_dat[[yname]])
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
    coding <- variable_coding(dat_xn)
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
    y_kind = y_kind,
    data = local_dat,
    term_names = unique(term_names),
    term_meta = term_meta,
    omnibus_groups = omnibus_groups
  )
}

#' Equation-wise lm / glm path models (observed variables only)
#'
#' @param dv_ids Optional DV node ids (from the full graph). Ensures intercept-only
#'   equations when a candidate omits all predictors for a DV.
#' @keywords internal
fit_path_equations <- function(state, dat, edges, dv_ids = NULL) {
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

  for (dv in dv_ids) {
    preds <- if (nrow(struct)) {
      struct[struct$to == dv, , drop = FALSE]
    } else {
      struct
    }

    if (!nrow(preds)) {
      # Intercept-only (null / reduced) equation for this DV
      yname <- lavaan_var_name(state, dv)
      if (!yname %in% names(dat)) {
        stop("Dependent variable '", yname, "' not found in analysis data.", call. = FALSE)
      }
      y_kind <- classify_variable(dat[[yname]])
      if (identical(y_kind, "categorical")) {
        stop(
          "Dependent variable '", yname, "' is multi-level categorical. ",
          "Only binary (glm) or continuous (lm) outcomes are supported in path mode.",
          call. = FALSE
        )
      }
      local_dat <- dat
      if (identical(y_kind, "binary")) {
        local_dat[[yname]] <- as_binary01(local_dat[[yname]])
        fit <- stats::glm(
          stats::as.formula(paste(yname, "~ 1")),
          data = local_dat,
          family = stats::binomial()
        )
        syntax_lines <- c(syntax_lines, paste0("glm(binomial): ", yname, " ~ 1"))
      } else {
        fit <- stats::lm(stats::as.formula(paste(yname, "~ 1")), data = local_dat)
        syntax_lines <- c(syntax_lines, paste0("lm: ", yname, " ~ 1"))
      }
      fits[[dv]] <- fit
      aic_sum <- aic_sum + stats::AIC(fit)
      bic_sum <- bic_sum + stats::BIC(fit)
      npar_sum <- npar_sum + length(stats::coef(fit))
      next
    }

    built <- build_path_terms(state, dat, preds)
    if (!length(built$term_names)) {
      stop("No predictors for dependent variable '", built$yname, "'.", call. = FALSE)
    }

    eng <- if (identical(built$y_kind, "binary")) "glm" else "lm"
    fml <- stats::as.formula(
      paste(built$yname, "~", paste(built$term_names, collapse = " + "))
    )

    if (identical(eng, "glm")) {
      fit <- stats::glm(fml, data = built$data, family = stats::binomial())
      syntax_lines <- c(
        syntax_lines,
        paste0("glm(binomial): ", built$yname, " ~ ", paste(built$term_names, collapse = " + "))
      )
    } else {
      fit <- stats::lm(fml, data = built$data)
      syntax_lines <- c(
        syntax_lines,
        paste0("lm: ", built$yname, " ~ ", paste(built$term_names, collapse = " + "))
      )
    }

    fits[[dv]] <- fit
    aic_sum <- aic_sum + stats::AIC(fit)
    bic_sum <- bic_sum + stats::BIC(fit)
    npar_sum <- npar_sum + length(stats::coef(fit))

    ct <- stats::coef(summary(fit))
    bstd <- standardized_term_coefs(fit)

    # Omnibus joint tests for categorical blocks
    omni <- built$omnibus_groups %||% list()
    for (xn in names(omni)) {
      wt <- joint_wald_test(fit, omni[[xn]])
      row_i <- row_i + 1L
      est_rows[[row_i]] <- estimate_row(
        lhs = built$yname,
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
      if (!tn %in% rownames(ct)) next
      meta <- built$term_meta[[tn]]
      row_i <- row_i + 1L
      est_rows[[row_i]] <- estimate_row(
        lhs = built$yname,
        op = "~",
        rhs = meta$rhs,
        est = as.numeric(ct[tn, 1L]),
        se = as.numeric(ct[tn, 2L]),
        pvalue = as.numeric(ct[tn, ncol(ct)]),
        est_std = if (tn %in% names(bstd)) as.numeric(bstd[[tn]]) else NA_real_,
        df = 1,
        level = meta$level,
        kind = "effect"
      )
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
