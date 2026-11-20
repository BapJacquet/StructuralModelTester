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

#' Detect a usable `make` on PATH (needed by rstan on Windows)
#' @keywords internal
stan_make_available <- function() {
  # Sys.which is enough: Rtools puts make on PATH when configured correctly.
  nzchar(Sys.which("make") %||% "")
}

#' Human-readable Windows / toolchain hint for Stan compilation failures
#' @keywords internal
stan_toolchain_hint <- function(err_msg = "") {
  msg <- as.character(err_msg %||% "")
  looks_make <- grepl("make'? not found|make not found|'make'|\"make\"", msg, ignore.case = TRUE) ||
    grepl("sink\\(type = \"output\"\\).*connexion incorrecte|invalid connection", msg, ignore.case = TRUE) ||
    grepl("Compilation ERROR|g\\+\\+.*not found", msg, ignore.case = TRUE)
  if (!isTRUE(.Platform$OS.type == "windows") && !looks_make) {
    return("")
  }
  if (!looks_make && stan_make_available()) return("")
  paste0(
    "\n\nStan could not compile the model (toolchain). On Windows with R 4.4.x:\n",
    "1. Install Rtools 4.4 from https://cran.r-project.org/bin/windows/Rtools/\n",
    "   (match Rtools major.minor to R, e.g. R 4.4 → Rtools44).\n",
    "2. Ensure C:\\\\rtools44\\\\usr\\\\bin is on PATH (or set RTOOLS44_HOME),\n",
    "   then fully restart R / RStudio.\n",
    "3. Check with: Sys.which(\"make\")  and  pkgbuild::check_build_tools(debug = TRUE)\n",
    "4. Prefer cmdstanr + CmdStan when possible:\n",
    "     install.packages(\"cmdstanr\", repos = c(\"https://stan-dev.r-universe.dev\", getOption(\"repos\")))\n",
    "     cmdstanr::install_cmdstan()\n",
    "The secondary 'sink(...): invalid connection / connexion incorrecte' error is usually\n",
    "a follow-on from the failed compile, not a separate Shiny bug."
  )
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
  if (requireNamespace("rstan", quietly = TRUE)) {
    if (identical(.Platform$OS.type, "windows") && !stan_make_available()) {
      if (isTRUE(must)) {
        stop(
          "Bayesian Run found rstan, but 'make' is not on PATH.",
          stan_toolchain_hint("'make' not found"),
          call. = FALSE
        )
      }
      return(NA_character_)
    }
    return("rstan")
  }
  if (isTRUE(must)) {
    stop(
      "Bayesian Run needs the brms package and a Stan backend (cmdstanr or rstan).",
      stan_toolchain_hint("'make' not found"),
      call. = FALSE
    )
  }
  NA_character_
}

#' One name legal for brms (no `__`, no trailing `_`)
#' @keywords internal
brms_safe_name_one <- function(nm) {
  nm <- as.character(nm %||% "")
  if (!nzchar(nm)) return("x")
  # brms reserves `__` in Stan parameter names; also bans trailing `_`
  s <- gsub("__+", "_", nm, perl = TRUE)
  s <- sub("_+$", "", s)
  if (!nzchar(s)) s <- "x"
  s
}

#' Map SMT design names → brms-safe names (unique)
#'
#' @param nms Names to rewrite.
#' @param reserved Other names that must not be reused as targets.
#' @return Named character vector: `names = original`, `values = safe`.
#' @keywords internal
brms_name_map <- function(nms, reserved = character()) {
  nms <- unique(as.character(nms %||% character()))
  nms <- nms[nzchar(nms) & !is.na(nms)]
  if (!length(nms)) return(character())
  reserved <- unique(as.character(reserved %||% character()))
  used <- setdiff(reserved, nms)
  out <- character(length(nms))
  for (i in seq_along(nms)) {
    cand <- brms_safe_name_one(nms[[i]])
    if (cand %in% used) {
      k <- 2L
      while (paste0(cand, "_", k) %in% used) k <- k + 1L
      cand <- paste0(cand, "_", k)
    }
    out[[i]] <- cand
    used <- c(used, cand)
  }
  stats::setNames(out, nms)
}

#' Inverse map: safe → original
#' @keywords internal
brms_name_map_inverse <- function(to_safe) {
  if (!length(to_safe)) return(character())
  stats::setNames(names(to_safe), unname(to_safe))
}

#' Rename data columns / formula terms so brms accepts them
#'
#' SMT uses `Var__Level` for treatment dummies; brms rejects `__`. Returns
#' remapped data, terms, and maps stored on the fit for coef recovery.
#' @keywords internal
brms_prepare_path_input <- function(data, yname, rhs_terms = NULL,
                                    slope_terms = NULL, cluster = NULL,
                                    term_meta = NULL) {
  rhs_terms <- as.character(rhs_terms %||% character())
  slope_terms <- as.character(slope_terms %||% character())
  cluster <- if (is.null(cluster)) NULL else as.character(cluster[[1]] %||% "")
  if (!nzchar(cluster %||% "")) cluster <- NULL
  yname <- as.character(yname[[1]] %||% "")

  need <- unique(c(rhs_terms, slope_terms, yname, cluster))
  need <- need[nzchar(need) & !is.na(need)]
  # Only rewrite names that violate brms rules
  dirty <- need[grepl("__", need, fixed = TRUE) | grepl("_$", need)]
  reserved <- unique(c(names(data), need))
  to_safe <- brms_name_map(dirty, reserved = reserved)
  clean <- setdiff(need, names(to_safe))
  if (length(clean)) {
    to_safe <- c(to_safe, stats::setNames(clean, clean))
  }
  from_safe <- brms_name_map_inverse(to_safe)

  rename_one <- function(nm) {
    nm <- as.character(nm %||% "")
    if (!nzchar(nm)) return(nm)
    if (nm %in% names(to_safe)) unname(to_safe[[nm]]) else nm
  }

  dat <- data
  # Rename columns that change
  for (orig in names(to_safe)) {
    safe <- unname(to_safe[[orig]])
    if (identical(orig, safe)) next
    if (!orig %in% names(dat)) next
    if (safe %in% names(dat) && !identical(orig, safe)) {
      # Should not happen after unique map; drop duplicate target if identical content
      dat[[orig]] <- NULL
      next
    }
    names(dat)[names(dat) == orig] <- safe
  }

  term_meta <- term_meta %||% list()
  meta_safe <- list()
  for (nm in names(term_meta)) {
    meta_safe[[rename_one(nm)]] <- term_meta[[nm]]
  }

  list(
    data = dat,
    yname = rename_one(yname),
    rhs_terms = vapply(rhs_terms, rename_one, character(1), USE.NAMES = FALSE),
    slope_terms = vapply(slope_terms, rename_one, character(1), USE.NAMES = FALSE),
    cluster = if (is.null(cluster)) NULL else rename_one(cluster),
    term_meta = meta_safe,
    to_safe = to_safe,
    from_safe = from_safe
  )
}

#' Attach brms name maps on a fitted object
#' @keywords internal
brms_tag_name_maps <- function(fit, to_safe, from_safe) {
  fit$smt_brms_to_safe <- to_safe %||% character()
  fit$smt_brms_from_safe <- from_safe %||% character()
  fit
}

#' Translate a coef / term name using a fit's brms maps
#' @keywords internal
brms_translate_term <- function(fit, term, direction = c("to_orig", "to_safe")) {
  direction <- match.arg(direction)
  term <- as.character(term %||% "")
  if (!nzchar(term)) return(term)
  map <- if (identical(direction, "to_orig")) {
    fit$smt_brms_from_safe
  } else {
    fit$smt_brms_to_safe
  }
  if (is.null(map) || !length(map)) return(term)
  if (term %in% names(map)) unname(map[[term]]) else term
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
  structure <- as.character(args$structure %||% "independent")[[1]]
  if (!structure %in% c("independent", "joint")) structure <- "independent"
  list(
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    ci = ci,
    rope_k = rope_k,
    structure = structure,
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
fill_brms_slope_priors <- function(prior_df, state, preds, term_meta,
                                   preds_by_resp = NULL,
                                   term_meta_by_resp = NULL) {
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
    resp <- if ("resp" %in% names(prior_df)) {
      as.character(prior_df$resp[[i]] %||% "")
    } else {
      ""
    }
    if (is.na(resp)) resp <- ""
    if (nzchar(resp) && !is.null(term_meta_by_resp)) {
      meta <- term_meta_by_resp[[resp]][[coef]]
      preds_i <- preds_by_resp[[resp]]
    } else {
      meta <- term_meta[[coef]]
      preds_i <- preds
    }
    sp <- if (is.null(meta)) {
      list(mean = 0, sd = 1)
    } else {
      edge_normal_prior(state, preds_i, meta)
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
      stop(
        "brms error: ", e$message,
        stan_toolchain_hint(e$message),
        call. = FALSE
      )
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

#' Whether joint equations include paths among outcomes (mediation / chain)
#'
#' If one response appears as a predictor of another, freely estimating residual
#' correlations among those outcomes is (near-)nonidentified with the path —
#' the same issue as freeing `M ~~ Y` alongside `Y ~ M` in lavaan.
#' @keywords internal
joint_outcomes_predict_each_other <- function(jobs) {
  if (!length(jobs) || length(jobs) < 2L) return(FALSE)
  ynames <- unique(vapply(jobs, function(j) as.character(j$yname %||% ""), character(1)))
  ynames <- ynames[!is.na(ynames) & nzchar(ynames)]
  key_roots <- unique(vapply(jobs, function(j) {
    k <- as.character(j$key %||% j$yname %||% "")
    sub("::.*$", "", k)
  }, character(1)))
  key_roots <- key_roots[!is.na(key_roots) & nzchar(key_roots)]
  for (j in jobs) {
    terms <- as.character(j$term_names %||% character())
    if (length(terms) && any(terms %in% ynames)) return(TRUE)
    preds <- j$preds
    if (is.data.frame(preds) && nrow(preds) && "from" %in% names(preds)) {
      fr <- as.character(preds$from)
      if (any(fr %in% key_roots)) return(TRUE)
    }
  }
  FALSE
}

#' Use residual correlations in a joint brms path fit?
#'
#' Enabled only when outcomes are "siblings" (no directed path among them).
#' Mediation / causal chains force `set_rescor(FALSE)` automatically.
#' @keywords internal
joint_brms_want_rescor <- function(jobs) {
  !isTRUE(joint_outcomes_predict_each_other(jobs))
}

#' Why a joint multivariate brms path fit is unavailable
#'
#' @return `NULL` if joint is feasible, else a short reason string.
#' @keywords internal
joint_brms_block_reason <- function(jobs) {
  if (!length(jobs) || length(jobs) < 2L) {
    return("need at least two outcome equations")
  }
  kinds <- vapply(jobs, function(j) as.character(j$y_kind %||% ""), character(1))
  if (any(!kinds %in% c("continuous", ""))) {
    return("joint mode currently requires continuous (gaussian) outcomes only")
  }
  if (any(vapply(jobs, function(j) length(j$slope_terms %||% character()) > 0, logical(1)))) {
    return("joint mode does not yet support random slopes")
  }
  clusters <- lapply(jobs, function(j) j$cluster)
  has_cl <- vapply(clusters, function(c) !is.null(c) && nzchar(as.character(c %||% "")), logical(1))
  if (any(has_cl) && !all(has_cl)) {
    return("joint mode needs the same clustering on every equation (or none)")
  }
  if (all(has_cl)) {
    cl_nm <- unique(vapply(clusters, as.character, character(1)))
    if (length(cl_nm) != 1L) {
      return("joint mode needs one shared cluster column across equations")
    }
  }
  ynames <- vapply(jobs, function(j) j$yname, character(1))
  if (anyDuplicated(ynames)) {
    return("joint mode cannot combine duplicate response names")
  }
  NULL
}

#' Tag a brmsfit view with the response used for path helpers
#' @keywords internal
tag_brms_equation_view <- function(fit, response, ci, rope_k, term_priors,
                                   structure = "independent") {
  resp <- as.character(response %||% "")
  if (length(resp) != 1L || is.na(resp) || !nzchar(resp)) resp <- ""
  fit$smt_response <- if (nzchar(resp)) resp else NULL
  fit$smt_ci <- ci
  fit$smt_rope_k <- rope_k
  fit$smt_term_priors <- term_priors %||% list()
  fit$smt_structure <- structure
  fit
}

#' Fit one multivariate gaussian brms model for several path equations
#' @keywords internal
fit_joint_brms_path <- function(jobs, state, brms_args = NULL) {
  args <- normalize_brms_args(brms_args)
  backend <- brms_stan_backend(must = TRUE)
  reason <- joint_brms_block_reason(jobs)
  if (!is.null(reason)) {
    return(list(ok = FALSE, reason = reason, fit = NULL, syntax = NULL))
  }

  # Shared analysis frame: union of columns (listwise deletion across outcomes)
  dat <- jobs[[1]]$data
  for (j in jobs[-1L]) {
    extra <- setdiff(names(j$data), names(dat))
    if (length(extra)) dat <- cbind(dat, j$data[extra])
  }
  ynames <- vapply(jobs, function(j) j$yname, character(1))
  keep <- stats::complete.cases(dat[, ynames, drop = FALSE])
  dat <- dat[keep, , drop = FALSE]
  if (!nrow(dat)) {
    return(list(ok = FALSE, reason = "no complete cases across joint outcomes",
                fit = NULL, syntax = NULL))
  }

  # Sanitize dummy `__` names once across the shared frame / all equations
  all_terms <- unique(unlist(lapply(jobs, function(j) j$term_names), use.names = FALSE))
  all_slopes <- unique(unlist(lapply(jobs, function(j) j$slope_terms), use.names = FALSE))
  all_clusters <- unique(vapply(jobs, function(j) {
    cl <- as.character(j$cluster %||% "")
    if (!nzchar(cl)) "" else cl
  }, character(1)))
  all_clusters <- all_clusters[nzchar(all_clusters)]
  prep0 <- brms_prepare_path_input(
    data = dat,
    yname = ynames[[1]],
    rhs_terms = all_terms,
    slope_terms = all_slopes,
    cluster = if (length(all_clusters)) all_clusters[[1]] else NULL,
    term_meta = list()
  )
  dat <- prep0$data
  to_safe <- prep0$to_safe
  from_safe <- prep0$from_safe
  rename_nm <- function(nm) {
    nm <- as.character(nm %||% "")
    if (nzchar(nm) && nm %in% names(to_safe)) unname(to_safe[[nm]]) else nm
  }

  forms <- list()
  syntax_lines <- character()
  term_meta_by_resp <- list()
  preds_by_resp <- list()
  for (j in jobs) {
    y_safe <- rename_nm(j$yname)
    terms_safe <- vapply(j$term_names, rename_nm, character(1), USE.NAMES = FALSE)
    rhs <- if (!length(terms_safe)) "1" else paste(terms_safe, collapse = " + ")
    re <- NULL
    cl <- as.character(j$cluster %||% "")
    if (nzchar(cl)) {
      cl_safe <- rename_nm(cl)
      dat[[cl_safe]] <- as.factor(dat[[cl_safe]])
      # Joint path currently forbids random slopes; intercept-only RE
      re <- rm_re_term(cl_safe, NULL)
      fml <- stats::as.formula(paste(y_safe, "~", rhs, "+", re))
    } else {
      fml <- stats::as.formula(paste(y_safe, "~", rhs))
    }
    forms[[length(forms) + 1L]] <- brms::bf(fml)
    # Syntax keeps SMT dummy names
    rhs_orig <- if (!length(j$term_names)) "1" else paste(j$term_names, collapse = " + ")
    re_orig <- if (nzchar(cl)) rm_re_term(cl, NULL) else NULL
    rhs_txt <- if (!is.null(re_orig)) paste(rhs_orig, re_orig, sep = " + ") else rhs_orig
    syntax_lines <- c(
      syntax_lines,
      paste0("  ", j$yname, " ~ ", rhs_txt)
    )
    meta <- j$term_meta %||% list()
    meta_safe <- list()
    for (nm in names(meta)) meta_safe[[rename_nm(nm)]] <- meta[[nm]]
    term_meta_by_resp[[y_safe]] <- meta_safe
    preds_by_resp[[y_safe]] <- j$preds
  }
  mv_formula <- forms[[1]]
  for (i in seq_along(forms)[-1L]) {
    mv_formula <- mv_formula + forms[[i]]
  }
  # Residual correlations among outcomes are only identified when no outcome
  # predicts another (sibling DVs). Mediation / chains → rescor off.
  use_rescor <- joint_brms_want_rescor(jobs)
  mv_formula <- mv_formula + brms::set_rescor(use_rescor)

  prior_df <- brms::get_prior(mv_formula, data = dat, family = stats::gaussian())
  prior_df <- fill_brms_slope_priors(
    prior_df, state, preds = NULL, term_meta = list(),
    preds_by_resp = preds_by_resp,
    term_meta_by_resp = term_meta_by_resp
  )
  fit <- tryCatch(
    brms::brm(
      formula = mv_formula,
      data = dat,
      family = stats::gaussian(),
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
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "brms joint error: ", fit$message,
        stan_toolchain_hint(fit$message)
      ),
      fit = NULL,
      syntax = NULL
    ))
  }
  fit <- brms_tag_name_maps(fit, to_safe, from_safe)
  fit$smt_rescor <- use_rescor

  prior_bits <- character()
  if (!is.null(prior_df) && nrow(prior_df)) {
    for (i in seq_len(nrow(prior_df))) {
      if (!identical(as.character(prior_df$class[[i]]), "b")) next
      coef <- as.character(prior_df$coef[[i]] %||% "")
      resp <- as.character(prior_df$resp[[i]] %||% "")
      if (is.na(coef) || !nzchar(coef)) next
      lab <- if (nzchar(resp)) paste0(resp, ":", coef) else coef
      prior_bits <- c(prior_bits, paste0(lab, " ~ ", prior_df$prior[[i]]))
    }
  }
  joint_tag <- if (isTRUE(use_rescor)) {
    "joint + rescor"
  } else {
    "joint · rescor off (path among outcomes)"
  }
  syntax <- paste0(
    "brms(gaussian, ", joint_tag, "):\n",
    paste(syntax_lines, collapse = "\n")
  )
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
  list(ok = TRUE, reason = NULL, fit = fit, syntax = syntax, data = dat)
}

#' Fit the structural model and mark links by HDI + ROPE
#'
#' Same graph and data contract as [fit_pvalue_model()]. `ci` is the HDI mass
#' (default 0.95). The ROPE for each slope is
#' `[-rope_k * prior_sd, +rope_k * prior_sd]` from the link's Expected SD.
#' Exploratory links are excluded when the HDI is not entirely outside the
#' ROPE. Confirmatory links stay in the model and are muted in that case.
#'
#' @param structure `"independent"` (one Stan model per equation, default) or
#'   `"joint"` (single multivariate gaussian model; residual correlations when
#'   outcomes do not predict each other — mediation / chains turn rescor off
#'   automatically; falls back to independent if equations are incompatible).
#' @param chains,iter,warmup,seed MCMC settings passed to brms. One core is
#'   used so sampling stays inside the Shiny process.
#' @export
fit_bayes_model <- function(state, data, ci = 0.95, rope_k = 0.1,
                            structure = c("independent", "joint"),
                            mark_nonsig = TRUE,
                            chains = 2L, iter = 2000L, warmup = 1000L, seed = 1L,
                            latent_score_method = "own") {
  structure <- match.arg(structure)
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
    ci = ci, rope_k = rope_k, structure = structure
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
    bayes_structure = fitted$bayes_structure %||% brms_args$structure,
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
