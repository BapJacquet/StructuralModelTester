#' Fit structural model under a p-value framework
#'
#' Observed-only path models use equation-wise lm / glm (binomial) based on
#' each dependent variable's type. Models with latents use a two-step procedure
#' (CFA factor scores, then the same path engine).
#'
#' @param state Model state.
#' @param data Data frame.
#' @param alpha Significance level for marking exploratory edges / confirmatory tests.
#' @param mark_nonsig If TRUE, exploratory edges with p > alpha are marked excluded.
#' @param conf_adjust Multiplicity adjustment applied to confirmatory edge p-values
#'   (`none`, `bonferroni`, `holm`, `BH`). Adjusted p-values are written onto
#'   confirmatory edges (and matching estimate rows) for display and detection.
#' @param uncertainty Post-selection uncertainty: "none", "bootstrap", or "plausible".
#' @param uncertainty_draws Number of bootstrap/PV draws (defaults by method).
#' @param uncertainty_progress Optional function(progress_i, total) callback.
#' @return A list with updated `state`, `fit`, `estimates`, `fit_measures`, and `syntax`.
#' @export
fit_pvalue_model <- function(state, data, alpha = 0.05, mark_nonsig = TRUE,
                             conf_adjust = c("holm", "none", "bonferroni", "BH"),
                             uncertainty = c("none", "bootstrap", "plausible"),
                             uncertainty_draws = NULL,
                             uncertainty_progress = NULL,
                             latent_score_method = "own") {
  uncertainty <- match.arg(uncertainty)
  conf_adjust <- match.arg(conf_adjust)
  latent_score_method <- normalize_latent_score_method(latent_score_method, state)
  edge_ids <- analyzable_edge_ids(state)
  # Still mark definitional wires as included (no coefficients)
  all_display_ids <- unique(c(edge_ids, state$edges$id[
    state$edges$type == "definitional" |
      vapply(state$edges$to, function(id) is_interaction_node(state, id), logical(1))
  ]))
  fitted <- fit_structural_model(
    state, data,
    include_edge_ids = edge_ids,
    latent_score_method = latent_score_method
  )
  fitted <- apply_postselection_uncertainty(
    state, data, edge_ids, fitted,
    method = uncertainty,
    draws = uncertainty_draws,
    progress = uncertainty_progress,
    latent_score_method = latent_score_method
  )
  state2 <- apply_estimates_to_edges(state, fitted$estimates, included_edge_ids = all_display_ids)
  state2 <- apply_r_squared_to_nodes(state2, fitted$r_squared)
  state2 <- apply_unidimensionality_to_nodes(
    state2, fitted$measurement_unidimensionality
  )
  adj <- apply_confirmatory_p_adjust(state2, fitted$estimates, method = conf_adjust)
  state2 <- adj$state
  fitted$estimates <- adj$estimates
  # Canvas mutes confirmatory links with p > α (gray); exploratory may also be excluded.
  state2$pvalue_alpha <- as.numeric(alpha)

  if (mark_nonsig && nrow(state2$edges)) {
    for (i in seq_len(nrow(state2$edges))) {
      if (state2$edges$type[i] != "exploratory") next
      pv <- state2$edges$pvalue[i]
      if (!is.na(pv) && pv > alpha) {
        state2$edges$status[i] <- "excluded"
        est_val <- state2$edges$estimate[i]
        lv <- if ("level" %in% names(state2$edges)) state2$edges$level[i] else NA_character_
        prefix <- "E"
        if (!is.na(lv) && nzchar(lv)) prefix <- paste0(prefix, "·", lv)
        state2$edges$label[i] <- if (!is.na(est_val)) {
          sprintf("%s %.2f ✕", prefix, est_val)
        } else {
          paste0(prefix, " ✕")
        }
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
    uncertainty = fitted$uncertainty %||% list(method = "none"),
    conf_adjust = conf_adjust,
    two_step = isTRUE(fitted$two_step),
    measurement_fit = fitted$measurement_fit,
    measurement_score_weights = fitted$measurement_score_weights,
    measurement_score_fits = fitted$measurement_score_fits,
    measurement_cronbach_alpha = fitted$measurement_cronbach_alpha,
    measurement_unidimensionality = fitted$measurement_unidimensionality,
    latent_score_method = fitted$latent_score_method %||% latent_score_method
  )
}

#' Adjust confirmatory edge p-values for multiplicity
#' @keywords internal
apply_confirmatory_p_adjust <- function(state, estimates, method = "none") {
  method <- as.character(method %||% "none")
  if (!method %in% c("bonferroni", "holm", "BH")) {
    return(list(state = state, estimates = estimates))
  }
  if (is.null(state) || !nrow(state$edges)) {
    return(list(state = state, estimates = estimates))
  }
  conf_idx <- which(
    state$edges$type == "confirmatory" &
      state$edges$id %in% analyzable_edge_ids(state, state$edges$id)
  )
  if (!length(conf_idx)) return(list(state = state, estimates = estimates))
  raw <- as.numeric(state$edges$pvalue[conf_idx])
  adj <- stats::p.adjust(raw, method = method)
  state$edges$pvalue[conf_idx] <- adj

  # Mirror onto estimate rows matched by edge endpoints when possible
  if (!is.null(estimates) && nrow(estimates) && "pvalue" %in% names(estimates)) {
    for (j in seq_along(conf_idx)) {
      i <- conf_idx[[j]]
      eid <- state$edges$id[[i]]
      from <- state$edges$from[[i]]
      to <- state$edges$to[[i]]
      from_nm <- tryCatch(lavaan_var_name(state, from), error = function(e) NA_character_)
      to_nm <- tryCatch(lavaan_var_name(state, to), error = function(e) NA_character_)
      if (is.na(from_nm) || is.na(to_nm)) next
      hit <- which(
        estimates$op == "~" &
          estimates$rhs == from_nm &
          estimates$lhs == to_nm
      )
      lv <- if ("level" %in% names(state$edges)) state$edges$level[[i]] else NA_character_
      if (!is.na(lv) && nzchar(lv) && "level" %in% names(estimates)) {
        hit2 <- hit[!is.na(estimates$level[hit]) & estimates$level[hit] == lv]
        if (length(hit2)) hit <- hit2
      }
      if (length(hit)) estimates$pvalue[hit] <- adj[[j]]
    }
  }
  list(state = state, estimates = estimates)
}

#' @keywords internal
extract_fit_measures <- function(fit) {
  want <- c("npar", "df", "pvalue", "aic", "bic")
  fm <- tryCatch(lavaan::fitMeasures(fit, want), error = function(e) NULL)
  if (is.null(fm)) {
    return(data.frame(measure = character(), value = numeric(), stringsAsFactors = FALSE))
  }
  vals <- as.numeric(fm)
  names(vals) <- names(fm)
  # WLSMV / categorical often has no AIC/BIC
  keep <- !is.na(vals)
  data.frame(
    measure = names(vals)[keep],
    value = vals[keep],
    stringsAsFactors = FALSE
  )
}

#' Shortest unique prefixes for a set of labels (first letters only as needed)
#' @keywords internal
shortest_unique_prefixes <- function(labels) {
  labels <- as.character(labels)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  labs <- unique(labels)
  if (!length(labs)) return(character())
  n <- length(labs)
  abbr <- labs
  for (i in seq_len(n)) {
    nm <- labs[[i]]
    nchar_nm <- nchar(nm, type = "chars")
    if (!nchar_nm) next
    for (L in seq_len(nchar_nm)) {
      cand <- substr(nm, 1L, L)
      clash <- FALSE
      for (j in seq_len(n)[-i]) {
        other <- labs[[j]]
        opref <- substr(other, 1L, min(L, nchar(other, type = "chars")))
        if (identical(opref, cand)) {
          clash <- TRUE
          break
        }
      }
      if (!clash) {
        abbr[[i]] <- cand
        break
      }
    }
  }
  stats::setNames(abbr, labs)
}

#' Base identifier for equation abbreviation (strip poly / dummy suffixes)
#' @keywords internal
equation_term_base <- function(name) {
  name <- as.character(name)
  if (!nzchar(name) || identical(name, "(Intercept)")) return(name)
  if (grepl("__", name, fixed = TRUE)) {
    return(strsplit(name, "__", fixed = TRUE)[[1]][[1]])
  }
  if (grepl("_p[0-9]+$", name)) {
    return(sub("_p[0-9]+$", "", name))
  }
  name
}

#' Core variable name inside an LHS display label (strip transforms / [level])
#' @keywords internal
equation_lhs_core <- function(yname) {
  y <- as.character(yname)
  if (!nzchar(y)) return(y)
  m <- regexec("^(log10|log1p|log|sqrt)\\((.*)\\)$", y, perl = TRUE)
  hit <- regmatches(y, m)[[1]]
  if (length(hit) >= 3L) y <- hit[[3]]
  sub("\\[[^\\]]*\\]$", "", y, perl = TRUE)
}

#' Apply unique-prefix abbreviation inside an LHS display label
#' @keywords internal
abbreviate_lhs_display <- function(yname, abbrev_map = NULL) {
  y <- as.character(yname)
  if (is.null(abbrev_map) || !length(abbrev_map) || !nzchar(y)) return(y)
  core <- equation_lhs_core(y)
  abbr <- if (core %in% names(abbrev_map)) {
    unname(abbrev_map[[core]])
  } else {
    core
  }
  if (identical(y, core)) return(abbr)
  # Preserve transform / [level] wrappers around the abbreviated core
  if (startsWith(y, paste0(core, "[")) && endsWith(y, "]")) {
    return(paste0(abbr, substring(y, nchar(core) + 1L)))
  }
  m <- regexec("^(log10|log1p|log|sqrt)\\((.*)\\)$", y, perl = TRUE)
  hit <- regmatches(y, m)[[1]]
  if (length(hit) >= 3L) {
    inner <- abbreviate_lhs_display(hit[[3]], abbrev_map)
    return(paste0(hit[[2]], "(", inner, ")"))
  }
  # Fallback: replace first occurrence of core
  sub(core, abbr, y, fixed = TRUE)
}

#' Shortest unique prefixes for all equation variable labels in a fit
#'
#' Interaction display labels (`Voice×Surprising`) are excluded: they embed
#' factor names and would otherwise block those factors from abbreviating.
#' @keywords internal
equation_abbrev_map <- function(result = NULL, state = NULL) {
  labs <- character()
  if (!is.null(state) && NROW(state$nodes)) {
    for (i in seq_len(nrow(state$nodes))) {
      id <- state$nodes$id[[i]]
      if (is_interaction_node(state, id)) {
        labs <- c(labs, tryCatch(lavaan_var_name(state, id), error = function(e) id))
        next
      }
      labs <- c(labs, node_display_name(state, id))
      labs <- c(labs, tryCatch(lavaan_var_name(state, id), error = function(e) NA_character_))
    }
  }
  est <- if (!is.null(result)) result$estimates else NULL
  if (is.null(est) && !is.null(result)) est <- result$best_estimates
  if (!is.null(est) && NROW(est)) {
    if ("rhs" %in% names(est)) {
      labs <- c(labs, vapply(as.character(est$rhs), equation_term_base, character(1)))
    }
    if ("lhs" %in% names(est)) {
      labs <- c(labs, vapply(as.character(est$lhs), equation_lhs_core, character(1)))
    }
  }
  labs <- labs[!is.na(labs) & nzchar(labs) & labs != "(Intercept)"]
  # Drop compound A×B labels if any leaked in from estimates / legacy state
  labs <- labs[!grepl("\u00D7", labs, fixed = TRUE)]
  labs <- unique(labs)
  shortest_unique_prefixes(labs)
}

#' Apply unique-prefix map to a term base name
#' @keywords internal
abbreviate_term_base <- function(name, abbrev_map = NULL) {
  if (is.null(abbrev_map) || !length(abbrev_map)) return(as.character(name))
  base <- equation_term_base(name)
  if (base %in% names(abbrev_map)) return(unname(abbrev_map[[base]]))
  if (name %in% names(abbrev_map)) return(unname(abbrev_map[[name]]))
  as.character(name)
}

#' Resolve estimate/coef name to an interaction node id, if any
#' @keywords internal
resolve_interaction_node_id <- function(state, name) {
  if (is.null(state) || !NROW(state$nodes)) return(NA_character_)
  base <- equation_term_base(name)
  if (!nzchar(base) || identical(base, "(Intercept)")) return(NA_character_)
  for (i in seq_len(nrow(state$nodes))) {
    id <- state$nodes$id[[i]]
    if (!is_interaction_node(state, id)) next
    ix_nm <- tryCatch(lavaan_var_name(state, id), error = function(e) id)
    if (identical(base, id) || identical(base, ix_nm) ||
          identical(base, sanitize_id(id)) || identical(base, sanitize_id(ix_nm))) {
      return(id)
    }
  }
  NA_character_
}

#' Escape plain text for use inside LaTeX math
#' @keywords internal
latex_escape_math <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("([&#$%_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x
}

#' Identifier in math mode (single letter italic; else \\mathrm)
#' @keywords internal
latex_var <- function(name) {
  name <- as.character(name %||% "")
  if (!nzchar(name)) return("")
  esc <- latex_escape_math(name)
  if (nchar(name, type = "chars") == 1L && grepl("^[A-Za-z]$", name)) {
    return(esc)
  }
  sprintf("\\mathrm{%s}", esc)
}

#' LHS label → LaTeX (preserves log/sqrt / [level] wrappers)
#' @keywords internal
latex_lhs_display <- function(yname) {
  y <- as.character(yname %||% "")
  if (!nzchar(y)) return("")
  if (grepl("\u00D7", y, fixed = TRUE)) {
    parts <- strsplit(y, "\u00D7", fixed = TRUE)[[1]]
    return(paste(
      vapply(parts, latex_lhs_display, character(1)),
      collapse = " \\times "
    ))
  }
  m <- regexec("^(log10|log1p|log|sqrt)\\((.*)\\)$", y, perl = TRUE)
  hit <- regmatches(y, m)[[1]]
  if (length(hit) >= 3L) {
    inner <- latex_lhs_display(hit[[3]])
    return(switch(
      hit[[2]],
      log10 = sprintf("\\log_{10}(%s)", inner),
      log1p = sprintf("\\operatorname{log1p}(%s)", inner),
      log = sprintf("\\log(%s)", inner),
      sqrt = sprintf("\\sqrt{%s}", inner),
      sprintf("\\mathrm{%s}(%s)", latex_escape_math(hit[[2]]), inner)
    ))
  }
  if (grepl("\\[[^\\]]+\\]$", y, perl = TRUE)) {
    core <- sub("\\[[^\\]]*\\]$", "", y, perl = TRUE)
    lv <- sub("^.*\\[([^\\]]*)\\]$", "\\1", y, perl = TRUE)
    return(sprintf("%s[\\mathrm{%s}]", latex_var(core), latex_escape_math(lv)))
  }
  latex_var(y)
}

#' Wrap a math string for MathJax (inline)
#' @keywords internal
smt_eq_math_line <- function(tex) {
  sprintf("<div class=\"smt-eq-line\">\\(%s\\)</div>", tex)
}

#' Abbreviated A×B plain label for tables (not MathJax)
#' @keywords internal
pretty_interaction_term_html <- function(state, ix_id, level = NULL,
                                         abbrev_map = NULL) {
  fac <- interaction_factors(state, ix_id)
  a_nm <- if (!is.na(fac$a) && nzchar(fac$a)) {
    node_display_name(state, fac$a)
  } else {
    "?"
  }
  b_nm <- if (!is.na(fac$b) && nzchar(fac$b)) {
    node_display_name(state, fac$b)
  } else {
    "?"
  }
  a_abbr <- abbreviate_term_base(a_nm, abbrev_map)
  b_abbr <- abbreviate_term_base(b_nm, abbrev_map)
  lv_a <- fac$level_a
  lv_b <- fac$level_b
  if (!is.null(level) && !is.na(level) && nzchar(as.character(level)) &&
        !is_poly_level(level)) {
    lv <- as.character(level)
    if (grepl("\u00D7", lv, fixed = TRUE)) {
      parts <- strsplit(lv, "\u00D7", fixed = TRUE)[[1]]
      if (length(parts) >= 2L) {
        lv_a <- parts[[1]]
        lv_b <- paste(parts[-1], collapse = "\u00D7")
      }
    } else if (is.na(lv_a) && is.na(lv_b)) {
      return(sprintf("%s\u00D7%s[%s]", a_abbr, b_abbr, lv))
    }
  }
  fmt_side <- function(abbr, lv) {
    if (!is.null(lv) && !is.na(lv) && nzchar(as.character(lv))) {
      sprintf("%s[%s]", abbr, as.character(lv))
    } else {
      abbr
    }
  }
  paste0(fmt_side(a_abbr, lv_a), "\u00D7", fmt_side(b_abbr, lv_b))
}

#' Abbreviated A×B as LaTeX
#' @keywords internal
pretty_interaction_term_tex <- function(state, ix_id, level = NULL,
                                        abbrev_map = NULL) {
  plain <- pretty_interaction_term_html(
    state, ix_id, level = level, abbrev_map = abbrev_map
  )
  # plain is A×B or A[lv]×B[lv]
  parts <- strsplit(plain, "\u00D7", fixed = TRUE)[[1]]
  if (length(parts) < 2L) return(latex_lhs_display(plain))
  paste(
    vapply(parts, latex_lhs_display, character(1)),
    collapse = " \\times "
  )
}

#' Pretty predictor name as LaTeX (ecole__B → ecole[B]; x_p2 → x^{2})
#' @keywords internal
pretty_term_tex <- function(name, level = NULL, abbrev_map = NULL,
                            state = NULL) {
  if (identical(name, "(Intercept)")) return(NULL)
  ix_id <- resolve_interaction_node_id(state, name)
  if (!is.na(ix_id)) {
    return(pretty_interaction_term_tex(
      state, ix_id, level = level, abbrev_map = abbrev_map
    ))
  }
  base_disp <- abbreviate_term_base(name, abbrev_map)
  if (!is.null(level) && is_poly_level(level)) {
    deg <- sub("^\\^", "", as.character(level))
    return(sprintf("%s^{%s}", latex_var(base_disp), latex_escape_math(deg)))
  }
  if (grepl("_p[0-9]+$", name)) {
    deg <- sub("^.*_p", "", name)
    return(sprintf("%s^{%s}", latex_var(base_disp), latex_escape_math(deg)))
  }
  if (grepl("__", name, fixed = TRUE)) {
    parts <- strsplit(name, "__", fixed = TRUE)[[1]]
    return(sprintf(
      "%s[\\mathrm{%s}]",
      latex_var(base_disp),
      latex_escape_math(paste(parts[-1], collapse = "__"))
    ))
  }
  if (!is.null(level) && !is.na(level) && nzchar(as.character(level))) {
    return(sprintf(
      "%s[\\mathrm{%s}]",
      latex_var(base_disp),
      latex_escape_math(as.character(level))
    ))
  }
  latex_var(base_disp)
}

#' Format one coefficient with ±SE as LaTeX superscript
#' @keywords internal
format_est_se_tex <- function(est, se, digits = 2) {
  est_txt <- formatC(est, digits = digits, format = "f")
  if (!is.finite(se)) return(est_txt)
  sprintf(
    "%s^{\\pm %s}",
    est_txt,
    formatC(se, digits = digits, format = "f")
  )
}

# Back-compat aliases used by older call sites / averaged equations
#' @keywords internal
format_est_se_html <- format_est_se_tex
#' @keywords internal
pretty_term_html <- pretty_term_tex

#' @keywords internal
format_est_ci_html <- function(est, lo, hi, digits = 2) {
  se <- if (is.finite(lo) && is.finite(hi)) (hi - lo) / (2 * 1.96) else NA_real_
  format_est_se_tex(est, se, digits = digits)
}

#' Linear predictor LaTeX from coefficients + SEs
#' @keywords internal
format_linear_predictor_html <- function(cf, se = NULL, digits = 2,
                                         abbrev_map = NULL, state = NULL) {
  parts <- character()
  for (i in seq_along(cf)) {
    nm <- names(cf)[[i]]
    est <- unname(cf[[i]])
    se_i <- if (!is.null(se) && nm %in% names(se)) {
      unname(se[[nm]])
    } else {
      NA_real_
    }
    piece <- format_est_se_tex(est, se_i, digits = digits)
    term <- pretty_term_tex(nm, abbrev_map = abbrev_map, state = state)
    body <- if (is.null(term)) piece else paste0(piece, " \\cdot ", term)
    if (i == 1L) {
      parts <- c(parts, body)
    } else {
      parts <- c(parts, paste0(" + ", body))
    }
  }
  paste(parts, collapse = "")
}

#' Linear predictor LaTeX from estimate rows (includes poly levels)
#' @keywords internal
format_eta_from_estimates <- function(est_rows, digits = 2, abbrev_map = NULL,
                                      state = NULL) {
  if (is.null(est_rows) || !nrow(est_rows)) return("")
  if ("kind" %in% names(est_rows)) {
    est_rows <- est_rows[est_rows$kind != "omnibus", , drop = FALSE]
  }
  est_rows <- est_rows[is.finite(as.numeric(est_rows$est)), , drop = FALSE]
  if (!nrow(est_rows)) return("")
  parts <- character()
  for (i in seq_len(nrow(est_rows))) {
    est <- as.numeric(est_rows$est[i])
    se <- as.numeric(est_rows$se[i])
    piece <- format_est_se_tex(est, se, digits = digits)
    lv <- if ("level" %in% names(est_rows)) est_rows$level[i] else NA_character_
    term <- pretty_term_tex(
      est_rows$rhs[i], level = lv, abbrev_map = abbrev_map, state = state
    )
    body <- if (is.null(term)) piece else paste0(piece, " \\cdot ", term)
    parts <- c(parts, if (!length(parts)) body else paste0(" + ", body))
  }
  paste(parts, collapse = "")
}

#' Random-effects formula fragment from a merMod fit (for equation tags)
#' @keywords internal
mer_re_tag <- function(fit, fallback = "(1|cluster)") {
  fml <- paste(deparse(stats::formula(fit)), collapse = "")
  m <- gregexpr("\\([^()]*\\|[^()]*\\)", fml, perl = TRUE)
  parts <- regmatches(fml, m)[[1]]
  if (!length(parts)) return(fallback)
  # Compact spaces around | for stable tags
  parts <- gsub("\\s*\\|\\s*", "|", gsub("\\s+", " ", parts))
  paste(parts, collapse = " + ")
}

#' Whether a merMod RE term includes slopes (not intercept-only)
#' @keywords internal
mer_has_random_slopes <- function(fit) {
  re <- mer_re_tag(fit, "")
  if (!nzchar(re)) return(FALSE)
  lhs <- sub("\\|.*$", "", re)
  lhs <- gsub("[()]", "", lhs)
  !grepl("^\\s*1\\s*$", lhs)
}

#' Compact a coef name for RE↔fixed matching (`Condition__Treat` ↔ `ConditionTreat`)
#' @keywords internal
coef_name_compact <- function(nm) {
  gsub("__", "", as.character(nm), fixed = TRUE)
}

#' Map a fixed-effect coef name to its random-effect \(u_{kj}\) symbol
#' @keywords internal
match_re_u_symbol <- function(fixed_nm, u_map) {
  if (is.null(u_map) || !length(u_map)) return(NULL)
  fixed_nm <- as.character(fixed_nm)
  if (fixed_nm %in% names(u_map)) return(unname(u_map[[fixed_nm]]))
  compact <- coef_name_compact(fixed_nm)
  nms <- names(u_map)
  hit <- which(coef_name_compact(nms) == compact)
  if (length(hit)) return(unname(u_map[[hit[[1]]]]))
  NULL
}

#' Wrap a fixed estimate with optional random term: `(β + u_{kj})`
#' @keywords internal
format_coef_with_re <- function(est_tex, u_sym = NULL) {
  if (is.null(u_sym) || !nzchar(as.character(u_sym))) return(est_tex)
  sprintf("(%s + %s)", est_tex, u_sym)
}

#' Random-effect \(u\) map + distribution TeX from a merMod fit (first group)
#'
#' @return list(group, u_map, dist_tex, n_terms) or NULL.
#'   `u_map` is named by lme4 RE coef names → `u_{0j}`, `u_{1j}`, …
#' @keywords internal
mer_re_equation_info <- function(fit, digits = 2) {
  if (is.null(fit) || !inherits(fit, "merMod")) return(NULL)
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc) || !length(vc)) return(NULL)
  grp <- names(vc)[[1]]
  mat <- vc[[1]]
  sds <- attr(mat, "stddev")
  if (is.null(sds) || !length(sds)) return(NULL)
  cors <- attr(mat, "correlation")
  nms <- names(sds)
  if (is.null(nms)) nms <- paste0("RE", seq_along(sds))

  u_syms <- vapply(seq_along(sds), function(i) {
    sprintf("u_{%dj}", i - 1L)
  }, character(1))
  u_map <- stats::setNames(u_syms, nms)

  singular <- isTRUE(tryCatch(
    lme4::isSingular(fit, tol = 1e-4),
    error = function(e) FALSE
  ))

  sd_txt <- formatC(as.numeric(sds), digits = digits, format = "f")
  if (length(sds) == 1L) {
    dist_tex <- sprintf(
      "%s \\sim \\mathcal{N}(0, %s)",
      u_syms[[1]],
      sd_txt[[1]]
    )
  } else {
    u_vec <- paste0(
      "\\begin{pmatrix}",
      paste(u_syms, collapse = "\\\\"),
      "\\end{pmatrix}"
    )
    sd_vec <- paste(sd_txt, collapse = ",\\,")
    cor_bits <- character()
    if (is.matrix(cors) && nrow(cors) >= 2L) {
      for (i in seq_len(nrow(cors) - 1L)) {
        for (j in (i + 1L):ncol(cors)) {
          rij <- cors[i, j]
          if (!is.finite(rij)) next
          # |ρ|≈1 or singular fit → color as boundary (not a trustworthy estimate)
          boundary <- isTRUE(singular) || abs(rij) >= 1 - 1e-3
          rho_txt <- sprintf(
            "\\rho_{%d%d}=%s",
            i - 1L, j - 1L,
            formatC(rij, digits = digits, format = "f")
          )
          if (boundary) {
            # Match --smt-danger; MathJax \\color keeps the value readable without a text tag
            rho_txt <- sprintf("\\color{#b33b3b}{%s}", rho_txt)
          }
          cor_bits <- c(cor_bits, rho_txt)
        }
      }
    }
    cor_txt <- if (length(cor_bits)) {
      paste0(",\\; ", paste(cor_bits, collapse = ",\\; "))
    } else {
      ""
    }
    dist_tex <- sprintf(
      "%s \\sim \\mathcal{N}(0,\\,\\Sigma_u),\\quad \\mathrm{sd}(u)=(%s)%s",
      u_vec,
      sd_vec,
      cor_txt
    )
  }
  grp_tex <- latex_var(grp)
  dist_tex <- paste0(dist_tex, sprintf(",\\; j\\sim %s", grp_tex))

  list(
    group = as.character(grp),
    u_map = u_map,
    dist_tex = dist_tex,
    n_terms = length(sds),
    singular = singular
  )
}

#' Build one LaTeX fragment per predictor (fixed ± SE, optional + u)
#' @return character vector of term fragments (no leading +/-)
#' @keywords internal
format_eta_term_lines <- function(cf, se = NULL, digits = 2,
                                  abbrev_map = NULL, state = NULL,
                                  u_map = NULL) {
  lines <- character()
  used_u <- character()
  for (i in seq_along(cf)) {
    nm <- names(cf)[[i]]
    est <- unname(cf[[i]])
    se_i <- if (!is.null(se) && nm %in% names(se)) {
      unname(se[[nm]])
    } else {
      NA_real_
    }
    piece <- format_est_se_tex(est, se_i, digits = digits)
    u_sym <- match_re_u_symbol(nm, u_map)
    if (!is.null(u_sym)) used_u <- c(used_u, u_sym)
    coef_tex <- format_coef_with_re(piece, u_sym)
    term <- pretty_term_tex(nm, abbrev_map = abbrev_map, state = state)
    if (is.null(term)) {
      lines <- c(lines, coef_tex)
    } else {
      lines <- c(lines, paste0(coef_tex, " \\cdot ", term))
    }
  }
  # Orphan RE slopes with no matching fixed coef (should be rare)
  if (!is.null(u_map) && length(u_map)) {
    for (i in seq_along(u_map)) {
      u_sym <- unname(u_map[[i]])
      if (u_sym %in% used_u) next
      nm <- names(u_map)[[i]]
      if (identical(nm, "(Intercept)")) {
        lines <- c(lines, u_sym)
      } else {
        lines <- c(lines, paste0(u_sym, "\\,", latex_var(nm)))
      }
    }
  }
  lines
}

#' Term lines from estimate rows (+ optional intercept), with RE merge
#' @keywords internal
format_eta_term_lines_from_estimates <- function(est_rows, intercept_tex = NULL,
                                                 digits = 2, abbrev_map = NULL,
                                                 state = NULL, u_map = NULL) {
  lines <- character()
  used_u <- character()
  if (!is.null(intercept_tex) && nzchar(intercept_tex)) {
    u0 <- match_re_u_symbol("(Intercept)", u_map)
    if (!is.null(u0)) used_u <- c(used_u, u0)
    lines <- c(lines, format_coef_with_re(intercept_tex, u0))
  }
  if (is.null(est_rows) || !nrow(est_rows)) {
    # still attach unused RE below
  } else {
    if ("kind" %in% names(est_rows)) {
      est_rows <- est_rows[est_rows$kind != "omnibus", , drop = FALSE]
    }
    est_rows <- est_rows[is.finite(as.numeric(est_rows$est)), , drop = FALSE]
    for (i in seq_len(nrow(est_rows))) {
      est <- as.numeric(est_rows$est[i])
      se <- as.numeric(est_rows$se[i])
      piece <- format_est_se_tex(est, se, digits = digits)
      lv <- if ("level" %in% names(est_rows)) est_rows$level[i] else NA_character_
      rhs <- as.character(est_rows$rhs[i])
      # Match RE against dummy / factor-level name when present
      fixed_key <- if (!is.na(lv) && nzchar(as.character(lv)) &&
                         !is_poly_level(lv)) {
        dummy_col_name(rhs, lv)
      } else {
        rhs
      }
      u_sym <- match_re_u_symbol(fixed_key, u_map)
      if (is.null(u_sym)) u_sym <- match_re_u_symbol(rhs, u_map)
      if (!is.null(u_sym)) used_u <- c(used_u, u_sym)
      coef_tex <- format_coef_with_re(piece, u_sym)
      term <- pretty_term_tex(
        rhs, level = lv, abbrev_map = abbrev_map, state = state
      )
      if (is.null(term)) {
        lines <- c(lines, coef_tex)
      } else {
        lines <- c(lines, paste0(coef_tex, " \\cdot ", term))
      }
    }
  }
  if (!is.null(u_map) && length(u_map)) {
    for (i in seq_along(u_map)) {
      u_sym <- unname(u_map[[i]])
      if (u_sym %in% used_u) next
      nm <- names(u_map)[[i]]
      if (identical(nm, "(Intercept)")) {
        lines <- c(lines, u_sym)
      } else {
        lines <- c(lines, paste0(u_sym, "\\,", latex_var(nm)))
      }
    }
  }
  lines
}

#' Fitted residual SD for equation display (NA if unavailable / not gaussian)
#' @keywords internal
fit_residual_sigma <- function(fit, yname = NULL) {
  if (is.null(fit)) return(NA_real_)
  if (inherits(fit, "glmerMod")) return(NA_real_)
  if (inherits(fit, "glm")) {
    fam <- tryCatch(stats::family(fit), error = function(e) NULL)
    # Residual SD only for identity-link continuous GLMs
    if (is.null(fam) || !identical(fam$link, "identity") ||
          identical(fam$family, "binomial")) {
      return(NA_real_)
    }
  }
  if (inherits(fit, "lavaan")) {
    yname <- as.character(yname %||% "")
    if (!nzchar(yname)) return(NA_real_)
    pe <- tryCatch(
      as.data.frame(lavaan::parameterEstimates(fit, se = FALSE)),
      error = function(e) NULL
    )
    if (is.null(pe) || !nrow(pe)) return(NA_real_)
    hit <- pe$op == "~~" & pe$lhs == yname & pe$rhs == yname
    if (!any(hit)) return(NA_real_)
    v <- as.numeric(pe$est[which(hit)[[1]]])
    if (!is.finite(v) || v < 0) return(NA_real_)
    return(sqrt(v))
  }
  s <- tryCatch(stats::sigma(fit), error = function(e) NA_real_)
  if (!is.finite(s) || s < 0) return(NA_real_)
  as.numeric(s)
}

#' LaTeX residual clause: ` + \varepsilon,\quad\varepsilon\sim\mathcal{N}(0,s)`
#' @keywords internal
generative_eps_tex <- function(sigma = NA_real_, digits = 2) {
  if (is.finite(sigma) && sigma >= 0) {
    s_txt <- formatC(sigma, digits = digits, format = "f")
    return(sprintf(
      " + \\varepsilon,\\quad \\varepsilon\\sim\\mathcal{N}(0, %s)",
      s_txt
    ))
  }
  " + \\varepsilon"
}

#' Assemble outcome equation TeX from wrapper + predictor term line(s)
#'
#' When `eta` is a character vector (one entry per predictor), builds an
#' `aligned` block with fixed+random merged on each line.
#' @keywords internal
smt_eq_outcome_tex <- function(wrap, eta) {
  eta_lines <- as.character(eta %||% "")
  eta_lines <- eta_lines[nzchar(eta_lines)]
  eps <- if (isTRUE(wrap$generative)) {
    generative_eps_tex(wrap$sigma %||% NA_real_)
  } else {
    ""
  }
  # Strip leading " + " from generative_eps_tex when used as its own align row
  eps_row <- if (nzchar(eps)) sub("^ \\+ ", "", eps) else ""

  if (length(eta_lines) <= 1L) {
    eta_one <- if (length(eta_lines)) eta_lines[[1]] else "0"
    body <- paste0(
      wrap$lhs %||% "",
      " = ",
      wrap$wrap_left %||% "",
      eta_one,
      wrap$wrap_right %||% ""
    )
    if (nzchar(eps)) body <- paste0(body, eps)
    return(body)
  }

  # Multi-line: one predictor per row; ε on its own final row when generative
  rows <- character()
  for (i in seq_along(eta_lines)) {
    pref <- if (i == 1L) "" else "+ "
    if (i == 1L) {
      rows <- c(rows, sprintf(
        "%s &= %s%s%s",
        wrap$lhs %||% "",
        wrap$wrap_left %||% "",
        eta_lines[[i]],
        if (!nzchar(eps_row) && !nzchar(wrap$wrap_right %||% "")) {
          wrap$wrap_right %||% ""
        } else {
          ""
        }
      ))
    } else {
      rows <- c(rows, sprintf("&\\quad %s%s", pref, eta_lines[[i]]))
    }
  }
  if (nzchar(wrap$wrap_right %||% "") && !nzchar(eps_row)) {
    rows[[length(rows)]] <- paste0(rows[[length(rows)]], wrap$wrap_right)
  } else if (nzchar(wrap$wrap_right %||% "") && nzchar(eps_row)) {
    # close link wrapper after last predictor, before residual row
    rows[[length(rows)]] <- paste0(rows[[length(rows)]], wrap$wrap_right)
  }
  if (nzchar(eps_row)) {
    rows <- c(rows, sprintf("&\\quad + %s", eps_row))
  }
  paste0("\\begin{aligned}", paste(rows, collapse = " \\\\"), "\\end{aligned}")
}

#' MathJax display for aligned / long outcome equations
#' @keywords internal
smt_eq_math_display <- function(tex) {
  sprintf("<div class=\"smt-eq-line\">\\[%s\\]</div>", tex)
}

#' One or more MathJax lines for an outcome equation (mean + optional RE dist)
#' @keywords internal
smt_eq_outcome_html <- function(wrap, eta) {
  tex <- smt_eq_outcome_tex(wrap, eta)
  use_display <- grepl("\\\\begin\\{aligned\\}", tex, perl = TRUE)
  lines <- if (use_display) smt_eq_math_display(tex) else smt_eq_math_line(tex)
  re_dist <- wrap$re_dist %||% ""
  if (nzchar(re_dist)) {
    lines <- paste0(lines, smt_eq_math_line(re_dist))
  }
  lines
}

#' Attach merMod RE fragments onto an equation wrapper list
#' @keywords internal
equation_wrapper_add_re <- function(wrap, fit) {
  info <- mer_re_equation_info(fit)
  if (is.null(info)) return(wrap)
  wrap$re_u_map <- info$u_map
  wrap$re_dist <- info$dist_tex
  wrap$re_group <- info$group
  wrap$re_terms <- NULL
  wrap
}

#' Link / expectation / generative wrapper for an outcome equation (LaTeX)
#' @return list(lhs, wrap_left, wrap_right, tag, note, generative, sigma)
#' @keywords internal
equation_link_wrapper <- function(fit, yname) {
  y_tex <- latex_lhs_display(yname)
  if (inherits(fit, "glmerMod")) {
    fam <- tryCatch(stats::family(fit), error = function(e) NULL)
    link <- if (!is.null(fam)) fam$link else "logit"
    re <- mer_re_tag(fit)
    if (identical(link, "logit")) {
      wrap <- list(
        tag = paste("glmer · binomial · logit ·", re),
        lhs = sprintf("\\mathrm{P}(%s = 1)", y_tex),
        wrap_left = "\\mathrm{logit}^{-1}(",
        wrap_right = ")",
        note = "\\(\\mathrm{logit}^{-1}\\) = logistic (expit); \\(j\\) indexes the clustering factor",
        generative = FALSE,
        sigma = NA_real_,
        re_terms = NULL,
        re_dist = NULL
      )
      return(equation_wrapper_add_re(wrap, fit))
    }
    wrap <- list(
      tag = paste0("glmer · binomial · ", link, " · ", re),
      lhs = sprintf("\\mathrm{P}(%s = 1)", y_tex),
      wrap_left = "",
      wrap_right = "",
      note = "\\(j\\) indexes the clustering factor",
      generative = FALSE,
      sigma = NA_real_,
      re_terms = NULL,
      re_dist = NULL
    )
    return(equation_wrapper_add_re(wrap, fit))
  }
  if (inherits(fit, "lmerMod") || (inherits(fit, "merMod") && !inherits(fit, "glmerMod"))) {
    re <- mer_re_tag(fit)
    wrap <- list(
      tag = paste("lmer · gaussian ·", re),
      lhs = y_tex,
      wrap_left = "",
      wrap_right = "",
      note = "\\(j\\) indexes the clustering factor; random effects shown with fitted SDs",
      generative = TRUE,
      sigma = fit_residual_sigma(fit),
      re_terms = NULL,
      re_dist = NULL
    )
    return(equation_wrapper_add_re(wrap, fit))
  }
  if (inherits(fit, "glm")) {
    fam <- tryCatch(stats::family(fit), error = function(e) NULL)
    fam_name <- if (!is.null(fam)) fam$family else "glm"
    link <- if (!is.null(fam)) fam$link else "unknown"
    if (identical(fam_name, "binomial") && identical(link, "logit")) {
      return(list(
        tag = "glm · binomial · logit",
        lhs = sprintf("\\mathrm{P}(%s = 1)", y_tex),
        wrap_left = "\\mathrm{logit}^{-1}(",
        wrap_right = ")",
        note = "\\(\\mathrm{logit}^{-1}\\) = logistic (expit)",
        generative = FALSE,
        sigma = NA_real_
      ))
    }
    if (identical(fam_name, "binomial") && identical(link, "probit")) {
      return(list(
        tag = "glm · binomial · probit",
        lhs = sprintf("\\mathrm{P}(%s = 1)", y_tex),
        wrap_left = "\\Phi(",
        wrap_right = ")",
        note = "\\(\\Phi\\) = standard normal CDF",
        generative = FALSE,
        sigma = NA_real_
      ))
    }
    if (identical(link, "identity")) {
      return(list(
        tag = sprintf("glm · %s · identity", fam_name),
        lhs = y_tex,
        wrap_left = "",
        wrap_right = "",
        note = NULL,
        generative = TRUE,
        sigma = fit_residual_sigma(fit)
      ))
    }
    return(list(
      tag = sprintf("glm · %s · %s", fam_name, link),
      lhs = sprintf("\\mathrm{E}(%s)", y_tex),
      wrap_left = sprintf(
        "g^{-1}_{\\mathrm{%s}}(",
        latex_escape_math(link)
      ),
      wrap_right = ")",
      note = sprintf(
        "\\(g^{-1}\\) = inverse %s link",
        htmltools::htmlEscape(link)
      ),
      generative = FALSE,
      sigma = NA_real_
    ))
  }
  if (inherits(fit, "lm")) {
    return(list(
      tag = "lm · gaussian · identity",
      lhs = y_tex,
      wrap_left = "",
      wrap_right = "",
      note = NULL,
      generative = TRUE,
      sigma = fit_residual_sigma(fit)
    ))
  }
  list(
    tag = "model",
    lhs = y_tex,
    wrap_left = "",
    wrap_right = "",
    note = NULL,
    generative = FALSE,
    sigma = NA_real_
  )
}

#' Map a lavaan variable name back to a node display label
#' @keywords internal
display_name_from_lavaan <- function(state, lavaan_nm, abbrev_map = NULL) {
  lavaan_nm <- as.character(lavaan_nm)
  if (!nzchar(lavaan_nm)) return(lavaan_nm)
  lab <- lavaan_nm
  if (!is.null(state) && NROW(state$nodes)) {
    for (i in seq_len(nrow(state$nodes))) {
      id <- state$nodes$id[[i]]
      nm <- tryCatch(lavaan_var_name(state, id), error = function(e) NA_character_)
      if (identical(nm, lavaan_nm)) {
        lab <- node_display_name(state, id)
        break
      }
    }
  }
  abbreviate_lhs_display(lab, abbrev_map)
}

#' HTML for path-stage factor-score equations (measurement)
#'
#' Shows the regression / Thomson score weights actually used in the path
#' stage. CFA loadings stay on the canvas, not in Model fit.
#' @keywords internal
format_measurement_equations_html <- function(result, state = NULL,
                                              abbrev_map = NULL) {
  if (is.null(result)) return(NULL)
  if (is.null(abbrev_map)) abbrev_map <- equation_abbrev_map(result, state)

  # Score projection weights used in the path stage
  score_method <- normalize_latent_score_method(
    result$latent_score_method %||% "own",
    state
  )
  score_tag <- if (identical(score_method, "joint")) {
    "joint Thomson"
  } else {
    "own indicators"
  }
  score_blocks <- character()
  w_list <- result$measurement_score_weights
  if (is.null(w_list) || !length(w_list)) {
    # Fallback for older results: derive from per-factor score fits if present
    fits <- result$measurement_score_fits
    if (is.list(fits) && length(fits)) {
      w_list <- list()
      for (nm in names(fits)) {
        ww <- cfa_regression_score_weights(fits[[nm]])
        if (!is.null(ww) && length(ww)) {
          w_list[[nm]] <- if (nm %in% names(ww)) ww[[nm]] else ww[[1]]
        }
      }
    }
  }
  cronbach <- result$measurement_cronbach_alpha
  if (!is.null(w_list) && length(w_list)) {
    score_blocks <- vapply(names(w_list), function(lat) {
      ww <- w_list[[lat]]
      ww <- ww[is.finite(ww)]
      if (!length(ww)) return(NA_character_)
      lat_disp <- display_name_from_lavaan(state, lat, abbrev_map)
      lat_tex <- latex_var(lat_disp)
      parts <- character()
      for (j in seq_along(ww)) {
        ind_tex <- latex_var(
          display_name_from_lavaan(state, names(ww)[[j]], abbrev_map)
        )
        piece <- formatC(unname(ww[[j]]), digits = 2, format = "f")
        body <- paste0(piece, " \\cdot ", ind_tex)
        parts <- c(parts, if (!length(parts)) body else paste0(" + ", body))
      }
      a_val <- if (!is.null(cronbach) && lat %in% names(cronbach)) {
        as.numeric(cronbach[[lat]])
      } else {
        NA_real_
      }
      a_txt <- if (is.finite(a_val)) {
        sprintf(
          " · Cronbach\u2019s \\(\\alpha = %s\\)",
          formatC(a_val, digits = 2, format = "f")
        )
      } else {
        ""
      }
      uni_slot <- NULL
      uni_map <- result$measurement_unidimensionality
      if (is.list(uni_map) && length(uni_map)) {
        for (uid in names(uni_map)) {
          slot <- uni_map[[uid]]
          if (!is.list(slot)) next
          lv_nm <- as.character(slot$lavaan_name %||% "")
          if (identical(lv_nm, lat) || identical(uid, lat)) {
            uni_slot <- slot
            break
          }
        }
      }
      uni_txt <- ""
      if (!is.null(uni_slot)) {
        ust <- as.character(uni_slot$status %||% "")
        ulab <- as.character(uni_slot$label %||% "")
        uratio <- suppressWarnings(as.numeric(uni_slot$ratio))
        if (nzchar(ust) && !is.na(ust)) {
          detail <- if (is.finite(uratio)) {
            sprintf(
              "\\(\\lambda_1/\\lambda_2 = %s\\)",
              formatC(uratio, digits = 2, format = "f")
            )
          } else {
            ""
          }
          lab_bit <- if (nzchar(ulab)) ulab else ust
          uni_txt <- if (nzchar(detail)) {
            sprintf(" · %s (%s)", lab_bit, detail)
          } else {
            sprintf(" · %s", lab_bit)
          }
        }
      }
      sprintf(
        paste0(
          "<div class=\"smt-eq-block\">",
          "<div class=\"smt-eq-tag\">factor score · %s (%s)%s%s</div>",
          "%s",
          "</div>"
        ),
        htmltools::htmlEscape(lat_disp),
        score_tag,
        a_txt,
        uni_txt,
        smt_eq_math_line(sprintf(
          "\\widehat{%s} = %s",
          lat_tex,
          paste(parts, collapse = "")
        ))
      )
    }, character(1))
    score_blocks <- score_blocks[!is.na(score_blocks)]
  }
  if (!length(score_blocks)) return(NULL)

  note <- if (identical(score_method, "joint")) {
    paste0(
      "<div class=\"smt-eq-note\">",
      "Path / plots use joint Thomson/regression scores from the multi-factor CFA ",
      "(items from other constructs can receive non-zero weight when factors correlate). ",
      "Cronbach\u2019s \\(\\alpha\\) and the PCA unidimensionality screen ",
      "(\\( \\lambda_1/\\lambda_2 \\)) use each latent\u2019s own indicators only.",
      "</div>"
    )
  } else {
    paste0(
      "<div class=\"smt-eq-note\">",
      "Path / plots score each latent from its own measurement indicators only ",
      "(items from other constructs are never mixed into the score). ",
      "Cronbach\u2019s \\(\\alpha\\) is classical reliability; ",
      "uni \u2713 / weak 1D / multi ? is a PCA screen ",
      "(\\( \\lambda_1/\\lambda_2 \\ge 3 \\) / 2\u20133 / &lt; 2).",
      "</div>"
    )
  }
  paste0(
    "<div class=\"smt-eq-caption\">Factor scores (path stage)</div>",
    paste(score_blocks, collapse = ""),
    note
  )
}

#' Build HTML equations for the fitted (best) model
#'
#' Shows each outcome with an explicit model/link form, e.g.
#' `P(y)=logit^{-1}(...)` for logistic GLM, plus estimates ± SE.
#' Variable names are shortened to the shortest unique prefixes.
#' When latents are used, path-stage factor-score equations are included.
#' @keywords internal
format_best_equations_html <- function(result, state = NULL) {
  if (is.null(result)) return(NULL)
  abbrev_map <- equation_abbrev_map(result, state)
  meas_html <- format_measurement_equations_html(result, state, abbrev_map)
  caption <- paste0(
    "<div class=\"smt-eq-caption\">Best model ",
    "(estimate\\(^{\\pm\\mathrm{SE}}\\))</div>"
  )

  # Path lm/glm: one fit object per DV
  if (identical(result$engine, "path") && is.list(result$fit) &&
        !inherits(result$fit, "lavaan")) {
    notes <- character()
    est_all <- result$estimates
    blocks <- lapply(names(result$fit), function(fit_key) {
      fit <- result$fit[[fit_key]]
      if (is.null(fit)) return(NULL)
      cf <- path_fixed_coefs(fit)
      if (!length(cf)) return(NULL)
      parts <- strsplit(fit_key, "::", fixed = TRUE)[[1]]
      dv_id <- parts[[1]]
      resp_lv <- if (length(parts) > 1L) parts[[2]] else NA_character_
      yname <- if (!is.null(state)) {
        tryCatch(node_display_name(state, dv_id), error = function(e) dv_id)
      } else {
        dv_id
      }
      if (!is.na(resp_lv) && nzchar(resp_lv)) {
        yname <- dv_outcome_lhs(yname, resp_lv)
      } else if (!is.null(state)) {
        yname <- dv_transform_lhs(yname, node_dv_transform(state, dv_id))
      } else {
        rn <- tryCatch(as.character(stats::formula(fit)[[2]]), error = function(e) yname)
        if (nzchar(rn)) yname <- rn
      }

      y_disp <- abbreviate_lhs_display(yname, abbrev_map)
      wrap <- equation_link_wrapper(fit, y_disp)
      if (!is.null(wrap$note)) notes <<- c(notes, wrap$note)
      u_map <- wrap$re_u_map

      # Prefer estimate rows (explicit poly levels) + intercept from fit
      eta_lines <- NULL
      if (!is.null(est_all) && nrow(est_all)) {
        rows <- est_all[est_all$op == "~" & est_all$lhs == yname, , drop = FALSE]
        if (nrow(rows) || "(Intercept)" %in% names(cf)) {
          intercept_tex <- NULL
          if ("(Intercept)" %in% names(cf)) {
            se0 <- tryCatch({
              V <- as.matrix(stats::vcov(fit))
              sqrt(diag(V))[["(Intercept)"]]
            }, error = function(e) NA_real_)
            b0 <- unname(cf[["(Intercept)"]])
            intercept_tex <- format_est_se_html(b0, se0)
          }
          eta_lines <- format_eta_term_lines_from_estimates(
            rows,
            intercept_tex = intercept_tex,
            abbrev_map = abbrev_map,
            state = state,
            u_map = u_map
          )
        }
      }
      if (is.null(eta_lines) || !length(eta_lines)) {
        se_vec <- tryCatch({
          V <- as.matrix(stats::vcov(fit))
          stats::setNames(sqrt(diag(V)), colnames(V))
        }, error = function(e) setNames(rep(NA_real_, length(cf)), names(cf)))
        eta_lines <- format_eta_term_lines(
          cf, se_vec, abbrev_map = abbrev_map, state = state, u_map = u_map
        )
      }
      sprintf(
        paste0(
          "<div class=\"smt-eq-block\">",
          "<div class=\"smt-eq-tag\">%s</div>",
          "%s",
          "</div>"
        ),
        htmltools::htmlEscape(wrap$tag),
        smt_eq_outcome_html(wrap, eta_lines)
      )
    })
    blocks <- Filter(Negate(is.null), blocks)
    if (!length(blocks) && is.null(meas_html)) return(NULL)
    notes <- unique(notes)
    note_html <- if (length(notes)) {
      paste0("<div class=\"smt-eq-note\">", paste(notes, collapse = " · "), "</div>")
    } else {
      ""
    }
    return(htmltools::HTML(paste0(
      "<div class=\"smt-equations\">",
      meas_html %||% "",
      if (length(blocks)) paste0(caption, paste(blocks, collapse = "")) else "",
      note_html,
      "</div>"
    )))
  }

  # lavaan — prefer SMT estimates (poly-aware), else parameterEstimates
  est_all <- result$estimates
  if (!is.null(est_all) && nrow(est_all) && any(est_all$op == "~")) {
    fit <- result$fit
    ordered <- character()
    if (inherits(fit, "lavaan")) {
      ordered <- tryCatch(lavaan::lavInspect(fit, "ordered"), error = function(e) character())
      if (is.null(ordered)) ordered <- character()
    }
    pe <- est_all[est_all$op == "~", , drop = FALSE]
    blocks <- lapply(unique(pe$lhs), function(y) {
      rows <- pe[pe$lhs == y, , drop = FALSE]
      eta <- format_eta_from_estimates(
        rows, abbrev_map = abbrev_map, state = state
      )
      y_tex <- latex_lhs_display(abbreviate_lhs_display(y, abbrev_map))
      is_ord <- y %in% ordered
      if (is_ord) {
        sprintf(
          paste0(
            "<div class=\"smt-eq-block\">",
            "<div class=\"smt-eq-tag\">lavaan · ordered · probit/WLSMV</div>",
            "%s",
            "</div>"
          ),
          smt_eq_math_line(sprintf(
            "\\mathrm{P}(%s^{*}) = \\Phi(%s)", y_tex, eta
          ))
        )
      } else {
        wrap_lav <- list(
          lhs = y_tex,
          wrap_left = "",
          wrap_right = "",
          generative = TRUE,
          sigma = fit_residual_sigma(fit, yname = y)
        )
        sprintf(
          paste0(
            "<div class=\"smt-eq-block\">",
            "<div class=\"smt-eq-tag\">lavaan · continuous · identity</div>",
            "%s",
            "</div>"
          ),
          smt_eq_math_line(smt_eq_outcome_tex(wrap_lav, eta))
        )
      }
    })
    return(htmltools::HTML(paste0(
      "<div class=\"smt-equations\">",
      meas_html %||% "",
      caption,
      paste(blocks, collapse = ""),
      "</div>"
    )))
  }

  fit <- result$fit
  if (!inherits(fit, "lavaan")) {
    if (!is.null(meas_html)) {
      return(htmltools::HTML(paste0(
        "<div class=\"smt-equations\">", meas_html, "</div>"
      )))
    }
    return(NULL)
  }
  pe <- tryCatch(
    as.data.frame(lavaan::parameterEstimates(fit, se = TRUE)),
    error = function(e) NULL
  )
  if (is.null(pe) || !nrow(pe)) return(NULL)
  pe <- pe[pe$op == "~", , drop = FALSE]
  if (!nrow(pe)) return(NULL)

  ordered <- tryCatch(lavaan::lavInspect(fit, "ordered"), error = function(e) character())
  if (is.null(ordered)) ordered <- character()

  # Extend abbrev map with lavaan parameter names if needed
  abbrev_map <- shortest_unique_prefixes(unique(c(
    names(abbrev_map),
    vapply(pe$rhs, equation_term_base, character(1)),
    vapply(pe$lhs, equation_lhs_core, character(1))
  )))

  blocks <- lapply(unique(pe$lhs), function(y) {
    rows <- pe[pe$lhs == y, , drop = FALSE]
    cf <- setNames(rows$est, rows$rhs)
    se_vec <- setNames(rows$se, rows$rhs)
    eta <- format_linear_predictor_html(
      cf, se_vec, abbrev_map = abbrev_map, state = state
    )
    y_tex <- latex_lhs_display(abbreviate_lhs_display(y, abbrev_map))
    is_ord <- y %in% ordered
    if (is_ord) {
      sprintf(
        paste0(
          "<div class=\"smt-eq-block\">",
          "<div class=\"smt-eq-tag\">lavaan · ordered · probit/WLSMV</div>",
          "%s",
          "</div>"
        ),
        smt_eq_math_line(sprintf(
          "\\mathrm{P}(%s^{*}) = \\Phi(%s)", y_tex, eta
        ))
      )
    } else {
      wrap_lav <- list(
        lhs = y_tex,
        wrap_left = "",
        wrap_right = "",
        generative = TRUE,
        sigma = fit_residual_sigma(fit, yname = y)
      )
      sprintf(
        paste0(
          "<div class=\"smt-eq-block\">",
          "<div class=\"smt-eq-tag\">lavaan · continuous · identity</div>",
          "%s",
          "</div>"
        ),
        smt_eq_math_line(smt_eq_outcome_tex(wrap_lav, eta))
      )
    }
  })
  htmltools::HTML(paste0(
    "<div class=\"smt-equations\">",
    meas_html %||% "",
    caption,
    paste(blocks, collapse = ""),
    "</div>"
  ))
}

#' Short predictor label for an edge (e.g. methode, ecole[B], V×Su)
#' @keywords internal
edge_predictor_label <- function(state, edge_id, abbrev_map = NULL) {
  idx <- match(edge_id, state$edges$id)
  if (is.na(idx)) return(edge_id)
  from_id <- state$edges$from[idx]
  if (is_interaction_node(state, from_id)) {
    # Strip HTML-ish not needed; return plain text A×B
    html <- pretty_interaction_term_html(
      state, from_id,
      level = if ("level" %in% names(state$edges)) state$edges$level[idx] else NULL,
      abbrev_map = abbrev_map
    )
    return(html)
  }
  from_lab <- node_display_name(state, from_id)
  from_lab <- abbreviate_term_base(from_lab, abbrev_map)
  lv <- if ("level" %in% names(state$edges)) state$edges$level[idx] else NA_character_
  if (!is.na(lv) && nzchar(lv)) {
    sprintf("%s[%s]", from_lab, lv)
  } else {
    from_lab
  }
}

#' Format free-edge id list as short predictor names
#' @keywords internal
format_predictor_set <- function(edge_ids_csv, state) {
  if (is.null(edge_ids_csv) || is.na(edge_ids_csv) || !nzchar(edge_ids_csv)) {
    return("(null)")
  }
  ids <- strsplit(edge_ids_csv, ",", fixed = TRUE)[[1]]
  labs <- vapply(ids, function(id) edge_predictor_label(state, id), character(1))
  paste(labs, collapse = ", ")
}

#' Format model-comparison table for display
#' @keywords internal
format_comparison_display <- function(comparison, state = NULL,
                                      include_role = TRUE) {
  if (is.null(comparison) || !nrow(comparison)) return(comparison)
  predictors <- if (is.null(state)) {
    ifelse(is.na(comparison$edges) | !nzchar(comparison$edges), "(null)", comparison$edges)
  } else {
    unname(vapply(comparison$edges, format_predictor_set, character(1), state = state))
  }
  role <- if ("role" %in% names(comparison)) {
    ifelse(is.na(comparison$role), "", as.character(comparison$role))
  } else {
    rep("", nrow(comparison))
  }
  out <- data.frame(
    role = role,
    predictors = predictors,
    AIC = round(comparison$aic),
    BIC = round(comparison$bic),
    delta = round(comparison$delta, 1),
    weight = round(comparison$weight, 3),
    status = comparison$status,
    stringsAsFactors = FALSE
  )
  if (!isTRUE(include_role) || !any(nzchar(out$role))) {
    out$role <- NULL
  }
  rownames(out) <- NULL
  out
}

#' Format the dedicated best-vs-confirmatory summary table
#' @keywords internal
format_best_vs_confirmatory_display <- function(bvc, state = NULL) {
  if (is.null(bvc) || !nrow(bvc)) return(NULL)
  format_comparison_display(bvc, state = state, include_role = TRUE)
}

#' Simple two-path mediation (indirect) effects a×b from fitted edges
#'
#' For every chain X → M → Y of included structural paths with finite a, b and
#' SEs, reports the product with a first-order Sobel SE / p-value.
#' @keywords internal
indirect_effects_table <- function(state) {
  empty <- data.frame(
    From = character(), Level = character(), To = character(),
    Type = character(), Status = character(),
    Estimate = numeric(), SE = numeric(), p = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(state) || !nrow(state$edges)) return(empty)
  edges <- ensure_edge_to_port(state)$edges
  if (!"level" %in% names(edges)) edges$level <- NA_character_
  if (!"status" %in% names(edges)) edges$status <- "pending"

  keep <- !edges$type %in% c("definitional", "measurement") &
    !vapply(edges$to, function(id) is_interaction_node(state, id), logical(1)) &
    !vapply(edges$from, function(id) is_interaction_node(state, id), logical(1)) &
    !is.na(edges$estimate) & is.finite(edges$estimate) &
    !is.na(edges$se) & is.finite(edges$se) & edges$se >= 0 &
    (is.na(edges$status) | edges$status != "excluded")
  # Skip omnibus / multi-level categorical edges (a×b not a single product)
  keep <- keep & (is.na(edges$level) | !nzchar(edges$level) |
                    !identical(as.character(edges$level), "omnibus"))
  e <- edges[keep, , drop = FALSE]
  if (nrow(e) < 2L) return(empty)

  rows <- list()
  for (i in seq_len(nrow(e))) {
    for (j in seq_len(nrow(e))) {
      if (i == j) next
      if (!identical(e$to[i], e$from[j])) next
      if (identical(e$from[i], e$to[j])) next
      a <- as.numeric(e$estimate[i])
      b <- as.numeric(e$estimate[j])
      se_a <- as.numeric(e$se[i])
      se_b <- as.numeric(e$se[j])
      ab <- a * b
      # First-order delta method (Sobel)
      se_ab <- sqrt(b^2 * se_a^2 + a^2 * se_b^2)
      pv <- if (is.finite(se_ab) && se_ab > 0) {
        2 * stats::pnorm(-abs(ab / se_ab))
      } else {
        NA_real_
      }
      med <- e$to[i]
      rows[[length(rows) + 1L]] <- data.frame(
        From = node_display_name(state, e$from[i]),
        Level = paste0("via ", node_display_name(state, med)),
        To = node_display_name(state, e$to[j]),
        Type = "indirect",
        Status = "derived",
        Estimate = round(ab, 3),
        SE = round(se_ab, 3),
        p = format_pvalue(pv, digits = 3),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' Column name for confirmatory-adjusted p-values in results tables
#' @keywords internal
conf_pvalue_column_name <- function(method = "none") {
  switch(
    as.character(method %||% "none"),
    holm = "pHolm",
    bonferroni = "pBonf",
    BH = "pBH",
    "p"
  )
}

#' Path coefficient table for results pane
#'
#' @param conf_adjust Multiplicity method used for confirmatory edges; names the
#'   p-column `p`, `pHolm`, `pBonf`, or `pBH`.
#' @keywords internal
estimates_table <- function(state, estimates = NULL, conf_adjust = "none") {
  empty <- data.frame(
    From = character(), Level = character(), To = character(),
    Type = character(), Status = character(),
    Estimate = numeric(), SE = numeric(), p = character(),
    stringsAsFactors = FALSE
  )

  # Prefer full estimate rows (includes polynomial powers)
  if (!is.null(estimates) && nrow(estimates)) {
    est <- estimates[estimates$op %in% c("~", "=~"), , drop = FALSE]
    if (!nrow(est)) {
      main <- empty
    } else {
      from_lab <- vapply(seq_len(nrow(est)), function(i) {
        if (identical(est$op[i], "=~")) est$lhs[i] else est$rhs[i]
      }, character(1))
      to_lab <- vapply(seq_len(nrow(est)), function(i) {
        if (identical(est$op[i], "=~")) est$rhs[i] else est$lhs[i]
      }, character(1))
      lv <- if ("level" %in% names(est)) {
        ifelse(is.na(est$level) | !nzchar(as.character(est$level)), "", as.character(est$level))
      } else {
        rep("", nrow(est))
      }
      if ("kind" %in% names(est)) {
        lv <- ifelse(est$kind == "omnibus", "omnibus", lv)
      }
      main <- data.frame(
        From = from_lab,
        Level = lv,
        To = to_lab,
        Type = ifelse(est$op == "=~", "measurement", "path"),
        Status = "included",
        Estimate = round(as.numeric(est$est), 3),
        SE = round(as.numeric(est$se), 3),
        p = vapply(est$pvalue, format_pvalue, character(1), digits = 3),
        stringsAsFactors = FALSE
      )
    }
  } else {
    edges <- state$edges
    if (!nrow(edges)) {
      main <- empty
    } else {
      if (!"level" %in% names(edges)) edges$level <- NA_character_
      # Hide definitional wires into interaction operators
      keep <- edges$type != "definitional" &
        !vapply(edges$to, function(id) is_interaction_node(state, id), logical(1))
      edges <- edges[keep, , drop = FALSE]
      if (!nrow(edges)) {
        main <- empty
      } else {
        main <- data.frame(
          From = vapply(edges$from, function(id) node_display_name(state, id), character(1)),
          Level = ifelse(is.na(edges$level) | !nzchar(edges$level), "", edges$level),
          To = vapply(edges$to, function(id) node_display_name(state, id), character(1)),
          Type = edges$type,
          Status = edges$status,
          Estimate = round(edges$estimate, 3),
          SE = round(edges$se, 3),
          p = vapply(edges$pvalue, format_pvalue, character(1), digits = 3),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  indir <- indirect_effects_table(state)
  out <- if (!nrow(main)) {
    indir
  } else if (!nrow(indir)) {
    main
  } else {
    rbind(main, indir)
  }
  pcol <- conf_pvalue_column_name(conf_adjust)
  if (!identical(pcol, "p") && "p" %in% names(out)) {
    names(out)[names(out) == "p"] <- pcol
  }
  out
}
