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
#'
#' Writes adjusted values into `pvalue` (used by canvas / power) and preserves
#' the unadjusted values in `pvalue_raw` on confirmatory edges and matching
#' estimate rows so results tables can show both.
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
  if (!"pvalue_raw" %in% names(state$edges)) {
    state$edges$pvalue_raw <- NA_real_
  }
  state$edges$pvalue_raw[conf_idx] <- raw
  state$edges$pvalue[conf_idx] <- adj

  # Mirror onto estimate rows matched by edge endpoints when possible
  if (!is.null(estimates) && nrow(estimates) && "pvalue" %in% names(estimates)) {
    if (!"pvalue_raw" %in% names(estimates)) {
      estimates$pvalue_raw <- NA_real_
    }
    for (j in seq_along(conf_idx)) {
      i <- conf_idx[[j]]
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
      if (length(hit)) {
        estimates$pvalue_raw[hit] <- as.numeric(estimates$pvalue[hit])
        estimates$pvalue[hit] <- adj[[j]]
      }
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

#' True if a TeX term fragment begins with a unary minus
#' @keywords internal
eta_tex_is_negative <- function(term) {
  grepl("^\\s*-", as.character(term %||% ""))
}

#' Drop a leading unary minus from a TeX term (for "+ −x" → "− x")
#' @keywords internal
eta_tex_strip_leading_minus <- function(term) {
  sub("^\\s*-\\s*", "", as.character(term %||% ""))
}

#' Join prefix before a term: `" + "` or `" - "` (never `+ -`)
#' @keywords internal
eta_tex_join_op <- function(term) {
  if (eta_tex_is_negative(term)) " - " else " + "
}

#' Paste predictor/coefficient fragments with proper ± joining
#' @keywords internal
join_eta_tex_parts <- function(parts) {
  parts <- as.character(parts %||% character())
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  out <- parts[[1]]
  if (length(parts) == 1L) return(out)
  for (i in 2:length(parts)) {
    op <- eta_tex_join_op(parts[[i]])
    body <- if (identical(op, " - ")) {
      eta_tex_strip_leading_minus(parts[[i]])
    } else {
      parts[[i]]
    }
    out <- paste0(out, op, body)
  }
  out
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
    parts <- c(parts, body)
  }
  join_eta_tex_parts(parts)
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
    parts <- c(parts, body)
  }
  join_eta_tex_parts(parts)
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

#' Build RE distribution TeX + \(u\) map from SDs / correlations
#'
#' Shared by lmer (`VarCorr`) and brms (posterior means of `sd_` / `cor_`).
#' @keywords internal
re_equation_info_from_vc <- function(grp, sds, cors = NULL, singular = FALSE,
                                     digits = 2, alias_map = NULL) {
  nms <- names(sds)
  sds <- as.numeric(sds)
  if (is.null(nms) || length(nms) != length(sds)) {
    nms <- paste0("RE", seq_along(sds))
  }
  if (!length(sds) || !all(is.finite(sds))) return(NULL)
  nms[nms == "Intercept"] <- "(Intercept)"
  names(sds) <- nms

  u_syms <- vapply(seq_along(sds), function(i) {
    sprintf("u_{%dj}", i - 1L)
  }, character(1))
  u_map <- stats::setNames(u_syms, nms)
  # Aliases so fixed-effect names (e.g. Condition__Treat) match RE keys
  alias_map <- alias_map %||% character()
  if (length(alias_map)) {
    for (orig in names(alias_map)) {
      safe <- unname(alias_map[[orig]])
      if (safe %in% names(u_map) && !orig %in% names(u_map)) {
        u_map[[orig]] <- u_map[[safe]]
      }
      if (orig %in% names(u_map) && !safe %in% names(u_map)) {
        u_map[[safe]] <- u_map[[orig]]
      }
    }
  }
  if ("(Intercept)" %in% names(u_map) && !"Intercept" %in% names(u_map)) {
    u_map[["Intercept"]] <- u_map[["(Intercept)"]]
  }

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
    if (is.matrix(cors) && nrow(cors) >= 2L && ncol(cors) >= 2L) {
      cn <- colnames(cors)
      rn <- rownames(cors)
      if (!is.null(cn)) cn[cn == "Intercept"] <- "(Intercept)"
      if (!is.null(rn)) rn[rn == "Intercept"] <- "(Intercept)"
      colnames(cors) <- cn
      rownames(cors) <- rn
      # Align to sd name order when possible
      if (all(nms %in% rownames(cors)) && all(nms %in% colnames(cors))) {
        cors <- cors[nms, nms, drop = FALSE]
      }
      for (i in seq_len(nrow(cors) - 1L)) {
        for (j in (i + 1L):ncol(cors)) {
          rij <- cors[i, j]
          if (!is.finite(rij)) next
          boundary <- isTRUE(singular) || abs(rij) >= 1 - 1e-3
          rho_txt <- sprintf(
            "\\rho_{%d%d}=%s",
            i - 1L, j - 1L,
            formatC(rij, digits = digits, format = "f")
          )
          if (boundary) {
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
    singular = isTRUE(singular)
  )
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
  singular <- isTRUE(tryCatch(
    lme4::isSingular(fit, tol = 1e-4),
    error = function(e) FALSE
  ))
  re_equation_info_from_vc(
    grp = grp, sds = sds, cors = cors, singular = singular, digits = digits
  )
}

#' Random-effects formula fragment from a brmsfit (for equation tags)
#' @keywords internal
brms_re_tag <- function(fit, fallback = "(1|cluster)") {
  if (!inherits(fit, "brmsfit")) return(fallback)
  fml <- tryCatch({
    fr <- fit$formula$formula
    if (is.null(fr)) fr <- stats::formula(fit)
    paste(deparse(fr), collapse = " ")
  }, error = function(e) "")
  if (!nzchar(fml)) return(fallback)
  m <- gregexpr("\\([^()]*\\|[^()]*\\)", fml, perl = TRUE)
  parts <- regmatches(fml, m)[[1]]
  if (!length(parts)) return(fallback)
  parts <- gsub("\\s*\\|\\s*", "|", gsub("\\s+", " ", parts))
  # Prefer SMT dummy names in the tag when we rewrote for brms
  to_safe <- fit$smt_brms_to_safe
  if (length(to_safe)) {
    orgs <- names(to_safe)
    orgs <- orgs[order(nchar(unname(to_safe[orgs])), decreasing = TRUE)]
    for (orig in orgs) {
      safe <- unname(to_safe[[orig]])
      if (!identical(orig, safe)) {
        parts <- gsub(safe, orig, parts, fixed = TRUE)
      }
    }
  }
  paste(parts, collapse = " + ")
}

#' Random-effect \(u\) map + distribution TeX from a brmsfit (first group)
#'
#' Uses posterior means of group-level SDs / correlations from [brms::VarCorr()].
#' @keywords internal
brms_re_equation_info <- function(fit, digits = 2) {
  if (is.null(fit) || !inherits(fit, "brmsfit")) return(NULL)
  if (!requireNamespace("brms", quietly = TRUE)) return(NULL)
  vc <- tryCatch(brms::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc) || !length(vc)) return(NULL)
  grps <- setdiff(names(vc), "residual__")
  if (!length(grps)) return(NULL)
  grp <- grps[[1]]
  block <- vc[[grp]]
  sd_tab <- block$sd
  if (is.null(sd_tab) || !nrow(sd_tab)) return(NULL)
  sds <- stats::setNames(
    as.numeric(sd_tab[, "Estimate"]),
    rownames(sd_tab)
  )
  cors <- NULL
  cor_arr <- block$cor
  if (!is.null(cor_arr) && is.array(cor_arr) && length(dim(cor_arr)) == 3L) {
    # [term, summary, term] — pull Estimate slice
    sum_names <- dimnames(cor_arr)[[2]]
    est_i <- match("Estimate", sum_names)
    if (is.finite(est_i)) {
      cors <- cor_arr[, est_i, , drop = TRUE]
      if (is.null(dim(cors))) {
        cors <- matrix(cors, nrow = dim(cor_arr)[[1]], ncol = dim(cor_arr)[[3]])
        dimnames(cors) <- list(dimnames(cor_arr)[[1]], dimnames(cor_arr)[[3]])
      }
    }
  }
  # Near-zero slope SD ≈ singular / unidentified boundary for display
  singular <- any(is.finite(sds) & sds < 1e-3)
  re_equation_info_from_vc(
    grp = grp,
    sds = sds,
    cors = cors,
    singular = singular,
    digits = digits,
    alias_map = fit$smt_brms_to_safe
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
    } else if (eta_tex_is_negative(eta_lines[[i]])) {
      rows <- c(rows, sprintf(
        "&\\quad - %s",
        eta_tex_strip_leading_minus(eta_lines[[i]])
      ))
    } else {
      rows <- c(rows, sprintf("&\\quad + %s", eta_lines[[i]]))
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

#' Attach random-effect fragments onto an equation wrapper list
#'
#' Works for merMod (lme4) and brmsfit (posterior mean SDs / correlations).
#' @keywords internal
equation_wrapper_add_re <- function(wrap, fit) {
  info <- if (inherits(fit, "brmsfit")) {
    brms_re_equation_info(fit)
  } else {
    mer_re_equation_info(fit)
  }
  if (is.null(info)) return(wrap)
  wrap$re_u_map <- info$u_map
  wrap$re_dist <- info$dist_tex
  wrap$re_group <- info$group
  wrap$re_terms <- NULL
  wrap
}

#' Family / link for a (possibly multivariate) brmsfit view
#'
#' Joint SMT views set `smt_response`; multivariate `fit$family` is then a list
#' of `brmsfamily` objects. Univariate fits keep a single `brmsfamily`.
#' @keywords internal
brms_view_family <- function(fit) {
  fam_obj <- fit$family
  resp <- as.character(fit$smt_response %||% "")[[1]]
  if (is.list(fam_obj) && !inherits(fam_obj, "brmsfamily")) {
    pick <- NULL
    if (!is.na(resp) && nzchar(resp) && !is.null(fam_obj[[resp]])) {
      pick <- fam_obj[[resp]]
    } else if (length(fam_obj)) {
      pick <- fam_obj[[1L]]
    }
    fam_obj <- pick
  }
  fam <- tryCatch(as.character(fam_obj$family %||% "gaussian"), error = function(e) "gaussian")
  link <- tryCatch(as.character(fam_obj$link %||% "identity"), error = function(e) "identity")
  if (!length(fam) || is.na(fam[[1]]) || !nzchar(fam[[1]])) fam <- "gaussian"
  if (!length(link) || is.na(link[[1]]) || !nzchar(link[[1]])) link <- "identity"
  list(family = fam[[1]], link = link[[1]])
}

#' Residual sigma (posterior mean) for a brms equation view
#' @keywords internal
brms_view_sigma <- function(fit) {
  resp <- as.character(fit$smt_response %||% "")[[1]]
  vars <- character()
  if (!is.na(resp) && nzchar(resp)) vars <- c(vars, paste0("sigma_", resp))
  vars <- c(vars, "sigma")
  for (v in vars) {
    s <- tryCatch({
      sm <- brms::posterior_summary(fit, variable = v)
      as.numeric(sm[1L, "Estimate"])
    }, error = function(e) NA_real_)
    if (length(s) == 1L && is.finite(s) && s >= 0) return(s)
  }
  NA_real_
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
  if (inherits(fit, "brmsfit")) {
    fl <- brms_view_family(fit)
    fam <- fl$family
    link <- fl$link
    joint <- identical(as.character(fit$smt_structure %||% "")[[1]], "joint")
    prefix <- if (joint) "brms · joint · " else "brms · "
    re <- brms_re_tag(fit, fallback = "")
    if (!nzchar(re)) {
      info0 <- brms_re_equation_info(fit, digits = 2)
      if (!is.null(info0)) re <- paste0("(1|", info0$group, ")")
    }
    has_re <- nzchar(re)
    re_suffix <- if (has_re) paste0(" · ", re) else ""
    sig <- if (identical(fam, "bernoulli") || identical(fam, "binomial")) {
      NA_real_
    } else {
      brms_view_sigma(fit)
    }
    note <- paste(
      "Posterior mean \\(\\pm\\) posterior SD. Slope priors are the Expected coefficients.",
      if (has_re) {
        "Group-level SDs / correlations are posterior means; \\(j\\) indexes the clustering factor."
      } else {
        NULL
      }
    )
    if ((identical(fam, "bernoulli") || identical(fam, "binomial")) &&
          identical(link, "logit")) {
      wrap <- list(
        tag = paste0(prefix, "bernoulli · logit", re_suffix),
        lhs = sprintf("\\mathrm{P}(%s = 1)", y_tex),
        wrap_left = "\\mathrm{logit}^{-1}(",
        wrap_right = ")",
        note = note,
        generative = FALSE,
        sigma = NA_real_,
        re_terms = NULL,
        re_dist = NULL
      )
      return(equation_wrapper_add_re(wrap, fit))
    }
    wrap <- list(
      tag = if (identical(fam, "bernoulli") || identical(fam, "binomial")) {
        paste0(prefix, fam, " · ", link, re_suffix)
      } else {
        paste0(prefix, "gaussian", re_suffix)
      },
      lhs = y_tex,
      wrap_left = "",
      wrap_right = "",
      note = note,
      generative = identical(link, "identity"),
      sigma = if (length(sig) == 1L) sig else NA_real_,
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

#' Per-latent CFA / measurement summary from a two-step fit result
#'
#' @return `NULL` when no measurement results are available for `latent_id`,
#'   otherwise a list with loadings, alpha, uni, weights, score_method, and
#'   optional joint CFA fit measures.
#' @keywords internal
latent_measurement_summary <- function(result, state, latent_id) {
  if (is.null(result) || is.null(state) || is.null(latent_id)) return(NULL)
  if (!isTRUE(result$two_step) && is.null(result$measurement_fit) &&
        is.null(result$measurement_estimates)) {
    return(NULL)
  }
  latent_id <- as.character(latent_id)[[1]]
  if (!nzchar(latent_id) || !is_latent_node(state, latent_id)) return(NULL)
  lat_nm <- tryCatch(
    lavaan_var_name(state, latent_id),
    error = function(e) NA_character_
  )
  if (is.na(lat_nm) || !nzchar(lat_nm)) return(NULL)

  loadings <- NULL
  est <- result$estimates
  if (!is.null(est) && nrow(est) && all(c("op", "lhs", "rhs") %in% names(est))) {
    hit <- est$op == "=~" & as.character(est$lhs) == lat_nm
    if (any(hit)) {
      loadings <- est[hit, , drop = FALSE]
      loadings <- loadings[order(as.character(loadings$rhs)), , drop = FALSE]
    }
  }
  if (is.null(loadings) || !nrow(loadings)) {
    mest <- result$measurement_estimates
    if (!is.null(mest) && nrow(mest) && all(c("op", "lhs") %in% names(mest))) {
      hit <- mest$op == "=~" & as.character(mest$lhs) == lat_nm
      if (any(hit)) {
        loadings <- mest[hit, , drop = FALSE]
        loadings <- loadings[order(as.character(loadings$rhs)), , drop = FALSE]
      }
    }
  }

  alpha <- NA_real_
  cronbach <- result$measurement_cronbach_alpha
  if (!is.null(cronbach) && length(cronbach)) {
    if (lat_nm %in% names(cronbach)) {
      alpha <- as.numeric(cronbach[[lat_nm]])
    } else if (latent_id %in% names(cronbach)) {
      alpha <- as.numeric(cronbach[[latent_id]])
    }
  }

  uni <- NULL
  uni_map <- result$measurement_unidimensionality
  if (is.list(uni_map) && length(uni_map)) {
    if (latent_id %in% names(uni_map)) {
      uni <- uni_map[[latent_id]]
    } else {
      for (uid in names(uni_map)) {
        slot <- uni_map[[uid]]
        if (!is.list(slot)) next
        lv_nm <- as.character(slot$lavaan_name %||% "")
        if (identical(lv_nm, lat_nm) || identical(uid, lat_nm)) {
          uni <- slot
          break
        }
      }
    }
  }
  if (is.null(uni) && nrow(state$nodes)) {
    idx <- match(latent_id, state$nodes$id)
    if (!is.na(idx) && "uni_status" %in% names(state$nodes)) {
      ust <- as.character(state$nodes$uni_status[[idx]] %||% NA_character_)
      if (!is.na(ust) && nzchar(ust)) {
        uni <- list(
          status = ust,
          label = as.character(state$nodes$uni_label[[idx]] %||% ""),
          ratio = suppressWarnings(as.numeric(state$nodes$uni_ratio[[idx]])),
          pct1 = suppressWarnings(as.numeric(state$nodes$uni_pct1[[idx]]))
        )
      }
    }
  }

  weights <- NULL
  w_list <- result$measurement_score_weights
  if (is.null(w_list) || !length(w_list)) {
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
  if (!is.null(w_list) && length(w_list)) {
    if (lat_nm %in% names(w_list)) {
      weights <- w_list[[lat_nm]]
    } else if (latent_id %in% names(w_list)) {
      weights <- w_list[[latent_id]]
    }
  }

  fit_meas <- NULL
  mfit <- result$measurement_fit
  if (inherits(mfit, "lavaan")) {
    want <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
    fm <- tryCatch(lavaan::fitMeasures(mfit, want), error = function(e) NULL)
    if (!is.null(fm)) {
      fit_meas <- data.frame(
        measure = names(fm),
        value = as.numeric(fm),
        stringsAsFactors = FALSE
      )
      fit_meas <- fit_meas[is.finite(fit_meas$value), , drop = FALSE]
    }
  }

  score_method <- normalize_latent_score_method(
    result$latent_score_method %||% "own",
    state
  )

  if (is.null(loadings) && !is.finite(alpha) && is.null(uni) && is.null(weights)) {
    return(NULL)
  }
  list(
    latent_id = latent_id,
    lavaan_name = lat_nm,
    display = node_display_name(state, latent_id),
    loadings = loadings,
    alpha = alpha,
    uni = uni,
    weights = weights,
    score_method = score_method,
    fit_measures = fit_meas
  )
}

#' Compact HTML table of CFA loadings for one latent
#' @keywords internal
format_latent_loadings_html <- function(loadings, state = NULL) {
  if (is.null(loadings) || !nrow(loadings)) return(NULL)
  rows <- lapply(seq_len(nrow(loadings)), function(i) {
    rhs <- as.character(loadings$rhs[[i]])
    ind <- if (!is.null(state)) {
      tryCatch(display_name_from_lavaan(state, rhs), error = function(e) rhs)
    } else {
      rhs
    }
    est <- suppressWarnings(as.numeric(loadings$est[[i]]))
    se <- suppressWarnings(as.numeric(loadings$se[[i]]))
    pv <- suppressWarnings(as.numeric(loadings$pvalue[[i]]))
    std <- if ("est_std" %in% names(loadings)) {
      suppressWarnings(as.numeric(loadings$est_std[[i]]))
    } else {
      NA_real_
    }
    sprintf(
      paste0(
        "<tr>",
        "<td>%s</td>",
        "<td class=\"smt-num\">%s</td>",
        "<td class=\"smt-num\">%s</td>",
        "<td class=\"smt-num\">%s</td>",
        "<td class=\"smt-num\">%s</td>",
        "</tr>"
      ),
      htmltools::htmlEscape(ind),
      if (is.finite(est)) formatC(est, digits = 3, format = "f") else "—",
      if (is.finite(se)) formatC(se, digits = 3, format = "f") else "—",
      if (is.finite(std)) formatC(std, digits = 3, format = "f") else "—",
      if (is.finite(pv)) format_pvalue(pv, digits = 3) else "—"
    )
  })
  paste0(
    "<table class=\"smt-cfa-table\">",
    "<thead><tr>",
    "<th>Indicator</th>",
    "<th class=\"smt-num\">Loading</th>",
    "<th class=\"smt-num\">SE</th>",
    "<th class=\"smt-num\">Std</th>",
    "<th class=\"smt-num\">p</th>",
    "</tr></thead><tbody>",
    paste(rows, collapse = ""),
    "</tbody></table>"
  )
}

#' Loadings table as a data frame for Report / export
#' @keywords internal
latent_loadings_df <- function(loadings, state = NULL) {
  if (is.null(loadings) || !nrow(loadings)) {
    return(data.frame(
      Indicator = character(), Loading = numeric(), SE = numeric(),
      Std = numeric(), p = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Indicator = vapply(seq_len(nrow(loadings)), function(i) {
      rhs <- as.character(loadings$rhs[[i]])
      if (!is.null(state)) {
        tryCatch(display_name_from_lavaan(state, rhs), error = function(e) rhs)
      } else {
        rhs
      }
    }, character(1)),
    Loading = round(suppressWarnings(as.numeric(loadings$est)), 3),
    SE = round(suppressWarnings(as.numeric(loadings$se)), 3),
    Std = if ("est_std" %in% names(loadings)) {
      round(suppressWarnings(as.numeric(loadings$est_std)), 3)
    } else {
      rep(NA_real_, nrow(loadings))
    },
    p = vapply(seq_len(nrow(loadings)), function(i) {
      pv <- suppressWarnings(as.numeric(loadings$pvalue[[i]]))
      if (is.finite(pv)) format_pvalue(pv, digits = 3) else "—"
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

#' PCA eigenvalue table as a data frame for Report / export
#' @keywords internal
latent_pca_eigen_df <- function(uni) {
  empty <- data.frame(
    Component = character(), Eigenvalue = numeric(),
    PctVar = numeric(), stringsAsFactors = FALSE
  )
  if (!is.list(uni)) return(empty)
  ev <- suppressWarnings(as.numeric(uni$eigenvalues))
  ev <- ev[is.finite(ev)]
  if (!length(ev)) {
    lam1 <- suppressWarnings(as.numeric(uni$lambda1))
    lam2 <- suppressWarnings(as.numeric(uni$lambda2))
    ev <- c(lam1, lam2)
    ev <- ev[is.finite(ev)]
  }
  if (!length(ev)) return(empty)
  tot <- sum(ev)
  data.frame(
    Component = paste0("PC", seq_along(ev)),
    Eigenvalue = round(ev, 3),
    PctVar = if (is.finite(tot) && tot > 0) round(100 * ev / tot, 1) else rep(NA_real_, length(ev)),
    stringsAsFactors = FALSE
  )
}

#' PCA item×component loadings as a data frame for Report / export
#'
#' @param which `"unrotated"` (PC*) or `"rotated"` (varimax RC*).
#' @keywords internal
latent_pca_loadings_df <- function(uni, state = NULL,
                                   which = c("unrotated", "rotated")) {
  which <- match.arg(which)
  empty <- data.frame(Item = character(), stringsAsFactors = FALSE)
  if (!is.list(uni)) return(empty)
  L <- if (identical(which, "rotated")) uni$loadings_rotated else uni$loadings
  if (is.null(L) || !is.matrix(L) || !nrow(L) || !ncol(L)) return(empty)
  items <- rownames(L)
  if (is.null(items) || length(items) != nrow(L)) {
    items <- paste0("item", seq_len(nrow(L)))
  }
  disp <- vapply(items, function(nm) {
    if (!is.null(state)) {
      tryCatch(display_name_from_lavaan(state, nm), error = function(e) nm)
    } else {
      nm
    }
  }, character(1))
  out <- data.frame(Item = unname(disp), stringsAsFactors = FALSE)
  prefix <- if (identical(which, "rotated")) "RC" else "PC"
  for (j in seq_len(ncol(L))) {
    pc <- colnames(L)[[j]]
    if (is.null(pc) || !nzchar(pc)) pc <- paste0(prefix, j)
    out[[pc]] <- round(as.numeric(L[, j]), 3)
  }
  out
}

#' Inspector / report block: CFA loadings, alpha, score, joint fit
#' @keywords internal
latent_cfa_block_ui <- function(result, state, latent_id) {
  sm <- latent_measurement_summary(result, state, latent_id)
  if (is.null(sm)) {
    return(shiny::helpText(
      "Run the analysis to see CFA loadings for this latent."
    ))
  }

  chips <- list()
  if (is.finite(sm$alpha)) {
    chips[[length(chips) + 1L]] <- shiny::span(
      class = "smt-cfa-chip",
      sprintf("Cronbach \u03B1 = %.2f", sm$alpha)
    )
  }
  score_tag <- if (identical(sm$score_method, "joint")) {
    "score: joint Thomson"
  } else {
    "score: own indicators"
  }
  chips[[length(chips) + 1L]] <- shiny::span(class = "smt-cfa-chip", score_tag)

  load_html <- format_latent_loadings_html(sm$loadings, state)
  score_html <- NULL
  if (!is.null(sm$weights) && length(sm$weights)) {
    ww <- sm$weights
    ww <- ww[is.finite(ww)]
    if (length(ww)) {
      lat_tex <- latex_var(sm$display)
      parts <- character()
      for (j in seq_along(ww)) {
        ind_tex <- latex_var(
          display_name_from_lavaan(state, names(ww)[[j]])
        )
        piece <- formatC(unname(ww[[j]]), digits = 2, format = "f")
        parts <- c(parts, paste0(piece, " \\cdot ", ind_tex))
      }
      score_html <- shiny::HTML(paste0(
        "<div class=\"smt-eq-tag\">factor score (", htmltools::htmlEscape(
          if (identical(sm$score_method, "joint")) "joint Thomson" else "own indicators"
        ), ")</div>",
        smt_eq_math_line(sprintf(
          "\\widehat{%s} = %s",
          lat_tex,
          join_eta_tex_parts(parts)
        ))
      ))
    }
  }

  fit_bits <- NULL
  if (!is.null(sm$fit_measures) && nrow(sm$fit_measures)) {
    pick <- function(nm) {
      hit <- match(nm, sm$fit_measures$measure)
      if (is.na(hit)) return(NA_real_)
      as.numeric(sm$fit_measures$value[[hit]])
    }
    bits <- character()
    chisq <- pick("chisq"); df <- pick("df"); pv <- pick("pvalue")
    if (is.finite(chisq) && is.finite(df)) {
      bits <- c(bits, sprintf(
        "\u03C7\u00B2(%s) = %s%s",
        formatC(df, digits = 0, format = "f"),
        formatC(chisq, digits = 2, format = "f"),
        if (is.finite(pv)) paste0(", p = ", format_pvalue(pv, digits = 3)) else ""
      ))
    }
    for (nm in c("cfi", "tli", "rmsea", "srmr")) {
      v <- pick(nm)
      if (is.finite(v)) {
        bits <- c(bits, sprintf("%s = %s", toupper(nm), formatC(v, digits = 3, format = "f")))
      }
    }
    if (length(bits)) {
      fit_bits <- shiny::div(
        class = "smt-cfa-fitmeas",
        shiny::div(class = "smt-cfa-subtitle", "Joint CFA fit"),
        shiny::div(paste(bits, collapse = " · "))
      )
    }
  }

  shiny::withMathJax(shiny::div(
    class = "smt-cfa-inspector",
    if (length(chips)) shiny::div(class = "smt-cfa-chips", chips),
    if (!is.null(load_html)) {
      shiny::tagList(
        shiny::div(class = "smt-cfa-subtitle", "Loadings"),
        shiny::HTML(load_html)
      )
    },
    if (!is.null(score_html)) {
      shiny::tagList(
        shiny::div(class = "smt-cfa-subtitle", "Path-stage score"),
        shiny::div(class = "smt-eq-block", score_html)
      )
    },
    fit_bits,
    shiny::helpText(
      "Loadings come from the confirmatory CFA on this latent's indicators. ",
      "Joint CFA fit indices describe the full measurement model."
    )
  ))
}

#' Inspector / report block: PCA unidimensionality for one latent
#' @keywords internal
latent_pca_block_ui <- function(result, state, latent_id) {
  sm <- latent_measurement_summary(result, state, latent_id)
  pca <- format_latent_pca_ui(sm$uni, state = state)
  if (is.null(pca)) {
    return(shiny::helpText(
      "Run the analysis to see the PCA unidimensionality screen for this latent."
    ))
  }
  shiny::div(class = "smt-cfa-inspector", pca)
}

#' @keywords internal
#' @rdname latent_cfa_block_ui
latent_cfa_inspector_ui <- function(result, state, latent_id) {
  # Compatibility wrapper: both blocks stacked (tests / older callers)
  shiny::tagList(
    latent_cfa_block_ui(result, state, latent_id),
    latent_pca_block_ui(result, state, latent_id)
  )
}

#' HTML table of PCA item loadings (items × components)
#' @keywords internal
format_latent_pca_loadings_html <- function(uni, state = NULL,
                                            which = c("unrotated", "rotated")) {
  which <- match.arg(which)
  tab <- latent_pca_loadings_df(uni, state = state, which = which)
  if (!nrow(tab) || ncol(tab) < 2L) return(NULL)
  pc_cols <- setdiff(names(tab), "Item")
  head_cells <- paste0(
    "<th>Item</th>",
    paste(sprintf("<th class=\"smt-num\">%s</th>", htmltools::htmlEscape(pc_cols)), collapse = "")
  )
  rows <- lapply(seq_len(nrow(tab)), function(i) {
    cells <- sprintf(
      "<td>%s</td>",
      htmltools::htmlEscape(as.character(tab$Item[[i]]))
    )
    for (pc in pc_cols) {
      v <- suppressWarnings(as.numeric(tab[[pc]][[i]]))
      cells <- paste0(
        cells,
        sprintf(
          "<td class=\"smt-num\">%s</td>",
          if (is.finite(v)) formatC(v, digits = 3, format = "f") else "—"
        )
      )
    }
    paste0("<tr>", cells, "</tr>")
  })
  paste0(
    "<table class=\"smt-cfa-table smt-cfa-pca-table\">",
    "<thead><tr>", head_cells, "</tr></thead><tbody>",
    paste(rows, collapse = ""),
    "</tbody></table>"
  )
}

#' Inspector block: PCA unidimensionality screen for one latent
#' @keywords internal
format_latent_pca_ui <- function(uni, state = NULL) {
  if (!is.list(uni)) return(NULL)
  ust <- as.character(uni$status %||% "")
  if (!nzchar(ust) || is.na(ust)) return(NULL)

  ulab <- as.character(uni$label %||% ust)
  ratio <- suppressWarnings(as.numeric(uni$ratio))
  pct1 <- suppressWarnings(as.numeric(uni$pct1))
  lam1 <- suppressWarnings(as.numeric(uni$lambda1))
  lam2 <- suppressWarnings(as.numeric(uni$lambda2))
  if (!is.finite(lam1) && is.finite(ratio) && is.finite(lam2)) {
    lam1 <- ratio * lam2
  }
  if (!is.finite(lam2) && is.finite(ratio) && is.finite(lam1) && ratio > 0) {
    lam2 <- lam1 / ratio
  }
  n_items <- suppressWarnings(as.integer(uni$n_items))
  n_obs <- suppressWarnings(as.integer(uni$n))
  ev <- suppressWarnings(as.numeric(uni$eigenvalues))
  ev <- ev[is.finite(ev)]
  if (!length(ev)) {
    ev <- c(lam1, lam2)
    ev <- ev[is.finite(ev)]
  }

  chip_class <- paste(
    "smt-cfa-chip",
    if (identical(ust, "ok")) "smt-cfa-chip-ok"
    else if (identical(ust, "warn")) "smt-cfa-chip-warn"
    else if (identical(ust, "fail")) "smt-cfa-chip-fail"
    else NULL
  )
  status_chip <- shiny::span(
    class = chip_class,
    if (nzchar(ulab)) ulab else ust
  )

  metric_bits <- character()
  if (is.finite(lam1)) {
    metric_bits <- c(metric_bits, sprintf(
      "\u03BB\u2081 = %s", formatC(lam1, digits = 2, format = "f")
    ))
  }
  if (is.finite(lam2)) {
    metric_bits <- c(metric_bits, sprintf(
      "\u03BB\u2082 = %s", formatC(lam2, digits = 2, format = "f")
    ))
  }
  if (is.finite(ratio)) {
    metric_bits <- c(metric_bits, sprintf(
      "\u03BB\u2081/\u03BB\u2082 = %s", formatC(ratio, digits = 2, format = "f")
    ))
  }
  if (is.finite(pct1)) {
    metric_bits <- c(metric_bits, sprintf(
      "PC1 = %s%%", formatC(100 * pct1, digits = 1, format = "f")
    ))
  }
  if (is.finite(n_items) && n_items > 0L) {
    metric_bits <- c(metric_bits, sprintf("%d items", n_items))
  }
  if (is.finite(n_obs) && n_obs > 0L) {
    metric_bits <- c(metric_bits, sprintf("n = %d", n_obs))
  }

  ev_html <- NULL
  if (length(ev)) {
    tot <- sum(ev)
    rows <- lapply(seq_along(ev), function(i) {
      pct <- if (is.finite(tot) && tot > 0) 100 * ev[[i]] / tot else NA_real_
      sprintf(
        paste0(
          "<tr>",
          "<td>PC%s</td>",
          "<td class=\"smt-num\">%s</td>",
          "<td class=\"smt-num\">%s</td>",
          "</tr>"
        ),
        i,
        formatC(ev[[i]], digits = 3, format = "f"),
        if (is.finite(pct)) paste0(formatC(pct, digits = 1, format = "f"), "%") else "—"
      )
    })
    ev_html <- paste0(
      "<table class=\"smt-cfa-table smt-cfa-pca-table\">",
      "<thead><tr>",
      "<th>Component</th>",
      "<th class=\"smt-num\">Eigenvalue</th>",
      "<th class=\"smt-num\">% var</th>",
      "</tr></thead><tbody>",
      paste(rows, collapse = ""),
      "</tbody></table>"
    )
  }

  load_html <- format_latent_pca_loadings_html(
    uni, state = state, which = "unrotated"
  )
  rot_html <- format_latent_pca_loadings_html(
    uni, state = state, which = "rotated"
  )
  n_rot <- suppressWarnings(as.integer(uni$n_rotated %||% ncol(uni$loadings_rotated %||% NULL)))

  rule_txt <- switch(
    ust,
    ok = "Ratio \u2265 3: essentially unidimensional.",
    warn = "Ratio 2\u20133: weak / borderline unidimensionality.",
    fail = "Ratio < 2: possible second dimension among these indicators.",
    "Correlation-matrix PCA on this latent's indicators."
  )

  shiny::tagList(
    shiny::div(class = "smt-cfa-chips", status_chip),
    if (length(metric_bits)) {
      shiny::div(class = "smt-cfa-fitmeas", paste(metric_bits, collapse = " · "))
    },
    if (!is.null(ev_html)) {
      shiny::tagList(
        shiny::div(class = "smt-cfa-subtitle", "Eigenvalues"),
        shiny::HTML(ev_html)
      )
    },
    if (!is.null(load_html)) {
      shiny::tagList(
        shiny::div(class = "smt-cfa-subtitle", "Item loadings (unrotated)"),
        shiny::HTML(load_html)
      )
    },
    if (!is.null(rot_html)) {
      shiny::tagList(
        shiny::div(
          class = "smt-cfa-subtitle",
          if (is.finite(n_rot) && n_rot > 0L) {
            sprintf("Item loadings (varimax, %d component%s)", n_rot, if (n_rot == 1L) "" else "s")
          } else {
            "Item loadings (varimax)"
          }
        ),
        shiny::HTML(rot_html)
      )
    },
    shiny::helpText(
      rule_txt,
      " Unrotated loadings are correlation-PCA (eigenvector \u00D7 \u221A\u03BB);",
      " PC1 often loads all items. Varimax is shown only when at least two",
      " components have \u03BB > 1 (Kaiser)."
    )
  )
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
        parts <- c(parts, body)
      }
      weight_tex <- join_eta_tex_parts(parts)
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
          weight_tex
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
  caption <- if (identical(result$sampler, "brms") ||
                   identical(result$pvalue_kind, "hdi_rope") ||
                   identical(result$pvalue_kind, "credible_interval") ||
                   identical(result$pvalue_kind, "posterior_tail")) {
    paste0(
      "<div class=\"smt-eq-caption\">Bayesian model ",
      "(posterior mean\\(^{\\pm\\mathrm{SD}}\\); HDI + ROPE)</div>"
    )
  } else {
    paste0(
      "<div class=\"smt-eq-caption\">Best model ",
      "(estimate\\(^{\\pm\\mathrm{SE}}\\))</div>"
    )
  }

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
              V <- path_vcov(fit)
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
          V <- path_vcov(fit)
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

#' Convert MathJax equation HTML to a LaTeX body fragment
#' @keywords internal
smt_eq_html_to_latex <- function(html) {
  s <- as.character(html %||% "")
  if (!nzchar(s)) return(NULL)
  s <- gsub("&lt;", "<", s, fixed = TRUE)
  s <- gsub("&gt;", ">", s, fixed = TRUE)
  s <- gsub("&amp;", "&", s, fixed = TRUE)
  s <- gsub("&quot;", "\"", s, fixed = TRUE)
  s <- gsub("&#39;", "'", s, fixed = TRUE)
  s <- gsub("&#x2713;", "$\\\\checkmark$", s, fixed = TRUE)
  s <- gsub("\u2713", "$\\\\checkmark$", s, fixed = TRUE)
  s <- gsub("\u2019", "'", s, fixed = TRUE)
  s <- gsub("\u2013", "--", s, fixed = TRUE)
  s <- gsub("\u2014", "---", s, fixed = TRUE)
  s <- gsub("\u00d7", "$\\\\times$", s, fixed = TRUE)
  s <- gsub("\u2265", "$\\\\ge$", s, fixed = TRUE)
  s <- gsub("\u03b1", "$\\\\alpha$", s, fixed = TRUE)
  s <- gsub("\u03b5", "$\\\\varepsilon$", s, fixed = TRUE)
  s <- gsub("\u03a3", "$\\\\Sigma$", s, fixed = TRUE)
  # MathJax \\color{#rrggbb}{...} → xcolor \\textcolor[HTML]{rrggbb}{...}
  s <- gsub(
    "\\\\color\\{#([0-9A-Fa-f]{6})\\}\\{",
    "\\\\textcolor[HTML]{\\1}{",
    s,
    perl = TRUE
  )
  s <- gsub(
    "<div class=\"smt-eq-caption\">(.*?)</div>",
    "\\\\subsubsection*{\\1}\n",
    s,
    perl = TRUE
  )
  s <- gsub(
    "<div class=\"smt-eq-tag\">(.*?)</div>",
    "\\\\textit{\\1}\\\\par\n",
    s,
    perl = TRUE
  )
  s <- gsub(
    "<div class=\"smt-eq-line\">(.*?)</div>",
    "\\1\n",
    s,
    perl = TRUE
  )
  s <- gsub(
    "<div class=\"smt-eq-note\">(.*?)</div>",
    "\\\\begin{quote}\\\\footnotesize \\1\\\\end{quote}\n",
    s,
    perl = TRUE
  )
  s <- gsub("<div class=\"smt-eq-block\">", "\n", s, fixed = TRUE)
  s <- gsub("<div class=\"smt-equations\">", "", s, fixed = TRUE)
  s <- gsub("</div>", "\n", s, fixed = TRUE)
  s <- gsub("<[^>]+>", "", s)
  s <- gsub("\n{3,}", "\n\n", s)
  trimws(s)
}

#' LaTeX equations for the fitted (best) model
#' @keywords internal
format_best_equations_latex <- function(result, state = NULL) {
  html <- format_best_equations_html(result, state)
  if (is.null(html)) return(NULL)
  smt_eq_html_to_latex(html)
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

#' Empty path-coefficient table schema (optional adjusted p column)
#' @keywords internal
estimates_table_empty <- function(conf_adjust = "none") {
  out <- data.frame(
    Path = character(), Level = character(),
    Estimate = numeric(), SE = numeric(), p = character(),
    stringsAsFactors = FALSE
  )
  pcol <- conf_pvalue_column_name(conf_adjust)
  if (!identical(pcol, "p")) out[[pcol]] <- character()
  out
}

#' Arrow glyph for a path-coef Level (direct / via / total)
#'
#' Direct: ← ; indirect (`via …`): ↜ ; total: ⇐.
#' @keywords internal
path_arrow_glyph <- function(level = "") {
  lv <- as.character(level %||% "")
  if (startsWith(lv, "via ")) return("\u219c")
  if (identical(lv, "total")) return("\u21d0")
  "\u2190"
}

#' Display label for a directed path: `Y ← X` (via: ↜, total: ⇐)
#' @keywords internal
path_arrow_label <- function(to, from, level = "") {
  to <- as.character(to %||% "")
  from <- as.character(from %||% "")
  level <- as.character(level %||% "")
  n <- max(length(to), length(from), length(level), 1L)
  to <- rep_len(to, n)
  from <- rep_len(from, n)
  level <- rep_len(level, n)
  glyph <- vapply(level, path_arrow_glyph, character(1))
  paste0(to, " ", glyph, " ", from)
}

#' Split a Path label (`Y ← X` / `Y ↜ X` / `Y ⇐ X`) into to / from
#' @keywords internal
path_arrow_split <- function(path) {
  path <- as.character(path %||% "")
  if (!length(path)) {
    return(list(to = character(), from = character()))
  }
  # ASCII <- or unicode ← / ↜ / ⇐
  parts <- strsplit(path, "\u2190|\u219c|\u21d0|<-", perl = TRUE)
  to <- vapply(parts, function(p) trimws(p[[1]] %||% ""), character(1))
  from <- vapply(parts, function(p) {
    if (length(p) < 2L) return("")
    trimws(paste(p[-1L], collapse = "\u2190"))
  }, character(1))
  list(to = to, from = from)
}

#' Put path-coef identity columns in display order: Path, Level, …
#' @keywords internal
estimates_table_order_cols <- function(tab) {
  if (is.null(tab) || !ncol(tab)) return(tab)
  prefer <- c("Path", "To", "From", "Level", "Estimate", "SE", "SD", "Σw", "p")
  rest <- setdiff(names(tab), prefer)
  tab[, c(intersect(prefer, names(tab)), rest), drop = FALSE]
}

#' Collapse To/From into a single Path column (`Y ← X` / via ↜ / total ⇐)
#' @keywords internal
estimates_table_collapse_path <- function(tab) {
  if (is.null(tab) || !ncol(tab)) return(tab)
  if ("Path" %in% names(tab) && !("To" %in% names(tab) && "From" %in% names(tab))) {
    return(estimates_table_order_cols(tab))
  }
  if (!("To" %in% names(tab) && "From" %in% names(tab))) {
    return(estimates_table_order_cols(tab))
  }
  level <- if ("Level" %in% names(tab)) tab$Level else ""
  tab$Path <- path_arrow_label(tab$To, tab$From, level)
  tab$To <- NULL
  tab$From <- NULL
  estimates_table_order_cols(tab)
}

#' Σw (sum of Akaike/BIC weights) for path-coef rows when search result present
#'
#' Blank for derived via/total rows. Direct slopes look up `result$edge_weights`
#' by matching From/To to graph edges.
#' @keywords internal
estimates_table_weight_sum_col <- function(state, result, to_lab, from_lab, level) {
  wsums <- result$edge_weights %||% NULL
  if (is.null(wsums) || !length(wsums)) return(NULL)
  if (is.null(result$comparison)) return(NULL)
  n <- length(to_lab)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lv <- as.character(level[[i]] %||% "")
    if (nzchar(lv) && (startsWith(lv, "via ") || identical(lv, "total"))) next
    from_id <- mediation_match_node_id(state, from_lab[[i]])
    to_id <- mediation_match_node_id(state, to_lab[[i]])
    if (is.na(from_id) || is.na(to_id) || is.null(state) || !nrow(state$edges)) next
    hit <- which(state$edges$from == from_id & state$edges$to == to_id)
    if (!length(hit)) next
    # Prefer blank-level structural edge when several share endpoints
    if ("level" %in% names(state$edges) && length(hit) > 1L) {
      elv <- as.character(state$edges$level[hit])
      blank <- which(is.na(elv) | !nzchar(elv))
      if (length(blank)) hit <- hit[blank]
    }
    eid <- as.character(state$edges$id[[hit[[1]]]])
    if (eid %in% names(wsums)) out[[i]] <- as.numeric(wsums[[eid]])
  }
  out
}

#' Format Σw column values for display
#' @keywords internal
format_weight_sum_col <- function(ws) {
  vapply(ws, function(w) {
    if (!is.finite(w)) return("")
    sprintf("%.2f", w)
  }, character(1))
}

#' Sort path-coef rows: To → From → Level (direct, factor, via, total)
#' @keywords internal
estimates_table_order_rows <- function(tab) {
  if (is.null(tab) || !nrow(tab)) return(tab)
  if ("To" %in% names(tab) && "From" %in% names(tab)) {
    to <- as.character(tab$To %||% "")
    from <- as.character(tab$From %||% "")
  } else {
    sp <- path_arrow_split(tab$Path)
    to <- sp$to
    from <- sp$from
  }
  lv <- as.character(tab$Level %||% "")
  lv[is.na(lv)] <- ""
  lv_rank <- ifelse(
    !nzchar(lv), 0L,
    ifelse(startsWith(lv, "via "), 2L,
           ifelse(lv == "total", 3L, 1L))
  )
  tab[order(to, from, lv_rank, lv, method = "radix"), , drop = FALSE]
}

#' Display / lavaan LHS labels that identify one DV in coef tables
#' @keywords internal
dv_coef_to_labels <- function(state, dv_id) {
  dv_id <- as.character(dv_id)[[1]]
  lav <- tryCatch(lavaan_var_name(state, dv_id), error = function(e) dv_id)
  disp <- tryCatch(node_display_name(state, dv_id), error = function(e) dv_id)
  tr <- node_dv_transform(state, dv_id)
  unique(c(
    lav,
    disp,
    dv_id,
    dv_transform_lhs(lav, tr),
    dv_transform_lhs(disp, tr)
  ))
}

#' Build the path-coefficient table for Results / Report (optional DV scope)
#' @keywords internal
estimates_table_for_report <- function(state, result, dv_id = NULL) {
  est <- if (!is.null(result) && !isTRUE(result$model_averaged)) {
    result$estimates
  } else {
    NULL
  }
  tab <- estimates_table(
    state,
    estimates = est,
    conf_adjust = result$conf_adjust %||% "none",
    result = result
  )
  dv_id <- as.character(dv_id %||% "")[[1]]
  if (nzchar(dv_id)) {
    tab <- filter_estimates_table_for_dv(tab, state, dv_id)
  }
  tab
}

#' Keep path-coefficient rows for one structural DV
#' @keywords internal
filter_estimates_table_for_dv <- function(tab, state, dv_id) {
  if (is.null(tab) || !nrow(tab) || is.null(dv_id)) {
    return(tab[0, , drop = FALSE])
  }
  labels <- dv_coef_to_labels(state, dv_id)
  if ("To" %in% names(tab) && "From" %in% names(tab)) {
    to <- as.character(tab$To %||% "")
    from <- as.character(tab$From %||% "")
  } else {
    sp <- path_arrow_split(tab$Path)
    to <- sp$to
    from <- sp$from
  }
  level <- as.character(tab$Level %||% "")
  is_to <- to %in% labels
  for (lab in labels) {
    if (!nzchar(lab)) next
    is_to <- is_to | startsWith(to, paste0(lab, "["))
  }
  is_derived <- startsWith(level, "via ") | identical(level, "total")
  keep <- is_to | (is_derived & (from %in% labels | is_to))
  tab[keep, , drop = FALSE]
}

#' Syntax lines / blocks relevant to one structural DV
#' @keywords internal
filter_result_syntax_for_dv <- function(syntax, state, dv_id) {
  syntax <- as.character(syntax %||% "")[[1]]
  if (!nzchar(syntax) || is.null(dv_id)) return("")
  labels <- dv_coef_to_labels(state, dv_id)
  labels <- labels[nzchar(labels)]
  if (!length(labels)) return("")

  lines <- strsplit(syntax, "\n", fixed = TRUE)[[1]]
  out <- character()
  in_measurement <- FALSE
  kept_eq <- FALSE
  for (ln in lines) {
    trim <- sub("^\\s+", "", ln)
    if (grepl("^#\\s*measurement\\b", trim, ignore.case = TRUE)) {
      in_measurement <- TRUE
      next
    }
    if (grepl("^#\\s*path\\b", trim, ignore.case = TRUE)) {
      in_measurement <- FALSE
      out <- c(out, ln)
      next
    }
    if (in_measurement) next
    # Drop standalone measurement equations outside a marked block
    if (grepl("^[A-Za-z][A-Za-z0-9._]*\\s*=~", trim)) next

    is_header <- startsWith(trim, "#")
    if (is_header) {
      out <- c(out, ln)
      next
    }
    if (!nzchar(trim)) {
      out <- c(out, ln)
      next
    }

    lhs <- NA_character_
    if (grepl("^(brms|glmer|glm|lmer|lm)\\b", trim)) {
      # lm/glm/...: LHS ~ ...  or  brms(...): LHS ~ ...
      rhs_side <- sub("^[^:]+:\\s*", "", trim)
      lhs <- sub("\\s*[~\\[].*$", "", rhs_side)
    } else if (grepl("~", trim, fixed = TRUE) &&
                 !grepl("^(prior|warning)\\b", trim, ignore.case = TRUE)) {
      # joint brms formula line: LHS ~ ...
      lhs <- sub("\\s*~.*$", "", trim)
    }
    if (!is.na(lhs) && nzchar(lhs) && lhs %in% labels) {
      out <- c(out, ln)
      kept_eq <- TRUE
    }
  }
  if (!kept_eq) return("")
  paste(out, collapse = "\n")
}

#' Pick raw vs stored p for display (`pvalue_raw` when finite)
#' @keywords internal
coef_raw_pvalue <- function(pvalue, pvalue_raw = NULL) {
  pvalue <- as.numeric(pvalue)
  if (is.null(pvalue_raw)) return(pvalue)
  raw <- as.numeric(pvalue_raw)
  ifelse(is.finite(raw), raw, pvalue)
}

#' Whether estimate rows correspond to confirmatory adjusted paths
#'
#' True when `pvalue_raw` is finite (set by [apply_confirmatory_p_adjust()]),
#' which marks rows that entered the multiplicity family.
#' @keywords internal
estimate_rows_have_adj_p <- function(est) {
  if (is.null(est) || !nrow(est)) return(logical(0))
  if (!"pvalue_raw" %in% names(est)) return(rep(FALSE, nrow(est)))
  is.finite(as.numeric(est$pvalue_raw))
}

#' Map an estimate lhs/rhs label back to a node id
#' @keywords internal
mediation_match_node_id <- function(state, lab) {
  lab <- as.character(lab %||% "")[[1]]
  if (!nzchar(lab) || is.null(state) || !nrow(state$nodes)) return(NA_character_)
  # Strip split-outcome suffix Y[level]
  core <- sub("\\[.*$", "", lab)
  for (id in state$nodes$id) {
    cands <- unique(c(
      tryCatch(lavaan_var_name(state, id), error = function(e) id),
      tryCatch(node_display_name(state, id), error = function(e) id),
      id,
      tryCatch(
        dv_transform_lhs(lavaan_var_name(state, id), node_dv_transform(state, id)),
        error = function(e) NA_character_
      ),
      tryCatch(
        dv_transform_lhs(node_display_name(state, id), node_dv_transform(state, id)),
        error = function(e) NA_character_
      )
    ))
    cands <- cands[!is.na(cands) & nzchar(cands)]
    if (lab %in% cands || core %in% cands) return(as.character(id))
  }
  NA_character_
}

#' Single-slope structural paths for mediation products
#'
#' Prefers `estimates` (same source as the main coef table) when provided so
#' totals stay consistent with displayed directs under AIC/BIC / averaging.
#' @return data.frame with from, to, from_lab, to_lab, estimate, se, edge_id
#' @keywords internal
mediation_slope_rows <- function(state, estimates = NULL) {
  empty <- data.frame(
    from = character(), to = character(),
    from_lab = character(), to_lab = character(),
    estimate = numeric(), se = numeric(),
    edge_id = character(),
    stringsAsFactors = FALSE
  )
  if (!is.null(estimates) && nrow(estimates)) {
    pe <- estimates[estimates$op == "~", , drop = FALSE]
    if (!nrow(pe)) return(empty)
    if ("kind" %in% names(pe)) {
      pe <- pe[is.na(pe$kind) | as.character(pe$kind) == "effect", , drop = FALSE]
    }
    if ("level" %in% names(pe)) {
      lv <- ifelse(is.na(pe$level), "", as.character(pe$level))
      drop_lv <- lv == "omnibus" | vapply(
        lv,
        function(x) nzchar(x) && isTRUE(tryCatch(is_poly_level(x), error = function(e) FALSE)),
        logical(1)
      )
      pe <- pe[!drop_lv, , drop = FALSE]
    }
    pe <- pe[is.finite(pe$est) & is.finite(pe$se) & pe$se >= 0, , drop = FALSE]
    if (!nrow(pe)) return(empty)
    from_id <- vapply(pe$rhs, function(x) mediation_match_node_id(state, x), character(1))
    to_id <- vapply(pe$lhs, function(x) mediation_match_node_id(state, x), character(1))
    ok <- !is.na(from_id) & !is.na(to_id)
    pe <- pe[ok, , drop = FALSE]
    from_id <- from_id[ok]
    to_id <- to_id[ok]
    if (!nrow(pe)) return(empty)
    edge_id <- character(nrow(pe))
    if (!is.null(state) && nrow(state$edges)) {
      for (i in seq_len(nrow(pe))) {
        hit <- which(state$edges$from == from_id[[i]] & state$edges$to == to_id[[i]])
        edge_id[[i]] <- if (length(hit)) as.character(state$edges$id[[hit[[1]]]]) else NA_character_
      }
    }
    return(data.frame(
      from = from_id, to = to_id,
      from_lab = as.character(pe$rhs),
      to_lab = as.character(pe$lhs),
      estimate = as.numeric(pe$est),
      se = as.numeric(pe$se),
      edge_id = edge_id,
      stringsAsFactors = FALSE
    ))
  }

  e <- mediation_path_edges(state)
  if (!nrow(e)) return(empty)
  data.frame(
    from = as.character(e$from),
    to = as.character(e$to),
    from_lab = vapply(e$from, function(id) node_display_name(state, id), character(1)),
    to_lab = vapply(e$to, function(id) node_display_name(state, id), character(1)),
    estimate = as.numeric(e$estimate),
    se = as.numeric(e$se),
    edge_id = as.character(e$id),
    stringsAsFactors = FALSE
  )
}

#' Structural edges usable for simple mediation products (single slopes)
#' @keywords internal
mediation_path_edges <- function(state) {
  if (is.null(state) || !nrow(state$edges)) {
    return(state$edges[0, , drop = FALSE])
  }
  edges <- ensure_edge_to_port(state)$edges
  if (!"level" %in% names(edges)) edges$level <- NA_character_
  if (!"status" %in% names(edges)) edges$status <- "pending"
  keep <- !edges$type %in% c("definitional", "measurement") &
    !vapply(edges$to, function(id) is_interaction_node(state, id), logical(1)) &
    !vapply(edges$from, function(id) is_interaction_node(state, id), logical(1)) &
    !is.na(edges$estimate) & is.finite(edges$estimate) &
    !is.na(edges$se) & is.finite(edges$se) & edges$se >= 0 &
    (is.na(edges$status) | edges$status != "excluded")
  # Skip omnibus / poly / multi-level categorical (a×b not a single product)
  lv <- ifelse(is.na(edges$level), "", as.character(edges$level))
  drop_lv <- lv == "omnibus" | vapply(
    lv,
    function(x) nzchar(x) && isTRUE(tryCatch(is_poly_level(x), error = function(e) FALSE)),
    logical(1)
  )
  edges[keep & !drop_lv, , drop = FALSE]
}

#' Moments of the product of two independent normals (prior for a×b)
#' @keywords internal
prior_product_moments <- function(m1, s1, m2, s2) {
  m1 <- as.numeric(m1 %||% 0); s1 <- as.numeric(s1 %||% 1)
  m2 <- as.numeric(m2 %||% 0); s2 <- as.numeric(s2 %||% 1)
  if (!is.finite(m1)) m1 <- 0
  if (!is.finite(m2)) m2 <- 0
  if (!is.finite(s1) || s1 <= 0) s1 <- 1
  if (!is.finite(s2) || s2 <= 0) s2 <- 1
  list(
    mean = m1 * m2,
    sd = sqrt(max(0, m1^2 * s2^2 + m2^2 * s1^2 + s1^2 * s2^2))
  )
}

#' Align posterior draw vectors to a common length (independent or joint MCMC)
#' @keywords internal
align_posterior_draws <- function(...) {
  xs <- list(...)
  ns <- vapply(xs, function(x) sum(is.finite(as.numeric(x))), integer(1))
  if (!length(ns) || any(ns < 2L)) return(NULL)
  n <- min(ns)
  lapply(xs, function(x) {
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    x[seq_len(n)]
  })
}

#' Format one classical mediation-derived row
#' @keywords internal
mediation_freq_row <- function(to, from, level, est, se, conf_adjust = "none",
                               weight_sum = NA_real_) {
  pv <- if (is.finite(se) && se > 0 && is.finite(est)) {
    2 * stats::pnorm(-abs(est / se))
  } else {
    NA_real_
  }
  pcol <- conf_pvalue_column_name(conf_adjust)
  row <- data.frame(
    To = to, From = from, Level = level,
    Estimate = round(est, 3),
    SE = round(se, 3),
    p = format_pvalue(pv, digits = 3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (is.finite(weight_sum)) {
    row[["Σw"]] <- format_weight_sum_col(weight_sum)
  }
  if (!identical(pcol, "p")) row[[pcol]] <- ""
  row
}

#' Format one Bayesian mediation-derived row from a draw summary
#' @keywords internal
mediation_bayes_row <- function(to, from, level, sm, ci = 0.95) {
  pct <- round(100 * as.numeric(ci %||% 0.95))
  if (!is.finite(pct)) pct <- 95
  hdi_nm <- sprintf("%d%% HDI", pct)
  cri <- if (is.finite(sm$hdi_low) && is.finite(sm$hdi_high)) {
    sprintf("[%.3f, %.3f]", sm$hdi_low, sm$hdi_high)
  } else {
    ""
  }
  fmt_bf <- function(b) {
    if (is.infinite(b) && b > 0) return("Inf")
    if (is.infinite(b) && b < 0) return("-Inf")
    if (!is.finite(b)) return("")
    sprintf("%+.2f", b)
  }
  d <- data.frame(
    To = to, From = from, Level = level,
    Estimate = round(as.numeric(sm$est), 3),
    SD = round(as.numeric(sm$se), 3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  d[[hdi_nm]] <- cri
  d$pd <- if (is.finite(sm$pd)) sprintf("%.1f%%", 100 * sm$pd) else ""
  d[["ROPE%"]] <- if (is.finite(sm$rope_pct)) {
    sprintf("%.1f%%", 100 * sm$rope_pct)
  } else {
    ""
  }
  d$ROPE <- if (!is.na(sm$rope_decision) && nzchar(as.character(sm$rope_decision))) {
    as.character(sm$rope_decision)
  } else {
    ""
  }
  d[["log BF10"]] <- fmt_bf(sm$log_bf10)
  d[["KL"]] <- format_bayes_kl(sm$kl_prior_post)
  d[["SD\u2193"]] <- format_bayes_sd_narrow(sm$post_narrow)
  d
}

#' Simple mediation derived rows: via M (a×b) and total (direct + indirects)
#'
#' Classical: Sobel SE/p for a×b; total uses an independence approximation
#' `sqrt(SE_direct² + Σ SE_ab²)`. Under AIC/BIC search, via rows carry joint Σw
#' (models containing both constituent edges). With model averaging on, via and
#' total use full model averages of the products (absent → 0).
#' Bayesian: posterior of a×b and of c′ + Σ a×b from MCMC draws.
#' @param result Optional fit result (needed for Bayesian draws / IC weights).
#' @param bayes When TRUE, emit Bayesian columns; otherwise classical SE/p.
#' @keywords internal
mediation_effects_table <- function(state, conf_adjust = "none",
                                    result = NULL, bayes = FALSE,
                                    estimates = NULL) {
  empty <- estimates_table_empty(conf_adjust)
  # Prefer the same coefficient source as the main table (estimates when present)
  if (is.null(estimates) && !is.null(result) && !isTRUE(result$model_averaged)) {
    estimates <- result$estimates %||% result$best_estimates
  }
  e <- mediation_slope_rows(state, estimates = estimates)
  if (nrow(e) < 2L) return(empty)

  chains <- list()
  for (i in seq_len(nrow(e))) {
    for (j in seq_len(nrow(e))) {
      if (i == j) next
      if (!identical(e$to[i], e$from[j])) next
      if (identical(e$from[i], e$to[j])) next
      chains[[length(chains) + 1L]] <- list(
        from = e$from[i], med = e$to[i], to = e$to[j],
        from_lab = e$from_lab[i], med_lab = e$to_lab[i], to_lab = e$to_lab[j],
        edge_a = e$edge_id[i], edge_b = e$edge_id[j],
        a = as.numeric(e$estimate[i]),
        b = as.numeric(e$estimate[j]),
        se_a = as.numeric(e$se[i]),
        se_b = as.numeric(e$se[j])
      )
    }
  }
  if (!length(chains)) return(empty)

  if (isTRUE(bayes)) {
    return(mediation_effects_table_bayes(state, chains, e, result))
  }

  use_avg <- isTRUE(result$model_averaged) &&
    !is.null(result$comparison) &&
    !is.null(result$coef_by_model)
  always_ids <- as.character(result$always %||% character())
  has_ic <- !is.null(result) && !is.null(result$comparison) &&
    !is.null(result$edge_weights)

  rows <- list()
  # Indirects
  for (ch in chains) {
    sw <- NA_real_
    if (use_avg && !is.na(ch$edge_a) && !is.na(ch$edge_b) &&
          nzchar(as.character(ch$edge_a)) && nzchar(as.character(ch$edge_b))) {
      avg <- full_average_mediation_product(
        result$comparison, result$coef_by_model,
        ch$edge_a, ch$edge_b, always_ids = always_ids
      )
      ab <- avg$est
      se_ab <- avg$se
      sw <- avg$weight_sum
    } else {
      ab <- ch$a * ch$b
      se_ab <- sqrt(ch$b^2 * ch$se_a^2 + ch$a^2 * ch$se_b^2)
      if (has_ic && !is.na(ch$edge_a) && !is.na(ch$edge_b) &&
            nzchar(as.character(ch$edge_a)) && nzchar(as.character(ch$edge_b))) {
        sw <- joint_edge_weight_sum(
          result$comparison, c(ch$edge_a, ch$edge_b), always_ids = always_ids
        )
      }
    }
    rows[[length(rows) + 1L]] <- mediation_freq_row(
      to = ch$to_lab,
      from = ch$from_lab,
      level = paste0("via ", ch$med_lab),
      est = ab, se = se_ab, conf_adjust = conf_adjust,
      weight_sum = sw
    )
  }
  # Totals per (X, Y) with at least one mediator
  pairs <- unique(lapply(chains, function(ch) c(ch$from, ch$to)))
  for (pr in pairs) {
    x_id <- pr[[1]]; y_id <- pr[[2]]
    chs <- Filter(function(ch) identical(ch$from, x_id) && identical(ch$to, y_id), chains)
    direct <- e[e$from == x_id & e$to == y_id, , drop = FALSE]
    edge_direct <- if (nrow(direct) && !is.na(direct$edge_id[[1]])) {
      as.character(direct$edge_id[[1]])
    } else {
      NA_character_
    }
    via_pairs <- lapply(chs, function(ch) c(ch$edge_a, ch$edge_b))
    sw <- NA_real_
    if (use_avg) {
      avg <- full_average_mediation_total(
        result$comparison, result$coef_by_model,
        edge_direct = edge_direct,
        via_pairs = via_pairs,
        always_ids = always_ids
      )
      tot <- avg$est
      se_tot <- avg$se
      sw <- avg$weight_sum
    } else {
      indir_est <- sum(vapply(chs, function(ch) ch$a * ch$b, numeric(1)))
      indir_var <- sum(vapply(chs, function(ch) {
        se_ab <- ch$b^2 * ch$se_a^2 + ch$a^2 * ch$se_b^2
        if (is.finite(se_ab)) se_ab else 0
      }, numeric(1)))
      d_est <- if (nrow(direct)) as.numeric(direct$estimate[[1]]) else 0
      d_se <- if (nrow(direct)) as.numeric(direct$se[[1]]) else 0
      if (!is.finite(d_est)) d_est <- 0
      if (!is.finite(d_se) || d_se < 0) d_se <- 0
      tot <- d_est + indir_est
      se_tot <- sqrt(d_se^2 + indir_var)
      if (has_ic) {
        # Σw of models with direct or any complete via
        need_sets <- list()
        if (!is.na(edge_direct) && nzchar(edge_direct)) {
          need_sets[[length(need_sets) + 1L]] <- edge_direct
        }
        for (ch in chs) {
          if (!is.na(ch$edge_a) && !is.na(ch$edge_b) &&
                nzchar(as.character(ch$edge_a)) && nzchar(as.character(ch$edge_b))) {
            need_sets[[length(need_sets) + 1L]] <- c(ch$edge_a, ch$edge_b)
          }
        }
        if (length(need_sets) && !is.null(result$comparison)) {
          ok <- result$comparison$status == "ok" & is.finite(result$comparison$weight)
          s <- 0
          for (i in which(ok)) {
            hit <- FALSE
            for (need in need_sets) {
              if (search_model_includes_edges(
                result$comparison$edges[i], need, always_ids = always_ids
              )) {
                hit <- TRUE
                break
              }
            }
            if (hit) s <- s + as.numeric(result$comparison$weight[i])
          }
          sw <- s
        }
      }
    }
    rows[[length(rows) + 1L]] <- mediation_freq_row(
      to = chs[[1]]$to_lab,
      from = chs[[1]]$from_lab,
      level = "total",
      est = tot, se = se_tot, conf_adjust = conf_adjust,
      weight_sum = sw
    )
  }
  do.call(rbind, rows)
}

#' Bayesian via / total rows from posterior draws
#' @keywords internal
mediation_effects_table_bayes <- function(state, chains, slopes, result) {
  if (is.null(result) || !identical(result$sampler, "brms")) {
    return(estimates_table_empty("none"))
  }
  rope_k <- as.numeric(result$rope_k %||% 0.1)
  ci <- as.numeric(result$ci_level %||% 0.95)
  rows <- list()
  chain_draws <- list()

  for (idx in seq_along(chains)) {
    ch <- chains[[idx]]
    if (is.na(ch$edge_a) || is.na(ch$edge_b) ||
          !nzchar(as.character(ch$edge_a %||% "")) ||
          !nzchar(as.character(ch$edge_b %||% ""))) {
      next
    }
    info_a <- edge_bayes_param_info(result, state, ch$edge_a)
    info_b <- edge_bayes_param_info(result, state, ch$edge_b)
    if (is.null(info_a) || is.null(info_b)) next
    aligned <- align_posterior_draws(info_a$draws, info_b$draws)
    if (is.null(aligned)) next
    draws_ab <- aligned[[1]] * aligned[[2]]
    pr_ab <- prior_product_moments(
      info_a$prior_mean, info_a$prior_sd,
      info_b$prior_mean, info_b$prior_sd
    )
    rope <- rope_bounds_from_prior(pr_ab$sd, rope_k)
    sm <- bayes_term_summary(
      draws_ab, rope = rope, ci = ci,
      prior_mean = pr_ab$mean, prior_sd = pr_ab$sd
    )
    rows[[length(rows) + 1L]] <- mediation_bayes_row(
      to = ch$to_lab,
      from = ch$from_lab,
      level = paste0("via ", ch$med_lab),
      sm = sm, ci = ci
    )
    chain_draws[[idx]] <- list(
      from = ch$from, to = ch$to,
      from_lab = ch$from_lab, to_lab = ch$to_lab,
      draws_ab = draws_ab,
      prior_mean = pr_ab$mean, prior_sd = pr_ab$sd
    )
  }
  if (!length(rows)) return(estimates_table_empty("none"))

  pairs <- unique(lapply(
    Filter(Negate(is.null), chain_draws),
    function(ch) c(ch$from, ch$to)
  ))
  for (pr in pairs) {
    x_id <- pr[[1]]; y_id <- pr[[2]]
    chs <- Filter(
      function(ch) !is.null(ch) && identical(ch$from, x_id) && identical(ch$to, y_id),
      chain_draws
    )
    if (!length(chs)) next
    # Direct X→Y draws when the slope exists in the same source table
    direct <- slopes[slopes$from == x_id & slopes$to == y_id, , drop = FALSE]
    draws_parts <- lapply(chs, function(ch) ch$draws_ab)
    prior_m <- sum(vapply(chs, function(ch) ch$prior_mean, numeric(1)))
    prior_v <- sum(vapply(chs, function(ch) ch$prior_sd^2, numeric(1)))
    if (nrow(direct) && !is.na(direct$edge_id[[1]]) &&
          nzchar(as.character(direct$edge_id[[1]]))) {
      info_d <- edge_bayes_param_info(result, state, direct$edge_id[[1]])
      if (!is.null(info_d) && length(info_d$draws)) {
        draws_parts <- c(list(info_d$draws), draws_parts)
        prior_m <- prior_m + as.numeric(info_d$prior_mean)
        prior_v <- prior_v + as.numeric(info_d$prior_sd)^2
      }
    }
    aligned <- do.call(align_posterior_draws, draws_parts)
    if (is.null(aligned)) next
    draws_tot <- Reduce(`+`, aligned)
    prior_sd <- sqrt(max(0, prior_v))
    if (!is.finite(prior_sd) || prior_sd <= 0) prior_sd <- 1
    rope <- rope_bounds_from_prior(prior_sd, rope_k)
    sm <- bayes_term_summary(
      draws_tot, rope = rope, ci = ci,
      prior_mean = prior_m, prior_sd = prior_sd
    )
    rows[[length(rows) + 1L]] <- mediation_bayes_row(
      to = chs[[1]]$to_lab,
      from = chs[[1]]$from_lab,
      level = "total",
      sm = sm, ci = ci
    )
  }
  do.call(rbind, rows)
}

#' @rdname mediation_effects_table
#' @keywords internal
indirect_effects_table <- function(state, conf_adjust = "none") {
  mediation_effects_table(state, conf_adjust = conf_adjust, bayes = FALSE)
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
#' Columns: Path (`Y ← X`, via `↜`, total `⇐`), Level, Estimate, SE, p, and (when `conf_adjust` ≠
#' `none`) an adjusted column (`pHolm` / `pBonf` / `pBH`) filled only for
#' confirmatory paths that entered the multiplicity adjustment.
#'
#' Structural paths only (`~`). Measurement loadings (`=~`) belong in the
#' CFA / measurement Results block, not Path coefficients.
#' @keywords internal
estimates_table <- function(state, estimates = NULL, conf_adjust = "none",
                            result = NULL) {
  empty <- estimates_table_empty(conf_adjust)
  pcol <- conf_pvalue_column_name(conf_adjust)
  want_adj <- !identical(pcol, "p")

  # Prefer full estimate rows (includes polynomial powers)
  if (!is.null(estimates) && nrow(estimates)) {
    est <- estimates[estimates$op == "~", , drop = FALSE]
    if (!nrow(est)) {
      main <- empty
    } else {
      from_lab <- as.character(est$rhs)
      to_lab <- as.character(est$lhs)
      lv <- if ("level" %in% names(est)) {
        ifelse(is.na(est$level) | !nzchar(as.character(est$level)), "", as.character(est$level))
      } else {
        rep("", nrow(est))
      }
      if ("kind" %in% names(est)) {
        lv <- ifelse(est$kind == "omnibus", "omnibus", lv)
      }
      raw_p <- coef_raw_pvalue(
        est$pvalue,
        if ("pvalue_raw" %in% names(est)) est$pvalue_raw else NULL
      )
      bayes_ci <- "ci_low" %in% names(est) && (
        any(is.finite(est$ci_low)) ||
          ("includes_zero" %in% names(est) && any(!is.na(est$includes_zero)))
      )
      main <- data.frame(
        To = to_lab,
        From = from_lab,
        Level = lv,
        Estimate = round(as.numeric(est$est), 3),
        SE = round(as.numeric(est$se), 3),
        stringsAsFactors = FALSE
      )
      if (bayes_ci && !any(is.finite(raw_p))) names(main)[names(main) == "SE"] <- "SD"
      if (bayes_ci) {
        lvl <- if ("ci_level" %in% names(est)) est$ci_level[is.finite(est$ci_level)] else numeric()
        pct <- if (length(lvl)) round(100 * lvl[[1]]) else 95
        cri <- ifelse(
          is.finite(est$ci_low) & is.finite(est$ci_high),
          sprintf("[%.3f, %.3f]", est$ci_low, est$ci_high),
          ""
        )
        main[[sprintf("%d%% HDI", pct)]] <- cri
        if ("pd" %in% names(est)) {
          main$pd <- ifelse(
            is.finite(est$pd),
            sprintf("%.1f%%", 100 * est$pd),
            ""
          )
        }
        if ("rope_pct" %in% names(est)) {
          main[["ROPE%"]] <- ifelse(
            is.finite(est$rope_pct),
            sprintf("%.1f%%", 100 * est$rope_pct),
            ""
          )
        }
        if ("rope_decision" %in% names(est)) {
          main$ROPE <- ifelse(
            is.na(est$rope_decision) | !nzchar(as.character(est$rope_decision)),
            "",
            as.character(est$rope_decision)
          )
        }
        if ("log_bf10" %in% names(est)) {
          # Natural log of Savage–Dickey BF10 vs θ = 0
          main[["log BF10"]] <- vapply(est$log_bf10, function(b) {
            if (is.infinite(b) && b > 0) return("Inf")
            if (is.infinite(b) && b < 0) return("-Inf")
            if (!is.finite(b)) return("")
            sprintf("%+.2f", b)
          }, character(1))
        }
        if ("kl_prior_post" %in% names(est)) {
          # KL(posterior || prior) in nats; low values highlighted in Results
          main[["KL"]] <- vapply(est$kl_prior_post, format_bayes_kl, character(1))
        }
        if ("post_narrow" %in% names(est)) {
          # prior SD / posterior SD; little narrowing highlighted in Results
          main[["SD\u2193"]] <- vapply(
            est$post_narrow, format_bayes_sd_narrow, character(1)
          )
        }
      }
      if (!bayes_ci || any(is.finite(raw_p))) {
        main$p <- vapply(raw_p, format_pvalue, character(1), digits = 3)
      }
      if (want_adj) {
        show_adj <- estimate_rows_have_adj_p(est)
        adj_chr <- rep("", nrow(est))
        if (any(show_adj)) {
          adj_chr[show_adj] <- vapply(
            est$pvalue[show_adj], format_pvalue, character(1), digits = 3
          )
        }
        main[[pcol]] <- adj_chr
      }
    }
  } else {
    edges <- state$edges
    if (!nrow(edges)) {
      main <- empty
    } else {
      if (!"level" %in% names(edges)) edges$level <- NA_character_
      # Structural paths only — skip measurement / definitional / excluded / empty
      keep <- !(edges$type %in% c("definitional", "measurement")) &
        !vapply(edges$to, function(id) is_interaction_node(state, id), logical(1)) &
        (is.na(edges$status) | edges$status != "excluded") &
        is.finite(edges$estimate) & is.finite(edges$se)
      edges <- edges[keep, , drop = FALSE]
      if (!nrow(edges)) {
        main <- empty
      } else {
        raw_p <- coef_raw_pvalue(
          edges$pvalue,
          if ("pvalue_raw" %in% names(edges)) edges$pvalue_raw else NULL
        )
        main <- data.frame(
          To = vapply(edges$to, function(id) node_display_name(state, id), character(1)),
          From = vapply(edges$from, function(id) node_display_name(state, id), character(1)),
          Level = ifelse(is.na(edges$level) | !nzchar(edges$level), "", edges$level),
          Estimate = round(edges$estimate, 3),
          SE = round(edges$se, 3),
          p = vapply(raw_p, format_pvalue, character(1), digits = 3),
          stringsAsFactors = FALSE
        )
        if (want_adj) {
          show_adj <- if ("pvalue_raw" %in% names(edges)) {
            edges$type == "confirmatory" &
              is.finite(as.numeric(edges$pvalue_raw))
          } else {
            rep(FALSE, nrow(edges))
          }
          adj_chr <- rep("", nrow(edges))
          if (any(show_adj)) {
            adj_chr[show_adj] <- vapply(
              edges$pvalue[show_adj], format_pvalue, character(1), digits = 3
            )
          }
          main[[pcol]] <- adj_chr
        }
      }
    }
  }

  bayes_main <- any(grepl("% HDI$", names(main))) || "pd" %in% names(main)
  indir <- mediation_effects_table(
    state,
    conf_adjust = conf_adjust,
    result = result,
    bayes = bayes_main,
    estimates = estimates
  )
  out <- if (!nrow(main)) {
    indir
  } else if (!nrow(indir)) {
    main
  } else {
    for (nm in setdiff(names(main), names(indir))) {
      indir[[nm]] <- if (is.character(main[[nm]])) {
        rep("", nrow(indir))
      } else {
        rep(NA, nrow(indir))
      }
    }
    for (nm in setdiff(names(indir), names(main))) {
      main[[nm]] <- if (is.character(indir[[nm]])) {
        rep("", nrow(main))
      } else {
        rep(NA, nrow(main))
      }
    }
    indir <- indir[names(main)]
    rbind(main, indir)
  }
  # AIC/BIC search: Σw on links; via/total keep joint / path Σw from mediation rows
  if (nrow(out) && !is.null(result) && !is.null(result$comparison) &&
        !is.null(result$edge_weights) && length(result$edge_weights)) {
    lv <- as.character(out$Level %||% "")
    lv[is.na(lv)] <- ""
    is_derived <- startsWith(lv, "via ") | lv == "total"
    ws <- estimates_table_weight_sum_col(
      state, result,
      to_lab = as.character(out$To),
      from_lab = as.character(out$From),
      level = lv
    )
    if (!is.null(ws)) {
      link_sw <- format_weight_sum_col(ws)
      if ("Σw" %in% names(out)) {
        # Keep joint Σw already set on via/total; fill links only
        cur <- as.character(out[["Σw"]])
        cur[is.na(cur)] <- ""
        cur[!is_derived] <- link_sw[!is_derived]
        out[["Σw"]] <- cur
      } else {
        out[["Σw"]] <- link_sw
      }
      if ("p" %in% names(out)) {
        out$p[!is_derived] <- ""
      }
      for (acol in c("pHolm", "pBonf", "pBH")) {
        if (acol %in% names(out)) out[[acol]][!is_derived] <- ""
      }
    }
  }
  estimates_table_collapse_path(
    estimates_table_order_rows(estimates_table_order_cols(out))
  )
}
