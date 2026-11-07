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
                             conf_adjust = c("none", "bonferroni", "holm", "BH"),
                             uncertainty = c("none", "bootstrap", "plausible"),
                             uncertainty_draws = NULL,
                             uncertainty_progress = NULL) {
  uncertainty <- match.arg(uncertainty)
  conf_adjust <- match.arg(conf_adjust)
  edge_ids <- analyzable_edge_ids(state)
  # Still mark definitional wires as included (no coefficients)
  all_display_ids <- unique(c(edge_ids, state$edges$id[
    state$edges$type == "definitional" |
      vapply(state$edges$to, function(id) is_interaction_node(state, id), logical(1))
  ]))
  fitted <- fit_structural_model(state, data, include_edge_ids = edge_ids)
  fitted <- apply_postselection_uncertainty(
    state, data, edge_ids, fitted,
    method = uncertainty,
    draws = uncertainty_draws,
    progress = uncertainty_progress
  )
  state2 <- apply_estimates_to_edges(state, fitted$estimates, included_edge_ids = all_display_ids)
  state2 <- apply_r_squared_to_nodes(state2, fitted$r_squared)
  adj <- apply_confirmatory_p_adjust(state2, fitted$estimates, method = conf_adjust)
  state2 <- adj$state
  fitted$estimates <- adj$estimates

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
    conf_adjust = conf_adjust
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

#' Pretty predictor name as HTML (ecole__B → ecole[B]; x_p2 → x<sup>2</sup>)
#' @keywords internal
pretty_term_html <- function(name, level = NULL) {
  if (identical(name, "(Intercept)")) return(NULL)
  if (!is.null(level) && is_poly_level(level)) {
    deg <- sub("^\\^", "", as.character(level))
    return(sprintf("%s<sup>%s</sup>", htmltools::htmlEscape(name), htmltools::htmlEscape(deg)))
  }
  if (grepl("_p[0-9]+$", name)) {
    base <- sub("_p[0-9]+$", "", name)
    deg <- sub("^.*_p", "", name)
    return(sprintf("%s<sup>%s</sup>", htmltools::htmlEscape(base), htmltools::htmlEscape(deg)))
  }
  if (grepl("__", name, fixed = TRUE)) {
    parts <- strsplit(name, "__", fixed = TRUE)[[1]]
    return(sprintf(
      "%s[%s]",
      htmltools::htmlEscape(parts[[1]]),
      htmltools::htmlEscape(paste(parts[-1], collapse = "__"))
    ))
  }
  if (!is.null(level) && !is.na(level) && nzchar(as.character(level))) {
    return(sprintf(
      "%s[%s]",
      htmltools::htmlEscape(name),
      htmltools::htmlEscape(as.character(level))
    ))
  }
  htmltools::htmlEscape(name)
}

#' Format one coefficient with 95% CI as superscript brackets
#' @keywords internal
format_est_ci_html <- function(est, lo, hi, digits = 2) {
  sprintf(
    "%s<sup class=\"smt-ci\">[%s, %s]</sup>",
    formatC(est, digits = digits, format = "f"),
    formatC(lo, digits = digits, format = "f"),
    formatC(hi, digits = digits, format = "f")
  )
}

#' Linear predictor HTML from coefficients + Wald CIs
#' @keywords internal
format_linear_predictor_html <- function(cf, ci, digits = 2) {
  parts <- character()
  for (i in seq_along(cf)) {
    nm <- names(cf)[[i]]
    est <- unname(cf[[i]])
    lo <- if (is.matrix(ci) && nm %in% rownames(ci)) unname(ci[nm, 1]) else est
    hi <- if (is.matrix(ci) && nm %in% rownames(ci)) unname(ci[nm, 2]) else est
    piece <- format_est_ci_html(est, lo, hi, digits = digits)
    term <- pretty_term_html(nm)
    body <- if (is.null(term)) piece else paste0(piece, "&middot;", term)
    if (i == 1L) {
      parts <- c(parts, body)
    } else {
      parts <- c(parts, paste0(" + ", body))
    }
  }
  paste(parts, collapse = "")
}

#' Linear predictor HTML from estimate rows (includes poly levels)
#' @keywords internal
format_eta_from_estimates <- function(est_rows, digits = 2) {
  if (is.null(est_rows) || !nrow(est_rows)) return("")
  # Skip omnibus rows (no single coefficient in the linear predictor)
  if ("kind" %in% names(est_rows)) {
    est_rows <- est_rows[est_rows$kind != "omnibus", , drop = FALSE]
  }
  est_rows <- est_rows[is.finite(as.numeric(est_rows$est)), , drop = FALSE]
  if (!nrow(est_rows)) return("")
  parts <- character()
  for (i in seq_len(nrow(est_rows))) {
    est <- as.numeric(est_rows$est[i])
    se <- as.numeric(est_rows$se[i])
    lo <- est - 1.96 * se
    hi <- est + 1.96 * se
    piece <- format_est_ci_html(est, lo, hi, digits = digits)
    lv <- if ("level" %in% names(est_rows)) est_rows$level[i] else NA_character_
    term <- pretty_term_html(est_rows$rhs[i], level = lv)
    body <- if (is.null(term)) piece else paste0(piece, "&middot;", term)
    parts <- c(parts, if (!length(parts)) body else paste0(" + ", body))
  }
  paste(parts, collapse = "")
}

#' Link / expectation wrapper for an outcome equation
#' @return list(lhs_html, wrap_left, wrap_right, tag, note)
#' @keywords internal
equation_link_wrapper <- function(fit, yname) {
  y_esc <- htmltools::htmlEscape(yname)
  if (inherits(fit, "glmerMod")) {
    fam <- tryCatch(stats::family(fit), error = function(e) NULL)
    link <- if (!is.null(fam)) fam$link else "logit"
    re <- "(1|cluster)"
    if (identical(link, "logit")) {
      return(list(
        tag = paste("glmer · binomial · logit ·", re),
        lhs = sprintf("P(%s = 1)", y_esc),
        wrap_left = "logit<sup class=\"smt-link-fn\">&minus;1</sup>(",
        wrap_right = ")",
        note = "Random intercept only; logit<sup>&minus;1</sup> = logistic (expit)"
      ))
    }
    return(list(
      tag = paste0("glmer · binomial · ", link, " · ", re),
      lhs = sprintf("P(%s = 1)", y_esc),
      wrap_left = "",
      wrap_right = "",
      note = "Random intercept only"
    ))
  }
  if (inherits(fit, "lmerMod") || (inherits(fit, "merMod") && !inherits(fit, "glmerMod"))) {
    return(list(
      tag = "lmer · gaussian · (1|cluster)",
      lhs = sprintf("E(%s)", y_esc),
      wrap_left = "",
      wrap_right = "",
      note = "Random intercept only"
    ))
  }
  if (inherits(fit, "glm")) {
    fam <- tryCatch(stats::family(fit), error = function(e) NULL)
    fam_name <- if (!is.null(fam)) fam$family else "glm"
    link <- if (!is.null(fam)) fam$link else "unknown"
    if (identical(fam_name, "binomial") && identical(link, "logit")) {
      return(list(
        tag = "glm · binomial · logit",
        lhs = sprintf("P(%s = 1)", y_esc),
        wrap_left = "logit<sup class=\"smt-link-fn\">&minus;1</sup>(",
        wrap_right = ")",
        note = "logit<sup>&minus;1</sup> = logistic (expit)"
      ))
    }
    if (identical(fam_name, "binomial") && identical(link, "probit")) {
      return(list(
        tag = "glm · binomial · probit",
        lhs = sprintf("P(%s = 1)", y_esc),
        wrap_left = "&Phi;(",
        wrap_right = ")",
        note = "&Phi; = standard normal CDF"
      ))
    }
    if (identical(link, "identity")) {
      return(list(
        tag = sprintf("glm · %s · identity", fam_name),
        lhs = sprintf("E(%s)", y_esc),
        wrap_left = "",
        wrap_right = "",
        note = NULL
      ))
    }
    # generic inverse-link F(·)
    return(list(
      tag = sprintf("glm · %s · %s", fam_name, link),
      lhs = sprintf("E(%s)", y_esc),
      wrap_left = sprintf("g<sup class=\"smt-link-fn\">&minus;1</sup><sub class=\"smt-link-fn\">%s</sub>(",
                          htmltools::htmlEscape(link)),
      wrap_right = ")",
      note = sprintf("g<sup>&minus;1</sup> = inverse %s link", htmltools::htmlEscape(link))
    ))
  }
  if (inherits(fit, "lm")) {
    return(list(
      tag = "lm · gaussian · identity",
      lhs = sprintf("E(%s)", y_esc),
      wrap_left = "",
      wrap_right = "",
      note = NULL
    ))
  }
  list(
    tag = "model",
    lhs = y_esc,
    wrap_left = "",
    wrap_right = "",
    note = NULL
  )
}

#' Build HTML equations for the fitted (best) model
#'
#' Shows each outcome with an explicit model/link form, e.g.
#' `P(y)=logit^{-1}(...)` for logistic GLM, plus estimates and 95% CIs.
#' @keywords internal
format_best_equations_html <- function(result, state = NULL) {
  if (is.null(result)) return(NULL)

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

      wrap <- equation_link_wrapper(fit, yname)
      if (!is.null(wrap$note)) notes <<- c(notes, wrap$note)

      # Prefer estimate rows (explicit poly levels) + intercept from fit
      eta <- NULL
      if (!is.null(est_all) && nrow(est_all)) {
        rows <- est_all[est_all$op == "~" & est_all$lhs == yname, , drop = FALSE]
        if (nrow(rows)) {
          parts <- character()
          if ("(Intercept)" %in% names(cf)) {
            se0 <- tryCatch(
              sqrt(diag(stats::vcov(fit)))[["(Intercept)"]],
              error = function(e) NA_real_
            )
            b0 <- unname(cf[["(Intercept)"]])
            if (is.finite(se0)) {
              parts <- c(parts, format_est_ci_html(b0, b0 - 1.96 * se0, b0 + 1.96 * se0))
            } else {
              parts <- c(parts, formatC(b0, digits = 2, format = "f"))
            }
          }
          pred <- format_eta_from_estimates(rows)
          if (nzchar(pred)) parts <- c(parts, pred)
          if (length(parts)) eta <- paste(parts, collapse = " + ")
        }
      }
      if (is.null(eta)) {
        ci <- tryCatch(
          stats::confint.default(fit, level = 0.95),
          error = function(e) {
            se <- sqrt(diag(stats::vcov(fit)))
            cbind(cf - 1.96 * se, cf + 1.96 * se)
          }
        )
        eta <- format_linear_predictor_html(cf, ci)
      }
      sprintf(
        paste0(
          "<div class=\"smt-eq-block\">",
          "<div class=\"smt-eq-tag\">%s</div>",
          "<div class=\"smt-eq-line\">",
          "<span class=\"smt-eq-lhs\">%s</span> = %s%s%s",
          "</div></div>"
        ),
        htmltools::htmlEscape(wrap$tag),
        wrap$lhs,
        wrap$wrap_left,
        eta,
        wrap$wrap_right
      )
    })
    blocks <- Filter(Negate(is.null), blocks)
    if (!length(blocks)) return(NULL)
    notes <- unique(notes)
    note_html <- if (length(notes)) {
      paste0("<div class=\"smt-eq-note\">", paste(notes, collapse = " · "), "</div>")
    } else {
      ""
    }
    return(htmltools::HTML(paste0(
      "<div class=\"smt-equations\">",
      "<div class=\"smt-eq-caption\">Best model (estimate<sup class=\"smt-ci\">[95% CI]</sup>)</div>",
      paste(blocks, collapse = ""),
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
      eta <- format_eta_from_estimates(rows)
      is_ord <- y %in% ordered
      if (is_ord) {
        sprintf(
          paste0(
            "<div class=\"smt-eq-block\">",
            "<div class=\"smt-eq-tag\">lavaan · ordered · probit/WLSMV</div>",
            "<div class=\"smt-eq-line\">",
            "<span class=\"smt-eq-lhs\">P(%s*)</span> = &Phi;(%s)",
            "</div></div>"
          ),
          htmltools::htmlEscape(y),
          eta
        )
      } else {
        sprintf(
          paste0(
            "<div class=\"smt-eq-block\">",
            "<div class=\"smt-eq-tag\">lavaan · continuous · identity</div>",
            "<div class=\"smt-eq-line\">",
            "<span class=\"smt-eq-lhs\">E(%s)</span> = %s",
            "</div></div>"
          ),
          htmltools::htmlEscape(y),
          eta
        )
      }
    })
    return(htmltools::HTML(paste0(
      "<div class=\"smt-equations\">",
      "<div class=\"smt-eq-caption\">Best model (estimate<sup class=\"smt-ci\">[95% CI]</sup>)</div>",
      paste(blocks, collapse = ""),
      "</div>"
    )))
  }

  fit <- result$fit
  if (!inherits(fit, "lavaan")) return(NULL)
  pe <- tryCatch(
    as.data.frame(lavaan::parameterEstimates(fit, ci = TRUE, level = 0.95)),
    error = function(e) NULL
  )
  if (is.null(pe) || !nrow(pe)) return(NULL)
  pe <- pe[pe$op == "~", , drop = FALSE]
  if (!nrow(pe)) return(NULL)

  ordered <- tryCatch(lavaan::lavInspect(fit, "ordered"), error = function(e) character())
  if (is.null(ordered)) ordered <- character()

  blocks <- lapply(unique(pe$lhs), function(y) {
    rows <- pe[pe$lhs == y, , drop = FALSE]
    cf <- setNames(rows$est, rows$rhs)
    ci <- cbind(rows$ci.lower, rows$ci.upper)
    rownames(ci) <- rows$rhs
    eta <- format_linear_predictor_html(cf, ci)
    is_ord <- y %in% ordered
    if (is_ord) {
      sprintf(
        paste0(
          "<div class=\"smt-eq-block\">",
          "<div class=\"smt-eq-tag\">lavaan · ordered · probit/WLSMV</div>",
          "<div class=\"smt-eq-line\">",
          "<span class=\"smt-eq-lhs\">P(%s*)</span> = &Phi;(%s)",
          "</div></div>"
        ),
        htmltools::htmlEscape(y),
        eta
      )
    } else {
      sprintf(
        paste0(
          "<div class=\"smt-eq-block\">",
          "<div class=\"smt-eq-tag\">lavaan · continuous · identity</div>",
          "<div class=\"smt-eq-line\">",
          "<span class=\"smt-eq-lhs\">E(%s)</span> = %s",
          "</div></div>"
        ),
        htmltools::htmlEscape(y),
        eta
      )
    }
  })
  htmltools::HTML(paste0(
    "<div class=\"smt-equations\">",
    "<div class=\"smt-eq-caption\">Best model (estimate<sup class=\"smt-ci\">[95% CI]</sup>)</div>",
    paste(blocks, collapse = ""),
    "</div>"
  ))
}

#' Short predictor label for an edge (e.g. methode, ecole[B])
#' @keywords internal
edge_predictor_label <- function(state, edge_id) {
  idx <- match(edge_id, state$edges$id)
  if (is.na(idx)) return(edge_id)
  from_lab <- node_display_name(state, state$edges$from[idx])
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

#' Path coefficient table for results pane
#' @keywords internal
estimates_table <- function(state, estimates = NULL) {
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
  if (!nrow(main)) return(indir)
  if (!nrow(indir)) return(main)
  rbind(main, indir)
}
