#' Fit structural model under a p-value framework
#'
#' Observed-only path models use equation-wise lm / glm (binomial) based on
#' each dependent variable's type. Models with latents use lavaan
#' (WLSMV when endogenous observed variables are binary).
#'
#' @param state Model state.
#' @param data Data frame.
#' @param alpha Significance level for marking exploratory edges.
#' @param mark_nonsig If TRUE, exploratory edges with p > alpha are marked excluded.
#' @return A list with updated `state`, `fit`, `estimates`, `fit_measures`, and `syntax`.
#' @export
fit_pvalue_model <- function(state, data, alpha = 0.05, mark_nonsig = TRUE) {
  edge_ids <- analyzable_edge_ids(state)
  # Still mark definitional wires as included (no coefficients)
  all_display_ids <- unique(c(edge_ids, state$edges$id[
    state$edges$type == "definitional" |
      vapply(state$edges$to, function(id) is_interaction_node(state, id), logical(1))
  ]))
  fitted <- fit_structural_model(state, data, include_edge_ids = edge_ids)
  state2 <- apply_estimates_to_edges(state, fitted$estimates, included_edge_ids = all_display_ids)
  state2 <- apply_r_squared_to_nodes(state2, fitted$r_squared)

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
    engine = fitted$engine
  )
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

#' Round AIC/BIC to integers for display
#' @keywords internal
format_fit_measures_display <- function(fm) {
  if (is.null(fm) || !nrow(fm)) return(fm)
  out <- fm
  ic <- tolower(out$measure) %in% c("aic", "bic")
  out$value[ic] <- round(out$value[ic])
  out
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
    blocks <- lapply(names(result$fit), function(dv_id) {
      fit <- result$fit[[dv_id]]
      if (is.null(fit)) return(NULL)
      cf <- stats::coef(fit)
      if (!length(cf)) return(NULL)
      yname <- if (!is.null(state)) {
        tryCatch(node_display_name(state, dv_id), error = function(e) dv_id)
      } else {
        dv_id
      }
      rn <- tryCatch(as.character(stats::formula(fit)[[2]]), error = function(e) yname)
      if (nzchar(rn)) yname <- rn

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
format_comparison_display <- function(comparison, state = NULL) {
  if (is.null(comparison) || !nrow(comparison)) return(comparison)
  predictors <- if (is.null(state)) {
    ifelse(is.na(comparison$edges) | !nzchar(comparison$edges), "(null)", comparison$edges)
  } else {
    unname(vapply(comparison$edges, format_predictor_set, character(1), state = state))
  }
  out <- data.frame(
    predictors = predictors,
    AIC = round(comparison$aic),
    BIC = round(comparison$bic),
    delta = round(comparison$delta, 1),
    weight = round(comparison$weight, 3),
    status = comparison$status,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Path coefficient table for results pane
#' @keywords internal
estimates_table <- function(state, estimates = NULL) {
  empty <- data.frame(
    From = character(), Level = character(), To = character(),
    Type = character(), Status = character(),
    Estimate = numeric(), SE = numeric(), p = numeric(),
    stringsAsFactors = FALSE
  )

  # Prefer full estimate rows (includes polynomial powers)
  if (!is.null(estimates) && nrow(estimates)) {
    est <- estimates[estimates$op %in% c("~", "=~"), , drop = FALSE]
    if (!nrow(est)) return(empty)
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
    return(data.frame(
      From = from_lab,
      Level = lv,
      To = to_lab,
      Type = ifelse(est$op == "=~", "measurement", "path"),
      Status = "included",
      Estimate = round(as.numeric(est$est), 3),
      SE = round(as.numeric(est$se), 3),
      p = vapply(est$pvalue, format_pvalue, character(1), digits = 3),
      stringsAsFactors = FALSE
    ))
  }

  edges <- state$edges
  if (!nrow(edges)) return(empty)
  if (!"level" %in% names(edges)) edges$level <- NA_character_
  # Hide definitional wires into interaction operators
  keep <- edges$type != "definitional" &
    !vapply(edges$to, function(id) is_interaction_node(state, id), logical(1))
  edges <- edges[keep, , drop = FALSE]
  if (!nrow(edges)) return(empty)

  data.frame(
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
