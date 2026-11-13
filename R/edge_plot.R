#' Valid inspect-panel relationship kinds
#' @keywords internal
edge_rel_kind_choices <- function() {
  c(
    "Linear" = "linear",
    "Quadratic" = "quadratic",
    "Cubic" = "cubic",
    "Smooth (cubic in model)" = "loess"
  )
}

#' Mapped analysis column for a node, or NA if unmapped
#'
#' Unmapped nodes must not pull analysis data by id/label coincidence.
#' @keywords internal
node_analysis_column <- function(state, node_id) {
  state <- ensure_node_kind_columns(state)
  if (is.na(node_id) || !nzchar(as.character(node_id))) return(NA_character_)
  if (is_interaction_node(state, node_id)) return(NA_character_)
  col <- state$nodes$observed_column[match(node_id, state$nodes$id)]
  if (!is.na(col) && nzchar(as.character(col))) return(as.character(col))
  NA_character_
}

#' Column name used for plotting a node (mapped column, else sim/score key)
#' @keywords internal
node_plot_column <- function(state, node_id) {
  state <- ensure_node_kind_columns(state)
  if (is.na(node_id) || !nzchar(as.character(node_id))) return(NA_character_)
  if (is_interaction_node(state, node_id)) return(NA_character_)
  mapped <- node_analysis_column(state, node_id)
  if (!is.na(mapped) && nzchar(mapped)) return(mapped)
  # Unmapped observed or latent: stable node-id key for simulation / factor scores
  sanitize_id(as.character(node_id))
}

#' Ensure analysis data includes regression factor scores for latents (plots)
#'
#' Recomputes the same CFA regression scores used in the two-step path stage
#' (`attach_latent_factor_scores`) when score columns are missing.
#' @keywords internal
enrich_data_with_factor_scores <- function(state, data, score_method = "own") {
  if (is.null(data) || !nrow(data)) return(data)
  state <- ensure_node_kind_columns(state)
  if (!model_uses_latents(state, state$edges)) return(data)
  meas <- state$edges[state$edges$type == "measurement", , drop = FALSE]
  if (!nrow(meas)) return(data)
  score_method <- normalize_latent_score_method(score_method, state)
  latent_ids <- unique(as.character(meas$from))
  score_nms <- vapply(latent_ids, function(id) lavaan_var_name(state, id), character(1))
  if (all(score_nms %in% names(data))) {
    ok <- vapply(score_nms, function(nm) {
      any(is.finite(suppressWarnings(as.numeric(data[[nm]]))))
    }, logical(1))
    if (all(ok)) return(data)
  }
  tryCatch(
    attach_latent_factor_scores(state, data, score_method = score_method)$data,
    error = function(e) {
      if (exists("smt_debug", mode = "function")) {
        smt_debug("enrich_data_with_factor_scores failed: ", e$message)
      }
      data
    }
  )
}

#' Axis label for a plotted node (marks latent scores)
#' @keywords internal
node_plot_label <- function(state, node_id) {
  lab <- node_display_name(state, node_id)
  if (is_latent_node(state, node_id)) paste0(lab, " (score)") else lab
}

#' Prior band for an unstandardized slope Normal(b, σ_b)
#'
#' Plots `y = ȳ + b · (x − x̄)` with a ribbon for b ∈ [μ − σ, μ + σ].
#' @keywords internal
edge_prior_line_data <- function(x, y, prior_mean, prior_sd = 1) {
  if (!is.finite(prior_mean)) return(NULL)
  if (!is.finite(prior_sd) || prior_sd < 0) prior_sd <- 0
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 2L) return(NULL)
  sx <- stats::sd(x)
  if (!is.finite(sx) || sx <= 0) return(NULL)
  mx <- mean(x)
  my <- mean(y)
  xr <- range(x)
  dx <- xr - mx
  data.frame(
    x_plot = xr,
    y_plot = my + prior_mean * dx,
    ymin = my + (prior_mean - prior_sd) * dx,
    ymax = my + (prior_mean + prior_sd) * dx,
    stringsAsFactors = FALSE
  )
}

#' Gray prior mean line + ±1 SD ribbon for continuous-x plots
#' @keywords internal
edge_prior_overlay_layers <- function(line_df) {
  if (is.null(line_df) || !nrow(line_df)) return(list())
  layers <- list()
  if (all(c("ymin", "ymax") %in% names(line_df)) &&
        any(is.finite(line_df$ymin)) && any(is.finite(line_df$ymax)) &&
        !isTRUE(all.equal(line_df$ymin, line_df$ymax))) {
    layers[[length(layers) + 1L]] <- ggplot2::geom_ribbon(
      data = line_df,
      ggplot2::aes(x = .data$x_plot, ymin = .data$ymin, ymax = .data$ymax),
      fill = "#8E8E93",
      alpha = 0.18,
      inherit.aes = FALSE
    )
  }
  layers[[length(layers) + 1L]] <- ggplot2::geom_line(
    data = line_df,
    ggplot2::aes(x = .data$x_plot, y = .data$y_plot),
    color = "#8E8E93",
    linetype = "dashed",
    linewidth = 0.9,
    inherit.aes = FALSE
  )
  layers
}

#' Interaction partners for a main-effect edge into the same DV
#'
#' Finds × nodes that wire `from_id` and also have a structural edge into
#' `to_id` (optionally matching `to_level`). Returns the other factor(s).
#' @return List of lists with `ix_id`, `partner_id`, `partner_wire_level`.
#' @keywords internal
main_effect_interaction_partners <- function(state, from_id, to_id,
                                             to_level = NA_character_) {
  state <- ensure_edge_to_port(state)
  state <- ensure_node_kind_columns(state)
  if (is.na(from_id) || !nzchar(from_id) || is.na(to_id) || !nzchar(to_id)) {
    return(list())
  }
  ix_ids <- state$nodes$id[
    vapply(state$nodes$id, function(id) is_interaction_node(state, id), logical(1))
  ]
  if (!length(ix_ids)) return(list())

  to_level <- if (is.null(to_level) || is.na(to_level) || !nzchar(as.character(to_level))) {
    NA_character_
  } else {
    as.character(to_level)
  }

  out <- list()
  for (ix in ix_ids) {
    fac <- interaction_factors(state, ix)
    if (identical(fac$a, from_id)) {
      partner_id <- fac$b
      partner_wire_level <- fac$level_b
    } else if (identical(fac$b, from_id)) {
      partner_id <- fac$a
      partner_wire_level <- fac$level_a
    } else {
      next
    }
    if (is.na(partner_id) || !nzchar(as.character(partner_id))) next

    outs <- which(
      state$edges$from == ix &
        state$edges$to == to_id &
        !state$edges$type %in% c("measurement", "definitional")
    )
    if (!length(outs)) next
    if (!is.na(to_level) && "to_level" %in% names(state$edges)) {
      tl <- as.character(state$edges$to_level[outs])
      outs <- outs[is.na(tl) | !nzchar(tl) | tl == to_level]
      if (!length(outs)) next
    }

    out[[length(out) + 1L]] <- list(
      ix_id = ix,
      partner_id = as.character(partner_id),
      partner_wire_level = if (is.na(partner_wire_level) || !nzchar(as.character(partner_wire_level))) {
        NA_character_
      } else {
        as.character(partner_wire_level)
      }
    )
  }
  out
}

#' Row mask: partner at interaction reference (base / centered 0)
#' @keywords internal
partner_reference_row_mask <- function(state, data, partner_id) {
  n <- if (is.null(data)) 0L else nrow(data)
  empty <- list(ok = logical(n), label = NULL, n_keep = 0L)
  if (!n) return(empty)
  col <- node_plot_column(state, partner_id)
  lab <- node_plot_label(state, partner_id)
  if (is.na(col) || !nzchar(col) || !col %in% names(data)) {
    return(list(ok = rep(TRUE, n), label = NULL, n_keep = n))
  }
  x <- data[[col]]
  kind <- classify_variable(x)

  if (kind %in% c("binary", "categorical")) {
    coding <- tryCatch(
      variable_coding_for_node(state, partner_id, data),
      error = function(e) NULL
    )
    base <- if (!is.null(coding) && !is.na(coding$base) && nzchar(coding$base)) {
      as.character(coding$base)
    } else {
      meta <- node_factor_coding(state, partner_id)
      if (!is.na(meta$base) && nzchar(meta$base)) {
        as.character(meta$base)
      } else if (is.factor(x)) {
        levels(droplevels(x))[[1]]
      } else {
        sort(unique(as.character(stats::na.omit(x))))[[1]]
      }
    }
    ok <- !is.na(x) & as.character(x) == as.character(base)
    list(
      ok = ok,
      label = sprintf("%s = %s", lab, base),
      n_keep = sum(ok, na.rm = TRUE)
    )
  } else {
    xv <- suppressWarnings(as.numeric(x))
    mu <- mean(xv, na.rm = TRUE)
    s <- stats::sd(xv, na.rm = TRUE)
    if (!is.finite(mu)) {
      return(list(ok = rep(FALSE, n), label = NULL, n_keep = 0L))
    }
    if (!is.finite(s) || s <= 0) s <- 1
    ok <- is.finite(xv) & abs(xv - mu) <= 0.25 * s
    if (sum(ok, na.rm = TRUE) < 10L) {
      qs <- stats::quantile(xv, probs = c(0.4, 0.6), na.rm = TRUE, names = FALSE)
      ok <- is.finite(xv) & xv >= qs[[1]] & xv <= qs[[2]]
    }
    list(
      ok = ok,
      label = sprintf("%s \u2248 mean (centered 0)", lab),
      n_keep = sum(ok, na.rm = TRUE)
    )
  }
}

#' Other predictors in the same DV equation to hold at reference
#'
#' For a focal structural edge `from_id → to_id`, returns node ids that should
#' be fixed when plotting the conditional effect (interaction partners and
#' any other co-predictors into the same DV / response level).
#' @keywords internal
co_predictor_ids_to_hold <- function(state, from_id, to_id,
                                     to_level = NA_character_) {
  state <- ensure_edge_to_port(state)
  state <- ensure_node_kind_columns(state)
  if (is.na(from_id) || !nzchar(from_id) || is.na(to_id) || !nzchar(to_id)) {
    return(character())
  }
  if (!nrow(state$edges)) return(character())

  to_level <- if (is.null(to_level) || is.na(to_level) || !nzchar(as.character(to_level))) {
    NA_character_
  } else {
    as.character(to_level)
  }

  hits <- which(
    state$edges$to == to_id &
      !state$edges$type %in% c("measurement", "definitional")
  )
  if (!length(hits)) return(character())
  if (!is.na(to_level) && "to_level" %in% names(state$edges)) {
    tl <- as.character(state$edges$to_level[hits])
    hits <- hits[is.na(tl) | !nzchar(tl) | tl == to_level]
  }
  if (!length(hits)) return(character())

  # Factors of the focal interaction (when plotting × → DV) stay free
  focal_free <- character()
  if (is_interaction_node(state, from_id)) {
    fac <- interaction_factors(state, from_id)
    focal_free <- unique(c(fac$a, fac$b))
    focal_free <- focal_free[!is.na(focal_free) & nzchar(as.character(focal_free))]
  }

  ids <- character()
  for (i in hits) {
    fr <- as.character(state$edges$from[[i]])
    if (identical(fr, as.character(from_id))) next
    if (is_interaction_node(state, fr)) {
      fac <- interaction_factors(state, fr)
      for (fid in c(fac$a, fac$b)) {
        if (is.na(fid) || !nzchar(as.character(fid))) next
        if (identical(as.character(fid), as.character(from_id))) next
        if (as.character(fid) %in% focal_free) next
        ids <- c(ids, as.character(fid))
      }
    } else {
      if (fr %in% focal_free) next
      ids <- c(ids, fr)
    }
  }
  unique(ids[nzchar(ids)])
}

#' Restrict rows so co-predictors sit at their reference / mean
#'
#' Makes the bivariate link plot match a conditional (partial) effect:
#' other predictors in the DV equation are held at treatment-coded 0 / base
#' or near their mean.
#' @keywords internal
filter_data_at_covariate_reference <- function(state, data, hold_ids) {
  if (is.null(data) || !nrow(data) || !length(hold_ids)) {
    return(list(data = data, note = NULL, labels = character(), n_keep = nrow(data) %||% 0L))
  }
  ok <- rep(TRUE, nrow(data))
  labels <- character()
  for (pid in hold_ids) {
    m <- partner_reference_row_mask(state, data, pid)
    ok <- ok & m$ok
    if (!is.null(m$label) && nzchar(m$label)) labels <- c(labels, m$label)
  }
  labels <- unique(labels)
  note <- if (length(labels)) {
    paste0("holding ", paste(labels, collapse = ", "))
  } else {
    NULL
  }
  list(
    data = data[ok, , drop = FALSE],
    note = note,
    labels = labels,
    n_keep = sum(ok)
  )
}

#' @keywords internal
filter_data_at_interaction_reference <- function(state, data, partners) {
  hold_ids <- unique(vapply(partners, function(p) {
    as.character(p$partner_id %||% NA_character_)
  }, character(1)))
  hold_ids <- hold_ids[!is.na(hold_ids) & nzchar(hold_ids)]
  filter_data_at_covariate_reference(state, data, hold_ids)
}

#' Build a ggplot for one edge's observed relationship
#'
#' @param state Model state (estimates used in subtitle when present).
#' @param data Analysis data frame.
#' @param edge_id Edge id to plot.
#' @param show_fit If TRUE, overlay the chosen relationship smooth / group means.
#' @param rel_kind One of linear / quadratic / cubic / loess / none.
#' @param prior_mean Optional override for the unstandardized expected slope `b`
#'   (defaults to the edge's stored prior mean).
#' @param prior_sd Optional override for prior uncertainty (SD).
#' @param simulated If TRUE, titles say "simulated X → Y" (sim fallback data).
#' @return A ggplot object, or NULL if the edge cannot be plotted.
#' @keywords internal
edge_relationship_plot <- function(state, data, edge_id,
                                   show_fit = FALSE,
                                   rel_kind = NULL,
                                   prior_mean = NULL,
                                   prior_sd = NULL,
                                   simulated = FALSE,
                                   latent_score_method = "own") {
  if (is.null(state) || is.null(data) || is.null(edge_id) || !nzchar(edge_id)) {
    return(NULL)
  }
  idx <- match(edge_id, state$edges$id)
  if (is.na(idx)) return(NULL)

  from_id <- state$edges$from[idx]
  to_id <- state$edges$to[idx]
  level <- if ("level" %in% names(state$edges)) state$edges$level[idx] else NA_character_
  etype <- state$edges$type[idx]
  est <- state$edges$estimate[idx]
  est_std <- if ("estimate_std" %in% names(state$edges)) state$edges$estimate_std[idx] else NA_real_
  if (is.null(rel_kind) || !nzchar(as.character(rel_kind))) {
    rel_kind <- if ("rel_kind" %in% names(state$edges)) state$edges$rel_kind[idx] else "linear"
  }
  if (is.na(rel_kind) || !nzchar(rel_kind)) rel_kind <- "linear"
  pr <- edge_prior(state, edge_id)
  if (is.null(prior_mean) || !is.finite(prior_mean)) prior_mean <- pr$mean
  if (is.null(prior_sd) || !is.finite(prior_sd) || prior_sd <= 0) prior_sd <- pr$sd

  to_level <- if ("to_level" %in% names(state$edges)) state$edges$to_level[idx] else NA_character_

  if (is_interaction_node(state, to_id)) {
    return(edge_plot_message(
      "Definitional link",
      "This wire feeds the × operator. Click the × → DV link for the interaction plot."
    ))
  }

  # Latents → regression factor scores (same as two-step path stage)
  data <- enrich_data_with_factor_scores(
    state, data, score_method = latent_score_method
  )

  if (is_interaction_node(state, from_id)) {
    return(edge_interaction_plot(
      state, data, from_id, to_id, est, est_std, etype,
      show_fit = show_fit, rel_kind = rel_kind,
      to_level = to_level,
      prior_mean = prior_mean,
      prior_sd = prior_sd,
      simulated = simulated,
      latent_score_method = latent_score_method
    ))
  }

  from_col <- node_plot_column(state, from_id)
  to_col <- node_plot_column(state, to_id)
  from_lab <- node_plot_label(state, from_id)
  to_lab <- node_plot_label(state, to_id)

  if (is.na(from_col) || !nzchar(from_col) || is.na(to_col) || !nzchar(to_col)) {
    return(edge_plot_message(
      "Cannot plot",
      "This link needs observed variables or a latent with measurement indicators."
    ))
  }
  if (!from_col %in% names(data) || !to_col %in% names(data)) {
    return(edge_plot_message(
      "Missing scores / data",
      "Could not build factor scores for this latent (check measurement links and data)."
    ))
  }

  # Hold co-predictors at reference/mean so the display is a conditional effect
  hold_ids <- co_predictor_ids_to_hold(
    state, from_id, to_id, to_level = to_level
  )
  effect_kind <- if (length(hold_ids)) "conditional" else "marginal"
  effect_caption <- NULL
  if (length(hold_ids)) {
    filt <- filter_data_at_covariate_reference(state, data, hold_ids)
    if (is.null(filt$data) || !nrow(filt$data)) {
      labs <- vapply(hold_ids, function(id) node_plot_label(state, id), character(1))
      return(edge_plot_message(
        "No data at covariate reference",
        sprintf(
          "Conditional-effect plot needs cases with %s at reference (0 / base / mean).",
          paste(labs, collapse = " and ")
        )
      ))
    }
    data <- filt$data
    effect_caption <- edge_plot_effect_caption(effect_kind, filt$labels)
  } else {
    effect_caption <- edge_plot_effect_caption(effect_kind, character())
  }

  df <- data.frame(
    x_raw = data[[from_col]],
    y_raw = data[[to_col]],
    stringsAsFactors = FALSE
  )
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (!nrow(df)) {
    return(edge_plot_message("No data", "No complete cases for this pair."))
  }

  x_kind <- classify_variable(df$x_raw)
  y_kind <- classify_variable(df$y_raw)
  # Split categorical DV: plot one-vs-rest for the wired response level
  if (identical(y_kind, "categorical")) {
    if (is.na(to_level) || !nzchar(to_level)) {
      return(edge_plot_message(
        "Pick a response level",
        "Wire this link into a left-hand category port on the DV (split one-vs-rest)."
      ))
    }
    df$y_raw <- as.integer(as.character(df$y_raw) == as.character(to_level))
    y_kind <- "binary"
    to_lab <- dv_outcome_lhs(to_lab, to_level)
  } else if (identical(y_kind, "continuous")) {
    y_tr <- node_dv_transform(state, to_id)
    if (!identical(y_tr, "none")) {
      df$y_raw <- apply_dv_transform(df$y_raw, y_tr, yname = to_lab)
      to_lab <- dv_transform_lhs(to_lab, y_tr)
    }
  }
  subtitle <- edge_plot_subtitle(
    est, est_std, etype, level, show_fit, rel_kind,
    prior_mean = prior_mean, prior_sd = prior_sd
  )
  title <- edge_plot_effect_title(
    from_lab, to_lab, effect = effect_kind, simulated = simulated
  )

  if (!is.na(level) && nzchar(level) && identical(x_kind, "categorical")) {
    meta <- node_factor_coding(state, from_id)
    df$x_raw <- coerce_with_factor_coding(
      df$x_raw, base = meta$base, levels_order = meta$levels, ordered = meta$ordered
    )
    coding <- variable_coding(
      df$x_raw, base = meta$base, levels_order = meta$levels
    )
    base <- coding$base
    # Only contrast level vs reference — drop remaining levels (no residual "other" bin)
    xr <- as.character(df$x_raw)
    keep <- xr %in% c(base, level)
    df <- df[keep, , drop = FALSE]
    if (!nrow(df)) {
      return(edge_plot_message(
        "No data",
        sprintf("No cases in levels '%s' or '%s'.", level, base)
      ))
    }
    df$x_plot <- factor(as.character(df$x_raw), levels = unique(c(base, level)))
    x_label <- sprintf("%s (%s vs %s)", from_lab, level, base)
    return(edge_plot_group(
      df, to_lab, y_kind, x_label, subtitle,
      highlight = level, show_fit = show_fit, simulated = simulated,
      effect = effect_kind, caption = effect_caption, title_override = title
    ))
  }

  if (identical(x_kind, "categorical")) {
    df$x_plot <- if (is.factor(df$x_raw)) droplevels(df$x_raw) else factor(df$x_raw)
    return(edge_plot_group(
      df, to_lab, y_kind, from_lab, subtitle,
      highlight = NULL, show_fit = show_fit, simulated = simulated,
      effect = effect_kind, caption = effect_caption, title_override = title
    ))
  }

  if (identical(x_kind, "binary")) {
    df$x_plot <- factor(as_binary01(df$x_raw), levels = c(0, 1), labels = c("0", "1"))
    return(edge_plot_group(
      df, to_lab, y_kind, from_lab, subtitle,
      highlight = NULL, show_fit = show_fit, simulated = simulated,
      effect = effect_kind, caption = effect_caption, title_override = title
    ))
  }

  # Continuous predictor
  df$x_plot <- as.numeric(df$x_raw)
  if (identical(y_kind, "binary")) {
    df$y_plot <- as_binary01(df$y_raw)
    prior_line <- edge_prior_line_data(
      df$x_plot, df$y_plot, prior_mean, prior_sd = prior_sd
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_plot, y = .data$y_plot)) +
      ggplot2::geom_jitter(
        height = 0.04, width = 0, alpha = 0.35, size = 1.6, color = "#2E86AB"
      ) +
      ggplot2::coord_cartesian(ylim = c(-0.05, 1.05)) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = effect_caption,
        x = from_lab,
        y = sprintf("P(%s = 1)", to_lab)
      ) +
      edge_plot_theme()
    for (ly in edge_prior_overlay_layers(prior_line)) p <- p + ly
    if (isTRUE(show_fit)) {
      sm <- edge_smooth_layer(rel_kind, y_binary = TRUE)
      if (!is.null(sm)) p <- p + sm
    }
    p
  } else {
    df$y_plot <- as.numeric(df$y_raw)
    prior_line <- edge_prior_line_data(
      df$x_plot, df$y_plot, prior_mean, prior_sd = prior_sd
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_plot, y = .data$y_plot)) +
      ggplot2::geom_point(alpha = 0.35, size = 1.8, color = "#2E86AB") +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = effect_caption,
        x = from_lab,
        y = to_lab
      ) +
      edge_plot_theme()
    for (ly in edge_prior_overlay_layers(prior_line)) p <- p + ly
    if (isTRUE(show_fit)) {
      sm <- edge_smooth_layer(rel_kind, y_binary = FALSE)
      if (!is.null(sm)) p <- p + sm
    }
    p
  }
}

#' Smooth / polynomial overlay for continuous-x plots
#' @keywords internal
edge_smooth_layer <- function(rel_kind, y_binary = FALSE) {
  if (is.null(rel_kind)) return(NULL)
  col <- if (y_binary) "#C0392B" else "#1F7A4D"
  fill <- if (y_binary) "#C0392B22" else "#1F7A4D22"
  method <- if (y_binary) "glm" else "lm"
  method_args <- if (y_binary) list(family = stats::binomial()) else list()

  formula <- switch(
    rel_kind,
    linear = y ~ x,
    quadratic = y ~ poly(x, 2),
    cubic = y ~ poly(x, 3),
    loess = y ~ x,
    y ~ x
  )
  if (identical(rel_kind, "loess")) {
    if (y_binary) {
      # LOESS not ideal for binary; fall back to linear logit
      method <- "glm"
      method_args <- list(family = stats::binomial())
      formula <- y ~ x
    } else {
      method <- "loess"
      method_args <- list()
    }
  }

  ggplot2::geom_smooth(
    method = method,
    formula = formula,
    method.args = method_args,
    se = TRUE,
    color = col,
    fill = fill,
    linewidth = 1
  )
}

#' @keywords internal
edge_plot_pair_title <- function(from_lab, to_lab, simulated = FALSE) {
  if (isTRUE(simulated)) {
    sprintf("simulated %s \u2192 %s", from_lab, to_lab)
  } else {
    sprintf("%s \u2192 %s", from_lab, to_lab)
  }
}

#' Title for link plots: Conditional / Marginal / Interaction effects
#' @keywords internal
edge_plot_effect_title <- function(from_lab, to_lab,
                                   effect = c("conditional", "marginal", "interaction"),
                                   simulated = FALSE,
                                   moderator_lab = NULL) {
  effect <- match.arg(effect)
  pair <- if (identical(effect, "interaction") &&
                !is.null(moderator_lab) && nzchar(moderator_lab)) {
    paste(from_lab, "\u00D7", moderator_lab, "\u2192", to_lab)
  } else {
    paste0(from_lab, " \u2192 ", to_lab)
  }
  prefix <- switch(
    effect,
    conditional = "Conditional effect",
    marginal = "Marginal effect",
    interaction = "Conditional effects"
  )
  core <- paste0(prefix, ": ", pair)
  if (isTRUE(simulated)) paste("simulated", core) else core
}

#' Caption clarifying what the link plot represents
#' @keywords internal
edge_plot_effect_caption <- function(effect = c("conditional", "marginal", "interaction"),
                                     hold_labels = character()) {
  effect <- match.arg(effect)
  hold_labels <- hold_labels[!is.na(hold_labels) & nzchar(hold_labels)]
  if (identical(effect, "interaction")) {
    return("Conditional effects (simple slopes) by moderator level")
  }
  if (identical(effect, "conditional") && length(hold_labels)) {
    return(paste0("Conditional effect holding ", paste(hold_labels, collapse = ", ")))
  }
  if (identical(effect, "conditional")) {
    return("Conditional effect (co-predictors at reference)")
  }
  "Marginal effect (no co-predictors in this equation)"
}

edge_plot_group <- function(df, to_lab, y_kind, x_label, subtitle,
                            highlight = NULL, show_fit = FALSE,
                            simulated = FALSE,
                            effect = c("conditional", "marginal"),
                            caption = NULL,
                            title_override = NULL) {
  effect <- match.arg(effect)
  if (!is.null(highlight) && highlight %in% levels(df$x_plot)) {
    df$hl <- ifelse(as.character(df$x_plot) == highlight, "focus", "other")
  } else {
    df$hl <- "other"
  }
  title <- if (!is.null(title_override) && nzchar(title_override)) {
    title_override
  } else {
    edge_plot_effect_title(x_label, to_lab, effect = effect, simulated = simulated)
  }
  if (is.null(caption) || !nzchar(caption)) {
    caption <- edge_plot_effect_caption(effect)
  }

  if (identical(y_kind, "binary")) {
    df$y_plot <- as_binary01(df$y_raw)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_plot, y = .data$y_plot)) +
      ggplot2::geom_jitter(
        height = 0.04, width = 0.12, alpha = 0.35, size = 1.6,
        ggplot2::aes(color = .data$hl), show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(values = c(focus = "#5B2C8B", other = "#2E86AB")) +
      ggplot2::coord_cartesian(ylim = c(-0.05, 1.05)) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = caption,
        x = x_label,
        y = sprintf("P(%s = 1)", to_lab)
      ) +
      edge_plot_theme()
    if (isTRUE(show_fit)) {
      p <- p +
        ggplot2::stat_summary(fun = mean, geom = "point", size = 3, color = "#C0392B") +
        ggplot2::stat_summary(
          fun.data = mean_se_fun, geom = "errorbar", width = 0.15, color = "#C0392B"
        )
    }
    p
  } else {
    df$y_plot <- as.numeric(df$y_raw)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_plot, y = .data$y_plot)) +
      ggplot2::geom_jitter(
        width = 0.12, alpha = 0.35, size = 1.4, color = "#2E86AB"
      ) +
      ggplot2::scale_fill_manual(values = c(focus = "#D9C7F5", other = "#E8EEF5")) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = caption,
        x = x_label,
        y = to_lab
      ) +
      edge_plot_theme()
    if (isTRUE(show_fit)) {
      p <- p +
        ggplot2::geom_boxplot(
          width = 0.55, outlier.alpha = 0.35, color = "#4A5568",
          ggplot2::aes(fill = .data$hl), show.legend = FALSE, alpha = 0.55
        ) +
        ggplot2::stat_summary(fun = mean, geom = "point", size = 2.8, color = "#1F7A4D")
    }
    p
  }
}

#' @keywords internal
mean_se_fun <- function(x) {
  x <- x[is.finite(x)]
  m <- mean(x)
  if (!length(x)) return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))
  s <- stats::sd(x) / sqrt(length(x))
  if (!is.finite(s)) s <- 0
  data.frame(y = m, ymin = m - s, ymax = m + s)
}

#' Mean ± 95% CI (Student-t) for `stat_summary`
#' @keywords internal
mean_ci95_fun <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (!n) return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))
  m <- mean(x)
  if (n < 2L) return(data.frame(y = m, ymin = m, ymax = m))
  se <- stats::sd(x) / sqrt(n)
  if (!is.finite(se)) se <- 0
  tcrit <- stats::qt(0.975, df = n - 1L)
  data.frame(y = m, ymin = m - tcrit * se, ymax = m + tcrit * se)
}

#' Shared dodge width for categorical/binary-X interaction summaries
#' @keywords internal
interaction_cat_dodge_width <- function() 0.55

#' Per-level x offsets matching ggplot2::position_dodge(width)
#' @keywords internal
interaction_group_dodge_offsets <- function(group_levels,
                                            width = interaction_cat_dodge_width()) {
  levs <- as.character(group_levels)
  levs <- levs[!is.na(levs) & nzchar(levs)]
  n <- length(levs)
  if (!n) return(numeric())
  if (n == 1L) return(stats::setNames(0, levs))
  offs <- ((seq_len(n) - 0.5) / n - 0.5) * as.numeric(width)
  stats::setNames(offs, levs)
}

#' Jittered raw points + mean + 95% CI for categorical/binary X interaction plots
#' @keywords internal
interaction_categorical_x_layers <- function(y_binary = FALSE,
                                             dodge = interaction_cat_dodge_width()) {
  list(
    ggplot2::geom_point(
      position = ggplot2::position_jitterdodge(
        jitter.width = 0.16,
        jitter.height = if (isTRUE(y_binary)) 0.035 else 0,
        dodge.width = dodge
      ),
      alpha = 0.2,
      size = 1.45
    ),
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      size = 3.8,
      position = ggplot2::position_dodge(width = dodge)
    ),
    ggplot2::stat_summary(
      fun.data = mean_ci95_fun,
      geom = "errorbar",
      width = 0.16,
      linewidth = 0.75,
      position = ggplot2::position_dodge(width = dodge)
    )
  )
}

#' Fitted simple-slope line endpoints at dodged binary-X positions
#' @keywords internal
interaction_binary_x_fit_line_df <- function(plot_df,
                                             y_binary = FALSE,
                                             rel_kind = "linear",
                                             group_levels = NULL,
                                             dodge_width = interaction_cat_dodge_width()) {
  if (is.null(plot_df) || !nrow(plot_df)) return(NULL)
  if (is.null(group_levels) || !length(group_levels)) {
    group_levels <- if (is.factor(plot_df$z)) {
      levels(plot_df$z)
    } else {
      unique(as.character(plot_df$z))
    }
  }
  group_levels <- as.character(group_levels)
  offs <- interaction_group_dodge_offsets(group_levels, dodge_width)
  # Only two X levels — higher-order smooths are not identifiable
  sm_rel <- as.character(rel_kind %||% "linear")
  if (isTRUE(y_binary) || sm_rel %in% c("quadratic", "cubic", "loess")) {
    sm_rel <- "linear"
  }
  xs <- c(0, 1)
  out <- list()
  for (g in group_levels) {
    sub <- plot_df[as.character(plot_df$z) == g, , drop = FALSE]
    sub <- sub[is.finite(sub$x) & is.finite(sub$y), , drop = FALSE]
    if (nrow(sub) < 2L) next
    fit <- if (isTRUE(y_binary)) {
      tryCatch(
        stats::glm(y ~ x, data = sub, family = stats::binomial()),
        error = function(e) NULL
      )
    } else {
      tryCatch(stats::lm(y ~ x, data = sub), error = function(e) NULL)
    }
    if (is.null(fit)) next
    nd <- data.frame(x = xs)
    pred <- tryCatch({
      if (isTRUE(y_binary)) {
        as.numeric(stats::predict(fit, newdata = nd, type = "response"))
      } else {
        as.numeric(stats::predict(fit, newdata = nd))
      }
    }, error = function(e) NULL)
    if (is.null(pred) || length(pred) != 2L || any(!is.finite(pred))) next
    off <- unname(offs[[g]])
    if (length(off) != 1L || !is.finite(off)) off <- 0
    out[[length(out) + 1L]] <- data.frame(
      x = xs + off,
      y = pred,
      z = factor(g, levels = group_levels),
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

#' ggplot layers for dodged binary-X fitted simple slopes
#' @keywords internal
interaction_binary_x_fit_layers <- function(fit_df) {
  if (is.null(fit_df) || !nrow(fit_df)) return(list())
  list(
    ggplot2::geom_line(
      data = fit_df,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        color = .data$z, group = .data$z
      ),
      linewidth = 1,
      inherit.aes = FALSE
    )
  )
}

#' @keywords internal
edge_plot_subtitle <- function(est, est_std, etype, level, show_fit = FALSE,
                               rel_kind = "linear", prior_mean = NULL,
                               prior_sd = NULL) {
  parts <- character()
  if (!is.na(etype) && nzchar(etype) && !identical(etype, "definitional")) {
    parts <- c(parts, etype)
  }
  if (!is.na(level) && nzchar(level)) parts <- c(parts, paste0("level: ", level))
  if (is.finite(prior_mean)) {
    if (is.finite(prior_sd) && prior_sd > 0) {
      parts <- c(parts, sprintf("expected b = %.2f ± %.2f", prior_mean, prior_sd))
    } else {
      parts <- c(parts, sprintf("expected b = %.2f (dashed)", prior_mean))
    }
  }
  if (isTRUE(show_fit) && is.finite(est)) {
    parts <- c(parts, sprintf("b = %.3f", est))
  }
  if (isTRUE(show_fit) && is.finite(est_std)) {
    parts <- c(parts, sprintf("β = %.3f", est_std))
  }
  if (isTRUE(show_fit)) {
    parts <- c(parts, paste0("fit: ", rel_kind))
  } else if (!isTRUE(show_fit)) {
    parts <- c(parts, "raw data (run analysis to overlay fit)")
  }
  if (!length(parts)) return("Bivariate relationship")
  paste(parts, collapse = " · ")
}

#' @keywords internal
edge_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13, color = "#2B2B2B"),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#6B6B73"),
      plot.caption = ggplot2::element_text(
        size = 10, color = "#3F6F8A", face = "italic", hjust = 0,
        margin = ggplot2::margin(t = 6)
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#ECECEE"),
      axis.title = ggplot2::element_text(size = 11, color = "#4A4A4A"),
      plot.margin = ggplot2::margin(6, 10, 4, 10)
    )
}

#' Prior or fitted β for a structural main-effect edge into a DV
#' @keywords internal
main_effect_beta_for_dv <- function(state, from_id, to_id,
                                    to_level = NA_character_,
                                    which = c("prior", "estimate")) {
  which <- match.arg(which)
  if (is.null(state) || is.na(from_id) || is.na(to_id)) return(NA_real_)
  if (is_interaction_node(state, from_id) || is_interaction_node(state, to_id)) {
    return(NA_real_)
  }
  hits <- which(
    state$edges$from == from_id &
      state$edges$to == to_id &
      !state$edges$type %in% c("measurement", "definitional")
  )
  if (!length(hits)) return(NA_real_)
  if (!is.na(to_level) && nzchar(as.character(to_level)) &&
        "to_level" %in% names(state$edges)) {
    tl <- as.character(state$edges$to_level[hits])
    hits2 <- hits[is.na(tl) | !nzchar(tl) | tl == as.character(to_level)]
    if (length(hits2)) hits <- hits2
  }
  eid <- state$edges$id[[hits[[1]]]]
  if (identical(which, "prior")) {
    pr <- edge_prior(state, eid)
    return(as.numeric(pr$mean))
  }
  as.numeric(state$edges$estimate[[hits[[1]]]])
}

#' Clamp probabilities away from 0/1 for logit transforms
#' @keywords internal
clamp_prob <- function(p, eps = 1e-6) {
  p <- as.numeric(p)
  p[!is.finite(p)] <- NA_real_
  pmax(eps, pmin(1 - eps, p))
}

#' Map plot-scale y to linear predictor (identity or logit)
#' @keywords internal
interaction_arrow_to_eta <- function(y_plot, link = "identity") {
  if (identical(link, "logit")) {
    return(stats::qlogis(clamp_prob(y_plot)))
  }
  as.numeric(y_plot)
}

#' Map linear predictor back to plot scale
#' @keywords internal
interaction_arrow_from_eta <- function(eta, link = "identity") {
  if (identical(link, "logit")) {
    return(stats::plogis(as.numeric(eta)))
  }
  as.numeric(eta)
}

#' Arrow + ghost-line data for interaction correction vs main-effects-only
#'
#' Coefficients (`beta_*`) are on the model linear-predictor scale. For binary
#' DVs the plot is probability (`P(Y=1)`), so effects are applied on the logit
#' scale and mapped back with `plogis`.
#'
#' @param x Numeric x-axis values (0/1 if binary; raw if continuous).
#' @param z01 Moderator coded 0/1 (reference = 0).
#' @param y Outcome on the plot scale (probability for binary DV).
#' @param link `"identity"` or `"logit"` (logistic / binomial DV plots).
#' @param active_group Label of the moderator level the correction applies to.
#' @param group_levels Full moderator level order (matches plot colour scale).
#' @return data.frame with ghost start (`x0`,`y0`), arrow (`x`,`y`,`yend`),
#'   `label`, `group`, or NULL.
#' @keywords internal
interaction_correction_arrow_df <- function(x, z01, y,
                                            beta_ix,
                                            beta_x = 0,
                                            beta_z = 0,
                                            x_is_binary = FALSE,
                                            link = c("identity", "logit"),
                                            active_group = NULL,
                                            group_levels = NULL) {
  link <- match.arg(link)
  if (!is.finite(beta_ix)) return(NULL)
  if (!is.finite(beta_x)) beta_x <- 0
  if (!is.finite(beta_z)) beta_z <- 0
  x <- as.numeric(x)
  z01 <- as.numeric(z01)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(z01) & is.finite(y)
  if (sum(ok) < 4L) return(NULL)
  x <- x[ok]
  z01 <- z01[ok]
  y <- y[ok]

  if (is.null(active_group) || !nzchar(as.character(active_group))) {
    active_group <- "1"
  }
  active_group <- as.character(active_group)

  if (isTRUE(x_is_binary)) {
    ref <- x <= 0.5 & z01 <= 0.5
    act0 <- x <= 0.5 & z01 > 0.5
    x_star <- 1
    z_star <- 1
    prod <- x_star * z_star
    delta <- beta_ix * prod
    if (!is.finite(delta) || abs(delta) < 1e-9) return(NULL)
    # Ghost must start on the active-group mean dot at x=0 (same summary
    # point as stat_summary), not a reference-cell + β_z reconstruction.
    if (sum(act0) >= 1L) {
      y0 <- mean(y[act0])
      y_ref <- if (sum(ref) >= 1L) mean(y[ref]) else y0
    } else if (sum(ref) >= 2L) {
      y_ref <- mean(y[ref])
      y0 <- interaction_arrow_from_eta(
        interaction_arrow_to_eta(y_ref, link) + beta_z * z_star,
        link
      )
    } else {
      return(NULL)
    }
    x0 <- 0
    eta0 <- interaction_arrow_to_eta(y0, link)
    eta_mains <- eta0 + beta_x * x_star
    eta_full <- eta_mains + delta
    y_mains <- interaction_arrow_from_eta(eta_mains, link)
    y_full <- interaction_arrow_from_eta(eta_full, link)
  } else {
    # Continuous x: product uses mean-centered x (fit coding)
    x_mu <- mean(x)
    x_c <- x - x_mu
    act <- z01 > 0.5
    ref <- z01 <= 0.5
    if (sum(act) < 2L || sum(ref) < 2L) return(NULL)
    x_star <- mean(x[act])
    x_star_c <- mean(x_c[act])
    z_star <- 1
    prod <- x_star_c * z_star
    delta <- beta_ix * prod
    if (!is.finite(delta) || abs(delta) < 1e-9) return(NULL)
    y_ref <- mean(y[ref])
    eta_ref <- interaction_arrow_to_eta(y_ref, link)
    eta_mains <- eta_ref + beta_x * x_star_c + beta_z * z_star
    eta_full <- eta_mains + delta
    y_mains <- interaction_arrow_from_eta(eta_mains, link)
    y_full <- interaction_arrow_from_eta(eta_full, link)
    # Ghost from reference-region center along mains-only active-group line
    x0 <- mean(x[ref])
    eta0 <- eta_ref + beta_x * (x0 - x_mu) + beta_z * z_star
    y0 <- interaction_arrow_from_eta(eta0, link)
  }

  delta_plot <- y_full - y_mains
  if (exists("smt_debug", mode = "function")) {
    smt_debug(
      "ix correction arrow | link=", link,
      " x_bin=", isTRUE(x_is_binary),
      " x0=", round(x0, 4), " y0=", round(y0, 4),
      " y_ref=", round(y_ref, 4),
      " y_mains=", round(y_mains, 4),
      " y_full=", round(y_full, 4),
      " delta_lp=", round(delta, 4),
      " delta_plot=", round(delta_plot, 4)
    )
  }

  out <- data.frame(
    x0 = x0,
    y0 = y0,
    x = x_star,
    y = y_mains,
    yend = y_full,
    # Label matches the visible arrow length on the plot scale
    label = sprintf("\u00D7 %+.2f", delta_plot),
    group = active_group,
    stringsAsFactors = FALSE
  )
  if (!is.null(group_levels) && length(group_levels)) {
    out$group <- factor(out$group, levels = as.character(group_levels))
  }
  out
}

#' ggplot layers for interaction correction arrow + mains-only ghost line
#' @keywords internal
interaction_correction_arrow_layers <- function(arrow_df,
                                                linetype = "solid",
                                                linewidth = 0.8,
                                                x_offset = NULL,
                                                ghost_alpha = 0.55,
                                                arrow_alpha = 0.95) {
  if (is.null(arrow_df) || !nrow(arrow_df)) return(list())
  need <- c("x0", "y0", "x", "y", "yend", "label", "group")
  if (!all(need %in% names(arrow_df))) return(list())

  xr <- range(c(arrow_df$x0, arrow_df$x), na.rm = TRUE)
  span <- diff(xr)
  if (!is.finite(span) || span <= 0) span <- 1
  if (is.null(x_offset) || !is.finite(x_offset)) {
    # Small nudge; keep room for the inward label
    x_offset <- 0.045 * span
  }
  arrow_df$x_arr <- arrow_df$x + x_offset
  # Label sits to the right of the correction arrow
  arrow_df$x_lab <- arrow_df$x_arr + 0.02 * span

  ghost_df <- data.frame(
    x = arrow_df$x0,
    xend = arrow_df$x_arr,
    y = arrow_df$y0,
    yend = arrow_df$y,
    group = arrow_df$group,
    stringsAsFactors = FALSE
  )

  list(
    # Counterfactual active-group line without the interaction term
    ggplot2::geom_segment(
      data = ghost_df,
      ggplot2::aes(
        x = .data$x, xend = .data$xend,
        y = .data$y, yend = .data$yend,
        color = .data$group
      ),
      linetype = "dashed",
      linewidth = max(0.55, linewidth * 0.85),
      alpha = ghost_alpha,
      inherit.aes = FALSE,
      show.legend = FALSE
    ),
    ggplot2::geom_segment(
      data = arrow_df,
      ggplot2::aes(
        x = .data$x_arr, xend = .data$x_arr,
        y = .data$y, yend = .data$yend,
        color = .data$group
      ),
      arrow = ggplot2::arrow(length = grid::unit(0.18, "cm"), type = "closed"),
      linetype = linetype,
      linewidth = linewidth,
      alpha = arrow_alpha,
      inherit.aes = FALSE,
      show.legend = FALSE
    ),
    ggplot2::geom_text(
      data = arrow_df,
      ggplot2::aes(
        x = .data$x_lab,
        y = (.data$y + .data$yend) / 2,
        label = .data$label,
        color = .data$group
      ),
      size = 3.1,
      hjust = -0.05,
      vjust = 0.5,
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' Build prior or fit correction arrows for an interaction plot
#'
#' Draws a single correction (fit estimate when available, otherwise prior)
#' so prior and fit do not stack as a double ghost/arrow.
#' @keywords internal
interaction_correction_arrows_for_plot <- function(state, ix_id, to_id, to_level,
                                                   x_id, z_id,
                                                   x, z01, y,
                                                   x_is_binary,
                                                   prior_mean,
                                                   est,
                                                   show_fit = FALSE,
                                                   active_group = NULL,
                                                   group_levels = NULL,
                                                   y_binary = FALSE) {
  # Ghost + correction arrow only when the × → DV link is kept (not excluded)
  if (!is.null(state) && nrow(state$edges)) {
    state <- ensure_edge_to_port(state)
    hits <- which(
      state$edges$from == ix_id &
        state$edges$to == to_id &
        !state$edges$type %in% c("measurement", "definitional")
    )
    if (length(hits) && !is.na(to_level) && nzchar(as.character(to_level)) &&
          "to_level" %in% names(state$edges)) {
      tl <- as.character(state$edges$to_level[hits])
      hits2 <- hits[is.na(tl) | !nzchar(tl) | tl == as.character(to_level)]
      if (length(hits2)) hits <- hits2
    }
    if (length(hits)) {
      st_edge <- as.character(state$edges$status[[hits[[1]]]])
      if (identical(st_edge, "excluded")) return(list())
    }
  }

  use_fit <- isTRUE(show_fit) && is.finite(est)
  if (use_fit) {
    beta_ix <- as.numeric(est)
    beta_x <- main_effect_beta_for_dv(state, x_id, to_id, to_level, "estimate")
    beta_z <- main_effect_beta_for_dv(state, z_id, to_id, to_level, "estimate")
    lt <- "solid"
    lw <- 0.9
  } else if (is.finite(prior_mean)) {
    beta_ix <- as.numeric(prior_mean)
    beta_x <- main_effect_beta_for_dv(state, x_id, to_id, to_level, "prior")
    beta_z <- main_effect_beta_for_dv(state, z_id, to_id, to_level, "prior")
    lt <- "dashed"
    lw <- 0.75
  } else {
    return(list())
  }

  link <- if (isTRUE(y_binary)) "logit" else "identity"
  ad <- interaction_correction_arrow_df(
    x, z01, y,
    beta_ix = beta_ix,
    beta_x = beta_x,
    beta_z = beta_z,
    x_is_binary = x_is_binary,
    link = link,
    active_group = active_group,
    group_levels = group_levels
  )
  # Shift ghost/arrow onto the dodged active-group track (binary/categorical X)
  if (!is.null(ad) && isTRUE(x_is_binary) &&
        !is.null(group_levels) && length(group_levels) &&
        !is.null(active_group) && nzchar(as.character(active_group))) {
    offs <- interaction_group_dodge_offsets(
      group_levels, interaction_cat_dodge_width()
    )
    off <- unname(offs[[as.character(active_group)]])
    if (length(off) == 1L && is.finite(off) && abs(off) > 0) {
      ad$x0 <- ad$x0 + off
      ad$x <- ad$x + off
    }
  }
  interaction_correction_arrow_layers(
    ad, linetype = lt, linewidth = lw, ghost_alpha = 0.55
  )
}

#' Level labels for a binary node matching `as_binary01` coding (0, then 1)
#' @keywords internal
binary_plot_level_labels <- function(state, node_id, x = NULL) {
  levs <- character()
  base <- NA_character_
  if (!is.null(state) && !is.na(node_id) && nzchar(as.character(node_id))) {
    meta <- node_factor_coding(state, node_id)
    if (length(meta$levels)) levs <- as.character(meta$levels)
    if (!is.na(meta$base) && nzchar(meta$base)) base <- as.character(meta$base)
    if (!length(levs)) {
      sim_levs <- tryCatch(
        node_sim_factor_levels(state, node_id, data = NULL),
        error = function(e) character()
      )
      if (length(sim_levs)) levs <- as.character(sim_levs)
    }
  }
  if (!length(levs) && !is.null(x)) {
    if (is.factor(x)) {
      levs <- levels(droplevels(x))
    } else {
      levs <- unique(as.character(stats::na.omit(x)))
      # Match as_binary01(character): sorted unique, second → 1
      levs <- sort(levs)
    }
  }
  if (!length(levs)) return(c("0", "1"))
  if (!is.na(base) && nzchar(base) && base %in% levs) {
    levs <- c(base, setdiff(levs, base))
  } else if (is.factor(x)) {
    # as_binary01(factor): first level → 0
    levs <- levels(droplevels(x))
  }
  if (length(levs) == 1L) levs <- c(levs[[1]], levs[[1]])
  c(as.character(levs[[1]]), as.character(levs[[min(2L, length(levs))]]))
}

#' Simple-slopes plot for an interaction product → DV edge
#' @keywords internal
edge_interaction_plot <- function(state, data, ix_id, to_id, est, est_std, etype,
                                  show_fit = FALSE, rel_kind = "linear",
                                  to_level = NA_character_,
                                  prior_mean = NULL,
                                  prior_sd = NULL,
                                  simulated = FALSE,
                                  latent_score_method = "own") {
  state <- ensure_node_kind_columns(state)
  data <- enrich_data_with_factor_scores(
    state, data, score_method = latent_score_method
  )
  fac <- interaction_factors(state, ix_id)
  fa <- fac$a
  fb <- fac$b
  if (is.na(fa) || is.na(fb) || !nzchar(fa) || !nzchar(fb)) {
    return(edge_plot_message(
      "Incomplete interaction",
      "Wire two predictors into the × input ports first."
    ))
  }
  col_a <- node_plot_column(state, fa)
  col_b <- node_plot_column(state, fb)
  col_y <- node_plot_column(state, to_id)
  if (any(is.na(c(col_a, col_b, col_y))) ||
        !all(c(col_a, col_b, col_y) %in% names(data))) {
    return(edge_plot_message(
      "Missing scores / data",
      "Interaction factors or DV unavailable (latents need measurement indicators)."
    ))
  }

  # Hold other equation co-predictors at reference (not the × factors themselves)
  hold_ids <- co_predictor_ids_to_hold(
    state, ix_id, to_id, to_level = to_level
  )
  ix_hold_labels <- character()
  if (length(hold_ids)) {
    filt <- filter_data_at_covariate_reference(state, data, hold_ids)
    if (is.null(filt$data) || !nrow(filt$data)) {
      labs <- vapply(hold_ids, function(id) node_plot_label(state, id), character(1))
      return(edge_plot_message(
        "No data at covariate reference",
        sprintf(
          "Conditional interaction plot needs cases with %s at reference.",
          paste(labs, collapse = " and ")
        )
      ))
    }
    data <- filt$data
    ix_hold_labels <- filt$labels %||% character()
  }
  ix_caption <- edge_plot_effect_caption("interaction")
  if (length(ix_hold_labels)) {
    ix_caption <- paste0(ix_caption, " · holding ", paste(ix_hold_labels, collapse = ", "))
  }

  df <- data.frame(
    a = data[[col_a]],
    b = data[[col_b]],
    y = data[[col_y]],
    stringsAsFactors = FALSE
  )
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (!nrow(df)) {
    return(edge_plot_message("No data", "No complete cases for this interaction."))
  }

  y_kind <- classify_variable(df$y)
  y_lab <- node_plot_label(state, to_id)
  if (identical(y_kind, "categorical")) {
    if (is.na(to_level) || !nzchar(to_level)) {
      return(edge_plot_message(
        "Pick a response level",
        "Wire × into a left-hand category port on the DV (split one-vs-rest)."
      ))
    }
    df$y <- as.integer(as.character(df$y) == as.character(to_level))
    y_kind <- "binary"
    y_lab <- dv_outcome_lhs(y_lab, to_level)
  } else if (identical(y_kind, "binary")) {
    df$y <- as_binary01(df$y)
  } else {
    y_tr <- node_dv_transform(state, to_id)
    df$y <- apply_dv_transform(as.numeric(df$y), y_tr, yname = y_lab)
    y_lab <- dv_transform_lhs(y_lab, y_tr)
  }

  a_kind <- classify_variable(df$a)
  b_kind <- classify_variable(df$b)
  fac_w <- interaction_factors(state, ix_id)
  x <- NULL
  z_raw <- NULL
  x_lab <- NULL
  z_lab <- NULL
  x_id <- NA_character_
  z_id <- NA_character_
  x_is_binary <- FALSE

  # Prefer continuous / binary as the x-axis when one side is categorical
  if (identical(a_kind, "categorical") || identical(b_kind, "categorical")) {
    # Use the continuous/binary factor as x when possible; otherwise message
    if (identical(a_kind, "categorical") &&
          (identical(b_kind, "continuous") || identical(b_kind, "binary"))) {
      # already set x from b in the else branch? re-resolve:
      x <- factor_numeric_for_product(df$b, center = FALSE)
      z_raw <- df$a
      x_lab <- node_plot_label(state, fb)
      z_lab <- node_plot_label(state, fa)
      x_id <- fb
      z_id <- fa
      x_is_binary <- identical(b_kind, "binary")
      if (!is.na(fac_w$level_a) && nzchar(fac_w$level_a)) {
        keep_lv <- as.character(df$a) == fac_w$level_a |
          as.character(df$a) == variable_coding(df$a)$base
        df <- df[keep_lv, , drop = FALSE]
        x <- factor_numeric_for_product(df$b, center = FALSE)
        z_raw <- df$a
        z_lab <- paste0(node_plot_label(state, fa), "[", fac_w$level_a, "]")
      }
    } else if (identical(b_kind, "categorical") &&
                 (identical(a_kind, "continuous") || identical(a_kind, "binary"))) {
      x <- factor_numeric_for_product(df$a, center = FALSE)
      z_raw <- df$b
      x_lab <- node_plot_label(state, fa)
      z_lab <- node_plot_label(state, fb)
      x_id <- fa
      z_id <- fb
      x_is_binary <- identical(a_kind, "binary")
      if (!is.na(fac_w$level_b) && nzchar(fac_w$level_b)) {
        keep_lv <- as.character(df$b) == fac_w$level_b |
          as.character(df$b) == variable_coding(df$b)$base
        df <- df[keep_lv, , drop = FALSE]
        x <- factor_numeric_for_product(df$a, center = FALSE)
        z_raw <- df$b
        z_lab <- paste0(node_plot_label(state, fb), "[", fac_w$level_b, "]")
      }
    } else {
      return(edge_plot_message(
        "Categorical \u00D7 categorical",
        "Plotting two categorical factors in an interaction is not supported yet."
      ))
    }
    if (!nrow(df)) {
      return(edge_plot_message("No data", "No cases for this interaction contrast."))
    }
  } else if (identical(a_kind, "continuous") ||
               (identical(a_kind, "binary") && !identical(b_kind, "continuous"))) {
    x <- factor_numeric_for_product(df$a, center = FALSE)
    z_raw <- df$b
    x_lab <- node_plot_label(state, fa)
    z_lab <- node_plot_label(state, fb)
    x_id <- fa
    z_id <- fb
    x_is_binary <- identical(a_kind, "binary")
  } else {
    x <- factor_numeric_for_product(df$b, center = FALSE)
    z_raw <- df$a
    x_lab <- node_plot_label(state, fb)
    z_lab <- node_plot_label(state, fa)
    x_id <- fb
    z_id <- fa
    x_is_binary <- identical(b_kind, "binary")
  }
  z_kind <- classify_variable(z_raw)
  if (identical(z_kind, "categorical")) {
    # Moderator is categorical — one smooth per level (incl. base)
    coding_z <- variable_coding(z_raw)
    z_fac <- factor(as.character(z_raw), levels = c(coding_z$base, coding_z$levels))
    plot_df <- data.frame(x = x, y = df$y, z = z_fac, stringsAsFactors = FALSE)
    plot_df <- plot_df[stats::complete.cases(plot_df), , drop = FALSE]
    subtitle <- edge_plot_subtitle(
      est, est_std, etype, NA_character_, show_fit, rel_kind,
      prior_mean = prior_mean, prior_sd = prior_sd
    )
    ylab <- if (identical(y_kind, "binary")) {
      sprintf("P(%s = 1)", y_lab)
    } else {
      y_lab
    }
    ix_title <- edge_plot_effect_title(
      x_lab, y_lab, effect = "interaction",
      simulated = simulated, moderator_lab = z_lab
    )
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$z, fill = .data$z, group = .data$z)
    )
    if (isTRUE(x_is_binary)) {
      for (ly in interaction_categorical_x_layers(y_binary = identical(y_kind, "binary"))) {
        p <- p + ly
      }
    } else {
      p <- p + ggplot2::geom_point(alpha = 0.55, size = 1.8)
    }
    p <- p +
      ggplot2::labs(
        title = ix_title,
        subtitle = subtitle,
        caption = ix_caption,
        x = x_lab,
        y = ylab,
        color = z_lab,
        fill = z_lab
      ) +
      edge_plot_theme() +
      ggplot2::theme(legend.position = "bottom")
    if (isTRUE(x_is_binary)) {
      x_labs <- binary_plot_level_labels(state, x_id, df[[if (identical(x_id, fa)) "a" else "b"]])
      p <- p + ggplot2::scale_x_continuous(breaks = c(0, 1), labels = x_labs)
    }
    z_levs <- levels(z_fac)
    if (isTRUE(show_fit)) {
      if (isTRUE(x_is_binary)) {
        fit_df <- interaction_binary_x_fit_line_df(
          plot_df,
          y_binary = identical(y_kind, "binary"),
          rel_kind = rel_kind,
          group_levels = z_levs
        )
        for (ly in interaction_binary_x_fit_layers(fit_df)) p <- p + ly
      } else {
        sm_rel <- if (identical(rel_kind, "loess") && identical(y_kind, "binary")) {
          "linear"
        } else {
          rel_kind
        }
        formula <- switch(
          sm_rel,
          quadratic = y ~ poly(x, 2),
          cubic = y ~ poly(x, 3),
          y ~ x
        )
        method <- if (identical(y_kind, "binary")) {
          "glm"
        } else if (identical(sm_rel, "loess")) {
          "loess"
        } else {
          "lm"
        }
        method_args <- if (identical(y_kind, "binary")) {
          list(family = stats::binomial())
        } else {
          list()
        }
        # Do not hard-code color — inherit group aesthetics so each level gets a line
        p <- p + ggplot2::geom_smooth(
          method = method,
          formula = formula,
          method.args = method_args,
          se = TRUE,
          linewidth = 1,
          alpha = 0.15
        )
      }
    }
    # Interaction correction vs main effects (categorical moderator → 0/1 vs base)
    z01_cat <- as.numeric(z_fac != levels(z_fac)[[1]])
    active_z <- if (length(z_levs) >= 2L) z_levs[[2]] else z_levs[[1]]
    for (ly in interaction_correction_arrows_for_plot(
      state, ix_id, to_id, to_level,
      x_id = x_id, z_id = z_id,
      x = x, z01 = z01_cat, y = df$y,
      x_is_binary = x_is_binary,
      prior_mean = prior_mean, est = est, show_fit = show_fit,
      active_group = active_z, group_levels = z_levs,
      y_binary = identical(y_kind, "binary")
    )) {
      p <- p + ly
    }
    return(p)
  }
  z01_bin <- NULL
  if (identical(z_kind, "binary")) {
    z_src <- if (identical(z_id, fa)) df$a else if (identical(z_id, fb)) df$b else z_raw
    z_labs <- binary_plot_level_labels(state, z_id, z_src)
    z01_bin <- as_binary01(z_raw)
    z_fac <- factor(z01_bin, levels = c(0, 1), labels = z_labs)
  } else {
    z_num <- as.numeric(z_raw)
    mu <- mean(z_num)
    s <- stats::sd(z_num)
    if (!is.finite(s) || s < .Machine$double.eps) {
      z_fac <- factor(rep(sprintf("%s (mean)", z_lab), length(z_num)))
    } else {
      lo <- mu - s
      hi <- mu + s
      keep <- z_num <= lo | z_num >= hi
      if (sum(keep) < 8L) {
        keep <- rep(TRUE, length(z_num))
        z_fac <- factor(
          ifelse(z_num <= mu, sprintf("%s (low)", z_lab), sprintf("%s (high)", z_lab))
        )
      } else {
        df <- df[keep, , drop = FALSE]
        x <- x[keep]
        z_num <- z_num[keep]
        z_fac <- factor(
          ifelse(z_num <= lo, sprintf("%s (−1 SD)", z_lab), sprintf("%s (+1 SD)", z_lab))
        )
      }
    }
  }

  plot_df <- data.frame(x = x, y = df$y, z = z_fac)
  subtitle <- edge_plot_subtitle(
    est, est_std, etype, NA_character_, show_fit, rel_kind,
    prior_mean = prior_mean, prior_sd = prior_sd
  )
  ylab <- if (identical(y_kind, "binary")) {
    sprintf("P(%s = 1)", y_lab)
  } else {
    y_lab
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y, color = .data$z, group = .data$z))
  if (isTRUE(x_is_binary)) {
    for (ly in interaction_categorical_x_layers(y_binary = identical(y_kind, "binary"))) {
      p <- p + ly
    }
  } else {
    p <- p + ggplot2::geom_point(alpha = 0.3, size = 1.6)
  }
  p <- p +
    ggplot2::labs(
      title = edge_plot_effect_title(
        x_lab, y_lab, effect = "interaction",
        simulated = simulated, moderator_lab = z_lab
      ),
      subtitle = subtitle,
      caption = ix_caption,
      x = x_lab,
      y = ylab,
      color = z_lab
    ) +
    edge_plot_theme() +
    ggplot2::theme(legend.position = "bottom")

  if (isTRUE(x_is_binary)) {
    x_src <- if (identical(x_id, fa)) df$a else if (identical(x_id, fb)) df$b else NULL
    x_labs <- binary_plot_level_labels(state, x_id, x_src)
    p <- p + ggplot2::scale_x_continuous(breaks = c(0, 1), labels = x_labs)
  }

  z_levs <- levels(z_fac)
  if (isTRUE(show_fit)) {
    if (isTRUE(x_is_binary)) {
      fit_df <- interaction_binary_x_fit_line_df(
        plot_df,
        y_binary = identical(y_kind, "binary"),
        rel_kind = rel_kind,
        group_levels = z_levs
      )
      for (ly in interaction_binary_x_fit_layers(fit_df)) p <- p + ly
    } else {
      sm_rel <- if (identical(rel_kind, "loess") && identical(y_kind, "binary")) {
        "linear"
      } else {
        rel_kind
      }
      formula <- switch(
        sm_rel,
        quadratic = y ~ poly(x, 2),
        cubic = y ~ poly(x, 3),
        y ~ x
      )
      method <- if (identical(y_kind, "binary")) {
        "glm"
      } else if (identical(sm_rel, "loess")) {
        "loess"
      } else {
        "lm"
      }
      method_args <- if (identical(y_kind, "binary")) {
        list(family = stats::binomial())
      } else {
        list()
      }
      p <- p + ggplot2::geom_smooth(
        method = method,
        formula = formula,
        method.args = method_args,
        se = TRUE,
        linewidth = 1
      )
    }
  }

  z01_plot <- if (!is.null(z01_bin)) {
    as.numeric(z01_bin)
  } else {
    # Continuous moderator split into low/high: treat high (+1 SD) as 1
    as.numeric(grepl("\\+|high", as.character(z_fac), ignore.case = TRUE))
  }
  active_z <- if (!is.null(z01_bin) && length(z_levs) >= 2L) {
    z_levs[[2]]
  } else if (length(z_levs) >= 2L) {
    hit <- grepl("\\+|high", z_levs, ignore.case = TRUE)
    if (any(hit)) z_levs[[which(hit)[[1]]]] else z_levs[[length(z_levs)]]
  } else {
    z_levs[[1]]
  }
  for (ly in interaction_correction_arrows_for_plot(
    state, ix_id, to_id, to_level,
    x_id = x_id, z_id = z_id,
    x = x, z01 = z01_plot, y = df$y,
    x_is_binary = x_is_binary,
    prior_mean = prior_mean, est = est, show_fit = show_fit,
    active_group = active_z, group_levels = z_levs,
    y_binary = identical(y_kind, "binary")
  )) {
    p <- p + ly
  }
  p
}

#' Placeholder ggplot with a short message
#' @keywords internal
edge_plot_message <- function(title, detail) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.58, label = title,
      size = 4.2, fontface = "bold", color = "#3D3D3D"
    ) +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.42, label = detail,
      size = 3.6, color = "#6B6B73"
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 8, 8, 8))
}

#' Default empty panel under the canvas
#' @keywords internal
edge_plot_placeholder <- function() {
  edge_plot_message(
    "Inspect a link",
    "Select a link to explore the raw relationship. Run analysis to overlay a fit."
  )
}
