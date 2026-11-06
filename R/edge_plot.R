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

#' Build a ggplot for one edge's observed relationship
#'
#' @param state Model state (estimates used in subtitle when present).
#' @param data Analysis data frame.
#' @param edge_id Edge id to plot.
#' @param show_fit If TRUE, overlay the chosen relationship smooth / group means.
#' @param rel_kind One of linear / quadratic / cubic / loess / none.
#' @return A ggplot object, or NULL if the edge cannot be plotted.
#' @keywords internal
edge_relationship_plot <- function(state, data, edge_id,
                                   show_fit = FALSE,
                                   rel_kind = NULL) {
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

  to_col <- state$nodes$observed_column[match(to_id, state$nodes$id)]
  from_lab <- node_display_name(state, from_id)
  to_lab <- node_display_name(state, to_id)

  if (is_interaction_node(state, to_id)) {
    return(edge_plot_message(
      "Definitional link",
      "This wire feeds the × operator. Click the × → DV link for the interaction plot."
    ))
  }

  if (is_interaction_node(state, from_id)) {
    return(edge_interaction_plot(
      state, data, from_id, to_id, est, est_std, etype,
      show_fit = show_fit, rel_kind = rel_kind
    ))
  }

  from_col <- state$nodes$observed_column[match(from_id, state$nodes$id)]

  if (is.na(from_col) || !nzchar(from_col) || is.na(to_col) || !nzchar(to_col)) {
    return(edge_plot_message(
      "Observed variables only",
      "This link involves a latent — no raw bivariate plot."
    ))
  }
  if (!from_col %in% names(data) || !to_col %in% names(data)) {
    return(edge_plot_message("Missing data", "Mapped columns not found in the loaded data."))
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
  subtitle <- edge_plot_subtitle(est, est_std, etype, level, show_fit, rel_kind)

  if (identical(y_kind, "categorical")) {
    return(edge_plot_message(
      "Unsupported DV",
      "Multi-level categorical outcomes are not plotted."
    ))
  }

  if (!is.na(level) && nzchar(level) && identical(x_kind, "categorical")) {
    coding <- variable_coding(df$x_raw)
    base <- coding$base
    df$x_plot <- factor(
      ifelse(
        as.character(df$x_raw) == level, level,
        ifelse(as.character(df$x_raw) == base, base, "other")
      ),
      levels = unique(c(base, level, "other"))
    )
    x_label <- sprintf("%s (%s vs %s)", from_lab, level, base)
    return(edge_plot_group(
      df, to_lab, y_kind, x_label, subtitle,
      highlight = level, show_fit = show_fit
    ))
  }

  if (identical(x_kind, "categorical")) {
    df$x_plot <- if (is.factor(df$x_raw)) droplevels(df$x_raw) else factor(df$x_raw)
    return(edge_plot_group(
      df, to_lab, y_kind, from_lab, subtitle,
      highlight = NULL, show_fit = show_fit
    ))
  }

  if (identical(x_kind, "binary")) {
    df$x_plot <- factor(as_binary01(df$x_raw), levels = c(0, 1), labels = c("0", "1"))
    return(edge_plot_group(
      df, to_lab, y_kind, from_lab, subtitle,
      highlight = NULL, show_fit = show_fit
    ))
  }

  # Continuous predictor
  df$x_plot <- as.numeric(df$x_raw)
  title <- sprintf("%s → %s", from_lab, to_lab)
  if (identical(y_kind, "binary")) {
    df$y_plot <- as_binary01(df$y_raw)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_plot, y = .data$y_plot)) +
      ggplot2::geom_jitter(
        height = 0.04, width = 0, alpha = 0.35, size = 1.6, color = "#2E86AB"
      ) +
      ggplot2::coord_cartesian(ylim = c(-0.05, 1.05)) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = from_lab,
        y = sprintf("P(%s = 1)", to_lab)
      ) +
      edge_plot_theme()
    if (isTRUE(show_fit)) {
      sm <- edge_smooth_layer(rel_kind, y_binary = TRUE)
      if (!is.null(sm)) p <- p + sm
    }
    p
  } else {
    df$y_plot <- as.numeric(df$y_raw)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_plot, y = .data$y_plot)) +
      ggplot2::geom_point(alpha = 0.35, size = 1.8, color = "#2E86AB") +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = from_lab,
        y = to_lab
      ) +
      edge_plot_theme()
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
edge_plot_group <- function(df, to_lab, y_kind, x_label, subtitle,
                            highlight = NULL, show_fit = FALSE) {
  if (!is.null(highlight) && highlight %in% levels(df$x_plot)) {
    df$hl <- ifelse(as.character(df$x_plot) == highlight, "focus", "other")
  } else {
    df$hl <- "other"
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
        title = sprintf("%s → %s", x_label, to_lab),
        subtitle = subtitle,
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
        title = sprintf("%s → %s", x_label, to_lab),
        subtitle = subtitle,
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
  m <- mean(x)
  s <- stats::sd(x) / sqrt(length(x))
  data.frame(y = m, ymin = m - s, ymax = m + s)
}

#' @keywords internal
edge_plot_subtitle <- function(est, est_std, etype, level, show_fit = FALSE, rel_kind = "linear") {
  parts <- character()
  if (!is.na(etype) && nzchar(etype) && !identical(etype, "definitional")) {
    parts <- c(parts, etype)
  }
  if (!is.na(level) && nzchar(level)) parts <- c(parts, paste0("level: ", level))
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
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#ECECEE"),
      axis.title = ggplot2::element_text(size = 11, color = "#4A4A4A"),
      plot.margin = ggplot2::margin(6, 10, 4, 10)
    )
}

#' Simple-slopes plot for an interaction product → DV edge
#' @keywords internal
edge_interaction_plot <- function(state, data, ix_id, to_id, est, est_std, etype,
                                  show_fit = FALSE, rel_kind = "linear") {
  state <- ensure_node_kind_columns(state)
  fac <- interaction_factors(state, ix_id)
  fa <- fac$a
  fb <- fac$b
  if (is.na(fa) || is.na(fb) || !nzchar(fa) || !nzchar(fb)) {
    return(edge_plot_message(
      "Incomplete interaction",
      "Wire two predictors into the × input ports first."
    ))
  }
  col_a <- state$nodes$observed_column[match(fa, state$nodes$id)]
  col_b <- state$nodes$observed_column[match(fb, state$nodes$id)]
  col_y <- state$nodes$observed_column[match(to_id, state$nodes$id)]
  if (any(is.na(c(col_a, col_b, col_y))) ||
        !all(c(col_a, col_b, col_y) %in% names(data))) {
    return(edge_plot_message("Missing data", "Interaction factors or DV not in data."))
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
  if (identical(y_kind, "categorical")) {
    return(edge_plot_message("Unsupported DV", "Multi-level categorical outcomes are not plotted."))
  }
  if (identical(y_kind, "binary")) df$y <- as_binary01(df$y) else df$y <- as.numeric(df$y)

  a_kind <- classify_variable(df$a)
  b_kind <- classify_variable(df$b)
  fac_w <- interaction_factors(state, ix_id)
  x <- NULL
  z_raw <- NULL
  x_lab <- NULL
  z_lab <- NULL

  # Prefer continuous / binary as the x-axis when one side is categorical
  if (identical(a_kind, "categorical") || identical(b_kind, "categorical")) {
    # Use the continuous/binary factor as x when possible; otherwise message
    if (identical(a_kind, "categorical") &&
          (identical(b_kind, "continuous") || identical(b_kind, "binary"))) {
      # already set x from b in the else branch? re-resolve:
      x <- factor_numeric_for_product(df$b, center = FALSE)
      z_raw <- df$a
      x_lab <- node_display_name(state, fb)
      z_lab <- node_display_name(state, fa)
      if (!is.na(fac_w$level_a) && nzchar(fac_w$level_a)) {
        keep_lv <- as.character(df$a) == fac_w$level_a |
          as.character(df$a) == variable_coding(df$a)$base
        df <- df[keep_lv, , drop = FALSE]
        x <- factor_numeric_for_product(df$b, center = FALSE)
        z_raw <- df$a
        z_lab <- paste0(node_display_name(state, fa), "[", fac_w$level_a, "]")
      }
    } else if (identical(b_kind, "categorical") &&
                 (identical(a_kind, "continuous") || identical(a_kind, "binary"))) {
      x <- factor_numeric_for_product(df$a, center = FALSE)
      z_raw <- df$b
      x_lab <- node_display_name(state, fa)
      z_lab <- node_display_name(state, fb)
      if (!is.na(fac_w$level_b) && nzchar(fac_w$level_b)) {
        keep_lv <- as.character(df$b) == fac_w$level_b |
          as.character(df$b) == variable_coding(df$b)$base
        df <- df[keep_lv, , drop = FALSE]
        x <- factor_numeric_for_product(df$a, center = FALSE)
        z_raw <- df$b
        z_lab <- paste0(node_display_name(state, fb), "[", fac_w$level_b, "]")
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
    x_lab <- node_display_name(state, fa)
    z_lab <- node_display_name(state, fb)
  } else {
    x <- factor_numeric_for_product(df$b, center = FALSE)
    z_raw <- df$a
    x_lab <- node_display_name(state, fb)
    z_lab <- node_display_name(state, fa)
  }
  z_kind <- classify_variable(z_raw)
  if (identical(z_kind, "categorical")) {
    # Moderator is categorical — one smooth per level (incl. base)
    coding_z <- variable_coding(z_raw)
    z_fac <- factor(as.character(z_raw), levels = c(coding_z$base, coding_z$levels))
    plot_df <- data.frame(x = x, y = df$y, z = z_fac, stringsAsFactors = FALSE)
    plot_df <- plot_df[stats::complete.cases(plot_df), , drop = FALSE]
    subtitle <- edge_plot_subtitle(est, est_std, etype, NA_character_, show_fit, rel_kind)
    ylab <- if (identical(y_kind, "binary")) {
      sprintf("P(%s = 1)", node_display_name(state, to_id))
    } else {
      node_display_name(state, to_id)
    }
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$z, fill = .data$z, group = .data$z)
    ) +
      ggplot2::geom_point(alpha = 0.55, size = 1.8) +
      ggplot2::labs(
        title = paste(x_lab, "\u00D7", z_lab, "\u2192", node_display_name(state, to_id)),
        subtitle = subtitle,
        x = x_lab,
        y = ylab,
        color = z_lab,
        fill = z_lab
      ) +
      edge_plot_theme() +
      ggplot2::theme(legend.position = "bottom")
    if (isTRUE(show_fit)) {
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
    return(p)
  }
  if (identical(z_kind, "binary")) {
    z_fac <- factor(as_binary01(z_raw), levels = c(0, 1), labels = c("0", "1"))
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
  subtitle <- edge_plot_subtitle(est, est_std, etype, NA_character_, show_fit, rel_kind)
  ylab <- if (identical(y_kind, "binary")) {
    sprintf("P(%s = 1)", node_display_name(state, to_id))
  } else {
    node_display_name(state, to_id)
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y, color = .data$z)) +
    ggplot2::geom_point(alpha = 0.3, size = 1.6) +
    ggplot2::labs(
      title = sprintf("%s → %s", node_display_name(state, ix_id), node_display_name(state, to_id)),
      subtitle = subtitle,
      x = x_lab,
      y = ylab,
      color = z_lab
    ) +
    edge_plot_theme() +
    ggplot2::theme(legend.position = "bottom")

  if (isTRUE(show_fit)) {
    sm_rel <- if (identical(rel_kind, "loess") && identical(y_kind, "binary")) "linear" else rel_kind
    formula <- switch(
      sm_rel,
      quadratic = y ~ poly(x, 2),
      cubic = y ~ poly(x, 3),
      y ~ x
    )
    method <- if (identical(y_kind, "binary")) "glm" else if (identical(sm_rel, "loess")) "loess" else "lm"
    method_args <- if (identical(y_kind, "binary")) list(family = stats::binomial()) else list()
    p <- p + ggplot2::geom_smooth(
      method = method,
      formula = formula,
      method.args = method_args,
      se = TRUE,
      linewidth = 1
    )
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
