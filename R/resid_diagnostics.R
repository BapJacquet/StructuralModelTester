#' Fitted objects for one DV from a path-engine result
#'
#' Keys are the DV id, or `dv::level` for split categorical outcomes.
#' @keywords internal
dv_result_fits <- function(result, dv_id) {
  if (is.null(result) || !identical(result$engine, "path") ||
        !is.list(result$fit) || !length(result$fit)) {
    return(list())
  }
  dv_id <- as.character(dv_id)
  keys <- names(result$fit)
  if (is.null(keys)) return(list())
  hit <- keys == dv_id | startsWith(keys, paste0(dv_id, "::"))
  result$fit[keys[hit]]
}

#' Short label for a DV fit key in the diagnostics selector
#' @keywords internal
dv_fit_key_label <- function(state, fit_key) {
  fit_key <- as.character(fit_key)
  if (!grepl("::", fit_key, fixed = TRUE)) {
    return(node_display_name(state, fit_key))
  }
  parts <- strsplit(fit_key, "::", fixed = TRUE)[[1]]
  paste0(node_display_name(state, parts[[1]]), " [", parts[[2]], "]")
}

#' Extract residuals / fitted values for path-model diagnostics
#' @keywords internal
path_fit_residual_info <- function(fit) {
  if (is.null(fit)) return(NULL)
  if (inherits(fit, "glmerMod")) {
    r <- tryCatch(
      stats::residuals(fit, type = "deviance"),
      error = function(e) stats::residuals(fit)
    )
    list(
      kind = "glmer",
      resid = as.numeric(r),
      fitted = as.numeric(stats::fitted(fit)),
      label = "Deviance residuals (conditional)"
    )
  } else if (inherits(fit, "lmerMod") || inherits(fit, "merMod")) {
    list(
      kind = "lmer",
      resid = as.numeric(stats::residuals(fit)),
      fitted = as.numeric(stats::fitted(fit)),
      label = "Conditional residuals"
    )
  } else if (inherits(fit, "glm")) {
    list(
      kind = "glm",
      resid = as.numeric(stats::residuals(fit, type = "deviance")),
      fitted = as.numeric(stats::fitted(fit)),
      label = "Deviance residuals"
    )
  } else if (inherits(fit, "lm")) {
    list(
      kind = "lm",
      resid = as.numeric(stats::residuals(fit)),
      fitted = as.numeric(stats::fitted(fit)),
      label = "Ordinary residuals"
    )
  } else {
    NULL
  }
}

#' QQ + residuals-vs-fitted panel for one path equation
#' @keywords internal
residual_diagnostics_plot <- function(fit, title = NULL) {
  empty <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, label = msg, color = "#6e6e73") +
      ggplot2::theme_void()
  }

  info <- path_fit_residual_info(fit)
  if (is.null(info)) return(empty("No residuals for this fit."))

  ok <- is.finite(info$resid) & is.finite(info$fitted)
  r <- info$resid[ok]
  f <- info$fitted[ok]
  if (length(r) < 3L) return(empty("Not enough finite residuals."))

  qq <- stats::qqnorm(r, plot.it = FALSE)
  qy <- stats::quantile(r, c(0.25, 0.75), names = FALSE, type = 7)
  qx <- stats::qnorm(c(0.25, 0.75))
  slope <- as.numeric(diff(qy) / diff(qx))
  intercept <- as.numeric(qy[1L] - slope * qx[1L])

  panel_levels <- c("Normal QQ", "Residuals vs fitted")
  both <- rbind(
    data.frame(
      panel = "Normal QQ",
      x = as.numeric(qq$x),
      y = as.numeric(qq$y),
      stringsAsFactors = FALSE
    ),
    data.frame(
      panel = "Residuals vs fitted",
      x = f,
      y = r,
      stringsAsFactors = FALSE
    )
  )
  both$panel <- factor(both$panel, levels = panel_levels)

  line_df <- data.frame(
    panel = factor("Normal QQ", levels = panel_levels),
    intercept = intercept,
    slope = slope
  )
  hline_df <- data.frame(
    panel = factor("Residuals vs fitted", levels = panel_levels),
    yintercept = 0
  )

  ggplot2::ggplot(both, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_abline(
      data = line_df,
      ggplot2::aes(intercept = .data$intercept, slope = .data$slope),
      color = "#b0b0b5", linewidth = 0.6, inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = hline_df,
      ggplot2::aes(yintercept = .data$yintercept),
      color = "#b0b0b5", linewidth = 0.6, inherit.aes = FALSE
    ) +
    ggplot2::geom_point(size = 1.35, alpha = 0.75, color = "#3d3d42") +
    ggplot2::facet_wrap(~panel, scales = "free", ncol = 1) +
    ggplot2::labs(
      title = title,
      subtitle = info$label,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#6e6e73"),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      strip.background = ggplot2::element_rect(fill = "#f3f3f4", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(6, 8, 4, 8)
    )
}
