#' Resolve the path-equation fit used for an edge's DV plot
#'
#' @param result Fit result (`engine == "path"`) or NULL.
#' @param to_id DV node id.
#' @param to_level Optional split categorical response level.
#' @return A fitted `lm`/`glm`/`lmerMod`/`glmerMod`, or NULL.
#' @keywords internal
edge_fit_for_plot <- function(result, to_id, to_level = NA_character_) {
  fits <- dv_result_fits(result, to_id)
  if (!length(fits)) return(NULL)
  to_id <- as.character(to_id)
  key <- if (!is.null(to_level) && !is.na(to_level) && nzchar(as.character(to_level))) {
    paste0(to_id, "::", as.character(to_level))
  } else {
    to_id
  }
  if (key %in% names(fits)) return(fits[[key]])
  if (to_id %in% names(fits)) return(fits[[to_id]])
  if (length(fits) == 1L) return(fits[[1L]])
  NULL
}

#' Normalize points-mode UI values
#' @keywords internal
normalize_edge_plot_points <- function(mode) {
  mode <- as.character(mode %||% "observed")
  if (identical(mode, "partial") || identical(mode, "partial_residuals")) {
    return("partial")
  }
  if (identical(mode, "none")) return("none")
  "observed"
}

#' Model-matrix / coef names belonging to a predictor node
#' @keywords internal
predictor_fit_columns <- function(state, fit, node_id) {
  xn <- tryCatch(lavaan_var_name(state, node_id), error = function(e) NA_character_)
  if (is.na(xn) || !nzchar(xn)) return(character())
  mm <- tryCatch(colnames(stats::model.matrix(fit)), error = function(e) character())
  mm <- setdiff(mm, "(Intercept)")
  if (!length(mm)) return(character())
  sx <- sanitize_id(xn)
  out <- character()
  if (xn %in% mm) out <- c(out, xn)
  if (sx %in% mm && !sx %in% out) out <- c(out, sx)
  pref_dum <- paste0(sx, "__")
  out <- c(out, mm[startsWith(mm, pref_dum)])
  out <- c(out, mm[grepl(paste0("^", sx, "_p[0-9]+$"), mm)])
  # Continuous × continuous interaction product column named like the × node
  if (is_interaction_node(state, node_id) && xn %in% mm) {
    out <- c(out, xn)
  }
  unique(out[nzchar(out)])
}

#' Build emmeans `at=` holds + caption labels for co-predictors
#'
#' Continuous predictors are fixed at their model-frame mean (poly powers at 0
#' when centered). Binary / dummy columns are fixed at 0 (treatment reference).
#' @keywords internal
emmeans_hold_specs <- function(state, data, fit, hold_ids) {
  at <- list()
  labels <- character()
  if (is.null(fit) || !length(hold_ids)) {
    return(list(at = at, labels = labels))
  }
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)

  for (pid in hold_ids) {
    cols <- predictor_fit_columns(state, fit, pid)
    if (!length(cols)) next
    lab <- node_plot_label(state, pid)
    xn <- tryCatch(lavaan_var_name(state, pid), error = function(e) NA_character_)
    raw <- if (!is.na(xn) && !is.null(data) && xn %in% names(data)) {
      data[[xn]]
    } else if (!is.na(xn) && !is.null(mf) && xn %in% names(mf)) {
      mf[[xn]]
    } else {
      NULL
    }
    kind <- if (!is.null(raw)) classify_variable(raw) else "continuous"
    has_dummy <- any(grepl("__", cols, fixed = TRUE))

    if (kind %in% c("binary", "categorical") || isTRUE(has_dummy)) {
      for (cn in cols) at[[cn]] <- 0
      base <- NA_character_
      coding <- tryCatch(
        variable_coding_for_node(state, pid, data),
        error = function(e) NULL
      )
      if (!is.null(coding) && !is.na(coding$base) && nzchar(coding$base)) {
        base <- as.character(coding$base)
      } else if (identical(kind, "binary")) {
        base <- "0"
      } else {
        base <- "reference"
      }
      labels <- c(labels, sprintf("%s = %s", lab, base))
    } else {
      # Continuous (and interaction product treated as numeric)
      mu <- if (!is.null(mf) && !is.na(xn) && xn %in% names(mf)) {
        mean(as.numeric(mf[[xn]]), na.rm = TRUE)
      } else if (!is.null(raw)) {
        mean(as.numeric(raw), na.rm = TRUE)
      } else {
        0
      }
      if (!is.finite(mu)) mu <- 0
      for (cn in cols) {
        if (identical(cn, xn) || identical(cn, sanitize_id(xn))) {
          at[[cn]] <- mu
        } else {
          # Centered poly powers are 0 at the training mean
          at[[cn]] <- 0
        }
      }
      labels <- c(labels, sprintf("%s = mean", lab))
    }
  }
  list(at = at, labels = unique(labels))
}

#' Focal predictor grid for emmeans (`at` entries for the focal terms)
#' @keywords internal
emmeans_focal_at <- function(fit, focal, x_grid) {
  at <- list()
  if (is.null(fit) || is.null(focal) || !nzchar(focal) || !length(x_grid)) {
    return(at)
  }
  x_grid <- as.numeric(x_grid)
  x_grid <- x_grid[is.finite(x_grid)]
  if (!length(x_grid)) return(at)
  at[[focal]] <- x_grid
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  x_mu <- if (!is.null(mf) && focal %in% names(mf)) {
    mean(as.numeric(mf[[focal]]), na.rm = TRUE)
  } else {
    mean(x_grid, na.rm = TRUE)
  }
  if (!is.finite(x_mu)) x_mu <- 0
  xc <- x_grid - x_mu
  mm <- tryCatch(colnames(stats::model.matrix(fit)), error = function(e) character())
  sx <- sanitize_id(focal)
  for (d in 2:6) {
    pn <- poly_col_name(focal, d)
    if (!pn %in% mm) {
      pn2 <- poly_col_name(sx, d)
      if (pn2 %in% mm) pn <- pn2 else next
    }
    at[[pn]] <- xc^d
  }
  at
}

#' emmeans prediction curve / level means for a focal predictor
#'
#' @return data.frame with `x`, `y`, `ymin`, `ymax` (and optional `x_lab`), or NULL.
#' @keywords internal
emmeans_edge_curve <- function(fit, focal, hold_at = list(),
                               x_grid = NULL, x_levels = NULL) {
  if (is.null(fit) || is.null(focal) || !nzchar(as.character(focal))) {
    return(NULL)
  }
  if (!requireNamespace("emmeans", quietly = TRUE)) return(NULL)

  focal <- as.character(focal)
  at <- hold_at %||% list()
  specs <- stats::as.formula(paste("~", focal))

  if (!is.null(x_levels) && length(x_levels)) {
    at[[focal]] <- x_levels
  } else if (!is.null(x_grid) && length(x_grid)) {
    at <- utils::modifyList(at, emmeans_focal_at(fit, focal, x_grid))
  } else {
    return(NULL)
  }

  use_response <- inherits(fit, "glm") || inherits(fit, "glmerMod")
  em <- tryCatch(
    {
      rg <- emmeans::ref_grid(fit, at = at)
      emmeans::emmeans(rg, specs)
    },
    error = function(e) {
      if (exists("smt_debug", mode = "function")) {
        smt_debug("emmeans_edge_curve failed: ", e$message)
      }
      NULL
    }
  )
  if (is.null(em)) return(NULL)

  s <- tryCatch(
    summary(em, type = if (use_response) "response" else "link"),
    error = function(e) tryCatch(summary(em), error = function(e2) NULL)
  )
  if (is.null(s) || !nrow(s)) return(NULL)
  s <- as.data.frame(s)

  ycol <- if ("emmean" %in% names(s)) {
    "emmean"
  } else if ("prob" %in% names(s)) {
    "prob"
  } else if ("rate" %in% names(s)) {
    "rate"
  } else {
    return(NULL)
  }
  lo <- if ("lower.CL" %in% names(s)) {
    s$lower.CL
  } else if ("asymp.LCL" %in% names(s)) {
    s$asymp.LCL
  } else {
    s[[ycol]]
  }
  hi <- if ("upper.CL" %in% names(s)) {
    s$upper.CL
  } else if ("asymp.UCL" %in% names(s)) {
    s$asymp.UCL
  } else {
    s[[ycol]]
  }

  x_raw <- s[[focal]]
  x_num <- suppressWarnings(as.numeric(x_raw))
  data.frame(
    x = if (all(is.finite(x_num))) x_num else as.numeric(as.factor(x_raw)),
    x_lab = as.character(x_raw),
    y = as.numeric(s[[ycol]]),
    ymin = as.numeric(lo),
    ymax = as.numeric(hi),
    stringsAsFactors = FALSE
  )
}

#' Partial-residual coordinates for a focal continuous predictor
#'
#' Uses `residuals(fit, type = "partial")` for the linear term column. Falls
#' back to NULL when unavailable (caller should use observed points).
#' @keywords internal
partial_residual_xy <- function(fit, focal) {
  if (is.null(fit) || is.null(focal) || !nzchar(focal)) return(NULL)
  pr <- tryCatch(
    stats::residuals(fit, type = "partial"),
    error = function(e) NULL
  )
  if (is.null(pr)) return(NULL)
  pr <- as.matrix(pr)
  if (!focal %in% colnames(pr)) return(NULL)
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf) || !focal %in% names(mf)) return(NULL)
  x <- as.numeric(mf[[focal]])
  y <- as.numeric(pr[, focal])
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(NULL)
  data.frame(x = x[ok], y = y[ok], stringsAsFactors = FALSE)
}

#' ggplot layers for an emmeans curve / categorical means
#' @keywords internal
emmeans_fit_layers <- function(curve_df, categorical = FALSE,
                               y_binary = FALSE) {
  if (is.null(curve_df) || !nrow(curve_df)) return(list())
  col <- if (y_binary) "#C0392B" else "#1F7A4D"
  fill <- if (y_binary) "#C0392B55" else "#1F7A4D55"
  if (isTRUE(categorical)) {
    list(
      ggplot2::geom_errorbar(
        data = curve_df,
        ggplot2::aes(x = .data$x_lab, y = .data$y, ymin = .data$ymin, ymax = .data$ymax),
        inherit.aes = FALSE,
        width = 0.15,
        color = col,
        linewidth = 0.7
      ),
      ggplot2::geom_point(
        data = curve_df,
        ggplot2::aes(x = .data$x_lab, y = .data$y),
        inherit.aes = FALSE,
        size = 3,
        color = col
      )
    )
  } else {
    list(
      ggplot2::geom_ribbon(
        data = curve_df,
        ggplot2::aes(x = .data$x, ymin = .data$ymin, ymax = .data$ymax),
        inherit.aes = FALSE,
        fill = fill,
        alpha = 0.35
      ),
      ggplot2::geom_line(
        data = curve_df,
        ggplot2::aes(x = .data$x, y = .data$y),
        inherit.aes = FALSE,
        color = col,
        linewidth = 1.05
      )
    )
  }
}
