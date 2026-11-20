#' Whether LaTeX PDF export can run (tinytex + engine)
#' @keywords internal
report_export_available <- function() {
  if (!requireNamespace("tinytex", quietly = TRUE)) return(FALSE)
  if (isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))) {
    return(TRUE)
  }
  nzchar(Sys.which("pdflatex")) ||
    nzchar(Sys.which("xelatex")) ||
    nzchar(Sys.which("lualatex"))
}

#' Escape plain text for LaTeX
#' @keywords internal
latex_escape_text <- function(x) {
  x <- as.character(x %||% "")
  vapply(x, function(s) {
    if (is.na(s)) return("")
    s <- gsub("\u2019", "'", s, fixed = TRUE)
    s <- gsub("\u2013", "--", s, fixed = TRUE)
    s <- gsub("\u2014", "---", s, fixed = TRUE)
    # Placeholder tokens for TeX math (restored after \\/$ escaping)
    s <- gsub("\u00d7", "\u0002", s, fixed = TRUE)       # ×
    s <- gsub("\u2265", "\u0003", s, fixed = TRUE)       # ≥
    s <- gsub("\u03b1", "\u0004", s, fixed = TRUE)       # α
    s <- gsub("\u03b5", "\u0005", s, fixed = TRUE)       # ε
    s <- gsub("\u03a3", "\u0006", s, fixed = TRUE)       # Σ
    s <- gsub("\u03b2", "\u0007", s, fixed = TRUE)       # β
    s <- gsub("\u2190", "\u000e", s, fixed = TRUE)       # ←
    s <- gsub("\u219c", "\u000e", s, fixed = TRUE)       # ↜ via ≈ ←
    s <- gsub("\u21d0", "\u000f", s, fixed = TRUE)       # ⇐ total
    s <- gsub("\u2193", "\u0010", s, fixed = TRUE)       # ↓ SD↓
    # Placeholder so later brace escaping does not break \\textbackslash{}
    s <- gsub("\\", "\u0001", s, fixed = TRUE)
    s <- gsub("([#$%&_{}])", "\\\\\\1", s)
    s <- gsub("~", "\\textasciitilde{}", s, fixed = TRUE)
    s <- gsub("^", "\\textasciicircum{}", s, fixed = TRUE)
    s <- gsub("\u0001", "\\textbackslash{}", s, fixed = TRUE)
    s <- gsub("\u0002", "$\\times$", s, fixed = TRUE)
    s <- gsub("\u0003", "$\\ge$", s, fixed = TRUE)
    s <- gsub("\u0004", "$\\alpha$", s, fixed = TRUE)
    s <- gsub("\u0005", "$\\varepsilon$", s, fixed = TRUE)
    s <- gsub("\u0006", "$\\Sigma$", s, fixed = TRUE)
    s <- gsub("\u0007", "$\\beta$", s, fixed = TRUE)
    s <- gsub("\u000e", "$\\leftarrow$", s, fixed = TRUE)
    s <- gsub("\u000f", "$\\Leftarrow$", s, fixed = TRUE)
    s <- gsub("\u0010", "$\\downarrow$", s, fixed = TRUE)
    s
  }, character(1), USE.NAMES = FALSE)
}

#' APA-ish booktabs table from a data.frame
#' @keywords internal
report_df_to_latex_table <- function(df, caption, table_no) {
  if (is.null(df) || !nrow(df)) {
    return(sprintf(
      paste0(
        "\\begin{table}[htbp]\n\\caption{%s}\n",
        "\\textit{Note.} No rows to display.\n\\end{table}\n"
      ),
      latex_escape_text(caption)
    ))
  }
  cols <- names(df)
  # Italicize common statistic column headers (APA)
  head_cells <- vapply(cols, function(nm) {
    if (nm %in% c("p", "pHolm", "pBonf", "pBH", "SE", "Estimate")) {
      sprintf("\\textit{%s}", latex_escape_text(nm))
    } else {
      latex_escape_text(nm)
    }
  }, character(1))
  align <- paste(rep("l", length(cols)), collapse = "")
  body_rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(cols, function(nm) {
      v <- df[[nm]][[i]]
      if (is.numeric(v)) {
        if (!is.finite(v)) return("")
        formatC(v, digits = 3, format = "f")
      } else {
        latex_escape_text(smt_plain_cell_text(v))
      }
    }, character(1))
    paste(cells, collapse = " & ")
  }, character(1))
  paste0(
    "\\begin{table}[htbp]\n",
    "\\caption{", latex_escape_text(caption), "}\n",
    "\\label{tab:smt", as.integer(table_no), "}\n",
    "\\begin{tabular}{", align, "}\n",
    "\\toprule\n",
    paste(head_cells, collapse = " & "), " \\\\\n",
    "\\midrule\n",
    paste0(body_rows, " \\\\", collapse = "\n"), "\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
  )
}

#' Save a ggplot for LaTeX inclusion; return relative filename
#'
#' Uses base [grDevices::pdf] / [grDevices::png] rather than ggplot2's default
#' device (often ragg). On some R builds, ragg/systemfonts are compiled against
#' a mismatched graphics API and raise "incompatible graphics API version".
#' @keywords internal
report_save_ggplot <- function(p, dir, basename, width = 6.5, height = 4,
                               dpi = 150) {
  path_pdf <- file.path(dir, paste0(basename, ".pdf"))
  pdf_ok <- FALSE
  tryCatch({
    grDevices::pdf(path_pdf, width = width, height = height, useDingbats = FALSE)
    print(p)
    grDevices::dev.off()
    pdf_ok <- TRUE
  }, error = function(e) {
    while (grDevices::dev.cur() > 1L) {
      try(grDevices::dev.off(), silent = TRUE)
    }
  })
  if (isTRUE(pdf_ok) && file.exists(path_pdf) &&
        isTRUE(file.info(path_pdf)$size > 0)) {
    return(paste0(basename, ".pdf"))
  }

  path_png <- file.path(dir, paste0(basename, ".png"))
  grDevices::png(
    path_png,
    width = as.integer(width * dpi),
    height = as.integer(height * dpi),
    res = as.integer(dpi)
  )
  tryCatch({
    print(p)
  }, finally = {
    while (grDevices::dev.cur() > 1L) {
      try(grDevices::dev.off(), silent = TRUE)
    }
  })
  paste0(basename, ".png")
}

#' APA figure block
#' @keywords internal
report_latex_figure <- function(file_rel, caption, fig_no, width = "\\linewidth") {
  paste0(
    "\\begin{figure}[htbp]\n",
    "\\centering\n",
    "\\includegraphics[width=", width, "]{", file_rel, "}\n",
    "\\caption{", latex_escape_text(caption), "}\n",
    "\\label{fig:smt", as.integer(fig_no), "}\n",
    "\\end{figure}\n"
  )
}

#' Residual diagnostics ggplot for a pinned DV
#' @keywords internal
report_residual_ggplot <- function(state, result, dv_id) {
  fits_dv <- dv_result_fits(result, dv_id)
  if (!length(fits_dv)) {
    return(residual_diagnostics_plot(NULL, title = NULL))
  }
  key <- names(fits_dv)[[1]]
  residual_diagnostics_plot(
    fits_dv[[key]],
    title = dv_fit_key_label(state, key)
  )
}

#' Edge relationship ggplot for a pinned edge
#' @keywords internal
report_edge_ggplot <- function(state, result, edge_id,
                               analysis_data = NULL, sim_data = NULL,
                               results_simulated = FALSE) {
  if (is.null(edge_id) || !nzchar(as.character(edge_id))) {
    return(edge_plot_placeholder())
  }
  st2 <- ensure_edge_to_port(state)
  idx <- match(edge_id, st2$edges$id)
  if (is.na(idx)) return(edge_plot_placeholder())
  from_id <- st2$edges$from[idx]
  to_id <- st2$edges$to[idx]
  need_ids <- if (is_interaction_node(st2, from_id)) {
    fac <- interaction_factors(st2, from_id)
    c(fac$a, fac$b, to_id)
  } else {
    c(from_id, to_id)
  }
  has_result <- !is.null(result)
  picked <- pick_plot_data_for_nodes(
    st2, analysis_data, sim_data, need_ids, has_result = has_result
  )
  if (is.null(picked$data)) {
    return(edge_plot_message(
      picked$title %||% "No data",
      picked$reason %||% "Load data or run Simulate to plot this link."
    ))
  }
  dat <- picked$data
  simulated <- isTRUE(picked$simulated)
  rk <- st2$edges$rel_kind[idx]
  if (is.na(rk) || !nzchar(rk)) rk <- "linear"
  res_sim <- isTRUE(results_simulated)
  show_fit <- !is.null(result) && identical(isTRUE(simulated), res_sim)
  pr <- edge_prior(st2, edge_id)
  to_level <- if ("to_level" %in% names(st2$edges)) {
    st2$edges$to_level[idx]
  } else {
    NA_character_
  }
  edge_fit <- if (isTRUE(show_fit)) {
    edge_fit_for_plot(result, to_id, to_level = to_level)
  } else {
    NULL
  }
  score_meth <- normalize_latent_score_method(
    result$latent_score_method %||% "own",
    st2
  )
  tryCatch(
    edge_relationship_plot(
      st2, dat, edge_id,
      show_fit = show_fit,
      rel_kind = rk,
      prior_mean = pr$mean,
      prior_sd = pr$sd,
      simulated = simulated,
      latent_score_method = score_meth,
      fit = edge_fit,
      points_mode = "observed"
    ),
    error = function(e) edge_plot_message("Plot error", e$message)
  )
}

#' Plain-text power recommendation for LaTeX
#' @keywords internal
format_power_recommend_text <- function(pw) {
  if (is.null(pw) || is.null(pw$power) || !nrow(pw$power)) {
    return("No power analysis results available.")
  }
  rec <- pw$recommended_n
  use_fdr <- isTRUE(attr(pw$power, "require_zero_null"))
  rule_txt <- if (use_fdr) {
    "power $\\ge$ 80\\% and $P$(0 false detections) $\\ge$ 95\\%"
  } else {
    "power $\\ge$ 80\\% (false-detection rate ignored for $N^*$ under $p$-values)"
  }
  n_cap <- pw$n_cap %||% attr(pw$power, "n_cap")
  if (!is.null(rec) && length(rec) == 1L && is.finite(rec)) {
    metrics <- format_power_metrics_at_n(pw$power, pw$fdr, rec)
    detail <- if (nzchar(metrics)) {
      paste0(" (", latex_escape_text(metrics), ")")
    } else {
      ""
    }
    return(sprintf(
      "Recommended $N^* = %d$%s under %s.",
      as.integer(rec), detail, rule_txt
    ))
  }
  if (isTRUE(pw$n_cap_hit) || isTRUE(attr(pw$power, "n_cap_hit"))) {
    raw <- pw$recommended_n_raw %||% attr(pw$power, "recommended_n_raw")
    return(sprintf(
      paste0(
        "$N^*$ exceeds Max sample. Curve-fit estimate $\\approx$ %s ",
        "but search is capped at %s. Raise Max sample or effect sizes for %s."
      ),
      if (is.finite(raw)) format(as.integer(raw), big.mark = ",") else "?",
      if (is.finite(n_cap)) format(as.integer(n_cap), big.mark = ",") else "the cap",
      rule_txt
    ))
  }
  sprintf(
    "No recommended $N$. Raise Max sample, Samples, or effect sizes until the curves reach %s.",
    rule_txt
  )
}

#' Extra power detail paragraphs (edge rates + DV residuals)
#' @keywords internal
format_power_details_latex <- function(pw) {
  chunks <- character()
  rec <- pw$recommended_n
  n_ref <- if (!is.null(rec) && length(rec) == 1L && is.finite(rec)) {
    as.integer(rec)
  } else {
    NULL
  }
  sub <- tryCatch(
    smt_power_edge_rates_at_n(pw, n = n_ref),
    error = function(e) NULL
  )
  if (!is.null(sub) && nrow(sub)) {
    n_used <- attr(sub, "n") %||% n_ref
    labs <- if ("label" %in% names(sub)) sub$label else sub$edge_id
    lines <- sprintf(
      "%s: %.0f\\%%",
      latex_escape_text(labs),
      100 * as.numeric(sub$power)
    )
    chunks <- c(chunks, sprintf(
      "\\paragraph*{Per-link power%s.}\n%s\n",
      if (!is.null(n_used) && is.finite(n_used)) {
        sprintf(" at $N=%d$", as.integer(n_used))
      } else {
        ""
      },
      paste(lines, collapse = "; ")
    ))
  }
  df <- pw$dv_residuals %||% NULL
  if (!is.null(df) && is.data.frame(df) && nrow(df)) {
    lines <- vapply(seq_len(nrow(df)), function(i) {
      lab <- df$label[[i]]
      if (is.null(lab) || is.na(lab) || !nzchar(as.character(lab))) {
        lab <- as.character(df$id[[i]])
      }
      kind <- as.character(df$kind[[i]] %||% "continuous")
      sd_val <- as.numeric(df$resid_sd[[i]])
      if (identical(kind, "binary")) {
        sd_txt <- "SD($\\varepsilon$) unused (logistic)"
      } else if (identical(kind, "categorical")) {
        sd_txt <- "SD($\\varepsilon$) unused (categorical)"
      } else if (is.finite(sd_val)) {
        sd_txt <- sprintf("SD($\\varepsilon$) = %.2f", sd_val)
      } else {
        sd_txt <- "SD($\\varepsilon$) unset"
      }
      sprintf("%s: %s", latex_escape_text(lab), sd_txt)
    }, character(1))
    chunks <- c(chunks, sprintf(
      "\\paragraph*{DV residual SD.}\n%s\n",
      paste(lines, collapse = "; ")
    ))
  }
  paste(chunks, collapse = "\n")
}

#' Figure block with a path comment (clipboard; no binary embed)
#' @keywords internal
report_latex_figure_stub <- function(suggested_file, caption, fig_no) {
  paste0(
    "% Figure file not embedded — use Export PDF, or save the plot as ",
    latex_escape_text(suggested_file), " and uncomment:\n",
    "\\begin{figure}[htbp]\n",
    "\\centering\n",
    "% \\includegraphics[width=\\linewidth]{", suggested_file, "}\n",
    "\\caption{", latex_escape_text(caption), "}\n",
    "\\label{fig:smt", as.integer(fig_no), "}\n",
    "\\end{figure}\n"
  )
}

#' Build LaTeX body for one report module; may write figure files into `dir`
#'
#' @param include_figures If `FALSE`, emit figure stubs instead of writing plots
#'   (for clipboard copy).
#' @return list(tex, table_no, fig_no)
#' @keywords internal
report_module_to_latex <- function(mod, state, result, power,
                                   dir, table_no = 1L, fig_no = 1L,
                                   analysis_data = NULL, sim_data = NULL,
                                   results_simulated = FALSE,
                                   include_figures = TRUE) {
  title <- report_module_title(mod, state)
  typ <- as.character(mod$type %||% "")
  tex <- sprintf("\\section*{%s}\n", latex_escape_text(title))

  if (identical(typ, "coef_table")) {
    if (is.null(result)) {
      tex <- paste0(tex, "\\textit{Run the analysis to populate path coefficients.}\n")
    } else {
      tab <- estimates_table_for_report(
        state, result, dv_id = mod$params$dv_id %||% NULL
      )
      tex <- paste0(
        tex,
        report_df_to_latex_table(tab, title, table_no)
      )
      table_no <- table_no + 1L
    }
  } else if (identical(typ, "cfa_measurement")) {
    lat <- mod$params$latent_id %||% NA_character_
    sm <- latent_measurement_summary(result, state, lat)
    if (is.null(sm) || is.null(sm$loadings) || !nrow(sm$loadings)) {
      tex <- paste0(tex, "\\textit{No CFA loadings available for this latent.}\n")
    } else {
      bits <- character()
      if (is.finite(sm$alpha)) {
        bits <- c(bits, sprintf("Cronbach $\\alpha$ = %.2f", sm$alpha))
      }
      bits <- c(bits, sprintf(
        "score = %s",
        if (identical(sm$score_method, "joint")) "joint Thomson" else "own indicators"
      ))
      if (length(bits)) {
        tex <- paste0(tex, paste(bits, collapse = "; "), "\n\n")
      }
      tab <- latent_loadings_df(sm$loadings, state)
      tex <- paste0(tex, report_df_to_latex_table(tab, title, table_no))
      table_no <- table_no + 1L
      if (!is.null(sm$fit_measures) && nrow(sm$fit_measures)) {
        fm <- sm$fit_measures
        fm_lab <- paste(
          sprintf("%s = %s", fm$measure, formatC(fm$value, digits = 3, format = "f")),
          collapse = "; "
        )
        tex <- paste0(tex, "\n\\textit{Joint CFA fit:} ", latex_escape_text(fm_lab), "\n")
      }
    }
  } else if (identical(typ, "pca_screen")) {
    lat <- mod$params$latent_id %||% NA_character_
    sm <- latent_measurement_summary(result, state, lat)
    uni <- if (is.null(sm)) NULL else sm$uni
    ust <- if (is.list(uni)) as.character(uni$status %||% NA_character_) else NA_character_
    if (!is.list(uni) || is.na(ust) || !nzchar(ust)) {
      tex <- paste0(tex, "\\textit{No PCA unidimensionality screen for this latent.}\n")
    } else {
      bits <- character()
      if (nzchar(as.character(uni$label %||% ""))) {
        bits <- c(bits, as.character(uni$label))
      }
      if (is.finite(uni$ratio)) {
        bits <- c(bits, sprintf("$\\lambda_1/\\lambda_2$ = %.2f", uni$ratio))
      }
      if (is.finite(uni$pct1)) {
        bits <- c(bits, sprintf("PC1 = %.1f\\%%", 100 * uni$pct1))
      }
      if (length(bits)) {
        tex <- paste0(tex, paste(bits, collapse = "; "), "\n\n")
      }
      tab <- latent_pca_eigen_df(uni)
      if (nrow(tab)) {
        names(tab)[names(tab) == "PctVar"] <- "% var"
        tex <- paste0(
          tex,
          report_df_to_latex_table(tab, paste(title, "— eigenvalues"), table_no)
        )
        table_no <- table_no + 1L
      }
      ld <- latent_pca_loadings_df(uni, state, which = "unrotated")
      if (nrow(ld) && ncol(ld) > 1L) {
        tex <- paste0(
          tex,
          report_df_to_latex_table(
            ld, paste(title, "— item loadings (unrotated)"), table_no
          )
        )
        table_no <- table_no + 1L
      }
      ld_rot <- latent_pca_loadings_df(uni, state, which = "rotated")
      if (nrow(ld_rot) && ncol(ld_rot) > 1L) {
        tex <- paste0(
          tex,
          report_df_to_latex_table(
            ld_rot, paste(title, "— item loadings (varimax)"), table_no
          )
        )
        table_no <- table_no + 1L
      }
    }
  } else if (identical(typ, "equations")) {
    if (is.null(result)) {
      tex <- paste0(tex, "\\textit{Run the analysis to see fitted equations.}\n")
    } else {
      eq <- if (isTRUE(result$model_averaged)) {
        format_averaged_equations_latex(result, state)
      } else {
        format_best_equations_latex(result, state)
      }
      if (is.null(eq) || !nzchar(eq)) {
        tex <- paste0(tex, "\\textit{No equations available for this fit.}\n")
      } else {
        tex <- paste0(tex, eq, "\n")
      }
    }
  } else if (identical(typ, "residual")) {
    bn <- paste0("fig_resid_", gsub("[^A-Za-z0-9_]", "", mod$uid %||% "x"))
    cap <- paste("Residual diagnostics —", title)
    if (isTRUE(include_figures)) {
      dv <- mod$params$dv_id %||% NA_character_
      p <- report_residual_ggplot(state, result, dv)
      rel <- report_save_ggplot(p, dir, bn, height = 4.2)
      tex <- paste0(tex, report_latex_figure(rel, cap, fig_no))
    } else {
      tex <- paste0(tex, report_latex_figure_stub(paste0(bn, ".pdf"), cap, fig_no))
    }
    fig_no <- fig_no + 1L
  } else if (identical(typ, "edge_plot")) {
    bn <- paste0("fig_edge_", gsub("[^A-Za-z0-9_]", "", mod$uid %||% "x"))
    cap <- paste("Conditional / marginal effect —", title)
    if (isTRUE(include_figures)) {
      eid <- mod$params$edge_id %||% NA_character_
      p <- report_edge_ggplot(
        state, result, eid,
        analysis_data = analysis_data,
        sim_data = sim_data,
        results_simulated = results_simulated
      )
      rel <- report_save_ggplot(p, dir, bn, height = 3.8)
      tex <- paste0(tex, report_latex_figure(rel, cap, fig_no))
    } else {
      tex <- paste0(tex, report_latex_figure_stub(paste0(bn, ".pdf"), cap, fig_no))
    }
    fig_no <- fig_no + 1L
  } else if (identical(typ, "power")) {
    tex <- paste0(tex, format_power_recommend_text(power), "\n\n")
    tex <- paste0(tex, format_power_details_latex(power), "\n")
    if (!is.null(power) && !is.null(power$power) && nrow(power$power)) {
      if (isTRUE(include_figures)) {
        p1 <- power_analysis_plot(power$power, recommended_n = power$recommended_n)
        rel1 <- report_save_ggplot(p1, dir, "fig_power_curve", height = 4)
        tex <- paste0(
          tex,
          report_latex_figure(rel1, "Confirmatory detection power by sample size", fig_no)
        )
      } else {
        tex <- paste0(
          tex,
          report_latex_figure_stub(
            "fig_power_curve.pdf",
            "Confirmatory detection power by sample size",
            fig_no
          )
        )
      }
      fig_no <- fig_no + 1L
      if (!is.null(power$fdr) && nrow(power$fdr)) {
        if (isTRUE(include_figures)) {
          p2 <- power_fdr_plot(power$fdr, recommended_n = power$recommended_n)
          rel2 <- report_save_ggplot(p2, dir, "fig_power_fdr", height = 4)
          tex <- paste0(
            tex,
            report_latex_figure(
              rel2,
              "False-detection rate under the complete null by sample size",
              fig_no
            )
          )
        } else {
          tex <- paste0(
            tex,
            report_latex_figure_stub(
              "fig_power_fdr.pdf",
              "False-detection rate under the complete null by sample size",
              fig_no
            )
          )
        }
        fig_no <- fig_no + 1L
      }
    }
  } else {
    tex <- paste0(tex, "\\textit{Unknown module type.}\n")
  }

  list(tex = tex, table_no = table_no, fig_no = fig_no)
}

#' Clipboard-oriented LaTeX for one pinned module (no figure files written)
#' @keywords internal
report_module_tex_clipboard <- function(mod, state, result = NULL, power = NULL,
                                        analysis_data = NULL, sim_data = NULL,
                                        results_simulated = FALSE) {
  chunk <- report_module_to_latex(
    mod, state, result, power,
    dir = tempdir(),
    table_no = 1L, fig_no = 1L,
    analysis_data = analysis_data,
    sim_data = sim_data,
    results_simulated = results_simulated,
    include_figures = FALSE
  )
  chunk$tex
}

#' Strip HTML tags from table cells (e.g. attention spans) for export
#' @keywords internal
smt_plain_cell_text <- function(x) {
  x <- as.character(x %||% "")
  gsub("<[^>]*>", "", x, perl = TRUE)
}

#' Escape plain text for HTML
#' @keywords internal
html_escape_text <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' HTML table fragment for Word paste (APA 7 booktabs)
#' @keywords internal
report_df_to_html_table <- function(df, caption) {
  cap_txt <- as.character(caption %||% "")
  # Prefer "Table. Title" italic caption (APA)
  if (nzchar(cap_txt) && !grepl("^Table\\b", cap_txt, ignore.case = TRUE)) {
    cap_txt <- paste("Table.", cap_txt)
  }
  cap <- sprintf(
    "<p style=\"font-style:italic;margin:0 0 6pt 0;\">%s</p>\n",
    html_escape_text(cap_txt)
  )
  if (is.null(df) || !nrow(df)) {
    return(paste0(cap, "<p><em>Note.</em> No rows to display.</p>\n"))
  }
  cols <- names(df)
  head <- paste0(
    "<tr>",
    paste(vapply(cols, function(nm) {
      lab <- html_escape_text(nm)
      if (apa_is_stat_col(nm)) lab <- sprintf("<em>%s</em>", lab)
      sprintf(
        paste0(
          "<th style=\"text-align:left;border:none;border-bottom:1px solid #000;",
          "padding:2pt 8pt;font-weight:600;\">%s</th>"
        ),
        lab
      )
    }, character(1)), collapse = ""),
    "</tr>"
  )
  body <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(cols, function(nm) {
      v <- df[[nm]][[i]]
      txt <- if (is.numeric(v)) {
        if (!is.finite(v)) "" else formatC(v, digits = 3, format = "f")
      } else {
        html_escape_text(smt_plain_cell_text(v))
      }
      sprintf(
        "<td style=\"border:none;padding:2pt 8pt;vertical-align:top;\">%s</td>",
        txt
      )
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0(
    cap,
    "<table style=\"border-collapse:collapse;border:none;",
    "border-top:1.5px solid #000;border-bottom:1.5px solid #000;",
    "font-family:Times New Roman,Times,serif;font-size:11pt;\">\n",
    "<thead>\n", head, "\n</thead>\n<tbody>\n",
    paste(body, collapse = "\n"),
    "\n</tbody>\n</table>\n"
  )
}

#' Strip tags for a plain-text clipboard fallback
#' @keywords internal
html_to_plain_text <- function(html) {
  s <- as.character(html %||% "")
  s <- gsub("<br\\s*/?>", "\n", s, ignore.case = TRUE, perl = TRUE)
  s <- gsub("</p>", "\n\n", s, fixed = TRUE)
  s <- gsub("</P>", "\n\n", s, fixed = TRUE)
  s <- gsub("</tr>", "\n", s, fixed = TRUE)
  s <- gsub("</TR>", "\n", s, fixed = TRUE)
  s <- gsub("</(h[1-6]|div|li|pre)>", "\n", s, ignore.case = TRUE, perl = TRUE)
  s <- gsub("<[^>]+>", "", s)
  s <- gsub("&amp;", "&", s, fixed = TRUE)
  s <- gsub("&lt;", "<", s, fixed = TRUE)
  s <- gsub("&gt;", ">", s, fixed = TRUE)
  s <- gsub("&quot;", "\"", s, fixed = TRUE)
  s <- gsub("&nbsp;", " ", s, fixed = TRUE)
  s <- gsub("\n{3,}", "\n\n", s)
  trimws(s)
}

#' Clipboard HTML (+ plain text) for one module — paste into Word
#'
#' @return list(html, text)
#' @keywords internal
report_module_html_clipboard <- function(mod, state, result = NULL, power = NULL,
                                         analysis_data = NULL, sim_data = NULL,
                                         results_simulated = FALSE) {
  title <- report_module_title(mod, state)
  typ <- as.character(mod$type %||% "")
  parts <- sprintf(
    "<div style=\"font-family:Times New Roman,Times,serif;font-size:12pt;\">\n<h2 style=\"font-size:14pt;\">%s</h2>\n",
    html_escape_text(title)
  )

  if (identical(typ, "coef_table")) {
    if (is.null(result)) {
      parts <- c(parts, "<p><em>Run the analysis to populate path coefficients.</em></p>\n")
    } else {
      tab <- estimates_table_for_report(
        state, result, dv_id = mod$params$dv_id %||% NULL
      )
      parts <- c(parts, report_df_to_html_table(tab, paste("Table.", title)))
    }
  } else if (identical(typ, "cfa_measurement")) {
    lat <- mod$params$latent_id %||% NA_character_
    sm <- latent_measurement_summary(result, state, lat)
    if (is.null(sm) || is.null(sm$loadings) || !nrow(sm$loadings)) {
      parts <- c(parts, "<p><em>No CFA loadings available for this latent.</em></p>\n")
    } else {
      bits <- character()
      if (is.finite(sm$alpha)) {
        bits <- c(bits, sprintf("Cronbach α = %.2f", sm$alpha))
      }
      bits <- c(bits, sprintf(
        "score = %s",
        if (identical(sm$score_method, "joint")) "joint Thomson" else "own indicators"
      ))
      if (length(bits)) {
        parts <- c(parts, "<p>", html_escape_text(paste(bits, collapse = " · ")), "</p>\n")
      }
      tab <- latent_loadings_df(sm$loadings, state)
      parts <- c(parts, report_df_to_html_table(tab, paste("Table.", title)))
    }
  } else if (identical(typ, "pca_screen")) {
    lat <- mod$params$latent_id %||% NA_character_
    sm <- latent_measurement_summary(result, state, lat)
    uni <- if (is.null(sm)) NULL else sm$uni
    ust <- if (is.list(uni)) as.character(uni$status %||% NA_character_) else NA_character_
    if (!is.list(uni) || is.na(ust) || !nzchar(ust)) {
      parts <- c(parts, "<p><em>No PCA unidimensionality screen for this latent.</em></p>\n")
    } else {
      bits <- character()
      if (nzchar(as.character(uni$label %||% ""))) bits <- c(bits, as.character(uni$label))
      if (is.finite(uni$ratio)) {
        bits <- c(bits, sprintf("λ₁/λ₂ = %.2f", uni$ratio))
      }
      if (is.finite(uni$pct1)) {
        bits <- c(bits, sprintf("PC1 = %.1f%%", 100 * uni$pct1))
      }
      if (length(bits)) {
        parts <- c(parts, "<p>", html_escape_text(paste(bits, collapse = " · ")), "</p>\n")
      }
      tab <- latent_pca_eigen_df(uni)
      if (nrow(tab)) {
        names(tab)[names(tab) == "PctVar"] <- "% var"
        parts <- c(
          parts,
          report_df_to_html_table(tab, paste("Table.", title, "— eigenvalues"))
        )
      }
      ld <- latent_pca_loadings_df(uni, state, which = "unrotated")
      if (nrow(ld) && ncol(ld) > 1L) {
        parts <- c(
          parts,
          report_df_to_html_table(
            ld, paste("Table.", title, "— item loadings (unrotated)")
          )
        )
      }
      ld_rot <- latent_pca_loadings_df(uni, state, which = "rotated")
      if (nrow(ld_rot) && ncol(ld_rot) > 1L) {
        parts <- c(
          parts,
          report_df_to_html_table(
            ld_rot, paste("Table.", title, "— item loadings (varimax)")
          )
        )
      }
    }
  } else if (identical(typ, "equations")) {
    if (is.null(result)) {
      parts <- c(parts, "<p><em>Run the analysis to see fitted equations.</em></p>\n")
    } else {
      # Word does not run MathJax; paste LaTeX source in a readable block.
      eq <- if (isTRUE(result$model_averaged)) {
        format_averaged_equations_latex(result, state)
      } else {
        format_best_equations_latex(result, state)
      }
      if (is.null(eq) || !nzchar(eq)) {
        parts <- c(parts, "<p><em>No equations available for this fit.</em></p>\n")
      } else {
        parts <- c(
          parts,
          "<p><em>Model equations (LaTeX; use the TeX button for a clean fragment):</em></p>\n",
          "<pre style=\"white-space:pre-wrap;font-family:Cambria Math,Times New Roman,serif;",
          "font-size:11pt;\">", html_escape_text(eq), "</pre>\n"
        )
      }
    }
  } else if (identical(typ, "residual") || identical(typ, "edge_plot")) {
    parts <- c(
      parts,
      "<p><em>Figure not embedded in the clipboard. ",
      "Use Export PDF to include plots, or copy the figure from the app.</em></p>\n"
    )
  } else if (identical(typ, "power")) {
    rec <- format_power_recommend_text(power)
    parts <- c(parts, sprintf("<p>%s</p>\n", html_escape_text(rec)))
    detail <- format_power_details_latex(power)
    if (nzchar(detail)) {
      parts <- c(
        parts,
        "<pre style=\"white-space:pre-wrap;font-family:Times New Roman,serif;\">",
        html_escape_text(detail),
        "</pre>\n"
      )
    }
    if (!is.null(power) && !is.null(power$power) && nrow(power$power)) {
      parts <- c(
        parts,
        "<p><em>Power curves are not embedded; use Export PDF for figures.</em></p>\n"
      )
    }
  } else {
    parts <- c(parts, "<p><em>Unknown module type.</em></p>\n")
  }

  parts <- c(parts, "</div>\n")
  # Word-friendly clipboard package: HTML fragment + Word conditional comment wrapper
  inner <- paste(parts, collapse = "")
  html <- paste0(
    "<html><body>\n",
    "<!--StartFragment-->\n",
    inner,
    "<!--EndFragment-->\n",
    "</body></html>\n"
  )
  list(html = html, text = html_to_plain_text(inner))
}

#' Full APA~7 LaTeX document (`apa7` class) for pinned report modules
#'
#' Uses manuscript mode with floats in the text. Geometry, booktabs, graphicx,
#' caption, and hyperref are provided by `apa7` itself.
#' @keywords internal
report_build_latex <- function(modules, state, result = NULL, power = NULL,
                               dir,
                               analysis_data = NULL, sim_data = NULL,
                               results_simulated = FALSE) {
  modules <- modules %||% empty_report_modules()
  body <- character()
  table_no <- 1L
  fig_no <- 1L
  if (!length(modules)) {
    body <- "\\textit{No modules pinned.}\n"
  } else {
    for (m in modules) {
      chunk <- report_module_to_latex(
        m, state, result, power, dir,
        table_no = table_no, fig_no = fig_no,
        analysis_data = analysis_data,
        sim_data = sim_data,
        results_simulated = results_simulated
      )
      body <- c(body, chunk$tex)
      table_no <- chunk$table_no
      fig_no <- chunk$fig_no
    }
  }
  paste0(
    "\\documentclass[man,floatsintext,12pt]{apa7}\n",
    "\\usepackage{amsmath,amssymb}\n",
    "\\usepackage[dvipsnames,svgnames,x11names,table]{xcolor}\n",
    "\\title{Structural Model Tester Report}\n",
    "\\shorttitle{SMT Report}\n",
    "\\author{Structural Model Tester}\n",
    "\\authorsaffiliations{Generated from pinned analysis modules}\n",
    "\\abstract{This report was exported from Structural Model Tester. ",
    "Pinned modules are rendered with APA~7 manuscript formatting ",
    "(apa7 document class).}\n",
    "\\begin{document}\n",
    "\\maketitle\n",
    paste(body, collapse = "\n"),
    "\n\\end{document}\n"
  )
}

#' Compile pinned report modules to a PDF file
#'
#' @param outfile Destination PDF path.
#' @return `outfile` (invisibly) on success; stops on failure.
#' @keywords internal
report_build_pdf <- function(modules, state, result = NULL, power = NULL,
                             outfile,
                             analysis_data = NULL, sim_data = NULL,
                             results_simulated = FALSE) {
  if (!report_export_available()) {
    stop(
      "PDF export requires the tinytex package and a LaTeX engine ",
      "(install.packages(\"tinytex\"); tinytex::install_tinytex()).",
      call. = FALSE
    )
  }
  modules <- modules %||% empty_report_modules()
  if (!length(modules)) {
    stop("Pin at least one module before exporting the report.", call. = FALSE)
  }
  work <- tempfile("smt-report-")
  dir.create(work)
  owd <- getwd()
  on.exit({
    setwd(owd)
    unlink(work, recursive = TRUE)
  }, add = TRUE)

  tex_body <- report_build_latex(
    modules, state, result, power, dir = work,
    analysis_data = analysis_data,
    sim_data = sim_data,
    results_simulated = results_simulated
  )
  tex_file <- file.path(work, "smt_report.tex")
  writeLines(tex_body, tex_file, useBytes = TRUE)

  # tinytex::latexmk (emulation) invokes pdflatex with an absolute .tex path
  # but does not chdir; relative \\includegraphics paths must resolve from cwd.
  setwd(work)
  pdf_path <- tryCatch(
    tinytex::latexmk("smt_report.tex", pdf_file = "smt_report.pdf"),
    error = function(e) {
      stop("LaTeX compilation failed: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (is.null(pdf_path) || !file.exists(pdf_path)) {
    alt <- "smt_report.pdf"
    if (file.exists(alt)) {
      pdf_path <- alt
    } else {
      stop("LaTeX compilation produced no PDF.", call. = FALSE)
    }
  }
  # Copy while still in work; on.exit restores cwd then deletes work
  if (!file.copy(pdf_path, outfile, overwrite = TRUE)) {
    stop("Could not write PDF to destination.", call. = FALSE)
  }
  invisible(outfile)
}
