# Pedagogical field help: "?" icons open a modal comparing analysis options.

#' Catalog of analysis-field help topics
#' @keywords internal
smt_field_help_topics <- function() {
  list(
    inference_mode = list(
      title = "Analysis framework",
      lead = paste(
        "This choice sets how evidence for each path is quantified and how",
        "exploratory links can be pruned. It does not change the graph you drew;",
        "it changes the inferential language of the Run."
      ),
      options = list(
        list(
          label = "p-values",
          summary = "Classical equation-wise fits (lm / glm / lmer) with null-hypothesis tests.",
          pros = c(
            "Familiar reporting (estimates, SE, p).",
            "Fast; works with mixed models and binary outcomes.",
            "Multiplicity tools for confirmatory links."
          ),
          cons = c(
            "p answers “compatibility with a sharp null”, not practical importance.",
            "Does not use your Expected b ± SD as a prior."
          )
        ),
        list(
          label = "Bayesian (brms)",
          summary = paste(
            "Stan sampling with normal priors from Expected b ± SD.",
            "Reports posterior mean, HDI, pd, ROPE, and log BF10."
          ),
          pros = c(
            "Priors make Expected effects part of the model.",
            "HDI + ROPE speak to negligible vs supported effects.",
            "Optional joint model can share residual correlation across outcomes."
          ),
          cons = c(
            "Slower (MCMC).",
            "Needs brms + a Stan backend.",
            "Interpretation depends on prior and ROPE width."
          )
        ),
        list(
          label = "Model comparison (AIC/BIC)",
          summary = paste(
            "Searches subsets of exploratory links and ranks models by AIC or BIC.",
            "Can reject weak links by Σw and optionally average coefficients."
          ),
          pros = c(
            "Directly targets which exploratory paths belong in the model.",
            "Σw summarizes evidence across the model set."
          ),
          cons = c(
            "Search cost grows with exploratory links.",
            "IC ranking is not the same as a p-value or a Bayes factor.",
            "Inference after search is conditional on the selected model."
          )
        )
      )
    ),
    mark_nonsig = list(
      title = "Mark unsupported exploratory links",
      lead = paste(
        "When enabled, exploratory links that fail the framework’s support rule",
        "are dashed on the canvas and treated as unsupported for power / reporting.",
        "Confirmatory links are never removed by this toggle."
      ),
      options = list(
        list(
          label = "On",
          summary = "Auto-mark exploratory links that lack support after Run.",
          pros = c(
            "Keeps the canvas honest about weak exploratory paths.",
            "Aligns power “detection” with what you would retain."
          ),
          cons = c(
            "A dashed link can still be scientifically interesting — marking is a filter, not proof of absence."
          )
        ),
        list(
          label = "Off",
          summary = "Keep all exploratory estimates visible regardless of support.",
          pros = c("Useful while exploring or teaching before pruning."),
          cons = c("Easy to over-interpret unsupported exploratory paths.")
        )
      ),
      note = paste(
        "p-values: unsupported when p > α.",
        "Bayesian: unsupported when the HDI is not entirely outside the ROPE."
      )
    ),
    conf_p_adjust = list(
      title = "Confirmatory multiplicity correction",
      lead = paste(
        "Several confirmatory paths inflate the chance of at least one false",
        "“significant” result. This adjusts confirmatory p-values used on the",
        "canvas, coefficient table, and power detection."
      ),
      options = list(
        list(
          label = "None",
          summary = "Raw p-values; no family-wise or FDR control.",
          pros = c("Most powerful per test.", "Matches a single-link analysis."),
          cons = c("Family-wise error rises with more confirmatory links.")
        ),
        list(
          label = "Bonferroni",
          summary = "Divide α by the number of confirmatory tests (strict).",
          pros = c("Strong family-wise error control."),
          cons = c("Often overly conservative with many links.")
        ),
        list(
          label = "Holm",
          summary = "Step-down improvement on Bonferroni; still controls family-wise error.",
          pros = c("Safer than Bonferroni in power.", "Good default for confirmatory sets."),
          cons = c("Still more conservative than FDR methods.")
        ),
        list(
          label = "Benjamini–Hochberg",
          summary = "Controls false discovery rate among confirmatory tests.",
          pros = c("More power when many effects may be real."),
          cons = c(
            "Allows a controlled share of false discoveries — not family-wise control."
          )
        )
      )
    ),
    bayes_structure = list(
      title = "Bayesian equation structure",
      lead = paste(
        "How the path equations are sampled. Link-level indices (HDI, pd, ROPE,",
        "log BF10) are always reported per coefficient; the structure changes",
        "whether residual dependence across outcomes is modeled."
      ),
      options = list(
        list(
          label = "Independent",
          summary = "One Stan model per outcome equation (like separate regressions).",
          pros = c(
            "Robust with mixed families (e.g. binary + continuous).",
            "A problem in one equation does not block the others.",
            "Closest analogue to the frequentist path engine."
          ),
          cons = c(
            "Ignores residual correlation between outcomes.",
            "No joint posterior across equations."
          )
        ),
        list(
          label = "Joint (residual correlations)",
          summary = paste(
            "One multivariate gaussian model with residual correlations (SUR / SEM-style).",
            "Falls back to independent if equations are incompatible."
          ),
          pros = c(
            "Models shared leftover covariance across DVs.",
            "Often more efficient when residuals are correlated.",
            "Supports joint uncertainty across equations."
          ),
          cons = c(
            "Currently continuous outcomes only (no binary / categorical).",
            "No random slopes yet; clustering must match across equations.",
            "One MCMC failure blocks the whole path stage.",
            "Listwise deletion across all joint outcomes."
          )
        )
      )
    ),
    bayes_ci = list(
      title = "HDI mass",
      lead = paste(
        "The highest-density interval (HDI) is the shortest interval containing",
        "the chosen posterior probability mass. It is the Bayesian interval used",
        "for ROPE decisions and canvas labels."
      ),
      options = list(
        list(
          label = "Higher (e.g. 0.99)",
          summary = "Wider interval; harder for the HDI to lie wholly outside the ROPE.",
          pros = c("More conservative “supported effect” claims."),
          cons = c("More links stay undecided / unsupported under HDI+ROPE.")
        ),
        list(
          label = "0.95 (default)",
          summary = "Common reporting level; balances width and decisiveness.",
          pros = c("Familiar analogue to a 95% interval."),
          cons = c("Still a convention — not a law of nature.")
        ),
        list(
          label = "Lower (e.g. 0.80)",
          summary = "Narrower interval; easier to clear the ROPE.",
          pros = c("More decisive with modest samples."),
          cons = c("Weaker coverage guarantee; easier to overclaim.")
        )
      )
    ),
    bayes_rope_k = list(
      title = "ROPE width",
      lead = paste(
        "The region of practical equivalence around 0 is",
        "± (k × Expected SD) for that link. Effects inside the ROPE are treated",
        "as negligible for HDI+ROPE decisions."
      ),
      options = list(
        list(
          label = "Smaller k (tighter ROPE)",
          summary = "Only tiny effects count as negligible.",
          pros = c("Easier to “reject” the ROPE (claim a non-negligible effect)."),
          cons = c(
            "May treat substantively tiny slopes as meaningful.",
            "Needs a clear substantive justification."
          )
        ),
        list(
          label = "k = 0.1 (default)",
          summary = "With Expected SD = 1, ROPE = [−0.1, 0.1].",
          pros = c("Transparent default tied to your prior SD."),
          cons = c("Still arbitrary unless it matches your “negligible” scale.")
        ),
        list(
          label = "Larger k (wider ROPE)",
          summary = "Larger effects can still count as practically zero.",
          pros = c("Harder to claim support; favors caution."),
          cons = c("May hide small-but-real effects you care about.")
        )
      )
    ),
    ic_criterion = list(
      title = "Information criterion",
      lead = paste(
        "During exploratory search, models are ranked by an information criterion.",
        "Lower is better. The criterion also drives Akaike/BIC weights (Σw)."
      ),
      options = list(
        list(
          label = "AIC",
          summary = "Favors predictive fit; lighter penalty for extra parameters.",
          pros = c("More inclusive — tends to keep more exploratory links."),
          cons = c("Higher risk of retaining noise paths.")
        ),
        list(
          label = "BIC",
          summary = "Stronger complexity penalty; closer to a unit-information Bayes factor intuition.",
          pros = c("More parsimonious models.", "Often preferred for “which links belong”."),
          cons = c("Can drop small true effects, especially at modest N.")
        )
      )
    ),
    min_weight = list(
      title = "Minimum Σw to keep",
      lead = paste(
        "Σw is the sum of Akaike/BIC weights of models that include the link.",
        "It is evidence that the link belongs somewhere in the model set — not a p-value."
      ),
      options = list(
        list(
          label = "High (e.g. 0.95)",
          summary = "Keep only links that appear in almost all plausible models.",
          pros = c("Strict, stable structures."),
          cons = c("May discard weakly supported but real paths.")
        ),
        list(
          label = "Moderate (e.g. 0.50)",
          summary = "Keep links with substantial cumulative weight.",
          pros = c("More exploratory retention."),
          cons = c("Noisier canvas; more disputed links survive.")
        )
      )
    ),
    mark_low_weight = list(
      title = "Reject links with Σw below threshold",
      lead = paste(
        "When enabled, confirmatory and exploratory links below Min Σw are dashed",
        "and do not count as detected in power analysis."
      ),
      options = list(
        list(
          label = "On",
          summary = "Apply the Σw gate to the canvas and detection rules.",
          pros = c("Consistent pruning rule across links."),
          cons = c("A confirmatory link can be marked unsupported if Σw is low.")
        ),
        list(
          label = "Off",
          summary = "Show estimates even when Σw is below the threshold.",
          pros = c("Useful to inspect borderline links."),
          cons = c("Easy to read dashed-worthy links as retained.")
        )
      )
    ),
    model_average = list(
      title = "Model averaging (full)",
      lead = paste(
        "Instead of committing to one best IC model, full averaging combines",
        "coefficients across the model set (missing predictors treated as 0)."
      ),
      options = list(
        list(
          label = "Off (default)",
          summary = "Canvas uses the single selected (best) model.",
          pros = c("Simple interpretation of one structure."),
          cons = c("Ignores model uncertainty.")
        ),
        list(
          label = "On",
          summary = "Canvas shows model-averaged estimates.",
          pros = c("Reflects uncertainty about which exploratory links belong."),
          cons = c(
            "Averaged coefficients are not “the” model’s OLS/ML estimates.",
            "Harder to narrate as a single equation system."
          )
        )
      )
    ),
    uncertainty_method = list(
      title = "Final-model uncertainty",
      lead = paste(
        "Optional resampling after the model is chosen. Intervals then reflect",
        "sampling (and/or score) noise conditional on that selected structure —",
        "not uncertainty about which model was selected. Not used under Bayesian Run."
      ),
      options = list(
        list(
          label = "None",
          summary = "Use analytic SE / p from the fitted equations only.",
          pros = c("Fast.", "Standard textbook output."),
          cons = c("Can understate uncertainty with latents or complex steps.")
        ),
        list(
          label = "Bootstrap",
          summary = "Resample people (rows / clusters) and refit the selected paths.",
          pros = c("Captures sampling variability of the final model."),
          cons = c(
            "Slower.",
            "Still conditional on the selected model (no model-selection uncertainty)."
          )
        ),
        list(
          label = "Plausible values",
          summary = "Redraw latent factor scores, then refit structural paths.",
          pros = c("Propagates factor-score uncertainty into path estimates."),
          cons = c(
            "Relevant mainly with latents.",
            "Does not replace bootstrap of the full sample."
          )
        )
      )
    ),
    latent_score_method = list(
      title = "Latent factor scores",
      lead = paste(
        "When several latents are scored for the path stage, this chooses whether",
        "each factor uses only its own indicators or a joint multi-factor scoring."
      ),
      options = list(
        list(
          label = "Own indicators",
          summary = "Score each latent from its measurement links only.",
          pros = c(
            "Cleaner separation between constructs.",
            "Default; coincides with joint when there is only one latent."
          ),
          cons = c("Ignores information from correlated factors’ items.")
        ),
        list(
          label = "Joint (Thomson)",
          summary = "Regression / Thomson scores from the multi-factor CFA.",
          pros = c(
            "Can improve score precision when factors correlate.",
            "Uses the full measurement model."
          ),
          cons = c(
            "Items from other constructs can receive non-zero weights.",
            "Harder to describe as “pure” single-factor scores."
          )
        )
      )
    )
  )
}

#' Help "?" control for one topic id
#' @keywords internal
smt_help_button <- function(topic_id) {
  topic_id <- as.character(topic_id)[[1]]
  shiny::actionLink(
    inputId = paste0("smt_help_", topic_id),
    label = shiny::span(class = "smt-help-btn-glyph", "?"),
    class = "smt-help-btn",
    title = "What do these options mean?"
  )
}

#' Label row with trailing help button
#' @keywords internal
smt_label_with_help <- function(text, topic_id) {
  shiny::tags$span(
    class = "smt-field-label-row",
    shiny::tags$span(class = "smt-field-label-text", text),
    smt_help_button(topic_id)
  )
}

#' Build the help modal for one topic
#' @keywords internal
smt_field_help_modal <- function(topic_id) {
  topic_id <- as.character(topic_id)[[1]]
  topics <- smt_field_help_topics()
  topic <- topics[[topic_id]]
  if (is.null(topic)) {
    return(shiny::modalDialog(
      title = "Help",
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      shiny::p("No help is registered for this field yet.")
    ))
  }

  opt_cards <- lapply(topic$options %||% list(), function(opt) {
    pros <- opt$pros %||% character()
    cons <- opt$cons %||% character()
    shiny::div(
      class = "smt-help-option",
      shiny::div(class = "smt-help-option-label", opt$label %||% ""),
      shiny::p(class = "smt-help-option-summary", opt$summary %||% ""),
      if (length(pros)) {
        shiny::div(
          class = "smt-help-list smt-help-pros",
          shiny::div(class = "smt-help-list-title", "Benefits"),
          shiny::tags$ul(lapply(pros, shiny::tags$li))
        )
      },
      if (length(cons)) {
        shiny::div(
          class = "smt-help-list smt-help-cons",
          shiny::div(class = "smt-help-list-title", "Trade-offs"),
          shiny::tags$ul(lapply(cons, shiny::tags$li))
        )
      }
    )
  })

  shiny::modalDialog(
    title = NULL,
    easyClose = TRUE,
    fade = TRUE,
    size = "l",
    footer = shiny::modalButton("Close"),
    shiny::div(
      class = "smt-help-modal",
      shiny::div(
        class = "smt-help-hero",
        shiny::div(class = "smt-help-kicker", "Help"),
        shiny::h3(topic$title %||% topic_id),
        shiny::p(class = "smt-help-lead", topic$lead %||% "")
      ),
      shiny::div(class = "smt-help-options", opt_cards),
      if (!is.null(topic$note) && nzchar(topic$note)) {
        shiny::div(class = "smt-help-note", topic$note)
      }
    )
  )
}

#' Register observers that open field-help modals
#' @keywords internal
smt_register_field_help <- function(input, session) {
  ids <- names(smt_field_help_topics())
  lapply(ids, function(id) {
    shiny::observeEvent(
      input[[paste0("smt_help_", id)]],
      {
        shiny::showModal(smt_field_help_modal(id))
      },
      ignoreInit = TRUE
    )
  })
  invisible(NULL)
}
