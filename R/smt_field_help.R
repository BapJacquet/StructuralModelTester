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
        "log BF10) are always reported per coefficient. Joint mode shares one",
        "multivariate posterior; residual correlations are used only when no",
        "outcome predicts another."
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
          label = "Joint (multivariate)",
          summary = paste(
            "One multivariate gaussian model for all continuous outcomes.",
            "Residual correlations are used when DVs are siblings only;",
            "if the graph has mediation / paths among outcomes, residual",
            "correlation is turned off automatically (otherwise path vs",
            "rescor is not identified). Falls back to independent if",
            "equations are incompatible."
          ),
          pros = c(
            "One joint posterior across equations.",
            "Models leftover covariance when DVs do not predict each other.",
            "Safe default for mediation: keeps directed paths identified."
          ),
          cons = c(
            "Currently continuous outcomes only (no binary / categorical).",
            "No random slopes yet; clustering must match across equations.",
            "One MCMC failure blocks the whole path stage.",
            "Listwise deletion across all joint outcomes.",
            "With mediation, joint without rescor is close to stacking equations."
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
    ),
    bayes_diagnostics = list(
      title = "Bayesian diagnostics (MCMC + PPC)",
      lead = paste(
        "After a Bayesian Run, SMT shows sampling health (MCMC) and whether the",
        "model can reproduce the outcome distribution (PPC). Use these before",
        "trusting HDI, ROPE, or BF reports for that equation."
      ),
      options = list(
        list(
          label = "MCMC — Rhat vs ESS",
          summary = paste(
            "Each point is a parameter. High Rhat or low ESS ratio means the",
            "chains have not mixed well enough for stable posterior summaries."
          ),
          figure = list(
            src = "help/mcmc-scatter.png",
            alt = "Example Rhat versus ESS ratio scatter with attention thresholds",
            caption = "Dashed lines: Rhat = 1.01 and ESS ratio = 0.10. Orange = flagged."
          ),
          details = c(
            "Rhat near 1 means chain means agree. SMT warns from 1.01 and treats ≥ 1.05 as serious.",
            "ESS ratio is effective sample size ÷ total post-warmup draws. Below 0.10 is weak; very low ESS also flags absolute bulk ESS < 100 when available.",
            "Divergent transitions (shown in the numeric summary) mean the sampler struggled with the geometry — do not ignore even if Rhat looks fine."
          ),
          pros_title = "What to do if flagged",
          pros = c(
            "Increase iterations / warmup, or add a chain.",
            "Check priors and scaling (very tight or very wide Expected SDs can hurt sampling).",
            "For repeated measures, weakly identified random slopes often show tiny ESS — simplify the RE structure if needed."
          ),
          cons_title = "What it is not",
          cons = c(
            "Passing MCMC checks does not prove the model is scientifically correct.",
            "A flat Rhat axis (all ≈ 1.00) with empty space above is still “OK” — the PPC is the better visual for fit."
          )
        ),
        list(
          label = "PPC — good dens overlay",
          summary = paste(
            "Grey curves are densities of replicated outcomes y_rep; black is the",
            "observed y. A good check: black sits inside the grey cloud."
          ),
          figure = list(
            src = "help/ppc-good.png",
            alt = "Example good posterior predictive dens overlay",
            caption = "Observed density (black) matches the y_rep cloud (grey)."
          ),
          details = c(
            "For binary outcomes SMT shows a histogram of replicated means with a line at the observed mean instead of dens overlay."
          ),
          pros_title = "Read as",
          pros = c(
            "The likelihood + priors can regenerate data that look like what you observed."
          ),
          cons_title = "Caveat",
          cons = c(
            "A good dens overlay checks the marginal outcome distribution, not every conditional effect."
          )
        ),
        list(
          label = "PPC — location problem",
          summary = "If the grey cloud is shifted away from the black curve, the model systematically misses the outcome level.",
          figure = list(
            src = "help/ppc-shift.png",
            alt = "Example PPC with mean shift between y and y_rep",
            caption = "y_rep centered too low/high relative to observed y."
          ),
          details = c(
            "Common causes: wrong intercept / omitted mean structure, bad coding of factors, or priors that pull the mean hard."
          ),
          pros_title = "What to check",
          pros = c(
            "Equation intercept and factor reference levels.",
            "Whether Expected priors on slopes are fighting a strong likelihood."
          ),
          cons_title = "Caveat",
          cons = c("A shift can also appear if you plot the wrong response in a joint model.")
        ),
        list(
          label = "PPC — spread problem",
          summary = "If y_rep is much wider or much tighter than observed y, residual / random-effect variance is off.",
          figure = list(
            src = "help/ppc-spread.png",
            alt = "Example PPC with overdispersed y_rep",
            caption = "Replicated densities flatter than the observed outcome."
          ),
          details = c(
            "Too-wide y_rep: residual SD or RE SDs too large (or missing predictors).",
            "Too-narrow y_rep: residual SD too small, or overfitting / underdispersed family."
          ),
          pros_title = "What to check",
          pros = c(
            "Random intercept / slope SDs in the Model fit equation.",
            "Whether a continuous family is appropriate for the outcome."
          ),
          cons_title = "Caveat",
          cons = c(
            "PPC dens overlay is sensitive to N and bandwidth; compare shapes, not pixel-perfect match."
          )
        )
      ),
      note = paste(
        "The Results MCMC strip is a run-level alarm; open a DV for the plots.",
        "Fix sampling problems before interpreting ROPE / BF; use PPC to critique the generative fit."
      )
    ),
    # Section overview; per-column help opens when a table header is clicked.
    path_coefficients = list(
      title = "Path coefficients",
      lead = paste(
        "Structural path estimates for outcome equations (not measurement loadings).",
        "Click any column header in the table for help on that indicator."
      ),
      options = list(
        list(
          label = "How to read the table",
          summary = paste(
            "Rows are directed effects (Path). Level marks factor codes, via / total",
            "mediation rows, or blank for ordinary slopes."
          ),
          details = c(
            "Select a DV on the canvas to filter rows to that outcome equation.",
            "Bayesian columns (HDI, pd, ROPE, KL, SD↓, …) appear under Framework = Bayesian.",
            "Classical SE / p appear under p-values (and may coexist in some tables)."
          ),
          pros_title = "Tip",
          pros = c(
            "Click a header (e.g. KL or ROPE) for a focused explanation of that column."
          ),
          cons_title = "Caveat",
          cons = c(
            "Pinning the table to Report copies values, not this interactive header help."
          )
        )
      ),
      note = paste(
        "For “how informative were the data?”, pair SD↓ (narrowing) with KL",
        "(total prior→posterior change) and the edge prior/posterior plot."
      )
    )
  )
}

#' Detailed help entries keyed by path-coefficient column kind
#' @keywords internal
path_coefficients_column_help_catalog <- function() {
  list(
    Path = list(
      summary = paste(
        "Directed structural effect as outcome ← predictor (e.g. Y ← X).",
        "Indirect rows use a wavy arrow (Y ↜ X); totals use a double arrow (Y ⇐ X)."
      ),
      details = c(
        "Ordinary rows are single-edge slopes with ← .",
        "via M uses ↜ for the product through M; total uses ⇐ for the path-analytic total.",
        "Level still names the mediator or \"total\"; the glyph is a quick visual cue.",
        "Split categorical DVs may appear as Y[level] ← X."
      ),
      pros_title = "How to read it",
      pros = c(
        "Rows with the same left-hand side belong to the same outcome equation.",
        "Arrow style marks direct vs indirect vs total without reading Level first."
      ),
      cons_title = "Caveats",
      cons = c(
        "Path names the endpoints only — size and uncertainty are in Estimate / SE / HDI.",
        "Polynomial or interaction regressors may use design-matrix labels on the right."
      )
    ),
    Level = list(
      summary = paste(
        "Extra identity for the coefficient: factor contrast, omnibus test,",
        "or mediation path."
      ),
      details = c(
        "Blank: continuous predictor (single slope).",
        "A category label: the level coded 1 in the treatment contrast (vs the reference base).",
        "For binary predictors this is the non-base level (e.g. Female when Male is base).",
        "A×B: product of the two sides’ coded-1 levels for an interaction term.",
        "omnibus: joint test that all levels of a categorical predictor are zero.",
        "via M: indirect effect a×b through mediator M (Sobel SE/p classically; posterior of a×b under Bayesian Run).",
        "total: direct + all via-mediator products for that Path."
      ),
      pros_title = "How to read it",
      pros = c(
        "Blank Level → continuous slope; named Level → contrast for that category vs base.",
        "Compare level rows within the same Path for category-specific slopes.",
        "Read via M and total together with the blank Level direct path for a mediation triad."
      ),
      cons_title = "Caveats",
      cons = c(
        "Omnibus rows often have no single Estimate (NA) — use p / HDI on the joint test.",
        "Indirect / total Levels are not factor levels of the predictor.",
        "Classical total SE approximates independence of direct and indirect pieces."
      )
    ),
    Estimate = list(
      summary = paste(
        "Point summary of the structural coefficient: classical MLE/OLS-style estimate,",
        "or posterior mean under Bayesian Run. via M rows show a×b; total rows show",
        "direct + all indirects for that Path."
      ),
      details = c(
        "Sign shows direction (positive = higher predictor associates with higher outcome, all else equal).",
        "Scale is in the units of the outcome per unit of the predictor (after any DV transform).",
        "Total is the path-analytic effect through the fitted graph (not a separate reduced-form regression)."
      ),
      pros_title = "How to read it",
      pros = c(
        "Classical: best single-number fit under the equation’s estimator.",
        "Bayesian: center of the posterior for θ after combining prior and data."
      ),
      cons_title = "Caveats",
      cons = c(
        "A large Estimate with huge SE/SD can still be compatible with zero.",
        "Posterior mean ≠ MAP and is pulled by the Expected b ± SD prior when data are weak."
      )
    ),
    `Σw` = list(
      summary = paste(
        "Sum of Akaike/BIC weights of candidate models that include this link",
        "(AIC/BIC search only)."
      ),
      details = c(
        "Each model’s weight is proportional to exp(−½ ΔIC); Σw adds those weights over models containing the edge.",
        "1 ≈ the link is in (almost) every plausible model; near 0 ≈ rarely selected.",
        "Always-included fixed/measurement links are not the usual Σw story — focus on confirmatory/exploratory free paths.",
        "Shown for searched links in place of p (AIC/BIC Run).",
        "via M: joint Σw = sum of weights of models that include both constituent edges.",
        "total: Σw of models with the direct edge or any complete via chain.",
        "With model averaging on, via/total Estimates are full averages of a×b (and c′+Σa×b), absent → 0."
      ),
      pros_title = "How to read it",
      pros = c(
        "Compare Σw to Min Σw: below the threshold the canvas can mark the link unsupported.",
        "High joint Σw on via M means the whole mediation chain is commonly selected together."
      ),
      cons_title = "Caveats",
      cons = c(
        "Σw is about model inclusion, not about whether the coefficient differs from zero.",
        "Depends on the candidate set (which free edges were searched).",
        "Product of averaged slopes ≠ averaged product — use the via row under averaging, not Σw margins alone."
      )
    ),
    SE = list(
      summary = "Classical standard error of Estimate (sampling variability under the fitted model).",
      details = c(
        "Used to form Wald-style tests and classical confidence intervals (not shown as a separate column).",
        "For indirect rows, SE is the Sobel (delta-method) SE of a×b."
      ),
      pros_title = "How to read it",
      pros = c(
        "Smaller SE → more precise Estimate.",
        "Rough 95% Wald interval ≈ Estimate ± 1.96×SE (continuous large-sample intuition)."
      ),
      cons_title = "Caveats",
      cons = c(
        "Assumes the model and asymptotics; bad for tiny N or awkward discrete DVs.",
        "Not a posterior uncertainty measure — see SD / HDI under Bayesian Run."
      )
    ),
    SD = list(
      summary = "Posterior standard deviation of the coefficient (Bayesian Run).",
      details = c(
        "Spread of the MCMC draws for θ after warm-up.",
        "Shown instead of SE when the table is in posterior mode without classical p-values."
      ),
      pros_title = "How to read it",
      pros = c(
        "Larger SD → more posterior uncertainty (weak data, wide prior, or collinearity).",
        "Compare SD to |Estimate|: if SD is similar to |Estimate|, the sign is not sharp."
      ),
      cons_title = "Caveats",
      cons = c(
        "SD alone does not say whether 0 is plausible — use the HDI and ROPE.",
        "Prior width (Expected SD) still influences posterior SD when N is small."
      )
    ),
    p = list(
      summary = paste(
        "Two-sided p-value against θ = 0 for that coefficient (or Sobel p for via M rows)."
      ),
      details = c(
        "Small p means the data are surprising under a sharp null of no association in this equation,",
        "given the model. It is not the probability that the null is true."
      ),
      pros_title = "How to read it",
      pros = c(
        "Use with Estimate and SE: p answers compatibility with zero, not practical size.",
        "Exploratory marking (if on) uses α; confirmatory paths may also show pHolm / pBonf / pBH."
      ),
      cons_title = "Caveats",
      cons = c(
        "p is not effect size and not posterior probability.",
        "Multiple paths inflate false positives unless you use the adjusted column for confirmatory links."
      )
    ),
    pHolm = list(
      summary = "Holm-adjusted p for confirmatory paths only (family-wise error control).",
      details = c(
        "Raw p stays in p. Blank cells are non-confirmatory or not in the adjustment set."
      ),
      pros_title = "How to read it",
      pros = c("Prefer pHolm over p when deciding confirmatory support under Holm correction."),
      cons_title = "Caveats",
      cons = c("Still a frequentist multiplicity adjustment — not a Bayesian posterior probability.")
    ),
    pBonf = list(
      summary = "Bonferroni-adjusted p for confirmatory paths only.",
      details = c("Stricter than Holm; blank for rows outside the confirmatory family."),
      pros_title = "How to read it",
      pros = c("Use when you want a simple, conservative confirmatory threshold."),
      cons_title = "Caveats",
      cons = c("Often under-powered with many confirmatory links.")
    ),
    pBH = list(
      summary = "Benjamini–Hochberg adjusted p (FDR) for confirmatory paths only.",
      details = c("Controls false discovery rate rather than family-wise error."),
      pros_title = "How to read it",
      pros = c("Less conservative than Bonferroni/Holm when many confirmatory tests are expected."),
      cons_title = "Caveats",
      cons = c("FDR ≠ FWER; interpret in a discovery / confirmatory-set context.")
    ),
    HDI = list(
      summary = paste(
        "Highest-density interval: the shortest interval containing the chosen posterior",
        "probability mass (e.g. 95%) for the coefficient."
      ),
      details = c(
        "Every point inside the HDI has density at least as high as every point outside.",
        "SMT uses this HDI for canvas support rules together with the ROPE.",
        "If the entire HDI lies outside the ROPE, the effect is treated as supported (non-negligible).",
        "If the HDI lies wholly inside the ROPE, the effect is practically equivalent to zero under your ROPE.",
        "Otherwise the result is inconclusive under the HDI+ROPE rule."
      ),
      pros_title = "How to read it",
      pros = c(
        "Check whether 0 is inside the HDI (sign uncertainty) separately from ROPE overlap (practical negligibility).",
        "Wider HDI (higher mass, e.g. 99%) is more conservative for “outside ROPE” claims."
      ),
      cons_title = "Caveats",
      cons = c(
        "HDI is not a frequentist confidence interval and does not have repeated-sampling coverage guarantees.",
        "Prior and ROPE width change what “supported” means — report them with the interval."
      )
    ),
    pd = list(
      summary = paste(
        "Probability of direction: the posterior probability that the coefficient has the",
        "same sign as the posterior median (max proportion of draws above or below 0)."
      ),
      details = c(
        "pd ≈ 1 means nearly all posterior mass is on one side of zero.",
        "pd = 0.5 means the posterior is balanced around zero (no preferred direction).",
        "pd is a pure directional index; it does not encode magnitude or practical significance."
      ),
      pros_title = "How to read it",
      pros = c(
        "pd = 0.97 → about 97% of the posterior agrees on the sign of Estimate.",
        "Useful as a continuous alternative to “is 0 in the HDI?” for direction only."
      ),
      cons_title = "Caveats",
      cons = c(
        "High pd can accompany a tiny effect that still sits inside the ROPE.",
        "pd is not a p-value and not a Bayes factor; do not threshold it as if it were α."
      )
    ),
    `ROPE%` = list(
      summary = paste(
        "Percent of posterior draws that fall inside the region of practical equivalence (ROPE)",
        "around 0 — by default ± k × Expected SD for that link."
      ),
      details = c(
        "ROPE operationalizes “negligibly different from zero” on the scale of your Expected SD prior.",
        "High ROPE% means much of the posterior is practically null under that band.",
        "Low ROPE% means little posterior mass is considered negligible."
      ),
      pros_title = "How to read it",
      pros = c(
        "Use with the HDI: mass in ROPE and whether the HDI clears the ROPE answer different questions.",
        "Compare ROPE% across links only if their Expected SDs (hence ROPE widths) are comparable."
      ),
      cons_title = "Caveats",
      cons = c(
        "ROPE% depends entirely on k and Expected SD — change the prior SD and the percent moves.",
        "It is not “probability the null is true”; it is mass inside a user-chosen indifference zone."
      )
    ),
    ROPE = list(
      summary = paste(
        "Discrete HDI+ROPE decision for this coefficient (accept / reject / undecided style label)."
      ),
      details = c(
        "reject (or equivalent): HDI is entirely outside the ROPE → non-negligible effect under your settings.",
        "accept: HDI is entirely inside the ROPE → practically equivalent to zero.",
        "undecided: HDI and ROPE overlap without either inclusion — data are inconclusive for that rule."
      ),
      pros_title = "How to read it",
      pros = c(
        "This is the same rule the canvas uses for Bayesian exploratory marking when enabled.",
        "Always pair the label with the numeric HDI and ROPE% when reporting."
      ),
      cons_title = "Caveats",
      cons = c(
        "A “reject ROPE” claim is only as meaningful as your ROPE width.",
        "Undecided is common with modest N — not evidence for the null."
      )
    ),
    `log BF10` = list(
      summary = paste(
        "Natural log of the Savage–Dickey Bayes factor BF10 for θ = 0 vs the alternative",
        "implied by the prior (evidence for the unrestricted coefficient vs a point null at 0)."
      ),
      details = c(
        "BF10 = posterior density at 0 under the prior predictive setup, inverted vs prior density at 0",
        "(Savage–Dickey): large BF10 favors the alternative (away from 0); BF10 < 1 favors the null.",
        "SMT shows log(BF10): 0 ↔ BF10 = 1; positive ↔ data favor H1; negative ↔ favor H0.",
        paste(
          "Rough guides (Kass & Raftery style) — BF10 / log BF10:",
          "~3 / ~1.1 notable, ~10 / ~2.3 strong, ~100 / ~4.6 decisive.",
          "Same magnitudes with a minus sign favor H0. These rungs are conventions, not bright lines."
        )
      ),
      pros_title = "How to read it",
      pros = c(
        "log BF10 ≈ 2.3 → BF10 ≈ 10 (strong evidence against θ = 0).",
        "log BF10 ≈ −2.3 → BF10 ≈ 0.1 (symmetric evidence favoring the null at 0).",
        "Compare with HDI+ROPE: BF speaks to the point null; ROPE speaks to practical equivalence."
      ),
      cons_title = "Caveats",
      cons = c(
        "Savage–Dickey is sensitive to the prior height at 0 — a very diffuse Expected SD can inflate BF10.",
        "If prior density at 0 is tiny, the BF can become unstable or undefined (blank / Inf).",
        "Do not treat log BF10 as a signed effect size; Estimate + HDI still carry magnitude."
      )
    ),
    KL = list(
      summary = paste(
        "Kullback–Leibler divergence KL(posterior ‖ prior) in nats: total change from",
        "Expected prior to posterior (location shift and/or concentration)."
      ),
      details = c(
        "0 means posterior ≈ prior (no information gain for this θ).",
        "Larger KL means the likelihood moved and/or concentrated beliefs relative to Expected b ± SD.",
        "Because KL mixes shift and narrowing, it is not a pure “how precise did the data make me?” index —",
        "use SD↓ for that. The edge prior/posterior density plot shows both aspects visually.",
        "Reported in natural units (nats); multiply by 1/log(2) for bits if you prefer."
      ),
      pros_title = "How to read it",
      pros = c(
        "Use KL as an overall prior→posterior learning diagnostic, not as hypothesis support.",
        "High KL with Estimate still near the prior mean often means sharpening (check SD↓).",
        "High KL with Estimate far from Expected b means the data pulled the mean as well.",
        "Values below 0.10 nats are highlighted in orange — little prior→posterior learning."
      ),
      cons_title = "Caveats",
      cons = c(
        "KL alone cannot tell shift vs narrowing — pair it with SD↓ and the edge plot.",
        "KL is not a Bayes factor and not a p-value.",
        "Between-link KL comparisons mix different priors and scales — interpret cautiously."
      )
    ),
    `SD↓` = list(
      summary = paste(
        "Posterior narrowing: Expected SD ÷ posterior SD. Pure precision gain relative",
        "to the Expected normal prior, ignoring whether the mean moved."
      ),
      details = c(
        "1 ≈ no tightening (posterior as wide as the prior).",
        "2 ≈ the posterior is half as wide as Expected SD (about 4× prior precision).",
        "Values below 1 can appear if the prior was tighter than the posterior (unusual with diffuse Expected SDs).",
        "This answers “how much did the data shrink uncertainty?”; KL answers “how much did beliefs change overall?”.",
        "Values below 1.5 are highlighted in orange — little shrinkage relative to Expected SD."
      ),
      pros_title = "How to read it",
      pros = c(
        "Compare SD↓ across links with similar Expected SDs to see which slopes the data pinned down.",
        "Large SD↓ with Estimate near 0 and high ROPE% → data confidently say “practically null”.",
        "Small SD↓ → the likelihood was weak relative to the prior width; HDI stays prior-like."
      ),
      cons_title = "Caveats",
      cons = c(
        "Depends on Expected SD: inflate the prior SD and SD↓ rises even with the same posterior.",
        "Does not encode direction or practical significance — still read Estimate, HDI, and ROPE.",
        "MCMC noise in the posterior SD can jitter the ratio slightly."
      )
    )
  )
}

#' Map displayed table column names to catalog keys
#' @keywords internal
path_coefficients_help_keys_for_columns <- function(columns) {
  columns <- unique(as.character(columns %||% character()))
  columns <- columns[nzchar(columns)]
  keys <- character()
  for (col in columns) {
    if (identical(col, "Path")) keys <- c(keys, "Path")
    else if (identical(col, "To")) keys <- c(keys, "Path")
    else if (identical(col, "From")) keys <- c(keys, "Path")
    else if (identical(col, "Level")) keys <- c(keys, "Level")
    else if (identical(col, "Estimate")) keys <- c(keys, "Estimate")
    else if (identical(col, "Σw") || identical(col, "\u03A3w")) keys <- c(keys, "Σw")
    else if (identical(col, "SE")) keys <- c(keys, "SE")
    else if (identical(col, "SD")) keys <- c(keys, "SD")
    else if (identical(col, "p")) keys <- c(keys, "p")
    else if (identical(col, "pHolm")) keys <- c(keys, "pHolm")
    else if (identical(col, "pBonf")) keys <- c(keys, "pBonf")
    else if (identical(col, "pBH")) keys <- c(keys, "pBH")
    else if (identical(col, "pd")) keys <- c(keys, "pd")
    else if (identical(col, "ROPE%")) keys <- c(keys, "ROPE%")
    else if (identical(col, "ROPE")) keys <- c(keys, "ROPE")
    else if (identical(col, "log BF10")) keys <- c(keys, "log BF10")
    else if (identical(col, "KL")) keys <- c(keys, "KL")
    else if (identical(col, "SD\u2193") || identical(col, "SD↓")) keys <- c(keys, "SD\u2193")
    else if (grepl("% HDI$", col)) keys <- c(keys, "HDI")
  }
  unique(keys)
}

#' Build option cards for the columns currently shown in the coef table
#' @keywords internal
path_coefficients_help_options_for_columns <- function(columns) {
  catalog <- path_coefficients_column_help_catalog()
  keys <- path_coefficients_help_keys_for_columns(columns)
  cols <- as.character(columns %||% character())
  out <- lapply(keys, function(key) {
    entry <- catalog[[key]]
    if (is.null(entry)) return(NULL)
    label <- if (identical(key, "HDI")) {
      hit <- cols[grepl("% HDI$", cols)]
      if (length(hit)) hit[[1]] else "HDI"
    } else {
      key
    }
    list(
      label = label,
      summary = entry$summary %||% "",
      details = entry$details %||% character(),
      pros = entry$pros %||% character(),
      pros_title = entry$pros_title %||% "Benefits",
      cons = entry$cons %||% character(),
      cons_title = entry$cons_title %||% "Trade-offs"
    )
  })
  Filter(Negate(is.null), out)
}

#' Contextual help modal for the live path-coefficients table (overview)
#' @keywords internal
path_coefficients_help_modal <- function(columns = NULL, scope_note = NULL) {
  # Prefer the static overview topic; columns arg kept for back-compat
  topic <- smt_field_help_topics()$path_coefficients
  if (is.null(topic)) {
    return(smt_field_help_modal("path_coefficients"))
  }
  lead <- as.character(topic$lead %||% "")
  if (!is.null(scope_note) && nzchar(as.character(scope_note)[[1]])) {
    lead <- paste(lead, as.character(scope_note)[[1]])
  }
  topic$lead <- lead
  smt_field_help_modal_from_topic(topic, topic_id = "path_coefficients")
}

#' Help modal for one path-coefficient column (from a header click)
#' @keywords internal
path_coefficient_column_help_modal <- function(column) {
  column <- as.character(column %||% "")[[1]]
  if (!nzchar(column)) {
    return(path_coefficients_help_modal())
  }
  opts <- path_coefficients_help_options_for_columns(column)
  if (!length(opts)) {
    return(shiny::modalDialog(
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
          shiny::h3(column),
          shiny::p(
            class = "smt-help-lead",
            "No dedicated help is registered for this column yet."
          )
        )
      )
    ))
  }
  opt <- opts[[1]]
  topic <- list(
    title = opt$label %||% column,
    # Lead comes from the option summary in the single-indicator modal path.
    lead = "",
    options = list(opt),
    note = paste(
      "Click other column headers for their indicators.",
      "Use the section “?” for a short table overview."
    )
  )
  smt_field_help_modal_from_topic(topic, topic_id = paste0("coef_", column))
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

#' Optional figure block for a help option (`src` under www/)
#' @keywords internal
smt_help_option_figure <- function(figure) {
  if (is.null(figure) || !is.list(figure)) return(NULL)
  src <- as.character(figure$src %||% "")[[1]]
  if (!nzchar(src)) return(NULL)
  q <- tryCatch(smt_www_asset_query(src), error = function(e) "")
  shiny::div(
    class = "smt-help-figure",
    shiny::tags$img(
      src = paste0(src, q),
      alt = as.character(figure$alt %||% ""),
      class = "smt-help-figure-img"
    ),
    if (!is.null(figure$caption) && nzchar(as.character(figure$caption)[[1]])) {
      shiny::tags$p(
        class = "smt-help-figure-caption",
        as.character(figure$caption)[[1]]
      )
    }
  )
}

#' Render one help option (card when comparing choices; flat for a single indicator)
#' @keywords internal
smt_help_option_card <- function(opt, as_card = TRUE, show_label = TRUE, show_summary = TRUE) {
  pros <- opt$pros %||% character()
  cons <- opt$cons %||% character()
  details <- opt$details %||% character()
  pros_title <- as.character(opt$pros_title %||% "Benefits")[[1]]
  cons_title <- as.character(opt$cons_title %||% "Trade-offs")[[1]]
  label <- as.character(opt$label %||% "")[[1]]
  summary <- as.character(opt$summary %||% "")[[1]]
  shiny::div(
    class = if (isTRUE(as_card)) "smt-help-option" else "smt-help-option smt-help-option-flat",
    if (isTRUE(show_label) && nzchar(label)) {
      shiny::div(class = "smt-help-option-label", label)
    },
    if (isTRUE(show_summary) && nzchar(summary)) {
      shiny::p(class = "smt-help-option-summary", summary)
    },
    smt_help_option_figure(opt$figure),
    if (length(details)) {
      shiny::div(
        class = "smt-help-details",
        lapply(details, function(p) shiny::p(class = "smt-help-detail", p))
      )
    },
    if (length(pros)) {
      shiny::div(
        class = "smt-help-list smt-help-pros",
        shiny::div(class = "smt-help-list-title", pros_title),
        shiny::tags$ul(lapply(pros, shiny::tags$li))
      )
    },
    if (length(cons)) {
      shiny::div(
        class = "smt-help-list smt-help-cons",
        shiny::div(class = "smt-help-list-title", cons_title),
        shiny::tags$ul(lapply(cons, shiny::tags$li))
      )
    }
  )
}

#' Build a help modal from an already-resolved topic list
#' @keywords internal
smt_field_help_modal_from_topic <- function(topic, topic_id = "help") {
  if (is.null(topic)) {
    return(shiny::modalDialog(
      title = "Help",
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      shiny::p("No help is registered for this field yet.")
    ))
  }
  opts <- topic$options %||% list()
  single <- length(opts) <= 1L
  title <- as.character(topic$title %||% topic_id)[[1]]
  lead <- as.character(topic$lead %||% "")[[1]]
  # Single-indicator modals: prose under the hero, not a comparison card.
  if (single && length(opts) == 1L) {
    opt <- opts[[1]]
    opt_label <- as.character(opt$label %||% "")[[1]]
    opt_summary <- as.character(opt$summary %||% "")[[1]]
    # Prefer the option summary as the lead when topic lead is empty/generic
    # or when the option label matches the hero title (header-click help).
    label_matches_title <- nzchar(opt_label) && identical(opt_label, title)
    if (!nzchar(lead) && nzchar(opt_summary)) {
      lead <- opt_summary
    } else if (label_matches_title && nzchar(opt_summary)) {
      lead <- opt_summary
    }
    body <- smt_help_option_card(
      opt,
      as_card = FALSE,
      show_label = !label_matches_title && nzchar(opt_label),
      show_summary = !identical(opt_summary, lead)
    )
    opts_class <- "smt-help-options smt-help-options-single"
  } else {
    body <- lapply(opts, smt_help_option_card)
    opts_class <- "smt-help-options"
  }
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
        shiny::h3(title),
        if (nzchar(lead)) shiny::p(class = "smt-help-lead", lead)
      ),
      shiny::div(class = opts_class, body),
      if (!is.null(topic$note) && nzchar(topic$note)) {
        shiny::div(class = "smt-help-note", topic$note)
      }
    )
  )
}

#' Build the help modal for one topic
#' @keywords internal
smt_field_help_modal <- function(topic_id) {
  topic_id <- as.character(topic_id)[[1]]
  topics <- smt_field_help_topics()
  smt_field_help_modal_from_topic(topics[[topic_id]], topic_id = topic_id)
}

#' Register observers that open field-help modals
#'
#' @param skip Topic ids handled elsewhere (e.g. contextual path-coefficients help).
#' @keywords internal
smt_register_field_help <- function(input, session, skip = character()) {
  ids <- setdiff(names(smt_field_help_topics()), as.character(skip %||% character()))
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
