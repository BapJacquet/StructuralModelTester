repeated_demo_bundle <- function() {
  csv <- StructuralModelTester:::smt_sample_path("repeated_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("repeated_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "repeated_demo sample files missing")
  list(
    dat = utils::read.csv(csv, stringsAsFactors = FALSE),
    state = StructuralModelTester:::model_from_json(
      paste(readLines(js, warn = FALSE), collapse = "\n")
    )
  )
}

test_that("factor_slope_cell_ok requires median ≥2 trials per cluster×level", {
  # 2 trials per condition within each subject
  dat2 <- data.frame(
    subject = rep(c("s1", "s2"), each = 4L),
    Condition = rep(c("Control", "Control", "Treat", "Treat"), 2L),
    stringsAsFactors = FALSE
  )
  expect_true(StructuralModelTester:::factor_slope_cell_ok(
    dat2$Condition, dat2$subject, min_trials = 2
  ))

  # 1 trial per condition — not enough residual vs slope
  dat1 <- data.frame(
    subject = rep(c("s1", "s2"), each = 2L),
    Condition = rep(c("Control", "Treat"), 2L),
    stringsAsFactors = FALSE
  )
  expect_false(StructuralModelTester:::factor_slope_cell_ok(
    dat1$Condition, dat1$subject, min_trials = 2
  ))
})

test_that("lmer path fit stores standardized coefficients for link plots", {
  bun <- repeated_demo_bundle()
  res <- suppressWarnings(
    StructuralModelTester:::fit_pvalue_model(
      bun$state, bun$dat, mark_nonsig = FALSE
    )
  )
  e <- res$state$edges
  expect_true(all(is.finite(e$estimate)))
  expect_true(all(is.finite(e$estimate_std)))
  bstd <- StructuralModelTester:::standardized_term_coefs(res$fit$Score)
  expect_true(is.finite(bstd[["Age"]]))
  expect_true(is.finite(bstd[["Condition__Treat"]]))
  # Subtitle path used by edge plots
  sub <- StructuralModelTester:::edge_plot_subtitle(
    e$estimate[[1]], e$estimate_std[[1]], "confirmatory", NA_character_,
    show_fit = TRUE, rel_kind = "linear"
  )
  expect_match(sub, "β =")
})

test_that("dv_random_slope_terms picks Intra Condition with multi-trial cells", {
  bun <- repeated_demo_bundle()
  st <- bun$state
  dat <- bun$dat
  preds <- st$edges[st$edges$to == "Score" & st$edges$type != "measurement", , drop = FALSE]
  terms <- StructuralModelTester:::dv_random_slope_terms(
    st, "Score", dat, "subject", preds
  )
  expect_identical(terms, "Condition")

  # Collapse to 1 trial/cell → no eligible slopes
  keep <- !duplicated(paste(dat$subject, dat$Condition, sep = "\r"))
  dat1 <- dat[keep, , drop = FALSE]
  terms1 <- StructuralModelTester:::dv_random_slope_terms(
    st, "Score", dat1, "subject", preds
  )
  expect_identical(terms1, character())
})

test_that("repeated_demo fits (1 + Condition | subject) when rm_slopes on", {
  skip_if_not_installed("lmerTest")
  bun <- repeated_demo_bundle()
  st <- bun$state
  expect_true(isTRUE(StructuralModelTester:::node_repeated_measures(st, "Score")$slopes))

  res <- StructuralModelTester:::fit_pvalue_model(
    st, bun$dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  expect_identical(res$engine, "path")
  fit <- res$fit[["Score"]]
  expect_true(inherits(fit, "merMod"))
  fml <- paste(deparse(stats::formula(fit)), collapse = "")
  expect_match(fml, "1 \\+ Condition\\s*\\|\\s*subject", perl = TRUE)

  re <- StructuralModelTester:::mer_re_tag(fit)
  expect_match(re, "Condition")
  expect_true(StructuralModelTester:::mer_has_random_slopes(fit))
})

test_that("rm_re_term builds intercept-only or slope formulas", {
  expect_identical(
    StructuralModelTester:::rm_re_term("subject"),
    "(1|subject)"
  )
  expect_identical(
    StructuralModelTester:::rm_re_term("subject", "Condition"),
    "(1 + Condition|subject)"
  )
  expect_identical(
    StructuralModelTester:::rm_re_term("subject", c("A", "B")),
    "(1 + A + B|subject)"
  )
})

test_that("rm_slopes clears when repeated measures disabled", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "Y", kind = "observed")
  st <- StructuralModelTester:::set_node_repeated_measures(
    st, "Y", enabled = TRUE, cluster_column = "subject", slopes = TRUE
  )
  expect_true(StructuralModelTester:::node_repeated_measures(st, "Y")$slopes)
  st <- StructuralModelTester:::set_node_repeated_measures(st, "Y", enabled = FALSE)
  rm <- StructuralModelTester:::node_repeated_measures(st, "Y")
  expect_false(rm$enabled)
  expect_false(rm$slopes)
})

test_that("brms_prepare_path_input rewrites Var__Level dummies", {
  prep <- StructuralModelTester:::brms_prepare_path_input(
    data = data.frame(
      Score = 1:4,
      Age = 1:4,
      Condition__Treat = c(0, 1, 0, 1),
      subject = c("a", "a", "b", "b"),
      Condition = factor(c("Control", "Treat", "Control", "Treat")),
      stringsAsFactors = FALSE
    ),
    yname = "Score",
    rhs_terms = c("Age", "Condition__Treat"),
    slope_terms = "Condition",
    cluster = "subject",
    term_meta = list(
      Condition__Treat = list(rhs = "Condition", level = "Treat")
    )
  )
  expect_false(any(grepl("__", names(prep$data), fixed = TRUE)))
  expect_identical(prep$rhs_terms, c("Age", "Condition_Treat"))
  expect_true("Condition_Treat" %in% names(prep$data))
  expect_identical(prep$from_safe[["Condition_Treat"]], "Condition__Treat")
  expect_identical(names(prep$term_meta), "Condition_Treat")
})

test_that("brms repeated_demo accepts Condition__Treat via safe rename", {
  skip_if_not(
    StructuralModelTester:::bayes_backend_available(),
    "brms + Stan backend not available"
  )
  bun <- repeated_demo_bundle()
  res <- StructuralModelTester:::fit_bayes_model(
    bun$state, bun$dat, structure = "independent",
    mark_nonsig = FALSE,
    chains = 1L, iter = 400L, warmup = 200L, seed = 3L
  )
  expect_identical(res$sampler, "brms")
  fit <- res$fit[["Score"]]
  expect_true(inherits(fit, "brmsfit"))
  cf <- StructuralModelTester:::path_fixed_coefs(fit)
  expect_true("Condition__Treat" %in% names(cf))
  expect_false(any(grepl("Variable names may not contain double underscores",
                         res$syntax %||% "", fixed = TRUE)))
  est <- res$estimates
  expect_true(any(est$rhs == "Condition" & est$level == "Treat"))
  # Standardized β on edges / plots (brms design-matrix path)
  expect_true(all(is.finite(res$state$edges$estimate_std)))
  bstd <- StructuralModelTester:::standardized_term_coefs(fit)
  expect_true(is.finite(bstd[["Age"]]))
  expect_true(is.finite(bstd[["Condition__Treat"]]))

  # brmsfit has no terms component — matrix names must unwrap brmsformula
  mm <- StructuralModelTester:::path_model_matrix_names(fit)
  expect_true(any(grepl("^Condition", mm)))
  expect_true("Age" %in% mm)
  # brms-safe dummy (Condition_Treat) must be found for co-predictor holds
  cond_cols <- StructuralModelTester:::predictor_fit_columns(
    bun$state, fit, "Condition"
  )
  expect_true(any(grepl("^Condition", cond_cols)))
  hs <- StructuralModelTester:::emmeans_hold_specs(
    bun$state, bun$dat, fit, "Condition"
  )
  expect_true(length(hs$at) >= 1L)
  expect_true(any(grepl("Condition", hs$labels, fixed = TRUE)))

  # Condition contrast link: emmeans overlay (not just raw boxplot)
  eid <- bun$state$edges$id[
    bun$state$edges$from == "Condition" & bun$state$edges$to == "Score"
  ][[1]]
  p <- StructuralModelTester:::edge_relationship_plot(
    res$state, bun$dat, eid, show_fit = TRUE, fit = fit
  )
  expect_true(inherits(p, "ggplot"))
  expect_match(p$labels$title %||% "", "Condition")
  texts <- smt_ggplot_text_labels(p)
  expect_false(any(texts == "Plot error"))
  expect_false(any(grepl("terms", texts, ignore.case = TRUE)))
  geoms <- vapply(p$layers, function(ly) class(ly$geom)[[1]], character(1))
  expect_true(any(geoms == "GeomErrorbar"))

  # Age link must hold Condition at reference under Bayes
  eid_age <- bun$state$edges$id[
    bun$state$edges$from == "Age" & bun$state$edges$to == "Score"
  ][[1]]
  p_age <- StructuralModelTester:::edge_relationship_plot(
    res$state, bun$dat, eid_age, show_fit = TRUE, fit = fit
  )
  expect_match(p_age$labels$caption %||% "", "Condition")
})

test_that("search-excluded Age link shows clear message instead of a fake fit", {
  bun <- repeated_demo_bundle()
  res <- suppressWarnings(
    StructuralModelTester:::fit_aic_bic_search(
      bun$state, bun$dat, criterion = "bic"
    )
  )
  fit <- StructuralModelTester:::edge_fit_for_plot(res, "Score")
  expect_false(is.null(fit))
  # Best BIC model drops Age
  expect_false("Age" %in% StructuralModelTester:::path_model_matrix_names(fit))
  eid_age <- bun$state$edges$id[
    bun$state$edges$from == "Age" & bun$state$edges$to == "Score"
  ][[1]]
  p_age <- StructuralModelTester:::edge_relationship_plot(
    res$state, bun$dat, eid_age, show_fit = TRUE, fit = fit
  )
  texts <- smt_ggplot_text_labels(p_age)
  expect_true(any(texts == "Not in selected model"))
  eid_cond <- bun$state$edges$id[
    bun$state$edges$from == "Condition" & bun$state$edges$to == "Score"
  ][[1]]
  p_cond <- StructuralModelTester:::edge_relationship_plot(
    res$state, bun$dat, eid_cond, show_fit = TRUE, fit = fit
  )
  expect_match(p_cond$labels$title %||% "", "Condition")
  expect_false(any(smt_ggplot_text_labels(p_cond) == "Not in selected model"))
})
