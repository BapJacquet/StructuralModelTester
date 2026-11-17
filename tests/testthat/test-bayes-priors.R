test_that("edge priors map onto linear and polynomial terms", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "X", observed_column = "X")
  st <- StructuralModelTester:::add_node(st, "Y", observed_column = "Y")
  xid <- st$nodes$id[[1]]
  yid <- st$nodes$id[[2]]
  st <- StructuralModelTester:::add_edge(st, xid, yid, type = "confirmatory")
  eid <- st$edges$id[[1]]
  st <- StructuralModelTester:::set_edge_prior(
    st, eid,
    mean = 0.4, sd = 0.2,
    mean2 = -0.1, sd2 = 0.05,
    mean3 = 0.01, sd3 = 0.02
  )
  st <- StructuralModelTester:::set_edge_rel_kind(st, eid, "cubic")
  dat <- data.frame(X = 1:8, Y = 1:8)
  built <- StructuralModelTester:::build_path_terms(st, dat, st$edges)

  p1 <- StructuralModelTester:::edge_normal_prior(st, st$edges, built$term_meta[["X"]])
  p2 <- StructuralModelTester:::edge_normal_prior(st, st$edges, built$term_meta[["X_p2"]])
  p3 <- StructuralModelTester:::edge_normal_prior(st, st$edges, built$term_meta[["X_p3"]])
  expect_equal(p1$mean, 0.4)
  expect_equal(p1$sd, 0.2)
  expect_equal(p2$mean, -0.1)
  expect_equal(p2$sd, 0.05)
  expect_equal(p3$mean, 0.01)
  expect_equal(p3$sd, 0.02)

  prior_df <- data.frame(
    prior = c("(flat)", "(flat)", "(flat)", "(flat)"),
    class = c("b", "b", "b", "Intercept"),
    coef = c("", "X", "X_p2", ""),
    source = c("default", "default", "default", "default"),
    stringsAsFactors = FALSE
  )
  filled <- StructuralModelTester:::fill_brms_slope_priors(
    prior_df, st, st$edges, built$term_meta
  )
  expect_equal(filled$prior[filled$coef == "X"], "normal(0.4, 0.2)")
  expect_equal(filled$prior[filled$coef == "X_p2"], "normal(-0.1, 0.05)")
  expect_false(any(filled$class == "b" & !nzchar(filled$coef)))
  expect_equal(filled$prior[filled$class == "Intercept"], "(flat)")
})

test_that("HDI, pd, ROPE, and log BF10 replace p on the canvas and in the table", {
  hdi <- unname(StructuralModelTester:::hdi_of(c(1, 2, 3, 4, 5), ci = 0.6))
  expect_equal(hdi, c(1, 3))
  expect_lt(diff(hdi), 3)
  expect_gt(StructuralModelTester:::pd_of(c(1, 2, 3, -0.1)), 0.5)
  expect_equal(
    StructuralModelTester:::rope_hdi_decision(c(0.2, 0.5), c(-0.1, 0.1)),
    "reject"
  )
  expect_equal(
    StructuralModelTester:::rope_hdi_decision(c(-0.05, 0.05), c(-0.1, 0.1)),
    "accept"
  )
  expect_equal(
    StructuralModelTester:::rope_hdi_decision(c(-0.05, 0.2), c(-0.1, 0.1)),
    "undecided"
  )
  # log BF10 vs θ=0 under N(0,1): mass away from 0 → log BF10 > 0
  set.seed(2)
  bf_out <- StructuralModelTester:::log_bf10_point_null_of(
    rnorm(4000, mean = 0.5, sd = 0.1),
    prior_mean = 0, prior_sd = 1
  )
  expect_gt(bf_out, 0)
  bf_in <- StructuralModelTester:::log_bf10_point_null_of(
    rnorm(4000, mean = 0, sd = 0.05),
    prior_mean = 0, prior_sd = 1
  )
  expect_lt(bf_in, 0)
  # KL: concentrated away from a wide prior → information gain > 0
  kl_gain <- StructuralModelTester:::kl_prior_post_of(
    rnorm(4000, mean = 0.5, sd = 0.1),
    prior_mean = 0, prior_sd = 1
  )
  expect_gt(kl_gain, 0)
  kl_same <- StructuralModelTester:::kl_prior_post_of(
    rnorm(4000, mean = 0, sd = 1),
    prior_mean = 0, prior_sd = 1
  )
  expect_lt(kl_same, 0.2)

  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "X", observed_column = "X")
  st <- StructuralModelTester:::add_node(st, "Y", observed_column = "Y")
  st <- StructuralModelTester:::add_edge(
    st, st$nodes$id[[1]], st$nodes$id[[2]], type = "confirmatory"
  )
  est <- StructuralModelTester:::estimate_row(
    "Y", "~", "X", 0.4, 0.05, NA_real_,
    ci_low = 0.2, ci_high = 0.6, ci_level = 0.95,
    pd = 0.99, rope_low = -0.1, rope_high = 0.1,
    rope_pct = 0, rope_decision = "reject",
    log_bf10 = log(12.5), kl_prior_post = 1.25
  )
  st2 <- StructuralModelTester:::apply_estimates_to_edges(st, est)
  expect_false(grepl("p=", st2$edges$label[[1]], fixed = TRUE))
  expect_match(st2$edges$label[[1]], "\\[0.20, 0.60\\]")
  expect_match(st2$edges$label[[1]], "pd=99%")
  expect_false(isTRUE(st2$edges$includes_zero[[1]]))

  wide <- StructuralModelTester:::estimate_row(
    "Y", "~", "X", 0.05, 0.1, NA_real_,
    ci_low = -0.1, ci_high = 0.2, ci_level = 0.95,
    pd = 0.7, rope_low = -0.1, rope_high = 0.1,
    rope_pct = 0.4, rope_decision = "undecided"
  )
  st3 <- StructuralModelTester:::apply_estimates_to_edges(st, wide)
  expect_true(isTRUE(st3$edges$includes_zero[[1]]))
  expect_identical(
    StructuralModelTester:::edge_color(st3$edges[1, ], alpha = NULL),
    "#B0B0B0"
  )

  tab <- StructuralModelTester:::estimates_table(st2, est)
  expect_true("95% HDI" %in% names(tab))
  expect_true("SD" %in% names(tab))
  expect_true("pd" %in% names(tab))
  expect_true("log BF10" %in% names(tab))
  expect_true("KL" %in% names(tab))
  expect_false("p" %in% names(tab))
  expect_equal(tab[["95% HDI"]][[1]], "[0.200, 0.600]")
  expect_equal(tab[["log BF10"]][[1]], sprintf("%+.2f", log(12.5)))
  expect_equal(tab[["KL"]][[1]], "1.25")
})

test_that("brms fit uses the link prior and HDI + ROPE", {
  skip_if_not(
    StructuralModelTester:::bayes_backend_available(),
    "brms + Stan backend not available"
  )
  set.seed(1)
  n <- 40L
  x <- rnorm(n)
  y <- 0.2 * x + rnorm(n)
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "X", observed_column = "X")
  st <- StructuralModelTester:::add_node(st, "Y", observed_column = "Y")
  st <- StructuralModelTester:::add_edge(
    st, st$nodes$id[[1]], st$nodes$id[[2]], type = "confirmatory"
  )
  # Mildly informative prior centered near 0 so Savage–Dickey BF10 is defined
  st <- StructuralModelTester:::set_edge_prior(
    st, st$edges$id[[1]], mean = 0.3, sd = 0.5
  )
  res <- StructuralModelTester:::fit_bayes_model(
    st,
    data.frame(X = x, Y = y),
    ci = 0.95,
    rope_k = 0.1,
    mark_nonsig = FALSE,
    chains = 1L,
    iter = 400L,
    warmup = 200L,
    seed = 1L
  )
  expect_identical(res$sampler, "brms")
  expect_identical(res$pvalue_kind, "hdi_rope")
  expect_match(res$syntax, "normal\\(0\\.3, 0\\.5\\)")
  row <- res$estimates[res$estimates$rhs == "X" & res$estimates$kind == "effect", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_true(is.finite(row$est))
  expect_true(is.finite(row$ci_low) && is.finite(row$ci_high))
  expect_true(is.finite(row$pd) && row$pd > 0.5)
  expect_true(as.character(row$rope_decision[[1]]) %in% c("reject", "undecided", "accept"))
  expect_true(is.finite(row$log_bf10) || is.infinite(row$log_bf10))
  expect_true(is.finite(row$kl_prior_post) && row$kl_prior_post >= 0)
  expect_true(is.na(row$pvalue))
  expect_false(grepl("p=", res$state$edges$label[[1]], fixed = TRUE))
  expect_match(res$state$edges$label[[1]], "\\[")

  info <- StructuralModelTester:::edge_bayes_param_info(
    res, st, st$edges$id[[1]], degree = 1L
  )
  expect_false(is.null(info))
  expect_gt(length(info$draws), 10L)
  expect_true(is.finite(info$summary$log_bf10) || is.infinite(info$summary$log_bf10))
  expect_true(is.finite(info$summary$kl_prior_post) && info$summary$kl_prior_post >= 0)
  p_eff <- ggplot2::ggplot() + ggplot2::theme_void()
  combo <- StructuralModelTester:::compose_edge_bayes_plot(p_eff, info)
  expect_true(inherits(combo, "patchwork") || inherits(combo, "ggplot"))
})

test_that("joint brms structure fits a multivariate gaussian path", {
  skip_if_not(
    StructuralModelTester:::bayes_backend_available(),
    "brms + Stan backend not available"
  )
  set.seed(3)
  n <- 50L
  x <- rnorm(n)
  y1 <- 0.4 * x + rnorm(n)
  y2 <- 0.3 * x + 0.5 * y1 + rnorm(n)
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "X", observed_column = "X")
  st <- StructuralModelTester:::add_node(st, "Y1", observed_column = "Y1")
  st <- StructuralModelTester:::add_node(st, "Y2", observed_column = "Y2")
  st <- StructuralModelTester:::add_edge(
    st, st$nodes$id[[1]], st$nodes$id[[2]], type = "confirmatory"
  )
  st <- StructuralModelTester:::add_edge(
    st, st$nodes$id[[1]], st$nodes$id[[3]], type = "confirmatory"
  )
  st <- StructuralModelTester:::add_edge(
    st, st$nodes$id[[2]], st$nodes$id[[3]], type = "confirmatory"
  )
  res <- StructuralModelTester:::fit_bayes_model(
    st,
    data.frame(X = x, Y1 = y1, Y2 = y2),
    structure = "joint",
    mark_nonsig = FALSE,
    chains = 1L,
    iter = 400L,
    warmup = 200L,
    seed = 3L
  )
  expect_identical(res$bayes_structure, "joint")
  expect_match(res$syntax, "joint")
  expect_true(any(res$estimates$rhs == "X" & res$estimates$lhs %in% c("Y1", "Y2")))
  # Same joint fit object is reused per equation key
  expect_true(inherits(res$fit[[st$nodes$id[[2]]]], "brmsfit"))
  expect_identical(
    res$fit[[st$nodes$id[[2]]]]$smt_response,
    "Y1"
  )
  expect_identical(
    res$fit[[st$nodes$id[[3]]]]$smt_response,
    "Y2"
  )
})

test_that("joint mode falls back when a binary outcome is present", {
  jobs <- list(
    list(yname = "Y", y_kind = "binary", slope_terms = character(), cluster = NULL),
    list(yname = "Z", y_kind = "continuous", slope_terms = character(), cluster = NULL)
  )
  expect_match(
    StructuralModelTester:::joint_brms_block_reason(jobs),
    "continuous"
  )
})
