test_that("ordered_quad example uses qualitative Stage levels + quadratic", {
  csv <- StructuralModelTester:::smt_sample_path("ordered_quad_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("ordered_quad_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "ordered_quad sample files missing")

  ex <- StructuralModelTester:::smt_example_resolved("ordered_quad")
  expect_true(!is.null(ex))

  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_true(all(c("Stage", "Y") %in% names(dat)))
  expect_identical(
    sort(unique(as.character(dat$Stage))),
    sort(c("None", "Low", "Moderate", "High"))
  )

  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  expect_true("quadratic" %in% st$edges$rel_kind)
  coding <- StructuralModelTester:::node_factor_coding(st, "Stage")
  expect_true(isTRUE(coding$ordered))
  expect_identical(
    coding$levels,
    c("None", "Low", "Moderate", "High")
  )
  # Run / Simulate sync must not alphabetize declared ordinal levels
  st2 <- StructuralModelTester:::sync_sim_kinds_from_data(st, dat)
  coding2 <- StructuralModelTester:::node_factor_coding(st2, "Stage")
  expect_identical(
    coding2$levels,
    c("None", "Low", "Moderate", "High")
  )
  expect_identical(
    StructuralModelTester:::node_sim_factor_levels(st2, "Stage", data = dat),
    c("None", "Low", "Moderate", "High")
  )

  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  est <- res$estimates
  expect_true(any(est$op == "~" & est$rhs == "Stage"))
  has_quad <- any(
    est$op == "~" & est$rhs == "Stage" &
      vapply(est$level, StructuralModelTester:::is_poly_level, logical(1))
  )
  expect_true(has_quad)
})

test_that("ordered poly fit recovers sim priors (same score order)", {
  set.seed(2)
  levs <- c("None", "Low", "Moderate", "High")
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st, "Stage", kind = "observed", observed_column = "Stage"
  )
  st <- StructuralModelTester:::add_node(
    st, "Y", kind = "observed", observed_column = "Y"
  )
  st <- StructuralModelTester:::set_node_sim_var_kind(st, "Stage", "categorical")
  st <- StructuralModelTester:::set_node_factor_coding(
    st, "Stage", base = "None", ordered = TRUE, levels = levs, data = NULL
  )
  st <- StructuralModelTester:::set_node_sim_intercept(st, "Y", intercept = 10)
  st <- StructuralModelTester:::set_node_sim_resid_sd(st, "Y", resid_sd = 0.05)
  st <- StructuralModelTester:::add_edge(st, "Stage", "Y", type = "confirmatory")
  eid <- st$edges$id[[1]]
  st <- StructuralModelTester:::set_edge_rel_kind(st, eid, "quadratic")
  st <- StructuralModelTester:::set_edge_prior(st, eid, mean = 0.5, mean2 = -0.4)

  dat <- StructuralModelTester:::simulate_path_data(st, n = 200L, seed = 2L)
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = FALSE, conf_adjust = "none"
  )
  fit <- StructuralModelTester:::edge_fit_for_plot(res, "Y")
  cf <- stats::coef(fit)
  expect_equal(unname(cf[["Stage"]]), 0.5, tolerance = 0.08)
  expect_equal(unname(cf[["Stage_p2"]]), -0.4, tolerance = 0.08)

  curve <- StructuralModelTester:::poly_edge_curve_data(
    fit, "Stage", x_grid = 1:4
  )
  # Arch: middle scores higher than ends when b2 < 0 and b1 modest
  expect_gt(curve$y[[2]], curve$y[[1]])
  expect_gt(curve$y[[3]], curve$y[[4]])
})

test_that("ordered categorical + quadratic uses integer scores not dummies", {
  set.seed(1)
  levs <- c("None", "Low", "Moderate", "High")
  dat <- data.frame(
    Stage = factor(rep(levs, each = 20), levels = levs, ordered = TRUE),
    Y = rnorm(80),
    stringsAsFactors = FALSE
  )
  dat$Y <- as.numeric(dat$Stage) + 0.4 * (as.numeric(dat$Stage) - 2.5)^2 +
    dat$Y * 0.3
  dat$Stage <- as.character(dat$Stage)

  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st, "Stage", kind = "observed", observed_column = "Stage"
  )
  st <- StructuralModelTester:::add_node(
    st, "Y", kind = "observed", observed_column = "Y"
  )
  st <- StructuralModelTester:::set_node_sim_var_kind(st, "Stage", "categorical")
  st <- StructuralModelTester:::set_node_factor_coding(
    st, "Stage", base = "None", ordered = TRUE, levels = levs, data = dat
  )
  st <- StructuralModelTester:::add_edge(st, "Stage", "Y", type = "confirmatory")
  st <- StructuralModelTester:::set_edge_rel_kind(st, st$edges$id[[1]], "quadratic")

  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = FALSE, conf_adjust = "none"
  )
  est <- res$estimates
  # Poly path: Stage + Stage^2 — no dummy columns like Stage__Low
  expect_false(any(grepl("__", est$rhs, fixed = TRUE)))
  expect_true(any(est$rhs == "Stage" & vapply(est$level, StructuralModelTester:::is_poly_level, logical(1))))
})

test_that("ordered_level_scores follow levels_order not base-first", {
  x <- c("None", "Low", "Moderate", "High")
  # Base High must not become score 1 when order is None→High
  sc <- StructuralModelTester:::ordered_level_scores(
    x,
    levels_order = c("None", "Low", "Moderate", "High"),
    base = "High"
  )
  expect_equal(
    as.integer(sc),
    c(1L, 2L, 3L, 4L)
  )
})

test_that("pick_plot_data prefers sim when prefer_simulated", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st, "X", kind = "observed", observed_column = "X"
  )
  st <- StructuralModelTester:::add_node(
    st, "Y", kind = "observed", observed_column = "Y"
  )
  analysis <- data.frame(X = 1:3, Y = 10:12)
  sim <- data.frame(X = c(0, 0, 0), Y = c(99, 99, 99))
  picked_default <- StructuralModelTester:::pick_plot_data_for_nodes(
    st, analysis, sim, c("X", "Y")
  )
  expect_false(isTRUE(picked_default$simulated))
  expect_equal(picked_default$data$Y[[1]], 10)
  picked_sim <- StructuralModelTester:::pick_plot_data_for_nodes(
    st, analysis, sim, c("X", "Y"), prefer_simulated = TRUE
  )
  expect_true(isTRUE(picked_sim$simulated))
  expect_equal(picked_sim$data$Y[[1]], 99)
})

test_that("ordered quadratic edge plot keeps boxplots and a curved fit", {
  csv <- StructuralModelTester:::smt_sample_path("ordered_quad_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("ordered_quad_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "ordered_quad sample files missing")

  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = FALSE, conf_adjust = "none"
  )
  fit <- StructuralModelTester:::edge_fit_for_plot(res, "Y")
  expect_false(is.null(fit))

  curve <- StructuralModelTester:::poly_edge_curve_data(
    fit, "Stage", x_grid = seq(1, 4, length.out = 21)
  )
  expect_false(is.null(curve))
  d2 <- diff(diff(curve$y))
  expect_gt(max(abs(d2)), 1e-6)

  pr <- StructuralModelTester:::edge_prior(st, st$edges$id[[1]])
  p <- StructuralModelTester:::edge_relationship_plot(
    st, dat, st$edges$id[[1]],
    show_fit = TRUE, rel_kind = "quadratic",
    prior_mean = pr$mean, prior_sd = pr$sd,
    prior_mean2 = pr$mean2, prior_mean3 = pr$mean3,
    fit = fit, points_mode = "observed"
  )
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1]], character(1))
  expect_true(any(grepl("Boxplot", geoms)))
  expect_true(any(grepl("Line", geoms)))
})
