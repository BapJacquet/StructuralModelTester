path_demo_bundle <- function() {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  list(
    dat = utils::read.csv(csv),
    state = StructuralModelTester:::model_from_json(
      paste(readLines(js, warn = FALSE), collapse = "\n")
    )
  )
}

test_that("emmeans conditional curve works for path_demo M→Y and X→Y", {
  skip_if_not_installed("emmeans")
  bun <- path_demo_bundle()
  dat <- bun$dat
  st <- bun$state
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  expect_identical(res$engine, "path")

  for (pair in list(c("M", "Y"), c("X", "Y"))) {
    eid <- st$edges$id[st$edges$from == pair[[1]] & st$edges$to == pair[[2]]][[1]]
    fit <- StructuralModelTester:::edge_fit_for_plot(res, pair[[2]])
    expect_false(is.null(fit))

    hold_ids <- StructuralModelTester:::co_predictor_ids_to_hold(
      st, pair[[1]], pair[[2]]
    )
    expect_true(length(hold_ids) >= 1L)
    hs <- StructuralModelTester:::emmeans_hold_specs(st, dat, fit, hold_ids)
    expect_true(length(hs$at) >= 1L)
    expect_true(length(hs$labels) >= 1L)

    curve <- StructuralModelTester:::emmeans_edge_curve(
      fit,
      focal = pair[[1]],
      hold_at = hs$at,
      x_grid = seq(-1, 1, length.out = 11)
    )
    expect_false(is.null(curve))
    expect_true(nrow(curve) >= 5L)
    expect_true(all(is.finite(curve$y)))

    p <- StructuralModelTester:::edge_relationship_plot(
      st, dat, eid,
      show_fit = TRUE,
      fit = fit,
      points_mode = "observed"
    )
    expect_s3_class(p, "ggplot")
    expect_false(identical(p$labels$title, "No data at covariate reference"))
    expect_match(p$labels$title %||% "", "Conditional effect")
  }
})

test_that("partial residuals return finite x/y for M in Y equation", {
  skip_if_not_installed("emmeans")
  bun <- path_demo_bundle()
  dat <- bun$dat
  st <- bun$state
  res <- StructuralModelTester:::fit_pvalue_model(st, dat)
  fit <- StructuralModelTester:::edge_fit_for_plot(res, "Y")
  pr <- StructuralModelTester:::partial_residual_xy(fit, "M")
  expect_false(is.null(pr))
  expect_true(nrow(pr) > 50L)
  expect_true(all(is.finite(pr$x) & is.finite(pr$y)))

  eid <- st$edges$id[st$edges$from == "M" & st$edges$to == "Y"][[1]]
  p <- StructuralModelTester:::edge_relationship_plot(
    st, dat, eid,
    show_fit = TRUE,
    fit = fit,
    points_mode = "partial"
  )
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption %||% "", "partial residuals")
})

test_that("points_mode none still draws an emmeans curve", {
  skip_if_not_installed("emmeans")
  bun <- path_demo_bundle()
  dat <- bun$dat
  st <- bun$state
  res <- StructuralModelTester:::fit_pvalue_model(st, dat)
  eid <- st$edges$id[st$edges$from == "M" & st$edges$to == "Y"][[1]]
  p <- StructuralModelTester:::edge_relationship_plot(
    st, dat, eid,
    show_fit = TRUE,
    fit = StructuralModelTester:::edge_fit_for_plot(res, "Y"),
    points_mode = "none"
  )
  expect_s3_class(p, "ggplot")
  expect_false(identical(p$labels$title, "No data at covariate reference"))
})
