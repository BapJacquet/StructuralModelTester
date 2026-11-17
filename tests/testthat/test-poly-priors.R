test_that("edge_prior returns degree-2 fields after set_edge_prior", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st, "X", kind = "observed", observed_column = "X"
  )
  st <- StructuralModelTester:::add_node(
    st, "Y", kind = "observed", observed_column = "Y"
  )
  st <- StructuralModelTester:::add_edge(st, "X", "Y", type = "confirmatory")
  eid <- st$edges$id[[1]]

  pr0 <- StructuralModelTester:::edge_prior(st, eid)
  expect_equal(pr0$mean2, 0)
  expect_equal(pr0$sd2, 1)
  expect_equal(pr0$mean3, 0)
  expect_equal(pr0$sd3, 1)

  st <- StructuralModelTester:::set_edge_prior(
    st, eid,
    mean = 0.5, sd = 0.2,
    mean2 = -0.15, sd2 = 0.4,
    mean3 = 0.01, sd3 = 0.5
  )
  pr <- StructuralModelTester:::edge_prior(st, eid)
  expect_equal(pr$mean, 0.5)
  expect_equal(pr$sd, 0.2)
  expect_equal(pr$mean2, -0.15)
  expect_equal(pr$sd2, 0.4)
  expect_equal(pr$mean3, 0.01)
  expect_equal(pr$sd3, 0.5)
})

test_that("JSON round-trip keeps prior_mean2", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st, "X", kind = "observed", observed_column = "X"
  )
  st <- StructuralModelTester:::add_node(
    st, "Y", kind = "observed", observed_column = "Y"
  )
  st <- StructuralModelTester:::add_edge(st, "X", "Y", type = "confirmatory")
  eid <- st$edges$id[[1]]
  st <- StructuralModelTester:::set_edge_rel_kind(st, eid, "quadratic")
  st <- StructuralModelTester:::set_edge_prior(
    st, eid, mean = 0.4, mean2 = -0.15, sd2 = 0.8
  )

  js <- StructuralModelTester:::model_to_json(st)
  st2 <- StructuralModelTester:::model_from_json(js)
  expect_true("prior_mean2" %in% names(st2$edges))
  expect_equal(st2$edges$prior_mean2[[1]], -0.15)
  expect_equal(st2$edges$prior_sd2[[1]], 0.8)
  expect_equal(st2$edges$prior_mean[[1]], 0.4)
  pr <- StructuralModelTester:::edge_prior(st2, eid)
  expect_equal(pr$mean2, -0.15)
})

test_that("prior poly curve uses DV sim_intercept not mean(y)", {
  x <- c(1, 2, 3, 4)
  y <- c(20, 21, 19, 22)
  pl0 <- StructuralModelTester:::edge_prior_line_data(
    x, y, prior_mean = 0.4, prior_sd = 1,
    prior_mean2 = -0.15, degree = 2L, intercept = 0
  )
  pl_i <- StructuralModelTester:::edge_prior_line_data(
    x, y, prior_mean = 0.4, prior_sd = 1,
    prior_mean2 = -0.15, degree = 2L, intercept = 17.3
  )
  expect_false(is.null(pl0))
  expect_false(is.null(pl_i))
  # intercept shifts the whole curve
  expect_equal(pl_i$y_plot - pl0$y_plot, rep(17.3, nrow(pl_i)), tolerance = 1e-9)
  # generative form: a + b1*x + b2*(x-mean(x))^2 at x=mean
  mx <- mean(x)
  expect_equal(
    pl_i$y_plot[which.min(abs(pl_i$x_plot - mx))],
    17.3 + 0.4 * mx,
    tolerance = 1e-6
  )
})

test_that("simulate quadratic edge applies linear and squared priors", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st, "X", kind = "observed", observed_column = "X"
  )
  st <- StructuralModelTester:::add_node(
    st, "Y", kind = "observed", observed_column = "Y"
  )
  st <- StructuralModelTester:::set_node_sim_var_kind(st, "X", "continuous")
  st <- StructuralModelTester:::set_node_sim_var_kind(st, "Y", "continuous")
  st <- StructuralModelTester:::set_node_sim_exogenous(st, "X", mean = 0, sd = 1)
  st <- StructuralModelTester:::set_node_sim_intercept(st, "Y", intercept = 0)
  st <- StructuralModelTester:::set_node_sim_resid_sd(st, "Y", resid_sd = 1e-6)
  st <- StructuralModelTester:::add_edge(st, "X", "Y", type = "confirmatory")
  eid <- st$edges$id[[1]]
  st <- StructuralModelTester:::set_edge_rel_kind(st, eid, "quadratic")
  st <- StructuralModelTester:::set_edge_prior(
    st, eid, mean = 2, mean2 = 3
  )

  dat <- StructuralModelTester:::simulate_path_data(st, n = 80L, seed = 42L)
  expect_true(all(c("X", "Y") %in% names(dat)))
  xc <- dat$X - mean(dat$X)
  fit <- stats::lm(Y ~ X + I(xc^2), data = dat)
  coefs <- stats::coef(fit)
  expect_equal(unname(coefs[["X"]]), 2, tolerance = 0.05)
  expect_equal(unname(coefs[["I(xc^2)"]]), 3, tolerance = 0.05)
})
