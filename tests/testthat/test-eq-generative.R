test_that("negative coefficients join with minus not plus-minus", {
  expect_equal(
    StructuralModelTester:::join_eta_tex_parts(c("1.00", "-0.15 \\cdot X")),
    "1.00 - 0.15 \\cdot X"
  )
  expect_equal(
    StructuralModelTester:::join_eta_tex_parts(c("1.00", "0.20 \\cdot X")),
    "1.00 + 0.20 \\cdot X"
  )
  w <- list(
    lhs = "Y", wrap_left = "", wrap_right = "", generative = FALSE, sigma = NA_real_
  )
  tex <- StructuralModelTester:::smt_eq_outcome_tex(
    w, c("1.00", "-0.15^{\\pm 0.05} \\cdot X")
  )
  expect_match(tex, "&\\\\quad - 0\\.15", perl = TRUE)
  expect_false(grepl("\\+ -", tex, fixed = TRUE))
})

test_that("lm equation wrapper is generative with residual SD", {
  dat <- data.frame(Y = c(1, 2, 3, 4, 5, 6), X = c(0, 1, 0, 1, 0, 1))
  fit <- stats::lm(Y ~ X, data = dat)
  w <- StructuralModelTester:::equation_link_wrapper(fit, "Y")
  expect_true(isTRUE(w$generative))
  expect_false(grepl("\\\\mathrm\\{E\\}", w$lhs, perl = TRUE))
  expect_true(is.finite(w$sigma))
  tex <- StructuralModelTester:::smt_eq_outcome_tex(w, "1.00 + 2.00 \\cdot X")
  expect_match(tex, "\\\\varepsilon", perl = TRUE)
  expect_match(tex, "\\\\mathcal\\{N\\}\\(0,", perl = TRUE)
})

test_that("logistic glm equation is not generative", {
  dat <- data.frame(
    Y = c(0, 1, 0, 1, 0, 1),
    X = c(0, 1, 0, 1, 0, 1)
  )
  fit <- stats::glm(Y ~ X, data = dat, family = stats::binomial())
  w <- StructuralModelTester:::equation_link_wrapper(fit, "Y")
  expect_false(isTRUE(w$generative))
  expect_match(w$lhs, "\\\\mathrm\\{P\\}")
  tex <- StructuralModelTester:::smt_eq_outcome_tex(w, "0.10 + 0.20 \\cdot X")
  expect_false(grepl("\\\\varepsilon", tex, perl = TRUE))
  expect_false(grepl("\\\\mathcal\\{N\\}", tex, perl = TRUE))
})

test_that("path_demo best equations include epsilon for continuous Y", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  html <- as.character(StructuralModelTester:::format_best_equations_html(res, st))
  expect_match(html, "\\\\varepsilon", perl = TRUE)
  expect_match(html, "\\\\mathcal\\{N\\}\\(0,", perl = TRUE)
})

test_that("lmer intercept-only merges u_0j into intercept term", {
  skip_if_not_installed("lmerTest")
  set.seed(1)
  dat <- data.frame(
    y = rnorm(40),
    x = rnorm(40),
    id = factor(rep(1:10, each = 4))
  )
  fit <- lmerTest::lmer(y ~ x + (1 | id), data = dat)
  w <- StructuralModelTester:::equation_link_wrapper(fit, "y")
  expect_true(isTRUE(w$generative))
  expect_true("(Intercept)" %in% names(w$re_u_map))
  expect_match(w$re_dist, "u_\\{0j\\}", perl = TRUE)

  cf <- StructuralModelTester:::path_fixed_coefs(fit)
  V <- as.matrix(stats::vcov(fit))
  se <- stats::setNames(sqrt(diag(V)), colnames(V))
  lines <- StructuralModelTester:::format_eta_term_lines(
    cf, se, u_map = w$re_u_map
  )
  expect_true(length(lines) >= 2L)
  expect_match(lines[[1]], "\\(.*u_\\{0j\\}.*\\)", perl = TRUE)
  expect_false(grepl("u_\\{0j\\}", lines[[2]], perl = TRUE))

  tex <- StructuralModelTester:::smt_eq_outcome_tex(w, lines)
  expect_match(tex, "\\\\begin\\{aligned\\}", perl = TRUE)
  expect_match(tex, "u_\\{0j\\}", perl = TRUE)
  expect_match(tex, "\\\\varepsilon", perl = TRUE)
})

test_that("lmer slopes merge into the Condition fixed term", {
  skip_if_not_installed("lmerTest")
  csv <- StructuralModelTester:::smt_sample_path("repeated_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("repeated_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "repeated_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  fit <- res$fit[["Score"]]
  expect_true(inherits(fit, "merMod"))
  w <- StructuralModelTester:::equation_link_wrapper(fit, "Score")
  expect_true(length(w$re_u_map) >= 2L)

  cf <- StructuralModelTester:::path_fixed_coefs(fit)
  se <- stats::setNames(
    sqrt(diag(as.matrix(stats::vcov(fit)))),
    names(cf)
  )
  lines <- StructuralModelTester:::format_eta_term_lines(
    cf, se, u_map = w$re_u_map
  )
  # Intercept has u_0j; Condition dummy has u_1j; Age has neither
  expect_match(lines[[1]], "u_\\{0j\\}", perl = TRUE)
  cond_line <- lines[grepl("Condition", lines)]
  expect_true(length(cond_line) >= 1L)
  expect_match(cond_line[[1]], "u_\\{1j\\}", perl = TRUE)
  age_line <- lines[grepl("Age", lines)]
  if (length(age_line)) {
    expect_false(grepl("u_\\{", age_line[[1]], perl = TRUE))
  }

  html <- as.character(StructuralModelTester:::format_best_equations_html(res, st))
  expect_match(html, "u_\\{0j\\}", perl = TRUE)
  expect_match(html, "u_\\{1j\\}", perl = TRUE)
  # No trailing separate RE addends like "+ u_{1j} Condition"
  expect_false(grepl("\\+ u_\\{1j\\}\\\\,", html, perl = TRUE))
  expect_match(html, "\\\\varepsilon", perl = TRUE)
  # Singular repeated_demo: rho shown in danger color (boundary), no text tag
  expect_match(w$re_dist, "\\\\color\\{#b33b3b\\}\\{\\\\rho_\\{01\\}=", perl = TRUE)
  expect_false(grepl("\\\\mathrm\\{boundary\\}", w$re_dist, perl = TRUE))
})
