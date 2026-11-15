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
