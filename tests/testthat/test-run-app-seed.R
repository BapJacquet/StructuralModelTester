test_that("smt_normalize_seed_data copies frames and rejects bad input", {
  dat <- data.frame(
    Age = c(20L, 30L),
    Group = factor(c("A", "B"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )
  out <- StructuralModelTester:::smt_normalize_seed_data(dat)
  expect_true(is.data.frame(out))
  expect_identical(out$Age, dat$Age)
  expect_true(is.factor(out$Group))
  expect_identical(levels(out$Group), c("A", "B"))
  # Independent copy
  out$Age[[1]] <- 99L
  expect_identical(dat$Age[[1]], 20L)

  expect_null(StructuralModelTester:::smt_normalize_seed_data(NULL))
  expect_error(
    StructuralModelTester:::smt_normalize_seed_data(list(a = 1)),
    "data.frame"
  )
  expect_error(
    StructuralModelTester:::smt_normalize_seed_data(data.frame()),
    "at least one"
  )
})

test_that("run_app pins smt.seed_data and clears it when data is NULL", {
  old <- getOption("smt.seed_data", NULL)
  on.exit(options(smt.seed_data = old), add = TRUE)

  dat <- data.frame(x = 1:3, y = 4:6)
  options(smt.seed_data = StructuralModelTester:::smt_normalize_seed_data(dat))
  expect_true(is.data.frame(getOption("smt.seed_data")))
  expect_equal(nrow(getOption("smt.seed_data")), 3L)

  # Mimic run_app(data = NULL) clearing
  options(smt.seed_data = StructuralModelTester:::smt_normalize_seed_data(NULL))
  expect_null(getOption("smt.seed_data"))
})

test_that("smt_debug console echo is gated by smt.debug.console", {
  old_c <- getOption("smt.debug.console", NULL)
  old_q <- getOption("smt.quiet", NULL)
  on.exit({
    options(smt.debug.console = old_c, smt.quiet = old_q)
  }, add = TRUE)
  options(smt.quiet = FALSE)

  options(smt.debug.console = FALSE)
  expect_silent(StructuralModelTester:::smt_debug("quiet-console-test"))

  options(smt.debug.console = TRUE)
  expect_message(
    StructuralModelTester:::smt_debug("loud-console-test"),
    "loud-console-test",
    fixed = TRUE
  )
})
