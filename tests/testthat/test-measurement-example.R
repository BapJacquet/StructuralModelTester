test_that("latent measurement example fits (two-step CFA + path)", {
  ex <- StructuralModelTester:::smt_example_resolved("measurement")
  skip_if(is.null(ex) || !nzchar(ex$data_path) || !nzchar(ex$model_path),
          "measurement example files missing")
  dat <- as.data.frame(readr::read_csv(ex$data_path, show_col_types = FALSE))
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(ex$model_path), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(st, dat, mark_nonsig = FALSE)
  expect_identical(res$engine, "path")
  expect_true(isTRUE(res$two_step))
  expect_true(any(res$estimates$op == "=~"))
  expect_true(any(res$estimates$op == "~"))
  # Schema must include Bayes/CI columns so measurement + path rbind works
  expect_true(all(c("log_bf10", "kl_prior_post", "ci_low") %in% names(res$estimates)))
})
