# Cross-check every built-in example: link plots before/after analysis under
# p-values, AIC/BIC search, and Bayesian frameworks, plus results + diagnostics.

example_ids <- vapply(
  StructuralModelTester:::smt_examples_catalog(),
  function(ex) as.character(ex$id),
  character(1)
)

test_that("built-in example catalog resolves to existing data + model files", {
  expect_true(length(example_ids) >= 1L)
  for (id in example_ids) {
    ex <- StructuralModelTester:::smt_example_resolved(id)
    expect_true(!is.null(ex), info = id)
    expect_true(nzchar(ex$data_path), info = id)
    expect_true(nzchar(ex$model_path), info = id)
    expect_true(file.exists(ex$data_path), info = id)
    expect_true(file.exists(ex$model_path), info = id)
  }
})

for (ex_id in example_ids) {
  local({
    id <- ex_id

    test_that(sprintf("example '%s': all link plots before analysis", id), {
      bun <- smt_example_bundle(id)
      expect_smt_all_edge_plots(
        bun$state, bun$dat, result = NULL,
        info = paste(id, "before")
      )
    })

    test_that(sprintf("example '%s': p-values results, plots, residuals", id), {
      bun <- smt_example_bundle(id)
      res <- suppressWarnings(
        StructuralModelTester:::fit_pvalue_model(
          bun$state, bun$dat,
          alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
        )
      )
      expect_true(is.list(res) && !is.null(res$state))
      expect_true(is.data.frame(res$estimates) && nrow(res$estimates) >= 1L)
      tab <- StructuralModelTester:::estimates_table(
        res$state,
        estimates = res$estimates,
        conf_adjust = "holm",
        result = res
      )
      expect_smt_estimates_table(tab, "pvalue", info = paste(id, "pvalue"))
      expect_smt_all_edge_plots(
        res$state, bun$dat, result = res,
        info = paste(id, "pvalue")
      )
      expect_smt_residual_diagnostics(res, info = paste(id, "pvalue"))
    })

    test_that(sprintf("example '%s': AIC/BIC search results, plots, residuals", id), {
      bun <- smt_example_bundle(id)
      res <- suppressWarnings(
        StructuralModelTester:::fit_aic_bic_search(
          bun$state, bun$dat, criterion = "bic"
        )
      )
      expect_true(is.list(res) && !is.null(res$state))
      expect_true(is.data.frame(res$estimates) && nrow(res$estimates) >= 1L)
      tab <- StructuralModelTester:::estimates_table(
        res$state,
        estimates = res$estimates,
        conf_adjust = "none",
        result = res
      )
      expect_smt_estimates_table(tab, "search", info = paste(id, "search"))
      expect_smt_all_edge_plots(
        res$state, bun$dat, result = res,
        info = paste(id, "search")
      )
      expect_smt_residual_diagnostics(res, info = paste(id, "search"))
    })

    test_that(sprintf("example '%s': Bayesian results, plots, MCMC/PPC", id), {
      skip_if_not(
        StructuralModelTester:::bayes_backend_available(),
        "brms + Stan backend not available"
      )
      bun <- smt_example_bundle(id)
      res <- suppressWarnings(
        StructuralModelTester:::fit_bayes_model(
          bun$state, bun$dat,
          structure = "independent",
          mark_nonsig = FALSE,
          chains = 1L, iter = 200L, warmup = 100L, seed = 11L
        )
      )
      expect_identical(res$sampler, "brms")
      expect_true(is.data.frame(res$estimates) && nrow(res$estimates) >= 1L)
      tab <- StructuralModelTester:::estimates_table(
        res$state,
        estimates = res$estimates,
        conf_adjust = "none",
        result = res
      )
      expect_smt_estimates_table(tab, "bayes", info = paste(id, "bayes"))
      expect_smt_all_edge_plots(
        res$state, bun$dat, result = res,
        info = paste(id, "bayes")
      )
      expect_smt_bayes_diagnostics(res, res$state, info = paste(id, "bayes"))
    })
  })
}
