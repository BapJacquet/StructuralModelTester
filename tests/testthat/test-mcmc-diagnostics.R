test_that("brms_mcmc_diagnostics summarizes a short path_demo fit", {
  skip_if_not(
    StructuralModelTester:::bayes_backend_available(),
    "brms + Stan backend not available"
  )
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_bayes_model(
    st, dat, structure = "independent",
    mark_nonsig = FALSE,
    chains = 1L, iter = 300L, warmup = 150L, seed = 5L
  )
  expect_identical(res$sampler, "brms")
  fit <- res$fit[[1]]
  expect_true(inherits(fit, "brmsfit"))

  d <- StructuralModelTester:::brms_mcmc_diagnostics(fit)
  expect_true(d$status %in% c("ok", "warn", "bad"))
  expect_true(is.finite(d$max_rhat))
  expect_true(is.finite(d$min_neff_ratio))
  expect_true(nzchar(d$message))

  agg <- StructuralModelTester:::brms_mcmc_diagnostics_aggregate(res, st)
  expect_gte(agg$n_models, 1L)
  expect_true(agg$status %in% c("ok", "warn", "bad"))
  html <- StructuralModelTester:::format_mcmc_health_html(agg)
  expect_true(inherits(html, "shiny.tag") || inherits(html, "html"))
  expect_match(as.character(html), "smt-mcmc-health", fixed = TRUE)

  p <- StructuralModelTester:::mcmc_diagnostics_plot(fit, title = "Y")
  expect_s3_class(p, "ggplot")

  y <- StructuralModelTester:::brms_observed_y(fit)
  expect_true(length(y) >= 5L)
  yrep <- StructuralModelTester:::brms_posterior_predict_y(fit, ndraws = 20L)
  expect_true(is.matrix(yrep) && nrow(yrep) >= 1L && ncol(yrep) == length(y))
  ppc <- StructuralModelTester:::brms_ppc_plot(fit, title = "PPC", ndraws = 20L)
  expect_s3_class(ppc, "ggplot")
})

test_that("non-brms results yield unavailable MCMC aggregate", {
  agg <- StructuralModelTester:::brms_mcmc_diagnostics_aggregate(
    list(sampler = "lmer", fit = list()),
    NULL
  )
  expect_identical(agg$status, "unavailable")
  expect_null(StructuralModelTester:::format_mcmc_health_html(agg))
})

test_that("bayes_diagnostics help topic includes PPC example figures", {
  topics <- StructuralModelTester:::smt_field_help_topics()
  expect_true("bayes_diagnostics" %in% names(topics))
  opts <- topics$bayes_diagnostics$options
  expect_gte(length(opts), 3L)
  figs <- vapply(opts, function(o) {
    src <- o$figure$src %||% ""
    as.character(src)[[1]]
  }, character(1))
  expect_true(any(grepl("ppc-good", figs, fixed = TRUE)))
  expect_true(any(grepl("ppc-shift", figs, fixed = TRUE)))
  # Assets must exist under www/help
  for (src in figs[nzchar(figs)]) {
    path <- system.file("app", "www", src, package = "StructuralModelTester")
    if (!nzchar(path)) {
      path <- file.path("inst", "app", "www", src)
    }
    expect_true(file.exists(path), info = src)
  }
  modal <- StructuralModelTester:::smt_field_help_modal("bayes_diagnostics")
  html <- paste(as.character(modal), collapse = "\n")
  expect_match(html, "help/ppc-good.png", fixed = TRUE)
  expect_match(html, "smt-help-figure", fixed = TRUE)
})
