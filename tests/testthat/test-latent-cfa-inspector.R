test_that("latent CFA inspector summary exposes loadings and diagnostics", {
  ex <- StructuralModelTester:::smt_example_resolved("measurement")
  skip_if(is.null(ex) || !nzchar(ex$data_path) || !nzchar(ex$model_path),
          "measurement example files missing")
  dat <- as.data.frame(readr::read_csv(ex$data_path, show_col_types = FALSE))
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(ex$model_path), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(st, dat, mark_nonsig = FALSE)
  lat <- st$nodes$id[vapply(st$nodes$id, function(id) {
    StructuralModelTester:::is_latent_node(st, id)
  }, logical(1))][[1]]
  sm <- StructuralModelTester:::latent_measurement_summary(res, st, lat)
  expect_false(is.null(sm))
  expect_true(!is.null(sm$loadings) && nrow(sm$loadings) >= 2L)
  expect_true(is.finite(sm$alpha) || is.list(sm$uni) || !is.null(sm$weights))
  expect_true(is.list(sm$uni))
  expect_true(is.finite(sm$uni$ratio) || length(sm$uni$eigenvalues) >= 2L)
  pca <- StructuralModelTester:::format_latent_pca_ui(sm$uni)
  expect_true(inherits(pca, "shiny.tag") || inherits(pca, "shiny.tag.list"))
  cfa_ui <- StructuralModelTester:::latent_cfa_block_ui(res, st, lat)
  pca_ui <- StructuralModelTester:::latent_pca_block_ui(res, st, lat)
  expect_true(inherits(cfa_ui, "shiny.tag") || inherits(cfa_ui, "shiny.tag.list"))
  expect_true(inherits(pca_ui, "shiny.tag") || inherits(pca_ui, "shiny.tag.list"))
  expect_true(nrow(StructuralModelTester:::latent_loadings_df(sm$loadings, st)) >= 2L)
  expect_true(nrow(StructuralModelTester:::latent_pca_eigen_df(sm$uni)) >= 2L)
  pca_ld <- StructuralModelTester:::latent_pca_loadings_df(
    sm$uni, st, which = "unrotated"
  )
  expect_true(nrow(pca_ld) >= 2L)
  expect_true(ncol(pca_ld) >= 3L)
  expect_true("Item" %in% names(pca_ld))
  expect_true(any(grepl("^PC", names(pca_ld))))

  # Uni: typically one Kaiser component → no varimax table
  sm_uni <- StructuralModelTester:::latent_measurement_summary(res, st, "Uni")
  if (!is.null(sm_uni$uni) && isTRUE(sum(sm_uni$uni$eigenvalues > 1) < 2L)) {
    expect_equal(
      nrow(StructuralModelTester:::latent_pca_loadings_df(
        sm_uni$uni, st, which = "rotated"
      )),
      0L
    )
  }
  # Multi: ≥2 Kaiser components → rotated RC table
  sm_multi <- StructuralModelTester:::latent_measurement_summary(res, st, "Multi")
  skip_if(is.null(sm_multi$uni), "Multi PCA missing")
  pca_rot <- StructuralModelTester:::latent_pca_loadings_df(
    sm_multi$uni, st, which = "rotated"
  )
  expect_true(nrow(pca_rot) >= 2L)
  expect_true(any(grepl("^RC", names(pca_rot))))
})
