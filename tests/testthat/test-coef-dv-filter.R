test_that("path coef table and syntax filter to the selected DV", {
  ex <- StructuralModelTester:::smt_example_resolved("measurement")
  skip_if(is.null(ex) || !nzchar(ex$data_path) || !nzchar(ex$model_path),
          "measurement example files missing")
  dat <- as.data.frame(readr::read_csv(ex$data_path, show_col_types = FALSE))
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(ex$model_path), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(st, dat, mark_nonsig = FALSE)
  dvs <- StructuralModelTester:::structural_dv_ids(st)
  expect_true(length(dvs) >= 1L)
  dv <- dvs[[1]]
  tab <- StructuralModelTester:::estimates_table(st, estimates = res$estimates)
  sp <- StructuralModelTester:::path_arrow_split(tab$Path)
  expect_true(any(sp$to == "z") || any(sp$to == "Z"))
  # Measurement loadings (=~) stay out of Path coefficients
  expect_false(any(sp$to %in% c("x1", "y1")))
  filt <- StructuralModelTester:::filter_estimates_table_for_dv(tab, st, dv)
  labs <- StructuralModelTester:::dv_coef_to_labels(st, dv)
  expect_true(nrow(filt) >= 1L)
  spf <- StructuralModelTester:::path_arrow_split(filt$Path)
  expect_true(all(spf$to %in% labs | grepl("[", spf$to, fixed = TRUE)))
  expect_false(any(spf$to %in% c("x1", "y1")))
  syn <- StructuralModelTester:::filter_result_syntax_for_dv(res$syntax, st, dv)
  expect_true(nzchar(syn))
  expect_false(grepl("=~", syn, fixed = TRUE))
  expect_true(grepl("z ~", syn, fixed = TRUE) || grepl(": z ~", syn, fixed = TRUE))
})
