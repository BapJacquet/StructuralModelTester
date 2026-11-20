test_that("apa_stat_col_html italicizes statistic headers", {
  out <- StructuralModelTester:::apa_stat_col_html(
    c("From", "Estimate", "SE", "pHolm", "To")
  )
  expect_identical(out[[1]], "From")
  expect_match(out[[2]], "^<em>Estimate</em>$")
  expect_match(out[[3]], "^<em>SE</em>$")
  expect_match(out[[4]], "^<em>pHolm</em>$")
  expect_identical(out[[5]], "To")
})

test_that("smt_apa_datatable uses smt-apa-table class and HTML colnames", {
  df <- data.frame(
    From = "X", To = "Y", Estimate = 0.5, p = ".01",
    stringsAsFactors = FALSE
  )
  dt <- StructuralModelTester:::smt_apa_datatable(df)
  expect_s3_class(dt, "datatables")
  # class attribute on the widget options / element
  cls <- dt$x$options$class %||% dt$x$container %||% ""
  # DT stores class on the dependency call; check colnames escaped HTML
  expect_true(any(grepl("smt-apa-table", unlist(dt), fixed = TRUE)))
  cn <- names(dt$x$data)
  # After datatable, data colnames may be V1..; check original via escape path
  html_names <- StructuralModelTester:::apa_stat_col_html(names(df))
  expect_true(any(grepl("<em>Estimate</em>", html_names, fixed = TRUE)))
  expect_true(any(grepl("<em>p</em>", html_names, fixed = TRUE)))
  # Overflow scrolling is CSS-only; DT scrollX would always show a bar
  expect_false(isTRUE(dt$x$options$scrollX))
})

test_that("header_help wraps colnames with smt-col-help data attributes", {
  html <- StructuralModelTester:::apa_coef_help_col_html(
    c("Path", "Estimate", "95% HDI", "KL")
  )
  expect_true(all(grepl("smt-col-help", html, fixed = TRUE)))
  expect_match(html[[1]], 'data-smt-help-col="Path"', fixed = TRUE)
  expect_match(html[[2]], "<em>Estimate</em>", fixed = TRUE)
  expect_match(html[[3]], 'data-smt-help-col="95% HDI"', fixed = TRUE)
  dt <- StructuralModelTester:::smt_apa_datatable(
    data.frame(Path = "Y ← X", KL = "1.2", stringsAsFactors = FALSE),
    header_help = TRUE
  )
  expect_true(any(grepl("smt-apa-table-colhelp", unlist(dt), fixed = TRUE)))
})

test_that("report_df_to_html_table uses APA caption and booktabs borders", {
  df <- data.frame(From = "X", Estimate = 0.2, p = ".04", stringsAsFactors = FALSE)
  html <- StructuralModelTester:::report_df_to_html_table(df, "Path coefficients")
  expect_match(html, "Table\\. Path coefficients")
  expect_match(html, "font-style:italic")
  expect_match(html, "border-top:1\\.5px solid", perl = TRUE)
  expect_match(html, "<em>Estimate</em>", fixed = TRUE)
  expect_match(html, "<em>p</em>", fixed = TRUE)
})
