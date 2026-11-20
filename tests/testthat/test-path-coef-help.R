test_that("path coefficient help is scoped to live columns", {
  keys_freq <- StructuralModelTester:::path_coefficients_help_keys_for_columns(
    c("Path", "Level", "Estimate", "SE", "p", "pHolm")
  )
  expect_true(all(c("Path", "Level", "Estimate", "SE", "p", "pHolm") %in% keys_freq))
  expect_false("HDI" %in% keys_freq)
  expect_false("pd" %in% keys_freq)

  keys_bayes <- StructuralModelTester:::path_coefficients_help_keys_for_columns(
    c("Path", "Estimate", "SD", "95% HDI", "pd", "ROPE%", "ROPE",
      "log BF10", "SD\u2193", "KL")
  )
  expect_true(all(c(
    "HDI", "pd", "ROPE%", "ROPE", "log BF10", "SD\u2193", "KL", "SD"
  ) %in% keys_bayes))
  expect_false("SE" %in% keys_bayes)
  expect_false("p" %in% keys_bayes)

  opts <- StructuralModelTester:::path_coefficients_help_options_for_columns(
    c("pd", "95% HDI", "log BF10")
  )
  labs <- vapply(opts, function(o) o$label, character(1))
  expect_true("pd" %in% labs)
  expect_true("95% HDI" %in% labs)
  expect_true("log BF10" %in% labs)
  pd <- opts[[match("pd", labs)]]
  expect_true(length(pd$details) >= 1L)
  expect_true(length(pd$pros) >= 1L)
  expect_match(pd$pros_title, "How to read", ignore.case = TRUE)

  modal <- StructuralModelTester:::path_coefficients_help_modal(
    c("Estimate", "pd"),
    scope_note = "Filtered to DV “Y”."
  )
  expect_true(inherits(modal, "shiny.tag") || inherits(modal, "shiny.tag.list"))
  html <- paste(as.character(modal), collapse = "\n")
  expect_match(html, "Click any column header", fixed = TRUE)

  col_modal <- StructuralModelTester:::path_coefficient_column_help_modal("pd")
  col_html <- paste(as.character(col_modal), collapse = "\n")
  expect_match(col_html, ">pd<", perl = TRUE)
  expect_match(col_html, "Probability of direction", fixed = TRUE)

  hdi_modal <- StructuralModelTester:::path_coefficient_column_help_modal("95% HDI")
  hdi_html <- paste(as.character(hdi_modal), collapse = "\n")
  expect_match(hdi_html, "95% HDI", fixed = TRUE)
})
