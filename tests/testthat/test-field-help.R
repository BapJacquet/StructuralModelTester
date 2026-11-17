test_that("field help catalog covers analysis topics and builds a modal", {
  topics <- StructuralModelTester:::smt_field_help_topics()
  expect_true(length(topics) >= 8L)
  expect_true(all(c(
    "inference_mode", "bayes_structure", "conf_p_adjust",
    "uncertainty_method", "latent_score_method"
  ) %in% names(topics)))
  for (id in names(topics)) {
    topic <- topics[[id]]
    expect_true(is.character(topic$title) && nzchar(topic$title))
    expect_true(is.character(topic$lead) && nzchar(topic$lead))
    expect_true(length(topic$options) >= 1L)
    modal <- StructuralModelTester:::smt_field_help_modal(id)
    expect_true(inherits(modal, "shiny.tag") || inherits(modal, "shiny.tag.list"))
  }
  btn <- StructuralModelTester:::smt_help_button("inference_mode")
  expect_true(inherits(btn, "shiny.tag"))
  lab <- StructuralModelTester:::smt_label_with_help("Framework", "inference_mode")
  expect_true(inherits(lab, "shiny.tag"))
})
