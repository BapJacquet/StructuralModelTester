test_that("report_module_types lists the v1 pin sources", {
  types <- StructuralModelTester:::report_module_types()
  expect_true(all(c(
    "coef_table", "equations", "cfa_measurement", "pca_screen",
    "residual", "edge_plot", "power"
  ) %in% names(types)))
})

test_that("report_add_module / report_remove_module round-trip", {
  mods <- StructuralModelTester:::empty_report_modules()
  mods <- StructuralModelTester:::report_add_module(mods, "coef_table")
  expect_length(mods, 1L)
  expect_identical(mods[[1]]$type, "coef_table")
  expect_true(nzchar(mods[[1]]$uid))
  uid <- mods[[1]]$uid
  mods <- StructuralModelTester:::report_remove_module(mods, uid)
  expect_length(mods, 0L)
})

test_that("residual / edge / latent modules require pin-time ids", {
  mods <- StructuralModelTester:::empty_report_modules()
  expect_length(
    StructuralModelTester:::report_add_module(mods, "residual"),
    0L
  )
  mods <- StructuralModelTester:::report_add_module(
    mods, "residual", list(dv_id = "Y")
  )
  expect_length(mods, 1L)
  expect_identical(mods[[1]]$params$dv_id, "Y")

  mods2 <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "edge_plot",
    list(edge_id = "e1")
  )
  expect_identical(mods2[[1]]$params$edge_id, "e1")

  expect_length(
    StructuralModelTester:::report_add_module(
      StructuralModelTester:::empty_report_modules(), "cfa_measurement"
    ),
    0L
  )
  cfa <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "cfa_measurement",
    list(latent_id = "Uni")
  )
  expect_identical(cfa[[1]]$params$latent_id, "Uni")
  pca <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "pca_screen",
    list(latent_id = "Multi")
  )
  expect_identical(pca[[1]]$params$latent_id, "Multi")
  expect_false(StructuralModelTester:::report_has_module(
    cfa, "cfa_measurement", list(latent_id = "Multi")
  ))
})

test_that("report_has_module matches type and params", {
  mods <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "residual",
    list(dv_id = "Score")
  )
  expect_true(StructuralModelTester:::report_has_module(
    mods, "residual", list(dv_id = "Score")
  ))
  expect_false(StructuralModelTester:::report_has_module(
    mods, "residual", list(dv_id = "Other")
  ))
  expect_false(StructuralModelTester:::report_has_module(mods, "coef_table"))
})

test_that("path coefficient pins can be global and per-DV", {
  mods <- StructuralModelTester:::empty_report_modules()
  mods <- StructuralModelTester:::report_add_module(mods, "coef_table", list())
  mods <- StructuralModelTester:::report_add_module(
    mods, "coef_table", list(dv_id = "z")
  )
  mods <- StructuralModelTester:::report_add_module(
    mods, "coef_table", list(dv_id = "Y")
  )
  expect_length(mods, 3L)
  expect_true(StructuralModelTester:::report_has_module(mods, "coef_table", list()))
  expect_true(StructuralModelTester:::report_has_module(
    mods, "coef_table", list(dv_id = "z")
  ))
  expect_true(StructuralModelTester:::report_has_module(
    mods, "coef_table", list(dv_id = "Y")
  ))
  expect_false(StructuralModelTester:::report_has_module(
    mods, "coef_table", list(dv_id = "Other")
  ))
  # Exact empty params must not match a DV-scoped pin
  only_dv <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "coef_table",
    list(dv_id = "z")
  )
  expect_false(StructuralModelTester:::report_has_module(
    only_dv, "coef_table", list()
  ))
  expect_match(
    StructuralModelTester:::report_module_title(mods[[1]], NULL),
    "all",
    ignore.case = TRUE
  )
  expect_match(
    StructuralModelTester:::report_module_title(mods[[2]], NULL),
    "z"
  )
})

test_that("smt_json_script_tag embeds JSON for clipboard buttons", {
  tag <- StructuralModelTester:::smt_json_script_tag("smt-clip-tex-x", "a \\cdot b")
  expect_true(inherits(tag, "shiny.tag") || inherits(tag, "html"))
  html <- as.character(tag)
  expect_match(html, "application/json", fixed = TRUE)
  expect_match(html, "smt-clip-tex-x", fixed = TRUE)
  expect_match(html, "a \\\\cdot b|a · b|cdot", perl = TRUE)
})

test_that("report_module_title includes DV / edge labels when state given", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "X", kind = "observed")
  st <- StructuralModelTester:::add_node(st, "Y", kind = "observed")
  st <- StructuralModelTester:::add_edge(st, "X", "Y", type = "confirmatory")
  eid <- st$edges$id[[1]]

  mod <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "residual",
    list(dv_id = "Y")
  )[[1]]
  expect_match(
    StructuralModelTester:::report_module_title(mod, st),
    "Y"
  )

  mod2 <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "edge_plot",
    list(edge_id = eid)
  )[[1]]
  tit <- StructuralModelTester:::report_module_title(mod2, st)
  expect_match(tit, "X")
  expect_match(tit, "Y")
})
