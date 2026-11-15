path_demo_power_state <- function() {
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(js), "path_demo_model.json missing")
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  st <- StructuralModelTester:::set_node_sim_resid_sd(st, "Y", resid_sd = 2.5)
  st <- StructuralModelTester:::set_node_sim_resid_sd(st, "M", resid_sd = 1.75)
  st
}

test_that("power_dv_residuals_snapshot lists confirmatory-target DVs", {
  st <- path_demo_power_state()
  snap <- StructuralModelTester:::power_dv_residuals_snapshot(st)
  expect_true(is.data.frame(snap))
  expect_true(all(c("id", "label", "resid_sd", "kind") %in% names(snap)))
  expect_true("Y" %in% snap$id)
  expect_true("M" %in% snap$id)
  expect_equal(snap$resid_sd[snap$id == "Y"], 2.5)
  expect_equal(snap$resid_sd[snap$id == "M"], 1.75)
  expect_true(all(snap$kind == "continuous"))
})

test_that("run_confirmatory_power attaches dv_residuals snapshot", {
  st <- path_demo_power_state()
  # Tiny grid so the test stays fast
  pw <- StructuralModelTester:::run_confirmatory_power(
    st,
    n_max = 40L,
    n_reps = 2L,
    seed = 1L,
    n_steps = 0L,
    n_cap = 40L,
    n_grid = c(20L, 40L)
  )
  expect_true(is.data.frame(pw$dv_residuals))
  expect_true("Y" %in% pw$dv_residuals$id)
  expect_equal(pw$dv_residuals$resid_sd[pw$dv_residuals$id == "Y"], 2.5)
})

test_that("smt_power_dv_resid_ui renders from snapshot or state", {
  st <- path_demo_power_state()
  ui_state <- StructuralModelTester:::smt_power_dv_resid_ui(state = st)
  expect_false(is.null(ui_state))

  pw <- list(dv_residuals = StructuralModelTester:::power_dv_residuals_snapshot(st))
  ui_pw <- StructuralModelTester:::smt_power_dv_resid_ui(pw = pw, compact = TRUE)
  expect_false(is.null(ui_pw))
})
