test_that("empty / ensure prepare state are stable", {
  empty <- StructuralModelTester:::empty_prepare_state()
  expect_equal(empty$mode, "steps")
  expect_equal(nrow(empty$steps), 0L)
  expect_equal(empty$script, "")
  expect_equal(empty$next_step, 1L)

  expect_equal(
    StructuralModelTester:::ensure_prepare_state(NULL)$mode,
    "steps"
  )
  # jsonlite empty array → list() must not break
  restored <- StructuralModelTester:::ensure_prepare_state(list(
    mode = "steps",
    steps = list(),
    script = "",
    next_step = 1L
  ))
  expect_equal(nrow(restored$steps), 0L)
  expect_true(is.data.frame(restored$steps))
})

test_that("add / update / remove / move prepare steps", {
  prep <- StructuralModelTester:::empty_prepare_state()
  prep <- StructuralModelTester:::add_prepare_step(prep, "filter", "Age > 25")
  prep <- StructuralModelTester:::add_prepare_step(prep, "mutate", "log_age = log(Age)")
  expect_equal(prep$steps$id, c("step_1", "step_2"))
  expect_equal(prep$next_step, 3L)

  prep <- StructuralModelTester:::update_prepare_step(
    prep, "step_1",
    expr = "Age >= 30",
    enabled = FALSE
  )
  expect_equal(prep$steps$expr[1], "Age >= 30")
  expect_false(prep$steps$enabled[1])

  prep <- StructuralModelTester:::move_prepare_step(prep, "step_2", "up")
  expect_equal(prep$steps$id, c("step_2", "step_1"))

  prep <- StructuralModelTester:::remove_prepare_step(prep, "step_1")
  expect_equal(prep$steps$id, "step_2")
})

test_that("apply_prepare filter and mutate steps", {
  raw <- sample_raw()
  prep <- StructuralModelTester:::empty_prepare_state()
  prep <- StructuralModelTester:::add_prepare_step(prep, "filter", "Age >= 30")
  prep <- StructuralModelTester:::add_prepare_step(
    prep, "mutate", "log_score = log(Score)"
  )

  out <- StructuralModelTester:::apply_prepare(raw, prep)
  expect_equal(nrow(out), 4L)
  expect_true("log_score" %in% names(out))
  expect_equal(out$log_score, log(out$Score))

  # Disabled steps are skipped
  prep$steps$enabled[1] <- FALSE
  out2 <- StructuralModelTester:::apply_prepare(raw, prep)
  expect_equal(nrow(out2), nrow(raw))
})

test_that("apply_prepare script mode returns a data frame", {
  raw <- sample_raw()
  prep <- StructuralModelTester:::empty_prepare_state()
  prep$mode <- "script"
  prep$script <- "subset(data, Age > 30)"

  out <- StructuralModelTester:::apply_prepare(raw, prep)
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3L)
  expect_true(all(out$Age > 30))
})

test_that("try_apply_prepare captures errors without throwing", {
  raw <- sample_raw()
  prep <- StructuralModelTester:::empty_prepare_state()
  prep <- StructuralModelTester:::add_prepare_step(prep, "filter", "not_a_column > 0")

  res <- StructuralModelTester:::try_apply_prepare(raw, prep)
  expect_null(res$data)
  expect_true(is.character(res$error) && nzchar(res$error))
  expect_match(res$error, "Step 1")
})

test_that("parse_mutate_assignment accepts = and <-", {
  a <- StructuralModelTester:::parse_mutate_assignment("y = x + 1")
  expect_equal(a$lhs, "y")
  expect_equal(a$rhs, "x + 1")

  b <- StructuralModelTester:::parse_mutate_assignment("y <- x + 1")
  expect_equal(b$lhs, "y")
  expect_equal(b$rhs, "x + 1")

  expect_error(
    StructuralModelTester:::parse_mutate_assignment("just_an_expr"),
    "new_col"
  )
})

test_that("prepare_is_active detects actionable content", {
  empty <- StructuralModelTester:::empty_prepare_state()
  expect_false(StructuralModelTester:::prepare_is_active(empty))

  with_step <- StructuralModelTester:::add_prepare_step(
    empty, "filter", "Age > 18"
  )
  expect_true(StructuralModelTester:::prepare_is_active(with_step))

  scripted <- empty
  scripted$mode <- "script"
  scripted$script <- "data"
  expect_true(StructuralModelTester:::prepare_is_active(scripted))
})

test_that("prepare survives JSON roundtrip on a project", {
  raw <- sample_raw()
  prep <- StructuralModelTester:::empty_prepare_state()
  prep <- StructuralModelTester:::add_prepare_step(prep, "filter", "Score >= 60")
  prep <- StructuralModelTester:::add_prepare_step(
    prep, "mutate", "z = scale(Score)"
  )

  st <- StructuralModelTester:::empty_model_state()
  json <- model_to_json(st, prepare = prep)
  back <- StructuralModelTester:::prepare_from_project_json(json)
  expect_equal(back$mode, "steps")
  expect_equal(nrow(back$steps), 2L)
  expect_equal(back$steps$expr[[1]], "Score >= 60")

  out <- StructuralModelTester:::apply_prepare(raw, back)
  expect_equal(nrow(out), 4L)
  expect_true("z" %in% names(out))
})

test_that("smt_browse_datatable enables scrollX", {
  dt <- StructuralModelTester:::smt_browse_datatable(
    sample_raw(),
    page_length = 5L,
    scroll_y = "200px"
  )
  expect_s3_class(dt, "datatables")
  opts <- dt$x$options
  expect_true(isTRUE(opts$scrollX))
  expect_equal(opts$scrollY, "200px")
})
