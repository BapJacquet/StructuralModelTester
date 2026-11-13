test_that("as_binary01 uses alphabetical order for character", {
  # Female < Male alphabetically → Female=0, Male=1
  x <- c("Male", "Female", "Male", "Female")
  expect_equal(
    StructuralModelTester:::as_binary01(x),
    c(1L, 0L, 1L, 0L)
  )
  # I < T alphabetically → I=0, T=1 (disagrees with treatment coding T=0)
  m <- c("T", "I", "T", "I")
  expect_equal(
    StructuralModelTester:::as_binary01(m),
    c(1L, 0L, 1L, 0L)
  )
})

test_that("binary01_for_node follows node base / treatment coding", {
  st <- binary_node_state(
    label = "Method",
    base = "T",
    levels = c("T", "I")
  )
  nid <- st$nodes$id[[1]]
  x <- c("T", "I", "T", "I", "T")

  coded <- StructuralModelTester:::binary01_for_node(st, nid, x)
  expect_equal(coded, c(0L, 1L, 0L, 1L, 0L))

  # Opposite of alphabetical as_binary01
  expect_false(identical(
    coded,
    StructuralModelTester:::as_binary01(x)
  ))
})

test_that("binary01_for_node handles Gender Male=0 Female=1", {
  st <- binary_node_state(
    label = "Gender",
    base = "Male",
    levels = c("Male", "Female")
  )
  nid <- st$nodes$id[[1]]
  x <- c("Male", "Female", "Male", "Female")

  coded <- StructuralModelTester:::binary01_for_node(st, nid, x)
  expect_equal(coded, c(0L, 1L, 0L, 1L))
})

test_that("binary_plot_level_labels put base first", {
  st <- binary_node_state(
    label = "Method",
    base = "T",
    levels = c("T", "I")
  )
  nid <- st$nodes$id[[1]]
  labs <- StructuralModelTester:::binary_plot_level_labels(st, nid)
  expect_equal(labs, c("T", "I"))
})

test_that("binary01_for_node falls back to as_binary01 without coding", {
  x <- c("T", "I", "T")
  expect_equal(
    StructuralModelTester:::binary01_for_node(NULL, NA_character_, x),
    StructuralModelTester:::as_binary01(x)
  )
})
