test_that("set_node_label renames display name without changing id", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "Old", kind = "observed")
  nid <- st$nodes$id[[1]]
  st2 <- StructuralModelTester:::set_node_label(st, nid, "NewName")
  expect_equal(st2$nodes$id[[1]], nid)
  expect_equal(st2$nodes$label[[1]], "NewName")
  expect_equal(
    StructuralModelTester:::node_display_name(st2, nid),
    "NewName"
  )
})

test_that("set_node_label rejects empty and duplicate names", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "A", kind = "observed")
  st <- StructuralModelTester:::add_node(st, "B", kind = "observed")
  a <- st$nodes$id[st$nodes$label == "A"][[1]]
  expect_error(
    StructuralModelTester:::set_node_label(st, a, "  "),
    "empty"
  )
  expect_error(
    StructuralModelTester:::set_node_label(st, a, "B"),
    "already named"
  )
})

test_that("graph_payload uses node label for mapped nodes", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st, "Age", observed_column = "Age", kind = "observed"
  )
  nid <- st$nodes$id[[1]]
  st <- StructuralModelTester:::set_node_label(st, nid, "Age years")
  payload <- StructuralModelTester:::graph_payload(st, data = NULL)
  expect_equal(payload$nodes[[1]]$label, "Age years")
})
