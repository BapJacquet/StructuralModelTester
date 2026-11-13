sample_raw <- function(n = 6L) {
  data.frame(
    Age = c(20, 25, 30, 35, 40, 45),
    Gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
    Method = c("T", "T", "I", "I", "T", "I"),
    Score = c(50, 55, 60, 65, 70, 75),
    stringsAsFactors = FALSE
  )
}

binary_node_state <- function(label = "Method",
                              base = "T",
                              levels = c("T", "I"),
                              column = label) {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(
    st,
    label = label,
    observed_column = column,
    kind = "observed"
  )
  nid <- st$nodes$id[[1]]
  StructuralModelTester:::set_node_factor_coding(
    st,
    nid,
    base = base,
    levels = levels,
    data = NULL
  )
}
