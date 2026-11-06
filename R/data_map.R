#' Read an uploaded data file
#' @keywords internal
read_data_file <- function(path, name = path) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("csv", "txt", "tsv")) {
    delim <- if (ext == "tsv") "\t" else ","
    # sniff semicolon
    if (ext == "csv") {
      first <- readLines(path, n = 1, warn = FALSE)
      if (length(first) && grepl(";", first) && !grepl(",", first)) delim <- ";"
    }
    return(as.data.frame(readr::read_delim(path, delim = delim, show_col_types = FALSE, progress = FALSE)))
  }
  if (ext == "rds") {
    obj <- readRDS(path)
    if (!is.data.frame(obj)) stop("RDS object must be a data frame.", call. = FALSE)
    return(as.data.frame(obj))
  }
  stop("Unsupported file type. Use CSV, TSV, or RDS.", call. = FALSE)
}

#' Variable list metadata for the left rail
#' @keywords internal
variable_list_from_data <- function(data, state) {
  if (is.null(data) || !ncol(data)) {
    return(data.frame(
      column = character(),
      mapped_to = character(),
      type = character(),
      stringsAsFactors = FALSE
    ))
  }
  mapped <- state$nodes$observed_column
  names(mapped) <- state$nodes$id
  cols <- names(data)
  mapped_to <- vapply(cols, function(col) {
    hit <- state$nodes$id[!is.na(state$nodes$observed_column) & state$nodes$observed_column == col]
    if (length(hit)) hit[[1]] else NA_character_
  }, character(1))

  types <- vapply(cols, function(col) {
    x <- data[[col]]
    if (is.numeric(x)) "continuous" else "other"
  }, character(1))

  data.frame(
    column = cols,
    mapped_to = mapped_to,
    type = types,
    stringsAsFactors = FALSE
  )
}
