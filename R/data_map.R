#' Sanitize header names from the first spreadsheet / text row
#' @keywords internal
clean_header_names <- function(nms, n_cols = length(nms)) {
  nms <- as.character(nms)
  if (length(nms) < n_cols) {
    nms <- c(nms, rep(NA_character_, n_cols - length(nms)))
  }
  if (length(nms) > n_cols) nms <- nms[seq_len(n_cols)]
  for (i in seq_along(nms)) {
    if (is.na(nms[[i]]) || !nzchar(trimws(nms[[i]]))) {
      nms[[i]] <- paste0("X", i)
    } else {
      nms[[i]] <- trimws(nms[[i]])
    }
  }
  make.unique(nms, sep = "_")
}

#' Resolve an Excel sheet by 1-based index or name
#' @keywords internal
resolve_excel_sheet <- function(path, sheet = 1) {
  sheets <- readxl::excel_sheets(path)
  if (!length(sheets)) stop("This Excel file has no sheets.", call. = FALSE)
  if (is.null(sheet) || (is.character(sheet) && !nzchar(trimws(sheet)))) {
    return(sheets[[1]])
  }
  if (is.numeric(sheet) || grepl("^[0-9]+$", as.character(sheet))) {
    idx <- as.integer(sheet)
    if (!is.finite(idx) || idx < 1L || idx > length(sheets)) {
      stop(
        sprintf("Excel sheet index must be between 1 and %d.", length(sheets)),
        call. = FALSE
      )
    }
    return(sheets[[idx]])
  }
  sheet <- as.character(sheet)
  if (!sheet %in% sheets) {
    stop(
      sprintf(
        "Excel sheet '%s' not found. Available: %s.",
        sheet, paste(sheets, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  sheet
}

#' Read an uploaded data file
#'
#' @param skip_after_header Extra rows to drop after the header row (e.g. `1`
#'   for SoSci Survey exports that put column descriptions on row 2).
#' @param sheet Excel sheet name or 1-based index (Excel only).
#' @keywords internal
read_data_file <- function(path, name = path, skip_after_header = 0L, sheet = 1) {
  skip_after_header <- suppressWarnings(as.integer(skip_after_header))
  if (length(skip_after_header) != 1L || is.na(skip_after_header) || skip_after_header < 0L) {
    skip_after_header <- 0L
  }
  ext <- tolower(tools::file_ext(name))

  if (ext %in% c("csv", "txt", "tsv")) {
    delim <- if (ext == "tsv") "\t" else ","
    # sniff semicolon
    if (ext == "csv") {
      first <- readLines(path, n = 1, warn = FALSE)
      if (length(first) && grepl(";", first) && !grepl(",", first)) delim <- ";"
    }
    if (skip_after_header > 0L) {
      # Keep row-1 names; skip description rows before type-guessing data
      nms <- names(readr::read_delim(
        path, delim = delim, n_max = 0L,
        show_col_types = FALSE, progress = FALSE
      ))
      dat <- readr::read_delim(
        path,
        delim = delim,
        skip = 1L + skip_after_header,
        col_names = nms,
        show_col_types = FALSE,
        progress = FALSE
      )
    } else {
      dat <- readr::read_delim(
        path, delim = delim, show_col_types = FALSE, progress = FALSE
      )
    }
    return(as.data.frame(dat, stringsAsFactors = FALSE))
  }

  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Install the 'readxl' package to load Excel files.", call. = FALSE)
    }
    sh <- resolve_excel_sheet(path, sheet)
    header <- as.data.frame(
      suppressMessages(readxl::read_excel(
        path, sheet = sh, n_max = 1L, col_names = FALSE,
        .name_repair = "minimal"
      )),
      stringsAsFactors = FALSE
    )
    if (!ncol(header)) stop("Excel sheet is empty.", call. = FALSE)
    dat <- as.data.frame(
      suppressMessages(readxl::read_excel(
        path,
        sheet = sh,
        skip = 1L + skip_after_header,
        col_names = FALSE,
        .name_repair = "minimal"
      )),
      stringsAsFactors = FALSE
    )
    if (!ncol(dat) || !nrow(dat)) {
      stop("No data rows found after the header / skipped rows.", call. = FALSE)
    }
    names(dat) <- clean_header_names(unlist(header[1, ], use.names = FALSE), ncol(dat))
    return(dat)
  }

  if (ext == "rds") {
    if (skip_after_header > 0L) {
      stop("skip_after_header applies to CSV / TSV / Excel, not RDS.", call. = FALSE)
    }
    obj <- readRDS(path)
    if (!is.data.frame(obj)) stop("RDS object must be a data frame.", call. = FALSE)
    return(as.data.frame(obj, stringsAsFactors = FALSE))
  }

  stop(
    "Unsupported file type. Use CSV, TSV, TXT, Excel (.xlsx / .xls), or RDS.",
    call. = FALSE
  )
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

#' Browseable DT for data / prepare preview (truncated cells + scroll)
#'
#' Long cell text is shortened for display; the full value is available as a
#' native browser tooltip (`title`). Table scrolls inside the center pane.
#' @keywords internal
smt_browse_datatable <- function(dat, page_length = 15L, max_chars = 24L) {
  if (is.null(dat)) return(NULL)
  max_chars <- as.integer(max_chars)
  if (!is.finite(max_chars) || max_chars < 8L) max_chars <- 24L
  # Escape + truncate in JS so tooltips keep the full raw cell text
  truncate_js <- sprintf(
    paste0(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') return data;",
      "  if (data === null || data === undefined) return '';",
      "  var s = String(data);",
      "  function esc(t) {",
      "    return t.replace(/&/g,'&amp;').replace(/</g,'&lt;')",
      "      .replace(/>/g,'&gt;').replace(/\"/g,'&quot;');",
      "  }",
      "  if (s.length <= %d) return esc(s);",
      "  return '<span class=\"smt-cell-trunc\" title=\"' + esc(s) + '\">' +",
      "    esc(s.slice(0, %d)) + '\\u2026</span>';",
      "}"
    ),
    max_chars,
    max_chars
  )
  DT::datatable(
    dat,
    rownames = FALSE,
    escape = FALSE,
    class = "compact stripe hover smt-browse-table",
    options = list(
      pageLength = as.integer(page_length),
      scrollX = TRUE,
      scrollY = "calc(100vh - 240px)",
      scrollCollapse = TRUE,
      autoWidth = FALSE,
      dom = "tip",
      columnDefs = list(list(
        targets = "_all",
        render = DT::JS(truncate_js)
      ))
    )
  )
}
