#' Statistic column names that APA sets in italics
#' @keywords internal
apa_stat_colnames <- function() {
  c("Estimate", "SE", "p", "pHolm", "pBonf", "pBH", "df", "t", "z", "F", "N", "n")
}

#' Whether a column name is a statistic label (italic in APA tables)
#' @keywords internal
apa_is_stat_col <- function(nm) {
  nm <- as.character(nm %||% "")
  nm %in% apa_stat_colnames()
}

#' HTML colnames with italic statistic headers for DT (`escape = FALSE`)
#' @keywords internal
apa_stat_col_html <- function(names) {
  names <- as.character(names %||% character())
  vapply(names, function(nm) {
    esc <- htmltools::htmlEscape(nm)
    if (apa_is_stat_col(nm)) sprintf("<em>%s</em>", esc) else esc
  }, character(1), USE.NAMES = FALSE)
}

#' APA 7–styled DT table for results / report analysis tables
#'
#' Booktabs chrome comes from CSS class `smt-apa-table`. Statistic headers are
#' italicized via HTML colnames (`escape = FALSE`).
#'
#' @param df Data frame to show.
#' @param page_length If > 0, enable paging with this length; otherwise `dom = "t"`.
#' @param options Extra DT options (merged over defaults).
#' @param ... Passed to [DT::datatable].
#' @keywords internal
smt_apa_datatable <- function(df, page_length = 0L, options = NULL, ...) {
  if (is.null(df)) df <- data.frame()
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df)) {
    names(df) <- apa_stat_col_html(names(df))
  }
  opts <- list(
    scrollX = TRUE,
    ordering = FALSE,
    autoWidth = FALSE
  )
  page_length <- as.integer(page_length %||% 0L)
  if (is.finite(page_length) && page_length > 0L) {
    opts$dom <- "tip"
    opts$pageLength <- page_length
    opts$paging <- TRUE
  } else {
    opts$dom <- "t"
    opts$paging <- FALSE
  }
  if (is.list(options) && length(options)) {
    opts <- utils::modifyList(opts, options)
  }
  DT::datatable(
    df,
    rownames = FALSE,
    escape = FALSE,
    class = "compact smt-apa-table",
    options = opts,
    ...
  )
}
