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

#' HTML colnames with clickable help affordance for path-coef DT headers
#'
#' Wraps each header in `.smt-col-help` with `data-smt-help-col` = original
#' column name. Clicks are handled in `smt-panels.js` → `input$coef_help_col`.
#' @keywords internal
apa_coef_help_col_html <- function(names) {
  names <- as.character(names %||% character())
  vapply(names, function(nm) {
    esc <- htmltools::htmlEscape(nm)
    lab <- if (apa_is_stat_col(nm)) sprintf("<em>%s</em>", esc) else esc
    attr <- htmltools::htmlEscape(nm, attribute = TRUE)
    sprintf(
      paste0(
        '<span class="smt-col-help" role="button" tabindex="0" ',
        'title="Help: %s" data-smt-help-col="%s">%s</span>'
      ),
      attr, attr, lab
    )
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
#' @param header_help If TRUE, make headers clickable for path-coef column help.
#' @param ... Passed to [DT::datatable].
#' @keywords internal
smt_apa_datatable <- function(df, page_length = 0L, options = NULL,
                              header_help = FALSE, ...) {
  if (is.null(df)) df <- data.frame()
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df)) {
    nms <- names(df)
    names(df) <- if (isTRUE(header_help)) {
      apa_coef_help_col_html(nms)
    } else {
      apa_stat_col_html(nms)
    }
  }
  opts <- list(
    # No DT scrollX — a CSS wrapper scrolls only when content overflows.
    # (scrollX always mounts a scroll body that often shows a bar when unused.)
    scrollX = FALSE,
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
  cls <- "compact smt-apa-table"
  if (isTRUE(header_help)) cls <- paste(cls, "smt-apa-table-colhelp")
  DT::datatable(
    df,
    rownames = FALSE,
    escape = FALSE,
    class = cls,
    options = opts,
    ...
  )
}
