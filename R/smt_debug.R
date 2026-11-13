#' Debug log path (project-local; gitignored)
#' @keywords internal
smt_debug_log_path <- function() {
  cached <- getOption("smt.debug.log", NULL)
  if (is.character(cached) && length(cached) == 1L && nzchar(cached)) {
    return(cached)
  }
  root <- NA_character_
  if (file.exists(file.path(getwd(), "DESCRIPTION"))) {
    desc <- tryCatch(read.dcf("DESCRIPTION")[, "Package"], error = function(e) "")
    if (identical(as.character(desc), "StructuralModelTester")) {
      root <- normalizePath(getwd())
    }
  }
  if (is.na(root) || !nzchar(root)) {
    root <- tryCatch(get_dev_pkg_root(), error = function(e) normalizePath(getwd()))
  }
  path <- file.path(root, "smt-debug.log")
  options(smt.debug.log = path)
  path
}

#' Append a structured debug line (also prints to console)
#' @keywords internal
smt_debug <- function(...) {
  if (isTRUE(getOption("smt.quiet", FALSE))) return(invisible(NULL))
  msg <- paste0(...)
  line <- sprintf(
    "%s | %s",
    format(Sys.time(), "%H:%M:%OS3"),
    msg
  )
  path <- tryCatch(smt_debug_log_path(), error = function(e) NULL)
  if (!is.null(path)) {
    tryCatch(
      cat(line, "\n", file = path, append = TRUE),
      error = function(e) message("[smt-debug:file-error] ", e$message)
    )
  }
  message("[smt-debug] ", msg)
  invisible(line)
}

#' Compact R object for logs
#' @keywords internal
smt_debug_fmt <- function(x, max_len = 240L) {
  txt <- tryCatch({
    if (is.null(x)) return("NULL")
    if (is.atomic(x) && length(x) <= 8L) {
      return(paste(capture.output(str(x, give.head = FALSE, width = 80)), collapse = " "))
    }
    paste(utils::capture.output(utils::str(x, max.level = 2, list.len = 8)), collapse = " | ")
  }, error = function(e) paste0("<err:", e$message, ">"))
  if (nchar(txt) > max_len) paste0(substr(txt, 1L, max_len), "…") else txt
}
