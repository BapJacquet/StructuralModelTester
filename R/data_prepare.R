#' Empty data-prepare recipe (filter / mutate steps or free script)
#' @keywords internal
empty_prepare_state <- function() {
  list(
    mode = "steps",
    steps = data.frame(
      id = character(),
      type = character(),
      expr = character(),
      enabled = logical(),
      stringsAsFactors = FALSE
    ),
    script = "",
    next_step = 1L,
    error = NA_character_
  )
}

#' Normalize / migrate a prepare list
#' @keywords internal
ensure_prepare_state <- function(prepare = NULL) {
  base <- empty_prepare_state()
  if (is.null(prepare) || !is.list(prepare)) return(base)
  mode <- as.character(prepare$mode %||% "steps")
  if (!mode %in% c("steps", "script")) mode <- "steps"
  steps <- prepare$steps
  if (is.null(steps)) {
    steps <- base$steps
  } else {
    steps <- as.data.frame(steps, stringsAsFactors = FALSE)
    need <- c("id", "type", "expr", "enabled")
    for (nm in need) {
      if (!nm %in% names(steps)) {
        steps[[nm]] <- if (nm == "enabled") TRUE else NA_character_
      }
    }
    if (!nrow(steps)) {
      steps <- base$steps
    } else {
      steps$id <- as.character(steps$id)
      steps$type <- as.character(steps$type)
      steps$expr <- as.character(steps$expr)
      steps$enabled <- as.logical(steps$enabled)
      miss_en <- is.na(steps$enabled)
      if (any(miss_en)) steps$enabled[miss_en] <- TRUE
      bad_type <- !steps$type %in% c("filter", "mutate")
      if (any(bad_type)) steps$type[bad_type] <- "filter"
      steps$expr[is.na(steps$expr)] <- ""
      steps <- steps[, need, drop = FALSE]
    }
  }
  script <- as.character(prepare$script %||% "")
  if (length(script) != 1L || is.na(script)) script <- ""
  next_step <- as.integer(prepare$next_step %||% 1L)
  if (!is.finite(next_step) || next_step < 1L) next_step <- 1L
  list(
    mode = mode,
    steps = steps,
    script = script,
    next_step = next_step,
    error = NA_character_
  )
}

#' Compact prepare for JSON
#' @keywords internal
prepare_for_json <- function(prepare) {
  prepare <- ensure_prepare_state(prepare)
  list(
    mode = prepare$mode,
    steps = prepare$steps,
    script = prepare$script,
    next_step = as.integer(prepare$next_step)
  )
}

#' Restore prepare from JSON payload piece
#' @keywords internal
prepare_from_list <- function(obj) {
  ensure_prepare_state(obj)
}

#' Add a prepare step
#' @keywords internal
add_prepare_step <- function(prepare, type = c("filter", "mutate"), expr = "") {
  prepare <- ensure_prepare_state(prepare)
  type <- match.arg(type)
  id <- paste0("step_", prepare$next_step)
  prepare$next_step <- prepare$next_step + 1L
  row <- data.frame(
    id = id,
    type = type,
    expr = as.character(expr %||% ""),
    enabled = TRUE,
    stringsAsFactors = FALSE
  )
  prepare$steps <- rbind(prepare$steps, row)
  prepare
}

#' Update a prepare step expression / type / enabled
#' @keywords internal
update_prepare_step <- function(prepare, step_id, type = NULL, expr = NULL,
                                enabled = NULL) {
  prepare <- ensure_prepare_state(prepare)
  idx <- match(as.character(step_id), prepare$steps$id)
  if (is.na(idx)) return(prepare)
  if (!is.null(type) && type %in% c("filter", "mutate")) {
    prepare$steps$type[idx] <- type
  }
  if (!is.null(expr)) prepare$steps$expr[idx] <- as.character(expr)
  if (!is.null(enabled)) prepare$steps$enabled[idx] <- isTRUE(enabled)
  prepare
}

#' Remove a prepare step
#' @keywords internal
remove_prepare_step <- function(prepare, step_id) {
  prepare <- ensure_prepare_state(prepare)
  prepare$steps <- prepare$steps[prepare$steps$id != as.character(step_id), , drop = FALSE]
  prepare
}

#' Move a prepare step up or down
#' @keywords internal
move_prepare_step <- function(prepare, step_id, direction = c("up", "down")) {
  prepare <- ensure_prepare_state(prepare)
  direction <- match.arg(direction)
  idx <- match(as.character(step_id), prepare$steps$id)
  if (is.na(idx)) return(prepare)
  n <- nrow(prepare$steps)
  if (!n) return(prepare)
  j <- if (identical(direction, "up")) idx - 1L else idx + 1L
  if (j < 1L || j > n) return(prepare)
  ord <- seq_len(n)
  ord[c(idx, j)] <- ord[c(j, idx)]
  prepare$steps <- prepare$steps[ord, , drop = FALSE]
  prepare
}

#' Environment for evaluating prepare expressions (columns + safe helpers)
#' @keywords internal
prepare_eval_env <- function(df) {
  env <- list2env(as.list(df), parent = baseenv())
  helpers <- list(
    log = base::log,
    log10 = base::log10,
    log1p = base::log1p,
    log2 = base::log2,
    sqrt = base::sqrt,
    exp = base::exp,
    abs = base::abs,
    pmin = base::pmin,
    pmax = base::pmax,
    round = base::round,
    signif = base::signif,
    floor = base::floor,
    ceiling = base::ceiling,
    mean = base::mean,
    median = stats::median,
    sd = stats::sd,
    var = stats::var,
    sum = base::sum,
    min = base::min,
    max = base::max,
    length = base::length,
    as.numeric = base::as.numeric,
    as.integer = base::as.integer,
    as.character = base::as.character,
    as.factor = base::as.factor,
    as.logical = base::as.logical,
    factor = base::factor,
    is.na = base::is.na,
    is.finite = base::is.finite,
    ifelse = base::ifelse,
    paste = base::paste,
    paste0 = base::paste0,
    grepl = base::grepl,
    sub = base::sub,
    gsub = base::gsub,
    trimws = base::trimws,
    scale = base::scale,
    `%in%` = base::`%in%`
  )
  for (nm in names(helpers)) assign(nm, helpers[[nm]], envir = env)
  env
}

#' Evaluate a single expression in a data-frame column environment
#' @keywords internal
eval_prepare_expr <- function(expr, df) {
  expr <- trimws(as.character(expr %||% ""))
  if (!nzchar(expr)) {
    stop("Empty expression.", call. = FALSE)
  }
  parsed <- tryCatch(
    parse(text = expr),
    error = function(e) stop("Parse error: ", e$message, call. = FALSE)
  )
  if (length(parsed) != 1L) {
    stop("Expression must be a single statement.", call. = FALSE)
  }
  env <- prepare_eval_env(df)
  tryCatch(
    eval(parsed, envir = env),
    error = function(e) stop(e$message, call. = FALSE)
  )
}

#' Parse mutate assignment `lhs = rhs` (first `=` only)
#' @keywords internal
parse_mutate_assignment <- function(expr) {
  expr <- as.character(expr %||% "")
  # Prefer `=` over `<-`
  if (grepl("<-", expr, fixed = TRUE) && !grepl("=", expr, fixed = TRUE)) {
    parts <- strsplit(expr, "<-", fixed = TRUE)[[1]]
  } else {
    parts <- strsplit(expr, "=", fixed = TRUE)[[1]]
  }
  if (length(parts) < 2L) {
    stop(
      "Mutate steps need `new_col = expression` (or `col = expression`).",
      call. = FALSE
    )
  }
  lhs <- trimws(parts[[1]])
  rhs <- trimws(paste(parts[-1], collapse = "="))
  if (!nzchar(lhs) || !nzchar(rhs)) {
    stop("Mutate steps need both a column name and an expression.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", lhs)) {
    stop(
      "Invalid column name '", lhs, "'. Use letters, digits, dots, underscores.",
      call. = FALSE
    )
  }
  list(lhs = lhs, rhs = rhs)
}

#' Apply one filter step
#' @keywords internal
apply_prepare_filter <- function(df, expr) {
  mask <- eval_prepare_expr(expr, df)
  if (!is.logical(mask)) {
    stop("Filter expression must return a logical vector.", call. = FALSE)
  }
  if (length(mask) == 1L) mask <- rep(mask, nrow(df))
  if (length(mask) != nrow(df)) {
    stop(
      "Filter length (", length(mask), ") does not match rows (", nrow(df), ").",
      call. = FALSE
    )
  }
  df[!is.na(mask) & mask, , drop = FALSE]
}

#' Apply one mutate step
#' @keywords internal
apply_prepare_mutate <- function(df, expr) {
  asg <- parse_mutate_assignment(expr)
  val <- eval_prepare_expr(asg$rhs, df)
  if (length(val) == 1L && nrow(df) > 1L) val <- rep(val, nrow(df))
  if (length(val) != nrow(df)) {
    stop(
      "Mutate result length (", length(val), ") does not match rows (", nrow(df), ").",
      call. = FALSE
    )
  }
  df[[asg$lhs]] <- val
  df
}

#' Apply ordered prepare steps
#' @keywords internal
apply_prepare_steps <- function(raw, prepare) {
  prepare <- ensure_prepare_state(prepare)
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  steps <- prepare$steps
  if (!nrow(steps)) return(df)
  for (i in seq_len(nrow(steps))) {
    if (!isTRUE(steps$enabled[i])) next
    typ <- steps$type[i]
    expr <- steps$expr[i]
    if (is.na(expr) || !nzchar(trimws(expr))) next
    df <- tryCatch(
      {
        if (identical(typ, "filter")) {
          apply_prepare_filter(df, expr)
        } else {
          apply_prepare_mutate(df, expr)
        }
      },
      error = function(e) {
        stop(
          sprintf("Step %d (%s): %s", i, typ, e$message),
          call. = FALSE
        )
      }
    )
  }
  df
}

#' Apply free-script prepare mode
#' @keywords internal
apply_prepare_script <- function(raw, prepare) {
  prepare <- ensure_prepare_state(prepare)
  script <- trimws(prepare$script %||% "")
  if (!nzchar(script)) {
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }
  parsed <- tryCatch(
    parse(text = script),
    error = function(e) stop("Script parse error: ", e$message, call. = FALSE)
  )
  env <- new.env(parent = baseenv())
  env$data <- as.data.frame(raw, stringsAsFactors = FALSE)
  env$`.` <- env$data
  # Safe helpers also available by name
  helpers_env <- prepare_eval_env(env$data)
  for (nm in ls(helpers_env, all.names = TRUE)) {
    if (!exists(nm, envir = env, inherits = FALSE)) {
      assign(nm, get(nm, envir = helpers_env, inherits = FALSE), envir = env)
    }
  }
  # Common base utilities for scripts
  env$subset <- base::subset
  env$transform <- base::transform
  env$within <- base::within
  env$with <- base::with
  env$cbind <- base::cbind
  env$rbind <- base::rbind
  env$data.frame <- base::data.frame
  env$as.data.frame <- base::as.data.frame
  env$names <- base::names
  env$nrow <- base::nrow
  env$ncol <- base::ncol
  env$seq_len <- base::seq_len
  env$seq_along <- base::seq_along
  env$which <- base::which
  env$complete.cases <- stats::complete.cases
  env$na.omit <- stats::na.omit

  result <- tryCatch(
    eval(parsed, envir = env),
    error = function(e) stop("Script error: ", e$message, call. = FALSE)
  )
  if (!is.data.frame(result)) {
    stop("Script must return a data frame (got ", class(result)[[1]], ").", call. = FALSE)
  }
  as.data.frame(result, stringsAsFactors = FALSE)
}

#' Apply prepare recipe to raw data (throws on failure)
#' @keywords internal
apply_prepare <- function(raw, prepare) {
  prepare <- ensure_prepare_state(prepare)
  if (is.null(raw)) return(NULL)
  if (identical(prepare$mode, "script")) {
    apply_prepare_script(raw, prepare)
  } else {
    apply_prepare_steps(raw, prepare)
  }
}

#' Try apply; returns list(data, error)
#' @keywords internal
try_apply_prepare <- function(raw, prepare) {
  prepare <- ensure_prepare_state(prepare)
  if (is.null(raw)) {
    return(list(data = NULL, error = NA_character_, prepare = prepare))
  }
  out <- tryCatch(
    apply_prepare(raw, prepare),
    error = function(e) e
  )
  if (inherits(out, "error") || inherits(out, "simpleError")) {
    prepare$error <- conditionMessage(out)
    return(list(data = NULL, error = prepare$error, prepare = prepare))
  }
  prepare$error <- NA_character_
  list(data = out, error = NA_character_, prepare = prepare)
}

#' True when prepare has any actionable content
#' @keywords internal
prepare_is_active <- function(prepare) {
  prepare <- ensure_prepare_state(prepare)
  if (identical(prepare$mode, "script")) {
    return(nzchar(trimws(prepare$script %||% "")))
  }
  if (!nrow(prepare$steps)) return(FALSE)
  any(
    prepare$steps$enabled &
      !is.na(prepare$steps$expr) &
      nzchar(trimws(prepare$steps$expr))
  )
}
