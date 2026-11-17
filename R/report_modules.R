#' Registry of pin-able report module types
#'
#' @return Named list of lists with `id`, `label`, `needs`
#'   (`needs` is a character vector among `result`, `dv`, `edge`, `power`).
#' @keywords internal
report_module_types <- function() {
  list(
    coef_table = list(
      id = "coef_table",
      label = "Path coefficients",
      needs = "result"
    ),
    equations = list(
      id = "equations",
      label = "Model fit equations",
      needs = "result"
    ),
    residual = list(
      id = "residual",
      label = "Residual diagnostics",
      needs = c("result", "dv")
    ),
    edge_plot = list(
      id = "edge_plot",
      label = "Conditional / marginal effect",
      needs = c("edge")
    ),
    power = list(
      id = "power",
      label = "Power analysis",
      needs = "power"
    )
  )
}

#' Look up one report module type
#' @keywords internal
report_module_type_info <- function(type) {
  types <- report_module_types()
  type <- as.character(type %||% "")
  if (!nzchar(type) || !type %in% names(types)) return(NULL)
  types[[type]]
}

#' Empty report module list
#' @keywords internal
empty_report_modules <- function() {
  list()
}

#' New unique module instance id
#' @keywords internal
report_new_uid <- function(existing = character()) {
  existing <- as.character(existing %||% character())
  for (i in seq_len(1000L)) {
    uid <- paste0(
      "rm",
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      "_",
      sample.int(1e6L, 1L),
      "_",
      i
    )
    uid <- gsub("[^A-Za-z0-9_]", "", uid)
    if (!uid %in% existing) return(uid)
  }
  paste0("rm", sample.int(1e9L, 1L))
}

#' Normalize params list for a module type
#' @keywords internal
report_module_params <- function(type, params = NULL) {
  info <- report_module_type_info(type)
  if (is.null(info)) return(list())
  params <- as.list(params %||% list())
  out <- list()
  if ("dv" %in% info$needs) {
    dv <- as.character(params$dv_id %||% params$dv %||% NA_character_)
    if (!is.na(dv) && nzchar(dv)) out$dv_id <- dv
  }
  if ("edge" %in% info$needs) {
    eid <- as.character(params$edge_id %||% params$edge %||% NA_character_)
    if (!is.na(eid) && nzchar(eid)) out$edge_id <- eid
  }
  out
}

#' Whether params satisfy the type's needs (structure only; not live data)
#' @keywords internal
report_module_params_ok <- function(type, params = NULL) {
  info <- report_module_type_info(type)
  if (is.null(info)) return(FALSE)
  p <- report_module_params(type, params)
  if ("dv" %in% info$needs &&
        (is.null(p$dv_id) || !nzchar(as.character(p$dv_id)))) {
    return(FALSE)
  }
  if ("edge" %in% info$needs &&
        (is.null(p$edge_id) || !nzchar(as.character(p$edge_id)))) {
    return(FALSE)
  }
  TRUE
}

#' Add a module instance to the report list
#'
#' @param modules Existing list of modules.
#' @param type Module type id from [report_module_types()].
#' @param params Optional pin-time params (`dv_id`, `edge_id`).
#' @return Updated modules list (or unchanged if type/params invalid).
#' @keywords internal
report_add_module <- function(modules, type, params = NULL) {
  modules <- modules %||% empty_report_modules()
  info <- report_module_type_info(type)
  if (is.null(info)) return(modules)
  p <- report_module_params(type, params)
  if (!report_module_params_ok(type, p)) return(modules)
  uids <- vapply(modules, function(m) as.character(m$uid %||% ""), character(1))
  uid <- report_new_uid(uids)
  modules[[length(modules) + 1L]] <- list(
    uid = uid,
    type = info$id,
    label = info$label,
    params = p
  )
  modules
}

#' Remove a module by uid
#' @keywords internal
report_remove_module <- function(modules, uid) {
  modules <- modules %||% empty_report_modules()
  uid <- as.character(uid %||% "")
  if (!nzchar(uid) || !length(modules)) return(modules)
  keep <- vapply(modules, function(m) {
    !identical(as.character(m$uid %||% ""), uid)
  }, logical(1))
  modules[keep]
}

#' Find modules matching type (+ optional dv/edge params)
#' @keywords internal
report_find_modules <- function(modules, type, params = NULL) {
  modules <- modules %||% empty_report_modules()
  type <- as.character(type %||% "")
  if (!nzchar(type) || !length(modules)) return(list())
  want <- report_module_params(type, params)
  Filter(function(m) {
    if (!identical(as.character(m$type %||% ""), type)) return(FALSE)
    if (!length(want)) return(TRUE)
    mp <- m$params %||% list()
    for (nm in names(want)) {
      if (!identical(as.character(mp[[nm]] %||% ""), as.character(want[[nm]]))) {
        return(FALSE)
      }
    }
    TRUE
  }, modules)
}

#' Whether a matching module is already pinned
#' @keywords internal
report_has_module <- function(modules, type, params = NULL) {
  length(report_find_modules(modules, type, params)) > 0L
}

#' Display title for a module instance
#' @keywords internal
report_module_title <- function(mod, state = NULL) {
  if (is.null(mod)) return("Module")
  lab <- as.character(mod$label %||% mod$type %||% "Module")
  p <- mod$params %||% list()
  if (!is.null(state) && !is.null(p$dv_id) && nzchar(as.character(p$dv_id))) {
    dv_lab <- tryCatch(
      node_display_name(state, p$dv_id),
      error = function(e) p$dv_id
    )
    return(sprintf("%s · %s", lab, dv_lab))
  }
  if (!is.null(state) && !is.null(p$edge_id) && nzchar(as.character(p$edge_id))) {
    eid <- as.character(p$edge_id)
    idx <- match(eid, state$edges$id)
    if (!is.na(idx)) {
      from_lab <- tryCatch(
        node_display_name(state, state$edges$from[[idx]]),
        error = function(e) state$edges$from[[idx]]
      )
      to_lab <- tryCatch(
        node_display_name(state, state$edges$to[[idx]]),
        error = function(e) state$edges$to[[idx]]
      )
      return(sprintf("%s · %s \u2192 %s", lab, from_lab, to_lab))
    }
  }
  lab
}

#' Compact +/− pin control for a reportable block
#'
#' @param input_id Shiny input id for the actionButton.
#' @param pinned Whether a matching module is already in the report.
#' @param enabled Whether pinning is currently possible.
#' @keywords internal
smt_report_pin_button <- function(input_id, pinned = FALSE, enabled = TRUE,
                                  title_add = "Add to Report",
                                  title_remove = "Remove from Report") {
  if (!isTRUE(enabled) && !isTRUE(pinned)) {
    return(shiny::tags$button(
      class = "smt-report-pin smt-report-pin-disabled",
      type = "button",
      disabled = NA,
      title = "Nothing to pin yet",
      shiny::span(class = "smt-report-pin-glyph", "+")
    ))
  }
  if (isTRUE(pinned)) {
    return(shiny::actionButton(
      input_id,
      label = shiny::span(class = "smt-report-pin-glyph", "\u2212"),
      class = "smt-btn smt-btn-ghost smt-report-pin smt-report-pin-on",
      title = title_remove
    ))
  }
  shiny::actionButton(
    input_id,
    label = shiny::span(class = "smt-report-pin-glyph", "+"),
    class = "smt-btn smt-btn-ghost smt-report-pin",
    title = title_add
  )
}

#' Section heading with optional report pin
#' @keywords internal
smt_report_section_head <- function(title, pin = NULL) {
  shiny::div(
    class = "smt-report-section-head",
    shiny::h4(title),
    pin
  )
}

#' Embed a JSON payload for synchronous clipboard JS (user-gesture safe)
#' @keywords internal
smt_json_script_tag <- function(id, value) {
  js <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA)
  js <- gsub("</", "<\\/", js, fixed = TRUE)
  htmltools::tags$script(
    type = "application/json",
    id = id,
    htmltools::HTML(js)
  )
}

#' Report module TeX / Word copy buttons (clipboard via embedded JSON + JS)
#' @keywords internal
smt_report_copy_buttons <- function(uid, tex = "", html_payload = NULL) {
  uid <- as.character(uid %||% "")
  tex_id <- paste0("smt-clip-tex-", uid)
  html_id <- paste0("smt-clip-html-", uid)
  if (is.null(html_payload)) {
    html_payload <- list(html = "", text = "")
  }
  htmltools::tagList(
    smt_json_script_tag(tex_id, as.character(tex %||% "")),
    smt_json_script_tag(html_id, html_payload),
    htmltools::tags$button(
      type = "button",
      class = "smt-btn smt-btn-ghost smt-report-tex",
      title = "Copy this module as LaTeX",
      onclick = sprintf("window.smtCopyTextFromEl('%s'); return false;", tex_id),
      htmltools::span(class = "smt-report-tex-label", "TeX")
    ),
    htmltools::tags$button(
      type = "button",
      class = "smt-btn smt-btn-ghost smt-report-tex smt-report-word",
      title = "Copy this module as HTML for pasting into Word",
      onclick = sprintf("window.smtCopyHtmlFromEl('%s'); return false;", html_id),
      htmltools::span(class = "smt-report-tex-label", "Word")
    )
  )
}
