test_that("latex_escape_text escapes specials", {
  out <- StructuralModelTester:::latex_escape_text("A_b & C%")
  expect_match(out, "\\\\_")
  expect_match(out, "\\\\&")
  expect_match(out, "\\\\%")
  # Path glyphs must become TeX math (pdflatex cannot embed U+219C raw)
  arrows <- StructuralModelTester:::latex_escape_text(
    paste0("Y", "\u2190", "X · Y", "\u219c", "X · Y", "\u21d0", "X")
  )
  expect_match(arrows, "\\\\leftarrow", perl = TRUE)
  expect_match(arrows, "\\\\Leftarrow", perl = TRUE)
})

test_that("report_df_to_latex_table emits booktabs", {
  df <- data.frame(
    From = "X", To = "Y", Estimate = 0.5, SE = 0.1, p = ".012",
    stringsAsFactors = FALSE
  )
  tex <- StructuralModelTester:::report_df_to_latex_table(
    df, "Path coefficients", 1L
  )
  expect_match(tex, "\\\\begin\\{tabular\\}", perl = TRUE)
  expect_match(tex, "\\\\toprule", perl = TRUE)
  expect_match(tex, "\\\\bottomrule", perl = TRUE)
  expect_match(tex, "Path coefficients")
  expect_match(tex, "\\\\textit\\{Estimate\\}", perl = TRUE)
})

test_that("format_best_equations_latex returns display math", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  tex <- StructuralModelTester:::format_best_equations_latex(res, st)
  expect_true(is.character(tex) && nzchar(tex))
  expect_false(grepl("<div", tex, fixed = TRUE))
  expect_match(tex, "\\\\varepsilon|\\\\mathcal\\{N\\}", perl = TRUE)
})

test_that("smt_eq_html_to_latex rewrites boundary color", {
  html <- paste0(
    "<div class=\"smt-equations\">",
    "<div class=\"smt-eq-line\">\\(\\rho = \\color{#b33b3b}{1.00}\\)</div>",
    "</div>"
  )
  tex <- StructuralModelTester:::smt_eq_html_to_latex(html)
  expect_match(tex, "\\\\textcolor\\[HTML\\]\\{b33b3b\\}", perl = TRUE)
  expect_false(grepl("\\\\color\\{#", tex, perl = TRUE))
})

test_that("report_build_latex includes coef table module", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "none"
  )
  mods <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "coef_table"
  )
  mods <- StructuralModelTester:::report_add_module(mods, "equations")
  work <- tempfile("smt-rep-test-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  doc <- StructuralModelTester:::report_build_latex(
    mods, st, result = res, power = NULL, dir = work
  )
  expect_match(doc, "\\\\documentclass\\[man,floatsintext,12pt\\]\\{apa7\\}", perl = TRUE)
  expect_match(doc, "\\\\shorttitle", perl = TRUE)
  expect_match(doc, "Path coefficients")
  expect_match(doc, "\\\\begin\\{tabular\\}", perl = TRUE)
  expect_match(doc, "Model fit equations|Best model", perl = TRUE)
})

test_that("report_module_html_clipboard returns Word-pasteable HTML table", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  mod <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "coef_table"
  )[[1]]
  payload <- StructuralModelTester:::report_module_html_clipboard(
    mod, st, result = res, power = NULL
  )
  expect_true(is.list(payload))
  expect_match(payload$html, "<table", fixed = TRUE)
  expect_match(payload$html, "StartFragment", fixed = TRUE)
  expect_true(nzchar(payload$text))
  expect_false(grepl("<table", payload$text, fixed = TRUE))
})

test_that("report_module_tex_clipboard returns body without writing figures", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "holm"
  )
  mod <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "coef_table"
  )[[1]]
  tex <- StructuralModelTester:::report_module_tex_clipboard(
    mod, st, result = res, power = NULL
  )
  expect_match(tex, "\\\\section\\*", perl = TRUE)
  expect_match(tex, "\\\\begin\\{tabular\\}", perl = TRUE)
  expect_true("pHolm" %in% names(
    StructuralModelTester:::estimates_table(res$state, res$estimates, "holm")
  ))

  mod2 <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "residual",
    list(dv_id = unique(as.character(st$edges$to))[[1]])
  )[[1]]
  tex2 <- StructuralModelTester:::report_module_tex_clipboard(
    mod2, st, result = res, power = NULL
  )
  expect_match(tex2, "Figure file not embedded", fixed = TRUE)
  expect_match(tex2, "\\\\begin\\{figure\\}", perl = TRUE)
})

test_that("report_save_ggplot writes PDF via base device", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  work <- tempfile("smt-fig-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  rel <- StructuralModelTester:::report_save_ggplot(p, work, "demo", height = 3)
  expect_true(grepl("\\.(pdf|png)$", rel))
  expect_true(file.exists(file.path(work, rel)))
  expect_gt(file.info(file.path(work, rel))$size, 100)
})

test_that("report_build_pdf smoke with figure module (tinytex)", {
  skip_if_not(
    StructuralModelTester:::report_export_available(),
    "tinytex / LaTeX not available"
  )
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_pvalue_model(
    st, dat, alpha = 0.05, mark_nonsig = TRUE, conf_adjust = "none"
  )
  dv <- unique(as.character(st$edges$to))[[1]]
  mods <- StructuralModelTester:::report_add_module(
    StructuralModelTester:::empty_report_modules(),
    "coef_table"
  )
  mods <- StructuralModelTester:::report_add_module(
    mods, "residual", list(dv_id = dv)
  )
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)
  StructuralModelTester:::report_build_pdf(
    modules = mods,
    state = st,
    result = res,
    power = NULL,
    outfile = out,
    analysis_data = dat
  )
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 500)
})
