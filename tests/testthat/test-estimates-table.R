test_that("estimates_table shows raw p and confirmatory-only pHolm", {
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
  tab <- StructuralModelTester:::estimates_table(
    res$state,
    estimates = res$estimates,
    conf_adjust = "holm"
  )
  expect_true(all(c("From", "Level", "To", "Estimate", "SE", "p", "pHolm") %in% names(tab)))
  expect_false("Type" %in% names(tab))
  expect_false("Status" %in% names(tab))

  conf_ids <- res$state$edges$id[
    res$state$edges$type == "confirmatory" &
      res$state$edges$id %in% StructuralModelTester:::analyzable_edge_ids(
        res$state, res$state$edges$id
      )
  ]
  expect_true(length(conf_ids) >= 1L)

  # Confirmatory estimate rows should carry pvalue_raw and appear in pHolm
  est <- res$estimates
  expect_true("pvalue_raw" %in% names(est))
  conf_est <- is.finite(as.numeric(est$pvalue_raw))
  expect_true(any(conf_est))

  # Map table rows with non-blank pHolm
  has_holm <- nzchar(as.character(tab$pHolm %||% ""))
  expect_true(any(has_holm))
  # Indirect / non-adjusted rows blank
  indir <- startsWith(as.character(tab$Level), "via ")
  if (any(indir)) expect_true(all(!nzchar(tab$pHolm[indir])))

  # Where both present, raw p <= adjusted (Holm)
  both <- has_holm & nzchar(as.character(tab$p))
  if (any(both)) {
    parse_p <- function(x) {
      x <- as.character(x)
      ifelse(x %in% c("<.001", "<0.001"), 0.0005, suppressWarnings(as.numeric(x)))
    }
    raw_v <- parse_p(tab$p[both])
    adj_v <- parse_p(tab$pHolm[both])
    ok <- is.finite(raw_v) & is.finite(adj_v)
    expect_true(any(ok))
    expect_true(all(raw_v[ok] <= adj_v[ok] + 1e-9))
  }
})

test_that("estimates_table omits adjusted column when conf_adjust is none", {
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
  tab <- StructuralModelTester:::estimates_table(
    res$state,
    estimates = res$estimates,
    conf_adjust = "none"
  )
  expect_true("p" %in% names(tab))
  expect_false("pHolm" %in% names(tab))
  expect_false("Type" %in% names(tab))
})

test_that("apply_confirmatory_p_adjust preserves pvalue_raw on edges", {
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "X", kind = "observed")
  st <- StructuralModelTester:::add_node(st, "Y", kind = "observed")
  st <- StructuralModelTester:::add_node(st, "Z", kind = "observed")
  st <- StructuralModelTester:::add_edge(st, "X", "Y", type = "confirmatory")
  st <- StructuralModelTester:::add_edge(st, "Z", "Y", type = "confirmatory")
  st$edges$pvalue <- c(0.01, 0.04)
  st$edges$estimate <- c(0.5, 0.2)
  st$edges$se <- c(0.1, 0.1)
  st$edges$status <- "included"
  out <- StructuralModelTester:::apply_confirmatory_p_adjust(st, NULL, method = "holm")
  expect_true("pvalue_raw" %in% names(out$state$edges))
  expect_equal(out$state$edges$pvalue_raw, c(0.01, 0.04))
  expect_true(all(out$state$edges$pvalue >= out$state$edges$pvalue_raw - 1e-12))
})
