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
  expect_true(all(c("Path", "Level", "Estimate", "SE", "p", "pHolm") %in% names(tab)))
  expect_identical(names(tab)[1:2], c("Path", "Level"))
  expect_false("To" %in% names(tab))
  expect_false("From" %in% names(tab))
  expect_false("Type" %in% names(tab))
  expect_false("Status" %in% names(tab))

  conf_ids <- res$state$edges$id[
    res$state$edges$type == "confirmatory" &
      res$state$edges$id %in% StructuralModelTester:::analyzable_edge_ids(
        res$state, res$state$edges$id
      )
  ]
  expect_true(length(conf_ids) >= 1L)

  est <- res$estimates
  expect_true("pvalue_raw" %in% names(est))
  conf_est <- is.finite(as.numeric(est$pvalue_raw))
  expect_true(any(conf_est))

  has_holm <- nzchar(as.character(tab$pHolm %||% ""))
  expect_true(any(has_holm))
  indir <- startsWith(as.character(tab$Level), "via ")
  if (any(indir)) expect_true(all(!nzchar(tab$pHolm[indir])))

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

test_that("estimates_table rows are ordered by outcome then predictor", {
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
    res$state, estimates = res$estimates, conf_adjust = "none", result = res
  )
  expect_true(nrow(tab) >= 2L)
  sp <- StructuralModelTester:::path_arrow_split(tab$Path)
  expect_equal(sp$to, sort(sp$to, method = "radix"))
  split_from <- split(sp$from, sp$to, drop = TRUE)
  expect_true(all(vapply(split_from, function(x) {
    identical(x, sort(x, method = "radix"))
  }, logical(1))))
  yx <- tab[sp$to == "Y" & sp$from == "X", , drop = FALSE]
  if (nrow(yx) >= 2L) {
    lv <- as.character(yx$Level)
    expect_true(!nzchar(lv[[1]]))
    expect_true(any(startsWith(lv, "via ")))
    expect_identical(lv[[length(lv)]], "total")
    expect_match(yx$Path[startsWith(lv, "via ")][[1]], "\u219c", fixed = TRUE)
    expect_match(yx$Path[lv == "total"][[1]], "\u21d0", fixed = TRUE)
  }
})

test_that("BIC path table includes Σw for searched edges", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_aic_bic_search(st, dat, criterion = "bic")
  tab <- StructuralModelTester:::estimates_table(
    res$state,
    estimates = res$estimates,
    conf_adjust = "none",
    result = res
  )
  expect_true("Σw" %in% names(tab))
  expect_true("p" %in% names(tab))
  direct <- tab[!nzchar(as.character(tab$Level)), , drop = FALSE]
  expect_true(any(nzchar(as.character(direct[["Σw"]]))))
  expect_true(all(!nzchar(as.character(direct$p))))
  mx <- direct[direct$Path == StructuralModelTester:::path_arrow_label("M", "X"), , drop = FALSE]
  expect_true(nrow(mx) == 1L)
  expect_gt(as.numeric(mx[["Σw"]][[1]]), 0.5)
  via <- tab[startsWith(as.character(tab$Level), "via "), , drop = FALSE]
  expect_true(nrow(via) >= 1L)
  expect_true(any(nzchar(as.character(via[["Σw"]]))))
  expect_true(any(nzchar(as.character(via$p))))
  mx_sw <- as.numeric(mx[["Σw"]][[1]])
  ym <- direct[direct$Path == StructuralModelTester:::path_arrow_label("Y", "M"), , drop = FALSE]
  ym_sw <- as.numeric(ym[["Σw"]][[1]])
  via_sw <- as.numeric(via[["Σw"]][[1]])
  expect_lte(via_sw, min(mx_sw, ym_sw) + 1e-9)
  tot <- tab[tab$Level == "total", , drop = FALSE]
  expect_true(nrow(tot) >= 1L)
  expect_true(any(nzchar(as.character(tot[["Σw"]]))))
  expect_true(any(nzchar(as.character(tot$p))))
})

test_that("model-averaged via uses average of products not product of averages", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_aic_bic_search(st, dat, criterion = "bic")
  res$model_averaged <- TRUE
  st_avg <- res$state_averaged
  tab <- StructuralModelTester:::estimates_table(
    st_avg,
    estimates = NULL,
    conf_adjust = "none",
    result = res
  )
  via <- tab[
    startsWith(as.character(tab$Level), "via ") &
      tab$Path == StructuralModelTester:::path_arrow_label("Y", "X", "via M"),
  ]
  expect_true(nrow(via) >= 1L)
  e <- st_avg$edges
  lab <- function(id) StructuralModelTester:::node_display_name(st_avg, id)
  id_a <- e$id[vapply(e$from, lab, character(1)) == "X" &
                 vapply(e$to, lab, character(1)) == "M"][[1]]
  id_b <- e$id[vapply(e$from, lab, character(1)) == "M" &
                 vapply(e$to, lab, character(1)) == "Y"][[1]]
  avg <- StructuralModelTester:::full_average_mediation_product(
    res$comparison, res$coef_by_model, id_a, id_b, always_ids = res$always
  )
  expect_equal(as.numeric(via$Estimate[[1]]), avg$est, tolerance = 0.02)
  expect_true(is.finite(avg$weight_sum) && avg$weight_sum > 0)
  expect_equal(as.numeric(via[["Σw"]][[1]]), avg$weight_sum, tolerance = 0.01)
})

test_that("BIC best-model total equals direct + via M", {
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_aic_bic_search(st, dat, criterion = "bic")
  tab <- StructuralModelTester:::estimates_table(
    res$state,
    estimates = res$estimates,
    conf_adjust = "none",
    result = res
  )
  sp <- StructuralModelTester:::path_arrow_split(tab$Path)
  yx <- sp$to == "Y" & sp$from == "X"
  d <- tab[yx & !nzchar(as.character(tab$Level)), ]
  v <- tab[yx & startsWith(as.character(tab$Level), "via "), ]
  t <- tab[yx & tab$Level == "total", ]
  expect_equal(nrow(d), 1L)
  expect_true(nrow(v) >= 1L)
  expect_equal(nrow(t), 1L)
  expect_match(v$Path[[1]], "\u219c", fixed = TRUE)
  expect_match(t$Path[[1]], "\u21d0", fixed = TRUE)
  expect_equal(
    as.numeric(t$Estimate[[1]]),
    as.numeric(d$Estimate[[1]]) + sum(as.numeric(v$Estimate)),
    tolerance = 0.02
  )
  expect_false(any(sp$from %in% c("Z1", "Z2") & !nzchar(as.character(tab$Level))))
})

test_that("mediation table includes via M and total for classical path_demo", {
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
    conf_adjust = "none",
    result = res
  )
  via <- startsWith(as.character(tab$Level), "via ")
  expect_true(any(via))
  sp <- StructuralModelTester:::path_arrow_split(tab$Path)
  yx <- sp$to == "Y" & sp$from == "X"
  expect_true(any(tab$Level == "total" & yx))
  d <- tab[yx & !nzchar(as.character(tab$Level)), , drop = FALSE]
  v <- tab[yx & startsWith(as.character(tab$Level), "via "), , drop = FALSE]
  t <- tab[yx & tab$Level == "total", , drop = FALSE]
  expect_true(nrow(d) == 1L && nrow(v) >= 1L && nrow(t) == 1L)
  expect_match(v$Path[[1]], "\u219c", fixed = TRUE)
  expect_match(t$Path[[1]], "\u21d0", fixed = TRUE)
  expect_equal(
    as.numeric(t$Estimate[[1]]),
    as.numeric(d$Estimate[[1]]) + sum(as.numeric(v$Estimate)),
    tolerance = 0.02
  )
})

test_that("bayesian mediation table includes via M and total", {
  skip_if_not(
    StructuralModelTester:::bayes_backend_available(),
    "brms + Stan backend not available"
  )
  csv <- StructuralModelTester:::smt_sample_path("path_demo.csv")
  js <- StructuralModelTester:::smt_sample_path("path_demo_model.json")
  skip_if_not(nzchar(csv) && nzchar(js), "path_demo sample files missing")
  dat <- utils::read.csv(csv, stringsAsFactors = FALSE)
  st <- StructuralModelTester:::model_from_json(
    paste(readLines(js, warn = FALSE), collapse = "\n")
  )
  res <- StructuralModelTester:::fit_bayes_model(
    st, dat, structure = "independent",
    mark_nonsig = FALSE,
    chains = 1L, iter = 400L, warmup = 200L, seed = 2L
  )
  tab <- StructuralModelTester:::estimates_table(
    res$state,
    estimates = res$estimates,
    conf_adjust = "none",
    result = res
  )
  expect_true(any(grepl("% HDI$", names(tab))))
  expect_true(any(startsWith(as.character(tab$Level), "via ")))
  sp <- StructuralModelTester:::path_arrow_split(tab$Path)
  yx <- sp$to == "Y" & sp$from == "X"
  expect_true(any(tab$Level == "total" & yx))
  expect_true(any(grepl("\u21d0", tab$Path[yx & tab$Level == "total"], fixed = TRUE)))
  via <- tab[startsWith(as.character(tab$Level), "via ") &
               startsWith(as.character(tab$Path), "Y "), , drop = FALSE]
  expect_true(all(grepl("\u219c", via$Path, fixed = TRUE)))
  expect_true(all(is.finite(as.numeric(via$Estimate))))
  expect_true(all(nzchar(as.character(via[[grep("% HDI$", names(via))[[1]]]]))))
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
