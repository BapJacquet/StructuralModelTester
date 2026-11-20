test_that("omnibus→split after fit does not crash when edges have extra columns", {
  # Mimic post-Bayes / Holm edges: extra columns that add_edge template omits
  st <- StructuralModelTester:::empty_model_state()
  st <- StructuralModelTester:::add_node(st, "complexite_stimuli", kind = "observed")
  st <- StructuralModelTester:::add_node(st, "empan_mnesique", kind = "observed")
  st$nodes$observed_column[st$nodes$id == "complexite_stimuli"] <- "complexite_stimuli"
  st$nodes$observed_column[st$nodes$id == "empan_mnesique"] <- "empan_mnesique"
  st$nodes$sim_var_kind[st$nodes$id == "complexite_stimuli"] <- "categorical"
  st$nodes$factor_base[st$nodes$id == "complexite_stimuli"] <- "faible"
  st$nodes$factor_levels[st$nodes$id == "complexite_stimuli"] <-
    "faible\nmoyenne\nélevée"
  st <- StructuralModelTester:::set_node_cat_mode(
    st, "complexite_stimuli", "omnibus", data = NULL
  )
  st <- StructuralModelTester:::add_edge(
    st, "complexite_stimuli", "empan_mnesique", type = "confirmatory"
  )

  # Simulate fit residue (Bayesian HDI + Holm raw p)
  st$edges$ci_low <- -0.1
  st$edges$ci_high <- 0.2
  st$edges$includes_zero <- TRUE
  st$edges$pd <- 0.8
  st$edges$pvalue_raw <- 0.04
  st$edges$estimate <- 0.5
  st$edges$se <- 0.1
  st$edges$pvalue <- 0.04
  st$edges$status <- "included"

  dat <- data.frame(
    complexite_stimuli = c("faible", "moyenne", "élevée", "faible", "moyenne", "élevée"),
    empan_mnesique = c(40, 50, 60, 42, 55, 65),
    stringsAsFactors = FALSE
  )

  expect_error(
    st2 <- StructuralModelTester:::set_node_cat_mode(
      st, "complexite_stimuli", "split", data = dat
    ),
    NA
  )
  outs <- st2$edges[
    st2$edges$from == "complexite_stimuli" &
      st2$edges$to == "empan_mnesique" &
      !st2$edges$type %in% c("measurement", "definitional"),
    ,
    drop = FALSE
  ]
  expect_gte(nrow(outs), 2L)
  expect_true(all(nzchar(as.character(outs$level))))
  expect_false(any(is.na(outs$level) | !nzchar(as.character(outs$level))))
  # Extra fit columns survived on the new leveled rows (aligned NAs)
  expect_true("ci_low" %in% names(st2$edges))
  expect_true("pvalue_raw" %in% names(st2$edges))
})

test_that("rbind_edges aligns mismatched edge schemas", {
  edges <- data.frame(
    id = "e1", from = "A", to = "B", type = "confirmatory",
    level = NA_character_, to_port = NA_character_, to_level = NA_character_,
    rel_kind = "linear", status = "included",
    estimate = 1, se = 0.1, pvalue = 0.01, estimate_std = NA_real_,
    label = "C", prior_family = "normal",
    prior_mean = 0, prior_sd = 1,
    prior_mean2 = 0, prior_sd2 = 1,
    prior_mean3 = 0, prior_sd3 = 1,
    pvalue_raw = 0.01,
    ci_low = -1, ci_high = 2,
    stringsAsFactors = FALSE
  )
  new_row <- data.frame(
    id = "e2", from = "A", to = "B", type = "confirmatory",
    level = "moyenne", to_port = NA_character_, to_level = NA_character_,
    rel_kind = "linear", status = "pending",
    estimate = NA_real_, se = NA_real_, pvalue = NA_real_,
    estimate_std = NA_real_, label = "C·moyenne",
    prior_family = "normal",
    prior_mean = 0, prior_sd = 1,
    prior_mean2 = 0, prior_sd2 = 1,
    prior_mean3 = 0, prior_sd3 = 1,
    stringsAsFactors = FALSE
  )
  out <- StructuralModelTester:::rbind_edges(edges, new_row)
  expect_equal(nrow(out), 2L)
  expect_true(all(c("pvalue_raw", "ci_low", "ci_high") %in% names(out)))
  expect_true(is.na(out$pvalue_raw[[2]]))
  expect_identical(as.character(out$level[[2]]), "moyenne")
})
