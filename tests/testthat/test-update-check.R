test_that("smt_parse_github_slug handles https and ssh remotes", {
  expect_equal(
    StructuralModelTester:::smt_parse_github_slug(
      "https://github.com/BapJacquet/StructuralModelTester.git"
    ),
    "BapJacquet/StructuralModelTester"
  )
  expect_equal(
    StructuralModelTester:::smt_parse_github_slug(
      "git@github.com:BapJacquet/StructuralModelTester.git"
    ),
    "BapJacquet/StructuralModelTester"
  )
  expect_true(is.na(StructuralModelTester:::smt_parse_github_slug("not-a-url")))
})

test_that("smt_version_is_newer compares package versions", {
  expect_true(StructuralModelTester:::smt_version_is_newer("0.2.4", "0.2.3"))
  expect_false(StructuralModelTester:::smt_version_is_newer("0.2.3", "0.2.3"))
  expect_false(StructuralModelTester:::smt_version_is_newer("0.2.2", "0.2.3"))
  expect_false(StructuralModelTester:::smt_version_is_newer(NA, "0.2.3"))
})

test_that("smt_update_check returns a structured list from the local clone", {
  root <- tryCatch(
    StructuralModelTester:::smt_find_pkg_root(),
    error = function(e) ""
  )
  skip_if_not(nzchar(root) && dir.exists(file.path(root, ".git")))

  info <- StructuralModelTester:::smt_update_check(
    timeout_sec = 12,
    root = root,
    local_version = "0.0.0.9000"
  )
  expect_type(info, "list")
  expect_true("update_available" %in% names(info))
  expect_true("message" %in% names(info))
  expect_true(is.logical(info$update_available))
  # Local 0.0.0.9000 should be older than anything on main, or at least
  # git-behind / version fields should be populated when online.
  expect_true(nzchar(info$message))
})
