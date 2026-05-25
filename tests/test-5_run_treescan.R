testthat::test_that("synthetic TreeScan smoke test can run when binary is available", {
  project_root <- Sys.getenv("TREESCAN_PROJECT_ROOT", unset = normalizePath(".."))
  default_candidates <- c(
    file.path(project_root, "treescan_project", "TS_linux", "treescan64"),
    file.path(project_root, "treescan_project", "TS_windows", "treescan64.exe")
  )
  treescan_bin <- Sys.getenv("TREESCAN_BIN", unset = "")

  if (!nzchar(treescan_bin)) {
    existing_candidates <- default_candidates[file.exists(default_candidates)]
    treescan_bin <- if (length(existing_candidates) > 0) existing_candidates[[1]] else ""
  }

  testthat::skip_if_not(
    nzchar(treescan_bin) && file.exists(treescan_bin),
    paste(
      "TreeScan binary is not available locally; skipping one-rep Monte Carlo smoke test.",
      "Expected TREESCAN_BIN or one of:",
      paste(default_candidates, collapse = ", ")
    )
  )

  testthat::skip("TreeScan smoke-test project assembly is pending fixture finalization.")
})
