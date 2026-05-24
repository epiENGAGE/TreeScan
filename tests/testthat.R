if (!requireNamespace("testthat", quietly = TRUE)) {
  stop(
    "The testthat package is required to run tests. ",
    "Install it with install.packages('testthat').",
    call. = FALSE
  )
}

Sys.setenv(TREESCAN_PROJECT_ROOT = normalizePath("..", mustWork = TRUE))
Sys.setenv(TREESCAN_TEST_DATA = normalizePath("test_data", mustWork = FALSE))

test_files <- list.files(".", pattern = "^test-.*[.][Rr]$", full.names = TRUE)

if (length(test_files) == 0) {
  stop("No test files found. Expected files named test-*.R in tests/.", call. = FALSE)
}

invisible(lapply(test_files, testthat::test_file))
