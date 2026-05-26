testthat::test_that("system metadata is only rewritten when environment changes", {
  project_root <- Sys.getenv("TREESCAN_PROJECT_ROOT", unset = normalizePath(".."))
  source_file <- file.path(
    project_root,
    "treescan_project",
    "code",
    "0.1_record_system_metadata.R"
  )

  env <- new.env(parent = globalenv())
  sys.source(source_file, envir = env)

  parent_dir <- tempfile("treescan-metadata-")
  dir.create(parent_dir)

  first_path <- env$record_system_metadata(parent_dir)
  testthat::skip_if(
    is.null(first_path),
    "sessioninfo package is not installed."
  )

  second_path <- env$record_system_metadata(parent_dir)

  metadata_files <- list.files(
    file.path(parent_dir, "system_metadata"),
    pattern = "^session_info_.*[.]txt$",
    full.names = TRUE
  )

  testthat::expect_true(file.exists(first_path))
  testthat::expect_equal(second_path, first_path)
  testthat::expect_length(metadata_files, 1)
  testthat::expect_match(basename(first_path), paste0("^session_info_", Sys.Date()))
})
