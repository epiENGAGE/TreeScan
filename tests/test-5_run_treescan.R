locate_test_treescan_binary <- function(project_root) {
  # Reuse the production locator so CI and local tests exercise the same binary
  # selection rules. On GitHub Linux this should pick TS_linux/treescan64; on a
  # Mac laptop it should pick TS_linux/treescan.
  env <- new.env(parent = globalenv())
  env$parent_dir <- file.path(project_root, "treescan_project")
  env$base_dir <- project_root
  env$server <- TRUE
  sys.source(
    file.path(project_root, "treescan_project", "code", "0_locate_treescan.R"),
    envir = env
  )
  env$treescan_bin
}

skip_if_treescan_not_runnable <- function(treescan_bin) {
  # The repo can contain local copies for multiple operating systems. Skip the
  # smoke test when the discovered file cannot execute on the current runner.
  file_info <- tryCatch(
    system2("file", treescan_bin, stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  file_info <- paste(file_info, collapse = "\n")
  sysname <- Sys.info()[["sysname"]]

  testthat::skip_if(
    identical(sysname, "Linux") && nzchar(file_info) && !grepl("ELF", file_info),
    paste("TreeScan binary is not a Linux executable:", file_info)
  )

  testthat::skip_if(
    identical(sysname, "Darwin") && nzchar(file_info) && !grepl("Mach-O", file_info),
    paste("TreeScan binary is not a macOS executable:", file_info)
  )

  quarantine <- tryCatch(
    suppressWarnings(
      system2("xattr", c("-p", "com.apple.quarantine", treescan_bin), stdout = TRUE, stderr = TRUE)
    ),
    error = function(e) character()
  )
  quarantine_status <- attr(quarantine, "status")
  if (is.null(quarantine_status)) {
    quarantine_status <- 0
  }

  # xattr exits non-zero when the quarantine attribute is absent, which is good.
  # Only skip when the attribute lookup succeeds and returns a value.
  testthat::skip_if(
    identical(sysname, "Darwin") && quarantine_status == 0 && length(quarantine) > 0,
    paste(
      "TreeScan binary is quarantined by macOS.",
      "Remove the quarantine attribute before running the smoke test."
    )
  )
}

testthat::test_that("TreeScan binary can be discovered when available", {
  project_root <- Sys.getenv("TREESCAN_PROJECT_ROOT", unset = normalizePath(".."))
  treescan_bin <- tryCatch(
    locate_test_treescan_binary(project_root),
    error = function(e) ""
  )

  testthat::skip_if_not(
    nzchar(treescan_bin) && file.exists(treescan_bin),
    "TreeScan binary is not available locally."
  )

  testthat::expect_true(file.exists(treescan_bin))
  testthat::expect_true(file.access(treescan_bin, mode = 1) == 0)
})

testthat::test_that("synthetic TreeScan smoke test can run when binary is available", {
  project_root <- Sys.getenv("TREESCAN_PROJECT_ROOT", unset = normalizePath(".."))
  treescan_bin <- tryCatch(
    locate_test_treescan_binary(project_root),
    error = function(e) ""
  )

  testthat::skip_if_not(
    nzchar(treescan_bin) && file.exists(treescan_bin),
    "TreeScan binary is not available locally."
  )
  skip_if_treescan_not_runnable(treescan_bin)

  parent_dir <- tempfile("treescan-smoke-")
  dir.create(file.path(parent_dir, "params"), recursive = TRUE)
  dir.create(file.path(parent_dir, "data", "analysis_count_files"), recursive = TRUE)
  dir.create(file.path(parent_dir, "results"), recursive = TRUE)

  file.copy(
    file.path(project_root, "treescan_project", "data", "Tree_File_2026.csv"),
    file.path(parent_dir, "data", "Tree_File_2026.csv"),
    overwrite = TRUE
  )

  # The production do-not-evaluate file is large and TreeScan prints thousands
  # of node-expansion notices for it. The smoke test only verifies that the
  # binary can consume a valid synthetic project, so keep this fixture tiny.
  not_eval_path <- file.path(parent_dir, "data", "Do_not_evaluate_nodes.csv")
  writeLines("ignore,description", not_eval_path)
  file.copy(
    file.path(project_root, "tests", "test_data", "Count_File_synthetic.txt"),
    file.path(parent_dir, "data", "analysis_count_files", "Count_File_synthetic.txt"),
    overwrite = TRUE
  )

  env <- new.env(parent = globalenv())
  env$parent_dir <- parent_dir
  env$final_date <- as.Date("2025-08-20")
  env$number_processors <- 1
  env$subregion <- FALSE
  env$initial_lags <- integer()
  sys.source(
    file.path(project_root, "treescan_project", "code", "4_update_parameter_file.R"),
    envir = env
  )

  prm <- file.path(parent_dir, "params", "Parameter_File_synthetic.prm")
  results <- file.path(parent_dir, "results", "Results_synthetic.txt")
  env$update_prm_file(
    lag = 0,
    todays_date = as.Date("2025-08-20"),
    end_date = as.Date("2025-08-20"),
    parent_dir = parent_dir,
    subregion = FALSE,
    number_processors = 1,
    prm_template = file.path(project_root, "tests", "test_data", "Parameter_File.prm"),
    prm_out = prm,
    tree_filename = file.path(parent_dir, "data", "Tree_File_2026.csv"),
    count_filename = file.path(parent_dir, "data", "analysis_count_files", "Count_File_synthetic.txt"),
    results_filename = results,
    not_evaluated_nodes_file = not_eval_path,
    monte_carlo_replications = 1,
    restrict_evaluated_nodes = "n",
    early_termination_threshold = 1,
    randomization_seed = 12345678
  )

  output <- system2(
    treescan_bin,
    shQuote(prm),
    stdout = TRUE,
    stderr = TRUE,
    timeout = 60
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0
  }

  testthat::expect_equal(status, 0, info = paste(output, collapse = "\n"))
  testthat::expect_true(
    file.exists(results) || file.exists(sub("[.]txt$", ".csv", results)),
    info = paste("TreeScan output:", paste(output, collapse = "\n"))
  )
})
