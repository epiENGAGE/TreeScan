testthat::test_that("update_prm_file updates dates, paths, and processor count", {
  parent_dir <- tempfile("treescan-test-")
  dir.create(file.path(parent_dir, "params"), recursive = TRUE, showWarnings = FALSE)
  dir.create(
    file.path(parent_dir, "data", "analysis_count_files"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  template <- c(
    "data-time-range=[2026/01/01,2026/01/31]",
    "window-start-range=[2026/01/01,2026/01/31]",
    "window-end-range=[2026/01/01,2026/01/31]",
    "tree-filename=old-tree.csv",
    "count-filename=old-count.txt",
    "results-filename=old-results.txt",
    "not-evaluated-nodes-file=old-nodes.csv",
    "parallel-processes=1"
  )
  writeLines(template, file.path(parent_dir, "params", "Parameter_File_lag0.prm"))

  env <- new.env(parent = globalenv())
  env$parent_dir <- parent_dir
  env$final_date <- as.Date("2026-05-23")
  env$number_processors <- 3
  env$subregion <- FALSE
  env$initial_lags <- integer()

  project_root <- Sys.getenv("TREESCAN_PROJECT_ROOT", unset = normalizePath(".."))
  source_file <- file.path(project_root, "treescan_project", "code", "4_update_parameter_file.R")

  update_prm_file_fn <- if (exists("update_prm_file", mode = "function")) {
    update_prm_file
  } else {
    sys.source(source_file, envir = env)
    env$update_prm_file
  }

  result <- update_prm_file_fn(
    lag = 2,
    todays_date = env$final_date,
    end_date = env$final_date - 2,
    parent_dir = parent_dir,
    subregion = FALSE,
    number_processors = 3
  )

  updated <- readLines(file.path(parent_dir, "params", "Parameter_File_lag2.prm"))

  testthat::expect_equal(result$start_date, as.Date("2026-02-20"))
  testthat::expect_equal(result$end_date, as.Date("2026-05-21"))
  testthat::expect_true("data-time-range=[2026/02/20,2026/05/21]" %in% updated)
  testthat::expect_true("window-start-range=[2026/02/20,2026/05/21]" %in% updated)
  testthat::expect_true("window-end-range=[2026/02/20,2026/05/21]" %in% updated)
  testthat::expect_true(
    paste0("tree-filename=", parent_dir, "/data/Tree_File_2026.csv") %in% updated
  )
  testthat::expect_true(
    paste0(
      "count-filename=",
      parent_dir,
      "/data/analysis_count_files/Analysis_Count_File_2026-05-23/lag2.txt"
    ) %in% updated
  )
  testthat::expect_true(
    paste0(
      "results-filename=",
      parent_dir,
      "/results/2026-05-23/Results_lag2_2026-05-23.txt"
    ) %in% updated
  )
  testthat::expect_true(
    paste0("not-evaluated-nodes-file=", parent_dir, "/data/Do_not_evaluate_nodes.csv") %in% updated
  )
  testthat::expect_true("parallel-processes=3" %in% updated)
})

testthat::test_that("synthetic TreeScan parameter template is rewritten to temp paths", {
  project_root <- Sys.getenv("TREESCAN_PROJECT_ROOT", unset = normalizePath(".."))
  test_data <- Sys.getenv("TREESCAN_TEST_DATA", unset = "test_data")

  parent_dir <- tempfile("treescan-synthetic-prm-")
  dir.create(file.path(parent_dir, "params"), recursive = TRUE)
  dir.create(file.path(parent_dir, "results"), recursive = TRUE)

  env <- new.env(parent = globalenv())
  env$parent_dir <- parent_dir
  env$final_date <- as.Date("2025-08-20")
  env$number_processors <- 1
  env$subregion <- FALSE
  env$initial_lags <- integer()

  update_prm_file_fn <- if (exists("update_prm_file", mode = "function")) {
    update_prm_file
  } else {
    sys.source(
      file.path(project_root, "treescan_project", "code", "4_update_parameter_file.R"),
      envir = env
    )
    env$update_prm_file
  }

  prm_out <- file.path(parent_dir, "params", "Parameter_File_synthetic.prm")
  tree_path <- file.path(parent_dir, "data", "Tree_File_2026.csv")
  count_path <- file.path(parent_dir, "data", "analysis_count_files", "Count_File_synthetic.txt")
  results_path <- file.path(parent_dir, "results", "Results_synthetic.txt")
  not_eval_path <- file.path(parent_dir, "data", "Do_not_evaluate_nodes.csv")

  update_prm_file_fn(
    lag = 0,
    todays_date = env$final_date,
    end_date = env$final_date,
    parent_dir = parent_dir,
    subregion = FALSE,
    number_processors = 1,
    prm_template = file.path(test_data, "Parameter_File.prm"),
    prm_out = prm_out,
    tree_filename = tree_path,
    count_filename = count_path,
    results_filename = results_path,
    not_evaluated_nodes_file = not_eval_path,
    monte_carlo_replications = 1,
    randomization_seed = 12345678
  )

  lines <- readLines(prm_out, warn = FALSE)

  testthat::expect_true(paste0("tree-filename=", tree_path) %in% lines)
  testthat::expect_true(paste0("count-filename=", count_path) %in% lines)
  testthat::expect_true(paste0("results-filename=", results_path) %in% lines)
  testthat::expect_true(paste0("not-evaluated-nodes-file=", not_eval_path) %in% lines)
  testthat::expect_true("monte-carlo-replications=1" %in% lines)
  testthat::expect_true("randomization-seed=12345678" %in% lines)
  testthat::expect_true("parallel-processes=1" %in% lines)
})
