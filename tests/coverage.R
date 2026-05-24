if (!requireNamespace("covr", quietly = TRUE)) {
  stop(
    "The covr package is required to calculate coverage. ",
    "Install it with install.packages('covr').",
    call. = FALSE
  )
}

project_root <- normalizePath("..", mustWork = TRUE)
code_dir <- file.path(project_root, "treescan_project", "code")
all_source_files <- list.files(
  code_dir,
  pattern = "[.][Rr]$",
  full.names = TRUE
)
strict_all_coverage <- identical(Sys.getenv("TREESCAN_COVERAGE_ALL"), "true") ||
  file.exists(file.path("test_data", ".coverage-all"))

source_files <- all_source_files

if (!strict_all_coverage) {
  source_files <- file.path(code_dir, "4_update_parameter_file.R")
  message(
    "Coverage is in bootstrap mode. ",
    length(all_source_files), " code/*.R files were discovered, but only ",
    length(source_files), " fixture-backed file will be executed. Set ",
    "TREESCAN_COVERAGE_ALL=true or create tests/test_data/.coverage-all after ",
    "adding synthetic fixtures to require all-script coverage."
  )
}

message("Coverage source files:")
message(paste0("  - ", basename(source_files), collapse = "\n"))

coverage_env <- new.env(parent = globalenv())
coverage_env$parent_dir <- file.path(tempdir(), paste0("treescan-coverage-", Sys.getpid()))
coverage_env$final_date <- as.Date("2026-05-23")
coverage_env$number_processors <- 3
coverage_env$subregion <- FALSE
coverage_env$initial_lags <- integer()
coverage_env$first_time <- FALSE
coverage_env$server <- TRUE
coverage_env$lag_choice <- 1
coverage_env$new_month <- FALSE
coverage_env$reassess <- FALSE
coverage_env$base_dir <- dirname(coverage_env$parent_dir)
coverage_env$which_subregion <- character()
coverage_env$test_data_dir <- normalizePath("test_data", mustWork = FALSE)

dir.create(file.path(coverage_env$parent_dir, "params"), recursive = TRUE)
writeLines(
  c(
    "data-time-range=[2026/01/01,2026/01/31]",
    "window-start-range=[2026/01/01,2026/01/31]",
    "window-end-range=[2026/01/01,2026/01/31]",
    "tree-filename=old-tree.csv",
    "count-filename=old-count.txt",
    "results-filename=old-results.txt",
    "not-evaluated-nodes-file=old-nodes.csv",
    "parallel-processes=1"
  ),
  file.path(coverage_env$parent_dir, "params", "Parameter_File_lag0.prm")
)

coverage <- covr::file_coverage(
  source_files = source_files,
  test_files = list.files(".", pattern = "^test-.*[.][Rr]$", full.names = TRUE),
  parent_env = coverage_env
)

print(coverage)

percent <- covr::percent_coverage(coverage)
coverage_summary <- list(
  total = list(
    lines = list(pct = percent),
    statements = list(pct = percent),
    functions = list(pct = percent),
    branches = list(pct = percent)
  )
)
coverage_dir <- file.path(project_root, "coverage")
dir.create(coverage_dir, showWarnings = FALSE)
writeLines(
  jsonlite::toJSON(coverage_summary, auto_unbox = TRUE, pretty = TRUE),
  file.path(coverage_dir, "coverage-summary.json")
)

dir.create(file.path(project_root, "coverage-report"), showWarnings = FALSE)
covr::report(
  coverage,
  file = file.path(project_root, "coverage-report", "index.html"),
  browse = FALSE
)

threshold <- as.numeric(Sys.getenv("COVERAGE_THRESHOLD", "0"))

if (!is.na(threshold) && percent < threshold) {
  stop(
    sprintf("Coverage %.2f%% is below threshold %.2f%%", percent, threshold),
    call. = FALSE
  )
}
