if (!requireNamespace("covr", quietly = TRUE)) {
  stop(
    "The covr package is required to calculate coverage. ",
    "Install it with install.packages('covr').",
    call. = FALSE
  )
}

source("dependencies.R", local = TRUE)

project_root <- normalizePath("..", mustWork = TRUE)
Sys.setenv(TREESCAN_PROJECT_ROOT = project_root)
Sys.setenv(TREESCAN_TEST_DATA = normalizePath("test_data", mustWork = FALSE))

code_dir <- file.path(project_root, "treescan_project", "code")

# Coverage has two file sets on purpose:
# 1) all_source_files is every R script in the production code directory and is
#    used for the final whole-code denominator, so untested files count as 0%.
# 2) source_files is the smaller set that covr may safely source during tests.
#    Many legacy pipeline scripts still execute downloads, dialogs, TreeScan, or
#    production file writes as soon as they are sourced. Until those files are
#    converted to testable functions, keep them out of source_files but still in
#    all_source_files so the coverage badge stays honest.
all_source_files <- list.files(
  code_dir,
  pattern = "[.][Rr]$",
  full.names = TRUE
)
strict_all_coverage <- identical(Sys.getenv("TREESCAN_COVERAGE_ALL"), "true") ||
  file.exists(file.path("test_data", ".coverage-all"))

source_files <- all_source_files

if (!strict_all_coverage) {
  source_files <- file.path(
    code_dir,
    c(
      "0.1_record_system_metadata.R",
      "3_create_count_file.R",
      "4_update_parameter_file.R"
    )
  )

  # Add a code file here only after it has synthetic fixtures and can be sourced
  # without external credentials, GUI prompts, long compute, or production data.
  # Once every script has that shape, set TREESCAN_COVERAGE_ALL=true or add
  # tests/test_data/.coverage-all and this bootstrap list can go away.
  message(
    "Coverage is in bootstrap mode. ",
    length(all_source_files), " code/*.R files were discovered, but only ",
    length(source_files), " fixture-backed files will be executed. Set ",
    "TREESCAN_COVERAGE_ALL=true or create tests/test_data/.coverage-all after ",
    "adding synthetic fixtures to require all-script coverage."
  )
}

message("Coverage source files:")
message(paste0("  - ", basename(source_files), collapse = "\n"))

# covr sources scripts in parent_env. These values mimic the globals expected by
# the current numbered scripts while pointing everything at temp/test locations.
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

measured_percent <- covr::percent_coverage(coverage)
message(sprintf(
  "Measured fixture-backed coverage: %.2f%% (%s of %s code/*.R files executed)",
  measured_percent,
  length(source_files),
  length(all_source_files)
))

count_code_lines <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  sum(nzchar(lines) & !grepl("^#", lines))
}

line_coverage <- covr::tally_coverage(coverage, by = "line")
line_coverage$filename <- normalizePath(line_coverage$filename, mustWork = FALSE)
line_coverage <- line_coverage[!duplicated(line_coverage[c("filename", "line")]), ]

source_files_norm <- normalizePath(all_source_files, mustWork = TRUE)
names(source_files_norm) <- basename(source_files_norm)

code_lines_by_file <- lapply(source_files_norm, function(path) {
  lines <- trimws(readLines(path, warn = FALSE))
  which(nzchar(lines) & !grepl("^#", lines))
})

covered_lines_by_file <- lapply(source_files_norm, function(path) {
  rows <- line_coverage[line_coverage$filename == path & line_coverage$value > 0, ]
  intersect(unique(rows$line), code_lines_by_file[[basename(path)]])
})

file_stats <- data.frame(
  file = basename(source_files_norm),
  path = unname(source_files_norm),
  statements = vapply(code_lines_by_file, length, integer(1)),
  covered = vapply(covered_lines_by_file, length, integer(1)),
  stringsAsFactors = FALSE
)
file_stats$missing <- file_stats$statements - file_stats$covered
file_stats$coverage <- ifelse(
  file_stats$statements == 0,
  100,
  file_stats$covered / file_stats$statements * 100
)
file_stats <- file_stats[order(file_stats$coverage, file_stats$file), ]

covered_lines <- sum(file_stats$covered)
total_code_lines <- sum(file_stats$statements)
percent <- if (total_code_lines == 0) 100 else covered_lines / total_code_lines * 100

message(sprintf(
  "Whole code-dir coverage: %.2f%% (%s covered lines / %s code lines across %s files)",
  percent,
  covered_lines,
  total_code_lines,
  length(all_source_files)
))

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

write_coverage_badge <- function(percent, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  label <- "coverage"
  value <- sprintf("%.2f%%", percent)
  color <- if (percent >= 80) "#4c1" else if (percent >= 50) "#dfb317" else "#e05d44"
  label_width <- 61
  value_width <- max(37, 7 * nchar(value) + 8)
  total_width <- label_width + value_width
  value_x <- label_width + value_width / 2

  svg <- sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="20" role="img" aria-label="%s: %s">\n',
      '  <title>%s: %s</title>\n',
      '  <linearGradient id="s" x2="0" y2="100%%">\n',
      '    <stop offset="0" stop-color="#bbb" stop-opacity=".1"/>\n',
      '    <stop offset="1" stop-opacity=".1"/>\n',
      '  </linearGradient>\n',
      '  <clipPath id="r"><rect width="%s" height="20" rx="3" fill="#fff"/></clipPath>\n',
      '  <g clip-path="url(#r)">\n',
      '    <rect width="%s" height="20" fill="#555"/>\n',
      '    <rect x="%s" width="%s" height="20" fill="%s"/>\n',
      '    <rect width="%s" height="20" fill="url(#s)"/>\n',
      '  </g>\n',
      '  <g fill="#fff" text-anchor="middle" font-family="Verdana,Geneva,DejaVu Sans,sans-serif" font-size="11">\n',
      '    <text x="30.5" y="15" fill="#010101" fill-opacity=".3">%s</text>\n',
      '    <text x="30.5" y="14">%s</text>\n',
      '    <text x="%.1f" y="15" fill="#010101" fill-opacity=".3">%s</text>\n',
      '    <text x="%.1f" y="14">%s</text>\n',
      '  </g>\n',
      '</svg>\n'
    ),
    total_width, label, value,
    label, value,
    total_width,
    label_width,
    label_width, value_width, color,
    total_width,
    label,
    label,
    value_x, value,
    value_x, value
  )

  writeLines(svg, path)
}

write_coverage_badge(percent, file.path(project_root, "tests", "badges", "coverage-total.svg"))

write_html_report <- identical(Sys.getenv("TREESCAN_COVERAGE_HTML"), "true")

if (write_html_report) {
  escape_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }

  coverage_class <- function(pct) {
    if (pct >= 80) "good" else if (pct >= 50) "warn" else "bad"
  }

  report_dir <- file.path(project_root, "coverage-report")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

  render_source_table <- function(path) {
    rel_file <- basename(path)
    raw_lines <- readLines(path, warn = FALSE)
    code_lines <- code_lines_by_file[[rel_file]]
    covered <- covered_lines_by_file[[rel_file]]
    missing <- setdiff(code_lines, covered)

    rendered_lines <- vapply(seq_along(raw_lines), function(i) {
      class <- if (i %in% covered) {
        "covered"
      } else if (i %in% missing) {
        "missing"
      } else {
        "neutral"
      }

      sprintf(
        '<tr class="%s"><td class="line">%s</td><td class="code"><pre><code>%s</code></pre></td></tr>',
        class,
        i,
        escape_html(raw_lines[[i]])
      )
    }, character(1))

    paste(
      '<table class="source"><tbody>',
      rendered_lines,
      "</tbody></table>",
      sep = "\n",
      collapse = "\n"
    )
  }

  rows <- apply(file_stats, 1, function(row) {
    pct <- as.numeric(row[["coverage"]])
    details_id <- paste0("source-", gsub("[^A-Za-z0-9_-]", "-", row[["file"]]))
    source_table <- render_source_table(row[["path"]])
    sprintf(
      paste0(
        '<tr>',
        '<td data-sort="%s"><button class="file-toggle" type="button" onclick="toggleSource(\'%s\')">%s</button></td>',
        '<td data-sort="%s">%s</td>',
        '<td data-sort="%s">%s</td>',
        '<td data-sort="%.6f" class="%s">%.2f%%</td>',
        '</tr>',
        '<tr id="%s" class="source-row">',
        '<td colspan="4"><div class="source-panel">%s</div></td>',
        '</tr>'
      ),
      escape_html(row[["file"]]),
      details_id,
      escape_html(row[["file"]]),
      row[["statements"]],
      row[["statements"]],
      row[["missing"]],
      row[["missing"]],
      pct,
      coverage_class(pct),
      pct,
      details_id,
      source_table
    )
  })

  css <- c(
    ":root { color-scheme: dark; --bg: #0d1117; --panel: #161b22; --border: #30363d; --text: #e6edf3; --muted: #8b949e; --link: #58a6ff; }",
    "body { background: var(--bg); color: var(--text); font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 0; }",
    ".page { margin: 2rem auto; max-width: 1200px; padding: 0 1.25rem; }",
    "h1 { font-size: 2.25rem; font-weight: 650; margin: 0 0 .5rem; }",
    "a { color: #7db7ff; text-decoration: none; }",
    "a:hover { text-decoration: underline; }",
    "table { border-collapse: collapse; width: 100%; background: var(--panel); border: 1px solid var(--border); border-radius: 6px; overflow: hidden; }",
    "th, td { border-bottom: 1px solid var(--border); padding: .55rem .75rem; text-align: right; }",
    "th:first-child, td:first-child { text-align: left; }",
    "th { color: #c9d1d9; background: #21262d; cursor: pointer; font-weight: 650; position: sticky; top: 0; }",
    "th:hover { background: #30363d; }",
    "th .sort { color: var(--muted); font-size: .85em; margin-left: .35rem; }",
    ".file-toggle { appearance: none; background: none; border: 0; color: var(--link); cursor: pointer; font: inherit; padding: 0; text-align: left; }",
    ".file-toggle:hover { text-decoration: underline; }",
    ".source-row { display: none; }",
    ".source-row.open { display: table-row; }",
    ".source-row > td { background: #0b1117; padding: 0; text-align: left; }",
    ".source-panel { border-top: 1px solid var(--border); max-height: 75vh; overflow: auto; }",
    ".good { color: #7bd88f; }",
    ".warn { color: #ffd166; }",
    ".bad { color: #ff6b6b; }",
    ".summary { color: var(--muted); margin: 0 0 1.25rem; }",
    ".source { font-size: 13px; }",
    ".source td { vertical-align: top; border-bottom: none; padding: 0; }",
    ".source .line { background: #0b1117; border-right: 1px solid var(--border); color: var(--muted); font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; padding: 0 .75rem; text-align: right; user-select: none; width: 4rem; }",
    ".source .code { text-align: left; }",
    ".source pre { margin: 0; min-height: 1.35rem; overflow: visible; padding: 0 .75rem; white-space: pre-wrap; }",
    ".source code { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; line-height: 1.35rem; tab-size: 2; }",
    ".source tr.covered { background: rgba(46, 160, 67, .18); }",
    ".source tr.missing { background: rgba(248, 81, 73, .20); }",
    ".source tr.neutral { color: #8b949e; }",
    ".source tr:hover { background: rgba(88, 166, 255, .14); }"
  )
  writeLines(css, file.path(report_dir, "style.css"))

  sort_js <- c(
    "function sortCoverageTable(columnIndex, type) {",
    "  const table = document.getElementById('coverage-table');",
    "  const tbody = table.tBodies[0];",
    "  const headers = table.tHead.rows[0].cells;",
    "  const currentDirection = headers[columnIndex].dataset.direction;",
    "  const direction = currentDirection",
    "    ? (currentDirection === 'asc' ? 'desc' : 'asc')",
    "    : (type === 'number' ? 'desc' : 'asc');",
    "  Array.from(headers).forEach((header) => {",
    "    header.dataset.direction = '';",
    "    const marker = header.querySelector('.sort');",
    "    if (marker) marker.textContent = '';",
    "  });",
    "  headers[columnIndex].dataset.direction = direction;",
    "  headers[columnIndex].querySelector('.sort').textContent = direction === 'asc' ? '▲' : '▼';",
    "  const groups = [];",
    "  for (let i = 0; i < tbody.rows.length; i += 1) {",
    "    const row = tbody.rows[i];",
    "    if (row.classList.contains('source-row')) continue;",
    "    const sourceRow = tbody.rows[i + 1]?.classList.contains('source-row') ? tbody.rows[i + 1] : null;",
    "    groups.push({ row, sourceRow });",
    "  }",
    "  groups.sort((a, b) => {",
    "    const aCell = a.row.cells[columnIndex];",
    "    const bCell = b.row.cells[columnIndex];",
    "    const aValue = aCell.dataset.sort || aCell.textContent;",
    "    const bValue = bCell.dataset.sort || bCell.textContent;",
    "    const result = type === 'number'",
    "      ? Number(aValue) - Number(bValue)",
    "      : aValue.localeCompare(bValue);",
    "    return direction === 'asc' ? result : -result;",
    "  });",
    "  groups.forEach(({ row, sourceRow }) => {",
    "    tbody.appendChild(row);",
    "    if (sourceRow) tbody.appendChild(sourceRow);",
    "  });",
    "}",
    "function toggleSource(id) {",
    "  const row = document.getElementById(id);",
    "  if (row) row.classList.toggle('open');",
    "}"
  )
  writeLines(sort_js, file.path(report_dir, "sort.js"))

  index <- c(
    "<!doctype html>",
    '<html lang="en">',
    "<head>",
    '<meta charset="utf-8">',
    "<title>TreeScan Coverage Report</title>",
    '<link rel="stylesheet" href="style.css">',
    '<script defer src="sort.js"></script>',
    "</head>",
    "<body>",
    '<main class="page">',
    sprintf("<h1>Coverage report: %.2f%%</h1>", percent),
    sprintf(
      '<p class="summary">%s covered lines / %s code lines across %s files</p>',
      covered_lines,
      total_code_lines,
      length(source_files_norm)
    ),
    '<table id="coverage-table">',
    paste0(
      "<thead><tr>",
      '<th onclick="sortCoverageTable(0, \'text\')">File<span class="sort"></span></th>',
      '<th onclick="sortCoverageTable(1, \'number\')">statements<span class="sort"></span></th>',
      '<th onclick="sortCoverageTable(2, \'number\')">missing<span class="sort"></span></th>',
      '<th onclick="sortCoverageTable(3, \'number\')">coverage<span class="sort"></span></th>',
      "</tr></thead>"
    ),
    "<tbody>",
    rows,
    "</tbody>",
    "</table>",
    "</main>",
    "</body>",
    "</html>"
  )
  writeLines(index, file.path(report_dir, "index.html"))
}

threshold <- as.numeric(Sys.getenv("COVERAGE_THRESHOLD", "0"))

if (!is.na(threshold) && percent < threshold) {
  stop(
    sprintf("Coverage %.2f%% is below threshold %.2f%%", percent, threshold),
    call. = FALSE
  )
}
