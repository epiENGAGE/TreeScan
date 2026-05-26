# Testing and Coverage

This repository uses `testthat` and `covr` to ensure code is tested on every push and new features/bug fixes do not introduce new bugs.

R package dependencies for the current test-backed code live in the top-level
`DESCRIPTION` file. GitHub Actions installs the hard dependencies from that
file plus `Config/Needs/ci`, then caches the installed package library for
later runs.

Test files are named to match the production script they cover. For example,
`test-3_create_count_file.R` covers `treescan_project/code/3_create_count_file.R`.

Run all tests locally:

```sh
cd tests
Rscript testthat.R
```

Run coverage locally:

```sh
cd tests
Rscript coverage.R
```

This writes `coverage/coverage-summary.json` for the GitHub coverage badge.
It also updates the local SVG badge at `tests/badges/coverage-total.svg`.
On pushes to the default branch, GitHub Actions regenerates and commits that
badge SVG.
For a local HTML drill-down report and a refreshed local coverage badge SVG, run:

```sh
cd tests
TREESCAN_COVERAGE_HTML=true Rscript coverage.R
```

The HTML report is generated at `coverage-report/index.html`. The top-level
README badge points to `tests/badges/coverage-total.svg`, which is updated by
`tests/coverage.R`.

By default, coverage is reported without enforcing a minimum. To fail when
coverage is below a threshold, set `COVERAGE_THRESHOLD`:

```sh
cd tests
COVERAGE_THRESHOLD=80 Rscript coverage.R
```

GitHub Actions runs tests and reports coverage on every push and pull request.
The coverage script discovers every R script in `treescan_project/code/`.
Until the synthetic fixtures are complete, it runs tests against the currently
fixture-backed source file, but reports the percentage against the whole code
directory. Untested code files count as uncovered.

Keep synthetic fixtures under `tests/test_data`. After those fixtures can
exercise the full script set, enable strict all-script coverage with either:

```sh
cd tests
TREESCAN_COVERAGE_ALL=true Rscript coverage.R
```

or by adding `tests/test_data/.coverage-all`.

TreeScan itself is not required for routine CI. The one-rep Monte Carlo smoke
test is skipped unless a TreeScan binary is available at
`treescan_project/TS_linux/treescan64` or provided with `TREESCAN_BIN`.

The full pipeline writes local system metadata to
`treescan_project/system_metadata/session_info_*.txt` when R, OS, or package
versions change. Those generated files are ignored by git because they describe
the machine that ran the pipeline.
