# Testing and Coverage

This repository uses `testthat` and `covr` to ensure code is tested on every push and new features/bug fixes do not introduce new bugs.

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
On pushes to the default branch, GitHub Actions commits generated badge SVGs
under `tests/badges/`.
For a local HTML drill-down report, install `DT` and `htmltools`, then run:

```sh
cd tests
TREESCAN_COVERAGE_HTML=true Rscript coverage.R
```

The HTML report is generated at `coverage-report/index.html`.

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
