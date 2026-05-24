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

This also writes an HTML drill-down report to `coverage-report/index.html`.
It also writes `coverage/coverage-summary.json` for the GitHub coverage badge.

By default, coverage is reported without enforcing a minimum. To fail when
coverage is below a threshold, set `COVERAGE_THRESHOLD`:

```sh
cd tests
COVERAGE_THRESHOLD=80 Rscript coverage.R
```

GitHub Actions runs tests and reports coverage on every push and pull request.
The coverage script discovers every R script in `treescan_project/code/`.
Until the synthetic fixtures are complete, it runs in bootstrap mode against
the currently fixture-backed source file. In bootstrap mode, the percentage is
coverage for that measured subset, not the full `treescan_project/code/`
directory.

Keep synthetic fixtures under `tests/test_data`. After those fixtures can
exercise the full script set, enable strict all-script coverage with either:

```sh
cd tests
TREESCAN_COVERAGE_ALL=true Rscript coverage.R
```

or by adding `tests/test_data/.coverage-all`.
