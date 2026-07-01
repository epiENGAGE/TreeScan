# 🌳 TreeScan World Cup 2026 Preparation

[![R tests and coverage](https://github.com/epiENGAGE/TreeScan/actions/workflows/r-tests.yml/badge.svg)](https://github.com/epiENGAGE/TreeScan/actions/workflows/r-tests.yml)
[![coverage](./tests/badges/coverage-total.svg)](https://github.com/epiENGAGE/TreeScan/actions/workflows/r-tests.yml)

[TreeScan™ v2.3](https://www.treescan.org/) implementation for public health officials using ESSENCE-based emergency department (ED) data and ICD-10 codes to detect deviations from a 90-day baseline period.

- **Goal:** Strengthen capacity to detect health issues not captured by syndromic surveillance for the US 2026 World Cup.
- **Pre-print:** Initial analytic pipeline, R code, and supporting files from NYC Health Dept.'s pre-print.
- **Overview:** This repository contains an R-based TreeScan analysis pipeline in `treescan_project/` for preparing input files, running TreeScan, and generating signal interpretation outputs.
- **Code expansion:** The authors' advice supported code expansion by Nyall Jamieson, Emily Javan, and Remy Pasco within epiENGAGE.

## 📦 Installation

### 1. Download this Repository
- Click the green **Code** button on GitHub.
- Select **Download ZIP**.
- Extract the ZIP file.
- Locate the `treescan_project` subfolder.
- Move `treescan_project` to your desired working directory.

### 2. Install RStudio
https://posit.co/download/rstudio-desktop/

### 3. Install TreeScan
https://www.treescan.org/download_treescan.html

### 4. Place TreeScan in the Project Folder

| Environment | Folder |
|---|---|
| Windows | `TS_windows/` |
| Linux | `TS_linux/` |

## 🚀 Running the Pipeline

```sh
treescan_project/code/run_full_pipeline.R
```

## 🧪 Testing

```sh
cd tests
Rscript testthat.R
```

```sh
cd tests
Rscript coverage.R
```

```sh
cd tests
TREESCAN_COVERAGE_HTML=true Rscript coverage.R
```

## ⚠️ Notes

- Ensure the correct TreeScan version is placed in the matching folder.
- The **non-graphical TreeScan version is required**.
- Users ideally run **99,999 Monte Carlo simulations** for operational analyses.
