required_test_packages <- c(
  "data.table",
  "dplyr",
  "lubridate",
  "sessioninfo",
  "stringr",
  "tidyr"
)

missing_test_packages <- required_test_packages[
  !vapply(required_test_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_test_packages) > 0) {
  stop(
    "Missing R packages required by the current tests: ",
    paste(missing_test_packages, collapse = ", "),
    ". Install them with `Rscript -e \"install.packages(c(",
    paste(sprintf("'%s'", missing_test_packages), collapse = ", "),
    "))\"`.",
    call. = FALSE
  )
}
