# Load required libraries
library(data.table)
library(lubridate)
library(Rcpp)

# Find path
path <- file.path(parent_dir, if (isTRUE(subregion)) "raw_data_subset" else "raw_data")

# Dates we want to get cleaned
dates_16m <- seq.Date(
  to = final_date - 1,
  from = final_date %m-% months(16),
  by = "day"
)

# Now get corresponding files
files_from_dates <- file.path(
  path,
  paste0("NSSP_data_", dates_16m, "_to_", dates_16m, ".csv")
)

# What month-year are we in?
year_month <- format(final_date, "%Y-%m")

monthly_lags_assessed <- list.files(
  file.path(parent_dir, "lag", "curves"),
  pattern = "\\.rds$"
)

have_we_got_lag <- isTRUE(paste0("lag_curve_", year_month, ".rds") %in% monthly_lags_assessed)
have_we_got_dataforlag <- isTRUE(length(list.files(file.path(parent_dir, "data", "data for lag"))) > 0)

# Now get ready for the lag assessment (if lag assessment is required)
need_lag <- !(have_we_got_lag && have_we_got_dataforlag)

safe_tmp <- file.path(parent_dir, "Temp")
dir.create(safe_tmp, showWarnings = FALSE, recursive = TRUE)

# Compile the C++ cleaner once and reuse it in future R sessions
# Save fast_codes.cpp in parent_dir
cpp_file <- paste0(parent_dir, "/code/2_optimizer.cpp")
cpp_cache <- file.path(parent_dir, ".rcpp_cache")
dir.create(cpp_cache, showWarnings = FALSE, recursive = TRUE)
Rcpp::sourceCpp(cpp_file, cacheDir = cpp_cache, showOutput = FALSE, verbose = FALSE)

# Load only the required columns unless full lag data is needed
needed_columns <- c(
  "C_Unique_Patient_ID",
  "C_Visit_Date_Time",
  "DischargeDiagnosis",
  "HasBeenAdmitted"
)
selected_columns <- if (need_lag) NULL else needed_columns

# Join each chunked data together
# Use option nssp.csv.workers to try parallel loading on Linux/macOS
workers <- max(1L, as.integer(getOption("nssp.csv.workers", 1L)))
parallel_load <- workers > 1L && .Platform$OS.type != "windows"

load_one_file <- function(f) {
  fread(
    file = f,
    select = selected_columns,
    colClasses = list(character = "C_Visit_Date_Time"),
    tmpdir = safe_tmp,
    nThread = if (parallel_load) 1L else getDTthreads(),
    showProgress = FALSE
  )
}

chunks <- if (parallel_load) {
  parallel::mclapply(
    files_from_dates,
    load_one_file,
    mc.cores = workers,
    mc.preschedule = TRUE
  )
} else {
  lapply(files_from_dates, load_one_file)
}

df_all <- rbindlist(chunks, use.names = TRUE, fill = FALSE)
rm(chunks)

# Clean
codes <- fast_clean_codes_cpp(df_all[["DischargeDiagnosis"]])

# We update severity to use the "HasBeenAdmitted" variable
severity <- ifelse(df_all[["HasBeenAdmitted"]] == 1, "A", "V")

# Let's get data ready to put into create count file
key <- df_all[["C_Unique_Patient_ID"]]
date <- as.Date(df_all[["C_Visit_Date_Time"]])

# Clean
# Keep base data.frame subsetting to preserve original row names and column types
data <- data.frame(
  "key" = key,
  "date" = date,
  "diagnosis_codes" = codes,
  "severity" = severity
)
data <- data[which(data$diagnosis_codes != "NA"), ]

# Create output path
out_file <- file.path(
  parent_dir,
  "data",
  "datasets",
  paste0("dataset_", final_date, ".rds")
)

# Ensure directory exists
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

# Save data
# FALSE avoids CPU-heavy compression while keeping the RDS readable by readRDS
saveRDS(data, file = out_file, compress = getOption("nssp.rds.compress", FALSE))

# Now get subset of data for the lag assessment (if lag assessment is required)
if (need_lag) {

  # Now add the cleaned codes to the full input for the lag file
  set(df_all, j = "codes", value = codes)

  # First we need to define some key dates
  current_date <- final_date
  current_date_minus_14 <- final_date - 14
  current_date_minus_90 <- final_date - 90

  # Now subset data to time period we're testing for lag structure
  df_all_for_lag <- df_all[which(
    date >= current_date_minus_90 & date <= current_date_minus_14
  ), ]

  lag_dir <- file.path(parent_dir, "data", "data for lag")
  dir.create(lag_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(
    df_all_for_lag,
    file.path(lag_dir, "data_for_lag.rds"),
    compress = getOption("nssp.rds.compress", FALSE)
  )
}
