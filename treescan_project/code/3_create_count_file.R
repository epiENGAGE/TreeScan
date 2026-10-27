# Which lags do we want to get a count file for?
# This defaults to do it for lags of 1:5 days

# Load required libraries
if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table")
if (!requireNamespace("stringr", quietly = TRUE)) stop("Install stringr")
if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Install Rcpp")
library(data.table)

# set your working directory to point to "Preparing TreeScan Input Files" folder
setwd(file.path(parent_dir, "data"))

# Keep the tree year used by the original script
# Set options(nssp.count.tree_year = 2026L) to override it
YEAR <- getOption("nssp.count.tree_year", 2026L)

# Pull in 15 months of ED visit data ending on final_date
in_file <- file.path(parent_dir, "data", "datasets", paste0("dataset_", final_date, ".rds"))
sample_ED_data <- readRDS(in_file)
setDT(sample_ED_data)

# Convert date and clean diagnosis_codes
sample_ED_data <- sample_ED_data[, .(date = as.Date(date), key, severity,
                                     diagnosis_codes = stringr::str_squish(diagnosis_codes))]

# Frequency table for every code in the original sample dataset
# Do this only once for all lags
split_codes <- strsplit(sample_ED_data$diagnosis_codes, " ", fixed = TRUE)
n_codes <- lengths(split_codes)
all_code <- gsub(".", "", unlist(split_codes, use.names = FALSE), fixed = TRUE)
all_code <- all_code[!is.na(all_code) & all_code != ""]
dx_freq_table <- data.table(code = all_code)[, .(Freq = .N), by = code]
rm(all_code)

# Split diagnosis_codes into long format faster than separate_longer_delim()
sample_ED_data_long <- sample_ED_data[rep.int(seq_len(.N), n_codes),
                                       .(date, key, severity)]
sample_ED_data_long[, code := unlist(split_codes, use.names = FALSE)]
rm(split_codes, n_codes, sample_ED_data)
sample_ED_data_long[, code := gsub("\\s+", "", code)]
sample_ED_data_long[, code := gsub(".", "", code, fixed = TRUE)]

# Clean and filter codes
sample_ED_data_long <- sample_ED_data_long[
  !is.na(code) & code != "" & !grepl("^[0-9]", code) & grepl("\\d", code)
]

# For repeat visits on same day, keep more severe outcome
# Only rows containing valid diagnosis codes participate in this calculation
sample_ED_data_long[, severity := if (any(severity == "A", na.rm = TRUE)) "A" else "V",
                    by = .(date, key)]

# Remove duplicate rows
sample_ED_data_long <- unique(sample_ED_data_long)

## Remove codes that are ineligible for this analysis
ineligible_code_pattern <- paste(
  "U071|J09|J10|J11|\\bJ301\\b|\\bJ302\\b|\\bJ3089\\b|\\bJ309\\b|J45|T7840",
  "Z0|Z10|Z1152|Z12|Z13|Z14|Z15|Z17|Z18|Z19|Z21|Z28|Z30|Z3|Z4|Z50|Z51|Z52|Z53|Z55|Z56|Z62|Z63|Z64|Z66|Z67|Z68|Z76|Z78|Z8|Z90|Z91|Z92|Z93|Z94|Z95|Z96|Z97|Z98",
  "\\bC|\\bD0|\\bD1|\\bD2|\\bD3|\\bD4|\\bQ", sep = "|"
)
eligible_visit_codes <- sample_ED_data_long[!grepl(ineligible_code_pattern, code)]
rm(sample_ED_data_long)

## Merge with tree file in wide format to get level2 and level3 parents for each code
icd10_treefile_wide <- fread(
  file.path(parent_dir, "data", paste0("Tree_File_", YEAR, "_wide_format.txt")),
  select = c("Name1", "Level2", "Level3")
)

## Remove tree rows that would be removed after merge anyway
icd10_treefile_wide <- icd10_treefile_wide[!is.na(Level2) & !is.na(Level3)]
eligible_visit_codes <- merge(
  eligible_visit_codes, icd10_treefile_wide,
  by.x = "code", by.y = "Name1", all = FALSE, sort = TRUE
)

# Transform again so each row is unique visit rather than unique visit-code
all_visits <- eligible_visit_codes[, .(
  Level3 = paste(Level3, collapse = ","),
  code = paste(code, collapse = ",")
), by = .(date, key, severity)]
rm(eligible_visit_codes)

# Change Level 3 "-" to "_"
all_visits[, Level3 := gsub("-", "_", Level3, fixed = TRUE)]

## Keep only incident diagnoses
# Count number of visits per patient/key
all_visits[, visit_count := .N, by = key]

# Define the union of the 90-day study periods
ends <- final_date - as.integer(initial_lags)
keep_date <- all_visits$date >= min(ends) - 89L & all_visits$date <= max(ends)

all_visits_single <- all_visits[visit_count == 1L & keep_date,
                                 .(date, key, severity, code)]
all_visits_multiple <- all_visits[visit_count >= 2L,
                                   .(date, key, severity, code, Level3)]
rm(all_visits, keep_date)

# Define incident diagnosis for patients with multiple visits
# Keep the original visit order within each key when generating search strings
if (nrow(all_visits_multiple)) {
  all_visits_multiple[, group_id := match(key, unique(key))]
  setorder(all_visits_multiple, group_id)

  # Apply the compiled patient function once for every lag
  # search  = earlier visit codes within 365 days (in existing row order)
  # search2 = later admit codes for earlier visits
  # searchf = both combined
  cpp_file <- paste0(parent_dir, "/code/3_optimizer.cpp")
  if (!file.exists(cpp_file)) {
    cpp_file <- getOption("nssp.count.cpp", "3_optimizer.cpp")
  }
  if (!file.exists(cpp_file)) stop("Cannot find 3_optimizer.cpp in parent_dir")
  Rcpp::sourceCpp(cpp_file, rebuild = FALSE, showOutput = FALSE, verbose = FALSE)
  searches <- nssp_patient_search_cpp(
    as.integer(all_visits_multiple$date),
    as.character(all_visits_multiple$severity),
    all_visits_multiple$Level3,
    all_visits_multiple$group_id
  )

  # Remove search3 regex matches from the past-year search, as in original code
  affected <- which(!is.na(searches$search3))
  if (length(affected)) {
    searches$search[affected] <- mapply(
      gsub, searches$search3[affected], "", searches$search[affected],
      USE.NAMES = FALSE
    )
  }

  # Combine search and search2 exactly as the original logic does
  searchf <- searches$search
  has2 <- which(!is.na(searches$search2))
  if (length(has2)) {
    s2 <- searches$search2[has2]
    s <- searches$search[has2]
    searchf[has2] <- ifelse(
      s2 == "NONE", "NONE",
      ifelse(s == "NONE", s2, paste0(s, "|", s2))
    )
  }
  rm(searches, affected, has2)

  # Only clean rows needed by at least one of the requested lag windows
  # Keep the search strings attached before filtering, so every row stays aligned
  all_visits_multiple[, searchf := searchf]
  rm(searchf)
  all_visits_multiple <- all_visits_multiple[
    date >= min(ends) - 89L & date <= max(ends)
  ]

  # Use search string to find non-incident codes to remove
  # remove searchf codes from Level3 and substitute REMOVE for corresponding code
  if (nrow(all_visits_multiple)) {
    all_visits_multiple[, Level3 := mapply(
      function(Level3, searchf) gsub(searchf, "REMOVE", Level3),
      Level3, searchf, USE.NAMES = FALSE
    )]
    all_visits_multiple[, code := mapply(
      function(code, Level3) {
        codes <- unlist(strsplit(code, ",", fixed = TRUE), use.names = FALSE)
        lvl3 <- unlist(strsplit(Level3, ",", fixed = TRUE), use.names = FALSE)
        codes[lvl3 == "REMOVE"] <- "REMOVE"
        paste0(codes, collapse = ",")
      }, code, Level3, USE.NAMES = FALSE
    )]
  }
}

# Merge back with patients with 1 visit
# every row is a unique patient-visit
study_cohort <- rbindlist(list(
  all_visits_single[, .(date, key, severity, code)],
  all_visits_multiple[, .(date, key, severity, code)]
), use.names = TRUE)
rm(all_visits_single, all_visits_multiple)

# Make long dataset by splitting diagnosis codes
split_codes <- strsplit(study_cohort$code, ",", fixed = TRUE)
n_codes <- lengths(split_codes)
study_data_long <- study_cohort[rep.int(seq_len(.N), n_codes),
                                .(date, key, severity)]
study_data_long[, code := unlist(split_codes, use.names = FALSE)]
rm(study_cohort, split_codes, n_codes)

# Remove non-incident codes
study_data_long <- study_data_long[code != "REMOVE"]

# Merge so that we have a column that is the frequency of every code
# in the original sample dataset
study_data_long <- merge(study_data_long, dx_freq_table, by = "code",
                         all.x = TRUE, sort = FALSE)
rm(dx_freq_table)

# Add Level3
study_data_long <- merge(study_data_long,
                         icd10_treefile_wide[, .(Name1, Level3)],
                         by.x = "code", by.y = "Name1", all.x = TRUE, sort = FALSE)
rm(icd10_treefile_wide)

# Every required input code should have a frequency; fail rather than silently
# change which row is retained when malformed data produces a missing frequency
if (anyNA(study_data_long$Freq)) {
  stop("Some diagnosis codes have no original frequency; inspect the input normalization")
}

# Create directory to store the count and v2 files
v2_dir <- file.path(parent_dir, "data", "v2", as.character(final_date))
count_dir <- file.path(parent_dir, "data", "analysis_count_files",
                       paste0("Analysis_Count_File_", final_date))
dir.create(v2_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(count_dir, recursive = TRUE, showWarnings = FALSE)


# Do the 90-day work for each requested lag, reusing the same full-history results
.create_count_lag <- function(LAG) {
  print(paste0("Creating count file for lag ", LAG))
  end <- final_date - as.integer(LAG)

  ### CREATE COUNT FILE FOR ASYNDROMIC TREESCAN ANALYSIS
  ## Ramona Lall & Alison Levin-Rector
  ## October 2025

  # Keep 90-day study period
  current <- copy(study_data_long[date >= end - 90L + 1L & date <= end])

  # Sort in order of increasing frequency, so rarest is first
  setorder(current, key, date, Level3, Freq)

  # Keep unique
  # If multiple codes within same visit with same Level3, keep rarest
  if (nrow(current)) {
    # Treat consecutive (key, date, Level3) values as one group
    group <- rleidv(current, c("key", "date", "Level3"))
    group_first <- which(!duplicated(group))
    group_size <- tabulate(group, nbins = length(group_first))

    # Same as: filter(n() == 1) for single-code groups
    no_repeats <- current[group_first[group_size == 1L]]

    # Same as: filter(n() >= 2 & Freq == first(Freq))
    # Retain the original random selection among the rarest tied codes
    tied_idx <- which(group_size[group] >= 2L &
                      current$Freq == current$Freq[group_first][group])
    if (length(tied_idx)) {
      ties <- data.table(group = group[tied_idx], row_index = tied_idx)
      chosen <- ties[, .(row_index = row_index[
        which(sample(seq_len(.N), size = .N, replace = FALSE) == 1L)
      ]), by = group]$row_index
      tie_breaker <- current[chosen]
    } else {
      tie_breaker <- current[0L]
    }
    current <- rbindlist(list(no_repeats, tie_breaker), use.names = TRUE)
  }

  # The original script resets n to 1 for every selected row
  # Its count-file sum(n) is therefore equivalent to counting selected rows

  v2 <- current[, .(
    date = as.Date(date),
    key = as.character(key),
    dispo = as.character(severity),
    code = gsub("\\.", "", as.character(code))
  )]

  # force exact column order expected downstream
  # write.csv(..., quote = FALSE) is used for byte-level compatibility
  csv_path <- file.path(v2_dir, paste0("lag", LAG, ".csv"))
  if (isTRUE(getOption("nssp.count.base_writer", FALSE))) {
    write.csv(as.data.frame(v2), csv_path, row.names = FALSE, quote = FALSE)
  } else {
    fwrite(v2, csv_path, sep = ",", quote = FALSE, na = "NA", bom = FALSE)
  }

  ### CREATE INPUT FILE
  ## CREATE INPUT FILE
  # input file is unique visit date, codes and count of that combo
  input_file <- current[, .(n = .N), by = .(code, severity, date)]

  # Add decimal
  input_file[nchar(code) >= 4L,
             code := paste0(substr(code, 1L, 3L), ".",
                            substr(code, 4L, nchar(code)))]

  # Add 0- and 1-
  input_file[, code := fifelse(severity == "V",
                              paste0("0-", code), paste0("1-", code))]

  # Keep and order final columns
  input_file <- input_file[, .(
    code = trimws(code),
    date = format(as.Date(date), "%Y/%m/%d"),
    n = as.integer(n)
  )]

  # Write a PURE ASCII tab-delimited file (no BOM, no UTF-16)
  txt_path <- file.path(count_dir, paste0("lag", LAG, ".txt"))
  if (isTRUE(getOption("nssp.count.base_writer", FALSE))) {
    write.table(as.data.frame(input_file), txt_path, sep = "\t",
                row.names = FALSE, col.names = TRUE, quote = FALSE,
                fileEncoding = "ASCII")
  } else {
    fwrite(input_file, txt_path, sep = "\t", quote = FALSE,
           na = "NA", bom = FALSE)
  }
  invisible(NULL)
}

# Process separate lag windows in parallel when requested
# Single-worker mode preserves the original R random-number stream
workers <- max(1L, as.integer(getOption("nssp.count.cores", 1L)))
if (anyDuplicated(initial_lags)) workers <- 1L  # Avoid writes to the same lag file
if (.Platform$OS.type == "unix" && workers > 1L && length(initial_lags) > 1L) {
  parallel::mclapply(initial_lags, .create_count_lag,
                     mc.cores = min(workers, length(initial_lags)),
                     mc.set.seed = TRUE)
} else {
  invisible(lapply(initial_lags, .create_count_lag))
}
