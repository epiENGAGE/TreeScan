# Load required libraries
library(Rnssp)
library(tidyverse)
library(lubridate)
library(readr)

out_dir <- file.path(parent_dir, "raw_data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

profile_file <- file.path(parent_dir, "myProfile.rda")
if (isTRUE(get0("first_time", ifnotfound = FALSE)) ||
    isTRUE(get0("password_refresh", ifnotfound = FALSE)) ||
    !file.exists(profile_file)) {
  myProfile <- Rnssp::create_profile()
  save(myProfile, file = profile_file)
} else {
  profile_env <- new.env(parent = baseenv())
  load(profile_file, envir = profile_env)
  if (!exists("myProfile", envir = profile_env, inherits = FALSE)) {
    stop("myProfile.rda does not contain an object named 'myProfile'.")
  }
  myProfile <- get("myProfile", envir = profile_env, inherits = FALSE)
  rm(profile_env)
}

# Download settings
nssp_settings <- list(
  end_date             = as.Date(final_date),
  months_back          = 16L,
  refresh_days         = 30L,
  max_backfill_days    = 120L,
  max_refresh_days     = 30L,
  max_attempts         = 3L,
  parallel_workers     = 1L,
  writer_threads       = 2L,
  allow_empty_chunks   = FALSE,
  visit_tz             = "UTC",
  date_col             = "C_Visit_Date_Time",
  field_list           = c(
    "C_Unique_Patient_ID", "DischargeDiagnosis", "C_Visit_Date_Time",
    "DischargeDiagnosisUpdates", "DischargeDiagnosisMDTUpdates",
    "HasBeenAdmitted", "Region"
  ),
  geographySystem      = "hospitalregion",
  geographies          = NULL,
  datasource           = "va_hosp",
  userId               = 7410,
  medicalGroupingSystem = "essencesyndromes",
  timeResolution       = "daily"
)

# Helper: build DataDetails URL and daily file paths
file_for_date <- function(d, folder) {
  file.path(folder, sprintf("NSSP_data_%s_to_%s.csv", d, d))
}

build_datadetails_url <- function(start_date, end_date, cfg) {
  esc <- function(x) utils::URLencode(as.character(x), reserved = TRUE)
  args <- c(
    paste0("datasource=", esc(cfg$datasource)),
    paste0("startDate=", format(as.Date(start_date), "%d%b%Y")),
    paste0("endDate=", format(as.Date(end_date), "%d%b%Y")),
    paste0("medicalGroupingSystem=", esc(cfg$medicalGroupingSystem)),
    paste0("userId=", esc(cfg$userId)),
    "percentParam=noPercent", "aqtTarget=DataDetails",
    paste0("geographySystem=", esc(cfg$geographySystem)),
    "detector=nodetectordetector",
    paste0("timeResolution=", esc(cfg$timeResolution)),
    "Facility_Type_Description=Emergency%20Care",
    paste0("field=", esc(cfg$field_list))
  )
  if (length(cfg$geographies)) {
    args <- c(args, paste0("geography=", esc(cfg$geographies)))
  }
  paste0(
    "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?",
    paste(args, collapse = "&")
  )
}

get_existing_daily_dates <- function(folder) {
  files <- list.files(
    folder,
    pattern = "^NSSP_data_[0-9]{4}-[0-9]{2}-[0-9]{2}_to_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv$",
    full.names = TRUE
  )
  if (!length(files)) return(as.Date(character()))
  names_only <- basename(files)
  first <- substr(names_only, 11L, 20L)
  second <- substr(names_only, 25L, 34L)
  sizes <- file.info(files)$size
  usable <- first == second & !is.na(sizes) & sizes > 0
  as.Date(first[usable], format = "%Y-%m-%d")
}

# Build backfill chunks from only the missing dates
make_date_chunks <- function(dates, max_days) {
  dates <- sort(unique(as.Date(dates)))
  if (!length(dates)) return(list())
  if (max_days < 1L) stop("max_days must be >= 1")
  
  runs <- split(seq_along(dates), cumsum(c(
    1L, as.integer(diff(as.integer(dates)) != 1L)
  )))
  chunks <- list()
  for (indices in runs) {
    for (i in seq.int(1L, length(indices), by = max_days)) {
      j <- min(i + max_days - 1L, length(indices))
      chunks[[length(chunks) + 1L]] <- list(
        start = dates[indices[i]],
        end = dates[indices[j]]
      )
    }
  }
  chunks
}

# Parse visit dates without changing their calendar day
parse_visit_dates <- function(x, tz = "UTC") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = tz))
  if (!is.character(x)) stop("Unexpected visit-date column type: ", class(x)[1L])
  x <- trimws(x)
  out <- suppressWarnings(as.Date(substr(x, 1L, 10L), format = "%Y-%m-%d"))
  todo <- which(is.na(out) & !is.na(x))
  if (length(todo)) {
    out[todo] <- suppressWarnings(as.Date(substr(x[todo], 1L, 10L),
                                          format = "%m/%d/%Y"))
  }
  out
}

fetch_nssp_csv <- function(url, cfg, profile) {
  for (attempt in seq_len(cfg$max_attempts)) {
    response <- tryCatch(
      Rnssp::get_api_data(
        url, fromCSV = TRUE, profile = profile,
        col_types = readr::cols(
          C_Unique_Patient_ID = readr::col_character(),
          .default = readr::col_guess()
        ),
        show_col_types = FALSE, progress = FALSE
      ),
      error = function(e) e
    )
    # Rnssp can return NULL on a non-200 HTTP response, so retry those too.
    if (is.data.frame(response) && cfg$date_col %in% names(response)) {
      return(response)
    }
    failure <- if (inherits(response, "error")) {
      conditionMessage(response)
    } else {
      "NSSP returned NULL or a response without the requested date column"
    }
    if (attempt == cfg$max_attempts) {
      stop("NSSP download failed after ", attempt, " attempt(s): ", failure)
    }
    message("  API attempt ", attempt, " failed: ", failure, "; retrying...")
    Sys.sleep(min(2^(attempt - 1L), 8))
  }
}

# Download and write daily files to a temporary folder before replacing any existing files
stage_chunk <- function(chunk, stage_dir, cfg, profile) {
  start <- chunk$start
  end <- chunk$end
  message(sprintf("  Request: %s to %s", start, end))
  url <- build_datadetails_url(start, end, cfg)
  
  t0 <- Sys.time()
  df <- fetch_nssp_csv(url, cfg, profile)
  download_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  if (!is.data.frame(df) || !cfg$date_col %in% names(df)) {
    stop("API response is not a CSV table with date column '", cfg$date_col, "'.")
  }
  if (!nrow(df) && !cfg$allow_empty_chunks) {
    stop("API returned zero rows for ", start, " to ", end,
         ". Existing files were not replaced. If this is expected, ",
         "set allow_empty_chunks = TRUE after verification.")
  }
  if (anyDuplicated(names(df))) stop("Duplicate CSV column names detected.")
  
  data.table::setDT(df)
  dt <- df
  visit_dates <- parse_visit_dates(dt[[cfg$date_col]], cfg$visit_tz)
  if (anyNA(visit_dates)) {
    stop(sum(is.na(visit_dates)), " missing/unparseable visit date(s) in ",
         start, " to ", end, ". Existing files were not replaced.")
  }
  if (any(visit_dates < start | visit_dates > end)) {
    stop("API returned visit dates outside ", start, " to ", end,
         ". Check visit_tz or query semantics before continuing.")
  }
  
  # split() stores *row indices*, not copies of the full data frame.
  groups <- split(seq_along(visit_dates), as.character(visit_dates))
  all_dates <- seq.Date(start, end, by = "day")
  written <- character(length(all_dates))
  empty_days <- 0L
  t1 <- Sys.time()
  
  for (i in seq_along(all_dates)) {
    d <- all_dates[i]
    rows <- groups[[as.character(d)]]
    if (is.null(rows)) {
      rows <- integer(0)
      empty_days <- empty_days + 1L
    }
    path <- file_for_date(d, stage_dir)
    data.table::fwrite(
      dt[rows], file = path,
      na = "", dateTimeAs = "ISO",
      nThread = cfg$writer_threads,
      showProgress = FALSE
    )
    if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size == 0) {
      stop("Failed to create a valid staged CSV for ", d)
    }
    written[i] <- path
  }
  
  write_s <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  message(sprintf("    %s rows; API+parse %.1f sec; write %.1f sec; %s empty day(s)",
                  format(nrow(dt), big.mark = ","), download_s, write_s, empty_days))
  list(paths = written, rows = nrow(dt), download_s = download_s,
       write_s = write_s, empty_days = empty_days)
}

# Replace existing daily files only after the complete download succeeds
publish_staged_files <- function(staged_paths, out_dir) {
  if (!length(staged_paths)) return(character())
  destination <- file.path(out_dir, basename(staged_paths))
  if (anyDuplicated(destination)) stop("Duplicate staged output filenames.")
  if (!all(file.exists(staged_paths))) stop("Missing staged CSV file(s).")
  
  backup_dir <- tempfile(".nssp_backup_", tmpdir = out_dir)
  if (!dir.create(backup_dir)) stop("Cannot create backup directory.")
  installed <- character()
  backed_up <- character()
  committed <- FALSE
  
  on.exit({
    if (!committed) {
      # Roll back all of this batch's successfully published files.
      for (p in rev(installed)) if (file.exists(p)) unlink(p)
      restore_ok <- vapply(rev(backed_up), function(p) {
        file.rename(file.path(backup_dir, basename(p)), p)
      }, logical(1))
      if (!all(restore_ok)) {
        warning("Rollback incomplete. Recover prior daily files from: ", backup_dir)
      } else {
        unlink(backup_dir, recursive = TRUE)
      }
    } else {
      unlink(backup_dir, recursive = TRUE)
    }
  }, add = TRUE)
  
  for (i in seq_along(staged_paths)) {
    dest <- destination[i]
    if (file.exists(dest)) {
      if (!file.rename(dest, file.path(backup_dir, basename(dest)))) {
        stop("Cannot back up existing file: ", dest)
      }
      backed_up <- c(backed_up, dest)
    }
    if (!file.rename(staged_paths[i], dest)) {
      stop("Cannot publish staged file: ", dest)
    }
    installed <- c(installed, dest)
  }
  committed <- TRUE
  destination
}

# Validate all chunks in a batch before replacing files
process_batch <- function(chunks, label, out_dir, cfg, profile) {
  if (!length(chunks)) return(character())
  stage_dir <- tempfile(paste0(".nssp_stage_", label, "_"), tmpdir = out_dir)
  if (!dir.create(stage_dir)) stop("Cannot create staging directory.")
  on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)
  
  work <- function(ch) {
    tryCatch(stage_chunk(ch, stage_dir, cfg, profile),
             error = function(e) list(error = conditionMessage(e)))
  }
  can_parallel <- cfg$parallel_workers > 1L &&
    .Platform$OS.type == "unix" && length(chunks) > 1L
  if (can_parallel) {
    # Fork workers are opt-in; Rnssp auth and ESSENCE concurrency need testing.
    info <- parallel::mclapply(
      chunks, work, mc.cores = min(cfg$parallel_workers, length(chunks)),
      mc.preschedule = FALSE
    )
  } else {
    if (cfg$parallel_workers > 1L && .Platform$OS.type != "unix") {
      warning("Parallel downloads are Unix-only in this script; using sequential requests.")
    }
    info <- lapply(chunks, work)
  }
  
  failures <- vapply(info, function(x) {
    inherits(x, "try-error") || !is.list(x) ||
      !is.null(x$error) || is.null(x$paths)
  }, logical(1))
  if (any(failures)) {
    errors <- vapply(info[failures], function(x) {
      if (inherits(x, "try-error")) {
        as.character(x)[1L]
      } else if (is.list(x) && !is.null(x$error)) {
        x$error
      } else {
        "Worker returned no download results"
      }
    }, character(1))
    stop(label, " batch aborted; existing files unchanged: ",
         paste(errors, collapse = " | "))
  }
  
  paths <- unlist(lapply(info, `[[`, "paths"), use.names = FALSE)
  published <- publish_staged_files(paths, out_dir)
  message(sprintf("  Published %s daily CSV(s) for %s.", length(published), label))
  published
}

# Download historical backfill and refresh the previous 30 days
download_nssp_backfill_and_refresh <- function(
    out_dir, cfg = nssp_settings, profile = myProfile
) {
  cfg$end_date <- as.Date(cfg$end_date)
  if (is.na(cfg$end_date)) stop("end_date must be a valid date.")
  if (cfg$months_back < 1L || cfg$refresh_days < 1L ||
      cfg$max_backfill_days < 1L || cfg$max_refresh_days < 1L ||
      cfg$max_attempts < 1L || cfg$parallel_workers < 1L ||
      cfg$writer_threads < 1L) stop("Invalid settings: day counts and worker counts must be >= 1.")
  
  full_start <- lubridate::`%m-%`(cfg$end_date, lubridate::period(months = cfg$months_back))
  refresh_start <- max(full_start, cfg$end_date - (cfg$refresh_days - 1L))
  existing <- get_existing_daily_dates(out_dir)
  
  historical_dates <- if (full_start < refresh_start) {
    seq.Date(full_start, refresh_start - 1L, by = "day")
  } else {
    as.Date(character())
  }
  historical_missing <- historical_dates[!(historical_dates %in% existing)]
  backfill_chunks <- make_date_chunks(historical_missing, cfg$max_backfill_days)
  
  message(sprintf("Period: %s to %s; rolling refresh: %s to %s",
                  full_start, cfg$end_date, refresh_start, cfg$end_date))
  message(sprintf("Historical backfill: %s missing day(s) in %s chunk(s).",
                  length(historical_missing), length(backfill_chunks)))
  
  results <- list(backfill_files = character(), refresh_files = character())
  
  # BACKFILL SECTION
  # Download only missing historical dates, using smaller chunks if needed.
  batch_size <- if (cfg$parallel_workers > 1L && .Platform$OS.type == "unix") {
    cfg$parallel_workers
  } else {
    1L
  }
  if (length(backfill_chunks)) {
    for (i in seq.int(1L, length(backfill_chunks), by = batch_size)) {
      j <- min(i + batch_size - 1L, length(backfill_chunks))
      message(sprintf("Backfill chunks %s-%s/%s", i, j, length(backfill_chunks)))
      output <- process_batch(backfill_chunks[i:j], "backfill", out_dir, cfg, profile)
      results$backfill_files <- c(results$backfill_files, output)
    }
  }
  
  # REFRESH SECTION
  # Always redownload the previous 30 days, even when daily files already exist.
  refresh_dates <- seq.Date(refresh_start, cfg$end_date, by = "day")
  refresh_chunks <- make_date_chunks(refresh_dates, cfg$max_refresh_days)
  message(sprintf("Refreshing ALL %s recent day(s)...", length(refresh_dates)))
  results$refresh_files <- process_batch(refresh_chunks, "refresh",
                                         out_dir, cfg, profile)
  
  message(sprintf("Done: %s backfill files, %s refreshed files.",
                  length(results$backfill_files), length(results$refresh_files)))
  invisible(results)
}

# Run it
results <- download_nssp_backfill_and_refresh(out_dir = out_dir)
