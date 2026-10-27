# Full script that runs all the does a lag assessment

# Load required libraries
library(data.table)
library(stringi)
library(lubridate)

# Find path
path <- file.path(parent_dir, "raw_data")

# First we need to define some key dates
current_date <- Sys.Date()
current_date_minus_14 <- current_date - 14L
current_date_minus_90 <- current_date - 90L
dates_for_lag <- seq.Date(from = current_date_minus_90, to = current_date_minus_14, by = "day")

# Load data
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
if (!length(files)) stop("No CSV files found in: ", path)

# Join each chunked data together
# Read only the columns used below and discard out-of-window rows before binding
required_cols <- c(
  "C_Visit_Date_Time",
  "DischargeDiagnosisUpdates",
  "DischargeDiagnosisMDTUpdates"
)

read_lag_file <- function(file) {
  x <- data.table::fread(
    file,
    select = required_cols,
    colClasses = list(character = required_cols),
    na.strings = "NA",
    strip.white = FALSE,
    check.names = TRUE,
    showProgress = FALSE
  )
  
  date <- as.Date(x$C_Visit_Date_Time)
  x[which(date >= current_date_minus_90 & date <= current_date_minus_14)]
}

df_all_for_lag <- data.table::rbindlist(
  lapply(files, read_lag_file),
  use.names = TRUE
)

# Before we want to finish cleaning we need to assess the lag

# Now subset data to time period we're testing for lag structure
# This is done while each file is loaded, preserving the original file and row order

# Parse codes
# Match both parts of each diagnosis event with a single regular expression
.diag_pattern <- "^\\{(\\d+)\\};;(.*)$"
.time_pattern <- paste0(
  "^\\{(\\d+)\\}",
  "(?:;(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}))?"
)
.code_pattern <- "[A-Z][0-9][0-9A-Z](?:\\.[0-9A-Z]{1,4})?"

parse_diag_events <- function(x) {
  ev <- stringi::stri_split_fixed(x, "|", omit_empty = FALSE)[[1L]]
  id <- stringi::stri_match_first_regex(ev, "^\\{(\\d+)\\}")[, 2L]
  diag <- stringi::stri_match_first_regex(ev, "^\\{\\d+\\};;(.*)$")[, 2L]
  ans <- tibble::tibble(
    event_id = suppressWarnings(as.integer(id)),
    diag_str = diag
  )
  ans[!is.na(ans$event_id), , drop = FALSE]
}

parse_time_events <- function(x) {
  ev <- stringi::stri_split_fixed(x, "|", omit_empty = FALSE)[[1L]]
  id <- stringi::stri_match_first_regex(ev, "^\\{(\\d+)\\}")[, 2L]
  tm <- stringi::stri_match_first_regex(
    ev, "^\\{\\d+\\};(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})"
  )[, 2L]
  ans <- tibble::tibble(
    event_id = suppressWarnings(as.integer(id)),
    update_time = lubridate::ymd_hms(tm, quiet = TRUE)
  )
  ans[!is.na(ans$event_id), , drop = FALSE]
}

.empty_nssp <- function() {
  data.table::data.table(
    row_id = integer(),
    visit_time = as.POSIXct(character(), tz = "UTC"),
    event_id = integer(),
    update_time = as.POSIXct(character(), tz = "UTC"),
    code = character()
  )
}

# Fast chunk processor
.process_nssp_chunk <- function(chunk_df) {
  if (!nrow(chunk_df)) return(.empty_nssp())
  
  # Keep only rows that could possibly contribute output.
  # This does not change results; rows failing this would have returned empty anyway.
  chunk_df <- chunk_df[
    !is.na(DischargeDiagnosisUpdates) &
      !is.na(DischargeDiagnosisMDTUpdates) &
      DischargeDiagnosisUpdates != "" &
      DischargeDiagnosisMDTUpdates != ""
  ]
  if (!nrow(chunk_df)) return(.empty_nssp())
  
  # -------------------------
  # Diagnosis side
  # -------------------------
  diag_list <- stringi::stri_split_fixed(
    chunk_df$DischargeDiagnosisUpdates, "|", omit_empty = FALSE
  )
  diag_n <- lengths(diag_list)
  diag_ev <- unlist(diag_list, use.names = FALSE)
  diag_match <- stringi::stri_match_first_regex(diag_ev, .diag_pattern)
  
  diag_tbl <- data.table::data.table(
    row_id = rep.int(chunk_df$row_id, diag_n),
    visit_time = rep(chunk_df$visit_time, diag_n),
    event_id = suppressWarnings(as.integer(diag_match[, 2L])),
    diag_str = diag_match[, 3L]
  )[
    !is.na(event_id) & !is.na(diag_str)
  ]
  
  if (!nrow(diag_tbl)) return(.empty_nssp())
  
  # Extract codes once per diagnosis event, not once per matching time event
  # This preserves duplicate-event output without repeating regex extraction
  diag_tbl[, codes := stringi::stri_extract_all_regex(
    diag_str, .code_pattern, omit_no_match = TRUE
  )]
  diag_tbl[, diag_str := NULL]
  diag_tbl <- diag_tbl[lengths(codes) > 0L]
  if (!nrow(diag_tbl)) return(.empty_nssp())
  
  # -------------------------
  # Time side
  # -------------------------
  time_list <- stringi::stri_split_fixed(
    chunk_df$DischargeDiagnosisMDTUpdates, "|", omit_empty = FALSE
  )
  time_n <- lengths(time_list)
  time_ev <- unlist(time_list, use.names = FALSE)
  time_match <- stringi::stri_match_first_regex(time_ev, .time_pattern)
  
  time_tbl <- data.table::data.table(
    row_id = rep.int(chunk_df$row_id, time_n),
    event_id = suppressWarnings(as.integer(time_match[, 2L])),
    update_text = time_match[, 3L]
  )[
    !is.na(event_id)
  ]
  
  if (!nrow(time_tbl)) return(.empty_nssp())
  time_tbl[, update_time := lubridate::ymd_hms(update_text, quiet = TRUE)]
  time_tbl[, update_text := NULL]
  
  # -------------------------
  # original joined by event_id inside each row
  # bulk equivalent is join by row_id + event_id
  # -------------------------
  ev_tbl <- merge(
    diag_tbl,
    time_tbl,
    by = c("row_id", "event_id"),
    all = FALSE,
    sort = TRUE,
    allow.cartesian = TRUE
  )
  
  if (!nrow(ev_tbl)) return(.empty_nssp())
  
  # -------------------------
  # str_extract_all(...), then unnest(code), then filter(!is.na(code), code != "")
  # Expand stored matches after the event join, retaining row order and duplicates
  # -------------------------
  code_n <- lengths(ev_tbl$codes)
  out <- ev_tbl[
    rep.int(seq_len(nrow(ev_tbl)), code_n),
    .(row_id, visit_time, event_id, update_time)
  ]
  out[, code := unlist(ev_tbl$codes, use.names = FALSE)]
  out
  
}

# Fast main cleaner
clean_nssp_updates <- function(df, chunk_size = 200000L, cores = 1L) {
  n <- nrow(df)
  if (!n) return(tibble::as_tibble(.empty_nssp()))
  
  df_prepped <- data.table::data.table(
    row_id = seq_len(n),
    visit_time = lubridate::ymd_hms(df$C_Visit_Date_Time, quiet = TRUE),
    DischargeDiagnosisUpdates = df$DischargeDiagnosisUpdates,
    DischargeDiagnosisMDTUpdates = df$DischargeDiagnosisMDTUpdates
  )
  
  starts <- seq.int(1L, n, by = chunk_size)
  chunk_fun <- function(start) {
    last <- min(start + chunk_size - 1L, n)
    .process_nssp_chunk(df_prepped[start:last])
  }
  
  if (.Platform$OS.type != "windows" && cores > 1L && length(starts) > 1L) {
    old_threads <- data.table::getDTthreads()
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(old_threads), add = TRUE)
    
    parts <- parallel::mclapply(
      starts, chunk_fun, mc.cores = min(cores, length(starts)),
      mc.preschedule = TRUE
    )
    if (any(vapply(parts, inherits, logical(1), "try-error"))) {
      stop("One or more NSSP parsing workers failed")
    }
  } else {
    parts <- lapply(starts, chunk_fun)
  }
  
  tibble::as_tibble(data.table::rbindlist(parts, use.names = TRUE))
}

# Run main cleaning
cores_available <- parallel::detectCores(logical = FALSE)
if (is.na(cores_available)) cores_available <- 1L
cores_to_use <- if (.Platform$OS.type == "windows") 1L else min(4L, cores_available)

df_long <- clean_nssp_updates(
  df_all_for_lag,
  chunk_size = 200000L,
  cores = cores_to_use
)

# Find first appearance for each code for each patient
first_appearance <- data.table::as.data.table(df_long)[
  , .(
    visit_time = visit_time[1L],
    first_time = min(update_time, na.rm = TRUE)
  ),
  keyby = .(row_id, code)
]
first_appearance[, delay_hours := as.numeric(
  difftime(first_time, visit_time, units = "hours")
)]
first_appearance <- tibble::as_tibble(
  first_appearance[!is.na(delay_hours) & delay_hours >= 0]
)

# Get time delay distributions for each specific ICD10 code
percentile_probs <- (1:100) / 100
percentile_names <- paste0("p", 1:100)

delay_by_code <- data.table::as.data.table(first_appearance)[
  , as.list(stats::quantile(
    delay_hours, probs = percentile_probs,
    na.rm = TRUE, names = FALSE
  )),
  keyby = code
]
data.table::setnames(delay_by_code, c("code", percentile_names))
delay_by_code <- tibble::as_tibble(delay_by_code)

# Overall time delay distribution
overall_values <- stats::quantile(
  first_appearance$delay_hours,
  probs = percentile_probs,
  na.rm = TRUE,
  names = FALSE
)
overall_delay <- tibble::as_tibble(
  as.list(stats::setNames(overall_values, percentile_names))
)

# Estimate the optimal time delay based on first elbow-like point
x <- as.numeric(unlist(overall_delay[1L, 1:95], use.names = FALSE))
y <- 1:95

ord <- order(x)
x <- x[ord]
y <- y[ord]

fit <- smooth.spline(x, y, spar = 0.6)

xx <- seq(min(x), max(x), length.out = 500)
p0 <- predict(fit, xx, deriv = 0)
p1 <- predict(fit, xx, deriv = 1)
p2 <- predict(fit, xx, deriv = 2)

curv <- abs(p2$y) / (1 + p1$y^2)^(3/2)

knee_x <- xx[which.max(curv)]
knee_y <- predict(fit, knee_x)$y

# plot(x, y, pch = 1)
# lines(p0$x, p0$y)
# points(knee_x, knee_y, col = "red", pch = 19, cex = 1.5)

x_days <- x / 24

plot(x_days, y,
     xlab = "Delay (days)",
     ylab = "Percentage of codes diagnosed (%)",
     type = "l",
     ylim = c(0, 100))

# integer x values within the plotted range
x_int <- seq(ceiling(min(x_days)), floor(max(x_days)), by = 1)

# y-values of the curve at those integer x-values
y_int <- approx(x_days, y, xout = x_int)$y

# vertical lines: from x-axis up to curve
segments(x0 = x_int, y0 = 0,
         x1 = x_int, y1 = y_int,
         col = "grey70", lty = 2)

# horizontal lines: from y-axis to curve
segments(x0 = 0, y0 = y_int,
         x1 = x_int, y1 = y_int,
         col = "grey70", lty = 2)

# redraw curve on top
lines(x_days, y, lwd = 2)

# Add highlighted knee point
points(knee_x / 24, knee_y, col = "red", pch = 19, cex = 1.5)
