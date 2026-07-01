# Retrospective World Cup TreeScan new-signal / duplicate tracker
# Append-only version:
#   - If tracker file does not exist: create it from wc_start_date through final_date.
#   - If tracker file exists: find the latest analysis_date already in the file,
#     process only dates after that through final_date, and append rows only.
# Source after parent_dir, subregion, initial_lags, and final_date exist.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(readxl)
  library(openxlsx)
  library(lubridate)
  library(stringi)
})

# First-run catch-up start date. Subsequent runs ignore this and append only missing dates.
wc_start_date <- as.Date("2026-06-04")
wc_end_date   <- as.Date("2026-07-31")

wc_start_date <- as.Date("2026-05-26")
wc_end_date   <- as.Date("2026-05-31")

duplicate_exact_start_days <- 3L
duplicate_close_branch_days <- 3L
duplicate_same_code_detection_days <- 14L
duplicate_branch_prefix_chars <- 3L
write_retrospective_signal_reports <- TRUE

results_root <- if (isTRUE(subregion)) {
  file.path(parent_dir, "results_subregion")
} else {
  file.path(parent_dir, "results")
}

signal_report_root <- if (isTRUE(subregion)) {
  file.path(parent_dir, "signal_report_subregion")
} else {
  file.path(parent_dir, "signal_report")
}

dedup_root <- if (isTRUE(subregion)) {
  file.path(parent_dir, "signal_report_subregion", "retrospective_dedup")
} else {
  file.path(parent_dir, "signal_report", "retrospective_dedup")
}

dir.create(dedup_root, recursive = TRUE, showWarnings = FALSE)
dir.create(signal_report_root, recursive = TRUE, showWarnings = FALSE)

tracker_file <- file.path(dedup_root, "WC_new_signal_tracker.xlsx")

clean_node_id <- function(x) {
  x <- stringi::stri_replace_all_fixed(as.character(x), "\xa0", "")
  trimws(x)
}

node_code <- function(node_identifier) {
  gsub("\\.", "", sub(".*-", "", clean_node_id(node_identifier)))
}

node_severity <- function(node_identifier) {
  suppressWarnings(as.integer(sub("-.*", "", clean_node_id(node_identifier))))
}

coerce_excel_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  suppressWarnings(as.Date(x))
}

results_file <- function(analysis_date, lag) {
  file.path(
    results_root,
    as.character(as.Date(analysis_date)),
    paste0("Results_lag", lag, "_", as.Date(analysis_date), ".csv")
  )
}

temporal_file <- function(analysis_date, lag) {
  file.path(
    results_root,
    as.character(as.Date(analysis_date)),
    paste0("Results_lag", lag, "_", as.Date(analysis_date), ".temporal.csv")
  )
}

signal_report_file <- function(analysis_date) {
  file.path(signal_report_root, paste0("Signals_Report_", as.Date(analysis_date), ".xlsx"))
}

read_one_results <- function(analysis_date, lag) {
  f <- results_file(analysis_date, lag)
  if (!file.exists(f)) return(NULL)

  x <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || nrow(x) == 0) return(NULL)

  results_keep_cols <- c(
    "Node.Identifier",
    "Node.Name",
    "Time.Window.Start",
    "Time.Window.End",
    "Recurrence.Interval",
    "Relative.Risk"
  )
  
  x <- x %>%
    dplyr::select(dplyr::any_of(results_keep_cols))
  
  missing_needed <- setdiff(c("Node.Identifier", "Recurrence.Interval", "Relative.Risk"), names(x))
  if (length(missing_needed) > 0) {
    stop(
      "Results file is missing required columns: ",
      paste(missing_needed, collapse = ", "),
      " in ", f,
      "\nActual columns: ", paste(names(x), collapse = ", ")
    )
  }

  x$Node.Identifier <- clean_node_id(x$Node.Identifier)
  x$Recurrence.Interval <- suppressWarnings(as.numeric(x$Recurrence.Interval))
  x$Relative.Risk <- suppressWarnings(as.numeric(x$Relative.Risk))
  if ("Excess.Cases" %in% names(x)) x$Excess.Cases <- suppressWarnings(as.numeric(x$Excess.Cases))

  x <- x[!is.na(x$Recurrence.Interval), , drop = FALSE]
  x <- x[x$Relative.Risk >= 1.3, , drop = FALSE]
  x <- x[(x$Recurrence.Interval >= 365) |
           (grepl("^1-", x$Node.Identifier) & x$Recurrence.Interval >= 100), , drop = FALSE]
  if (nrow(x) == 0) return(NULL)

  x %>%
    mutate(
      analysis_date = as.Date(analysis_date),
      lag = as.character(lag),
      node_clean = node_code(Node.Identifier),
      severity = node_severity(Node.Identifier),
      cluster_start = suppressWarnings(as.Date(.data$Time.Window.Start)),
      cluster_end = suppressWarnings(as.Date(.data$Time.Window.End)),
      source_results_file = f
    )
}

assign_new_from_history <- function(all_signals) {
  if (nrow(all_signals) == 0) return(all_signals %>% mutate(Trend = character()))

  all_signals %>%
    arrange(lag, Node.Identifier, analysis_date) %>%
    group_by(lag, Node.Identifier) %>%
    mutate(
      prev_ri = lag(Recurrence.Interval),
      prev_rr = lag(Relative.Risk),
      Trend = case_when(
        is.na(prev_ri) ~ "1.New",
        grepl("^1-", Node.Identifier) & !is.na(prev_ri) & prev_ri < 100 ~ "1.New",
        !grepl("^1-", Node.Identifier) & Recurrence.Interval >= 365 &
          ((!is.na(prev_ri) & prev_ri < 365) | (!is.na(prev_rr) & prev_rr < 1.3)) ~ "1.New",
        TRUE ~ "not_new"
      )
    ) %>%
    ungroup()
}

drop_internal_columns <- function(x) {
  x %>%
    select(-any_of(c("prev_ri", "prev_rr", "node_clean")))
}

icd_common_prefix <- function(a, b) {
  a <- gsub("\\.", "", toupper(as.character(a)))
  b <- gsub("\\.", "", toupper(as.character(b)))
  n <- min(nchar(a), nchar(b))
  if (n == 0) return(0L)
  aa <- strsplit(substr(a, 1, n), "", fixed = TRUE)[[1]]
  bb <- strsplit(substr(b, 1, n), "", fixed = TRUE)[[1]]
  m <- which(aa != bb)
  if (length(m) == 0) n else m[1] - 1L
}

same_or_close_branch <- function(code_a, code_b, min_prefix = duplicate_branch_prefix_chars) {
  code_a <- gsub("\\.", "", toupper(as.character(code_a)))
  code_b <- gsub("\\.", "", toupper(as.character(code_b)))
  if (is.na(code_a) || is.na(code_b) || code_a == "" || code_b == "") return(FALSE)
  startsWith(code_a, code_b) || startsWith(code_b, code_a) || icd_common_prefix(code_a, code_b) >= min_prefix
}

pick_stronger_prior <- function(current_row, prior_rows) {
  if (nrow(prior_rows) == 0) return(NULL)
  
  prior_rows %>%
    mutate(
      ri_num = suppressWarnings(as.numeric(Recurrence.Interval)),
      rr_num = suppressWarnings(as.numeric(Relative.Risk))
    ) %>%
    arrange(desc(ri_num), desc(rr_num), analysis_date) %>%
    slice(1)
}

add_duplicate_columns <- function(x) {
  if (!"duplicate_class" %in% names(x)) x$duplicate_class <- "unique_initial"
  if (!"duplicate_of_node" %in% names(x)) x$duplicate_of_node <- NA_character_
  if (!"duplicate_of_date" %in% names(x)) x$duplicate_of_date <- as.Date(NA)
  if (!"duplicate_of_cluster_start" %in% names(x)) x$duplicate_of_cluster_start <- as.Date(NA)
  if (!"duplicate_reason" %in% names(x)) x$duplicate_reason <- NA_character_
  
  # Reviewer-entered field. Existing values are preserved on rerun.
  if (!"reviewer_conclusion" %in% names(x)) x$reviewer_conclusion <- NA_character_
  
  x
}

classify_duplicates <- function(new_signals) {
  if (nrow(new_signals) == 0) return(add_duplicate_columns(new_signals))

  new_signals <- new_signals %>%
    arrange(analysis_date, lag, desc(Recurrence.Interval), Node.Identifier) %>%
    mutate(
      duplicate_class = "unique_initial",
      duplicate_of_node = NA_character_,
      duplicate_of_date = as.Date(NA),
      duplicate_of_cluster_start = as.Date(NA),
      duplicate_reason = NA_character_
    )

  for (i in seq_len(nrow(new_signals))) {
    cur <- new_signals[i, , drop = FALSE]
    prior <- new_signals[seq_len(i - 1), , drop = FALSE]
    prior <- prior[prior$analysis_date <= cur$analysis_date, , drop = FALSE]
    if (nrow(prior) == 0) next

    cand <- prior %>%
      filter(
        Node.Identifier == cur$Node.Identifier,
        !is.na(cluster_start), !is.na(cur$cluster_start),
        abs(as.integer(cluster_start - cur$cluster_start)) <= duplicate_exact_start_days
      )
    if (nrow(cand) > 0) {
      p <- pick_stronger_prior(cur, cand)
      new_signals$duplicate_class[i] <- "auto_duplicate_exact_node_start"
      new_signals$duplicate_of_node[i] <- p$Node.Identifier[1]
      new_signals$duplicate_of_date[i] <- p$analysis_date[1]
      new_signals$duplicate_of_cluster_start[i] <- p$cluster_start[1]
      new_signals$duplicate_reason[i] <- paste0("Same Node.Identifier and cluster_start within +/-", duplicate_exact_start_days, " days")
      next
    }

    cand <- prior %>%
      filter(
        Node.Identifier == cur$Node.Identifier,
        abs(as.integer(analysis_date - cur$analysis_date)) <= duplicate_same_code_detection_days
      )
    if (nrow(cand) > 0) {
      p <- pick_stronger_prior(cur, cand)
      new_signals$duplicate_class[i] <- "manual_review_same_node_shifted_window"
      new_signals$duplicate_of_node[i] <- p$Node.Identifier[1]
      new_signals$duplicate_of_date[i] <- p$analysis_date[1]
      new_signals$duplicate_of_cluster_start[i] <- p$cluster_start[1]
      new_signals$duplicate_reason[i] <- paste0("Same Node.Identifier recurred within ", duplicate_same_code_detection_days, " days but cluster_start not within exact rule")
      next
    }

    branch_hits <- vapply(prior$node_clean, function(z) same_or_close_branch(cur$node_clean, z), logical(1))
    cand <- prior[branch_hits, , drop = FALSE] %>%
      filter(
        !is.na(cluster_start), !is.na(cur$cluster_start),
        abs(as.integer(cluster_start - cur$cluster_start)) <= duplicate_close_branch_days
      )
    if (nrow(cand) > 0) {
      p <- pick_stronger_prior(cur, cand)
      new_signals$duplicate_class[i] <- "manual_review_close_branch_start"
      new_signals$duplicate_of_node[i] <- p$Node.Identifier[1]
      new_signals$duplicate_of_date[i] <- p$analysis_date[1]
      new_signals$duplicate_of_cluster_start[i] <- p$cluster_start[1]
      new_signals$duplicate_reason[i] <- paste0("Close ICD branch and cluster_start within +/-", duplicate_close_branch_days, " days")
      next
    }
  }

  new_signals
}

classify_new_against_existing <- function(new_rows, existing_rows) {
  if (nrow(new_rows) == 0) return(add_duplicate_columns(new_rows))
  existing_rows <- add_duplicate_columns(existing_rows)

  new_rows <- new_rows %>%
    arrange(analysis_date, lag, desc(Recurrence.Interval), Node.Identifier) %>%
    mutate(
      duplicate_class = "unique_initial",
      duplicate_of_node = NA_character_,
      duplicate_of_date = as.Date(NA),
      duplicate_of_cluster_start = as.Date(NA),
      duplicate_reason = NA_character_
    )

  for (i in seq_len(nrow(new_rows))) {
    cur <- new_rows[i, , drop = FALSE]
    prior <- bind_rows(existing_rows, new_rows[seq_len(i - 1), , drop = FALSE])
    prior <- prior[prior$analysis_date <= cur$analysis_date, , drop = FALSE]
    if (nrow(prior) == 0) next

    cand <- prior %>%
      filter(
        Node.Identifier == cur$Node.Identifier,
        !is.na(cluster_start), !is.na(cur$cluster_start),
        abs(as.integer(cluster_start - cur$cluster_start)) <= duplicate_exact_start_days
      )
    if (nrow(cand) > 0) {
      p <- pick_stronger_prior(cur, cand)
      new_rows$duplicate_class[i] <- "auto_duplicate_exact_node_start"
      new_rows$duplicate_of_node[i] <- p$Node.Identifier[1]
      new_rows$duplicate_of_date[i] <- p$analysis_date[1]
      new_rows$duplicate_of_cluster_start[i] <- p$cluster_start[1]
      new_rows$duplicate_reason[i] <- paste0("Same Node.Identifier and cluster_start within +/-", duplicate_exact_start_days, " days")
      next
    }

    cand <- prior %>%
      filter(
        Node.Identifier == cur$Node.Identifier,
        abs(as.integer(analysis_date - cur$analysis_date)) <= duplicate_same_code_detection_days
      )
    if (nrow(cand) > 0) {
      p <- pick_stronger_prior(cur, cand)
      new_rows$duplicate_class[i] <- "manual_review_same_node_shifted_window"
      new_rows$duplicate_of_node[i] <- p$Node.Identifier[1]
      new_rows$duplicate_of_date[i] <- p$analysis_date[1]
      new_rows$duplicate_of_cluster_start[i] <- p$cluster_start[1]
      new_rows$duplicate_reason[i] <- paste0("Same Node.Identifier recurred within ", duplicate_same_code_detection_days, " days but cluster_start not within exact rule")
      next
    }

    branch_hits <- vapply(prior$node_clean, function(z) same_or_close_branch(cur$node_clean, z), logical(1))
    cand <- prior[branch_hits, , drop = FALSE] %>%
      filter(
        !is.na(cluster_start), !is.na(cur$cluster_start),
        abs(as.integer(cluster_start - cur$cluster_start)) <= duplicate_close_branch_days
      )
    if (nrow(cand) > 0) {
      p <- pick_stronger_prior(cur, cand)
      new_rows$duplicate_class[i] <- "manual_review_close_branch_start"
      new_rows$duplicate_of_node[i] <- p$Node.Identifier[1]
      new_rows$duplicate_of_date[i] <- p$analysis_date[1]
      new_rows$duplicate_of_cluster_start[i] <- p$cluster_start[1]
      new_rows$duplicate_reason[i] <- paste0("Close ICD branch and cluster_start within +/-", duplicate_close_branch_days, " days")
      next
    }
  }

  new_rows
}

read_existing_tracker <- function(path) {
  x <- openxlsx::read.xlsx(path, sheet = "new_signals_classified")
  if (nrow(x) == 0) return(x)

  date_cols <- c("analysis_date", "cluster_start", "cluster_end", "duplicate_of_date", "duplicate_of_cluster_start")
  for (cc in intersect(date_cols, names(x))) x[[cc]] <- coerce_excel_date(x[[cc]])

  if ("Node.Identifier" %in% names(x)) x$Node.Identifier <- clean_node_id(x$Node.Identifier)
  if (!"node_clean" %in% names(x) && "Node.Identifier" %in% names(x)) x$node_clean <- node_code(x$Node.Identifier)
  if (!"severity" %in% names(x) && "Node.Identifier" %in% names(x)) x$severity <- node_severity(x$Node.Identifier)
  if ("lag" %in% names(x)) x$lag <- as.character(x$lag)

  add_duplicate_columns(x)
}

write_simple_signal_report <- function(day_signals, analysis_date) {
  out <- signal_report_file(analysis_date)
  wb <- createWorkbook()
  addWorksheet(wb, "Signals")
  writeDataTable(wb, "Signals", day_signals, tableStyle = "TableStyleMedium2")
  freezePane(wb, "Signals", firstRow = TRUE, firstCol = TRUE)
  setColWidths(wb, "Signals", cols = seq_len(ncol(day_signals)), widths = "auto")
  saveWorkbook(wb, out, overwrite = TRUE)
  invisible(out)
}

write_tracker_workbook <- function(combined_tracker, rows_added, all_filtered_signals, daily_collapsed, path, run_start_date, end_date) {
  combined_tracker <- combined_tracker %>%
    add_duplicate_columns() %>%
    arrange(analysis_date, Node.Identifier, lag) %>%
    drop_internal_columns()
  
  rows_added <- rows_added %>%
    add_duplicate_columns() %>%
    drop_internal_columns()
  
  all_filtered_signals <- all_filtered_signals %>%
    drop_internal_columns()
  
  daily_collapsed <- daily_collapsed %>%
    drop_internal_columns()
  
  final_unique <- combined_tracker %>% filter(duplicate_class == "unique_initial")
  manual_review <- combined_tracker %>% filter(grepl("^manual_review", duplicate_class))
  auto_duplicates <- combined_tracker %>% filter(grepl("^auto_duplicate", duplicate_class))

  rule_summary <- data.frame(
    rule = c(
      "Tracker file",
      "First run",
      "Later runs",
      "Input source",
      "Signal filter",
      "New-signal logic",
      "Auto duplicate",
      "Manual review: same node shifted window",
      "Manual review: close branch"
    ),
    definition = c(
      path,
      "If tracker file does not exist, process wc_start_date through final_date and create it",
      "If tracker file exists, process only max(existing analysis_date) + 1 through final_date and append those rows",
      paste0(results_root, "/YYYY-MM-DD/Results_lag<LAG>_YYYY-MM-DD.csv"),
      "RI not missing; RR >= 1.3; RI >= 365 OR admitted 1-* RI >= 100",
      "Assigned using full filtered results history from wc_start_date through final_date; only missing-date rows are appended",
      paste0("Same Node.Identifier and cluster_start within +/-", duplicate_exact_start_days, " days of prior signal"),
      paste0("Same Node.Identifier detected again within ", duplicate_same_code_detection_days, " days but cluster_start shifted"),
      paste0("ICD code shares branch/prefix and cluster_start within +/-", duplicate_close_branch_days, " days")
    )
  )

  run_log <- data.frame(
    run_time = as.character(Sys.time()),
    run_start_date = as.character(as.Date(run_start_date)),
    run_end_date = as.character(as.Date(end_date)),
    rows_added = nrow(rows_added),
    tracker_file = path
  )

  wb <- createWorkbook()
  addWorksheet(wb, "new_signals_classified")
  writeDataTable(wb, "new_signals_classified", combined_tracker, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "rows_added_this_run")
  writeDataTable(wb, "rows_added_this_run", rows_added, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "deduplicated_output")
  writeDataTable(wb, "deduplicated_output", final_unique, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "auto_duplicates")
  writeDataTable(wb, "auto_duplicates", auto_duplicates, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "manual_review")
  writeDataTable(wb, "manual_review", manual_review, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "all_filtered_signals_this_run")
  writeDataTable(wb, "all_filtered_signals_this_run", all_filtered_signals, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "daily_collapsed_this_run")
  writeDataTable(wb, "daily_collapsed_this_run", daily_collapsed, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "run_log")
  writeDataTable(wb, "run_log", run_log, tableStyle = "TableStyleMedium2")
  addWorksheet(wb, "rule_summary")
  writeDataTable(wb, "rule_summary", rule_summary, tableStyle = "TableStyleMedium2")

  for (s in names(wb)) {
    freezePane(wb, s, firstRow = TRUE, firstCol = TRUE)
    setColWidths(wb, s, cols = 1:100, widths = "auto")
  }

  saveWorkbook(wb, path, overwrite = TRUE)
}

run_wc_retrospective_new_signal_dedup <- function(start_date = wc_start_date,
                                                  end_date = wc_end_date,
                                                  lags = initial_lags) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (length(end_date) == 0 || is.na(end_date)) {
    stop("end_date is missing. Set wc_end_date or pass end_date explicitly.")
  }

  if (!file.exists(tracker_file)) {
    existing_tracker <- NULL
    run_start_date <- as.Date(start_date)
  } else {
    existing_tracker <- read_existing_tracker(tracker_file)
    run_start_date <- max(existing_tracker$analysis_date, na.rm = TRUE) + 1
  }

  if (run_start_date > end_date) {
    message("Tracker already current through ", end_date, ": ", tracker_file)
    return(invisible(list(
      new_signals_classified = existing_tracker,
      rows_added = NULL,
      output_file = tracker_file
    )))
  }

  history_dates <- seq(as.Date(start_date), end_date, by = "day")

  all_signals <- purrr::map_dfr(history_dates, function(d) {
    purrr::map_dfr(lags, function(lg) read_one_results(d, lg))
  })

  if (nrow(all_signals) == 0) {
    warning("No filtered TreeScan signals found in ", results_root, " for ", start_date, " to ", end_date)
    return(invisible(NULL))
  }

  all_signals <- assign_new_from_history(all_signals)

  daily_collapsed <- all_signals %>%
    arrange(
      analysis_date,
      Node.Identifier,
      desc(Recurrence.Interval),
      desc(Relative.Risk),
      lag
    ) %>%
    group_by(analysis_date, Node.Identifier) %>%
    mutate(
      lags_all = paste(sort(unique(as.integer(lag))), collapse = ",")
    ) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(lag = lags_all) %>%
    select(-lags_all)
  
  new_signals_to_add <- daily_collapsed %>%
    filter(
      Trend == "1.New",
      analysis_date >= run_start_date,
      analysis_date <= end_date
    ) %>%
    arrange(analysis_date, Node.Identifier)

  if (nrow(new_signals_to_add) == 0) {
    rows_added <- add_duplicate_columns(new_signals_to_add)
    combined_tracker <- if (is.null(existing_tracker)) rows_added else existing_tracker
  } else if (is.null(existing_tracker)) {
    rows_added <- classify_duplicates(new_signals_to_add)
    combined_tracker <- rows_added
  } else {
    rows_added <- classify_new_against_existing(new_signals_to_add, existing_tracker)
    combined_tracker <- bind_rows(existing_tracker, rows_added)
  }

  if (isTRUE(write_retrospective_signal_reports)) {
    day_rows <- daily_collapsed %>% filter(analysis_date >= run_start_date, analysis_date <= end_date)
    by_day <- split(day_rows, day_rows$analysis_date)
    invisible(lapply(names(by_day), function(d) write_simple_signal_report(by_day[[d]], as.Date(d))))
  }

  write_tracker_workbook(
    combined_tracker = combined_tracker,
    rows_added = rows_added,
    all_filtered_signals = all_signals %>% filter(analysis_date >= run_start_date, analysis_date <= end_date),
    daily_collapsed = daily_collapsed %>% filter(analysis_date >= run_start_date, analysis_date <= end_date),
    path = tracker_file,
    run_start_date = run_start_date,
    end_date = end_date
  )

  message("Tracker saved to: ", tracker_file)
  message("Rows added this run: ", nrow(rows_added))

  invisible(list(
    new_signals_classified = combined_tracker,
    rows_added = rows_added,
    output_file = tracker_file
  ))
}

wc_dedup_results <- run_wc_retrospective_new_signal_dedup()
