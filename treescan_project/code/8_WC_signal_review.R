# Retrospective World Cup TreeScan new-signal report
#
# Does:
#   - Reads TreeScan Results_lag*.csv files.
#   - Reads existing Signals_Report_YYYY-MM-DD.xlsx files as inputs only.
#   - Creates/updates one output workbook:
#       parent_dir/retrospective_dedup/WC_new_signal_tracker.xlsx
#   - Writes one sheet only:
#       new_signals_classified
#   - Keeps one earliest row per 3-character ICD stem.
#   - Adds reviewer_categorisation as the final column.
#   - On later runs, preserves existing rows and reviewer_categorisation values,
#     and appends only new rows at the bottom.
#
# Does NOT:
#   - Does not edit or overwrite existing Signals_Report_YYYY-MM-DD.xlsx files.
#   - Does not output duplicate_class or duplicate_reason columns.

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

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

wc_start_date <- as.Date("2026-06-04")
wc_end_date   <- as.Date("2026-07-31")

reviewer_col <- "reviewer_categorisation"

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

dedup_root <- file.path(parent_dir, "retrospective_dedup")
dir.create(dedup_root, recursive = TRUE, showWarnings = FALSE)

tracker_file <- file.path(dedup_root, "WC_new_signal_tracker.xlsx")

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

clean_node_id <- function(x) {
  x <- stringi::stri_replace_all_fixed(as.character(x), "\xa0", "")
  trimws(x)
}

node_code <- function(node_identifier) {
  gsub("\\.", "", sub(".*-", "", clean_node_id(node_identifier)))
}

node_stem <- function(node_identifier) {
  substr(node_code(node_identifier), 1, 3)
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

signal_report_file <- function(analysis_date) {
  file.path(
    signal_report_root,
    paste0("Signals_Report_", as.Date(analysis_date), ".xlsx")
  )
}

first_existing_col <- function(x, possible_names) {
  hit <- possible_names[possible_names %in% names(x)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

ensure_reviewer_col_last <- function(x) {
  if (is.null(x)) return(x)
  
  if (!reviewer_col %in% names(x)) {
    x[[reviewer_col]] <- NA_character_
  }
  
  x %>%
    select(-all_of(reviewer_col), all_of(reviewer_col))
}

drop_internal_columns <- function(x) {
  x %>%
    select(-any_of(c(
      "prev_ri",
      "prev_rr",
      "node_clean",
      "node_stem",
      "severity"
    )))
}

# -------------------------------------------------------------------------
# Existing output reader: preserves manual reviewer column
# -------------------------------------------------------------------------

read_existing_output <- function(path) {
  if (!file.exists(path)) return(NULL)
  
  x <- openxlsx::read.xlsx(path, sheet = "new_signals_classified")
  
  if (is.null(x)) return(NULL)
  
  if (nrow(x) > 0) {
    date_cols <- c("analysis_date", "cluster_start", "cluster_end")
    
    for (cc in intersect(date_cols, names(x))) {
      x[[cc]] <- coerce_excel_date(x[[cc]])
    }
    
    if ("Node.Identifier" %in% names(x)) {
      x$Node.Identifier <- clean_node_id(x$Node.Identifier)
    }
  }
  
  ensure_reviewer_col_last(x)
}

# -------------------------------------------------------------------------
# Read TreeScan results
# -------------------------------------------------------------------------

read_one_results <- function(analysis_date, lag) {
  f <- results_file(analysis_date, lag)
  if (!file.exists(f)) return(NULL)
  
  x <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(x) || nrow(x) == 0) return(NULL)
  
  names(x) <- trimws(names(x))
  
  keep_cols <- c(
    "Node.Identifier",
    "Node.Name",
    "Time.Window.Start",
    "Time.Window.End",
    "Recurrence.Interval",
    "Relative.Risk",
    "Excess.Cases",
    "Cases",
    "Expected.Cases"
  )
  
  x <- x %>%
    select(any_of(keep_cols))
  
  missing_needed <- setdiff(
    c("Node.Identifier", "Recurrence.Interval", "Relative.Risk"),
    names(x)
  )
  
  if (length(missing_needed) > 0) {
    stop(
      "Results file is missing required columns: ",
      paste(missing_needed, collapse = ", "),
      " in ",
      f,
      "\nActual columns: ",
      paste(names(x), collapse = ", ")
    )
  }
  
  x$Node.Identifier <- clean_node_id(x$Node.Identifier)
  x$Recurrence.Interval <- suppressWarnings(as.numeric(x$Recurrence.Interval))
  x$Relative.Risk <- suppressWarnings(as.numeric(x$Relative.Risk))
  
  if ("Excess.Cases" %in% names(x)) {
    x$Excess.Cases <- suppressWarnings(as.numeric(x$Excess.Cases))
  }
  
  if ("Cases" %in% names(x)) {
    x$Cases <- suppressWarnings(as.numeric(x$Cases))
  }
  
  if ("Expected.Cases" %in% names(x)) {
    x$Expected.Cases <- suppressWarnings(as.numeric(x$Expected.Cases))
  }
  
  x %>%
    mutate(
      analysis_date = as.Date(analysis_date),
      lag = as.character(lag),
      node_clean = node_code(Node.Identifier),
      node_stem = node_stem(Node.Identifier),
      severity = node_severity(Node.Identifier),
      cluster_start = suppressWarnings(as.Date(.data$Time.Window.Start)),
      cluster_end = suppressWarnings(as.Date(.data$Time.Window.End)),
      source_results_file = f
    )
}

# -------------------------------------------------------------------------
# Read existing signal reports as input only
# -------------------------------------------------------------------------

read_one_signal_report <- function(analysis_date) {
  f <- signal_report_file(analysis_date)
  if (!file.exists(f)) return(NULL)
  
  sheets <- tryCatch(readxl::excel_sheets(f), error = function(e) character())
  if (length(sheets) == 0) return(NULL)
  
  sheet <- if ("Signals" %in% sheets) "Signals" else sheets[1]
  
  x <- tryCatch(
    readxl::read_xlsx(f, sheet = sheet),
    error = function(e) NULL
  )
  
  if (is.null(x) || nrow(x) == 0) return(NULL)
  
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  names(x) <- trimws(names(x))
  
  node_col <- first_existing_col(
    x,
    c("Node.Identifier", "Node Identifier", "NodeIdentifier", "node_identifier", "node_id")
  )
  
  if (is.na(node_col)) return(NULL)
  
  start_col <- first_existing_col(
    x,
    c("Time.Window.Start", "Time Window Start", "cluster_start", "Cluster Start")
  )
  
  end_col <- first_existing_col(
    x,
    c("Time.Window.End", "Time Window End", "cluster_end", "Cluster End")
  )
  
  out <- data.frame(
    Node.Identifier = clean_node_id(x[[node_col]]),
    analysis_date = as.Date(analysis_date),
    stringsAsFactors = FALSE
  )
  
  out$node_clean <- node_code(out$Node.Identifier)
  out$node_stem <- node_stem(out$Node.Identifier)
  out$severity <- node_severity(out$Node.Identifier)
  
  out$cluster_start <- if (!is.na(start_col)) {
    coerce_excel_date(x[[start_col]])
  } else {
    as.Date(NA)
  }
  
  out$cluster_end <- if (!is.na(end_col)) {
    coerce_excel_date(x[[end_col]])
  } else {
    as.Date(NA)
  }
  
  out$source_signal_report_file <- f
  
  out
}

# -------------------------------------------------------------------------
# Filter qualifying result signals
# -------------------------------------------------------------------------

filter_result_signals <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(x)
  
  x <- x[!is.na(x$Recurrence.Interval), , drop = FALSE]
  x <- x[x$Relative.Risk >= 1.3, , drop = FALSE]
  
  x <- x[
    (x$Recurrence.Interval >= 365) |
      (grepl("^1-", x$Node.Identifier) & x$Recurrence.Interval >= 100),
    ,
    drop = FALSE
  ]
  
  x
}

# -------------------------------------------------------------------------
# Assign 1.New versus not_new
# -------------------------------------------------------------------------

assign_new_from_history <- function(all_signals) {
  if (is.null(all_signals) || nrow(all_signals) == 0) {
    return(all_signals %>% mutate(Trend = character()))
  }
  
  all_signals %>%
    arrange(lag, Node.Identifier, analysis_date) %>%
    group_by(lag, Node.Identifier) %>%
    mutate(
      prev_ri = lag(Recurrence.Interval),
      prev_rr = lag(Relative.Risk),
      Trend = case_when(
        is.na(prev_ri) ~ "1.New",
        
        grepl("^1-", Node.Identifier) &
          !is.na(prev_ri) &
          prev_ri < 100 ~ "1.New",
        
        !grepl("^1-", Node.Identifier) &
          Recurrence.Interval >= 365 &
          (
            (!is.na(prev_ri) & prev_ri < 365) |
              (!is.na(prev_rr) & prev_rr < 1.3)
          ) ~ "1.New",
        
        TRUE ~ "not_new"
      )
    ) %>%
    ungroup()
}

# -------------------------------------------------------------------------
# Collapse duplicate ICD stems in new candidate output
# -------------------------------------------------------------------------

collapse_to_one_row_per_icd_stem <- function(candidate_rows, existing_output = NULL) {
  if (is.null(candidate_rows) || nrow(candidate_rows) == 0) return(candidate_rows)
  
  candidate_rows <- candidate_rows %>%
    mutate(
      node_stem = node_stem(Node.Identifier)
    )
  
  existing_stems <- character()
  
  if (!is.null(existing_output) && nrow(existing_output) > 0 && "Node.Identifier" %in% names(existing_output)) {
    existing_stems <- unique(node_stem(existing_output$Node.Identifier))
    existing_stems <- existing_stems[!is.na(existing_stems) & existing_stems != ""]
  }
  
  candidate_rows %>%
    filter(!node_stem %in% existing_stems) %>%
    arrange(
      node_stem,
      analysis_date,
      desc(Recurrence.Interval),
      desc(Relative.Risk),
      Node.Identifier,
      lag
    ) %>%
    group_by(node_stem) %>%
    slice(1) %>%
    ungroup()
}

# -------------------------------------------------------------------------
# Write one-sheet append-only output
# -------------------------------------------------------------------------

write_tracker_workbook <- function(new_rows, path, existing_output = NULL) {
  new_rows <- new_rows %>%
    arrange(analysis_date, Node.Identifier, lag) %>%
    drop_internal_columns() %>%
    ensure_reviewer_col_last()
  
  if (!is.null(existing_output) && nrow(existing_output) > 0) {
    existing_output <- ensure_reviewer_col_last(existing_output)
    
    all_cols <- union(names(existing_output), names(new_rows))
    
    for (cc in setdiff(all_cols, names(existing_output))) {
      existing_output[[cc]] <- NA
    }
    
    for (cc in setdiff(all_cols, names(new_rows))) {
      new_rows[[cc]] <- NA
    }
    
    combined_output <- bind_rows(
      existing_output[, all_cols, drop = FALSE],
      new_rows[, all_cols, drop = FALSE]
    ) %>%
      ensure_reviewer_col_last()
  } else {
    combined_output <- new_rows %>%
      ensure_reviewer_col_last()
  }
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "new_signals_classified")
  
  writeDataTable(
    wb,
    "new_signals_classified",
    combined_output,
    tableStyle = "TableStyleMedium2"
  )
  
  freezePane(wb, "new_signals_classified", firstRow = TRUE, firstCol = TRUE)
  setColWidths(wb, "new_signals_classified", cols = 1:100, widths = "auto")
  
  saveWorkbook(wb, path, overwrite = TRUE)
  
  invisible(combined_output)
}

# -------------------------------------------------------------------------
# Main runner
# -------------------------------------------------------------------------

run_wc_retrospective_new_signal_dedup <- function(start_date = wc_start_date,
                                                  end_date = wc_end_date,
                                                  lags = initial_lags) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (length(end_date) == 0 || is.na(end_date)) {
    stop("end_date is missing. Set wc_end_date or pass end_date explicitly.")
  }
  
  existing_output <- read_existing_output(tracker_file)
  
  if (!is.null(existing_output) && nrow(existing_output) > 0 && "analysis_date" %in% names(existing_output)) {
    run_start_date <- max(existing_output$analysis_date, na.rm = TRUE) + 1
  } else {
    run_start_date <- start_date
  }
  
  if (run_start_date > end_date) {
    message("Report already current through ", end_date, ": ", tracker_file)
    
    return(invisible(list(
      new_signals_classified = existing_output,
      rows_added = NULL,
      output_file = tracker_file
    )))
  }
  
  history_dates <- seq(start_date, end_date, by = "day")
  
  all_results <- purrr::map_dfr(history_dates, function(d) {
    purrr::map_dfr(lags, function(lg) read_one_results(d, lg))
  })
  
  # Read signal reports as inputs only. They are not edited or overwritten.
  signal_report_inputs <- purrr::map_dfr(history_dates, read_one_signal_report)
  
  if (is.null(all_results) || nrow(all_results) == 0) {
    warning(
      "No TreeScan result rows found in ",
      results_root,
      " for ",
      start_date,
      " to ",
      end_date
    )
    return(invisible(NULL))
  }
  
  all_filtered_signals <- filter_result_signals(all_results)
  
  if (is.null(all_filtered_signals) || nrow(all_filtered_signals) == 0) {
    warning(
      "No filtered TreeScan signals found in ",
      results_root,
      " for ",
      start_date,
      " to ",
      end_date
    )
    return(invisible(NULL))
  }
  
  all_filtered_signals <- assign_new_from_history(all_filtered_signals)
  
  daily_collapsed <- all_filtered_signals %>%
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
  
  candidate_new_signals <- daily_collapsed %>%
    filter(
      Trend == "1.New",
      analysis_date >= run_start_date,
      analysis_date <= end_date
    ) %>%
    arrange(
      analysis_date,
      node_stem,
      desc(Recurrence.Interval),
      desc(Relative.Risk),
      Node.Identifier
    )
  
  rows_to_append <- collapse_to_one_row_per_icd_stem(
    candidate_rows = candidate_new_signals,
    existing_output = existing_output
  )
  
  combined_output <- write_tracker_workbook(
    new_rows = rows_to_append,
    path = tracker_file,
    existing_output = existing_output
  )
  
  message("Report saved to: ", tracker_file)
  message("Run start date: ", run_start_date)
  message("Candidate new signals this run before duplicate removal: ", nrow(candidate_new_signals))
  message("Rows appended this run: ", nrow(rows_to_append))
  message("Total rows now in report: ", nrow(combined_output))
  
  invisible(list(
    new_signals_classified = combined_output,
    rows_added = rows_to_append,
    signal_report_inputs_read = signal_report_inputs,
    output_file = tracker_file
  ))
}

wc_dedup_results <- run_wc_retrospective_new_signal_dedup()