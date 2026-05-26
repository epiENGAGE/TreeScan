normalize_system_metadata <- function(lines) {
  lines <- lines[!grepl("^Generated at:", lines)]
  lines <- lines[!grepl("^Run date:", lines)]
  lines <- lines[!grepl("^ date[[:space:]]+", lines)]
  trimws(lines)
}

next_system_metadata_path <- function(metadata_dir, run_date) {
  base_name <- paste0("session_info_", run_date, ".txt")
  path <- file.path(metadata_dir, base_name)

  if (!file.exists(path)) {
    return(path)
  }

  index <- 2L
  repeat {
    path <- file.path(metadata_dir, paste0("session_info_", run_date, "_", index, ".txt"))
    if (!file.exists(path)) {
      return(path)
    }
    index <- index + 1L
  }
}

latest_system_metadata_path <- function(metadata_dir) {
  files <- list.files(
    metadata_dir,
    pattern = "^session_info_[0-9]{4}-[0-9]{2}-[0-9]{2}.*[.]txt$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    return(NULL)
  }

  files[which.max(file.info(files)$mtime)]
}

record_system_metadata <- function(parent_dir) {
  metadata_dir <- file.path(parent_dir, "system_metadata")
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

  if (!requireNamespace("sessioninfo", quietly = TRUE)) {
    warning(
      "Package 'sessioninfo' is not installed; skipping system metadata capture.",
      call. = FALSE
    )
    return(invisible(NULL))
  }

  run_date <- Sys.Date()
  current_report <- c(
    paste0("Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Run date: ", run_date),
    "",
    capture.output(sessioninfo::session_info())
  )

  latest_path <- latest_system_metadata_path(metadata_dir)

  if (!is.null(latest_path)) {
    latest_report <- readLines(latest_path, warn = FALSE)
    if (identical(
      normalize_system_metadata(current_report),
      normalize_system_metadata(latest_report)
    )) {
      message("System metadata unchanged since ", basename(latest_path), "; no new file written.")
      return(invisible(latest_path))
    }
  }

  output_path <- next_system_metadata_path(metadata_dir, run_date)
  writeLines(current_report, output_path)
  message("Wrote system metadata to ", output_path)
  invisible(output_path)
}
