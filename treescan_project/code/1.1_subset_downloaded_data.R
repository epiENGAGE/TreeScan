time4 <- system.time({
  if (isTRUE(subregion)) {
    
    subset_dir <- file.path(parent_dir, "raw_data_subset")
    dir.create(subset_dir, showWarnings = FALSE, recursive = TRUE)
    
    subset_end_date <- as.Date(final_date)
    subset_start_date <- subset_end_date %m-% lubridate::period(months=16)
    
    # What files do we have from previous days
    files <- list.files(subset_dir)
    
    # What are the dates in that directory
    dates_in_dir <- as.Date(
      stringr::str_extract(files, "\\d{4}-\\d{2}-\\d{2}")
    )
    
    # Get all source files and their dates
    source_files <- list.files(
      file.path(parent_dir, "raw_data"),
      pattern = "\\.csv$",
      full.names = TRUE
    )
    
    source_dates <- as.Date(
      sub(
        ".*NSSP_data_(\\d{4}-\\d{2}-\\d{2})_to_.*",
        "\\1",
        basename(source_files)
      ),
      format = "%Y-%m-%d"
    )
    
    # What dates are we missing
    is_missing <- !(source_dates %in% dates_in_dir)
    
    # Now get 30 most recent dates as they were refreshed
    is_recent <- source_dates >= (subset_end_date - 29L)
    
    # Now what dates need processing
    selected <- which(
      !is.na(source_dates) &
        source_dates >= subset_start_date &
        source_dates <= subset_end_date &
        (is_missing | is_recent)
    )
    
    if (length(selected) > 0L) {
      
      inputs <- source_files[selected]
      outputs <- file.path(subset_dir, basename(inputs))
      
      jobs <- Map(
        function(input, output) list(input = input, output = output),
        inputs,
        outputs
      )
      
      # Use available cores without oversubscribing the CSV parser
      cores <- parallel::detectCores(logical = FALSE) / 2
      if (is.na(cores)) cores <- 1L
      
      workers <- min(length(jobs), cores)
      threads <- if (workers == 1L) data.table::getDTthreads() else 1L
      
      # Loop through only dates that need to subset now
      process_file <- function(job, regions, threads) {
        
        A <- data.table::fread(
          job$input,
          sep = ",",
          header = TRUE,
          na.strings = "NA",
          check.names = TRUE,
          strip.white = FALSE,
          blank.lines.skip = TRUE,
          fill = TRUE,
          integer64 = "double",
          nThread = threads,
          showProgress = FALSE
        )
        
        B <- A[A[["Region"]] %in% regions]
        
        data.table::fwrite(
          B,
          job$output,
          row.names = FALSE,
          col.names = TRUE,
          quote = TRUE,
          na = "NA",
          logical01 = FALSE,
          dateTimeAs = "write.csv",
          nThread = threads,
          showProgress = FALSE
        )
        
        invisible(NULL)
      }
      
      for (date in source_dates[selected]) {
        print(paste0("Reducing file for date ", as.Date(date)))
      }
      
      if (workers == 1L) {
        
        invisible(lapply(
          jobs, process_file,
          regions = which_subregion,
          threads = threads
        ))
        
      } else if (.Platform$OS.type == "windows") {
        
        cl <- parallel::makeCluster(workers)
        
        tryCatch(
          parallel::parLapplyLB(
            cl, jobs, process_file,
            regions = which_subregion,
            threads = threads
          ),
          finally = parallel::stopCluster(cl)
        )
        
      } else {
        
        results <- parallel::mclapply(
          jobs, process_file,
          regions = which_subregion,
          threads = threads,
          mc.cores = workers,
          mc.preschedule = FALSE
        )
        
        if (any(vapply(results, inherits, logical(1), "try-error"))) {
          stop("One or more CSV files failed to process.")
        }
      }
    }
  }
})