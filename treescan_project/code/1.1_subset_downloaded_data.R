if (isTRUE(subregion)){
  dir.create(paste0(parent_dir, "/raw_data_subset"))
  
  subset_end_date <- as.Date(final_date)
  subset_start_date <- subset_end_date %m-% months(16)
  
  subset_target_dates <- as.Date(seq.Date(subset_start_date, subset_end_date, by = "day"))
  
  # What files do we have from previous days
  files <- list.files(file.path(parent_dir, "raw_data_subset"))
  
  # What are the dates in that directory
  dates_in_dir <- str_extract(files, "\\d{4}-\\d{2}-\\d{2}") |>
    as.Date()
  
  # What dates are we missing
  missing_dates <- setdiff(subset_target_dates, dates_in_dir)
  
  # Now get 30 most recent dates as they were refreshed
  recent_30_dates <- seq.Date(subset_end_date - 29, subset_end_date, by = "day")
  
  # Now what dates need processing
  dates_to_process <- sort(unique(c(
    missing_dates,
    recent_40_dates
  )))
  
  # Loop through only dates that need to be subset now
  for (i in list.files(paste0(parent_dir, "/raw_data"), pattern = "\\.csv$", full.names = TRUE)) {
    
    date <- as.Date(sub(".*NSSP_data_(\\d{4}-\\d{2}-\\d{2})_to_.*", "\\1", basename(i)))
    
    if (date %in% dates_to_process) {
      print(paste0("Reducing file for date ", date))
      
      A <- read.csv(i)
      B <- A[A$Region %in% which_subregion, ]
      
      i_subset <- sub("/raw_data/", "/raw_data_subset/", i)
      write.csv(B, i_subset, row.names = FALSE)
    }
  }
}
  

