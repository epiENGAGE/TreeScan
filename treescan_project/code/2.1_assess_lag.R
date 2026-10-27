# Load in the data to assess lag
# Create output path
in_file <- file.path(
  parent_dir,
  "data",
  "datasets",
  paste0("dataset_", final_date, ".rds")
)

# Save data
data <- readRDS(file = in_file)

# What month-year are we in?
year_month <- format(final_date, "%Y-%m")

monthly_lags_assessed <- list.files(
  file.path(parent_dir, "lag", "curves"),
  pattern = "\\.rds$"
)

have_we_got_lag <- isTRUE(paste0("lag_curve_", year_month, ".rds") %in% monthly_lags_assessed)
have_we_got_databycode <- isTRUE(length(list.files(file.path(parent_dir, "data_by_code"))) > 0)

if (!(isTRUE(have_we_got_lag) && isTRUE(have_we_got_databycode))) {

  new_month <- TRUE

  # Load required libraries
  library(data.table)
  library(lubridate)
  library(Rcpp)

  # Load the data for lag
  df_all_for_lag <- readRDS(file.path(parent_dir, "data", "data for lag", "data_for_lag.rds"))

  # Compile the native parser (cached after the first run)
  cpp_file <- paste0(parent_dir, "/code/2.1_optimizer.cpp")
  if (!file.exists(cpp_file)) {
    stop("Put 2.1_optimizer.cpp in parent_dir before running this script.")
  }
  cpp_cache <- file.path(parent_dir, "Temp", "lag_rcpp_cache")
  dir.create(cpp_cache, showWarnings = FALSE, recursive = TRUE)
  Rcpp::sourceCpp(cpp_file, cacheDir = cpp_cache, rebuild = FALSE)

  # Keep only usable rows
  dt <- as.data.table(df_all_for_lag)
  usable <- which(
    !is.na(dt$DischargeDiagnosisUpdates) &
      !is.na(dt$DischargeDiagnosisMDTUpdates) &
      dt$DischargeDiagnosisUpdates != "" &
      dt$DischargeDiagnosisMDTUpdates != ""
  )

  # Parse visit dates once, only for rows that can contribute codes
  visit_time <- ymd_hms(dt$C_Visit_Date_Time[usable], quiet = TRUE)

  # Main cleaning: join event IDs and keep first appearance in C++
  # Rows remain ordered by original row_id, then code
  first_appearance <- as.data.table(fast_lag_first_appearances(
    diag = dt$DischargeDiagnosisUpdates[usable],
    mdt = dt$DischargeDiagnosisMDTUpdates[usable],
    visit_epoch = as.numeric(visit_time),
    original_row = as.integer(usable),
    threads = as.integer(getOption("nssp.lag.threads", 1L))
  ))

  # Tree / ancestor mapping
  tree_year <- format(Sys.Date(), "%Y")
  tree_file <- file.path(parent_dir, "data", paste0("Tree_File_", tree_year, "_wide_format.txt"))

  # Read only the tree columns and names that occur in the lag data
  ancestor_columns <- paste0("Level", 4:8)
  icd_tree <- fread(tree_file, select = c("Name", ancestor_columns))
  icd_tree <- icd_tree[Name %chin% unique(first_appearance$code)]

  ancestor_map <- melt(
    icd_tree,
    id.vars = "Name",
    measure.vars = ancestor_columns,
    value.name = "ancestor_code",
    variable.name = "level"
  )[
    !is.na(ancestor_code) & ancestor_code != "",
    .(Name, ancestor_code)
  ]

  ancestor_map <- unique(ancestor_map)

  # Expand each code to itself + ancestors
  self_map <- unique(first_appearance[, .(code = code, code_for_lag = code)])
  anc_map <- unique(ancestor_map[, .(code = Name, code_for_lag = ancestor_code)])
  map_all <- unique(
    rbindlist(list(self_map, anc_map), use.names = TRUE),
    by = c("code", "code_for_lag")
  )

  # Percentiles by code
  # Collapse duplicate (row_id, code_for_lag, delay_hours) in C++
  # Quantiles use the same default type 7 interpolation as quantile()
  probs <- seq(0.01, 1, by = 0.01)
  pnames <- paste0("p", 1:100)

  q <- fast_lag_group_quantiles(
    first_appearance$row_id,
    first_appearance$code,
    first_appearance$delay_hours,
    map_all$code,
    map_all$code_for_lag,
    probs
  )
  delay_by_code <- as.data.table(c(list(code = q$code), setNames(q$quantiles, pnames)))

  # Create directory to store delay by code
  dir.create(file.path(parent_dir, "data_by_code"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    delay_by_code,
    file.path(parent_dir, "data_by_code", "data_by_code.rds"),
    compress = getOption("nssp.lag.rds.compress", FALSE)
  )

  # Overall delay
  # Keep the base R quantile for exact downstream knee calculations
  overall_delay <- as.data.table(as.list(setNames(
    quantile(first_appearance$delay_hours, probs = probs, na.rm = TRUE, names = FALSE),
    pnames
  )))

  # Knee estimate
  x <- as.numeric(overall_delay[1, 1:95])
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

  optimal_minimal_lag <- ceiling(knee_x / 24)

  # Save the same lag plot
  plot_file <- file.path(parent_dir, "lag", "plots", paste0("lag_curve_", year_month, ".png"))
  dir.create(dirname(plot_file), recursive = TRUE, showWarnings = FALSE)
  png(plot_file, width = 800, height = 600)
  x_days <- x / 24

  plot(x_days, y,
       xlab = "Delay (days)",
       ylab = "Percentage of codes diagnosed (%)",
       type = "l",
       ylim = c(0, 100),
       main = paste0("Lag curve ran on ", final_date, " for data 14-90 days ago"))

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
  dev.off()

  # Save the lag curve
  dir.create(file.path(parent_dir, "lag", "curves"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    data.frame(x_days, y),
    file.path(parent_dir, "lag", "curves", paste0("lag_curve_", year_month, ".rds")),
    compress = getOption("nssp.lag.rds.compress", FALSE)
  )
}
