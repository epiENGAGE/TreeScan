# Load back in the delay curve by code
delay_by_code <- readRDS(file.path(parent_dir, "data_by_code", "data_by_code.rds"))

monthly_lags_assessed2 <- list.files(
  file.path(parent_dir, "lag", "curves"),
  pattern = "\\.rds$"
)

# Try get in overall curve
curve <- readRDS(file.path(
  parent_dir, "lag", "curves",
  monthly_lags_assessed2[length(monthly_lags_assessed2)]
))

# Turn pooled curve into one-row overall_delay object in HOURS
# curve$x_days = quantile times in days
# curve$y      = percentile numbers (1, 2, ..., likely 95)
overall_delay <- as.data.frame(
  setNames(
    as.list(curve$x_days * 24),
    paste0("p", curve$y)
  )
)

# Figure out which percentile columns actually exist in both objects
pcols_delay   <- grep("^p[0-9]+$", names(delay_by_code), value = TRUE)
pcols_overall <- grep("^p[0-9]+$", names(overall_delay), value = TRUE)

pnums_delay   <- as.integer(sub("^p", "", pcols_delay))
pnums_overall <- as.integer(sub("^p", "", pcols_overall))

common_pnums <- sort(intersect(pnums_delay, pnums_overall))
pcols <- paste0("p", common_pnums)

# These are the probabilities actually available
probs <- common_pnums / 100

# Get CDF of code's delay on a time grid
# Keep original interpolation for validation and irregular input rows
get_cdf_on_grid <- function(qvec, time_grid, probs) {
  keep <- !is.na(qvec) & !is.na(probs)

  qvec <- qvec[keep]
  probs_use <- probs[keep]

  if (length(qvec) == 0L) {
    return(rep(NA_real_, length(time_grid)))
  }

  # Order just in case
  ord <- order(qvec, probs_use)
  qvec <- qvec[ord]
  probs_use <- probs_use[ord]

  # Anchor at time 0 with CDF 0
  x <- c(0, qvec)
  y <- c(0, probs_use)

  approx(
    x = x,
    y = y,
    xout = time_grid,
    method = "linear",
    rule = 2,
    ties = "ordered"
  )$y
}

# Get normalized AUC difference metric
# Keep the original function available for downstream calls
auc_diff_score <- function(q_code, q_overall, probs, T = 48, by = 0.5) {
  time_grid <- seq(0, T, by = by)
  F_code <- get_cdf_on_grid(q_code, time_grid, probs)
  F_overall <- get_cdf_on_grid(q_overall, time_grid, probs)
  if (all(is.na(F_code)) || all(is.na(F_overall))) return(NA_real_)
  diff_vals <- F_code - F_overall
  auc <- sum(diff_vals[-1L] + diff_vals[-length(diff_vals)],
             na.rm = TRUE) * by / 2
  auc / T
}

# The pooled curve and the time grid do not change between codes
auc_T <- 48
auc_step <- 0.5
time_grid <- seq(0, auc_T, by = auc_step)

# Pull pooled quantiles
overall_q <- as.numeric(overall_delay[1, pcols, drop = TRUE])
pooled_cdf <- get_cdf_on_grid(overall_q, time_grid, probs)

# Compute artifact score for each code
# Column-major input: each code's quantiles are contiguous in C++
q_matrix <- t(as.matrix(delay_by_code[, ..pcols]))

# Use the compiled engine when available, with the original R calculation as fallback
score_one <- function(q_code) {
  F_code <- get_cdf_on_grid(q_code, time_grid, probs)

  if (all(is.na(F_code)) || all(is.na(pooled_cdf))) {
    return(NA_real_)
  }

  diff_vals <- F_code - pooled_cdf

  # trapezoid rule
  auc <- sum(diff_vals[-1L] + diff_vals[-length(diff_vals)],
             na.rm = TRUE) * auc_step / 2

  # normalize by T
  auc / auc_T
}

n_codes <- ncol(q_matrix)
scores <- rep(NA_real_, n_codes)

if (n_codes > 0L && !all(is.na(pooled_cdf))) {
  cpp_file <- file.path(parent_dir, "code", "2.4_optimizer.cpp")

  if (all(is.finite(pooled_cdf)) &&
      requireNamespace("Rcpp", quietly = TRUE) && file.exists(cpp_file)) {
    cache_dir <- file.path(parent_dir, "code", ".rcpp_cache")
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    compiled <- tryCatch({
      cpp_env <- new.env(parent = globalenv())
      Rcpp::sourceCpp(file = cpp_file, env = cpp_env,
                      cacheDir = cache_dir, showOutput = FALSE,
                      verbose = FALSE)
      cpp_env$nssp_auc_scores_cpp(q_matrix, probs, pooled_cdf,
                                  time_grid, auc_step, auc_T)
    }, error = function(e) {
      warning("C++ engine unavailable; using the original R interpolator: ",
              conditionMessage(e))
      NULL
    })

    if (!is.null(compiled)) {
      scores <- compiled$scores

      # Irregular negative or infinite quantiles use the original R semantics
      if (length(compiled$fallback)) {
        scores[compiled$fallback] <- vapply(
          compiled$fallback,
          function(i) score_one(q_matrix[, i]),
          numeric(1)
        )
      }
    } else {
      scores <- vapply(seq_len(n_codes),
                       function(i) score_one(q_matrix[, i]), numeric(1))
    }
  } else {
    scores <- vapply(seq_len(n_codes),
                     function(i) score_one(q_matrix[, i]), numeric(1))
  }
}

artifact_scores <- tibble::as_tibble(delay_by_code)
artifact_scores[["artifact_score"]] <- scores

dir.create(file.path(parent_dir, "data_artifact_assessment"),
           showWarnings = FALSE, recursive = TRUE)

# Write the same R object without gzip overhead (larger file, usually faster on SSD)
saveRDS(artifact_scores,
        file.path(parent_dir, "data_artifact_assessment", "artifact_scores.rds"),
        compress = FALSE)
