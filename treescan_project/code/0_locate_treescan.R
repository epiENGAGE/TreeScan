locate_treescan_binary <- function(parent_dir, server = FALSE) {
  # TREESCAN_BIN lets power users, CI, or self-hosted runners point at a binary
  # outside the repo. The committed repo intentionally ignores executables.
  env_bin <- Sys.getenv("TREESCAN_BIN", unset = "")
  candidates <- character()

  if (nzchar(env_bin)) {
    candidates <- c(candidates, env_bin)
  }

  sysname <- Sys.info()[["sysname"]]

  # TreeScan 2.4 uses different executable names by platform. Prefer the file
  # that can run on the current OS, especially when both macOS and Linux copies
  # exist locally for testing.
  default_candidates <- switch(
    sysname,
    "Darwin" = c(
      file.path(parent_dir, "TS_linux", "treescan"),
      file.path(parent_dir, "TS_linux", "treescan64"),
      file.path(parent_dir, "TS_windows", "treescan64.exe"),
      file.path(parent_dir, "TS_windows", "treescan.exe")
    ),
    "Linux" = c(
      file.path(parent_dir, "TS_linux", "treescan64"),
      file.path(parent_dir, "TS_linux", "treescan"),
      file.path(parent_dir, "TS_windows", "treescan64.exe"),
      file.path(parent_dir, "TS_windows", "treescan.exe")
    ),
    c(
      file.path(parent_dir, "TS_windows", "treescan64.exe"),
      file.path(parent_dir, "TS_windows", "treescan.exe"),
      file.path(parent_dir, "TS_linux", "treescan64"),
      file.path(parent_dir, "TS_linux", "treescan")
    )
  )

  candidates <- c(candidates, default_candidates)

  candidates <- unique(path.expand(candidates))
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop(
      "TreeScan binary not found. Expected TREESCAN_BIN or one of: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }

  treescan_bin <- normalizePath(existing[[1]], mustWork = TRUE)

  # Downloaded binaries often lose their executable bit after copying.
  Sys.chmod(treescan_bin, mode = "0755")
  treescan_bin
}

# create the directory
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

# locate treescan
treescan_bin <- locate_treescan_binary(parent_dir, server = server)

