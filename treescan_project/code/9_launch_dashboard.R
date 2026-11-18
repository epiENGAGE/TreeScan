# TreeScan dashboard launcher -- no TreeScan pipeline variables required.
# Use: source('code/9_launch_dashboard.R') from the treescan_project folder.
local({
  if (!requireNamespace("shiny",quietly=TRUE)) stop("Install shiny: install.packages('shiny')")
  sourced <- unlist(lapply(sys.frames(),function(x) {
    if(is.null(x$ofile)) character() else as.character(x$ofile)
  }),use.names=FALSE)
  own <- sourced[basename(sourced)=="9_launch_dashboard.R"]
  candidates <- c(if(length(own)) dirname(dirname(normalizePath(tail(own,1),winslash="/",mustWork=FALSE))) else character(),
                  getwd(),dirname(getwd()))
  candidates <- unique(normalizePath(candidates,winslash="/",mustWork=FALSE))
  valid <- vapply(candidates,function(x) file.exists(file.path(x,"dashboard","app.R")) &&
                     file.exists(file.path(x,"dashboard","helpers.R")),logical(1))
  if(!any(valid)) stop("Could not locate dashboard/app.R and dashboard/helpers.R. Current folder: ",getwd())
  project <- candidates[which(valid)[1]]
  message("TreeScan project: ",project)
  for (scope in c("subregion","full")) {
    suffix <- if (scope=="subregion") "_subregion" else ""
    d <- file.path(project,paste0("signal_interpretation",suffix))
    message(scope," line-list folder: ",d," [exists=",dir.exists(d),"]")
  }
  missing <- c("DT","openxlsx")[!vapply(c("DT","openxlsx"),requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing)) {
    stop("Missing required packages: ",paste(missing,collapse=", "),
         ". Run install.packages(c(",paste(sprintf("'%s'",missing),collapse=", "),"))")
  }
  # Ensure the default collection/date has a CURRENT prepared file BEFORE Shiny starts.
  # This prevents a blank Shiny screen when step 9 was not run for a newly saved date.
  source(file.path(project,"dashboard","helpers.R"),local=TRUE)
  m <- tryCatch(readRDS(file.path(project,"dashboard","latest_run.rds")),error=function(e)NULL)
  scope <- if(is.list(m) && length(m$preferred_scope)==1L &&
              m$preferred_scope %in% c("subregion","full")) m$preferred_scope else "subregion"
  c <- ts_catalog(project,scope)
  if(!length(ts_dates(c))) {
    alternative <- if(identical(scope,"subregion")) "full" else "subregion"
    if(length(ts_dates(ts_catalog(project,alternative)))) {
      scope <- alternative; c <- ts_catalog(project,scope)
    }
  }
  preferred_day <- if(is.list(m) && length(m$final_date)==1L) as.character(m$final_date) else NULL
  day <- ts_date_default(c,preferred_day)
  if(!identical(day,"none")) {
    message("Validating saved data before opening Shiny: ",scope," / ",day)
    p <- ts_ensure_prepared(project,scope,day,verbose=TRUE)
    if(is.null(p)) message("Import failed. Shiny can still browse saved workbook plots; check console errors.")
    # Keep date/scope aligned with what actually opened (also in a fresh R session).
    saveRDS(list(final_date=as.Date(day),preferred_scope=scope,cache_version=12L),
            file.path(project,"dashboard","latest_run.rds"))
  } else message("No saved analysis workbooks found; Shiny will show an empty-file diagnostic.")
  Sys.setenv(TREESCAN_PROJECT_DIR=project)
  # Force real browser, not RStudio Viewer (which can show a grayed disconnected app).
  # An exception is printed clearly in the R console with an R traceback.
  tryCatch(
    shiny::runApp(appDir=file.path(project,"dashboard"),
      host="127.0.0.1",port=NULL,
      launch.browser=function(url) {
        message("OPEN THIS URL in Chrome or Edge if the RStudio Viewer is gray: ",url)
        utils::browseURL(url)
      }),
    error=function(e) {
      message("\nTreeScan Shiny failed: ",conditionMessage(e))
      traceback(10)
      stop(e)
    }
  )
})
