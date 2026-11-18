# TreeScan step 9. Uses ONLY final_date from the running pipeline.
# If subregion exists, it changes the default collection; it is NOT required.
# Does not need parent_dir, TreeScan objects, or anything held by steps 1-8.
local({
  if(!exists("final_date",envir=.GlobalEnv,inherits=FALSE))
    stop("Set final_date <- as.Date('2026-11-13') (lowercase d) first.",call.=FALSE)
  day <- tryCatch(as.Date(get("final_date",envir=.GlobalEnv,inherits=FALSE)),error=function(e)NA)
  if(length(day)!=1L || is.na(day)) stop("final_date must be a single valid date",call.=FALSE)
  day <- as.character(day)
  project <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
  helper <- file.path(project,"dashboard","helpers.R")
  if(!file.exists(helper)) stop("Set the working directory to your treescan_project folder. Missing: ",helper,call.=FALSE)
  source(helper,local=TRUE)
  preferred <- "subregion"
  if(exists("subregion",envir=.GlobalEnv,inherits=FALSE)) {
    flag <- get("subregion",envir=.GlobalEnv,inherits=FALSE)
    if(is.logical(flag) && length(flag)==1L && !is.na(flag))
      preferred <- if(isTRUE(flag)) "subregion" else "full"
  } else {
    previous <- tryCatch(readRDS(file.path(project,"dashboard","latest_run.rds")),error=function(e)NULL)
    if(is.list(previous) && length(previous$preferred_scope)==1L &&
       previous$preferred_scope %in% c("subregion","full")) preferred <- previous$preferred_scope
  }
  cat("\nTreeScan dashboard | Prepare ",day," | requested default: ",preferred,"\n",sep="")
  found <- list()
  for(scope in c("subregion","full")) {
    c <- ts_catalog(project,scope)
    if(!day %in% ts_dates(c)) {
      cat("  ",scope,": no saved outputs for ",day,"\n",sep="")
      next
    }
    p <- ts_ensure_prepared(project,scope,day,verbose=TRUE)
    if(is.null(p)) {
      cat("  ",scope,": import FAILED; see messages above\n",sep="")
    } else found[[scope]] <- p
  }
  if(!length(found)) stop("No validated outputs for ",day,"; inspect the saved Excel folders and errors above.",call.=FALSE)
  if(!preferred %in% names(found)) {
    warning("Requested collection ",preferred," has no readable outputs for ",day,
            "; the dashboard will open with the other collection.",call.=FALSE)
    preferred <- names(found)[1]
  }
  manifest <- list(final_date=as.Date(day),preferred_scope=preferred,
                   prepared=basename(ts_cache_file(project,preferred,day)),
                   count=nrow(found[[preferred]]$lines),
                   workbooks=nrow(found[[preferred]]$records),
                   cache_version=12L)
  saveRDS(manifest,file.path(project,"dashboard","latest_run.rds"))
  cat("\nREADY: ",preferred," / ",day,"; ",manifest$count," imported visit rows.\n",sep="")
  cat("Open dashboard: source('code/9_launch_dashboard.R')\n\n")
})
