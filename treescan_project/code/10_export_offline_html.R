# Export offline dashboard
export_treescan_offline_html <- function(project_dir, final_date,
                                        subregion = TRUE, output_file = NULL) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  day <- as.character(as.Date(final_date))
  if (length(day) != 1L || is.na(day) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", day))
    stop("Supply one valid final_date, e.g. as.Date('2026-11-13').", call. = FALSE)
  if (length(subregion) != 1L || is.na(subregion) || !is.logical(subregion))
    stop("subregion must be TRUE or FALSE.", call. = FALSE)
  scope <- if (subregion) "subregion" else "full"
  for (p in c("openxlsx", "jsonlite", "base64enc")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Required package missing: ", p, ". Run install.packages('", p, "').", call. = FALSE)
  }
  helper_file <- file.path(project_dir, "dashboard", "helpers.R")
  if (!file.exists(helper_file)) stop("Can't find ", helper_file, call. = FALSE)
  # Source helper functions only into this function's environment.
  source(helper_file, local = TRUE)
  template_file <- file.path(project_dir, "dashboard", "offline_template.html")
  if (!file.exists(template_file)) stop("Can't find ", template_file, call. = FALSE)
  catalog <- ts_catalog(project_dir, scope)
  if (!(day %in% ts_dates(catalog)))
    stop("No saved TreeScan outputs for ", scope, " / ", day, call. = FALSE)
  prepared <- ts_ensure_prepared(project_dir, scope, day, verbose = TRUE)
  if (is.null(prepared) || !ts_cache_valid(prepared, scope, day))
    stop("Saved workbook import failed; HTML export cancelled rather than creating an empty dashboard.", call. = FALSE)
  if (!nrow(prepared$report))
    stop("The step-7 Signals worksheet has no rows for this analysis. Export cancelled.", call. = FALSE)
  reports_on_date <- catalog$reports$path[as.character(catalog$reports$date) == day]
  if (length(reports_on_date) != 1L) stop("Expected one saved step-7 report for this date/scope.")
  if (!file.exists(reports_on_date)) stop("Missing step-7 report: ", reports_on_date)
  report <- prepared$report

  # One most-recent interpretation workbook per signal through the analysis day.
  records <- ts_lines_for_date(catalog, day, earlier = TRUE)
  if (!nrow(records)) stop("No step-6 workbooks through ", day, ".", call. = FALSE)
  records <- records[order(records$label),,drop=FALSE]
  records$id <- vapply(as.character(records$label), function(x) {
    x <- sub("^[0-9]+-", "", trimws(x))
    toupper(gsub("[^A-Za-z0-9]", "", x))
  }, character(1))
  if (anyDuplicated(records$id))
    stop("Duplicate normalized signal labels found. Export cancelled to avoid linking wrong signals.")

  # Respect the exact Most recent linelist date from step 7 for each report
  # node if it identifies a different saved step-6 workbook. This mirrors
  # the working Shiny click-through rather than assuming a current-day file.
  normal_id <- function(x) {
    x <- trimws(as.character(x)); x[is.na(x)] <- ""
    toupper(gsub("[^A-Za-z0-9]", "", sub("^[0-9]+-", "", x)))
  }
  find_col <- function(x) {
    key <- tolower(gsub("[^A-Za-z0-9]", "", as.character(x)))
    col <- which(tolower(gsub("[^A-Za-z0-9]", "", names(report))) == key)
    if (length(col) == 1L) report[[col]] else rep(NA_character_,nrow(report))
  }
  report_ids <- find_col("Node.Identifier")
  recent_days <- ts_date(find_col("Most recent linelist"))
  for (j in seq_len(nrow(report))) {
    key <- normal_id(report_ids[j])
    row <- which(records$id == key)
    if (length(row) != 1L || is.na(recent_days[j]) || recent_days[j] > as.Date(day)) next
    exact <- catalog$lines[normal_id(catalog$lines$label) == key &
                           !is.na(catalog$lines$date) & catalog$lines$date == recent_days[j],,drop=FALSE]
    if (nrow(exact) == 1L) {
      for (col in c("path","date","label","signal_key")) records[row,col] <- exact[1,col]
    }
  }

  # The Step-9 validated prepared lines contain current-day workbooks; earlier
  # latest workbooks need importing if the Step-7 report references them.
  current_records <- records[as.character(records$date) == day,,drop=FALSE]
  older_records <- records[as.character(records$date) < day,,drop=FALSE]
  lines_current <- prepared$lines
  if (nrow(lines_current))
    lines_current <- lines_current[as.character(lines_current$.cluster) %in% current_records$id,,drop=FALSE]
  lines_older <- ts_empty_visit_data()
  import_status <- prepared$import_status
  if (nrow(older_records)) {
    message("[Offline] Loading ", nrow(older_records), " earlier latest-workbook line lists...")
    old <- ts_read_lines(older_records)
    lines_older <- old$data
    if (nrow(old$status)) import_status <- ts_bind(list(import_status, old$status))
  }
  if (nrow(lines_current) && nrow(lines_older)) lines <- ts_bind(list(lines_current, lines_older)) else
    if (nrow(lines_current)) lines <- lines_current else lines <- lines_older
  if (!is.data.frame(lines)) lines <- ts_empty_visit_data()
  message("[Offline] Exporting ", nrow(report), " report rows, ",
          nrow(records), " workbooks and ", nrow(lines), " visit rows")

  safe_sheet <- function(path, sheet) tryCatch(ts_xlsx_sheet(path, sheet),
    error = function(e) {
      message("[Offline] Worksheet issue in ", basename(path), " / ", sheet, ": ", conditionMessage(e))
      data.frame()
    })
  safe_codes <- function(path) tryCatch(ts_original_codes(path), error = function(e) {
    message("[Offline] Other_Codes issue in ", basename(path), ": ", conditionMessage(e))
    data.frame()
  })
  # Embed the exact original OOXML media bytes directly from the XLSX ZIP.
  # This bypasses Windows ZIP-extraction path behavior entirely and uses
  # the same verified worksheet-to-image index as the Shiny dashboard.
  zip_members <- function(path) {
    z <- utils::unzip(path, list=TRUE)
    if (!is.data.frame(z) || !all(c("Name", "Length") %in% names(z)))
      stop("Cannot inspect XLSX ZIP members: ",basename(path),call.=FALSE)
    z
  }
  media_uri <- function(path, member, listing) {
    len <- listing$Length[match(member,listing$Name)]
    if (length(len)!=1L || is.na(len) || len<1 || len>100000000)
      stop("Missing or oversized embedded image: ",basename(path)," / ",member,call.=FALSE)
    con <- unz(path,member,open="rb")
    on.exit(close(con),add=TRUE)
    binary <- readBin(con,what="raw",n=as.integer(len))
    if (length(binary)!=as.integer(len))
      stop("Short image read: ",basename(path)," / ",member,call.=FALSE)
    ext <- tolower(tools::file_ext(member))
    typ <- switch(ext, jpg="image/jpeg",jpeg="image/jpeg",gif="image/gif",
                  png="image/png", "application/octet-stream")
    paste0("data:",typ,";base64,",base64enc::base64encode(binary))
  }
  get_figures <- function(path, sheet) {
    idx <- ts_media_index(path, sheet)
    if (!nrow(idx)) return(list())
    if (identical(sheet, "Plots")) idx <- ts_report_plot_titles(path, idx)
    listing <- zip_members(path)
    if (any(!idx$member %in% listing$Name))
      stop("The ",sheet," worksheet references missing images in ",basename(path),call.=FALSE)
    lapply(seq_len(nrow(idx)), function(i) list(
      title=as.character(idx$title[i]),
      src=media_uri(path,as.character(idx$member[i]),listing)))
  }
  # Report node matching uses the same normalization + historical-date concept
  # as v13.4 report-to-signal navigation. Ambiguity is reported, not guessed.
  lookup_key <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x <- sub("^[0-9]+-", "", x)
    toupper(gsub("[^A-Za-z0-9]", "", x))
  }
  get_report_column <- function(d, n) {
    norm <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
    loc <- which(norm(names(d)) == norm(n))
    if (length(loc) == 1L) d[[loc]] else rep(NA_character_, nrow(d))
  }
  rid <- get_report_column(report, "Node.Identifier")
  rn <- get_report_column(report, "Node.Name")
  rdate <- get_report_column(report, "Most recent linelist")
  record_report_indexes <- vector("list", nrow(records))
  report_unmatched <- character()
  for (j in seq_len(nrow(report))) {
    keys <- unique(lookup_key(c(rid[j], rn[j])))
    keys <- keys[nzchar(keys)]
    matches <- which(records$id %in% keys)
    # Prefer the identifier; the name is a fallback only.
    by_id <- which(records$id == lookup_key(rid[j]))
    if (length(by_id)) matches <- by_id
    if (length(matches) != 1L) {
      report_unmatched <- c(report_unmatched, as.character(rid[j]))
      next
    }
    # If a historical date was supplied and conflicts with selected workbook,
    # do NOT misrepresent its provenance; it is surfaced in the audit table.
    record_report_indexes[[matches]] <- c(record_report_indexes[[matches]],j-1L)
  }
  if (length(report_unmatched)) message("[Offline] Report nodes without an unambiguous step-6 match: ",
                                       paste(unique(report_unmatched),collapse=", "))
  signals <- vector("list",nrow(records))
  for (i in seq_len(nrow(records))) {
    path <- as.character(records$path[i])
    message(sprintf("[Offline] [%d/%d] %s",i,nrow(records),basename(path)))
    figures <- list(
      Trends=get_figures(path,"Trends"),
      Maps=get_figures(path,"Maps"),
      Hospitals=get_figures(path,"Hospitals"))
    signals[[i]] <- list(
      id=as.character(records$id[i]),
      label=as.character(records$label[i]),
      scope=scope,
      date=as.character(records$date[i]),
      file=basename(path),
      rows=if (nrow(lines)) sum(as.character(lines$.cluster)==records$id[i],na.rm=TRUE) else 0L,
      reportIndexes=as.list(as.integer(record_report_indexes[[i]])),
      figures=figures,
      sheets=list(Other_Codes=safe_codes(path),
                  Demographics=safe_sheet(path,"Demographics"),
                  TopCC_words=safe_sheet(path,"TopCC_words")),
      error="")
  }
  message("[Offline] Embedding step-7 upload-delay curves...")
  delay_plots <- get_figures(reports_on_date,"Plots")
  dataset <- list(scope=scope, date=day, report=report, signals=signals,
                  lines=lines, reportPlots=delay_plots,
                  importStatus=import_status,
                  unmatchedReportNodes=unique(report_unmatched))
  json <- jsonlite::toJSON(dataset, dataframe="rows", auto_unbox=TRUE,
                           na="null", null="null", Date="ISO8601", POSIXt="ISO8601",
                           digits=NA, force=TRUE)
  # Prevent spreadsheet-provided text from terminating our embedded JSON script.
  json <- paste(strsplit(as.character(json),"<",fixed=TRUE)[[1]],collapse="\\u003c")
  html <- paste(readLines(template_file,warn=FALSE,encoding="UTF-8"),collapse="\n")
  parts <- strsplit(html,"__TREESCAN_DATA__",fixed=TRUE)[[1]]
  if (length(parts)!=2L) stop("Offline HTML template marker is missing or duplicated.")
  out <- paste0(parts[1],json,parts[2])
  if (is.null(output_file)) output_file <- file.path(project_dir,"dashboard","exports",
    paste0("TreeScan_", if (subregion) "Subregion" else "Full", "_",day,".html"))
  dir.create(dirname(output_file),showWarnings=FALSE,recursive=TRUE)
  # Write through a dedicated function so on.exit closes this connection
  # exactly once, before we report the completed output. The previous
  # version closed it explicitly and then called isOpen() after close(),
  # causing an invalid-connection error even after successful export.
  write_html <- function(path, content) {
    con <- file(path, open="wb")
    on.exit(close(con), add=TRUE)
    writeBin(charToRaw(enc2utf8(content)), con)
    invisible(NULL)
  }
  write_html(output_file, out)
  n_fig <- sum(vapply(signals,function(z) sum(vapply(z$figures,length,integer(1))),integer(1))) + length(delay_plots)
  message("[Offline] DONE. Saved single offline HTML: ", normalizePath(output_file,winslash="/",mustWork=TRUE))
  message("[Offline] ",n_fig," original Excel figures embedded; ",nrow(lines)," visit rows; ",
          round(file.info(output_file)$size/1024^2,1)," MB")
  invisible(normalizePath(output_file,winslash="/",mustWork=TRUE))
}

# Running source("code/10_export_offline_html.R") from the project root:
.ts_project <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
.ts_day <- if (exists("final_date",envir=.GlobalEnv,inherits=FALSE)) get("final_date",envir=.GlobalEnv) else
  if (exists("final_Date",envir=.GlobalEnv,inherits=FALSE)) get("final_Date",envir=.GlobalEnv) else NULL
if (is.null(.ts_day)) stop("Set final_date <- as.Date('2026-11-13') before sourcing step 10.",call.=FALSE)
.ts_scope <- if (exists("subregion",envir=.GlobalEnv,inherits=FALSE)) get("subregion",envir=.GlobalEnv) else TRUE
.ts_export <- export_treescan_offline_html(.ts_project,.ts_day,.ts_scope)
rm(.ts_project,.ts_day,.ts_scope,.ts_export)
