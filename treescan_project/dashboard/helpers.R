# TreeScan Shiny utilities. No global pipeline objects or packages required to source.
# File discovery never opens Excel files.
ts_empty_lines <- function() data.frame(path=character(), date=as.Date(character()),
  label=character(), signal_key=character(), stringsAsFactors=FALSE)
ts_empty_reports <- function() data.frame(path=character(), date=as.Date(character()), stringsAsFactors=FALSE)
# Pick ONE small workbook for the initial preview, without opening any Excel data.
ts_preview_path <- function(records) {
  if (!nrow(records)) return(character())
  sizes <- suppressWarnings(file.info(records$path)$size)
  sizes[!is.finite(sizes)] <- Inf
  records$path[which.min(sizes)]
}
ts_signal_key <- function(x) {
  x <- as.character(x)
  x <- sub("^[0-9]-", "", x)
  toupper(gsub("[^A-Za-z0-9]", "", x))
}
ts_catalog <- function(project, scope) {
  suffix <- if (identical(scope, "subregion")) "_subregion" else ""
  ldir <- file.path(project, paste0("signal_interpretation", suffix))
  rdir <- file.path(project, paste0("signal_report", suffix))
  files <- if (dir.exists(ldir)) list.files(ldir, pattern="^[0-9]{4}-[0-9]{2}-[0-9]{2}_.+\\.xlsx$", full.names=TRUE, ignore.case=TRUE) else character()
  files <- files[!grepl("^~\\$", basename(files))]
  lines <- ts_empty_lines()
  if (length(files)) {
    b <- basename(files)
    st <- substr(b, 1, 10)
    label <- sub("^[0-9]{4}-[0-9]{2}-[0-9]{2}_(.*)\\.xlsx$", "\\1", b, ignore.case=TRUE)
    lines <- data.frame(path=files, date=as.Date(st), label=label,
                        signal_key=ts_signal_key(label), stringsAsFactors=FALSE)
    lines <- lines[!is.na(lines$date) & nzchar(lines$signal_key),,drop=FALSE]
    lines <- lines[order(lines$date, decreasing=TRUE),,drop=FALSE]
  }
  files <- if (dir.exists(rdir)) list.files(rdir, pattern="^Signals_Report_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.xlsx$", full.names=TRUE, ignore.case=TRUE) else character()
  reports <- ts_empty_reports()
  if (length(files)) {
    st <- sub("^Signals_Report_([0-9]{4}-[0-9]{2}-[0-9]{2})\\.xlsx$", "\\1", basename(files), ignore.case=TRUE)
    reports <- data.frame(path=files, date=as.Date(st), stringsAsFactors=FALSE)
    reports <- reports[!is.na(reports$date),,drop=FALSE]
    reports <- reports[order(reports$date, decreasing=TRUE),,drop=FALSE]
  }
  list(lines=lines, reports=reports, folders=c(ldir,rdir))
}
ts_dates <- function(catalog) {
  d <- c(as.character(catalog$lines$date), as.character(catalog$reports$date))
  sort(unique(d[!is.na(d) & nzchar(d)]), decreasing=TRUE)
}
ts_date_default <- function(catalog, preferred=NULL) {
  ld <- sort(unique(as.character(catalog$lines$date)), decreasing=TRUE)
  ld <- ld[!is.na(ld) & nzchar(ld)]
  all <- ts_dates(catalog)
  if (length(preferred)==1L && !is.na(preferred) && preferred %in% all) return(preferred)
  if (length(ld)) return(ld[1])
  if (length(all)) return(all[1])
  "none"
}
ts_lines_for_date <- function(catalog, day, earlier=FALSE) {
  d <- catalog$lines
  if (!nrow(d) || !length(day) || is.na(day) || identical(day, "none")) return(d[FALSE,,drop=FALSE])
  if (isTRUE(earlier)) {
    d <- d[!is.na(d$date) & d$date <= as.Date(day),,drop=FALSE]
    if (nrow(d)) d <- d[!duplicated(d$signal_key),,drop=FALSE]
  } else d <- d[as.character(d$date)==day,,drop=FALSE]
  d[order(d$label),,drop=FALSE]
}
ts_xlsx_sheet <- function(path, sheet) {
  if (!length(path) || is.na(path) || !file.exists(path)) return(data.frame())
  if (!requireNamespace("openxlsx", quietly=TRUE)) stop("Missing openxlsx: install.packages('openxlsx')")
  nm <- openxlsx::getSheetNames(path)
  if (!sheet %in% nm) stop(sprintf("Workbook %s has no sheet named %s. Available sheets: %s",
                                 basename(path), sheet, paste(nm,collapse=", ")))
  as.data.frame(openxlsx::read.xlsx(path, sheet=sheet, detectDates=TRUE),
                check.names=FALSE, stringsAsFactors=FALSE)
}
ts_clean <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | toupper(x) %in% c("NA", "NULL", "N/A", "")] <- NA_character_
  x
}
ts_col <- function(d, col) if (col %in% names(d)) d[[col]] else rep(NA_character_,nrow(d))
ts_date <- function(x) {
  if (inherits(x,"Date")) return(x)
  if (inherits(x,"POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin="1899-12-30"))
  raw <- trimws(as.character(x))
  raw[is.na(raw) | !nzchar(raw)] <- NA_character_
  out <- as.Date(rep(NA_character_,length(raw)))
  serial <- !is.na(raw) & grepl("^[0-9]{5}(\\.[0-9]+)?$",raw)
  if(any(serial)) out[serial] <- as.Date(as.numeric(raw[serial]),origin="1899-12-30")
  for(fmt in c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%d-%b-%Y","%Y%m%d")) {
    take <- !is.na(raw) & is.na(out)
    if(any(take)) out[take] <- as.Date(substr(raw[take],1,if(fmt=="%Y%m%d") 8 else 10),format=fmt)
  }
  out
}
ts_clock <- function(x) {
  if (inherits(x,"POSIXt")) return(format(x,"%H:%M:%S"))
  if (is.numeric(x)) {
    s <- round((x %% 1)*86400)
    ans <- sprintf("%02d:%02d:%02d", (s%/%3600)%%24,(s%%3600)%/%60,s%%60)
    ans[is.na(x)] <- NA_character_
    return(ans)
  }
  t <- ts_clean(x)
  t <- sub("^.*[ T]([0-9]{1,2}:[0-9]{2}:[0-9]{2})$","\\1",t)
  short <- !is.na(t) & grepl("^[0-9]{1,2}:[0-9]{2}$",t)
  t[short] <- paste0(t[short],":00")
  t
}
ts_bind <- function(lst) {
  if (!length(lst)) return(data.frame())
  nms <- unique(unlist(lapply(lst,names), use.names=FALSE))
  lst <- lapply(lst, function(d) {
    missing <- setdiff(nms,names(d))
    for (n in missing) d[[n]] <- rep(NA_character_,nrow(d))
    d[nms]
  })
  rownames(lst[[1]]) <- NULL
  do.call(rbind,lst)
}
# The empty schema is intentional: an output workbook with no visits is NOT
# a broken workbook; it may still contain valid Trends/Maps/Hospitals figures.
ts_empty_visit_data <- function() {
  data.frame(date=as.Date(character()), .cluster=character(),
    .cluster_label=character(),.source_file=character(),.source_path=character(),
    .source_row=integer(),.person_key=character(),.visit_id=character(),
    .linkable=logical(),stringsAsFactors=FALSE)
}
ts_empty_import_status <- function() {
  data.frame(workbook=character(),signal=character(),status=character(),
             imported_rows=integer(),note=character(), stringsAsFactors=FALSE)
}
ts_read_lines <- function(records, progress=NULL) {
  if (!nrow(records)) return(list(data=ts_empty_visit_data(),warnings=character(),status=ts_empty_import_status()))
  lst <- vector("list",nrow(records))
  status <- vector("list",nrow(records))
  errs <- character()
  for(i in seq_len(nrow(records))) {
    p <- records$path[i]
    if(is.function(progress)) progress(i,nrow(records),basename(p))
    outcome <- "error"
    reason <- ""
    count <- 0L
    d <- tryCatch(ts_xlsx_sheet(p,"ClusterLinelist"),error=function(e) {
      reason <<- conditionMessage(e)
      data.frame()
    })
    if(nrow(d) && ncol(d)) names(d) <- trimws(names(d))
    if(!nzchar(reason)) {
      if(!nrow(d) || (ncol(d)==1L && tolower(names(d)[1])=="message")) {
        outcome <- "empty"
        reason <- "No cluster linelist rows (original workbook may still have plots)"
      } else {
        for(nm in c("date","time","key","patientid","hospital","age_group","diagnosiscode1",
                    "age","sex","dispo","dxtype","diagnosiscode")) {
          hit <- which(tolower(names(d))==nm)
          if(!(nm %in% names(d)) && length(hit)==1L) names(d)[hit] <- nm
        }
        if(!"date" %in% names(d)) {
          reason <- paste0("missing date column; found: ",paste(names(d),collapse=", "))
        } else {
          raw_date <- as.character(d$date[1])
          d$date <- ts_date(d$date)
          if(all(is.na(d$date))) {
            reason <- paste0("no readable dates; first value: ",raw_date)
          } else {
            d$.source_path <- normalizePath(p,winslash="/",mustWork=FALSE)
            d$.cluster <- records$signal_key[i]
            d$.cluster_label <- records$label[i]
            d$.source_file <- basename(p)
            d$.source_row <- seq_len(nrow(d))+1L
            person <- ts_clean(ts_col(d,"key"))
            backup <- ts_clean(ts_col(d,"patientid"))
            person[is.na(person)] <- backup[is.na(person)]
            hosp <- ts_clean(ts_col(d,"hospital"))
            clock <- ts_clock(ts_col(d,"time"))
            valid <- !is.na(person) & !is.na(d$date) & !is.na(hosp) & !is.na(clock)
            join <- paste(person,as.character(d$date),hosp,clock,sep=" | ")
            fallback <- paste("unmatched",basename(p),seq_len(nrow(d)),sep=" | ")
            d$.visit_id <- ifelse(valid,join,fallback)
            d$.person_key <- person
            d$.linkable <- valid
            for(n in c("age_group","age","sex","hospital","dispo","dxtype","diagnosiscode1","diagnosiscode"))
              if(!n %in% names(d)) d[[n]] <- rep(NA_character_,nrow(d))
            for(n in setdiff(names(d),c("date",".source_row",".linkable"))) d[[n]] <- as.character(d[[n]])
            lst[[i]] <- d
            outcome <- "imported"
            count <- nrow(d)
            if(any(is.na(d$date))) reason <- sprintf("%d rows have an unreadable date",sum(is.na(d$date)))
          }
        }
      }
    }
    if(identical(outcome,"error")) errs <- c(errs,paste(basename(p),reason,sep=": "))
    status[[i]] <- data.frame(workbook=basename(p),signal=records$label[i],
      status=outcome,imported_rows=as.integer(count),note=reason,stringsAsFactors=FALSE)
  }
  lst <- Filter(Negate(is.null),lst)
  result <- if(length(lst)) ts_bind(lst) else ts_empty_visit_data()
  list(data=result,warnings=errs,status=do.call(rbind,status))
}
ts_dx_codes <- function(value) {
  if (length(value)!=1L || is.na(value) || !nzchar(trimws(as.character(value)))) return(character())
  x <- strsplit(gsub("\\|", "", as.character(value)), ";", fixed=TRUE)[[1]]
  x <- toupper(gsub("[^A-Za-z0-9]","",x))
  x <- substr(x,1,3)
  unique(x[grepl("^[A-Z][0-9][0-9]$",x)])
}
ts_dx_long <- function(d) {
  empty <- data.frame(.cluster=character(), .visit_id=character(), code=character(),stringsAsFactors=FALSE)
  if (!nrow(d)) return(empty)
  dx <- ts_col(d,"diagnosiscode1")
  fallback <- ts_col(d,"diagnosiscode")
  miss <- is.na(dx) | !nzchar(trimws(as.character(dx)))
  dx[miss] <- fallback[miss]
  codes <- lapply(dx,ts_dx_codes)
  n <- lengths(codes)
  if (!any(n)) return(empty)
  x <- data.frame(.cluster=rep(d$.cluster,n),.visit_id=rep(d$.visit_id,n),
                  code=unlist(codes,use.names=FALSE),stringsAsFactors=FALSE)
  unique(x)
}
ts_dx_summary <- function(x) {
  if (!nrow(x)) return(data.frame(code=character(),clusters=integer(),visits=integer(),signals=character()))
  spl <- split(x,x$code)
  d <- do.call(rbind,lapply(names(spl),function(code) {
    y <- spl[[code]]
    data.frame(code=code,clusters=length(unique(y$.cluster)), visits=length(unique(y$.visit_id)),
               signals=paste(sort(unique(y$.cluster)),collapse=", "),stringsAsFactors=FALSE)
  }))
  rownames(d) <- NULL
  d[order(-d$clusters,-d$visits,d$code),,drop=FALSE]
}
ts_pairwise <- function(df) {
  codes <- sort(unique(as.character(df$.cluster)))
  if (length(codes)<2L) return(data.frame())
  cm <- utils::combn(codes,2)
  out <- lapply(seq_len(ncol(cm)),function(i) {
    a <- unique(df$.visit_id[df$.cluster==cm[1,i]])
    b <- unique(df$.visit_id[df$.cluster==cm[2,i]])
    n <- length(intersect(a,b))
    data.frame(cluster_a=cm[1,i],cluster_b=cm[2,i],shared_visits=n,
               union_visits=length(union(a,b)),jaccard=round(n/length(union(a,b)),3))
  })
  do.call(rbind,out)
}
ts_repeats <- function(d) {
  if (!nrow(d)) return(data.frame())
  d <- d[!is.na(d$.person_key) & d$.linkable,,drop=FALSE]
  if (!nrow(d)) return(data.frame())
  spl <- split(d,d$.person_key)
  x <- lapply(names(spl),function(p) {
    y <- spl[[p]]
    if (length(unique(y$.visit_id))<2L) return(NULL)
    data.frame(patient_key=p,visits=length(unique(y$.visit_id)),clusters=length(unique(y$.cluster)),
               first_visit=as.character(min(y$date,na.rm=TRUE)),last_visit=as.character(max(y$date,na.rm=TRUE)))
  })
  x <- Filter(Negate(is.null),x)
  if (!length(x)) return(data.frame())
  z <- do.call(rbind,x)
  z[order(-z$visits),,drop=FALSE]
}
ts_original_codes <- function(path) {
  if (!requireNamespace("openxlsx", quietly=TRUE)) return(data.frame())
  nm <- openxlsx::getSheetNames(path)
  if (!"Other_Codes" %in% nm) return(data.frame())
  side <- function(cols,colnames) {
    d <- tryCatch(openxlsx::read.xlsx(path,sheet="Other_Codes",cols=cols,skipEmptyCols=FALSE),error=function(e)data.frame())
    if (ncol(d)<4L) return(data.frame())
    d <- d[,seq_len(4),drop=FALSE]
    names(d) <- colnames
    d$code <- toupper(trimws(as.character(d$code)))
    d[!is.na(d$code) & grepl("^[A-Z][0-9][0-9]$",d$code),,drop=FALSE]
  }
  a <- side(1:4,c("code","description","cluster_freq","cluster_pct"))
  b <- side(6:9,c("code","baseline_description","baseline_freq","baseline_pct"))
  if (!nrow(a)) return(b)
  if (!nrow(b)) return(a)
  merge(a,b,by="code",all=TRUE,sort=FALSE)
}
ts_images <- function(path) {
  if (!length(path) || is.na(path) || !file.exists(path)) return(character())
  contents <- utils::unzip(path,list=TRUE)$Name
  contents <- contents[grepl("^xl/media/[A-Za-z0-9_.-]+\\.(png|jpeg|jpg|gif)$",contents,ignore.case=TRUE)]
  if (!length(contents)) return(character())
  dest <- tempfile("treescan_images_")
  dir.create(dest)
  utils::unzip(path,files=contents,exdir=dest)
  files <- file.path(dest,contents)
  files[file.exists(files)]
}
# Preserve the interface used by the step-9 manifest preparation script.
scan_outputs <- function(project_dir, scope="subregion") ts_catalog(project_dir,scope)[c("lines","reports")]
empty_catalog <- ts_empty_lines


# ---------- Workbook-embedded image discovery (base R; no extra packages) ----------
# Uses OOXML relationships to identify WHICH worksheet owns each image.
# A flat xl/media listing cannot distinguish Trends, Maps, Hospitals or Plots.
ts_xml_tags <- function(xml, name) {
  pattern <- paste0("<(?:[A-Za-z0-9_]+:)?", name, "\\b[^>]*>")
  m <- gregexpr(pattern, xml, perl=TRUE)[[1]]
  if (length(m)==1L && m[1L]==-1L) return(character())
  regmatches(xml, list(m))[[1]]
}
ts_xml_attr <- function(tag, name) {
  if (!length(tag) || is.na(tag) || !nzchar(tag)) return(NA_character_)
  # A colon is literal in PCRE; keep r:id and r:embed unchanged.
  pattern <- paste0("(?:^|[[:space:]])", name, "[[:space:]]*=[[:space:]]*['\"]([^'\"]+)['\"]")
  m <- regexec(pattern, tag, perl=TRUE)
  hit <- regmatches(tag,m)[[1]]
  if (length(hit)<2L) NA_character_ else hit[2]
}
ts_zip_member <- function(xlsx, member) {
  z <- tryCatch(unz(xlsx, member, open="rb"),error=function(e)NULL)
  if (is.null(z)) return("")
  on.exit(close(z),add=TRUE)
  tryCatch(paste(readLines(z,warn=FALSE,encoding="UTF-8"),collapse=""),error=function(e)"")
}
ts_zip_path <- function(origin,target) {
  if (is.na(target) || !nzchar(target)) return(NA_character_)
  target <- gsub("\\\\","/",target)
  if (startsWith(target,"/")) origin <- "" else origin <- dirname(origin)
  joined <- paste(origin,sub("^/","",target),sep="/")
  parts <- strsplit(joined,"/",fixed=TRUE)[[1]]
  stack <- character()
  for (p in parts) {
    if (!nzchar(p) || p==".") next
    if (p=="..") {if (length(stack)) stack <- head(stack,-1L)}
    else stack <- c(stack,p)
  }
  paste(stack,collapse="/")
}
ts_rels <- function(xlsx, source_xml) {
  rel_path <- paste0(dirname(source_xml),"/_rels/",basename(source_xml),".rels")
  xml <- ts_zip_member(xlsx,rel_path)
  tags <- ts_xml_tags(xml,"Relationship")
  if (!length(tags)) return(data.frame(id=character(),target=character(),type=character()))
  d <- data.frame(id=vapply(tags,ts_xml_attr,character(1),name="Id"),
                  target=vapply(tags,ts_xml_attr,character(1),name="Target"),
                  type=vapply(tags,ts_xml_attr,character(1),name="Type"),
                  mode=vapply(tags,ts_xml_attr,character(1),name="TargetMode"))
  d <- d[!is.na(d$id) & !is.na(d$target) & (is.na(d$mode) | d$mode!="External"),,drop=FALSE]
  d$target <- vapply(d$target,function(t) ts_zip_path(source_xml,t),character(1))
  d
}
ts_extract_anchor <- function(xml) {
  matches <- gregexpr("(?s)<(?:[A-Za-z0-9_]+:)?(?:oneCellAnchor|twoCellAnchor|absoluteAnchor)\\b[^>]*>.*?</(?:[A-Za-z0-9_]+:)?(?:oneCellAnchor|twoCellAnchor|absoluteAnchor)>",xml,perl=TRUE)[[1]]
  if (length(matches)==1L && matches[1L]==-1L) return(character())
  regmatches(xml,list(matches))[[1]]
}
ts_anchor_pos <- function(xml, field) {
  from <- regmatches(xml,regexpr("(?s)<(?:[A-Za-z0-9_]+:)?from\\b[^>]*>.*?</(?:[A-Za-z0-9_]+:)?from>",xml,perl=TRUE))
  if (!length(from) || !nzchar(from)) return(NA_integer_)
  pattern <- paste0("<(?:[A-Za-z0-9_]+:)?",field,">([0-9]+)</(?:[A-Za-z0-9_]+:)?",field,">")
  hit <- regmatches(from,regexec(pattern,from,perl=TRUE))[[1]]
  if(length(hit)<2L) NA_integer_ else as.integer(hit[2])
}
ts_media_title <- function(sheet,row,col,i) {
  if(sheet=="Trends") return(if(!is.na(row) && row<30) "Weekly diagnosis trends (step 6)" else "Temporal observed / expected trend (step 6)")
  if(sheet=="Maps") {
    r <- if(!is.na(row) && row<27) "Cluster" else "Baseline"
    c <- if(!is.na(col) && col<7) "Hospital ZIP" else "Patient ZIP"
    return(paste(c,r,"spatial distribution"))
  }
  if(sheet=="Hospitals") return("Hospital distribution: change in percentage of ED visits (cluster vs baseline)")
  if(sheet=="Plots") return(paste("Original upload-delay distribution, plot",i,"(step 7)"))
  paste(sheet,"image",i)
}
ts_media_index <- function(path, sheet=NULL) {
  empty <- data.frame(sheet=character(),row=integer(),col=integer(),member=character(),title=character(),stringsAsFactors=FALSE)
  if (length(path)!=1L || is.na(path) || !file.exists(path)) return(empty)
  members <- tryCatch(utils::unzip(path,list=TRUE)$Name,error=function(e)character())
  wb <- ts_zip_member(path,"xl/workbook.xml")
  wbrel <- ts_rels(path,"xl/workbook.xml")
  sheets <- ts_xml_tags(wb,"sheet")
  if (!length(sheets)) return(empty)
  result <- list()
  for (s in sheets) {
    sn <- ts_xml_attr(s,"name")
    if (!is.null(sheet) && !(sn %in% sheet)) next
    rid <- ts_xml_attr(s,"r:id")
    wpath <- wbrel$target[match(rid,wbrel$id)]
    if (length(wpath)!=1L || is.na(wpath) || !(wpath %in% members)) next
    sxml <- ts_zip_member(path,wpath)
    dtag <- ts_xml_tags(sxml,"drawing")
    if(!length(dtag)) next
    srel <- ts_rels(path,wpath)
    for(dt in dtag) {
      drid <- ts_xml_attr(dt,"r:id")
      dpath <- srel$target[match(drid,srel$id)]
      if(length(dpath)!=1L || is.na(dpath) || !(dpath %in% members)) next
      drawing <- ts_zip_member(path,dpath)
      drel <- ts_rels(path,dpath)
      anchors <- ts_extract_anchor(drawing)
      if(!length(anchors)) next
      for(a in anchors) {
        image_id <- ts_xml_attr(a,"r:embed")
        im_path <- drel$target[match(image_id,drel$id)]
        if(length(im_path)!=1L || is.na(im_path) || !(im_path %in% members)) next
        if (!grepl("^xl/media/[A-Za-z0-9_.-]+\\.(png|jpg|jpeg|gif)$",im_path,ignore.case=TRUE)) next
        result[[length(result)+1L]] <- data.frame(sheet=sn,row=ts_anchor_pos(a,"row"),
                          col=ts_anchor_pos(a,"col"),member=im_path,stringsAsFactors=FALSE)
      }
    }
  }
  if(!length(result)) return(empty)
  out <- do.call(rbind,result)
  out <- out[order(out$sheet,out$row,out$col,na.last=TRUE),,drop=FALSE]
  rownames(out) <- NULL
  out$title <- vapply(seq_len(nrow(out)),function(i) ts_media_title(out$sheet[i],out$row[i],out$col[i],i),character(1))
  out
}
ts_media_extract <- function(path,sheet,dir=tempfile("treescan_embedded_")) {
  idx <- ts_media_index(path,sheet)
  if(!nrow(idx)) {idx$file <- character();return(idx)}
  dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  utils::unzip(path,files=unique(idx$member),exdir=dir)
  idx$file <- file.path(dir,idx$member)
  idx <- idx[file.exists(idx$file),,drop=FALSE]
  idx
}

# Extract selected indexed images, not the entire workbook (critical for dozens
# of step-7 node-specific PNGs). idx comes from ts_media_index().
ts_media_extract_rows <- function(path,idx,dir=tempfile("ts_media_page_")) {
  if(!is.data.frame(idx) || !nrow(idx)) {
    if(is.data.frame(idx)) idx$file <- character()
    return(idx)
  }
  if(length(path)!=1L || is.na(path) || !file.exists(path)) stop("Workbook not found: ",path)
  dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  utils::unzip(path,files=unique(idx$member),exdir=dir)
  idx$file <- file.path(dir,idx$member)
  idx[file.exists(idx$file),,drop=FALSE]
}

# Read the node heading immediately ABOVE each image in the original step-7 Plots
# sheet. That is the label written by script 7 at block_row. The drawing's PNG
# begins at block_row+1, so its zero-based anchor row equals the label's
# one-based Excel row. Do not assume plot number == report row: script 7 skips
# nodes with invalid / missing delay data.
ts_xml_decode <- function(x) {
  x <- gsub("&quot;",'"',x,fixed=TRUE)
  x <- gsub("&apos;","'",x,fixed=TRUE)
  x <- gsub("&lt;","<",x,fixed=TRUE)
  x <- gsub("&gt;",">",x,fixed=TRUE)
  gsub("&amp;","&",x,fixed=TRUE)
}
ts_xml_blocks <- function(xml,tag) {
  pattern <- paste0("(?s)<(?:[A-Za-z0-9_]+:)?",tag,"\\b[^>]*>.*?</(?:[A-Za-z0-9_]+:)?",tag,">")
  matches <- gregexpr(pattern,xml,perl=TRUE)[[1]]
  if(length(matches)==1L && matches[1L]==-1L) return(character())
  regmatches(xml,list(matches))[[1]]
}
ts_xml_cell_text <- function(cell,shared=character()) {
  if(!length(cell) || is.na(cell) || !nzchar(cell)) return(NA_character_)
  typ <- ts_xml_attr(cell,"t")
  bits <- ts_xml_blocks(cell,"t")
  if(length(bits)) {
    words <- vapply(bits,function(b) ts_xml_decode(gsub("<[^>]+>","",b,perl=TRUE)),character(1))
    val <- paste(words,collapse="")
    if(nzchar(val)) return(val)
  }
  m <- regmatches(cell,regexec("<(?:[A-Za-z0-9_]+:)?v>([^<]*)</(?:[A-Za-z0-9_]+:)?v>",cell,perl=TRUE))[[1]]
  if(length(m)<2L) return(NA_character_)
  if(identical(typ,"s")) {
    i <- suppressWarnings(as.integer(m[2]))+1L
    if(length(i)==1L && !is.na(i) && i>=1L && i<=length(shared)) return(shared[i])
  }
  ts_xml_decode(m[2])
}
ts_excel_col <- function(col) {
  if(is.na(col) || col<1L) return(NA_character_)
  ans <- ""
  while(col>0L) {k <- (col-1L)%%26L;ans <- paste0(LETTERS[k+1L],ans);col <- (col-1L)%/%26L}
  ans
}
# Image captions in script 7 are written by writeData() to the Plots sheet.
# Pictures are then inserted one row below them, in two columns per row-block.
# Prefer original worksheet reading order when every picture has one caption:
# this avoids relying on Excel drawing anchors and read.xlsx's sparse cell offsets.
ts_report_plot_titles <- function(path, idx) {
  if (!is.data.frame(idx) || !nrow(idx)) return(idx)
  if (!"title" %in% names(idx)) idx$title <- rep("Saved delay distribution", nrow(idx))

  clean_labels <- function(v) {
    v <- trimws(as.character(v))
    v <- v[!is.na(v) & nzchar(v)]
    v <- v[!(tolower(v) %in% c("node plots", "x", "v1", "value", "character"))]
    v
  }
  node_labels <- function(v) {
    v <- clean_labels(v)
    # Step 7 uses Node.Identifier, normally "2-A408" etc. The synthetic
    # workbook uses A40_node_1. Reject automatic column names like "X1".
    v[grepl("(^[0-9]+[-:]|[A-Za-z][0-9]{2}|node[_-])", v,
            ignore.case=TRUE, perl=TRUE)]
  }
  # Unlike earlier versions, this read deliberately SKIPS empty rows/columns.
  # It preserves the order of all captions without pretending sparse grid
  # offsets are absolute Excel row/column addresses.
  grid <- tryCatch(openxlsx::read.xlsx(
    path, sheet="Plots", colNames=FALSE,
    skipEmptyRows=TRUE, skipEmptyCols=TRUE
  ), error=function(e) NULL)
  ordered <- character()
  if (is.data.frame(grid) && nrow(grid) && ncol(grid)) {
    # as.vector(t(.)) traverses original rows, left column before right.
    ordered <- node_labels(as.vector(t(as.matrix(grid))))
  }
  if (length(ordered)==nrow(idx)) {
    position <- order(idx$row, idx$col, na.last=TRUE)
    idx$title[position] <- paste0("Node ", ordered,
                                  " — ESSENCE upload-delay curve")
    return(idx)
  }

  # A plot can fail AFTER script 7 writes its heading. If there are more
  # headings than embedded pictures, position-by-sequence is unsafe. In that
  # case, consult worksheet cell addresses and match each caption to the
  # actual picture anchor instead.
  raw <- character()
  wb <- ts_zip_member(path,"xl/workbook.xml")
  sheets <- ts_xml_tags(wb,"sheet")
  sheets <- sheets[vapply(sheets,function(x)
    identical(ts_xml_attr(x,"name"),"Plots"),logical(1))]
  if(length(sheets)) {
    rels <- ts_rels(path,"xl/workbook.xml")
    src <- rels$target[match(ts_xml_attr(sheets[1],"r:id"),rels$id)]
    if(length(src)==1L && !is.na(src)) {
      xml <- ts_zip_member(path,src)
      sx <- ts_zip_member(path,"xl/sharedStrings.xml")
      si <- ts_xml_blocks(sx,"si")
      shared <- if(length(si)) vapply(si,function(node) {
        words <- ts_xml_blocks(node,"t")
        paste(vapply(words,function(w)
          ts_xml_decode(gsub("<[^>]+>","",w,perl=TRUE)),character(1)),collapse="")
      },character(1)) else character()
      # Older extraction incorrectly matched a self-closing <c .../> to the
      # NEXT </c>, losing real cell addresses. Recognize both forms here.
      pat <- "(?s)<(?:[A-Za-z0-9_]+:)?c\\b[^>]*?(?:/>|>.*?</(?:[A-Za-z0-9_]+:)?c>)"
      hit <- gregexpr(pat,xml,perl=TRUE)[[1]]
      if(length(hit) && hit[1]!=-1L) {
        cells <- regmatches(xml,list(hit))[[1]]
        addresses <- vapply(cells,ts_xml_attr,character(1),name="r")
        values <- vapply(cells,ts_xml_cell_text,character(1),shared=shared)
        use <- !is.na(addresses) & !is.na(values) & nzchar(trimws(values))
        raw <- values[use]
        names(raw) <- addresses[use]
      }
    }
  }

  # Try positions directly: zero-based picture row == the original one-based
  # heading row when script 7 inserts the image at block_row + 1.
  for (i in seq_len(nrow(idx))) {
    r0 <- suppressWarnings(as.integer(idx$row[i]))
    c0 <- suppressWarnings(as.integer(idx$col[i]))
    if(is.na(r0) || is.na(c0) || r0<0L || c0<0L) next
    rows <- unique(pmax(1L,c(r0,r0+1L,r0-1L,r0+2L)))
    cols <- unique(pmax(1L,c(c0+1L,c0+2L)))
    labels <- character()
    for (cc in cols) for (rr in rows) {
      val <- raw[paste0(ts_excel_col(cc),rr)]
      if (length(val)==1L && !is.na(val)) labels <- c(labels,val)
    }
    labels <- node_labels(labels)
    if(length(labels)) idx$title[i] <- paste0(
      "Node ", labels[1], " — ESSENCE upload-delay curve")
  }
  # Never invent a node-to-image correspondence when counts differ and cell
  # coordinates are unavailable. The generic original title remains visible.
  idx
}


# ---------- Strictly scoped, date-specific prepared-data cache (v12) ----------
# Cache fingerprint covers the original Excel input files, so a newly written
# run or replaced workbook never silently serves stale visit data.
ts_cache_file <- function(project,scope,day) {
  if(length(scope)!=1L || is.na(scope) || !(scope %in% c("subregion","full")))
    stop("Invalid collection scope",call.=FALSE)
  day <- as.character(as.Date(day))
  if(length(day)!=1L || is.na(day)) stop("Invalid analysis date",call.=FALSE)
  file.path(project,"dashboard",paste0("prepared_",scope,"_",day,".rds"))
}
ts_file_fingerprint <- function(paths) {
  paths <- sort(unique(normalizePath(paths,winslash="/",mustWork=FALSE)))
  if(!length(paths)) return(character())
  info <- file.info(paths)
  paste(paths,ifelse(is.na(info$size),"MISSING",as.character(info$size)),
        ifelse(is.na(info$mtime),"MISSING",format(info$mtime,"%Y-%m-%dT%H:%M:%OS6%z")),sep=" | ")
}
ts_cache_read <- function(project,scope,day) {
  f <- tryCatch(ts_cache_file(project,scope,day),error=function(e)NULL)
  if(is.null(f) || !file.exists(f)) return(NULL)
  tryCatch(readRDS(f),error=function(e){
    message("Unreadable prepared cache: ",f," — ",conditionMessage(e));NULL
  })
}
ts_cache_valid <- function(p,scope,day) {
  is.list(p) && identical(p$scope,scope) && length(p$date)==1L &&
  identical(as.character(p$date),as.character(as.Date(day))) &&
  is.data.frame(p$lines) && is.data.frame(p$records) &&
  is.data.frame(p$report) &&
  (!nrow(p$lines) || all(c("date",".cluster",".cluster_label",".visit_id",
                             ".person_key",".source_file") %in% names(p$lines)))
}
ts_cache_current <- function(p,scope,day,catalog) {
  if(!ts_cache_valid(p,scope,day) || !identical(p$cache_version,12L)) return(FALSE)
  records <- ts_lines_for_date(catalog,as.character(as.Date(day)),FALSE)
  reports <- catalog$reports$path[as.character(catalog$reports$date)==as.character(as.Date(day))]
  same_paths <- identical(sort(as.character(p$records$path)),sort(as.character(records$path)))
  same_paths && identical(p$input_fingerprint,ts_file_fingerprint(c(records$path,reports)))
}
ts_prepare_scope_date <- function(project,scope,day,force=FALSE,verbose=TRUE) {
  day <- as.character(as.Date(day))
  catalog <- ts_catalog(project,scope)
  records <- ts_lines_for_date(catalog,day,FALSE)
  rp <- catalog$reports$path[as.character(catalog$reports$date)==day]
  if(!nrow(records) && !length(rp)) return(NULL)
  p <- ts_cache_read(project,scope,day)
  if(!isTRUE(force) && ts_cache_current(p,scope,day,catalog)) {
    if(verbose) message(sprintf("[Step 9] Using validated cache: %s / %s (%d line-list rows)",
                              scope,day,nrow(p$lines)))
    return(p)
  }
  if(!requireNamespace("openxlsx",quietly=TRUE))
    stop("Missing package openxlsx; run install.packages('openxlsx')",call.=FALSE)
  if(verbose) message(sprintf("[Step 9] Importing %d %s workbook(s) for %s",nrow(records),scope,day))
  result <- ts_read_lines(records,progress=if(isTRUE(verbose)) function(i,n,name)
    message(sprintf("  [%d/%d] %s",i,n,name)) else NULL)
  report_path <- if(length(rp)) rp[1] else NA_character_
  report_error <- if(length(rp)) "" else "No saved step-7 report for this collection and date"
  report <- if(length(rp)) tryCatch(ts_xlsx_sheet(report_path,"Signals"),error=function(e) {
    report_error <<- conditionMessage(e);data.frame()
  }) else data.frame()
  p <- list(cache_version=12L,date=day,scope=scope,records=records,
            lines=result$data,import_status=result$status,warnings=result$warnings,
            report=report,report_path=report_path,report_error=report_error,
            input_fingerprint=ts_file_fingerprint(c(records$path,rp)))
  f <- ts_cache_file(project,scope,day)
  dir.create(dirname(f),recursive=TRUE,showWarnings=FALSE)
  tmp <- tempfile(".prepared_",tmpdir=dirname(f),fileext=".rds")
  on.exit(if(file.exists(tmp)) unlink(tmp),add=TRUE)
  saveRDS(p,tmp,compress=TRUE)
  if(!file.copy(tmp,f,overwrite=TRUE)) stop("Could not save cache: ",f,call.=FALSE)
  if(verbose) {
    message(sprintf("[Step 9] %s / %s: %d rows, %d imported workbooks, %d empty workbooks, %d import errors; report rows=%d",
       scope,day,nrow(p$lines),sum(p$import_status$status=="imported"),
       sum(p$import_status$status=="empty"),sum(p$import_status$status=="error"),nrow(p$report)))
    if(length(p$warnings)) message(paste(p$warnings,collapse="\n"))
  }
  p
}
# Fast when a matching cache already exists; automatically builds from saved
# Excel inputs when the date/scope is new. Does NOT require pipeline variables.
ts_ensure_prepared <- function(project,scope,day,force=FALSE,verbose=TRUE) {
  tryCatch(ts_prepare_scope_date(project,scope,day,force=force,verbose=verbose),
    error=function(e) {
      message("Step-9 import failed for ",scope," / ",day,": ",conditionMessage(e))
      NULL
    })
}
