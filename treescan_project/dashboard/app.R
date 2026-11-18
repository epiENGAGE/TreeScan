# TreeScan Signal Review | report-first interface
# Source data and cache: original, unchanged v12.2 helpers.R.
# All original step-6 and step-7 images are read from saved XLSX files, never redrawn.
if (!requireNamespace("shiny", quietly=TRUE)) stop("Install shiny")
if (!requireNamespace("DT", quietly=TRUE)) stop("Install DT")
source("helpers.R", local=TRUE)

project <- Sys.getenv("TREESCAN_PROJECT_DIR", unset="")
if (!nzchar(project)) project <- normalizePath("..", winslash="/", mustWork=FALSE)
if (!dir.exists(project)) stop("Cannot find the TreeScan project directory")
manifest <- tryCatch(readRDS(file.path(project,"dashboard","latest_run.rds")),error=function(e)NULL)
preferred_scope <- if (is.list(manifest) && length(manifest$preferred_scope)==1L &&
  manifest$preferred_scope %in% c("subregion","full")) manifest$preferred_scope else "subregion"
preferred_day <- if (is.list(manifest) && length(manifest$final_date)==1L)
  as.character(manifest$final_date) else NULL
initial_catalog <- ts_catalog(project,preferred_scope)
if (!length(ts_dates(initial_catalog))) {
  alt_scope <- if (identical(preferred_scope,"subregion")) "full" else "subregion"
  alt_catalog <- ts_catalog(project,alt_scope)
  if (length(ts_dates(alt_catalog))) {
    preferred_scope <- alt_scope
    initial_catalog <- alt_catalog
  }
}
initial_date <- ts_date_default(initial_catalog, preferred_day)
initial_prepared <- if (!identical(initial_date,"none"))
  ts_ensure_prepared(project,preferred_scope,initial_date,verbose=FALSE) else NULL
if (!ts_cache_valid(initial_prepared,preferred_scope,initial_date)) initial_prepared <- NULL
empty_bundle <- function() list(lines=ts_empty_visit_data(), records=ts_empty_lines(),
  report=data.frame(), import_status=ts_empty_import_status(),warnings=character(),report_error="")
init_bundle <- if (is.null(initial_prepared)) empty_bundle() else initial_prepared
init_records <- ts_lines_for_date(initial_catalog,initial_date,FALSE)
init_paths <- init_records$path
focus_choices_for <- function(cat,day) {
  d <- ts_lines_for_date(cat,day,TRUE)
  if (!nrow(d)) return(stats::setNames(character(),character()))
  labels <- paste0(as.character(d$label)," · ",as.character(d$date))
  stats::setNames(as.character(d$path),labels)
}
init_group <- head(init_paths, min(2L,length(init_paths)))
init_focus_choices <- focus_choices_for(initial_catalog,initial_date)
init_focus_path <- if (length(init_paths)) init_paths[1] else
  if (length(init_focus_choices)) unname(init_focus_choices[1]) else ""
choices_for <- function(rec) stats::setNames(as.character(rec$path),as.character(rec$label))
not_empty <- function(x) length(x)==1L && !is.na(x) && nzchar(as.character(x))
get_column <- function(d,n) if (n %in% names(d)) d[[n]] else rep(NA_character_,nrow(d))
fmt_int <- function(x) format(as.integer(x),big.mark=",",trim=TRUE)
normal_value <- function(x) {x <- as.character(x); x[is.na(x)] <- ""; x}

# Step 6 saves an ICD node as YYYY-MM-DD_<code>.xlsx, usually after
# removing the TreeScan level prefix and dots. Step 7 records the date of its
# latest available step-6 workbook in `Most recent linelist`.
# A report row MUST resolve by both node identity and date, within the current
# scope; a failed or ambiguous match must never navigate to another node.
report_signal_key <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x <- sub("^[0-9]+-", "", x)
  toupper(gsub("[^A-Za-z0-9]", "", x))
}
report_signal_candidates <- function(id, node_name="") {
  ids <- unique(c(as.character(id),as.character(node_name)))
  ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
  if (!length(ids)) return(character())
  # Prefer the actual Node.Identifier; Node.Name is a secondary fallback.
  unique(report_signal_key(ids))
}
report_link <- function(id, records, analysis_date, recent_date=NA_character_,
                        node_name="") {
  fail <- function(reason) list(path="",status=reason,date=NA_character_)
  if (!not_empty(id)) return(fail("No node identifier in the selected report row"))
  if (!is.data.frame(records) || !nrow(records))
    return(fail("There are no step-6 workbooks in this collection"))
  day <- suppressWarnings(as.Date(analysis_date))
  if (length(day)!=1L || is.na(day)) return(fail("Invalid selected analysis date"))
  d <- records[!is.na(records$date) & records$date<=day & file.exists(records$path),,drop=FALSE]
  if (!nrow(d)) return(fail("No saved step-6 workbooks on or before this date"))
  keys <- report_signal_candidates(id,node_name)
  for (key in keys) {
    matches <- d[report_signal_key(d$label)==key,,drop=FALSE]
    if (!nrow(matches)) next
    recent <- if (inherits(recent_date,"Date")) recent_date else
      if (is.numeric(recent_date)) ts_date(recent_date) else
      suppressWarnings(as.Date(as.character(recent_date),format="%Y-%m-%d"))
    if (length(recent)==1L && !is.na(recent) && recent<=day &&
        any(matches$date==recent)) {
      matches <- matches[matches$date==recent,,drop=FALSE]
    } else {
      # Prefer same-day step 6. Otherwise use the most recent earlier saved
      # interpretation workbook as explicitly anticipated by script 7.
      matches <- matches[matches$date==max(matches$date),,drop=FALSE]
    }
    if (nrow(matches)!=1L)
      return(fail(sprintf("More than one step-6 workbook matches node %s on %s",id,as.character(matches$date[1]))))
    return(list(path=as.character(matches$path[1]),status="matched",
                date=as.character(matches$date[1])))
  }
  fail(sprintf("No step-6 workbook matches report node %s in this collection through %s",id,day))
}
report_value <- function(d,column,i,default=NA_character_) {
  if (!is.data.frame(d) || !nrow(d) || i<1L || i>nrow(d)) return(default)
  normalize <- function(x) tolower(gsub("[^A-Za-z0-9]","",as.character(x)))
  names_normal <- normalize(names(d))
  col <- which(names_normal==normalize(column))
  if(length(col)!=1L) return(default)
  d[[col]][i]
}
match_node_path <- function(id,records) {
  if (!is.data.frame(records) || !nrow(records)) return(NA_character_)
  result <- report_link(id,records,max(records$date,na.rm=TRUE))
  if (nzchar(result$path)) result$path else NA_character_
}

# The step-7 Signals worksheet is the source of truth. Never silently remove
# columns from the default report view. Its original Trend values are categorical:
# "1.New", "2.Increasing", "3.Maximum-outlier", "4.Maximum",
# "5.Stable", and "6.Decreasing". We do not recalculate them.
trend_levels <- c("1.New","2.Increasing","3.Maximum-outlier",
                  "4.Maximum","5.Stable","6.Decreasing")
trend_colors <- c("#C6EFCE","#FFEB9C","#FFC7CE",
                  "#F4CCCC","#EDF2F7","#E3F0FB")
report_table_data <- function(d,trend="") {
  if (!is.data.frame(d)) return(data.frame())
  if (!"Trend" %in% names(d)) return(d)
  # Always display Trend FIRST, retaining every other column and its order.
  d <- d[,c("Trend",setdiff(names(d),"Trend")),drop=FALSE]
  if (length(trend)==1L && !is.na(trend) && nzchar(as.character(trend))) {
    d <- d[!is.na(d$Trend) & as.character(d$Trend)==as.character(trend),,drop=FALSE]
  }
  d
}

css <- '
:root {--navy:#14283f;--ink:#23364b;--muted:#687a8e;--line:#e5eaf1;--blue:#2877b8;--sea:#0d8a87;--bg:#f3f6fb;--card:#fff;}
html,body {background:var(--bg);color:var(--ink);font-family:Inter,"Segoe UI",Arial,sans-serif;font-size:14px;}
.container-fluid {max-width:1720px;margin:auto;padding:0 26px 30px;}
.top-hero {background:linear-gradient(115deg,#14283f,#1c4360);border-radius:0 0 22px 22px;color:white;padding:27px 30px 20px;margin:0 -4px 22px;box-shadow:0 9px 22px rgba(20,40,63,.12);}
.eyebrow {font-size:10px;letter-spacing:.15em;font-weight:800;text-transform:uppercase;color:#a4d4e2;}
.hero-title {font-size:27px;font-weight:800;letter-spacing:-.035em;line-height:1.22;margin:5px 0 5px;color:white;}
.hero-sub {font-size:13px;color:#cbdfec;max-width:760px;}
.hero-controls {display:flex;gap:12px;justify-content:flex-end;align-items:flex-end;flex-wrap:wrap;}
.hero-controls .form-group {margin-bottom:0;min-width:184px;}
.hero-controls label {color:#c6e0ec;font-size:11px;font-weight:700;letter-spacing:.02em;}
.hero-controls .selectize-input,.hero-controls .form-control {border:1px solid #6d91a5;background:#fff;box-shadow:none;border-radius:9px;min-height:37px;}
.nav-tabs {display:flex;flex-wrap:wrap;gap:5px;border-bottom:1px solid #dfe7ee;padding:0 0 10px;margin:0 0 18px;}
.nav-tabs>li {float:none;}.nav-tabs>li>a {border:0!important;border-radius:10px!important;padding:10px 15px;color:#62778b;font-weight:700;background:transparent;margin:0;}
.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover {background:#e5effa!important;color:#17598e!important;box-shadow:none;}
.nav-tabs>li>a:hover {background:#edf2f7;border:0;}
.section-title {font-size:22px;color:var(--navy);font-weight:800;letter-spacing:-.03em;margin:0 0 5px;}
.section-lede {font-size:13px;color:var(--muted);margin:0 0 17px;line-height:1.6;}
.section-row {display:flex;align-items:center;justify-content:space-between;gap:12px;flex-wrap:wrap;margin-bottom:12px;}
.metric-grid {display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:13px;margin-bottom:20px;}
.metric {background:white;border:1px solid var(--line);border-radius:15px;padding:19px 21px;box-shadow:0 3px 12px rgba(39,66,90,.035);}
.metric .number {font-size:28px;color:var(--navy);font-weight:800;letter-spacing:-.04em;line-height:1.2;}
.metric .caption {color:var(--muted);font-size:12px;margin-top:5px;}
.card {background:white;border:1px solid var(--line);border-radius:15px;padding:20px 22px;margin-bottom:16px;box-shadow:0 4px 14px rgba(39,66,90,.035);}
.card h3,.card h4 {font-weight:800;color:var(--navy);margin:0 0 11px;font-size:16px;}
.card h5 {color:var(--navy);font-weight:700;}
.card-sub {color:var(--muted);font-size:12px;line-height:1.5;margin:0 0 13px;}
.button-row {display:flex;gap:9px;align-items:center;flex-wrap:wrap;margin:12px 0;}
.btn {font-weight:700;border-radius:9px;padding:8px 12px;}.btn-primary {background:#206ca8;border-color:#206ca8;}.btn-primary:hover {background:#175b91;}
.btn-default {border-color:#cbd7e1;color:#36536d;background:#fff;}
.alert-soft {border:1px solid #c5dae9;background:#eaf4fb;color:#224e71;padding:13px 15px;border-radius:12px;margin-bottom:15px;font-size:12px;}
.alert-warn {border-color:#f1d2b1;background:#fff8ef;color:#875923;white-space:pre-wrap;}
.inline-stat {font-size:12px;color:var(--muted);}
.filter-grid {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:9px 16px;}
.filter-grid .form-group {margin-bottom:8px;}
.filter-grid label,.control-row label {font-size:12px;color:#53687c;font-weight:700;}
.report-note {background:#f0f6fb;border-left:3px solid #2e85b4;padding:10px 12px;margin:12px 0;font-size:12px;color:#4a6479;}
.figuregrid {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px;align-items:start;}
.figurecard {padding:12px;border:1px solid var(--line);border-radius:12px;background:#fff;overflow:hidden;}
.figurecard h4 {font-size:13px;line-height:1.45;overflow-wrap:anywhere;min-height:32px;margin:0 0 10px;color:var(--navy);}
.figurecard img {max-width:100%;height:auto;}
.figurecard .shiny-image-output {max-width:100%;min-height:120px;}
.muted {font-size:12px;color:var(--muted)}.status-pill {display:inline-block;padding:4px 9px;font-weight:750;font-size:11px;border-radius:8px;background:#e4f5ef;color:#247155;}
.status-pill.warning {background:#fff0d8;color:#865821;}
.tab-content {min-height:250px;}.form-control {border-radius:8px;}.selectize-input {border-radius:8px;}
.dataTables_wrapper {font-size:12px;}table.dataTable tbody tr.selected {background:#dceefb!important;}
.hint {padding:12px 14px;border-radius:10px;background:#f7f9fc;color:#607487;font-size:12px;}
.trend-chips {display:flex;align-items:center;flex-wrap:wrap;gap:6px;margin:4px 0 12px;}
.trend-chip {border-radius:7px;padding:5px 9px;font-size:11px;font-weight:700;color:#23364b;}
details.filters {background:#fff;border:1px solid var(--line);border-radius:12px;margin:13px 0 17px;padding:14px 17px;}
details.filters summary {font-weight:750;cursor:pointer;color:#315675;}
details.filters[open] summary {margin-bottom:14px;}
@media(max-width:1100px){.metric-grid {grid-template-columns:repeat(2,1fr);}.hero-controls {justify-content:flex-start;}}
@media(max-width:750px){.container-fluid{padding:0 12px 20px;}.top-hero{padding:18px 16px;}.metric-grid,.figuregrid,.filter-grid{grid-template-columns:1fr;}.hero-title{font-size:22px;}.card{padding:15px;}}
'
metric_box <- function(label,out) shiny::div(class="metric",
  shiny::div(class="number",shiny::textOutput(out,inline=TRUE)),shiny::div(class="caption",label))
card <- function(...,class="") shiny::div(class=paste("card",class),...)
section_head <- function(title,desc) shiny::div(shiny::h2(class="section-title",title),shiny::p(class="section-lede",desc))
make_filter_controls <- function(prefix) shiny::tags$details(class="filters",
  shiny::tags$summary("Filter visit records  ·  age, hospital, sex, diagnosis, dates and other columns"),
  shiny::div(class="filter-grid",
    shiny::uiOutput(paste0(prefix,"_date_ui")),
    shiny::selectizeInput(paste0(prefix,"_age"),"Age group",choices=character(),multiple=TRUE),
    shiny::selectizeInput(paste0(prefix,"_hospital"),"Hospital",choices=character(),multiple=TRUE),
    shiny::selectizeInput(paste0(prefix,"_sex"),"Sex",choices=character(),multiple=TRUE),
    shiny::selectInput(paste0(prefix,"_dispo"),"Disposition",choices=c("All"="","Admitted (A)"="A","Visit (V)"="V")),
    shiny::selectInput(paste0(prefix,"_incident"),"Diagnosis incident type",choices=c("All"="","0"="0","1"="1","-1"="-1")),
    shiny::selectInput(paste0(prefix,"_col"),"Other line-list column",choices=c("None"="")),
    shiny::textInput(paste0(prefix,"_contains"),"Column contains",value=""),
    shiny::checkboxInput(paste0(prefix,"_repeat"),"Repeat-visit patients only",FALSE)
  ))

ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$meta(name="viewport",content="width=device-width, initial-scale=1"),
                   shiny::tags$style(shiny::HTML(css))),
  shiny::div(class="top-hero",
    shiny::fluidRow(
      shiny::column(7,shiny::div(class="eyebrow","Public health  /  surveillance analytics"),
        shiny::div(class="hero-title","TreeScan Signal Review"),
        shiny::div(class="hero-sub","Dashboard for analysis TreeScan signals.")),
      shiny::column(5,shiny::div(class="hero-controls",
        shiny::selectInput("scope","Collection",choices=c("Subregion"="subregion","Full jurisdiction"="full"),selected=preferred_scope),
        shiny::selectInput("analysis_date","Analysis date",choices=if(length(ts_dates(initial_catalog))) ts_dates(initial_catalog) else c("No saved dates"="none"),selected=initial_date)
      ))
    )),
  shiny::uiOutput("system_message"),
  shiny::tabsetPanel(id="workspace",type="tabs",
    shiny::tabPanel(title="Daily signal report",value="report",
      section_head("Daily signal report","Your step-7 Signals worksheet is the starting point. Click a row to open its investigation. Use the other report views to review artifact scores and upload-delay curves."),
      shiny::div(class="metric-grid",
        metric_box("Signals in report","kpi_signals"),metric_box("Step-6 signal workbooks","kpi_workbooks"),
        metric_box("Imported signal line-list rows","kpi_rows"),metric_box("Scores available","kpi_scores")),
      shiny::tabsetPanel(id="report_view",selected="queue",
        shiny::tabPanel("Signal queue",value="queue",
          card(shiny::div(class="section-row",shiny::h3("Complete original step-7 report"),
               shiny::span(class="status-pill","Click a row to investigate")),
            shiny::p(class="card-sub","Complete original step-7 Signals report, with Trend first. Search, filter or click a row to investigate; RI/RR history and all other report columns remain available."),
            shiny::fluidRow(
              shiny::column(4,shiny::selectInput("report_trend","Signal trend classification",
                choices=c("All classifications"="", stats::setNames(trend_levels,trend_levels)),selected="")),
              shiny::column(8,shiny::uiOutput("report_trend_breakdown"))
            ),
            shiny::uiOutput("report_trend_warning"),
            DT::DTOutput("report_queue"),
            shiny::div(class="button-row",shiny::downloadButton("dl_report","Download complete original step-7 report CSV"))
          )
        ),
        shiny::tabPanel("Artifact scores",value="artifact",
          card(shiny::h3("Artifact scores from the Signals worksheet"),
               shiny::p(class="card-sub","These are original numeric artifact scores, not reconstructed probabilities or a replacement for epidemiological review."),
               shiny::plotOutput("artifact_chart",height="570px"))
        ),
        shiny::tabPanel("Upload-delay curves",value="delays",
          card(shiny::h3("Original upload-delay distributions"),
               shiny::p(class="card-sub","All images embedded in step 7's Plots worksheet. Curves are indexed by saved node labels and loaded a page at a time."),
               shiny::fluidRow(
                 shiny::column(7,shiny::textInput("delay_search","Search node / ICD code",placeholder="e.g. A408")),
                 shiny::column(3,shiny::selectInput("delay_page_size","Curves per page",choices=c("6"="6","12"="12","24"="24"),selected="6")),
                 shiny::column(2,shiny::numericInput("delay_page","Page",1,min=1,step=1))
               ),
               shiny::div(class="section-row",shiny::textOutput("delay_count"),
                 shiny::div(class="button-row",shiny::actionButton("delay_prev","Previous"),
                   shiny::actionButton("delay_next","Next"))),
               shiny::uiOutput("delay_gallery")
          )
        )
      )
    ),
    shiny::tabPanel(title="Signal investigation",value="investigate",
      section_head("Investigate a signal","Select one signal and see its time series, geography, co-diagnoses, demographics and line list together. Saved workbook figures do not change when visit filters change."),
      card(shiny::fluidRow(
        shiny::column(8,shiny::selectizeInput("focus_signal","Signal workbook",choices=init_focus_choices,
          selected=if(nzchar(init_focus_path)) init_focus_path else NULL,options=list(placeholder="Select a signal"))),
        shiny::column(4,shiny::div(class="button-row",style="margin-top:22px;",
          shiny::actionButton("focus_add_group","Add to comparison group",class="btn-primary")))
      ),shiny::uiOutput("focus_context")),
      shiny::tabsetPanel(id="invest_view",
        shiny::tabPanel("Summary",value="summary",
          shiny::div(class="metric-grid",metric_box("Signal visit rows","focus_rows"),
            metric_box("Distinct visits","focus_visits"),metric_box("Patients","focus_patients"),metric_box("Co-diagnosis codes","focus_codes")),
          shiny::fluidRow(shiny::column(7,card(shiny::h3("Visit activity"),shiny::plotOutput("focus_date_plot",height="285px"))),
            shiny::column(5,card(shiny::h3("Age distribution"),shiny::plotOutput("focus_age_plot",height="285px")))),
          card(shiny::h3("Signal's step-7 report entry"),DT::DTOutput("focus_report_row"),
               shiny::fluidRow(shiny::column(6,shiny::h4("Recurrence interval history"),shiny::plotOutput("focus_ri_plot",height="235px")),
                              shiny::column(6,shiny::h4("Odds ratio history"),shiny::plotOutput("focus_or_plot",height="235px"))))
        ),
        shiny::tabPanel("Trends",value="trends",
          card(shiny::h3("Original temporal figures · step 6 / Trends"),shiny::p(class="card-sub","Every embedded trend figure in this signal workbook."),
               shiny::uiOutput("fig_trends"))
        ),
        shiny::tabPanel("Maps",value="maps",
          card(shiny::h3("Spatial patterns · step 6 / Maps"),shiny::p(class="card-sub","Cluster and baseline maps for hospital ZIP and patient ZIP. Original spatial distributions, not a recalculated change map."),
               shiny::uiOutput("fig_maps"))
        ),
        shiny::tabPanel("Hospitals",value="hospitals",
          card(shiny::h3("Hospital mix change · step 6 / Hospitals"),shiny::p(class="card-sub","Original hospital-distribution percentage-change figure."),
               shiny::uiOutput("fig_hospitals")),
          card(shiny::h3("Hospitals in this signal's visit rows"),shiny::plotOutput("focus_hospital_plot",height="330px"))
        ),
        shiny::tabPanel("Co-diagnoses",value="diagnoses",
          card(shiny::h3("Original cluster versus baseline · step 6 / Other_Codes"),DT::DTOutput("focus_other_codes")),
          card(shiny::h3("Codes recorded on the signal's visits"),DT::DTOutput("focus_code_summary"))
        ),
        shiny::tabPanel("Demographics & CC",value="demographics",
          card(shiny::h3("Original demographics · step 6 / Demographics"),DT::DTOutput("focus_demo")),
          card(shiny::h3("Chief-complaint words · step 6 / TopCC_words"),DT::DTOutput("focus_cc"))
        ),
        shiny::tabPanel("Visits",value="visits",
          card(shiny::h3("Underlying visits for this signal"),shiny::p(class="card-sub","Use table search and column filters; all saved line-list columns are included."),
               DT::DTOutput("focus_visit_table"),shiny::downloadButton("dl_focus_visits","Download signal visits"))
        ),
        shiny::tabPanel("Delay curve",value="delay",
          card(shiny::h3("Corresponding step-7 upload-delay curves"),shiny::p(class="card-sub","Shows plots whose saved node labels match the selected signal. Open the Daily report gallery to browse every curve."),
               shiny::uiOutput("focus_delay_gallery"),
               shiny::actionButton("open_all_delays","Browse all delay curves"))
        )
      )
    ),
    shiny::tabPanel(title="Compare signals",value="compare",
      section_head("Compare & group signals","Select any combination of signals, including multiple diagnoses and nodes. Explore their union, their shared visits, their shared co-diagnoses and repeat patients."),
      card(shiny::fluidRow(
        shiny::column(9,shiny::selectizeInput("group_signals","Signals in comparison group",choices=choices_for(init_records),
          selected=init_group,multiple=TRUE,options=list(placeholder="Select two or more signals",maxOptions=5000))),
        shiny::column(3,shiny::div(class="button-row",style="margin-top:22px;",
          shiny::actionButton("apply_group","Update comparison",class="btn-primary"))),
        shiny::column(12,shiny::checkboxInput("include_earlier","Include latest saved earlier workbooks when selecting signals",FALSE))
      ),shiny::div(class="button-row",shiny::actionButton("group_all","Select all available"),
         shiny::actionButton("group_clear","Clear group"),shiny::downloadButton("dl_group","Download group selection")),
         shiny::textOutput("group_status")),
      make_filter_controls("cmp"),
      card(shiny::fluidRow(shiny::column(4,shiny::selectInput("joint_mode","Visit membership",choices=c(
        "All visits in selected signals"="any","Visits shared by 2+ signals"="shared",
        "Visits in every selected signal"="all","Visits unique to one signal"="unique"))),
        shiny::column(6,shiny::selectizeInput("cmp_diagnoses","Co-diagnosis codes (ICD-10, 3 characters)",choices=character(),multiple=TRUE)),
        shiny::column(2,shiny::checkboxInput("cmp_require_all","Require all codes",FALSE))
      )),
      shiny::div(class="metric-grid",metric_box("Signals in group","cmp_nsignals"),
        metric_box("Distinct visits after filters","cmp_nvisits"),metric_box("Patients","cmp_npatients"),
        metric_box("Shared visits (2+ signals)","cmp_nshared")),
      shiny::tabsetPanel(id="compare_view",
        shiny::tabPanel("Overview",value="overview",
          shiny::fluidRow(shiny::column(7,card(shiny::h3("Visits across time by signal"),shiny::plotOutput("cmp_trends",height="340px"))),
            shiny::column(5,card(shiny::h3("Number of signals per visit"),shiny::plotOutput("cmp_membership_plot",height="340px")))),
          card(shiny::h3("Pairwise shared visits"),DT::DTOutput("cmp_pairs"))
        ),
        shiny::tabPanel("Co-diagnosis overlap",value="codes",
          card(shiny::h3("Codes seen across the selected signals"),shiny::p(class="card-sub","Select diagnosis codes above to filter matching visits. Codes are extracted from the original diagnosis strings."),
               DT::DTOutput("cmp_code_table"),shiny::downloadButton("dl_codes","Download code overlap summary"),
               shiny::plotOutput("cmp_codes_plot",height="330px")),
          card(shiny::h3("Visits matching the diagnosis selection"),DT::DTOutput("cmp_code_visits"))
        ),
        shiny::tabPanel("Visit overlap",value="overlap",
          card(shiny::h3("Shared visit membership"),shiny::p(class="card-sub","Estimated linkage uses patient key + date + time + hospital, not a verified encounter identifier."),
               DT::DTOutput("cmp_membership_table")),
          card(shiny::h3("Pairwise overlap"),DT::DTOutput("cmp_pairs_detail"))
        ),
        shiny::tabPanel("Repeat patients",value="repeats",
          card(shiny::h3("Patients with multiple distinct signal-related visits"),DT::DTOutput("cmp_repeat_table"))
        ),
        shiny::tabPanel("Combined line list",value="lines",
          card(shiny::h3("Filtered combined visit line list"),DT::DTOutput("cmp_visit_table"),
               shiny::downloadButton("dl_cmp_visits","Download filtered group visits"))
        )
      )
    ),
    shiny::tabPanel(title="Visit explorer",value="explore",
      section_head("Visit explorer","Examine the complete step-9 validated line lists, or restrict the view to your currently applied comparison group."),
      card(shiny::checkboxInput("explore_group_only","Use only comparison-group signals",FALSE),
        shiny::p(class="card-sub","Includes all original columns. Filters below affect this explorer only, not saved report figures.")),
      make_filter_controls("exp"),
      shiny::div(class="metric-grid",metric_box("Matching line-list rows","exp_rows"),
        metric_box("Distinct visits","exp_visits"),metric_box("Patients","exp_patients"),
        metric_box("Signals represented","exp_signals")),
      card(shiny::h3("Visit line list"),DT::DTOutput("explorer_table"),
           shiny::downloadButton("dl_explorer","Download filtered visits"))
    ),
    shiny::tabPanel(title="Data & help",value="help",
      section_head("Data sources & diagnostics","Verify exactly what was loaded, see empty line lists, and find every saved Excel worksheet."),
      card(shiny::h3("Import status for current date and collection"),shiny::textOutput("cache_status"),DT::DTOutput("import_table"),
           shiny::actionButton("refresh_data","Rescan saved files & refresh cache")),
      card(shiny::h3("Workbook guide"),shiny::tableOutput("guide")),
      card(shiny::h3("Original image archive"),
        shiny::p(class="card-sub","Fallback viewer for any embedded image in any step-6 sheet. This preserves the original workbook-image browser."),
        shiny::fluidRow(shiny::column(6,shiny::selectInput("archive_file","Signal workbook",
          choices=choices_for(init_records),selected=if(length(init_paths))init_paths[1] else NULL)),
          shiny::column(4,shiny::selectInput("archive_sheet","Worksheet",
            choices=c("ClusterLinelist","Other_Codes","TopCC_words","Trends","Maps","Hospitals","Demographics"),selected="Trends")),
          shiny::column(2,shiny::div(class="button-row",style="margin-top:22px;",
            shiny::actionButton("archive_load","Show images")))),
        shiny::uiOutput("archive_gallery")
      ),
      card(shiny::h3("Reading the dashboard"),
        shiny::tags$p("The Daily report and original step-6 and step-7 figures come from saved Excel outputs. They do not change with visit filters."),
        shiny::tags$p("The comparison and visit-explorer charts are recalculated from validated ClusterLinelist rows. An empty line list can still have valuable saved plots and report entries."),
        shiny::tags$p("Overlap means an estimated shared visit, using the patient key, date, time and hospital. It should not be treated as a verified unique encounter without reviewing your source identifiers."),
        shiny::tags$p("Files stay on the local computer. CSV exports may include patient-level information; handle them using your usual secure procedures."))
    )
  )
)

server <- function(input,output,session) {
  active_scope <- shiny::reactiveVal(preferred_scope)
  active_date <- shiny::reactiveVal(initial_date)
  catalog <- shiny::reactiveVal(initial_catalog)
  bundle <- shiny::reactiveVal(init_bundle)
  load_error <- shiny::reactiveVal("")
  group_paths <- shiny::reactiveVal(init_group)
  group_rows <- shiny::reactiveVal({
    d <- init_bundle$lines
    if (!nrow(d) || !length(init_group)) d[FALSE,,drop=FALSE]
    else d[basename(d$.source_file) %in% basename(init_group),,drop=FALSE]
  })
  group_warning <- shiny::reactiveVal("")
  figure_cache <- new.env(parent=emptyenv())
  temp_folders <- character()
  session$onSessionEnded(function() {
    for (f in unique(temp_folders)) if (dir.exists(f)) unlink(f,recursive=TRUE)
  })
  images_trends <- shiny::reactiveVal(data.frame())
  images_maps <- shiny::reactiveVal(data.frame())
  images_hospitals <- shiny::reactiveVal(data.frame())
  image_index <- shiny::reactiveVal(data.frame())
  image_index_path <- shiny::reactiveVal("")
  delay_visible <- shiny::reactiveVal(data.frame())
  focus_delay_visible <- shiny::reactiveVal(data.frame())
  focus_original <- shiny::reactiveVal(data.frame())
  focus_demographics <- shiny::reactiveVal(data.frame())
  focus_ccwords <- shiny::reactiveVal(data.frame())
  archive_images <- shiny::reactiveVal(data.frame())
  selected_node <- shiny::reactiveVal("")
  # Server-authoritative focus: a report click sets this BEFORE changing tabs.
  # The selectize update is asynchronous and must not control data loading.
  focus_selected_path <- shiny::reactiveVal(init_focus_path)
  focus_report_row <- shiny::reactiveVal(NULL)
  focus_load_error <- shiny::reactiveVal("")

  records <- shiny::reactive(ts_lines_for_date(catalog(),active_date(),FALSE))
  focus_records <- shiny::reactive({
    d <- catalog()$lines
    day <- suppressWarnings(as.Date(active_date(),format="%Y-%m-%d"))
    if (length(day)!=1L || is.na(day)) return(d[FALSE,,drop=FALSE])
    d[!is.na(d$date) & d$date<=day,,drop=FALSE]
  })
  eligible <- shiny::reactive(ts_lines_for_date(catalog(),active_date(),isTRUE(input$include_earlier)))
  report <- shiny::reactive({
    p <- bundle(); if (is.data.frame(p$report)) p$report else data.frame()
  })
  report_path <- shiny::reactive({
    rs <- catalog()$reports
    f <- rs$path[as.character(rs$date)==active_date()]
    if (length(f) && file.exists(f[1])) f[1] else ""
  })
  full_lines <- shiny::reactive({
    p <- bundle(); if (is.data.frame(p$lines)) p$lines else ts_empty_visit_data()
  })
  # Switching analysis date / scope always replaces the complete prepared state.
  # This deliberately uses the verified v12.2 loader and cache fingerprinting.
  apply_group_paths <- function(paths,quiet=FALSE) {
    available <- ts_lines_for_date(catalog(),active_date(),TRUE)
    # Include selected exact historical file paths, not just latest per code.
    historic <- focus_records()
    available <- unique(rbind(available,historic[historic$path %in% paths,,drop=FALSE]))
    sel <- available[available$path %in% paths,,drop=FALSE]
    p <- bundle()
    d <- if (is.data.frame(p$lines)) p$lines else ts_empty_visit_data()
    from_cache <- if (nrow(sel)) sel$path[sel$path %in% as.character(p$records$path)] else character()
    primary <- d[basename(d$.source_file) %in% basename(from_cache),,drop=FALSE]
    outside <- sel[!sel$path %in% from_cache,,drop=FALSE]
    extra <- ts_empty_visit_data(); warn <- character()
    if (nrow(outside)) {
      read <- tryCatch(ts_read_lines(outside),error=function(e) {
        warn <<- c(warn,paste("Earlier workbook import:",conditionMessage(e)))
        NULL
      })
      if (is.list(read) && is.data.frame(read$data)) {
        extra <- read$data
        warn <- c(warn,read$warnings)
      }
    }
    all <- if (nrow(primary) && nrow(extra)) ts_bind(list(primary,extra)) else
      if (nrow(primary)) primary else if (nrow(extra)) extra else ts_empty_visit_data()
    group_paths(sel$path)
    group_rows(all)
    group_warning(paste(warn,collapse="\n"))
    if (!quiet) shiny::showNotification(sprintf("Comparison updated: %d signals, %s line-list rows.",nrow(sel),fmt_int(nrow(all))),type="message")
    invisible(NULL)
  }
  change_run <- function(scope,day,new_catalog=catalog(),force=FALSE) {
    p <- if (!identical(day,"none")) ts_ensure_prepared(project,scope,day,force=force,verbose=TRUE) else NULL
    good <- ts_cache_valid(p,scope,day)
    bundle(if (good) p else empty_bundle())
    load_error(if (!good) sprintf("No prepared visit/report data for %s / %s. Check Import status; saved figures may still be available.",scope,day) else "")
    active_scope(scope); active_date(day); catalog(new_catalog)
    chosen <- ts_lines_for_date(new_catalog,day,FALSE)
    new_focus_choices <- focus_choices_for(new_catalog,day)
    first <- if(nrow(chosen)) as.character(chosen$path[1]) else
      if(length(new_focus_choices)) as.character(unname(new_focus_choices[1])) else ""
    focus_selected_path(first)
    focus_report_row(NULL)
    focus_load_error("")
    shiny::freezeReactiveValue(input,"focus_signal")
    shiny::updateSelectizeInput(session,"focus_signal",choices=new_focus_choices,
      selected=if(nzchar(first)) first else character(),server=TRUE)
    g <- head(chosen$path,min(2L,nrow(chosen)))
    group_inventory <- ts_lines_for_date(new_catalog,day,isTRUE(input$include_earlier))
    shiny::updateSelectizeInput(session,"group_signals",choices=choices_for(group_inventory),selected=g,server=TRUE)
    apply_group_paths(g,quiet=TRUE)
    images_trends(data.frame());images_maps(data.frame());images_hospitals(data.frame())
    focus_original(data.frame());focus_demographics(data.frame());focus_ccwords(data.frame());archive_images(data.frame())
    shiny::updateSelectInput(session,"archive_file",choices=choices_for(chosen),selected=first)
    image_index(data.frame());image_index_path("");delay_visible(data.frame());focus_delay_visible(data.frame())
    selected_node("")
    shiny::updateNumericInput(session,"delay_page",value=1,min=1)
  }
  shiny::observeEvent(input$scope, {
    new_scope <- input$scope
    if (!not_empty(new_scope) || identical(new_scope,active_scope())) return()
    cnew <- ts_catalog(project,new_scope)
    days <- ts_dates(cnew)
    day <- if (active_date() %in% days) active_date() else ts_date_default(cnew)
    change_run(new_scope,day,cnew)
    shiny::freezeReactiveValue(input,"analysis_date")
    shiny::updateSelectInput(session,"analysis_date",choices=if(length(days))days else c("No saved dates"="none"),selected=day)
  },ignoreNULL=TRUE)
  shiny::observeEvent(input$analysis_date, {
    day <- input$analysis_date
    if (!not_empty(day) || !identical(input$scope,active_scope()) || identical(day,active_date())) return()
    if (!day %in% ts_dates(catalog())) return()  # ignore stale browser values during scope change
    change_run(active_scope(),day)
  },ignoreNULL=TRUE)
  shiny::observeEvent(input$refresh_data, {
    sc <- active_scope(); cnew <- ts_catalog(project,sc)
    days <- ts_dates(cnew)
    day <- if (active_date() %in% days) active_date() else ts_date_default(cnew)
    change_run(sc,day,cnew,force=TRUE)
    shiny::updateSelectInput(session,"analysis_date",choices=if(length(days))days else c("No saved dates"="none"),selected=day)
    shiny::showNotification("Saved-file inventory and prepared cache refreshed.",type="message")
  },ignoreInit=TRUE)
  shiny::observeEvent(input$include_earlier, {
    rec <- eligible()
    keep <- intersect(as.character(input$group_signals),as.character(rec$path))
    shiny::updateSelectizeInput(session,"group_signals",choices=choices_for(rec),selected=keep,server=TRUE)
  },ignoreInit=TRUE)
  shiny::observeEvent(input$apply_group, apply_group_paths(input$group_signals),ignoreInit=TRUE)
  shiny::observeEvent(input$group_all, {
    shiny::updateSelectizeInput(session,"group_signals",selected=eligible()$path,server=TRUE)
    shiny::showNotification("All available signals selected. Click Update comparison to apply.",type="message")
  },ignoreInit=TRUE)
  shiny::observeEvent(input$group_clear, {
    shiny::updateSelectizeInput(session,"group_signals",selected=character(),server=TRUE)
    apply_group_paths(character(),quiet=TRUE)
  },ignoreInit=TRUE)
  shiny::observeEvent(input$focus_add_group, {
    f <- input$focus_signal
    if (!not_empty(f)) return()
    current <- unique(c(group_paths(),f))
    if (!(f %in% eligible()$path)) shiny::updateCheckboxInput(session,"include_earlier",value=TRUE)
    shiny::updateSelectizeInput(session,"group_signals",selected=current,server=TRUE)
    apply_group_paths(current,quiet=TRUE)
    shiny::updateTabsetPanel(session,"workspace",selected="compare")
    shiny::showNotification("Signal added to the comparison group.",type="message")
  },ignoreInit=TRUE)

  output$system_message <- shiny::renderUI({
    p <- bundle(); msgs <- character()
    if (nzchar(load_error())) msgs <- c(msgs,load_error())
    if (is.list(p) && length(p$warnings)) msgs <- c(msgs,p$warnings)
    if (is.list(p) && not_empty(p$report_error)) msgs <- c(msgs,paste("Step-7 report:",p$report_error))
    if (length(msgs)) shiny::div(class="alert-soft alert-warn",paste(unique(msgs),collapse="\n"))
    else shiny::div(class="alert-soft",shiny::span(class="status-pill","Saved outputs ready"),
      sprintf("  %s  ·  %s  ·  %s report rows  ·  %s signal workbooks",
        if(identical(active_scope(),"subregion"))"Subregion" else "Full jurisdiction",
        active_date(),fmt_int(nrow(report())),fmt_int(nrow(records()))))
  })
  output$kpi_signals <- shiny::renderText(fmt_int(nrow(report())))
  output$kpi_workbooks <- shiny::renderText(fmt_int(nrow(records())))
  output$kpi_rows <- shiny::renderText(fmt_int(nrow(full_lines())))
  output$kpi_scores <- shiny::renderText({
    d <- report()
    if (!"artifact_score" %in% names(d)) return("0")
    fmt_int(sum(is.finite(suppressWarnings(as.numeric(as.character(d$artifact_score))))))
  })
  # Keep all source columns. Filter ONLY visible rows, never the saved report.
  # The selected row index must refer to exactly the same filtered data frame
  # shown in the DT widget, otherwise a click can open a different signal.
  report_queue_data <- shiny::reactive(report_table_data(report(),input$report_trend))
  output$report_trend_breakdown <- shiny::renderUI({
    d <- report()
    if (!"Trend" %in% names(d)) return(NULL)
    counts <- vapply(trend_levels,function(z) sum(as.character(d$Trend)==z,na.rm=TRUE),integer(1))
    shiny::div(class="trend-chips",lapply(seq_along(trend_levels),function(i)
      shiny::tags$span(class="trend-chip",style=paste0("background:",trend_colors[i],";"),
        paste0(trend_levels[i]," · ",counts[i]))))
  })
  output$report_trend_warning <- shiny::renderUI({
    d <- report()
    if (nrow(d) && !"Trend" %in% names(d))
      shiny::div(class="alert-soft alert-warn",
        "The saved step-7 Signals worksheet has no Trend column. Its other original columns are shown without modification.")
  })
  # A stable hidden row ID is necessary: with DataTables sorting, searching,
  # column filters and pagination, display-row positions are not report indices.
  # The ID is sent by the clicked DOM row, not inferred from rows_selected.
  output$report_queue <- DT::renderDT({
    d <- report()
    d$.ts_report_row_id <- seq_len(nrow(d))
    d <- report_table_data(d,input$report_trend)
    has_trend <- "Trend" %in% names(d)
    if (!nrow(d)) d <- data.frame(Notice=if (nrow(report()))
      "No signals match this trend classification" else "No step-7 Signals rows for this analysis date")
    clickable <- ".ts_report_row_id" %in% names(d)
    hidden_idx <- if (clickable) as.integer(match(".ts_report_row_id",names(d))-1L) else -1L
    cb <- if (clickable) DT::JS(sprintf(
      "$(table.table().container()).on('click','tbody tr',function(){var row=table.row(this).data();if(!row){var pos=$(this).index();var inds=table.rows({page:'current',order:'applied',search:'applied'}).indexes().toArray();if(pos>=0&&pos<inds.length)row=table.row(inds[pos]).data();}if(!row)return;var id=Number(row[%d]);if(Number.isInteger(id)&&id>0&&window.Shiny&&Shiny.setInputValue)Shiny.setInputValue('report_row_clicked',{index:id,nonce:Date.now()},{priority:'event'});});",
       hidden_idx)) else DT::JS("")
    opt <- list(pageLength=16,scrollX=TRUE,autoWidth=TRUE,
      searchHighlight=TRUE,order=list(),dom="ftip")
    defs <- list()
    if (has_trend) {
      opt$fixedColumns <- list(leftColumns=min(2L,ncol(d)-as.integer(clickable)))
    }
    if (clickable) defs <- list(list(targets=hidden_idx,visible=FALSE,searchable=FALSE))
    if (length(defs)) opt$columnDefs <- defs
    tab <- DT::datatable(d,rownames=FALSE,selection="none",filter="top",callback=cb,
      extensions=if(has_trend)"FixedColumns" else character(),options=opt)
    if (has_trend && nrow(d)) tab <- DT::formatStyle(tab,columns="Trend",
      backgroundColor=DT::styleEqual(trend_levels,trend_colors),
      color="#26364a",fontWeight="bold")
    tab
  },server=FALSE)
  # Resolve a report row to its saved interpretation workbook. A match may be
  # from an EARLIER date: step 7 deliberately reports Most recent linelist.
  investigate_report_row <- function(row_id) {
    d <- report()
    row_id <- suppressWarnings(as.integer(row_id))
    if (length(row_id)!=1L || is.na(row_id) || row_id<1L || row_id>nrow(d) ||
        !"Node.Identifier" %in% names(d)) return(invisible(NULL))
    id <- as.character(d$Node.Identifier[row_id])
    recent <- report_value(d,"Most recent linelist",row_id)
    name <- as.character(report_value(d,"Node.Name",row_id,""))
    link <- report_link(id,focus_records(),active_date(),recent,name)
    if (!nzchar(link$path)) {
      shiny::showNotification(paste("Cannot open signal",id,":",link$status,
        ". The original step-7 report row remains available."),type="warning",duration=12)
      return(invisible(NULL))
    }
    selected_node(id)
    focus_report_row(list(path=link$path,index=row_id,date=active_date(),scope=active_scope()))
    focus_selected_path(link$path)  # synchronous, before navigation
    focus_load_error("")
    shiny::freezeReactiveValue(input,"focus_signal")
    shiny::updateSelectizeInput(session,"focus_signal",selected=link$path,server=TRUE)
    shiny::updateTabsetPanel(session,"workspace",selected="investigate")
    shiny::updateTabsetPanel(session,"invest_view",selected="summary")
    invisible(link)
  }
  shiny::observeEvent(input$report_row_clicked, {
    event <- input$report_row_clicked
    if (is.list(event)) investigate_report_row(event$index)
  },ignoreInit=TRUE)
  output$dl_report <- shiny::downloadHandler(
    filename=function() paste0("TreeScan_report_",active_scope(),"_",active_date(),".csv"),
    content=function(path) utils::write.csv(report(),path,row.names=FALSE,na=""))
  output$artifact_chart <- shiny::renderPlot({
    d <- report()
    shiny::validate(shiny::need(nrow(d)>0 && "artifact_score" %in% names(d) &&
       "Node.Identifier" %in% names(d),"No numeric artifact scores found in this report"))
    sc <- suppressWarnings(as.numeric(as.character(d$artifact_score)))
    keep <- which(is.finite(sc));shiny::validate(shiny::need(length(keep)>0,"No numeric artifact scores"))
    keep <- head(keep[order(abs(sc[keep]),decreasing=TRUE)],40L)
    x <- rev(sc[keep]);labs <- rev(as.character(d$Node.Identifier[keep]))
    par(mar=c(4,14,2,2),bg="white")
    barplot(x,names.arg=labs,horiz=TRUE,las=1,col=ifelse(x>=0,"#24829f","#d18d55"),
       border=NA,xlab="Artifact score",cex.names=.78,cex.axis=.83)
    abline(v=0,col="#596c7d")
  })

  # Manual selection is separate from report-click navigation. Only an actual
  # valid workbook in this scope/date is allowed to become the focus.
  shiny::observeEvent(input$focus_signal, {
    f <- input$focus_signal
    if (!not_empty(f) || !(f %in% focus_records()$path) ||
        identical(f,focus_selected_path())) return()
    focus_selected_path(f)
    focus_report_row(NULL)
    selected_node("")
  },ignoreInit=TRUE)
  focus_path <- shiny::reactive({
    f <- focus_selected_path()
    if (!not_empty(f) || !(f %in% focus_records()$path) || !file.exists(f)) return("")
    as.character(f)
  })
  focus_rec <- shiny::reactive({
    z <- focus_records(); f <- focus_path()
    if (!nzchar(f)) return(z[FALSE,,drop=FALSE])
    z[z$path==f,,drop=FALSE]
  })
  focus_lines <- shiny::reactive({
    d <- full_lines();f <- focus_path(); z <- focus_rec()
    if (!nzchar(f) || !nrow(z)) return(d[FALSE,,drop=FALSE])
    # The prepared cache only contains current-date workbooks. Do not silently
    # return zero rows when the clicked report refers to an earlier workbook.
    current <- bundle()$records
    if (is.data.frame(current) && f %in% as.character(current$path)) {
      return(d[!is.na(d$.source_path) & d$.source_path==
        normalizePath(f,winslash="/",mustWork=FALSE),,drop=FALSE])
    }
    imported <- tryCatch(ts_read_lines(z),error=function(e) {
      focus_load_error(conditionMessage(e));NULL
    })
    if (!is.list(imported) || !is.data.frame(imported$data))
      return(ts_empty_visit_data())
    if (length(imported$warnings)) focus_load_error(paste(imported$warnings,collapse="; "))
    imported$data
  })
  focus_report <- shiny::reactive({
    d <- report(); rec <- focus_rec(); f <- focus_path()
    if (!nrow(d) || !nrow(rec) || !"Node.Identifier" %in% names(d))
      return(d[FALSE,,drop=FALSE])
    clicked <- focus_report_row()
    if (is.list(clicked) && identical(clicked$path,f) &&
        identical(clicked$date,active_date()) && identical(clicked$scope,active_scope()) &&
        clicked$index>=1L && clicked$index<=nrow(d)) {
      return(d[clicked$index,,drop=FALSE])
    }
    links <- vapply(seq_len(nrow(d)),function(i) {
      r <- d[i,,drop=FALSE]
      recent <- report_value(r,"Most recent linelist",1L)
      name <- as.character(report_value(r,"Node.Name",1L,""))
      report_link(as.character(r$Node.Identifier[1]),focus_records(),
                  active_date(),recent,name)$path
    },character(1))
    hits <- which(links==f & nzchar(f))
    if (!length(hits)) return(d[FALSE,,drop=FALSE])
    d[hits[1],,drop=FALSE]
  })
  output$focus_context <- shiny::renderUI({
    z <- focus_rec(); d <- focus_lines(); r <- focus_report()
    if (!nrow(z)) return(shiny::div(class="alert-soft alert-warn",
      "No workbook is selected. Choose a saved signal workbook or click a report row."))
    is_historical <- as.character(z$date[1])!=active_date()
    status <- bundle()$import_status
    import_note <- ""
    if (!is_historical && is.data.frame(status) && nrow(status)) {
      row <- status[status$workbook==basename(z$path[1]),,drop=FALSE]
      if (nrow(row) && row$status[1] %in% c("error","empty"))
        import_note <- paste0(" · Import status: ",row$status[1],
          if(nzchar(row$note[1])) paste0(" (",row$note[1],")") else "")
    }
    shiny::div(class=if (!nrow(d)) "alert-soft alert-warn" else "hint",
      shiny::strong(z$label[1])," · Saved workbook: ",as.character(z$date[1]),
      if(is_historical) " (latest saved earlier interpretation; NOT current-day visits)" else "",
      " · ",fmt_int(nrow(d))," visit rows · ",
      if(nrow(r)) "Matched step-7 report row" else "No corresponding step-7 report entry",
      import_note,
      if(nzchar(focus_load_error())) paste0(" · Load error: ",focus_load_error()) else "",
      if(!nrow(d)) " · Zero visit rows does NOT mean report statistics or saved figures are missing" else "")
  })
  output$focus_rows <- shiny::renderText(fmt_int(nrow(focus_lines())))
  output$focus_visits <- shiny::renderText(fmt_int(length(unique(focus_lines()$.visit_id))))
  output$focus_patients <- shiny::renderText(fmt_int(length(unique(stats::na.omit(focus_lines()$.person_key)))))
  output$focus_codes <- shiny::renderText(fmt_int(length(unique(ts_dx_long(focus_lines())$code))))
  empty_plot <- function(msg) {plot.new();text(.5,.5,msg,col="#758597",cex=.9)}
  output$focus_date_plot <- shiny::renderPlot({
    d <- focus_lines()
    if(!nrow(d)) {empty_plot("No visits for this signal");return()}
    d <- d[!duplicated(d$.visit_id) & !is.na(d$date),,drop=FALSE]
    if (!nrow(d)) {empty_plot("No usable visit dates");return()}
    t <- table(as.character(d$date));par(mar=c(4,4,2,1),bg="white")
    plot(as.Date(names(t)),as.integer(t),type="o",pch=19,col="#2877b8",lwd=2,
         xlab="Visit date",ylab="Distinct visits",cex.axis=.86)
    grid(col="#ebeff4");lines(as.Date(names(t)),as.integer(t),col="#2877b8",lwd=2)
  })
  output$focus_age_plot <- shiny::renderPlot({
    d <- focus_lines()
    if(!nrow(d)) {empty_plot("No age-group data");return()}
    d <- d[!duplicated(d$.visit_id),,drop=FALSE]
    t <- sort(table(ts_clean(ts_col(d,"age_group"))),decreasing=TRUE)
    if(!length(t)) {empty_plot("No age-group values");return()}
    par(mar=c(4,10,2,1),bg="white")
    barplot(rev(head(t,10L)),horiz=TRUE,las=1,col="#168a91",border=NA,xlab="Visits",cex.names=.8)
  })
  output$focus_hospital_plot <- shiny::renderPlot({
    d <- focus_lines()
    if(!nrow(d)) {empty_plot("No visits for this signal");return()}
    d <- d[!duplicated(d$.visit_id),,drop=FALSE]
    t <- sort(table(ts_clean(ts_col(d,"hospital"))),decreasing=TRUE)
    if(!length(t)) {empty_plot("No hospital values");return()}
    par(mar=c(4,12,2,1),bg="white")
    barplot(rev(head(t,16L)),horiz=TRUE,las=1,col="#2877b8",border=NA,xlab="Distinct visits",cex.names=.77)
  })
  safe_dt <- function(d,message="No rows to display",filter="top",page=12L) {
    if (!is.data.frame(d) || !nrow(d)) d <- data.frame(Notice=message)
    DT::datatable(d,rownames=FALSE,filter=filter,
      options=list(pageLength=page,scrollX=TRUE,autoWidth=TRUE,searchHighlight=TRUE))
  }
  shown_lines <- function(d) {
    if(!nrow(d)) return(data.frame(Notice="No matching visit rows"))
    n <- c(".cluster_label",setdiff(names(d),c(".cluster_label",".visit_id",".person_key",".linkable",".source_path")))
    d[,n,drop=FALSE]
  }
  output$focus_report_row <- DT::renderDT(safe_dt(focus_report(),"No matching report row for this signal",filter="none",page=3L),server=TRUE)
  draw_history <- function(prefix,current) {
    d <- focus_report()
    if (!nrow(d)) {empty_plot("No report history");return()}
    cols <- grep(paste0("^",prefix,"_[0-9]{8}$"),names(d),value=TRUE)
    dates <- as.Date(sub(paste0("^",prefix,"_"),"",cols),format="%Y%m%d")
    vals <- suppressWarnings(as.numeric(unlist(d[1,cols,drop=FALSE],use.names=FALSE)))
    if(current %in% names(d)) {
      dates <- c(dates,as.Date(active_date()))
      vals <- c(vals,suppressWarnings(as.numeric(as.character(d[[current]][1]))))
    }
    ok <- !is.na(dates) & is.finite(vals)
    if(!any(ok)) {empty_plot("No saved history for this signal");return()}
    oo <- order(dates[ok]);par(mar=c(4,4,2,1),bg="white")
    plot(dates[ok][oo],vals[ok][oo],type="o",pch=19,lwd=2,col="#168a91",
       xlab="Analysis date",ylab=if(prefix=="RI")"Recurrence interval" else "Odds ratio",cex.axis=.85)
  }
  output$focus_ri_plot <- shiny::renderPlot(draw_history("RI","Recurrence.Interval"))
  output$focus_or_plot <- shiny::renderPlot(draw_history("RR","Odds.Ratio"))
  output$focus_visit_table <- DT::renderDT(safe_dt(shown_lines(focus_lines()),filter="top",page=12L),server=TRUE)
  output$dl_focus_visits <- shiny::downloadHandler(
    filename=function() paste0("TreeScan_signal_visits_",active_date(),".csv"),
    content=function(f) utils::write.csv(shown_lines(focus_lines()),f,row.names=FALSE,na=""))
  output$focus_code_summary <- DT::renderDT(safe_dt(ts_dx_summary(ts_dx_long(focus_lines())),"No diagnosis codes available"),server=TRUE)
  # These saved sheets are read only when their respective investigation tab opens.
  shiny::observeEvent(list(input$workspace,input$invest_view,focus_path()), {
    if (!identical(input$workspace,"investigate") || !not_empty(focus_path())) return()
    sheet <- input$invest_view
    if (identical(sheet,"diagnoses")) {
      focus_original(tryCatch(ts_original_codes(focus_path()),error=function(e) {
        shiny::showNotification(paste("Other_Codes:",conditionMessage(e)),type="error");data.frame()
      }))
    }
    if (identical(sheet,"demographics")) {
      focus_demographics(tryCatch(ts_xlsx_sheet(focus_path(),"Demographics"),error=function(e) {
        shiny::showNotification(paste("Demographics:",conditionMessage(e)),type="error");data.frame()
      }))
      focus_ccwords(tryCatch(ts_xlsx_sheet(focus_path(),"TopCC_words"),error=function(e) {
        shiny::showNotification(paste("TopCC_words:",conditionMessage(e)),type="error");data.frame()
      }))
    }
  },ignoreInit=FALSE)
  output$focus_other_codes <- DT::renderDT(safe_dt(focus_original(),"No original Other_Codes rows"),server=TRUE)
  output$focus_demo <- DT::renderDT(safe_dt(focus_demographics(),"No saved Demographics rows"),server=TRUE)
  output$focus_cc <- DT::renderDT(safe_dt(focus_ccwords(),"No saved TopCC_words rows"),server=TRUE)

  # ORIGINAL workbook images: use the existing, tested OOXML relationship
  # extractor, including all figures on a worksheet. Never regenerate plots.
  register_images <- function(z,prefix) {
    if (!is.data.frame(z) || !nrow(z)) return(invisible(NULL))
    for (i in seq_len(nrow(z))) local({
      j <- i
      pic <- z$file[j]
      kind <- switch(tolower(tools::file_ext(pic)),"jpeg"="image/jpeg","jpg"="image/jpeg","gif"="image/gif","image/png")
      output[[paste0(prefix,"_",j)]] <- shiny::renderImage({
        list(src=pic,contentType=kind,alt=basename(pic),width="100%")
      },deleteFile=FALSE)
    })
    invisible(NULL)
  }
  picture_gallery <- function(z,prefix,missing) {
    if (!is.data.frame(z) || !nrow(z)) return(shiny::div(class="hint",missing))
    shiny::div(class="figuregrid",shiny::tagList(lapply(seq_len(nrow(z)),function(i)
      shiny::div(class="figurecard",shiny::h4(as.character(z$title[i])),
                 shiny::imageOutput(paste0(prefix,"_",i),width="100%",height="auto")))))
  }
  # Step-6 figures load when their panel is actually rendered. The old
  # observeEvent(input$invest_view, ...) could leave a visible panel showing a
  # false 'no figures' message if the observer did not run. We deliberately
  # keep this separate from report, line-list, and step-7 plot loading.
  # The proven ts_media_index() is the source of truth for saved drawings.
  read_saved_figures <- function(f,sheet) {
    meta <- file.info(f)
    cache_key <- paste(normalizePath(f,winslash="/",mustWork=TRUE),
      sheet,as.numeric(meta$size[1]),as.numeric(meta$mtime[1]),sep="||")
    if (exists(cache_key,envir=figure_cache,inherits=FALSE))
      return(get(cache_key,envir=figure_cache,inherits=FALSE))

    idx <- ts_media_index(f,sheet)
    result <- list(photos=idx,expected=nrow(idx),problems=character())
    if (!nrow(idx)) {
      idx$file <- character()
      result$photos <- idx
      assign(cache_key,result,envir=figure_cache)
      return(result)
    }

    dest <- tempfile(paste0("ts_saved_",tolower(sheet),"_"))
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    temp_folders <<- c(temp_folders,dest)
    # First try the working v12.2 ZIP extractor. On Windows, unzip() can
    # occasionally report no extracted files even though index() found them.
    extract_error <- NULL
    tryCatch(ts_media_extract(f,sheet,dir=dest),error=function(e) {
      extract_error <<- conditionMessage(e)
      NULL
    })
    idx$file <- file.path(dest,idx$member)
    ok <- file.exists(idx$file)
    ok[which(ok)] <- file.info(idx$file[which(ok)])$size > 0
    ok[is.na(ok)] <- FALSE

    # Read any missing image directly from the zip member as a fallback,
    # without depending on external unzip programs or path separators.
    if (any(!ok)) {
      zip_info <- tryCatch(utils::unzip(f,list=TRUE),error=function(e) NULL)
      for (i in which(!ok)) {
        member <- idx$member[i]
        expected_bytes <- if (is.data.frame(zip_info))
          zip_info$Length[match(member,zip_info$Name)] else NA_real_
        if (!length(expected_bytes) || !is.finite(expected_bytes)) expected_bytes <- 0
        tryCatch({
          dir.create(dirname(idx$file[i]),recursive=TRUE,showWarnings=FALSE)
          con <- unz(f,member,open="rb")
          raw <- tryCatch(readBin(con,what="raw",
                    n=as.integer(min(250000000,max(1048576,expected_bytes+1)))),
                    finally=close(con))
          if (!length(raw)) stop("ZIP member contains no image bytes")
          writeBin(raw,idx$file[i])
        },error=function(e) {
          result$problems <<- c(result$problems,
            paste(member,conditionMessage(e),sep=": "))
        })
      }
    }
    good <- file.exists(idx$file)
    good[which(good)] <- file.info(idx$file[which(good)])$size > 0
    good[is.na(good)] <- FALSE
    result$photos <- idx[good,,drop=FALSE]
    if (sum(good)<nrow(idx)) {
      result$problems <- c(result$problems,sprintf(
        "Excel drawing index found %d figure(s), but %d could not be extracted.",
        nrow(idx),nrow(idx)-sum(good)))
      if (!is.null(extract_error)) result$problems <- c(result$problems,
        paste("Initial ZIP extraction:",extract_error))
    }
    assign(cache_key,result,envir=figure_cache)
    result
  }

  # Render the images directly from the currently selected workbook.
  # Shiny normally suspends hidden outputs, so figures are not unpacked
  # during server startup or when visiting the report/other tabs.
  figure_panel <- function(sheet,prefix) {
    f <- focus_path()
    if (!nzchar(f)) return(shiny::div(class="alert-soft alert-warn",
      "No signal workbook selected. Click a row in the report, or select a workbook."))
    res <- tryCatch(read_saved_figures(f,sheet),error=function(e) {
      list(photos=data.frame(),expected=NA_integer_,
        problems=paste("Cannot examine saved Excel drawings:",conditionMessage(e)))
    })
    z <- res$photos
    count <- if (is.data.frame(z)) nrow(z) else 0L
    shiny::div(
      shiny::p(class="card-sub",sprintf(
        "Source workbook: %s  ·  %s worksheet  ·  %d saved image(s) indexed / %d extracted",
        basename(f),sheet,if(is.na(res$expected)) 0L else res$expected,count)),
      if (length(res$problems)) shiny::div(class="alert-soft alert-warn",
        paste(res$problems,collapse="; ")),
      if (!count) {
        shiny::div(class="alert-soft alert-warn",
          if (identical(res$expected,0L))
            "The selected workbook has no indexed drawings on this worksheet. Check its filename above; the report may link to an earlier workbook."
          else "Images exist in the selected workbook but extraction failed. This is a reader error, not evidence that the Excel plots are absent.")
      } else {
        register_images(z,prefix)
        picture_gallery(z,prefix,"")
      }
    )
  }
  output$fig_trends <- shiny::renderUI(figure_panel("Trends","step6_trends"))
  output$fig_maps <- shiny::renderUI(figure_panel("Maps","step6_maps"))
  output$fig_hospitals <- shiny::renderUI(figure_panel("Hospitals","step6_hospitals"))

  # Step 7 delay gallery. Index all *actual embedded images*, page the extracts.
  ensure_delay_index <- function() {
    f <- report_path()
    if (!nzchar(f)) {image_index_path("");image_index(data.frame());return()}
    if (identical(f,image_index_path())) return()
    z <- tryCatch(ts_media_index(f,"Plots"),error=function(e) {
      shiny::showNotification(paste("Step-7 plot index:",conditionMessage(e)),type="error",duration=NULL)
      data.frame()
    })
    if (is.data.frame(z) && nrow(z)) z <- ts_report_plot_titles(f,z)
    image_index_path(f);image_index(z)
    shiny::updateNumericInput(session,"delay_page",value=1,min=1)
  }
  shiny::observeEvent(list(input$workspace,input$report_view,input$invest_view,report_path()), {
    if ((identical(input$workspace,"report") && identical(input$report_view,"delays")) ||
        (identical(input$workspace,"investigate") && identical(input$invest_view,"delay"))) ensure_delay_index()
  },ignoreInit=FALSE)
  image_matches <- shiny::reactive({
    z <- image_index()
    if (!is.data.frame(z) || !nrow(z)) return(data.frame())
    q <- input$delay_search
    if (not_empty(q)) z <- z[grepl(trimws(q),z$title,fixed=TRUE,ignore.case=TRUE),,drop=FALSE]
    z
  })
  page_size <- shiny::reactive({
    n <- suppressWarnings(as.integer(input$delay_page_size))
    if(length(n)!=1L || is.na(n) || n<1L) 6L else n
  })
  page_count <- shiny::reactive(max(1L,ceiling(nrow(image_matches())/page_size())))
  shiny::observeEvent(list(input$delay_search,input$delay_page_size), {
    shiny::updateNumericInput(session,"delay_page",value=1,min=1,max=page_count())
  },ignoreInit=TRUE)
  shiny::observeEvent(input$delay_prev, {
    n <- suppressWarnings(as.integer(input$delay_page))
    if(length(n)!=1L || is.na(n)) n <- 1L
    shiny::updateNumericInput(session,"delay_page",value=max(1L,n-1L),max=page_count())
  },ignoreInit=TRUE)
  shiny::observeEvent(input$delay_next, {
    n <- suppressWarnings(as.integer(input$delay_page))
    if(length(n)!=1L || is.na(n)) n <- 1L
    shiny::updateNumericInput(session,"delay_page",value=min(page_count(),n+1L),max=page_count())
  },ignoreInit=TRUE)
  page_data <- shiny::reactive({
    d <- image_matches()
    if (!nrow(d)) return(d)
    n <- suppressWarnings(as.integer(input$delay_page))
    if(length(n)!=1L || is.na(n)) n <- 1L
    n <- min(max(1L,n),page_count())
    a <- (n-1L)*page_size()+1L
    d[seq.int(a,min(nrow(d),a+page_size()-1L)),,drop=FALSE]
  })
  shiny::observeEvent(list(input$workspace,input$report_view,page_data(),image_index_path()), {
    if(!identical(input$workspace,"report") || !identical(input$report_view,"delays")) return()
    z <- page_data(); f <- image_index_path()
    if(!nrow(z) || !nzchar(f)) {delay_visible(data.frame());return()}
    dest <- tempfile("ts_curves_page_")
    pics <- tryCatch(ts_media_extract_rows(f,z,dir=dest),error=function(e) {
      shiny::showNotification(paste("Could not display upload-delay curves:",conditionMessage(e)),type="error")
      data.frame()
    })
    temp_folders <<- c(temp_folders,dest)
    delay_visible(pics);register_images(pics,"delay")
  },ignoreInit=FALSE)
  output$delay_count <- shiny::renderText({
    n <- nrow(image_matches())
    pg <- suppressWarnings(as.integer(input$delay_page))
    if(length(pg)!=1L || is.na(pg)) pg <- 1L
    sprintf("%d matching curves (%d total)  ·  page %d of %d",n,nrow(image_index()),min(max(pg,1L),page_count()),page_count())
  })
  output$delay_gallery <- shiny::renderUI(picture_gallery(delay_visible(),"delay",
    "No saved step-7 curves match this search, or the report Plots worksheet contains no images."))
  shiny::observeEvent(list(input$workspace,input$invest_view,focus_path(),image_index()), {
    if(!identical(input$workspace,"investigate") || !identical(input$invest_view,"delay")) return()
    ensure_delay_index()
    d <- image_index()
    r <- focus_report(); z <- focus_rec()
    if (!nrow(d) || !nrow(z)) {focus_delay_visible(data.frame());return()}
    id <- if(nrow(r) && "Node.Identifier" %in% names(r)) as.character(r$Node.Identifier[1]) else z$label[1]
    code <- sub("^[0-9]+-","",id)
    # Match the saved node label rather than wrongly assuming the Nth plot
    # belongs to the Nth report row (some step-7 nodes are skipped).
    hits <- which(grepl(id,d$title,ignore.case=TRUE,fixed=TRUE) |
                  grepl(code,d$title,ignore.case=TRUE,fixed=TRUE))
    if(!length(hits)) {focus_delay_visible(data.frame());return()}
    dest <- tempfile("ts_focus_delays_")
    pics <- tryCatch(ts_media_extract_rows(image_index_path(),d[hits,,drop=FALSE],dir=dest),
      error=function(e){shiny::showNotification(conditionMessage(e),type="error");data.frame()})
    temp_folders <<- c(temp_folders,dest)
    focus_delay_visible(pics);register_images(pics,"focus_delay")
  },ignoreInit=FALSE)
  output$focus_delay_gallery <- shiny::renderUI(picture_gallery(focus_delay_visible(),"focus_delay",
    "No saved upload-delay image could be matched unambiguously to this node. The full step-7 gallery is available under Daily signal report."))
  shiny::observeEvent(input$open_all_delays,{
    shiny::updateTabsetPanel(session,"workspace",selected="report")
    shiny::updateTabsetPanel(session,"report_view",selected="delays")
  },ignoreInit=TRUE)

  # Comparison groups are explicit and can contain any selected signal set.
  # Current-date records are taken from the prepared RDS; only an explicitly
  # selected earlier workbook needs a new Excel read.
  output$group_status <- shiny::renderText({
    paste0(length(group_paths())," signal workbook(s) applied  ·  ",
           fmt_int(nrow(group_rows()))," imported line-list rows",
           if(nzchar(group_warning())) paste0("  ·  Warning: ",group_warning()) else "")
  })
  output$dl_group <- shiny::downloadHandler(
    filename=function() paste0("TreeScan_comparison_group_",active_scope(),"_",active_date(),".csv"),
    content=function(f) {
      r <- eligible();r <- r[r$path %in% group_paths(),c("date","label","signal_key"),drop=FALSE]
      utils::write.csv(r,f,row.names=FALSE,na="")
    })
  # A separate filter set is used for comparison and for the visit explorer;
  # report statistics and saved workbook images never inherit these filters.
  explore_source <- shiny::reactive(if(isTRUE(input$explore_group_only)) group_rows() else full_lines())
  update_filter_choices <- function(prefix,d) {
    for (pair in list(c("age","age_group"),c("hospital","hospital"),c("sex","sex"))) {
      vals <- if(nrow(d)) sort(unique(stats::na.omit(ts_clean(ts_col(d,pair[2]))))) else character()
      shiny::updateSelectizeInput(session,paste0(prefix,"_",pair[1]),choices=vals,selected=character(),server=TRUE)
    }
    cols <- if(nrow(d)) setdiff(names(d),c(".visit_id",".person_key",".source_path",".linkable")) else character()
    shiny::updateSelectInput(session,paste0(prefix,"_col"),choices=c("None"="",stats::setNames(cols,cols)),selected="")
  }
  shiny::observeEvent(group_rows(), {
    update_filter_choices("cmp",group_rows())
    dx <- ts_dx_long(group_rows())
    shiny::updateSelectizeInput(session,"cmp_diagnoses",choices=sort(unique(dx$code)),selected=character(),server=TRUE)
  },ignoreInit=FALSE)
  shiny::observeEvent(explore_source(),update_filter_choices("exp",explore_source()),ignoreInit=FALSE)
  for (prefix in c("cmp","exp")) local({
    px <- prefix
    output[[paste0(px,"_date_ui")]] <- shiny::renderUI({
      d <- if(identical(px,"cmp")) group_rows() else explore_source()
      if (!nrow(d) || !"date" %in% names(d)) return(shiny::div(class="muted","No visit dates in this selection"))
      dates <- d$date[!is.na(d$date)]
      if (!length(dates)) return(shiny::div(class="muted","No readable visit dates"))
      shiny::dateRangeInput(paste0(px,"_dates"),"Visit date",start=min(dates),end=max(dates),
        min=min(dates),max=max(dates))
    })
  })
  visit_filters <- function(d,prefix) {
    if(!is.data.frame(d) || !nrow(d)) return(d)
    dates <- input[[paste0(prefix,"_dates")]]
    if(length(dates)==2L && all(!is.na(dates)) && "date" %in% names(d)) {
      from <- as.Date(dates[1]); to <- as.Date(dates[2])
      actual <- d$date[!is.na(d$date)]
      # An old browser date range can persist for one flush after switching runs.
      if(length(actual) && from<=max(actual) && to>=min(actual))
        d <- d[!is.na(d$date) & d$date>=from & d$date<=to,,drop=FALSE]
    }
    for (pair in list(c("age","age_group"),c("hospital","hospital"),c("sex","sex"))) {
      value <- input[[paste0(prefix,"_",pair[1])]]
      if(length(value)) d <- d[ts_clean(ts_col(d,pair[2])) %in% value,,drop=FALSE]
    }
    for (pair in list(c("dispo","dispo"),c("incident","dxtype"))) {
      value <- input[[paste0(prefix,"_",pair[1])]]
      if(not_empty(value)) d <- d[ts_clean(ts_col(d,pair[2])) %in% value,,drop=FALSE]
    }
    field <- input[[paste0(prefix,"_col")]]
    text <- input[[paste0(prefix,"_contains")]]
    if(not_empty(field) && field %in% names(d) && not_empty(text)) {
      hit <- grepl(text,normal_value(d[[field]]),ignore.case=TRUE,fixed=TRUE)
      d <- d[hit,,drop=FALSE]
    }
    if(isTRUE(input[[paste0(prefix,"_repeat")]]) && nrow(d)) {
      r <- ts_repeats(d)
      d <- if(nrow(r)) d[d$.person_key %in% r$patient_key,,drop=FALSE] else d[FALSE,,drop=FALSE]
    }
    d
  }
  group_basic <- shiny::reactive(visit_filters(group_rows(),"cmp"))
  # Diagnosis selector matches original ICD-10 three-character extracted codes.
  # Require-all is applied at the estimated VISIT level, never across a person's
  # unrelated encounters. The source line list is never modified.
  group_code_filtered <- shiny::reactive({
    d <- group_basic()
    codes <- input$cmp_diagnoses
    if(!nrow(d) || !length(codes)) return(d)
    dx <- ts_dx_long(d)
    dx <- dx[dx$code %in% codes,,drop=FALSE]
    if(!nrow(dx)) return(d[FALSE,,drop=FALSE])
    if(isTRUE(input$cmp_require_all)) {
      sets <- split(dx$code,dx$.visit_id)
      matches <- names(sets)[vapply(sets,function(x)
        all(unique(codes) %in% unique(x)),logical(1))]
    } else matches <- unique(dx$.visit_id)
    d[d$.visit_id %in% matches,,drop=FALSE]
  })
  membership <- function(d) {
    if(!nrow(d)) return(data.frame(.visit_id=character(),n_signals=integer()))
    x <- unique(d[,c(".visit_id",".cluster"),drop=FALSE])
    n <- table(x$.visit_id)
    data.frame(.visit_id=names(n),n_signals=as.integer(n),stringsAsFactors=FALSE)
  }
  membership_all <- shiny::reactive(membership(group_code_filtered()))
  comparison_visits <- shiny::reactive({
    d <- group_code_filtered(); if(!nrow(d)) return(d)
    mem <- membership_all(); mode <- if(not_empty(input$joint_mode)) input$joint_mode else "any"
    n <- length(group_paths())
    ids <- switch(mode,
      "shared"=mem$.visit_id[mem$n_signals>=2L],
      "all"=if(n>=2L) mem$.visit_id[mem$n_signals==n] else character(),
      "unique"=mem$.visit_id[mem$n_signals==1L],
      mem$.visit_id)
    d[d$.visit_id %in% ids,,drop=FALSE]
  })
  output$cmp_nsignals <- shiny::renderText(fmt_int(length(group_paths())))
  output$cmp_nvisits <- shiny::renderText(fmt_int(length(unique(comparison_visits()$.visit_id))))
  output$cmp_npatients <- shiny::renderText(fmt_int(length(unique(stats::na.omit(comparison_visits()$.person_key)))))
  output$cmp_nshared <- shiny::renderText(fmt_int(sum(membership(comparison_visits())$n_signals>=2L)))
  output$cmp_pairs <- DT::renderDT(safe_dt(ts_pairwise(comparison_visits()),"Select two or more signals with visit rows"),server=TRUE)
  output$cmp_pairs_detail <- DT::renderDT(safe_dt(ts_pairwise(comparison_visits()),"No pairwise overlap available"),server=TRUE)
  output$cmp_trends <- shiny::renderPlot({
    d <- comparison_visits()
    if(!nrow(d)) {empty_plot("No visits in the selected group and filters");return()}
    x <- unique(d[!is.na(d$date),c(".cluster",".visit_id","date"),drop=FALSE])
    if(!nrow(x)) {empty_plot("No dated visits");return()}
    counts <- aggregate(rep(1L,nrow(x)),by=list(date=x$date,signal=x$.cluster),FUN=sum)
    names(counts)[3] <- "visits"
    groups <- unique(counts$signal)
    cols <- grDevices::hcl.colors(length(groups),"Dark 3")
    par(mar=c(4,4,2,2),bg="white")
    plot(range(counts$date),c(0,max(1,counts$visits)),type="n",xlab="Visit date",ylab="Distinct visits",cex.axis=.85)
    grid(col="#e8eef3")
    for(j in seq_along(groups)) {
      a <- counts[counts$signal==groups[j],,drop=FALSE]
      a <- a[order(a$date),,drop=FALSE]
      lines(a$date,a$visits,col=cols[j],lwd=2,type="o",pch=16,cex=.7)
    }
    legend("topright",legend=groups,col=cols,lty=1,pch=16,cex=.7,bg="white",bty="n")
  })
  output$cmp_membership_plot <- shiny::renderPlot({
    m <- membership(comparison_visits())
    if(!nrow(m)) {empty_plot("No visits under these filters");return()}
    t <- table(m$n_signals)
    par(mar=c(4,4,2,1),bg="white")
    barplot(t,col="#168a91",border=NA,xlab="Signals containing this visit",ylab="Distinct visits")
  })
  output$cmp_code_table <- DT::renderDT({
    d <- ts_dx_summary(ts_dx_long(group_basic()))
    safe_dt(d,"No extracted diagnosis codes under the non-diagnosis filters")
  },server=TRUE)
  output$dl_codes <- shiny::downloadHandler(
    filename=function() paste0("TreeScan_group_diagnoses_",active_scope(),"_",active_date(),".csv"),
    content=function(f) utils::write.csv(ts_dx_summary(ts_dx_long(group_basic())),f,row.names=FALSE,na=""))
  output$cmp_codes_plot <- shiny::renderPlot({
    d <- ts_dx_summary(ts_dx_long(comparison_visits()))
    if(!nrow(d)) {empty_plot("No co-diagnosis matches");return()}
    d <- head(d[order(-d$clusters,-d$visits),,drop=FALSE],20L)
    par(mar=c(4,9,2,1),bg="white")
    barplot(rev(d$visits),names.arg=rev(d$code),horiz=TRUE,las=1,col="#2877b8",border=NA,
      xlab="Distinct visits",cex.names=.84)
  })
  output$cmp_code_visits <- DT::renderDT(safe_dt(shown_lines(comparison_visits()),"No matching visits"),server=TRUE)
  output$cmp_visit_table <- DT::renderDT(safe_dt(shown_lines(comparison_visits()),"No matching visits"),server=TRUE)
  output$cmp_repeat_table <- DT::renderDT(safe_dt(ts_repeats(comparison_visits()),"No repeat patients in this selection"),server=TRUE)
  output$cmp_membership_table <- DT::renderDT({
    d <- comparison_visits();m <- membership(d)
    if(!nrow(m)) return(safe_dt(m,"No linked visits"))
    labs <- split(as.character(d$.cluster),as.character(d$.visit_id))
    m$signals <- vapply(m$.visit_id,function(id) paste(sort(unique(labs[[id]])),collapse=", "),character(1))
    m <- m[order(-m$n_signals,m$.visit_id),,drop=FALSE]
    safe_dt(m)
  },server=TRUE)
  output$dl_cmp_visits <- shiny::downloadHandler(
    filename=function() paste0("TreeScan_group_visits_",active_scope(),"_",active_date(),".csv"),
    content=function(f) utils::write.csv(shown_lines(comparison_visits()),f,row.names=FALSE,na=""))

  # Full visit explorer; comparison membership is optional and does not change
  # either of the original source Excel workbooks or the report.
  explorer_visits <- shiny::reactive(visit_filters(explore_source(),"exp"))
  output$exp_rows <- shiny::renderText(fmt_int(nrow(explorer_visits())))
  output$exp_visits <- shiny::renderText(fmt_int(length(unique(explorer_visits()$.visit_id))))
  output$exp_patients <- shiny::renderText(fmt_int(length(unique(stats::na.omit(explorer_visits()$.person_key)))))
  output$exp_signals <- shiny::renderText(fmt_int(length(unique(explorer_visits()$.cluster))))
  output$explorer_table <- DT::renderDT(safe_dt(shown_lines(explorer_visits()),"No visit rows match the filters"),server=TRUE)
  output$dl_explorer <- shiny::downloadHandler(
    filename=function() paste0("TreeScan_visit_explorer_",active_scope(),"_",active_date(),".csv"),
    content=function(f) utils::write.csv(shown_lines(explorer_visits()),f,row.names=FALSE,na=""))

  output$cache_status <- shiny::renderText({
    p <- bundle()
    sprintf("%s · %s · %s validated line-list rows · %s report rows · %s signal workbooks",
      active_scope(),active_date(),fmt_int(nrow(full_lines())),fmt_int(nrow(report())),fmt_int(nrow(records())))
  })
  output$import_table <- DT::renderDT({
    d <- bundle()$import_status
    safe_dt(d,"No imported step-6 workbook records for this date")
  },server=TRUE)
  shiny::observeEvent(input$archive_load, {
    f <- input$archive_file
    sh <- input$archive_sheet
    if (!not_empty(f) || !not_empty(sh) || !(f %in% records()$path) || !file.exists(f)) {
      shiny::showNotification("Choose a saved signal workbook and worksheet.",type="warning")
      archive_images(data.frame());return()
    }
    dest <- tempfile("ts_archive_figure_")
    z <- tryCatch(ts_media_extract(f,sh,dir=dest),error=function(e) {
      shiny::showNotification(paste("Archive image reader:",conditionMessage(e)),type="error")
      data.frame()
    })
    temp_folders <<- c(temp_folders,dest)
    archive_images(z);register_images(z,"archive")
  },ignoreInit=TRUE)
  output$archive_gallery <- shiny::renderUI(picture_gallery(archive_images(),"archive",
    "Choose a workbook and click Show images. A worksheet may validly contain no embedded images."))
  output$guide <- shiny::renderTable({
    data.frame(Source=c(rep("Step 7",2),rep("Step 6",7)),
      Worksheet=c("Signals","Plots","ClusterLinelist","Other_Codes","TopCC_words","Trends","Maps","Hospitals","Demographics"),
      Location=c("Daily signal report → Signal queue, Artifact scores, Signal investigation → Summary",
                 "Daily signal report → Upload-delay curves; Signal investigation → Delay curve",
                 "Signal investigation → Summary / Visits; Compare signals; Visit explorer",
                 "Signal investigation → Co-diagnoses (original cluster vs baseline)",
                 "Signal investigation → Demographics & CC",
                 "Signal investigation → Trends (all original figures)",
                 "Signal investigation → Maps (all original figures)",
                 "Signal investigation → Hospitals (original percentage-change figure)",
                 "Signal investigation → Demographics & CC"),stringsAsFactors=FALSE)
  },striped=TRUE,spacing="m")
}

shiny::shinyApp(ui=ui,server=server)
