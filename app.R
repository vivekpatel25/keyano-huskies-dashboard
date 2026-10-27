# ============================================================
# KEYANO HUSKIES COACHES DASHBOARD
# Original design + workbook data connection
# ============================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

parse_match_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(x))
  }

  # Excel dates may be stored as numeric serial numbers.
  if (is.numeric(x)) {
    result <- as.Date(x, origin = "1899-12-30")
    result[x < 1 | x > 100000] <- NA
    return(result)
  }

  x <- trimws(as.character(x))
  result <- rep(as.Date(NA), length(x))

  # Try common date formats explicitly.
  formats <- c(
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%m/%d/%Y",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%d-%b-%Y",
    "%d-%B-%Y",
    "%b %d, %Y",
    "%B %d, %Y",
    "%d %b %Y",
    "%d %B %Y"
  )

  for (fmt in formats) {
    missing <- is.na(result) & !is.na(x) & nzchar(x)

    if (!any(missing)) break

    parsed <- suppressWarnings(
      as.Date(x[missing], format = fmt)
    )

    result[which(missing)] <- parsed
  }

  result
}

match_dir <- file.path("data", "matches")
roster_path <- file.path("data", "metadata", "roster.csv")

# ---------------- ROSTER ----------------

roster <- if (file.exists(roster_path)) {
  read.csv(
    roster_path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  ) |>
    as_tibble() |>
    arrange(suppressWarnings(as.numeric(Jersey)))
} else {
  tibble(
    Jersey = character(),
    FirstName = character(),
    Position = character()
  )
}

match_windows <- c(
  "Most Recent Match",
  "Previous Match",
  "Last 5",
  "Last 10",
  "Full Season"
)

# ---------------- WORKBOOK READER ----------------

read_match <- function(path) {

  tryCatch({

    sheets <- excel_sheets(path)

    required <- c(
      "Match Information",
      "Key Metrics",
      "By Skill",
      "Attack Detail",
      "Rotation Details",
      "Setting Stats",
      "Passing by Contact Point"
    )

    missing <- setdiff(required, sheets)

    if (length(missing)) {
      stop(
        paste(
          "Missing worksheets:",
          paste(missing, collapse = ", ")
        )
      )
    }

    # Match Information
    info <- read_excel(
      path,
      sheet = "Match Information",
      col_names = FALSE
    )

    get_info <- function(label) {

      ix <- which(
        trimws(as.character(info[[1]])) == label
      )

      if (!length(ix)) {
        return(NA_character_)
      }

      val <- info[[2]][ix[1]]

      if (!length(val) || is.na(val)) {
        return(NA_character_)
      }

      as.character(val)
    }

   
date_val <- get_info("Match date")

match_date <- parse_match_date(date_val)

# If the workbook date is missing, try the date in the filename.
if (is.na(match_date)) {
  filename_date <- sub(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2}).*$",
    "\\1",
    basename(path)
  )

  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", filename_date)) {
    match_date <- as.Date(
      filename_date,
      format = "%Y-%m-%d"
    )
  }
}


    opponent <- get_info("Opponent")

    if (is.na(opponent) || !nzchar(opponent)) {
      opponent <- tools::file_path_sans_ext(
        basename(path)
      )
    }

    result <- get_info("Match result")

    # Key Metrics
    km <- read_excel(
      path,
      sheet = "Key Metrics",
      col_names = FALSE
    )

    # Preserve category headings and descriptions from the workbook.
    key_rows <- list()
    current_category <- "Other"
    current_metric <- NA_character_
    current_description <- ""

    for (i in seq_len(nrow(km))) {
      a <- if (ncol(km) >= 1) as.character(km[[1]][i]) else NA_character_
      b <- if (ncol(km) >= 2) as.character(km[[2]][i]) else NA_character_
      c <- if (ncol(km) >= 3) km[[3]][i] else NA
      a <- ifelse(is.na(a), "", trimws(a))
      b <- ifelse(is.na(b), "", trimws(b))

      if (nzchar(a) && !nzchar(b) && (is.na(c) || !nzchar(as.character(c)))) {
        current_category <- a
      } else if (nzchar(b) && !is.na(c) && nzchar(as.character(c)) &&
                 !is.na(suppressWarnings(as.numeric(c)))) {
        key_rows[[length(key_rows) + 1]] <- tibble(
          category = current_category,
          metric = b,
          value = suppressWarnings(as.numeric(c)),
          description = current_description
        )
        current_metric <- b
        current_description <- ""
      } else if (nzchar(b) && (is.na(c) || !nzchar(as.character(c))) &&
                 !is.na(current_metric)) {
        current_description <- b
        if (length(key_rows)) {
          key_rows[[length(key_rows)]]$description <- b
        }
      }
    }

    key <- if (length(key_rows)) bind_rows(key_rows) else tibble(
      category = character(), metric = character(), value = numeric(), description = character()
    )

    # By Skill
    bs <- read_excel(
      path,
      sheet = "By Skill",
      col_names = FALSE
    )

    skill_rows <- list()
    section <- NA_character_

    for (i in seq_len(nrow(bs))) {

      a <- bs[[1]][i]
      b <- bs[[2]][i]
      c <- bs[[3]][i]

      if (
        !is.na(a) &&
        (is.na(b) || identical(as.character(b), "")) &&
        (is.na(c) || identical(as.character(c), ""))
      ) {

        section <- as.character(a)

      } else if (
        !is.na(b) &&
        !is.na(c) &&
        !is.na(section)
      ) {

        skill_rows[[length(skill_rows) + 1]] <- tibble(
          skill = section,
          metric = as.character(b),
          value = suppressWarnings(as.numeric(c))
        )
      }
    }

    skills <- if (length(skill_rows)) {
      bind_rows(skill_rows)
    } else {
      tibble(
        skill = character(),
        metric = character(),
        value = numeric()
      )
    }

    # Attack Detail
    attack <- read_excel(
      path,
      sheet = "Attack Detail"
    )

    if ("Attack Type" %in% names(attack)) {
      attack <- attack |>
        fill(`Attack Type`, .direction = "down")
    }

    for (nm in intersect(
      c("Attempts", "Kills", "Error"),
      names(attack)
    )) {
      attack[[nm]] <- suppressWarnings(
        as.numeric(attack[[nm]])
      )
    }

    # Rotation Details
    rotation <- read_excel(
      path,
      sheet = "Rotation Details",
      skip = 1
    )

    names(rotation) <- trimws(names(rotation))

    if ("Rotation" %in% names(rotation)) {
      rotation <- rotation |>
        filter(
          !is.na(Rotation),
          grepl("^Ro[1-6]$", as.character(Rotation))
        )
    }

    # Setting Stats:
    # Rows 1-8: rotation totals
    # Rows 10-13: first-ball setting
    # Rows 15-18: transition setting

    setting_raw <- read_excel(
      path,
      sheet = "Setting Stats",
      col_names = FALSE
    )

    read_setting_block <- function(
      header_row,
      first_row,
      last_row
    ) {

      header <- as.character(
        unlist(
          setting_raw[header_row, 1:8],
          use.names = FALSE
        )
      )

      dat <- setting_raw[first_row:last_row, 1:8]

      names(dat) <- make.unique(
        ifelse(
          is.na(header) | header == "",
          paste0("Column", seq_along(header)),
          trimws(header)
        )
      )

      dat
    }

    # Setting totals: row 1 is the worksheet header, rows 2:7 are
    # Ro1-Ro6, and row 8 is the total. Keep only the requested count columns.
    rotation_setting <- setting_raw[2:8, 1:3]
    names(rotation_setting) <- c("Rotation", "Sets", "Kills")
    rotation_setting <- as_tibble(rotation_setting) |>
      mutate(
        Rotation = trimws(as.character(Rotation)),
        Sets = suppressWarnings(as.numeric(Sets)),
        Kills = suppressWarnings(as.numeric(Kills))
      )

    fb_sets <- read_setting_block(
      header_row = 10,
      first_row = 11,
      last_row = 13
    )

    trans_sets <- read_setting_block(
      header_row = 15,
      first_row = 16,
      last_row = 18
    )

    # Passing by Contact Point
    passing <- read_excel(
      path,
      sheet = "Passing by Contact Point"
    )

    fill_cols <- intersect(
      c("Player No.", "Player Name", "Position"),
      names(passing)
    )

    if (length(fill_cols)) {
      passing <- passing |>
        fill(
          all_of(fill_cols),
          .direction = "down"
        )
    }

    list(
      path = path,
      file = basename(path),
      date = match_date,
      opponent = opponent,
      result = result,
      key = key,
      skills = skills,
      attack = attack,
      rotation = rotation,
      rotation_setting = rotation_setting,
      fb_sets = fb_sets,
      trans_sets = trans_sets,
      passing = passing,
      error = NULL
    )

  }, error = function(e) {

    message(
      "Workbook error [",
      basename(path),
      "]: ",
      conditionMessage(e)
    )

    list(
      path = path,
      file = basename(path),
      date = as.Date(NA),
      opponent = tools::file_path_sans_ext(
        basename(path)
      ),
      result = NA_character_,
      error = conditionMessage(e)
    )
  })
}

# ---------------- LOAD MATCHES ----------------

load_matches <- function() {

  if (!dir.exists(match_dir)) {
    return(list())
  }

  files <- list.files(
    match_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  files <- files[
    !grepl("^~\\$", basename(files))
  ]

  if (!length(files)) {
    return(list())
  }

  out <- lapply(files, read_match)

  out <- Filter(
    function(x) is.null(x$error),
    out
  )

  if (length(out)) {

    ord <- vapply(
      out,
      function(x) {
        if (is.na(x$date)) {
          -Inf
        } else {
          as.numeric(x$date)
        }
      },
      numeric(1)
    )

    out <- out[
      order(ord, decreasing = TRUE)
    ]
  }

  out
}

# ---------------- HELPERS ----------------

fmt_value <- function(metric, value) {

  # Safely format missing or non-numeric workbook values.
  if (!length(value) || is.na(value)) {
    return("—")
  }

  value <- suppressWarnings(as.numeric(value))

  if (is.na(value)) {
    return("—")
  }

  metric_name <- as.character(metric)[1]

  # Attack efficiency is conventionally shown as a decimal in this workbook
  # (for example, .321), not as 32.1%.
  if (grepl("efficiency|\\beff\\b|attack eff", metric_name, ignore.case = TRUE)) {
    if (abs(value) < 1) {
      return(sub("^0", "", sprintf("%.3f", value)))
    }
    return(sprintf("%.3f", value))
  }

  # These metrics are percentages. Workbook values are normally proportions
  # (0.737 = 73.7%). If a workbook already stores 73.7, keep it as 73.7%.
  is_percentage <- grepl(
    "%|in-system|aced|point scoring|conversion|block impact|first[- ]?ball|opponent attack|disruption|side[- ]?out|kill rate|error rate|positive pass|good pass",
    metric_name,
    ignore.case = TRUE
  )

  if (is_percentage) {
    percentage <- if (abs(value) <= 1) value * 100 else value
    return(paste0(round(percentage, 1), "%"))
  }

  if (abs(value - round(value)) < 1e-8) {
    return(as.character(round(value)))
  }

  as.character(round(value, 2))
}

metric_value <- function(m, metric) {

  x <- m$key |>
    filter(.data$metric == metric)

  if (!nrow(x)) {
    return(NA_real_)
  }

  suppressWarnings(
    as.numeric(x$value[1])
  )
}

metric_card <- function(label, value, note = "") {

  div(
    class = "metric-card",
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    div(class = "metric-note", note)
  )
}

section_title <- function(title, intro) {

  div(
    class = "section-block-title",
    h2(title),
    p(class = "section-intro", intro)
  )
}

# ---------------- USER INTERFACE ----------------

ui <- page_fluid(

  theme = bs_theme(
    version = 5,
    bg = "#EEF2F5",
    fg = "#263746",
    primary = "#0B2D59",
    secondary = "#7D8B99",
    base_font = font_google("Roboto Condensed"),
    heading_font = font_google("Oswald")
  ),

  tags$head(

    tags$title(
      "Huskies Coaches Dashboard | Keyano ACAC 2026–27"
    ),

    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),

    tags$style(HTML("
      :root {
        --orange: #2F75B5;
      }

      .period-button.active {
        background: #2F75B5 !important;
      }

      .athletes-icon-button:hover,
      .athletes-icon-button:focus {
        color: #2F75B5 !important;
      }

      .content-card {
        min-height: 320px;
        width: 100% !important;
        max-width: none !important;
        box-sizing: border-box;
      }

      /* Let Attack Detail use the full content area, not the table's natural width. */
      .analytics-section,
      .chart-card.attack-summary-card {
        width: 100% !important;
        max-width: none !important;
        box-sizing: border-box;
      }

      .chart-card.attack-summary-card {
        display: block !important;
      }

      .attack-summary-card .shiny-table,
      .attack-summary-card table,
      .attack-summary-card .table-responsive {
        width: 100% !important;
        max-width: none !important;
        box-sizing: border-box;
      }

      /* Emphasize attack-type labels and efficiency values in Attack Detail. */
      #attack_table table tbody td:first-child,
      #attack_table table tbody td:nth-child(n+5) {
        font-weight: 700 !important;
      }

      .rotation-heatmap-wrap {
        width: 100%;
        overflow-x: auto;
      }
      table.rotation-heatmap {
        width: 100%;
        min-width: 1000px;
        border-collapse: separate;
        border-spacing: 0;
        font-size: 12px;
        color: #17324d;
      }
      .rotation-heatmap th,
      .rotation-heatmap td {
        padding: 9px 8px;
        text-align: center;
        white-space: nowrap;
        border-bottom: 1px solid #d9e2eb;
        font-weight: 800;
        font-variant-numeric: tabular-nums;
      }
      .rotation-heatmap thead tr:first-child th {
        background: #fff;
        color: #52677b;
        font-size: 11px;
        font-weight: 800;
        letter-spacing: .06em;
        text-transform: uppercase;
        border-bottom: 0;
      }
      .rotation-heatmap thead tr:nth-child(2) th {
        background: #f7f9fb;
        color: #52677b;
        font-weight: 800;
        border-top: 1px solid #d9e2eb;
      }
      .rotation-heatmap tbody td:first-child {
        text-align: left;
        font-weight: 800;
        background: #f7f9fb;
      }
      /* Strong vertical separators between Game Phase, Serve, Reception and Attack. */
      .rotation-heatmap .phase-end {
        border-right: 3px solid #c4d0dc !important;
      }
      .rotation-heatmap tbody tr.average-row td {
        font-weight: 800;
        border-top: 3px solid #17324d;
        border-bottom: 0;
      }
      .rotation-heatmap tbody tr.average-row td:not(:first-child) {
        background-color: #f4f6f8 !important;
      }

      .analytics-section {
        padding: 4px 0 26px;
        margin-bottom: 20px;
        border-bottom: 1px solid #e5ebf0;
      }

      .analytics-section:last-child {
        border-bottom: 0;
      }

      .section-block-title {
        margin: 26px 0 12px;
      }

      .section-block-title h2 {
        margin-bottom: 4px;
      }

      .section-block-title p {
        margin-bottom: 14px;
      }

      .status-error {
        background: #fff1f1;
        color: #a12626;
        padding: 12px 14px;
        border-radius: 10px;
        overflow-wrap: anywhere;
      }

      .status-good {
        background: #edf7f1;
        color: #216c47;
        padding: 10px 14px;
        border-radius: 10px;
      }

      .chart-card {
        border: 1px solid #e4e9ee;
        border-radius: 13px;
        padding: 16px;
        margin: 12px 0;
        background: #fff;
        overflow-x: auto;
      }

      .table-wrap {
        overflow-x: auto;
        width: 100%;
      }
      .table-wrap table {
        width: 100% !important;
        table-layout: fixed;
      }
      .table-wrap table th,
      .table-wrap table td {
        white-space: normal;
      }
      .attack-summary-card table { width:100% !important; table-layout:fixed; }
      .attack-summary-card table th { color:#5b7184; font-size:11px; text-transform:uppercase; border-bottom:1px solid #d6e0e8; }
      .attack-summary-card table td { border-top:1px solid #d6e0e8; padding:8px 10px; }
      .attack-summary-card .form-group { margin:0; }
      .attack-summary-card .control-label { display:none; }

      .key-metrics-panel { background:#fff; border:1px solid #dce5ed; border-radius:14px; padding:22px; }
      .metric-group { margin-top:22px; }
      .metric-group-title { font-size:12px; font-weight:800; letter-spacing:1.4px; border-bottom:3px solid #34424d; padding-bottom:7px; margin-bottom:4px; color:#435363; }
      .metric-row { display:grid; grid-template-columns:minmax(220px, 34%) 82px minmax(180px, 1fr); gap:16px; align-items:center; padding:10px 0; border-bottom:1px solid #d9e1e8; }
      .metric-copy { min-width:0; }
      .metric-name { font-weight:700; color:#142c43; }
      .metric-description { font-size:12px; color:#64788a; margin-top:2px; line-height:1.25; }
      .metric-value { font-size:20px; font-weight:800; text-align:right; color:#159b68; white-space:nowrap; }
      .metric-bar-area { position:relative; padding:10px 0 20px; min-width:0; }
      .metric-track { height:13px; background:#f4eae2; border-radius:99px; overflow:hidden; }
      .metric-fill { height:100%; border-radius:99px; }
      .metric-good { background:#159b68; }
      .metric-bad { background:#df4b3e; }
      .metric-marker-wrap { position:absolute; top:0; transform:translateX(-50%); height:30px; white-space:nowrap; }
      .metric-marker { height:22px; width:3px; background:#df4b3e; margin:auto; }
      .metric-marker-label { position:absolute; bottom:23px; left:50%; transform:translateX(-50%); font-size:10px; color:#d34b40; }
      .skill-cards-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:16px; }
      .skill-card { background:#fff; border:1px solid #dce5ed; border-radius:13px; overflow:hidden; box-shadow:0 3px 10px rgba(18,45,75,.06); }
      .skill-card-header { background:#155A96; color:#fff; padding:12px 16px; font-weight:800; letter-spacing:.5px; display:flex; justify-content:space-between; align-items:center; }
      .skill-card-body { display:grid; grid-template-columns:135px minmax(0,1fr); gap:12px; align-items:center; padding:16px; min-height:190px; }
      .skill-donut-wrap { width:142px; height:142px; position:relative; display:flex; align-items:center; justify-content:center; margin:auto; }
      .skill-donut { width:124px; height:124px; border-radius:50%; display:flex; align-items:center; justify-content:center; position:relative; margin:auto; }
      .skill-outer-ring { position:absolute; inset:0; border-radius:50%; -webkit-mask:radial-gradient(farthest-side, transparent calc(100% - 7px), #000 calc(100% - 6px)); mask:radial-gradient(farthest-side, transparent calc(100% - 7px), #000 calc(100% - 6px)); }
      .skill-rate-bar { padding:0 16px 8px; }
      .skill-rate-bar-label { display:flex; justify-content:space-between; gap:8px; font-size:12px; color:#263d50; margin-bottom:6px; }
      .skill-rate-track { height:9px; border-radius:99px; background:#f4eae2; overflow:hidden; }
      .skill-rate-fill { height:100%; border-radius:99px; }
      .rate-good { background:#159b68; }
      .rate-bad { background:#df4b3e; }
      .skill-rate-scale { display:flex; justify-content:space-between; font-size:9px; color:#8a99a6; margin-top:2px; }
      .skill-donut-hole { background:#fff; width:76px; height:76px; border-radius:50%; display:flex; flex-direction:column; align-items:center; justify-content:center; text-align:center; box-shadow:0 0 0 1px #edf1f4; }
      .skill-donut-value { font-size:20px; line-height:1.05; font-weight:900; color:#18344c; }
      .skill-donut-label { font-size:8px; letter-spacing:.7px; color:#60788b; font-weight:800; margin-top:4px; }
      .skill-legend { display:flex; flex-direction:column; gap:9px; min-width:0; }
      .skill-legend-row { display:grid; grid-template-columns:10px minmax(0,1fr) 44px 28px; gap:7px; align-items:center; font-size:12px; }
      .skill-legend-swatch { width:10px; height:10px; border-radius:3px; }
      .skill-legend-name { overflow-wrap:anywhere; color:#263d50; }
      .skill-legend-pct { text-align:right; font-weight:800; color:#263d50; white-space:nowrap; }
      .skill-legend-count { text-align:right; font-size:11px; color:#6d8192; }
      .skill-card-footer { border-top:1px dashed #dce5ed; margin:0 16px; padding:10px 0 12px; text-align:center; color:#6b8295; font-size:11px; }
      .passing-section-note { color:#718397; font-size:12px; margin:0 0 12px; }
      .passing-player-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:14px; align-items:stretch; }
      .passing-player-card { background:#fff; border:1px solid #d8e2ec; border-radius:13px; overflow:hidden; box-shadow:0 3px 10px rgba(18,45,75,.06); min-width:0; }
      .passing-player-header { background:#123f68 !important; color:#fff !important; padding:11px 14px; display:flex; justify-content:space-between; align-items:center; gap:8px; font-weight:800; }
      .passing-player-name { font-size:16px; line-height:1.2; }
      .passing-player-meta { font-size:11px; font-weight:500; opacity:.9; white-space:nowrap; }
      .passing-player-caption { padding:9px 12px 0; color:#718397; font-size:11px; }
      .passing-court-stage { display:grid; grid-template-columns:minmax(0,1fr) 190px minmax(0,1fr); grid-template-rows:54px 205px 54px; gap:5px 2px; padding:8px 9px 15px; min-height:350px; align-items:center; }
      .passing-point { text-align:center; min-width:0; z-index:2; color:#5d7081; }
      .passing-point-title { font-size:11px; letter-spacing:.35px; text-transform:uppercase; font-weight:800; margin-bottom:3px; }
      .passing-point-count { font-size:26px; font-weight:900; line-height:1; color:#263746; }
      .passing-point-sub { font-size:10px; line-height:1.3; margin-top:3px; white-space:normal; }
      .passing-point-empty { color:#a6b1bb; font-style:italic; font-size:11px; }
      .passing-overhand { grid-column:1 / 4; grid-row:1; }
      .passing-left { grid-column:1; grid-row:2; }
      .passing-right { grid-column:3; grid-row:2; }
      .passing-low { grid-column:1 / 4; grid-row:3; }
      .passing-player-figure { grid-column:2; grid-row:2; height:205px; width:190px; display:flex; justify-content:center; align-items:center; position:relative; z-index:1; overflow:visible; }
      .passing-player-figure img { display:block; width:100%; height:100%; max-width:none; max-height:none; object-fit:contain; object-position:center; }
      .passing-body-point { position:absolute; top:49%; left:50%; transform:translate(-50%,-50%); width:100%; min-height:54px; padding:5px 4px; border:0; border-radius:0; background:rgba(255,255,255,.18); text-align:center; box-shadow:none; }
      .passing-body-point { color:#123f68; font-weight:800; text-shadow:0 1px 2px rgba(255,255,255,.95); background:rgba(255,255,255,.78); border-radius:5px; }\n      .passing-body-point .passing-point-title { font-size:12px; font-weight:900; margin:0; color:#123f68; }
      .passing-body-point .passing-point-count { font-size:25px; font-weight:900; color:#123f68; }\n      .passing-body-point .passing-point-sub { font-weight:800; color:#123f68; }
      .passing-player-footer { border-top:1px solid #e7edf2; padding:8px 12px; display:flex; justify-content:space-between; color:#607488; font-size:10px; gap:8px; }
      .passing-player-footer strong { color:#263746; }
      @media(max-width:1050px) { .passing-player-grid { grid-template-columns:repeat(2,minmax(0,1fr)); } }
      @media(max-width:1050px) { .passing-court-stage { grid-template-columns:minmax(0,1fr) 170px minmax(0,1fr); } .passing-player-figure { width:170px; } }
      @media(max-width:650px) { .passing-player-grid { grid-template-columns:1fr; } .passing-court-stage { grid-template-columns:minmax(0,1fr) 145px minmax(0,1fr); grid-template-rows:54px 185px 54px; } .passing-player-figure { width:145px; height:185px; } .passing-body-point { width:100%; } }
      @media(max-width:1000px) { .skill-cards-grid { grid-template-columns:repeat(2,minmax(0,1fr)); } }
      @media(max-width:650px) { .skill-cards-grid { grid-template-columns:1fr; } .skill-card-body { grid-template-columns:120px minmax(0,1fr); } }
      @media(max-width:850px) { .metric-row { grid-template-columns:minmax(140px, 1fr) 65px; gap:10px; } .metric-bar-area { grid-column:1 / -1; } }
      @media(max-width:700px) {
        .dashboard-header {
          grid-template-columns: 1fr !important;
        }

        .match-summary {
          text-align: left !important;
          border-left: 0 !important;
          padding-left: 0 !important;
        }
      }
    "))
  ),

  # Original Huskies header
  div(
    class = "dashboard-header",

    div(
      class = "brand-area",

      img(
        src = "keyano-huskies-logo.png",
        class = "team-logo",
        alt = "Keyano Huskies logo"
      ),

      div(
        class = "brand-copy",

        h1("HUSKIES COACHES DASHBOARD"),

        div(
          class = "season-label",
          "KEYANO · ACAC · 2026–27"
        )
      )
    ),

    div(
      class = "match-summary",

      div(
        class = "eyebrow",
        "LATEST MATCH"
      ),

      h3(
        textOutput("latest_match", inline = TRUE)
      ),

      div(
        class = "match-detail",
        textOutput("match_details", inline = TRUE)
      )
    ),

    div(
      class = "athletes-link",

      actionButton(
        "athletes_nav",
        label = NULL,
        icon = icon("grip"),
        class = "athletes-icon-button"
      ),

      div(
        "ATHLETES",
        class = "athletes-label"
      )
    )
  ),

  # Match-window buttons
  div(
    class = "period-nav",
    uiOutput("period_buttons")
  ),

  # Single continuous scrolling content area
  div(
    class = "content-card",
    style = "width:100%; max-width:none; box-sizing:border-box;",
    uiOutput("page_content")
  ),

  div(
    class = "footer-note",
    "Keyano Huskies · ACAC · 2026–27 | Internal performance analysis"
  )
)

# ---------------- SERVER ----------------

server <- function(input, output, session) {

  selected_period <- reactiveVal(
    "Most Recent Match"
  )

  show_athletes <- reactiveVal(FALSE)

  matches <- reactive({

    invalidateLater(60000, session)

    load_matches()
  })

  period_matches <- reactive({

    ms <- matches()

    if (!length(ms)) {
      return(list())
    }

    n <- length(ms)

    switch(
      selected_period(),

      "Most Recent Match" = ms[1],

      "Previous Match" = {
        if (n >= 2) ms[2] else list()
      },

      "Last 5" = ms[
        seq_len(min(5, n))
      ],

      "Last 10" = ms[
        seq_len(min(10, n))
      ],

      "Full Season" = ms,

      ms[1]
    )
  })

  current_match <- reactive({

    ms <- period_matches()

    if (length(ms)) {
      ms[[1]]
    } else {
      NULL
    }
  })

  # Header latest-match details
  output$latest_match <- renderText({

    m <- current_match()

    if (is.null(m)) {
      "Match data not connected yet"
    } else {
      m$opponent
    }
  })

  output$match_details <- renderText({

    m <- current_match()

    if (is.null(m)) {
      "Sets · receptions · swings · rallies"
    } else {

      d <- if (is.na(m$date)) {
        "Date unknown"
      } else {
        format(m$date, "%b %d, %Y")
      }

      paste(
        d,
        "·",
        ifelse(
          is.na(m$result),
          "Result not recorded",
          m$result
        )
      )
    }
  })

  # Match-window buttons
  output$period_buttons <- renderUI({

    div(
      class = "period-buttons",

      lapply(match_windows, function(period) {

        actionButton(
          inputId = paste0(
            "period_",
            gsub("[^A-Za-z0-9]", "", period)
          ),

          label = period,

          class = if (
            identical(selected_period(), period)
          ) {
            "period-button active"
          } else {
            "period-button"
          },

          width = "100%"
        )
      })
    )
  })

  lapply(match_windows, function(period) {

    local({

      this_period <- period

      observeEvent(
        input[[
          paste0(
            "period_",
            gsub("[^A-Za-z0-9]", "", this_period)
          )
        ]],

        {
          selected_period(this_period)
          show_athletes(FALSE)
        }
      )
    })
  })

  observeEvent(
    input$athletes_nav,
    {
      show_athletes(!show_athletes())
    }
  )

  # ---------------- PAGE CONTENT ----------------

  output$page_content <- renderUI({

    # Athletes view
    if (show_athletes()) {

      return(
        tagList(

          h2("Athletes"),

          p(
            class = "section-intro",
            "Season roster. Public-facing names use first names only."
          ),

          textInput(
            "athlete_search",
            "Search athletes",
            placeholder = "First name, jersey number, or position..."
          ),

          tableOutput("roster_table")
        )
      )
    }

    ms <- period_matches()
    m <- current_match()

    # Workbook diagnostics if no match loads
    if (is.null(m)) {

      files <- if (dir.exists(match_dir)) {
        list.files(
          match_dir,
          pattern = "\\.xlsx$",
          full.names = TRUE,
          ignore.case = TRUE
        )
      } else {
        character()
      }

      files <- files[
        !grepl("^~\\$", basename(files))
      ]

      file_errors <- character()

      if (length(files)) {

        for (f in files) {

          err <- tryCatch({

            x <- read_match(f)

            if (!is.null(x$error)) {
              x$error
            } else {
              "Workbook parsed, but no match was returned."
            }

          }, error = function(e) {
            conditionMessage(e)
          })

          file_errors <- c(
            file_errors,
            paste0(basename(f), ": ", err)
          )
        }
      }

      return(
        tagList(

          div(
            class = "section-heading-row",

            div(
              h2(selected_period()),

              p(
                class = "section-intro",
                "Team performance overview"
              )
            ),

            span(
              class = "data-status",
              "WAITING FOR MATCH DATA"
            )
          ),

          div(
            class = "placeholder-panel",

            h3("Match data could not be loaded"),

            if (!dir.exists(match_dir)) {

              p(
                "The data/matches folder is missing from the deployed app."
              )

            } else if (!length(files)) {

              p(
                "No .xlsx files were found in data/matches/."
              )

            } else {

              tagList(

                p(
                  "The workbook files were found. The exact parsing error is:"
                ),

                div(
                  class = "status-error",

                  lapply(
                    file_errors,
                    function(x) p(x)
                  )
                )
              )
            },

            p(
              "Keep your workbook in data/matches/ and check that the deployment includes the latest committed file."
            )
          )
        )
      )
    }

    # Key Metrics performance profile; no duplicate summary cards.
    date_label <- if (is.na(m$date)) "Date unknown" else format(m$date, "%b %d, %Y")

    tagList(
      div(
        class = "analytics-section",
        div(
          class = "section-heading-row",
          div(
            h2(selected_period()),
            p(class = "section-intro", paste("Team performance overview ·", m$opponent, "·", date_label))
          ),
          span(class = "data-status", "MATCH DATA CONNECTED")
        ),
        div(
          class = "key-metrics-panel",
          h2("KEY METRICS · PERFORMANCE PROFILE"),
          p(class = "section-intro", "Grouped by game phase, passing, key offensive and key defensive metrics. Bars use fixed scales: 100% for percentage metrics and 1.000 for efficiency metrics."),
          uiOutput("key_metrics_ui")
        )
      ),

      # 2. By Skill
      div(
        class = "analytics-section",

        section_title(
          "BY SKILL",
          "Attack, blocking, serving, reception and other skill totals from the workbook."
        ),

        uiOutput("skill_cards_ui")
      ),

      # 3. Attack Detail
      div(
        class = "analytics-section",

        section_title(
          "ATTACK DETAIL",
          "First-ball and transition attack by attack type."
        ),

        div(
          class = "chart-card table-wrap attack-summary-card",
          div(
            style = "display:flex; align-items:center; justify-content:space-between; gap:16px; margin-bottom:10px;",
            h3("ATTACK TYPE SUMMARY", style = "margin:0;"),
            selectInput(
              "attack_phase_filter", NULL,
              choices = c("All" = "All", "First Ball (FB)" = "FB", "Transition" = "Transition"),
              selected = "All", width = "190px"
            )
          ),
          tableOutput("attack_table")
        ),

        div(
          class = "chart-card attack-type-detail",
          h3("ATTACK TYPE BREAKDOWN"),
          p(class = "section-intro", "Select an attack type to compare first-ball, transition and combined efficiency for the selected match period."),
          uiOutput("attack_type_selector"),
          plotOutput("attack_efficiency_plot", height = 220)
        )
      ),

      # 4. Rotation Details
      div(
        class = "analytics-section",

        section_title(
          "ROTATION DETAILS",
          "Serve, reception and rotation outcomes."
        ),

        div(
          class = "chart-card",
          plotOutput("rotation_plot", height = 320)
        ),

        div(
          class = "chart-card table-wrap",
          uiOutput("rotation_table")
        )
      ),

      # 5. Setting Stats
      div(
        class = "analytics-section",

        section_title(
          "SETTING STATS",
          "Rotation distribution and who sets on first-ball and transition."
        ),

        tags$style(HTML("
          .setting-stats-card { width:100%; max-width:none; box-sizing:border-box; }
          .setting-stats-heading { display:flex; align-items:center; justify-content:space-between; gap:16px; flex-wrap:wrap; }
          .setting-stats-heading h4 { margin:0 0 12px 0; }
          .setting-setter-label { margin:0 0 12px auto; color:#60788d; font-size:13px; font-weight:800; text-transform:uppercase; letter-spacing:.04em; }
          .setting-stats-card table { width:100% !important; table-layout:fixed; }
          .setting-stats-card .table-responsive { width:100%; }
          .setting-stats-card th, .setting-stats-card td { white-space:normal; text-align:center !important; vertical-align:middle; }
          .setting-stats-card thead th { font-weight:800; }
          /* Keep ordinary setting-table values readable without making every number bold. */
          .setting-stats-card tbody td { font-weight:400 !important; font-variant-numeric:tabular-nums; }
          .setting-stats-card tbody td:first-child { font-weight:400 !important; }
          .setting-stats-card .table-responsive table { margin:0; }
          /* Emphasize only the Total row in Rotation setting totals. */
          .rotation-setting-card tbody tr:last-child td {
            background:#e7eef5 !important;
            color:#17324d !important;
            font-weight:800 !important;
            border-top:3px solid #17324d;
          }
        ")),

        div(
          class = "chart-card table-wrap setting-stats-card rotation-setting-card",
          div(
            class = "setting-stats-heading",
            h4("Rotation setting totals"),
            div(class = "setting-setter-label", "Setter · Kai Rosenau")
          ),
          tableOutput("rotation_setting_table")
        ),

        fluidRow(
          column(
            6,
            div(
              class = "chart-card table-wrap setting-stats-card",
              h4("Who Sets · First Ball"),
              tableOutput("fb_table")
            )
          ),
          column(
            6,
            div(
              class = "chart-card table-wrap setting-stats-card",
              h4("Who Sets · Transition"),
              tableOutput("trans_table")
            )
          )
        )
      ),

      # 6. Passing by Contact Point
      div(
        class = "analytics-section",

        section_title(
          "PASSING BY CONTACT POINT",
          "Reception outcomes by player and contact point."
        ),

        p(
          class = "passing-section-note",
          "Each player is shown separately. The contact-point labels are arranged around the player; the figures reflect the selected match period."
        ),
        uiOutput("passing_cards_ui")
      )
    )
  })

  # ---------------- ATHLETES TABLE ----------------

  output$roster_table <- renderTable({

    search <- if (is.null(input$athlete_search)) {
      ""
    } else {
      tolower(input$athlete_search)
    }

    roster |>
      filter(
        grepl(
          search,
          tolower(
            paste(FirstName, Jersey, Position)
          )
        )
      ) |>
      rename(
        `Jersey #` = Jersey,
        `First Name` = FirstName
      )

  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s")

  # ---------------- KEY METRICS ----------------

  output$key_metrics_ui <- renderUI({
    selected_matches <- period_matches()
    req(length(selected_matches) > 0)

    # Use the most recent workbook as the template so every metric and
    # description present in the Excel Key Metrics sheet remains visible.
    template <- selected_matches[[1]]$key
    if (is.null(template) || !nrow(template)) {
      return(div(class = "status-error", "No numeric Key Metrics were found in the selected workbooks."))
    }

    # Calculate the mean for each metric across all matches in the selected period.
    # Match by metric name; skip missing/non-numeric observations.
    metric_mean <- function(metric_name) {
      vals <- vapply(selected_matches, function(match_data) {
        k <- match_data$key
        if (is.null(k) || !nrow(k)) return(NA_real_)
        hit <- which(trimws(k$metric) == trimws(metric_name))
        if (!length(hit)) return(NA_real_)
        suppressWarnings(as.numeric(k$value[hit[1]]))
      }, numeric(1))
      if (!any(is.finite(vals))) NA_real_ else mean(vals[is.finite(vals)])
    }

    d <- template
    d$value <- vapply(d$metric, metric_mean, numeric(1))
    d <- d[is.finite(d$value), , drop = FALSE]
    if (!nrow(d)) return(div(class = "status-error", "No numeric Key Metrics were found in the selected workbooks."))

    # Respect the workbook's section headings first. This keeps Conversion %
    # under Key Defensive as shown in the supplied workbook.
    category_group <- function(cat, metric) {
      c <- tolower(trimws(as.character(cat)))
      m <- tolower(trimws(as.character(metric)))
      if (grepl("game phase", c)) return("GAME PHASE")
      if (grepl("passing", c)) return("PASSING")
      if (grepl("key defensive|defensive", c)) return("KEY DEFENSIVE")
      if (grepl("key offensive|offensive", c)) return("KEY OFFENSIVE")
      if (grepl("game phase|side-out|first-ball so|point scoring", m)) return("GAME PHASE")
      if (grepl("passing|in-system|aced", m)) return("PASSING")
      if (grepl("defensive|block|disruption|dig|opponent|conversion", m)) return("KEY DEFENSIVE")
      if (grepl("offensive|attack", m)) return("KEY OFFENSIVE")
      toupper(as.character(cat))
    }

    d$display_group <- mapply(category_group, d$category, d$metric, USE.NAMES = FALSE)
    d$metric_lower_better <- grepl("aced %|opponent attack eff|error|deadball|disruption", d$metric, ignore.case = TRUE)
    d$is_eff <- grepl("eff", d$metric, ignore.case = TRUE)
    d$is_pct <- grepl("%", d$metric, fixed = TRUE)

    # Fixed scale: percentages 0-100%; efficiency 0-1.000.
    d$scale_value <- ifelse(
      d$is_eff,
      d$value,
      ifelse(d$is_pct & abs(d$value) <= 1, d$value * 100, d$value)
    )
    d$bar_value <- pmax(0, pmin(ifelse(d$is_eff, 1, 100), d$scale_value))

    # Marker is the average of the matches immediately outside the selected window,
    # when one exists. For Most Recent / Previous Match, this is the next older match.
    all_matches <- matches()
    selected_files <- vapply(selected_matches, function(x) x$file, character(1))
    older <- all_matches[!vapply(all_matches, function(x) x$file %in% selected_files, logical(1))]
    comparison_match <- if (length(older)) older else NULL

    comparison_mean <- function(metric_name) {
      if (is.null(comparison_match)) return(NA_real_)
      vals <- vapply(comparison_match, function(match_data) {
        k <- match_data$key
        if (is.null(k) || !nrow(k)) return(NA_real_)
        hit <- which(trimws(k$metric) == trimws(metric_name))
        if (!length(hit)) return(NA_real_)
        suppressWarnings(as.numeric(k$value[hit[1]]))
      }, numeric(1))
      if (!any(is.finite(vals))) NA_real_ else mean(vals[is.finite(vals)])
    }

    groups_order <- c("GAME PHASE", "PASSING", "KEY OFFENSIVE", "KEY DEFENSIVE")
    groups <- c(groups_order[groups_order %in% d$display_group], setdiff(unique(d$display_group), groups_order))

    tagList(lapply(groups, function(g) {
      rows <- d[d$display_group == g, , drop = FALSE]
      div(class = "metric-group",
        div(class = "metric-group-title", g),
        lapply(seq_len(nrow(rows)), function(i) {
          r <- rows[i, , drop = FALSE]
          maxval <- if (r$is_eff) 1 else 100
          width <- max(0, min(100, r$bar_value / maxval * 100))
          val_label <- if (r$is_eff) sprintf("%.3f", r$value) else paste0(sprintf("%.1f", r$scale_value), "%")
          cmp_value <- comparison_mean(r$metric)
          cmp_bar <- if (is.na(cmp_value)) NA_real_ else if (r$is_eff) cmp_value else if (r$is_pct && abs(cmp_value) <= 1) cmp_value * 100 else cmp_value
          marker <- if (!is.na(cmp_bar) && is.finite(cmp_bar)) paste0("left:", max(0, min(100, cmp_bar / maxval * 100)), "%") else NULL
          cmp_label <- if (is.null(comparison_match)) "" else paste(vapply(comparison_match, function(x) x$opponent, character(1)), collapse = "/")
          div(class = "metric-row",
            div(class = "metric-copy",
              div(class = "metric-name", r$metric),
              div(class = "metric-description", ifelse(nzchar(r$description), r$description, " "))
            ),
            div(class = "metric-value", val_label),
            div(class = "metric-bar-area",
              div(class = "metric-track", div(class = paste("metric-fill", if (r$metric_lower_better) "metric-bad" else "metric-good"), style = paste0("width:", width, "%"))),
              if (!is.null(marker)) div(class = "metric-marker-wrap", style = marker, title = paste("Comparison average:", cmp_value), div(class = "metric-marker"), div(class = "metric-marker-label", paste0(cmp_label, " ", if (r$is_eff) sprintf("%.3f", cmp_value) else paste0(round(cmp_bar, 1), "%"))))
            )
          )
        })
      )
    }))
  })

  # ---------------- BY SKILL ----------------

  output$skill_cards_ui <- renderUI({
    selected_matches <- period_matches()
    req(length(selected_matches) > 0)

    all_skills <- bind_rows(lapply(selected_matches, function(mm) {
      if (is.null(mm$skills) || !nrow(mm$skills)) {
        return(tibble(skill=character(), metric=character(), value=numeric()))
      }
      mm$skills |>
        mutate(
          skill=trimws(as.character(skill)),
          metric=trimws(as.character(metric)),
          value=suppressWarnings(as.numeric(value))
        )
    }))

    if (!nrow(all_skills)) {
      return(div(class="status-error", "No By Skill data found in the selected workbook(s)."))
    }

    find_skill <- function(pattern) {
      hit <- unique(all_skills$skill[grepl(pattern, all_skills$skill, ignore.case=TRUE)])
      if (length(hit)) hit[1] else NA_character_
    }
    attack_skill <- find_skill("^Attack$")
    receive_skill <- find_skill("Serve Receive|Reception")
    block_skill <- find_skill("Block")
    serve_skill <- find_skill("^Serve$")
    defense_skill <- find_skill("Defense")
    gapp_skill <- find_skill("GAPP")

    skill_rows <- function(skill_name) {
      if (is.na(skill_name)) return(tibble(metric=character(), value=numeric()))
      all_skills |>
        filter(tolower(skill) == tolower(skill_name), is.finite(value)) |>
        group_by(metric) |>
        summarise(
          value = if (any(grepl("%", metric, fixed=TRUE))) mean(value) else sum(value),
          .groups="drop"
        )
    }
    get_skill_value <- function(d, nm) {
      z <- d$value[tolower(trimws(d$metric)) == tolower(trimws(nm))]
      if (length(z)) z[1] else NA_real_
    }
    key_metric_mean <- function(metric_name) {
      vals <- vapply(selected_matches, function(mm) {
        k <- mm$key
        if (is.null(k) || !nrow(k)) return(NA_real_)
        idx <- which(tolower(trimws(k$metric)) == tolower(trimws(metric_name)))
        if (!length(idx)) return(NA_real_)
        suppressWarnings(as.numeric(k$value[idx[1]]))
      }, numeric(1))
      if (any(is.finite(vals))) mean(vals[is.finite(vals)]) else NA_real_
    }
    pct_value <- function(x) {
      if (!length(x) || !is.finite(x)) return(NA_real_)
      if (abs(x) <= 1) x * 100 else x
    }

    # Positive/negative outcome colours; card headers remain Huskies blue.
    palette <- c("#159B68", "#2F75B5", "#E5483B", "#E6A52B", "#7D9DB8", "#A9C8E2")
    make_card <- function(title, skill_name, center_value, center_label,
                          legend_metrics, footer="", extra_bar=NULL,
                          outer_ring_pct=NULL, show_legend_pct=TRUE) {
      d <- skill_rows(skill_name)
      if (is.na(skill_name) || !nrow(d)) {
        return(div(class="skill-card", div(class="skill-card-header", title),
                   div(class="skill-card-body", "No data in workbook")))
      }
      d$metric <- trimws(as.character(d$metric))
      legend_metrics <- legend_metrics[legend_metrics %in% d$metric]
      ld <- d[match(legend_metrics, d$metric), , drop=FALSE]
      ld <- ld[!is.na(ld$metric) & is.finite(ld$value), , drop=FALSE]
      counts <- pmax(0, ld$value)
      total <- sum(counts, na.rm=TRUE)
      pct <- if (total > 0) counts / total * 100 else rep(0, length(counts))
      donut_style <- if (length(pct) && total > 0) {
        ends <- cumsum(pct)
        starts <- c(0, head(ends, -1))
        slices <- paste0(
          palette[(seq_along(pct)-1) %% length(palette)+1],
          " ", round(starts, 2), "% ", round(ends, 2), "%"
        )
        paste0("background:conic-gradient(", paste(slices, collapse=", "), ");")
      } else {
        "background:conic-gradient(#dce5ed 0% 100%);"
      }
      legend <- if (nrow(ld)) lapply(seq_len(nrow(ld)), function(i) {
        val <- ld$value[i]
        div(class="skill-legend-row",
          div(class="skill-legend-swatch", style=paste0("background:", palette[(i-1) %% length(palette)+1])),
          div(class="skill-legend-name", ld$metric[i]),
          div(class="skill-legend-pct",
              if (show_legend_pct) paste0(round(pct[i]), "%") else ""),
          div(class="skill-legend-count",
              if (abs(val-round(val)) < .001) as.character(round(val)) else format(round(val, 1), nsmall=1))
        )
      }) else list(div("No category data"))

      donut <- div(class="skill-donut-wrap",
        if (!is.null(outer_ring_pct) && is.finite(outer_ring_pct)) {
          div(class="skill-outer-ring",
              style=paste0("background:conic-gradient(#E6A52B 0% ", max(0,min(100,outer_ring_pct)), "%, #eaf0f4 ", max(0,min(100,outer_ring_pct)), "% 100%);"))
        },
        div(class="skill-donut", style=donut_style,
          div(class="skill-donut-hole",
            div(class="skill-donut-value", center_value),
            div(class="skill-donut-label", center_label)
          )
        )
      )
      div(class="skill-card",
        div(class="skill-card-header", title, span(style="font-size:10px;opacity:.9;", center_label)),
        div(class="skill-card-body",
          donut,
          div(class="skill-legend", legend)
        ),
        if (!is.null(extra_bar) && is.finite(extra_bar$value)) {
          div(class="skill-rate-bar",
            div(class="skill-rate-bar-label",
              span(extra_bar$label),
              strong(paste0(round(extra_bar$value, 1), "%"))
            ),
            div(class="skill-rate-track",
              div(class=paste("skill-rate-fill", if (isTRUE(extra_bar$lower_better)) "rate-bad" else "rate-good"),
                  style=paste0("width:", max(0,min(100,extra_bar$value/50*100)), "%"))
            ),
            div(class="skill-rate-scale", "0%", "50%")
          )
        },
        div(class="skill-card-footer", footer)
      )
    }

    attack <- skill_rows(attack_skill)
    attack_get <- function(nm) get_skill_value(attack, nm)
    swings <- attack_get("Total Swings")
    kills <- attack_get("Kills")
    attack_eff <- key_metric_mean("Attack Efficiency")
    attack_center <- if (is.finite(attack_eff)) {
      sprintf("%.3f", attack_eff)
    } else "—"

    rec <- skill_rows(receive_skill)
    rec_get <- function(nm) get_skill_value(rec, nm)
    good_rec <- rec_get("Good Pass")
    oos_rec <- rec_get("OOS Pass")
    aced_rec <- rec_get("Aced")
    over_rec <- rec_get("Overpass")
    rec_total <- sum(c(good_rec,oos_rec,aced_rec,over_rec)[is.finite(c(good_rec,oos_rec,aced_rec,over_rec))])
    good_pct <- if (rec_total > 0 && is.finite(good_rec)) good_rec / rec_total * 100 else NA_real_
    allowed_disruption <- pct_value(rec_get("Allowed Disruption %"))

    block <- skill_rows(block_skill)
    block_total <- sum(block$value[tolower(block$metric) %in% tolower(c("Stuff","Positive","Fulfilled","Negative","Error"))], na.rm=TRUE)

    serve <- skill_rows(serve_skill)
    serve_get <- function(nm) get_skill_value(serve, nm)
    serve_count <- sum(serve$value[!grepl("%", serve$metric, fixed=TRUE)], na.rm=TRUE)
    disruption <- pct_value(serve_get("Disruption %"))

    defense <- skill_rows(defense_skill)
    digs <- get_skill_value(defense, "Digs")
    in_play <- get_skill_value(defense, "In Play")
    converted <- get_skill_value(defense, "Kills (Converted)")
    conversion <- if (is.finite(in_play) && in_play > 0 && is.finite(converted)) converted/in_play*100 else NA_real_

    gapp <- skill_rows(gapp_skill)
    gapp_val <- get_skill_value(gapp, "GAPP %")
    gapp_pct <- pct_value(gapp_val)
    possessions <- get_skill_value(gapp, "Total Possessions")
    attacks <- get_skill_value(gapp, "Attacks")

    tagList(
      div(class="skill-cards-grid",
        make_card("ATTACK", attack_skill, attack_center, "ATK EFF",
          c("Kills", "In Play", "Blocked", "Error"),
          if (is.finite(swings)) paste0("Kill% ", if(is.finite(kills) && swings>0) round(kills/swings*100) else 0, " · ", round(swings), " swings (reference)") else ""),
        make_card("SERVE RECEIVE", receive_skill,
          if (is.finite(good_pct)) paste0(round(good_pct), "%") else "—",
          "GOOD PASS", c("Good Pass", "OOS Pass", "Aced", "Overpass"),
          if(is.finite(rec_total)) paste0(round(rec_total), " receptions") else "",
          extra_bar=if(is.finite(allowed_disruption)) list(label="Allowed Disruption %", value=allowed_disruption, lower_better=TRUE) else NULL),
        make_card("BLOCK", block_skill, if (is.finite(block_total)) as.character(round(block_total)) else "—",
          "BLOCK EVENTS", c("Stuff", "Positive", "Error"),
          if (is.finite(block_total)) paste0(round(block_total), " block outcomes") else ""),
        make_card("SERVE", serve_skill, if(is.finite(serve_count)) as.character(round(serve_count)) else "—",
          "SERVES", c("Aces", "Overpass", "OOS Pass", "Good Pass", "Serve Error"),
          if(is.finite(serve_count)) paste0(round(serve_count), " serves · ", if(is.finite(serve_get("Aces"))) round(serve_get("Aces")) else 0, " aces") else "",
          extra_bar=if(is.finite(disruption)) list(label="Disruption %", value=disruption, lower_better=FALSE) else NULL),
        make_card("DEFENSE", defense_skill, if(is.finite(digs)) as.character(round(digs)) else "—",
          "DIGS", c("In Play", "Kills (Converted)"),
          if(is.finite(conversion)) paste0("Conversion ", round(conversion), "% · ", if(is.finite(converted)) round(converted) else 0, " converted") else "",
          outer_ring_pct=conversion, show_legend_pct=FALSE),
        make_card("GAPP", gapp_skill, if(is.finite(gapp_pct)) paste0(round(gapp_pct, 1), "%") else "—",
          "GAPP %", c("Total Possessions", "Attacks"), 
          paste0(if(is.finite(attacks)) round(attacks) else "—", " attacks / ", if(is.finite(possessions)) round(possessions) else "—", " possessions"),
          show_legend_pct=FALSE)
      )
    )
  })

  # ---------------- ATTACK DETAIL ----------------

  # Prepare attack rows from every workbook in the selected period. Counts are
  # summed by attack type and previous-play phase; efficiencies are recalculated
  # from the pooled counts rather than averaging match-level efficiencies.
  attack_period_data <- reactive({
    selected_matches <- period_matches()
    req(length(selected_matches) > 0)

    pieces <- lapply(selected_matches, function(mm) {
      d <- mm$attack
      if (is.null(d) || !nrow(d) || !("Attack Type" %in% names(d))) return(NULL)
      d <- as.data.frame(d, stringsAsFactors = FALSE)
      d[["Attack Type"]] <- trimws(as.character(d[["Attack Type"]]))
      if ("Previous Play" %in% names(d)) {
        d[["Previous Play"]] <- trimws(as.character(d[["Previous Play"]]))
      } else {
        d[["Previous Play"]] <- "FB"
      }
      # Workbook continuation rows leave Attack Type blank; carry the label down.
      d[["Attack Type"]][d[["Attack Type"]] == ""] <- NA_character_
      d[["Attack Type"]] <- tidyr::fill(tibble::tibble(x = d[["Attack Type"]]), x, .direction = "down")$x
      for (nm in c("Attempts", "Kills", "Error")) {
        if (!nm %in% names(d)) d[[nm]] <- 0
        d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
        d[[nm]][!is.finite(d[[nm]])] <- 0
      }
      d <- d[!is.na(d[["Attack Type"]]) & nzchar(d[["Attack Type"]]), , drop = FALSE]
      d
    })
    d <- dplyr::bind_rows(pieces)
    if (!nrow(d)) return(data.frame())
    d[["Previous Play"]][tolower(d[["Previous Play"]]) %in% c("first ball", "first-ball", "firstball")] <- "FB"
    d[["Previous Play"]][tolower(d[["Previous Play"]]) %in% c("trans", "transition attack")] <- "Transition"
    d
  })

  attack_eff <- function(kills, errors, attempts) {
    ifelse(is.finite(attempts) & attempts > 0, (kills - errors) / attempts, NA_real_)
  }

  attack_summary <- reactive({
    d <- attack_period_data()
    if (!nrow(d)) return(data.frame())
    types <- unique(d[["Attack Type"]])
    rows <- lapply(types, function(tp) {
      z <- d[d[["Attack Type"]] == tp, , drop = FALSE]
      fb <- z[tolower(z[["Previous Play"]]) %in% c("fb", "first ball", "first-ball", "firstball"), , drop = FALSE]
      tr <- z[tolower(z[["Previous Play"]]) %in% c("transition", "trans", "transition attack"), , drop = FALSE]
      sumcol <- function(dat, col) sum(dat[[col]], na.rm = TRUE)
      fb_a <- sumcol(fb, "Attempts"); fb_k <- sumcol(fb, "Kills"); fb_e <- sumcol(fb, "Error")
      tr_a <- sumcol(tr, "Attempts"); tr_k <- sumcol(tr, "Kills"); tr_e <- sumcol(tr, "Error")
      all_a <- fb_a + tr_a; all_k <- fb_k + tr_k; all_e <- fb_e + tr_e
      data.frame(
        `Attack Type` = tp,
        Attempts = all_a,
        Kills = all_k,
        Errors = all_e,
        `FB Eff.` = attack_eff(fb_k, fb_e, fb_a),
        `Transition Eff.` = attack_eff(tr_k, tr_e, tr_a),
        `Total Eff.` = attack_eff(all_k, all_e, all_a),
        `FB Attempts` = fb_a,
        `FB Kills` = fb_k,
        `FB Errors` = fb_e,
        `Transition Attempts` = tr_a,
        `Transition Kills` = tr_k,
        `Transition Errors` = tr_e,
        check.names = FALSE
      )
    })
    out <- dplyr::bind_rows(rows)
    out[order(match(out[["Attack Type"]], types)), , drop = FALSE]
  })

  output$attack_table <- renderTable({
    d <- attack_summary()
    validate(need(nrow(d) > 0, "No attack detail found in the selected matches."))

    phase <- if (is.null(input$attack_phase_filter) || !nzchar(input$attack_phase_filter)) "All" else input$attack_phase_filter
    if (identical(phase, "FB")) {
      shown <- data.frame(
        `Attack Type` = d[["Attack Type"]],
        Attempts = d[["FB Attempts"]],
        Kills = d[["FB Kills"]],
        Errors = d[["FB Errors"]],
        `FB Eff.` = d[["FB Eff."]],
        check.names = FALSE
      )
    } else if (identical(phase, "Transition")) {
      shown <- data.frame(
        `Attack Type` = d[["Attack Type"]],
        Attempts = d[["Transition Attempts"]],
        Kills = d[["Transition Kills"]],
        Errors = d[["Transition Errors"]],
        `Transition Eff.` = d[["Transition Eff."]],
        check.names = FALSE
      )
    } else {
      shown <- d[, c("Attack Type", "Attempts", "Kills", "Errors", "FB Eff.", "Transition Eff.", "Total Eff."), drop = FALSE]
    }

    for (nm in intersect(c("Attempts", "Kills", "Errors"), names(shown))) {
      shown[[nm]] <- ifelse(is.finite(shown[[nm]]), as.character(as.integer(round(shown[[nm]]))), "—")
    }
    for (nm in intersect(c("FB Eff.", "Transition Eff.", "Total Eff."), names(shown))) {
      shown[[nm]] <- ifelse(is.finite(shown[[nm]]), sprintf("%.3f", shown[[nm]]), "—")
    }
    shown
  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s", width = "100%", align = "l")


  output$attack_type_selector <- renderUI({
    d <- attack_summary()
    types <- if (nrow(d)) as.character(d[["Attack Type"]]) else character()
    selectInput("attack_type_selected", "Attack type", choices = types,
                selected = if (length(types)) types[1] else NULL, width = "320px")
  })

  output$attack_efficiency_plot <- renderPlot({
    d <- attack_summary()
    req(nrow(d) > 0, input$attack_type_selected)
    selected <- d[d[["Attack Type"]] == input$attack_type_selected, , drop = FALSE]
    validate(need(nrow(selected) > 0, "Select an attack type."))
    vals <- data.frame(
      Phase = factor(c("First Ball", "Transition", "Total"), levels = c("First Ball", "Transition", "Total")),
      Efficiency = as.numeric(unlist(selected[1, c("FB Eff.", "Transition Eff.", "Total Eff.")], use.names = FALSE))
    )
    vals <- vals[is.finite(vals$Efficiency), , drop = FALSE]
    validate(need(nrow(vals) > 0, "No efficiency values are available for this attack type."))
    ggplot(vals, aes(x = Efficiency, y = Phase, fill = Phase)) +
      geom_vline(xintercept = 0, colour = "#AAB7C4", linewidth = 0.6) +
      geom_col(width = 0.48, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.3f", Efficiency)),
                hjust = ifelse(vals$Efficiency >= 0, -0.18, 1.18), size = 4, colour = "#17324D") +
      scale_fill_manual(values = c("First Ball" = "#2F78B7", "Transition" = "#75A9D6", "Total" = "#174F86")) +
      scale_x_continuous(limits = c(min(-0.1, min(vals$Efficiency) - 0.08), max(1.05, max(vals$Efficiency) + 0.12)),
                         labels = function(x) sprintf("%.2f", x)) +
      labs(x = "Attack Efficiency  (Kills − Errors) / Attempts", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            axis.text.y = element_text(face = "bold", colour = "#17324D"),
            plot.margin = margin(10, 30, 10, 10))
  })

  # ---------------- ROTATION DETAILS ----------------

  output$rotation_plot <- renderPlot({

    selected_matches <- period_matches()
    req(length(selected_matches) > 0)

    # Aggregate across the selected period: average rate metrics by rotation.
    rotation_list <- lapply(selected_matches, function(mm) {
      if (is.null(mm$rotation) || !nrow(mm$rotation)) return(NULL)
      mm$rotation
    })
    rotation_list <- Filter(Negate(is.null), rotation_list)
    validate(need(length(rotation_list) > 0, "No rotation details are available."))

    all_rotation <- bind_rows(rotation_list)
    validate(need("Rotation" %in% names(all_rotation), "Rotation column is missing."))
    rate_cols <- intersect(c("SO%", "PS%", "Serve In %"), names(all_rotation))
    validate(need(length(rate_cols) > 0, "No rotation rate columns found."))

    for (nm in rate_cols) all_rotation[[nm]] <- suppressWarnings(as.numeric(all_rotation[[nm]]))
    all_rotation <- all_rotation |>
      filter(grepl("^Ro[1-6]$", as.character(Rotation))) |>
      group_by(Rotation) |>
      summarise(across(all_of(rate_cols), ~ {
        vals <- .x[is.finite(.x)]
        if (length(vals)) mean(vals) else NA_real_
      }), .groups = "drop")

    long <- all_rotation |>
      pivot_longer(-Rotation, names_to = "Metric", values_to = "Value") |>
      filter(is.finite(Value))
    validate(need(nrow(long) > 0, "No rotation values found."))

    ggplot(long, aes(x = Rotation, y = Value, fill = Metric)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Rate", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$rotation_table <- renderUI({

    selected_matches <- period_matches()
    req(length(selected_matches) > 0)

    rotation_list <- lapply(selected_matches, function(mm) {
      if (is.null(mm$rotation) || !nrow(mm$rotation)) return(NULL)
      mm$rotation
    })
    rotation_list <- Filter(Negate(is.null), rotation_list)
    validate(need(length(rotation_list) > 0, "No rotation details are available."))
    all_rotation <- bind_rows(rotation_list)
    validate(need("Rotation" %in% names(all_rotation), "Rotation column is missing."))

    # Group headings mirror the Rotation Details worksheet.
    groups <- list(
      "Game Phase" = c("SO%", "PS%"),
      "Serve" = c("Ace", "Serve Err", "Serve Att", "Serve In %"),
      "Reception" = c("SR Perf", "SR Good", "SR Poor", "SR Err", "SR Att"),
      "Attack" = c("Kill", "A Err", "Att", "H %")
    )
    ordered_cols <- c("Rotation", unlist(groups, use.names = FALSE))
    cols <- ordered_cols[ordered_cols %in% names(all_rotation)]
    validate(need("Rotation" %in% cols, "Rotation column is missing."))

    all_rotation <- all_rotation[, cols, drop = FALSE]
    for (nm in setdiff(names(all_rotation), "Rotation")) {
      all_rotation[[nm]] <- suppressWarnings(as.numeric(all_rotation[[nm]]))
    }

    # Across Last 5 / Last 10 / Full Season, combine each rotation across matches:
    # average rate metrics, sum count metrics. Then calculate the AVG row below.
    average_metrics <- c("SO%", "PS%", "Serve In %")
    metric_cols <- setdiff(names(all_rotation), "Rotation")
    d <- all_rotation |>
      filter(grepl("^Ro[1-6]$", as.character(Rotation))) |>
      group_by(Rotation) |>
      summarise(across(all_of(metric_cols), ~ {
        vals <- .x[is.finite(.x)]
        if (!length(vals)) NA_real_
        else if (dplyr::cur_column() %in% average_metrics) mean(vals)
        else sum(vals)
      }), .groups = "drop") |>
      arrange(match(Rotation, paste0("Ro", 1:6)))

    # AVG row: average the three rate metrics requested; sum all other metrics.
    metric_cols <- setdiff(names(d), "Rotation")
    avg <- as.list(rep(NA_real_, length(names(d))))
    names(avg) <- names(d)
    avg$Rotation <- "AVG"
    for (nm in metric_cols) {
      x <- d[[nm]]
      x <- x[is.finite(x)]
      if (!length(x)) {
        avg[[nm]] <- NA_real_
      } else if (nm %in% average_metrics) {
        avg[[nm]] <- mean(x)
      } else {
        avg[[nm]] <- sum(x)
      }
    }
    display_data <- bind_rows(d, as.data.frame(avg, check.names = FALSE))

    # Colour values relative to the six rotations; no Top 50 comparison.
    lower_is_better <- c("Serve Err", "SR Poor", "SR Err", "A Err")
    cell_colour <- function(value, values, metric) {
      valid <- values[is.finite(values)]
      if (!is.finite(value) || length(valid) < 2 || diff(range(valid)) == 0) {
        return("#f3f6f8")
      }
      score <- (value - min(valid)) / (max(valid) - min(valid))
      if (metric %in% lower_is_better) score <- 1 - score
      palette <- grDevices::colorRampPalette(c("#e88978", "#fff0b3", "#59b78b"))(101)
      palette[max(1, min(101, round(score * 100) + 1))]
    }

    format_cell <- function(value, metric) {
      if (!is.finite(value)) return("—")
      if (metric %in% c("SO%", "PS%", "Serve In %")) return(sprintf("%.1f%%", value * 100))
      if (metric == "H %") return(sprintf("%.3f", value))
      if (metric %in% c("SR Perf", "Ace", "Serve Err", "Serve Att", "SR Good", "SR Poor", "SR Err", "SR Att", "Kill", "A Err", "Att")) {
        return(as.character(round(value)))
      }
      sprintf("%.1f", value)
    }

    # Find the final column in each phase group for thick separators.
    phase_end_cols <- character()
    for (group_name in names(groups)) {
      present <- intersect(groups[[group_name]], cols)
      if (length(present)) phase_end_cols <- c(phase_end_cols, tail(present, 1))
    }

    group_header <- list(tags$th(rowspan = 2, "Rotation"))
    for (group_name in names(groups)) {
      present <- intersect(groups[[group_name]], cols)
      if (length(present)) {
        group_header[[length(group_header) + 1]] <- tags$th(
          colspan = length(present),
          class = if (tail(present, 1) %in% phase_end_cols) "phase-end" else NULL,
          group_name
        )
      }
    }
    sub_header <- lapply(cols[-1], function(nm) {
      tags$th(class = if (nm %in% phase_end_cols) "phase-end" else NULL, nm)
    })

    body_rows <- lapply(seq_len(nrow(display_data)), function(i) {
      is_avg <- identical(as.character(display_data$Rotation[i]), "AVG")
      row_cells <- lapply(cols, function(nm) {
        extra_class <- if (nm %in% phase_end_cols) "phase-end" else NULL
        if (nm == "Rotation") {
          tags$td(class = extra_class, style = "text-align:left;font-weight:800;",
                  as.character(display_data$Rotation[i]))
        } else {
          value <- display_data[[nm]][i]
          # The AVG row is neutral; colour only the six rotation rows.
          bg <- if (is_avg) "#f4f6f8" else cell_colour(value, d[[nm]], nm)
          tags$td(
            class = extra_class,
            style = paste0("background-color:", bg, ";font-weight:800;font-variant-numeric:tabular-nums;"),
            format_cell(value, nm)
          )
        }
      })
      tags$tr(class = if (is_avg) "average-row" else NULL, row_cells)
    })

    div(
      class = "rotation-heatmap-wrap",
      tags$table(
        class = "rotation-heatmap",
        tags$thead(tags$tr(group_header), tags$tr(sub_header)),
        tags$tbody(body_rows)
      )
    )
  })

  # ---------------- SETTING STATS ----------------

  # Aggregate Setting Stats across the selected match window.
  # Count fields are summed; percentage/efficiency fields are recalculated
  # from the summed counts so Last 5, Last 10 and Full Season are meaningful.
  aggregate_setting_totals <- function(selected_matches) {
    pieces <- lapply(selected_matches, function(mm) {
      d <- mm$rotation_setting
      if (is.null(d) || !nrow(d)) return(NULL)
      d <- as.data.frame(d, check.names = FALSE)
      d <- d[grepl("^Ro[1-6]$", trimws(as.character(d$Rotation))), , drop = FALSE]
      if (!nrow(d)) return(NULL)
      d$Sets <- suppressWarnings(as.numeric(as.character(d$Sets)))
      d$Kills <- suppressWarnings(as.numeric(as.character(d$Kills)))
      d
    })
    pieces <- Filter(Negate(is.null), pieces)
    if (!length(pieces)) return(data.frame(Rotation=character(), Sets=numeric(), Kills=numeric()))
    combined <- bind_rows(pieces) |>
      group_by(Rotation) |>
      summarise(Sets=sum(Sets, na.rm=TRUE), Kills=sum(Kills, na.rm=TRUE), .groups="drop")
    order_rot <- paste0("Ro", 1:6)
    combined <- combined[match(order_rot[order_rot %in% combined$Rotation], combined$Rotation), , drop=FALSE]
    bind_rows(combined, data.frame(Rotation="Total", Sets=sum(combined$Sets, na.rm=TRUE), Kills=sum(combined$Kills, na.rm=TRUE)))
  }

  aggregate_setter_block <- function(selected_matches, block_name) {
    pieces <- lapply(selected_matches, function(mm) {
      d <- mm[[block_name]]
      if (is.null(d) || !nrow(d)) return(NULL)
      as.data.frame(d, check.names=FALSE)
    })
    pieces <- Filter(Negate(is.null), pieces)
    if (!length(pieces)) return(data.frame())

    first_col <- names(pieces[[1]])[1]
    count_cols <- intersect(c("Sets", "Kills", "Errors"), names(pieces[[1]]))
    combined <- bind_rows(pieces) |>
      mutate(.who = trimws(as.character(.data[[first_col]]))) |>
      group_by(.who) |>
      summarise(across(all_of(count_cols), ~sum(suppressWarnings(as.numeric(as.character(.x))), na.rm=TRUE)), .groups="drop")
    names(combined)[names(combined)==".who"] <- first_col

    # Preserve the share values recorded in each workbook. For multi-match
    # periods, average the workbook's share column by role instead of
    # recalculating it from combined set counts.
    share_cols <- intersect(
      c("Share of FB Sets", "Share of Trans. Sets"),
      names(pieces[[1]])
    )
    if (length(share_cols)) {
      share_data <- bind_rows(pieces) |>
        mutate(.who = trimws(as.character(.data[[first_col]]))) |>
        group_by(.who) |>
        summarise(
          across(
            all_of(share_cols),
            ~{
              v <- suppressWarnings(as.numeric(as.character(.x)))
              if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
            }
          ),
          .groups = "drop"
        )
      names(share_data)[names(share_data) == ".who"] <- first_col
      combined <- combined |>
        select(-any_of(share_cols)) |>
        left_join(share_data, by = first_col)
    }
    if (all(c("Kills", "Sets") %in% names(combined)) && "K%" %in% names(pieces[[1]])) {
      combined[["K%"]] <- ifelse(combined$Sets > 0, combined$Kills / combined$Sets, NA_real_)
    }
    if (all(c("Errors", "Sets") %in% names(combined)) && "E%" %in% names(pieces[[1]])) {
      combined[["E%"]] <- ifelse(combined$Sets > 0, combined$Errors / combined$Sets, NA_real_)
    }
    eff_col <- intersect(c("EFF.", "Eff.", "Eff", "Efficiency"), names(pieces[[1]]))
    if (length(eff_col) && all(c("Kills", "Errors", "Sets") %in% names(combined))) {
      combined[[eff_col[1]]] <- ifelse(combined$Sets > 0, (combined$Kills - combined$Errors) / combined$Sets, NA_real_)
    }

    # Keep the workbook's role order: Setter, Lib, Non-setter.
    role_order <- c("Setter", "Lib", "Non-setter")
    combined <- combined[order(match(combined[[first_col]], role_order), na.last=TRUE), , drop=FALSE]
    combined
  }

  output$rotation_setting_table <- renderTable({
    selected_matches <- period_matches()
    req(length(selected_matches) > 0)
    d <- aggregate_setting_totals(selected_matches)
    validate(need(nrow(d) > 0, "No rotation setting totals found."))
    d
  }, striped=TRUE, bordered=FALSE, spacing="s", digits=0, width="100%", rownames=FALSE)

  format_setting_table <- function(d) {
    if (is.null(d) || !nrow(d)) return(d)
    d <- as.data.frame(d, check.names=FALSE)
    for (nm in names(d)) {
      values <- suppressWarnings(as.numeric(as.character(d[[nm]])))
      if (!all(is.na(values))) {
        nml <- tolower(trimws(nm))
        if (grepl("share", nml) || nml %in% c("k%", "e%") || grepl("percent|pct|%", nml)) {
          d[[nm]] <- ifelse(is.na(values), "–", paste0(sprintf("%.1f", values * ifelse(max(abs(values), na.rm=TRUE) <= 1, 100, 1)), "%"))
        } else if (grepl("eff", nml)) {
          d[[nm]] <- ifelse(is.na(values), "–", sprintf("%.3f", values))
        } else if (grepl("sets|kills|errors", nml)) {
          d[[nm]] <- ifelse(is.na(values), "–", sprintf("%.0f", values))
        }
      }
    }
    d
  }

  output$fb_table <- renderTable({
    selected_matches <- period_matches()
    req(length(selected_matches) > 0)
    format_setting_table(aggregate_setter_block(selected_matches, "fb_sets"))
  }, striped=TRUE, bordered=FALSE, spacing="s", width="100%", rownames=FALSE)

  output$trans_table <- renderTable({
    selected_matches <- period_matches()
    req(length(selected_matches) > 0)
    format_setting_table(aggregate_setter_block(selected_matches, "trans_sets"))
  }, striped=TRUE, bordered=FALSE, spacing="s", width="100%", rownames=FALSE)

  # ---------------- PASSING BY CONTACT POINT ----------------

  # Use the player silhouette already stored with the app's static assets.
  # If your repository uses a different filename, update this single value.
  passing_player_image <- "Volleyball Player in Defensive Stance"

  # Period logic: counts (Total Pass, Good Pass, Aced) are summed across selected matches;
  # GP % and Aced % are calculated from those period totals (weighted rates, not averages of match rates).
  # Period aggregation: counts are summed across selected matches; percentages are calculated from those totals (weighted rates, not averages of match percentages).
  passing_period_data <- reactive({
    selected_matches <- period_matches()
    req(length(selected_matches) > 0)

    rows <- lapply(selected_matches, function(match_data) {
      d <- match_data$passing
      if (is.null(d) || !nrow(d)) return(NULL)
      required <- c("Player No.", "Player Name", "Contact Point", "Total Pass", "Good Pass", "Aced")
      if (!all(required %in% names(d))) return(NULL)
      d
    })
    d <- dplyr::bind_rows(rows)
    validate(need(nrow(d) > 0, "No Passing by Contact Point data found for this period."))
    if (!"Position" %in% names(d)) d$Position <- ""

    d <- d |>
      mutate(
        `Player No.` = as.character(`Player No.`),
        `Player Name` = trimws(as.character(`Player Name`)),
        Position = as.character(Position),
        `Contact Point` = trimws(as.character(`Contact Point`)),
        `Total Pass` = suppressWarnings(as.numeric(`Total Pass`)),
        `Good Pass` = suppressWarnings(as.numeric(`Good Pass`)),
        Aced = suppressWarnings(as.numeric(Aced))
      ) |>
      mutate(
        `Total Pass` = tidyr::replace_na(`Total Pass`, 0),
        `Good Pass` = tidyr::replace_na(`Good Pass`, 0),
        Aced = tidyr::replace_na(Aced, 0)
      ) |>
      group_by(`Player No.`, `Player Name`, Position, `Contact Point`) |>
      summarise(
        `Total Pass` = sum(`Total Pass`, na.rm = TRUE),
        `Good Pass` = sum(`Good Pass`, na.rm = TRUE),
        Aced = sum(Aced, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        `GP %` = ifelse(`Total Pass` > 0, `Good Pass` / `Total Pass`, NA_real_),
        `Aced %` = ifelse(`Total Pass` > 0, Aced / `Total Pass`, NA_real_)
      )
    d
  })

  output$passing_cards_ui <- renderUI({
    d <- passing_period_data()
    validate(need(nrow(d) > 0, "No passing data to display."))

    players <- d |>
      distinct(`Player No.`, `Player Name`, Position) |>
      arrange(suppressWarnings(as.numeric(`Player No.`)), `Player Name`)

    point_ui <- function(player_rows, point_name, class_name) {
      z <- player_rows |> filter(`Contact Point` == point_name)
      if (!nrow(z) || is.na(z$`Total Pass`[1]) || z$`Total Pass`[1] <= 0) {
        return(div(class = paste("passing-point", class_name),
          div(class = "passing-point-title", point_name),
          div(class = "passing-point-empty", "none")))
      }
      total <- z$`Total Pass`[1]
      good <- z$`Good Pass`[1]
      aced <- z$Aced[1]
      gp <- z$`GP %`[1]
      ap <- z$`Aced %`[1]
      div(class = paste("passing-point", class_name),
        div(class = "passing-point-title", point_name),
        div(class = "passing-point-count", if (is.finite(gp)) sprintf("%.0f%%", gp * 100) else "—"),
        div(class = "passing-point-sub",
          tags$strong(paste0(format(round(total), nsmall = 0, trim = TRUE), " attempts"))),
        div(class = "passing-point-sub",
          paste0(aced, " aced", ifelse(is.finite(ap), paste0(" · ", round(ap * 100), "%"), ""))))
    }

    cards <- lapply(seq_len(nrow(players)), function(i) {
      pl <- players[i, ]
      rows <- d |>
        filter(`Player No.` == pl$`Player No.`, `Player Name` == pl$`Player Name`)
      get_count <- function(point) {
        z <- rows |> filter(`Contact Point` == point)
        if (!nrow(z)) 0 else z$`Total Pass`[1]
      }
      total_passes <- sum(rows$`Total Pass`, na.rm = TRUE)
      total_good <- sum(rows$`Good Pass`, na.rm = TRUE)
      total_aced <- sum(rows$Aced, na.rm = TRUE)
      div(class = "passing-player-card",
        div(class = "passing-player-header", style = "background-color:#123f68 !important; color:#ffffff !important;",
          div(class = "passing-player-name", paste0("#", pl$`Player No.`, " ", pl$`Player Name`)),
          div(class = "passing-player-meta", ifelse(is.na(pl$Position) || !nzchar(pl$Position), "", pl$Position))
        ),
        div(class = "passing-player-caption", "Contact-point outcomes · player facing you"),
        div(class = "passing-court-stage",
          point_ui(rows, "Overhand", "passing-overhand"),
          point_ui(rows, "His Right", "passing-left"),
          div(class = "passing-player-figure",
            tags$img(src = URLencode(passing_player_image, reserved = TRUE), alt = "Volleyball player silhouette"),
            point_ui(rows, "On body", "passing-body-point")
          ),
          point_ui(rows, "His Left", "passing-right"),
          point_ui(rows, "Low", "passing-low")
        ),
        div(class = "passing-player-footer",
          span(HTML(paste0("<strong>", format(round(total_passes), trim=TRUE), "</strong> passes"))),
          span(HTML(paste0("<strong>", if (total_passes > 0) sprintf("%.1f%%", total_good / total_passes * 100) else "—", "</strong> good"))),
          span(HTML(paste0("<strong>", format(round(total_aced), trim=TRUE), "</strong> aced")))
        )
      )
    })

    div(class = "passing-player-grid", cards)
  })

}

shinyApp(ui, server)
