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


# ---------------- RED ZONE PARSER ----------------
# The Match Information sheet contains a variable-length point sequence for each set.
# Find set markers dynamically; never assume a fixed number of rows per set.
parse_red_zone <- function(info) {
  empty_points <- tibble(
    point_order = integer(),
    huskies_event = character(),
    score = character(),
    opponent_event = character(),
    error_type = character()
  )
  empty <- list(description = "After Huskies reach 20", sets = list())
  if (is.null(info) || !nrow(info) || ncol(info) < 3) return(empty)

  cell <- function(r, c) {
    if (r < 1 || r > nrow(info) || c < 1 || c > ncol(info)) return("")
    x <- info[[c]][r]
    if (!length(x) || is.na(x)) return("")
    trimws(as.character(x))
  }

  # The workbook stores the section label in A10 and its description in A11.
  red_rows <- which(vapply(seq_len(nrow(info)), function(r) {
    grepl("^red\\s*zone$", cell(r, 1), ignore.case = TRUE)
  }, logical(1)))
  if (!length(red_rows)) return(empty)

  red_row <- red_rows[1]
  description <- cell(red_row + 1, 1)
  if (!nzchar(description)) description <- "After Huskies reach 20"

  # Only recognize set markers below the Red Zone heading. This avoids
  # accidentally picking up any set labels elsewhere in Match Information.
  set_rows <- which(vapply(seq_len(nrow(info)), function(r) {
    r > red_row && grepl("^set\\s*\\d+", cell(r, 1), ignore.case = TRUE)
  }, logical(1)))
  if (!length(set_rows)) return(list(description = description, sets = list()))

  set_list <- vector("list", length(set_rows))
  for (j in seq_along(set_rows)) {
    start_row <- set_rows[j]
    end_row <- if (j < length(set_rows)) set_rows[j + 1] - 1L else nrow(info)
    set_label <- cell(start_row, 1)
    set_number <- suppressWarnings(as.integer(sub(
      "(?i)^set\\s*(\\d+).*", "\\1", set_label, perl = TRUE
    )))
    opening_score <- cell(start_row, 2)

    first_data <- start_row + 1L
    # Skip the team-label row (Huskies / BC), wherever it occurs.
    if (first_data <= end_row &&
        (grepl("^huskies$", cell(first_data, 2), ignore.case = TRUE) ||
         grepl("^bc$", cell(first_data, 3), ignore.case = TRUE))) {
      first_data <- first_data + 1L
    }

    point_rows <- list()
    if (first_data <= end_row) {
      for (r in seq.int(first_data, end_row)) {
        huskies_cell <- cell(r, 2)
        opponent_cell <- cell(r, 3)
        if (!nzchar(huskies_cell) && !nzchar(opponent_cell)) next
        if (grepl("^huskies$", huskies_cell, ignore.case = TRUE) ||
            grepl("^bc$", opponent_cell, ignore.case = TRUE)) next

        huskies_is_score <- grepl("^\\d+\\s*[-–]\\s*\\d+$", huskies_cell)
        opponent_is_score <- grepl("^\\d+\\s*[-–]\\s*\\d+$", opponent_cell)
        score <- if (huskies_is_score) huskies_cell else if (opponent_is_score) opponent_cell else ""
        huskies_event <- if (huskies_is_score) "" else huskies_cell
        opponent_event <- if (opponent_is_score) "" else opponent_cell

        if (!nzchar(score) && !nzchar(huskies_event) && !nzchar(opponent_event)) next

        event_text <- tolower(trimws(huskies_event))
        error_type <- if (grepl("serve\\s*error", event_text)) {
          "Serve Error"
        } else if (grepl("attack\\s*error", event_text)) {
          "Attack Error"
        } else if (grepl("negative\\s*block\\s*\\(\\s*kill\\s*\\)|block\\s*error", event_text)) {
          "Block Error"
        } else {
          ""
        }

        point_rows[[length(point_rows) + 1L]] <- tibble(
          point_order = length(point_rows) + 1L,
          huskies_event = huskies_event,
          score = score,
          opponent_event = opponent_event,
          error_type = error_type
        )
      }
    }

    point_df <- if (length(point_rows)) bind_rows(point_rows) else empty_points
    set_list[[j]] <- list(
      set_number = if (is.na(set_number)) j else set_number,
      set_label = set_label,
      opening_score = opening_score,
      points = point_df,
      serve_errors = as.numeric(sum(point_df$error_type == "Serve Error", na.rm = TRUE)),
      attack_errors = as.numeric(sum(point_df$error_type == "Attack Error", na.rm = TRUE)),
      block_errors = as.numeric(sum(point_df$error_type == "Block Error", na.rm = TRUE))
    )
  }

  list(description = description, sets = set_list)
}

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
    red_zone <- parse_red_zone(info)

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
      red_zone = red_zone,
      info = info,
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
    !grepl("^~\\$", basename(files)) &
      !grepl("-Athlete\\.xlsx$", basename(files), ignore.case = TRUE)
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

    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('printReport', function(message) {
          window.print();
        });
      });
    ")),

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
      /* Full-width searchable athlete roster and profile */
      .athlete-roster-wrap, .athlete-profile-wrap { width:100%; max-width:none; box-sizing:border-box; }
      .athlete-roster-table { width:100%; border-collapse:collapse; background:#fff; }
      .athlete-roster-table th { text-align:left; color:#17324d; font-weight:800; border-bottom:2px solid #17324d; padding:12px 14px; }
      .athlete-roster-table td { padding:10px 14px; border-bottom:1px solid #e7edf2; vertical-align:middle; }
      .athlete-roster-table tr:nth-child(even) td { background:#f2f5f7; }
      .athlete-open-link { border:0; background:transparent; color:#2F75B5; font-weight:800; padding:0; text-decoration:none; }
      .athlete-open-link:hover { text-decoration:underline; color:#17466B; }
      .athlete-profile-heading { display:flex; justify-content:space-between; align-items:center; gap:16px; flex-wrap:wrap; margin-bottom:16px; }
      .athlete-profile-title { margin:0; color:#17324d; }
      .athlete-profile-subtitle { color:#6b8194; margin-top:4px; }
      .athlete-profile-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:16px; }
      .athlete-profile-card { background:#fff; border:1px solid #d8e2ec; border-radius:12px; padding:16px; min-width:0; box-shadow:0 3px 10px rgba(18,45,75,.04); }
      .athlete-profile-card h3 { margin:0 0 12px; font-size:18px; color:#17324d; }
      .athlete-profile-card table { width:100% !important; }
      .athlete-profile-card .table-responsive { width:100%; }
      #athlete_attack_table table tbody tr:last-child td,
      #athlete_setting_table table tbody tr:last-child td,
      #athlete_dump_table table tbody tr:last-child td {
        font-weight: 800 !important;
        background: #e7eef5 !important;
        color: #17324d !important;
        border-top: 2px solid #17466B !important;
      }
      .athlete-passing-profile-card { width:100%; max-width:760px; margin:0 auto; }
      .athlete-profile-wide { grid-column:1 / -1; }
      @media(max-width:800px) { .athlete-profile-grid { grid-template-columns:1fr; } .athlete-profile-wide { grid-column:auto; } }

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
      .passing-point-count { font-size:26px; font-weight:900; line-height:1; color:#263746; display:flex; align-items:baseline; justify-content:center; gap:4px; }
      .passing-good-label { font-size:10px; font-weight:600; line-height:1; color:#607488; }
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

      /* Keep both navigation controls together at the far right of the main header. */
      .dashboard-header {
        display: grid !important;
        grid-template-columns: minmax(0, 1fr) auto auto auto !important;
        align-items: center !important;
        column-gap: 22px !important;
      }
      .athletes-link, .reports-link {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 2px;
        margin: 0 !important;
        min-width: 64px;
        justify-self: end;
      }
      .reports-link { grid-column: 4; }
      .athletes-link { grid-column: 3; }
      .reports-icon-button { min-width: 44px; }
      @media(max-width:700px) {
        .dashboard-header {
          grid-template-columns: minmax(0, 1fr) auto auto !important;
          column-gap: 10px !important;
        }
        .brand-area { grid-column: 1 / -1; }
        .match-summary { grid-column: 1; }
        .athletes-link { grid-column: 2; }
        .reports-link { grid-column: 3; }
      }
      .reports-link { display:flex; flex-direction:column; align-items:center; justify-content:center; gap:2px; margin-left:0; min-width:64px; }
      .reports-icon-button { border:0; background:transparent; color:#17324d; padding:5px 10px 0; box-shadow:none !important; }
      .reports-icon-button:hover, .reports-icon-button:focus { color:#2F75B5 !important; background:transparent !important; }
      .reports-link .athletes-label { font-size:10px; letter-spacing:.08em; font-weight:800; color:#7a8794; }
      .report-full-section { margin:26px 0; }
      .report-data-table { width:100%; border-collapse:collapse; background:#fff; }
      .report-data-table th,.report-data-table td { padding:9px 11px; border-bottom:1px solid #dce5ed; text-align:left; vertical-align:middle; }
      .report-data-table th { color:#52677b; border-bottom:2px solid #17324d; font-weight:800; }
      .report-data-table tbody tr:nth-child(even) td { background:#f0f3f6; }
      .report-title-block { padding:8px 0 16px; border-bottom:1px solid #d8e2ec; margin-bottom:20px; }
      .report-title-block h1 { margin:0 0 5px; }
      .report-title-block h3 { margin:0 0 6px; color:#718397; font-weight:500; }
      .redzone-good-row td { background:#e8f7ef !important; color:#14784f; font-weight:700; }
      .report-full-section .analytics-section { margin-top:0; }
      @media print { .report-full-section { break-inside:auto; } .report-data-table tr,.skill-card,.passing-player-card { break-inside:avoid; } }
      .reports-nav-button { border:0; background:transparent; color:#17324d; font-weight:800; }
      .reports-nav-button:hover, .reports-nav-button:focus { color:#2F75B5 !important; }
      .redzone-match-card { background:#fff; border:1px solid #d8e2ec; border-radius:12px; padding:16px; margin:14px 0; break-inside:avoid; }
      .redzone-set-card { border:1px solid #e1e8ef; border-radius:9px; padding:12px; margin:10px 0; }
      .redzone-summary { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:10px; margin:12px 0 18px; }
      .redzone-summary-card { background:#f1f5f9; border:1px solid #d8e2ec; border-radius:9px; padding:12px; }
      .redzone-summary-card strong { display:block; font-size:23px; color:#17324d; }
      .redzone-error-row td { background:#fff0ee !important; color:#8f2e25; font-weight:700; }
      .redzone-table { width:100%; border-collapse:collapse; }
      .redzone-table th, .redzone-table td { padding:7px 9px; border-bottom:1px solid #e5ebf0; text-align:left; }
      .redzone-table th { color:#52677b; border-bottom:2px solid #17324d; }
      .reports-page-heading { margin-bottom:18px; }
      .report-match-card { display:flex; justify-content:space-between; align-items:center; gap:18px; background:#fff; border:1px solid #d8e2ec; border-radius:14px; padding:18px 22px; margin:12px 0; box-shadow:0 2px 8px rgba(23,50,77,.04); }
      .report-match-copy h3 { margin:3px 0 5px; color:#17324d; }
      .report-match-date { color:#6c8296; font-size:12px; font-weight:800; letter-spacing:.06em; text-transform:uppercase; }
      .report-match-result { margin:0; color:#60788d; }
      .report-match-open { border:0; border-radius:7px; background:#0e315e; color:#fff; font-weight:800; padding:11px 15px; white-space:nowrap; }
      .report-match-open:hover { background:#245786; }
      .report-back-row { display:flex; justify-content:space-between; gap:10px; margin-bottom:16px; }
      .report-performance-panel { border:1px solid #d8e2ec; border-radius:12px; padding:20px; margin:18px 0; }
      .report-performance-panel .metric-group { margin:14px 0; }
      .report-performance-panel .metric-group-title { font-size:12px; letter-spacing:.08em; font-weight:900; border-bottom:3px solid #344454; padding-bottom:8px; margin-bottom:8px; }
      .report-performance-panel .metric-row { display:grid; grid-template-columns:minmax(180px, .8fr) 80px minmax(260px, 1.5fr); gap:18px; align-items:center; padding:12px 0; border-bottom:1px solid #d8e2ec; }
      .report-performance-panel .metric-track { background:#f2e8e1; height:15px; border-radius:10px; overflow:hidden; }
      .report-performance-panel .metric-fill { height:100%; border-radius:10px; }
      .report-performance-panel .metric-good { background:#159b68; }
      .report-performance-panel .metric-bad { background:#e5483b; }
      .report-performance-panel .metric-value { color:#159b68; font-size:20px; font-weight:900; text-align:right; }
      @media(max-width:700px) { .report-match-card { align-items:flex-start; flex-direction:column; } .report-performance-panel .metric-row { grid-template-columns:1fr 70px; gap:8px; } .report-performance-panel .metric-bar-area { grid-column:1/-1; } }
      .report-controls { display:flex; flex-wrap:wrap; gap:16px; align-items:end; margin:16px 0; }
      .report-controls .form-group { min-width:230px; }
      @media(max-width:700px) { .redzone-summary { grid-template-columns:repeat(2,minmax(0,1fr)); } }
      @media print {
        body * { visibility:hidden !important; }
        #printable-report, #printable-report * { visibility:visible !important; }
        #printable-report { position:absolute !important; left:0 !important; top:0 !important; width:100% !important; padding:0 !important; margin:0 !important; box-shadow:none !important; }
        .no-print { display:none !important; }
        .redzone-match-card, .redzone-set-card { break-inside:avoid; box-shadow:none !important; }
        @page { margin:12mm; }
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

    ,
    div(
      class = "reports-link",
      actionButton(
        "reports_nav",
        label = NULL,
        icon = icon("file-text"),
        class = "reports-icon-button"
      ),
      div("REPORTS", class = "athletes-label")
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
  selected_athlete <- reactiveVal(NULL)
  show_reports <- reactiveVal(FALSE)
  selected_report_file <- reactiveVal(NULL)

  matches <- reactive({

    invalidateLater(60000, session)

    load_matches()
  })

  period_matches <- reactive({

    ms <- matches()

    if (!length(ms)) {
      return(list())
    }

    # When a report is opened, drive the SAME dashboard outputs from that
    # workbook, exactly as if it were the most recent match on the home page.
    if (isTRUE(show_reports()) && !is.null(selected_report_file())) {
      target <- Filter(function(x) identical(as.character(x$file), as.character(selected_report_file())), ms)
      if (length(target)) return(target[1])
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
          selected_athlete(NULL)
          show_reports(FALSE)
        }
      )
    })
  })

  observeEvent(
    input$athletes_nav,
    {
      if (show_athletes()) {
        show_athletes(FALSE)
        selected_athlete(NULL)
      } else {
        show_athletes(TRUE)
        selected_athlete(NULL)
        show_reports(FALSE)
      }
    }
  )

  observeEvent(input$reports_nav, {
    show_reports(!show_reports())
    show_athletes(FALSE)
    selected_athlete(NULL)
  })

  observeEvent(input$athlete_back, {
    selected_athlete(NULL)
  })

  # ---------------- PAGE CONTENT ----------------

  output$page_content <- renderUI({

    # Individual athlete profile takes priority while the Athletes area is open.
    if (show_athletes() && !is.null(selected_athlete())) {
      return(uiOutput("athlete_profile_ui"))
    }

    # Full-width searchable roster
    if (show_athletes()) {
      return(
        tagList(
          div(class = "athlete-roster-wrap",
            h2("Athletes"),
            p(class = "section-intro", "Season roster. Public-facing names use first names only. Select a player to open their individual performance profile."),
            textInput("athlete_search", "Search athletes", placeholder = "First name, jersey number, or position...", width = "100%"),
            uiOutput("roster_table_ui")
          )
        )
      )
    }

    if (show_reports() && is.null(selected_report_file())) {
      return(tagList(
        div(class = "reports-page-heading",
          h2("Match Reports"),
          p(class = "section-intro", "Select a match to open its full dashboard-style report. Matches are listed newest first.")
        ),
        uiOutput("report_matches_ui")
      ))
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

    report_mode <- isTRUE(show_reports()) && !is.null(selected_report_file())
    report_toolbar <- if (report_mode) tagList(
      div(class = "no-print report-back-row",
        actionButton("back_to_reports", "All Matches", icon = icon("arrow-left"), class = "btn-default"),
        actionButton("print_report_btn", "Print / Save as PDF", icon = icon("print"), class = "btn-primary")
      )
    ) else NULL

    tagList(
      report_toolbar,
      div(id = if (report_mode) "printable-report" else NULL,
      tagList(
      if (report_mode) div(class="report-title-block",
        h1("Match Report"),
        h3(paste(if (is.na(m$date)) "Date unknown" else format(m$date, "%B %d, %Y"), "·", m$opponent)),
        div(class="report-match-result", ifelse(is.na(m$result), "Result not recorded", m$result))
      ) else NULL,
      div(
        class = "analytics-section",
        div(
          class = "section-heading-row",
          div(
            h2(if (report_mode) "Most Recent Match" else selected_period()),
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
      ),

      # 7. Red Zone
      div(
        class = "analytics-section",
        section_title("RED ZONE", "Point-by-point pressure sequence after the Huskies reach 20."),
        uiOutput("redzone_dashboard_ui")
      )
      )
      )
    )
  })

  # ---------------- RED ZONE AND REPORTS ----------------

  redzone_period_data <- reactive({
    ms <- period_matches()
    if (!length(ms)) return(list())
    lapply(ms, function(m) {
      rz <- m$red_zone
      if (is.null(rz)) rz <- list(description = "After Huskies reach 20", sets = list())
      list(date = m$date, opponent = m$opponent, file = m$file,
           result = m$result, description = rz$description, sets = rz$sets)
    })
  })

  output$redzone_dashboard_ui <- renderUI({
    records <- redzone_period_data()
    all_sets <- unlist(lapply(records, function(m) m$sets), recursive = FALSE)
    serve_total <- sum(vapply(all_sets, function(z) z$serve_errors %||% 0, numeric(1)), na.rm=TRUE)
    attack_total <- sum(vapply(all_sets, function(z) z$attack_errors %||% 0, numeric(1)), na.rm=TRUE)
    block_total <- sum(vapply(all_sets, function(z) z$block_errors %||% 0, numeric(1)), na.rm=TRUE)
    total <- serve_total + attack_total + block_total

    summary_ui <- div(class="redzone-summary",
      div(class="redzone-summary-card", "Serve errors", tags$strong(serve_total)),
      div(class="redzone-summary-card", "Attack errors", tags$strong(attack_total)),
      div(class="redzone-summary-card", "Block errors", tags$strong(block_total)),
      div(class="redzone-summary-card", "Total errors", tags$strong(total))
    )

    match_ui <- lapply(records, function(m) {
      date_text <- if (is.na(m$date)) "Date unknown" else format(m$date, "%b %d")
      sets_ui <- lapply(m$sets, function(st) {
        point_table <- if (nrow(st$points)) {
          tags$table(class="redzone-table",
            tags$thead(tags$tr(tags$th("#"), tags$th("Huskies"), tags$th("Score"), tags$th(m$opponent))),
            tags$tbody(lapply(seq_len(nrow(st$points)), function(i) {
              p <- st$points[i,]
              event <- tolower(trimws(as.character(p$huskies_event)))
              is_error <- nzchar(trimws(as.character(p$error_type))) || grepl("error", event, ignore.case=TRUE)
              is_good <- grepl("fbk|d-s-k|(^|[^a-z])ace([^a-z]|$)|block|(^|[^a-z])k([^a-z]|$)|kill", event, ignore.case=TRUE)
              cls <- if (is_error) "redzone-error-row" else if (is_good) "redzone-good-row" else ""
              tags$tr(class=cls,
                tags$td(p$point_order),
                tags$td(if (nzchar(p$huskies_event)) p$huskies_event else ""),
                tags$td(p$score),
                tags$td(if (nzchar(p$opponent_event)) p$opponent_event else "")
              )
            }))
          )
        } else {
          p(class="section-intro", "No point-by-point Red Zone entries were found for this set in the workbook.")
        }
        div(class="redzone-set-card",
          h4(paste0("Set ", st$set_number, if (nzchar(st$opening_score)) paste0(" · ", st$opening_score) else "")),
          point_table,
          p(class="section-intro", paste0("Errors: Serve ", st$serve_errors, " · Attack ", st$attack_errors, " · Block ", st$block_errors))
        )
      })
      div(class="redzone-match-card",
        h3(paste0(date_text, " · ", m$opponent)),
        if (!length(m$sets)) p(class="section-intro", "This workbook does not contain Red Zone set sequences yet.") else tagList(sets_ui)
      )
    })
    tagList(summary_ui, if (length(match_ui)) tagList(match_ui) else p("No match data available for this period."))
  })

  `%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x[1])) y else x

  output$report_matches_ui <- renderUI({
    ms <- matches()
    if (!length(ms)) return(div(class="placeholder-panel", "No match workbooks are available."))
    tagList(lapply(seq_along(ms), function(i) {
      m <- ms[[i]]
      d <- if (is.na(m$date)) "Date unknown" else format(m$date, "%b %d, %Y")
      result <- if (is.null(m$result) || !length(m$result) || is.na(m$result) || !nzchar(as.character(m$result))) "Result not recorded" else as.character(m$result)
      file_value <- as.character(m$file)
      onclick <- sprintf("Shiny.setInputValue('report_open_match', %s, {priority: 'event'});", jsonlite::toJSON(file_value, auto_unbox=TRUE))
      div(class="report-match-card",
        div(class="report-match-copy",
          div(class="report-match-date", d),
          h3(m$opponent),
          p(class="report-match-result", result)
        ),
        tags$button(type="button", class="report-match-open", onclick=onclick, "Open Report", icon("arrow-right"))
      )
    }))
  })

  observeEvent(input$back_to_reports, {
    selected_report_file(NULL)
  })

  observeEvent(input$report_open_match, {
    req(input$report_open_match)
    selected_report_file(as.character(input$report_open_match))
  }, ignoreInit = TRUE)

  observeEvent(input$print_report_btn, {
    session$sendCustomMessage("printReport", list())
  })

  output$print_report_ui <- renderUI({
    ms <- matches()
    if (!length(ms)) return(h3("No match data available."))
    target <- Filter(function(x) identical(as.character(x$file), as.character(selected_report_file() %||% ms[[1]]$file)), ms)
    if (!length(target)) target <- ms[1]
    m <- target[[1]]
    rz <- m$red_zone
    if (is.null(rz)) rz <- list(description="After Huskies reach 20", sets=list())
    d <- if (is.na(m$date)) "Date unknown" else format(m$date, "%b %d, %Y")

    esc_num <- function(x, digits=0) {
      z <- suppressWarnings(as.numeric(x))
      if (!length(z) || !is.finite(z[1])) return("—")
      if (digits == 0) format(round(z[1]), scientific=FALSE, trim=TRUE) else sprintf(paste0("%.",digits,"f"), z[1])
    }
    section <- function(title, subtitle, content) div(class="analytics-section report-full-section",
      h2(title), p(class="section-intro", subtitle), content)
    table_html <- function(headers, rows, class="report-data-table") {
      tags$div(class="table-responsive", tags$table(class=class,
        tags$thead(tags$tr(lapply(headers, tags$th))),
        tags$tbody(rows)))
    }
    metric_ui <- function() {
      k <- m$key
      if (is.null(k) || !nrow(k)) return(div(class="placeholder-panel", "No Key Metrics data found in this workbook."))
      groups <- vapply(seq_len(nrow(k)), function(i) {
        cat <- tolower(trimws(as.character(k$category[i]))); met <- tolower(trimws(as.character(k$metric[i])))
        if (grepl("game phase", cat)) "GAME PHASE" else if (grepl("passing", cat)) "PASSING" else if (grepl("defensive|block|dig|conversion", paste(cat,met))) "KEY DEFENSIVE" else if (grepl("offensive|attack", paste(cat,met))) "KEY OFFENSIVE" else toupper(as.character(k$category[i]))
      }, character(1))
      tagList(lapply(unique(groups), function(gname) {
        g <- k[groups == gname,,drop=FALSE]
        div(class="metric-group", div(class="metric-group-title",gname), lapply(seq_len(nrow(g)), function(i) {
          v <- suppressWarnings(as.numeric(g$value[i])); met <- as.character(g$metric[i]); eff <- grepl("eff",met,ignore.case=TRUE); pct <- grepl("%",met,fixed=TRUE)
          scaled <- if (eff) v else if (pct && abs(v)<=1) v*100 else v
          maxv <- if (eff) 1 else 100; w <- if (is.finite(scaled)) max(0,min(100,scaled/maxv*100)) else 0
          bad <- grepl("aced %|error|disruption|opponent",met,ignore.case=TRUE)
          label <- if (!is.finite(v)) "—" else if (eff) sprintf("%.3f",v) else paste0(sprintf("%.1f",scaled),"%")
          div(class="metric-row", div(class="metric-copy",div(class="metric-name",met),div(class="metric-description",ifelse(nzchar(as.character(g$description[i])),as.character(g$description[i])," "))), div(class="metric-value",label), div(class="metric-bar-area",div(class="metric-track",div(class=paste("metric-fill",if(bad) "metric-bad" else "metric-good"),style=paste0("width:",w,"%")))))
        }))
      }))
    }
    skill_ui <- function() {
      sk <- m$skills
      if (is.null(sk) || !nrow(sk)) return(div(class="placeholder-panel", "No By Skill data found in this workbook."))
      palette <- c("#159B68", "#2F75B5", "#E5483B", "#E6A52B", "#7D9DB8", "#A9C8E2")
      div(class="skill-cards-grid", lapply(unique(sk$skill), function(sn) {
        z <- sk[sk$skill == sn, , drop=FALSE]
        z$value <- suppressWarnings(as.numeric(z$value))
        z <- z[is.finite(z$value), , drop=FALSE]
        if (!nrow(z)) return(NULL)
        total <- z$value[1]
        pct <- grepl("%", z$metric[1], fixed=TRUE)
        center <- if (pct) paste0(round(ifelse(abs(total) <= 1, total*100, total)), "%") else esc_num(total)
        legend_rows <- lapply(seq_len(nrow(z)), function(j) {
          val_pct <- grepl("%", z$metric[j], fixed=TRUE)
          val_label <- if (val_pct) paste0(round(ifelse(abs(z$value[j]) <= 1, z$value[j]*100, z$value[j])), "%") else ""
          div(class="skill-legend-row",
            span(class="skill-legend-swatch", style=paste0("background:", palette[(j-1) %% length(palette)+1])),
            span(class="skill-legend-name", z$metric[j]),
            span(class="skill-legend-pct", val_label),
            span(class="skill-legend-count", esc_num(z$value[j]))
          )
        })
        div(class="skill-card",
          div(class="skill-card-header", span(toupper(sn)), span(toupper(z$metric[1]))),
          div(class="skill-card-body",
            div(class="skill-donut-wrap",
              div(class="skill-donut", style=paste0("background:conic-gradient(",palette[1]," 0deg 180deg,",palette[2]," 180deg 270deg,",palette[3]," 270deg 315deg,",palette[4]," 315deg 360deg)"),
                div(class="skill-donut-hole", div(class="skill-donut-value", center), div(class="skill-donut-label", toupper(z$metric[1])))
              )
            ),
            div(class="skill-legend", legend_rows)
          )
        )
      }))
    }
    attack_ui <- function() {
      a <- m$attack
      if (is.null(a)||!nrow(a)) return(div(class="placeholder-panel","No Attack Detail data found in this workbook."))
      reqcols <- c("Attack Type","Previous Play","Attempts","Kills","Error")
      if (!all(reqcols %in% names(a))) {
        raw_rows <- lapply(seq_len(nrow(a)), function(i) {
          tags$tr(lapply(names(a), function(nm) tags$td(as.character(a[[nm]][i]))))
        })
        return(table_html(names(a), raw_rows))
      }
      a[["Attack Type"]] <- trimws(as.character(a[["Attack Type"]])); a[["Previous Play"]] <- trimws(as.character(a[["Previous Play"]]))
      types <- unique(a[["Attack Type"]][!is.na(a[["Attack Type"]]) & nzchar(a[["Attack Type"]])])
      rows <- lapply(types,function(tp) { z<-a[a[["Attack Type"]]==tp,,drop=FALSE]; fb<-z[tolower(z[["Previous Play"]]) %in% c("fb","first ball","first-ball","firstball"),,drop=FALSE]; tr<-z[tolower(z[["Previous Play"]]) %in% c("transition","trans","transition attack"),,drop=FALSE]
        sums<-function(q,col) sum(suppressWarnings(as.numeric(q[[col]])),na.rm=TRUE); fa<-sums(fb,"Attempts"); fk<-sums(fb,"Kills"); fe<-sums(fb,"Error"); ta<-sums(tr,"Attempts"); tk<-sums(tr,"Kills"); te<-sums(tr,"Error"); at<-fa+ta; kk<-fk+tk; ee<-fe+te; eff<-function(k,e,n) if(n>0)(k-e)/n else NA_real_
        tags$tr(tags$td(tp),tags$td(esc_num(at)),tags$td(esc_num(kk)),tags$td(esc_num(ee)),tags$td(esc_num(eff(fk,fe,fa),3)),tags$td(esc_num(eff(tk,te,ta),3)),tags$td(esc_num(eff(kk,ee,at),3)))
      })
      table_html(c("Attack Type","Attempts","Kills","Errors","FB Eff.","Transition Eff.","Total Eff."),rows)
    }
    rotation_ui <- function() {
      z <- m$rotation
      if (is.null(z)||!nrow(z)) return(div(class="placeholder-panel","No Rotation Details data found in this workbook."))
      headers <- names(z); rows <- lapply(seq_len(nrow(z)),function(i) tags$tr(lapply(headers,function(nm) { v<-z[[nm]][i]; numeric_col<-is.numeric(z[[nm]]); pct<-grepl("%",nm,fixed=TRUE); val<-if(is.na(v)) "—" else if(numeric_col && pct) paste0(round(as.numeric(v),1),"%") else if(numeric_col) esc_num(v) else as.character(v); tags$td(val) })))
      table_html(headers,rows,"report-data-table rotation-report-table")
    }
    setting_ui <- function() {
      rot <- m$rotation_setting
      rot_table <- if (is.null(rot)||!nrow(rot)) p("No rotation setting totals found.") else table_html(names(rot),lapply(seq_len(nrow(rot)),function(i) tags$tr(lapply(names(rot),function(nm) tags$td(if(is.numeric(rot[[nm]])) esc_num(rot[[nm]][i]) else as.character(rot[[nm]][i]))))))
      block <- function(dat,label) { if(is.null(dat)||!nrow(dat)) return(div(class="chart-card",h4(label),p("No data found."))); headers<-names(dat); rows<-lapply(seq_len(nrow(dat)),function(i) tags$tr(lapply(headers,function(nm) {v<-dat[[nm]][i]; tags$td(if(is.numeric(v)) if(grepl("%|Share|K%|E%",nm,ignore.case=TRUE)) paste0(sprintf("%.1f",ifelse(abs(v)<=1,v*100,v)),"%") else esc_num(v) else as.character(v))}))); div(class="chart-card",h4(label),table_html(headers,rows)) }
      tagList(div(class="chart-card",h3("Rotation setting totals"),rot_table),fluidRow(column(6,block(m$fb_sets,"Who Sets · First Ball")),column(6,block(m$trans_sets,"Who Sets · Transition"))))
    }
    passing_ui <- function() {
      z <- m$passing
      if (is.null(z) || !nrow(z)) return(div(class="placeholder-panel", "No Passing by Contact Point data found in this workbook."))
      needed <- c("Player No.", "Player Name", "Contact Point", "Total Pass", "Good Pass", "Aced")
      if (!all(needed %in% names(z))) {
        raw_rows <- lapply(seq_len(nrow(z)), function(i) {
          tags$tr(lapply(names(z), function(nm) tags$td(as.character(z[[nm]][i]))))
        })
        return(table_html(names(z), raw_rows))
      }
      if (!"Position" %in% names(z)) z$Position <- ""
      z <- z |>
        mutate(`Total Pass`=suppressWarnings(as.numeric(`Total Pass`)),
               `Good Pass`=suppressWarnings(as.numeric(`Good Pass`)),
               Aced=suppressWarnings(as.numeric(Aced))) |>
        group_by(`Player No.`, `Player Name`, Position, `Contact Point`) |>
        summarise(Passes=sum(`Total Pass`,na.rm=TRUE), Good=sum(`Good Pass`,na.rm=TRUE), Aced=sum(Aced,na.rm=TRUE), .groups="drop")
      div(class="passing-player-grid", lapply(unique(z$`Player Name`), function(nm) {
        q <- z[z$`Player Name` == nm, , drop=FALSE]
        total <- sum(q$Passes); good <- sum(q$Good); aced <- sum(q$Aced)
        point_rows <- lapply(seq_len(nrow(q)), function(i) {
          good_pct <- if (q$Passes[i] > 0) paste0(round(q$Good[i]/q$Passes[i]*100,1), "%") else "—"
          tags$tr(tags$td(q[["Contact Point"]][i]), tags$td(esc_num(q$Passes[i])), tags$td(esc_num(q$Good[i])), tags$td(esc_num(q$Aced[i])), tags$td(good_pct))
        })
        div(class="passing-player-card",
          div(class="passing-player-header", span(paste0("#", q$`Player No.`[1], " ", nm)), span(q$Position[1])),
          div(class="chart-card",
            div(class="metric-row", div(class="metric-copy", div(class="metric-name", "Good Pass %")),
              div(class="metric-value", if (total > 0) paste0(round(good/total*100,1), "%") else "—"),
              div(class="metric-bar-area", div(class="metric-track", div(class="metric-fill metric-good", style=paste0("width:", if (total > 0) good/total*100 else 0, "%"))))),
            table_html(c("Contact Point", "Passes", "Good Pass", "Aced", "Good %"), point_rows)
          ),
          div(class="passing-player-footer", tags$strong(paste(total,"passes")), tags$strong(paste0(if(total>0) round(good/total*100,1) else 0,"% good")), tags$strong(paste(aced,"aced")))
        )
      }))
    }
    set_blocks <- lapply(rz$sets, function(st) {
      rows <- if (nrow(st$points)) {
        point_rows <- lapply(seq_len(nrow(st$points)), function(i) {
          pp <- st$points[i, ]
          event <- tolower(as.character(pp$huskies_event))
          is_error <- nzchar(as.character(pp$error_type))
          is_good <- grepl("fbk|d-s-k|ace|block|kill|(^|[^a-z])k([^a-z]|$)", event, ignore.case=TRUE)
          cls <- if (is_error) "redzone-error-row" else if (is_good) "redzone-good-row" else ""
          tags$tr(class=cls,
            tags$td(pp$point_order),
            tags$td(if (nzchar(as.character(pp$huskies_event))) as.character(pp$huskies_event) else "—"),
            tags$td(pp$score),
            tags$td(if (nzchar(as.character(pp$opponent_event))) as.character(pp$opponent_event) else "—")
          )
        })
        tags$table(class="redzone-table",
          tags$thead(tags$tr(tags$th("#"), tags$th("Huskies"), tags$th("Score"), tags$th(m$opponent))),
          tags$tbody(point_rows)
        )
      } else p("No point-by-point entries found in this set.")
      div(class="redzone-set-card",
        h3(paste0("Set ", st$set_number, if (nzchar(st$opening_score)) paste0(" · ", st$opening_score) else "")),
        rows,
        p(paste0("Errors: Serve ", st$serve_errors, " · Attack ", st$attack_errors, " · Block ", st$block_errors))
      )
    })
    redzone_totals <- c(
      sum(vapply(rz$sets, function(st) st$serve_errors, numeric(1))),
      sum(vapply(rz$sets, function(st) st$attack_errors, numeric(1))),
      sum(vapply(rz$sets, function(st) st$block_errors, numeric(1)))
    )
    tagList(
      div(class="report-title-block",
        h1("Most Recent Match"),
        h3(paste(m$opponent, "·", d)),
        div(class="report-match-result", ifelse(is.na(m$result), "Result not recorded", m$result)),
        span(class="data-status", "MATCH DATA CONNECTED")
      ),
      section("KEY METRICS · PERFORMANCE PROFILE", "Grouped by game phase, passing, key offensive and defensive metrics. Bars use fixed scales for percentages and efficiency.", metric_ui()),
      section("BY SKILL", "Attack, serving, reception, blocking, defense and other skill totals from the workbook.", skill_ui()),
      section("ATTACK DETAIL", "First-ball and transition attack by attack type. Efficiency is (Kills − Errors) / Attempts.", div(class="chart-card", h3("ATTACK TYPE SUMMARY"), attack_ui())),
      section("ROTATION DETAILS", "Serve, reception and rotation outcomes.", div(class="chart-card", rotation_ui())),
      section("SETTING STATS", "Rotation distribution and who sets on first-ball and transition.", setting_ui()),
      section("PASSING BY CONTACT POINT", "Reception outcomes by player and contact point.", passing_ui()),
      section("RED ZONE", "Point-by-point pressure sequence after the Huskies reach 20. Positive Huskies outcomes are green; Huskies errors are red.",
        tagList(
          div(class="redzone-summary",
            div(class="redzone-summary-card", span("Serve errors"), strong(redzone_totals[1])),
            div(class="redzone-summary-card", span("Attack errors"), strong(redzone_totals[2])),
            div(class="redzone-summary-card", span("Block errors"), strong(redzone_totals[3])),
            div(class="redzone-summary-card", span("Total errors"), strong(sum(redzone_totals)))
          ),
          if (length(set_blocks)) tagList(set_blocks) else p(class="status-error", "This workbook does not contain Red Zone set sequences yet.")
        )
      )
    )
  })

  # ---------------- ATHLETES TABLE ----------------

  athlete_button_id <- function(jersey, first_name) {
    paste0("athlete_open_", gsub("[^A-Za-z0-9]", "", paste(jersey, first_name, sep = "_")))
  }

  # Each roster row has a real Shiny action control so selecting a name opens its profile.
  output$roster_table_ui <- renderUI({
    search <- if (is.null(input$athlete_search)) "" else tolower(input$athlete_search)
    shown <- roster |>
      filter(grepl(search, tolower(paste(FirstName, Jersey, Position)))) |>
      arrange(suppressWarnings(as.numeric(Jersey)))

    if (!nrow(shown)) return(div(class = "placeholder-panel", "No athletes match that search."))

    tags$div(class = "table-responsive",
      tags$table(class = "athlete-roster-table",
        tags$thead(tags$tr(tags$th("Jersey #"), tags$th("First Name"), tags$th("Position"))),
        tags$tbody(lapply(seq_len(nrow(shown)), function(i) {
          player <- shown[i, , drop = FALSE]
          id <- athlete_button_id(player$Jersey[[1]], player$FirstName[[1]])
          tags$tr(
            tags$td(player$Jersey[[1]]),
            tags$td(actionLink(id, label = player$FirstName[[1]], class = "athlete-open-link")),
            tags$td(player$Position[[1]])
          )
        }))
      )
    )
  })

  # Register profile-open events for the season roster.
  if (nrow(roster)) {
    for (i in seq_len(nrow(roster))) {
      local({
        player_name <- as.character(roster$FirstName[i])
        player_jersey <- as.character(roster$Jersey[i])
        this_id <- athlete_button_id(player_jersey, player_name)
        observeEvent(input[[this_id]], {
          selected_athlete(list(name = player_name, jersey = player_jersey,
                                position = as.character(roster$Position[match(player_jersey, roster$Jersey)])))
          show_athletes(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  }

  # Read the per-match athlete workbook paired with each main match workbook.
  athlete_match_data <- reactive({
    ms <- matches()
    if (!length(ms)) return(list())
    lapply(ms, function(m) {
      athlete_file <- sub("\\.xlsx$", "-Athlete.xlsx", basename(m$file), ignore.case = TRUE)
      athlete_path <- file.path(match_dir, athlete_file)
      # Also accept files uploaded directly into data/ to remain compatible with earlier folder guidance.
      if (!file.exists(athlete_path)) athlete_path <- file.path("data", athlete_file)
      if (!file.exists(athlete_path)) return(list(file = athlete_file, date = m$date, opponent = m$opponent, attack = tibble(), setting = tibble(), dump = tibble(), exists = FALSE))

      raw_attack <- tryCatch(read_excel(athlete_path, sheet = "Attack"), error = function(e) tibble())
      if (nrow(raw_attack) && all(c("Player Name", "Attack Type") %in% names(raw_attack))) {
        raw_attack <- raw_attack |>
          fill(`Player Name`, `Attack Type`, .direction = "down") |>
          filter(!is.na(`Attack Attempts`))
      }
      for (nm in intersect(c("Attack Attempts", "Kills", "Stuffed Attacks", "Error", "Eff."), names(raw_attack))) raw_attack[[nm]] <- suppressWarnings(as.numeric(raw_attack[[nm]]))
      if (nrow(raw_attack)) {
        raw_attack <- raw_attack |>
          mutate(`Player Name` = trimws(as.character(`Player Name`)),
                 `Attack Type` = as.character(`Attack Type`),
                 date = m$date, opponent = m$opponent, match_file = m$file)
      }

      raw_setter <- tryCatch(read_excel(athlete_path, sheet = "Setter", col_names = FALSE), error = function(e) tibble())
      setting <- tibble()
      dump <- tibble()
      if (nrow(raw_setter) >= 2) {
        setting_header <- trimws(as.character(unlist(raw_setter[1, ], use.names = FALSE)))
        setting_header[is.na(setting_header) | setting_header == ""] <- paste0("X", which(is.na(setting_header) | setting_header == ""))
        setting_row <- which(!is.na(raw_setter[[1]]) & trimws(as.character(raw_setter[[1]])) != "" & trimws(as.character(raw_setter[[1]])) != "Player Name")[1]
        if (length(setting_row) && !is.na(setting_row)) {
          setting <- as_tibble(raw_setter[setting_row, seq_len(min(7, ncol(raw_setter)))])
          names(setting) <- setting_header[seq_len(ncol(setting))]
          setting <- setting |>
            mutate(`Player Name` = trimws(as.character(`Player Name`)), date = m$date, opponent = m$opponent, match_file = m$file)
        }
        dump_header_row <- which(vapply(seq_len(nrow(raw_setter)), function(r) any(trimws(as.character(unlist(raw_setter[r, ], use.names = FALSE))) == "Attempts", na.rm = TRUE), logical(1)))[1]
        if (length(dump_header_row) && !is.na(dump_header_row) && dump_header_row < nrow(raw_setter)) {
          dump_header <- trimws(as.character(unlist(raw_setter[dump_header_row, ], use.names = FALSE)))
          dump_header <- dump_header[seq_len(min(6, ncol(raw_setter)))]
          dump_row <- dump_header_row + 1
          if (!is.na(raw_setter[[1]][dump_row]) && trimws(as.character(raw_setter[[1]][dump_row])) != "") {
            dump <- as_tibble(raw_setter[dump_row, seq_len(length(dump_header))])
            names(dump) <- dump_header
            dump <- dump |>
              mutate(`Player Name` = trimws(as.character(`Player Name`)), date = m$date, opponent = m$opponent, match_file = m$file)
          }
        }
      }
      list(file = athlete_file, date = m$date, opponent = m$opponent, attack = raw_attack, setting = setting, dump = dump, exists = TRUE)
    })
  })

  canonical_player <- function(x) {
    x <- trimws(as.character(x))
    x[x == "Raf"] <- "Rafael"
    x
  }

  athlete_role <- function(position) {
    p <- toupper(trimws(as.character(position)))
    if (p %in% c("S", "SETTER")) return("Setter")
    if (p %in% c("L", "LIBERO", "DS")) return("Libero")
    if (p %in% c("OH", "OUTSIDE HITTER")) return("Outside Hitter")
    if (p %in% c("OPP", "OP", "OPPOSITE", "RS", "RIGHT SIDE")) return("Opposite")
    if (p %in% c("MH", "MB", "MIDDLE", "MIDDLE HITTER", "MIDDLE BLOCKER")) return("Middle Hitter")
    "Attacker"
  }

  athlete_attack_data <- reactive({
    sel <- selected_athlete(); req(sel)
    chunks <- lapply(athlete_match_data(), function(x) {
      d <- x$attack
      if (!nrow(d) || !all(c("Player Name", "Attack Type") %in% names(d))) return(tibble())
      d |>
        mutate(player_key = canonical_player(`Player Name`)) |>
        filter(player_key == canonical_player(sel$name))
    })
    bind_rows(chunks)
  })

  athlete_reception_data <- reactive({
    sel <- selected_athlete(); req(sel)
    ms <- matches()
    chunks <- lapply(ms, function(m) {
      d <- m$passing
      if (is.null(d) || !nrow(d) || !all(c("Player Name", "Contact Point") %in% names(d))) return(tibble())
      d |>
        mutate(player_key = canonical_player(`Player Name`), date = m$date, opponent = m$opponent, match_file = m$file) |>
        filter(player_key == canonical_player(sel$name))
    })
    bind_rows(chunks)
  })

  athlete_profile_name <- reactive({
    sel <- selected_athlete(); req(sel)
    paste0("#", sel$jersey, " ", sel$name)
  })

  output$athlete_profile_ui <- renderUI({
    sel <- selected_athlete(); req(sel)
    role <- athlete_role(sel$position)
    is_setter <- identical(role, "Setter")
    is_libero <- identical(role, "Libero")
    is_oh <- identical(role, "Outside Hitter")
    is_reception_role <- is_libero || is_oh
    is_attacker <- !is_setter && !is_libero
    tagList(
      div(class = "athlete-profile-wrap",
        div(class = "athlete-profile-heading",
          div(h2(class = "athlete-profile-title", athlete_profile_name()),
              div(class = "athlete-profile-subtitle", paste(role, "· Individual match-by-match profile"))),
          actionButton("athlete_back", "Back to Athletes", icon = icon("arrow-left"), class = "btn btn-outline-primary")
        ),
        if (is_setter) {
          div(class = "athlete-profile-grid",
            div(class = "athlete-profile-card athlete-profile-wide", h3("Setting Quality · Match by Match"), tableOutput("athlete_setting_table")),
            div(class = "athlete-profile-card athlete-profile-wide", h3("Setter Attack / Dump · Match by Match"), tableOutput("athlete_dump_table"))
          )
        } else tagList(
          if (is_attacker) div(class = "athlete-profile-grid",
            div(class = "athlete-profile-card athlete-profile-wide", h3("Attack Efficiency by Match"), plotOutput("athlete_attack_plot", height = 310)),
            div(class = "athlete-profile-card athlete-profile-wide", h3("Attack Type Summary · All Matches"), tableOutput("athlete_attack_table"))
          ),
          if (is_reception_role) div(class = "athlete-profile-grid",
            div(class = "athlete-profile-card athlete-profile-wide", h3("Passing by Contact Point"), uiOutput("athlete_passing_profile_ui")),
            div(class = "athlete-profile-card athlete-profile-wide", h3("Reception by Match"), tableOutput("athlete_reception_matches_table"))
          )
        )
      )
    )
  })

  output$athlete_attack_plot <- renderPlot({
    sel <- selected_athlete(); req(sel)
    d <- athlete_attack_data()
    validate(need(nrow(d) > 0, "No matching athlete attack rows were found in the paired -Athlete.xlsx files."))
    d <- d |>
      group_by(date, opponent, match_file) |>
      summarise(
        attempts = sum(suppressWarnings(as.numeric(`Attack Attempts`)), na.rm = TRUE),
        kills = sum(suppressWarnings(as.numeric(Kills)), na.rm = TRUE),
        errors = sum(suppressWarnings(as.numeric(Error)), na.rm = TRUE),
        stuffed = sum(suppressWarnings(as.numeric(`Stuffed Attacks`)), na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        efficiency = ifelse(attempts > 0, (kills - errors - stuffed) / attempts, NA_real_),
        match_key = basename(as.character(match_file)),
        match_label = ifelse(is.na(date), opponent, paste(format(date, "%b %d"), opponent, sep = " · "))
      )

    # Full-season team attack efficiency is calculated from ALL paired -Athlete.xlsx
    # Attack sheets, pooling counts before calculating efficiency. This avoids relying
    # on columns that are not present in the main match workbook's Attack Detail sheet.
    all_athlete_matches <- athlete_match_data()
    all_team_attack <- bind_rows(lapply(all_athlete_matches, function(x) {
      a <- x$attack
      if (is.null(a) || !nrow(a) ||
          !all(c("Attack Attempts", "Kills", "Stuffed Attacks", "Error") %in% names(a))) {
        return(tibble())
      }
      a |>
        transmute(
          attempts = suppressWarnings(as.numeric(`Attack Attempts`)),
          kills = suppressWarnings(as.numeric(Kills)),
          stuffed = suppressWarnings(as.numeric(`Stuffed Attacks`)),
          errors = suppressWarnings(as.numeric(Error))
        )
    }))

    team_average <- NA_real_
    if (nrow(all_team_attack)) {
      team_attempts <- sum(all_team_attack$attempts, na.rm = TRUE)
      if (is.finite(team_attempts) && team_attempts > 0) {
        team_average <- (
          sum(all_team_attack$kills, na.rm = TRUE) -
          sum(all_team_attack$errors, na.rm = TRUE) -
          sum(all_team_attack$stuffed, na.rm = TRUE)
        ) / team_attempts
      }
    }

    d <- d |> arrange(date)
    d$match_label <- factor(d$match_label, levels = unique(d$match_label))

    p <- ggplot(d, aes(x = match_label, y = efficiency, group = 1)) +
      geom_line(color = "#17466B", linewidth = 1.1, na.rm = TRUE) +
      geom_point(color = "#17466B", size = 3, na.rm = TRUE) +
      labs(x = NULL, y = "Attack efficiency",
           caption = "Blue: player efficiency · Orange dashed: team attack-efficiency average") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid.minor = element_blank())
    if (is.finite(team_average)) {
      p <- p + geom_hline(yintercept = team_average, color = "#F07832",
                          linewidth = 1, linetype = "dashed")
    }
    p
  })

  output$athlete_attack_table <- renderTable({
    d <- athlete_attack_data()
    validate(need(nrow(d) > 0, "No attack data found for this player."))
    type_col <- "Attack Type"
    agg <- d |>
      group_by(`Attack Type`) |>
      summarise(Attempts = sum(`Attack Attempts`, na.rm=TRUE), Kills = sum(Kills, na.rm=TRUE),
                `Stuffed Attacks` = sum(`Stuffed Attacks`, na.rm=TRUE), Error = sum(Error, na.rm=TRUE), .groups="drop") |>
      mutate(Efficiency = ifelse(Attempts > 0, (Kills - `Stuffed Attacks` - Error) / Attempts, NA_real_))
    total_row <- tibble(
      `Attack Type` = "TOTAL",
      Attempts = sum(agg$Attempts, na.rm=TRUE),
      Kills = sum(agg$Kills, na.rm=TRUE),
      `Stuffed Attacks` = sum(agg$`Stuffed Attacks`, na.rm=TRUE),
      Error = sum(agg$Error, na.rm=TRUE),
      Efficiency = ifelse(sum(agg$Attempts, na.rm=TRUE) > 0,
        (sum(agg$Kills, na.rm=TRUE) - sum(agg$`Stuffed Attacks`, na.rm=TRUE) - sum(agg$Error, na.rm=TRUE)) / sum(agg$Attempts, na.rm=TRUE), NA_real_)
    )
    bind_rows(agg, total_row) |>
      mutate(Efficiency = ifelse(is.na(Efficiency), "—", sprintf("%.3f", as.numeric(Efficiency)))) |>
      rename(`Attack Eff.` = Efficiency)
  }, striped=TRUE, bordered=FALSE, hover=TRUE, spacing="s", digits=0)

  output$athlete_passing_profile_ui <- renderUI({
    sel <- selected_athlete(); req(sel)
    d <- athlete_reception_data()
    validate(need(nrow(d) > 0, "No passing-by-contact-point data found for this player."))

    d <- d |>
      mutate(
        `Contact Point` = trimws(as.character(`Contact Point`)),
        `Total Pass` = suppressWarnings(as.numeric(`Total Pass`)),
        `Good Pass` = suppressWarnings(as.numeric(`Good Pass`)),
        Aced = suppressWarnings(as.numeric(Aced))
      ) |>
      group_by(`Contact Point`) |>
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

    point_ui <- function(point_name, class_name) {
      z <- d[tolower(d$`Contact Point`) == tolower(point_name), , drop = FALSE]
      if (!nrow(z) || is.na(z$`Total Pass`[1]) || z$`Total Pass`[1] <= 0) {
        return(div(class = paste("passing-point", class_name),
          div(class = "passing-point-title", point_name),
          div(class = "passing-point-empty", "none")))
      }
      total <- z$`Total Pass`[1]
      gp <- z$`GP %`[1]
      aced <- z$Aced[1]
      ap <- z$`Aced %`[1]
      div(class = paste("passing-point", class_name),
        div(class = "passing-point-title", point_name),
        div(class = "passing-point-count",
          span(if (is.finite(gp)) sprintf("%.0f%%", gp * 100) else "—"),
          span(class = "passing-good-label", "good")
        ),
        div(class = "passing-point-sub", tags$strong(paste0(format(round(total), trim = TRUE), " attempts"))),
        div(class = "passing-point-sub",
          paste0(format(round(aced), trim = TRUE), " aced",
                 ifelse(is.finite(ap), paste0(" · ", round(ap * 100), "%"), "")))
      )
    }

    total_passes <- sum(d$`Total Pass`, na.rm = TRUE)
    total_good <- sum(d$`Good Pass`, na.rm = TRUE)
    total_aced <- sum(d$Aced, na.rm = TRUE)
    div(class = "athlete-passing-profile-card",
      div(class = "passing-player-card",
        div(class = "passing-player-header", style = "background-color:#123f68 !important; color:#ffffff !important;",
          div(class = "passing-player-name", paste0("#", sel$jersey, " ", sel$name)),
          div(class = "passing-player-meta", as.character(sel$position))
        ),
        div(class = "passing-player-caption", "Contact-point outcomes · player facing you"),
        div(class = "passing-court-stage",
          point_ui("Overhand", "passing-overhand"),
          point_ui("His Right", "passing-left"),
          div(class = "passing-player-figure",
            tags$img(src = URLencode(passing_player_image, reserved = TRUE), alt = "Volleyball player silhouette"),
            point_ui("On body", "passing-body-point")
          ),
          point_ui("His Left", "passing-right"),
          point_ui("Low", "passing-low")
        ),
        div(class = "passing-player-footer",
          span(HTML(paste0("<strong>", format(round(total_passes), trim = TRUE), "</strong> passes"))),
          span(HTML(paste0("<strong>", if (total_passes > 0) sprintf("%.1f%%", total_good / total_passes * 100) else "—", "</strong> good"))),
          span(HTML(paste0("<strong>", format(round(total_aced), trim = TRUE), "</strong> aced")))
        )
      )
    )
  })

  output$athlete_reception_table <- renderTable({
    d <- athlete_reception_data()
    validate(need(nrow(d) > 0, "No reception data found for this player in the match workbooks."))
    needed <- c("Contact Point", "Total Pass", "Good Pass", "Aced")
    validate(need(all(needed %in% names(d)), "Reception columns in the match workbook do not match the expected format."))
    d |>
      group_by(`Contact Point`) |>
      summarise(Attempts = sum(`Total Pass`, na.rm=TRUE), `Good Pass` = sum(`Good Pass`, na.rm=TRUE), Aced = sum(Aced, na.rm=TRUE), .groups="drop") |>
      mutate(`Good %` = ifelse(Attempts>0, sprintf("%.1f%%", 100*`Good Pass`/Attempts), "—"), `Aced %` = ifelse(Attempts>0, sprintf("%.1f%%", 100*Aced/Attempts), "—")) |>
      select(`Contact Point`, Attempts, `Good Pass`, `Good %`, Aced, `Aced %`)
  }, striped=TRUE, bordered=FALSE, hover=TRUE, spacing="s", digits=0)

  output$athlete_reception_matches_table <- renderTable({
    d <- athlete_reception_data()
    validate(need(nrow(d) > 0, "No reception data found for this player."))
    needed <- c("Total Pass", "Good Pass", "Aced")
    validate(need(all(needed %in% names(d)), "Reception columns in the match workbook do not match the expected format."))
    d |>
      group_by(date, opponent, match_file) |>
      summarise(Attempts = sum(`Total Pass`, na.rm=TRUE), `Good Pass` = sum(`Good Pass`, na.rm=TRUE), Aced = sum(Aced, na.rm=TRUE), .groups="drop") |>
      mutate(`Good %` = ifelse(Attempts>0, sprintf("%.1f%%",100*`Good Pass`/Attempts),"—"), Match = ifelse(is.na(date),opponent,paste(format(date,"%b %d"),opponent,sep=" · "))) |>
      select(Match, Attempts, `Good Pass`, `Good %`, Aced)
  }, striped=TRUE, bordered=FALSE, hover=TRUE, spacing="s", digits=0)

  output$athlete_setting_table <- renderTable({
    sel <- selected_athlete(); req(sel)
    rows <- lapply(athlete_match_data(), function(x) {
      d <- x$setting
      if (!nrow(d) || !"Player Name" %in% names(d)) return(tibble())
      d |> mutate(player_key=canonical_player(`Player Name`)) |> filter(player_key==canonical_player(sel$name))
    })
    d <- bind_rows(rows)
    validate(need(nrow(d)>0, "No setting-quality data found for this setter."))
    for (nm in intersect(c("Positive Sets","Off Sets","Poor Sets","Setting Errors","Total Attempts","Positive Sets %"),names(d))) d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
    counts <- intersect(c("Positive Sets","Off Sets","Poor Sets","Setting Errors","Total Attempts"), names(d))
    total <- d[1,,drop=FALSE]
    for(nm in counts) total[[nm]] <- sum(d[[nm]],na.rm=TRUE)
    pct_col <- "Positive Sets %"
    if(pct_col %in% names(total)) total[[pct_col]] <- ifelse(total[["Total Attempts"]]>0,total[["Positive Sets"]]/total[["Total Attempts"]],NA_real_)
    total$opponent <- "TOTAL"
    total$date <- as.Date(NA)
    total$match_file <- "TOTAL"
    out <- bind_rows(d,total) |> mutate(Match=ifelse(opponent=="TOTAL","TOTAL",ifelse(is.na(date),opponent,paste(format(date,"%b %d"),opponent,sep=" · "))))
    if(pct_col %in% names(out)) out[[pct_col]] <- ifelse(is.na(out[[pct_col]]),"—",sprintf("%.1f%%",100*as.numeric(out[[pct_col]])))
    out |> select(Match, any_of(c("Positive Sets","Off Sets","Poor Sets","Setting Errors","Total Attempts","Positive Sets %")))
  }, striped=TRUE, bordered=FALSE, hover=TRUE, spacing="s", digits=0)

  output$athlete_dump_table <- renderTable({
    sel <- selected_athlete(); req(sel)
    rows <- lapply(athlete_match_data(), function(x) {
      d <- x$dump
      if (!nrow(d) || !"Player Name" %in% names(d)) return(tibble())
      d |> mutate(player_key=canonical_player(`Player Name`)) |> filter(player_key==canonical_player(sel$name))
    })
    d <- bind_rows(rows)
    validate(need(nrow(d)>0, "No setter attack/dump data found for this player."))
    for(nm in intersect(c("Attempts","Kills","Stuffed Attacks","Error","Eff."),names(d))) d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
    total <- d[1,,drop=FALSE]
    for(nm in intersect(c("Attempts","Kills","Stuffed Attacks","Error"),names(d))) total[[nm]] <- sum(d[[nm]],na.rm=TRUE)
    if("Eff." %in% names(total)) total[["Eff."]] <- ifelse(total$Attempts>0,(total$Kills-total$`Stuffed Attacks`-total$Error)/total$Attempts,NA_real_)
    total$opponent <- "TOTAL"; total$date <- as.Date(NA); total$match_file <- "TOTAL"
    out <- bind_rows(d,total) |> mutate(Match=ifelse(opponent=="TOTAL","TOTAL",ifelse(is.na(date),opponent,paste(format(date,"%b %d"),opponent,sep=" · "))))
    if("Eff." %in% names(out)) out[["Eff."]] <- ifelse(is.na(out[["Eff."]]),"—",sprintf("%.3f",as.numeric(out[["Eff."]])))
    out |> select(Match, any_of(c("Attempts","Kills","Stuffed Attacks","Error","Eff.")))
  }, striped=TRUE, bordered=FALSE, hover=TRUE, spacing="s", digits=0)

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
    pieces <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, pieces)
    if (!length(pieces)) return(data.frame())
    d <- tryCatch(dplyr::bind_rows(pieces), error=function(e) data.frame())
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
    if (!is.data.frame(d) || !nrow(d) || !all(c("Attack Type", "Previous Play", "Attempts", "Kills", "Error") %in% names(d))) return(data.frame())
    d[["Attack Type"]] <- trimws(as.character(d[["Attack Type"]]))
    d <- d[!is.na(d[["Attack Type"]]) & nzchar(d[["Attack Type"]]), , drop=FALSE]
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
    validate(need(is.data.frame(d) && nrow(d) > 0, "No usable attack detail found. Check the Attack Detail worksheet column headers."))

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
        div(class = "passing-point-count",
          span(if (is.finite(gp)) sprintf("%.0f%%", gp * 100) else "—"),
          span(class = "passing-good-label", "good")
        ),
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
