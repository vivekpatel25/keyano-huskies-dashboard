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

    rotation_setting <- setting_raw[1:8, 1:4]

    names(rotation_setting) <- c(
      "Rotation",
      "Sets",
      "Kills",
      "Attacks"
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
      }

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
          class = "chart-card",
          plotOutput("attack_plot", height = 320)
        ),

        div(
          class = "chart-card table-wrap",
          tableOutput("attack_table")
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
          tableOutput("rotation_table")
        )
      ),

      # 5. Setting Stats
      div(
        class = "analytics-section",

        section_title(
          "SETTING STATS",
          "Rotation distribution and who sets on first-ball and transition."
        ),

        div(
          class = "chart-card",

          h4("Rotation setting totals"),

          tableOutput("rotation_setting_table")
        ),

        fluidRow(

          column(
            6,

            div(
              class = "chart-card",

              h4("Who Sets · First Ball"),

              tableOutput("fb_table")
            )
          ),

          column(
            6,

            div(
              class = "chart-card",

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

        uiOutput("passing_player_ui"),

        div(
          class = "chart-card",
          plotOutput("passing_plot", height = 300)
        ),

        div(
          class = "chart-card table-wrap",
          tableOutput("passing_table")
        )
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

  output$attack_plot <- renderPlot({

    m <- current_match()
    req(m)

    d <- m$attack

    validate(
      need(
        all(c("Attack Type", "Attempts") %in% names(d)),
        "Attack columns are missing."
      )
    )

    d <- d |>
      filter(
        !is.na(Attempts),
        Attempts > 0
      )

    validate(
      need(nrow(d) > 0, "No attack attempts found.")
    )

    if (!"Previous Play" %in% names(d)) {
      d$`Previous Play` <- "All"
    }

    ggplot(
      d,
      aes(
        x = `Attack Type`,
        y = Attempts,
        fill = `Previous Play`
      )
    ) +
      geom_col(position = "dodge") +
      labs(
        x = NULL,
        y = "Attempts",
        fill = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1
        )
      )
  })

  output$attack_table <- renderTable({

    m <- current_match()
    req(m)

    m$attack

  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------------- ROTATION DETAILS ----------------

  output$rotation_plot <- renderPlot({

    m <- current_match()
    req(m)

    d <- m$rotation

    validate(
      need(
        "Rotation" %in% names(d),
        "Rotation column is missing."
      )
    )

    cols <- intersect(
      c("SO%", "PS%", "Serve In %"),
      names(d)
    )

    validate(
      need(
        length(cols) > 0,
        "No rotation rate columns found."
      )
    )

    long <- d |>
      select(
        Rotation,
        all_of(cols)
      ) |>
      pivot_longer(
        -Rotation,
        names_to = "Metric",
        values_to = "Value"
      ) |>
      mutate(
        Value = suppressWarnings(
          as.numeric(Value)
        )
      ) |>
      filter(!is.na(Value))

    validate(
      need(
        nrow(long) > 0,
        "No rotation values found."
      )
    )

    ggplot(
      long,
      aes(
        x = Rotation,
        y = Value,
        fill = Metric
      )
    ) +
      geom_col(position = "dodge") +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(
        x = NULL,
        y = "Rate",
        fill = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom"
      )
  })

  output$rotation_table <- renderTable({

    m <- current_match()
    req(m)

    m$rotation

  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------------- SETTING STATS ----------------

  output$rotation_setting_table <- renderTable({

    m <- current_match()
    req(m)

    m$rotation_setting

  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$fb_table <- renderTable({

    m <- current_match()
    req(m)

    m$fb_sets

  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$trans_table <- renderTable({

    m <- current_match()
    req(m)

    m$trans_sets

  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------------- PASSING BY CONTACT POINT ----------------

  output$passing_player_ui <- renderUI({

    m <- current_match()

    if (
      is.null(m) ||
      !all(
        c("Player No.", "Player Name") %in% names(m$passing)
      )
    ) {
      return(NULL)
    }

    players <- unique(
      paste0(
        m$passing$`Player No.`,
        " · ",
        m$passing$`Player Name`
      )
    )

    selectInput(
      "pass_player",
      "Player",
      choices = players,
      selected = if (length(players)) players[1] else NULL
    )
  })

  passing_data <- reactive({

    m <- current_match()

    req(m, input$pass_player)

    d <- m$passing

    validate(
      need(
        all(
          c("Player No.", "Player Name") %in% names(d)
        ),
        "Player columns missing."
      )
    )

    d |>
      filter(
        paste0(
          `Player No.`,
          " · ",
          `Player Name`
        ) == input$pass_player
      )
  })

  output$passing_plot <- renderPlot({

    d <- passing_data()

    validate(
      need(
        all(
          c("Contact Point", "Total Pass") %in% names(d)
        ),
        "Passing columns missing."
      )
    )

    d <- d |>
      mutate(
        `Total Pass` = suppressWarnings(
          as.numeric(`Total Pass`)
        )
      ) |>
      filter(!is.na(`Total Pass`))

    validate(
      need(
        nrow(d) > 0,
        "No passing data for this player."
      )
    )

    ggplot(
      d,
      aes(
        x = `Contact Point`,
        y = `Total Pass`
      )
    ) +
      geom_col(fill = "#2F75B5") +
      geom_text(
        aes(label = `Total Pass`),
        vjust = -0.3
      ) +
      labs(
        x = NULL,
        y = "Total Pass"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(
          angle = 20,
          hjust = 1
        )
      )
  })

  output$passing_table <- renderTable({

    passing_data()

  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui, server)
