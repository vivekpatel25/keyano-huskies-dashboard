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

        div(
          class = "chart-card",
          plotOutput("skill_plot", height = 300)
        ),

        div(
          class = "chart-card table-wrap",
          tableOutput("skill_table")
        )
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
    m <- current_match()
    req(m)
    d <- m$key
    if (!nrow(d)) return(div(class = "status-error", "No numeric Key Metrics were found in this workbook."))

    # Match comparison: use the next available match in the loaded, date-sorted season list.
    all_matches <- matches()
    prev <- NULL
    if (length(all_matches) > 1) {
      idx <- which(vapply(all_matches, function(x) identical(x$file, m$file), logical(1)))
      if (length(idx) && idx[1] < length(all_matches)) prev <- all_matches[[idx[1] + 1]]
    }

    category_group <- function(cat, metric) {
      z <- tolower(paste(cat, metric))
      if (grepl("game phase|side-out|first-ball so|point scoring", z)) return("GAME PHASE")
      if (grepl("passing|in-system|aced", z)) return("PASSING")
      if (grepl("offensive|attack|conversion", z)) return("KEY OFFENSIVE")
      if (grepl("defensive|block|disruption|dig|opponent", z)) return("KEY DEFENSIVE")
      toupper(cat)
    }
    d$display_group <- mapply(category_group, d$category, d$metric, USE.NAMES = FALSE)
    d$metric_lower_better <- grepl("aced %|opponent attack eff|error|deadball", d$metric, ignore.case = TRUE)
    d$is_eff <- grepl("eff", d$metric, ignore.case = TRUE)
    d$bar_value <- ifelse(d$is_eff, pmax(0, pmin(1, d$value)), pmax(0, pmin(100, ifelse(abs(d$value) <= 1 & grepl("%", d$metric), d$value * 100, d$value))))

    groups <- unique(d$display_group)
    tagList(lapply(groups, function(g) {
      rows <- d[d$display_group == g, , drop = FALSE]
      div(class = "metric-group",
        div(class = "metric-group-title", g),
        lapply(seq_len(nrow(rows)), function(i) {
          r <- rows[i, , drop = FALSE]
          maxval <- if (r$is_eff) 1 else 100
          width <- max(0, min(100, r$bar_value / maxval * 100))
          val_label <- if (r$is_eff) sprintf("%.3f", r$value) else paste0(sprintf("%.1f", ifelse(abs(r$value) <= 1 & grepl("%", r$metric), r$value * 100, r$value)), "%")
          prev_value <- NA_real_
          if (!is.null(prev) && !is.null(prev$key) && nrow(prev$key)) {
            hit <- prev$key[prev$key$metric == r$metric, , drop = FALSE]
            if (nrow(hit)) prev_value <- suppressWarnings(as.numeric(hit$value[1]))
          }
          prev_bar <- if (is.na(prev_value)) NA_real_ else if (r$is_eff) prev_value else if (abs(prev_value) <= 1 && grepl("%", r$metric)) prev_value * 100 else prev_value
          marker <- if (!is.na(prev_bar) && is.finite(prev_bar)) paste0("left:", max(0, min(100, prev_bar / maxval * 100)), "%") else NULL
          div(class = "metric-row",
            div(class = "metric-copy",
              div(class = "metric-name", r$metric),
              div(class = "metric-description", ifelse(nzchar(r$description), r$description, " "))
            ),
            div(class = "metric-value", val_label),
            div(class = "metric-bar-area",
              div(class = "metric-track", div(class = paste("metric-fill", if (r$metric_lower_better) "metric-bad" else "metric-good"), style = paste0("width:", width, "%"))),
              if (!is.null(marker)) div(class = "metric-marker-wrap", style = marker, title = paste("Previous match:", prev_value), div(class = "metric-marker"), div(class = "metric-marker-label", paste0(prev$opponent, " ", if (r$is_eff) sprintf("%.3f", prev_value) else paste0(round(prev_bar, 1), "%"))))
            )
          )
        })
      )
    }))
  })

  # ---------------- BY SKILL ----------------

  output$skill_plot <- renderPlot({

    m <- current_match()
    req(m)

    d <- m$skills |>
      filter(!is.na(value))

    validate(
      need(nrow(d) > 0, "No By Skill data found.")
    )

    ggplot(
      d,
      aes(
        x = reorder(metric, value),
        y = value,
        fill = skill
      )
    ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      facet_wrap(~skill, scales = "free_y") +
      labs(
        x = NULL,
        y = "Workbook value"
      ) +
      theme_minimal(base_size = 12)
  })

  output$skill_table <- renderTable({

    m <- current_match()
    req(m)

    m$skills |>
      rename(
        Skill = skill,
        Metric = metric,
        Value = value
      )

  }, striped = TRUE, bordered = FALSE, spacing = "s")

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
