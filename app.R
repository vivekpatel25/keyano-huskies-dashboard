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

    # Preserve the workbook's metric labels, values, and italic description rows.
    key_rows <- which(
      !is.na(km[[2]]) & trimws(as.character(km[[2]])) != "" &
        !is.na(km[[3]])
    )

    key <- lapply(key_rows, function(i) {
      description <- ""
      if (i < nrow(km)) {
        next_label <- km[[2]][i + 1]
        next_value <- km[[3]][i + 1]
        if (!is.na(next_label) && trimws(as.character(next_label)) != "" && is.na(next_value)) {
          description <- trimws(as.character(next_label))
        }
      }
      tibble(
        metric = trimws(as.character(km[[2]][i])),
        value = km[[3]][i],
        description = description
      )
    }) |>
      bind_rows() |>
      filter(!is.na(metric), metric != "", !is.na(value))

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

# ---------------- DISPLAY HELPERS ----------------

fmt_value <- function(metric, value) {
  if (!length(value) || is.na(value)) return("—")
  value <- suppressWarnings(as.numeric(value))
  if (is.na(value)) return("—")
  nm <- as.character(metric)[1]
  if (grepl("attack efficiency|attack eff|\\beff\\b", nm, ignore.case = TRUE)) {
    return(if (abs(value) < 1) sub("^0", "", sprintf("%.3f", value)) else sprintf("%.3f", value))
  }
  pct <- grepl("%|side.?out|positive pass|good pass|serve.?in|reception|conversion|kill rate|error rate|point scoring|first.?ball|in.?system|opponent attack|block impact|efficiency", nm, ignore.case = TRUE)
  if (pct) {
    # Preserve percentages stored as 0-100; convert proportions only.
    v <- if (abs(value) <= 1) value * 100 else value
    return(paste0(round(v, 1), "%"))
  }
  if (abs(value - round(value)) < 1e-8) return(as.character(round(value)))
  as.character(round(value, 2))
}

metric_value <- function(m, metric) {
  if (is.null(m$key) || !nrow(m$key)) return(NA_real_)
  x <- m$key |> filter(tolower(.data$metric) == tolower(metric))
  if (!nrow(x)) return(NA_real_)
  suppressWarnings(as.numeric(x$value[1]))
}

metric_card <- function(label, value, note = "") {
  div(class = "metric-card", div(class = "metric-label", label), div(class = "metric-value", value), div(class = "metric-note", note))
}

section_title <- function(title, intro) {
  div(class = "section-block-title", div(class = "section-kicker", "HUSKIES PERFORMANCE"), h2(title), p(class = "section-intro", intro))
}

safe_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

# ---------------- USER INTERFACE ----------------

ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#F3F6F9", fg = "#172B43", primary = "#0B2D59", secondary = "#68798D", base_font = font_google("Roboto Condensed"), heading_font = font_google("Oswald")),
  tags$head(
    tags$title("Huskies Coaches Dashboard | Keyano ACAC 2026–27"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("\n      :root { --navy:#0B2D59; --blue:#2F75B5; --ice:#EAF2F8; --ink:#172B43; --muted:#65758A; --line:#DCE5ED; }\n      body { background:#F3F6F9; color:var(--ink); }\n      .container-fluid { max-width:1500px; padding:22px clamp(14px,3vw,42px) 36px; }\n      .dashboard-header { display:grid; grid-template-columns:minmax(0,1fr) minmax(220px,auto) 72px; align-items:center; gap:24px; background:var(--navy); color:white; border-radius:18px; padding:22px 26px; box-shadow:0 12px 28px #0B2D5920; }\n      .brand-area { display:flex; gap:18px; align-items:center; min-width:0; } .team-logo { width:76px; height:76px; object-fit:contain; }\n      .brand-copy h1 { font-size:clamp(23px,3vw,36px); line-height:1.05; letter-spacing:.04em; margin:0; font-weight:700; } .season-label { color:#D7E7F7; font-size:12px; letter-spacing:.17em; margin-top:8px; font-weight:700; }\n      .match-summary { border-left:1px solid #FFFFFF45; padding-left:24px; } .eyebrow,.section-kicker { font-size:10px; font-weight:800; letter-spacing:.17em; color:#9FC9EE; } .match-summary h3 { margin:4px 0; font-size:22px; } .match-detail { color:#D7E7F7; font-size:13px; }\n      .athletes-link { text-align:center; } .athletes-icon-button { color:white!important; background:#FFFFFF18!important; border:1px solid #FFFFFF55!important; border-radius:14px!important; width:54px; height:48px; } .athletes-label { font-size:9px; letter-spacing:.1em; margin-top:5px; font-weight:800; }\n      .period-nav { margin:18px 0 24px; } .period-buttons { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:8px; } .period-button { border-radius:10px!important; border:1px solid var(--line)!important; background:white!important; color:var(--navy)!important; font-weight:700!important; padding:10px 8px!important; } .period-button.active { background:var(--blue)!important; color:white!important; border-color:var(--blue)!important; }\n      .content-card { background:transparent; } .analytics-section { margin:0 0 26px; padding:0 0 26px; border-bottom:1px solid var(--line); } .section-block-title { margin:0 0 15px; } .section-kicker { color:var(--blue); } .section-block-title h2 { margin:3px 0 4px; font-size:25px; letter-spacing:.02em; } .section-intro { color:var(--muted); margin:0 0 14px; font-size:14px; }\n      .section-heading-row { display:flex; justify-content:space-between; align-items:flex-start; gap:12px; margin-bottom:15px; } .section-heading-row h2 { margin:0 0 4px; } .data-status { font-size:10px; font-weight:800; letter-spacing:.1em; border-radius:999px; background:#E1F3E9; color:#216C47; padding:7px 10px; white-space:nowrap; }\n      .metric-grid { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; margin:16px 0; } .metric-card,.panel,.skill-card,.player-card { background:white; border:1px solid var(--line); border-radius:15px; padding:17px; box-shadow:0 5px 18px #122B4308; min-width:0; } .metric-label { color:var(--muted); text-transform:uppercase; font-size:10px; letter-spacing:.1em; font-weight:800; } .metric-value { color:var(--navy); font-size:clamp(22px,2.4vw,32px); font-weight:800; margin:7px 0 3px; overflow-wrap:anywhere; } .metric-note { color:var(--muted); font-size:11px; }\n      .panel { margin:12px 0; } .panel h3,.panel h4 { color:var(--navy); font-weight:800; margin:0 0 10px; } .skill-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:12px; } .skill-card { padding:16px; } .skill-card h3 { margin:0 0 4px; font-size:17px; color:var(--navy); } .skill-card .skill-note { color:var(--muted); font-size:11px; min-height:30px; } .skill-card .shiny-plot-output { margin-top:6px; }\n      .table-wrap { overflow-x:auto; } table { font-size:12px!important; } .table>thead>tr>th { background:#EAF2F8; color:var(--navy); border-bottom:1px solid var(--line)!important; } .table>tbody>tr>td { vertical-align:middle; } .status-error { background:#FFF0F0; color:#9D2828; padding:13px; border-radius:10px; overflow-wrap:anywhere; } .placeholder-panel { background:white; border:1px solid var(--line); border-radius:15px; padding:22px; margin:12px 0; }\n      .rotation-key { font-size:11px; color:var(--muted); margin:4px 0 10px; } .passing-layout { display:grid; grid-template-columns:minmax(180px,260px) minmax(0,1fr); gap:18px; align-items:start; } .passing-art { width:100%; max-height:360px; object-fit:contain; object-position:center top; } .player-card { margin-bottom:10px; } .player-card h4 { margin:0 0 8px; } .footer-note { color:var(--muted); text-align:center; font-size:11px; padding:18px 0 0; }\n      @media(max-width:900px) { .dashboard-header { grid-template-columns:1fr auto; } .match-summary { grid-column:1/-1; border-left:0; border-top:1px solid #FFFFFF45; padding:14px 0 0; } .metric-grid { grid-template-columns:repeat(2,minmax(0,1fr)); } .skill-grid { grid-template-columns:repeat(2,minmax(0,1fr)); } }\n      @media(max-width:600px) { .dashboard-header { grid-template-columns:1fr auto; padding:17px; gap:12px; } .team-logo { width:54px;height:54px; } .brand-area { gap:10px; } .brand-copy h1 { font-size:22px; } .period-buttons { grid-template-columns:repeat(2,minmax(0,1fr)); } .period-buttons .period-button:last-child { grid-column:1/-1; } .metric-grid,.skill-grid { grid-template-columns:1fr 1fr; gap:8px; } .metric-card { padding:12px; } .metric-value { font-size:23px; } .passing-layout { grid-template-columns:1fr; } .passing-art { max-height:220px; } }\n    "))
  ),
  div(class="dashboard-header",
    div(class="brand-area", img(src="keyano-huskies-logo.png", class="team-logo", alt="Keyano Huskies logo"), div(class="brand-copy", h1("HUSKIES COACHES DASHBOARD"), div(class="season-label", "KEYANO · ACAC · 2026–27"))),
    div(class="match-summary", div(class="eyebrow", "LATEST MATCH"), h3(textOutput("latest_match", inline=TRUE)), div(class="match-detail", textOutput("match_details", inline=TRUE))),
    div(class="athletes-link", actionButton("athletes_nav", label=NULL, icon=icon("grip"), class="athletes-icon-button"), div("ATHLETES", class="athletes-label"))
  ),
  div(class="period-nav", uiOutput("period_buttons")),
  div(class="content-card", uiOutput("page_content")),
  div(class="footer-note", "Keyano Huskies · ACAC · 2026–27 | Internal performance analysis")
)

# ---------------- SERVER ----------------

server <- function(input, output, session) {
  selected_period <- reactiveVal("Most Recent Match")
  show_athletes <- reactiveVal(FALSE)
  matches <- reactive({ invalidateLater(60000, session); load_matches() })

  period_matches <- reactive({
    ms <- matches()
    if (!length(ms)) return(list())
    n <- length(ms)
    switch(selected_period(),
      "Most Recent Match" = ms[1],
      "Previous Match" = if (n >= 2) ms[2] else list(),
      "Last 5" = ms[seq_len(min(5,n))],
      "Last 10" = ms[seq_len(min(10,n))],
      "Full Season" = ms,
      ms[1]
    )
  })
  current_match <- reactive({ ms <- period_matches(); if (length(ms)) ms[[1]] else NULL })

  output$latest_match <- renderText({ m <- current_match(); if (is.null(m)) "Match data not connected yet" else m$opponent })
  output$match_details <- renderText({
    m <- current_match()
    if (is.null(m)) return("Sets · receptions · swings · rallies")
    d <- if (is.na(m$date)) "Date unknown" else format(m$date, "%b %d, %Y")
    paste(d, "·", ifelse(is.na(m$result), "Result not recorded", m$result))
  })
  output$period_buttons <- renderUI({
    div(class="period-buttons", lapply(match_windows, function(period) {
      actionButton(paste0("period_", gsub("[^A-Za-z0-9]", "", period)), period,
        class=if (identical(selected_period(),period)) "period-button active" else "period-button", width="100%")
    }))
  })
  lapply(match_windows, function(period) local({
    this_period <- period
    observeEvent(input[[paste0("period_",gsub("[^A-Za-z0-9]","",this_period))]], {
      selected_period(this_period); show_athletes(FALSE)
    })
  }))
  observeEvent(input$athletes_nav, { show_athletes(!show_athletes()) })

  output$page_content <- renderUI({
    if (show_athletes()) return(tagList(
      section_title("ATHLETES", "Season roster · first names only"),
      textInput("athlete_search", "Search athletes", placeholder="First name, jersey number, or position"),
      tableOutput("roster_table")
    ))
    ms <- period_matches(); m <- current_match()
    if (is.null(m)) {
      files <- if (dir.exists(match_dir)) list.files(match_dir, pattern="\\.xlsx$", full.names=TRUE, ignore.case=TRUE) else character()
      files <- files[!grepl("^~\\$",basename(files))]
      errors <- lapply(files, function(f) { x <- read_match(f); paste0(basename(f), ": ", if (!is.null(x$error)) x$error else "Workbook parsed but was not selected") })
      return(tagList(div(class="placeholder-panel", h2("Match data could not be loaded"),
        if (!dir.exists(match_dir)) p("The data/matches folder is missing from the deployed app.") else if (!length(files)) p("No Excel workbooks were found in data/matches/.") else div(class="status-error", lapply(errors, function(x) p(x))),
        p("Check that your latest workbook files are committed inside data/matches/ and contain the required worksheets."))))
    }
    date_label <- if (is.na(m$date)) "Date unknown" else format(m$date,"%b %d, %Y")
    selected_n <- length(ms)
    # Helpers for metric values, preserving workbook scale and terminology.
    ae <- metric_value(m,"Attack Efficiency")
    so <- metric_value(m,"Side-Out %")
    tagList(
      # 1. Overview
      div(class="analytics-section",
        div(class="section-heading-row", div(h2(selected_period()), p(class="section-intro", paste("Team performance overview ·",m$opponent,"·",date_label))), span(class="data-status","MATCH DATA CONNECTED")),
        div(class="metric-grid",
          metric_card("Attack Efficiency",fmt_value("Attack Efficiency",ae),"Match workbook value"),
          metric_card("Side-Out %",fmt_value("Side-Out %",so),"Match workbook value"),
          metric_card("Matches in View",as.character(selected_n),"Selected period"),
          metric_card("Match Date",date_label,"Most recent selected match")
        ),
        div(class="panel",h3("KEY METRICS · PERFORMANCE PROFILE"),p(class="section-intro","Each bar uses a fixed scale: 100% for percentage metrics and 1.000 for efficiency metrics. Excel descriptions are shown below each metric; the red marker compares the previous available match when one exists."),plotOutput("benchmark_plot",height=320),div(class="table-wrap",tableOutput("key_table")))
      ),
      # 2. By skill
      div(class="analytics-section",section_title("BY SKILL","Skill-specific cards preserve the workbook's skill labels and show the recorded metrics for the selected match."),
        uiOutput("skill_cards"),div(class="panel",h3("By Skill · Full Values"),div(class="table-wrap",tableOutput("skill_table")))
      ),
      # 3. Attack detail
      div(class="analytics-section",section_title("ATTACK DETAIL","Compact scouting tables for attack type and previous-play context. Use attempts, kills and errors together to evaluate volume and execution."),
        div(class="panel",h3("Attack Type Comparison"),plotOutput("attack_plot",height=300)),div(class="panel table-wrap",h3("Attack Detail · Workbook Values"),tableOutput("attack_table"))
      ),
      # 4. Rotation details
      div(class="analytics-section",section_title("ROTATION DETAILS","Rotation-by-rotation scouting. The heatmap highlights relative strengths and weaknesses without mixing different measures into a generic bar chart."),
        div(class="panel",h3("Rotation Heatmap"),p(class="rotation-key","Darker cells indicate larger values within each metric column. Read each metric vertically; different metrics may use different scales."),plotOutput("rotation_heatmap",height=330)),div(class="panel table-wrap",h3("Rotation Details · Workbook Values"),tableOutput("rotation_table"))
      ),
      # 5. Setting stats
      div(class="analytics-section",section_title("SETTING STATS","Setter distribution separated into rotation totals, first-ball setting and transition setting."),
        fluidRow(column(12,div(class="panel",h3("Rotation Setting Totals"),div(class="table-wrap",tableOutput("rotation_setting_table")))),
          column(6,div(class="panel",h3("Who Sets · First Ball"),div(class="table-wrap",tableOutput("fb_table")))),
          column(6,div(class="panel",h3("Who Sets · Transition"),div(class="table-wrap",tableOutput("trans_table")))))
      ),
      # 6. Passing by contact point
      div(class="analytics-section",section_title("PASSING BY CONTACT POINT","Choose a player to review reception/contact-point totals. Workbook values remain available beside the visual summary."),
        uiOutput("passing_player_ui"),uiOutput("passing_player_view"),div(class="panel table-wrap",h3("Passing by Contact Point · Workbook Values"),tableOutput("passing_table"))
      )
    )
  })

  # ---------------- ATHLETES ----------------
  output$roster_table <- renderTable({
    search <- if (is.null(input$athlete_search)) "" else tolower(input$athlete_search)
    roster |> filter(grepl(search,tolower(paste(FirstName,Jersey,Position)))) |> rename(`Jersey #`=Jersey, `First Name`=FirstName)
  }, striped=TRUE,bordered=FALSE,hover=TRUE,spacing="s")

  # ---------------- OVERVIEW / KEY METRICS ----------------
  output$key_table <- renderTable({
    m <- current_match(); req(m)
    m$key |> transmute(Metric=metric, Description=description, Value=vapply(seq_len(n()), function(i) fmt_value(metric[i], value[i]), character(1)))
  }, striped=TRUE,bordered=FALSE,spacing="s")

  output$benchmark_plot <- renderPlot({
    ms <- period_matches(); m <- current_match(); req(m)
    d <- m$key |> mutate(
      raw_value = safe_numeric(value),
      is_efficiency = grepl("efficiency|\\beff\\b|atk eff", metric, ignore.case=TRUE),
      is_percent = grepl("%", metric, fixed=TRUE) | grepl("side.?out|aced|in.?system|point scoring|conversion|block impact|disruption|dig|serve.?in|positive pass|good pass", metric, ignore.case=TRUE),
      bar_value = ifelse(is_efficiency, raw_value * 100,
                         ifelse(is_percent & abs(raw_value) <= 1, raw_value * 100, raw_value)),
      bar_max = 100,
      direction = ifelse(grepl("aced|error|deadball", metric, ignore.case=TRUE), "Lower is better", "Higher is better"),
      display_value = vapply(seq_len(n()), function(i) fmt_value(metric[i], raw_value[i]), character(1)),
      description = ifelse(is.na(description), "", description)
    ) |> filter(is.finite(bar_value))
    validate(need(nrow(d)>0, "No numeric Key Metrics were found in this workbook."))
    # The second most recent available match is the comparison marker when present.
    comparison <- if (length(ms) >= 2) ms[[2]] else NULL
    d$comparison_value <- NA_real_
    d$comparison_label <- ""
    if (!is.null(comparison)) {
      for (i in seq_len(nrow(d))) {
        other <- metric_value(comparison, d$metric[i])
        if (is.finite(other)) {
          other_is_eff <- d$is_efficiency[i]
          other_is_pct <- d$is_percent[i]
          d$comparison_value[i] <- if (other_is_eff) other * 100 else if (other_is_pct && abs(other) <= 1) other * 100 else other
        }
      }
      d$comparison_label <- comparison$opponent
    }
    d$metric_label <- factor(ifelse(nzchar(d$description), paste0(d$metric, "\n", d$description), d$metric), levels=rev(ifelse(nzchar(d$description), paste0(d$metric, "\n", d$description), d$metric)))
    ggplot(d, aes(y=metric_label)) +
      geom_segment(aes(x=0, xend=100, yend=metric_label), linewidth=7, colour="#F3EAE2", lineend="round") +
      geom_segment(aes(x=0, xend=pmax(0,pmin(100,bar_value)), yend=metric_label, colour=direction), linewidth=7, lineend="round") +
      geom_segment(data=d |> filter(is.finite(comparison_value)), aes(x=comparison_value, xend=comparison_value, y=as.numeric(metric_label)-0.28, yend=as.numeric(metric_label)+0.28), inherit.aes=FALSE, colour="#E74C3C", linewidth=1.1) +
      scale_x_continuous(limits=c(0,100), breaks=c(0,25,50,75,100), labels=c("0","25","50","75","100%"), expand=expansion(mult=c(0,0))) +
      scale_colour_manual(values=c("Higher is better"="#159B68", "Lower is better"="#DF4B3F"), guide="none") +
      labs(x=NULL,y=NULL,caption=if (length(ms)>=2) paste("Red marker:", comparison$opponent, "(previous available match)") else "Bar scale: 100% for percentage metrics; 1.000 for efficiency metrics.") +
      theme_minimal(base_size=12) + theme(panel.grid.major.y=element_blank(), panel.grid.minor=element_blank(), axis.text.y=element_text(face="bold",colour="#172B43"), axis.text.x=element_text(colour="#65758A"), plot.caption=element_text(colour="#65758A",hjust=0), plot.margin=margin(12,18,12,12))
  }, res=100)

  # ---------------- BY SKILL CARDS ----------------
  output$skill_cards <- renderUI({
    m <- current_match(); req(m); d <- m$skills
    if (is.null(d) || !nrow(d)) return(div(class="placeholder-panel","No By Skill rows were found in this workbook."))
    skills <- unique(as.character(d$skill))
    tagList(div(class="skill-grid",lapply(skills,function(sk) {
      ds <- d |> filter(.data$skill==sk)
      div(class="skill-card",h3(sk),div(class="skill-note",paste(nrow(ds),"recorded metrics · workbook terminology retained")),tableOutput(paste0("skill_table_",make.names(sk))))
    })))
  })
  observe({
    m <- current_match()
    if (is.null(m) || is.null(m$skills) || !nrow(m$skills)) return()
    for (sk in unique(as.character(m$skills$skill))) local({
      this_skill <- sk; output_id <- paste0("skill_table_",make.names(this_skill))
      output[[output_id]] <- renderTable({
        mm <- current_match(); req(mm)
        mm$skills |> filter(.data$skill==this_skill) |> transmute(Metric=metric,Value=vapply(seq_len(n()),function(i) fmt_value(metric[i],value[i]),character(1)))
      },striped=TRUE,bordered=FALSE,spacing="s")
    })
  })
  output$skill_table <- renderTable({ m <- current_match(); req(m); m$skills |> transmute(Skill=skill,Metric=metric,Value=vapply(seq_len(n()),function(i) fmt_value(metric[i],value[i]),character(1))) },striped=TRUE,bordered=FALSE,spacing="s")

  # ---------------- ATTACK DETAIL ----------------
  output$attack_plot <- renderPlot({
    m <- current_match(); req(m); d <- m$attack
    validate(need("Attack Type" %in% names(d),"Attack Type column was not found."),need("Attempts" %in% names(d),"Attempts column was not found."))
    d <- d |> mutate(Attempts=safe_numeric(Attempts)) |> filter(!is.na(Attempts),Attempts>0)
    validate(need(nrow(d)>0,"No attack attempts found for this match."))
    if (!"Previous Play" %in% names(d)) d$`Previous Play` <- "All"
    ggplot(d,aes(x=reorder(`Attack Type`,Attempts,sum),y=Attempts,fill=`Previous Play`))+geom_col(position=position_dodge(width=.8),width=.72)+coord_flip()+labs(x=NULL,y="Attempts",fill="Previous Play")+theme_minimal(base_size=12)+theme(legend.position="bottom",panel.grid.major.y=element_blank())
  })
  output$attack_table <- renderTable({ m <- current_match(); req(m); d <- m$attack; if (!nrow(d)) return(data.frame(Note="No attack detail rows")); d },striped=TRUE,bordered=FALSE,spacing="s")

  # ---------------- ROTATION HEATMAP ----------------
  output$rotation_heatmap <- renderPlot({
    m <- current_match(); req(m); d <- m$rotation
    validate(need("Rotation" %in% names(d),"Rotation column was not found."))
    cols <- intersect(c("SO%","PS%","Serve In %","Side-Out %","Positive Pass %","Attack Efficiency","OOS%","Opponent Attack %"),names(d))
    validate(need(length(cols)>0,"No supported rotation rate columns were found. See the table below for all workbook values."))
    long <- d |> select(Rotation,all_of(cols)) |> pivot_longer(-Rotation,names_to="Metric",values_to="Value") |> mutate(Value=safe_numeric(Value)) |> filter(is.finite(Value))
    validate(need(nrow(long)>0,"No numeric rotation values found."))
    # Standardize each metric to its own observed range so the colour compares rotations within a column.
    long <- long |> group_by(Metric) |> mutate(Heat=if (max(Value,na.rm=TRUE)==min(Value,na.rm=TRUE)) .5 else (Value-min(Value,na.rm=TRUE))/(max(Value,na.rm=TRUE)-min(Value,na.rm=TRUE))) |> ungroup()
    ggplot(long,aes(x=Metric,y=factor(Rotation,levels=rev(unique(Rotation))),fill=Heat))+geom_tile(colour="white",linewidth=1)+geom_text(aes(label=ifelse(grepl("%",Metric),paste0(round(ifelse(abs(Value)<=1,Value*100,Value),1),"%"),ifelse(abs(Value)<1,sprintf("%.3f",Value),round(Value,2)))),size=3.5,colour="#172B43")+scale_fill_gradient(low="#F1F6FA",high="#2F75B5",limits=c(0,1),guide="none")+labs(x=NULL,y=NULL)+theme_minimal(base_size=12)+theme(panel.grid=element_blank(),axis.text.x=element_text(angle=20,hjust=1,colour="#172B43"),axis.text.y=element_text(colour="#172B43"))
  })
  output$rotation_table <- renderTable({ m <- current_match(); req(m); m$rotation },striped=TRUE,bordered=FALSE,spacing="s")

  # ---------------- SETTING STATS ----------------
  output$rotation_setting_table <- renderTable({ m <- current_match(); req(m); m$rotation_setting },striped=TRUE,bordered=FALSE,spacing="s")
  output$fb_table <- renderTable({ m <- current_match(); req(m); m$fb_sets },striped=TRUE,bordered=FALSE,spacing="s")
  output$trans_table <- renderTable({ m <- current_match(); req(m); m$trans_sets },striped=TRUE,bordered=FALSE,spacing="s")

  # ---------------- PASSING BY CONTACT POINT ----------------
  output$passing_player_ui <- renderUI({
    m <- current_match()
    if (is.null(m) || !all(c("Player No.","Player Name") %in% names(m$passing))) return(NULL)
    players <- unique(paste0(m$passing$`Player No.`," · ",m$passing$`Player Name`))
    selectInput("pass_player","Select player",choices=players,selected=if(length(players)) players[1] else NULL)
  })
  passing_data <- reactive({
    m <- current_match(); req(m,input$pass_player); d <- m$passing
    validate(need(all(c("Player No.","Player Name") %in% names(d)),"Player columns were not found."))
    d |> filter(paste0(`Player No.`," · ",`Player Name`)==input$pass_player)
  })
  output$passing_player_view <- renderUI({
    d <- passing_data()
    art_exists <- file.exists(file.path("www","volleyball-player-defense.png"))
    art <- if (art_exists) tags$img(src="volleyball-player-defense.png",class="passing-art",alt="Volleyball defensive player illustration") else NULL
    numeric_cols <- intersect(c("Total Pass","Perfect Pass","Positive Pass","Good Pass","Error","Errors","Reception","Total Receptions"),names(d))
    values <- lapply(numeric_cols,function(nm) {
      v <- sum(safe_numeric(d[[nm]]),na.rm=TRUE)
      metric_card(nm,fmt_value(nm,v),"Selected player · selected match")
    })
    tagList(div(class="passing-layout",div(art),div(if(length(values)) div(class="metric-grid",values) else p("No recognized passing total columns were found; use the table below for the workbook values."),
      if ("Contact Point" %in% names(d) && "Total Pass" %in% names(d)) plotOutput("passing_plot",height=270) else NULL)))
  })
  output$passing_plot <- renderPlot({
    d <- passing_data()
    validate(need(all(c("Contact Point","Total Pass") %in% names(d)),"Contact Point and Total Pass columns were not found."))
    d <- d |> mutate(`Total Pass`=safe_numeric(`Total Pass`)) |> filter(is.finite(`Total Pass`))
    validate(need(nrow(d)>0,"No passing totals available for this player."))
    ggplot(d,aes(x=reorder(`Contact Point`,`Total Pass`),y=`Total Pass`))+geom_col(fill="#2F75B5",width=.68)+geom_text(aes(label=`Total Pass`),hjust=-.2,size=3.5)+coord_flip()+expand_limits(y=max(d$`Total Pass`,na.rm=TRUE)*1.15)+labs(x=NULL,y="Total Pass")+theme_minimal(base_size=12)+theme(panel.grid.major.y=element_blank())
  })
  output$passing_table <- renderTable({ passing_data() },striped=TRUE,bordered=FALSE,spacing="s")
}

shinyApp(ui, server)
