# ============================================================
# HUSKIES COACHES DASHBOARD
# Single-page scrolling layout
# ============================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

MATCH_DIR <- file.path("data", "matches")

# ---------- Workbook loading ----------

read_match_file <- function(path) {
  tryCatch({

    sheets <- excel_sheets(path)

    required_sheets <- c(
      "Match Information",
      "Key Metrics",
      "By Skill",
      "Attack Detail",
      "Rotation Details",
      "Setting Stats",
      "Passing by Contact Point"
    )

    missing_sheets <- setdiff(required_sheets, sheets)

    if (length(missing_sheets)) {
      stop(
        paste(
          "Missing worksheet(s):",
          paste(missing_sheets, collapse = ", ")
        )
      )
    }

    info <- read_excel(
      path,
      sheet = "Match Information",
      col_names = FALSE
    )

    get_info <- function(label) {
      hit <- which(trimws(as.character(info[[1]])) == label)

      if (!length(hit)) return(NA_character_)

      value <- info[[2]][hit[1]]

      if (!length(value) || is.na(value)) {
        return(NA_character_)
      }

      as.character(value)
    }

    raw_date <- get_info("Match date")
    match_date <- suppressWarnings(as.Date(raw_date))

    if (is.na(match_date)) {
      match_date <- suppressWarnings(
        as.Date(raw_date, format = "%m/%d/%Y")
      )
    }

    opponent <- get_info("Opponent")
    result <- get_info("Match result")

    if (is.na(opponent) || !nzchar(opponent)) {
      opponent <- tools::file_path_sans_ext(basename(path))
    }

    # Key Metrics
    key_raw <- read_excel(
      path,
      sheet = "Key Metrics",
      col_names = FALSE
    )

    key <- tibble(
      metric = as.character(key_raw[[2]]),
      value = key_raw[[3]]
    ) %>%
      filter(
        !is.na(metric),
        !is.na(value),
        nzchar(trimws(metric))
      ) %>%
      mutate(metric = trimws(metric))

    # By Skill
    skill_raw <- read_excel(
      path,
      sheet = "By Skill",
      col_names = FALSE
    )

    skill_rows <- list()
    section <- NA_character_

    for (i in seq_len(nrow(skill_raw))) {
      a <- skill_raw[[1]][i]
      b <- skill_raw[[2]][i]
      c <- skill_raw[[3]][i]

      if (!is.na(a) && is.na(b) && is.na(c)) {
        section <- as.character(a)
      } else if (!is.na(b) && !is.na(c) && !is.na(section)) {
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
    attack <- read_excel(path, sheet = "Attack Detail")

    if ("Attack Type" %in% names(attack)) {
      attack <- attack %>%
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

    names(rotation) <- make.unique(names(rotation))

    if ("Rotation" %in% names(rotation)) {
      rotation <- rotation %>%
        filter(
          !is.na(Rotation),
          grepl("^Ro[1-6]$", as.character(Rotation))
        )
    }

    # Setting Stats
    setting <- read_excel(
      path,
      sheet = "Setting Stats",
      col_names = FALSE
    )

    headers <- which(
      as.character(setting[[1]]) == "Who Sets"
    )

    fb_sets <- tibble()
    trans_sets <- tibble()

    if (length(headers) >= 1) {
      h <- headers[1]
      end_row <- min(h + 3, nrow(setting))

      if (h < nrow(setting)) {
        fb_sets <- setting[(h + 1):end_row, 1:8]
        names(fb_sets) <- make.unique(
          as.character(setting[h, 1:8])
        )
      }
    }

    if (length(headers) >= 2) {
      h <- headers[2]
      end_row <- min(h + 3, nrow(setting))

      if (h < nrow(setting)) {
        trans_sets <- setting[(h + 1):end_row, 1:8]
        names(trans_sets) <- make.unique(
          as.character(setting[h, 1:8])
        )
      }
    }

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
      passing <- passing %>%
        fill(all_of(fill_cols), .direction = "down")
    }

    for (nm in intersect(
      c("Total Pass", "Good Pass", "Aced"),
      names(passing)
    )) {
      passing[[nm]] <- suppressWarnings(
        as.numeric(passing[[nm]])
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
      opponent = tools::file_path_sans_ext(basename(path)),
      result = NA_character_,
      key = tibble(metric = character(), value = numeric()),
      skills = tibble(
        skill = character(),
        metric = character(),
        value = numeric()
      ),
      attack = tibble(),
      rotation = tibble(),
      fb_sets = tibble(),
      trans_sets = tibble(),
      passing = tibble(),
      error = conditionMessage(e)
    )
  })
}

load_matches <- function() {
  if (!dir.exists(MATCH_DIR)) return(list())

  files <- list.files(
    MATCH_DIR,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  files <- files[!grepl("^~\\$", basename(files))]

  if (!length(files)) return(list())

  matches <- lapply(files, read_match_file)

  matches <- Filter(
    function(m) is.null(m$error),
    matches
  )

  if (length(matches)) {
    dates <- vapply(
      matches,
      function(m) {
        if (is.na(m$date)) -Inf else as.numeric(m$date)
      },
      numeric(1)
    )

    matches <- matches[order(dates, decreasing = TRUE)]
  }

  matches
}

# ---------- Formatting helpers ----------

fmt_value <- function(metric, value) {
  if (!length(value) || is.na(value)) return("—")

  value <- suppressWarnings(as.numeric(value))

  if (is.na(value)) return("—")

  if (grepl(
    "%|Eff|SO|In-System|Aced|Point Scoring|Conversion|Block Impact|First-Ball|First Ball|Opponent Attack",
    metric,
    ignore.case = TRUE
  )) {
    return(paste0(round(value * 100, 1), "%"))
  }

  as.character(round(value, 2))
}

metric_value <- function(match, metric) {
  row <- match$key %>%
    filter(.data$metric == metric)

  if (!nrow(row)) return(NA_real_)

  suppressWarnings(as.numeric(row$value[1]))
}

metric_card <- function(label, value, note = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    if (!is.null(note)) {
      div(class = "metric-note", note)
    }
  )
}

section_header <- function(title, subtitle = "") {
  div(
    class = "page-head",
    h2(title),
    p(subtitle)
  )
}

empty_message <- function(message) {
  div(class = "empty-state", message)
}

# ---------- UI ----------

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#17365D",
    secondary = "#5B9BD5",
    bg = "#EEF2F5",
    fg = "#203447"
  ),

  tags$head(
    tags$style(HTML("
      body {
        background: #EEF2F5;
        color: #203447;
        font-family: Arial, sans-serif;
      }

      .topbar {
        background: #FFFFFF;
        border: 1px solid #DFE7EF;
        border-radius: 18px;
        padding: 25px 30px;
        margin: 8px 0 18px;
        box-shadow: 0 4px 18px rgba(16,42,67,.05);
      }

      .brand {
        font-size: 30px;
        font-weight: 900;
        letter-spacing: .6px;
        color: #17365D;
      }

      .brand span {
        color: #5B9BD5;
      }

      .brand-subtitle {
        color: #70849A;
        font-size: 12px;
        letter-spacing: 2px;
        margin-top: 5px;
      }

      .filter-card {
        background: #FFFFFF;
        border: 1px solid #DFE7EF;
        border-radius: 15px;
        padding: 16px 20px;
        margin-bottom: 20px;
      }

      .filter-row {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        align-items: center;
      }

      .filter-row .form-group {
        margin: 0;
      }

      .page-head {
        background: linear-gradient(110deg, #102A43, #174A78);
        color: white;
        border-radius: 15px;
        padding: 22px 26px;
        margin: 26px 0 16px;
      }

      .page-head h2 {
        margin: 0;
        font-size: 28px;
        font-weight: 800;
        letter-spacing: .5px;
      }

      .page-head p {
        margin: 7px 0 0;
        color: #D8E9F8;
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(175px, 1fr));
        gap: 12px;
        margin: 12px 0 18px;
      }

      .metric-card {
        background: #FFFFFF;
        border: 1px solid #DCE5EE;
        border-radius: 13px;
        padding: 16px;
        min-height: 108px;
        box-shadow: 0 2px 8px rgba(16,42,67,.035);
      }

      .metric-label {
        color: #70849A;
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .5px;
      }

      .metric-value {
        color: #17365D;
        font-size: 29px;
        font-weight: 800;
        margin-top: 9px;
      }

      .metric-note {
        color: #70849A;
        font-size: 11px;
        margin-top: 4px;
      }

      .panel-card {
        background: #FFFFFF;
        border: 1px solid #DCE5EE;
        border-radius: 14px;
        padding: 20px;
        margin-bottom: 16px;
        overflow-x: auto;
      }

      .section-title {
        color: #17365D;
        font-weight: 800;
        margin: 2px 0 14px;
      }

      .small-muted {
        color: #70849A;
        font-size: 12px;
      }

      .empty-state {
        background: #F7F9FB;
        border-radius: 12px;
        padding: 22px;
        color: #70849A;
      }

      .status-error {
        background: #FFF4F2;
        color: #A33131;
        padding: 12px 15px;
        border-radius: 10px;
        overflow-wrap: anywhere;
      }

      .status-success {
        background: #EDF7F1;
        color: #216C47;
        padding: 10px 14px;
        border-radius: 10px;
      }

      .btn-primary {
        background: #17365D;
        border-color: #17365D;
      }

      table {
        font-size: 13px;
      }

      .footer {
        color: #70849A;
        font-size: 11px;
        padding: 20px 10px;
      }
    "))
  ),

  # Header
  div(
    class = "topbar",
    div(
      class = "brand",
      HTML("HUSKIES <span>COACHES DASHBOARD</span>")
    ),
    div(
      class = "brand-subtitle",
      "KEYANO HUSKIES · ACAC · 2026–27"
    )
  ),

  # Match filters
  div(
    class = "filter-card",
    h4(class = "section-title", "Match Selection"),

    fluidRow(
      column(
        4,
        selectInput(
          "period",
          "Match window",
          choices = c(
            "Most Recent Match",
            "Previous Match",
            "Last 5",
            "Last 10",
            "Full Season"
          ),
          selected = "Most Recent Match"
        )
      ),
      column(8, uiOutput("match_picker"))
    ),

    uiOutput("data_status")
  ),

  # All sections are stacked vertically
  uiOutput("overview_ui"),
  uiOutput("skill_ui"),
  uiOutput("attack_ui"),
  uiOutput("rotation_ui"),
  uiOutput("setting_ui"),
  uiOutput("passing_ui"),

  div(
    class = "footer",
    "KEYANO HUSKIES · ACAC · 2026–27 · Internal performance analysis"
  )
)

# ---------- Server ----------

server <- function(input, output, session) {

  matches <- reactive({
    invalidateLater(60000, session)
    load_matches()
  })

  selected_matches <- reactive({
    ms <- matches()
    req(length(ms) > 0)

    n <- length(ms)

    if (input$period == "Most Recent Match") {
      ms[1]
    } else if (input$period == "Previous Match") {
      validate(need(n >= 2, "Upload at least two match workbooks."))
      ms[2]
    } else if (input$period == "Last 5") {
      ms[seq_len(min(5, n))]
    } else if (input$period == "Last 10") {
      ms[seq_len(min(10, n))]
    } else {
      ms
    }
  })

  current_match <- reactive({
    ms <- selected_matches()

    if (
      !is.null(input$match_file) &&
      input$period %in% c("Most Recent Match", "Previous Match")
    ) {
      found <- which(vapply(
        ms,
        function(m) m$file == input$match_file,
        logical(1)
      ))

      if (length(found)) return(ms[[found[1]]])
    }

    ms[[1]]
  })

  # Detailed data status
  output$data_status <- renderUI({
    files <- if (dir.exists(MATCH_DIR)) {
      list.files(
        MATCH_DIR,
        pattern = "\\.xlsx$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    } else {
      character()
    }

    files <- files[!grepl("^~\\$", basename(files))]
    ms <- matches()

    if (length(ms)) {
      return(
        div(
          class = "status-success",
          paste(length(ms), "match workbook(s) loaded successfully.")
        )
      )
    }

    if (!dir.exists(MATCH_DIR)) {
      return(
        div(
          class = "status-error",
          strong("Match folder not found."),
          p("Expected folder: data/matches/")
        )
      )
    }

    if (!length(files)) {
      return(
        div(
          class = "status-error",
          strong("No Excel workbooks found."),
          p("Add your completed .xlsx match workbooks to data/matches/.")
        )
      )
    }

    errors <- lapply(files, function(f) {
      tryCatch({
        sheets <- excel_sheets(f)
        paste0(
          basename(f),
          ": Excel file opens successfully. Sheets: ",
          paste(sheets, collapse = ", "),
          ". The workbook parser needs further checking."
        )
      }, error = function(e) {
        paste0(
          basename(f),
          ": ",
          conditionMessage(e)
        )
      })
    })

    div(
      class = "status-error",
      strong("Workbook files were found, but none could be loaded."),
      tags$ul(
        lapply(errors, tags$li)
      )
    )
  })

  output$match_picker <- renderUI({
    ms <- matches()

    if (!length(ms)) {
      return(helpText("No readable match files available yet."))
    }

    if (!(input$period %in% c("Most Recent Match", "Previous Match"))) {
      return(NULL)
    }

    labels <- vapply(ms, function(m) {
      date_txt <- if (is.na(m$date)) {
        "Date unknown"
      } else {
        format(m$date, "%b %d, %Y")
      }

      paste(date_txt, "—", m$opponent)
    }, character(1))

    choices <- stats::setNames(
      vapply(ms, function(m) m$file, character(1)),
      labels
    )

    selectInput(
      "match_file",
      "Choose match",
      choices = choices,
      selected = ms[[1]]$file
    )
  })

  # ---------- Overview ----------

  output$overview_ui <- renderUI({
    ms <- selected_matches()
    m <- current_match()

    date_txt <- if (is.na(m$date)) {
      "Date not entered"
    } else {
      format(m$date, "%B %d, %Y")
    }

    metric_names <- c(
      "Side-Out %",
      "First-Ball SO %",
      "Point Scoring %",
      "In-System %",
      "Attack Efficiency",
      "Block Impact %",
      "Conversion %",
      "Opponent Attack Eff"
    )

    cards <- lapply(metric_names, function(lbl) {
      metric_card(
        lbl,
        fmt_value(lbl, metric_value(m, lbl)),
        "Selected match"
      )
    })

    tagList(
      section_header(
        "MATCH PERFORMANCE",
        paste(
          date_txt, "·", m$opponent, "·",
          ifelse(is.na(m$result), "Result not entered", m$result)
        )
      ),

      div(class = "metric-grid", cards),

      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            h4(class = "section-title", "Keyano at a glance"),
            p(strong("Match window: "), input$period),
            p(strong("Matches included: "), length(ms)),
            p(
              class = "small-muted",
              "Metric cards reflect the selected reference match."
            )
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            h4(class = "section-title", "Set score"),
            uiOutput("score_ui")
          )
        )
      ),

      div(
        class = "panel-card",
        h4(class = "section-title", "Key metric trend across included matches"),
        plotOutput("trend_plot", height = 300)
      )
    )
  })

  output$score_ui <- renderUI({
    m <- current_match()

    info <- read_excel(
      m$path,
      sheet = "Match Information",
      col_names = FALSE
    )

    row <- which(as.character(info[[1]]) == "Sets won and lost")

    if (!length(row)) return(p("Set score not entered."))

    keyano <- suppressWarnings(
      as.numeric(unlist(info[row[1], 2:4], use.names = FALSE))
    )

    opp <- suppressWarnings(
      as.numeric(unlist(
        info[min(row[1] + 1, nrow(info)), 2:4],
        use.names = FALSE
      ))
    )

    keep <- !is.na(keyano) & !is.na(opp)
    keyano <- keyano[keep]
    opp <- opp[keep]

    tagList(
      div(
        style = "display:flex;justify-content:space-between;gap:18px;align-items:center;flex-wrap:wrap;",
        div(
          h5("KEYANO", style = "font-weight:800;color:#17365D;"),
          h2(sum(keyano > opp), style = "font-weight:900;")
        ),
        div(
          h5("SET SCORES", style = "font-weight:800;color:#70849A;"),
          p(
            paste(paste0(keyano, "–", opp), collapse = " · "),
            style = "font-weight:700;font-size:18px;"
          )
        ),
        div(
          h5("OPPONENT", style = "font-weight:800;color:#70849A;"),
          h2(sum(opp > keyano), style = "font-weight:900;")
        )
      )
    )
  })

  output$trend_plot <- renderPlot({
    ms <- selected_matches()

    wanted <- c(
      "Side-Out %",
      "First-Ball SO %",
      "Point Scoring %",
      "In-System %",
      "Attack Efficiency",
      "Block Impact %"
    )

    dat <- bind_rows(lapply(ms, function(m) {
      tibble(
        Match = if (is.na(m$date)) {
          m$opponent
        } else {
          paste(format(m$date, "%b %d"), m$opponent)
        },
        Metric = wanted,
        Value = vapply(
          wanted,
          function(x) metric_value(m, x),
          numeric(1)
        )
      )
    })) %>%
      filter(!is.na(Value))

    validate(need(
      nrow(dat) > 0,
      "No trend metrics available in the selected match window."
    ))

    dat$Metric <- factor(dat$Metric, levels = wanted)

    ggplot(
      dat,
      aes(
        x = Match,
        y = Value,
        group = Metric,
        color = Metric
      )
    ) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2.5) +
      scale_y_continuous(
        labels = function(x) paste0(round(x * 100), "%")
      ) +
      labs(x = NULL, y = "Rate", color = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })

  # ---------- By Skill ----------

  output$skill_ui <- renderUI({
    m <- current_match()

    tagList(
      section_header(
        "BY SKILL",
        paste("Performance breakdown ·", m$opponent)
      ),
      div(
        class = "panel-card",
        plotOutput("skill_plot", height = 320)
      ),
      div(
        class = "panel-card",
        h4(class = "section-title", "Skill breakdown"),
        tableOutput("skill_table")
      )
    )
  })

  output$skill_plot <- renderPlot({
    d <- current_match()$skills %>%
      filter(!is.na(value))

    validate(need(nrow(d) > 0, "No By Skill data found."))

    ggplot(
      d,
      aes(x = reorder(metric, value), y = value, fill = skill)
    ) +
      geom_col() +
      coord_flip() +
      facet_wrap(~skill, scales = "free_y") +
      labs(x = NULL, y = "Count / workbook value", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank()
      )
  })

  output$skill_table <- renderTable({
    current_match()$skills %>%
      filter(!is.na(value)) %>%
      rename(
        Skill = skill,
        Metric = metric,
        Value = value
      )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------- Attack Detail ----------

  output$attack_ui <- renderUI({
    tagList(
      section_header(
        "ATTACK DETAIL",
        "First-ball and transition attack by attack type."
      ),
      div(
        class = "panel-card",
        plotOutput("attack_plot", height = 360)
      ),
      div(
        class = "panel-card",
        tableOutput("attack_table")
      )
    )
  })

  output$attack_plot <- renderPlot({
    d <- current_match()$attack

    validate(need(
      all(c("Attempts", "Attack Type") %in% names(d)),
      "Required attack columns are missing."
    ))

    d <- d %>% filter(!is.na(Attempts), Attempts > 0)

    validate(need(nrow(d) > 0, "No attack attempts found."))

    if (!"Previous Play" %in% names(d)) {
      d$`Previous Play` <- "All"
    }

    ggplot(
      d,
      aes(x = `Attack Type`, y = Attempts, fill = `Previous Play`)
    ) +
      geom_col(position = "dodge") +
      labs(x = NULL, y = "Attempts", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
  })

  output$attack_table <- renderTable({
    d <- current_match()$attack

    cols <- intersect(
      c(
        "Attack Type", "Previous Play", "Attempts", "Kills",
        "Error", "FB Eff.", "Transition Eff.", "Total Eff."
      ),
      names(d)
    )

    d %>% select(all_of(cols))
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------- Rotation Details ----------

  output$rotation_ui <- renderUI({
    tagList(
      section_header(
        "ROTATION DETAILS",
        "Serve, reception and attack performance across rotations."
      ),
      div(
        class = "panel-card",
        plotOutput("rotation_plot", height = 330)
      ),
      div(
        class = "panel-card",
        tableOutput("rotation_table")
      )
    )
  })

  output$rotation_plot <- renderPlot({
    d <- current_match()$rotation

    validate(need(
      "Rotation" %in% names(d),
      "Rotation data is not available."
    ))

    rate_col <- intersect(c("SO%", "PS%", "H %"), names(d))

    validate(need(
      length(rate_col) > 0,
      "No rotation rate columns found."
    ))

    dd <- d %>%
      select(Rotation, all_of(rate_col)) %>%
      pivot_longer(
        -Rotation,
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      mutate(Value = suppressWarnings(as.numeric(Value))) %>%
      filter(!is.na(Value))

    validate(need(nrow(dd) > 0, "No rotation values available."))

    ggplot(dd, aes(x = Rotation, y = Value, fill = Metric)) +
      geom_col(position = "dodge") +
      scale_y_continuous(
        labels = function(x) paste0(round(x * 100), "%")
      ) +
      labs(x = NULL, y = "Rate", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$rotation_table <- renderTable({
    current_match()$rotation
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------- Setting Stats ----------

  output$setting_ui <- renderUI({
    tagList(
      section_header(
        "SETTING STATS",
        "Who sets the ball and what happens after the set."
      ),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            h4(class = "section-title", "First-ball sets"),
            tableOutput("fb_table")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            h4(class = "section-title", "Transition sets"),
            tableOutput("trans_table")
          )
        )
      )
    )
  })

  output$fb_table <- renderTable({
    current_match()$fb_sets
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$trans_table <- renderTable({
    current_match()$trans_sets
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------- Passing by Contact Point ----------

  output$passing_ui <- renderUI({
    tagList(
      section_header(
        "PASSING BY CONTACT POINT",
        "Reception outcomes by player and contact point."
      ),
      div(
        class = "panel-card",
        selectInput("pass_player", "Player", choices = NULL)
      ),
      div(
        class = "panel-card",
        plotOutput("passing_plot", height = 330)
      ),
      div(
        class = "panel-card",
        tableOutput("passing_table")
      )
    )
  })

  observeEvent(current_match(), {
    d <- current_match()$passing

    if (
      all(c("Player No.", "Player Name") %in% names(d))
    ) {
      players <- unique(
        paste0(d$`Player No.`, " · ", d$`Player Name`)
      )

      updateSelectInput(
        session,
        "pass_player",
        choices = players,
        selected = if (length(players)) players[1] else character(0)
      )
    }
  }, ignoreInit = FALSE)

  passing_filtered <- reactive({
    d <- current_match()$passing

    req(input$pass_player)

    validate(need(
      all(c("Player No.", "Player Name") %in% names(d)),
      "Player columns are missing from the passing worksheet."
    ))

    d %>%
      filter(
        paste0(`Player No.`, " · ", `Player Name`) == input$pass_player
      )
  })

  output$passing_plot <- renderPlot({
    d <- passing_filtered()

    validate(need(
      all(c("Contact Point", "Total Pass") %in% names(d)),
      "Contact Point or Total Pass columns are missing."
    ))

    d <- d %>% filter(!is.na(`Total Pass`))

    validate(need(
      nrow(d) > 0,
      "No passing data found for this player."
    ))

    ggplot(d, aes(x = `Contact Point`, y = `Total Pass`)) +
      geom_col(fill = "#2F75B5") +
      geom_text(
        aes(label = `Total Pass`),
        vjust = -0.35,
        fontface = "bold"
      ) +
      labs(x = NULL, y = "Total Pass") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid.minor = element_blank()
      )
  })

  output$passing_table <- renderTable({
    d <- passing_filtered()

    cols <- intersect(
      c(
        "Player No.", "Player Name", "Position",
        "Contact Point", "Total Pass", "Good Pass",
        "Aced", "GP %", "Aced %"
      ),
      names(d)
    )

    d %>% select(all_of(cols))
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui, server)
