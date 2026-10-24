# Huskies Coaches Dashboard
# Single-page, vertically scrolling layout

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---------- Paths and helpers ----------

MATCH_DIR <- file.path("data", "matches")
MATCH_ERRORS <- character()

find_match_files <- function() {
  folders <- unique(c(
    file.path(getwd(), "data", "matches"),
    file.path(".", "data", "matches"),
    "data/matches"
  ))
  folders <- folders[dir.exists(folders)]

  if (!length(folders)) return(character())

  files <- unlist(lapply(folders, function(folder) {
    list.files(
      folder,
      pattern = "\\.xlsx$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  }), use.names = FALSE)

  unique(files[!grepl("^~\\$", basename(files))])
}

read_match_file <- function(path) {
  tryCatch({
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

    match_date <- suppressWarnings(
      as.Date(get_info("Match date"))
    )

    opponent <- get_info("Opponent")
    result <- get_info("Match result")

    if (is.na(opponent) || !nzchar(opponent)) {
      opponent <- tools::file_path_sans_ext(basename(path))
    }

    # Key metrics
    key <- read_excel(
      path,
      sheet = "Key Metrics",
      col_names = FALSE
    )

    key_metrics <- tibble(
      metric = as.character(key[[2]]),
      value = key[[3]]
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
    attack <- read_excel(
      path,
      sheet = "Attack Detail"
    ) %>%
      fill(`Attack Type`, .direction = "down") %>%
      mutate(
        Attempts = suppressWarnings(as.numeric(Attempts)),
        Kills = suppressWarnings(as.numeric(Kills)),
        Error = suppressWarnings(as.numeric(Error))
      )

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

    headers <- which(as.character(setting[[1]]) == "Who Sets")
    fb_sets <- tibble()
    trans_sets <- tibble()

    if (length(headers) >= 1 && !is.na(headers[1])) {
      fb_header <- headers[1]
      fb_end <- min(fb_header + 3, nrow(setting))

      if (fb_header < nrow(setting)) {
        fb_sets <- setting[(fb_header + 1):fb_end, 1:8]
        names(fb_sets) <- as.character(setting[fb_header, 1:8])
      }
    }

    if (length(headers) >= 2 && !is.na(headers[2])) {
      trans_header <- headers[2]
      trans_end <- min(trans_header + 3, nrow(setting))

      if (trans_header < nrow(setting)) {
        trans_sets <- setting[(trans_header + 1):trans_end, 1:8]
        names(trans_sets) <- as.character(setting[trans_header, 1:8])
      }
    }

    # Passing by Contact Point
    passing <- read_excel(
      path,
      sheet = "Passing by Contact Point"
    ) %>%
      fill(
        `Player No.`,
        `Player Name`,
        Position,
        .direction = "down"
      ) %>%
      mutate(
        `Total Pass` = suppressWarnings(as.numeric(`Total Pass`)),
        `Good Pass` = suppressWarnings(as.numeric(`Good Pass`)),
        Aced = suppressWarnings(as.numeric(Aced))
      )

    list(
      path = path,
      file = basename(path),
      date = match_date,
      opponent = opponent,
      result = result,
      key = key_metrics,
      skills = skills,
      attack = attack,
      rotation = rotation,
      fb_sets = fb_sets,
      trans_sets = trans_sets,
      passing = passing
    )

  }, error = function(e) {
    message(
      "Could not read ",
      basename(path),
      ": ",
      conditionMessage(e)
    )

    NULL
  })
}

load_matches <- function() {
  files <- find_match_files()
  MATCH_ERRORS <<- character()

  if (!length(files)) return(list())

  matches <- lapply(files, function(f) {
    result <- read_match_file(f)

    if (is.null(result)) {
      MATCH_ERRORS <<- c(
        MATCH_ERRORS,
        paste0(
          basename(f),
          ": workbook could not be read. Check sheet names and column headers."
        )
      )
    }

    result
  })

  matches <- Filter(Negate(is.null), matches)

  if (length(matches)) {
    dates <- vapply(
      matches,
      function(x) {
        if (is.na(x$date)) -Inf else as.numeric(x$date)
      },
      numeric(1)
    )

    matches <- matches[order(dates, decreasing = TRUE)]
  }

  matches
}

fmt_value <- function(metric, value) {
  if (length(value) == 0 || is.na(value) || !is.numeric(value)) {
    return("—")
  }

  if (grepl(
    "%|Eff|SO|In-System|Aced|Point Scoring|Conversion|Block Impact|First-Ball|First Ball|Opponent Attack",
    metric,
    ignore.case = TRUE
  )) {
    paste0(round(value * 100, 1), "%")
  } else {
    as.character(round(value, 2))
  }
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
    if (!is.null(note)) div(class = "metric-note", note)
  )
}

# ---------- UI: single scrolling page ----------

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#17365D",
    secondary = "#5B9BD5",
    bg = "#F5F7FA",
    fg = "#152238"
  ),

  tags$head(
    tags$style(HTML("
      body {
        background: #F5F7FA;
      }

      .topbar {
        background: #102A43;
        color: white;
        padding: 22px 26px;
        margin: 0 0 18px;
      }

      .brand {
        font-size: 25px;
        font-weight: 800;
        letter-spacing: 1px;
      }

      .brand span {
        color: #8CC8FF;
      }

      .page-head {
        background: linear-gradient(110deg, #102A43, #174A78);
        color: white;
        border-radius: 16px;
        padding: 22px 26px;
        margin: 22px 0 18px;
      }

      .page-head h2 {
        margin: 0;
        font-weight: 800;
        letter-spacing: .5px;
      }

      .page-head p {
        margin: 6px 0 0;
        color: #D8E9F8;
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(155px, 1fr));
        gap: 12px;
        margin: 12px 0 20px;
      }

      .metric-card {
        background: white;
        border: 1px solid #E0E7EF;
        border-radius: 13px;
        padding: 15px 17px;
        box-shadow: 0 2px 8px rgba(16,42,67,.04);
        min-height: 102px;
      }

      .metric-label {
        color: #60758A;
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .6px;
      }

      .metric-value {
        color: #17365D;
        font-size: 28px;
        font-weight: 800;
        margin-top: 7px;
      }

      .metric-note {
        color: #718096;
        font-size: 11px;
        margin-top: 3px;
      }

      .panel-card {
        background: white;
        border: 1px solid #E0E7EF;
        border-radius: 14px;
        padding: 18px;
        margin-bottom: 16px;
      }

      .section-title {
        font-weight: 800;
        color: #17365D;
        margin: 2px 0 12px;
      }

      .small-muted {
        color: #718096;
        font-size: 12px;
      }

      table {
        font-size: 13px;
      }

      .filter-card {
        background: white;
        border: 1px solid #E0E7EF;
        border-radius: 14px;
        padding: 18px;
        margin-bottom: 18px;
      }
    "))
  ),

  div(
    class = "topbar",
    div(
      class = "brand",
      HTML("HUSKIES <span>COACHES DASHBOARD</span>")
    ),
    div(
      style = "color:#D8E9F8;margin-top:5px;",
      "KEYANO HUSKIES · ACAC · 2026–27"
    )
  ),

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

    helpText(
      "Completed match workbooks should be stored in data/matches/."
    ),

    uiOutput("data_status")
  ),

  # Every section appears vertically in this order.
  uiOutput("overview_ui"),
  uiOutput("skill_ui"),
  uiOutput("attack_ui"),
  uiOutput("rotation_ui"),
  uiOutput("setting_ui"),
  uiOutput("passing_ui"),

  div(
    style = "padding:18px 24px;color:#718096;font-size:11px;",
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

    period <- input$period
    n <- length(ms)

    if (period == "Most Recent Match") {
      ms[1]
    } else if (period == "Previous Match") {
      validate(
        need(
          n >= 2,
          "Upload at least two match workbooks to view the previous match."
        )
      )
      ms[2]
    } else if (period == "Last 5") {
      ms[seq_len(min(5, n))]
    } else if (period == "Last 10") {
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

  # Match-loading diagnostic
  output$data_status <- renderUI({
    ms <- matches()

    if (length(ms) > 0) {
      return(
        div(
          style = "color:#16734A;font-weight:600;",
          paste(length(ms), "match workbook(s) loaded successfully.")
        )
      )
    }

    files <- find_match_files()

    if (!length(files)) {
      return(
        div(
          style = "color:#A33A38;",
          strong("No match workbooks were found by the running app."),
          p(
            "Check that the .xlsx files are committed to GitHub under data/matches/ and included in the deployment."
          )
        )
      )
    }

    div(
      style = "color:#A33A38;",
      strong("Workbook files were found, but none could be read."),
      p(paste(MATCH_ERRORS, collapse = " | "))
    )
  })

  output$match_picker <- renderUI({
    ms <- matches()

    if (!length(ms)) {
      return(helpText("No readable match files yet. See the status message above."))
    }

    if (!(input$period %in% c("Most Recent Match", "Previous Match"))) {
      return(NULL)
    }

    choices <- vapply(
      ms,
      function(m) {
        date_txt <- if (is.na(m$date)) {
          "Date unknown"
        } else {
          format(m$date, "%b %d, %Y")
        }

        paste(date_txt, "—", m$opponent)
      },
      character(1)
    )

    names(choices) <- vapply(
      ms,
      function(m) m$file,
      character(1)
    )

    selectInput(
      "match_file",
      "Choose match",
      choices = stats::setNames(
        vapply(ms, function(m) m$file, character(1)),
        choices
      ),
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

    vals <- c(
      "Side-Out %" = "Side-Out %",
      "First-Ball SO %" = "First-Ball SO %",
      "Point Scoring %" = "Point Scoring %",
      "In-System %" = "In-System %",
      "Attack Efficiency" = "Attack Efficiency",
      "Block Impact %" = "Block Impact %",
      "Conversion %" = "Conversion %",
      "Opponent Attack Eff" = "Opponent Attack Eff"
    )

    cards <- lapply(names(vals), function(lbl) {
      v <- metric_value(m, vals[[lbl]])
      metric_card(lbl, fmt_value(lbl, v))
    })

    tagList(
      div(
        class = "page-head",
        h2("MATCH PERFORMANCE"),
        p(paste(
          date_txt, " · ", m$opponent, " · ",
          ifelse(is.na(m$result), "Result not entered", m$result)
        ))
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
              "For multi-match windows, match-by-match trends are shown below. The metric cards reflect the selected reference match."
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
        style = "display:flex;justify-content:space-between;gap:18px;align-items:center;",
        div(
          h5("KEYANO", style = "font-weight:800;color:#2563EB;"),
          h2(sum(keyano > opp), style = "font-weight:900;")
        ),
        div(
          h5("SET SCORES", style = "font-weight:800;color:#60758A;"),
          p(
            paste(paste0(keyano, "–", opp), collapse = "  ·  "),
            style = "font-weight:700;font-size:18px;"
          )
        ),
        div(
          h5("OPPONENT", style = "font-weight:800;color:#60758A;"),
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
        Date = m$date,
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
      "No trend metrics are available in these match files."
    ))

    dat$Metric <- factor(dat$Metric, levels = wanted)

    ggplot(
      dat,
      aes(
        x = ifelse(
          is.na(Date),
          Match,
          paste(format(Date, "%b %d"), Match)
        ),
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
      div(
        class = "page-head",
        h2("BY SKILL"),
        p(paste(
          m$opponent, "·",
          ifelse(is.na(m$date), "Date unknown", format(m$date, "%b %d, %Y"))
        ))
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
    d <- current_match()$skills

    validate(need(nrow(d) > 0, "No By Skill data found."))

    d <- d %>% filter(!is.na(value))

    ggplot(
      d,
      aes(x = reorder(metric, value), y = value, fill = skill)
    ) +
      geom_col(show.legend = TRUE) +
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
      rename(Skill = skill, Metric = metric, Value = value)
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------- Attack Detail ----------

  output$attack_ui <- renderUI({
    tagList(
      div(
        class = "page-head",
        h2("ATTACK DETAIL"),
        p("First-ball and transition attack by attack type.")
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
    d <- current_match()$attack %>%
      filter(!is.na(Attempts), Attempts > 0)

    validate(need(nrow(d) > 0, "No attack attempts found."))

    d$`Attack Type` <- factor(
      d$`Attack Type`,
      levels = unique(d$`Attack Type`)
    )

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
    current_match()$attack %>%
      select(
        `Attack Type`,
        `Previous Play`,
        Attempts,
        Kills,
        Error,
        `FB Eff.`,
        `Transition Eff.`,
        `Total Eff.`
      )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  # ---------- Rotation Details ----------

  output$rotation_ui <- renderUI({
    tagList(
      div(
        class = "page-head",
        h2("ROTATION DETAILS"),
        p("Serve, reception and attack performance across rotations.")
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
      tidyr::pivot_longer(
        -Rotation,
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      mutate(Value = suppressWarnings(as.numeric(Value))) %>%
      filter(!is.na(Value))

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
      div(
        class = "page-head",
        h2("SETTING STATS"),
        p("Who sets the ball and what happens after the set.")
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
      div(
        class = "page-head",
        h2("PASSING BY CONTACT POINT"),
        p("Reception outcomes by player and contact point.")
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

    players <- unique(
      paste0(d$`Player No.`, " · ", d$`Player Name`)
    )

    updateSelectInput(
      session,
      "pass_player",
      choices = players,
      selected = if (length(players)) players[1] else character(0)
    )
  }, ignoreInit = FALSE)

  passing_filtered <- reactive({
    d <- current_match()$passing

    req(input$pass_player)

    d %>%
      filter(
        paste0(`Player No.`, " · ", `Player Name`) == input$pass_player
      )
  })

  output$passing_plot <- renderPlot({
    d <- passing_filtered() %>%
      filter(!is.na(`Total Pass`))

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
    passing_filtered() %>%
      select(
        `Player No.`,
        `Player Name`,
        Position,
        `Contact Point`,
        `Total Pass`,
        `Good Pass`,
        Aced,
        `GP %`,
        `Aced %`
      )
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui, server)
