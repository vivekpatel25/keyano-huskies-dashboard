# Keyano Huskies Coaches Dashboard
# Beginner note: this file controls the website's layout and interactions.

library(shiny)
library(bslib)
library(readr)
library(dplyr)

# Read the season roster. Only first names are used in the public-facing app.
roster_path <- file.path("data", "metadata", "roster.csv")
roster <- read_csv(roster_path, show_col_types = FALSE) |>
  mutate(
    Jersey = as.character(Jersey),
    FirstName = as.character(FirstName),
    Position = as.character(Position)
  ) |>
  arrange(suppressWarnings(as.numeric(Jersey)))

# These are the match windows shown beneath the header.
match_windows <- c("Most Recent Match", "Previous Match", "Last 5", "Last 10", "Full Season")

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
    tags$title("Huskies Coaches Dashboard | Keyano ACAC 2026–27"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    class = "dashboard-header",
    div(class = "brand-area",
        img(src = "keyano-huskies-logo.png", class = "team-logo", alt = "Keyano Huskies logo"),
        div(class = "brand-copy",
            h1("HUSKIES COACHES DASHBOARD"),
            div(class = "season-label", "KEYANO · ACAC · 2026–27")
        )
    ),
    div(class = "match-summary",
        div(class = "eyebrow", "LATEST MATCH"),
        h3(textOutput("latest_match", inline = TRUE)),
        div(class = "match-detail", textOutput("match_details", inline = TRUE))
    ),
    div(class = "athletes-link",
        actionButton("athletes_nav", label = NULL, icon = icon("grip"), class = "athletes-icon-button"),
        div("ATHLETES", class = "athletes-label")
    )
  ),
  div(class = "period-nav",
      uiOutput("period_buttons")
  ),
  div(class = "content-card",
      uiOutput("page_content")
  ),
  div(class = "footer-note", "Keyano Huskies · ACAC · 2026–27 | Match statistics will appear when game workbooks are connected.")
)

server <- function(input, output, session) {
  selected_period <- reactiveVal("Most Recent Match")
  show_athletes <- reactiveVal(FALSE)

  output$latest_match <- renderText("Match data not connected yet")
  output$match_details <- renderText("Sets · receptions · swings · rallies")

  output$period_buttons <- renderUI({
    div(class = "period-buttons",
        lapply(match_windows, function(period) {
          actionButton(
            inputId = paste0("period_", gsub("[^A-Za-z0-9]", "", period)),
            label = period,
            class = if (identical(selected_period(), period)) "period-button active" else "period-button",
            width = "100%"
          )
        })
    )
  })

  lapply(match_windows, function(period) {
    local({
      this_period <- period
      observeEvent(input[[paste0("period_", gsub("[^A-Za-z0-9]", "", this_period))]], {
        selected_period(this_period)
        show_athletes(FALSE)
      })
    })
  })

  observeEvent(input$athletes_nav, {
    show_athletes(!show_athletes())
  })

  output$page_content <- renderUI({
    if (show_athletes()) {
      tagList(
        h2("Athletes"),
        p(class = "section-intro", "Season roster. Public-facing names use first names only, as requested."),
        textInput("athlete_search", "Search athletes", placeholder = "Type a first name, jersey number, or position..."),
        tableOutput("roster_table")
      )
    } else {
      tagList(
        div(class = "section-heading-row",
            div(h2(selected_period()), p(class = "section-intro", "Team performance overview")),
            span(class = "data-status", "WAITING FOR MATCH DATA")
        ),
        div(class = "metric-grid",
            div(class = "metric-card", div(class = "metric-label", "ATTACK EFFICIENCY"), div(class = "metric-value", "—"), div(class = "metric-note", "Calculated from match data")),
            div(class = "metric-card", div(class = "metric-label", "SIDEOUT %"), div(class = "metric-value", "—"), div(class = "metric-note", "Calculated from match data")),
            div(class = "metric-card", div(class = "metric-label", "MATCHES PLAYED"), div(class = "metric-value", "—"), div(class = "metric-note", "Season to date")),
            div(class = "metric-card", div(class = "metric-label", "LAST UPDATED"), div(class = "metric-value", "—"), div(class = "metric-note", "Updated after workbook import"))
        ),
        div(class = "placeholder-panel",
            h3("Your analytics will appear here"),
            p("The roster and Keyano branding are set up. The next step is mapping your match Excel workbooks so the website can calculate team performance, player statistics, rotation results, and league benchmarks."),
            p(strong("No statistics are fabricated."), " Metrics remain blank until real match data is connected.")
        )
      )
    }
  })

  output$roster_table <- renderTable({
    search <- if (is.null(input$athlete_search)) "" else tolower(input$athlete_search)
    roster |>
      filter(grepl(search, tolower(paste(FirstName, Jersey, Position)))) |>
      rename(`Jersey #` = Jersey, `First Name` = FirstName, Position = Position)
  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s")
}

shinyApp(ui, server)
