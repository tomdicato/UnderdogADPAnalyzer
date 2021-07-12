library(shiny)
library(reactable)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(here)
library(httr)
library(shinycssloaders)
library(bslib)
library(shinyjs)
library(shinyvalidate)
source(paste0(here(), "/ud_adp_helpers.R"))

x <- GET(
  "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_current.rds",
  authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
)

### THANK YOU TAN ###

ud_player_list <-
  content(x, as = "raw") %>%
  parse_raw_rds() %>%
  arrange(adp) %>%
  transmute(player_name = paste(firstName, lastName), id = id)

named_player_list <-
  deframe(ud_player_list)

nfl_team <- get_nfl_teams() %>%
  select(full_name, team_nickname, team_name, team_short_name, logo) %>%
  mutate(logo = paste0(
    "<img src='", logo, "' width=30px><div class='jhr'>",
    full_name, "</div></img>"
  )) %>%
  arrange(full_name)

shinyUI(
  fluidPage(
    tags$head(tags$style("
                       .jhr{
                       display: inline;
                       vertical-align: middle;
                       padding-left: 10px;
                       }")),
    theme = bslib::bs_theme(bootswatch = "simplex"),
    titlePanel("UD Best Ball Mania II"),
    sidebarLayout(
      sidebarPanel(
        tags$style(".well {background-color:transparent;
                    border-color:transparent;}"),
        actionButton("upload_csv",
                     "Upload Exposure"),
        hidden(
        pickerInput(
          inputId = "player_search",
          label = "Show teams with:",
          choices = named_player_list,
          options = pickerOptions(
            actionsBox = TRUE,
            size = 10,
            liveSearch = TRUE,
            noneSelectedText = "---"
          ),
          multiple = TRUE
        )),
        shinyjs::useShinyjs(),
        downloadButton("data_file", label = ".csv export"),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          id = "user_upload_tabs",
          type = "pills",
          tabPanel(
            "UD In Draft",
            fluidRow(
              column(
                4,
                shiny::numericInput("user_filter_adp", value = 12,
                                    label = "Your Next Pick",
                                    min = 1,
                                    max = 217,
                                    width = "150px")
                ,offset = 2
              ),
              column(
                4,
                selectInput(
                  "current_draft_stack_position",
                  "Position(s)",
                  choices = c("QB", "RB", "WR", "TE"),
                  multiple = TRUE,
                  selectize = TRUE
                )
              ),
              pickerInput(
                inputId = "current_draft_teams",
                label = NULL,
                choices = nfl_team %>%
                  mutate(team_name = ifelse(is.na(team_name), "Football Team", team_name)) %>%
                  select(full_name, team_short_name) %>%
                  deframe(),
                choicesOpt = list(content = nfl_team$logo),
                options = pickerOptions(
                  size = 10,
                  liveSearch = TRUE,
                  noneSelectedText = "Select Your Stacks",
                  multipleSeparator = " | "
                ),
                multiple = TRUE,
                width = "100%"
              ),
              reactableOutput("in_draft_helper_table"),
              plotOutput("in_draft_helper_plot")
            )
          ),
          tabPanel(
            "UD ADP",
            sliderInput("ud_adp_slider",
              label = "ADP Range",
              min = 0,
              max = 216,
              value = c(0, 216),
              step = 1,
              ticks = FALSE,
              round = TRUE
            ),
            shinycssloaders::withSpinner(
              reactableOutput("ud_adp"),
              type = 7,
              color = "#D9230F"
            )
          )
        )
      )
    )
  )
)
