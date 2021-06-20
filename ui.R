library(shiny)
library(reactable)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(here)
library(httr)
source(paste0(here(), "/ud_adp_helpers.R"))

x <- GET(
    "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_current.rds",
    authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
)

### THANK YOU TAN ###

ud_player_list <-
    content(x, as = "raw") %>%
    parse_raw_rds() %>%
    transmute( player_name = paste(firstName, lastName),id = id)

named_player_list <-
    deframe(ud_player_list)

shinyUI(
    fluidPage(theme = shinytheme("simplex"),

    # Application title
    titlePanel("UD Best Ball Mania II"),
    
    sidebarLayout(
        
        sidebarPanel(
            tags$style(".well {background-color:transparent;},
                       .well {border-width: 0px;}"),
            tags$style(),
            fileInput(
                "ud_user_adp",
                "Upload CSV",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
            pickerInput(
                inputId = "player_search",
                label = "Find Your Players",
                choices = named_player_list,
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `live-search` = TRUE
                ),
                multiple = TRUE
            ),
            # downloadLink("downloadData", 
            #              "Download"),
            width = 3
            
        ),
        
        mainPanel(
            # textOutput("search_display"),
            # tableOutput("showPAT"),
            tabsetPanel(type = "pills",
                        tabPanel("ADP Comparison", uiOutput("map"),
                                 reactableOutput("adp_table")),
                        tabPanel("Team Breakdown", reactableOutput("team_breakdown"))
        )
    )
)
)
)

