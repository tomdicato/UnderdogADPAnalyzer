library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(fs)
library(reactable)
library(htmltools)
library(httr)
library(bslib)
library(shinyjs)
library(shinyvalidate)

source(paste0(here(), "/ud_adp_helpers.R"))

trace(grDevices:::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)


### Get user ADP ###

## Used for local testing ##
 # user_data_directory <-
 #   str_replace_all(paste0(here(), "/user_data"), "/", "\\\\")
 # 
 # user_adp_file <-
 #   list.files(user_data_directory) %>%
 #   as_tibble() %>%
 #   filter(endsWith(value, ".csv")) %>%
 #   slice(1) %>%
 #   .$value

## FIX FLEX ALWAYS BEING A TE

server <- function(input, output, session) {
  
  observeEvent(input$upload_csv, {
    showModal(modalDialog(
      fileInput(
        "ud_user_adp",
        "Upload CSV",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      title = "Upload UD Exposure",
      # tags$href(href="https://www.underdogfantasy.com", "Underdog"),
      tags$img(src = "UD_Export_Helper_1.jpg"),
      p(),
      tags$img(src = "UD_Export_Helper_2.jpg"),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$ud_user_adp, {
    showElement(id = "player_search")
  })

  user_upload <- reactive({
    input$ud_user_adp
  })

  output$map <- renderUI({
    uu <- user_upload()

    if (!is.null(uu)) {
      return(NULL)
    }
    h3(
      "How to export your exposure .csv",
      p(),
      tags$img(src = "UD_Export_Helper_1.jpg"),
      p(),
      tags$img(src = "UD_Export_Helper_2.jpg"),
      p(),
      h5("Get it from your email and upload it here!")
    )
  })

  observe({
    if (!is.null(user_upload())) {
      appendTab(inputId = "user_upload_tabs", tabPanel(
        title = "ADP Comparison",
        value = "adp_comp",
        uiOutput("map"),
        shinycssloaders::withSpinner(
          reactableOutput("user_adp_table"),
          type = 7,
          color = "#D9230F",
          hide.ui = TRUE
        )
      ))
      appendTab(
        inputId = "user_upload_tabs",
        tabPanel(
          "Team by Team",
          shinycssloaders::withSpinner(
            reactableOutput("team_breakdown"),
            type = 7,
            color = "#D9230F"
          )
        )
      )
      appendTab(
        inputId = "user_upload_tabs",
        tabPanel(
          "Team Builds",
          shinycssloaders::withSpinner(
            tagList(
              reactableOutput("build_explorer"),
              plotOutput("build_bargraph")
            ),
            type = 7,
            color = "#D9230F"
          )
        )
      )
      appendTab(
        inputId = "user_upload_tabs",
        tabPanel(
          "Draft Slots",
          shinycssloaders::withSpinner(
            plotOutput("draft_slot_frequency"),
            type = 7,
            color = "#D9230F"
          )
        )
      )
    }
  })

  nfl_team <- reactive({
    get_nfl_teams()
  })
  ### Get underdog overall ADP ###

  current_ud_adp <- reactive({
    x <- GET(
      "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_current.rds",
      authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
    )
    ### THANK YOU TAN ###
    current_ud_adp <-
      content(x, as = "raw") %>%
      parse_raw_rds() %>%
      mutate(
        adp = as.double(adp),
        teamName = case_when(
          teamName == "NY Jets" ~ "New York Jets",
          teamName == "NY Giants" ~ "New York Giants",
          teamName == "Washington Football Team" ~ "Washington",
          TRUE ~ teamName
        )
      ) %>%
      left_join(nfl_team(), by = c("teamName" = "full_name"))
  })

  user_adp_table_data <-
    # read_csv(paste0("user_data/DiCatoUDExposure.csv"))
    # read_csv(paste0("user_data/cgudbbm.csv"))
    reactive({
      file <- user_upload()

      req(file)

      ext <- tools::file_ext(file$datapath)

      validate(need(ext == "csv", "Please upload a csv file"))

      read_csv(file$datapath) %>%
        clean_names()
    })

  output$ud_adp <- renderReactable({
    adp_date <- max(current_ud_adp()$adp_date) %>%
      as.Date()

    ud_adp_data <-
      current_ud_adp() %>%
      clean_names() %>%
      mutate(
        player_name = paste(first_name, last_name),
        position_color = case_when(
          slot_name == "QB" ~ "#9647B8",
          slot_name == "RB" ~ "#15967B",
          slot_name == "WR" ~ "#E67E22",
          slot_name == "TE" ~ "#2980B9",
          TRUE ~ ""
        ),
        team_name = ifelse(is.na(team_name), "FA", team_name),
        projected_points = ifelse(is.na(projected_points), 0, projected_points)
      ) %>%
      select(
        player_name,
        position = slot_name,
        team_name,
        adp,
        projected_points,
        position_color,
        logo
      ) %>%
      filter(adp >= input$ud_adp_slider[1] &
        adp <= input$ud_adp_slider[2])

    reactable(ud_adp_data,
      columns = list(
        player_name = colDef(
          name = "Player",
          cell = function(value, index) {
            pos_color <- ud_adp_data$position_color[index]
            pos_color <- if (!is.na(pos_color)) pos_color else "#000000"
            tagList(
              htmltools::div(
                style = list(color = pos_color),
                value
              )
            )
          },
          minWidth = 240
        ),
        position = colDef(name = "Position"),
        team_name = colDef(
          name = "Team",
          cell = function(value, index) {
            image <- img(src = ud_adp_data$logo[index], height = "24px")

            tagList(
              div(style = list(display = "inline-block", alignItems = "center"), image),
              value
            )
          },
          minWidth = 200
        ),
        adp = colDef(
          name = "UD ADP",
          header = function(value, index) {
            note <- div(style = "color: #666", paste("("), adp_date, ")")
            tagList(
              div(title = value, value),
              div(style = list(fontSize = 12), note)
            )
          }, filterable = FALSE
        ),
        projected_points = colDef(
          name = "Underdog<br>Projection",
          html = TRUE,
          filterable = FALSE,
          show = FALSE
        ),
        position_color = colDef(show = FALSE),
        logo = colDef(show = FALSE)
      ),
      filterable = TRUE,
      highlight = TRUE,
      defaultPageSize = 24,
      style = list(fontFamily = "Montserrat")
    )
  })

  output$build_explorer <- renderReactable({
    cg_table() %>%
      ungroup() %>%
      mutate(draft_start_date = format(as.Date(draft_start_date, format = "%Y-%Om-%d %H:%M:%S"), format = "%B %e")) %>%
      group_by(draft, draft_slot) %>%
      summarise(
        QB = sum(position == "QB"),
        RB = sum(position == "RB"),
        WR = sum(position == "WR"),
        TE = sum(position == "TE")
      ) %>%
      ungroup() %>%
      count(QB, WR, RB, TE, name = "build_count", sort = TRUE) %>%
      mutate(build = paste0("QB: ", QB, "\nRB: ", RB, "\nWR: ", WR, "\nTE: ", TE), build_count) %>%
      select(QB:TE, build_count) %>%
      reactable(
        columns = list(
          QB = colDef(
            name = "QB", headerStyle = "color: #9647B8",
            footer = "Total", footerStyle = list(fontWeight = "bold")
          ),
          RB = colDef(name = "RB", headerStyle = "color: #15967B"),
          WR = colDef(name = "WR", headerStyle = "color: #E67E22"),
          TE = colDef(name = "TE", headerStyle = "color: #2980B9"),
          build_count = colDef(
            name = "Count",
            footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return + total.toFixed(0)
      }"), filterable = FALSE,
            footerStyle = list(fontWeight = "bold")
          )
        ),
        filterable = TRUE,
        highlight = TRUE,
        defaultPageSize = 20,
        style = list(fontFamily = "Montserrat"),
        defaultColDef = colDef(align = "center")
      )
  })

  output$build_bargraph <- renderPlot({
    team_build_count <-
      cg_table() %>%
      ungroup() %>%
      mutate(draft_start_date = format(as.Date(draft_start_date, format = "%Y-%Om-%d %H:%M:%S"), format = "%B %e")) %>%
      group_by(draft, draft_number, draft_start_date) %>%
      summarise(
        QB = sum(position == "QB"),
        RB = sum(position == "RB"),
        WR = sum(position == "WR"),
        TE = sum(position == "TE")
      ) %>%
      ungroup() %>%
      count(QB, WR, RB, TE, name = "build_count", sort = TRUE) %>%
      transmute(build = paste0("QB: ", QB, "\nRB: ", RB, "\nWR: ", WR, "\nTE: ", TE), build_count) %>%
      arrange(desc(build_count))

    max_build_count <-
      max(team_build_count$build_count)
    
    

    ggplot(team_build_count, aes(x = reorder(build, -build_count), y = build_count)) +
      geom_bar(stat = "identity", fill = "#393939", color = "#393939") +
      geom_text(
        data = subset(team_build_count, build_count > 1),
        aes(label = build_count), vjust = 1.5, color = "White", family = "Roboto", size = 4
      ) +
      hrbrthemes::theme_ipsum_rc() +
      theme(
        axis.text.x = element_text(angle = 0),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#1B1B1B"),
        panel.grid.minor.y = element_line(colour = "#E0B400"),
        plot.background = element_rect(fill = "#393939"),
        axis.title = element_text(color = "#E0B400"),
        axis.text = element_text(color = "white", hjust = 0.5),
        axis.text.x.bottom = element_text(hjust = .5),
        plot.title = element_text(color = "white", size = 14, hjust = 0.5)
      ) +
      scale_y_continuous(expand = c(.01, 0), limits = c(0, max_build_count * 1.15)) +
      xlab("Build Type") +
      ylab("Count") +
      labs(
        title = "Underdog Best Ball Build Count"
      )
  })

  output$user_adp_table <- renderReactable({
    file <- user_upload()

    req(file)

    ext <- tools::file_ext(file$datapath)

    validate(need(ext == "csv", "Please upload a csv file"))

    adp_date <- max(current_ud_adp()$adp_date) %>%
      as.Date()

    if (!isTruthy(input$player_search)) {
      appearance_list <-
        user_adp_table_data() %>%
        .$appearance
    } else {
      appearance_list <-
        input$player_search
    }

    user_adp_vs_ud_adp <-
      user_adp_table_data() %>%
      clean_names() %>%
      filter(tournament_title == "Best Ball Mania II") %>%
      # filter(appearance %in% appearance_list) %>%
      select(id = appearance, picked_at:position, tournament_title, draft_entry) %>%
      mutate(picked_at = as.POSIXct(picked_at)) %>%
      group_by(draft_entry) %>%
      mutate(draft_start_date = min(picked_at)) %>%
      ungroup() %>%
      arrange(desc(draft_start_date), pick_number) %>%
      left_join(current_ud_adp(), by = c("id" = "id")) %>%
      as_tibble() %>%
      mutate(
        max_adp = max(adp, na.rm = TRUE),
        adp = coalesce(adp, max_adp)
      ) %>%
      mutate(
        adp_delta = pick_number - adp,
        player_name = paste(firstName, lastName)
      ) %>%
      select(-c(slotName, lineupStatus, byeWeek, teamName, first_name, last_name, firstName, lastName), pick_datetime = picked_at) %>%
      relocate(player_name, position, pick_number, adp, adp_delta, pick_datetime) %>%
      mutate(
        team_pos = paste0(team, "-", position),
        position_color = case_when(
          position == "QB" ~ "#9647B8",
          position == "RB" ~ "#15967B",
          position == "WR" ~ "#E67E22",
          position == "TE" ~ "#2980B9",
          TRUE ~ ""
        ),
        draft_count = n_distinct(draft_start_date)
      ) %>%
      arrange(desc(draft_start_date), pick_datetime)

    parent_data <-
      user_adp_vs_ud_adp %>%
      select(-pick_datetime) %>%
      group_by(id, player_name, position, adp_date, position_color) %>%
      summarise(
        pick_number = mean(pick_number),
        pick_count = n(),
        exposure = n() / draft_count,
        adp = max(adp),
        adp_delta = pick_number - adp,
        projectedPoints = max(projectedPoints),
        team_pos = max(team_pos),
        player_name = max(paste0(player_name, " (", pick_count, ")"))
      ) %>%
      ungroup() %>%
      distinct() %>%
      # relocate(pick_count, .after = player_name) %>%
      arrange(desc(pick_count))

    reactable(parent_data,
      columns = list(
        id = colDef(show = FALSE),
        team_pos = colDef(show = FALSE),
        player_name = colDef(
          name = "Player (Pick Count)",
          # show team under player name
          cell = function(value, index) {
            team_pos <- parent_data$team_pos[index]
            team_pos <- if (!is.na(team_pos)) team_pos else "Unknown"
            position_color <- parent_data$position_color[index]
            position_color <- if (!is.na(position_color)) position_color else "#000000"
            tagList(
              div(style = list(fontWeight = 600), value),
              div(style = list(
                fontSize = 12,
                color = position_color
              ), team_pos)
            )
          },
          align = "left",
          minWidth = 200
        ),
        pick_number = colDef(name = "Avg. ADP", format = colFormat(digits = 1)),
        exposure = colDef(
          name = "Exposure",
          format = colFormat(percent = TRUE, digits = 0)
        ),
        pick_count = colDef(show = FALSE),
        position = colDef(show = FALSE),
        adp_date = colDef(show = FALSE),
        projectedPoints = colDef(name = "Projected Pts."),
        position_color = colDef(show = FALSE),
        adp = colDef(name = "UD ADP", header = function(value, index) {
          note <- div(style = "color: #666", paste("("), adp_date, ")")
          tagList(
            div(title = value, value),
            div(style = list(fontSize = 12), note)
          )
        }),
        adp_delta = colDef(
          name = "Pick Value vs. ADP", format = colFormat(digits = 1),
          style = function(value) {
            if (value > 0) {
              color <- "#008000"
            } else if (value < 0) {
              color <- "#e00000"
            } else if (value == 0) {
              color <- "#77777"
            }
            list(color = color, fontWeight = "bold")
          }
        )
      ),
      details = function(index) {
        player_details <- user_adp_vs_ud_adp[user_adp_vs_ud_adp$id == parent_data$id[index], ]

        htmltools::div(
          style = "padding: 16px",
          reactable(player_details,
            columns = list(
              pick_number = colDef(name = "Pick"),
              adp = colDef(name = "Current ADP"),
              uid = colDef(show = FALSE),
              team_name = colDef(name = "Team"),
              team_short_name = colDef(show = FALSE),
              team_color = colDef(show = FALSE),
              alternate_color = colDef(show = FALSE),
              adp_delta = colDef(
                name = "Pick Value vs. ADP", format = colFormat(digits = 1),
                style = function(value) {
                  if (value > 0) {
                    color <- "#008000"
                  } else if (value < 0) {
                    color <- "#e00000"
                  } else if (value == 0) {
                    color <- "#77777"
                  }
                  list(color = color, fontWeight = "bold")
                }
              ),
              pick_datetime = colDef(
                format = colFormat(date = TRUE),
                name = "Pick Date"
              ),
              draft_start_date = colDef(
                format = colFormat(date = TRUE),
                name = "Draft Start",
                show = FALSE
              ),
              projectedPoints = colDef(show = FALSE),
              player_name = colDef(show = FALSE),
              position = colDef(show = FALSE),
              team = colDef(show = FALSE),
              tournament_title = colDef(show = FALSE),
              adp_date = colDef(show = FALSE),
              max_adp = colDef(show = FALSE),
              team_pos = colDef(show = FALSE),
              position_color = colDef(show = FALSE),
              id = colDef(show = FALSE),
              draft_count = colDef(show = FALSE),
              team_nickname = colDef(show = FALSE),
              logo = colDef(show = FALSE),
              draft_entry = colDef(show = FALSE)
            ),
            outlined = FALSE
          )
        )
      },
      style = list(fontFamily = "Montserrat"),
      theme = reactableTheme(
        # Vertically center cells
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
      )
    )
  })

  cg_table <-
    reactive({
      
      if (!isTruthy(input$player_search)) {
        draft_entry_list <-
          user_adp_table_data() %>%
          clean_names() %>%
          .$draft_entry
      } else {
        draft_entry_list <-
          user_adp_table_data() %>%
          clean_names()
          filter(appearance %in% input$player_search) %>%
          .$draft_entry %>% 
          unique()
      }

      user_adp_table_data() %>%
        clean_names() %>%
        filter(tournament_title == "Best Ball Mania II") %>%
        filter(draft_entry %in% draft_entry_list) %>%
        group_by(draft_entry) %>%
        filter(n() == 18) %>%
        mutate(
          draft_start_date = min(picked_at),
          draft_slot = min(pick_number),
          .before = 1
        ) %>%
        ungroup() %>%
        mutate(draft_number = dense_rank(draft_start_date), .before = 1) %>%
        arrange(draft_start_date, team) %>%
        select(!(tournament_entry_fee:draft_pool_size) & !(draft_entry:draft_total_prizes)) %>%
        group_by(draft, team) %>%
        add_count(team, name = "stack_count") %>%
        arrange(desc(stack_count)) %>%
        mutate(team_stack = paste0(team, " (", stack_count, ")")) %>%
        ungroup() %>%
        group_by(draft_number, position) %>%
        arrange(pick_number, stack_count) %>%
        mutate(position_order = row_number(), .before = 1) %>%
        ungroup() %>%
        mutate(
          stack_num = dense_rank(desc(stack_count)),
          draft_display_label = case_when(
            (position == "QB" & position_order == 1) ~ "QB",
            (position == "RB" & position_order <= 2) ~ "RB",
            (position == "WR" & position_order <= 3) ~ "WR",
            (position == "TE" & position_order == 1) ~ "TE",
            TRUE ~ "BE"
          ),
          position_color = case_when(
            position == "QB" ~ "#9647B8",
            position == "RB" ~ "#15967B",
            position == "WR" ~ "#E67E22",
            position == "TE" ~ "#2980B9",
            TRUE ~ ""
          )
        ) %>%
        group_by(draft_number, draft_display_label) %>%
        arrange(draft_number, draft_display_label, position_order) %>%
        mutate(
          bench_order = row_number(),
          bench_order = case_when(
            (position == "QB" & bench_order == 1) ~ as.double(bench_order) + 1,
            TRUE ~ as.double(bench_order)
          )
        ) %>%
        mutate(draft_display_label = case_when(
          (bench_order == min(bench_order) &
            draft_display_label == "BE" & position != "QB")
          ~ "FLEX-1",
          TRUE ~ draft_display_label
        )) %>%
        ungroup() %>%
        mutate(
          position_sortorder = case_when(
            draft_display_label == "QB" ~ 1,
            draft_display_label == "RB" ~ 2,
            draft_display_label == "WR" ~ 3,
            draft_display_label == "TE" ~ 4,
            str_detect(draft_display_label, "FLEX") ~ 5,
            TRUE ~ 6
          ),
          draft_display_label = case_when(
            draft_display_label == "BE" ~ paste0("BE-", bench_order),
            str_detect(draft_display_label, "FLEX") ~ draft_display_label,
            TRUE ~ paste0(draft_display_label, "-", position_order)
          )
        ) %>%
        group_by(draft_number, draft_display_label) %>%
        arrange(draft_number, draft_display_label, position_order) %>%
        mutate(bench_order = row_number()) %>%
        select(
          draft_display_label, appearance, picked_at, pick_number, first_name, last_name, team, position, team_stack,
          stack_num, position_sortorder, position_color, draft, draft_number, position_order, bench_order, draft_slot,
          draft_start_date
        ) %>%
        arrange(
          draft_number,
          position_sortorder,
          pick_number
        )
    })

  output$team_breakdown <- renderReactable({
    reactable(cg_final2(),
      defaultColDef = (
        colDef(
          align = "center",
          width = 150,
          style =
            function(value) {
              list(color = case_player_position_color(value))
            },
          header =
            function(value) {
              value
            },
          html = TRUE,
        )
      ),
      columns = list(
        draft_display_label = colDef(
          name = "",
          style = list(position = "sticky", background = "#fff", left = 0, zIndex = 1),
          width = 65
        )
      ),
      pagination = FALSE,
      searchable = FALSE,
      sortable = FALSE,
      compact = TRUE,
      style = list(fontFamily = "Montserrat"),
      theme = reactableTheme(
        # Vertically center cells
        cellStyle = list(
          fontSize = "12px", display = "flex", flexDirection = "column", justifyContent = "center",
          align = "center"
        )
      )
    )
  })

  cg_final2 <-
    reactive({
      file <- user_upload()

      req(file)

      ext <- tools::file_ext(file$datapath)

      validate(need(ext == "csv", "Please upload a csv file"))

      data <- read_csv(file$datapath) %>%
        clean_names() %>%
        filter(tournament_title == "Best Ball Mania II")

      ### Get the drafts where the user selected players they're searching for ###

      if (!isTruthy(input$player_search)) {
        draft_entry_list <-
          user_adp_table_data() %>%
          .$draft_entry %>%
          unique()
      } else {
        draft_entry_list <-
          user_adp_table_data() %>%
          filter(appearance %in% input$player_search) %>%
          .$draft_entry %>%
          unique()
      }

      cg_table_pivot <-
        cg_table() %>%
        mutate(
          player_name = paste(first_name, last_name),
          draft_start_date = format(as.Date(draft_start_date,
            format = "%Y-%Om-%d %H:%M:%S"
          ), format = "%B %e")
        ) %>%
        arrange(position_sortorder, draft_number, pick_number, bench_order) %>%
        select(draft_number, player_name, draft_display_label, draft_start_date) %>%
        pivot_wider(
          names_from = c(draft_number, draft_start_date),
          id_cols = c(draft_display_label),
          names_glue = "Draft# { draft_number }
        { draft_start_date }",
          values_from = player_name
        ) %>%
        mutate(draft_display_label = word(draft_display_label, 1, sep = "-"))

      cg_table_filtered <-
        cg_table() %>%
        ungroup() %>%
        mutate(
          player_name = paste(first_name, last_name),
          draft_start_date = format(as.Date(draft_start_date, format = "%Y-%Om-%d %H:%M:%S"), format = "%B %e"),
          team_stack = case_when(
            str_detect(team_stack, "1") ~ NA_character_,
            TRUE ~ team_stack
          )
        ) %>%
        distinct(draft_number, team_stack, .keep_all = TRUE) %>%
        filter(!is.na(team_stack)) %>%
        arrange(desc(str_extract(team_stack, "\\d")), draft_number, stack_num) %>%
        select(draft_number, team_stack, draft_display_label, draft_start_date) %>%
        pivot_wider(
          names_from = c(draft_number, draft_start_date),
          names_glue = "Draft# { draft_number }
        { draft_start_date }",
          values_from = team_stack
        ) %>%
        select(-draft_display_label) %>%
        fill(everything(), .direction = "up") %>%
        mutate(across(everything(), ~ replace(.x, duplicated(.x), NA))) %>%
        filter(if_any(everything(), ~ !is.na(.x))) %>%
        fill(everything(), .direction = "up") %>%
        mutate(across(everything(), ~ replace(.x, duplicated(.x), NA))) %>%
        filter(if_any(everything(), ~ !is.na(.x))) %>%
        fill(everything(), .direction = "up") %>%
        mutate(across(everything(), ~ replace(.x, duplicated(.x), NA))) %>%
        filter(if_any(everything(), ~ !is.na(.x))) %>%
        fill(everything(), .direction = "up") %>%
        mutate(across(everything(), ~ replace(.x, duplicated(.x), NA))) %>%
        filter(if_any(everything(), ~ !is.na(.x))) %>%
        fill(everything(), .direction = "up") %>%
        mutate(across(everything(), ~ replace(.x, duplicated(.x), NA))) %>%
        filter(if_any(everything(), ~ !is.na(.x))) %>%
        add_column(draft_display_label = "Stack", .before = 1) %>%
        mutate(draft_display_label = ifelse(row_number() == 1, "Stack", NA_character_))

      cg_table_draft_slot <-
        cg_table() %>%
        ungroup() %>%
        mutate(draft_start_date = format(as.Date(draft_start_date, format = "%Y-%Om-%d %H:%M:%S"), format = "%B %e")) %>%
        select(draft_slot, draft_number, draft_start_date) %>%
        distinct() %>%
        pivot_wider(
          names_from = c(draft_number, draft_start_date),
          names_glue = "Draft# { draft_number }
        { draft_start_date }",
          values_from = draft_slot,
          values_fn = as.character
        ) %>%
        add_column(draft_display_label = "Draft Slot", .before = 1)

      bbm_playoff_stacks <-
        cg_table() %>%
        ungroup() %>%
        mutate(draft_start_date = format(as.Date(draft_start_date, format = "%Y-%Om-%d %H:%M:%S"), format = "%B %e")) %>%
        group_by(draft, team, draft_number, draft_start_date) %>%
        summarise(stack_count = n()) %>%
        inner_join(bbm_playoff_matchups_team_by_team(), by = c("team" = "team_short_name")) %>% 
        group_by(draft, draft_number, draft_start_date) %>%
        #filter(opp %in% team) %>%
        group_by(game_id, roof_emoji, week, draft_number, draft_start_date) %>%
        mutate(stack_total = sum(stack_count)) %>%
        separate(game_id, into = c("season", "week", "team_1", "team_2"), sep = "_") %>%
        select(-c("week", "season")) %>%
        mutate(
          bbm_playoff_stack = ifelse(stack_count > 1,
                                     paste0("Week: ", week, " ", roof_emoji, "<br>", team_1, "-", team_2, " (", stack_total, ")"),
                                     NA_character_
          )
        ) %>%
        select(stack_total, bbm_playoff_stack, draft_number, draft_start_date, draft, week, team) %>%
        distinct() %>%
        arrange(draft_number, desc(stack_total), week) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = c(week, team, stack_total),
          names_from = c(draft_number, draft_start_date),
          names_glue = "Draft# { draft_number }
        { draft_start_date }",
        values_from = bbm_playoff_stack
        ) %>%
        select(-c(week, team, stack_total)) %>%
        distinct(.keep_all = TRUE) %>%
        sort_na_to_bottom_colwise() %>%
        mutate(draft_display_label = case_when(
          row_number() == 1 ~ NA_character_,
          row_number() == 2 ~ "BBM II",
          row_number() == 3 ~ "Playoff",
          row_number() == 4 ~ "Game",
          row_number() == 5 ~ "Stacks",
          TRUE ~ NA_character_
        ), .before = 1)

       bind_rows(cg_table_pivot, cg_table_filtered, cg_table_draft_slot, bbm_playoff_stacks)
      
    })


  output$draft_slot_frequency <- renderPlot({
    expected_frequency <-
      cg_table() %>%
      ungroup() %>%
      distinct(draft_slot, draft) %>%
      summarise(expected_frequency = round(n() / 12, 1)) %>%
      .$expected_frequency

    cg_table() %>%
      ungroup() %>%
      distinct(draft_slot, draft) %>%
      select(draft_slot) %>%
      ggplot(aes(draft_slot)) +
      geom_bar(fill = "#DDB423", color = "#DDB423") +
      geom_hline(
        yintercept = expected_frequency, size = 2,
        alpha = .3, color = "white",
        linetype = "longdash"
      ) +
      geom_text(stat = "count", aes(label = stat(count), vjust = 1.5), color = "#393939") +
      geom_text(
        data = data.frame(x = 5, y = expected_frequency), aes(x, y),
        label = paste0("Expected Frequency"),
        vjust = -1,
        color = "white",
        alpha = .5,
        size = 8
      ) +
      hrbrthemes::theme_ipsum_rc() +
      expand_limits(x = c(0, 12)) +
      scale_x_continuous(breaks = 1:12) +
      xlab("Draft Slot") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "#393939")
      )
  })

  nfl_schedule <- reactive({
    read_csv(url("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"))
  })

  observeEvent(input$ud_user_adp,
    {
      file <- user_upload()

      req(file)

      ext <- tools::file_ext(file$datapath)

      validate(need(ext == "csv", "Please upload a csv file"))

      appearance_list_choices <- read_csv(file$datapath) %>%
        clean_names() %>%
        filter(tournament_title == "Best Ball Mania II") %>%
        transmute(player_name = paste(first_name, last_name), id = appearance) %>%
        distinct() %>%
        deframe()

      updatePickerInput(
        session = session, inputId = "player_search",
        choices = appearance_list_choices
      )
    },
    ignoreInit = TRUE
  )

  observe({
    if (isTruthy(input$ud_user_adp)) {
      Sys.sleep(.25)
      # enable the download button
      shinyjs::enable("data_file")
      # change the html of the download button
      shinyjs::html(
        "data_file",
        sprintf("<i class='fa fa-download'></i>
                              Download Ready")
      )
    }
  })

  output$data_file <- downloadHandler(
    filename = function() {
      paste("TeamAnalysis-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cg_final2(), file)
    }
  )
  # disable the downdload button on page load
  shinyjs::disable("data_file")

  user_adp_filter <- reactive({
    as.numeric(input$user_filter_adp)
  })

  user_filter_position <- reactive({
    input$current_draft_stack_position
  })

  user_filter_team <- reactive({
    input$current_draft_teams
  })

  bbm_playoff_matchups_team_by_team <- reactive({
    
      nfl_schedule() %>%
      #    read_csv(url("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv")) %>%
      filter(
        #home_team %in% "ARI" |
        (home_team %in% user_filter_team() |
           #   away_team %in% "ARI",
           away_team %in% user_filter_team()),
        season == 2021,
        week > 14,
        week < 18
      ) %>%
      select(home_team, away_team, roof, week, game_id) %>%
      bind_rows(
        nfl_schedule() %>%
          # read_csv(url("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv")) %>%
          select(home_team, away_team, season, week) %>%
          filter(
            #  home_team %in% "ARI",
            (home_team %in% user_filter_team()),
            season == 2021,
            week > 14,
            week < 18
          ) %>%
          rename(home_team = away_team, away_team = home_team)
      ) %>%
      mutate(roof = ifelse((roof == "closed" | is.na(roof)), "retractable", roof)) %>%
      select(team = home_team, opp = away_team, roof, week, game_id) %>%
      left_join(get_nfl_teams(), by = c("team" = "team_short_name")) %>%
      select(team, main_team_name = team_name, opp, roof, week, game_id) %>%
      left_join(get_nfl_teams(), by = c("opp" = "team_short_name")) %>%
      select(team, main_team_name, opp, opp_team_name = team_name, roof, week, game_id) %>%
      filter(opp != user_filter_team()) %>% 
      left_join(
        current_ud_adp(),
        #current_ud_adp(),
        by = c("opp" = "team_short_name")
      ) %>%
      filter(
        as.double(adp) + 12 >= as.double(user_adp_filter()),
        slotName %in% user_filter_position(),
        (team %in% user_filter_team() |
           opp %in% user_filter_team())
        # as.double(adp) + 12 >= as.double(user_adp_filter,
        # slotName %in% user_filter_position,
        # (team %in% user_filter_team |
        #    opp %in% user_filter_team)
      ) %>%
    transmute(
      team_name,
      game_id,
      player = paste(firstName, lastName),
      slotName, opp, week, adp, logo,
      roof_emoji = case_when(
        roof == "retractable" ~ "\U00026C5",
        roof == "dome" ~ "\U2600",
        roof == "outdoors" ~ "\U1F327"
      ),
      id
    ) %>%
    arrange(adp) %>% 
      inner_join(get_nfl_teams(), by = c("team_name" = "team_name"))
  })

bbm_playoff_matchups_explorer <- reactive({
  nfl_schedule() %>%
    #    read_csv(url("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv")) %>%
    filter(
      #home_team %in% "ARI" |
      (home_team %in% user_filter_team() |
         #   away_team %in% "ARI",
         away_team %in% user_filter_team()),
      season == 2021,
      week > 14,
      week < 18
    ) %>%
    select(home_team, away_team, roof, week, game_id) %>%
    bind_rows(
      nfl_schedule() %>%
        # read_csv(url("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv")) %>%
        select(home_team, away_team, season, week) %>%
        filter(
          #  home_team %in% "ARI",
          (home_team %in% user_filter_team() |
             #    away_team %in% "ARI",
             away_team %in% user_filter_team()),
          season == 2021,
          week > 14,
          week < 18
        ) %>%
        rename(home_team = away_team, away_team = home_team)
    ) %>%
    mutate(roof = ifelse((roof == "closed" | is.na(roof)), "retractable", roof)) %>%
    select(team = home_team, opp = away_team, roof, week, game_id) %>%
    left_join(get_nfl_teams(), by = c("team" = "team_short_name")) %>%
    select(team, main_team_name = team_name, opp, roof, week, game_id) %>%
    left_join(get_nfl_teams(), by = c("opp" = "team_short_name")) %>%
    select(team, main_team_name, opp, opp_team_name = team_name, roof, week, game_id) %>%
    left_join(
      #current_ud_adp,
      current_ud_adp(),
      by = c("opp" = "team_short_name")
    ) %>%
    filter(
      as.double(adp) + 12 >= as.double(user_adp_filter()),
      slotName %in% user_filter_position(),
      (team %in% user_filter_team() |
         opp %in% user_filter_team())
      # as.double(adp) + 12 >= as.double(user_adp_filter,
      # slotName %in% user_filter_position,
      # (team %in% user_filter_team |
      #    opp %in% user_filter_team)
    ) %>%
  transmute(
    team_name,
    player = paste(firstName, lastName),
    slotName, opp, week, adp, logo,
    roof_emoji = case_when(
      roof == "retractable" ~ "\U00026C5",
      roof == "dome" ~ "\U2600",
      roof == "outdoors" ~ "\U1F327"
    ),
    id
  ) %>%
  arrange(adp)
})

  output$in_draft_helper_table <- renderReactable({
    req(
      user_adp_filter(),
      user_filter_team(),
      user_filter_position()
    )

    position_color <-
      bbm_playoff_matchups_explorer() %>%
      mutate(
        position_color = case_when(
          slotName == "QB" ~ "#9647B8",
          slotName == "RB" ~ "#15967B",
          slotName == "WR" ~ "#E67E22",
          slotName == "TE" ~ "#2980B9",
          TRUE ~ ""
        )
      )

    adp_date <-
      as.Date(min(current_ud_adp()$adp_date)) %>%
      format("%m/%d/%y") %>%
      str_remove_all("0")

    reactable(
      bbm_playoff_matchups_explorer(),
      columns = list(
        id = colDef(show = FALSE),
        player = colDef(
          name = "Player",
          cell = function(value, index) {
            pos_color <- position_color$position_color[index]
            pos_color <- if (!is.na(pos_color)) pos_color else "#000000"
            tagList(
              htmltools::div(
                style = list(color = pos_color),
                value
              )
            )
          },
          minWidth = 240
        ),
        slotName = colDef(name = "Position"),
        team_name = colDef(
          name = "Team",
          cell = function(value, index) {
            image <- img(src = position_color$logo[index], height = "20px")
            tagList(
              div(style = list(display = "inline-block", alignItems = "center"), image),
              " ", value
            )
          },
          minWidth = 200,
          align = "center"
        ),
        opp = colDef(
          show = FALSE
        ),
        roof_emoji = colDef(
          name = "Roof",
          html = TRUE,
          filterable = FALSE,
          align = "center"
        ),
        week = colDef(
          name = "Week",
          align = "center"
        ),
        adp = colDef(
          name = "UD ADP",
          header = function(value, index) {
            note <- div(style = "color: #666", paste("("), adp_date, ")")
            tagList(
              div(title = value, value),
              div(style = list(fontSize = 12), note)
            )
          }, filterable = FALSE,
          align = "center"
        ),
        logo = colDef(
          show = FALSE
        )
      ),
      filterable = FALSE,
      highlight = TRUE,
      style = list(fontFamily = "Montserrat")
    )
  })

  output$in_draft_helper_plot <- renderPlot({
    req(
      user_adp_filter(),
      user_filter_team(),
      user_filter_position()
    )

    total_adp <- GET(
      "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_total.rds",
      authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
    )

    ud_adp_total <-
      content(total_adp, as = "raw") %>%
      parse_raw_rds() %>%
      mutate(
        adp = as.double(adp),
        teamName = case_when(
          teamName == "NY Jets" ~ "New York Jets",
          teamName == "NY Giants" ~ "New York Giants",
          teamName == "Washington Football Team" ~ "Washington",
          TRUE ~ teamName
        )
      ) %>%
      left_join(nfl_team(), by = c("teamName" = "full_name"))

    relevant_players <-
      reactive({
      ud_adp_total %>%
      filter(
        slotName %in% user_filter_position()
        # ,team_short_name %in% user_filter_team()
      ) %>%
      arrange(desc(adp_date)) %>%
      group_by(id) %>%
      filter(!is.na(adp)) %>%
      summarize(
        min_adp = min(adp),
        max_adp = max(adp),
        most_recent_draft_adp = ifelse(row_number() == 1, adp, 60000)
      ) %>%
      ungroup() %>%
      filter(
        most_recent_draft_adp + 12 >= as.double(user_adp_filter()),
        most_recent_draft_adp - 50 <= as.double(user_adp_filter())
      ) %>%
      distinct()
      })

    bbm_playoff_matchups_explorer() %>%
      inner_join(ud_adp_total %>%
        filter(id %in% relevant_players()$id),
      by = c("team_name", "slotName", "logo", "id")
      ) %>%
      select(-adp.x) %>%
      mutate(adp = adp.y) %>%
      distinct() %>%
      mutate(
        adp_date = as.Date(adp_date),
        player = paste0(player, " (", opp, ")")
      ) %>%
      ggplot2::ggplot(aes(x = as.Date(adp_date), y = adp, group = id, color = player)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = user_adp_filter(), alpha = .4, colour = "#E0B400") +
      ggplot2::annotate("text",
        x = median(as.Date(ud_adp_total$adp_date)),
        y = user_adp_filter(),
        vjust = 1.5,
        label = "Upcoming pick",
        alpha = .7, colour = "#E0B400",
        size = 8
      ) +
      # geom_blank(aes(y = adp * 1.05)) +
      hrbrthemes::theme_ft_rc() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(color = "#E0B400", size = 18, face ="bold"),
        axis.title.x = element_text(color = "#E0B400", size = 18, face ="bold"),
        axis.text.x.bottom = element_text(color = "white", hjust = 1, angle = 45, size = 18),
        axis.text.y.left = element_text(color = "white", hjust = 1, size = 18),
        plot.title = element_text(color = "white", size = 14, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)
      ) +
      scale_x_date(date_labels = "%b %d", date_breaks = "3 days") +
      scale_y_continuous(trans = "reverse") +
      # scale_color_manual(values = c(
      #   "#175FC7",
      #   "#E67E22",
      #   "#6CBE45",
      #   "#0FC8D4",
      #   "#D0402E",
      #   "#9647B8",
      #   "#2980B9",
      #   "#15997E"
      # )) +
      xlab("Date") +
      ylab("ADP")
    
  })
}