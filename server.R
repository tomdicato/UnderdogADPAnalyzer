library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(fs)
library(reactable)
library(htmltools)
library(httr)
source(paste0(here(), "/ud_adp_helpers.R"))

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

function(input, output, session) {
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

  adp_table_data <-
    reactive({
      x <- GET(
        "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_current.rds",
        authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
      )

      ### THANK YOU TAN ###

      current_ud_adp <-
        content(x, as = "raw") %>%
        parse_raw_rds() %>%
        mutate(adp = as.double(adp))

      file <- user_upload()

      req(file)

      ext <- tools::file_ext(file$datapath)

      validate(need(ext == "csv", "Please upload a csv file"))

      read_csv(file$datapath)
    })

  output$adp_table <- renderReactable({

    ### Get underdog overall ADP ###

    x <- GET(
      "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_current.rds",
      authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
    )

    ### THANK YOU TAN ###

    current_ud_adp <-
      content(x, as = "raw") %>%
      parse_raw_rds() %>%
      mutate(adp = as.double(adp))

    file <- user_upload()

    req(file)

    ext <- tools::file_ext(file$datapath)

    validate(need(ext == "csv", "Please upload a csv file"))


    # adp_table_data <- read_csv(file$datapath)

    # local
    # adp_table_data <- read_csv("C:\\Users\\tom\\Downloads\\73ddf544-d36b-4771-9373-e49e55d9653f_2021-06-13.csv")

    adp_date <- max(current_ud_adp$adp_date) %>%
      as.Date()

    if (!isTruthy(input$player_search)) {
      appearance_list <-
        adp_table_data() %>%
        clean_names() %>%
        .$appearance
    } else {
      appearance_list <-
        input$player_search
    }

    user_adp_vs_ud_adp <-
      adp_table_data() %>%
      clean_names() %>%
      filter(tournament_title == "Best Ball Mania II") %>%
      filter(appearance %in% appearance_list) %>%
      select(id = appearance, picked_at:position, tournament_title, draft_entry) %>%
      mutate(picked_at = as.POSIXct(picked_at)) %>%
      group_by(draft_entry) %>%
      mutate(draft_start_date = min(picked_at)) %>%
      ungroup() %>%
      arrange(desc(draft_start_date), pick_number) %>%
      left_join(current_ud_adp, by = c("id" = "id")) %>%
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
                name = "When"
              ),
              draft_start_date = colDef(
                format = colFormat(date = TRUE),
                name = "Draft Start"
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
              id = colDef(show = FALSE)
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
        data %>%
        .$draft_entry
    } else {
      draft_entry_list <-
        data %>%
        filter(appearance %in% input$player_search) %>%
        .$draft_entry
    }
    
    cg_table <-
      data %>%
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
      mutate(team_stack = paste0(team, " (", stack_count, ")")) %>%
      ungroup() %>%
      group_by(draft_number, position) %>%
      arrange(pick_number) %>%
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
    
    cg_table_pivot <-
      cg_table %>%
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
      cg_table %>%
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
      arrange(draft_number, stack_num) %>%
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
      cg_table %>%
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
    
      bind_rows(cg_table_pivot, cg_table_filtered, cg_table_draft_slot)
    
  })
  
  
  output$team_breakdown <- renderReactable({
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
        data %>%
        .$draft_entry
    } else {
      draft_entry_list <-
        data %>%
        filter(appearance %in% input$player_search) %>%
        .$draft_entry
    }

    cg_table <-
      data %>%
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
      mutate(team_stack = paste0(team, " (", stack_count, ")")) %>%
      ungroup() %>%
      group_by(draft_number, position) %>%
      arrange(pick_number) %>%
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

    cg_table_pivot <-
      cg_table %>%
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
      cg_table %>%
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
      arrange(draft_number, stack_num) %>%
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
      cg_table %>%
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

    cg_final <-
      bind_rows(cg_table_pivot, cg_table_filtered, cg_table_draft_slot)
        

    reactable(cg_final,
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
          html = TRUE
        )
      ),
      columns = list(
        draft_display_label = colDef(
          name = "",
          style = list(position = "sticky", background = "#fff", left = 0, zIndex = 1),
          width = 65
        )
      ),
      defaultPageSize = 25,
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
  
  observeEvent(input$ud_user_adp, {
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
  }, ignoreInit = TRUE)
  
  output$download <- downloadHandler(
    filename = function() {
      paste("TeamAnalysis-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file){
      write.csv(cg_final2(), file)
      
    }
  )
}
