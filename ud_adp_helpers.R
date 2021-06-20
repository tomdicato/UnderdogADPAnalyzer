library(dplyr)

## Raw RDS Parser

parse_raw_rds <- function(raw) {
  con <- gzcon(rawConnection(raw))
  
  on.exit(close(con))
  
  readRDS(con) %>%
    tibble::tibble()
}

x <- GET(
  "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_current.rds",
  authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
)

current_ud_adp <-
  content(x, as = "raw") %>%
  parse_raw_rds() %>%
  mutate(adp = as.double(adp))

player_name_to_color <- 
  
  current_ud_adp %>%
  mutate(player_name = paste(firstName, lastName),
         position_color = case_when(
           slotName == "QB" ~ "#9647B8",
           slotName == "RB" ~ "#15967B",
           slotName == "WR" ~ "#E67E22",
           slotName == "TE" ~ "#2980B9",
           TRUE ~ ""
         )) %>%
  select(player_name, position_color)

case_player_position_color <- function(value) {

color <-
  
  player_name_to_color %>% 
    filter(player_name == value) %>% 
    slice(1) %>% 
    .$position_color
  
if (length(color) == 0) {
  return("#000000")
  } else

  return(color)
  
}

