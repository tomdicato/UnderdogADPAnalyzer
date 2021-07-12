library(dplyr)

## Raw RDS Parser

parse_raw_rds <- function(raw) {
  con <- gzcon(rawConnection(raw))
  
  on.exit(close(con))
  
  readRDS(con) %>%
    tibble::tibble()
}

get_rds <- function(url) {
  
  url_clean <- paste0("https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/", url)
  
  x <- GET(
    url_clean,
    authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
  )
  
  content(x, as = "raw") %>%
    parse_raw_rds()
  
}

x <- GET(
  "https://raw.githubusercontent.com/DangyWing/Underdog-ADP/main/data/ud_adp_current.rds",
  authenticate(Sys.getenv("GITHUB_PAT_DANGY"), "")
)

current_ud_adp <-
  content(x, as = "raw") %>%
  parse_raw_rds() %>%
  mutate(adp = as.double(adp),
         teamName = case_when(
           teamName == "NY Jets" ~ "New York Jets",
           teamName == "NY Giants" ~ "New York Giants",
           teamName == "Washington Football Team" ~ "Washington",
           TRUE ~ teamName
         ))

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

get_nfl_teams <- function() {
  
  team_url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams?&limit=50"
  raw_teams <- jsonlite::fromJSON(url(team_url))
  
  purrr::pluck(raw_teams, "sports", "leagues", 1, "teams", 1, "team") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(logos = purrr::map_chr(logos, function(df) df[1, 1])) %>%
    dplyr::select(id, name:alternateColor, logos, -shortDisplayName) %>%
    purrr::set_names(
      nm = c(
        "uid", "team_name", "team_nickname", "team_short_name", "full_name", "team_color",
        "alternate_color", "logo"
      )
    ) %>%
    dplyr::mutate(
      team_color = paste0("#", team_color),
      alternate_color = paste0("#", alternate_color)
    )
}

sort_na_to_bottom_colwise <- function(df) {
  
  df <- df %>% 
    
    ## have at least one row in each columns that is not NA ##
    mutate(across(everything(), ~ ifelse(row_number() == 1, replace_na(.x, "-"), .x)))
  
  max_length <- max(unlist(lapply(df, length)))
  
  df %>% 
    map(sort) %>% 
    map(function(x) {
      ans <- rep(NA, length = max_length)
      ans[1:length(x)] <- x
      return(ans)
    }
    ) %>%
    as_tibble() %>% 
    distinct() %>%
    ## Replace the placeholder "-" with NA ##
    mutate(across(everything(), na_if, "-"))
}
# 
# df <- bbm_playoff_stacks_test
#   
#   
# df <- 
#   df %>% 
#   add_row(.before = 1) %>% 
#   ## have at least one row in each columns that is not NA ##
#   mutate(across(everything(), ~ ifelse(row_number() == 1, replace_na(.x, "-"), .x)))
# 
# max_length <- max(unlist(lapply(df, length)))
# 
# df %>% 
#   map(sort) %>% 
#   lapply(unique) %>% 
#   map(function(x) {
#     ans <- rep(NA, length = max_length)
#     ans[1:length(x)] <- x
#     return(ans)
#   }
#   ) %>%
#   as_tibble() %>%
#   distinct() %>%
#   ## Replace the placeholder "-" with NA ##
#   mutate(across(everything(), na_if, "-"))