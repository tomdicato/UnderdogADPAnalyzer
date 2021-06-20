library(tidyverse)
library(here)
library(janitor)

readxl::read_xlsx(here("DricoUD.xlsx")) %>%
  clean_names() %>%
  select(
    picked_at, pick_number, first_name, last_name,
    player_guid = appearance,
    team, position, draft_size, tournament_title,
    tournament_size, tournament, draft
  ) %>%
  mutate(total_drafts = n_distinct(draft)) %>%
  group_by(player_guid, first_name, last_name, position, total_drafts) %>%
  summarise(
    ADP = mean(pick_number),
    min_pick = min(pick_number),
    max_pick = max(pick_number),
    draft_count = n_distinct(draft)
  ) %>%
  mutate(
    draft_percentage = (draft_count / total_drafts) * 100
  ) %>%
  arrange(ADP) %>%
  View()
