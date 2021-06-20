library(tidyverse)
library(here)
library(fs)

directory <- str_replace_all(paste0(here(), "/data"), "/", "\\\\")

list.files(directory) %>%
  as_tibble() %>%
  mutate(file_date = as.Date.character(str_extract(value, "\\d{4}\\-\\d{2}-\\d{2}-T\\d{4}"))) %>%
  arrange(desc(file_date))

ud_adp <-
  dir_info(path = directory) %>%
  filter(endsWith(path, ".csv")) %>%
  select(path) %>%
  mutate(
    data = purrr::map(path, read_csv),
    adp_date = str_extract(path, "\\d{4}\\-\\d{2}-\\d{2}-T\\d{4}")
  ) %>%
  unnest(cols = c(data)) %>%
  select(!c(path, lineupStatus, byeWeek), position = slotName) %>%
  filter(adp != "-") %>%
  mutate(
    adp = as.double(adp),
    projectedPoints = as.double(projectedPoints),
    adp_date = as.Date.character(adp_date)
  ) %>%
  arrange(desc(adp_date), adp) %>%
  saveRDS(paste0(
    directory,
    "\\ud_adp_complete",
    format(Sys.time(), "%Y-%m-%d-T%H%M"),
    ".rds"
  ))
