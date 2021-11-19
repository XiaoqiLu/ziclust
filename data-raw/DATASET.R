library(tidyverse)

sleep <- read_csv("./inst/extdata/sleep.csv",
                col_names = c("subject_id", "date", "sleep_onset", "wake_onset"),
                skip = 1) %>%
  filter(!is.na(sleep_onset) & !is.na(wake_onset))

usethis::use_data(sleep, overwrite = TRUE)
