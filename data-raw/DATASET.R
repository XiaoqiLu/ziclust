#' Dataset: sleep
#'
#' A dataset containing sleep/awake onsets of sleep events.
#'
#' @format A data frame with 1015 rows (each is a sleep event) and 4 variables:
#' \describe{
#'   \item{subject_id}{subject id}
#'   \item{date}{date of sleep onset}
#'   \item{sleep_onset}{sleep time}
#'   \item{awake_onset}{awake time}
#' }
#' @source Ying Chen, \email{Ying.Chen@@nyspi.columbia.edu}
"sleep"

library(tidyverse)

sleep <- read_csv("./inst/extdata/sleep.csv",
                col_names = c("subject_id", "date", "sleep_onset", "wake_onset"),
                skip = 1) %>%
  filter(!is.na(sleep_onset) & !is.na(wake_onset))

usethis::use_data(sleep, overwrite = TRUE)
