library(tidyverse)
library(kableExtra)

mean_scores <- function(question_variable) {
  responses_long %>%
    filter(str_detect(variable, question_variable)) %>%
    group_by(verbose_variable) %>%
    summarise(
      `Mean response` = mean(response, na.rm = TRUE),
      `Median response` = median(response, na.rm = TRUE)
    ) %>%
    arrange(desc(`Mean response`)) %>%
    rename(`Response variable` = verbose_variable) %>%
    kable("latex") %>%
    kable_styling(position = "center")
}

mean_scores_raw <- function(question_variable) {
  responses_long %>%
    filter(str_detect(variable, question_variable)) %>%
    group_by(verbose_variable) %>%
    summarise(
      `Mean response` = mean(response, na.rm = TRUE),
      `Median response` = median(response, na.rm = TRUE)
    ) %>%
    arrange(desc(`Mean response`)) %>%
    rename(`Response variable` = verbose_variable)
}