library(tidyverse)

freqs <- function(question_variable){
  responses_long %>% 
    filter(str_detect(variable, question_variable),
           !is.na(response)) %>% 
    add_count(variable) %>% 
    rename(variable_n = n) %>% 
    group_by(variable, response) %>% 
    mutate(response_n = n()) %>% 
    mutate(freq = response_n/variable_n) %>% 
    select(variable, variable_n, response_n, freq) %>% 
    distinct() %>% 
    split(.$variable) %>% 
    purrr::map(~arrange(.x, desc(freq))) %>%
  purrr::map(kable("latex"))
  
}

freqs_raw <- function(question_variable){
  responses_long %>% 
    filter(str_detect(variable, question_variable),
           !is.na(response)) %>% 
    add_count(variable) %>% 
    rename(variable_n = n) %>% 
    group_by(variable, response) %>% 
    mutate(response_n = n()) %>% 
    mutate(freq = response_n/variable_n) %>% 
    select(variable, variable_n, response_n, freq) %>% 
    distinct() %>% 
    split(.$variable) %>% 
    purrr::map(~arrange(.x, desc(freq)))
  
}
