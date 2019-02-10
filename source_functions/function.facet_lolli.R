library(tidyverse)


facet_lolli <- function(question_variable, breaks, n_row, n_col) {
  header <- responses_long %>%
    filter(str_detect(variable,
                      question_variable)) %>%
    distinct(verbose_question) %>%
    as.character(.)
  
  
  responses_long %>%
    filter(str_detect(variable,
                      question_variable) &
             !is.na(verbose_response)) %>%
    mutate(verbose_response = fct_reorder(verbose_response, response)) %>%
    add_count(verbose_variable) %>%
    rename(`Question n` = n) %>%
    group_by(variable, response) %>%
    mutate(`Response n` = n()) %>%
    ungroup() %>%
    ggplot(aes(x = verbose_response,
               y = `Response n`)) +
    geom_point() +
    geom_linerange(aes(ymin = 0, ymax = `Response n`)) +
    theme(
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      axis.title.x = element_text(size = 18)
    ) +
    #scale_x_continuous(breaks = breaks) +
    #theme(axis.text.y = element_text(angle = 15)) +
    coord_flip() +
    facet_wrap(~ verbose_variable,
               nrow = n_row,
               ncol = n_col) +
    xlab("") +
    ggtitle(str_wrap(header, width = 55)) +
    theme(
      strip.text.x = element_text(size = 24, margin = margin(.3, 0, .3, 0, "cm")),
      plot.title = element_text(size = 26)
    )
}
