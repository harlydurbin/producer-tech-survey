library(tidyverse)

#https://github.com/alexpghayes/broom/blob/some_cleanup/R/leaps.R
tidy.regsubsets <- function(x, ...) {
  s <- summary(x)
  inclusions <- as_tibble(s$which)
  metrics <- with(
    s,
    tibble(
      r.squared = rsq,
      adj.r.squared = adjr2,
      BIC = bic,
      mallows_cp = cp
    )
  )
  bind_cols(inclusions, metrics)
}