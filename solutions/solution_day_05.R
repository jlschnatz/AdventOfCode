library(tidyverse)
library(unglue)
library(here)

extract_stacks <- function(.string) {
  str_replace_all(.string, ".+([A-Z]).+", "\\1") %>% 
    na.omit() %>% 
    rev()
}

stack <- read_fwf(
  file = here("data/data_day_05.txt"),
  fwf_widths(widths = rep(4, 9)),
  n_max = 8
  ) %>% 
  map(extract_stacks)

moves <- read_lines(file = here("data/data_day_05.txt"))[-c(1:10)] %>% 
  unglue("move {move} from {from} to {to}", convert = TRUE)


move_crate <- function(x, m) {
  x[[m$to]] <- c(x[[m$to]], rev(tail(x[[m$from]], m$move)))
  x[[m$from]] <- head(x[[m$from]], -m$move)
  return(x)
}


reduce(.x = moves, .init = stack, .f = move_crate) %>% 
  map(last) %>% 
  str_flatten()