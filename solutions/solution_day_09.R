library(tidyverse)
library(here)

follow_head <- function(tail, head) {
  if (max(abs(head - tail)) <= 1) {
    return(tail)
  }
  map2_dbl(tail, head, ~.x + sign(.y - .x))
}

input <- read_fwf(
  file = here("data/data_day_09.txt"),
  name_repair = ~c("direction", "steps"),
  col_types = cols(steps = "i", direction = "f"),
  fwf_cols(steps = 1, direction = 2)

  )
directional_vectors <- tibble(
  direction = factor(c("L", "R", "U", "D")),
  x = c(-1, 1, 0, 0),
  y = c(0, 0, 1, -1)
  )

lookup_moves <- input %>% 
  uncount(steps) %>% 
  inner_join(directional_vectors, by = "direction") %>% 
  mutate(across(
    .cols = x:y,
    .fns = cumsum,
    .names = "head_{col}"),
    head = map2(.x = head_x, .y = head_y, .f = ~c(.x, .y))
  ) %>% 
  mutate(tail = accumulate(.x = head, .f = follow_head, .init = c(0, 0))[-1]) %>%
  hoist(.col = tail, tail_x = 1, tail_y = 2)


lookup_moves %>% 
  distinct(tail_x, tail_y) %>% 
  nrow()


