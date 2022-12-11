library(tidyverse)
library(here)

input <- read_fwf(
  file = here("data/data_day_10.txt"),
  name_repair = ~c("op", "add"),
  fwf_widths(widths = c(4, 5))
  )

processed_data <- input %>% 
  mutate(
    add = coalesce(add, 0),
    cycles = case_when(
      op == "noop" ~ 1,
      op == "addx" ~ 2)
    ) %>% 
  uncount(cycles, .id = "id") %>% 
  mutate(cycle = row_number(), .before = everything()) %>% 
  mutate(is_last = case_when(
    op == "noop" ~ 1,
    op == "addx" & id == 1 ~ 0,
    op == "addx" & id == 2 ~ 1
  )) %>% 
  mutate(add_cumsum = lag(1 + cumsum(is_last * add), default = 0))

# part I:
processed_data %>% 
  filter((cycle - 20) %% 40 == 0) %>% 
  summarise(signal_strength = cycle * add_cumsum) %>% 
  sum()

# part II:

processed_data %>% 
  mutate(
    row = (cycle - 1) %/% 40,
    col = (cycle - 1) %% 40,
    pixel = if_else(abs(col - add_cumsum) <= 1, "#", " ")) %>% 
  group_by(row) %>% 
  summarise(paste0(pixel, collapse = ""))



