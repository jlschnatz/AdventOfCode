library(tidyverse)
library(here)
library(slider)

signal <- read_lines(here("data/data_day_06.txt")) %>% 
  str_split("") %>%
  unlist()

# part I:
which.min(slide_dbl(.x = signal, .f = anyDuplicated, .after = 4)) + 3

# part II:
which.min(slide(.x = signal, .f = anyDuplicated, .after = 14)) + 13
