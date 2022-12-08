library(tidyverse)
library(here)

raw_data <- read_fwf(
  file = here("data/data_day_01.txt"),
  col_select = c(calories = X1)
  )

total_calories_elf <- raw_data %>% 
  mutate(id_elf = cumsum(if_else(is.na(calories), 1, 0))) %>% 
  group_by(id_elf) %>% 
  summarise(total_elf = sum(calories, na.rm = TRUE)) 

# Part I:
total_calories_elf %>% 
  slice_max(total_elf, n = 1) %>% 
  pull(total_elf)

# Part II:

total_calories_elf %>% 
  slice_max(total_elf, n = 3) %>% 
  summarise(total_three = sum(total_elf)) %>% 
  pull()
