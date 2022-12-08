library(tidyverse)
library(here)

data <- read_csv(
  file = here("data/data_day_04.txt"),
  col_names = FALSE,
  name_repair = ~c("e1", "e2")
  )

assigment_pairs_parsed <- data %>% 
  separate(col = e1, into = c("e1_1", "e1_2"), sep = "-") %>% 
  separate(col = e2, into = c("e2_1", "e2_2"), sep = "-") %>% 
  mutate(across(everything(), as.integer)) 
  
# part I:
fully_contained_logic <- quo(
  (e1_1 >= e2_1 & e1_2 <= e2_2) |
    (e2_1 >= e1_1 & e2_2 <= e1_2)
)

assigment_pairs_parsed %>% 
mutate(score = if_else(!!!fully_contained_logic, 1, 0)) %>% 
  summarise(total = sum(score)) %>% 
  pull()

