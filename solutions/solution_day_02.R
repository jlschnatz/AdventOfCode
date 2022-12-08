library(tidyverse)
library(here)

data <- read_fwf(
  file = here("data/data_day_02.txt"),
  col_select = c(opponent = X1, own = X2)
  ) %>% 
  mutate(
    opponent = match(opponent, LETTERS),
    own = match(own, LETTERS) - 23
    ) 


# part I:
data %>% 
  mutate(score = own + ((1 + own - opponent) %% 3) * 3) %>% 
  summarise(total_sore = sum(score))  %>% 
  pull()

# part II:

data %>% 
  mutate(shape_needed = ((opponent + own) %% 3) + 1 + 3 * (own - 1)) %>% 
  summarise(total = sum(shape_needed))
  