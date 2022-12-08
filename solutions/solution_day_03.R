library(tidyverse)
library(here)
library(vctrs)

input <- read_lines(here("data/data_day_03.txt"))
alphabet <- c(letters, LETTERS)

str_split_half <- function(string) {
  n <- str_length(string)
  c(
    C1 = str_sub(string, start = 1, end = n/2),
    C2 = str_sub(string, start = n/2 + 1, end = n)
  )
}

fetch_common <- function(string) {
  str_split(string, "") %>% 
    reduce(intersect)
}

# part I:
map(input, str_split_half) %>% 
  map(fetch_common) %>% 
  match(alphabet) %>% 
  sum()

# part II:

group_id <- sort(rep_along(input, seq_len(length(input)/3)))

vec_split(x = input, by = group_id) %>% 
  mutate(badge = map(val, fetch_common)) %>% 
  mutate(score = match(badge, alphabet)) %>% 
  summarise(total = sum(score)) %>% 
  pull()
  
