library(tidyverse)
library(here)

input <- read_lines(here("data/data_day_07.txt"))

# cd x  -> move down one level
# cd .. -> move out one level
# cd /  -> switch to outermost directory
head(input)
