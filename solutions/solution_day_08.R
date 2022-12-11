library(tidyverse)
library(here)

read_matrix <- function(.path, .sep = "", .fill = NA, .type = identity) {
  input <- readr::read_lines(here::here(.path))
  input_list <- stringr::str_split(input, .sep)
  n_row <- length(input)
  n_col <- lengths(input_list)
  mat <- matrix(data = .fill, nrow = n_row, ncol = max(n_col))
  for (i in seq_along(input)) {
    mat[i, seq_len(n_col[i])] <- .type(input_list[[i]])
  }
  return(mat)
}

input <- read_matrix(
  .path = here("data/data_day_08.txt"), 
  .type = as.integer
  )

# part I:
above <- function(x, row, col) x[seq_len(row - 1), col]
below <- function(x, row, col) x[seq(nrow(input), row + 1), col]
left <- function(x, row, col)  x[row, seq_len(col - 1)] 
right <- function(x, row, col) x[row, seq(ncol(input), col + 1)]


bin_vis <- matrix(data = FALSE, nrow = nrow(input), ncol = ncol(input))
for (row in seq_len(nrow(input))) {
  for (col in seq_len(ncol(input))) {
    pointer <- input[row, col]
    # detect edge
    if (row == 1 || row == nrow(input) || col == 1 || col == ncol(input)) {
      bin_vis[row, col] <- TRUE
      next
    }
    # check above
    above_vec <- above(input, row, col)
    if (all(pointer > above_vec)) {
      bin_vis[row, col] <- TRUE
      next
    }
    # check below
    below_vec <- below(input, row, col)
    if (all(pointer > below_vec)) {
      bin_vis[row, col] <- TRUE
      next
    }
    # check right
    right_vec <- right(input, row, col)
    if (all(pointer > right_vec)) {
      bin_vis[row, col] <- TRUE
      next
    }
    # check left
    left_vec <- left(input, row, col) 
    if (all(pointer > left_vec)) {
      bin_vis[row, col] <- TRUE
      next
    }
  }
}
sum(bin_vis)

# part II:

compute_distance <- function(x, rows, cols) {
  direction <- x[rows, cols]
  distance <- suppressWarnings(min(which(!(pointer > direction))))
  if (distance == Inf) {
    distance <- length(direction)
  }
  return(distance)
}

scenic_map <- matrix(data = 0, nrow = nrow(input), ncol = ncol(input))
for (row in seq_len(nrow(input))) {
  for (col in seq_len(ncol(input))) {
    pointer <- input[row, col]
    tot <- 1
    
    # check above
    if (row > 1) {
      distance <- compute_distance(input, seq(row - 1, 1), col)
      tot <- tot * distance
    }
    
    # check below
    if (row < nrow(input)) {
      distance <- compute_distance(input, seq(row + 1, nrow(input)))
      tot <- tot * distance
    }
    
    # check left
    if (col > 1) {
      distance <- compute_distance(input, row, seq(col - 1, 1))
      tot <- tot * distance
    }
    
    # check right 
    if (col < ncol(input)) {
      distance <- compute_distance(input, row, seq(col + 1, ncol(input)))
      tot <- tot * distance
    }
    
    scenic_map[row, col] <- tot
  }
}

max(scenic_map)


