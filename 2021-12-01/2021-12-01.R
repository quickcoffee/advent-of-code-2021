library(tidyverse)
library(slider)
library(here)

# Part 1
input_1 <- tibble(
  measurements = read_lines(here("2021-12-01/input/input_1.txt"))
) %>% 
  mutate(
    measurements = as.integer(measurements),
         is_larger = if_else(measurements > lag(measurements), 1, 0)
    )

larger_measurements <- input_1$is_larger %>% 
  sum(na.rm = TRUE)

print(glue::glue("There are {larger_measurements} larger measurmenets!"))

# Part 2

input_1 <- input_1 %>% 
  mutate(window =
           slide_int(
             .x = measurements,
             .f = sum,
             .after = 2,
             .complete = TRUE
             ),
         is_window_larger = if_else(window > lag(window), 1, 0)
         )

larger_windows <- input_1$is_window_larger %>% 
  sum(na.rm = TRUE)

print(glue::glue("There are {larger_windows} larger measurmenets!"))
