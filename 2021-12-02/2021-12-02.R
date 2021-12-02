library(tidyverse)
library(here)

# Part 1
input_2 <- read_lines(file = here("2021-12-02/input/input_2.txt"))

controlls <- tibble(commands = input_2) %>% 
  mutate(
    split_command = str_split(commands, pattern = " ", n = 2)
         ) %>% 
  unnest_wider(split_command) %>% 
  rename(direction = 2, length = 3) %>% 
  mutate(length = as.numeric(length),
         length = if_else(direction == 'up',
                          length * -1,
                          length))

horizontal <- controlls %>% 
  filter(direction == "forward") %>%
  summarise(value = sum(length)) %>% 
  pull(value)

depth <- controlls %>% 
  filter(direction %in% c("down", "up")) %>%
  summarise(value = sum(length)) %>% 
  pull(value)

glue::glue("final horizontal position by final depth: {horizontal*depth}")  

# Part 2

horizontal_pos = 0
aim = 0
depth_pos = 0

for (i in 1:length(controlls$commands)) {
  if (controlls$direction[i] == "forward") {
    horizontal_pos <- horizontal_pos + controlls$length[i]
    depth_pos <- depth_pos + aim * controlls$length[i]
  }
  
  if (controlls$direction[i] %in% c("up", "down")) {
    aim <- aim + controlls$length[i]
  }
}

glue::glue("final horizontal position by final depth: {horizontal_pos*depth_pos}")