################################################################
# This file implements analysis and viz for
# the April 29, 2025 Tidy Tuesday data set
# Sesion Info:
# R version 4.4.2 (2024-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sequoia 15.4.1
################################################################

# load libraries
library(tidyverse) # the GOAT
library(readr) # for reading in data
library(scales) # for formatting %s
library(ggtext) # for custom title/legend
library(extrafont) # for custom fonts
library(ggstream) # doing all the work here

# set seed just in case
set.seed(4292025)

# read in data from github
user2025 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-29/user2025.csv')

# take a look at our data
glimpse(user2025)

# create a color palette inspired by LaCroix
# and inspired by https://github.com/johannesbjork/LaCroixColoR

lacroix_palette <- c(
  "#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF",  # pink, peach, yellow, mint, sky
  "#E0BBE4", "#957DAD", "#D291BC", "#FEC8D8", "#FFDFD3",  # purple tones
  "#FF9AA2", "#FFB7B2", "#FFDAC1", "#E2F0CB", "#B5EAD7",  # warm-cool pastels
  "#C7CEEA", "#AEC6CF", "#FFDAB9", "#C1F0F6", "#F4B6C2",  # blues & peachy
  "#B2F7EF", "#E3F2FD", "#FADADD"                        # aqua, baby blue, rose
)


# let's just look at session type by day
p <- user2025 |> 
  filter(time!="TBD") |> 
  mutate(date = lubridate::wday(date, label = T)) |>
  group_by(date, time, session) |> 
  count() |> 
  ungroup() |> 
  group_by(date, time) |> 
  mutate(perct = n/sum(n)
         , date = factor(date, levels = c("Fri", "Sat", "Sun"))
  ) |> 
  ungroup() |> 
  ggplot(aes(x = time, y = perct, fill = session, label = session)) +
  geom_stream(alpha = 0.95
              , sorting = "onset"
              , true_range = "none"
              , bw = 0.8) +
  geom_stream_label(size = 4,  family = "Gill Sans", color = "#000000", fontface = "bold") +
  usaidplot::usaid_plot(data_type = "discrete", ppt = TRUE) +
  scale_fill_manual(values = lacroix_palette) +
  theme(axis.text.y = element_blank()
        , panel.grid.major.y = element_blank()) +
  labs(x = "", y = "", title = "Distribution of UseR sessions by time", subtitle = "The 10:30 time slot is packed with good stuff!")
  
ggsave("streamgraph.png", p, dpi = 300, width = 14, height = 9)
  