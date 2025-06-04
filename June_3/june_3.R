################
# Tidy Tuesday for June 3, 2025
# R version 4.4.2 (2024-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sequoia 15.5
#################

# load libraries
library(tidyverse)
library(ggforce)

# set seed
set.seed(642025)

# bring in data
gutenberg_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_authors.csv')

# drop missings
df_clean <- gutenberg_authors |>
  filter(is.finite(birthdate), is.finite(deathdate)) |>
  filter(!is.na(birthdate) & !is.na(deathdate)) |>
  filter(birthdate != deathdate)

# add century and round to decade
df_clean <- df_clean |>
  mutate(
    , birth_decade = floor(birthdate / 10) * 10
    , death_decade = floor(deathdate / 10) * 10
    , century = if_else(
      birth_decade < 0
      , paste0(abs(floor(birth_decade / 100)), "th BCE")
      , paste0(floor(birth_decade / 100) + 1, "th Century")
    )
  )

# Count people per birth–death decade pair
pair_counts <- df_clean |> 
  # this is roughly when Cervantes was born
  filter(birthdate>=1547) |> 
  count(birth_decade, death_decade, century, name = "n")

# Build 3-point bezier paths for each grouped pair
bezier_df <- pair_counts |>
  mutate(
    group = paste0(birth_decade, "-", death_decade)
    , xmid = (birth_decade + death_decade) / 2
    , ymid = abs(death_decade - birth_decade) / 2
  ) |>
  pmap(function(birth_decade, death_decade, century, n, group, xmid, ymid) {
    tibble(
      x = c(birth_decade, xmid, death_decade)
      , y = c(0, ymid, 0)
      , group = group
      , size = n
      , century = century
    )
  }) |>
  list_rbind() |>
  group_by(group) |>
  filter(n() == 3) |>
  ungroup()

# Plot 
p <- ggplot(bezier_df, aes(x = x, y = y, group = group)) +
  geom_bezier(aes(size = size, color = group), alpha = 0.7, lineend = "round") +
  scale_size(range = c(0.2, 4), guide = "none") +
  scale_color_viridis_d(option = "A", end = 0.9) +
  labs(
    title = "Writer Birth–Death Arcs Since the Birth of the Novel",
    subtitle = "Line thickness reflects number of people per birth–death decade pair",
    x = "", y = ""
    , caption = "Miguel Cervantes was born in 1547. While there is debate about 'the novel,'\nI'm considering Don Quixote the first modern novel and it's author's birth for filtering the data."
  ) +
  theme_minimal() +
  theme(legend.position = "NA"
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , panel.grid.major.y = element_blank()
    , title = element_text(family = 'Gill Sans', size = 26)
    , subtitle = element_text(family = 'Gill Sans', size = 18)
    , strip.text = element_text(size = 16)
  ) + facet_wrap(~century, scales = "free")

p

ggsave('writer_deaths.png', p, width = 16, height = 9, dpi = 1000)