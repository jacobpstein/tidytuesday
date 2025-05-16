######################
# This file analyzes data from the Tidy Tuesday for May 12
# 
# Session info:
# R version 4.4.2 (2024-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sequoia 15.4.1
#
#
####################

# set seed
set.seed(5152025)

# load libraries
library(tidyverse)
library(spatialreg)
library(sf)
library(spdep)
library(janitor)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# load data
vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv') |> 
  # let's create some new columns, this hack comes from https://stackoverflow.com/questions/63677684/extract-date-and-time-from-datetime-field-in-r
  mutate(date = as.Date(time)
         , month = month(time)
         , hour = hour(time)
         , minute = minute(time)
         , second = second(time)
        ) |> 
  mutate(
    # going to create a new scaled version of the magnitude since the actual measure has negative values
     min_duration = abs(min(duration_magnitude_md, na.rm=T))) |> 
  rowwise() |> 
  mutate(duration_mag_scaled = duration_magnitude_md + min_duration
  ) |> 
  ungroup() |> 
  mutate(
    lagged_duration_scaled = lag(duration_mag_scaled)
    , lagged_duration = lag(duration_magnitude_md)
)

# GGally::ggpairs(vesuvius)

# looks like we have some missing values in our data
clean_ves <- vesuvius |> filter(!is.na(latitude) & is.na(duration_mag_scaled)!=T)

# create a spatial data frame
sf_df <- st_as_sf(clean_ves, coords = c("longitude", "latitude"), crs = 4326)

# Create point for Mt. Vesuvius
mt_v <- st_sfc(st_point(c(14.42600, 40.82167)), crs = 4326)

# put everything into meters
sf_eq_proj <- st_transform(sf_df, 32633)
mt_proj <- st_transform(mt_v, 32633)

# create new var taht is distance to Vesuvius
sf_eq_proj$dist_to_vesuvius <- as.numeric(st_distance(sf_eq_proj, mt_proj))

# let's get the largest distance by year
farthest_eqs <- sf_eq_proj |>
  group_by(year) |>
  mutate(n = n()) |> 
  filter(dist_to_vesuvius == max(dist_to_vesuvius, na.rm = TRUE)) |>
  ungroup() |>
  select(year, duration_mag_scaled, geometry, dist_to_vesuvius, n) |> 
  filter(year>=2015) # not a lot of earthquakes before this and the past 10 years is a nice window

# neat!
farthest_eqs |> 
  ggplot(aes(y = dist_to_vesuvius, x = factor(year), group = factor(year), col = factor(year))) +
  geom_point(aes(size = duration_mag_scaled)) + 

  
# let's clean that up
p1 <- sf_eq_proj |> 
  filter(year>=2015) |> 
  group_by(year) |> 
  mutate(fill = ifelse(dist_to_vesuvius == max(dist_to_vesuvius, na.rm = TRUE), "fill", "no-fill")) |> 
  ggplot(aes(y = dist_to_vesuvius, x = factor(year), group = factor(year), col = factor(year))) +
  geom_point(aes(size = duration_mag_scaled, fill = fill, alpha = fill), shape = 21, stroke = 1, col = "white") +
  scale_alpha_manual(values = c(1, 0.1)) +
  scale_size_continuous(range = c(.1, 15)) +
  # used ggannotate for this
  geom_text(data = data.frame(x = 3.0, y = 3166.24521626062, label = "Each point represents a\nseismic event"),
            mapping = aes(x = x, y = y, label = label),
            family = "Gill Sans", inherit.aes = FALSE, size = 4) +
  geom_text(data = data.frame(x = 6.1, y = 4247.56603054106, label = "Larger points are higher\nmagnitude events"),
            mapping = aes(x = x, y = y, label = label),
            family = "Gill Sans", inherit.aes = FALSE, size = 4) +
  geom_text(data = data.frame(x = c(8.9, 10.004168471596 ),
                              y = c(3800, 7400),
                              label = c("The top point for each year is\nthe farthest away from Vesuvius", "yikes!")),
            mapping = aes(x = x, y = y, label = label),
            family = "Gill Sans", inherit.aes = FALSE, size = 4) +
  usaidplot::usaid_plot(ppt = F) +
  scale_fill_manual(values = c("#FF5733", "#FF5733")) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = "", y = "Distance from Vesuvius (in meters)"
       , title = "Earthquakes are generally felt closer to Mt. Vesuvius, but every once\nand a while they get recorded much farther away"
       ) +
  theme(plot.title = element_text(size = 22))


ggsave("May_13/earthquakes.png", p1, dpi = 300, width = 10, height = 8)


# gave up on modeling but here's some code----
# create neighbors list--I chose 10 but that's random, could pick something else
coords <- st_coordinates(sf_df)
knn_nb <- knearneigh(coords, k = 10)
nb <- knn2nb(knn_nb)

# create spatial weights
listw <- nb2listw(nb, style = "W")

# moran’s I test
moran_test <- moran.mc(sf_df$duration_mag_scaled, listw, nsim=999)

# shows that we have spatial autocorrelation

moran.plot(sf_df$duration_mag_scaled, listw=listw)

mod1 <- lm(duration_mag_scaled ~ lagged_duration_scaled + dist_to_vesuvius, data = sf_eq_proj)

lm.morantest(mod1, listw) # this confirms that we do indeed have some spatial autocorrelation


mod2 <- lagsarlm(duration_mag_scaled ~ lagged_duration_scaled + dist_to_vesuvius
                 , data = sf_eq_proj
                 , listw = listw)

summary(mod2)

mod2_effects <- impacts(mod2, listw = listw, R = 999)

summary(fit.lag.effects, zstats = TRUE, short = TRUE)

