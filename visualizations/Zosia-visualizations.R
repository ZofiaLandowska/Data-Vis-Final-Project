rm(list = ls())

library(ggplot2)
library(seriation)
library(tidyverse)
library(vcd)
library(maps)
library(ggrepel)

accidents <- read.csv("data/US_Accidents_March23_sampled_500k.csv")


# Four-Fold Display: Severity vs. Presence of Junction
# By states with most accidents 

accidents %>%
  mutate(Severity_bin = ifelse(Severity >= 3, "High", "Low")) %>%
  rename(`Severity Level` = Severity_bin) %>%
  filter(State %in% c("CA", "FL", "TX", "SC")) %>%
  count(State, `Severity Level`, Junction) %>%
  xtabs(n ~ `Severity Level` + Junction + State, data = .) %>%
  fourfold(
    conf_level = 0.95,
    margin = c(1, 2),
    main = "Accident Severity vs Junction Presence by State"
  )


# Contour Plots showing where accidents cluster in California and Florida (states with the highest number of accidents)

# Create state map
california_map <- map_data("state") %>% filter(region == "california")

# Filter for accidents in California 
accidents_california <- accidents %>% filter(State == "CA")

# 5 biggest cities in California
# Los Angeles, San Diego, San Jose, San Francisco, and Fresno
# city coordinates:
data(world.cities)
cities_ca <- world.cities %>%
  filter(name %in% c("Los Angeles", "San Diego", "San Jose", "San Francisco", "Fresno"),
         country.etc == "USA") %>%
  select(name, long, lat)

shared_fill_scale <- scale_fill_gradientn(
  colors = c("white", "yellow", "orange", "red"),
  limits = c(0.0, 1.1), # both maps will use this exact density ceiling
  name = "Density Level"
)

ggplot() +
  geom_polygon(data = california_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", linewidth = 0.5) +
  stat_density_2d(data = accidents_california,aes(x = Start_Lng, y = Start_Lat, fill = after_stat(level)),
                  geom = "polygon", alpha = 0.8) +
  geom_point(data = cities_ca, aes(x = long, y = lat), color = "black", size = 1) +
  geom_label_repel(data = cities_ca, aes(x = long, y = lat, label = name), size = 3) +
  shared_fill_scale +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Accident Density in California", subtitle = "Dark red areas have the most accidents")

# Contour Plot showing where accidents cluster in the state of Florida

# Create state map
florida_map <- map_data("state") %>% filter(region == "florida")

# Filter for accidents in Florida 
accidents_florida <- accidents %>% filter(State == "FL")

# 5 biggest cities in Florida
# Jacksonville, Miami, Tampa, Orlando, and St. Petersburg
# city coordinates:
data(world.cities)
cities_fl <- world.cities %>%
  filter(name %in% c("Jacksonville", "Miami", "Tampa", "Orlando", "St Petersburg"),
         country.etc == "USA") %>%
  # only keep the Jacksonville in Florida by filtering out anything too far north
  filter(!(name == "Jacksonville" & lat > 31)) %>% 
  select(name, long, lat)

ggplot() +
  geom_polygon(data = florida_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", linewidth = 0.5) +
  stat_density_2d(data = accidents_florida,aes(x = Start_Lng, y = Start_Lat, fill = after_stat(level)),
                  geom = "polygon", alpha = 0.8) +
  geom_point(data = cities_fl, aes(x = long, y = lat), color = "black", size = 1) +
  geom_label_repel(data = cities_fl, aes(x = long, y = lat, label = name), size = 3) +
  shared_fill_scale +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Accident Density in Florida", subtitle = "Dark red areas have the most accidents")

# -------------
# Map of California using a different scale than the map of Florida 

ggplot() +
  geom_polygon(data = california_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", linewidth = 0.5) +
  stat_density_2d(data = accidents_california,aes(x = Start_Lng, y = Start_Lat, fill = after_stat(level)),
                  geom = "polygon", alpha = 0.8) +
  geom_point(data = cities_ca, aes(x = long, y = lat), color = "black", size = 1) +
  geom_label_repel(data = cities_ca, aes(x = long, y = lat, label = name), size = 3) +
  scale_fill_gradientn(
    colors = c("white", "yellow", "orange", "red"),
    name = "Density Level") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Accident Density in California", subtitle = "Dark red areas have the most accidents")

