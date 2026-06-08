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

# -------------
accidents <- read.csv("data/US_Accidents_March23_sampled_500k.csv")
# Create a month column
accidents <- accidents %>% mutate(Month = month(as.POSIXct(Start_Time), label = TRUE))

# Radial chart of number of accidents by month

acc_by_month <- accidents %>%
  group_by(Month) %>%
  summarize(count = n())

# Calculate min and max
min_count <- min(acc_by_month$count)
max_count <- max(acc_by_month$count)
mid_count <- round(mean(c(min_count, max_count)), -3)

breaks_vals <- c(min_count, mid_count, max_count)

ggplot(acc_by_month, aes(x = Month, y = count, fill = count)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  scale_fill_gradient(
    low = "#132B43", high = "#56B1F7",
    breaks = breaks_vals,
    labels = function(x) ifelse(x == min_count, paste0(x),
                                ifelse(x == max_count, paste0(x), x))
  ) +
  coord_polar() + 
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  labs(title = "Number of Accidents by Month", x=NULL, y=NULL, fill="Number of Accidents") +
  theme(plot.title = element_text(size = 17, hjust = 0.5),
        axis.text = element_text(size = 10))

# Define regions
southern_states <- c("AL", "AR", "FL", "GA", "KY", "LA", "MS",
                     "NC", "SC", "TN", "VA", "WV", "TX", "OK")

northern_states  <- c("CT", "IL", "IN", "IA", "ME", "MA", "MI",
                      "MN", "NH", "NJ", "NY", "OH", "PA", "RI",
                      "VT", "WI")


# Filter accident data
south_data <- accidents %>%
  filter(State %in% southern_states) %>%
  group_by(Month) %>%
  summarize(count = n())

north_data <- accidents %>%
  filter(State %in% northern_states) %>%
  group_by(Month) %>%
  summarize(count = n())

# Calculate breaks per region so the legend fits the data
south_breaks <- c(min(south_data$count),
                  round(mean(range(south_data$count)), -3),
                  max(south_data$count))

north_breaks <- c(min(north_data$count),
                  round(mean(range(north_data$count)), -3),
                  max(north_data$count))


ggplot(south_data, aes(x = Month, y = count, fill = count)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  scale_fill_gradient(
    low = "#132B43", high = "#56B1F7",
    breaks = south_breaks,
    labels = function(x) ifelse(x == min_count, paste0(x),
                                ifelse(x == max_count, paste0(x), x))
  ) +
  coord_polar() + 
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  labs(title = "Number of Accidents by Month in Southern States", x=NULL, y=NULL, fill="Number of Accidents") +
  theme(plot.title = element_text(size = 17, hjust = 0.5),
        axis.text = element_text(size = 10))

ggplot(north_data, aes(x = Month, y = count, fill = count)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  scale_fill_gradient(
    low = "#132B43", high = "#56B1F7",
    breaks = north_breaks,
    labels = function(x) ifelse(x == min_count, paste0(x),
                                ifelse(x == max_count, paste0(x), x))
  ) +
  coord_polar() + 
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  labs(title = "Number of Accidents by Month in Northern States", x=NULL, y=NULL, fill="Number of Accidents") +
  theme(plot.title = element_text(size = 17, hjust = 0.5),
        axis.text = element_text(size = 10))


# Calculate breaks from combined data
min_count <- min(combined_data$count)
max_count <- max(combined_data$count)
mid_count <- round(mean(c(min_count, max_count)), -3)
breaks_vals <- c(min_count, mid_count, max_count)

# Shared y-axis ceiling
y_max <- max(combined_data$count)

# Plot
ggplot(combined_data, aes(x = Month, y = count, fill = count)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  scale_fill_gradient(
    low = "#132B43", high = "#56B1F7",
    breaks = breaks_vals,
    labels = c(min_count, mid_count, max_count)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  coord_polar() +
  facet_wrap(~ Region) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 13, face = "bold")
  ) +
  labs(title = "Number of Accidents by Month", x = NULL, y = NULL, fill = "Number of Accidents")

# Bar chart of most common road features at accident sites 

road_features <- c("Amenity", "Bump", "Crossing", "Give_Way", "Junction", 
                   "No_Exit", "Railway", "Station", "Stop", "Traffic_Signal")

acc_road_features <- accidents %>%
  pivot_longer(cols = all_of(road_features), names_to = "Feature", values_to = "Present") %>%
  filter(Present == "True") %>%
  group_by(Feature) %>%
  summarise(count = n())

ggplot(acc_road_features, aes(x = reorder(Feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Common Road Features at Accident Sites", x = "Road Feature", y = "Number of Accidents")

# Accident patterns by year - normalize results 
acc_by_year <- accidents %>%
  mutate(
    Year = year(as.POSIXct(Start_Time)),
    Hour = hour(as.POSIXct(Start_Time)),
    DayOfWeek = wday(as.POSIXct(Start_Time), label = TRUE, abbr = TRUE)
  ) %>%
  filter(Year %in% c(2019, 2020, 2021, 2022)) %>%
  group_by(Year, DayOfWeek, Hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup() %>%
  mutate(DayOfWeek = factor(DayOfWeek, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))


ggplot(acc_by_year, aes(x = DayOfWeek, y = Hour, fill = percentage)) +
  geom_tile() +
  facet_wrap(~ Year) +
  scale_fill_viridis_c(option = "magma", labels = scales::percent) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24),
                     labels = c("12am", "4am", "8am", "12pm", "4pm", "8pm", "12am")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Accident Patterns (2019–2022)",
       subtitle = "Each cell shows the percentage of that year's total accidents",
       x = NULL, y = NULL, fill = "Percentage of \nyearly accidents")









