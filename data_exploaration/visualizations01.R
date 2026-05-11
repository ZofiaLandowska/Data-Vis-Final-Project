library(dplyr)
library(lubridate)
library(corrplot)
library(ggplot2)
library(maps)

accidents <- read.csv("US_Accidents_March23_sampled_500k.csv")

# get dates out, fix severity
accidents <- accidents %>% mutate(
  Start_Time = ymd_hms(Start_Time),
  End_Time   = ymd_hms(End_Time),
  Hour       = hour(Start_Time),
  Month      = month(Start_Time, label = TRUE),
  Year       = year(Start_Time),
  DayOfWeek  = wday(Start_Time, label = TRUE),
  Severity   = as.factor(Severity))

head(accidents)

# check for NAs, maybe drop certain columns?
colSums(is.na(accidents))

# Severity bar chart
ggplot(accidents, aes(x = Severity, fill = Severity)) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Accident Severity",
       x = "Severity Level", y = "Count") +
  theme(legend.position = "none")

# states wtih most accidents
accidents %>%
  count(State, sort=TRUE) %>%
  slice_head(n=10) %>%
  ggplot(aes(x=reorder(State, n), y=n, fill=n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 States with Most Accidents",
       x = "State", y = "Accident Count") +
  theme(legend.position = "none")

# accidents by hour of the day
accidents %>%
  count(Hour) %>%
  ggplot(aes(x = Hour, y = n)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Accidents by hour of day",
       x = "Hour (0–23)", y = "Count")

# accidents by day of the week
accidents %>%
  count(DayOfWeek) %>%
  ggplot(aes(x = DayOfWeek, y = n)) +
  geom_col(fill="blue") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Accidents by Day of Week",
       x = "Day", y = "Count") +
  theme(legend.position = "none")

# choropleth of accidents
state_counts <- accidents %>%
  count(State) %>%
  mutate(region = tolower(state.name[match(State, state.abb)]))
map_data("state") %>%
  left_join(state_counts, by = "region") %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(color = "black", linewidth = 0.3) +
  scale_fill_gradient(low = "white", high = "red", labels = scales::comma) +
  coord_fixed(1.3) +
  labs(title = "Accident Count by State", fill = "Count") +
  theme_void()
