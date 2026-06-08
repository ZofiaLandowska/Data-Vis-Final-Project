# waffle chart: accident distribution across categories
# frequency of each weather condition
accidents %>% count(Weather_Condition, sort = TRUE)

# removing missing values
accidents_clean <- accidents %>% filter(!is.na(Weather_Condition))

# verifying counts
accidents_clean %>% count(Weather_Condition, sort = TRUE)

# grouping weather into broader categories
grouped_weather <- accidents_clean %>%
  mutate(
    Weather = case_when(
      Weather_Condition %in% c(
        "Fair", "Clear", "Fair / Windy"
      ) ~ "Clear",
      Weather_Condition %in% c(
        "Cloudy", "Mostly Cloudy", "Partly Cloudy",
        "Overcast", "Scattered Clouds",
        "Cloudy / Windy", "Mostly Cloudy / Windy",
        "Partly Cloudy / Windy"
      ) ~ "Cloudy",
      Weather_Condition %in% c(
        "Rain", "Light Rain", "Heavy Rain",
        "Light Drizzle", "Drizzle",
        "Rain / Windy", "Light Rain / Windy",
        "Heavy Rain / Windy",
        "Light Rain with Thunder",
        "Light Thunderstorms with Rain",
        "Thunderstorms and Rain"
      ) ~ "Rainy",
      Weather_Condition %in% c(
        "Snow", "Light Snow", "Heavy Snow",
        "Snow / Windy", "Light Snow / Windy",
        "Heavy Snow / Windy",
        "Wintry Mix", "Sleet", "Ice Pellets",
        "Light Freezing Rain", "Freezing Rain"
      ) ~ "Snow/Ice",
      Weather_Condition %in% c(
        "Fog", "Mist", "Haze", "Smoke",
        "Fog / Windy", "Haze / Windy",
        "Patches of Fog", "Shallow Fog"
      ) ~ "Fog/Haze",
      Weather_Condition %in% c(
        "T-Storm", "Thunder", "Thunderstorm",
        "Heavy T-Storm",
        "Thunder in the Vicinity",
        "T-Storm / Windy"
      ) ~ "Thunderstorm",
      TRUE ~ "Other"
    )
  )

# number of accidents in each category
grouped_weather <- grouped_weather %>% group_by(Weather) %>% summarise(Total = n()) %>% arrange(desc(Total))

# percentage of accidents in each category
grouped_weather <- grouped_weather %>% mutate(Percentage = Total / sum(Total) * 100)

# installing the waffle package since it was not in R libraries
if (!require("remotes")) install.packages("remotes")
remotes::install_github("hrbrmstr/waffle")

# accident distribution waffle chart
ggplot(grouped_weather,
       aes(fill = Weather, values = Percentage)) +
  geom_waffle(color = "white", size = 0.5, n_rows = 10) +
  scale_fill_manual(values = c(
    Clear = "#EFB743",
    Cloudy = "#748b97",
    Rainy = "#4269D0",
    "Snow/Ice" = "#97BBF5",
    "Fog/Haze" = "#9498D8",
    Thunderstorm = "#984EA3",
    Other = "#BDBDBD"
  )) +
  labs(
    title = "Distribution of Traffic Accidents by Weather Condition",
    fill = "Weather Condition"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# stakced bar chart: severity across categories
# missing values
sum(is.na(accidents_severity$Severity))

# clean
accidents_severity_clean <- accidents_severity %>% filter(!is.na(Weather_Condition))
sum(is.na(accidents_severity_clean$Weather_Condition))

# grouping weather into broader categories
accidents_severity_grouped <- accidents_severity_clean %>%
  mutate(
    Weather = case_when(
      
      Weather_Condition %in% c(
        "Fair", "Clear", "Fair / Windy"
      ) ~ "Clear",
      
      Weather_Condition %in% c(
        "Cloudy", "Mostly Cloudy", "Partly Cloudy",
        "Overcast", "Scattered Clouds",
        "Cloudy / Windy", "Mostly Cloudy / Windy",
        "Partly Cloudy / Windy"
      ) ~ "Cloudy",
      
      Weather_Condition %in% c(
        "Rain", "Light Rain", "Heavy Rain",
        "Light Drizzle", "Drizzle",
        "Rain / Windy", "Light Rain / Windy",
        "Heavy Rain / Windy",
        "Light Rain with Thunder",
        "Light Thunderstorms with Rain",
        "Thunderstorms and Rain"
      ) ~ "Rainy",
      
      Weather_Condition %in% c(
        "Snow", "Light Snow", "Heavy Snow",
        "Snow / Windy", "Light Snow / Windy",
        "Heavy Snow / Windy",
        "Wintry Mix", "Sleet", "Ice Pellets",
        "Light Freezing Rain", "Freezing Rain"
      ) ~ "Snow/Ice",
      
      Weather_Condition %in% c(
        "Fog", "Mist", "Haze", "Smoke",
        "Fog / Windy", "Haze / Windy",
        "Patches of Fog", "Shallow Fog"
      ) ~ "Fog/Haze",
      
      Weather_Condition %in% c(
        "T-Storm", "Thunder", "Thunderstorm",
        "Heavy T-Storm",
        "Thunder in the Vicinity",
        "T-Storm / Windy"
      ) ~ "Thunderstorm",
      
      TRUE ~ "Other"
    )
  )
# accidents by weather condition and severity level
severity_weather <- accidents_severity_grouped %>% group_by(Weather, Severity) %>% summarise(Count = n(), .groups = "drop")

# severity normalized stacked bar chart
ggplot(severity_weather,
       aes(x = Weather, y = Count,
           fill = factor(Severity))) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "1" = "green",
    "2" = "gold",
    "3" = "orange",
    "4" = "red"
  )) +
  labs(
    title = "Severity Distribution Across Weather Conditions",
    x = "Weather Condition",
    y = "Percentage of Accidents",
    fill = "Severity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold")
  )
