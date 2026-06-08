# Milestone 2

###### Preprocessing ######

# import libs
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(corrplot)
library(ggplot2)
library(ggmosaic)
library(maps)
library(scales)
library(tigris)
library(sf)

# load dataset
accidents <- read.csv("US_Accidents_March23_sampled_500k.csv")

# format dates and times, convert severity to factor
accidents <- accidents %>% mutate(
  Start_Time = ymd_hms(Start_Time),
  End_Time = ymd_hms(End_Time),
  Hour = hour(Start_Time),
  Month = month(Start_Time, label=TRUE),
  Year = year(Start_Time),
  DayOfWeek = wday(Start_Time, label=TRUE),
  Severity = as.factor(Severity))

head(accidents)

###### Exploratory Visualizations #######

# accidents by hour of the day
accidents %>%
  count(Hour) %>%
  ggplot(aes(x=Hour, y=n)) +
  geom_col(fill="steelblue") +
  scale_x_continuous(breaks=0:23) +
  labs(title = "Accidents by Hour of the Day",
       x = "Hour",
       y = "Number of Accidents")

# accidents by day of the week
accidents %>%
  filter(!is.na(DayOfWeek)) %>%
  count(DayOfWeek) %>%
  ggplot(aes(x=DayOfWeek, y=n)) +
  geom_col(fill="steelblue") +
  labs(title = "Accidents by Day of the Week",
       x = NULL,
       y = "Number of Accidents") +
  theme(legend.position = "none")


# aggregation of states
state_counts <- accidents %>%
  count(State) %>%
  mutate(region = tolower(state.name[match(State, state.abb)]))

# choropleth of accident frequency
map_data("state") %>%
  left_join(state_counts, by="region") %>%
  ggplot(aes(x=long, y=lat, group=group, fill=n)) +
  geom_polygon(color="black", linewidth=0.3) +
  scale_fill_gradient(low="white", high="red",
                      labels=scales::comma,
                      breaks=c(min(state_counts$n), 30000, 60000, 
                               90000, max(state_counts$n))) +
  coord_fixed(1.3) +
  labs(title = "Accident Count by State", fill = "Count") +
  theme_void()

# top 10 states by accidents bar chart
accidents %>%
  count(State, sort=TRUE) %>%
  slice_head(n=10) %>%
  ggplot(aes(x=reorder(State, n), y=n, fill=n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 States with Most Accidents",
       x = "State",
       y = "Accident Count") +
  theme(legend.position = "none")

# severity bar chart
ggplot(accidents, aes(x=Severity, fill=Severity)) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Accident Severity",
       x = "Severity Level",
       y = "Accident Count") +
  theme(legend.position = "none")



# Milestone 3-4

###### Explanatory Visualizations ######




# 2020 census population
pop2020 <- c(AL = 5024279,  AK = 733391,   AZ = 7151502,  AR = 3011524,
             CA = 39538223, CO = 5773714,  CT = 3605944,  DE = 989948,
             FL = 21538187, GA = 10711908, HI = 1455271,  ID = 1839106,
             IL = 12812508, IN = 6785528,  IA = 3190369,  KS = 2937880,
             KY = 4505836,  LA = 4657757,  ME = 1362359,  MD = 6177224,
             MA = 7029917,  MI = 10077331, MN = 5706494,  MS = 2961279,
             MO = 6154913,  MT = 1084225,  NE = 1961504,  NV = 3104614,
             NH = 1377529,  NJ = 9288994,  NM = 2117522,  NY = 20201249,
             NC = 10439388, ND = 779094,   OH = 11799448, OK = 3959353,
             OR = 4237256,  PA = 13002700, RI = 1097379,  SC = 5118425,
             SD = 886667,   TN = 6910840,  TX = 29145505, UT = 3271616,
             VT = 643077,   VA = 8631393,  WA = 7705281,  WV = 1793716,
             WI = 5893718,  WY = 576851,   DC = 689545)

# state data aggregated
state_counts <- accidents %>%
  count(State) %>%
  mutate(region = tolower(state.name[match(State, state.abb)]),
         pop = pop2020[State],
         accidents_per_100k = (n/pop)*100000)



# national rate of accidents
national_rate <- (sum(state_counts$n, na.rm = TRUE) / sum(state_counts$pop, na.rm = TRUE)) * 100000




# gamma priors
alpha_0 <- 5
beta_0 <- (alpha_0/national_rate)




# compute bayesian surprise
state_counts <- state_counts %>%
  filter(!is.na(pop)) %>%
  mutate(alpha_1 = (alpha_0+n),
         beta_1 = (beta_0+(pop/100000)),
         post_mean = (alpha_1/beta_1),
         surprise_int = (alpha_1-alpha_0)*digamma(alpha_1) 
         - lgamma(alpha_1) + lgamma(alpha_0) + alpha_0 * log(beta_1/beta_0)
         + alpha_1 * (beta_0/beta_1-1),
         surprise = sign(post_mean-national_rate)*surprise_int)




# make sure state counts meet threshold
state_counts <- state_counts %>%
  mutate(surprise = ifelse(n < 25, NA_real_, surprise))



# map pipeline
allStates <- states(cb=TRUE, resolution="5m", year=2020) %>%
  shift_geometry() %>%
  # drop territories
  filter(!STUSPS %in% c("PR", "GU", "VI", "MP", "AS")) %>%  
  left_join(state_counts, by=c("STUSPS"="State")) %>%
  mutate(accidents_per_100k=ifelse(n < 25, NA_real_, accidents_per_100k))



# range
rate_min <- min(allStates$accidents_per_100k, na.rm=TRUE)
rate_max <- max(allStates$accidents_per_100k, na.rm=TRUE)
max_abs <- max(abs(allStates$surprise), na.rm=TRUE)



# VIZ 1 - Accident Rate Choropleth
ggplot(allStates) +
  geom_sf(aes(fill=accidents_per_100k), color="black", linewidth=0.3) +
  scale_fill_gradientn(colors=c("lightyellow", "orange", "red"),
                       trans="log10",
                       limits=c(rate_min, rate_max),
                       breaks=c(rate_min, 15, 25, 50, 100, 200, rate_max),
                       labels=~comma(round(.x)),
                       na.value="grey50",
                       name="Accidents",
                       guide=guide_colorbar(direction="horizontal",
                                              title.position="top",
                                              title.hjust=0.5,
                                              barwidth=unit(15, "cm"),
                                              barheight=unit(0.4, "cm"))) +
  labs(title = "US Traffic Accidents",
       subtitle = "Per 100,000 Residents",
       caption  = paste0("*Grey indicates sparse coverage")) +
  theme_void() +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5),
        plot.caption = element_text(size=10, color="black", hjust=1),
        legend.position = "bottom",
        legend.title = element_text(size=12, face="bold", hjust=0.5),
        legend.text = element_text(size=10, color="grey20"),
        legend.margin = margin(t=5),
        plot.margin = margin(10, 10, 10, 10))



# VIZ 1 - BS Choropleth
max_abs <- max(abs(allStates$surprise), na.rm=TRUE)

ggplot(allStates) +
  geom_sf(aes(fill=surprise), color="black", linewidth=0.3) +
  scale_fill_gradient2(low = "steelblue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       limits = c(-max_abs, max_abs),
                       breaks = c(-max_abs, -5, 0, 5, max_abs),
                       labels = ~comma(round(.x)),
                       na.value = "grey50",
                       name = "Bayesian Surprise",
                       guide = guide_colorbar(direction="horizontal",
                                              title.position="top",
                                              title.hjust=0.5,
                                              barwidth=unit(15, "cm"),
                                              barheight=unit(0.4, "cm"))) +
  labs(title = "Bayesian Surprise in Accident Rates",
       caption = "*Grey indicates sparse coverage") +
  theme_void() +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5),
        plot.caption = element_text(size=10, color="black", hjust=1),
        legend.position = "bottom",
        legend.title = element_text(size=12, face="bold", hjust=0.5),
        legend.text = element_text(size=10, color="grey20"),
        legend.margin = margin(t=5),
        plot.margin = margin(10, 10, 10, 10))




# VIZ 2 - Severity Choropleth:

# proportion of high severity accidents by state
severity_counts <- accidents %>%
  group_by(State) %>%
  summarise(n=n(), n_severe=sum(as.numeric(as.character(Severity)) >= 3, na.rm=TRUE)) %>%
  mutate(pop=pop2020[State], prop_severe=n_severe/n) %>%
  filter(!is.na(pop))

# national proportion of high severity accidents
p_national <- sum(severity_counts$n_severe)/sum(severity_counts$n)
# priors
concentration <- 10
alpha_0 <- concentration*p_national
beta_0 <- concentration*(1-p_national)

# severity surprise
severity_counts <- severity_counts %>%
  mutate(alpha_1 = alpha_0+n_severe,
         beta_1  = beta_0+(n-n_severe),
         post_mean = alpha_1/(alpha_1+beta_1),
         surprise_int = lbeta(alpha_0, beta_0) - lbeta(alpha_1, beta_1)
         + (alpha_1-alpha_0) * (digamma(alpha_1) - digamma(alpha_1+beta_1))
         + (beta_1-beta_0) * (digamma(beta_1) - digamma(alpha_1+beta_1)),
         surprise = sign(post_mean-p_national) * surprise_int)

# mask sparse states
severity_counts <- severity_counts %>%
  mutate(prop_severe = ifelse(n<25, NA_real_, prop_severe),
         surprise = ifelse(n<25, NA_real_, surprise))

allStates_sev <- allStates %>%
  select(STUSPS, geometry) %>%
  left_join(severity_counts, by = c("STUSPS" = "State"))

# scale ranges
sev_min <- min(allStates_sev$prop_severe, na.rm=TRUE)
sev_max <- max(allStates_sev$prop_severe, na.rm=TRUE)
sev_abs <- max(abs(allStates_sev$surprise), na.rm=TRUE)

# high severity map
ggplot(allStates_sev) +
  geom_sf(aes(fill=prop_severe), color="black", linewidth=0.3) +
  scale_fill_gradientn(colors = c("lightyellow", "orange", "red"),
                       limits = c(sev_min, sev_max),
                       breaks = c(sev_min, 0.6, 0.7, 0.8, sev_max),
                       labels = ~percent(.x, accuracy=1),
                       na.value = "grey50",
                       name = "Proportion severity 3 or 4",
                       guide = guide_colorbar(direction="horizontal",
                              title.position="top",
                              title.hjust=0.5,
                              barwidth=unit(15, "cm"),
                              barheight=unit(0.4, "cm"))) +
  labs(title = "Percentage of High Severity Traffic Accidents",
       caption = "*Grey indicates sparse coverage") +
  theme_void() +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5),
        plot.caption = element_text(size=10, color="black", hjust=1),
        legend.position = "bottom",
        legend.title = element_text(size=12, face="bold", hjust=0.5),
        legend.text = element_text(size=10, color="grey20"),
        legend.margin = margin(t=5),
        plot.margin = margin(10, 10, 10, 10))


# VIZ 2 - Bayesian Surprise severity choropleth
ggplot(allStates_sev) +
  geom_sf(aes(fill=surprise), color="black", linewidth=0.3) +
  scale_fill_gradient2(low = "steelblue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       limits = c(-sev_abs, sev_abs),
                       breaks = c(-sev_abs, 0, sev_abs),
                       labels = ~comma(round(.x, 1)),
                       na.value = "grey50",
                       name = "Bayesian Surprise",
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barwidth = unit(15, "cm"),
                                              barheight = unit(0.4, "cm"))) +
  labs(title = "Bayesian Surprise in Accident Severity",
       caption = "*Grey indicates sparse coverage") +
  theme_void() +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5),
        plot.caption = element_text(size=10, color="black", hjust=1),
        legend.position = "bottom",
        legend.title = element_text(size=12, face="bold", hjust=0.5),
        legend.text = element_text(size=10, color="grey20"),
        legend.margin = margin(t=5),
        plot.margin = margin(10, 10, 10, 10))




# VIZ 3 - Heatmap

# aggregation
hourday_acc <- accidents %>%
  group_by(DayOfWeek, Hour) %>%
  summarise(crashes=n(), .groups="drop") %>%
  mutate(DayOfWeek=factor(DayOfWeek, levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# heatmap
ggplot(hourday_acc, aes(x=DayOfWeek, y=Hour, fill=crashes)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma",
                       name = "Accidents",
                       breaks = c(min(hourday_acc$crashes), 3000, 5500, max(hourday_acc$crashes)),
                       labels = ~comma(round(.x))) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 23),
                     labels = c("12am", "4am", "8am", "12pm", "4pm", "8pm", "12am"),
                     trans = "reverse") +
  labs(title="Car Accidents by Hour and Day of Week") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face="bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.key.height = unit(2, "cm"))




# VIZ 4 - Radial Plot
# Define regions
southern_states <- c("AL", "AR", "FL", "GA", "KY", "LA", "MS",
                     "NC", "SC", "TN", "VA", "WV", "TX", "OK")

northern_states  <- c("CT", "IL", "IN", "IA", "ME", "MA", "MI",
                      "MN", "NH", "NJ", "NY", "OH", "PA", "RI",
                      "VT", "WI")

# Create a month column
accidents <- accidents %>% mutate(Month = month(as.POSIXct(Start_Time), label = TRUE))

south_data <- accidents %>%
  filter(State %in% southern_states) %>%
  group_by(Month) %>%
  summarize(count = n()) %>%
  mutate(Region = "South")

north_data <- accidents %>%
  filter(State %in% northern_states) %>%
  group_by(Month) %>%
  summarize(count = n()) %>%
  mutate(Region = "North")

combined_data <- bind_rows(south_data, north_data)

# Calculate breaks
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



# VIZ 5 - Mosaic plot 1:

# bin accident times and make factors
accidents <- accidents %>%
  mutate(Time = case_when(Hour >= 0  & Hour < 3  ~ "12AM - 3AM",
                          Hour >= 3  & Hour < 6  ~ "3AM - 6AM",
                          Hour >= 6  & Hour < 9  ~ "6AM - 9AM",
                          Hour >= 9  & Hour < 12 ~ "9AM - 12PM",
                          Hour >= 12 & Hour < 15 ~ "12PM - 3PM",
                          Hour >= 15 & Hour < 18 ~ "3PM - 6PM",
                          Hour >= 18 & Hour < 21 ~ "6PM - 9PM",
                          Hour >= 21 ~ "9PM - 12AM"),
         Time = factor(Time, levels = c("12AM - 3AM",
                                        "3AM - 6AM",
                                        "6AM - 9AM",
                                        "9AM - 12PM",
                                        "12PM - 3PM",
                                        "3PM - 6PM",
                                        "6PM - 9PM",
                                        "9PM - 12AM")),
         Severity = factor(Severity))

# make mosaic plot
ggplot(data=accidents) +
  geom_mosaic(aes(x=product(Severity, Time), fill=Severity)) +
  labs(title="Car Accident Severity by Time of Day",
       y="Severity",
       fill="Severity") +
  # color palette
  scale_fill_manual(values = c("1"="blue",
                               "2"="steelblue",
                               "3"="orange",
                               "4"="red")) +
  # no grey background
  theme_minimal() +
  # angle x-axis labels
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(size=17, hjust=0.5)) # put title in middle


# Viz 6:
# Massiel's code
