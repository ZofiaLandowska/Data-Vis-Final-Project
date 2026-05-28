# Homework 4/Milestone 3
# Alexander Williams

# 1)
# import libs
library(ggplot2)
library(ggmosaic)
library(dplyr)

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



# 2)
# aggregate accidents by hour and day of week
hourday_acc <- accidents %>%
  group_by(DayOfWeek, Hour) %>%
  summarise(crashes=n(), .groups="drop") %>%
  mutate(DayOfWeek=factor(DayOfWeek, levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# heat map
ggplot(hourday_acc, aes(x=DayOfWeek, y=Hour, fill=crashes)) +
  geom_tile() +
  scale_fill_viridis_c(option="plasma", name="Accidents") +
  scale_y_continuous(
    breaks=seq(0, 23, by=1),
    labels=c("12am", "1am", "2am", "3am", "4am", "5am","6am", "7am", 
             "8am", "9am", "10am", "11am","12pm", "1pm", "2pm", "3pm", 
             "4pm", "5pm","6pm", "7pm", "8pm", "9pm", "10pm", "11pm")) +
  labs(title="Car Accidents by Hour and Day of Week", x="Day of Week", y="Hour of Day") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17, hjust = 0.5), # title big and in middle
        axis.text = element_text(size = 10)) # slightly bigger labels
