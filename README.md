# Data-Vis-Final-Project

library(dplyr)
library(lubridate)
library(corrplot)
library(ggplot2)

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
