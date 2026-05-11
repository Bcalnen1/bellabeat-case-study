# Bellabeat Case Study
# Smart Device Usage Analysis
# Google Data Analytics Capstone Project

# Author: Breanna M. Calnen
# Tools Used: R, tidyverse, dplyr, ggplot2

library(tidyverse)
library(gridExtra)
daily_activity <- read_csv("dailyActivity_merged_2nd.csv")
head(daily_activity)
glimpse(daily_activity)
summary(daily_activity)
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")
daily_activity$day_of_week <- weekdays(daily_activity$ActivityDate)
daily_activity$day_of_week <- factor(daily_activity$day_of_week, levels = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"))
sum(duplicated(daily_activity))
daily_steps_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = mean(TotalSteps, na.rm = TRUE))
daily_calories_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(avg_calories = mean(Calories, na.rm = TRUE))
mean(daily_activity$TotalSteps, na.rm = TRUE)
mean(daily_activity$Calories, na.rm = TRUE)
min(daily_activity$TotalSteps, na.rm = TRUE)
max(daily_activity$TotalSteps, na.rm = TRUE)
sd(daily_activity$TotalSteps, na.rm = TRUE)
sd(daily_activity$Calories, na.rm = TRUE)

# Visualization

p1 <- ggplot(daily_steps_by_day,
             aes(x = day_of_week, y = avg_steps)) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_cartesian(ylim = c(6500, 8500)) +
  labs(title = "Average Steps by Day of Week",
       x = "Day of Week",
       y = "Average Steps") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

p2 <- ggplot(daily_calories_by_day,
             aes(x = day_of_week, y = avg_calories)) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_cartesian(ylim = c(2000,2500)) +
  labs(title = "Average Calories by Day of Week",
       x = "Day of Week",
       y = "Average Calories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
grid.arrange(p1, p2, ncol = 2)