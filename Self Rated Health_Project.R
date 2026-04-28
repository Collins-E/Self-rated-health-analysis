# Big Data Final Project

rm(list = ls())
shell("cls")

# Load data into R
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

health <- read_csv("Self Rated Health Clean.csv")

# Convert to long format
health_long <- health %>%
  pivot_longer(
    cols = `2015`:`2024`,
    names_to = "Year",
    values_to = "Value"
  )
#Clean NA
health_long <- health_long %>%
  filter(!is.na(Value))
sum(is.na(health_long$Value))

health_long$Year <- as.numeric(health_long$Year)

# Create summary statistics
summary_stats <- health_long %>%
  summarise(
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE),
    mean = mean(Value, na.rm = TRUE),
    median = median(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE)
  )

summary_stats

# Create charts

# Canada Trend Over Time
canada <- health_long %>%
  filter(Geography == "Canada")

ggplot(canada, aes(x = Year, y = Value)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Self-Rated Health in Canada Over Time",
    y = "Percent",
    x = "Year"
  ) +
  theme_minimal()

# Overall trend decline after 2020
# Highest year was 2020 vs lowest year 2024
# Possible reason is COVID

# Province Comparison (Latest Year)
latest <- health_long %>%
  filter(Year == max(Year))

ggplot(latest, aes(x = reorder(Geography, Value), y = Value, fill = Geography)) +
  geom_col() +
  geom_text(aes(label = round(Value, 1)), hjust = -0.1, size = 3) +
  coord_flip() +
  expand_limits(y = max(latest$Value) + 5) +
  labs(
    title = "Self-Rated Health by Province (Latest Year)",
    x = "Province",
    y = "Percent"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Highest province is Quebec
# Lowest province is New Brunswick
# Ontario lower than Canada

# Top vs Bottom Provinces Over Time
top_bottom <- health_long %>%
  filter(Geography %in% c("Canada", "Ontario", "Quebec", "Alberta"))

ggplot(top_bottom, aes(x = Year, y = Value, color = Geography)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Health Trends: Selected Provinces",
    y = "Percent",
    x = "Year"
  ) +
  theme_minimal()

# Quebec stays the highest
# Alberta declines the most

# Change Over Time (2015-2024)
change <- health_long %>%
  group_by(Geography) %>%
  summarise(
    change = Value[Year == 2024] - Value[Year == 2015]
  )

ggplot(change, aes(x = reorder(Geography, change), y = change, fill = change > 0)) +
  geom_col() +
  geom_text(
    aes(label = round(change, 1)),
    hjust = ifelse(change$change > 0, -0.1, 1.1),
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  expand_limits(y = max(change$change) + 2) +
  labs(
    title = "Change in Health (2015–2024)",
    y = "Change (%)",
    x = "Province"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Alberta declined the most
# Overall rating change for Canada was -9%