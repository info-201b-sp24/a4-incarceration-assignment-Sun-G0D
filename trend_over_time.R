library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

data <- data %>%
  select(year, state, county_name, aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate, white_jail_pop_rate) %>%
  filter(!is.na(year) & year >= 1990) %>%
  mutate(across(starts_with("aapi_jail_pop_rate"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("black_jail_pop_rate"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("latinx_jail_pop_rate"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("native_jail_pop_rate"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("white_jail_pop_rate"), ~ replace_na(., 0)))

agg_data <- data %>%
  group_by(year) %>%
  summarise(
    aapi_jail_pop_rate = mean(aapi_jail_pop_rate, na.rm = TRUE),
    black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE),
    latinx_jail_pop_rate = mean(latinx_jail_pop_rate, na.rm = TRUE),
    native_jail_pop_rate = mean(native_jail_pop_rate, na.rm = TRUE),
    white_jail_pop_rate = mean(white_jail_pop_rate, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "racial_group",
    values_to = "incarceration_rate"
  )

agg_data$racial_group <- str_replace_all(agg_data$racial_group, "_jail_pop_rate", "")
agg_data$racial_group <- str_replace_all(agg_data$racial_group, "_", " ")
agg_data$racial_group <- str_to_title(agg_data$racial_group)

ggplot(agg_data, aes(x = year, y = incarceration_rate, color = racial_group)) +
  geom_line(size = 1) +
  labs(
    title = "Incarceration Trends Over Time by Racial Group",
    x = "Year",
    y = "Incarceration Rate per 100,000 People",
    color = "Racial Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )
