library(dplyr)

incarceration_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

# Summary Information Calculations
current_year <- max(incarceration_data$year)

# Calculate average values for recent year data
recent_year_data <- incarceration_data %>% filter(year == current_year)

avg_total_jail_pop_rate <- mean(recent_year_data$total_jail_pop_rate, na.rm = TRUE)
avg_female_jail_pop_rate <- mean(recent_year_data$female_jail_pop_rate, na.rm = TRUE)
avg_male_jail_pop_rate <- mean(recent_year_data$male_jail_pop_rate, na.rm = TRUE)

# Calculate changes over time
jail_pop_rate_1970 <- incarceration_data %>%
  filter(year == 1970) %>%
  pull(total_jail_pop_rate) %>%
  mean(na.rm = TRUE)

jail_pop_rate_2018 <- incarceration_data %>%
  filter(year == 2018) %>%
  pull(total_jail_pop_rate) %>%
  mean(na.rm = TRUE)

jail_pop_rate_change <- jail_pop_rate_2018 - jail_pop_rate_1970

latest_data <- incarceration_data %>% filter(year == current_year)
avg_black_jail_pop_rate <- mean(latest_data$black_jail_pop_rate, na.rm = TRUE)
avg_white_jail_pop_rate <- mean(latest_data$white_jail_pop_rate, na.rm = TRUE)
avg_latinx_jail_pop_rate <- mean(latest_data$latinx_jail_pop_rate, na.rm = TRUE)

# Identify highest and lowest incarceration rates
highest_total_jail_county <- latest_data %>%
  filter(!is.na(total_jail_pop_rate)) %>%
  arrange(desc(total_jail_pop_rate)) %>%
  slice(1) %>%
  select(state, county_name, total_jail_pop_rate, urbanicity)

