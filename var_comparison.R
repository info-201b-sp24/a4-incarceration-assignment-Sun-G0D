library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
incarceration_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

incarceration_data$urbanicity_cont <- as.numeric(factor(incarceration_data$urbanicity, 
                                                        levels = c("rural", "small/mid", "suburban", "urban"), 
                                                        labels = c(0, 1, 2, 3)))

avg_incarceration_rates <- incarceration_data %>%
  group_by(urbanicity_cont) %>%
  summarise(avg_aapi = mean(aapi_jail_pop_rate, na.rm = TRUE),
            avg_black = mean(black_jail_pop_rate, na.rm = TRUE),
            avg_latinx = mean(latinx_jail_pop_rate, na.rm = TRUE),
            avg_native = mean(native_jail_pop_rate, na.rm = TRUE),
            avg_white = mean(white_jail_pop_rate, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "ethnicity", values_to = "avg_rate") %>%
  mutate(ethnicity = str_replace(ethnicity, "avg_", ""))

# Plotting the chart
ggplot(avg_incarceration_rates, aes(x = urbanicity_cont, y = avg_rate, color = ethnicity)) +
  geom_line(size = 1) +
  labs(title = "Average Incarceration Rates by Urbanicity and Ethnicity",
       x = "Urbanicity (Continuous Scale)",
       y = "Average Incarceration Rate",
       color = "Ethnic Group") + # Provide a clearer legend title
  theme_minimal() +
  scale_x_continuous(breaks = 0:4, labels = c("","Rural", "Small/Mid", "Suburban", "Urban")) +
  scale_color_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))
