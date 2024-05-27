library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
incarceration_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

urbanicity_mapping <- c("rural" = 0, "small/mid" = 1, "suburban" = 2, "urban" = 3)
incarceration_data <- incarceration_data %>%
  mutate(urbanicity_numeric = urbanicity_mapping[urbanicity])

head(urbanicity_mapping)

ethnic_groups <- c("aapi_jail_pop_rate", "black_jail_pop_rate", "latinx_jail_pop_rate", "native_jail_pop_rate", "white_jail_pop_rate")
plot_data <- incarceration_data %>%
  select(year, state, county_name, urbanicity_numeric, all_of(ethnic_groups)) %>%
  pivot_longer(cols = all_of(ethnic_groups), names_to = "ethnicity", values_to = "rate") %>%
  filter(!is.na(rate)) %>%
  mutate(
    ethnicity = str_replace(ethnicity, "_jail_pop_rate", ""),
    ethnicity = str_to_title(ethnicity)
  )

# Create the comparison plot
ggplot(plot_data, aes(x = urbanicity_numeric, y = rate, color = ethnicity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_continuous(breaks = 0:3, labels = names(urbanicity_mapping)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Incarceration Rates by Ethnic Group Across Different Urbanization Levels",
    x = "Urbanicity",
    y = "Incarceration Rate per 100,000 People",
    color = "Ethnic Group"
  ) +
  theme_minimal()

