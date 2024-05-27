library(maps)
library(dplyr)
library(ggplot2)
library(reshape2)
library(mapproj)

# Load your data (assuming it's in a dataframe called 'incarceration_data')
incarceration_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true", stringsAsFactors = FALSE)

# Convert urbanicity to a numerical scale
incarceration_data$urbanicity_numeric <- factor(incarceration_data$urbanicity, 
                                                levels = c("rural", "small/mid", "suburban", "urban"),
                                                labels = c(0, 1, 2, 3))


# Filter out rows where total_jail_pop_rate or urbanicity is NA
incarceration_data <- incarceration_data %>% 
  filter(!is.na(total_jail_pop_rate) & !is.na(urbanicity))



# Group by state and urbanicity, then calculate the mean total_jail_pop_rate
average_rates <- incarceration_data %>%
  group_by(state, urbanicity) %>%
  summarize(average_rate = mean(total_jail_pop_rate, na.rm = TRUE), .groups = 'drop')

average_rates <- average_rates %>%
  arrange(state, urbanicity)

# Calculate the change in incarceration rates between subsequent urbanicity levels per state
average_rates <- average_rates %>%
  group_by(state) %>%
  mutate(change_in_rate = c(NA, diff(average_rate)))

average_rates <- average_rates %>%
  filter(!is.na(change_in_rate))

state_changes <- average_rates %>%
  group_by(state) %>%
  summarize(avg_change = mean(change_in_rate, na.rm = TRUE), .groups = 'drop')

state.name.map <- data.frame(
  state = state.abb,
  full_name = tolower(state.name)  
)

state_changes <- full_join(state_changes, state.name.map, by = "state")


# Merge the map data with our state changes using full state names
states_map <- map_data("state")
plot_data <- merge(states_map, state_changes, by.x = "region", by.y = "full_name")
plot_data <- plot_data[order(plot_data$order), ]

plot_data$missing_data <- FALSE
plot_data$missing_data[is.na(plot_data$avg_change)] <- TRUE

# Plot
ggplot() +
  geom_polygon(data = plot_data, aes(x = long, y = lat, group = group, fill = avg_change),
               color = "black") +
  geom_point(data = data.frame(missing_data = TRUE), aes(y = 39.84922, x = -75.52730, color = missing_data), size = 0) +
  coord_fixed(1.3) +
  scale_fill_gradient2(low = "red", mid = "gray92", high = "blue", midpoint = 0,
                       name = "Avg Change in\nIncarceration Rate",
                       na.value = "yellow",
                       limits = c(min(plot_data$avg_change, na.rm = TRUE), 
                                  max(plot_data$avg_change, na.rm = TRUE))) +
  scale_color_manual(name = "",
                     values = c("TRUE" = "yellow"),
                     labels = c("Not Enough Data"),
                     guide = guide_legend(override.aes = list(size = 5))) +
  labs(title = "Incarceration Rate Change Across Urbanicity Levels by State") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.box.just = "right",
        plot.title = element_text(hjust = 0.5, size = 14)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5),
         color = guide_legend(order = 2, title = NULL, override.aes = list(shape = 15, size = 5)))
