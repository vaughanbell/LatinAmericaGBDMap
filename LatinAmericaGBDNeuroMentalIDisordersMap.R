library(dplyr)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(scales)

setwd("/home/main/data/")

# Set up map countries
countries <- c("Brazil", "Argentina", "Chile", "Colombia", "Peru", "Venezuela", "Ecuador", "Bolivia", "Paraguay", "Uruguay", "Guyana", "Suriname", "French Guiana", "Guyane", "Trinidad and Tobago", "Barbados", "Grenada", "St. Kitts and Nevis", "Antigua and Barbuda", "St. Vincent and the Grenadines", "St. Lucia", "Dominica", "Martinique", "Guadeloupe", "Puerto Rico", "Honduras", "Nicaragua", "El Salvador", "Costa Rica", "Panama", "Belize", "Guatemala", "Mexico")

# Get the world boundaries dataset from rnaturalearth
world <- ne_countries(scale = "medium", country = countries)

# Load GBD data extracted from Global Burden of Disease Study 2019 https://vizhub.healthdata.org/gbd-results/
gbd_df <- read.csv("IHME-GBD_2019_DATA-7c9bc585-1.csv")

# Concise country names
gbd_df <- gbd_df %>%
  mutate(location_name = ifelse(location_name == "Bolivia (Plurinational State of)", "Bolivia", location_name)) %>%
  mutate(location_name = ifelse(location_name == "Venezuela (Bolivarian Republic of)", "Venezuela", location_name))

# Convert to readable percentages
gbd_df <- gbd_df %>%
  mutate(val = val * 100)

# Extract neurological disorders data into separate dataframe
neuro_df <- gbd_df %>%
  filter(cause_name == "Neurological disorders") %>%
  filter(measure_name == "Prevalence") %>%
  filter(metric_name == "Percent")

world_neuro <- merge(world, neuro_df, by.x = "sovereignt", by.y = "location_name", all.x = TRUE)

# Extract mental disorders data into separate dataframe
mental_df <- gbd_df %>%
  filter(cause_name == "Mental disorders") %>%
  filter(measure_name == "Prevalence") %>%
  filter(metric_name == "Percent")

world_mental <- merge(world, mental_df, by.x = "sovereignt", by.y = "location_name", all.x = TRUE)

#
# Mapping
#

legend_size = 18

# Custom formatting function to add a '%' sign after each label
percent_format <- function(x) {
  paste0(x, "%")
}

# Plot neuro disorders prevalence map
neuro_map <- ggplot(data = world_neuro) +
  geom_sf(aes(fill = val)) +
  scale_fill_gradient(
    low = "#8ab8f6",
    high = "#fac4d4",
    breaks = pretty(range(world_neuro$val, na.rm = TRUE), n = 5),
    labels = percent_format(pretty(range(world_neuro$val, na.rm = TRUE), n = 5))
  ) +
  guides(fill = guide_colorbar(
    barwidth = 24,  # Adjust the width of color blocks
    barheight = 1.4,  # Adjust the height of color blocks
    title.position = "top",
    title.hjust = 0.5,
    title.vjust = 1.1
  )) +
  theme_void() +
  theme(legend.position = "top") +
  theme(legend.text = element_text(size = legend_size)) +
  theme(legend.title = element_blank())

# Plot mental disorders prevalence map
mental_map <- ggplot(data = world_mental) +
  geom_sf(aes(fill = val)) +
  scale_fill_gradient(
    low = "#a1c99b",
    high = "#fee08b",
    breaks = pretty(range(world_mental$val, na.rm = TRUE), n = 5),
    labels = percent_format(pretty(range(world_mental$val, na.rm = TRUE), n = 5))
  ) +
  guides(fill = guide_colorbar(
    barwidth = 18,  # Adjust the width of color blocks
    barheight = 1.4,  # Adjust the height of color blocks
    title.position = "top",
    title.hjust = 0.5,
    title.vjust = 1.1
  )) +
  theme_void() +
  theme(legend.position = "top") +
  theme(legend.text = element_text(size = legend_size)) +
  theme(legend.title = element_blank())

# Combine the plots into a single plot with a common legend
plot_grid(neuro_map, mental_map)
