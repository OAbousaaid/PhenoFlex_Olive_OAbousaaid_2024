# Three sub figures of figure S1
# Install and load required packages
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(ggsn)

# Loading map data
# The world
world <- ne_countries(scale = "medium", returnclass = "sf")
# Africa
africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")
# Morocco
morocco <- ne_countries(country = "Morocco", scale = "medium", returnclass = "sf")
# disputed territory
disputed_territory <- ne_countries(country = "Western Sahara", scale = "medium", returnclass = "sf")
# All morocco
morocco <- st_union(morocco, disputed_territory)

# Create the world map
world_map <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "black") +
  labs(title = "", caption = "") +
  theme_classic()

print(world_map)

ggsave("figures/Figure_S1_a.png", width = 6, height = 4, units = "cm", dpi = 600)

# Create the Africa map####
africa_map <- ggplot() +
  geom_sf(data = africa, fill = "lightgrey", color = "black") +
  geom_sf(data = morocco, fill = "mistyrose4", color = "mistyrose4") +
  labs(title = "", caption = "") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.ontop = FALSE,
    panel.grid = element_blank()
  )

print(africa_map)

ggsave("figures/Figure_S1_b.png", width = 16, height = 14, units = "cm", dpi = 600)

# Morocco
# Add the study site ####
location <- data.frame(
  Longitude = -7.432778,
  Latitude =  31.819444, 
  label = "WOGBM")

morocco_map <- ggplot() +
  geom_sf(data = morocco, fill = "mistyrose4", color = "mistyrose4") +
  geom_point(data = location, aes(x = Longitude, y = Latitude), color = "red4", shape = "+", size = 4) +
  geom_text(
    data = location,
    aes(x = Longitude, y = Latitude, label = label),
    color = "white",
    size = 2,
    nudge_y = 0.01,
    vjust = -1,
    fontface = "bold" ) +
  labs(title = "Morocco", caption = "Data source: Natural Earth") +
  theme_bw()+
  annotation_scale(
    location = "br",
    plot_unit = NULL,
    bar_cols = c("black", "white"),
    line_width = 0.5,
    height = unit(0.1, "cm"),
    pad_x = unit(0.25, "cm"),
    pad_y = unit(0.25, "cm"),
    text_pad = unit(0.15, "cm"),
    text_cex = 0.7,
    text_face = NULL,
    text_family = "",
    tick_height = 0.2
  )

print(morocco_map)

ggsave("figures/Figure_S1_c.png", width = 16, height = 14, units = "cm", dpi = 600)
 
# Figure S2 ####
# monthly average temperature and rainfall during the period between 1985-2022
# Import data

temp_rain_data <- read_excel("Mean_temp_rainfall_1985_2022.xlsx")
temp_rain_data <- as.data.frame(temp_rain_data)
temp_rain_data$Month <- as.factor(temp_rain_data$Month)

# Plot the data
ggplot(temp_rain_data, aes(x = Month, group = 1)) +
  geom_bar(aes(y = Precipitation / 1.2, fill = "Precipitation"), stat = "identity") +
  geom_errorbar(aes(ymin = (Precipitation - sd_rf) / 1.2, ymax = (Precipitation + sd_rf) / 1.2, 
                    width = 0.2),color = "skyblue2") +
  geom_point(aes(y = Temperature, color = "Temperature")) +
  geom_line(aes(y = Temperature, color = "Temperature")) +
  geom_errorbar(aes(ymin = Temperature - sd_temp, ymax = Temperature + sd_temp, color = "Temperature"), 
                width = 0.2) +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(
    expression("Average Monthly Temperature (" ~ degree*C ~ ")"),
    sec.axis = sec_axis(~ .*1.2, name = "Monthly Precipitation (mm)")
  ) +
  scale_colour_manual("", values = c(Temperature = "red4")) +
  scale_fill_manual("", values = c(Precipitation = "skyblue2")) +
  theme_bw()+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top")

# Save the final plot to folder
ggsave("figures/Figure_S2.png", width = 14, height = 12, units = "cm", dpi = 600)
