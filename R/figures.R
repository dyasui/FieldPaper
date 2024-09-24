library(tidyverse)
library(ipumsr)
library(labelled)
library(sf)

# read data to plot
data <- read_csv("data/data.csv")
county_1990_shp <- read_ipums_sf("data/maps/*_shape.zip")

# read shapefiles and append distances
county_1990_shp <- read_ipums_sf("data/maps/nhgis0025_shape.zip") %>%
  filter( ! STATENAM %in% c("Alaska", "Hawaii") ) %>%
  mutate(id1990 = as.numeric(NHGISST) * 10000 + as.numeric(NHGISCTY)) %>%
  left_join(read_csv("data/distances.csv"), by = c("id1990"))

camplocations_df <- 
  read_csv("data/BehindBarbedWire_StoryMap/BehindBarbedWire_StoryMap_InternmentCampLocationsMap_Data.csv") %>% 
  st_as_sf(
    .,
    coords = c("Longitude", "Latitude"), 
    crs = 4326
    ) %>% 
  mutate(
    geometry = st_transform(geometry, crs = st_crs(county_1990_shp$geometry))
    )

countymap <- ggplot( data = county_1990_shp) +
  geom_sf( aes(geometry = geometry) ) +
  theme_void()

unlink("figures/countymap.png")
distancesmap <- countymap + 
  geom_sf( aes(fill = campclosest_dist) ) +
  geom_sf( data = camplocations_df, aes(color = "red"), show.legend = FALSE) +
  labs(
    title = "Internment Camp Locations and Distances to Counties",
    fill = "Distance from county to closest camp (m)",
  ) + 
  viridis::scale_fill_viridis()
ggsave("figures/countymap.png", plot = distancesmap, dpi = 350)

migrationsmap <- data %>% 
  left_join(county_1990_shp, by = "id1990") %>% 
  ggplot(data = .) +
  geom_sf(aes(geometry = geometry, fill = mig_japn)) + 
  labs(
    title = "Japanese Migration Patterns over Time",
    fill = "Number of new Japanese-American migrants"
  ) +
  facet_wrap(vars(Year)) +
  viridis::scale_fill_viridis() +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title.position = "top"
  )
ggsave("figures/migrationmap.png", plot = migrationsmap, dpi = 350)

# Relationship of dist, migratio, and ez
data %>% 
  ggplot(data = ., 
    aes(x = log(campclosest_dist), y = y, group = as.factor(ez))
  ) +
  geom_point(aes(color = as.factor(ez))) +
  geom_smooth(method = "lm", aes(color = as.factor(ez))) +
  facet_wrap(vars(Year)) +
  ylim(0.0,0.2)
