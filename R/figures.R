library(tidyverse)
library(ipumsr)
library(labelled)
library(sf)

county_1990_shp <- read_ipums_sf("data/maps/*_shape.zip")

#---- Distances Map
load("data/ctycmpdist.RData")
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

ggplot() + 
  geom_sf( data = ctycmpdist_shp, aes(fill = campclosest_dist) ) +
  geom_sf( data = camplocations_df, aes(color = "red"))
  scale_fill_continuous() +
  theme_minimal()

#---*from Data.qmd*---#

#---- read cleaned census dataset ----#

county1940_df <- countyyr_df %>% filter(Year==1940)

# plot(county1940_df$geo, col = pop_total)

county_map <- ggplot(data = county1940_df) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()

county_map + 
  geom_sf(aes(geometry = geometry, fill = migratio_total), size = 0.1) +
  scale_fill_viridis_c() 

#---- calculate distances to each camp ----#

#---- summarise crosswalked counties by pop ----#
county_sample <- census_df %>% 
  select(Year, id1990) %>%
  unite(ctyr_id, c("id1990","Year"), remove = TRUE) %>% 
  pull()

# crosswalk historical county population tables from nhgis to 1990 counties
countypop_df <- read_csv(here::here("data/census/nhgis0019_csv/nhgis0019_ts_nominal_county.csv")) %>%
  mutate(id = as.numeric(STATENH)*10000 + as.numeric(COUNTYNH)) %>% 
  right_join(crosswalk, .,
           by = c("id", "Year"="YEAR"),
           relationship = "many-to-many") %>%
  group_by(Year, id1990, STATENAM_1990, NHGISNAM_1990) %>%
  summarise(pop = sum(A00AA * weight)) %>% 
  ungroup() %>% 
  unite(ctyr_id, c("id1990","Year"), remove = FALSE)

countypop_df <- countypop_df %>% 
  mutate(
    insample = (ctyr_id %in% county_sample)
  )
