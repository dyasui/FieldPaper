library(tidyverse)
library(ipumsr)
library(labelled)
library(sf)

# Relocation Destinations Map?
RelocationDestinations_Cities <-
  read_csv("./data/WRA-infinitecoop/RelocationDestinations_Cities.csv") %>% 
  mutate(City = str_replace(City, "Berkely", "Berkeley")) %>% 
  mutate(State_name = usdata::abbr2state(State))

places_1950 <- read_ipums_sf("data/maps/placepoints-1950_shape.zip") 

RelocationDestinations_Cities <- 
  left_join(RelocationDestinations_Cities, places_1950, 
            by = c('City'='NAME', 'State_name'='STATE')) 

county_1990_shp <- read_ipums_sf("data/maps/counties-1990_shape.zip")

counties_maps <- 
  left_join(county_1990_shp, county_shapes, 
              by = c("State", "County")) %>% 
  rename(Group = group.x, group = group.y)

ggplot() + 
  geom_polygon(data=map_data("state"), aes(x=long, y=lat, group=group),
               color = "black") + 
  geom_polygon(data = map_data("county"), aes(x=long, y=lat, group=group), 
               color = "grey", size = .05) +
  geom_point(data = RelocationDestinations_Cities,
             aes(x=lng, y=lat, size = People), color = "red") +
  theme_void() + 
  ggtitle('Relocated People by City') 

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
