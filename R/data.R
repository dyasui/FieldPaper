
library(tidyverse)
library(ipumsr)
library(labelled)
library(geos)
library(data.table)
library(sf)

#---- download shapefiles from NHGIS ----#
county1990_shp_name <- get_metadata_nhgis("shapefiles") %>%
  filter(year == 1990,
         geographic_level == 'County',
         str_detect( basis, '^2008' )) %>% # 2008 TIGER/Lines basis
  select(name) %>% pull()

define_extract_nhgis(
    description = "NHGIS shapefile for 1990 county boundaries",
    shapefiles = "us_county_1990_tl2008" ) %>%
    submit_extract() %>%
    wait_for_extract() %>%
    download_extract(download_dir = "./data/maps/")

# read nhgis shapefile, only cont. us states
county1990_shp <- read_ipums_sf("data/maps/nhgis0018_shape/nhgis0018_shapefile_tl2008_us_county_1990.zip") %>% 
  as.data.table() %>% 
  filter(STATENAM %notin% c('Alaska', 'Hawaii')) %>% 
  # need an extra zero to go from STATEFIPS to NHGISST
  mutate(
    id1990 = as.numeric(NHGISST) * 10000 + as.numeric(NHGISCTY) ,
    geo = as_geos_geometry(geometry, crs=102003)
    ) 

#---- read in historical county crosswalks ----#
# Crosswalk file from Eckert, Gvirtz, Liang, and Peters (2020)
# github repo link for replication data:
url <- "https://raw.githubusercontent.com/liang-jack-a/EGLP_Crosswalk/master/Crosswalks/county_crosswalk_endyr_1990.csv"
crosswalk <- read_csv(url) %>%
  filter(Year %in% 1940:1980) %>% # 1990 counties don't appear as their own rows
  mutate( # combine state and county codes into single id variable
    id     = as.numeric(NHGISST) * 10000 + as.numeric(NHGISCTY),
    id1990 = as.numeric(NHGISST_1990) * 10000 + as.numeric(NHGISCTY_1990)) 
crosswalk <- crosswalk %>% 
  bind_rows( # fill in rows for 1990 counties
    crosswalk %>% 
      distinct(id1990, .keep_all = TRUE) %>% 
      mutate(
        Year     = 1990,
        NHGISST  = NHGISST_1990,
        NHGISCTY = NHGISCTY_1990,
        STATENAM = STATENAM_1990,
        NHGISNAM = NHGISNAM_1990,
        ICPSRST  = ICPSRST_1990,
        ICPSRCTY = ICPSRCTY_1990,
        area_base= NA,
        area     = NA,
        weight   = 1,
      )
  ) 

#---- read cleaned census dataset ----#
census_df <- read_csv(here::here("./data/migrations_crosswalked.csv"), show_col_types = F)
obsbyyear_tbl <- census_df %>% count(Year)

countyyr_df <- 
 right_join(crosswalk, census_df, 
            by = c("id1990", "Year"), 
            relationship = "many-to-many") %>% 
  group_by(Year, id1990, STATENAM, NHGISNAM) %>% 
  summarise(across(starts_with(c("mig","pop")), ~ sum(.x * weight))) 

countyyr_df <- left_join(countyyr_df, county1990_shp, by = "id1990")


county1940_df <- countyyr_df %>% filter(Year==1940)

# plot(county1940_df$geo, col = pop_total)

county_map <- ggplot(data = county1940_df) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()

county_map + 
  geom_sf(aes(geometry = geometry, fill = migratio_total), size = 0.1) +
  scale_fill_viridis_c() 

#---- calculate distances to each camp ----#
camplocations_df <- 
  read_csv("data/BehindBarbedWire_StoryMap/BehindBarbedWire_StoryMap_InternmentCampLocationsMap_Data.csv") %>% 
  st_as_sf(
    .,
    coords = c("Longitude", "Latitude"), 
    crs = 4326
    ) %>% 
  mutate(
    geometry = st_transform(geometry, crs = st_crs(county1990_shp$geometry))
    )

distance_matrix <- st_distance(
  county1990_shp$geometry, 
  camplocations_df$geometry, 
  ) %>% 
  as_tibble()

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
