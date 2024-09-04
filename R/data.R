
library(tidyverse)
library(ipumsr)
library(labelled)
library(geos)
library(data.table)
library(sf)

#---*from clean-data.R*---#

#----MIGRATIONS----

# Read from downloaded file
ipumsmicro_df <- read_ipums_micro("data/census/usa_00079.xml") %>% 
  filter(COUNTYICP != 0) %>% # subset with location identified
  mutate( IsMigrant = ((MIGRATE5 %in% c(2,3,4)) | (MIGRATE1 %in% c(2,3,4))) ) 

# shorten race category names for column names
val_labels(ipumsmicro_df$RACE) <- c( white = 1, black = 2, aian  = 3,  chin  = 4,
                                     japn  = 5, oapi  = 6, other = 7,  multi = c(8,9) )

# create county-year-race summary statistics
migrations_race <- ipumsmicro_df %>% 
  group_by(YEAR, STATEFIP, COUNTYICP, RACE) %>% 
  summarise(
    pop      = sum(PERWT), 
    mig      = sum(IsMigrant * PERWT),
    migratio = mig/pop,
    ) %>% 
  ungroup() %>% 
  mutate(RACE = to_factor(RACE)) %>% 
  pivot_wider(names_from = RACE, values_from = c( pop, mig, migratio ) ) %>% 
  # assuming that if there is no-one in sub-sample, pop is zero 
  mutate_all( ~replace(., is.na(.), 0))
  
# create county-year summary statistics
migrations_county <- ipumsmicro_df %>% 
  select(-RACE) %>% 
  group_by(YEAR, STATEFIP, COUNTYICP) %>% 
  summarise(
    pop_total      = sum(PERWT), 
    mig_total      = sum(IsMigrant * PERWT),
    migratio_total = pop_total / mig_total
    ) %>% 
  ungroup() 

migrations_df <-
  left_join(
    migrations_county, migrations_race,
    by = c("YEAR", "STATEFIP", "COUNTYICP")
    ) 

write_csv(migrations_df, file = "./data/migrations.csv")

#----DISTANCES----

library(sf)

# read in county-camp distances from QGIS
county_distances <- st_read("./data/maps/camp-distances.gpkg")%>% 
  filter(!STATENAM %in% c("Hawaii Territory", "Alaska Territory")) %>% 
  rename(
    dist_GilaRiver = `Gila.River.Relocation.Center`,
    dist_Poston    = `Poston.Relocation.Center`,
    dist_Jerome    = `Jerome.Relocation.Center`,
    dist_Rohwer    = `Rohwer.Relocation.Center`,
    dist_Manzanar  = `Manzanar.Relocation.Center`,
    dist_Tule      = `Tule.Lake.Relocation.Center`,
    dist_Granada   = `Granada.Relocation.Center`,
    dist_Minidoka  = `Minidoka.Relocation.Center`,
    dist_HeartMt   = `Heart.Mountain.Relocation.Center`
  ) %>% 
  select(
    NHGISNAM,
    STATENAM,
    NHGISST,
    NHGISCTY,
    starts_with("dist_")
  )

write_csv(county_distances, "./data/distances.csv")

#----CROSSWALKS----

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

migrations_df <- read_csv("./data/migrations.csv")
migrations_df <- migrations_df %>% 
  # need an extra zero to go from STATEFIPS to NHGISST
  mutate(id = STATEFIP * 100000 + COUNTYICP) 

migrations_cleaned_df <-
  right_join(crosswalk, migrations_df, 
            by = c("id", "Year"="YEAR"), 
            relationship = "many-to-many") %>% 
  group_by(Year, id1990, STATENAM_1990, NHGISNAM_1990) %>% 
  summarise(across(starts_with(c("mig","pop")), ~ sum(.x * weight))) 

write_csv(migrations_cleaned_df, file = "./data/migrations_crosswalked.csv")

# migcountypanel <- migrations_cleaned_df %>% 
#   pivot_wider(names_from = Year, 
#               values_from = c(mig_total, mig_white, mig_black, mig_aian, mig_oapi, mig_chin, mig_japn, mig_other,
#                               pop_total, pop_white, pop_black, pop_aian, pop_oapi, pop_chin, pop_japn, pop_other)
#               ) %>% 
#   filter(!is.na(pop_total_1940), !is.na(pop_total_1950), !is.na(pop_total_1960), !is.na(pop_total_1970), !is.na(pop_total_1980))
census_df <- read_csv(here::here("./data/migrations_crosswalked.csv"), show_col_types = f)

countyyr_df <- 
 right_join(crosswalk, census_df, 
            by = c("id1990", "Year"), 
            relationship = "many-to-many") %>% 
  group_by(Year, id1990, STATENAM, NHGISNAM) %>% 
  summarise(across(starts_with(c("mig","pop")), ~ sum(.x * weight))) 

countyyr_df <- left_join(countyyr_df, county1990_shp, by = "id1990")
save(countyyr_df, file = "data/countyyr_df.rda")

#---*from Data.qmd*---#
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
