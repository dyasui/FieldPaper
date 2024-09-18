
library(tidyverse)
library(labelled)
library(ipumsr)
library(sf)

#----MIGRATIONS----

# Read from downloaded ddi file after running download-data.R
ipums_ddi <- list.files(path = "data/census", pattern = "*.xml") %>%
  # in case of multiple files, choose most recent
  sort(decreasing = TRUE) %>%
  head(1)

# read ipums microdata and identify inter-county migrants
ipumsmicro_df <- read_ipums_micro(paste("data/census/", ipums_ddi, sep = "")) %>% 
  mutate(
    i_countyid = ifelse(COUNTYICP != 0, 1, 0), # is county identified?
    i_migrant = ((MIGRATE5 %in% c(2,3,4)) | (MIGRATE1 %in% c(2,3,4))) ,
    # Recode ipums variables
    INCWAGE = ifelse(INCWAGE == 999999, NA, INCWAGE), 
    INCTOT = ifelse(INCTOT == 9999999, NA, INCTOT), 
    i_female = case_when(SEX == 2 ~ 1, SEX == 1 ~ 0),
    i_unemployed = case_when(EMPSTAT == 2 ~ 1, EMPSTAT == 0 ~ NA, .default = 0),
    i_labforce = case_when(LABFORCE == 2 ~ 1, LABFORCE == 1 ~ 0, LABFORCE == 0 ~ NA), 
    i_issei = ifelse(BPL == 501, 1, 0)
  )

# shorten race category names for column names
val_labels(ipumsmicro_df$RACE) <- 
  c( white = 1, black = 2, aian  = 3,  chin  = 4, 
     japn  = 5, oapi  = 6, other = 7,  multi = c(8,9) )

# create county-year-race summary statistics
demographics_race <- ipumsmicro_df %>% 
  group_by(YEAR, STATEFIP, COUNTYICP, RACE) %>% 
  summarise(
    pop      = sum(PERWT), 
    mig      = sum(i_migrant * PERWT),
    migratio = mig/pop,
    across( # weight demographics by census person weight
      c(AGE, INCWAGE, INCTOT, i_female, i_unemployed, i_labforce, i_issei),
      ~ weighted.mean(., PERWT, na.rm = TRUE) 
    ), 
    # calculate unemployment rate
    unmp_rate = sum(i_unemployed) / sum(i_labforce)
    ) %>% 
  ungroup() %>% 
  # assuming that if there is no-one in sub-sample, pop is zero 
  mutate(across(c(pop, mig, migratio), ~replace(., is.na(.), 0))) %>% 
  # pivot race group averages to columns
  mutate(RACE = to_factor(RACE)) %>% 
  pivot_wider(names_from = RACE, values_from = pop:unmp_rate ) 
  
# create county-year summary statistics
demographics_county <- ipumsmicro_df %>% 
  select(-RACE) %>% 
  group_by(YEAR, STATEFIP, COUNTYICP) %>% 
  summarise(
    pop      = sum(PERWT), 
    mig      = sum(i_migrant * PERWT),
    migratio = mig/pop,
    across( # weight demographics by census person weight
      c(AGE, INCWAGE, INCTOT, i_female, i_unemployed, i_labforce, i_issei),
      ~ weighted.mean(., PERWT, na.rm = TRUE) 
    ), 
    # calculate unemployment rate
    unmp_rate = sum(i_unemployed) / sum(i_labforce)
    ) %>% 
  ungroup() 

demographics_df <-
  left_join(
    demographics_county, demographics_race,
    by = c("YEAR", "STATEFIP", "COUNTYICP")
    ) %>%
  # need an extra zero to go from STATEFIPS to NHGISST
  mutate(id = STATEFIP * 100000 + COUNTYICP) 

write_csv(demographics_df, file = "./data/demographics.csv")

#----DISTANCES----

nhgis_shape <- list.files(path = "data/maps", pattern = "*_shape.zip") %>%
  # in case of multiple files, choose most recent
  sort(decreasing = TRUE) %>%
  head(1)

county_1990_shp <- read_ipums_sf(paste("data/maps/", nhgis_shape, sep = "")) %>%
  filter( ! STATENAM %in% c("Alaska", "Hawaii") )

camplocations_df <- 
  read_csv("data/BehindBarbedWire_StoryMap/BehindBarbedWire_StoryMap_InternmentCampLocationsMap_Data.csv") %>% 
  st_as_sf(.,
    coords = c("Longitude", "Latitude"), 
    crs = 4326
    ) %>% 
  mutate(
    geometry = st_transform(geometry, crs = st_crs(county_1990_shp$geometry))
    )

# shorter camp names
campnames_lookup <- c(GilaRiver = "V1", Poston = "V2", Jerome = "V3", Rohwer = "V4", Manzanar = "V5",
  Tule = "V6", Granada = "V7", Minidoka = "V8", Topaz = "V9", HeartMt = "V10")

temp_dist <- st_distance(county_1990_shp, camplocations_df$geometry) %>%
  as_tibble() %>%
  rename(any_of(campnames_lookup)) %>% 
  # keep track of closest camp to county
  rowwise() %>%
  mutate(
    # minimum of dist to any camp
    campclosest_dist = min(c_across(GilaRiver:HeartMt)), 
    # (annoying syntax to get name of closest camp)
    campclosest = names(.)[1:10][which.min(c_across(GilaRiver:HeartMt))],
    logdistclosest = log(campclosest_dist)
  )

# Bind the distances to the county dataset
ctycmpdist_shp <- county_1990_shp %>%
  bind_cols(temp_dist) %>%
  mutate(id1990 = as.numeric(NHGISST) * 10000 + as.numeric(NHGISCTY)) %>%
  select(GilaRiver:id1990) %>%
  st_drop_geometry() # drop gis formatting to make analysis easier

# save distances dataset to file
write_csv(ctycmpdist_shp, "./data/distances.csv")

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

demographics_crosswalked <-
  right_join(crosswalk, demographics_df, 
            by = c("id", "Year"="YEAR"), 
            relationship = "many-to-many") %>% 
  group_by(Year, id1990, STATENAM_1990, NHGISNAM_1990) %>% 
  summarise(across(pop:unmp_rate_other, ~ sum(.x * weight))) %>%
  mutate(
    # outcome: migration percentage of japanese to total new migrants
    y = mig_japn / mig * 100,
    # evacuation zone status
    ez = ifelse(
      STATENAM_1990 %in% c("Arizona", "California", "Oregon", "Washington"),
      1, 0
      )
    ) 

# bind distances to 
main_data <- left_join(demographics_crosswalked, ctycmpdist_shp, by = "id1990")

write_csv(main_data, file = "data/data.csv")

