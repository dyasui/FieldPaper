# download data using IPUMS API 
library(tidyverse)
library(ipumsr)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - # 
# NOTE: use `set_ipums_api_key()`                       # 
# with API key from https://account.ipums.org/api_keys  #
# to be able to use the functions below to query ipums  #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - # 

# ---- 1940-1990 1% MIGRATION CENSUS DATA ----
# select census samples for ipums API -----
migration_samples <- get_sample_info("usa") %>%
  filter(str_detect(description, 
        '1940 1%|1950 1%|1960 5%|1970 Form 2 Metro|1980 1%|1990 1%')) %>%
  select(name) %>% pull()

# select variables -----
variables <- list(
  "YEAR"     , "PERWT"    , 
  "STATEFIP" , "STATEICP" , "COUNTYICP" , "METAREA"  , "CITY"     , 
  "MIGRATE5" , "MIGPLAC5" , "MIGRATE1"  , "MIGPLAC1" ,
  "RACE"     , "BPL"      ,  "AGE"       , "SEX" ,
  "EMPSTAT"  , "LABFORCE" , "INCWAGE"   , "INCTOT"   , "OCCSCORE" 
  )

# pipeline to create and download data extract using ipumsr -----
define_extract_usa(description = "census migration data for years 1940 through 1990",
                     samples = migration_samples,
                     variables = variables) %>%
  submit_extract() %>%
  # use is_extract_ready() if you want to avoid R session from being occupied
  wait_for_extract() %>%
  download_extract(download_dir = "data/census")


# ---- FULL COUNT 1940 & 1950 CENSUS ----
fullcount_samples <- get_sample_info("usa") %>% 
  filter( description %in% c( "1940 100% database", "1950 100% database" )) %>%
  select(name) %>% pull()

# migration variables not available for 1950 100% database
variables <- list(
  "YEAR"     , "PERWT"    , 
  "STATEFIP" , "STATEICP" , "COUNTYICP" , "METAREA"  , "CITY"     , 
  # "MIGRATE5" , "MIGPLAC5" , "MIGRATE1"  , "MIGPLAC1" ,
  "RACE"     , "BPL"      , # "AGE"       , "SEX",
  "EMPSTAT"  , "LABFORCE" , "INCWAGE"   , "INCTOT"   , "OCCSCORE" 
)

# Submit extract and download
define_extract_usa(
  description = "full-count census data for years 1940 and 1950",
  samples = fullcount_samples,
  variables = variables) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = "data/census")

fullcount_df <- fread("./data/census/usa_00075.dat.gz")

# Download county shapefiles with IPUMS api: ----
county_shapefiles <- ipumsr::get_metadata_nhgis("shapefiles") %>%
  filter(year == 1990,
         geographic_level == 'County',
         str_detect( basis, '^2008' )) %>% # 2008 TIGER/Lines basis
  select(name) %>% pull()

tictoc::tic()
for (year in county_shapefiles) {
  define_extract_nhgis(
    description = "NHGIS county shapefiles for census years 1940 to 1990",
    shapefiles = year ) %>%
    submit_extract() %>%
    wait_for_extract() %>%
    download_extract(download_dir = "./data/maps/")
}
tictoc::toc()


