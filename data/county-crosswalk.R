library(tidyverse)
library(ipumsr)

# Crosswalk file from Eckert, Gvirtz, Liang, and Peters (2020)
# github repo link for replication data:
url <- "https://raw.githubusercontent.com/liang-jack-a/EGLP_Crosswalk/master/Crosswalks/county_crosswalk_endyr_1990.csv"
crosswalk <- read_csv(url) %>%
  filter(Year %in% 1940:1980) %>% # 1990 counties don't appear as their own rows
  mutate( # combine state and county codes into single id variable
    id     = as.numeric(NHGISST) * 10000 + as.numeric(NHGISCTY),
    id1990 = as.numeric(NHGISST_1990) * 10000 + as.numeric(NHGISCTY_1990)) 
crossalk <- crosswalk %>% 
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

migcountypanel <- migrations_cleaned_df %>% 
  pivot_wider(names_from = Year, 
              values_from = c(mig_total, mig_white, mig_black, mig_aian, mig_oapi, mig_chin, mig_japn, mig_other,
                              pop_total, pop_white, pop_black, pop_aian, pop_oapi, pop_chin, pop_japn, pop_other)
              ) %>% 
  filter(!is.na(pop_total_1940), !is.na(pop_total_1950), !is.na(pop_total_1960), !is.na(pop_total_1970), !is.na(pop_total_1980))
