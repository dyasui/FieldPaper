library(tidyverse)
library(labelled)
library(ipumsr)

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

