library(tidyverse)
library(ipumsr)
library(labelled)

RelocationDestinations_Cities <-
  read_csv("./data/WRA-infinitecoop/RelocationDestinations_Cities.csv") %>% 
  mutate(City = str_replace(City, "Berkely", "Berkeley"))

uscities <- read_csv("~/Desktop/FieldPaper/data/simplemaps_uscities_basicv1.78/uscities.csv") %>% 
  add_row(city = "Bridgeton", state_id = "NJ", county_name = "Cumberland") %>% 
  add_row(city = "Venice", state_id = "CA", county_name = "Los Angeles")

RelocationDestinations_Cities <-
  left_join(RelocationDestinations_Cities, uscities, 
            by = c('City'='city', 'State'='state_id')) %>% 
  group_by(State, county_name) %>% 
  mutate(state_name = usdata::abbr2state(State))

load("~/Desktop/FieldPaper/data/counties.Rda")

county_shapes <- map_data("county") %>% 
  mutate(State = str_to_title(region), County = str_to_title(subregion))

counties_maps <- 
  left_join(counties_df, county_shapes, 
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