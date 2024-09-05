library(tidyverse)
library(ipumsr)
library(labelled)
library(sf)

RelocationDestinations_Cities <-
  read_csv("./data/WRA-infinitecoop/RelocationDestinations_Cities.csv") %>% 
  mutate(City = str_replace(City, "Berkely", "Berkeley")) %>% 
  mutate(State_name = usdata::abbr2state(State))

places_1950 <- read_ipums_sf("data/maps/nhgis0023_shape.zip") 

RelocationDestinations_Cities <- 
  left_join(RelocationDestinations_Cities, places_1950, 
            by = c('City'='NAME', 'State_name'='STATE')) 

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
