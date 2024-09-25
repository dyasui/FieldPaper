library(tidyverse)
library(ipumsr)

# Categories and stuff: ---------------
# evacuation zone west coast states:
c(61, 71, 72, 73)
# evac zone counties ICPSR codes:
ezAZ = c(30, 130, 190, 230, 270)
ezOR = c(30, 50, 70, 90, 110, 190, 270, 290, 330, 390, 410, 430, 470, 510, 530, 570, 670, 710)
ezWA = c(50, 70, 90, 150, 270, 310, 330, 350, 370, 390, 410, 450, 490, 530, 570, 590, 610, 670, 690, 730, 770)
# counties split by zone boundary:
spAZ = c(90, 130, 150, 210, 250)
spOR = c(170, 310, 350, 550, 650)
spWA = c(470)
# drop overseas military and Wash. D.C. and unclassified:
drop_states = c(83, 96, 97, 98, 99)


# IPUMS DATA: ------------------------
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

# Read from downloaded ddi file after running download-data.R
fullcount_ddi <- list.files(path = "data/fullcount", pattern = "*.xml") %>%
  # in case of multiple files, choose most recent
  sort(decreasing = TRUE) %>%
  head(1)

ddi <- read_ipums_ddi(paste("data/fullcount/", fullcount_ddi, sep = ""))

df = read_ipums_micro(ddi) %>% 
  filter(!(STATEICP %in% drop_states)) %>% # drop extra states/territories
  mutate(EZ = ifelse(#STATEICP %in% c(61, 71, 72, 73),
                     (STATEICP == 71) | # all CA counties
                     ((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
                     ((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
                     ((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
                     1, # 1 is subject to evacuation
                     0 # 0 were not (fully) subject to evac
  )) %>% 
  mutate(ISSEI = ifelse(NATIVITY==5, 1, 0)) 

sum_data = df %>%
  #filter(AGE>=18) %>% 
  group_by(EZ) %>% 
  summarise(SEX = mean(SEX, na.rm=TRUE),
            AGE = mean(AGE, na.rm=TRUE),
            ISSEI = mean(ISSEI, na.rm=TRUE),
            N = n()
            )

county_data = df %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  summarise(SEX = mean(SEX, na.rm=TRUE),
            AGE = mean(AGE, na.rm=TRUE),
            ISSEI = mean(ISSEI, na.rm=TRUE),
            N = n()
  ) %>% 
  mutate(shrJA = N/nrow(df))

# Get historical state id codes:
library("readxl")
icpsrcnt <- read_excel("icpsrcnt.xlsx") # historical state ID codes
p_load(maps, stringr)
p_load(sf) # shapefiles package
counties40 = st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1940/nhgis0002_shapefile_tl2000_us_county_1940/US_county_1940.shp")
states <- map_data("county") %>% 
  mutate(State = str_to_title(region),
         County = str_to_title(subregion)) 
countiesICP = left_join(states, icpsrcnt, by = c("State","County")) %>% 
  mutate(EZ = ifelse(#STATEICP %in% c(61, 71, 72, 73),
    (STATEICP == 71) | # all CA counties
      ((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
      ((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
      ((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
    1, # 1 is subject to evacuation
    0 # 0 were not (fully) subject to evac
  ))

# get data into map form:
county_map_data = left_join(countiesICP, county_data, by = c("STATEICP","COUNTYICP"))
county_map_data[is.na(county_map_data)] = 0 # replace NA with zeros

# create map:
p_load(ggplot2, ggthemes, paletteer, viridis, gganimate, transformr)
county_map = ggplot() + 
  geom_polygon(data=county_map_data, aes(x=long, y=lat, group=group,
                                        fill=shrJA),
               color="black", linewidth=.1 ) +
  scale_fill_viridis(
                     breaks = c(.001, .01, .1, 0.2952104925),
                     trans="log", 
                     option="D") +
  # geom_polygon(data=county_map_data, aes(x=long, y=lat, group=group),
  #              fill=NA, color=ifelse(county_map_data$EZ==1,"white",NA), 
  #              linewidth=.4, alpha=.5) +
  labs(title="County shares of Total Japanese Population",
       subtitle="in 1940 Full-Count Census",
       x="",
       y="",
       caption="Share of all Japanese individuals reported in the 1940 cenus living in each county (drawn with 2010 boundaries).
       Data provided by IPUMS USA: 
       Steven Ruggles, Catherine A. Fitch, Ronald Goeken, J. David Hacker, Matt A. Nelson, Evan Roberts, Megan Schouweiler, and Matthew Sobek. 
       IPUMS Ancestry Full Count Data: Version 3.0 [dataset]. Minneapolis, MN: IPUMS, 2021. https://doi.org/10.18128/D014.V3.0") +
  theme_stata() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom") +
  guides(colour = guide_legend(title="County's share of Japanese Population in 1940",
                               title.position = "top"))
county_map
ggsave("county_JAmap.png")

# Occupations:
OCC1950_codes = read_csv("occupations_codes.csv")
occupation_table = as_tibble(df %>% group_by(OCC1950)  %>% count() %>% ungroup()) 
occupation_table = occupation_table[order(-occupation_table$n),]
print(occupation_table)

