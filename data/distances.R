library(tidyverse)
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
