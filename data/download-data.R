library(tidyverse)
library(ipumsr)

# Download with IPUMS api: ----
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
