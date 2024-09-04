
library(tidyverse)
library(ipumsr)
library(labelled)
library(geos)
library(data.table)
library(sf)

library(tidyverse)
library(stargazer)
data <- read_csv("data/joined.csv") %>% 
  select(!`...1`) %>% 
  filter(!if_any(dist_GilaRiver:dist_HeartMt, is.na))

data <- data %>% 
  mutate(
    y = mig_japn / mig_total,
    ez = ifelse(
      STATENAM_1990 %in% c("Arizona", "California", "Oregon", "Washington"),
      1, 0
    )
    ) %>% 
  rowwise() %>% 
  mutate(dist_nearest = min(dist_GilaRiver:dist_HeartMt, na.rm=TRUE)) 

lm1 <- lm(y ~ dist_nearest, data = data) 
lm2 <- lm(y ~ dist_nearest*ez, data = data)
stargazer(lm1, lm2, type = "text")


