
library(tidyverse)
library(ipumsr)
library(labelled)
library(geos)
library(data.table)
library(sf)
library(stargazer)

data <- read_csv("data/data.csv")

data <- data %>% 
  mutate(
    y = mig_japn / mig_total,
    ez = ifelse(
      STATENAM %in% c("Arizona", "California", "Oregon", "Washington"),
      1, 0
    )
    ) # %>%
  # mutate(across(GilaRiver:HeartMt, ~ lm(., na.rm=TRUE)))

lm1_1940 <- lm(y ~ lm(campclosest_dist), data = data %>% filter(Year==1940)) 
lm1_1950 <- lm(y ~ lm(campclosest_dist), data = data %>% filter(Year==1950)) 
lm1_1960 <- lm(y ~ lm(campclosest_dist), data = data %>% filter(Year==1960)) 
lm1_1970 <- lm(y ~ lm(campclosest_dist), data = data %>% filter(Year==1970)) 
lm1_1980 <- lm(y ~ lm(campclosest_dist), data = data %>% filter(Year==1980)) 
lm1_1990 <- lm(y ~ lm(campclosest_dist), data = data %>% filter(Year==1990)) 
stargazer(lm1_1940, lm1_1950, lm1_1960, lm1_1970, lm1_1980, lm1_1990,  out = "tables/fig1.tex")

lm2_1940 <- lm(y ~ lm(campclosest_dist) * ez, data = data %>% filter(Year==1940)) 
lm2_1950 <- lm(y ~ lm(campclosest_dist) * ez, data = data %>% filter(Year==1950)) 
lm2_1960 <- lm(y ~ lm(campclosest_dist) * ez, data = data %>% filter(Year==1960)) 
lm2_1970 <- lm(y ~ lm(campclosest_dist) * ez, data = data %>% filter(Year==1970)) 
lm2_1980 <- lm(y ~ lm(campclosest_dist) * ez, data = data %>% filter(Year==1980)) 
lm2_1990 <- lm(y ~ lm(campclosest_dist) * ez, data = data %>% filter(Year==1990)) 
stargazer(lm2_1940, lm2_1950, lm2_1960, lm2_1970, lm2_1980, lm2_1990,  out = "tables/fig2.tex")

