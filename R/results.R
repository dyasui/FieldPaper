
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
    # outcome: migration ratio of japanese to total new migrants
    y = mig_japn / mig_total,
    # evacuation zone status
    ez = ifelse(
      STATENAM %in% c("Arizona", "California", "Oregon", "Washington"),
      1, 0
    )
    ) # %>%
  # mutate(across(GilaRiver:HeartMt, ~ lm(., na.rm=TRUE)))

lm1_1940 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1940)) 
lm1_1950 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1950)) 
lm1_1960 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1960)) 
lm1_1970 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1970)) 
lm1_1980 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1980)) 
lm1_1990 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1990)) 
# stargazer(lm1_1940, lm1_1950, lm1_1960, lm1_1970, lm1_1980, lm1_1990,  out = "tables/fig1.tex")
stargazer(
  lm1_1940, lm1_1950, lm1_1960, lm1_1970, lm1_1980, lm1_1990,
  type = "text",
  title = "Effects of Distance to Closest Camp on Japanese Migration by Decade",
  column.labels = c("1940", "1950", "1960", "1970", "1980", "1990"),
  dep.var.labels = "Ratio of Japanese American migrants to total migrants",
  covariate.labels = c("Log distance to closest camp in meters", "Constant"),
  omit.stat = c("ser", "f"),
  no.space=TRUE
)


lm2_1940 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1940)) 
lm2_1950 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1950)) 
lm2_1960 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1960)) 
lm2_1970 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1970)) 
lm2_1980 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1980)) 
lm2_1990 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1990)) 
# stargazer(lm2_1940, lm2_1950, lm2_1960, lm2_1970, lm2_1980, lm2_1990,  out = "tables/fig2.tex")
stargazer(
  lm2_1940, lm2_1950, lm2_1960, lm2_1970, lm2_1980, lm2_1990,
  type = "text"
)

