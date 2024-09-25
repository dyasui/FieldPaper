
library(tidyverse)
library(stargazer)

# log(0) = -Inf makes problems w/ regressions,
# so I replace them with NA's
data <- read_csv("data/data.csv") %>%
  mutate(
    logdistclosest = ifelse(logdistclosest == -Inf, NA, logdistclosest)
    )

lm1_1940 <- lm(y ~ logdistclosest, data = data %>% filter(Year==1940)) 
lm1_1950 <- lm(y ~ logdistclosest, data = data %>% filter(Year==1950)) 
lm1_1960 <- lm(y ~ logdistclosest, data = data %>% filter(Year==1960)) 
lm1_1970 <- lm(y ~ logdistclosest, data = data %>% filter(Year==1970)) 
lm1_1980 <- lm(y ~ logdistclosest, data = data %>% filter(Year==1980)) 
lm1_1990 <- lm(y ~ logdistclosest, data = data %>% filter(Year==1990)) 
stargazer(
  lm1_1940, lm1_1950, lm1_1960, lm1_1970, lm1_1980, lm1_1990,
  type = "text",
  title = "Effects of Distance to Closest Camp on Japanese Migration by Decade",
  column.labels = c("1940", "1950", "1960", "1970", "1980", "1990"),
  dep.var.labels = "Ratio of Japanese American migrants to total migrants",
  covariate.labels = c( "Log(Distance)", "Constant" ),
  # notes = "Log(Distance) refers to the log of the distance in meters from each the centroid of each county's 1990 area to the nearest of the ten WRA internment camps. Each column represents a separate regression for each census year from 1940 to 1990.",
  omit.stat = c("ser", "f"),
  no.space=TRUE, 
  out = "tables/distreg.tex"
)


lm2_1940 <- lm(y ~ logdistclosest * ez, data = data %>% filter(Year==1940)) 
lm2_1950 <- lm(y ~ logdistclosest * ez, data = data %>% filter(Year==1950)) 
lm2_1960 <- lm(y ~ logdistclosest * ez, data = data %>% filter(Year==1960)) 
lm2_1970 <- lm(y ~ logdistclosest * ez, data = data %>% filter(Year==1970)) 
lm2_1980 <- lm(y ~ logdistclosest * ez, data = data %>% filter(Year==1980)) 
lm2_1990 <- lm(y ~ logdistclosest * ez, data = data %>% filter(Year==1990)) 
stargazer(
  lm2_1940, lm2_1950, lm2_1960, lm2_1970, lm2_1980, lm2_1990,
  type = "text",
  title = "Effects of Distance to Closest Camp on Japanese Migration by Decade",
  column.labels = c("1940", "1950", "1960", "1970", "1980", "1990"),
  dep.var.labels = "Ratio of Japanese American migrants to total migrants",
  covariate.labels = c(
    "Log(distance)",
    "Evacuation Zone (EZ)",
    "Log distance * EZ ",
    "Constant" ),
  # notes = "Log(Distance) refers to the log of the distance in meters from each the centroid of each county's 1990 area to the nearest of the ten WRA internment camps. Counties in Arizona, California, Oregon, and Washington were part of the WRA's 'Evacuation Zone' which in this regression is an indicator value for whether each county is in one of these states. Log(Distance) * EZ is the interaction of the Evacuation Zone indicator variable with the log distance to the cclosest internment camp.",
  omit.stat = c("ser", "f"),
  no.space = TRUE,
  out = "tables/ezdistreg.tex"
)

# Try using polynomial eqn instead of logs
data <- data %>% 
  mutate(cc_dist_km = campclosest_dist / 1000) %>%
  filter(!is.na(cc_dist_km))

lm_poly_1940 <- lm(y ~ cc_dist_km + I((cc_dist_km)^2), data = data %>% filter(Year==1940)) 
lm_poly_1950 <- lm(y ~ cc_dist_km + I((cc_dist_km)^2), data = data %>% filter(Year==1950)) 
lm_poly_1960 <- lm(y ~ cc_dist_km + I((cc_dist_km)^2), data = data %>% filter(Year==1960)) 
lm_poly_1970 <- lm(y ~ cc_dist_km + I((cc_dist_km)^2), data = data %>% filter(Year==1970)) 
lm_poly_1980 <- lm(y ~ cc_dist_km + I((cc_dist_km)^2), data = data %>% filter(Year==1980)) 
lm_poly_1990 <- lm(y ~ cc_dist_km + I((cc_dist_km)^2), data = data %>% filter(Year==1990)) 

stargazer(
  lm_poly_1940, lm_poly_1950, lm_poly_1960, lm_poly_1970, lm_poly_1980, lm_poly_1990,
  type = "text",
  # title = "Effects of Distance to Closest Camp on Japanese Migration by Decade",
  column.labels = c("1940", "1950", "1960", "1970", "1980", "1990"),
  dep.var.labels = "Ratio of Japanese American migrants to total migrants",
  # covariate.labels = c( "Log(Distance)", "Constant" ),
  # notes = "Log(Distance) refers to the log of the distance in meters from each the centroid of each county's 1990 area to the nearest of the ten WRA internment camps. Each column represents a separate regression for each census year from 1940 to 1990.",
  omit.stat = c("ser", "f"),
  no.space=TRUE, 
  # out = "tables/distreg.tex"
)
