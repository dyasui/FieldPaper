
library(tidyverse)
library(ipumsr)
library(labelled)
library(geos)
library(data.table)
library(sf)
library(stargazer)

data <- read_csv("data/data.csv") %>% 
  mutate(
    # outcome: migration percentage of japanese to total new migrants
    y = mig_japn / mig_total * 100,
    # evacuation zone status
    ez = ifelse(
      STATENAM %in% c("Arizona", "California", "Oregon", "Washington"),
      1, 0
    )
    ) # %>%
  # mutate(across(GilaRiver:HeartMt, ~ lm(., na.rm=TRUE)))

# data <- tibble(
#   campclosest_dist = rnorm(1800000, 100000, n = 3141),
#   y = rnorm(0.15, 0.1, n = 3141),
#   Year = sample(c(1940, 1950 ,1960, 1970, 1980, 1990), size = 3141, replace = TRUE)
# )

lm1_1940 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1940)) 
lm1_1950 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1950)) 
lm1_1960 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1960)) 
lm1_1970 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1970)) 
lm1_1980 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1980)) 
lm1_1990 <- lm(y ~ log(campclosest_dist), data = data %>% filter(Year==1990)) 
stargazer(
  lm1_1940, lm1_1950, lm1_1960, lm1_1970, lm1_1980, lm1_1990,
  type = "text",
  title = "Effects of Distance to Closest Camp on Japanese Migration by Decade",
  column.labels = c("1940", "1950", "1960", "1970", "1980", "1990"),
  dep.var.labels = "Ratio of Japanese American migrants to total migrants",
  # covariate.labels = c("Log distance to closest camp in meters", "Constant"),
  omit.stat = c("ser", "f"),
  no.space=TRUE, 
  out = "tables/distreg.tex"
)


lm2_1940 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1940)) 
lm2_1950 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1950)) 
lm2_1960 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1960)) 
lm2_1970 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1970)) 
lm2_1980 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1980)) 
lm2_1990 <- lm(y ~ log(campclosest_dist) * ez, data = data %>% filter(Year==1990)) 
stargazer(
  lm2_1940, lm2_1950, lm2_1960, lm2_1970, lm2_1980, lm2_1990,
  type = "text",
  title = "Effects of Distance to Closest Camp on Japanese Migration by Decade",
  column.labels = c("1940", "1950", "1960", "1970", "1980", "1990"),
  dep.var.labels = "Ratio of Japanese American migrants to total migrants",
  covariate.labels = c(
    "Log distance to closest camp in meters",
    "Evacuation Zone status",
    "Log distance * Evacuation Zone status",
    "Constant"
  ),
  omit.stat = c("ser", "f"),
  no.space=TRUE,
  out = "tables/ezdistreg.tex"
)

