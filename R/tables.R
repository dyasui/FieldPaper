library(tidyverse)
library(ipumsr)
library(labelled)
library(sf)

data <- read_csv("data/data.csv")

library(stargazer)
data %>% 
  select(pop, mig, migratio, mig_japn, pop_japn, y, AGE, i_female, INCWAGE, unmp_rate, campclosest_dist) %>%
  as.data.frame() %>% 
  stargazer(
    # type = "text", 
    out = "tables/ctysumstats.tex",
    title = "County-Year Summary Statistics",
    label = "ctysumstats",
    covariate.labels = c(
      "Population", "New Migrants", "Migrant/Pop Ratio",
      "New Japanese American Migrants", "Current Japanese Population", 
      "Japanese/Total Migration Ratio", "Average Age", "Percent Female",
      "Average Wage", "Unemployment Rate", "Distance to Closest Camp"
    ),
    digits = 2
  )
