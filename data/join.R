library(tidyverse)

migrations_crosswalked_df <- read_csv("./data/migrations_crosswalked.csv")

distances_df <- read_csv("./data/distances.csv")

joined_df <-
  left_join(migrations_crosswalked_df, distances_df, 
            by = c("STATENAM_1990"="STATENAM", "NHGISNAM_1990"="NHGISNAM"))
write.csv(joined_df, file = "./data/joined.csv")
