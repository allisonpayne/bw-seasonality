library(tidyverse)
source(here::here("R/bw_funcs.R"))

### Zc ###

zc2016 <- bw_sightings(c(
  "data/raw_bw/zc/zc_2016_c5.csv", 
  "data/raw_bw/zc/zc_2016_c17.csv", 
  "data/raw_bw/zc/zc_2016_c41.csv", 
  "data/raw_bw/zc/zc_2016_c5_ii.csv"
))

zc2017 <- bw_sightings(c(
  "data/raw_bw/zc/zc_2017_c8.csv", 
  "data/raw_bw/zc/zc_2017_c40.csv", 
  "data/raw_bw/zc/zc_2017_c41.csv"))

zc2018 <- bw_sightings(c(
  "data/raw_bw/zc/zc_2018_c20.csv", 
  "data/raw_bw/zc/zc_2018_c31.csv", 
  "data/raw_bw/zc/zc_2018_c36.csv"
))

zc <- full_join(zc2016, zc2017) %>% 
  full_join(zc2018)
write_csv(zc, "data/processed_bw/zc_monterey.csv")

### Bb ###

bb2016 <- bw_sightings(c(
  "data/raw_bw/bb/bb_2016_c14.csv",
  "data/raw_bw/bb/bb_2016_c16.csv", 
  "data/raw_bw/bb/bb_2016_c19.csv", 
  "data/raw_bw/bb/bb_2016_c25.csv", 
  "data/raw_bw/bb/bb_2016_c50.csv"
))

bb2017 <- bw_sightings(c(
  "data/raw_bw/bb/bb_2017_c17.csv",
  "data/raw_bw/bb/bb_2017_c23.csv"
))

bb2018 <- bw_sightings(c(
  "data/raw_bw/bb/bb_2018_c11.csv", 
  "data/raw_bw/bb/bb_2018_c13.csv", 
  "data/raw_bw/bb/bb_2018_c14.csv", 
  "data/raw_bw/bb/bb_2018_c40.csv"))

bb <- full_join(bb2016, bb2017) %>% 
  full_join(bb2018)
write_csv(bb, "data/processed_bw/bb_monterey.csv")
