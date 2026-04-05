library(tidyverse)
source(here::here("R/bw_funcs.R"))

### Zc ###

zc2017 <- bw_sightings(c(
  "data/raw_bw/zc/zc_2017_c8.csv", 
  "data/raw_bw/zc/zc_2017_c40.csv", 
  "data/raw_bw/zc/zc_2017_c41.csv"))

zc2018 <- bw_sightings(c(
  "data/raw_bw/zc/zc_2018_c20.csv", 
  "data/raw_bw/zc/zc_2018_c31.csv", 
  "data/raw_bw/zc/zc_2018_c36.csv"
))

zc <- full_join(zc2017, zc2018)
write_csv(zc, "data/processed_bw/zc_monterey.csv")

### Bb ###

bb2017 <- bw_sightings(c(
  "data/raw_bw/bb/bb_2017_c17.csv",
  "data/raw_bw/bb/bb_2017_c23.csv"
))

bb2018 <- bw_sightings(c(
  "data/raw_bw/bb/bb_2018_c11.csv", 
  "data/raw_bw/bb/bb_2018_c13.csv", 
  "data/raw_bw/bb/bb_2018_c14.csv", 
  "data/raw_bw/bb/bb_2018_c40.csv"))

bb <- full_join(bb2017, bb2018)
write_csv(bb, "data/processed_bw/bb_monterey.csv")
