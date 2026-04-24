
library(tidyverse)
theme_set(theme_bw())

var <- read.csv("data/funnel.csv") %>% select(Trial, n_Runs, n_Clusters)

ggplot(var, aes(n_Runs, n_Clusters)) + 
  geom_jitter(width = 0.15) +
  ylim(40, 55)
