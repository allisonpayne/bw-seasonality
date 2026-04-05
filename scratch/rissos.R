library(tidyverse)

#for 2017

gg1 <- read.csv("data/gg/gg_2017_c1.csv") %>% 
  mutate(Date = dmy_hms(Date))
gg2 <- read.csv("data/gg/gg_2017_c2.csv") %>% 
  mutate(Date = dmy_hms(Date))
gg3 <- read.csv("data/gg/gg_2017_c24.csv") %>% 
  mutate(Date = dmy_hms(Date))

gg <- full_join(gg1, gg2, by = "Date") %>% 
  full_join(gg3, by = "Date")

gg <- gg %>% 
  mutate(Date = as.Date(Date)) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Month = month(Date)) %>% 
  group_by(Month) %>% 
  mutate(count = n())

ggplot(gg, aes(Month)) + 
  geom_bar(bins = 12, color = "white") +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                     limits = c(0.5, 12.5)) +
  ylim(0, 18) +
  ylab("Days with Gg") + 
  xlab("Month (2017)") +
  geom_text(aes(label = count),  y = 18, vjust = .75) + 
  theme_bw()

#for 2018

gg1_18 <- read.csv("data/gg/gg_2018_c2.csv") %>% 
  mutate(Date = dmy_hms(Date))
gg2_18 <- read.csv("data/gg/gg_2018_c3.csv") %>% 
  mutate(Date = dmy_hms(Date))
gg3_18 <- read.csv("data/gg/gg_2018_dolph_c2.csv") %>% 
  mutate(Date = dmy_hms(Date))
gg4_18 <- read.csv("data/gg/gg_2018_dolph_c7.csv") %>% 
  mutate(Date = dmy_hms(Date))
gg5_18 <- read.csv("data/gg/gg_2018_dolph_c8.csv") %>% 
  mutate(Date = dmy_hms(Date))
gg6_18 <- read.csv("data/gg/gg_2018_dolph_c11.csv") %>% 
  mutate(Date = dmy_hms(Date))

gg_18 <- full_join(gg1_18, gg2_18, by = "Date") %>% 
  full_join(gg3_18, by = "Date") %>% 
  full_join(gg4_18, by = "Date") %>% 
  full_join(gg5_18, by = "Date") %>% 
  full_join(gg6_18, by = "Date")

gg_18 <- gg_18 %>% 
  mutate(Date = as.Date(Date)) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Month = month(Date)) %>% 
  group_by(Month) %>% 
  mutate(count = n())

ggplot(gg_18, aes(Month)) + 
  geom_bar(bins = 12, color = "white") +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                     limits = c(0.5, 12.5)) +
  ylim(0, 30) +
  ylab("Days with Gg") + 
  xlab("Month (2018)") +
  geom_text(aes(label = count),  y = 30, vjust = .75) + 
  theme_bw()
