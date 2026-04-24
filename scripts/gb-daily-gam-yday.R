library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
source(here::here("R/bw_funcs.R"))

zc_mb <- read_csv(here::here("data/processed_bw/zc_monterey.csv")) 

zc_mb_monthly <- zc_mb %>% 
  mutate(date = ymd(Date),
         month = Month,
         year = year(Date)) %>%
  arrange(date) %>%
  #this is okay because I know we had full effort in 2016-18, but will need to change if adding data from years where we had off effort months
  complete(year = 2016:2018, month = 1:12, fill = list(count = 0)) %>%
  distinct(year, month, count) %>%
  mutate(date = ISOdate(year, month, day = 1))

load(here::here("data/MARS_DailyPercentRecorded.RData")) 

P <- P %>% 
  rename(Date = daystart) %>% 
  filter(year(Date) < 2019)

zc_mb_daily <- P %>% 
  left_join(
    zc_mb %>% 
      distinct(Date) %>% 
      mutate(present = TRUE), 
    by = "Date"
  ) %>% 
  mutate(present = replace_na(present, FALSE),
         month = month(Date), 
         year = factor(year(Date))) %>% 
  filter(PercentRecorded >= 85)

ggplot(zc_mb_daily, aes(Date, present)) + 
  geom_point()

#GAM#

zc_mb_daily_mod <- gam(present ~ 
                         s(month, bs = "cc", k = 12) +
                         s(year, bs = "re"),
                       family = binomial(link = "logit"),
                       data = zc_mb_daily,
                       method = "REML")

summary(zc_mb_daily_mod)
gam.check(zc_mb_daily_mod)
acf(residuals(zc_mb_daily_mod))

zc_mb_grid <- expand.grid(
  month = seq(0, 13, by = 0.1),   # extend beyond 1-12
  year  = 0)

pred <- predict(zc_mb_daily_mod,
                newdata = zc_mb_grid,
                exclude = "s(year)",
                newdata.guaranteed = TRUE,
                type = "link",
                se.fit = TRUE)

zc_mb_grid <- zc_mb_grid %>%
  mutate(
    fit = plogis(pred$fit),
    lwr = plogis(pred$fit - 1.96 * pred$se.fit),
    upr = plogis(pred$fit + 1.96 * pred$se.fit)
  ) %>% 
  mutate(across(fit:upr, \(x) x / pracma::trapz(month, fit), .names = "{.col}_norm"))

zc_mb_peaks <- find_peaks(zc_mb_grid)
zc_mb_peaks$fit_norm <- zc_mb_peaks$fit / pracma::trapz(zc_mb_grid$month, zc_mb_grid$fit)

# Clip grid back to 1-12 for plotting
ggplot(zc_mb_grid %>% filter(month >= 1, month <= 12), 
                          aes(x = month, y = fit_norm)) +
  geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = zc_mb_peaks[2:3,], aes(x = month, y = fit_norm),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  # ylim(0, .2) + 
  theme_bw() +
  labs(x = NULL, 
       y = "Zc")

#with regular fit, not normalized
ggplot(zc_mb_grid %>% filter(month >= 1, month <= 12), 
       aes(x = month, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = zc_mb_peaks[2:3,], aes(x = month, y = fit),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  # ylim(0, .2) + 
  theme_bw() +
  labs(x = NULL, 
       y = "Zc")
