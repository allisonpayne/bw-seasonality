library(tidyverse)
library(mgcv)
source(here::here("R/bw_funcs.R"))


dat <- read_csv(c("data/othersites/channel_islands/SOCAL_W_01_WW_encounterTimes_Ziphius_cavirostris.csv", 
                "data/othersites/channel_islands/SOCAL_W_02_WW_encounterTimes_Ziphius_cavirostris.csv", 
                "data/othersites/channel_islands/SOCAL_W_03_WW_encounterTimes_Ziphius_cavirostris.csv",
                "data/othersites/channel_islands/SOCAL_W_04_WW_encounterTimes_Ziphius_cavirostris.csv", 
                "data/othersites/channel_islands/SOCAL_W_05_WW_encounterTimes_Ziphius_cavirostris.csv"))

dat <- dat %>% 
  mutate(mins = as.numeric(encDur) / 60, 
         date = dmy_hms(endEnc), 
         year = year(date), 
         month = month(date)) %>% 
  group_by(year, month) %>% 
  summarize(total_mins = sum(mins), 
            avg_mins = mean(mins)) 

ggplot(dat, aes(month, total_mins, color = factor(year))) + 
  geom_line() +
  geom_point() + 
  theme_bw()

ci_bw_mod <- gam(total_mins ~ 
                    s(month, bs = "cc", k = 12) +
                    s(year, bs = "re"),
                  family = tw(link = "log"),
                  data = dat,
                  method = "fREML",
                  knots = list(month = c(1, 12)))

ci_bw_grid <- expand.grid(
  month = seq(0, 13, by = 0.1),   # extend beyond 1-12
  year  = 0
) 

pred <- predict(ci_bw_mod,
                newdata = ci_bw_grid,
                exclude = "s(year)",
                newdata.guaranteed = TRUE,
                type = "link",
                se.fit = TRUE)

ci_bw_grid <- ci_bw_grid %>%
  mutate(
    fit = exp(pred$fit),
    lwr = exp(pred$fit - 1.96 * pred$se.fit),
    upr = exp(pred$fit + 1.96 * pred$se.fit)
  ) %>% 
  mutate(across(fit:upr, \(x) x / pracma::trapz(month, fit), .names = "{.col}_norm"))

# Find peaks on extended grid, then fold month back to 1-12

peaks <-  find_peaks(ci_bw_grid)

peaks$fit_norm <- peaks$fit / pracma::trapz(ci_bw_grid$month, ci_bw_grid$fit)

# Clip grid back to 1-12 for plotting
ggplot(ci_bw_grid %>% filter(month >= 1, month <= 12), 
       aes(x = month, y = fit_norm)) +
  geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = peaks, aes(x = month, y = fit_norm),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw() +
  labs(x = NULL, y = "Zc presence", color = "Site", fill = "Site", 
       title = "Channel Islands | 34° N", 
       subtitle = "2021-2023") 
