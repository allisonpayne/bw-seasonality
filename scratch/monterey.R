library(tidyverse)
library(lubridate)
library(mgcv)
source(here::here("R/bw_funcs.R"))

monterey_bw <- read_csv(here::here("data/zc_monterey.csv")) %>% 
  mutate(date = ymd(Date), 
         month = Month,
         year = year(Date)) %>% 
  arrange(date) %>% 
  complete(year = 2017:2018, month = 1:12, fill = list(count = 0))


ggplot(monterey_bw, aes(factor(month), count)) +
  geom_boxplot() +
  theme_bw()

monterey_bw_mod <- gam(count ~ 
                       s(month, bs = "cc", k = 12) +
                       s(year, bs = "re"),
                     family = tw(link = "log"),
                     data = monterey_bw,
                     method = "fREML")

summary(monterey_bw_mod)
gam.check(monterey_bw_mod)
acf(residuals(monterey_bw_mod))

monterey_bw_grid <- expand.grid(
  month = seq(0, 13, by = 0.1),   # extend beyond 1-12
  year  = 0)

pred <- predict(monterey_bw_mod,
                newdata = monterey_bw_grid,
                exclude = "s(year)",
                newdata.guaranteed = TRUE,
                type = "link",
                se.fit = TRUE)

monterey_bw_grid <- monterey_bw_grid %>%
  mutate(
    fit = exp(pred$fit),
    lwr = exp(pred$fit - 1.96 * pred$se.fit),
    upr = exp(pred$fit + 1.96 * pred$se.fit)
  ) %>% 
  mutate(across(fit:upr, \(x) x / pracma::trapz(month, fit), .names = "{.col}_norm"))

peaks <-  find_peaks(monterey_bw_grid)
peaks$fit_norm <- peaks$fit / pracma::trapz(monterey_bw_grid$month, monterey_bw_grid$fit)

# Clip grid back to 1-12 for plotting
ggplot(monterey_bw_grid %>% filter(month >= 1, month <= 12), 
       aes(x = month, y = fit_norm)) +
  geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = peaks, aes(x = month, y = fit_norm),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw() +
  labs(x = NULL, 
       y = "Zc presence", 
       title = "Monterey Bay, California | 36° N", 
       subtitle = "2017-2018") 

ggsave(here::here("outputs/monterey.png"), width = 5, height = 3, plot = last_plot())
