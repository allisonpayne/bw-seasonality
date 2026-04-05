library(tidyverse)
library(lubridate)
library(mgcv)
source(here::here("R/bw_funcs.R"))

oregon_bw <- read_csv(here::here("data/othersites/monthly_presence_zc_oregon.csv")) %>% 
  mutate(date = ymd(month_year), 
         month = month(month_year),
         year = year(month_year)) %>% 
  arrange(date) %>% 
  complete(year = 2022, month = 1:12, fill = list(days_present = 0))
  
  
ggplot(oregon_bw, aes(factor(month), days_present)) +
  geom_boxplot() +
  theme_bw()

oregon_bw_mod <- gam(days_present ~ 
                    s(month, bs = "cc", k = 12) +
                    s(year, bs = "re"),
                  family = tw(link = "log"),
                  data = oregon_bw,
                  method = "fREML")

summary(oregon_bw_mod)
gam.check(oregon_bw_mod)
acf(residuals(oregon_bw_mod))

oregon_bw_grid <- expand.grid(
  month = seq(0, 13, by = 0.1),   # extend beyond 1-12
  year  = 0)

pred <- predict(oregon_bw_mod,
                newdata = oregon_bw_grid,
                exclude = "s(year)",
                newdata.guaranteed = TRUE,
                type = "link",
                se.fit = TRUE)

oregon_bw_grid <- oregon_bw_grid %>%
  mutate(
    fit = exp(pred$fit),
    lwr = exp(pred$fit - 1.96 * pred$se.fit),
    upr = exp(pred$fit + 1.96 * pred$se.fit)
  ) %>% 
  mutate(across(fit:upr, \(x) x / pracma::trapz(month, fit), .names = "{.col}_norm"))

peaks <-  find_peaks(oregon_bw_grid)
peaks$fit_norm <- peaks$fit / pracma::trapz(oregon_bw_grid$month, oregon_bw_grid$fit)

# Clip grid back to 1-12 for plotting
ggplot(oregon_bw_grid %>% filter(month >= 1, month <= 12), 
       aes(x = month, y = fit_norm)) +
  geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = first(peaks), aes(x = month, y = fit_norm),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw() +
  labs(x = NULL, 
       y = "Zc presence", 
       title = "Newport, Oregon | 45° N", 
       subtitle = "2021-2022") 

ggsave(here::here("outputs/oregon.png"), width = 5, height = 3, plot = last_plot())
