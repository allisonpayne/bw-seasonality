library(tidyverse)
library(lubridate)
library(mgcv)
# Data from Schoenbeck et al. (2024)

scb_bw <- rbind(
  read_csv("~/Downloads/SiteN_Zc_monthly_bin.csv") %>% 
    mutate(site = "N"),
  read_csv("~/Downloads/SiteH_Zc_monthly_bin.csv") %>% 
    mutate(site = "H")
) %>% 
  mutate(date = dmy(bin),
         month = month(date),
         year = year(date),
         site = factor(site)) %>% 
  arrange(site, date) %>% 
  drop_na(Zc_pres_eff_adj) %>% 
  group_by(site) %>% 
  # For AR1 modeling later - restart within sites
  mutate(AR.start = row_number() == 1) %>%
  ungroup()

ggplot(scb_bw, aes(factor(month), Zc_pres_eff_adj, color = site)) +
  geom_boxplot() +
  theme_bw()

scb_bw_mod <- gam(Zc_pres_eff_adj ~ 
                    s(month, by = site, bs = "cc", k = 12) +
                    site +
                    s(year, bs = "re"),
                  family = tw(link = "log"),
                  data = scb_bw,
                  method = "fREML",
                  knots = list(month = c(1, 12)))

summary(scb_bw_mod)
gam.check(scb_bw_mod)
acf(residuals(scb_bw_mod))

# Residuals are autocorrelated, but for peak timing we don't care. Be careful
# about p-values etc.

scb_bw_grid <- expand.grid(
  month = seq(0, 13, by = 0.1),   # extend beyond 1-12
  site  = unique(scb_bw$site),
  year  = 0
) %>% mutate(site = factor(site, levels = levels(scb_bw$site)))

pred <- predict(scb_bw_mod,
                newdata = scb_bw_grid,
                exclude = "s(year)",
                newdata.guaranteed = TRUE,
                type = "link",
                se.fit = TRUE)

scb_bw_grid <- scb_bw_grid %>%
  mutate(
    fit = exp(pred$fit),
    lwr = exp(pred$fit - 1.96 * pred$se.fit),
    upr = exp(pred$fit + 1.96 * pred$se.fit)
  ) %>% 
  mutate(across(fit:upr, \(x) x / pracma::trapz(month, fit), .names = "{.col}_norm"))

# Find peaks on extended grid, then fold month back to 1-12

peaks <- scb_bw_grid %>%
  group_by(site) %>%
  group_modify(~ find_peaks(.x)) %>%
  ungroup()

peaks$fit_norm <- peaks$fit / pracma::trapz(scb_bw_grid$month, scb_bw_grid$fit)

# Clip grid back to 1-12 for plotting
ggplot(scb_bw_grid %>% filter(month >= 1, month <= 12), 
       aes(x = month, y = fit_norm, color = site)) +
  geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm, fill = site), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = peaks, aes(x = month, y = fit_norm, color = site),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  facet_wrap(~ site, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(x = NULL, y = "Zc presence", color = "Site", fill = "Site", 
       title = "Southern California Bight | 33° N", 
       subtitle = "2007-2020") 

ggsave(here::here("outputs/socal.png"), width = 5, height = 3, plot = last_plot())



############INCIDES#####################

#plot the raw data first - walking through to make sure that there are no data gaps, etc. 
#want to make sure we don't drop any values of beuti or if there are any gaps

#read in the indices
beuti <- read_csv(here::here("data/indices/BEUTI_06-25.csv")) %>% 
  set_names(c("date", "latitude", "beuti")) %>% 
  filter(latitude %in% c(33, 36, 45)) %>% 
  mutate(month = month(date), 
         year = year(date))
scb_beuti <- filter(beuti, latitude == 33)

p1 <- scb_beuti %>% ggplot(aes(date, beuti)) + geom_point() + geom_line() 
p1 +  geom_line(data = scb_bw, aes(as.POSIXct(date), Zc_pres_eff_adj))


scb_beuti_bw <- expand_grid(beuti, site = c("H", "N")) %>%
  select(site, month, year, beuti) %>% 
  full_join(scb_bw,
            by = c("month", "year", "site")) %>% 
  arrange(year, month, site) %>% 
  mutate(site = factor(site))

scb_beuti_mod <- gam(Zc_pres_eff_adj ~ 
                    s(beuti, by = site) +
                    site +
                    s(year, bs = "re"),
                  family = tw(link = "log"),
                  data = scb_beuti_bw,
                  method = "fREML")

max_lag <- 6
lag_mods <- map(0:max_lag, \(.lag) {
  lagged_beuti <- scb_beuti_bw %>% 
    group_by(site) %>% 
    mutate(beuti = lag(beuti, .lag)) %>% 
    slice(-c(1:max_lag)) %>% 
    ungroup()
  gam(Zc_pres_eff_adj ~ 
        s(beuti, by = site) +
        site +
        s(year, bs = "re"),
      family = tw(link = "log"),
      data = lagged_beuti,
      method = "fREML")
})
lag_mods <- map(0:max_lag, \(.lag) {
  lagged_beuti <- scb_beuti_bw %>% 
    group_by(site) %>% 
    mutate(beuti = lag(beuti, .lag)) %>% 
    slice(-c(1:max_lag)) %>% 
    ungroup()
  gam(Zc_pres_eff_adj ~ 
        s(beuti, by = site) +
        site +
        s(year, bs = "re"),
      family = tw(link = "log"),
      data = lagged_beuti,
      method = "fREML")
})
lag_aic <- map_dbl(lag_mods, AIC)
tibble(lag = 0:max_lag,
       aic = lag_aic,
       daic = lag_aic - min(lag_aic))
lag4_mod <- lag_mods[[5]]
p_lagged <- scb_beuti_bw %>% 
  group_by(site) %>% 
  mutate(lag4_beuti = lag(beuti, 4)) %>% 
  slice(-(1:max_lag)) %>% 
  mutate(across(c(lag4_beuti, Zc_pres_eff_adj), 
                \(x) (x - mean(x)) / sd(x))) %>% 
  ungroup() %>% 
  pivot_longer(c(lag4_beuti, Zc_pres_eff_adj), 
               names_to = "var", 
               values_to = "val") %>%
  ggplot(aes(date, val, color = var)) +
  geom_line() +
  facet_grid(cols = vars(site)) +
  theme_classic()
p_nolag <- scb_beuti_bw %>% 
  group_by(site) %>% 
  slice(-(1:max_lag)) %>% 
  mutate(across(c(beuti, Zc_pres_eff_adj), 
                \(x) (x - mean(x)) / sd(x))) %>% 
  ungroup() %>% 
  pivot_longer(c(beuti, Zc_pres_eff_adj), 
               names_to = "var", 
               values_to = "val") %>%
  ggplot(aes(date, val, color = var)) +
  geom_line() +
  facet_grid(cols = vars(site)) +
  theme_classic()
cowplot::plot_grid(p_nolag, p_lagged, ncol = 1)
