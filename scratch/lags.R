library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
source(here::here("R/bw_funcs.R"))

#READ IN DATA
monterey_bw <- read_csv(here::here("data/zc_monterey.csv")) %>% 
  mutate(date = ymd(Date), 
         month = Month,
         year = year(Date)) %>% 
  arrange(date) %>% 
  complete(year = 2017:2018, month = 1:12, fill = list(count = 0)) %>% 
  distinct(year, month, count) %>% 
  mutate(date = ISOdate(year, month, day = 15))

#FIT GAM 
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

peaks_mb <-  find_peaks(monterey_bw_grid)
peaks_mb$fit_norm <- peaks_mb$fit / pracma::trapz(monterey_bw_grid$month, monterey_bw_grid$fit)

# Clip grid back to 1-12 for plotting
mb_norm_plot <- ggplot(monterey_bw_grid %>% filter(month >= 1, month <= 12), 
                       aes(x = month, y = fit_norm)) +
  # geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = peaks_mb[2,], aes(x = month, y = fit_norm),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw() +
  labs(x = NULL, 
       y = "Zc presence", 
       title = "Monterey Bay, California | 36° N", 
       subtitle = "2017-2018") 

#SIMULATE FOR ZC
n_sim <- 10000
Xp_bw <- predict(monterey_bw_mod, 
                 newdata = monterey_bw_grid, 
                 type = "lpmatrix")
coef_sims_bw <- MASS::mvrnorm(n_sim, 
                              mu = coef(monterey_bw_mod), 
                              Sigma = vcov(monterey_bw_mod))
sim_curves_bw <- Xp_bw %*% t(coef_sims_bw)
sim_curves_response <- monterey_bw_mod$family$linkinv(sim_curves_bw)
boot_bw <- expand_grid(i = 1:nrow(monterey_bw_grid), 
            j = 1:ncol(sim_curves_response)) %>% 
  mutate(month = monterey_bw_grid$month[i], 
         sim = j, 
         count = sim_curves_response[cbind(i, j)]) %>% 
  select(-c(i, j)) 

# %>% 
#   ggplot(aes(month, count, group = sim)) + 
#   geom_line(alpha = 0.1)


#BEUTI SETUP
beuti <- read_csv(here::here("data/indices/BEUTI_06-25.csv")) %>% 
  set_names(c("date", "latitude", "beuti")) %>% 
  filter(latitude %in% c(33, 36, 45)) %>% 
  mutate(month = month(date), 
         year = year(date))

mb_beuti <- filter(beuti, latitude == 36)

#climatology - gets at the long term average
beuti_clim <- beuti %>% 
  group_by(latitude, month) %>% 
  summarize(beuti = mean(beuti))

beuti_raw_plot <- beuti_clim %>% 
  mutate(latitude = factor(latitude)) %>% 
  ggplot(aes(month, 
             beuti, 
             color = latitude)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "BEUTI") +
  theme_bw() + 
  theme(legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1), 
        legend.background = element_rect(fill = "transparent"))

beuti_clim_mod <- gam(beuti ~ 
                        s(month, bs = "cc", k = 12) +
                        s(year, bs = "re"),
                      family = gaussian(),
                      data = mb_beuti,
                      method = "REML")
summary(beuti_clim_mod)

mb_beuti_grid <- expand.grid(
  month = seq(0, 13, by = 0.1),   # extend beyond 1-12
  year  = 0)

pred <- predict(beuti_clim_mod,
                newdata = mb_beuti_grid,
                exclude = "s(year)",
                newdata.guaranteed = TRUE,
                type = "link",
                se.fit = TRUE)

mb_beuti_grid$beuti <- pred$fit
mb_beuti_grid$lower <- pred$fit - pred$se.fit * 1.96
mb_beuti_grid$upper <- pred$fit + pred$se.fit * 1.96

mb_beuti_grid %>% 
  filter(between(month, 1, 12)) %>% 
  ggplot(aes(month, beuti)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() + 
  theme_classic() +
  scale_x_continuous(breaks = 1:12) + 
  ggtitle("Monterey Bay")


#SIMULATE FOR BEUTI
Xp <- predict(beuti_clim_mod, newdata = mb_beuti_grid, type = "lpmatrix")
coef_sims <- MASS::mvrnorm(n_sim, mu = coef(beuti_clim_mod), Sigma = vcov(beuti_clim_mod))
sim_curves <- Xp %*% t(coef_sims)
#will need to transform scale for beaked whale one: sim_curves_response <- gam_mod$family$linkinv(sim_curves)
boot_beuti <- expand_grid(i = 1:nrow(mb_beuti_grid), 
            j = 1:ncol(sim_curves)) %>% 
  mutate(month = mb_beuti_grid$month[i], 
         sim = j, 
         beuti = sim_curves[cbind(i, j)]) %>% 
  select(-c(i, j)) 

# %>% 
#   ggplot(aes(month, beuti, group = sim)) + 
#   geom_line(alpha = 0.1)

boot_beuti_peak <- boot_beuti %>% 
  group_by(sim) %>% 
  summarize(peak_month_beuti = month[which.max(beuti)])

boot_bw_peak <- boot_bw %>% 
  group_by(sim) %>% 
  summarize(peak_month_bw = month[which.max(count)])

boot_peak_lag <- left_join(boot_beuti_peak, boot_bw_peak, by = "sim") %>% 
  mutate(peak_lag = peak_month_bw - peak_month_beuti, 
         peak_lag = ifelse(peak_lag <= 0, peak_lag + 12, peak_lag)) 

mean_peak_lag <- CircStats::circ.mean(boot_peak_lag$peak_lag * 2 * pi / 12) * 12 / 2 / pi

# Define CI as shortest arc containing 95% of boot samples
shortest_arc_ci <- function(x, T = 12, conf = 0.95) {
  n <- length(x)
  x2 <- x %% T
  x2_sorted <- sort(x2)
  
  n_in <- floor(conf * n)
  
  unrolled <- c(x2_sorted, x2_sorted + T)
  
  arc_widths <- unrolled[(n_in + 1):(n + 1)] - unrolled[1:(n - n_in + 1)]
  best <- which.min(arc_widths)
  
  lwr <- unrolled[best] %% T
  upr <- unrolled[best + n_in] %% T
  
  c(lwr = lwr, upr = upr, width = min(arc_widths))
}
ci_peak_lag <- shortest_arc_ci(boot_peak_lag$peak_lag)

if (mean_peak_lag < 0) mean_peak_lag <- mean_peak_lag + 12
ggplot(boot_peak_lag, aes(peak_lag)) + 
  geom_histogram() + 
  geom_vline(xintercept = mean_peak_lag, color = "red") + 
  theme_classic()
