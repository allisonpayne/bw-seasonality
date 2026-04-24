bb_mb <- read_csv(here::here("data/processed_bw/bb_monterey.csv")) %>% 
  mutate(date = ymd(Date), 
         month = Month,
         year = year(Date)) %>% 
  arrange(date) %>% 
  #this is okay because I know we had full effort in 2017-18, but will need to change if adding data from years where we had off effort months
  complete(year = 2016:2018, month = 1:12, fill = list(count = 0)) %>% 
  distinct(year, month, count) %>% 
  mutate(date = ISOdate(year, month, day = 1))

bb_raw_plot <- ggplot(bb_mb, aes(month, count)) +
  geom_area(aes(fill = factor(year))) +
  scale_fill_manual(values = c("lavender", "violet", "darkmagenta", "purple")) + 
  labs(x = "Month", 
       y = "Bb (days)") +
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA))

#GAMS

bb_mb_mod <- gam(count ~ 
                   s(month, bs = "cc", k = 12) +
                   s(year, bs = "re"),
                 family = tw(link = "log"),
                 data = bb_mb,
                 method = "fREML")

summary(bb_mb_mod)
gam.check(bb_mb_mod)
acf(residuals(bb_mb_mod))

bb_mb_grid <- expand.grid(
  month = seq(0, 13, by = 0.1),   # extend beyond 1-12
  year  = 0)

pred <- predict(bb_mb_mod,
                newdata = bb_mb_grid,
                exclude = "s(year)",
                newdata.guaranteed = TRUE,
                type = "link",
                se.fit = TRUE)

bb_mb_grid <- bb_mb_grid %>%
  mutate(
    fit = exp(pred$fit),
    lwr = exp(pred$fit - 1.96 * pred$se.fit),
    upr = exp(pred$fit + 1.96 * pred$se.fit)
  ) %>% 
  mutate(across(fit:upr, \(x) x / pracma::trapz(month, fit), .names = "{.col}_norm"))

bb_mb_peaks <-  find_peaks(bb_mb_grid)
bb_mb_peaks$fit_norm <- bb_mb_peaks$fit / pracma::trapz(bb_mb_grid$month, bb_mb_grid$fit)

# Clip grid back to 1-12 for plotting
bb_mb_norm_plot <- ggplot(bb_mb_grid %>% filter(month >= 1, month <= 12), 
                          aes(x = month, y = fit_norm)) +
  geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = bb_mb_peaks, aes(x = month, y = fit_norm),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw() +
  labs(x = NULL, 
       y = "Bb")


### Bb curve sim ###

Xp_bb <- predict(bb_mb_mod, 
                 newdata = bb_mb_grid, 
                 type = "lpmatrix")
coef_sims_bb <- MASS::mvrnorm(n_sim, 
                              mu = coef(bb_mb_mod), 
                              Sigma = vcov(bb_mb_mod))
sim_curves_bb <- Xp_bb %*% t(coef_sims_bb)
sim_curves_response_bb <- bb_mb_mod$family$linkinv(sim_curves_bb)
boot_bb <- expand_grid(i = 1:nrow(bb_mb_grid), 
                       j = 1:ncol(sim_curves_response_bb)) %>% 
  mutate(month = bb_mb_grid$month[i], 
         sim = j, 
         count = sim_curves_response_bb[cbind(i, j)]) %>% 
  select(-c(i, j)) 

#bb lags

boot_bb_peak <- boot_bb %>% 
  group_by(sim) %>% 
  summarize(peak_month_bb = month[which.max(count)])

boot_peak_lag_bb <- left_join(boot_beuti_peak, boot_bb_peak, by = "sim") %>% 
  mutate(peak_lag = peak_month_bb - peak_month_beuti, 
         peak_lag = ifelse(peak_lag <= 0, peak_lag + 12, peak_lag)) 

mean_peak_lag_bb <- CircStats::circ.mean(boot_peak_lag_bb$peak_lag * 2 * pi / 12) * 12 / 2 / pi
ci_peak_lag_bb <- shortest_arc_ci(boot_peak_lag_bb$peak_lag)

if (mean_peak_lag_bb < 0) mean_peak_lag_bb <- mean_peak_lag_bb + 12

ggplot(boot_peak_lag_bb, aes(peak_lag)) + 
  geom_histogram() + 
  geom_vline(xintercept = mean_peak_lag_bb, color = "red") + 
  ggtitle("Bb") + 
  theme_classic()