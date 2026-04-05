library(tidyverse)
library(lubridate)

#read and join multiple csvs
bw_sightings <- function(csv_paths) {
  files <- lapply(csv_paths, function(p) {
    read.csv(p) %>% mutate(Date = dmy_hms(Date))
  })
  reduce(files, full_join, by = "Date") %>% 
    mutate(Date = as.Date(Date)) %>%
    distinct(Date, .keep_all = TRUE) %>%
    mutate(Month = month(Date)) %>%
    group_by(Month) %>%
    mutate(count = n())
}

plot_bw_sightings <- function(bw_data, year, species, sightings_data = NULL) {
  p <- ggplot(bw_data, aes(Month)) +
    geom_bar(color = "white") +
    scale_x_continuous(breaks = 1:12,
                       labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec"),
                       limits = c(0.5, 12.5)) +
    ylim(0, 15) +
    ylab(paste("Days with", species)) +
    xlab(paste0("Month (", year, ")")) +
    theme_bw()
  
  if (!is.null(sightings_data)) {
    p <- p + geom_text(data = sightings_data, aes(x = Month),
                       label = "*", y = 3, size = 9, color = "hotpink")
  }
  p
}

find_peaks <- function(df, min_pct = 0.5) {
  y <- df$fit
  is_peak <- c(FALSE, diff(sign(diff(y))) == -2, FALSE)
  result <- df[is_peak, ] %>%
    filter(fit >= min_pct * max(fit)) %>%
    mutate(month = case_when(
      month > 12 ~ month - 12,
      TRUE       ~ month
    )) %>%
    arrange(desc(fit))
  # Duplicates possible at boundary
  if (all(c(1, 12) %in% result$month))
    result <- filter(result, month != 12)
  result
}

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

