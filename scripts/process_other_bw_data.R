library(tidyverse)
library(lubridate)

### SCB ###
zc_scb <- rbind(
  read_csv("data/other_sites/SCB/SiteN_Zc_monthly_bin.csv") %>% 
    mutate(site = "N"),
  read_csv("data/other_sites/SCB/SiteH_Zc_monthly_bin.csv") %>% 
    mutate(site = "H")
) %>% 
  mutate(date = dmy(bin),
         month = month(date),
         year = year(date),
         site = factor(site)) %>% 
  arrange(site, date) %>% 
  #already effort adjusted
  drop_na(Zc_pres_eff_adj) %>% 
  group_by(site)

write_csv(zc_scb, "data/processed_bw/zc_scb.csv")

### SRI ###
zc_sri_raw <- read_csv(here::here(c(
  "data/other_sites/SRI/SOCAL_W_01_WW_encounterTimes_Ziphius_cavirostris.csv", 
  "data/other_sites/SRI/SOCAL_W_02_WW_encounterTimes_Ziphius_cavirostris.csv", 
  "data/other_sites/SRI/SOCAL_W_03_WW_encounterTimes_Ziphius_cavirostris.csv",
  "data/other_sites/SRI/SOCAL_W_04_WW_encounterTimes_Ziphius_cavirostris.csv", 
  "data/other_sites/SRI/SOCAL_W_05_WW_encounterTimes_Ziphius_cavirostris.csv")))

zc_sri_raw <- zc_sri_raw %>%
  mutate(startEnc = str_replace(startEnc, "-0022 ", "-2022 "))

#calculating effort from Baggett et al 2025 metadata (table s1). 
#note that i think there is an error in the endtime of deployment 2 - I think it should be May, not march.
sri_deps <- read_csv(here::here("data/other_sites/SRI/sri_effort.csv"))
sri_effort_parsed <- sri_deps %>% 
  mutate(dep_start = mdy(dep_start), 
         dep_end = mdy(dep_end))

sri_effort <- sri_effort_parsed %>% 
  rowwise() %>% #treats each row as its own group
  reframe( #like summarize but allows for multiple output rows
    deploy_id = deploy_id, 
    month_start = floor_date(seq.Date(dep_start, dep_end, by = "month"), "month")
  ) %>% 
  left_join(sri_effort_parsed, by = "deploy_id") %>% 
  mutate(
    month_end    = ceiling_date(month_start, "month") - days(1),
    active_start = pmax(dep_start, month_start),
    active_end   = pmin(dep_end, month_end),
    effort_days  = as.numeric(active_end - active_start) + 1
  ) %>% 
  mutate(year  = year(month_start),
         month = month(month_start), 
         total_days = days_in_month(make_date(year, month, 1)), 
         effort_pct = effort_days / total_days) %>% 
  select(deploy_id, year, month, effort_days, total_days, effort_pct)
  
zc_sri <- zc_sri_raw %>% 
  mutate(mins = as.numeric(encDur) / 60, 
         date = dmy_hms(endEnc), 
         year = year(date), 
         month = month(date)) %>% 
  group_by(year, month) %>% 
  summarize(total_mins = sum(mins), 
            avg_mins = mean(mins)) %>% 
  left_join(sri_effort, by = c("year", "month")) %>% 
  select(year, month, total_mins, effort_pct) %>% 
  mutate(mins_eff_adj = total_mins / effort_pct)

write_csv(zc_sri, "data/processed_bw/zc_sri.csv")

### Oregon ###
or_deps <- tibble(
  deploy_id = c(1, 2),
  dep_start = mdy(c("10-09-2021", "06-08-2022")),
  dep_end   = mdy(c("04-17-2022", "12-15-2022"))
)

or_raw <- read_csv(here::here("data/other_sites/oregon/monthly_presence_zc_oregon.csv"))

or_effort <- or_deps %>% 
  rowwise() %>% #treats each row as its own group
  reframe( #like summarize but allows for multiple output rows
    deploy_id = deploy_id, 
    month_start = floor_date(seq.Date(dep_start, dep_end, by = "month"), "month")
  ) %>% 
  left_join(or_deps, by = "deploy_id") %>% 
  mutate(
    month_end    = ceiling_date(month_start, "month") - days(1),
    active_start = pmax(dep_start, month_start),
    active_end   = pmin(dep_end, month_end),
    effort_days  = as.numeric(active_end - active_start) + 1
  ) %>% 
  mutate(year  = year(month_start),
         month = month(month_start), 
         total_days = days_in_month(make_date(year, month, 1)), 
         effort_pct = effort_days / total_days) %>% 
  select(deploy_id, year, month, effort_days, total_days, effort_pct)

zc_or <- or_raw %>% 
  mutate(date = ymd(month_year), 
         month = month(month_year),
         year = year(month_year)) %>% 
  arrange(date) %>% 
  left_join(or_effort, by = c("year", "month")) %>% 
  select(year, month, days_present, effort_pct) %>% 
  mutate(days_eff_adj = days_present / effort_pct)

write_csv(zc_or, "data/processed_bw/zc_or.csv")
