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

#calculating effort from Baggett et al 2025 metadata (table s1). note that i
#think there is an error in the endtime of deployment 2 - I think it should be
#May, not march.
sri_deps <- read_csv(here::here("data/other_sites/SRI/sri_effort.csv"))
sri_effort_parsed <- sri_deps %>% 
  mutate(dep_start = mdy(dep_start), 
         dep_end = mdy(dep_end)) %>% 
  arrange(dep_start)

#because an old deployment ends and a new one begins on the same day, it’s being
#counted twice in the effort table. how do i fix this?
sri_effort <- sri_effort_parsed %>% 
  rowwise() %>% #treats each row as its own group
  reframe( #like summarize but allows for multiple output rows
    deploy_id = deploy_id, 
    effort_day = seq.Date(dep_start, dep_end, by = "day")
  ) %>% 
  mutate(year_month = floor_date(effort_day, unit = "months")) %>% 
  group_by(year_month) %>% 
  summarize(effort_days = n_distinct(effort_day)) %>% 
  mutate(total_days = days_in_month(year_month), 
         effort_pct = effort_days / total_days,
         year = year(year_month),
         month = month(year_month)) %>% 
  select(-year_month) %>% 
  relocate(year, month)
  
zc_sri <- zc_sri_raw %>% 
  mutate(mins = as.numeric(encDur) / 60, 
         date = dmy_hms(endEnc), 
         year = year(date), 
         month = month(date)) %>% 
  group_by(year, month) %>% 
  summarize(total_mins = sum(mins), 
            avg_mins = mean(mins),
            .groups = "drop") %>% 
  left_join(sri_effort, by = c("year", "month")) %>% 
  select(year, month, total_mins, effort_pct) %>% 
  mutate(mins_eff_adj = total_mins / effort_pct,
         month_date = ISOdate(year, month, 1))

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
    effort_day = seq.Date(dep_start, dep_end, by = "day")
  ) %>% 
  mutate(year_month = floor_date(effort_day, unit = "months")) %>% 
  group_by(year_month) %>% 
  summarize(effort_days = n_distinct(effort_day)) %>% 
  mutate(total_days = days_in_month(year_month), 
         effort_pct = effort_days / total_days,
         year = year(year_month),
         month = month(year_month)) %>% 
  select(-year_month) %>% 
  relocate(year, month)

zc_or <- or_raw %>% 
  mutate(date = ymd(month_year), 
         month = month(month_year),
         year = year(month_year)) %>% 
  arrange(date) %>% 
  left_join(or_effort, by = c("year", "month")) %>% 
  select(year, month, days_present, effort_pct) %>% 
  mutate(days_eff_adj = days_present / effort_pct,
         month_date = ISOdate(year, month, 1))

write_csv(zc_or, "data/processed_bw/zc_or.csv")
