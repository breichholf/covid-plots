library(tidyverse)
library(lubridate)
library(data.table)
library(gghighlight)
library(cowplot)

data_url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'
wide_countries <-
  read_csv(data_url) %>%
  rename(province_state = `Province/State`,
         country_region = `Country/Region`,
         lat = Lat,
         long = Long)

long_countries <-
  wide_countries %>%
  select(-lat, -long) %>%
  pivot_longer(c(-province_state, -country_region), names_to = "date",
               values_to = "cases") %>%
  mutate(date = mdy(date))

selected_countries <- c(
  'Austria', 'Germany', 'United Kingdom', 'Italy',
  'Korea, South', 'Switzerland', 'France', 'Denmark',
  'US'
)

tallied_cases <-
  long_countries %>%
  filter(country_region %in% selected_countries) %>%
  group_by(country_region, date) %>%
  tally(wt=cases) %>%
  mutate(days_to_today = date - max(date))

max_cases <- filter(tallied_cases, date == max(date))

max_country <- filter(max_cases %>% ungroup(), n == max(n))$country_region

italy_exp_date = as.Date("2020-02-22")

max_country_fit <- lm(log10(n) ~ days_to_today,
                      data = tallied_cases %>%
                        filter(country_region == max_country &
                               date > italy_exp_date))

cases_d0 <- coef(max_country_fit)[1]
fit_slope <- coef(max_country_fit)[2]

date_diffed_data <-
  tallied_cases %>%
  filter(date > max(date) - 15) %>%
  mutate(day_coeff = mean(log10(n) - fit_slope * days_to_today)) %>%
  select(country_region, day_coeff) %>%
  unique()

processed_data <-
  tallied_cases %>%
  filter(n > 0) %>%
  left_join(date_diffed_data) %>%
  mutate(date_diff = round(as.numeric(-(cases_d0 - day_coeff)/fit_slope), 0),
         date_lag = date + date_diff,
         diff_today_lag = as.numeric(date_lag - max(date)))

processed_data %>% filter(diff_today_lag >= -21) %>%
  mutate(date_diff = if_else(date != max(date), as.numeric(NA), date_diff)) %>%
  ggplot(aes(x = diff_today_lag, y = n)) +
  geom_abline(intercept = cases_d0, slope = fit_slope, linetype = 2) +
  geom_point(aes(color=country_region), size=1.5) +
  geom_line(aes(color=country_region), size=0.75) +
  xlab(paste0("Lag in days behind ", max_country, " (",
             filter(ungroup(max_cases), n == max(n))$n,
             " cases on ",
             format(filter(ungroup(max_cases), n == max(n))$date, format = "%d-%m-%Y"),
             ")")) +
  ylab("Confirmed SARS-CoV-2 Cases") +
  scale_y_log10(limits = c(1, NA)) +
  scale_x_continuous(breaks = seq(-1000, 0, 5)) +
  gghighlight(aes(country_region),
              use_direct_label = TRUE,
              label_key = date_diff,
              label_params = list(point.padding = 1, nudge_y = -0.9,
                                  nudge_x = 1.2, size = 5.5),
              unhighlighted_params = list(color=grey(0.65), alpha = 0.35)) +
  facet_wrap(~country_region) +
  # annotation_logticks(base = 10, sides = "l") +
  theme_cowplot()
  