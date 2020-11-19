Data Cleaning
================

# Setup

## COVID Case Data

Reading in the data and basic cleaning.

``` r
nyc_cases =
  GET("https://data.cityofnewyork.us/resource/rc75-m7u3.csv") %>% # Reading in the data
  content("parsed") %>% 
  rename(date = date_of_interest) %>% # Renaming the date variable for simplicity
  select(date, case_count) %>% # Only retaining date and case count for simplicity
  mutate(
    date = as.Date(date, tryFormats = c("%Y-%m-%d")) # as.Date() by default cannot convert POSIXct to date; no data lost
  )
## Note: This dataset also contains hospitalized count and death count.
## I removed those measures for now for simplicity, but we can always decide to retain them later if we think they would be useful.
```

Trying out a simple plot of cases over time.

``` r
nyc_cases %>% 
  ggplot(aes(x = date, y = case_count, color = case_count)) + 
    geom_point(alpha = .2) +
    geom_line(alpha = .75, size = .75) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
      scale_y_continuous(breaks = seq(0, 6500, 500)) +
  labs(
    title = "COVID-19 New Cases Per Day, NYC, 2020",
    x = "Month",
    y = "Case Count (Persons)",
    caption = "Examining COVID-19 Incidence, P8105 Final Project") 
```

![](data_cleaning_files/figure-gfm/clean_cases-1.png)<!-- -->

``` r
nyc_daily_borough_testing =
  read_csv(
      "./data/covid_data/nyc_daily_covid.csv") %>% 
  janitor::clean_names() %>% 
  rename(date = date_of_interest) %>% 
  mutate(
    date = as.Date(date, format = "%m/%d/%Y")
  ) %>%
  pivot_longer(
    bx_case_count:si_death_count_7day_avg,
    names_to = "borough_variable",
    values_to = "observed_value"
  ) %>% 
  mutate(
    borough_variable = str_replace(borough_variable, "bx_", "Bronx/"),
    borough_variable = str_replace(borough_variable, "bk_", "Brooklyn/"),
    borough_variable = str_replace(borough_variable, "qn_", "Queens/"),
    borough_variable = str_replace(borough_variable, "si_", "Staten Island/"),
    borough_variable = str_replace(borough_variable, "mn_", "Manhattan/")
  ) %>% 
  separate(borough_variable, into = c("borough", "observation_type"), sep = "/") %>% 
  arrange(date, borough, observation_type, observed_value) %>% 
  pivot_longer(
    case_count:incomplete,
    names_to = "total_observation_type",
    values_to = "total_observed_value"
  ) %>% 
  filter(observation_type == c("case_count", "death_count"))
```

``` r
nyc_daily_borough_testing %>% 
  group_by(date, borough, observation_type) %>% 
  ggplot(aes(x = date, color = observation_type)) + 
  geom_line(aes(x = date, y = observed_value)) +
  geom_smooth(
    aes(x = date, y = observed_value), 
        alpha = 1, inherit.aes = F, se = F) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    title = "Observed Values over Time, per Participant",
    x = "Week Number",
    y = "Value",
    caption = "Testing 1 2 3") 
```

![](data_cleaning_files/figure-gfm/quickplot%20nycgov%20covid%20dataframe%20import%20and%20cleaning-1.png)<!-- -->

``` r
### WORK IN PROGRESS
```

## Air Quality Data
