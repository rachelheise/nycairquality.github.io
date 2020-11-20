Data Cleaning
================

## Setup

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
  filter(observation_type == c("case_count", "case_count_7day_avg"))

#add proof-of-concept covid policy changes to df
nyc_daily_borough_testing = 
  nyc_daily_borough_testing %>% 
  mutate(
    policy_change = case_when(
      date == "2020-03-07" ~ "State of Emergency",
      date == "2020-03-17" ~ "DeBlasio / Cuomo Conflict on Shutdown",
      date == "2020-03-22" ~ "PAUSE Order",
      date == "2020-08-08" ~ "Phase 1 Opening",
      date == "2020-08-17" ~ "Gyms Starting",
      date == "2020-10-01" ~ "Primary Schools"),
    dates_vline = date,
    dates_vline = case_when(
      date == "2020-03-07" ~ "TRUE",
      date == "2020-03-17" ~ "TRUE",
      date == "2020-03-22" ~ "TRUE",
      date == "2020-08-08" ~ "TRUE",
      date == "2020-08-17" ~ "TRUE",
      date == "2020-10-01" ~ "TRUE")
  )
```

``` r
nyc_daily_borough_testing %>% 
  filter(observation_type == c("case_count")) %>% 
  group_by(date, borough, observation_type) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(x = date, y = observed_value, color = borough)) +
  geom_smooth(
    aes(x = date, y = observed_value, color = borough), 
        alpha = 1, se = F) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  coord_cartesian(ylim = c(0,2000)) +
  labs(
    title = "Case Count per NYC Borough over Time",
    x = "Week Number",
    y = "Value",
    caption = "P8105 Final Project") 
```

![](data_cleaning_files/figure-gfm/quickplot%20nycgov%20covid%20case_count-1.png)<!-- -->

``` r
nyc_daily_borough_testing %>% 
  filter(observation_type == c("case_count_7day_avg")) %>% 
  group_by(date, borough, observation_type) %>% 
  ggplot(aes(x = date, y = observed_value, color = borough)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  coord_cartesian(ylim = c(0,2000)) +
  labs(
    title = "7 Day Case Count Average per NYC Borough over Time",
    x = "Week Number",
    y = "Value",
    caption = "P8105 Final Project") 
```

![](data_cleaning_files/figure-gfm/quickplot%20nycgov%20covid%207day_avg%20with%20reference%20lines-1.png)<!-- -->

## Air Quality Data

``` r
air_quality = fread("./data/waqi-covid19-airqualitydata-2020.csv") %>%
  filter(Country == "US") %>%
  filter(City == "Brooklyn" | City == "Queens" | City == "The Bronx" | City == "Staten Island" | City == "Manhattan") %>%
  mutate(borough = City) %>%
  select(-c(City,Country)) %>%
  pivot_wider(names_from = "Specie", values_from = c("count","min","max","median","variance")) %>% 
  janitor::clean_names()

fwrite(air_quality,"./data/Just_NYC_Air_Quality_Data.csv")
```

``` r
air_quality %>% 
  ggplot(aes(x = date, y = median_co, color = borough)) + 
    geom_point(alpha = .2) +
    geom_line(alpha = .75, size = .75) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    title = "Median CO Emmissions over Time for each NYC borough, 2020",
    x = "Month",
    y = "Median CO (Parts per Million)",
    caption = "Examining COVID-19 Incidence, P8105 Final Project") 
```

    ## Warning: Removed 92 rows containing missing values (geom_point).

    ## Warning: Removed 4 row(s) containing missing values (geom_path).

![](data_cleaning_files/figure-gfm/plot_aq-1.png)<!-- -->

``` r
air_quality %>% 
  ggplot(aes(x = date, y = median_pm25, color = borough)) + 
    geom_point(alpha = .2) +
    geom_line(alpha = .75, size = .75) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    title = "Median Particulate Matter (size<2.5 micrometers) Levels over Time for each NYC borough, 2020",
    x = "Month",
    y = "Median Particulate Matter (size < 2.5 micrometeres) Levels (Parts per Millon)",
    caption = "Examining COVID-19 Incidence, P8105 Final Project") 
```

![](data_cleaning_files/figure-gfm/plot_aq-2.png)<!-- -->

``` r
air_quality %>% 
  ggplot(aes(x = date, y = median_temperature, color = borough)) + 
    geom_point(alpha = .2) +
    geom_line(alpha = .75, size = .75) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    title = "Median Temperature over Time for each NYC borough, 2020",
    x = "Month",
    y = "Median Temperature (degrees C)",
    caption = "Examining COVID-19 Incidence, P8105 Final Project") 
```

    ## Warning: Removed 65 rows containing missing values (geom_point).

    ## Warning: Removed 17 row(s) containing missing values (geom_path).

![](data_cleaning_files/figure-gfm/plot_aq-3.png)<!-- -->

``` r
AQI_temp = air_quality %>%
  select(date,borough,median_pm25,median_o3,median_co,median_no2)

AQI_formula_O3 = function(AQI){
  if (AQI <= 54 && AQI >= 0 ){
    AQI_O3 = (50/54)*(AQI-0) + 0
  }
}

AQI_formula_pm25 = function(AQI){
  if (AQI<= 12&& AQI>=0 ){
    AQI_pm25 = (50/12)*(AQI-0) + 0
  }
  
  else if (AQI<=35.4 && AQI>=12.1 ){
    AQI_pm25 = (50/23.3)*(AQI-12.1) + 51
  }
  
  else if (AQI<=55.4 && AQI>=35.5 ){
    AQI_pm25 = (50/19.9)*(AQI-35.5) + 101
  }
  
  else{
    AQI_pm25 = (50/94.9)*(AQI-55.5) + 151
  }
}

AQI_formula_co = function(AQI){
  if (AQI<= 4.4&& AQI>=0 ){
    AQI_co = (50/4.4)*(AQI-0) + 0
  }
  
  else if (AQI<=9.4 && AQI>=4.5 ){
    AQI_co = (50/4.9)*(AQI-4.5) + 51
  }
  
  else{
    AQI_co = (50/2.9)*(AQI-12.4) + 101
  }
}

AQI_formula_no2 = function(AQI){
  if (AQI<= 53&& AQI>=0 ){
    AQI_no2 = (50/53)*(AQI-0) + 0
  }
  
  else if (AQI<=100 && AQI>=54 ){
    AQI_no2 = (50/46)*(AQI-54) + 51
  }
  
  else{
    AQI_no2 = (50/259)*(AQI-101) + 101
  }
}

AQI_O3 = AQI_temp%>%
  select(median_o3)%>%
  purrr::map(~AQI_formula_O3(.x))%>%
  data.frame()

AQI_pm25 = AQI_temp%>%
  select(median_pm25)%>%
  purrr::map(~AQI_formula_pm25(.x))%>%
  data.frame()

AQI_co = AQI_temp%>%
  select(median_co)%>%
  purrr::map(~AQI_formula_co(.x))%>%
  data.frame()

AQI_no2 = AQI_temp%>%
  select(median_no2)%>%
  purrr::map(~AQI_formula_no2(.x))%>%
  data.frame()

AQI_combined = cbind(AQI_O3,AQI_co,AQI_no2,AQI_pm25)%>%
  rowwise() %>% 
  mutate(AQI = max(median_o3,median_co,median_no2,median_pm25))%>%
  cbind(air_quality%>%pull(date),
        air_quality%>%pull(borough))
colnames(AQI_combined) = c("AQI_o3","AQI_co","AQI_no2","AQI_pm25","AQI_Final","date","borough")

AQI_combined = AQI_combined%>%
  select(c("date","borough","AQI_Final","AQI_o3","AQI_co","AQI_no2","AQI_pm25"))%>%
  mutate(AQI_Category = if_else(
    AQI_Final<=50 & AQI_Final>=0,"Good",
    if_else(AQI_Final<=100&AQI_Final>50,"Moderate",
    if_else(AQI_Final<=150&AQI_Final>100,"Unhealthy for Sensitive Populations","Unhealthy"))))


fwrite(AQI_combined,"./data/Air_Quality_Data_with_AQI.csv")

AQI_combined %>% 
  group_by(borough) %>% 
  ggplot(aes(x = date, y = AQI_Final)) +
  geom_line() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 336 rows containing non-finite values (stat_smooth).

![](data_cleaning_files/figure-gfm/Air_Quality_Index-1.png)<!-- -->
