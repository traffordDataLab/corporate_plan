# Under 75 mortality rate from causes considered preventable #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/mortality-profile
# Licence: Open Government Licence v3.0

library(tidyverse) 

#From fingertips "The official population estimates for mid 2012 to mid 2020 will be revised, to incorporate the data now available from Census 2021. As such, this indicator is currently only presenting data for 2021. Once revised populations for mid 2012 to mid 2020 are published, the updated back series for this indicator will be published."

#"Data from 2012 to 2021 are based on unrevised population estimates and therefore should not be used to make comparisons with the published 2021 data."

mortality_rate_trend_pre2021 <- read_csv("https://fingertips.phe.org.uk/documents/indicator-93721-all-areas.data.csv") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`) %>%
  mutate(period = as.integer(period))

mortality_rate_trend_2021 <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93721&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`)

mortality_rate_trend <- mortality_rate_trend_pre2021 %>%
  bind_rows(mortality_rate_trend_2021)


mortality_rate_england <- mortality_rate_trend %>%
  filter(`area_code` == "E92000001") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`)) %>%
  mutate(measure = "Age-standardised rate",
         area_type = "Country")

mortality_rate_districsts <- mortality_rate_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Age-standardised rate")

df <- bind_rows(mortality_rate_england, mortality_rate_districsts) %>%
  filter(period %in% c("2010":"2021")) %>%
  mutate(value = round(value, 1),
         indicator = "Under 75 mortality rate from causes considered preventable (per 100,000 population)") %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../mortality_rate.csv")
