# Child obesity in 4-5 year olds #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) 

CIPFA <- read_csv("../../cipfa.csv")

obese_reception_quintiles <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=92026&area_type_id=101") %>%
  filter(`Area Name` == "Trafford", Sex == "Persons", `Time period` == "2015/16 - 19/20", `Category Type` == "LSOA11 deprivation quintiles in England (IMD2019)") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category) %>%
  mutate(measure = "Percentage")

obese_reception_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=90319&area_type_id=101") %>%
select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`)
  
  
obese_reception_england <- obese_reception_trend %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`)) %>%
  mutate(measure = "Percentage")

obese_reception_districsts <- obese_reception_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  select(-c(`Category Type`, `Area Type`)) %>%
  mutate(measure = "Percentage")

df <- bind_rows(obese_reception_quintiles, obese_reception_england, obese_reception_districsts) %>%
  unique()

write_csv(df, "../obese_reception.csv")
