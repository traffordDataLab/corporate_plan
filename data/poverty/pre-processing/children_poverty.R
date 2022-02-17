# Children Poverty #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/wider-determinants
# Licence: Open Government Licence v3.0

library(tidyverse) 

cssn <- read_csv("../../cssn.csv") %>%
  select(area_code)

children_absolute_low_income <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93701&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, `Category Type`, `Area Type`, Age)


children_absolute_low_income_england <- children_absolute_low_income  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

children_absolute_low_income_cssn <- children_absolute_low_income %>%
  filter(area_code %in% c(cssn$area_code, "E08000009")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Percentage")

df1 <- bind_rows(children_absolute_low_income_england, children_absolute_low_income_cssn) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  mutate(unit = "Persons") %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value)

children_relative_low_income <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93700&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, `Category Type`, `Area Type`, Age)


children_relative_low_income_england <- children_relative_low_income  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

children_relative_low_income_cssn <- children_relative_low_income %>%
  filter(area_code %in% c(cssn$area_code, "E08000009")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Percentage")

df2 <- bind_rows(children_relative_low_income_england, children_relative_low_income_cssn) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  mutate(unit = "Persons") %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value)

write_csv(bind_rows(df1,df2), "../children_poverty.csv")

