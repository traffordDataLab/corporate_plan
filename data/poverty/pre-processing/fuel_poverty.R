# Reduce % of households fuel poverty levels #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/wider-determinants
# Licence: Open Government Licence v3.0

library(tidyverse) 

cipfa <- read_csv("../../cipfa2019.csv") %>%
  select(area_code) 

fuel_poverty_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=90356&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, `Category Type`) %>%
  filter(`area_code` %in% c("E92000001", cipfa$area_code, "E08000009")) %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`))

fuel_poverty_2019 <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93759&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, `Category Type`) %>%
  filter(`area_code` %in% c("E92000001", cipfa$area_code, "E08000009")) %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`))

df <- bind_rows(fuel_poverty_trend, fuel_poverty_2019) %>%
  mutate(value = round(value, 1)) %>%
  mutate(measure = "Percentage", unit = "Households") %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

write_csv(df, "../fuel_poverty.csv")
