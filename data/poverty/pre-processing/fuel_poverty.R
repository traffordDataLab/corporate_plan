# Reduce % of households fuel poverty levels #

# Source: Department for Business, Energy & Industrial Strategy
# URL: https://www.gov.uk/government/collections/fuel-poverty-sub-regional-statistics
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(readxl) ; library(httr) 

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

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1072034/fuel-poverty-sub-regional-2022-tables.xlsx",
    write_disk(tmp))

fuel_poverty_2020 <- read_xlsx(tmp, sheet = 6, skip = 2) %>%
  mutate(area_name = coalesce(`Area names`, `...3`, `...4`))%>%
  select(area_code = `Area Codes`, area_name, value = `Proportion of households fuel poor (%)`) %>%
  filter(`area_code` %in% c("E92000001", cipfa$area_code, "E08000009")) %>%
  mutate(area_name = if_else(area_name == "ENGLAND", "England", area_name),
         value = as.numeric(value),
         period = 2020,
         indicator = "Fuel poverty (low income, low energy efficiency methodology)")
  

df <- bind_rows(fuel_poverty_trend, fuel_poverty_2019, fuel_poverty_2020) %>%
  mutate(value = round(value, 1)) %>%
  mutate(measure = "Percentage", unit = "Households") %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

write_csv(df, "../fuel_poverty.csv")
