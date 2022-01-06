# Household Waste Recycling
# Created: 2021-12-23

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistical-data-sets/env18-local-authority-collected-waste-annual-results-tables


# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readODS)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1040529/LA_and_Regional_Spreadsheet_2021.ods",
    write_disk(tmp))

# Extract the raw data ---------------------------
df_raw_recycling <- read_ods(tmp, sheet = 8, skip = 3)
df_raw_not_recycled <- read_ods(tmp, sheet = 4, skip = 3)


# Household waste collected for recycling ---------------------------

# Data for England
england_recycling <- df_raw_recycling %>%
  filter(Region == "Total England") %>%
  mutate(area_code = "E92000001",
         area_name = "England") %>%
  select(area_code, area_name,
         period = Year,
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`)

# Data for Trafford and similar neighbours
df_household_waste_recycled <- df_raw_recycling %>%
  filter(`ONS code` %in% authorities$area_code) %>%
  rename(area_code = `ONS code`) %>%
  left_join(authorities) %>%
  select(area_code,
         area_name,
         period = Year,
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`) %>%
  # Join with the data for England
  bind_rows(england_recycling) %>%
  mutate(indicator = "Reuse, recycling or composting of household waste",
         measure = "Percentage",
         unit = "Waste",
         period = str_replace(period, "-", "/"),
         value = round((value * 100), digits = 1)) %>%
  select(area_code, area_name, period, value, indicator, unit, measure) %>%
  arrange(period)

# Export the tidied data
write_csv(df_household_waste_recycled, "../household_waste_recycling.csv")


# Household waste not sent for recycling (NOTE: not including England as the values can't be compared to individual LAs ---------------------------

# Data for Trafford and similar neighbours
df_household_waste_not_recycled <- df_raw_not_recycled %>%
  filter(`ONS Code` %in% authorities$area_code) %>%
  rename(area_code = `ONS Code`) %>%
  left_join(authorities) %>%
  select(area_code,
         area_name,
         period = `Financial Year`,
         value = `Household - waste not sent for recycling (tonnes)`) %>%
  mutate(indicator = "Household waste not sent for recycling",
         measure = "Tonnes",
         unit = "Waste",
         period = str_replace(period, "-", "/")) %>%
  select(area_code, area_name, period, value, indicator, unit, measure) %>%
  arrange(period)

# Export the tidied data
write_csv(df_household_waste_not_recycled, "../household_waste_not_recycled.csv")
