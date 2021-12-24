# Household Waste Recycling
# Created: 2021-12-23

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistical-data-sets/env18-local-authority-collected-waste-annual-results-tables


# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readODS)

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1040529/LA_and_Regional_Spreadsheet_2021.ods",
    write_disk(tmp))

df_raw <- read_ods(tmp, sheet = 8, skip = 3)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Data for England ---------------------------
england <- df_raw %>%
  filter(Region == "Total England") %>%
  mutate(area_code = "E92000001",
         area_name = "England") %>%
  select(area_code, area_name,
         period = Year,
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`)

# Data for Trafford and similar neighbours ---------------------------
df_household_recycling <- df_raw %>%
  filter(`ONS code` %in% authorities$area_code) %>%
  rename(area_code = `ONS code`) %>%
  left_join(authorities) %>%
  select(area_code,
         area_name,
         period = Year,
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`) %>%
  # Join with the data for England
  bind_rows(england) %>%
  mutate(indicator = "Reuse, recycling or composting of household waste",
         measure = "Percentage",
         unit = "Waste") %>%
  select(area_code, area_name, period, value, indicator, unit, measure) %>%
  arrange(period)

# Export the tidied data ---------------------------
write_csv(df_household_recycling, "../household_waste_recycling.csv")
