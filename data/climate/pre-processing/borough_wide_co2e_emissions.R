# Borough wide CO2e emissions (Full set data)
# Created: 2022-01-27

# Source: Department for Business, Energy & Industrial Strategy
#         https://www.gov.uk/government/statistics/uk-local-authority-and-regional-carbon-dioxide-emissions-national-statistics-2005-to-2019#full-publication-update-history

# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readODS)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/996058/2005-19_UK_local_and_regional_CO2_emissions.ods",
    write_disk(tmp))

# Extract the raw data ---------------------------
df_co2e_raw <- read_ods(tmp, sheet = 2, skip = 1)

# Tidy the data ---------------------------
df_co2e <- df_co2e_raw %>%
  rename(area_code = Code,
         area_name = "Local Authority",
         period = Year,
         value = "Grand Total") %>%
  mutate(indicator = "Local Authority territorial CO2 emissions estimates",
         measure = "Frequency",
         unit = "kilotonnes") %>%
  filter(area_code %in% authorities$area_code, period >= 2010) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_co2e, "../borough_wide_co2_emissions.csv")    
