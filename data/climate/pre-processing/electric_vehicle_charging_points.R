# Electric vehicle charging points.
# Created: 2022-01-10  Updated: 2023-09-22  Data: 2023-07-26

# Source: Department for Transport (DfT) and Office for Zero Emission Vehicles (OZEV)
#         https://www.gov.uk/government/collections/electric-vehicle-charging-infrastructure-statistics
#         https://www.gov.uk/government/statistics/electric-vehicle-charging-device-statistics-july-2023
#         https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1173344/electric-vehicle-charging-device-statistics-july-2023.ods


# Load required packages ---------------------------
library(tidyverse); library(tidyselect); library(readODS); library(httr)

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1173344/electric-vehicle-charging-device-statistics-july-2023.ods",
    write_disk(tmp))

# Setup objects ---------------------------
# Trafford, its CIPFA nearest neighbours (2019) and England:
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Get the raw data ---------------------------
df_raw <- read_ods(tmp, sheet = "1a", col_names = TRUE, col_types = NA, skip = 2)

# Tidy the data ---------------------------
df_charging_points_rate <- df_raw %>%
  # Due to merged cells the source spreadsheet, the headings are misaligned/missing. Rename them and select the ones we want all within the select().
  # NOTE: due to the misalignment of the headings it's better to reference the column number
  select(area_code = `Local.Authority...Region.Code`,
         area_name = `Local.Authority...Region.Name`,
         contains('population') # we only want the data which is per 100K population
  ) %>%
  # We need to rename the first 12 columns of data to the correct date format we need for the chart
  rename(
    `2023-07` = 3,
    `2023-04` = 4,
    `2023-01` = 5,
    `2022-10` = 6,
    `2022-07` = 7,
    `2022-04` = 8,
    `2022-01` = 9,
    `2021-10` = 10,
    `2021-07` = 11,
    `2021-04` = 12,
    `2021-01` = 13,
    `2020-10` = 14
  ) %>%
  select(1:14) %>% # Now just select the first 14 columns of data
  filter(area_code %in% authorities$area_code) %>%
  mutate(area_name = if_else(area_name == "ENGLAND", "England", area_name)) %>%
  # convert to 'tidy' data by transposing the dataset to long format
  pivot_longer(c(-area_code, -area_name), names_to = "period", values_to = "value") %>%
  mutate(value = round(as.numeric(value), 1),
         indicator = "Publicly available electric vehicle charging devices at all speeds",
         measure = "Per 100,000 population",
         unit = "Devices") %>%
  arrange(area_name, period) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_charging_points_rate, "../electric_vehicle_charging_points.csv")
