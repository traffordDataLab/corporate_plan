# Electric vehicle charging points.
# Created: 2022-01-10

# Source: Department for Transport (DfT) and Office for Zero Emission Vehicles (OZEV)
#         https://www.gov.uk/government/statistics/electric-vehicle-charging-device-statistics-april-2022
#         https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1069013/electric-vehicle-charging-device-statistics-april-2022.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS);

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1069013/electric-vehicle-charging-device-statistics-april-2022.ods",
    write_disk(tmp))

# Setup objects ---------------------------
# Trafford, its CIPFA nearest neighbours (2019) and England:
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Get the raw data ---------------------------
df_raw <- read_ods(tmp, sheet = 2, col_names = TRUE, col_types = NA, skip = 6)

# Tidy the data ---------------------------
df_charging_points_rate <- df_raw %>%
  # Due to merged cells the source spreadsheet, the headings are misaligned/missing. Rename them and select the ones we want all within the select().
  # NOTE: due to the misalignment of the headings the renamed columns look like a mistake but they are correct!
  select(area_code = `LA / Region Code`,
         area_name = `Local Authority / Region Name`,
         `2022-04` = `Jan-22`,
         `2022-01` = `Jul-21`,
         `2021-10` = `Jan-21`,
         `2021-07` = `Jul-20`,
         `2021-04` = `Jan-20`,
         `2021-01` = 14,
         `2020-10` = 16,
         `2020-07` = 18,
         `2020-04` = 20,
         `2020-01` = 22,
         `2019-10` = 24
  ) %>%
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
