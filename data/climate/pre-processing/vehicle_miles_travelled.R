# Vehicle miles travelled on roads.
# Created: 2022-01-07.  Last updated: 2022-10-10

# Source: Department for Transport (DfT)
#         https://www.gov.uk/government/statistical-data-sets/road-traffic-statistics-tra#traffic-by-local-authority-tra89
#         https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1107032/tra8901.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS); library(httr)

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1107032/tra8901.ods",
    write_disk(tmp))

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2021):
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Get the raw data ---------------------------
df_raw <- read_ods(tmp, sheet = 1, col_names = TRUE, col_types = NA, skip = 5)

# Tidy the data ---------------------------
df_vehicle_miles <- df_raw %>%
  # renaming columns via select due to cols 3 and 4 being unnamed
  select(area_code = `LA Code`,
         area_name = 3,
         NOTWANTED = 4, # This is an unnamed notes column which we don't need
         `2010` = `2010R`, # The additional "R" appears to refer to "Revised" as these values have changed from the previous version, however no footnotes are present in the datasheet
         `2011` = `2011R`,
         `2012` = `2012R`,
         `2013` = `2013R`,
         `2014` = `2014R`,
         `2015` = `2015R`,
         `2016` = `2016R`,
         `2017` = `2017R`,
         `2018` = `2018R`,
         `2019` = `2019R`,
         `2020` = `2020R`,
         `2021`) %>%
  select(-NOTWANTED) %>%
  # Filter out rows with area_codes not in the format "E06xxxxxx", "E08xxxxxx", "E09xxxxxx" or "E10xxxxxx" as these are region aggregations
  filter(str_detect(area_code, pattern = "E06|E08|E09|E10[0-9]{6}")) %>%
  # convert to 'tidy' data by transposing the dataset to long format
  pivot_longer(c(-area_code, -area_name), names_to = "period", values_to = "value") %>%
  # Remove rows with no data, signified with ".."
  filter(value != "..") %>%
  mutate(value = as.numeric(value))

# Calculate England averages for each year ---------------------------
df_england_averages <- df_vehicle_miles %>%
  mutate(area_code = "E92000001",
         area_name = "England LA average") %>%
  group_by(period, area_code, area_name) %>%
  summarise(value = round(mean(value), digits = 0)) %>%
  select(area_code, area_name, period, value)

# Get the data for Trafford and CIPFA neighbours and bind it with the England averages ---------------------------
df_vehicle_miles <- df_vehicle_miles %>%
  filter(area_code %in% authorities$area_code) %>%
  bind_rows(df_england_averages) %>%
  mutate(indicator = "Vehicle miles travelled",
         measure = "Frequency",
         unit = "Miles (million)") %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_vehicle_miles, "../vehicle_miles_travelled.csv")

# Cleanup the downloaded ODS
unlink(tmp)
