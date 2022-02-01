# Vehicle miles travelled on roads.
# Created: 2022-01-07

# Source: Department for Transport (DfT)
#         https://www.gov.uk/government/statistical-data-sets/road-traffic-statistics-tra#traffic-by-local-authority-tra89
#         https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/982024/tra8901.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS);

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/982024/tra8901.ods",
    write_disk(tmp))

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Get the raw data ---------------------------
df_raw <- read_ods(tmp, sheet = 1, col_names = TRUE, col_types = NA, skip = 5)

# Tidy the data ---------------------------
df_vehicle_miles <- df_raw %>%
  # renaming columns via select due to cols 3 and 4 being unnamed
  select(area_code = `LA Code`,
         area_name = 3,
         NOTWANTED = 4, # This is an unnamed notes column which we don't need
         `2010` = `20103`, # The additional "3" relates to a footnote
         `2011` = `20113`,
         `2012` = `20123`,
         `2013` = `20133`,
         `2014` = `20143`,
         `2015` = `20153`,
         `2016` = `20163`,
         `2017` = `20173`,
         `2018` = `20183`,
         `2019`,
         `2020`) %>%
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
  select(area_code,area_name,period,value,indicator,unit,measure)

# Export the tidied data ---------------------------
write_csv(df_vehicle_miles, "../vehicle_miles_travelled.csv")
