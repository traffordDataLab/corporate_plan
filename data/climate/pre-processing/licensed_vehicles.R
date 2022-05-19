# Licensed Vehicles.
# Created: 2021-11-29, last update: 2022-01-13

# Source: Department for Transport (DfT) & Driver and Vehicle Licensing Authority (DVLA)
#         https://www.gov.uk/government/statistical-data-sets/all-vehicles-veh01
# All Vehicles by body type:       https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/985605/veh0105.ods
# All Ultra Low Emission Vehicles: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1046001/veh0132.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS); library(lubridate)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the data for all vehicles ---------------------------
# These data are reported as correct at the end of each year.
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/985605/veh0105.ods",
    write_disk(tmp))

# The data is arranged within the workbook with a separate tab for each year, so we first need to load it into a single data frame
df_raw <- tmp %>%
  list_ods_sheets() %>%
  set_names() %>% # instead of using the sheet ordinal, this uses the name within the map function
  map_df(~ read_ods(tmp, sheet = .x,
                    col_names = TRUE, col_types = NA, skip = 7), .id = "year")

# Tidy the data. NOTE: the headings for the area code and area name are different for years pre-2020
df_all_vehicles <- df_raw %>%
  mutate(area_code = if_else(is.na(`ONS LA Code`), `ONS LA Code (Apr-2019)`, `ONS LA Code`),
         area_name = if_else(is.na(`Region/Local Authority`), `Region/Local Authority (Apr-2019)3`, `Region/Local Authority`),
         area_name = str_remove(area_name, " UA"), # remove the suffix UA from Unitary Authorities to just leave the LA name
         period = year,
         value_all_vehicles = as.integer(as.numeric(`Total`)*1000)
  ) %>%
  filter(year >= 2011, # The ULEV data that we'll be getting later begins at 2011, so we need to get the same period range here
         area_code %in% authorities$area_code) %>%
  select(area_code,area_name,period,value_all_vehicles) %>%
  # Sort data into "blocks" of all time periods for each area
  arrange(desc(area_code), desc(period))


# Download the data for ULEV vehicles ---------------------------
# These data are reported as correct at the end of each calendar quarter, therefore Q4 is the same period as the data in df_all_vehicles
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1046001/veh0132.ods",
    write_disk(tmp))

# Load in the raw data
df_raw <- read_ods(tmp, sheet = 1, skip = 6)

df_ulev <- df_raw %>%
  rename(area_code = `ONS LA Code (Apr-2019)`,
         area_name = `Region/Local Authority (Apr-2019)3`) %>%
  
  # convert to 'tidy' data by transposing the dataset to long format
  pivot_longer(c(-area_code, -area_name), names_to = "period", values_to = "value_ulev") %>%
  
  # convert the reporting period into a separate year and quarter, then just keep the years corresponding to Q4
  # the notes say that the vehicle counts are the end of each stated quarter
  separate(period, into = c("period", "quarter"), sep = " ") %>%
  filter(period >= 2011,
         area_code %in% authorities$area_code,
         quarter == "Q4") %>%
  mutate(value_ulev = as.integer(na_if(value_ulev, "c"))) %>%
  
  # get the dataset into the same format as df_all_vehicles ready to join
  select(area_code,area_name,period,value_ulev) %>%
  arrange(desc(area_code), desc(period))

# Now combine the ulev and all vehicles datasets
df_licensed_vehicles <- df_ulev %>%
  left_join(df_all_vehicles, by = c("area_code", "period")) %>%
  mutate(indicator = "Licensed vehicles - ultra low emission vehicles (ulev) and all vehicles including ulev",
         measure = "Frequency",
         unit = "Vehicles") %>%
  arrange(period, area_name.x) %>%
  select(area_code,
         area_name = area_name.x,
         period,
         indicator,
         measure,
         unit,
         value_ulev,
         value_all_vehicles)

write_csv(df_licensed_vehicles, "../licensed_vehicles.csv")
