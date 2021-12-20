# Licensed Vehicles.
# Created: 2021-11-29

# Source: Department for Transport (DfT) & Driver and Vehicle Licensing Authority (DVLA)
#         https://www.gov.uk/government/statistical-data-sets/all-vehicles-veh01
# All Vehicles by body type:      https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/985605/veh0105.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS); library(lubridate)

# Download the data ---------------------------
download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/985605/veh0105.ods", "veh0105.ods")

# All vehicles ---------------------------
# These data are reported at the end of each year, so we'll align to the Q4 data in the other datasets

# The data is arranged within the workbook with a separate tab for each year, so we first need to load it into a single data frame
df_raw <- "veh0105.ods" %>%
  list_ods_sheets() %>%
  set_names() %>% # instead of using the sheet ordinal, this uses the name within the map function
  map_df(~ read_ods(path = "veh0105.ods", sheet = .x,
                    col_names = TRUE, col_types = NA, skip = 7), .id = "year")

# Tidy the data. NOTE: the headings for the area code and area name are different for years pre-2020
df_all_vehicles <- df_raw %>%
  mutate(area_code = if_else(is.na(`ONS LA Code`), `ONS LA Code (Apr-2019)`, `ONS LA Code`),
         area_name = if_else(is.na(`Region/Local Authority`), `Region/Local Authority (Apr-2019)3`, `Region/Local Authority`),
         area_name = str_remove(area_name, " UA"), # remove the suffix UA from Unitary Authorities to just leave the LA name
         indicator = "Licensed vehicles",
         period = ymd(paste0(as.integer(year), "-12-31")),
         measure = "Frequency",
         unit = "Vehicles",
         value = as.integer(as.numeric(`Total`)*1000)
  ) %>%
  filter(year >= 2010) %>%
  select(area_code,area_name,period,value,indicator,unit,measure)

# Export the tidied data ---------------------------
write_csv(df_all_vehicles, "../licensed_vehicles.csv")

# Clean up ---------------------------
if (file.exists("veh0105.ods")) file.remove("veh0105.ods")
