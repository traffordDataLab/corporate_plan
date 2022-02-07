# Proportion of adults who do any walking or cycling, for any purpose, five times per week
# Created: 2022-02-07

# Source: Department for Transport (DfT)
#         https://www.gov.uk/government/statistical-data-sets/walking-and-cycling-statistics-cw#participation-in-walking-and-cycling
#         CW0301: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1019282/cw0301.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS); library(httr);

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1019282/cw0301.ods",
    write_disk(tmp))

# The sheets in the workbook are arranged alternately in pairs: year_x_percentages, year_x_confidence_interval
# We're only interested in the sheets containing the percentages, not those with "CI" in the sheet name
# Also the column names are not consistent within the sheets, meaning that we can't use them as variable names
# We therefore need col_names = FALSE and we'll rename them later
df_raw <- list_ods_sheets(tmp)[!str_detect(list_ods_sheets(tmp), "CI")] %>%
  set_names() %>% # instead of using the sheet ordinal, this uses the name within the map function
  map_df(~ read_ods(tmp, sheet = .x,
                    col_names = FALSE, col_types = NA, skip = 8), .id = "src_sht")

# Tidy the data ---------------------------
df_wlk_cyc <- df_raw %>%
  # Start by selecting and renaming the columns we are interested in
  select(period = src_sht,
         area_code = A,
         area_name = B,
         value = I) %>% # This is the Five times per week column
  filter(area_code %in% authorities$area_code) %>%
  # We need to change the format of period from CW0301_YYYY into 20YY-YY using a regex
  # Replace "CW0301_" with "20" then get the next 2 digits in one group and the last 2 in another and put a hyphen between them
  mutate(period = str_replace(period, "CW0301_([0-9]{2})([0-9]{2})", "20\\1-\\2"),
         area_name = if_else(area_name == "ENGLAND", "England", area_name),
         value = round(as.numeric(value), 1),
         indicator = "Proportion of adults who do any walking or cycling, for any purpose, five times per week",
         measure = "Percentage",
         unit = "Persons") %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_wlk_cyc, "../adults_walking_or_cycling.csv")
