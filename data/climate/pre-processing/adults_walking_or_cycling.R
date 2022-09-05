# Proportion of adults who do any walking or cycling, for any purpose, five times per week
# Created: 2022-02-07

# Source: Department for Transport (DfT)
#         https://www.gov.uk/government/statistical-data-sets/walking-and-cycling-statistics-cw#participation-in-walking-and-cycling
#         CW0301: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1100941/cw0301.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS); library(httr);

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2021):
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1100941/cw0301.ods",
    write_disk(tmp))

df_raw <- read_ods(tmp, sheet = "CW0301", skip = 7)

# Tidy the data ---------------------------
df_wlk_cyc <- df_raw %>%
  rename(area_code = `ONS Code`,
         area_name = `Area name`) %>%
  filter(Frequency == "At least 5 times per week",
         area_code %in% authorities$area_code) %>%
  select(-Mode, -Purpose, -Frequency) %>% # remove unwanted columns
  mutate(`2021` = as.character(`2021`)) %>% # annoyingly this column is num when the others are char - need all the same to pivot
  pivot_longer(cols = c(-area_code, -area_name),
               names_to = "period",
               values_to = "value") %>%
  # the period needs to be in the format 20YY-YY.
  # each year value in period is the latest, so we need to create the year before number
  mutate(period_to = str_sub(period, -2, -1),
         period_from = (as.numeric(period) -1),
         period = paste0(period_from, "-", period_to),
         value = round(as.numeric(value), 1),
         indicator = "Proportion of adults who do any walking or cycling, for any purpose, five times per week",
         measure = "Percentage",
         unit = "Persons") %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_wlk_cyc, "../adults_walking_or_cycling.csv")

# delete the downloaded raw data
unlink(tmp)
