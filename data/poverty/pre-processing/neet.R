# 16-17 academic year olds not in education, employment or training (NEET)
# Created: 2022-02-16

# Source: PHE Fingertips
#         https://fingertips.phe.org.uk/profile/cypmh


# Load required packages ---------------------------
library(tidyverse) 

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the data ---------------------------
df_neet_raw <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93203&area_type_id=101")

# Tidy the data ---------------------------
df_neet <- df_neet_raw %>%
  select(area_code = `Area Code`,
         area_name = `Area Name`,
         period = `Time period`,
         value = Value,
         indicator = `Indicator Name`,
         unit = Sex,
         Category) %>%
  filter(unit == "Persons",
         area_code %in% authorities$area_code,
         is.na(Category)) %>%
  mutate(measure = "Percentage",
         value = round(value, 2)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_neet, "../neet.csv")
