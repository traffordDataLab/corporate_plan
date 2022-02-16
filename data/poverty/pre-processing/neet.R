# 16-17  year olds not in education, employment or training (NEET)
# Created: 2022-02-16

# Source
# NEET & unknown: PHE Fingertips
#                 https://fingertips.phe.org.uk/profile/cypmh
# 
# NEET only:      LG Inform+ (Requires API key)
#                 https://developertools.esd.org.uk/data?value.valueType=raw&metricType=9613&area=E08000009%2CTrafford_CIPFA_Near_Neighbours%2CE92000001&period=latest%3A5&rowGrouping=area


# Load required packages ---------------------------
library(tidyverse) 

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the data from PHE Fingertips: NEET and unknown combined ---------------------------
df_neet_raw <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93203&area_type_id=101")

# Tidy the PHE Fingertips data ---------------------------
df_neet_and_unknown <- df_neet_raw %>%
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
         value = round(value, 1),
         period = as.character(period)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Download the data from LG Inform+: NEET only ---------------------------
# NOTE: this requires an API key
api_key <- "[INSERT API TOKEN HERE]"
df_neet_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?value.valueType=raw&metricType=9613&area=E08000009%2CTrafford_CIPFA_Near_Neighbours%2CE92000001&period=latest%3A5&rowGrouping=area&ApplicationKey=", api_key), skip = 2)

# Tidy the LG Inform+ data ---------------------------
df_neet <- df_neet_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Percentage",
         unit = "Persons",
         indicator = "16-17 year olds not in education, employment or training (NEET)",
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)
    
# Combine the datasets ---------------------------
df_neet <- bind_rows(df_neet_and_unknown, df_neet)
  
# Export the tidied data ---------------------------
write_csv(df_neet, "../neet.csv")
