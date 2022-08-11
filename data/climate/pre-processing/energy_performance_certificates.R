# Domestic Energy Performance Certificates (EPC).
# Created: 2022-01-17

# Source: Department for Levelling Up, Housing & Communities
#         https://www.gov.uk/government/statistical-data-sets/live-tables-on-energy-performance-of-buildings-certificates
#         (Also data available at: https://epc.opendatacommunities.org e.g. Trafford: https://epc.opendatacommunities.org/files/domestic-E08000009-Trafford.zip)

# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readODS) ; library(lubridate)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours:
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1094219/D1_-_Domestic_EPCs.ods",
    write_disk(tmp))

# Extract the raw data ---------------------------
df_epc_england_raw <- read_ods(tmp, sheet = 5, skip = 3)
df_epc_la_raw <- read_ods(tmp, sheet = 8, skip = 3)

# Prepare England data ---------------------------
df_epc_england <- df_epc_england_raw %>%
  filter(is.na(Quarter) == FALSE) %>% # This removes the totals at the top of the sheet
  mutate(area_code = "E92000001",
         area_name = "England") %>%
  select(area_code, area_name, Quarter, `Number of Lodgements`, `A`, `B`, `C`)

# Prepare LA data ---------------------------
df_epc_la <- df_epc_la_raw %>%
  select(area_code = `Local Authority Code`,
         area_name = `Local Authority`,
         Quarter, `Number of Lodgements`, `A`, `B`, `C`) %>%
  filter(area_code %in% authorities$area_code)

# Join both datasets together and tidy ---------------------------
df_epc <- bind_rows(df_epc_england, df_epc_la) %>%
  separate(Quarter, into = c("year", "quarter"), sep = "/") %>%
  rename(value_certificates_lodged = `Number of Lodgements`,
         value_rating_A = `A`,
         value_rating_B = `B`,
         value_rating_C = `C`) %>%
  mutate(period = ymd(paste0(year, "-", case_when(quarter == "1" ~ "03-31",
                                                  quarter == "2" ~ "06-30",
                                                  quarter == "3" ~ "09-30",
                                                  quarter == "4" ~ "12-31"))),
         indicator = "Domestic Energy Performance Certificates (EPC) lodged on the Buildings Register",
         measure = "Frequency",
         unit = "Certificates") %>%
  select(area_code, area_name, period, indicator, measure, unit, value_certificates_lodged, value_rating_A, value_rating_B, value_rating_C)

# Export the tidied data ---------------------------
write_csv(df_epc, "../energy_performance_certificates.csv")

# Remove downloaded data
unlink(tmp)
