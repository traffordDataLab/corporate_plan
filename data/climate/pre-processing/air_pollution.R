# Nitrogen Dioxide (NO2) and Particulate Matter (PM10) concentrations 
# Created: 2022-01-27

# Source: Ricardo EE
#         https://www.airqualityengland.co.uk/

# Load required packages ---------------------------
library(openair) ; library(tidyverse) ; library(lubridate) ; library(rvest)

# Function to download data ---------------------------
airqualityengland <- function(site_id, start_date, end_date, pollutant) {
  
  if(pollutant=="PM10"){poll="GE10"}else{poll=pollutant}
  
  url <- paste0("https://www.airqualityengland.co.uk/site/data.php?site_id=", site_id, "&parameter_id%5B%5D=" ,poll , "&f_query_id=1818812&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=", start_date, "&f_date_ended=", end_date, "&la_id=368&action=download&submit=Download+Data")
  
  readings <- read_html(url) %>%
    html_node("a.b_xls.valignt") %>%
    html_attr('href') %>%
    read_csv(skip = 5) %>%
    mutate(`End Date` = as.Date(`End Date`, format = "%d/%m/%Y"),
           `End Time` = gsub("NA", "24:00:00", `End Time`),
           date = as.POSIXct(paste(`End Date`, `End Time`), format = "%Y-%m-%d %H:%M:%S"),
           pollutant = pollutant,
           value = as.double(get(pollutant))) %>%
    select(date,pollutant,value) %>% 
    arrange(date) %>%
    mutate(station=site_id) %>%
    filter(!is.na(date))
  
  return(readings)
  
}

# Monitoring site IDs
A56 <- "TRF2"
MossPark <- "TRAF"
Wellacre <- "TRF3"

# Pollutants: Nitrogen Dioxide = "NO2", Particulate Matter 10mg = "GE10" in the API

# Latest year of data we want
max_year <- 2021

# Get NO2 data for last complete 10 years ---------------------------
# After getting each year, merge it into a single dataset

df_a56 <- airqualityengland(A56, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "NO2")

df_no2 <- bind_rows(df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-4, "-01-01"), paste0(max_year-4, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-4, "-01-01"), paste0(max_year-4, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-4, "-01-01"), paste0(max_year-4, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-5, "-01-01"), paste0(max_year-5, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-5, "-01-01"), paste0(max_year-5, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-5, "-01-01"), paste0(max_year-5, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-6, "-01-01"), paste0(max_year-6, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-6, "-01-01"), paste0(max_year-6, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-6, "-01-01"), paste0(max_year-6, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-7, "-01-01"), paste0(max_year-7, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-7, "-01-01"), paste0(max_year-7, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-7, "-01-01"), paste0(max_year-7, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

df_a56 <- airqualityengland(A56, paste0(max_year-8, "-01-01"), paste0(max_year-8, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-8, "-01-01"), paste0(max_year-8, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-8, "-01-01"), paste0(max_year-8, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

# NOTE: Seems that Wellacre only started recording NO2 from 2013-09-20
df_a56 <- airqualityengland(A56, paste0(max_year-9, "-01-01"), paste0(max_year-9, "-12-31"), "NO2")
df_mp <- airqualityengland(MossPark, paste0(max_year-9, "-01-01"), paste0(max_year-9, "-12-31"), "NO2")
df_wa <- airqualityengland(Wellacre, paste0(max_year-9, "-01-01"), paste0(max_year-9, "-12-31"), "NO2")

df_no2 <- bind_rows(df_no2, df_a56, df_mp, df_wa)

# Tidy the NO2 data ---------------------------
df_no2_final <- df_no2 %>%
  drop_na() %>%
  mutate(period = year(date)) %>%
  filter(period <= max_year) %>% # This removes a slight anomaly in the results of having results from max_year+1-01-01T00:00:00
  group_by(period, station) %>%
  summarise(value = round(mean(value), digits = 1)) %>%
  mutate(station_name = case_when(station == "TRF2" ~ "Trafford A56",
                                  station == "TRF3" ~ "Trafford Wellacre Academy",
                                  TRUE ~ "Trafford Moss Park"),
         indicator = "Nitrogen Dioxide (NO2)",
         measure = "Annual mean concentration",
         unit = "µg/m3") %>%
  select(station_code = station, station_name, period, indicator, measure, unit, value)
  
# Export the tidied NO2 data ---------------------------
write_csv(df_no2_final, "../no2_concentration.csv")


# Get PM10 data for last complete 10 years ---------------------------
# After getting each year, merge it into a single dataset

df_a56 <- airqualityengland(A56, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-4, "-01-01"), paste0(max_year-4, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-4, "-01-01"), paste0(max_year-4, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-5, "-01-01"), paste0(max_year-5, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-5, "-01-01"), paste0(max_year-5, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-6, "-01-01"), paste0(max_year-6, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-6, "-01-01"), paste0(max_year-6, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-7, "-01-01"), paste0(max_year-7, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-7, "-01-01"), paste0(max_year-7, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-8, "-01-01"), paste0(max_year-8, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-8, "-01-01"), paste0(max_year-8, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)

df_a56 <- airqualityengland(A56, paste0(max_year-9, "-01-01"), paste0(max_year-9, "-12-31"), "PM10")
df_mp <- airqualityengland(MossPark, paste0(max_year-9, "-01-01"), paste0(max_year-9, "-12-31"), "PM10")

df_pm10 <- bind_rows(df_pm10, df_a56, df_mp)


# Tidy the PM10 data ---------------------------
df_pm10_final <- df_pm10 %>%
  drop_na() %>%
  mutate(period = year(date)) %>%
  filter(period <= max_year) %>% # This removes a slight anomaly in the results of having results from max_year+1-01-01T00:00:00
  group_by(period, station) %>%
  summarise(value = round(mean(value), digits = 1)) %>%
  mutate(station_name = if_else(station == "TRF2", "Trafford A56", "Trafford Moss Park"),
         indicator = "Particulate Matter (PM10)",
         measure = "Annual mean concentration",
         unit = "µg/m3") %>%
  select(station_code = station, station_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_pm10_final, "../pm10_concentration.csv")
