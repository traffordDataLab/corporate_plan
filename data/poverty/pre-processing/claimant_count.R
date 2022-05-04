# Claimant Count rate #
# Source: ONS
# URL: https://www.nomisweb.co.uk/sources/cc
# Licence: Open Government Licence

cipfa <- read_csv("../../cipfa2019.csv") %>%
  select(area_code) 

df <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=",paste(c("E92000001",cipfa$area_code, "E08000009"), collapse = ','),"&date=latestMINUS36-latest&gender=0&age=0&measure=1,2&measures=20100")) %>%
  mutate(units = "Persons",
         measure = ifelse(MEASURE_NAME == "Claimant count", "Count", "Percentage")) %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         period = DATE_NAME,
         indicator = MEASURE_NAME, measure, units,
         value = OBS_VALUE)

write_csv(df, "../claimant_count.csv")