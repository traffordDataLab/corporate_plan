# Children Poverty #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/wider-determinants
# Licence: Open Government Licence v3.0

library(tidyverse) 

cssn <- read_csv("../../cssn.csv") %>%
  select(area_code)

children_absolute_low_income <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93701&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, `Category Type`, `Area Type`, Age)


children_absolute_low_income_england <- children_absolute_low_income  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

children_absolute_low_income_cssn <- children_absolute_low_income %>%
  filter(area_code %in% c(cssn$area_code, "E08000009")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Percentage")

df1 <- bind_rows(children_absolute_low_income_england, children_absolute_low_income_cssn) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  mutate(unit = "Persons") %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value)

children_relative_low_income <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93700&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, `Category Type`, `Area Type`, Age)


children_relative_low_income_england <- children_relative_low_income  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

children_relative_low_income_cssn <- children_relative_low_income %>%
  filter(area_code %in% c(cssn$area_code, "E08000009")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Percentage")

df2 <- bind_rows(children_relative_low_income_england, children_relative_low_income_cssn) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  mutate(unit = "Persons") %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value)

# Source:Mid-2020 population estimates for local authorities in England
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: Open Government Licence v3.0

pop_15_ward <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1656750701...1656750715,1656750717,1656750716,1656750718...1656750721&date=latest&gender=0&c_age=201&measures=20100") %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         pop0_15 = OBS_VALUE)

# Proportion of people claiming Universal Credit #
# Source: DWP
# URL: https://www.gov.uk/government/collections/children-in-low-income-families-local-area-statistics
# Licence: Open Government Licence


lookup <- read_csv("https://www.trafforddatalab.io/spatial_data/lookups/administrative_lookup.csv") %>%
  filter(lad17nm == "Trafford")

query <- list(database = unbox("str:database:CILIF_REL"),
              measures = "str:count:CILIF_REL:V_F_CILIF_REL",
              dimensions = c("str:field:CILIF_REL:V_F_CILIF_REL:WARD_CODE",
                             "str:field:CILIF_REL:F_CILIF_DATE:DATE_NAME") %>% matrix(),
              recodes = list(
                `str:field:CILIF_REL:V_F_CILIF_REL:WARD_CODE` = list(
                  map = as.list(paste0("str:value:CILIF_REL:V_F_CILIF_REL:WARD_CODE:V_C_MASTERGEOG11_WARD_TO_LA_NI:E0", seq(5000819, 5000839, 1)))),
                `str:field:CILIF_REL:F_CILIF_DATE:DATE_NAME` = list(
                  map = as.list(paste0("str:value:CILIF_REL:F_CILIF_DATE:DATE_NAME:C_CILIF_YEAR:",c(2020))))
              )) %>% toJSON()
request <- POST(
  url = path,
  body = query,
  config = add_headers(APIKey = api_key),
  encode = "json")
response <- fromJSON(content(request, as = "text"), flatten = TRUE)
# extract list items and convert to a dataframe
tabnames <- response$fields$items %>% map(~.$labels %>% unlist)
values <- response$cubes[[1]]$values
dimnames(values) <- tabnames

child_poverty_ward <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(c(response$fields$label,"value"))  %>%
  select(area_name = "National - Regional - LA - Wards", period = Year, value) %>%
  left_join(pop_15_ward, by="area_name") %>%
  mutate(value = round(value*100/pop0_15,1),
         indicator = "Children in relative low income families (under 16s)",
         measure = "Percentage",
         unit = "Persons",
         area_type = "Ward") %>%
  select(-pop0_15)
  
write_csv(bind_rows(df1,df2, child_poverty_ward), "../children_poverty.csv")

