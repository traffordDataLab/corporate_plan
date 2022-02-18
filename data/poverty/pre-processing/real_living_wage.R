# Employees paid at/above real living wage defined by the Living Wage Foundation https://www.livingwage.org.uk/what-real-living-wage
# Created: 2022-02-18

# Source: Annual Survey of Hours and Earnings, ONS
# To find the page and file URLs required - as shown below, the following URL performs the required search:
# Search URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datalist?sortBy=release_date&query=living%20wage&filter=user_requested_data&fromDateDay=&fromDateMonth=&fromDateYear=&toDateDay=&toDateMonth=&toDateYear=&size=50
# Page URLs containing data ZIP files to download. The data is usually provided for 2 year periods at a time within each ZIP file. Always get/use the latest year available just in case the data has been revised from the previous release:
#   - NOTE: earliest data is 2015 but doesn't state "work geography" and is in a different format to all the later files, so ignore and start at 2016
#   - 2016 (ignore 2017): https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/007656annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployejobswithhourlypaybelowthelivingwagebyparliamentaryconstituencyandlocalauthorityukapril2016andapril2017
#   - 2017 (ignore 2018): https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/009211annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2017andapril2018
#   - 2018 (ignore 2019): https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/10743annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2018andapril2019
#   - 2019 (ignore 2020): https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/12439annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2019andapril2020
#   - 2020 (revised) and 2021 (provisional): https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/13855annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2020andapril2021

# Licence: Open Government Licence


# Load required packages ---------------------------
library(tidyverse); library(readxl)


# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2019.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Tibble to hold the completed dataset
df_rlw <- tibble();

# Function to download ZIP data file and extract the data in a consistent format
get_data <- function (url, workbook, data_year) {
  # Get the filename from the url (all characters after the last "/")
  file_name <- str_replace(url, "^.+/", "")
  
  # Download the file to temporary location  
  download.file(url, dest = file_name)
  
  # Extract the required Excel workbook from the zip
  unzip(file_name, files = workbook, exdir = ".")
  
  # Open the workbook, extract the data and store it, ready to return it to the caller
  # NOTE: value in all instances is the % BELOW the real Living wage. need to mutate value = 100 - value to get % at or above the real living wage.
  df_temp <- read_xls(workbook, sheet = 2, skip = 4) %>%
    select(area_code = "Code", area_name = "Description", value = 4) %>%
    filter(area_code %in% authorities$area_code) %>%
    mutate(period = data_year,
           value = 100 - as.numeric(value))
    
  # Tidy up the filesystem by deleting the zip and workbook
  file.remove(c(file_name, workbook))
  
  # Return the data to the caller
  df_temp %>%
    select(area_code, area_name, period, value)
}


# Download, extract and tidy the data for each year and bind it together into a complete dataset ---------------------------

# 2016
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/007656annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployejobswithhourlypaybelowthelivingwagebyparliamentaryconstituencyandlocalauthorityukapril2016andapril2017/livingwagebylaandpc.zip",
                               workbook = "Work Geography LW Table 7.1a   lpmgx 2016.xls",
                               data_year = "2016")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2017
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/009211annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2017andapril2018/20172018livingwagebyworkgeographyv2.zip",
                               workbook = "Work Geography LW Table 7.1a   lpmgx 2017.xls",
                               data_year = "2017")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2018
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/10743annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2018andapril2019/20182019livingwagebyworkgeography.zip",
                               workbook = "Work Geography LW Table 7.1a   lwfmgx 2018.xls",
                               data_year = "2018")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2019
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/12439annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2019andapril2020/20192020livingwagebyworkgeography.zip",
                               workbook = "Work Geography LW Table 7.1a   lwfmgx 2019.xls",
                               data_year = "2019")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2020
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/13855annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2020andapril2021/livingwagebyworkgeography2020revised.zip",
                               workbook = "Work Geography LWF Table 7 LWF.1a   lwfmgx 2020.xls",
                               data_year = "2020")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2021
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/13855annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2020andapril2021/livingwagebyworkgeography2021provisional.zip",
                               workbook = "Work Geography LWF Table 7 LWF.1a   lwfmgx 2021.xls",
                               data_year = "2021")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset


# Finalise the completed dataset with the common variables ---------------------------
df_rlw <- df_rlw %>%
  mutate(indicator = "Employees paid at or above the real living wage by work geography",
         measure = "Percentage",
         unit = "Persons") %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)


# Export the tidied data ---------------------------
write_csv(df_rlw, "../real_living_wage.csv")
