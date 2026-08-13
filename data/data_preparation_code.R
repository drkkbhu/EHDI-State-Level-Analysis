
# 
# '# PROLOG   ####################################################'
# 
# '# PROJECT: EHDI State Level Analysis  #'
# '# PURPOSE: To prepare a single CSV with all necessary ACS,and KFF data #'
# '# DIR:     C:\Users\kesha\OneDrive\Desktop\R_git\capstone_data_prep #'
# '# RPRJ:    C:\Users\kesha\OneDrive\Desktop\R_git\capstone_data_prep #'
# '# DATA:    C:\Users\kesha\OneDrive\Desktop\R_git\capstone_data_prep #'
# '# AUTHOR:  Keshav Kumar #'
# '# CREATED: Dec 20 2025 #'
# '# LATEST:  Apr 28 2026 #'
# '# NOTES:   Any relevant notes can go here, #' 
# '#            such as what version changes include  #'
# 
# '# PROLOG   ####################################################'


# Set up ------------------------------------------------------------------

# Libraries
library(readxl)
library(purrr)
library(writexl)
library(openxlsx)
library(odbc)
library(magrittr)
library(tidyverse)
library(kableExtra)


###############################################################################

#selecting rows as per desired states (51 states)
state_names <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                 "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                 "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                 "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                 "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                 "West Virginia", "Wisconsin", "Wyoming","Totals")


# reading the social explorer data------
acs_09 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2009.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2009",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_10 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2010.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2010",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_11 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2011.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2011",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_12 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2012.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2012",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_13 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2013.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2013",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_14 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2014.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2014",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_15 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2015.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2015",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_16 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2016.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2016",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_17 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2017.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2017",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")


acs_18 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2018.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2018",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")


acs_19 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2019.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2019",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")


# acs_20 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2020.csv", skip=1) %>% 
# select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
#        `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
#   rename("State" = `Geo_NAME`,
#          "Total_population" = `SE_A01001_001`,
#          "age_under5" = `SE_A01001_002`,
#          "Median_household_income" = `SE_A14006_001`,
#          "male_under5" = `SE_A02002_003`,
#          "female_under5" = `SE_A02002_016`,
#          "have_insurance_under18"=`SE_A20002_004`,
#          "without_insurance_under18"= `SE_A20002_003`,
#          "Population_under18"=`SE_A20002_002`,
#          "Family_income_below_poverty"=`SE_A13002_002`,
#          "Family_income_above_poverty" = `SE_A13002_011`,
#          "Total_families" = `SE_A13002_001`,
#          "Population_under18_in_poverty"= `SE_A13003A_002`,
#          "Population_under18_status"=`SE_A13003A_001`,
#         "Total_population18_count" =`SE_B01001_002`) %>% 
#   filter(State %in% state_names) %>% 
#   #Add identifying year
#   mutate(Year ="2020",
#          uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
#          F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
#          under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
#   relocate(Year, .before= "Total_population")

acs_21 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2021.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2021",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

acs_22 <- read_csv("ACS_data_1yr_est/Data_age_race_income_2022.csv", skip=1) %>% 
  select(`Geo_NAME`,`SE_A01001_001`,`SE_A01001_002`, `SE_A14006_001`,`SE_A02002_003`,`SE_A02002_016`,`SE_A13002_001`,
         `SE_A20002_004`,`SE_A20002_003`, `SE_A13002_002`,`SE_A20002_002`, `SE_A13002_011`,`SE_A13003A_001`,`SE_A13003A_002`,`SE_B01001_002`) %>% 
  rename("State" = `Geo_NAME`,
         "Total_population" = `SE_A01001_001`,
         "age_under5" = `SE_A01001_002`,
         "Median_household_income" = `SE_A14006_001`,
         "male_under5" = `SE_A02002_003`,
         "female_under5" = `SE_A02002_016`,
         "have_insurance_under18"=`SE_A20002_004`,
         "without_insurance_under18"= `SE_A20002_003`,
         "Population_under18"=`SE_A20002_002`,
         "Family_income_below_poverty"=`SE_A13002_002`,
         "Family_income_above_poverty" = `SE_A13002_011`,
         "Total_families" = `SE_A13002_001`,
         "Population_under18_in_poverty"= `SE_A13003A_002`,
         "Population_under18_status"=`SE_A13003A_001`,
         "Total_population18_count" =`SE_B01001_002`) %>% 
  filter(State %in% state_names) %>% 
  #Add identifying year
  mutate(Year ="2022",
         uninsured_under18_rate =without_insurance_under18/Population_under18*1000,
         F_income_below_poverty_rate=Family_income_below_poverty/Total_families*1000,
         under18_poverty_rate=Population_under18_in_poverty/Population_under18_status*1000) %>%
  relocate(Year, .before= "Total_population")

# reading KFF health care spending per capita data
kff_data <- read_csv("Kaiser_old/KFF_Health_care_expenditure_2007-2020.csv", skip=2)

# Pivot the data to have years as columns
kff_pivoted <- kff_data %>%
  pivot_longer(cols = -Location, names_to = "Variable", values_to = "Value") %>%
  separate(Variable, into = c("Year", "Type"), sep = "__") %>%
  mutate(Value = as.numeric(Value)) %>% # to store spending data in numeric/column not list
  pivot_wider(names_from = Type, values_from = Value, values_fn = dplyr::first)

# Remove the rows with extra "State" data that we don't need
kff_filter <- kff_pivoted %>%
  #select(-c("NA", "Total")) %>% 
  filter(
    # Health Spending per Capita is NA and NULL for the rows with extra data we don't want
    !is.na(`Health Spending per Capita`)  &
      # Totals is included but maybe we don't want - we'll just get ourselves with the entire dataset
      `Health Spending per Capita` != "NULL" &
      Location !=""
  )%>%
  rename("State" = `Location`,
         Health_care_expenditure = `Health Spending per Capita`)
kff_filter$Year <- as.character(kff_filter$Year)

# reading CHIP data
chip_data <- read_csv("KFF_data_new/CHIP_enrollment_annual_data.csv", skip=2) %>% 
  select(`Location`, `FY2015__CHIP Enrollment`, `FY2016__CHIP Enrollment`, `FY2017__CHIP Enrollment`,
         `FY2018__CHIP Enrollment`, `FY2019__CHIP Enrollment`,`FY2020__CHIP Enrollment`,`FY2021__CHIP Enrollment`) %>% 
  rename("State" = `Location`) %>% 
  filter(State %in% state_names)



# Pivot the data to have years as columns
chip_pivoted <- chip_data %>%
  pivot_longer(cols = -State, names_to = "Variable", values_to = "Value") %>%
  separate(Variable, into = c("Year", "Type"), sep = "__") %>%
  mutate(
    Year = gsub("^FY", "", Year)  # remove FY prefix
  ) %>%
  pivot_wider(names_from = Type, values_from = Value)

# Remove the rows with extra "State" data that we don't need
chip_filter <- chip_pivoted %>%
  #select(-c("NA", "Total")) %>% 
  filter(
    # CHIP enrollment is NA and NULL for the rows with extra data we don't want
    !is.na(`CHIP Enrollment`)  &
      # Totals is included but maybe we don't want - we'll just get ourselves with the entire dataset
      `CHIP Enrollment` != "NULL" &
      State !=""
  ) %>% 
  rename("CHIP_Enrollment" = `CHIP Enrollment`) 
chip_filter$Year <- as.character(chip_filter$Year)

# reading KFF percent reproductive women insurance data
per_rep_women_uninsu <- read_csv("KFF_data_new/insurance reproductive age.csv", skip=2) %>% 
  select(`Location`, `2008__Uninsured`, `2009__Uninsured`, `2010__Uninsured`,
         `2011__Uninsured`, `2012__Uninsured`,`2013__Uninsured`,`2014__Uninsured`, `2015__Uninsured`,
         `2016__Uninsured`, `2017__Uninsured`,`2018__Uninsured`,`2019__Uninsured`, 
         `2021__Uninsured`, `2022__Uninsured`,`2023__Uninsured`,`2024__Uninsured`) %>% 
  rename("State" = `Location`) %>% 
  filter(State %in% state_names)



# Pivot the data to have years as columns
per_rep_women_uninsu_pivoted <- per_rep_women_uninsu %>%
  pivot_longer(cols = -State, names_to = "Variable", values_to = "Value") %>%
  separate(Variable, into = c("Year", "Type"), sep = "__") %>%
  # mutate(Year = gsub("^FY", "", Year)) %>%  # remove FY prefix
  pivot_wider(names_from = Type, values_from = Value)

# Remove the rows with extra "State" data that we don't need
per_rep_women_uninsu_filter <- per_rep_women_uninsu_pivoted %>%
  #select(-c("NA", "Total")) %>% 
  filter(
    # percent women uninsured is NA and NULL for the rows with extra data we don't want
    !is.na(`Uninsured`)  &
      # Totals is included but maybe we don't want - we'll just get ourselves with the entire dataset
      `Uninsured` != "NULL" &
      State !=""
  ) %>% 
  rename("Per_rep_women_uninsured" = `Uninsured`) 
#converting year column to character
per_rep_women_uninsu_filter$Year <- as.character(per_rep_women_uninsu_filter$Year)

# reading KFF percent women ever breastfed data
per_women_breastfed <- read_csv("KFF_data_new/women ever breastfed.csv", skip=2) %>% 
  select(`Location`, `2009__Ever Breastfed`, `2010__Ever Breastfed`,
         `2011__Ever Breastfed`, `2012__Ever Breastfed`,`2013__Ever Breastfed`,`2014__Ever Breastfed`, `2015__Ever Breastfed`,
         `2016__Ever Breastfed`, `2017__Ever Breastfed`,`2018__Ever Breastfed`,`2019__Ever Breastfed`,`2020__Ever Breastfed`, 
         `2021__Ever Breastfed`, `2022__Ever Breastfed`) %>% 
  rename("State" = `Location`) %>% 
  filter(State %in% state_names)
#converting year column to character
per_women_breastfed$`2014__Ever Breastfed` <- as.character(per_women_breastfed$`2014__Ever Breastfed`)
per_women_breastfed$`2015__Ever Breastfed` <- as.character(per_women_breastfed$`2015__Ever Breastfed`)


# Pivot the data to have years as columns
per_women_breastfed_pivoted <- per_women_breastfed %>%
  pivot_longer(cols = -State, names_to = "Variable", values_to = "Value") %>%
  separate(Variable, into = c("Year", "Type"), sep = "__") %>%
  # mutate(Year = gsub("^FY", "", Year)) %>%  # remove FY prefix
  pivot_wider(names_from = Type, values_from = Value)

# Remove the rows with extra "State" data that we don't need
per_women_breastfed_filter <- per_women_breastfed_pivoted %>%
  #select(-c("NA", "Total")) %>% 
  filter(
    # percent women breastfed is NA and NULL for the rows with extra data we don't want
    !is.na(`Ever Breastfed`)  &
      # Totals is included but maybe we don't want - we'll just get ourselves with the entire dataset
      `Ever Breastfed` != "NULL" &
      State !=""
  ) %>% 
  rename("Per_women_breastfed" = `Ever Breastfed`) 
#converting year column to character
per_women_breastfed_filter$Year <- as.character(per_women_breastfed_filter$Year)

# reading SHADAC data for state public health funding
state_per_capita <- read_csv("SHADAC_minnesota/Per person state public health funding.csv", skip=6) %>% 
  select(`Location`, `TimeFrame`, `Data`) %>% 
  rename("State" = `Location`,
         "Year" = `TimeFrame`,
         "state_per_capita_fund"=`Data`) %>% 
  filter(State %in% state_names, 
         Year >=2009 & Year <=2022)
#converting year column to character
state_per_capita$`Year` <- as.character(state_per_capita$`Year`)
state_per_capita$`state_per_capita_fund` <- as.numeric(state_per_capita$`state_per_capita_fund`)

# reading SHADAC data for drinks per capita
drinks_per_capita <- read_csv("SHADAC_minnesota/Per capita alcohol consumption by Beverage Type.csv", skip=6) %>% 
  select(`Location`, `TimeFrame`, `Beverage Type`,`Data`) %>% 
  rename("State" = `Location`,
         "Year" = `TimeFrame`,
         "drinks_per_capita"=`Data`) %>% 
  filter(State %in% state_names,
         `Beverage Type` == "All beverages", 
         Year >=2009 & Year <=2022)
#converting year column to character
drinks_per_capita$`Year` <- as.character(drinks_per_capita$`Year`)

# merging the acsdataset
acs <- bind_rows(acs_09,acs_10,acs_11,acs_12,acs_13,acs_14,acs_15, acs_16, acs_17, acs_18, acs_19, acs_21, acs_22)
acs$Year <- as.character(acs$Year)


# creating final social dataset with all variables from acs, kff, chip data
acs_social <- chip_filter %>% 
  left_join(acs, by = c("State", "Year")) %>% 
  left_join(kff_filter, by = c("State", "Year"))%>% 
  mutate( CHIP_enrollment_rate= CHIP_Enrollment/Total_population18_count*1000)

# reading latest ehdi data till 2022
ehdi_data <- read_csv("EHDI_data12182025/ehdidata_2007to2022_published20250226.csv")

#converting year column to character
ehdi_data$Year <- as.character(ehdi_data$Year)
# Filter the 'ehdi_data' data frame for the years 2015 to 2020
#filtered_ehdi_data <- ehdi_data[ehdi_data$Year >= 2015 & ehdi_data$Year <= 2022, ]


# creating comprehensive dataset for evaluation
# 1) Create a complete set of State-Year keys that appear anywhere
state_year_key <- bind_rows(
  ehdi_data %>% select(State, Year),
  kff_filter %>% select(State, Year),
  chip_filter %>% select(State, Year),
  acs %>% select(State, Year),
  per_women_breastfed_filter %>% select(State, Year),
  per_rep_women_uninsu_filter %>% select(State, Year),
  state_per_capita %>% select(State, Year),
  drinks_per_capita %>% select(State, Year)
) %>%
  distinct() %>%
  filter(State %in% state_names)

# 2) Join everything onto that key (keeping all State-Year rows)
complete_data <- state_year_key %>%
  left_join(ehdi_data, by = c("State", "Year")) %>%
  left_join(kff_filter, by = c("State", "Year")) %>%
  left_join(chip_filter, by = c("State", "Year")) %>%
  left_join(acs, by = c("State", "Year")) %>%
  left_join(per_women_breastfed_filter, by = c("State", "Year")) %>%
  left_join(per_rep_women_uninsu_filter, by = c("State", "Year")) %>% 
  left_join(state_per_capita, by = c("State", "Year")) %>%
  left_join(drinks_per_capita, by = c("State", "Year")) %>%
  mutate(
    CHIP_enrollment_rate = if_else(
      is.na(CHIP_Enrollment) | is.na(Total_population18_count) | Total_population18_count == 0,
      NA_real_,
      CHIP_Enrollment / Total_population18_count * 1000
    )
  )


# missing state variable investigation

# # calculating annual state summaries for acs data ----
# acs_social <- acs_social %>%
#   # State average
#   bind_rows(acs_social %>%
#               group_by(Year) %>% 
#               summarise(across(where(is.numeric), ~mean(., na.rm=TRUE))) %>%
#               mutate(State="State average")) %>%
#   # State mean
#   bind_rows(acs_social %>%
#               group_by(Year) %>% 
#               summarise(across(where(is.numeric), ~median(., na.rm=TRUE))) %>%
#               mutate(State="State median")) %>% 
#   #state SD
#   bind_rows(acs_social %>%
#               group_by(Year) %>% 
#               summarise(across(where(is.numeric), ~sd(., na.rm=TRUE))) %>%
#               mutate(State="State SD"))


# writing complete data
#write csv data
# write_csv(complete_data,
#           "Output_dataprep/ehdi_acs_chip_womenunin_breastfed_drink_shadac_data_2007to2022_cleaned12262025.csv")


# state level investigation

# filter data for the desired year
data_2009_2022 <- complete_data %>% 
  filter(Year >= 2009 & Year <=2022)

# data investigation for missingness

vars_primary <- c("PerTscr","Dr",
  "HLp", "EIr","state_per_capita_fund",
  "Median_household_income",
  "Per_women_breastfed",
  "Per_rep_women_uninsured")

years_needed <- 2009:2022

make_missing_years_state_variable <- function(df_raw,
                                              vars = vars_primary,
                                              years = years_needed,
                                              states = NULL,
                                              state_col = "State",
                                              year_col  = "Year",
                                              show_complete_as = "C") {
  
  years <- as.integer(years)
  
  df0 <- df_raw %>%
    mutate(!!sym(year_col) := as.integer(.data[[year_col]])) %>%
    filter(.data[[year_col]] %in% years)
  
  if (!is.null(states)) {
    df0 <- df0 %>% filter(.data[[state_col]] %in% states)
  }
  
  # Full State x Year grid based on states present in df0 (or your provided states)
  grid <- df0 %>%
    distinct(.data[[state_col]]) %>%
    tidyr::crossing(!!sym(year_col) := years) %>%
    left_join(df0, by = c(state_col, year_col))
  
  # Helper: missing if NA OR empty string OR "NA"/"NaN" as text
  is_missing_value <- function(x) {
    if (is.numeric(x)) return(is.na(x))
    x_chr <- trimws(as.character(x))
    is.na(x) | x_chr %in% c("", "NA", "NaN")
  }
  
  miss_long <- lapply(vars, function(v) {
    grid %>%
      group_by(.data[[state_col]]) %>%
      summarise(
        variable = v,
        missing_years = paste(
          sort(unique(.data[[year_col]][is_missing_value(.data[[v]])])),
          collapse = ", "
        ),
        .groups = "drop"
      )
  }) %>%
    bind_rows() %>%
    mutate(missing_years = if_else(missing_years == "", show_complete_as, missing_years))
  
  miss_wide <- miss_long %>%
    pivot_wider(names_from = variable, values_from = missing_years) %>%
    arrange(.data[[state_col]])
  
  miss_wide
}

# Use raw data here 
tab_missing_raw <- make_missing_years_state_variable(
  df_raw = data_2009_2022,      # or complete_data %>% filter(Year between 2009-2022)
  vars   = vars_primary,
  years  = years_needed,
  states = state_names,         # optional
  show_complete_as = "C"
)

kable(tab_missing_raw)
# write_csv(tab_missing_raw,
#           "Output_dataprep/missing_data_v1_2009to2022_12262025.csv")

#no need to change any state inclusion and exclusion decision as it does not violates 10% inclusion rule


#balance check for panel
years_needed <- 2009:2022

base <- data_2009_2022 %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Year %in% years_needed) %>%
  select(State, Year, HLp, EIr, PerTscr, Dr) %>%
  distinct()

balance_check <- base %>%
  group_by(State) %>%
  summarise(
    n_years_HLp = sum(!is.na(HLp)),
    n_years_EIr = sum(!is.na(EIr)),
    miss_HLp = length(years_needed) - n_years_HLp,
    miss_EIr = length(years_needed) - n_years_EIr,
    .groups = "drop"
  ) %>%
  arrange(desc(miss_HLp + miss_EIr), State)

balance_check

# print states with one missingness
states_with_one_missing <- balance_check %>%
  filter(miss_HLp == 1 | miss_EIr == 1) %>%
  select(State, miss_HLp, miss_EIr)

states_with_one_missing


# now export final analysis dataset

years_needed <- 2009:2022

# now export final analysis dataset (common states; CHIP excluded)
analysis_data_base <- data_2009_2022 %>%
  select( "State","Year","HLp","EIr","PerTscr","Dr", "state_per_capita_fund","drinks_per_capita","Health_care_expenditure",
          "age_under5","Total_population","uninsured_under18_rate",
          "Median_household_income","under18_poverty_rate",
          "F_income_below_poverty_rate",
          "Per_rep_women_uninsured",
          "Per_women_breastfed", "EI", "TDia"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(EIrd = (EI/TDia)*1000) %>% # added eird based on diagnosed not screened
  filter(Year %in% years_needed) %>%
  distinct()

# export complete data without exclusion of states
# write_csv(analysis_data_base,
#           "Output_dataprep/analysis_complete_data_v2_2009to2022_04282026.csv")



# identify excluded states: more than 1 missing year in HLp OR EIr
excluded_states <- analysis_data_base %>%
  group_by(State) %>%
  summarise(
    n_years_HLp = sum(!is.na(HLp)),
    n_years_EIr = sum(!is.na(EIr)),
    n_years_EIrd = sum(!is.na(EIrd)),
    n_years_PerTscr = sum(!is.na(PerTscr)),
    n_years_Dr = sum(!is.na(Dr)),
    miss_HLp = length(years_needed) - n_years_HLp,
    miss_EIr = length(years_needed) - n_years_EIr,
    miss_EIrd = length(years_needed) - n_years_EIrd,
    miss_PerTscr = length(years_needed) - n_years_PerTscr,
    miss_Dr = length(years_needed) - n_years_Dr,
    .groups = "drop"
  ) %>%
  filter(miss_HLp > 1 | miss_EIr > 1 |miss_EIrd > 1 |miss_PerTscr>1|miss_Dr>1) %>%
  arrange(desc(miss_HLp + miss_EIr+miss_PerTscr+miss_Dr+miss_Dr), State)

# final included states list
included_states <- analysis_data_base %>%
  group_by(State) %>%
  summarise(
    miss_HLp = length(years_needed) - sum(!is.na(HLp)),
    miss_EIr = length(years_needed) - sum(!is.na(EIr)),
    miss_EIrd = length(years_needed) - sum(!is.na(EIrd)),
    miss_PerTscr = length(years_needed) - sum(!is.na(PerTscr)),
    miss_Dr = length(years_needed) - sum(!is.na(Dr)),
    .groups = "drop"
  ) %>%
  filter(miss_HLp <= 1 & miss_EIr <= 1 & miss_EIrd <= 1& miss_PerTscr<=1 & miss_Dr<=1) %>%
  pull(State)

# final analysis dataset (only included states)
analysis_data <- analysis_data_base %>%
  filter(State %in% included_states)

# quick checks
excluded_states
analysis_data %>% summarise(n_states = n_distinct(State), n_rows = n())

# Additional missing states from the required dataframe
analysis_df <- analysis_data %>%
  mutate(complete_HE = if_else(!is.na(HLp) & !is.na(EIr), 1, 0))

#chcek count by year
analysis_df %>%
  group_by(Year) %>%
  summarise(
    total_states = n_distinct(State),
    complete_states = sum(complete_HE)
  )
# identify exactly which states drop
analysis_df %>%
  filter(Year %in% c(2009,2011,2015), complete_HE == 0) %>%
  select(State, Year, HLp, EIr)


# export data
# write_csv(analysis_data,
#           "Output_dataprep/analysis_data_v3_EIrd_2009to2022_4282026.csv")
# 


# complete case sample for sensitivity analysis -----------

years_sensitivity <- years_needed[years_needed != 2020]   # 13 years: 2009-2019, 2021-2022

complete_states <- analysis_data_base %>%
  filter(Year %in% years_sensitivity) %>%
  group_by(State) %>%
  summarise(
    n_years_HLp     = sum(!is.na(HLp)),
    n_years_EIr     = sum(!is.na(EIr)),
    n_years_EIrd     = sum(!is.na(EIrd)),
    n_years_PerTscr = sum(!is.na(PerTscr)),
    n_years_Dr      = sum(!is.na(Dr)),
    miss_HLp     = length(years_sensitivity) - n_years_HLp,
    miss_EIr     = length(years_sensitivity) - n_years_EIr,
    miss_EIrd     = length(years_sensitivity) - n_years_EIrd,
    miss_PerTscr = length(years_sensitivity) - n_years_PerTscr,
    miss_Dr      = length(years_sensitivity) - n_years_Dr,
    .groups = "drop"
  ) %>%
  filter(miss_HLp == 0 & miss_EIr == 0 & miss_EIrd == 0 & miss_PerTscr == 0 & miss_Dr == 0) %>%
  arrange(State)

analysis_data_complete <- analysis_data_base %>%
  filter(State %in% complete_states$State,
         Year %in% years_sensitivity)

# quick check
cat(sprintf("Dataset 1 (primary):       %d states, %d obs\n",
            n_distinct(analysis_data$State), nrow(analysis_data)))
cat(sprintf("Dataset 2 (complete-case): %d states, %d obs\n",
            n_distinct(analysis_data_complete$State), nrow(analysis_data_complete)))

# export sensitive analysis data
# write_csv(analysis_data_complete,
#           "Output_dataprep/analysis_data_complete_case_sensitivity_with_EIrd.csv")
