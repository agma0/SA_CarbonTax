
library(dplyr)
library(tidyverse)
library(haven)
library(openxlsx)
library(data.table)

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####

# Loading Data ####

house       <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-households-v1.csv",       col_types = cols(UQNO = col_character())) # general information on households
hhassets    <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-household-assets-v1.csv", col_types = cols(UQNO = col_character())) # generel information on asset ownership
# income_v3 <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-personincome-v1.csv",     col_types = cols(UQNO = col_character())) # not necessarily needed as we are not interested in income
person      <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-persons-final-v1.csv",    col_types = cols(UQNO = col_character())) # general information on eduaction and age structure
items       <- read_dta("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-total-v1-stata14/lcs-2014-2017-total-v1-stata14/lcs-2014-2015-total-v1.dta") # expenditures



#### CHANGE PATH ####
# Set working directory Agatha
# setwd("C:/Users/agath/Desktop/Code GitHub")

# LCS 2014/2015 
#items = read_dta("LCS 2014-2015/lcs-2014-2015-total-v1-stata14/lcs-2014-2017-total-v1-stata14/lcs-2014-2015-total-v1.dta")
#house = read_dta("LCS 2014-2015/lcs-2014-2015-v1-stata/stata/lcs-2014-2015-households-v1.dta")
#person = read_dta("LCS 2014-2015/lcs-2014-2015-v1-stata/stata/lcs-2014-2015-persons-final-v1.dta")
#hhassets = read_dta("LCS 2014-2015/lcs-2014-2015-v1-stata/stata/lcs-2014-2015-household-assets-v1.dta") 
#income = read_dta("LCS 2014-2015/lcs-2014-2015-v1-stata/stata/lcs-2014-2015-personincome-v1.dta") # not used

item_code <- read.xlsx("GTAP/Item_Codes.xlsx")


# Household Data ####

# Get Variables from Household Data
household_information <- house %>%
  add_count (UQNO) %>%
  select (UQNO, hhsize, hholds_wgt , SexOfHead, PopGroupOfHead, province_code, SETTLEMENT_TYPE, Q524ELECT, 
          Q525AMAINS, Q525BSOURCE, Q526FREEELCET, Q527COOK, Q527LIGHT, Q527WATER, Q527SPACE, income, expenditure  ) %>%
  rename (hh_id              = UQNO, 
          hh_size            = hhsize, 
          hh_weights         = hholds_wgt, 
          gender_hhh         = SexOfHead, 
          ethnicity_hhh      = PopGroupOfHead, 
          province           = province_code, 
          urban_1            = SETTLEMENT_TYPE, 
          electricitycon     = Q524ELECT, 
          electricitymains   = Q525AMAINS, 
          electricitysource  = Q525BSOURCE, 
          electricityfree    = Q526FREEELCET, 
          cookingsource      = Q527COOK, 
          lightingsource     = Q527LIGHT, 
          heatingsourcewater = Q527WATER, 
          heatingsourcespace = Q527SPACE, 
          income             = income, 
          consumption        = expenditure) %>%
  distinct(hh_id, .keep_all = TRUE)


# Get Variables from Person Data
householdmembers <- person %>%
  select (UQNO, PERSONNO, Q14AGE, Q16RELATION, Q21HIGHLEVEL) %>%
  rename (hh_id         = UQNO, 
          perno         = PERSONNO, 
          age           = Q14AGE, 
          relationhhh   = Q16RELATION, 
          education_hhh = Q21HIGHLEVEL)


#ADULTS
#Number Adults/Children
adults <- householdmembers %>%
  select (hh_id, age) %>%
  group_by(hh_id) %>%
  mutate (adult = sum (age >= 18 , na.rm = TRUE)) %>%
  mutate (child = sum (age <18 , na.rm = TRUE)) %>%
  distinct (hh_id, .keep_all = TRUE) %>%
  select(-c(age))


# Get Education Household Head
hhh <- householdmembers  %>%
  select (hh_id, education_hhh, relationhhh)

hhh <- hhh [!(hhh$relationhhh > 1),] %>%
  select (-c(relationhhh))


#assets - stove & motor vehicle
assets <- hhassets %>%
  select (UQNO, Q69108STOVE, Q69126VEHICLE) %>%
  rename (hh_id = UQNO, own_stove = Q69108STOVE, own_vehicle = Q69126VEHICLE)


#merge household_members
household_members1       <- left_join(hhh, assets, by ="hh_id")
household_information_sa <- left_join(household_information, household_members1, by ="hh_id")

household_information_sa <- household_information_sa %>% 
  relocate(gender_hhh, .after = last_col()) %>%
  relocate(ethnicity_hhh, .after = last_col())



# Table with household Information
write_csv(household_information_sa, "LCS_results/household_information_sa.csv")



# Expenditure data ####

# Get Variables
items_list <- items %>%
  select (UQNO, Coicop, valueannualized_adj) %>%
  rename (hh_id = UQNO, coicop = Coicop, expenditures_year = valueannualized_adj)


# Matching COICOP to items
expenditure_information <- merge(x = items_list, y = item_code, by = "coicop") %>%
  select(-c( coicop, label))

expenditure_information <- expenditure_information[order(expenditure_information$hh_id),]

rownames(expenditure_information) <- NULL



# write new table
write_csv(expenditure_information, "LCS_results/expenditure_information_sa.csv")


rm(list=ls())





