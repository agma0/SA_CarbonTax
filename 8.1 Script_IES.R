install.packages("haven")
install.packages("openxlsx")

library(dplyr)
library(tidyverse)
library(haven)
library(labelled)
library(sjlabelled)
library(Hmisc)
library(openxlsx)

#IES 2010/11

#### CHANGE PATH ####
# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Code GitHub")

#Import Household Data
items <- read_dta("IES 2010-2011/ies_2010_2011_v1_stata12/Stata12/IES 2010-2011 Total_v1.dta")
house <- read_dta("IES 2010-2011/ies_2010_2011_v1_stata12/Stata12/IES 2010-2011 House_info_v1.dta")
income <- read_dta("IES 2010-2011/ies_2010_2011_v1_stata12/Stata12/IES 2010-2011 Person_Income_v1.dta")
person <- read_dta("IES 2010-2011/ies_2010_2011_v1_stata12/Stata12/IES 2010-2011 Person_info_v1.dta")

item_code <- read.xlsx("GTAP/Item_Codes.xlsx")


########################################## Household Data ##############

#Get variables from household data
household_information_sa <- house %>%
  ### Get HH 
  add_count(UQNO) %>%
  ### variables
  select(UQNO, Hsize, Full_calwgt, GenderOfHead, PopGrpOfHead ,Province, Settlement_type, Q41MAINDWELLING, Q44BSUBSIDISEDDU, Q52MAINDWELLING, Q554VALUEDU, Q45DRINKINGWATER, Q5101A06STOVE, Q5101A12GENERATOR, Q91OWNPRODUCTION, Q49MAINSCONNECTION, Q410FREEELECTRICITY, Consumptions, Income, IncomeDecile) %>%
  ### rename variables
  rename(hh_id = UQNO, hh_size = Hsize, hh_weights = Full_calwgt, gender_hhh = GenderOfHead, ethnicity_hhh = PopGrpOfHead, province = Province, urban_1 = Settlement_type, dwellingtype = Q41MAINDWELLING, dwellingsub =  Q44BSUBSIDISEDDU, dwellingown = Q52MAINDWELLING, dwellingval = Q554VALUEDU, drinkingwater = Q45DRINKINGWATER, stoveown= Q5101A06STOVE, generatorown = Q5101A12GENERATOR, productionown = Q91OWNPRODUCTION, electricitycon = Q49MAINSCONNECTION, electricityfree = Q410FREEELECTRICITY,  consumption = Consumptions, income = Income, Income_Decile = IncomeDecile) %>%
  ### 
  distinct(hh_id, .keep_all = TRUE)


#Householdmembers and age
householdmembers <- person %>%
select (UQNO, PERSONNO, Q14AGE, Q15RELATIONSHIP, Q1603SUBSFARM, Q1609GRANTS, Q21HIGHESTLEVEL) %>%
  rename (hh_id = UQNO, perno = PERSONNO, age = Q14AGE, relationhhh = Q15RELATIONSHIP, incomefarm = Q1603SUBSFARM, socialgrant = Q1609GRANTS, education = Q21HIGHESTLEVEL)

#ADULTS
#Number Adults/Children
adults <- householdmembers %>%
  select (hh_id, age) %>%
  group_by(hh_id) %>%
  mutate (adult = sum (age >=18 , na.rm = TRUE)) %>%
  mutate (child = sum (age <18 , na.rm = TRUE)) %>%
  distinct (hh_id, .keep_all = TRUE) %>%
  select(-c(age))

#Adult equivalent bei adults 
adults$adulteq <- adults$adult*0.5 + 0.5 + adults$child *0.3


#HHH
# age, gender, education hhh
hhh <- householdmembers  %>%
 select (hh_id, age ,education, relationhhh) %>%
  rename (age_hhh = age, education_hhh = education)

hhh <- hhh [!(hhh$relationhhh > 1),] %>%
  select (-c(relationhhh))


#merge household_members
household_members <- merge(adults, hhh, by = "hh_id", all=T)

household_information_sa <- merge(household_information_sa, household_members, by ="hh_id", all = TRUE)

household_information_sa <- household_information_sa %>% 
  relocate(gender_hhh, .after = last_col()) %>%
  relocate(ethnicity_hhh, .after = last_col())

fwrite(household_information_sa, "C:/Users/agath/Desktop/Masterthesis/Data/IES/household_information_sa")




################################    Expenditure data ################

# expenditure items
items_list <- items %>%
  select (UQNO, Coicop, Valueannualized) %>%
  rename (hh_id = UQNO, coicop = Coicop, expenditures_year = Valueannualized)

# MATCHING COICOP TO ITEM CODES - delete columns
items_f <- merge(x = items_list, y = item_code, by = "coicop")%>% 
  select(-c( coicop, label))

# order
items_f <- items_f[order(items_f$hh_id),]
rownames(items_f) <- NULL

# save
fwrite(items_f,"IES/expenditure_information_sa")


# clean Global Environment
rm(list=ls())







