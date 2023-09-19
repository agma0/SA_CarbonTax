
library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
library("dplyr")
library("data.table")
options(scipen=999)

#### CHANGE PATH ####
# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Code GitHub")


# 1. Setup

Country.Name <- "South Africa"
Year_0 <- "2014"
carbon.price <- 30 #in USD/tCO2


# 2. Load Household and Expenditure File 

household_information <- fread("LCS_results/household_information_sa")
expenditure_information <- fread("LCS_results/expenditure_information_sa")


# check if same amount households in expenditure and household information
if(nrow(count(expenditure_information, hh_id)) != nrow(count(household_information, hh_id))) print("WARNING!")


# 3       Data Cleaning #### 

# 3.1     Identify Duplicates ####   

#Households, whose characteristics are identical with another
household_information_1 <- household_information %>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()

if(nrow(filter(household_information_1, flag != 0))>0) View(filter(household_information_1, flag != 0))

hh_duplicates_information <- household_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)

print(paste0(sprintf("For %s, ", Country.Name), nrow(hh_duplicates_information), " households have duplicate information on their characteristics."))


# Households, who spend exactly the same amount of money on each item than any other households
expenditure_information_1 <- expenditure_information %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year")%>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()

print(paste("There are ", nrow(count(expenditure_information, hh_id)) - nrow(filter(expenditure_information_1, flag == 0)), sprintf(" cases of exact duplicates of expenditures on the item level in %s.", Country.Name)))

hh_duplicates_expenditures_1 <- expenditure_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)


# Households, who do not report any individual amount of expenditures on any items
expenditure_information_2 <- expenditure_information %>%
  filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
  group_by(item_code, expenditures_year)%>%
  mutate(duplicate_flag = ifelse(n()>1,1,0))%>%
  ungroup()%>%
  group_by(hh_id)%>%
  mutate(duplicate_share = sum(duplicate_flag)/n())%>%
  ungroup()

hh_duplicates_expenditures_2 <- expenditure_information_2 %>%
  filter(duplicate_share == 1)%>%
  select(hh_id)%>%
  distinct()


# Households, who report the same amount of total expenditures on all items as some other household 
expenditure_information_3 <- expenditure_information %>%
  filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
  group_by(hh_id)%>%
  summarise(hh_expenditures = sum(expenditures_year))%>%
  ungroup()%>%
  group_by(hh_expenditures)%>%
  mutate(duplicate_flag_2 = ifelse(n()>1,1,0))%>%
  ungroup()

print(paste0(nrow(filter(expenditure_information_3, duplicate_flag_2 == 1)), sprintf(" households report the same total expenditures in %s.", Country.Name)))

hh_duplicates_expenditures_3 <- expenditure_information_3 %>%
  filter(duplicate_flag_2 == 1)%>%
  select(hh_id)


# Negative total expenditures
expenditure_information_4 <- expenditure_information_3 %>%
  mutate(flag_negative = ifelse(hh_expenditures < 0,1,0))

hh_negative_expenditures_4 <- expenditure_information_4 %>%
  filter(flag_negative == 1)%>%
  select(hh_id)


# 3.1.1  Duplicate Check #####

# hh_duplicates_information captures all households, whose characteristics are identical with another 
# hh_duplicates_expenditures_1 captures all households, who spend exactly the same amount of money on each item than any other households
# hh_duplicates_expenditures_2 captures all households, who do not report any individual amount of expenditures on any items. 
# hh_duplicates_expenditures_3 captures all households, who report the same amount of total expenditures as some other household

check_0 <- subset(expenditure_information, (hh_id %in% hh_duplicates_information$hh_id)) %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year") # no duplicates

check_1 <- subset(expenditure_information, (hh_id %in% hh_duplicates_expenditures_1$hh_id)) %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year") # no duplicates

check_2 <- subset(expenditure_information, (hh_id %in% hh_duplicates_expenditures_2$hh_id)) %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year") # no duplicates - 418 hh spend same amount on one item - not 100% duplicate

check_3 <- subset(expenditure_information, (hh_id %in% hh_duplicates_expenditures_3$hh_id)) %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year") # no duplicates - 2 hh spend same total expenditures - not duplicate

check_neg <- subset(expenditure_information, (hh_id %in% hh_negative_expenditures_4$hh_id)) %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year") # no negative expenditures


# 3.1.2   Duplicate Removal ####

# if(Country.Name == "South Africa"){
#   household_information <- household_information %>%
#     filter(!hh_id %in% hh_duplicates_expenditures_x$hh_id)
#   expenditure_information <- expenditure_information %>%
#     filter(!hh_id %in% hh_duplicates_expenditures_x$hh_id)
# }


# Clean Environment
rm(expenditure_information_1, expenditure_information_2, expenditure_information_3, household_information_1, 
   hh_duplicates_expenditures_1, hh_duplicates_expenditures_2, hh_duplicates_expenditures_3, hh_duplicates_information,
   hh_negative_expenditures_4, expenditure_information_4, duplicate_information, check_0, check_1, check_2,check_3, check_neg)


# 3.2     Cleaning per Item_code #### 

expenditure_information_4 <- expenditure_information %>%
  left_join(select(household_information, hh_id, hh_weights))%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0 )%>%
  group_by(item_code)%>%
  mutate(outlier_95 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.95),
         outlier_99 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.99),
         median_exp = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.5),
         mean_exp   = wtd.mean(    expenditures_year, weights = hh_weights))%>%
  ungroup()%>%
  mutate(flag_outlier_95 = ifelse(expenditures_year>= outlier_95,1,0),
         flag_outlier_99 = ifelse(expenditures_year>= outlier_99,1,0)) %>%
  mutate(expenditures = ifelse(flag_outlier_99 == 1, median_exp, expenditures_year))%>% # set outlier_99 to median
  select(hh_id, item_code, expenditures, hh_weights)

expenditure_information <- expenditure_information_4 %>%
  select(-hh_weights)


# 3.2.1   Cleaning per Total Expenditures (99%) 

expenditure_information_4.1 <- expenditure_information_4 %>%
  group_by(hh_id) %>%
  summarise(total_expenditures = sum(expenditures),
            hh_weights = first(hh_weights)) %>%
  ungroup() %>%
  mutate(outlier_95 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.95),
         outlier_99 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.99))

expenditure_outlier <- expenditure_information_4.1 %>%
  filter(total_expenditures >= outlier_99)%>%
  select(hh_id)%>%
  distinct()


# delete outliers -> not deleted
# if(Country.Name == "South Africa"){
#   household_information <- household_information %>%
#     filter(!hh_id %in% expenditure_outlier$hh_id)
#   expenditure_information <- expenditure_information%>%
#     filter(!hh_id %in% expenditure_outlier$hh_id)
# }

print("Expenditure data cleaned!")

rm(expenditure_information_4.1, expenditure_information_4, expenditure_outlier)


# 4       Summary Statistics ####

# no duplicates were identifies and no outliers were deleted -> no summary needed


# 5       Transformation and Modelling ####

# 5.1.   Matching GTAP Concordance ####
matching <- read.xlsx(sprintf("GTAP/Item_GTAP_Concordance_South_Africa.xlsx"))

matching <- matching %>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))


# Check if single item codes are assigned to two different GTAP categories
item_codes <- select(expenditure_information, item_code)%>%
  distinct()%>%
  left_join(matching)%>%
  filter(is.na(GTAP))

if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Excel-File!"))

matching.check <- count(matching, item_code)%>%
  filter(n != 1)

if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different GTAP-categories in Excel-File"))

if(nrow(item_codes != 0) | nrow(matching.check) != 0) break

rm(matching.check, item_codes)


# 5.2.   Matching Category Concordance ####
categories <- read.xlsx("GTAP/categories.xlsx") %>%
  drop_na()

item_codes <- select(expenditure_information, item_code) %>%
  distinct() %>%
  left_join(categories) %>%
  filter(is.na(category))

if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Category-Excel-File!"))

matching.check <- count(categories, item_code)%>%
  filter(n != 1)

if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different Categories-categories in Excel-File"))

if(nrow(item_codes != 0) | nrow(matching.check) != 0) break

rm(matching.check, item_codes)



# 5.3.   Matching Fuel Concordance ####
fuels <- read.xlsx("GTAP/Item_Fuel_Concordance_South_Africa.xlsx", colNames = FALSE)

fuels <- fuels %>%
  pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  rename(fuel = X1)%>%
  select(fuel, item_code)

energy <- filter(categories, category == "energy")%>%
  full_join(fuels)%>%
  filter(is.na(fuel) | is.na(category))

if(nrow(energy) >0) print("Warning. Watch out for energy item codes.")

rm(energy)


# 5.4.   Vector with Carbon Intensities ####
carbon_intensities_0 <- read.xlsx("GTAP/Supplementary Data/Carbon_Intensities_Full_0.xlsx", sheet = Country.Name) #2014
#carbon_intensities_0 <- read.xlsx("GTAP/Supplementary Data/Carbon_Intensities_ZAF.xlsx", sheet = Country.Name) #2017
GTAP_code            <- read_delim("GTAP/Supplementary Data/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE)

carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, - Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD)%>%
  select(GTAP, starts_with("CO2_t"))

rm(carbon_intensities_0, GTAP_code)


# 6       Transformation of Data ####

# 6.1     Anonymising Household-ID ####
household_ids <- select(household_information, hh_id)%>%
  distinct()%>%
  mutate(hh_id_new = 1:n())

household_information <- left_join(household_information, household_ids)%>%
  select(hh_id_new, everything(), -hh_id)%>%
  rename(hh_id = hh_id_new)

expenditure_information <- left_join(expenditure_information, household_ids)%>%
  select(hh_id_new, everything(), -hh_id)%>%
  rename(hh_id = hh_id_new)

rm(household_ids)

basic_household_information <- household_information %>%
  select(hh_id, hh_size, hh_weights)


# 6.2     Merging Expenditure Data and GTAP ####
expenditure_information_1 <- left_join(expenditure_information, matching, by = "item_code")%>%
  filter(GTAP != "deleted")

rm(matching)



# 6.3     Assign Households to Expenditure Bins 2, 3, 5, 10 ####
binning_0 <- expenditure_information_1 %>%
  group_by(hh_id)%>%
  mutate(hh_expenditures = sum(expenditures))%>%
  ungroup()%>%
  left_join(basic_household_information)%>%
  mutate(hh_expenditures_pc = hh_expenditures/hh_size)%>%
  select(hh_id, hh_expenditures, hh_expenditures_pc, hh_weights)%>%
  filter(!duplicated(hh_id))%>%
  mutate(Income_Group_2  = as.numeric(binning(hh_expenditures_pc, bins = 2,  method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_3  = as.numeric(binning(hh_expenditures_pc, bins = 3,  method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_5  = as.numeric(binning(hh_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_10 = as.numeric(binning(hh_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  select(hh_id, hh_expenditures, hh_expenditures_pc, starts_with("Income"))



# 6.4     Calculating Expenditure Shares on Energy/Food/Goods/Services ####
expenditures_categories_0 <- left_join(expenditure_information, categories)%>%
  filter(category != "deleted" & category != "in-kind" & category != "self-produced")%>%
  group_by(hh_id, category)%>%
  summarise(expenditures_category = sum(expenditures))%>%
  ungroup()%>%
  group_by(hh_id)%>%
  mutate(share_category = expenditures_category/sum(expenditures_category))%>%
  ungroup()%>%
  select(hh_id, category, share_category)%>%
  pivot_wider(names_from = "category", values_from = "share_category", names_prefix = "share_", values_fill = 0)

rm(categories)


# 6.5     Calculating Expenditure Shares on detailed Energy Items ####
expenditures_fuels <- left_join(expenditure_information, fuels)%>%
  filter(!is.na(fuel))%>%
  group_by(hh_id, fuel)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  pivot_wider(names_from = "fuel", values_from = "expenditures", names_prefix = "exp_LCU_")

rm(expenditure_information, fuels)


# 6.6     Summarizing Expenditures on the GTAP Level ####

# Exchange Rates ####
information.ex <- read.xlsx("GTAP/Supplementary Data/Exchange_Rates.xlsx") # from World Bank
exchange.rate  <- as.numeric(information.ex$exchange_rate[information.ex$Country == Country.Name]) # not ppp-adjusted

expenditure_information_1 <- expenditure_information_1 %>%
  group_by(hh_id, GTAP)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  mutate(expenditures_USD_2014 = expenditures*exchange.rate)%>%
  group_by(hh_id)%>%
  mutate(hh_expenditures_USD_2014 = sum(expenditures_USD_2014))%>%
  ungroup()

expenditure_information_2 <- expenditure_information_1 %>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_LCU = sum(expenditures))%>%
  ungroup()

rm(exchange.rate, information.ex, basic_household_information)


# 6.7     Merging Expenditures and Carbon Intensities ####
household_carbon_footprint <- left_join(expenditure_information_1, carbon_intensities, by = "GTAP")%>%
  filter(GTAP != "other")%>%
  mutate(CO2_t_global      = expenditures_USD_2014*CO2_t_per_dollar_global,
         CO2_t_national    = expenditures_USD_2014*CO2_t_per_dollar_national,
         CO2_t_electricity = expenditures_USD_2014*CO2_t_per_dollar_electricity,
         CO2_t_transport   = expenditures_USD_2014*CO2_t_per_dollar_transport)%>%
  select(-starts_with("CO2_t_per"))%>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_USD_2014 = first(hh_expenditures_USD_2014),
            CO2_t_global      = sum(CO2_t_global),    
            CO2_t_national    = sum(CO2_t_national),  
            CO2_t_electricity = sum(CO2_t_electricity),
            CO2_t_transport   = sum(CO2_t_transport))%>%
  ungroup()



# 6.8 Descriptive Statistics ####
histogram(household_carbon_footprint$CO2_t_national)
boxplot(household_carbon_footprint$CO2_t_national) 
summary(household_carbon_footprint$CO2_t_national)


# Show Households over 150 t/a
hh_id_high_carbon_footprint <- household_carbon_footprint %>%
  filter(CO2_t_national > 150) %>%
  select(hh_id)

high_c_f_categories <- expenditures_categories_0 %>%
  filter(hh_id %in% hh_id_high_carbon_footprint$hh_id)

high_c_f_expenditures <- expenditure_information_1 %>%
  filter(hh_id %in% hh_id_high_carbon_footprint$hh_id)

high_c_f_binning <- binning_0 %>%
  filter(hh_id %in% hh_id_high_carbon_footprint$hh_id)

rm(high_c_f_binning, high_c_f_expenditures, high_c_f_categories, hh_id_high_carbon_footprint, carbon_intensities, expenditure_information_1)


# 7       Model / Calculating Carbon Incidence ####

# Analysis of Carbon Pricing Incidence
household_carbon_incidence <- household_carbon_footprint %>%
  mutate(exp_CO2_global              = CO2_t_global*carbon.price,
         exp_CO2_national            = CO2_t_national*carbon.price,
         exp_CO2_electricity         = CO2_t_electricity*carbon.price,
         exp_CO2_transport           = CO2_t_transport*carbon.price)%>%
  mutate(burden_CO2_global           = exp_CO2_global/     hh_expenditures_USD_2014,
         burden_CO2_national         = exp_CO2_national/   hh_expenditures_USD_2014,
         burden_CO2_electricity      = exp_CO2_electricity/hh_expenditures_USD_2014,
         burden_CO2_transport        = exp_CO2_transport/  hh_expenditures_USD_2014)

final_incidence_information <- household_carbon_incidence %>%
  left_join(binning_0)%>%
  left_join(expenditures_categories_0)%>%
  left_join(expenditures_fuels)

if(max(final_incidence_information$CO2_t_global) == "Inf") "Warning! Check Intensities."

if(max(final_incidence_information$CO2_t_global) == "Inf") break


# Define Output Directory
write_csv(final_incidence_information, sprintf("LCS_results/Carbon_Pricing_Incidence_%s.csv",  Country.Name))
write_csv(household_information,       sprintf("LCS_results/household_information_%s.csv", Country.Name))
write_csv(left_join(expenditures_fuels, expenditure_information_2), sprintf("LCS_results/fuel_expenditures_%s.csv", Country.Name))

# Clean Environment
rm(list=ls())


