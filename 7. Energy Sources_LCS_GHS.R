library(dplyr)
library(tidyverse)
library(haven)
library(Hmisc)
library(openxlsx)
library(ggplot2)
library(stringr)

# Differences in energy supply between LCS 2014/2015 and GHS 2021 - Visualization ####

#### CHANGE PATH ####
# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Code GitHub")


# 1. LCS data ####
household_information_lcs <- read.csv("LCS_results/hh_final_LCS.csv")

household_information_lcs <- household_information_lcs %>%
  select(hh_id, Income_Group_10, province, electricitycon, electricitymains, electricitysource, cookingsource, lightingsource, heatingsourcewater, heatingsourcespace)


# 2. GHS data ####
house = read_dta("GHS 2021/ghs-2021-v1/ghs-2021-hhold-v1.dta")


# 2.1 Get Variables from GHS ####

# Get variables from household data
household_information_ghs <- house %>%
  add_count (uqnr) %>%
  select (uqnr, prov, eng_access, eng_mainelect, eng_mains, eng_cook, eng_light, eng_wheat, eng_sheat, hwl_assets_geyser) %>%
  rename (hh_id = uqnr, province = prov, electricitycon = eng_access , electricitymains = eng_mains, electricitysource = eng_mainelect,  
          cookingsource = eng_cook, lightingsource = eng_light, heatingsourcewater = eng_wheat, heatingsourcespace = eng_sheat, geysers = hwl_assets_geyser) %>%
  distinct(hh_id, .keep_all = TRUE)


# Anonymising Household-ID
household_ids <- select(household_information_ghs, hh_id)%>%
  distinct()%>%
  mutate(hh_id_new = 1:n())

household_information_ghs <- left_join(household_information_ghs, household_ids)%>%
  select(hh_id_new, everything(), -hh_id)%>%
  rename(hh_id = hh_id_new)

rm(household_ids, house)


# 3. Renaming Variables ####

# Province #
household_information_lcs <- household_information_lcs %>%
  mutate(province = case_when (province == 1 ~'Western Cape',
                               province == 2 ~'Eastern Cape',
                               province == 3 ~'Northern Cape',
                               province == 4 ~'Free State',
                               province == 5 ~'KWaZulu-Natal',
                               province == 6 ~'North West',
                               province == 7 ~'Gauteng',
                               province == 8 ~'Mpumalanga',
                               province == 9 ~'Limpopo'))

household_information_lcs$province <- factor(household_information_lcs$province, 
                                             levels = c("Western Cape", "Eastern Cape", "Northern Cape", "Free State", "KWaZulu-Natal", "North West", 
                                                        "Gauteng", "Mpumalanga", "Limpopo"))


household_information_ghs <- household_information_ghs %>%
  mutate(province = case_when (province == 1 ~'Western Cape',
                               province == 2 ~'Eastern Cape',
                               province == 3 ~'Northern Cape',
                               province == 4 ~'Free State',
                               province == 5 ~'KWaZulu-Natal',
                               province == 6 ~'North West',
                               province == 7 ~'Gauteng',
                               province == 8 ~'Mpumalanga',
                               province == 9 ~'Limpopo'))

household_information_ghs$province <- factor(household_information_ghs$province, 
                                             levels = c("Western Cape", "Eastern Cape", "Northern Cape", "Free State", "KWaZulu-Natal", "North West", 
                                                        "Gauteng", "Mpumalanga", "Limpopo"))


# Electricity Access #

household_information_lcs1 <- household_information_lcs %>%
  filter(!electricitycon == 9)%>% #unspecified
  mutate(electricitycon = case_when (electricitycon ==1 ~ 'Access',
                                     electricitycon ==2 ~ 'No Access'))

household_information_lcs1$electricitycon <- factor(household_information_lcs1$electricitycon, levels = c('No Access', 'Access'))


household_information_ghs1 <- household_information_ghs %>%
  filter(!electricitycon == 9)%>% #unspecified
  mutate(electricitycon = case_when (electricitycon ==1 ~ 'Access',
                                     electricitycon ==2 ~ 'No Access'))

household_information_ghs1$electricitycon <- factor(household_information_ghs1$electricitycon, levels = c('No Access', 'Access'))

colors_elec_access <- c('#606060', '#330000')
         


# Electricity Source #

#LCS
household_information_lcs2 <- household_information_lcs %>%
  mutate(electricitysource = case_when (electricitymains == 1 ~ 'Mains',
                                        electricitysource == 1 ~'Other', #other source (e.g. connected to neighbour's line and paying neighbour, paying landlord, etc.)
                                        electricitysource == 2 ~'Other', #other source (e.g. connected to neighbour'sline and not paying neighbour, etc.)
                                        electricitysource == 3 ~'Generator',
                                        electricitysource == 4 ~'Solar Energy',
                                        electricitysource == 6 ~ 'Other')) %>% #other unspecified
  filter(!electricitysource == 5)%>% #Battery - scarely represented
  filter(!electricitysource == 8)%>% #not applicable
  filter(!electricitysource == 9) #unspecified

household_information_lcs2$electricitysource <- factor(household_information_lcs2$electricitysource, 
                                                       levels = c( 'Generator', 'Solar Energy','Other', 'Mains'))


#GHS
household_information_ghs2 <- household_information_ghs %>%
  mutate(electricitysource = case_when (electricitymains == 1 ~ 'Mains', 
                                        electricitysource == 1 ~'Other', #In-house conventional meter
                                        electricitysource == 2 ~'Other', #In-house pre-paid meter
                                        electricitysource == 3 ~'Other', #other source (e.g. connected to neighbour's line and paying neighbour, paying landlord, etc.)
                                        electricitysource == 4 ~'Other', #other source (e.g. connected to neighbour'sline and not paying neighbour, etc.)
                                        electricitysource == 5 ~'Generator',
                                        electricitysource == 6 ~ 'Solar Energy', #Other source of main electricity supply
                                        electricitysource == 7 ~ 'Other')) %>%
  filter(!electricitysource == 8) #not applicable

household_information_ghs2$electricitysource <- factor(household_information_ghs2$electricitysource, 
                                                       levels = c('Generator', 'Solar Energy', 'Other', 'Mains'))

colors_elec_source <- c('#6a3d9a', '#FFC125', '#A61212', '#620F0F')


# Cooking Fuel #

#LCS
household_information_lcs3 <- household_information_lcs %>%
  filter(!cookingsource == 99) %>% #unspecified
  filter(!cookingsource == 11) %>% #no access
  mutate(cookingsource = case_when (cookingsource == 1 ~'Electricity from Mains',
                                    cookingsource == 2 ~'Other Source of Electricity', #e.g. generator, etc.
                                    cookingsource == 3 ~'Gas',
                                    cookingsource == 4 ~'Paraffin',
                                    cookingsource == 5 ~'Wood',
                                    cookingsource == 6 ~'Coal',
                                    cookingsource == 8 ~'Animal Dung',
                                    cookingsource == 9 ~'Solar Energy',
                                    cookingsource == 10 ~'Other'))

household_information_lcs3$cookingsource <- factor(household_information_lcs3$cookingsource, 
                                                   levels = c('Other', 'Solar Energy', 'Paraffin', 'Animal Dung', 
                                                              'Gas', 'Coal', 'Wood', 'Other Source of Electricity','Electricity from Mains'))

#GHS
household_information_ghs3 <- household_information_ghs %>% 
  mutate(cookingsource = case_when (cookingsource == 1 ~'Electricity from Mains',
                                    cookingsource == 2 ~'Other Source of Electricity', #e.g. generator, etc.
                                    cookingsource == 3 ~'Gas',
                                    cookingsource == 4 ~'Paraffin',
                                    cookingsource == 5 ~'Wood',
                                    cookingsource == 6 ~'Coal',
                                    cookingsource == 8 ~'Animal Dung',
                                    cookingsource == 9 ~'Solar Energy',
                                    cookingsource == 11 ~'Other'))

household_information_ghs3$cookingsource <- factor(household_information_ghs3$cookingsource, 
                                                   levels = c('Other', 'Solar Energy', 'Paraffin', 'Animal Dung', 
                                                              'Gas', 'Coal', 'Wood','Other Source of Electricity', 'Electricity from Mains'))

colors_cooking_source <- c('#b2df8a', '#FFC125', '#6666FF', '#b15928', '#1f78b4', '#030303', '#33a02c','#A61212', '#620F0F')


# Lighting Fuel #

#LCS
household_information_lcs4 <- household_information_lcs %>%
  filter(!lightingsource == 99) %>% #unspecified
  mutate(lightingsource = case_when (lightingsource == 1 ~'Electricity from Mains',
                                     lightingsource == 2 ~'Other Source of Electricity', #e.g. generator, etc.
                                     lightingsource == 3 ~'Gas',
                                     lightingsource == 4 ~'Paraffin', #5,6,8 not included in data
                                     lightingsource == 7 ~'Candles',
                                     lightingsource == 9 ~'Solar Energy',
                                     lightingsource == 10 ~'Other'))

household_information_lcs4$lightingsource <- factor(household_information_lcs4$lightingsource, 
                                                    levels = c('Other', 'Solar Energy', 'Candles', 'Paraffin', 'Gas',
                                                               'Other Source of Electricity','Electricity from Mains'))

#GHS
household_information_ghs4 <- household_information_ghs %>%
  filter(!lightingsource == 10)%>% #none
  mutate(lightingsource = case_when (lightingsource == 1 ~'Electricity from Mains',
                                     lightingsource == 2 ~'Other Source of Electricity',
                                     lightingsource == 3 ~'Gas',
                                     lightingsource == 4 ~'Paraffin', #5,6,8 not included in data
                                     lightingsource == 7 ~'Candles',
                                     lightingsource == 9 ~'Solar Energy',
                                     lightingsource == 11 ~'Other'))
    
household_information_ghs4$lightingsource <- factor(household_information_ghs4$lightingsource, 
                                                    levels = c('Other', 'Solar Energy', 'Candles', 'Paraffin', 'Gas',
                                                                'Other Source of Electricity','Electricity from Mains'))

colors_lighting_source <- c('#b2df8a', '#FFC125', '#a6cee3', '#6666FF', '#1f78b4','#A61212', '#620F0F')


# Water Heating Fuel #

#LCS
household_information_lcs5 <- household_information_lcs %>%
  filter(!heatingsourcewater == 99) %>% #unspecified
  mutate(heatingsourcewater = case_when (heatingsourcewater == 1 ~'Electricity from Mains',
                                         heatingsourcewater == 2 ~'Other Source of Electricity', #e.g. generator, etc.
                                         heatingsourcewater == 3 ~'Gas',
                                         heatingsourcewater == 4 ~'Paraffin',
                                         heatingsourcewater == 5 ~'Wood',
                                         heatingsourcewater == 6 ~'Coal', #7 not included in data
                                         heatingsourcewater == 8 ~'Animal Dung',
                                         heatingsourcewater == 9 ~'Solar Energy',
                                         heatingsourcewater == 10 ~'Other',
                                         heatingsourcewater == 11 ~'No Access'))

household_information_lcs5$heatingsourcewater <- factor(household_information_lcs5$heatingsourcewater, 
                                                        levels = c('No Access', 'Other', 'Solar Energy', 'Paraffin', 'Animal Dung', 'Gas',
                                                                   'Coal', 'Wood','Other Source of Electricity', 'Electricity from Mains'))

#GHS
household_information_ghs5 <- household_information_ghs %>% 
  filter(!heatingsourcewater == 99) %>% #unspecified
  mutate(heatingsourcewater = case_when (heatingsourcewater == 1 ~'Electricity from Mains',
                                         heatingsourcewater == 2 ~'Other Source of Electricity',
                                         heatingsourcewater == 3 ~'Gas',
                                         heatingsourcewater == 4 ~'Paraffin',
                                         heatingsourcewater == 5 ~'Wood',
                                         heatingsourcewater == 6 ~'Coal',
                                         heatingsourcewater == 8 ~'Animal Dung',
                                         heatingsourcewater == 9 ~'Solar Energy',
                                         heatingsourcewater == 10 ~'No Access',
                                         heatingsourcewater == 11 ~'Other'))

household_information_ghs5$heatingsourcewater <- factor(household_information_ghs5$heatingsourcewater, 
                                                       levels = c('No Access', 'Other', 'Solar Energy', 'Paraffin', 'Animal Dung', 'Gas',
                                                                  'Coal', 'Wood','Other Source of Electricity', 'Electricity from Mains'))

colors_wheating_source <- c('#606060', '#b2df8a', '#FFC125', '#6666FF', '#b15928', '#1f78b4', '#030303', '#33a02c','#A61212', '#620F0F')


# Space Heating Fuel #

#LCS
household_information_lcs6 <- household_information_lcs %>%
  filter(!heatingsourcespace == 99) %>% #unspecified
  mutate(heatingsourcespace = case_when (heatingsourcespace == 1 ~'Electricity from Mains',
                                         heatingsourcespace == 2 ~'Other Source of Electricity', #e.g. generator, etc.
                                         heatingsourcespace == 3 ~'Gas',
                                         heatingsourcespace == 4 ~'Paraffin',
                                         heatingsourcespace == 5 ~'Wood',
                                         heatingsourcespace == 6 ~'Coal', #7 not included in data
                                         heatingsourcespace == 8 ~'Animal Dung',
                                         heatingsourcespace == 9 ~'Solar Energy',
                                         heatingsourcespace == 10 ~'Other',
                                         heatingsourcespace == 11 ~'No Access'))

household_information_lcs6$heatingsourcespace <- factor(household_information_lcs6$heatingsourcespace, 
                                                        levels = c('No Access', 'Other', 'Solar Energy', 'Paraffin', 'Animal Dung', 'Gas',
                                                                   'Coal', 'Wood','Other Source of Electricity', 'Electricity from Mains'))

#GHS
household_information_ghs6 <- household_information_ghs %>% 
  mutate(heatingsourcespace = case_when (heatingsourcespace == 1 ~'Electricity from Mains',
                                         heatingsourcespace == 2 ~'Other Source of Electricity',
                                         heatingsourcespace == 3 ~'Gas',
                                         heatingsourcespace == 4 ~'Paraffin',
                                         heatingsourcespace == 5 ~'Wood',
                                         heatingsourcespace == 6 ~'Coal', #7 not included in data
                                         heatingsourcespace == 8 ~'Animal Dung',
                                         heatingsourcespace == 9 ~'Solar Energy',
                                         heatingsourcespace == 10 ~'No Access',
                                         heatingsourcespace == 11 ~'Other'))

household_information_ghs6$heatingsourcespace <- factor(household_information_ghs6$heatingsourcespace, 
                                                        levels = c('No Access', 'Other', 'Solar Energy', 'Paraffin', 'Animal Dung', 'Gas',
                                                                   'Coal', 'Wood','Other Source of Electricity', 'Electricity from Mains'))

colors_sheating_source <- c('#606060', '#b2df8a', '#FFC125', '#6666FF', '#b15928', '#1f78b4', '#030303', '#33a02c','#A61212', '#620F0F')


# 4. Visualization ####

# 4.1. Electricity Access ####

#LCS
elec_access_share_lcs <- household_information_lcs1 %>%
  select (province, electricitycon)%>%
  group_by(province, electricitycon) %>%
  summarise(amount = n())

elec_access_share_lcs <- transform(elec_access_share_lcs, perc = ave(amount,province, FUN=prop.table)) 

elec_access_share_lcs <- elec_access_share_lcs %>%
  select(province, electricitycon, perc)

ggplot(elec_access_share_lcs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = electricitycon)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_elec_access)+
  labs (x = "Provinces", y = "Share of Electricity Access (%)", title = "Share of Electricity Access for different Provinces - LCS 2014/15", fill = "Electricity Access") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#GHS
elec_access_share_ghs <- household_information_ghs1 %>%
  select (province, electricitycon)%>%
  group_by(province, electricitycon) %>%
  summarise(amount = n())

elec_access_share_ghs <- transform(elec_access_share_ghs, perc = ave(amount,province, FUN=prop.table)) 

elec_access_share_ghs <- elec_access_share_ghs %>%
  select(province, electricitycon, perc)

ggplot(elec_access_share_ghs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = electricitycon)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_elec_access)+
  labs (x = "Provinces", y = "Share of Electricity Access (%)", title = "Share of Electricity Access for different Provinces - GHS 2021", fill = "Electricity Access") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



# 4.2. Electricity Source ####

#LCS
elec_source_share_lcs <- household_information_lcs2 %>%
  select (province, electricitysource)%>%
  group_by(province, electricitysource) %>%
  summarise(amount = n())

elec_source_share_lcs <- transform(elec_source_share_lcs, perc = ave(amount,province, FUN=prop.table)) 

elec_source_share_lcs <- elec_source_share_lcs %>%
  select(province, electricitysource, perc)

ggplot(elec_source_share_lcs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = electricitysource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_elec_source)+
  labs (x = "Provinces", y = "Share of Electricity Source (%)", title = "Share of Electricity Source for different Provinces - LCS 2014/15", fill = "Electricity Source") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#GHS
elec_source_share_ghs <- household_information_ghs2 %>%
  select (province, electricitysource)%>%
  group_by(province, electricitysource) %>%
  summarise(amount = n())

elec_source_share_ghs <- transform(elec_source_share_ghs, perc = ave(amount,province, FUN=prop.table)) 

elec_source_share_ghs <- elec_source_share_ghs %>%
  select(province, electricitysource, perc)

ggplot(elec_source_share_ghs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = electricitysource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_elec_source)+
  labs (x = "Provinces", y = "Share of Electricity Source (%)", title = "Share of Electricity Source for different Provinces - GHS 2021", fill = "Electricity Source") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# 4.3. Cooking Fuel ####

#LCS
cooking_source_share_lcs <- household_information_lcs3 %>%
  select (province, cookingsource)%>%
  group_by(province, cookingsource) %>%
  summarise(amount = n())

cooking_source_share_lcs <- transform(cooking_source_share_lcs, perc = ave(amount,province, FUN=prop.table)) 

cooking_source_share_lcs <- cooking_source_share_lcs %>%
  select(province, cookingsource, perc)

ggplot(cooking_source_share_lcs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = cookingsource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_cooking_source)+
  labs (x = "Provinces", y = "Share of Cooking Fuel (%)", title = "Share of Cooking Fuel for different Provinces - LCS 2014/15", fill = "Cooking Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#GHS
cooking_source_share_ghs <- household_information_ghs3 %>%
  select (province, cookingsource)%>%
  group_by(province, cookingsource) %>%
  summarise(amount = n())

cooking_source_share_ghs <- transform(cooking_source_share_ghs, perc = ave(amount,province, FUN=prop.table)) 

cooking_source_share_ghs <- cooking_source_share_ghs %>%
  select(province, cookingsource, perc)

ggplot(cooking_source_share_ghs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = cookingsource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_cooking_source)+
  labs (x = "Provinces", y = "Share of Cooking Fuel (%)", title = "Share of Cooking Fuel for different Provinces - GHS 2021", fill = "Cooking Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# 4.4. Lighting Fuel ####

#LCS
lighting_source_share_lcs <- household_information_lcs4 %>%
  select (province, lightingsource)%>%
  group_by(province, lightingsource) %>%
  summarise(amount = n())

lighting_source_share_lcs <- transform(lighting_source_share_lcs, perc = ave(amount,province, FUN=prop.table)) 

lighting_source_share_lcs <- lighting_source_share_lcs %>%
  select(province, lightingsource, perc)

ggplot(lighting_source_share_lcs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = lightingsource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_lighting_source)+
  labs (x = "Provinces", y = "Share of Lighting Fuel (%)", title = "Share of Lighting Fuel for different Provinces - LCS 2014/15", fill = "Lighting Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#GHS
lighting_source_share_ghs <- household_information_ghs4 %>%
  select (province, lightingsource)%>%
  group_by(province, lightingsource) %>%
  summarise(amount = n())

lighting_source_share_ghs <- transform(lighting_source_share_ghs, perc = ave(amount,province, FUN=prop.table)) 

lighting_source_share_ghs <- lighting_source_share_ghs %>%
  select(province, lightingsource, perc)

ggplot(lighting_source_share_ghs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = lightingsource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_lighting_source)+
  labs (x = "Provinces", y = "Share of Lighting Fuel (%)", title = "Share of Lighting Fuel for different Provinces - GHS 2021", fill = "Lighting Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# 4.5. Water Heating Fuel ####

#LCS
wheating_source_share_lcs <- household_information_lcs5 %>%
  select (province, heatingsourcewater)%>%
  group_by(province, heatingsourcewater) %>%
  summarise(amount = n())

wheating_source_share_lcs <- transform(wheating_source_share_lcs, perc = ave(amount,province, FUN=prop.table)) 

wheating_source_share_lcs <- wheating_source_share_lcs %>%
  select(province, heatingsourcewater, perc)

ggplot(wheating_source_share_lcs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = heatingsourcewater)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_wheating_source)+
  labs (x = "Provinces", y = "Share of Water Heating Fuel (%)", title = "Share of Water Heating Fuel for different Provinces - LCS 2014/15", fill = "Water Heating Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#GHS
wheating_source_share_ghs <- household_information_ghs5 %>%
  select (province, heatingsourcewater)%>%
  group_by(province, heatingsourcewater) %>%
  summarise(amount = n())

wheating_source_share_ghs <- transform(wheating_source_share_ghs, perc = ave(amount,province, FUN=prop.table)) 

wheating_source_share_ghs <- wheating_source_share_ghs %>%
  select(province, heatingsourcewater, perc)

ggplot(wheating_source_share_ghs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = heatingsourcewater)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_wheating_source)+
  labs (x = "Provinces", y = "Share of Water Heating Fuel (%)", title = "Share of Water Heating Fuel for different Provinces - GHS 2021", fill = "Water Heating Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



# 4.6. Space Heating Fuel ####

#LCS
sheating_source_share_lcs <- household_information_lcs6 %>%
  select (province, heatingsourcespace)%>%
  group_by(province, heatingsourcespace) %>%
  summarise(amount = n())

sheating_source_share_lcs <- transform(sheating_source_share_lcs, perc = ave(amount,province, FUN=prop.table)) 

sheating_source_share_lcs <- sheating_source_share_lcs %>%
  select(province, heatingsourcespace, perc)

ggplot(sheating_source_share_lcs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = heatingsourcespace)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_sheating_source)+
  labs (x = "Provinces", y = "Share of Space Heating Fuel (%)", title = "Share of Space Heating Fuel for different Provinces - LCS 2014/15", fill = "Space Heating Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#GHS
sheating_source_share_ghs <- household_information_ghs6 %>%
  select (province, heatingsourcespace)%>%
  group_by(province, heatingsourcespace) %>%
  summarise(amount = n())

sheating_source_share_ghs <- transform(sheating_source_share_ghs, perc = ave(amount,province, FUN=prop.table)) 

sheating_source_share_ghs <- sheating_source_share_ghs %>%
  select(province, heatingsourcespace, perc)

ggplot(sheating_source_share_ghs, aes(x = str_wrap(province, width = 10), y = (perc*100), fill = heatingsourcespace)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_sheating_source)+
  labs (x = "Provinces", y = "Share of Space Heating Fuel (%)", title = "Share of Space Heating Fuel for different Provinces - GHS 2021", fill = "Space Heating Fuell") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 





#### Expenditure Deciles ###

elec_access_share_lcs <- household_information_lcs1 %>%
  select (Income_Group_10, electricitycon)%>%
  group_by(Income_Group_10, electricitycon) %>%
  summarise(amount = n())

elec_access_share_lcs <- transform(elec_access_share_lcs, perc = ave(amount,Income_Group_10, FUN=prop.table)) 

elec_access_share_lcs <- elec_access_share_lcs %>%
  select(Income_Group_10, electricitycon, perc)

ggplot(elec_access_share_lcs, aes(x = factor(Income_Group_10, levels = 1:10), y = (perc*100), fill = electricitycon)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_elec_access)+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  labs (x = "Expenditure Decile", y = "Share of Electricity Access (%)", title = "Share of Electricity Access by Expenditures - LCS 2014/15", fill = "Electricity Access") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))



elec_source_share_lcs <- household_information_lcs2 %>%
  select (Income_Group_10, electricitysource)%>%
  group_by(Income_Group_10, electricitysource) %>%
  summarise(amount = n())

elec_source_share_lcs <- transform(elec_source_share_lcs, perc = ave(amount,Income_Group_10, FUN=prop.table)) 

elec_source_share_lcs <- elec_source_share_lcs %>%
  select(Income_Group_10, electricitysource, perc)

ggplot(elec_source_share_lcs, aes(x = factor(Income_Group_10, levels = 1:10), y = (perc*100), fill = electricitysource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_elec_source)+
  labs (x = "Expenditure Decile", y = "Share of Electricity Source (%)", title = "Share of Electricity Source by Expenditures - LCS 2014/15", fill = "Electricity Source") +
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm")) 



cooking_source_share_lcs <- household_information_lcs3 %>%
  select (Income_Group_10, cookingsource)%>%
  group_by(Income_Group_10, cookingsource) %>%
  summarise(amount = n())

cooking_source_share_lcs <- transform(cooking_source_share_lcs, perc = ave(amount,Income_Group_10, FUN=prop.table)) 

cooking_source_share_lcs <- cooking_source_share_lcs %>%
  select(Income_Group_10, cookingsource, perc)

ggplot(cooking_source_share_lcs, aes(x = factor(Income_Group_10, levels = 1:10), y = (perc * 100), fill = cookingsource)) +
  geom_bar(position = "stack", stat = "identity", color = "black") +
  scale_fill_manual(values = colors_cooking_source) +
  labs(x = "Expenditure Decile", y = "Share of Cooking Fuel (%)", title = "Share of Cooking Fuel by Expenditures - LCS 2014/15", fill = "Cooking Fuel") +
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = seq(10, 90, 10), linetype = "dotted", color = "black") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))



lighting_source_share_lcs <- household_information_lcs4 %>%
  select (Income_Group_10, lightingsource)%>%
  group_by(Income_Group_10, lightingsource) %>%
  summarise(amount = n())

lighting_source_share_lcs <- transform(lighting_source_share_lcs, perc = ave(amount,Income_Group_10, FUN=prop.table)) 

lighting_source_share_lcs <- lighting_source_share_lcs %>%
  select(Income_Group_10, lightingsource, perc)

ggplot(lighting_source_share_lcs, aes(x = factor(Income_Group_10, levels = 1:10), y = (perc*100), fill = lightingsource)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_lighting_source)+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  labs (x = "Expenditure Decile", y = "Share of Lighting Fuel (%)", title = "Share of Lighting Fuel by Expenditure - LCS 2014/15", fill = "Lighting Fuel") +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm")) 





wheating_source_share_lcs <- household_information_lcs5 %>%
  select (Income_Group_10, heatingsourcewater)%>%
  group_by(Income_Group_10, heatingsourcewater) %>%
  summarise(amount = n())

wheating_source_share_lcs <- transform(wheating_source_share_lcs, perc = ave(amount,Income_Group_10, FUN=prop.table)) 

wheating_source_share_lcs <- wheating_source_share_lcs %>%
  select(Income_Group_10, heatingsourcewater, perc)

ggplot(wheating_source_share_lcs, aes(x = factor(Income_Group_10, levels = 1:10), y = (perc*100), fill = heatingsourcewater)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_wheating_source)+
  labs (x = "Expenditure Decile", y = "Share of Water Heating Fuel (%)", title = "Share of Water Heating Fuel by Expenditures - LCS 2014/15", fill = "Water Heating Fuel") +
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm")) 


sheating_source_share_lcs <- household_information_lcs6 %>%
  select (Income_Group_10, heatingsourcespace)%>%
  group_by(Income_Group_10, heatingsourcespace) %>%
  summarise(amount = n())

sheating_source_share_lcs <- transform(sheating_source_share_lcs, perc = ave(amount,Income_Group_10, FUN=prop.table)) 

sheating_source_share_lcs <- sheating_source_share_lcs %>%
  select(Income_Group_10, heatingsourcespace, perc)

ggplot(sheating_source_share_lcs, aes(x = factor(Income_Group_10, levels = 1:10), y = (perc*100), fill = heatingsourcespace)) +
  geom_bar(position="stack", stat="identity", color="black") +
  scale_fill_manual(values = colors_sheating_source)+
  labs (x = "Expenditure Decile", y = "Share of Space Heating Fuel (%)", title = "Share of Space Heating Fuel by Expenditures - LCS 2014/15", fill = "Space Heating Fuel") +
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5), labels = function(x) paste0(x, "%"))+
  geom_hline(yintercept = seq(10, 90, 10), linetype="dotted", color = "black") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm")) 
