## 6. Random Forest and Household Cluster ####

library(ggpubr)
library(data.table)
library(doFuture)
library(tidyverse)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
# library(ggpmisc)
library(cluster)
library(GGally)
library(tidymodels)
library(kernelshap)
library(shapviz)
library("ggsci")
library("Hmisc")


#### CHANGE PATH ####
# Set working directory Agatha
# etwd("C:/Users/agath/Desktop/Code GitHub/LCS_results")

# Load data 1 - LCS
household_all <- read_csv("LCS_results/hh_final_LCS.csv")

household_all <- household_all %>% 
  select(hh_id, hh_expenditures_USD_2014, hh_weights, hh_size,
         # exp_CO2_national, 
         burden_CO2_national, 
         province, urban_1, 
         electricitycon, electricitymains,
         electricitysource, electricityfree, 
         lightingsource, cookingsource, heatingsourcewater, heatingsourcespace, 
         education_hhh, gender_hhh, ethnicity_hhh, own_stove, own_vehicle)%>%
  mutate(log_expenditures = log(hh_expenditures_USD_2014))

# 6.1 Pre-Processing - transforming data with recipes and splitting data ####

household_all_0 <- household_all %>%
  mutate(province = ifelse(province == '1', "Western Cape", 
                           ifelse(province == "2", 'Eastern Cape',  
                                  ifelse(province == "3", 'Northern Cape',  
                                         ifelse(province == "4", 'Free State', 
                                                ifelse(province == "5", 'Kwazulu Natal', 
                                                       ifelse(province == "6", 'North-West', 
                                                              ifelse(province == "7", 'Gauteng', 
                                                                     ifelse(province == "8", 'Mpumalanga', 
                                                                            ifelse(province == "9", 'Limpopo', NA))))))))))%>%
  mutate(gender_hhh = ifelse(gender_hhh == 1, "Male", "Female"))%>%
  mutate(ethnicity_hhh = case_when(ethnicity_hhh == 1 ~ "African-black",
                                   ethnicity_hhh == 2 ~ "Coloured",
                                   ethnicity_hhh == 3 ~ "Indian/Asian",
                                   ethnicity_hhh == 4 ~ "White"))%>%
  mutate(education_hhh = case_when(education_hhh < 9  ~ "1", 
                                   education_hhh < 15 ~ "2", 
                                   education_hhh < 25 ~ "3",
                                   education_hhh < 27 ~ "4",
                                   education_hhh < 29 ~ "6",
                                   education_hhh < 30 ~ "7",
                                   education_hhh < 31 ~ "8",
                                   education_hhh > 30 ~ "9"))%>%
  mutate(car.01  = ifelse(own_vehicle == 1,1,0))%>%
  mutate(stove.01 = ifelse(own_stove   == 1,1,0))%>%
  mutate(electricity = case_when(electricitymains == 1  ~ "Mains",
                                 electricitysource == 4 ~ "Solar",
                                 electricitycon == 2    ~ "No access",
                                 TRUE ~ "Other"))%>%
  mutate(electricity_free = ifelse(electricityfree == 1, "Free electricity", "No free electricity"))%>%
  mutate(LF = case_when(lightingsource == 1 | lightingsource == 2 ~ "Electricity",
                        lightingsource == 4  ~ "Gas",
                        lightingsource == 7 ~ "Candles",
                        lightingsource == 9 ~ "Solar",
                        TRUE ~ "Other"))%>%
  mutate(CF = case_when(cookingsource == 1 | cookingsource == 2 ~ "Electricity",
                        cookingsource == 3 ~ "Gas",
                        cookingsource == 4 ~ "Paraffin",
                        cookingsource == 5 ~ "Wood",
                        cookingsource == 6 ~ "Coal",
                        cookingsource == 8 ~ "Biomass",
                        cookingsource == 9 ~ "Solar energy",
                        TRUE ~ "Other"))%>%
  mutate(HFW = case_when(heatingsourcewater == 1 | heatingsourcewater == 2 ~ "Electricity",
                        heatingsourcewater == 3 ~ "Gas",
                        heatingsourcewater == 4 ~ "Paraffin",
                        heatingsourcewater == 5 ~ "Wood",
                        heatingsourcewater == 6 ~ "Coal",
                        heatingsourcewater == 8 ~ "Biomass",
                        heatingsourcewater == 9 ~ "Solar energy",
                        TRUE ~ "Other"))%>%
  mutate(HFS = case_when(heatingsourcespace == 1 | heatingsourcespace == 2 ~ "Electricity",
                         heatingsourcespace == 3 ~ "Gas",
                         heatingsourcespace == 4 ~ "Paraffin",
                         heatingsourcespace == 5 ~ "Wood",
                         heatingsourcespace == 6 ~ "Coal",
                         heatingsourcespace == 8 ~ "Biomass",
                         heatingsourcespace == 9 ~ "Solar energy",
                         TRUE ~ "Other"))%>%
  mutate(urban_01 = ifelse(urban_1 == 1 | urban_1 == 2, "Urban", "Rural"))%>%
  select(-own_vehicle, -own_stove, -electricitymains, -electricitysource, -electricitycon, -lightingsource, -cookingsource,
         -heatingsourcewater, -heatingsourcespace, -log_expenditures, -electricityfree, -urban_1)

household_all_1 <- household_all_0 %>%
  select(-hh_id, -hh_weights, -hh_size)

set.seed(2023)

# split in train/test
sample_split <- initial_split(household_all_1, prop = 0.75)
# train_set <- subset(x = household2, sample_split == TRUE)
train_set <- training(sample_split)
# test_set <- subset(x = household2, sample_split == FALSE)
test_set <- testing(sample_split)

recipe_0 <- recipe(burden_CO2_national ~ ., data = train_set)%>%
  step_filter_missing(all_predictors(), threshold = 0)%>%
  step_other(all_nominal(), threshold = 0.05)%>%
  step_dummy(all_nominal())

training_set <- recipe_0 %>%
  prep(training = train_set)%>%
  bake(new_data = NULL)

testing_set <- recipe_0 %>%
  prep(training = test_set)%>%
  bake(new_data = NULL)

folds_set <- vfold_cv(training_set, v = 5)

# 6.2 Tuning the model ####

tune_model <- rand_forest(
  mtry  = tune(),
  trees = 1000,
  min_n = tune(),
)%>%
  set_mode("regression")%>%
  set_engine("ranger")

grid_0 <- grid_latin_hypercube(
  min_n(),
  mtry(c(round((ncol(training_set)-1)/2,0), ncol(training_set)-1)),
  size = 30)

# Build a model for every grid point + evaluate every model out-of-sample

tree_3.2 <- tune_grid(tune_model, #untuned specification of tree
                      burden_CO2_national ~ ., # formula
                      resamples = folds_set, # folds
                      grid      = grid_0, # tuning grid
                      metrics   = metric_set(mae, rmse, rsq))

autoplot(tree_3.2)

# use the best performing parameters

tree_3.3 <- select_best(tree_3.2) # mtry 16, min_n 36

# plug best performing paramters into specification - updated specification

tree_3.4 <- finalize_model(tune_model,
                           tree_3.3)

# 6.3 Fitting the final model ####

model_rf <- rand_forest(
  trees = 1000,
  mtry  = 16,
  min_n = 36
)%>%
  set_mode("regression")%>%
  set_engine("ranger", importance = "impurity")

model_rf_1 <- model_rf %>%
  fit(burden_CO2_national ~ .,
      data = training_set)

# 6.4 Evaluating the final model ####

predictions <- augment(model_rf_1, new_data = testing_set)
rsq  <- rsq(predictions,  truth = burden_CO2_national, estimate = .pred) # 0.34 without tuning - 0.373 with tuning
mae  <- mae(predictions,  truth = burden_CO2_national, estimate = .pred)
rmse <- rmse(predictions, truth = burden_CO2_national, estimate = .pred)

# 6.5 SHAP-values ####

testing_set_0 <- testing_set %>%
  select(-burden_CO2_national)

testing_set_1 <- testing_set %>%
  sample_n(100)

doParallel::registerDoParallel()
t <- kernelshap(model_rf_1, testing_set_0, bg_X = testing_set_1, parallel = TRUE)
doParallel::stopImplicitCluster()

shp <- shapviz(t)

shap_values <- shp$S%>%
  as_tibble()%>%
  rename_at(vars(everything()), ~ str_replace(., "$","_SHAP"))

testing_set_2 <- shp$X%>%
  as_tibble()

shap_values_final <- bind_cols(shap_values, testing_set_2)

write_csv(shap_values_final, "LCS_results/SHAP_values.csv")

# 6.5 Variable importance plots ####

shap_values_0 <- read_csv("LCS_results/SHAP_values.csv")

shap_values <- shap_values_0 %>%
  select(ends_with("_SHAP"))%>%
  summarise_all(~ mean(abs(.)))%>%
  pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
  arrange(desc(SHAP_contribution))%>%
  mutate(tot_contribution = sum(SHAP_contribution))%>%
  mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
  select(-tot_contribution)

shap_values_1 <- shap_values %>%
  mutate(var_1 = ifelse(variable %in% c("hh_size", "hh_expenditures_USD_2014") | grepl("sex_hhh", variable) | grepl(".01", variable), NA, str_remove(variable, "^[^_]*")))%>%
  mutate(var_1 = str_remove(var_1, "_X"))%>%
  mutate(var_1 = str_remove(var_1, "_"))%>%
  mutate(var_1 = str_remove(var_1, "hhh_"))%>%
  mutate(var_1 = str_remove(var_1, "free_"))%>%
  mutate(var_1 = str_replace_all(var_1, "\\.", " "))%>%
  mutate(var_0 = ifelse(variable %in% c("hh_size", "hh_expenditures_USD_2014") | grepl("sex_hhh", variable) | grepl(".01", variable), str_remove(variable, "_X."), str_remove(variable, "_.*")))%>%
  select(var_0, var_1, everything(), -variable)%>%
  rename(Variable = var_0)%>%
  mutate(Var_0 = ifelse(grepl("District", Variable), "District", 
                        ifelse(grepl("province", Variable), "Province", 
                               ifelse(grepl("ISCED", Variable), "ISCED", 
                                      ifelse(grepl("ethnicity", Variable), "Ethnicity", 
                                             ifelse(grepl("Religion", Variable), "Religion", 
                                                    ifelse(Variable == "hh_expenditures_USD_2014", "HH expenditures",
                                                           ifelse(Variable == "hh_size", "HH size",
                                                                  ifelse(grepl("car.01", Variable), "Car own.",
                                                                         ifelse(grepl("urban_01", Variable), "Urban",
                                                                                ifelse(grepl("gender", Variable), "Gender HHH",
                                                                                       ifelse(grepl("CF_", Variable), "Cooking", 
                                                                                              ifelse(grepl("HF_", Variable), "Heating", 
                                                                                                     ifelse(grepl("LF_", Variable), "Lighting", 
                                                                                                            ifelse(Variable == "electricity", "Electricity access", Variable)))))))))))))))%>%
  mutate(Var_0 = ifelse(Var_0 == "religiosity", "Religiosity",
                        ifelse(Var_0 %in% c("refrigerator.01", "ac.01", "tv.01", "washing_machine.01"), "Appliance own.", 
                               ifelse(Var_0 == "motorcycle.01", "Motorcycle own.", 
                                      ifelse(Var_0 == "stove.01", "Stove own.", Var_0)))))%>%
  mutate(order_number = 1:n())%>%
  group_by(Var_0)%>%
  mutate(order_number_2 = min(order_number))%>%
  ungroup()%>%
  arrange(order_number_2, order_number)%>%
  rename(Var_1 = var_1)%>%
  select(Var_0, Var_1, everything(), - Variable)%>%
  select(-order_number, -order_number_2)

shap_values_2 <- shap_values_1 %>%
  group_by(Var_0)%>%
  summarise_at(vars(share_SHAP), ~ sum(.))%>%
  ungroup()%>%
  mutate(help_0 = ifelse(share_SHAP < 0.025,1,0))%>%
  arrange(desc(share_SHAP))%>%
  mutate(Var_0 = case_when(Var_0 == "CF"  ~ "Cooking fuel",
                           Var_0 == "HFS" ~ "Heating fuel (space)",
                           Var_0 == "HFW" ~ "Heating fuel (water)",
                           Var_0 == "LF"  ~ "Lighting fuel",
                           TRUE ~ Var_0))%>%
  mutate(order = 1/1:n())%>%
  mutate(cumsum_0 = cumsum(share_SHAP))

P_1 <- ggplot(shap_values_2)+
  geom_col(aes(x = share_SHAP, y = reorder(Var_0, order), fill = factor(help_0)), width = 0.7, colour = "black", size = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(0,0.5))+
  scale_fill_manual(values = c("#E18727FF","#6F99ADFF"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  guides(fill = "none")+
  xlab("Variable importance (SHAP)")+
  ylab("Variable")+
  # ggtitle(paste0("Cluster ", data_8.5.2.C$cluster[data_8.5.2.C$Country == i],
  #                ": ", Country.Set$Country_long[Country.Set$Country == i]), " (")+
  theme(axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 9),
        plot.title.position = "plot",
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("plots_GTAP11/Variable_Importance_ZAF.jpg", width = 15.5, height = 10, unit = "cm", res = 300)
print(P_1)
dev.off()

# sv_importance(shap_values, kind = "bar")

# 6.6 Partial dependence plots ####

data_6.6 <- shap_values_final %>%
  left_join(test_set)%>%
  filter(hh_expenditures_USD_2014 < 50000)%>%
  mutate(sd_exp   = sd(hh_expenditures_USD_2014),
         mean_exp = mean(hh_expenditures_USD_2014))%>%
  mutate(z_score  = (hh_expenditures_USD_2014 - mean_exp)/sd_exp)%>%
  # Car ownership
  mutate(car.01 = ifelse(car.01 == 0, "No car", "Owns a car"))%>%
  # Province
  mutate(province_short = case_when(province == "Gauteng" ~ "GP",
                                    province == "Free State" ~ "FS",
                                    province == "Eastern Cape" ~ "EC",
                                    province == "Kwazulu Natal" ~ "KZN",
                                    province == "Limpopo" ~ "LP",
                                    province == "Mpumalanga" ~ "MP",
                                    province == "Northern Cape" ~ "NC",
                                    province == "North-West" ~ "NW",
                                    province == "Western Cape" ~ "WC"))%>%
  mutate(province = factor(province, levels = c("Gauteng", "Free State", "Kwazulu Natal", "Limpopo", "North-West",
                                                "Mpumalanga", "Northern Cape", "Eastern Cape", "Western Cape")))%>%
  mutate(province_short = factor(province_short, levels = c("GP", "FS", "KZN", "LP", "NW",
                                                  "MP", "NC", "EC", "WC")))%>%
  mutate(province_SHAP = province_Free.State_SHAP + province_Gauteng_SHAP + province_Kwazulu.Natal_SHAP + province_Limpopo_SHAP + province_Northern.Cape_SHAP + province_Western.Cape_SHAP + province_Mpumalanga_SHAP + province_North.West_SHAP)%>%
  # mutate(province_SHAP = ifelse(province == "Free State"      , province_Free.State_SHAP, 0))%>%
  # mutate(province_SHAP = ifelse(province == "Gauteng"            , province_Gauteng_SHAP, province_SHAP))%>%
  # mutate(province_SHAP = ifelse(province == "Kwazulu Natal"      , province_Kwazulu.Natal_SHAP, province_SHAP))%>%
  # mutate(province_SHAP = ifelse(province == "Limpopo"            , province_Limpopo_SHAP, province_SHAP))%>%
  # mutate(province_SHAP = ifelse(province == "Northern Cape"      , province_Northern.Cape_SHAP, province_SHAP))%>%
  # mutate(province_SHAP = ifelse(province == "Eastern Cape"       , province_Western.Cape_SHAP, province_SHAP))%>%
  # mutate(province_SHAP = ifelse(province == "North-West"         , province_North.West_SHAP, province_SHAP))%>%
  # mutate(province_SHAP = ifelse(province == "Mpumalanga"         , province_Mpumalanga_SHAP, province_SHAP))%>%
  # Electricity
  mutate(electricity_SHAP = electricity_other_SHAP + electricity_No.access_SHAP)%>%
  #mutate(electricity_SHAP = ifelse(electricity == "Solar" | electricity == "Other", electricity_other_SHAP,0))%>%
  #mutate(electricity_SHAP = ifelse(electricity == "No access", electricity_No.access_SHAP,electricity_SHAP))%>%
  mutate(electricity = ifelse(electricity == "Solar", "Other", electricity))%>%
  mutate(electricity = factor(electricity, levels = c("Mains", "No access", "Other")))%>%
  # Ethnicity
  mutate(ethnicity_SHAP = ethnicity_hhh_White_SHAP + ethnicity_hhh_Coloured_SHAP + ethnicity_hhh_other_SHAP)%>%
  # mutate(ethnicity_SHAP = ifelse(ethnicity_hhh == "White",    ethnicity_hhh_White_SHAP, 0))%>%
  # mutate(ethnicity_SHAP = ifelse(ethnicity_hhh == "Coloured", ethnicity_hhh_Coloured_SHAP, ethnicity_SHAP))%>%
  # mutate(ethnicity_SHAP = ifelse(ethnicity_hhh == "Indian/Asian",    ethnicity_hhh_other_SHAP, ethnicity_SHAP))%>%
  mutate(ethnicity_hhh = factor(ethnicity_hhh, levels = c("African-black", "Indian/Asian", "Coloured", "White")))%>%
  # Heating fuel (space)
  mutate(HFS_SHAP = HFS_Other_SHAP + HFS_Paraffin_SHAP + HFS_Wood_SHAP + HFS_other_SHAP)%>%
  mutate(HFS = ifelse(HFS %in% c("Biomass", "Coal", "Gas", "Solar energy", "Other"), "Other", HFS))%>%
  mutate(HFS = factor(HFS, levels = c("Electricity", "Wood", "Paraffin", "Other")))
  
P_2.1 <- ggplot(filter(data_6.6, hh_expenditures_USD_2014 < 30000), 
                aes(y = hh_expenditures_USD_2014_SHAP, x = hh_expenditures_USD_2014, fill = z_score, colour = z_score))+
  geom_hline(aes(yintercept = 0))+
  #geom_point()+
  geom_point(size = 0.5)+
  geom_smooth(method = "loess", color = "black", size = 0.4, se = FALSE,
              formula = y ~ x)+
  coord_cartesian(xlim = c(0,32000))+
  theme_bw()+
  scale_colour_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  scale_fill_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  guides(colour = "none", fill = "none")+
  #ggtitle(paste0(labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"]))+
  scale_x_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  xlab("Household expenditures in US-$ (2014)")+
  ylab("SHAP value for household expenditures")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("plots_GTAP11/PDP_EXP_ZAF.jpg", width = 10, height = 10, unit = "cm", res = 300)
print(P_2.1)
dev.off()

# Car ownership
P_2.2 <- ggplot(data = data_6.6, aes(fill = z_score, colour = z_score))+
  geom_hline(aes(yintercept = 0))+
  geom_jitter(aes(x = as.factor(car.01),
                   y = car.01_SHAP), height = 0, width = 0.25, shape = 21, size = 0.5)+
  # geom_boxplot(aes(x = as.factor(car.01), y = car.01_SHAP), width = 0.2, alpha = 0.5)+
  theme_bw()+
  scale_colour_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  scale_fill_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  guides(colour = "none", fill = "none")+
  scale_y_continuous(breaks = c(seq(-0.01,0.06,0.01)), expand = c(0,0))+
  coord_cartesian(ylim = c(-0.018,0.065))+
  #ggtitle(paste0(labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"]))+
  xlab("Car ownership")+
  ylab("SHAP value for household expenditures")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Province

P_2.3 <- ggplot(data = data_6.6, aes(fill = z_score, colour = z_score))+
  geom_hline(aes(yintercept = 0))+
  geom_jitter(aes(x = as.factor(province_short),
                  y = province_SHAP), height = 0, width = 0.25, shape = 21, size = 0.5)+
  # geom_boxplot(aes(x = as.factor(car.01), y = car.01_SHAP), width = 0.2, alpha = 0.5)+
  theme_bw()+
  scale_colour_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  scale_fill_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  guides(colour = "none", fill = "none")+
  scale_y_continuous(breaks = c(seq(-0.01,0.015,0.005)), expand = c(0,0))+
  coord_cartesian(ylim = c(-0.012,0.018))+
  #ggtitle(paste0(labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"]))+
  xlab("Province")+
  ylab("SHAP value for household expenditures")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Electricity access

P_2.4 <- ggplot(data = data_6.6, aes(fill = z_score, colour = z_score))+
  geom_hline(aes(yintercept = 0))+
  geom_jitter(aes(x = electricity,
                  y = electricity_SHAP), height = 0, width = 0.25, shape = 21, size = 0.5)+
  # geom_boxplot(aes(x = as.factor(car.01), y = car.01_SHAP), width = 0.2, alpha = 0.5)+
  theme_bw()+
  scale_colour_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  scale_fill_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  guides(colour = "none", fill = "none")+
  scale_y_continuous(breaks = c(seq(-0.02,0.015,0.005)), expand = c(0,0))+
  coord_cartesian(ylim = c(-0.02,0.0051))+
  #ggtitle(paste0(labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"]))+
  xlab("Province")+
  ylab("SHAP value for household expenditures")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Ethnicity

P_2.5 <- ggplot(data = data_6.6, aes(fill = z_score, colour = z_score))+
  geom_hline(aes(yintercept = 0))+
  geom_jitter(aes(x = ethnicity_hhh,
                  y = ethnicity_SHAP), height = 0, width = 0.25, shape = 21, size = 0.5)+
  # geom_boxplot(aes(x = as.factor(car.01), y = car.01_SHAP), width = 0.2, alpha = 0.5)+
  theme_bw()+
  scale_colour_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  scale_fill_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  guides(colour = "none", fill = "none")+
  scale_y_continuous(breaks = c(seq(-0.01,0.02,0.005)), expand = c(0,0))+
  coord_cartesian(ylim = c(-0.01,0.02))+
  #ggtitle(paste0(labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"]))+
  xlab("Self-identified ethnicity of household head")+
  ylab("SHAP value for household expenditures")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 5),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Heating fuel (space)

P_2.6 <- ggplot(data = data_6.6, aes(fill = z_score, colour = z_score))+
  geom_hline(aes(yintercept = 0))+
  geom_jitter(aes(x = HFS,
                  y = HFS_SHAP), height = 0, width = 0.25, shape = 21, size = 0.5)+
  # geom_boxplot(aes(x = as.factor(car.01), y = car.01_SHAP), width = 0.2, alpha = 0.5)+
  theme_bw()+
  scale_colour_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  scale_fill_viridis_c(option = "inferno", begin = 0.25, end = 0.85)+
  guides(colour = "none", fill = "none")+
  scale_y_continuous(breaks = c(seq(-0.01,0.02,0.005)), expand = c(0,0))+
  coord_cartesian(ylim = c(-0.01,0.01))+
  #ggtitle(paste0(labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"]))+
  xlab("Main heating fuel for space")+
  ylab("SHAP value for household expenditures")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_2 <- ggarrange(P_2.2, P_2.1, P_2.3, P_2.4, P_2.5, P_2.6, ncol = 3, nrow = 2)

jpeg("plots_GTAP11/PDP_ZAF.jpg", width = 20, height = 15, unit = "cm", res = 300)
print(P_2)
dev.off()

# sv_dependence(shp, v = "ethnicity_hhh_White", color_var = "auto")

# Waterfall plot interesting for clustering

# sv_waterfall(shp, shp$X$car.01 == 1)

# 6.7. Cluster Analysis ####

# One option: gower distance

# Preferable option: one-hot-encoding

set.seed(2023)

household_6.7 <- household_all_1 %>%
  select(-burden_CO2_national)%>%
  # One-hot-encoding for Province
  mutate(value = 1)%>%
  pivot_wider(names_from = "province", values_from = "value", names_prefix = "province_", values_fill = 0)%>%
  select(-'province_Kwazulu Natal')%>%
  # gender_hhh
  # mutate(gender_hhh_Female = ifelse(gender_hhh == "Female",1,0))%>%
  select(-gender_hhh)%>%
  # ethnicity_hhh
  mutate(value = 1)%>%
  pivot_wider(names_from = "ethnicity_hhh", values_from = "value", names_prefix = "ethnicity_", values_fill = 0)%>%
  select(-'ethnicity_African-black')%>%
  # electricity
  mutate(electricity_No_access = ifelse(electricity == "No access",1,0),
         electricity_Other     = ifelse(electricity == "Other" | electricity == "Solar",1,0))%>%
  select(-electricity)%>%
  # electricity_free
  mutate(electricity_Free = ifelse(electricity_free == "Free electricity",1,0))%>%
  select(-electricity_free)%>%
  # LF 
  # mutate(LF_Candles = ifelse(LF == "Candles",1,0))%>%
  # mutate(LF_Other   = ifelse(LF %in% c("Gas", "Solar", "Other"),1,0))%>%
  select(-LF)%>%
  # CF
  # mutate(CF_Wood     = ifelse(CF == "Wood",1,0),
  #        CF_Paraffin = ifelse(CF == "Paraffin",1,0),
  #        CF_Other    = ifelse(!CF %in% c("Paraffin", "Wood", "Electricity"),1,0))%>%
  select(-CF)%>%
  # HFW --> relatively uninteresting
  # mutate(HFW_Wood = ifelse(HFW == "Wood",1,0),
  #        HFW_Other = ifelse(HFW != "Wood" & HFW != "Electricity",1,0))%>%
  select(-HFW) %>%
  # HFS
  mutate(HFS_Paraffin = ifelse(HFS == "Paraffin",1,0),
         HFS_Wood     = ifelse(HFS == "Wood",1,0),
         HFS_Other    = ifelse(!HFS %in% c("Wood", "Paraffin", "Electricity"),1,0))%>%
  select(-HFS)%>%
  # urban_01
  mutate(urban_01_Rural = ifelse(urban_01 == "Rural",1,0))%>%
  select(-urban_01)%>%
  # education_hhh
  # mutate(education_Primary         = ifelse(education_hhh == "1",1,0),
  #        education_Upper_secondary = ifelse(education_hhh == "3",1,0),
  #        education_Post_secondary  = ifelse(education_hhh %in% c("4","6","7","8"),1,0),
  #        education_Other           = ifelse(education_hhh %in% c("9"),1,0))%>%
  select(-education_hhh)%>%
  select(-stove.01)%>%
  mutate_at(vars(everything()), ~ (. - mean(.))/sd(.))

# 6.7.1 Clustering #####

data_6.7.1 <- data.frame()

for(k in 2:30){
  model_6.7 <- kmeans(household_6.7, centers = k, nstart = 50)
  
  # total within-cluster sum of squares
  
  tot_within_ss <- model_6.7$tot.withinss
  
  # Silhouette
  
  silhouette_1 <- mean((cluster::silhouette(model_6.7$cluster, dist(household_6.7)))[,3])
  
  data_6.7.1 <- bind_rows(data_6.7.1,
                          data.frame(k_0             = k,
                                     tot_within_ss_0 = tot_within_ss,
                                     silhouette_0    = silhouette_1))
  print(k)
}

P_6.7.1 <- ggplot(data_6.7.1)+
  geom_vline(aes(xintercept = data_6.7.1$k[silhouette_0 == max(silhouette_0)]))+
  geom_line(aes(x = k_0, y = silhouette_0))+
  geom_point(aes(x = k_0,y = silhouette_0), shape = 21, size = 1.5)+
  scale_fill_manual(values = c("#0072B5FF", "#BC2C29FF"))+
  guides(fill = "none")+
  xlab("Number of clusters (k)")+
  ylab("Average silhouette width")+
  coord_cartesian(ylim = c(min(data_6.7.1$silhouette_0)*0.8, 0.5),
                  xlim = c(0,30.5))+
  scale_x_continuous(expand = c(0,0), breaks = seq(0,30,5), minor_breaks = seq(0,30,1))+
  scale_y_continuous(expand = c(0,0.01))+
  theme_bw()+
  ggtitle("Silhouette plot")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        # panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# 14 clusters can be justified

model_6.7.1      <- kmeans(household_6.7, centers = 14, nstart = 2000)
silhouette_6.7.1 <- cluster::silhouette(model_6.7.1$cluster, dist(household_6.7))[,3]

household_6.7.1 <- household_all_0 %>%
  mutate(cluster    = model_6.7.1$cluster,
         silhouette = silhouette_6.7.1)

household_6.7.2 <- household_6.7.1 %>%
  group_by(cluster)%>%
  summarise(number = n(),
            silhouette = mean(silhouette))%>%
  ungroup()

# Household expenditures, car ownership, province, ethnicity, electricity access, free electricity, HFS

household_6.7.3 <- household_6.7 %>%
  mutate(cluster = model_6.7.1$cluster)%>%
  group_by(cluster)%>%
  summarise_all(~mean(.))%>%
  ungroup()%>%
  pivot_longer(-cluster, names_to = "Type", values_to = "value")

household_6.7.4 <- household_6.7 %>%
  mutate(cluster    = model_6.7.1$cluster,
         silhouette = silhouette_6.7.1)%>%
  group_by(cluster)%>%
  summarise(number = n(),
            silhouette = mean(silhouette))%>%
  ungroup()%>%
  arrange(desc(number))%>%
  mutate(cluster_NEW = LETTERS[1:n()])

cluster_code <- select(household_6.7.4, cluster_NEW, cluster)

household_clusters <- left_join(household_6.7.1, cluster_code)%>%
  select(hh_id, cluster_NEW)

write_csv(household_clusters, "LCS_results/Clusters_ID_ZAF.csv")

household_6.7.5 <- household_6.7.1 %>%
  left_join(cluster_code)%>%
  group_by(cluster_NEW)%>%
  summarise(y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(burden_CO2_national,    weights = hh_weights))%>%
  ungroup()

ggplot(household_6.7.5, aes(x = reorder(cluster_NEW,y50)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "#0072B5FF",
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 0.7) +
  theme_bw()+
  ylab("Carbon pricing burden")+ xlab("Cluster")+
  geom_point(aes(y = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  # scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_flip(ylim = c(0,0.1501))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.border = element_rect(size = 0.3))

# For each cluster: Burden with carbon tax, full lump sum transfer, full CO2 tax electricity compensation
# Bar chart showing key features of particular cluster
# 6.8 Revenue recycling ####

household_6.8.0 <- read_csv("LCS_results/hh_final_LCS.csv")%>%
  select(hh_id, exp_CO2_national, burden_CO2_electricity, hh_size, hh_weights)%>%
  # Weights of total CO2 expenditures
  mutate(exp_CO2_national_weighted = exp_CO2_national*hh_weights)%>%
  # Total population
  mutate(population                = hh_size*hh_weights)

transfer_0 <- sum(household_6.8.0$exp_CO2_national_weighted)/sum(household_6.8.0$population)*0.25

household_6.8 <- household_6.7.1 %>%
  left_join(select(household_6.8.0, hh_id, exp_CO2_national, burden_CO2_electricity), by = "hh_id")%>%
  left_join(cluster_code, by = "cluster")%>%
  select(hh_id, cluster_NEW, everything(), burden_CO2_national)%>%
  select(-cluster)%>%
  # Compensation through exempting electricity prices
  mutate(burden_CO2_national_rev_electricity = burden_CO2_national - burden_CO2_electricity)%>%
  # Compensation through lump-sum transfer
  mutate(burden_CO2_national_rev_LST = burden_CO2_national - ((hh_size*transfer_0)/hh_expenditures_USD_2014))%>%
  select(cluster_NEW, hh_weights, burden_CO2_national, burden_CO2_national_rev_LST, burden_CO2_national_rev_electricity)%>%
  pivot_longer(c(starts_with("burden")), names_to = "Type", values_to = "values", names_prefix = "burden_")%>%
  group_by(cluster_NEW, Type)%>%
  summarise(y5  = wtd.quantile(values, weights = hh_weights, probs = 0.10),
            y25 = wtd.quantile(values, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(values, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(values, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(values, weights = hh_weights, probs = 0.90),
            mean = wtd.mean(values,    weights = hh_weights))%>%
  ungroup()

P_6.8 <- ggplot(household_6.8, aes(x = reorder(Type,y50), group = interaction(cluster_NEW, Type), fill = Type))+
  geom_hline(aes(yintercept = 0))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 0.7) +
  theme_bw()+
  facet_wrap(. ~ cluster_NEW, ncol = 5)+
  ylab("Additional costs to households")+ xlab("")+
  geom_point(aes(y = mean), shape = 23, size = 1.5, stroke = 0.5, fill = "white", position = position_dodge (0.5))+
  scale_fill_nejm(labels = c("No compensation", "Excluding electricity sector", "Lump-sum transfer"), name = "Policy")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  # scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_flip(ylim = c(-0.12,0.135))+
  ggtitle("")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 7),
        axis.title.x  = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("plots_GTAP11/Cluster_Compensation_ZAF.jpg", width = 20, height = 15, unit = "cm", res = 300)
print(P_6.8)
dev.off()
