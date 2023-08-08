## 6. Random Forest and Household Cluster ####

library(data.table)
library(tidyverse)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(ggpmisc)
library(cluster)
library(GGally)


#### CHANGE PATH ####
# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Code GitHub/LCS_results")

# Load data 1 - LCS
household_all <- fread("hh_final_LCS.csv")

household_all <- household_all %>% 
  dplyr::select(-1)


# Select Variables
household <- household_all %>%
  dplyr::select(hh_id, hh_expenditures_USD_2014, exp_CO2_national, burden_CO2_national, province, urban_1, electricitycon, electricitymains,
         electricitysource, lightingsource, cookingsource, heatingsourcewater, heatingsourcespace, education_hhh, gender_hhh, ethnicity_hhh, own_stove, own_vehicle)

household1 <- household %>%
  dplyr::select(burden_CO2_national)



# log expenditures
household1$log_expenditures <- log(household$hh_expenditures_USD_2014)

## 6.1. Create binary variables ####

 # Provinces
 household1$pro_westerncape <- ifelse(household$province == '1', 1, 0)
 household1$pro_easterncape <- ifelse(household$province == '2', 1, 0)
 household1$pro_northerncape <- ifelse(household$province == '3', 1, 0)
 household1$pro_freestate <- ifelse(household$province == '4', 1, 0)
 household1$pro_kwazulunatal <- ifelse(household$province == '5', 1, 0)
 household1$pro_northwest <- ifelse(household$province == '6', 1, 0)
 household1$pro_gauteng <- ifelse(household$province == '7', 1, 0)
 household1$pro_mpumalanga <- ifelse(household$province == '8', 1, 0)
 household1$pro_limpopo <- ifelse(household$province == '9', 1, 0)

 # Urban/Rural
 household1$urb_urban_formal <- ifelse(household$urban_1 == '1', 1, 0)
 household1$urb_urban_informal <- ifelse(household$urban_1 == '2', 1, 0)
 household1$urb_rural_traditional <- ifelse(household$urban_1 == '4', 1, 0)
 household1$urb_rural_formal <- ifelse(household$urban_1 == '5', 1, 0)

 # Gender
 household1$gender_male <- ifelse(household$gender_hhh == '1', 1, 0)
 household1$gender_female <- ifelse(household$gender_hhh == '2', 1, 0)
 
 
 # Ethnicity
 household1$eth_africanblack <- ifelse(household$ethnicity_hhh == '1', 1, 0)
 household1$eth_coloured <- ifelse(household$ethnicity_hhh == '2', 1, 0)
 household1$eth_indian_asian <- ifelse(household$ethnicity_hhh == '3', 1, 0)
 household1$eth_white <- ifelse(household$ethnicity_hhh == '4', 1, 0)


# Education
household1$edu_preprimary <- ifelse(household$education_hhh == '1', 1, 0)
household1$edu_primary <- ifelse(household$education_hhh == '2' | household$education_hhh == '3' | household$education_hhh == '4'| household$education_hhh == '5', 1, 0)
household1$edu_secondary <- ifelse(household$education_hhh == '6' | household$education_hhh == '7' | household$education_hhh == '8'| household$education_hhh == '9'|
                                  household$education_hhh == '10' | household$education_hhh == '11' | household$education_hhh == '12'| household$education_hhh == '13'|
                                    household$education_hhh == '14' , 1, 0)
household1$edu_postsecondary_nontertiary <- ifelse(household$education_hhh == '15' | household$education_hhh == '16' | household$education_hhh == '17'| household$education_hhh == '18'|
                                    household$education_hhh == '19' | household$education_hhh == '20' | household$education_hhh == '21'| household$education_hhh == '22'|
                                    household$education_hhh == '23'| household$education_hhh == '24', 1, 0)
household1$edu_shortcycle_tertiary <- ifelse(household$education_hhh == '25'| household$education_hhh == '26', 1, 0)
household1$edu_bachelor_eq <- ifelse(household$education_hhh == '27'| household$education_hhh == '28'| household$education_hhh == '29', 1, 0)
household1$edu_master_eq <- ifelse(household$education_hhh == '30', 1, 0)


# own car/stove
household1$veh_own <- ifelse(household$own_vehicle == '1', 1, 0)
household1$veh_noaccess <- ifelse(household$own_vehicle == '3', 1, 0)
 household1$sto_own <- ifelse(household$own_stove == '1', 1, 0)
 household1$sto_noaccess <- ifelse(household$own_stove == '3', 1, 0)

# electricity source
household1$elec_mains <- ifelse(household$electricitymains == '1', 1, 0)
household1$elec_other <- ifelse(household$electricitysource == '1'| household$electricitysource == '6', 1, 0)
household1$elec_gen_bat <- ifelse(household$electricitysource == '3'| household$electricitysource == '5', 1, 0)
household1$elec_solar <- ifelse(household$electricitysource == '4', 1, 0)
household1$elec_noaccess <- ifelse(household$electricitycon == '2', 1, 0)

# lighting source
household1$light_elecmains <- ifelse(household$lightingsource == '1', 1, 0)
household1$light_elecother <- ifelse(household$lightingsource == '2', 1, 0)
household1$light_gas_par_can <- ifelse(household$lightingsource == '3'|household$lightingsource == '4'|household$lightingsource == '7', 1, 0)
household1$light_solar <- ifelse(household$lightingsource == '9', 1, 0)

# cooking source
household1$cooking_elecmains <- ifelse(household$cookingsource == '1', 1, 0)
household1$cooking_elecother <- ifelse(household$cookingsource == '2', 1, 0)
household1$cooking_gas_par_coa <- ifelse(household$cookingsource == '3'|household$cookingsource == '4'|household$cookingsource == '6', 1, 0)
household1$cooking_wood_dung <- ifelse(household$cookingsource == '5'|household$cookingsource == '8', 1, 0)
household1$cooking_solar <- ifelse(household$cookingsource == '9', 1, 0)
household1$cooking_noaccess <- ifelse(household$cookingsource == '11', 1, 0)

# water heating source
household1$heat_w_elecmains <- ifelse(household$heatingsourcewater == '1', 1, 0)
household1$heat_w_elecother <- ifelse(household$heatingsourcewater == '2', 1, 0)
household1$heat_w_gas_par_coa <- ifelse(household$heatingsourcewater == '3'|household$heatingsourcewater == '4'|household$heatingsourcewater == '6', 1, 0)
household1$heat_w_wood_anid <- ifelse(household$heatingsourcewater == '5'|household$heatingsourcewater == '8', 1, 0)
household1$heat_w_solar <- ifelse(household$heatingsourcewater == '9', 1, 0)
household1$heat_w_noaccess <- ifelse(household$heatingsourcewater == '11', 1, 0)

# space heating source
household1$heat_s_elecmains <- ifelse(household$heatingsourcespace == '1', 1, 0)
household1$heat_s_elecother <- ifelse(household$heatingsourcespace == '2', 1, 0)
household1$heat_s_gas_par_coa <- ifelse(household$heatingsourcespace == '3'|household$heatingsourcespace == '4'|household$heatingsourcespace == '6', 1, 0)
household1$heat_s_wood_anid <- ifelse(household$heatingsourcespace == '5'|household$heatingsourcespace == '8', 1, 0)
household1$heat_s_solar <- ifelse(household$heatingsourcespace == '9', 1, 0)
household1$heat_s_noaccess <- ifelse(household$heatingsourcespace == '11', 1, 0)



 household2 <- household1
#   select(!log_expenditures)



## 6.2. Random Forest ####

set.seed(1)


# split in train/test
sample_split <- sample.split(Y = household2$burden_CO2_national, SplitRatio = 0.75)
train_set <- subset(x = household2, sample_split == TRUE)
test_set <- subset(x = household2, sample_split == FALSE)


#Cross Validation
tree_train_control = trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final"       # save predictions for the optimal tuning parameter
)


# random forest with cross validation
tree_rf <- train(
  burden_CO2_national ~ .,
  data = train_set,
  method = "rf",
  tuneGrid = expand.grid(mtry = 1:20), # searching around mtry=3
  trControl = tree_train_control
)


tree_rf


plot(tree_rf)


# Predictions
tree_preds_rf <- bind_cols(
  Predicted = predict(tree_rf, newdata = test_set),
  Actual = test_set$burden_CO2_national
)

# RMSE random forest
rmse_rf <- RMSE(pred = tree_preds_rf$Predicted, obs = tree_preds_rf$Actual)

# Plot Predicted vs Actual
tree_preds_rf %>%
  ggplot(aes(x = Actual*100, y = Predicted*100)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth(method = "loess", formula = "y ~ x") +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Burden Carbon Tax - Random Forest: Predicted vs Actual",
       x = "Actual Burden (% total expenditures)",
       y = "Predicted Burden (% total expenditures)")


# Important variables
plot(varImp(tree_rf), main="Variable Importance with Random Forest")



## 6.3. Cluster Analysis ####
set.seed(123)

household_c <- household2


#Optimal Number of Clusters

# Silhoulette Method - result 2
silhouette_score <- function(k) {
  model <- kmeans(household_c, centers=k)
  ss <- silhouette(model$cluster, dist(household_c))
  mean(ss[, 3])
}
k <- 2:10
plot(k, sapply(k, silhouette_score), type='b')


# Elbow Method - result 1 
wss <- (nrow(household_c)-1)*sum(apply(household_c,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(household_c, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




# create 5 clusters
kmeans_result <- kmeans(household_c, centers = 5)

# add the cluster labels to data frame
household_c$cluster <- kmeans_result$cluster

# mean burden_co2_national for each cluster
means <- aggregate(household_c$burden_CO2_national ~ household_c$cluster, FUN = mean)

print(means)

# table with results
cluster_summary <- aggregate(. ~ cluster, data = household_c, mean)



# Plot with only high proportion of varibales in cluster

#add burden co2 as percent
household_c$burden_CO2_national <- household_c$burden_CO2_national*100

household_c <- household_c %>% 
  dplyr::select(log_expenditures, burden_CO2_national, everything())

# Get the means of all variables in each cluster
cluster_summary <- aggregate(. ~ cluster, data = household_c, mean)

# Filter to only keep variables where at least one cluster has a mean above 0.5
vars_to_keep <- colSums(cluster_summary > 0.5) > 0

# Subset the data to only these variables
cluster_summary_subset <- cluster_summary[, vars_to_keep]

# Convert cluster to factor (required by ggparcoord)
cluster_summary_subset$cluster <- as.factor(cluster_summary_subset$cluster)



# Create plot
ggparcoord(cluster_summary_subset, columns = 2:ncol(cluster_summary_subset), groupColumn = 1, scale = "globalminmax", alpha = 1) +
  scale_color_discrete(name = "Cluster") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_minimal() + 
  geom_path(size = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Convert data into 'long' format
long_data <- gather(cluster_summary_subset, variable, value, -cluster)

# Convert 'variable' to a factor and specify order of levels
long_data$variable <- factor(long_data$variable, levels = unique(long_data$variable))
long_data$variable <- factor(long_data$variable, levels = rev(levels(long_data$variable)))


# Add an index column that indicates the order of rows within each cluster
long_data <- long_data %>%
  group_by(cluster) %>%
  mutate(index = row_number()) %>%
  ungroup()

# Create plot
ggplot(long_data, aes(x = value, y = variable, group = as.factor(cluster))) +
  geom_path(aes(color = as.factor(cluster))) +
  geom_point(aes(color = as.factor(cluster)), size=3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(color = "Cluster")



# Bar Plot
ggplot(long_data, aes(x = variable, y = value, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + # Bar plot with black borders
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") + # Line at y = 0.5
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + # Line at y = 1
  scale_y_continuous(breaks = seq(floor(min(long_data$value)), ceiling(max(long_data$value)), by = 1)) + # Y-axis breaks by 1
  theme_minimal() +
  labs(fill = "Cluster") +
  coord_flip() # Flip the axes to make the bars horizontal



# Clean Environment
rm(list=ls())

