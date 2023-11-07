## 6. Random Forest and Household Cluster ####

library(data.table)
library(tidyverse)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
# library(caret)
# library(ggpmisc)
library(cluster)
library(GGally)
library(tidymodels)


#### CHANGE PATH ####
# Set working directory Agatha
# etwd("C:/Users/agath/Desktop/Code GitHub/LCS_results")

# Load data 1 - LCS
household_all <- read_csv("LCS_results/hh_final_LCS.csv")

household_all <- household_all %>% 
  select(hh_id, hh_expenditures_USD_2014, 
         # exp_CO2_national, 
         burden_CO2_national, 
         province, urban_1, 
         electricitycon, electricitymains,
         electricitysource, electricityfree, 
         lightingsource, cookingsource, heatingsourcewater, heatingsourcespace, 
         education_hhh, gender_hhh, ethnicity_hhh, own_stove, own_vehicle)%>%
  mutate(log_expenditures = log(hh_expenditures_USD_2014))

# 6.1 Pre-Processing - transforming data with recipes and splitting data ####

household_all_1 <- household_all %>%
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
  mutate(education_hhh = case_when(education_hhh < 9  ~ 1, 
                                   education_hhh < 15 ~ 2, 
                                   education_hhh < 25 ~ 3,
                                   education_hhh < 27 ~ 4,
                                   education_hhh < 29 ~ 6,
                                   education_hhh < 30 ~ 7,
                                   education_hhh < 31 ~ 8,
                                   education_hhh > 39 ~ 9))%>%
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
         -heatingsourcewater, -heatingsourcespace, -log_expenditures, -electricityfree, -urban_1, -hh_id)

# 6.2 Tuning the model ####

# 6.3 Fitting the final model ####

# 6.4 SHAP-values ####

# 6.5 Variable importance plots ####

# 6.6 Partial dependence plots ####


## 6.2. Random Forest ####

household_all <- household_all %>%
   mutate_at(vars(province:own_vehicle), ~ as.character(.))%>%
   select(-log_expenditures, -hh_id)
 
set.seed(1)

# split in train/test
# sample_split <- sample.split(Y = household2$burden_CO2_national, SplitRatio = 0.75)
sample_split <- initial_split(household_all, prop = 0.75)
# train_set <- subset(x = household2, sample_split == TRUE)
train_set <- training(sample_split)
# test_set <- subset(x = household2, sample_split == FALSE)
test_set <- testing(sample_split)

recipe_0 <- recipe(burden_CO2_national ~ ., data = train_set)%>%
  step_filter_missing(all_predictors(), threshold = 0)%>%
  step_other(all_nominal())%>%
  step_dummy(all_nominal())

training_set <- recipe_0 %>%
  prep(training = train_set)%>%
  bake(new_data = NULL)

tune_model <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune(),
)%>%
  set_mode("regression")%>%
  set_engine("ranger")

# Workflow
wf <- workflow()%>%
  add_recipe(recipe_0)%>%
  add_model(tune_model)
  
# Cross-validation

folds <- vfold_cv(training_set, v = 5)

tuned_model <- tune_grid(
  wf,
  resamples = folds, 
  grid = 10
)

#Cross Validation
tree_train_control = trainControl(
  method = "repeatedcv",
  number = 10,
  #repeats = 3,
  savePredictions = "final"       # save predictions for the optimal tuning parameter
)

# # Tuning
# tuneGrid = expand.grid(
#   mtry = seq(1, ncol(train_set) - 1, by = 1),
#   ntree = c(100, 500, 1000),
#   node.size = c(1, 3, 5, 10)
# )


# # random forest with cross validation and tuning
# tree_rf <- randomForest(
#   burden_CO2_national ~ .,
#   data = train_set,
#   tuneGrid = tuneGrid,
#   trControl = tree_train_control
# )

# random forest with cross validation
tree_rf <- train(
  burden_CO2_national ~ .,
  data = train_set,
  method = "rf",
  tuneGrid = expand.grid(mtry = 1:10), # searching around mtry=3
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

#Importance
vi <- varImp(tree_rf)


# Replace names
replacements <- c(
  cooking_ = "Cooking Fuel: ",
  elec_ = "Electricity Source: ",
  elecf_ = "Free Electricity: ",
  veh_ = "Car: ",
  sto_ = "Stove: ",
  edu_ = "Education: ",
  eth_ = "Ethnicity:",
  urb_ = "Res. Area: ",
  gender_ = "Gender: ",
  pro_ = "Province: ",
  log_expenditures = "Expenditures (log)",
  heat_w_ = "Heating Water: ",
  heat_s_ = "Heating Space: ",
  light_ = "Lighting Fuel: ",
  westerncape = "Western Cape",
  kwazulunatal = "KwaZulu Natal"
)

# Specific replacements
specific_replacements <- c(
  "elec_other" = "Electricity Source: Other", 
  "mains" = "Mains",
  "noaccess" = "No Access",
  "other" = "Other",
  "own" = "Ownership",
  "westerncape" = "Western Cape",
  "easterncape" = "Eastern Cape",
  "northerncape" = "Northern Cape",
  "freestate" = "Free State",
  "kwazulunatal" = "KwaZulu Natal",
  "northwest" = "North West",
  "gauteng" = "Gauteng",
  "mpumalanga" = "Mpumalanga",
  "limpopo" = "Limpopo",
  "urban_formal" = "Urban Formal",
  "urban_informal" = "Urban Informal",
  "rural_traditional" = "Rural Traditional",
  "rural_formal" = "Rural Formal",
  #"female" = "Female",
  #"male" = "Male",
  "africanblack" = "African Black",
  "coloured" = "Coloured",
  "indian_asian" = "Indian/Asian",
  "white" = "White",
  "preprimary" = "Pre-primary",
  "primary" = "Primary",
  "lower_secondary" = "Lower Secondary",
  "upper_secondary" = "Upper Secondary",
  "postsecondary_nontertiary" = "Post-secondary Non-tertiary",
  "shortcycle_tertiary" = "Short-cycle Tertiary",
  "bachelor_eq" = "Bachelor's or Equivalent",
  "master_eq" = "Master's or Equivalent",
  "no_schooling" = "No Schooling",
  "solar" = "Solar",
  "paraffin" = "Paraffin",
  "candles" = "Candles",
  "gas" = "Gas",
  "coal" = "Coal",
  "wood" = "Wood",
  "dung" = "Dung",
  "nofree" = "Not Free",
  "free" = "Free",
  "elecmains" = "Electricity from Mains",
  "elec" = "Electricity "
)


# Replace 'female' and 'male'
rownames(vi$importance) <- gsub("male", "Male", rownames(vi$importance))
rownames(vi$importance) <- gsub("feMale", "Female", rownames(vi$importance))


for (term in names(specific_replacements)) {
  rownames(vi$importance) <- gsub(term, specific_replacements[term], rownames(vi$importance))
}

# Replace prefixes
for (prefix in names(replacements)) {
  rownames(vi$importance) <- gsub(paste0("^", prefix), replacements[prefix], rownames(vi$importance))
}

rownames(vi$importance) <- gsub("Electricity _", "Electricity Source: ", rownames(vi$importance))
rownames(vi$importance) <- gsub("Electricity f_", "Free Electricity: ", rownames(vi$importance))



# Plot important variables
plot(vi, main="Variable Importance with Random Forest")


p <- ggplot(data= vi, aes(x=rownames(vi), y=Overall)) +
  geom_bar(position="dodge", stat="identity", width=0.9, color="white") +
  geom_point(color='black', size=3) +
  xlab("") +
  ggtitle("Variable Importance") +
  scale_y_continuous(breaks=seq(0, 100, by=10), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  expand_limits(y = c(0, 105)) +
  geom_hline(yintercept=seq(0, 100, by=5), linetype="dotted", color="grey50") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

# Plot
plot(p)

# Save
png("RF_importance.png", family = "sans", units = "cm",
    width = 17.35, height = 23.35, pointsize = 18, res = 300)
print(p)
dev.off()


    

######################################################################################



## 6.3. Cluster Analysis ####

set.seed(2)

household_c <- household2
household_c1 <- household2



#  6.3.1. Number of Clusters

household_c2 <- household_c %>%
  dplyr::select(-burden_CO2_national)

household_c3 <- household_c %>%
  dplyr::select(-burden_CO2_national)


# Silhoulette Method - result  3 (0.16) or 7 (0.12)
silhouette_score <- function(k) {
  model <- kmeans(household_c2, centers=k)
  ss <- silhouette(model$cluster, dist(household_c2))
  mean(ss[, 3])
}
k <- 2:10

plot(k, sapply(k, silhouette_score), type='b')



# Elbow Method - result 8
wss <- (nrow(household_c3)-1)*sum(apply(household_c3,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(household_c3, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



# 
# # create 8 clusters
# kmeans_result <- kmeans(household_c, centers = 8)
# 
# # add the cluster labels to data frame
# household_c$cluster <- kmeans_result$cluster
# 
# # Count of households in each cluster
# cluster_counts <- table(household_c$cluster)
# print(cluster_counts)
# 
# # mean burden_co2_national for each cluster
# means <- aggregate(household_c$burden_CO2_national ~ household_c$cluster, FUN = mean)
# print(means)
# 
# # get means of all variables
# cluster_summary <- aggregate(. ~ cluster, data = household_c, mean)
# 
# 
# # Filter to only keep variables where at least one cluster has a mean above 0.5
# vars_to_keep <- colSums(cluster_summary > 0.5) > 0
# 
# # Subset the data to only these variables
# cluster_summary_subset <- cluster_summary[, vars_to_keep]
# 
# # Convert cluster to factor 
# cluster_summary_subset$cluster <- as.factor(cluster_summary_subset$cluster)



#### create cluster without burden #######################

# Scal Variables to have equal weightage across all variables - exclude burden_CO2_national
scaled_data <- as.data.frame(scale(select(household_c1, -burden_CO2_national)))

# Create 4 clusters using the scaled data
kmeans_result1 <- kmeans(scaled_data, centers = 4)

# Add the cluster labels back to the original data frame
household_c1$cluster <- kmeans_result1$cluster

# Count of households in each cluster
cluster_counts1 <- table(household_c1$cluster)
print(cluster_counts1)

# Calculate mean burden_co2_national for each cluster using the original data
means1 <- aggregate(burden_CO2_national ~ cluster, data = household_c1, FUN = mean)
print(means1)

# Get the means of all variables in each cluster
cluster_summary1 <- aggregate(. ~ cluster, data = household_c1, mean)

#add burden co2 as percent
cluster_summary1$burden_CO2_national <- cluster_summary1$burden_CO2_national*100


# Pivot the data to wider format
cluster_summary <- cluster_summary1 %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = cluster, values_from = value)

# Save to CSV
write.csv(cluster_summary, "cluster_all.csv", row.names = FALSE)



# # Filter to only keep variables where at least one cluster has a mean above 0.5
# vars_to_keep1 <- colSums(cluster_summary1 > 0.5) > 0
# 
# # Subset the data to only these variables
# cluster_summary_subset1 <- cluster_summary1[, vars_to_keep1]
# 
# # Convert cluster to factor 
# cluster_summary_subset1$cluster <- as.factor(cluster_summary_subset1$cluster)
# 

# # Pivot the data to wider format
# cluster_summary_sub <- cluster_summary_subset1 %>%
#   pivot_longer(cols = -cluster, names_to = "variable", values_to = "value") %>%
#   pivot_wider(names_from = cluster, values_from = value)
# 
# # Save to CSV
# write.csv(cluster_summary_sub, "cluster_sub.csv", row.names = FALSE)
# 



#### Plots not used

# Create plot
ggparcoord(cluster_summary_subset1, columns = 2:ncol(cluster_summary_subset1), groupColumn = 1, scale = "globalminmax", alpha = 1) +
  scale_color_discrete(name = "Cluster") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_minimal() + 
  geom_path(size = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Convert data into 'long' format
long_data <- gather(cluster_summary_subset1, variable, value, -cluster)

# Convert 'variable' to a factor and specify order of levels
long_data$variable <- factor(long_data$variable, levels = unique(long_data$variable))
long_data$variable <- factor(long_data$variable, levels = rev(levels(long_data$variable)))


# Add an index column that indicates the order of rows within each cluster
long_data <- long_data %>%
  group_by(cluster) %>%
  mutate(index = row_number()) %>%
  ungroup()


# Dot plot
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

