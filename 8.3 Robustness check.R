## Robustness check #####
#Variables: expenditures, burden_CO2_national, province, urban_1, electricity (mains/other), gender, ethnicity

library(tidyverse)
library(ggplot2)



#### CHANGE PATH ####
# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Code GitHub")


# Load data 1 - LCS
household_lcs <- read_csv("TBD/hh_final_LCS.csv")
household_lcs <- household_lcs %>% 
  select(-1)

household <- household_lcs %>%
  select(hh_id, hh_expenditures_USD_2014, exp_CO2_national, burden_CO2_national, province, urban_1, electricitymains,
         education_hhh, ethnicity_hhh, gender_hhh)


# Load data 2 - IES
household_ies <- fread("TBD/hh_final_IES.csv")
household_ies <- household_ies %>% 
  select(-1)

household <- household_ies %>%
  select(hh_id, hh_expenditures_USD_2011, exp_CO2_national, burden_CO2_national, province, urban_1, electricitycon,
         education_hhh, ethnicity_hhh, gender_hhh) %>%
  rename(electricitymains=electricitycon)



## Rename variables and set levels ####

### Provinces
household <- household %>%
  mutate(province = factor(recode(province,
                                  `1` = 'Western Cape',
                                  `2` = 'Eastern Cape',
                                  `3` = 'Northern Cape',
                                  `4` = 'Free State',
                                  `5` = 'KwaZulu-Natal',
                                  `6` = 'North West',
                                  `7` = 'Gauteng',
                                  `8` = 'Mpumalanga',
                                  `9` = 'Limpopo'), 
                           levels = c("Western Cape","Eastern Cape","Northern Cape","Free State",
                                      "KwaZulu-Natal","North West", "Gauteng","Mpumalanga","Limpopo")))


### Urban
household <- household %>%
  mutate(urban_1 = factor(recode(urban_1,
                                 `1` = 'Urban formal',
                                 `2` = 'Urban informal',
                                 `4` = 'Traditional area',
                                 `5` = 'Rural formal'),
                          levels = c('Urban formal','Urban informal','Traditional area','Rural formal')))


### Ethnicity household head
household <- household %>%
  mutate(ethnicity_hhh = factor(recode(ethnicity_hhh,
                                       `1` = 'African Black',
                                       `2` = 'Coloured',
                                       `3` = 'Indian/Asian',
                                       `4` = 'White'),
                                levels = c('African Black','Coloured','Indian/Asian','White')))

### Gender household head
household <- household %>%
  mutate(gender_hhh = factor(recode(gender_hhh,
                                    `1` = 'Male',
                                    `2` = 'Female'),
                             levels = c('Male', 'Female')))


### Electricity source
household <- household %>%
  mutate(electricitymains = case_when (electricitymains == 1 ~ 'Mains',
                                        electricitymains > 1 ~ 'Other')) %>%
  mutate(electricitymains = factor(electricitymains, levels = c('Mains', 'Other')))



##### x ####
#household_lcs <- household
#household_ies <- household

# create log expenditures variable
household_lcs$log_expenditures_hh <- log(household_lcs$hh_expenditures_USD_2014)
household_ies$log_expenditures_hh <- log(household_ies$hh_expenditures_USD_2011)




## Training and Test LCS ####

set.seed(123)
train_index <- createDataPartition(y = household_lcs$burden_CO2_national, p = 0.7, list = FALSE)
train_data <- household_lcs[train_index, ]
test_data <- household_lcs[-train_index, ]


#Fit model using the training set
model <- lm(burden_CO2_national ~ log_expenditures_hh + electricitymains + province + urban_1 + ethnicity_hhh + 
              gender_hhh, na.action=na.exclude, data=train_data)

# evaluate performance
predictions <- predict(model, newdata = test_data)
rsquared <- summary(lm(burden_CO2_national ~ log_expenditures_hh + electricitymains + province + urban_1 + ethnicity_hhh + 
                         gender_hhh, na.action=na.exclude, data = test_data))$r.squared



# prediction of ies with model of lcs
predictions <- predict(model, newdata = household_ies)
rsquared <- summary(lm(burden_CO2_national ~ log_expenditures_hh + electricitymains + province + urban_1 + ethnicity_hhh + 
                         gender_hhh, na.action=na.exclude, data = household_ies))$r.squared



# MSE and MAW - mean squared error and mean absolute error
#Fit model using the training set
model <- lm(burden_CO2_national ~ log_expenditures_hh + electricitymains + province + urban_1 + ethnicity_hhh + 
              gender_hhh, na.action=na.exclude, data=train_data)

# evaluate performance on test set
predictions_test <- predict(model, newdata = test_data)
mse_test <- mean((test_data$burden_CO2_national - predictions_test)^2)
mae_test <- mean(abs(test_data$burden_CO2_national - predictions_test))

# evaluate performance on second dataset
predictions_ies <- predict(model, newdata = household_ies)
mse_ies <- mean((household_ies$burden_CO2_national - predictions_ies)^2)
mae_ies <- mean(abs(household_ies$burden_CO2_national - predictions_ies))


# Root Mean Squared Error (RMSE)



#######################################################################################################################

p <- ggplot(household_lcs, aes(x=hh_expenditures_USD_2014, y=burden_CO2_national*100)) +
  geom_smooth(method = "lm", level = 0.95) +
  geom_point(color='black', alpha = 0.06) +
  labs(title = "Burden CO2 tax (LCS 2014/15) vs Expenditures", x = "Total Expenditures (USD)", y = "Total burden CO2 tax (% of expenditures)")

# Add the regression equation to the plot
p2=p + stat_poly_eq(formula = y ~ x, 
                    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                    parse = TRUE)

q <- ggplot(household_ies, aes(x=hh_expenditures_USD_2011, y=burden_CO2_national*100)) +
  geom_smooth(method = "lm", level = 0.95) +
  geom_point(color='black', alpha = 0.06)+
  labs(title = "Burden CO2 tax (IES 2010/11) vs Expenditures", x = "Total Expenditures (USD)", y = "Total burden CO2 tax (% of expenditures)")

# Add the regression equation to the plot
q2= q + stat_poly_eq(formula = y ~ x, 
                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE)


# Set the y-axis limits to the maximum range of both plots
ylim <- range(c(ggplot_build(p2)$layout$panel_params[[1]]$y.range,
                ggplot_build(q2)$layout$panel_params[[1]]$y.range))

# Set the x-axis limits to the maximum range of both plots
xlim <- range(c(ggplot_build(p2)$layout$panel_params[[1]]$x.range,
                ggplot_build(q2)$layout$panel_params[[1]]$x.range))

# Update plots p2 and q2 with the same x- and y-axis limits
p2 <- p2 + xlim(xlim) + ylim(ylim) + expand_limits(x = xlim, y = ylim)
q2 <- q2 + xlim(xlim) + ylim(ylim) + expand_limits(x = xlim, y = ylim)

# Arrange plots p2 and q2 side-by-side
grid.arrange(p2, q2, ncol=2)




## OLS ####
ols <- lm(burden_CO2_national ~  log_expenditures_hh + province + urban_1 + ethnicity_hhh + gender_hhh+
            electricitymains, na.action=na.exclude, data=household_ies)
summary(ols)

write.csv(summary(ols)['coefficients'],file='ols_result_ies.csv')



ols2 <- lm(burden_CO2_national ~  log_expenditures_hh + province + urban_1 + ethnicity_hhh + gender_hhh +
            electricitymains, na.action=na.exclude, data=household_lcs)
summary(ols2)

write.csv(summary(ols2)['coefficients'],file='ols_result_lcs.csv')



