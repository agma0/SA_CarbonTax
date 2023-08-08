library(tidyverse)
library(dplyr)


options(scipen=999)


#### CHANGE PATH ####
# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Code GitHub/LCS_results")


# Load data 1 - LCS
household_all <- read_csv("hh_final_LCS.csv")

household_all <- household_all %>% 
  select(-1)



# Select variables
household <- household_all %>%
  select(hh_id, hh_expenditures_USD_2014, exp_CO2_national, burden_CO2_national, Income_Group_5, province, urban_1, electricitycon, electricitymains,
         electricitysource, lightingsource, cookingsource, heatingsourcewater, heatingsourcespace, education_hhh, ethnicity_hhh, gender_hhh, own_stove, own_vehicle)


## 1. Rename variables and set levels ####

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


### Education ISCED codes
household <- household %>%
  mutate(education_hhh = factor(recode(education_hhh, 
                                       `1` = 'Pre-primary Education',
                                       `2` = 'Primary Education',
                                       `3` = 'Primary Education',
                                       `4` = 'Primary Education',
                                       `5` = 'Primary Education',
                                       `6` = 'Secondary Education',
                                       `7` = 'Secondary Education',
                                       `8` = 'Secondary Education',
                                       `9` = 'Secondary Education',
                                       `10` = 'Secondary Education',
                                       `11` = 'Secondary Education',
                                       `12` = 'Secondary Education',
                                       `13` = 'Secondary Education',
                                       `14` = 'Secondary Education',
                                       `15` = 'Post-secondary Non-tertiary Education',
                                       `16` = 'Post-secondary Non-tertiary Education',
                                       `17` = 'Post-secondary Non-tertiary Education',
                                       `18` = 'Post-secondary Non-tertiary Education',
                                       `19` = 'Post-secondary Non-tertiary Education',
                                       `20` = 'Post-secondary Non-tertiary Education',
                                       `21` = 'Post-secondary Non-tertiary Education',
                                       `22` = 'Post-secondary Non-tertiary Education',
                                       `23` = 'Post-secondary Non-tertiary Education',
                                       `24` = 'Post-secondary Non-tertiary Education',
                                       `25` = 'Short-cycle Tertiary Education',
                                       `26` = 'Short-cycle Tertiary Education',
                                       `27` = 'Bachelors or Equivalent',
                                       `28` = 'Bachelors or Equivalent',
                                       `29` = 'Bachelors or Equivalent',
                                       `30` = 'Masters or Equivalent',
                                       .default = 'NA'),
                                levels = c("Secondary Education",'Pre-primary Education','Primary Education',
                                           'Post-secondary Non-tertiary Education','Short-cycle Tertiary Education',
                                           'Bachelors or Equivalent','Masters or Equivalent')))


### Ownership stove, car
household <- household %>%
  mutate(own_stove = factor(recode(own_stove,
                                   `1` = 'Ownership',
                                   `3` = 'No Access',
                                   .default = 'NA'),
                            levels = c('Ownership', 'No Access')))

household <- household %>%
  mutate(own_vehicle = factor(recode(own_vehicle,
                                     `1` = 'Ownership',
                                     `3` = 'No Access',
                                     .default = 'NA'),
                              levels = c('Ownership', 'No Access')))


### Electricity source
household <- household %>%
  mutate(electricitysource = case_when (electricitycon ==2 ~ 'No Access',
                                        electricitymains == 1 ~ 'Mains',
                                        electricitysource == 1 ~'Other', #paid
                                        electricitysource == 6 ~ 'Other',
                                        electricitysource == 3 ~'Generator/Battery',
                                        electricitysource == 5 ~'Generator/Battery',
                                        electricitysource == 4 ~'Solar Energy',
                                        electricitysource == 2 ~'NA',
                                        electricitysource > 6 ~ 'NA')) %>%
  mutate(electricitysource = factor(electricitysource, levels = c('Mains','Other','Generator/Battery','Solar Energy', 'No Access')))


### Lighting fuel
household <- household %>%
  mutate(lightingsource = factor(recode(lightingsource,
                                        `1` = 'Electricity Mains',
                                        `2` = 'Electricity Other',
                                        `3` = 'Gas/Paraffin/Candles',
                                        `4` = 'Gas/Paraffin/Candles',
                                        `7` = 'Gas/Paraffin/Candles',
                                        `9` = 'Solar Energy',
                                        .default = 'NA'),
                                 levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Candles','Solar Energy')))


### Cooking fuel
household <- household %>%
  mutate(cookingsource = factor(recode(cookingsource,
                                       `1` = 'Electricity Mains',
                                       `2` = 'Electricity Other',
                                       `3` = 'Gas/Paraffin/Coal',
                                       `4` = 'Gas/Paraffin/Coal',
                                       `6` = 'Gas/Paraffin/Coal',
                                       `5` = 'Wood/Animal Dung',
                                       `8` = 'Wood/Animal Dung',
                                       `9` = 'Solar Energy',
                                       `11` = 'No Access',
                                       .default = 'NA'),
                                levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Coal', 'Wood/Animal Dung',
                                           'Solar Energy','No Access')))


### Water heating fuel
household <- household %>%
  mutate(heatingsourcewater = factor(recode(heatingsourcewater,
                                            `1` = 'Electricity Mains',
                                            `2` = 'Electricity Other',
                                            `3` = 'Gas/Paraffin/Coal',
                                            `4` = 'Gas/Paraffin/Coal',
                                            `6` = 'Gas/Paraffin/Coal',
                                            `5` = 'Wood/Animal Dung',
                                            `8` = 'Wood/Animal Dung',
                                            `9` = 'Solar Energy',
                                            `11` = 'No Access',
                                            .default = 'NA'),
                                     levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Coal',
                                                'Wood/Animal Dung','Solar Energy','No Access')))

### Space heating fuel
household <- household %>%
  mutate(heatingsourcespace = factor(recode(heatingsourcespace,
                                            `1` = 'Electricity Mains',
                                            `2` = 'Electricity Other',
                                            `3` = 'Gas/Paraffin/Coal',
                                            `4` = 'Gas/Paraffin/Coal',
                                            `6` = 'Gas/Paraffin/Coal',
                                            `5` = 'Wood/Animal Dung',
                                            `8` = 'Wood/Animal Dung',
                                            `9` = 'Solar Energy',
                                            `11` = 'No Access',
                                            .default = 'NA'),
                                     levels = c('Electricity Mains','Electricity Other','Gas/Paraffin/Coal',
                                                'Wood/Animal Dung','Solar Energy','No Access')))




# create log expenditures variable
household$log_expenditures_hh <- log(household$hh_expenditures_USD_2014)
  


## 2. OLS ####
ols <- lm(burden_CO2_national ~  log_expenditures_hh + province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh +
             own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
             heatingsourcewater + heatingsourcespace, na.action=na.exclude, data=household)

summary(ols)

write.csv(summary(ols)['coefficients'],file='ols_result.csv')



## 3. LOGIT ####
# households that are more affected than 90%

# Full sample
household_0 <- subset(household)

household_0$burden_decile <- ntile(household_0$burden_CO2_national, 10)

household_0 <- household_0 %>%
  mutate(burden_decile = as.factor(case_when(burden_decile <= 9 ~ FALSE,
                                             burden_decile == 10 ~ TRUE)))

household_0$log_expenditures_hh <- log(household_0$hh_expenditures_USD_2014)

# Create new tables with expenditure quintiles
quintiles <- c(1, 2, 3, 4, 5)

for (i in quintiles) {
  name <- paste0("household_", i)
  assign(name, household %>% filter(Income_Group_5 == i))
  
  var <- get(name)
  var$burden_decile <- ntile(var$burden_CO2_national, 10)
  var <- var %>%
    mutate(burden_decile = as.factor(case_when(burden_decile <= 9 ~ FALSE,
                                               burden_decile == 10 ~ TRUE)))
  var$log_expenditures_hh <- log(var$hh_expenditures_USD_2014)
  assign(name, var)
}



# logistic regression - data: household_0 = full sample , household_1 = first quintile, ...
log_reg <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
              own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
              heatingsourcewater + heatingsourcespace, na.action=na.exclude, data=household_0, family="binomial"(link=logit))

summary(log_reg)

# Mc Faddens R-squared
with(summary(log_reg), 1 - deviance/null.deviance)

# save results as csv
results <- summary(log_reg)$coefficients
variable_name <- rownames(results)
intercept <- results[, 1]
standard_error <- results[, 2]
signif <- results[, 4]
signif <- ifelse(signif <= 0.001, "***", ifelse(signif <= 0.01, "**", ifelse(signif <= 0.05, "*", "")))

log_table <- data.frame(variable_name, intercept, standard_error, signif)
write.csv(log_table, file="log_reg_result.csv")



## 4. Summary Regression Results ####

# logistic regression - data: household_0 = full sample
log_reg <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                 own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
                 heatingsourcewater + heatingsourcespace, na.action=na.exclude, data=household_0, family="binomial"(link=logit))

summary(log_reg)


pro_reg <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                 own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
                 heatingsourcewater + heatingsourcespace, na.action=na.exclude, data=household_0, family="binomial"(link=probit))


summary(pro_reg)


library(stargazer)

stargazer(ols, log_reg, pro_reg, type ="text")




## 5. Marginal Effects ####

library(mfx)

# Fit logistic regression using logitmfx
logit_mfx_result <- logitmfx(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                                  own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
                                  heatingsourcewater + heatingsourcespace, data=household_0, atmean=FALSE)

summary(logit_mfx_result)


# Extract the marginal effects (dF/dx) and the corresponding variable names
mfx_values <- logit_mfx_result$mfxest[, "dF/dx"]
var_names <- rownames(logit_mfx_result$mfxest)
p_value <- logit_mfx_result$mfxest[, "P>|z|"]

# Create a data frame with the marginal effects
mar_gg <- data.frame(
  factor = var_names,
  AME = mfx_values
)

# Constants for a 95% confidence interval
z_value_95 <- qnorm(0.975)

# Calculate the lower and upper bounds of the confidence intervals
mar_gg$lower <- mar_gg$AME - z_value_95 * logit_mfx_result$mfxest[, "Std. Err."]
mar_gg$upper <- mar_gg$AME + z_value_95 * logit_mfx_result$mfxest[, "Std. Err."]


# Add the p-values to the mar_gg data frame
mar_gg$p <- p_value

# Filter p <= 0.1
mar_gg <- mar_gg %>%
  filter(p <= 0.1)

# Select the desired columns
mar_gg <- mar_gg[, c("factor", "AME", "lower", "upper")]
head(mar_gg)


# Change Prefixes

## Replace
replacements <- c(
  cookingsource = "Cooking Fuel: ",
  electricitysource = "Electricity Source: ",
  own_vehicle = "Car: ",
  education_hhh = "Education: ",
  ethnicity_hhh = "Ethnicity:",
  urban_1 = "Res. Area: ",
  log_expenditures_hh = "Expenditures (log)",
  heatingsourcewater = "Heating water: ",
  heatingsourcespace = "Heating space: "
)

for (prefix in names(replacements)) {
  mar_gg$factor <- gsub(paste0("^", prefix), replacements[prefix], mar_gg$factor)
}

## Strip
prefixes_to_strip <- c("province", "gender_hhh") # Add more as needed

for (prefix in prefixes_to_strip) {
  mar_gg$factor <- gsub(paste0("^", prefix), "", mar_gg$factor)
}


mar_gg <- mar_gg %>% 
  mutate(effect_color = ifelse(AME < 0, "Negative", "Positive"))


p <- ggplot(data = mar_gg, aes(x = reorder(factor, AME * -1), y = AME, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_linerange(aes(ymin = lower, ymax = upper), color = "black") + 
  geom_errorbar(aes(y = lower, ymin = lower, ymax = lower), color = "black", width = 0.5) +
  geom_errorbar(aes(y = upper, ymin = upper, ymax = upper), color = "black", width = 0.5) +
  geom_point(aes(fill = effect_color), shape = 22, size = 3, color = "black") + # squares with black border and colored fill
  coord_flip() +
  labs(x = NULL, y = "Average Marginal Effect of being within 10% most affected Households") +
  scale_fill_manual(values = c("Negative" = "#9b2226", "Positive" = "#94d2bd")) + # notice the use of scale_fill_manual
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"))

p



# Clean Environment
rm(list=ls())





