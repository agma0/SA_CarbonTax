library(tidyverse)
library(dplyr)
library(fixest)
library(stargazer)


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
         electricitysource, electricityfree, lightingsource, cookingsource, heatingsourcewater, heatingsourcespace, education_hhh, ethnicity_hhh, gender_hhh, own_stove, own_vehicle)




# make table of all variables and count occurances
# excluded_columns <- c("hh_id", "hh_expenditures_USD_2014", "exp_CO2_national", "burden_CO2_national", "Income_Group_5")
# 
# result <- household %>%
#   select(-all_of(excluded_columns)) %>%   # Drop excluded columns
#   gather(column_name, value) %>%           # Convert dataframe to long format
#   group_by(column_name, value) %>%         # Group by column name and value
#   summarise(Count = n()) %>%               # Count occurrences
#   mutate(Percentage = (Count / sum(Count)) * 100) %>% # Calculate percentage
#   arrange(column_name, as.numeric(value))  # Arrange results
# 
# print(result)
# 
# write.csv(result, "result.csv", row.names = FALSE)



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

### Gender household head
household <- household %>%
  mutate(gender_hhh = factor(recode(gender_hhh,
                                    `1` = 'Male',
                                    `2` = 'Female'),
                             levels = c('Male', 'Female')))



### Ethnicity household head
household <- household %>%
  mutate(ethnicity_hhh = factor(recode(ethnicity_hhh,
                                       `1` = 'African Black',
                                       `2` = 'Coloured',
                                       `3` = 'Indian/Asian',
                                       `4` = 'White'),
                                levels = c('African Black','Coloured','Indian/Asian','White')))




### Education ISCED codes
household <- household %>%
  mutate(education_hhh = factor(recode(education_hhh, 
                                       `1` = 'Pre-primary Education',
                                       `2` = 'Primary Education',
                                       `3` = 'Primary Education',
                                       `4` = 'Primary Education',
                                       `5` = 'Primary Education',
                                       `6` = 'Lower Secondary Education',
                                       `7` = 'Lower Secondary Education',
                                       `8` = 'Lower Secondary Education',
                                       `9` = 'Upper Secondary Education',
                                       `10` = 'Upper Secondary Education',
                                       `11` = 'Upper Secondary Education',
                                       `12` = 'Upper Secondary Education',
                                       `13` = 'Upper Secondary Education',
                                       `14` = 'Upper Secondary Education',
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
                                       `98` = 'No Schooling',
                                       .default = 'Other'),
                                levels = c("Upper Secondary Education",'No Schooling','Pre-primary Education','Primary Education', 'Lower Secondary Education',
                                           'Post-secondary Non-tertiary Education','Short-cycle Tertiary Education',
                                           'Bachelors or Equivalent','Masters or Equivalent', 'Other')))


### Ownership stove, car
household <- household %>%
  mutate(own_vehicle = factor(recode(own_vehicle,
                                     `1` = 'Ownership',
                                     `3` = 'No Access',
                                     .default = 'Other'),
                              levels = c('Ownership', 'No Access','Other')))

household <- household %>%
  mutate(own_stove = factor(recode(own_stove,
                                   `1` = 'Ownership',
                                   `3` = 'No Access',
                                   .default = 'Other'),
                            levels = c('Ownership', 'No Access','Other')))




### Electricity source
household <- household %>%
  mutate(electricitysource = case_when (electricitycon ==2 ~ 'No Access',
                                        electricitymains == 1 ~ 'Mains',
                                        electricitysource == 1 ~'Other', #connected to other source e.g. neighbour -paid
                                        electricitysource == 2 ~'Other', #connected to other source e.g. neighbour -not paid
                                        electricitysource == 3 ~'Other', #generator 7hh
                                        electricitysource == 5 ~'Other', #battery 1hh
                                        electricitysource == 4 ~'Solar Energy', #home solar system
                                        electricitysource == 6 ~ 'Other', #other
                                        electricitysource > 6 ~ 'Other')) %>%
  mutate(electricitysource = factor(electricitysource, levels = c('Mains','Solar Energy', 'No Access', 'Other')))


# Free electricity - government program
household <- household %>%
  mutate(electricityfree = factor(recode(electricityfree,
                                         `1` = 'Free Electricity',
                                         `2` = 'No Free Electricity',
                                         .default = 'Other'),
                                  levels = c('No Free Electricity', 'Free Electricity', 'Other')))




### Lighting fuel
household <- household %>%
  mutate(lightingsource = factor(recode(lightingsource,
                                        `1` = 'Electricity Mains',
                                        `2` = 'Electricity Other',
                                        `3` = 'Other', #gas 9hh
                                        `4` = 'Paraffin', #parrafin
                                        `7` = 'Candles',
                                        `9` = 'Solar Energy',
                                        .default = 'Other'),
                                 levels = c('Electricity Mains','Electricity Other','Paraffin','Candles','Solar Energy','Other')))


### Cooking fuel
household <- household %>%
  mutate(cookingsource = factor(recode(cookingsource,
                                       `1` = 'Mains',
                                       `2` = 'Electricity Other',
                                       `3` = 'Gas',
                                       `4` = 'Paraffin',
                                       `5` = 'Wood',
                                       `6` = 'Coal',
                                       `8` = 'Animal Dung',
                                       `9` = 'Other',
                                       `11` = 'No Access',
                                       .default = 'Other'),
                                levels = c('Mains','Electricity Other','Gas', 'Paraffin', 'Coal','Wood','Animal Dung',
                                           'Solar Energy','No Access','Other')))


### Water heating fuel
household <- household %>%
  mutate(heatingsourcewater = factor(recode(heatingsourcewater,
                                            `1` = 'Mains',
                                            `2` = 'Electricity Other',
                                            `3` = 'Gas',
                                            `4` = 'Paraffin',
                                            `6` = 'Coal',
                                            `5` = 'Wood',
                                            `8` = 'Animal Dung',
                                            `9` = 'Solar Energy',
                                            `11` = 'No Access',
                                            .default = 'Other'),
                                     levels = c('Mains','Electricity Other','Gas','Paraffin','Coal',
                                                'Wood','Animal Dung','Solar Energy','No Access','Other')))

### Space heating fuel
household <- household %>%
  mutate(heatingsourcespace = factor(recode(heatingsourcespace,
                                            `1` = 'Mains',
                                            `2` = 'Electricity Other',
                                            `3` = 'Gas',
                                            `4` = 'Paraffin',
                                            `6` = 'Coal',
                                            `5` = 'Wood',
                                            `8` = 'Animal Dung',
                                            `9` = 'Solar Energy',
                                            `11` = 'No Access',
                                            .default = 'Other'),
                                     levels = c('Mains','Electricity Other','Gas','Paraffin','Coal',
                                                'Wood','Animal Dung','Solar Energy','No Access','Other')))




# create log expenditures variable
household$log_expenditures_hh <- log(household$hh_expenditures_USD_2014)
  


## 2. OLS ####
ols <- lm(burden_CO2_national ~  log_expenditures_hh + province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh +
             own_vehicle + own_stove + electricitysource + lightingsource + cookingsource + 
             heatingsourcewater + heatingsourcespace, data=household)

summary(ols)

write.csv(summary(ols)['coefficients'],file='ols_result.csv')

stargazer(ols, title="OLS Regression Results", label="tab:olsresults", out="ols_results.tex")

stargazer(ols, type ="text")



# Generate LaTeX code with stargazer
stargazer(ols, title="Regression Results", type="latex",
          column.labels=c("Estimate", "Std. Error", "Signif. codes"),
          omit.stat=c("f", "adj.rsq", "rsq", "aic", "bic", "ser"),
          single.row=TRUE,
          header=FALSE)



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
              own_vehicle + own_stove + electricitysource + electricityfree + lightingsource + cookingsource + 
              heatingsourcewater + heatingsourcespace, na.action=na.exclude, data=household_5, family="binomial"(link=logit))

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
log_reg0 <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                 own_vehicle + own_stove + electricitysource + electricityfree + lightingsource + cookingsource + 
                 heatingsourcewater + heatingsourcespace, data=household_0, family="binomial"(link=logit))

log_reg1 <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                  own_vehicle + own_stove + electricitysource + electricityfree + lightingsource + cookingsource + 
                  heatingsourcewater + heatingsourcespace, data=household_1, family="binomial"(link=logit))

log_reg2 <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                  own_vehicle + own_stove + electricitysource + electricityfree + lightingsource + cookingsource + 
                  heatingsourcewater + heatingsourcespace, data=household_2, family="binomial"(link=logit))

log_reg3 <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                  own_vehicle + own_stove + electricitysource + electricityfree + lightingsource + cookingsource + 
                  heatingsourcewater + heatingsourcespace, data=household_3, family="binomial"(link=logit))

log_reg4 <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                  own_vehicle + own_stove + electricitysource + electricityfree + lightingsource + cookingsource + 
                  heatingsourcewater + heatingsourcespace, data=household_4, family="binomial"(link=logit))

log_reg5 <- glm(burden_decile ~ log_expenditures_hh +  province + urban_1 + gender_hhh + ethnicity_hhh + education_hhh + 
                  own_vehicle + own_stove + electricitysource + electricityfree + lightingsource + cookingsource + 
                  heatingsourcewater + heatingsourcespace, data=household_5, family="binomial"(link=logit))

summary(log_reg0)



stargazer(log_reg0, log_reg1, log_reg2, log_reg3, log_reg4, log_reg5, type ="text")

# stargazer(log_reg0, log_reg1, log_reg2, log_reg3, log_reg4, log_reg5, type ="latex", out="log_reg.tex")
# 
# 
# 
# # Generate LaTeX code with stargazer
# stargazer(log_reg0, log_reg1, log_reg2, log_reg3, log_reg4, log_reg5, 
#           title="Logistic Regression Results",
#           type="latex", 
#           out="log_reg.tex",
#           column.labels=c("Full Sample", "1st Quintile", "2nd Quintile", "3rd Quintile", "4th Quintile", "5th Quintile"),
#           omit.stat=c("f", "aic", "bic", "lr"),
#           single.row=TRUE,
#           header=FALSE)
# 
# 
# # Capture stargazer output
# capture.output(stargazer(log_reg0, log_reg1, log_reg2, log_reg3, log_reg4, log_reg5, 
#                          title="Logistic Regression Results",
#                          type="text", 
#                          column.labels=c("Full Sample", "1st Quintile", "2nd Quintile", "3rd Quintile", "4th Quintile", "5th Quintile"),
#                          omit.stat=c("f", "aic", "bic", "lr"),
#                          single.row=TRUE,
#                          header=FALSE, 
#                          digits=4),
#                file = "log_reg.csv")














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
#mar_gg <- mar_gg %>%
#  filter(p <= 0.1)

# Select the desired columns
mar_gg <- mar_gg[, c("factor", "AME", "lower", "upper")]
head(mar_gg)


# Change Prefixes

## Replace
replacements <- c(
  cookingsource = "Cooking Fuel: ",
  electricitysource = "Electricity Source: ",
  #free elec
  own_vehicle = "Car: ",
  own_stove = "Stove: ",
  education_hhh = "Education: ",
  ethnicity_hhh = "Ethnicity:",
  urban_1 = "Res. Area: ",
  gender_hhh = "Gender: ",
  province = "Province: ",
  log_expenditures_hh = "Expenditures (log)",
  heatingsourcewater = "Heating Water: ",
  heatingsourcespace = "Heating Space: ",
  lightingsource = "Lighting Fuel: "
)

for (prefix in names(replacements)) {
  mar_gg$factor <- gsub(paste0("^", prefix), replacements[prefix], mar_gg$factor)
}




mar_gg <- mar_gg %>% 
  mutate(effect_color = ifelse(AME < 0, "Negative", "Positive"))



# Specify the desired order of the prefixes (change this to match your desired order)
prefix_order <- c(
  "Expenditures (log)","Province:", "Res. Area:",  "Education:", "Ethnicity:", "Gender:", "Car:", "Stove:", "Electricity Source:",  "Lighting Fuel:", "Cooking Fuel:", 
    "Heating Water:", "Heating Space:"
)

# Custom function to map each factor to its prefix's index in 'prefix_order'
get_prefix_order <- function(factor) {
  for (prefix in prefix_order) {
    if (startsWith(factor, prefix)) {
      return(match(prefix, prefix_order))
    }
  }
  return(length(prefix_order) + 1) # If no prefix matches, assign the last order
}


# Assign prefix order
mar_gg$prefix_order <- sapply(as.character(mar_gg$factor), get_prefix_order)

# Order by the prefix and then by the AME within each prefix
mar_gg <- mar_gg[order(mar_gg$prefix_order, mar_gg$AME),]

# Reorder the factor levels based on the sorted DataFrame, then reverse them
mar_gg$factor <- factor(mar_gg$factor, levels = rev(unique(mar_gg$factor)))



# Limit the 'lower' and 'upper' columns to be within the range of -1 to 1
#mar_gg$lower <- pmin(pmax(mar_gg$lower, -1), 1)
#mar_gg$upper <- pmin(pmax(mar_gg$upper, -1), 1)

###
# heating water no access deleted because se over 200
mar_gg <- mar_gg[mar_gg$factor != "Heating water: No Access", ]


  

p <- ggplot(data = mar_gg, aes(x = factor, y = AME, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_linerange(aes(ymin = lower, ymax = upper), color = "black") + 
  geom_errorbar(aes(y = lower, ymin = lower, ymax = lower), color = "black", width = 0.5) +
  geom_errorbar(aes(y = upper, ymin = upper, ymax = upper), color = "black", width = 0.5) +
  geom_point(aes(fill = effect_color), shape = 22, size = 3, color = "black") + # squares with black border and colored fill
  coord_flip() +
  labs(x = NULL, y = "Average Marginal Effect of being within 10% most affected Households") +
  scale_fill_manual(values = c("Negative" = "#9b2226", "Positive" = "#94d2bd")) + # notice the use of scale_fill_manual
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1)) + # Add this line to control the y-axis breaks
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"))

p





# Clean Environment
rm(list=ls())





