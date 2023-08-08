library(tidyverse)
library(readr)
library(data.table)
library(ggplot2)
library(Hmisc)
library(gridExtra)

options(scipen=999)

#### CHANGE PATH ####
# Set working directory Agatha
setwd("C:/Users/agath/Desktop/Code GitHub/LCS_results")

# Load data - LCS_results
carbon_pricing    <- read_csv("Carbon_Pricing_Incidence_South Africa.csv")
fuel_expenditures <- read_csv("fuel_expenditures_South Africa.csv") 
hh_info           <- read_csv("household_information_South Africa.csv")

# Create one dataframe with all information
hh_fuel_carbon <- left_join(carbon_pricing, fuel_expenditures)
hh_final <- left_join(hh_fuel_carbon, hh_info)

# save dataframe 
write.csv(hh_final, "hh_final_LCS.csv")

rm(list = setdiff(ls(), "hh_final"))


#---------------------------------------------------------------------------------------------------------

# Farben

# rot      organge     türkis    blau
#"#9b2226","#ca6702","#94d2bd","#194A84"


## Share of expenditure categories over expenditure deciles ####
summary_expenditures_categories <- hh_final %>%
  group_by(Income_Group_10) %>%
  summarise(Services = mean(share_services), Goods = mean(share_goods), Food = mean(share_food), Energy = mean(share_energy)  ) %>%
  pivot_longer(cols=2:5, names_to = "category", values_to = "percentage")

summary_expenditures_categories$percentage <- summary_expenditures_categories$percentage * 100

ggplot(summary_expenditures_categories, aes(x = Income_Group_10, y = percentage, fill = category)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw()+
  labs (x = "Expenditure Deciles", y = "Expenditure Share (%)", title = "Share of Expenditures for different Categories", fill = "Expenditure Category") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  #scale_x_discrete(limits = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  scale_x_discrete(limits = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) +
  scale_fill_manual(values=c("#9b2226","#ca6702","#94d2bd","#194A84"))+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))

# dataframe
pivoted_df = pivot_wider(summary_expenditures_categories, names_from = category, values_from = percentage)

# check and write
print(pivoted_df)
write.csv(pivoted_df, "pivoted_categories_share.csv", row.names = FALSE)



## Distribution CO2 footprint ####

# Barplot #not used
ggplot(hh_final, aes(x = CO2_t_national)) +
  geom_histogram(aes(y = (..density..)*1000), binwidth = 10, fill = "#ca6702", color ="black", position = position_nudge(x=5))+
  geom_vline(xintercept = mean(hh_final$CO2_t_national), 
             color = "black", linetype = "dashed", size = 0.5) +
  labs(x = "CO2 emissions (t/household)", 
       y = "Share of all households (%)", 
       title = "Distribution of CO2 Footprint") +
  scale_x_continuous(breaks = seq(0, 200, by = 10), minor_breaks = seq(0, 200, by = 10)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        panel.grid.major = element_line(colour = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank())


# Boxplot #not used
ggplot(hh_final, aes(x = CO2_t_national)) +
  geom_boxplot(fill = "#ca6702", color = "#4C4C4C", alpha = 0.5, size = 0.4, width=0.1) +
  stat_boxplot(geom = "errorbar", width = 0.02, coef = 1.5, color = "black") +
  geom_point(aes(y=0, x = mean(hh_final$CO2_t_national)), shape = 23, size = 1.5, fill = "black")+
  labs(y = "", 
       x = "CO2 emissions (t/household)", 
       title = "Distribution of CO2 Footprint") +
  coord_trans(y = "reverse") +
  scale_x_continuous(breaks = seq(0, 200, by = 10), minor_breaks = seq(0, 200, by = 10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"),
        panel.grid.major = element_line(colour = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank())


## BAR CHART national carbon footprint ####
CO2_tons$Income_Group_10 <- factor(CO2_tons$Income_Group_10)

CO2_tons <- hh_final %>% 
  group_by(Income_Group_10) %>%
  summarise(t_national = mean(CO2_t_national))

ggplot(CO2_tons, aes(x = Income_Group_10, y = t_national)) +
  geom_bar(stat = "identity", fill="#ca6702", color="black") +
  theme_bw()+
  labs(title = "Carbon Footprint per Household", x = "Expenditure Deciles", y = "Mean Carbon Footprint (tCO2/household)") +
  #scale_x_discrete(limits = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  scale_x_discrete(limits = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))

summary(hh_final$CO2_t_national)

# dataframe
write.csv(CO2_tons, "CO2_tons.csv", row.names = FALSE)



# ## DOT PLOT Average carbon price burden for a global carbon price (red) and a national carbon price (blue) over deciles ####
# # not used #
# income_C02_averages_national <- hh_final %>%
#   group_by(Income_Group_10) %>%
#   summarise(national = mean(burden_CO2_national))
# 
# income_C02_averages_global <- hh_final %>%
#   group_by(Income_Group_10) %>%
#   summarise(global = mean(burden_CO2_global))
# 
# average_carbon_expenses <- left_join(income_C02_averages_global, income_C02_averages_national) %>% 
#   pivot_longer(-Income_Group_10) %>% 
#   rename("region" = "name",
#          "burden" = "value") %>%
#   mutate(income_group = factor(Income_Group_10, levels = as.character(1:10)))
# 
# ggplot(average_carbon_expenses, aes(x = income_group, y = burden, color = region)) +
#   geom_point() +
#   scale_y_continuous(labels = scales::label_percent(), "Carbon Price Burden (% of total budget)") +
#   labs(title = "Income Deciles versus Global Carbon Pricing Burden",
#        x = "Expenditure Decile")


## Distribution of a national carbon price burden within each income decile ####
hh_final_1 <- hh_final %>%
  group_by(Income_Group_10)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_1, aes(x = factor(Income_Group_10), fill=""))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "black")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  #scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2")+
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  scale_fill_manual(values=c("#ca6702"))+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))

# dataframe
data <- hh_final_1 %>%
  select(Income_Group_10, mean)

write.csv(data, "Additional_costs_deciles.csv", row.names = FALSE)




## Carbon price burden in different social groups - Urban/Rural ####
hh_final$urban_1 <- as.factor(hh_final$urban_1)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, urban_1)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = urban_1))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=urban_1), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Residential Areas")+
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("Urban formal", "Urban informal", "Rural traditional", "Rural formal")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
   theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

# dataframe
data <- hh_final_2 %>%
  select(Income_Group_5, mean, urban_1)%>%
  pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = urban_1)
write.csv(data, "Additional_costs_urb.csv", row.names = FALSE)


## Carbon price burden in different social groups - Province #####
hh_final$province <- as.factor(hh_final$province)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_3, province)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_3), fill = province))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Tertiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=province), shape = 23, size = 1, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 33 Percent", "2", "3 \n Richest \n 33 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Provinces")+
  scale_fill_manual(values=c("#F8766D", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#006147"), labels = c("Western Cape", "Eastern Cape","Northern Cape","Free State","KwaZulu-Natal","North West","Gauteng","Mpumalanga","Limpopo")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

# dataframe
data <- hh_final_2 %>%
  select(Income_Group_3, mean, province)%>%
  pivot_wider(names_from = Income_Group_3, values_from = mean, id_cols = province)
write.csv(data, "Additional_costs_pro.csv", row.names = FALSE)

# Table
#nrow(hh_final[hh_final$province == '5' & hh_final$Income_Group_5 == '1', ]) 


# # province on x
# hh_final$province <- as.factor(hh_final$province)
# 
# hh_final_pr <- hh_final %>%
#   group_by(province)%>%
#   summarise(
#     y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
#     y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
#     y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
#     y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
#     y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
#     mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
#   ungroup()
# 
# ggplot(hh_final_pr, aes(x = factor(province), color = province))+
#   geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
#   theme_bw()+
#   xlab("Provinces")+
#   ylab("Additional Costs (% of total expenditures)")+
#   geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
#   coord_cartesian(ylim = c(0,0.2))+
#   ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Provinces")+
#   scale_color_manual(values=c("#F8766D", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#006147"), labels = c("Western Cape", "Eastern Cape","Northern Cape","Free State","KwaZulu-Natal","North West","Gauteng","Mpumalanga","Limpopo")) + 
#   theme(axis.text = element_text(size = 8), 
#         axis.title = element_text(size = 8),
#         plot.title = element_text(size = 11),
#         plot.subtitle = element_text(size = 10),
#         legend.position = "bottom",
#         plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
#   labs(colour = "")


## Carbon price burden in different social groups - Ethnicity ####
hh_final$ethnicity_hhh <- as.factor(hh_final$ethnicity_hhh)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, ethnicity_hhh)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = ethnicity_hhh))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=ethnicity_hhh), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.31))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Ethnicity")+
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("African Black", "Coloured", "Indian/Asian", "White")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

  # dataframe
data <- hh_final_2 %>%
  select(Income_Group_5, mean, ethnicity_hhh)%>%
  pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = ethnicity_hhh)
write.csv(data, "Additional_costs_eth.csv", row.names = FALSE)


# ethnicity in general - not used
hh_final$ethnicity_hhh <- as.factor(hh_final$ethnicity_hhh)

hh_final_et <- hh_final %>%
  group_by(ethnicity_hhh)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_et, aes(x = factor(ethnicity_hhh), color = ethnicity_hhh))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("")+
  ylab("Additional Costs (% of Total Expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.21))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Ethnicity")+
  scale_color_manual(values=c("#F8766D", "#00BFC4", "#E69F00", "#009E73"), labels = c("African Black", "Coloured", "Indian/Asian", "White")) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+
  labs(colour = "")


## Carbon price burden in different social groups - Gender #### - not used
hh_final$gender_hhh <- as.factor(hh_final$gender_hhh)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, gender_hhh)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = gender_hhh))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=gender_hhh), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.21))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Gender")+
  scale_fill_manual(values=c("#F8766D", "#009E73"), labels = c("Male", "Female")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")

  # dataframe
  data <- hh_final_2 %>%
  select(Income_Group_5, mean, gender_hhh)%>%
  pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = gender_hhh)
  write.csv(hh_final_2, "Additional_costs_gender.csv", row.names = FALSE)
  
  print(sum(hh_final$gender_hhh == 2))
  print(sum(hh_final$gender_hhh == 1))

# Table
nrow(hh_final[hh_final$gender_hhh == '3' & hh_final$Income_Group_5 == '1', ]) 


## Access to Electricity ####

hh_final$electricitycon <- as.factor(hh_final$electricitycon)

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, electricitycon)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  na.omit()

hh_final_2 <- hh_final_2 %>%
  filter(!electricitycon == 9)

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = electricitycon))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group=electricitycon), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Electricity Access")+
  scale_fill_manual(values=c("#ca6702","#194A84"), labels = c("Access", "No Access")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")
  
  # dataframe
data <- hh_final_2 %>%
  select(Income_Group_5, mean, electricitycon)%>%
  pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = electricitycon)
  write.csv(data, "Additional_costs_elec_ac.csv", row.names = FALSE)


  
  
  
## Free Electricity ####
  
# 4403 / 16701 = 26.4 % 
length(which(hh_final$electricityfree==2))
  
table(hh_final$Income_Group_5[hh_final$electricityfree == 1])

check <- hh_final %>%
  filter(electricityfree %in% c(1))




hh_final_1 <- hh_final %>%
filter(electricityfree %in% c(1, 2))
  
hh_final_1$electricityfree <- as.factor(hh_final_1$electricityfree)
  
hh_final_2 <- hh_final_1 %>%
    group_by(Income_Group_5, electricityfree)%>%
    summarise(
      y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
    ungroup()
  
  ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = electricityfree))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
    theme_bw()+
    xlab("Expenditure Quintiles")+
    ylab("Additional Costs (% of total expenditures)")+
    geom_point(aes(y = mean, group=electricityfree), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
    coord_cartesian(ylim = c(0,0.21))+
    ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Free Electricity Acess")+
    scale_fill_manual(values=c("#F8766D", "#009E73"), labels = c("Free", "Paid")) + 
    guides(fill = guide_legend(title = NULL)) +  # remove legend title
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14),
          legend.position = "bottom",
          plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
    labs(colour = "")
  
  # dataframe
  data <- hh_final_2 %>%
    select(Income_Group_5, mean, electricityfree)%>%
    pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = electricityfree)
  write.csv(data, "Additional_costs_elec_free.csv", row.names = FALSE)

  
  
  
## Connected to MAINS #### not used

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5, electricitymains)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  na.omit()

hh_final_2 <- hh_final_2 %>%
  filter(!electricitymains > 2)

hh_final_2$electricitymains <- as.factor(hh_final_2$electricitymains)

ggplot(hh_final_2, aes(x = factor(Income_Group_5), fill = electricitymains))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group = electricitymains), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Electricity Source")+
  scale_fill_manual(values=c("#ca6702","#194A84"), labels = c("MAINS", "Other")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")
  
  # dataframe
data <- hh_final_2 %>%
  select(Income_Group_5, mean, electricitymains)%>%
  pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = electricitymains)

  write.csv(data, "Additional_costs_elec_mains.csv", row.names = FALSE)






## Burden CO2 - electricity / transport ####

hh_final_1 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

hh_final_1$category <- "burden_CO2_national"

hh_final_2 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_electricity, weights = hh_weights))%>%
  ungroup()

hh_final_2$category <- "burden_CO2_electricity"

hh_final_3 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_transport, weights = hh_weights))%>%
  ungroup()

hh_final_3$category <- "burden_CO2_transport"


hh_burden <- merge(hh_final_1, hh_final_2, all=TRUE)
hh_burden2 <- merge(hh_burden, hh_final_3, all=TRUE)

hh_burden2$category <- factor(hh_burden2$category, levels = c("burden_CO2_national","burden_CO2_electricity","burden_CO2_transport"))


ggplot(hh_burden2, aes(x = factor(Income_Group_5), fill = category))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Additional Costs (% of total expenditures)")+
  geom_point(aes(y = mean, group = category), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.151))+
  ggtitle("Additional Costs with a National Carbon Tax of US$ 30/tCO2 by Energy Sources")+
  scale_fill_manual(values=c("#9b2226","#ca6702","#194A84"), labels = c("Burden (total)", "Burden Electricity", "Burden Transport")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")
  
  # dataframe
data <- hh_burden_2 %>%
  select(Income_Group_5, mean, category)%>%
  pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = category)
  write.csv(hh_burden2, "Additional_costs_ener.csv", row.names = FALSE)



## Income Deciles with mean expenditures ####

hh_final_dec <- hh_final %>%
  group_by(Income_Group_10) %>%
  summarise(
  mean_hh_expendituresUSD = mean(hh_expenditures_USD_2014)) %>%
  ungroup()

# dataframe
write.csv(hh_final_dec, "Expenditure Deciles.csv", row.names = FALSE)

# Clean Environment
rm(list=ls())
