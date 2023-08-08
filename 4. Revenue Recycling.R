## 4. Revenue Recycling ####

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


# read dataframe hh_final
hh_final    <- read_csv("hh_final_LCS.csv")



#### Revenue Recycling - Total Burden, Lump Sum 100%, Compensation CO2 tax electricity 100%

# total burden
hh_final_0 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

#negativ
hh_final_0n <- hh_final_0 *(-1)
hh_final_0n$Income_Group_5 <- hh_final_0n$Income_Group_5 *(-1)

hh_final_0n$category <- "burden_CO2_national"


## Lump Sum ##
CP_per_capita = sum(hh_final$exp_CO2_national) / nrow(hh_final)

#hh_final$burden_CO2_national_revenue <- (hh_final$exp_CO2_national - CP_per_capita) / hh_final$hh_expenditures_USD_2014
hh_final$burden_CO2_national_revenue100 <- ((hh_final$exp_CO2_national - CP_per_capita) / hh_final$hh_expenditures_USD_2014) *(-1)

hh_final_1 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_revenue100, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_CO2_national_revenue100, weights = hh_weights))%>%
  ungroup()

hh_final_1$category <- "burden_CO2_revenue100"


## Electricity Subsidy Carbon tax ##
sum_electricity_co2 <- sum(hh_final[["exp_CO2_electricity"]])

# Net income gain elec sub
hh_final$burden_elec_sub <- ((hh_final$exp_CO2_national - hh_final$exp_CO2_electricity) / hh_final$hh_expenditures_USD_2014) *(-1)


hh_final_2 <- hh_final %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_elec_sub, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(burden_elec_sub, weights = hh_weights))%>%
  ungroup()

hh_final_2$category <- "burden_elec_sub"

hh_rr <- merge(hh_final_1, hh_final_2, all=TRUE)
hh_rr2 <- merge(hh_rr, hh_final_0n, all=TRUE)


hh_rr22$category <- factor(hh_elec_sub2$category, levels = c("burden_CO2_national","burden_CO2_revenue100", "burden_elec_sub"))


 
#plot
vs <- ggplot(hh_rr2, aes(x = factor(Income_Group_5), fill = category))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+
  ylab("Net Income Gain (% of total expenditures)")+
  geom_point(aes(y = mean, group=category), shape = 23, size = 1.5, fill = "black", position=position_dodge(width=0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.21,0.62))+
  ggtitle("Revenue Recycling with National Carbon Tax of US$ 30t/CO2")+
  scale_fill_manual(values=c("#9b2226", "#00BFC4", "#669999"), labels = c("Total Burden", "Full Lump Sum","Full CO2 Tax Electricity Compensation")) + 
  guides(fill = guide_legend(title = NULL)) +  # remove legend title
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(0.1, 0.3, 0.1, 0.3, "cm"))+ 
  labs(colour = "")   

vs + geom_hline(yintercept=0)




# dataframe

data <- hh_rr2 %>%
  select(Income_Group_5, mean, category)%>%
  pivot_wider(names_from = Income_Group_5, values_from = mean, id_cols = category)

write.csv(data, "Revenue Recycling.csv", row.names = FALSE)


# Clean Environment
rm(list=ls())
