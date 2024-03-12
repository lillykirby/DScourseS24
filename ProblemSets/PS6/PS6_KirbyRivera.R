library(tidyverse)
library(ggplot2)
library(dplyr)

salesmonthly <- read_excel("C:/Users/lilly/OneDrive/Desktop/salesmonthly.xlsx")
View(salesmonthly)

#Reformat date to remove day:
salesmonthly$date <- format(salesmonthly$datum, "%m-%Y")
#Get monthly data for 2018:
salesmonthly_clean <- salesmonthly %>% filter(datum >= "2018-01-01") %>% filter(datum <= "2018-12-31")
                     
#Graph one of monthly sales data for antihistamines:
ggplot(salesmonthly_clean, aes(x = date, y = R06, group = 1)) +
  geom_point() +
  geom_line() +
  labs(title = "Antihistamine (for Systemic Use) Monthly Sales",
       x = "Month",
       y = "Antihistamine Sales") +
  theme_minimal()

#Generate total product sales share data:
selected_columns <- c("M01AB", "M01AE", "N02BA", "N02BE", "N05B", 
                      "N05C", "R03", "R06")
totalprodsales <- colSums(salesmonthly_clean[, selected_columns, drop = FALSE])
pie(totalprodsales, main = "Total 2018 Sales by Product Category")

#Sales by month for top two groups:
ggplot(salesmonthly_clean, aes(x = date)) +
  geom_bar(aes(y = N02BE, fill = "N02BE"), stat = "identity", position = "dodge", color = "black") +
  geom_bar(aes(y = N05B, fill = "N05B"), stat = "identity", position = "dodge", color = "black") +
  ggtitle("Monthly Sales Comparison of Top Two Groups") +
  ylab("Sales") +
  scale_fill_manual(values = c("N02BE" = "blue", "N05B" = "red")) +
  theme_minimal()