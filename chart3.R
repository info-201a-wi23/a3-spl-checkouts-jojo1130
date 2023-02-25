library("dplyr")
library("stringr")
library("ggplot2")

#Loading data frame into variable spl_data
spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data-csv.csv", stringsAsFactors = FALSE)

#Grouping data by number of checkouts by CheckoutType
groupcheck <- spl_data %>%
  group_by(CheckoutYear, CheckoutType) %>%
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE))
  

#Creating bar chart
ggplot(data = groupcheck, aes (x = CheckoutYear, y = sum_checkouts, fill= CheckoutType)) + geom_bar(stat = "identity") + labs(
  title = "Total Checkouts by the Checkout Types By Year",
  x = "Year",
  y= "Total Checkouts",
  color = "Checkout Type")
