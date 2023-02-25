library("dplyr")
library("stringr")
library("ggplot2")

#Loading data frame into variable spl_data
spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data-csv.csv", stringsAsFactors = FALSE)

#Remove samples where creator is empty
spl_data <- spl_data %>%
  mutate(Creator = trimws(Creator)) %>%
  filter(!is.na(Creator) & Creator !="")

#Grouping data by number of checkouts by the creator
createcheck <- spl_data %>%
  group_by(CheckoutYear, Creator) %>%
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE))

#Top 5 Creators by highest checkouts
top_5 <- createcheck %>%
  group_by(Creator) %>%
  summarize(creator_sum_checkouts = sum(sum_checkouts)) %>%
  top_n(5)

#Filter createcheck data frame by the top 5 Creators
top5_by_year <- createcheck %>%
  filter(Creator %in% top_5$Creator)

#Graph top5_by_year
ggplot(top5_by_year) +
  geom_smooth(mapping = aes (x = CheckoutYear, y = sum_checkouts, color = Creator)) + labs(
    title = "Total Checkouts by the Top 5 Creators By Year",
    x = "Year",
    y= "Total Checkouts",
    color = "Top 5 Creators")
  




