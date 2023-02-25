library("dplyr")
library("stringr")
library("ggplot2")

spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data-csv.csv", stringsAsFactors = FALSE)

#Total Checkouts by Material Type Per Year

#Remove samples where material type is empty
spl_data <- spl_data %>%
  mutate(MaterialType = trimws(MaterialType)) %>%
  filter(!is.na(MaterialType) & MaterialType !="")

#Grouping data by number of checkouts by the material type
materialcheck <- spl_data %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE))

#Top 5 Material Types by highest checkouts
top_5 <- materialcheck %>%
  group_by(MaterialType) %>%
  summarize(materialtype_sum_checkouts = sum(sum_checkouts)) %>%
  top_n(5)

#Filter materialcheck data frame by the top 5 material types
top5_by_year <- materialcheck %>%
  filter(MaterialType %in% top_5$MaterialType)

#Graph top5_by_year
ggplot(top5_by_year) +
  geom_smooth(mapping = aes (x = CheckoutYear, y = sum_checkouts, color = MaterialType)) + labs(
    title = "Total Checkouts by the Top 5 Material Type By Year",
    x = "Year",
    y= "Total Checkouts",
    color = "Top 5 Material Types")