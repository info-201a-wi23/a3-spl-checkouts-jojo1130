#load libraries
library("dplyr")
library("knitr")

#loading the data frame into a variable "spl_df"
spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data-csv.csv", stringsAsFactors = FALSE)

#Q1. What is the average number of checkouts per checkout type?
#Answer: The checkout type with the most number of checkouts was Zinio with 61 average checkouts. 
avg_checks_per_type <- spl_data %>%
  group_by(CheckoutType) %>%
  summarize(
    mean_value = mean(Checkouts, na.rm = TRUE),
  )
#Q2. What is the year with most and least checkouts for the book Meditations?
#Answer: 2022 had the the highest checkouts while 2017 had the least checkouts for Meditations.
meditations <- spl_data %>%
  group_by(Title, CheckoutYear) %>%
  filter(Title == "Meditations") %>%
  summarize(
    sum_checks = sum(Checkouts, na.rm = TRUE)
  )

#Q3. What is the year with the most and least checkouts for EBooks?
#Answer: 2020 had highest checkouts for Ebooks, 2023 had the lowest checkouts for Ebooks
ebooks <- spl_data %>%
  group_by(MaterialType, CheckoutYear) %>%
  filter(MaterialType == "EBOOK") %>%
  summarize(
    sum_checks = sum(Checkouts, na.rm = TRUE)
  )

#Q4. What is the average number of checkouts for each material type, specifically looking at audiobooks?
#The average number of checkouts for audiobooks is 25 checkouts. 
avg_checks_per_material <- spl_data %>%
  group_by(MaterialType) %>%
  summarize(
    mean_value = mean(Checkouts, na.rm = TRUE)
  )

#Q5. What is the year with the most checkouts for Jo Walton
#Answer: 2021
walton <- spl_data %>%
  group_by(Creator, CheckoutYear) %>%
  filter(Creator == "Jo Walton") %>%
  summarize(
    sum_checks = sum(Checkouts, na.rm = TRUE))