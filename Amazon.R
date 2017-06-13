#Load packages
library(dplyr)
library(tidyr)
library(readr)

#Import dataset
amazon_original <- read.csv("Reviews2.csv", stringsAsFactors = F)
amazon_columns_removed <- select(amazon_original, ProductId, UserId, Score, Summary, Text)

#Create new rating_category variable
amazon_categories <- amazon_columns_removed %>% mutate(rating_category = case_when(
  .$Score == 5 ~ "Positive",
  .$Score == 4 ~ "Positive",
  .$Score == 3 ~ "Neutral",
  .$Score == 2 ~ "Negative",
  .$Score == 1 ~ "Negative"))
