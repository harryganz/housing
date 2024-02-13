library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)

source('./helper_methods.R')

housing_combined <- read_csv('./data/housing_combined.csv')

# Convert Year bought to factor and reorder in based on start year
# Convert housing costs to thousands
housing_combined <- housing_combined %>% 
  mutate(`Year Bought` = factor(`Year Bought`, levels = sort(unique(`Year Bought`))))
