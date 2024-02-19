library(readr)
library(arrow)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Read Consumer Price Index values for years 1974-2023 for use in 
# Adjusting home value and household income
historic_cpi <- read_csv("./data/cpi_urban_us.csv") %>% mutate(Year = as.numeric(substr(DATE, 1, 4)))

#' Calculates the adjusted (real) value of an item
#' at the current (or target) time using the 
#' current CPI and a historic CPI
#' @param nominal_value The original value of the time
#' @param current_cpi The CPI at the time you want to adjust
#' the value to (numeric)
#' @param historic_cpi The CPI at the time you want to adjust
#' the value from (numeric)
#' @return The "real" value adjusted to the current CPI
cpi_adj <- function(nominal_value, current_cpi, historic_cpi) {
  nominal_value * (current_cpi/historic_cpi)
}

#' Calculates the "weighted median" of a numeric vector given
#' a set of integer weights
#' @param x A numeric vector of values
#' @param weights A set of integer weights greater than or equal to zero
#' @return The weighted mean, which should be equivalent to calling
#' median(rep(x, weights)). Returns NA if any of the values are NA
weighted_median <- function(x, weights) {
  # Sort weights by x and sort x
  weights <- weights[order(x)]
  x <- sort(x)
  # Stop when count is half of total number
  target <- sum(weights)/2

  n <- length(x)
  s <- 0
  i <- 0

  while (s <= target) {
    i <- i + 1
    s <- s + weights[i]
  }

  # If there is equal weight above and below i, then average x[i-1] and x[i]
  # this is equivalent to a case of even number of items
  # otherwise, just return the ith value
  if (i > 1 & sum(weights[0:(i-1)]) == sum(weights[i:n])) {
    (x[i-1] + x[i])/2
  } else {
    x[i]
  }
}



ahs <- open_dataset("data/ahs")

ahs_year <- ahs %>%
  rename(Year = YEAR) %>%
  filter(INTSTATUS == "'1'") %>% # completed interview
  group_by(Year)

income <- ahs_year %>%
  filter(HINCP > -9999999) %>%
  collect() %>%
  summarize(
    household_income = weighted_median(HINCP, WEIGHT)
  )

housing_costs <- ahs_year %>% 
  filter(TENURE %in% c("'1'", "'2'")) %>% # Owner or Renter
  collect() %>%
  summarize(
    housing_costs = weighted_median(TOTHCAMT, WEIGHT)
  )

house_value <- ahs_year %>% 
  filter(TENURE == "'1'" & MARKETVAL >= 0) %>%
  collect() %>%
  summarise(
    house_value = weighted_median(MARKETVAL, WEIGHT)
  )

ahs_combined <- income %>%
  inner_join(housing_costs, by = "Year") %>% 
  inner_join(house_value, by = "Year") %>%
  mutate(
    value_to_income = house_value/household_income * 100,
    cost_to_income = (housing_costs*12)/household_income * 100
  )
