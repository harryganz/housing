library(readr)
library(arrow)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Read Consumer Price Index values for years 1974-2023 for use in 
# Adjusting home value and household income
historic_cpi <- read_csv("./data/cpi_urban_us.csv") %>% mutate(YEAR = as.numeric(substr(DATE, 1, 4)))

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

# Adjust prices to 2023 value
cpi_value <- function(value, year, current_year = 2023) {
  current_cpi = historic_cpi$CPIAUCSL[historic_cpi$YEAR == current_year]
  cpi_adj(value, current_cpi, historic_cpi$CPIAUCSL[historic_cpi$YEAR == year])
}



ahs <- open_dataset("data/ahs")

ahs_summary <- ahs %>%
    rename(Year = YEAR) %>%
    filter(INTSTATUS == "'1'" & TENURE == "'1'" & HINCP > 0) %>% # Interview complete and owner-occupied
    group_by(Year) %>%
    collect() %>%
    summarize(
      `Median House Value`= weighted_median(MARKETVAL, WEIGHT)/1000,
      `Median Household Income` = weighted_median(HINCP, WEIGHT)/1000,
      `House Value : Household Income` = weighted_median(MARKETVAL/HINCP, WEIGHT)
    )

# Plots
 p_income_value_year <- ahs_summary %>%
  pivot_longer(!c(Year, `House Value : Household Income`), names_to = "Category", values_to = "Thousands USD") %>% 
  ggplot(aes(x = Year, y = `Thousands USD`, color = `Category`)) +
  geom_line() + 
  ggtitle("House Value and Household Income (CPI Adjusted to 2023 Dollars)") + 
  scale_y_continuous(limits = c(0, max(ahs_summary$`Median House Value`))) +
  theme_minimal()

p_ratio_income_house_year <- ahs_summary %>% 
  ggplot(aes(x = Year, y = `House Value : Household Income`)) +
  geom_line() +
  ggtitle("Ratio of Median House Value to Household Income") +
  theme_minimal()
