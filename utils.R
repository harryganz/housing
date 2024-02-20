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

#' Returns the amortized per-period payment amount for a given
#' loan amount and number of pay periods
#' @param amt The original loan amount
#' @param r The per pay-period nominal interest rate
#' @param n The number of pay periods
pmt <- function(amt, r, n) {
    amt * (r * (1 + r)^n) / ((1 + r)^n - 1)
}