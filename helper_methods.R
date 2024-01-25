#' Calculates the per-period payment for a loan
#' @param pv Present value of the loan (numeric)
#' @param n Number of pay periods for the loan (numeric)
#' @param r The per-period interest rate (numeric)
pmt <- function(pv, n, r) {
  pv * ((r * (1 + r)^n)/((1 + r)^n - 1))
}

#' Calculates the present value given an amortized payment
#' @param pmt The per-period payment amount (numeric)
#' @param n The number of pay periods for a loan (numeric)
#' @param r The per-period interest rate (numeric)
pv <- function(pmt, n, r) {
  pmt * ((1 + r)^n - 1)/(r * (1 + r)^n)
}

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
