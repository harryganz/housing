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
