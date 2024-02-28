library(readr)
library(arrow)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Utility functions for use in analysis
source("./utils.R")

# Read Consumer Price Index values for years 1974-2023 for use in 
# Adjusting home value and household income
historic_cpi <- read_csv("./data/cpi_urban_us.csv") %>%
  mutate(Year = as.numeric(substr(DATE, 1, 4)))

cpi_2023 <- historic_cpi %>%
  filter(Year == 2023) %>%
  select(CPIAUCSL) %>%
  unlist() %>%
  unname()

# Read Historic Mortgage Rates to use for housing cost analysis
mortage_rates <- read_csv("./data/mortgage_rates_30_year.csv") %>%
  mutate(Year = as.numeric(substr(DATE, 1, 4)))


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
    housing_costs = weighted_median(TOTHCAMT, WEIGHT),
    cost_to_income = weighted_median((TOTHCAMT * 12) / HINCP, WEIGHT) * 100
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
  inner_join(historic_cpi %>% select(Year, CPIAUCSL), by = "Year") %>%
  inner_join(mortage_rates %>% select(Year, MORTGAGE30US), by = "Year") %>%
  mutate(
    household_income_adj = cpi_adj(household_income, cpi_2023, CPIAUCSL),
    housing_costs_adj = cpi_adj(housing_costs, cpi_2023, CPIAUCSL),
    house_value_adj = cpi_adj(house_value, cpi_2023, CPIAUCSL),
  ) %>%
  mutate(
    # Expected mortgage payment for a new house with a 20% down payment
    expected_mortgage_payment = pmt(house_value_adj * 0.8, MORTGAGE30US / 1200, 30 * 12)
  ) %>% 
  mutate(
    mortgage_to_income = (expected_mortgage_payment * 12) / household_income_adj * 100
  ) %>%
  select(-c(CPIAUCSL, MORTGAGE30US))


# Create a plot showing house prices and household income
fig1 <- ahs_combined %>% 
  select(Year, household_income, house_value) %>%
  mutate(
    `Median Household Income` = household_income/1000,
    `Median House Value` = house_value/1000,
    .keep = "unused"
  ) %>%
  pivot_longer(
    starts_with("Median"),
    names_to = "Category",
    values_to = "Thousands $"
  ) %>%
  ggplot(aes(x = Year, y = `Thousands $`, color = Category)) +
  geom_line() +
  ylim(0, 400) +
  theme_minimal()
ggsave("./figures/fig1.png", fig1, units = "in", height = 4, width = 8, bg = "#FFFFFF")

fig2 <- ahs_combined %>%
  ggplot(aes(x = Year, y = expected_mortgage_payment)) +
  geom_line() +
  ylab("Expected Mortgage Payment ($)") +
  theme_minimal()

ggsave("./figures/fig2.png", fig2, units = "in", height = 4, width = 6, bg = "#FFFFFF")

costs_to_ownership <- ahs_year %>%
  filter(TENURE %in% c("'1'", "'2'")) %>%
  collect() %>%
  mutate(Ownership = ifelse(TENURE == "'1'", "Owner", "Renter")) %>%
  group_by(Year, Ownership) %>%
  summarise(
    housing_costs = weighted_median(TOTHCAMT, WEIGHT),
    cost_to_income = weighted_median((TOTHCAMT * 12) / HINCP, WEIGHT),
    household_income = weighted_median(HINCP, WEIGHT)
  ) %>% 
  inner_join(historic_cpi, by = "Year") %>%
  mutate(
    housing_costs = cpi_adj(housing_costs, cpi_2023, CPIAUCSL),
    houshold_income = cpi_adj(household_income, cpi_2023, CPIAUCSL)
    )

fig3 <- costs_to_ownership %>%
  ggplot(aes(x = Year, y = housing_costs, color = Ownership)) +
  geom_line() +
  ylab("Housing Costs ($)") +
  theme_minimal()
ggsave("./figures/fig3.png", fig3, units = "in", height = 4, width = 8, bg = "#FFFFFF")

fig4 <- costs_to_ownership %>%
  ggplot(aes(x = Year, y = cost_to_income*100, color = Ownership)) +
  geom_line() +
  ylab("Costs : Income (%)") +
  theme_minimal()
ggsave("./figures/fig4.png", fig4, units = "in", height = 4, width = 8, bg = "#FFFFFF")

fig5 <- costs_to_ownership %>%
  ggplot(aes(x = Year, y = household_income/1000, color = Ownership)) +
  geom_line() +
  ylab("Household Income (Thousands $)") +
  theme_minimal()
ggsave("./figures/fig5.png", fig5, units = "in", height = 4, width = 8, bg = "#FFFFFF")

