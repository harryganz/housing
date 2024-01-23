library(tidyverse)

source('./helper_methods.R')

# Create a custom theme
theme_set(theme_minimal() + theme(plot.margin = margin(1.25, 1, 1.25, 1, "cm"), text = element_text(size = 8)))

# Import Housing Data
household_income <- read_csv('./data/household_income.csv')
home_price <- read_csv('./data/home_price.csv')
cpi <- read_csv('./data/cpi_urban_us.csv') %>%
  mutate(Year = as.numeric(substr(DATE, 1, 4))) %>% select(-DATE) %>%
  rename(CPI = CPIAUCSL)
mortgage_rates <- read_csv('./data/mortgage_rates_30yr.csv') %>%
  mutate(Year = as.numeric(substr(DATE, 1, 4)), Rate = round(MORTGAGE30US, 3)) %>%
  select(-c(DATE, MORTGAGE30US))

merged_data <- household_income %>% inner_join(home_price, by = "Year") %>%
  inner_join(cpi, by = "Year") %>% 
  inner_join(mortgage_rates, by = "Year")

# Adjust Home Price for CPI using 2022 Dollars (same as for income)
merged_data <- merged_data %>% mutate(`Adj Median Home Price` = cpi_adj(`Median Home Price`, CPI[Year == 2022], CPI))

# Calculate ratio of home price to household income and plot
merged_data %>% 
  mutate(`Median New Home Price / Median Household Income` = `Adj Median Home Price`/`Median Income`) %>% 
  ggplot(aes(x = Year, y = `Median New Home Price / Median Household Income`)) + 
  geom_path()

ggsave("./figures/med_price_to_med_income.png", width = 6, height = 4, units = "in", bg = "#FFFFFF", dpi = 150)

# Calculate the expected monthly payment assuming monthly payments on a 30 year fixed mortgage with a 20% down payment
merged_data <- merged_data %>% mutate(`Monthly Payment` = pmt(`Adj Median Home Price`*0.8, 30*12, Rate/1200))


# Plot Monthly Payment / Income 
merged_data %>% mutate(`Monthly Payment / Monthly Income (%)` = 1200 * `Monthly Payment`/`Median Income`) %>% 
  ggplot(aes(x = Year, y = `Monthly Payment / Monthly Income (%)`)) +
  geom_path()

ggsave("./figures/monthly_payment_to_monthly_income.png", width = 6, height = 4, units = "in", bg="#FFFFFF", dpi =  150)


