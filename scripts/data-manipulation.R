###Load packagaes 

library(readr)
library(tidyverse)

####Load data
gapminder <- read_csv("data/gapminder_data.csv")
View(gapminder)

####data manipulation
africa_mean_gdp <- mean(gapminder$gdpPercap[gapminder$continent == "Africa"])
africa_mean_gdp

america_mean_gdp <- mean(gapminder$gdpPercap[gapminder$continent == "America"])
america_mean_gdp

##########
year_country_gdp <- gapminder %>% 
  select(year, country, gdpPercap)


smaller_gapminder_data <- gapminder %>% 
  select(-continent)

tidy_gdp <- year_country_gdp %>% 
  rename(gdp_per_capita = gdpPercap)

