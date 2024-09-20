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

##### Create data subsets
year_country_gdp <- gapminder %>% 
  select(year, country, gdpPercap)

smaller_gapminder_data <- gapminder %>% 
  select(-continent)

tidy_gdp <- year_country_gdp %>% 
  rename(gdp_per_capita = gdpPercap)

### filter items
# annual gdp in Europe
year_country_gdp_europe <- gapminder %>% 
  filter(continent == "Europe") %>% 
  select(-continent)

europe_lifeExp_2007 <- gapminder %>% 
  filter(continent == "Europe", year == 2007) %>% 
  select(-c(continent, gdpPercap))

gapminder %>% 
  filter(continent == "Europe")%>% 
  group_by(country) %>% 
  summarise(mean_gdp = mean(gdpPercap)) 

### Mean Life Expectancy across African countries
gapminder %>% 
  filter(continent == "Africa") %>% 
  select(-c(continent, pop)) %>% 
  group_by(country) %>% 
  summarise(mean_life_exp = mean(lifeExp),
          lowest_life_exp = min(lifeExp),
          highest_life_exp = max(lifeExp),
          life_exp_range = range(lifeExp))

gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_gdpPerCap = mean(gdpPercap))

gapminder %>% 
  group_by(continent) %>% 
 #filter(continent == "America") %>% 
  summarise(mean_gdpPerCap = mean(gdpPercap))



gapminder %>% 
  #filter(continent == "Africa") %>% 
  select(-c(continent, pop)) %>% 
  group_by(country) %>% 
  summarise(mean_life_exp = mean(lifeExp),
            lowest_life_exp = min(lifeExp),
            highest_life_exp = max(lifeExp)) %>% 
  arrange(mean_life_exp)


lifeExpCountry <- gapminder %>%
  group_by(country) %>% 
  summarise(mean_life_exp = mean(lifeExp))%>% 
  arrange(desc(mean_life_exp))

?arrange

## Filter by country with the maximum and minimum life expectancy

lifeExpCountry %>% 
  filter(mean_life_exp == min(mean_life_exp) | mean_life_exp == max(mean_life_exp))


gapminder %>%
  group_by(country) %>% 
  summarise(mean_life_exp = mean(lifeExp))%>% 
  filter(mean_life_exp == min(mean_life_exp)| mean_life_exp == max(mean_life_exp))


## Gdp by year and country 
gdp_by_continents_by_year <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(mean_GdpPerCap = mean(gdpPercap),
            std_GdpPerCap = sd(gdpPercap),
            se_GdpPerCap = sd(gdpPercap)/sqrt(n()),
            minGdpPerCap = min(gdpPercap),
            maxGdpPerCap = max(gdpPercap),
            Gdp_range = (maxGdpPerCap - minGdpPerCap))

gdp_by_continents_by_year

###########################################
gapminder %>% 
  filter(year == 2002) %>% 
  count(continent, sort = TRUE)

gapminder %>% 
  group_by(continent) %>% 
  summarise(se_le = sd(lifeExp)/sqrt(n()))



###Mutate function

gdp_by_pop_continents_by_year <- gapminder %>% 
  mutate(gdp_billion = gdpPercap*pop/10^9) %>% 
  group_by(continent, year) %>% 
  summarise(mean_GdpPerCap = mean(gdpPercap),
            std_GdpPerCap = sd(gdpPercap),
            se_GdpPerCap = sd(gdpPercap)/sqrt(n()),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion),
            minGdpPerCap = min(gdpPercap),
            maxGdpPerCap = max(gdpPercap),
            Gdp_range = (maxGdpPerCap - minGdpPerCap))

gdp_by_pop_continents_by_year


####Using ifelse to add additional conditions

gapminder %>% 
  mutate(gdp_billion = ifelse(lifeExp > 55, gdpPercap*pop/10^9, NA)) %>% 
  group_by(continent, year) %>% 
  summarise(mean_GdpPerCap = mean(gdpPercap),
            std_GdpPerCap = sd(gdpPercap),
            se_GdpPerCap = sd(gdpPercap)/sqrt(n()),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))







