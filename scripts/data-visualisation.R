### Load packages

# Load pacman and install the required packages
library(ggplot2)
library(readr)

#####Load in the data###############################
gapminder <- read_csv("data/gapminder_data.csv")
View(gapminder)
head(gapminder, 5)

#########exploratory visualisation ##############

## Relationship between GDP per capital and life expectancy
ggplot(
  data = gapminder,
  mapping = aes(y = lifeExp,
                x = gdpPercap)
  ) +
  geom_point()
