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
                x = gdpPercap)) +
  geom_point(alpha = 0.3, mapping = aes(colour = continent)) +
  scale_x_log10() +
  geom_smooth(method = "lm", colour = "red", linewidth = 0.4) 

############trend of life expectancy over time########################
ggplot(
  data = gapminder,
  mapping = aes(y = lifeExp,
                x = year                )
  ) +
  geom_line(mapping = aes(colour = continent, group = country))+
  geom_point(alpha = 0.2) 



##############################################
ggplot(
  data = gapminder[gapminder$country == "Cambodia", ],
  mapping = aes(y = lifeExp,
                x = year)) +
  geom_line(mapping = aes(colour = continent, group = country)) +
  geom_point()


####################################
ggplot(
  data = gapminder %>% 
    filter(country == "Rwanda"),
  mapping = aes(y = lifeExp,
                x = year                )
) +
  geom_line(mapping = aes(colour = continent, group = country)) +
  geom_point()



