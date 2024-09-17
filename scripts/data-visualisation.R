### Load packages

# Load pacman and install the required packages
library(ggplot2)
library(readr)
library(tidyverse)

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


### ADVANCED DATA VISUALISATION
## select data from America
americas <- gapminder %>% 
  filter(continent == "Americas")

#americas <- gapminder[gapminder$continent == "Americas", ]
ggplot(
  data = americas, mapping = aes(x = year, y = lifeExp)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45))


ggplot(
  data = americas, mapping = aes(x = year, y = lifeExp)) +
    geom_line() +
    facet_wrap(~country) +
    theme(axis.text.x = element_text(angle = 45))



gapminder %>% 
  filter(continent == "Americas") %>% 
  ggplot(mapping = aes(x = year, y = lifeExp)) +
  geom_line(aes(colour = country)) +
  facet_wrap(~country) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position="none")
###############################################################

gapminder %>% 
  mutate(startsWith = substr(country,1,1)) %>% 
  filter(startsWith %in% c("A", "Z")) %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent)) +
  geom_line() +
  facet_wrap(vars(country))+ #vars used in place of ~ to allow multiple variables
  theme_minimal()

###############################
# Calculate the average life expectancy in 2002 of 2 randomly selected countries
# for each continent. then arrange the continent names in reverse order.
# Hint: Use the dplyr functions arrange() and sample_n(),
# they have similar syntax to other dplyr functions

gapminder %>% 
  filter(year == 2002) %>% 
  group_by(continent) %>% 
  sample_n(2) %>% 
  summarise(mean_lifeExp = mean(lifeExp)) %>% 
  arrange(desc(mean_lifeExp))


?slice_sample





