## Load packagaes ########
library(tidyverse)
library(usethis)
library(waldo)


####import data
gapminder <- read.csv("data/gapminder_data.csv")

lifeExp_plot <- gapminder %>% 
  ggplot(aes(year,lifeExp)) +
  geom_point(alpha = 0.3, size = 0.2)+
  facet_wrap(vars(continent))

lifeExp_plot

ggsave("results/life-expectancy-plot.pdf", lifeExp_plot, dpi = 300)

lifeExp_plot
