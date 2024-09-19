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



##############
gap_australia <- gapminder[gapminder$country =="Australia", ]


gap_australia2 <- gapminder %>% 
  filter(country == "Australia")

write.csv(gap_australia, "results/gapminder-australia.csv",
          row.names = FALSE)#use this to remove row names 

write.csv(gap_australia2, "results/gapminder-australia2.csv",
          row.names = FALSE)#use this to remove row names 

waldo::compare(gap_australia, gap_australia2)
