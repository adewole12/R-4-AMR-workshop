## Load packagaes ########
library(tidyverse)
library(usethis)
library(waldo)


## import data ####
gapminder <- read_csv("data/gapminder_data.csv")
View(gapminder)

###Import alternate layout ###
gap_wide <- read.csv("data/gapminder_wide.csv")
view(gap_wide)
str(gap_wide)

###convert from wide to long format
gap_long <- gap_wide %>% 
  pivot_longer(
    cols = c(starts_with('gdpPercap'), starts_with('pop'), starts_with('lifeExp')),
    names_to = "obstype_year", 
    values_to = "obs_values")

gap_long <- gap_long %>% 
  separate(obstype_year, into = c("obstype", "year"),
           sep = '_')

gap_long$year <- as.integer(gap_long$year)

###
#column_names <- colnames(gap_wide)
#column_names[-c(1:2)]


#column_names2 <- colnames(gap_wide)[-c(1:2)]

gap_long2 <- gap_wide %>% 
  pivot_longer(
    cols = colnames(gap_wide)[-c(1:2)],
    names_to = "obstype_year", 
    values_to = "obs_values")
gap_long$year <- as.integer(gap_long$year)


waldo::compare(gap_long2,gap_long)















