## Load packagaes ########
library(tidyverse)
library(usethis)

gitcreds::gitcreds_set()





## import data ####
gapminder <- read_csv("data/gapminder_data.csv")
View(gapminder)

###