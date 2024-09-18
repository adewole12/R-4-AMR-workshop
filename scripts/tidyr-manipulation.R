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
    values_to = "obs_values") %>% 
  separate(obstype_year, into = c("obstype", "year"),
           sep = '_') %>% 
  mutate(year = as.integer(gap_long2$year))

##########Iterative explanation ############
# Start with the gap_wide dataframe
gap_long2 <- gap_wide %>% 
  # Pivot the dataframe from wide to long format
  pivot_longer(
    cols = colnames(gap_wide)[-c(1:2)],  # Select all columns except the first two
    names_to = "obstype_year",           # Name the new column that will hold the original column names
    values_to = "obs_values"             # Name the new column that will hold the values
  ) %>% 
    # Separate the combined column into two new columns
  separate(
    obstype_year,                        # The column to split
    into = c("obstype", "year"),         # Names of the new columns
    sep = '_'                            # Character to split on
  ) %>% 
    # Convert the year column from character to integer
  mutate(
    year = as.integer(year)              # Convert year to integer
  )


#####################################
b <- gap_long %>% 
  group_by(continent, obstype) %>% 
  summarise(means = mean(obs_values))


a <- gap_long2 %>% 
  group_by(continent, obstype) %>% 
  summarise(means = mean(obs_values))

waldo::compare(a,b)
all.equal(gap_long, gap_long2)

##### Convert long to wider format
gap_normal <- gap_long %>% 
  pivot_wider(names_from = obstype, values_from = obs_values) 

dim(gap_normal)
gap_normal <- gap_normal[,names(gapminder)]

# compare the two formats
waldo::compare(gap_normal, gap_wide)
all.equal(gap_normal, gap_wide) #you can also use this to compare different items

?all.equal

