## Load packagaes ########
library(tidyverse)
library(gtsummary)
library(kableExtra)


### load imd dims data for london

lon_dims_imd_2019 <- read.csv("data/English_IMD_2019_Domains_rebased_London_by_CDRC.csv")

View(lon_dims_imd_2019)

## Create binary membership variable for City of London 
lon_dims_imd_2019 <- lon_dims_imd_2019 %>% 
  mutate(city = la19nm == "City of London")

head(lon_dims_imd_2019)

## histogram of log values for barriers
lon_dims_imd_2019 %>% 
  ggplot(aes(x = log(barriers_london_rank))) + 
  geom_histogram()


### small standard deviation
dummy_1 <- rnorm(1000, mean = 10, sd = 0.5)
dummy_1 <- as.data.frame(dummy_1)


dummy_1 %>% 
  ggplot(aes(x = dummy_1)) + 
  geom_histogram()


dummy_2 <- rnorm(1000, mean = 10, sd = 200)
dummy_2 <- as.data.frame(dummy_2)


dummy_2 %>% 
  ggplot(aes(x = dummy_2)) + 
  geom_histogram()


# mean for barriers_london_rank
mean_barriers <- mean(lon_dims_imd_2019$barriers_london_rank, na.rm = TRUE)
mean_barriers

sd_barriers <- sd(lon_dims_imd_2019$barriers_london_rank, na.rm = TRUE)
sd_barriers

skimr::skim(lon_dims_imd_2019$barriers_london_rank)

## variance barriers
varbarriers <- var(lon_dims_imd_2019$barriers_london_rank)
varbarriers

sqrt(varbarriers) == sd_barriers

shapiro.test(lon_dims_imd_2019$barriers_london_rank)


#### 
areas_table <- table(lon_dims_imd_2019$la19nm)
areas_table

prop.table(areas_table)

idaop_decile_freq <-  table(
  lon_dims_imd_2019$la19nm,
  lon_dims_imd_2019$IDAOP_london_decile
)

idaop_decile_freq
heatmap(idaop_decile_freq)
?heatmap


lon_dims_imd_2019 %>% 
  group_by(la19nm) %>% 
  summarise(avg = mean(Income_london_rank)) %>% 
  arrange(la19nm, .locale = "en")


## t-test health rank and city of london 
t.test(health_london_rank ~ city, 
       data = lon_dims_imd_2019)


tidy(t.test(health_london_rank ~ city, 
               data = lon_dims_imd_2019))


anovamodel <- aov(lon_dims_imd_2019$health_london_rank ~ lon_dims_imd_2019$la19nm)
summary(anovamodel)

augment(anovamodel)
tidy(anovamodel)


#### correlation 
cor.test(lon_dims_imd_2019$Income_london_rank, lon_dims_imd_2019$health_london_rank)

tidy(cor.test(lon_dims_imd_2019$Income_london_rank, lon_dims_imd_2019$health_london_rank))

plot(lon_dims_imd_2019$Income_london_rank, lon_dims_imd_2019$health_london_rank)

lon_dims_imd_2019 %>% 
  ggplot(aes(x = Income_london_rank, y = health_london_rank)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method='lm', formula= y~x) #use regression line 




lowest_life_exp2002 <- gapminder %>% 
  filter(year == 2002) %>% 
  summarise('lowest life expectancy' = min(lifeExp))

####### changes made to the basic script on macWole branch

