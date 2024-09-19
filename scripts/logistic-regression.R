## Load libraries and import data
library(broom)
library(tidyverse)


amrData <- read.csv("data/dig_health_hub_amr.csv")
view(amrData)

glimpse(amrData)

sapply(amrData, class)

## Calculate age in years when speciment taken
amrData <- amrData %>% 
  mutate(age_years_sd = (dob %--% spec_date) %/% years(1)
         )

View(amrData)

amrData <- amrData %>% 
  mutate(
    spec_date_YMD = as.Date(amrData$spec_date),
    dob_YMD = as.Date(amrData$dob)
  )

head(amrData)

############
ageHisto <- amrData %>% 
  ggplot(aes(x = age_years_sd)) +
  geom_histogram(bins = 10, colour = "white") +
  theme_minimal(base_size = 14, base_family = "sans") +
  labs(x = "Age when specimen taken (years)", 
       y = "Frequency")

ageHisto


table(amrData$ethnicity)

xtabs(~region, data = amrData)

antibiotic_crosstab <- prop.table(xtabs(~coamox + cipro + gentam, data = amrData))

View(antibiotic_crosstab)




library(gtsummary)

# to summarise the AMR data into a table
table1 <-  amrData %>% 
  tbl_summary(include = c("sex_male", "age_years_sd","region", "had_surgery_past_yr", "ethnicity", "imd", "organism", "coamox", "cipro", "gentam"),
              missing = c("ifany", "no", "always"),
              statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~
                                 "{n} ({p}%)"))

library(gtsummary)

amrData %>% 
  tbl_summary(include = c("sex_male", "age_years_sd","region", "had_surgery_past_yr", "ethnicity", "imd", "organism", "coamox", "cipro", "gentam"),
              missing = c("ifany", "no", "always"),
              statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"))

table1

? tbl_summary

#### logit model

coamox_logit <- glm(coamox ~ age_years_sd + sex_male,
                    data = amrData,
                    family = "binomial")

summary(coamox_logit)


tidy(coamox_logit)


##Check VIF for n prfect multicolineraity assumption 
car::vif(coamox_logit)

logit.use <- log(coamox_logit$fitted.values / (1 - coamox_logit$fitted.values))

linearity.data <- data.frame(logit.use, age = coamox_logit$model$age_years_sd)

linearPlot <- linearity.data %>% 
  ggplot(aes(age, logit.use)) +
  geom_point(aes(size = "Observation"), colour = "blue", alpha = 0.6) +
  geom_smooth(se = FALSE, aes(colour = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(colour = "linear")) +
  labs(x = "Age in years on sample date",
       y = "Log-odds of co-amox resistance predicted probability")



library(report)

# Get a plain English report of the model
report(coamox_logit)


