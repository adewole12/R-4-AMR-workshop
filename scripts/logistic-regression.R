## Load libraries and import data
library(broom)
library(tidyverse)
library(car)
library(lmtest)
library(aod)


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




####################################################################
#####

coamox_surg_logit <- glm(coamox ~ age_years_sd + sex_male + had_surgery_past_yr,
                    data = amrData,
                    family = "binomial")

tidy(coamox_surg_logit)

car::vif(coamox_surg_logit)


#### Including reporting for all regions
coamox_region_logit <- glm(coamox ~ age_years_sd + sex_male + region,
                         data = amrData,
                         family = "binomial")


tidy(coamox_region_logit)

summary(coamox_region_logit)


glance(coamox_region_logit)

# report(coamox_region_logit)

waldtest(b = coeff(coamox_region_logit),
          Sigma = vcov(coamox_region_logit), Terms = 3:11)


??wald.test


###################################################
tbl_regression(coamox_region_logit, exponentiate = TRUE)

?tbl_regression


?gtsummary

tidy_wald_test(coamox_region_logit)

# #########################################################
# Update the model you created in Challenge 1 to include 
# either ethnicity or imd as an independent variable
# What is the log odds reported and is it statistically significant 

coamox_ethn_logit <- glm(coamox ~ age_years_sd + sex_male + ethnicity + had_surgery_past_yr,
                         data = amrData,
                         family = "binomial")

tidy(coamox_ethn_logit)

car::vif(coamox_ethn_logit)

tidy_wald_test(coamox_ethn_logit)

tbl_regression(coamox_ethn_logit, exponentiate = TRUE)


### CI for region model 
confint(coamox_region_logit)


# Descriptive statistics alongside univariate regression, with no spanning header
region_model_summary <-
  amrData[c("age_years_sd", "sex_male", "region")] %>%
  tbl_summary(missing = "no") %>%
  add_n() %>%
  modify_header(stat_0 ~ "**Summary Statistics**")


region_model <- tbl_regression(coamox_region_logit, exponentiate = TRUE)

tbl_merge(tbls = list(region_model_summary, region_model)) %>%
  modify_spanning_header(everything() ~ NA_character_)



?tbl_merge

###########

amrData$imd <- as.factor(amrData$imd)


coamox_imd_logit <- glm(coamox ~ age_years_sd + sex_male + imd + had_surgery_past_yr,
                         data = amrData,
                         family = "binomial")

tbl_regression(coamox_imd_logit, exponentiate = TRUE)

tidy_wald_test(coamox_imd_logit)


# #
# You can also use odds.n.ends to check the quality of your model 

library(odds.n.ends)

coamox_region_logitOR <- odds.n.ends(coamox_region_logit)
coamox_region_logitOR

?odds.n.ends


##################################################################################


colnames(amrData)










