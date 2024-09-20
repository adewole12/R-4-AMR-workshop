# load libraries, DBI first as needed by SQLite
library(DBI)
library(RSQLite)
library(tidyverse)


## Import data
ahah_2022 <- read_csv("data/AHAH_V3_0.csv")
imd_2019 <- read_csv("data/uk_imd2019.csv")


# connect to SQLite
# creating a local SQLite Db on our system for a simulated case study
cdrcDB <- dbConnect(RSQLite::SQLite(), "crdcDB.sqLite")

# create table
dbWriteTable(cdrcDB, "ahah", ahah_2022, overwrite = TRUE) #ignore the overwrite = TRUE if working with work database!!!

dbWriteTable(cdrcDB, "imd", imd_2019, overwrite = TRUE)

dbListTables(cdrcDB)

# view fields
dbListFields(cdrcDB, "imd")
dbListFields(cdrcDB, "ahah")

#########
##Query data
dbGetQuery(cdrcDB,
           "SELECT LANAME
           FROM imd
           LIMIT 10"
           )
