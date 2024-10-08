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

### Use this to alter field name
dbExecute(cdrcDB,
          "ALTER TABLE imd RENAME COLUMN LANAME to laname;")

## add other queries to 
dbGetQuery(cdrcDB,
           "SELECT DISTINCT laname
           FROM imd
           LIMIT 10")

dbGetQuery(cdrcDB,
           "SELECT laname, Rank
           FROM imd
           LIMIT 10")

dbGetQuery(cdrcDB,
           "SELECT *
           FROM imd
           LIMIT 10")

dbGetQuery(cdrcDB,
           "SELECT *
           FROM imd
           ORDER BY Rank ASC")

dbGetQuery(cdrcDB,
           "SELECT *
           FROM imd
           ORDER BY Rank, laname ASC")


#####
# Write a query that returns lsoa11, ah3h_rank, ah3g_rnk and ah3e_rnk
# from the ahah table, ordered by the ah3h_rank from the highest to the lowest value

dbGetQuery(cdrcDB,
           "
           SELECT *
           FROM ahah
           LIMIT 10
           ")

dbGetQuery(cdrcDB,
           "
           SELECT lsoa11, ah3h_rnk, ah3g_rnk, ah3e_rnk
           FROM ahah
           ORDER BY ah3h_rnk DESC
           LIMIT 10
           ")


### FILTERING SQL database
dbGetQuery(cdrcDB,
           "
           SELECT LSOA, laname, SOA_decile
           FROM imd
           WHERE laname = 'City of London'
           ")

dbGetQuery(cdrcDB,
           "
           SELECT LSOA, laname, SOA_decile
           FROM imd
           WHERE ((SOA_decile < 5) AND laname != 'City of London')
           ")

dbGetQuery(cdrcDB,
           "
           SELECT DISTINCT laname
           FROM imd
           WHERE laname LIKE '% and %'
           ")

# The %and% will return values different from % and % 
# because of the wild card matching


### Challenge 2
# Write a query that returns lsoa11, ah3h_rank, ah3g_rnk and ah3e_rnk
# from the ahah table, where ah3h_rnk s below 500 or ah3e_rnk is between 3500 and 4500.
# Limit responses to 15

dbGetQuery(cdrcDB,
           "
           SELECT lsoa11, ah3h_rnk, ah3g_rnk, ah3e_rnk
           FROM ahah
           WHERE (ah3h_rnk < 500) OR (ah3e_rnk BETWEEN 3500 AND 4500)
           LIMIT 15
           ")

lsoa11_subset <- dbGetQuery(cdrcDB,
                             "SELECT lsoa11, ah3h_rnk, ah3g_rnk, ah3e_rnk
                             FROM ahah
                             WHERE (ah3h_rnk < 500) OR (ah3e_rnk BETWEEN 3500 AND 4500)
                             LIMIT 15")
lsoa11_subset




#################################################
## Joining data

dbListFields(cdrcDB, "imd")
dbListFields(cdrcDB, "ahah")

dbGetQuery(cdrcDB,"
           SELECT lsoa11, LSOA, ah3gp, laname 
           FROM ahah
           JOIN imd
           ON ahah.lsoa11 = imd.LSOA
           LIMIT 10
           ")


imd_table <- tbl(cdrcDB, "imd")
ahah_table <- tbl(cdrcDB, "ahah")

first_10_lanames <- imd_table %>% 
  select(laname) %>% 
  head(10)

first_10_lanames 

## this helps to show the SQL query equivalence of specific R codes
imd_table %>% 
  select(laname) %>% 
  head(10) %>% 
  show_query()

ahah_table %>% 
  left_join(imd_table, by = join_by(lsoa11 == LSOA)) %>% 
  select(lsoa11, ah3gp, laname) 

ahah_table %>% 
  left_join(imd_table, by = join_by(lsoa11 == LSOA)) %>% 
  select(lsoa11, ah3gp, laname) %>% 
  show_query()



