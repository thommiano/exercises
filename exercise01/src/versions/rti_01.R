
################################################################################
################################################################################
# Author: Thom Miano
# Project: RTI Exercise 01
# Purpose: Flatten the SQLite database using R
################################################################################
################################################################################

# Set working directory
setwd("~/GitHub/exercises/exercise01/src")

# Loading library for connecting to the sqlite db
library("RSQLite")

# Connecting to the db
db.connection <- dbConnect(drv = "SQLite", dbname = "../data/exercise01.sqlite")

# List all tables
tables <- dbListTables(db.connection)

# Exclude sqlite_sequence (contains table infromation)
tables <- tables[tables != "sqlite_secuence"]

# Building a list that will hold the tables
dataframes.list <- vector("list", length = length(tables))

# Creating a list of dataframes for each table
for (i in seq(along = tables)) {
  dataframes.list[[i]] <- dbGetQuery(conn = db.connection, 
                                 statement = paste("SELECT * FROM '", 
                                                   tables[[i]], "'", sep = ""))
}

# Create seperate dataframes of each table from our list of tables
countries.id <- dataframes.list[[1]]
education.levels.id <- dataframes.list[[2]]
marital.statuses.id <- dataframes.list[[3]]
occupations.id <- dataframes.list[[4]]
races.id <- dataframes.list[[5]]
records <- dataframes.list[[6]] # This is our primary table
relationships.id <- dataframes.list[[7]]
sexes.id <- dataframes.list[[8]]
workclasses.id <- dataframes.list[[9]]

# Flattening country
records[["country_id"]] <- countries.id[ match(records[["country_id"]], 
                                      countries.id[["id"]]), "name"]

# Flattening education levels
records[["education_level_id"]] <- education.levels.id[ match(records[["education_level_id"]], 
                                               education.levels.id[["id"]]), "name"]

# Flattening marital statuses
records[["marital_statuses_id"]] <- marital.statuses.id[ match(records[["marital_status_id"]], 
                                               marital.statuses.id[["id"]]), "name"]

# Flattening occupations
records[["occupation_id"]] <- occupations.id[ match(records[["occupation_id"]], 
                                               occupations.id[["id"]]), "name"]

# Flattening races
records[["race_id"]] <- races.id[ match(records[["race_id"]], 
                                               races.id[["id"]]), "name"]

# Flattening relationships
records[["relationship_id"]] <- relationships.id[ match(records[["relationship_id"]], 
                                               relationships.id[["id"]]), "name"]

# Flattening sexes
records[["sex_id"]] <- sexes.id[ match(records[["sex_id"]], 
                                               sexes.id[["id"]]), "name"]

# Flattening workclasses
records[["workclass_id"]] <- workclasses.id[ match(records[["workclass_id"]], 
                                               workclasses.id[["id"]]), "name"]

write.csv(records, "exercise01_flat.csv", row.names = FALSE)













################################################################################
################################################################################
### References

# stackoverflow on RSQLite:
# http://stackoverflow.com/questions/9802680/importing-files-with-extension-sqlite-into-r

# stackoverflow on replacing ID values
# http://stackoverflow.com/questions/14417612/r-replace-an-id-value-with-a-name
