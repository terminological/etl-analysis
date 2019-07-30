# pwd <- readline(prompt="Enter DB: ");

# args <- commandArgs()
# print(args)

library(config)
library(Achilles)

setwd("~/Dropbox/tmp/achilles/")
config <- config::get(file="~/Dropbox/db.yaml")

# pwd <- args[4]

connectionDetails <- createConnectionDetails(
  dbms=config$dbms, 
  server=config$server, 
  user=config$user, 
  password=config$password, 
  port=config$port)

achilles(connectionDetails, 
         cdmDatabaseSchema = "omop.dbo", 
         resultsDatabaseSchema="omopResults.dbo",
         vocabDatabaseSchema = "omop.dbo",
         numThreads = 1,
         sourceName = "My Source Name", 
         cdmVersion = "6.0",
         runHeel = TRUE,
         runCostAnalysis = FALSE,
         # sqlOnly = TRUE,
         outputFolder = "~/Dropbox/tmp/achilles/sql"
         #analysisIds = c(500)
         )

fetchAchillesHeelResults(connectionDetails,resultsDatabaseSchema='omopResults.dbo')

# exportToJson(connectionDetails, 
#              cdmDatabaseSchema = "omop.dbo", 
#              resultsDatabaseSchema = "omopResults.dbo", 
#              outputPath = "~/Dropbox/tmp/achilles/json", 
#              cdmVersion = "6.0",
#              compressIntoOneFile = TRUE # creates gzipped file of all JSON files)