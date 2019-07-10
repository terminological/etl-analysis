pwd <- readline(prompt="Enter DB: ");

library(Achilles)

connectionDetails <- createConnectionDetails(
  dbms="sql server", 
  server="10.174.129.118", 
  user="RobertSQL", 
  password=pwd, 
  port="1433")

achilles(connectionDetails, 
         cdmDatabaseSchema = "omop", 
         resultsDatabaseSchema="omopResults",
         vocabDatabaseSchema = "omop",
         numThreads = 1,
         sourceName = "My Source Name", 
         cdmVersion = "6.0",
         runHeel = TRUE,
         runCostAnalysis = FALSE)