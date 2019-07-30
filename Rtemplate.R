library(DBI)
library(odbc)
library(tidyverse)
library(reshape2)
# library(cowplot)
# library(huxtable)
# library(riverplot)
# library(circlize)

library(config)
config <- config::get(file="~/Dropbox/db.yaml")

con <- dbConnect(odbc(),
                 Driver = config$odbcName,
                 Server = config$server,
                 Database = config$databaseName,
                 UID = config$user,
                 PWD = config$password,
                 Port = config$port,
                 bigint = "integer");