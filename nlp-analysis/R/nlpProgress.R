
library(DBI)
library(odbc)
library(datasets)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(tidyr)
library(stringr)
library(huxtable)
library(lubridate)
pwd <- readline(prompt="Enter DB: ");

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "10.174.129.118",
                 Database = "RobsDatabase",
                 UID = "RobertSQL",
                 PWD = pwd,
                 Port = 1433,
                 bigint = "integer");

data <- dbGetQuery(con, "SELECT * FROM omopBuild.dbo.NlpAudit WHERE event_type='COMPLETE'")

dataByHour <- data %>% mutate(event_hour = as.POSIXct(round(event_time, units="hours"))) %>%
  group_by(nlp_system_instance,event_hour) %>%
  summarise(docs_per_hour = n())

ggplot(dataByHour)+geom_line(aes(x=event_hour,y=docs_per_hour,colour=nlp_system_instance))

dataByMinute <- data %>% filter(event_time > now()-hours(1)) %>% mutate(event_min = floor_date(event_time, unit="minute")) %>%
  group_by(nlp_system_instance,event_min) %>%
  summarise(docs_per_min = n())

ggplot(dataByMinute)+geom_line(aes(x=event_min,y=docs_per_min,colour=nlp_system_instance))