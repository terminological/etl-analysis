

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
library(riverplot)
library(circlize)

pwd <- readline(prompt="Enter DB: ");

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "10.174.129.118",
                 Database = "RobsDatabase",
                 UID = "RobertSQL",
                 PWD = pwd,
                 Port = 1433,
                 bigint = "integer");
mapping <- dbReadTable(con,"mappingFullUmlsOmopSCT")

mapping <- mapping %>% mutate(
  CUI = as.numeric(str_sub(CUI,2)) # string comparisons too slow
)

# The many to many map from CUI to omop concept (specifically for SNOMECT CT)
# This has 

characterise <- dbReadTable(con,"mappingComparisonUmlsOmopSCT")

cui2omop <- mapping %>% filter(!is.na(CUI)) %>% group_by(CUI) %>% summarise(
  mappings=n_distinct(concept_id)
) %>% bind_rows(
  mapping %>% filter(is.na(CUI)) %>% select(CUI) %>% mutate(mappings=0)
)

ggplot(cui2omop,aes(x=as.factor(mappings)))+geom_bar()+scale_y_log10()

omop2cui <- mapping %>% filter(!is.na(concept_id)) %>% group_by(concept_id) %>% summarise(
  mappings=n_distinct(CUI)
) %>% bind_rows(
  mapping %>% filter(is.na(concept_id)) %>% select(concept_id) %>% mutate(mappings=0)
)

ggplot(omop2cui,aes(x=as.factor(mappings)))+geom_bar()+scale_y_log10()

nodes <- tibble(
  y=seq(0,max(characterise$cui_map,na.rm = TRUE)),
  x=0,
  ID = paste0('C',y)
) %>% bind_rows(
    tibble(
      y=seq(0,max(characterise$omop_map,na.rm = TRUE)),
      x=2,
      ID = paste0('O',y)
    )
) %>% bind_rows(
  tibble(
    y=c(-5),
    x=c(1),
    ID=c('unmapped')
  )
)
  
edges <- characterise %>% mutate(
  N1 = ifelse(is.na(cui_map),'unmapped',paste0('C',cui_map)),
  N2 = ifelse(is.na(omop_map),'unmapped',paste0('O',omop_map)),
  ID = paste0('E_',N1,'_',N2),
  Value = log10(mappings_count)
)

ggplot(edges, aes(
    x=factor(N1,ordered=TRUE,levels=c('unmapped',paste0('C',seq(0:50)))), 
    y=factor(N2,ordered=TRUE,levels=c('unmapped',paste0('O',seq(0:50)))), 
    fill=Value))+
  geom_tile() + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="blue") 

plot(makeRiver(as.data.frame(nodes),edges), default_style=default.style())

# TODO: circos plot. incoming versus outgoing.

chordDiagram(edges %>% select(from=N1, to=N2, value=Value))