setwd("~/Dropbox/ECMM433 data science")

# install.packages("circlize")

library(circlize)
library(dplyr)
data <- read.delim("./identifiers.tsv")

cat <- function(number) {
  # return(number)
  # ifelse(number>6, return("7+"), return(number));
  return(cut(number,breaks=c(-Inf,0.5,1.5,2.5,Inf),labels=c("0","1","2","3+"),include.lowest=TRUE))
}

data <- data %>% mutate(
  nodeNME = paste0("N>","M",cat(mrn_for_nhs),"E",cat(emis_for_nhs)),
  nodeMEN = paste0("M>","E",cat(emis_for_mrn),"N",cat(nhs_for_mrn)),
  nodeENM = paste0("E>","N",cat(nhs_for_emis),"M",cat(mrn_for_emis))
)


# https://jokergoo.github.io/circlize_book/book/advanced-usage-of-chorddiagram.html#customize-sector-labels

graph <- 
  (
    data %>% ungroup() %>% group_by(from=nodeNME,to=nodeMEN) %>% summarise( value=sum(c)) 
#  ) %>% union(
#    data %>% ungroup() %>% group_by(source=nodeNME,target=nodeENM) %>% summarise( weight=sum(c))
  ) %>% union (
    data %>% ungroup() %>% group_by(from=nodeMEN,to=nodeENM) %>% summarise( value=sum(c))
#  ) %>% union (
#    data %>% ungroup() %>% group_by(source=nodeMEN,target=nodeNME) %>% summarise( weight=sum(c))
  ) %>% union (
    data %>% ungroup() %>% group_by(from=nodeENM,to=nodeNME) %>% summarise( value=sum(c))
#  ) %>% union (
#    data %>% ungroup() %>% group_by(source=nodeENM,target=nodeMEN) %>% summarise( weight=sum(c))
  ) %>% arrange(from)
circos.clear()
  
chordDiagram(graph ,directional = 1)


