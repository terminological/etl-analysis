library(DBI)
library(odbc)
library(datasets)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(tidyr)
library(datamodelr)
library(stringr)
library(huxtable)

# Define functions ------------------------------------------------------


createCounter <- function(value) { function() { value <<- value+1; return(value) } }

setupGraphInsertions <- function(graph) {
  if (is.null(graph$id)) {graph$id = createCounter(0)}
  if (is.na(str_locate(graph$dot_code,"\n# EXTRA NODES\n")[1,2])) {
    
    relsPosition <- str_locate(graph$dot_code,"\n\n('[^']+':'[^']+'->'[^']+':'[^']+'\n)+")
    if(is.na(relsPosition[1,1])) {
      beginRels = str_length(graph$dot_code)
      endRels = str_length(graph$dot_code)-1
    } else {
      beginRels=relsPosition[1,1]
      endRels=relsPosition[1,2]
    }
    preamble <- str_sub(graph$dot_code, start=0, end=beginRels-1)
    rels <- str_sub(graph$dot_code, start=beginRels, end=endRels)
    postscript <- str_sub(graph$dot_code, start=endRels+1, end=-1)
    graph$dot_code <- paste0(preamble,"\n# EXTRA NODES\n##########\n",rels,"\n# EXTRA EDGES\n##########\n",postscript)
    
    graph$dot_code <- str_replace_all(graph$dot_code,"([^P][^O][^R][^T]=\"[^\"]+\")>([^<]+)</TD>","\\1 PORT=\"\\2\">\\2</TD>")
  }
  return(graph)
}

addMapping <- function(graph, listSources, label, listTargets) {
  graph <- setupGraphInsertions(graph);
  id = paste0("mapping_",graph$id())
  listSourceTables = str_split(listSources,'\\$',simplify=TRUE)[,1]
  listSourceFields = str_split(listSources,'\\$',simplify=TRUE)[,2]
  listTargetTables = str_split(listTargets,'\\$',simplify=TRUE)[,1]
  listTargetFields = str_split(listTargets,'\\$',simplify=TRUE)[,2]
  edges <- data.frame(
    from=listSourceTables, 
    fromCol=listSourceFields, 
    to=rep(id,length(listSourceTables)), 
    toCol=rep(id,length(listSourceTables)),
    stringsAsFactors = FALSE)
  out_edge <- data.frame(
    from=rep(id,length(listTargetTables)), 
    fromCol=rep(id,length(listTargetTables)), 
    to=listTargetTables, 
    toCol=listTargetFields,
    stringsAsFactors = FALSE)
  edges <- edges %>% bind_rows(out_edge)
  dotEdges <- (edges %>% 
    mutate(dot = sprintf("'%s':'%s'->'%s':'%s' [style=dashed,color=orangered2]\n", from ,fromCol, to, toCol)) %>% 
    summarise(out = paste0(dot,collapse = " ")))$out[1]
  
  # Set up insert points if not already in existence
  
  
  nodeInsert <- str_locate(graph$dot_code,"\n# EXTRA NODES\n")[1,2]
  edgeInsert <- str_locate(graph$dot_code,"\n# EXTRA EDGES\n")[1,2]
  graph$dot_code <- paste0(
    str_sub(graph$dot_code, end=nodeInsert),
    # id='abc'; label='xyz';
    sprintf("'%s' [label = <<TABLE><TR><TD PORT=\"%s\">%s</TD></TR></TABLE>>, shape = 'plaintext']\n",id,id,label),
    str_sub(graph$dot_code, start=nodeInsert+1,end=edgeInsert),
    dotEdges,
    str_sub(graph$dot_code, start=edgeInsert+1)
  )
  # identify position of relationships by regex
  
  return(graph)
}

addNote <- function(graph, listSources, note, id) {
  graph <- setupGraphInsertions(graph);
  id = paste0("note_",graph$id())
  listSourceTables = str_split(listSources,'\\$',simplify=TRUE)[,1]
  listSourceFields = str_split(listSources,'\\$',simplify=TRUE)[,2]
  edges <- data.frame(
    from=listSourceTables, 
    fromCol=listSourceFields, 
    stringsAsFactors = FALSE)
  dotEdges <- (edges %>% 
    mutate(dot = sprintf("'%s':'%s'->'%s' [style=solid,arrowhead=none,color=grey50]\n", from ,fromCol, id)) %>% 
    summarise(out = paste0(dot,collapse = " ")))$out[1]
  
  note = str_replace_all(note,"\\|","<BR/>")
  nodeInsert <- str_locate(graph$dot_code,"\n# EXTRA NODES\n")[1,2]
  edgeInsert <- str_locate(graph$dot_code,"\n# EXTRA EDGES\n")[1,2]
  graph$dot_code <- paste0(
    str_sub(graph$dot_code, end=nodeInsert),
    sprintf("'%s' [label = <<TABLE ALIGN=\"LEFT\" BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" COLOR=\"#D0D0D0\"><TR><TD><FONT COLOR=\"#202020\">%s</FONT></TD></TR></TABLE>>, shape = 'plaintext']\n",id,note),
    str_sub(graph$dot_code, start=nodeInsert+1,end=edgeInsert),
    dotEdges,
    str_sub(graph$dot_code, start=edgeInsert+1)
  )
  
  return(graph)
}

addDirectMapping <- function(graph, source, target) {
  graph <- setupGraphInsertions(graph);
  id = paste0("direct_mapping_",graph$id())
  
  sourceTable = str_split(source,'\\$',simplify=TRUE)[,1]
  sourceField = str_split(source,'\\$',simplify=TRUE)[,2]
  targetTable = str_split(target,'\\$',simplify=TRUE)[,1]
  targetField = str_split(target,'\\$',simplify=TRUE)[,2]
  # Set up insert points if not already in existence
  
  edgeInsert <- str_locate(graph$dot_code,"\n# EXTRA EDGES\n")[1,2]
  graph$dot_code <- paste0(
    str_sub(graph$dot_code, end=edgeInsert),
    sprintf("'%s':'%s'->'%s':'%s' [style=dashed,color=orangered2]\n", sourceTable , sourceField, targetTable, targetField),
    str_sub(graph$dot_code, start=edgeInsert+1)
  )
  
  
  return(graph)
}

addJoin <- function(graph, source, target) {
  graph <- setupGraphInsertions(graph);
  id = paste0("mapping_",graph$id())
  
  sourceTable = str_split(source,'\\$',simplify=TRUE)[,1]
  sourceField = str_split(source,'\\$',simplify=TRUE)[,2]
  targetTable = str_split(target,'\\$',simplify=TRUE)[,1]
  targetField = str_split(target,'\\$',simplify=TRUE)[,2]
  # Set up insert points if not already in existence
  
  edgeInsert <- str_locate(graph$dot_code,"\n# EXTRA EDGES\n")[1,2]
  graph$dot_code <- paste0(
    str_sub(graph$dot_code, end=edgeInsert),
    sprintf("'%s':'%s'->'%s':'%s' [style=solid,arrowhead=none,color=blue]\n", sourceTable , sourceField, targetTable, targetField),
    str_sub(graph$dot_code, start=edgeInsert+1)
  )
  
  
  return(graph)
}

getSchema <- function(database, pwd, alias) {
  con <- dbConnect(odbc(),
                   Driver = "ODBC Driver 13 for SQL Server",
                   Server = "10.174.129.118",
                   Database = database,
                   UID = "RobertSQL",
                   PWD = pwd,
                   Port = 1433);
  sQuery <- dm_re_query("sqlserver")
  dm_schema <- dbGetQuery(con, sQuery)
  dm_schema$table <- paste0(alias,".",dm_schema$table)
  dm_schema$ref <- ifelse(is.na(dm_schema$ref),NA,paste0(alias,".",dm_schema$ref))
  dm_schema <- transform(dm_schema, ref=as.character(ref))
  return(dm_schema)
  dbDisconnect(con)
}

getUnifiedSchema <- function(pwd) {
  
  dm_orderComms <- getSchema("ordercomms_review",pwd,"rv")
  dm_omopBuild <- getSchema("omopBuild",pwd,"ob")
  dm_omop <- getSchema("omop",pwd,"o")
  dm_epro <- getSchema("EproLive-Copy",pwd,"e")
  dm_trinetx <- getSchema("trinetx",pwd,"tx")
  
  dm_unified <- dm_orderComms %>% 
    union(dm_omopBuild) %>%
    union(dm_omop) %>%
    union(dm_epro) %>%
    union(dm_trinetx)
  
  dm_unified <- as.data_model(dm_unified)
  
  # dm_unified <- dm_set_segment(dm_unified, list(
  #   "ordercomms"=(dm_orderComms %>% distinct(table))$table,
  #   "omop"=(dm_omop %>% distinct(table))$table,
  #   "omopBuild"=(dm_omopBuild %>% distinct(table))$table,
  #   "epro"=(dm_epro %>% distinct(table))$table,
  #   "TriNetX"=(dm_trinetx %>% distinct(table))$table
  # ))
  
my_colors <-
  dm_color_scheme(
    green = dm_palette(
      line_color = "#7FC97F",
      header_bgcolor = "#7FC97F",
      header_font = "#000000",
      bgcolor = "#f0f9f0"
    ),
    purple = dm_palette(
      line_color = "#BEAED4",
      header_bgcolor = "#BEAED4",
      header_font = "#000000",
      bgcolor = "#f2f2f7"
    ),
    orange = dm_palette(
      line_color = "#FFC000",
      header_bgcolor = "#FFC000",
      header_font = "#000000",
      bgcolor = "#fffae9"
      ),
    yellow = dm_palette(
      line_color = "#FFFF99",
      header_bgcolor = "#FFFF99",
      header_font = "#000000",
      bgcolor = "#FFFFD0"
      ),
    blue = dm_palette(
      line_color = "#a8c2e3",
      header_bgcolor = "#a8c2e3",
      header_font = "#000000",
      bgcolor = "#eef2f9"
      ),
    magenta = dm_palette(
      line_color = "#fd78be",
      header_bgcolor = "#fd78be",
      header_font = "#000000",
      bgcolor = "#ffeef7"
    )
)

dm_add_colors(my_colors)
  
  dm_unified <- dm_set_display(dm_unified, list(
    green=(dm_omop %>% distinct(table))$table,
    orange=(dm_omopBuild %>% distinct(table))$table,
    purple=(dm_orderComms %>% distinct(table))$table,
    magenta=(dm_epro %>% distinct(table))$table,
    blue=(dm_trinetx %>% distinct(table))$table
  ))
  dm_unified$columns = dm_unified$columns %>% arrange(table,column)
  return(dm_unified)
}

listFields <- function(db_schema, regexpr, prefix="",suffix="") {
  cat(
  ((db_schema$columns %>%
    mutate(fqn = paste0("# ",prefix,'"',table,'$',column,'"',suffix)) %>%
    filter(str_detect(fqn,regexpr)) %>%
    select(fqn))$fqn)
  ,sep="\n")
}

listScopedFields <- function(db_schema, tableList) {
  cat(
    ((db_schema$columns %>%
        mutate(fqn = paste0("# \"",table,'$',column,'",')) %>%
        filter(table %in% tableList) %>%
        select(fqn))$fqn)
    ,sep="\n")
}

# Setup data ------------------------------------------------------


dm_unified <- getUnifiedSchema(pwd)


# ordercomms_revie = rv
# omopBuild = ob
# omop = o
# EproLive-Copy = e
# trinetx = tx

# listFields(dm_unified,"ob\\.MasterIndex",suffix=")")
# listFields(dm_unified,"rv\\.patient\\$[nhs|fir|mid|las|dob|deat|add|pos|pho|mob|las]",prefix="graph <- addDirectMapping(graph,")
# listFields(dm_unified,"rv\\.patient\\$[nhs|fir|mid|las|dob|deat|add|pos|pho|mob|las]",prefix="graph <- addDirectMapping(graph,")
# listFields(dm_unified,"e\\.t_patient_local_identifiers",prefix="graph <- addDirectMapping(graph,")
# listFields(dm_unified,"e\\.t_patients\\$(fore|mid|sur|gen|date|perm|tele)",prefix="graph <- addDirectMapping(graph,",suffix=",")
# listFields(dm_unified,"tx\\.tblDemographicData\\$(pat|zip|date)",prefix="graph <- addDirectMapping(graph,",suffix=",")





#Identifier Graph
dm_unified <- dm_add_references(dm_unified,
    ob.MasterIndex$instanceId==ob.IdentityMap$sourceInstanceId,
    ob.IdentityMap$targetInstanceId==ob.MasterIndex$instanceId
)


# Identification ------------------------------------------------------

focus <- list(tables = c(
  "ob.MasterIndex",
  "ob.IdentityMap",
  "ob.MasterLookup",
  "ob.EproLookup",
  "ob.OrdercommsLookup",
  "ob.TrinetxLookup",
  "ob.StudyPopulation",
  "ob.Blacklist",
  "rv.patient",
  "rv.lab_patient",
  "e.t_patient_local_identifiers",
  "e.t_patients",
  "tx.tblDemographicData"
))

# convert to a data model 
graph <- dm_create_graph(
  dm_unified, 
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
                         graph_attrs = "ranksep=1.5, fontname = 'Arial'") #"nodesep=1, ranksep=1",
                         #graph_attrs = "fontname = 'Arial'")


graph <- addDirectMapping(graph,"rv.patient$family_name","ob.MasterIndex$lastName")
graph <- addDirectMapping(graph,"rv.patient$dob","ob.MasterIndex$dateOfBirth")
graph <- addDirectMapping(graph,"rv.patient$post_code","ob.MasterIndex$postcode")
graph <- addDirectMapping(graph,"rv.patient$death_date","ob.MasterIndex$dateOfDeath")
graph <- addDirectMapping(graph,"rv.patient$phone_number","ob.MasterIndex$telephoneHome")
graph <- addDirectMapping(graph,"rv.patient$first_name","ob.MasterIndex$firstName")
graph <- addDirectMapping(graph,"rv.patient$middle_name","ob.MasterIndex$middleNames")
graph <- addDirectMapping(graph,"rv.patient$last_demographic_update","ob.MasterIndex$updateDate")
graph <- addDirectMapping(graph,"rv.patient$mobile_phone_number","ob.MasterIndex$telephoneMobile")
graph <- addMapping(graph,"rv.patient$sex","MAP GENDER","ob.MasterIndex$gender")
graph <- addMapping(graph,c(
  "rv.patient$address1",
  "rv.patient$address2",
  "rv.patient$address3",
  "rv.patient$address4",
  "rv.patient$address5"),"CONCATENATE","ob.MasterIndex$address")

graph <- addMapping(graph,c(
  "rv.lab_patient$hospital_no",
  "rv.patient$nhsno",
  "e.t_patient_local_identifiers$value",
  "e.t_patients$nhsNumber",
  "tx.tblDemographicData$patient_id"),"ALTERNATE","ob.MasterIndex$rawId")

graph <- addDirectMapping(graph,"e.t_patients$surname","ob.MasterIndex$lastName")
graph <- addDirectMapping(graph,"e.t_patients$dateOfBirth","ob.MasterIndex$dateOfBirth")
graph <- addDirectMapping(graph,"e.t_patients$gender","ob.MasterIndex$gender")
graph <- addDirectMapping(graph,"e.t_patients$permanentAddressPostCode","ob.MasterIndex$postcode")
graph <- addDirectMapping(graph,"e.t_patients$dateOfDeath","ob.MasterIndex$dateOfDeath")
graph <- addDirectMapping(graph,"e.t_patients$forename","ob.MasterIndex$firstName")
graph <- addDirectMapping(graph,"e.t_patients$telephoneHome","ob.MasterIndex$telephoneHome")
graph <- addDirectMapping(graph,"e.t_patients$telephoneMobile","ob.MasterIndex$telephoneMobile")
graph <- addDirectMapping(graph,"e.t_patients$middleNames", "ob.MasterIndex$middleNames")
graph <- addDirectMapping(graph,"e.t_patients$datestamp","ob.MasterIndex$updateDate")
graph <- addDirectMapping(graph,"e.t_patients$permanentAddressPostCode","ob.MasterIndex$postcode")

graph <- addMapping(graph,c(
  "e.t_patients$permanentAddressLine1",
  "e.t_patients$permanentAddressLine2",
  "e.t_patients$permanentAddressLine3",
  "e.t_patients$permanentAddressLine4",
  "e.t_patients$permanentAddressLine5"),"CONCATENATE","ob.MasterIndex$address")

#TriNetX
graph <- addDirectMapping(graph,"tx.tblDemographicData$date_of_birth","ob.MasterIndex$dateOfBirth")
graph <- addDirectMapping(graph,"tx.tblDemographicData$date_of_death","ob.MasterIndex$dateOfDeath")
graph <- addDirectMapping(graph,"tx.tblDemographicData$zip_code","ob.MasterIndex$postcode")

graph <- addDirectMapping(graph,"rv.lab_patient$hospital_no","ob.IdentityMap$sourceInstanceId")
graph <- addDirectMapping(graph,"rv.patient$nhsno","ob.IdentityMap$targetInstanceId")
graph <- addDirectMapping(graph,"e.t_patient_local_identifiers$value","ob.IdentityMap$sourceInstanceId")
graph <- addDirectMapping(graph,"e.t_patients$nhsNumber","ob.IdentityMap$targetInstanceId")
graph <- addDirectMapping(graph,"tx.tblDemographicData$patient_id","ob.IdentityMap$sourceInstanceId")

graph <- addMapping(graph,c(
  "ob.MasterIndex$rawId",
  "ob.Blacklist$rawId"
),"FILTER","ob.MasterLookup$rawId")
graph <- addDirectMapping(graph,"ob.MasterIndex$groupId","ob.MasterLookup$groupId")

graph <- addDirectMapping(graph,"ob.MasterLookup$rawId","ob.EproLookup$pGuid")
graph <- addDirectMapping(graph,"ob.MasterLookup$groupId","ob.EproLookup$groupId")

graph <- addDirectMapping(graph,"ob.MasterLookup$rawId","ob.OrdercommsLookup$patient_id")
graph <- addDirectMapping(graph,"ob.MasterLookup$groupId","ob.OrdercommsLookup$groupId")

graph <- addDirectMapping(graph,"ob.MasterLookup$rawId","ob.TrinetxLookup$Patient id")
graph <- addDirectMapping(graph,"ob.MasterLookup$groupId","ob.TrinetxLookup$groupId")

graph <- addMapping(graph,c("ob.EproLookup$groupId","ob.OrdercommsLookup$groupId","ob.TrinetxLookup$groupId"),"TSFT patients","ob.StudyPopulation$groupId")

dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/01_identifierGraph.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/01_identifierGraph.png")

# Person table ------------------------------------------------------

focus <- list(tables = c(
  "ob.MasterIndex",
  "ob.ConceptMapping",
  "ob.StudyPopulation",
  "o.person"
))

# listFields(dm_unified,"ob\\.MasterIndex\\$[g|date]",prefix="graph <- addDirectMapping(graph",suffix=",")
# listFields(dm_unified,"o\\.person",suffix=")")
# listFields(dm_unified,"ob\\.StudyPopulation")
# listFields(dm_unified,"ob\\.ConceptMapping")

# convert to a data model 
graph <- dm_create_graph(
  dm_unified, 
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'") #"nodesep=1, ranksep=1",
#graph_attrs = "fontname = 'Arial'")

graph <- addJoin(graph,"ob.StudyPopulation$groupId","ob.MasterIndex$groupId")


graph <- addMapping(graph, 
  c("ob.MasterIndex$dateOfBirth","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
  c("o.person$day_of_birth","o.person$month_of_birth","o.person$year_of_birth","o.person$birth_datetime"))
graph <- addMapping(graph, 
  c("ob.MasterIndex$dateOfDeath","ob.StudyPopulation$dateOffset"),"OBFUSCATION","o.person$death_datetime")

graph <- addMapping(graph, 
  c("ob.MasterIndex$gender","ob.ConceptMapping$omopConceptId"),"VOCAB MAPPING",
  c("ob.ConceptMapping$sourceId","o.person$gender_concept_id","o.person$gender_source_concept_id"))

# graph <- addJoin(graph,"ob.MasterIndex$gender","ob.ConceptMapping$sourceId")

graph <- addDirectMapping(graph, "ob.MasterIndex$gender","o.person$gender_source_value")
graph <- addDirectMapping(graph,"ob.MasterIndex$groupId","o.person$person_id")


# note ethnicity
graph <- addNote(graph, c(
  "o.person$ethnicity_concept_id",
  "o.person$race_concept_id", 
  "o.person$ethnicity_source_concept_id",
  "o.person$ethnicity_source_value",
  "o.person$race_concept_id",
  "o.person$race_source_concept_id",
  "o.person$race_source_value",
  "o.person$person_source_value"
  ), "Not mapped")
# note ethnicity
graph <- addNote(graph, c(
  "o.person$location_id",
  "o.person$provider_id",
  "o.person$care_site_id"), "In this phase location,|provider and care site|are not considered")

dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/04_person.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/04_person.png")

# Visit table -------------------------------

focus <- list(tables = c(
  "ob.ConceptMapping",
  "ob.TrinetxLookup",
  "ob.StudyPopulation",
  "o.visit_occurrence",
  "tx.tblTriNetXEncounter"
))

# listScopedFields(dm_unified,focus$tables)

graph <- dm_create_graph(
  dm_unified, 
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'")
# listFields(dm_unified,"tx\\.tblTriNetXEncounter",suffix=",")
# listFields(dm_unified,"o\\.visit_occurrence",suffix=",")
# listFields(dm_unified,"ob\\.TrinetxLookup",suffix=",")
# listFields(dm_unified,"ob\\.StudyPopulation",suffix=",")
# listFields(dm_unified,"ob\\.ConceptMapping\\$(omopConceptId|sourceId)",suffix=",")

graph <- graph %>% 
  addJoin("tx.tblTriNetXEncounter$Patient id","ob.TrinetxLookup$Patient id") %>%
  addJoin("ob.TrinetxLookup$groupId","ob.StudyPopulation$groupId") %>%
  
  addNote(c(
    "o.visit_occurrence$admitted_from_concept_id",
    "o.visit_occurrence$admitted_from_source_value",
    "o.visit_occurrence$care_site_id",
    "o.visit_occurrence$discharge_to_concept_id",
    "o.visit_occurrence$discharge_to_source_value",
    "o.visit_occurrence$preceding_visit_occurrence_id",
    "o.visit_occurrence$provider_id",
    "o.visit_occurrence$visit_source_concept_id",
    "o.visit_occurrence$visit_source_value"
    ), "not mapped or|value not available") %>%
  addNote("o.visit_occurrence$visit_occurrence_id","automatically|generated") %>%
  addNote("o.visit_occurrence$visit_type_concept_id","fixed value:|32035 - Visit derived|from EHR record") %>%
  
  addDirectMapping("ob.StudyPopulation$groupId","o.visit_occurrence$person_id") %>%
  addMapping( 
    c("tx.tblTriNetXEncounter$Encounter End Date","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c("o.visit_occurrence$visit_end_date","o.visit_occurrence$visit_end_datetime")) %>%
  addMapping(
    c("tx.tblTriNetXEncounter$Encounter Start Date","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c("o.visit_occurrence$visit_start_date","o.visit_occurrence$visit_start_datetime")) %>%
  addMapping(
    c("tx.tblTriNetXEncounter$Encounter Type","tx.tblTriNetXEncounter$Location id","ob.ConceptMapping$omopConceptId"),"VOCAB MAPPING",
    c("o.visit_occurrence$visit_concept_id","ob.ConceptMapping$sourceId"))

dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/05_visit.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/05_visit.png")

# Procedure table ---------------------------------------------

focus <- list(tables = c(
  "o.concept",
  "o.concept_relationship",
  "tx.tblTriNetXProcedure",
  "ob.TrinetxLookup",
  "ob.StudyPopulation",
  "o.procedure_occurrence"
))

# listScopedFields(dm_unified,focus$tables)

graph <- dm_create_graph(
  dm_unified,
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'")

graph <- graph %>% 
  addJoin("tx.tblTriNetXProcedure$Patient id","ob.TrinetxLookup$Patient id") %>%
  addJoin("ob.TrinetxLookup$groupId","ob.StudyPopulation$groupId") %>%
  addJoin("o.concept$concept_id","o.concept_relationship$concept_id_1") %>%
  addJoin("o.concept$concept_id","o.concept_relationship$concept_id_2") %>%
  addMapping(c(
      "tx.tblTriNetXProcedure$Procedure Code",
      "o.concept$concept_id"
    ),"VOCAB MAPPING",c(
      "o.procedure_occurrence$procedure_concept_id",
      "o.procedure_occurrence$procedure_source_concept_id",
      "o.concept$concept_code")) %>%
  addMapping(
    c("tx.tblTriNetXProcedure$Procedure Code","o.concept$concept_id"),"VOCAB MAPPING",
    c("o.procedure_occurrence$modifier_concept_id","o.concept$concept_code")) %>%
  addNote(c(
    "o.procedure_occurrence$procedure_concept_id",
    "o.procedure_occurrence$modifier_concept_id",
    "o.procedure_occurrence$procedure_source_value",
    "o.procedure_occurrence$modifier_source_value"
    ),"OPCS4 codes are|divided between core|procedures or modifiers|based on the code") %>%
  addMapping(
    c("tx.tblTriNetXProcedure$Procedure Date","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c("o.procedure_occurrence$procedure_date","o.procedure_occurrence$procedure_datetime")) %>%
  addNote("o.procedure_occurrence$procedure_occurrence_id","Auto generated id") %>%
  addDirectMapping("ob.StudyPopulation$groupId","o.procedure_occurrence$person_id") %>%
  addDirectMapping("tx.tblTriNetXProcedure$Encounter id","o.procedure_occurrence$visit_occurrence_id") %>%
  addDirectMapping("tx.tblTriNetXProcedure$Procedure Code","o.procedure_occurrence$procedure_source_value") %>%
  addDirectMapping("tx.tblTriNetXProcedure$Procedure Code","o.procedure_occurrence$modifier_source_value") %>%
  addNote(c(
    "o.procedure_occurrence$provider_id",
    "o.procedure_occurrence$quantity",
    "o.procedure_occurrence$visit_detail_id"),"Not mapped") %>%
  addNote(
    "o.procedure_occurrence$procedure_type_concept_id","Fixed values|44786630:Primary procedure|44786631:Secondary procedure"
  )
dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/06_procedure.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/06_procedure.png")

# Conditions table ---------------------------------------------

focus <- list(tables = c(
  "o.concept",
  "o.concept_relationship",
  "tx.tblTriNetXDIAGNOSIS",
  "ob.TrinetxLookup",
  "ob.StudyPopulation",
  "o.condition_occurrence",
  "o.observation"
))

# listScopedFields(dm_unified,focus$tables)

graph <- dm_create_graph(
  dm_unified,
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'")

graph <- graph %>% 
  addJoin("tx.tblTriNetXDIAGNOSIS$Patient id","ob.TrinetxLookup$Patient id") %>%
  addJoin("ob.TrinetxLookup$groupId","ob.StudyPopulation$groupId") %>%
  addJoin("o.concept$concept_id","o.concept_relationship$concept_id_1") %>%
  addJoin("o.concept$concept_id","o.concept_relationship$concept_id_2") %>%
  addMapping(c(
    "tx.tblTriNetXDIAGNOSIS$Diagnosis Code",
    "o.concept$concept_id",
    "o.concept$domain_id"
  ),"VOCAB MAPPING AND ROUTING",c(
    "o.condition_occurrence$condition_concept_id",
    "o.condition_occurrence$condition_source_concept_id",
    "o.concept$concept_code")) %>%
  addMapping(c(
    "tx.tblTriNetXDIAGNOSIS$Diagnosis Code",
    "o.concept$concept_id",
    "o.concept$domain_id"
  ),"VOCAB MAPPING AND ROUTING",c(
    "o.observation$observation_source_concept_id",
    "o.observation$observation_concept_id",
    "o.concept$concept_code")) %>%
  addNote(c(
      "o.observation$observation_concept_id",
      "o.observation$observation_source_concept_id",
      "o.observation$observation_source_value",
      "o.condition_occurrence$condition_concept_id",
      "o.condition_occurrence$condition_source_concept_id",
      "o.condition_occurrence$condition_source_value"
  ),"Diagnosis codes are|divided between condition_occurence|or observation tables|based on the code domain") %>%
  addMapping(
    c("tx.tblTriNetXDIAGNOSIS$Diagnosis Date","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c(
      "o.condition_occurrence$condition_start_date",
      "o.condition_occurrence$condition_start_datetime"
    )) %>%
  addMapping(
    c("tx.tblTriNetXDIAGNOSIS$Diagnosis Date","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c(
      "o.observation$observation_date",
      "o.observation$observation_datetime"
    )) %>%
  addNote(
    c("o.condition_occurrence$condition_end_date",
    "o.condition_occurrence$condition_end_datetime"),"No resolution date given|for administrative diagnoses"
  ) %>%
  addNote("o.condition_occurrence$condition_occurrence_id","Auto generated id") %>%
  addNote("o.observation$observation_id","Auto generated id") %>%
  addDirectMapping("ob.StudyPopulation$groupId","o.condition_occurrence$person_id") %>%
  addDirectMapping("ob.StudyPopulation$groupId","o.observation$person_id") %>%
  addDirectMapping("tx.tblTriNetXDIAGNOSIS$Encounter id","o.observation$visit_occurrence_id") %>%
  addDirectMapping("tx.tblTriNetXDIAGNOSIS$Encounter id","o.condition_occurrence$visit_occurrence_id") %>%
  addDirectMapping("tx.tblTriNetXDIAGNOSIS$Diagnosis Code","o.observation$observation_source_value") %>%
  addDirectMapping("tx.tblTriNetXDIAGNOSIS$Diagnosis Code","o.condition_occurrence$condition_source_value") %>%
  addDirectMapping("tx.tblTriNetXDIAGNOSIS$Diagnosis Source","o.condition_occurrence$condition_status_source_value") %>%
  addNote(c(
   "o.condition_occurrence$condition_status_concept_id",
   "o.condition_occurrence$condition_type_concept_id",
   "o.condition_occurrence$provider_id",
   "o.condition_occurrence$stop_reason",
   "o.condition_occurrence$visit_detail_id"
  ),"Not mapped") %>%
  addNote("o.condition_occurrence$condition_type_concept_id","Fixed value|32020:EHR encounter diagnosis") %>%
  addNote("o.condition_occurrence$condition_status_concept_id","Fixed value|4230359:Final diagnosis") %>%
  addNote("o.observation$value_as_concept_id","Fixed value|4253628:Known present") %>%
  addNote("o.observation$observation_type_concept_id","Fixed value|38000280:Observation recorded from EHR") %>%
  addNote(c(
    "o.observation$obs_event_id",
    "o.observation$obs_event_field_concept_id",
    "o.observation$provider_id",
    "o.observation$qualifier_concept_id",
    "o.observation$qualifier_source_value",
    "o.observation$unit_concept_id",
    "o.observation$unit_source_value",
    "o.observation$value_as_datetime",
    "o.observation$value_as_number",
    "o.observation$value_as_string",
    "o.observation$visit_detail_id"
  ),"Not mapped")
dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/07_conditions.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/07_conditions.png")

# Lab results -----------------------



focus <- list(tables = c(
  "o.concept",
#  "o.concept_relationship",
  "o.measurement",
  "ob.IdentifiableNote",
  "rv.rtest",
  "rv.rbattery",
  "rv.testSynonym",
  "rv.batterySynonym",
  "rv.report",
  "rv.discipline",
  "ob.ConceptMapping",
  "ob.OrdercommsLookup",
  "ob.StudyPopulation"
))

graph <- dm_create_graph(
 dm_unified,
 rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
 graph_attrs = "ranksep=1.5, fontname = 'Arial'") #, layout='fdp', overlap='prism', splines='curved', K=1.5")
 
 graph <- graph %>% 
   addJoin("rv.report$patient_id","ob.OrdercommsLookup$patient_id") %>%
   addJoin("ob.OrdercommsLookup$groupId","ob.StudyPopulation$groupId") %>%
   addJoin("o.concept$concept_id","ob.ConceptMapping$omopConceptId") %>%
   
   addDirectMapping("ob.StudyPopulation$groupId","o.measurement$person_id") %>%
   
   
   addMapping(c(
      "rv.testSynonym$from_code",
      "rv.batterySynonym$from_code",
      "ob.ConceptMapping$omopConceptId"
     ),"TEST MAP",c(
      "ob.ConceptMapping$sourceId",
      "o.measurement$measurement_concept_id",
      "o.measurement$measurement_source_concept_id"
   )) %>%
   addMapping(c(
     "rv.testSynonym$unit",
     "ob.ConceptMapping$omopConceptId"
   ),"UNIT MAP",c(
     "o.measurement$unit_concept_id",
     "ob.ConceptMapping$sourceId"
   )) %>%
   addMapping(c(
     "rv.rtest$comparitor",
     "o.concept$concept_id"
   ), "COMPARITOR MAP",c(
     "o.measurement$operator_concept_id",
     "o.concept$concept_name"
   )) %>%
   addMapping(c(
     "rv.rtest$normalcy",
     "rv.rtest$textual_result",
     "ob.ConceptMapping$omopConceptId"
   ),"VALUE MAP",c(
     "ob.ConceptMapping$sourceId",
     "o.measurement$value_as_concept_id"
   )) %>%
   addMapping(
     c("rv.report$result_date","rv.report$result_time","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
      c(
        "o.measurement$measurement_date",
        "o.measurement$measurement_datetime",
        "o.measurement$measurement_time"
   )) %>%
   
   addDirectMapping("rv.rtest$numeric_result","o.measurement$value_as_number") %>%
   addDirectMapping("rv.testSynonym$high_range","o.measurement$range_high") %>%
   addDirectMapping("rv.testSynonym$low_range","o.measurement$range_low") %>%
   
   addDirectMapping("rv.testSynonym$unit","o.measurement$unit_source_value") %>%
   addDirectMapping("rv.rtest$textual_result","o.measurement$value_source_value") %>%
   addMapping(c("rv.testSynonym$original_display_name","rv.batterySynonym$from_code"),"CONCAT","o.measurement$measurement_source_value") %>%
   
   addNote("o.measurement$measurement_id","Auto generated ID") %>%
   addNote("rv.discipline$discipline_id","Excluding:|Andrology (id:13)|Histopathology (id:2)|Radiology (id:25+)") %>%
   
   addNote("o.measurement$measurement_type_concept_id","Fixed value|44818702:Lab result") %>%
   addNote(c( 
     "o.measurement$provider_id",
     "o.measurement$visit_detail_id",
     "o.measurement$visit_occurrence_id"), "Not mapped|data quality issues") %>%
 
   ####
   
  addNote("ob.IdentifiableNote$note_id","Auto generated ID") %>%
  addDirectMapping("ob.StudyPopulation$groupId","ob.IdentifiableNote$person_id") %>%
  addDirectMapping("o.measurement$measurement_id","ob.IdentifiableNote$note_event_id") %>%
  addNote("ob.IdentifiableNote$note_event_field_concept_id","Fixed value|21:measurement|0:Unknown") %>%
   addMapping(
     c("rv.report$result_date","rv.report$result_time","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
     c(
       "ob.IdentifiableNote$note_date",
        "ob.IdentifiableNote$note_datetime"
    )) %>%
     addNote("ob.IdentifiableNote$note_type_concept_id","Fixed value|44814642:Pathology report") %>%
     addNote("ob.IdentifiableNote$note_class_concept_id","Fixed value|37395601:Record artifact") %>%
     addDirectMapping("rv.testSynonym$original_display_name","ob.IdentifiableNote$note_title") %>%
     addDirectMapping("rv.rtest$textual_result","ob.IdentifiableNote$note_text") %>%
     addNote("ob.IdentifiableNote$encoding_concept_id","Fixed value|0:Unknown") %>%
     addNote("ob.IdentifiableNote$language_concept_id","Fixed value|4180182:English language") %>%
     addNote(c( 
       "ob.IdentifiableNote$provider_id",
       "ob.IdentifiableNote$visit_detail_id",
       "ob.IdentifiableNote$visit_occurrence_id"), "Not mapped|data quality issues") %>%
     addDirectMapping("rv.discipline$discipline_name","ob.IdentifiableNote$note_source_value")

   
dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/08a_laboratory.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/08a_laboratory.png")
# Radiology results -----------------------

focus <- list(tables = c(
  "o.measurement",
  "ob.IdentifiableNote",
  "rv.rnpi",
  "rv.npisynonym",
  "rv.report",
  "rv.discipline",
  "ob.ConceptMapping",
  "ob.OrdercommsLookup",
  "ob.StudyPopulation"
))

graph <- dm_create_graph(
  dm_unified,
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'") #, layout='fdp', overlap='prism', splines='curved', K=1.5")

graph <- graph %>% 
  addJoin("rv.report$patient_id","ob.OrdercommsLookup$patient_id") %>%
  addJoin("ob.OrdercommsLookup$groupId","ob.StudyPopulation$groupId") %>%
  
  addDirectMapping("ob.StudyPopulation$groupId","o.measurement$person_id") %>%
  
  
  addMapping(c(
    "rv.npisynonym$from_code",
    "ob.ConceptMapping$omopConceptId"
  ),"NPI MAP",c(
    "ob.ConceptMapping$sourceId",
    "o.measurement$measurement_concept_id",
    "o.measurement$measurement_source_concept_id"
  )) %>%
  
  addMapping(
    c("rv.report$result_date","rv.report$result_time","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c(
      "o.measurement$measurement_date",
      "o.measurement$measurement_datetime",
      "o.measurement$measurement_time"
    )) %>%
  
  addNote(c("o.measurement$value_as_number"
            ,"o.measurement$range_high","o.measurement$range_low"
            ,"o.measurement$unit_source_value"          
            ),"Not applicable") %>%
  
  
  addNote("o.measurement$value_source_value","Report available") %>%
  addMapping(c("rv.npisynonym$original_display_name","rv.npisynonym$from_code"),"CONCAT",c(
    "o.measurement$measurement_source_value","ob.IdentifiableNote$note_title")) %>%
  
  addNote("o.measurement$measurement_id","Auto generated ID") %>%
  addNote("rv.discipline$discipline_id","Radiology results (id:25+)") %>%
  
  addNote("o.measurement$measurement_type_concept_id","Fixed value|5001:Test ordered through EHR") %>%
  addNote("o.measurement$value_as_concept_id","Fixed value|4160030:Performed") %>%
  addNote(c( 
    "o.measurement$provider_id",
    "o.measurement$visit_detail_id",
    "o.measurement$visit_occurrence_id"), "Not mapped|data quality issues") %>%
  
  ####
  
  addNote("ob.IdentifiableNote$note_id","Auto generated ID") %>%
  addDirectMapping("ob.StudyPopulation$groupId","ob.IdentifiableNote$person_id") %>%
  addDirectMapping("o.measurement$measurement_id","ob.IdentifiableNote$note_event_id") %>%
  addNote("ob.IdentifiableNote$note_event_field_concept_id","Fixed value|21:measurement") %>%
  addMapping(
    c("rv.report$result_date","rv.report$result_time","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c(
      "ob.IdentifiableNote$note_date",
      "ob.IdentifiableNote$note_datetime"
    )) %>%
  addNote("ob.IdentifiableNote$note_type_concept_id","Fixed value|44814641:Radiology report") %>%
  addNote("ob.IdentifiableNote$note_class_concept_id","Fixed value|36716202:Radiology studies report") %>%
  
  addDirectMapping("rv.rnpi$text","ob.IdentifiableNote$note_text") %>%
  addNote("ob.IdentifiableNote$encoding_concept_id","Fixed value|0:Unknown") %>%
  addNote("ob.IdentifiableNote$language_concept_id","Fixed value|4180182:English language") %>%
  addNote(c( 
    "ob.IdentifiableNote$provider_id",
    "ob.IdentifiableNote$visit_detail_id",
    "ob.IdentifiableNote$visit_occurrence_id"), "Not mapped|data quality issues") %>%
  addDirectMapping("rv.discipline$discipline_name","ob.IdentifiableNote$note_source_value")


dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/08b_radiology.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/08b_radiology.png")
# Histopathology results -----------------------

focus <- list(tables = c(
  "ob.IdentifiableNote",
  "rv.rtest",
  "rv.testSynonym",
  "rv.report",
  "rv.discipline",
  "ob.OrdercommsLookup",
  "ob.StudyPopulation"
))

graph <- dm_create_graph(
  dm_unified,
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'") #, layout='fdp', overlap='prism', splines='curved', K=1.5")

graph <- graph %>% 
  addJoin("rv.report$patient_id","ob.OrdercommsLookup$patient_id") %>%
  addJoin("ob.OrdercommsLookup$groupId","ob.StudyPopulation$groupId") %>%
  

  addNote("rv.discipline$discipline_id","Histopathology (id:2) only") %>%
  

  ####
  
  addNote("ob.IdentifiableNote$note_id","Auto generated ID") %>%
  addDirectMapping("ob.StudyPopulation$groupId","ob.IdentifiableNote$person_id") %>%
  addNote("ob.IdentifiableNote$note_event_id","No associated event") %>%
  addNote("ob.IdentifiableNote$note_event_field_concept_id","Fixed value|0:Unknown") %>%
  addMapping(
    c("rv.report$result_date","rv.report$result_time","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c(
      "ob.IdentifiableNote$note_date",
      "ob.IdentifiableNote$note_datetime"
    )) %>%
  addNote("ob.IdentifiableNote$note_type_concept_id","Fixed value|44814642:Pathology report") %>%
  addNote("ob.IdentifiableNote$note_class_concept_id","Fixed value|37395601:Record artifact") %>%
  addDirectMapping("rv.testSynonym$original_display_name","ob.IdentifiableNote$note_title") %>%
  addDirectMapping("rv.rtest$textual_result","ob.IdentifiableNote$note_text") %>%
  addNote("ob.IdentifiableNote$encoding_concept_id","Fixed value|0:Unknown") %>%
  addNote("ob.IdentifiableNote$language_concept_id","Fixed value|4180182:English language") %>%
  addNote(c( 
    "ob.IdentifiableNote$provider_id",
    "ob.IdentifiableNote$visit_detail_id",
    "ob.IdentifiableNote$visit_occurrence_id"), "Not mapped|data quality issues") %>%
  addDirectMapping("rv.discipline$discipline_name","ob.IdentifiableNote$note_source_value")


dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/08c_histopathology.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/08c_histopathology.png")


# Ordercomms requests -----------------------

focus <- list(tables = c(
  "ob.IdentifiableNote",
  "rv.rqsample",
  "rv.request",
  "ob.OrdercommsLookup",
  "ob.StudyPopulation"
))

graph <- dm_create_graph(
  dm_unified,
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'") #, layout='fdp', overlap='prism', splines='curved', K=1.5")

graph <- graph %>% 
  addJoin("rv.request$patient_id","ob.OrdercommsLookup$patient_id") %>%
  addJoin("ob.OrdercommsLookup$groupId","ob.StudyPopulation$groupId") %>%
  
  ####
  
  addNote("ob.IdentifiableNote$note_id","Auto generated ID") %>%
  addDirectMapping("ob.StudyPopulation$groupId","ob.IdentifiableNote$person_id") %>%
  addNote("ob.IdentifiableNote$note_event_id","No associated event") %>%
  addNote("ob.IdentifiableNote$note_event_field_concept_id","Fixed value|0:Unknown") %>%
  addMapping(
    c("rv.request$request_date","rv.request$request_time","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c(
      "ob.IdentifiableNote$note_date",
      "ob.IdentifiableNote$note_datetime"
    )) %>%
  addNote("ob.IdentifiableNote$note_type_concept_id","Fixed value|44814645:Note") %>%
  addNote("ob.IdentifiableNote$note_class_concept_id","Fixed value|44803912:Service order request") %>%
  addNote("ob.IdentifiableNote$note_title","Not mapped") %>%
  addDirectMapping("rv.rqsample$src_comments","ob.IdentifiableNote$note_text") %>%
  addNote("ob.IdentifiableNote$encoding_concept_id","Fixed value|0:Unknown") %>%
  addNote("ob.IdentifiableNote$language_concept_id","Fixed value|4180182:English language") %>%
  addNote(c( 
    "ob.IdentifiableNote$provider_id",
    "ob.IdentifiableNote$visit_detail_id",
    "ob.IdentifiableNote$visit_occurrence_id"), "Not mapped|data quality issues") %>%
  addNote("ob.IdentifiableNote$note_source_value","Fixed value|Lab test request|clinical details")


dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/09_ordercomms_request.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/09_ordercomms_request.png")

# Epro drugs ----

focus <- list(tables = c(
  "o.concept",
  "o.concept_relationship",
  # "o.drug_strength",
  "ob.EproLookup",
  "ob.StudyPopulation",
  "e.t_drugs_tto",
  "o.drug_exposure",
  "e.t_order_sentences",
  "o.device_exposure"
))

# listScopedFields(dm_unified,focus$tables)

graph <- dm_create_graph(
  dm_unified,
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'") #, layout='fdp', overlap='prism', splines='curved', K=1.5")

graph <- graph %>%
  addJoin("e.t_drugs_tto$pGuid","ob.EproLookup$pGuid") %>%
  addJoin("ob.EproLookup$groupId","ob.StudyPopulation$groupId") %>%
  addJoin("o.concept$concept_id","o.concept_relationship$concept_id_1") %>%
  addJoin("o.concept$concept_id","o.concept_relationship$concept_id_2") %>%
  addJoin("e.t_drugs_tto$orderSentenceId","e.t_order_sentences$orderSentenceId") %>%
  addDirectMapping("ob.StudyPopulation$groupId","o.drug_exposure$person_id") %>%
  addNote("o.drug_exposure$drug_exposure_id","Auto generated ID") %>%
  
  addMapping(c(
    "e.t_order_sentences$vmpId",
    "o.concept$concept_id"
  ),"VMP MAPPING",c(
    "o.drug_exposure$drug_concept_id",
    "o.drug_exposure$drug_source_concept_id",
    "o.device_exposure$device_concept_id",
    "o.device_exposure$device_source_concept_id",
    "o.concept$concept_code")) %>%
  addMapping(c(
    "e.t_order_sentences$route",
    "o.concept$concept_id"
  ),"ROUTE MAPPING",c(
    "o.drug_exposure$route_concept_id",
    "o.drug_exposure$route_source_value"
  )) %>%
  addMapping(
    c("e.t_drugs_tto$startDate","e.t_drugs_tto$prescribedDate","e.t_drugs_tto$datestamp","ob.StudyPopulation$dateOffset"),"OBFUSCATION / ALTERNATE",
    c("o.drug_exposure$drug_exposure_start_date","o.drug_exposure$drug_exposure_start_datetime",
      "o.device_exposure$device_exposure_start_date","o.device_exposure$device_exposure_start_datetime")) %>%
  addMapping(
    c("e.t_drugs_tto$endDate","ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c("o.drug_exposure$drug_exposure_end_date","o.drug_exposure$drug_exposure_end_datetime",
      "o.device_exposure$device_exposure_end_date","o.device_exposure$device_exposure_end_datetime")) %>%
  addNote("o.drug_exposure$drug_type_concept_id","Fixed value|38000178:Medication list entry") %>%
  addNote(c(
    "o.drug_exposure$verbatim_end_date",
    "o.drug_exposure$stop_reason",
    "o.drug_exposure$refills",
    "o.drug_exposure$quantity",
    "o.drug_exposure$days_supply",
    "o.drug_exposure$sig",
    "o.drug_exposure$lot_number"
  ),"Not available") %>%
  addNote(c("o.drug_exposure$provider_id",
    "o.drug_exposure$visit_detail_id",
    "o.drug_exposure$visit_occurrence_id"
  ),"Not mapped|data quality issues") %>%
  addDirectMapping("e.t_order_sentences$vmpName","o.drug_exposure$drug_source_value") %>%
  addDirectMapping("e.t_order_sentences$routeName","o.drug_exposure$route_source_value") %>%
  addDirectMapping("e.t_order_sentences$doseUnitsText","o.drug_exposure$dose_unit_source_value") %>%

  addDirectMapping("ob.StudyPopulation$groupId","o.device_exposure$person_id") %>%
  addNote("o.device_exposure$device_exposure_id","Auto generated ID") %>%
  addNote("o.device_exposure$device_type_concept_id","Fixed value|44818707:EHR Detail") %>%
  addNote(c(
    "o.device_exposure$unique_device_id",
    "o.device_exposure$quantity"
  ),"Not available") %>%
  addNote(c("o.device_exposure$provider_id",
    "o.device_exposure$visit_detail_id",
    "o.device_exposure$visit_occurrence_id"
  ),"Not mapped|data quality issues") %>%
  addDirectMapping("e.t_order_sentences$vmpName","o.device_exposure$device_source_value") #%>%

dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/11_epro_tto_drugs.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/11_epro_tto_drugs.png")

# Epro documents ----

focus <- list(tables = c(
  "e.tlu_specialties",
  "ob.ConceptMapping",
  "ob.EproLookup",
  "ob.StudyPopulation",
  # "e.t_clinical_records",
  "e.t_documents_current",
  "e.t_document_workflow",
  "e.t_document_html",
  "e.t_letter_status_dtos",
  # "e.t_clinic_appointments",
  # "e.t_pathways",
  "ob.IdentifiableNote"
))

graph <- dm_create_graph(
  dm_unified,
  rankdir = "LR", col_attr = c("column"), focus = focus, columnArrows = TRUE, 
  graph_attrs = "ranksep=1.5, fontname = 'Arial'") #, layout='fdp', overlap='prism', splines='curved', K=1.5")

graph <- graph %>% 
  # addJoin("e.t_documents_current$clinicalRecordId","e.t_clinical_records$id") %>%
  addJoin("e.t_documents_current$id","e.t_letter_status_dtos$id") %>%
  addJoin("e.t_documents_current$versionId","e.t_document_html$versionId") %>%
  addJoin("e.t_documents_current$versionId","e.t_document_workflow$versionId") %>%
  # addJoin("e.t_document_workflow$clinicAppointmentId","e.t_clinic_appointments$id") %>%
  # addJoin("e.t_clinic_appointments$pathwayId","e.t_pathways$id") %>%
  
  
  addJoin("e.tlu_specialties$national_code","ob.ConceptMapping$sourceId") %>%
  addNote("ob.IdentifiableNote$note_id","Auto generated ID") %>%
  addJoin("e.t_documents_current$id","ob.EproLookup$pGuid") %>%
  addJoin("ob.EproLookup$groupId","ob.StudyPopulation$groupId") %>%
  addDirectMapping("ob.StudyPopulation$groupId","ob.IdentifiableNote$person_id") %>%
  addNote("ob.IdentifiableNote$note_event_id","No associated event") %>%
  addNote("ob.IdentifiableNote$note_event_field_concept_id","Fixed value|0:Unknown") %>%
  addMapping(
    c(
      "e.t_document_workflow$clinicDate",
      "e.t_documents_current$creationDate",
      "e.t_document_workflow$creationDate",
      "e.t_letter_status_dtos$writtenDate",
      "e.t_documents_current$datestamp",
      "ob.StudyPopulation$dateOffset"),"OBFUSCATION",
    c(
      "ob.IdentifiableNote$note_date",
      "ob.IdentifiableNote$note_datetime"
    )) %>%
  addNote("ob.IdentifiableNote$note_datetime",
          "Dates chosen for note|relate to clinically|relevant date, as clinic date|document creation date,|letter written date, or|system generated timestamp") %>%
  addNote("ob.IdentifiableNote$note_type_concept_id","Fixed value|44814645:Note") %>%
  addMapping(c(
    "e.t_documents_current$metaSpecId",
    "ob.ConceptMapping$omopConceptId"
  ),"SPECIALTY NOTE CLASS MAPPING",c(
    "ob.ConceptMapping$sourceId",
    "ob.IdentifiableNote$note_class_concept_id",
    "ob.IdentifiableNote$note_source_value"
  )) %>%
  addDirectMapping("e.t_documents_current$subject","ob.IdentifiableNote$note_title") %>%
  addDirectMapping("e.t_document_html$notesActive","ob.IdentifiableNote$note_text") %>%
  addNote("ob.IdentifiableNote$encoding_concept_id","Fixed value|0:Unknown") %>%
  addNote("ob.IdentifiableNote$language_concept_id","Fixed value|4180182:English language") %>%
  addNote(c( 
    "ob.IdentifiableNote$provider_id",
    "ob.IdentifiableNote$visit_detail_id",
    "ob.IdentifiableNote$visit_occurrence_id"), "Not mapped|data quality issues") 

dm_render_graph(graph)
dm_export_graph(graph,"~/Dropbox/dbSchema/12_epro_current_documents.svg")
dm_export_graph(graph,"~/Dropbox/dbSchema/12_epro_current_documents.png")

# Vocab mappings ----

# Gender to gender_concept_id

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "10.174.129.118",
                 Database = "omopBuild",
                 UID = "RobertSQL",
                 PWD = pwd,
                 Port = 1433);
mapping <- dbReadTable(con,"ConceptMapping")
dbDisconnect(con)

filenameFromUrn <- function(urn) {
  return(str_replace_all(urn,"[:\\-]","_"))
}

defaultLayout = function(...) {
  return( theme_article(...) %>% 
    set_width("400pt") %>%
    set_wrap(TRUE) %>%
    set_all_padding(everywhere,everywhere,2) %>%
    set_valign(everywhere,everywhere,"top") )
}

formatTables <- function(mapping,urn,filename=filenameFromUrn(urn)) {
  mapped <- huxtable(
    mapping %>% filter(sourceDomain==urn) %>% 
      arrange(desc(usedCount)) %>%
      top_n(10) %>% 
      filter(row_number() < 11) %>%
      select(
        'Source id'=sourceId,
        'Source term'=sourceTerm,
        'OMOP domain'=omopDomainId, 
        'OMOP concept'=omopConceptId, 
        'OMOP term'=omopConceptName,
        'No. uses'=usedCount),
    add_colnames = TRUE
  )
  mapped <- defaultLayout(mapped, header_col = FALSE)
  quick_html(mapped,file=paste0(filename,"_mapped.html"),open=FALSE)
    
  unmapped <- huxtable(
    mapping %>% filter(sourceDomain==urn, omopConceptId==0) %>% 
      arrange(desc(usedCount)) %>%
      top_n(5) %>% 
      filter(row_number() < 6) %>%
      select(
        'Source id'=sourceId,
        'Source term'=sourceTerm,
        'No. uses'=usedCount),
    add_colnames = TRUE
  )
  unmapped <- defaultLayout(unmapped, header_col = FALSE)
  quick_html(unmapped,file=paste0(filename,"_unmapped.html"),open=FALSE)
  
  stats_summary <- huxtable(
    mapping %>% filter(sourceDomain==urn) %>% 
    mutate(
      status = ifelse(omopConceptId!=0,"mapped","not mapped"),
      total = sum(usedCount)
    ) %>%
    group_by(status) %>%
    summarise(
      number=sum(usedCount),
      percent=sum(usedCount)/max(total)*100
    ),
    add_colnames = TRUE
  )
  stats_summary <- defaultLayout(stats_summary, header_col = TRUE)
  quick_html(stats_summary,file=paste0(filename,"_stats_summary.html"),open=FALSE)

}

setwd("~/Dropbox/dbSchema/")
formatTables(mapping,"urn:ordercomms:code-battery-code")

for (urn in unlist(mapping %>% select(sourceDomain) %>% distinct())) {
  formatTables(mapping,urn)
}
# Encounter type & Location id to visit_concept_id
# Procedure Code (OPCS4) to procedure_concept_id or modifier_concept_id
# Diagnosis Code (ICD10 and SNOMED) to observation_concept_id and condition_concept_id
# Lab test & battery codes to measurement_concept_id
# Unit to unit_concept_id
# Textual result to value_as_concept_id
# Comparitor to operator_concept_id
# NPI radiology test to measurement_concept_id
# VMP id to drug_concept_id
# Route to route_concept_id
# Specialty to note_class_concept_id
