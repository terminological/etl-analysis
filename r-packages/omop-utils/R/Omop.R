# library(config)
# library(DBI)
# library(odbc)
# library(dbplyr)
# library(dbplot)

#' Get a handle to the omop databse
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
Omop = R6::R6Class("Omop", public=list(
  #### Fields ----
  #' @field con an connection the omop database
  con = NULL,
  #' @field resultsCon an connection the omop results database
  resultsCon = NULL,
  #' @field resultsCon an connection the omop results database
  persistance = TRUE,

  #' @field care_site omop table
  care_site = NULL,
  #' @field cdm_source omop table
  cdm_source = NULL,
  #' @field concept omop table
  concept = NULL,
  #' @field concept_ancestor omop table
  concept_ancestor = NULL,
  #' @field concept_class omop table
  concept_class = NULL,
  #' @field concept_relationship omop table
  concept_relationship = NULL,
  #' @field concept_synonym omop table
  concept_synonym = NULL,
  #' @field condition_era omop table
  condition_era = NULL,
  #' @field condition_occurrence omop table
  condition_occurrence = NULL,
  #' @field cost omop table
  cost = NULL,
  #' @field device_exposure omop table
  device_exposure = NULL,
  #' @field domain omop table
  domain = NULL,
  #' @field dose_era omop table
  dose_era = NULL,
  #' @field drug_era omop table
  drug_era = NULL,
  #' @field drug_exposure omop table
  drug_exposure = NULL,
  #' @field drug_strength omop table
  drug_strength = NULL,
  #' @field fact_relationship omop table
  fact_relationship = NULL,
  #' @field location omop table
  location = NULL,
  #' @field location_history omop table
  location_history = NULL,
  #' @field measurement omop table
  measurement = NULL,
  #' @field metadata omop table
  metadata = NULL,
  #' @field note omop table
  note = NULL,
  #' @field note_nlp omop table
  note_nlp = NULL,
  #' @field observation omop table
  observation = NULL,
  #' @field observation_period omop table
  observation_period = NULL,
  #' @field payer_plan_period omop table
  payer_plan_period = NULL,
  #' @field person omop table
  person = NULL,
  #' @field procedure_occurrence omop table
  procedure_occurrence = NULL,
  #' @field provider omop table
  provider = NULL,
  #' @field relationship omop table
  relationship = NULL,
  #' @field source_to_concept_map omop table
  source_to_concept_map = NULL,
  #' @field specimen omop table
  specimen = NULL,
  #' @field survey_conduct omop table
  survey_conduct = NULL,
  #' @field visit_detail omop table
  visit_detail = NULL,
  #' @field visit_occurrence omop table
  visit_occurrence = NULL,
  #' @field vocabulary omop table
  vocabulary = NULL,

  #### Constructors ----
  #' @description Sets up an omop database connection
  #' @param con an ODBC connection to the omop database
  #' @param dbname String value of dbname defaults to "omop".
  #' @param resultsdbname String value of dbname defaults to "omopResults".
  #' @param config Config object from yaml file. (defaults to config::get(file="~/Dropbox/db.yaml"))
  #' @examples omop = Omop$new(dbname,config)
  initialize = function(dbname = "omop", resultsdbname = "omopResults", config = config::get(file="~/Dropbox/db.yaml")) {
    
    #TODO: conside switching to jdbc: https://cran.r-project.org/web/packages/RJDBC/RJDBC.pdf
    self$con = odbc::dbConnect(odbc::odbc(),
         Driver = config$odbcName,
         Server = config$server,
         Database = dbname,
         UID = config$user,
         PWD = config$password,
         Port = config$port,
         bigint = "integer64");
    
    self$resultsCon = odbc::dbConnect(odbc::odbc(),
          Driver = config$odbcName,
          Server = config$server,
          Database = resultsdbname,
          UID = config$user,
          PWD = config$password,
          Port = config$port,
          bigint = "integer64");
    
    self$care_site = typedTbl(self$con, "care_site")
    self$cdm_source = (typedTbl(self$con, "cdm_source") %>% select(-source_description, everything())) # varchar(max) fields have to be at end of query. 
    self$concept = typedTbl(self$con, "concept")
    self$concept_ancestor = typedTbl(self$con, "concept_ancestor")
    self$concept_class = typedTbl(self$con, "concept_class")
    self$concept_synonym = typedTbl(self$con, "concept_synonym")
    self$concept_relationship = typedTbl(self$con, "concept_relationship")
    self$condition_era = typedTbl(self$con, "condition_era")
    self$condition_occurrence = typedTbl(self$con, "condition_occurrence")
    self$cost = typedTbl(self$con, "cost")
    self$device_exposure = typedTbl(self$con, "device_exposure")
    self$domain = typedTbl(self$con, "domain")
    self$dose_era = typedTbl(self$con, "dose_era")
    self$drug_era = typedTbl(self$con, "drug_era")
    self$drug_exposure = (typedTbl(self$con, "drug_exposure") %>% select(-sig)) # varchar(max) fields have to be at end of query. in our case they are null anyway.
    self$drug_strength = typedTbl(self$con, "drug_strength")
    self$fact_relationship = typedTbl(self$con, "fact_relationship")
    self$location = typedTbl(self$con, "location")
    self$location_history = typedTbl(self$con, "location_history")
    self$measurement = typedTbl(self$con, "measurement")
    self$metadata = (typedTbl(self$con, "metadata") %>% select(-value_as_string, everything())) # varchar(max) fields have to be at end of query. 
    self$note = (typedTbl(self$con,"note") %>% select(-note_text)) # varchar(max) fields have to be at end of query. in our case they are null anyway.
    self$note_nlp = typedTbl(self$con,"note_nlp")
    self$observation = typedTbl(self$con,"observation")
    self$observation_period = typedTbl(self$con,"observation_period")
    self$payer_plan_period = typedTbl(self$con,"payer_plan_period")
    self$person = typedTbl(self$con, "person")
    self$procedure_occurrence = typedTbl(self$con,"procedure_occurrence")
    self$provider = typedTbl(self$con,"provider")
    self$relationship = typedTbl(self$con,"relationship")
    self$source_to_concept_map = typedTbl(self$con,"source_to_concept_map")
    self$specimen = typedTbl(self$con,"specimen")
    self$survey_conduct = typedTbl(self$con,"survey_conduct")
    self$visit_detail = typedTbl(self$con,"visit_detail")
    self$visit_occurrence = typedTbl(self$con,"visit_occurrence")
    self$vocabulary = typedTbl(self$con,"vocabulary")
  },
  
  #' @description Closes the omop database connection
  finalize = function() {
    odbc::dbDisconnect(self$con)
  },


  #' @description force recaluclation of cached results
  forceCalculation = function() {
    self$persistance = FALSE
  },
  
  #' @description persist a dbplyr result in the omopResults database
  useCached = function() {
    self$persistance = TRUE
  },
  
  #' @description persist a dbplyr result in the omopResults database.
  #' Do not combine with compute(). MS SQL specific code.
  #' @param ... passed to dbplyr::copy_to - of particular interest is unique_indexes = list("x","y"), indexes = list("x","y"), analyze = TRUE
  persist = function(tableName, remoteDf, ...) {
    if (!self$persistance || !db_has_table(self$resultsCon, tableName)) {
      message(paste0("caching result for: ",tableName))
      # create a temp table with the remoteDf result in omop:
      try(db_drop_table(self$con, paste0("##",tableName)),TRUE) # drop the temporary table if possible
      tmpTable = remoteDf %>% compute(name=tableName, overwrite=TRUE, temporary=TRUE)
      # access it from omopResults
      cached = dplyr::tbl(self$resultsCon, paste0("##",tableName))
      # copy to a permanent table in omopResults
      copy_to(self$resultsCon, cached, name=tableName, overwrite=TRUE, temporary=FALSE, ...)
      return(tmpTable)
    } else {
      message(paste0("using cached: ",tableName))
      # load as tmp table in omopResults
      try(db_drop_table(self$resultsCon, paste0("##",tableName)),TRUE) # drop the temporary table if possible
      dplyr::tbl(self$resultsCon, tableName) %>% compute(name = tableName, overwrite=TRUE)
      # access from omop databse
      cached = dplyr::tbl(self$con, paste0("##",tableName)) %>% self$inferType()
      return(cached)
    }
  },
  
  #### Factory methods ----
  
  #' @description create a new vocabulary specification
  #' @return a searcher
  buildVocabSet = function() {
    return(Searcher$new(self))
  },
  
  #' @description create a new cohort
  #' @param name a very short descriptive name for the cohort
  #' @return a cohort
  buildCohort = function(name) {
    return(Cohort$new(self,name))
  },
  
  #### Vocabulary specific functions ----
  
  #' @description resolve all ???_concept_id fields in result set and look them up in the concept table
  #' @details N.B. if the df is a table to lookup is done in the database, otherwise the vocab is copied accross
  #' @param df an input data frame (or dbplyr lazy table)
  #' @keywords omop
  #' @examples
  #' omop = Omop$new()
  #' df %>% omop$getConceptNames()
  getConceptNames = function(df) {
    # "tbl_sql" %>% in class(df)  => remote; use raw left join.
    for (colname in stringr::str_subset(colnames(df),"^.*(concept_id).*$")) {
      #if (endsWith(colname,"concept_id")) {
      vocabCol = stringr::str_replace(colname, stringr::coll("concept_id"), "concept_name")
        #prefix = stringr::str_remove(colname,"concept_id")
        #vocabCol = paste0(prefix,"concept_name")
        # check result column is not already there
        if (!(vocabCol %in% colnames(df))) {
          # relabel the concept table
          tmpView = self$concept %>%
            select((!!colname) := concept_id, (!!vocabCol) := concept_name)

          if ("tbl_sql" %in% class(df)) {
            # perform the join on the whole relabelled concept table in the database
            df = df %>% left_join(tmpView, by=colname)

          } else {
            # "data.frame" %>% in class(df) => local; use con -> copy_to("tempConceptIds") and join to create lookup
            # TODO: for larger tables... if (df %>% count() < 100) {
            # for this magic syntax see stackoverflow 26003574
            lookup = tmpView %>% filter(UQ(rlang::sym(colname)) %in% local(unique(df[[colname]]))) %>% collect()
            df = df %>% left_join(lookup, by=colname)
          }
        #}
      }
    }
    return(df)
  },
  
  #' @description get ancestors for a grouped set of concepts preserving grouping and count information
  #' @param groupedDf a grouped data frame
  #' @param conceptIdVar the concept variable
  #' @return the analyser with expanded groupedDf including extra concepts
  expandAncestorConcepts = function(groupedDf, conceptIdVar, countVar = NULL) {
	  conceptIdVar = ensym(conceptIdVar)
	  grps = groupedDf %>% groups()
	  # first get the concepts by group in groupedDf - i.e. concepts in each document and sum counts
	  # this gives us the freqency of a concept_id in a document
	
  	if (identical(countVar,NULL)) {
  		tmp = groupedDf %>% group_by(!!!grps, !!conceptIdVar) %>% summarise(count = n())
  	} else {
  		countVar = ensym(countVar)
  		tmp = groupedDf %>% group_by(!!!grps, !!conceptIdVar) %>% summarise(count = sum(!!countVar))
  	}
  	
	  
	  tmp = tmp %>% rename(descendant_concept_id=!!conceptIdVar)
	  # expand to the ancestor table.
	  tmp2 = self$concept_ancestor %>%
			  inner_join(tmp, by="descendant_concept_id", copy=TRUE) %>%
	      mutate(original = ifelse(descendant_concept_id == ancestor_concept_id,1L,0L)) %>%
			  rename(concept_id=ancestor_concept_id) %>%
			  # reapply the grouping as the columns have changed name
			  # this will include the original concepts (as they are 1:1 mapped in the ancestor table)
			  # as well as probably multiple copies of all the ancestors to root
			  ungroup() %>% group_by(!!!grps,concept_id,original)
	  tmp2 = tmp2 %>%
			  # when more than one concept within a document matches the ancestor we sum the count
			  summarise(
			      count = sum(count, na.rm = TRUE)
			  ) %>% compute()
	  #the searcher here gives us a standard filtered list of concepts
	  return(
			  Searcher$new(self)$toDataframe() %>% select(concept_id,concept_name) %>% inner_join(tmp2, by="concept_id") %>% 
			  compute() %>% select(!!!grps,!!conceptIdVar:=concept_id, count, original) %>% group_by(!!!grps)
	)
	  
  },
  
  #' @description for a set of concepts get all the hierarchical relationships between all concept pairs
  #' @param df a data frame containing concept ids
  #' @param conceptIdVar the concept variable
  #' @param min the minimum distance to consider
  #' @param max the maximum distance to consider
  #' @return a set of edges with 
  getSpanningGraphEdges = function(df, conceptIdVar, min=0L,max=1000L) {
    conceptIdVar = ensym(conceptIdVar)
    children = df %>% ungroup() %>% select(!!conceptIdVar) %>% distinct() %>% rename(descendant_concept_id = !!conceptIdVar) %>% compute()
    parents = children %>% rename(ancestor_concept_id = descendant_concept_id)
    edges = self$concept_ancestor %>% 
      filter(descendant_concept_id != ancestor_concept_id) %>%
      inner_join(children, by="descendant_concept_id", copy=TRUE) %>% 
      inner_join(parents, by="ancestor_concept_id", copy=TRUE) %>%
      filter(min_levels_of_separation >= local(min) & min_levels_of_separation <= local(max))
    return(edges)
  },
  
  #### Specific data accessors ----
  
  getVisits = function(visit_type = omopCodes$visit_type %>% anyOf(), ...) {
    tmp = self$visit_occurrence %>% 
      filter(visit_concept_id %in% local(c(visit_type))) %>% 
      mutate(los_days = datediff(sql("day"),visit_start_date,visit_end_date)) %>%
      filter(...)
    class(tmp) = c(class(tmp),"omop_visit_occurrence")
    return(tmp)
  },
  
  getDemographics = function(gender = omopCodes$gender %>% anyOf(), ...) {
    tmp = self$person %>% filter(gender_concept_id %in% local(c(gender))) %>% filter(...)
    class(tmp) = c(class(tmp),"omop_person")
    return(tmp)
  },
  
  #### code set accessors
  getObserations = function(conceptSet = NULL, ...) {
    if (any(class(conceptSet)=="Searcher")) conceptSet = conceptSet$toDataframe()
    tmp = self$observation %>% filter(...) 
    if (!identical(conceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(observation_concept_id=concept_id) , by="observation_concept_id" )
    class(tmp) = c(class(tmp),"omop_observation")
    return(tmp)
  },
  
  getProcedures = function(conceptSet = NULL, modifierConceptSet = NULL, ...) {
    if (any(class(conceptSet)=="Searcher")) conceptSet = conceptSet$toDataframe()
    if (any(class(modifierConceptSet)=="Searcher")) modifierConceptSet = modifierConceptSet$toDataframe()
    tmp = self$procedure_occurrence %>% filter(...) 
    if (!identical(conceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(procedure_concept_id=concept_id) , by="procedure_concept_id" )
    if (!identical(modifierConceptSet,NULL)) tmp = tmp %>% inner_join(modifierConceptSet %>% rename(modifier_concept_id=concept_id) , by="modifier_concept_id" )
    class(tmp) = c(class(tmp),"omop_procedure_occurrence")
    return(tmp)
  },
  
  getMeasurements = function(conceptSet = NULL, valueConceptSet = NULL, ...) {
    if (any(class(conceptSet)=="Searcher")) conceptSet = conceptSet$toDataframe()
    if (any(class(valueConceptSet)=="Searcher")) valueConceptSet = valueConceptSet$toDataframe()
    tmp = self$measurement %>% filter(...)
    if (!identical(conceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(measurement_concept_id=concept_id) , by="measurement_concept_id" )
    if (!identical(valueConceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(value_as_concept_id=concept_id) , by="value_as_concept_id" )
    class(tmp) = c(class(tmp),"omop_measurement")
    return(tmp)
  },
  
  getConditions = function(conceptSet = NULL, ...) {
    if (any(class(conceptSet)=="Searcher")) conceptSet = conceptSet$toDataframe()
    tmp = self$condition_occurrence %>% filter(...)
    if (!identical(conceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(condition_concept_id=concept_id) , by="condition_concept_id" )
    class(tmp) = c(class(tmp),"omop_condition_occurrence")
    return(tmp)
  },
  
  getNotes = function(note_type = NULL, note_class = NULL, ...) {
    tmp = self$note %>% filter(...)
    if (!identical(note_type, NULL)) tmp = tmp %>% filter(note_type_concept_id %in% local(c(note_type)))
    if (!identical(note_class, NULL)) tmp = tmp %>% filter(note_class_concept_id %in% local(c(note_class)))
    class(tmp) = c(class(tmp),"omop_note")
    return(tmp)
  },
  
  getNoteNlp = function(conceptSet = NULL, note_type = NULL, note_class = NULL, note_nlp_temporal = NULL, ...) {
    if (any(class(conceptSet)=="Searcher")) conceptSet = conceptSet$toDataframe()
    tmp = self$note_nlp
    if (!identical(conceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(note_nlp_concept_id=concept_id) , by="note_nlp_concept_id" )
    if (!identical(note_nlp_temporal, NULL)) tmp = tmp %>% filter(note_nlp_temporal %in% local(c(note_nlp_temporal)))
    # if (!identical(note_class, NULL)) tmp = tmp %>% filter(note_nlp_section %in% local(c(note_class))) # note_nlp_section and note_class are the same - this is specific to our omop.
    return(self$getNotes(note_type, note_class) %>% select(note_datetime, note_id, person_id) %>% inner_join(tmp, by="note_id") %>% filter(...))
    class(tmp) = c(class(tmp),"omop_note_nlp")
  },
  
  getDrugs = function(conceptSet = NULL, drug_route = NULL, ...) {
    if (any(class(conceptSet)=="Searcher")) conceptSet = conceptSet$toDataframe()
    tmp = self$drug_exposure %>% filter(...)
    if (!identical(conceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(drug_concept_id=concept_id) , by="drug_concept_id" )
    if (!identical(drug_route, NULL)) tmp = tmp %>% filter(route_concept_id %in% local(c(drug_route)))
    class(tmp) = c(class(tmp),"omop_drug_exposure")
    return(tmp)
  },
  
  getDevices = function(conceptSet = NULL, ...) {
    if (any(class(conceptSet)=="Searcher")) conceptSet = conceptSet$toDataframe()
    tmp = self$device_exposure %>% filter(...)
    if (!identical(conceptSet,NULL)) tmp = tmp %>% inner_join(conceptSet %>% rename(device_concept_id=concept_id) , by="device_concept_id" )
    class(tmp) = c(class(tmp),"omop_device_exposure")
    return(tmp)
  },
  
  #### vocab set factory ----
  getDeviceConcepts = function() {Searcher$fromDataframe(self, self$device_exposure, device_concept_id)$persist("vocabDeviceConcepts")},
  getDrugConcepts = function() {Searcher$fromDataframe(self, self$drug_exposure, drug_concept_id)$persist("vocabDrugConcepts")},
  getNoteNlpConcepts = function() {Searcher$fromDataframe(self, self$note_nlp, note_nlp_concept_id)$persist("vocabNoteNlpConcepts")},
  getConditionConcepts = function() {Searcher$fromDataframe(self, self$condition_occurrence, condition_concept_id)$persist("vocabConditionConcepts")},
  getMeasurementConcepts = function() {Searcher$fromDataframe(self, self$measurement, measurement_concept_id)$persist("vocabMeasurementConcepts")},
  getMeasurementValueConcepts = function() {Searcher$fromDataframe(self, self$measurement, value_as_concept_id)$persist("vocabMeasurementValueConcepts")},
  getProcedureConcepts = function() {Searcher$fromDataframe(self, self$procedure_occurrence, procedure_concept_id)$persist("vocabProcedureConcepts")},
  getProcedureModifierConcepts = function() {Searcher$fromDataframe(self, self$procedure_occurrence, modifier_concept_id)$persist("vocabProcedureModifierConcepts")},
  getObservationConcepts = function() {Searcher$fromDataframe(self, self$observation, observation_concept_id)$persist("vocabObservationConcepts")},
  
  ### Class provenance ----
  
  inferType = function(df) {
    for(xName in ls(self)) {
      x = get(xName,envir=self)
      if (any(class(x)=="tbl")) {
        if(all(colnames(x) %in% colnames(df))) 
          class(df) = unique(c(class(df),paste0("omop_",xName)))
      }
    }
    return(df)
  },
  
  #### Standardise output ----
  
  #' Normalise an omop dataframe to a consistent format to construct a single feature set accross
  #'
  #' with the following columns:
  #'
  #' * cohort_person_id,
  #' * cohort_entry_datetime,
  #' * feature_source, e.g. measurement, observation, etc...
  #' * feature_name,
  #' * feature_concept_id,
  #' * feature_value_as_number,
  #' * feature_value_as_date,
  #' * feature_value_as_name,
  #' * feature_value_as_concept_id,
  #' * feature_days_offset,
  #' * feature_display
  #'
  #' or the equivalent with observation_ prefix
  #'
  #' @param omopDf - a df which may be a dbplyr table
  #' @param outcome - is the
  #' @return a dbplyr dataframe
  normaliseColumns = function(omopDf, prefix = "feature") {
    
    selection = c("person_id","cohort_person_id","cohort_entry_datetime", "prefix_source", "prefix_datetime", "prefix_concept_id", "prefix_value_as_number", 
                  "prefix_value_as_number_min", "prefix_value_as_number_max", "prefix_value_as_number_unit", "prefix_value_as_date", "prefix_value_as_text",
                  "prefix_value_as_concept_id", "prefix_days_offset", "prefix_display", "prefix_facet_1", "prefix_facet_2", "prefix_facet_3")
    omopDf = omopDf %>% defaultNormalisedColumns()
    omopDf = omopDf %>% self$inferType()
    out = NULL
    if ("omop_condition_occurrence" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_condition_occurrence",
        prefix_datetime = condition_start_datetime,
        prefix_concept_id = condition_concept_id,
        prefix_facet_1 = condition_status_source_value
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if ("omop_device_exposure" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_device_exposure",
        prefix_datetime = device_exposure_start_datetime,
        prefix_concept_id = device_concept_id,
        prefix_display = device_source_value
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if ("omop_drug_exposure" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_drug_exposure",
        prefix_datetime = drug_exposure_start_datetime,
        prefix_concept_id = drug_concept_id,
        #TODO: dose in here?
        prefix_value_as_concept_id = route_concept_id,
        prefix_display = drug_source_value
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if ("omop_measurement" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_measurement",
        prefix_datetime = measurement_datetime,
        prefix_concept_id = measurement_concept_id,
        prefix_value_as_number = value_as_number,
        prefix_value_as_number_min = range_low,
        prefix_value_as_number_max = range_high,
        prefix_value_as_number_unit = unit_source_value,
        prefix_value_as_concept_id = value_as_concept_id,
        prefix_days_offset = days_offset,
        prefix_display = paste0(measurement_source_value,": ",value_source_value," ",unit_source_value)
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if ("omop_note" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_note",
        prefix_datetime = note_datetime,
        prefix_concept_id = note_class_concept_id,
        prefix_value_as_concept_id = note_type_concept_id,
        prefix_display = note_title
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if ("omop_note_nlp" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_note_nlp",
        prefix_datetime = note_datetime, # this is always here because you can;t get a note_nlp without a note
        prefix_concept_id = note_nlp_concept_id,
        prefix_value_as_text = term_exists, # this is an indicator of negation
        prefix_value_as_omop_id = note_id,
        prefix_facet_1 = as.character(lexical_variant),
        prefix_facet_2 = as.character(term_temporal),
        prefix_facet_3 = term_modifiers #creates a varchar(max) whcih may breaks odbc library
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if ("omop_observation" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_observation",
        prefix_datetime = observation_datetime,
        prefix_concept_id = observation_concept_id,
        prefix_value_as_number = value_as_number,
        prefix_value_as_number_unit = unit_source_value,
        prefix_value_as_date = value_as_datetime,
        prefix_value_as_text = value_as_string,
        prefix_value_as_concept_id = value_as_concept_id,
        prefix_facet_1 = as.character(qualifier_concept_id)
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if ("omop_procedure_occurrence" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_procedure_occurrence",
        prefix_datetime = procedure_datetime,
        prefix_concept_id = procedure_concept_id,
        prefix_value_as_concept_id = modifier_concept_id
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    if("omop_visit_occurrence" %in% class(omopDf)) {
      tmp = omopDf %>% mutate(
        prefix_source = "omop_visit_occurrence",
        prefix_datetime = visit_start_datetime,
        prefix_concept_id = visit_concept_id,
        prefix_value_as_number = los_days,
        prefix_value_as_number_unit = "days",
        prefix_value_as_date = visit_end_datetime
      ) %>% select(selection) %>% standardiseOutputColumns()
      if (identical(out,NULL)) {out = tmp} else {out = out %>% union_all(tmp)}
    }
    #Label concepts
    
    out = out %>% left_join(self$concept %>% select(prefix_value_as_concept_id=concept_id,concept_name), by="prefix_value_as_concept_id") %>%
      mutate(prefix_value_as_text = ifelse(is.na(prefix_value_as_text),concept_name,prefix_value_as_text)) %>% select(-concept_name)
    
    out = out %>% left_join(self$concept %>% select(prefix_concept_id=concept_id,concept_name), by="prefix_concept_id") %>%
      mutate(prefix_display = ifelse(is.na(prefix_display),
                                     ifelse(is.na(prefix_value_as_text),concept_name,paste0(concept_name,": ",prefix_value_as_text)),
                                     prefix_display)) %>% select(-concept_name)
    
    #Rename for feature set or outcome set
    ren = selection %>% stringr::str_replace("prefix",prefix)
    names(selection) = ren
    selection = selection[ren!=selection]
    if(!is.null(selection)) out = out %>% rename(selection)
    return(out)
    #x = tibble(y=c(1,2,3),w=c(3,2,1))
    #x %>% select(y,z=any_of(c("a","c")))#,"w","d","y")))
  }
  
  
))

