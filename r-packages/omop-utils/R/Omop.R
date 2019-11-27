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

  #### Methods ----
  #' @description Sets up an omop database connection
  #' @param dbname String value of dbname defaults to "omop".
  #' @param config Config object from yaml file. (defaults to config::get(file="~/Dropbox/db.yaml"))
  #' @examples omop = Omop$new(dbname,config)
  initialize = function(dbname = "omop", config = config::get(file="~/Dropbox/db.yaml")) {
    self$con <- odbc::dbConnect(odbc::odbc(),
                   Driver = config$odbcName,
                   Server = config$server,
                   Database = dbname,
                   UID = config$user,
                   PWD = config$password,
                   Port = config$port,
                   bigint = "integer64");
    self$care_site <- dplyr::tbl(self$con, "care_site")
    self$cdm_source <- dplyr::tbl(self$con, "cdm_source")
    self$concept <- dplyr::tbl(self$con, "concept")
    self$concept_ancestor <- dplyr::tbl(self$con, "concept_ancestor")
    self$concept_class <- dplyr::tbl(self$con, "concept_class")
    self$concept_synonym <- dplyr::tbl(self$con, "concept_synonym")
    self$concept_relationship <- dplyr::tbl(self$con, "concept_relationship")
    self$condition_era <- dplyr::tbl(self$con, "condition_era")
    self$condition_occurrence <- dplyr::tbl(self$con, "condition_occurrence")
    self$cost <- dplyr::tbl(self$con, "cost")
    self$device_exposure <- dplyr::tbl(self$con, "device_exposure")
    self$domain <- dplyr::tbl(self$con, "domain")
    self$dose_era <- dplyr::tbl(self$con, "dose_era")
    self$drug_era <- dplyr::tbl(self$con, "drug_era")
    self$drug_exposure <- dplyr::tbl(self$con, "drug_exposure")
    self$drug_strength <- dplyr::tbl(self$con, "drug_strength")
    self$fact_relationship <- dplyr::tbl(self$con, "fact_relationship")
    self$location <- dplyr::tbl(self$con, "location")
    self$location_history <- dplyr::tbl(self$con, "location_history")
    self$measurement <- dplyr::tbl(self$con, "measurement")
    self$metadata <- dplyr::tbl(self$con, "metadata")
    self$note <- (dplyr::tbl(self$con,"note") %>% select(-note_text)) # varchar(max) fields have to be at end of query. in our case they are null anyway.
    self$note_nlp <- dplyr::tbl(self$con,"note_nlp")
    self$observation <- dplyr::tbl(self$con,"observation")
    self$observation_period <- dplyr::tbl(self$con,"observation_period")
    self$payer_plan_period <- dplyr::tbl(self$con,"payer_plan_period")
    self$person <- dplyr::tbl(self$con, "person")
    self$procedure_occurrence <- dplyr::tbl(self$con,"procedure_occurrence")
    self$provider <- dplyr::tbl(self$con,"provider")
    self$relationship <- dplyr::tbl(self$con,"relationship")
    self$source_to_concept_map <- dplyr::tbl(self$con,"source_to_concept_map")
    self$specimen <- dplyr::tbl(self$con,"specimen")
    self$survey_conduct <- dplyr::tbl(self$con,"survey_conduct")
    self$visit_detail <- dplyr::tbl(self$con,"visit_detail")
    self$visit_occurrence <- dplyr::tbl(self$con,"visit_occurrence")
    self$vocabulary <- dplyr::tbl(self$con,"vocabulary")
  },
  #' @description Closes the omop database connection
  finalize = function() {
    odbc::dbDisconnect(self$con)
  },

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
  
  #' @description resolve all ???_concept_id fields in result set and look them up in the concept table
  #' @details N.B. if the df is a table to lookup is done in the database, otherwise the vocab is copied accross
  #' @param df an input data frame (or dbplyr lazy table)
  #' @keywords omop
  #' @examples
  #' omop = Omop$new()
  #' df %>% omop$getConceptNames()
  getConceptNames = function(df) {
    # "tbl_sql" %>% in class(df)  => remote; use raw left join.
    for (colname in colnames(df)) {
      if (endsWith(colname,"concept_id")) {
        prefix = stringr::str_remove(colname,"concept_id")
        vocabCol = paste0(prefix,"concept_name")
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
        }
      }
    }
    return(df)
  }
))

