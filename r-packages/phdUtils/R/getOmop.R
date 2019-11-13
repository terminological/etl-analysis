# library(config)
# library(DBI)
# library(odbc)
# library(dbplyr)
# library(dbplot)

#' Get a handle to the omop databse
#'
#' This function sets up connect to the omop databse.
#' @param dbname String value of dbname defaults to "omop".
#' @param config Config object from yaml file. (defaults to config::get(file="~/Dropbox/db.yaml"))
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' omop = Omop$new(dbname,config)
Omop = R6::R6Class("Omop", public=list(
  #### Fields ----
  con = NULL,
  searcher = NULL,
  care_site = NULL,
  cdm_source = NULL,
  concept = NULL,
  concept_ancestor = NULL,
  concept_class = NULL,
  concept_relationship = NULL,
  concept_synonym = NULL,
  condition_era = NULL,
  condition_occurrence = NULL,
  cost = NULL,
  device_exposure = NULL,
  domain = NULL,
  dose_era = NULL,
  drug_era = NULL,
  drug_exposure = NULL,
  drug_strength = NULL,
  fact_relationship = NULL,
  location = NULL,
  location_history = NULL,
  measurement = NULL,
  metadata = NULL,
  note = NULL,
  note_nlp = NULL,
  observation = NULL,
  observation_period = NULL,
  payer_plan_period = NULL,
  person = NULL,
  procedure_occurrence = NULL,
  provider = NULL,
  relationship = NULL,
  source_to_concept_map = NULL,
  specimen = NULL,
  survey_conduct = NULL,
  visit_detail = NULL,
  visit_occurrence = NULL,
  vocabulary = NULL,

  #### Methods ----
  initialize = function(dbname = "omop", config = config::get(file="~/Dropbox/db.yaml")) {
    self$con <- odbc::dbConnect(odbc::odbc(),
                   Driver = config$odbcName,
                   Server = config$server,
                   Database = dbname,
                   UID = config$user,
                   PWD = config$password,
                   Port = config$port,
                   bigint = "integer");
    self$searcher <- Searcher$new(self);
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
    self$note <- dplyr::tbl(self$con,"note")
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
  finalize = function() {
    odbc::dbDisconnect(self$con)
  },
  vocabLookup = function(df) {
    # "tbl_sql" %>% in class(df)  => remote; use raw left join.
    for (colname in colnames(df)) {
      if (endsWith(colname,"concept_id")) {
        prefix = str_remove(colname,"concept_id")
        vocabCol = paste0(prefix,"concept_name")

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
    return(df)
  }
))

#' directly lookup concepts in db
#'
#' @param df an input data frame
#' @param omop S6 connection to an omop database
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' omop = Omop$new()
#' df %>% vocabLookup(omop)
#' df %>% omop$vocabLookup()
vocabLookup <- function(df,omop) {
  return(omop$vocabLookup(df))
}
