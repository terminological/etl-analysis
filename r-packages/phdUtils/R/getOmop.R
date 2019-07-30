library(config)
library(DBI)
library(odbc)
library(dbplyr)
library(dbplot)

#' Get a handle to the omop databse
#'
#' This function sets up connect to the omop databse.
#' @param dbname String value of dbname defaults to "omop".
#' @param config Config object from yaml file.
#' @keywords omop
#' @import config
#' @import DBI
#' @import odbc
#' @import dplyr
#' @import dbplyr
#' @import dbplot
#' @export
#' @examples
#' getOmop()
getOmop = function(dbname = "omop", config = config::get(file="~/Dropbox/db.yaml")) {
  con <- odbc::dbConnect(odbc(),
                 Driver = config$odbcName,
                 Server = config$server,
                 Database = dbname,
                 UID = config$user,
                 PWD = config$password,
                 Port = config$port,
                 bigint = "integer");
  omop <- list("con"=con)
  omop$care_site <- dplyr::tbl(con, "care_site")
  omop$cdm_source <- dplyr::tbl(con, "cdm_source")
  omop$concept <- dplyr::tbl(con, "concept")
  omop$concept_ancestor <- dplyr::tbl(con, "concept_ancestor")
  omop$concept_class <- dplyr::tbl(con, "concept_class")
  omop$concept_relationship <- dplyr::tbl(con, "concept_relationship")
  omop$condition_era <- dplyr::tbl(con, "condition_era")
  omop$condition_occurrence <- dplyr::tbl(con, "condition_occurrence")
  omop$cost <- dplyr::tbl(con, "cost")
  omop$device_exposure <- dplyr::tbl(con, "device_exposure")
  omop$domain <- dplyr::tbl(con, "domain")
  omop$dose_era <- dplyr::tbl(con, "dose_era")
  omop$drug_era <- dplyr::tbl(con, "drug_era")
  omop$drug_exposure <- dplyr::tbl(con, "drug_exposure")
  omop$drug_strength <- dplyr::tbl(con, "drug_strength")
  omop$fact_relationship <- dplyr::tbl(con, "fact_relationship")
  omop$location <- dplyr::tbl(con, "location")
  omop$location_history <- dplyr::tbl(con, "location_history")
  omop$measurement <- dplyr::tbl(con, "measurement")
  omop$metadata <- dplyr::tbl(con, "metadata")
  omop$note <- dplyr::tbl(con,"note")
  omop$note_nlp <- dplyr::tbl(con,"note_nlp")
  omop$observation <- dplyr::tbl(con,"observation")
  omop$observation_period <- dplyr::tbl(con,"observation_period")
  omop$payer_plan_period <- dplyr::tbl(con,"payer_plan_period")
  omop$person <- dplyr::tbl(con, "person")
  omop$procedure_occurrence <- dplyr::tbl(con,"procedure_occurrence")
  omop$provider <- dplyr::tbl(con,"provider")
  omop$relationship <- dplyr::tbl(con,"relationship")
  omop$source_to_concept_map <- dplyr::tbl(con,"source_to_concept_map")
  omop$specimen <- dplyr::tbl(con,"specimen")
  omop$survey_conduct <- dplyr::tbl(con,"survey_conduct")
  omop$visit_detail <- dplyr::tbl(con,"visit_detail")
  omop$visit_occurrence <- dplyr::tbl(con,"visit_occurrence")
  omop$vocabulary <- dplyr::tbl(con,"vocabulary")
  return(omop)
}
