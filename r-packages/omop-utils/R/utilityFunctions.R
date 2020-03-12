#### Miscellaneous utilities ----

#' convert a single row dataframe of values to a vector of values for use with %in%
#' 
#' @param singleRowDf - a single rowed df of values
#' @return vector of values
#' @export
anyOf = function(singleRowDf, ...) {
  if (length(rlang::list2(...)) > 0) {
    local(singleRowDf %>% head(1) %>% select(...) %>% collect() %>% unlist(., use.names=FALSE))
  } else {
    local(singleRowDf %>% head(1)  %>% collect() %>% unlist(., use.names=FALSE))
  }
}

#' check an omop related dataset for a start date
#' 
#' @param df - an omop derived df which may be a dbplyr table
#' @return the symbolic representation of the first ??_start_datetime or ??_datetime column or NULL if there is none
findStartDatetime = function(df) {
  if (any(class(df)=="Searcher")) df = df$toDataframe()
  x = colnames(df)
  out = x[stringr::str_ends(x,"_start_datetime")]
  if (length(out)==0) out = x[stringr::str_ends(x,"_datetime")]
  if (length(out)==0) return(NULL)
  return(as.symbol(out[[1]]))
}

#' Unarrange an ordered data set
#' 
#' @param remote_df - a df which may be a dbplyr table
#' @return a dbplyr dataframe without any ordering
unarrange = function(remote_df) {
  if ("tbl_sql" %in% class(remote_df)) {
    existing_groups = groups(remote_df)
    remote_df = remote_df %>% collapse()
    remote_df = tbl(remote_df$src$con, sql_render(remote_df))
    remote_df = group_by(remote_df, !!!existing_groups)
  }
  return(remote_df)
}




#' c("care_site","cdm_source","concept","concept_ancestor","concept_class","concept_relationship","concept_synonym","condition_era",
#'   "condition_occurrence","cost","device_exposure","domain","dose_era","drug_era","drug_exposure","drug_strength",
#'   "fact_relationship","location","location_history","measurement","metadata","note","note_nlp","observation","observation_period",
#'   "payer_plan_period","persist","persistance","person","procedure_occurrence","provider","relationship","source_to_concept_map",
#'   "specimen","survey_conduct","visit_detail","visit_occurrence","vocabulary")
#' 

# TODO: MSSQL specific code. quite a lot of variation in 
defaultNormalisedColumns = function(omopDf) {
  if(!("days_offset" %in% colnames(omopDf))) {
    omopDf = omopDf %>% mutate(days_offset = 0L)
  }
  omopDf = omopDf %>% mutate(
    prefix_days_offset = days_offset,
    prefix_source = sql("CAST(NULL AS VARCHAR(255))"),
    prefix_datetime =  sql("CAST(NULL AS DATETIME2)"),
    prefix_concept_id = as.integer(NA),
    prefix_value_as_number = as.double(NA),
    prefix_value_as_number_min = as.double(NA),
    prefix_value_as_number_max = as.double(NA),
    prefix_value_as_number_unit = sql("CAST(NULL AS VARCHAR(255))"),
    prefix_value_as_date = sql("CAST(NULL AS DATETIME2)"), # asPOSIXct(NULL) produces sql : CAST(NULL as TIMESTAMP), which causes an exception in nanodbc on line 3186
    prefix_value_as_text = sql("CAST(NULL AS VARCHAR(255))"),
    prefix_value_as_concept_id = as.integer(NA),
    #TODO: could have a omop_table field and allow X-links.
    prefix_value_as_omop_id = as.integer64(NA),
    prefix_display = sql("CAST(NULL AS VARCHAR(255))"),
    prefix_facet_1 = sql("CAST(NULL AS VARCHAR(255))"),
    prefix_facet_2 = sql("CAST(NULL AS VARCHAR(255))"),
    prefix_facet_3 = sql("CAST(NULL AS VARCHAR(255))") #as.character(NA) #generates a varchar(max) column #sql("CAST(NULL AS VARCHAR(255))")
  )
  return(omopDf %>% collapse())
}

# We have to fix non standard length columns resulting from queries on different fields
standardiseOutputColumns = function(omopDf) {
  omopDf = omopDf %>% mutate(
    prefix_source = CAST(prefix_source %AS% VARCHAR(255L)),
    prefix_value_as_number_unit = CAST(prefix_value_as_number_unit %AS% VARCHAR(255L)),
    prefix_value_as_text = CAST(prefix_value_as_text %AS% VARCHAR(255L)),
    prefix_display = CAST(prefix_display %AS% VARCHAR(255L)),
    prefix_facet_1 = CAST(prefix_facet_1 %AS% VARCHAR(255L)),
    prefix_facet_2 = CAST(prefix_facet_2 %AS% VARCHAR(255L)),
    prefix_facet_3 = CAST(prefix_facet_3 %AS% VARCHAR(255L)) #as.character(NA) #generates a varchar(max) column #sql("CAST(NULL AS VARCHAR(255))")
  )
  return(omopDf %>% collapse())
}


#' a dbplyr table with addition class information
#' 
#' @param con - an omop database connection
#' @param tableName - the table name
#' @return the dbplyr table with addition class information
typedTbl = function(con, tableName) {
  tmp = dplyr::tbl(con, tableName)
  class(tmp) = c(class(tmp),paste0("omop_",tableName))
  return(tmp)
}