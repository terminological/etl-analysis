#' #' get
#' #'
#' #' @param timeUnit - https://docs.microsoft.com/en-us/sql/t-sql/functions/datediff-transact-sql?view=sql-server-ver15
#' getDataForPerson = function(sourceDf, targetDf, sourceDateVar=NULL, targetDateVar=NULL, timeDiffUnit = "day", timeDiffVar = "time_to_event", ...) {
#'   sourceDateVar = tryCatch(ensym(sourceDateVar), error=function(e) NULL)
#'   targetDateVar = tryCatch(ensym(targetDateVar), error=function(e) NULL)
#'   timeDiffVar = ensym(timeDiffVar)
#'   timeDiffUnitVar = as.symbol(paste0(rlang::as_name(timeDiffVar),"_unit"))
#'   
#'   targetDf = targetDf %>% filter(...)
#'   
#'   if (identical(sourceDateVar,NULL) | identical(targetDateVar,NULL)) {
#'     
#'     tmp = targetDf %>% inner_join(sourceDf %>% select(person_id), copy=TRUE, by="person_id")
#'     
#'   } else {
#'     
#'     tmp = targetDf %>% inner_join(sourceDf %>% select(person_id, !!sourceDateVar), copy=TRUE, by="person_id") %>% 
#'       filter(!!sourceDateVar >= !!targetDateVar) %>%
#'       mutate(
#'         !!timeDiffVar := datediff(sql(timeDiffUnit), !!targetDateVar, !!sourceDateVar),
#'         !!timeDiffUnitVar := local(timeDiffUnit)
#'       )
#'     
#'   }
#'   
#'   
#'   
#' },
#' 
#' #TODO: some form of event_id
#' 
#' getVisits = function(personDf, visit_type = omopCodes$visit_type %>% anyOf(), beforeDateVar = NULL,  timeDiffUnit="day", ...) {
#'   beforeDateVar = tryCatch(ensym(beforeDateVar), error=function(e) NULL)
#'   tmp2 = self$visit_occurrence %>% filter(visit_concept_id %in% local(c(visit_type)))
#'   tmp = self$getDataForPerson(personDf, tmp2, !!beforeDateVar, visit_start_datetime, timeDiffUnit, "visit_time_diff", ...)
#'   tmp = tmp %>% mutate(los_days = datediff(sql("day"),visit_start_date,visit_end_date))
#'   return(tmp)
#' },
#' 
#' 
#' getObserations = function(personDf, beforeDateVar = NULL,  timeDiffUnit="day", ...) {
#'   beforeDateVar = tryCatch(ensym(beforeDateVar), error=function(e) NULL)
#'   tmp = self$getDataForPerson(personDf, omop$observation, !!beforeDateVar, observation_datetime, timeDiffUnit, "observation_time_diff", ...)
#'   return(tmp)
#' },
#' 
#' getMeasurements = function(personDf, beforeDateVar = NULL,  timeDiffUnit="day", ...) {
#'   beforeDateVar = tryCatch(ensym(beforeDateVar), error=function(e) NULL)
#'   tmp = self$getDataForPerson(personDf, omop$measurement, !!beforeDateVar, measurement_datetime, timeDiffUnit, "measurement_time_diff", ...)
#'   return(tmp)
#' },
#' 
#' getDemographics = function(personDf, gender = omopCodes$gender %>% anyOf, beforeDateVar = NULL,  ageUnit="year", ...) {
#'   beforeDateVar = tryCatch(ensym(beforeDateVar), error=function(e) NULL)
#'   tmp2 = self$person %>% filter(gender_concept_id %in% local(c(gender)))
#'   if (!identical(beforeDateVar,NULL)) {
#'     tmp = tmp2 %>% inner_join(personDf %>% select(person_id,!!beforeDateVar), copy=TRUE, by="person_id")
#'     tmp = tmp %>% mutate(
#'       age_at_event = datediff(sql(ageUnit), birth_datetime, !!beforeDateVar),
#'       age_at_event_unit = local(ageUnit)
#'     )
#'     tmp = tmp %>% mutate(days_survival_after_event = datediff(sql("day"), !!beforeDateVar, death_datetime))
#'   } else {
#'     tmp = tmp2 %>% inner_join(personDf %>% select(person_id), copy=TRUE, by="person_id")
#'   }
#'   return(tmp)
#' },
#' 
#' getConditions = function(personDf, beforeDateVar = NULL,  timeDiffUnit="day", ...) {
#'   beforeDateVar = tryCatch(ensym(beforeDateVar), error=function(e) NULL)
#'   tmp = self$getDataForPerson(personDf, omop$condition_occurrence, !!beforeDateVar, condition_start_datetime, timeDiffUnit, "condition_start_time_diff", ...)
#'   return(tmp)
#' }#,
#' 
#' #### Utilities ----
#' #' @description apply filter to cohort that allows requires stated field is one from a range of targetConcepts
#' #' @param observationDf a dataframe with a person_id field and a ???_targetConcept_id field, optionally grouped by columns that may be needed for further filtering
#' #' @param targetConceptIdField a dataframe with a targetConcept_id field
#' #' @param targetConceptDf a dataframe with a targetConcept_id field holding targetConcepts, that are outcomes of interest
#' #' @return the builder with a cohort filtered and augmented with any additional fields from the observationDf groups, targetConceptDf groups, the targetConceptIdField
#' withConceptSetFilter = function(observationDf, targetConceptDf, targetConceptIdField) {
#'   targetConceptIdField = ensym(targetConceptIdField)
#'   targetConceptJoin = as.character(targetConceptIdField)
#'   #grps = c(observationDf %>% groups(),targetConceptDf %>% groups(), targetConceptIdField,as.symbol("person_id"))
#'   self$cohort = self$cohort %>% inner_join(
#'     observationDf %>% inner_join(targetConceptDf, by=targetConceptJoin), #%>% select(!!!grps),
#'     by("person_id")
#'   )
#'   invisible(self)
#' },
#' 
#' #' @description apply filter to cohort that requires stated field is of specific value (the value may be a concept id or e.g. age)
#' #' @param observationDf a dataframe with a person_id field, and optionally grouped by columns that may be needed for further filtering
#' #' @param ... a set of standard dplyr filter expressions for observationDf
#' #' @return the builder with a cohort filtered and augmented with any additional fields from the person_df groups
#' withValueFilter = function(observationDf, ...) {
#'   valueExprs = enexprs(...)
#'   #grps = c(observationDf %>% groups(),as.symbol("person_id"))
#'   self$cohort = self$cohort %>% inner_join(
#'     observationDf %>% filter(!!!valueExprs), #%>% select(!!!grps),
#'     by="person_id"
#'   )
#'   invisible(self)
#' },
#' 
#' #' @description apply filter to cohort that requires stated field is of specific value (which may be a concept id)
#' #' @param observationDf a dataframe with a person_id field and a ???_concept_id field, optionally grouped by columns that may be needed for further filtering
#' #' @param targetConceptIdField the name of the ???_concept_id field in the target dataframe
#' #' @param targetConceptDf a dataframe with a concept_id field holding concepts and potentially filtering criteria (e.g. targetConceptId - sensisitvity to penicilling, value = sensitive )
#' #' @param ... standard dplyr filter expressions which can reference columns in observationDf or targetConceptDf
#' #' @return the builder with a cohort filtered and augmented with any additional fields from the person_df groups
#' withConceptAndValueFilter = function(observationDf, targetConceptDf, targetConceptIdField, ...) {
#'   targetConceptIdField = ensym(targetConceptIdField)
#'   targetConceptJoin = as.character(targetConceptIdField)
#'   valueExprs = enexprs(...)
#'   #grps = c(observationDf %>% groups(), targetConceptDf %>% groups(),as.symbol("person_id"))
#'   self$cohort = self$cohort %>% inner_join(
#'     observationDf %>% inner_join(targetConceptDf, by=targetConceptJoin) %>% filter(!!!valueExprs), # %>% select(!!!grps),
#'     by("person_id")
#'   )
#'   invisible(self)
#' },
#' 
#' 
#' #' @description given a set of concepts, find every instance in an arbitrary dataframe that is a descendant of that concept
#' #' @param targetConceptSetDf a data frame containing minimally a set of concepts of interest in a concept_id column, plus optionally any other metadata about that concept - (e.g. associated outcome class...)
#' #' @param observationsDf a data frame containing observations with associated concept ids
#' #' @param observationsConceptIdVar the concept variable in the observations table
#' #' @param min the minimum distance to consider - if not zero then the results will not include the target concepts themselves
#' #' @param max the maximum distance to consider - how many levels to expand
#' #' @return a dataframe containing matching observations, augmented with a observed_concept_id, a hierarchical distance (separation), a target_concept_id, and all columns of targetConceptSetDf
#' getMatchingDescendantsFrom = function(targetConceptSetDf, observationsDf, observationsConceptIdVar, min=0, max=1000) {
#'   outputCols = c(colnames(targetConceptSetDf),colnames(observationsDf),"observed_concept_id","target_concept_id","separation")
#'   if (anyDuplicated(outputCols)) warning(paste("Name collision in matching descendants:",outputCols[duplicated(outputCols)],sep=" ", collapse=" "))
#'   observationsConceptIdVar = ensym(observationsConceptIdVar)
#'   tmp = observationsDf %>% mutate(observed_concept_id = !!observationsConceptIdVar)
#'   tmp2 = self$concept_ancestor %>% select(
#'     observed_concept_id=descendant_concept_id,
#'     target_concept_id=ancestor_concept_id,
#'     separation=min_levels_of_separation
#'   ) %>% filter(separation < max & separation >= min) %>%
#'     inner_join(targetConceptSetDf, by="target_concept_id", copy=TRUE) %>%
#'     inner_join(tmp, by="observed_concept_id", copy=TRUE)
#'   return(tmp2)
#' },
#' 
#'   #### Temp table management ----
#' 
#' #' Drops a temporary table created by dbplyr
#' #' 
#' #' This function could have major side effects and does not check to
#' #' see if the table is still in use anywhere to use with caution only
#' #' if you know that no other current queries depend on it
#' #' @param tableRef a dbplyr backed dataframe
#' #' @param dryRun defaults to debuggingState(). will only log intention when TRUE. set to FALSE to actually drop the table.
#' #' @param force defaults to FALSE. If an attemp is made to drop a non temporary table it will only be attempted if this flag is TRUE
#' tidyTempTable = function(tableRef, dryRun = debuggingState(), force=FALSE) {
#'   return()
#'   # TODO: this needs a lot of work
#'   # ?enquo, ?eval, ?eval_tidy
#'   name = deparse(substitute(tableRef))
#'   tableRef = enquo(tableRef)
#'   if (is.null(rlang::eval_tidy(tableRef))) return() # evaluates to null of self$nodes but works on groupedDf
#'     # possible we shoudl be dealing with enquo sures
#'   rn = rlang::eval_tidy(tableRef) %>% dbplyr::remote_name() # doesn;t in general return a table name but a remote_query seems to work
#'   if (is.null(rn)) {
#'     print(paste0("attempt to drop a table failed - maybe it is a query: ",name))
#'     return()
#'   }
#'   rn = rn %>% as.character()
#'   if (substring(rn,1,2) == "##") {
#'     if (test) {
#'       print(paste0("DEBUG: would have dropped: ",name," = ",rn))
#'     } else {
#'       odbc::dbRemoveTable(self$con,rn)
#'     }
#'   } else {
#'     if (force) {
#'       print(paste0("WARNING: Forced drop of non temporary table: ",name," = ",rn))
#'       odbc::dbRemoveTable(self$con,rn)
#'     } else {
#'       stop(paste0("CRITICAL: tried to drop non temporary table: ",name," = ",rn))
#'     }
#'   }
#' }
