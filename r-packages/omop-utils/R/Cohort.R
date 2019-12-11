#' Get a searcher of the omop databse
#'
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' search = Searcher$new(omop)
Cohort = R6::R6Class("Cohort", public=list(
  #### Fields ----
  #' @field omop the omop connection
  omop = NULL,
  #' @field cohort the filtered cohort as a dataframe
  cohort = NULL,
  
  #### Methods ----
  #' @description create a cohort builder
  #' @param omop an R6 Omop object
  #' @param name a very short description of the cohort
  initialize = function(omop, name) {
    self$omop = omop
    self$cohort = omop$person %>% select(person_id)
  },

  #' @description apply filter to cohort that allows requires stated field is one from a range of concepts
  #' @param personDf a dataframe with a person_id field and a ???_concept_id field, optionally grouped by columns that may be needed for further filtering
  #' @param conceptIdField a dataframe with a concept_id field
  #' @param conceptDf a dataframe with a concept_id field holding concepts
  #' @return the builder with a cohort filtered and augmented with any additional fields from the person_df groups
  withConceptSetFilter = function(personDf, conceptIdField, conceptDf) {
    conceptIdField = ensym(conceptIdField)
    grps = c(personDf %>% groups(),as.symbol("person_id"))
    self$cohort = self$cohort %>% inner_join(
      personDf %>% mutate(concept_id=(!!conceptIdField)) %>% inner_join(conceptDf, by="concept_id") %>% select(!!!grps),
      by("person_id")
    )
    invisible(self)
  },

  #' @description apply filter to cohort that requires stated field is of specific value (which may be a concept id)
  #' @param personDf a dataframe with a person_id field and a ???_concept_id field, optionally grouped by columns that may be needed for further filtering
  #' @param valueExpr a standard dplyr filter expression
  #' @return the builder with a cohort filtered and augmented with any additional fields from the person_df groups
  withValueFilter = function(personDf, valueExpr) {
    valueExpr = enexpr(valueExpr)
    grps = c(personDf %>% groups(),as.symbol("person_id"))
    self$cohort = self$cohort %>% inner_join(
      personDf %>% filter(!!valueExpr) %>% select(!!!grps),
      by("person_id")
    )
    invisible(self)
  },

  #' @description apply filter to cohort that requires stated field is of specific value (which may be a concept id)
  #' @param personDf a dataframe with a person_id field and a ???_concept_id field, optionally grouped by columns that may be needed for further filtering
  #' @param conceptIdField a dataframe with a concept_id field
  #' @param conceptDf a dataframe with a concept_id field holding concepts
  #' @param valueExpr a standard dplyr filter expression
  #' @return the builder with a cohort filtered and augmented with any additional fields from the person_df groups
  withConceptAndValueFilter = function(personDf, conceptIdField, conceptDf, valueExpr) {
    conceptIdField = ensym(conceptIdField)
    valueExpr = enexpr(valueExpr)
    grps = c(personDf %>% groups(),as.symbol("person_id"))
    self$cohort = self$cohort %>% inner_join(
      personDf %>% mutate(concept_id = (!!conceptIdField)) %>% inner_join(conceptDf, by="concept_id") %>% filter(!!valueExpr) %>% select(!!!grps),
      by("person_id")
    )
    invisible(self)
  },

  #' @description apply filter to cohort itself
  #' @param filterExpr a filter expression as expected by dplyr::filter
  withFilter = function(filterExpr) {
    filterExpr = enexpr(filterExpr)
    self$cohort = self$cohort %>% filter(!!filterExpr)
    invisible(self)
  },

  #' @description determine the column names in the cohort (which may change after lots of filtering)
  #' @return the columns of the cohort (as a list of symbols)
  getCohortVariables = function() {
    return(lapply(colnames(self$cohort),as.symbol))
  },

  #' @description filter the
  #' @param gender a M or F value
  #' @return the columns of the cohort (as a list of symbols)
  withGender = function(gender) {
    if(gender=='F') {
      self$withFilter(gender_concept_id==8532)
    } else if (gender=="M") {
      self$withFilter(gender_concept_id==8507)
    } else {
      stop("gender must be F or M")
    }
  },

  #' @description execute a dplyr::compute
  compute = function() {
    self$cohort = self$cohort %>% compute() #name=paste0("cohort_",name),overwrite=TRUE)
    invisible(self)
  },

  #' @description execute a dplyr::compute and
  #' @return the cohort as a data frame
  get = function() {
    self$cohort = self$cohort %>% compute() #name=paste0("cohort_",name),overwrite=TRUE)
    return(self$cohort)
  },

  #' @description saves the data locally
  #' @param name the name of the file - initial part of path
  save = function(name) {
    filename = normalizePath(paste0(name,".cohort.rds"),mustWork = FALSE)
    saveRDS(self$cohort %>% collect(), filename)
    invisible(self)
  },
  
  #' @description loads the data
  #' @param name the name of the file - initial part of path
  load = function(name) {
    filename = normalizePath(paste0(name,".cohort.rds"),mustWork = FALSE)
    self$cohort = readRDS(filename)
    table = stringr::str_match(filename,"/([^/\\.]+)\\..+$")[1,2]
    dplyr::copy_to(self$omop$con, self$cohort,name=paste0(table,"_cohort"),overwrite=TRUE)
    invisible(self)
  },
  
  #' @description print the cohort name as size
  print = function() {
    print(paste0("The cohort has ",self$cohort %>% count() %>% collect()," patients"))
    invisible(self)
  }
))
