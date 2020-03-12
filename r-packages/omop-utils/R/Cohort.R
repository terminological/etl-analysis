#' Get a cohort of patients in the omop database that satisfy a set of criteria, define a cohort_person_id and a cohort_entry_datetime for the members of the cohort
#' 
#' A cohort may be patients for example
#' 
#' * Admitted into the hospital at a specific time
#' * Patients with a specific type of test result
#' * 
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
  
  #### Constructor ----
  #' @description create a cohort builder
  #' @param omop an R6 Omop object
  #' @param eventDf a dataframe that defines the principal zero day event that admits person to cohort. It must define a person_id column.
  #' @param cohortEntryDateVar the column in eventDf that has the zero day date. if not present the first column named ??_start_datetime or ??_datetime will be used, or if none found today's date
  #' @param cohortIdVar a column in eventDf that contians a unique id for this event. If not present then one is generated
  #' @param retainFilteringCriteria keep the columns from eventDf in the cohort? (can use applySelect to modify them later) - can create duplicates. must have cohortIdVar defined
  initialize = function(omop, eventDf, cohortEntryDateVar = NULL, cohortIdVar=NULL, retainFilteringCriteria=FALSE) {
    
    cohortEntryDateVar = tryCatch(ensym(cohortEntryDateVar),error = function(e) findStartDatetime(eventDf))
    cohortIdVar = tryCatch(ensym(cohortIdVar), error = function(e) return(NULL))
    if (retainFilteringCriteria & identical(cohortIdVar,NULL)) stop("if retainFilteringCriteria is true cohortIdVar must be given (and reference a unique id)")
    
    self$omop = omop
    
    
    if (!identical(cohortEntryDateVar,NULL)) {
      eventDf = eventDf %>% mutate(cohort_entry_datetime = !!cohortEntryDateVar)
    } else {
      eventDf = eventDf %>% mutate(cohort_entry_datetime = sql("getdate()"))
    }
    
    
    if (identical(cohortIdVar,NULL)) {
      eventDf = eventDf %>% arrange(person_id,cohort_entry_datetime) %>% mutate(cohort_person_id = as.integer64(row_number())) %>% unarrange()
    } else {
      eventDf = eventDf %>% mutate(cohort_person_id = !!cohortIdVar)
    }
    
    if(!retainFilteringCriteria) eventDf = eventDf %>% select(person_id, cohort_person_id, cohort_entry_datetime)
    
    self$cohort = omop$person %>% select(person_id, gender_concept_id, birth_datetime, death_datetime) %>% inner_join(eventDf, by="person_id", copy=TRUE)
    
    self$cohort = self$cohort %>% mutate(
        age_in_days = datediff(sql("day"), birth_datetime, cohort_entry_datetime),
        survival_in_days = datediff(sql("day"), cohort_entry_datetime, death_datetime)
    )
    
    class(self$cohort) = unique(c(class(self$cohort),"omop_cohort"))

  },

  
  
  #' @description apply filter to cohort itself
  #' @param filterExpr a filter expression as expected by dplyr::filter
  applyFilter = function(...) {
    filterExprs = enexpr(...)
    self$cohort = self$cohort %>% filter(!!!filterExprs)
    return(self)
  },
  
  #' @description apply mutate to cohort itself
  #' @param filterExpr a filter expression as expected by dplyr::filter
  applyMutate = function(...) {
    mutateExprs = enexpr(...)
    self$cohort = self$cohort %>% mutate(!!!mutateExprs)
    return(self)
  },

  #' @description apply mutate to cohort itself
  #' @param filterExpr a filter expression as expected by dplyr::filter
  applySelect = function(...) {
    selectExprs = enexpr(...)
    self$cohort = self$cohort %>% select(person_id,cohort_person_id,cohort_entry_datetime,!!!selectExprs)
    return(self)
  },
  
  #' @description determine the column names in the cohort (which may change after lots of filtering)
  #' @return the columns of the cohort (as a list of symbols)
  getCohortVariables = function() {
    return(lapply(colnames(self$cohort),as.symbol))
  },

  #### Cohort filtering ----
  #' @description apply gender filter to cohort itself
  #' @param gender a vector of codes from omopCodes$gender
  applyGenderFilter = function(gender = c(omopCodes$gender$FEMALE, omopCodes$gender$MALE)) {
    self$cohort = self$cohort %>% filter(gender_concept_id %in% local(c(gender)))
    return(self)
  },
  
  #' @description apply age filter to cohort itself
  #' @param minYears minimum age as float (calculation based on age in days)
  #' @param maxYears maximum age as float
  applyAgeFilter = function(minYears=0,maxYears=200) {
    self$cohort = self$cohort %>% filter(age_in_days<=(maxYears*365.25) & age_in_years>=(minYears*365.25))
    return(self)
  },
  
  #' @description match only patients that have a specific observation (see observationsPrior)
  #' @param observationDf a dataframe of desired observations with a 
  requireObservation = function(observationDf) {self$requireFeature(observationDf, negate=FALSE, retainFilteringCriteria=FALSE)},
  requireOutcome = function(outcomeDf) {self$requireFeature(outcomeDf, negate=FALSE, retainFilteringCriteria=FALSE)},
  excludeObservation = function(observationDf) {self$requireFeature(observationDf, negate=TRUE, retainFilteringCriteria=FALSE)},
  excludeOutcome = function(outcomeDf) {self$requireFeature(outcomeDf, negate=TRUE, retainFilteringCriteria=FALSE)},
  
  #' @description match only patients that have a specific feature (see observationsPrior() or outcomesPost())
  #' @param observationDf a dataframe of desired observations with a 
  #' @param negate - if TRUE the patients in featureDf will be excluded if FALSE all other patients are excluded
  #' @param retainFilteringCriteria - keep the feature in the cohort table. This can lead to duplicate cohort_person_id records and needs to be treated with care
  requireFeature = function(featureDf, negate, retainFilteringCriteria=FALSE) {
    joinList = c("person_id")
    if ("cohort_person_id" %in% colnames(featureDf)) {joinList = c(joinList,"cohort_person_id")}
    if (negate) {
      self$cohort = self$cohort %>% anti_join(featureDf, by=joinList)
    } else {
      if (!retainFilteringCriteria) {
        self$cohort = self$cohort %>% semi_join(featureDf, by=joinList)
      } else {
        # TODO: potential to duplicate unless cohort_person_id is present and unique
        # identify overlapping columns in the feature dataframe
        overlap = colnames(featureDf)[colnames(featureDf) %in% colnames(self$cohort)]
        overlap = overlap[not(overlap %in% joinList)]
        featureDf = featureDf %>% select(-overlap) %>% compute()
        self$cohort = self$cohort %>% inner_join(featureDf, by=joinList)
      }
    }
  },
  
  #### Create features ----
  
  generateFeatures = function(omopDf, timeVar, before=TRUE) {
    cohortDf = self$cohort %>% select(person_id, cohort_person_id, cohort_entry_datetime)
    timeVar = tryCatch(ensym(timeVar),error = function(e) findStartDatetime(omopDf))
    
    joinList = c("person_id")
    if ("cohort_person_id" %in% colnames(omopDf)) {joinList = c(joinList,"cohort_person_id")}
    
    tmp = omopDf %>% mutate(tmp_cohort_time = !!timeVar) %>% inner_join(cohortDf, by=joinList)
    if (before) {
      tmp = tmp %>% filter(tmp_cohort_time <= cohort_entry_datetime)
    } else {
      tmp = tmp %>% filter(tmp_cohort_time >= cohort_entry_datetime)
    } 
    
    tmp = tmp %>% mutate(
      days_offset := as.double(DATEDIFF_BIG(sql("second"), cohort_entry_datetime, tmp_cohort_time))/86400.0
    ) %>% select(-tmp_cohort_time)
    class(tmp) = unique(c(class(tmp),"omop_cohort"))
    return(tmp)
  },
  
  observationsPrior = function(observationDataDf, observationDataTimeVar = NULL, normalise=FALSE) {
    observationDataTimeVar = tryCatch(ensym(observationDataTimeVar),error = function(e) NULL)
    tmp = self$generateFeatures(observationDataDf, !!observationDataTimeVar, before=TRUE)
    tmp = tmp %>% mutate(days_offset = -days_offset)
    if (normalise) tmp = tmp %>% self$omop$normaliseColumns("feature")
    return(tmp)
  },
  
  outcomesPost = function(outcomeDataDf, outcomeDataTimeVar = NULL, normalise=FALSE) {
    outcomeDataTimeVar = tryCatch(ensym(observationDataTimeVar),error = function(e) NULL)
    tmp = self$generateFeatures(outcomeDataDf, !!outcomeDataTimeVar, before=FALSE)
    if (normalise) tmp = tmp %>% self$omop$normaliseColumns("outcome")
    return(tmp)
  },
  
  demographicOutcomes = function() {
    prefix = "outcome"
    selection = c("person_id","cohort_person_id","cohort_entry_datetime", "prefix_source", "prefix_datetime", "prefix_concept_id", "prefix_value_as_number", 
                  "prefix_value_as_number_min", "prefix_value_as_number_max", "prefix_value_as_number_unit", "prefix_value_as_date", "prefix_value_as_text",
                  "prefix_value_as_concept_id", "prefix_days_offset", "prefix_display", "prefix_facet_1", "prefix_facet_2", "prefix_facet_3")
    omopDf = self$cohort %>% defaultNormalisedColumns()
    out = omopDf %>% mutate(
      prefix_source = "omop_person",
      prefix_datetime = cohort_entry_datetime,
      prefix_concept_id = local(omopCodes$demographics$observation_of_death),
      prefix_value_as_number = survival_in_days,
      prefix_value_as_number_unit = "days",
      prefix_value_as_datetime = death_datetime,
      prefix_display = ifelse(!is.na(survival_in_days),paste0("Survival: ",as.character(survival_in_days)," days"),"Survival: censored")
    ) %>% select(selection) %>% standardiseOutputColumns()
    ren = selection %>% stringr::str_replace("prefix",prefix)
    names(selection) = ren
    selection = selection[ren!=selection]
    if(!is.null(selection)) out = out %>% rename(selection)
    return(out)
  },
  
  demographicObservations = function() {
    prefix = "feature"
    selection = c("person_id","cohort_person_id","cohort_entry_datetime", "prefix_source", "prefix_datetime", "prefix_concept_id", "prefix_value_as_number", 
                  "prefix_value_as_number_min", "prefix_value_as_number_max", "prefix_value_as_number_unit", "prefix_value_as_date", "prefix_value_as_text",
                  "prefix_value_as_concept_id", "prefix_days_offset", "prefix_display", "prefix_facet_1", "prefix_facet_2", "prefix_facet_3")
    omopDf = self$cohort %>% defaultNormalisedColumns()
    out = omopDf %>% mutate(
      prefix_source = "omop_person",
      prefix_datetime = cohort_entry_datetime,
      prefix_concept_id = local(omopCodes$demographics$current_chronological_age),
      prefix_value_as_number = age_in_days/365.25,
      prefix_value_as_number_unit = "year",
      prefix_display = paste0("Age: ",as.character((age_in_days/365.25))," years")
    ) %>% select(selection) %>% standardiseOutputColumns()
    out = out %>% union_all(
      omopDf %>% mutate(
        prefix_source = "omop_person",
        prefix_datetime = cohort_entry_datetime,
        prefix_concept_id = local(omopCodes$demographics$gender),
        prefix_value_as_concept_id = gender_concept_id,
        prefix_display = paste0("Gender: ",
            ifelse(gender_concept_id == local(omopCodes$gender$FEMALE),"female",
                   ifelse(gender_concept_id == local(omopCodes$gender$MALE),"male","unknown")
                   ))
      ) %>% select(selection) %>% standardiseOutputColumns()
    )
    #TODO: race etc.
    ren = selection %>% stringr::str_replace("prefix",prefix)
    names(selection) = ren
    selection = selection[ren!=selection]
    if(!is.null(selection)) out = out %>% rename(selection)
    return(out)
  },
  
  #### Persistance / Printing ----
  
  #' @description execute a dplyr::compute
  compute = function() {
    self$cohort = self$cohort %>% compute() #name=paste0("cohort_",name),overwrite=TRUE)
    return(self)
  },
  
  #' @description save or restore the cohort from a cache database table
  #' @return the searcher with a cached result
  persist = function(tablename) {
    self$result = self$omop$persist(tablename, self$cohort, indexes=list("person_id"))
    invisible(self)
  },
  
  #' @description saves the data locally in rds file
  #' @param name the name of the file - initial part of path
  save = function(name) {
    filename = normalizePath(paste0(name,".cohort.rds"),mustWork = FALSE)
    saveRDS(self$cohort %>% collect(), filename)
    invisible(self)
  },
  
  #' @description print the cohort name as size
  print = function() {
    print(paste0("The cohort has ",self$cohort %>% count() %>% collect()," patients"))
    invisible(self)
  },
  
  #' @description plot a summary of the cohort
  plot = function(...) {
    plot.Cohort(self, ...)
  }
    
))

#### Static functions ----


#' @name Cohort_fromDataframe
#' @title Create a cohort from a data frame
#' @usage Cohort$fromDataframe(df)
#' @param omop the omop connection as a Omop class object
#' @param df a dataframe with a person_id field.
#' @return a data from of targetConcepts
NULL
Cohort$fromDataframe = function(omop, df) {
  s = Cohort$new(omop, df)
  return(s)
}

#' @name Searcher_load
#' @title Create a searcher from a .RDS file
#' @usage Searcher$load(path)
#' @description loads the data from local path
#' @param name the name of the file - initial part of path - no .vocab.rds extension
#' @return the modified searcher itself
NULL
Cohort$load = function(omop, name) {
  s = Cohort$new(omop)
  filename = normalizePath(paste0(name,".cohort.rds"),mustWork = FALSE)
  s$cohort = readRDS(filename)
  table = stringr::str_match(filename,"/([^/\\.]+)\\..+$")[1,2]
  dplyr::copy_to(self$omop$con, self$cohort,name=paste0(table,"_cohort"),overwrite=TRUE)
  return(s)
}

#' plots a Cohort object
#' 
#' @param x - a cohort
#' @param smoothing - the half width of the smoothing window to apply to the date  in measurements
#' @param binwidth - the width in years of each age group in the histogram
#' @export
plot.Cohort = function(x, smoothing=20, binwidth=5, ...) {
  tmp = x$cohort %>% collect()
  tmp = tmp %>% 
    filter(gender_concept_id %in% local(c(omopCodes$gender$FEMALE,omopCodes$gender$MALE))) %>% collect() %>% 
    mutate(
      age_in_years = as.integer(floor(age_in_days/365)),
      gender_concept_id = factor(gender_concept_id, levels=c(omopCodes$gender$FEMALE,omopCodes$gender$MALE), labels=c("Female","Male"))
    )
  p1 = ggplot(tmp,aes(x=age_in_years,fill=gender_concept_id))+
      xlab("Age")+ylab("Count")+
      geom_histogram(binwidth=binwidth, )+
      facet_wrap(vars(gender_concept_id))+
      guides(fill="none")
  tmp = tmp %>% mutate(time = as.double(difftime_big(cohort_entry_datetime,max(cohort_entry_datetime),units="secs")))
  tmp2 = tidyinfostats::probabilitiesFromContinuous_SGolay(tmp, time, k_05 = smoothing) %>% mutate(date = max(tmp$cohort_entry_datetime)+time) %>% mutate(yearly = nrow(tmp)*p_x*60*60*24*365.25)
  tmp2 = tmp2 %>% head(-smoothing) %>% tail(-smoothing)
  p2 = tmp %>% 
    ggplot(aes(x=cohort_entry_datetime))+
    geom_rug(sides="b",alpha=0.1,size=1,length = unit(1, "npc"))+
    geom_line(aes(x=date, y=yearly),tmp2,colour="blue")+
    #stat_density(aes(y=stat(count)*60*60*24*365.25),geom="line",colour="blue")+
    xlab("Date")+
    ylab("Per yr")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(patchwork::wrap_plots(p1,p2,ncol=1,heights = c(3,1)))
}

