# ---
# title: "Analyse the content of the "
# output: html_document
# knit: (function(inputFile, encoding,...) {
#   rmarkdown::render(inputFile, encoding = encoding, output_dir = "~/Dropbox/threadAnalysis/working") })
# ---
# 
# ```{r include=FALSE}
library(tidyverse)
library(ggplot2)
devtools::load_all("~/Git/tidy-info-stats")
devtools::load_all("~/Git/standard-print-output")
# detach("package:standardPrintOutput", unload=TRUE)
# library(standardPrintOutput)
theme_set(defaultFigureLayout())

# ---- database setup ----
setwd('~/Dropbox/threadAnalysis/summary')
# ```

# ```{r}
#omop$finalize()
devtools::load_all("~/Git/etl-analysis/r-packages/omop-utils/")
# config <- config::get(file="~/Dropbox/db.yaml")
omop <- Omop$new("omop")#,config)

icuVisits = Cohort$new(omop, omop$getVisits(visit_type = omopCodes$visit_type$`Intensive Care`))
#View(icuVisits$cohort %>% collect())
# icuVisits$observationsPrior(omop$getDrugs())
tmpDiabConc = omop$getConditionConcepts()$applyTermFilter("%iabetes%")$compute()
tmpDiab = omop$getConditions(tmpDiabConc) %>% compute()
diabeticsDxAfter = icuVisits$outcomesPost(tmpDiab) %>% show_query() %>% compute()
diabeticsDxAfter = diabeticsDxAfter %>% filter(days_offset < 30) %>% compute()

icuVisits$requireOutcome(diabeticsDxAfter)
icuVisits$demographicObservations() %>% arrange(person_id) %>% glimpse()
icuVisits$demographicOutcomes() %>% glimpse()

#icuVisits$plot(binwidth=5)



# idd = icuVisits$observationsPrior(omop$getDrugs(omop$getDrugConcepts()$applyTermFilter("%nsulin%"))) %>% compute()
# glimpse(idd)
# class(idd)
# nidd = normaliseColumns(idd) %>% compute()
# nidd %>% show_query()
# glimpse(nidd)


omop$getConditions() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
#omop$getDemographics() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
omop$getDevices() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
omop$getDrugs() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
omop$getMeasurements() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
omop$getNoteNlp() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
omop$getNotes() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
omop$getObserations() %>% icuVisits$observationsPrior(normalise = TRUE) %>% glimpse()
omop$getProcedures() %>% icuVisits$observationsPrior() %>% omop$normaliseColumns() %>% glimpse()
omop$getVisits() %>% glimpse() %>% icuVisits$observationsPrior() %>% omop$normaliseColumns() %>% glimpse()





#### visualise cohort features ----
tmpCohort = icuVisits

tmpObs = tmpCohort$demographicObservations()
# tmpOut = icuVisits$demographicOutcomes()
tmpObs = tmpObs %>% union_all(omop$getConditions() %>% tmpCohort$observationsPrior(normalise = TRUE))
tmpObs = tmpObs %>% union_all(omop$getDevices() %>% tmpCohort$observationsPrior(normalise = TRUE))
tmpObs = tmpObs %>% union_all(omop$getDrugs() %>% tmpCohort$observationsPrior(normalise = TRUE))
tmpObs = tmpObs %>% union_all(omop$getMeasurements() %>% tmpCohort$observationsPrior(normalise = TRUE))
#tmpObs = tmpObs %>% union_all(omop$getNoteNlp() %>% tmpCohort$observationsPrior(normalise = TRUE))
tmpObs = tmpObs %>% union_all(omop$getNotes() %>% tmpCohort$observationsPrior(normalise = TRUE))
tmpObs = tmpObs %>% union_all(omop$getObserations() %>% tmpCohort$observationsPrior(normalise = TRUE))
tmpObs = tmpObs %>% union_all(omop$getProcedures() %>% tmpCohort$observationsPrior(normalise = TRUE))
tmpObs = tmpObs %>% union_all(omop$getVisits() %>% tmpCohort$observationsPrior(normalise = TRUE))

tmpObs = tmpObs %>% collect()


#### ----
tmpObs %>% group_by(person_id) %>% count() 
ggplot(tmpObs %>% filter(person_id == 48659),(aes(x=as.factor(feature_source), y=feature_days_offset,colour=as.factor(feature_source))))+geom_point(show.legend = FALSE)
#ggrepel::geom_text_repel(aes(label=feature_display))

ggplot(tmpObs %>% filter(feature_days_offset<365*3),aes(x=-feature_days_offset, colour=feature_source))+stat_density(aes(y=stat(scaled)),geom="line")
# icuVisits$withValueFilter(omop$visit_occurrence, visit_concept_id==local(omopCodes$visit_type$`Intensive Care`))
testVisits = icuVisits$get(cohortEntryDateVar = visit_start_datetime) %>% collect()

# tmpVisits = omop$person %>% omop$getVisits(visit_type = omopCodes$visit_type$`Intensive Care`) %>% omop$getConceptNames() %>% collect()
# 240652 has an ICU visit

devtools::load_all("~/Git/etl-analysis/r-packages/omop-utils/")
testPersonDf = tibble(person_id = c(347,379,351,240652))
summary = Cohort$new(omop, testPersonDf)
tmpVisits = summary$observationsPrior(omop$getVisits(visit_type = omopCodes$visit_type$`Emergency Room Visit`))
#  omop$getConceptNames() %>% collect()
# ```

tmpDemographics = 
  #  glimpse(
  testPersonDf %>% omop$getVisits(visit_type = omopCodes$visit_type$`Intensive Care`) %>%
  omop$getDemographics(beforeDateVar = visit_start_datetime) %>% show_query() %>% collect()



tmpConditions = testPersonDf %>% omop$getConditions() %>% omop$getConceptNames() %>% collect()
tmpMeasurements = testPersonDf %>% omop$getMeasurements() %>% omop$getConceptNames() %>% collect()
# Searcher$new(omop)$applyUsedInFilter(omop$observation, observation_type_concept_id)

#View(omop$note_nlp %>% select(lexical_variant,note_nlp_concept_id) %>% omop$getConceptNames() %>% head(1000) %>% collect())
#```


The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
