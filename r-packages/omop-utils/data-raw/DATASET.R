## code to prepare `DATASET` dataset goes here
library(dplyr)
library(stringr)
devtools::load_all("~/Git/classifier-result")
devtools::load_all("~/Git/tidy-info-stats")
devtools::load_all("~/Git/etl-analysis/r-packages/omop-utils")

dataDev = new.env()
with(dataDev, {
  
  setwd("~/Git/etl-analysis/r-packages/omop-utils/")

  omop = Omop$new()
  
  usedIn = function(table,field) {
    s = Searcher$new(omop)
    s$applyUsedInFilter(table, {{field}})
    s$toDataframe() %>% select(concept_name,concept_id) %>% collect() %>% tidyr::pivot_wider(names_from = "concept_name", values_from = "concept_id")
  }
  
  omopCodes = list(
    gender = usedIn(omop$person, gender_concept_id),
    visit_type = usedIn(omop$visit_occurrence, visit_concept_id),
    observation_value = usedIn(omop$observation, value_as_concept_id),
    demographics=list(
      observation_of_death = 4306655,
      current_chronological_age = 4314456,
      survival_time = 40482950,
      gender=2,
      length_of_stay = 46236995 #1 Hospital stay duration
    ),
    #measurement_value = usedIn(omop$measurement, value_as_concept_id), - lots of bacteria / use a dataframe / searcher
    measurement_type = usedIn(omop$measurement, measurement_type_concept_id),
    #condition_status = usedIn(omop$condition_occurrence, condition_status_concept_id), - only single value
    #condition_type = usedIn(omop$condition_occurrence, condition_type_concept_id) - only single value
    note_type = usedIn(omop$note, note_type_concept_id),
    note_class = usedIn(omop$note, note_class_concept_id),
    #note_nlp_section = usedIn(omop$note_nlp, section_concept_id), # note_class and note_nlp_section are identical and this is redundant (in our omop)
    note_nlp_temporal = list(current="current",historical="historical"),
    drug_route = usedIn(omop$drug_exposure, route_concept_id)
    
  )
  
  omopSchema = NULL
  for(xName in ls(omop)) {
    x = get(xName,envir = omop)
    if (any(class(x)=="tbl")) {
      omopSchema = omopSchema %>% bind_rows(tibble(
        tbl = xName,
        cols = colnames(x),
        sql_types = sapply(x %>% filter(sql("0=1")) %>% collect(), type_sum)
      ))
    }
  }
  
  # tmpMapped = c("condition_occurrence","device_exposure","drug_exposure","measurement","note","note_nlp","observation","person","procedure_occurrence","visit_occurrence")
  # 
  # omopSchema = omopSchema %>% mutate(maps_to = NA) %>%
  #   mutate(maps_to = ifelse(cols %>% stringr::str_ends("_start_datetime"), "prefix_datetime", maps_to)) %>%
  #   mutate(maps_to = ifelse(cols %>% stringr::str_detect(".+_*(?<!(type|method|source|status))_concept_id"), "prefix_concept_id", maps_to)) %>%
  #   mutate(maps_to = ifelse(tbl %in% tmpMapped, maps_to, NA))
  
  usethis::use_data(omopCodes, overwrite = TRUE)
  usethis::use_data(omopSchema, overwrite = TRUE)
  rm(omop)
})

rm(dataDev)