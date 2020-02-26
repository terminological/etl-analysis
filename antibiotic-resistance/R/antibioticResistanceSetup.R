
omop = Omop$new()
labTestValues = omop$measurement %>% 
    filter(!is.na(value_as_concept_id)) %>% 
    group_by(measurement_concept_id,value_as_concept_id) %>% 
    summarise(count=n()) %>% 
    omop$getConceptNames() %>% 
    arrange(desc(count))
  
testConcepts = Searcher$fromDataframe(omop, labTestValues,measurement_concept_id)
valueConcepts = Searcher$fromDataframe(omop, labTestValues,value_as_concept_id)
  
  #labTestValues %>% collect()
  
bacteriaIdentified = testConcepts$clone()$applyFilter(
    (concept_name %like% "%Bacteria identifi%")
  )
  
resistanceOutcome = valueConcepts$clone()$applyFilter(
    ((concept_name %like% "%sensiti%") |
       (concept_name %like% "%resis%")) & !(concept_name %like% "%ethicill%")
  )
