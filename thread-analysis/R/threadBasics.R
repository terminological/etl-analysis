library(tidyverse)
library(reshape2)
library(cowplot)
# detach("package:phdUtils", unload=TRUE)
library(phdUtils)
library(dbplot)
# library(boot)
library(zoo)


# ---- database setup ----
setwd('~/Dropbox/threadAnalysis/summary')
config <- config::get(file="~/Dropbox/db.yaml")
omop <- getOmop("omop",config)

# ---- people by year of birth ----
omop$person %>% count()
omop$person %>% 
  filter(!is.na(year_of_birth) & year_of_birth>1910) %>%
  db_compute_bins(year_of_birth, binwidth = 1) %>% 
  select(year_of_birth,count) %>%
  ggplot(aes(x=year_of_birth,y=count))+
  xlab("Year of birth")+ylab("Count")+
  geom_col(fill="blue",width=0.8)
savePubBig("peopleByYob",print_aspect_ratio=2)

# ---- visits by type ----
omop$visit_occurrence %>% 
  inner_join(omop$concept, by=c("visit_concept_id"="concept_id")) %>%
  dbplot_bar(concept_name) + scale_y_log10()
savePubSmall("visitsByType")

# tryCatch({
tmp <- omop$person %>% left_join(
  omop$visit_occurrence %>% 
    inner_join(omop$concept, by=c("visit_concept_id"="concept_id")) %>% 
    group_by(person_id, concept_name) %>%
    summarise(count = n())) %>%
  select(person_id,concept_name,count) %>%
  collect() %>%
  filter(!is.na(concept_name)) %>%
  mutate(concept_name=factor(concept_name)) %>%
  complete(person_id,concept_name,fill = list(count = 0)) %>%
  group_by("Visit type"=concept_name) %>%
  summarise(
    "Count" = as.integer(sum(count)),
    "Count per patient" = mean(count)
  ) %>% saveTable("visitsByType")

# },error = function(e) e)

# %>% scale_y_log10()

omop$visit_occurrence %>% 
  db_compute_count(visit_start_date) %>% 
  ggplot(aes(x=visit_start_date, y=`n()`))+geom_point(alpha=0.4)
savePubBig("visitsByTime",print_aspect_ratio=2)



# ---- non outpatient visits by type - length of stay (int days) and counts ----

outpatient_visit_concept_id <- 9202

# omop$visit %>% 
#   mutate( los = datediff(minute,visit_start_datetime,visit_end_datetime)) %>%
#   filter( visit_concept_id != outpatient_visit_concept_id) %>%
#   group_by( visit_concept_id ) %>%
#   summarise( 
#     count = n(),
#     mean = mean(los)
# ) %>% inner_join(omop$concept, by=c("visit_concept_id"="concept_id"))

# ---- non outpatient visits by time of week - los vs time of week ----
# No clear cut weekly pattern but affected by deidentification
tmp <- omop$visit_occurrence %>% 
  mutate( 
    los = datediff(minute,visit_start_datetime,visit_end_datetime),
    from_week_start = datediff(hour,dateadd(week, datediff(week, '19050101', visit_start_datetime), '19050101'),visit_start_datetime)
  ) %>% 
  filter( visit_concept_id != outpatient_visit_concept_id) %>%
  group_by( from_week_start ) %>%
  summarise( 
    count = n(),
    los = mean(los)
  ) %>%
  collect()

tmp <- tmp %>%
  mutate( los_trend = rollmean(los,21, fill="extend"))

tmp %>%
  ggplot() +
  geom_line(aes(x=from_week_start, y=los_trend))+
  geom_point(aes(x=from_week_start, y=los))

savePubBig("losOverWeek",print_aspect_ratio=2)

# ---- non outpatient visits by time of week - los vs daily volume ----
# No clear cut weekly pattern but affected by deidentification
tmp <- omop$visit_occurrence %>% 
  mutate( 
    los = datediff(minute,visit_start_datetime,visit_end_datetime)
  ) %>% 
  filter( visit_concept_id != 9202) %>%
  group_by( visit_start_date ) %>%
  summarise( 
    count = n(),
    los = mean(los)
  )
tmp %>% ggplot() +
  geom_point(aes(x=count, y=los))
savePubSmall("losVsInptVisitsPerDay",print_aspect_ratio=1)

summary(lm(count ~ los,tmp))
# No clear relationship between los and daily counts but again this will be affected by deidentification
# https://www.statmethods.net/advstats/bootstrapping.html could be used if we wanted a confidence interval on Rsq


# ---- counts of various observations by day ----
tmp <- omop$condition_occurrence %>% db_compute_count( condition_start_date ) %>% 
  collect() %>% select( date = condition_start_date, count = `n()`) %>% mutate(type = 'condition') %>% 
  union(
    omop$procedure_occurrence %>% db_compute_count( procedure_date ) %>% 
      collect() %>% select( date = procedure_date, count = `n()`) %>% mutate(type = 'procedure')
  ) %>%
  union(
    omop$drug_exposure %>% db_compute_count( drug_exposure_start_date ) %>% 
      collect() %>% select( date = drug_exposure_start_date, count = `n()`) %>% mutate(type = 'drug')
  ) %>%
  union(
    omop$note %>% db_compute_count( note_date ) %>% 
      collect() %>% select( date = note_date, count = `n()`) %>% mutate(type = 'note')
  ) %>%
  union(
    omop$measurement %>% db_compute_count( measurement_date ) %>% 
      collect() %>% select( date = measurement_date, count = `n()`) %>% mutate(type = 'measurement')
  ) %>%
  union(
    omop$observation %>% db_compute_count( observation_date ) %>% 
      collect() %>% select( date = observation_date, count = `n()`) %>% mutate(type = 'observation')
  )

# https://www.ryanopel.com/post/dplyr-zoo-rollapply/
ggplot(tmp %>% filter(date > '2000-01-01'), aes(x=date, y=count, colour=type))+
  geom_point(alpha=0.4)+
  lims(x=c(as.Date('2009-01-01'),as.Date('2019-01-01')),y=c(0,6000))

savePubBig("dataPointsOverTime",print_aspect_ratio = 2)

# ---- summary of various observations by patient ----

counts <-
    omop$visit_occurrence %>% count() %>% mutate(table="Visits") %>% 
    union(omop$condition_occurrence %>% count() %>% mutate(table="Condition codes")) %>% 
    union(omop$procedure_occurrence %>% count() %>% mutate(table="Procedures")) %>% 
    union(omop$drug_exposure %>% count() %>% mutate(table="Medication orders")) %>% 
    union(omop$note %>% count() %>% mutate(table="Clinical note metadata")) %>% 
    union(omop$note_nlp %>% count() %>% mutate(table="NLP derived codes")) %>% 
    union(omop$measurement %>% count() %>% mutate(table="Measurements")) %>% 
    union(omop$observation %>% count() %>% mutate(table="Observations")) %>% collect();

distincts <-
  omop$visit_occurrence %>% distinct(visit_concept_id) %>% count() %>% mutate(table="Visits") %>% 
  union(omop$condition_occurrence %>% distinct(condition_concept_id) %>% count() %>% mutate(table="Condition codes")) %>% 
  union(omop$procedure_occurrence %>% distinct(procedure_concept_id) %>% count() %>% mutate(table="Procedures")) %>% 
  union(omop$drug_exposure %>% distinct(drug_concept_id) %>% count() %>% mutate(table="Medication orders")) %>% 
  union(omop$note %>% distinct(note_class_concept_id) %>% count() %>% mutate(table="Clinical note metadata")) %>% 
  union(omop$note_nlp %>% distinct(note_nlp_concept_id) %>% count() %>% mutate(table="NLP derived codes")) %>% 
  union(omop$measurement %>% distinct(measurement_concept_id) %>% count() %>% mutate(table="Measurements")) %>% 
  union(omop$observation %>% distinct(observation_concept_id) %>% count() %>% mutate(table="Observations")) %>% collect();

people = omop$person %>% count() %>% collect() %>% .$n
counts <- counts %>% mutate(per_patient = n/people) %>% inner_join(distincts %>% rename(dist=n));
counts %>% select("Data type"=table,"Total"=n,"Per patient"=per_patient, "Distinct types"=dist) %>% saveTable("dataPoints")

# ---- los by visit start time density map - doesn't work as los is integer days
# omop$visit %>% 
#   mutate( 
#     log_los = log(datediff(minute,visit_start_datetime,visit_end_datetime)+1),
#     from_week_start = datediff(minute,dateadd(week, datediff(week, '19050101', visit_start_datetime), '19050101'),visit_start_datetime)
#   ) %>% 
#   filter( visit_concept_id != 9202) %>%
#   db_compute_raster2(from_week_start,log_los) %>%
#   collect() %>%
#   ggplot() +
#   geom_rect(aes(xmin=from_week_start, xmax=from_week_start_2,ymin=log_los,ymax=log_los_2, fill=`n()`))
# 
# omop$visit %>% 
#   mutate( 
#     los = datediff(minute,visit_start_datetime,visit_end_datetime),
#     from_week_start = datediff(minute,dateadd(week, datediff(week, '19050101', visit_start_datetime), '19050101'),visit_start_datetime)
#   ) %>% 
#   filter( visit_concept_id != 9202) %>%
#   filter( los < 30*24*60) %>%
#   mutate( log_los = log(los+1)) %>%
#   db_compute_raster2(from_week_start,log_los) %>%
#   collect() %>%
#   ggplot() +
#   geom_rect(aes(xmin=from_week_start, xmax=from_week_start_2,ymin=log_los,ymax=log_los_2, fill=`n()`))


# ---- notes by type over time ----
omop$note %>% group_by(note_type_concept_id) %>% summarise(count = n()) %>%
  inner_join(omop$concept_ancestor, by=c("note_type_concept_id"="descendant_concept_id")) %>%
  inner_join(omop$concept, by=c("ancestor_concept_id"="concept_id")) %>%
  group_by(concept_name) %>%
  summarise(count = sum(count)) %>%
  select("Note type"=concept_name, "Count" = count) %>%
  collect() %>%
  arrange(desc(Count)) %>%
  saveTable("noteTypeCounts")
  
  
# ---- notes by class over time ----
omop$note %>% group_by(note_class_concept_id) %>% summarise(count = n()) %>%
  # inner_join(omop$concept_ancestor, by=c("note_class_concept_id"="descendant_concept_id")) %>%
  # inner_join(omop$concept, by=c("ancestor_concept_id"="concept_id")) %>%
  inner_join(omop$concept, by=c("note_class_concept_id"="concept_id")) %>%
  group_by(concept_name, note_class_concept_id) %>%
  summarise(count = sum(count)) %>%
  select(
    "Note class"=concept_name, 
    # "Concept id"=note_class_concept_id,
    "Count" = count) %>%
  collect() %>%
  arrange(desc(Count)) %>%
  saveTable("noteClassCounts")

tmp <- omop$note %>% 
  inner_join(omop$concept, by=c("note_class_concept_id"="concept_id")) %>%
  mutate(concept_name=if_else(
    note_class_concept_id == 44803912 |
      note_class_concept_id == 36716202 |
      note_class_concept_id == 37395601,
    concept_name,
    "Clinical letter"
  )) %>% group_by(concept_name,note_date) %>%
  summarise(Count = n()) %>%
  select(Date = note_date, Type = concept_name, Count) %>%
  collect() %>%
  ungroup()

ggplot(tmp, aes(x=Date, y=Count, colour=Type))+
  geom_point(alpha=0.4)+
  lims(x=c(as.Date('2009-01-01'),as.Date('2019-01-01')),y=c(0,3000))

savePubBig("noteClassesOverTime",print_aspect_ratio = 2)

# ---- diagnoses ----

omop$note %>% group_by(note_title) %>% summarise(count = n())
omop$note %>% group_by(note_source_value) %>% summarise(count = n())