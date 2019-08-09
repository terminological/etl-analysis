library(tidyverse)
library(reshape2)
library(cowplot)
# detach("package:phdUtils", unload=TRUE)
library(phdUtils)
library(dbplot)
# library(boot)
library(zoo)
library(extrafont)

theme_set(themePhd())

# ---- database setup ----
setwd('~/Dropbox/threadAnalysis/summary')
config <- config::get(file="~/Dropbox/db.yaml")
omop <- getOmop("omop",config)

visits <- omop$visit_occurrence %>% summarise(min=min(visit_start_date),max=max(visit_start_date)) %>% collect()

# ---- people by year of birth (figure) and gender (table) ----

genders = data.frame(
  concept_id = c(0,8532,8507),
  concept_name = c("unknown","female","male")
)

totalPeople=(omop$person %>% count() %>% collect())$n[1]
tmp <- omop$person %>% group_by(gender_concept_id) %>% 
  summarise(Count = n()) %>% collect() %>%
  inner_join(genders, by=c("gender_concept_id"="concept_id")) %>% select(Gender=concept_name, Count) %>%
  mutate(`% age`=Count/totalPeople*100) %>%
  union(tibble(Gender=factor("total"),Count=totalPeople,`% age`=100)) %>% 
  saveTable("peopleByGender2",colWidths = c(0.5,0.25,0.25))

omop$person %>% 
  filter(!is.na(year_of_birth) & year_of_birth>1910) %>%
  db_compute_bins(year_of_birth, binwidth = 1) %>% 
  select(year_of_birth,count) %>%
  ggplot(aes(x=year_of_birth,y=count))+
  xlab("Year of birth")+ylab("Count")+
  geom_col(fill="blue",width=0.8)
saveThesisHalfPage("peopleByYob")

# ---- visits by type (table) and over time (figure) ----
# omop$visit_occurrence %>% 
#   inner_join(omop$concept, by=c("visit_concept_id"="concept_id")) %>%
#   dbplot_bar(concept_name) + scale_y_log10()
# saveThesisSixthPage("visitsByType")

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
    "Per patient" = mean(count)
  ) %>% saveTable("visitsByType",colWidths = c(0.5,0.25,0.25))


tmp <- omop$visit_occurrence %>% 
  group_by(visit_concept_id,visit_start_date) %>% 
  summarise(count=n()) %>%
  inner_join(omop$concept, by=c("visit_concept_id"="concept_id")) %>%
  select(visit_start_date,concept_name,count) %>%
  collect()
tmp <- tmp %>% mutate(roll = rollmedian(count,49, fill="extend"))
tmp %>%
  ggplot(aes(x=visit_start_date, y=count,colour=concept_name))+
  geom_point(alpha=0.2,size=1,stroke=0)+
  geom_line(aes(y=roll))+
  xlab("start date")+
  ylab("visits")+labs(colour=NULL)+scale_color_brewer(palette="Set1")+narrowAndTall()
saveThesisThirdPage("visitsByTime")



# ---- Deprecated: non outpatient visits by type - length of stay (int days) and counts ----

outpatient_visit_concept_id <- 9202

# omop$visit %>% 
#   mutate( los = datediff(minute,visit_start_datetime,visit_end_datetime)) %>%
#   filter( visit_concept_id != outpatient_visit_concept_id) %>%
#   group_by( visit_concept_id ) %>%
#   summarise( 
#     count = n(),
#     mean = mean(los)
# ) %>% inner_join(omop$concept, by=c("visit_concept_id"="concept_id"))

# ---- Deprecated: non outpatient visits by time of week - los vs time of week (figure) ----
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
  geom_point(aes(x=from_week_start, y=los))+
  xlab("hours from start of week")+
  ylab("daily average length of stay (minutes)")+narrowAndTall()

saveThesisThirdPage("losOverWeek")

# ---- Maybe deprecated: non outpatient visits by time of week - los vs daily volume (figure) ----
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
  geom_point(aes(x=count, y=los),alpha=0.1)+
  xlab("daily volume of inpatient stays")+
  ylab("daily average length of stay (minutes)")
saveThesisSixthPage("losVsInptVisitsPerDay")

summary(lm(count ~ los,tmp))
# No clear relationship between los and daily counts but again this will be affected by deidentification
# https://www.statmethods.net/advstats/bootstrapping.html could be used if we wanted a confidence interval on Rsq


# ---- counts of various observations by day (figure) ----
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
tmp <- tmp %>% arrange(date) %>% group_by(type) %>% mutate( count_trend = rollmedian(count,49, fill="extend"))


# https://www.ryanopel.com/post/dplyr-zoo-rollapply/
ggplot(tmp %>% filter(date > '2000-01-01'), aes(x=date, y=count, colour=type))+
  annotate("rect", xmin = visits$min, xmax = visits$max, ymin = 5600, ymax = 5800)+
  geom_point(alpha=0.2,stroke=0,size=1)+
  geom_line(aes(y=count_trend))+
  lims(x=c(as.Date('2009-01-01'),as.Date('2019-02-01')),y=c(0,6000))+
  scale_color_brewer(palette="Set3")+narrowAndTall()

saveThesisHalfPage("dataPointsOverTime")

# ---- summary of various observations by patient (table) ----

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
counts %>% select("Data type"=table,"Total"=n,"Per patient"=per_patient, "Distinct types"=dist) %>% 
  saveTable("dataPoints",colWidths = c(0.4,0.2,0.2,0.2))

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


# ---- notes by type summary (table) ----
omop$note %>% group_by(note_type_concept_id) %>% summarise(count = n()) %>%
  inner_join(omop$concept_ancestor, by=c("note_type_concept_id"="descendant_concept_id")) %>%
  inner_join(omop$concept, by=c("ancestor_concept_id"="concept_id")) %>%
  group_by(concept_name) %>%
  summarise(count = sum(count)) %>%
  select("Note type"=concept_name, "Count" = count) %>%
  collect() %>%
  arrange(desc(Count)) %>%
  saveTable("noteTypeCounts")
  
  
# ---- notes by class over time (figure) ----
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
  saveTable("noteClassCounts", colWidths = c(0.8,0.2))

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

tmp <- tmp %>% arrange(Date) %>% group_by(Type) %>% mutate( count_trend = rollmedian(Count,49, fill="extend"))

# https://www.ryanopel.com/post/dplyr-zoo-rollapply/
ggplot(tmp, aes(x=Date, y=Count, colour=Type))+
  annotate("rect", xmin = visits$min, xmax = visits$max, ymin = 2800, ymax = 2900)+
  geom_point(alpha=0.2,stroke=0,size=1)+
  geom_line(aes(y=count_trend))+narrowAndTall()+
  lims(x=c(as.Date('2009-01-01'),as.Date('2019-02-01')),y=c(0,3000))+scale_color_brewer(palette="Set2")+narrowAndTall()

saveThesisHalfPage("noteClassesOverTime")


# ---- diagnoses ----

omop$note %>% group_by(note_title) %>% summarise(count = n()) %>% arrange(desc(count))
omop$note %>% group_by(note_source_value) %>% summarise(count = n()) %>% arrange(desc(count))