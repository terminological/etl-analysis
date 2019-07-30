

--select 500 as analysis_id, 
--	cast(C.condition_concept_id AS varchar(255)) as stratum_1,
--	cast(null as varchar(255)) as stratum_2, 
--	cast(null as varchar(255)) as stratum_3, 
--	cast(null as varchar(255)) as stratum_4, 
--	cast(null as varchar(255)) as stratum_5,
--	count_big(distinct O.person_id) as count_value
---- into #s_tmpach_500
--from omop.dbo.observation O
--join omop.dbo.person P on O.person_id = P.person_id
--  and P.death_datetime = O.observation_datetime
--left join omop.dbo.condition_occurrence C on C.person_id = O.person_id
--  and P.death_datetime = C.condition_start_datetime
--left join omop.dbo.concept CN on C.condition_type_concept_id = CN.concept_id
--  and CN.concept_class_id = 'Death Type'
--where O.observation_concept_id = 4306655 -- death concept id
--group by C.condition_concept_id
--;

-- UPDATE the observation and condition_occurence tables to deal with deaths.

select max(observation_id) as id_base from observation

-- I've done this once. TODO doesn;t check if it needs to be done again,
INSERT INTO omop.dbo.observation (
	observation_id, person_id, observation_concept_id, observation_date, observation_datetime, 
	observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id, 
	qualifier_concept_id, unit_concept_id, provider_id, visit_occurrence_id, visit_detail_id, 
	observation_source_value, observation_source_concept_id, unit_source_value, qualifier_source_value, 
	observation_event_id, obs_event_field_concept_id, value_as_datetime
) 
SELECT 
	tmp.id_base+ROW_NUMBER() OVER(ORDER BY person_id) as observation_id, --TODO: how to generate these in absense of an autoincrement
	person_id,
	4306655 as observation_concept_id,
	convert(date,death_datetime) as observation_date,
	death_datetime as observation_datetime,
	38000280 as observation_type_concept_id, 
	null as value_as_number, 
	null as value_as_string, 
	null as value_as_concept_id, 
	null as qualifier_concept_id, 
	null as unit_concept_id, 
	null as provider_id, 
	null as visit_occurrence_id, 
	null as visit_detail_id, 
	null as observation_source_value,
	0 as observation_source_concept_id, 
	null as unit_source_value, 
	null as qualifier_source_value, 
	null as observation_event_id, 
	0 as obs_event_field_concept_id, 
	death_datetime as value_as_datetime
from person, 
	(select max(observation_id) as id_base from observation) tmp
where death_datetime IS NOT NULL;

select distinct observation_type_concept_id from observation