DELETE FROM omop.dbo.person;

DECLARE @genderMap TABLE (
source CHAR(1) PRIMARY KEY, 
gender_concept_id INT)

INSERT INTO  @genderMap
SELECT 
CAST(sourceId as CHAR(1)) AS source,
omopConceptId as gender_concept_id
FROM omopBuild.dbo.ConceptMapping
WHERE sourceDomain = 'urn:omop-build:master-index:Gender'
;

-- TODO: generate locations for patient postcodes
-- TODO: generate providers for GPs including GP id & create a temp table mapping GP ids to omop provider_id

INSERT INTO omop.dbo.person
SELECT
	CAST(x.groupId AS BIGINT) AS person_id,
	y.gender_concept_id,
	YEAR(x.dateOfBirth) as year_of_birth,
	MONTH(x.dateOfBirth) as year_of_birth,
	DAY(x.dateOfBirth) as year_of_birth,
	CAST(x.dateOfBirth as DATETIME2) as birth_datetime,
	CAST(x.dateOfDeath as DATETIME2) as death_datetime,
	0 as race_concept_id,
	0 as ethnicity_concept_id,
	NULL as location_id, -- FK to stub location for postcode group
	NULL as provider_id, -- FK to record for GP? - have to update identifier graph to include this.
	NULL as care_site_id,
	NULL as person_source_value,
	NULL as gender_source_value,
	0 as gender_source_concept_id,
	NULL as race_source_value,
	0 as race_source_concept_id,
	NULL as ethnicity_source_value,
	0 as ethnicity_source_concept_id
FROM (
	SELECT 
	 -- TOP(100)
		m.groupId,
		CASE 
		WHEN STRING_AGG(m.gender,'') LIKE '%U%' THEN 'U'
		WHEN STRING_AGG(m.gender,'') LIKE '%M%F%' THEN 'U'
		WHEN STRING_AGG(m.gender,'') LIKE '%F%M%' THEN 'U'
		WHEN STRING_AGG(m.gender,'') LIKE '%M%' THEN 'M'
		WHEN STRING_AGG(m.gender,'') LIKE '%F%' THEN 'F'
		END as gender,
		MIN(m.dateOfBirth + s.dateOffset) as dateOfBirth,
		MIN(m.dateOfDeath + s.dateOffset) as dateOfDeath
		-- ROW_NUMBER() OVER(PARTITION BY m.groupId ORDER BY ISNULL(m.updateDate,'1900-01-01') DESC) as rowNum
	FROM 
		omopBuild.dbo.MasterIndex m
		INNER JOIN omopBuild.dbo.StudyPopulation s ON m.groupId = s.groupId
	GROUP BY m.groupId
) x
LEFT JOIN @genderMap y ON x.gender = y.source
-- WHERE rowNum = 1

-- -------------------------------------------------------
-- TODO: Each Person has to have at least one observation period.
-- OBSERVATION_PERIOD table contains records which uniquely define 
-- the spans of time for which a Person is at-risk to have clinical 
-- events recorded within the source systems, even if no events in 
-- fact are recorded (healthy patient with no healthcare interactions).
-- This will probably be their whole life in our case.
-- -------------------------------------------------------
--concept_id	concept_name
--44814722	Period while enrolled in insurance
--44814723	Period while enrolled in study
--44814724	Period covering healthcare encounters
--44814725	Period inferred by algorithm
--45747656	Pre-qualification time period
--45890994	Period of complete data capture based on geographic isolation