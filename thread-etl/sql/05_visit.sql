/****** Script for SelectTopNRows command from SSMS  ******/
DELETE FROM omop.dbo.visit_occurrence;

DECLARE @visitMap TABLE (
[Encounter Type] VARCHAR(255),
[Location Id] VARCHAR(255),
visit_concept_id INT,
INDEX X_primary UNIQUE([Encounter Type],[Location Id])
)

INSERT INTO  @visitMap
SELECT 
LEFT(sourceId,(CHARINDEX('|',sourceId)-1)) AS [Encounter Type],
RIGHT(sourceId,LEN(sourceId)-CHARINDEX('|',sourceId)) AS [Location id],
omopConceptId as visit_concept_id
FROM omopBuild.dbo.ConceptMapping
WHERE sourceDomain LIKE 'urn:trinetx:tbl-trinetx-encounter:Location-id'
;

-- Should be no results
SELECT [Encounter Type], [Location id], COUNT(visit_concept_id) FROM 
@visitMap
GROUP BY [Encounter Type], [Location id]
HAVING COUNT(visit_concept_id)>1


-- TODO: Get better temporal resolution - direct from source system
-- TODO: Provider and care site ids
-- TODO: admitted from and discharged to status
INSERT INTO omop.dbo.visit_occurrence
SELECT 
	--TOP (1000) 
      omopBuild.dbo.getId(CONVERT(INT,[Encounter id]),'tENC') as visit_occurence_id
	  ,CONVERT(BIGINT,groupId) as person_id
	  ,visit_concept_id
	  ,CONVERT(DATE,[Encounter Start Date]+dateOffset) as visit_start_date
	  ,CONVERT(DATETIME2,[Encounter Start Date]+dateOffset) as visit_start_datetime
	  ,CONVERT(DATE,[Encounter End Date]+dateOffset) as visit_end_date
	  ,CONVERT(DATETIME2,[Encounter End Date]+dateOffset) as visit_end_datetime
	  ,32035 as visit_type_concept_id --Visit derived from EHR encounter record
      ,NULL as provider_id -- this is responsible clinician and not the same as [Provider id]
      ,NULL as care_site_id -- need some form of location mapping - [Location id]
      ,NULL as visit_source_value
	  ,0 as visit_source_concept_id
	  ,0 as admitted_from_concept_id --unknown value
	  ,NULL as admitted_from_source_value --unknown value
	  ,NULL as discharge_to_source_value --unknown value
	  ,0 as discharge_to_concept_id --unknown value
	  ,NULL as preceding_visit_occurrence_id
  FROM 
	(
	SELECT e.[Encounter id], 
		CONVERT(DATETIME,[Encounter Start Date],112) as [Encounter Start Date],
		CONVERT(DATETIME,[Encounter End Date],112) as [Encounter End Date],
		tl.groupId,
		sp.dateOffset,
		m.visit_concept_id,
		ROW_NUMBER() OVER(PARTITION BY e.[Encounter id] ORDER BY [Encounter Start Date]) as row
	FROM TriNetX.dbo.tblTriNetXEncounter e
	inner join @visitMap m ON
		e.[Encounter Type] collate Latin1_General_CI_AS = m.[Encounter Type] AND
		e.[Location id] collate Latin1_General_CI_AS = m.[Location id]
	inner join omopBuild.dbo.TrinetxLookup tl 
		ON e.[Patient id] collate Latin1_General_CI_AS = tl.[Patient id]
	inner join omopBuild.dbo.StudyPopulation sp
		ON tl.groupId = sp.groupId
	) e
	WHERE
		e.row=1
;

--TODO: Handling of death: In the case when a patient died during admission 
--(VISIT_OCCURRENCE.DISCHARGE_TO_CONCEPT_ID = 4216643 'Patient died'), 
--a record in the Observation table should be created with 
--OBSERVATION_TYPE_CONCEPT_ID = 44818516 (EHR discharge status 'Expired').

SELECT * FROM 
	omop.dbo.visit_occurrence v LEFT JOIN omop.dbo.person p on v.person_id = p.person_id
	where 
	v.visit_end_datetime IS NOT NULL
	AND ABS(DATEDIFF(day,p.death_datetime,v.visit_end_datetime)) < 2

--TODO: populate preceding_visit_occurrence_id based on date

--TODO: admitted from could be populated if episode in ED immediately predeeding inpatient
--(Vocab is CMS Place of Service)
-- concept_id	concept_name
-- 8870	Emergency Room - Hospital
-- 8536	Home