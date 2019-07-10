
-- ----------------------------------------------------
-- get mappings for OPCS4 procedure codes
-- These will be mapped into the procedure occcurence table
-- as main operations

DROP TABLE IF EXISTS #tmpConditionMap

CREATE TABLE #tmpConditionMap (
[Diagnosis Code System] VARCHAR(255) NOT NULL,
condition_source_value VARCHAR(50) NOT NULL,
domain_id VARCHAR(50) NOT NULL, 
condition_concept_id INT NOT NULL,
condition_source_concept_id INT NOT NULL,
INDEX X_condition_code ([Diagnosis Code System],condition_source_value))


-- Get source procedure maps from the concept vocabularies for 
INSERT INTO #tmpConditionMap
  SELECT 
	used.[Diagnosis Code System],
	used.code as condition_source_value,
	c.domain_id,
	IIF(c.standard_concept='S',c.concept_id,ISNULL(r.concept_id_2,0)) as condition_concept_id,
	c.concept_id as condition_source_concept_id
  FROM
  (
	  SELECT
		  [Diagnosis Code System],
		  [Diagnosis Code],
		  UPPER([Diagnosis Code System]) as codeSystem
		  ,[Diagnosis Code] as code
		  ,COUNT(*) AS number
	  FROM [TriNetX].[dbo].[tblTriNetXDIAGNOSIS] 
	  GROUP BY [Diagnosis Code System],[Diagnosis Code]
	) used 
		INNER JOIN 
			[omop].[dbo].[concept] c ON 
			used.codeSystem collate Latin1_General_CI_AS = c.vocabulary_id AND 
			used.code collate Latin1_General_CI_AS = REPLACE(c.concept_code,'.','')
		LEFT JOIN 
			[omop].[dbo].[concept_relationship] r 
			ON c.concept_id = r.concept_id_1 AND r.relationship_id = 'Maps to'
	WHERE c.domain_id is NOT NULL;
-- ----------------------------------------------------
-- There are some ICD10 codes not recognised by above
-- They are all ones which have an extra digit on the end 
-- link J9691 which should be J96.9 respiratory failure not specified
--  or I10X which shoudl be I10 essential hypertension

-- NB this is super slow for some reason.
INSERT INTO #tmpConditionMap
  SELECT
	'ICD10' as [Diagnosis Code System],
	used.[Diagnosis Code] as condition_source_value,
	c2.domain_id,
	IIF(c2.standard_concept='S',c2.concept_id,ISNULL(r.concept_id_2,0)) as condition_concept_id,
	c2.concept_id as condition_source_concept_id
  FROM
	(
		SELECT 
			LEFT([Diagnosis Code],LEN([Diagnosis Code])-1) as code -- shortened by 1 char
			,*
		FROM (  
			  SELECT
				  [Diagnosis Code],
				  COUNT(*) AS number
			  FROM [TriNetX].[dbo].[tblTriNetXDIAGNOSIS]
			  WHERE [Diagnosis Code System]='ICD10'
			  GROUP BY [Diagnosis Code]
		) z
	) used 
		LEFT JOIN 
			#tmpConditionMap m ON 
			'ICD10' = m.[Diagnosis Code System] 
			AND used.[Diagnosis Code] collate Latin1_General_CI_AS = m.condition_source_value
		INNER JOIN 
			[omop].[dbo].[concept] c2 ON 
			'ICD10' = c2.vocabulary_id AND 
			used.code collate Latin1_General_CI_AS = REPLACE(c2.concept_code,'.','')
		LEFT JOIN 
			[omop].[dbo].[concept_relationship] r 
			ON c2.concept_id = r.concept_id_1 AND r.relationship_id = 'Maps to'
	WHERE 
		m.condition_source_value IS NULL -- exclude those we have already mapped
;

INSERT INTO #tmpConditionMap
SELECT DISTINCT
	sourceDomain,
	sourceId,
	omopDomainId,
	omopConceptId,
	0 as sourceConceptId
FROM 
	omopBuild.dbo.ConceptMapping
WHERE 
	sourceDomain in (
		'urn:code-system:SNOMED',
		'urn:code-system:ICD10'
	)
;

-- -----------------------------------------
-- Unmatched entries - used concepts in the Diagnosis table that
-- have no omop mapping, presented in a format that can be pasted onto the 
-- end of the vocab mapping file or appended to the concept mapping file:
-- INSERT INTO omopBuild.dbo.ConceptMapping
SELECT
	used.[Diagnosis Code System] as sourceDomain,
	used.[Diagnosis Code] as sourceId,
	used.[Diagnosis Description] as sourceTerm,
	0 as certainty,
	0 as reviewStatus,
	'Condition' as omopDomainId,
	0 as omopConceptId,
	'Unknown' as omopConceptName,
	number as usedCount
FROM (
	SELECT
		[Diagnosis Code System],
		[Diagnosis Code],
		[Diagnosis Description],
		COUNT(*) AS number
	FROM [TriNetX].[dbo].[tblTriNetXDIAGNOSIS] 
	  GROUP BY [Diagnosis Code System],[Diagnosis Code],[Diagnosis Description]
	) used 
		LEFT JOIN 
			#tmpConditionMap m ON 
			used.[Diagnosis Code System] collate Latin1_General_CI_AS = m.[Diagnosis Code System] 
			AND used.[Diagnosis Code] collate Latin1_General_CI_AS = m.condition_source_value
WHERE 
	m.condition_concept_id IS NULL
	
-- -----------------------------------------
USE [TriNetX]
GO
-- get rid of duplicates due to providers and assign identity based on original row order
DROP TABLE IF EXISTS #tmpDiagnosis;

SELECT 
	-- TOP(100)
	original_row
	,[Patient id]
    ,[Encounter id]
    ,[Provider id]
    ,[Diagnosis Code System]
    ,[Diagnosis Code]
    ,[Diagnosis Description]
    ,[Diagnosis Date]
    ,[Principal Diagnosis Indicator]
    ,[Diagnosis Source]
INTO #tmpDiagnosis
FROM (
	SELECT *
	  ,ROW_NUMBER() OVER(PARTITION BY 
			p.[Patient id],
			p.[Encounter id],
			p.[Diagnosis Date],
			p.[Diagnosis Code] ORDER BY original_row) as provider
	FROM 
		(SELECT *,
		ROW_NUMBER() OVER(ORDER BY (SELECT NULL)) as original_row
		FROM [dbo].[tblTriNetXDIAGNOSIS]) p
	) q
WHERE provider = 1
GO




-- -----------------------------------------
DELETE FROM omop.dbo.condition_occurrence

INSERT INTO omop.dbo.condition_occurrence
SELECT
	-- TOP(100)
	omopBuild.dbo.getId(original_row,'tDIA') as condition_occurrence_id,
	s.groupId as person_id,
	cm.condition_concept_id,
	CONVERT(DATE,(CONVERT(DATETIME,[Diagnosis Date],112)+s.dateOffset)) as condition_start_date,
	CONVERT(DATETIME2,(CONVERT(DATETIME,[Diagnosis Date],112)+s.dateOffset)) as condition_start_datetime,
	NULL as condition_end_date,
	NULL as condition_end_datetime,
	32020 as condition_type_concept_id,	--EHR encounter diagnosis
	4230359 as condition_status_concept_id, --Final diagnosis
	NULL as stop_reason,
	NULL as provider_id,
	omopBuild.dbo.getId(CONVERT(INT,[Encounter id]),'tENC') as visit_occurrence_id,
	NULL as visit_detail_id,
	condition_source_value,
	condition_source_concept_id,
	[Diagnosis source] as condition_status_source_value
FROM #tmpDiagnosis p
	INNER JOIN omopBuild.dbo.TrinetxLookup l on l.[Patient id]=p.[Patient id] COLLATE Latin1_General_CI_AS
	INNER JOIN omopBuild.dbo.StudyPopulation s on s.groupId = l.groupId
	LEFT JOIN #tmpConditionMap cm 
		on cm.[Diagnosis Code System] = p.[Diagnosis Code System] collate Latin1_General_CI_AS
		and cm.condition_source_value = p.[Diagnosis code] collate Latin1_General_CI_AS
		where cm.domain_id = 'Condition'

--Presently, there is no designated vocabulary, domain, or class that represents condition status. The following concepts from SNOMED are recommended:
--Admitting diagnosis: 4203942
--Final diagnosis: 4230359 (should also be used for discharge diagnosis
--Preliminary diagnosis: 4033240

-- -----------------------------------------
-- 
INSERT INTO omop.dbo.observation
SELECT 
-- TOP(100)
	omopBuild.dbo.getId(original_row,'tDIA') as condition_occurrence_id,
	s.groupId as person_id,
	cm.condition_concept_id as observation_concept_id,
	CONVERT(DATE,(CONVERT(DATETIME,[Diagnosis Date],112)+s.dateOffset)) as observation_date,
	CONVERT(DATETIME2,(CONVERT(DATETIME,[Diagnosis Date],112)+s.dateOffset)) as observation_datetime,
	38000280 as observation_type_concept_id,	--	Observation recorded from EHR
	NULL as value_as_number,
	NULL as value_as_string,
	4253628 as value_as_concept_id,	-- Known present
	NULL as qualifier_concept_id,
	NULL as unit_concept_id,
	NULL as provider_id,
	omopBuild.dbo.getId(CONVERT(INT,[Encounter id]),'tENC') as visit_occurrence_id,
	NULL as visit_detail_id,
	condition_source_value as observation_source_value,
	condition_source_concept_id as observation_source_concept_id,
	NULL as unit_source_value,
	NULL as qualifier_source_value,
	NULL as observation_event_id,
	0 as obs_event_field_concept_id,
	NULL as value_as_datetime
FROM #tmpDiagnosis p
	INNER JOIN omopBuild.dbo.TrinetxLookup l on l.[Patient id]=p.[Patient id] COLLATE Latin1_General_CI_AS
	INNER JOIN omopBuild.dbo.StudyPopulation s on s.groupId = l.groupId
	LEFT JOIN #tmpConditionMap cm 
		on cm.[Diagnosis Code System] = p.[Diagnosis Code System] collate Latin1_General_CI_AS
		and cm.condition_source_value = p.[Diagnosis code] collate Latin1_General_CI_AS
		where cm.domain_id <> 'Condition'

-- SELECT COUNT(*) FROM omop.dbo.observation
-- SELECT COUNT(*) FROM omop.dbo.condition_occurrence

DROP TABLE IF EXISTS #tmpConditionMap
DROP TABLE IF EXISTS #tmpDiagnosis