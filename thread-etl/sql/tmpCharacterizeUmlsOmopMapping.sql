SELECT DISTINCT SAB FROM UmlsMRCONSO

SELECT count(DISTINCT u.CODE) FROM UmlsMRCONSO u, omop.dbo.concept c where u.SAB='SNOMEDCT_US' AND c.vocabulary_id='SNOMED' AND u.CODE = c.concept_code AND standard_concept = 'S'
SELECT count(DISTINCT c.concept_id) FROM UmlsMRCONSO u, omop.dbo.concept c where u.SAB='SNOMEDCT_US' AND c.vocabulary_id='SNOMED' AND u.CODE = c.concept_code AND standard_concept = 'S'
-- 432033
-- 295449 standard omop concepts

SELECT count(DISTINCT CODE) FROM  UmlsMRCONSO where SAB='SNOMEDCT_US'
-- 432811

SELECT count(DISTINCT concept_code) FROM  omop.dbo.concept where vocabulary_id='SNOMED' AND standard_concept = 'S'
-- 861732
-- 529381 standard concept

SELECT TOP(100) c.* FROM omop.dbo.concept c LEFT OUTER JOIN  UmlsMRCONSO u ON u.SAB='SNOMEDCT_US' AND c.vocabulary_id='SNOMED' AND u.CODE = c.concept_code WHERE u.CODE IS NULL AND c.vocabulary_id='SNOMED'

SELECT u.* FROM UmlsMRCONSO u LEFT OUTER JOIN omop.dbo.concept c ON u.SAB='SNOMEDCT_US' AND c.vocabulary_id='SNOMED' AND u.CODE = c.concept_code WHERE c.concept_code IS NULL AND u.SAB='SNOMEDCT_US'

select count(*) from (
SELECT distinct u.CUI --, STRING_AGG(c.concept_name,'|')
--,c.*,c.concept_id,c.concept_name 
FROM UmlsMRCONSO u, omop.dbo.concept c 
where u.SAB='SNOMEDCT_US' AND c.vocabulary_id='SNOMED' AND u.CODE = c.concept_code AND standard_concept = 'S'
GROUP BY u.CUI HAVING COUNT(*)>2
) tmp
-- 143138 concepts with multiple matches (CUI to OMOP standard concept)


select count(*) from (
SELECT distinct c.concept_id --, STRING_AGG(c.concept_name,'|')
--,c.*,c.concept_id,c.concept_name 
FROM UmlsMRCONSO u, omop.dbo.concept c 
where u.SAB='SNOMEDCT_US' AND c.vocabulary_id='SNOMED' AND u.CODE = c.concept_code AND standard_concept = 'S'
GROUP BY c.concept_id HAVING COUNT(*)>2
) tmp
-- 144860 concepts with multiple matches (OMOP standard concept to CUI)

-- mapping stats
select
*,
total_omop-unmapped_omop as mapped_omop,
total_umls-unmapped_umls as mapped_umls
FROM (
	select 

	count(distinct concept_id) as total_omop,
	count(distinct CUI) as total_umls,
	sum(iif(CUI IS NULL,1,0)) as unmapped_omop,
	sum(iif(concept_id IS NULL,1,0)) as unmapped_umls,
	sum(iif(concept_id is not null and CUI is not null,1,0)) as mappings

	from (

		SELECT distinct c.concept_id, u.CUI
		FROM 
			(
			SELECT * FROM UmlsMRCONSO WHERE SAB='SNOMEDCT_US'
			) u
			FULL OUTER JOIN 
			(
			SELECT * FROM omop.dbo.concept WHERE vocabulary_id='SNOMED' AND standard_concept = 'S'
			) c
		ON u.CODE = c.concept_code

	) tmp
) tmp2

-- mapping breakdown
SELECT
	mapping_count,
	count(*) as cuis_count
FROM 
	(
		SELECT sum(iif(concept_id IS NOT NULL,1,0)) as mapping_count FROM 
		(
			SELECT distinct c.concept_id, u.CUI
			FROM 
				(
				SELECT * FROM UmlsMRCONSO WHERE SAB='SNOMEDCT_US'
				) u
				FULL OUTER JOIN 
				(
				SELECT * FROM omop.dbo.concept WHERE vocabulary_id='SNOMED' AND standard_concept = 'S'
				) c
			ON u.CODE = c.concept_code
		) tmp
		WHERE CUI IS NOT NULL
		GROUP BY CUI
	) tmp2
GROUP BY mapping_count

-- mapping breakdown
SELECT
	mapping_count,
	count(*) as omop_concept_count
FROM 
	(
		SELECT sum(iif(CUI IS NOT NULL,1,0)) as mapping_count FROM 
		(
			SELECT distinct c.concept_id, u.CUI
			FROM 
				(
				SELECT * FROM UmlsMRCONSO WHERE SAB='SNOMEDCT_US'
				) u
				FULL OUTER JOIN 
				(
				SELECT * FROM omop.dbo.concept WHERE vocabulary_id='SNOMED' AND standard_concept = 'S'
				) c
			ON u.CODE = c.concept_code
		) tmp
		WHERE concept_id IS NOT NULL
		GROUP BY concept_id
	) tmp2
GROUP BY mapping_count



DROP TABLE IF EXISTS RobsDatabase.dbo.mappingFullUmlsOmopSCT;
GO

CREATE TABLE RobsDatabase.dbo.mappingFullUmlsOmopSCT (
	[CUI] [char](8) NULL,
	[source_concept_id] [int] NULL,
	[concept_id] [int] NULL
) ON [PRIMARY]
GO

INSERT INTO RobsDatabase.dbo.mappingFullUmlsOmopSCT
SELECT CUI, source_concept_id, concept_id FROM (
	SELECT 
		u.CUI,
		c.source_concept_id,
		c.concept_id,
		ROW_NUMBER() OVER(PARTITION BY u.CUI,c.concept_id ORDER BY c.source_concept_id) as uniquifier,
		ROW_NUMBER() OVER(PARTITION BY u.CUI ORDER BY c.concept_id DESC) as filter
	FROM 
		(
			SELECT *, LEFT(SAB,1) as src
			 FROM omopBuild.dbo.UmlsMRCONSO WHERE SAB in ('SNOMEDCT_US','RXNORM') --AND ISPREF='Y'
		) u
		FULL OUTER JOIN 
		(
			SELECT 
				c1.concept_id as source_concept_id, 
				c1.concept_code, c1.concept_name, 
				COALESCE(c2.concept_id,0) as concept_id,
				LEFT(c1.vocabulary_id,1) as src
			FROM
			[omop].[dbo].[concept] c1 LEFT OUTER JOIN
			[omop].[dbo].[concept_relationship] cr ON c1.concept_id = cr.concept_id_1  AND cr.relationship_id='Maps to' LEFT OUTER JOIN
			[omop].[dbo].[concept] c2 ON c2.concept_id = cr.concept_id_2
			WHERE c1.vocabulary_id in ('SNOMED','RxNorm') AND c1.invalid_reason IS NULL
		) c
		ON u.CODE = c.concept_code and u.src = c.src
) x WHERE
uniquifier = 1 and not ( concept_id = 0 and filter > 1 )


USE RobsDatabase
GO

-- CUIS that are not mapped at all in OMOP
-- I.e. the SCT subset and RxNorm subset does not contain any code that matches.
-- Not quite clear what these are - retirements? new concepts?
select * FROM omopBuild.dbo.UmlsMRCONSO c, dbo.mappingFullUmlsOmopSCT m
where m.CUI = c.CUI and
-- concept_id IS NULL
m.CUI = ''
and SAB in ('SNOMEDCT_US','RXNORM')

DROP VIEW IF EXISTS dbo.mappingComparisonUmlsOmopSCT
GO

Create VIEW dbo.mappingComparisonUmlsOmopSCT AS
SELECT
	cui_map,
	omop_map,
	count(*) as mappings_count
FROM 
	dbo.mappingFullUmlsOmopSCT map LEFT JOIN
	(
		SELECT CUI,sum(iif(concept_id IS NOT NULL,1,0)) as cui_map FROM 
		dbo.mappingFullUmlsOmopSCT
		WHERE CUI IS NOT NULL
		GROUP BY CUI
	) cui2omop ON map.CUI = cui2omop.CUI 
	FULL OUTER JOIN
	(
		SELECT concept_id,sum(iif(CUI IS NOT NULL,1,0)) as omop_map FROM 
		dbo.mappingFullUmlsOmopSCT
		WHERE concept_id IS NOT NULL
		GROUP BY concept_id
	) omop2cui ON map.concept_id=omop2cui.concept_id
GROUP BY cui_map,omop_map

select * from dbo.mappingComparisonUmlsOmopSCT order by cui_map, omop_map