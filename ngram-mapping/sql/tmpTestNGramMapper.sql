--use omop

--select relationship_id, count(*) as num from concept_relationship
--group by relationship_id order by num desc


--select c1.vocabulary_id as source, c2.domain_id as target, count(*) as mappings from concept_relationship r, concept c1, concept c2
--where relationship_id like 'Maps to' and r.concept_id_1 = c1.concept_id and r.concept_id_2 = c2.concept_id
--group by c1.vocabulary_id, c2.domain_id order by mappings desc


--select c1.*, r.relationship_id, c2.* from concept_relationship r, concept c1, concept c2
--where r.concept_id_1 = c1.concept_id and r.concept_id_2 = c2.concept_id
--and c1.vocabulary_id = 'HES Specialty'

--select c1.vocabulary_id, count(*) as number from concept c1 group by c1.vocabulary_id order by number desc

use omopBuild



-- FIND ancestor codes


DECLARE @indexInput2 AS dbo.IndexInputType;
INSERT INTO @indexInput2 
select DISTINCT c.concept_id,cs.concept_synonym_name from omop.dbo.concept c 
LEFT JOIN omop.dbo.concept_synonym cs ON c.concept_id = cs.concept_id
LEFT JOIN omop.dbo.concept_ancestor ca on c.concept_id = ca.descendant_concept_id
where domain_id like 'Procedure' and standard_concept='S' 
and ancestor_concept_id = '4043018' -- Procedure on digestive systems


--select c1.domain_id, count(*) as number from concept c1 group by c1.domain_id order by number desc


-- #############################################

use omopBuild

--DECLARE @indexInput2 AS dbo.IndexInputType;
--INSERT INTO @indexInput2 
--SELECT c.concept_id, c.concept_name FROM 
--[omop].[dbo].[concept] c
--where c.domain_id = 'Procedure'
--AND standard_concept = 'S'

EXEC dbo.fn4gramIndex 4, @indexInput2

--DECLARE @bulkSearchInput2 AS dbo.BulkSearchInputType
--INSERT INTO @bulkSearchInput2
--SELECT c.concept_id, c.concept_name FROM 
--[omop].[dbo].[concept] c
--where c.vocabulary_id = 'OPCS4'

DECLARE @bulkSearchInput2 AS dbo.BulkSearchInputType
INSERT INTO @bulkSearchInput2
select 
-- TOP(10)
DISTINCT 
c.concept_id, cs.concept_synonym_name From omop.dbo.concept c, omop.dbo.concept_synonym cs 
where c.vocabulary_id='OPCS4' and (c.concept_code like 'H%' OR c.concept_code like 'G%' OR c.concept_code like 'J%')
and c.concept_id = cs.concept_id

DROP TABLE IF EXISTS #tmpBulkSearchOutput2
SELECT * 
INTO #tmpBulkSearchOutput2
FROM dbo.fnBulkSearch4Gram(
	@bulkSearchInput2,
	4,0.1,20)

select * from  #tmpBulkSearchOutput2

--select min(outMap.concept_id) as concept_id, outMap.concept_name, count(*) as items FROM 
--	@bulkSearchInput2 inMap
--	LEFT OUTER JOIN omop.dbo.concept_relationship cr ON inMap.local_id = convert(varchar,cr.concept_id_1) and cr.relationship_id like 'Maps to'
--	LEFT OUTER JOIN omop.dbo.concept_ancestor ca on cr.concept_id_2=ca.descendant_concept_id
--	LEFT OUTER JOIN omop.dbo.concept outMap ON ca.ancestor_concept_id = outMap.concept_id 
--	group by outMap.concept_name order by items desc

SELECT * FROM #tmpBulkSearchOutput2 where similarity IS NULL

--SELECT * FROM
--[omop].[dbo].[concept] s INNER JOIN
--#tmpBulkSearchOutput2 m ON s.concept_id = m.local_id
--INNER JOIN [omop].[dbo].[concept] c on c.concept_id = m.concept_id
--order by s.concept_code ASC, similarity DESC




--

USE omopBuild




select ROW_NUMBER() OVER (ORDER BY c.concept_id,cs.concept_synonym_name) as description_id, c.concept_id,cs.concept_synonym_name
INTO RobsDatabase.dbo.mappingIndexInput
from omop.dbo.concept c 
LEFT JOIN omop.dbo.concept_synonym cs ON c.concept_id = cs.concept_id
LEFT JOIN omop.dbo.concept_ancestor ca on c.concept_id = ca.descendant_concept_id
where domain_id like 'Procedure' and standard_concept='S' 
and ancestor_concept_id = '4043018' -- Procedure on digestive systems


select 
-- TOP(10)
DISTINCT 
ROW_NUMBER() OVER(ORDER BY c.concept_id, cs.concept_synonym_name) as description_id, c.concept_id, cs.concept_synonym_name
INTO RobsDatabase.dbo.mappingSearchInput
From omop.dbo.concept c, omop.dbo.concept_synonym cs 
where c.vocabulary_id='OPCS4' and (c.concept_code like 'H%' OR c.concept_code like 'G%' OR c.concept_code like 'J%')
and c.concept_id = cs.concept_id



select c.concept_id, count(distinct concept_synonym_name) from  RobsDatabase.dbo.mappingSearchInput c group by c.concept_id having count(distinct concept_synonym_name)>1

DROP TABLE IF EXISTS RobsDatabase.dbo.mappingGoldStandardOPCS4
GO

SELECT *
INTO
	RobsDatabase.dbo.mappingGoldStandardOPCS4
FROM
( 
	SELECT DISTINCT
		s.concept_id as source_concept_id,
		s.description_id as source_description_id, 
		s.concept_synonym_name as source_description_name, 
		c.concept_id as target_concept_id,
		c.description_id as target_description_id, 
		c.concept_synonym_name as target_description_name
	FROM
		RobsDatabase.dbo.mappingSearchInput s 
		LEFT OUTER JOIN [omop].[dbo].[concept_relationship] r ON (r.concept_id_1 = s.concept_id  and r.relationship_id like 'Maps to')
		LEFT OUTER JOIN RobsDatabase.dbo.mappingIndexInput c on (r.concept_id_2 = c.concept_id)
) t

-- -----------------------------------------

use omopBuild

DECLARE @indexInput2 AS dbo.IndexInputType;
INSERT INTO @indexInput2 
SELECT description_id,concept_synonym_name FROM RobsDatabase.dbo.mappingIndexInput

EXEC dbo.fn4gramIndex 4, @indexInput2

-- -----------------------------------------

DECLARE @bulkSearchInput2 AS dbo.BulkSearchInputType
INSERT INTO @bulkSearchInput2
SELECT description_id,concept_synonym_name FROM RobsDatabase.dbo.mappingSearchInput

DROP TABLE IF EXISTS #tmpBulkSearchOutput2
SELECT * 
INTO #tmpBulkSearchOutput2
FROM dbo.fnBulkSearch4Gram(
	@bulkSearchInput2,
	4,0.1,20)

DROP TABLE IF EXISTS RobsDatabase.dbo.mappingTestDetailOPCS4

SELECT
	s.description_id as target_desc_id,
	s.concept_id as target_concept_id,
	s.concept_synonym_name as target_term,
	c.description_id as source_desc_id,
	c.concept_id as source_concept_id,
	c.concept_synonym_name as source_term,
	m.similarity
INTO
 RobsDatabase.dbo.mappingTestDetailOPCS4
FROM 
#tmpBulkSearchOutput2 m 
INNER JOIN RobsDatabase.dbo.mappingSearchInput c on (CONVERT(varchar,c.description_id)) = m.local_id
INNER JOIN RobsDatabase.dbo.mappingIndexInput s on s.description_id = m.concept_id

-- ------------------------------------------


Use RobsDatabase
GO

DROP VIEW IF EXISTS mappingComparison
GO

CREATE VIEW mappingComparison AS
SELECT 
	COALESCE(m.[source_desc_id],g.[source_description_id]) as source_description_id
	,COALESCE(m.[source_concept_id],g.[source_concept_id]) as source_concept_id
      ,COALESCE(m.[source_term],g.[source_description_name]) as source_description_name
      ,m.[target_concept_id]
	  ,m.target_desc_id
      ,m.[target_term]
      ,m.[similarity] as similarity
	  ,m.[rankOrder] as rankOrder
	  ,g.target_concept_id as mapped_concept_id
	  ,g.[target_description_id] as mapped_description_id
      ,g.[target_description_name] as mapped_description_name
  FROM 
  (SELECT * , ROW_NUMBER() OVER(PARTITION BY source_concept_id ORDER BY similarity DESC) as rankOrder FROM [dbo].[mappingTestDetailOPCS4]) m
  LEFT OUTER JOIN
  [dbo].[mappingGoldStandardOPCS4] g ON (m.[source_concept_id]=g.[source_concept_id] and m.[target_desc_id]=g.[target_description_id])
GO

Select * from mappingComparison order by source_concept_id, rankOrder

Select * FROM mappingComparison where source_description_name like 'Specified leg%' order by source_concept_id, rankOrder desc



