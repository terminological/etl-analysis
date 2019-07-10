
-------------------------------------------
-- MICROBIOLOGY TESTS value sets
-- Typically microbiology results are short and follow a set proforma.
-- This maybe amenable to mapping
-------------------------------------------

DROP TABLE IF EXISTS #tmpMicroResult

CREATE TABLE #tmpMicroResult (
	value VARCHAR(255)
)

INSERT INTO #tmpMicroResult
SELECT 
  LEFT(CONVERT(VARCHAR(MAX),textual_result),255) as value
  FROM [ordercomms_review].[dbo].[rtest] rt,
  [ordercomms_review].[dbo].[report] r
  WHERE 
  r.report_id = rt.report_id
  AND discipline_id = 3
  AND DATALENGTH(textual_result) > 0


CREATE INDEX X_value on #tmpMicroResult (value)


-- ---------------------------------------------------
--SELECT value, COUNT(*) as c FROM
--	#tmpMicroResult
--GROUP BY value
--ORDER BY c desc


-- ---------------------------------------------------

DROP TABLE IF EXISTS #tmpCounts
SELECT 
	value as local_id
    ,value as search_term
	,COUNT(*) as occurrence
INTO #tmpCounts
FROM #tmpMicroResult
WHERE value <> ''
GROUP BY value
;

SELECT * FROM #tmpCounts order by occurrence desc

DROP TABLE IF EXISTS #tmpIndexInput
SELECT 
	concept_id, concept_name 
INTO #tmpIndexInput
FROM [omop].[dbo].[concept]
WHERE 
standard_concept = 'S' -- TODO: Raise fact that organism is not measurement value
AND concept_class_id = 'Organism' 

INSERT INTO #tmpIndexInput
SELECT 
	concept_id, concept_name 
FROM [omop].[dbo].[concept]
WHERE 
standard_concept = 'S'
AND domain_id = 'Meas Value'
AND concept_class_id = 'Qualifier Value'
;

USE omopBuild;

SELECT Count(*) FROM #tmpIndexInput

-- DECLARE @conceptIndex as dbo.SearchIndexType
-- INSERT INTO @conceptIndex

DECLARE @indexInput AS dbo.IndexInputType;
INSERT INTO @indexInput SELECT * FROM #tmpIndexInput;
EXEC dbo.fn4gramIndex 2, @indexInput

SELECT COUNT(*) FROM SearchIndex where ngram_length < 1

-- FROM @conceptIndex
-- CREATE INDEX X_concept_id ON #tmpConceptIndex (concept_id)
-- CREATE INDEX X_ngram ON #tmpConceptIndex (ngram)

DECLARE @bulkSearchInput AS dbo.BulkSearchInputType
INSERT INTO @bulkSearchInput
SELECT local_id,search_term FROM #tmpCounts

DROP TABLE IF EXISTS #tmpBulkSearchOutput
SELECT * 
INTO #tmpBulkSearchOutput
FROM dbo.fnBulkSearch4Gram(
	@bulkSearchInput,
	2,
	0.1,
	5)

SELECT t.local_id,search_term,0 as review,similarity,b.concept_id,concept_name,occurrence FROM
	#tmpCounts t 
	LEFT JOIN #tmpBulkSearchOutput b ON b.local_id = t.local_id COLLATE Latin1_General_CI_AS
	LEFT JOIN #tmpIndexInput i ON b.concept_id = i.concept_id
	ORDER BY occurrence DESC, b.local_id, similarity DESC


SELECT * FROM tempdb..sysobjects WHERE name LIKE '%AllClasses%'