
-- ---------------------------------
-- Radiology oders
/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [from_code]
      ,MIN([original_display_name])
	  ,COUNT(*) as occurrence
  FROM [ordercomms_review].[dbo].[npisynonym] s INNER JOIN
	[ordercomms_review].[dbo].rnpi r ON s.[npisynonym_id]=r.[npisynonym_id]
  GROUP BY from_code
  ORDER BY occurrence DESC


/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP(100) *
  FROM [ordercomms_review].[dbo].[request]
  WHERE result_date IS NULL or result_time IS NULL
  AND discipline_id = 
	-- 27 -- MRI
	28 -- USS

-- MRI requests are stored in flatfiles.
SELECT TOP(100) *
  FROM [ordercomms_review].[dbo].[request] r
  LEFT OUTER JOIN [ordercomms_review].[dbo].[flatfile] f ON f.flatfile_id = r.flatfile_id
  WHERE result_date IS NOT NULL or result_time IS NOT NULL
  AND discipline_id = 27 -- MRI
  --ORDER BY request_id DESC

-- N requests in rnpi - rnpi is only for radiology - discipline 29
SELECT discipline_id, COUNT(*)
  FROM [ordercomms_review].[dbo].[request] r
  INNER JOIN [ordercomms_review].[dbo].[rnpi] f ON f.request_id = r.request_id
  WHERE result_date IS NOT NULL AND result_time IS NOT NULL
  GROUP BY discipline_id

SELECT TOP(100) *
  FROM [ordercomms_review].[dbo].[request] r
  INNER JOIN [ordercomms_review].[dbo].[rnpi] f ON f.request_id = r.request_id
  --WHERE result_date IS NULL or result_time IS NULL


USE [ordercomms_review]
GO

-- --------------------
-- Lab test disciplines

SELECT 
      d.discipline_id,MIN(d.[name])
      ,COUNT(request_id) as c
  FROM [dbo].[discipline] d LEFT JOIN [dbo].[request] r ON d.discipline_id = r.discipline_id
  GROUP BY d.discipline_id
  HAVING COUNT(request_id)  > 0
  ORDER BY  d.discipline_id
GO

--discipline_id	(No column name)	c
--1	Chem/Haem	25275619
--2	Histopathology	1029961
--3	Microbiology	5090922
--7	Virology	649215
--8	Gynae Cytology	2102127
--10	Non-Gynae Cytology	126963
--12	Downs Screening	23182
--13	Andrology	31672
--14	Blood Transfusion	1002405
--18	Molecular Pathology	487
--20	Unknown	8586
--21	Pathology	1097
--25	X-Ray	448121
--26	CAT	67399
--27	MRI	45456
--28	Ultrasound	174958
--29	Radiology	852686

SELECT TOP (1000) [discipline_id]
      ,[name]
      ,[code]
  FROM [ordercomms_review].[dbo].[discipline]

--histopathology requests
SELECT TOP (1000) *
  FROM [ordercomms_review].[dbo].[rtest] rt
  INNER JOIN [ordercomms_review].[dbo].[request] r ON rt.request_id = r.request_id
  INNER JOIN [ordercomms_review].[dbo].[testSynonym] ts ON rt.testSynonym_id = ts.testSynonym_id
  WHERE --r.discipline_id = 18 --2
  spc_comments IS NOT NULL
  AND amended=0
--  ORDER BY rt.rtest_id DESC


--microbiology requests but includes results
SELECT TOP (1000) *
  FROM [ordercomms_review].[dbo].[rtest] rt
  INNER JOIN [ordercomms_review].[dbo].[request] r ON rt.request_id = r.request_id
  INNER JOIN [ordercomms_review].[dbo].[testSynonym] ts ON rt.testSynonym_id = ts.testSynonym_id
  WHERE r.discipline_id = 3
  AND amended = 0
  AND r.patient_id = 604413
  -- AND textual_result
  ORDER BY rt.rsample_id DESC


-- radiology requests
SELECT TOP (1000) *
  FROM [ordercomms_review].[dbo].[rnpi] rn
  INNER JOIN [ordercomms_review].[dbo].[request] r ON rn.request_id = r.request_id
  INNER JOIN [ordercomms_review].[dbo].[npiSynonym] ts ON rn.npiSynonym_id = ts.npiSynonym_id
  WHERE r.discipline_id >25
  AND amended = 0
  -- AND r.patient_id = 604413
  ORDER BY rn.rnpi_id DESC

-- TODO: Insert these



		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqt.rnpi_id as INT),'nNPI') as note_id,
			bat.groupid AS person_id, 
			omopBuild.dbo.getId(CAST(rqt.rnpi_id as INT),'rNPI') as note_event_id, --TODO: possible to map this to a sample entry if created
			21 as note_event_field_concept_id, -- specifies the note_event_id is from the measurement table
			CONVERT(DATE,request_date) AS note_date,
			CONVERT(DATETIME2,request_date) AS note_datetime, 
			44814641 as note_type_concept_id, -- Radiology request from vocabulary_id = 'Note Type'
			36716202 as note_class_concept_id, -- Radiology studies request from concept_class_id = 'Record Artifact' --TODO raise with OHSDI
			measurement_source_value as note_title,
			CONVERT(VARCHAR(MAX),rqt.text) as note_text,
			0 as encoding_concept_id, --TODO: raise with OHSDI
			4180186	as language_concept_id, --English language
			NULL as provider_id,
			NULL as visit_occurrence_id,
			NULL as visit_detail_id,
			d.name as note_source_value
		FROM 
		#tmpBatchRequest bat
			INNER JOIN ordercomms_review.dbo.request r ON bat.request_id=r.request_id
			INNER JOIN ordercomms_review.dbo.rnpi rqt on r.request_id = rqt.request_id
			INNER JOIN #tmpNpiMap tnm on rqt.npisynonym_id = tnm.npiSynonym_id
			INNER JOIN ordercomms_review.dbo.discipline d ON r.discipline_id = d.discipline_id
		WHERE 
			r.discipline_id in (25,26,27,28,29)


  -- notes from requests - 3407545
SELECT 
	TOP (1000)
	CONVERT(VARCHAR(MAX),src_comments) as note_text,
	* FROM 
	--COUNT(*) FROM
  [ordercomms_review].[dbo].[rqsample] rqs
  INNER JOIN [ordercomms_review].[dbo].[request] r on r.request_id = rqs.request_id
  WHERE amended = 0
  AND DATALENGTH(src_comments) > 0

-- notes from radiology requests
SELECT TOP (1000)
	CONVERT(VARCHAR(MAX),text) as note_text,
	*
FROM
  [ordercomms_review].[dbo].[rqnpi] rqs
  INNER JOIN [ordercomms_review].[dbo].[request] r on r.request_id = rqs.request_id
  WHERE amended = 0
  AND DATALENGTH(text) > 0


---------------------------------------------------
-- The main ETL alogrithm
-- an interruption tolerant ETL procedure for extracting 
-- results from the emis viewer platform using batching to prevent
-- saturating the server. This is still pretty slow and takes about 8
-- hours to copy accross. This may be because the indexing is switched on
-- before the loading happens, but in the end it is a lot of rows.

-- This can be run as a single call to update an existing table or
-- on a periodic basis to keep it up to date.
-- progress is written to the tblExtractLog table to allow for resume.
------------------------------------------------------
DROP PROCEDURE IF EXISTS dbo.extractBatchEMISRequests;
GO

CREATE PROCEDURE dbo.extractBatchEMISRequests AS
BEGIN

	RAISERROR ('Extracting requests...', 10, 1)  WITH NOWAIT

	DECLARE @minRequestId INT;
	DECLARE @maxRequestId INT;
	-- DECLARE @date datetime;
	-- DECLARE @message nvarchar(max);
	
	DECLARE @insertRows INT;
	SET @insertRows = -1;

	DECLARE @size int;
	DECLARE @message varchar(max);
	SET @size = 100000;
	
	-- initialise the minRequestId to be the largest value that appears in the tblExtract log
	SET @minRequestId = (SELECT MAX(CAST(maxPkValue as INT)) FROM omopBuild.dbo.ExtractLog WHERE db='ordercomms_review' and tbl='request' and pkName='request_id' and job='lab_orders');
	SET @minRequestId = IIF(@minRequestId IS NULL, 0, @minRequestId);
	-- last entered request in ordercomms database
	SET @maxRequestId = (SELECT TOP 1 request_id FROM ordercomms_review.dbo.request order by request_id DESC); 

	DROP TABLE IF EXISTS #tmpBatchRequest

	-- @batchRequest is a temp table to hold the next items that will be inserted. This is usually done in 100000 record batches.
	CREATE TABLE #tmpBatchRequest (
		id INT IDENTITY PRIMARY KEY,
		request_id INT,
		groupId INT,
		request_date DATETIME,
		INDEX X_hospital_no (request_id),
		INDEX X_no_duplicate_requests UNIQUE (groupId,request_id)
	);

	WHILE @minRequestId <= @maxRequestId AND @insertRows <> 0
	BEGIN
		RAISERROR ('Starting batch', 10, 1)  WITH NOWAIT
		DELETE FROM #tmpBatchRequest;

		-- This join generates duplicates wherever there are multiple RBA identifiers for a patient or other slight variations in the patient identifier. 
		-- It also select out blood results for patients that have ever been RBA patients 
		-- through the study population table. These tests may theoretically have been requested in a different context (by the GP).
		-- but is very difficult to prove that tests were not ordered in TSFT from the data. or in collaboration with TSFT
		-- All the test results would be available to a clinician in TSFT regardless of their 
		
		--TEST DECLARE @size INT; SET @size=1000; DECLARE @minRequestId INT; SET @minRequestId=5000000 
		INSERT INTO #tmpBatchRequest
		SELECT TOP(@size) 
			r.request_id,
			l.groupId,
			CONVERT(DATETIME,r.request_date+r.request_time)+sp.dateOffset as request_date
		FROM ordercomms_review.dbo.request r --WITH (INDEX(request_patientid))
			INNER JOIN omopBuild.dbo.OrdercommsLookup l ON r.patient_id = l.patient_id
			INNER JOIN omopBuild.dbo.StudyPopulation sp ON l.groupId = sp.groupId
		WHERE 
			r.request_id > @minRequestId
			and r.amended=0 
			and r.request_date IS NOT NULL
			and r.request_time IS NOT NULL
			--TEST and r.discipline_id = 2 -- histology
			--TEST and r.discipline_id >= 25 -- radiology
		
		SET @message='Processing batch, num requests: '+CONVERT(CHAR,@@ROWCOUNT);
		RAISERROR (@message, 10, 1) WITH NOWAIT

		-- Insert the non radiology requests
		-- This naturally excludes histopathology as they never have a battery_id 
		-- and are the only things that don't. [discipline_id <> 2 & battery_id IS NULL and opposite tested]
		-- N.B. Histopathology wil not be included in measurement, only in NOTE table and often contains PID
		-- TODO: Microbiology request details will not be in the measurement table but in the note table
		-- TODO: Microbiology as structured data
		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqs.rqsample_id as INT),'nREQ') as note_id,
			bat.groupid AS person_id, 
			NULL as note_event_id, --TODO: possible to map this to a sample entry if created
			NULL as note_event_field_concept_id, -- specifies the note_event_id is from the measurement table
			CONVERT(DATE,bat.request_date) AS note_date,
			CONVERT(DATETIME2,bat.request_date) AS note_datetime, 
			44814645 as note_type_concept_id, -- 44814645	Note - generic note from vocabulary_id = 'Note Type'
			44803912 as note_class_concept_id, -- 44803912	Service order request details from concept_class_id = 'Record Artifact' --TODO raise with OHSDI
			NULL as note_title,
			-- DATALENGTH(src_comments) as note_len,
			CONVERT(VARCHAR(MAX),src_comments) as note_text,
			0 as encoding_concept_id, --TODO: raise with OHSDI
			4180186	as language_concept_id, --English language
			NULL as provider_id,
			NULL as visit_occurrence_id,
			NULL as visit_detail_id,
			'Lab test request clinical details' as note_source_value
		FROM 
		#tmpBatchRequest bat
			INNER JOIN [ordercomms_review].[dbo].[rqsample] rqs on bat.request_id = rqs.request_id 
			INNER JOIN [ordercomms_review].[dbo].[request] r ON r.request_id = rqs.request_id
		WHERE DATALENGTH(src_comments) > 3 --N.b. 2+length of string
		
		SET @insertRows = @@ROWCOUNT

		-- TODO: Radiology into NOTE
		-- 44814641	Radiology request as note_type
		-- 36716202	Radiology studies request as note_class
		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqs.rqnpi_id as INT),'nRNP') as note_id,
			bat.groupid AS person_id, 
			NULL as note_event_id, --TODO: possible to map this to a sample entry if created
			NULL as note_event_field_concept_id, -- specifies the note_event_id is from the measurement table
			CONVERT(DATE,bat.request_date) AS note_date,
			CONVERT(DATETIME2,bat.request_date) AS note_datetime, 
			44814641 as note_type_concept_id, -- 	Radiology request from vocabulary_id = 'Note Type'
			44803912 as note_class_concept_id, -- 44803912	Service order request details from concept_class_id = 'Record Artifact' --TODO raise with OHSDI
			NULL as note_title,
			TRIM(CONVERT(VARCHAR(MAX),SUBSTRING([text],1,PATINDEX('%===%',text)-1))) as note_text,
			0 as encoding_concept_id, --TODO: raise with OHSDI
			4180186	as language_concept_id, --English language
			NULL as provider_id,
			NULL as visit_occurrence_id,
			NULL as visit_detail_id,
			'Radiology test request clinical details' as note_source_value
		FROM 
		#tmpBatchRequest bat
			INNER JOIN [ordercomms_review].[dbo].[rqnpi] rqs on bat.request_id = rqs.request_id 
			INNER JOIN [ordercomms_review].[dbo].[request] r ON r.request_id = rqs.request_id
		WHERE 
			DATALENGTH(text) > 3
		
		-- discipline 18 molecular pathology - textual_result is just metadata about the request whcih must be a seperate file

		SET @insertRows = @insertRows+@@ROWCOUNT


		SET @message='Inserted rows: '+CONVERT(CHAR,@insertRows);
		RAISERROR (@message, 10, 1) WITH NOWAIT
		

		-- TODO: this may not handle failures.  
		SET @minRequestId = IIF(@insertRows>0, (SELECT MAX(request_id) from #tmpBatchRequest), @minRequestId);
		-- This shouldn't happen but just in case....
		SET @minRequestId = IIF(@minRequestId IS NULL, @maxRequestId, @minRequestId);

		DECLARE @insertRequests INT;
		SET @insertRequests = (SELECT COUNT(*) from #tmpBatchRequest);
		
		-- Log successful extractions for resuming / incremental updates
		INSERT INTO omopBuild.dbo.ExtractLog (db,tbl,pkName,job,maxPkValue,extractDate,comment) 
		VALUES ('ordercomms_review','request','request_id','lab_orders',@minRequestId, getdate(),
			CONCAT('inserted ',@insertRequests,' requests, consisting of ',@insertRows,' test requestss'));
		

	END
	DROP TABLE  IF EXISTS #tmpBatchRequest
END


GO


-- --------------------------------------------------
-- Execute a single run of the stored procedure to bring TriNetX up to date with ordercomms_review
ALTER DATABASE omop
SET SINGLE_USER
WITH ROLLBACK IMMEDIATE;
GO

EXEC dbo.extractBatchEMISRequests
GO

ALTER DATABASE omop
SET MULTI_USER;
GO