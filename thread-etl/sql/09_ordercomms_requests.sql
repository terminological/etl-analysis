
-- ---------------------------------
-- Lab test and Radiology oders clinical details
-- ---------------------------------


-----------------------------------------
-- create an audit log of extractions
BEGIN TRY
	Create table omopBuild.dbo.ExtractLog (
		db VARCHAR(70),
		tbl VARCHAR(70),
		pkName VARCHAR(70), -- the name of the primary key
		job VARCHAR(70), -- the name of the extraction job
		maxPkValue VARCHAR(70), -- the max value of the primary key extracted so far
		extractDate DATETIME,
		comment TEXT
	)
END TRY
BEGIN CATCH END CATCH

-----------------------------------------
-- create a table of identifiable note values
BEGIN TRY
	CREATE TABLE omopBuild.dbo.IdentifiableNote (
		[note_id] [bigint] NOT NULL,
		[person_id] [bigint] NOT NULL,
		[note_event_id] [bigint] NULL,
		[note_event_field_concept_id] [int] NOT NULL,
		[note_date] [date] NULL,
		[note_datetime] [datetime2](7) NOT NULL,
		[note_type_concept_id] [int] NOT NULL,
		[note_class_concept_id] [int] NOT NULL,
		[note_title] [varchar](250) NULL,
		[note_text] [varchar](max) NULL,
		[encoding_concept_id] [int] NOT NULL,
		[language_concept_id] [int] NOT NULL,
		[provider_id] [bigint] NULL,
		[visit_occurrence_id] [bigint] NULL,
		[visit_detail_id] [bigint] NULL,
		[note_source_value] [varchar](50) NULL
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END TRY
BEGIN CATCH END CATCH

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
	
	DECLARE @insertRows INT = -1;
	DECLARE @size int = 100000;

	DECLARE @message varchar(max);
	
	-- initialise the minRequestId to be the largest value that appears in the tblExtract log
	SET @minRequestId = (SELECT MAX(CAST(maxPkValue as INT)) FROM omopBuild.dbo.ExtractLog WHERE db='ordercomms_review' and tbl='request' and pkName='request_id' and job='lab_orders');
	SET @minRequestId = IIF(@minRequestId IS NULL, 
		(SELECT TOP 1 request_id FROM ordercomms_review.dbo.request order by request_id ASC)
		, @minRequestId);
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

	WHILE @minRequestId <= @maxRequestId -- AND @insertRows <> 0
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
		SELECT
			r.request_id,
			l.groupId,
			CONVERT(DATETIME,r.request_date+r.request_time)+sp.dateOffset as request_date
		FROM ordercomms_review.dbo.request r --WITH (INDEX(request_patientid))
			INNER JOIN omopBuild.dbo.OrdercommsLookup l ON r.patient_id = l.patient_id
			INNER JOIN omopBuild.dbo.StudyPopulation sp ON l.groupId = sp.groupId
		WHERE 
			r.request_id > @minRequestId
			AND r.request_id < @minRequestId+@size
			and r.amended=0 
			and r.request_date IS NOT NULL
			and r.request_time IS NOT NULL
			--TEST and r.discipline_id = 2 -- histology
			--TEST and r.discipline_id >= 25 -- radiology
		
		SET @message='Processing batch, num requests: '+CONVERT(CHAR,@@ROWCOUNT);
		RAISERROR (@message, 10, 1) WITH NOWAIT

		-- Lab test requests into NOTE
		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqs.rqsample_id as INT),'nREQ') as note_id,
			bat.groupid AS person_id, 
			NULL as note_event_id, --TODO: possible to map this to a sample entry if created
			0 as note_event_field_concept_id, -- specifies the note_event_id is from the measurement table -- TODO: this is off spec, supposed to allow nulls but doesn;t
			CONVERT(DATE,bat.request_date) AS note_date,
			CONVERT(DATETIME2,bat.request_date) AS note_datetime, 
			44814645 as note_type_concept_id, -- 44814645	Note - generic note from vocabulary_id = 'Note Type'
			44803912 as note_class_concept_id, -- 44803912	Service order request details from concept_class_id = 'Record Artifact' --TODO raise with OHSDI
			NULL as note_title,
			-- DATALENGTH(src_comments) as note_len,
			CONVERT(VARCHAR(MAX),src_comments) as note_text,
			0 as encoding_concept_id, --TODO: raise with OHSDI - no values
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

		-- Radiology requests into NOTE
		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqs.rqnpi_id as INT),'nRNP') as note_id,
			bat.groupid AS person_id, 
			NULL as note_event_id, --TODO: possible to map this to a sample entry if created
			0 as note_event_field_concept_id, -- specifies the note_event_id is from the measurement table
			CONVERT(DATE,bat.request_date) AS note_date,
			CONVERT(DATETIME2,bat.request_date) AS note_datetime, 
			44814645 as note_type_concept_id, -- 44814645	Note - generic note from vocabulary_id = 'Note Type'
			44803912 as note_class_concept_id, -- 44803912	Service order request details from concept_class_id = 'Record Artifact' --TODO raise with OHSDI
			NULL as note_title,
			TRIM(CONVERT(VARCHAR(MAX),
				IIF(
					PATINDEX('%===%',text)>0,
					SUBSTRING([text],1,PATINDEX('%===%',text)-1),
					[text]
				))) as note_text,
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
			CONCAT('inserted ',@insertRequests,' requests, consisting of ',@insertRows,' test request clinical details'));

	END
	DROP TABLE  IF EXISTS #tmpBatchRequest
END


GO


-- --------------------------------------------------
-- Execute a single run of the stored procedure to bring TriNetX up to date with ordercomms_review
--ALTER DATABASE omopBuild
--SET SINGLE_USER
--WITH ROLLBACK IMMEDIATE;
--GO

EXEC dbo.extractBatchEMISRequests
GO

--ALTER DATABASE omopBuild
--SET MULTI_USER;
--GO