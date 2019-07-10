use omopBuild;

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

---------------------------------------------------
-- create a procedure to wipe the slate clean for a full rebuild
----------------------------------------------------
USE omopBuild;
DROP PROCEDURE IF EXISTS dbo.wipeEMISHistopathologyResults;
GO

CREATE PROCEDURE dbo.wipeEMISHistopathologyResults AS
BEGIN
	DELETE FROM omopBuild.dbo.ExtractLog WHERE job='histology';
	DECLARE @nTES BIGINT = omopBuild.dbo.getId(0,'nHIS')
	DELETE FROM omopBuild.dbo.IdentifiableNote WHERE note_id & @nTES = @nTES;
END
GO


EXEC dbo.wipeEMISHistopathologyResults;
---------------------------------------------------
-- create a shadow note table to store notes which have
-- not been de-identified
----------------------------------------------------

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
DROP PROCEDURE IF EXISTS dbo.extractBatchEMISHistopathologyResults;
GO

CREATE PROCEDURE dbo.extractBatchEMISHistopathologyResults AS
BEGIN

	RAISERROR ('Extracting tests...', 10, 1)  WITH NOWAIT

	DECLARE @minReportId INT;
	DECLARE @maxReportId INT;
	
	DECLARE @insertRows INT = -1;

	DECLARE @size INT = 10000;
	DECLARE @message varchar(max);

	DECLARE @histologyNotes INT = 0;
	
	-- initialise the minReportId to be the largest value that appears in the tblExtract log
	SET @minReportId = (SELECT MAX(CAST(maxPkValue as INT)) FROM omopBuild.dbo.ExtractLog WHERE db='ordercomms_review' and tbl='report' and pkName='report_id' and job='histology');
	
	SET @minReportId = IIF(@minReportId IS NULL OR @minReportId=0, 
		(SELECT TOP(1) report_id from ordercomms_review.dbo.report order by report_id ASC), @minReportId);
	-- last entered report in ordercomms database
	SET @maxReportId = (SELECT TOP 1 report_id FROM ordercomms_review.dbo.report order by report_id DESC); 

	DROP TABLE IF EXISTS #tmpBatchReport

	-- @batchReport is a temp table to hold the next items that will be inserted. This is usually done in 100000 record batches.
	CREATE TABLE #tmpBatchReport (
		id INT IDENTITY PRIMARY KEY,
		report_id INT,
		groupId INT,
		report_date DATETIME,
		INDEX X_hospital_no (report_id),
		INDEX X_no_duplicate_reports UNIQUE (groupId,report_id)
	);

	WHILE @minReportId <= @maxReportId AND @insertRows <> 0
	BEGIN
		RAISERROR ('Starting batch', 10, 1)  WITH NOWAIT
		DELETE FROM #tmpBatchReport;

		-- This join generates duplicates wherever there are multiple RBA identifiers for a patient or other slight variations in the patient identifier. 
		-- It also select out blood results for patients that have ever been RBA patients 
		-- through the study population table. These tests may theoretically have been requested in a different context (by the GP).
		-- but is very difficult to prove that tests were not ordered in TSFT from the data. or in collaboration with TSFT
		-- All the test results would be available to a clinician in TSFT regardless of their 
		
		-- interim microbiology reports were inspected and oterh than occasional no growth after 2 days seemed
		-- uninformative so have been excluded. All other interim reports were excluded.

		--TEST DECLARE @size INT; SET @size=1000; DECLARE @minReportId INT; SET @minReportId=0 
		INSERT INTO #tmpBatchReport
		SELECT TOP(@size)
			r.report_id,
			l.groupId,
			CONVERT(DATETIME,r.result_date+r.result_time)+sp.dateOffset as report_date
		FROM ordercomms_review.dbo.report r --WITH (INDEX(report_patientid))
			INNER JOIN omopBuild.dbo.OrdercommsLookup l ON r.patient_id = l.patient_id
			INNER JOIN omopBuild.dbo.StudyPopulation sp ON l.groupId = sp.groupId
		WHERE 
			r.report_id > @minReportId
			and r.amended=0 
			and r.result_date IS NOT NULL
			and r.result_time IS NOT NULL
			and r.discipline_id =2
		ORDER BY r.report_id
		
		SET @message='Processing batch, num reports: '+CONVERT(CHAR,@@ROWCOUNT);
		RAISERROR (@message, 10, 1) WITH NOWAIT

		--TODO: Could this be inserted into the specimen table
		---- TODO: Microbiology report details will not be in the measurement table but in the note table
		---- TODO: Microbiology as structured data
		--INSERT INTO omop.dbo.measurement
		--SELECT		
		--	omopBuild.dbo.getId(CAST(rqt.rtest_id as INT),'rTES') AS measurement_id,
		--	bat.groupid AS person_id, 
		--	tmm.measurement_concept_id,
		--	CONVERT(DATE,report_date) AS measurement_date,
		--	CONVERT(DATETIME2,report_date) AS measurement_datetime, 
		--	CONVERT(VARCHAR(8),report_date,108) as measurement_time,
		--	44818702 as measurement_type_concept_id, --	Lab result
		--	tcm.concept_id as operator_concept_id,
		--	rqt.numeric_result AS value_as_number,
		--	omopBuild.dbo.fnValueMapping(normalcy, textual_result) AS value_as_concept_id,
		--	tum.unit_concept_id,
		--	ts.low_range as range_low,
		--	ts.high_range as range_high,
		--	NULL as provider_id,
		--	NULL as visit_occurrence_id,
		--	NULL as visit_detail_id,
		--	tmm.measurement_source_value,
		--	tmm.measurement_concept_id as measurement_source_concept_id,
		--	unit_source_value,
		--	LEFT(TRIM(CONCAT(rqt.numeric_result,' ',rqt.textual_result)),50) AS value_source_value -- TODO: check this for PID
		--FROM 
		--#tmpBatchReport bat
		--	-- INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
		--	-- INNER JOIN ordercomms_review.dbo.rsample rqs ON bat.report_id = rqs.report_id
		--	INNER JOIN ordercomms_review.dbo.rbattery rqb on bat.report_id = rqb.report_id --rqs.rsample_id = rqb.rsample_id
		--	INNER JOIN ordercomms_review.dbo.rtest rqt on (bat.report_id = rqt.report_id and rqb.rbattery_id = rqt.rbattery_id)
		--	INNER JOIN ordercomms_review.dbo.testSynonym ts on rqt.testsynonym_id = ts.testSynonym_id
			
		--	INNER JOIN #tmpMeasurementMap tmm on rqt.testsynonym_id = tmm.testSynonym_id AND rqb.battery_id = tmm.battery_id
		--	LEFT OUTER JOIN #tmpUnitMap tum on ts.unit collate Latin1_General_CI_AS  = tum.unit_code
		--	LEFT OUTER JOIN #tmpComparitorMap tcm on tcm.concept_name = comparitor COLLATE Latin1_General_CI_AS
		--ORDER BY measurement_id
		--;


		--SET @labResults = @@ROWCOUNT
		--SET @message='Inserted lab results: '+CONVERT(CHAR,@labResults);
		--RAISERROR (@message, 10, 1) WITH NOWAIT


		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqt.rtest_id as INT),'nHIS') as note_id,
			bat.groupid AS person_id, 
			NULL as note_event_id, --TODO: possible to map this to a sample entry if created
			0 as note_event_field_concept_id, -- specifies the note_event_id is not from any table (TODO: sample/specimen?)
			CONVERT(DATE,report_date) AS note_date,
			CONVERT(DATETIME2,report_date) AS note_datetime, 
			44814642 as note_type_concept_id, -- Pathology report from vocabulary_id = 'Note Type'
			37395601 as note_class_concept_id, -- Laboratory report from concept_class_id = 'Record Artifact' --TODO raise with OHSDI
			original_display_name as note_title,
			CONVERT(VARCHAR(MAX),textual_result) as note_text,
			0 as encoding_concept_id, --TODO: raise with OHSDI
			4180186	as language_concept_id, --English language
			NULL as provider_id,
			NULL as visit_occurrence_id,
			NULL as visit_detail_id,
			'Histopathology report' as note_source_value
		FROM 
		#tmpBatchReport bat
			-- INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			INNER JOIN ordercomms_review.dbo.rtest rqt on bat.report_id = rqt.report_id
			INNER JOIN ordercomms_review.dbo.testSynonym ts ON rqt.testSynonym_id = ts.testSynonym_id
			
		WHERE 
			DATALENGTH(rqt.textual_result) > 4
		-- discipline 18 molecular pathology - textual_result is just metadata about the report whcih must be a seperate file


		SET @histologyNotes = @@ROWCOUNT
		SET @message='Inserted histology text results: '+CONVERT(CHAR,@histologyNotes);
		RAISERROR (@message, 10, 1) WITH NOWAIT
		
		SET @insertRows = @histologyNotes

		
		-- TODO: this may not handle failures.  
		SET @minReportId = IIF(@insertRows>0, (SELECT MAX(report_id) from #tmpBatchReport), @minReportId);
		-- This shouldn't happen but just in case....
		SET @minReportId = IIF(@minReportId IS NULL, @maxReportId, @minReportId);

		DECLARE @insertReports INT;
		SET @insertReports = (SELECT COUNT(*) from #tmpBatchReport);
		
		-- Log successful extractions for resuming / incremental updates
		INSERT INTO omopBuild.dbo.ExtractLog (db,tbl,pkName,job,maxPkValue,extractDate,comment) 
		VALUES ('ordercomms_review','report','report_id','histology',@minReportId, getdate(),
			CONCAT('inserted total ',@insertReports,', consisting of ', 
				@histologyNotes,' histology notes (identifiable), '
				));
		

	END
	DROP TABLE  IF EXISTS #tmpBatchReport
END


GO


-- --------------------------------------------------
-- Execute a single run of the stored procedure to bring TriNetX up to date with ordercomms_review

EXEC dbo.extractBatchEMISHistopathologyResults
GO



