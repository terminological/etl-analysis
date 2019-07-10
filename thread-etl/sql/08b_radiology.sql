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
DROP PROCEDURE IF EXISTS dbo.wipeEMISRadiologyResults;
GO

CREATE PROCEDURE dbo.wipeEMISRadiologyResults AS
BEGIN
	DELETE FROM omopBuild.dbo.ExtractLog WHERE job='radiol';
	DECLARE @nNPI BIGINT = omopBuild.dbo.getId(0,'nNPI')
	DECLARE @rNPI BIGINT = omopBuild.dbo.getId(0,'rNPI')
	DELETE FROM omop.dbo.measurement where measurement_id & @rNPI = @rNPI;
	DELETE FROM omopBuild.dbo.IdentifiableNote WHERE note_id & @nNPI = @nNPI;
END
GO


EXEC dbo.wipeEMISRadiologyResults;
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
-- MPI codes - radiology codes mapped by npi synonym
-- NPI map created after manual review of automatically generated
----------------------------------------------------

DROP TABLE IF EXISTS #tmpNpiMap;

CREATE TABLE #tmpNpiMap (
npisynonym_id INT PRIMARY KEY,
measurement_concept_id INT NOT NULL,
measurement_source_value VARCHAR(50)
);

INSERT INTO #tmpNpiMap
SELECT DISTINCT
	npisynonym_id,
	omopConceptId as measurement_concept_id,
	LEFT(CONCAT(from_code,'|',original_display_name),50) as measurement_source_value
FROM 
	ordercomms_review.dbo.npiSynonym ns LEFT OUTER JOIN
	omopBuild.dbo.ConceptMapping cm ON ns.from_code Collate Latin1_General_CI_AS = cm.sourceId
WHERE 
	cm.sourceDomain = 'urn:ordercomms:npi-synonym:from-code'
GO

-- SELECT * FROM #tmpNpiMap

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
DROP PROCEDURE IF EXISTS dbo.extractBatchEMISRadiologyResults;
GO

CREATE PROCEDURE dbo.extractBatchEMISRadiologyResults AS
BEGIN

	RAISERROR ('Extracting radiology...', 10, 1)  WITH NOWAIT

	DECLARE @minReportId INT;
	DECLARE @maxReportId INT;
	-- DECLARE @date datetime;
	-- DECLARE @message nvarchar(max);
	
	DECLARE @insertRows INT = -1;

	DECLARE @size INT = 100000;
	DECLARE @message varchar(max);

	DECLARE @radiolResults INT = 0;
	DECLARE @radiolNotes INT = 0;
	
	-- initialise the minReportId to be the largest value that appears in the tblExtract log
	SET @minReportId = (SELECT MAX(CAST(maxPkValue as INT)) FROM omopBuild.dbo.ExtractLog WHERE db='ordercomms_review' and tbl='report' and pkName='report_id' and job='radiol');
	
	SET @minReportId = IIF(@minReportId IS NULL OR @minReportId=0, 
		(SELECT TOP(1) report_id from ordercomms_review.dbo.report where discipline_id in (25,26,27,28,29) order by report_id ASC), @minReportId);
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

		--TEST DECLARE @size INT=1000; DECLARE @minReportId INT=18999810
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
			and r.discipline_id in (25,26,27,28,29,35,39,42,43,44,46,47,48,49,52,53,54,58,59,60,61,68)
		ORDER BY r.report_id

		SET @message='Processing batch, num reports: '+CONVERT(CHAR,@@ROWCOUNT);
		RAISERROR (@message, 10, 1) WITH NOWAIT

		-- Radiology TESTS (but not results) are included in the measurement table but results in the NOTE table
		-- and linked to measurement record. 
		INSERT INTO omop.dbo.measurement
		SELECT 	
			omopBuild.dbo.getId(CAST(rqt.rnpi_id as INT),'rNPI') AS measurement_id,
			bat.groupid AS person_id, 
			tnm.measurement_concept_id,
			CONVERT(DATE,report_date) AS measurement_date,
			CONVERT(DATETIME2,report_date) AS measurement_datetime, 
			CONVERT(VARCHAR(8),report_date,108) as measurement_time,
			5001 as measurement_type_concept_id, --	5001	Test ordered through EHR N.b. no good match for this
			NULL as operator_concept_id,
			NULL AS value_as_number,
			4160030 AS value_as_concept_id, -- 4160030	Performed
			NULL as unit_concept_id, 
			NULL as range_low,
			NULL as range_high,
			NULL as provider_id,
			NULL as visit_occurrence_id,
			NULL as visit_detail_id,
			tnm.measurement_source_value,
			tnm.measurement_concept_id as measurement_source_concept_id,
			NULL as unit_source_value,
			'report available' AS value_source_value
		FROM 
		#tmpBatchReport bat
			-- INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			INNER JOIN ordercomms_review.dbo.rnpi rqt on bat.report_id = rqt.report_id
			INNER JOIN #tmpNpiMap tnm on rqt.npisynonym_id = tnm.npiSynonym_id
		ORDER BY measurement_id
		;
		
		SET @radiolResults = @@ROWCOUNT
		SET @message='Inserted radiology results: '+CONVERT(CHAR,@radiolResults);
		RAISERROR (@message, 10, 1) WITH NOWAIT
		
		-- TODO: Radiology into NOTE
		-- 44814641	Radiology report as note_type
		-- 36716202	Radiology studies report as note_class

		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqt.rnpi_id as INT),'nNPI') as note_id,
			bat.groupid AS person_id, 
			omopBuild.dbo.getId(CAST(rqt.rnpi_id as INT),'rNPI') as note_event_id, 
			21 as note_event_field_concept_id, -- specifies the note_event_id is from the measurement table
			CONVERT(DATE,report_date) AS note_date,
			CONVERT(DATETIME2,report_date) AS note_datetime, 
			44814641 as note_type_concept_id, -- Radiology report from vocabulary_id = 'Note Type'
			36716202 as note_class_concept_id, -- Radiology studies report from concept_class_id = 'Record Artifact' --TODO raise with OHSDI
			measurement_source_value as note_title,
			CONVERT(VARCHAR(MAX),rqt.text) as note_text,
			0 as encoding_concept_id, --TODO: raise with OHSDI
			4180186	as language_concept_id, --English language
			NULL as provider_id,
			NULL as visit_occurrence_id,
			NULL as visit_detail_id,
			d.name+' report' as note_source_value
		FROM 
		#tmpBatchReport bat
			INNER JOIN ordercomms_review.dbo.rnpi rqt on bat.report_id = rqt.report_id
			INNER JOIN #tmpNpiMap tnm on rqt.npisynonym_id = tnm.npiSynonym_id
			INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			INNER JOIN ordercomms_review.dbo.discipline d ON r.discipline_id = d.discipline_id
		
			
		SET @radiolNotes = @@ROWCOUNT
		SET @message='Inserted radiology text results: '+CONVERT(CHAR,@radiolNotes);
		RAISERROR (@message, 10, 1) WITH NOWAIT

		SET @insertRows = @radiolNotes+@radiolResults

		-- TODO: Request comments into NOTE
		-- 44814645	Note as note type
		-- 45770247	Request

		SET @message='Inserted rows: '+CONVERT(CHAR,@insertRows);
		RAISERROR (@message, 10, 1) WITH NOWAIT
		

		-- TODO: this may not handle failures.  
		SET @minReportId = IIF(@insertRows>0, (SELECT MAX(report_id) from #tmpBatchReport), @minReportId);
		-- This shouldn't happen but just in case....
		SET @minReportId = IIF(@minReportId IS NULL, @maxReportId, @minReportId);

		DECLARE @insertReports INT;
		SET @insertReports = (SELECT COUNT(*) from #tmpBatchReport);
		
		-- Log successful extractions for resuming / incremental updates
		INSERT INTO omopBuild.dbo.ExtractLog (db,tbl,pkName,job,maxPkValue,extractDate,comment) 
		VALUES ('ordercomms_review','report','report_id','radiol',@minReportId, getdate(),
			CONCAT('inserted total ',@insertReports,', consisting of ', 
				@radiolNotes,' radiology notes (identifiable), ',
				@radiolResults,' radiology results')
				);
		

	END
	DROP TABLE  IF EXISTS #tmpBatchReport
END


GO


-- --------------------------------------------------
-- Execute a single run of the stored procedure to bring TriNetX up to date with ordercomms_review
ALTER DATABASE omop
SET SINGLE_USER
WITH ROLLBACK IMMEDIATE;
GO

EXEC dbo.extractBatchEMISRadiologyResults
GO

ALTER DATABASE omop
SET MULTI_USER;
GO


-------------------------------------------------
---- find out progress
--SELECT [db]
--      ,[tbl]
--      ,[pkName]
--	  ,job
--      ,[maxPkValue]
--      ,[extractDate]
--      ,[comment]
--FROM omopBuild.dbo.ExtractLog
--GO

-------------------------------------------------------------
-- TODO: clinician ids
-- the emis database has its own unique set of clinicians and locations which are not
-- immediately compatible with the clinician in the TriNetX extraction so far.
-- the relevant clinician ids and all synonyms in the EMIS database are as follows and will need manual mapping
-------------------------------------------------------------
--SELECT COUNT(*) as reports,
--	cs.*
--FROM 
--	ordercomms_review.dbo.lab_patient l
--	LEFT JOIN ordercomms_review.dbo.report r on l.patient_id = r.patient_id
--	LEFT JOIN ordercomms_review.dbo.clinicianSynonym cs on cs.clinician_id = r.responsible_clinician_id
--WHERE
--	l.hospital_no like 'RBA%'
--GROUP BY [subtype_id]
--      ,cs.[lab_id]
--      ,[clinician_id]
--      ,[from_code]
--      ,[from_code_instance]
--      ,[clinician_type]
--      ,[clinicianSynonym_id]
--      ,[original_display_name]
--      ,[original_clinician_id]
--ORDER BY reports DESC
--GO

---------------------------------------------------------------
-- TODO: Location ids
-- This is not actually part of the TriNetX spec so probably not worth worrying about
-- it is also not in the OMOP spec. having said that it is indirectly specified by the "visit" or "encounter" which are both in the 
-- TriNetX and OMOP specs

-- technically if we are trying to tie a test result back to an encounter we need to look at the requested
-- location, request date and try and match them up to the visit/encounter. probably this can be done just on the request date or more likely 
-- the specimen recieved date which is not in the extract as is.
--------------------------------------------------------------

