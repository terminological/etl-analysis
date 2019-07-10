use omopBuild;

-- =====================================
-- DEPRECATED - FOR ERFERENCE ONLY
-- =====================================

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
DROP PROCEDURE IF EXISTS dbo.wipeEMISResults;
GO

CREATE PROCEDURE dbo.wipeEMISResults AS
BEGIN
	DELETE FROM omopBuild.dbo.ExtractLog WHERE job='labs';
	DECLARE @nTES BIGINT = omopBuild.dbo.getId(0,'nTES')
	DECLARE @rTES BIGINT = omopBuild.dbo.getId(0,'rTES')
	DECLARE @nNPI BIGINT = omopBuild.dbo.getId(0,'nNPI')
	DECLARE @rNPI BIGINT = omopBuild.dbo.getId(0,'rNPI')
	DELETE FROM omop.dbo.measurement where measurement_id & @rTES = @rTES;
	DELETE FROM omop.dbo.measurement where measurement_id & @rNPI = @rNPI;
	DELETE FROM omopBuild.dbo.IdentifiableNote WHERE note_id & @nTES = @nTES;
	DELETE FROM omopBuild.dbo.IdentifiableNote WHERE note_id & @nNPI = @nNPI;
END
GO


EXEC dbo.wipeEMISResults;
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
-- create a temp table to store the various vocab mappings
-- Measurement codes - lab codes mapped by test and battery ids
----------------------------------------------------

DROP TABLE IF EXISTS #tmpMeasurementMap;

CREATE TABLE #tmpMeasurementMap (
batterySynonym_id INT,
testSynonym_id INT,
measurement_domain_id varchar(20),
measurement_concept_id INT,
measurement_source_value VARCHAR(255),
INDEX X_measurement_code UNIQUE (batterySynonym_id,testSynonym_id))
;

INSERT INTO  #tmpMeasurementMap
SELECT 
	-- TOP(100) *,
	used.batterySynonym_id as batterySynonym_id,
	used.testsynonym_id as testSynonym_id,
	mapped.omopDomainId as measurement_domain_id,
	mapped.omopConceptId as measurement_concept_id,
	CAST(used.sourceId as VARCHAR(255)) as measurement_source_value
FROM
(
	SELECT DISTINCT
		rb.batterySynonym_id,
		rt.testsynonym_id,
		--bs.original_display_name,
		--bs.from_code as battery_from_code,
		--ts.original_display_name,
		--ts.from_code as test_from_code,
		CONCAT(ts.from_code,'|',bs.from_code) collate Latin1_General_CI_AS as sourceId
	FROM
		ordercomms_review.dbo.rtest rt,
		ordercomms_review.dbo.rbattery rb,
		ordercomms_review.dbo.testSynonym ts,
		ordercomms_review.dbo.batterySynonym bs
		--,
		--omopBuild.dbo.ConceptMapping c
	WHERE 
		rt.rbattery_id = rb.rbattery_id
		AND rt.testsynonym_id = ts.testSynonym_id
		AND rb.batterySynonym_id = bs.batterySynonym_id
) used LEFT OUTER JOIN 
	omopBuild.dbo.ConceptMapping mapped 
	ON used.sourceId = mapped.sourceId
WHERE mapped.sourceDomain = 'urn:ordercomms:code-battery-code'

--select * from  #tmpMeasurementMap
--where measurement_source_value like '%|B'

--SELECT COUNT(*), testSynonym_id, batterySynonym_id
--FROM #tmpMeasurementMap
--GROUP BY testSynonym_id, batterySynonym_id

---------------------------------------------------
-- Unit codes - unit codes mapped by unit string
----------------------------------------------------

DROP TABLE IF EXISTS #tmpUnitMap

CREATE TABLE #tmpUnitMap (
unit_code VARCHAR(35),
unit_concept_id INT NOT NULL,
unit_source_value VARCHAR(35) NOT NULL,
INDEX X_unit_code (unit_code))

INSERT INTO #tmpUnitMap
	SELECT 
		sourceId,
		omopConceptId,
		sourceId
	FROM [omopBuild].[dbo].[ConceptMapping]
	WHERE sourceDomain = 'urn:unit'	

SELECT * FROM #tmpUnitMap
GO

---------------------------------------------------
-- Comparitor codes - greater than etc. codes mapped by string
----------------------------------------------------

DROP TABLE IF EXISTS #tmpComparitorMap

SELECT
      [concept_name],
	  [concept_id]
INTO #tmpComparitorMap
FROM [omop].[dbo].[concept]
WHERE domain_id = 'Meas Value Operator'
GO

-- SELECT * FROM #tmpComparitorMap

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

---------------------------------------------------
-- Result value mapping
-- A heuristic for identifying results that can be a coded value
---------------------------------------------------

-- -------------------------------------------------
-- Textual results where outcome is a short value 
-- Typically the result '*' is indicative of a haemolysed sample
-- The result ':' or '@' suggests there is something useful in the spc_comments
-- The values 'R' and 'S' are resistant or sensitive respectively
-- 'N' and 'Y'

--VALUE AS CONCEPT ID
--vocab with comain_id = 'Meas Value'
--concept_id	concept_name
--45878745	Abnormal
--45884153	Normal
--9189	Negative
--9190	Not detected
--9191	Positive
--4155143	Abnormally low
--4155142	Abnormally high
--4148441	Resistant
--4307105	Sensitive
--4123508	+
--4126673	++
--4125547	+++
--4126674	++++

DROP FUNCTION IF EXISTS dbo.fnValueMapping;
GO

CREATE FUNCTION dbo.fnValueMapping(@normalcy VARCHAR(25), @textual_result TEXT) RETURNS INT
AS
BEGIN
	DECLARE @text VARCHAR(MAX);
	SET @text = CONVERT(VARCHAR(MAX), @textual_result)
	DECLARE @lookup INT = (SELECT omopConceptId FROM dbo.ConceptMapping cm WHERE sourceDomain = 'urn:ordercomms:rtest:textual-result' AND sourceId=@text);
	RETURN CASE 
		WHEN(@lookup IS NOT NULL) THEN @lookup	
		WHEN(@normalcy='High') THEN 4155143 --Abnormally low
		WHEN(@normalcy='Low') THEN 4155142 --Abnormally high
		WHEN(@text='Y') THEN 9191 --Positive
		WHEN(@text='N') THEN 9189	--Negative
		WHEN(@text='R') THEN 4148441 --Resistant
		WHEN(@text='S') THEN 4307105 --Sensitive
		WHEN(@text='+') THEN 4123508	-- +
		WHEN(@text='++') THEN 4126673	-- ++
		WHEN(@text='+++') THEN 4125547	-- +++
		WHEN(@text='++++') THEN 4126674	-- ++++
		WHEN(@normalcy='Abnormal') THEN 45878745 --Abnormal
		ELSE NULL 
	END
END
GO

-- SELECT *,omopConceptId FROM dbo.ConceptMapping cm WHERE sourceDomain = 'urn:ordercomms:rtest:textual-result' AND sourceId='None seen'
-- SELECT dbo.fnValueMapping(NULL,'None seen')

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
DROP PROCEDURE IF EXISTS dbo.extractBatchEMISResults;
GO

CREATE PROCEDURE dbo.extractBatchEMISResults AS
BEGIN

	RAISERROR ('Extracting tests...', 10, 1)  WITH NOWAIT

	DECLARE @minReportId INT;
	DECLARE @maxReportId INT;
	-- DECLARE @date datetime;
	-- DECLARE @message nvarchar(max);
	
	DECLARE @insertRows INT = -1;

	DECLARE @size INT = 100000;
	DECLARE @message varchar(max);

	DECLARE @labResults INT = 0;
	DECLARE @radiolResults INT = 0;
	DECLARE @labNotes INT = 0;
	DECLARE @radiolNotes INT = 0;
	
	-- initialise the minReportId to be the largest value that appears in the tblExtract log
	SET @minReportId = (SELECT MAX(CAST(maxPkValue as INT)) FROM omopBuild.dbo.ExtractLog WHERE db='ordercomms_review' and tbl='report' and pkName='report_id' and job='labs');
	
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

		--TEST DECLARE @size INT; SET @size=1000; DECLARE @minReportId INT; SET @minReportId=5000000 
		INSERT INTO #tmpBatchReport
		SELECT
			r.report_id,
			l.groupId,
			CONVERT(DATETIME,r.result_date+r.result_time)+sp.dateOffset as report_date
		FROM ordercomms_review.dbo.report r --WITH (INDEX(report_patientid))
			INNER JOIN omopBuild.dbo.OrdercommsLookup l ON r.patient_id = l.patient_id
			INNER JOIN omopBuild.dbo.StudyPopulation sp ON l.groupId = sp.groupId
		WHERE 
			r.report_id > @minReportId
			AND r.report_id < @minReportId+@size
			and r.amended=0 
			and r.result_date IS NOT NULL
			and r.result_time IS NOT NULL
			--TEST and r.discipline_id = 2 -- histology
			--TEST and r.discipline_id >= 25 -- radiology
		
		SET @message='Processing batch, num reports: '+CONVERT(CHAR,@@ROWCOUNT);
		RAISERROR (@message, 10, 1) WITH NOWAIT

		-- Insert the non radiology reports
		-- This naturally excludes histopathology as they never have a battery_id 
		-- and are the only things that don't. [discipline_id <> 2 & battery_id IS NULL and opposite tested]
		-- N.B. Histopathology wil not be included in measurement, only in NOTE table and often contains PID
		-- TODO: Microbiology report details will not be in the measurement table but in the note table
		-- TODO: Microbiology as structured data
		INSERT INTO omop.dbo.measurement
		SELECT		
			omopBuild.dbo.getId(CAST(rqt.rtest_id as INT),'rTES') AS measurement_id,
			bat.groupid AS person_id, 
			tmm.measurement_concept_id,
			CONVERT(DATE,report_date) AS measurement_date,
			CONVERT(DATETIME2,report_date) AS measurement_datetime, 
			CONVERT(VARCHAR(8),report_date,108) as measurement_time,
			44818702 as measurement_type_concept_id, --	Lab result
			tcm.concept_id as operator_concept_id,
			rqt.numeric_result AS value_as_number,
			omopBuild.dbo.fnValueMapping(normalcy, textual_result) AS value_as_concept_id,
			tum.unit_concept_id,
			ts.low_range as range_low,
			ts.high_range as range_high,
			NULL as provider_id,
			NULL as visit_occurrence_id,
			NULL as visit_detail_id,
			tmm.measurement_source_value,
			tmm.measurement_concept_id as measurement_source_concept_id,
			unit_source_value,
			LEFT(TRIM(CONCAT(rqt.numeric_result,' ',rqt.textual_result)),50) AS value_source_value -- TODO: check this for PID
		FROM 
		#tmpBatchReport bat
			INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			INNER JOIN ordercomms_review.dbo.rsample rqs ON r.report_id = rqs.report_id
			LEFT OUTER JOIN ordercomms_review.dbo.rbattery rqb on rqs.rsample_id = rqb.rsample_id
			LEFT OUTER JOIN ordercomms_review.dbo.rtest rqt on r.report_id = rqt.report_id
			INNER JOIN ordercomms_review.dbo.testSynonym ts on rqt.testsynonym_id = ts.testSynonym_id
			INNER JOIN #tmpMeasurementMap tmm on rqt.testsynonym_id = tmm.testSynonym_id AND rqb.batterySynonym_id = tmm.batterySynonym_id
			INNER JOIN #tmpUnitMap tum on ts.unit collate Latin1_General_CI_AS  = tum.unit_code
			LEFT OUTER JOIN #tmpComparitorMap tcm on tcm.concept_name = comparitor COLLATE Latin1_General_CI_AS
		ORDER BY measurement_id
		;


		SET @labResults = @@ROWCOUNT
		SET @message='Inserted lab results: '+CONVERT(CHAR,@labResults);
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
			INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			INNER JOIN ordercomms_review.dbo.rnpi rqt on r.report_id = rqt.report_id
			-- INNER JOIN ordercomms_review.dbo.npiSynonym ts on rqt.npisynonym_id = ts.npiSynonym_id
			INNER JOIN #tmpNpiMap tnm on rqt.npisynonym_id = tnm.npiSynonym_id
		ORDER BY measurement_id
		;
		
		SET @radiolResults = @@ROWCOUNT
		SET @message='Inserted radiology results: '+CONVERT(CHAR,@radiolResults);
		RAISERROR (@message, 10, 1) WITH NOWAIT
		

		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqt.rtest_id as INT),'nTES') as note_id,
			bat.groupid AS person_id, 
			IIF(rbattery_id IS NOT NULL, omopBuild.dbo.getId(CAST(rqt.rtest_id as INT),'rTES'), NULL) as note_event_id, --TODO: possible to map this to a sample entry if created
			IIF(rbattery_id IS NOT NULL, 21, 0) as note_event_field_concept_id, -- specifies the note_event_id is from the measurement table
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
			d.name as note_source_value
		FROM 
		#tmpBatchReport bat
			INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			LEFT OUTER JOIN ordercomms_review.dbo.rtest rqt on r.report_id = rqt.report_id
			INNER JOIN ordercomms_review.dbo.testSynonym ts ON rqt.testSynonym_id = ts.testSynonym_id
			INNER JOIN ordercomms_review.dbo.discipline d ON r.discipline_id = d.discipline_id
		WHERE 
			r.discipline_id in (2,5,7,8,10,12,13,21)
			and DATALENGTH(textual_result) > 4
		-- discipline 18 molecular pathology - textual_result is just metadata about the report whcih must be a seperate file


		SET @labNotes = @@ROWCOUNT
		SET @message='Inserted lab text results: '+CONVERT(CHAR,@labNotes);
		RAISERROR (@message, 10, 1) WITH NOWAIT
		
		SET @insertRows = @insertRows+@@ROWCOUNT

		-- TODO: Radiology into NOTE
		-- 44814641	Radiology report as note_type
		-- 36716202	Radiology studies report as note_class

		INSERT INTO omopBuild.dbo.IdentifiableNote
		SELECT
			omopBuild.dbo.getId(CAST(rqt.rnpi_id as INT),'nNPI') as note_id,
			bat.groupid AS person_id, 
			omopBuild.dbo.getId(CAST(rqt.rnpi_id as INT),'rNPI') as note_event_id, --TODO: possible to map this to a sample entry if created
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
			d.name as note_source_value
		FROM 
		#tmpBatchReport bat
			INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			INNER JOIN ordercomms_review.dbo.rnpi rqt on r.report_id = rqt.report_id
			INNER JOIN #tmpNpiMap tnm on rqt.npisynonym_id = tnm.npiSynonym_id
			INNER JOIN ordercomms_review.dbo.discipline d ON r.discipline_id = d.discipline_id
		WHERE 
			r.discipline_id in (25,26,27,28,29)
			
		SET @radiolNotes = @@ROWCOUNT
		SET @message='Inserted radiology text results: '+CONVERT(CHAR,@radiolNotes);
		RAISERROR (@message, 10, 1) WITH NOWAIT

		SET @insertRows = @labNotes+@labResults+@radiolNotes+@radiolResults

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
		VALUES ('ordercomms_review','report','report_id','labs',@minReportId, getdate(),
			CONCAT('inserted total ',@insertReports,', consisting of ', 
				@labNotes,' lab notes (identifiable), ',
				@labResults,' lab results, ',
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

EXEC dbo.extractBatchEMISResults
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

