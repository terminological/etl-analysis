use TriNetX;

-----------------------------------------
-- create an audit log of extractions
BEGIN TRY
	Create table TriNetX.dbo.tblExtractLog (
		db VARCHAR(70),
		tbl VARCHAR(70),
		pkName VARCHAR(70), -- the name of the primary key
		maxPkValue VARCHAR(70), -- the max value of the primary key extracted so far
		extractDate DATETIME,
		comment TEXT
	)
END TRY
BEGIN CATCH END CATCH

-------------------------------------------
-- create the TriNetX lab result table
-- N.B. this is based on best information I had at the time and may need to be revised with the spec
BEGIN TRY
	-- excludes radiology
	CREATE TABLE TriNetX.dbo.tblTriNetXLabResult (
		[Patient id] VARCHAR(255), 
		[Encounter id] VARCHAR(255), 
		[Provider id] VARCHAR(255), 
		[Code System] VARCHAR(70), 
		[Code] VARCHAR(70), 
		[Description] VARCHAR(70), 
		[Battery Code System] VARCHAR(70), 
		[Battery Code] VARCHAR(70), 
		[Battery Description] VARCHAR(70), 
		[Lab Section] VARCHAR(70), 
		[Normal Range] VARCHAR(51), 
		[Test date] SMALLDATETIME, 
		[Result type] VARCHAR(10), --NUMERIC or TEXT 
		[Numeric value] FLOAT, 
		[Text value] TEXT, 
		[Units of measure] VARCHAR(25)

		INDEX X_Patient_id ([Patient id]),
		INDEX X_Code ([Code]),
		INDEX X_Battery_Code ([Battery Code]),
		INDEX X_Test_date ([Test date])
	);
END TRY
BEGIN CATCH 
END CATCH
GO

---------------------------------------------------
-- create a procedure to wipe the slate clean for a full rebuild
----------------------------------------------------
DROP PROCEDURE IF EXISTS dbo.uspWipeEMISResults;
GO

CREATE PROCEDURE dbo.uspWipeEMISResults AS
BEGIN
	DELETE FROM TriNetX.dbo.tblExtractLog WHERE db='ordercomms_review' and tbl='report' and pkName='report_id';
	DELETE FROM TriNetX.dbo.tblTriNetXLabResult;
END
GO


---------------------------------------------------
-- an interruption tolerant ETL procedure for extracting 
-- results from the emis viewer platform using batching to prevent
-- saturating the server. This is still pretty slow and takes about 8
-- hours to copy accross. This may be because the indexing is switched on
-- before the loading happens, but in the end it is a lot of rows.

-- This can be run as a single call to update an existing table or
-- on a periodic basis to keep it up to date.
-- progress is written to the tblExtractLog table to allow for resume.
------------------------------------------------------
DROP PROCEDURE IF EXISTS dbo.uspEMISResults;
GO

CREATE PROCEDURE dbo.uspEMISResults AS
BEGIN

	DECLARE @minReportId INT;
	DECLARE @maxReportId INT;
	-- DECLARE @date datetime;
	-- DECLARE @message nvarchar(max);
	DECLARE @size int;
	DECLARE @insertRows INT;
	SET @insertRows = -1;

	SET @size = 100000;
	-- initialise the minReportId to be the largest value that appears in the tblExtract log
	SET @minReportId = (SELECT MAX(CAST(maxPkValue as INT)) FROM TriNetX.dbo.tblExtractLog WHERE db='ordercomms_review' and tbl='report' and pkName='report_id');
	SET @minReportId = IIF(@minReportId IS NULL, 0, @minReportId);
	-- last entered report in ordercomms database
	SET @maxReportId = (SELECT TOP 1 report_id FROM ordercomms_review.dbo.report order by report_id DESC); 

	-- @batchReport is a temp table to hold the next items that will be inserted. This is usually done in 100000 record batches.
	DECLARE @batchReport TABLE (
		report_id INT,
		[Patient id] VARCHAR(255),

		INDEX X_report_id (report_id),
		INDEX X_hospital_no ([Patient id])
	);
	
	WHILE @minReportId <= @maxReportId OR @insertRows <> 0
	BEGIN

		DELETE FROM @batchReport;

		INSERT INTO @batchReport
		SELECT TOP(@size)
			r.report_id,
			RIGHT(l.hospital_no,LEN(l.hospital_no)-3) as hospital_no
		FROM ordercomms_review.dbo.report r WITH (INDEX(PK_report_reportid))
			INNER JOIN ordercomms_review.dbo.lab_patient l 
			ON r.patient_id = l.patient_id
		WHERE 
			l.hospital_no like 'RBA%' -- previously done on request location id...
			and r.report_id > @minReportId
			and r.amended=0 -- TODO: interim microbiology reports?
			and r.result_date IS NOT NULL
			and r.result_time IS NOT NULL;

		INSERT INTO TriNetX.dbo.tblTriNetXLabResult
		SELECT
			anon.anonpid AS [Patient id], 
			NULL AS [Encounter id], -- TODO: could map into existing encounters by date but may not be known
			r.requester_id AS [Provider id], -- TODO: needs mapping to clinician list. non trivial.
			'TSFT_INTERNAL' AS [Code System], 
			-- ts.test_id AS [Code], -- could use this instead not sure which is better
			ts.from_code AS [Code], 
			ts.original_display_name AS [Description], 
			'TSFT_INTERNAL' AS [Battery Code System], 
			-- IIF(b.battery_id IS NULL, ts.test_id, b.battery_id) AS [Battery Code], -- could use this instead not sure which is better
			IIF(b.from_code IS NULL, ts.from_code, b.from_code) AS [Battery Code],
			IIF(b.original_display_name IS NULL, ts.original_display_name, b.original_display_name) AS [Battery Description], 
			d.name AS [Lab Section], 
			IIF(LEN(ts.low_range)+LEN(ts.high_range)>0, concat(ts.low_range,'-',ts.high_range), NULL) AS [Normal Range] , 
			r.result_date+r.result_time AS [Test date], 
			IIF(rqt.numeric_result IS NULL, 'TEXT', 'NUMERIC') AS [Result type], -- NUMERIC or TEXT 
			rqt.numeric_result AS [Numeric value], 
			cast(rqt.textual_result as nvarchar(max)) AS [Text value], 
			ts.unit AS [Units of measure]
		FROM 
		@batchReport bat
			INNER JOIN ordercomms_review.dbo.report r ON bat.report_id=r.report_id
			INNER JOIN TriNetX.dbo.tblPatlink anon ON anon.pid = bat.[Patient id]
			INNER JOIN ordercomms_review.dbo.rsample rqs ON r.report_id = rqs.report_id
			LEFT OUTER JOIN ordercomms_review.dbo.rbattery rqb on rqs.rsample_id = rqb.rsample_id
			INNER JOIN ordercomms_review.dbo.sampleSynonym s ON rqs.sampleSynonym_id = s.sampleSynonym_id
			INNER JOIN ordercomms_review.dbo.batterySynonym b ON rqb.batterySynonym_id = b.batterySynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.rtest rqt on rqs.rsample_id = rqt.rsample_id and rqt.rbattery_id = rqb.rbattery_id
			INNER JOIN ordercomms_review.dbo.testSynonym ts on rqt.testsynonym_id = ts.testSynonym_id
			INNER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
		;

		-- TODO: Radiology results are not included in these. The code to include these is similar but uses the NPI table
		-- however there is a question about whether the textual reports should be held here or as documents relating to patient.
		-- there is an argument that unstructured microbiology reports should not be included here either.
		
		DECLARE @insertReports INT;

		SET @insertRows = @@ROWCOUNT

		-- TODO: this may not handle failures.  
		SET @minReportId = (SELECT MAX(report_id) from @batchReport);
		SET @minReportId = IIF(@minReportId IS NULL, 0, @minReportId);

		SET @insertReports = (SELECT COUNT(*) from @batchReport);
		
		-- Log successful extractions for resuming / incremental updates
		INSERT INTO TriNetX.dbo.tblExtractLog (db,tbl,pkName,maxPkValue,extractDate,comment) 
		VALUES ('ordercomms_review','report','report_id',@minReportId, getdate(),
			CONCAT('inserted ',@insertReports,' reports, consisting of ',@insertRows,' test results'));
		

	END
END
GO


-- --------------------------------------------------
-- Execute a single run of the stored procedure to bring TriNetX up to date with ordercomms_review
-- ALTER DATABASE TriNetX
-- SET SINGLE_USER
-- WITH ROLLBACK IMMEDIATE;
-- GO

EXEC dbo.uspEMISResults
GO

-- ALTER DATABASE TriNetX
-- SET MULTI_USER;
-- GO


-----------------------------------------------
-- find out progress
SELECT TOP [db]
      ,[tbl]
      ,[pkName]
      ,[maxPkValue]
      ,[extractDate]
      ,[comment]
FROM [TriNetX].[dbo].[tblExtractLog]
GO

-----------------------------------------------
-- TODO: codes and codeSystem mappings
-- Generate file for TrinetX to do code system mapping
-- this is an aggregate of the counts of individual tests as they appear in the database
-- to inform mapping them to standard codeSystems
-- this view needs to be exported as a CSV (tasks... > export Data... > )
-- or from powershell using bcp as 
-- bcp -S MPH-EPRDTSDB1\DTSDEV TriNetX.dbo.viewTriNetXLabCodeSystems out C:\Test\TriNetXLabCodeSystems.csv
-----------------------------------------------
CREATE VIEW dbo.viewTriNetXLabCodeSystems AS
SELECT
	'TSFT' as [Source Id],
	[Code],
	[Code System],
	[Description],
	[Units of measure],
	[Lab section], --i.e. specialty
	NULL as [LOINC Code],
	COUNT(Distinct([Patient id])) as [Patient count], --patients with this type of result
	COUNT(*) as [Observation count], --number of observations for this type of result
	[Normal Range], -- as a range ##-##
	[Battery Code],
	[Battery Description],
	[Battery Code System],
	NULL as [Related Code],
	NULL as [Related Code System],
	NULL as [Related Code Description],
	MIN([Test date]) as [Earliest Observation timestamp],
	MAX([Test date]) as [Most Recent Observation timestamp],
	MIN([Numeric value]) as [Minimum value],
	MAX([Numeric value]) as [Maximum value],
	AVG([Numeric value]) as [Average value]
from TriNetX.dbo.tblTriNetXLabResult GROUP BY
	[Code System], 
	[Code], 
	[Description], 
	[Battery Code System], 
	[Battery Code], 
	[Battery Description], 
	[Lab Section], 
	[Normal Range], 
	[Units of measure];
GO

-------------------------------------------------------------
-- TODO: clinician ids
-- the emis database has its own unique set of clinicians and locations which are not
-- immediately compatible with the clinician in the TriNetX extraction so far.
-- the relevant clinician ids and all synonyms in the EMIS database are as follows and will need manual mapping
-------------------------------------------------------------
SELECT COUNT(*) as reports,
	cs.*
FROM 
	ordercomms_review.dbo.lab_patient l
	LEFT JOIN ordercomms_review.dbo.report r on l.patient_id = r.patient_id
	LEFT JOIN ordercomms_review.dbo.clinicianSynonym cs on cs.clinician_id = r.responsible_clinician_id
WHERE
	l.hospital_no like 'RBA%'
GROUP BY [subtype_id]
      ,cs.[lab_id]
      ,[clinician_id]
      ,[from_code]
      ,[from_code_instance]
      ,[clinician_type]
      ,[clinicianSynonym_id]
      ,[original_display_name]
      ,[original_clinician_id]
ORDER BY reports DESC
GO

---------------------------------------------------------------
-- TODO: Location ids
-- This is not actually part of the TriNetX spec so probably not worth worrying about
-- it is also not in the OMOP spec. having said that it is indirectly specified by the "visit" or "encounter" which are both in the 
-- TriNetX and OMOP specs

-- technically if we are trying to tie a test result back to an encounter we need to look at the requested
-- location, request date and try and match them up to the visit/encounter. probably this can be done just on the request date or more likely 
-- the specimen recieved date.
--------------------------------------------------------------

