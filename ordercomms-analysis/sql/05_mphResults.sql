-- COPY ACCROSS THE DETAIL OF THE RESULTS
-- this copies individual test results and takes a long time
-- also anonymises the patient id
-- it uses a rather complex process to do the results in chunks
-- if interrupted it will resume where it was.
-- it can handle incremental updates

-- Use ordercomms_review;
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO


ALTER DATABASE RobsDatabase
SET SINGLE_USER
WITH ROLLBACK IMMEDIATE;
GO

DROP TABLE IF EXISTS RobsDatabase.[dbo].[tsftResult];

CREATE TABLE RobsDatabase.[dbo].[tsftResult](
	[internal_id] [int] NOT NULL,
	[location_id] [int] NOT NULL,
	[date] [smalldatetime] NOT NULL,
	[patient_id] [binary](32) NOT NULL,
	[clinician_id] [int] NOT NULL,
	
	[src_comments] [nvarchar](max) NULL,
	[discipline] [varchar](70) NULL,
	[discipline_name] [varchar](70) NULL,

	[investigation_id] [int] NULL,
	[investigation] [varchar](70) NULL,
	[investigation_name] [varchar](70) NULL,
	[investigation_abnormal] [tinyint] NULL,

	[sample_id] [int] NULL,
	[sample] [varchar](70) NULL,
	
	[test_id] [int] NULL,
	[test] [varchar](70) NULL,
	[test_name] [varchar](70) NULL,
	[test_abnormal] [tinyint] NULL,

	[textual_result] [nvarchar](max) NULL,
	[numeric_result] [varchar](25) NULL,
	[unit] [varchar](25) NULL,
	[normalcy] [varchar](25) NULL,
	[low_range] [varchar](25) NULL,
	[high_range] [varchar](25) NULL,

	INDEX X_internal_id (internal_id),
	INDEX X_location_id (location_id),
	INDEX X_patient_id (patient_id),
	INDEX X_clinician_id (clinician_id),
	INDEX X_investigation (investigation),
	INDEX X_test (test),
	INDEX X_test_normalcy (test,normalcy),
	INDEX X_discipline_id (discipline_id)
) ON [PRIMARY];


CREATE CLUSTERED INDEX X_result_date ON RobsDatabase.[dbo].tsftResult (date);
GO

Use RobsDatabase;
GO

DROP PROCEDURE IF EXISTS dbo.sliceReports;
GO 

DROP TABLE IF EXISTS RobsDatabase.dbo.tmpReport;
CREATE TABLE RobsDatabase.dbo.tmpReport (
	report_id INT PRIMARY KEY,
	location_id INT,
	result_date SMALLDATETIME,
	result_time CHAR(5),
	patient_id INT,
	requester_id INT,
	discipline_id INT,
	abnormal BIT,
	INDEX X_location_id (location_id),
	INDEX X_patient_id (patient_id),
	INDEX X_requester_id (requester_id),
	INDEX X_discipline_id (discipline_id)
);
GO

CREATE PROCEDURE dbo.sliceReports @maxReportId INT, @size INT as
BEGIN
	DELETE FROM RobsDatabase.dbo.tmpReport;
	INSERT INTO RobsDatabase.dbo.tmpReport
	SELECT 
		r.report_id,
		r.location_id,
		r.result_date,
		r.result_time,
		r.patient_id,
		r.requester_id,
		r.discipline_id,
		r.abnormal
	FROM ordercomms_review.dbo.report r WITH (INDEX(PK_report_reportid))
		INNER JOIN RobsDatabase.dbo.tsftInpatientLocations loc on r.location_id = loc.location_id
	WHERE r.report_id <= @maxReportId 
	and r.report_id > @maxReportId-@size
	and r.amended=0
	and r.result_date IS NOT NULL
	and r.result_time IS NOT NULL;
END
GO

-- EXEC sliceReports 41308775,100000;

-- https://stackoverflow.com/questions/1602244/batch-commit-on-large-insert-operation-in-native-sql
-- batch insertion answer.

DECLARE @minReportId INT;
DECLARE @maxReportId INT;
DECLARE @date datetime;
DECLARE @message nvarchar(max);
DECLARE @size int;
SET @size = 100000;
SET @minReportId = (SELECT TOP 1 report_id FROM ordercomms_review.dbo.report order by report_id ASC);
SET @maxReportId = (SELECT TOP 1 internal_id FROM RobsDatabase.[dbo].[tsftResult] order by internal_id ASC);
SET @maxReportId = IIF(@maxReportId IS NULL, (SELECT TOP 1 report_id FROM ordercomms_review.dbo.report order by report_id DESC) , @maxReportId);
SET @message = cast(@maxReportId as nvarchar(max))+' (target: '+cast(@minReportId as nvarchar(max))+')';
RAISERROR(@message,0,0) WITH NOWAIT;

WHILE @minReportId <= @maxReportId
BEGIN
	EXEC RobsDatabase.dbo.sliceReports @maxReportId,@size;

	INSERT INTO RobsDatabase.[dbo].[tsftResult]
	SELECT
	-- SELECT TOP(1000)
			r.report_id as internal_id,
			r.location_id,
			r.result_date+r.result_time as date,
			CONVERT(binary(32),hashbytes('SHA2_256',tmp.salt+CONVERT(nvarchar(4000),r.patient_id))) as patient_id,
			r.requester_id as clinician_id,
			 
			cast(rqs.src_comments as nvarchar(max)) as src_comments,
			d.code as discipline,
			d.name as discipline_name,
			
			-- if no battery but there is a test we use that instead
			IIF(b.battery_id IS NULL, ts.test_id, b.battery_id) as investigation_id,
			IIF(b.from_code IS NULL, ts.from_code, b.from_code) as investigation,
			IIF(b.original_display_name IS NULL, ts.original_display_name, b.original_display_name) as investigation_name,
			r.abnormal as investigation_abnormal,
			
			s.sample_id,
			s.from_code as sample,

			ts.test_id as test_id,
			ts.from_code as test,
			ts.original_display_name as test_name,
			IIF(rqt.abnormal IS NULL,0,rqt.abnormal) as test_abnormal,
			
			-- b.subtype_id as test_discipline,
			cast(rqt.textual_result as nvarchar(max)) as textual_result,
			rqt.numeric_result,
			ts.unit,
			rqt.normalcy,
			ts.low_range,
			ts.high_range

		FROM 
		(Select CONVERT(char(36),uuid) as salt from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id)) tmp,
		 RobsDatabase.dbo.tmpReport r
			INNER JOIN ordercomms_review.dbo.rsample rqs ON r.report_id = rqs.report_id
			LEFT OUTER JOIN ordercomms_review.dbo.rbattery rqb on rqs.rsample_id = rqb.rsample_id
			INNER JOIN ordercomms_review.dbo.sampleSynonym s ON rqs.sampleSynonym_id = s.sampleSynonym_id
			INNER JOIN ordercomms_review.dbo.batterySynonym b ON rqb.batterySynonym_id = b.batterySynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.rtest rqt on rqs.rsample_id = rqt.rsample_id and rqt.rbattery_id = rqb.rbattery_id
			INNER JOIN ordercomms_review.dbo.testSynonym ts on rqt.testsynonym_id = ts.testSynonym_id
			INNER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
		-- ORDER BY patient_id, date, investigation
		;

	INSERT INTO RobsDatabase.[dbo].[tsftResult]
	SELECT
			r.report_id as internal_id,
			r.location_id,
			r.result_date+r.result_time as request_date,
			CONVERT(binary(32),hashbytes('SHA2_256',tmp.salt+CONVERT(nvarchar(4000),r.patient_id))) as patient_id,
			r.requester_id as clinician_id, 
			
			cast('' as nvarchar(max)) as src_comments,
			d.code as discipline,
			d.name as discipline_name,
			
			-- test and investigation are the same for radiology
			ns.npi_id as investigation_id,
			ns.from_code as investigation,
			ns.original_display_name as investigation_name,
			r.abnormal as investigation_abnormal,

			NULL as sample_id,
			NULL as sample_code,
			
			ns.npi_id as test_id,
			ns.from_code as test,
			ns.original_display_name as test_name,
			r.abnormal as test_abnormal,

			-- b.subtype_id as test_discipline,
			cast(rn.text as nvarchar(max)) as textual_result,
			NULL as numeric_result,
			NULL as unit,
			NULL as normalcy,
			NULL as low_range,
			NULL as high_range
			
		FROM 
		(Select CONVERT(char(36),uuid) as salt from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id)) tmp,
		RobsDatabase.dbo.tmpReport r
			INNER JOIN ordercomms_review.dbo.rnpi rn ON r.report_id = rn.report_id
			INNER JOIN ordercomms_review.dbo.npisynonym ns on rn.npisynonym_id = ns.npisynonym_id
			INNER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
		;

		SET @date = (SELECT TOP 1 date from RobsDatabase.[dbo].[tsftResult] order by internal_id asc);
		SET @maxReportId = @maxReportId-@size;
		SET @message = cast(@maxReportId as nvarchar(max))+' (target: '+cast(@minReportId as nvarchar(max))+') '+cast(@date as nvarchar(max));
		RAISERROR(@message,0,0) WITH NOWAIT;
		RAISERROR('end of loop',0,0) WITH NOWAIT;
	END
;
GO


/*-- FOR EXAMPLE
SELECT DISTINCT * FROM (
SELECT TOP 200 src_comments FROM RobsDatabase.dbo.tsftRequest order by request_date desc
) tmp
*/



DROP TABLE IF EXISTS RobsDatabase.[dbo].[tsftOrderedChemHaemResult];

CREATE TABLE RobsDatabase.[dbo].[tsftOrderedChemHaemResult](
	[sequence_no] [int] NOT NULL,
	[internal_id] [int] NOT NULL,
	[location_id] [int] NOT NULL,
	[date] [smalldatetime] NOT NULL,
	[patient_id] [binary](32) NOT NULL,
	[clinician_id] [int] NOT NULL,
	
	[src_comments] [nvarchar](max) NULL,
	[discipline] [varchar](70) NULL,
	[discipline_name] [varchar](70) NULL,

	[investigation_id] [int] NULL,
	[investigation] [varchar](70) NULL,
	[investigation_name] [varchar](70) NULL,
	[investigation_abnormal] [tinyint] NULL,

	[sample_id] [int] NULL,
	[sample] [varchar](70) NULL,
	
	[test_id] [int] NULL,
	[test] [varchar](70) NULL,
	[test_name] [varchar](70) NULL,
	[test_abnormal] [tinyint] NULL,

	[textual_result] [nvarchar](max) NULL,
	[numeric_result] [varchar](25) NULL,
	[unit] [varchar](25) NULL,
	[normalcy] [varchar](25) NULL,
	[low_range] [varchar](25) NULL,
	[high_range] [varchar](25) NULL,
	[last_test_normalcy] [varchar](25) NULL,

	INDEX X_sequence_no (sequence_no),
	INDEX X_internal_id (internal_id),
	INDEX X_location_id (location_id),
	INDEX X_patient_id (patient_id),
	INDEX X_clinician_id (clinician_id),
	INDEX X_investigation (investigation),
	INDEX X_test (test),
	INDEX X_test_normalcy (test,normalcy),
	INDEX X_last_test_normalcy (last_test_normalcy)
) ON [PRIMARY];


CREATE CLUSTERED INDEX X_result_date ON RobsDatabase.[dbo].tsftOrderedChemHaemResult (date);
GO

INSERT INTO tsftOrderedChemHaemResult 
SELECT
	ROW_NUMBER() OVER (
		PARTITION BY patient_id,test,investigation ORDER BY date asc
	) as sequence_no,
	*,
	NULL as last_test_normalcy
FROM
	tsftResult
WHERE discipline = 'C/H' 
and numeric_result IS NOT NULL
and date <= '20170930'
and date >= '20121001'
;
GO

UPDATE r1 SET
r1.last_test_normalcy = r2.normalcy
FROM tsftOrderedChemHaemResult r1, tsftOrderedChemHaemResult r2 
WHERE r2.sequence_no+1 = r1.sequence_no
AND r1.patient_id = r2.patient_id
AND r1.test = r2.test
AND r1.investigation = r2.investigation;
GO

ALTER DATABASE RobsDatabase
SET MULTI_USER;
GO