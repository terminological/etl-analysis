-- This is the correct logic but in implementation this query maxes out the server's 64Gb of memory
-- we have had to split this up into chunks

Use ordercomms_review;
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
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
	INDEX X_date (date),
	INDEX X_location_id (location_id),
	INDEX X_patient_id (patient_id),
	INDEX X_clinician_id (clinician_id),
	INDEX X_investigation (investigation),
	INDEX X_test (test),
	INDEX X_test_normalcy (test,normalcy),
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY];
GO

-- DECLARE @salt char(36);
-- SET @salt = (Select CONVERT(char(36),uuid) from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id));

-- https://stackoverflow.com/questions/1602244/batch-commit-on-large-insert-operation-in-native-sql
-- batch insertion answer.



Use RobsDatabase;
GO

IF OBJECT_ID('dbo.tmpResultView', 'V') IS NOT NULL
    DROP VIEW dbo.tmpResultView
GO

CREATE VIEW dbo.tmpResultView AS
SELECT
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
			rqt.abnormal as test_abnormal,
			
			-- b.subtype_id as test_discipline,
			cast(rqt.textual_result as nvarchar(max)) as textual_result,
			rqt.numeric_result,
			ts.unit,
			rqt.normalcy,
			ts.low_range,
			ts.high_range

		FROM 
		(Select CONVERT(char(36),uuid) as salt from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id)) tmp,
		RobsDatabase.dbo.tsftInpatientLocations loc
			INNER JOIN ordercomms_review.dbo.report r on r.location_id = loc.location_id
			INNER JOIN ordercomms_review.dbo.rsample rqs ON r.report_id = rqs.report_id
			LEFT OUTER JOIN ordercomms_review.dbo.rbattery rqb on rqs.rsample_id = rqb.rsample_id
			LEFT OUTER JOIN ordercomms_review.dbo.sampleSynonym s ON rqs.sampleSynonym_id = s.sampleSynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.batterySynonym b ON rqb.batterySynonym_id = b.batterySynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.rtest rqt on rqs.rsample_id = rqt.rsample_id
			LEFT OUTER JOIN ordercomms_review.dbo.testSynonym ts on rqt.testsynonym_id = ts.testSynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
		where r.amended=0
		and r.result_date IS NOT NULL
		and r.result_time IS NOT NULL
UNION
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
		RobsDatabase.dbo.tsftInpatientLocations loc 
			INNER JOIN ordercomms_review.dbo.report r ON r.location_id = loc.location_id
			INNER JOIN ordercomms_review.dbo.rnpi rn ON r.report_id = rn.report_id
			LEFT OUTER JOIN ordercomms_review.dbo.npisynonym ns on rn.npisynonym_id = ns.npisynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
		where amended=0
		and r.result_date IS NOT NULL
		and r.result_time IS NOT NULL
	;
GO

-- now execute 
-- bcp RobsDatabase.dbo.tmpResultView out C:\Users\robert.challen\tmpResultView.raw -T
-- bcp RobsDatabase.dbo.tsftResult in C:\Users\robert.challen\tmpResultView.raw -b 10000 -T

/*-- FOR EXAMPLE
SELECT DISTINCT * FROM (
SELECT TOP 200 src_comments FROM RobsDatabase.dbo.tsftRequest order by request_date desc
) tmp
*/



