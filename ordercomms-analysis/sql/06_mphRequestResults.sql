-- GENERATES a cut down view of the results looking only at 
-- the battery level of information.
-- This is the level at which the main time_to_view analysis will be done

Use ordercomms_review;
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

DROP TABLE IF EXISTS RobsDatabase.[dbo].[tsftRequestedTest];

CREATE TABLE RobsDatabase.[dbo].[tsftRequestedTest] (
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

	INDEX X_internal_id (internal_id),
	-- INDEX X_date (date),
	INDEX X_location_id (location_id),
	INDEX X_patient_id (patient_id),
	INDEX X_clinician_id (clinician_id),
	INDEX X_investigation (investigation)
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX X_requested_test_date ON  RobsDatabase.[dbo].tsftRequestedTest (date);
GO

INSERT INTO RobsDatabase.[dbo].[tsftRequestedTest]
SELECT DISTINCT
	r.internal_id,
	r.location_id,
	r.date,
	r.patient_id,
	r.clinician_id,
	r.src_comments,
	r.discipline,
	r.discipline_name,
	r.investigation_id,
	r.investigation,
	r.investigation_name,
	r.investigation_abnormal
FROM RobsDatabase.dbo.tsftResult r


-- SELECT TOP 1000 * FROM RobsDatabase.[dbo].[tsftRequestedTest]

/*
IN THEORY THIS SHOULD CREATE THE tsftRequestedTest table
Without generating the intermediate tsftResults table
In practice it is very slow as the join to the testSynonymMatrix is going to be about 450M rows

INSERT INTO RobsDatabase.[dbo].[tsftRequestedTest]
SELECT DISTINCT
		r.report_id as internal_id,
		r.location_id,
		r.result_date+r.result_time as date,
		CONVERT(binary(32),hashbytes('SHA2_256',tmp.salt+CONVERT(nvarchar(4000),r.patient_id))) as patient_id,
		r.requester_id as clinician_id,
			 
		cast(rqs.src_comments as nvarchar(max)) as src_comments,
		convert(varchar(70), d.code) as discipline,
		convert(varchar(70), d.name) as discipline_name,
			
		-- if no battery but there is a test we use that instead
		IIF(b.battery_id IS NULL, ts.test_id, b.battery_id) as investigation_id,
		IIF(b.from_code IS NULL, ts.from_code, b.from_code) as investigation,
		IIF(b.original_display_name IS NULL, ts.original_display_name, b.original_display_name) as investigation_name,
		r.abnormal as investigation_abnormal

	FROM 
	(Select CONVERT(char(36),uuid) as salt from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id)) tmp,
	RobsDatabase.dbo.tsftInpatientLocations loc,
	ordercomms_review.dbo.Report r
		INNER JOIN ordercomms_review.dbo.rsample rqs ON r.report_id = rqs.report_id
		LEFT OUTER JOIN ordercomms_review.dbo.rbattery rqb on rqs.rsample_id = rqb.rsample_id
		INNER JOIN ordercomms_review.dbo.sampleSynonym s ON rqs.sampleSynonym_id = s.sampleSynonym_id
		INNER JOIN ordercomms_review.dbo.batterySynonym b ON rqb.batterySynonym_id = b.batterySynonym_id
		INNER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
		LEFT OUTER JOIN ordercomms_review.dbo.reportTestSynonymMatrix rtsm on rtsm.report_id = r.report_id
		INNER JOIN ordercomms_review.dbo.testSynonym ts on rtsm.testSynonym_id = ts.testSynonym_id
	WHERE 
		r.location_id = loc.location_id
		and r.amended=0
		and r.result_date IS NOT NULL
		and r.result_time IS NOT NULL
	;

INSERT INTO RobsDatabase.[dbo].[tsftResult]
SELECT DISTINCT
		r.report_id as internal_id,
		r.location_id,
		r.result_date+r.result_time as request_date,
		CONVERT(binary(32),hashbytes('SHA2_256',tmp.salt+CONVERT(nvarchar(4000),r.patient_id))) as patient_id,
		r.requester_id as clinician_id, 
			
		cast('' as nvarchar(max)) as src_comments,
		convert(varchar(70), d.code) as discipline,
		convert(varchar(70), d.name) as discipline_name,
			
		-- test and investigation are the same for radiology
		ns.npi_id as investigation_id,
		ns.from_code as investigation,
		ns.original_display_name as investigation_name,
		r.abnormal as investigation_abnormal
			
	FROM 
	(Select CONVERT(char(36),uuid) as salt from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id)) tmp,
	RobsDatabase.dbo.tsftInpatientLocations loc,
	ordercomms_review.dbo.Report r
		INNER JOIN ordercomms_review.dbo.rnpi rn ON r.report_id = rn.report_id
		INNER JOIN ordercomms_review.dbo.npisynonym ns on rn.npisynonym_id = ns.npisynonym_id
		INNER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
	WHERE 
		r.location_id = loc.location_id
		and r.amended=0
		and r.result_date IS NOT NULL
		and r.result_time IS NOT NULL
	;
*/




