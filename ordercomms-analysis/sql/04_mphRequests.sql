-- COPY THE REQUESTS TABLE, FILTERING FOR INPATIENT RESULTS
-- AND ANONYMISING THE PATIENT ID

Use ordercomms_review;
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

DROP TABLE IF EXISTS RobsDatabase.[dbo].[tsftRequest];

CREATE TABLE RobsDatabase.[dbo].[tsftRequest] (
	[internal_id] [int] NOT NULL,
	[location_id] [int] NOT NULL,
	[date] [smalldatetime] NULL,
	[patient_id] [binary](32) NOT NULL,
	[clinician_id] [int] NULL,
	
	
	[status] [int] NOT NULL,
	[src_comments] [nvarchar](max) NULL,
	[discipline] [varchar](70) NULL,
	[discipline_name] [varchar](70) NULL,
	
	[investigation_id] [int] NULL,
	[investigation] [varchar](70) NULL,
	[investigation_name] [varchar](70) NULL,
	
	INDEX X_internal_id (internal_id),
	-- INDEX X_date (date),
	INDEX X_location_id (location_id),
	INDEX X_patient_id (patient_id),
	INDEX X_clinician_id (clinician_id),
	INDEX X_discipline (discipline),
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

CREATE CLUSTERED INDEX X_request_date ON RobsDatabase.[dbo].tsftRequest (date);
GO

DECLARE @salt char(36);
SET @salt = (Select CONVERT(char(36),uuid) from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id));

INSERT INTO RobsDatabase.dbo.tsftRequest
SELECT 
			r.request_id as internal_id,
			r.location_id,
			r.request_date+r.request_time as date,
			CONVERT(binary(32),hashbytes('SHA2_256',@salt+CONVERT(nvarchar(4000),r.patient_id))) as patient_id,
			r.initiating_requester_id as clinician_id, 
			
			r.status, -- see requestStatusType for explanation of status
			cast(rqs.src_comments as nvarchar(max)) as src_comments,
			d.code as discipline,
			d.name as discipline_name,

			b.battery_id as investigation_id,
			b.from_code as investigation,
			b.original_display_name as investigation_name
			
		FROM 
		RobsDatabase.dbo.tsftInpatientLocations loc,
		ordercomms_review.dbo.request r
			INNER JOIN ordercomms_review.dbo.rqsample rqs ON r.request_id = rqs.request_id
			LEFT OUTER JOIN ordercomms_review.dbo.rqbattery rqb on rqs.rqsample_id = rqb.rqsample_id
			LEFT OUTER JOIN ordercomms_review.dbo.sampleSynonym s ON rqs.sampleSynonym_id = s.sampleSynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.batterySynonym b ON rqb.batterySynonym_id = b.batterySynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
			-- LEFT OUTER JOIN rqtest rqt on rqb.rqbattery_id = rqt.rqbattery_id
		where r.location_id = loc.location_id
		and amended=0;

INSERT INTO RobsDatabase.dbo.tsftRequest
SELECT 
			r.request_id as internal_id,
			r.location_id,
			r.request_date+r.request_time as date,
			CONVERT(binary(32),hashbytes('SHA2_256',@salt+CONVERT(nvarchar(4000),r.patient_id))) as patient_id,
			r.initiating_requester_id as clinician_id,
			
			r.status,
			cast(rqs.text as nvarchar(max)) as src_comments,
			d.code as discipline,
			d.name as discipline_name,

			ns.npi_id as investigation_id,
			ns.from_code as investigation,
			ns.original_display_name as invetigation_name
		FROM 
		RobsDatabase.dbo.tsftInpatientLocations loc,
		ordercomms_review.dbo.request r
			INNER JOIN ordercomms_review.dbo.rqnpi rqs ON r.request_id = rqs.request_id
			LEFT OUTER JOIN ordercomms_review.dbo.npisynonym ns on rqs.npisynonym_id = ns.npisynonym_id
			LEFT OUTER JOIN ordercomms_review.dbo.discipline d on r.discipline_id = d.discipline_id
		where r.location_id = loc.location_id
		and amended=0;
GO

-- FOR EXAMPLE
-- SELECT DISTINCT * FROM (
-- SELECT TOP 200 src_comments FROM RobsDatabase.dbo.tsftRequest order by request_date desc
-- ) tmp