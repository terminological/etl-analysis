-- VARIOUS EXPERIMENTS AROUND LOCATIONS AND PATIENT IDS


USE ordercomms_review;

create table RobsDatabase.dbo.tsftLocations (
	[location_id] [int] NOT NULL PRIMARY KEY,
	[name] [varchar](70) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[code] [varchar](70)  COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL INDEX FK_code,
	[inpatient] [bit] NOT NULL
);

-- create a table of MPH locations.
-- MPH locations have MPH or RBA in the code or the name
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;

Insert into RobsDatabase.dbo.tsftLocations 
	Select 
			location_id,
			name,
			code,
			IIF(
				name like '%OPD%' OR code like '%OPD%' 
				OR name like '%T\T\O OP%' OR code like '%T\T\O OP%'
				OR name like '%POAC%' OR code like '%POAC%'
				OR name like '%MEDICAL SECRETARY%'
				OR name like '%COMM%' OR code like '%COMM%'
				OR name like '%Outpatient%'
				OR name like '%Clinic%'
				OR name like '%GP%SERVICE%'
			,0,1) as inpatient
		from location
		where (code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS)
		AND NOT code='ZMPH'
		AND NOT code like '%RBA-BCH%'
		AND NOT name like '%STAFF OCC%'
	;

DECLARE @tsftLocationsTmp table(
	[location_id] [int] NOT NULL PRIMARY KEY,
	[name] [varchar](70) NOT NULL,
	[code] [varchar](70) NOT NULL INDEX FK_code,
	[inpatient] [bit] NOT NULL
);

-- Add in locations which use the same code as the locations identified
-- these are synonyms really
Insert into @tsftLocationsTmp 
	Select l.location_id, l.name, l2.code, l2.inpatient FROM 
		location l
		inner join (
			SELECT DISTINCT 
				code, 
				min(IIF(inpatient=1,1,0)) as inpatient 
			from RobsDatabase.dbo.tsftLocations group by code) l2 on l2.code = l.code
		left outer join RobsDatabase.dbo.tsftLocations l3 on l3.location_id = l.location_id
		where l3.location_id IS NULL;

Insert into RobsDatabase.dbo.tsftLocations Select * from @tsftLocationsTmp;

-- how do we abstract to a more rational representation of ward?
-- we need a mapping from code to a cleansed list of locations
Select distinct name,code,inpatient from RobsDatabase.dbo.tsftLocations order by code;

-- useful view of wards and various synonyms organised by code
Select distinct
		string_agg(tmp.name,', ') as name,
		tmp.code,
		min(iif(tmp.inpatient=1,1,0)) as inpatient
	from (
		SELECT DISTINCT
			name, 
			code, 
			inpatient
		FROM RobsDatabase.dbo.tsftLocations) as tmp
	group by code
	order by code;

-- but which clinician_ids are actually used by location (requests)
SELECT DISTINCT c.* --, loc.* 
FROM request r
left outer join clinician c on (c.clinician_id = r.initiating_requester_id OR c.clinician_id = r.requester_id )
inner join RobsDatabase.dbo.tsftLocations loc on r.location_id = loc.location_id 
-- WHERE 
--	r.request_date >= '20160701'
order by c.code;


-- which users review MPH tests?
SELECT DISTINCT u.*
FROM report r
left outer join reportViewedby rv on r.report_id = rv.report_id
left outer join rvUser u on rv.user_id = u.user_id
inner join RobsDatabase.dbo.tsftLocations loc on r.location_id = loc.location_id 
-- WHERE 
--	r.result_date >= '20160701' and r.active = 1
;

select * into RobsDatabase.dbo.tmpClinician from clinician;
select * into RobsDatabase.dbo.tmpClinicianSynonym from clinicianSynonym;
select * into RobsDatabase.dbo.tmpRvUser from rvUser;


select top 10 patient_id, hashbytes('SHA2_256', nhsno) from patient

SELECT top 10 HASHBYTES('SHA2_256',nhsno+patient_id) as id,*
  FROM [ordercomms_review].[dbo].[patient] where nhsno is not null;