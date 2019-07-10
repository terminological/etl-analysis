
USE ordercomms_review;

drop table if exists RobsDatabase.dbo.tsftLocations;

create table RobsDatabase.dbo.tsftLocations (
	[location_id] [int] NOT NULL PRIMARY KEY,
	[name] [varchar](70) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	synonyms varchar(400) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[code] [varchar](70)  COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL INDEX FK_code,
	[inpatient] [bit] NOT NULL
);

-- create a table of MPH locations.
-- MPH locations have MPH or RBA in the code or the name
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;

Insert into RobsDatabase.dbo.tsftLocations
Select distinct tmp.* FROM (
	Select distinct
			l.location_id,
			l.name,
			'' as synonyms,
			l.code,
			IIF(
				l.name like '%OPD%' OR code like '%OPD%' 
				OR l.name like '%T\T\O OP%' OR code like '%T\T\O OP%'
				OR l.name like '%POAC%' OR code like '%POAC%'
				OR l.name like '%MEDICAL SECRETARY%'
				OR l.name like '%COMM%' OR code like '%COMM%'
				OR l.name like '%Outpatient%'
				OR l.name like '%Clinic%'
				OR l.name like '%GP%SERVICE%'
			,0,1) as inpatient
		from ordercomms_review.dbo.location l,
		ordercomms_review.dbo.report r
		where (l.code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR l.code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR l.name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR l.name like '%RBA%' COLLATE Latin1_General_CS_AS)
		AND NOT l.code='ZMPH'
		AND NOT l.code like '%RBA-BCH%'
		AND NOT l.name like '%STAFF OCC%'
		AND r.location_id = l.location_id
		and r.result_date > '20160101'
UNION
	Select distinct
			l.location_id,
			l.name,
			'' as synonyms,
			l.code,
			IIF(
				l.name like '%OPD%' OR code like '%OPD%' 
				OR l.name like '%T\T\O OP%' OR code like '%T\T\O OP%'
				OR l.name like '%POAC%' OR code like '%POAC%'
				OR l.name like '%MEDICAL SECRETARY%'
				OR l.name like '%COMM%' OR code like '%COMM%'
				OR l.name like '%Outpatient%'
				OR l.name like '%Clinic%'
				OR l.name like '%GP%SERVICE%'
			,0,1) as inpatient
		from ordercomms_review.dbo.location l,
		ordercomms_review.dbo.request r
		where (l.code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR l.code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR l.name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR l.name like '%RBA%' COLLATE Latin1_General_CS_AS)
		AND NOT l.code='ZMPH'
		AND NOT l.code like '%RBA-BCH%'
		AND NOT l.name like '%STAFF OCC%'
		AND r.location_id = l.location_id
		and r.request_date > '20160101'
	) as tmp
	;


update RobsDatabase.dbo.tsftLocations
SET synonyms = tmp2.synonyms
from (
SELECT 
	tmp.location_id,
	string_agg(tmp.name,', ') as synonyms,
	tmp.code
FROM
(select distinct
	tl.location_id,
	l.name,
	l.code
from 
	ordercomms_review.dbo.location l,
	RobsDatabase.dbo.tsftLocations tl
where l.code = tl.code) as tmp
group by tmp.code, tmp.location_id) as tmp2,
RobsDatabase.dbo.tsftLocations tl2
where tl2.location_id = tmp2.location_id

select * from RobsDatabase.dbo.tsftLocations;