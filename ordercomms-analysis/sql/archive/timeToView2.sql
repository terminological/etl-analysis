-- EARLY ATTEMPTS AT TIME TO VIEW

USE ordercomms_review;

-- create a table of MPH locations as a variable.
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;

DECLARE @tsftLocations table(
	[location_id] [int] NOT NULL PRIMARY KEY,
	[name] [varchar](70) NOT NULL,
	[code] [varchar](70) NOT NULL INDEX FK_code,
	[inpatient] [bit] NOT NULL
);

Insert into @tsftLocations 
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

Insert into @tsftLocationsTmp 
	Select l.location_id, l.name, l2.code, l2.inpatient FROM 
		location l
		inner join 
			(SELECT DISTINCT code, min(IIF(inpatient=1,1,0)) as inpatient from @tsftLocations group by code) l2 on l2.code = l.code
		left outer join @tsftLocations l3 on l3.location_id = l.location_id
		where l3.location_id IS NULL;

Insert into @tsftLocations Select * from @tsftLocationsTmp;


select
	count(*) as reports,
	avg(q2.min_time_to_view) as min_time_to_view,
	avg(q2.reviewers) as avg_reviewers,
	avg(avg_time_to_view) as avg_time_to_view,
	MIN(IIF(q2.inpatient=1,1,0)) as inpatient,
	q2.code as code,
	q2.discipline_id
FROM (
	select 
		q1.report_id,
		MIN(q1.time_to_review) as min_time_to_view,
		COUNT(*)*1.0 as reviewers,
		AVG(q1.time_to_review) as avg_time_to_view,
		MIN(IIF(q1.inpatient=1,1,0)) as inpatient,
		MIN(q1.code) as code,
		MIN(q1.discipline_id) as discipline_id
	FROM (
		select
			rv.report_id, 
			r.result_date+result_time as result_datetime,
			rv.viewed_date,
			DATEDIFF(second,(r.result_date+result_time),rv.viewed_date)/3600.0 as time_to_review,
			rv.user_id,
			rv.view_type,
			r.abnormal,
			loc.inpatient,
			loc.code,
			r.discipline_id
		FROM
			reportViewedby rv
			inner join report r on rv.report_id = r.report_id
			inner join @tsftLocations loc on r.location_id = loc.location_id
		WHERE result_date >= '20160701' and result_date < '20170701' AND active = 1 AND loc.inpatient = 1
	) as q1
	GROUP BY q1.report_id
) as q2
GROUP by q2.code, q2.discipline_id