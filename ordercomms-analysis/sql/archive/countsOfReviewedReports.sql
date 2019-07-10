-- EARLY ATTEMPTS AT PERCENT TESTS VIEWED BEFORE FORMAL LOCATIONS AVAILABLE


-- Number of reports reviewed and not reviewed
SELECT
	COUNT(*) as total,
	SUM(IIF(r.viewed=1,1,0)) as reviewed,
	SUM(IIF(r.viewed=0,1,0)) as not_reviewed
FROM
	report r
	inner join (Select * from location
		where code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS) loc on r.location_id = loc.location_id
Where r.result_date >= '20160701' and r.result_date < '20170701' AND r.active = 1
;

-- Number of reports reviewed by location
SELECT
	loc.code,
	min(loc.name) as location_name,
	COUNT(*) as total,
	SUM(IIF(r.viewed=1,1,0)) as reviewed,
	SUM(IIF(r.viewed=1,0,1)) as not_reviewed
	-- SUM(IIF(rv.num_views IS NULL,0,1)) as reviewed_detail,
	-- SUM(IIF(rv.num_views IS NULL,1,0))  as not_reviewed_detail
FROM
	report r
	inner join (Select * from location
		where code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS) loc on r.location_id = loc.location_id
	-- left outer join 
	--	(SELECT report_id, count(user_id) as num_views from reportViewedby group by report_id) rv on r.report_id = rv.report_id
Where r.result_date >= '20160701' and r.result_date < '20170701' AND r.active = 1
GROUP BY loc.code
ORDER BY location_name;

-- Number of test results broken down to path versus rad, reviewed and not reviewed
SELECT
	COUNT(*) as total,
	SUM(IIF(rn.rnpi_id IS NULL,0,1)) as rad,
	SUM(IIF(rt.rtest_id IS NULL,0,1)) as path,
	SUM(IIF(rn.rnpi_id IS NOT NULL AND r.viewed=1,1,0)) as rad_reviewed,
	SUM(IIF(rn.rnpi_id IS NOT NULL AND r.viewed=0,1,0)) as rad_not_reviewed,
	SUM(IIF(rt.rtest_id IS NOT NULL AND r.viewed=1,1,0)) as lab_reviewed,
	SUM(IIF(rt.rtest_id IS NOT NULL AND r.viewed=0,1,0)) as lab_not_reviewed
FROM
	report r
	inner join (Select * from location
		where code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS) loc on r.location_id = loc.location_id
	left join rtest rt on rt.report_id=r.report_id
	left join rnpi rn on rn.report_id=r.report_id
Where r.result_date >= '20160701' and r.result_date < '20170701' AND r.active = 1
;

-- Number of test results reviewed by location
-- Some locations only have path tests, some only have rad tests
-- must be the two seperate feeder systems using different
-- location codes.
SELECT
	loc.code,
	min(loc.name) as location_name,
	COUNT(*) as total,
	SUM(IIF(rn.rnpi_id IS NULL,0,1)) as rad,
	SUM(IIF(rt.rtest_id IS NULL,0,1)) as path,
	SUM(IIF(r.viewed=1,1,0)) as reviewed,
	SUM(IIF(r.viewed=1,0,1)) as not_reviewed
	-- SUM(IIF(rv.num_views IS NULL,0,1)) as reviewed_detail,
	-- SUM(IIF(rv.num_views IS NULL,1,0))  as not_reviewed_detail
FROM
	report r
	inner join (Select * from location
		where code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS) loc on r.location_id = loc.location_id
	full join rtest rt on rt.report_id=r.report_id
	full join rnpi rn on rn.report_id=r.report_id
	-- left outer join 
	--	(SELECT report_id, count(user_id) as num_views from reportViewedby group by report_id) rv on r.report_id = rv.report_id
Where r.result_date >= '20160701' and r.result_date < '20170701' AND r.active = 1
GROUP BY loc.code
ORDER BY location_name;

-- reveiewd and unreviewed reports by discipline
select 
	r.disciplineSynonym_id, 
	min(d.name) as discipline_name,
	COUNT(r.report_id) as total,
	SUM(IIF(r.viewed=1,1,0)) as reviewed,
	SUM(IIF(r.viewed=0,1,0)) as unreviewed
from report r
	inner join (Select * from location
		where code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS) loc on r.location_id = loc.location_id
	full join rtest rt on rt.report_id=r.report_id
	full join rnpi rn on rn.report_id=r.report_id
	left outer join disciplineSynonym s on r.disciplineSynonym_id = s.disciplineSynonym_id
	left outer join discipline d on s.discipline_id = d.discipline_id
where r.active = 1 and r.result_date >= '20160701' and r.result_date < '20170701'
group by r.disciplineSynonym_id;



/*
SELECT 
	-- min(c.clinician_id) as clinician_id, 
	-- min(c.name) as clinician_name,
	-- c.code,
	loc.code as location,
	COUNT(r.report_id) as reports,
	SUM(IIF(rv.num_views IS NULL,1,0)) as never_viewed,
	SUM(IIF(rv.num_views IS NOT NULL,1,0)) as viewed
FROM 
report r
left outer join 
	(SELECT report_id, count(user_id) as num_views from reportViewedby group by report_id) rv on r.report_id = rv.report_id
inner join (SELECT DISTINCT 
	l.location_id,
	l.code
FROM 
	location l, 
	locationSynonym l2
WHERE 
	l.code = l2.from_code
	AND (
		l2.from_code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR l2.original_display_name like '%RBA%' COLLATE Latin1_General_CS_AS
		OR l2.from_code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR l2.original_display_name like '%RBA%' COLLATE Latin1_General_CS_AS
	)
) loc on r.location_id = loc.location_id
WHERE 
r.result_date >= '20160701' and r.result_date < '20170701'
GROUP BY loc.code
order by location, reports desc
*/