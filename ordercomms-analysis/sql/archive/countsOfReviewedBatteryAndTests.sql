-- EARLY REVIEWED STATS FOR BATTERIES AND TESTS

-- reveiewd and unreviewed reports by battery type
select 
	b.from_code, 
	min(b.original_display_name) as battery_name,
	COUNT(*) as rows,
	-- COUNT(DISTINCT rb.rbattery_id) as total_requests,
	COUNT(DISTINCT r.report_id) as total_reports,
	SUM(IIF(r.viewed=1,1,0)) as reviewed,
	SUM(IIF(r.viewed=0,1,0)) as unreviewed,
	SUM(IIF(r.abnormal=0,1,0)) as normal,
	SUM(IIF(r.abnormal=1,1,0)) as abnormal,
	SUM(IIF(r.viewed=0 AND r.abnormal=1,1,0)) as abnormal_not_reviewed,
	100.0*SUM(IIF(r.viewed=0 AND r.abnormal=1,1,0))/COUNT(*) as abnormal_not_reviewed_percent
-- top(1000) *
from report r
	inner join (Select * from location
		where code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS) loc on r.location_id = loc.location_id
	inner join reportBatterySynonymMatrix rbsm on r.report_id = rbsm.report_id
	-- inner join rtest rt on rt.report_id=r.report_id
	-- inner join rbattery rb on rt.rbattery_id=rb.rbattery_id
	inner join batterySynonym b on rbsm.batterySynonym_id=b.batterySynonym_id
	-- left outer join test t on rt.test_id=t.test_id
	-- left outer join disciplineSynonym s on r.disciplineSynonym_id = s.disciplineSynonym_id
	-- left outer join discipline d on s.discipline_id = d.discipline_id
where r.active = 1 and r.result_date >= '20160701' and r.result_date < '20170701'
group by b.from_code order by abnormal_not_reviewed DESC;


-- reveiewd and unreviewed reports by battery type
select 
	ts.from_code, 
	min(ts.original_display_name) as test_name,
	COUNT(*) as rows,
	-- COUNT(DISTINCT rb.rbattery_id) as total_requests,
	COUNT(DISTINCT rt.rtest_id) as total_tests,
	SUM(IIF(r.viewed=1,1,0)) as reviewed,
	SUM(IIF(r.viewed=0,1,0)) as unreviewed,
	SUM(IIF(rt.abnormal=1,0,1)) as normal,
	SUM(IIF(rt.abnormal=1,1,0)) as abnormal,
	SUM(IIF(r.viewed=0 AND rt.abnormal=1,1,0)) as abnormal_not_reviewed,
	100.0*SUM(IIF(r.viewed=0 AND rt.abnormal=1,1,0))/COUNT(*) as abnormal_not_reviewed_percent
-- top(1000) *
from report r
	inner join (Select * from location
		where code like '%MPH%' COLLATE Latin1_General_CS_AS
		OR code like '%RBA%' COLLATE Latin1_General_CS_AS
		OR name like '%MPH%' COLLATE Latin1_General_CS_AS
		OR name like '%RBA%' COLLATE Latin1_General_CS_AS) loc on r.location_id = loc.location_id
	inner join rtest rt on rt.report_id=r.report_id
	left outer join testSynonym ts on rt.testsynonym_id=ts.testSynonym_id
	-- left outer join test t on ts.test_id=t.test_id
where r.active = 1 and r.result_date >= '20160701' and r.result_date < '20170701'
group by ts.from_code order by abnormal_not_reviewed DESC;
