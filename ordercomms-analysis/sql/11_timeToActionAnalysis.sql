/*
TIME TO ACTION ANALYSIS
TWO APPROACHES HERE
ONE IS TO LOOK AT ALL REQUESTS CONDUCTED IN A SHORT TIME FRAME AFTER A RESULT
OTHER IS TO LOOK AT REQUESTS MADE BY SAME CLINICIAN FOR SAME PATIENT
*/

USE [RobsDatabase]
GO

DROP TABLE IF EXISTS [dbo].[aggTimeToAction]
GO

SELECT -- TOP(100)
	res.internal_id,
	res.date,
	res.investigation,
	res.investigation_name,
	res.test,
	res.test_name,
	res.numeric_result,
	res.unit,
	res.normalcy,
	res.low_range,
	res.high_range,
	loc.ward_name,
	loc.dependency_level,
	loc.patient_group,
	YEAR(res.date)-p.year_of_birth as patient_age,
	p.sex as patient_gender,
	v.viewed_date,
	DATEDIFF(mi,res.date,v.viewed_date) as minutes_to_view, 
	-- map.similarity,
	req.date as request_date,
	DATEDIFF(mi,res.date,req.date) as minutes_to_action, 
	DATEDIFF(mi,v.viewed_date,req.date) as minutes_to_response,
	req.discipline as request_discipline,
	req.investigation as request_investigation,
	cast(0.0 as float) as chi_2
INTO aggTimeToAction
FROM
	tsftOrderedChemHaemResult res
	INNER JOIN tsftInpatientLocations loc ON  loc.location_id = res.location_id
	INNER JOIN tsftUniquePatientIndex p ON res.patient_id = p.patient_id,
	tsftUniqueResultViews v,
	tsftNameMap map,
	tsftRequest req
Where 
	res.internal_id = v.report_id
	and v.user_id = map.user_id
	and map.clinician_id = req.clinician_id
	and res.patient_id = req.patient_id
	AND res.normalcy IS NOT NULL
	AND res.last_test_normalcy IS NULL
	AND res.date-0.25 < req.date -- request less than 6 hours before result
	AND v.viewed_date+0.25 > req.date -- request less than 6 hours after viewed_date
	AND v.viewed_date-0.5 < req.date  -- viewed_date less than 6 hours after result
GO

DROP VIEW IF EXISTS chi_squared_test_investigation;
GO

CREATE VIEW chi_squared_test_investigation AS
SELECT test, test_name, normalcy, request_investigation, test_and_investigation,
	POWER((test_and_investigation-expected_test_and_investigation),2)/expected_test_and_investigation +
	POWER((test_and_no_investigation-expected_test_and_no_investigation),2)/expected_test_and_no_investigation +
	POWER((no_test_and_investigation-expected_no_test_and_investigation),2)/expected_no_test_and_investigation +
	POWER((no_test_and_no_investigation-expected_no_test_and_no_investigation),2)/expected_no_test_and_no_investigation as chi_2
	FROM (
SELECT 
	corr.test,
	corr.test_name,
	corr.normalcy,
	corr.request_investigation,
	corr.observed as test_and_investigation,
	res.observed-corr.observed as test_and_no_investigation,
	req.observed-corr.observed as no_test_and_investigation,
	a.all_combinations-res.observed-req.observed+corr.observed as no_test_and_no_investigation,
	cast(res.observed*req.observed as float)/all_combinations as expected_test_and_investigation,
	cast(res.observed*(all_combinations-req.observed) as float)/all_combinations as expected_test_and_no_investigation,
	cast((all_combinations-res.observed)*req.observed as float)/all_combinations as expected_no_test_and_investigation,
	cast((all_combinations-res.observed)*(all_combinations-req.observed) as float)/all_combinations as expected_no_test_and_no_investigation
	--,
	-- res.observed as total_test_observed,
	-- req.observed as total_investigation_observed,
	-- cast(res.observed as float)/i.distinct_investigations as investigation_expected_given_test,
	-- cast(req.observed as float)/t.distinct_tests as test_expected_given_investigation,
	
FROM
(Select test, min(test_name) as test_name, normalcy, request_investigation, cast(count(*) as BIGINT) as observed from aggTimeToAction
	where request_date>viewed_date group by test, normalcy, request_investigation) corr
LEFT OUTER JOIN 
(Select test, normalcy, cast(count(*) AS BIGINT) as observed from aggTimeToAction group by test, normalcy) res 
ON res.test = corr.test AND res.normalcy = corr.normalcy
LEFT OUTER JOIN
(Select request_investigation, cast(count(*) as BIGINT) as observed from aggTimeToAction group by request_investigation) req
ON req.request_investigation = corr.request_investigation,
(select cast(count(distinct request_investigation) as BIGINT) as distinct_investigations from  aggTimeToAction) i,
(select cast(count(distinct concat(test,normalcy)) as BIGINT) as distinct_tests  from  aggTimeToAction) t,
(select cast(count(*) AS BIGINT) as all_combinations from aggTimeToAction) a
) tmp
GO


UPDATE a SET a.chi_2=c.chi_2
FROM aggTimeToAction a, chi_squared_test_investigation c
WHERE a.test=c.test and a.normalcy=c.normalcy and a.request_investigation = c.request_investigation


SELECT * from aggTimeToAction where chi_2>11