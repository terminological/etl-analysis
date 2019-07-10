USE ordercomms_review;

-- Pathology
SELECT DISTINCT a.battery_id
      ,a.from_code as battery_code
      ,a.batterySynonym_id
      ,a.original_display_name as battery_name
	  ,ts.subtype_id
	  ,ts.name
      ,t.test_id
	  ,t.from_code as test_code
	  ,t.testSynonym_id
	  ,t.original_display_name as test_name
	  ,t.low_range
	  ,t.high_range
	  ,t.unit
	  ,s.sampleSynonym_id
	  ,s.original_display_name as sample_name
FROM 
	dbo.batterySynonym a
	full outer join dbo.testSynonym t on t.battery_id = a.battery_id
	left outer join dbo.sampleSynonym s on s.sampleSynonym_id = t.sampleSynonym_id
	left outer join dbo.subtype ts on t.subtype_id = ts.subtype_id
ORDER BY battery_code

-- This 31K combinations of battery and test is what is possible, 

SELECT DISTINCT 
      a.from_code as battery_code,
      a.original_display_name as battery_name,
	  t.from_code as test_code,
	  t.original_display_name as test_name
FROM 
	dbo.batterySynonym a
	full outer join dbo.testSynonym t on t.battery_id = a.battery_id
ORDER BY battery_code
-- 22.9 K of possible combniations of battery and test (excluding subtype, sample, and reference range oddities
-- some tests do not have battery_codes - most notably histology

-- however:

select DISTINCT 
	b.battery_id, 
	b.from_code as battery_code,
	b.batterySynonym_id,
	b.original_display_name as battery_name,
	ts.subtype_id,
	ts.name,
	t.test_id,
	t.original_display_name as test_name,
	t.unit as unit,
	t.high_range as high_range,
	t.low_range as low_range,
	t.from_code as test_code,
	s.sample_id,
	s.name as sample_name
from 
	(select * from report where result_date > '20160101') rep
	inner join rtest rt on rt.report_id = rep.report_id
	left outer join testSynonym t on t.testSynonym_id = rt.testSynonym_id
	left outer join rbattery rb on rt.rbattery_id = rb.rbattery_id 
	left outer join batterySynonym b on rb.batterySynonym_id = b.batterySynonym_id 
	left outer join sample s on t.sample_id = s.sample_id
	left outer join subtype ts on t.subtype_id = ts.subtype_id
where t.subtype_id <> 7 -- exclude histology
ORDER BY battery_code;

-- gives us only 12936 battery-test-sample combinations that are actually used since 20160101

-- Radiology
select ts.name, n.* from npisynonym n, dbo.subtype ts where n.subtype_id = ts.subtype_id;

-- Samples
select * from sampleSynonym;


-- what is the effective catalogue of the test results?

select DISTINCT 
	b.battery_id, 
	min(b.original_display_name) as battery_name,
	min(b.from_code) as battery_code,
	t.test_id,
	min(t.original_display_name) as test_name,
	min(t.unit) as unit,
	min(t.high_range) as high_range,
	min(t.low_range) as low_range,
	min(t.from_code) as test_code,
	s.sample_id,
	min(s.name) as sample_name,
	count(rt.rtest_id) as frequency_per_million
from testSynonym t, 
	(select top(1000000) * from rtest order by rtest_id desc) rt, rbattery rb, batterySynonym b, sample s
where t.testSynonym_id = rt.testSynonym_id
and rt.rbattery_id = rb.rbattery_id
and rb.batterySynonym_id = b.batterySynonym_id
and t.sample_id = s.sample_id
GROUP BY b.battery_id, t.test_id, s.sample_id
ORDER BY battery_code, test_code, frequency_per_million DESC;

-- select count(*) from rtest; 318 639 679 test results.

