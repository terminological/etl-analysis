-- distribution of tests and normal / abnormal
SELECT
	test,
	-- cast(numeric_result as float) as value,
	count(res.internal_id) as freq,
	cast(sum(iif(res.normalcy='Low',1,0)) as float)/count(res.internal_id)*100 as low_percent,
	cast(sum(iif(res.normalcy='High',1,0)) as float)/count(res.internal_id)*100 as high_percent
FROM
	tsftOrderedChemHaemResult res
group by test
order by freq desc
GO
