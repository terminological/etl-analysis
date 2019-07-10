Use RobsDatabase;
GO

/*

THIS IS DEPRECATED
THE SAME VIEWS CAN BE CONSTRUCTED EASIER IN R
AND FOR THIS SCALE DATASET (3.5M) THERE IS NO ISSUE WITH SIZE

THIS APPROACH MAY BE NEEDED IF WE ARE REPORTNG INTO A DIFFERENT ENVIRONMENT
OR FOR TEST RESULTS WHERE THERE ARE A LOT MORE

*/


CREATE VIEW viewTestViewedStatusAndNormalityByMonth AS
-- tests requested and viewed by month and abnormality
select 
	tmp.year,
	tmp.month,
	tmp.test_numbers,
	tmp.test_normal/tmp.test_numbers*100 as percent_normal,
	tmp.test_abnormal/tmp.test_numbers*100 as percent_abnormal,
	tmp.test_viewed/tmp.test_numbers*100 as percent_viewed,
	tmp.test_not_viewed/tmp.test_numbers*100 as percent_not_viewed,
	-- tmp.test_normal_and_not_viewed/tmp.test_not_viewed*100 as percent_not_viewed_that_are_normal,
	tmp.test_normal_and_not_viewed,
	tmp.test_abnormal_and_not_viewed,
	tmp.average_views_per_test,
	tmp.average_views_per_test-1.96*tmp.std_views_per_test/sqrt(tmp.test_numbers) as low_ci,
	tmp.average_views_per_test+1.96*tmp.std_views_per_test/sqrt(tmp.test_numbers) as high_ci
from (
	select 
		DATEPART(YEAR,date) as year,
		DATEPART(MONTH,date) as month,
		cast(count(internal_id) as float) as test_numbers,
		cast(sum(iif(total_views > 0,1,0)) as float) as test_viewed,
		cast(sum(iif(total_views = 0,1,0)) as float) as test_not_viewed,
		cast(sum(iif(investigation_abnormal = 0,1,0)) as float) as test_normal,
		cast(sum(iif(investigation_abnormal = 1,1,0)) as float) as test_abnormal,
		cast(sum(iif(total_views > 0 and investigation_abnormal = 0,1,0)) as float) as test_normal_and_viewed,
		cast(sum(iif(total_views = 0 and investigation_abnormal = 0,1,0)) as float) as test_normal_and_not_viewed,
		cast(sum(iif(total_views > 0 and investigation_abnormal = 1,1,0)) as float) as test_abnormal_and_viewed,
		cast(sum(iif(total_views = 0 and investigation_abnormal = 1,1,0)) as float) as test_abnormal_and_not_viewed,
		avg(cast(total_views as float)) as average_views_per_test,
		stdev(cast(total_views as float)) as std_views_per_test
	from aggTimeToView
		where discipline_name <> 'Radiology'
	group by
		DATEPART(YEAR,date),
		DATEPART(MONTH,date)
) tmp
-- ORDER BY year,month
;
GO

CREATE VIEW viewCumulativeTestViewsByHourFromResult AS
-- cumulative probability of test viewed by age of test in first week
SELECT 
	tmp2.*,
	cast(tmp2.cumulative as float)/total.items as percent_viewed
FROM (
	SELECT 
		tmp.*,
		SUM(tmp.tests_viewed) OVER(ORDER BY tmp.hours_from_result ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cumulative
	FROM (
		SELECT 
			cast(a.minutes_to_view/60 as int) as hours_from_result,
			count(a.internal_id) as tests_viewed
		FROM 
			aggTimeToView a
		WHERE a.minutes_to_view IS NOT NULL
		and a.minutes_to_view < 60*24*7
		GROUP BY
			cast(a.minutes_to_view/60 as int)
	) tmp
) tmp2,
(select count(*) as items from aggTimeToView) total
-- ORDER BY hours_from_result ASC;
GO

CREATE VIEW viewCumulativeTestViewsByMinuteFromResult AS
-- cumulative probability of test viewed by age of test in first 8 hours
SELECT 
	tmp2.*,
	cast(tmp2.cumulative as float)/total.items*100 as percent_viewed
FROM (
	SELECT 
		tmp.*,
		SUM(tmp.tests_viewed) OVER(ORDER BY tmp.minutes_to_view ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cumulative
	FROM (
		SELECT 
			a.minutes_to_view,
			count(a.internal_id) as tests_viewed
		FROM 
			aggTimeToView a
		WHERE a.minutes_to_view IS NOT NULL
			and a.minutes_to_view < 60*8 and a.minutes_to_view >= 0
		GROUP BY
			a.minutes_to_view
	) tmp
) tmp2,
(select count(*) as items from aggTimeToView) total
-- ORDER BY minutes_to_view ASC;
GO

/*
CREATE PROCEDURE procCumulativeTestViewsByHourFromResultAndWard AS
-- cumulative probability of test viewed by age of test by ward
BEGIN
DECLARE @cols  AS NVARCHAR(MAX)='';
SELECT @cols = @cols + QUOTENAME(ward_name) + ',' FROM (select distinct ward_name from aggTimeToView ) as tmp;
select @cols = substring(@cols, 0, len(@cols)) --trim "," at end
DECLARE @query AS NVARCHAR(MAX)='';
Set @query = 
'select * from (
		SELECT 
			cast(a.minutes_to_view/60 as int) as hours,
			a.ward_name as [ward_name],
			--count(a.internal_id) as viewed
			--max(b.tot) as viewed
			cast(count(a.internal_id) as float)/max(b.tot)*100 as viewed
		FROM 
			aggTimeToView a INNER JOIN
			(select ward_name, count(internal_id) as tot from aggTimeToView where total_views > 0 group by ward_name) b on a.ward_name=b.ward_name
		where a.minutes_to_view IS NOT NULL
		GROUP BY
			cast(a.minutes_to_view/60 as int), a.ward_name
	) tmp
PIVOT 
(
	max(viewed)
	for [ward_name] in ('+@cols+')
) as piv
order by hours asc;';
execute(@query);
END


CREATE VIEW viewCumulativeTestViewsByHourFromResultAndWard AS
EXEC procCumulativeTestViewsByHourFromResultAndWard();
GO
*/
/*
select * from (
		SELECT 
			cast(a.minutes_to_view/60 as int) as hours,
			a.ward_name as [ward_name],
			--count(a.internal_id) as viewed
			--max(b.tot) as viewed
			cast(count(a.internal_id) as float)/max(b.tot)*100 as viewed
		FROM 
			aggTimeToView a INNER JOIN
			(select ward_name, count(internal_id) as tot from aggTimeToView group by ward_name) b on a.ward_name=b.ward_name
		where a.minutes_to_view IS NOT NULL
		GROUP BY
			cast(a.minutes_to_view/60 as int), a.ward_name
	) tmp
PIVOT 
(
	max(viewed)
	for [ward_name] in ([Acorn],[Ward 9])
) as piv
order by hours asc;
*/


-- time to view by month
-- the first year (and last month) are artefacts
CREATE VIEW viewMeanTimeToViewByMonth AS
select 
	DATEPART(YEAR,first_viewed_date) as year,
	DATEPART(MONTH,first_viewed_date) as month,
	count(internal_id) as test_numbers,
	avg(cast(minutes_to_view as float)) as mean_minutes_test_age,
	avg(cast(minutes_to_view as float))-stdev(cast(minutes_to_view as float))/sqrt(count(internal_id)) as lower_ci_test_age,
	avg(cast(minutes_to_view as float))+stdev(cast(minutes_to_view as float))/sqrt(count(internal_id)) as higher_ci_test_age
from
	aggTimeToView
where
	total_views > 0
group by
	DATEPART(YEAR,first_viewed_date),
	DATEPART(MONTH,first_viewed_date)
-- ORDER BY year,month;
GO

-- calculate median time to view by month
CREATE VIEW viewMedianTimeToViewByMonth AS
SELECT 
	tmp.yr, 
	tmp.mth, 
	tmp.viewed_tests as tests, 
	tmp.minutes_to_view as median_minutes_to_view, 
	cast(tmp.viewed_tests as float)/tmp.total_tests*100 as viewed_percent
FROM ( 
	SELECT
		ROW_NUMBER() OVER (PARTITION BY 
			DATEPART(YEAR,a.date),
			DATEPART(MONTH,a.date)
				ORDER BY a.minutes_to_view asc) as viewed_test_ordered
		, 
		b.yr,
		b.mth,
		b.total_tests,
		b.viewed_tests,
		a.*
	FROM 
		aggTimeToView a INNER JOIN
		(
			SELECT 
				DATEPART(YEAR,date) as yr, 
				DATEPART(MONTH,date) as mth, 
				count(internal_id) as total_tests, 
				sum(iif(total_views>0,1,0)) as viewed_tests
			from aggTimeToView
			group by 
				DATEPART(YEAR,date), 
				DATEPART(MONTH,date) 
		) b ON b.yr = DATEPART(YEAR,a.date) and b.mth = DATEPART(MONTH,a.date) 
		where a.total_views > 0
	) tmp	
where tmp.viewed_test_ordered = tmp.viewed_tests / 2
-- ORDER by yr,mth
GO


-- calculate median time to view and viewed percent by ward
CREATE VIEW viewMedianTimeToViewByWard AS
SELECT 
	tmp.ward_name, 
	tmp.viewed_tests as tests, 
	tmp.minutes_to_view as median_minutes_to_view, 
	cast(tmp.viewed_tests as float)/tmp.total_tests*100 as viewed_percent
FROM ( 
	SELECT
		ROW_NUMBER() OVER (PARTITION BY 
			b.ward_name
				ORDER BY a.minutes_to_view asc) as viewed_test_ordered
		, 
		-- b.ward_name,
		b.total_tests,
		b.viewed_tests,
		a.*
	FROM 
		aggTimeToView a INNER JOIN
		(
			SELECT 
				ward_name, 
				count(internal_id) as total_tests, 
				sum(iif(total_views>0,1,0)) as viewed_tests
			from aggTimeToView
			group by 
				ward_name 
		) b ON b.ward_name = a.ward_name 
		where a.total_views > 0
	) tmp	
where tmp.viewed_test_ordered = tmp.viewed_tests / 2
-- ORDER by ward_name
GO

-- calculate median time to view and viewed percent by discipline
CREATE VIEW viewMedianTimeToViewByDiscipline AS
SELECT 
	tmp.discipline_name,
	tmp.viewed_tests as tests, 
	tmp.minutes_to_view as median_minutes_to_view, 
	cast(tmp.viewed_tests as float)/tmp.total_tests*100 as viewed_percent
FROM ( 
	SELECT
		ROW_NUMBER() OVER (PARTITION BY 
			b.discipline_name
				ORDER BY a.minutes_to_view asc) as viewed_test_ordered
		, 
		-- b.ward_name,
		b.total_tests,
		b.viewed_tests,
		a.*
	FROM 
		aggTimeToView a INNER JOIN
		(
			SELECT 
				discipline_name, 
				count(internal_id) as total_tests, 
				sum(iif(total_views>0,1,0)) as viewed_tests
			from aggTimeToView
			group by 
				discipline_name
		) b ON b.discipline_name=a.discipline_name
		where a.total_views > 0
	) tmp	
where tmp.viewed_test_ordered = tmp.viewed_tests / 2
-- ORDER by discipline_name
GO

-- calculate median time to view and viewed percent by discipline
DROP VIEW IF EXISTS viewMedianTimeToViewByTest;
GO

CREATE VIEW viewMedianTimeToViewByTest AS
SELECT
	tmp.investigation,
	tmp.investigation_name,
	tmp.viewed_tests as tests, 
	tmp.minutes_to_view as median_minutes_to_view, 
	cast(tmp.viewed_tests as float)/tmp.total_tests*100 as viewed_percent
FROM ( 
	SELECT
		ROW_NUMBER() OVER (PARTITION BY 
			b.investigation_name
				ORDER BY a.minutes_to_view asc) as viewed_test_ordered
		, 
		-- b.ward_name,
		b.total_tests,
		b.viewed_tests,
		a.*
	FROM 
		aggTimeToView a INNER JOIN
		(
			SELECT 
				investigation_name, 
				count(internal_id) as total_tests, 
				sum(iif(total_views>0,1,0)) as viewed_tests
			from aggTimeToView
			group by 
				investigation_name
		) b ON b.investigation_name=a.investigation_name
		where a.total_views > 0
	) tmp	
where tmp.viewed_test_ordered = tmp.viewed_tests / 2
-- ORDER by tests desc
GO

