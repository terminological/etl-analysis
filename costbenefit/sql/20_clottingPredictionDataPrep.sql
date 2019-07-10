/****** Script for SelectTopNRows command from SSMS  ******/
USE RobsDatabase;

-- two approaches to this.
-- 1) build something that looks at known results and tests done on the same day to see if we can predict 
-- things clotting would have been normal if we had not done the test

-- 2) build something that looks at previous results, and their timing in relation to a clotting result

-- Lets look at (1) first

-- Patients on anti-coagulation
DROP view if exists tmpAntiCoagPatients;
GO

create view tmpAntiCoagPatients as
SELECT distinct [patient_id], min(date) as date
  FROM [RobsDatabase].[dbo].[tsftRequest]
  where (
	src_comments like '%arfarin%' OR 
	src_comments like '%eparin%' OR
	src_comments like '%nti%oagulat%')
	group by patient_id
;
GO

DROP VIEW IF EXISTS aggClottingTrainingSet;
GO

CREATE VIEW aggClottingTrainingSet AS
SELECT 
	ch1.sequence_no,
	ch1.internal_id,
	ch1.date,
	ch1.test as label_test,
	ch1.test_abnormal as label_test_abnormal,
	ch1.numeric_result as label_numeric_result,
	ch1.unit as label_unit,
	ch2.test as value_test,
	ch2.test_abnormal as value_test_abnormal,
	ch2.numeric_result as value_numeric_result,
	ch2.unit as value_unit,
	ch1.location_id,
	ch1.patient_id,
	IIF(c.patient_id IS NULL,-1,1) as anticoagulated,
	l.inpatient,
	IIF(p.sex='Male',1,-1) as sex,
	YEAR(ch1.date)-p.year_of_birth as age,
	dependency = CASE l.dependency_level
		WHEN 'TRANSITIONAL_CARE' THEN -0.5
		WHEN 'GENERAL' THEN 0
		WHEN 'EMERGENCY' THEN 0.5
		WHEN 'HIGH_DEPENDENCY' THEN 1
		ELSE 1 --outpatient
	END
  FROM 
	tsftOrderedChemHaemResult ch1 left outer join tmpAntiCoagPatients c on c.patient_id=ch1.patient_id and ch1.date>c.date,
	tsftOrderedChemHaemResult ch2,
	tsftLocationTypes l,
	tsftPatientIndex p
  WHERE 
	ch1.investigation = 'CS' AND
	floor(cast(ch1.date as float)) = floor(cast(ch2.date as float)) AND
	ch1.patient_id = ch2.patient_id AND
	ch2.investigation in ('LFT','B','UEC','CRP') AND
	ch1.location_id=l.location_id AND
	p.patient_id = ch1.patient_id;
GO

SELECT value_test,value_unit FROM  aggClottingTrainingSet group by value_test,value_unit;

DROP TABLE IF EXISTS tmpTestIndex;
GO
SELECT 
	row_number() OVER( ORDER BY investigation,value_test) as value_id, 
	investigation, 
	value_test, 
	mean,
	sd
INTO tmpTestIndex
FROM (
	SELECT 
		investigation,
		test as value_test,
		AVG(CAST(numeric_result as FLOAT)) as mean,
		STDEV(CAST(numeric_result as FLOAT)) as sd
	 FROM 
		tsftOrderedChemHaemResult where investigation in ('LFT','B','UEC','CRP') 
	group by 
		investigation,
		test
) as tmp;
GO
-- SELECT * FROM tmpTestIndex
-- https://www.mssqltips.com/sqlservertip/2783/script-to-create-dynamic-pivot-queries-in-sql-server/
-- https://stackoverflow.com/questions/10404348/sql-server-dynamic-pivot-query

DROP TABLE IF EXISTS aggClottingTrainingLibSVM;
GO

DROP TABLE IF EXISTS aggClottingTrainingMatrix;
GO

DECLARE @columns NVARCHAR(MAX),@columns2 NVARCHAR(MAX),@missing NVARCHAR(MAX), @sql NVARCHAR(MAX);

SET @columns = N'';
SELECT @columns += N', 
ISNULL(p.'+QUOTENAME(value_test)+','''+CAST(value_id AS VARCHAR)+':0'') as '+value_test  FROM tmpTestIndex order by value_id;
SET @columns = STUFF(@columns, 1, 3, '')

SET @columns2 = N'';
SELECT @columns2 += N',' + QUOTENAME(value_test) FROM tmpTestIndex order by value_id;
SET @columns2 = STUFF(@columns2, 1, 1, '')

SET @missing = N'';
SELECT @missing += N'+IIF(p.'+QUOTENAME(value_test)+' IS NULL,1,0)' FROM tmpTestIndex order by value_id;
SET @missing = STUFF(@missing, 1, 1, '')

DECLARE @meanAge FLOAT, @sdAge FLOAT;
select
	@meanAge = AVG(cast(age as float)),
	@sdAge = STDEV(cast(age as float))
from aggClottingTrainingSet



SET @sql = N'
SELECT 
	date, label_test, label_test_abnormal, label_numeric_result, 
	'+@missing+' as missing,
	'+@columns+',
	inpatient,sex,age,dependency,anticoagulated
INTO aggClottingTrainingLibSVM
FROM
(
	SELECT DISTINCT
		a.date,
		a.internal_id, 
		a.sequence_no,
		a.location_id,
		a.patient_id, 
		a.label_test, 
		a.label_test_abnormal,
		a.label_numeric_result,
		a.value_test,
		CONCAT(t.value_id,'':'',((a.value_numeric_result-t.mean)/(2*t.sd))) as feature,
		CONCAT(''30:'',a.inpatient) as inpatient,
		CONCAT(''31:'',a.sex) as sex,
		CONCAT(''32:'',
			(age-'+CAST(@meanAge as CHAR)+')/(2*'+CAST(@sdAge as CHAR)+')
		) as age,
		CONCAT(''33:'',a.dependency) as dependency,
		CONCAT(''34:'',a.anticoagulated) as anticoagulated
		FROM aggClottingTrainingSet a, tmpTestIndex t
  WHERE a.value_test=t.value_test
) j
PIVOT
(
  MAX(feature) FOR value_test IN ('+ @columns2+')
) AS p;';
PRINT @sql;
-- EXEC sp_executesql @sql;


SET @sql = N'
SELECT 
	date, label_test, label_test_abnormal, label_numeric_result, 
	'+@missing+' as missing,
	'+@columns2+',
	inpatient,sex,age,dependency,anticoagulated
INTO aggClottingTrainingMatrix
FROM
(
	SELECT
		a.date,
		a.internal_id, 
		a.sequence_no,
		a.location_id,
		a.patient_id, 
		a.label_test, 
		a.label_test_abnormal,
		a.label_numeric_result,
		a.value_test,
		a.value_numeric_result as feature,
		a.inpatient,
		a.sex,
		a.age,
		a.dependency,
		a.anticoagulated
	FROM aggClottingTrainingSet a, tmpTestIndex t
	WHERE a.value_test=t.value_test
) j
PIVOT
(
  MAX(feature) FOR value_test IN ('+ @columns2+')
) AS p;';
PRINT @sql;
EXEC sp_executesql @sql;
GO


 

SELECT missing,COUNT(*) FROM aggClottingTrainingLibSVM group by missing order by missing;

SELECT label_test, sum(label_test_abnormal) as abnormal, sum(1-label_test_abnormal) from aggClottingTrainingLibSVM where missing <= 2 group by label_test

SELECT TOP 1000 * from aggClottingTrainingLibSVMout
