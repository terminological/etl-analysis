
USE RobsDatabase
GO

DROP VIEW IF EXISTS tmpTimeToRecovery;
GO

CREATE VIEW tmpTimeToRecovery AS
SELECT 
	*
FROM
(
SELECT 
r1.*,
r2.date as resolved_date,
r2.sequence_no as resolved_sequence_no,
ROW_NUMBER() OVER (
		PARTITION BY r1.patient_id,r1.test,r1.investigation,r1.sequence_no ORDER BY r2.sequence_no asc
	) as sequence_no_2
FROM 
tsftOrderedChemHaemResult r1, 
tsftOrderedChemHaemResult r2
WHERE 
r1.last_test_normalcy IS NULL
AND r1.normalcy IS NOT NULL 
AND r1.normalcy = r2.last_test_normalcy
AND r2.last_test_normalcy IS NOT NULL
AND r2.normalcy IS NULL 
AND r1.sequence_no < r2.sequence_no
AND r1.patient_id = r2.patient_id
AND r1.test = r2.test
AND r1.investigation = r2.investigation
) tmp
WHERE tmp.sequence_no_2 = 1;
GO

DROP TABLE IF EXISTS aggTimeToRecovery;

SELECT
	t.date,
	t.investigation,
	t.investigation_name,
	t.test,
	t.test_name,
	t.numeric_result,
	t.unit,
	t.normalcy,
	t.low_range,
	t.high_range,
	t.resolved_date,
	DATEDIFF(mi,t.date,t.resolved_date) as minutes_to_resolution,
	loc.ward_name,
	loc.dependency_level,
	loc.patient_group,
	Y.first_viewed_date,
	Y.view_type,
	DATEDIFF(mi,t.date,Y.first_viewed_date) as minutes_to_view, 
	IIF(Y.total_views IS NULL, 0, Y.total_views) as total_views, 
	Y.first_user_id,
	YEAR(t.date)-p.year_of_birth as patient_age,
	p.sex as patient_gender
INTO aggTimeToRecovery
FROM
tmpTimeToRecovery t
	INNER JOIN tsftInpatientLocations loc ON  loc.location_id = t.location_id
	INNER JOIN tsftUniquePatientIndex p ON t.patient_id = p.patient_id
	LEFT OUTER JOIN tsftFirstResultView Y on t.internal_id = Y.report_id

