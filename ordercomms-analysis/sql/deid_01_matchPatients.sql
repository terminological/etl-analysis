/****** Script for SelectTopNRows command from SSMS  ******/
USE RobsDatabase;
GO

DROP FUNCTION IF EXISTS dbo.normaliseText;
GO

CREATE FUNCTION dbo.normaliseText(@input varchar(max))
returns varchar(max)
AS
BEGIN
	DECLARE @output varchar(max);
	SET @output = lower(@input)
	SET @output = replace(@output,'(','')
	SET @output = replace(@output,')','')
	SET @output = replace(@output,'[','')
	SET @output = replace(@output,']','')
	SET @output = replace(@output,'\','')
	SET @output = replace(@output,'/','')
	SET @output = replace(@output,'''','')
	SET @output = replace(@output,'"','')
	SET @output = replace(@output,',',' ')
	SET @output = replace(@output,'.',' ')
	SET @output = replace(@output,'-',' ')
	SET @output = replace(@output,'_',' ')
	SET @output = replace(@output,'!',' ')
	SET @output = replace(@output,'?',' ')
	SET @output = replace(@output,'%',' ')
	SET @output = replace(@output,'   ',' ')
	SET @output = replace(@output,'  ',' ')
	RETURN(trim(@output))
END;
GO

SELECT dbo.normaliseText('HElllo... !! World')

DROP VIEW deidMatchedParticipants
GO

-- match patients by family name, first name (begins with), DOB and post_code
-- some patients have moved and post code not undated so falls back to matching on just
-- family, firstname, DOB if postcode cannot be matched.
CREATE VIEW deidMatchedParticipants as
SELECT DISTINCT p1.*,
	IIF(p2.family_name IS NULL,p3.family_name,p2.family_name) as matched_family_name,
	-- IIF(p2.first_name IS NULL,p3.first_name,p2.first_name) as first_name,
	IIF(p2.dob IS NULL,p3.dob,p2.dob) as matched_dob,
	p2.post_code as matched_post_code,
	IIF(p2.nhsno IS NULL,p3.nhsno,p2.nhsno) as nhsno
  FROM [RobsDatabase].[dbo].[deidParticipants] p1
  LEFT OUTER JOIN ordercomms_review.dbo.patient p2 ON (
	p2.family_name like p1.Surname
	AND
	(p1.Forenames like CONCAT(p2.first_name,'%') OR
	p2.first_name like CONCAT(p1.Forenames,'%'))
    AND
	replace(p2.post_code,' ','') = replace(p1.Postcode,' ','')
    AND
	p2.dob = p1.DOB
	and p2.nhsno not like ''
  )
  LEFT OUTER JOIN ordercomms_review.dbo.patient p3 ON (
	p3.family_name like p1.Surname
    AND
	(p1.Forenames like CONCAT(p3.first_name,'%') OR
	p3.first_name like CONCAT(p1.Forenames,'%'))
	AND
	p3.dob = p1.DOB
	AND
	p2.patient_id IS NULL
	and p3.nhsno not like ''
  );
GO

-- Find RBA mrns's for matched patients
-- Write this to table so we can 
DROP TABLE IF EXISTS deidMatchedCohort;

SELECT DISTINCT m.*,SUBSTRING(n2.node_id,2,LEN(n2.node_id)) as mrn
INTO deidMatchedCohort
from deidMatchedParticipants m
INNER JOIN tmpIdNode n ON CONCAT('N',m.nhsno) = n.node_id
LEFT OUTER JOIN tmpIdNode n2 ON n.graph_id = n2.graph_id AND n2.node_id like 'MRBA%' -- OR l1.hospital_no like 'PP%')
ORDER BY m.Study_id asc
GO

DROP VIEW IF EXISTS deidRealPatients;
GO

CREATE VIEW deidRealPatients AS 
SELECT * FROM
ordercomms_review.dbo.patient p
	where NOT(p.nhsno like ''
		OR p.nhsno like '""'
		or p.nhsno like '000%'
		or p.nhsno like '999%'
		or p.nhsno like '11111%'
		or p.nhsno like '22222%'
		or p.nhsno is null)
	and left(family_name,2) <> 'zz'
	and left(family_name,2) <> 'xx'
GO

DROP TABLE IF EXISTS deidLastNameComponents;

-- Generate all the unique words in surnames
select
	value as name_component,
	count(patient_id) as freq
INTO deidLastNameComponents
from (
	select 
		dbo.normaliseText(family_name)	as string, patient_id
	from deidRealPatients p
	) t
CROSS APPLY STRING_SPLIT(string,' ')
 where len(value)>1 and
 value not like '%[0-9]%'
 group by value
 order by freq desc, name_component asc 

 DROP TABLE IF EXISTS deidFirstNameComponents;

 -- Generate all the unique words in names
 select
	trim(value) as name_component,
	sex,
	count(patient_id) as freq 
	into deidFirstNameComponents
from (
	select 
		dbo.normaliseText(
			concat(
				IIF(first_name IS NOT NULL,concat(' ',first_name),''),
				IIF(middle_name IS NOT NULL,concat(' ',middle_name),''))) 
		as string,sex,patient_id
	from deidRealPatients ) t
CROSS APPLY STRING_SPLIT(string,' ')
 where len(trim(value))>1
 and value not like '%[0-9]%'
 and trim(value) not in ('baby','girl','boy')
 and left(trim(value),2) <> 'zz'
 and left(trim(value),2) <> 'xx'
 group by trim(value),sex;
 GO 
 
SELECT * FROM deidLastNameComponents
 order by  name_component asc 

-- =========================================================================



DROP TABLE IF EXISTS deidAddressComponents;
-- Generate all the unique words in address lines
Select
	trim(value) as address_component,
	count(patient_id) as freq 
	into deidAddressComponents
from (
	select 
		dbo.normaliseText(concat(
								address1,' ',
								IIF(address2 IS NOT NULL,address2,''),' ',
								IIF(address3 IS NOT NULL,address3,''),' ',
								IIF(address4 IS NOT NULL,address4,''),' ',
								IIF(address5 IS NOT NULL,address5,'')))
						 
		as string, patient_id
	from deidRealPatients
	) t
CROSS APPLY STRING_SPLIT(string,' ')
where len(trim(value))>1 and
value not like '%[0-9]%'
--AND (
--	PATINDEX('[1-9]',value) = 0 AND
--	PATINDEX('[1-9][0-9]',value) = 0 AND
--	PATINDEX('[1-9][0-9][0-9]',value) = 0 AND
--	PATINDEX('[1-9][0-9][0-9][0-9]',value) = 0 AND
--	PATINDEX('[1-9][a-z]',value) = 0 AND
--	PATINDEX('[1-9][0-9][a-z]',value) = 0 AND
--	PATINDEX('[1-9][0-9][0-9][a-z]',value) = 0 AND
--	PATINDEX('[1-9][0-9][0-9][0-9][a-z]',value) = 0
--)
 and trim(value) not in ('the','""','c/o','co','of','and','no', 'on')
 -- and isnumeric(value)=0
 -- and left(value,2) <> 'zz'
 -- and left(value,2) <> 'xx'
 group by trim(value)
 order by freq desc, address_component asc 


DROP TABLE IF EXISTS deidAddresses;
-- Generate all the unique words in address lines
	select distinct 
		REPLACE(REPLACE(TRIM('| ' FROM t.address),'|||','|'),'||','|') as address,
		IIF(t.post_code NOT LIKE '[A-Z][A-Z]%[A-Z][A-Z]', NULL, t.post_code) as post_code
	INTO deidAddresses
	FROM (
	select
		dbo.normaliseText(concat(
								IIF(address5 IS NOT NULL,address5,''),'|',
								IIF(address4 IS NOT NULL,address4,''),'|',
								IIF(address3 IS NOT NULL,address3,''),'|',
								IIF(address2 IS NOT NULL,address2,''),'|',
								IIF(address1 IS NOT NULL,address1,'')))
		as address, post_code
	
	from deidRealPatients
	) t;

 
 -- =========================================================================
 -- rvUser.long_name, clinician.name, clinicianSynonym.original_display_name for clinicians 

  DROP TABLE IF EXISTS deidClinicianNameComponents
 Select
	trim(value) as clinician_name_component,
	count(1) as freq
into deidClinicianNameComponents
from (
	select 
		dbo.normaliseText(t.input)
		as string
	from (
		SELECT CAST(long_name as VARCHAR(500)) as input from ordercomms_review.dbo.rvUser
		UNION SELECT CAST(name as VARCHAR(500)) as input from ordercomms_review.dbo.clinician
		UNION SELECT CAST(original_display_name as VARCHAR(500)) as input from ordercomms_review.dbo.clinicianSynonym
		) as t
	) as t2
	CROSS APPLY STRING_SPLIT(string,' ')
 where len(trim(value))>1
 and trim(value) not in ('the','""','c/o','co','of','and','no', 'on','it')
 and isnumeric(value)=0
 and left(value,2) <> 'zz'
 and left(value,2) <> 'xx'
 group by trim(value)
 order by freq desc, clinician_name_component asc 

 -- =========================================================================
 -- practice.name for surgeries
 -- location.name & locationSynonym.original_display_name for locations
 DROP TABLE IF EXISTS deidLocationNameComponents
 Select
	trim(value) as location_name_component,
	count(1) as freq
    into deidLocationNameComponents
from (
	select 
		dbo.normaliseText(t.input)
		as string
	from (
		SELECT CAST(name as VARCHAR(500)) as input from ordercomms_review.dbo.practice
		UNION SELECT CAST(name as VARCHAR(500)) as input from ordercomms_review.dbo.location
		UNION SELECT CAST(original_display_name as VARCHAR(500)) as input from ordercomms_review.dbo.locationSynonym
		) as t
	) as t2
	CROSS APPLY STRING_SPLIT(string,' ')
 where len(trim(value))>1
 and trim(value) not in ('the','""','c/o','co','of','and','no', 'on','it')
 and isnumeric(value)=0
 and left(value,2) <> 'zz'
 and left(value,2) <> 'xx'
 group by trim(value)
 order by freq desc, location_name_component asc 