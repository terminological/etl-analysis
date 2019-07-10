-- Generates a de-identified patient demographics table
-- contains a salted hash of internal identifier plus
-- all external identifiers as a salted hash
-- this will include NHS number and all known hospital numbers for a patient
-- this is the table to use to integrate patients from DM
-- the de-id tests table is used to provide test data to DM to make sure they are
-- salting and hasing correctly.
-- this includes year of birth unless patient is over 90 (at which point yob is 2017-90)
-- also includes year of death

drop table if exists RobsDatabase.dbo.tsftPatientIndex;
GO

IF OBJECT_ID(N'RobsDatabase.dbo.tsftSalt', N'U') IS NULL BEGIN
	Create table RobsDatabase.dbo.tsftSalt (
		id int IDENTITY(1,1) Primary key,
		uuid uniqueidentifier NOT NULL
	);
END;

insert into  RobsDatabase.dbo.tsftSalt values (NEWID());
GO

Create table RobsDatabase.dbo.tsftPatientIndex (
  patient_id binary(32) NOT NULL,
  -- code_type tinyint,
  -- hospital_id varchar(35),
  shared_id binary(32) NOT NULL,
  sex varchar(11),
  year_of_birth int,
  year_of_death int
)

CREATE UNIQUE INDEX X_tsftPatientIndex on RobsDatabase.dbo.tsftPatientIndex 
(patient_id, shared_id);

CREATE INDEX X_patientId on RobsDatabase.dbo.tsftPatientIndex 
(patient_id);

CREATE INDEX X_sharedId on RobsDatabase.dbo.tsftPatientIndex 
(shared_id);

-- patients have multiple MRNs on the system, e.g. they have a PP MRN and a RBA one, then they
-- might have more MRNs from Yeovil etc.
-- realistically I can't be sure that a given message will reference the same MRN for a patient
-- so I have to be flexible about identifier and ask for anything.
-- some patients don't have NHS number.
-- We should salt this..?

DECLARE @salt char(36);  
SET @salt = (Select CONVERT(char(36),uuid) from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id));

INSERT INTO RobsDatabase.dbo.tsftPatientIndex
SELECT DISTINCT * FROM (
 SELECT 
	CONVERT(binary(32),hashbytes('SHA2_256',@salt+CONVERT(nvarchar(4000),lp.patient_id))) as patient_id,
	CONVERT(binary(32),hashbytes('SHA2_256',@salt+CONVERT(nvarchar(4000),lp.hospital_no))) as shared_id,
	p.sex,
	iif(YEAR(p.dob)<(2018-90),(2018-90),YEAR(p.dob)) as year_of_birth,
	YEAR(p.death_date) as year_of_death
  FROM 
	ordercomms_review.dbo.lab_patient lp,
	ordercomms_review.dbo.patient p
  WHERE 
	lp.patient_id = p.patient_id
UNION
-- some additional NHS number which are not in the lab_patient table.
SELECT
	CONVERT(binary(32),hashbytes('SHA2_256',@salt+CONVERT(nvarchar(4000),p.patient_id))) as patient_id,
	CONVERT(binary(32),hashbytes('SHA2_256',@salt+CONVERT(nvarchar(4000),p.nhsno))) as shared_id,
	p.sex,
	iif(YEAR(p.dob)<(2018-90),(2018-90),YEAR(p.dob)) as year_of_birth,
	YEAR(p.death_date) as year_of_death
  FROM 
	ordercomms_review.dbo.patient p
  WHERE 
	p.nhsno is not null and p.nhsno not like ''
) tmp;
GO

-- Test salting
DECLARE @salt char(36);
SET @salt = (Select CONVERT(char(36),uuid) from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id));
SELECT 
	[test_value],
	@salt as salt,
	CONVERT(binary(32),hashbytes('SHA2_256',@salt+CONVERT(nvarchar(4000),test_value))) as patient_id
  FROM [RobsDatabase].[dbo].[deidTests];
GO