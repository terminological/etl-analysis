-- SCRIPT TO IMPORT TSFT LOCATIONS CLASSIFICATION
-- imports a spreadsheet that maps location_code to wards
-- location_code are the lab system identifiers for wards and vary by the lab system

DROP TABLE IF EXISTS RobsDatabase.dbo.tsftLocationTypes;
GO

CREATE TABLE [RobsDatabase].[dbo].[tsftLocationTypes](
	[location_id] [int] NOT NULL,
	[name] [nvarchar](50) NOT NULL,
	[other_names] [nvarchar](100) NOT NULL,
	[location_code] [nvarchar](50) NOT NULL,
	[inpatient] [int] NOT NULL,
	[ward] [nvarchar](50) NULL,
	[dependency_level] [nvarchar](50) NULL,
	[patient_group] [nvarchar](50) NULL,
	[discipline] [nvarchar](50) NULL
) ON [PRIMARY]
GO

-- We don't have permissions to do a bulk insert
-- so we have to import "P:\data\locationsInTSFT - tsftLocationsUsed.csv" into RobsDatabase.dbo.tsftLocationTypes
-- manually. This table has to be dropped first to allow this.

BULK INSERT  [RobsDatabase].[dbo].[tsftLocationTypes]
FROM "P:\data\locationsInTSFT - tsftLocationsUsed (4).csv"
WITH 
(
	FIRSTROW = 2,
	FIELDTERMINATOR = ',',  --CSV field delimiter
	ROWTERMINATOR = '\n',   --Use to shift the control to next row
	TABLOCK
)

DROP TABLE IF EXISTS RobsDatabase.dbo.tsftInpatientLocations;

CREATE TABLE RobsDatabase.[dbo].[tsftInpatientLocations](
	[location_id] [int] NOT NULL PRIMARY KEY,
	[name] [varchar](70) NOT NULL,
	[code] [varchar](70) NOT NULL,
	[ward_name] [nvarchar](50) NULL,
	[dependency_level] [nvarchar](50) NULL,
	[patient_group] [nvarchar](50) NULL,
	[discipline] [nvarchar](50) NULL,
	INDEX X_ward_name (ward_name),
	INDEX X_ward_type (dependency_level, patient_group, discipline)
) ON [PRIMARY]
GO



-- find all the location_ids that correspond to locations
INSERT INTO RobsDatabase.[dbo].[tsftInpatientLocations]
SELECT DISTINCT 
	l.location_id,
	l.name,
	l.code,
	lt.ward as ward_name,
	lt.dependency_level,
	lt.patient_group,
	lt.discipline
  FROM 
	[RobsDatabase].[dbo].[tsftLocationTypes] lt, 
	ordercomms_review.dbo.location l
  WHERE 
	lt.location_code = l.code and lt.inpatient = 1
  ORDER BY ward_name ASC;

