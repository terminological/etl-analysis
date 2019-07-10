-- -----------------------------------------------------
-- Create blacklist and prepopulate with ids known to associate with test patients
-- -----------------------------------------------------

DROP TABLE IF EXISTS omopBuild.dbo.Blacklist;

CREATE TABLE omopBuild.dbo.Blacklist (
	rawId VARCHAR(35),
	idType CHAR(1),
	
	INDEX X_Blacklist_rawId (rawId),
	INDEX X_Blacklist_rawId_idType UNIQUE (rawId,idType)
);

-- nhs numbers from ordercomms
INSERT INTO omopBuild.dbo.Blacklist
SELECT DISTINCT
	p.nhsno as rawId,
	'N' as idType
FROM ordercomms_review.dbo.patient p 
WHERE p.nhsno LIKE ''
	OR p.nhsno LIKE '""'
	OR p.nhsno LIKE '999%'
	OR p.nhsno LIKE '11111%'
	OR p.nhsno LIKE '22222%'
;

-- hospital numbers from ordercomms
INSERT INTO omopBuild.dbo.Blacklist
SELECT DISTINCT
	l.hospital_no AS rawId,
	-- TRY_CONVERT(BIGINT,RIGHT(l.hospital_no,LEN(l.hospital_no) - PATINDEX('%[0-9]%',l.hospital_no))) AS id_as_int,
	'M' AS idType
FROM 
	ordercomms_review.dbo.patient p, 
	ordercomms_review.dbo.lab_patient l
WHERE 
	(
		family_name LIKE 'XX%'
		OR family_name LIKE 'ZZ%'
		OR p.nhsno LIKE '999%'
		OR p.nhsno LIKE '11111%'
		OR p.nhsno LIKE '22222%'
	)
	AND p.patient_id = l.patient_id
;

-- nhs numbers from epro
INSERT INTO omopBuild.dbo.Blacklist
SELECT DISTINCT
	nhsNumber AS rawId,
	'N' AS idType
FROM [EproLive-Copy].dbo.t_patients
WHERE 
	(
		surname LIKE 'ZZ%'
		OR surname LIKE 'XX%'
		OR nhsNumber LIKE '999%'
		OR nhsNumber LIKE '11111%'
		OR nhsNumber LIKE '22222%'
	)
	AND nhsNumber IS NOT NULL
	AND NOT EXISTS (SELECT * FROM omopBuild.dbo.Blacklist WHERE rawId=nhsNumber AND idType='N')
;

INSERT INTO omopBuild.dbo.Blacklist
SELECT DISTINCT
	l.value AS rawId,
	'R' AS idType
FROM 
	[EproLive-Copy].dbo.t_patients p,
	[EproLive-Copy].[dbo].[t_patient_local_identifiers] l
WHERE 
	l.patientId = p.pGuid
	AND (
		p.surname LIKE 'ZZ%'
		OR p.surname LIKE 'XX%'
		OR p.nhsNumber LIKE '999%'
		OR p.nhsNumber LIKE '11111%'
		OR p.nhsNumber LIKE '22222%'
	)
	AND nhsNumber IS NOT NULL
;

-- -----------------------------------------------------
-- TODO: Create a trigger that updates the MPI when a record added to the blacklist
-- -----------------------------------------------------


-- -----------------------------------------------------
-- Create a master index table that includes demographics from the source databases
-- -----------------------------------------------------

DROP TABLE IF EXISTS omopBuild.dbo.MasterIndex;

CREATE TABLE omopBuild.dbo.MasterIndex (
	instanceId INT IDENTITY PRIMARY KEY,
	rawId VARCHAR(35) NOT NULL,
	idType CHAR(1),
	source VARCHAR(10),
	groupId INT DEFAULT NULL,
	-- externalId UNIQUEIDENTIFIER DEFAULT NULL,
	firstName NVARCHAR(255) DEFAULT NULL,
	middleNames NVARCHAR(255) DEFAULT NULL,
	lastName NVARCHAR(255) DEFAULT NULL,
	gender CHAR(1) DEFAULT NULL,
	dateOfBirth DATETIME DEFAULT NULL,
	dateOfDeath DATETIME DEFAULT NULL,
	address NVARCHAR(MAX) DEFAULT NULL,
	postcode VARCHAR(15) DEFAULT NULL,
	telephoneHome VARCHAR(70) DEFAULT NULL,
	telephoneMobile VARCHAR(70) DEFAULT NULL,
	updateDate DATETIME DEFAULT NULL,

	INDEX X_MasterIndex_rawId (rawId),
	INDEX X_MasterIndex_rawId_idType_source (rawId,idType,source),
	INDEX X_MasterIndex_groupId (groupId)
);
GO

-- -----------------------------------------------------
USE omopBuild;
GO


DROP VIEW IF EXISTS dbo.CleanMasterIndex;
GO

CREATE VIEW dbo.CleanMasterIndex AS 
SELECT * FROM omopBuild.dbo.MasterIndex p
WHERE NOT(
	p.rawId LIKE ''
	OR p.rawId LIKE '""'
	OR (p.lastName IS NOT NULL AND p.lastName LIKE 'ZZ%')
	OR (p.lastName IS NOT NULL AND p.lastName LIKE 'XX%')
	OR (p.rawId LIKE '999%' and p.idType = 'N')
	OR (p.rawId LIKE '11111%' and p.idType = 'N')
	OR (p.rawId LIKE '22222%' and p.idType = 'N')
	OR p.groupId IS NULL
);
GO


DROP VIEW IF EXISTS dbo.DirtyMasterIndex;
GO

CREATE VIEW dbo.DirtyMasterIndex AS 
SELECT * FROM omopBuild.dbo.MasterIndex p
WHERE
	p.rawId LIKE ''
	OR p.rawId LIKE '""'
	OR p.lastName LIKE 'ZZ%'
	OR p.lastName LIKE 'XX%'
	OR (p.rawId LIKE '999%' and p.idType = 'N')
	OR (p.rawId LIKE '11111%' and p.idType = 'N')
	OR (p.rawId LIKE '22222%' and p.idType = 'N')
;
GO

INSERT INTO omopBuild.dbo.MasterIndex
SELECT DISTINCT
	p.nhsno as rawId,
	'N' as idType,
	'ordercomms' as source,
	NULL as groupId,
	-- NULL as externalId,
	p.first_name as firstName,
	p.middle_name as middleNames,
	p.family_name as lastName,
	IIF(p.sex='Male','M',IIF(p.sex='Female','F','U')) as gender,
	p.dob as dateOfBirth,
	p.death_date as dateOfDeath,
	TRIM(CONCAT(
		ISNULL(address1,''),' ',
		ISNULL(address2,''),' ',
		ISNULL(address3,''),' ',
		ISNULL(address4,''),' ',
		ISNULL(address5,''),' '
		)) as address,
	p.post_code as postcode,
	p.phone_number as telephoneHome,
	p.mobile_phone_number as telephoneMobile,
	p.last_demographic_update as updateDate
FROM 
	ordercomms_review.dbo.patient p
WHERE 
	p.nhsno IS NOT NULL
	AND p.nhsno <> ''
;
-- TODO:
INSERT INTO omopBuild.dbo.MasterIndex
SELECT DISTINCT 
	l.hospital_no AS rawId,
	'M' as idType,
	'ordercomms' as source,
	NULL as groupId,
	-- NULL as externalId,
	p.first_name as firstName,
	p.middle_name as middleNames,
	p.family_name as lastName,
	IIF(p.sex='Male','M',IIF(p.sex='Female','F','U')) as gender,
	p.dob as dateOfBirth,
	p.death_date as dateOfDeath,
	TRIM(CONCAT(
		ISNULL(address1,''),' ',
		ISNULL(address2,''),' ',
		ISNULL(address3,''),' ',
		ISNULL(address4,''),' ',
		ISNULL(address5,''),' '
		)) as address,
	post_code as postcode,
	p.phone_number as telephoneHome,
	p.mobile_phone_number as telephoneMobile,
	p.last_demographic_update as updateDate
from 
	ordercomms_review.dbo.lab_patient l,
	ordercomms_review.dbo.patient p
where 
	l.hospital_no is not null
	AND l.patient_id = p.patient_id;

INSERT INTO omopBuild.dbo.MasterIndex
SELECT DISTINCT 
	l.value as rawId,
	'R' as idType,
	'epro' as source,
	NULL as groupId,
	-- NULL as externalId,
	p.forename as firstName,
	p.middleNames as middleNames,
	p.surname as lastName,
	IIF(p.gender=1,'M',IIF(p.gender=2,'F','U')) as gender,
	p.dateOfBirth as dateOfBirth,
	p.dateOfDeath as dateOfDeath,
	TRIM(CONCAT(
		ISNULL(permanentAddressLine1,''),' ',
		ISNULL(permanentAddressLine2,''),' ',
		ISNULL(permanentAddressLine3,''),' ',
		ISNULL(permanentAddressLine4,''),' ',
		ISNULL(permanentAddressLine5,''),' '
		)) as address,
	permanentAddressPostCode as postcode,
	p.telephoneHome as telephoneHome,
	p.telephoneMobile as telephoneMobile,
	p.datestamp as updateDate
	FROM
		[EproLive-Copy].[dbo].[t_patient_local_identifiers] l,
		[EproLive-Copy].[dbo].[t_patients] p
	WHERE 
		l.patientId = p.pGuid
;

INSERT INTO omopBuild.dbo.MasterIndex
SELECT DISTINCT 
	p.nhsNumber as rawId,
	'N' as idType,
	'epro' as source,
	NULL as groupId,
	-- NULL as externalId,
	p.forename as firstName,
	p.middleNames as middleNames,
	p.surname as lastName,
	IIF(p.gender=1,'M',IIF(p.gender=2,'F','U')) as gender,
	p.dateOfBirth as dateOfBirth,
	p.dateOfDeath as dateOfDeath,
	TRIM(CONCAT(
		ISNULL(permanentAddressLine1,''),' ',
		ISNULL(permanentAddressLine2,''),' ',
		ISNULL(permanentAddressLine3,''),' ',
		ISNULL(permanentAddressLine4,''),' ',
		ISNULL(permanentAddressLine5,''),' '
		)) as address,
	permanentAddressPostCode as postcode,
	IIF(p.telephoneHome='',NULL,p.telephoneHome) as telephoneHome,
	IIF(p.telephoneMobile='',NULL,p.telephoneMobile) as telephoneMobile,
	p.datestamp as updateDate
	FROM
		[EproLive-Copy].[dbo].[t_patients] p
	WHERE
		p.nhsNumber IS NOT NULL
		AND p.nhsNumber <> ''
;

-- TODO: Insert from TrinetX with RBA number as identifier
INSERT INTO omopBuild.dbo.MasterIndex
SELECT DISTINCT 
	p.patient_id as rawId,
	'R' as idType,
	'trinetx' as source,
	NULL as groupId,
	-- NULL as externalId,
	NULL as firstName,
	NULL as middleNames,
	NULL as lastName,
	LEFT(p.gender,1) as gender,
	CONVERT(DATETIME,p.date_of_birth,112) as dateOfBirth,
	CONVERT(DATETIME,p.date_of_death,112) as dateOfDeath,
	NULL as address,
	zip_code as postcode,
	NULL as telephoneHome,
	NULL as telephoneMobile,
	NULL as updateDate
	FROM
		[TriNetX].[dbo].[tblDemographicData] p
;

	
-- -----------------------------------------------------
-- get relationships defined in source databases and from nhs number matches
-- this will only be a partial map
-- -----------------------------------------------------

DROP TABLE IF EXISTS omopBuild.dbo.IdentityMap;

CREATE TABLE omopBuild.dbo.IdentityMap (
	sourceInstanceId INT,
	targetInstanceId INT,
	
	INDEX X_IdentityMap_sourceInstanceId_targetInstanceId UNIQUE (sourceInstanceId,targetInstanceId)
)

-- ----------------------------------------------------------
-- Hook together same ids and types from different sources
-- e.g. NHS numbers from different sources
-- RBA numbers from different sources.

INSERT INTO omopBuild.dbo.IdentityMap
SELECT DISTINCT 
	m.instanceId as sourceInstanceId,
	m2.instanceId as targetInstanceId
FROM
	omopBuild.dbo.MasterIndex m,
	omopBuild.dbo.MasterIndex m2
WHERE 
	m.rawId = m2.rawId
	AND m.source <> m2.source
	AND m.idType = m2.idType
;

-- ----------------------------------------------------------
-- Could combine above and below in one stage in theory
-- below is code to combine duplicate / historical entries in same sources e.g.
-- the ordercomms data

-- same source same identifier
INSERT INTO omopBuild.dbo.IdentityMap
SELECT DISTINCT 
	m.instanceId as sourceInstanceId,
	m2.instanceId as targetInstanceId
FROM
	omopBuild.dbo.MasterIndex m,
	omopBuild.dbo.MasterIndex m2
WHERE 
	m.rawId = m2.rawId
	AND m.source = m2.source
	AND m.idType = m2.idType
	AND m.instanceId <> m2.instanceId
;

-- short and long version of RBA identifier
INSERT INTO omopBuild.dbo.IdentityMap
SELECT DISTINCT 
	m.instanceId as sourceInstanceId,
	m2.instanceId as targetInstanceId
FROM
	omopBuild.dbo.MasterIndex m,
	omopBuild.dbo.MasterIndex m2
WHERE 
	'RBA'+m.rawId = m2.rawId
	AND m.idType = 'R'
	AND m2.idType = 'M'
;

-- epro NHS number to RBA mappings
INSERT INTO omopBuild.dbo.IdentityMap
SELECT DISTINCT 
	m.instanceId as sourceInstanceId,
	m2.instanceId as targetInstanceId
FROM
	[EproLive-Copy].[dbo].[t_patient_local_identifiers] l,
	[EproLive-Copy].[dbo].[t_patients] p,
	omopBuild.dbo.MasterIndex m,
	omopBuild.dbo.MasterIndex m2
WHERE 
	l.patientId = p.pGuid
	AND l.value = m.rawId
	AND m.source = 'epro'
	AND m.idType = 'R'
	AND p.nhsNumber = m2.rawId
	AND m2.source = 'epro'
	AND m2.idType = 'N'
;



-- MRNS asserted to be the same as an NHS no by the ordercomms system
INSERT INTO omopBuild.dbo.IdentityMap
SELECT DISTINCT 
		m.instanceId as sourceInstanceId,
		m2.instanceId as targetInstanceId
	FROM
		ordercomms_review.dbo.lab_patient l,
		ordercomms_review.dbo.patient p,
		omopBuild.dbo.MasterIndex m,
		omopBuild.dbo.MasterIndex m2
	WHERE 
		l.patient_id = p.patient_id
		AND m.rawId = l.hospital_no COLLATE Latin1_General_CI_AS
		AND m.source = 'ordercomms'
		AND m.idType = 'M'
		AND m2.rawId = p.nhsNo COLLATE Latin1_General_CI_AS
		AND m2.source = 'ordercomms'
		AND m2.idType = 'N'
;

-- 2 MRNS asserted to be the same patient by the ordercomms system
INSERT INTO omopBuild.dbo.IdentityMap
SELECT DISTINCT 
		m.instanceId as sourceInstanceId,
		m2.instanceId as targetInstanceId
	FROM
		ordercomms_review.dbo.lab_patient l,
		ordercomms_review.dbo.lab_patient l2,
		omopBuild.dbo.MasterIndex m,
		omopBuild.dbo.MasterIndex m2
	WHERE 
		l.patient_id = l2.patient_id
		AND m.rawId = l.hospital_no COLLATE Latin1_General_CI_AS
		AND m.source = 'ordercomms'
		AND m.idType = 'M'
		AND m2.rawId = l2.hospital_no COLLATE Latin1_General_CI_AS
		AND m2.source = 'ordercomms'
		AND m2.idType = 'M'
		AND m.rawId <> m2.rawId
;

--TODO: look at amalagamated with field in EPro for detecting more identities

-- -----------------------------------------------------
-- FIND MINIMUM GRAPH IDS
-- basically the strategy here is to pick the lowest graph_id of nodes for a given source_node_id, target_node_id and 
-- update the edges. Then pick the lowest graph id of an edge and update each node. repeat until no changes.
-- -----------------------------------------------------

UPDATE m
SET m.groupId = m.instanceId
FROM omopBuild.dbo.MasterIndex m;

DECLARE @tmp int;
SET @tmp = -1;
WHILE @tmp <> 0
BEGIN

	UPDATE n1 
	SET n1.groupId = n2.groupId --,
	-- n1.externalId = n2.externalId
	FROM 
	omopBuild.dbo.IdentityMap e 
	INNER JOIN  omopBuild.dbo.MasterIndex n1 on n1.instanceId = e.sourceInstanceId
	INNER JOIN  omopBuild.dbo.MasterIndex n2 on n2.instanceId = e.targetInstanceId
	WHERE n1.groupId > n2.groupId;
	
	SET @tmp = @@ROWCOUNT;

	UPDATE n1 
	SET n1.groupId = n2.groupId --,
	-- n1.externalId = n2.externalId
	FROM 
	omopBuild.dbo.IdentityMap e 
	INNER JOIN  omopBuild.dbo.MasterIndex n1 on n1.instanceId = e.targetInstanceId
	INNER JOIN  omopBuild.dbo.MasterIndex n2 on n2.instanceId = e.sourceInstanceId
	WHERE n1.groupId > n2.groupId;

	SET @tmp = @tmp+@@ROWCOUNT;

END

-- SELECT COUNT(*) FROM omopBuild.dbo.DirtyMasterIndex

-- -----------------------------------------------------
-- delete rows relating to blacklisted
-- TODO: remove this from here and put into the logic for the study population
-- would this work though? It leaves the garbage in the master index 
-- however this test patient garbage should be filtered out not using a 
-- blacklist anyway
-- maybe we can do this as a view which removes all the dodgy rows
-- possibly before the whole lot is mapped down to single group Id
-- Hmmm.....
-- -----------------------------------------------------

UPDATE m2
	SET m2.groupId = NULL
FROM 
	omopBuild.dbo.DirtyMasterIndex m,
	omopBuild.dbo.MasterIndex m2
WHERE
	m.groupId = m2.groupId;

-- -----------------------------------------------------

-- TODO: graph analytics on the master index
-- Multiple patients

SELECT count(*) as number, size, min(groupId) from (
SELECT count(*) as size, groupId from omopBuild.dbo.CleanMasterIndex group by groupId
) t GROUP BY size ORDER BY size DESC

SELECT * FROM omopBuild.dbo.CleanMasterIndex WHERE groupId = 46204

SELECT * FROM omopBuild.dbo.CleanMasterIndex WHERE groupId = 23036

-- -----------------------------------------------------

DROP TABLE IF EXISTS omopBuild.dbo.MasterLookup;

CREATE TABLE omopBuild.dbo.MasterLookup (
	rawId VARCHAR(35),
	idType CHAR(1),
	source VARCHAR(10), -- TODO: consider removing
	groupId INT,
	
	INDEX X_IdentityLookup_rawId (rawId),
	INDEX X_IdentityLookup_rawId_idType_source UNIQUE (rawId,idType,source),
	INDEX X_IdentityLookup_rawId_idType_source_groupId UNIQUE (rawId,idType,source,groupId),
	INDEX X_IdentityLookup_groupId (groupId)
);

INSERT INTO omopBuild.dbo.MasterLookup
SELECT DISTINCT
	rawId, idType, 
	IIF(idType='N','nhs',source) as source, 
	groupId
FROM
	omopBuild.dbo.CleanMasterIndex
--WHERE groupId IS NOT NULL
--GROUP BY
--	rawId, idType, source, groupId
;
-- -----------------------------------------------------

-- SHOULD BE NO ROWS
--SELECT * FROM omopBuild.dbo.MasterLookup m1,
--omopBuild.dbo.MasterLookup m2
--WHERE m1.rawId = m2.rawId
--AND m1.idType = m2.idType
--AND m1.groupId <> m2.groupId

-- -----------------------------------------------------

--USE omopBuild;
--GO

--DROP FUNCTION dbo.dateAdj

--CREATE FUNCTION dbo.dateAdj(@groupId INT, @date DATETIME) RETURNS DATETIME AS
--	BEGIN
--		RETURN(DATEADD(day,(@groupId % 5 - 2),@date));
--	END
--GO

-- -----------------------------------------------------

DROP TABLE IF EXISTS omopBuild.dbo.EproLookup;

CREATE TABLE omopBuild.dbo.EproLookup (
	pGuid UNIQUEIDENTIFIER PRIMARY KEY,
	groupId INT DEFAULT NULL,
	INDEX X_EproLookup_pGuid_groupId UNIQUE(pGuid,groupId)
);

INSERT INTO omopBuild.dbo.EproLookup
SELECT DISTINCT p.pGuid, i.groupId FROM 
	omopBuild.dbo.MasterLookup i 
	INNER JOIN [EproLive-Copy].[dbo].[t_patient_local_identifiers] l 
		--ON (l.value = i.rawId AND i.source = 'epro' AND i.idType = 'R')
		ON (l.value = i.rawId AND i.idType = 'R')
	INNER JOIN [EproLive-Copy].[dbo].[t_patients] p
		ON l.patientId = p.pGuid;

INSERT INTO omopBuild.dbo.EproLookup
SELECT DISTINCT p.pGuid, i.groupId FROM 
	omopBuild.dbo.MasterLookup i 
	INNER JOIN [EproLive-Copy].[dbo].[t_patients] p
		ON (p.nhsNumber = i.rawId AND i.idType = 'N')
	LEFT JOIN omopBuild.dbo.EproLookup o 
		ON p.pGuid = o.pGuid AND o.groupId = i.groupId
		WHERE o.pGuid IS NULL;

-- -----------------------------------------------------
-- -----------------------------------------------------
-- TODO:



DROP TABLE IF EXISTS omopBuild.dbo.OrdercommsLookup;

CREATE TABLE omopBuild.dbo.OrdercommsLookup (
	patient_id INT PRIMARY KEY,
	groupId INT DEFAULT NULL,
	-- dateAdj INT,
	INDEX X_OrdercommsLookup_patientid_groupId UNIQUE(patient_id,groupId)
);

--INSERT INTO omopBuild.dbo.OrdercommsLookup
--SELECT DISTINCT p.patient_id, i.groupId FROM 
--	omopBuild.dbo.MasterLookup i,
--	ordercomms_review.dbo.lab_patient l,
--	ordercomms_review.dbo.patient p
--WHERE 
--	l.patient_id = p.patient_id
--	AND (
--		(l.hospital_no COLLATE Latin1_General_CI_AS = i.rawId AND i.source = 'ordercomms' AND i.idType = 'M') 
--			OR
--		(p.nhsNo COLLATE Latin1_General_CI_AS = i.rawId AND i.source = 'ordercomms' AND i.idType = 'N')
--	)
--;

INSERT INTO omopBuild.dbo.OrdercommsLookup
SELECT DISTINCT p.patient_id, i.groupId FROM
	omopBuild.dbo.MasterLookup i 
	INNER JOIN ordercomms_review.dbo.lab_patient l 
		ON (l.hospital_no COLLATE Latin1_General_CI_AS  = i.rawId AND i.source = 'ordercomms' AND i.idType = 'M')
	INNER JOIN ordercomms_review.dbo.patient p
		ON l.patient_id = p.patient_id;

INSERT INTO omopBuild.dbo.OrdercommsLookup
SELECT DISTINCT p.patient_id, i.groupId FROM
	omopBuild.dbo.MasterLookup i 
	INNER JOIN ordercomms_review.dbo.patient p
		ON (p.nhsNo COLLATE Latin1_General_CI_AS  = i.rawId AND i.idType = 'N')
	LEFT JOIN omopBuild.dbo.OrdercommsLookup o 
		ON p.patient_id = o.patient_id AND o.groupId = i.groupId
		WHERE o.patient_id IS NULL;

-- -----------------------------------------------------

DROP TABLE IF EXISTS omopBuild.dbo.TrinetxLookup;

CREATE TABLE omopBuild.dbo.TrinetxLookup (
	[Patient id] VARCHAR(255) PRIMARY KEY,
	groupId INT,
	INDEX X_TrinetxLookup_patientid_groupId UNIQUE([Patient id],groupId)
);

--INSERT INTO omopBuild.dbo.TrinetxLookup
--SELECT DISTINCT p.anonpid as [Patient id], i.groupId, i.dateAdj FROM 
--	omopBuild.dbo.MasterLookup i,
--	TriNetX.dbo.tblPatlink p
--WHERE 
--	(p.pid COLLATE Latin1_General_CI_AS = i.rawId AND i.source = 'epro' AND i.idType = 'R') 
--		OR
--	('RBA'+p.pid COLLATE Latin1_General_CI_AS = i.rawId AND i.source = 'ordercomms' AND i.idType = 'M')
--;

INSERT INTO omopBuild.dbo.TrinetxLookup
SELECT DISTINCT p.anonpid AS [Patient id], i.groupId FROM
	omopBuild.dbo.MasterLookup i 
	INNER JOIN TriNetX.dbo.tblPatlink p 
		ON ((p.pid COLLATE Latin1_General_CI_AS) = i.rawId AND i.source = 'epro' AND i.idType = 'R');

INSERT INTO omopBuild.dbo.TrinetxLookup
SELECT DISTINCT p.anonpid AS [Patient id], i.groupId FROM
	omopBuild.dbo.MasterLookup i 
	INNER JOIN TriNetX.dbo.tblPatlink p 
		ON ('RBA'+(p.pid COLLATE Latin1_General_CI_AS) = i.rawId AND i.source = 'ordercomms' AND i.idType = 'M')
	LEFT JOIN omopBuild.dbo.TrinetxLookup o 
		ON p.anonpid COLLATE Latin1_General_CI_AS = o.[Patient id] AND o.groupId = i.groupId
		WHERE o.[Patient id] IS NULL;

-- SELECT * FROM TriNetX.dbo.tblPatlink p WHERE p.anonpid='0000075078'
-- SELECT * FROM omopBuild.dbo.MasterLookup i where i.rawId = 'RBA2007088' and i.source = 'ordercomms' AND i.idType = 'M'
-- SELECT * FROM omopBuild.dbo.MasterLookup i where i.rawId = '2007088' and i.source = 'epro' AND i.idType = 'R'

-- SELECT * FROM omopBuild.dbo.CleanMasterIndex WHERE groupId in (5786779,2650455)

-- ------------------------------------------------------
-- SHOULD BE NO RESULTS
	--SELECT [Patient id], count(DISTINCT groupId) as c
	--FROM omopBuild.dbo.TrinetxLookup
	--GROUP BY [Patient id]
	--HAVING count(DISTINCT groupId) > 2

-- ------------------------------------------------------
-- find the subset of patients who appear in all our data sources.
-- TODO: This is the more logical place to apply a blacklist - possibly as an indexed view
-- rather than the table.
-- ------------------------------------------------------

DROP TABLE IF EXISTS omopBuild.dbo.StudyPopulation;

SELECT DISTINCT o.groupId, RAND(o.groupId)*5-2.5 as dateOffset
INTO omopBuild.dbo.StudyPopulation
FROM 
	omopBuild.dbo.OrdercommsLookup o,
	omopBuild.dbo.EproLookup e,
	omopBuild.dbo.TrinetxLookup t
WHERE e.groupId = o.groupId AND o.groupId = t.groupId;

CREATE UNIQUE INDEX X_StudyPopulation_groupId on omopBuild.dbo.StudyPopulation (groupId);


-- SELECT m2.* FROM omopBuild.dbo.MasterIndex m1, omopBuild.dbo.MasterIndex m2
-- WHERE m1.rawId IS NULL AND m1.idType = 'N' and m1.source = 'epro' and m1.groupId = m2.groupId ORDER BY m2.groupId
-- ------------------------------------------------------

--SELECT CONVERT(uniqueidentifier,hashbytes('SHA_128','TEMPTEST')),
--hashbytes('SHA_128','TEMPTEST'),
--CONVERT(BINARY(16),hashbytes('SHA_128','TEMPTEST'))

---- ------------------------------------------------------
--SELECT 
--	m2.*
--FROM (
--	SELECT 
--		groupId,
--		count(instanceId) as size
--	FROM omopBuild.dbo.MasterIndex m 
--	GROUP BY groupId
--) t,
--omopBuild.dbo.MasterIndex m2 
--WHERE t.size > 25
--AND t.groupId = m2.groupId



---- CREATE GRAPH IDS
--drop table if exists RobsDatabase.dbo.tmpIdGraph;

--create table RobsDatabase.dbo.tmpIdGraph (
--	graph_id int primary key,
--	emis int,
--	nhsnos int,
--	mrns int,
--	rba_mrns int,
--	identifiers int
--)

--INSERT INTO RobsDatabase.dbo.tmpIdGraph
--select
--		graph_id,
--		sum(IIF(type='E',1,0)) as emis,
--		sum(IIF(type='N',1,0)) as nhsnos,
--		sum(IIF(type='M',1,0)) as mrns,
--		sum(IIF(type='M' AND node_id LIKE 'MRBA%',1,0)) as rba_mrns,
--		count(node_id) as identifiers
--from RobsDatabase.dbo.tmpIdNode 
--group by graph_id;


------ select the overall counts of identifier graphs.
--select count(graph_id) as patients, identifiers FROM RobsDatabase.dbo.tmpIdGraph t 
--GROUP BY identifiers order by identifiers

------ breakdown by type of identifier
----select count(graph_id) as patients, t.emis,t.nhsnos,t.rba_mrns from
----RobsDatabase.dbo.tmpIdGraph t
----group by t.emis,t.nhsnos,t.rba_mrns order by emis asc,patients desc

------ select out large graphs
----select 
----	e.*
----FROM
----RobsDatabase.dbo.tmpIdEdge e inner join
----(
--	select top(5)
--		graph_id,
--		count(node_id) as identifiers from RobsDatabase.dbo.tmpIdNode group by graph_id
--		-- having count(node_id)=4
--		order by identifiers desc
----) t on e.graph_id=t.graph_id
----order by graph_id, source_node_id


------ select out selection of small graphs
----select 
----	e.*
----FROM
----RobsDatabase.dbo.tmpIdEdge e inner join
----(
----	select top(5)
----		graph_id,
----		count(node_id) as identifiers from RobsDatabase.dbo.tmpIdNode group by graph_id
----		having count(node_id)=4
----) t on e.graph_id=t.graph_id
----order by graph_id, source_node_id

------ select out large graphs
----select 
----	e.*
----FROM
----RobsDatabase.dbo.tmpIdEdge e inner join
----(
----	select top(5)
----		graph_id,
----		count(node_id) as identifiers from RobsDatabase.dbo.tmpIdNode group by graph_id
----		-- having count(node_id)=4
----		order by identifiers desc
----) t on e.graph_id=t.graph_id
----order by graph_id, source_node_id

------ graph ids for report based on patient_id
----select top(100) g.*, r.report_id, r.patient_id, r.original_patient_id
----FROM 
----	RobsDatabase.dbo.tmpIdNode n, 
----	RobsDatabase.dbo.tmpIdGraph g, 
----	ordercomms_review.dbo.report r
----WHERE CONCAT('E', r.patient_id) = n.node_id
----AND n.graph_id = g.graph_id

---- graph ids 
--drop table if exists RobsDatabase.dbo.tsftIdResult

--Create table RobsDatabase.dbo.tsftIdResult (
--	internal_id int primary key,
----	original_node_id varchar(35),
----	node_id varchar(35),
----	graph_id int,
--	date smalldatetime,
--	emis int,
--	nhsnos int,
--	mrns int,
--	rba_mrns int,
--	patient_id_updated int,
--	same_graph int,
--	tsft_test int
--)

--INSERT INTO RobsDatabase.dbo.tsftIdResult select
---- select top(100)
--	r.report_id as internal_id,
--	r.result_date as date,
----	 CONCAT('E', r.original_patient_id) as original_node_id, 
----	 CONCAT('E', r.patient_id) as node_id, 
----	 g.graph_id,  
--	 g.emis,
--	 g.nhsnos,
--	 g.mrns,
--	 g.rba_mrns,
--	-- r.patient_id, r.original_patient_id, n.graph_id, n2.graph_id,
--	IIF(r.patient_id <> r.original_patient_id,1,0) as patient_id_updated,
--	IIF(n.graph_id = n2.graph_id,1,0) as same_graph,
--	IIF(l.location_id IS NULL,0,1) as tsft_test
--FROM 
--	ordercomms_review.dbo.report r
--	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', r.original_patient_id) = n.node_id 
--	left join RobsDatabase.dbo.tmpIdNode n2 on CONCAT('E', r.patient_id) = n2.node_id 
--	left join RobsDatabase.dbo.tmpIdGraph g on n.graph_id = g.graph_id
--	left join RobsDatabase.dbo.tsftLocations l on l.location_id = r.location_id
--WHERE
--	r.result_date IS NOT NULL
--	and r.result_time IS NOT NULL;

---- stats on the level of test movement within and between graph.
--select patient_id_updated, same_graph, count(internal_id) from RobsDatabase.dbo.tsftIdResult
--group by patient_id_updated, same_graph;
--GO

--DROP VIEW IF EXISTS percent_results_updated_by_date
--GO

--CREATE VIEW percent_results_updated_by_date as
--SELECT TOP (365*6)
--	date,
--	100*CAST(updated as float)/results as percent_updated,
--	100*CAST(tsft_updated as float)/tsft_results as tsft_percent_updated
--FROM
--(SELECT date, 
--count(r.internal_id) as results, 
--sum(r.patient_id_updated) updated,
--sum(r.tsft_test) as tsft_results,
--sum(IIF(r.patient_id_updated=1 and r.tsft_test=1,1,0)) as tsft_updated
--from RobsDatabase.dbo.tsftIdResult r
--group by date
--) t
--where results > 0 and tsft_results>0
--order by date desc;
--GO




---- CREATE PROCEDURE TO GET THE RESULTS AND REQUESTS GRAPH FOR A GRAPH ID
--USE RobsDatabase;
--GO

--DROP TABLE IF EXISTS tmpRequestResultGraph;

--CREATE TABLE [dbo].[tmpRequestResultGraph](
--	[source_node_id] [varchar](13) NOT NULL,
--	[target_node_id] [varchar](13) NOT NULL,
--	[graph_id] [int] NULL,
--	[rel_type] [varchar](7) NOT NULL
--	constraint uniq_edge unique(source_node_id, target_node_id,graph_id,rel_type)
--) 
--ON [PRIMARY]

--GO

--INSERT INTO tmpRequestResultGraph
--SELECT *
-- FROM (
--select
--	CONCAT('E', r.original_patient_id) as source_node_id,  
--	CONCAT('R', r.report_id) as target_node_id,
--	n.graph_id as graph_id,
--	'res' as rel_type
--FROM 
--	ordercomms_review.dbo.report r
--	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', r.original_patient_id) = n.node_id 
--WHERE r.amended=0
--UNION
--select
--	CONCAT('E', r.patient_id) as source_node_id, 
--	CONCAT('R', r.report_id) as target_node_id,
--	n.graph_id as graph_id, 
--	'new_res' as rel_type
--FROM 
--	ordercomms_review.dbo.report r
--	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', r.patient_id) = n.node_id 
--where r.original_patient_id <> r.patient_id
--	and r.amended=0
--UNION
--select
--	CONCAT('O', rq.request_id) as source_node_id,
--	CONCAT('E', rq.original_patient_id) as target_node_id,  
--	n.graph_id as graph_id,
--	'req' as rel_type
--FROM 
--	ordercomms_review.dbo.request rq
--	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', rq.original_patient_id) = n.node_id 
--WHERE rq.amended=0
--UNION
--select
--	CONCAT('O', rq.request_id) as source_node_id, 
--	CONCAT('E', rq.patient_id) as target_node_id, 
--	n.graph_id as graph_id,
--	'new_req' as rel_type
--FROM 
--	ordercomms_review.dbo.request rq
--	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', rq.patient_id) = n.node_id 
--where rq.original_patient_id <> rq.patient_id and rq.amended=0
--	) t


