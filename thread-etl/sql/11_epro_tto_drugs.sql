-- --------------------------------------------


-- --------------------------------------------
DROP TABLE IF EXISTS #tmpRouteMap;

CREATE TABLE #tmpRouteMap (
	routeId DECIMAL PRIMARY KEY,
	route_concept_id INT
)

INSERT INTO #tmpRouteMap
SELECT DISTINCT routeId, concept_id
  FROM [EproLive-Copy].[dbo].[t_vmp_routes] r
  LEFT OUTER JOIN omop.dbo.concept c ON CONVERT(VARCHAR,r.routeId) = c.concept_code
WHERE c.domain_id='Route' and c.standard_concept='S'

-- --------------------------------------------

DROP TABLE IF EXISTS #tmpDrugMap;
GO

CREATE TABLE #tmpDrugMap (
	vmpId DECIMAL PRIMARY KEY,
	drug_concept_id INT,
	drug_source_concept_id INT,
	domain_id VARCHAR(20)
)
GO

INSERT INTO #tmpDrugMap
SELECT 
	v.vmpId,
	coalesce(min(concept_id_2),concept_id,0) as drug_concept_id, --Occasionally more than one mapping hence need for group by statement
	coalesce(concept_id,0) as drug_source_concept_id,
	c.domain_id
	-- SELECT *
FROM [EproLive-Copy].[dbo].[t_vmps] v
	LEFT OUTER JOIN omop.dbo.concept c ON (CONVERT(VARCHAR,v.vmpId) = c.concept_code AND c.concept_class_id = 'VMP')
	LEFT OUTER JOIN omop.dbo.concept_relationship r ON (c.concept_id = r.concept_id_1 AND r.relationship_id = 'Maps to' and coalesce(c.standard_concept,'z') NOT LIKE 'S')
	-- LEFT OUTER JOIN omop.dbo.concept_relationship r2 ON (c.concept_id = r.concept_id_1)
GROUP BY vmpId,concept_id,domain_id
GO



-- --------------------------------------------

-- DROP TABLE omopBuild.dbo.EproGuidMap;

BEGIN TRY
CREATE TABLE omopBuild.dbo.EproGuidMap (
	id INT IDENTITY PRIMARY KEY,
	guid UNIQUEIDENTIFIER,
	domain CHAR(4),
	omopId BIGINT,
	INDEX X_guid (guid),
	INDEX X_guid_omopid UNIQUE (guid,omopId)
)
END TRY
BEGIN CATCH END CATCH
GO

DROP TABLE IF EXISTS  #tmpUnmatchedIds;
GO

CREATE TABLE #tmpUnmatchedIds (
	guid UNIQUEIDENTIFIER PRIMARY KEY,
	groupId INT,
	dateOffset FLOAT
)
GO

-- appropriate duplicates are present in patient pGUID and lookup groudId
-- SELECT * from 
--	(SELECT groupId FROM omopBuild.dbo.EproLookup GROUP BY groupId HAVING COUNT(*)>1) dups,
--	omopBuild.dbo.EproLookup el,
--	dbo.t_patients p
-- WHERE dups.groupId = el.groupId and el.pGuid = p.pGuid
-- ORDER BY el.groupId
-- many have an amalgamated with Id
-- could be that this is known by ePRO but question is does it do anything about them


INSERT INTO #tmpUnmatchedIds
SELECT DISTINCT
	C.id as guid,
	sp.groupId,
	sp.dateOffset
FROM
	omopBuild.dbo.StudyPopulation sp 
		INNER JOIN omopBuild.dbo.EproLookup el on sp.groupId = el.groupId
		INNER JOIN [EproLive-Copy].[dbo].t_drugs_tto C ON C.pGuid = el.pGuid
		LEFT OUTER JOIN  omopBuild.dbo.EproGuidMap m ON C.id = m.guid
WHERE
	m.guid IS NULL

BEGIN TRANSACTION T1
	-- insert mising values
	INSERT INTO omopBuild.dbo.EproGuidMap 
	SELECT 
		guid,
		'dEPR',
		NULL
	FROM #tmpUnmatchedIds

	-- generate an omopId
	UPDATE m
	SET m.omopId = omopBuild.dbo.getId(m.id,m.domain)
	FROM omopBuild.dbo.EproGuidMap m
	WHERE m.omopId IS NULL
COMMIT TRANSACTION T1



DROP TABLE IF EXISTS  omop.dbo.drug_exposure

-- N.B. recreate drug_exposure table as original has an error in the spec on 
-- drug exposure end date. TODO: Raise with OHDSI
-- Cannot insert the value NULL into column 'drug_exposure_end_datetime', table 'omop.dbo.drug_exposure'; column does not allow nulls. INSERT fails.
CREATE TABLE omop.[dbo].[drug_exposure](
	[drug_exposure_id] [bigint] NOT NULL,
	[person_id] [bigint] NOT NULL,
	[drug_concept_id] [int] NOT NULL,
	[drug_exposure_start_date] [date] NULL,
	[drug_exposure_start_datetime] [datetime2](7) NOT NULL,
	[drug_exposure_end_date] [date] NULL,
	[drug_exposure_end_datetime] [datetime2](7),
	[verbatim_end_date] [date] NULL,
	[drug_type_concept_id] [int] NOT NULL,
	[stop_reason] [varchar](20) NULL,
	[refills] [int] NULL,
	[quantity] [float] NULL,
	[days_supply] [int] NULL,
	[sig] [varchar](max) NULL,
	[route_concept_id] [int] NOT NULL,
	[lot_number] [varchar](50) NULL,
	[provider_id] [bigint] NULL,
	[visit_occurrence_id] [bigint] NULL,
	[visit_detail_id] [bigint] NULL,
	[drug_source_value] [varchar](50) NULL,
	[drug_source_concept_id] [int] NOT NULL,
	[route_source_value] [varchar](50) NULL,
	[dose_unit_source_value] [varchar](50) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

-- -------------------------
-- rebuild
DELETE from omop.dbo.drug_exposure
DELETE from omop.dbo.device_exposure

INSERT INTO #tmpUnmatchedIds
SELECT DISTINCT
	C.id as guid,
	sp.groupId,
	sp.dateOffset
FROM
	omopBuild.dbo.StudyPopulation sp 
		INNER JOIN omopBuild.dbo.EproLookup el on sp.groupId = el.groupId
		INNER JOIN [EproLive-Copy].[dbo].t_drugs_tto C ON C.pGuid = el.pGuid
-- --------------------------------
SELECT COUNT(*) FROM #tmpUnmatchedIds
SELECT COUNT(*) FROM #tmpRouteMap
SELECT COUNT(*) FROM #tmpDrugMap

-- TODO: duplicates here that need resolving
-- 


INSERT INTO omop.dbo.drug_exposure
SELECT
DISTINCT
 --TOP(100)
	e.omopId as drug_exposure_id,
	u.groupId as person_id,
	dm.drug_concept_id as drug_concept_id,
	CONVERT(DATE,COALESCE(C.[startDate],C.[prescribedDate],C.datestamp)+u.dateOffset) as drug_exposure_start_date,
	CONVERT(DATETIME2,COALESCE(C.[startDate],C.[prescribedDate],C.datestamp)+u.dateOffset) as drug_exposure_start_datetime,
	CONVERT(DATE,C.[endDate]+u.dateOffset) as drug_exposure_end_date,
	CONVERT(DATETIME2,C.[endDate]+u.dateOffset) as drug_exposure_end_datetime,
	NULL as verbatim_end_date,
	38000178 as drug_type_concept_id, -- 38000178	Medication list entry
	NULL as stop_reason,
	NULL as refills,
	NULL as quantity,
	-- totalDailyDose as quantity, --TODO: Raise this with OHDSI - focus is around dispensing derived records not prescription records. this is unitless as is.
	-- we really need a different set of measures for quantity (maybe a rate) for prescription data (particularly open ended)
	-- TODO: drug_strength table has some relevant info in it - probably need to try and calculate a number of multiples of drug strength per day
	NULL as daysSupply,
	NULL as sig,
	rm.route_concept_id as route_concept_id,
	NULL as lot_number,
	NULL as provider_id, -- TODO: C.prescriberVersionId would be potential candidate if mapped...
	NULL as visit_occurrence_id, -- TODO: C.dischargeSummaryId could be mapped to an epro encounter....
	NULL as visit_detail_id,
	LEFT(D.vmpName,50) as drug_source_value,
	dm.drug_source_concept_id as drug_source_concept_id,
	routeName as route_source_value,
	doseUnitsText as dose_unit_source_value
	-- -----------------
	-- hard to work out a daily dose without VMP strength number which is not in ePRO, not in omop vocab, but is in drug_strength table
	--D.vmpName,
	--D.form,
	--D.unitDoseFormSize,
	--D.unitDoseFormSizeUnits,
	--D.unitDoseUnits,
	--D.strengthUnits,
	--frequency,
	--dose,
	--doseUnits,
	--doseUnitsText,
	--totalDailyDose,
	--frequencyText,
	--frequencyType,
	--routeName,
	--df.dailyMinimum,
	--df.dailyMaximum,
	--vmp.definedDailyDose,
	--vmp.definedDailyDoseUnits
	-- -----------------
	-- dischargeSummaryId, -- TODO: Can we make use of this?
--SELECT TOP(100) * 
-- SELECT activeStatus, COUNT(*) 
FROM #tmpUnmatchedIds u 
	INNER JOIN omopBuild.dbo.EproGuidMap e on u.guid = e.guid
	INNER JOIN [EproLive-Copy].[dbo].t_drugs_tto C ON C.id = u.guid
	INNER JOIN [EproLive-Copy].[dbo].[t_order_sentences] D ON D.orderSentenceId = C.orderSentenceId
	INNER JOIN #tmpRouteMap rm ON rm.routeId = D.route
	INNER JOIN #tmpDrugMap dm ON dm.vmpId = D.vmpId
	-- INNER JOIN [dbo].[t_vmps] vmp ON dm.vmpId = vmp.vmpId
	-- INNER JOIN [dbo].[tlu_drug_frequencies] df ON D.frequency = df.id
WHERE
	deleted = 0
	AND C.versionId = C.childId -- TODO: This is an attempt to rectify duplicates. It turns out that the drugs_tto table is versioned so any correction to the discharge summary creates a new entry
	AND activeStatus = 1
	AND dm.domain_id = 'Drug'
-- GROUP BY activeStatus



INSERT INTO omop.dbo.device_exposure
SELECT --TOP(100)
DISTINCT
	e.omopId as device_exposure_id,
	u.groupId as person_id,
	dm.drug_concept_id as device_concept_id,
	CONVERT(DATE,COALESCE(C.[startDate],C.[prescribedDate],C.datestamp)+u.dateOffset) as device_exposure_start_date,
	CONVERT(DATETIME2,COALESCE(C.[startDate],C.[prescribedDate],C.datestamp)+u.dateOffset) as device_exposure_start_datetime,
	CONVERT(DATE,C.[endDate]+u.dateOffset) as device_exposure_end_date,
	CONVERT(DATETIME2,C.[endDate]+u.dateOffset) as device_exposure_end_datetime,
	44818707 as device_type_concept_id, -- 44818707	EHR Detail
	NULL as unique_device_id,
	NULL as quantity, --TODO: Raise this with OHDSI - focus is around dispensing derived records not prescription records. this is unitless as is.
	NULL as provider_id, -- TODO: C.prescriberVersionId would be potential candidate if mapped...
	NULL as visit_occurrence_id, -- TODO: C.dischargeSummaryId could be mapped to an epro encounter....
	NULL as visit_detail_id,
	LEFT(D.vmpName,50) as device_source_value,
	dm.drug_source_concept_id as device_source_concept_id
FROM #tmpUnmatchedIds u 
	INNER JOIN omopBuild.dbo.EproGuidMap e on u.guid = e.guid
	INNER JOIN [EproLive-Copy].[dbo].t_drugs_tto C ON C.id = u.guid
	INNER JOIN [EproLive-Copy].[dbo].[t_order_sentences] D ON D.orderSentenceId = C.orderSentenceId
	INNER JOIN #tmpRouteMap rm ON rm.routeId = D.route
	INNER JOIN #tmpDrugMap dm ON dm.vmpId = D.vmpId
	-- INNER JOIN [dbo].[t_vmps] vmp ON dm.vmpId = vmp.vmpId
	-- INNER JOIN [dbo].[tlu_drug_frequencies] df ON D.frequency = df.id
WHERE
	deleted = 0
	AND activeStatus = 1
	AND C.versionId = C.childId -- TODO: This is an attempt to rectify duplicates. It turns out that the drugs_tto table is versioned so any correction to the discharge summary creates a new entry
	AND dm.domain_id = 'Device'
