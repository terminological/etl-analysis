USE omopBuild;
GO

-----------------------------------------
-- create a mapping table for identifiers
--BEGIN TRY
--	Create table omopBuild.dbo.UniqueIdentifiers (
--		id BIGINT IDENTITY PRIMARY KEY,
--		sourceId BIGINT,
--		sourceField VARCHAR(70)
--		INDEX X_source UNIQUE (sourceField,sourceId)
--	)
--END TRY
--BEGIN CATCH END CATCH
--GO

CREATE FUNCTION dbo.getId(@sourceId INT, @source CHAR(4)) RETURNS BIGINT
AS
BEGIN
	RETURN CONVERT(BIGINT,(CONVERT(BINARY(4),@source)))*256*256*256*256+@sourceId
END
GO

-- SELECT dbo.getId(1235,'a')
-- SELECT dbo.getId(1236,'ZZZZ')

-----------------------------------------
-- TODO some sort of audit table
-- functions to find entries from the source database
-- procedures to delete entries based on sourceId
-- procedures to ensure ids exist for entries based on sourceId (which may not be unique)... Assume all have been deleted?
-- deletions need to cascade down through to the target tables... otherwise we would need to do some form of update...

CREATE TABLE EtlAudit (
	id bigint IDENTITY(1,1) PRIMARY KEY,
	groupId int, -- the patient id in the master index
	sourceUrn VARCHAR(512), -- a urn describing the source of the record including database name, table name and field name.
	sourceId bigint, -- a source id 
	sourceUuid uniqueidentifier, -- alternative source id
	targetUrn VARCHAR(512), -- a urn describing the destination of the record (e.g. urn:omop:procedure:procedure_occurrence_id)
	targetId bigint, -- omop v6 uses bigint, v5 uses int.
	extractTime timestamp,
	etlVersion VARCHAR(50), 
)

CREATE SEQUENCE omopProcedureOccurrenceId AS bigint START WITH 1
CREATE SEQUENCE omopConditionOccurrenceId AS bigint START WITH 1
CREATE SEQUENCE omopVisitOccurrenceId AS bigint START WITH 1
CREATE SEQUENCE omopDrugExposureId AS bigint START WITH 1
CREATE SEQUENCE omopMeasurementId AS bigint START WITH 1
CREATE SEQUENCE omopMeasurementId AS bigint START WITH 1
CREATE SEQUENCE omopNoteId AS bigint START WITH 1
CREATE SEQUENCE omopNoteNlpId AS bigint START WITH 1
CREATE SEQUENCE omopOtherId AS bigint START WITH 1

DROP TYPE IF EXISTS dbo.EtlAuditInputType;
CREATE TYPE dbo.EtlAuditInputType AS TABLE
	(sourceId bigint, sourceUuid uniqueidentifier, uniquifier int)
GO

CREATE PROCEDURE dbo.fnEtlDeleteExisting(
	@sourceUrn VARCHAR(512),
	@input dbo.EtlAuditInputType READONLY
)
AS
BEGIN
-- TODO: rethink this...
RETURN
END
GO


CREATE PROCEDURE dbo.fnEtlGenerateIds(
	@sourceUrn VARCHAR(512),
	@forTargetUrn VARCHAR(512),
	@input dbo.EtlAuditInputType READONLY,
	@etlVersion VARCHAR(50)
) 
AS
BEGIN
-- TODO: rethink this...
RETURN 
END
GO

CREATE FUNCTION dbo.fnEtlGetIds(
	@sourceUrn VARCHAR(512),
	@forTargetUrn VARCHAR(512),
	@input dbo.EtlAuditInputType READONLY)
RETURNS @results TABLE ( 
	id bigint IDENTITY(1,1) PRIMARY KEY,
	groupId int, -- the patient id in the master index
	sourceUrn VARCHAR(512), -- a urn describing the source of the record including database name, table name and field name.
	sourceId bigint, -- a source id 
	sourceUuid uniqueidentifier, -- alternative source id
	targetUrn VARCHAR(512), -- a urn describing the destination of the record (e.g. urn:omop:procedure:procedure_occurrence_id)
	targetId bigint, -- omop v6 uses bigint, v5 uses int.
	extractTime timestamp,
	etlVersion VARCHAR(50)
)
AS
BEGIN
INSERT INTO @results
	SELECT * from EtlAudit e 
		INNER JOIN @input x ON 
			(x.sourceId IS NOT NULL AND e.sourceId=x.sourceId) OR (x.sourceUuid IS NOT NULL AND e.sourceUuid = x.sourceUuid)
	WHERE sourceUrn=@sourceUrn and targetUrn = @forTargetUrn
RETURN
END
GO