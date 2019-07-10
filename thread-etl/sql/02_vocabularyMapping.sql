DROP TABLE IF EXISTS omopBuild.dbo.ConceptMapping;

CREATE TABLE omopBuild.dbo.ConceptMapping (
id INT IDENTITY PRIMARY KEY,
sourceDomain VARCHAR(512),
sourceId VARCHAR(255),
sourceTerm VARCHAR(255),
certainty FLOAT,
reviewStatus INT,
omopDomainId VARCHAR(20),
omopConceptId INT,
omopConceptName VARCHAR(255),
usedCount INT

INDEX X_ConceptMapping_omopConceptId (omopConceptId)
)
GO

DROP VIEW IF EXISTS  dbo.loadConceptMapping
GO

CREATE VIEW dbo.loadConceptMapping
AS SELECT
sourceDomain, sourceId, sourceTerm, certainty, reviewStatus, omopDomainId, omopConceptId, omopConceptName, usedCount
FROM omopBuild.dbo.ConceptMapping;
GO

BULK INSERT omopBuild.dbo.loadConceptMapping 
FROM 'P:\Git\working-drafts\sql\omop\vocabMappings.txt'
WITH ( FIRSTROW=2, FIELDTERMINATOR = '\t')
GO

BULK INSERT omopBuild.dbo.loadConceptMapping 
FROM 'P:\Git\working-drafts\sql\omop\labsExport.txt'
WITH ( FIRSTROW=2, FIELDTERMINATOR = '\t')
GO

BULK INSERT omopBuild.dbo.loadConceptMapping 
FROM 'P:\Git\working-drafts\sql\omop\specimensExport.txt'
WITH ( FIRSTROW=2, FIELDTERMINATOR = '\t')
GO

BULK INSERT omopBuild.dbo.loadConceptMapping 
FROM 'P:\Git\working-drafts\sql\omop\unitsExport.txt'
WITH ( FIRSTROW=2, FIELDTERMINATOR = '\t')
GO

BULK INSERT omopBuild.dbo.loadConceptMapping 
FROM 'P:\Git\working-drafts\sql\omop\radiologyExport.txt'
WITH ( FIRSTROW=2, FIELDTERMINATOR = '\t')
GO

BULK INSERT omopBuild.dbo.loadConceptMapping 
FROM 'P:\Git\working-drafts\sql\omop\microbiologyExport.txt'
WITH ( FIRSTROW=2, FIELDTERMINATOR = '\t')
GO

BULK INSERT omopBuild.dbo.loadConceptMapping 
FROM 'P:\Git\working-drafts\sql\omop\specialtyNoteExport.txt'
WITH ( FIRSTROW=2, FIELDTERMINATOR = '\t')
GO

--Insert into omopBuild.dbo.ConceptMapping 
--(sourceDomain, sourceId, sourceTerm, certainty, reviewStatus, omopDomainId, omopConceptId, omopConceptName)
--VALUES
--('MasterIndex_Gender','M','Male',1.0,1,'Gender',8507,'MALE'),
--('MasterIndex_Gender','F','Female',1.0,1,'Gender',8532,'FEMALE'),
--('MasterIndex_Gender','U','Unknown',1.0,1,'Gender',0,'Unknown concept'),
--('MasterIndex_Race','U','Unknown',1.0,1,'Race',0,'Unknown concept'),
--('MasterIndex_Ethnicity','U','Unknown',1.0,1,'Ethnicity',0,'Unknown concept')
--;
-- TODO: externalise mappings



SELECT SUM(c), sourceDomain, sourceId, COUNT(DISTINCT omopConceptId) as mappings, STRING_AGG(sourceTerm,CHAR(13)+CHAR(10))
FROM (
	SELECT COUNT(*) as c,sourceDomain, sourceId, sourceTerm, omopConceptId
	FROM omopBuild.dbo.ConceptMapping
	GROUP BY sourceDomain, sourceId, omopConceptId, sourceTerm
	) r
GROUP BY  sourceDomain, sourceId
HAVING SUM(c) > 1

DELETE m FROM 
--SELECT * FROM
	omopBuild.dbo.ConceptMapping m,
	(SELECT *,ROW_NUMBER() OVER(PARTITION BY sourceDomain, sourceId order by reviewStatus DESC,certainty desc) as filter FROM omopBuild.dbo.ConceptMapping) m2
WHERE m.id = m2.id and m2.filter > 1

CREATE UNIQUE INDEX X_unique_sourceDomain_sourceId ON omopBuild.dbo.ConceptMapping (sourceDomain, sourceId)