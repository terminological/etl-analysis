/****** Script for SelectTopNRows command from SSMS  ******/
SELECT 'title' as tableName,COUNT(*) as numberRows FROM dbo.title
UNION SELECT 'titleSynonym' as tableName,COUNT(*) as numberRows FROM dbo.titleSynonym
UNION SELECT 'test' as tableName,COUNT(*) as numberRows FROM dbo.test
UNION SELECT 'testSynonym' as tableName,COUNT(*) as numberRows FROM dbo.testSynonym
UNION SELECT 'title' as tableName,COUNT(*) as numberRows FROM dbo.title
UNION SELECT 'batterySynonym' as tableName,COUNT(*) as numberRows FROM dbo.batterySynonym
UNION SELECT 'npi' as tableName,COUNT(*) as numberRows FROM dbo.npi
UNION SELECT 'npisynonym' as tableName,COUNT(*) as numberRows FROM dbo.npisynonym
UNION SELECT 'location' as tableName,COUNT(*) as numberRows FROM dbo.location
UNION SELECT 'locationSynonym' as tableName,COUNT(*) as numberRows FROM dbo.locationSynonym
UNION SELECT 'request' as tableName,COUNT(*) as numberRows FROM dbo.request
UNION SELECT 'requestBatterySynonymMatrix' as tableName,COUNT(*) as numberRows FROM dbo.requestBatterySynonymMatrix
UNION SELECT 'requestSampleSynonymMatrix' as tableName,COUNT(*) as numberRows FROM dbo.requestSampleSynonymMatrix
UNION SELECT 'requestTestSynonymMatrix' as tableName,COUNT(*) as numberRows FROM dbo.requestTestSynonymMatrix
UNION SELECT 'requestNPISynonymMatrix' as tableName,COUNT(*) as numberRows FROM dbo.requestNPISynonymMatrix
;


/*
SELECT TOP(50) * FROM dbo.title;
SELECT TOP (50) * FROM dbo.titleSynonym;
SELECT TOP (50) * FROM dbo.testSynonym;
SELECT TOP (50) * FROM dbo.batterySynonym;
select top (1000) * from npisynonym
*/