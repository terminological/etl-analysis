/****** Script for SelectTopNRows command from SSMS  ******/
-- Local identifiers in ePRO = RBA mrn
SELECT c.*, e.id, e.versionId, e.surname, e.forename, l.*
  FROM [RobsDatabase].[dbo].[deidMatchedCohort] c 
  LEFT OUTER JOIN [EproLive-Copy].dbo.t_patients e ON c.nhsno = e.nhsNumber COLLATE DATABASE_DEFAULT
  LEFT OUTER JOIN [EproLive-Copy].dbo.t_patient_local_identifiers l ON e.id = l.patientId


  /****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) c.*, e.id, e.versionId, e.surname, e.forename, l.*, h.*, html.*
  FROM [RobsDatabase].[dbo].[deidMatchedCohort] c 
  LEFT OUTER JOIN [EproLive-Copy].dbo.t_patients e ON c.nhsno = e.nhsNumber COLLATE DATABASE_DEFAULT
  LEFT OUTER JOIN [EproLive-Copy].dbo.t_episodes l ON e.pGuid = l.pGuid
  LEFT OUTER JOIN [EproLive-Copy].dbo.t_documents h ON l.versionId = h.episodeVersionId
  LEFT OUTER JOIN [EproLive-Copy].dbo.t_document_html html ON h.versionId = html.versionId

USE [EproLive-Copy];

SELECT P.*, D.*, N.*
  FROM [RobsDatabase].[dbo].[deidMatchedCohort] c 
  LEFT JOIN t_patients P ON c.nhsno = P.nhsNumber COLLATE DATABASE_DEFAULT
  LEFT JOIN t_clinical_records CR ON CR.patientId = P.id
  LEFT JOIN t_documents_current D ON CR.id = D.clinicalRecordId
  LEFT JOIN t_document_html N ON D.[versionId] = N.[versionId]
  WHERE notesActive <> '' and D.datestamp < c.date

SELECT DISTINCT 
	c.date as consentDate,
	c.Study_id as studyPatientId,
	c.mrn,
	P.nhsNumber, P.title, P.forename, P.middleNames, P.surname, P.preferredName, P.dateOfBirth, P.telephoneHome, P.telephoneWork, P.telephoneMobile, P.email,
	P.permanentAddressLine1, P.permanentAddressLine2, P.permanentAddressLine3, P.permanentAddressLine4, P.permanentAddressLine5, P.permanentAddressPostCode,
	G.letterName, G.nationalCode, 
	GS.description, GS.practiceCode,
	D.datestamp, D.authorName, D.authorAddrString, D.addresseeName, D.addresseeAddrString, D.addresseeIsGp, D.subject, D.importedDocumentIdentifierId,
	N.versionId, N.notesActive
INTO [RobsDatabase].[dbo].[deidSourceDocuments]
  FROM [RobsDatabase].[dbo].[deidMatchedCohort] c 
  LEFT JOIN t_patients P ON c.nhsno = P.nhsNumber COLLATE DATABASE_DEFAULT
  LEFT JOIN t_gps G ON P.gpId = G.versionId
  LEFT JOIN t_gp_practices GS ON P.surgeryId = GS.primarySurgeryId
  LEFT JOIN t_clinical_records CR ON CR.patientId = P.id
  JOIN t_documents_current D ON CR.id = D.clinicalRecordId AND c.date > d.datestamp
  LEFT JOIN t_document_html N ON D.[versionId] = N.[versionId]
  ORDER BY c.Study_id ASC

  
-- Drug JSON files - not many in here
  /****** Script for SelectTopNRows command from SSMS  ******/
SELECT [patientId]
      ,[versionId]
      ,[currentDrugsJson]
      ,[tagsString]
      ,[previousDrugsJson]
      ,[dischargeSummaryDate]
  FROM [EproLive-Copy].[dbo].[t_patient_current_drugs]
  where dischargeSummaryDate IS NOT NULL
  and (previousDrugsJson <> '[]' OR currentDrugsJson <> '[]')
