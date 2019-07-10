/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [versionId]
      ,[id]
      ,[previousVersionId]
      ,[userDate]
      ,[datestamp]
      ,[name]
      ,[registeredTypes]
      ,[xml]
      ,[isDeleted]
      ,[timestamp]
      ,[validationErrors]
      ,[patientContextMode]
      ,[editorVersionId]
      ,[authorId]
      ,[externalId]
      ,[description]
      ,[editorHeaderText]
  FROM [EproLive-Copy].[dbo].[t_epro_templates]


  /****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [Template name]
      ,[Latest doc]
      ,[Total Letters]
  FROM [EproLive-Copy].[dbo].[v_templates_usage_report]
  -- Are discharge summaries different? no record of usage ni here.