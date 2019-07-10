/****** Script for SelectTopNRows command from SSMS  ******/

USE [TriNetX]
GO

SET ANSI_PADDING ON
GO

/****** Object:  Index [X_Patient_id]    Script Date: 05/03/2019 12:49:32 ******/
CREATE NONCLUSTERED INDEX [X_Patient_id] ON [dbo].[tblTriNetXProcedure] ([Patient id] ASC)
CREATE NONCLUSTERED INDEX [X_Procedure_code] ON [dbo].[tblTriNetXProcedure] ([Procedure code] ASC)
GO

SELECT
	COUNT(DISTINCT p.[Patient id]) as num
  FROM [TriNetX].[dbo].[tblTriNetXProcedure] p
  WHERE p.[Procedure Code] like 'H01%'

Drop table if exists RobsDatabase.dbo.appendicitisExample

SELECT
	-- TOP(100)
	DATEDIFF(dd,CONVERT(date,l.[Test date]),CONVERT(date, p.[Procedure Date], 112)) as days,
	l.[Patient id],
	[Procedure Description],
	[Code],
	[Description],
	[Normal Range],
	[Numeric value],
	[Units of measure]
  INTO RobsDatabase.dbo.appendicitisExample
  FROM [TriNetX].[dbo].[tblTriNetXProcedure] p
	LEFT JOIN [TriNetX].[dbo].[tblTriNetXLabResult] l ON l.[Patient id] = p.[Patient id]
  AND p.[Procedure Code] like 'H01%'
  WHERE
  CONVERT(date, p.[Procedure Date], 112) >= CONVERT(date,l.[Test date])
  AND CONVERT(date, p.[Procedure Date], 112) < CONVERT(date,DATEADD(m,1,l.[Test date]))
  AND l.[Result type] = 'NUMERIC'


-- TODO: figure out a control group... Patients with diagnosis of adbo pain...
-- Hopefully overlap with patients who had appendicitis...
SELECT COUNT(Distinct [Patient id])
 FROM [TriNetX].[dbo].[tblTriNetXDIAGNOSIS]
 Where [Diagnosis Code] like 'R10%'

 USE [RobsDatabase]
GO


SELECT Days, Code, Min(Description) as descr,
	Count(*) as count
		,AVG([Numeric value]) as avg
		,STDEV([Numeric value]) as stdev
      ,MIN([Units of measure]) as unit
	  ,MIN([Normal Range]) as unit
  FROM RobsDatabase.[dbo].[appendicitisExample]
  GROUP BY Code, days
  ORDER By Code,days ASC
GO

