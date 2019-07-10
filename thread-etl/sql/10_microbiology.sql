/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP(100) *
  FROM [ordercomms_review].[dbo].[report]
  WHERE result_date IS NULL or result_time IS NULL
  AND discipline_id = 
	-- 27 -- MRI
	28 -- USS

-- N reports in rnpi - rnpi is only for radiology - discipline 29
SELECT discipline_id, COUNT(*)
  FROM [ordercomms_review].[dbo].[report] r
  INNER JOIN [ordercomms_review].[dbo].[rtest] f ON f.report_id = r.report_id
  WHERE result_date IS NOT NULL AND result_time IS NOT NULL
  GROUP BY discipline_id

SELECT TOP(100) *
  FROM [ordercomms_review].[dbo].[report] r
  INNER JOIN [ordercomms_review].[dbo].[rtest] f ON f.report_id = r.report_id
  WHERE result_date IS NOT NULL AND result_time IS NOT NULL
  AND discipline_id = 3
  order by rsample_id DESC 


USE [ordercomms_review]
GO

-- --------------------
-- Lab test disciplines

SELECT 
      d.discipline_id,MIN(d.[name])
      ,COUNT(rtest_id) as c
	  ,SUM(IIF(numeric_result IS NULL,0,1)) as numeric
	  ,SUM(IIF(textual_result IS NULL,0,1)) as textual
  FROM [dbo].[discipline] d LEFT JOIN [dbo].[report] r ON d.discipline_id = r.discipline_id
  INNER JOIN [ordercomms_review].[dbo].[rtest] f ON f.report_id = r.report_id
  WHERE result_date IS NOT NULL AND result_time IS NOT NULL
  GROUP BY d.discipline_id
  HAVING COUNT(rtest_id)  > 0
  ORDER BY  d.discipline_id
GO

--discipline_id	(No column name)	c
--1	Chem/Haem	25275619
--2	Histopathology	1029961
--3	Microbiology	5090922
--7	Virology	649215
--8	Gynae Cytology	2102127
--10	Non-Gynae Cytology	126963
--12	Downs Screening	23182
--13	Andrology	31672
--14	Blood Transfusion	1002405
--18	Molecular Pathology	487
--20	Unknown	8586
--21	Pathology	1097
--25	X-Ray	448121
--26	CAT	67399
--27	MRI	45456
--28	Ultrasound	174958
--29	Radiology	852686