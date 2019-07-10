/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) 

	HASHBYTES('SHA2_256',concat(str(patient_id),str(nhsno))) as hash
    ,[sex]
	--,[dob]
	--,DATEDIFF(year, dob,GETDATE())
	--,FLOOR(DATEDIFF(year, dob,GETDATE())/10)+1
    ,CHOOSE(FLOOR(DATEDIFF(year, dob,GETDATE())/10)+1,'0-10','11-20','21-30','31-40','41-50','51-60','61-70','71-80','80+','80+','80+','80+','80+') as age_category
    ,IIF(death_date IS NULL, 0, 1) as dead
    FROM [ordercomms_review].[dbo].[patient]