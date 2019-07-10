/****** Script for SelectTopNRows command from SSMS  ******/
SELECT COUNT(*) as reports,
	cs.*
FROM 
	ordercomms_review.dbo.lab_patient l
	LEFT JOIN ordercomms_review.dbo.report r on l.patient_id = r.patient_id
	LEFT JOIN ordercomms_review.dbo.clinicianSynonym cs on cs.clinician_id = r.responsible_clinician_id
WHERE
	l.hospital_no like 'RBA%'
GROUP BY [subtype_id]
      ,cs.[lab_id]
      ,[clinician_id]
      ,[from_code]
      ,[from_code_instance]
      ,[clinician_type]
      ,[clinicianSynonym_id]
      ,[original_display_name]
      ,[original_clinician_id]
ORDER BY reports DESC


SELECT TOP (1000) [Provider id]
      ,[Provider Name]
      ,[Provider Degree]
      ,[Provider Specialty]
      ,[Provider Department]
  FROM [TriNetX].[dbo].[tblTriNetXProvider]