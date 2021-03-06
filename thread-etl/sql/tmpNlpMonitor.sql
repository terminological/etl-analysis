/****** Script for SelectTopNRows command from SSMS  ******/
SELECT event_type, COUNT(*)
  FROM [omopBuild].[dbo].[NlpAudit]
  GROUP BY event_type
GO


SELECT 
	*,
	(totalDocs-totalComplete) as docsLeft
	-- CONVERT(FLOAT,(totalDocs-totalComplete))/docsLastHour as hoursLeft,
	-- CONVERT(FLOAT,(totalDocs-totalComplete))/docsLastHour/24 as daysLeft 
FROM
(SELECT COUNT(*) as docsLastHour
  FROM [omopBuild].[dbo].[NlpAudit]
  WHERE event_type='COMPLETE' and event_time > DATEADD(HOUR,-1,GETDATE())) t,
(SELECT COUNT(*) as totalDocs
  FROM [omopBuild].[dbo].[NlpAudit]
  WHERE event_type='PENDING') p,
(SELECT COUNT(*) as totalComplete
  FROM [omopBuild].[dbo].[NlpAudit]
  WHERE event_type='COMPLETE') c
;

DROP VIEW IF EXISTS NlpProcessingTime
GO

CREATE VIEW NlpProcessingTime AS
SELECT nlp_system_instance, DATEDIFF(ms,prev_event_time,event_time) as millisecondsTaken, prev_event_type+'_'+event_type as transition FROM (
	SELECT 
		n1.*,
		LAG(event_time,1,event_time) OVER(PARTITION BY n1.nlp_system_instance ORDER BY n1.event_time) as prev_event_time,
		LAG(event_type,1,event_type) OVER(PARTITION BY n1.nlp_system_instance ORDER BY n1.event_time) as prev_event_type
	  FROM 
		[omopBuild].[dbo].[NlpAudit] n1
	  WHERE n1.event_type in ('COMPLETE','PROCESSING')
) x
GO 




SELECT 60*1000/avg(millisecondsTaken) as docsPerMin, avg(millisecondsTaken) as msPerDoc, count(*) as volume, nlp_system_instance, transition from (
SELECT 
	n1.*
  FROM NlpProcessingTime n1
  WHERE millisecondsTaken > 0
) x
GROUP BY nlp_system_instance,transition
ORDER BY nlp_system_instance, transition




SELECT avg(millisecondsTaken)*6000000.0/1000/60/60/24  FROM NlpProcessingTime n1

SELECT COUNT(DISTINCT note_id) from omop.dbo.note_nlp


SELECT COUNT(*) from omop.dbo.note_nlp

SELECT COUNT(DISTINCT note_id) from NlpAudit WHERE event_type = 'COMPLETE'