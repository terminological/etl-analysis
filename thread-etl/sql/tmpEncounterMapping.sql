


SELECT Distinct
      'Trinetx Encounter Type|Location id' as sourceDomain,
	  CONCAT(e.[Encounter Type],'|', e.[Location id]) as sourceId,
	  CONCAT(e.[Encounter Type],'|', e.[Location id]) as sourceTerm,
	  1 as certainty,
	  c.domain_id as omopDomainId,
	  c.[concept_id] as omopConceptId,
     c.[concept_name] as omopConceptName
INTO RobsDatabase.dbo.tmpOmopVisitMapping	 
FROM [omop].[dbo].[concept] c FULL OUTER JOIN 
	 (SELECT DISTINCT [Encounter Type], [Location id] FROM
	 [TriNetX].[dbo].[tblTriNetXEncounter]) e ON (
		c.[concept_name] LIKE (CONCAT('%',e.[Encounter Type],'%') COLLATE Latin1_General_CI_AS))
	 Where c.vocabulary_id='Visit'
	 AND c.concept_id <> 262



  