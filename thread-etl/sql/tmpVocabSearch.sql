SELECT TOP (1000) [concept_id]
      ,[concept_name]
      ,[domain_id]
      ,[vocabulary_id]
      ,[concept_class_id]
      ,[standard_concept]
      ,[concept_code]
      ,[valid_start_date]
      ,[valid_end_date]
      ,[invalid_reason]
  FROM [omop].[dbo].[concept]
  WHERE 
  --vocabulary_id = 'Meas Type'
 --  domain_id = 'Type Concept'
  -- concept_class_id = 'Record Artifact'
  -- AND concept_class_id = 'Qualifier Value'
concept_name like 'chest pain'
 -- concept_name like '%eview of systems%'
 -- concept_name like '%performed%'
-- concept_id = 40524613

  SELECT TOP (1000) *
  FROM [omop].[dbo].[concept] c1,
  [omop].[dbo].[concept_relationship] r,
   [omop].[dbo].[concept] c2
  WHERE c1.concept_name like '%eparin%'
  AND c1.domain_id like 'Drug'
  -- AND c1.vocabulary_id like 'SNOMED'
  -- AND c1.concept_class_id like 'VMP'
  AND c1.concept_id = r.concept_id_1
  AND r.concept_id_2 = c2.concept_id

  SELECT TOP(100) * FROM
	[omop].[dbo].[concept] c1,
	omop.dbo.drug_strength ds,
	omop.dbo.concept ing,
	--omop.dbo.concept nuc,
	--omop.dbo.concept duc
	WHERE c1.concept_name like '%rifamp%'
		AND c1.domain_id like 'Drug'
		AND c1.concept_id = ds.drug_concept_id
		AND ing.concept_id = ds.ingredient_concept_id
	--AND ds.amount_unit_concept_id = auc.concept_id
	--AND ds.numerator_unit_concept_id = nuc.concept_id
	--AND ds.denominator_unit_concept_id = duc.concept_id


  SELECT DISTINCT domain_id FROM [omop].[dbo].[concept];
  SELECT DISTINCT vocabulary_id FROM [omop].[dbo].[concept];
  SELECT DISTINCT concept_class_id FROM [omop].[dbo].[concept];

  SELECT * FROM 
  [omop].[dbo].[concept] c,
  [omop].[dbo].[concept_ancestor] a
  where ancestor_concept_id=36209248
  AND descendant_concept_id=concept_id
  AND standard_concept = 'S'
  AND concept_name like '%%'


  SELECT TOP(100) * FROM
  [omop].[dbo].[concept] c1 LEFT OUTER JOIN
  [omop].[dbo].[concept_relationship] cr ON c1.concept_id = cr.concept_id_1  AND cr.relationship_id='Maps to' LEFT OUTER JOIN
  [omop].[dbo].[concept] c2 ON c2.concept_id = cr.concept_id_2
  WHERE 
  -- c1.concept_code = '40317633' --and 
  cr.concept_id_1 ='40524613'

  --standard_concept NOT LIKE 'S'

  SELECT DISTINCT relationship_id FROM [omop].[dbo].[concept_relationship] cr
  SELECT * from 