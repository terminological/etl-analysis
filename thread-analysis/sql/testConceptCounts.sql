Use omop;

SELECT * FROM 
	(SELECT ancestor_concept_id, count(*) as count FROM condition_occurrence co, concept_ancestor a 
	WHERE  condition_status_source_value='P' and a.descendant_concept_id = co.condition_concept_id
	 GROUP BY a.ancestor_concept_id) co,
	concept c
  WHERE 
  co.ancestor_concept_id = c.concept_id
  ORDER BY count DESC
  
SELECT * FROM 
	(SELECT ancestor_concept_id, count(*) as count FROM observation co, concept_ancestor a 
	WHERE a.descendant_concept_id = co.observation_concept_id
	 GROUP BY a.ancestor_concept_id) co,
	concept c
  WHERE 
  co.ancestor_concept_id = c.concept_id
  ORDER BY count DESC

SELECT * FROM 
	(SELECT condition_concept_id, count(*) as count FROM condition_occurrence WHERE  condition_status_source_value='B' GROUP BY condition_concept_id) co,
  concept c
  WHERE 
  co.condition_concept_id = c.concept_id
  ORDER BY count DESC

SELECT * FROM 
	(SELECT note_nlp_concept_id, count(*) as count FROM note_nlp GROUP BY note_nlp_concept_id) co,
  concept c
  WHERE 
  co.note_nlp_concept_id = c.concept_id
  ORDER BY count DESC

