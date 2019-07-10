SELECT * FROM (
	SELECT ancestor_concept_id, count(*) as count
	FROM condition_occurrence co, concept_ancestor a
	WHERE co.condition_status_source_value='P' 
	AND a.descendant_concept_id=co.condition_concept_id
	GROUP BY a.ancestor_concept_id
	) x, concept c
WHERE x.ancestor_concept_id = c.concept_id
	