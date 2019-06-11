SELECT TOP(100) n.* 
  FROM omopBuild.dbo.IdentifiableNote n LEFT OUTER JOIN omop.dbo.note_nlp nlp ON n.note_id = nlp.note_id
  WHERE nlp.note_id IS NULL