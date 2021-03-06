/****** Script for SelectTopNRows command from SSMS  ******/
DELETE FROM omop.dbo.note

INSERT INTO omop.dbo.note
SELECT [note_id]
      ,[person_id]
      ,[note_event_id]
      ,[note_event_field_concept_id]
      ,[note_date]
      ,[note_datetime]
      ,[note_type_concept_id]
      ,[note_class_concept_id]
      ,[note_title]
      ,'redacted' as [note_text]
      ,[encoding_concept_id]
      ,[language_concept_id]
      ,[provider_id]
      ,[visit_occurrence_id]
      ,[visit_detail_id]
      ,[note_source_value]
  FROM [omopBuild].[dbo].[IdentifiableNote]