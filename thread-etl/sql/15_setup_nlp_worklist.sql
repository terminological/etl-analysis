--USE [omop]
--GO

--/****** Object:  Table [dbo].[note_nlp]    Script Date: 14/06/2019 15:29:10 ******/
--SET ANSI_NULLS ON
--GO

--SET QUOTED_IDENTIFIER ON
--GO

--DROP TABLE IF EXISTS [dbo].[note_nlp]
--GO

--CREATE TABLE [dbo].[note_nlp] (
--	[note_nlp_id] [bigint] primary key identity(1,1),
--	[note_id] [bigint] NOT NULL,
--	[section_concept_id] [int] NOT NULL,
--	[snippet] [varchar](250) NULL,
--	[offset] [varchar](250) NULL,
--	[lexical_variant] [varchar](250) NOT NULL,
--	[note_nlp_concept_id] [int] NOT NULL,
--	[nlp_system] [varchar](250) NULL,
--	[nlp_date] [date] NOT NULL,
--	[nlp_datetime] [datetime2](7) NULL,
--	[term_exists] [varchar](1) NULL,
--	[term_temporal] [varchar](50) NULL,
--	[term_modifiers] [varchar](2000) NULL,
--	[note_nlp_source_concept_id] [int] NOT NULL,
--	[custom_code] [int] NOT NULL
--) ON [PRIMARY]
--GO

USE omopBuild

DROP TABLE IF EXISTS NlpAudit;
DELETE FROM omop.dbo.note_nlp;


CREATE TABLE NlpAudit (
	note_id bigint,
	event_time datetime,
	nlp_system varchar(250),
	nlp_system_instance varchar(250),
	event_type varchar(20),
	event_detail varchar(512),
	priority int
)
GO

DROP index if exists X_primary on IdentifiableNote
Create unique clustered index X_primary on IdentifiableNote (note_id);

Create clustered index X_note_id on NlpAudit (note_id);

CREATE UNIQUE INDEX X_unique_status ON NlpAudit (note_id,nlp_system,event_type,event_detail)

CREATE INDEX X_time on NlpAudit (event_time)
CREATE INDEX X_priority on NlpAudit (priority)
CREATE INDEX X_note_id_nlp_system on NlpAudit (note_id,nlp_system)

DROP VIEW IF EXISTS NlpWorklist
GO

CREATE VIEW NlpWorklist
AS
SELECT
	n.[note_id]
      ,[person_id]
      ,[note_event_id]
      ,[note_event_field_concept_id]
      ,[note_date]
      ,[note_datetime]
      ,[note_type_concept_id]
      ,[note_class_concept_id]
      ,[note_title]
      ,[note_text]
      ,[encoding_concept_id]
      ,[language_concept_id]
      ,[provider_id]
      ,[visit_occurrence_id]
      ,[visit_detail_id]
      ,[note_source_value]
	  , y.event_type as nlp_event_type
	  , y.nlp_system 
	  , y.nlp_system_instance
	  , y.event_time as nlp_event_time
	  , y.event_detail as nlp_event_detail
	  , y.priority as nlp_priority
FROM IdentifiableNote n
INNER JOIN (
		SELECT TOP(10000) 
			n1.*
		FROM
			NlpAudit n1 LEFT OUTER JOIN
			NlpAudit n2 ON
				n1.note_id = n2.note_id and n1.nlp_system = n2.nlp_system
				and (n1.event_type = 'PENDING' or n1.event_type = 'RETRY')
				and n2.event_type = 'PROCESSING'
		where n2.note_id IS NULL
		ORDER BY priority DESC
) y
on n.note_id = y.note_id		
	

GO

DROP VIEW IF EXISTS NlpComplete
GO

CREATE VIEW NlpComplete
AS
SELECT * FROM (
	SELECT 
		*,
		ROW_NUMBER() OVER(PARTITION BY note_id,nlp_system ORDER BY event_time DESC) as reverseTimeOrder
	FROM
		NlpAudit n1
	) x
WHERE reverseTimeOrder = 1
AND event_type in ('COMPLETE','CANCELLED')
GO

DROP VIEW IF EXISTS NlpProcessing
GO

CREATE VIEW NlpProcessing
AS
SELECT * FROM (
	SELECT 
		*,
		ROW_NUMBER() OVER(PARTITION BY note_id,nlp_system ORDER BY event_time DESC) as reverseTimeOrder
	FROM
		NlpAudit n1
	) x
	WHERE reverseTimeOrder = 1
	AND event_type in ('PROCESSING')
GO


-- Add a few docs to worklist
INSERT INTO NlpAudit
SELECT
	n.note_id, 
	GETDATE() as event_time,
	'CTAKESv1' as nlp_system,
	NULL as nlp_system_instance,
	'PENDING' as event_type,
	NULL as event_detail,
	5 as priority 
FROM IdentifiableNote n 
	LEFT OUTER JOIN NlpAudit p ON n.note_id = p.note_id AND p.nlp_system = 'CTAKESv1'
WHERE p.note_id IS NULL


SELECT * FROM NlpWorklist order by nlp_priority, note_datetime



