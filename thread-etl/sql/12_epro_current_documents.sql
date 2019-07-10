USE omopBuild
-- DROP TABLE omopBuild.dbo.EproGuidMap

BEGIN TRY
CREATE TABLE omopBuild.dbo.EproGuidMap (
	id INT IDENTITY PRIMARY KEY,
	guid UNIQUEIDENTIFIER,
	domain CHAR(4),
	omopId BIGINT,
	INDEX X_guid (guid),
	INDEX X_guid_omopid UNIQUE (guid,omopId)
)
END TRY
BEGIN CATCH END CATCH
GO

DROP PROCEDURE IF EXISTS dbo.wipeEproNotes;
GO

CREATE PROCEDURE dbo.wipeEproNotes AS
BEGIN
	DELETE FROM omopBuild.dbo.ExtractLog WHERE job='epro notes';
	DECLARE @nEPR BIGINT = omopBuild.dbo.getId(0,'nEPR')
	DELETE FROM omopBuild.dbo.IdentifiableNote WHERE note_id & @nEPR = @nEPR;
	DELETE FROM omopBuild.dbo.EproGuidMap;
END
GO

EXEC dbo.wipeEproNotes

DROP PROCEDURE IF EXISTS dbo.extractBatchEProClinicalNotes;
GO

CREATE PROCEDURE dbo.extractBatchEProClinicalNotes AS
BEGIN

	-- ------------------------------------------
	-- Load vocabmappings for sepciality -> note class
	-- ------------------------------------------

	DROP TABLE IF EXISTS #tmpNoteClassMap;

	CREATE TABLE #tmpNoteClassMap (
	metaSpecId CHAR(10) PRIMARY KEY,
	note_class_concept_id INT NOT NULL,
	note_source_value VARCHAR(50)
	);

	INSERT INTO #tmpNoteClassMap
	SELECT DISTINCT
		Sp.[code] as metaSpecId,
		omopConceptId as note_class_concept_id,
		cm.sourceTerm as note_source_value
	FROM 
		[EproLive-Copy].[dbo].[tlu_specialties] Sp  LEFT OUTER JOIN
		omopBuild.dbo.ConceptMapping cm ON CONVERT(INTEGER,cm.sourceId) = Sp.nationalCode
	WHERE 
		cm.sourceDomain = 'urn:epro:speciaties:national-code'
	;

	-- ------------------------------------------
	-- Find ids that are not yet imported
	-- ------------------------------------------

	DROP TABLE IF EXISTS  #tmpAllUnmatchedIds;
	

	CREATE TABLE #tmpAllUnmatchedIds (
		guid UNIQUEIDENTIFIER PRIMARY KEY,
		groupId INT,
		dateOffset FLOAT
	)
	
	INSERT INTO #tmpAllUnmatchedIds
	SELECT DISTINCT
		C.id as guid,
		sp.groupId,
		sp.dateOffset
	FROM
		omopBuild.dbo.StudyPopulation sp 
			INNER JOIN omopBuild.dbo.EproLookup el on sp.groupId = el.groupId
			INNER JOIN [EproLive-Copy].dbo.t_clinical_records CR on CR.patientId = el.pGuid
			INNER JOIN [EproLive-Copy].dbo.t_documents_current C ON CR.id = C.clinicalRecordId
			LEFT OUTER JOIN  omopBuild.dbo.EproGuidMap m ON C.id = m.guid
	WHERE
		C.userTypeId = 40000 -- comes from the v_documents_current definintion, unsure of significance
		AND C.deleted=0
		AND m.guid IS NULL

	-- ------------------------------------------
	-- set up batching framework
	-- ------------------------------------------

	DROP TABLE IF EXISTS  #tmpUnmatchedIds;

	CREATE TABLE #tmpUnmatchedIds (
		guid UNIQUEIDENTIFIER PRIMARY KEY,
		groupId INT,
		dateOffset FLOAT
	)

	DECLARE @batch INT = 100000
	DECLARE @rowsLeft INT = (SELECT COUNT(*) FROM #tmpAllUnmatchedIds)
	DECLARE @message VARCHAR(MAX)
	DECLARE @rowsDone INT = -1
	DECLARE @err INT = 0

	-- ------------------------------------------
	-- iterate over all rows that have not been transferred
	-- ------------------------------------------

	WHILE @rowsLeft > 0 BEGIN
	SET @message='Starting batch, remaining rows: '+CONVERT(CHAR,@rowsLeft);
	RAISERROR (@message, 10, 1)  WITH NOWAIT

		
		-- get a random batch of records to import
		DELETE FROM #tmpUnmatchedIds
		--TEST DECLARE @batch INT = 1000
		INSERT INTO #tmpUnmatchedIds SELECT TOP(@batch) * from #tmpAllUnmatchedIds

		BEGIN TRANSACTION T1
			-- create new sequential ids for records as BIGINT values using auto increment of primary key
			-- TODO: consider making this generic for all batche imports rather than re-using internal ids. map per omop table
			-- PROS: improves de-identification, can map to ints for better R support rather than bigints, remove need for namespacing
			-- CONS: have to go back and refactor, have to figure out better way to delete imports.
			INSERT INTO omopBuild.dbo.EproGuidMap 
			SELECT 
				guid,
				'nEPR',
				NULL
			FROM #tmpUnmatchedIds

			-- generate an omopId as BIGINT using namespacing strategy.
			UPDATE m
			SET m.omopId = omopBuild.dbo.getId(m.id,m.domain)
			FROM omopBuild.dbo.EproGuidMap m
			WHERE m.omopId IS NULL
		COMMIT TRANSACTION T1
	
		-- ------------------------------------------

	--concept_id	concept_name
	--44814637	Discharge summary
	--44814638	Admission note
	--44814639	Inpatient note
	--44814640	Outpatient note
	--44814641	Radiology report
	--44814642	Pathology report
	--44814643	Ancillary report
	--44814644	Nursing report
	--44814645	Note
	--44814646	Emergency department note
		BEGIN TRY
			INSERT INTO omopBuild.dbo.IdentifiableNote
			SELECT
				m.omopId as note_id,
				tmp.groupId as person_id,
				NULL as note_event_id,
				0 as note_event_field_concept_id,
				CONVERT(DATE,COALESCE(W.clinicDate,D.creationDate,W.creationDate,STATUS.writtenDate,D.datestamp)+tmp.dateOffset) AS note_date,
				CONVERT(DATETIME2,COALESCE(W.clinicDate,D.creationDate,W.creationDate,STATUS.writtenDate,D.datestamp)+tmp.dateOffset) AS note_datetime,
				44814645 as note_type_concept_id, --Note
				Sp.note_class_concept_id, --HL7 LOINC Document Type Vocabulary 
				D.subject as note_title,
				N.notesActive as note_text,
				0 as encoding_concept_id,
				4180182 as language_concept_id, --English language
				NULL as provider_id,
				NULL as visit_occurrence_id,
				NULL as visit_detail_id,
				Sp.note_source_value
			FROM 
				omopBuild.dbo.EproGuidMap m 
				LEFT JOIN #tmpUnmatchedIds tmp ON tmp.guid = m.guid 
				LEFT JOIN [EproLive-Copy].dbo.t_documents_current D on D.id = tmp.guid
				-- left join [EproLive-Copy].dbo.t_clinical_records CR on CR.id = D.clinicalRecordId
				LEFT JOIN [EproLive-Copy].dbo.t_document_workflow W ON W.versionId = D.versionId
				INNER JOIN #tmpNoteClassMap Sp ON COALESCE(D.metaSpecId,'0000091000') = Sp.metaSpecId --Unknown
				LEFT JOIN [EproLive-Copy].dbo.t_document_html N ON D.[versionId] = N.[versionId]
				LEFT JOIN [EproLive-Copy].dbo.t_letter_status_dtos STATUS ON STATUS.id = D.id
				-- LEFT JOIN [EproLive-Copy].dbo.t_clinic_appointments CA on CA.id = W.clinicAppointmentId
				-- LEFT JOIN [EproLive-Copy].dbo.t_pathways PA on CA.pathwayId = PA.id  
			WHERE 
				D.userTypeId = 40000
				AND D.deleted=0
			;
			SET @rowsDone = @@ROWCOUNT;

			INSERT INTO omopBuild.dbo.ExtractLog (db,tbl,pkName,job,maxPkValue,extractDate,comment) 
			VALUES ('epro','t_documents_current','id','epro notes',NULL, getdate(),
				CONCAT('inserted total ',@rowsDone,' documents out of ',@rowsLeft)
			);

			DELETE m FROM 
				#tmpAllUnmatchedIds m, 
				#tmpUnmatchedIds b
			WHERE m.guid = b.guid

			SET @rowsLeft = (SELECT COUNT(*) FROM #tmpAllUnmatchedIds)

		END TRY
		BEGIN CATCH
			SET @message= ERROR_MESSAGE()
			RAISERROR(@message,25,0)
	 	END CATCH
		
		

		
	END

	--TODO: PID fields - we need to pull all identifier from the database and UNPIVOT into a sigle column list.

	DROP TABLE IF EXISTS #tmpNoteClassMap;
	DROP TABLE IF EXISTS #tmpAllUnmatchedIds;
	DROP TABLE IF EXISTS #tmpUnmatchedIds;
END
GO
-- appropriate duplicates are present in patient pGUID and lookup groudId
-- SELECT * from 
--	(SELECT groupId FROM omopBuild.dbo.EproLookup GROUP BY groupId HAVING COUNT(*)>1) dups,
--	omopBuild.dbo.EproLookup el,
--	dbo.t_patients p
-- WHERE dups.groupId = el.groupId and el.pGuid = p.pGuid
-- ORDER BY el.groupId
-- many have an amalgamated with Id
-- could be that this is known by ePRO but question is does it do anything about them

EXEC dbo.extractBatchEProClinicalNotes
GO

-- The generation of the specialties map
--select * FROM (
--	select D.metaSpecId,count(*) as occ from [EproLive-Copy].dbo.t_documents_current D 
--	group by metaSpecId
--	) s FULL OUTER JOIN
--	[EproLive-Copy].dbo.tlu_specialties sp on COALESCE(s.metaSpecId,'0000091000') = sp.code
--	order by occ desc