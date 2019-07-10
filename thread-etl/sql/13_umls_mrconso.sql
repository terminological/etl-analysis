USE omopBuild;

-- N.B. https://www.nlm.nih.gov/research/umls/implementation_resources/community/dbloadscripts/metathesaurus_sql.txt

DROP TABLE IF EXISTS dbo.UmlsMRCONSO;
CREATE TABLE dbo.UmlsMRCONSO (
    CUI	char(8) NOT NULL,
    LAT	char(3) NOT NULL,
    TS	char(1) NOT NULL,
    LUI	varchar(10) NOT NULL,
    STT	varchar(3) NOT NULL,
    SUI	varchar(10) NOT NULL,
    ISPREF	char(1) NOT NULL,
    AUI	varchar(9) NOT NULL,
    SAUI	varchar(50) DEFAULT null,
    SCUI	varchar(100) DEFAULT null,
    SDUI	varchar(100) DEFAULT null,
    SAB	varchar(40) NOT NULL,
    TTY	varchar(40) NOT NULL,
    CODE	varchar(100) NOT NULL,
    STR	text NOT NULL,
    SRL	int NOT NULL,
    SUPPRESS	char(1) NOT NULL,
    CVF	int DEFAULT null
);

bulk insert
dbo.UmlsMRCONSO  from
'P:/data/umls/MRCONSO.RRF'
with (
fieldquote='',fieldterminator='|', rowterminator='|\n', batchsize=10000
--codepage=65001, 
--keepnulls
)

CREATE INDEX X_MRCONSO_CUI ON dbo.UmlsMRCONSO(CUI);

CREATE INDEX X_MRCONSO_SUI ON dbo.UmlsMRCONSO(SUI);

CREATE INDEX X_MRCONSO_LUI ON dbo.UmlsMRCONSO(LUI);

CREATE INDEX X_MRCONSO_CODE ON dbo.UmlsMRCONSO(CODE);

CREATE INDEX X_MRCONSO_SAB_TTY ON dbo.UmlsMRCONSO(SAB,TTY);
GO

DROP TABLE IF EXISTS CuiOmopMap;
GO

CREATE TABLE CuiOmopMap (
	[CUI] [char](8) NOT NULL,
	[source_concept_id] [int] NULL,
	[concept_id] [int] NULL
) ON [PRIMARY]
GO

INSERT INTO CuiOmopMap
SELECT CUI, source_concept_id, concept_id FROM (
	SELECT 
		u.CUI,
		c.source_concept_id,
		c.concept_id,
		ROW_NUMBER() OVER(PARTITION BY u.CUI,c.concept_id ORDER BY c.source_concept_id) as uniquifier,
		ROW_NUMBER() OVER(PARTITION BY u.CUI ORDER BY c.concept_id DESC) as filter
	FROM 
		(
			SELECT *, LEFT(SAB,1) as src
			 FROM omopBuild.dbo.UmlsMRCONSO WHERE SAB in ('SNOMEDCT_US','RXNORM') --AND ISPREF='Y'
		) u
		INNER JOIN 
		(
			SELECT 
				c1.concept_id as source_concept_id, 
				c1.concept_code, c1.concept_name, 
				COALESCE(c2.concept_id,0) as concept_id,
				LEFT(c1.vocabulary_id,1) as src
			FROM
			[omop].[dbo].[concept] c1 LEFT OUTER JOIN
			[omop].[dbo].[concept_relationship] cr ON c1.concept_id = cr.concept_id_1  AND cr.relationship_id='Maps to' LEFT OUTER JOIN
			[omop].[dbo].[concept] c2 ON c2.concept_id = cr.concept_id_2
			WHERE c1.vocabulary_id in ('SNOMED','RxNorm') AND c1.invalid_reason IS NULL
		) c
		ON u.CODE = c.concept_code and u.src = c.src
) x WHERE
uniquifier = 1 and not ( concept_id = 0 and filter > 1 )
-- ORDER BY CUI ASC

CREATE UNIQUE INDEX X_CuiOmopMap_id ON CuiOmopMap (CUI,concept_id)
CREATE INDEX X_CuiOmopMap_CUI ON CuiOmopMap (CUI)


SELECT degree,count(*) as number FROM (
	SELECT count(*) as degree, CUI FROM CuiOmopMap GROUP BY CUI
) x GROUP BY degree


-- SELECT DISTINCT SAB from dbo.UmlsMRCONSO

SELECT TOP(100) * FROM CuiOmopMap c1 where CUI = 'C0004057'