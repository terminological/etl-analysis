
USE omopBuild;
GO

-- NGRAM index stolen from
-- https://social.technet.microsoft.com/wiki/contents/articles/33419.sql-server-implementation-of-n-gram-search-index.aspx


-- https://en.wikipedia.org/wiki/Nearest_neighbor_search
-- https://www.cs.ubc.ca/research/flann/

-- -------------------------------------------------

DROP FUNCTION IF EXISTS dbo.fn4GramRange2;
GO

CREATE FUNCTION dbo.fn4GramRange2(@max INT)
	RETURNS @ids TABLE (beginning INT)
AS
BEGIN
INSERT INTO @ids
SELECT 
	1+ones.n+16*tens.n
	as beginning
FROM 
	(VALUES(0x00),(0x01),(0x02),(0x03),(0x04),(0x05),(0x06),(0x07),(0x08),(0x09),(0x0A),(0x0B),(0x0C),(0x0D),(0x0E),(0x0F)) ones(n)
	,(VALUES(0x00),(0x01),(0x02),(0x03),(0x04),(0x05),(0x06),(0x07),(0x08),(0x09),(0x0A),(0x0B),(0x0C),(0x0D),(0x0E),(0x0F)) tens(n)
WHERE 1+ones.n+16*tens.n < @max
ORDER BY 1
RETURN;
END
GO

DROP FUNCTION IF EXISTS dbo.fn4GramStringNorm2;
GO

CREATE FUNCTION dbo.fn4GramStringNorm2(@search VARCHAR(255))
	RETURNS VARCHAR(255)
AS
BEGIN
	RETURN '  '+UPPER(@search)+'  '
END
GO

DROP FUNCTION IF EXISTS dbo.fn4GramCalculate2;
GO

CREATE FUNCTION dbo.fn4GramCalculate2(@search VARCHAR(255),@beginning int)
RETURNS BINARY(4)
AS
BEGIN
	RETURN 
	CONVERT(BINARY(4),SUBSTRING(@search,@beginning,4))
END
GO

DROP FUNCTION IF EXISTS dbo.fnPrev4GramCalculate2;
GO

CREATE FUNCTION dbo.fnPrev4GramCalculate2(@search VARCHAR(255),@beginning int)
RETURNS BINARY(1)
AS
BEGIN
	RETURN 
	IIF(@beginning=1,0,CONVERT(BINARY(1),SUBSTRING(@search,@beginning-1,1)))
END
GO

-- SELECT dbo.fn4GramCalculate('ZZZZ',1);

-- -----------------------------------------------

DROP PROCEDURE IF EXISTS dbo.fn4GramIndex2;
GO
DROP FUNCTION IF EXISTS dbo.fnBulkSearch4Gram2;
GO

DROP TYPE IF EXISTS dbo.IndexInputType2;
CREATE TYPE dbo.IndexInputType2 AS TABLE
	(concept_id INT, concept_name VARCHAR(255)
)

DROP TABLE IF EXISTS dbo.SearchIndex2;
CREATE TABLE dbo.SearchIndex2 
	(
	concept_id INT, 
	ngram BINARY(4), 
	prev_ngram BINARY(1), 
	ngram_length smallint, 
	seq smallint,
	id smallint,
	remaining_ngrams smallint,
	this_level smallint,
	next_level smallint,
	INDEX X_SearchIndex_ngram NONCLUSTERED (id,ngram),
	INDEX X_SearchIndex_concept_id NONCLUSTERED (id,concept_id),
	INDEX X_SearchIndex_seq NONCLUSTERED (id,seq),
	-- INDEX X_SearchIndex_id NONCLUSTERED (id) INCLUDE (concept_id,ngram,seq,ngram_length)
)

DROP TABLE IF EXISTS dbo.SearchFreq2;
CREATE TABLE dbo.SearchFreq2 
	(
	ngram BINARY(4), 
	id smallint,
	concepts int,
	INDEX X_SearchFreq_ngram NONCLUSTERED (id,ngram),
	INDEX X_SearchFreq_concepts NONCLUSTERED (id,concepts)
)



DROP TYPE IF EXISTS dbo.BulkSearchInputType2;
CREATE TYPE dbo.BulkSearchInputType2 AS TABLE   
	( local_id VARCHAR(255), search_term VARCHAR(255) );  
GO 

DROP TYPE IF EXISTS dbo.SearchOutputType2;
CREATE TYPE dbo.SearchOutputType2 AS TABLE   
	( concept_id int, strength float,
	INDEX X_concept_id (concept_id))
GO 

DROP TYPE IF EXISTS dbo.BulkSearchOutputType2;
CREATE TYPE dbo.BulkSearchOutputType2 AS TABLE   
	( local_id VARCHAR(255), concept_id int, strength float,
	INDEX X_concept_id (concept_id),
	INDEX X_local_id (local_id));  
GO 

-- -------------------------------------------------

DROP PROCEDURE IF EXISTS dbo.fn4GramIndex2;
GO

CREATE PROCEDURE dbo.fn4GramIndex2(
	@id smallint, 
	@input dbo.IndexInputType2 READONLY) 
AS
BEGIN

-- PRINT 'loading ngram index';
DELETE FROM SearchIndex2 WHERE id=@id;
DELETE FROM SearchFreq2 WHERE id=@id;

-- TEST: DECLARE @id AS INT=0;DECLARE @input AS dbo.IndexInputType;INSERT INTO @input SELECT concept_id,concept_name from omop.dbo.concept where vocabulary_id LIKE 'HES%'

INSERT INTO SearchIndex2
SELECT 
	concept_id,
	dbo.fn4GramCalculate2(normalised_name,beginning) as ngram,
	dbo.fnPrev4GramCalculate2(normalised_name,beginning) as prev_ngram,
	LEN(normalised_name+'x')-4 as ngram_length, -- 'A' 1 becomes '  A  ' 5 becomes ['  A ',' A  '] 2
	beginning,
	@id as id,
	0 as remaining_ngrams,
	0 as this_level,
	0 as next_level
	FROM
	(
		SELECT 
			i.concept_id,
			dbo.fn4GramStringNorm2(i.concept_name) as normalised_name
		FROM @input i
	) c,
	 dbo.fn4GramRange2(253) n 
WHERE n.beginning<=LEN(normalised_name+'x')-4
ORDER BY concept_id, beginning
;

INSERT INTO SearchFreq2
SELECT 
	ngram,
	id,
	count(distinct concept_id) as concepts
FROM SearchIndex2
group BY id,ngram;

UPDATE dbo.SearchIndex2
	SET	this_level = f1.concepts
	FROM
	dbo.SearchIndex2 s INNER JOIN SearchFreq2 f1 ON s.ngram=f1.ngram and f1.id = @id

UPDATE dbo.SearchIndex2
	SET	next_level = t.next_level,
	remaining_ngrams = t.remaining_ngrams
	FROM 
		dbo.SearchIndex2 s INNER JOIN
		(
			SELECT
				s1.concept_id,
				s1.ngram,
				min(s2.this_level) as next_level,
				count(s2.ngram) as remaining_ngrams
		FROM
			dbo.SearchIndex2 s1,
			dbo.SearchIndex2 s2
		WHERE
			s1.concept_id = s2.concept_id
			AND s2.this_level > s1.this_level
		GROUP BY 
			s1.concept_id, s1.ngram
		) t ON (t.concept_id = s.concept_id AND t.ngram = s.ngram)

	-- TODO: calculate frequency of Ngram in words i.e. an IDF measure
	-- change search function to use a recursive CTE to search first for ngram matches with low numbers of target words
	-- stop looking for a given search / match pair when threshold for inclusion reached
	-- can figure out for a given target what level of commonality needs to reached to make it pass a given threshold.
	-- so if threshold is 50% we cannot reach 50% unless at least half of target ngrams matched regardless of number of search ngrams
	-- in fact number fo terms matched > length target / (1-threshold value) - not useful
	-- but actually you are still looking to figure out which ones cannot be bigger than the ones you have already found.

	-- TODO: possible to include previous ngram into index using a single byte as delta. This enables calculation of the positional match at same time


-- PRINT 'indexing ngrams';
-- PRINT 'done';

RETURN
END
GO

DECLARE @indexInput2 AS dbo.IndexInputType2;
INSERT INTO @indexInput2 
SELECT concept_id,concept_name from omop.dbo.concept where vocabulary_id LIKE 'HES%'
EXEC dbo.fn4gramIndex2 0, @indexInput2

SELECT * FROM [omopBuild].[dbo].[SearchIndex2]
  ORDER BY concept_id, seq

-- --------------------------------------------------



DROP FUNCTION IF EXISTS dbo.fnBulkSearch4Gram2;
GO

CREATE FUNCTION dbo.fnBulkSearch4Gram2(
	@search dbo.BulkSearchInputType2 READONLY,
	@id smallint,
    @factor decimal(9, 4),
	@max int
) RETURNS @results TABLE --BulkSearchOutputType
	( local_id VARCHAR(255), concept_id int, similarity float )
AS
BEGIN

	-- TEST DECLARE @max as INT=10; DECLARE @factor as FLOAT=0.5; DECLARE @id as INT=0;DECLARE @search AS dbo.BulkSearchInputType; INSERT INTO @search SELECT TOP(10) c.concept_id, c.concept_name FROM omop.dbo.concept c where c.vocabulary_id LIKE 'HES %';


	DECLARE @searchTermsNgrams2 AS TABLE (
		local_id VARCHAR(255), 
		ngram BINARY(4), 
		prev_ngram BINARY(1), 
		ngram_length smallint, 
		seq smallint,
		remaining_ngrams smallint,
		this_level smallint,
		next_level smallint,
		INDEX X_SearchTerms_ngram NONCLUSTERED (ngram),
		INDEX X_SearchIndex_concept_id NONCLUSTERED (local_id)
	);
	
	-- Calculate NGrams for the search terms
	INSERT INTO @searchTermsNgrams2
	SELECT
		a.local_id,
		dbo.fn4GramCalculate2(a.norm_term,b.beginning) as ngram,
		dbo.fnPrev4GramCalculate2(a.norm_term,b.beginning) as prevNgram,
		ngram_length,
		b.beginning as seq,
		0 as remaining_ngrams,
		0 as this_level,
		0 as next_level
	FROM
		(
			SELECT 
			local_id,
			dbo.fn4GramStringNorm2(search_term) as norm_term,
			LEN(dbo.fn4GramStringNorm2(search_term)+'x')-4 as ngram_length
			FROM @search
		) a,
		dbo.fn4GramRange2(253) b 
	WHERE b.beginning <= ngram_length;

	UPDATE @searchTermsNgrams2
	SET	this_level = f1.concepts
	FROM
	@searchTermsNgrams2 s INNER JOIN SearchFreq2 f1 ON s.ngram=f1.ngram and f1.id = @id

	UPDATE @searchTermsNgrams2
	SET	next_level = t.next_level,
	remaining_ngrams = t.remaining_ngrams
	FROM 
		@searchTermsNgrams2 s INNER JOIN
		(
			SELECT
				s1.local_id,
				s1.ngram,
				min(s2.this_level) as next_level,
				count(s2.ngram) as remaining_ngrams
		FROM
			@searchTermsNgrams2 s1,
			@searchTermsNgrams2 s2
		WHERE
			s1.local_id = s2.local_id
			AND s2.this_level > s1.this_level
		GROUP BY 
			s1.local_id, s1.ngram
		) t ON (t.local_id = s.local_id AND t.ngram = s.ngram)

	-- SELECT * from @searchTermsNgrams2 order by local_id, seq;
-- =======================================================

-- TODO:


-- match Ngrams starting on level where orderRank = 1
SELECT * FROM (
	SELECT *, RANK() OVER(PARTITION BY local_id order by this_level) as orderRank from @searchTermsNgrams2 
	WHERE this_level>0
) t where orderRank=1
-- This is the one most discriminitive match for a 

-- get all ngram matches where this_level matches source and target
-- and total remaining score > minimum score constraint (write this as a function)
-- and ngram_length for source and target does not violate minimum score constraint
-- N.B. This is the big set. - more optimisation here - can we prevent consideration of matches that are above minimum score cut off but highly unlikely to be in top N without considering them all anyway

-- aggregate by local_id and concept_id and calculate score  / distance on matches so far
-- calculate minimum (=current score?) and maximum possible scores based on remaining ngrams (source and target assuming best possible matching)
-- find out top-Nth highest minimum score and prune results with a maximum score less than this. Do this in a way that prevents them being rediscovered.

-- Consider next level for a given source and match
-- update current score based on next_level match
	
	;

-- =======================================================
	--DECLARE @searchTermMatchFilter AS TABLE
	--	(local_id VARCHAR(255),concept_id INT,
	--	INDEX X_local_id_concept_id (local_id,concept_id)
	--)

	--INSERT INTO @searchTermMatchFilter
	--SELECT
	--	c.local_id,
	--	d.concept_id
	--FROM
	--	@searchTermsNgrams c 
	--	INNER JOIN SearchIndex d ON (c.ngram = d.ngram and d.id = @id)
	--GROUP BY c.local_id, d.concept_id
	--HAVING COUNT(DISTINCT c.seq)*2 > MAX(d.ngram_length+c.ngram_length)*@factor;
	
	---- SELECT * from @searchTermMatchFilter;

	----SELECT local_id,concept_id FROM (
	----	SELECT DISTINCT
	----			c.local_id,
	----			d.concept_id,
	----			c.seq,
	----			d.ngram_length + c.ngram_length as maxLength
	----		FROM
	----			@searchTermsNgrams c 
	----			INNER JOIN SearchIndex d ON (c.ngram = d.ngram and d.id = @id)
	----	) t 
	----GROUP BY local_id,concept_id
	----HAVING count(*)*2 > max(maxLength)*@factor
	--		-- INNER JOIN SearchIndex d2 ON d.concept_id = d2.concept_id -- select out only those with more than one ngram match
	--		-- INNER JOIN @searchTermsNgrams c2 on (c2.ngram = d2.ngram and d2.id = @id and c2.local_id = c.local_id and c2.ngram <> c.ngram)
	--	--GROUP BY
	--	--	c.local_id, d.concept_id
	--	--HAVING count(d.concept_id)*2 > max(d.ngram_length + c.ngram_length)*@factor
	--	-- ORDER BY matches desc

	--DECLARE @searchTermMatches AS TABLE
	--	(local_id VARCHAR(255),concept_id INT,searchSeq SMALLINT,matchSeq SMALLINT, joint_ngram_length INT, seqDelta INT,
	--	INDEX X_local_id_concept_id (local_id,concept_id,searchSeq)
	--)
	---- Join this to the main search table
	--INSERT INTO @searchTermMatches
	--	SELECT
	--		c.local_id,
	--		d.concept_id,
	--		c.seq as searchSeq,
	--		d.seq as matchSeq,
	--		d.ngram_length + c.ngram_length as joint_ngram_length,
	--		ABS(c.seq-d.seq) as seqDelta
	--	FROM
	--		@searchTermMatchFilter f 
	--		INNER JOIN @searchTermsNgrams c ON c.local_id = f.local_id
	--		INNER JOIN SearchIndex d ON (f.concept_id = d.concept_id and d.id = @id)
	--	WHERE c.ngram = d.ngram
	
	---- SELECT count(*) from  @searchTermMatches;

	--DECLARE @searchTermMatchStats AS TABLE
	--	(local_id VARCHAR(255),
	--	concept_id INT,
	--	aligned SMALLINT,searchSeq SMALLINT,matchSeq SMALLINT,matchSeqDelta SMALLINT,searchSeqDelta SMALLINT, joint_ngram_length INT,
	--	INDEX X_stats (local_id, concept_id, joint_ngram_length)
	--	-- index X_ensure_unique UNIQUE(local_id,concept_id,searchSeq,matchSeq)
	--	)
	--INSERT INTO @searchTermMatchStats
	--SELECT 
	--	e.local_id,
	--	e.concept_id,
	--	iif(e.searchSeq=e.matchSeq,1,0) as aligned,
	--	e.searchSeq,
	--	e.matchSeq,
	--	(e.matchSeq - LAG(e.matchSeq,1,0) OVER(PARTITION by e.local_id,e.concept_id ORDER BY e.searchSeq)) as matchSeqDelta,
	--	(e.searchSeq - LAG(e.searchSeq,1,0) OVER(PARTITION by e.local_id,e.concept_id ORDER BY e.searchSeq)) as searchSeqDelta,
	--	e.joint_ngram_length
	--FROM (
	--	SELECT
	--		d.*,
	--		ROW_NUMBER() OVER(PARTITION BY d.local_id,d.concept_id,d.searchSeq ORDER BY d.seqDelta,d.searchSeq) as filter
	--		-- filter should ensure a unique match for each search-match ngram pair
	--	FROM @searchTermMatches d
	--) e
	--WHERE e.filter = 1
	
    
	---- INSERT INTO @results
	--SELECT local_id,concept_id,similarity FROM (
	--	SELECT 
	--		local_id,concept_id,
	--		1 - SQRT(
	--				IIF(
	--					(ngram_euclidean_squared/max_euclidean_squared) < 0 , 0 , (ngram_euclidean_squared/max_euclidean_squared)
	--				)
	--			) as similarity,
	--		ROW_NUMBER() OVER(PARTITION BY local_id ORDER BY ngram_euclidean_squared) as rowNo
	--	FROM (
	--		SELECT 
	--			f.local_id,f.concept_id,
	--			CONVERT(FLOAT,
	--				f.joint_ngram_length-2*count(f.searchSeq)
	--				+f.joint_ngram_length-2*sum(iif(f.searchSeqDelta=f.matchSeqDelta,1,0)) -- as relPosNgramEuclidean
	--			) AS ngram_euclidean_squared,
	--			CONVERT(FLOAT,2*f.joint_ngram_length) as max_euclidean_squared
	--		FROM @searchTermMatchStats f
	--		GROUP BY f.local_id, f.concept_id, f.joint_ngram_length
	--	) x
	--) w
	--WHERE w.rowNo <= @max
	--AND similarity > @factor
	--;

RETURN
END
GO


-- ---------------------------------------------------

--DROP FUNCTION IF EXISTS dbo.fnBulkSearch4Gram2;
--GO

--CREATE FUNCTION dbo.fnBulkSearch4Gram2(
--	@search dbo.BulkSearchInputType READONLY,
--	@id smallint,
--    @factor decimal(9, 4),
--	@max int
--) RETURNS @results TABLE --BulkSearchOutputType
--	( local_id VARCHAR(255), concept_id int, similarity float )
--AS
--BEGIN

--	 TEST DECLARE @max as int = 10; DECLARE @factor as FLOAT=0.5; DECLARE @id as INT=4;DECLARE @search AS dbo.BulkSearchInputType; INSERT INTO @search SELECT TOP(5) c.concept_id, c.concept_name FROM omop.dbo.concept c where c.vocabulary_id = 'OPCS4';
--	WITH 
	
--	cteSearchTermsNgrams (local_id, ngram, seq, ngram_length)
--	AS (
--		SELECT
--			a.local_id,
--			dbo.fn4GramCalculate(a.norm_term,b.beginning) as ngram,
--			b.beginning as seq,
--			ngram_length
--		FROM
--			(
--				SELECT 
--				local_id,
--				dbo.fn4GramStringNorm(search_term) as norm_term,
--				LEN(dbo.fn4GramStringNorm(search_term))-3 as ngram_length
--				FROM @search
--			) a,
--			dbo.fn4GramRange(253) b 
--	WHERE b.beginning <= ngram_length
--	),

--	cteSearchTermMatchFilter (local_id, concept_id)
--	AS (
--		SELECT
--			c.local_id,
--			d.concept_id
--		FROM
--			cteSearchTermsNgrams c 
--			INNER JOIN SearchIndex d ON (c.ngram = d.ngram and d.id = @id)
--		GROUP BY c.local_id, d.concept_id
--		HAVING COUNT(DISTINCT c.seq)*2 > MAX(d.ngram_length+c.ngram_length)*@factor
--	),
	
--	cteSearchTermMatches (local_id, concept_id, searchSeq, matchSeq, joint_ngram_length, seqDelta) 
--	AS (
--		SELECT
--			c.local_id,
--			d.concept_id,
--			c.seq as searchSeq,
--			d.seq as matchSeq,
--			d.ngram_length + c.ngram_length as joint_ngram_length,
--			ABS(c.seq-d.seq) as seqDelta
--		FROM
--			cteSearchTermMatchFilter f
--			INNEr JOIN cteSearchTermsNgrams c ON c.local_id = f.local_id
--			INNER JOIN SearchIndex d ON (f.concept_id = d.concept_id and d.id = @id and c.ngram=d.ngram)
--			cteSearchTermsNgrams c 
--			INNER JOIN SearchIndex d ON (c.ngram=d.ngram and d.id = @id)
--	),
	
--	cteSearchTermMatchStats (local_id, concept_id, aligned,
--		searchSeq, matchSeq, matchSeqDelta, searchSeqDelta, joint_ngram_length)
--	AS (
--		SELECT 
--			e.local_id,
--			e.concept_id,
--			iif(e.searchSeq=e.matchSeq,1,0) as aligned,
--			e.searchSeq,
--			e.matchSeq,
--			(e.matchSeq - LAG(e.matchSeq,1,0) OVER(PARTITION by e.local_id,e.concept_id ORDER BY e.searchSeq)) as matchSeqDelta,
--			(e.searchSeq - LAG(e.searchSeq,1,0) OVER(PARTITION by e.local_id,e.concept_id ORDER BY e.searchSeq)) as searchSeqDelta,
--			e.joint_ngram_length
--		FROM (
--			SELECT
--				d.*,
--				ROW_NUMBER() OVER(PARTITION BY d.local_id,d.concept_id,d.searchSeq ORDER BY d.seqDelta,d.searchSeq) as filter
--				 80% of query here
--				 filter should ensure a unique match for each search-match ngram pair
--			FROM cteSearchTermMatches d
--		) e
--		WHERE e.filter = 1
--	)
    
--	 INSERT INTO @results
--	SELECT local_id,concept_id,similarity FROM (
--		SELECT 
--			local_id,concept_id,
--			1 - SQRT(
--					IIF(
--						(ngram_euclidean_squared/max_euclidean_squared) < 0 , 0 , (ngram_euclidean_squared/max_euclidean_squared)
--					)
--				) as similarity,
--			ROW_NUMBER() OVER(PARTITION BY local_id ORDER BY ngram_euclidean_squared) as rowNo
--		FROM (
--			SELECT 
--				f.local_id,f.concept_id,
--				CONVERT(FLOAT,
--					f.joint_ngram_length-2*count(f.searchSeq)
--					+f.joint_ngram_length-2*sum(iif(f.searchSeqDelta=f.matchSeqDelta,1,0)) -- as relPosNgramEuclidean
--				) AS ngram_euclidean_squared,
--				CONVERT(FLOAT,2*f.joint_ngram_length) as max_euclidean_squared
--			FROM cteSearchTermMatchStats f
--			GROUP BY f.local_id, f.concept_id, f.joint_ngram_length
--		) x
--	) w
--	WHERE w.rowNo <= @max
--	AND similarity > @factor
--	;

--RETURN
--END
--GO

-- ---------------------------------------------------

--DROP TABLE IF EXISTS #tmpCounts
--SELECT 
--	[from_code] as local_id
--    ,MIN([original_display_name]) as search_term
--	,COUNT(*) as occurrence
--INTO #tmpCounts
--FROM [ordercomms_review].[dbo].[npisynonym] s INNER JOIN
--	[ordercomms_review].[dbo].rnpi r ON s.[npisynonym_id]=r.[npisynonym_id]
--	GROUP BY from_code
--;

--SELECT COUNT(*) FROM #tmpCounts

--DROP TABLE IF EXISTS #tmpIndexInput
--SELECT 
--	concept_id, concept_name 
--	 DISTINCT concept_class_id, vocabulary_id	
--	 count(*) = 22418
--INTO #tmpIndexInput
--FROM [omop].[dbo].[concept]
--WHERE 
--standard_concept = 'S'
--AND domain_id = 'Measurement'
--AND concept_class_id = 'Clinical Observation'
--AND vocabulary_id='LOINC'
--;


--SELECT Count(*) FROM #tmpIndexInput

-- DECLARE @conceptIndex as dbo.SearchIndexType
-- INSERT INTO @conceptIndex

--DECLARE @indexInput AS dbo.IndexInputType;
--INSERT INTO @indexInput SELECT * FROM #tmpIndexInput;
--EXEC dbo.fn4gramIndex 1, @indexInput

--SELECT COUNT(*) FROM SearchIndex

-- FROM @conceptIndex
-- CREATE INDEX X_concept_id ON #tmpConceptIndex (concept_id)
-- CREATE INDEX X_ngram ON #tmpConceptIndex (ngram)

--DECLARE @bulkSearchInput AS dbo.BulkSearchInputType
--INSERT INTO @bulkSearchInput
--SELECT local_id,search_term FROM #tmpCounts

--DROP TABLE IF EXISTS #tmpBulkSearchOutput
--SELECT * 
--INTO #tmpBulkSearchOutput
--FROM dbo.fnBulkSearch4Gram(
--	@bulkSearchInput,
--	1,
--	0.1,
--	5)

--SELECT b.local_id,search_term,0 as review,similarity,b.concept_id,concept_name,occurrence FROM
--	#tmpCounts t 
--	LEFT JOIN #tmpBulkSearchOutput b ON b.local_id = t.local_id COLLATE Latin1_General_CI_AS
--	LEFT JOIN #tmpIndexInput i ON b.concept_id = i.concept_id
--	ORDER BY occurrence DESC, b.local_id, similarity DESC



--/****** Script for SelectTopNRows command from SSMS  ******/

--DECLARE @indexInput2 AS dbo.IndexInputType;
--INSERT INTO @indexInput2 
--SELECT c.concept_id, c.concept_name FROM 
--[omop].[dbo].[concept] c,
--[omop].[dbo].[concept_ancestor] a
--where ancestor_concept_id=36209248
--AND descendant_concept_id=concept_id
--AND standard_concept = 'S'

--EXEC dbo.fn4gramIndex 3, @indexInput2

--DECLARE @bulkSearchInput2 AS dbo.BulkSearchInputType
--INSERT INTO @bulkSearchInput2
--SELECT TOP (1000) [id],[text]
--FROM [EproLive-Copy].[dbo].[tlu_specialties]

--DROP TABLE IF EXISTS #tmpBulkSearchOutput2
--SELECT * 
--INTO #tmpBulkSearchOutput2
--FROM dbo.fnBulkSearch4Gram(
--	@bulkSearchInput2,
--	3,
--	0.1,
--	5)

--SELECT s.id, s.text, m.similarity, m.concept_id, c.concept_name FROM 
--[EproLive-Copy].[dbo].[tlu_specialties] s LEFT OUTER JOIN
--#tmpBulkSearchOutput2 m ON s.id = m.local_id
--LEFT OUTER JOIN [omop].[dbo].[concept] c on c.concept_id = m.concept_id
--order by id ASC, similarity DESC
