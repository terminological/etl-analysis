Use ordercomms_review

--NODES

drop table if exists RobsDatabase.dbo.tmpIdNode;

Create table RobsDatabase.dbo.tmpIdNode (
	node_id varchar(35) primary key,
	type char(1),
	graph_id int 
	)

Insert into RobsDatabase.dbo.tmpIdNode
select 
	concat('E',p.patient_id) as node_id,
	'E' as type,
	p.patient_id as graph_id
from patient p;

Insert into RobsDatabase.dbo.tmpIdNode
select 
	concat('N',p.nhsno) as node_id,
	'N' as type,
	min(p.patient_id) as graph_id
from patient p 
where NOT(p.nhsno like ''
			OR p.nhsno like '""'
			or p.nhsno like '000%'
			or p.nhsno like '999%'
			or p.nhsno like '11111%'
			or p.nhsno like '22222%'
			or p.nhsno is null)
group by p.nhsno;

Insert into RobsDatabase.dbo.tmpIdNode
select 
	concat('M',l.hospital_no) as node_id,
	'M' as type,
	min(l.patient_id) as graph_id
from lab_patient l
where l.hospital_no is not null
group by l.hospital_no
;



-- EDGES

drop table if exists RobsDatabase.dbo.tmpIdEdge;

Create table RobsDatabase.dbo.tmpIdEdge (
	source_node_id varchar(35) not null,
	target_node_id varchar(35) not null,
	graph_id int,
	constraint uniq_edge unique(source_node_id, target_node_id)
)

Insert into RobsDatabase.dbo.tmpIdEdge select
-- select TOP(100)
	concat('E',p.patient_id) as source_node_id,
	concat('N',p.nhsno) as target_node_id,
	p.patient_id as graph_id
from patient p 
where NOT(p.nhsno like ''
			OR p.nhsno like '""'
			or p.nhsno like '000%'
			or p.nhsno like '999%'
			or p.nhsno like '11111%'
			or p.nhsno like '22222%'
			or p.nhsno is null);

Insert into RobsDatabase.dbo.tmpIdEdge select 
-- select top(100)
	concat('E',l.patient_id) as source_node_id,
	concat('M',l.hospital_no) as target_node_id,
	min(l.patient_id) as graph_id
from lab_patient l
where l.hospital_no is not null
-- and l.patient_id=1000000
group by patient_id,hospital_no
;

-- FIND MINIMUM GRAPH IDS
-- basically the strategy here is to pick the lowest graph_id of nodes for a given source_node_id, target_node_id and 
-- update the edges. Then pick the lowest graph id of an edge and update each node. repeat until no changes.

DECLARE @tmp int;
SET @tmp = 1;
WHILE @tmp <> 0
BEGIN

	UPDATE e 
	SET e.graph_id = IIF(n1.graph_id < n2.graph_id, n1.graph_id, n2.graph_id)
	FROM 
	RobsDatabase.dbo.tmpIdEdge e 
	INNER JOIN RobsDatabase.dbo.tmpIdNode n1 on n1.node_id = e.source_node_id
	INNER JOIN RobsDatabase.dbo.tmpIdNode n2 on n2.node_id = e.target_node_id
	WHERE n1.graph_id <> n2.graph_id;

	SET @tmp = @@ROWCOUNT;

	UPDATE n 
	SET n.graph_id = e.graph_id
	FROM 
	RobsDatabase.dbo.tmpIdNode n
	INNER JOIN RobsDatabase.dbo.tmpIdEdge e on n.node_id = e.source_node_id
	WHERE e.graph_id < n.graph_id

	SET @tmp = @tmp+@@ROWCOUNT;

	UPDATE n 
	SET n.graph_id = e.graph_id
	FROM 
	RobsDatabase.dbo.tmpIdNode n
	INNER JOIN RobsDatabase.dbo.tmpIdEdge e on n.node_id = e.target_node_id
	WHERE e.graph_id < n.graph_id

	SET @tmp = @tmp+@@ROWCOUNT;

END

-- CREATE GRAPH IDS
drop table if exists RobsDatabase.dbo.tmpIdGraph;

create table RobsDatabase.dbo.tmpIdGraph (
	graph_id int primary key,
	emis int,
	nhsnos int,
	mrns int,
	rba_mrns int,
	identifiers int
)

INSERT INTO RobsDatabase.dbo.tmpIdGraph
select
		graph_id,
		sum(IIF(type='E',1,0)) as emis,
		sum(IIF(type='N',1,0)) as nhsnos,
		sum(IIF(type='M',1,0)) as mrns,
		sum(IIF(type='M' AND node_id like 'MRBA%',1,0)) as rba_mrns,
		count(node_id) as identifiers
from RobsDatabase.dbo.tmpIdNode 
group by graph_id;


---- select the overall counts of identifier graphs.
select count(graph_id) as patients, identifiers FROM RobsDatabase.dbo.tmpIdGraph t 
GROUP BY identifiers order by identifiers

---- breakdown by type of identifier
--select count(graph_id) as patients, t.emis,t.nhsnos,t.rba_mrns from
--RobsDatabase.dbo.tmpIdGraph t
--group by t.emis,t.nhsnos,t.rba_mrns order by emis asc,patients desc

---- select out large graphs
--select 
--	e.*
--FROM
--RobsDatabase.dbo.tmpIdEdge e inner join
--(
	select top(5)
		graph_id,
		count(node_id) as identifiers from RobsDatabase.dbo.tmpIdNode group by graph_id
		-- having count(node_id)=4
		order by identifiers desc
--) t on e.graph_id=t.graph_id
--order by graph_id, source_node_id


---- select out selection of small graphs
--select 
--	e.*
--FROM
--RobsDatabase.dbo.tmpIdEdge e inner join
--(
--	select top(5)
--		graph_id,
--		count(node_id) as identifiers from RobsDatabase.dbo.tmpIdNode group by graph_id
--		having count(node_id)=4
--) t on e.graph_id=t.graph_id
--order by graph_id, source_node_id

---- select out large graphs
--select 
--	e.*
--FROM
--RobsDatabase.dbo.tmpIdEdge e inner join
--(
--	select top(5)
--		graph_id,
--		count(node_id) as identifiers from RobsDatabase.dbo.tmpIdNode group by graph_id
--		-- having count(node_id)=4
--		order by identifiers desc
--) t on e.graph_id=t.graph_id
--order by graph_id, source_node_id

---- graph ids for report based on patient_id
--select top(100) g.*, r.report_id, r.patient_id, r.original_patient_id
--FROM 
--	RobsDatabase.dbo.tmpIdNode n, 
--	RobsDatabase.dbo.tmpIdGraph g, 
--	ordercomms_review.dbo.report r
--WHERE CONCAT('E', r.patient_id) = n.node_id
--AND n.graph_id = g.graph_id

-- graph ids 
drop table if exists RobsDatabase.dbo.tsftIdResult

Create table RobsDatabase.dbo.tsftIdResult (
	internal_id int primary key,
--	original_node_id varchar(35),
--	node_id varchar(35),
--	graph_id int,
	date smalldatetime,
	emis int,
	nhsnos int,
	mrns int,
	rba_mrns int,
	patient_id_updated int,
	same_graph int,
	tsft_test int
)

INSERT INTO RobsDatabase.dbo.tsftIdResult select
-- select top(100)
	r.report_id as internal_id,
	r.result_date as date,
--	 CONCAT('E', r.original_patient_id) as original_node_id, 
--	 CONCAT('E', r.patient_id) as node_id, 
--	 g.graph_id,  
	 g.emis,
	 g.nhsnos,
	 g.mrns,
	 g.rba_mrns,
	-- r.patient_id, r.original_patient_id, n.graph_id, n2.graph_id,
	IIF(r.patient_id <> r.original_patient_id,1,0) as patient_id_updated,
	IIF(n.graph_id = n2.graph_id,1,0) as same_graph,
	IIF(l.location_id IS NULL,0,1) as tsft_test
FROM 
	ordercomms_review.dbo.report r
	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', r.original_patient_id) = n.node_id 
	left join RobsDatabase.dbo.tmpIdNode n2 on CONCAT('E', r.patient_id) = n2.node_id 
	left join RobsDatabase.dbo.tmpIdGraph g on n.graph_id = g.graph_id
	left join RobsDatabase.dbo.tsftLocations l on l.location_id = r.location_id
WHERE
	r.result_date IS NOT NULL
	and r.result_time IS NOT NULL;

-- stats on the level of test movement within and between graph.
select patient_id_updated, same_graph, count(internal_id) from RobsDatabase.dbo.tsftIdResult
group by patient_id_updated, same_graph;
GO

DROP VIEW IF EXISTS percent_results_updated_by_date
GO

CREATE VIEW percent_results_updated_by_date as
SELECT TOP (365*6)
	date,
	100*CAST(updated as float)/results as percent_updated,
	100*CAST(tsft_updated as float)/tsft_results as tsft_percent_updated
FROM
(SELECT date, 
count(r.internal_id) as results, 
sum(r.patient_id_updated) updated,
sum(r.tsft_test) as tsft_results,
sum(IIF(r.patient_id_updated=1 and r.tsft_test=1,1,0)) as tsft_updated
from RobsDatabase.dbo.tsftIdResult r
group by date
) t
where results > 0 and tsft_results>0
order by date desc;
GO




-- CREATE PROCEDURE TO GET THE RESULTS AND REQUESTS GRAPH FOR A GRAPH ID
USE RobsDatabase;
GO

DROP TABLE IF EXISTS tmpRequestResultGraph;

CREATE TABLE [dbo].[tmpRequestResultGraph](
	[source_node_id] [varchar](13) NOT NULL,
	[target_node_id] [varchar](13) NOT NULL,
	[graph_id] [int] NULL,
	[rel_type] [varchar](7) NOT NULL
	constraint uniq_edge unique(source_node_id, target_node_id,graph_id,rel_type)
) 
ON [PRIMARY]

GO

INSERT INTO tmpRequestResultGraph
SELECT *
 FROM (
select
	CONCAT('E', r.original_patient_id) as source_node_id,  
	CONCAT('R', r.report_id) as target_node_id,
	n.graph_id as graph_id,
	'res' as rel_type
FROM 
	ordercomms_review.dbo.report r
	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', r.original_patient_id) = n.node_id 
WHERE r.amended=0
UNION
select
	CONCAT('E', r.patient_id) as source_node_id, 
	CONCAT('R', r.report_id) as target_node_id,
	n.graph_id as graph_id, 
	'new_res' as rel_type
FROM 
	ordercomms_review.dbo.report r
	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', r.patient_id) = n.node_id 
where r.original_patient_id <> r.patient_id
	and r.amended=0
UNION
select
	CONCAT('O', rq.request_id) as source_node_id,
	CONCAT('E', rq.original_patient_id) as target_node_id,  
	n.graph_id as graph_id,
	'req' as rel_type
FROM 
	ordercomms_review.dbo.request rq
	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', rq.original_patient_id) = n.node_id 
WHERE rq.amended=0
UNION
select
	CONCAT('O', rq.request_id) as source_node_id, 
	CONCAT('E', rq.patient_id) as target_node_id, 
	n.graph_id as graph_id,
	'new_req' as rel_type
FROM 
	ordercomms_review.dbo.request rq
	left join RobsDatabase.dbo.tmpIdNode n on CONCAT('E', rq.patient_id) = n.node_id 
where rq.original_patient_id <> rq.patient_id and rq.amended=0
	) t


