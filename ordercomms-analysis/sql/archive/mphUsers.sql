-- Some early analysis on the question of users and clinicians.
-- This mapping was ultimately done in the java code

USE ordercomms_review;

-- but which clinician_ids are actually used by location (requests)
SELECT DISTINCT c.* --, loc.* 
FROM request r
left outer join clinician c on c.clinician_id = r.initiating_requester_id 
inner join RobsDatabase.dbo.tsftLocations loc on r.location_id = loc.location_id 
-- WHERE 
--	r.request_date >= '20160701' and r.request_date < '20170701'
order by c.code;

  -- but which clinician_ids are actually used by location (results)
SELECT DISTINCT c.* --, loc.* 
FROM report r
left outer join clinician c on (c.clinician_id = r.responsible_clinician_id OR c.clinician_id = r.requester_id )
inner join RobsDatabase.dbo.tsftLocations loc on r.location_id = loc.location_id 
-- WHERE 
--	r.request_date >= '20160701'
order by c.code;

-- which users review MPH tests?
SELECT DISTINCT u.*
FROM report r
left outer join reportViewedby rv on r.report_id = rv.report_id
left outer join rvUser u on rv.user_id = u.user_id
inner join RobsDatabase.dbo.tsftLocations loc on r.location_id = loc.location_id 
WHERE 
	r.result_date >= '20160701' and r.active = 1
;

-- can we identify from results who is reviewing whose tests? :
SELECT 
	min(qc.clinician_id) as clinician_id, 
	min(qc.name) as clinician_name,
	qc.code,
	-- loc.code as location,
	min(rvu.user_id) as reviewer_id,
	min(rvu.long_name) as user_name,
	rvu.login,
	min(rvu.email_address) as email,
	COUNT(DISTINCT r.report_id) as reports
FROM 
report r
left outer join reportViewedby rv on r.report_id = rv.report_id
inner join RobsDatabase.dbo.tsftInpatientLocations l on l.location_id = r.location_id
left outer join clinician qc on r.requester_id = qc.clinician_id
left outer join rvUser rvu on rv.user_id = rvu.user_id
WHERE 
-- r.result_date >= '20160701' and r.result_date < '20170701' and 
r.active = 1
and rvu.login IS NOT NULL
GROUP BY qc.code, rvu.login /*, loc.code*/
order by clinician_id desc


/*revisiting whether we can map names to clinicians*/
DROP TABLE IF EXISTS [RobsDatabase].[dbo].[tsftNameMap];

CREATE TABLE  [RobsDatabase].[dbo].[tsftNameMap] (
	[user_id] int NOT NULL,
	[long_name] [nvarchar](100) NOT NULL,
	[clinician_id] int NOT NULL,
	[name] [nvarchar](100) NOT NULL,
	[similarity] [float] NOT NULL,
	INDEX X_tsftNameMap_user_id (user_id),
	INDEX X_tsftNameMap_clinician_id (clinician_id),
	INDEX X_tsftNameMap UNIQUE (user_id, clinician_id),
) ON [PRIMARY];

INSERT INTO [RobsDatabase].[dbo].[tsftNameMap]
SELECT
	tmp.user_id,
	max(tmp.long_name) as long_name,
	tmp.clinician_id,
	max(tmp.name) as name,
	max(tmp.similarity) as similarity
FROM (
	SELECT DISTINCT u.user_id, u.long_name, c.clinician_id, c.name, xmap.similarity as similarity from 
		ordercomms_review.dbo.rvUser u 
		INNER JOIN [RobsDatabase].[dbo].[clustering3] xmap ON u.long_name = xmap.source_name
		INNER JOIN ordercomms_review.dbo.clinician c ON c.name = xmap.target_name
	UNION
	SELECT DISTINCT u.user_id, u.long_name, c.clinician_id, c.name, 0.95 as similarity from 
		ordercomms_review.dbo.rvUser u 
		INNER JOIN ordercomms_review.dbo.clinician c ON u.login = c.code
		where u.active = 1
	UNION
	SELECT DISTINCT u.user_id, u.long_name, c.clinician_id, c.name, 1 as similarity from 
		ordercomms_review.dbo.rvUser u 
		INNER JOIN ordercomms_review.dbo.clinician c ON u.long_name = c.name
		where u.active = 1
	) as tmp		
GROUP BY tmp.user_id, tmp.clinician_id;

select rv.* from [ordercomms_review].dbo.rvUser rv

;

-- Which users review tests?
SELECT DISTINCT u.*
FROM ordercomms_review.dbo.report r
left outer join ordercomms_review.dbo.reportViewedby rv on r.report_id = rv.report_id
left outer join ordercomms_review.dbo.rvUser u on rv.user_id = u.user_id
left join RobsDatabase.[dbo].[tsftNameMap] t on u.user_id = t.user_id
inner join RobsDatabase.dbo.tsftInpatientLocations loc on r.location_id = loc.location_id 
WHERE 
	r.result_date >= '20160701' and r.active = 1
	and t.user_id is null
;
-- there are 1005 users who are not mapped to clinicians
-- this compares to 4628 overall - i.e. 3389 are mapped.
-- thsi is a mix of people.
-- lots of GP and allied health professionals.

-- but which clinician_ids order requests
SELECT DISTINCT c.* -- 
FROM request r
left outer join clinician c on c.clinician_id = r.initiating_requester_id 
left join RobsDatabase.[dbo].[tsftNameMap] t on c.clinician_id = t.clinician_id
inner join RobsDatabase.dbo.tsftInpatientLocations loc on r.location_id = loc.location_id 
  WHERE 
  r.request_date >= '20160701'
  and t.clinician_id IS NULL
order by c.code;
-- there are only 33 which have not been mapped.