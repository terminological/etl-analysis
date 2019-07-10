-- ASSEMBLES A VIEWED BY TABLE WHICH MATCHES REPORTS to USER VIEWS
-- This includes the methodology for including the grid views into the 
-- data by either assuming the user views the most recent N tests or 
-- all the tests released in the last M hours.

DROP TABLE IF EXISTS RobsDatabase.[dbo].[tsftResultViewedby];

-- we need to create a date index
CREATE TABLE RobsDatabase.[dbo].[tsftResultViewedby] (
	[viewed_date] [datetime] NOT NULL,
	[user_id] [int] NOT NULL,
	[duration] int NOT NULL,
	[report_id] [int] NOT NULL,
	[view_type] [tinyint] NOT NULL,
	INDEX X_user_id (user_id),
	INDEX X_report_id (report_id),
) ON [PRIMARY];

CREATE CLUSTERED INDEX X_viewed_date ON RobsDatabase.[dbo].tsftResultViewedBy (viewed_date);
GO

-- This picks out log entries feom the user audit table which correspond to direct result views
INSERT INTO RobsDatabase.[dbo].[tsftResultViewedby]
SELECT        
		rv.datetime,
		rv.user_id,
		rv.duration,
		r.report_id,
		1 as view_type
FROM            
	RobsDatabase.dbo.tsftInpatientLocations loc,
	ordercomms_review.dbo.rvUserAudit rv INNER JOIN
	ordercomms_review.dbo.report r ON rv.additional_id1 = r.report_id 
WHERE type in (100,101,102)
AND loc.location_id = r.location_id;

/* -- or this can be done via the reportViewedBy table
SELECT rv.* from 
		RobsDatabase.dbo.tsftInpatientLocations loc,
		ordercomms_review.dbo.report r,
		ordercomms_review.dbo.reportViewedby rv
Where loc.location_id = r.location_id 
and r.report_id = rv.report_id;
*/
GO

DROP TABLE IF EXISTS RobsDatabase.[dbo].[tsftResultGridView];
GO

CREATE TABLE RobsDatabase.[dbo].[tsftResultGridView] (
	[viewed_date] [datetime] NOT NULL,
	[user_id] [int] NOT NULL,
	[duration] [int] NOT NULL,
	[patient_id] binary(32) NOT NULL,
	INDEX X_viewed_date (viewed_date),
	INDEX X_user_id (user_id),
	INDEX X_patient_id (patient_id),
) ON [PRIMARY];
GO


-- This picks out log entries feom the user audit table which correspond to gridt views
INSERT INTO RobsDatabase.[dbo].[tsftResultGridView]
SELECT         
		ua.datetime,
		ua.user_id,
		ua.duration,
		CONVERT(binary(32),hashbytes('SHA2_256',tmp.salt+CONVERT(nvarchar(4000),
			ua.additional_id1
		))) as patient_id
FROM            
	(Select CONVERT(char(36),uuid) as salt from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id)) tmp,
	ordercomms_review.dbo.rvUserAudit ua INNER JOIN
    ordercomms_review.dbo.methodText mt ON mt.method_code = ua.type LEFT OUTER JOIN
    ordercomms_review.dbo.summary s ON s.summary_id = ua.additional_id2
WHERE (mt.type_id1 <> 0)
AND ua.type = 160

-- The grid view is not location specific.
-- We also need to tie it back to a subset of the patient's results
-- we need a copy of all the dates for all results which are viewable from the grid so
-- we can take a subset of those as viewed by a grid view
DROP TABLE IF EXISTS RobsDatabase.[dbo].[allResultIds];
GO

CREATE TABLE RobsDatabase.[dbo].[allResultIds] (
	[date] [datetime] NOT NULL,
	[report_id] [int] NOT NULL,
	[patient_id] binary(32) NOT NULL,
	[location_id] [int] NOT NULL,
	INDEX X_location_id (location_id),
	INDEX X_report_id (report_id),
	INDEX X_patient_id (patient_id),
) ON [PRIMARY];

CREATE CLUSTERED INDEX X_date ON RobsDatabase.[dbo].allResultIds (date);
GO

-- A copy of the full report table with the minimum information 
-- and dates cluster indexed
INSERT INTO RobsDatabase.[dbo].[allResultIds]
SELECT
	r.result_date+r.result_time as date,
	r.report_id,
	CONVERT(binary(32),hashbytes('SHA2_256',tmp.salt+CONVERT(nvarchar(4000),r.patient_id))) as patient_id,
	r.location_id
FROM 
	(Select CONVERT(char(36),uuid) as salt from RobsDatabase.dbo.tsftSalt GROUP BY id,uuid having id=max(id)) tmp,
	ordercomms_review.dbo.report r
WHERE 
	result_date IS NOT NULL and result_time IS NOT NULL;
GO

-- Add the views from the grid view into the tsftResultViewedBy table
-- @numTestsPerGridView is the number of recent tests that the grid view is assumed to relate to
-- in this case 5 of the most recently available tests from any location
-- this will then be filtered to just tsft inpatient tests.
DECLARE @numTestsPerGridView INT;
SET @numTestsPerGridView = 10;
INSERT INTO RobsDatabase.[dbo].[tsftResultViewedby]
SELECT 
	tmp2.viewed_date,
	tmp2.user_id,
	tmp2.duration,
	tmp2.report_id,
	2 as view_type
FROM (
	SELECT
		gv.*,
		tmp.report_id,
		tmp.location_id
	FROM 
		RobsDatabase.[dbo].tsftResultGridView gv
		CROSS APPLY (
			SELECT -- TOP(@numTestsPerGridView)
				ar.*
			FROM
				RobsDatabase.[dbo].[allResultIds] ar
			WHERE 
				gv.patient_id = ar.patient_id
				AND ar.date < gv.viewed_date
				-- if we want to put a time constrain rather than a number of test constraint it needs to go here.
				AND ar.date > gv.viewed_date - 1
			-- ORDER BY ar.date desc
		) tmp
	WHERE 
		gv.patient_id = tmp.patient_id
) tmp2,
RobsDatabase.dbo.tsftInpatientLocations loc
WHERE 
	tmp2.location_id = loc.location_id;
GO

-- Assemble the base data for TTV
-- need to look at earliest time a test is viewed.
-- number of distinct clinicians viewing a test

Use RobsDatabase;
GO

DROP VIEW IF EXISTS tsftUniqueResultViews;
DROP VIEW IF EXISTS tsftFirstResultView;
GO

-- The subquery assigns a row number for each report_id & user_id group based on viewed_date
-- the first of which is selected by the outer query
-- this results in the first view of a result for a given user
-- this is then further grouped by report_id to get a count of users viewing each report.
CREATE VIEW tsftUniqueResultViews AS
	SELECT
		ROW_NUMBER() OVER (
			PARTITION BY tmp.report_id ORDER BY tmp.viewed_date asc) as viewed_by_distinct_user_number
		, tmp.*
	from 
		(
			SELECT 
				*,
				ROW_NUMBER() OVER (
					PARTITION BY report_id,user_id ORDER BY viewed_date asc
				) as user_view_ordinality
			FROM
			tsftResultViewedBy
		) tmp
	WHERE tmp.user_view_ordinality = 1;
GO

CREATE VIEW tsftFirstResultView AS
SELECT a.report_id, a.viewed_date as first_viewed_date, a.view_type, tmp.total_views, a.user_id as first_user_id
		FROM
			tsftUniqueResultViews a,
			( SELECT
				max(viewed_by_distinct_user_number) as total_views,
				report_id
			FROM
				tsftUniqueResultViews 
			GROUP BY report_id ) tmp
		WHERE a.report_id = tmp.report_id
		AND a.viewed_by_distinct_user_number = 1
GO
	