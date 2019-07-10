


-- SELECT TOP 100 * from tsftUniqueResultViews where view_type = 2;

USE [RobsDatabase]
GO

/****** Object:  Table [dbo].[aggTimeToView]    Script Date: 05/01/2018 12:03:45 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

DROP TABLE IF EXISTS [dbo].[aggTimeToView]

CREATE TABLE [dbo].[aggTimeToView](
	[internal_id] [int] NOT NULL,
	[date] [smalldatetime] NOT NULL,
	[discipline_name] [varchar](70) NULL,
	[investigation] [varchar](70) NULL,
	[investigation_name] [varchar](70) NULL,
	[investigation_abnormal] [tinyint] NULL,
	[ward_name] [nvarchar](50) NULL,
	[dependency_level] [nvarchar](50) NULL,
	[patient_group] [nvarchar](50) NULL,
	[specimen_date] [smalldatetime] NULL,
	[first_viewed_date] [smalldatetime] NULL,
	[view_type] [tinyint] NULL,
	[minutes_to_view] [int] NULL,
	[minutes_processing] [int] NULL,
	[total_views] [bigint] NULL,
	[first_user_id] [int] NULL,
	[patient_age] [int] NULL,
	[patient_gender] [varchar](11) NULL,
	[emis] [int] NULL,
	[nhsnos] [int] NULL,
	[mrns] [int] NULL,
	[rba_mrns] [int] NULL,
	[patient_id_updated] [int] NULL,
	[same_graph] [int] NULL,
	[tsft_test] [int] NULL,
	INDEX X_internal_id (internal_id),
	INDEX X_first_viewed_date (first_viewed_date),
	INDEX X_discipline_name (discipline_name),
	INDEX X_investigation (investigation),
	INDEX X_ward_name (ward_name),
	INDEX X_dependency_level (dependency_level)
) ON [PRIMARY];

USE [RobsDatabase]
GO





CREATE CLUSTERED INDEX X_date on dbo.aggTimeToView (date);
GO

DROP VIEW IF EXISTS tsftUniquePatientIndex;
GO

CREATE VIEW tsftUniquePatientIndex AS
select distinct patient_id, year_of_birth, sex from tsftPatientIndex;
GO

INSERT INTO [dbo].[aggTimeToView]
SELECT
	t.internal_id,
	t.date,
	t.discipline_name,
	t.investigation,
	t.investigation_name,
	t.investigation_abnormal,
	loc.ward_name,
	loc.dependency_level,
	loc.patient_group,
	rep.earliest_sri_date as specimen_date,
	Y.first_viewed_date,
	Y.view_type,
	DATEDIFF(mi,t.date,Y.first_viewed_date) as minutes_to_view, 
	DATEDIFF(mi,rep.earliest_sri_date,t.date) as minutes_processing,
	IIF(Y.total_views IS NULL, 0, Y.total_views) as total_views, 
	Y.first_user_id,
	YEAR(t.date)-p.year_of_birth as patient_age,
	p.sex as patient_gender,
	z.emis,
	z.nhsnos,
	z.mrns,
	z.rba_mrns,
	z.patient_id_updated,
	z.same_graph,
	z.tsft_test
from
	tsftRequestedTest t 
	INNER JOIN tsftInpatientLocations loc ON  loc.location_id = t.location_id
	INNER JOIN tsftUniquePatientIndex p ON t.patient_id = p.patient_id
	LEFT OUTER JOIN tsftFirstResultView Y on t.internal_id = Y.report_id
	LEFT OUTER JOIN tsftIdResult z on t.internal_id=z.internal_id
	LEFT OUTER JOIN ordercomms_review.dbo.report rep on t.internal_id=rep.report_id
WHERE t.date <= '20170930'
and t.date >= '20121001'
order by date desc
GO




