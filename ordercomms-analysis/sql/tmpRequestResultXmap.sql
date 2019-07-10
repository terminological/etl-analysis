Use ordercomms_review;

-- THIS is investigations into how to map requests to results
-- actually I dont need to do this, as the analysis doesn't require it.
-- basically this set of queries show that the id_from_lims and id_from_source
-- are preserved for microbiology but not for blood tests
-- clinician ids are not preserved. test names are not preserved
-- there is a source_id in the result which is not in the request
-- and a remote_system_sample_id in the request which is not in the result

-- get all the useful identifiers for a request

SELECT TOP 100
	r.location_id,
	r.request_date+r.request_time as request_date,
	r.patient_id,
	r.discipline_id as discipline,
	r.initiating_requester_id as clinician_id, 
	r.status, -- see requestStatusType for eplainination of status
	s.sample_id,
	s.from_code as sample_code,
	'battery' as investigation_type,
	b.battery_id as investigation_id,
	b.from_code as investigation,
	b.original_display_name as investigation_name
	,rqs.id_from_lims
	,rqs.id_from_source
	,rqs.remote_system_sample_id
	,IIF(rqs.sco_date IS NULL OR rqs.sco_time IS NULL,NULL,rqs.sco_date+rqs.sco_time) as specimen_collected_date
	,IIF(rqs.sri_date IS NULL OR rqs.sri_time IS NULL,NULL,rqs.sri_date+rqs.sri_time) as specimen_recieved_date
FROM 
RobsDatabase.dbo.tsftInpatientLocations loc,
request r
	INNER JOIN rqsample rqs ON r.request_id = rqs.request_id
	LEFT OUTER JOIN rqbattery rqb on rqs.rqsample_id = rqb.rqsample_id
	LEFT OUTER JOIN sampleSynonym s ON rqs.sampleSynonym_id = s.sampleSynonym_id
	LEFT OUTER JOIN batterySynonym b ON rqb.batterySynonym_id = b.batterySynonym_id
	-- LEFT OUTER JOIN rqtest rqt on rqb.rqbattery_id = rqt.rqbattery_id -- the rqtest table is empty
where r.location_id = loc.location_id
and amended=0
and patient_id = 822206
-- and b.battery_id IS NULL --< this never happens - there is always a battery
-- and rqt.rqtest_id IS NOT NULL --< this never happens there is never a rqtest
--  and r.request_date >= '20160701'
-- and (r.status & 4) <> 0 -- has fully completed status
order by specimen_collected_date desc, id_from_lims desc;



-- get all the useful identifiers for a result
Select top 100
	r.location_id,
	r.result_date+r.result_time as result_date, -- this sometimes has nulls in it.
	r.patient_id,
	r.discipline_id as discipline,
	r.requester_id as clinician_id,
	s.sample_id,
	s.from_code as sample_code,
	'battery' as investigation_type,
	b.battery_id as investigation_id,
	b.from_code as investigation,
	b.original_display_name as investigation_name,
	r.sri_id,
	CAST(RIGHT(r.sri_id,10) as bigint) source_id,
	rs.id_from_lims,
	rs.id_from_source
	,IIF(rs.sco_date IS NULL OR rs.sco_time IS NULL,NULL,rs.sco_date+rs.sco_time) as specimen_collected_date
	,IIF(rs.sri_date IS NULL OR rs.sri_time IS NULL,NULL,rs.sri_date+rs.sri_time) as specimen_recieved_date
from
RobsDatabase.dbo.tsftInpatientLocations loc,
report r
	INNER JOIN rsample rs ON r.report_id = rs.report_id
	LEFT OUTER JOIN sampleSynonym s ON rs.sampleSynonym_id = s.sampleSynonym_id
	LEFT OUTER JOIN rbattery rb on rs.rsample_id = rb.rsample_id
	LEFT OUTER JOIN batterySynonym b ON rb.batterySynonym_id = b.batterySynonym_id
where r.location_id = loc.location_id
and amended=0
and patient_id = 822206
ORDER BY specimen_collected_date desc, id_from_lims desc;

-- This 2 sets of results basically demonstrates that it is often not possible to link a request and its result
-- They don't share useful identifiers such as investigation id - even the investigation name is changed.
-- The id_from_source and id_from_lims are sometimes the same if there is a request on the system but you can have mulitple samples
-- for a request and multiple results for a sample.
-- They are filed under different clinician ids 
-- Even sample id is inconsistent between samples
-- just about patient id and sepcimen_collected_date (sco_date, sco_time) is the only consistent thing.
-- There are many results which are requested off the system.
-- I wonder how they actually bill / reconcile these things...