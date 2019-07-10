-- requests have both a requestor and an initiating_requester
SELECT TOP (100) *
  FROM [ordercomms_review].[dbo].[request] r
  left outer join clinicianSynonym ocs on r.requesterSynonym_id = ocs.clinicianSynonym_id
  left outer join clinician oc on r.requester_id = oc.clinician_id AND ocs.clinician_id = oc.clinician_id
  left outer join clinicianSynonym qcs on r.initiating_requesterSynonym_id = qcs.clinicianSynonym_id
  left outer join clinician qc on r.initiating_requester_id = qc.clinician_id AND qcs.clinician_id = qc.clinician_id 
  ORDER BY request_id DESC


-- reports have a sample
  SELECT TOP (100) *
  FROM [ordercomms_review].[dbo].[report] rep,
  rsample rs
  WHERE 
  rs.report_id = rep.report_id -- AND 
-- rep.result_date >= '20160701' and rep.result_date < '20170701' and rep.active = 1
and rs.id_from_lims = 'M00483015'

-- there is no request with this lims id
  SELECT TOP (100) *
  FROM [ordercomms_review].[dbo].[request] req,
  rqsample rqs
  WHERE 
  req.request_id = rqs.request_id -- AND
-- req.request_date >= '20160701' and req.request_date < '20170701' and req.active = 1
and rqs.id_from_lims = 'M00483015'

-- try and join request and result samples on id and battery type.
-- this is unindexed
SELECT TOP(100) *
FROM 
	(
		request rq 
		inner join rqsample rqs on rq.request_id = rqs.request_id
		inner join rqbattery rqb on rqb.rqsample_id = rqs.rqsample_id
		inner join batterySynonym qb on qb.batterySynonym_id = rqb.batterySynonym_id
	) inner join (
		report r
		inner join rsample rs on r.report_id = rs.report_id
		inner join rbattery rb on rb.rsample_id = rs.rsample_id
		inner join batterySynonym b on b.batterySynonym_id = rb.batterySynonym_id
	)
	on (rqs.id_from_lims = rs.id_from_lims OR rqs.id_from_source = rs.id_from_source) AND (rb.battery_id = rqb.battery_id OR b.from_code = qb.from_code)
	WHERE r.result_date >= '20160701' and r.result_date < '20170701' AND r.active = 1 -- AND loc.inpatient = 1
