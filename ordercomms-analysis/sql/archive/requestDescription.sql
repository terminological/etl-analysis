-- This gets lists of batteries ordered in a single request.
-- It is the rqbattery that defines what tests will be run (not rqtest which is empty)
-- The request is for some samples to be processed. The samples describe the tests to be run on them.
-- I would tend to have modelled it the other way round and put the request for tests to be run on samples, and the data structure could have supported that
-- N.B. this only works for lab tests.
select top(100) * from rqsample rq, request r, rqbattery b, batterySynonym bs, sampleSynonym ss
WHERE r.request_id = rq.request_id
AND r.request_id = b.request_id
AND rq.rqsample_id = b.rqsample_id
AND r.active = 1
AND bs.batterySynonym_id = b.batterySynonym_id
AND rq.sampleSynonym_id = ss.sampleSynonym_id
-- AND rq.request_id in ('17789873','17789874') -- asthma patient
AND rq.request_id in ('17789848','17789847') -- copd patient
;

--Radiology
SELECT TOP (1000) *
  FROM rqnpi rq, npi b, request r 
  WHERE b.npi_id = rq.npi_id
  AND r.request_id = rq.request_id
  AND r.active = 1
  AND rq.request_id in ('17785268','17785137') -- fall and pneumonia
  order by rq.request_id DESC;

