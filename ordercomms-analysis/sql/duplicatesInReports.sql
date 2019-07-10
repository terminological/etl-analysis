/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) 
 r.*,
 rh.*,
 r2.*
  FROM request r left join requestHistory rh ON r.request_id=rh.amends_request_id 
  LEFT JOIN request r2 ON rh.amended_request_id = r2.request_id WHERE r.patient_id <> r2.patient_id
  ORDER BY r.request_id DESC;


SELECT TOP (1000) 
 r.*,
 rh.*,
 r2.*
  FROM report r left join reportHistory rh ON r.report_id=rh.amends_report_id 
  LEFT JOIN request r2 ON rh.amended_report_id = r2.request_id WHERE r.patient_id <> r2.patient_id
  ORDER BY r.report_id DESC

-- all active reports for TSFT hospital numbers
SELECT count(*)
FROM report r where hospital_no like 'RBA%' and active = 1;


-- TODO: Extract this into a form that we can use to predict delay to view.
-- active reports which have been amended to have a different emis internal patient id and this is recorded
SELECT count(*)
FROM report r where patient_id <> original_patient_id and hospital_no like 'RBA%' and active=1;

-- TODO: Requests also


-- active reports which have been amended to have a different emis internal patient id without this being recorded
-- This happens frequently outside of TSFT but hardly ever so inside.
-- What is odd here is that there is no record in the report history of the changes at we saw above - about 10% of tests change EMIS identifiers without and entry in the reportHistory.
SELECT
 count(distinct r.report_id )
 -- r.*,
 -- rh.*,
 -- r2.*
  FROM report r left join reportHistory rh ON r.report_id=rh.amends_report_id 
  LEFT JOIN request r2 ON rh.amended_report_id = r2.request_id 
  WHERE 
  r.patient_id <> r2.patient_id
  -- and r.patient_id = r.original_patient_id
  and r.hospital_no like 'RBA%'
  -- and r.active = 1
--  and r2.hospital_no like 'RBA%'
--  ORDER BY r.report_id DESC