USE ordercomms_review;


DROP TABLE IF EXISTS RobsDatabase.dbo.tmpIndentifierMap;
Select 
p.patient_id as emis_id,
IIF(emis1.nhs_for_emis IS NULL,0,emis1.nhs_for_emis) as nhs_for_emis,
IIF(emis2.mrn_for_emis IS NULL,0,emis2.mrn_for_emis) as mrn_for_emis,
l.hospital_no,
IIF(mrn1.hospital_no IS NULL,0,mrn1.emis_for_mrn) as emis_for_mrn,
IIF(mrn2.hospital_no IS NULL,0,mrn2.nhs_for_mrn) as nhs_for_mrn,
p.nhsno,
IIF(nhs1.nhsno IS NULL,0,nhs1.emis_for_nhs) as emis_for_nhs,
IIF(nhs2.nhsno IS NULL,0,nhs2.mrn_for_nhs) as mrn_for_nhs
INTO RobsDatabase.dbo.tmpIndentifierMap
from 
	patient p 
	left join lab_patient l on l.patient_id = p.patient_id
	left join (
		select 
			l.hospital_no,
			count(distinct l.patient_id) as emis_for_mrn
		from lab_patient l 
		group by l.hospital_no
	) mrn1 on l.hospital_no = mrn1.hospital_no
	left join (
		select
			p.nhsno,
			count(distinct p.patient_id) as emis_for_nhs
		from patient p 
		WHERE NOT(p.nhsno like ''
			OR p.nhsno like '""'
			or p.nhsno like '000%'
			or p.nhsno like '999%'
			or p.nhsno is null)
		group by p.nhsno
	) nhs1 on p.nhsno = nhs1.nhsno
	left join (
		select
			l.hospital_no,
			count(distinct p.nhsno) as nhs_for_mrn
		from lab_patient l, patient p
		where l.patient_id = p.patient_id 
		group by l.hospital_no
	) mrn2 on l.hospital_no = mrn2.hospital_no
	left join (
		select
			p.nhsno,
			count(distinct l.hospital_no) as mrn_for_nhs
		from lab_patient l, patient p
		where l.patient_id = p.patient_id 
		AND NOT(p.nhsno like ''
			OR p.nhsno like '""'
			or p.nhsno like '000%'
			or p.nhsno like '999%'
			or p.nhsno is null)
		group by p.nhsno
	) nhs2 on p.nhsno = nhs2.nhsno 
	left join (
		select
			p.patient_id,
			1 as nhs_for_emis
		FROM patient p
		WHERE NOT(p.nhsno like ''
			OR p.nhsno like '""'
			or p.nhsno like '000%'
			or p.nhsno like '999%'
			or p.nhsno is null)
	) emis1 on p.patient_id = emis1.patient_id
	left join (
		select
			l.patient_id,
			count(distinct l.hospital_no) as mrn_for_emis
		from lab_patient l
		group by l.patient_id
	) emis2 on p.patient_id = emis2.patient_id
-- WHERE nhs1.nhsno is not null and l.hospital_no like 'RBA%'
WHERE (l.hospital_no like 'RBA%' OR l.hospital_no IS NULL)