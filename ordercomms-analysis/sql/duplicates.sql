use ordercomms_review;


-- PATIENTS AS IDENTIFIED BY MRN.
-- what proportion have something different than 1 associated emis internal identifier?
-- Somerset wide
select 
	t.count_emis_ids,
	count(distinct hospital_no) as instances
FROM
(
	select 
		l.hospital_no,
		count(distinct l.patient_id) as count_emis_ids
	from lab_patient l 
	WHERE hospital_no like 'RBA%'
	group by hospital_no
) t 
group by count_emis_ids

-- DETAIL OF PATIENTS AS IDENTIFIED BY MRN WITH >1 EMIS IDS.
-- what proportion have something different than 1 associated NHS number?
select 
	p2.patient_id
	,t.hospital_no
	,[first_name]
    ,[middle_name]
    ,[family_name]
    ,[suffix]
    ,[address1]
    ,[address2]
    ,[address3]
    ,[address4]
    ,[address5]
    ,[post_code]
    ,[sex]
    ,[dob]
    ,[nhsno]
    ,[category]
    ,[phone_number]
FROM
	(
	select 
		l.hospital_no,
		count(distinct l.patient_id) as count_emis_ids
	from lab_patient l group by hospital_no
	) t  , 
	patient p2, 
	lab_patient l2
where 
	l2.hospital_no=t.hospital_no
	and p2.patient_id = l2.patient_id
	and t.count_emis_ids > 25
	and t.hospital_no like 'RBA%'
order by hospital_no asc

-- PATIENTS AS IDENTIFIED BY MRN.
-- what proportion have something different than 1 associated NHS number?
-- Somerset wide
select 
	t.count_nhs_numbers,
	count(distinct hospital_no) as instances
FROM
(
	select 
		l.hospital_no,
		count(distinct p.nhsno) as count_nhs_numbers
	from lab_patient l, patient p 
	where l.patient_id=p.patient_id 
	group by hospital_no
) t 
group by count_nhs_numbers

-- DETAIL OF PATIENTS AS IDENTIFIED BY MRN WITH >1 NHS NUMBER IN TSFT.
-- what proportion have something different than 1 associated NHS number?
select 
	t.hospital_no
	,[first_name]
    ,[middle_name]
    ,[family_name]
    ,[suffix]
    ,[address1]
    ,[address2]
    ,[address3]
    ,[address4]
    ,[address5]
    ,[post_code]
    ,[sex]
    ,[dob]
    ,[nhsno]
    ,[category]
    ,[phone_number]
FROM
	(select 
		l.hospital_no,
		count(distinct p.nhsno) as count_nhs_numbers
		from lab_patient l, patient p where l.patient_id=p.patient_id and nhsno <> '' group by hospital_no
		) t , 
		patient p2, 
		lab_patient l2
where 
	l2.hospital_no=t.hospital_no
	and p2.patient_id = l2.patient_id
	and t.count_nhs_numbers > 1 
	-- and t.hospital_no like 'RBA%'
order by hospital_no asc


-- HOW MANY DIFFERENT DATES OF BIRTH DOES A GIVEN MRN HAVE?
select 
	t.count_dates_of_birth,
	count(distinct hospital_no) as instances
FROM
(
	select 
		l.hospital_no,
		count(distinct p.dob) as count_dates_of_birth
		from lab_patient l, patient p 
		where l.patient_id=p.patient_id 
		group by hospital_no
) t 
group by count_dates_of_birth

-- DETAIL OF PATIENTS AS IDENTIFIED BY MRN WITH >1 NHS NUMBER.
-- what proportion have something different than 1 associated NHS number?
select 
	t.hospital_no
	,[first_name]
    ,[middle_name]
    ,[family_name]
    ,[suffix]
    ,[address1]
    ,[address2]
    ,[address3]
    ,[address4]
    ,[address5]
    ,[post_code]
    ,[sex]
    ,[dob]
    ,[nhsno]
    ,[category]
    ,[phone_number]
FROM
	(select 
		l.hospital_no,
		count(distinct p.dob) as count_dates_of_birth
		from lab_patient l, patient p 
		where l.patient_id=p.patient_id and nhsno <> '' 
		group by hospital_no
		) t , 
		patient p2, 
		lab_patient l2
where 
	l2.hospital_no=t.hospital_no
	and p2.patient_id = l2.patient_id
	and t.count_dates_of_birth > 1 
	and t.hospital_no like 'RBA%'
order by hospital_no asc

-- HOW MANY DIFFERENT TSFT MRNS DOES A GIVEN NHS NUMBER HAVE?
-- often many different MRNs for different hospitals, but here we are just looking at RBA MRNs
select 
	t.count_of_mrns,
	count(distinct t.nhsno) as instances
FROM
(
	select 
		count(distinct l.hospital_no) as count_of_mrns,
		p.nhsno
	from lab_patient l, patient p 
	where l.patient_id=p.patient_id 
	and l.hospital_no like 'RBA%'
	and p.nhsno <> ''
	group by p.nhsno
) t 
group by count_of_mrns 
order by count_of_mrns asc

-- DETAIL OF PATIENTS AS IDENTIFIED BY NHS NUMBER WITH >1 TSFT MRN.
select 
	t.[nhsno],
	l2.hospital_no
	,[first_name]
    ,[middle_name]
    ,[family_name]
    ,[suffix]
    ,[address1]
    ,[address2]
    ,[address3]
    ,[address4]
    ,[address5]
    ,[post_code]
    ,[sex]
    ,[dob]
    ,[category]
    ,[phone_number]
FROM
	(
		select 
			count(distinct l.hospital_no) as count_of_mrns,
			p.nhsno
		from lab_patient l, patient p 
		where l.patient_id=p.patient_id 
		and l.hospital_no like 'RBA%'
		and p.nhsno <> ''
		group by p.nhsno
	) t , 
	patient p2, 
	lab_patient l2
where 
	p2.nhsno = t.nhsno
	and p2.patient_id = l2.patient_id
	and t.count_of_mrns > 4 
	and l2.hospital_no like 'RBA%'
order by nhsno asc

-- HOW MANY PATIENTS IDENTIFIED BY NHS NO HAVE SEVERAL DATES OF BIRTH
select 
	t.count_of_dobs,
	count(distinct t.nhsno) as instances
FROM
(
	select	
		count(distinct p.dob) as count_of_dobs,
		p.nhsno
	from lab_patient l, patient p where l.patient_id=p.patient_id 
		and p.dob<>'' and p.nhsno <> ''
	group by p.nhsno
) t 
group by count_of_dobs 
order by count_of_dobs asc

-- DETAIL OF PATIENTS AS IDENTIFIED BY NHS NUMBER WITH >1 DATE OF BIRTH .
-- somerset wide
select 
	t.[nhsno],
	[dob],
	l2.hospital_no
	,[first_name]
    ,[middle_name]
    ,[family_name]
    ,[suffix]
    ,[address1]
    ,[address2]
    ,[address3]
    ,[address4]
    ,[address5]
    ,[post_code]
    ,[sex]
    ,[category]
    ,[phone_number]
FROM
	(
		select 
			count(distinct p.dob) as count_of_dobs,
			p.nhsno
		from lab_patient l, patient p 
		where l.patient_id=p.patient_id 
		and p.dob <> '' and nhsno <> ''
		group by p.nhsno
	) t , 
	patient p2, 
	lab_patient l2
where 
	p2.nhsno = t.nhsno
	and p2.patient_id = l2.patient_id
	and t.count_of_dobs > 1
	-- and l2.hospital_no like 'RBA%'
order by nhsno asc

-- Other data quality of interest 

SELECT l.hospital_no, * FROM lab_patient l, patient p where l.patient_id=p.patient_id and post_code in ('ZZ99 4GZ','XX1 1XX') and 
	l.hospital_no like 'RBA%' order by post_code

SELECT l.hospital_no, * FROM lab_patient l, patient p where l.patient_id=p.patient_id and
	(nhsno like ''
	OR nhsno like '""'
	or nhsno like '000%'
	or nhsno like '999%'
	or nhsno is null) and 
	l.hospital_no like 'RBA%'
	 order by nhsno