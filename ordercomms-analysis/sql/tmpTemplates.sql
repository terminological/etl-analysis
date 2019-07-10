-- TEMPLATES DESCRIBE WHAT DATA IS SEEN IN GRID VIEW
-- The template type can be seen from the rvUserAudit table
-- it is always "All test summary without detail" which is all the tests

SELECT ts.*,s.summary_name,s.all_items, s.max_items, si.item1_code
FROM 
	[ordercomms_review].[dbo].[template_summary] ts 
	LEFT OUTER JOIN
    [ordercomms_review].[dbo].[summary] s 
	ON ts.summary_id=s.summary_id
	LEFT OUTER JOIN
	[ordercomms_review].[dbo].[summaryItem] si
	ON s.summary_id=si.summary_id
order by ts.template_id,ts.ordinality,si.ordinality
;