SELECT
    ST.name AS Table_Name,
    SUM(DMS.row_count) AS NUMBER_OF_ROWS
FROM
    SYS.TABLES AS ST
    INNER JOIN SYS.DM_DB_PARTITION_STATS AS DMS ON ST.object_id = DMS.object_id
WHERE
    DMS.index_id in (0,1)
GROUP BY ST.name