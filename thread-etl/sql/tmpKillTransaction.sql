Use omopBuild
DBCC OPENTRAN
Commit Transaction T1

kill 57 go exec T1 go

SELECT conn.session_id, host_name, program_name,
    nt_domain, login_name, connect_time, last_request_end_time 
FROM sys.dm_exec_sessions AS sess
JOIN sys.dm_exec_connections AS conn
   ON sess.session_id = conn.session_id;


DBCC SHRINKDATABASE (tempdb, 90)
use tempdb
DBCC SHRINKFILE (templog,120)