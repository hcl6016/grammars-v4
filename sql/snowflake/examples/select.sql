-- Table with types and functions as column names
-- Causes timeout on PHP build test?
-- select user, date, iff, current_timestamp, cluster, type, region, function from edge;
SELECT MAX(src.COL3) OVER (PARTITION BY COL1) as MAXCol3 FROM Table1 AS src;
SELECT MAX(src.COL3) OVER (PARTITION BY COL1 ORDER BY COL2 DESC, COL1 ASC) as MAXCol3 FROM Table1 AS src;
SELECT  MAX(src.COL3) OVER (PARTITION BY COL1 ORDER BY COL2) as MAXCol3 FROM Table1 AS src;
SELECT MAX(src.COL3) OVER (ORDER BY COL2) as MAXCol3 FROM Table1 AS src;
SELECT  MAX(src.COL3) OVER (ORDER BY COL2 DESC, COL1 ASC) as MAXCol3 FROM Table1 AS src;
SELECT concat('A',' ','bcd') as result;
SELECT concat('A',' ','bcd');
SELECT 'teststring';  
SELECT 'teststring' AS result;
SELECT  cast(concat(to_date(ColDate, 'yyyyMMdd'), ' ', substring(ColHour, 1, 2), ':', substr(ColHour, 3, 2), ':', substring(ColHour, 5, 2)) as datetime) as FinalDate;
with src as (SELECT 1 as COL1 UNION SELECT 2) SELECT COL1 FROM SRC;
SELECT IFNULL(DATEADD(DAY, -1, CAST(ColDate as date)), CAST('2999-12-31' as datetime)) , NVL(TIMEADD(DAY, -1, CAST(ColDate as date)), CAST('2999-12-31' as datetime));
SELECT RES FROM (SELECT LEFT('COL1',1) AS RES UNION SELECT RIGHT('COL1',1)) as TableRes;
SELECT to_date(select dateadd(d,-((date_part(dw,getdate())+1)%7+1),getdate())) AS Res;
SELECT TRY_CAST("150" as INT);
SELECT LEN(COL1), LENGTH (COL2) from t;
SELECT REPLACE('abcd', 'bc'), CHARINDEX('abcd','c') F;
