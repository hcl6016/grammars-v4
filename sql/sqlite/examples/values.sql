SELECT (VALUES(1),(2),(3),(4));
VALUES(1) UNION VALUES(2);
VALUES(1),(2),(3) EXCEPT VALUES(2)
VALUES(1),(2),(3) EXCEPT VALUES(1),(3);
VALUES(1),(2),(3),(4) UNION ALL SELECT 5 LIMIT 99;
VALUES(1),(2),(3),(4) UNION ALL SELECT 5 LIMIT 3;
VALUES('0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ');
VALUES(1,8),(2,11),(3,1),(2,15),(1,4),(1,99)
VALUES('g.txt', 1000000002, 5, '12345', 0)
VALUES(9223372036854775807)
