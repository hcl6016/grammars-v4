CREATE FUNCTION FUNC1 (p1 FLOAT, p2 FLOAT) RETURNS TABLE (c1 Date) LANGUAGE JAVASCRIPT IMMUTABLE AS $$ $$;
CREATE FUNCTION FUNC1 (p1 FLOAT) RETURNS BOOLEAN VOLATILE MEMOIZABLE AS $$ $$;
