/*
    CREATE OR REPLACE PROCEDURE prc_hw
      IS
    BEGIN
        DBMS_OUTPUT.PUT_LINE('Hello World!');
    END;
    /
*/

CREATE OR REPLACE PROCEDURE prc_pragma_inline
  IS
BEGIN
    PRAGMA INLINE (prc_hw, 'YES');

    DBMS_OUTPUT.PUT_LINE('');
END;
/
