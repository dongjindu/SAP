REPORT ZZCHKONH.

TABLES: KONH.
TABLES: T681.

DATA: BEGIN OF INT_KNUMH OCCURS 0,
      KNUMH LIKE KONH-KNUMH,
      END OF INT_KNUMH.

********************************************************
* This report deletes A-RECORDS WITHOUT KONH and KONP. *
********************************************************

**********************************************************
* A specific A-table is scanned and a related KONH entry *
* is searched; in case there is no entry, the A-rec will *
* be deleted. In Testmode, only the corresponding KNUMH  *
* will be displayed.                                     *
**********************************************************

DATA: TABLE(4) TYPE C.
DATA: KNUM LIKE KONH-KNUMH.
DATA: K_VEWE LIKE T681-KVEWE VALUE 'A'.
DATA: T681_STR  LIKE T681.
DATA: LV_NUM TYPE I.

* GET PARAMETERS
  PARAMETERS: TABNO LIKE T681-KOTABNR.

  PARAMETERS: TESTMODE DEFAULT 'X' AS CHECKBOX.

  REFRESH INT_KNUMH.
  select single * from T681 into T681_STR
               where kvewe = K_VEWE AND
                     KOTABNR = TABNO.
  IF SY-SUBRC NE 0.
    WRITE: / 'No entry in T681 for number ', TABNO.
    WRITE: / 'Check whether corresponding condition table exists.'.
    EXIT.
  ENDIF.
  TABLE = T681_STR-KOTAB.

* scan A-table for KNUMH
  SELECT KNUMH FROM (TABLE) INTO KNUM.

* check whether there exists a corresponding KONH entry
    SELECT SINGLE * FROM KONH WHERE KNUMH = KNUM.

    IF SY-SUBRC NE 0.
      INT_KNUMH-KNUMH = KNUM.
      COLLECT INT_KNUMH.
    ENDIF.
  ENDSELECT.

  DESCRIBE TABLE INT_KNUMH LINES LV_NUM.
  IF LV_NUM EQ 0.
    WRITE: / 'No inconsistent entries found.'.
    WRITE: / 'Each record in the condition table has a corresponding.'.
    WRITE: / 'entry in the KONH table.'.
    EXIT.
  ENDIF.

  LOOP AT INT_KNUMH.
    IF TESTMODE IS INITIAL.
* remove all A-REC entries for this KNUMH
      DELETE FROM (TABLE) WHERE
                      KNUMH = INT_KNUMH-KNUMH.
      IF SY-SUBRC = 0.
 WRITE: / 'KNUMH =', INT_KNUMH-KNUMH(10), ' deleted from table ' ,TABLE.
      ELSE.
        WRITE: / 'DELETE: SY-SUBRC is', SY-SUBRC , ' FOR KNUMH = ' .
        WRITE: INT_KNUMH-KNUMH(10).
      ENDIF.
    ELSE.
      WRITE: / 'TESTRUN: KNUMH =', INT_KNUMH-KNUMH(10).
    ENDIF.
  ENDLOOP.
