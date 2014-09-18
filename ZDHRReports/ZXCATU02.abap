*----------------------------------------------------------------------*
*   INCLUDE ZXCATU02                                                   *
*----------------------------------------------------------------------*
  TABLES: pa0001.
*
  IF enrich_table-skostl = space.
    CLEAR pa0001.
    LOOP AT enrich_table.
      SELECT SINGLE kostl INTO pa0001-kostl
        FROM pa0001 WHERE pernr = enrich_table-pernr
                      AND begda <= enrich_table-workdate
                      AND endda >= enrich_table-workdate.

      enrich_table-skostl = pa0001-kostl.

      MODIFY enrich_table.
    ENDLOOP.
  ENDIF.
