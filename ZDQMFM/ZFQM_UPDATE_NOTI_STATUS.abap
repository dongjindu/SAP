FUNCTION ZFQM_UPDATE_NOTI_STATUS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(ZMESS) TYPE  ZMESS
*"  TABLES
*"      IT_DATA STRUCTURE  ZSQM_SCRAP_PDI
*"----------------------------------------------------------------------
  DATA: L_INDEX LIKE SY-TABIX,
        L_CN TYPE I,
        l_matnr like qmel-matnr,
        l_QMNUM like qmel-QMNUM.

  DESCRIBE TABLE IT_DATA LINES L_CN.

  IF L_CN > 0.
    ZRESULT = 'S'.
    LOOP AT IT_DATA WHERE NOT PDI_PICK IS INITIAL.
      L_INDEX = SY-TABIX.
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = IT_DATA-QMNUM
       IMPORTING
            OUTPUT = l_QMNUM.

      UPDATE QMEL SET KZDKZ = IT_DATA-PDI_PICK
         WHERE QMNUM = l_QMNUM
           AND MATNR = IT_DATA-MATNR.
      IF SY-SUBRC = 0.
        IT_DATA-ZRESULT = 'S'.
        COMMIT WORK.
      ELSE.
        IT_DATA-ZRESULT = 'E'.
        ZRESULT = 'E'.
        ROLLBACK WORK.
      ENDIF.
      MODIFY IT_DATA INDEX L_INDEX.
    ENDLOOP.
  ELSE.
    ZRESULT = 'E'.
    ZMESS = 'No data found'.
  ENDIF.
ENDFUNCTION.
