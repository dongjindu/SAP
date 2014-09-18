FUNCTION ZFMM_EXCEL_UPLOAD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(FILENAME) LIKE  RLGRAP-FILENAME
*"     REFERENCE(I_BEGIN_COL) TYPE  I
*"     REFERENCE(I_BEGIN_ROW) TYPE  I
*"     REFERENCE(I_END_COL) TYPE  I
*"     REFERENCE(I_END_ROW) TYPE  I
*"  TABLES
*"      OUTAB
*"----------------------------------------------------------------------
  DATA : LT_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.

  FIELD-SYMBOLS : <LW_FS>.
  DATA : LW_FIELD_TYPE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = FILENAME
            I_BEGIN_COL             = I_BEGIN_COL
            I_BEGIN_ROW             = I_BEGIN_ROW
            I_END_COL               = I_END_COL
            I_END_ROW               = I_END_ROW
       TABLES
            INTERN                  = LT_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK NOT LT_ITAB[] IS INITIAL.

*-- Delete Header line.

*  DELETE LT_ITAB WHERE ROW Lt I_BEGIN_ROw.

  SORT LT_ITAB BY ROW COL.

  LOOP AT LT_ITAB.
    MOVE : LT_ITAB-COL TO LW_INDEX.
    ASSIGN COMPONENT LW_INDEX OF STRUCTURE OUTAB TO <LW_FS>.

    DESCRIBE FIELD <LW_FS> TYPE LW_FIELD_TYPE.

    IF LW_FIELD_TYPE = 'D'.  "'MM/DD/YYYY"
      CONCATENATE LT_ITAB-VALUE+6(4)    "YEAR  (YYYY)
                  LT_ITAB-VALUE+0(2)    "MONTH (MM)
                  LT_ITAB-VALUE+3(2)    "DAY   (DD)
                              INTO <LW_FS>.
    ELSE.
      MOVE : LT_ITAB-VALUE TO <LW_FS>.
    ENDIF.

    AT END OF ROW.
      APPEND OUTAB.
      CLEAR OUTAB.
    ENDAT.
  ENDLOOP.

ENDFUNCTION.
