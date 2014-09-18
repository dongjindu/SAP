*----------------------------------------------------------------------*
*   INCLUDE ZCPP104C_CLASSIFI_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VALUE_REQUEST_MARA
*&---------------------------------------------------------------------*
FORM VALUE_REQUEST_MARA.
* Check Material
  SELECT SINGLE * FROM MARA
         WHERE MATNR EQ P_MATNR.

  IF SY-SUBRC NE 0.
    MESSAGE E002 WITH 'There is no DATA.'
                      'Use after creation!'.
  ENDIF.

ENDFORM.                    " VALUE_REQUEST_MARA

*&---------------------------------------------------------------------*
*&      Form  VALUE_REQUEST_FILE
*&---------------------------------------------------------------------*
FORM VALUE_REQUEST_FILE.
*Choice Local PC File
  TMP_MASK = ',*.*,*.*.'.
  FIELDLN = STRLEN( P_FILE ) - 1.
  ASSIGN P_FILE+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = P_FILE
            DEF_PATH         = P_FILE
            MASK             = TMP_MASK
            MODE             = 'O'
*           TITLE            = ' '
       IMPORTING
            FILENAME         = TMP_FILENAME
*         RC               =
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
  ENDIF.

ENDFORM.                    " VALUE_REQUEST_FILE

*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
* From Excel File to Internal Table
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME    = TMP_FILENAME
            I_BEGIN_COL = '1'
            I_BEGIN_ROW = '1'
            I_END_COL   = '100'
            I_END_ROW   = '20000'
       TABLES
            INTERN      = IT_EXCEL.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA: K TYPE I,
        L_LINE TYPE I.

*Calculate total number of internal table
  DESCRIBE TABLE IT_EXCEL LINES L_LINE.

* Spare as field number
  L_LINE = L_LINE / 2.

  K = 0.
  LOOP AT IT_EXCEL.
    K = K + 1.
    IF L_LINE GE K.
      LOOP AT IT_EXCEL WHERE ROW EQ K.
        TRANSLATE IT_EXCEL-VALUE TO UPPER CASE.
        CASE IT_EXCEL-COL.
          WHEN '0001'.
            IT_AUSP-ATNAM = IT_EXCEL-VALUE.
          WHEN '0002'.
            IT_AUSP-ATWRT = IT_EXCEL-VALUE.
        ENDCASE.
      ENDLOOP.
      APPEND IT_AUSP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  AT_USER_COMMAND
*&---------------------------------------------------------------------*
FORM AT_USER_COMMAND.
  CASE SY-UCOMM.
    WHEN 'SAVE'.
      LOOP AT IT_AUSP.
        MOVE-CORRESPONDING IT_AUSP TO IT_TABLE.
        APPEND IT_TABLE.
      ENDLOOP.

* Classification Upload
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT    = P_MATNR
                MODE      = 'W'
           TABLES
                VAL_TABLE = IT_TABLE
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

      IF SY-SUBRC EQ 0.
        MESSAGE S001 WITH 'Inserted Successfully!!'.
      ELSE.
        MESSAGE E001 WITH 'Failed!!'.
      ENDIF.
  ENDCASE.
ENDFORM.                    " AT_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  SELECT SINGLE MAKTX
         FROM MAKT
         INTO MAKT-MAKTX
         WHERE MATNR EQ P_MATNR.

  WRITE:/(10) P_MATNR, (18) MAKT-MAKTX.
  ULINE.                                                    " AT 50.
  LOOP AT IT_AUSP.
    WRITE:/ IT_AUSP.
  ENDLOOP.
ENDFORM.                    " WRITE_PROCESS
