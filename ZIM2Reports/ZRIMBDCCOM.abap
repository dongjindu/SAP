*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMBDCCOM                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 BDC 공통 Include                             *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.04.12                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&*
*&---------------------------------------------------------------------*
TABLES : T100.

DATA:    BEGIN OF BDCDATA OCCURS 0.
         INCLUDE STRUCTURE BDCDATA.
DATA END OF BDCDATA.

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   MESSTXT(255) TYPE C.
DATA:   L_SUBRC      LIKE SY-SUBRC.

DATA : DISP_MODE         TYPE   C,
       UMODE             TYPE   C   VALUE 'S',     " Async, Sync
       W_SUBRC           LIKE   SY-SUBRC.

*----------------------------------------------------------------------*
*   FORM: create batchinput session
*----------------------------------------------------------------------*
FORM P2000_OPEN_GROUP.
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING  CLIENT   =  SY-MANDT
                    GROUP    =  'HEI_IM'
                    USER     =  SY-UNAME.
ENDFORM.

*----------------------------------------------------------------------*
*   FORM: end batchinput session
*----------------------------------------------------------------------*
FORM P2000_CLOSE_GROUP.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.

*----------------------------------------------------------------------*
*   FORM: Start new transaction
*----------------------------------------------------------------------*
FORM P2000_BDC_TRANSACTION USING TCODE.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING TCODE     = TCODE
         TABLES    DYNPROTAB = BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*   FORM: Start new screen
*----------------------------------------------------------------------*
FORM P2000_BDC_DYNPRO USING PROGRAM DYNPRO.
    CLEAR BDCDATA.
    BDCDATA-PROGRAM  = PROGRAM.
    BDCDATA-DYNPRO   = DYNPRO.
    BDCDATA-DYNBEGIN = 'X'.
    APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*   FORM: Insert field
*----------------------------------------------------------------------*
FORM P2000_BDC_FIELD USING FNAM FVAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*    FORM: BDCDATA DYNPRO ;
*----------------------------------------------------------------------*
FORM P2000_DYNPRO USING DYNBEGIN NAME VALUE.
     IF DYNBEGIN = 'X'.
          CLEAR BDCDATA.
          MOVE : NAME  TO BDCDATA-PROGRAM,
                 VALUE TO BDCDATA-DYNPRO,
                 'X'   TO BDCDATA-DYNBEGIN.
          APPEND BDCDATA.
     ELSE.
          CLEAR BDCDATA.
          MOVE : NAME  TO BDCDATA-FNAM,
                 VALUE TO BDCDATA-FVAL.
          APPEND BDCDATA.
     ENDIF.
ENDFORM.
*
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM P2000_CALL_TRANSACTION USING     TCODE
                            CHANGING  P_SUBRC.
  DATA : L_LINE LIKE SY-TABIX.

  REFRESH : MESSTAB.
  CALL TRANSACTION TCODE   USING       BDCDATA
                           MODE        DISP_MODE
                           UPDATE      UMODE
                           MESSAGES    INTO   MESSTAB.

  P_SUBRC = SY-SUBRC.

  DESCRIBE TABLE MESSTAB LINES L_LINE.
  READ TABLE MESSTAB  INDEX L_LINE.

  CALL function 'MESSAGE_TEXT_BUILD'
       exporting
             msgid               = messtab-msgid
             msgnr               = messtab-msgnr
             msgv1               = messtab-msgv1
             msgv2               = messtab-msgv2
             msgv3               = messtab-msgv3
             msgv4               = messtab-msgv4
       importing
             message_text_output = MESSTXT.

ENDFORM.                    " P2000_CALL_TRANSACTION

FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
FORM BDC_TRANSACTION USING    TCODE
                              CHANGING  P_SUBRC.
  DATA: L_MSTRING(480).

  CALL TRANSACTION TCODE
       USING       BDCDATA
       MODE        DISP_MODE
       UPDATE      UMODE.

  P_SUBRC = SY-SUBRC.
  LOOP AT MESSTAB.
      SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
                                AND   ARBGB = MESSTAB-MSGID
                                AND   MSGNR = MESSTAB-MSGNR.
      IF SY-SUBRC = 0.
        L_MSTRING = T100-TEXT.
        IF L_MSTRING CS '&1'.
          REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
          REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
          REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
          REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
        ELSE.
          REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
        ENDIF.
        CONDENSE L_MSTRING.
        WRITE: / MESSTAB-MSGTYP, L_MSTRING(250).
      ELSE.
        WRITE: / MESSTAB.
      ENDIF.
   ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_DATE_CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_DATE_CONVERSION USING W_DATE.
*  DATA : external_date(10), internal_date TYPE d.
*  orginal_date = syst-datum.

*  CALL 'DATE_CONV_INT_TO_EXT' ID 'DATINT' FIELD orginal_date
*                              ID 'DATEXT' FIELD external_date.
*  CALL 'DATE_CONV_EXT_TO_INT' ID 'DATEXT' FIELD external_date
*                              ID 'DATINT' FIELD internal_date.

*  write / external_date.
*  write / internal_date.

DATA : DATE1 LIKE ZTBKPF-BUDAT,
       DATE2(10) TYPE C.
WRITE : DATE1 TO DATE2.

ENDFORM.                    " BDC_DATE_CONVERSION
