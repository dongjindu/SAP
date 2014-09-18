*----------------------------------------------------------------------*
***INCLUDE ZRIM_DATA_MIG_F01 .*
*----------------------------------------------------------------------*
*&  프로그램명 : DATA MIGRATION 공통 Include                           *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2003.01.04                                            *
*&  적용회사PJT: 한수원                                                *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TABLES : T100.

DATA:    BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA END OF BDCDATA.

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   MESSTXT(255) TYPE C.

DATA : DISP_MODE         TYPE   C,
       UMODE             TYPE   C   VALUE 'S',     " Async, Sync
       W_SUBRC           LIKE   SY-SUBRC.

*----------------------------------------------------------------------*
*   FORM: create batchinput session
*----------------------------------------------------------------------*
FORM P2000_OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      CLIENT = SY-MANDT
      GROUP  = 'HEI_IM'
      USER   = SY-UNAME.
ENDFORM.                    "P2000_OPEN_GROUP

*----------------------------------------------------------------------*
*   FORM: end batchinput session
*----------------------------------------------------------------------*
FORM P2000_CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.                    "P2000_CLOSE_GROUP

*----------------------------------------------------------------------*
*   FORM: Start new transaction
*----------------------------------------------------------------------*
FORM P2000_BDC_TRANSACTION USING TCODE.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      TCODE     = TCODE
    TABLES
      DYNPROTAB = BDCDATA.
ENDFORM.                    "P2000_BDC_TRANSACTION

*----------------------------------------------------------------------*
*   FORM: Start new screen
*----------------------------------------------------------------------*
FORM P2000_BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "P2000_BDC_DYNPRO

*----------------------------------------------------------------------*
*   FORM: Insert field
*----------------------------------------------------------------------*
FORM P2000_BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "P2000_BDC_FIELD

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
ENDFORM.                    "P2000_DYNPRO
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

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = MESSTAB-MSGID
      MSGNR               = MESSTAB-MSGNR
      MSGV1               = MESSTAB-MSGV1
      MSGV2               = MESSTAB-MSGV2
      MSGV3               = MESSTAB-MSGV3
      MSGV4               = MESSTAB-MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = MESSTXT.

ENDFORM.                    " P2000_CALL_TRANSACTION

*&--------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*---------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
FORM BDC_TRANSACTION USING    TCODE
                              F_MODE
                    CHANGING  P_SUBRC.
  DATA: L_MSTRING(480).
  REFRESH : MESSTAB.  CLEAR : MESSTAB.
  CALL TRANSACTION TCODE
       USING       BDCDATA
       MODE        F_MODE
       UPDATE      UMODE
       MESSAGES    INTO   MESSTAB.

  P_SUBRC = SY-SUBRC.
  LOOP AT MESSTAB.
    IF MESSTAB-MSGTYP EQ 'E'
       OR MESSTAB-MSGTYP EQ 'A'.

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
    ENDIF.
  ENDLOOP.
ENDFORM.                    "BDC_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM READ_DATA_FILE  TABLES P_TAB
                     USING  W_FILENAME.

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME     = W_FILENAME
      FILETYPE     = 'DAT'
      DAT_D_FORMAT = 'YYYYMMDD'
    TABLES
      DATA_TAB     = P_TAB.

  IF SY-SUBRC NE 0.
    MESSAGE E463(ZIM1) WITH W_FILENAME.
  ENDIF.

ENDFORM.                    " READ_DATA_FILE
