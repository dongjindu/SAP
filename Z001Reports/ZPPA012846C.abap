* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0128 - using PA30
* Authors : Srinidhi/Mrudula Patel - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA012846C MESSAGE-ID ZP.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P0128 OCCURS 0,
       PERID(13),
       PERNR LIKE P0128-PERNR,
       BEGDA1(10),
       ENDDA1(10),
       SUBTY LIKE P0128-SUBTY,
       OBNAM LIKE Q0128-OBNAM,
       TDTITLE LIKE Q0128-TDTITLE,
       SPRSL LIKE P0128-SPRSL,
       ORDNO LIKE P0128-ORDNO,
       SSYST LIKE P0128-SSYST,
       PAYTY LIKE P0128-PAYTY,
       PAYID LIKE P0128-PAYID,
       TDLINE(1000).
DATA: END OF _P0128.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.
DATA: TEXT1(72), TEXT2(72), TEXT3(72), TEXT4(72), LEN(4), TEXT5(72).

*PARAMETER : FILE0016 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0016.TXT' .

PARAMETER : FILE0128 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0128.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0128' SY-UNAME.
PERFORM UPLOAD_0128 USING FILE0128 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0128
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0128 USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
*  file1(65535),
  FILE1(8192),
   END OF ITAB.



  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME                = F
           FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*   importing
*         FILELENGTH              =
       TABLES
            DATA_TAB                = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
          OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.

  DATA : T .
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
         _P0128-PERID _P0128-BEGDA1 _P0128-ENDDA1 _P0128-SUBTY
         _P0128-OBNAM
         _P0128-TDTITLE _P0128-SPRSL _P0128-ORDNO _P0128-SSYST
         _P0128-PAYTY _P0128-PAYID _P0128-TDLINE T .
    MOVE  _P0128-PERID TO _P0128-PERNR.
    IF _P0128-PERID NE SPACE.
      APPEND _P0128.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_SUBRC  text                                             *
*      -->P_ERR  text                                                  *
*----------------------------------------------------------------------*
FORM CHECK_ERROR USING ERR_CD STAGE.
  CASE ERR_CD.
    WHEN 0.
    WHEN OTHERS.
*      write:/ 'Error in the process ', stage, '. Error -', err_cd.
      STOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPULATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULATE_BDC.
  LOOP AT _P0128.
    CLEAR SSN. CONCATENATE '=c..' _P0128-PERID INTO SSN.
    LEN = STRLEN( _P0128-TDLINE ).
IF LEN  > 72.
TEXT1 = _P0128-TDLINE+0(72).
TEXT2 = _P0128-TDLINE+72(72).
TEXT3 = _P0128-TDLINE+144(72).
TEXT4 = _P0128-TDLINE+216(72).
*TEXT5 = _P0128-TDLINE+228(72).
ELSE.
TEXT1 = _P0128-TDLINE.
ENDIF.

    PERFORM DYNPRO TABLES BDC_DATA
                   USING: 'X' 'SAPMP50A' '1000' ' ',
                          ' ' 'rp50g-pernr' SSN ' ',
                          ' ' 'RP50G-CHOIC' '0128' ' ',
                          ' ' 'RP50G-SUBTY' _P0128-SUBTY ' ',
                          ' ' 'BDC_OKCODE' '/05' ' '.

    PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP012800' '2000' ' ',
                          ' ' 'P0128-BEGDA' _P0128-BEGDA1 ' ',
                          ' ' 'P0128-ENDDA' _P0128-ENDDA1 ' ',
                          ' ' 'P0128-SPRSL' _P0128-SPRSL ' ',
                          ' ' 'P0128-ORDNO' _P0128-ORDNO ' ',
                          ' ' 'p0128-SSYST' _P0128-SSYST ' ',
                          ' ' 'P0128-PAYTY' _P0128-PAYTY ' ',
                          ' ' 'P0128-PAYID' _P0128-PAYID ' '.
 IF _P0128-SUBTY = 2.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                          ' ' 'Q0128-TDTITLE' _P0128-TDTITLE ' ',
                          ' ' 'Q0128-TDLINE' TEXT1 '01',
                          ' ' 'Q0128-TDLINE' TEXT2 '02',
                          ' ' 'Q0128-TDLINE' TEXT3 '03',
                          ' ' 'Q0128-TDLINE' TEXT4 '04'.
*                          ' ' 'Q0128-TDLINE' TEXT5 '05'.
ELSE.
 PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'Q0128-OBNAM' _P0128-OBNAM ' '.
ENDIF.

      PERFORM DYNPRO TABLES BDC_DATA USING:
                          ' ' 'BDC_OKCODE' '/11' ' ' .

    PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
  ENDLOOP.

ENDFORM.                               " POPULATE_BDC

*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_DATA  text                                             *
*      -->P_0153   text                                                *
*      -->P_0154   text                                                *
*      -->P_0155   text                                                *
*      -->P_0156   text                                                *
*----------------------------------------------------------------------*
FORM DYNPRO TABLES BDC_DATA STRUCTURE BDCDATA
           USING  DYNBEGIN NAME VALUE IDX.
  IF DYNBEGIN = 'X'.
    CLEAR   BDC_DATA.
    BDC_DATA-PROGRAM = NAME.
    BDC_DATA-DYNPRO = VALUE.
    BDC_DATA-DYNBEGIN = 'X'.
    APPEND BDC_DATA.
  ELSE.
    CLEAR   BDC_DATA.
    IF IDX = ' '.
      BDC_DATA-FNAM = NAME.
    ELSE.
      CONCATENATE NAME '(' IDX ')' INTO BDC_DATA-FNAM.
    ENDIF.
    BDC_DATA-FVAL = VALUE.
    APPEND BDC_DATA.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INSERT_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_DATA  text                                             *
*      -->P_0297   text                                                *
*----------------------------------------------------------------------*
FORM INSERT_BDC TABLES BTAB USING TRCD.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = TRCD
       TABLES
            DYNPROTAB = BTAB.
  REFRESH BTAB.
  CLEAR BTAB.
  CNT1 = CNT1 + 1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0044   text                                                *
*      -->P_SY_UNAME  text                                             *
*----------------------------------------------------------------------*
FORM INIT_BDC USING SES_NAME SAP_USER.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = SES_NAME
            USER   = SAP_USER.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLOSE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM CLOSE_PROGRAM.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
*  write:/ 'No. of Transaction : ',cnt1.
*  write:/ ' BDC session created ' . " , cnt2, 'documents.' .

ENDFORM.
