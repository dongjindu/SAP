* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0221 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA022146C MESSAGE-ID ZP.

* Internal table declaration for reading the source data
* Field perid added - Mrudula.
* BDC OKCODE changed - Mrudula.

DATA: BEGIN OF _P0221 OCCURS 0,
       PERID(13),
       PERNR LIKE P0221-PERNR,
       SUBTY LIKE P0221-SUBTY,
       BEGDA(10) TYPE C,
       PAYID LIKE P0221-PAYID,
       CHECN LIKE P0221-CHECN,
       WAERS LIKE P0221-WAERS,
       LGART LIKE Q0221-LGART,
       TAXAU LIKE Q0221-TAXAU,
       ANZHL(7) TYPE C,
       BETRG(9) TYPE C.
DATA: END OF _P0221.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0221 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0221.TXT' .

PARAMETER : FILE0221 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0221.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0221' SY-UNAME.
PERFORM UPLOAD_0221 USING FILE0221 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0221
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0221 USING F ERR.

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
       _P0221-PERID   _P0221-SUBTY
                    _P0221-BEGDA _P0221-PAYID _P0221-CHECN
       _P0221-WAERS _P0221-LGART _P0221-TAXAU _P0221-ANZHL
       _P0221-BETRG  T .
  MOVE _P0221-PERID TO _P0221-PERNR.
    IF _P0221-PERID NE SPACE.
    APPEND _P0221.
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
DATA: CNT5 TYPE I.
DATA: CNT6 TYPE I.
DATA: ST5 TYPE C.
DATA: LGART1(16) TYPE C.
DATA: TAXAU1(16) TYPE C.
DATA: ANZHL1(16) TYPE C.
DATA: BETRG1(16) TYPE C.


DATA: TEMPER LIKE P0221-PERNR VALUE '00000000'.
CLEAR _P0221.
CNT5 = 1.
LOOP AT _P0221.
  IF TEMPER <> _P0221-PERNR.
    IF CNT5 <> 1.
      PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
      PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
      CNT5 = 1.
    ENDIF.
    CLEAR SSN. CONCATENATE '=c..' _P0221-PERID INTO SSN.
     PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0221' ' ',
                       ' ' 'RP50G-SUBTY' _P0221-SUBTY ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

     ST5 = CNT5.
     CONCATENATE 'Q0221-LGART(' ST5 ')' INTO LGART1.
     CONCATENATE 'Q0221-TAXAU(' ST5 ')' INTO TAXAU1.
     CONCATENATE 'Q0221-ANZHL(' ST5 ')' INTO ANZHL1.
     CONCATENATE 'Q0221-BETRG(' ST5 ')' INTO BETRG1.

     PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP022100' '2000' ' ',
                        ' ' 'P0221-BEGDA' _P0221-BEGDA ' ',
                        ' ' 'P0221-PAYID' _P0221-PAYID ' ',
                        ' ' 'P0221-CHECN' _P0221-CHECN ' ',
                        ' ' 'P0221-WAERS' _P0221-WAERS ' ',
                        ' ' LGART1 _P0221-LGART ' ',
                        ' ' TAXAU1 _P0221-TAXAU ' ',
                        ' ' ANZHL1 _P0221-ANZHL ' ',
                        ' ' BETRG1 _P0221-BETRG ' '.
   ELSE.
  ST5 = CNT5.
  CNT6 = CNT5 MOD 7.
  IF CNT6 = 0.
   PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' '/23' ' ' ,
                        'X' 'mp022100' '2000' ' '.
    CNT5 = 1.
    ST5 = CNT5.
  ENDIF.
  CONCATENATE 'Q0221-LGART(' ST5 ')' INTO LGART1.
  CONCATENATE 'Q0221-TAXAU(' ST5 ')' INTO TAXAU1.
  CONCATENATE 'Q0221-ANZHL(' ST5 ')' INTO ANZHL1.
  CONCATENATE 'Q0221-BETRG(' ST5 ')' INTO BETRG1.

     PERFORM DYNPRO TABLES BDC_DATA
               USING:   ' ' LGART1  _P0221-LGART ' ',
                        ' ' TAXAU1  _P0221-TAXAU ' ',
                        ' ' ANZHL1  _P0221-ANZHL ' ',
                        ' ' BETRG1  _P0221-BETRG ' '.
  ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P0221-PERNR.
 ENDLOOP.
 PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
 PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.

ENDFORM.                    " POPULATE_BDC

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
