* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 2013 - using PA30
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA201346C MESSAGE-ID ZP.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P2013 OCCURS 0,
      PERID(13),
      PERNR LIKE P2013-PERNR,
      CRRT_DATE(10),
      ACCCO_TYP(2),
      ACCCO_NUM(10),
      OPADD,
      OPSUB,
      OPREP,
      ACCTR,
      END OF _P2013.


DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE2013 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\2013.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA2013' SY-UNAME.
PERFORM UPLOAD_2013 USING FILE2013 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0017
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_2013 USING F ERR.

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
             _P2013-PERID _P2013-CRRT_DATE _P2013-ACCCO_TYP
             _P2013-ACCCO_NUM _P2013-OPADD _P2013-OPSUB
             _P2013-OPREP _P2013-ACCTR T.
    MOVE  _P2013-PERID TO _P2013-PERNR.
    IF _P2013-PERID NE SPACE.
      APPEND _P2013.
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
  LOOP AT _P2013.
    CLEAR SSN. CONCATENATE '=c..' _P2013-PERID INTO SSN.

    PERFORM DYNPRO TABLES BDC_DATA
                   USING: 'X' 'SAPMPT50' '2000' ' ',
                          ' ' 'RP51L-PERNR' SSN ' ',
                          ' ' 'BDC_OKCODE' 'QTAC' ' '.

   PERFORM DYNPRO TABLES BDC_DATA
                   USING: 'X' 'SAPMPT50' '2000' ' ',
                          ' ' 'BDC_OKCODE' 'CORI' ' '.

   PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPMPT50' '1030' ' ',
                          ' ' 'RP51L-GDATE' _P2013-CRRT_DATE ' ',
                          ' ' 'RP51L-ACCCO_TYP' _P2013-ACCCO_TYP ' ',
                          ' ' 'RP51L-ACCCO_NUM' _P2013-ACCCO_NUM ' ',
                          ' ' 'RP51L-ACCCO_TRA' _P2013-ACCTR ' '.
   IF _P2013-OPADD NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPMPT50' '1030' ' ',
                          ' ' 'OPADD' 'X' ' '.
   ENDIF.

   IF _P2013-OPSUB NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPMPT50' '1030' ' ',
                          ' ' 'OPSUB' 'X' ' '.
   ENDIF.

   IF _P2013-OPREP NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPMPT50' '1030' ' ',
                          ' ' 'OPREP' 'X' ' '.
   ENDIF.

   PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPMPT50' '1030' ' ',
                          ' ' 'BDC_OKCODE' 'GOON' ' ' .
   PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPMPT50' '1030' ' ',
                          ' ' 'BDC_OKCODE' '/11' ' ' .
   PERFORM INSERT_BDC TABLES BDC_DATA USING 'PT50'.
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
