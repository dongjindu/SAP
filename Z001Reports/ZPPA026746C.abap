* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0267 - using PA30
* Authors : Srinidhi/Mrudula Patel - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA026746C MESSAGE-ID ZP.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P0267 OCCURS 0,
       PERID(13),
       PERNR LIKE P0128-PERNR,
       BEGDA1(10),
       ENDDA1(10),
       SUBTY LIKE P0267-SUBTY,
       LGART LIKE P0267-LGART,
       BETRG(9),
       WAERS LIKE P0267-WAERS,
       ANZHL(7),
       EITXT LIKE Q0267-EITXT,
       PABRP LIKE Q0267-PABRP,
       PABRJ LIKE Q0267-PABRJ,
       ZUORD LIKE P0267-ZUORD,
       PREAS LIKE P0267-PREAS,
       OCRSN LIKE P0267-OCRSN,
       PAYTY LIKE P0267-PAYTY,
       PAYID LIKE P0267-PAYID,
END OF _P0267.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0267 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0267.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0267' SY-UNAME.
PERFORM UPLOAD_0267 USING FILE0267 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0267
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0267 USING F ERR.

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
    _P0267-PERID _P0267-BEGDA1  _P0267-ENDDA1 _P0267-SUBTY _P0267-LGART
         _P0267-BETRG _P0267-WAERS _P0267-ANZHL _P0267-EITXT
         _P0267-PABRP _P0267-PABRJ _P0267-ZUORD
          _P0267-PREAS _P0267-OCRSN _P0267-PAYTY _P0267-PAYID T .

    MOVE  _P0267-PERID TO _P0267-PERNR.
    IF _P0267-PERID NE SPACE.
      APPEND _P0267.
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
  LOOP AT _P0267.
    CLEAR SSN. CONCATENATE '=c..' _P0267-PERID INTO SSN.

    PERFORM DYNPRO TABLES BDC_DATA
                   USING: 'X' 'SAPMP50A' '1000' ' ',
                          ' ' 'rp50g-pernr' SSN ' ',
                          ' ' 'RP50G-CHOIC' '0267' ' ',
                          ' ' 'RP50G-SUBTY' _P0267-SUBTY ' ',
                          ' ' 'BDC_OKCODE' '/05' ' '.

    PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP026700' '2000' ' ',
                          ' ' 'P0267-BEGDA' _P0267-BEGDA1 ' ',
                          ' ' 'P0267-LGART' _P0267-SUBTY ' ',
                          ' ' 'Q0267-BETRG' _P0267-BETRG ' ',
                          ' ' 'P0267-WAERS' _P0267-WAERS ' ',
                          ' ' 'P0267-ANZHL' _P0267-ANZHL ' ',
                          ' ' 'Q0267-EITXT' _P0267-EITXT ' ',
                          ' ' 'Q0267-PABRP' _P0267-PABRP ' ',
                          ' ' 'Q0267-PABRJ' _P0267-PABRJ ' ',
                          ' ' 'P0267-ZUORD' _P0267-ZUORD ' ',
                          ' ' 'P0267-PREAS' _P0267-PREAS ' ',
                          ' ' 'P0267-OCRSN' _P0267-OCRSN ' '.
     IF _P0267-PAYTY NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                          ' ' 'P0267-PAYTY' _P0267-PAYTY ' '.
     ENDIF.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                          ' ' 'P0267-PAYID' _P0267-PAYID ' '.

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
