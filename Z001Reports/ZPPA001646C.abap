* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0016 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA001646C MESSAGE-ID ZP.

* Internal table declaration for reading the source data
DATA: BEGIN OF _P0016 OCCURS 0,
       PERID(13),
       PERNR LIKE P0016-PERNR,
       BEGDA1(10),
       ENDDA1(10),
       CTTYP LIKE P0016-CTTYP,
       WTTKL LIKE P0016-WTTKL,
       LFZFR(3) TYPE C,
       LFZZH LIKE Q0016-LFZZH,
       KGZFR(3) TYPE C,
       KGZZH LIKE Q0016-KGZZH,
       LFZSO LIKE P0016-LFZSO,
       PRBZT(3) TYPE C,
       PRBEH LIKE Q0016-PRBEH,
       KDGFR LIKE P0016-KDGFR,
       KDGF2 LIKE P0016-KDGF2,
       ARBER LIKE P0016-ARBER,
       EINDT1(10),
       KONDT1(10),
       KONSL LIKE Q0016-KONSL,
       CTEDT1(10).
DATA: END OF _P0016.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0016 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0016.TXT' .

PARAMETER : FILE0016 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0016.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0016' SY-UNAME.
PERFORM UPLOAD_0016 USING FILE0016 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0016
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0016 USING F ERR.

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
         _P0016-PERID _P0016-BEGDA1 _P0016-ENDDA1 _P0016-CTTYP
         _P0016-WTTKL _P0016-LFZFR _P0016-LFZZH _P0016-KGZFR
         _P0016-KGZZH _P0016-LFZSO _P0016-PRBZT _P0016-PRBEH
         _P0016-KDGFR _P0016-KDGF2 _P0016-ARBER _P0016-EINDT1
         _P0016-KONDT1 _P0016-KONSL _P0016-CTEDT1 T .
    MOVE  _P0016-PERID TO _P0016-PERNR.
    IF _P0016-PERID NE SPACE.
      APPEND _P0016.
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
  LOOP AT _P0016.
    CLEAR SSN. CONCATENATE '=c..' _P0016-PERID INTO SSN.

    PERFORM DYNPRO TABLES BDC_DATA
                   USING: 'X' 'SAPMP50A' '1000' ' ',
                          ' ' 'rp50g-pernr' SSN ' ',
                          ' ' 'RP50G-CHOIC' '0016' ' ',
                          ' ' 'BDC_OKCODE' '/05' ' '.

    PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP001600' '2000' ' ',
                          ' ' 'P0016-BEGDA' _P0016-BEGDA1 ' ',
                          ' ' 'P0016-ENDDA' _P0016-ENDDA1 ' ',
                          ' ' 'P0016-CTTYP' _P0016-CTTYP ' ',
                          ' ' 'P0016-WTTKL' _P0016-WTTKL ' ',
                          ' ' 'P0016-LFZFR' _P0016-LFZFR ' ',
                          ' ' 'Q0016-LFZZH' _P0016-LFZZH ' ',
                          ' ' 'P0016-KGZFR' _P0016-KGZFR ' ',
                          ' ' 'Q0016-KGZZH' _P0016-KGZZH ' ',
                          ' ' 'P0016-LFZSO' _P0016-LFZSO ' ',
                          ' ' 'P0016-PRBZT' _P0016-PRBZT ' ',
                          ' ' 'Q0016-PRBEH' _P0016-PRBEH ' ',
                          ' ' 'P0016-KDGFR' _P0016-KDGFR ' ',
                          ' ' 'P0016-KDGF2' _P0016-KDGF2 ' ',
                          ' ' 'P0016-ARBER' _P0016-ARBER ' ',
                          ' ' 'P0016-EINDT' _P0016-EINDT1 ' ',
                          ' ' 'P0016-KONDT' _P0016-KONDT1 ' ',
                          ' ' 'Q0016-KONSL' _P0016-KONSL ' '.
   IF _P0016-CTEDT1 NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                          ' ' 'P0016-CTEDT' _P0016-CTEDT1 ' '.
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
