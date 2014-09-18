* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0017 - using PA30
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA001746C MESSAGE-ID ZP.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P0017 OCCURS 0,
      PERID(13),
      PERNR LIKE P0017-PERNR,
      BEGDA1(10),
      ENDDA1(10),
      ERKLA LIKE P0017-ERKLA,
      ERGRU LIKE P0017-ERGRU,
      SPEBE LIKE P0017-SPEBE,
      PTZUO LIKE P0017-PTZUO,
      PKWRG LIKE P0017-PKWRG,
      KZPMF LIKE P0017-KZPMF,
      PKWKL LIKE P0017-PKWKL,
      KFZKZ LIKE P0017-KFZKZ,
      BWAEN LIKE P0017-BWAEN,
      BUKRS LIKE P0017-BUKRS,
      GSBER LIKE P0017-GSBER,
      KOSTL LIKE P0017-KOSTL,
      FISTL LIKE P0017-FISTL,
      GEBER LIKE P0017-GEBER,
      END OF _P0017.


DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0016 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0016.TXT' .

PARAMETER : FILE0017 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0017.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0017' SY-UNAME.
PERFORM UPLOAD_0017 USING FILE0017 'Error'.
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
FORM UPLOAD_0017 USING F ERR.

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
         _P0017-PERID _P0017-BEGDA1 _P0017-ENDDA1 _P0017-ERKLA
         _P0017-ERGRU _P0017-SPEBE _P0017-PTZUO _P0017-PKWRG
         _P0017-KZPMF _P0017-PKWKL _P0017-KFZKZ _P0017-BWAEN
         _P0017-BUKRS _P0017-GSBER _P0017-KOSTL _P0017-FISTL
         _P0017-GEBER T .
    MOVE  _P0017-PERID TO _P0017-PERNR.
    IF _P0017-PERID NE SPACE.
      APPEND _P0017.
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
  LOOP AT _P0017.
    CLEAR SSN. CONCATENATE '=c..' _P0017-PERID INTO SSN.

    PERFORM DYNPRO TABLES BDC_DATA
                   USING: 'X' 'SAPMP50A' '1000' ' ',
                          ' ' 'rp50g-pernr' SSN ' ',
                          ' ' 'RP50G-CHOIC' '0017' ' ',
                          ' ' 'BDC_OKCODE' '/05' ' '.

    PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP001700' '2000' ' ',
                          ' ' 'P0017-BEGDA' _P0017-BEGDA1 ' ',
                          ' ' 'P0017-ENDDA' _P0017-ENDDA1 ' ',
                          ' ' 'p0017-erkla' _P0017-ERKLA ' ',
                          ' ' 'P0017-ERGRU' _P0017-ERGRU ' ',
                          ' ' 'P0017-SPEBE' _P0017-SPEBE ' ',
                          ' ' 'P0017-PTZUO' _P0017-PTZUO ' ',
                          ' ' 'P0017-PKWRG' _P0017-PKWRG ' ',
                          ' ' 'P0017-KZPMF' _P0017-KZPMF ' ',
                          ' ' 'P0017-PKWKL' _P0017-PKWKL ' ',
                          ' ' 'P0017-KFZKZ' _P0017-KFZKZ ' ',
                          ' ' 'P0017-BWAEN' _P0017-BWAEN ' ',
                          ' ' 'P0017-BUKRS' _P0017-BUKRS ' ',
                          ' ' 'P0017-GSBER' _P0017-GSBER ' ',
                          ' ' 'P0017-KOSTL' _P0017-KOSTL ' ',
                          ' ' 'P0017-FISTL' _P0017-FISTL ' ',
                          ' ' 'P0017-GEBER' _P0017-GEBER ' '.

*  IF _P0016-CTEDT1 NE SPACE.
*     PERFORM DYNPRO TABLES BDC_DATA USING:
*                         ' ' 'P0016-CTEDT' _P0016-CTEDT1 ' '.
*  ENDIF.
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
