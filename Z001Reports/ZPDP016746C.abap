* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Dependent/Beneficiary Load
* Version 1.0  - August 2000

* PA Infotype 0167
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPDP016746C MESSAGE-ID ZP.

* Internal table declaration for reading the source data

data: begin of _pa0167 occurs 0.
       include structure pa0167.
DATA : PERID(13),ENRTX(20),CSTDT(8),EECST(13), BEGDA1(10),ENDDA1(10),
      ZEITX(8),ELDTO1(10),PARDT1(10), EEPTX(20), DTY(4), DID(2).
data: end of _pa0167.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0167 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\D167.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRDP0167' SY-UNAME.
PERFORM UPLOAD_0167 USING FILE0167 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0167
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0167 USING F ERR.

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

 DATA T.
  LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
 _PA0167-PERID _PA0167-BEGDA1 _PA0167-ENDDA1  _PA0167-PLTYP
 _PA0167-BPLAN _PA0167-BOPTI _PA0167-DEPCV
 _PA0167-DTY _PA0167-DID T.
    MOVE _PA0167-PERID TO _PA0167-PERNR.

IF _PA0167-PERID NE SPACE.
    APPEND _PA0167.
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
DATA: CNT5(2) TYPE N.
DATA: TEMPER LIKE PA0167-PERNR VALUE '00000000'.
DATA: TEMSUBTY LIKE _PA0167-PLTYP VALUE '0000'.
DATA: SUBTY1 LIKE _PA0167-PLTYP.
CLEAR _PA0167.
CNT5 = 1.
SORT _PA0167 BY PERID PLTYP DTY DID.
LOOP AT _PA0167.
   IF TEMPER <> _PA0167-PERNR OR TEMSUBTY <> _PA0167-PLTYP.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
       CNT5 = 1.
     ENDIF.

  CLEAR SSN. CONCATENATE '=c..' _PA0167-PERID INTO SSN.
  CLEAR SUBTY1.
  MOVE _PA0167-PLTYP TO SUBTY1.

    PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'SAPMP50A' '1000' ' ',
                         ' ' 'RP50G-PERNR' SSN ' ',
                         ' ' 'RP50G-CHOIC' '0167' ' ',
                         ' ' 'RP50G-SUBTY' SUBTY1 ' ' , "subtype
                         ' ' 'BDC_OKCODE' '/06' ' '.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'MP016700' '2000' ' ',
                         ' ' 'P0167-BEGDA' _PA0167-BEGDA1 ' ',
                         ' ' 'P0167-ENDDA' _PA0167-ENDDA1 ' '.
    IF _PA0167-BPLAN NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0167-BPLAN' _PA0167-BPLAN ' '.
    ENDIF.
    IF _PA0167-BOPTI NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0167-BOPTI' _PA0167-BOPTI ' '.
    ENDIF.
    IF _PA0167-DEPCV NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0167-DEPCV' _PA0167-DEPCV ' '.
    ENDIF.
              PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0167-DTY' _PA0167-DTY CNT5,
                         ' ' 'P0167-DID' _PA0167-DID CNT5.
  ELSE.
              PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0167-DTY' _PA0167-DTY CNT5,
                         ' ' 'P0167-DID' _PA0167-DID CNT5.
  ENDIF.
  CNT5 = CNT5 + 1.
  TEMPER = _PA0167-PERNR.
  TEMSUBTY = _PA0167-PLTYP.
  ENDLOOP.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'BDC_OKCODE' '/11' ' '.        " SAVE
 perform insert_bdc tables bdc_data using 'PA30'.
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
     CONCATENATE NAME ' ' IDX ' ' INTO BDC_DATA-FNAM.
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
