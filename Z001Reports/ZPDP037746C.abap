* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Dependent/Beneficiary Load
* Version 1.0  - August 2000

* PA Infotype 0377
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPDP037746C MESSAGE-ID ZP.

*DATA DEFINITION
* Internal table declaration for reading the source data

DATA: BEGIN OF _PA0377 OCCURS 0.
       INCLUDE STRUCTURE PA0377.
DATA: PERID(13),BEGDA1(10),ENDDA1(10),
      ITS(1),DTY(4),DID(2),BPT(3),
      CIS(1),CTY(4),CID(2),CPT(3),
      INV(4),IPT(3).
DATA: END OF _PA0377.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0377 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\D377.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRDP0377' SY-UNAME.
PERFORM UPLOAD_0377 USING FILE0377 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0377
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0377 USING F ERR.

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
 _PA0377-PERID _PA0377-BEGDA1 _PA0377-ENDDA1 _PA0377-PLTYP
 _PA0377-BPLAN _PA0377-LEVL1
 _PA0377-ITS _PA0377-DTY _PA0377-DID _PA0377-BPT
 _PA0377-CIS _PA0377-CTY _PA0377-CID _PA0377-CPT
 _PA0377-INV _PA0377-IPT.
    MOVE  _PA0377-PERID TO _PA0377-PERNR.

IF _PA0377-PERID NE SPACE.
    APPEND _PA0377.
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
DATA: CNT6(2) TYPE N.
DATA: TEMPER LIKE PA0377-PERNR VALUE '00000000'.
DATA: TEMSUBTY LIKE _PA0377-PLTYP VALUE '0000'.
DATA: SUBTY1 LIKE _PA0377-PLTYP.
CLEAR _PA0377.
CNT5 = 1.
CNT6 = 1.
SORT _PA0377 BY PERID PLTYP DTY DID INV IPT.
LOOP AT _PA0377.
   IF TEMPER <> _PA0377-PERNR OR TEMSUBTY <> _PA0377-PLTYP.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
       CNT5 = 1.
       CNT6 = 1.
     ENDIF.

  CLEAR SSN. CONCATENATE '=c..' _PA0377-PERID INTO SSN.
    CLEAR SUBTY1.
    MOVE _PA0377-PLTYP TO SUBTY1.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'SAPMP50A' '1000' ' ',
                         ' ' 'RP50G-PERNR' SSN ' ',
                         ' ' 'RP50G-CHOIC' '0377' ' ',
                         ' ' 'RP50G-SUBTY' SUBTY1 ' ' , "subtype
                         ' ' 'BDC_OKCODE' '/06' ' '.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'MP037700' '2000' ' ',
                         ' ' 'P0377-BEGDA' _PA0377-BEGDA1 ' ',
                         ' ' 'P0377-ENDDA' _PA0377-ENDDA1 ' '.
       IF _PA0377-BPLAN NE SPACE.
            PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0377-BPLAN' _PA0377-BPLAN ' '.
       ENDIF.
       IF _PA0377-LEVL1 NE SPACE.
            PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0377-LEVL1' _PA0377-LEVL1 ' '.
       ENDIF.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0377-ITS' _PA0377-ITS CNT5,
                         ' ' 'P0377-DTY' _PA0377-DTY CNT5,
                         ' ' 'P0377-DID' _PA0377-DID CNT5,
                         ' ' 'P0377-BPT' _PA0377-BPT CNT5,
                         ' ' 'P0377-CIS' _PA0377-CIS CNT6,
                         ' ' 'P0377-CTY' _PA0377-CTY CNT6,
                         ' ' 'P0377-CID' _PA0377-CID CNT6,
                         ' ' 'P0377-CPT' _PA0377-CPT CNT6,
                         ' ' 'P0377-INV' _PA0377-INV CNT5,
                         ' ' 'P0377-IPT' _PA0377-IPT CNT5.
   ELSE.
      IF CNT5 <= 20.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0377-ITS' _PA0377-ITS CNT5,
                         ' ' 'P0377-DTY' _PA0377-DTY CNT5,
                         ' ' 'P0377-DID' _PA0377-DID CNT5,
                         ' ' 'P0377-BPT' _PA0377-BPT CNT5,
                         ' ' 'P0377-INV' _PA0377-INV CNT5,
                         ' ' 'P0377-IPT' _PA0377-IPT CNT5.
      ENDIF.
      IF CNT6 <= 10.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0377-CIS' _PA0377-CIS CNT6,
                         ' ' 'P0377-CTY' _PA0377-CTY CNT6,
                         ' ' 'P0377-CID' _PA0377-CID CNT6,
                         ' ' 'P0377-CPT' _PA0377-CPT CNT6.
       ENDIF.
    ENDIF.

   CNT5 = CNT5 + 1.
   CNT6 = CNT6 + 1.
   TEMPER = _PA0377-PERNR.
   TEMSUBTY = _PA0377-PLTYP.
  ENDLOOP.

      PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' '/11' ' '.        "Save

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
