* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Dependent/Beneficiary Load
* Version 1.0  - August 2000

* PA Infotype 0169
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPDP016946C MESSAGE-ID ZP.

*DATA DEFINITION
* Internal table declaration for reading the source data
*** Modified By Mrudula 30-Mar-00.
*** Field 'IAM' Added.

DATA: BEGIN OF _PA0169 OCCURS 0.
       INCLUDE STRUCTURE PA0169.
DATA : PERID(13),BEGDA1(10),ENDDA1(10),
       ITS(1),DTY(4),DID(2),BPT(3),
       CIS(1),CTY(4),CID(2),CPT(3),
       INV(4),IPT(3),IAM(15),SPADT1(10).
DATA: END OF _PA0169.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0169 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\D169.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRDP0169' SY-UNAME.
PERFORM UPLOAD_0169 USING FILE0169 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0169
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0169 USING F ERR.

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
 _PA0169-PERID _PA0169-BEGDA1 _PA0169-ENDDA1 _PA0169-PLTYP _PA0169-BPLAN
 _PA0169-ITS _PA0169-DTY _PA0169-DID _PA0169-BPT
 _PA0169-CIS _PA0169-CTY _PA0169-CID _PA0169-CPT
 _PA0169-INV _PA0169-IPT _PA0169-IAM _PA0169-SPADT1.
    MOVE  _PA0169-PERID TO _PA0169-PERNR.

 IF _PA0169-PERID NE SPACE.
    APPEND _PA0169.
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
DATA: TEMPER LIKE PA0169-PERNR VALUE '00000000'.
DATA: TEMSUBTY LIKE _PA0169-PLTYP VALUE '0000'.
DATA: SUBTY1 LIKE _PA0169-PLTYP.
CLEAR _PA0169.
CNT5 = 1.
CNT6 = 1.
SORT _PA0169 BY PERID PLTYP DTY DID INV IPT.
LOOP AT _PA0169.
   IF TEMPER <> _PA0169-PERNR OR TEMSUBTY <> _PA0169-PLTYP.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
       CNT5 = 1.
       CNT6 = 1.
     ENDIF.

  CLEAR SSN. CONCATENATE '=c..' _PA0169-PERID INTO SSN.
  CLEAR SUBTY1.
    MOVE _PA0169-PLTYP TO SUBTY1.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'SAPMP50A' '1000' ' ',
                         ' ' 'RP50G-PERNR' SSN ' ',
                         ' ' 'RP50G-CHOIC' '0169' ' ',
                         ' ' 'RP50G-SUBTY' SUBTY1 ' ' , "subtype
                         ' ' 'BDC_OKCODE' '/06' ' '.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'MP016900' '2000' ' ',
                         ' ' 'P0169-BEGDA' _PA0169-BEGDA1 ' ',
                         ' ' 'P0169-ENDDA' _PA0169-ENDDA1 ' '.
     IF _PA0169-BPLAN NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0169-BPLAN' _PA0169-BPLAN ' '.
     ENDIF.
     IF _PA0169-SPADT1 NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0169-SPADT' _PA0169-SPADT1 ' '.
     ENDIF.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0169-ITS' _PA0169-ITS CNT5,
                         ' ' 'P0169-DTY' _PA0169-DTY CNT5,
                         ' ' 'P0169-DID' _PA0169-DID CNT5,
                         ' ' 'P0169-BPT' _PA0169-BPT CNT5,
                         ' ' 'P0169-CIS' _PA0169-CIS CNT6,
                         ' ' 'P0169-CTY' _PA0169-CTY CNT6,
                         ' ' 'P0169-CID' _PA0169-CID CNT6,
                         ' ' 'P0169-CPT' _PA0169-CPT CNT6,
                         ' ' 'P0169-INV' _PA0169-INV CNT5,
                         ' ' 'P0169-IPT' _PA0169-IPT CNT5,
                         ' ' 'P0169-IAM' _PA0169-IAM CNT5.
     ELSE.
      IF CNT5 <= 20.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0169-ITS' _PA0169-ITS CNT5,
                         ' ' 'P0169-DTY' _PA0169-DTY CNT5,
                         ' ' 'P0169-DID' _PA0169-DID CNT5,
                         ' ' 'P0169-BPT' _PA0169-BPT CNT5,
                         ' ' 'P0169-INV' _PA0169-INV CNT5,
                         ' ' 'P0169-IPT' _PA0169-IPT CNT5,
                         ' ' 'P0169-IAM' _PA0169-IAM CNT5.
       ENDIF.
      IF CNT6 <= 10.
                PERFORM DYNPRO TABLES BDC_DATA USING:
                         ' ' 'P0169-CIS' _PA0169-CIS CNT6,
                         ' ' 'P0169-CTY' _PA0169-CTY CNT6,
                         ' ' 'P0169-CID' _PA0169-CID CNT6,
                         ' ' 'P0169-CPT' _PA0169-CPT CNT6.
      ENDIF.
    ENDIF.
   CNT5 = CNT5 + 1.
   CNT6 = CNT6 + 1.
   TEMPER = _PA0169-PERNR.
   TEMSUBTY = _PA0169-PLTYP.
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
