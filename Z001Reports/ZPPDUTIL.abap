* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Conversion PD Data Individual Infotypes Load
* Version 1.0  - December 98

* Personnel Development/Org Management include program
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
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
*&      Form  DETERMINE_TCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TRCODE  text                                               *
*      <--P__P1000_OTYPE  text                                         *
*----------------------------------------------------------------------*
FORM DETERMINE_TCODE CHANGING P_TRCODE
                              P__P1000_OTYPE.

CASE P__P1000_OTYPE.
  WHEN 'C' . P_TRCODE = 'PO03' .         " JOB
  WHEN 'S' . P_TRCODE = 'PO13' .         " POSITION
  WHEN 'O' . P_TRCODE = 'PO10' .         " ORG UNIT
  WHEN 'A' . P_TRCODE = 'PO01' .         " WORK CTR
  WHEN 'B' . P_TRCODE = 'PO02' .         " TRAINING PRG
  WHEN OTHERS.
   MESSAGE E991 WITH 'TCode could not be determined' .
ENDCASE.

ENDFORM.                    " DETERMINE_TCODE

****

FORM CLOSE_PROGRAM.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
* write:/ 'No. of files read: ',cnt.

*  write:/ ' BDC session created with ', cnt, 'documents.' .

ENDFORM.


FORM INIT_BDC USING SES_NAME SAP_USER.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = SES_NAME
            USER   = SAP_USER.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INSERT_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_BDC TABLES BTAB USING TRCD.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = TRCD
       TABLES
            DYNPROTAB = BTAB.
  REFRESH BTAB.
  CLEAR BTAB.
ENDFORM.
