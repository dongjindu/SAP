* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1017
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPPD101746C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1017 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1017.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1017 OCCURS 10.
        INCLUDE STRUCTURE P1017.
DATA : BEGDA1(10),ENDDA1(10),PROFL1(12),DPATT(40).
DATA : END OF _P1017.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1017' SY-UNAME.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.



** FORMS

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
DATA: CNT7(3) TYPE N.
DATA: TEMPER LIKE _P1017-OBJID VALUE '00000000'.
DATA: OBJID1 LIKE _P1017-OBJID.
DATA: TEMSUBTY LIKE _P1017-SUBTY VALUE '0000'.
DATA: SUBTY1 LIKE _P1017-SUBTY.
CLEAR _P1017.
CNT5 = 1.
SORT _P1017 BY OBJID SUBTY.
LOOP AT _P1017.
   IF TEMPER <> _P1017-OBJID OR TEMSUBTY <> _P1017-SUBTY.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING: ' ' 'BDC_OKCODE' '/11' ' ',
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PP02'.
       CNT5 = 1.
     ENDIF.
      CLEAR OBJID1.
      CLEAR SUBTY1.
      MOVE _P1017-OBJID TO OBJID1.
      MOVE _P1017-SUBTY TO SUBTY1.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1017-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' OBJID1 ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1017' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' SUBTY1 ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1017-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1017-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1017-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' ',          "CREATE-F5
            'X' 'MP101700' '2000' ' ',
            ' ' 'PT1017-PROFL' _P1017-PROFL1 CNT5. "PROFILE
    ELSE.
      IF CNT5 <= 11.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PT1017-PROFL' _P1017-PROFL1 CNT5. "PROFILE
CNT6 = 2.
    ELSEIF CNT5 > 11 AND CNT5 < 22.
       IF CNT5 = 12.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' 'NEWE' ' '.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP101700' '2000' ' '.
       ENDIF.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PT1017-PROFL' _P1017-PROFL1 CNT6. "PROFILE
      CNT6 = CNT6 + 1.
    ELSE.
      CNT7 = ( CNT5 MOD 11 ) + 2.
      IF CNT7 = 2.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' 'NEWE' ' '.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP101700' '2000' ' '.
      ENDIF.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PT1017-PROFL' _P1017-PROFL1 CNT7. "PROFILE
    ENDIF.
    ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P1017-OBJID.
    TEMSUBTY = _P1017-SUBTY.
    ENDLOOP.
    PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' ',            "SAVE
            'X' 'SAPMH5A0' '1000' ' '         ,
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
      PERFORM INSERT_BDC TABLES BDC_DATA USING 'PP02'.
ENDFORM.                    " POPULATE_BDC

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

DATA : BEGIN OF WA OCCURS 100,
       STR(1000),
       END OF WA.

CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
*         CODEPAGE                = ' '
         FILENAME                = FILE1017
         FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
*
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*    IMPORTING
*         FILELENGTH              =
     TABLES
          DATA_TAB                = WA
     EXCEPTIONS
          CONVERSION_ERROR        = 1
          FILE_OPEN_ERROR         = 2
          FILE_READ_ERROR         = 3
          INVALID_TABLE_WIDTH     = 4
          INVALID_TYPE            = 5
          NO_BATCH                = 6
          UNKNOWN_ERROR           = 7
          GUI_REFUSE_FILETRANSFER = 8
          OTHERS                  = 9.


 LOOP AT WA.
   SPLIT WA-STR AT DELIMITER INTO
           _P1017-OTYPE
           _P1017-BEGDA1
           _P1017-ENDDA1
           _P1017-OBJID
           _P1017-SUBTY
           _P1017-DPATT
           _P1017-PROFL1.
IF   _P1017-OBJID NE SPACE.
   APPEND _P1017.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
