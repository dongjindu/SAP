* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1015
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPPD101546C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1015 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1015.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1015 OCCURS 10.
        INCLUDE STRUCTURE P1015.
DATA : BEGDA1(10),ENDDA1(10),BUDGT1(13),WAERS1(5),DPATT(40).
DATA : END OF _P1015.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1015' SY-UNAME.
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
DATA : CNT5(2) TYPE N.
DATA: TEMPER LIKE _P1015-OBJID VALUE '00000000'.
DATA: OBJID1 LIKE _P1015-OBJID.
DATA: TEMSUBTY LIKE _P1015-SUBTY VALUE '0000'.
DATA: SUBTY1 LIKE _P1015-SUBTY.
CLEAR _P1015.
CNT5 = 1.
SORT _P1015 BY OBJID SUBTY.
LOOP AT _P1015.
   IF TEMPER <> _P1015-OBJID OR TEMSUBTY <> _P1015-SUBTY.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING: ' ' 'BDC_OKCODE' '/11' ' ',
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PP02'.
       CNT5 = 1.
     ENDIF.
      CLEAR OBJID1.
      CLEAR SUBTY1.
      MOVE _P1015-OBJID TO OBJID1.
      MOVE _P1015-SUBTY TO SUBTY1.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1015-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' OBJID1 ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1015' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' SUBTY1 ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1015-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1015-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1015-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' ',          "CREATE-F5
            'X' 'MP101500' '2000' ' ',
            ' ' 'PX015-CTYPE' _P1015-CTYPE CNT5, "WAGE ELEMENT
            ' ' 'PX015-DIRCT' _P1015-DIRCT CNT5, "DIRECT WAGE EVALUATI
            ' ' 'PX015-BUDGT' _P1015-BUDGT1 CNT5, "AMOUNT
            ' ' 'PX015-WAERS' _P1015-WAERS1 CNT5, "CURRENCY
            ' ' 'PX015-ABART' _P1015-ABART CNT5. "TIME UNIT
    ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PX015-CTYPE' _P1015-CTYPE CNT5, "WAGE ELEMENT
            ' ' 'PX015-DIRCT' _P1015-DIRCT CNT5, "DIRECT WAGE EVALUATI
            ' ' 'PX015-BUDGT' _P1015-BUDGT1 CNT5, "AMOUNT
            ' ' 'PX015-WAERS' _P1015-WAERS1 CNT5, "CURRENCY
            ' ' 'PX015-ABART' _P1015-ABART CNT5. "TIME UNIT
    ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P1015-OBJID.
    TEMSUBTY = _P1015-SUBTY.
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
         FILENAME                = FILE1015
         FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
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
           _P1015-OTYPE
           _P1015-BEGDA1
           _P1015-ENDDA1
           _P1015-OBJID
           _P1015-SUBTY
           _P1015-DPATT
           _P1015-CTYPE
           _P1015-DIRCT
           _P1015-BUDGT1
           _P1015-WAERS1
           _P1015-ABART.
IF   _P1015-OBJID NE SPACE.
   APPEND _P1015.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
