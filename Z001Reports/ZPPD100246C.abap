* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1002
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPPD100246C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1002 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1002.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1002 OCCURS 10.
        INCLUDE STRUCTURE P1002.
DATA : BEGDA1(10),ENDDA1(10),DPATT(40),TLINE(79).
DATA : END OF _P1002.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1002' SY-UNAME.
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
DATA: TEMPER LIKE _P1002-OBJID VALUE '00000000'.
DATA: OBJID1 LIKE _P1002-OBJID.
DATA: TEMSUBTY LIKE _P1002-SUBTY VALUE '0000'.
DATA: SUBTY1 LIKE _P1002-SUBTY.
CLEAR _P1002.
CNT5 = 1.
SORT _P1002 BY OBJID SUBTY.
LOOP AT _P1002.
   IF TEMPER <> _P1002-OBJID OR TEMSUBTY <> _P1002-SUBTY.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING: ' ' 'BDC_OKCODE' '/11' ' ',
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PP02'.
       CNT5 = 1.
     ENDIF.
      CLEAR OBJID1.
      CLEAR SUBTY1.
      MOVE _P1002-OBJID TO OBJID1.
      MOVE _P1002-SUBTY TO SUBTY1.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ',          "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1002-OTYPE ' ',  "OBJ TYPE
            ' ' 'PM0D1-SEARK' OBJID1 ' ',        "OBJ ID
            ' ' 'PPHDR-INFTY' '1002' ' ',        "INFOTYPE
            ' ' 'PPHDR-SUBTY' SUBTY1 ' ',        "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',           "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1002-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1002-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1002-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' ',          "CREATE-F5
            'X' 'MP100200' '2100' ' ',
            ' ' 'PT1002-TLINE' _P1002-TLINE CNT5. "LINE
    ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PT1002-TLINE' _P1002-TLINE CNT5. "LINE
    ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P1002-OBJID.
    TEMSUBTY = _P1002-SUBTY.
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
         FILENAME                = FILE1002
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
           _P1002-OTYPE
           _P1002-BEGDA1
           _P1002-ENDDA1
           _P1002-OBJID
           _P1002-SUBTY
           _P1002-DPATT
           _P1002-TLINE.
IF   _P1002-OBJID NE SPACE.
   APPEND _P1002.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
