* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1050
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPPD105046C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1050 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1050.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1050 OCCURS 10.
        INCLUDE STRUCTURE P1050.
DATA : BEGDA1(10),ENDDA1(10),DPATT(40).
DATA : END OF _P1050.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1050' SY-UNAME.
LOOP AT _P1050.
  PERFORM POPULATE_BDC.
  TRCODE = 'PP02' .
  PERFORM INSERT_BDC TABLES BDC_DATA USING TRCODE.
  CNT = CNT + 1.
ENDLOOP.
PERFORM CLOSE_PROGRAM.
*WRITE:/ 'TOTAL RECORD LOADED =', CNT.


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

   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1050-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1050-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1050' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1050-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1050-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1050-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1050-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5
      PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP105000' '2000' ' '.
    IF _P1050-EVPTS NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1050-EVPTS' _P1050-EVPTS ' '.    "Evaluation points
    ENDIF.
    IF _P1050-EVGRD NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1050-EVGRD' _P1050-EVGRD ' '.    "Evaluation group
    ENDIF.
    IF _P1050-BMARK NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1050-BMARK' _P1050-BMARK ' '.    "Benchmark job
    ENDIF.
      PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.            "SAVE
      PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '. "BACK
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
         FILENAME                = FILE1050
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
           _P1050-OTYPE
           _P1050-BEGDA1
           _P1050-ENDDA1
           _P1050-OBJID
           _P1050-SUBTY
           _P1050-DPATT
           _P1050-EVPTS
           _P1050-EVGRD
           _P1050-BMARK.
IF   _P1050-OBJID NE SPACE.
   APPEND _P1050.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
