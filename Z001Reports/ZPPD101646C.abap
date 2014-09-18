* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1016
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPPD101646C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1016 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1016.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1016 OCCURS 10.
        INCLUDE STRUCTURE P1016.
DATA : BEGDA1(10),ENDDA1(10),PROFILE1(12),DPATT(40).
DATA : END OF _P1016.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1016' SY-UNAME.
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
DATA: TEMPER LIKE _P1016-OBJID VALUE '00000000'.
DATA: OBJID1 LIKE _P1016-OBJID.
DATA: TEMSUBTY LIKE _P1016-SUBTY VALUE '0000'.
DATA: SUBTY1 LIKE _P1016-SUBTY.
CLEAR _P1016.
CNT5 = 1.
SORT _P1016 BY OBJID SUBTY.
LOOP AT _P1016.
   IF TEMPER <> _P1016-OBJID OR TEMSUBTY <> _P1016-SUBTY.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING: ' ' 'BDC_OKCODE' '/11' ' ',
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PP02'.
       CNT5 = 1.
     ENDIF.
      CLEAR OBJID1.
      CLEAR SUBTY1.
      MOVE _P1016-OBJID TO OBJID1.
      MOVE _P1016-SUBTY TO SUBTY1.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1016-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' OBJID1 ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1016' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' SUBTY1 ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1016-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1016-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1016-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' ',          "CREATE-F5
            'X' 'MP101600' '2000' ' ',
            ' ' 'PT1016-PROFILE' _P1016-PROFILE1 CNT5. "PROFILE
    ELSE.
* Changed By Mrudula - '10' : 30-Mar-00.
      IF CNT5 <= 10.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PT1016-PROFILE' _P1016-PROFILE1 CNT5. "PROFILE
CNT6 = 2.
* Changed By Mrudula - '10' & '20' & '11' : 30-Mar-00.
    ELSEIF CNT5 > 10 AND CNT5 < 20.
       IF CNT5 = 11.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' 'NEWE' ' '.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP101600' '2000' ' '.
       ENDIF.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PT1016-PROFILE' _P1016-PROFILE1 CNT6. "PROFILE
      CNT6 = CNT6 + 1.
    ELSE.
* Changed By Mrudula - '10' : 30-Mar-00.
      CNT7 = ( CNT5 MOD 10 ) + 2.
      IF CNT7 = 2.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' 'NEWE' ' '.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP101600' '2000' ' '.
      ENDIF.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'PT1016-PROFILE' _P1016-PROFILE1 CNT7. "PROFILE
    ENDIF.
    ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P1016-OBJID.
    TEMSUBTY = _P1016-SUBTY.
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
         FILENAME                = FILE1016
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
           _P1016-OTYPE
           _P1016-BEGDA1
           _P1016-ENDDA1
           _P1016-OBJID
           _P1016-SUBTY
           _P1016-DPATT
           _P1016-PROFILE1.
IF   _P1016-OBJID NE SPACE.
   APPEND _P1016.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
