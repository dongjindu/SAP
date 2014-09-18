* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1005
* Authors : Mrudula - Annance Consulting
* Changed : 4/20/2000.Sunil Dandekar
* This infotype can save data only for one TAB i.e Pay Grade or  *
* PayScale or Direct. The user needs to select the tab by putting 'X' *
* in the template.
*---------------------------------------------------------------------*

REPORT ZPPD100546C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1005 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1005.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1005 OCCURS 10.
        INCLUDE STRUCTURE P1005.
DATA : BEGDA1(10),ENDDA1(10),DPATT(40),GRADE_TAB,MOLGA1(2),SLTYP(2),
       SLREG(2),SLGRP(8),SLLV1(2),SLLV2(2),INDDA1(10),SCALE_TAB,
       MOLGA2(2),TRFAR1(2),TRFGB1(2),TRFKZ1(1),TRFG11(8),TRFG21(8),
       TRFS11(2),TRFS21(2),INDDA2(10),FREQU1(1),DIRECT_TAB,
       CURCY1(5),CPMIN1(18),CPMAX1(18),FREQU2(1).
DATA : END OF _P1005.


 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1005' SY-UNAME.
LOOP AT _P1005.
 IF _P1005-GRADE_TAB cs 'X'  OR " To check if either tab  has been
    _P1005-SCALE_TAB cs 'X' OR  " selected.
    _P1005-DIRECT_TAB cs 'X'.
   IF ( _P1005-GRADE_TAB cs 'X'  AND  _P1005-SCALE_TAB cs 'X' ) OR
      ( _P1005-GRADE_TAB cs 'X' AND  _P1005-DIRECT_TAB cs 'X' ) OR
      ( _P1005-SCALE_TAB CS 'X' AND _P1005-DIRECT_TAB CS 'X' ).
  ELSE.
    PERFORM POPULATE_BDC.
    TRCODE = 'PP02' .
    PERFORM INSERT_BDC TABLES BDC_DATA USING TRCODE.
    CNT = CNT + 1.
  ENDIF. " No 2 tabs are selected.
ENDIF. " At least 1 tab selected. cs
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
            ' ' 'PPHDR-OTYPE' _P1005-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1005-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1005' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1005-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1005-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1005-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1005-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5

    IF _P1005-GRADE_TAB cs 'X'.
       PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP100500' '2000' ' ',
            ' ' 'Q1005-MOLGA' _P1005-MOLGA1 ' ',
            ' ' 'Q1005-SLTYP' _P1005-SLTYP ' ',
            ' ' 'Q1005-SLREG' _P1005-SLREG ' ',
            ' ' 'Q1005-SLGRP' _P1005-SLGRP ' ',
            ' ' 'Q1005-SLLV1' _P1005-SLLV1 ' ',
            ' ' 'Q1005-SLLV2' _P1005-SLLV2 ' ',
            ' ' 'Q1005-INDDA' _P1005-INDDA1 ' ',
            ' ' 'BDC_OKCODE' '/11' ' '.
      PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' '         ,
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK

    ENDIF. "FOR PAY-GRADE_TAB


    IF _P1005-SCALE_TAB cs 'X' AND _P1005-GRADE_TAB Ns 'X' AND
       _P1005-DIRECT_TAB Ns 'X'.

       PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP100500' '2000' ' ',
            ' ' 'BDC_OKCODE' 'TRF' ' '.
       PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP100500' '2000' ' ',
            ' ' 'Q1005-MOLGA' _P1005-MOLGA2 ' ',
            ' ' 'Q1005-TRFAR' _P1005-TRFAR1 ' ',
            ' ' 'Q1005-TRFGB' _P1005-TRFGB1 ' ',
            ' ' 'Q1005-TRFKZ' _P1005-TRFKZ1 ' ',
            ' ' 'Q1005-TRFG1' _P1005-TRFG11 ' ',
            ' ' 'Q1005-TRFG2' _P1005-TRFG21 ' ',
            ' ' 'Q1005-TRFS1' _P1005-TRFS11 ' ',
            ' ' 'Q1005-TRFS2' _P1005-TRFS21 ' ',
            ' ' 'Q1005-INDDA' _P1005-INDDA2 ' ',
            ' ' 'BDC_OKCODE' '/11' ' '.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' '         ,
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
    ENDIF. "FOR PAY_SCALE_TAB

    IF _P1005-DIRECT_TAB cs 'X' AND  _P1005-SCALE_TAB Ns 'X' AND
       _P1005-GRADE_TAB Ns 'X'.

       PERFORM DYNPRO TABLES BDC_DATA USING:
              'X' 'MP100500' '2000' ' ',
              ' ' 'BDC_OKCODE' 'DIR' ' '.
       PERFORM DYNPRO TABLES BDC_DATA USING:
             'X' 'MP100500' '2000' ' '.
       IF _P1005-FREQU1 NE SPACE.
        PERFORM DYNPRO TABLES BDC_DATA USING:
             ' ' 'Q1005-FREQU' _P1005-FREQU1 ' '.
       ENDIF.
       IF _P1005-CURCY1 NE SPACE.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'Q1005-CURCY' _P1005-CURCY1 ' '.
       ENDIF.
       IF _P1005-CPMIN1 NE SPACE.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'Q1005-CPMIN' _P1005-CPMIN1 ' '.
       ENDIF.
       IF _P1005-CPMAX1 NE SPACE.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'Q1005-CPMAX' _P1005-CPMAX1 ' '.
       ENDIF.
       IF _P1005-FREQU2 NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
             ' ' 'Q1005-FREQU' _P1005-FREQU2 ' '.
       ENDIF.

        PERFORM DYNPRO TABLES BDC_DATA USING:
             ' ' 'BDC_OKCODE' '/11' ' '.            "SAVE
        PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' '         ,
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK

     ENDIF. "FOR DIRECT_TAB


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
         FILENAME                = FILE1005
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
           _P1005-OTYPE
           _P1005-BEGDA1
           _P1005-ENDDA1
           _P1005-OBJID
           _P1005-SUBTY
           _P1005-DPATT
           _P1005-GRADE_TAB
           _P1005-MOLGA1
           _P1005-SLTYP
           _P1005-SLREG
           _P1005-SLGRP
           _P1005-SLLV1
           _P1005-SLLV2
           _P1005-INDDA1
           _P1005-SCALE_TAB
           _P1005-MOLGA2
           _P1005-TRFAR1
           _P1005-TRFGB1
           _P1005-TRFKZ1
           _P1005-TRFG11
           _P1005-TRFG21
           _P1005-TRFS11
           _P1005-TRFS21
           _P1005-INDDA2
           _P1005-FREQU1
           _P1005-DIRECT_TAB
           _P1005-CURCY1
           _P1005-CPMIN1
           _P1005-CPMAX1
           _P1005-FREQU2.
IF   _P1005-OBJID NE SPACE.
   APPEND _P1005.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
