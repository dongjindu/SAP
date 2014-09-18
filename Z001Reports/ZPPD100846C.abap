* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1000
* Authors :Mrudula Patel - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPD100046C MESSAGE-ID ZP.
* SELECTION SCREEN
** PARAMETER
PARAMETER : FILE1008 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1008.txt' .



* data decleration
** Tables

** internal tables

DATA : BEGIN OF _P1008 OCCURS 10.
        INCLUDE STRUCTURE P1008.
DATA : BEGDA1(10),ENDDA1(10),GDATE1(10),DPATT(40).
DATA : END OF _P1008.

DATA : T(10) TYPE C.

DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1008' SY-UNAME.
LOOP AT _P1008.
  PERFORM POPULATE_BDC.
* perform determine_tcode changing trcode _p1000-otype.
  TRCODE = 'PP02' .
  PERFORM INSERT_BDC TABLES BDC_DATA USING TRCODE.
  CNT = CNT + 1.
ENDLOOP.
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

   PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'SAPMH5A0' '1000' ' '         ,
                         ' ' 'PPHDR-OTYPE' _P1008-OTYPE ' ', "OBJ TYPE
                         ' ' 'PPHDR-PLVAR' '01'         ' ', "PLAN VERSN
                         ' ' 'PM0D1-SEARK' _P1008-OBJID ' ', "OBJ ID
                         ' ' 'PPHDR-INFTY' '1008'       ' ', "INFOTYPE
                         ' ' 'PPHDR-SUBTY' _P1008-SUBTY ' ', "SUBTYPE
                         ' ' 'PPHDR-ISTAT' '1' ' ',
                         ' ' 'PPHDR-BEGDA' _P1008-BEGDA1 ' ', "BEGIN DT
                         ' ' 'PPHDR-ENDDA' _P1008-ENDDA1 ' ', "END DT
                         ' ' 'PM0D1-DPATT' _P1008-DPATT ' ' , "DATA SMPL
                         ' ' 'BDC_OKCODE' '/05' ' '        . "CREATE-F5
 PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'MP100800' '2000' ' '        ,
                         ' ' 'P1008-BUKRS' _P1008-BUKRS ' ', "COMP CODE
                         ' ' 'P1008-GSBER' _P1008-GSBER ' ', "BUS AREA
                         ' ' 'P1008-PERSA' _P1008-PERSA ' ', "PER AREA
                         ' ' 'P1008-BTRTL' _P1008-BTRTL ' ', "PER SUB-AR
*                        ' ' 'Q1008-KOKRS' _P1008-KOKRS ' ', "CONT AREA
                         ' ' 'BDC_OKCODE' '/11' ' '        . "SAVE-F11
  PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'SAPMH5A0' '1000' ' '         ,
                         ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK

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
         FILENAME                = FILE1008
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
           _P1008-OTYPE
           _P1008-BEGDA1
           _P1008-ENDDA1
           _P1008-OBJID
           _P1008-SUBTY
           _P1008-DPATT
           _P1008-BUKRS
           _P1008-GSBER
           _P1008-PERSA
           _P1008-BTRTL.
*           _P1008-KOKRS.

 IF   _P1008-OBJID NE SPACE.
   APPEND _P1008.
 ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
