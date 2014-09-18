* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1028
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPPD102846C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1028 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1028.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1028 OCCURS 10.
        INCLUDE STRUCTURE P1028.
DATA : BEGDA1(10),ENDDA1(10),DPATT(40),ENTKM1(3).
DATA : END OF _P1028.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1028' SY-UNAME.
LOOP AT _P1028.
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
            ' ' 'PPHDR-OTYPE' _P1028-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1028-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1028' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1028-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1028-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1028-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1028-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5

  IF _P1028-OTYPE = 'O'.
      PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP102800' '2000' ' '.
      IF _P1028-CNAME NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-CNAME' _P1028-CNAME ' '.   "ADDRESS SUPPL
      ENDIF.
      IF _P1028-STRAS NE SPACE.
        PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-STRAS' _P1028-STRAS ' '.   "HOUSE NO & STREET
       ENDIF.
       IF _P1028-STRS2 NE SPACE.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-STRS2' _P1028-STRS2 ' '.   "HOUSE NO & STREET
       ENDIF.
       IF _P1028-HAUSN NE SPACE.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-HAUSN' _P1028-HAUSN ' '.   "HOUSE NO
        ENDIF.
       IF _P1028-PSTLZ NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-PSTLZ' _P1028-PSTLZ ' '.   "POSTAL CODE
        ENDIF.
        IF _P1028-ORT01 NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-ORT01' _P1028-ORT01 ' '.   "LOCATION
        ENDIF.
        IF _P1028-LAND1 NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-LAND1' _P1028-LAND1 ' '.   "COUNTRY
        ENDIF.
        IF _P1028-REGIO NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-REGIO' _P1028-REGIO ' '.   "REGION
        ENDIF.
        IF _P1028-TELNR NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-TELNR' _P1028-TELNR ' '.   "TEL NO
        ENDIF.
        IF _P1028-FAXNR NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-FAXNR' _P1028-FAXNR ' '.   "FAX NO
        ENDIF.
        IF _P1028-ENTKM1 NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-ENTKM' _P1028-ENTKM1 ' '.   "DISTANCE IN KM
        ENDIF.
  ELSEIF _P1028-OTYPE = 'S'.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'MP102800' '2100' ' '.
        IF _P1028-BUILD NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-BUILD' _P1028-BUILD ' '.   "BUILDING
        ENDIF.
        IF _P1028-ROOM1 NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-ROOM1' _P1028-ROOM1 ' '.   "PHYSICAL ROOM NUMBER
        ENDIF.
        IF _P1028-TELNR NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-TELNR' _P1028-TELNR ' '.   "TEL NO
        ENDIF.
        IF _P1028-FAXNR NE SPACE.
          PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'P1028-FAXNR' _P1028-FAXNR ' '.   "FAX NO
        ENDIF.
  ENDIF.
           PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.            "SAVE
           PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
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
         FILENAME                = FILE1028
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
           _P1028-OTYPE
           _P1028-BEGDA1
           _P1028-ENDDA1
           _P1028-OBJID
           _P1028-SUBTY
           _P1028-DPATT
           _P1028-CNAME
           _P1028-STRAS
           _P1028-STRS2
           _P1028-HAUSN
           _P1028-PSTLZ
           _P1028-ORT01
           _P1028-LAND1
           _P1028-REGIO
           _P1028-TELNR
           _P1028-FAXNR
           _P1028-ENTKM1
           _P1028-BUILD
           _P1028-ROOM1.
IF   _P1028-OBJID NE SPACE.
   APPEND _P1028.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
