* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1613
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPD161346C MESSAGE-ID ZP.
TABLES : T5U28.

* SELECTION SCREEN
** PARAMETER

PARAMETER : FILE1613 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1613.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1613 OCCURS 10.
        INCLUDE STRUCTURE P1613.
DATA : BEGDA1(10),ENDDA1(10),DPATT(40),
       PLANS1(8),WCOFFICER1(1),WCSTATE1(3),WCCODE1(4),WCSOCODE1(4),
       WCAKAREACD1(2),WCTITLEWY1(1),BEGIN1(10),END1(10).
DATA : END OF _P1613.
DATA : TCNT TYPE I VALUE 0.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1613' SY-UNAME.
SORT _P1613 BY OTYPE OBJID WCSTATE1.
DATA : TOBJID LIKE P1613-OBJID VALUE '00000000'.
LOOP AT _P1613.
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
IF TOBJID <> _P1613-OBJID.

SELECT COUNT( * ) FROM T5U28 INTO TCNT WHERE PLANS = _P1613-OBJID.
 IF TCNT = 0.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1613-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1613-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1613' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1613-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1613-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1613-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1613-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'NEWL' ' '.          "NEW ENTRY
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'V_T5U28-PLANS' _P1613-PLANS1 ' ', "POSITION
            ' ' 'V_T5U28-WCSTATE' _P1613-WCSTATE1 ' ', "WC STATE
            ' ' 'V_T5U28-WCCODE' _P1613-WCCODE1 ' ', "WC CODE
            ' ' 'V_T5U28-WCSOCODE' _P1613-WCSOCODE1 ' ',"STD OCC CODE
            ' ' 'V_T5U28-WCAKAREACD' _P1613-WCAKAREACD1 ' ',"ALKA ARE CO
            ' ' 'V_T5U28-WCTITLEWY' _P1613-WCTITLEWY1 ' ',"WC CODE WYOMI
            ' ' 'D0001_BEGIN(01)' _P1613-BEGIN1 ' ', "FROM DATE
            ' ' 'D0001_END(01)' _P1613-END1 ' '. "END DATE
   IF _P1613-WCOFFICER1 = 'X'.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'V_T5U28-WCOFFICER' 'X' ' '. "BENEFIT COMPANY OFFICER
   ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
         ' ' 'V_T5U28-WCOFFICER' ' ' ' '. "BENEFIT COMPANY OFFICER
   ENDIF.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.   "SAVE
* BEGIN PERFORM STATEMENT FOR 'CTS' REQUEST SCREEN WHICH SHOULD BE
* REMOVED WHEN LOADING THIS PROGRAM TO QAS OR PRD.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPLSTRD' '0300' ' ',
            ' ' 'BDC_OKCODE' '/00' ' '.
* END OF 'CTS' REQUEST SCREEN.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
 ELSEIF TCNT = 1.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1613-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1613-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1613' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1613-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1613-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1613-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1613-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'NEWL' ' '.          "NEW ENTRY
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'V_T5U28-PLANS' _P1613-PLANS1 ' ', "POSITION
            ' ' 'V_T5U28-WCSTATE' _P1613-WCSTATE1 ' ', "WC STATE
            ' ' 'V_T5U28-WCCODE' _P1613-WCCODE1 ' ', "WC CODE
            ' ' 'V_T5U28-WCSOCODE' _P1613-WCSOCODE1 ' ',"STD OCC CODE
            ' ' 'V_T5U28-WCAKAREACD' _P1613-WCAKAREACD1 ' ',"ALKA ARE CO
            ' ' 'V_T5U28-WCTITLEWY' _P1613-WCTITLEWY1 ' ',"WC CODE WYOMI
            ' ' 'D0001_BEGIN(01)' _P1613-BEGIN1 ' ', "FROM DATE
            ' ' 'D0001_END(01)' _P1613-END1 ' '. "END DATE
   IF _P1613-WCOFFICER1 = 'X'.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'V_T5U28-WCOFFICER' 'X' ' '. "BENEFIT COMPANY OFFICER
   ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
         ' ' 'V_T5U28-WCOFFICER' ' ' ' '. "BENEFIT COMPANY OFFICER
   ENDIF.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.   "SAVE
* BEGIN PERFORM STATEMENT FOR 'CTS' REQUEST SCREEN WHICH SHOULD BE
* REMOVED WHEN LOADING THIS PROGRAM TO QAS OR PRD.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPLSTRD' '0300' ' ',
            ' ' 'BDC_OKCODE' '/00' ' '.
* END OF 'CTS' REQUEST SCREEN.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
*         PERFORM DYNPRO TABLES BDC_DATA USING:
*            'X' 'SAPL0PUN' '2012' ' ',
*            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
 ELSE.
 PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1613-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1613-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1613' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1613-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1613-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1613-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1613-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'NEWL' ' '.          "NEW ENTRY
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'V_T5U28-PLANS' _P1613-PLANS1 ' ', "POSITION
            ' ' 'V_T5U28-WCSTATE' _P1613-WCSTATE1 ' ', "WC STATE
            ' ' 'V_T5U28-WCCODE' _P1613-WCCODE1 ' ', "WC CODE
            ' ' 'V_T5U28-WCSOCODE' _P1613-WCSOCODE1 ' ',"STD OCC CODE
            ' ' 'V_T5U28-WCAKAREACD' _P1613-WCAKAREACD1 ' ',"ALKA ARE CO
            ' ' 'V_T5U28-WCTITLEWY' _P1613-WCTITLEWY1 ' ',"WC CODE WYOMI
            ' ' 'D0001_BEGIN(01)' _P1613-BEGIN1 ' ', "FROM DATE
            ' ' 'D0001_END(01)' _P1613-END1 ' '. "END DATE
   IF _P1613-WCOFFICER1 = 'X'.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'V_T5U28-WCOFFICER' 'X' ' '. "BENEFIT COMPANY OFFICER
   ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
         ' ' 'V_T5U28-WCOFFICER' ' ' ' '. "BENEFIT COMPANY OFFICER
   ENDIF.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.   "SAVE
* BEGIN PERFORM STATEMENT FOR 'CTS' REQUEST SCREEN WHICH SHOULD BE
* REMOVED WHEN LOADING THIS PROGRAM TO QAS OR PRD.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPLSTRD' '0300' ' ',
            ' ' 'BDC_OKCODE' '/00' ' '.
* END OF 'CTS' REQUEST SCREEN.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
*         PERFORM DYNPRO TABLES BDC_DATA USING:
*            'X' 'SAPL0PUN' '2012' ' ',
*            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
 ENDIF.
ELSE.
TCNT = TCNT + 1.
 IF TCNT = 1.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1613-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1613-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1613' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1613-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1613-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1613-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1613-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'NEWL' ' '.          "NEW ENTRY
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'V_T5U28-PLANS' _P1613-PLANS1 ' ', "POSITION
            ' ' 'V_T5U28-WCSTATE' _P1613-WCSTATE1 ' ', "WC STATE
            ' ' 'V_T5U28-WCCODE' _P1613-WCCODE1 ' ', "WC CODE
            ' ' 'V_T5U28-WCSOCODE' _P1613-WCSOCODE1 ' ',"STD OCC CODE
            ' ' 'V_T5U28-WCAKAREACD' _P1613-WCAKAREACD1 ' ',"ALKA ARE CO
            ' ' 'V_T5U28-WCTITLEWY' _P1613-WCTITLEWY1 ' ',"WC CODE WYOMI
            ' ' 'D0001_BEGIN(01)' _P1613-BEGIN1 ' ', "FROM DATE
            ' ' 'D0001_END(01)' _P1613-END1 ' '. "END DATE
   IF _P1613-WCOFFICER1 = 'X'.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'V_T5U28-WCOFFICER' 'X' ' '. "BENEFIT COMPANY OFFICER
   ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
         ' ' 'V_T5U28-WCOFFICER' ' ' ' '. "BENEFIT COMPANY OFFICER
   ENDIF.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.   "SAVE
* BEGIN PERFORM STATEMENT FOR 'CTS' REQUEST SCREEN WHICH SHOULD BE
* REMOVED WHEN LOADING THIS PROGRAM TO QAS OR PRD.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPLSTRD' '0300' ' ',
            ' ' 'BDC_OKCODE' '/00' ' '.
* END OF 'CTS' REQUEST SCREEN.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
*         PERFORM DYNPRO TABLES BDC_DATA USING:
*            'X' 'SAPL0PUN' '2012' ' ',
*            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
  ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1613-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1613-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1613' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1613-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1613-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1613-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1613-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'NEWL' ' '.          "NEW ENTRY
   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'V_T5U28-PLANS' _P1613-PLANS1 ' ', "POSITION
            ' ' 'V_T5U28-WCSTATE' _P1613-WCSTATE1 ' ', "WC STATE
            ' ' 'V_T5U28-WCCODE' _P1613-WCCODE1 ' ', "WC CODE
            ' ' 'V_T5U28-WCSOCODE' _P1613-WCSOCODE1 ' ',"STD OCC CODE
            ' ' 'V_T5U28-WCAKAREACD' _P1613-WCAKAREACD1 ' ',"ALKA ARE CO
            ' ' 'V_T5U28-WCTITLEWY' _P1613-WCTITLEWY1 ' ',"WC CODE WYOMI
            ' ' 'D0001_BEGIN(01)' _P1613-BEGIN1 ' ', "FROM DATE
            ' ' 'D0001_END(01)' _P1613-END1 ' '. "END DATE
   IF _P1613-WCOFFICER1 = 'X'.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'V_T5U28-WCOFFICER' 'X' ' '. "BENEFIT COMPANY OFFICER
   ELSE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
         ' ' 'V_T5U28-WCOFFICER' ' ' ' '. "BENEFIT COMPANY OFFICER
   ENDIF.
   PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.   "SAVE
* BEGIN PERFORM STATEMENT FOR 'CTS' REQUEST SCREEN WHICH SHOULD BE
* REMOVED WHEN LOADING THIS PROGRAM TO QAS OR PRD.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPLSTRD' '0300' ' ',
            ' ' 'BDC_OKCODE' '/00' ' '.
* END OF 'CTS' REQUEST SCREEN.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2012' ' ',
            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
*         PERFORM DYNPRO TABLES BDC_DATA USING:
*            'X' 'SAPL0PUN' '2012' ' ',
*            ' ' 'BDC_OKCODE' 'UEBE' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PUN' '2011' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '.           "BACK
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
 ENDIF.
ENDIF.
TOBJID = _P1613-OBJID.
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
         FILENAME                = FILE1613
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
           _P1613-OTYPE
           _P1613-BEGDA1
           _P1613-ENDDA1
           _P1613-OBJID
           _P1613-SUBTY
           _P1613-DPATT
           _P1613-PLANS1
           _P1613-WCOFFICER1
           _P1613-WCSTATE1
           _P1613-WCCODE1
           _P1613-WCSOCODE1
           _P1613-WCAKAREACD1
           _P1613-WCTITLEWY1
           _P1613-BEGIN1
           _P1613-END1.
IF   _P1613-OBJID NE SPACE.
   APPEND _P1613.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
