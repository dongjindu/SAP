* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1610
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*

REPORT ZPPD161046C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER
*** Modified By Mrudula. 30-Mar-00
*** Screen For CTS Request in Dev is not reqd now so it is commented out
*** Modified By Sunil. 19-Apr-00
****Check for the Job exists. New Screen added for New Job.

PARAMETER : FILE1610 LIKE  RLGRAP-FILENAME DEFAULT
          'C:\WINDOWS\SAP\1610.txt'.

* data decleration
** Tables
** internal tables

DATA : BEGIN OF _P1610 OCCURS 10.
        INCLUDE STRUCTURE P1610.
DATA : BEGDA1(10),ENDDA1(10),EEOCT1(2),AAPCT1(4),DPATT(40),
       EXEMP(1),NONEX(1).
DATA : END OF _P1610.

 DATA: BEGIN OF BDC_DATA OCCURS 100.
         INCLUDE STRUCTURE BDCDATA.
 DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE, JOB_EXISTS VALUE SPACE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.
TABLES: T5U13.
* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1610' SY-UNAME.
LOOP AT _P1610.
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

   SELECT SINGLE * FROM T5U13 WHERE STELL =  _P1610-OBJID.
    MOVE SPACE TO JOB_EXISTS.

    IF SY-SUBRC = 0.
      MOVE 'X' TO  JOB_EXISTS.
    ENDIF.

   PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPMH5A0' '1000' ' ',
            ' ' 'PPHDR-PLVAR' '01' ' ', "PLAN VERSN
            ' ' 'PPHDR-OTYPE' _P1610-OTYPE ' ', "OBJ TYPE
            ' ' 'PM0D1-SEARK' _P1610-OBJID ' ', "OBJ ID
            ' ' 'PPHDR-INFTY' '1610' ' ',       "INFOTYPE
            ' ' 'PPHDR-SUBTY' _P1610-SUBTY ' ', "SUBTYPE
            ' ' 'PPHDR-ISTAT' '1' ' ',          "PLANNING STATUS
            ' ' 'PPHDR-BEGDA' _P1610-BEGDA1 ' ', "BEGIN DT
            ' ' 'PPHDR-ENDDA' _P1610-ENDDA1 ' ', "END DT
            ' ' 'PM0D1-DPATT' _P1610-DPATT ' ' , "DATA SAMPLE
            ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5

     IF JOB_EXISTS NE 'X'.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                'X' 'SAPL0PU0' '2013' ' ',
                ' ' 'BDC_OKCODE' '/05' ' '.          "CREATE-F5
        PERFORM DYNPRO TABLES BDC_DATA USING:
                'X' 'SAPL0PU0' '2014' ' ',
                ' ' 'V_T5U13_B-STELL' _P1610-OBJID ' ',
                ' ' 'D0001_BEGIN(01)' _P1610-BEGDA1 ' ',
                ' ' 'D0001_END(01)' _P1610-ENDDA1 ' ',
                ' ' 'V_T5U13_B-EEOCT' _P1610-EEOCT1 ' ', "EEO CATEGORY
                ' ' 'V_T5U13_B-AAPCT' _P1610-AAPCT1 ' '. "AAP CATEGORY

          IF _P1610-EXEMP = 'X'.
              PERFORM DYNPRO TABLES BDC_DATA USING:
              ' ' 'Q5U00-EXEMP' 'X' ' '.            "EXEMPT FIELD
          ENDIF.
          IF _P1610-NONEX = 'X'.
              PERFORM DYNPRO TABLES BDC_DATA USING:
              ' ' 'Q5U00-NONEX' 'X' ' '.           "NON-EXEMPT FIELD
          ENDIF.



     ELSE.

         PERFORM DYNPRO TABLES BDC_DATA USING:
                'X' 'SAPL0PU0' '2014' ' ',
                ' ' 'V_T5U13_B-EEOCT' _P1610-EEOCT1 ' ', "EEO CATEGORY
                ' ' 'V_T5U13_B-AAPCT' _P1610-AAPCT1 ' '. "AAP CATEGORY

          IF _P1610-EXEMP = 'X'.
              PERFORM DYNPRO TABLES BDC_DATA USING:
              ' ' 'Q5U00-EXEMP' 'X' ' '.            "EXEMPT FIELD
          ENDIF.
          IF _P1610-NONEX = 'X'.
              PERFORM DYNPRO TABLES BDC_DATA USING:
              ' ' 'Q5U00-NONEX' 'X' ' '.           "NON-EXEMPT FIELD
          ENDIF.
     ENDIF.  "For JOB_EXISTS

     PERFORM DYNPRO TABLES BDC_DATA USING:
            ' ' 'BDC_OKCODE' '/11' ' '.          "SAVE
* BEGIN PERFORM STATEMENT FOR 'CTS' REQUEST SCREEN WHICH SHOULD BE
* REMOVED WHEN LOADING THIS PROGRAM TO QAS OR PRD.
*         PERFORM DYNPRO TABLES BDC_DATA USING:
*            'X' 'SAPLSTRD' '0300' ' ',
*            ' ' 'BDC_OKCODE' '/00' ' '.
* END OF 'CTS' REQUEST SCREEN.
         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PU0' '2014' ' ',
            ' ' 'BDC_OKCODE' '/03' ' '.
         IF JOB_EXISTS NE 'X'.
            PERFORM DYNPRO TABLES BDC_DATA USING:
                'X' 'SAPL0PU0' '2014' ' ',
               ' ' 'BDC_OKCODE' '/03' ' '.
         ENDIF.

         PERFORM DYNPRO TABLES BDC_DATA USING:
            'X' 'SAPL0PU0' '2013' ' ',
            ' ' 'BDC_OKCODE' '/03' ' '.
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
         FILENAME                = FILE1610
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
           _P1610-OTYPE
           _P1610-BEGDA1
           _P1610-ENDDA1
           _P1610-OBJID
           _P1610-SUBTY
           _P1610-DPATT
           _P1610-EEOCT1
           _P1610-AAPCT1
           _P1610-EXEMP
           _P1610-NONEX.
IF   _P1610-OBJID NE SPACE.
   APPEND _P1610.
ENDIF.
 ENDLOOP.

ENDFORM.                    " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
