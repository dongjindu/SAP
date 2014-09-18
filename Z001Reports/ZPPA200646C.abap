* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 2006 - using PA30
* Authors : Srinidhi/Mrudula Patel - Annance Consulting
*---------------------------------------------------------------------*
REPORT  ZPPA200646C  MESSAGE-ID ZP.

* Internal table declaration for reading the source data
DATA: BEGIN OF _P2006 OCCURS 0,
       PERID(13),
       BEGUZ1(8) TYPE C,
       ENDUZ1(8) TYPE C,
       ANZHL1(13) TYPE C,
       KVERB1(13) TYPE C,
       DESTA1(10) TYPE C,
       DEEND1(10) TYPE C,
       BEGDA1(10) TYPE C,
       ENDDA1(10) TYPE C.
       INCLUDE STRUCTURE PA2006.
DATA: END OF _P2006.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE2006 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\2006.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA2006' SY-UNAME.
PERFORM UPLOAD_2006 USING FILE2006 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_2006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_2006 USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(8192),
   END OF ITAB.

  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME               = F
           FILETYPE               = 'ASC'
*         HEADLEN                 = ' '
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*   importing
*         FILELENGTH              =
       TABLES
            DATA_TAB                = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
PERFORM CHECK_ERROR USING SY-SUBRC ERR.
DATA : T.
LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
   _P2006-PERID _P2006-BEGDA1 _P2006-ENDDA1 _P2006-SUBTY
   _P2006-BEGUZ1 _P2006-ENDUZ1 _P2006-ANZHL1
   _P2006-DESTA1 _P2006-DEEND1 _P2006-KVERB1 T.
 IF _P2006-PERID NE SPACE.
    APPEND _P2006.
 ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_SUBRC  text                                             *
*      -->P_ERR  text                                                  *
*----------------------------------------------------------------------*
FORM CHECK_ERROR USING ERR_CD STAGE.
  CASE ERR_CD.
    WHEN 0.
    WHEN OTHERS.
*      write:/ 'Error in the process ', stage, '. Error -', err_cd.
      STOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPULATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULATE_BDC.
LOOP AT _P2006.
 CLEAR SSN. CONCATENATE '=c..' _P2006-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA USING:
                       'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '2006' ' ',
                       ' ' 'RP50G-BEGDA' _P2006-BEGDA1 ' ',
                       ' ' 'RP50G-ENDDA' _P2006-ENDDA1 ' ',
                       ' ' 'RP50G-SUBTY' _P2006-SUBTY ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING:
                        'X' 'MP200000' '2250' ' ',
*                        ' ' 'P2006-KTART' _P2006-KTART ' ',
                        ' ' 'P2006-BEGUZ' _P2006-BEGUZ1 ' ',
                        ' ' 'P2006-ENDUZ' _P2006-ENDUZ1 ' ',
                        ' ' 'P2006-ANZHL' _P2006-ANZHL1 ' ',
                        ' ' 'P2006-DESTA' _P2006-DESTA1 ' ',
                        ' ' 'P2006-DEEND' _P2006-DEEND1 ' '.
  IF _P2006-KVERB1 NE SPACE.
  PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P2006-KVERB' _P2006-KVERB1 ' '.
   ENDIF.
   PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' '/11' ' ' .

     PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
ENDLOOP.
ENDFORM.                    " POPULATE_BDC

*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_DATA  text                                             *
*      -->P_0153   text                                                *
*      -->P_0154   text                                                *
*      -->P_0155   text                                                *
*      -->P_0156   text                                                *
*----------------------------------------------------------------------*
FORM DYNPRO TABLES BDC_DATA STRUCTURE BDCDATA
           USING  DYNBEGIN NAME VALUE IDX.
 IF DYNBEGIN = 'X'.
   CLEAR   BDC_DATA.
   BDC_DATA-PROGRAM = NAME.
   BDC_DATA-DYNPRO = VALUE.
   BDC_DATA-DYNBEGIN = 'X'.
   APPEND BDC_DATA.
 ELSE.
   CLEAR   BDC_DATA.
   IF IDX = ' '.
     BDC_DATA-FNAM = NAME.
   ELSE.
     CONCATENATE NAME '(' IDX ')' INTO BDC_DATA-FNAM.
   ENDIF.
   BDC_DATA-FVAL = VALUE.
   APPEND BDC_DATA.
 ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INSERT_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_DATA  text                                             *
*      -->P_0297   text                                                *
*----------------------------------------------------------------------*
FORM INSERT_BDC TABLES BTAB USING TRCD.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = TRCD
       TABLES
            DYNPROTAB = BTAB.
  REFRESH BTAB.
  CLEAR BTAB.
  CNT1 = CNT1 + 1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0044   text                                                *
*      -->P_SY_UNAME  text                                             *
*----------------------------------------------------------------------*
FORM INIT_BDC USING SES_NAME SAP_USER.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = SES_NAME
            USER   = SAP_USER.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLOSE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM CLOSE_PROGRAM.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
*  write:/ 'No. of transaction: ',cnt1.
*  write:/ ' BDC session created ' . , cnt2, 'documents.' .

ENDFORM.
