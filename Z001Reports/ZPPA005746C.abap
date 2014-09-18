* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0057 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA005746C MESSAGE-ID ZP.
TABLES: Q0057.

* Internal table declaration for reading the source data
* Field Perid added - Mrudula.
* BDC OKCODE changed - Mrudula.

DATA: BEGIN OF _P0057 OCCURS 0,
       PERID(13),
       PERNR LIKE P0057-PERNR,
       SUBTY LIKE P0057-SUBTY,
       BEGDA(10) TYPE C,
       ENDDA(10) TYPE C,
       LGART LIKE P0057-LGART,
       BETRG(9) TYPE C,
       WAERS LIKE P0057-WAERS,
       ZWECK LIKE P0057-ZWECK,
       ANZHL(7) TYPE C,
       EITXT LIKE Q0057-EITXT,
       ZFPER LIKE P0057-ZFPER,
       ZDATE(10) TYPE C,
       ZNPER LIKE Q0057-ZNPER,
       ZANZL(3) TYPE C,
       ZEITX LIKE Q0057-ZEITX,
       EMFSL LIKE P0057-EMFSL,
       GRPRG LIKE P0057-GRPRG,
       MTGLN LIKE P0057-MTGLN.
DATA: END OF _P0057.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.


PARAMETER : FILE0057 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0057.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0057' SY-UNAME.
PERFORM UPLOAD_0057 USING FILE0057 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0057
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0057 USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
* file1(65535),
  FILE1(8192),
   END OF ITAB.



  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME                = F
           FILETYPE                = 'ASC'
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

LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
       _P0057-PERID _P0057-SUBTY _P0057-BEGDA _P0057-ENDDA
       _P0057-LGART _P0057-BETRG _P0057-WAERS _P0057-ZWECK
       _P0057-ANZHL _P0057-EITXT _P0057-ZFPER _P0057-ZDATE
       _P0057-ZNPER _P0057-ZANZL _P0057-ZEITX  _P0057-EMFSL
       _P0057-GRPRG _P0057-MTGLN.
  MOVE _P0057-PERID TO _P0057-PERNR.
   IF _P0057-PERID NE SPACE.
    APPEND _P0057.
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
LOOP AT _P0057.
 CLEAR SSN. CONCATENATE '=c..' _P0057-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0057' ' ',
                       ' ' 'RP50G-SUBTY' _P0057-SUBTY ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP005700' '2010' ' ',
                        ' ' 'P0057-BEGDA' _P0057-BEGDA ' ',
                        ' ' 'P0057-ENDDA' _P0057-ENDDA ' ',
                        ' ' 'P0057-LGART' _P0057-LGART ' ',
                        ' ' 'Q0057-BETRG' _P0057-BETRG ' ',
                        ' ' 'P0057-WAERS' _P0057-WAERS ' ',
                        ' ' 'P0057-ZWECK' _P0057-ZWECK ' ',
                        ' ' 'P0057-ANZHL' _P0057-ANZHL ' ',
                        ' ' 'Q0057-EITXT' _P0057-EITXT ' ',
                        ' ' 'P0057-ZFPER' _P0057-ZFPER ' ',
                        ' ' 'P0057-ZDATE' _P0057-ZDATE ' ',
                        ' ' 'Q0057-ZNPER' _P0057-ZNPER ' ',
                        ' ' 'Q0057-ZANZL' _P0057-ZANZL ' ',
                        ' ' 'Q0057-ZEITX' _P0057-ZEITX ' ',
                        ' ' 'P0057-EMFSL' _P0057-EMFSL ' ',
                        ' ' 'P0057-GRPRG' _P0057-GRPRG ' ',
                        ' ' 'P0057-MTGLN' _P0057-MTGLN ' ',
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
