* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0011 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA001146C MESSAGE-ID ZP.
TABLES: Q0011.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P0011 OCCURS 0,
        PERID(13),
        EITXT LIKE Q0011-EITXT,
        ZNPER LIKE Q0011-ZNPER,
        ZEITX LIKE Q0011-ZEITX,
        BETRG1(9) TYPE C,
        ANZHL1(7) TYPE C,
        BEGDA1(10),
        ENDDA1(10),
        ZDATE1(10),
        ZANZL1(7) TYPE C.
        INCLUDE STRUCTURE P0011.
DATA: END OF _P0011.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0011 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0011.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0011' SY-UNAME.
PERFORM UPLOAD_0011 USING FILE0011 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0011 USING F ERR.

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
       _P0011-PERID _P0011-BEGDA1 _P0011-ENDDA1 _P0011-LGART
       _P0011-WAERS _P0011-BETRG1 _P0011-ANZHL1 _P0011-EITXT
       _P0011-ZFPER _P0011-ZNPER _P0011-ZDATE1 _P0011-ZANZL1
       _P0011-ZEITX _P0011-EMFSL _P0011-EMFTX _P0011-BKPLZ
       _P0011-BKORT _P0011-BANKS _P0011-BANKL _P0011-BANKN
       _P0011-ZLSCH _P0011-ZWECK _P0011-BKONT _P0011-PRITY.
  MOVE _P0011-PERID TO _P0011-PERNR.
    IF _P0011-PERID NE SPACE.
     APPEND _P0011.
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
LOOP AT _P0011.
 CLEAR SSN. CONCATENATE '=c..' _P0011-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0011' ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP001100' '2000' ' ',
                        ' ' 'P0011-BEGDA' _P0011-BEGDA1 ' ',
                        ' ' 'P0011-ENDDA' _P0011-ENDDA1 ' ',
                        ' ' 'P0011-LGART' _P0011-LGART ' ',
                        ' ' 'P0011-WAERS' _P0011-WAERS ' ',
                        ' ' 'P0011-BETRG' _P0011-BETRG1 ' ',
                        ' ' 'P0011-ANZHL' _P0011-ANZHL1 ' ',
                        ' ' 'Q0011-EITXT' _P0011-EITXT ' ',
                        ' ' 'P0011-ZFPER' _P0011-ZFPER ' ',
                        ' ' 'Q0011-ZNPER' _P0011-ZNPER ' ',
                        ' ' 'P0011-ZDATE' _P0011-ZDATE1 ' ',
                        ' ' 'Q0011-ZANZL' _P0011-ZANZL1 ' ',
                        ' ' 'Q0011-ZEITX' _P0011-ZEITX ' ',
                        ' ' 'P0011-EMFSL' _P0011-EMFSL ' ',
                        ' ' 'Q0011-EMFTX' _P0011-EMFTX ' ',
                        ' ' 'Q0011-BKPLZ' _P0011-BKPLZ ' ',
                        ' ' 'Q0011-BKORT' _P0011-BKORT ' ',
                        ' ' 'Q0011-BANKS' _P0011-BANKS ' ',
                        ' ' 'Q0011-BANKL' _P0011-BANKL ' ',
                        ' ' 'Q0011-BANKN' _P0011-BANKN ' ',
                        ' ' 'Q0011-ZLSCH' _P0011-ZLSCH ' ',
                        ' ' 'P0011-ZWECK' _P0011-ZWECK ' ',
                        ' ' 'Q0011-BKONT' _P0011-BKONT ' ',
                        ' ' 'P0011-PRITY' _P0011-PRITY ' ',
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
