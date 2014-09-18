* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0211 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA021146C MESSAGE-ID ZP.
TABLES: Q0211.

* Internal table declaration for reading the source data
* Field Perid added - Mrudula.
* BDC OKCODE changed - Mrudula.

DATA: BEGIN OF _P0211 OCCURS 0,
       PERID(13),
       PERNR LIKE P0211-PERNR,
       SUBTY LIKE P0211-SUBTY,
       BEGDA(10) TYPE C,
       ENDDA(10) TYPE C,
       BNAME LIKE Q0211-BNAME,
       CSTAT LIKE P0211-CSTAT,
       TRMRE LIKE P0211-TRMRE,
       REPLY(10) TYPE C,
*      refnr like p0211-refnr,
* Field objps included - Mrudula.
       OBJPS LIKE P0211-OBJPS,
*      seqnr like p0211-seqnr,
*      evtyp like p0211-evtyp,
*      evdat(10) type c,
*      secev like p0211-secev,
*      admle(10) type c,
       BAREA LIKE P0211-BAREA,
       BENGR LIKE P0211-BENGR,
       BSTAT LIKE P0211-BSTAT,
       COBST LIKE P0211-COBST,
       EVTYP LIKE P0211-EVTYP,
       EVDAT(10) TYPE C,
       SECEV LIKE P0211-SECEV,
       NOTIF(10) TYPE C.
*       inpay(10) type c.
DATA: END OF _P0211.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0211 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0211.TXT' .

PARAMETER : FILE0211 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0211.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0211' SY-UNAME.
PERFORM UPLOAD_0211 USING FILE0211 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0211 USING F ERR.

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
DATA : T .
LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
       _P0211-PERID _P0211-SUBTY _P0211-BEGDA _P0211-ENDDA
       _P0211-BNAME _P0211-CSTAT _P0211-TRMRE _P0211-REPLY
       _P0211-BAREA _P0211-BENGR _P0211-BSTAT _P0211-COBST
       _P0211-EVTYP _P0211-EVDAT _P0211-SECEV _P0211-NOTIF
* Field added objps - Mrudula.
       _P0211-OBJPS                                    T .
 MOVE _P0211-PERID TO _P0211-PERNR.
    IF _P0211-PERID NE SPACE.
    APPEND _P0211.
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
LOOP AT _P0211.
 CLEAR SSN. CONCATENATE '=c..' _P0211-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0211' ' ',
                       ' ' 'RP50G-SUBTY' _P0211-SUBTY ' ',
* Field objps added - Mrudula.
                       ' ' 'RP50G-OBJPS' _P0211-OBJPS ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP021100' '2000' ' ',
                        ' ' 'P0211-BEGDA' _P0211-BEGDA ' ',
                        ' ' 'P0211-ENDDA' _P0211-ENDDA ' ',
*                        ' ' 'Q0211-BNAME' _p0211-bname ' ',
                        ' ' 'P0211-EVTYP' _P0211-EVTYP ' ',
                        ' ' 'P0211-EVDAT' _P0211-EVDAT ' ',
                        ' ' 'P0211-SECEV' _P0211-SECEV ' ',
                        ' ' 'P0211-BAREA' _P0211-BAREA ' ',
                        ' ' 'P0211-BENGR' _P0211-BENGR ' ',
                        ' ' 'P0211-BSTAT' _P0211-BSTAT ' ',
                        ' ' 'P0211-COBST' _P0211-COBST ' ',
                        ' ' 'P0211-CSTAT' _P0211-CSTAT ' ',
                        ' ' 'P0211-TRMRE' _P0211-TRMRE ' ',
                        ' ' 'P0211-NOTIF' _P0211-NOTIF ' ',
                        ' ' 'P0211-REPLY' _P0211-REPLY ' ',
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
