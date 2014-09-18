* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0028 - using PA30
* Authors : Mrudula Patel - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA002846C MESSAGE-ID ZP.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P0028 OCCURS 0.
      INCLUDE STRUCTURE P0028.
DATA:  PERID(13),
       BEGDA1(10) TYPE C,
       ENDDA1(10) TYPE C,
       EXDAT1(10) TYPE C,
       LXDAT1(10) TYPE C,
       SBJKT(2) TYPE C,
       JNFLD(1) TYPE C,
       NMFLD(10) TYPE C,
       WTFLD(10) TYPE C,
       STEXT(25) TYPE C,
       DTFLD1(10) TYPE C.
DATA: END OF _P0028.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0028 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0028.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0028' SY-UNAME.
PERFORM UPLOAD_0028 USING FILE0028 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0028
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0028 USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
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
DATA : T.
LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
      _P0028-PERID _P0028-BEGDA1 _P0028-ENDDA1 _P0028-SUBTY
      _P0028-EXDAT1 _P0028-LXDAT1 _P0028-RESUL _P0028-DIANR _P0028-SBJKT
      _P0028-JNFLD _P0028-NMFLD _P0028-WTFLD _P0028-STEXT _P0028-DTFLD1.
  MOVE _P0028-PERID TO _P0028-PERNR.
    IF _P0028-PERID NE SPACE.
    APPEND _P0028.
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
DATA: CNT5(2) TYPE N.
DATA: N1(2) TYPE N,
      N2(2) TYPE N,
      D1 TYPE I,
      M1 TYPE I,
      P1 TYPE I,
      T1 TYPE I.
DATA: TEMPER LIKE P0028-PERNR VALUE '00000000'.
DATA: TEMSUBTY LIKE _P0028-SUBTY VALUE '0000'.
DATA: SUBTY1 LIKE _P0028-SUBTY.
CLEAR _P0028.
CNT5 = 1.
SORT _P0028 BY PERID SUBTY SBJKT.
LOOP AT _P0028.
   IF TEMPER <> _P0028-PERNR OR TEMSUBTY <> _P0028-SUBTY.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
       CNT5 = 1.
       D1 = 0.
       P1 = 0.
       T1 = 0.
     ENDIF.
     CLEAR SUBTY1.
     MOVE _P0028-SUBTY TO SUBTY1.
      CLEAR SSN. CONCATENATE '=c..' _P0028-PERID INTO SSN.
       PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'RP50G-PERNR' SSN ' ',
                       ' ' 'RP50G-BEGDA' _P0028-BEGDA1 ' ',
                       ' ' 'RP50G-ENDDA' _P0028-ENDDA1 ' ',
                       ' ' 'RP50G-CHOIC' '0028' ' ',
                       ' ' 'RP50G-SUBTY' SUBTY1 ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

     PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP002800' '2000' ' ',
                        ' ' 'P0028-EXDAT' _P0028-EXDAT1 ' ',
                        ' ' 'P0028-LXDAT' _P0028-LXDAT1 ' ',
                        ' ' 'P0028-RESUL' _P0028-RESUL ' ',
                        ' ' 'P0028-DIANR' _P0028-DIANR ' '.
*    IF _P0028-SBJKT NE SPACE.
*       PERFORM DYNPRO TABLES BDC_DATA USING:
*                        ' ' 'Q0028-SBJKT' _P0028-SBJKT CNT5.
*    ENDIF.
    N1 = _P0028-SBJKT.
    N2 = '00'.
    D1 = N1 DIV 6.
    M1 = N1 MOD 6.
    IF M1 = 0.
       D1 = D1 - 1.
    ENDIF.
    T1 = D1 - P1.
    IF T1 > 0.
       DO T1 TIMES.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' 'P+' ' '.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                        'X' 'MP002800' '2000' ' '.
          P1 = P1 + 1.
       ENDDO.
    ENDIF.
    N2 = N1 MOD 6.
    IF N2 = '00'.
       N2 = '06'.
    ENDIF.

    IF _P0028-JNFLD NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-JNFLD' _P0028-JNFLD N2.
    ENDIF.
    IF _P0028-NMFLD NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-NMFLD' _P0028-NMFLD N2.
    ENDIF.
    IF _P0028-WTFLD NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-WTFLD' _P0028-WTFLD N2.
    ENDIF.
    IF _P0028-STEXT NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-STEXT' _P0028-STEXT N2.
    ENDIF.
    IF _P0028-DTFLD1 NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-DTFLD' _P0028-DTFLD1 N2.
    ENDIF.
   ELSE.
*    IF _P0028-SBJKT NE SPACE.
*       PERFORM DYNPRO TABLES BDC_DATA USING:
*                        ' ' 'Q0028-SBJKT' _P0028-SBJKT CNT5.
*    ENDIF.
    N1 = _P0028-SBJKT.
    N2 = '00'.
    D1 = N1 DIV 6.
    M1 = N1 MOD 6.
    IF M1 = 0.
       D1 = D1 - 1.
    ENDIF.
    T1 = D1 - P1.
    IF T1 > 0.
       DO T1 TIMES.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' 'P+' ' '.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                        'X' 'MP002800' '2000' ' '.
          P1 = P1 + 1.
       ENDDO.
    ENDIF.
    N2 = N1 MOD 6.
    IF N2 = '00'.
       N2 = '06'.
    ENDIF.
    IF _P0028-JNFLD NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-JNFLD' _P0028-JNFLD N2.
     ENDIF.
    IF _P0028-NMFLD NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-NMFLD' _P0028-NMFLD N2.
    ENDIF.
    IF _P0028-WTFLD NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-WTFLD' _P0028-WTFLD N2.
    ENDIF.
    IF _P0028-STEXT NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-STEXT' _P0028-STEXT N2.
    ENDIF.
    IF _P0028-DTFLD1 NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0028-DTFLD' _P0028-DTFLD1 N2.
    ENDIF.
   ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P0028-PERNR.
    TEMSUBTY = _P0028-SUBTY.
 ENDLOOP.
 PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
 PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
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
*  write:/ 'No. of Transaction : ',cnt1.
*  write:/ ' BDC session created ' . " , cnt2, 'documents.' .

ENDFORM.
