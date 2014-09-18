* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0025 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA002546C MESSAGE-ID ZP.
TABLES: Q0025.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P0025 OCCURS 0,
        PERID(13),
        PERNR LIKE P0025-PERNR,
        BEGDA(10),
        ENDDA(10),
        KRT01 LIKE P0025-KRT01,
        PKT01(3) TYPE C,
        KSU01(6) TYPE C,
        KRT02 LIKE P0025-KRT02,
        PKT02(3) TYPE C,
        KSU02(6) TYPE C,
        KRT03 LIKE P0025-KRT03,
        PKT03(3) TYPE C,
        KSU03(6) TYPE C,
        KRT04 LIKE P0025-KRT04,
        PKT04(3) TYPE C,
        KSU04(6) TYPE C,
        KRT05 LIKE P0025-KRT05,
        PKT05(3) TYPE C,
        KSU05(6) TYPE C,
        KRT06 LIKE P0025-KRT06,
        PKT06(3) TYPE C,
        KSU06(6) TYPE C,
        GRPNR LIKE P0025-GRPNR,
        BWNAM LIKE P0025-BWNAM,
        DAT25(10) TYPE C,
        LWKJN LIKE P0025-LWKJN,
        KENJN LIKE P0025-KENJN.
DATA: END OF _P0025.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0025 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0025.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0025' SY-UNAME.
PERFORM UPLOAD_0025 USING FILE0025 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0025
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0025 USING F ERR.

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
DATA : T.
LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
       _P0025-PERID _P0025-BEGDA _P0025-ENDDA _P0025-KRT01
       _P0025-PKT01 _P0025-KSU01 _P0025-KRT02 _P0025-PKT02
       _P0025-KSU02 _P0025-KRT03 _P0025-PKT03 _P0025-KSU03
       _P0025-KRT04 _P0025-PKT04 _P0025-KSU04 _P0025-KRT05
       _P0025-PKT05 _P0025-KSU05 _P0025-KRT06 _P0025-PKT06
       _P0025-KSU06 _P0025-GRPNR _P0025-BWNAM _P0025-DAT25
       _P0025-LWKJN _P0025-KENJN T.
    MOVE  _P0025-PERID TO _P0025-PERNR.
   IF _P0025-PERID NE SPACE.
    APPEND _P0025.
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
LOOP AT _P0025.
 CLEAR SSN. CONCATENATE '=c..' _P0025-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-BEGDA' _P0025-BEGDA ' ',
                       ' ' 'RP50G-ENDDA' _P0025-ENDDA ' ',
                       ' ' 'RP50G-CHOIC' '0025' ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

*  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP002500' '2000' ' ',
*                        ' ' 'P0025-BEGDA' _P0025-BEGDA ' ',
*                        ' ' 'P0025-ENDDA' _P0025-ENDDA ' ',
*                        ' ' 'P0025-KRT01' _P0025-KRT01 ' ',
*                        ' ' 'P0025-PKT01' _P0025-PKT01 ' ',
*                        ' ' 'P0025-KSU01' _P0025-KSU01 ' ',
*                        ' ' 'P0025-KRT02' _P0025-KRT02 ' ',
*                        ' ' 'P0025-PKT02' _P0025-PKT02 ' ',
*                        ' ' 'P0025-KSU02' _P0025-KSU02 ' ',
*                        ' ' 'P0025-KRT03' _P0025-KRT03 ' ',
*                        ' ' 'P0025-PKT03' _P0025-PKT03 ' ',
*                        ' ' 'P0025-KSU03' _P0025-KSU03 ' ',
*                        ' ' 'P0025-KRT04' _P0025-KRT04 ' ',
*                        ' ' 'P0025-PKT04' _P0025-PKT04 ' ',
*                        ' ' 'P0025-KSU04' _P0025-KSU04 ' ',
*                        ' ' 'P0025-KRT05' _P0025-KRT05 ' ',
*                        ' ' 'P0025-PKT05' _P0025-PKT05 ' ',
*                        ' ' 'P0025-KSU05' _P0025-KSU05 ' ',
*                        ' ' 'P0025-KRT06' _P0025-KRT06 ' ',
*                        ' ' 'P0025-PKT06' _P0025-PKT06 ' ',
*                        ' ' 'P0025-KSU06' _P0025-KSU06 ' ',
*                        ' ' 'P0025-GRPNR' _P0025-GRPNR ' ',
*                        ' ' 'P0025-BWNAM' _P0025-BWNAM ' ',
*                        ' ' 'P0025-DAT25' _P0025-DAT25 ' ',
*                        ' ' 'P0025-LWKJN' _P0025-LWKJN ' ',
*                        ' ' 'P0025-KENJN' _P0025-KENJN ' ',
  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPLRHP6' '6000' ' ',
                         ' ' 'BDC_OKCODE' 'XADD' ' '.
  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPMSSY0' '0120' ' ',
                         ' ' 'BDC_OKCODE' 'PICK' ' '.
  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPLRHPA_SHOW' '0999' ' ',
                         ' ' 'BDC_OKCODE' 'DOIT' ' '.
  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'SAPLRHPA_SHOW' '0999' ' ',
                         ' ' 'PT1045_EXT-RATING_TXT(01)' '01' ' ',
                         ' ' 'PT1045_EXT-RATING_TXT(02)' '02' ' ',
                         ' ' 'PT1045_EXT-RATING_TXT(03)' '03' ' ',
                         ' ' 'PT1045_EXT-RATING_TXT(04)' '04' ' ',
                         ' ' 'BDC_OKCODE' '/11' ' '.

*PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP002500' '2000' ' ',

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
