* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0081 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA008146C MESSAGE-ID ZP.

* Internal table declaration for reading the source data
* Field Perid added - Mrudula.
* BDC OKCODE changed - Mrudula.

DATA: BEGIN OF _P0081 OCCURS 0,
       PERID(13),
       PERNR LIKE P0081-PERNR,
       BEGDA(10) TYPE C,
       ENDDA(10) TYPE C,
       WDART LIKE P0081-WDART,
       WDGRD LIKE P0081-WDGRD,
       WDPFL LIKE P0081-WDPFL,
       WDEIN LIKE P0081-WDEIN,
*       wderd like p0081-wderd,
       WDERD1(10) TYPE C,
       AWART LIKE P2001-AWART,
       BEGDA1(10) TYPE C,
       ENDDA1(10) TYPE C.
DATA: END OF _P0081.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0081 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0081.TXT' .

PARAMETER : FILE0081 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0081.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0081' SY-UNAME.
PERFORM UPLOAD_0081 USING FILE0081 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0081
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0081 USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
*  file1(65535),
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
       _P0081-PERID _P0081-BEGDA _P0081-ENDDA _P0081-WDART
       _P0081-WDGRD _P0081-WDEIN _P0081-WDPFL _P0081-WDERD1
       _P0081-AWART _P0081-BEGDA1 _P0081-ENDDA1 T.
  MOVE _P0081-PERID TO _P0081-PERNR.
    IF _P0081-PERID NE SPACE.
    APPEND _P0081.
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
DATA: TEMPER LIKE P0081-PERNR VALUE '00000000'.
CLEAR _P0081.
CNT5 = 1.
LOOP AT _P0081.
   IF TEMPER <> _P0081-PERNR.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
       CNT5 = 1.
     ENDIF.
     CLEAR SSN. CONCATENATE '=c..' _P0081-PERID INTO SSN.
     PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0081' ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

     PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP008100' '2000' ' ',
                        ' ' 'P0081-BEGDA' _P0081-BEGDA ' ',
                        ' ' 'P0081-ENDDA' _P0081-ENDDA ' ',
                        ' ' 'P0081-WDART' _P0081-WDART ' ',
                        ' ' 'P0081-WDGRD' _P0081-WDGRD ' ',
                        ' ' 'P0081-WDEIN' _P0081-WDEIN ' '.
   IF _P0081-WDPFL NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0081-WDPFL' _P0081-WDPFL ' '.
   ENDIF.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0081-WDERD' _P0081-WDERD1 ' ',
                        ' ' 'P2001-AWART' _P0081-AWART CNT5,
                        ' ' 'P2001-BEGDA' _P0081-BEGDA1 CNT5,
                        ' ' 'P2001-ENDDA' _P0081-ENDDA1 CNT5.
  ELSE.
      PERFORM DYNPRO TABLES BDC_DATA
               USING:   ' ' 'P2001-AWART' _P0081-AWART CNT5,
                        ' ' 'P2001-BEGDA' _P0081-BEGDA1 CNT5,
                        ' ' 'P2001-ENDDA' _P0081-ENDDA1 CNT5.
  ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P0081-PERNR.
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
