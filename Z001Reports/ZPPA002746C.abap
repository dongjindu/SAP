* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0027 - using PA30
* Authors : Mrudula Patel - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA002746C MESSAGE-ID ZP.

* Internal table declaration for reading the source data

DATA: BEGIN OF _P0027 OCCURS 0.
      INCLUDE STRUCTURE P0027.
DATA:  PERID(13),
       BEGDA1(10) TYPE C,
       ENDDA1(10) TYPE C,
       BUKRS1(4) TYPE C,
       KOSTL1(10) TYPE C,
       AUFNR1(12) TYPE C,
       POSNR1(8) TYPE C,
       PROZT1(5) TYPE C.
DATA: END OF _P0027.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE N.
DATA: CNT6(2) TYPE N.
DATA: D, M, CNT5 TYPE I.

PARAMETER : FILE0027 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0027.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0027' SY-UNAME.
PERFORM UPLOAD_0027 USING FILE0027 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0027
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0027 USING F ERR.

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
       _P0027-PERID _P0027-BEGDA1 _P0027-ENDDA1 _P0027-SUBTY
       _P0027-BUKRS1 _P0027-KOSTL1 _P0027-AUFNR1
       _P0027-POSNR1 _P0027-PROZT1 T.
  MOVE _P0027-PERID TO _P0027-PERNR.
    IF _P0027-PERID NE SPACE.
    APPEND _P0027.
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
DATA: TEMPER LIKE P0027-PERNR VALUE '00000000'.
DATA: TEMSUBTY LIKE _P0027-SUBTY VALUE '0000'.
DATA: SUBTY1 LIKE _P0027-SUBTY.
CLEAR _P0027.
CNT5 = 1.
SORT _P0027 BY PERNR SUBTY KOSTL1.
LOOP AT _P0027.
   IF TEMPER <> _P0027-PERNR OR TEMSUBTY <> _P0027-SUBTY.
     IF CNT5 <> 1.
       PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
       PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
       CNT5 = 1.
     ENDIF.
     CLEAR SUBTY1.
     CNT6 = '01'.
      MOVE _P0027-SUBTY TO SUBTY1.
       CLEAR SSN. CONCATENATE '=c..' _P0027-PERID INTO SSN.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                       'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'RP50G-PERNR' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0027' ' ',
                       ' ' 'RP50G-SUBTY' SUBTY1 ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

        PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP002700' '2500' ' ',
                        ' ' 'P0027-BEGDA' _P0027-BEGDA1 ' ',
                        ' ' 'P0027-ENDDA' _P0027-ENDDA1 ' ',
                        ' ' 'RHCD_TAB-BUKRS' _P0027-BUKRS1 CNT6.
     IF _P0027-AUFNR1 NE SPACE.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'RHCD_TAB-AUFNR' _P0027-AUFNR1 CNT6.
     ENDIF.
     IF _P0027-POSNR1 NE SPACE.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'RHCD_TAB-POSNR' _P0027-POSNR1 CNT6.
     ENDIF.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'RHCD_TAB-KOSTL' _P0027-KOSTL1 CNT6,
                        ' ' 'RHCD_TAB-PROZT' _P0027-PROZT1 CNT6.
   ELSE.
     D = ( CNT5 DIV 7 ).
     M = ( CNT5 MOD 7 ).
       IF D = 0.
          CNT6 = CNT5.
       ELSEIF M = 0.
          CNT6 = 7.
       ELSEIF M = 1.
          CNT6 = 8.
       ELSE.
          CNT6 = M.
       ENDIF.
       IF D <> 0 AND M = 2.
           PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' '=RHCD_NEW_ENTRIES' ' '.
           PERFORM DYNPRO TABLES BDC_DATA USING:
                        'X' 'MP002700' '2500' ' '.
           PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' '=RHCD_NEW_ENTRIES' ' '.
           PERFORM DYNPRO TABLES BDC_DATA USING:
                        'X' 'MP002700' '2500' ' '.
       ENDIF.

          PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'RHCD_TAB-BUKRS' _P0027-BUKRS1 CNT6.
         IF _P0027-AUFNR1 NE SPACE.
           PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'RHCD_TAB-AUFNR' _P0027-AUFNR1 CNT6.
         ENDIF.
         IF _P0027-POSNR1 NE SPACE.
           PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'RHCD_TAB-POSNR' _P0027-POSNR1 CNT6.
         ENDIF.
           PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'RHCD_TAB-KOSTL' _P0027-KOSTL1 CNT6,
                        ' ' 'RHCD_TAB-PROZT' _P0027-PROZT1 CNT6.
    ENDIF.
    CNT5 = CNT5 + 1.
    TEMPER = _P0027-PERNR.
    TEMSUBTY = _P0027-SUBTY.
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
