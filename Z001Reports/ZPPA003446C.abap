* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0034 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA003446C MESSAGE-ID ZP.

* Internal table declaration for reading the source data
* Field Perid added - Mrudula.
* BDC OKCODE changed - Mrudula.

DATA: BEGIN OF _P0034 OCCURS 0,
       PERID(13),
       PERNR LIKE P0034-PERNR,
       BEGDA(10) TYPE C,
       ENDDA(10) TYPE C,
       FUNKT LIKE P0034-FUNKT,
       DAT34(10) TYPE C,
       TEXTF LIKE RP50M-TEXT1.
DATA: END OF _P0034.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0034 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0034.TXT' .

PARAMETER : FILE0034 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0034.TXT' .

* Source Code

PERFORM INIT_BDC USING 'HRPA0034' SY-UNAME.
PERFORM UPLOAD_0034 USING FILE0034 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0034
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0034 USING F ERR.

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
DATA : T .
LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
       _P0034-PERID _P0034-BEGDA _P0034-ENDDA _P0034-FUNKT
       _P0034-DAT34 _P0034-TEXTF T.
 MOVE _P0034-PERID TO _P0034-PERNR.
IF _P0034-PERID NE SPACE.
    APPEND _P0034.
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
DATA: CNT5 TYPE I.
DATA: TEMPER LIKE P0034-PERNR VALUE '00000000'.
CLEAR _P0034.
CNT5 = 0.
LOOP AT _P0034.
 IF TEMPER <> _P0034-PERNR.
    IF CNT5 <> 0.
      PERFORM DYNPRO TABLES BDC_DATA USING ' ' 'BDC_OKCODE' '/11' ' ' .
      PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
      CNT5 = 0.
    ENDIF.
    CLEAR SSN. CONCATENATE '=c..' _P0034-PERID INTO SSN.
    PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0034' ' ',
                       ' ' 'RP50G-SUBTY' _P0034-FUNKT ' ', "Sub-type
                       ' ' 'BDC_OKCODE' '/05' ' '.

    PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP003400' '2000' ' ',
                        ' ' 'P0034-BEGDA' _P0034-BEGDA ' ',
                        ' ' 'P0034-ENDDA' _P0034-ENDDA ' ',
* Changed by Mrudula.
*                       ' ' 'P0034-FUNKT' _p0034-funkt ' ',
                        ' ' 'P0034-DAT34' _P0034-DAT34 ' ',
                        ' ' 'RP50M-TEXT1' _P0034-TEXTF ' '.
  ELSE.
   IF CNT5 = 1.
     PERFORM DYNPRO TABLES BDC_DATA
                  USING ' ' 'RP50M-TEXT2' _P0034-TEXTF ' '.
   ELSEIF CNT5 = 2.
     PERFORM DYNPRO TABLES BDC_DATA
                  USING ' ' 'RP50M-TEXT3' _P0034-TEXTF ' '.
   ENDIF.
  ENDIF.
   CNT5 = CNT5 + 1.
   TEMPER = _P0034-PERNR.
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
