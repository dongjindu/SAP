* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0216 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA021646C MESSAGE-ID ZP.
TABLES: Q0216.

* Internal table declaration for reading the source data
* Field Perid added - Mrudula.
* BDC OKCODE changed - Mrudula.

DATA: BEGIN OF _P0216 OCCURS 0,
       PERID(13),
       PERNR LIKE P0216-PERNR,
       SUBTY LIKE P0216-SUBTY,
       BEGDA(10) TYPE C,
       ENDDA(10) TYPE C,
       AREAS LIKE P0216-AREAS,
       ADATE(10) TYPE C,
       OROBJ LIKE P0216-OROBJ,
       ADAMT(7) TYPE C,
       AWGTY LIKE P0216-AWGTY,
       ENDDY LIKE P0216-ENDDY.
DATA: END OF _P0216.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0216 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0216.TXT' .

PARAMETER : FILE0216 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0216.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0216' SY-UNAME.
PERFORM UPLOAD_0216 USING FILE0216 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0216
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0216 USING F ERR.

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
       _P0216-PERID _P0216-SUBTY _P0216-BEGDA _P0216-ENDDA
       _P0216-AREAS _P0216-ADATE _P0216-OROBJ _P0216-ADAMT
       _P0216-AWGTY _P0216-ENDDY T.
MOVE _P0216-PERID TO _P0216-PERNR.
 IF _P0216-PERID NE SPACE.
    APPEND _P0216.
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
LOOP AT _P0216.
 CLEAR SSN. CONCATENATE '=c..' _P0216-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0216' ' ',
                       ' ' 'RP50G-SUBTY' _P0216-SUBTY ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP021600' '2000' ' ',
                        ' ' 'P0216-BEGDA' _P0216-BEGDA ' ',
                        ' ' 'P0216-ENDDA' _P0216-ENDDA ' ',
                        ' ' 'P0216-AREAS' _P0216-AREAS ' ',
                        ' ' 'P0216-ADATE' _P0216-ADATE ' ',
                        ' ' 'P0216-OROBJ' _P0216-OROBJ ' ',
                        ' ' 'P0216-ADAMT' _P0216-ADAMT ' ',
                        ' ' 'P0216-AWGTY' _P0216-AWGTY ' ',
                        ' ' 'P0216-ENDDY' _P0216-ENDDY ' ',
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
