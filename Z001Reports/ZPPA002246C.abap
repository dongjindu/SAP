* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0022 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA002246C MESSAGE-ID ZP.
TABLES: Q0022.

* Internal table declaration for reading the source data
* Field Perid added - Mrudula.
* BDC OKCODE changed - Mrudula.

DATA: BEGIN OF _P0022 OCCURS 0,
        PERID(13),
        PERNR LIKE P0022-PERNR,
        BEGDA(10),
        ENDDA(10),
        SLART LIKE P0022-SLART,
        INSTI LIKE P0022-INSTI,
        SLABS LIKE P0022-SLABS,
        ANZKL(3) TYPE C,
        ANZEH LIKE Q0022-ANZEH,
        SLTP1 LIKE P0022-SLTP1,
        SLTP2 LIKE P0022-SLTP2,
        TX122 LIKE P0022-TX122,
        JBEZ1(11) TYPE C,
        WAERS LIKE P0022-WAERS,
        SLRZG LIKE P0022-SLRZG.
DATA: END OF _P0022.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0022 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0022.TXT' .

PARAMETER : FILE0022 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0022.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0022' SY-UNAME.
PERFORM UPLOAD_0022 USING FILE0022 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0022
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0022 USING F ERR.

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
DATA : T.  " TO STORE JUNK CHAR AT THE END OF RECORD
LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
       _P0022-PERID _P0022-BEGDA _P0022-ENDDA _P0022-SLART
       _P0022-INSTI _P0022-SLABS _P0022-ANZKL _P0022-ANZEH
       _P0022-SLTP1 _P0022-SLTP2 _P0022-TX122 _P0022-JBEZ1
       _P0022-WAERS _P0022-SLRZG T.
  MOVE  _P0022-PERID TO _P0022-PERNR.
 IF _P0022-PERID NE SPACE.
    APPEND _P0022.
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
LOOP AT _P0022.
 CLEAR SSN. CONCATENATE '=c..' _P0022-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0022' ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP002200' '2000' ' ',
                        ' ' 'P0022-BEGDA' _P0022-BEGDA ' ',
                        ' ' 'P0022-ENDDA' _P0022-ENDDA ' ',
                        ' ' 'P0022-SLART' _P0022-SLART ' ',
                        ' ' 'P0022-INSTI' _P0022-INSTI ' ',
                        ' ' 'P0022-SLABS' _P0022-SLABS ' ',
                        ' ' 'P0022-ANZKL' _P0022-ANZKL ' ',
                        ' ' 'Q0022-ANZEH' _P0022-ANZEH ' ',
                        ' ' 'P0022-SLTP1' _P0022-SLTP1 ' ',
                        ' ' 'P0022-SLTP2' _P0022-SLTP2 ' '.
*                       ' ' 'P0022-TX122' _P0022-TX122 ' '.
*                       ' ' 'P0022-JBEZ1' _P0022-JBEZ1 ' '.
*                       ' ' 'P0022-WAERS' _P0022-WAERS ' '.
 IF _P0022-SLRZG  NE SPACE.
   PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0022-SLRZG' _P0022-SLRZG ' '.
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
