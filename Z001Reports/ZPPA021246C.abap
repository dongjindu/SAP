* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 1.0  - August 2000

* PA Infotype 0130 - using PA30
* Authors : Hemang/Mrudula - Annance Consulting
* Program Changed By Mrudula On 10-Oct-2000.
*---------------------------------------------------------------------*

REPORT  ZPPA021246C  MESSAGE-ID ZP       .

* Internal table declaration for reading the source data
DATA: BEGIN OF _P0212 OCCURS 0,
       PERID(13),
       BEGDA1(10) TYPE C,
       ENDDA1(10) TYPE C,
       BNAME LIKE Q0212-BNAME.
        INCLUDE STRUCTURE PA0212.
DATA: END OF _P0212.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

PARAMETER : FILE0212 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0212.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0212' SY-UNAME.
PERFORM UPLOAD_0212 USING FILE0212 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0212
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0212 USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(8192),
   END OF ITAB.

  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME               = F
           FILETYPE               = 'ASC'
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
       _P0212-PERID  _P0212-BEGDA1 _P0212-ENDDA1 _P0212-BNAME
                                                 _P0212-BPLAN
 _P0212-BOPTI _P0212-DEPCV _P0212-COORD _P0212-PROVI _P0212-POLNR
_P0212-DSUBT _P0212-DOBJP T.
 IF _P0212-PERID NE SPACE.
    APPEND _P0212.
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
* CHANGE BY MRUDULA 10-OCT-00. >>>>
SORT _P0212 BY PERID DSUBT DOBJP.  " >>> INSERT.
* END OF CHANGE BY MRUDULA 10-OCT-00. >>>>
LOOP AT _P0212.
 CLEAR SSN. CONCATENATE '=c..' _P0212-PERID INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '1000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'RP50G-CHOIC' '0212' ' ',
                       ' ' 'BDC_OKCODE' '/05' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP021200' '2000' ' ',
                        ' ' 'P0212-BEGDA' _P0212-BEGDA1 ' ',
                        ' ' 'P0212-ENDDA' _P0212-ENDDA1 ' '.
    IF _P0212-BNAME NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'Q0212-BNAME' _P0212-BNAME ' '.
    ENDIF.

    IF _P0212-BPLAN NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-BPLAN' _P0212-BPLAN ' '.
    ENDIF.

    IF _P0212-BOPTI NE SPACE.
      PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-BOPTI' _P0212-BOPTI ' '.
    ENDIF.

    IF _P0212-DEPCV NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-DEPCV' _P0212-DEPCV ' '.
    ENDIF.

    IF _P0212-COORD NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-COORD' _P0212-COORD ' '.
    ENDIF.

    IF _P0212-PROVI NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-PROVI' _P0212-PROVI ' '.
    ENDIF.

    IF _P0212-POLNR NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-POLNR' _P0212-POLNR ' '.
    ENDIF.
    IF _P0212-DSUBT NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-DSUBT' _P0212-DSUBT ' '.
    ENDIF.
    IF _P0212-DOBJP NE SPACE.
       PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0212-DOBJP' _P0212-DOBJP ' '.
    ENDIF.

     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' '/11' ' ' .

     PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.

* CHANGE BY MRUDULA : 10-Oct-00  >>>>.

PERFORM DYNPRO TABLES BDC_DATA  USING:                      " >>> INSERT
                       'X' 'SAPMP50A' '1000' ' ',           " >>> INSERT
                       ' ' 'rp50g-pernr' SSN ' ',           " >>> INSERT
                       ' ' 'RP50G-CHOIC' '0211' ' ',        " >>> INSERT
                       ' ' 'RP50G-SUBTY' _P0212-DSUBT ' ',  " >>> INSERT
                       ' ' 'RP50G-OBJPS' _P0212-DOBJP ' ',  " >>> INSERT
                       ' ' 'BDC_OKCODE' '=COP' ' '.         " >>> INSERT

PERFORM DYNPRO TABLES BDC_DATA USING:                       " >>> INSERT
                       'X' 'MP021100' '2000' ' ',           " >>> INSERT
                        ' ' 'P0211-BEGDA' _P0212-BEGDA1 ' '," >>> INSERT
                        ' ' 'P0211-CSTAT' '3' ' ',          " >>> INSERT
                        ' ' 'BDC_OKCODE' '/11' ' ' .        " >>> INSERT
     PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.       " >>> INSERT
* END OF CHANGE BY MRUDULA : 10-Oct-00.  >>>>

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
