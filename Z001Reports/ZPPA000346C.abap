* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Individual Infotype Load
* Version 2.0  - August 2000

* PA Infotype 0003 - using PU03
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPA000346C MESSAGE-ID ZP.

* Internal table declaration for reading the source data
DATA: BEGIN OF _P0003 OCCURS 0.
        INCLUDE STRUCTURE PA0003.
DATA: BEGDA1(10),ENDDA1(10), PRDAT1(10), ABRDT1(10), RRDAT1(10),
      ABWD11(10), ABWD21(10), PERNR1(13).
DATA : RRDAF1(10), RCBON1(10), BDERR1(10), PRTEV1(10), PKGAB1(10). "  #1

DATA: END OF _P0003.

DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.


DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.


PARAMETER : FILE0003 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0003.TXT' .
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0003' SY-UNAME.
PERFORM UPLOAD_0003 USING FILE0003 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_0003 USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
*file1(65535),
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



DATA T.
LOOP AT ITAB.
  SPLIT ITAB-FILE1 AT DELIMITER INTO
       _P0003-PERNR1 _P0003-BEGDA1 _P0003-ENDDA1 _P0003-PRDAT1
       _P0003-ABWD11 _P0003-ABWD21 _P0003-ABRSP _P0003-ABRDT1
       _P0003-RRDAT1
       _P0003-RRDAF1
       _P0003-RCBON1
       _P0003-KOABR
       _P0003-PRTEV1
       _P0003-BDERR1
       _P0003-KOBDE
       _P0003-PKGAB1
       T.
   IF _P0003-PERNR1 NE SPACE.
    APPEND _P0003.
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
LOOP AT _P0003.
  CLEAR SSN. CONCATENATE '=c..' _P0003-PERNR1 INTO SSN.

 PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'SAPMP50A' '3000' ' ',
                       ' ' 'rp50g-pernr' SSN ' ',
                       ' ' 'BDC_OKCODE' 'DOIT' ' '.

  PERFORM DYNPRO TABLES BDC_DATA USING: 'X' 'MP000300' '2000' ' '.
  IF _P0003-PRDAT1 NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-PRDAT' _P0003-PRDAT1 ' '.
  ENDIF.
  IF _P0003-ABWD11 NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-ABWD1' _P0003-ABWD11 ' '.
  ENDIF.
  IF _P0003-ABWD21 NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-ABWD2' _P0003-ABWD21 ' '.
  ENDIF.
  IF _P0003-ABRSP  NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-ABRSP' _P0003-ABRSP ' '.
  ENDIF.
  IF _P0003-ABRDT1 NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:

                        ' ' 'P0003-ABRDT' _P0003-ABRDT1 ' '.
  ENDIF.
  IF _P0003-RRDAT1 NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-RRDAT' _P0003-RRDAT1 ' '.
    ENDIF.
   IF _P0003-RRDAF1 NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-RRDAF' _P0003-RRDAF1' '.
  ENDIF.
   IF _P0003-RCBON1 NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-RCBON' _P0003-RCBON1 ' ' .
  ENDIF.
   IF _P0003-KOABR NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-KOABR' _P0003-KOABR ' '.
  ENDIF.
   IF _P0003-PRTEV1 NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-PRTEV' _P0003-PRTEV1  ' '.
   ENDIF.
  IF _P0003-BDERR1 NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-BDERR' _P0003-BDERR1 ' '.
  ENDIF.
  IF _P0003-KOBDE NE SPACE.
    PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-KOBDE' _P0003-KOBDE ' '.
  ENDIF.
   IF _P0003-PKGAB1 NE SPACE.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'P0003-PKGAB' _P0003-PKGAB1 ' '.
   ENDIF.
     PERFORM DYNPRO TABLES BDC_DATA USING:
                        ' ' 'BDC_OKCODE' '/11' ' ' .
* Had to press OK Button twice - why ??! ! !
*    perform dynpro tables bdc_data using:
*                       'X' 'MP000300' '2000' ' ' ,
*                       ' ' 'BDC_OKCODE' '/11' ' ' .
PERFORM DYNPRO TABLES BDC_DATA USING:
        'X' 'SAPMP50A' '3000' ' ',
        ' ' 'BDC_OKCODE' '/03' ' '.

     PERFORM INSERT_BDC TABLES BDC_DATA USING 'PU03'.

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
*  write:/ 'No. of Transaction : ',cnt1.
*  write:/ ' BDC session created ' . " , cnt2, 'documents.' .

ENDFORM.
