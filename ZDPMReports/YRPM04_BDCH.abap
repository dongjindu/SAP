************************************************************************
* Created by  : Myoungho Park                                          *
* Created on  : 2003.08.14.                                            *
* Description : Equiment breakdown rate change report                  *
*                                                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.08.14.     Myoungho Park                                        *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************

REPORT YRPM04_BDCH NO STANDARD PAGE HEADING
                       LINE-SIZE  132
                       LINE-COUNT 64(1)
                       MESSAGE-ID ZMPM.
*----- Include
INCLUDE OLE2INCL.

TABLES: ZTPM_SHOP,
        ZTPM_ANBD,
        ZTPM_OPTIME.

*** Base Shop list
DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP LIKE ZTPM_SHOP-SHOP,
      END OF IT_SHOP.

*** Base Year list
DATA: BEGIN OF IT_YEAR OCCURS 0,
         AJAHR LIKE ZTPM_OPTIME-AJAHR,
      END OF IT_YEAR.

*** Annually average breakdown rate
DATA: IT_ZTPM_ANBD LIKE ZTPM_ANBD OCCURS 0 WITH HEADER LINE.

*** Monthly average breakdown rate
DATA: IT_RATE LIKE ZSPM_BDMON OCCURS 0 WITH HEADER LINE.

*** Screen Control Flag
DATA: FLG_SCREEN_INPUT  VALUE '0'.

*-----variables for excel
DATA: EXCEL TYPE OLE2_OBJECT,
      BOOKS TYPE OLE2_OBJECT,
      CELL  TYPE OLE2_OBJECT,
      SHEET TYPE OLE2_OBJECT.

*** Global variables
DATA: WA_AJAHR LIKE ZTPM_ANBD-AJAHR,
      WA_SHTXT LIKE ZTPM_SHOP-SHTXT,
      WA_ROW TYPE I,
      WA_COL TYPE I.

*** Constants
CONSTANTS : C_BASE_ROW  TYPE I VALUE 26,
            C_BASE_COL  TYPE I VALUE 2,
            C_BASE_COL2 TYPE I VALUE 8.


*********** SELECTION-SCREEN ***********************************
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_AJAHR FOR ZTPM_ANBD-AJAHR NO INTERVALS
                                             NO-EXTENSION
                                             DEFAULT SY-DATUM,
                 S_SHOP  FOR ZTPM_SHOP-SHOP  NO INTERVALS
                                             NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
PARAMETER: P_FILE LIKE RLGRAP-FILENAME MODIF ID ABC.
SELECTION-SCREEN PUSHBUTTON /70(10) PH_FNAME USER-COMMAND CHG.
SELECTION-SCREEN END OF BLOCK BLOCK2.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-003.
PARAMETER: P_FILE2 LIKE RLGRAP-FILENAME MODIF ID BCD.
SELECTION-SCREEN PUSHBUTTON /70(10) PH_NAME2 USER-COMMAND CHG2.
SELECTION-SCREEN END OF BLOCK BLOCK3.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.
  P_FILE  = '/usr/sap/trans/tmp/HMMA2.xls'.
  P_FILE2 = 'C:\SAP\TEMP\HMMA2.XLS'.
  MOVE 'Change' TO PH_FNAME.
  MOVE 'Change' TO PH_NAME2.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'CHG'.
      CLEAR: SY-UCOMM.
      PERFORM CHANGE_FILE_LOCATION.

    WHEN 'CHG2'.
      CLEAR: SY-UCOMM.
      PERFORM CHANGE_FILE_LOCATION2.
  ENDCASE.
************* AT SELECTION-SCREEN OUTPUT ******************************
***********************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'ABC'.
      SCREEN-INPUT = FLG_SCREEN_INPUT.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
****************START-OF-SELECTION ************************************
***********************************************************************
START-OF-SELECTION.
  SELECT DISTINCT SHOP  INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  ZTPM_SHOP
          WHERE SHOP IN S_SHOP
          AND   SPRAS = SY-LANGU.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-002.
  ENDIF.

  READ TABLE S_AJAHR INDEX 1.
  WA_AJAHR = S_AJAHR-LOW.

  S_AJAHR-LOW = S_AJAHR-LOW - 1.
  APPEND S_AJAHR.
  S_AJAHR-LOW = S_AJAHR-LOW - 1.
  APPEND S_AJAHR.
  S_AJAHR-LOW = S_AJAHR-LOW - 1.
  APPEND S_AJAHR.

  SELECT  DISTINCT AJAHR INTO CORRESPONDING FIELDS OF TABLE IT_YEAR
          FROM  ZTPM_OPTIME
          WHERE AJAHR IN S_AJAHR.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-004.
  ELSE.
    SORT  IT_YEAR BY AJAHR DESCENDING.
  ENDIF.

END-OF-SELECTION.
****************** END-OF-SELECTION **********************************
**********************************************************************


********************* MAIN PROCESS ***********************************
**********************************************************************
  PERFORM READ_DATA.

  PERFORM DOWNLOAD_EXCEL_FILE.

  PERFORM CREATE_EXCEL_OBJECT.

  PERFORM DISPLAY_EXCEL.
****************** END OF MAIN PROCESS *******************************
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_EXCEL_FILE.
  DATA: L_FTAPPL LIKE RCGFILETR-FTAPPL,
        L_FTFRONT LIKE RCGFILETR-FTFRONT,
        L_E_FLAG_OPEN_ERROR LIKE BOOLE-BOOLE,
        L_E_OS_MESSAGE.

  L_FTFRONT = P_FILE2.
  L_FTAPPL  = P_FILE.

*** downloading files
  CALL FUNCTION 'C13Z_FILE_DOWNLOAD_BINARY'
       EXPORTING
            I_FILE_FRONT_END    = L_FTFRONT
            I_FILE_APPL         = L_FTAPPL
            I_FILE_OVERWRITE    = 'X'
       IMPORTING
            E_FLG_OPEN_ERROR    = L_E_FLAG_OPEN_ERROR
            E_OS_MESSAGE        = L_E_OS_MESSAGE
       EXCEPTIONS
            FE_FILE_OPEN_ERROR  = 1
            FE_FILE_EXISTS      = 2
            FE_FILE_WRITE_ERROR = 3
            AP_NO_AUTHORITY     = 4
            AP_FILE_OPEN_ERROR  = 5
            AP_FILE_EMPTY       = 6
            OTHERS              = 7.
  CASE SY-SUBRC.
    WHEN 0.
    WHEN 1.
      MESSAGE I000(ZZ) WITH 'Check file is opened now'.
      STOP.
    WHEN OTHERS.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDCASE.
ENDFORM.                    " DOWNLOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FILE_LOCATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_FILE_LOCATION.
  IF FLG_SCREEN_INPUT = '0'.
    FLG_SCREEN_INPUT = '1'.
  ELSE.
    FLG_SCREEN_INPUT = '0'.
  ENDIF.
ENDFORM.                    " CHANGE_FILE_LOCATION
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FILE_LOCATION2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_FILE_LOCATION2.
  DATA: WA_RC LIKE SY-SUBRC.

*** Call file selector
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.xls,*.xls.'
            MODE             = 'S'
            TITLE            = 'Save'
       IMPORTING
            FILENAME         = P_FILE2
            RC               = WA_RC
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-M01.
  ENDIF.
ENDFORM.                    " CHANGE_FILE_LOCATION2
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
*** get Annually Breakdown rate...
  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_ANBD
           FROM  ZTPM_ANBD
           WHERE SHOP  IN S_SHOP
           AND   AJAHR IN S_AJAHR.

  READ TABLE S_SHOP INDEX 1.
  IF S_SHOP-LOW NE SPACE.
*** Calculate Monthly Breakdown rate...
    CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
         EXPORTING
              E_AJAHR = WA_AJAHR
              E_SHOP  = S_SHOP-LOW
         TABLES
              IT_RATE = IT_RATE.
  ELSE.
*** Calculate Monthly Breakdown rate...
    CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
      EXPORTING
           E_AJAHR = WA_AJAHR
*            E_SHOP  = S_SHOP-LOW
      TABLES
           T_RATE  = IT_RATE.
  ENDIF.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_EXCEL_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_EXCEL_OBJECT.
* USING EXCEL OBJECT
  CREATE OBJECT EXCEL 'EXCEL.APPLICATION'.

* select SHEET
  BOOKS = '2.°íÀåÀ²'.
  CALL METHOD OF EXCEL 'WORKBOOKS' = BOOKS.

* Open file
  CALL METHOD OF BOOKS 'OPEN' EXPORTING #1 = P_FILE2.
  IF SY-SUBRC <> 0.
    MESSAGE E000(ZZ) WITH 'Error open file!'.
  ENDIF.

* sheet Number
  CALL METHOD OF EXCEL 'Worksheets' = SHEET EXPORTING #1 = 2.
  CALL METHOD OF SHEET 'Activate'.

ENDFORM.                    " CREATE_EXCEL_OBJECT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_EXCEL.

  WA_ROW = C_BASE_ROW.
  WA_COL = C_BASE_COL.

  PERFORM WRITE_YEAR_VLAUE.

  PERFORM WRITE_MONTH_VALUE.

  SET PROPERTY OF EXCEL 'VISIBLE' = 1.

  FREE OBJECT EXCEL.
ENDFORM.                    " DISPLAY_EXCEL
*&---------------------------------------------------------------------*
*&      Form  fill_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LINE  text
*      -->P_5      text
*      -->P_ITAB_AMT05  text
*----------------------------------------------------------------------*
FORM FILL_CELL USING P_LINE P_ROW P_VALUE.

  CALL METHOD OF EXCEL 'CELLS' = CELL EXPORTING #1 = P_LINE #2 = P_ROW.
  SET PROPERTY OF CELL 'VALUE' = P_VALUE.

ENDFORM.                    " fill_cell
*&---------------------------------------------------------------------*
*&      Form  WRITE_YEAR_VLAUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_YEAR_VLAUE.
**** Write Year
  PERFORM WRITE_YEAR.

  WA_ROW = C_BASE_ROW.
  WA_COL = C_BASE_COL.

  LOOP AT IT_SHOP.
*** Write shop text
    PERFORM WRITE_SHTXT.

    LOOP AT IT_YEAR.
      WA_COL = WA_COL + 1.
      LOOP AT IT_ZTPM_ANBD WHERE SHOP  = IT_SHOP-SHOP
                           AND   AJAHR = IT_YEAR-AJAHR.
**** BD Rate Value
        PERFORM FILL_CELL USING WA_ROW
                                WA_COL
                                IT_ZTPM_ANBD-AVRATE.
      ENDLOOP.
    ENDLOOP.

    WA_ROW = WA_ROW + 1.
    WA_COL = C_BASE_COL.
  ENDLOOP.
ENDFORM.                    " WRITE_YEAR_VLAUE

*&---------------------------------------------------------------------*
*&      Form  WRITE_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_YEAR.
  WA_ROW = WA_ROW - 1.
  LOOP AT IT_YEAR.
    WA_COL = WA_COL + 1.

    PERFORM FILL_CELL USING WA_ROW
                      WA_COL
                      IT_YEAR-AJAHR.
  ENDLOOP.
ENDFORM.                    " WRITE_YEAR
*&---------------------------------------------------------------------*
*&      Form  WRITE_SHTXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_SHTXT.
  CLEAR : WA_SHTXT.
  SELECT SINGLE SHTXT INTO WA_SHTXT
         FROM   ZTPM_SHOP
         WHERE  SHOP  = IT_SHOP-SHOP
         AND    SPRAS = SY-LANGU.
**** shop text
  PERFORM FILL_CELL USING WA_ROW
                          WA_COL
                          WA_SHTXT.

ENDFORM.                    " WRITE_SHTXT
*&---------------------------------------------------------------------*
*&      Form  WRITE_MONTH_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_MONTH_VALUE.
  DATA: WA_MON(2) TYPE N.

  WA_ROW = C_BASE_ROW.
  WA_COL = C_BASE_COL2.

  LOOP AT IT_SHOP.
    DO 12 TIMES.
      WA_MON = WA_MON + 1.
      LOOP AT IT_RATE WHERE SHOP = IT_SHOP-SHOP.
        WA_COL = C_BASE_COL2 + WA_MON - 1.
        IF IT_RATE-ZMONTH = WA_MON.
          PERFORM FILL_CELL USING WA_ROW
                                  WA_COL
                                  IT_RATE-AVRATE.
        ENDIF.
      ENDLOOP.
    ENDDO.
    CLEAR: WA_MON.
    WA_ROW = WA_ROW + 1.
    WA_COL = C_BASE_COL2.
  ENDLOOP.

ENDFORM.                    " WRITE_MONTH_VALUE
