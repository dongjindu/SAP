************************************************************************
* Program Name      : ZAPP_ENG_PRDT_PLAN
* Creation Date     : 07/27/2006
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZAPP_ENG_PRDT_PLAN NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.
TABLES: ZTPP_ENG_PIR, t001w.
TYPE-POOLS SLIS.
DATA: BEGIN OF IT_TAB_TEMP OCCURS 0,
      WERKS LIKE MARC-WERKS,
      MATNR LIKE MARA-MATNR,
      ALC_VALS LIKE ZTPP_SEQ_SUM02-ALC_VALS,
      END OF IT_TAB_TEMP.

DATA: BEGIN OF IT_TAB OCCURS 0,
      WERKS LIKE MARC-WERKS,
      MATNR LIKE MARA-MATNR,
      MATNR_S LIKE MARA-MATNR,
*      DAYS LIKE MDSM-BDMNG,
      LABST LIKE MARD-LABST,
      SEQ(1),
      DESC(15),
      QTYD_01 LIKE MDSM-BDMNG,
      QTYD_02 LIKE MDSM-BDMNG,
      QTYD_03 LIKE MDSM-BDMNG,
      QTYD_04 LIKE MDSM-BDMNG,
      QTYD_05 LIKE MDSM-BDMNG,
      QTYD_06 LIKE MDSM-BDMNG,
      QTYD_07 LIKE MDSM-BDMNG,
      QTYD_08 LIKE MDSM-BDMNG,
      QTYD_09 LIKE MDSM-BDMNG,
      QTYD_10 LIKE MDSM-BDMNG,
      QTYD_11 LIKE MDSM-BDMNG,
      QTYD_12 LIKE MDSM-BDMNG,
      QTYD_13 LIKE MDSM-BDMNG,
      QTYD_14 LIKE MDSM-BDMNG,
      QTYD_15 LIKE MDSM-BDMNG,
      QTYD_16 LIKE MDSM-BDMNG,
      QTYD_17 LIKE MDSM-BDMNG,
      QTYD_18 LIKE MDSM-BDMNG,
      QTYD_19 LIKE MDSM-BDMNG,
      QTYD_20 LIKE MDSM-BDMNG,
      QTYD_21 LIKE MDSM-BDMNG,
      QTYW_04 LIKE MDSM-BDMNG,
      QTYW_05 LIKE MDSM-BDMNG,
      QTYW_06 LIKE MDSM-BDMNG,
      QTYW_07 LIKE MDSM-BDMNG,
      QTYW_08 LIKE MDSM-BDMNG,
      QTYW_09 LIKE MDSM-BDMNG,
      QTYW_10 LIKE MDSM-BDMNG,
      QTYW_11 LIKE MDSM-BDMNG,
      QTYW_12 LIKE MDSM-BDMNG,
      QTYW_13 LIKE MDSM-BDMNG,
      QTYW_14 LIKE MDSM-BDMNG,
      QTYW_15 LIKE MDSM-BDMNG,
      QTYW_16 LIKE MDSM-BDMNG,
      QTYW_17 LIKE MDSM-BDMNG,
      QTYW_18 LIKE MDSM-BDMNG,
      QTYW_19 LIKE MDSM-BDMNG,
      QTYW_20 LIKE MDSM-BDMNG,
      QTYW_21 LIKE MDSM-BDMNG,
      MEINS LIKE MARA-MEINS,
      TOTAL LIKE MDSM-BDMNG,
      MAKTX LIKE MAKT-MAKTX,
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
     END OF IT_TAB.

DATA: BEGIN OF IT_OUTPUT OCCURS 0,
      WERKS LIKE MARC-WERKS,
      MATNR LIKE MARA-MATNR,
      MATNR_S LIKE MARA-MATNR,
*      DAYS LIKE MDSM-BDMNG,
      LABST LIKE MARD-LABST,
      SEQ(1),
      DESC(15),
     QTYD_01(13),
      QTYD_02(13),
      QTYD_03(13),
      QTYD_04(13),
      QTYD_05(13),
      QTYD_06(13),
      QTYD_07(13),
      QTYD_08(13),
      QTYD_09(13),
      QTYD_10(13),
      QTYD_11(13),
      QTYD_12(13),
      QTYD_13(13),
      QTYD_14(13),
      QTYD_15(13),
      QTYD_16(13),
      QTYD_17(13),
      QTYD_18(13),
      QTYD_19(13),
      QTYD_20(13),
      QTYD_21(13),
      QTYW_04(13),
      QTYW_05(13),
      QTYW_06(13),
      QTYW_07(13),
      QTYW_08(13),
      QTYW_09(13),
      QTYW_10(13),
      QTYW_11(13),
      QTYW_12(13),
      QTYW_13(13),
      QTYW_14(13),
      QTYW_15(13),
      QTYW_16(13),
      QTYW_17(13),
      QTYW_18(13),
      QTYW_19(13),
      QTYW_20(13),
      QTYW_21(13),
*      MEINS LIKE MARA-MEINS,
      TOTAL LIKE MDSM-BDMNG,
      MAKTX LIKE MAKT-MAKTX,
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
*      CHECK(1),
     END OF IT_OUTPUT.

DATA: WA_TOT1 LIKE IT_TAB,
      WA_TOT2 LIKE IT_TAB,
      WA_TOT3 LIKE IT_TAB.

DATA: BEGIN OF IT_TOTAL OCCURS 0,
      WERKS LIKE MARC-WERKS,
      MATNR LIKE MARA-MATNR,
      MATNR_S LIKE MARA-MATNR,
*      DAYS LIKE MDSM-BDMNG,
      LABST LIKE MARD-LABST,
      SEQ(1),
      DESC(15),
      QTYD_01 LIKE MDSM-BDMNG,
      QTYD_02 LIKE MDSM-BDMNG,
      QTYD_03 LIKE MDSM-BDMNG,
      QTYD_04 LIKE MDSM-BDMNG,
      QTYD_05 LIKE MDSM-BDMNG,
      QTYD_06 LIKE MDSM-BDMNG,
      QTYD_07 LIKE MDSM-BDMNG,
      QTYD_08 LIKE MDSM-BDMNG,
      QTYD_09 LIKE MDSM-BDMNG,
      QTYD_10 LIKE MDSM-BDMNG,
      QTYD_11 LIKE MDSM-BDMNG,
      QTYD_12 LIKE MDSM-BDMNG,
      QTYD_13 LIKE MDSM-BDMNG,
      QTYD_14 LIKE MDSM-BDMNG,
      QTYD_15 LIKE MDSM-BDMNG,
      QTYD_16 LIKE MDSM-BDMNG,
      QTYD_17 LIKE MDSM-BDMNG,
      QTYD_18 LIKE MDSM-BDMNG,
      QTYD_19 LIKE MDSM-BDMNG,
      QTYD_20 LIKE MDSM-BDMNG,
      QTYD_21 LIKE MDSM-BDMNG,
      QTYW_04 LIKE MDSM-BDMNG,
      QTYW_05 LIKE MDSM-BDMNG,
      QTYW_06 LIKE MDSM-BDMNG,
      QTYW_07 LIKE MDSM-BDMNG,
      QTYW_08 LIKE MDSM-BDMNG,
      QTYW_09 LIKE MDSM-BDMNG,
      QTYW_10 LIKE MDSM-BDMNG,
      QTYW_11 LIKE MDSM-BDMNG,
      QTYW_12 LIKE MDSM-BDMNG,
      QTYW_13 LIKE MDSM-BDMNG,
      QTYW_14 LIKE MDSM-BDMNG,
      QTYW_15 LIKE MDSM-BDMNG,
      QTYW_16 LIKE MDSM-BDMNG,
      QTYW_17 LIKE MDSM-BDMNG,
      QTYW_18 LIKE MDSM-BDMNG,
      QTYW_19 LIKE MDSM-BDMNG,
      QTYW_20 LIKE MDSM-BDMNG,
      QTYW_21 LIKE MDSM-BDMNG,
*      MEINS LIKE MARA-MEINS,
      TOTAL LIKE MDSM-BDMNG,
      MAKTX LIKE MAKT-MAKTX,
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
*      CHECK(1),
     END OF IT_TOTAL.

*DATA: IT_TOTAL LIKE TABLE OF IT_OUTPUT WITH HEADER LINE.

*DATA: BEGIN OF IT_OUTPUT OCCURS 0,
*      WERKS LIKE MARC-WERKS,
*      MATNR LIKE MARA-MATNR,
**      DAYS LIKE MDSM-BDMNG,
*      LABST LIKE MARD-LABST,
*      SEQ(1),
**      RQTY_P LIKE MDSM-BDMNG,
*      QTYD_01 LIKE MDSM-BDMNG,
*      QTYD_02 LIKE MDSM-BDMNG,
*      QTYD_03 LIKE MDSM-BDMNG,
*      QTYD_04 LIKE MDSM-BDMNG,
*      QTYD_05 LIKE MDSM-BDMNG,
*      QTYD_06 LIKE MDSM-BDMNG,
*      QTYD_07 LIKE MDSM-BDMNG,
*      QTYD_08 LIKE MDSM-BDMNG,
*      QTYD_09 LIKE MDSM-BDMNG,
*      QTYD_10 LIKE MDSM-BDMNG,
*      QTYD_11 LIKE MDSM-BDMNG,
*      QTYD_12 LIKE MDSM-BDMNG,
*      QTYD_13 LIKE MDSM-BDMNG,
*      QTYD_14 LIKE MDSM-BDMNG,
*      QTYD_15 LIKE MDSM-BDMNG,
*      QTYD_16 LIKE MDSM-BDMNG,
*      QTYD_17 LIKE MDSM-BDMNG,
*      QTYD_18 LIKE MDSM-BDMNG,
*      QTYD_19 LIKE MDSM-BDMNG,
*      QTYD_20 LIKE MDSM-BDMNG,
*      QTYD_21 LIKE MDSM-BDMNG,
*      QTYW_04 LIKE MDSM-BDMNG,
*      QTYW_05 LIKE MDSM-BDMNG,
*      QTYW_06 LIKE MDSM-BDMNG,
*      QTYW_07 LIKE MDSM-BDMNG,
*      QTYW_08 LIKE MDSM-BDMNG,
*      QTYW_09 LIKE MDSM-BDMNG,
*      QTYW_10 LIKE MDSM-BDMNG,
*      QTYW_11 LIKE MDSM-BDMNG,
*      QTYW_12 LIKE MDSM-BDMNG,
*      QTYW_13 LIKE MDSM-BDMNG,
*      QTYW_14 LIKE MDSM-BDMNG,
*      QTYW_15 LIKE MDSM-BDMNG,
*      QTYW_16 LIKE MDSM-BDMNG,
*      QTYW_17 LIKE MDSM-BDMNG,
*      QTYW_18 LIKE MDSM-BDMNG,
*      QTYW_19 LIKE MDSM-BDMNG,
*      QTYW_20 LIKE MDSM-BDMNG,
*      QTYW_21 LIKE MDSM-BDMNG,
*      TOTAL LIKE MDSM-BDMNG,
*      MAKTX LIKE MAKT-MAKTX,
*      CT TYPE LVC_T_SCOL,
*     END OF IT_OUTPUT.

DATA : BEGIN OF IT_STOCK OCCURS 0,
       MATNR LIKE MARD-MATNR,
       SUM01 LIKE MDKP-SUM01,
       END OF IT_STOCK.

*DATA : BEGIN OF IT_MARD OCCURS 0,
*       MATNR LIKE MARD-MATNR,
*       LABST LIKE MARD-LABST,
*       END OF IT_MARD.
*
*DATA : BEGIN OF IT_MARC OCCURS 0,
*         WERKS LIKE MARC-WERKS,
*         MATNR LIKE MARC-MATNR,
*         SOBSL LIKE MARC-SOBSL,
*       END OF IT_MARC.
*
*DATA:  BEGIN OF IT_MDSM OCCURS 0,
*         WERKS LIKE MARC-WERKS,
*         MATNR LIKE MARA-MATNR,
*         BDTER LIKE MDSM-BDTER,
*         BDMNG LIKE MDSM-BDMNG,
*         SBNUM LIKE MDSM-SBNUM,
*         SBPOS LIKE MDSM-SBPOS,
*       END OF IT_MDSM.

DATA : BEGIN OF IT_MARD_TEMP OCCURS 0,
*         werks LIKE marc-werks,
         MATNR LIKE MARD-MATNR,
         LABST LIKE MARD-LABST,
       END OF IT_MARD_TEMP.

DATA: BEGIN OF IT_DAY OCCURS 21,
        SEQ(2)  TYPE N,
        DATUM   LIKE   SY-DATUM,
      END   OF IT_DAY.

DATA: IT_DAY_NOWORK LIKE TABLE OF IT_DAY WITH HEADER LINE.

DATA: BEGIN OF IT_WEEK OCCURS 21,
        SEQ(2)  TYPE N,
        DATUM   LIKE   SY-DATUM,
      END   OF IT_WEEK.

DATA : Z_MAX_DATE LIKE SY-DATUM,
       Z_BEG_DATE LIKE SY-DATUM,
       W_MAX_DAY_CN(2) TYPE N,
       W_DATUM LIKE SY-DATUM,
       W_PRGRP LIKE PGMI-PRGRP.

DATA: OK_CODE      LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID,
      W_CNT       TYPE   I,
      W_NO_DATA(1),
      W_FT_NOWORK(1).

DATA:  L_KALID LIKE KAKO-KALID.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_TOT  TYPE LVC_T_FCAT WITH HEADER LINE,
*       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_FNAME_TOT    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_EXCLUDE      TYPE UI_FUNCTIONS,
       IT_EXCLUDE_TOT  TYPE UI_FUNCTIONS.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       WA_TOT_LAYOUT TYPE LVC_S_LAYO,
       W_FIELDNAME  LIKE LINE OF IT_FIELDCAT.


DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
      WA_TOT_SAVE  TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_TOT_VARIANT TYPE DISVARIANT.     "for parameter IS_VARIANT


DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL_TOT TYPE    SCRFNAME VALUE 'ALV_CONTAINER_TOT',
      ALV_GRID_TOT          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER_TOT    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

FIELD-SYMBOLS : <FS01>, <FS-QTY>.

**--- Ranges
RANGES : R_LGORT FOR MARD-LGORT,
         R_LGTYP FOR LQUA-LGTYP,
         R_PEDTR FOR PLAF-PEDTR,
         R_PEDTR3 FOR PLAF-PEDTR.

**--- Constants
** for E002
*CONSTANTS : C_WERKS LIKE MARC-WERKS VALUE 'E001'.

*            C_LGNUM LIKE T300-LGNUM VALUE 'P01',
*            C_PLSCN LIKE PLAF-PLSCN VALUE '900'.

DATA:  W_REFRESH(1),
       W_NEW(1) VALUE 'X'.
*DATA  SAVE_DATA.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA: G_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED.

    DATA: ERROR_IN_DATA TYPE C.

ENDCLASS.
DATA :IT_LVC  LIKE LVC_S_ROW.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_DATA_CHANGED.

    DATA: LS_GOOD TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          W_QTY(13),
*          W_QTY type p decimals 0,
          LVC_T_ROW TYPE LVC_T_ROW.

    ERROR_IN_DATA = SPACE.
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.
      CASE LS_GOOD-FIELDNAME.
* check if column Name1 of this row was changed
        WHEN 'QTYD_01' OR 'QTYD_02' OR 'QTYD_03' OR 'QTYD_04' OR
             'QTYD_05' OR 'QTYD_06' OR 'QTYD_07' OR 'QTYD_08' OR
             'QTYD_09' OR 'QTYD_10' OR 'QTYD_11' OR 'QTYD_12' OR
             'QTYD_13' OR 'QTYD_14' OR 'QTYD_15' OR 'QTYD_16' OR
             'QTYD_17' OR 'QTYD_18' OR 'QTYD_19' OR 'QTYD_20' OR
             'QTYD_21'.
          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
                     EXPORTING
                        I_ROW_ID  = LS_GOOD-ROW_ID
                        I_FIELDNAME = LS_GOOD-FIELDNAME
                     IMPORTING
                        E_VALUE =   LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
                  EXPORTING
                       I_ROW_ID = LS_GOOD-ROW_ID
                       I_FIELDNAME = LS_GOOD-FIELDNAME
                       I_VALUE     = LV_VALUE.
        WHEN 'QTYW_04' OR 'QTYW_05' OR 'QTYW_06' OR 'QTYW_07' OR
             'QTYW_08' OR 'QTYW_09' OR 'QTYW_10' OR 'QTYW_11' OR
             'QTYW_12' OR 'QTYW_13' OR 'QTYW_14' OR 'QTYW_15' OR
             'QTYW_16' OR 'QTYW_17' OR 'QTYW_18' OR 'QTYW_19' OR
             'QTYW_20' OR 'QTYW_21'.

          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
                     EXPORTING
                        I_ROW_ID  = LS_GOOD-ROW_ID
                        I_FIELDNAME = LS_GOOD-FIELDNAME
                     IMPORTING
                        E_VALUE =   LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
                  EXPORTING
                       I_ROW_ID = LS_GOOD-ROW_ID
                       I_FIELDNAME = LS_GOOD-FIELDNAME
                       I_VALUE     = LV_VALUE.


*
*          W_QTY = LV_VALUE.
*          IF W_QTY < 0 OR W_QTY > 9999999999.
*            CALL METHOD ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
*                        EXPORTING
*                              I_MSGID =  'SU'
*                              I_MSGNO =  '000'
*                              I_MSGTY =  'E'
*         I_MSGV1 = 'Please input correct PIR value'
*         I_MSGV2 = '(Value->)'
*         I_MSGV3 = LV_VALUE
*                             I_FIELDNAME = LS_GOOD-FIELDNAME
*                              I_ROW_ID  =  LS_GOOD-ROW_ID.
*          ELSE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*                    EXPORTING
*                         I_ROW_ID = LS_GOOD-ROW_ID
*                         I_FIELDNAME = LS_GOOD-FIELDNAME
*                         I_VALUE     = LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*                    EXPORTING
*                         I_ROW_ID = LS_GOOD-ROW_ID
*                         I_FIELDNAME = 'CHECK'
*                        I_VALUE     = 'X'.

*          ENDIF.
*CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.
*

      ENDCASE.
    ENDLOOP.

*§7.Display application log if an error has occured.
    IF ERROR_IN_DATA EQ 'X'.
      CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
PARAMETERS : R01 RADIOBUTTON GROUP RB1 DEFAULT 'X'.
PARAMETERS : R02 RADIOBUTTON GROUP RB1.
PARAMETERS : R03 RADIOBUTTON GROUP RB1.
parameters: w_werks like t001w-werks.
SELECTION-SCREEN END OF BLOCK BLOCK1.

*SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
**SELECTION-SCREEN SKIP.
*PARAMETERS : R11 RADIOBUTTON GROUP RB2 DEFAULT 'X'.
*PARAMETERS : R12 RADIOBUTTON GROUP RB2.
*
*SELECTION-SCREEN END OF BLOCK BLOCK2.

*AT SELECTION-SCREEN OUTPUT.

*AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM SET_DATA.
  PERFORM CLEAR_DATA.
  WHILE W_REFRESH = 'X' OR W_NEW = 'X'.
    PERFORM GET_DATA.
    IF W_NO_DATA = 'X'.
      CLEAR: W_NO_DATA.
      EXIT.
    ENDIF.
    PERFORM SELECT_EDIT_LINE.
    PERFORM DISPLAY_DATA.
  ENDWHILE.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  PERFORM GET_REQ_DATA.
  IF W_NO_DATA = 'X'.
    EXIT.
  ENDIF.
  PERFORM GET_PIR_DATA.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_REQ_DATA.
  DATA: IT_SEQ_SUM03 LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
        IT_SEQ_SUM02 LIKE TABLE OF ZTPP_SEQ_SUM02 WITH HEADER LINE.

  DATA: LT_PGMI LIKE TABLE OF PGMI WITH HEADER LINE.
  DATA: L_LINE TYPE I,
        L_CN(2) TYPE N,
        L_TEXT(30).

  CLEAR : IT_TAB, IT_TAB[], IT_DAY, IT_DAY[], IT_WEEK, IT_WEEK[].

* reading working calendar
  SELECT SINGLE KALID INTO L_KALID
    FROM ZVPP_CAPACITY
   WHERE ARBPL = 'T'   .

  PERFORM READ_SHOP_CALID  USING L_KALID.

  PERFORM SET_DAYS.
  PERFORM SET_WEEKS.

*  CLEAR: IT_MARD_TEMP, IT_MARD_TEMP[], IT_MARD, IT_MARD[].

  SELECT * INTO TABLE LT_PGMI
   FROM PGMI
   WHERE PRGRP = W_PRGRP
** for E002
*     AND WERKS = C_WERKS.
     AND WERKS = w_WERKS.

  LOOP AT LT_PGMI.
    IT_TAB_TEMP-MATNR = LT_PGMI-NRMIT.
    IT_TAB_TEMP-ALC_VALS = LT_PGMI-NRMIT.
*    IT_TAB_TEMP-WERKS = C_WERKS.
    IT_TAB_TEMP-WERKS = w_WERKS.

    APPEND IT_TAB_TEMP.
  ENDLOOP.

  REFRESH LT_PGMI.

  IF IT_TAB_TEMP[] IS INITIAL.
    W_NO_DATA = 'X'.
    MESSAGE I000(ZZ) WITH  'No data found'.
    EXIT.
  ELSE.

    SELECT MATNR SUM01 INTO TABLE IT_STOCK
       FROM MDKP
       FOR ALL ENTRIES IN IT_TAB_TEMP
       WHERE MATNR = IT_TAB_TEMP-MATNR
         AND PLWRK = IT_TAB_TEMP-WERKS
         AND DTART = 'MD'
         AND PLSCN = '   '.
*         AND DSDAT = Z_BEG_DATE.

*    L_LINE = 1.
    LOOP AT IT_TAB_TEMP.
      IT_TAB-MATNR = IT_TAB_TEMP-MATNR.
      IT_TAB-MATNR_S = IT_TAB_TEMP-MATNR.
      IT_TAB-DESC = 'Veh Req'.
      SELECT SINGLE MAKTX INTO IT_TAB-MAKTX
      FROM MAKT
      WHERE MATNR = IT_TAB_TEMP-MATNR.

      READ TABLE IT_STOCK WITH KEY MATNR = IT_TAB_TEMP-MATNR.
      IF SY-SUBRC = 0.
        IT_TAB-LABST = IT_STOCK-SUM01.
      ENDIF.
*      IF L_LINE = 1.
*        IT_TAB-IF = 'C210'.
*      ENDIF.
      APPEND IT_TAB.
      CLEAR: IT_TAB_TEMP, IT_TAB.
*      IF L_LINE = 0.
*        L_LINE = 1.
*      ELSE.
*        L_LINE = 0.
*      ENDIF.
    ENDLOOP.

    SELECT * INTO TABLE IT_SEQ_SUM02
     FROM ZTPP_SEQ_SUM02
     FOR ALL ENTRIES IN IT_TAB_TEMP
     WHERE ALC_VALS = IT_TAB_TEMP-ALC_VALS.

    SELECT * INTO TABLE IT_SEQ_SUM03
      FROM ZTPP_SEQ_SUM03
      FOR ALL ENTRIES IN IT_TAB_TEMP
      WHERE ALC_VALS = IT_TAB_TEMP-ALC_VALS.

    LOOP AT IT_TAB.
*     L_CN = '00'.
*      L_DATE = SY-DATUM.
      READ TABLE IT_SEQ_SUM02 WITH KEY ALC_VALS = IT_TAB-MATNR.
      IF SY-SUBRC = 0.
        LOOP AT IT_DAY.
          CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS-QTY>.
          CONCATENATE 'IT_SEQ_SUM02-D' IT_DAY-SEQ INTO  L_TEXT.
          ASSIGN (L_TEXT) TO <FS01>.
          <FS-QTY> = <FS01>.
        ENDLOOP.
      ENDIF.
*        DO 21 TIMES.
*          L_CN = L_CN + 1.
*          CONCATENATE 'IT_TAB-QTYD_' L_CN INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS-QTY>.
*          CONCATENATE 'IT_SEQ_SUM02-D' L_CN INTO  L_TEXT.
*          ASSIGN (L_TEXT) TO <FS01>.
*          <FS-QTY> = <FS01>.
*        ENDDO.
*        IT_TAB-TOTAL = IT_TAB-QTYD_01 + IT_TAB-QTYD_02
*                  + IT_TAB-QTYD_03 + IT_TAB-QTYD_04 + IT_TAB-QTYD_05
*                  + IT_TAB-QTYD_06 + IT_TAB-QTYD_07 + IT_TAB-QTYD_08
*                  + IT_TAB-QTYD_09 + IT_TAB-QTYD_10 + IT_TAB-QTYD_11
*                  + IT_TAB-QTYD_12 + IT_TAB-QTYD_13 + IT_TAB-QTYD_14
*                  + IT_TAB-QTYD_15 + IT_TAB-QTYD_16 + IT_TAB-QTYD_17
*                  + IT_TAB-QTYD_18 + IT_TAB-QTYD_19 + IT_TAB-QTYD_20
*                  + IT_TAB-QTYD_21.


      READ TABLE IT_SEQ_SUM03 WITH KEY ALC_VALS = IT_TAB-MATNR.
      IF SY-SUBRC = 0.
        LOOP AT IT_WEEK.
          CONCATENATE 'IT_TAB-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS-QTY>.
          CONCATENATE 'IT_SEQ_SUM03-W' IT_WEEK-SEQ INTO  L_TEXT.
          ASSIGN (L_TEXT) TO <FS01>.
          <FS-QTY> = <FS01>.
        ENDLOOP.
      ENDIF.
*        L_CN = '03'.
*        DO 18 TIMES.
*          L_CN = L_CN + 1.
*          CONCATENATE 'IT_TAB-QTYW_' L_CN INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS-QTY>.
*          CONCATENATE 'IT_SEQ_SUM03-D' L_CN INTO  L_TEXT.
*          ASSIGN (L_TEXT) TO <FS01>.
*          <FS-QTY> = <FS01>.
*        ENDDO.
*        IT_TAB-TOTAL = IT_TAB-TOTAL + IT_TAB-QTYW_04 + IT_TAB-QTYW_05
*                  + IT_TAB-QTYW_06 + IT_TAB-QTYW_07 + IT_TAB-QTYW_08
*                  + IT_TAB-QTYW_09 + IT_TAB-QTYW_10 + IT_TAB-QTYW_11
*                  + IT_TAB-QTYW_12 + IT_TAB-QTYW_13 + IT_TAB-QTYW_14
*                  + IT_TAB-QTYW_15 + IT_TAB-QTYW_16 + IT_TAB-QTYW_17
*                  + IT_TAB-QTYW_18 + IT_TAB-QTYW_19 + IT_TAB-QTYW_20
*                  + IT_TAB-QTYW_21.
      IT_TAB-SEQ = '1'.
      MODIFY IT_TAB.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_req_data

*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SCREEN.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EXCEL'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*---------------------------------------------------------------------*
*       FORM set_days                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_DAYS.
  DATA: L_COUNT TYPE I.
  DATA: L_DATE LIKE SY-DATUM.

  SELECT MAX( SQDT ) INTO Z_MAX_DATE
    FROM ZTPP_PMT07JB_A
     WHERE GUBB = 'A'.


** Chnaged by Furong on 05/04/09

  L_DATE = Z_BEG_DATE.
  PERFORM READ_WORKING_DATE USING '+' L_KALID  L_DATE.
  IF L_DATE = Z_BEG_DATE.
    IT_DAY-SEQ = 1.
    IT_DAY-DATUM = Z_BEG_DATE.
    APPEND IT_DAY.
    CLEAR: W_FT_NOWORK.
  ELSE.
    W_FT_NOWORK = 'X'.
  ENDIF.
** ENd of change
  L_COUNT = '01'.
  L_DATE = Z_BEG_DATE.

  WHILE L_DATE < Z_MAX_DATE.
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+' L_KALID  L_DATE.
    IT_DAY-SEQ     = L_COUNT.
    IT_DAY-DATUM   = L_DATE .
    APPEND IT_DAY.  CLEAR: IT_DAY.
  ENDWHILE.
  IF W_MAX_DAY_CN > 1.
    W_MAX_DAY_CN = L_COUNT - 1.
  ELSE.
    W_MAX_DAY_CN = L_COUNT.
  ENDIF.
*  DO 20 TIMES.
*    L_COUNT  = L_COUNT + 1.
*    L_DATE   = L_DATE  + 1.
*    PERFORM READ_WORKING_DATE USING '+' L_KALID  L_DATE.
*    IT_DAY-SEQ     = L_COUNT.
*    IT_DAY-DATUM   = L_DATE .
*    APPEND IT_DAY.  CLEAR: IT_DAY.
*  ENDDO.

ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING P_L_KALID.
  SELECT SINGLE KALID INTO P_L_KALID
  FROM ZVPP_CAPACITY
 WHERE ARBPL = 'T'   .
ENDFORM.                    " read_shop_calid
*---------------------------------------------------------------------*
*       FORM read_working_date                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_TYPE                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WDATE                                                      *
*---------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = PA_TYPE
            DATE                         = PA_WDATE
            FACTORY_CALENDAR_ID          = PA_KALID
       IMPORTING
            DATE                         = PA_WDATE
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  PERFORM PREPARE_DISPLAY.
  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  get_lips_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PIR_DATA.
  DATA: IT_ENG_PIR LIKE TABLE OF ZTPP_ENG_PIR WITH HEADER LINE.
  DATA: L_MATNR LIKE MARA-MATNR,
        L_CN(2) TYPE N,
        L_TEXT(30).
  DATA: WA_TAB1 LIKE IT_TAB,
        WA_TAB3 LIKE IT_TAB.

  FIELD-SYMBOLS : <L_STOCK>, <L_PIR>, <L_REQ>.

  SELECT * INTO TABLE IT_ENG_PIR
   FROM ZTPP_ENG_PIR
   FOR ALL ENTRIES IN IT_TAB_TEMP
   WHERE MATNR  = IT_TAB_TEMP-MATNR
     AND WDATU = W_DATUM.

  LOOP AT IT_TAB_TEMP.
    READ TABLE IT_TAB WITH KEY MATNR = IT_TAB_TEMP-MATNR.
    WA_TAB1 = IT_TAB.
    CLEAR: IT_TAB.
    LOOP AT IT_DAY.
      READ TABLE IT_ENG_PIR WITH KEY MATNR = IT_TAB_TEMP-MATNR
                                     PDATU = IT_DAY-DATUM
                                     ENTLU = '1'.
      IF SY-SUBRC = 0.
        CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          <FS01> = IT_ENG_PIR-PLNMG.
        ENDIF.
      ENDIF.
    ENDLOOP.
    LOOP AT IT_WEEK.
      READ TABLE IT_ENG_PIR WITH KEY MATNR = IT_TAB_TEMP-MATNR
                                     PDATU = IT_WEEK-DATUM
                                     ENTLU = '2'.
      IF SY-SUBRC = 0.
        CONCATENATE 'IT_TAB-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          <FS01> = IT_ENG_PIR-PLNMG.
        ENDIF.
      ENDIF.
    ENDLOOP.

*    LOOP AT IT_ENG_PIR WHERE MATNR = IT_TAB_TEMP-MATNR.
*      IF IT_ENG_PIR-ENTLU = '1'.
*        READ TABLE IT_DAY WITH KEY DATUM = IT_ENG_PIR-PDATU.
*        IF SY-SUBRC = 0.
*          CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS01>.
*          IF SY-SUBRC = 0.
*            <FS01> = <FS01> + IT_ENG_PIR-PLNMG.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        READ TABLE IT_WEEK WITH KEY DATUM = IT_ENG_PIR-PDATU.
*        IF SY-SUBRC = 0.
*          CONCATENATE 'IT_TAB-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS01>.
*          IF SY-SUBRC = 0.
*            <FS01> = <FS01> + IT_ENG_PIR-PLNMG.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
    IT_TAB-MATNR = IT_TAB_TEMP-MATNR.
    IT_TAB-SEQ = '3'.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
*      IT_TAB-IF = 'C210'.
*    ENDIF.
    IT_TAB-DESC = 'Plan'.
    WA_TAB3 = IT_TAB.
    APPEND IT_TAB.
    CLEAR: IT_TAB.

    IT_TAB-SEQ = '2'.
    IT_TAB-MATNR = IT_TAB_TEMP-MATNR.
    IT_TAB-QTYD_01 = WA_TAB1-LABST.

    LOOP AT IT_DAY FROM 2 .
      L_CN = IT_DAY-SEQ - 1.
      CONCATENATE 'WA_TAB1-QTYD_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <L_REQ>.
      CONCATENATE 'IT_TAB-QTYD_'  L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <L_STOCK>.
      CONCATENATE 'WA_TAB3-QTYD_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <L_PIR>.

      CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS-QTY>.
      <FS-QTY> = <L_STOCK> + <L_PIR> - <L_REQ>.
    ENDLOOP.

    CONCATENATE 'WA_TAB1-QTYD_' W_MAX_DAY_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <L_REQ>.
    CONCATENATE 'IT_TAB-QTYD_'  W_MAX_DAY_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <L_STOCK>.
    CONCATENATE 'WA_TAB3-QTYD_' W_MAX_DAY_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <L_PIR>.

    IT_TAB-QTYW_04 = <L_STOCK> + <L_PIR> - <L_REQ>.

    LOOP AT IT_WEEK FROM 2.
      L_CN = IT_WEEK-SEQ - 1.
      CONCATENATE 'WA_TAB1-QTYW_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <L_REQ>.
      CONCATENATE 'IT_TAB-QTYW_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <L_STOCK>.
      CONCATENATE 'WA_TAB3-QTYW_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <L_PIR>.

      CONCATENATE 'IT_TAB-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS-QTY>.
      <FS-QTY> = <L_STOCK> + <L_PIR> - <L_REQ>.
    ENDLOOP.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
*      IT_TAB-IF = 'C210'.
*    ENDIF.
    IT_TAB-DESC = 'Stock'.
    APPEND IT_TAB.
    CLEAR: IT_TAB.
  ENDLOOP.

  SORT IT_TAB BY MATNR SEQ.

ENDFORM.                    " get_lips_data

INCLUDE ZAPP_ENG_PRDT_PLAN_PBO.
*INCLUDE ZRMM_REQUIREMENT_PLAN_PBO.

INCLUDE ZAPP_ENG_PRDT_PLAN_PAI.
*INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO.
*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LINE  text
*      -->P_1971   text
*----------------------------------------------------------------------*
*FORM BUILD_COLOR_ALL USING p_line p_fname.
*  if p_line  = 1.
*    wa_color-color-col = 6.
*    wa_color-color-int = 1.
*    wa_color-fname = p_fname.
*    append wa_color to it_color.
*    clear wa_color.
*  endif.
*endform.                    " BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*&      Form  SET_WEEKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_WEEKS.
  DATA: BEGIN OF LT_WEEK OCCURS 0,
        SQDT LIKE SY-DATUM,
        END OF LT_WEEK.
  DATA: L_CN(2) TYPE N.

  SELECT SQDT INTO TABLE LT_WEEK
   FROM ZTPP_PMT07JB_A
    WHERE GUBB EQ 'B'.

  SORT LT_WEEK BY SQDT.
  DELETE ADJACENT DUPLICATES FROM LT_WEEK COMPARING SQDT.

  L_CN = '04'.
  LOOP AT LT_WEEK.
    IT_WEEK-SEQ = L_CN.
    IT_WEEK-DATUM = LT_WEEK-SQDT.
    APPEND IT_WEEK.
    L_CN =  L_CN + 1.
  ENDLOOP.

*  DATA: L_DATE LIKE SY-DATUM,
*        L_DATE_1 LIKE SY-DATUM,
*        L_CN(2) TYPE N.
*
*  CLEAR: IT_WEEK, IT_WEEK[].
*  L_DATE = SY-DATUM.
*  CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
*       EXPORTING
*            P_PDATE  = L_DATE
*       IMPORTING
*            P_SUNDAY = L_DATE.
*  L_DATE = L_DATE + 1.                                      "03/13
*  L_CN = '01'.
*  DO 21 TIMES.
*    IT_WEEK-SEQ = L_CN.
*    L_DATE_1 = L_DATE.
*    PERFORM READ_WORKING_DATE USING '+'  L_KALID  L_DATE_1.
*    L_DATE = L_DATE + 7.
*    IF L_DATE_1 > L_DATE.
*      L_DATE = L_DATE + 7.
*    ELSE.
*      IF L_DATE = L_DATE_1.
*        L_DATE_1 = L_DATE_1 - 7.
*      ENDIF.
*    ENDIF.
*    IT_WEEK-DATUM = L_DATE_1.
*    APPEND IT_WEEK.
*    L_CN = L_CN + 1.
*  ENDDO.
ENDFORM.                    " SET_WEEKS
*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_EDIT_LINE.
  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        W_CELLTAB TYPE LVC_S_STYL,
        L_INDEX TYPE I,
        L_MODE TYPE RAW4.

  LOOP AT IT_TAB.
    L_INDEX = SY-TABIX.
    REFRESH LT_CELLTAB.
    IF IT_TAB-SEQ = '3'.
      L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    W_CELLTAB-FIELDNAME = 'MATNR_S'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'MAKTX'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'DESC'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'QTYD_01'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_02'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_03'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_04'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_05'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_06'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_07'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_08'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_09'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_10'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_11'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_12'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_13'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_14'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_15'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_16'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_17'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_18'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_19'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_20'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYD_21'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'QTYW_04'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_05'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_06'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_07'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_08'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_09'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_10'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_11'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_12'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_13'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_14'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_15'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_16'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_17'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_18'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_19'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_20'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    W_CELLTAB-FIELDNAME = 'QTYW_21'.
    W_CELLTAB-STYLE = L_MODE.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    INSERT LINES OF LT_CELLTAB INTO TABLE IT_TAB-CELLTAB.
    MODIFY IT_TAB INDEX L_INDEX.
  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_DISPLAY.
  DATA: L_QTY TYPE I,
        L_CN(2) TYPE N,
        L_QTY1(13), " LIKE IT_TAB-QTYD_01.
        L_TEXT(35).

  CLEAR: IT_OUTPUT, IT_OUTPUT[], IT_TOTAL, IT_TOTAL[],
         WA_TOT1, WA_TOT2, WA_TOT3.
  LOOP AT IT_TAB.
    MOVE-CORRESPONDING IT_TAB TO IT_OUTPUT.
    L_CN = '00'.
    DO 21 TIMES.
      L_CN =  L_CN + 1.
      CONCATENATE 'IT_TAB-QTYD_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS01>.
      IF SY-SUBRC = 0.
        L_QTY = <FS01>.
        L_QTY1 = L_QTY.
** total
        CASE IT_TAB-SEQ.
          WHEN '1'.
            CONCATENATE 'WA_TOT1-QTYD_' L_CN INTO L_TEXT.
            ASSIGN (L_TEXT) TO <FS01>.
            IF SY-SUBRC = 0.
              <FS01> = <FS01> + L_QTY.
            ENDIF.
          WHEN '2'.
            CONCATENATE 'WA_TOT2-QTYD_' L_CN INTO L_TEXT.
            ASSIGN (L_TEXT) TO <FS01>.
            IF SY-SUBRC = 0.
              <FS01> = <FS01> + L_QTY.
            ENDIF.
          WHEN '3'.
            CONCATENATE 'WA_TOT3-QTYD_' L_CN INTO L_TEXT.
            ASSIGN (L_TEXT) TO <FS01>.
            IF SY-SUBRC = 0.
              <FS01> = <FS01> + L_QTY.
            ENDIF.
        ENDCASE.
**
        CONCATENATE 'IT_OUTPUT-QTYD_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          <FS-QTY> = L_QTY1.
        ENDIF.
      ENDIF.
    ENDDO.
    L_CN = '03'.
    DO 18 TIMES.
      L_CN =  L_CN + 1.
      CONCATENATE 'IT_TAB-QTYW_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS01>.
      IF SY-SUBRC = 0.
        L_QTY = <FS01>.
        L_QTY1 = L_QTY.
** total
        CONCATENATE 'IT_TOTAL-QTYW_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          <FS01> = <FS01> + L_QTY.
        ENDIF.
**
      ENDIF.
      CONCATENATE 'IT_OUTPUT-QTYW_' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS-QTY>.
      IF SY-SUBRC = 0.
        <FS-QTY> = L_QTY1.
      ENDIF.
    ENDDO.
    APPEND IT_OUTPUT.
  ENDLOOP.
  WA_TOT1-MATNR_S = 'Total'.
  WA_TOT1-DESC = 'Veh Req'.
  WA_TOT1-IF = 'C210'.
  MOVE-CORRESPONDING WA_TOT1 TO IT_TOTAL.
  APPEND IT_TOTAL.
  WA_TOT2-DESC = 'Stock'.
  WA_TOT2-IF = 'C210'.
  MOVE-CORRESPONDING WA_TOT2 TO IT_TOTAL.
  APPEND IT_TOTAL.
  WA_TOT3-DESC = 'Plan'.
  WA_TOT3-IF = 'C210'.
  MOVE-CORRESPONDING WA_TOT3 TO IT_TOTAL.
  APPEND IT_TOTAL.
ENDFORM.                    " PREPARE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA.
  REFRESH: IT_TAB_TEMP, IT_TAB, IT_OUTPUT, IT_STOCK.
  CLEAR: IT_TAB_TEMP, IT_TAB, IT_OUTPUT, IT_STOCK.
ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATA.
  DATA: C_JOBS(40) VALUE 'ZAPP903R_INPUT_PLAN',
      C_KEY1(18) VALUE 'TRIM_INPUT'.

  IF R01 = 'X'.
    W_PRGRP = 'MIP-ENG'.
  ELSEIF R02 = 'X'.
    W_PRGRP = 'KD-ENG'.
  ELSE.
    W_PRGRP = 'MTP-3C'.
  ENDIF.
*  IF R11 = 'X'.
*    W_DATUM = SY-DATUM.
*  ELSE.

  SELECT SINGLE DATES INTO Z_BEG_DATE
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = C_JOBS
     AND KEY2 = C_KEY1.

*  PERFORM READ_WORKING_DATE USING '+' L_KALID  Z_BEG_DATE.
*  Z_BEG_DATE = ztpp_common_vals-dates.

  SELECT MAX( WDATU ) INTO W_DATUM
  FROM ZTPP_ENG_PIR.
*  ENDIF.
ENDFORM.                    " SET_DATA
