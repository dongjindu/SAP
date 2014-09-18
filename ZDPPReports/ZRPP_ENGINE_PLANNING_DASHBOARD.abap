************************************************************************
* Author                 : Furong Wang
* Creation Date          : 10/14/09
* Development Request No :
* Addl documentation     :
* Description            :
* Modification Log
* Date       Developer    Request ID Description
* Description            :
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************

REPORT ZRPP_ENGINE_PLANNING_DASHBOARD MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS.

DATA: BEGIN OF IT_JITCALL OCCURS 0,
      MATNR LIKE JITMA-MATNR,
      DATUM LIKE SY-DATUM,
      QUANT TYPE I,
      END OF IT_JITCALL.

*DATA: BEGIN OF IT_JIT_OUTPUT OCCURS 0,
*      MATNR LIKE JITMA-MATNR,
*      QTYD_01(13),
*      QTYD_02(13),
*      QTYD_03(13),
*      QTYD_04(13),
*      QTYD_05(13),
*      QTYD_06(13),
*      QTYD_07(13),
*      QTYD_08(13),
*      QTYD_09(13),
*      QTYD_10(13),
*      QTYD_11(13),
*      QTYD_12(13),
*      QTYD_13(13),
*      QTYD_14(13),
*      QTYD_15(13),
*      QTYD_16(13),
*      QTYD_17(13),
*      QTYD_18(13),
*      QTYD_19(13),
*      QTYD_20(13),
*      QTYD_21(13),
*      QTYW_04(13),
*      QTYW_05(13),
*      QTYW_06(13),
*      QTYW_07(13),
*      QTYW_08(13),
*      QTYW_09(13),
*      QTYW_10(13),
*      QTYW_11(13),
*      QTYW_12(13),
*      QTYW_13(13),
*      QTYW_14(13),
*      QTYW_15(13),
*      QTYW_16(13),
*      QTYW_17(13),
*      QTYW_18(13),
*      QTYW_19(13),
*      QTYW_20(13),
*      QTYW_21(13),
**      TOTAL LIKE MDSM-BDMNG,
**      MAKTX LIKE MAKT-MAKTX,
*      IF(4) TYPE C,
*      CELLTAB TYPE LVC_T_STYL,
*     END OF IT_JIT_OUTPUT.

DATA: BEGIN OF IT_JIT_OUTPUT OCCURS 0,
      MATNR LIKE JITMA-MATNR,
      QTYD_01 TYPE I,
      QTYD_02 TYPE I,
      QTYD_03 TYPE I,
      QTYD_04 TYPE I,
      QTYD_05 TYPE I,
      QTYD_06 TYPE I,
      QTYD_07 TYPE I,
      QTYD_08 TYPE I,
      QTYD_09 TYPE I,
      QTYD_10 TYPE I,
      QTYD_11 TYPE I,
      QTYD_12 TYPE I,
      QTYD_13 TYPE I,
      QTYD_14 TYPE I,
      QTYD_15 TYPE I,
      QTYD_16 TYPE I,
      QTYD_17 TYPE I,
      QTYD_18 TYPE I,
      QTYD_19 TYPE I,
      QTYD_20 TYPE I,
      QTYD_21 TYPE I,
      QTYD_22 TYPE I,
      QTYD_23 TYPE I,
      QTYD_24 TYPE I,
      QTYD_25 TYPE I,
      QTYD_26 TYPE I,
      QTYD_27 TYPE I,
      QTYD_28 TYPE I,
      QTYD_29 TYPE I,
      QTYD_30 TYPE I,
*      TOTAL LIKE MDSM-BDMNG,
*      MAKTX LIKE MAKT-MAKTX,
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
     END OF IT_JIT_OUTPUT.

DATA: BEGIN OF IT_DEL_OUTPUT OCCURS 0,
      MATNR LIKE JITMA-MATNR,
      QTYD_01 TYPE I,
      QTYD_02 TYPE I,
      QTYD_03 TYPE I,
      QTYD_04 TYPE I,
      QTYD_05 TYPE I,
      QTYD_06 TYPE I,
      QTYD_07 TYPE I,
      QTYD_08 TYPE I,
      QTYD_09 TYPE I,
      QTYD_10 TYPE I,
      QTYD_11 TYPE I,
      QTYD_12 TYPE I,
      QTYD_13 TYPE I,
      QTYD_14 TYPE I,
      QTYD_15 TYPE I,
      QTYD_16 TYPE I,
      QTYD_17 TYPE I,
      QTYD_18 TYPE I,
      QTYD_19 TYPE I,
      QTYD_20 TYPE I,
      QTYD_21 TYPE I,
      QTYD_22 TYPE I,
      QTYD_23 TYPE I,
      QTYD_24 TYPE I,
      QTYD_25 TYPE I,
      QTYD_26 TYPE I,
      QTYD_27 TYPE I,
      QTYD_28 TYPE I,
      QTYD_29 TYPE I,
      QTYD_30 TYPE I,

*      TOTAL LIKE MDSM-BDMNG,
*      MAKTX LIKE MAKT-MAKTX,
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
     END OF IT_DEL_OUTPUT.

DATA: BEGIN OF IT_FC_OUTPUT OCCURS 0,
      MATNR LIKE JITMA-MATNR,
*      QTYD_01 TYPE I,
*      QTYD_02 TYPE I,
*      QTYD_03 TYPE I,
*      QTYD_04 TYPE I,
*      QTYD_05 TYPE I,
*      QTYD_06 TYPE I,
*      QTYD_07 TYPE I,
*      QTYD_08 TYPE I,
*      QTYD_09 TYPE I,
*      QTYD_10 TYPE I,
*      QTYD_11 TYPE I,
*      QTYD_12 TYPE I,
*      QTYD_13 TYPE I,
*      QTYD_14 TYPE I,
*      QTYD_15 TYPE I,
*      QTYD_16 TYPE I,
*      QTYD_17 TYPE I,
*      QTYD_18 TYPE I,
*      QTYD_19 TYPE I,
*      QTYD_20 TYPE I,
*      QTYD_21 TYPE I,
      QTYW_01 TYPE I,
      QTYW_02 TYPE I,
      QTYW_03 TYPE I,
      QTYW_04 TYPE I,
      QTYW_05 TYPE I,
      QTYW_06 TYPE I,
      QTYW_07 TYPE I,
      QTYW_08 TYPE I,
      QTYW_09 TYPE I,
      QTYW_10 TYPE I,
      QTYW_11 TYPE I,
      QTYW_12 TYPE I,
      QTYW_13 TYPE I,
      QTYW_14 TYPE I,
      QTYW_15 TYPE I,
      QTYW_16 TYPE I,
      QTYW_17 TYPE I,
      QTYW_18 TYPE I,
      QTYW_19 TYPE I,
      QTYW_20 TYPE I,
      QTYW_21 TYPE I,
      QTYW_22 TYPE I,
      QTYW_23 TYPE I,
      QTYW_24 TYPE I,
      QTYW_25 TYPE I,
      QTYW_26 TYPE I,
      QTYW_27 TYPE I,
      QTYW_28 TYPE I,
      QTYW_29 TYPE I,
      QTYW_30 TYPE I,

*      TOTAL LIKE MDSM-BDMNG,
*      MAKTX LIKE MAKT-MAKTX,
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
     END OF IT_FC_OUTPUT.

DATA: BEGIN OF IT_DAY OCCURS 35,
      SEQ(2) TYPE N,
      DATUM LIKE SY-DATUM,
      END OF IT_DAY.

DATA: BEGIN OF IT_DAY_DEL OCCURS 35,
      SEQ(2) TYPE N,
      DATUM LIKE SY-DATUM,
      END OF IT_DAY_DEL.

DATA: BEGIN OF IT_WEEK OCCURS 35,
      SEQ(2) TYPE N,
      DATUM LIKE SY-DATUM,
      END OF IT_WEEK.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT TYPE I.

DATA: WA_STBL TYPE LVC_S_STBL.
DATA : IT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_DAY TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_WEEK TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME TYPE SLIS_T_FIELDCAT_ALV,
       IT_FNAME_DAY TYPE SLIS_T_FIELDCAT_ALV,
       IT_FNAME_WEEK TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT TYPE LVC_T_SORT WITH HEADER LINE,
       IT_EXCLUDE TYPE UI_FUNCTIONS,
       IT_EXCLUDE_DAY TYPE UI_FUNCTIONS,
       IT_EXCLUDE_WEEK TYPE UI_FUNCTIONS.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       WA_DAY_LAYOUT TYPE LVC_S_LAYO,
       WA_WEEK_LAYOUT TYPE LVC_S_LAYO,
       W_FIELDNAME  LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE TYPE C VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
      WA_DAY_SAVE TYPE C VALUE 'A',   "for Parameter I_SAVE
      WA_DAY_VARIANT TYPE DISVARIANT,     "for parameter IS_VARIANT
      WA_WEEK_SAVE TYPE C VALUE 'A',   "for Parameter I_SAVE
      WA_WEEK_VARIANT TYPE DISVARIANT.     "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL_DAY TYPE SCRFNAME VALUE 'ALV_CONTAINER_DAY',
      ALV_GRID_DAY TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER_DAY TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL_WEEK TYPE SCRFNAME VALUE 'ALV_CONTAINER_WEEK',
      ALV_GRID_WEEK TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER_WEEK TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

FIELD-SYMBOLS : <FS01>, <FS-QTY>.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_VBELN LIKE JITMA-VBELN OBLIGATORY,
            P_CRDATE LIKE SY-DATUM DEFAULT SY-DATUM,
            P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM.

SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.

  SUBMIT ZAPP_ENGINE_PLANNING_DASHBOARD WITH P_VBELN = P_VBELN
    AND RETURN.
  WAIT UP TO 2 SECONDS.

  PERFORM GET_DATA_JIT.
  PERFORM GET_DATA_DEL.
  PERFORM GET_DATA_FORECAST.

  CALL SCREEN 200.


*---------------------------------------------------------------------*
*       FORM GET_JIT_CALL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_JIT_CALL.
  DATA: BEGIN OF LT_TEMP OCCURS 0,
       MATNR LIKE JITMA-MATNR,
       EXDAT LIKE JITIT-EXDAT,
       QUANT LIKE JITCO-QUANT,
       END OF LT_TEMP.
  DATA: L_DATE_C(8),
        L_CHAR14(14).

*  SELECT MATNR EXDAT QUANT INTO TABLE LT_TEMP
  SELECT MATNR RDATE AS EXDAT QUANT INTO TABLE LT_TEMP
   FROM JITMA AS A
  INNER JOIN JITCO AS B
  ON A~MATID = B~MATID
   INNER JOIN JITIT AS C
   ON B~POSID = C~POSID
   WHERE VBELN = P_VBELN
     AND C~INTST = '0000'.

  LOOP AT LT_TEMP.
    IT_JITCALL-MATNR = LT_TEMP-MATNR.
    IT_JITCALL-QUANT = LT_TEMP-QUANT.
    L_CHAR14 = LT_TEMP-EXDAT.
    L_DATE_C = L_CHAR14+0(8).
    IT_JITCALL-DATUM = L_DATE_C .
    COLLECT IT_JITCALL.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  set_day_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DAY_JIT.
  DATA: L_COUNT TYPE I.

  LOOP AT IT_JITCALL.
    IT_DAY-DATUM = IT_JITCALL-DATUM.
    COLLECT IT_DAY.
  ENDLOOP.
  SORT IT_DAY BY DATUM.
  L_COUNT = '01'.
  LOOP AT IT_DAY.
    IT_DAY-SEQ = L_COUNT.
    MODIFY IT_DAY.
    L_COUNT = L_COUNT + 1.
  ENDLOOP.

ENDFORM.                    " set_day_week
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_200 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_JIT_OUTPUT'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    WA_STBL-ROW = 'X'.
    WA_STBL-COL = 'X'.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING IS_STABLE = WA_STBL.
  ENDIF.
  IF  GRID_CONTAINER_DAY IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT_DAY.
    PERFORM SET_ATTRIBUTES_ALV_GRID_DAY.
*    PERFORM EXCLUDE_TB_FUNCTIONS_DAY.
    PERFORM BUILD_FIELD_CATALOG_DAY USING 'IT_DEL_OUTPUT'.
    PERFORM ASSIGN_ITAB_TO_ALV_DAY.
  ELSE.
    WA_STBL-ROW = 'X'.
    CALL METHOD ALV_GRID_DAY->REFRESH_TABLE_DISPLAY
     EXPORTING IS_STABLE = WA_STBL.
  ENDIF.
  IF  GRID_CONTAINER_WEEK IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT_WEEK.
    PERFORM SET_ATTRIBUTES_ALV_GRID_WEEK.
*    PERFORM EXCLUDE_TB_FUNCTIONS_DAY.
    PERFORM BUILD_FIELD_CATALOG_WEEK USING 'IT_FC_OUTPUT'.
    PERFORM ASSIGN_ITAB_TO_ALV_WEEK.
  ELSE.
    WA_STBL-ROW = 'X'.
    CALL METHOD ALV_GRID_DAY->REFRESH_TABLE_DISPLAY
     EXPORTING IS_STABLE = WA_STBL.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  IF SY-TCODE = 'ZAPP_ENG_PIR'.
    WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  ELSE.
    WA_IS_LAYOUT-EDIT       = ' '.
  ENDIF.
  WA_IS_LAYOUT-GRID_TITLE = 'JIT Call'.
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = ' '.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '8'.


  LOOP AT IT_DAY.
    CONCATENATE 'QTYD_' IT_DAY-SEQ INTO L_RQTY.
    WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.

    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_RQTY        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                   ' ' 'COLTEXT'     L_DATUM(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '5'.
    CLEAR: L_RQTY.
  ENDLOOP.

*  LOOP AT IT_WEEK.
*    CONCATENATE 'QTYW_' IT_WEEK-SEQ INTO L_RQTY.
*    WRITE IT_WEEK-DATUM TO L_DATUM MM/DD/YY.
*
*    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
*
*                                   'S' L_RQTY        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
*                                   ' ' 'COLTEXT'     L_DATUM(5),
*                                   ' ' 'DECIMALS_O'  '0',
*                                   'E' 'OUTPUTLEN'   '5'.
*    CLEAR: L_RQTY.
*  ENDLOOP.
ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).
  FIELD-SYMBOLS <FS>.

  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.
    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.
    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
*               i_default        = space
               IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_JIT_OUTPUT[].
*               it_sort          = it_sort[].

** ENTER
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
                EXPORTING
                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.


ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS.
  DATA LS_EXCLUDE TYPE UI_FUNC.

* Row manipulation
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.

*  Sort buttons
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS


*** ALV for Dailly Schedule

*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT_DAY.
  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER_DAY
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL_DAY
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT ALV_GRID_DAY
         EXPORTING I_PARENT = GRID_CONTAINER_DAY
                   I_APPL_EVENTS = 'X'.


ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_DAY
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_DAY.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_DAY_LAYOUT, WA_DAY_VARIANT.

*//-- Set Layout Structure
  WA_DAY_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_DAY_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_DAY_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_DAY_LAYOUT-GRID_TITLE = 'KMMG Daily Production Plan'.
  WA_DAY_LAYOUT-CWIDTH_OPT = ' '.   "/optimizes the column width
  WA_DAY_LAYOUT-INFO_FNAME = 'IF'.
*//-- Set Variant Structure
  WA_DAY_VARIANT-REPORT       = SY-REPID.
  WA_DAY_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_DAY
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_DAY.
  CALL METHOD ALV_GRID_DAY->SET_TABLE_FOR_FIRST_DISPLAY
   EXPORTING   IS_LAYOUT        = WA_DAY_LAYOUT
             I_SAVE           = WA_DAY_SAVE
             IS_VARIANT       = WA_DAY_VARIANT
*               i_default        = space
               IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_DAY
   CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT_DAY[]
             IT_OUTTAB        = IT_DEL_OUTPUT[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_DAY
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0044   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_DAY USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT_DAY,  IT_FIELDCAT_DAY[],
         IT_FNAME_DAY, IT_FNAME_DAY[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FNAME_DAY.

  PERFORM SETTING_FIELDCAT_DAY TABLES IT_FIELDCAT_DAY USING :

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '8'.


*  WRITE W_D-1 TO L_DATUM MM/DD/YY.
*  PERFORM SETTING_FIELDCAT_DAY TABLES IT_FIELDCAT_DAY USING :
*
*                                     'S' 'MTD'        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
*                                     ' ' 'COLTEXT'     'MTD',
*                                     ' ' 'DECIMALS_O'  '0',
*                                     'E' 'OUTPUTLEN'   '6',
*
*
*                                     'S' 'D-1'        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
**                                     ' ' 'COLTEXT'     L_DATUM(5),
*
*                                     ' ' 'COLTEXT'     'Yesterday',
*                                     ' ' 'DECIMALS_O'  '0',
*                                     'E' 'OUTPUTLEN'   '5'.
*


  LOOP AT IT_DAY_DEL.
    CONCATENATE 'QTYD_' IT_DAY_DEL-SEQ INTO L_RQTY.
    WRITE IT_DAY_DEL-DATUM TO L_DATUM MM/DD/YY.

    PERFORM SETTING_FIELDCAT_DAY TABLES IT_FIELDCAT_DAY USING :

                                   'S' L_RQTY        ' ',
                                   ' ' 'COLTEXT'     L_DATUM(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '5'.
    CLEAR: L_RQTY.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELD_CATALOG_DAY
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_TOT  text
*      -->P_0759   text
*      -->P_0760   text
*      -->P_0761   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT_DAY TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                           USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FNAME_DAY INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT_tot

*---------------------------------------------------------------------*
*       FORM EXCLUDE_TB_FUNCTIONS_DAY                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS_DAY.
  DATA LS_EXCLUDE TYPE UI_FUNC.
**  This excludes all buttons
  LS_EXCLUDE = '&EXCLALLFC'.
  APPEND LS_EXCLUDE TO IT_EXCLUDE_DAY.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS_DAY


*** ALV for Froecast Schedule
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT_WEEK.
  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER_WEEK
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL_WEEK
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT ALV_GRID_WEEK
         EXPORTING I_PARENT = GRID_CONTAINER_WEEK
                   I_APPL_EVENTS = 'X'.


ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_WEEK
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_WEEK.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_WEEK_LAYOUT, WA_WEEK_VARIANT.

*//-- Set Layout Structure
  WA_WEEK_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_WEEK_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_WEEK_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_WEEK_LAYOUT-GRID_TITLE = 'KMMG Weekly Production Plan'.
  WA_WEEK_LAYOUT-CWIDTH_OPT = ' '.   "/optimizes the column width
  WA_WEEK_LAYOUT-INFO_FNAME = 'IF'.
*//-- Set Variant Structure
  WA_WEEK_VARIANT-REPORT       = SY-REPID.
  WA_WEEK_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_DAY
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_WEEK.
  CALL METHOD ALV_GRID_WEEK->SET_TABLE_FOR_FIRST_DISPLAY
   EXPORTING   IS_LAYOUT        = WA_WEEK_LAYOUT
             I_SAVE           = WA_WEEK_SAVE
             IS_VARIANT       = WA_WEEK_VARIANT
*               i_default        = space
*               IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_WEEK
   CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT_WEEK[]
             IT_OUTTAB        = IT_FC_OUTPUT[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_DAY
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0044   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_WEEK USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT_WEEK,  IT_FIELDCAT_WEEK[],
         IT_FNAME_WEEK, IT_FNAME_WEEK[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FNAME_WEEK.

  PERFORM SETTING_FIELDCAT_WEEK TABLES IT_FIELDCAT_WEEK USING :

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '8'.


*  WRITE W_D-1 TO L_DATUM MM/DD/YY.
*  PERFORM SETTING_FIELDCAT_DAY TABLES IT_FIELDCAT_DAY USING :
*
*                                     'S' 'MTD'        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
*                                     ' ' 'COLTEXT'     'MTD',
*                                     ' ' 'DECIMALS_O'  '0',
*                                     'E' 'OUTPUTLEN'   '6',
*
*
*                                     'S' 'D-1'        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
**                                     ' ' 'COLTEXT'     L_DATUM(5),
*
*                                     ' ' 'COLTEXT'     'Yesterday',
*                                     ' ' 'DECIMALS_O'  '0',
*                                     'E' 'OUTPUTLEN'   '5'.
*


  LOOP AT IT_WEEK.
    CONCATENATE 'QTYW_' IT_WEEK-SEQ INTO L_RQTY.
    WRITE IT_WEEK-DATUM TO L_DATUM MM/DD/YY.

    PERFORM SETTING_FIELDCAT_WEEK TABLES IT_FIELDCAT_WEEK USING :

                                   'S' L_RQTY        ' ',
                                   ' ' 'COLTEXT'     L_DATUM(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '5'.
    CLEAR: L_RQTY.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELD_CATALOG_WEEK
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_TOT  text
*      -->P_0759   text
*      -->P_0760   text
*      -->P_0761   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT_WEEK TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                           USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FNAME_WEEK INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT_tot

*---------------------------------------------------------------------*
*       FORM EXCLUDE_TB_FUNCTIONS_WEEK                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS_WEEK.
  DATA LS_EXCLUDE TYPE UI_FUNC.
**  This excludes all buttons
  LS_EXCLUDE = '&EXCLALLFC'.
  APPEND LS_EXCLUDE TO IT_EXCLUDE_WEEK.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS_WEEK


*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA_JIT.
  DATA: L_TEXT(30).
  DATA: BEGIN OF LT_MATNR OCCURS 50,
        MATNR LIKE IT_JITCALL-MATNR,
        END OF LT_MATNR.

  LOOP AT IT_JITCALL.
    LT_MATNR-MATNR = IT_JITCALL-MATNR.
    COLLECT LT_MATNR.
  ENDLOOP.

  SORT IT_JITCALL BY MATNR DATUM.
  LOOP AT LT_MATNR.
    IT_JIT_OUTPUT-MATNR = LT_MATNR-MATNR.
    LOOP AT IT_DAY.
      READ TABLE IT_JITCALL WITH KEY MATNR = LT_MATNR-MATNR
                                     DATUM = IT_DAY-DATUM
                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        CONCATENATE 'IT_JIT_OUTPUT-QTYD_' IT_DAY-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          <FS-QTY> = IT_JITCALL-QUANT.
        ENDIF.

      ENDIF.
    ENDLOOP.
    APPEND IT_JIT_OUTPUT.
    CLEAR: IT_JIT_OUTPUT.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA_JIT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_FORECAST_SCHEDULE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_FORECAST.

  DATA: BEGIN OF LT_TEMP OCCURS 0,
        MATNR LIKE VBAP-MATNR,
        EDATU LIKE VBEP-EDATU,
        WMENG TYPE I,
        END OF LT_TEMP.

  DATA: BEGIN OF LT_MATNR OCCURS 50,
         MATNR LIKE IT_JITCALL-MATNR,
         END OF LT_MATNR.

  DATA: L_CN(2) TYPE N,
        L_DATE LIKE SY-DATUM,
        L_DATE_1 LIKE SY-DATUM,
        L_DATE_2 LIKE SY-DATUM,
        L_DATE_3 LIKE SY-DATUM,
        L_DATE_4 LIKE SY-DATUM,
        L_DATE_5 LIKE SY-DATUM,
        L_DATE_6 LIKE SY-DATUM,
        L_DATE_7 LIKE SY-DATUM,
        L_DATE_8 LIKE SY-DATUM,
        L_DATE_9 LIKE SY-DATUM,
        L_DATE_10 LIKE SY-DATUM,
        L_DATE_11 LIKE SY-DATUM,
        L_DATE_12 LIKE SY-DATUM,
        L_DATE_13 LIKE SY-DATUM,
        L_DATE_14 LIKE SY-DATUM,
        L_DATE_15 LIKE SY-DATUM,
        L_DATE_16 LIKE SY-DATUM,
        L_DATE_17 LIKE SY-DATUM,
        L_DATE_18 LIKE SY-DATUM,
        L_DATE_19 LIKE SY-DATUM,
        L_DATE_20 LIKE SY-DATUM,
        L_DATE_21 LIKE SY-DATUM,
        L_DATE_22 LIKE SY-DATUM,
        L_TEXT(30),
        L_KALID LIKE KAKO-KALID.

  L_DATE = P_DATUM.

  CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
    EXPORTING
      P_PDATE             = L_DATE
   IMPORTING
     P_SUNDAY            = L_DATE
*   P_SATURDAY          =
*   P_DAY_IN_WEEK       =
*   P_WEEK_NO           =
             .
  L_DATE = L_DATE + 1.
  L_CN = '01'.
  DO 30 TIMES.
    IT_WEEK-SEQ = L_CN.
    L_DATE_1 = L_DATE.
    PERFORM READ_WORKING_DATE USING '+'  L_KALID  L_DATE_1.
    L_DATE = L_DATE + 7.
    IF L_DATE_1 > L_DATE.
      L_DATE = L_DATE + 7.
    ELSE.
      IF L_DATE = L_DATE_1.
        L_DATE_1 = L_DATE_1 - 7.
      ENDIF.
    ENDIF.
    IT_WEEK-DATUM = L_DATE_1.
    APPEND IT_WEEK.
    L_CN = L_CN + 1.
  ENDDO.

  READ TABLE IT_WEEK INDEX 1.
  L_DATE_1 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 2.
  L_DATE_2 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 3.
  L_DATE_3 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 4.
*  L_DATE_4 = IT_WEEK-DATUM - 1.
  L_DATE_4 = IT_WEEK-DATUM.

  READ TABLE IT_WEEK INDEX 5.
  L_DATE_5 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 6.
  L_DATE_6 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 7.
  L_DATE_7 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 8.
  L_DATE_8 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 9.
  L_DATE_9 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 10.
  L_DATE_10 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 11.
  L_DATE_11 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 12.
  L_DATE_12 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 13.
  L_DATE_13 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 14.
  L_DATE_14 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 15.
  L_DATE_15 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 16.
  L_DATE_16 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 17.
  L_DATE_17 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 18.
  L_DATE_18 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 19.
  L_DATE_19 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 20.
  L_DATE_20 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 21.
  L_DATE_21 = IT_WEEK-DATUM.
  READ TABLE IT_WEEK INDEX 2.
  L_DATE_22 = IT_WEEK-DATUM - 1.

*  SELECT MATNR EDATU SUM( WMENG ) AS WMENG INTO TABLE LT_TEMP
*     FROM VBAP AS A
*     INNER JOIN VBEP AS B
*     ON A~VBELN = B~VBELN
*     AND A~POSNR = B~POSNR
*     WHERE A~VBELN = P_VBELN
*     AND ABART = '1'
*     AND EDATU >= P_DATUM
*     GROUP BY MATNR EDATU.
*  SORT LT_TEMP BY MATNR EDATU.

  SELECT MATNR EDATU SUM( WMENG ) AS WMENG INTO TABLE LT_TEMP
      FROM ZTPP_ENG_DB
      WHERE SA_VBELN = P_VBELN
      AND CRDATE = P_CRDATE
      AND EDATU >= L_DATE_1
      AND ABART = '1'
     GROUP BY MATNR EDATU.
  SORT LT_TEMP BY MATNR EDATU.

  LOOP AT LT_TEMP.
    LT_MATNR-MATNR = LT_TEMP-MATNR.
    COLLECT LT_MATNR.
  ENDLOOP.

  SORT LT_MATNR BY MATNR.

  LOOP AT LT_MATNR.
    IT_FC_OUTPUT-MATNR = LT_MATNR-MATNR.

*    LOOP AT LT_TEMP WHERE MATNR = LT_MATNR-MATNR
*                     AND EDATU BETWEEN L_DATE_1 AND L_DATE_4.
*      IF LT_TEMP-EDATU >= L_DATE_1 AND LT_TEMP-EDATU < L_DATE_2.
*        IT_FC_OUTPUT-QTYW_01 = IT_FC_OUTPUT-QTYW_01 + LT_TEMP-WMENG.
*      ELSEIF LT_TEMP-EDATU >= L_DATE_2 AND LT_TEMP-EDATU < L_DATE_3.
*        IT_FC_OUTPUT-QTYW_02 = IT_FC_OUTPUT-QTYW_02 + LT_TEMP-WMENG.
*      ELSEIF LT_TEMP-EDATU >= L_DATE_3 AND LT_TEMP-EDATU =< L_DATE_4.
*        IT_FC_OUTPUT-QTYW_03 = IT_FC_OUTPUT-QTYW_03 + LT_TEMP-WMENG.
*      ENDIF.
*    ENDLOOP.

*    LOOP AT IT_WEEK WHERE SEQ > 3.
*      READ TABLE LT_TEMP WITH KEY MATNR = LT_MATNR-MATNR
*                                     EDATU = IT_WEEK-DATUM
*                                     BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        CONCATENATE 'IT_FC_OUTPUT-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS-QTY>.
*        IF SY-SUBRC = 0.
*          <FS-QTY> = LT_TEMP-WMENG.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
    LOOP AT LT_TEMP WHERE MATNR = LT_MATNR-MATNR.
      IF LT_TEMP-EDATU >= L_DATE_1 AND LT_TEMP-EDATU < L_DATE_2.
        IT_FC_OUTPUT-QTYW_01 = IT_FC_OUTPUT-QTYW_01 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_2 AND LT_TEMP-EDATU < L_DATE_3.
        IT_FC_OUTPUT-QTYW_02 = IT_FC_OUTPUT-QTYW_02 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_3 AND LT_TEMP-EDATU < L_DATE_4.
        IT_FC_OUTPUT-QTYW_03 = IT_FC_OUTPUT-QTYW_03 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_4 AND LT_TEMP-EDATU < L_DATE_5.
        IT_FC_OUTPUT-QTYW_04 = IT_FC_OUTPUT-QTYW_04 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_5 AND LT_TEMP-EDATU < L_DATE_6.
        IT_FC_OUTPUT-QTYW_05 = IT_FC_OUTPUT-QTYW_05 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_6 AND LT_TEMP-EDATU < L_DATE_7.
        IT_FC_OUTPUT-QTYW_06 = IT_FC_OUTPUT-QTYW_06 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_7 AND LT_TEMP-EDATU < L_DATE_8.
        IT_FC_OUTPUT-QTYW_07 = IT_FC_OUTPUT-QTYW_07 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_8 AND LT_TEMP-EDATU < L_DATE_9.
        IT_FC_OUTPUT-QTYW_08 = IT_FC_OUTPUT-QTYW_08 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_9 AND LT_TEMP-EDATU < L_DATE_10.
        IT_FC_OUTPUT-QTYW_09 = IT_FC_OUTPUT-QTYW_09 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_10 AND LT_TEMP-EDATU < L_DATE_11.
        IT_FC_OUTPUT-QTYW_10 = IT_FC_OUTPUT-QTYW_10 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_11 AND LT_TEMP-EDATU < L_DATE_12.
        IT_FC_OUTPUT-QTYW_11 = IT_FC_OUTPUT-QTYW_11 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_12 AND LT_TEMP-EDATU < L_DATE_13.
        IT_FC_OUTPUT-QTYW_12 = IT_FC_OUTPUT-QTYW_12 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_13 AND LT_TEMP-EDATU < L_DATE_14.
        IT_FC_OUTPUT-QTYW_13 = IT_FC_OUTPUT-QTYW_13 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_14 AND LT_TEMP-EDATU < L_DATE_15.
        IT_FC_OUTPUT-QTYW_14 = IT_FC_OUTPUT-QTYW_14 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_15 AND LT_TEMP-EDATU < L_DATE_16.
        IT_FC_OUTPUT-QTYW_15 = IT_FC_OUTPUT-QTYW_15 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_16 AND LT_TEMP-EDATU < L_DATE_17.
        IT_FC_OUTPUT-QTYW_16 = IT_FC_OUTPUT-QTYW_16 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_17 AND LT_TEMP-EDATU < L_DATE_18.
        IT_FC_OUTPUT-QTYW_17 = IT_FC_OUTPUT-QTYW_17 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_18 AND LT_TEMP-EDATU < L_DATE_19.
        IT_FC_OUTPUT-QTYW_18 = IT_FC_OUTPUT-QTYW_18 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_19 AND LT_TEMP-EDATU < L_DATE_20.
        IT_FC_OUTPUT-QTYW_19 = IT_FC_OUTPUT-QTYW_19 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_20 AND LT_TEMP-EDATU < L_DATE_21.
        IT_FC_OUTPUT-QTYW_20 = IT_FC_OUTPUT-QTYW_20 + LT_TEMP-WMENG.
      ELSEIF LT_TEMP-EDATU >= L_DATE_21 AND LT_TEMP-EDATU =< L_DATE_22.
        IT_FC_OUTPUT-QTYW_21 = IT_FC_OUTPUT-QTYW_21 + LT_TEMP-WMENG.
      ENDIF.
    ENDLOOP.

    APPEND IT_FC_OUTPUT.
    CLEAR: IT_FC_OUTPUT.
  ENDLOOP.
  SORT IT_FC_OUTPUT BY MATNR.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_JIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_JIT.
  PERFORM GET_JIT_CALL.
  PERFORM SET_DAY_JIT.
  PERFORM PROCESS_DATA_JIT.
ENDFORM.                    " GET_DATA_JIT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_DEL.

  DATA: BEGIN OF LT_TEMP OCCURS 0,
       MATNR LIKE VBAP-MATNR,
       EDATU LIKE VBEP-EDATU,
       WMENG TYPE I,
       END OF LT_TEMP.

  DATA: BEGIN OF LT_MATNR OCCURS 50,
         MATNR LIKE IT_JITCALL-MATNR,
         END OF LT_MATNR.

  DATA: L_CN(2) TYPE N,
        L_DATE LIKE SY-DATUM,
        L_TEXT(30),
        L_KALID LIKE KAKO-KALID.

*  SELECT MATNR EDATU SUM( WMENG ) AS WMENG INTO TABLE LT_TEMP
*    FROM VBAP AS A
*    INNER JOIN VBEP AS B
*    ON A~VBELN = B~VBELN
*    AND A~POSNR = B~POSNR
*    WHERE A~VBELN = P_VBELN
*    AND ABART = '2'
*    AND EDATU >= P_DATUM
*    GROUP BY MATNR EDATU.
*  SORT LT_TEMP BY MATNR EDATU.

  SELECT MATNR EDATU SUM( WMENG ) AS WMENG INTO TABLE LT_TEMP
      FROM ZTPP_ENG_DB
      WHERE SA_VBELN = P_VBELN
      AND CRDATE = P_CRDATE
      AND EDATU >= P_DATUM
      AND ABART = '2'
     GROUP BY MATNR EDATU.

  SORT LT_TEMP BY MATNR EDATU.

  LOOP AT LT_TEMP.
    LT_MATNR-MATNR = LT_TEMP-MATNR.
    COLLECT LT_MATNR.
  ENDLOOP.
  SORT LT_MATNR BY MATNR.

  SELECT SINGLE KALID INTO L_KALID
  FROM ZVPP_CAPACITY
  WHERE ARBPL = 'T'   .

  L_CN = '00'.
  L_DATE = P_DATUM.

  WHILE L_CN < 30.
    L_CN = L_CN + 1.
    IT_DAY_DEL-SEQ = L_CN.
*    PERFORM READ_WORKING_DATE USING '+' L_KALID L_DATE.
    IT_DAY_DEL-DATUM = L_DATE.
    APPEND IT_DAY_DEL.
    L_DATE = L_DATE + 1.
  ENDWHILE.

  LOOP AT LT_MATNR.
    IT_DEL_OUTPUT-MATNR = LT_MATNR-MATNR.
    LOOP AT IT_DAY_DEL.
      READ TABLE LT_TEMP WITH KEY MATNR = LT_MATNR-MATNR
                                     EDATU = IT_DAY_DEL-DATUM
                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        CONCATENATE 'IT_DEL_OUTPUT-QTYD_' IT_DAY_DEL-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          <FS-QTY> = LT_TEMP-WMENG.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND IT_DEL_OUTPUT.
    CLEAR: IT_DEL_OUTPUT.
  ENDLOOP.
  SORT IT_DEL_OUTPUT BY MATNR.
ENDFORM.                    " GET_DATA_DEL
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
FORM READ_WORKING_DATE USING PA_TYPE PA_KALID PA_WDATE.

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
