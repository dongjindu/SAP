************************************************************************
* Program Name      : ZMMR_OVERSTOCK
* Author            : Furong Wang
* Creation Date     : 05/2010
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZMMR_OVERSTOCK MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TABLES: MARA, ZTMM_OVERSTOCK.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF IT_DATA OCCURS 0,
      MATNR LIKE MARA-MATNR,
      WERKS LIKE MARC-WERKS,
      EFFECTIVE_OUT LIKE SY-DATUM,
      ASSY LIKE ZTMM_OVERSTOCK-ASSY,
END OF IT_DATA .

DATA: BEGIN OF IT_OVERSTOCK OCCURS 0.
        INCLUDE STRUCTURE ZTMM_OVERSTOCK.
DATA: END OF IT_OVERSTOCK.

DATA: BEGIN OF IT_MBEW OCCURS 0,
        MATNR LIKE MBEW-MATNR,
        WERKS LIKE MBEW-BWKEY,
        BKLAS LIKE MBEW-BKLAS,
        LBKUM LIKE MBEW-LBKUM,
        SALK3 LIKE MBEW-SALK3,
        MTART LIKE MARA-MTART,
        MSTAE LIKE MARA-MSTAE,
        DISPO LIKE MARC-DISPO,
        LGPRO LIKE MARC-LGPRO,
        MATKL LIKE MARA-MATKL,
*---<< 11/08/2013. bsbae
       BDMNG  LIKE MDSM-BDMNG,
*--->> 11/08/2013. bsbae
        END OF IT_MBEW.

DATA: BEGIN OF IT_STOCK OCCURS 0,
        MATNR LIKE MBEW-MATNR,
        LBKUM LIKE MBEW-LBKUM,
        SALK3 LIKE MBEW-SALK3,
        DISPO LIKE MARC-DISPO,
        LGPRO LIKE MARC-LGPRO,
        END OF IT_STOCK.

DATA: BEGIN OF IT_MARA OCCURS 0,
        MATNR LIKE MBEW-MATNR,
        WERKS LIKE MBEW-BWKEY,
        DISPO LIKE MARC-DISPO,
        LGPRO LIKE MARC-LGPRO,
        END OF IT_MARA.

DATA: IT_WULTB TYPE TABLE OF STPOV WITH HEADER LINE.

** Parallel Processing

DATA: W_MAX TYPE I,
        W_FREE TYPE I,
        W_LINES TYPE I,
        W_REM TYPE I,
        W_NO_TIMES TYPE I,
        W_I TYPE I,
        W_FRM TYPE I,
        W_TO TYPE I.
DATA: W_TASKNAME(4) TYPE N VALUE '0001',
      W_EXCEP_FLAG TYPE C,
      W_SND_JOBS TYPE I VALUE 1,
      W_RCV_JOBS TYPE I VALUE 1.

DATA: W_MATNR LIKE MBEW-MATNR,
      W_WERKS LIKE MBEW-BWKEY.
** ALV
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
      IT_EXCLUDE TYPE UI_FUNCTIONS.

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL_800 TYPE SCRFNAME VALUE 'ALV_CONTAINER_800',
      ALV_GRID_800          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER_800    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: G_LIGHTS_FIELDNAME  TYPE SLIS_FIELDNAME VALUE 'LIGHTS'.

DATA: OK_CODE LIKE SY-UCOMM,
      W_CODE LIKE SY-UCOMM,
      W_OLD_CODE LIKE SY-UCOMM,
      W_CNT   TYPE   I,
      W_BASE_DATE LIKE SY-DATUM,
      W_BASE_TIME LIKE SY-UZEIT,
      W_REPID LIKE SY-REPID,
      W_DYNNR LIKE SY-DYNNR.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:   P_SAVE(1) DEFAULT 'X',
 P_BOM AS CHECKBOX DEFAULT 'X',
 P_TIME TYPE I DEFAULT 40.

SELECT-OPTIONS: S_MATRN FOR MARA-MATNR.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
INITIALIZATION.
*  PERFORM INIT_DATA.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM CHECKING_BATCH_JOB.
  PERFORM GET_DATA.
  IF P_BOM = 'X'.
    PERFORM PROCESS_DATA_BOM.
  ELSE.
    PERFORM PROCESS_DATA_NO_BOM.
  ENDIF.
  IF P_SAVE = 'X'.
    PERFORM SAVE_TO_TABLE.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE DISPLAY_ALV OUTPUT.
*  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
*    PERFORM CREATE_CONTAINER_N_OBJECT.
*    PERFORM SET_ATTRIBUTES_ALV_GRID.
*    PERFORM BUILD_SORTCAT_DISPLAY.
**    PERFORM EXCLUDE_TB_FUNCTIONS.
*    PERFORM BUILD_FIELD_CATALOG USING 'IT_INPUT_PLAN'.
*    PERFORM ASSIGN_ITAB_TO_ALV.
**    PERFORM sssign_event_9000.
*  ELSE.
*    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
*  ENDIF.
*
*ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CREATE_CONTAINER_N_OBJECT.
*  CLEAR: W_REPID.
*  CREATE OBJECT GRID_CONTAINER
*          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
*          EXCEPTIONS
*           CNTL_ERROR = 1
*           CNTL_SYSTEM_ERROR = 2
*           CREATE_ERROR = 3
*           LIFETIME_ERROR = 4
*           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*  W_REPID = SY-REPID.
*  IF SY-SUBRC NE 0.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*         EXPORTING
*              TITEL = W_REPID
*              TXT2  = SY-SUBRC
*              TXT1  = 'The control can not be created'.
*  ENDIF.
*  CREATE OBJECT ALV_GRID
*         EXPORTING I_PARENT = GRID_CONTAINER
*                   I_APPL_EVENTS = 'X'.
*ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.
* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog' P_GUBUN P_FIELD.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME .
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

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS.
*  DATA LS_EXCLUDE TYPE UI_FUNC.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_0800 OUTPUT.
  IF GRID_CONTAINER_800 IS INITIAL.
    PERFORM CREATE_CONTAINER_OBJECT_800.
    PERFORM SET_ATTRIBUTES_ALV_GRID_800.
*    PERFORM BUILD_SORTCAT_DISPLAY.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG_800 USING 'IT_DATA'.
    PERFORM ASSIGN_ITAB_TO_ALV_800.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID_800->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_OBJECT_800.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER_800
    EXPORTING
      CONTAINER_NAME              = WA_CUSTOM_CONTROL_800
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = W_REPID
        TXT2  = SY-SUBRC
        TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID_800
    EXPORTING
      I_PARENT      = GRID_CONTAINER_800
      I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_800.
  DATA: L_DATE_C(10),
        L_TIME(8),
        L_UPH_C(6).
  .

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
  CONCATENATE W_BASE_DATE+4(2) '/' W_BASE_DATE+6(2) '/' W_BASE_DATE+0(4)
                                                           INTO L_DATE_C.
  CONCATENATE W_BASE_TIME+0(2) ':' W_BASE_TIME+2(2) ':' W_BASE_TIME+4(2)
                                                             INTO L_TIME.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-CTAB_FNAME  = 'COLOR'.
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.
  CONCATENATE 'As of' L_DATE_C L_TIME INTO WA_IS_LAYOUT-GRID_TITLE
 SEPARATED BY SPACE.

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

  WA_IS_LAYOUT-ZEBRA             = 'X'.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3194   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_800 USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,
  DATA: L_CN(2) TYPE N,
  L_RP(30),
  L_HR(10).

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

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

                                  'S' 'MATNR'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Part',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'MAKTX'    ' ',
                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                   'S' 'HR_OH'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'COLTEXT'     'On Hand',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'LIFNR'    ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '18',

                                'S' 'RP'    ' ',
                                  ' ' 'COLTEXT'     'RP',
                                  'E' 'OUTPUTLEN'   '3',

                                 'S' 'PRVBE'    ' ',
                                 ' ' 'COLTEXT'     'Supp Area',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'DSNAM'    ' ',
                                  ' ' 'COLTEXT'     'MRP Controller',
                                  'E' 'OUTPUTLEN'   '18',

                                'S' 'MATKL'    ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'LN_OHQTY'    ' ',
                                 ' ' 'COLTEXT'     'Stock',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '13'.
*
*                                 'S' 'WH_OHQTY'    ' ',
*                                  ' ' 'COLTEXT'     'WH Qty',
*                                 ' ' 'DECIMALS_O'  '0',
*                                 'E' 'OUTPUTLEN'   '13',

*                                 'S' 'ADJ_QTY'    ' ',
*                                  ' ' 'COLTEXT'     'Adj Qty',
*                                 ' ' 'DECIMALS_O'  '0',
*                                 'E' 'OUTPUTLEN'   '13'.

  L_CN = '00'.
  DO 40 TIMES.
    L_CN = L_CN + 1.

*    READ TABLE it_day WITH KEY seq = l_cn.


    CONCATENATE 'RP' L_CN INTO L_RP.
    CONCATENATE  L_CN 'Hr' INTO L_HR SEPARATED BY SPACE.

    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_RP        ' ',
                                   ' ' 'COLTEXT'     L_HR,
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '10'.
    CLEAR: L_RP.
  ENDDO.
  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                    'S' 'TOTAL'      ' ',
                                    ' ' 'COLTEXT'     'Total',
                                    ' ' 'DECIMALS_O'  '0',
                                    'E' 'OUTPUTLEN'   '10'.

ENDFORM.                    " BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_800.
  CALL METHOD ALV_GRID_800->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = IT_EXCLUDE[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_DATA[]
      IT_SORT              = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID_800->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID_800->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
    EXPORTING
      CONTROL = ALV_GRID_800.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0800 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_cell_color
*&---------------------------------------------------------------------*
*       Set Cell Color
*----------------------------------------------------------------------*
FORM SET_CELL_COLOR  USING    U_COL
                              U_INT
                              U_FIELD
                     CHANGING COLOR_TAB
                              TYPE SLIS_T_SPECIALCOL_ALV.
*----------------------------------------------------------------------*
* No  Colour
*  0  COL_BACKGROUND
*  1  COL_HEADING
*  2  COL_NORMAL
*  3  COL_TOTAL
*  4  COL_KEY
*  5  COL_POSITIVE
*  6  COL_NEGATIVE
*  7  COL_GROUP
*----------------------------------------------------------------------*
  DATA : L_COLOR TYPE SLIS_SPECIALCOL_ALV.
  L_COLOR-FIELDNAME = U_FIELD.
  L_COLOR-COLOR-COL = U_COL.
  L_COLOR-COLOR-INT = U_INT.
  APPEND L_COLOR TO COLOR_TAB.
ENDFORM.                    " set_cell_color
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM INIT_DATA.
*  W_REPID = SY-REPID.
*  W_DYNNR = SY-DYNNR.
*ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  save_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TO_TABLE.

  DELETE FROM ZTMM_OVERSTOCK CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

*  IF SY-SUBRC = 0.
  INSERT ZTMM_OVERSTOCK FROM TABLE IT_OVERSTOCK.
*  ELSE.
*    MESSAGE E000(ZZ) WITH 'Error: Z-Table deletion (ZTMM_HOUR_SHORT)'.
*  ENDIF.
ENDFORM.                    " save_to_table
*&---------------------------------------------------------------------*
*&      Form  checking_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM CHECKING_BATCH_JOB.
  DATA: L_BACKJOB LIKE  SY-REPID,
        LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.
  L_BACKJOB = SY-REPID.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      ABAP_PROGRAM_NAME             = L_BACKJOB
      DIALOG                        = 'N'
      STATUS                        = 'R'
    TABLES
      JOBLIST                       = LT_JOBLIST
    EXCEPTIONS
      NO_JOBS_FOUND                 = 1
      PROGRAM_SPECIFICATION_MISSING = 2
      INVALID_DIALOG_TYPE           = 3
      JOB_FIND_CANCELED             = 4
      OTHERS                        = 5.

  IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S999(PP) WITH TEXT-M01.
      LEAVE PROGRAM.
    ENDIF.
*  ELSE.
*    READ TABLE LT_JOBLIST INDEX 1.
*    IF SY-SUBRC EQ 0.
*      MESSAGE E999(PP) WITH TEXT-M01.
*    ENDIF.
  ENDIF.

ENDFORM.                    " checking_batch_job
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
*  DATA: L_MATNR LIKE MARA-MATNR,
*         L_WERKS LIKE MARC-WERKS,
*         L_LGPRO LIKE MARC-LGPRO,
*         L_NEXT(1),
  DATA:    L_LINES TYPE I.

  DATA:    L_INDEX LIKE SY-TABIX.


  DATA: BEGIN OF LT_LGPRO OCCURS 4,
        MATNR LIKE MARC-MATNR,
        WERKS LIKE MARC-WERKS,
        DISPO LIKE MARC-DISPO,
        LGPRO LIKE MARC-LGPRO,
        END OF LT_LGPRO.

  DATA: LT_MBEW_TEMP LIKE TABLE OF IT_MBEW WITH HEADER LINE.

** Changed by Furong on 01/07/11
*  SELECT A~MATNR BWKEY AS WERKS BKLAS LBKUM SALK3 MTART MSTAE
*    DISPO LGPRO MATKL
*    INTO TABLE IT_MBEW   "up TO 1O0 rows
*    FROM MBEW AS A
*    INNER JOIN MARC AS C
*    ON A~MATNR = C~MATNR
*    AND A~BWKEY = C~WERKS
*    INNER JOIN MARA AS B
*    ON B~MATNR = C~MATNR
*    WHERE ( B~MTART = 'ROH' OR  B~MTART = 'ROH1' )
*     AND MSTAE <> '11'
*     AND LBKUM > 0
*     AND A~MATNR IN S_MATRN    " for test only
*     AND NOT EXISTS ( SELECT * FROM MDSM WHERE MATNR = A~MATNR
*                                           AND PLSCN = '900' ).

* IF SY-SUBRC <> 0.
*    MESSAGE E000(ZZ) WITH 'No Data'.
*  ENDIF.

  SELECT A~MATNR BWKEY AS WERKS BKLAS LBKUM SALK3 MTART MSTAE
         MATKL
    INTO CORRESPONDING FIELDS OF TABLE IT_MBEW   "up TO 10 rows
    FROM MBEW AS A INNER JOIN MARC AS C
                           ON A~MATNR = C~MATNR
                          AND A~BWKEY = C~WERKS
                   INNER JOIN MARA AS B
                           ON B~MATNR = C~MATNR
   WHERE ( B~MTART = 'ROH' OR B~MTART = 'ROH1' )
** Changed on 02/28/11
*       AND MSTAE <> '11'
     AND ( MSTAE <> '11' AND  MSTAE <> 'OB' )
** End of change
*     AND LBKUM > 0
     AND A~MATNR IN S_MATRN    " for test only
     AND NOT EXISTS ( SELECT * FROM MDSM WHERE MATNR = A~MATNR
                                           AND PLSCN = '900' )
** Furong on 05/23/12
      %_HINTS ORACLE
      'LEADING(T_02) USE_NL(T_00 T_01) INDEX (T_02 "MARA~T")'.
** End on 05/2312

*---<< 11/08/2013. bsbae
  PERFORM GET_LTP_MATERIAL.
*--->> 11/08/2013. bsbae

  IF IT_MBEW[] IS INITIAL.
    MESSAGE E000(ZZ) WITH 'No Data'.
  ENDIF.

  LOOP AT IT_MBEW.
    IF IT_MBEW-LBKUM > 0.
      LT_MBEW_TEMP = IT_MBEW.
      APPEND LT_MBEW_TEMP.
    ENDIF.
  ENDLOOP.
** End of change on 01/07/11

*  SORT IT_MBEW BY MATNR WERKS.
  SORT IT_MBEW BY MATNR.


** Changed by Furong on 01/07/11
  SORT LT_MBEW_TEMP BY MATNR WERKS.
*  LOOP AT IT_MBEW.
*    IT_STOCK-MATNR = IT_MBEW-MATNR.
*    IT_STOCK-LBKUM = IT_MBEW-LBKUM.
*    IT_STOCK-SALK3 = IT_MBEW-SALK3.
*    COLLECT IT_STOCK.
*  ENDLOOP.

  LOOP AT LT_MBEW_TEMP.
    IT_STOCK-MATNR = LT_MBEW_TEMP-MATNR.
    IT_STOCK-LBKUM = LT_MBEW_TEMP-LBKUM.
    IT_STOCK-SALK3 = LT_MBEW_TEMP-SALK3.
    COLLECT IT_STOCK.
  ENDLOOP.
** End of change

  SORT IT_STOCK BY MATNR.

  LOOP AT IT_STOCK.
    REFRESH LT_LGPRO.
    CLEAR: IT_MARA, LT_LGPRO.
    SELECT MATNR WERKS DISPO LGPRO
      INTO TABLE LT_LGPRO
      FROM MARC
     WHERE MATNR = IT_STOCK-MATNR
       AND LGPRO <> ' '.

** Changed on 03/09/11
*    CLEAR: L_LINES.
*    DESCRIBE TABLE LT_LGPRO LINES L_LINES.
*    IF L_LINES > 1.
*      DELETE LT_LGPRO WHERE WERKS = 'P001'.
*    ENDIF.
*    IF SY-SUBRC = 0.
*      LOOP AT LT_LGPRO.
*        IT_MARA-MATNR = IT_STOCK-MATNR.
*        IT_MARA-WERKS = LT_LGPRO-WERKS.
*        APPEND IT_MARA.
*      ENDLOOP.
*    ENDIF.
    IF SY-SUBRC = 0.
      CLEAR: L_LINES.
      DESCRIBE TABLE LT_LGPRO LINES L_LINES.
      IF L_LINES > 1.
        READ TABLE LT_LGPRO WITH KEY WERKS = 'E001'.
        IF SY-SUBRC = 0.
          IT_MARA-MATNR = IT_STOCK-MATNR.
          IT_MARA-WERKS = LT_LGPRO-WERKS.
          IT_MARA-DISPO = LT_LGPRO-DISPO.
          IT_MARA-LGPRO = LT_LGPRO-LGPRO.
          APPEND IT_MARA.
        ELSE.
          SELECT SINGLE DISPO LGPRO
            INTO (IT_MARA-DISPO,IT_MARA-LGPRO)
            FROM MARC
           WHERE MATNR = IT_STOCK-MATNR
             AND WERKS = 'E002'.
          IF SY-SUBRC = 0.
            IT_MARA-MATNR = IT_STOCK-MATNR.
            IT_MARA-WERKS = 'E002'.
            APPEND IT_MARA.
          ELSE.
            SELECT SINGLE DISPO LGPRO
              INTO (IT_MARA-DISPO,IT_MARA-LGPRO)
              FROM MARC
             WHERE MATNR = IT_STOCK-MATNR
               AND WERKS = 'P001'.
            IF SY-SUBRC = 0.
              IT_MARA-MATNR = IT_STOCK-MATNR.
              IT_MARA-WERKS = 'P001'.
              APPEND IT_MARA.
            ELSE.
              IT_MARA-MATNR = IT_STOCK-MATNR.
              IT_MARA-WERKS = 'ERR'.
              APPEND IT_MARA.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        READ TABLE LT_LGPRO INDEX 1.
        IT_MARA-MATNR = IT_STOCK-MATNR.
        IT_MARA-WERKS = LT_LGPRO-WERKS.
        IT_MARA-DISPO = LT_LGPRO-DISPO.
        IT_MARA-LGPRO = LT_LGPRO-LGPRO.
        APPEND IT_MARA.
      ENDIF.

      IT_STOCK-DISPO = IT_MARA-DISPO.
      IT_STOCK-LGPRO = IT_MARA-LGPRO.

    ELSE.
      READ TABLE LT_MBEW_TEMP WITH KEY MATNR = IT_STOCK-MATNR
                                       WERKS = 'E001'
                              BINARY SEARCH.
      IF SY-SUBRC = 0.
        IT_MARA-MATNR = IT_STOCK-MATNR.
        IT_MARA-WERKS = LT_MBEW_TEMP-WERKS.
      ELSE.
        READ TABLE LT_MBEW_TEMP WITH KEY MATNR = IT_STOCK-MATNR
                                         WERKS = 'P001'
                                BINARY SEARCH.
        IF SY-SUBRC = 0.
          IT_MARA-MATNR = IT_STOCK-MATNR.
          IT_MARA-WERKS = LT_MBEW_TEMP-WERKS.
        ELSE.
          IT_MARA-MATNR = IT_STOCK-MATNR.
          IT_MARA-WERKS = 'P001'.
        ENDIF.
      ENDIF.

      SELECT SINGLE DISPO LGPRO
        INTO (IT_MARA-DISPO,IT_MARA-LGPRO)
        FROM MARC
       WHERE MATNR = IT_MARA-MATNR
         AND WERKS = IT_MARA-WERKS.

      APPEND IT_MARA.

      IT_STOCK-DISPO = IT_MARA-DISPO.
      IT_STOCK-LGPRO = IT_MARA-LGPRO.
    ENDIF.
** End of change

    MODIFY IT_STOCK. CLEAR IT_STOCK.

  ENDLOOP.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA_BOM.

  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      GROUP_NAME                     = 'PG_FTZ'
    IMPORTING
      MAX_PBT_WPS                    = W_MAX
      FREE_PBT_WPS                   = W_FREE
    EXCEPTIONS
      INVALID_GROUP_NAME             = 1
      INTERNAL_ERROR                 = 2
      PBT_ENV_ALREADY_INITIALIZED    = 3
      CURRENTLY_NO_RESOURCES_AVAIL   = 4
      NO_PBT_RESOURCES_FOUND         = 5
      CANT_INIT_DIFFERENT_PBT_GROUPS = 6
      OTHERS                         = 7.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DESCRIBE TABLE IT_MARA LINES W_LINES.

  IF W_LINES > 0.

    IF P_TIME > 1.
      W_FREE = W_FREE * P_TIME.
    ENDIF.
    IF W_LINES > W_FREE.
*{ 09/13/2011 PAUL CHANGES DIVIDE ERROR.
*      W_REM = W_LINES MOD W_FREE.
*      W_NO_TIMES = W_LINES / W_FREE.
      IF W_FREE = 0.
        W_REM = 0.
        W_NO_TIMES = 0.
      ELSE.
        W_REM = W_LINES MOD W_FREE.
        W_NO_TIMES = W_LINES / W_FREE.
      ENDIF.
*}
      IF W_REM EQ 0.
      ELSE.
        W_NO_TIMES = W_NO_TIMES + 1.
      ENDIF.
    ELSE.
      W_NO_TIMES = 1.
    ENDIF.
    W_I = 1.
    WHILE W_I <= W_NO_TIMES.
      IF W_I = 1.
        W_FRM = W_I.
      ELSE.
        W_FRM = W_TO + 1.
      ENDIF.
      IF W_LINES > W_FREE.
        W_TO = W_I * W_FREE.
      ELSE.
        W_TO = W_LINES.
      ENDIF.
      LOOP AT IT_MARA FROM W_FRM TO W_TO.
        DO.
          CALL FUNCTION 'Z_FMM_WHERE_USED_MAT_MULTI'
            STARTING NEW TASK W_TASKNAME
            DESTINATION IN GROUP 'PG_FTZ'
            PERFORMING WHERE_USED_MAT_MULTI ON END OF TASK
            EXPORTING
              P_MATNR               = IT_MARA-MATNR
              P_WERKS               = IT_MARA-WERKS
            TABLES
              PT_WULTB              = IT_WULTB
            EXCEPTIONS
              COMMUNICATION_FAILURE = 1
              SYSTEM_FAILURE        = 2
              RESOURCE_FAILURE      = 3.

          CASE SY-SUBRC.
            WHEN 0.
              W_TASKNAME = W_TASKNAME + 1.
              W_SND_JOBS = W_SND_JOBS + 1.
              EXIT.
            WHEN 1 OR 2.
              W_EXCEP_FLAG = 'X'.
            WHEN 3.
              IF W_EXCEP_FLAG = SPACE.
                W_EXCEP_FLAG = 'X'.
                WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.01' SECONDS.
              ELSE.
                WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.1' SECONDS.
              ENDIF.
              IF SY-SUBRC EQ 0.
                CLEAR W_EXCEP_FLAG.
*          ELSE.
*            EXIT.
              ENDIF.
          ENDCASE.
        ENDDO.
      ENDLOOP.

      DO.
        WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
        IF W_RCV_JOBS >= W_SND_JOBS.
          EXIT.
        ENDIF.
      ENDDO.

      W_I = W_I + 1.

    ENDWHILE.

    PERFORM FINAL_PROCESS.
  ENDIF.

ENDFORM.                    " process_data_BOM
*&---------------------------------------------------------------------*
*&      Form  WHERE_USED_MAT_MULTI
*&---------------------------------------------------------------------*
FORM WHERE_USED_MAT_MULTI USING W_TASKNAME.
  DATA: L_ASSY LIKE MARA-MATNR,
        L_CON(1),
        L_LINE TYPE I,
        L_LESS(1),
        L_INDEX LIKE SY-TABIX.

  DATA: LW_DATA LIKE LINE OF IT_DATA.

  RECEIVE RESULTS FROM FUNCTION 'Z_FMM_WHERE_USED_MAT_MULTI'
          IMPORTING PO_MATNR = W_MATNR
                    PO_WERKS = W_WERKS
          TABLES    PT_WULTB        = IT_WULTB
          EXCEPTIONS
                    COMMUNICATION_FAILURE = 1
                    SYSTEM_FAILURE        = 2.
  IF SY-SUBRC NE 0.
    W_EXCEP_FLAG = 'X'.
    EXIT.
  ENDIF.
  W_RCV_JOBS = W_RCV_JOBS + 1.

  CLEAR: IT_DATA.
  DESCRIBE TABLE IT_WULTB LINES L_LINE.

  IF L_LINE > 0.
    SORT IT_WULTB BY DATUB DESCENDING.
    LOOP AT IT_WULTB.
      IF IT_WULTB-DATUB <= SY-DATUM.
        IF IT_WULTB-MATNR+4(1) = 'P' OR
           IT_WULTB-MATNR+4(1) = 'W'.
        ELSE.
          IT_DATA-EFFECTIVE_OUT = IT_WULTB-DATUB.
          L_ASSY = IT_WULTB-MATNR.
          L_LESS = 'T'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF IT_DATA-EFFECTIVE_OUT IS INITIAL.
      SORT IT_WULTB BY DATUB.
** Changed by Furong on 09/09/10
      LOOP AT IT_WULTB.
        IF IT_WULTB-MATNR+4(1) = 'P' OR
           IT_WULTB-MATNR+4(1) = 'W'.
          CONTINUE.
        ELSE.
          IT_DATA-EFFECTIVE_OUT = IT_WULTB-DATUB.
          L_ASSY = IT_WULTB-MATNR.
          EXIT.
        ENDIF.
      ENDLOOP.
*      READ TABLE IT_WULTB INDEX 1.
*      IT_DATA-EFFECTIVE_OUT = IT_WULTB-DATUB.
*      L_ASSY = IT_WULTB-MATNR.
*      CLEAR L_LESS.
** End of change
      CLEAR L_LESS.
    ENDIF.

    SORT IT_WULTB BY LEVEL.
    L_CON = 'T'.
    WHILE L_CON = 'T'.
      READ TABLE IT_WULTB WITH KEY IDNRK = L_ASSY.
      IF SY-SUBRC = 0.
        IF IT_WULTB-MATNR+4(1) = 'P' OR
          IT_WULTB-MATNR+4(1) = 'W'.
          L_CON = 'F'.
        ELSE.
          L_ASSY = IT_WULTB-MATNR.
        ENDIF.
      ELSE.
        L_CON = 'F'.
      ENDIF.
    ENDWHILE.
    IT_DATA-ASSY = L_ASSY.
  ENDIF.
  IT_DATA-MATNR = W_MATNR.
  IT_DATA-WERKS = W_WERKS.

  READ TABLE IT_DATA INTO LW_DATA WITH KEY MATNR = W_MATNR.
  IF SY-SUBRC = 0.
    L_INDEX = SY-TABIX.
    IF L_LESS = 'T'.
      IF LW_DATA-EFFECTIVE_OUT > SY-DATUM.
        MODIFY IT_DATA INDEX L_INDEX.
      ELSEIF LW_DATA-EFFECTIVE_OUT < IT_DATA-EFFECTIVE_OUT.
        MODIFY IT_DATA INDEX L_INDEX.
      ENDIF.
    ELSE.
      IF IT_DATA-EFFECTIVE_OUT < LW_DATA-EFFECTIVE_OUT.
        MODIFY IT_DATA INDEX L_INDEX.
      ENDIF.
    ENDIF.
  ELSE.
    APPEND IT_DATA.
  ENDIF.
  REFRESH: IT_WULTB.
  CLEAR: LW_DATA, L_LESS.
ENDFORM.                    " WHERE_USED_MAT_MULTI
*&---------------------------------------------------------------------*
*&      Form  FINAL_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FINAL_PROCESS.
  DATA: L_DATE LIKE SY-DATUM,
        LGR_DATE LIKE SY-DATUM,
        LBF_DATE LIKE SY-DATUM,
        L_PRE_FIRST_DATE LIKE SY-DATUM,
        L_TIME LIKE SY-UZEIT,
        L_MATKL LIKE MARA-MATKL,
        L_MTART LIKE MARA-MTART,
        L_COUNT_P001 LIKE ZTMM_OVERSTOCK-SEQ,
        L_COUNT_E001 LIKE ZTMM_OVERSTOCK-SEQ,
        L_COUNT_TOTAL LIKE ZTMM_OVERSTOCK-SEQ.


  L_DATE = SY-DATUM.
  L_TIME = SY-UZEIT.

  SORT IT_DATA BY MATNR EFFECTIVE_OUT DESCENDING.
  DELETE ADJACENT DUPLICATES FROM IT_DATA COMPARING MATNR.

  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_OVERSTOCK.

    READ TABLE IT_MBEW WITH KEY MATNR = IT_DATA-MATNR
*                                WERKS = IT_DATA-WERKS
                                BINARY SEARCH.

    IT_OVERSTOCK-BKLAS = IT_MBEW-BKLAS.
    IT_OVERSTOCK-MTART = IT_MBEW-MTART.
    IT_OVERSTOCK-MSTAE = IT_MBEW-MSTAE.
    IT_OVERSTOCK-MATKL = IT_MBEW-MATKL.
    IT_OVERSTOCK-BDMNG = IT_MBEW-BDMNG.

    CLEAR IT_STOCK.
    READ TABLE IT_STOCK WITH KEY MATNR = IT_DATA-MATNR
                        BINARY SEARCH.

    IT_OVERSTOCK-LBKUM = IT_STOCK-LBKUM.
    IT_OVERSTOCK-SALK3 = IT_STOCK-SALK3.
    IT_OVERSTOCK-DISPO = IT_STOCK-DISPO.

    IF IT_DATA-ASSY IS INITIAL.

    ELSE.
*    SELECT SINGLE VTEXT MAKTX LIFNR INTO
*     (IT_OVERSTOCK-MODEL, IT_OVERSTOCK-ASSY_DESC, IT_OVERSTOCK-LIFNR)
*     FROM MARA AS A
*     INNER JOIN T179T AS B
*     ON A~PRDHA = B~PRODH
*     INNER JOIN MAKT AS C
*     ON A~MATNR = C~MATNR
*     INNER JOIN EORD AS D
*     ON A~MATNR = D~MATNR
*     WHERE A~MATNR = IT_DATA-MATNR
*       AND WERKS = 'P001'
*       AND B~SPRAS = 'EN'
*       AND C~SPRAS = 'EN'
*       AND VDATU <= SY-DATUM
*       AND BDATU >= SY-DATUM.


      SELECT SINGLE MTART MATKL MAKTX
        INTO (L_MTART, L_MATKL, IT_OVERSTOCK-ASSY_DESC)
        FROM MARA AS A INNER JOIN MAKT AS C
                               ON A~MATNR = C~MATNR
*         left JOIN T179T AS B
*         ON A~PRDHA = B~PRODH
       WHERE A~MATNR = IT_DATA-ASSY
         AND C~SPRAS = 'EN'.

      IF L_MTART = 'FERT'.
        SELECT SINGLE VTEXT
          INTO IT_OVERSTOCK-MODEL
          FROM MARA AS A INNER JOIN T179T AS B
                                 ON A~PRDHA = B~PRODH
         WHERE MATNR = IT_DATA-ASSY
           AND SPRAS = 'EN'.

      ELSE.
        IT_OVERSTOCK-MODEL = IT_DATA-ASSY.
      ENDIF.
    ENDIF.

    SELECT SINGLE LIFNR
      INTO IT_OVERSTOCK-LIFNR
      FROM MARA AS A INNER JOIN EORD AS D
                             ON A~MATNR = D~MATNR
     WHERE A~MATNR = IT_DATA-MATNR
       AND WERKS = 'P001'
       AND VDATU <= SY-DATUM
       AND BDATU >= SY-DATUM.

* IT_OVERSTOCK-MODEL, IT_OVERSTOCK-ASSY_DESC
    IT_OVERSTOCK-ZSDAT = L_DATE.
    IT_OVERSTOCK-ZSTIM = L_TIME.
    APPEND IT_OVERSTOCK.
    CLEAR: IT_OVERSTOCK.
  ENDLOOP.

** Changed by Furong on 10/27/10
  SORT IT_OVERSTOCK BY SALK3 DESCENDING.
  L_COUNT_P001 = '00001'.
  L_COUNT_E001 = '00001'.
  L_COUNT_TOTAL = '00001'.

  CONCATENATE SY-DATUM+0(6) '01' INTO L_PRE_FIRST_DATE.
  L_PRE_FIRST_DATE = L_PRE_FIRST_DATE - 1.
  CONCATENATE L_PRE_FIRST_DATE+0(6) '01' INTO L_PRE_FIRST_DATE.

  LOOP AT IT_OVERSTOCK.
*    IF IT_OVERSTOCK-WERKS = 'P001' AND L_COUNT_P001 <= '00010'.
    CLEAR: LGR_DATE, LBF_DATE.
    SELECT SINGLE LGR_DATE LBF_DATE
      INTO (LGR_DATE, LBF_DATE)
      FROM ZTMM_OVERSTOCK
     WHERE MATNR = IT_OVERSTOCK-MATNR
       AND WERKS = IT_OVERSTOCK-WERKS
       AND MTART = IT_OVERSTOCK-MTART.

    IF LGR_DATE IS INITIAL.
      SELECT SINGLE MAX( ZBUDAT )
        INTO IT_OVERSTOCK-LGR_DATE
        FROM MSEG
       WHERE MATNR = IT_OVERSTOCK-MATNR
*        AND WERKS = IT_OVERSTOCK-WERKS
         AND BWART = '101'.
    ELSE.
*      IF LGR_DATE < L_PRE_FIRST_DATE.
*        LGR_DATE = L_PRE_FIRST_DATE.
*      ENDIF.
      SELECT SINGLE MAX( ZBUDAT )
        INTO IT_OVERSTOCK-LGR_DATE
        FROM MSEG
       WHERE ZBUDAT BETWEEN LGR_DATE AND SY-DATUM
         AND MATNR = IT_OVERSTOCK-MATNR
*        AND WERKS = IT_OVERSTOCK-WERKS
         AND BWART = '101'.
      IF SY-SUBRC <> 0.
        IT_OVERSTOCK-LGR_DATE = LGR_DATE.
      ENDIF.
    ENDIF.

    IF LBF_DATE IS INITIAL.
      SELECT SINGLE MAX( ZBUDAT )
        INTO IT_OVERSTOCK-LBF_DATE
        FROM MSEG
       WHERE MATNR = IT_OVERSTOCK-MATNR
*        AND WERKS = IT_OVERSTOCK-WERKS
         AND BWART = '261'.
    ELSE.
*      IF LBF_DATE < L_PRE_FIRST_DATE.
*        LBF_DATE = L_PRE_FIRST_DATE.
*      ENDIF.
      SELECT SINGLE MAX( ZBUDAT )
        INTO IT_OVERSTOCK-LBF_DATE
        FROM MSEG
       WHERE ZBUDAT BETWEEN LBF_DATE AND SY-DATUM
         AND MATNR = IT_OVERSTOCK-MATNR
*        AND WERKS = IT_OVERSTOCK-WERKS
         AND BWART = '261'.
      IF SY-SUBRC <> 0.
        IT_OVERSTOCK-LBF_DATE = LBF_DATE.
      ENDIF.

    ENDIF.
*      L_COUNT_P001 =     L_COUNT_P001 + 1.
*    ENDIF.
*    IF IT_OVERSTOCK-WERKS = 'E001' AND L_COUNT_E001 <= '00010'.
*      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OVERSTOCK-LGR_DATE
*      FROM MSEG
*      WHERE MATNR = IT_OVERSTOCK-MATNR
**        AND WERKS = IT_OVERSTOCK-WERKS
*        AND BWART = '101'.
*      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OVERSTOCK-LBF_DATE
*       FROM MSEG
*       WHERE MATNR = IT_OVERSTOCK-MATNR
**         AND WERKS = IT_OVERSTOCK-WERKS
*         AND BWART = '261'.
*      L_COUNT_E001 =     L_COUNT_E001 + 1.
*    ENDIF.

    IT_OVERSTOCK-SEQ =     L_COUNT_TOTAL.
    L_COUNT_TOTAL =     L_COUNT_TOTAL + 1.
    MODIFY IT_OVERSTOCK.
    CLEAR: IT_OVERSTOCK.
  ENDLOOP.

** End of change

ENDFORM.                    " FINAL_PROCESS
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA_NO_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA_NO_BOM.
  DESCRIBE TABLE IT_MARA LINES W_LINES.

  IF W_LINES > 0.

    LOOP AT IT_MARA.
      MOVE-CORRESPONDING IT_MARA TO IT_DATA.
      APPEND IT_DATA.
    ENDLOOP.

    PERFORM FINAL_PROCESS.
  ENDIF.

ENDFORM.                    " PROCESS_DATA_NO_BOM
*&---------------------------------------------------------------------*
*&      Form  GET_LTP_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LTP_MATERIAL .

  DATA: LT_MBEW LIKE IT_MBEW OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF LT_LTP OCCURS 0,
          MATNR   LIKE   MDSM-MATNR,
          WERKS   LIKE   MDSM-WERKS,
          BDMNG   LIKE   MDSM-BDMNG,
        END   OF LT_LTP.

  SELECT A~MTART A~MATNR A~MSTAE A~MATKL "B~DISPO B~LGPRO
         C~BKLAS
         SUM( C~SALK3 ) AS SALK3
         SUM( C~LBKUM ) AS LBKUM
    INTO CORRESPONDING FIELDS OF TABLE LT_MBEW
    FROM MARA AS A INNER JOIN MARC AS B
                      ON B~MATNR = A~MATNR
                   INNER JOIN MBEW AS C
                      ON C~MATNR = B~MATNR
                     AND C~BWKEY = B~WERKS
   WHERE A~MTART IN ('ROH','ROH1')
     AND A~MATNR IN S_MATRN
     AND A~MSTAE NOT IN ('11','OB')
     AND C~LBKUM >  0
     AND EXISTS ( SELECT * FROM MDSM WHERE PLSCN = '900'
                                       AND MATNR = B~MATNR
                                       AND WERKS = B~WERKS )
   GROUP BY A~MTART A~MATNR A~MSTAE A~MATKL "B~DISPO B~LGPRO
            C~BKLAS.

  CHECK SY-SUBRC EQ 0.

  SELECT A~MATNR SUM( C~BDMNG ) AS BDMNG
    INTO CORRESPONDING FIELDS OF TABLE LT_LTP
    FROM MARA AS A INNER JOIN MARC AS B
                      ON B~MATNR = A~MATNR
                   INNER JOIN MDSM AS C
                      ON C~PLSCN = '900'
                     AND C~MATNR = B~MATNR
                     AND C~WERKS = B~WERKS
   WHERE A~MTART IN ('ROH','ROH1')
     AND A~MATNR IN S_MATRN
     AND A~MSTAE NOT IN ('11','OB')
*   GROUP BY A~MATNR B~WERKS.
   GROUP BY A~MATNR.

*  SORT LT_LTP BY MATNR WERKS.
  SORT LT_LTP BY MATNR.

  LOOP AT LT_MBEW.
    READ TABLE LT_LTP WITH KEY MATNR = LT_MBEW-MATNR
** Changed by Park On 11/20/13
*                               WERKS = LT_MBEW-WERKS
** End of change 11/20/13
                      BINARY SEARCH.
    IF SY-SUBRC NE 0.
      APPEND LT_MBEW TO IT_MBEW.

    ELSE.
      IF LT_MBEW-LBKUM > LT_LTP-BDMNG.
        LT_MBEW-BDMNG = LT_LTP-BDMNG.
        APPEND LT_MBEW TO IT_MBEW.
      ENDIF.
    ENDIF.

  ENDLOOP.
** End of change 11/20/13

ENDFORM.                    " GET_LTP_MATERIAL
