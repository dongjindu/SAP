************************************************************************
* Program Name      : ZZPPR_DELAY_VEH_MANAGEMENT
* Creation Date     : 04/2013
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZPPR_DELAY_VEH_MANAGEMENT NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.
INCLUDE: <ICON>.
TABLES: ZTPP_VM.

TABLES: ZTPP_DELAY_VEH, ZTPP_DELAY_SECT, T001W.

DATA: BEGIN OF IT_SECTION OCCURS 0.
        INCLUDE STRUCTURE ZTPP_DELAY_SECT.
DATA:   LIMIT_TIME(14),
        FIELD_IDX(2) TYPE N,

      END   OF IT_SECTION.

DATA: IT_DELAY      LIKE ZTPP_DELAY_VEH  OCCURS 0 WITH HEADER LINE,
      IT_DELAY_TMP  LIKE ZTPP_DELAY_VEH  OCCURS 0 WITH HEADER LINE,
      IT_DELAY_PREV LIKE ZTPP_DELAY_VEH  OCCURS 0 WITH HEADER LINE,
      IT_SYS_FDBACK LIKE ZTPP_DELAY_IF   OCCURS 0 WITH HEADER LINE,
      IT_STRACK     LIKE ZTPP_SIDE_TRACK OCCURS 0 WITH HEADER LINE,
      IT_DELAY_IF   LIKE ZTPP_DELAY_IF   OCCURS 0 WITH HEADER LINE,
      IT_UM         LIKE ZTSD_UM         OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF ST_HISTORY.
        INCLUDE STRUCTURE ZTPP_DELAY_VEH.
DATA: ZSECTX LIKE ZTPP_DELAY_SECT-ZSECTX,
      ZITIME_OUT(14),
      END OF ST_HISTORY.

DATA: IT_HISTORY LIKE ST_HISTORY OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_WORK_TIME OCCURS 0,
        SHOP   TYPE   ARBPL.
        INCLUDE STRUCTURE ZSMM_WORKING_TIME.
DATA: END   OF IT_WORK_TIME.

CONSTANTS: C_RP_STATUS LIKE CABN-ATNAM   VALUE 'P_RP_STATUS',
           C_USAGE_CAR LIKE CABN-ATNAM   VALUE 'P_USAGE_CAR',
           C_WORDER    LIKE CABN-ATNAM   VALUE 'P_WORK_ORDER',
           C_MI        LIKE CABN-ATNAM   VALUE 'P_MI',
           C_OCN       LIKE CABN-ATNAM   VALUE 'P_OCN',
           C_DEALER_NO   LIKE CABN-ATNAM VALUE 'P_DEALER_NO',
           C_RP21      LIKE CABN-ATNAM   VALUE 'P_RP21_SHOP_DATE',
           C_EXTC      LIKE CABN-ATNAM   VALUE 'P_EXT_COLOR',
           C_INTC      LIKE CABN-ATNAM   VALUE 'P_INT_COLOR'.

DATA: W_RP_STATUS LIKE CABN-ATINN,
      W_USAGE_CAR LIKE CABN-ATINN,
      W_WORDER    LIKE CABN-ATINN,
      W_MI        LIKE CABN-ATINN,
      W_OCN       LIKE CABN-ATINN,
      W_DEALER_NO LIKE CABN-ATINN,
      W_RP21      LIKE CABN-ATINN,
      W_EXTC      LIKE CABN-ATINN,
      W_INTC      LIKE CABN-ATINN,
      W_CURRENT_SHOP_DATE LIKE SY-DATUM,
      W_SYS_DATE  LIKE SY-DATUM,
      W_SYS_TIME  LIKE SY-UZEIT,
      W_PREV_DATE LIKE SY-DATUM,
      W_PREV_TIME LIKE SY-UZEIT,
      W_CURR_TIME(14),
      W_MODE      LIKE SY-UCOMM,
      OK_CODE LIKE SY-UCOMM.

DATA: V_BDATE_B      LIKE   SY-DATUM,
      V_BDATE_P      LIKE   SY-DATUM,
      V_BDATE_T      LIKE   SY-DATUM.

*DATA: BEGIN OF it_wip OCCURS 0,
*        objek         LIKE   ausp-objek,
*        klart         LIKE   ausp-klart,
*        rpid          LIKE   ausp-atwrt,
*        usage         LIKE   ausp-atwrt,
*        worder        LIKE   ausp-atwrt,
*        mi            LIKE   ausp-atwrt,
*        ocn           LIKE   ausp-atwrt,
*        atinn_input   LIKE   ausp-atinn,
*        zitime(14),
*        zsecid        LIKE   ztpp_delay_sect-zsecid,
*      END   OF it_wip.

*---// Grid ITAB
DATA: BEGIN OF ST_HEADER,
        LINES(1),
        KEY(15),
        TOTAL TYPE I,
        01QTY TYPE I,
        02QTY TYPE I,
        03QTY TYPE I,
        04QTY TYPE I,
        05QTY TYPE I,
        06QTY TYPE I,
        07QTY TYPE I,
        08QTY TYPE I,
        09QTY TYPE I,
        10QTY TYPE I,
        11QTY TYPE I,
        12QTY TYPE I,
        13QTY TYPE I,
        14QTY TYPE I,
        15QTY TYPE I,
        16QTY TYPE I,
        17QTY TYPE I,
        18QTY TYPE I,
        19QTY TYPE I,
        20QTY TYPE I,
        21QTY TYPE I,
        22QTY TYPE I,
        23QTY TYPE I,
        24QTY TYPE I,
        25QTY TYPE I,
        26QTY TYPE I,
        27QTY TYPE I,
      END   OF ST_HEADER.

DATA: IT_HEADER LIKE ST_HEADER OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_HEADER_TXT OCCURS 0,
        INDEX(2) TYPE N,
        ZSECID   LIKE ZTPP_DELAY_SECT-ZSECID,
        ZSECTX   LIKE ZTPP_DELAY_SECT-ZSECTX,
      END   OF IT_HEADER_TXT.

DATA: BEGIN OF ST_DETAIL,
        ZSECID      LIKE ZTPP_DELAY_SECT-ZSECID,
        SEQ         LIKE ZTPP_DELAY_SECT-SEQ,
        ZSECTX      LIKE ZTPP_DELAY_SECT-ZSECTX,
        MODEL       LIKE ZTPP_VM-MODEL_CODE,
        BODY_NO     LIKE ZTPP_VM-BODY_NO,
        RP_STATUS   LIKE ZTPP_DELAY_IF-RP_STATUS,
        ZRPFR       LIKE ZTPP_DELAY_SECT-ZRPFR,
        ZRPTO       LIKE ZTPP_DELAY_SECT-ZRPTO,
      " ztpp_vm-rp_cstatus,
        PROGRESS    LIKE ZTPP_STATUS-PROGRESS,
        USAGE       LIKE ZTPP_VM-USG_CAR,
        EXTC        LIKE ZTPP_VM-EXTC,
        INTC        LIKE ZTPP_VM-INTC,
*        delay_hr    LIKE ztpp_delay_sect-zdura,
        ZETA        LIKE ZTPP_DELAY_VEH-ZETA,
        ZESOFF      LIKE ZTPP_DELAY_VEH-ZESOFF,
        URGENCY     LIKE ZTSD_UM-URGENCY,
        URGCDATE    LIKE ZTSD_UM-URGCDATE,
        DEALER_NO   LIKE ZTPP_VM-DEALER_NO,
        RP21_ATFLV  LIKE AUSP-ATFLV,
        ALLOC_DATE  LIKE SY-DATUM,
        FLET        LIKE ZTSD_UM-FLET,
        ZSFDBK      LIKE ZTPP_DELAY_VEH-ZSFDBK,
        ZLFDBK      LIKE ZTPP_DELAY_VEH-ZLFDBK,
        ZSYSTEM     LIKE ZTPP_DELAY_VEH-ZSYSTEM,
        ZPFDBK      LIKE ZTPP_DELAY_VEH-ZPFDBK,
        ZSTTXT      LIKE ZTPP_DELAY_VEH-ZSTTXT,
        ZITIME      LIKE ZTPP_DELAY_VEH-ZTTIME,
        ZITIME_OUT(14),
        WORDER      LIKE MARA-MATNR,
        MI          LIKE ZTPP_VM-MI,
        OCN         LIKE ZTPP_VM-OCN,
        OBJEK       LIKE AUSP-OBJEK,
        RPID        LIKE AUSP-ATWRT,
        ATINN_INPUT LIKE CABN-ATINN,
        KLART       LIKE AUSP-KLART,
        ZDRDY       LIKE ZTPP_DELAY_SECT-ZDRDY,
        ZDHOUR      LIKE ZTPP_DELAY_VEH-ZDHOUR,
        ZTTIME      LIKE ZTPP_DELAY_VEH-ZTTIME,
        INDEX       TYPE I,
        CELLTAB     TYPE LVC_T_STYL,
      END OF ST_DETAIL.

DATA: IT_DETAIL LIKE ST_DETAIL OCCURS 0 WITH HEADER LINE.

**---// Internal Tables
DATA: IT_WIP       LIKE ST_DETAIL OCCURS 0 WITH HEADER LINE,
      IT_WIP_DELAY LIKE ST_DETAIL OCCURS 0 WITH HEADER LINE.

*---// Global variable
DATA: V_MODEL LIKE ZTPP_VM-MODEL_CODE,
      V_SEQ,
      V_SHOP_DATE LIKE SY-DATUM,

** Changed by Park On 11/22/13
*      V_DELHR(5) TYPE N,
** End of change 11/22/13

      V_USAGE(1),
      W_SHOP_DATE_BK LIKE SY-DATUM,
      V_MODEL_200  LIKE ZTPP_VM-MODEL_CODE,
      V_BODY_NO_200 LIKE ZTPP_VM-BODY_NO.

*---// Constants
CONSTANTS: C_WIP   TYPE STRING VALUE 'Curr WIP Qty',
           C_TOTAL TYPE STRING VALUE 'Delay Total QTY'.

*---// Ranges
RANGES: R_BODY FOR ZTPP_VM-BODY_NO,
        R_NATN FOR ZTPP_VM-WO_NATION.

*---// Button
DATA: G_BODY_EXT(4)  VALUE ICON_ENTER_MORE.
DATA: G_NATN_EXT(4)  VALUE ICON_ENTER_MORE.

*<<< LIST BOX DATA >>>*
TYPE-POOLS VRM.
DATA: IT_LIST  TYPE VRM_VALUES,
      ST_VALUE LIKE LINE OF IT_LIST.

*-----/// Grid Control : START
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

* Splitter Declaration
DATA: WC_SPLITTER_0100     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      WC_SP_CONTAINER_0100 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WC_CONTROL_0100      TYPE        SCRFNAME VALUE 'CC_0100'.

* Declare for Grid
DATA: WC_CONTAINER_HEADER   TYPE REF TO CL_GUI_CONTAINER,
      WC_GRID_HEADER        TYPE REF TO CL_GUI_ALV_GRID,
      V_LAYOUT_HEADER       TYPE LVC_S_LAYO,
      V_VARIANT_HEADER      TYPE DISVARIANT,
      IT_SORT_HEADER        TYPE LVC_T_SORT WITH HEADER LINE,
      IT_FILTER_HEADER      TYPE LVC_T_FILT WITH HEADER LINE.

DATA: WC_CONTAINER_DETAIL    TYPE REF TO CL_GUI_CONTAINER,
      WC_GRID_DETAIL         TYPE REF TO CL_GUI_ALV_GRID,
      V_LAYOUT_DETAIL        TYPE LVC_S_LAYO,
      V_VARIANT_DETAIL       TYPE DISVARIANT,
      IT_SORT_DETAIL         TYPE LVC_T_SORT WITH HEADER LINE,
      IT_FILTER_DETAIL       TYPE LVC_T_FILT WITH HEADER LINE.

DATA: WC_SPLITTER_0200     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      WC_SP_CONTAINER_0200 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WC_CONTROL_0200      TYPE        SCRFNAME VALUE 'CC_0200'.


DATA: WC_CONTAINER_HISTORY  TYPE REF TO CL_GUI_CONTAINER,
      WC_GRID_HISTORY       TYPE REF TO CL_GUI_ALV_GRID,
      V_LAYOUT_HISTORY      TYPE LVC_S_LAYO,
      V_VARIANT_HISTORY     TYPE DISVARIANT,
      IT_SORT_HISTORY       TYPE LVC_T_SORT WITH HEADER LINE,
      IT_FILTER_HISTORY     TYPE LVC_T_FILT WITH HEADER LINE.


CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Interal tables for ALV GRID
DATA : IT_ROWS         TYPE LVC_T_ROW  WITH HEADER LINE,
       IT_ROW_NO       TYPE LVC_T_ROID WITH HEADER LINE,
       IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV.

* Global variable for ALV GRID
DATA : V_FIELDNAME   TYPE LINE OF SLIS_T_FIELDCAT_ALV,
       V_REPID       LIKE SY-REPID,
       V_CNT         TYPE I,                   "Field count
       V_SCROLL      TYPE LVC_S_STBL,
       V_SAVE        TYPE C   VALUE 'A'.       "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: V_CONTAINER(100),
      V_CONTROL(100),
      V_SPLITTER(100),
      V_GRID(100),
      V_ITAB(100),
      V_STRUCTURE LIKE DD02L-TABNAME.

FIELD-SYMBOLS: <FS_CONTAINER> TYPE REF TO   CL_GUI_CUSTOM_CONTAINER,
               <FS_CONTROL>   TYPE          SCRFNAME,
               <FS_SPLITTER>  TYPE REF TO   CL_GUI_SPLITTER_CONTAINER,
               <FS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID,
               <FS_ITAB>      TYPE STANDARD TABLE,
               <FS_ITAB_OLD>  TYPE STANDARD TABLE.

CONSTANTS: C_STRUCTURE(100) VALUE 'ST_'.

DATA : G_CC100        TYPE  SCRFNAME VALUE 'CC100',
       GS_VARIANT     LIKE  DISVARIANT,
       GS_PRINT       TYPE  LVC_S_PRNT,
       GS_LAYOUT      TYPE  LVC_S_LAYO,
       GT_FIELDCAT    TYPE  LVC_T_FCAT,
       GT_FCAT_DETAIL TYPE  LVC_T_FCAT,
       GS_FIELDCAT    TYPE  LVC_S_FCAT,
       GS_SORT        TYPE  LVC_S_SORT,
       GT_SORT        TYPE  LVC_T_SORT,
       GT_F4          TYPE  LVC_T_F4,
       GS_F4          TYPE  LVC_S_F4,
       GT_EXCLUDE     TYPE  UI_FUNCTIONS.

DATA : GRID100              TYPE REF TO CL_GUI_ALV_GRID,
       CONTAINER100         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_EVENT_RECEIVER     TYPE REF TO LCL_EVENT_RECEIVER.

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS: HOTSPOT_HEADER
        FOR  EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
             IMPORTING E_ROW_ID
                      E_COLUMN_ID
                      ES_ROW_NO.

    METHODS: CLICK_DETAIL
        FOR  EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
             IMPORTING E_ROW_ID
                       E_COLUMN_ID
                       ES_ROW_NO.

    METHODS: DBLCLK_DETAIL
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW
                      E_COLUMN
                      ES_ROW_NO.

    METHODS: HANDLE_TOOLBAR_HEADER
        FOR  EVENT TOOLBAR OF CL_GUI_ALV_GRID
             IMPORTING E_OBJECT E_INTERACTIVE.

    METHODS: DETAIL_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED.

*    METHODS: handle_user_command_prog_sum
*             FOR EVENT user_command OF cl_gui_alv_grid
*             IMPORTING e_ucomm.
    DATA: ERROR_IN_DATA TYPE C.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HOTSPOT_HEADER.
    PERFORM HOTSPOT_HEADER USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                           "handle_double_click

  METHOD CLICK_DETAIL.
*    PERFORM CLICK_WO_DETAIL USING E_ROW_ID
*                                  E_COLUMN_ID
*                                  ES_ROW_NO.
  ENDMETHOD.                           "handle_double_click

  METHOD DBLCLK_DETAIL.
*    PERFORM DBL_CLICK_WO_DETAIL USING E_COLUMN-FIELDNAME
*                                      ES_ROW_NO-ROW_ID.
  ENDMETHOD.                           "handle_double_click
  METHOD HANDLE_TOOLBAR_HEADER.
*    PERFORM HANDLE_TOOLBAR_PROG_SUM USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

  METHOD DETAIL_CHANGED.

    DATA: LS_GOOD TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          W_QTY(13),
          LVC_T_ROW TYPE LVC_T_ROW,
          LW_OUTPUT LIKE IT_DETAIL,
          LV_ZETA   LIKE ZTPP_DELAY_VEH-ZETA,
          LV_ZESOFF LIKE ZTPP_DELAY_VEH-ZESOFF.

    ERROR_IN_DATA = SPACE.
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.

      CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = LS_GOOD-FIELDNAME
        IMPORTING
          E_VALUE     = LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = LS_GOOD-FIELDNAME
          I_VALUE     = LV_VALUE.

      CHECK LS_GOOD-FIELDNAME EQ 'ZETA'.

      LV_ZETA   = LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'ZDRDY'
        IMPORTING
          E_VALUE     = LV_VALUE.

      PERFORM GET_ESTIMATED_SOFF USING LV_ZETA LV_VALUE LV_ZESOFF.

      LV_VALUE  = LV_ZESOFF.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'ZESOFF'
          I_VALUE     = LV_VALUE.

    ENDLOOP.

*§7.Display application log if an error has occured.
    IF ERROR_IN_DATA EQ 'X'.
      CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    ENDIF.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED
*  METHOD handle_user_command_prog_sum.
*    PERFORM handle_user_command_prog_sum USING  e_ucomm.
*  ENDMETHOD.                           "handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

START-OF-SELECTION.
  PERFORM INITIALIZATION.
*  PERFORM refresh_rtn.

  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  DATA: L_PARA(20).

  CASE W_MODE.
    WHEN 'DISPLAY'.
      WRITE: W_PREV_DATE TO L_PARA(10),
             W_PREV_TIME TO L_PARA+11(8).
    WHEN 'REFRESH'.
      WRITE: W_SYS_DATE TO L_PARA(10),
             W_SYS_TIME TO L_PARA+11(8).
  ENDCASE.

  CASE SY-DYNNR.
    WHEN 100.
      SET PF-STATUS '100'.
      SET TITLEBAR  '100' WITH L_PARA.
    WHEN 200.
      SET PF-STATUS '200'.
      SET TITLEBAR  '200' WITH L_PARA.
  ENDCASE.

ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_CONTROL OUTPUT.
  PERFORM CREATE_SPLITTER USING '0100' 2.
  PERFORM CREATE_GRID     USING 'HEADER' '1' '1'.
  PERFORM CREATE_GRID     USING 'DETAIL' '2' '1'.
ENDMODULE.                 " CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_SPLITTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0377   text
*----------------------------------------------------------------------*
FORM CREATE_SPLITTER USING PV_DYNNR P_ROW.
  CONCATENATE: 'WC_SP_CONTAINER_' PV_DYNNR INTO V_CONTAINER.
  ASSIGN:      (V_CONTAINER)               TO   <FS_CONTAINER>.

  CONCATENATE: 'WC_CONTROL_' PV_DYNNR INTO V_CONTROL.
  ASSIGN:      (V_CONTROL)            TO   <FS_CONTROL>.

  CONCATENATE: 'WC_SPLITTER_' PV_DYNNR INTO V_SPLITTER.
  ASSIGN:      (V_SPLITTER)            TO   <FS_SPLITTER>.


  IF <FS_CONTAINER> IS INITIAL.
    CREATE OBJECT <FS_CONTAINER>
      EXPORTING
        CONTAINER_NAME              = <FS_CONTROL>
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT <FS_SPLITTER>
      EXPORTING
        PARENT            = <FS_CONTAINER>
        ROWS              = P_ROW "2
        COLUMNS           = 1
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    " CREATE_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0381   text
*      -->P_0382   text
*      -->P_0383   text
*----------------------------------------------------------------------*
FORM CREATE_GRID USING PV_GRID PV_ROW PV_COLUMN.
  DATA: LV_CONTAINER(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO CL_GUI_CONTAINER.

  CONCATENATE: 'WC_CONTAINER_' PV_GRID INTO LV_CONTAINER.
  ASSIGN:      (LV_CONTAINER)          TO   <LFS_CONTAINER>.

  IF <LFS_CONTAINER> IS INITIAL.
    PERFORM CREATE_GRID_CONTAINER USING PV_GRID PV_ROW PV_COLUMN.
    PERFORM BUILD_FIELD_CATALOG   USING PV_GRID.
    PERFORM SET_ATTRIBUTE         USING PV_GRID.
    PERFORM SELECT_EDIT_FIELDS.
    PERFORM ASSIGN_ITAB_TO_ALV    USING PV_GRID.
    PERFORM ASSIGN_EVENT          USING PV_GRID.
  ELSE.
    IF PV_GRID = 'DETAIL'.
      PERFORM SELECT_EDIT_FIELDS.
    ENDIF.
    PERFORM BUILD_FIELD_CATALOG   USING PV_GRID.
    PERFORM ASSIGN_ITAB_TO_ALV    USING PV_GRID.
*    PERFORM refresh_grid USING pv_grid.
  ENDIF.

ENDFORM.                    " CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*      -->P_PV_ROW  text
*      -->P_PV_COLUMN  text
*----------------------------------------------------------------------*
FORM CREATE_GRID_CONTAINER USING PV_GRID PV_ROW PV_COLUMN.
  DATA: LV_CONTAINER(100),
        LV_GRID(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO   CL_GUI_CONTAINER,
                 <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_SPLITTER_' PV_GRID  INTO V_SPLITTER.
  ASSIGN:      (V_SPLITTER)            TO   <FS_SPLITTER>.

  CONCATENATE: 'WC_CONTAINER_' PV_GRID INTO LV_CONTAINER.
  ASSIGN:      (LV_CONTAINER)          TO   <LFS_CONTAINER>.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CALL METHOD <FS_SPLITTER>->GET_CONTAINER
    EXPORTING
      ROW       = PV_ROW
      COLUMN    = PV_COLUMN
    RECEIVING
      CONTAINER = <LFS_CONTAINER>.

  CREATE OBJECT <LFS_GRID>
    EXPORTING
      I_PARENT          = <LFS_CONTAINER>
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CASE PV_ROW.
    WHEN 1.
      CALL METHOD <FS_SPLITTER>->SET_ROW_HEIGHT
        EXPORTING
          ID                = PV_ROW
          HEIGHT            = 9
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    WHEN 2.
      CALL METHOD <FS_SPLITTER>->SET_ROW_HEIGHT
        EXPORTING
          ID                = PV_ROW
          HEIGHT            = 36
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
  ENDCASE.

ENDFORM.                    " CREATE_GRID_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING PV_GRID.
*--- adjust field catalog to suppress the output of already
*  displayed key fields of structure

  PERFORM SET_FIELDNAME USING PV_GRID.
  PERFORM SET_SCREEN_FIELDS USING PV_GRID.
ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_FIELDNAME  USING PV_GRID.

  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  MOVE: SY-REPID TO V_REPID.
  CONCATENATE C_STRUCTURE PV_GRID INTO LW_ITAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = V_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = V_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

ENDFORM.                    " SET_FIELDNAME
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS USING PV_GRID.
  CASE PV_GRID.
    WHEN 'HEADER'.
      PERFORM SET_SCREEN_FIELDS_HEADER.
    WHEN 'DETAIL'.
      PERFORM SET_SCREEN_FIELDS_DETAIL.
    WHEN 'HISTORY'.
      PERFORM SET_SCREEN_FIELDS_HISTORY.
  ENDCASE.
ENDFORM.                    " SET_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_HEADER .
  DATA: L_QTY(30).

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                  'S' 'KEY'          ' ',
                                  ' ' 'COLTEXT'      'Model',
                                  ' ' 'OUTPUTLEN'    '15',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'TOTAL'          ' ',
                                  ' ' 'COLTEXT'      'Total',
                                  ' ' 'OUTPUTLEN'    '6',
                                  ' ' 'FIX_COLUMN'   'X',
                                  ' ' 'HOTSPOT'      'X',
                                  'E' 'EMPHASIZE'    'C100'.

  LOOP AT IT_HEADER_TXT.
    CONCATENATE IT_HEADER_TXT-INDEX 'QTY' INTO L_QTY.
    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                   'S' L_QTY       ' ',
                                   ' ' 'COLTEXT'   IT_HEADER_TXT-ZSECTX,
                                   ' ' 'OUTPUTLEN' '10',
                                   ' ' 'HOTSPOT'   'X',
                                   'E' 'EMPHASIZE' 'C250'.
  ENDLOOP.
ENDFORM.                    " SET_SCREEN_FIELDS_HEADER
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_DETAIL .
  IF IT_HEADER-LINES = '1'.
    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                    'S' 'INDEX'        ' ',
                                    ' ' 'COLTEXT'      'Seq',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '6',

                                    'S' 'ZSECTX'        ' ',
                                    ' ' 'COLTEXT'      'Section',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '15',

                                    'S' 'MODEL'       ' ',
                                    ' ' 'COLTEXT'      'Model',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '3',

                                    'S' 'BODY_NO'      ' ',
                                    ' ' 'COLTEXT'      'Body No',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '7',

                                    'S' 'RP_STATUS'     ' ',
                                    ' ' 'COLTEXT'      'RP',
                                    'E' 'OUTPUTLEN'    '2',

                                    'S' 'EXTC'         ' ',
                                    ' ' 'COLTEXT'      'EXT',
                                    'E' 'OUTPUTLEN'    '4',

                                    'S' 'INTC'         ' ',
                                    ' ' 'COLTEXT'      'INT',
                                    'E' 'OUTPUTLEN'    '4',

                                    'S' 'URGENCY'    ' ',
                                    ' ' 'COLTEXT'      'Urgcy',
                                    'E' 'OUTPUTLEN'    '5',

                                   'S' 'URGCDATE'       ' ',
                                    ' ' 'COLTEXT'      'Urgcy Date',
                                    'E' 'OUTPUTLEN'    '10',

                                    'S' 'DEALER_NO'    ' ',
                                    ' ' 'COLTEXT'      'Dealer',
                                    'E ' 'OUTPUTLEN'    '6',

                                   'S' 'ALLOC_DATE'       ' ',
                                    ' ' 'COLTEXT'      'Allo. Date',
                                    'E' 'OUTPUTLEN'    '10',

                                    'S' 'FLET'    ' ',
                                    ' ' 'COLTEXT'      'Fleet',
                                    'E' 'OUTPUTLEN'    '4',
*
                                    'S' 'ZITIME_OUT'   ' ',
                                    ' ' 'COLTEXT'      'Input Time',
                               ' ' 'EDIT_MASK'    '__/__/____ __:__:__',
                               'E' 'OUTPUTLEN'    '19',

                                    'S' 'WORDER'       ' ',
                                    ' ' 'COLTEXT'      'Work Order',
                                    'E' 'OUTPUTLEN'    '14',

                                    'S' 'MI'    ' ',
                                    ' ' 'COLTEXT'      'MI',
                                    'E' 'OUTPUTLEN'    '8',

                                   'S' 'OCN'       ' ',
                                    ' ' 'COLTEXT'      'OCN',
                                    'E' 'OUTPUTLEN'    '4'.

  ELSE.
    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                    'S' 'INDEX'        ' ',
                                    ' ' 'COLTEXT'      'Seq',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '6',

                                    'S' 'ZSECTX'        ' ',
                                    ' ' 'COLTEXT'      'Section',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '15',

                                    'S' 'MODEL'       ' ',
                                    ' ' 'COLTEXT'      'Model',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '3',

                                    'S' 'BODY_NO'      ' ',
                                    ' ' 'COLTEXT'      'Body No',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '7',

                                    'S' 'ZDHOUR'       ' ',
                                    ' ' 'COLTEXT'      'D.Hour',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E ' 'OUTPUTLEN'    '8',

                                    'S' 'RP_STATUS'     ' ',
                                    ' ' 'COLTEXT'      'RP',
                                    'E' 'OUTPUTLEN'    '2',

                                    'S' 'ZITIME_OUT'   ' ',
                                    ' ' 'COLTEXT'      'Input Time',
                               ' ' 'EDIT_MASK'    '__/__/____ __:__:__',
                               'E' 'OUTPUTLEN'    '19',

                                    'S' 'EXTC'         ' ',
                                    ' ' 'COLTEXT'      'EXT',
                                    'E' 'OUTPUTLEN'    '4',

                                    'S' 'INTC'         ' ',
                                    ' ' 'COLTEXT'      'INT',
                                    'E' 'OUTPUTLEN'    '4',

                                  'S' 'ZETA'    ' ',
                                  ' ' 'COLTEXT'      'ETA',
                                  'E' 'OUTPUTLEN'    '10',

                                  'S' 'ZESOFF'       ' ',
                                  ' ' 'COLTEXT'      'Est. S/Off',
                                  'E' 'OUTPUTLEN'    '10',

                                    'S' 'URGENCY'    ' ',
                                    ' ' 'COLTEXT'      'Urgcy',
                                    'E' 'OUTPUTLEN'    '5',

                                   'S' 'URGCDATE'       ' ',
                                    ' ' 'COLTEXT'      'Urgcy Date',
                                    'E' 'OUTPUTLEN'    '10',

                                    'S' 'DEALER_NO'    ' ',
                                    ' ' 'COLTEXT'      'Dealer',
                                    'E ' 'OUTPUTLEN'    '6',

                                   'S' 'ALLOC_DATE'       ' ',
                                    ' ' 'COLTEXT'      'Allo. Date',
                                    'E' 'OUTPUTLEN'    '14',

                                    'S' 'FLET'    ' ',
                                    ' ' 'COLTEXT'      'Fleet',
                                    'E' 'OUTPUTLEN'    '4',
*
                               'S' 'ZSFDBK'       ' ',
                                ' ' 'COLTEXT'      'Shop Feedback',
                                'E' 'OUTPUTLEN'    '20',

                                'S' 'ZLFDBK'    ' ',
                                ' ' 'COLTEXT'      'System Feedback',
                                'E' 'OUTPUTLEN'    '20',

                                'S' 'ZPFDBK'       ' ',
                                ' ' 'COLTEXT'      'PC Feedback',
                                'E' 'OUTPUTLEN'    '20',

                                'S' 'ZSTTXT'    ' ',
                                ' ' 'COLTEXT'      'Side Track',
                                'E' 'OUTPUTLEN'    '20',

                                'S' 'WORDER'       ' ',
                                ' ' 'COLTEXT'      'Work Order',
                                'E' 'OUTPUTLEN'    '14',

                                'S' 'MI'    ' ',
                                ' ' 'COLTEXT'      'MI',
                                'E' 'OUTPUTLEN'    '8',

                               'S' 'OCN'       ' ',
                                ' ' 'COLTEXT'      'OCN',
                                'E' 'OUTPUTLEN'    '4'.

  ENDIF.
ENDFORM.                    " SET_SCREEN_FIELDS_DETAIL
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0855   text
*      -->P_0856   text
*      -->P_0857   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT  STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : LV_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO V_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check filed catalog:' P_FIELD.
    ENDIF.

    MOVE: V_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO LV_COL.
  ASSIGN (LV_COL) TO <FS>.
  MOVE   P_VALUE  TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    IF P_FIELDCAT-COL_POS IS INITIAL.
      ADD 1 TO V_CNT.
      P_FIELDCAT-COL_POS = V_CNT.
    ENDIF.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTE USING PV_GRID.
  PERFORM SET_LAYOUT                USING PV_GRID.
  PERFORM SET_VARIANT               USING PV_GRID.
  PERFORM SET_SORT_TOTAL_FIELD      USING PV_GRID.
  PERFORM SET_FILTER                USING PV_GRID.
ENDFORM.                    " SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV USING PV_GRID.
  DATA: LV_GRID(100),
        LV_ITAB(100),
        LV_LAYOUT(100),
        LV_VARIANT(100),
        LV_SORT(100),
        LV_FILTER(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_ITAB>  TYPE STANDARD TABLE,
                 <LFS_LAYOUT> TYPE LVC_S_LAYO,
                 <LFS_VARIANT> TYPE DISVARIANT,
                 <LFS_SORT> TYPE LVC_T_SORT,
                 <LFS_FILTER> TYPE LVC_T_FILT.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'V_LAYOUT_' PV_GRID INTO LV_LAYOUT.
  ASSIGN:      (LV_LAYOUT)         TO   <LFS_LAYOUT>.

  CONCATENATE: 'V_VARIANT_' PV_GRID INTO LV_VARIANT.
  ASSIGN:      (LV_VARIANT)         TO   <LFS_VARIANT>.

  CONCATENATE: 'IT_'      PV_GRID '[]' INTO LV_ITAB.
  ASSIGN:      (LV_ITAB)               TO   <LFS_ITAB>.

  CONCATENATE: 'IT_SORT_' PV_GRID '[]' INTO LV_SORT.
  ASSIGN:      (LV_SORT)               TO   <LFS_SORT>.

  CONCATENATE: 'IT_FILTER_' PV_GRID '[]' INTO LV_FILTER.
  ASSIGN:      (LV_FILTER)               TO   <LFS_FILTER>.

  CONCATENATE: C_STRUCTURE PV_GRID INTO V_STRUCTURE.

  CALL METHOD <LFS_GRID>->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_STRUCTURE_NAME              = V_STRUCTURE
      IS_LAYOUT                     = <LFS_LAYOUT>
      I_SAVE                        = V_SAVE
      IS_VARIANT                    = <LFS_VARIANT>
      I_DEFAULT                     = SPACE
    CHANGING
      IT_FIELDCATALOG               = IT_FIELDCAT[]
      IT_FILTER                     = <LFS_FILTER>
      IT_SORT                       = <LFS_SORT>
      IT_OUTTAB                     = <LFS_ITAB>
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM ASSIGN_EVENT USING PV_GRID.
  DATA: LV_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

*--  Regist event for Edit
  CALL METHOD <LFS_GRID>->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.

  CASE PV_GRID.
    WHEN 'HEADER'.
      SET HANDLER EVENT_RECEIVER->HOTSPOT_HEADER FOR <LFS_GRID>.
*    SET HANDLER event_receiver->handle_toolbar FOR <lfs_grid>.
    WHEN 'DETAIL'.
      SET HANDLER EVENT_RECEIVER->DBLCLK_DETAIL  FOR <LFS_GRID>.
      SET HANDLER EVENT_RECEIVER->CLICK_DETAIL FOR <LFS_GRID>.
      SET HANDLER EVENT_RECEIVER->DETAIL_CHANGED FOR <LFS_GRID>.
  ENDCASE.

  CALL METHOD <LFS_GRID>->SET_TOOLBAR_INTERACTIVE.
ENDFORM.                    " ASSIGN_EVENT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM REFRESH_GRID USING PV_GRID.
  DATA: LV_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  V_SCROLL-ROW = 'X'.
  V_SCROLL-COL = 'X'.

  CALL METHOD <LFS_GRID>->REFRESH_TABLE_DISPLAY
    EXPORTING
*     i_soft_refresh = 'X'
      IS_STABLE      = V_SCROLL.     "## ### ### refresh
ENDFORM.                    " REFRESH_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT USING PV_GRID.
  DATA: LV_GRID(100),
        LV_LAYOUT(100).

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_LAYOUT> TYPE LVC_S_LAYO.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'V_LAYOUT_' PV_GRID INTO LV_LAYOUT.
  ASSIGN:      (LV_LAYOUT)         TO   <LFS_LAYOUT>.

  CALL METHOD <LFS_GRID>->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = <LFS_LAYOUT>.

  IF <LFS_LAYOUT> IS INITIAL.

    CASE PV_GRID.
      WHEN 'HEADER'.
        <LFS_LAYOUT>-EDIT       = ' '.     " Edit Mode Enable
        <LFS_LAYOUT>-SEL_MODE   = 'A'.     " mode for select col and row
        <LFS_LAYOUT>-LANGUAGE   = SY-LANGU." Language Key
        <LFS_LAYOUT>-TOTALS_BEF = 'X'.     " Upper Total Line
        <LFS_LAYOUT>-ZEBRA      = 'X'.     " Emphasize C250
*        <lfs_layout>-cwidth_opt = 'X'.    " optimizes the column width
*        <LFS_LAYOUT>-no_merging = 'X'.        " Disable cell merging
*        <lfs_layout>-totals_bef = 'X'.        " Upper Total Line
*        <lfs_layout>-no_totline = ' '.        " Disable Total Line
        <LFS_LAYOUT>-INFO_FNAME = 'ROW_COLOR'. " Line color field
      WHEN 'DETAIL'.
        <LFS_LAYOUT>-EDIT       = 'X'.     " Edit Mode Enable
        <LFS_LAYOUT>-SEL_MODE   = 'A'.     " mode for select col and row
        <LFS_LAYOUT>-LANGUAGE   = SY-LANGU." Language Key
        <LFS_LAYOUT>-TOTALS_BEF = 'X'.     " Upper Total Line
        <LFS_LAYOUT>-ZEBRA      = 'X'.         " Emphasize C250
        <LFS_LAYOUT>-STYLEFNAME = 'CELLTAB'.
*<lfs_layout>-cwidth_opt = 'X'.      "/optimizes the column width
*        <lfs_layout>-no_totline = ' '.        " Disable Total Line
*        <lfs_layout>-info_fname = 'ROW_COLOR'. " Line color field
      WHEN 'HISTORY'.
*        <lfs_layout>-edit       = 'X'.     " Edit Mode Enable
*        <lfs_layout>-sel_mode   = 'A'.   " mode for select col and row
        <LFS_LAYOUT>-LANGUAGE   = SY-LANGU." Language Key
*        <lfs_layout>-totals_bef = 'X'.     " Upper Total Line
        <LFS_LAYOUT>-ZEBRA      = 'X'.         " Emphasize C250
*        <lfs_layout>-stylefname = 'CELLTAB'.
*<lfs_layout>-cwidth_opt = 'X'.      "/optimizes the column width
*        <lfs_layout>-no_totline = ' '.        " Disable Total Line
*        <lfs_layout>-info_fname = 'ROW_COLOR'. " Line color field
    ENDCASE.
  ENDIF.

  CALL METHOD <LFS_GRID>->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = <LFS_LAYOUT>.
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_VARIANT USING PV_GRID.
  DATA: LV_GRID(100),
        LV_VARIANT(100).

  FIELD-SYMBOLS: <LFS_GRID>    TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_VARIANT> TYPE DISVARIANT.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'V_VARIANT_' PV_GRID INTO LV_VARIANT.
  ASSIGN:      (LV_VARIANT)         TO   <LFS_VARIANT>.


  CALL METHOD <LFS_GRID>->GET_VARIANT
    IMPORTING
      ES_VARIANT = <LFS_VARIANT>.

  IF <LFS_VARIANT> IS INITIAL.
    <LFS_VARIANT>-REPORT      = SY-REPID.
    <LFS_VARIANT>-HANDLE      = SPACE.
    <LFS_VARIANT>-LOG_GROUP   = SPACE.
    <LFS_VARIANT>-USERNAME    = SPACE.
    <LFS_VARIANT>-VARIANT     = SPACE.
    <LFS_VARIANT>-TEXT        = SPACE.
*    <lfs_variant>-dependvars  = space.
  ENDIF.
ENDFORM.                    " SET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_SORT_TOTAL_FIELD USING PV_GRID.
  DATA: LV_GRID(100),
        LV_SORT(100),
        LST_SORT LIKE LVC_S_SORT.

  FIELD-SYMBOLS: <LFS_GRID> TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_SORT> TYPE LVC_T_SORT.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'IT_SORT_' PV_GRID '[]' INTO LV_SORT.
  ASSIGN:      (LV_SORT)               TO   <LFS_SORT>.

  CALL METHOD <LFS_GRID>->GET_SORT_CRITERIA
    IMPORTING
      ET_SORT = <LFS_SORT>.

  CHECK <LFS_SORT> IS INITIAL.

  CASE PV_GRID.
    WHEN 'HEADER'.
*      REFRESH <lfs_sort>.
*      CLEAR : lst_sort.
*      lst_sort-fieldname = 'KEY'.
*      APPEND lst_sort TO <lfs_sort>.
    WHEN 'DETAIL'.
      REFRESH <LFS_SORT>.
      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'INDEX'.
      APPEND LST_SORT TO <LFS_SORT>.
      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'ZSECTX'.
      APPEND LST_SORT TO <LFS_SORT>.
  ENDCASE.
ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_FILTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_FILTER  USING PV_GRID.
  DATA: LV_GRID(100),
        LV_FILTER(100).

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_FILTER> TYPE LVC_T_FILT.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'IT_FILTER_' PV_GRID '[]' INTO LV_FILTER.
  ASSIGN:      (LV_FILTER)               TO   <LFS_FILTER>.

  CALL METHOD <LFS_GRID>->GET_FILTER_CRITERIA
    IMPORTING
      ET_FILTER = <LFS_FILTER>.

  IF <LFS_FILTER> IS INITIAL.
*    CASE pv_grid.
*      WHEN 'KEY_SUM'.
*        CLEAR : it_filter_key_sum. REFRESH it_filter_key_sum.
*    ENDCASE.
  ENDIF.

ENDFORM.                    " SET_FILTER
*&---------------------------------------------------------------------*
*&      Module  SET_LIST_BOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_LIST_BOX OUTPUT.
  PERFORM SET_LISTBOX_MODEL.
  PERFORM SET_LISTBOX_NATION.
ENDMODULE.                 " SET_LIST_BOX  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LISTBOX_MODEL .
  DATA: L_ATINN              LIKE CABN-ATINN.

  CLEAR : IT_LIST, ST_VALUE.
  REFRESH: IT_LIST.

  ST_VALUE-KEY  = '*'.
  ST_VALUE-TEXT = 'All'.
  APPEND: ST_VALUE TO IT_LIST.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT A~ATWRT AS KEY B~ATWTB AS TEXT
    APPENDING CORRESPONDING FIELDS OF TABLE IT_LIST
    FROM CAWN AS A INNER JOIN CAWNT AS B
                      ON B~ATINN = A~ATINN
                     AND B~ATZHL = A~ATZHL
   WHERE A~ATINN = L_ATINN
     AND B~SPRAS = SY-LANGU.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'V_MODEL'
      VALUES = IT_LIST.

ENDFORM.                    " SET_LISTBOX_MODEL
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: SY-UCOMM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SELECTION_CONDITION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SELECTION_CONDITION INPUT.
  PERFORM SET_RANGES_FIRST_LINE USING 'R_BODY'   R_BODY.
  PERFORM SET_RANGES_FIRST_LINE USING 'R_NATN'   R_NATN.
ENDMODULE.                 " SET_SELECTION_CONDITION  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1809   text
*      -->P_R_BODY  text
*----------------------------------------------------------------------*
FORM SET_RANGES_FIRST_LINE  USING PV_RANGE PV_RANGE_VAL.
  DATA: L_RANGE        TYPE STRING,
        L_RANGE_SIGN   TYPE STRING,
        L_RANGE_OPTION TYPE STRING,
        L_RANGE_LOW    TYPE STRING,
        L_RANGE_HIGH   TYPE STRING,
        L_BUTTON       TYPE STRING.

  FIELD-SYMBOLS: <LFS_RANGE_IT>     TYPE STANDARD TABLE,
                 <LFS_RANGE_SIGN>,
                 <LFS_RANGE_OPTION>,
                 <LFS_RANGE_LOW>,
                 <LFS_RANGE_HIGH>,
                 <LFS_BUTTON>.

  CONCATENATE: PV_RANGE '[]'          INTO L_RANGE,
               PV_RANGE '-SIGN'       INTO L_RANGE_SIGN,
               PV_RANGE '-OPTION'     INTO L_RANGE_OPTION,
               PV_RANGE '-LOW'        INTO L_RANGE_LOW,
               PV_RANGE '-HIGH'       INTO L_RANGE_HIGH,
               'G_' PV_RANGE+2 '_EXT' INTO L_BUTTON.

  ASSIGN: (L_RANGE)        TO <LFS_RANGE_IT>,
          (L_RANGE_SIGN)   TO <LFS_RANGE_SIGN>,
          (L_RANGE_OPTION) TO <LFS_RANGE_OPTION>,
          (L_RANGE_LOW)    TO <LFS_RANGE_LOW>,
          (L_RANGE_HIGH)   TO <LFS_RANGE_HIGH>,
          (L_BUTTON)       TO <LFS_BUTTON>.

  READ TABLE <LFS_RANGE_IT> INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    IF <LFS_RANGE_HIGH> IS INITIAL.
      IF <LFS_RANGE_LOW> IS INITIAL.
        DELETE <LFS_RANGE_IT> INDEX 1.
      ELSEIF <LFS_RANGE_LOW> CA '*+'.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'CP' TO <LFS_RANGE_OPTION>.
        MODIFY <LFS_RANGE_IT> INDEX 1 FROM PV_RANGE_VAL.
      ELSE.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'EQ' TO <LFS_RANGE_OPTION>.
        MODIFY <LFS_RANGE_IT> INDEX 1 FROM PV_RANGE_VAL.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <LFS_RANGE_SIGN>, 'BT' TO <LFS_RANGE_OPTION>.
      MODIFY <LFS_RANGE_IT> INDEX 1 FROM PV_RANGE_VAL.
    ENDIF.
  ELSE.
    IF <LFS_RANGE_HIGH> IS INITIAL.
      IF <LFS_RANGE_LOW> IS INITIAL.
        " N/A
      ELSEIF <LFS_RANGE_LOW> CA '*+'.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'CP' TO <LFS_RANGE_OPTION>.
        APPEND PV_RANGE_VAL TO <LFS_RANGE_IT>.
      ELSE.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'EQ' TO <LFS_RANGE_OPTION>.
        APPEND PV_RANGE_VAL TO <LFS_RANGE_IT>.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <LFS_RANGE_SIGN>, 'BT' TO <LFS_RANGE_OPTION>.
      APPEND PV_RANGE_VAL TO <LFS_RANGE_IT>.
    ENDIF.
  ENDIF.

  READ TABLE <LFS_RANGE_IT> INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    MOVE: ICON_DISPLAY_MORE TO <LFS_BUTTON>.
  ELSE.
    MOVE: ICON_ENTER_MORE TO <LFS_BUTTON>.
  ENDIF.
ENDFORM.                    " SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  PERFORM EVENT_TRIGGER.

  OK_CODE  = SY-UCOMM.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'DISPLAY'.
      W_MODE = SY-UCOMM.
      CLEAR: SY-UCOMM.
      PERFORM DISPLAY_RTN.
    WHEN 'REFRESH'.
      W_MODE = SY-UCOMM.
      CLEAR: SY-UCOMM.
      PERFORM REFRESH_RTN.
    WHEN 'DETAIL'.
      CLEAR: SY-UCOMM.
      PERFORM DISPLAY_HISTORY.
    WHEN 'SAVE'.
      CLEAR: SY-UCOMM.
      PERFORM SAVE_DATA.
    WHEN 'FEEDBACK'.
      CLEAR: SY-UCOMM.
      PERFORM FEEDBACK USING 'SHOP'.
    WHEN 'FEEDBACKPC'.
      CLEAR: SY-UCOMM.
      PERFORM FEEDBACK USING 'PC'.
    WHEN 'SETUP'.
      CLEAR: SY-UCOMM.
      PERFORM CALL_ZPPA00001.
    WHEN 'SIDETRACK'.
      CLEAR: SY-UCOMM.
      PERFORM CALL_SIDETRACK.
    WHEN 'BODY_EXT'.
      CLEAR: SY-UCOMM.
      PERFORM CLICK_EXTENSION USING 'BODY' 'ZTPP_VM-BODY_NO'.
    WHEN 'NATN_EXT'.
      CLEAR: SY-UCOMM.
      PERFORM CLICK_EXTENSION USING 'NATN' 'ZTPP_VM-WO_NATION'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CLICK_EXTENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2094   text
*      -->P_2095   text
*----------------------------------------------------------------------*
FORM CLICK_EXTENSION USING P_FIELD P_REF.
  DATA: LS_REF      LIKE RSTABFIELD,
        L_RANGE_H(50),               " Body of ranges
        L_RANGE_B(50),               " Header of ranges
        L_BUTTON(50).

  FIELD-SYMBOLS: <LFS_RANGE_H>,
                 <LFS_RANGE_B> TYPE STANDARD TABLE,
                 <LFS_BUTTON>.

  SPLIT P_REF AT '-' INTO LS_REF-TABLENAME LS_REF-FIELDNAME.

  CONCATENATE 'R_' P_FIELD INTO L_RANGE_H.
  ASSIGN (L_RANGE_H) TO <LFS_RANGE_H>.

  CONCATENATE 'R_' P_FIELD '[]' INTO L_RANGE_B.
  ASSIGN (L_RANGE_B) TO <LFS_RANGE_B>.

  CONCATENATE 'G_' P_FIELD '_EXT' INTO L_BUTTON.
  ASSIGN (L_BUTTON) TO <LFS_BUTTON>.

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      TAB_AND_FIELD = LS_REF
    TABLES
      RANGE         = <LFS_RANGE_B>
    EXCEPTIONS
      CANCELLED     = 1
      OTHERS        = 2.

  READ TABLE <LFS_RANGE_B> INTO <LFS_RANGE_H> INDEX 1.
  IF SY-SUBRC EQ 0.
    MOVE: ICON_DISPLAY_MORE TO <LFS_BUTTON>.
  ELSE.
    CLEAR: <LFS_RANGE_H>.
    MOVE: ICON_ENTER_MORE   TO <LFS_BUTTON>.
  ENDIF.
ENDFORM.                    " CLICK_EXTENSION
*&---------------------------------------------------------------------*
*&      Form  REFRESH_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_RTN .
  PERFORM CLEAR_DATA.

  V_SHOP_DATE = W_CURRENT_SHOP_DATE.

  PERFORM READ_SECTION_MASTER.
  PERFORM READ_PREVIOUS_DELAY_VEH.
  PERFORM READ_DELAY_TEMP.
  PERFORM READ_WIP.
  PERFORM MAKE_HEADER.
*  PERFORM save_for_refresh.

ENDFORM.                    " REFRESH_RTN
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION .
  MOVE: '*' TO V_MODEL,
        '*' TO V_USAGE.
  W_SYS_TIME = SY-UZEIT.
  W_SYS_DATE = SY-DATUM.
  CONCATENATE W_SYS_DATE W_SYS_TIME INTO W_CURR_TIME.
  PERFORM GET_CURRENT_SHOPE_DATE.

  V_SHOP_DATE = W_CURRENT_SHOP_DATE.

  PERFORM READ_SECTION_MASTER.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_VEH_OBJEK  text
*      -->P_LT_VEH_ATWRT  text
*----------------------------------------------------------------------*
FORM MAKE_HEADER_WIP USING PV_MODE PV_RP PV_WORDER P_LINES.
  CLEAR: IT_HEADER.

  READ TABLE IT_HEADER WITH KEY KEY = PV_MODE.
  IF SY-SUBRC NE 0.
    MOVE: PV_MODE TO IT_HEADER-KEY.
    APPEND IT_HEADER.
  ENDIF.
  IT_HEADER-LINES = P_LINES.
  IT_HEADER-TOTAL = IT_HEADER-TOTAL + 1.

  LOOP AT IT_SECTION WHERE ZRPFR <= PV_RP
                       AND ZRPTO >= PV_RP.
    IF IT_SECTION-ZSHIP_FLG EQ 'X'.
      CASE IT_SECTION-ZOPER.
        WHEN '='.
          IF NOT PV_WORDER+9(3) = 'B28'.
            CONTINUE.
          ENDIF.
        WHEN '<>'.
          IF NOT PV_WORDER+9(3) <> 'B28'.
            CONTINUE.
          ENDIF.
      ENDCASE.
    ENDIF.

    CASE IT_SECTION-FIELD_IDX.
      WHEN '01'.
        IT_HEADER-01QTY = IT_HEADER-01QTY + 1.
      WHEN '02'.
        IT_HEADER-02QTY = IT_HEADER-02QTY + 1.
      WHEN '03'.
        IT_HEADER-03QTY = IT_HEADER-03QTY + 1.
      WHEN '04'.
        IT_HEADER-04QTY = IT_HEADER-04QTY + 1.
      WHEN '05'.
        IT_HEADER-05QTY = IT_HEADER-05QTY + 1.
      WHEN '06'.
        IT_HEADER-06QTY = IT_HEADER-06QTY + 1.
      WHEN '07'.
        IT_HEADER-07QTY = IT_HEADER-07QTY + 1.
      WHEN '08'.
        IT_HEADER-08QTY = IT_HEADER-08QTY + 1.
      WHEN '09'.
        IT_HEADER-09QTY = IT_HEADER-09QTY + 1.
      WHEN '10'.
        IT_HEADER-10QTY = IT_HEADER-10QTY + 1.
      WHEN '11'.
        IT_HEADER-11QTY = IT_HEADER-11QTY + 1.
      WHEN '12'.
        IT_HEADER-12QTY = IT_HEADER-12QTY + 1.
      WHEN '13'.
        IT_HEADER-13QTY = IT_HEADER-13QTY + 1.
      WHEN '14'.
        IT_HEADER-14QTY = IT_HEADER-14QTY + 1.
      WHEN '15'.
        IT_HEADER-15QTY = IT_HEADER-15QTY + 1.
      WHEN '16'.
        IT_HEADER-16QTY = IT_HEADER-16QTY + 1.
      WHEN '17'.
        IT_HEADER-17QTY = IT_HEADER-17QTY + 1.
      WHEN '18'.
        IT_HEADER-18QTY = IT_HEADER-18QTY + 1.
      WHEN '19'.
        IT_HEADER-19QTY = IT_HEADER-19QTY + 1.
      WHEN '20'.
        IT_HEADER-20QTY = IT_HEADER-20QTY + 1.
      WHEN '21'.
        IT_HEADER-21QTY = IT_HEADER-21QTY + 1.
      WHEN '22'.
        IT_HEADER-22QTY = IT_HEADER-22QTY + 1.
      WHEN '23'.
        IT_HEADER-23QTY = IT_HEADER-23QTY + 1.
      WHEN '24'.
        IT_HEADER-24QTY = IT_HEADER-24QTY + 1.
      WHEN '25'.
        IT_HEADER-25QTY = IT_HEADER-25QTY + 1.
      WHEN '26'.
        IT_HEADER-26QTY = IT_HEADER-26QTY + 1.
      WHEN '27'.
        IT_HEADER-27QTY = IT_HEADER-27QTY + 1.
    ENDCASE.

    EXIT.
  ENDLOOP.

  MODIFY IT_HEADER INDEX SY-TABIX.

ENDFORM.                    " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM HOTSPOT_HEADER USING PV_ROW_ID PV_COLUMN_ID PS_ROW_NO.
  DATA: L_TABIX LIKE SY-TABIX.

  DATA: IT_SYS_FDBACK LIKE ZTPP_DELAY_IF  OCCURS 0 WITH HEADER LINE,
        LT_UM LIKE ZTSD_UM OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF LT_SIDE_TRACK OCCURS 0,
          MODEL_CODE LIKE ZTPP_SIDE_TRACK-MODEL_CODE,
          BODY_NO    LIKE ZTPP_SIDE_TRACK-BODY_NO,
          ZSTTXT     LIKE ZTPP_SIDE_TRACK-ZSTTXT,
          ZSEQ       LIKE ZTPP_SIDE_TRACK-ZSEQ,
        END OF LT_SIDE_TRACK.

  READ TABLE IT_HEADER INDEX PV_ROW_ID.

  CHECK SY-SUBRC EQ 0.

  CLEAR: IT_DETAIL[].

  SORT IT_WIP BY RP_STATUS ZITIME MODEL BODY_NO.
  SORT IT_WIP_DELAY BY ZDHOUR DESCENDING ZITIME MODEL BODY_NO.

  REFRESH IT_DETAIL.

  CASE IT_HEADER-KEY.
    WHEN C_WIP.             "WIP
      CASE PV_COLUMN_ID.
        WHEN 'TOTAL'.
          LOOP AT IT_WIP.
            CLEAR: IT_DETAIL.

            MOVE-CORRESPONDING IT_WIP TO IT_DETAIL.
            APPEND IT_DETAIL.
          ENDLOOP.
        WHEN OTHERS.
          LOOP AT IT_SECTION WHERE FIELD_IDX = PV_COLUMN_ID(2).
            LOOP AT IT_WIP WHERE ZSECID = IT_SECTION-ZSECID
                             AND SEQ    = IT_SECTION-SEQ.
              CLEAR: IT_DETAIL.
              MOVE-CORRESPONDING IT_WIP TO IT_DETAIL.

              APPEND IT_DETAIL.
            ENDLOOP.
          ENDLOOP.
      ENDCASE.
    WHEN C_TOTAL.          "Delay Total
      CASE PV_COLUMN_ID.
        WHEN 'TOTAL'.
          LOOP AT IT_WIP_DELAY.
            CLEAR: IT_DETAIL.
            MOVE-CORRESPONDING IT_WIP_DELAY TO IT_DETAIL.
            APPEND IT_DETAIL.
          ENDLOOP.
        WHEN OTHERS.
          LOOP AT IT_SECTION WHERE FIELD_IDX = PV_COLUMN_ID(2).
            LOOP AT IT_WIP_DELAY WHERE ZSECID = IT_SECTION-ZSECID
                                   AND SEQ    = IT_SECTION-SEQ.

              CLEAR: IT_DETAIL.
              MOVE-CORRESPONDING IT_WIP_DELAY TO IT_DETAIL.
              APPEND IT_DETAIL.
            ENDLOOP.
          ENDLOOP.
          SORT IT_DETAIL BY ZDHOUR DESCENDING ZITIME MODEL BODY_NO.
      ENDCASE.
    WHEN OTHERS.
      CASE PV_COLUMN_ID.
        WHEN 'TOTAL'.
          LOOP AT IT_WIP_DELAY WHERE MODEL = IT_HEADER-KEY(3).
            CLEAR: IT_DETAIL.
            MOVE-CORRESPONDING IT_WIP_DELAY TO IT_DETAIL.
            APPEND IT_DETAIL.
          ENDLOOP.
        WHEN OTHERS.
          LOOP AT IT_SECTION WHERE FIELD_IDX = PV_COLUMN_ID(2).
            LOOP AT IT_WIP_DELAY WHERE ZSECID = IT_SECTION-ZSECID
                                   AND SEQ    = IT_SECTION-SEQ
                                   AND MODEL  = IT_HEADER-KEY(3).
              CLEAR: IT_DETAIL.
              MOVE-CORRESPONDING IT_WIP_DELAY TO IT_DETAIL.
              APPEND IT_DETAIL.
            ENDLOOP.
          ENDLOOP.
          SORT IT_DETAIL BY ZDHOUR DESCENDING ZITIME MODEL BODY_NO.
      ENDCASE.
  ENDCASE.


  IF NOT IT_DETAIL[] IS INITIAL.
** Get system feedback/side track

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_SYS_FDBACK
      FROM ZTPP_DELAY_IF
       FOR ALL ENTRIES IN IT_DETAIL
     WHERE MODEL_CODE  = IT_DETAIL-MODEL
       AND BODY_NO  = IT_DETAIL-BODY_NO
       AND RP_STATUS = IT_DETAIL-RP_STATUS.

    SELECT MODEL_CODE BODY_NO ZSTTXT ZSEQ
      INTO TABLE LT_SIDE_TRACK
      FROM ZTPP_SIDE_TRACK
       FOR ALL ENTRIES IN IT_DETAIL
     WHERE MODEL_CODE	= IT_DETAIL-MODEL
       AND BODY_NO  = IT_DETAIL-BODY_NO.

    SELECT *
      INTO TABLE LT_UM
      FROM ZTSD_UM
       FOR ALL ENTRIES IN IT_DETAIL
     WHERE MODEL_CODE	= IT_DETAIL-MODEL
       AND BODY_NO  = IT_DETAIL-BODY_NO.

    SORT LT_UM BY MODEL_CODE BODY_NO DESCENDING.
    SORT LT_SIDE_TRACK BY MODEL_CODE BODY_NO ZSEQ DESCENDING.
    SORT IT_SYS_FDBACK BY MODEL_CODE BODY_NO ZLDATIM DESCENDING.

    LOOP AT IT_DETAIL.
      L_TABIX = SY-TABIX.

      IT_DETAIL-INDEX = SY-TABIX.

      MODIFY IT_DETAIL INDEX L_TABIX.

      CHECK W_MODE EQ 'REFRESH'.

      READ TABLE IT_SYS_FDBACK WITH KEY MODEL_CODE = IT_DETAIL-MODEL
                                   BODY_NO = IT_DETAIL-BODY_NO
                                   BINARY SEARCH.
      IF SY-SUBRC = 0.
        IT_DETAIL-ZLFDBK  = IT_SYS_FDBACK-ZRSN.
        IT_DETAIL-ZSYSTEM = IT_SYS_FDBACK-ZSYSTEM.
      ENDIF.

      READ TABLE LT_SIDE_TRACK WITH KEY MODEL_CODE = IT_DETAIL-MODEL
                                   BODY_NO = IT_DETAIL-BODY_NO
                                   BINARY SEARCH.
      IF SY-SUBRC = 0.
        IT_DETAIL-ZSTTXT = LT_SIDE_TRACK-ZSTTXT.
      ENDIF.

      READ TABLE LT_UM  WITH KEY MODEL_CODE = IT_DETAIL-MODEL
                                   BODY_NO = IT_DETAIL-BODY_NO
                                   BINARY SEARCH.
      IF SY-SUBRC = 0.
        IT_DETAIL-URGENCY = LT_UM-URGENCY.
        IT_DETAIL-URGCDATE = LT_UM-URGCDATE.
        IT_DETAIL-FLET = LT_UM-FLET.
        PERFORM GET_ESTIMATED_SOFF USING IT_DETAIL-ZETA
                                         IT_DETAIL-ZDRDY
                                         IT_DETAIL-ZESOFF.

      ENDIF.
      MODIFY IT_DETAIL INDEX L_TABIX.
    ENDLOOP.
  ENDIF.
  CLEAR: OK_CODE.
  LEAVE TO SCREEN 0100.
ENDFORM.                    " HOTSPOT_HEADER
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX_NATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LISTBOX_NATION .
  DATA: L_ATINN              LIKE CABN-ATINN.

  CLEAR : IT_LIST, ST_VALUE.
  REFRESH: IT_LIST.

  ST_VALUE-KEY  = '*'.
  ST_VALUE-TEXT = 'All'.
  APPEND: ST_VALUE TO IT_LIST.

  ST_VALUE-KEY  = 'P'.
  ST_VALUE-TEXT = 'Sales Car Only'.
  APPEND: ST_VALUE TO IT_LIST.

  ST_VALUE-KEY  = 'A'.
  ST_VALUE-TEXT = 'Allocation Car Only'.
  APPEND: ST_VALUE TO IT_LIST.

  ST_VALUE-KEY  = 'T'.
  ST_VALUE-TEXT = 'Test Car Only'.
  APPEND: ST_VALUE TO IT_LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'V_USAGE'
      VALUES = IT_LIST.
ENDFORM.                    " SET_LISTBOX_NATION
*&---------------------------------------------------------------------*
*&      Form  READ_SECTION_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_SECTION_MASTER .
  DATA: L_INDEX(2) TYPE N.

  SELECT * INTO TABLE IT_SECTION
    FROM ZTPP_DELAY_SECT.

  SORT IT_SECTION BY ZRPFR ZRPTO ZSECID SEQ.

  REFRESH: IT_HEADER_TXT.
  LOOP AT IT_SECTION.
    CLEAR: IT_HEADER_TXT.

    READ TABLE IT_HEADER_TXT WITH KEY ZSECID = IT_SECTION-ZSECID.
    IF SY-SUBRC NE 0.
      L_INDEX = L_INDEX + 1.

      MOVE: L_INDEX           TO IT_HEADER_TXT-INDEX,
            IT_SECTION-ZSECID TO IT_HEADER_TXT-ZSECID,
            IT_SECTION-ZSECTX TO IT_HEADER_TXT-ZSECTX.

      APPEND IT_HEADER_TXT.
    ENDIF.

    PERFORM READ_LIMIT_TIME.

    MOVE: L_INDEX TO IT_SECTION-FIELD_IDX.

    MODIFY IT_SECTION.
  ENDLOOP.

  SELECT SINGLE ATINN INTO W_RP_STATUS
    FROM CABN WHERE ATNAM = C_RP_STATUS.

  SELECT SINGLE ATINN INTO W_USAGE_CAR
    FROM CABN WHERE ATNAM = C_USAGE_CAR.

  SELECT SINGLE ATINN INTO W_WORDER
    FROM CABN WHERE ATNAM = C_WORDER.

  SELECT SINGLE ATINN INTO W_MI
    FROM CABN WHERE ATNAM = C_MI.

  SELECT SINGLE ATINN INTO W_OCN
    FROM CABN WHERE ATNAM = C_OCN.

  SELECT SINGLE ATINN INTO W_DEALER_NO
   FROM CABN WHERE ATNAM = C_DEALER_NO.

  SELECT SINGLE ATINN INTO W_RP21
    FROM CABN WHERE ATNAM = C_RP21.

  SELECT SINGLE ATINN INTO W_EXTC
    FROM CABN WHERE ATNAM = C_EXTC.

  SELECT SINGLE ATINN INTO W_INTC
    FROM CABN WHERE ATNAM = C_INTC.

ENDFORM.                    " READ_SECTION_MASTER
*&---------------------------------------------------------------------*
*&      Form  READ_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WIP .
  DATA: LT_TIME     LIKE AUSP OCCURS 0 WITH HEADER LINE,
        LT_TEMP     LIKE TABLE OF IT_WIP WITH HEADER LINE,
        L_ACTUAL    TYPE STRING,
        L_INDEX     LIKE SY-TABIX,
        L_TEMP_DATE LIKE SY-DATUM,
        L_DATUM(8) TYPE N,
        L_ZITIME(19).

  RANGES: R_USAGE FOR AUSP-ATWRT.

  SELECT A~OBJEK A~KLART A~ATWRT AS RPID
         B~ATWRT AS USAGE
         C~ATWRT AS WORDER
         D~ATWRT AS MI E~ATWRT AS OCN
         F~ATWRT AS DEALER_NO
         G~ATFLV AS RP21_ATFLV
         H~ATWRT AS EXTC
         I~ATWRT AS INTC
      INTO CORRESPONDING FIELDS OF TABLE IT_WIP
      FROM AUSP AS A LEFT OUTER JOIN AUSP AS B
                        ON B~OBJEK = A~OBJEK
                       AND B~ATINN = W_USAGE_CAR
                       AND B~MAFID = A~MAFID
                       AND B~KLART = A~KLART
                     LEFT OUTER JOIN AUSP AS C
                        ON C~OBJEK = A~OBJEK
                       AND C~ATINN = W_WORDER
                       AND C~MAFID = A~MAFID
                       AND C~KLART = A~KLART
                     LEFT OUTER JOIN AUSP AS D
                        ON D~OBJEK = A~OBJEK
                       AND D~ATINN = W_MI
                       AND D~MAFID = A~MAFID
                       AND D~KLART = A~KLART
                     LEFT OUTER JOIN AUSP AS E
                        ON E~OBJEK = A~OBJEK
                       AND E~ATINN = W_OCN
                       AND E~MAFID = A~MAFID
                       AND E~KLART = A~KLART
                     LEFT OUTER JOIN AUSP AS F
                        ON F~OBJEK = A~OBJEK
                       AND F~ATINN = W_DEALER_NO
                       AND F~MAFID = A~MAFID
                       AND F~KLART = A~KLART
                     LEFT OUTER JOIN AUSP AS G
                        ON G~OBJEK = A~OBJEK
                       AND G~ATINN = W_RP21
                       AND G~MAFID = A~MAFID
                       AND G~KLART = A~KLART
                     LEFT OUTER JOIN AUSP AS H
                         ON H~OBJEK = A~OBJEK
                        AND H~ATINN = W_EXTC
                        AND H~MAFID = A~MAFID
                        AND H~KLART = A~KLART
                     LEFT OUTER JOIN AUSP AS I
                         ON I~OBJEK = A~OBJEK
                        AND I~ATINN = W_INTC
                        AND I~MAFID = A~MAFID
                        AND I~KLART = A~KLART
      WHERE A~KLART EQ '002'
        AND A~ATINN EQ W_RP_STATUS
        AND A~ATWRT IN ('01','02','03','04','05',
                        '06','07','08','09','10',
                        '11','12','13','14','15',
                        '16','17','18','19','20',
                        '21','22','23','24','26').

  R_USAGE-OPTION = 'EQ'.
  R_USAGE-SIGN = 'I'.
  R_USAGE-LOW = 'D'.
  APPEND R_USAGE.
  R_USAGE-LOW = 'S'.
  APPEND R_USAGE.
  R_USAGE-LOW = '2'.
  APPEND R_USAGE.

  DELETE IT_WIP WHERE USAGE = ' '.
  DELETE IT_WIP WHERE USAGE IN R_USAGE.

  LOOP AT IT_SECTION.
    CONCATENATE 'P_RP' IT_SECTION-ZRPFR '_ACTUAL_DATE' INTO L_ACTUAL.

    IT_WIP-ZSECID = IT_SECTION-ZSECID.
    IT_WIP-SEQ    = IT_SECTION-SEQ.
    IT_WIP-ZSECTX = IT_SECTION-ZSECTX.
    IT_WIP-ZDRDY  = IT_SECTION-ZDRDY.
    IT_WIP-ZRPFR  = IT_SECTION-ZRPFR.
    IT_WIP-ZRPTO  = IT_SECTION-ZRPTO.

    SELECT SINGLE ATINN INTO IT_WIP-ATINN_INPUT
      FROM CABN
     WHERE ATNAM = L_ACTUAL.

    CASE IT_SECTION-ZSHIP_FLG.
      WHEN SPACE.
        MODIFY IT_WIP TRANSPORTING ATINN_INPUT ZSECID SEQ ZSECTX
                                   ZDRDY ZRPFR ZRPTO
                      WHERE RPID >= IT_SECTION-ZRPFR
                        AND RPID <= IT_SECTION-ZRPTO.
      WHEN 'X'.
        CASE IT_SECTION-ZOPER.
          WHEN '='.
            MODIFY IT_WIP TRANSPORTING ATINN_INPUT ZSECID SEQ ZSECTX
                                       ZDRDY ZRPFR ZRPTO
                          WHERE RPID >= IT_SECTION-ZRPFR
                            AND RPID <= IT_SECTION-ZRPTO
                            AND WORDER+9(3) = 'B28'.
          WHEN '<>'.
            MODIFY IT_WIP TRANSPORTING ATINN_INPUT ZSECID SEQ ZSECTX
                                       ZDRDY ZRPFR ZRPTO
                          WHERE RPID >= IT_SECTION-ZRPFR
                            AND RPID <= IT_SECTION-ZRPTO
                            AND WORDER+9(3) <> 'B28'.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

  DELETE IT_WIP WHERE ZSECID IS INITIAL.

  CHECK IT_WIP[] IS NOT INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_TIME
    FROM AUSP
     FOR ALL ENTRIES IN IT_WIP
   WHERE OBJEK = IT_WIP-OBJEK
     AND ATINN = IT_WIP-ATINN_INPUT
     AND KLART = IT_WIP-KLART.

  SORT LT_TIME BY OBJEK.

  LOOP AT IT_WIP.
    L_INDEX = SY-TABIX.

    IF V_MODEL <> '*'.
      IF V_MODEL <> IT_WIP-OBJEK(3).
        DELETE IT_WIP. CONTINUE.
      ENDIF.
    ENDIF.

    IF NOT IT_WIP-WORDER+9(3) IN R_NATN.
      DELETE IT_WIP. CONTINUE.
    ENDIF.

    IF NOT IT_WIP-OBJEK+3(6) IN R_BODY[].
      DELETE IT_WIP. CONTINUE.
    ENDIF.

    CASE V_USAGE.
      WHEN 'P'.
        IF  IT_WIP-WORDER+12(2) = 'XX' OR
            IT_WIP-WORDER+12(2) = 'XY' OR
            IT_WIP-WORDER+12(2) = 'XA'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
      WHEN 'A'.
        IF IT_WIP-DEALER_NO IS INITIAL.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
      WHEN 'T'.
        IF  NOT IT_WIP-WORDER+12(2) = 'XX' AND
           NOT IT_WIP-WORDER+12(2)  = 'XY' AND
           NOT IT_WIP-WORDER+12(2)  = 'XA'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
    ENDCASE.

    CASE IT_WIP-WORDER+12(2).
      WHEN 'XX'.         "BIW
        IF IT_WIP-RPID > '01'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
      WHEN 'XY'.         "BIP
        IF IT_WIP-RPID > '05'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
      WHEN 'XA'.         "Pilot Car
        IF IT_WIP-RPID > '17'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
    ENDCASE.

    IT_WIP-ALLOC_DATE = L_DATUM = IT_WIP-RP21_ATFLV.

    READ TABLE LT_TIME WITH KEY OBJEK = IT_WIP-OBJEK
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE: LT_TIME-ATWRT TO IT_WIP-ZITIME.
      IT_WIP-MODEL = IT_WIP-OBJEK+0(3).
      IT_WIP-BODY_NO = IT_WIP-OBJEK+3(6).
      IT_WIP-RP_STATUS = IT_WIP-RPID.
    ELSE.
      L_TEMP_DATE = W_SYS_DATE - 180.
      CONCATENATE L_TEMP_DATE W_SYS_TIME INTO IT_WIP-ZITIME.
    ENDIF.

    MOVE: IT_WIP-ZITIME+4(4) TO L_ZITIME(4),
      IT_WIP-ZITIME(4)   TO L_ZITIME+4(4),
      IT_WIP-ZITIME+8(6) TO L_ZITIME+8(6).

    MOVE: L_ZITIME TO IT_WIP-ZITIME_OUT.

    MODIFY IT_WIP INDEX L_INDEX.
  ENDLOOP.

  IF IT_WIP[] IS NOT INITIAL.
    PERFORM READ_OTHER_INFO TABLES IT_WIP.
    PERFORM SET_OTHER_INFO  TABLES IT_WIP.
  ENDIF.

ENDFORM.                    " READ_WIP
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA .
  REFRESH: IT_HEADER, IT_DETAIL, IT_HISTORY, IT_DELAY,
           IT_SYS_FDBACK, IT_DELAY_IF, IT_SECTION,
           IT_WORK_TIME, IT_WIP, IT_WIP_DELAY.
  CLEAR: V_BDATE_B, V_BDATE_P, V_BDATE_T.

  W_SHOP_DATE_BK = V_SHOP_DATE.

  W_SYS_TIME = SY-UZEIT.
  W_SYS_DATE = SY-DATUM.

  CONCATENATE W_SYS_DATE W_SYS_TIME INTO W_CURR_TIME.

  PERFORM GET_CURRENT_SHOPE_DATE.
ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Module  CHECK_CHANGED  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CHANGED INPUT.
  DATA: L_ANSWER(1),
        L_TEXT(50)
        VALUE 'Shop Date was changed. Do you want to refresh data?'.
  CHECK OK_CODE <> 'REFRESH'.
  IF V_SHOP_DATE <> W_SHOP_DATE_BK
     AND NOT W_SHOP_DATE_BK IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'Confirmation'
*       DIAGNOSE_OBJECT       = ' '
        TEXT_QUESTION         = L_TEXT
        TEXT_BUTTON_1         = 'YES'
        ICON_BUTTON_1         = 'ICON_OKAY'
        TEXT_BUTTON_2         = 'NO'
        ICON_BUTTON_2         = 'ICON_CANCEL'
*       DEFAULT_BUTTON        = '1'
        DISPLAY_CANCEL_BUTTON = ' '
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN          = ' '
*       START_ROW             = 6
*       POPUP_TYPE            =
      IMPORTING
        ANSWER                = L_ANSWER.

    IF L_ANSWER = '1'.
      CLEAR: SY-UCOMM.
      PERFORM REFRESH_RTN.
    ELSE.
      V_SHOP_DATE = W_SHOP_DATE_BK.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_CHANGED  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_SHOPE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CURRENT_SHOPE_DATE .
  DATA: L_GAP TYPE I,
        L_DATE LIKE SY-DATUM,
        L_TIME(14),
        L_DATE_C(8),
        L_UZEIT LIKE SY-UZEIT.

  DATA: LT_WORK_TIME  LIKE ZSMM_WORKING_TIME OCCURS 0
                       WITH HEADER LINE.

** Changed by Park On 11/22/13
*  l_date = w_sys_date - 1.
*  l_gap = 2.
*  l_date_c = w_sys_date.
*  l_uzeit = w_sys_time.
*
*
*  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
*    EXPORTING
*      i_datum              = l_date
*      i_day                = 2
*      i_arbpl              = 'T'
*    TABLES
*      t_working_time       = lt_work_time
*    EXCEPTIONS
*      cannot_read_dayname  = 1
*      incorrect_shift_info = 2
*      incorrect_capa_info  = 3
*      OTHERS               = 4.
*
*  LOOP AT lt_work_time WHERE wofrm <= w_curr_time
*                         AND woend >= w_curr_time.
*    w_current_shop_date = lt_work_time-datum.
*    EXIT.
*  ENDLOOP.
*  IF sy-subrc NE 0.
*    IF w_sys_time >= '000000'  AND w_sys_time <= '000645'.
*      w_current_shop_date = w_sys_date - 1.
*    ELSE.
*      w_current_shop_date = w_sys_date.
*    ENDIF.
*  ENDIF.

  W_CURRENT_SHOP_DATE = W_SYS_DATE.
** End of change 11/22/13

ENDFORM.                    " GET_CURRENT_SHOPE_DATE
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_HEADER .
  DATA: LT_VEH LIKE TABLE OF ZTPP_DELAY_VEH WITH HEADER LINE,
        LW_TOTAL LIKE IT_HEADER,
        L_MODEL_DESC(40),
        L_LINES(1) TYPE N,
        L_ZITIME(14).

  REFRESH: IT_HEADER, IT_WIP_DELAY.
  CLEAR: L_LINES.
  L_LINES = L_LINES + 1.

  SORT IT_SECTION BY ZSECID SEQ.

  LOOP AT IT_WIP.
    PERFORM MAKE_HEADER_WIP USING C_WIP IT_WIP-RPID
                                  IT_WIP-WORDER  L_LINES.
  ENDLOOP.
  IF IT_HEADER[] IS INITIAL.
    CLEAR: IT_HEADER.
    MOVE: C_WIP TO IT_HEADER-KEY.
    APPEND IT_HEADER.
  ENDIF.
  L_LINES = L_LINES + 1.

  PERFORM GET_DELAY_VEHICLE.

  LOOP AT IT_WIP_DELAY.
    PERFORM MAKE_HEADER_WIP USING IT_WIP_DELAY-MODEL IT_WIP_DELAY-RPID
                                  IT_WIP_DELAY-WORDER L_LINES.
  ENDLOOP.

  LOOP AT IT_HEADER FROM 2.
    LW_TOTAL-TOTAL = LW_TOTAL-TOTAL + IT_HEADER-TOTAL.
    LW_TOTAL-01QTY = LW_TOTAL-01QTY + IT_HEADER-01QTY.
    LW_TOTAL-02QTY = LW_TOTAL-02QTY + IT_HEADER-02QTY.
    LW_TOTAL-03QTY = LW_TOTAL-03QTY + IT_HEADER-03QTY.
    LW_TOTAL-04QTY = LW_TOTAL-04QTY + IT_HEADER-04QTY.
    LW_TOTAL-05QTY = LW_TOTAL-05QTY + IT_HEADER-05QTY.
    LW_TOTAL-06QTY = LW_TOTAL-06QTY + IT_HEADER-06QTY.
    LW_TOTAL-07QTY = LW_TOTAL-07QTY + IT_HEADER-07QTY.
    LW_TOTAL-08QTY = LW_TOTAL-08QTY + IT_HEADER-08QTY.
    LW_TOTAL-09QTY = LW_TOTAL-09QTY + IT_HEADER-09QTY.
    LW_TOTAL-10QTY = LW_TOTAL-10QTY + IT_HEADER-10QTY.
    LW_TOTAL-11QTY = LW_TOTAL-11QTY + IT_HEADER-11QTY.
    LW_TOTAL-12QTY = LW_TOTAL-12QTY + IT_HEADER-12QTY.
    LW_TOTAL-13QTY = LW_TOTAL-13QTY + IT_HEADER-13QTY.
    LW_TOTAL-14QTY = LW_TOTAL-14QTY + IT_HEADER-14QTY.
    LW_TOTAL-15QTY = LW_TOTAL-15QTY + IT_HEADER-15QTY.
    LW_TOTAL-16QTY = LW_TOTAL-16QTY + IT_HEADER-16QTY.
    LW_TOTAL-17QTY = LW_TOTAL-17QTY + IT_HEADER-17QTY.
    LW_TOTAL-18QTY = LW_TOTAL-18QTY + IT_HEADER-18QTY.
    LW_TOTAL-19QTY = LW_TOTAL-19QTY + IT_HEADER-19QTY.
    LW_TOTAL-20QTY = LW_TOTAL-20QTY + IT_HEADER-20QTY.
    LW_TOTAL-21QTY = LW_TOTAL-21QTY + IT_HEADER-21QTY.
    LW_TOTAL-22QTY = LW_TOTAL-22QTY + IT_HEADER-22QTY.
    LW_TOTAL-23QTY = LW_TOTAL-23QTY + IT_HEADER-23QTY.
    LW_TOTAL-24QTY = LW_TOTAL-24QTY + IT_HEADER-24QTY.
    LW_TOTAL-25QTY = LW_TOTAL-25QTY + IT_HEADER-25QTY.
    LW_TOTAL-26QTY = LW_TOTAL-26QTY + IT_HEADER-26QTY.
    LW_TOTAL-27QTY = LW_TOTAL-27QTY + IT_HEADER-27QTY.
  ENDLOOP.
  L_LINES = L_LINES + 1.
  LW_TOTAL-KEY = C_TOTAL.
  LW_TOTAL-LINES = L_LINES.
  APPEND LW_TOTAL TO IT_HEADER.
  SORT IT_HEADER BY LINES.
ENDFORM.                    " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_DELAY_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DELAY_VEHICLE .
  DATA: L_DATE   LIKE SY-DATUM,
        L_TIME   LIKE SY-UZEIT,
        L_WDATE  LIKE SY-DATUM,
        L_WTIME  LIKE SY-UZEIT,
        L_WKTIME TYPE I,
        L_ARBPL  LIKE CRHD-ARBPL,
        L_TARGET_TIME(14),
        L_CURRENT_TIME(14),
        L_INDEX  LIKE SY-TABIX.

  DATA: BEGIN OF LT_SIDE_TRACK OCCURS 0,
          MODEL_CODE LIKE ZTPP_SIDE_TRACK-MODEL_CODE,
          BODY_NO    LIKE ZTPP_SIDE_TRACK-BODY_NO,
          ZSTTXT     LIKE ZTPP_SIDE_TRACK-ZSTTXT,
          ZSEQ       LIKE ZTPP_SIDE_TRACK-ZSEQ,
        END OF LT_SIDE_TRACK.

  PERFORM READ_WORKING_TIME.

  REFRESH: IT_WIP_DELAY.

  SORT IT_SECTION BY ZSECID SEQ.

  LOOP AT IT_WIP.
    CLEAR: IT_WIP_DELAY, L_TARGET_TIME, L_INDEX.

    L_INDEX = SY-TABIX.

    READ TABLE IT_SECTION WITH KEY ZSECID =  IT_WIP-ZSECID
                                   SEQ    =  IT_WIP-SEQ
                          BINARY SEARCH.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    L_DATE = IT_WIP-ZITIME+0(8).
    L_TIME = IT_WIP-ZITIME+8(6).
    L_WKTIME = IT_SECTION-ZDURA * 60.
    L_ARBPL = IT_SECTION-ZSUPH.

*    PERFORM get_target_time USING    l_date l_time l_wktime l_arbpl
*                            CHANGING l_target_time.

*    IF l_target_time >= w_curr_time.
    IF IT_WIP-ZITIME >= IT_SECTION-LIMIT_TIME.
** Changed by Park On 11/22/13
      CLEAR: LT_SIDE_TRACK, LT_SIDE_TRACK[].
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE LT_SIDE_TRACK
        FROM ZTPP_SIDE_TRACK
       WHERE MODEL_CODE = IT_WIP-MODEL
         AND BODY_NO    = IT_WIP-BODY_NO
         AND ST_STATUS  = 'I'.
      IF SY-SUBRC = 0.
        IT_WIP_DELAY = IT_WIP.
        APPEND IT_WIP_DELAY.
      ENDIF.
** End of change 11/22/13

    ELSE.
      IT_WIP_DELAY = IT_WIP.
      IT_WIP_DELAY-ZTTIME = L_TARGET_TIME.
      PERFORM GET_DELAY_HOUR USING L_ARBPL
              IT_WIP_DELAY-ZITIME IT_WIP_DELAY-ZDHOUR.

      IF IT_WIP_DELAY-ZDHOUR =< IT_SECTION-ZDURA.
** Changed by Park On 11/22/13
*        CONTINUE.
        CLEAR: LT_SIDE_TRACK, LT_SIDE_TRACK[].
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE LT_SIDE_TRACK
          FROM ZTPP_SIDE_TRACK
         WHERE MODEL_CODE = IT_WIP-MODEL
           AND BODY_NO    = IT_WIP-BODY_NO
           AND ST_STATUS  = 'I'.
        IF SY-SUBRC <> 0.
          CONTINUE.
        ENDIF.
** End of change 11/22/13
      ENDIF.

** Changed by Park On 11/22/13
*      IF IT_WIP_DELAY-ZDHOUR >= V_DELHR.
*        APPEND IT_WIP_DELAY.
*      ENDIF.
      APPEND IT_WIP_DELAY.
** End of change 11/22/13

    ENDIF.
  ENDLOOP.


ENDFORM.                    " GET_DELAY_VEHICLE
*&---------------------------------------------------------------------*
*&      Form  GET_DELAY_HOUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DELAY_HOUR USING P_SHOP P_FR_TIME P_GAP.
  DATA: L_GAP TYPE P DECIMALS 2.

  PERFORM CALCULATE_DELAY_TIME USING:
          P_SHOP P_FR_TIME W_CURR_TIME L_GAP.
  P_GAP = L_GAP.
ENDFORM.                    " GET_DELAY_HOUR
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_DELAY_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SHOP  text
*      -->P_IT_DELAY_ZITIME  text
*      -->P_W_CURR_TIME  text
*      -->P_L_GAP  text
*----------------------------------------------------------------------*
FORM CALCULATE_DELAY_TIME USING PV_SHOP PV_FROM_TIME
                                PV_TO_TIME PV_TIME_GAP.
  DATA: LV_TABIX LIKE SY-TABIX.

  PERFORM GET_FIRST_TIME_ZONE USING PV_SHOP PV_FROM_TIME LV_TABIX.

  LOOP AT IT_WORK_TIME  FROM LV_TABIX
                       WHERE SHOP  =   PV_SHOP.
    IF IT_WORK_TIME-WOSEC EQ 0 OR IT_WORK_TIME-OPSEC EQ 0.
      IF IT_WORK_TIME-WOFRM <= PV_TO_TIME AND
         IT_WORK_TIME-WOEND >= PV_TO_TIME.
        EXIT.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF IT_WORK_TIME-WOFRM > PV_TO_TIME.
      EXIT.
    ENDIF.

    IF IT_WORK_TIME-WOFRM <= PV_FROM_TIME AND
       IT_WORK_TIME-WOEND >= PV_TO_TIME.
      PERFORM GET_TIME_GAP USING PV_FROM_TIME PV_TO_TIME PV_TIME_GAP.
      EXIT.
    ENDIF.

    IF IT_WORK_TIME-WOFRM <= PV_FROM_TIME AND
       IT_WORK_TIME-WOEND >= PV_FROM_TIME.
      PERFORM GET_TIME_GAP USING PV_FROM_TIME IT_WORK_TIME-WOEND
                                 PV_TIME_GAP.
      CONTINUE.
    ENDIF.

    IF IT_WORK_TIME-WOFRM <= PV_TO_TIME AND
       IT_WORK_TIME-WOEND >= PV_TO_TIME.
      PERFORM GET_TIME_GAP USING IT_WORK_TIME-WOFRM PV_TO_TIME
                                 PV_TIME_GAP.
      EXIT.
    ENDIF.

    PV_TIME_GAP = PV_TIME_GAP + IT_WORK_TIME-OPSEC.
  ENDLOOP.

  PV_TIME_GAP = PV_TIME_GAP / 3600.
ENDFORM.                    " CALCULATE_DELAY_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_FIRST_TIME_ZONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_SHOP  text
*      -->P_PV_FROM_TIME  text
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM GET_FIRST_TIME_ZONE USING PV_SHOP PV_FROM_TIME PV_TABIX.
  DATA: LV_FROM_TIME(14).

  READ TABLE IT_WORK_TIME WITH KEY SHOP = PV_SHOP
                          BINARY SEARCH.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M06.
  ENDIF.

  CONCATENATE PV_FROM_TIME(10) IT_WORK_TIME-WOFRM+10(4)
         INTO LV_FROM_TIME.

  READ TABLE IT_WORK_TIME WITH KEY SHOP  = PV_SHOP
                                   WOFRM = LV_FROM_TIME
                          BINARY SEARCH.
  IF SY-SUBRC NE 0.
    LOOP AT IT_WORK_TIME WHERE SHOP  = PV_SHOP
                           AND WOFRM >= PV_FROM_TIME.
      MOVE: SY-TABIX TO PV_TABIX.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.
  ENDIF.

  IF PV_FROM_TIME >= LV_FROM_TIME.
    PV_TABIX = SY-TABIX.
  ELSE.
    PV_TABIX = SY-TABIX - 1.
  ENDIF.
ENDFORM.                    " GET_FIRST_TIME_ZONE

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WORKING_TIME .

  PERFORM GET_BASE_TIME.

  CLEAR: IT_WORK_TIME. REFRESH: IT_WORK_TIME.

  PERFORM GET_WORKING_TIME USING 'B' V_BDATE_B.
  PERFORM GET_WORKING_TIME USING 'P' V_BDATE_P.
  PERFORM GET_WORKING_TIME USING 'T' V_BDATE_T.

  SORT IT_WORK_TIME BY SHOP WOFRM WOEND.

ENDFORM.                    " READ_WORKING_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1871   text
*      -->P_V_BDATE_B  text
*----------------------------------------------------------------------*
FORM GET_WORKING_TIME USING PV_SHOP PV_BDATE.
  DATA: LV_GAP TYPE I.

  DATA: LT_WORK_TIME  LIKE ZSMM_WORKING_TIME OCCURS 0 WITH HEADER LINE.

  CHECK PV_BDATE IS NOT INITIAL.

  LV_GAP = W_SYS_DATE - PV_BDATE + 1.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
    EXPORTING
      I_DATUM              = PV_BDATE
      I_DAY                = LV_GAP
      I_ARBPL              = PV_SHOP
    TABLES
      T_WORKING_TIME       = LT_WORK_TIME
    EXCEPTIONS
      CANNOT_READ_DAYNAME  = 1
      INCORRECT_SHIFT_INFO = 2
      INCORRECT_CAPA_INFO  = 3
      OTHERS               = 4.

*  CALL FUNCTION 'Z_fPP_GET_WORKING_TIME'
*    EXPORTING
*      I_DATUM                     = PV_BDATE
*      I_WERKS                     = C_WERKS
*      I_ARBPL                     = PV_SHOP
*      I_DAY                       = LV_GAP
*    TABLES
*      T_0160                      = LT_WORK_TIME
*    EXCEPTIONS
*      INCORRECT_PLANT_VALUE       = 1
*      INCORRECT_WORK_CENTER_VALUE = 2
*      INCORRECT_TIME_FLAG         = 3
*      NO_WORKING_SCHEDULE         = 4
*      INCORRECT_TIME_ZONE         = 5
*      UNKNOWN_ERROR               = 6
*      OTHERS                      = 7.
  IF SY-SUBRC <> 0.
    MESSAGE E000(ZZ) WITH TEXT-M02 PV_SHOP.
  ENDIF.

  LOOP AT LT_WORK_TIME.
    CLEAR: IT_WORK_TIME.
    MOVE: PV_SHOP TO IT_WORK_TIME-SHOP.
    MOVE-CORRESPONDING LT_WORK_TIME TO IT_WORK_TIME.

    APPEND IT_WORK_TIME.

  ENDLOOP.

ENDFORM.                    " GET_WORKING_TIME
*&--------------------------------------------------------------------
*&      Form  GET_TIME_GAP
*&--------------------------------------------------------------------
*       text
*--------------------------------------------------------------------
*      -->P_PV_FROM_TIME  text
*      -->P_LV_TO_TIME  text
*      -->P_PV_TIME_GAP  text
*--------------------------------------------------------------------
FORM GET_TIME_GAP USING PV_TIME_F PV_TIME_T PV_TIME_GAP.
  DATA: LV_TIME_F   LIKE   SY-UZEIT,
        LV_TIME_T   LIKE   SY-UZEIT.

  MOVE: PV_TIME_F+8(6) TO LV_TIME_F,
        PV_TIME_T+8(6) TO LV_TIME_T.

  IF LV_TIME_F < LV_TIME_T.
    PV_TIME_GAP = PV_TIME_GAP +
                  ( LV_TIME_T - LV_TIME_F ) *
                  ( IT_WORK_TIME-OPSEC / IT_WORK_TIME-WOSEC ).
  ELSEIF LV_TIME_F > LV_TIME_T.
    PV_TIME_GAP = PV_TIME_GAP +
                  ( 86400 - ( LV_TIME_F - LV_TIME_T ) ) *
                  ( IT_WORK_TIME-OPSEC / IT_WORK_TIME-WOSEC ).
  ELSEIF LV_TIME_F = LV_TIME_T.
    " Time Gap is 0
  ENDIF.
ENDFORM.                    " GET_TIME_GAP
*&---------------------------------------------------------------------*
*&      Form  GET_BASE_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BASE_TIME .
  DATA: LV_BTIME(14),
        L_DATE     LIKE SY-DATUM,
        L_PRE_DATE LIKE SY-DATUM,
        L_BDATE    LIKE SY-DATUM,
        L_DAY_GAP  TYPE I.

  L_DATE = W_SYS_DATE - 200.
  L_PRE_DATE = W_SYS_DATE - 1.

  CLEAR: V_BDATE_B, V_BDATE_P, V_BDATE_T.

* " Read minimum base date of Body Shop
  CLEAR: LV_BTIME.
  SELECT SINGLE MIN( ZTTIME ) INTO LV_BTIME
    FROM ZTPP_DELAY_VEH AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                ON A~ZSECID = B~ZSECID
                               AND A~SEQ    = B~SEQ
   WHERE DATUM = L_PRE_DATE
     AND ZSUPH = 'B'. " ST_CRITICAL_RP-P_IN_RP.
  IF L_BDATE IS NOT INITIAL.
    MOVE: LV_BTIME(8) TO L_BDATE.
    L_DAY_GAP = W_SYS_DATE - L_BDATE.
    IF L_DAY_GAP > 200.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ELSE.
    CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
  ENDIF.

  PERFORM GET_YESTERDAY USING LV_BTIME V_BDATE_B 'B'.

  " Read minimum base date of Paint Shop
  CLEAR: LV_BTIME.
  SELECT SINGLE MIN( ZTTIME ) INTO LV_BTIME
    FROM ZTPP_DELAY_VEH AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                ON A~ZSECID = B~ZSECID
                               AND A~SEQ    = B~SEQ
   WHERE DATUM = L_PRE_DATE
     AND ZSUPH = 'P'. " ST_CRITICAL_RP-P_IN_RP.
  IF SY-SUBRC EQ 0.
    MOVE: LV_BTIME(8) TO L_BDATE.
    L_DAY_GAP = W_SYS_DATE - L_BDATE.
    IF L_DAY_GAP > 200.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ELSE.
    CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
  ENDIF.
  PERFORM GET_YESTERDAY USING LV_BTIME V_BDATE_P 'P'.

*  " Read minimum base date of Trim Shop
  CLEAR: LV_BTIME.
  SELECT SINGLE MIN( ZTTIME ) INTO LV_BTIME
    FROM ZTPP_DELAY_VEH AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                ON A~ZSECID = B~ZSECID
                               AND A~SEQ    = B~SEQ
   WHERE DATUM = L_PRE_DATE
     AND ZSUPH = 'T'. " ST_CRITICAL_RP-P_IN_RP.
  IF SY-SUBRC EQ 0.
    MOVE: LV_BTIME(8) TO L_BDATE.
    L_DAY_GAP = W_SYS_DATE - L_BDATE.
    IF L_DAY_GAP > 200.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ELSE.
    CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
  ENDIF.
  PERFORM GET_YESTERDAY USING LV_BTIME V_BDATE_T 'T'.
ENDFORM.                    " GET_BASE_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_YESTERDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_BDATE_B  text
*----------------------------------------------------------------------*
FORM GET_YESTERDAY USING PV_BTIME PV_BDATE PV_SHOP.
  " Get yesterday(Working Day Criteria)

  DATA: LV_DATE LIKE SY-DATUM,
        LV_TIME LIKE SY-UZEIT.

  CHECK PV_BTIME IS NOT INITIAL.

  MOVE: PV_BTIME(8)   TO LV_DATE,
        PV_BTIME+8(6) TO LV_TIME.

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      C_DATE                   = LV_DATE
      C_TIME                   = LV_TIME
      OPCODE                   = '-'
      WKTIME                   = 1440
      WERKS                    = 'P001'
      ARBPL                    = PV_SHOP
    IMPORTING
      T_DATE                   = PV_BDATE
    EXCEPTIONS
      ERROR_OPERATION          = 1
      CANNOT_READ_WORKING_TIME = 2
      OTHERS                   = 3.
  IF SY-SUBRC NE 0.
    PV_BDATE = PV_BDATE - 60.
  ENDIF.
ENDFORM.                    " GET_YESTERDATE
*&---------------------------------------------------------------------*
*&      Form  GET_TARGET_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TARGET_TIME  text
*----------------------------------------------------------------------*
FORM GET_TARGET_TIME USING P_DATE P_TIME P_WKTIME P_ARBPL
      CHANGING P_TARGET_TIME.

  DATA:   L_WDATE LIKE SY-DATUM,
          L_WTIME LIKE SY-UZEIT,
          L_WDATE_C8(8).

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      C_DATE                   = P_DATE
      C_TIME                   = P_TIME
      OPCODE                   = '+'
      WKTIME                   = P_WKTIME
      WERKS                    = 'P001'
      ARBPL                    = P_ARBPL
    IMPORTING
      T_DATE                   = L_WDATE
      T_TIME                   = L_WTIME
    EXCEPTIONS
      ERROR_OPERATION          = 1
      CANNOT_READ_WORKING_TIME = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH 'Getting work date failed ' IT_WIP-ZSECID
            IT_WIP-OBJEK.
  ENDIF.

  L_WDATE_C8 = L_WDATE.
  CONCATENATE L_WDATE_C8 L_WTIME INTO P_TARGET_TIME.
ENDFORM.                    " GET_TARGET_TIME
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DELAY_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_HISTORY .
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I,
        L_ZITIME(19).

  CALL METHOD WC_GRID_DETAIL->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS[]
      ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = 'Message'
        TXT2  = SY-SUBRC
        TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.
  REFRESH: IT_HISTORY.
  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC = 0.

    READ TABLE IT_DETAIL INDEX LT_ROWS-INDEX.
    IF IT_DETAIL-BODY_NO = ' '.
      MESSAGE E000(ZZ) WITH TEXT-M01.
    ENDIF.

    SELECT DATUM  A~ZSECID MODEL_CODE BODY_NO A~ZRPFR ZITIME A~ZRPTO
           ZTTIME ZDHOUR ZETA ZESOFF ZSFDBK ZPFDBK ZLFDBK ZSTTXT ZSECTX
      INTO CORRESPONDING FIELDS OF TABLE IT_HISTORY
      FROM ZTPP_DELAY_VEH  AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                   ON A~ZSECID = B~ZSECID
                                  AND A~SEQ    = B~SEQ
      WHERE MODEL_CODE = IT_DETAIL-MODEL
        AND BODY_NO    = IT_DETAIL-BODY_NO.

    V_MODEL_200   = IT_DETAIL-MODEL.
    V_BODY_NO_200 = IT_DETAIL-BODY_NO.
  ELSE.
    CLEAR: V_MODEL_200, V_BODY_NO_200.
  ENDIF.

  LOOP AT IT_HISTORY.
    MOVE: IT_HISTORY-ZITIME+4(4) TO L_ZITIME(4),
          IT_HISTORY-ZITIME(4)   TO L_ZITIME+4(4),
          IT_HISTORY-ZITIME+8(6) TO L_ZITIME+8(6).

    MOVE: L_ZITIME TO IT_HISTORY-ZITIME_OUT.

    MODIFY IT_HISTORY.
  ENDLOOP.

  SORT IT_HISTORY BY DATUM DESCENDING.

  CALL SCREEN 0200 STARTING AT 10  5
                   ENDING   AT 120 25.

ENDFORM.                    " DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTROL_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_CONTROL_0200 OUTPUT.
  PERFORM CREATE_SPLITTER USING '0200' 1.
  PERFORM CREATE_GRID     USING 'HISTORY' '1' '1'.
ENDMODULE.                 " CREATE_CONTROL_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  OK_CODE  = SY-UCOMM.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      CLEAR: SY-UCOMM.
      PERFORM REFRESH_HISTORY.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_MODULE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIST_BOX_MODULE OUTPUT.
  DATA: L_ATINN              LIKE CABN-ATINN.

  CLEAR : IT_LIST, ST_VALUE.
  REFRESH: IT_LIST.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT A~ATWRT AS KEY B~ATWTB AS TEXT
    APPENDING CORRESPONDING FIELDS OF TABLE IT_LIST
    FROM CAWN AS A INNER JOIN CAWNT AS B
                      ON B~ATINN = A~ATINN
                     AND B~ATZHL = A~ATZHL
   WHERE A~ATINN = L_ATINN
     AND B~SPRAS = SY-LANGU.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'V_MODEL_200'
      VALUES = IT_LIST.

ENDMODULE.                 " LIST_BOX_MODULE  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_HISTORY .
  DATA: L_ZITIME(19).

  SELECT A~DATUM A~ZSECID MODEL_CODE BODY_NO A~ZRPFR ZITIME A~ZRPTO
         ZTTIME  ZDHOUR ZETA ZESOFF ZSFDBK ZPFDBK ZLFDBK ZSTTXT ZSECTX
     INTO CORRESPONDING FIELDS OF TABLE IT_HISTORY
     FROM ZTPP_DELAY_VEH AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                 ON A~ZSECID = B~ZSECID
                                AND A~SEQ    = B~SEQ
     WHERE MODEL_CODE = V_MODEL_200
       AND BODY_NO    = V_BODY_NO_200.

  LOOP AT IT_HISTORY.
    MOVE: IT_HISTORY-ZITIME+4(4) TO L_ZITIME(4),
          IT_HISTORY-ZITIME(4)   TO L_ZITIME+4(4),
          IT_HISTORY-ZITIME+8(6) TO L_ZITIME+8(6).

    MOVE: L_ZITIME TO IT_HISTORY-ZITIME_OUT.

    MODIFY IT_HISTORY.
  ENDLOOP.

  SORT IT_HISTORY BY DATUM DESCENDING.
ENDFORM.                    " REFRESH_HISTORY
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_HISTORY .
  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                    'S' 'DATUM'        ' ',
                                    ' ' 'COLTEXT'      'Date',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'OUTPUTLEN'    '10',
                                    'E' 'EMPHASIZE'    'C100',

                                    'S' 'ZSECTX'        ' ',
                                    ' ' 'COLTEXT'      'Section',
                                    ' ' 'FIX_COLUMN'   'X',
                                    ' ' 'EMPHASIZE'    'C100',
                                    'E' 'OUTPUTLEN'    '15',

                                    'S' 'ZRPFR'       ' ',
                                    ' ' 'COLTEXT'      'Fr RP',
                                    ' ' 'OUTPUTLEN'    '5',
                                    'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZRPTO'    ' ',
                                  ' ' 'COLTEXT'      'To RP',
                                  ' ' 'OUTPUTLEN'    '5',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZITIME_OUT'       ' ',
                                  ' ' 'COLTEXT'      'Input Time',
                               ' ' 'EDIT_MASK'    '__/__/____ __:__:__',
                                  ' ' 'OUTPUTLEN'    '19',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZDHOUR'       ' ',
                                  ' ' 'COLTEXT'      'Delay Hr',
                                  ' ' 'OUTPUTLEN'    '8',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZSFDBK'       ' ',
                                  ' ' 'COLTEXT'      'Shop Feedback',
                                  ' ' 'OUTPUTLEN'    '20',
                                  'E' 'EMPHASIZE'    'C250',



                                  'S' 'ZLFDBK'       ' ',
                                  ' ' 'COLTEXT'      'System Feedback',
                                  ' ' 'OUTPUTLEN'    '20',
                                  'E' 'EMPHASIZE'    'C250',



                                  'S' 'ZPFDBK'       ' ',
                                  ' ' 'COLTEXT'      'PC Feedback',
                                  ' ' 'OUTPUTLEN'    '20',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZSTTXT'       ' ',
                                  ' ' 'COLTEXT'      'Side Track',
                                  ' ' 'OUTPUTLEN'    '20',
                                  'E' 'EMPHASIZE'    'C250'.

ENDFORM.                    " SET_SCREEN_FIELDS_HISTORY
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAVE_DATA .

  DATA: LT_DELAY LIKE ZTPP_DELAY_VEH OCCURS 0 WITH HEADER LINE.

  DATA: LT_DETAIL LIKE IT_DETAIL OCCURS 0 WITH HEADER LINE.

  LT_DETAIL[] = IT_DETAIL[].

  SORT IT_WIP       BY ZSECID SEQ MODEL BODY_NO.
  SORT IT_WIP_DELAY BY ZSECID SEQ MODEL BODY_NO.
  SORT LT_DETAIL    BY ZSECID SEQ MODEL BODY_NO.

  IF W_CURRENT_SHOP_DATE = V_SHOP_DATE AND
     W_MODE              = 'REFRESH'.

    LOOP AT IT_WIP_DELAY.
      CLEAR LT_DELAY.

      MOVE-CORRESPONDING IT_WIP_DELAY TO LT_DELAY.
      LT_DELAY-DATUM      = V_SHOP_DATE.
      LT_DELAY-MODEL_CODE = IT_WIP_DELAY-MODEL.
      LT_DELAY-AENAM      = SY-UNAME.
      LT_DELAY-AEDAT      = W_SYS_DATE.
      LT_DELAY-AEZET      = W_SYS_TIME.

** Changed by Park On 11/22/13
*      LT_DELAY-ZTEMP      = 'X'.
** End of change 11/22/13

      READ TABLE LT_DETAIL WITH KEY ZSECID  = IT_WIP_DELAY-ZSECID
                                    SEQ     = IT_WIP_DELAY-SEQ
                                    MODEL   = IT_WIP_DELAY-MODEL
                                    BODY_NO = IT_WIP_DELAY-BODY_NO
                           BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_DELAY-ZETA     = LT_DETAIL-ZETA.
        LT_DELAY-ZESOFF   = LT_DETAIL-ZESOFF.
        LT_DELAY-ZSFDBK   = LT_DETAIL-ZSFDBK.
        LT_DELAY-ZPFDBK   = LT_DETAIL-ZPFDBK.
        LT_DELAY-ZLFDBK   = LT_DETAIL-ZLFDBK.
        LT_DELAY-ZSTTXT   = LT_DETAIL-ZSTTXT.
        LT_DELAY-URGENCY  = LT_DETAIL-URGENCY.
        LT_DELAY-URGCDATE = LT_DETAIL-URGCDATE.
        LT_DELAY-FLET     = LT_DETAIL-FLET.

        MOVE LT_DETAIL TO IT_WIP_DELAY.
      ENDIF.

** Changed by Park On 11/22/13
*      READ TABLE IT_WIP WITH KEY ZSECID  = IT_DETAIL-ZSECID
*                                 SEQ     = IT_DETAIL-SEQ
*                                 MODEL   = IT_DETAIL-MODEL
*                                 BODY_NO = IT_DETAIL-BODY_NO
*                        BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        MOVE-CORRESPONDING LT_DELAY TO IT_WIP.
*        MODIFY IT_WIP INDEX SY-TABIX.
*      ENDIF.
** End of change 11/22/13

      READ TABLE IT_DELAY_TMP WITH KEY DATUM      = V_SHOP_DATE
                                       ZSECID     = LT_DETAIL-ZSECID
                                       SEQ        = LT_DETAIL-SEQ
                                       MODEL_CODE = LT_DETAIL-MODEL
                                      BODY_NO     = LT_DETAIL-BODY_NO
                              BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_DELAY-ERNAM      = IT_DELAY_TMP-ERNAM.
        LT_DELAY-ERDAT      = IT_DELAY_TMP-ERDAT.
        LT_DELAY-ERZET      = IT_DELAY_TMP-ERZET.
      ELSE.
        LT_DELAY-ERNAM      = SY-UNAME.
        LT_DELAY-ERDAT      = W_SYS_DATE.
        LT_DELAY-ERZET      = W_SYS_TIME.
      ENDIF.

      APPEND LT_DELAY.

      MODIFY IT_WIP_DELAY.

    ENDLOOP.





*    LOOP AT it_detail.
*      CLEAR lt_delay.
*
*      MOVE-CORRESPONDING it_detail TO lt_delay.
*      lt_delay-datum      = v_shop_date.
*      lt_delay-model_code = it_detail-model.
*      lt_delay-ztemp      = 'X'.
*
*      READ TABLE it_delay_tmp WITH KEY datum      = v_shop_date
*                                       zsecid     = it_detail-zsecid
*                                       seq        = it_detail-seq
*                                       model_code = it_detail-model
*                                      body_no    = it_detail-body_no
*                              BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        lt_delay-ernam      = it_delay_tmp-ernam.
*        lt_delay-erdat      = it_delay_tmp-erdat.
*        lt_delay-erzet      = it_delay_tmp-erzet.
*      ELSE.
*        lt_delay-ernam      = sy-uname.
*        lt_delay-erdat      = w_sys_date.
*        lt_delay-erzet      = w_sys_time.
*      ENDIF.
*
*      lt_delay-aenam      = sy-uname.
*      lt_delay-aedat      = w_sys_date.
*      lt_delay-aezet      = w_sys_time.
*
*      APPEND lt_delay.
*
*      READ TABLE it_wip WITH KEY zsecid = it_detail-zsecid
*                                 seq    = it_detail-seq
*                                 model  = it_detail-model
*                                 body_no = it_detail-body_no
*                        BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        MOVE: it_detail TO it_wip.
*        MODIFY it_wip INDEX sy-tabix.
*      ENDIF.
*
*      READ TABLE it_wip_delay WITH KEY zsecid = it_detail-zsecid
*                                       seq    = it_detail-seq
*                                       model  = it_detail-model
*                                       body_no = it_detail-body_no
*                              BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        MOVE: it_detail TO it_wip_delay.
*        MODIFY it_wip_delay INDEX sy-tabix.
*      ENDIF.
*    ENDLOOP.

    MODIFY ZTPP_DELAY_VEH FROM TABLE LT_DELAY.
    IF SY-SUBRC = 0.
      COMMIT WORK AND WAIT.
      MESSAGE S009 WITH 'Table ZTPP_DELAY_VEH updated successfully'.
    ENDIF.
  ELSE.
    MESSAGE E009 WITH 'Delay Vehicle List can be saved.'
                      'REFRESH mode.'.
  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_EDIT_FIELDS .
  DATA: LT_CELLTAB TYPE LVC_T_STYL,
       W_CELLTAB TYPE LVC_S_STYL,
       L_INDEX TYPE I.

  LOOP AT IT_DETAIL.
    L_INDEX = SY-TABIX.
    REFRESH LT_CELLTAB.
    CLEAR: W_CELLTAB.

    IF OK_CODE = 'FEEDBACKPC' OR
       OK_CODE = 'FEEDBACK'.
      W_CELLTAB-FIELDNAME = 'ZETA'.
      W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
      IF OK_CODE = 'FEEDBACK'.
        W_CELLTAB-FIELDNAME = 'ZSFDBK'.
        W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
        W_CELLTAB-FIELDNAME = 'ZPFDBK'.
        W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
      ELSE.
        W_CELLTAB-FIELDNAME = 'ZPFDBK'.
        W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
        W_CELLTAB-FIELDNAME = 'ZSFDBK'.
        W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
      ENDIF.
    ELSE.
      W_CELLTAB-FIELDNAME = 'ZETA'.
      W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

      W_CELLTAB-FIELDNAME = 'ZSFDBK'.
      W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

      W_CELLTAB-FIELDNAME = 'ZPFDBK'.
      W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
    ENDIF.

    W_CELLTAB-FIELDNAME = 'INDEX'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZSECTX'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'RP_STATUS'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'PROGRESS'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'MODEL'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'BODY_NO'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'EXTC'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'INTC'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZETA'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZDHOUR'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZESOFF'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'URGENCY'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'URGCDATE'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'DEALER_NO'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ALLOC_DATE'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZSTTXT'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZLFDBK'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZITIME'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'FLET'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'WORDER'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'MI'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'OCN'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    W_CELLTAB-FIELDNAME = 'ZITIME_OUT'.
    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    REFRESH: IT_DETAIL-CELLTAB.

    INSERT LINES OF LT_CELLTAB INTO TABLE IT_DETAIL-CELLTAB.
    MODIFY IT_DETAIL INDEX L_INDEX.
  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_FIELDS
*&---------------------------------------------------------------------*
*&      Form  FEEDBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3901   text
*----------------------------------------------------------------------*
FORM FEEDBACK USING P_TYPE.

  IF IT_HEADER-LINES = 1.
    CLEAR: OK_CODE.
    MESSAGE E009 WITH 'Please select valid function'.
  ENDIF.

  IF NOT ( W_CURRENT_SHOP_DATE = V_SHOP_DATE AND
           W_MODE              = 'REFRESH' ).
    MESSAGE E000 WITH 'Feedback can be input'
                      'REFRESH mode only.'.
  ENDIF.
ENDFORM.                    " FEEDBACK
*&---------------------------------------------------------------------*
*&      Module  INIT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_DATA OUTPUT.

ENDMODULE.                 " INIT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_ZPPA00001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ZPPA00001 .
  CALL TRANSACTION 'ZPPA00001'.
ENDFORM.                    " CALL_ZPPA00001
*&---------------------------------------------------------------------*
*&      Form  CALL_SIDETRACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_SIDETRACK .
  CALL TRANSACTION 'ZPPR00013' AND SKIP FIRST SCREEN.
ENDFORM.                    " CALL_SIDETRACK
*&---------------------------------------------------------------------*
*&      Form  READ_LIMIT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_LIMIT_TIME .
  DATA:   L_WDATE  LIKE SY-DATUM,
          L_WTIME  LIKE SY-UZEIT,
          L_WDATE_C8(8),
          L_WKTIME TYPE I,
          L_ARBPL  LIKE CRHD-ARBPL.

  L_WKTIME = IT_SECTION-ZDURA * 60.
  L_ARBPL  = IT_SECTION-ZSUPH.

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      C_DATE                   = W_SYS_DATE
      C_TIME                   = W_SYS_TIME
      OPCODE                   = '-'
      WKTIME                   = L_WKTIME
      WERKS                    = 'P001'
      ARBPL                    = L_ARBPL
    IMPORTING
      T_DATE                   = L_WDATE
      T_TIME                   = L_WTIME
    EXCEPTIONS
      ERROR_OPERATION          = 1
      CANNOT_READ_WORKING_TIME = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH 'Getting work date failed ' IT_SECTION-ZSECID.
  ENDIF.

  L_WDATE_C8 = L_WDATE.
  CONCATENATE L_WDATE_C8 L_WTIME INTO IT_SECTION-LIMIT_TIME.
ENDFORM.                    " READ_LIMIT_TIME
*&---------------------------------------------------------------------*
*&      Form  SAVE_FOR_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_FOR_REFRESH .
  DATA: LT_DELAY_VEH LIKE ZTPP_DELAY_VEH OCCURS 0 WITH HEADER LINE.

  DATA: L_LINES TYPE I,
        L_LINES_TXT(6).

  SORT IT_SECTION BY ZSECID SEQ.


*  CHECK w_current_shop_date = v_shop_date.

*  CHECK sy-uname(3) NE 'HIS'.   "Check HISNA ID

  LOOP AT IT_WIP_DELAY.
    CLEAR: LT_DELAY_VEH.

    MOVE-CORRESPONDING IT_WIP_DELAY TO LT_DELAY_VEH.
    LT_DELAY_VEH-DATUM = V_SHOP_DATE.
    LT_DELAY_VEH-MODEL_CODE = IT_WIP_DELAY-MODEL.
    LT_DELAY_VEH-ZTEMP      = 'X'.

    READ TABLE IT_DELAY_TMP
      WITH KEY DATUM      = V_SHOP_DATE
               ZSECID     = IT_WIP_DELAY-ZSECID
               SEQ        = IT_WIP_DELAY-SEQ
               MODEL_CODE = IT_WIP_DELAY-MODEL
               BODY_NO    = IT_WIP_DELAY-BODY_NO
      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_DELAY_VEH-ERNAM      = IT_DELAY_TMP-ERNAM.
      LT_DELAY_VEH-ERDAT      = IT_DELAY_TMP-ERDAT.
      LT_DELAY_VEH-ERZET      = IT_DELAY_TMP-ERZET.
    ELSE.
      LT_DELAY_VEH-ERNAM      = SY-UNAME.
      LT_DELAY_VEH-ERDAT      = W_SYS_DATE.
      LT_DELAY_VEH-ERZET      = W_SYS_TIME.

      MOVE: LT_DELAY_VEH TO IT_DELAY_TMP.
      APPEND IT_DELAY_TMP.
    ENDIF.

    LT_DELAY_VEH-AENAM      = SY-UNAME.
    LT_DELAY_VEH-AEDAT      = W_SYS_DATE.
    LT_DELAY_VEH-AEZET      = W_SYS_TIME.

    APPEND LT_DELAY_VEH.
  ENDLOOP.

  MODIFY ZTPP_DELAY_VEH FROM TABLE LT_DELAY_VEH.

  DESCRIBE TABLE LT_DELAY_VEH LINES L_LINES.

  WRITE: L_LINES TO L_LINES_TXT.

  MESSAGE S000 WITH L_LINES_TXT TEXT-M02.

ENDFORM.                    " SAVE_FOR_REFRESH
*&---------------------------------------------------------------------*
*&      Form  READ_DELAY_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DELAY_TEMP .

  CHECK W_CURRENT_SHOP_DATE = V_SHOP_DATE AND
        W_MODE              = 'REFRESH'.

** Changed by Park On 11/22/13
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DELAY_TMP
*    FROM ZTPP_DELAY_VEH
*   WHERE DATUM = V_SHOP_DATE
*     AND ZTEMP = 'X'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DELAY_TMP
    FROM ZTPP_DELAY_VEH
   WHERE DATUM = V_SHOP_DATE
     AND ZTEMP = ''.
** End of change 11/22/13

ENDFORM.                    " READ_DELAY_TEMP
*&---------------------------------------------------------------------*
*&      Form  READ_OTHER_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WIP_DELAY  text
*----------------------------------------------------------------------*
FORM READ_OTHER_INFO TABLES PT_DELAY STRUCTURE ST_DETAIL.

  DATA: LT_SIDE_TRACK LIKE ZTPP_SIDE_TRACK OCCURS 0 WITH HEADER LINE.

  REFRESH: IT_DELAY_IF, IT_STRACK, IT_UM.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_DELAY_IF
    FROM ZTPP_DELAY_IF
     FOR ALL ENTRIES IN PT_DELAY
   WHERE MODEL_CODE =  PT_DELAY-MODEL
     AND BODY_NO    =  PT_DELAY-BODY_NO.
*     AND rp_status  >= pt_delay-zrpfr
*     AND rp_status  <= pt_delay-zrpto.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_STRACK
    FROM ZTPP_SIDE_TRACK
     FOR ALL ENTRIES IN PT_DELAY
   WHERE MODEL_CODE  = PT_DELAY-MODEL
     AND BODY_NO   = PT_DELAY-BODY_NO.

  SELECT * INTO TABLE IT_UM
    FROM ZTSD_UM
     FOR ALL ENTRIES IN PT_DELAY
   WHERE MODEL_CODE =  PT_DELAY-MODEL
     AND BODY_NO    =  PT_DELAY-BODY_NO
     AND STATUS     IN ('','F').

ENDFORM.                    " READ_OTHER_INFO
*&---------------------------------------------------------------------*
*&      Form  SET_OTHER_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WIP_DELAY  text
*----------------------------------------------------------------------*
FORM SET_OTHER_INFO TABLES PT_DELAY STRUCTURE ST_DETAIL.
  DATA: L_TABIX LIKE SY-TABIX.

  SORT IT_DELAY_IF   BY MODEL_CODE BODY_NO ZLDATIM   DESCENDING
                                           RP_STATUS DESCENDING.
  SORT IT_STRACK     BY MODEL_CODE BODY_NO ZSEQ DESCENDING.
  SORT IT_UM         BY MODEL_CODE BODY_NO.
  SORT IT_DELAY_TMP  BY DATUM ZSECID SEQ MODEL_CODE BODY_NO.
  SORT IT_DELAY_PREV BY DATUM ZSECID SEQ MODEL_CODE BODY_NO.

  LOOP AT PT_DELAY.
    READ TABLE IT_DELAY_IF WITH KEY MODEL_CODE = PT_DELAY-MODEL
                                    BODY_NO    = PT_DELAY-BODY_NO
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE: SY-TABIX TO L_TABIX.

      LOOP AT IT_DELAY_IF FROM L_TABIX
                         WHERE RP_STATUS >= PT_DELAY-ZRPFR
                           AND RP_STATUS <= PT_DELAY-ZRPTO.
        IF IT_DELAY_IF-MODEL_CODE NE PT_DELAY-MODEL   OR
           IT_DELAY_IF-BODY_NO    NE PT_DELAY-BODY_NO.
          EXIT.
        ENDIF.

        PT_DELAY-ZLFDBK  = IT_DELAY_IF-ZRSN.
        PT_DELAY-ZSYSTEM = IT_DELAY_IF-ZSYSTEM.

        EXIT.
      ENDLOOP.
    ENDIF.

    READ TABLE IT_UM WITH KEY MODEL_CODE = PT_DELAY-MODEL
                              BODY_NO    = PT_DELAY-BODY_NO
                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PT_DELAY-URGENCY  = IT_UM-URGENCY.
      PT_DELAY-URGCDATE = IT_UM-URGCDATE.
      PT_DELAY-FLET     = IT_UM-FLET.
    ENDIF.

    READ TABLE IT_DELAY_PREV WITH KEY ZSECID     = PT_DELAY-ZSECID
                                      SEQ        = PT_DELAY-SEQ
                                      MODEL_CODE = PT_DELAY-MODEL
                                      BODY_NO    = PT_DELAY-BODY_NO
                                      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PT_DELAY-ZETA   = IT_DELAY_PREV-ZETA.
      PT_DELAY-ZSFDBK = IT_DELAY_PREV-ZSFDBK.
      PT_DELAY-ZPFDBK = IT_DELAY_PREV-ZPFDBK.

      CLEAR: PT_DELAY-ZESOFF.
      PERFORM GET_ESTIMATED_SOFF USING PT_DELAY-ZETA
                                       PT_DELAY-ZDRDY
                                       PT_DELAY-ZESOFF.
    ENDIF.

    READ TABLE IT_DELAY_TMP WITH KEY ZSECID     = PT_DELAY-ZSECID
                                     SEQ        = PT_DELAY-SEQ
                                     MODEL_CODE = PT_DELAY-MODEL
                                     BODY_NO    = PT_DELAY-BODY_NO
                                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PT_DELAY-ZETA   = IT_DELAY_TMP-ZETA.
      PT_DELAY-ZSFDBK = IT_DELAY_TMP-ZSFDBK.
      PT_DELAY-ZPFDBK = IT_DELAY_TMP-ZPFDBK.

      CLEAR: PT_DELAY-ZESOFF.
      PERFORM GET_ESTIMATED_SOFF USING PT_DELAY-ZETA
                                       PT_DELAY-ZDRDY
                                       PT_DELAY-ZESOFF.
    ENDIF.

    READ TABLE IT_STRACK WITH KEY MODEL_CODE = PT_DELAY-MODEL
                                  BODY_NO    = PT_DELAY-BODY_NO
                         BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PT_DELAY-ZSTTXT = IT_STRACK-ZSTTXT.

      IF PT_DELAY-ZETA IS INITIAL AND
         PT_DELAY-ZRPFR     <= IT_STRACK-RP_CSTATUS AND
         PT_DELAY-ZRPTO     >= IT_STRACK-RP_CSTATUS AND
         IT_STRACK-ST_STATUS = 'I' AND
         IT_STRACK-ZETA IS NOT INITIAL.
        PT_DELAY-ZETA   = IT_STRACK-ZETA.

        CLEAR: PT_DELAY-ZESOFF.
        PERFORM GET_ESTIMATED_SOFF USING PT_DELAY-ZETA
                                         PT_DELAY-ZDRDY
                                         PT_DELAY-ZESOFF.
      ENDIF.
    ENDIF.

    MODIFY PT_DELAY.
  ENDLOOP.

ENDFORM.                    " SET_OTHER_INFO
*&---------------------------------------------------------------------*
*&      Form  READ_PREVIOUS_DELAY_VEH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PREVIOUS_DELAY_VEH .
  DATA: L_DATUM LIKE SY-DATUM.

  SELECT MAX( DATUM ) INTO L_DATUM
    FROM ZTPP_DELAY_VEH
   WHERE DATUM < W_CURRENT_SHOP_DATE.

  SELECT * INTO TABLE IT_DELAY_PREV
    FROM ZTPP_DELAY_VEH
   WHERE DATUM = L_DATUM.

ENDFORM.                    " READ_PREVIOUS_DELAY_VEH
*&---------------------------------------------------------------------*
*&      Form  EVENT_TRIGGER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EVENT_TRIGGER .
  DATA : L_VALID(1),
         L_REFRESH(1) TYPE C VALUE 'X'.

  CALL METHOD WC_GRID_DETAIL->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CALL METHOD WC_GRID_DETAIL->CHECK_CHANGED_DATA
    IMPORTING
      E_VALID   = L_VALID
    CHANGING
      C_REFRESH = L_REFRESH.

ENDFORM.                    " EVENT_TRIGGER
*&---------------------------------------------------------------------*
*&      Form  GET_ESTIMATED_SOFF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ZETA  text
*      -->P_LV_ZESOFF  text
*----------------------------------------------------------------------*
FORM GET_ESTIMATED_SOFF USING PV_ZETA PV_DAY PV_ZESOFF.
  DATA: LW_DATE    TYPE D,
        LW_INDEX   TYPE I,
        LW_DAYNR   LIKE HRVSCHED-DAYNR,
        LW_DAYFREE LIKE HRVSCHED-NODAY.

  IF PV_DAY EQ 0.
    PV_ZESOFF = PV_ZETA.
    EXIT.
  ENDIF.

  IF PV_ZETA IS INITIAL.
    CLEAR: PV_ZESOFF.
    EXIT.
  ENDIF.

  LW_DATE = PV_ZETA.

  DO.
    LW_DATE  = LW_DATE  + 1.

    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        LANGU               = SY-LANGU
        DATE                = LW_DATE
        CALID               = 'HM'
      IMPORTING
        DAYNR               = LW_DAYNR
        DAYFREE             = LW_DAYFREE
      EXCEPTIONS
        NO_LANGU            = 1
        NO_DATE             = 2
        NO_DAYTXT_FOR_LANGU = 3
        INVALID_DATE        = 4
        OTHERS              = 5.
    IF SY-SUBRC <> 0.
      RAISE CANNOT_READ_DAYNAME.
    ENDIF.

    IF LW_DAYFREE EQ 'X'.
      CONTINUE.
    ENDIF.

    LW_INDEX = LW_INDEX + 1.

    IF LW_INDEX >= PV_DAY OR LW_INDEX >= 200.
      EXIT.
    ENDIF.
  ENDDO.

  PV_ZESOFF = LW_DATE.
ENDFORM.                    " GET_ESTIMATED_SOFF
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_RTN .
  PERFORM CLEAR_DATA.
  PERFORM READ_SECTION_MASTER.
  PERFORM READ_PREVIOUS_DELAY_VEH.
  PERFORM READ_DELAY_TEMP.
  PERFORM READ_WIP.
  PERFORM MAKE_HEADER_FOR_DISPLAY.
ENDFORM.                    " DISPLAY_RTN
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER_FOR_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_HEADER_FOR_DISPLAY .
  DATA: LT_VEH LIKE TABLE OF ZTPP_DELAY_VEH WITH HEADER LINE,
        LW_TOTAL LIKE IT_HEADER,
        L_MODEL_DESC(40),
        L_LINES(1) TYPE N,
        L_ZITIME(14).

  REFRESH: IT_HEADER, IT_WIP_DELAY.
  CLEAR: L_LINES.
  L_LINES = L_LINES + 1.

  SORT IT_SECTION BY ZSECID SEQ.

  LOOP AT IT_WIP.
    PERFORM MAKE_HEADER_WIP USING C_WIP IT_WIP-RPID
                                  IT_WIP-WORDER  L_LINES.
  ENDLOOP.
  IF IT_HEADER[] IS INITIAL.
    CLEAR: IT_HEADER.
    MOVE: C_WIP TO IT_HEADER-KEY.
    APPEND IT_HEADER.
  ENDIF.
  L_LINES = L_LINES + 1.

  REFRESH LT_VEH.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_VEH
    FROM ZTPP_DELAY_VEH
   WHERE DATUM   = V_SHOP_DATE
** Changed by Park On 11/22/13
*    AND ZDHOUR >= V_DELHR
** End of change 11/22/13
     AND BODY_NO IN R_BODY.


  IF V_MODEL <> '*'.
    DELETE LT_VEH WHERE MODEL_CODE <> V_MODEL.
  ENDIF.

  SORT LT_VEH BY MODEL_CODE.
  LOOP AT LT_VEH.
    MOVE: LT_VEH-AEDAT TO W_PREV_DATE,
          LT_VEH-AEZET TO W_PREV_TIME.

    CONCATENATE LT_VEH-MODEL_CODE 'Delay QTY'
           INTO L_MODEL_DESC SEPARATED BY SPACE.

    PERFORM MAKE_HEADER_WIP USING L_MODEL_DESC  LT_VEH-ZRPFR
                                  LT_VEH-WORDER L_LINES .

    CLEAR: IT_WIP_DELAY.
    MOVE-CORRESPONDING LT_VEH TO IT_WIP_DELAY.
    IT_WIP_DELAY-MODEL = LT_VEH-MODEL_CODE.

    MOVE: LT_VEH-ZITIME+4(4) TO L_ZITIME(4),
          LT_VEH-ZITIME(4)   TO L_ZITIME+4(4),
          LT_VEH-ZITIME+8(6) TO L_ZITIME+8(6).

    MOVE: L_ZITIME TO IT_WIP_DELAY-ZITIME_OUT.

    CLEAR: IT_SECTION.

    READ TABLE IT_SECTION WITH KEY ZSECID = IT_WIP_DELAY-ZSECID
                                   SEQ    = IT_WIP_DELAY-SEQ
                          BINARY SEARCH.

    MOVE: IT_SECTION-ZSECTX TO IT_WIP_DELAY-ZSECTX.

    APPEND IT_WIP_DELAY.
  ENDLOOP.

  LOOP AT IT_HEADER FROM 2.
    LW_TOTAL-TOTAL = LW_TOTAL-TOTAL + IT_HEADER-TOTAL.
    LW_TOTAL-01QTY = LW_TOTAL-01QTY + IT_HEADER-01QTY.
    LW_TOTAL-02QTY = LW_TOTAL-02QTY + IT_HEADER-02QTY.
    LW_TOTAL-03QTY = LW_TOTAL-03QTY + IT_HEADER-03QTY.
    LW_TOTAL-04QTY = LW_TOTAL-04QTY + IT_HEADER-04QTY.
    LW_TOTAL-05QTY = LW_TOTAL-05QTY + IT_HEADER-05QTY.
    LW_TOTAL-06QTY = LW_TOTAL-06QTY + IT_HEADER-06QTY.
    LW_TOTAL-07QTY = LW_TOTAL-07QTY + IT_HEADER-07QTY.
    LW_TOTAL-08QTY = LW_TOTAL-08QTY + IT_HEADER-08QTY.
    LW_TOTAL-09QTY = LW_TOTAL-09QTY + IT_HEADER-09QTY.
    LW_TOTAL-10QTY = LW_TOTAL-10QTY + IT_HEADER-10QTY.
    LW_TOTAL-11QTY = LW_TOTAL-11QTY + IT_HEADER-11QTY.
    LW_TOTAL-12QTY = LW_TOTAL-12QTY + IT_HEADER-12QTY.
    LW_TOTAL-13QTY = LW_TOTAL-13QTY + IT_HEADER-13QTY.
    LW_TOTAL-14QTY = LW_TOTAL-14QTY + IT_HEADER-14QTY.
    LW_TOTAL-15QTY = LW_TOTAL-15QTY + IT_HEADER-15QTY.
    LW_TOTAL-16QTY = LW_TOTAL-16QTY + IT_HEADER-16QTY.
    LW_TOTAL-17QTY = LW_TOTAL-17QTY + IT_HEADER-17QTY.
    LW_TOTAL-18QTY = LW_TOTAL-18QTY + IT_HEADER-18QTY.
    LW_TOTAL-19QTY = LW_TOTAL-19QTY + IT_HEADER-19QTY.
    LW_TOTAL-20QTY = LW_TOTAL-20QTY + IT_HEADER-20QTY.
    LW_TOTAL-21QTY = LW_TOTAL-21QTY + IT_HEADER-21QTY.
    LW_TOTAL-22QTY = LW_TOTAL-22QTY + IT_HEADER-22QTY.
    LW_TOTAL-23QTY = LW_TOTAL-23QTY + IT_HEADER-23QTY.
    LW_TOTAL-24QTY = LW_TOTAL-24QTY + IT_HEADER-24QTY.
    LW_TOTAL-25QTY = LW_TOTAL-25QTY + IT_HEADER-25QTY.
    LW_TOTAL-26QTY = LW_TOTAL-26QTY + IT_HEADER-26QTY.
    LW_TOTAL-27QTY = LW_TOTAL-27QTY + IT_HEADER-27QTY.
  ENDLOOP.
  L_LINES = L_LINES + 1.
  LW_TOTAL-KEY = C_TOTAL.
  LW_TOTAL-LINES = L_LINES.
  APPEND LW_TOTAL TO IT_HEADER.
  SORT IT_HEADER BY LINES.
ENDFORM.                    " MAKE_HEADER_FOR_DISPLAY
