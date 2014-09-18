*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0006TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& # DATA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  TABLES
*&---------------------------------------------------------------------*
TABLES : equi, EQKT , T001W , iloa .
TABLES : CTU_PARAMS .
TABLES : sscrfields . .
*&---------------------------------------------------------------------*
*&  TYPE-POOL
*&---------------------------------------------------------------------*
TYPE-POOLS : SLIS.
TYPE-POOLS : KCDE  .
TYPE-POOLS : TRUXS .
TYPE-POOLS : SYDES .
TYPE-POOLS : ICON.

*&---------------------------------------------------------------------*
*&  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : c_a TYPE char01 VALUE 'A' ,
            c_b TYPE char01 VALUE 'B' ,
            c_c TYPE char01 VALUE 'C' ,
            c_d TYPE char01 VALUE 'D' ,
            c_e TYPE char01 VALUE 'E' ,
            c_f TYPE char01 VALUE 'F' ,
            c_h TYPE char01 VALUE 'H' ,
            c_i TYPE char01 VALUE 'I' ,
            c_n TYPE char01 VALUE 'N' ,
            c_p TYPE char01 VALUE 'P' ,
            c_s TYPE char01 VALUE 'S' ,
            c_x TYPE char01 VALUE 'X' .

CONSTANTS : c_01 TYPE char02 VALUE '01' ,
            c_02 TYPE char02 VALUE '02' ,
            c_03 TYPE char02 VALUE '03' ,
            c_04 TYPE char02 VALUE '04' ,
            c_05 TYPE char02 VALUE '05' ,
            c_06 TYPE char02 VALUE '06' ,
            c_07 TYPE char02 VALUE '07' ,
            c_08 TYPE char02 VALUE '08' ,
            c_09 TYPE char02 VALUE '09' ,
            c_10 TYPE char02 VALUE '10' ,
            c_99 TYPE char02 VALUE '99' .

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLES
*&---------------------------------------------------------------------*

* Job_start, Job_end.
DATA: g_job_start_date LIKE sy-datum,
      g_job_start_time TYPE syuzeit,
      g_job_end_date   LIKE sy-datum,
      g_job_end_time   TYPE syuzeit.

* Result count
DATA : g_total   LIKE sy-tabix ,
       g_success LIKE sy-tabix ,
       g_error   LIKE sy-tabix .

DATA : g_time_stamp  TYPE timestampl.

*.# Data for BDC
DATA  : GT_BDCTAB   LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
DATA  : GT_MSGTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA  : G_DISMODE   TYPE CTU_MODE.

DATA : st_functxt  TYPE smp_dyntxt ,
       g_filename  TYPE string .

DATA : g_excel .

*&---------------------------------------------------------------------*
*&  INTERNAL TABLES
*&---------------------------------------------------------------------*

DATA : BEGIN OF gt_data OCCURS 0 ,
        CELLTAB  TYPE LVC_T_STYL .
        INCLUDE STRUCTURE ZHKPMS0010 .
DATA   END OF GT_DATA .

data : begin of gt_upload occurs 0 ,
         equnr like ZHKPMS0010-equnr ,
         SWERK like zhkpms0010-SWERK ,
         eqart like zhkpms0010-eqart ,
         ANLNR like zhkpms0010-ANLNR ,
         ANLUN like zhkpms0010-ANLUN ,
         GROES like zhkpms0010-GROES ,
         INBDT(10), " like zhkpms0010-INBDT ,
         ANSWT(10), " like zhkpms0010-ANSWT ,
         WAERS like zhkpms0010-WAERS ,
         ANSDT(10), " like zhkpms0010-ANSDT ,
         HERST like zhkpms0010-HERST ,
         HERLD like zhkpms0010-HERLD ,
         TYPBZ like zhkpms0010-TYPBZ ,
         BAUJJ like zhkpms0010-BAUJJ ,
         BAUMM like zhkpms0010-BAUMM ,
       end of gt_upload .

*&---------------------------------------------------------------------*
*&  Define of ALV Variavles
*&---------------------------------------------------------------------*

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
CLASS LCL_ALV_GRID      DEFINITION DEFERRED.

DATA: G_ALV_DOC_HEAD   TYPE REF TO "cl_gui_container,
                                    CL_GUI_DOCKING_CONTAINER,

      G_GRID_HEAD      TYPE REF TO LCL_ALV_GRID, "cl_gui_alv_grid,
      G_EVENT_HEAD     TYPE REF TO LCL_EVENT_RECEIVER.

DATA: G_ALV_DOC_ITEM   TYPE REF TO   CL_GUI_CUSTOM_CONTAINER,"cl_gui_container,
                                   "CL_GUI_DOCKING_CONTAINER,

      G_GRID_ITEM      TYPE REF TO LCL_ALV_GRID, "cl_gui_alv_grid,
      G_EVENT_ITEM     TYPE REF TO LCL_EVENT_RECEIVER.


DATA: G_ALV_DOC_MDAT   TYPE REF TO "cl_gui_container,
                                    CL_GUI_DOCKING_CONTAINER,

      G_GRID_MDAT      TYPE REF TO LCL_ALV_GRID, "cl_gui_alv_grid,
      G_EVENT_MDAT     TYPE REF TO LCL_EVENT_RECEIVER.

DATA: G_ALV_DOC_WDAT   TYPE REF TO "cl_gui_container,
                                    CL_GUI_DOCKING_CONTAINER,

      G_GRID_WDAT      TYPE REF TO LCL_ALV_GRID, "cl_gui_alv_grid,
      G_EVENT_WDAT     TYPE REF TO LCL_EVENT_RECEIVER.

DATA:  G_TITLE             TYPE LVC_TITLE.

DATA: G_HTML_CONTAINER   TYPE REF TO CL_GUI_CONTAINER,
      G_DOCUMENT         TYPE REF TO CL_DD_DOCUMENT,
      G_HTML_CNTL        TYPE REF TO CL_GUI_HTML_VIEWER.

DATA: G_ROWS_T           TYPE LVC_T_ROW,  " ALV ##: ####
      G_ROWS_S           LIKE LVC_S_ROW,
      G_INDEX            LIKE SY-TABIX,
      G_TCODE            TYPE SY-TCODE.

DATA: ST_LAY           TYPE LVC_S_LAYO.

DATA: GT_FIELD         TYPE LVC_T_FCAT,           "ALV Field catagory(SCREEN)
      ST_FIELD         TYPE LVC_S_FCAT,
      GT_SORTD         TYPE LVC_T_SORT,
      ST_SORTD         TYPE LVC_S_SORT,
      GT_ROWS          TYPE LVC_T_ROW,  " ALV ##: ####
      ST_ROWS          LIKE LVC_S_ROW,
      GT_EXCLUDE       TYPE UI_FUNCTIONS,
      ST_VARIANT       LIKE DISVARIANT,     "variant
      GT_VARIANT       LIKE DISVARIANT,     "variant
      ST_VARIANT_0200  LIKE DISVARIANT,     "variant
      GT_VARIANT_0200  LIKE DISVARIANT.     "variant

DATA: G_CONTAINER TYPE SCRFNAME VALUE 'WC_CONTAINER_0100'.

FIELD-SYMBOLS: <FS_CONTAINER> TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
               <FS_CONTROL>   TYPE        SCRFNAME,
               <FS_ALV>       TYPE REF TO CL_GUI_ALV_GRID,
               <FS_ITAB>      TYPE STANDARD TABLE.


DEFINE SET_FIELDCAT_M .
  ST_FIELD-REPTEXT    = &1.
  ST_FIELD-SCRTEXT_L  = &1.
  ST_FIELD-SCRTEXT_M  = &1.
  ST_FIELD-SCRTEXT_S  = &1.
  ST_FIELD-COLDDICTXT = 'L'.
  ST_FIELD-OUTPUTLEN  = &2.
END-OF-DEFINITION.

DATA: GT_COLINFO_TABLE   TYPE SLIS_T_SPECIALCOL_ALV WITH HEADER LINE,
      ST_CELLTAB     TYPE LVC_S_STYL,
      GT_CELLTAB     TYPE LVC_T_STYL.

DATA: IT_EDIT        TYPE LVC_T_FCAT  WITH HEADER LINE.
*---------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.

  PUBLIC SECTION.
    METHODS: SET_OPTIMIZE_ALL_COLS,
    SET_CURSOR IMPORTING ROW  TYPE I
      COL  TYPE I,
      SET_FIXED_COL,
      SET_RESIZE_ROW.

ENDCLASS.                    "LCL_ALV_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_ALV_GRID IMPLEMENTATION.

  METHOD SET_OPTIMIZE_ALL_COLS.

    CALL METHOD ME->OPTIMIZE_ALL_COLS
      EXPORTING
        INCLUDE_HEADER = 1.

  ENDMETHOD.                    "SET_OPTIMIZE_ALL_COLS

  METHOD SET_CURSOR.

    CALL METHOD ME->SET_CURRENT_CELL_BASE
      EXPORTING
        ROW = ROW
        COL = COL.

  ENDMETHOD.                    "SET_CURSOR

  METHOD SET_FIXED_COL.

    CALL METHOD ME->SET_FIXED_COLS
      EXPORTING
        COLS = 11.

  ENDMETHOD.                    "SET_FIXED_COL

  METHOD SET_RESIZE_ROW.

    CALL METHOD ME->SET_RESIZE_ROWS
      EXPORTING
        ENABLE = 1.

  ENDMETHOD.                    "SET_RESIZE_ROW

ENDCLASS.                    "LCL_ALV_GRID IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
* GRID Header
     DOUBLE_CLICK          FOR EVENT DOUBLE_CLICK
                           OF CL_GUI_ALV_GRID
                           IMPORTING E_ROW
                                     E_COLUMN
                                     ES_ROW_NO,
* GRID Item
     HOTSPOT_CLICK_ITEM    FOR EVENT HOTSPOT_CLICK
                           OF CL_GUI_ALV_GRID
                           IMPORTING E_ROW_ID
                                     E_COLUMN_ID
                                     ES_ROW_NO,

      HOTSPOT_CLICK_MDAT   FOR EVENT HOTSPOT_CLICK
                           OF CL_GUI_ALV_GRID
                           IMPORTING E_ROW_ID
                                     E_COLUMN_ID
                                     ES_ROW_NO,

      HOTSPOT_CLICK_WDAT   FOR EVENT HOTSPOT_CLICK
                           OF CL_GUI_ALV_GRID
                           IMPORTING E_ROW_ID
                                     E_COLUMN_ID
                                     ES_ROW_NO,

     TOOLBAR_ITEM          FOR EVENT TOOLBAR
                           OF CL_GUI_ALV_GRID
                           IMPORTING E_OBJECT
                                     E_INTERACTIVE,

     USER_COMMAND_ITEM    FOR EVENT USER_COMMAND
                           OF CL_GUI_ALV_GRID
                           IMPORTING E_UCOMM SENDER,


     DATA_CHANGED          FOR EVENT DATA_CHANGED
                           OF CL_GUI_ALV_GRID
                           IMPORTING ER_DATA_CHANGED
                                     E_ONF4
                                     E_ONF4_BEFORE
                                     E_ONF4_AFTER
                                     E_UCOMM.



  PRIVATE SECTION.
    DATA: G_INITIAL_TABLE TYPE C.
ENDCLASS.                    "lcl_event_receiver DEFINITION

**---------------------------------------------------------
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
*
*---------------------[ GRID bottom ]---------------------------------*
*---------------------------------------------------------------------*
*       METHOD double_click                                           *
*---------------------------------------------------------------------*
  METHOD DOUBLE_CLICK.
*    PERFORM EVENT_DOUBLE_CLICK USING E_ROW
*                                     E_COLUMN.
  ENDMETHOD.                    "toolbar
*---------------------------------------------------------------------*
*       METHOD hotspot_click_Item                                     *
*---------------------------------------------------------------------*
  METHOD HOTSPOT_CLICK_ITEM.
*    PERFORM EVENT_HOTSPOT_CLICK_ITEM  USING E_ROW_ID
*                                            E_COLUMN_ID.

  ENDMETHOD.                    "hotspot_click
*---------------------------------------------------------------------*
*       METHOD hotspot_click_MDAT                                     *
*---------------------------------------------------------------------*
  METHOD HOTSPOT_CLICK_MDAT .
*    PERFORM EVENT_HOTSPOT_CLICK_MDAT  USING E_ROW_ID
*                                            E_COLUMN_ID.

  ENDMETHOD.                    "hotspot_click

*---------------------------------------------------------------------*
*       METHOD hotspot_click_MDAT                                     *
*---------------------------------------------------------------------*
  METHOD HOTSPOT_CLICK_WDAT .
*    PERFORM EVENT_HOTSPOT_CLICK_WDAT  USING E_ROW_ID
*                                            E_COLUMN_ID.

  ENDMETHOD.                    "hotspot_click

*---------------------------------------------------------------------*
*       METHOD toolbar_Item                                           *
*---------------------------------------------------------------------*
  METHOD TOOLBAR_ITEM.
*    PERFORM EVENT_TOOLBAR_ITEM  USING E_OBJECT
*                                      E_INTERACTIVE.
  ENDMETHOD.                    "toolbar
*---------------------------------------------------------------------*
*       METHOD user_command_Item                                      *
*---------------------------------------------------------------------*
  METHOD USER_COMMAND_ITEM.
*    PERFORM EVENT_UCOMM_ITEM   USING E_UCOMM.
  ENDMETHOD.                    "user_command

*---------------------------------------------------------------------*
*       METHOD data_changed                                           *
*---------------------------------------------------------------------*
  METHOD DATA_CHANGED.

*    PERFORM EVENT_DATA_CHANGED       USING ER_DATA_CHANGED
*                                           E_ONF4
*                                           E_ONF4_BEFORE
*                                           E_ONF4_AFTER
*                                           E_UCOMM.

  ENDMETHOD.                    "data_changed

*---------------------------------------------------------------------*

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*& # Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : s_werks for t001w-werks ,
                 s_beber for iloa-beber .

SELECT-OPTIONS : s_EQUNR FOR EQUI-EQUNR ,
                 s_EQKTX FOR EQKT-EQKTX ,
                 s_EQTYP FOR EQUI-EQTYP .

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002 .
SELECTION-SCREEN COMMENT 1(70) text-p01  .
SELECTION-SCREEN skip .
SELECTION-SCREEN COMMENT 1(70) text-p02  .
SELECTION-SCREEN skip .
SELECTION-SCREEN COMMENT 1(70) text-p03  .
SELECTION-SCREEN END OF BLOCK b2.
