************************************************************************
* Program Name      : ZMMR_OBSOLET_STOCK_DISPLAY
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
REPORT ZMMR_OBSOLET_STOCK_DISPLAY MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TABLES: MARA, ZTMM_OBS_STOCK.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF IT_OBS_STOCK OCCURS 0.
        INCLUDE STRUCTURE ZTMM_OBS_STOCK.
DATA: VEN_NAME LIKE LFA1-NAME1,
      END OF IT_OBS_STOCK.

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
       W_RUN_DATE(8),
      W_RUN_TIME(6),
      W_DYNNR LIKE SY-DYNNR.

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER DEFINITION INHERITING FROM CL_GUI_CONTROL.

  PUBLIC SECTION.

    CONSTANTS:  EVENTID_FINISHED TYPE I VALUE 1 .

    CLASS-DATA: INTERVAL TYPE I VALUE '0'.

    EVENTS:     FINISHED .

    METHODS:
*             show_alv,
             CANCEL
                  EXCEPTIONS
                     ERROR,
             CONSTRUCTOR
                 IMPORTING
                     LIFETIME TYPE I OPTIONAL
                     VALUE(SHELLSTYLE) TYPE I OPTIONAL
                     VALUE(PARENT) TYPE REF TO CL_GUI_CONTAINER OPTIONAL
                 EXCEPTIONS
                     ERROR,
             RUN
                 EXCEPTIONS
                     ERROR,
             DISPATCH REDEFINITION.


ENDCLASS.                    "lcl_gui_timer DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
                ON_FINISHED
                       FOR EVENT FINISHED OF LCL_GUI_TIMER.

ENDCLASS.                    "lcl_event_handler DEFINITION


DATA: TIMER_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GUI_TIMER TYPE REF TO LCL_GUI_TIMER,
      EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER,
      TIMEOUT_INTERVAL TYPE I.
*      L_ALV TYPE REF TO CL_GUI_ALV_GRID,
*      FIRST_CALL(1) TYPE C,
**      ok_code     TYPE syucomm,
*      L_IS_STABLE TYPE LVC_S_STBL.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_FINISHED.

* Start Timer again

    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.

* cause PAI
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = 'REFR'.

  ENDMETHOD.                    "on_finished

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    TYPE-POOLS: SFES.

    DATA CLSID(80).
    DATA EVENT_TAB TYPE CNTL_SIMPLE_EVENTS.
    DATA EVENT_TAB_LINE TYPE CNTL_SIMPLE_EVENT.

    IF CLSID IS INITIAL.
      DATA: RETURN,
            GUITYPE TYPE I.

      GUITYPE = 0.
      CALL FUNCTION 'GUI_HAS_OBJECTS'
           EXPORTING
                OBJECT_MODEL = SFES_OBJ_ACTIVEX
           IMPORTING
                RETURN       = RETURN
           EXCEPTIONS
                OTHERS       = 1.
      IF SY-SUBRC NE 0.
        RAISE ERROR.
      ENDIF.

      IF RETURN = 'X'.
        GUITYPE = 1.
      ENDIF.
      IF GUITYPE = 0.
        CALL FUNCTION 'GUI_HAS_OBJECTS'
             EXPORTING
                  OBJECT_MODEL = SFES_OBJ_JAVABEANS
             IMPORTING
                  RETURN       = RETURN
             EXCEPTIONS
                  OTHERS       = 1.
        IF SY-SUBRC NE 0.
          RAISE ERROR.
        ENDIF.

        IF RETURN = 'X'.
          GUITYPE = 2.
        ENDIF.
      ENDIF.

      CASE GUITYPE.
        WHEN 1.
          CLSID = 'Sapgui.InfoCtrl.1'.
        WHEN 2.
          CLSID = 'com.sap.components.controls.sapImage.SapImage'.
      ENDCASE.
    ENDIF.

    CALL METHOD SUPER->CONSTRUCTOR
      EXPORTING
        CLSID      = CLSID
        SHELLSTYLE = 0
        PARENT     = CL_GUI_CONTAINER=>DEFAULT_SCREEN
        AUTOALIGN  = SPACE
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

    CALL METHOD CL_GUI_CFW=>SUBSCRIBE
      EXPORTING
        SHELLID = H_CONTROL-SHELLID
        REF     = ME
      EXCEPTIONS
        OTHERS  = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

* Register the events
    EVENT_TAB_LINE-EVENTID = LCL_GUI_TIMER=>EVENTID_FINISHED.
    APPEND EVENT_TAB_LINE TO EVENT_TAB.

    CALL METHOD SET_REGISTERED_EVENTS
      EXPORTING
        EVENTS = EVENT_TAB.

  ENDMETHOD.                    "constructor

  METHOD CANCEL.

    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = -1
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.


  ENDMETHOD.                    "cancel

  METHOD RUN.

    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = INTERVAL
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

  ENDMETHOD.                    "run

  METHOD DISPATCH .

    CASE EVENTID.
      WHEN EVENTID_FINISHED.
        RAISE EVENT FINISHED.
    ENDCASE.

    CLEAR TIMER_CONTAINER.

  ENDMETHOD.                    "dispatch

ENDCLASS.                    "lcl_gui_timer IMPLEMENTATION


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATRN FOR MARA-MATNR,
                S_WERKS FOR ZTMM_OBS_STOCK-WERKS.
SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK BLOCK6 WITH FRAME TITLE TEXT-006.
PARAMETERS :
    P_INTRV TYPE I DEFAULT '10' MODIF ID REQ.
*    p_clock TYPE rlmon-clock.
SELECTION-SCREEN END OF BLOCK BLOCK6.

*----------------------------------------------------------------------
INITIALIZATION.
*  PERFORM INIT_DATA.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM GET_DATA.
  CALL SCREEN 0800.

*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
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
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object
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
*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'STATUS'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

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
    PERFORM BUILD_FIELD_CATALOG_800 USING 'IT_OBS_STOCK'.
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
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL_800
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
  CREATE OBJECT ALV_GRID_800
         EXPORTING I_PARENT = GRID_CONTAINER_800
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
          L_TIME(8).

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-CTAB_FNAME  = 'COLOR'.
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.

  CONCATENATE W_RUN_DATE+4(2) '/' W_RUN_DATE+6(2) '/' W_RUN_DATE+0(4)
                                                        INTO L_DATE_C.
  CONCATENATE W_RUN_TIME+0(2) ':' W_RUN_TIME+2(2) ':' W_RUN_TIME+4(2)
                                                       INTO L_TIME.
  CONCATENATE 'As of' L_DATE_C L_TIME INTO WA_IS_LAYOUT-GRID_TITLE
    SEPARATED BY SPACE.

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
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'WERKS'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '4',

                                 'S' 'MTART'    ' ',
                                  ' ' 'COLTEXT'     'Matl Type',
                                  'E' 'OUTPUTLEN'   '4',


                                  'S' 'OBS_DATE'    ' ',
                                  ' ' 'COLTEXT'     'OB Date',
                                  'E' 'OUTPUTLEN'   '8',

*                                'S' 'MSTAE'    ' ',
*                                  ' ' 'COLTEXT'     'Status',
*                                  'E' 'OUTPUTLEN'   '2',

                                 'S' 'BKLAS'    ' ',
                                 ' ' 'COLTEXT'     'Valuation Class',
                                  'E' 'OUTPUTLEN'   '4',

                                 'S' 'DSNAM'    ' ',
                                  ' ' 'COLTEXT'     'MRP Controller',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'MATKL'    ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '4',

                                'S' 'VEN_NAME'     ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'LBKUM'    ' ',
                                 ' ' 'COLTEXT'     'Stock',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '9',

                                 'S' 'SALK3'    ' ',
                                 ' ' 'COLTEXT'     'Value',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '11',

                                 'S' 'LGR_DATE'    ' ',
                                 ' ' 'COLTEXT'     'GR Date',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'LBF_DATE'    ' ',
                                  ' ' 'COLTEXT'     'B/F Date',
                                  'E' 'OUTPUTLEN'   '15'.
                                 .

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

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = IT_EXCLUDE[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_OBS_STOCK[]
               IT_SORT          = IT_SORT[].

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
                        EXPORTING CONTROL = ALV_GRID_800.

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
    WHEN 'REFR'.
      SUBMIT ZMMR_OBSOLET_STOCK_DISPLAY
      WITH S_MATRN  IN S_MATRN
      WITH S_WERKS IN S_WERKS
       WITH P_INTRV = P_INTRV.

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

  DELETE FROM ZTMM_OBS_STOCK CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

*  IF SY-SUBRC = 0.
  INSERT ZTMM_OBS_STOCK FROM TABLE IT_OBS_STOCK.
*  ELSE.
*    MESSAGE E000(ZZ) WITH 'Error: Z-Table deletion (ZTMM_HOUR_SHORT)'.
*  ENDIF.
ENDFORM.                    " save_to_table
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_INDEX LIKE SY-TABIX.

  TIMEOUT_INTERVAL = P_INTRV * 60.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_OBS_STOCK
    FROM ZTMM_OBS_STOCK
    WHERE MATNR IN S_MATRN
      AND WERKS IN S_WERKS
      AND LBKUM > 0.

  IF SY-SUBRC <> 0.
    MESSAGE E000(ZZ) WITH 'No Data'.
  ENDIF.

  LOOP AT IT_OBS_STOCK.
    L_INDEX = SY-TABIX.
    SELECT SINGLE MAKTX DSNAM INTO (IT_OBS_STOCK-MAKTX,
         IT_OBS_STOCK-DSNAM)
      FROM MARC AS A
      INNER JOIN MAKT AS C
      ON A~MATNR = C~MATNR
      INNER JOIN T024D AS D
      ON A~DISPO = D~DISPO
      WHERE A~MATNR = IT_OBS_STOCK-MATNR
      AND D~WERKS = IT_OBS_STOCK-WERKS
      AND SPRAS = 'EN'.

    SELECT SINGLE NAME1 INTO IT_OBS_STOCK-VEN_NAME
      FROM LFA1
      WHERE LIFNR = IT_OBS_STOCK-LIFNR.

    MODIFY IT_OBS_STOCK INDEX L_INDEX.
  ENDLOOP.
  READ TABLE IT_OBS_STOCK INDEX 1.
  W_RUN_DATE = IT_OBS_STOCK-ZSDAT.
  W_RUN_TIME = IT_OBS_STOCK-ZSTIM.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0800 OUTPUT.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'ST800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TIMER OUTPUT.
  IF TIMER_CONTAINER IS INITIAL.
    CREATE OBJECT:

       TIMER_CONTAINER
             EXPORTING
                  CONTAINER_NAME = 'TI_CONTAINER',
       GUI_TIMER
             EXPORTING
                  PARENT = TIMER_CONTAINER.

    SET HANDLER EVENT_HANDLER->ON_FINISHED FOR GUI_TIMER.

    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.
  ENDIF.

ENDMODULE.                 " SET_TIMER  OUTPUT
