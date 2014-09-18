************************************************************************
* Program Name      : ZMMR_OVERSTOCK_DISPLAY
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
REPORT zmmr_overstock_display MESSAGE-ID zmmm.

TYPE-POOLS: slis .
TABLES: mara, ztmm_overstock.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF it_overstock OCCURS 0.
        INCLUDE STRUCTURE ztmm_overstock.
DATA: ven_name LIKE lfa1-name1,
      END OF it_overstock.

** ALV
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant,      "for parameter IS_VARIANT
      it_exclude TYPE ui_functions.

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: wa_custom_control_800 TYPE scrfname VALUE 'ALV_CONTAINER_800',
      alv_grid_800          TYPE REF TO cl_gui_alv_grid,
      grid_container_800    TYPE REF TO cl_gui_custom_container.

DATA: g_lights_fieldname  TYPE slis_fieldname VALUE 'LIGHTS'.

DATA: ok_code LIKE sy-ucomm,
      w_code LIKE sy-ucomm,
      w_old_code LIKE sy-ucomm,
      w_cnt   TYPE   i,
      w_base_date LIKE sy-datum,
      w_base_time LIKE sy-uzeit,
      w_repid LIKE sy-repid,
      w_run_date(8),
      w_run_time(6),
      w_dynnr LIKE sy-dynnr.

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_gui_timer DEFINITION INHERITING FROM cl_gui_control.

  PUBLIC SECTION.

    CONSTANTS:  eventid_finished TYPE i VALUE 1 .

    CLASS-DATA: interval TYPE i VALUE '0'.

    EVENTS:     finished .

    METHODS:
*             show_alv,
             cancel
                  EXCEPTIONS
                     error,
             constructor
                 IMPORTING
                     lifetime TYPE i OPTIONAL
                     value(shellstyle) TYPE i OPTIONAL
                     value(parent) TYPE REF TO cl_gui_container OPTIONAL
                 EXCEPTIONS
                     error,
             run
                 EXCEPTIONS
                     error,
             dispatch REDEFINITION.


ENDCLASS.                    "lcl_gui_timer DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
                on_finished
                       FOR EVENT finished OF lcl_gui_timer.

ENDCLASS.                    "lcl_event_handler DEFINITION


DATA: timer_container TYPE REF TO cl_gui_custom_container,
      gui_timer TYPE REF TO lcl_gui_timer,
      event_handler TYPE REF TO lcl_event_handler,
      timeout_interval TYPE i.
*      L_ALV TYPE REF TO CL_GUI_ALV_GRID,
*      FIRST_CALL(1) TYPE C,
**      ok_code     TYPE syucomm,
*      L_IS_STABLE TYPE LVC_S_STBL.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_finished.

* Start Timer again

    gui_timer->interval = timeout_interval.
    CALL METHOD gui_timer->run.

* cause PAI
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = 'REFR'.

  ENDMETHOD.                    "on_finished

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_gui_timer IMPLEMENTATION.

  METHOD constructor.

    TYPE-POOLS: sfes.

    DATA clsid(80).
    DATA event_tab TYPE cntl_simple_events.
    DATA event_tab_line TYPE cntl_simple_event.

    IF clsid IS INITIAL.
      DATA: return,
            guitype TYPE i.

      guitype = 0.
      CALL FUNCTION 'GUI_HAS_OBJECTS'
        EXPORTING
          object_model = sfes_obj_activex
        IMPORTING
          return       = return
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc NE 0.
        RAISE error.
      ENDIF.

      IF return = 'X'.
        guitype = 1.
      ENDIF.
      IF guitype = 0.
        CALL FUNCTION 'GUI_HAS_OBJECTS'
          EXPORTING
            object_model = sfes_obj_javabeans
          IMPORTING
            return       = return
          EXCEPTIONS
            OTHERS       = 1.
        IF sy-subrc NE 0.
          RAISE error.
        ENDIF.

        IF return = 'X'.
          guitype = 2.
        ENDIF.
      ENDIF.

      CASE guitype.
        WHEN 1.
          clsid = 'Sapgui.InfoCtrl.1'.
        WHEN 2.
          clsid = 'com.sap.components.controls.sapImage.SapImage'.
      ENDCASE.
    ENDIF.

    CALL METHOD super->constructor
      EXPORTING
        clsid      = clsid
        shellstyle = 0
        parent     = cl_gui_container=>default_screen
        autoalign  = space
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.

    CALL METHOD cl_gui_cfw=>subscribe
      EXPORTING
        shellid = h_control-shellid
        ref     = me
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.

* Register the events
    event_tab_line-eventid = lcl_gui_timer=>eventid_finished.
    APPEND event_tab_line TO event_tab.

    CALL METHOD set_registered_events
      EXPORTING
        events = event_tab.

  ENDMETHOD.                    "constructor

  METHOD cancel.

    CALL METHOD call_method
      EXPORTING
        method     = 'SetTimer'
        p_count    = 1
        p1         = -1
        queue_only = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.


  ENDMETHOD.                    "cancel

  METHOD run.

    CALL METHOD call_method
      EXPORTING
        method     = 'SetTimer'
        p_count    = 1
        p1         = interval
        queue_only = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.


  ENDMETHOD.                    "run

  METHOD dispatch .

    CASE eventid.
      WHEN eventid_finished.
        RAISE EVENT finished.
    ENDCASE.

    CLEAR timer_container.

  ENDMETHOD.                    "dispatch

ENDCLASS.                    "lcl_gui_timer IMPLEMENTATION




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS:   P_SAVE(1) DEFAULT 'X',
* P_BOM AS CHECKBOX DEFAULT 'X',
* P_TIME TYPE I DEFAULT 40.
*
SELECT-OPTIONS: s_matrn FOR mara-matnr,
                s_werks FOR ztmm_overstock-werks.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK block6 WITH FRAME TITLE text-006.
PARAMETERS :
    p_intrv TYPE i DEFAULT '10' MODIF ID req.
*    p_clock TYPE rlmon-clock.
SELECTION-SCREEN END OF BLOCK block6.


*----------------------------------------------------------------------
INITIALIZATION.
*  PERFORM INIT_DATA.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM get_data.
  CALL SCREEN 0800.

*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  CLEAR: w_repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.
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
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.
* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog' p_gubun p_field.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname .
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sortcat_display.

  it_sort-spos           = 1.
  it_sort-fieldname      = 'STATUS'.
  it_sort-up             = 'X'.
  it_sort-subtot         = 'X'.
  APPEND it_sort.

ENDFORM.                    " build_sortcat_display

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_tb_functions.
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
MODULE display_alv_0800 OUTPUT.
  IF grid_container_800 IS INITIAL.
    PERFORM create_container_object_800.
    PERFORM set_attributes_alv_grid_800.
*    PERFORM BUILD_SORTCAT_DISPLAY.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM build_field_catalog_800 USING 'IT_OVERSTOCK'.
    PERFORM assign_itab_to_alv_800.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid_800->refresh_table_display.
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
FORM create_container_object_800.
  CLEAR: w_repid.
  CREATE OBJECT grid_container_800
    EXPORTING
      container_name              = wa_custom_control_800
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid_800
    EXPORTING
      i_parent      = grid_container_800
      i_appl_events = 'X'.

ENDFORM.                    " CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_800.
  DATA: l_date_c(10),
          l_time(8).

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-CTAB_FNAME  = 'COLOR'. "04.02.2014 Victor
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.


  CONCATENATE w_run_date+4(2) '/' w_run_date+6(2) '/' w_run_date+0(4)
                                                       INTO l_date_c.
  CONCATENATE w_run_time+0(2) ':' w_run_time+2(2) ':' w_run_time+4(2)
                                                       INTO l_time.

  CONCATENATE 'As of' l_date_c l_time INTO wa_is_layout-grid_title
    SEPARATED BY space.

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

  wa_is_layout-zebra             = 'X'.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3194   text
*----------------------------------------------------------------------*
FORM build_field_catalog_800 USING p_itab.
  DATA: lw_itab TYPE slis_tabname.
*        lw_waers LIKE t001-waers,
  DATA: l_cn(2) TYPE n,
  l_rp(30),
  l_hr(10).

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

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

                                'S' 'MSTAE'    ' ',
                                  ' ' 'COLTEXT'     'Status',
                                  'E' 'OUTPUTLEN'   '2',

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

*                               CH.Jeong on 10/22/2013
                                 'S' 'BDMNG'    ' ',
                                 ' ' 'COLTEXT'     'Requirement',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '9',
*                               End on 10/22/2013

                                 'S' 'EFFECTIVE_OUT'    ' ',
                                  ' ' 'COLTEXT'     'Effective Out',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'ASSY'    ' ',
                                  ' ' 'COLTEXT'     'Assy',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'MODEL'    ' ',
                                 ' ' 'COLTEXT'     'Model',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'ASSY_DESC'    ' ',
                                  ' ' 'COLTEXT'     'Assy Description',
                                  'E' 'OUTPUTLEN'   '15',

                                 'S' 'LGR_DATE'    ' ',
                                 ' ' 'COLTEXT'     'GR Date',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'LBF_DATE'    ' ',
                                  ' ' 'COLTEXT'     'B/F Date',
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
FORM assign_itab_to_alv_800.
  CALL METHOD alv_grid_800->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
      i_default            = space
*      it_toolbar_excluding = it_exclude[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_overstock[]
      it_sort              = it_sort[].

** ENTER
  CALL METHOD alv_grid_800->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  CALL METHOD alv_grid_800->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = alv_grid_800.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
  w_code = ok_code.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REFR'.
      SUBMIT zmmr_overstock_display
      WITH s_matrn  IN s_matrn
      WITH s_werks IN s_werks
       WITH p_intrv = p_intrv.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_cell_color
*&---------------------------------------------------------------------*
*       Set Cell Color
*----------------------------------------------------------------------*
FORM set_cell_color  USING    u_col
                              u_int
                              u_field
                     CHANGING color_tab
                              TYPE slis_t_specialcol_alv.
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
  DATA : l_color TYPE slis_specialcol_alv.
  l_color-fieldname = u_field.
  l_color-color-col = u_col.
  l_color-color-int = u_int.
  APPEND l_color TO color_tab.
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
FORM save_to_table.

  DELETE FROM ztmm_overstock CLIENT SPECIFIED WHERE mandt = sy-mandt.

*  IF SY-SUBRC = 0.
  INSERT ztmm_overstock FROM TABLE it_overstock.
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
FORM get_data.
  DATA: l_index LIKE sy-tabix,
        l_date LIKE sy-datum,
        l_date_c(8).

  l_date = l_date_c = '99991230'.

  timeout_interval = p_intrv * 60.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_overstock
    FROM ztmm_overstock
    WHERE matnr IN s_matrn
      AND werks IN s_werks.

  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH 'No Data'.
  ENDIF.

  LOOP AT it_overstock.
    l_index = sy-tabix.
    SELECT SINGLE maktx dsnam INTO (it_overstock-maktx,
         it_overstock-dsnam)
      FROM marc AS a
      INNER JOIN makt AS c
      ON a~matnr = c~matnr
      INNER JOIN t024d AS d
      ON a~dispo = d~dispo
      WHERE a~matnr = it_overstock-matnr
      AND d~werks = it_overstock-werks
      AND spras = 'EN'.

    SELECT SINGLE name1 INTO it_overstock-ven_name
      FROM lfa1
      WHERE lifnr = it_overstock-lifnr.

    IF it_overstock-effective_out >= l_date.
      CLEAR: it_overstock-effective_out.
    ENDIF.
    MODIFY it_overstock INDEX l_index.
  ENDLOOP.
  READ TABLE it_overstock INDEX 1.
  w_run_date = it_overstock-zsdat.
  w_run_time = it_overstock-zstim.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
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
MODULE set_timer OUTPUT.
  IF timer_container IS INITIAL.
    CREATE OBJECT:

       timer_container
             EXPORTING
                  container_name = 'TI_CONTAINER',
       gui_timer
             EXPORTING
                  parent = timer_container.

    SET HANDLER event_handler->on_finished FOR gui_timer.

    gui_timer->interval = timeout_interval.
    CALL METHOD gui_timer->run.
  ENDIF.

ENDMODULE.                 " SET_TIMER  OUTPUT
