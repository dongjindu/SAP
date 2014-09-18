************************************************************************
* Program Name      : ZIPP_SCRAP_CAR
* Author            : Furong Wang
* Creation Date     : 02/26/2007
* Specifications By :
* Pattern           :
* Development Request No : UD1K930870
* Addl Documentation:
* Description       : ALC interface for scrap cars
*

* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zipp_scrap_car  MESSAGE-ID zmpp.

DATA: it_data     LIKE TABLE OF ztpp_scrap_car WITH HEADER LINE,
      it_data_sel LIKE TABLE OF ztpp_scrap_car WITH HEADER LINE.

CONSTANTS: c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination

* ALV
DATA:   ok_code LIKE sy-ucomm,
        save_ok LIKE sy-ucomm.

DATA:   wc_control        TYPE        scrfname VALUE 'CC_ALV',
        wc_alv            TYPE REF TO cl_gui_alv_grid,
        wc_container      TYPE REF TO cl_gui_custom_container.

* CLASS DECLARATION
CLASS   lcl_event_receiver DEFINITION DEFERRED. "ALV EVENT HANDLE

DATA :  event_receiver TYPE REF TO lcl_event_receiver.

* INTERNAL TABLES FOR ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat ,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort.

* VARIABLES FOR ALV GRID
DATA : ws_layout TYPE lvc_s_layo,
       w_variant   TYPE disvariant,          "for parameter IS_VARIANT
       w_fieldname LIKE LINE OF it_fieldcat,
       w_repid     LIKE sy-repid,
       w_cnt       TYPE i,                   "Field count
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: w_container(100),
      w_control(100),
      w_alv(100),
      w_itab(100),
      w_structure LIKE dd02l-tabname.

****************************************************************
* LOCAL CLASSES: EVEN HANDLING
****************************************************************
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    METHODS:
    handle_data_changed
       FOR EVENT data_changed OF cl_gui_alv_grid
            IMPORTING er_data_changed.

    METHODS:
     handle_toolbar
         FOR EVENT toolbar OF cl_gui_alv_grid
             IMPORTING e_object e_interactive,

     handle_user_command
         FOR EVENT user_command OF cl_gui_alv_grid
             IMPORTING e_ucomm,

     menu_button
          FOR EVENT menu_button OF cl_gui_alv_grid
             IMPORTING e_ucomm.

    DATA: error_in_data TYPE c.

    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no.
ENDCLASS.

****************************************************************
* LOCAL CLASSES:IMPLEMENTATION
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          flag TYPE c,
          lvc_t_row TYPE lvc_t_row.

    DATA: ls_toolbar  TYPE stb_button.



    error_in_data = space.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      IF ls_good-fieldname = 'BETRG'.
        CALL METHOD er_data_changed->get_cell_value
         EXPORTING
            i_row_id  = ls_good-row_id
            i_fieldname = ls_good-fieldname
         IMPORTING
            e_value =   lv_value.
        CALL METHOD er_data_changed->modify_cell
                EXPORTING
                     i_row_id = ls_good-row_id
                     i_fieldname = ls_good-fieldname
                     i_value     = lv_value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM dbl_click USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click
  METHOD  handle_toolbar.
    DATA: ls_toolbar  TYPE stb_button.
    LOOP AT e_object->mt_toolbar INTO ls_toolbar.

    ENDLOOP.

  ENDMETHOD.
  METHOD  handle_user_command.
    CASE  e_ucomm.
      WHEN OTHERS.
        WRITE :/ 'I am here'.
    ENDCASE.
  ENDMETHOD.
  METHOD  menu_button.
    CASE  e_ucomm.
      WHEN OTHERS.
        WRITE :/ 'I am here'.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*end include

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_datum FOR sy-datum.

SELECTION-SCREEN END OF BLOCK b1.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM get_date_range.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.
  CALL SCREEN '100'.


*&****ALV FORMS*********************************************************

*&---------------------------------------------------------------------*
*&      Form  DBL_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click USING    p_e_column_fieldname
                        p_es_row_no_row_id.

ENDFORM.                    " DBL_CLICK
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv OUTPUT.

  IF wc_container IS INITIAL.
    PERFORM create_container .
    PERFORM set_alv_layout .
    PERFORM build_field_catalog .
    PERFORM set_sort_total .
    PERFORM start_alv_display.
    PERFORM sssign_event.
  ELSE.
    PERFORM set_alv_layout .
    PERFORM build_field_catalog .
    PERFORM set_sort_total .
    PERFORM refresh_display.
  ENDIF.

ENDMODULE.                 " SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container.
  CREATE OBJECT wc_container
         EXPORTING container_name = 'CC_ALV'
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT wc_alv
         EXPORTING i_parent      = wc_container
                   i_appl_events = 'X'.

ENDFORM.                    " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_alv_layout.

  CLEAR : ws_layout, w_variant.
*  WS_LAYOUT-ZEBRA           = 'X'.
  ws_layout-edit            = ' '.
  ws_layout-sel_mode        = 'A'.
  ws_layout-language        = sy-langu.
*  ws_layout-cwidth_opt      = 'X'.
  ws_layout-no_merging      = 'X'.
  ws_layout-no_keyfix       = 'X'.
*  ws_layout-ctab_fname      = 'CLRTB'.
  w_variant-report            = sy-repid.
  w_variant-username          = sy-uname.
* BUILD THE CELL COLOR
*  perform build_cell_color.

ENDFORM.                    " SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_color.
*  data: ct type lvc_t_scol.
*  data: w_ct  like line of ct.
*  data: wa_dis type s_dis.
*
*  loop at it_dis into wa_dis.
*    w_ct-fname = 'TCOST'.
*    w_ct-color-col = '5'.
*    w_ct-color-int = '1'.
*    append w_ct to ct.
*    w_ct-fname = 'TOTCO'.
*    w_ct-color-col = '5'.
*    append w_ct to ct.
*    w_ct-fname = 'TOTBE'.
*    w_ct-color-col = '5'.
*    append w_ct to ct.
*    w_ct-fname = 'THOUR'.
*    w_ct-color-col = '5'.
*    append w_ct to ct.
*
*    wa_dis-clrtb = ct.
*    modify it_dis from wa_dis.
*  endloop.

ENDFORM.                    " BUILD_CELL_COLOR

*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog.
  DATA: lw_itab TYPE slis_tabname.
  DATA: wa_fc   LIKE LINE OF it_fieldcat.
  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO w_repid.
  lw_itab = 'ZTPP_SCRAP_CAR'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE '
       EXPORTING
            i_structure_name   = lw_itab
            i_bypassing_buffer = 'X'
       CHANGING
            ct_fieldcat        = it_fieldcat.
* SET THE FIELD ATTRIBUTE
  LOOP AT it_fieldcat INTO wa_fc.
    IF wa_fc-fieldname = ' '.
      wa_fc-edit = 'X'.
      MODIFY it_fieldcat FROM wa_fc.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SORT  text
*----------------------------------------------------------------------*
FORM set_sort_total .

  PERFORM fill_sort_filed USING:  '1' 'KOSTL' 'X' ' ' 'X',
                                  '2' 'FROCC' 'X' ' ' ' ',
                                  '3' 'TOCC'  'X' ' ' ' ',
                                  '4' 'GTEXT' 'X' ' ' ' ',
                                  '5' 'KTEXT' ' ' 'X' 'X',
                                  '6' 'KZTXT' 'X' ' ' ' '.
ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM start_alv_display.
  DATA: lw_dynnr   LIKE   sy-dynnr.

  CALL METHOD wc_alv->set_table_for_first_display
   EXPORTING
             is_layout        = ws_layout
             i_save           = w_save
             is_variant       = w_variant
             i_default        = space
   CHANGING  it_fieldcatalog  = it_fieldcat
             it_sort          = it_sort
             it_outtab        = it_data[].

* Enter---
  CALL METHOD wc_alv->register_edit_event
                EXPORTING
                   i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  CALL METHOD wc_alv->register_edit_event
                EXPORTING
                   i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT event_receiver.
  SET HANDLER event_receiver->handle_data_changed FOR wc_alv.
  SET HANDLER event_receiver->handle_double_click FOR wc_alv.


ENDFORM.                    " START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SSSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event.

ENDFORM.                    " SSSIGN_EVENT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZD1'.
  SET TITLEBAR 'ZD1'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SEND'.
      PERFORM get_selected_rows.
      PERFORM send_to_alc.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FILL_SORT_FILED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_sort_filed USING p_spos
                           p_field
                           p_up
                           p_down
                           p_total.
  DATA: wa_sort LIKE LINE OF it_sort.

  wa_sort-spos = p_spos.
  wa_sort-fieldname  = p_field.
  wa_sort-up         = p_up.
  wa_sort-down       = p_down.
  wa_sort-subtot     = p_total.
  APPEND wa_sort TO it_sort.
ENDFORM.                    " FILL_SORT_FILED

*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_display.

  CALL METHOD wc_alv->refresh_table_display.
ENDFORM.                    " REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  set_ALVobject  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alvobject OUTPUT.
  IF wc_container IS INITIAL.
    PERFORM create_container .
    PERFORM set_alv_layout .
    PERFORM build_field_catalog .
*    perform set_sort_total .
    PERFORM start_alv_display.
    PERFORM sssign_event.
  ELSE.
    PERFORM set_alv_layout .
    PERFORM build_field_catalog .
    PERFORM set_sort_total .
    PERFORM refresh_display.
  ENDIF.
ENDMODULE.                 " set_ALVobject  OUTPUT
*---------------------------------------------------------------------*
*       MODULE exit INPUT                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE exit INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' .
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_rows.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
           lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CLEAR it_data_sel.
  CALL METHOD wc_alv->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ELSE.
    CLEAR: it_data_sel[].
    LOOP AT lt_rows WHERE index NE 0.
      READ TABLE it_data INDEX lt_rows-index.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING it_data TO it_data_sel.
        APPEND it_data_sel.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  get_date_range
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date_range.
  s_datum-sign = 'I'.
  s_datum-option = 'EQ'.
  s_datum-low = sy-datum - 7.
  s_datum-high = sy-datum.
  APPEND s_datum.
ENDFORM.                    " get_date_range
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_objek(18). " LIKE ausp-objek.
  DATA: l_vals LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
*            percentage = p_percentage
            text       = 'Reading data ...'
       EXCEPTIONS
            OTHERS     = 1.

  SELECT * INTO TABLE it_data
    FROM ztpp_scrap_car
    WHERE erdat IN s_datum.

  IF sy-subrc = 0.
    l_vals-atnam = 'P_USAGE_CAR'.
    APPEND l_vals.
    l_vals-atnam = 'P_SCRAP_DATE'.
    APPEND l_vals.
    LOOP AT it_data.
      CONCATENATE it_data-model it_data-body_ser INTO l_objek.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = l_objek
                ctype        = '002'
           TABLES
                val_table    = l_vals
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                error_value  = 4
                OTHERS       = 5.

      IF sy-subrc = 0.
        READ TABLE l_vals INDEX 1.
        IF l_vals-atwrt = 'S' OR l_vals-atwrt = 'D'.
          it_data-u_car = l_vals-atwrt.
          READ TABLE l_vals INDEX 2.
          IF l_vals-atwrt IS INITIAL.
            DELETE it_data.
          ELSE.
            MODIFY it_data.
          ENDIF.
        ELSE.
          DELETE it_data.
        ENDIF.
      ELSE.
        DELETE it_data.
      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE i000 WITH text-m01.
    EXIT.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  send_to_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_to_alc.
  DATA : l_msgtxt(100),
         l_result(1).

  DATA: lt_ztppvp LIKE TABLE OF ztppvp WITH HEADER LINE,
        lt_send LIKE TABLE OF zspp_scrap_car_i WITH HEADER LINE.

  CLEAR: lt_send[].
  LOOP AT it_data_sel.
    lt_send-zk99flg = 'DL'.
    lt_send-zk99car = it_data_sel-model.
    lt_send-zk99bno = it_data_sel-body_ser.
    lt_send-zk99pky = it_data_sel-worder+0(9).
    lt_send-zk99nat = it_data_sel-worder+9(3).
    lt_send-zk99del = it_data_sel-worder+12(2).
    lt_send-zk99icl = it_data_sel-intc.
    lt_send-zk99ocl = it_data_sel-extc.
    lt_send-zk99vin = it_data_sel-vin.
    APPEND lt_send.
  ENDLOOP.

  CALL FUNCTION 'Z_FPP_SCRAP_CAR'
    DESTINATION c_dest
    IMPORTING
      flag          = l_result
    TABLES
      i_scrap       = lt_send
    EXCEPTIONS
           communication_failure = 1 MESSAGE l_msgtxt
           system_failure        = 2 MESSAGE l_msgtxt.

  IF l_result = 'S'.
    LOOP AT it_data_sel.
      lt_ztppvp-flg = 'DL'.
      lt_ztppvp-modl = it_data_sel-model.
      lt_ztppvp-vhno = it_data_sel-body_ser.
      lt_ztppvp-ordr = it_data_sel-worder.
      lt_ztppvp-vinn = it_data_sel-vin.
      lt_ztppvp-intc = it_data_sel-intc.
      lt_ztppvp-extc = it_data_sel-extc.
      lt_ztppvp-zsdat = it_data_sel-erdat.
      lt_ztppvp-zstim = it_data_sel-erzet.
      lt_ztppvp-zedat = sy-datum.
      lt_ztppvp-zetim = sy-uzeit.
      lt_ztppvp-zresult = 'S'.
      APPEND lt_ztppvp.
    ENDLOOP.
    MODIFY ztppvp FROM TABLE lt_ztppvp.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    MESSAGE i001 WITH 'Successfully sent out'.
  ELSE.
    MESSAGE i001 WITH l_msgtxt.
  ENDIF.
ENDFORM.                    " send_to_alc
