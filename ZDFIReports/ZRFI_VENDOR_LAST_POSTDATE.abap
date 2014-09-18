***********************************************************************
* Program Name      : ZRFI_VENDOR_LAST_POSTDATE
* Author            : Furong Wang
* Creation Date     : 04/2014
* Specifications By : YoungKi Hong
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : FI: Vendor AP display
*                     last posting date by Vend and Doc Type
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
***********************************************************************

REPORT zrfi_vendor_last_postdate NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID zmco.
TYPE-POOLS : slis.
TABLES: lfa1, bsik.

DATA: BEGIN OF it_type OCCURS 50,
      blart LIKE bsik-blart,
      seq(2) TYPE n,
      END OF it_type.

DATA: BEGIN OF it_data OCCURS 0,
      bukrs LIKE bsik-bukrs,
      lifnr LIKE lfa1-lifnr,
      name1 LIKE lfa1-name1,
      belnr LIKE bsik-belnr,
      budat LIKE bsik-budat,
      budat01 LIKE bsik-budat,
      belnr01 LIKE bsik-belnr,
      budat02 LIKE bsik-budat,
      belnr02 LIKE bsik-belnr,
      budat03 LIKE bsik-budat,
      belnr03 LIKE bsik-belnr,
      budat04 LIKE bsik-budat,
      belnr04 LIKE bsik-belnr,
      budat05 LIKE bsik-budat,
      belnr05 LIKE bsik-belnr,
      budat06 LIKE bsik-budat,
      belnr06 LIKE bsik-belnr,
      budat07 LIKE bsik-budat,
      belnr07 LIKE bsik-belnr,
      budat08 LIKE bsik-budat,
      belnr08 LIKE bsik-belnr,
      budat09 LIKE bsik-budat,
      belnr09 LIKE bsik-belnr,
      budat10 LIKE bsik-budat,
      belnr10 LIKE bsik-belnr,
      budat11 LIKE bsik-budat,
      belnr11 LIKE bsik-belnr,
      budat12 LIKE bsik-budat,
      belnr12 LIKE bsik-belnr,
      budat13 LIKE bsik-budat,
      belnr13 LIKE bsik-belnr,
      budat14 LIKE bsik-budat,
      belnr14 LIKE bsik-belnr,
      budat15 LIKE bsik-budat,
      belnr15 LIKE bsik-belnr,
      budat16 LIKE bsik-budat,
      belnr16 LIKE bsik-belnr,
      budat17 LIKE bsik-budat,
      belnr17 LIKE bsik-belnr,
      budat18 LIKE bsik-budat,
      belnr18 LIKE bsik-belnr,
      budat19 LIKE bsik-budat,
      belnr19 LIKE bsik-belnr,
      budat20 LIKE bsik-budat,
      belnr20 LIKE bsik-belnr,
** Furong on 06/24/14 (
      sperr LIKE lfb1-sperr,
      loevm LIKE lfb1-loevm,
** )
      END OF it_data.

DATA : it_fieldcat_100  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname_100 TYPE slis_t_fieldcat_alv,
       it_fieldcat_110  TYPE lvc_t_fcat WITH HEADER LINE,
       it_sort_100     TYPE lvc_t_sort WITH HEADER LINE.

DATA : w_fieldname_100  LIKE LINE OF it_fieldcat_100,
       wa_is_layout_100 TYPE lvc_s_layo.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant_100 TYPE disvariant.


DATA: it_exclude TYPE ui_functions.

DATA: wa_custom_control TYPE scrfname VALUE 'ALV_CONTAINER'.

DATA: alv_grid_100          TYPE REF TO cl_gui_alv_grid,
      g_docking_container_100 TYPE REF TO cl_gui_docking_container.

DATA: ok_code  LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt TYPE i,
      w_code      LIKE sy-ucomm.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
*              IMPORTING e_oject e_ucomm,
          IMPORTING es_col_id es_row_no,

handle_hotspot_click FOR  EVENT hotspot_click OF cl_gui_alv_grid
             IMPORTING e_row_id
                       e_column_id
                       es_row_no.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_button_click.
    PERFORM handle_button_click USING es_col_id es_row_no.
  ENDMETHOD .                    "handle_button_click
  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_row_id
                                  e_column_id
                                  es_row_no.

  ENDMETHOD.                    "handle_hotspot_click1
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
   s_bukrs FOR bsik-bukrs.
SELECT-OPTIONS:
   s_lifnr FOR lfa1-lifnr,
   s_ktokk FOR lfa1-ktokk,
   s_budat FOR bsik-budat,
   s_blart FOR bsik-blart.
SELECTION-SCREEN SKIP.
PARAMETERS: p_open  RADIOBUTTON  GROUP radl,
            p_clr  RADIOBUTTON GROUP radl,
            p_all   RADIOBUTTON GROUP radl.

SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-m13.
SELECTION-SCREEN COMMENT 1(60) text-m14.
SELECTION-SCREEN COMMENT /15(60) text-m15.
SELECTION-SCREEN END OF BLOCK block2.

INITIALIZATION.
  PERFORM init_data.

AT SELECTION-SCREEN OUTPUT.
*  PERFORM set_screen.

START-OF-SELECTION.
  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE s000 WITH text-m01.
  ELSE.
    PERFORM display_data.
  ENDIF.
*&--------------------------------------------------------------------*
*&      Form  get_message
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_IT_MESS_MSGID  text
*      -->P_IT_MESS_MSGNR  text
*      -->P_IT_MESS_MSGV1  text
*      -->P_IT_MESS_MSGV2  text
*      -->P_IT_MESS_MSGV3  text
*      -->P_IT_MESS_MSGV4  text
*      <--P_L_MESSA  text
*--------------------------------------------------------------------*
FORM get_message USING    p_msgid
                          p_msgnr
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                 CHANGING p_l_messa.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = p_msgid
      msgnr               = p_msgnr
      msgv1               = p_msgv1
      msgv2               = p_msgv2
      msgv3               = p_msgv3
      msgv4               = p_msgv4
    IMPORTING
      message_text_output = p_l_messa.
ENDFORM.                    " get_message

*&--------------------------------------------------------------------*
*&      Form  display_data
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 100.
ENDFORM.                    " display_data

*&--------------------------------------------------------------------*
*&      Form  GET_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM get_data.

  DATA: BEGIN OF lt_temp OCCURS 0,
       bukrs LIKE bsik-bukrs,
       lifnr LIKE lfa1-lifnr,
       blart LIKE bsik-blart,
       budat LIKE bsik-budat,
       belnr LIKE bsik-belnr,
       gjahr LIKE bsik-gjahr,
       ktokk LIKE lfa1-ktokk,
       name1 LIKE lfa1-name1,
       END OF lt_temp.

  DATA: lt_last LIKE TABLE OF lt_temp WITH HEADER LINE.

  DATA: l_flag(1),
         l_seq(2) TYPE n,
         l_text(50),
         l_postdate TYPE sy-datum.

  DATA: begin of lt_lfb1 OCCURS 0,
        lifnr like lfb1-lifnr,
        bukrs like lfb1-bukrs,
        sperr like lfb1-sperr,
        loevm like lfb1-loevm,
        end of lt_lfb1.
  data: l_index LIKE sy-index.

  FIELD-SYMBOLS: <fs_date>, <fs_doc>.

  CASE 'X'.
    WHEN p_open.
      SELECT bukrs a~lifnr blart budat belnr gjahr ktokk name1
       INTO CORRESPONDING FIELDS OF TABLE lt_temp
       FROM bsik AS a
       INNER JOIN lfa1 AS b
       ON a~lifnr = b~lifnr
       WHERE bukrs IN s_bukrs
         AND a~lifnr IN s_lifnr
         AND ktokk IN s_ktokk
         AND budat IN s_budat
         AND blart IN s_blart.
    WHEN p_clr.
      SELECT bukrs a~lifnr blart budat belnr gjahr ktokk name1
         INTO CORRESPONDING FIELDS OF TABLE lt_temp
         FROM bsak AS a
         INNER JOIN lfa1 AS b
         ON a~lifnr = b~lifnr
         WHERE bukrs IN s_bukrs
           AND a~lifnr IN s_lifnr
           AND ktokk IN s_ktokk
           AND budat IN s_budat
           AND blart IN s_blart.
    WHEN p_all.
      SELECT bukrs a~lifnr blart budat belnr gjahr ktokk name1
      INTO CORRESPONDING FIELDS OF TABLE lt_temp
      FROM bsik AS a
      INNER JOIN lfa1 AS b
      ON a~lifnr = b~lifnr
      WHERE bukrs IN s_bukrs
        AND a~lifnr IN s_lifnr
        AND ktokk IN s_ktokk
        AND budat IN s_budat
        AND blart IN s_blart.

      SELECT bukrs a~lifnr blart budat belnr gjahr ktokk name1
        APPENDING CORRESPONDING FIELDS OF TABLE lt_temp
         FROM bsak AS a
        INNER JOIN lfa1 AS b
        ON a~lifnr = b~lifnr
        WHERE bukrs IN s_bukrs
          AND a~lifnr IN s_lifnr
          AND ktokk IN s_ktokk
          AND budat IN s_budat
          AND blart IN s_blart.
  ENDCASE.

  CHECK sy-subrc = 0.
  SORT lt_temp BY bukrs lifnr blart budat DESCENDING.

  LOOP AT lt_temp.
    it_type-blart = lt_temp-blart.
    COLLECT it_type.

    AT NEW blart.
      l_flag = 'X'.
    ENDAT.
    IF l_flag = 'X'.
      CLEAR: l_flag.
      MOVE-CORRESPONDING lt_temp TO lt_last.
      APPEND lt_last.
    ENDIF.
  ENDLOOP.

  SORT it_type BY blart.
  l_seq = '00'.
  LOOP AT it_type.
    l_seq = l_seq + 1.
    it_type-seq = l_seq.
    MODIFY it_type.
  ENDLOOP.

  l_seq = '00'.
  SORT lt_last BY bukrs lifnr.
  LOOP AT lt_last.
    AT NEW lifnr.
      l_flag = 'X'.
    ENDAT.
    IF l_flag = 'X'.
      CLEAR: l_flag.
      MOVE-CORRESPONDING lt_last TO it_data.
    ENDIF.

    READ TABLE it_type WITH KEY blart = lt_last-blart.
    CONCATENATE 'IT_DATA-BUDAT' it_type-seq INTO l_text.
    ASSIGN (l_text) TO <fs_date>.
    IF sy-subrc = 0.
      <fs_date> = lt_last-budat.
    ENDIF.
    IF  lt_last-budat > l_postdate.
      l_postdate = lt_last-budat.
    ENDIF.
    CONCATENATE 'IT_DATA-BELNR' it_type-seq INTO l_text.
    ASSIGN (l_text) TO <fs_doc>.
    IF sy-subrc = 0.
      <fs_doc> = lt_last-belnr.
    ENDIF.
    AT END OF lifnr.
      it_data-budat = l_postdate.
      APPEND it_data.
      CLEAR: it_data, l_postdate.
    ENDAT.
  ENDLOOP.
** Furong on 06/24/14 (
  SELECT lifnr bukrs sperr loevm INTO TABLE lt_lfb1
     FROM lfb1
    FOR ALL ENTRIES IN it_data
    WHERE lifnr = it_data-lifnr
      AND bukrs = it_data-bukrs.
  SORT lt_lfb1 BY lifnr bukrs.
  LOOP AT it_data.
    l_index = sy-tabix.
    READ TABLE lt_lfb1 WITH KEY lifnr = it_data-lifnr
                                bukrs = it_data-bukrs
                                BINARY SEARCH.
    IF sy-subrc = 0.
      it_data-sperr = lt_lfb1-sperr.
      it_data-loevm = lt_lfb1-loevm.
      MODIFY it_data INDEX l_index TRANSPORTING sperr loevm.
    ENDIF.
  ENDLOOP.
** )
ENDFORM.                    " GET_DATA
*&--------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_container_n_object_100 .
  DATA: l_repid LIKE sy-repid,
        l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container_100
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid_100
    EXPORTING
      i_parent      = g_docking_container_100
      i_appl_events = 'X'.

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_button_click FOR alv_grid_100.
  SET HANDLER g_event_receiver->handle_hotspot_click FOR alv_grid_100.


ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_100

*&--------------------------------------------------------------------*
*&      Form  build_field_catalog_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_ITAB     text
*---------------------------------------------------------------------*
FORM build_field_catalog_100 USING p_itab.
  DATA: lw_itab TYPE slis_tabname,
         lw_waers LIKE t001-waers,
         l_rqty(9),
         l_datum(8),
         l_cn(2) TYPE n,
         l_text(30).

  CLEAR: it_fieldcat_100,  it_fieldcat_100[],
         it_fieldname_100, it_fieldname_100[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname_100.



  PERFORM setting_fieldcat_100 TABLES it_fieldcat_100 USING :

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'NAME1'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Name',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'BUDAT'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Post. Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SPERR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Blocked tag',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'LOEVM'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Deletion tag',
                                  'E' 'OUTPUTLEN'   '4'.

  LOOP AT it_type.
    CONCATENATE 'BUDAT' it_type-seq INTO l_text.

    PERFORM setting_fieldcat_100 TABLES it_fieldcat_100 USING :
                                    'S' l_text       ' ',
                                    ' ' 'COLTEXT'     it_type-blart,
                                    ' ' 'HOTSPOT'     'X',
                                    ' ' 'JUST'        'C',
                                    'E' 'OUTPUTLEN'   '5'.
  ENDLOOP.
*

ENDFORM.                    " build_field_catalog
*&--------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST100'.
  SET TITLEBAR 'ST100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&--------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0200  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE display_alv_0100 OUTPUT.
  IF g_docking_container_100 IS INITIAL.
    PERFORM create_container_n_object_100.
    PERFORM set_attributes_alv_grid_100.
    PERFORM build_sortcat_display_100.
    PERFORM build_field_catalog_100 USING 'IT_DATA'.
    PERFORM assign_itab_to_alv_100.

  ELSE.
    CALL METHOD alv_grid_100->refresh_table_display.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_0200  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM assign_itab_to_alv_100 .
  CALL METHOD alv_grid_100->set_table_for_first_display
    EXPORTING
      is_layout       = wa_is_layout_100
      i_save          = wa_save
      is_variant      = wa_variant_100
    CHANGING
      it_fieldcatalog = it_fieldcat_100[]
      it_sort         = it_sort_100[]
      it_outtab       = it_data[].
ENDFORM.                    " ASSIGN_ITAB_TO_ALV_100
*&--------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  w_code = ok_code.
  CASE w_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      PERFORM refresh.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&--------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_100  text
*      -->P_3903   text
*      -->P_3904   text
*      -->P_3905   text
*---------------------------------------------------------------------*
FORM setting_fieldcat_100 TABLES p_fieldcat STRUCTURE it_fieldcat_100
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname_100 INTO w_fieldname_100
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname_100-fieldname TO p_fieldcat-fieldname.
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

*&-------------------------------------------------------------------*
*&      Form  REFRESH
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM refresh .
  DATA: ls_stable TYPE lvc_s_stbl.
  ls_stable-row = 'X'.
  ls_stable-col = 'X'.
  CALL METHOD alv_grid_100->refresh_table_display
    EXPORTING
      is_stable = ls_stable.
*      i_soft_refresh = 'X'.
ENDFORM.                    " REFRESH

*&--------------------------------------------------------------------*
*&      Form  HANDLE_BUTTON_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_ES_COL_ID  text
*      -->P_ES_ROW_NO  text
*---------------------------------------------------------------------*
FORM handle_button_click  USING  e_column  TYPE lvc_s_col
                                 es_row_no TYPE lvc_s_roid.

  READ TABLE it_data INDEX es_row_no-row_id.
  CHECK sy-subrc = 0.

  CASE e_column-fieldname.
    WHEN 'NAME'.

  ENDCASE.
ENDFORM.                    " HANDLE_BUTTON_CLICK

*&--------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*---------------------------------------------------------------------*
FORM hotspot_click  USING p_e_row_id  TYPE  lvc_s_row
                          p_e_column_id TYPE  lvc_s_col
                          p_es_row_no	TYPE lvc_s_roid.

  DATA: l_seq(2) TYPE n,
        l_text(40),
        l_gjahr LIKE bsik-gjahr..

  FIELD-SYMBOLS: <fs>.

  READ TABLE it_data INDEX p_es_row_no-row_id.
  CHECK sy-subrc = 0.

  CASE p_e_column_id-fieldname.
    WHEN 'LIFNR'.
      SET PARAMETER ID 'LIF' FIELD it_data-lifnr.
      SET PARAMETER ID 'BUK' FIELD it_data-bukrs.
      CALL TRANSACTION 'FK03'. " AND SKIP FIRST SCREEN.
*      SUBMIT zemmpm45r_dis_mod_prc_ebom_ztb
*          WITH p_mcode = it_mod_info-mcode
*          WITH p_matnr = it_mod_info-matnr
*          AND RETURN.
    WHEN OTHERS.
      CHECK p_e_column_id-fieldname+0(5) = 'BUDAT'.
      l_seq = p_e_column_id-fieldname+5(2).
      CONCATENATE 'IT_DATA-BELNR' l_seq INTO l_text.
      ASSIGN (l_text) TO <fs>.
      IF sy-subrc = 0.
        CHECK <fs> IS NOT INITIAL.

        SET PARAMETER ID 'BLN' FIELD <fs>.

        CONCATENATE 'IT_DATA-' p_e_column_id-fieldname INTO l_text.
        ASSIGN (l_text) TO <fs>.
        l_gjahr = <fs>+0(4).

        CHECK sy-subrc = 0.
        SET PARAMETER ID 'GJR' FIELD <fs>.
        SET PARAMETER ID 'BUK' FIELD it_data-bukrs.
        CALL TRANSACTION 'FB03'  AND SKIP FIRST SCREEN..

      ENDIF.
  ENDCASE.
ENDFORM.                    " HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_100 .

  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout_100, wa_variant_100.

*//-- Set Layout Structure
  wa_is_layout_100-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout_100-sel_mode   = 'A'.
  wa_is_layout_100-zebra      = 'X'.
  wa_is_layout_100-language   = sy-langu. "/Language Key
  wa_is_layout_100-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout_100-ctab_fname = 'CT'.
*  wa_is_layout_100-stylefname = 'CELLTAB'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant_100-report       = sy-repid.
  wa_variant_100-username     = sy-uname.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_100

*&--------------------------------------------------------------------*
*&      Form  EXCLUDE_UI_FUNCTION
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM exclude_ui_function .

  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO it_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
  APPEND ls_exclude TO it_exclude.

ENDFORM.                    " EXCLUDE_UI_FUNCTION
*&--------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM build_sortcat_display_100.
  it_sort_100-spos           = 1.
  it_sort_100-fieldname      = 'LIFNR'.
  it_sort_100-up             = 'X'.
  it_sort_100-subtot         = 'X'.
  APPEND it_sort_100.

*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = 'X'.
*  APPEND it_sort.
*

  APPEND it_sort_100.
ENDFORM.                    " build_sortcat_display_100
*&--------------------------------------------------------------------*
*&      Form  INIT_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM init_data .

  s_bukrs-sign = 'I'.
  s_bukrs-option = 'EQ'.
  s_bukrs-low = 'H201'.
  APPEND s_bukrs.

  s_ktokk-sign = 'I'.
  s_ktokk-option = 'EQ'.
  s_ktokk-low = 'Y020'.
  APPEND s_ktokk.
  s_ktokk-low = 'Y030'.
  APPEND s_ktokk.
  s_ktokk-low = 'Y040'.
  APPEND s_ktokk.
  s_ktokk-low = 'Y050'.
  APPEND s_ktokk.
  s_ktokk-low = 'Y060'.
  APPEND s_ktokk.
ENDFORM.                    " INIT_DATA
