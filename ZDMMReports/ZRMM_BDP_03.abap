************************************************************************
* Program Name      : ZRMM_BDP_03
* Creation Date     : 12/2009
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrmm_bdp_03 NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.
TABLES: mara, mard, lagp, lfa1, ztmm_bdp.

DATA: BEGIN OF it_itab OCCURS 0,
      mstae LIKE mara-mstae,
      werks LIKE ztmm_bdp-werks,
      lgort LIKE ztmm_bdp-lgort,
      matnr LIKE ztmm_bdp-matnr,
      maktx LIKE makt-maktx,
      lifnr LIKE eord-lifnr,
      name1 LIKE lfa1-name1,
      matkl LIKE mara-matkl,
**Paul
*      lgtyp LIKE lagp-lgtyp,
*      ltypt LIKE t301t-ltypt,
      bstrf LIKE marc-bstrf,
      feeder LIKE ztmm_bdp-feeder,
      lgpla LIKE ztmm_bdp-lgpla,
      del_method LIKE ztmm_bdp-del_method,
      zusage LIKE ztmm_bdp-zusage,
      zoption LIKE ztmm_bdp-zoption,
      zvariant LIKE ztmm_bdp-zvariant,
      apps LIKE ztmm_bdp-apps,
      route_cycle LIKE ztmm_bdp-route_cycle,
      erdat LIKE ztmm_bdp-erdat,
      erzet LIKE ztmm_bdp-erzet,
      ernam LIKE ztmm_bdp-ernam,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
     END OF it_itab.

*DATA: WA_BDP LIKE ZTMM_BDP.
DATA: w_old_bin LIKE ztmm_bdp-lgpla.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt TYPE i.

DATA:  wa_stbl  TYPE lvc_s_stbl.
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_exclude      TYPE ui_functions.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname  LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

FIELD-SYMBOLS : <fs01>, <fs02>, <fs03>.

DATA:  w_refresh(1),
       w_new(1) VALUE 'X'.

* Printing option for Label
DATA: zoptions LIKE	itcpo OCCURS 0 WITH HEADER LINE.
DATA: zprinter(4) VALUE 'RFL'.
DATA: w_frm_lab(1),
      w_frm_bin(1),
      w_barc_kb(1),
      w_barc_hd(1),
      w_barc_no(1),
      w_icon_stl(1),
      w_icon_hd(1),
      w_icon_no(1).
DATA: it_bdp_label LIKE TABLE OF zsmm_bdp_label WITH HEADER LINE.

*DATA: BEGIN OF IT_BDP_LABEL OCCURS 0,
*      MATNR LIKE ZTMM_BDP-MATNR,
*      WERKS LIKE ZTMM_BDP-WERKS,
*      LGORT LIKE ZTMM_BDP-LGORT,
*      LGPLA LIKE ZTMM_BDP-LGPLA,
*      RDMNG LIKE ZTMM_BDP-RDMNG,
*      FEEDER LIKE ZTMM_BDP-FEEDER,
*      SCRAP_WS LIKE ZTMM_BDP-SCRAP_WS,
*      DEL_METHOD LIKE ZTMM_BDP-DEL_METHOD,
*      ZOPTION LIKE ZTMM_BDP-ZOPTION,
*      ZVARIANT LIKE ZTMM_BDP-ZVARIANT,
*      APPS LIKE ZTMM_BDP-APPS,
*      ZUSAGE LIKE ZTMM_BDP-ZUSAGE,
*      ROUTE_CYCLE LIKE ZTMM_BDP-ROUTE_CYCLE,
*      BARCODE(80),
*      ICON(1),
*      ICONP(20),
*      MAKTX(40),
*      END OF IT_BDP_LABEL.
* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.

    DATA: error_in_data TYPE c.

ENDCLASS.
DATA :it_lvc  LIKE lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          w_qty(13),
          lvc_t_row TYPE lvc_t_row.

    error_in_data = space.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
* check if column Name1 of this row was changed
        WHEN 'FEEDER' OR 'SCRAP_WS' OR 'DEL_METHOD' OR 'ZOPTION' OR
             'ZVARIANT' OR 'APPS' OR 'ZUSAGE' OR 'ROUTE_CYCLE'.
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

      ENDCASE.
    ENDLOOP.
*§7.Display application log if an error has occured.
    IF error_in_data EQ 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  s_werks FOR mard-werks,
  s_mstae FOR mara-mstae,
  s_matnr FOR mara-matnr,
  s_matkl FOR mara-matkl,
  s_lgort FOR mard-lgort,
  s_lgpla FOR lagp-lgpla,
  s_feeder FOR ztmm_bdp-feeder,
  s_method FOR ztmm_bdp-del_method,
  s_lifnr FOR lfa1-lifnr.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
PARAMETERS: p_mode(1).
SELECTION-SCREEN END OF BLOCK block2.

INITIALIZATION.

  IF sy-tcode = 'ZRMM_BDP03'.
    p_mode = 'E'.
  ELSE.
    p_mode = 'D'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
*  IF SY-TCODE = 'ZRMM_BDP03'.
  LOOP AT SCREEN.
    IF screen-name = 'P_MODE'.
      screen-input = '0'.
*        SCREEN-INVISIBLE  = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*  ENDIF.
*AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM get_data.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    CALL SCREEN 200.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_screen.
  LOOP AT SCREEN.
    IF screen-name = 'P_EXCEL'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_edit_line.
  DATA: lt_celltab TYPE lvc_t_styl,
        w_celltab TYPE lvc_s_styl,
        l_index TYPE i,
        l_mode TYPE raw4.
  REFRESH: lt_celltab.
  LOOP AT it_itab.
    l_index = sy-tabix.
    REFRESH lt_celltab.

    w_celltab-fieldname = 'MSTAE'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'WERKS'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'LGORT'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'MATNR'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'MAKTX'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'LIFNR'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'NAME1'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'MATKL'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.

*    w_celltab-fieldname = 'LGTYP'.
*    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*    INSERT w_celltab INTO TABLE lt_celltab.
*    w_celltab-fieldname = 'LTYPT'.
*    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'BSTRF'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'LGPLA'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.


    l_mode = cl_gui_alv_grid=>mc_style_enabled.
    w_celltab-fieldname = 'FEEDER'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'SCRAP_WS'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'DEL_METHOD'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'ZOPTION'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'ZVARIANT'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'APPS'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'ZUSAGE'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'ROUTE_CYCLE'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    INSERT LINES OF lt_celltab INTO TABLE it_itab-celltab.
    MODIFY it_itab INDEX l_index.
  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  IF p_mode = 'D'.
    SET PF-STATUS 'ST200A'.
    SET TITLEBAR 'ST200A'.
  ELSE.
    SET PF-STATUS 'ST200'.
    SET TITLEBAR 'ST200'.
  ENDIF.


ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_flag(1),
        l_rdmng LIKE mlgt-rdmng,
        l_bstrf like pkhd-behmg.

  DATA: BEGIN OF lt_data OCCURS 0,
        mstae LIKE mara-mstae,
        werks LIKE ztmm_bdp-werks,
        lgort LIKE ztmm_bdp-lgort,
        matnr LIKE ztmm_bdp-matnr,
        maktx LIKE makt-maktx,
        lifnr LIKE eord-lifnr,
        name1 LIKE lfa1-name1,
        matkl LIKE mara-matkl,
*        lgtyp LIKE lagp-lgtyp,
*        ltypt LIKE t301t-ltypt,
        bstrf LIKE marc-bstrf,
        feeder LIKE ztmm_bdp-feeder,
        lgpla LIKE ztmm_bdp-lgpla,
        del_method LIKE ztmm_bdp-del_method,
        zusage LIKE ztmm_bdp-zusage,
        zoption LIKE ztmm_bdp-zoption,
        zvariant LIKE ztmm_bdp-zvariant,
        apps LIKE ztmm_bdp-apps,
        route_cycle LIKE ztmm_bdp-route_cycle,
        erdat LIKE ztmm_bdp-erdat,
        erzet LIKE ztmm_bdp-erzet,
        ernam LIKE ztmm_bdp-ernam,
        END OF lt_data.

  SELECT mstae a~werks a~lgort a~matnr maktx
      matkl bstrf feeder lgpla del_method zusage zoption
       zvariant apps route_cycle
     INTO CORRESPONDING FIELDS OF TABLE lt_data
     FROM ztmm_bdp AS a
     INNER JOIN mara AS b
     ON a~matnr = b~matnr
     INNER JOIN makt AS m
     ON a~matnr = m~matnr
     INNER JOIN mard AS c
      ON a~matnr = c~matnr
      AND a~werks = c~werks
      AND a~lgort = c~lgort
       INNER JOIN marc AS d
      ON a~matnr = d~matnr
      AND a~werks = d~werks
     WHERE a~matnr IN s_matnr
      AND a~werks IN s_werks
      AND a~lgort IN s_lgort
      AND lgpla IN s_lgpla
      AND mstae IN s_mstae
      AND feeder IN s_feeder
      AND del_method IN s_method
      AND c~lvorm <> 'X'.

  LOOP AT it_itab.
    REFRESH it_itab-celltab.
    MODIFY it_itab.
  ENDLOOP.
  REFRESH it_itab.

  IF sy-subrc = 0.
    LOOP AT lt_data.
      MOVE-CORRESPONDING lt_data TO it_itab.
**      C__Paul : 07/01/11
*      SELECT SINGLE a~lgtyp b~ltypt INTO (it_itab-lgtyp, it_itab-ltypt)
*        FROM lagp AS a
*        INNER JOIN t301t AS b
*        ON a~lgnum = b~lgnum
*        AND a~lgtyp = b~lgtyp
*        WHERE a~lgnum = 'P01'
*          AND lgpla = it_itab-lgpla
*          AND spras = 'EN'.

      IF sy-subrc = 0.
        SELECT SINGLE a~lifnr name1 INTO (it_itab-lifnr, it_itab-name1)
         FROM lfa1 AS a
         INNER JOIN eord AS b
         ON a~lifnr = b~lifnr
         WHERE a~lifnr IN s_lifnr
           AND b~matnr = it_itab-matnr
           AND b~werks = it_itab-werks.
** Changed on 01/31/12
** Added by Furong on 02/07/11
*        SELECT rdmng INTO l_rdmng UP TO 1 ROWS
*                FROM mlgt
*                WHERE matnr = it_itab-matnr
*                  AND lgnum = 'P01'
*                  AND lvorm = ' '.
*        ENDSELECT.
**               AND lgtyp = it_itab-lgtyp.
*        IF sy-subrc = 0.
*          it_itab-bstrf = l_rdmng.
*        ENDIF.
** End of addition
** End on 01/31/12
** Changed on 02/15/12
        SELECT behmg
            INTO l_bstrf UP TO 1 ROWS
            FROM PKHD
           WHERE matnr = it_itab-matnr
             AND WERKS = it_itab-WERKS
*             AND UMLGO = IT_BDP_LABEL-LGORT.
             AND ZFEEDER = it_itab-FEEDER.
          ENDSELECT.
        IF sy-subrc = 0.
          it_itab-bstrf = l_bstrf.
        ENDIF.
      ENDIF.
** End on 02/15/12
      APPEND it_itab.
      CLEAR: it_itab.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM select_edit_line.
    PERFORM exclude_tb_functions.
    PERFORM build_field_catalog USING 'IT_ITAB'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
*     PERFORM BUILD_FIELD_CATALOG USING 'IT_TAB_DAY'.
*    PERFORM ASSIGN_ITAB_TO_ALV.
*
    wa_stbl-row = 'X'.
    wa_stbl-col = 'X'.
    CALL METHOD alv_grid->refresh_table_display
      EXPORTING is_stable = wa_stbl.
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
FORM create_container_n_object.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container
          EXPORTING container_name = wa_custom_control
          EXCEPTIONS
           cntl_error = 1
           cntl_system_error = 2
           create_error = 3
           lifetime_error = 4
           lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
         EXPORTING i_parent = grid_container
                   i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  IF p_mode = 'D'.
    wa_is_layout-edit       =  ' '.
  ELSE.
    wa_is_layout-edit       = 'X'.      "/Edit Mode Enable
  ENDIF.
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = ' '.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-stylefname = 'CELLTAB'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.
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
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).
  FIELD-SYMBOLS <fs>.

  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.
    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.
    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
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
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
*               i_default        = space
               it_toolbar_excluding = it_exclude
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_itab[].
*               it_sort          = it_sort[].

** ENTER
  CALL METHOD alv_grid->register_edit_event
                EXPORTING
                   i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  CALL METHOD alv_grid->register_edit_event
                EXPORTING
                   i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_data_changed FOR alv_grid.


  CALL METHOD cl_gui_control=>set_focus
                        EXPORTING control = alv_grid.


ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_tb_functions.
  DATA ls_exclude TYPE ui_func.

* Row manipulation
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
  IF p_mode = 'D'.
    ls_exclude = 'COPY'.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = 'UPDAT_BIN'.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = 'DELETE'.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = 'SAVE'.
    APPEND ls_exclude TO it_exclude.
  ENDIF.
*  Sort buttons
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8).


  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

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

                                  'S' 'MSTAE'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'ST',
                                   'E' 'OUTPUTLEN'   '2',

                                  'S' 'WERKS'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'PLNT',
                                   'E' 'OUTPUTLEN'   '4',

                                  'S' 'LGORT'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'SLOC',
                                   'E' 'OUTPUTLEN'   '4',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '15',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Description',
                                   'E' 'OUTPUTLEN'   '20',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                   'E' 'OUTPUTLEN'   '7',

                                  'S' 'NAME1'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor Name',
                                   'E' 'OUTPUTLEN'   '15',

                                  'S' 'MATKL'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Group',
                                   'E' 'OUTPUTLEN'   '9',
**Paul
*                                  'S' 'LGTYP'       ' ',
*                                  ' ' 'COLTEXT'     'Type',
*                                  'E' 'OUTPUTLEN'   '4',
*
*                                  'S' 'LTYPT'       ' ',
*                                  ' ' 'KEY'         '',
*                                  ' ' 'COLTEXT'     'Storage',
*                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'BSTRF'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Rnd Qty',
                                  'E' 'OUTPUTLEN'   '6',

                                   'S' 'FEEDER'   ' ',
                                    ' ' 'KEY'         '',
                                    ' ' 'COLTEXT'     'FDR',
                                    'E' 'OUTPUTLEN'   '4',

                                     'S' 'LGPLA'   ' ',
                                     ' ' 'COLTEXT'     'BIN',
                                     'E' 'OUTPUTLEN'   '10',

                                     'S' 'DEL_METHOD'   ' ',
                                     ' ' 'COLTEXT'     'DLV',
                                     'E' 'OUTPUTLEN'   '2',

                                     'S' 'ZUSAGE'   ' ',
                                     ' ' 'COLTEXT'     'Usage',
                                     'E' 'OUTPUTLEN'   '5',

                                     'S' 'ZOPTION'   ' ',
                                     ' ' 'COLTEXT'     'Option',
                                     'E' 'OUTPUTLEN'   '7',

                                    'S' 'ZVARIANT'   ' ',
                                     ' ' 'KEY'         '',
                                     ' ' 'COLTEXT'     'Var',
                                     'E' 'OUTPUTLEN'   '5',

                                     'S' 'APPS'   ' ',
                                     ' ' 'COLTEXT'     'Apps',
                                     'E' 'OUTPUTLEN'   '12',

                                     'S' 'ROUTE_CYCLE'   ' ',
                                     ' ' 'COLTEXT'     'Cyc',
                                     'E' 'OUTPUTLEN'   '3'.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'DELETE'.
      PERFORM delete_data.
    WHEN 'COPY'.
      PERFORM copy_data.
    WHEN 'UPDAT_BIN'.
      PERFORM updat_bin.
    WHEN 'PRN_LABEL'.
      PERFORM print_label.
    WHEN 'REFRESH'.
      PERFORM refresh_data.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  DATA: lt_bdp LIKE TABLE OF ztmm_bdp WITH HEADER LINE.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

*  READ TABLE lt_rows INDEX 1.
  LOOP AT lt_rows.
    READ TABLE it_itab INDEX lt_rows-index.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_itab TO lt_bdp.
      lt_bdp-aedat = sy-datum.
      lt_bdp-aezet = sy-uzeit.
      lt_bdp-aenam = sy-uname.
      APPEND lt_bdp.
    ENDIF.
    CLEAR: lt_bdp.
  ENDLOOP.
  IF  lt_bdp[] IS INITIAL.
    MESSAGE i000 WITH 'No Data was selected'.
  ELSE.
    UPDATE ztmm_bdp FROM TABLE lt_bdp.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s000 WITH 'Data was updated successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Data update error'.
    ENDIF.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  DATA: lt_bdp LIKE TABLE OF ztmm_bdp WITH HEADER LINE.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  LOOP AT lt_rows.
    READ TABLE it_itab INDEX lt_rows-index.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_itab TO lt_bdp.
      lt_bdp-aedat = sy-datum.
      lt_bdp-aezet = sy-uzeit.
      lt_bdp-aenam = sy-uname.
      APPEND lt_bdp.
    ENDIF.
    CLEAR: lt_bdp.
  ENDLOOP.

  IF  lt_bdp[] IS INITIAL.
    MESSAGE i000 WITH 'No Data was selected'.
  ELSE.
    DELETE ztmm_bdp FROM TABLE lt_bdp.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s000 WITH 'Data was deleted successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Data deletion error'.
    ENDIF.
  ENDIF.
  PERFORM refresh_data.
ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  refresh_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_data.
  PERFORM get_data.
  wa_stbl-row = 'X'.
  wa_stbl-col = 'X'.
  PERFORM select_edit_line.
*  CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
*    EXPORTING IS_STABLE = WA_STBL.

ENDFORM.                    " refresh_data
*&---------------------------------------------------------------------*
*&      Form  COPY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_data.
* DATA: LT_BDP LIKE TABLE OF ZTMM_BDP WITH HEADER LINE.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  READ TABLE it_itab INDEX lt_rows-index.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING it_itab TO ztmm_bdp.
*    WA_BDP-AEDAT = SY-DATUM.
*    WA_BDP-AEZET = SY-UZEIT.
*    WA_BDP-AENAM = SY-UNAME.

    CALL SCREEN '0210'.
    PERFORM refresh_data.
  ELSE.
    MESSAGE i000 WITH 'No Data was selected'.
  ENDIF.
*    INSERT ZTMM_BDP.
*    IF SY-SUBRC = 0.
*      COMMIT WORK.
*      MESSAGE S000 WITH 'Data was deleted successfully'.
*    ELSE.
*      ROLLBACK WORK.
*      MESSAGE I000 WITH 'Data deletion error'.
*    ENDIF.

ENDFORM.                    " COPY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0210 OUTPUT.
  SET PF-STATUS 'ST0210'.
  SET TITLEBAR 'ST0210'.

ENDMODULE.                 " STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0210 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'SAVE'.
      PERFORM insert_data.
      LEAVE TO SCREEN 0.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*&      Form  INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_data.

  DATA: l_matnr LIKE mard-matnr,
        l_lgpla LIKE ztmm_bdp-lgpla.

  SELECT SINGLE matnr INTO l_matnr
  FROM mard
  WHERE matnr = ztmm_bdp-matnr
    AND werks = ztmm_bdp-werks
    AND lgort = ztmm_bdp-lgort.
  IF sy-subrc <> 0.
    MESSAGE w009(zmmm) WITH 'No Data in Material Master(MARD)'.
    EXIT.
  ENDIF.

  SELECT SINGLE lgpla INTO l_lgpla
  FROM lagp
  WHERE lgnum = 'P01'
    AND lgpla = ztmm_bdp-lgpla.
  IF sy-subrc <> 0.
    MESSAGE w009(zmmm) WITH 'BIN not found'.
    EXIT.
  ENDIF.
  SELECT SINGLE matnr INTO l_matnr
  FROM ztmm_bdp
  WHERE matnr = ztmm_bdp-matnr
    AND werks = ztmm_bdp-werks
    AND lgort = ztmm_bdp-lgort
    AND lgpla = ztmm_bdp-lgpla.
  IF sy-subrc = 0.
    MESSAGE w009(zmmm) WITH 'Data has existed already'.
  ELSE.
    ztmm_bdp-aedat = sy-datum.
    ztmm_bdp-aezet = sy-uzeit.
    ztmm_bdp-aenam = sy-uname.

    INSERT ztmm_bdp.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s009(zmmm) WITH 'Data was saved'.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

ENDFORM.                    " INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDAT_BIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM updat_bin.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
          lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  READ TABLE it_itab INDEX lt_rows-index.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING it_itab TO ztmm_bdp.
    w_old_bin = ztmm_bdp-lgpla.
    CLEAR: ztmm_bdp-lgpla.
    CALL SCREEN '0220'.
    PERFORM refresh_data.
  ELSE.
    MESSAGE i000 WITH 'No Data was selected'.
  ENDIF.

ENDFORM.                    " UPDAT_BIN
*&---------------------------------------------------------------------*
*&      Module  STATUS_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0220 OUTPUT.
  SET PF-STATUS 'ST0220'.
  SET TITLEBAR 'ST0220'.

ENDMODULE.                 " STATUS_0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0220 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'SAVE'.
      PERFORM update_bin.
      LEAVE TO SCREEN 0.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_bin.
  DATA: l_matnr LIKE mard-matnr,
        l_lgpla LIKE ztmm_bdp-lgpla.

*  SELECT SINGLE MATNR INTO L_MATNR
*  FROM MARD
*  WHERE MATNR = ZTMM_BDP-MATNR
*    AND WERKS = ZTMM_BDP-WERKS
*    AND LGORT = ZTMM_BDP-LGORT.
*  IF SY-SUBRC <> 0.
*    MESSAGE W009(ZMMM) WITH 'No Data in Material Master(MARD)'.
*    EXIT.
*  ENDIF.
*
  SELECT SINGLE lgpla INTO l_lgpla
  FROM lagp
  WHERE lgnum = 'P01'
    AND lgpla = ztmm_bdp-lgpla.
  IF sy-subrc <> 0.
    MESSAGE w009(zmmm) WITH 'BIN not found'.
    EXIT.
  ENDIF.
  IF ztmm_bdp-lgpla <> w_old_bin.

    SELECT SINGLE matnr INTO l_matnr
    FROM ztmm_bdp
    WHERE matnr = ztmm_bdp-matnr
      AND werks = ztmm_bdp-werks
      AND lgort = ztmm_bdp-lgort
      AND lgpla = ztmm_bdp-lgpla.
    IF sy-subrc = 0.
      MESSAGE w009(zmmm) WITH 'New Bin Data has existed already'.
    ELSE.
      ztmm_bdp-aedat = sy-datum.
      ztmm_bdp-aezet = sy-uzeit.
      ztmm_bdp-aenam = sy-uname.

      INSERT ztmm_bdp.
      IF sy-subrc = 0.

        DELETE FROM ztmm_bdp WHERE matnr = ztmm_bdp-matnr
      AND werks = ztmm_bdp-werks
      AND lgort = ztmm_bdp-lgort
      AND lgpla = w_old_bin.
        IF sy-subrc = 0.
          COMMIT WORK.
          MESSAGE s009(zmmm) WITH 'Data was updated'.
          EXIT.
        ENDIF.

        ROLLBACK WORK.
        MESSAGE w009(zmmm) WITH 'Data updated error'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                     " UPDATE_BIN
*&---------------------------------------------------------------------*
*&      Form  PRINT_LABEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_label.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.
  REFRESH: it_bdp_label.
*  READ TABLE lt_rows INDEX 1.
  LOOP AT lt_rows.
    READ TABLE it_itab INDEX lt_rows-index.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_itab TO it_bdp_label.
*      CASE 'X'.
*        WHEN W_BARC_KB.
*          CONCATENATE '[)>*06:P' IT_BDP_LABEL-MATNR ':20L'
*                  IT_BDP_LABEL-WERKS ':22L' IT_BDP_LABEL-FEEDER '*'
*             INTO IT_BDP_LABEL-BARCODE.
*        WHEN W_BARC_HD.
*          SELECT SINGLE BSTRF INTO L_BSTRF
*            FROM MARC
*            WHERE MATNR = IT_BDP_LABEL-MATNR
*              AND WERKS = IT_BDP_LABEL-WERKS.
*          SPLIT L_BSTRF AT '.' INTO L_BSTRF_INT L_BSTRF_DEC.
*          SELECT SINGLE LGTYP INTO L_LGTYP
*          FROM LAGP
*         WHERE LGNUM = 'P01'
*           AND LGPLA = IT_BDP_LABEL-LGPLA.
*          CONCATENATE '[)>*06:P' IT_BDP_LABEL-MATNR ':RFL'
*                  IT_BDP_LABEL-FEEDER ':7Q' L_BSTRF_INT ':25L'
*                  L_LGTYP ':26L' IT_BDP_LABEL-LGPLA '*'
*             INTO IT_BDP_LABEL-BARCODE.
*        WHEN OTHERS.
*          CLEAR: IT_BDP_LABEL-BARCODE.
*      ENDCASE.
*      IT_BDP_LABEL-AEDAT = SY-DATUM.
*      IT_BDP_LABEL-AEZET = SY-UZEIT.
*      IT_BDP_LABEL-AENAM = SY-UNAME.
      APPEND it_bdp_label.
    ENDIF.
    CLEAR: it_bdp_label.
  ENDLOOP.
  IF  it_bdp_label[] IS INITIAL.
    MESSAGE i000 WITH 'No Data was selected'.
  ELSE.
    CALL SCREEN '0230'.
    PERFORM refresh_data.
*    PERFORM INIT_PRINT.
*    PERFORM PRINTING TABLES LT_BDP.
  ENDIF.
*    UPDATE ZTMM_BDP FROM TABLE LT_BDP.
*    IF SY-SUBRC = 0.
*      COMMIT WORK.
*      MESSAGE S000 WITH 'Data was updated successfully'.
*    ELSE.
*      ROLLBACK WORK.
*      MESSAGE I000 WITH 'Data update error'.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " PRINT_LABEL
*&---------------------------------------------------------------------*
*&      Form  printing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM printing.
  DATA: l_bstrf LIKE marc-bstrf,
        l_bstrf_c(17),
        l_bstrf_int(13),
        l_bstrf_dec(3),
        l_lgtyp LIKE lagp-lgtyp,
        l_index LIKE sy-tabix.

  LOOP AT it_bdp_label.
    l_index = sy-tabix.
    SELECT SINGLE maktx INTO it_bdp_label-maktx
      FROM makt
      WHERE matnr = it_bdp_label-matnr
        AND spras = 'EN'.
    CASE 'X'.
      WHEN w_barc_kb.
        IF it_bdp_label-matnr IS INITIAL.
          CLEAR: it_bdp_label-barcode.
        ELSE.
          CONCATENATE '[)>*06:P' it_bdp_label-matnr ':20L'
                  it_bdp_label-werks ':22L' it_bdp_label-feeder '*'
             INTO it_bdp_label-barcode.
        ENDIF.
      WHEN w_barc_hd.
        IF it_bdp_label-matnr IS INITIAL.
          CLEAR: it_bdp_label-barcode.
        ELSE.
** Changed by Furong on 02/07/11
** Changed by Pablo on 07/01/11

*          SELECT SINGLE lgtyp INTO l_lgtyp
*          FROM lagp
*         WHERE lgnum = 'P01'
*           AND lgpla = it_bdp_label-lgpla.

*          SELECT SINGLE BSTRF INTO L_BSTRF
*            FROM MARC
*            WHERE MATNR = IT_BDP_LABEL-MATNR
*              AND WERKS = IT_BDP_LABEL-WERKS.

*          SELECT rdmng INTO l_bstrf UP TO 1 ROWS
*                      FROM mlgt
*           WHERE matnr = it_bdp_label-matnr
*
*                        AND lgnum = 'P01'
*                        AND lvorm = ' '.
*          ENDSELECT.

          SELECT behmg
            INTO l_bstrf UP TO 1 ROWS
            FROM PKHD
           WHERE matnr = it_bdp_label-matnr
             AND WERKS = IT_BDP_LABEL-WERKS
*             AND UMLGO = IT_BDP_LABEL-LGORT.
             AND ZFEEDER = IT_BDP_LABEL-FEEDER.
          ENDSELECT.

          l_bstrf_c = l_bstrf.
          SPLIT l_bstrf_c AT '.'
                                 INTO l_bstrf_int l_bstrf_dec.
          CONDENSE l_bstrf_int.

** End of change
          CONCATENATE '[)>*06:P' it_bdp_label-matnr ':RFL'
                  it_bdp_label-feeder ':7Q'
                  l_bstrf_int
*                  ':25L'
*                  l_lgtyp
                  ':26L'
                  it_bdp_label-lgpla '*'
             INTO it_bdp_label-barcode.
        ENDIF.
      WHEN OTHERS.
        CLEAR: it_bdp_label-barcode.
    ENDCASE.
    CASE 'X'.
      WHEN w_icon_stl.
        it_bdp_label-icon = 'S'.
*        IT_BDP_LABEL-ICONP = 'TEST_002.GRF'.
        it_bdp_label-iconp = 'NEWLA001.GRF'.
      WHEN w_icon_hd.
        it_bdp_label-icon = 'H'.
*        IT_BDP_LABEL-ICONP = 'TEST_001.GRF'.
        it_bdp_label-iconp = 'NEWLA002.GRF'.
      WHEN w_icon_no.
        it_bdp_label-icon = 'N'.
        CLEAR: it_bdp_label-iconp.
    ENDCASE.

    MODIFY it_bdp_label INDEX l_index.
    CLEAR: l_bstrf, l_bstrf_int, l_lgtyp.
  ENDLOOP.

  IF w_frm_lab = 'X'.
    PERFORM printing_label.
  ELSE.
    PERFORM printing_binder.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PRINTING_label                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM printing_label.
  DATA: l_form(20).

  IF w_frm_lab = 'X'.
    l_form = 'ZMM_BDP_LABEL'.
  ELSE.
    l_form = 'ZMMP_BDP_BINDER'.
  ENDIF.

  MOVE '1' TO zoptions-tdcopies.
  MOVE 'X' TO zoptions-tdimmed.
  MOVE 'X' TO zoptions-tdnewid.
  MOVE zprinter TO zoptions-tddest.
  APPEND zoptions.
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            device                      = 'PRINTER'
            dialog                      = 'X'
            form                        = l_form
            language                    = sy-langu
            options                     = zoptions
            raw_data_interface          = '*'
       EXCEPTIONS
            canceled                    = 1
            device                      = 2
            form                        = 3
            options                     = 4
            unclosed                    = 5
            mail_options                = 6
            archive_error               = 7
            invalid_fax_number          = 8
            more_params_needed_in_batch = 9
            spool_error                 = 10
            OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  LOOP AT it_bdp_label.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element  = 'PRINT_LABEL'
              function = 'SET'
              type     = 'BODY'
              window   = 'MAIN'.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'CLOSE_FORM'.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " printing
*&---------------------------------------------------------------------*
*&      Module  STATUS_0230  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0230 OUTPUT.
  SET PF-STATUS 'ST0230'.
  SET TITLEBAR 'ST0230'.
*  PERFORM INIT_0230.
ENDMODULE.                 " STATUS_0230  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0230  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0230 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'PRINT'.
      PERFORM printing.
      LEAVE TO SCREEN 0.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0230  INPUT
*&---------------------------------------------------------------------*
*&      Form  init_0230
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_0230.
  w_frm_lab = 'X'.
  w_barc_kb = 'X'.
  w_icon_stl = 'X'.
ENDFORM.                                                    " init_0230
*&---------------------------------------------------------------------*
*&      Form  PRINTING_BINDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM printing_binder.
  DATA :  fm_name TYPE rs38l_fnam,
          l_cn(1) TYPE n,
          l_text(15).
  DATA:   ws_bdp_1 LIKE zsmm_bdp_label,
          ws_bdp_2 LIKE zsmm_bdp_label,
          ws_bdp_3 LIKE zsmm_bdp_label,
          ws_bdp_4 LIKE zsmm_bdp_label,
          ws_bdp_5 LIKE zsmm_bdp_label,
          ws_bdp_6 LIKE zsmm_bdp_label.

  FIELD-SYMBOLS: <fs>.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            formname           = 'ZMM_BDP_BINDER'
       IMPORTING
            fm_name            = fm_name
       EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.
  CLEAR: l_cn.
  LOOP AT it_bdp_label.
    l_cn = l_cn + 1.
    CONCATENATE 'WS_BDP_' l_cn INTO l_text.
    ASSIGN (l_text) TO <fs>.
    <fs> = it_bdp_label.
    IF l_cn >= 6.

      CALL FUNCTION fm_name
             EXPORTING
                w_datum         = sy-datum
                ws_bdp_1        = ws_bdp_1
                ws_bdp_2        = ws_bdp_2
                ws_bdp_3        = ws_bdp_3
                ws_bdp_4        = ws_bdp_4
                ws_bdp_5        = ws_bdp_5
                ws_bdp_6        = ws_bdp_6
*       TABLES
*            IT_BDP_LABEL       = IT_BDP_LABEL
           EXCEPTIONS
                formatting_error = 1
                internal_error   = 2
                send_error       = 3
                user_canceled    = 4
                OTHERS           = 5.
      CLEAR: l_cn, ws_bdp_1, ws_bdp_2, ws_bdp_3,
             ws_bdp_4, ws_bdp_5, ws_bdp_6.
    ENDIF.
  ENDLOOP.
  IF l_cn > 0.

    CALL FUNCTION fm_name
           EXPORTING
              w_datum         = sy-datum
              ws_bdp_1        = ws_bdp_1
              ws_bdp_2        = ws_bdp_2
              ws_bdp_3        = ws_bdp_3
              ws_bdp_4        = ws_bdp_4
              ws_bdp_5        = ws_bdp_5
              ws_bdp_6        = ws_bdp_6
*       TABLES
*            IT_BDP_LABEL       = IT_BDP_LABEL
         EXCEPTIONS
              formatting_error = 1
              internal_error   = 2
              send_error       = 3
              user_canceled    = 4
              OTHERS           = 5.
    CLEAR: l_cn, ws_bdp_1, ws_bdp_2, ws_bdp_3,
           ws_bdp_4, ws_bdp_5, ws_bdp_6.
  ENDIF.

ENDFORM.                    " PRINTING_BINDER
