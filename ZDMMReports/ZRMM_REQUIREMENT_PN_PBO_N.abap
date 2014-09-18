*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
** On 08/26/13 by Furong
*  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    IF g_docking_container IS INITIAL.
** End on 08/26/13
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_OUTPUT'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
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
  DATA:   l_repid LIKE sy-repid,
          l_dynnr like sy-dynnr.

** O 08/26/13 By Furong
*  CREATE OBJECT grid_container
*          EXPORTING container_name = wa_custom_control
*          EXCEPTIONS
*           cntl_error = 1
*           cntl_system_error = 2
*           create_error = 3
*           lifetime_error = 4
*           lifetime_dynpro_dynpro_link = 5.
    l_repid = sy-repid.
    l_dynnr = sy-dynnr.
    CREATE OBJECT g_docking_container
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.
** End on 08/26/13
   IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = l_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
*         EXPORTING i_parent = grid_container
         EXPORTING i_parent = g_docking_container
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
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
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
*
*  it_sort-spos           = 4.
*  it_sort-fieldname      = 'DISPO'.
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
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid "'ZRMM_REQUIREMENT_PLAN_PBO_N'
            i_internal_tabname = lw_itab
            i_inclname         = w_repid "'ZRMM_REQUIREMENT_PLAN_PBO_N'
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'WERKS'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'DISPO'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'MRP',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'DAYS'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Days',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DECIMALS_O'  '1',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'LABST'       ' ',
                                  ' ' 'COLTEXT'     'Inventory',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'SEQ'         ' ',
                                  ' ' 'COLTEXT'     'S',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'RQTY_P'       ' ',
                                  ' ' 'COLTEXT'     'Past Due',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '13'.
  l_cn = '00'.
  DO 21 TIMES.
    l_cn = l_cn + 1.
    IF r01 = 'X'.
      READ TABLE it_day WITH KEY seq = l_cn.
    ELSE.
      READ TABLE it_week WITH KEY seq = l_cn.
    ENDIF.

    CONCATENATE 'RQTY_' l_cn INTO l_rqty.
    IF r01 = 'X'.
      WRITE it_day-datum TO l_datum MM/DD/YY.
    ELSE.
      WRITE it_week-datum TO l_datum MM/DD/YY.
    ENDIF.
    PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                   'S' l_rqty        ' ',
                                   ' ' 'COLTEXT'     l_datum,
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '13'.
    CLEAR: l_rqty.
  ENDDO.

*                                  'S' 'DMBTR'       ' ',
*                                  ' ' 'DO_SUM'      'X',
*                                  ' ' 'COLTEXT'     'Trn Amount',
*                                  'E' 'CURRENCY'    lw_waers,

*                                  'S' 'MAKTX'       ' ',
*                                  ' ' 'KEY'         'X',
*                                  'E' 'COLTEXT'     'Description',

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'EISBE'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Safety',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Mat. Desc',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'WMSTK'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'WM Stk',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'COGI'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'COGI',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13'.

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
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_output[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0210 output.
  SET PF-STATUS 'ST210'.
  SET TITLEBAR 'T210'.
endmodule.                 " STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object_PO.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container_PO
          EXPORTING container_name = wa_custom_control_PO
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
  CREATE OBJECT alv_grid_PO
         EXPORTING i_parent = grid_container_PO
                   i_appl_events = 'X'.

endform.                    " create_container_n_object_PO

MODULE display_alv_210 OUTPUT.
  IF grid_container_PO IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_PO.
    PERFORM set_attributes_alv_grid.
*    PERFORM build_sortcat_display.
    PERFORM build_field_catalog_PO USING 'IT_PO_DATA'.
    PERFORM assign_itab_to_alv_PO.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid_PO->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT



FORM build_field_catalog_PO USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.
  CLEAR: it_fieldcat[], it_fieldname[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                 'S' 'VENDOR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATL_GROUP'  ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATERIAL'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'DESC'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                   'S' 'PLANT'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '4',
*
*                                  'S' 'STGE_LOC'    ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Storage',
*                                  'E' 'OUTPUTLEN'   '4',


                                  'S' 'QUANTITY'       ' ',
                                  ' ' 'COLTEXT'     'DashBoard Qty',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'ORDER_QTY'       ' ',
                                  ' ' 'COLTEXT'     'Order Qty',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'ROUND_QTY'       ' ',
                                  ' ' 'COLTEXT'     'Round Qty',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'PO_UNIT'    ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Per',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'PO_NO'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '12'.


ENDFORM.                    " build_field_catalog_PO

FORM assign_itab_to_alv_PO.

  CALL METHOD alv_grid_PO->set_table_for_first_display

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
*               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_PO_DATA[].
*               it_sort          = it_sort[].

ENDFORM.
