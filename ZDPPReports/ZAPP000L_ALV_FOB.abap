*----------------------------------------------------------------------*
*   INCLUDE ZAPP000L_ALV_FOB                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       Building events For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events.

  CONSTANTS : c_pss(20) TYPE c VALUE 'PF_STATUS_SET',
              c_uc(20)  TYPE c VALUE 'USER_COMMAND'.
*              c_top type slis_formname value 'TOP_OF_PAGE'.
  REFRESH gt_events.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  PERFORM modify_gt_events
          TABLES  gt_events
          USING : slis_ev_pf_status_set c_pss,
                  slis_ev_user_command  c_uc.
*                  slis_ev_top_of_page   c_top.

ENDFORM.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Setting Name&Form For ALV Event
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_PF_STATUS_SET  text
*      -->P_C_PSS  text
*----------------------------------------------------------------------*
FORM modify_gt_events TABLES p_events_t LIKE gt_events
                      USING  p_form p_value.

  DATA: ls_event TYPE slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_form
                          INTO ls_event.
  IF sy-subrc EQ 0.
    MOVE     p_value     TO   ls_event-form.
    MODIFY   p_events_t  FROM ls_event INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " modify_gt_events
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Setting Layout For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout USING p_cb p_color p_sum.

  CLEAR gs_layout.

  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
  gs_layout-default_item      = 'X'.
* check box
  IF p_cb = 'X'.
    gs_layout-box_fieldname    = 'CHKBOX'.
  ENDIF.
* line color
  IF p_color = 'X'.
    gs_layout-coltab_fieldname = 'COLOR'.
  ENDIF.
* sum
  IF p_sum = 'X'.
    gs_layout-totals_text       = 'TOT'.
  ENDIF.
*
ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Calling A Func. For Starting GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT01  text
*----------------------------------------------------------------------*
FORM start_grid_viewer TABLES p_intab.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
*            i_background_id    = 'ALV_BACKGROUND' "HEADER? ??
            i_callback_program = g_repid
            is_layout          = gs_layout
            it_fieldcat        = gt_fieldcat[]
*            IT_SORT            = GT_SORT[]
            i_save             = 'A'
            is_variant         = g_variant
            it_events          = gt_events[]
            is_print           = gs_print
*            it_list_commentary = gt_header
       IMPORTING
            e_exit_caused_by_caller = g_exit_caused_by_caller
            es_exit_caused_by_user  = gs_exit_caused_by_user
       TABLES
            t_outtab           = p_intab.

*  if gs_exit_caused_by_user cs 'X'.
*    set screen 0.
*  endif.

ENDFORM.                    " start_grid_viewer
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       Setting Field Category
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0108   text
*      -->P_0109   text
*      -->P_0110   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat LIKE gt_fieldcat
                      USING    p_gubun p_field p_value.

  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR : g_fieldcat_s.
    READ TABLE gt_fc INTO g_fieldcat_s
                     WITH KEY fieldname  = p_field.
    EXIT.
  ENDIF.

  CONCATENATE 'G_FIELDCAT_S-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO cnt.
    g_fieldcat_s-col_pos = cnt.
    APPEND g_fieldcat_s TO p_fieldcat.
  ENDIF.

ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_print_option
*&---------------------------------------------------------------------*
*       Setting Print Option For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_print_option.

  gs_print-no_print_listinfos = 'X'.

ENDFORM.                    " build_print_option
