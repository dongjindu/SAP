*&---------------------------------------------------------------------*
*&  Include           ZHRR99990T
*&     Subroutine for VRM of ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&	TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*&	ALV
*&---------------------------------------------------------------------*
DATA : gt_nodtb         LIKE snodetext OCCURS 0 WITH HEADER LINE,
       gt_sort          TYPE slis_t_sortinfo_alv,
       gt_sort_lvc      TYPE lvc_t_sort,
       gt_events_lvc    TYPE slis_t_event,
       gt_fieldcat      TYPE slis_t_fieldcat_alv,
       gt_fieldcat_lvc  TYPE lvc_t_fcat,
       gt_header        TYPE slis_t_listheader,
       st_header        TYPE slis_listheader,
       st_layout        TYPE slis_layout_alv,
       st_layout_lvc    TYPE lvc_s_layo,
       gt_gridset       TYPE lvc_s_glay,
       st_gridset       TYPE lvc_s_glay,
       st_variant       LIKE disvariant,
       st_keyinfo       TYPE slis_keyinfo_alv,
       g_save           VALUE 'A',
       g_user_command   TYPE slis_formname VALUE 'ALV_USER_COMMAND',
       g_top_of_page    TYPE slis_formname VALUE 'TOP_OF_PAGE',
       g_data_changed   TYPE slis_formname VALUE 'ALV_DATA_CHANGED',
       g_data_list_chan TYPE slis_formname VALUE 'LIST_CHANGE',
       g_hot_spot       TYPE slis_formname VALUE 'ALV_HOT_SPOT',
       g_sortpos        LIKE alvdynp-sortpos.
DATA : gs_fieldcat_lvc  TYPE lvc_s_fcat,
       gs_fieldcat      like line of gt_fieldcat,
       gs_layout        TYPE lvc_s_layo, "slis_layout_alv,
       gs_variant       LIKE disvariant,
       gs_sort          type slis_sortinfo_alv,
       gs_sort_lvc      TYPE lvc_s_sort, "slis_sortinfo_alv,
       gs_filter        TYPE slis_filter_alv,
       g_index          TYPE i,
       g_repid          LIKE sy-repid,
       G_TITLE(100).
data :
      GT_SPECIALCOL        TYPE SLIS_T_SPECIALCOL_ALV,
      GS_SPECIALCOL        TYPE SLIS_SPECIALCOL_ALV.

*&---------------------------------------------------------------------*
*&      Form  ALV_SORTCAT
*&---------------------------------------------------------------------*
*       ALV SORT definition
*----------------------------------------------------------------------*
FORM alv_sortcat USING  p_fieldname  p_up  p_down.

  DATA : l_sort_ln LIKE LINE OF gt_sort.

  g_sortpos = g_sortpos + 1.

  l_sort_ln-spos      = g_sortpos.
  l_sort_ln-fieldname = p_fieldname.
  l_sort_ln-up        = p_up.
  l_sort_ln-down      = p_down.
  APPEND l_sort_ln TO gt_sort.

ENDFORM.                    " ALV_SORTCAT
*&---------------------------------------------------------------------*
*&      Form  ALV_SORTCAT_LVC
*&---------------------------------------------------------------------*
*       ALV SORT
*----------------------------------------------------------------------*
FORM alv_sortcat_lvc USING  p_fieldname  p_up  p_down.

  DATA : l_sort_ln LIKE LINE OF gt_sort_lvc.

  g_sortpos = g_sortpos + 1.

  l_sort_ln-spos      = g_sortpos.
  l_sort_ln-fieldname = p_fieldname.
  l_sort_ln-up        = p_up.
  l_sort_ln-down      = p_down.
  APPEND l_sort_ln TO gt_sort_lvc.

ENDFORM.                    " ALV_SORTCAT_LVC

*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       ALV VARIANT
*----------------------------------------------------------------------*
FORM variant_init .

  CLEAR st_variant.
  st_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = st_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
  ENDIF.

ENDFORM.                    " VARIANT_INIT

*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTCAT
*&---------------------------------------------------------------------*
*       ALV EVENT
*----------------------------------------------------------------------*
FORM build_eventcat USING p_type.

* P_TYPE = SPACE
* P_TYPE = '1'    ==> DATA_CHANGED
* P_TYPE = '2'    ==> LIST_MODIFY

  DATA: st_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events_lvc.

  READ TABLE gt_events_lvc WITH KEY name = slis_ev_top_of_page
                               INTO st_event.
  IF sy-subrc = 0.
    MOVE g_top_of_page TO st_event-form.
    APPEND st_event TO gt_events_lvc.
  ENDIF.

* [DATA_CHANGED] Event
  IF p_type = '1'.
    MOVE slis_ev_data_changed TO st_event-name.
    MOVE g_data_changed TO st_event-form.
    APPEND st_event TO gt_events_lvc.

* [LIST_MODIFY] Event
  ELSEIF p_type = '2'.
    MOVE slis_ev_list_modify TO st_event-name.
    MOVE g_data_list_chan TO st_event-form.
    APPEND st_event TO gt_events_lvc.
  ENDIF.

ENDFORM.                    " BUILD_EVENTCAT

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       ALV TOP_OF_PAGE SET
*----------------------------------------------------------------------*
FORM top_of_page1.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      gt_list_commentary = gt_header.

ENDFORM.                    " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  ALV_EXEC_DATA_CHANGED
*&---------------------------------------------------------------------*
*   Subroutine 'ALV_DATA_CHANGED' is excuted by compulsion
*            when button is clicked in ALV
*----------------------------------------------------------------------*
FORM alv_exec_data_changed  CHANGING p_valid.

  DATA : l_alv     TYPE REF TO cl_gui_alv_grid,
         l_refresh TYPE char01.

  CLEAR : l_alv, p_valid, l_refresh.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_alv.

  CALL METHOD l_alv->check_changed_data
    IMPORTING
      e_valid   = p_valid
    CHANGING
      c_refresh = l_refresh.

ENDFORM.                    " ALV_EXEC_DATA_CHANGED

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Function 'REUSE_ALV_GRID_DISPLAY' CALL
*----------------------------------------------------------------------*
FORM call_alv_grid_display USING p_tabname.

  DATA : l_tabname TYPE slis_tabname.

  FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

  l_tabname = p_tabname.

  ASSIGN (l_tabname) TO <table>.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_interface_check        = 'ALV_BACKGROUD'
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'G_USER_STATUS'
      i_callback_user_command  = g_user_command
      gt_fieldcat              = gt_fieldcat
      gt_sort                  = gt_sort
      i_save                   = g_save
      is_variant               = st_variant
      i_grid_settings          = st_gridset
      is_layout                = st_layout
      gt_events                = gt_events_lvc[]
    TABLES
      t_outtab                 = <table>
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_GRID_DISPLAY_LVC
*&---------------------------------------------------------------------*
*       Function "REUSE_ALV_GRID_DISPLAY_LVC" CALL
*----------------------------------------------------------------------*
FORM call_alv_grid_display_lvc  USING p_tabname.

  DATA : l_tabname TYPE slis_tabname.

  FIELD-SYMBOLS : <table> TYPE STANDARD TABLE.

  l_tabname = p_tabname.

  ASSIGN (l_tabname) TO <table>.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_interface_check        = 'ALV_BACKGROUD'
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'G_USER_STATUS'
      i_callback_user_command  = g_user_command
      i_grid_settings          = gt_gridset   "st_gridset
      is_layout_lvc            = gs_layout    "st_layout_lvc
      gt_fieldcat_lvc          = gt_fieldcat  "_lvc
      gt_sort_lvc              = gt_sort_lvc  "gt_sortcat_lvc
      i_save                   = g_save
      is_variant               = st_variant
      gt_events                = gt_events_lvc[]
    TABLES
      t_outtab                 = <table>
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_GRID_DISPLAY_LVC

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_HIERSEQ_LIST_DISPLAY
*&---------------------------------------------------------------------*
*       Function "REUSE_ALV_HIERSEQ_LIST_DISPLAY" CALL
*----------------------------------------------------------------------*
FORM call_alv_hierseq_list_display  USING  p_htab      p_itab
                                           p_htabname  p_itabname.


  DATA : l_htab TYPE slis_tabname,
         l_itab TYPE slis_tabname.

  FIELD-SYMBOLS : <htab> TYPE STANDARD TABLE,
                  <itab> TYPE STANDARD TABLE.

  l_htab = p_htab.
  l_itab = p_itab.

  ASSIGN (l_htab) TO <htab>.
  ASSIGN (l_itab) TO <itab>.

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'G_USER_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = st_layout
      gt_fieldcat              = gt_fieldcat[]
      gt_sort                  = gt_sort[]
      i_save                   = g_save
      is_variant               = st_variant
      gt_events                = gt_events_lvc[]
      i_tabname_header         = p_htabname
      i_tabname_item           = p_itabname
      is_keyinfo               = st_keyinfo
    TABLES
      t_outtab_header          = <htab>
      t_outtab_item            = <itab>.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_HIERSEQ_LIST_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY_ERROR
*&---------------------------------------------------------------------*
*       ALV Message Display
*----------------------------------------------------------------------*
FORM alv_display_error  USING  rr_data_changed TYPE REF TO
                                    cl_alv_changed_data_protocol
                               p_row TYPE i
                               p_msgid  p_msgno  p_msgty  p_msgv1
                               p_msgv2  p_msgv3  p_msgv4  p_fieldname.

* ALV Message Display
  CALL METHOD rr_data_changed->add_protocol_entry
    EXPORTING
      i_msgid     = p_msgid
      i_msgno     = p_msgno
      i_msgty     = p_msgty
      i_msgv1     = p_msgv1
      i_msgv2     = p_msgv2
      i_msgv3     = p_msgv3
      i_msgv4     = p_msgv4
      i_fieldname = p_fieldname
      i_row_id    = p_row.   "RS_MOD_CELLS-ROW_ID.

ENDFORM.                    " ALV_DISPLAY_ERROR

*&---------------------------------------------------------------------*
*&      Form  CALL_ICON_CREATE
*&---------------------------------------------------------------------*
*       ICON Display
*----------------------------------------------------------------------*
FORM call_icon_create  USING p_name   p_text
                    CHANGING p_result.

  CLEAR : p_result.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = p_name
      text                  = p_text
    IMPORTING
      RESULT                = p_result
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

ENDFORM.                    " CALL_ICON_CREATE


*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_ALL
*&---------------------------------------------------------------------*
*       All dequeue of LOCK
*----------------------------------------------------------------------*
FORM dequeue_all .

  CALL FUNCTION 'DEQUEUE_ALL'.

ENDFORM.                    " DEQUEUE_ALL

*&---------------------------------------------------------------------*
*&      Form  CALL_POPUP_COMFIRM
*&---------------------------------------------------------------------*
*       Confirmation POP-UP
*&---------------------------------------------------------------------*
FORM call_popup_comfirm  USING    p_text1  p_text2  p_title
                         CHANGING p_answer.

  CLEAR : p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
    EXPORTING
      textline1 = p_text1
      textline2 = p_text2
      titel     = p_title
    IMPORTING
      answer    = p_answer.

ENDFORM.                    " CALL_POPUP_COMFIRM

*&---------------------------------------------------------------------*
*&      Form  CALL_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       Possible Entry
*----------------------------------------------------------------------*
FORM call_value_request  TABLES   pt_tab
                         USING    p_stepl TYPE sy-stepl
                                  p_retfield p_dynprofield.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = p_retfield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = p_dynprofield
      stepl           = p_stepl
      value_org       = 'S'
    TABLES
      value_tab       = pt_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_VALUE_REQUEST
