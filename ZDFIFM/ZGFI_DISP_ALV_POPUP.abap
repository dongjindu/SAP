FUNCTION ZGFI_DISP_ALV_POPUP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_GRID_TITLE) TYPE  LVC_TITLE OPTIONAL
*"     REFERENCE(I_STRUCTURE_NAME) TYPE  DD02L-TABNAME OPTIONAL
*"     REFERENCE(I_SCREEN_START_COLUMN) DEFAULT 10
*"     REFERENCE(I_SCREEN_START_LINE) DEFAULT 5
*"     REFERENCE(I_SCREEN_END_COLUMN) DEFAULT 110
*"     REFERENCE(I_SCREEN_END_LINE) DEFAULT 20
*"     REFERENCE(I_FLG) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_ALVTAB OPTIONAL
*"      IM_HEADER TYPE  ZDFI_ALV_HEADER OPTIONAL
*"----------------------------------------------------------------------

  TYPE-POOLS: slis.
  DATA: l_flg TYPE c LENGTH 1.
  DATA : w_index(2)       TYPE n,
         w_lines          LIKE sy-tabix,
         w_subrc          LIKE sy-subrc,
         l_slis_repid     LIKE sy-repid,
         l_variant        LIKE disvariant,
         lt_slis_fieldcat TYPE slis_t_fieldcat_alv,
         lt_slis_field    TYPE slis_t_fieldcat_alv,
         lt_event         TYPE slis_t_event,
         lt_fieldcat      TYPE lvc_t_fcat,
         ls_slis_fieldcat  TYPE slis_fieldcat_alv,
         ls_events        TYPE slis_alv_event,
         ls_slis_layout    TYPE slis_layout_alv,
         ls_fieldcat       TYPE lvc_s_fcat.    "TITLE

  CLEAR l_flg.
  l_flg = i_flg.

  l_slis_repid = sy-repid.
  ls_slis_layout-colwidth_optimize = 'X'.
  ls_slis_layout-zebra             = 'X'.
  ls_slis_layout-cell_merge        = 'X'.

  it_zctco_alv_header[] = im_header[].

  CLEAR : lt_fieldcat[], lt_slis_fieldcat[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = i_structure_name
    CHANGING
      ct_fieldcat      = lt_fieldcat[].

  LOOP AT lt_fieldcat INTO ls_fieldcat.
    MOVE-CORRESPONDING ls_fieldcat TO ls_slis_fieldcat.

    ls_slis_fieldcat-seltext_m      = ls_fieldcat-scrtext_m.
    ls_slis_fieldcat-ddic_outputlen = ls_fieldcat-dd_outlen.
    ls_slis_fieldcat-ref_tabname    = ls_fieldcat-ref_table.
    ls_slis_fieldcat-ref_fieldname  = ls_fieldcat-fieldname.

    APPEND ls_slis_fieldcat TO lt_slis_fieldcat.
    CLEAR ls_slis_fieldcat.
  ENDLOOP.

  IF im_header[] IS INITIAL.
  ELSE.

    ls_events-name = 'TOP_OF_PAGE'.
    ls_events-form = 'TOP_OF_PAGE'.
    APPEND ls_events TO lt_event.
  ENDIF.

  IF l_flg EQ 'Y'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = l_slis_repid
        i_structure_name         = i_structure_name
        i_grid_title             = i_grid_title
        is_layout                = ls_slis_layout
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        is_variant               = l_variant
        it_events                = lt_event
        it_fieldcat              = lt_slis_fieldcat[]
        i_screen_start_column    = i_screen_start_column
        i_screen_start_line      = i_screen_start_line
        i_screen_end_column      = i_screen_end_column
        i_screen_end_line        = i_screen_end_line
      TABLES
        t_outtab                 = t_alvtab.

  ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program    = l_slis_repid
        i_structure_name      = i_structure_name
        i_grid_title          = i_grid_title
        is_layout             = ls_slis_layout
        is_variant            = l_variant
        it_events             = lt_event
        it_fieldcat           = lt_slis_fieldcat[]
        i_screen_start_column = i_screen_start_column
        i_screen_start_line   = i_screen_start_line
        i_screen_end_column   = i_screen_end_column
        i_screen_end_line     = i_screen_end_line
      TABLES
        t_outtab              = t_alvtab.
  ENDIF.



ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.

  DATA : ls_zctco_alv_header TYPE ZDSFI_ALV_HEADER.
  DATA : lt_listheader     TYPE slis_t_listheader.    "TITLE

  DATA: ls_line TYPE slis_listheader,
        l_date(50) TYPE c,
        l_date_mask TYPE c LENGTH 10,
        l_time_mask TYPE c LENGTH 8.

  REFRESH lt_listheader.
  CLEAR ls_line.

  LOOP AT it_zctco_alv_header INTO ls_zctco_alv_header.

    CLEAR ls_line.
    ls_line-typ  = 'S'.
    ls_line-key  = ls_zctco_alv_header-zkey.
    ls_line-info = ls_zctco_alv_header-zinfo.
    APPEND ls_line TO lt_listheader.

  ENDLOOP.

* Disp Title to ALV
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_listheader.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  alv_pf_status
*&---------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'STATUS' EXCLUDING rt_extab.

ENDFORM.                    " alv_pf_status
*&---------------------------------------------------------------------*
*&      Form  alv_pf_status
*&---------------------------------------------------------------------*
FORM user_command using r_ucomm ls_selfield.

  IF ( r_ucomm = 'CANC' OR r_ucomm = 'ENTR' OR
       r_ucomm = 'BACK' OR r_ucomm = 'EXIT' ).
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " alv_pf_status
