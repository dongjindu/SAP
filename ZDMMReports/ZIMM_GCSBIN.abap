*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZIMM_GCSBIN
*& Program Name   : Bin Master Interface to GCS
*& Created by     : Victor Park
*& Created on     : 08.30.2013
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_MM_IF_OB_02_010
*& Stru.     : ZTMM_GCSBIN, ZSMM_GCSBIN
*&----------------------------------------------------------------------

REPORT zimm_gcsbin MESSAGE-ID zmpp.

TABLES : pkhd.

*- ALV
TYPE-POOLS: slis.
DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gs_layout           TYPE slis_layout_alv,
      gs_sort             TYPE slis_sortinfo_alv,
      gt_sort             TYPE slis_t_sortinfo_alv,
      gs_light            TYPE lvc_s_layo,
      gs_print            TYPE slis_print_alv,
      gt_sp_group         TYPE slis_t_sp_group_alv,
      gt_events           TYPE slis_t_event,
      gs_events           LIKE  LINE OF gt_events,
      g_save              VALUE 'A',
      gx_variant          LIKE disvariant,
      g_variant           LIKE disvariant.

DATA : ls_title         TYPE slis_listheader, "alv header
       alv_t_listheader TYPE slis_t_listheader.

DATA : g_extab          TYPE slis_t_extab,
       g_extab_ln       LIKE   LINE  OF  g_extab.

DATA : g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : t_colinfo_table TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA : g_repid         LIKE sy-repid.


DATA : it_data LIKE zsmm_gcsbin OCCURS 0 WITH HEADER LINE.
DATA : it_log  LIKE zsmm_gcsbin OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK  b2 WITH FRAME TITLE text-b02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_alv  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM modify_data.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF p_send = 'X'.
    PERFORM  pro_batch.
  ELSE.
    PERFORM pro_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .

  SELECT matnr  werks  ablad
    INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM pkhd
  WHERE rksta = 'I'
    AND prvbe  IN ('DL-ONEKIT', 'DR-ONEKIT', 'T2-ONEKIT', 'T3-ONEKIT').

  SELECT * INTO TABLE it_log
  FROM  ztmm_gcsbin
  WHERE type  = 'S'.

  SORT it_data BY matnr  werks  ablad.
  SORT it_log  BY matnr  werks  ablad.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .

  LOOP AT it_data.
    CLEAR : it_log.
    READ TABLE it_log WITH KEY matnr  = it_data-matnr
                               werks  = it_data-werks
                               ablad  = it_data-ablad
                           BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE it_data.
    ELSE.
      it_data-zedat = sy-datum.
      MODIFY it_data.
    ENDIF.
  ENDLOOP.

  IF it_data[] IS INITIAL.
    MESSAGE s000 WITH 'There is No I/F data'.
    STOP.
  ENDIF.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
*
  DATA : v_dest(30) VALUE 'WMPM01'.   "Interface Destination.
  DATA : l_lines TYPE i.

  CLEAR : e_return.

  CHECK NOT it_data[] IS INITIAL.
  DESCRIBE TABLE it_data LINES l_lines.
  CALL FUNCTION 'Z_MM_IF_OB_02_010' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S'.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success. Records:' l_lines.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE e000 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.

  LOOP AT it_data.
    IF p_type = 'E'.
      it_data-type  = p_type.
      IF NOT  p_msg1 IS INITIAL.
        it_data-message  = p_msg1.
      ELSE.
        it_data-message  = p_msg2.
      ENDIF.
    ELSE.
      it_data-type  = p_type.
    ENDIF.
    MODIFY it_data.
  ENDLOOP.

  MODIFY ztmm_gcsbin FROM TABLE it_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE e000 WITH 'I/F data Sent, but was not saved in ztmm_gcsbin'
                      'successfully'.
  ENDIF.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_DATA'.

  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
*     i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
*     is_variant               = g_variant
      it_events                = gt_events[]
    TABLES
      t_outtab                 = it_data[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
*  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
*  p_layout-coltab_fieldname = 'COL_COLOR'. "color field of itabe
*  p_layout-cell_merge        = 'X'.
*  p_layout-detail_popup      = 'X'.
*  p_layout-detail_titlebar   = sy-title.
*  p_layout-no_subtotals      = ''.

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SORTTAB_BUILD
*&---------------------------------------------------------------------*
FORM sorttab_build  USING   p_sort TYPE slis_t_sortinfo_alv.

*  CLEAR: gs_sort, p_sort[].
*
*  gs_sort-spos      = '1'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'MATNR'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO p_sort.
*
*  gs_sort-spos      = '2'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'WERKS'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO p_sort.

ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
               USING    p_name      TYPE  slis_tabname.

  DATA: l_datum(08).

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
*     i_structure_name   = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
      WHEN 'MATNR'.
        pt_fieldcat-seltext_m    = 'Material'.
      WHEN 'ZEDAT'.
        pt_fieldcat-seltext_m    = 'Batch Date'.
      WHEN 'TYPE'.
        pt_fieldcat-seltext_m    = 'Result'.
      WHEN 'MESSAGE'.
        pt_fieldcat-seltext_m    = 'Message'.
      WHEN OTHERS.

    ENDCASE.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    MODIFY pt_fieldcat.
  ENDLOOP.

ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.
  DATA : h_title(30), s_title(60),  a_title(60).
  DATA : lv_lines(5) TYPE n.

  DESCRIBE TABLE it_data LINES lv_lines.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  ls_title-typ  = 'H'. "(H:Header, S:Selection, A:Action)
  ls_title-info = 'BIN Master Interface to GCS'.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ  = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE 'Records : '  lv_lines INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.
ENDFORM.                    " LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
FORM append_alv_event  CHANGING p_alv_event TYPE slis_t_event.
* TOP-OF-PAGE Event

  DATA ls_events TYPE slis_alv_event.
  ls_events-name  =  'TOP_OF_PAGE'.
  ls_events-form  =  'TOP_OF_PAGE'.
  APPEND ls_events TO p_alv_event.

ENDFORM.                    " APPEND_ALV_EVENT

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = alv_t_listheader.

ENDFORM. " TOP_OF_PAGE

*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.

  ENDCASE.

  p_selfield-refresh = 'X'.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .

  g_repid  = sy-repid.

ENDFORM.                    " INIT
