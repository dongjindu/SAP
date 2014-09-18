*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZMMR_VAATZ_PENDING
*& Program Name   : Vattz pending PRs
*& Created by     : Victor Park
*& Created on     : 01.21.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     :
*&----------------------------------------------------------------------
*& Desc.     : price : per 1 unit
*&
*&----------------------------------------------------------------------

REPORT zmmr_vaatz_pending MESSAGE-ID zmpp.

TABLES : ztmm_vaz_if001, eban.

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


DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztmm_vaz_if001.
DATA : chk(1).
DATA : END OF it_data.

DATA : BEGIN OF it_zflag OCCURS 0,
        banfn LIKE ztmm_vaz_if001-banfn,
        bnfpo LIKE ztmm_vaz_if001-bnfpo,
        zflag LIKE ztmm_vaz_if001-zflag,
      END OF it_zflag.

DATA : BEGIN OF it_type OCCURS 0,
        banfn LIKE ztmm_vaz_if001-banfn,
        bnfpo LIKE ztmm_vaz_if001-bnfpo,
        type  LIKE ztmm_vaz_if001-type,
      END OF it_type.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_prdt FOR  ztmm_vaz_if001-erdat  OBLIGATORY,
                 s_prno FOR  ztmm_vaz_if001-banfn,
                 s_name FOR eban-afnam NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.


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
  PERFORM modify_screen.

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
  PERFORM pro_alv.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM ztmm_vaz_if001 AS a
  WHERE a~erdat IN s_prdt
    AND a~banfn IN s_prno
    AND a~afnam IN s_name.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  LOOP AT it_data.
    IF it_data-zflag = 'D'.
      MOVE-CORRESPONDING it_data TO it_zflag.
      COLLECT it_zflag. CLEAR it_zflag.
    ENDIF.
    IF it_data-type = 'S'.
      MOVE-CORRESPONDING it_data TO it_type.
      COLLECT it_type.  CLEAR it_type.
    ENDIF.
  ENDLOOP.

  SORT it_zflag BY banfn bnfpo.
  SORT it_type  BY banfn bnfpo.

  LOOP AT it_zflag.
    DELETE it_data WHERE banfn = it_zflag-banfn
                     AND bnfpo = it_zflag-bnfpo.
  ENDLOOP.
  LOOP AT it_type.
    DELETE it_data WHERE banfn = it_type-banfn
                     AND bnfpo = it_type-bnfpo.
  ENDLOOP.

  SORT it_data  BY banfn bnfpo tran_date DESCENDING
                               tran_time DESCENDING.

  DELETE ADJACENT DUPLICATES FROM it_data COMPARING banfn bnfpo.

ENDFORM.                    " MODIFY_DATA

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
  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
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

  CLEAR: gs_sort, p_sort[].

  gs_sort-spos      = '1'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'BANFN'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'BNFPO'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

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
      WHEN 'BANFN'.
        pt_fieldcat-seltext_m    = 'PR No.'.
        pt_fieldcat-col_pos      = 1.
        pt_fieldcat-hotspot      = 'X'.
      WHEN 'BNFPO'.
        pt_fieldcat-seltext_m    = 'L/N'.
        pt_fieldcat-col_pos      = 2.
      WHEN 'MATNR'.
        pt_fieldcat-seltext_m    = 'Material'.
        pt_fieldcat-col_pos      = 3.
      WHEN 'TXZ01'.
        pt_fieldcat-seltext_m    = 'Desription'.
        pt_fieldcat-col_pos      = 4.
      WHEN 'MENGE'.
        pt_fieldcat-seltext_m    = 'Quantity'.
        pt_fieldcat-col_pos      = 5.
      WHEN 'UNIT'.
        pt_fieldcat-seltext_m    = 'Unit'.
        pt_fieldcat-col_pos      = 6.
      WHEN 'LFDAT'.
        pt_fieldcat-seltext_m    = 'Del.Date'.
        pt_fieldcat-col_pos      = 7.
      WHEN 'ERDAT'.
        pt_fieldcat-seltext_m    = 'Creation Date'.
        pt_fieldcat-col_pos      = 8.
      WHEN 'TYPE'.
        pt_fieldcat-seltext_m    = 'I/R'.
        pt_fieldcat-col_pos      = 9.
      WHEN 'ERDAT'.
        pt_fieldcat-seltext_m    = 'Creation Date'.
        pt_fieldcat-col_pos      = 10.
      WHEN 'AFNAM'.
        pt_fieldcat-seltext_m    = 'Requistioner'.
        pt_fieldcat-col_pos      = 11.
      WHEN 'MESSAGE'.
        pt_fieldcat-seltext_m    = 'Message'.
        pt_fieldcat-col_pos      = 12.
      WHEN OTHERS.
        pt_fieldcat-no_out       = 'X'.

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

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*PR Creation Date : '  s_prdt-low '~' s_prdt-high
                             INTO ls_title-info SEPARATED BY space.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*PR Number : '  s_prno-low '~' s_prno-high
                             INTO ls_title-info SEPARATED BY space.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE '*Requisitioner : ' s_name-low   INTO ls_title-info.
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
    WHEN '&IC1'.
      IF p_selfield-fieldname = 'BANFN'
                  AND p_selfield-value IS NOT INITIAL.
        SET PARAMETER ID 'BAN' FIELD  p_selfield-value.
        CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

*  p_selfield-refresh = 'X'.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .
  g_repid  = sy-repid.

  s_prdt-low     = sy-datum - 5.
  s_prdt-high    = sy-datum.
  s_prdt-sign   = 'I'.
  s_prdt-option = 'BT'.
  APPEND  s_prdt.

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
FORM cal_month_lastdate  USING    p_spmon
                                  p_edate.
  DATA : lv_date TYPE sy-datum.
  CONCATENATE p_spmon '01' INTO lv_date.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_date
    IMPORTING
      last_day_of_month = p_edate
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
FORM progress_bar  USING    p_tabix
                            p_lines.
  DATA : lv_per TYPE i .
  DATA : lv_text(50).
  DATA : lv_mode TYPE i.
  DATA : lv_lines(7) TYPE n,
         lv_tabix(7)  TYPE n.

  lv_lines  = p_lines.
  lv_tabix  = p_tabix.

  lv_per = ( p_tabix * 100 / p_lines ) .
  CONCATENATE 'Processing : ' lv_tabix ' / ' lv_lines INTO lv_text.
  lv_mode =  lv_per MOD 5.
  IF lv_mode   =  0.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_per
        text       = lv_text.
  ENDIF.
ENDFORM.                    " PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  GET_COLOR_FROM_OD
*&---------------------------------------------------------------------*
FORM get_color_from_od  USING    p_knobj
                                 p_idnrk
                                 p_edate
                                 p_intc
                                 p_chk .

  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.
  DATA: lt_cukb LIKE cukb OCCURS 0 WITH HEADER LINE.
  DATA : BEGIN OF it_ktab OCCURS 0,
          text(100),
         END OF it_ktab.

  SELECT * INTO TABLE lt_cuob
    FROM cuob
   WHERE kntab =  'STPO'
     AND knobj =  p_knobj
     AND datuv <= p_edate.

  CHECK lt_cuob[] IS NOT INITIAL.

  SELECT * INTO TABLE lt_cukb
    FROM cukb
     FOR ALL ENTRIES IN lt_cuob
   WHERE knnum =  lt_cuob-knnum
     AND adzhl =  lt_cuob-adzhl
     AND datuv <= p_edate.

  LOOP AT lt_cukb.
    CALL FUNCTION 'CUKD_GET_KNOWLEDGE'
      EXPORTING
        knowledge_type     = 'S'
        relation           = lt_cukb-knnam
*       RELATION_NR        = ' '
        date               = sy-datum
      TABLES
        knowledge_tab      = it_ktab
      EXCEPTIONS
        no_knowledge_found = 1
        no_relation_found  = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    ELSE.
      READ TABLE it_ktab INDEX 1.
      SHIFT it_ktab UP TO `'` LEFT.
      REPLACE `'` WITH '' INTO it_ktab.
      REPLACE `'` WITH '' INTO it_ktab.
      CONDENSE it_ktab.

      IF it_ktab = p_intc.
        p_chk = 'X'.
      ELSE.
        p_chk = ''.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_COLOR_FROM_OD
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-name = 'S_NAME-HIGH'.
      screen-invisible  = 1.
      screen-active     = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
