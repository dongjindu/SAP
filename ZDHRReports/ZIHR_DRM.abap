*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZIHR_DRM
*& Program Name   : HR DRM Interface
*& Created by     : Victor Park
*& Created on     : 08.30.2013
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     :  ZSHR_DRM_TM, ZSHR_DRM_GROUP, ZSHR_DRM_JOB
*&----------------------------------------------------------------------

REPORT zihr_drm MESSAGE-ID zmpp.

TABLES : pa0001.

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

DATA : BEGIN OF it_tm_tmp OCCURS 0,
         pernr  LIKE pa0001-pernr,
         ename  LIKE zshr_drm_tm-ename  ,
         orgeh  LIKE zshr_drm_tm-orgeh  ,
         stell  LIKE zshr_drm_tm-stell  ,
         comp_id   LIKE zshr_drm_tm-comp_id  ,
         comp_type LIKE zshr_drm_tm-comp_type  ,
         comp_name LIKE zshr_drm_tm-comp_name  ,
         name_eng  LIKE zshr_drm_tm-name_eng  ,
         nachn LIKE pa0002-nachn,
         vorna LIKE pa0002-vorna.
DATA : END OF it_tm_tmp.

DATA : it_tm LIKE zshr_drm_tm OCCURS 0 WITH HEADER LINE,
       it_group LIKE zshr_drm_group OCCURS 0 WITH HEADER LINE,
       it_job   LIKE zshr_drm_job   OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_list OCCURS 0,
         a(5),
         b(10),
         c(20),
         d(20),
         e(20),
       END OF it_list.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_pernr FOR  pa0001-pernr.
SELECTION-SCREEN SKIP.
PARAMETERS :     p_date TYPE datum DEFAULT sy-datum  OBLIGATORY  .
SELECTION-SCREEN END OF BLOCK b1.

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
  CLEAR : it_tm[].

*-TM
  SELECT a~pernr b~orgeh b~stell c~nachn c~vorna
    INTO CORRESPONDING FIELDS OF TABLE it_tm_tmp
  FROM pa0000 AS a INNER JOIN pa0001 AS b
                    ON a~pernr  = b~pernr
                   INNER JOIN pa0002 AS c
                    ON a~pernr  = c~pernr
  WHERE a~endda = '99991231'
    AND a~stat2 <> '0'
    AND a~begda <= p_date
    AND a~endda >= p_date
    AND a~endda = '99991231'
    AND b~begda <= p_date
    AND b~endda >= p_date
    AND a~pernr IN s_pernr
    AND c~endda = '99991231'.

  LOOP AT it_tm_tmp.
    MOVE-CORRESPONDING it_tm_tmp TO it_tm.
    it_tm-pernr     = it_tm_tmp-pernr+2(6).
    it_tm-comp_id   = 'H201'.
    it_tm-comp_type = 'A1'.
    it_tm-comp_name = 'HMMA'.
    CONCATENATE it_tm_tmp-vorna it_tm_tmp-nachn INTO it_tm-ename
                                                 SEPARATED BY space.
    it_tm-name_eng  = it_tm-ename.
    APPEND it_tm.
  ENDLOOP.

*-Group
  SELECT a~objid  a~stext b~sobid
    INTO CORRESPONDING FIELDS OF TABLE it_group
  FROM hrp1000 AS a INNER JOIN hrp1001 AS b
          ON a~objid  = b~objid
  WHERE a~otype   = 'O'
    AND a~endda   = '99991231'
    AND a~langu   = sy-langu
    AND a~plvar   = '01'
    AND b~otype   = 'O'
    AND b~endda   = '99991231'
    AND b~plvar   = '01'
    AND b~sclas   = 'O'
    AND b~subty   = 'A002'.

*-<ADD President & CEO manually
  CLEAR : it_group.
  it_group-objid = '90001922'.
  it_group-sobid  = 'H201'.

  SELECT SINGLE  a~stext INTO it_group-stext
  FROM hrp1000 AS a
  WHERE a~objid   = it_group-objid
    AND a~otype   = 'O'
    AND a~endda   = '99991231'
    AND a~langu   = sy-langu
    AND a~plvar   = '01'.

  APPEND it_group.
*->

  LOOP AT it_group.
    it_group-comp_id = 'H201'.
    it_group-comp_type = 'A1'.
    it_group-comp_name = 'HMMA'.
    MODIFY it_group.
  ENDLOOP.

*-Job
  SELECT a~objid  a~stext
    INTO CORRESPONDING FIELDS OF TABLE it_job
  FROM hrp1000 AS a
  WHERE a~otype   = 'C'
    AND a~endda   = '99991231'
    AND a~langu   = sy-langu
    AND a~plvar   = '01'.

  LOOP AT it_job.
    it_job-comp_id = 'H201'.
    it_job-comp_type = 'A1'.
    it_job-comp_name = 'HMMA'.
    MODIFY it_job.
  ENDLOOP.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .

  IF it_tm[] IS INITIAL.
    MESSAGE s000 WITH 'There is No I/F data'.
    STOP.
  ENDIF.

  LOOP AT it_tm.
    it_list-a = 'TM'.
    it_list-b = it_tm-pernr.
    it_list-c = it_tm-ename.
    it_list-d = it_tm-orgeh.
    it_list-e = it_tm-stell.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.

  LOOP AT it_group.
    it_list-a = 'GROUP'.
    it_list-b = it_group-objid.
    it_list-c = it_group-stext.
    it_list-d = it_group-sobid.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.

  LOOP AT it_job.
    it_list-a = 'JOB'.
    it_list-b = it_job-objid.
    it_list-c = it_job-stext.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.
  DATA : l_lines TYPE i.

  CLEAR : e_return.

  CHECK NOT it_tm[] IS INITIAL.
  DESCRIBE TABLE it_tm LINES l_lines.
  CALL FUNCTION 'ZFHR_DRM_TMINFO' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_tm                  = it_tm
      t_group               = it_group
      t_job                 = it_job
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S'.   "Success
    MESSAGE s000 WITH 'Interface : Success. Records:' l_lines.
  ELSE.
    MESSAGE e000 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH


*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_LIST'.

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
      t_outtab                 = it_list[]
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
      WHEN 'A'.
        pt_fieldcat-seltext_m    = 'TYPE'.
*      WHEN 'ZEDAT'.
*        pt_fieldcat-seltext_m    = 'Batch Date'.
*      WHEN 'TYPE'.
*        pt_fieldcat-seltext_m    = 'Result'.
*      WHEN 'MESSAGE'.
*        pt_fieldcat-seltext_m    = 'Message'.
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

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  ls_title-typ  = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Key Date: ' p_date INTO ls_title-info.
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
