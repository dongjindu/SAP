*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZIIT_BPML_ENHANCE
*& Program Name   : APM Performance Enhance List I/F
*& Created by     : Victor Park
*& Created on     : 12.04.2013
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     :
*&----------------------------------------------------------------------

REPORT  ziit_bpml_enhance MESSAGE-ID zmpp.

TABLES :  ztitbpma.

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

DATA : it_bpml   LIKE ztitbpma OCCURS 0 WITH HEADER LINE,
       it_tcode  LIKE ztit_apm_tcode OCCURS 0 WITH HEADER LINE,
       wa_tcode  TYPE ztit_apm_tcode,
       it_data   LIKE zsit_apm_enhance OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).



*&----------------------------------------------------------------------
*&    SELECTION-SCREEN
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS  s_year FOR ztitbpma-apmyear
                                    DEFAULT sy-datum+0(4) OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_alv RADIOBUTTON GROUP r1.
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


  IF p_send = 'X'.
    PERFORM pro_send.
  ELSE.
    PERFORM  pro_alv.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.










*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .

ENDFORM.                    " INIT

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  DATA : l_date   TYPE sy-datum,
         l_time   TYPE sy-uzeit.

  DATA : lv_edate TYPE sy-datum,
         lv_date  TYPE sy-datum,
         lv_time  TYPE sy-uzeit.

*-Main Data "Only send data which has Enhance Month
  SELECT * INTO  TABLE it_bpml
  FROM ztitbpma
  WHERE apmyear IN  s_year
    AND ptypea  = 'P'
    AND enh_plandt <> '00000000'.

  SORT it_bpml BY  tcode zseq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_bpml COMPARING tcode.


*-Get the latest T-code List I/F
  SELECT zdate ztime INTO (l_date, l_time)
    FROM ztit_apm_tcode
    UP TO 1 ROWS
    ORDER BY zdate DESCENDING ztime DESCENDING.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'There is NO T-code List I/F'.
    STOP.
  ENDIF.

  SELECT * INTO TABLE it_tcode
  FROM ztit_apm_tcode
  WHERE zdate =  l_date
    AND ztime =  l_time.

ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : l_index TYPE sy-index.
  DATA : i_cnt TYPE i,
         l_line TYPE i.

  CLEAR : it_data[], it_data.

  SORT it_bpml  BY tcode.
  SORT it_tcode BY tcode.

  LOOP AT it_bpml.
    CLEAR : it_tcode.

    it_data-yyyy     = it_bpml-apmyear.

    READ TABLE it_tcode WITH KEY tcode  = it_bpml-tcode
                                         BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_tcode TO it_data.
    ELSE.
*-    when t-code was deleted during processing
      SELECT * INTO wa_tcode
      FROM ztit_apm_tcode
      UP TO 1 ROWS
      WHERE tcode = it_bpml-tcode
      ORDER BY zdate DESCENDING ztime DESCENDING.
      ENDSELECT.
      IF wa_tcode IS NOT INITIAL.
        MOVE-CORRESPONDING wa_tcode TO it_data.
      ELSE.
        MESSAGE s000 WITH 'T-code information is missing'
                          ' in T-code List'.
        STOP.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING it_bpml TO it_data.

    it_data-resp_tm     = it_bpml-respmt.
    it_data-server_tm   = it_bpml-cpumt + it_bpml-dbmt.
    it_data-expect_month   = it_bpml-enh_plandt+0(6).
    it_data-user_nm        = it_bpml-consultant.

    IF it_bpml-cstatus >= '10' AND it_bpml-cstatus <= '30'.
      it_data-appr_stat = 'PA'.
    ELSEif it_bpml-cstatus > '30'.
      it_data-appr_stat = 'PC'.
    ENDIF.

    APPEND it_data.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_SEND
*&---------------------------------------------------------------------*
FORM pro_send .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.

  CHECK it_data[] IS NOT INITIAL.

  CALL FUNCTION 'ZFIT_OB_BPML04' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S'.   "Success
    PERFORM save_log USING e_return-type 'Success' ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log USING e_return-type 'Error'  l_msgtxt.
    MESSAGE e000 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_SEND
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .
  g_repid  = sy-repid.


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
FORM layout_build   USING p_layout TYPE slis_layout_alv.

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

  CLEAR: gs_sort, p_sort[].

  gs_sort-spos      = '1'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'ID'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat   TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
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
      WHEN 'ID'.
        pt_fieldcat-seltext_m    = 'Seq.'.
      WHEN 'YYYY'.
        pt_fieldcat-seltext_m    = 'Year'.
      WHEN 'SYS_CD'.
        pt_fieldcat-seltext_m    = 'Sys.Code.'.
      WHEN 'TCODE'.
        pt_fieldcat-seltext_m    = 'T-code'.
      WHEN 'TCODE_NM'.
        pt_fieldcat-seltext_m    = 'T-code Name'.
      WHEN 'USER_NM'.
        pt_fieldcat-seltext_m    = 'User'.
      WHEN 'RESP_TM'.
        pt_fieldcat-seltext_m    = 'Response time'.
      WHEN 'SERVER_TM'.
        pt_fieldcat-seltext_m    = 'Server time'.
      WHEN 'NETWORK_TM'.
        pt_fieldcat-seltext_m    = 'Network time'.
      WHEN 'MENU_NAME1'.
        pt_fieldcat-seltext_m    = 'Menu1'.
      WHEN 'MENU_NAME2'.
        pt_fieldcat-seltext_m    = 'Menu2'.
      WHEN 'MENU_NAME3'.
        pt_fieldcat-seltext_m    = 'Menu3'.
      WHEN 'PRESULT'.
        pt_fieldcat-seltext_m    = 'Analysis Result(Plan)'.
      WHEN 'RESULT_COMMENT'.
        pt_fieldcat-seltext_m    = 'Analysis Result Comment'.
      WHEN 'CDATE'.
        pt_fieldcat-seltext_m    = 'Creat.Dt'.
      WHEN 'EXPECT_MONTH'.
        pt_fieldcat-seltext_m    = 'Expecting Month'.
      WHEN 'APPR_STAT'.
        pt_fieldcat-seltext_m    = 'Process Stat.'.
      WHEN 'CLOSEDT'.
        pt_fieldcat-seltext_m    = 'Completed Dt'.
      WHEN 'CLOSE_COMMENT'.
        pt_fieldcat-seltext_m    = 'Complete Comment'.
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
  DATA : l_line(5) TYPE n.
  DATA : h_title(30), s_title(60),  a_title(60).

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Year : '  s_year-low '~' s_year-high INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  DESCRIBE TABLE it_data LINES l_line.
  ls_title-typ = 'A'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Total Count : '  l_line INTO ls_title-info.
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
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING   p_type p_msg1 p_msg2.

  DATA : it_save LIKE ztit_apm_enhance OCCURS 0 WITH HEADER LINE.
  DATA : l_zseq(10) TYPE n.

  SELECT zseq INTO l_zseq
    FROM ztit_apm_enhance
      UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-type = p_type.
    IF p_type = 'E'.
      IF NOT  p_msg1 IS INITIAL.
        it_save-message  = p_msg1.
      ELSE.
        it_save-message  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.
  ENDLOOP.

  INSERT ztit_apm_enhance FROM TABLE it_save.

  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_LOG
