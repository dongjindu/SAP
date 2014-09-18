*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZRPP_PRDLOG_DATE_CHECKING
*& Program Name   : Production Log Shop Date Checking
*& Created by     : Victor Park
*& Created on     : 02.13.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     :
*&----------------------------------------------------------------------
*& Desc.     :
*&
*&----------------------------------------------------------------------

REPORT zrpp_prdlog_date_checking MESSAGE-ID zmpp.

TABLES : ztppvr, ztppvr_bk.

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
        INCLUDE STRUCTURE  ztppvr.
DATA : END OF it_data.

DATA : BEGIN OF it_display OCCURS 0.
        INCLUDE STRUCTURE  ztppvr.
DATA :  chk(1).
DATA : END OF it_display.

*-To send by e-mail
DATA : it_display_dup LIKE it_display OCCURS 0 WITH HEADER LINE.

DATA : l_cnt(5)   TYPE n,
       l_cnt_dup(5) TYPE n.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).


*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_zedat FOR  ztppvr-zedat  OBLIGATORY.
PARAMETERS     : p_zetim LIKE ztppvr-zetim,
                 p_tol(1) DEFAULT 1,
                 p_mail AS CHECKBOX.
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
  PERFORM save_ztppvr_chk.
  IF p_mail = 'X' AND it_display_dup[] IS NOT INITIAL.
    PERFORM send_error_email.
  ENDIF.

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
  FROM ztppvr AS a
  WHERE a~zedat IN s_zedat.

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_data
  FROM ztppvr_bk AS a
  WHERE a~zedat IN s_zedat.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : l_gap_day TYPE i,
         l_target_date TYPE sy-datum,
         it_check_log LIKE ztppvr_chk OCCURS 0 WITH HEADER LINE.

  CLEAR : it_display[], it_display.
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_display.

    IF it_data-zetim < p_zetim.
      l_target_date = it_data-zedat - 1.
    ELSE.
      l_target_date = it_data-zedat.
    ENDIF.

    l_gap_day = l_target_date - it_data-k04pdat.
    IF l_gap_day > p_tol.
      APPEND it_display.
    ELSEIF l_target_date+0(6) <> it_data-k04pdat+0(6).
      APPEND it_display.
    ENDIF.
  ENDLOOP.

*-delete already sent information
  CHECK it_display[] IS NOT INITIAL.

  it_display_dup[]  = it_display[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_check_log
  FROM ztppvr_chk
    FOR ALL ENTRIES IN it_display
  WHERE flag           = it_display-flag
    AND p_status       = it_display-p_status
    AND p_rp_serial    = it_display-p_rp_serial
    AND p_model        = it_display-p_model
    AND p_body_serial  = it_display-p_body_serial .

  CHECK it_check_log[] IS NOT INITIAL.
  SORT it_check_log BY flag p_status p_rp_serial p_model p_body_serial.

  LOOP AT it_display_dup.
    READ TABLE it_check_log WITH KEY flag = it_display_dup-flag
                           p_status = it_display_dup-p_status
                           p_rp_serial = it_display_dup-p_rp_serial
                           p_model     = it_display_dup-p_model
                           p_body_serial = it_display_dup-p_body_serial
                           BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE it_display_dup.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .


  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_DISPLAY'.

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
      t_outtab                 = it_display[]
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
  gs_sort-tabname   = 'IT_DISPLAY'.
  gs_sort-fieldname = 'P_MODEL'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_DISPLAY'.
  gs_sort-fieldname = 'P_BODY_SERIAL'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '3'.
  gs_sort-tabname   = 'IT_DISPLAY'.
  gs_sort-fieldname = 'FLAG'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '4'.
  gs_sort-tabname   = 'IT_DISPLAY'.
  gs_sort-fieldname = 'ZEDAT'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '5'.
  gs_sort-tabname   = 'IT_DISPLAY'.
  gs_sort-fieldname = 'ZETIM'.
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

  DATA : tmp_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
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

  tmp_fieldcat[] = pt_fieldcat[].
  CLEAR : pt_fieldcat[], pt_fieldcat.

  LOOP AT   tmp_fieldcat.
    MOVE-CORRESPONDING tmp_fieldcat TO pt_fieldcat.

    CASE tmp_fieldcat-fieldname.
      WHEN 'P_MODEL'.
        pt_fieldcat-seltext_m    = 'Model'.
        pt_fieldcat-col_pos      = 1.
      WHEN 'P_BODY_SERIAL'.
        pt_fieldcat-seltext_m    = 'Body No'.
        pt_fieldcat-col_pos      = 2.
      WHEN 'FLAG'.
        pt_fieldcat-seltext_m    = 'Flag'.
        pt_fieldcat-col_pos      = 3.
      WHEN 'P_STATUS'.
        pt_fieldcat-seltext_m    = 'TP ID'.
        pt_fieldcat-col_pos      = 4.
      WHEN 'ZEDAT'.
        pt_fieldcat-seltext_m    = 'I/F Date'.
        pt_fieldcat-col_pos      = 5.
      WHEN 'ZETIM'.
        pt_fieldcat-seltext_m    = 'I/F Time'.
        pt_fieldcat-col_pos      = 6.
      WHEN 'K04PDAT'.
        pt_fieldcat-seltext_m    = 'Shop Date'.
        pt_fieldcat-col_pos      = 7.
      WHEN 'P_RP_ACTUAL_DATE'.
        pt_fieldcat-seltext_m    = 'Actual Date'.
        pt_fieldcat-col_pos      = 8.
      WHEN 'P_RP_ACTUAL_TIME'.
        pt_fieldcat-seltext_m    = 'Actual Time'.
        pt_fieldcat-col_pos      = 9.
      WHEN 'P_WORK_ORDER'.
        pt_fieldcat-seltext_m    = 'W/Order'.
        pt_fieldcat-col_pos      = 10.
      WHEN 'P_EXT_COLOR'.
        pt_fieldcat-seltext_m    = 'Ext'.
        pt_fieldcat-col_pos      = 11.
      WHEN 'P_INT_COLOR'.
        pt_fieldcat-seltext_m    = 'Int'.
        pt_fieldcat-col_pos      = 12.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    APPEND pt_fieldcat.
  ENDLOOP.

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
*      WHEN 'BANFN'.
*        pt_fieldcat-seltext_m    = 'PR No.'.
*        pt_fieldcat-col_pos      = 1.
*        pt_fieldcat-hotspot      = 'X'.
*      WHEN 'BNFPO'.
*        pt_fieldcat-seltext_m    = 'L/N'.
*        pt_fieldcat-col_pos      = 2.
*      WHEN 'MATNR'.
*        pt_fieldcat-seltext_m    = 'Material'.
*        pt_fieldcat-col_pos      = 3.
*      WHEN 'TXZ01'.
*        pt_fieldcat-seltext_m    = 'Desription'.
*        pt_fieldcat-col_pos      = 4.
*      WHEN 'MENGE'.
*        pt_fieldcat-seltext_m    = 'Quantity'.
*        pt_fieldcat-col_pos      = 5.
*      WHEN 'UNIT'.
*        pt_fieldcat-seltext_m    = 'Unit'.
*        pt_fieldcat-col_pos      = 6.
*      WHEN 'LFDAT'.
*        pt_fieldcat-seltext_m    = 'Del.Date'.
*        pt_fieldcat-col_pos      = 7.
*      WHEN 'ERDAT'.
*        pt_fieldcat-seltext_m    = 'Creation Date'.
*        pt_fieldcat-col_pos      = 8.
*      WHEN 'TYPE'.
*        pt_fieldcat-seltext_m    = 'I/R'.
*        pt_fieldcat-col_pos      = 9.
*      WHEN 'ERDAT'.
*        pt_fieldcat-seltext_m    = 'Creation Date'.
*        pt_fieldcat-col_pos      = 10.
*      WHEN 'AFNAM'.
*        pt_fieldcat-seltext_m    = 'Requistioner'.
*        pt_fieldcat-col_pos      = 11.
*      WHEN 'MESSAGE'.
*        pt_fieldcat-seltext_m    = 'Message'.
*        pt_fieldcat-col_pos      = 12.
*      WHEN OTHERS.
*        pt_fieldcat-no_out       = 'X'.

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
  CONCATENATE '*Interface Date : '  s_zedat-low '~' s_zedat-high
                             INTO ls_title-info SEPARATED BY space.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Closing Time : '  p_zetim
                             INTO ls_title-info SEPARATED BY space.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE '*Tolerance Day : ' p_tol
                             INTO ls_title-info SEPARATED BY space.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'A'.
  CONCATENATE '*Total Counts:' l_cnt
                              INTO ls_title-info SEPARATED BY space.
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

  s_zedat-low     = sy-datum - 2.
  s_zedat-high    = sy-datum.
  s_zedat-sign   = 'I'.
  s_zedat-option = 'BT'.
  APPEND  s_zedat.

  p_zetim = '064500'.

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
*  LOOP AT SCREEN.
*    IF screen-name = 'S_NAME-HIGH'.
*      screen-invisible  = 1.
*      screen-active     = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SEND_ERROR_EMAIL
*&---------------------------------------------------------------------*
FORM send_error_email .
  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
           it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
           it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
           it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
           it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                   WITH HEADER LINE,
           gd_cnt TYPE i,
           gd_sent_all(1) TYPE c,
           gd_doc_data LIKE sodocchgi1.

  DATA : v_qty(9).
  DATA : lv_cnt(10).
  DATA : lv_desc(100).

  DATA : lv_seq_message1(100),  lv_seq_message2(100),
         lv_seq_message3(100).
  DATA : lv_date(10).
  DATA lv_count(8).

  CLEAR : it_mail[].

  CONCATENATE 'Production Log Shop Date Checking'
              'List' INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.
  APPEND it_mail.  CLEAR : it_mail.

  WRITE sy-datum TO lv_date.

  APPEND '=================================='  TO it_mail.
  APPEND '   Checking List.                    '  TO it_mail.
  APPEND '=================================='  TO it_mail.

  CONCATENATE 'Date : ' lv_date 'Total Counts:' l_cnt_dup
                           INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.
  APPEND it_mail.  CLEAR : it_mail.

  it_mail = text-h01.
  APPEND it_mail.  CLEAR : it_mail.

  LOOP AT it_display_dup.
    MOVE : it_display_dup-p_model       TO  it_mail+2(6),
           it_display_dup-p_body_serial TO  it_mail+10(7),
           it_display_dup-flag          TO  it_mail+20(4),
           it_display_dup-p_status      TO  it_mail+27(4),
           it_display_dup-zedat         TO  it_mail+34(10),
           it_display_dup-zetim         TO  it_mail+46(8),
           it_display_dup-k04pdat       TO  it_mail+56(10),
           it_display_dup-p_rp_actual_date TO  it_mail+69(10).
    APPEND it_mail.  CLEAR : it_mail.
  ENDLOOP.


  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr =  'Prod.Log Shop Date Checking  List'.
  gd_doc_data-sensitivty = 'F'.


* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_mail LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

  CLEAR : it_receivers[].

*-  T-CODE :  mailing list  -> SO23
  it_receivers-receiver = 'PP_ALERT'.
  it_receivers-rec_type = 'C'.   "U - Internet address
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = ''.
  it_receivers-notif_ndel = ''.
  APPEND it_receivers. CLEAR it_receivers.

  CHECK it_receivers[] IS NOT INITIAL.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
      put_in_outbox              = ' '
      commit_work   = 'X'
*    IMPORTING
*      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_txt               = it_mail
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.
  ENDIF.
ENDFORM.                    " SEND_ERROR_EMAIL
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTPPVR_CHK
*&---------------------------------------------------------------------*
FORM save_ztppvr_chk .
  DATA : it_save_log LIKE ztppvr_chk OCCURS 0 WITH HEADER LINE.

  DESCRIBE TABLE it_display LINES l_cnt.
  DESCRIBE TABLE it_display_dup LINES l_cnt_dup.

  CHECK sy-uname <> 'HIS20158'.

  CLEAR : it_save_log[], it_save_log.
  LOOP AT it_display.
    MOVE-CORRESPONDING it_display TO it_save_log.
    it_save_log-erdat = sy-datum.
    it_save_log-erzet = sy-uzeit.
    it_save_log-ernam = sy-uname.

    APPEND it_save_log.
  ENDLOOP.

  MODIFY ztppvr_chk FROM TABLE it_save_log.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_ZTPPVR_CHK
