*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZIIT_BPML_TIME
*& Program Name   : APM Performance I/F
*& Created by     : Victor Park
*& Created on     : 10.18.2013
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     :
*&----------------------------------------------------------------------

REPORT  ziit_bpml_time MESSAGE-ID zmpp.

TABLES : ztitbpml_log.

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

DATA : it_bpml   LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       it_bpmla  LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       it_bpml_test  LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       it_bpml_data  LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       wa_bpml   TYPE ztitbpml_log,
       it_package LIKE ztitdevc  OCCURS 0 WITH HEADER LINE,
       it_data  LIKE zsit_apm_time OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).



*&----------------------------------------------------------------------
*&    SELECTION-SCREEN
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS : p_month TYPE s021-spmon DEFAULT sy-datum+0(6) OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS : p_old   AS CHECKBOX.
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
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month.
  PERFORM pov_month USING p_month.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_old <> 'X'.  "normal case
    PERFORM select_data.
  ELSE.               "Read from Original table->No enough data
    PERFORM select_data_old.
  ENDIF.

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
*&      Form  POV_MONTH
*&---------------------------------------------------------------------*
FORM pov_month  USING    p_month.
  DATA: lv_spmon TYPE spmon.

  MOVE: sy-datum(6) TO lv_spmon.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = lv_spmon
    IMPORTING
      selected_month             = p_month
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " POV_MONTH

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  DATA : lv_edate TYPE sy-datum,
         lv_sdate TYPE sy-datum,
         lv_date  TYPE sy-datum,
         lv_time  TYPE sy-uzeit.

  CLEAR : it_bpml_data[], it_bpml_data.

  CONCATENATE p_month '01' INTO lv_sdate.
  PERFORM cal_month_lastdate USING p_month  lv_edate.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpml
  FROM ztitbpml_log
  WHERE zdate >= lv_sdate
    AND zdate <= lv_edate
    AND tcode LIKE  'Z%'
    AND apm = 'X'
    AND respmt > 0.

  SORT it_bpml BY  l4f tcode zdate DESCENDING ztime DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_bpml COMPARING l4f tcode.

  SELECT l4f tcode SUM( diastepcnt )  SUM( users ) AVG( respmt )
        AVG( cpumt ) AVG( dbmt )
  INTO (it_bpml_data-l4f, it_bpml_data-tcode, it_bpml_data-diastepcnt,
          it_bpml_data-users,
          it_bpml_data-respmt, it_bpml_data-cpumt, it_bpml_data-dbmt)
  FROM  ztitbpml_log
  WHERE zdate >= lv_sdate
    AND zdate <= lv_edate
    AND apm = 'X'
    AND tcode LIKE  'Z%'
    AND respmt > 0
  GROUP BY l4f tcode.

    APPEND it_bpml_data.
  ENDSELECT.

  SORT it_bpml_data BY tcode.

  SELECT * INTO TABLE it_package
  FROM ztitdevc.

  SORT it_package BY devclass.

  it_bpmla[] = it_bpml[].

ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : l_index TYPE sy-index.
  DATA : i_cnt TYPE i.
  CLEAR : it_data[], it_data.

  SORT it_bpml  BY tcode.
  SORT it_bpmla BY tcode.
  DELETE ADJACENT DUPLICATES FROM it_bpmla COMPARING tcode.

  LOOP AT it_bpmla.
    CLEAR : wa_bpml.

    it_data-yyyymm     = p_month.

    CLEAR : it_package.
    READ TABLE it_package WITH KEY devclass  = it_bpmla-devclass
                                              BINARY SEARCH.

    READ TABLE it_bpml WITH KEY tcode  = it_bpmla-tcode
                                BINARY SEARCH.
    l_index = sy-tabix.
    LOOP AT it_bpml  FROM l_index.
      IF it_bpmla-tcode <> it_bpml-tcode.
        EXIT.
      ELSE.
        IF it_bpml-l2  =  it_package-ztdevc2.
          MOVE-CORRESPONDING it_bpml TO wa_bpml.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF wa_bpml IS INITIAL.
      MOVE-CORRESPONDING it_bpmla TO wa_bpml.
    ENDIF.

    it_data-tcode       = it_bpmla-tcode.

    CLEAR : it_bpml_data.
    READ TABLE it_bpml_data WITH KEY "l4f   = wa_bpml-l4f
                                     tcode = wa_bpml-tcode
                                     BINARY SEARCH.
    IF sy-subrc = 0.
      it_data-call_cnt   = it_bpml_data-diastepcnt.
      it_data-users_cnt  = it_bpml_data-users.
      it_data-resp_tm    = it_bpml_data-respmt.
      it_data-server_tm  = it_bpml_data-cpumt + it_bpml_data-dbmt .

    ENDIF.

    IF it_data-call_cnt = 0 OR it_data-resp_tm = 0.
      CONTINUE.
    ENDIF.

*-System Code
    IF wa_bpml-l2 = 'SD'.
      it_data-sys_cd   = 'E0048'.
    ELSEIF wa_bpml-l2 = 'HR'.
      it_data-sys_cd   = 'E0051'.
    ELSEIF wa_bpml-l2 = 'PM'.
      it_data-sys_cd   = 'E0053'.
    ELSEIF wa_bpml-l2 = 'QM'.
      it_data-sys_cd   = 'E0341'.
    ELSEIF wa_bpml-l2 = 'CO'.
      it_data-sys_cd   = 'E0049'.
    ELSEIF wa_bpml-l2 = 'FI'.
      it_data-sys_cd   = 'E0050'.
    ELSEIF wa_bpml-l2 = 'MM'.
      it_data-sys_cd   = 'E0052'.
    ELSEIF wa_bpml-l2 = 'PP'.
      it_data-sys_cd   = 'E0054'.
    ELSEIF wa_bpml-l2 = 'ES'.
      it_data-sys_cd   = 'B0146'.
    ELSEIF wa_bpml-l2 = 'BI'.
      it_data-sys_cd   = 'E0343'.
    ELSEIF wa_bpml-l2 = 'CS'.
      it_data-sys_cd   = 'E0342'.
    ELSEIF wa_bpml-l2 = 'BM' OR  it_bpml-l2 = 'TR'.
      it_data-sys_cd   = 'E0050'.
    ELSE.
      CONTINUE.   "Dont' send
    ENDIF.

    APPEND it_data. CLEAR : it_data.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_SEND
*&---------------------------------------------------------------------*
FORM pro_send .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.

  CHECK it_data[] IS NOT INITIAL.

  CALL FUNCTION 'ZFIT_OB_BPML02' DESTINATION v_dest
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
*&      Form  CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
FORM cal_month_lastdate  USING    p_month
                                  p_edate.

  DATA : lv_date TYPE sy-datum.
  CONCATENATE p_month '01' INTO lv_date.

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
  gs_sort-fieldname = 'TCODE'.
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
      WHEN 'YYYYMM'.
        pt_fieldcat-seltext_m    = 'Month'.
      WHEN 'SYS_CD'.
        pt_fieldcat-seltext_m    = 'System Code'.
      WHEN 'TCODE '.
        pt_fieldcat-seltext_m    = 'T-code'.
      WHEN 'CALL_CNT'.
        pt_fieldcat-seltext_m    = 'Monthly Called Count'.
      WHEN 'USERS_CNT'.
        pt_fieldcat-seltext_m    = 'Monthly Users Count'.
      WHEN 'RESP_TM'.
        pt_fieldcat-seltext_m    = 'Ave. Response time'.
      WHEN 'SERVER_TM'.
        pt_fieldcat-seltext_m    = 'Ave. Server time'.
      WHEN 'NETWORK_TM'.
        pt_fieldcat-seltext_m    = 'Ave. Network time'.
      WHEN 'Error Cnt'.
        pt_fieldcat-seltext_m    = 'Monthly Error Count'.
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
  CONCATENATE '*Month : '  p_month INTO ls_title-info.
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

  DATA : it_save LIKE ztit_apm_time OCCURS 0 WITH HEADER LINE.
  DATA : l_zseq(10) TYPE n.

  SELECT zseq INTO l_zseq
  FROM ztit_apm_time
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

  INSERT ztit_apm_time FROM TABLE it_save
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_OLD
*&---------------------------------------------------------------------*
FORM select_data_old .
  DATA : lv_edate TYPE sy-datum,
          lv_sdate TYPE sy-datum,
          lv_date  TYPE sy-datum,
          lv_time  TYPE sy-uzeit.

  CLEAR : it_bpml_data[], it_bpml_data.

  CONCATENATE p_month '01' INTO lv_sdate.
  PERFORM cal_month_lastdate USING p_month  lv_edate.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpml
  FROM ztitbpml_log
  WHERE zdate >= lv_sdate
    AND zdate <= lv_edate
    AND tcode LIKE  'Z%'
    AND apm = 'X'.
*    AND respmt > 0.

  SORT it_bpml BY  l4f tcode zdate DESCENDING ztime DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_bpml COMPARING l4f tcode.


  SELECT tcode SUM( diastepcnt ) COUNT( DISTINCT account )
               AVG( respmt ) AVG( cpumt ) AVG( dbmt )
  INTO (it_bpml_data-tcode, it_bpml_data-diastepcnt,
        it_bpml_data-users, it_bpml_data-respmt, it_bpml_data-cpumt,
        it_bpml_data-dbmt)
    FROM zthrappusge
  WHERE ldate >= lv_sdate
    AND ldate <= lv_edate
*    AND apm = 'X'
    AND tcode LIKE  'Z%'
    AND respmt > 0
  GROUP BY tcode.

    APPEND it_bpml_data.
  ENDSELECT.

  SORT it_bpml_data BY tcode.

  SELECT * INTO TABLE it_package
  FROM ztitdevc.

  SORT it_package BY devclass.

  it_bpmla[] = it_bpml[].
ENDFORM.                    " SELECT_DATA_OLD
