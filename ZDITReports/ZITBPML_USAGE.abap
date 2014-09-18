*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZITBPML_USAGE
*& Program Name   : APM Usage Report
*& Created by     : Victor Park
*& Created on     : 01.13.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     :
*&----------------------------------------------------------------------

REPORT  zitbpml_usage MESSAGE-ID zmpp.

TABLES : s021.

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

DATA : gt_fieldcat_out      TYPE  lvc_t_fcat,
       gs_fieldcat_out      TYPE  lvc_s_fcat.

DATA : ls_title         TYPE slis_listheader, "alv header
       alv_t_listheader TYPE slis_t_listheader.

DATA : g_extab          TYPE slis_t_extab,
       g_extab_ln       LIKE   LINE  OF  g_extab.

DATA : g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : t_colinfo_table TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA : g_repid         LIKE sy-repid.

DATA : it_tcode  LIKE ztit_apm_tcode OCCURS 0 WITH HEADER LINE,
       it_time   LIKE ztit_apm_time  OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_usage_count OCCURS 0,
         sys_cd LIKE zsit_apm_tcode-sys_cd,
         module_nm LIKE zsit_apm_tcode-module_nm,
*         yyyymm LIKE zsit_apm_tcode-yyyymm,
         yyyymm(6),
         tcode_cnt TYPE i,
         time_cnt  TYPE i,
       END OF it_usage_count.

DATA : BEGIN OF it_month OCCURS 0,
          spmon LIKE s021-spmon,
       END OF it_month.
DATA : BEGIN OF it_year OCCURS 0,
          year(5),
       END OF it_year.

DATA : l_date LIKE sy-datum.

*Dynamic table
DATA : it_table TYPE REF TO data.
FIELD-SYMBOLS: <gt_table> TYPE STANDARD TABLE.

DATA : it_bpml   LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       it_bpmla  LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       it_bpml_test  LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       wa_bpml   TYPE ztitbpml_log,
       it_bpmlh LIKE ztitbpmlh OCCURS 0 WITH HEADER LINE,
       it_package LIKE ztitdevc  OCCURS 0 WITH HEADER LINE,
       it_data    LIKE zsit_apm_tcode OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).



*&----------------------------------------------------------------------
*&    SELECTION-SCREEN
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_month FOR s021-spmon DEFAULT sy-datum+0(6)
                                        TO sy-datum+0(6) OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_month RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_year RADIOBUTTON GROUP r1.
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


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_month-low.
  PERFORM pov_month USING s_month-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_month-high.
  PERFORM pov_month USING s_month-high.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM select_data.
  PERFORM get_dynamic_table_layout.
  PERFORM modify_data.


  PERFORM  pro_alv.
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

  PERFORM check_input_data.
  PERFORM calculate_period.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tcode
  FROM ztit_apm_tcode
  WHERE yyyymm  IN s_month
    AND sys_cd  <> ''.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_time
  FROM ztit_apm_time
  WHERE yyyymm  IN s_month
    AND sys_cd  <> ''.

  IF p_year = 'X'.
    LOOP AT it_tcode.
      MOVE '00' TO it_tcode-yyyymm+4(2).
      MODIFY it_tcode.
    ENDLOOP.
    LOOP AT it_time.
      MOVE '00' TO it_time-yyyymm+4(2).
      MODIFY it_time.
    ENDLOOP.
  ENDIF.

  SORT it_tcode BY yyyymm sys_cd tcode.
  SORT it_time  BY yyyymm sys_cd tcode.
  DELETE ADJACENT DUPLICATES FROM it_tcode
                          COMPARING yyyymm sys_cd tcode.
  DELETE ADJACENT DUPLICATES FROM it_time
                          COMPARING yyyymm sys_cd tcode.

ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  FIELD-SYMBOLS : <l_field>,
                  <l_data>,
                  <ls_table>.
  DATA : fname(30),
         l_sys_cd LIKE zsit_apm_tcode-sys_cd.

  CLEAR : it_usage_count[], it_usage_count.

  ASSIGN LOCAL COPY OF INITIAL LINE OF <gt_table> TO <ls_table>.

  LOOP AT it_tcode.
    IF p_month = 'X'.
      it_usage_count-yyyymm  = it_tcode-yyyymm.
    ELSE.
      it_usage_count-yyyymm  = it_tcode-yyyymm+0(4).

    ENDIF.
    it_usage_count-sys_cd  = it_tcode-sys_cd.
    it_usage_count-tcode_cnt = 1.

    COLLECT it_usage_count. CLEAR it_usage_count.
  ENDLOOP.

  LOOP AT it_time.
    IF p_month = 'X'.
      it_usage_count-yyyymm  = it_time-yyyymm.
    ELSE.
      it_usage_count-yyyymm  = it_time-yyyymm+0(4).
    ENDIF.

    it_usage_count-sys_cd  = it_time-sys_cd.
    it_usage_count-time_cnt = 1.

    COLLECT it_usage_count. CLEAR it_usage_count.
  ENDLOOP.

  SORT it_usage_count BY sys_cd yyyymm.


  LOOP AT it_usage_count.
    IF sy-tabix <> 1.
      IF it_usage_count-sys_cd <> l_sys_cd.
        APPEND <ls_table> TO <gt_table>.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 1 OF STRUCTURE <ls_table> TO <l_field>.
    <l_field> = it_usage_count-sys_cd.

    PERFORM get_module_name.
    ASSIGN COMPONENT 2 OF STRUCTURE <ls_table> TO <l_field>.
    <l_field> = it_usage_count-module_nm.

    CONCATENATE '<LS_TABLE>-' it_usage_count-yyyymm INTO fname.

    ASSIGN (fname) TO <l_field>.
    <l_field> = ( it_usage_count-time_cnt / it_usage_count-tcode_cnt )
                           * 100.
    IF p_year = 'X'.
      CONCATENATE '<LS_TABLE>-' it_usage_count-yyyymm 'T' INTO fname.

      ASSIGN (fname) TO <l_field>.
      <l_field> =  it_usage_count-tcode_cnt .

      CONCATENATE '<LS_TABLE>-' it_usage_count-yyyymm 'U' INTO fname.

      ASSIGN (fname) TO <l_field>.
      <l_field> =  it_usage_count-time_cnt .
    ENDIF.
    AT LAST.
      APPEND <ls_table> TO <gt_table>.
    ENDAT.

    l_sys_cd = it_usage_count-sys_cd.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .
  g_repid  = sy-repid.

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat.
*                             using   'IT_DATA'.

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
      t_outtab                 = <gt_table>
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

*  CLEAR: gs_sort, p_sort[].
*
*  gs_sort-spos      = '1'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'ID'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO p_sort.

ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat   TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv.
*                        using    p_name      type  slis_tabname.

  CLEAR : pt_fieldcat[], pt_fieldcat.

  LOOP AT gt_fieldcat_out INTO gs_fieldcat_out.
    IF gs_fieldcat_out-fieldname+4(1) = ''.
      pt_fieldcat-fieldname = gs_fieldcat_out-fieldname.
      pt_fieldcat-seltext_m = gs_fieldcat_out-coltext.
      pt_fieldcat-ref_fieldname = gs_fieldcat_out-ref_field.
      pt_fieldcat-ref_tabname   = gs_fieldcat_out-ref_table.
      pt_fieldcat-outputlen     = gs_fieldcat_out-outputlen.
      pt_fieldcat-emphasize     = 'C100'.
    ELSE.
      pt_fieldcat-fieldname = gs_fieldcat_out-fieldname.
      pt_fieldcat-seltext_m = gs_fieldcat_out-coltext.
      pt_fieldcat-ref_fieldname = gs_fieldcat_out-ref_field.
      pt_fieldcat-ref_tabname   = gs_fieldcat_out-ref_table.
      pt_fieldcat-outputlen     = gs_fieldcat_out-outputlen.

    ENDIF.

    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.
    APPEND pt_fieldcat. CLEAR  pt_fieldcat.
  ENDLOOP.


*  DATA: l_datum(08).

*  sy-datum = sy-datum + 1.
*  MOVE sy-datum TO l_datum.
*  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name     = g_repid
**     i_structure_name   = p_name
*      i_internal_tabname = p_name
*      i_inclname         = g_repid
*    CHANGING
*      ct_fieldcat        = pt_fieldcat[].

*  LOOP AT pt_fieldcat.
*    CASE pt_fieldcat-fieldname.
*      WHEN 'ID'.
*        pt_fieldcat-seltext_m    = 'Seq.'.
*      WHEN 'SYS_CD'.
*        pt_fieldcat-seltext_m    = 'Sys.Code.'.
*      WHEN 'COMP_NAME'.
*        pt_fieldcat-seltext_m    = 'Corp. Name'.
*      WHEN 'MODULE_NM'.
*        pt_fieldcat-seltext_m    = 'Module'.
*      WHEN 'TCODE'.
*        pt_fieldcat-seltext_m    = 'T-code'.
*      WHEN 'TCODE_NM'.
*        pt_fieldcat-seltext_m    = 'T-code Name'.
*      WHEN 'MENU_NAME1'.
*        pt_fieldcat-seltext_m    = 'Menu1'.
*      WHEN 'MENU_NAME2'.
*        pt_fieldcat-seltext_m    = 'Menu2'.
*      WHEN 'MENU_NAME3'.
*        pt_fieldcat-seltext_m    = 'Menu3'.
*      WHEN OTHERS.
*
*    ENDCASE.
*    pt_fieldcat-reptext_ddic =
*    pt_fieldcat-seltext_s    =
*    pt_fieldcat-seltext_l    =
*    pt_fieldcat-seltext_m.
*
*    MODIFY pt_fieldcat.
*
*  ENDLOOP.
ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.
  DATA : l_line(5) TYPE n.
  DATA : h_title(30), s_title(60),  a_title(60).

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  ls_title-typ = 'H'. "(H:Header, S:Selection, A:Action)
  CONCATENATE 'Usage per Module '  ' (%)' INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Period : '  s_month-low '~' s_month-high
                                              INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  DESCRIBE TABLE it_data LINES l_line.
  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  IF p_month = 'X'.
    CONCATENATE '*Option : '  'Monthly' INTO ls_title-info.
  ELSE.
    CONCATENATE '*Option : '  'Yearly' INTO ls_title-info.
  ENDIF.
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
*&      Form  CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
FORM check_input_data .
  READ TABLE s_month INDEX 1.
  IF s_month-low IS INITIAL OR s_month-high IS INITIAL.
    MESSAGE s000 WITH 'Period months should be filled(from-to Both)'.
    STOP.
  ENDIF.

  IF p_year = 'X'.
    IF s_month-low+4(2) <> '01' OR  s_month-high+4(2) <> '12'.
      MESSAGE s000 WITH 'Months sould be from 01 to 12 for year option'.
      STOP.
    ENDIF.

*    IF s_month-low+0(4) <>  s_month-high+0(4).
*      MESSAGE s000 WITH 'Only One year can be displayed'.
*      STOP.
*    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_PERIOD
*&---------------------------------------------------------------------*
FORM calculate_period .
  CLEAR : it_month[], it_month, it_year[], it_year.

  it_month-spmon  = s_month-high.
  APPEND it_month. CLEAR it_month.

  IF s_month-high <> s_month-low.
    DO 100 TIMES.
      IF sy-index = 1.
        CONCATENATE s_month-high '01' INTO   l_date .
      ELSE.
        CONCATENATE it_month-spmon  '01' INTO l_date.
      ENDIF.

      l_date = l_date - 1.

      IF   l_date+0(6) < s_month-low.
        EXIT.
      ELSE.
        it_month-spmon  = l_date+0(6).
        APPEND it_month.
      ENDIF.
    ENDDO.
  ENDIF.

  IF p_year = 'X'.
    LOOP AT it_month.
      it_year = it_month+0(4).
      APPEND it_year.

      CONCATENATE it_month+0(4) 'T' INTO it_year.
      APPEND it_year.      "Total count

      CONCATENATE it_month+0(4) 'U' INTO it_year.
      APPEND it_year.      "Used count
    ENDLOOP.

    SORT it_year BY year.
    DELETE ADJACENT DUPLICATES FROM it_year COMPARING year.

  ENDIF.

  SORT it_month BY spmon.
ENDFORM.                    " CALCULATE_PERIOD
*&---------------------------------------------------------------------*
*&      Form  GET_DYNAMIC_TABLE_LAYOUT
*&---------------------------------------------------------------------*
FORM get_dynamic_table_layout .

  CLEAR : gt_fieldcat_out[], gt_fieldcat_out, gs_fieldcat_out.


  gs_fieldcat_out-fieldname = 'SYS_CD'.
  gs_fieldcat_out-coltext   = 'System Code'.
  gs_fieldcat_out-outputlen = '12'.
  APPEND gs_fieldcat_out TO gt_fieldcat_out.
  CLEAR : gs_fieldcat_out.

  gs_fieldcat_out-fieldname = 'MODULE_NM'.
  gs_fieldcat_out-coltext   = 'Module Name'.
  gs_fieldcat_out-outputlen = '12'.
  APPEND gs_fieldcat_out TO gt_fieldcat_out.
  CLEAR : gs_fieldcat_out.

  IF p_month = 'X'.
    LOOP AT it_month.
      gs_fieldcat_out-fieldname = it_month-spmon.
      gs_fieldcat_out-coltext   = it_month-spmon.
      gs_fieldcat_out-ref_field = 'PROZ1'.
      gs_fieldcat_out-ref_table = 'IFMSN'.
      gs_fieldcat_out-outputlen = '7'.
      APPEND gs_fieldcat_out TO gt_fieldcat_out.
      CLEAR : gs_fieldcat_out.
    ENDLOOP.
  ENDIF.

  IF p_year = 'X'.
    LOOP AT it_year.
      IF it_year-year+4(1) = 'T'.
        gs_fieldcat_out-fieldname = it_year-year.
        gs_fieldcat_out-coltext   = 'Total Count'.
        gs_fieldcat_out-ref_field = 'CALL_CNT'.    "Total Cnt
        gs_fieldcat_out-ref_table = 'ZSIT_APM_TIME'.
        gs_fieldcat_out-outputlen = '12'.
        APPEND gs_fieldcat_out TO gt_fieldcat_out.
        CLEAR : gs_fieldcat_out.
      ELSEIF it_year-year+4(1) = 'U'.
        gs_fieldcat_out-fieldname = it_year-year.
        gs_fieldcat_out-coltext   = 'Used Count'.
        gs_fieldcat_out-ref_field = 'CALL_CNT'.    "Used Cnt
        gs_fieldcat_out-ref_table = 'ZSIT_APM_TIME'.
        gs_fieldcat_out-outputlen = '12'.
        APPEND gs_fieldcat_out TO gt_fieldcat_out.
        CLEAR : gs_fieldcat_out.
      ELSE.
        gs_fieldcat_out-fieldname = it_year-year.
        gs_fieldcat_out-coltext   = it_year-year.
        gs_fieldcat_out-ref_field = 'PROZ1'.    "Percent
        gs_fieldcat_out-ref_table = 'IFMSN'.
        gs_fieldcat_out-outputlen = '7'.
        APPEND gs_fieldcat_out TO gt_fieldcat_out.
        CLEAR : gs_fieldcat_out.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fieldcat_out
    IMPORTING
      ep_table                  = it_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    ASSIGN it_table->* TO <gt_table>.
  ENDIF.

ENDFORM.                    " GET_DYNAMIC_TABLE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  GET_MODULE_NAME
*&---------------------------------------------------------------------*
FORM get_module_name .

  IF it_usage_count-sys_cd = 'E0048'.
    it_usage_count-module_nm = 'SD'.
  ELSEIF  it_usage_count-sys_cd = 'E0051'.
    it_usage_count-module_nm = 'HR'.
  ELSEIF  it_usage_count-sys_cd = 'E0053'.
    it_usage_count-module_nm = 'PM'.
  ELSEIF  it_usage_count-sys_cd = 'E0341'.
    it_usage_count-module_nm = 'QM'.
  ELSEIF  it_usage_count-sys_cd = 'E0049'.
    it_usage_count-module_nm = 'CO'.
  ELSEIF  it_usage_count-sys_cd = 'E0050'.
    it_usage_count-module_nm = 'FI'.
  ELSEIF  it_usage_count-sys_cd = 'E0052'.
    it_usage_count-module_nm = 'MM'.
  ELSEIF  it_usage_count-sys_cd = 'E0054'.
    it_usage_count-module_nm = 'PP'.
  ELSEIF  it_usage_count-sys_cd = 'B0146'.
    it_usage_count-module_nm = 'ES'.
  ELSEIF  it_usage_count-sys_cd = 'E0343'.
    it_usage_count-module_nm = 'BI'.
  ELSEIF  it_usage_count-sys_cd = 'E0342'.
    it_usage_count-module_nm = 'CS'.
  ENDIF.


ENDFORM.                    " GET_MODULE_NAME
