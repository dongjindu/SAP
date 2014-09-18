*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZHRR_PAYROLL_HOUR_AUDIT
*& Program Name   : Payroll Hour Audit Report
*& Created by     : Victor Park
*& Created on     : 08.07.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& Desc.
*&
*&----------------------------------------------------------------------

REPORT  zhrr_payroll_hour_audit MESSAGE-ID zmhr.


TABLES : pa0001, pa2001, pa2002, hrp1001, zthr_tm02.

*- ALV
TYPE-POOLS: slis.
DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gs_fieldcat         TYPE slis_fieldcat_alv,
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
DATA : g_repid         LIKE sy-repid,
       v_cnt           TYPE i.

*-Internal table
DATA: BEGIN OF lt_cpid OCCURS 0,
        objid LIKE hrp1001-objid ,
        sobid LIKE hrp1001-sobid,
        pernr LIKE pa0001-pernr,
        END OF lt_cpid.

DATA: BEGIN OF it_pernr OCCURS 0,
        objid  LIKE hrp1001-objid,
        pernr  LIKE pa0000-pernr,
        stat2  LIKE pa0000-stat2,
        text1  LIKE t529u-text1,  "Status
        begda  LIKE pa0000-begda,
      END   OF it_pernr.

DATA : BEGIN OF it_timeinfo OCCURS 0,
        pernr LIKE pa2002-pernr,
        awart LIKE pa2002-awart,
       group_cd LIKE zthr_tm02-group_cd,
        stdaz LIKE pa2002-stdaz,
      END OF it_timeinfo.
DATA : it_timeinfo_sum LIKE it_timeinfo OCCURS 0 WITH HEADER LINE.

DATA : it_paygroup LIKE zthr_tm02 OCCURS  0 WITH HEADER LINE,
       it_paygroup_tmp LIKE zthr_tm02 OCCURS  0 WITH HEADER LINE.

*Dynamic table
DATA : it_table TYPE REF TO data.
FIELD-SYMBOLS: <gt_table> TYPE STANDARD TABLE.

*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-t01.
SELECTION-SCREEN POSITION 33.
PARAMETERS     p_abkrs LIKE  pa0001-abkrs OBLIGATORY DEFAULT '11'.
SELECTION-SCREEN COMMENT 40(5) text-t02.
SELECTION-SCREEN POSITION 45.
SELECT-OPTIONS s_begda FOR pa0001-begda NO-EXTENSION MODIF ID dis.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_curren RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND uc1.
SELECTION-SCREEN COMMENT 2(20) text-t03 FOR FIELD p_curren.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_cprd TYPE t569v-pabrp MODIF ID dis,
            p_cyr  TYPE t569v-pabrj MODIF ID dis.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_other RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 2(20) text-t04 FOR FIELD p_other.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_oprd TYPE t569v-pabrp,
            p_oyr  TYPE t569v-pabrj.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECT-OPTIONS s_objid FOR hrp1001-objid NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.



*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM pro_init.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM get_pay_period.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.



*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_it_pernr.
  PERFORM get_time_info.
  PERFORM get_dynamic_table_layout.
  PERFORM modify_data.

  PERFORM display_alv.
*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  PRO_INIT
*&---------------------------------------------------------------------*
FORM pro_init .
  DATA: l_period TYPE t569v-pabrp,
       l_year   TYPE t569v-pabrj.

  CLEAR : s_begda[], s_begda.

  g_repid  = sy-repid.

  CALL FUNCTION 'PA03_PERIODDATES_GET'
    EXPORTING
      f_abkrs               = p_abkrs
    IMPORTING
      f_current_begda       = s_begda-low
      f_current_endda       = s_begda-high
    CHANGING
      f_current_period      = l_period
      f_current_year        = l_year
    EXCEPTIONS
      pcr_does_not_exist    = 1
      abkrs_does_not_exist  = 2
      period_does_not_exist = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    s_begda-sign = 'I'.
    s_begda-option = 'BT'.
    APPEND s_begda.

    IF p_curren = 'X'.
      p_cprd = l_period.
      p_cyr  = l_year.
    ENDIF.
  ENDIF.

  SELECT * INTO TABLE it_paygroup
  FROM zthr_tm02.

  SORT it_paygroup BY awart.
ENDFORM.                    " PRO_INIT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*

FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-group1 = 'DIS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  GET_PAY_PERIOD
*&---------------------------------------------------------------------*
FORM get_pay_period .
  DATA: l_period TYPE t569v-pabrp,
        l_year   TYPE t569v-pabrj.

  DATA: l_vabrp LIKE t549q-vabrp,
        l_vabrj LIKE t549q-vabrj.


  CASE 'X'.
    WHEN p_curren.
      CLEAR: s_begda, s_begda[].
      CLEAR: l_period, l_year,
             p_oprd, p_oyr.

      CALL FUNCTION 'PA03_PERIODDATES_GET'
        EXPORTING
          f_abkrs               = p_abkrs
        IMPORTING
          f_current_begda       = s_begda-low
          f_current_endda       = s_begda-high
        CHANGING
          f_current_period      = l_period
          f_current_year        = l_year
        EXCEPTIONS
          pcr_does_not_exist    = 1
          abkrs_does_not_exist  = 2
          period_does_not_exist = 3
          OTHERS                = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        s_begda-sign = 'I'.
        s_begda-option = 'BT'.
        APPEND s_begda.

        p_cprd = l_period.
        p_cyr  = l_year.

      ENDIF.


    WHEN p_other.
      CHECK p_oprd IS NOT INITIAL AND p_oyr IS NOT INITIAL.

      CLEAR: s_begda, s_begda[].
      CLEAR: p_cprd, p_cyr.
*      SHIFT: p_cprd LEFT DELETING LEADING '0'.

      l_period = p_oprd.
      l_year   = p_oyr.
      IF NOT l_period IS INITIAL.
        CALL FUNCTION 'PA03_PERIODDATES_GET'
          EXPORTING
            f_abkrs               = p_abkrs
          IMPORTING
            f_current_begda       = s_begda-low
            f_current_endda       = s_begda-high
          CHANGING
            f_current_period      = l_period
            f_current_year        = l_year
          EXCEPTIONS
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            OTHERS                = 4.

        s_begda-sign = 'I'.
        s_begda-option = 'BT'.
        APPEND s_begda.
      ENDIF.
  ENDCASE.


ENDFORM.                    " GET_PAY_PERIOD
*&---------------------------------------------------------------------*
*&      Form  GET_IT_PERNR
*&---------------------------------------------------------------------*
FORM get_it_pernr .

  SELECT objid sobid INTO CORRESPONDING FIELDS OF TABLE lt_cpid
  FROM hrp1001
  WHERE otype = 'CP'
    AND objid IN s_objid
    AND plvar = '01'
    AND endda = '99991231'
    AND sclas = 'P'.

  LOOP AT lt_cpid.
    lt_cpid-pernr = lt_cpid-sobid.
    MODIFY lt_cpid.
  ENDLOOP.

  CHECK lt_cpid[] IS NOT INITIAL.
  SELECT a~pernr b~stat2 c~text1 b~begda
           INTO CORRESPONDING FIELDS OF TABLE it_pernr
           FROM pa0001 AS a INNER JOIN pa0000 AS b
                              ON a~pernr  = b~pernr
                            INNER JOIN t529u AS c
                              ON b~stat2 = c~statv
                             AND c~sprsl = sy-langu
                             AND c~statn = '2'
    FOR ALL ENTRIES IN lt_cpid
             WHERE a~pernr = lt_cpid-pernr
             AND a~endda EQ '99991231'
             AND a~abkrs EQ p_abkrs
             AND b~endda EQ '99991231'.

  LOOP AT it_pernr WHERE stat2 = 0.
    IF it_pernr-begda < s_begda-low.
      DELETE it_pernr.
    ENDIF.
  ENDLOOP.

  IF it_pernr[] IS INITIAL.
    MESSAGE s000 WITH 'There is No data'.
    STOP.
  ENDIF.
ENDFORM.                    " GET_IT_PERNR
*&---------------------------------------------------------------------*
*&      Form  GET_TIME_INFO
*&---------------------------------------------------------------------*
FORM get_time_info .
  SELECT pernr awart stdaz docnr
  INTO CORRESPONDING FIELDS OF TABLE it_timeinfo
  FROM pa2001
  FOR ALL ENTRIES IN it_pernr
  WHERE pernr = it_pernr-pernr
    AND begda >= s_begda-low
    AND endda <= s_begda-high.


  SELECT pernr awart stdaz docnr
  APPENDING CORRESPONDING FIELDS OF TABLE it_timeinfo
  FROM pa2002
  FOR ALL ENTRIES IN it_pernr
  WHERE pernr = it_pernr-pernr
    AND begda >= s_begda-low
    AND endda <= s_begda-high.

  LOOP AT it_timeinfo.
    CLEAR it_timeinfo_sum.

    it_timeinfo_sum-pernr = it_timeinfo-pernr.
    it_timeinfo_sum-stdaz = it_timeinfo-stdaz.

    LOOP AT it_paygroup WHERE awart = it_timeinfo-awart.
      it_timeinfo_sum-group_cd = it_paygroup-group_cd.
      COLLECT it_timeinfo_sum.
    ENDLOOP.
*    READ TABLE it_paygroup WITH KEY awart = it_timeinfo-awart.
*    IF sy-subrc = 0.
*      it_timeinfo_sum-group_cd = it_paygroup-group_cd.
*      COLLECT it_timeinfo_sum.
*    ELSE.  """???
**      MESSAGE s000 WITH 'No Payroll Audit Group mapping'
**                        it_timeinfo-awart.
**      STOP.
*    ENDIF.
  ENDLOOP.

*-fill Person ID
  SORT lt_cpid BY pernr.
  LOOP AT it_pernr.
    READ TABLE lt_cpid WITH KEY pernr = it_pernr-pernr
                                       BINARY SEARCH.
    IF sy-subrc = 0.
      it_pernr-objid = lt_cpid-objid.
      MODIFY it_pernr.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_TIME_INFO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv .
  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM convert_fieldcat    TABLES gt_fieldcat.

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

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
*  p_layout-colwidth_optimize = 'X'.
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
  gs_sort-tabname   = '<GT_TABLE>'.
  gs_sort-fieldname = 'OBJID'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat.

  PERFORM setting_fieldcat   USING :
                                  'S' 'CHK'        ' ',
                                  ' ' 'COLTEXT'    ' ',
                                  ' ' 'OUTPUTLEN'    '1',
                                  ' ' 'KEY'          'X',
                                  ' ' 'NO_OUT'       'X',
                                  'E' 'FIX_COLUMN'   'X',

                                  'S' 'OBJID'        ' ',
                                  ' ' 'COLTEXT'    'CP ID',
                                  ' ' 'OUTPUTLEN'    '10',
                                  ' ' 'KEY'          'X',
                                  'E' 'FIX_COLUMN'   'X',


                                  'S' 'PERNR'        ' ',
                                  ' ' 'COLTEXT'    'TM #',
                                  ' ' 'OUTPUTLEN'    '10',
                                  ' ' 'KEY'          'X',
                                  'E' 'FIX_COLUMN'   'X',

                                  'S' 'TEXT1'        ' ',
                                  ' ' 'COLTEXT'    'Status',
                                  ' ' 'KEY'          'X',
                                  ' ' 'LOWERCASE'    'X',
                                  ' ' 'OUTPUTLEN'    '10',
                                  'E' 'FIX_COLUMN'   'X'.


  it_paygroup_tmp[] = it_paygroup[].
  SORT it_paygroup_tmp BY group_cd.
  DELETE ADJACENT DUPLICATES FROM it_paygroup_tmp COMPARING group_cd.

  LOOP AT it_paygroup_tmp.

    PERFORM setting_fieldcat   USING :
                          'S' it_paygroup_tmp-group_cd  ' ',
                          ' ' 'COLTEXT'    it_paygroup_tmp-group_nm,
                          ' ' 'JUST'       'R',
                          ' ' 'DATATYPE'   'DEC',
                          ' ' 'DECIMALS'  '2',
                          ' ' 'NO_ZERO'   'X',
*                          ' ' 'DO_SUM'    'X',
                          'E' 'OUTPUTLEN'    '6'.


  ENDLOOP.

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




ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write   USING alv_t_listheader TYPE slis_t_listheader.
  DATA : l_cnt(5).
  DATA : h_title(30), s_title(60),  a_title(60).

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DESCRIBE TABLE <gt_table> LINES l_cnt.

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  ls_title-info = ''.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE 'Period : ' s_begda-low+4(2) '/' s_begda-low+6(2) '/'
                         s_begda-low+0(4) '~' s_begda-high+4(2) '/'
                         s_begda-high+6(2) '/' s_begda-high+0(4)
            INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE 'Print Date : ' sy-datum+4(2) '/' sy-datum+6(2) '/'
                              sy-datum+0(4) INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE 'Prepared by : ' sy-uname   INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE 'Count : ' l_cnt   INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.
ENDFORM.                    " LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
FORM append_alv_event   CHANGING p_alv_event TYPE slis_t_event.
* TOP-OF-PAGE Event

  DATA ls_events TYPE slis_alv_event.
  ls_events-name  =  'TOP_OF_PAGE'.
  ls_events-form  =  'TOP_OF_PAGE'.
  APPEND ls_events TO p_alv_event.

ENDFORM.                    " APPEND_ALV_EVENT

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
FORM setting_fieldcat  USING   p_gubun
                               p_field
                               p_value.

  DATA : lv_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: gs_fieldcat_out.

    gs_fieldcat_out-fieldname  = p_field.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'GS_FIELDCAT_OUT-' p_field  INTO lv_col.
  ASSIGN (lv_col) TO <fs>.
  MOVE   p_value  TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    IF gs_fieldcat_out-col_pos IS INITIAL.
      ADD 1 TO v_cnt.
      gs_fieldcat_out-col_pos = v_cnt.
    ENDIF.
    APPEND gs_fieldcat_out TO gt_fieldcat_out.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  GET_DYNAMIC_TABLE_LAYOUT
*&---------------------------------------------------------------------*
FORM get_dynamic_table_layout .

  PERFORM fieldcat.

ENDFORM.                    " GET_DYNAMIC_TABLE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_FIELDCAT
*&---------------------------------------------------------------------*
FORM convert_fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv.
  CLEAR : pt_fieldcat[], pt_fieldcat.

  LOOP AT gt_fieldcat_out INTO gs_fieldcat_out.

    MOVE-CORRESPONDING gs_fieldcat_out TO pt_fieldcat.
    pt_fieldcat-seltext_m = gs_fieldcat_out-coltext.

    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.
    APPEND pt_fieldcat. CLEAR  pt_fieldcat.
  ENDLOOP.
ENDFORM.                    " CONVERT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  FIELD-SYMBOLS : <l_field>,
                  <l_data>,
                  <ls_table>.
  DATA : fname(30).

  ASSIGN LOCAL COPY OF INITIAL LINE OF <gt_table> TO <ls_table>.

  SORT it_timeinfo_sum BY pernr.

  LOOP AT it_pernr.

    MOVE-CORRESPONDING it_pernr TO <ls_table>.

    LOOP AT it_timeinfo_sum WHERE pernr = it_pernr-pernr.
      CONCATENATE '<LS_TABLE>-' it_timeinfo_sum-group_cd INTO fname.
      ASSIGN (fname) TO <l_field>.
      <l_field> = it_timeinfo_sum-stdaz.

    ENDLOOP.

    APPEND <ls_table> TO <gt_table>.     CLEAR : <ls_table>.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
