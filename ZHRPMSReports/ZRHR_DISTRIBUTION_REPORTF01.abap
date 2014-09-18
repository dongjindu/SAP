*&---------------------------------------------------------------------*
*&  Include           ZRHR_DISTRIBUTION_REPORTF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_droplist_year .

  TYPE-POOLS: vrm.

  DATA: lt_values     TYPE vrm_values,
        ls_value      LIKE LINE OF lt_values,
        l_fieldname   TYPE vrm_id,
        l_year        TYPE zdhr_year.

  l_fieldname = 'P_YEAR'.
  l_year = sy-datum(4).

  CLEAR lt_values.
  DO 10 TIMES.
    ls_value-key = l_year.
    ls_value-text = l_year.
    APPEND ls_value TO lt_values.CLEAR ls_value.
    l_year = sy-datum(4) - sy-index.
  ENDDO.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_fieldname
      values = lt_values.

ENDFORM.                    " SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*&      Form  SET_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_init_value .

  " set Year
  IF p_year IS INITIAL.
    p_year = sy-datum(4).
  ENDIF.

  " set Distributor
  p_type1 = 'X'.

ENDFORM.                    " SET_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen .

  IF p_type1 EQ 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'TP1'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'TP2'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'TP3'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF p_type2 EQ 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'TP1'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'TP2'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'TP3'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF p_type3 EQ 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'TP1'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'TP2'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'TP3'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " SET_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F4_SEARCH_HELP_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_search_help_disp.

  DATA: lt_dpool              TYPE TABLE OF zthr_dpool WITH HEADER LINE,
        lt_return             TYPE TABLE OF ddshretval WITH HEADER LINE.

  DATA: BEGIN OF lt_value OCCURS 0,
          dcode               TYPE zthr_dpool-dcode,
          pernr               TYPE persno,
          ename               TYPE p0001-ename,
          org01               TYPE zthr_dpool-org01,
          otx01               TYPE stext,
          org02               TYPE zthr_dpool-org02,
          otx02               TYPE stext,
          org03               TYPE zthr_dpool-org03,
          otx03               TYPE stext,
          org04               TYPE zthr_dpool-org04,
          otx04               TYPE stext,
          org05               TYPE zthr_dpool-org05,
          otx05               TYPE stext,
        END OF lt_value.

  DATA: BEGIN OF lt_p0001 OCCURS 0,
          pernr               TYPE p0001-pernr,
          ename               TYPE p0001-ename,
        END OF lt_p0001.

  DATA: rt_pernr              TYPE RANGE OF persno WITH HEADER LINE.

  DATA: l_endda               TYPE endda,
        l_title               TYPE char255,
        l_fieldname           TYPE string,
        l_index               TYPE n LENGTH 2.

  FIELD-SYMBOLS: <fs_orgeh>   TYPE any,
                 <fs_orgeh2>  TYPE any,
                 <fs_orgtx>   TYPE any,
                 <fs>         TYPE any.

  CHECK p_year IS NOT INITIAL.

  CONCATENATE p_year '1231' INTO l_endda.

  " set title
  MESSAGE s037 INTO l_title.

  " get distribution pool
  CLEAR lt_dpool[].
  SELECT * FROM zthr_dpool
    INTO TABLE lt_dpool.
  SORT lt_dpool BY dcode pernr.

  CHECK lines( lt_dpool ) > 0.
  SORT lt_dpool BY dcode pernr.

  CLEAR rt_pernr[].
  LOOP AT lt_dpool.
    rt_pernr-sign = 'I'.
    rt_pernr-option = 'EQ'.
    rt_pernr-low = lt_dpool-pernr.
    APPEND rt_pernr.CLEAR rt_pernr.
  ENDLOOP.

  SORT rt_pernr BY low.
  DELETE ADJACENT DUPLICATES FROM rt_pernr COMPARING low.

  " get ename
  CLEAR lt_p0001[].
  SELECT pernr
         ename
    FROM pa0001
    INTO TABLE lt_p0001
    WHERE pernr IN rt_pernr
      AND endda >= l_endda
      AND begda <= l_endda.
  SORT lt_p0001 BY pernr.

  " set export data
  LOOP AT lt_dpool.
    lt_value-dcode = lt_dpool-dcode.
    lt_value-pernr = lt_dpool-pernr.
    READ TABLE lt_p0001 WITH KEY pernr = lt_dpool-pernr
                        BINARY SEARCH.
    IF sy-subrc = 0.
      lt_value-ename = lt_p0001-ename.
    ENDIF.

    CLEAR l_index.
    " set org text
    DO 5 TIMES.
      l_index = sy-index.
      CONCATENATE 'LT_DPOOL-ORG' l_index INTO l_fieldname.
      ASSIGN (l_fieldname) TO <fs_orgeh>.
      CONCATENATE 'LT_VALUE-ORG' l_index INTO l_fieldname.
      ASSIGN (l_fieldname) TO <fs_orgeh2>.
      CONCATENATE 'LT_VALUE-OTX' l_index INTO l_fieldname.
      ASSIGN (l_fieldname) TO <fs_orgtx>.
      IF <fs_orgeh> IS NOT INITIAL.
        <fs_orgeh2> = <fs_orgeh>.
        SELECT SINGLE stext FROM hrp1000
          INTO <fs_orgtx>
          WHERE plvar = '01'
            AND otype = 'O'
            AND objid = <fs_orgeh>
            AND istat = '1'
            AND begda <= l_endda
            AND endda >= l_endda
            AND langu = sy-langu.
      ENDIF.
      UNASSIGN: <fs_orgeh>, <fs_orgtx>.
    ENDDO.

    APPEND lt_value.
    CLEAR: lt_dpool, lt_value.
  ENDLOOP.

  " set search help
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'PERNR'
      dynpprog         = sy-repid
      dynpnr           = sy-dynnr
      dynprofield      = 'P_DISP'
      window_title     = l_title
      value_org        = 'S'
      callback_program = sy-repid
      callback_form    = 'CALLBACK_F4_DISP'
    TABLES
      value_tab        = lt_value[]
      return_tab       = lt_return[]
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  " set selected value
  IF lines( lt_return ) > 0.
    CLEAR gs_dpool.
    LOOP AT lt_return.
      ASSIGN (lt_return-retfield) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = lt_return-fieldval.
      ENDIF.
    ENDLOOP.
    p_disp = gs_dpool-pernr.
  ENDIF.

ENDFORM.                    " F4_SEARCH_HELP_DISP
*&---------------------------------------------------------------------*
*&      Form  F4_SEARCH_HELP_ADIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_search_help_adis .

  DATA: lt_appraisal_id         TYPE hap_t_appraisal_id,
        lt_others               TYPE TABLE OF hrhap_others WITH HEADER LINE.

  DATA: BEGIN OF lt_apprv OCCURS 0,
          pernr                 TYPE p0001-pernr,
          ename                 TYPE p0001-ename,
        END OF lt_apprv.

  DATA: ls_sel_with_or_without  TYPE hap_s_sel_with_or_without,
        ls_sel_dates            TYPE hap_s_sel_dates,
        ls_appraisal_id         TYPE hap_s_appraisal_id.

  DATA: l_begda                 TYPE begda,
        l_endda                 TYPE endda,
        l_title                 TYPE char255.

  DATA: rt_pernr                TYPE RANGE OF persno WITH HEADER LINE.

  CHECK p_year IS NOT INITIAL.

  CONCATENATE p_year '0101' INTO l_begda.
  CONCATENATE p_year '1231' INTO l_endda.

  " set title
  MESSAGE s038 INTO l_title.

  CLEAR ls_sel_with_or_without.
  ls_sel_with_or_without-sel_display_existing = 'X'.

  CLEAR ls_sel_dates.
  ls_sel_dates-validity_from_date = l_begda.
  ls_sel_dates-validity_to_date = l_endda.

  " get appraisal document list
  CLEAR lt_appraisal_id.
  CALL FUNCTION 'HRHAP_DOCUMENT_GET_LIST'
    EXPORTING
      plan_version          = '01'
      s_sel_with_or_without = ls_sel_with_or_without
      s_sel_dates           = ls_sel_dates
    IMPORTING
      t_appraisal_id        = lt_appraisal_id.

  CHECK lines( lt_appraisal_id ) > 0.
  SORT lt_appraisal_id BY appraisal_id.
  DELETE ADJACENT DUPLICATES FROM lt_appraisal_id COMPARING appraisal_id.

  " get approver list
  CLEAR lt_others[].
  SELECT * FROM hrhap_others
    INTO TABLE lt_others
    FOR ALL ENTRIES IN lt_appraisal_id
    WHERE plan_version = '01'
      AND appraisal_id = lt_appraisal_id-appraisal_id
      AND role_id      = 'Z7'.

  CHECK lines( lt_others ) > 0.
  SORT lt_others BY id.
  DELETE ADJACENT DUPLICATES FROM lt_others COMPARING id.

  CLEAR rt_pernr[].
  LOOP AT lt_others.
    rt_pernr-sign = 'I'.
    rt_pernr-option = 'EQ'.
    rt_pernr-low = lt_others-id.
    APPEND rt_pernr.CLEAR rt_pernr.
  ENDLOOP.

  " get ename
  CLEAR lt_apprv[].
  SELECT pernr ename FROM pa0001
    INTO TABLE lt_apprv
    WHERE pernr IN rt_pernr
      AND endda >= l_endda
      AND begda <= l_endda.

  " set search help
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PERNR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_ADIS'
      window_title    = l_title
      value_org       = 'S'
    TABLES
      value_tab       = lt_apprv[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " F4_SEARCH_HELP_ADIS
*&---------------------------------------------------------------------*
*&      Form  F4_SEARCH_HELP_DDIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_search_help_ddis .

  DATA: lt_return     TYPE TABLE OF ddshretval WITH HEADER LINE.

  DATA: BEGIN OF lt_objid OCCURS 0,
          objid       TYPE hrp1000-objid,
          stext       TYPE hrp1000-stext,
        END OF lt_objid.

  DATA: BEGIN OF lt_orglv OCCURS 0,
          objid       TYPE hrp1001-objid,
          sclas       TYPE hrp1001-sclas,
          sobid       TYPE hrp1001-sobid,
        END OF lt_orglv.

  DATA: BEGIN OF lt_chief_ps OCCURS 0,
          sclas       TYPE hrp1001-sclas,
          sobid       TYPE hrp1001-sobid,
        END OF lt_chief_ps.

  DATA: BEGIN OF lt_chief OCCURS 0,
          sclas       TYPE hrp1001-sclas,
          sobid       TYPE hrp1001-sobid,
        END OF lt_chief.

  DATA: BEGIN OF lt_p0001 OCCURS 0,
          pernr       TYPE p0001-pernr,
          ename       TYPE p0001-ename,
          orgeh       TYPE p0001-orgeh,
          stext       TYPE hrp1000-stext,
        END OF lt_p0001.

  DATA: BEGIN OF lt_addps OCCURS 0,
          pernr       TYPE p0001-pernr,
          ename       TYPE p0001-ename,
          orgeh       TYPE p0001-orgeh,
        END OF lt_addps.

  DATA: l_endda       TYPE endda,
        l_title       TYPE char255,
        l_index       TYPE i.

  DATA: rt_objid      TYPE RANGE OF hrobjid WITH HEADER LINE,
        rt_pernr      TYPE RANGE OF persno  WITH HEADER LINE.

  FIELD-SYMBOLS: <fs> TYPE any.

  CHECK p_year IS NOT INITIAL.

  CONCATENATE p_year '1231' INTO l_endda.

  " set title
  MESSAGE s039 INTO l_title.

  " get org list
  CLEAR lt_objid[].
  SELECT objid stext FROM hrp1000
    INTO TABLE lt_objid
    WHERE plvar = '01'
      AND otype = 'O'
      AND istat = '1'
      AND begda <= l_endda
      AND endda >= l_endda
      AND langu = sy-langu.

  CHECK lines( lt_objid ) > 0.
  SORT lt_objid BY objid.
  DELETE ADJACENT DUPLICATES FROM lt_objid COMPARING objid.

  " get org level 40(Department)
  CLEAR lt_orglv[].
  SELECT objid sclas sobid FROM hrp1001
    INTO TABLE lt_orglv
    FOR ALL ENTRIES IN lt_objid
    WHERE otype = 'O'
      AND objid = lt_objid-objid
      AND plvar = '01'
      AND rsign = 'A'
      AND relat = 'Z03'
      AND istat = '1'
      AND begda <= l_endda
      AND endda >= l_endda
      AND sclas = 'OL'
      AND sobid LIKE '%40'.

  CHECK lines( lt_orglv ) > 0.
  SORT lt_orglv BY objid.
  DELETE ADJACENT DUPLICATES FROM lt_orglv COMPARING objid.

  " get chief position
  CLEAR lt_chief_ps[].
  SELECT sclas sobid FROM hrp1001
    INTO TABLE lt_chief_ps
    FOR ALL ENTRIES IN lt_orglv
    WHERE otype = 'O'
      AND objid = lt_orglv-objid
      AND plvar = '01'
      AND rsign = 'B'
      AND relat = '012'
      AND istat = '1'
      AND begda <= l_endda
      AND endda >= l_endda.

  CHECK lines( lt_chief_ps ) > 0.
  SORT lt_chief_ps BY sobid.
  DELETE ADJACENT DUPLICATES FROM lt_chief_ps COMPARING sobid.

  CLEAR rt_objid[].
  LOOP AT lt_chief_ps.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = lt_chief_ps-sobid.
    APPEND rt_objid.CLEAR rt_objid.
  ENDLOOP.

  " get chief employee
  CLEAR lt_chief[].
  SELECT sclas sobid FROM hrp1001
    INTO TABLE lt_chief
    WHERE otype = 'S'
      AND objid IN rt_objid
      AND plvar = '01'
      AND rsign = 'A'
      AND relat = '008'
      AND istat = '1'
      AND begda <= l_endda
      AND endda >= l_endda.

  CHECK lines( lt_chief ) > 0.
  SORT lt_chief BY sobid.
  DELETE ADJACENT DUPLICATES FROM lt_chief COMPARING sobid.

  CLEAR rt_pernr[].
  LOOP AT lt_chief.
    rt_pernr-sign = 'I'.
    rt_pernr-option = 'EQ'.
    rt_pernr-low = lt_chief-sobid.
    APPEND rt_pernr.CLEAR rt_pernr.
  ENDLOOP.

  " get chief employee
  CLEAR lt_p0001[].
  SELECT pernr ename orgeh FROM pa0001
    INTO CORRESPONDING FIELDS OF TABLE lt_p0001
    WHERE pernr IN rt_pernr
      AND endda >= l_endda
      AND begda <= l_endda.

  " get additional position
  CLEAR lt_addps[].
  SELECT a~pernr
         b~ename
         a~orgeh
    FROM zthr_addps AS a
      INNER JOIN pa0001 AS b ON  a~pernr = b~pernr
                             AND b~endda >= l_endda
                             AND b~begda <= l_endda
    INTO TABLE lt_addps
    WHERE a~begda <= l_endda
      AND a~endda >= l_endda.

  " add additional position
  LOOP AT lt_addps.
    lt_p0001-pernr = lt_addps-pernr.
    lt_p0001-orgeh = lt_addps-orgeh.
    lt_p0001-ename = lt_addps-ename.
    APPEND lt_p0001.
    CLEAR: lt_p0001, lt_addps.
  ENDLOOP.

  SORT lt_p0001 BY pernr.

  " set org text
  LOOP AT lt_p0001.
    l_index = sy-tabix.
    READ TABLE lt_objid WITH KEY objid = lt_p0001-orgeh
                        BINARY SEARCH.
    IF sy-subrc = 0.
      lt_p0001-stext = lt_objid-stext.
      MODIFY lt_p0001 INDEX l_index TRANSPORTING stext.
    ENDIF.
  ENDLOOP.

  " set search help
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'PERNR'
      dynpprog         = sy-repid
      dynpnr           = sy-dynnr
      dynprofield      = 'P_DDIS'
      window_title     = l_title
      value_org        = 'S'
      callback_program = sy-repid
      callback_form    = 'CALLBACK_F4_DDIS'
    TABLES
      value_tab        = lt_p0001[]
      return_tab       = lt_return[]
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF lines( lt_return ) > 0.
    CLEAR gs_addps.
    LOOP AT lt_return.
      ASSIGN (lt_return-retfield) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = lt_return-fieldval.
      ENDIF.
    ENDLOOP.
    p_ddis = gs_addps-pernr.
  ENDIF.

ENDFORM.                    " F4_SEARCH_HELP_DDIS
*&---------------------------------------------------------------------*
*&      Form  GET_FIELD_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0401   text
*      -->P_0402   text
*      <--P_LS_FIELD  text
*----------------------------------------------------------------------*
FORM get_field_info  USING    p_tabname
                              p_fieldname
                     CHANGING ps_field.

  DATA: lt_dfies    TYPE TABLE OF dfies WITH HEADER LINE.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tabname
      fieldname      = p_fieldname
*     lfieldname     = p_tabname
    IMPORTING
      dfies_wa       = ps_field
    TABLES
      dfies_tab      = lt_dfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE lt_dfies INDEX 1.
  IF sy-subrc = 0.
    ps_field = lt_dfies.
  ENDIF.

ENDFORM.                    " GET_FIELD_INFO
*&---------------------------------------------------------------------*
*&      Form  callback_f4_disp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RECORD_TAB   text
*      -->SHLP         text
*      -->CALLCONTROL  text
*----------------------------------------------------------------------*
FORM callback_f4_disp TABLES    record_tab  STRUCTURE seahlpres
                      CHANGING  shlp        TYPE shlp_descr
                                callcontrol LIKE ddshf4ctrl.
  DATA: ls_intf     LIKE LINE OF shlp-interface,
        ls_prop     LIKE LINE OF shlp-fieldprop.

  REFRESH: shlp-interface.
  ls_intf-shlpfield = 'F0001'.
  ls_intf-valtabname = 'GS_DPOOL'.
  ls_intf-valfield = 'DCODE'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.
  ls_intf-shlpfield = 'F0002'.
  ls_intf-valtabname = 'GS_DPOOL'.
  ls_intf-valfield = 'PERNR'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.
  ls_intf-shlpfield = 'F0004'.
  ls_intf-valtabname  = 'GS_DPOOL'.
  ls_intf-valfield = 'ORG01'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.
  ls_intf-shlpfield = 'F0006'.
  ls_intf-valtabname  = 'GS_DPOOL'.
  ls_intf-valfield = 'ORG02'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.
  ls_intf-shlpfield = 'F0008'.
  ls_intf-valtabname  = 'GS_DPOOL'.
  ls_intf-valfield = 'ORG03'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.
  ls_intf-shlpfield = 'F0010'.
  ls_intf-valtabname  = 'GS_DPOOL'.
  ls_intf-valfield = 'ORG04'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.
  ls_intf-shlpfield = 'F0012'.
  ls_intf-valtabname  = 'GS_DPOOL'.
  ls_intf-valfield = 'ORG05'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.

ENDFORM.                    "callback_f4_disp
*&---------------------------------------------------------------------*
*&      Form  callback_f4_ddis
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RECORD_TAB   text
*      -->SHLP         text
*      -->CALLCONTROL  text
*----------------------------------------------------------------------*
FORM callback_f4_ddis TABLES    record_tab  STRUCTURE seahlpres
                      CHANGING  shlp        TYPE shlp_descr
                                callcontrol LIKE ddshf4ctrl.
  DATA: ls_intf     LIKE LINE OF shlp-interface,
        ls_prop     LIKE LINE OF shlp-fieldprop.

  REFRESH: shlp-interface.
  ls_intf-shlpfield = 'F0001'.
  ls_intf-valtabname = 'GS_ADDPS'.
  ls_intf-valfield = 'PERNR'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.
  ls_intf-shlpfield = 'F0003'.
  ls_intf-valtabname = 'GS_ADDPS'.
  ls_intf-valfield = 'ORGEH'.
  APPEND ls_intf TO shlp-interface.CLEAR ls_intf.

ENDFORM.                    "callback_f4_ddis
*&---------------------------------------------------------------------*
*&      Form  GET_DIS_POOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dis_pool .

  CHECK gs_dpool IS NOT INITIAL.



ENDFORM.                    " GET_DIS_POOL
*&---------------------------------------------------------------------*
*&      Form  GET_APP_DIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_app_dis .

ENDFORM.                    " GET_APP_DIS
*&---------------------------------------------------------------------*
*&      Form  GET_DEP_DIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dep_dis .

ENDFORM.                    " GET_DEP_DIS
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv_100 .

  IF gr_cont IS INITIAL.
    CREATE OBJECT gr_cont
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = gr_cont.

    PERFORM set_layout.
    PERFORM set_fcat.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layo
      CHANGING
        it_outtab                     = gt_result[]
        it_fieldcatalog               = gt_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CALL METHOD gr_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDFORM.                    " CREATE_ALV_100
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_layout .

  gs_layo-cwidth_opt = 'X'.
  gs_layo-no_toolbar = 'X'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fcat .

ENDFORM.                    " SET_FCAT
