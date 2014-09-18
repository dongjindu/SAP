*&---------------------------------------------------------------------*
*&  Include           ZRHR_APP_MORNITORF01
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
  DATA: l_year        TYPE zdhr_year.

  g_fieldname = 'P_YEAR'.
  l_year = sy-datum(4).
*** Changed on 01/08/14
*  if not p_year2 is INITIAL.
*    p_year = p_year2.
*  endif.
*** End on 01/08/14
  CLEAR gt_values.
  DO 10 TIMES.
    gs_value-key = l_year.
    gs_value-text = l_year.
    APPEND gs_value TO gt_values.CLEAR gs_value.
    l_year = sy-datum(4) - sy-index.
  ENDDO.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.

ENDFORM.                    " SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_droplist_status .

  DATA: lt_values_tab TYPE TABLE OF dd07v WITH HEADER LINE.

  g_fieldname = 'P_ST'.

  " get status domain value
  CLEAR: lt_values_tab[].
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'HAP_AP_STATUS'
    TABLES
      values_tab      = lt_values_tab
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR gt_values.
  LOOP AT lt_values_tab.
    IF lt_values_tab-domvalue_l > 1 AND lt_values_tab-domvalue_l < 6.
      gs_value-key = lt_values_tab-domvalue_l.
      gs_value-text = lt_values_tab-ddtext.
      APPEND gs_value TO gt_values.CLEAR gs_value.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.

ENDFORM.                    " SET_DROPLIST_STATUS
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_SUBSTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ST  text
*----------------------------------------------------------------------*
FORM set_droplist_substatus  USING p_st TYPE hap_ap_status.

  IF p_st IS INITIAL OR p_st = 5.
    CLEAR: p_subst.
  ENDIF.

  g_fieldname = 'P_SUBST'.

  " get substatus for status
  CLEAR gt_values.
  SELECT ap_status_sub AS key
         ap_status_sub_na AS text
    FROM t77hap_sstatus_t
    INTO TABLE gt_values
    WHERE langu = sy-langu
      AND ap_status = p_st
      AND ap_status_sub BETWEEN 'A' AND 'Z'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.

ENDFORM.                    " SET_DROPLIST_SUBSTATUS
*&---------------------------------------------------------------------*
*&      Form  SET_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_init_value .

  IF p_year IS INITIAL.
    p_year = sy-datum(4).
  ENDIF.

** Changed on 01/07/14
*  p_year2 = p_year.
  PERFORM set_droplist_year2.
** End on 01/07/14
  p_st2 = '2'.
  PERFORM set_droplist_status2.  " set droplist Status
  PERFORM set_droplist_substatus2 USING p_st2.
ENDFORM.                    " SET_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv_100 .

  DATA: lt_sortab TYPE lvc_t_sort.
  DATA: ls_variant TYPE disvariant.

  IF gr_cont IS INITIAL.
    CREATE OBJECT gr_cont
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = gr_cont.

    PERFORM set_layout.
    PERFORM set_fcat.
    PERFORM build_sort CHANGING lt_sortab.

    ls_variant-report   = sy-repid.
    ls_variant-username = sy-uname.


    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layo
        is_variant                    = ls_variant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = gt_result[]
        it_fieldcatalog               = gt_fcat[]
        it_sort                       = lt_sortab
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

  gs_layo-zebra = 'X'.
  gs_layo-sel_mode = 'D'.
  gs_layo-numc_total = 'X'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_CTNRNM  text
*      -->PR_SORT    text
*----------------------------------------------------------------------*
FORM build_sort CHANGING pr_sort TYPE lvc_t_sort.
  PERFORM build_sort_value
    USING: pr_sort 'AP_STATUS_NAME' 1 '0' 'X',
           pr_sort 'AP_STATUS_SUB_NAME' 1 '0' 'X'.
ENDFORM.                    "BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  build_sort_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_SORT    text
*      -->P_FIELD    text
*      -->P_UP       text
*      -->P_SPOS     text
*      -->P_SUB      text
*----------------------------------------------------------------------*
FORM build_sort_value USING pt_sort TYPE lvc_t_sort
                            p_field
                            p_up
                            p_spos
                            p_sub.

  DATA: ls_sort TYPE lvc_s_sort.

  ls_sort-fieldname = p_field.
  ls_sort-spos      = p_spos.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_sub.
  ls_sort-group     = 'UL'.

  APPEND ls_sort TO pt_sort.

ENDFORM.                    "build_sort_value
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fcat .

  CLEAR: gt_fcat[].
  " Duration(From)
  gt_fcat-fieldname = 'AP_START_DATE'.
  gt_fcat-coltext = text-t01.
  gt_fcat-col_pos = 1.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Duration(To)
  gt_fcat-fieldname = 'AP_END_DATE'.
  gt_fcat-coltext = text-t02.
  gt_fcat-col_pos = 2.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Team Member
  gt_fcat-fieldname = 'APPRAISEE_ID'.
  gt_fcat-coltext = text-t03.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 3.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " First Name
  gt_fcat-fieldname = 'VORNA'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 4.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Last Name
  gt_fcat-fieldname = 'NACHN'.
  gt_fcat-coltext = text-t05.
  gt_fcat-col_pos = 5.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Classification
  gt_fcat-fieldname = 'CLFTX'.
  gt_fcat-coltext = text-t26.
  gt_fcat-col_pos = 6.
  gt_fcat-outputlen = 25.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Coster Center
  gt_fcat-fieldname = 'KOSTL'.
  gt_fcat-coltext = text-t24.
  gt_fcat-col_pos = 7.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Coster Center Description
  gt_fcat-fieldname = 'KTEXT'.
  gt_fcat-coltext = text-t25.
  gt_fcat-col_pos = 8.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Org. Unit
  gt_fcat-fieldname = 'ORGEH'.
  gt_fcat-coltext = text-t21.
  gt_fcat-col_pos = 9.
  gt_fcat-outputlen = 8.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Org. Unit Description
  gt_fcat-fieldname = 'STEXT'.
  gt_fcat-coltext = text-t22.
  gt_fcat-col_pos = 10.
  gt_fcat-outputlen = 40.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Status
  gt_fcat-fieldname = 'AP_STATUS_NAME'.
  gt_fcat-coltext = text-t06.
  gt_fcat-col_pos = 11.
  gt_fcat-outputlen = 15.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " SubStatus
  gt_fcat-fieldname = 'AP_STATUS_SUB_NAME'.
  gt_fcat-coltext = text-t07.
  gt_fcat-col_pos = 12.
  gt_fcat-outputlen = 15.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Count
  gt_fcat-fieldname = 'COUNT'.
  gt_fcat-coltext = text-t84.
  gt_fcat-col_pos = 13.
  gt_fcat-outputlen = 6.
  gt_fcat-do_sum = 'X'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 1st Evaluator
  gt_fcat-fieldname = 'APPRAISER_ID'.
  gt_fcat-coltext = text-t08.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 14.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'APPRAISER_NAME'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 15.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 1st Coordinator
  gt_fcat-fieldname = 'COR01'.
  gt_fcat-coltext = text-t10.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 16.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM01'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 17.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 2nd Evaluator
  gt_fcat-fieldname = 'EVA02'.
  gt_fcat-coltext = text-t11.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 18.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'EVANM02'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 19.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 2nd Coordinator
  gt_fcat-fieldname = 'COR02'.
  gt_fcat-coltext = text-t12.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 20.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM02'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 21.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 3rd Evaluator
  gt_fcat-fieldname = 'EVA03'.
  gt_fcat-coltext = text-t13.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 22.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'EVANM03'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 23.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 3rd Coordinator
  gt_fcat-fieldname = 'COR03'.
  gt_fcat-coltext = text-t14.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 24.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM03'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 25.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Approver
  gt_fcat-fieldname = 'APPRV'.
  gt_fcat-coltext = text-t15.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 26.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'APPRVNM'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 27.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 4th Coordinator
  gt_fcat-fieldname = 'COR04'.
  gt_fcat-coltext = text-t16.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 28.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM04'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 29.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " HR Team
  gt_fcat-fieldname = 'HRTEAM'.
  gt_fcat-coltext = text-t17.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 30.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'HRTEAMNM'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 31.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CHANGE_DATE'.
  gt_fcat-coltext = text-t23.
  gt_fcat-col_pos = 32.
  gt_fcat-outputlen = 12.
  APPEND gt_fcat.CLEAR: gt_fcat.


ENDFORM.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_appraisees         TYPE hap_t_hrsobid,
        lt_documents          TYPE hap_t_documents,
        lt_sel_status_sub     TYPE hap_t_status_sub.

  DATA: ls_sel_dates          TYPE hap_s_sel_dates,
        ls_sel_status         TYPE hap_s_sel_status,
        ls_sel_status_sub     TYPE hap_s_status_sub,
        ls_sel_with_or_witout TYPE hap_s_sel_with_or_without,
        ls_appraisees         TYPE hrsobid,
        ls_documents          TYPE hap_s_documents,
        ls_return             TYPE bapiret2.

  DATA: BEGIN OF lt_p0000 OCCURS 0,
          pernr               TYPE p0000-pernr,
          stat2               TYPE p0000-stat2,
        END OF lt_p0000.

  DATA: BEGIN OF lt_p0001 OCCURS 0,
          pernr               TYPE pa0001-pernr,
          kostl               TYPE pa0001-kostl,
          plans               TYPE pa0001-plans,
          stell               TYPE pa0001-stell,
          orgeh               TYPE pa0001-orgeh,
        END OF lt_p0001.

  DATA: BEGIN OF lt_p0002 OCCURS 0,
          pernr               TYPE pa0002-pernr,
          vorna               TYPE pa0002-vorna,
          nachn               TYPE pa0002-nachn,
        END OF lt_p0002.

  DATA: l_begda               TYPE begda,
        l_endda               TYPE endda,
        l_rfc_destination     TYPE rfcdest.

  DATA: BEGIN OF lt_class OCCURS 0,
          clfid               TYPE zthr_clfaj-clfid,
          jobid               TYPE zthr_clfaj-jobid,
          clftx               TYPE zthr_class-clftx,
          zefrom              TYPE zthr_class-zefrom,
          zeto                TYPE zthr_class-zeto,
        END OF lt_class.

  CLEAR: gt_result[].

  IF p_year IS INITIAL.
    MESSAGE s026 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    " set begin date, end date
    CONCATENATE p_year '0101' INTO l_begda.
    CONCATENATE p_year '1231' INTO l_endda.
  ENDIF.

**********************************************
*   Get Data
**********************************************

** On 11/14/13 by Furong for adding Classification
  CLEAR lt_class[].
  SELECT a~clfid
         a~jobid
         b~clftx
         b~zefrom
         b~zeto
    FROM zthr_clfaj AS a
      INNER JOIN zthr_class AS b
      ON a~clfid = b~clfid
    INTO TABLE lt_class.
  LOOP AT lt_class.
    IF p_year >= lt_class-zefrom+0(4)
       AND p_year <= lt_class-zeto+0(4).
    ELSE.
      DELETE lt_class.
    ENDIF.
  ENDLOOP.
  SORT lt_class BY jobid.
** End On 11/14/13

  " check active
  CLEAR: lt_p0000[].
  SELECT pernr stat2 FROM pa0000
    INTO TABLE lt_p0000
    WHERE endda >= l_endda
      AND begda <= l_endda
      AND stat2 = '3'.

  CHECK lines( lt_p0000 ) > 0.
  SORT lt_p0000 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_p0000 COMPARING pernr.

  " check Employee Group, Employee Subgroup
  CLEAR lt_p0001[].
  SELECT pernr
         kostl
         plans
         stell
         orgeh
    FROM pa0001
    INTO TABLE lt_p0001
    FOR ALL ENTRIES IN lt_p0000
    WHERE pernr = lt_p0000-pernr
      AND endda >= l_endda
      AND begda <= l_endda
      AND persg = '1'
      AND persk IN ('U2','U3').

  CHECK lines( lt_p0001 ) > 0.
  SORT lt_p0001 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_p0001 COMPARING pernr.

  " get first name, last name
  CLEAR lt_p0002[].
  SELECT pernr vorna nachn FROM pa0002
    INTO TABLE lt_p0002
    FOR ALL ENTRIES IN lt_p0001
    WHERE pernr = lt_p0001-pernr
      AND endda >= l_endda
      AND begda <= l_endda.
  SORT lt_p0002 BY pernr.

  " set appraisal dates
  CLEAR ls_sel_dates.
  ls_sel_dates-validity_from_date = l_begda.
  ls_sel_dates-validity_to_date = l_endda.

  " set appraisal status
  CLEAR ls_sel_status.
  IF p_st IS INITIAL.
    ls_sel_status-ap_status_2 = 'X'.
    ls_sel_status-ap_status_3 = 'X'.
    ls_sel_status-ap_status_4 = 'X'.
    ls_sel_status-ap_status_5 = 'X'.
  ELSE.
    IF p_st EQ 2.
      ls_sel_status-ap_status_2 = 'X'.
    ELSEIF p_st EQ 3.
      ls_sel_status-ap_status_3 = 'X'.
    ELSEIF p_st EQ 4.
      ls_sel_status-ap_status_4 = 'X'.
    ELSEIF p_st EQ 5.
      ls_sel_status-ap_status_5 = 'X'.
    ENDIF.
  ENDIF.

  " set appraisal substatus
  CLEAR: ls_sel_status_sub, lt_sel_status_sub.
  IF p_subst IS INITIAL.
  ELSE.
    ls_sel_status_sub-ap_status = p_st.
    ls_sel_status_sub-ap_status_sub = p_subst.
    APPEND ls_sel_status_sub TO lt_sel_status_sub.
  ENDIF.

  " set appraisal display
  CLEAR ls_sel_with_or_witout.
  ls_sel_with_or_witout-sel_display_existing = 'X'.

  " get destination
  CALL FUNCTION 'HRHAP_GET_RFC_DESTINATION'
*   EXPORTING
*     SYSTEM                = 'WDU' " YHM left empty to force space
    IMPORTING
      rfc_destination       = l_rfc_destination.

  LOOP AT lt_p0001.
    " set appraisal appraisee
    CLEAR: ls_appraisees, lt_appraisees.
    ls_appraisees-plvar = '01'.
    ls_appraisees-otype = 'P'.
    ls_appraisees-sobid = lt_p0001-pernr.
    APPEND ls_appraisees TO lt_appraisees.

    " get appraisal document
    CALL FUNCTION 'HRHAP_RFC_DOCUMENT_GET_LIST'
      DESTINATION l_rfc_destination
      EXPORTING
        plan_version          = '01'
        s_sel_date            = ls_sel_dates
        s_sel_status          = ls_sel_status
        s_sel_with_or_without = ls_sel_with_or_witout
      IMPORTING
        s_return              = ls_return
      TABLES
        t_appraisees          = lt_appraisees
        t_sel_status_sub      = lt_sel_status_sub
        t_documents           = lt_documents.

    IF ls_return-type IS NOT INITIAL.
      CONTINUE.
    ENDIF.

* On 11/14/13 by Furong for adding Classification
    " read classification
    READ TABLE lt_class WITH KEY jobid = lt_p0001-stell
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " set classification
*      gt_result-clfid = lt_class-clfid.
      gt_result-clftx = lt_class-clftx.
    ENDIF.
** End on 11/14/13

    " read the most recent appraisal document
    SORT lt_documents BY ap_end_date DESCENDING.
    READ TABLE lt_documents INTO ls_documents
                            INDEX 1.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    gt_result-count = 1.
    MOVE-CORRESPONDING ls_documents TO gt_result.
    "read first, last name
    READ TABLE lt_p0002 WITH KEY pernr = gt_result-appraisee_id
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " set first, last name
      gt_result-vorna = lt_p0002-vorna.
      gt_result-nachn = lt_p0002-nachn.
    ENDIF.

*   Get Org. Unit
    gt_result-orgeh = lt_p0001-orgeh.

    SELECT stext INTO gt_result-stext
      FROM hrp1000
     UP TO 1 ROWS
     WHERE objid = lt_p0001-orgeh
       AND endda = '99991231'.
    ENDSELECT.

    PERFORM get_detail USING ls_documents-plan_version
                             ls_documents-appraisal_id
                             ls_documents-ap_start_date.

    PERFORM omit_prefix CHANGING: gt_result-appraiser_name,
                                  gt_result-cornm01,
                                  gt_result-evanm02,
                                  gt_result-cornm02,
                                  gt_result-evanm03,
                                  gt_result-cornm03,
                                  gt_result-apprvnm,
                                  gt_result-cornm04,
                                  gt_result-hrteamnm.

*** 07/22/2013 - T00306 Start
    gt_result-kostl = lt_p0001-kostl.  " Coster Center

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gt_result-kostl
      IMPORTING
        output = gt_result-kostl.

    SELECT SINGLE ktext INTO gt_result-ktext  " Description
      FROM cskt
     WHERE kostl = lt_p0001-kostl
       AND spras = 'E'
       AND datbi = '99991231'.
*** 07/22/2013 - T00306 End

    APPEND gt_result.

    CLEAR: lt_p0001, ls_return, lt_documents, ls_documents, gt_result.
  ENDLOOP.

  IF gt_result[] IS INITIAL.
    MESSAGE s027 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DOCUMENTS  text
*      <--P_GS_RESULT  text
*----------------------------------------------------------------------*
FORM get_detail USING p_plan_version TYPE hap_plan_version
                      p_appraisal_id TYPE hap_appraisal_id
                      p_start_date   TYPE hap_ap_start_date.

  DATA: lt_others   TYPE TABLE OF hrhap_others WITH HEADER LINE.

  DATA: BEGIN OF lt_p0001 OCCURS 0,
          pernr     TYPE p0001-pernr,
          ename     TYPE p0001-ename,
        END OF lt_p0001.

  DATA: rt_pernr    TYPE RANGE OF persno WITH HEADER LINE.

  CLEAR: lt_others[].
  SELECT * FROM hrhap_others
    INTO TABLE lt_others
    WHERE plan_version = p_plan_version
      AND appraisal_id = p_appraisal_id.

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
  CLEAR lt_p0001[].
  SELECT pernr ename FROM pa0001
    INTO TABLE lt_p0001
    WHERE pernr IN rt_pernr
* BEGIN OF UD1K956832
      AND endda >= sy-datum
      AND begda <= sy-datum.
*     AND endda >= p_start_date
*     AND begda <= p_start_date.
* END OF UD1K956832
  SORT lt_p0001 BY pernr.

  LOOP AT lt_others.
    " read ename
    READ TABLE lt_p0001 WITH KEY pernr = lt_others-id
                        BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CASE lt_others-role_id.
      WHEN 'Z2'.    " 1st Coordinator
        gt_result-cor01 = lt_others-id.
        gt_result-cornm01 = lt_p0001-ename.
      WHEN 'Z3'.    " 2nd Evaluator
        gt_result-eva02 = lt_others-id.
        gt_result-evanm02 = lt_p0001-ename.
      WHEN 'Z4'.    " 2nf Coordinator
        gt_result-cor02 = lt_others-id.
        gt_result-cornm02 = lt_p0001-ename.
      WHEN 'Z5'.    " 3rd Evaluator
        gt_result-eva03 = lt_others-id.
        gt_result-evanm03 = lt_p0001-ename.
      WHEN 'Z6'.    " 3rd Coordinator
        gt_result-cor03 = lt_others-id.
        gt_result-cornm03 = lt_p0001-ename.
      WHEN 'Z7'.    " Approver
        gt_result-apprv = lt_others-id.
        gt_result-apprvnm = lt_p0001-ename.
      WHEN 'Z8'.    " 4th Coordinator
        gt_result-cor04 = lt_others-id.
        gt_result-cornm04 = lt_p0001-ename.
      WHEN 'Z9'.    " 1st HR Team
        gt_result-hrteam = lt_others-id.
        gt_result-hrteamnm = lt_p0001-ename.
    ENDCASE.

    CLEAR: lt_others, lt_p0001.
  ENDLOOP.

ENDFORM.                    " GET_DETAIL
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       Send email
*----------------------------------------------------------------------*
*      -->P_PERNR  TM Number
*      -->P_EMAIL  Email address
*----------------------------------------------------------------------*
FORM send_email  USING    p_pernr
                          p_email.

  DATA: send_request       TYPE REF TO cl_bcs.
  DATA: text               TYPE bcsy_text.
  DATA: document           TYPE REF TO cl_document_bcs.
  DATA: sender             TYPE REF TO cl_sapuser_bcs.
  DATA: recipient          TYPE REF TO if_recipient_bcs.
  DATA: bcs_exception      TYPE REF TO cx_bcs.
  DATA: sent_to_all        TYPE os_boolean.

  DATA: l_subject          TYPE string,
        l_text             TYPE LINE OF bcsy_text,
        l_nachn            TYPE p0002-nachn,
        l_vorna            TYPE p0002-vorna,
        l_fname(100)       TYPE c,
        l_pernr(10)        TYPE c.

  DATA: l_desc(100).

* Set Subject
  l_subject = 'e-Performance System : Action Required'.

* Get Role
  READ TABLE gt_subst WITH KEY key = p_subst2.

  TRY.
*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document -------------------------------
*     create document from internal table with text

* Start of e-mail body

** 02/21/14 by Furong
      if p_st = 4 AND gt_subst-key = 'K'. "for acknowldgement
        CONCATENATE 'The annual evaluation for the following Team Member is'
                    'ready for acknowledgement in the e-Performance system.'
                 INTO l_text SEPARATED BY space.
        APPEND l_text TO text. CLEAR l_text.
        APPEND l_text TO text. CLEAR l_text.

        DO 92 TIMES.
          CONCATENATE '-' l_text INTO l_text.
        ENDDO.
        APPEND l_text TO text. CLEAR l_text.

        MOVE: 'TM#' TO l_text(10),
              'First Name' TO l_text+11(40),
              'Last Name' TO l_text+52(40).
        APPEND l_text TO text. CLEAR l_text.

        DO 92 TIMES.
          CONCATENATE '-' l_text INTO l_text.
        ENDDO.
        APPEND l_text TO text. CLEAR l_text.

        LOOP AT gt_notif WHERE receiver = p_pernr.
          MOVE: gt_notif-appraisee_id TO l_text(10),
                gt_notif-vorna TO l_text+11(40),
                gt_notif-nachn TO l_text+52(40).
          APPEND l_text TO text. CLEAR l_text.
        ENDLOOP.

        APPEND l_text TO text. CLEAR l_text.

        CONCATENATE 'Acknowledgement in the system is required after'
                       'review of the evaluation and performance discussion.'
                    INTO l_text SEPARATED BY space.
        APPEND l_text TO text. CLEAR l_text.
        APPEND l_text TO text. CLEAR l_text.

        CONCATENATE 'Please Note: Acknowledgement does not indicate'
                           'agreement with the evaluation.'
                        INTO l_text SEPARATED BY space.
        APPEND l_text TO text. CLEAR l_text.
        APPEND l_text TO text. CLEAR l_text.

        CONCATENATE g_spelldate
                     'is the due date for all forms to be completed.'
                INTO l_text SEPARATED BY space.
        APPEND l_text TO text. CLEAR l_text.
        APPEND l_text TO text. CLEAR l_text.


      ELSE.
** End on 02/21/14
        IF gt_subst-key NE 'A'.
          CONCATENATE 'Appraisal Documents are ready for your review in'
                      'the e-Performance system.'
                 INTO l_text SEPARATED BY space.
          APPEND l_text TO text. CLEAR l_text.
        ENDIF.

* 06/26/2013 T00306 - Start
        l_desc = gt_subst-text.
        PERFORM update_substatus_description USING gt_subst-key
                                          CHANGING l_desc.
* 06/26/2013 T00306 - End

        IF gt_subst-key NE 'A'.
          CONCATENATE 'Your role is' l_desc
                 INTO l_text SEPARATED BY space.
          CONCATENATE l_text '.' INTO l_text.
          APPEND l_text TO text. CLEAR l_text.
          APPEND l_text TO text. CLEAR l_text.
        ENDIF.

* 06/26/2013 T00306 - Start
        IF p_st = 2 AND gt_subst-key = 'A'.
          CONCATENATE 'Please complete Goal Setting in your e-Performance'
                      'form and submit to your supervisor as soon as possible.'
            INTO l_text SEPARATED BY space.
          APPEND l_text TO text. CLEAR l_text.
          APPEND l_text TO text. CLEAR l_text.
        ELSEIF ( p_st EQ 3 OR p_st EQ 4 ) AND gt_subst-key = 'A'.
          CONCATENATE 'Please complete your self-appraisal in your e-Performance'
                      'form and submit to your supervisor as soon as possible.'
            INTO l_text SEPARATED BY space.
          APPEND l_text TO text. CLEAR l_text.
          APPEND l_text TO text. CLEAR l_text.
        ELSE.
          l_text = 'Please review the following documents:'.
          APPEND l_text TO text. CLEAR l_text.
          APPEND l_text TO text. CLEAR l_text.

* Set header of the list
          IF gt_subst-key NE 'A'.
            DO 92 TIMES.
              CONCATENATE '-' l_text INTO l_text.
            ENDDO.
            APPEND l_text TO text. CLEAR l_text.

            MOVE: 'TM#' TO l_text(10),
                  'First Name' TO l_text+11(40),
                  'Last Name' TO l_text+52(40).
            APPEND l_text TO text. CLEAR l_text.

            DO 92 TIMES.
              CONCATENATE '-' l_text INTO l_text.
            ENDDO.
            APPEND l_text TO text. CLEAR l_text.
          ENDIF.
        ENDIF.

* 06/26/2013 T00306 - End

* Write the list
        IF gt_subst-key NE 'A'.

          LOOP AT gt_notif WHERE receiver = p_pernr.
            MOVE: gt_notif-appraisee_id TO l_text(10),
                  gt_notif-vorna TO l_text+11(40),
                  gt_notif-nachn TO l_text+52(40).
            APPEND l_text TO text. CLEAR l_text.
          ENDLOOP.

          APPEND l_text TO text. CLEAR l_text.

          CONCATENATE g_spelldate
                       'is the due date for all forms to be completed.'
                  INTO l_text SEPARATED BY space.
          APPEND l_text TO text. CLEAR l_text.
          APPEND l_text TO text. CLEAR l_text.
        ENDIF.

      endif.

      CONCATENATE 'If you have questions or need assistance, please'
                  'contact the HR Learning & Development Team:'
             INTO l_text SEPARATED BY space.
      APPEND l_text TO text. CLEAR l_text.

      l_text = '- Kristina Spencer x 8115; KristinaSpencer@hmmausa.com'.
      APPEND l_text TO text. CLEAR l_text.
*      l_text = '- Nancy Powers     x 8164; nancypowers@hmmausa.com'.
*      append l_text to text. clear l_text.
      l_text = '- Elisa Walden     x 8038; Elisa.Walden@hmmausa.com'.
      APPEND l_text TO text. CLEAR l_text.
      l_text = '- Linda Cook       x 8027; Linda.Cook@hmmausa.com'.
      APPEND l_text TO text. CLEAR l_text.

      document = cl_document_bcs=>create_document(
                      i_type    = 'RAW'
                      i_text    = text
                      i_length  = '12'
                      i_subject = 'Subject' ).

*     add document to send request
      CALL METHOD send_request->set_document( document ).

*     change subject line using more than 50 characters
      CALL METHOD send_request->set_message_subject( l_subject ).

*     No delivery status
      CALL METHOD send_request->set_status_attributes( 'N' ).

*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.

      sender = cl_sapuser_bcs=>create( sy-uname ).
      CALL METHOD send_request->set_sender
        EXPORTING
          i_sender = sender.

*     --------- add recipient (e-mail address) -----------------------
*     create recipient - please replace e-mail address !!!
*      p_email = 'sunhojeong@hmmausa.com'.
      recipient = cl_cam_address_bcs=>create_internet_address(
                                                             p_email ).

*     add recipient with its respective attributes to send request
      CALL METHOD send_request->add_recipient
        EXPORTING
          i_recipient = recipient
          i_express   = 'X'.

*     ---------- send document ---------------------------------------
      CALL METHOD send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = sent_to_all ).

      COMMIT WORK.

* -----------------------------------------------------------
* *                     exception handling
* -----------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* -----------------------------------------------------------
    CATCH cx_bcs INTO bcs_exception.
      WRITE: / text-001, p_email.
      WRITE:   text-002, bcs_exception->error_type.
      EXIT.

  ENDTRY.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_SUBSTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ST  text
*----------------------------------------------------------------------*
FORM set_droplist_substatus2  USING p_st TYPE hap_ap_status.

  IF p_st IS INITIAL OR p_st = 5.
    CLEAR: p_subst.
  ENDIF.

  g_fieldname = 'P_SUBST2'.

  " get substatus for status
  CLEAR gt_values.
  SELECT ap_status_sub AS key
         ap_status_sub_na AS text
    FROM t77hap_sstatus_t
    INTO TABLE gt_values
    WHERE langu = sy-langu
      AND ap_status = p_st
      AND ap_status_sub BETWEEN 'A' AND 'Z'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.

  gt_subst[] = gt_values[].

ENDFORM.                    " SET_DROPLIST_SUBSTATUS2
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_droplist_status2 .

  DATA: lt_values_tab TYPE TABLE OF dd07v WITH HEADER LINE.

  g_fieldname = 'P_ST2'.

  " get status domain value
  CLEAR: lt_values_tab[].
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'HAP_AP_STATUS'
    TABLES
      values_tab      = lt_values_tab
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR gt_values.
  LOOP AT lt_values_tab.
    IF lt_values_tab-domvalue_l > 1 AND lt_values_tab-domvalue_l < 6.
      gs_value-key = lt_values_tab-domvalue_l.
      gs_value-text = lt_values_tab-ddtext.
      APPEND gs_value TO gt_values.CLEAR gs_value.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.

ENDFORM.                    " SET_DROPLIST_STATUS2
*&---------------------------------------------------------------------*
*&      Form  OMIT_PREFIX
*&---------------------------------------------------------------------*
*       Remove name with prefix
*----------------------------------------------------------------------*
*      <--P_NAME  Name that contains prefix
*----------------------------------------------------------------------*
FORM omit_prefix  CHANGING p_name.
  CHECK NOT p_name IS INITIAL.
  REPLACE FIRST OCCURRENCE OF 'Mrs' IN p_name WITH ''.
  IF sy-subrc <> 0.
    REPLACE FIRST OCCURRENCE OF 'Mr' IN p_name WITH ''.
    IF sy-subrc <> 0.
      REPLACE FIRST OCCURRENCE OF 'Ms' IN p_name WITH ''.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF 'Miss' IN p_name WITH ''.
      ENDIF.
    ENDIF.
  ENDIF.

  CONDENSE p_name.
ENDFORM.                    " OMIT_PREFIX
*&---------------------------------------------------------------------*
*&      Form  email_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM email_processing.

  DATA: l_receiver_text(100),
        l_count TYPE i,
        l_endda TYPE endda.

  data: l_sup_email like gt_notif-email.

  CONCATENATE p_year '1231' INTO l_endda.
  .
  SELECT SINGLE ap_end_date INTO g_duedate
    FROM zthr_evapd
   WHERE zyear = p_year
     AND ap_status = p_st
     AND ap_status_sub = ' '.

  IF sy-subrc = 0.
    CALL FUNCTION 'MONTH_NAMES_GET'
      TABLES
        month_names = gt_month_names.
    READ TABLE gt_month_names WITH KEY mnr = g_duedate+4(2).
    IF sy-subrc = 0.
      g_spelldate = g_duedate+6(2).
      SHIFT g_spelldate LEFT DELETING LEADING '0'.
      CONCATENATE gt_month_names-ltx g_spelldate
             INTO g_spelldate SEPARATED BY space.
      CONCATENATE g_spelldate ',' INTO g_spelldate.
      CONCATENATE g_spelldate g_duedate(4)
             INTO g_spelldate SEPARATED BY space.
    ENDIF.
  ENDIF.

  LOOP AT gt_result.
    MOVE-CORRESPONDING gt_result TO gt_notif.

    CASE p_subst.
      WHEN 'A'.
        gt_notif-receiver = gt_result-appraisee_id.
        CONCATENATE gt_notif-vorna gt_notif-nachn
        INTO gt_notif-name SEPARATED BY space.
      WHEN 'B'.
        gt_notif-receiver = gt_result-appraiser_id.
        gt_notif-name = gt_result-appraiser_name.
      WHEN 'C'.
        gt_notif-receiver = gt_result-cor01.
        gt_notif-name = gt_result-cornm01.
      WHEN 'D'.
        gt_notif-receiver = gt_result-eva02.
        gt_notif-name = gt_result-evanm02.
      WHEN 'E'.
        gt_notif-receiver = gt_result-cor02.
        gt_notif-name = gt_result-cornm02.
      WHEN 'F'.
        gt_notif-receiver = gt_result-eva03.
        gt_notif-name = gt_result-evanm03.
      WHEN 'G'.
        gt_notif-receiver = gt_result-cor03.
        gt_notif-name = gt_result-cornm03.
      WHEN 'H'.
        gt_notif-receiver = gt_result-apprv.
        gt_notif-name = gt_result-apprvnm.
      WHEN 'I'.
        gt_notif-receiver = gt_result-cor04.
        gt_notif-name = gt_result-cornm04.
** Furong on 02/21/14
      WHEN 'K'.
        gt_notif-receiver = gt_result-appraisee_id.
        CONCATENATE gt_notif-vorna gt_notif-nachn
        INTO gt_notif-name SEPARATED BY space.
** End on 02/21/14
      WHEN OTHERS.
        CLEAR gt_notif.
        CONTINUE.
    ENDCASE.

    SELECT usrid_long INTO gt_notif-email
      FROM pa0105
     UP TO 1 ROWS
     WHERE pernr = gt_notif-receiver
       AND subty = '0010'
       AND endda = '99991231'.
    ENDSELECT.
    APPEND gt_notif. CLEAR gt_notif.
  ENDLOOP.

  SORT gt_notif BY receiver name email.

  LOOP AT gt_notif.
    AT NEW email.
      IF NOT gt_notif-email IS INITIAL.
        PERFORM send_email USING gt_notif-receiver gt_notif-email.

** Furong ( add supervisor in the email)
        if p_st = 4 AND gt_subst-key = 'K'. "for acknowldgement
          read table gt_result with key appraisee_id = gt_notif-receiver.
          SELECT usrid_long INTO l_sup_email
            FROM pa0105
            UP TO 1 ROWS
            WHERE pernr = gt_result-appraiser_id
            AND subty = '0010'
            AND endda = '99991231'.
          ENDSELECT.
          if sy-subrc = 0.
          PERFORM send_email USING gt_notif-receiver l_sup_email.
          endif.
        endif.
** )
      ELSEIF NOT gt_notif-receiver IS INITIAL.
        CLEAR gt_duplicate.
        gt_duplicate-appraisee_id = gt_notif-receiver.
        APPEND gt_duplicate.
      ENDIF.
    ENDAT.
  ENDLOOP.

  LOOP AT gt_notif.
    CLEAR l_count.
    SELECT SINGLE COUNT(*) INTO l_count
      FROM pa0105
     WHERE pernr = gt_notif-appraisee_id
       AND subty = '0010'
       AND endda = '99991231'.

    IF l_count EQ 0.
      MOVE-CORRESPONDING gt_notif TO gt_duplicate.
      APPEND gt_duplicate.
    ENDIF.
  ENDLOOP.

  SORT gt_duplicate BY appraisee_id.
  DELETE ADJACENT DUPLICATES FROM gt_duplicate COMPARING appraisee_id.

  LOOP AT gt_duplicate.
    AT FIRST.
      WRITE: /.
      WRITE: /.
      WRITE: / 'No e-mail address for the below TM(s):'.
      WRITE: / '------------------------------------------------------------------------------------------'.
      WRITE: /(10) 'TM#',(25) 'First name',(25) 'Last name'.
      WRITE: / '------------------------------------------------------------------------------------------'.
    ENDAT.

    IF gt_duplicate-vorna IS INITIAL.
      SELECT SINGLE vorna nachn
        FROM pa0002
        INTO (gt_duplicate-vorna, gt_duplicate-nachn)
       WHERE pernr = gt_duplicate-appraisee_id
         AND endda >= l_endda
         AND begda <= l_endda.
    ENDIF.

    WRITE: /(10) gt_duplicate-appraisee_id,
            (25) gt_duplicate-vorna,
            (25) gt_duplicate-nachn.
  ENDLOOP.


  LOOP AT gt_notif WHERE receiver IS INITIAL.
    AT FIRST.
      PERFORM update_substatus_description  USING p_subst
                                         CHANGING l_receiver_text.
      CONCATENATE 'Please check (' l_receiver_text ') for the below TM(s):'
             INTO l_receiver_text.
      WRITE: /.
      WRITE: /.
      WRITE: / l_receiver_text.
      WRITE: / '------------------------------------------------------------------------------------------'.
      WRITE: /(10) 'TM#',(25) 'First name',(25) 'Last name'.
      WRITE: / '------------------------------------------------------------------------------------------'.
    ENDAT.
    WRITE: /(10) gt_notif-appraisee_id, (25) gt_notif-vorna,(25) gt_notif-nachn.
  ENDLOOP.

*   Push mail out from SAP outbox
  SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.

ENDFORM.                    " SENDING_EMAIL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SUBSTATUS_DESCRIPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SUBST_TEXT  text
*----------------------------------------------------------------------*
FORM update_substatus_description  USING p_key
                                CHANGING c_text.
  CLEAR c_text.

  CASE p_key.
    WHEN 'A'.
      c_text = 'Team Members'.
    WHEN 'B'.
      c_text = '1st Evaluator'.
    WHEN 'C'.
      c_text = '1st Coordinator'.
    WHEN 'D'.
      c_text = '2nd Evaluator'.
    WHEN 'E'.
      c_text = '2nd Coordinator'.
    WHEN 'F'.
      c_text = '3rd Evaluator'.
    WHEN 'G'.
      c_text = '3rd Coordinator'.
    WHEN 'H'.
      c_text = 'Final Approver'.
    WHEN 'I'.
      c_text = '4th Coordinator'.
  ENDCASE.

ENDFORM.                    " UPDATE_SUBSTATUS_DESCRIPTION
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_YEAR2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_droplist_year2 .
  DATA: l_year        TYPE zdhr_year.

  g_fieldname = 'P_YEAR2'.
  l_year = sy-datum(4).
  if p_year2 is INITIAL.
    p_year2 = l_year.
  endif.
  CLEAR gt_values.
  DO 2 TIMES.
    gs_value-key = l_year.
    gs_value-text = l_year.
    APPEND gs_value TO gt_values.CLEAR gs_value.
    l_year = sy-datum(4) - sy-index.
  ENDDO.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.
ENDFORM.                    " SET_DROPLIST_YEAR2
