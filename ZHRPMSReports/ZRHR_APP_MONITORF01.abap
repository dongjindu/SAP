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

  gs_layo-zebra = 'X'.
  gs_layo-sel_mode = 'D'.

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

  " Status
  gt_fcat-fieldname = 'AP_STATUS_NAME'.
  gt_fcat-coltext = text-t06.
  gt_fcat-col_pos = 6.
  gt_fcat-outputlen = 15.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " SubStatus
  gt_fcat-fieldname = 'AP_STATUS_SUB_NAME'.
  gt_fcat-coltext = text-t07.
  gt_fcat-col_pos = 7.
  gt_fcat-outputlen = 15.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 1st Evaluator
  gt_fcat-fieldname = 'APPRAISER_ID'.
  gt_fcat-coltext = text-t08.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 8.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'APPRAISER_NAME'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 9.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 1st Coordinator
  gt_fcat-fieldname = 'COR01'.
  gt_fcat-coltext = text-t10.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 10.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM01'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 11.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 2nd Evaluator
  gt_fcat-fieldname = 'EVA02'.
  gt_fcat-coltext = text-t11.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 12.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'EVANM02'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 13.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 2nd Coordinator
  gt_fcat-fieldname = 'COR02'.
  gt_fcat-coltext = text-t12.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 14.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM02'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 15.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 3rd Evaluator
  gt_fcat-fieldname = 'EVA03'.
  gt_fcat-coltext = text-t13.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 16.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'EVANM03'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 17.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 3rd Coordinator
  gt_fcat-fieldname = 'COR03'.
  gt_fcat-coltext = text-t14.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 18.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM03'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 19.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Approver
  gt_fcat-fieldname = 'APPRV'.
  gt_fcat-coltext = text-t15.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 20.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'APPRVNM'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 21.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 4th Coordinator
  gt_fcat-fieldname = 'COR04'.
  gt_fcat-coltext = text-t16.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 22.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM04'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 23.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " HR Team
  gt_fcat-fieldname = 'HRTEAM'.
  gt_fcat-coltext = text-t17.
  gt_fcat-no_zero = 'X'.
  gt_fcat-col_pos = 24.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'HRTEAMNM'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 25.
  gt_fcat-outputlen = 20.
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
        END OF lt_p0001.

  DATA: BEGIN OF lt_p0002 OCCURS 0,
          pernr               TYPE pa0002-pernr,
          vorna               TYPE pa0002-vorna,
          nachn               TYPE pa0002-nachn,
        END OF lt_p0002.

  DATA: l_begda               TYPE begda,
        l_endda               TYPE endda,
        l_rfc_destination     TYPE rfcdest.

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

    " read the most recent appraisal document
    SORT lt_documents BY ap_end_date DESCENDING.
    READ TABLE lt_documents INTO ls_documents
                            INDEX 1.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING ls_documents TO gt_result.
    "read first, last name
    READ TABLE lt_p0002 WITH KEY pernr = gt_result-appraisee_id
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " set first, last name
      gt_result-vorna = lt_p0002-vorna.
      gt_result-nachn = lt_p0002-nachn.
    ENDIF.

    PERFORM get_detail USING ls_documents-plan_version
                             ls_documents-appraisal_id
                             ls_documents-ap_start_date.

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
