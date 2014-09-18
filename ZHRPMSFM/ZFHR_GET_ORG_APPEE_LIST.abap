FUNCTION zfhr_get_org_appee_list.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_PLAN_VERSION) TYPE  HAP_PLAN_VERSION DEFAULT '01'
*"     REFERENCE(I_ORGEH) TYPE  ORGEH
*"     REFERENCE(I_PERNR) TYPE  PERSNO
*"     REFERENCE(IT_CATEGORIES) TYPE  HAP_T_C
*"     REFERENCE(I_BEGDA) TYPE  BEGDA DEFAULT SY-DATUM
*"     REFERENCE(I_ENDDA) TYPE  ENDDA DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(E_EMPCNT) TYPE  I
*"     REFERENCE(ET_EMPLT) TYPE  ZTYHR_EMPLT
*"----------------------------------------------------------------------

  DATA: lt_objec                TYPE TABLE OF objec WITH HEADER LINE,
        lt_appraisee            TYPE hap_t_hrsobid,
        lt_documents            TYPE hap_t_wd_documents,
        lt_clfaj                TYPE TABLE OF zthr_clfaj,
        lt_pa0000               TYPE TABLE OF pa0000.

  DATA: ls_appraisee            TYPE hrsobid,
        ls_sel_with_or_without  TYPE hap_s_sel_with_or_without,
        ls_sel_dates            TYPE hap_s_sel_dates,
        ls_sel_status           TYPE hap_s_sel_status,
        ls_documents            TYPE hap_s_wd_documents,
        ls_emplt                LIKE LINE OF et_emplt,
        ls_clfaj                TYPE zthr_clfaj,
        ls_hrp1000              TYPE hrp1000.

  DATA: BEGIN OF ls_pernr,
          pernr                 TYPE pa0001-pernr,
          persg                 TYPE pa0001-persg,
          persk                 TYPE pa0001-persk,
          stell                 TYPE pa0001-stell,
          ename                 TYPE pa0001-ename,
        END OF ls_pernr,
        lt_pernr                LIKE TABLE OF ls_pernr.

  DATA: l_keydt                 TYPE sy-datum.

  DATA: rt_pernr                TYPE RANGE OF persno WITH HEADER LINE.

  CHECK i_orgeh IS NOT INITIAL.

  CLEAR: e_empcnt, et_emplt.

* set key date
  IF i_endda < sy-datum.
    l_keydt = i_endda.
  ELSEIF i_begda < sy-datum AND i_endda > sy-datum.
    l_keydt = sy-datum.
  ELSE.
    l_keydt = sy-datum.
  ENDIF.

* check org
  SELECT SINGLE * FROM hrp1000
    INTO ls_hrp1000
    WHERE plvar = i_plan_version
      AND otype = 'O'
      AND objid = i_orgeh
      AND istat = '1'
      AND begda <= l_keydt
      AND endda >= l_keydt
      AND langu = sy-langu.
  IF sy-subrc = 0.
*   get org struc
    CLEAR lt_objec[].
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype    = 'O'
        act_objid    = i_orgeh
        act_wegid    = 'O-S-P'
        act_plvar    = '01'
        act_begda    = l_keydt
        act_endda    = l_keydt
      TABLES
        result_objec = lt_objec.

    DELETE lt_objec WHERE otype <> 'P'.
    CHECK lines( lt_objec ) > 0.
  ENDIF.

  CLEAR rt_pernr[].
  LOOP AT lt_objec.
    rt_pernr-sign = 'I'.
    rt_pernr-option = 'EQ'.
    rt_pernr-low = lt_objec-objid.
    APPEND rt_pernr.CLEAR rt_pernr.
  ENDLOOP.

* check active
  CLEAR lt_pa0000.
  SELECT * FROM pa0000
    INTO TABLE lt_pa0000
    WHERE pernr IN rt_pernr
** By Furong for terminated employee
      AND endda >= sy-datum
      AND begda <= sy-datum
*      AND endda >= l_keydt
*      AND begda <= l_keydt
** End
      AND stat2 = '3'.

  SORT lt_pa0000 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_pa0000 COMPARING pernr.

* get personal info
  CLEAR lt_pernr.
  SELECT pernr
         persg
         persk
         stell
         ename
    FROM pa0001
    INTO TABLE lt_pernr
    FOR ALL ENTRIES IN lt_pa0000
    WHERE pernr = lt_pa0000-pernr
      AND endda >= l_keydt
      AND begda <= l_keydt
      AND persg = '1'
      AND persk IN ('U2', 'U3').

* delete himself
  DELETE lt_pernr WHERE pernr = i_pernr.

* get Classification and Job Maintenance
* get executive jobid
  CLEAR lt_clfaj.
  SELECT * FROM zthr_clfaj
    INTO TABLE lt_clfaj
    WHERE clfid = 'E01'.

* delete executive classification id
  LOOP AT lt_clfaj INTO ls_clfaj.
    DELETE lt_pernr WHERE stell = ls_clfaj-jobid.
  ENDLOOP.

** set appraisee
*  CLEAR lt_appraisee.
*  LOOP AT lt_pernr INTO ls_pernr.
*    ls_appraisee-plvar = i_plan_version.
*    ls_appraisee-otype = 'P'.
*    ls_appraisee-sobid = ls_pernr-pernr.
*    APPEND ls_appraisee TO lt_appraisee.
*    CLEAR: ls_pernr, ls_appraisee.
*  ENDLOOP.
*
** set date
*  CLEAR ls_sel_dates.
*  ls_sel_dates-validity_from_date = i_begda.
*  ls_sel_dates-validity_to_date = i_endda.
*
** set status
*  CLEAR ls_sel_status.
*  ls_sel_status-ap_status_4 = abap_true.
*  ls_sel_status-ap_status_5 = abap_true.
*
*  CLEAR ls_sel_with_or_without.
*  ls_sel_with_or_without-sel_display_existing = abap_true.
*
** get appraisal documents list.
*  CALL METHOD cl_hap_wd_start_page_ui=>document_get_list
*    EXPORTING
*      add_on_application    = 'PA'
*      plan_version          = '01'
*      t_categories          = it_categories
*      t_appraisees          = lt_appraisee
*      s_sel_dates           = ls_sel_dates
*      s_sel_status          = ls_sel_status
**     t_sel_status_sub      =
*      s_sel_with_or_without = ls_sel_with_or_without
*    IMPORTING
*      t_document            = lt_documents.
*
*  CHECK lines( lt_documents ) > 0.
*
** export data setting
*  LOOP AT lt_documents INTO ls_documents.
*    ls_emplt-pernr = ls_documents-appraisee_id.
*    ls_emplt-appraisal_id = ls_documents-appraisal_id.
*    READ TABLE lt_pernr INTO ls_pernr
*                        WITH KEY pernr = ls_documents-appraisee_id
*                        BINARY SEARCH.
*    ls_emplt-stell = ls_pernr-stell.
*    ls_emplt-ename = ls_pernr-ename.
*    APPEND ls_emplt TO et_emplt.
*    e_empcnt = e_empcnt + 1.
*
*    CLEAR: ls_documents, ls_emplt, ls_pernr.
*  ENDLOOP.



* set date
  CLEAR ls_sel_dates.
  ls_sel_dates-validity_from_date = i_begda.
  ls_sel_dates-validity_to_date = i_endda.

* set status
  CLEAR ls_sel_status.
  ls_sel_status-ap_status_4 = abap_true.
  ls_sel_status-ap_status_5 = abap_true.

  CLEAR ls_sel_with_or_without.
  ls_sel_with_or_without-sel_display_existing = abap_true.

  CLEAR ls_pernr.
  LOOP AT lt_pernr INTO ls_pernr.
*   set appraisee
    CLEAR: ls_appraisee, lt_appraisee.
    ls_appraisee-plvar = i_plan_version.
    ls_appraisee-otype = 'P'.
    ls_appraisee-sobid = ls_pernr-pernr.
    APPEND ls_appraisee TO lt_appraisee.

*   get appraisal documents list.
    CALL METHOD cl_hap_wd_start_page_ui=>document_get_list
      EXPORTING
        add_on_application    = 'PA'
        plan_version          = '01'
        t_categories          = it_categories
        t_appraisees          = lt_appraisee
        s_sel_dates           = ls_sel_dates
        s_sel_status          = ls_sel_status
*       t_sel_status_sub      =
        s_sel_with_or_without = ls_sel_with_or_without
      IMPORTING
        t_document            = lt_documents.

    CHECK lines( lt_documents ) > 0.
    SORT lt_documents BY ap_end_date DESCENDING.
    CLEAR: ls_documents.
    READ TABLE lt_documents INTO ls_documents
                            INDEX 1.
    IF ( ls_documents-ap_status = '4' AND ls_documents-ap_status_sub = 'J' ) OR
       ( ls_documents-ap_status = '4' AND ls_documents-ap_status_sub = 'K' ) OR
         ls_documents-ap_status = '5'.
*     export data setting
      MOVE: ls_documents-appraisee_id TO ls_emplt-pernr,
            ls_documents-appraisal_id TO ls_emplt-appraisal_id.
      READ TABLE lt_pernr INTO ls_pernr
                          WITH KEY pernr = ls_documents-appraisee_id
                          BINARY SEARCH.
      MOVE: ls_pernr-stell TO ls_emplt-stell,
            ls_pernr-ename TO ls_emplt-ename.
      APPEND ls_emplt TO et_emplt.
      e_empcnt = e_empcnt + 1.
    ELSE.
*     export data setting
      CLEAR: ls_documents.
      LOOP AT lt_documents INTO ls_documents.
        ls_emplt-pernr = ls_documents-appraisee_id.
        ls_emplt-appraisal_id = ls_documents-appraisal_id.
        READ TABLE lt_pernr INTO ls_pernr
                            WITH KEY pernr = ls_documents-appraisee_id
                            BINARY SEARCH.
        ls_emplt-stell = ls_pernr-stell.
        ls_emplt-ename = ls_pernr-ename.
        APPEND ls_emplt TO et_emplt.
        e_empcnt = e_empcnt + 1.

        CLEAR: ls_documents, ls_emplt, ls_pernr.
      ENDLOOP.

    ENDIF.

    CLEAR: ls_pernr, lt_documents.
  ENDLOOP.

ENDFUNCTION.
