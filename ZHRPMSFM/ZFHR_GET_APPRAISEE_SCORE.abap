FUNCTION zfhr_get_appraisee_score.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_PLAN_VERSION) TYPE  HAP_PLAN_VERSION
*"     REFERENCE(IT_EMPLT) TYPE  ZTYHR_EMPLT
*"  EXPORTING
*"     REFERENCE(ET_EMPLT) TYPE  ZTYHR_EMPLT
*"----------------------------------------------------------------------

  DATA: lt_body_columns         TYPE hap_t_body_columns,
        lt_body_elements        TYPE hap_t_body_elements,
        lt_body_elements_old    TYPE hap_t_body_elements,
        lt_body_cells           TYPE hap_t_body_cells,
        lt_hrp9873              TYPE TABLE OF hrp9873,
        lt_hrp9873_old          TYPE TABLE OF hrp9873,
        lt_emplt                LIKE it_emplt.

  DATA: ls_emplt                LIKE LINE OF it_emplt,
        ls_appraisal_id         TYPE hap_s_appraisal_id,
        ls_header_status        TYPE hap_s_header_status,
        ls_header_dates         TYPE hap_s_header_dates,
        ls_body_columns         TYPE hap_s_body_columns,
        ls_body_elements        TYPE hap_s_body_elements,
        ls_body_cells           TYPE hap_s_body_cells,
        ls_hrp9873              TYPE hrp9873.

  DATA: BEGIN OF ls_list,
          pernr	                TYPE persno,
          stell	                TYPE stell,
          ename                 TYPE emnam,
          appraisal_id          TYPE hap_appraisal_id,
          status                TYPE hap_ap_status,
          substatus             TYPE hap_ap_status_sub,
          start_date            TYPE hap_ap_start_date,
          end_date              TYPE hap_ap_end_date,
          score	                TYPE zdhr_score,
        END OF ls_list,
        lt_list                 LIKE TABLE OF ls_list.

  DATA: BEGIN OF ls_dates,
          pernr                 TYPE persno,
          days                  TYPE i,
        END OF ls_dates,
        lt_dates                LIKE TABLE OF ls_dates.

  DATA: BEGIN OF ls_score,
          pernr                 TYPE persno,
          score                 TYPE p LENGTH 15 DECIMALS 3,
        END OF ls_score,
        lt_score                LIKE TABLE OF ls_score.

  DATA: BEGIN OF ls_count,
          pernr                 TYPE persno,
          count                 TYPE i,
        END OF ls_count,
        lt_count                LIKE TABLE OF ls_count.

  DATA: l_column_zp16_iid       TYPE hap_column_iid,
        l_tabix                 TYPE sy-tabix,
        l_days                  TYPE int4.

  CHECK lines( it_emplt ) > 0.

* get role id & myey of vc object
  SELECT * FROM hrp9873
    INTO TABLE lt_hrp9873_old
    WHERE plvar = '01'
      AND otype = 'VC'
      AND istat IN ('3', '4')
      AND begda <= sy-datum
      AND endda >= sy-datum
*      AND role_id IN ('Z0', 'Z7', 'Z9')
      AND role_id IN ('Z0', 'Z7')
      AND myey = 'EY'.
  SORT lt_hrp9873_old BY objid.

  LOOP AT it_emplt INTO ls_emplt.
*   set appraisal id
    ls_appraisal_id-appraisal_id = ls_emplt-appraisal_id.

*   get appraisal document detail
    CALL FUNCTION 'HRHAP_DOCUMENT_GET_DETAIL'
      EXPORTING
        plan_version     = i_plan_version
        s_appraisal_id   = ls_appraisal_id
      IMPORTING
        s_header_status	 = ls_header_status
        s_header_dates   = ls_header_dates
        t_body_columns   = lt_body_columns
        t_body_elements  = lt_body_elements_old
        t_body_cells     = lt_body_cells.

    IF lt_body_elements_old[] IS INITIAL.
      CONTINUE.
    ENDIF.

*   set column iid of final score
    READ TABLE lt_body_columns INTO ls_body_columns
                               WITH KEY column_id = 'ZP16'.
    IF sy-subrc = 0.
      l_column_zp16_iid = ls_body_columns-column_iid.
    ENDIF.

    DELETE lt_body_elements_old WHERE element_type <> 'VC'.
    LOOP AT lt_body_elements_old INTO ls_body_elements.
      READ TABLE lt_hrp9873_old INTO ls_hrp9873
                                WITH KEY objid = ls_body_elements-element_id
                                BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND ls_hrp9873 TO lt_hrp9873.
        APPEND ls_body_elements TO lt_body_elements.
      ENDIF.

      CLEAR: ls_body_elements, ls_hrp9873, ls_body_elements.
    ENDLOOP.

    SORT lt_hrp9873 BY role_id.
    SORT lt_body_elements BY element_type element_id.

    LOOP AT lt_hrp9873 INTO ls_hrp9873.
      READ TABLE lt_body_elements INTO ls_body_elements
                                  WITH KEY element_type = ls_hrp9873-otype
                                           element_id = ls_hrp9873-objid
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_body_cells INTO ls_body_cells
                                 WITH KEY row_iid = ls_body_elements-row_iid
                                          column_iid = l_column_zp16_iid
                                 BINARY SEARCH.
        IF sy-subrc = 0 AND ls_body_cells-value_num > 0.
*         set final score
          ls_emplt-score = ls_body_cells-value_num.
        ENDIF.
      ENDIF.

      CLEAR: ls_hrp9873, ls_body_elements, ls_body_cells.
    ENDLOOP.

*    MODIFY it_emplt FROM ls_emplt TRANSPORTING score.
    APPEND ls_emplt TO lt_emplt.
    MOVE-CORRESPONDING ls_emplt TO ls_list.
    MOVE: ls_header_status-ap_status TO ls_list-status,
          ls_header_status-ap_status_sub TO ls_list-substatus,
          ls_header_dates-ap_start_date TO ls_list-start_date,
          ls_header_dates-ap_end_date TO ls_list-end_date.
    APPEND ls_list TO lt_list.

    CLEAR: ls_emplt,
           lt_body_columns,
           lt_body_elements_old,
           lt_body_elements,
           lt_body_cells,
           lt_hrp9873,
           ls_list.
  ENDLOOP.

  SORT lt_emplt BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_emplt COMPARING pernr.

* get total days
  CLEAR: lt_dates, ls_emplt.
  LOOP AT lt_emplt INTO ls_emplt.
    LOOP AT lt_list INTO ls_list
                    WHERE pernr = ls_emplt-pernr.
      ls_dates-pernr = ls_list-pernr.
      CLEAR l_days.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = ls_list-start_date
          i_date_to   = ls_list-end_date
        IMPORTING
          e_days      = l_days.
      l_days = l_days + 1.
      ls_dates-days = l_days.
      COLLECT ls_dates INTO lt_dates.

      CLEAR: ls_list, ls_dates.
    ENDLOOP.

    CLEAR: ls_emplt.
  ENDLOOP.

  SORT lt_dates BY pernr.
  SORT lt_list BY pernr start_date.

* get total score, template count
  CLEAR: lt_score, lt_count, ls_list.
  LOOP AT lt_list INTO ls_list.
    READ TABLE lt_dates INTO ls_dates
                        WITH KEY pernr = ls_list-pernr
                        BINARY SEARCH.
    IF ls_dates IS INITIAL.
      CONTINUE.
    ENDIF.
    ls_score-pernr = ls_list-pernr.
    CLEAR l_days.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = ls_list-start_date
        i_date_to   = ls_list-end_date
      IMPORTING
        e_days      = l_days.
    l_days = l_days + 1.
    IF ls_dates-days > 0.
      ls_score-score = ls_list-score * l_days / ls_dates-days.
    ENDIF.

    COLLECT ls_score INTO lt_score.

    MOVE: ls_score-pernr TO ls_count-pernr.
    ls_count-count = 1.
    COLLECT ls_count INTO lt_count.

*    AT END OF pernr.
*      IF ( ls_list-status = '4' AND ls_list-substatus = 'J' ) OR
*         ( ls_list-status = '4' AND ls_list-substatus = 'K' ) OR
*           ls_list-status = '5'.
*
*      ENDIF.
*    ENDAT.

    CLEAR: ls_list, ls_dates, ls_score, ls_count, l_days.
  ENDLOOP.

  LOOP AT lt_emplt INTO ls_emplt.
    l_tabix = sy-tabix.
    READ TABLE lt_score INTO ls_score
                        WITH KEY pernr = ls_emplt-pernr.
    READ TABLE lt_count INTO ls_count
                        WITH KEY pernr = ls_emplt-pernr.
    IF ls_count-count = 1.
      READ TABLE lt_list INTO ls_list
                         WITH KEY pernr = ls_emplt-pernr.
      IF ls_list-status EQ '5' AND ls_list-end_date+4(4) NE '1231'.
        ls_emplt-unchange = 'X'.
      ENDIF.
    ENDIF.

    ls_emplt-score = ls_score-score.

    MODIFY lt_emplt FROM ls_emplt INDEX l_tabix.

    CLEAR: ls_emplt, ls_score.
  ENDLOOP.

  et_emplt[] = lt_emplt[].

ENDFUNCTION.
