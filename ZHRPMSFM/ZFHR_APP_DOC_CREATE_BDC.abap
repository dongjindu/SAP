FUNCTION zfhr_app_doc_create_bdc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_RECORD STRUCTURE  ZSHR_ADC_BDC
*"----------------------------------------------------------------------

  DATA: ls_appraisal_id           TYPE hap_s_appraisal_id,
        ls_header_texts	          TYPE hap_s_header_texts,
        ls_header_status          TYPE hap_s_header_status,
        lt_header_add_data        TYPE hap_t_header_add_data,
        ls_header_display         TYPE hap_s_header_display,
        lt_buttons                TYPE hap_t_buttons,
        lt_body_columns           TYPE hap_t_body_columns,
        lt_body_elements          TYPE hap_t_body_elements,
        lt_body_element_descr     TYPE hap_t_body_element_descr,
        lt_body_element_buttons   TYPE hap_t_body_element_buttons,
        lt_body_cells             TYPE hap_t_body_cells,
        lt_body_cell_val_values   TYPE hap_t_body_cell_val_values,
        lt_body_cell_val_ranges   TYPE hap_t_body_cell_val_ranges,
        lt_body_cell_val_c_like   TYPE hap_t_body_cell_val_c_like,
        lt_body_cell_val_descr    TYPE hap_t_body_cell_val_descr,
        lt_body_cell_notes        TYPE hap_t_body_cell_notes,
        lt_status_description     TYPE hap_t_status_description,
        ls_return                 TYPE bal_s_msg,
        ls_doc_processing         TYPE hap_s_doc_processing,
        lt_header_appraiser       TYPE hap_t_header_appraiser,
        ls_header_appraiser       TYPE hap_s_header_appraiser,
        lt_header_appraisee       TYPE hap_t_header_appraisee,
        lt_header_part_appraisers TYPE hap_t_header_part_appraisers,
        ls_header_appraisee       TYPE hap_s_header_appraisee,
        lt_header_others          TYPE hap_t_header_others,
        ls_header_others          TYPE hap_s_header_others,
        ls_header_dates           TYPE hap_s_header_dates,
        lt_status_notes	          TYPE hap_t_status_note.

  DATA: ls_t100                   TYPE t100.

  DATA: l_reccnt                  TYPE n LENGTH 6,
        l_rectot                  TYPE i,
        l_percent                 TYPE i,
        l_indtxt                  TYPE char40,
        l_begda                   TYPE sy-datum,
        l_endda                   TYPE sy-datum,
        l_plans                   TYPE plans,
        l_stell                   TYPE stell,
        l_grade                   TYPE zdhrgrade,
        l_apptem                  TYPE zdhr_apptem,
        l_message                 TYPE string,
        l_index                   TYPE sy-tabix,
        l_appee_pernr             TYPE persno.


  CHECK lines( it_record ) > 0.


  DESCRIBE TABLE it_record LINES l_rectot.

  CLEAR it_record.
  LOOP AT it_record WHERE error <> 'E'.
    l_index = sy-tabix.

*   progress indicator
    l_reccnt = l_reccnt + 1.
    l_percent = ( l_reccnt / l_rectot ) * 100.
    CONCATENATE '[' l_reccnt '] BDC ###.....' it_record-appee INTO l_indtxt.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percent
        text       = l_indtxt
      EXCEPTIONS
        OTHERS     = 0.

*   conversion begda, endda
    CLEAR: l_begda, l_endda.
    l_begda = it_record-begda.
    l_endda = it_record-endda.
*    CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'
*      EXPORTING
*        input  = it_record-begda
*      IMPORTING
*        output = l_begda.
*    CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'
*      EXPORTING
*        input  = it_record-endda
*      IMPORTING
*        output = l_endda.

*   get position and job
    CLEAR: l_appee_pernr, l_plans, l_stell.
    l_appee_pernr = it_record-appee.
    CALL FUNCTION 'ZFHR_GET_JOB_POSITION'
      EXPORTING
        i_pernr = l_appee_pernr
        i_begda = l_begda
        i_endda = l_endda
      IMPORTING
        e_plans = l_plans
        e_stell = l_stell.

*   get grade
    CLEAR: l_grade.
    SELECT SINGLE grade
      FROM hrp9870
      INTO l_grade
      WHERE plvar = '01'
        AND otype = 'S'
        AND objid = l_plans
        AND istat = '1'
        AND begda <= l_begda
        AND endda >= l_begda.

    IF l_grade IS INITIAL.
*     error type
      it_record-error = 'E'.
*     error message
      l_message = cl_hr_get_message_jp=>get_message( msgid = 'ZMHRPMS'
                                                     msgnr = 25 ).
      it_record-etext = l_message.
      MODIFY it_record INDEX l_index
                       TRANSPORTING error etext.
      CLEAR it_record.
      CONTINUE.
    ENDIF.

*   get appraisal template id
    CLEAR: l_apptem.
    SELECT SINGLE apptem
      FROM zthr_gaj01
      INTO l_apptem
      WHERE grade = l_grade
        AND job   = l_stell
        and ( zefrom <= l_begda
          and zeto >= l_begda ).
    IF l_apptem IS INITIAL.
      SELECT SINGLE apptem
        FROM zthr_gaj01
        INTO l_apptem
        WHERE grade = l_grade
          AND job   = ' '
          and ( zefrom <= l_begda
            and zeto >= l_begda ).
    ENDIF.

    IF l_apptem IS INITIAL.
*     error type
      it_record-error = 'E'.
*     error message
      l_message = cl_hr_get_message_jp=>get_message( msgid = 'ZMHRPMS'
                                                     msgnr = 23 ).
      it_record-etext = l_message.
      MODIFY it_record INDEX l_index
                       TRANSPORTING error etext.
      CLEAR it_record.
      CONTINUE.
    ENDIF.

*   get appraisal template detail
    CLEAR: ls_appraisal_id,
           ls_header_texts,
           ls_header_status,
           lt_header_add_data,
           ls_header_display,
           lt_buttons,
           lt_body_columns,
           lt_body_elements,
           lt_body_element_descr,
           lt_body_element_buttons,
           lt_body_cells,
           lt_body_cell_val_values,
           lt_body_cell_val_ranges,
           lt_body_cell_val_c_like,
           lt_body_cell_val_descr,
           lt_body_cell_notes,
           lt_status_description,
           ls_return.
    CALL FUNCTION 'HRHAP_TEMPLATE_GET_DETAIL'
      EXPORTING
        plan_version             = '01'
        template_id              = l_apptem
      IMPORTING
        s_appraisal_id           = ls_appraisal_id
        s_header_texts           = ls_header_texts
        s_header_status          = ls_header_status
        t_header_add_data        = lt_header_add_data
        s_header_display         = ls_header_display
        t_buttons                = lt_buttons
        t_body_columns           = lt_body_columns
        t_body_elements          = lt_body_elements
        t_body_element_descr     = lt_body_element_descr
        t_body_element_buttons   = lt_body_element_buttons
        t_body_cells             = lt_body_cells
        t_body_cell_val_values   = lt_body_cell_val_values
        t_body_cell_val_ranges   = lt_body_cell_val_ranges
        t_body_cell_val_c_like   = lt_body_cell_val_c_like
        t_body_cell_val_descr    = lt_body_cell_val_descr
        t_body_cell_notes        = lt_body_cell_notes
        t_status_description     = lt_status_description
        s_return                 = ls_return
      CHANGING
        s_doc_processing         = ls_doc_processing
        t_header_appraiser       = lt_header_appraiser
        t_header_appraisee       = lt_header_appraisee
        t_header_part_appraisers = lt_header_part_appraisers
        t_header_others          = lt_header_others
        s_header_dates           = ls_header_dates.

    IF ls_return-msgty EQ 'E'.
*     error type
      it_record-error = ls_return-msgty.
*     error message
      SELECT SINGLE * FROM t100
        INTO ls_t100
        WHERE sprsl = sy-langu
          AND arbgb = ls_return-msgid
          AND msgnr = ls_return-msgno.
      IF sy-subrc = 0.
        it_record-etext = ls_t100-text.
        IF it_record-etext CS '&1'.
          REPLACE '&1' WITH ls_return-msgv1 INTO it_record-etext.
          REPLACE '&2' WITH ls_return-msgv2 INTO it_record-etext.
          REPLACE '&3' WITH ls_return-msgv3 INTO it_record-etext.
          REPLACE '&4' WITH ls_return-msgv4 INTO it_record-etext.
        ELSE.
          REPLACE '&' WITH ls_return-msgv1 INTO it_record-etext.
          REPLACE '&' WITH ls_return-msgv2 INTO it_record-etext.
          REPLACE '&' WITH ls_return-msgv3 INTO it_record-etext.
          REPLACE '&' WITH ls_return-msgv4 INTO it_record-etext.
        ENDIF.
        CONDENSE it_record-etext.
      ENDIF.

      MODIFY it_record INDEX l_index
                       TRANSPORTING error etext.
      CLEAR it_record.
      CONTINUE.
    ENDIF.

*   set appraisee
    CLEAR: lt_header_appraisee, ls_header_appraisee.
    IF it_record-appee IS NOT INITIAL AND it_record-appee NE '00000000'.
      ls_header_appraisee-plan_version = '01'.
      ls_header_appraisee-type = 'P'.
      ls_header_appraisee-id = it_record-appee.
      APPEND ls_header_appraisee TO lt_header_appraisee.CLEAR ls_header_appraisee.
    ENDIF.
*   set appraiser
    CLEAR: lt_header_appraiser, ls_header_appraiser.
    IF it_record-apper IS NOT INITIAL AND it_record-apper NE '00000000'.
      ls_header_appraiser-plan_version = '01'.
      ls_header_appraiser-type = 'P'.
      ls_header_appraiser-id = it_record-apper.
      APPEND ls_header_appraiser TO lt_header_appraiser.CLEAR ls_header_appraiser.
    ENDIF.
*   set 1st coordinator
    CLEAR: lt_header_others, ls_header_others.
    IF it_record-cori1 IS NOT INITIAL AND it_record-cori1 NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-cori1.
      ls_header_others-role_id = 'Z2'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.
*   set 2nd evaluation
    IF it_record-evai2 IS NOT INITIAL AND it_record-evai2 NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-evai2.
      ls_header_others-role_id = 'Z3'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.
*   set 2nd coordinator
    IF it_record-cori2 IS NOT INITIAL AND it_record-cori2 NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-cori2.
      ls_header_others-role_id = 'Z4'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.
*   set 3rd evaluation
    IF it_record-evai3 IS NOT INITIAL AND it_record-evai3 NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-evai3.
      ls_header_others-role_id = 'Z5'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.
*   set 3rd coordinator
    IF it_record-cori3 IS NOT INITIAL AND it_record-cori3 NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-cori3.
      ls_header_others-role_id = 'Z6'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.
*   set approver
    IF it_record-appri IS NOT INITIAL AND it_record-appri NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-appri.
      ls_header_others-role_id = 'Z7'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.
*   set 4th coordinator
    IF it_record-cori4 IS NOT INITIAL AND it_record-cori4 NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-cori4.
      ls_header_others-role_id = 'Z8'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.
*   set hr team
    IF it_record-hrtmi IS NOT INITIAL AND it_record-hrtmi NE '00000000'.
      ls_header_others-plan_version = '01'.
      ls_header_others-type = 'P'.
      ls_header_others-id = it_record-hrtmi.
      ls_header_others-role_id = 'Z9'.
      APPEND ls_header_others TO lt_header_others.CLEAR ls_header_others.
    ENDIF.

*   set start, end date
    CLEAR ls_header_dates.
    ls_header_dates-ap_start_date = l_begda.
    ls_header_dates-ap_end_date = l_endda.

*   create document
    CLEAR: ls_return.
    CALL FUNCTION 'HRHAP_DOCUMENT_CHANGE_STATUS'
      EXPORTING
        plan_version           = '01'
        template_id            = l_apptem
        button_id              = 'OBJECTIVE'
      IMPORTING
        s_return               = ls_return
      CHANGING
        s_appraisal_id         = ls_appraisal_id
        s_header_status        = ls_header_status
        s_doc_processing       = ls_doc_processing
        s_header_display       = ls_header_display
        t_header_appraiser     = lt_header_appraiser
        t_header_appraisee     = lt_header_appraisee
        t_header_p_appraiser   = lt_header_part_appraisers
        t_header_others        = lt_header_others
        t_header_add_data      = lt_header_add_data
        s_header_texts         = ls_header_texts
        s_header_dates         = ls_header_dates
        t_buttons              = lt_buttons
        t_body_elements        = lt_body_elements
        t_body_element_descr   = lt_body_element_descr
        t_body_element_buttons = lt_body_element_buttons
        t_body_columns         = lt_body_columns
        t_body_cells           = lt_body_cells
        t_body_cell_val_values = lt_body_cell_val_values
        t_body_cell_val_ranges = lt_body_cell_val_ranges
        t_body_cell_val_c_like = lt_body_cell_val_c_like
        t_body_cell_val_descr  = lt_body_cell_val_descr
        t_body_cell_notes      = lt_body_cell_notes.

    IF ls_return-msgty EQ 'E'.
*     error type
      it_record-error = ls_return-msgty.
*     error message
      SELECT SINGLE * FROM t100
        INTO ls_t100
        WHERE sprsl = sy-langu
          AND arbgb = ls_return-msgid
          AND msgnr = ls_return-msgno.
      IF sy-subrc = 0.
        it_record-etext = ls_t100-text.
        IF it_record-etext CS '&1'.
          REPLACE '&1' WITH ls_return-msgv1 INTO it_record-etext.
          REPLACE '&2' WITH ls_return-msgv2 INTO it_record-etext.
          REPLACE '&3' WITH ls_return-msgv3 INTO it_record-etext.
          REPLACE '&4' WITH ls_return-msgv4 INTO it_record-etext.
        ELSE.
          REPLACE '&' WITH ls_return-msgv1 INTO it_record-etext.
          REPLACE '&' WITH ls_return-msgv2 INTO it_record-etext.
          REPLACE '&' WITH ls_return-msgv3 INTO it_record-etext.
          REPLACE '&' WITH ls_return-msgv4 INTO it_record-etext.
        ENDIF.
        CONDENSE it_record-etext.
      ENDIF.

      MODIFY it_record INDEX l_index
                       TRANSPORTING error etext.
      CLEAR it_record.
      CONTINUE.
    ENDIF.

    it_record-error = 'S'.
    MODIFY it_record INDEX l_index
                     TRANSPORTING error.

    CLEAR: it_record.
  ENDLOOP.

ENDFUNCTION.
