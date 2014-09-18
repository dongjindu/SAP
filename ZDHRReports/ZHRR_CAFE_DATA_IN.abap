*&----------------------------------------------------------------------
*& Program ID        : ZHR_CAFE_DATA_IN
*& Title             : [HR] - Cafeteria Inbound Data Transfer
*& Created by        : Hyunok Pak
*& Created on        : 05/20/2014
*& Specifications By : Grace Li
*& Reference Pgm     : N/A
*& Description       : This program is to receive inbound data from GEMPay
*&                     via EAI. After receiving data this program check the
*&                     validation and execute BDC for processing.
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*&
*&----------------------------------------------------------------------

REPORT  zhrr_cafe_data_in MESSAGE-ID zmhr.

TABLES: t549q.
DATA : lt_read          TYPE hrpad_prelp_tab,
       lt_readn         TYPE hrpad_prelp_tab,
       lv_pernr(8)      TYPE n,
       validitybegin    TYPE dats,
       validityend      TYPE dats.

DATA: gv_total_cnt      TYPE string,
      gv_error_cnt      TYPE string,
      gv_cnt            TYPE i.

DATA: gs_text           TYPE LINE OF bcsy_text,
      gt_e_text         TYPE bcsy_text.

DATA: gs_result         TYPE bapiret2.

DATA: g_return          TYPE zmms0053,
      ls_return         LIKE bapireturn1,
      lv_key            LIKE bapipakey,
      return            LIKE TABLE OF bapireturn WITH HEADER LINE.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zshr_cafe_in.
DATA:   begda           TYPE dats,
        endda           TYPE dats,
      END   OF it_data,

      is_cafe_in        LIKE LINE OF it_data.
DATA: is_0015           TYPE p0015.

DATA: BEGIN OF it_subtot OCCURS 0,
        pernr           TYPE pernr_d,
        betrg           TYPE pad_amt7s,
      END   OF it_subtot.

DATA: it_periods        LIKE t549q OCCURS 1,
      is_periods        LIKE LINE OF it_periods.
DATA: get_pabrj         LIKE t549q-pabrj,
      get_pabrp         LIKE t549q-pabrp,
      lv_last_day       TYPE dats,
      lv_flag           TYPE c
    .


DATA: gs_option LIKE ctu_params.
CONSTANTS: insert TYPE pspar-actio VALUE 'INS'.


SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE text-t01.
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE text-t01.
PARAMETERS: p_pernr(8),
            p_date        TYPE dats, "20140530 Add
            p_nachn(40) NO-DISPLAY,                         "20140530
            p_vorna(40) NO-DISPLAY,                         "20140530
            p_begda       TYPE dats NO-DISPLAY,
            p_endda       TYPE dats NO-DISPLAY,
            p_amont       TYPE char8.
*
SELECTION-SCREEN SKIP.
PARAMETERS: p_dest LIKE rzllitab-classname OBLIGATORY
                   DEFAULT 'WMHR01'.
*
SELECTION-SCREEN END OF BLOCK blk.

PARAMETERS: p_batch AS CHECKBOX.

SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_bperd FOR sy-datum NO-EXTENSION MODIF ID bpe.
SELECTION-SCREEN END OF BLOCK blk4.

INITIALIZATION.

*** 20140612 Request -> Grace
  PERFORM next_payroll_date USING sy-datum s_bperd-low.
  s_bperd-high = s_bperd-low.
  APPEND s_bperd.
***  s_bperd-high = s_bperd-low = sy-datum - 1.
  APPEND s_bperd.

START-OF-SELECTION.
  p_begda = s_bperd-low.
  p_endda = s_bperd-high.

  CASE 'X'.
    WHEN p_batch.
      validitybegin = sy-datum - 1.
      PERFORM next_payroll_date USING validitybegin lv_last_day.

      CALL FUNCTION 'ZFHR_CAFE_IN' DESTINATION p_dest
        IMPORTING
          e_return                       = g_return
        TABLES
          it_data                        = it_data
        EXCEPTIONS
          call_function_destination_no_t = 1
          call_function_no_dest          = 2
          call_function_remote_error     = 3
          rfc_no_authority               = 4
          OTHERS                         = 5.

      IF sy-subrc <> 0.
        MESSAGE i001 WITH g_return-message.
        EXIT.    "Marking & when testing
*        message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

**-- for Testing s
*      CLEAR: it_data[], it_data, it_subtot, it_subtot[].
*      it_data-pernr = '00103548'.
**      it_data-pernr = '00103548'.
*      it_data-nachn = 'ln103548'.
*      it_data-vorna = 'fn103548'.
***      it_data-begda = '05122014'.
*      WRITE '05122014' USING EDIT MASK '__/__/____' TO it_data-begda.
***      it_data-endda = '05252014'.
*      WRITE '05252014' USING EDIT MASK '__/__/____' TO it_data-endda.
*      it_data-tdate = '05222014'.
*      it_data-amont = '33.48'.
*      APPEND it_data.
*
*      CLEAR: it_data.
*      it_data-pernr = '00103548'.
*      it_data-pernr = '00100103'.
*      it_data-nachn = 'ln100103'.
*      it_data-vorna = 'fn100103'.
***      it_data-begda = '05122014'.
*      WRITE '05122014' USING EDIT MASK '__/__/____' TO it_data-begda.
***      it_data-endda = '05252014'.
*      WRITE '05252014' USING EDIT MASK '__/__/____' TO it_data-endda.
*      it_data-tdate = '05222014'.
*      it_data-amont = '33.03'.
*      APPEND it_data.
*
*      CLEAR: it_data.
*      it_data-pernr = '00103548'.
**      it_data-pernr = '00100170'.
*      it_data-nachn = 'ln100170'.
*      it_data-vorna = 'fn100170'.
***      it_data-begda = '05122014'.
*      WRITE '05122014' USING EDIT MASK '__/__/____' TO it_data-begda.
***      it_data-endda = '05252014'.
*      WRITE '05252014' USING EDIT MASK '__/__/____' TO it_data-endda.
*      it_data-tdate = '05222014'.
*      it_data-amont = '33.70'.
*      APPEND it_data.
*
*      CLEAR: it_data.
*      it_data-pernr = '00100171'.
**      it_data-pernr = '00100171'.
*      it_data-nachn = 'ln100171'.
*      it_data-vorna = 'fn100171'.
***      it_data-begda = '05122014'.
*      WRITE '05122014' USING EDIT MASK '__/__/____' TO it_data-begda.
***      it_data-endda = '05252014'.
*      WRITE '05252014' USING EDIT MASK '__/__/____' TO it_data-endda.
*      it_data-tdate = '05222014'.
*      it_data-amont = '33.71'.
*      APPEND it_data.
*
*      CLEAR: it_data.
*      it_data-pernr = '00100171'.
**      it_data-pernr = '00100101'.
*      it_data-nachn = 'ln100101'.
*      it_data-vorna = 'fn100101'.
***      it_data-begda = '05122014'.
*      WRITE '05122014' USING EDIT MASK '__/__/____' TO it_data-begda.
***      it_data-endda = '05252014'.
*      WRITE '05252014' USING EDIT MASK '__/__/____' TO it_data-endda.
*      it_data-tdate = '05222014'.
*      it_data-amont = '33.01'.
*      APPEND it_data.
*
**-- for test e

      DESCRIBE TABLE it_data LINES gv_cnt. "gv_total_cnt.

      SORT it_data BY pernr tdate.
      LOOP AT it_data INTO is_cafe_in.
        IF is_cafe_in-pernr IS INITIAL.
          EXIT.
        ENDIF.
        CLEAR: it_subtot.
        it_subtot-pernr = is_cafe_in-pernr.
        it_subtot-betrg = is_cafe_in-amont.
        COLLECT it_subtot.
      ENDLOOP.

      SORT it_subtot BY pernr.
      LOOP AT it_subtot.
        READ TABLE it_data WITH KEY pernr = it_subtot-pernr
                                     INTO is_cafe_in.
        IF sy-subrc NE 0.
          CLEAR return.
          return-type = 'E'.
          CONCATENATE it_subtot-pernr ': Error Subtotal Internal table.'
            INTO return-message.
          CONDENSE return-message NO-GAPS.
          APPEND return.
          CONTINUE.
        ENDIF.

        lv_pernr = it_subtot-pernr.
        READ TABLE it_periods INDEX 1 INTO is_periods.

        "This code is requred and locks the record ready for modification
        lv_flag = 'E'.
        PERFORM employee_enqueue USING lv_flag lv_pernr.

        is_0015-pernr = lv_pernr.
        is_0015-endda = s_bperd-low.
        is_0015-begda = s_bperd-high.
        is_0015-lgart = '3050'.          " lv_wagetype'.
        is_0015-betrg = it_subtot-betrg. " is_cafe_in-amont.
        is_0015-waers = 'USD'.           " lv_currency.
        is_0015-opken = 'A'.
*        is_0015-model = '    '.
        is_0015-subty = '3050'.
        is_0015-seqnr = '000'.

        "plus populate any other fields you need to update
        PERFORM insert_record USING p_batch.
        IF ls_return IS NOT INITIAL.
          return-type = 'E'.
          CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
          CONDENSE ls_return-id NO-GAPS.

          return-message = ls_return-message.
          APPEND return.
        ELSE.
          COMMIT WORK.
          return-type = 'S'.
          CONCATENATE 'Success :' lv_key INTO return-message
                      SEPARATED BY space.
*        return-message = 'Success!'.
          APPEND return.
        ENDIF.

        "unlock record after modification
        lv_flag = 'D'.
        PERFORM employee_enqueue USING lv_flag lv_pernr.

      ENDLOOP.

    WHEN OTHERS.
      it_data-pernr = p_pernr.
      it_data-nachn = p_nachn.
      it_data-vorna = p_vorna.
      it_data-begda = p_begda.
      it_data-endda = p_endda.
      IF NOT p_begda IS INITIAL.
        WRITE p_begda USING EDIT MASK '__/__/____' TO it_data-begda.
      ENDIF.
      IF NOT p_endda IS INITIAL.
        WRITE p_endda USING EDIT MASK '__/__/____' TO it_data-endda.
      ENDIF.
      it_data-amont = p_amont.

      gv_total_cnt = 1.
      CLEAR gs_result.

      PERFORM check_validation USING it_data CHANGING gs_result.
      IF gs_result-type EQ 'S'.

        lv_pernr = p_pernr.

        it_data-begda = p_begda.
        it_data-endda = p_endda.

        CLEAR: it_periods[], it_periods, lv_last_day.
        validitybegin = sy-datum - 1.
        PERFORM next_payroll_date USING validitybegin lv_last_day.
        READ TABLE it_periods INDEX 1 INTO is_periods.


        "This code is requred and locks the record ready for modification
        lv_flag = 'E'.
        PERFORM employee_enqueue USING lv_flag lv_pernr.

        is_0015-pernr = lv_pernr.
        is_0015-endda = it_data-endda. "'99991231'.
        is_0015-begda = it_data-begda. "is_periods-begda. "sy-datum.
        is_0015-lgart = '3050'. " lv_wagetype'.
        is_0015-betrg = p_amont.
        is_0015-waers = 'USD'. " lv_currency.
        is_0015-opken = 'A'.
*        is_0015-model = '    '.
        is_0015-subty = '3050'.
        is_0015-seqnr = '000'.

        "plus populate any other fields you need to update
        PERFORM insert_record USING p_batch.

        IF ls_return IS NOT INITIAL.
          return-type = 'E'.
          CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
          CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
          return-message = ls_return-message.
          APPEND return.
        ELSE.
          COMMIT WORK.
          return-type = 'S'.
          CONCATENATE 'Success :' lv_key INTO return-message
                      SEPARATED BY space.
*        return-message = 'Success!'.
          APPEND return.
        ENDIF.

        "unlock record after modification
        lv_flag = 'D'.
        PERFORM employee_enqueue USING lv_flag lv_pernr.

      ENDIF.

  ENDCASE.



*&---------------------------------------------------------------------*
*&      Form  check_validation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA     text
*----------------------------------------------------------------------*
FORM check_validation USING p_data STRUCTURE zshr_cafe_in
                   CHANGING c_result STRUCTURE bapiret2.

  c_result-type ='S'.

ENDFORM.                    "check_validation



*&---------------------------------------------------------------------*
*&      Form  APPEND_MSG_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TYPE     text
*      -->P_MSG      text
*----------------------------------------------------------------------*
FORM append_msg_list USING p_type p_msg.

*  CLEAR st_msg_list.
*
*  PERFORM divide_message  USING p_msg
*                                     st_msg_list-msgv1
*                                     st_msg_list-msgv2 .
*
*  st_msg_list-msgid = 'ZKUSSD'.
*  st_msg_list-msgty = p_type.
*  st_msg_list-msgno = '001'.
*  APPEND st_msg_list TO it_msg_list.

ENDFORM.                    "APPEND_MSG_LIST



*&---------------------------------------------------------------------*
*&      Form  DIVIDE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MESSAGE  text
*      -->P_MSG1     text
*      -->P_MSG2     text
*----------------------------------------------------------------------*
FORM divide_message USING p_message p_msg1 p_msg2.

  DATA: l_segment  TYPE i,
        l_offset   TYPE i,
        l_cnt      TYPE i.

  l_cnt = strlen( p_message ).

  IF l_cnt > 50.
    CALL FUNCTION 'TRUNCATE_MULTIPLE_BYTE_STRING'
      EXPORTING
        string        = p_message
        target_length = 50
      IMPORTING
        use_length    = l_segment.

    l_offset = l_cnt - l_segment + 1.
    p_msg1   = p_message+0(l_segment).
    p_msg2   = p_message+l_segment(l_offset).
  ELSE.
    p_msg1   = p_message.
  ENDIF.

ENDFORM.                    "DIVIDE_MESSAGE



*&---------------------------------------------------------------------*
*&      Form  NEXT_PAYROLL_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DATUM  text
*      -->P_S_BPERD_LOW  text
*----------------------------------------------------------------------*
FORM next_payroll_date  USING    p_datum
                                 p_endda.

  p_datum = p_datum - 1.
  CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
    EXPORTING
      get_begda       = p_datum "validitybegin
      get_permo       = '04' "RPTIME_PERIOD
    IMPORTING
      get_pabrj       = get_pabrj
      get_pabrp       = get_pabrp
    TABLES
      get_periods     = it_periods
    EXCEPTIONS
      no_period_found = 1
      no_valid_permo  = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE it_periods INDEX 1 INTO is_periods.
  p_endda = is_periods-endda.

ENDFORM.                    " NEXT_PAYROLL_DATE



*&---------------------------------------------------------------------*
*&      Form  EMPLOYEE_ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PERNR  text
*----------------------------------------------------------------------*
FORM employee_enqueue  USING   p_flag p_pernr.

  IF p_flag = 'E'.
    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = p_pernr.
  ELSE.
    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = p_pernr.
  ENDIF.

ENDFORM.                    " EMPLOYEE_ENQUEUE



*&---------------------------------------------------------------------*
*&      Form  INSERT_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BATCH  text
*----------------------------------------------------------------------*
FORM insert_record  USING    p_batch.

  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty         = '0015'
      subtype       = is_0015-subty
      number        = lv_pernr     "employeenumber
      validityend   = it_data-endda
      validitybegin = it_data-begda
      record        = is_0015
      operation     = insert
      nocommit      = 'A' "nocommit
      dialog_mode   = '0'
    IMPORTING
      return        = ls_return
      key           = lv_key
    EXCEPTIONS
      OTHERS        = 0.

ENDFORM.                    " INSERT_RECORD
