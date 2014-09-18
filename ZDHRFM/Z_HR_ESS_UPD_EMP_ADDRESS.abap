FUNCTION z_hr_ess_upd_emp_address.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(ANSSA) TYPE  ANSSA DEFAULT 1
*"     VALUE(UPDATE) TYPE  CHAR1 OPTIONAL
*"     VALUE(BOTH) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      ZESS_EMP_ADDRESS STRUCTURE  ZESS_EMP_ADDRESS
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
*-----------------------------------------------------------------------
* 04/05/2012 Valerian   UD1K954438  Validate Phone Number for MyESS
*                                   (Should be 10 digits only)
* 08/01/2012 Valerian   UD1K955293  Use communication type "HOME" as
*                                   telephone number
* 08/01/2012 Valerian   UD1K955303  Extend ZESS_EMP_ADDRESS to store
*                                   Home Phone Number.
*-----------------------------------------------------------------------
* BEGIN OF UD1K954438
  DEFINE INVALID_PHONE.
    return-type = 'E'.
    return-message = 'Phone No. should be 10 digits only'.
    APPEND return.
    EXIT.
  END-OF-DEFINITION.
* END OF UD1K954438
* {
  DATA $CNT TYPE I.
  data : COMKY_xx type COMKY,
         COMNR_xx type COMNR,
         l_error(1) TYPE c.                                 "UD1K954438

  DATA return1 LIKE bapireturn1 OCCURS 0 WITH HEADER LINE.

  $CNT = 1.

  IF BOTH EQ 'X'.
    $CNT = 2.
  ENDIF.

  DO $CNT TIMES.

    IF SY-INDEX = 2.
      IF anssa EQ '1'.
        anssa = '5'.
      ELSE.
        anssa = '1'.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
         EXPORTING
              number = employee_number
         IMPORTING
              return = return1.

    IF return1-type EQ 'E'.
      return-type = 'I'.
      return-message = 'Record is locked. Please try again later.'.
      APPEND return.
      EXIT.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
         EXPORTING
              number = employee_number
         IMPORTING
              return = return1.
* }


    READ TABLE zess_emp_address INDEX 1.
    CHECK sy-subrc EQ 0.

* BEGIN OF UD1K954438
* BEGIN OF UD1K955303
    IF NOT zess_emp_address-home_num is initial.
      PERFORM validate_phone USING    zess_emp_address-home_num
                             CHANGING l_error.
*   IF NOT zess_emp_address-telephone is initial.
*     PERFORM validate_phone USING    zess_emp_address-telephone
*                            CHANGING l_error.
* END OF UD1K955303
      IF NOT l_error IS INITIAL.
        INVALID_PHONE.
      ENDIF.
    ENDIF.

    IF NOT zess_emp_address-cell_num  is initial.
      PERFORM validate_phone USING    zess_emp_address-cell_num
                             CHANGING l_error.
      IF NOT l_error IS INITIAL.
        INVALID_PHONE.
      ENDIF.
    ENDIF.

    IF NOT zess_emp_address-work_num  is initial.
      PERFORM validate_phone USING    zess_emp_address-work_num
                             CHANGING l_error.
      IF NOT l_error IS INITIAL.
        INVALID_PHONE.
      ENDIF.
    ENDIF.

    IF NOT zess_emp_address-wcel_num  is initial.
      PERFORM validate_phone USING    zess_emp_address-wcel_num
                             CHANGING l_error.
      IF NOT l_error IS INITIAL.
        INVALID_PHONE.
      ENDIF.
    ENDIF.
* END OF UD1K954438

    IF anssa IS INITIAL.
      anssa = '1'.
    ENDIF.

    SELECT SINGLE * FROM pa0006
                   WHERE pernr EQ employee_number
                     AND subty EQ anssa
                     AND anssa EQ anssa
                     AND endda EQ '99991231'.

    IF sy-subrc NE 0.
      SELECT SINGLE * FROM pa0006
                     WHERE pernr EQ employee_number
                       AND endda EQ '99991231'.

      IF sy-subrc NE 0.
        return-type = 'E'.
        return-message = 'Invalid Employee Number'.
        APPEND return.
        EXIT.
      ENDIF.

    ENDIF.

    CHECK update EQ true.

     *pa0006 = pa0006.
     *pa0006-endda = sy-datum - 1.

    DELETE FROM pa0006
                 WHERE pernr EQ employee_number
                   AND subty EQ anssa
                   AND anssa EQ anssa
                   AND begda = sy-datum.

    UPDATE pa0006 SET endda = *pa0006-endda
                   WHERE pernr EQ employee_number
                     AND subty EQ anssa
                     AND anssa EQ anssa
                     AND endda EQ '99991231'.

*  IF SY-SUBRC NE 0.
*    ROLLBACK WORK.
*    RETURN-TYPE = 'E'.
*    RETURN-MESSAGE = 'Error Occured when Update'.
*    APPEND RETURN.
*    EXIT.
*  ENDIF.

     *pa0006-name2 = zess_emp_address-care_of.
     *pa0006-stras = zess_emp_address-address1.
     *pa0006-locat = zess_emp_address-address2.
     *pa0006-ort01 = zess_emp_address-city.
     *pa0006-ort02 = zess_emp_address-county.
     *pa0006-state = zess_emp_address-state.
     *pa0006-pstlz = zess_emp_address-zip_code.

    if zess_emp_address-home_num is initial and             "UD1K955303
*   if zess_emp_address-telephone is initial and            "UD1K955303
       zess_emp_address-cell_num is initial and
       zess_emp_address-work_num is initial and
       zess_emp_address-wcel_num is initial.

*     zess_emp_address-telephone = *pa0006-telnr.           "UD1K955293

      do 6 times varying COMKY_xx FROM *pa0006-COM01 next *pa0006-com02
                 varying COMNR_xx FROM *pa0006-NUM01 next *pa0006-num02.

        case COMKY_xx.
          when 'CELL'.
            zess_emp_address-cell_num = COMNR_xx.
          when 'WORK'.
            zess_emp_address-work_num = COMNR_xx.
          when 'WCEL'.
            zess_emp_address-wcel_num = COMNR_xx.
          when 'HOME'.
            zess_emp_address-home_num = COMNR_xx.
        endcase.

      enddo.

    endif.


*    *pa0006-telnr = zess_emp_address-telephone.            "UD1K955303


    PERFORM clear_phone_field.

    IF NOT zess_emp_address-cell_num IS INITIAL.
      PERFORM fill_phone_to_empty_field USING 'CELL'
                                zess_emp_address-cell_num.
    ENDIF.

    IF NOT zess_emp_address-work_num IS INITIAL.
      PERFORM fill_phone_to_empty_field USING 'WORK'
                                zess_emp_address-work_num.
    ENDIF.

    IF NOT zess_emp_address-wcel_num IS INITIAL.
      PERFORM fill_phone_to_empty_field USING 'WCEL'
                                zess_emp_address-wcel_num.
    ENDIF.

* BEGIN OF UD1K955293
* BEGIN OF UD1K955303
    IF NOT zess_emp_address-home_num IS INITIAL.
      PERFORM fill_phone_to_empty_field USING 'HOME'
                                zess_emp_address-home_num.
    ENDIF.
*   IF NOT zess_emp_address-telephone IS INITIAL.
*     PERFORM fill_phone_to_empty_field USING 'HOME'
*                               zess_emp_address-telephone.
*   ENDIF.
* END OF UD1K955303
* END OF UD1K955293

     *pa0006-land1 = 'US'.
     *pa0006-begda = sy-datum.
     *pa0006-endda = '99991231'.
     *pa0006-aedtm = sy-datum.
     *pa0006-uname = sy-uname.
     *pa0006-anssa = anssa.
     *pa0006-subty = anssa.

    INSERT pa0006 FROM *pa0006.

    IF sy-subrc NE 0.

      DELETE FROM pa0006
                   WHERE pernr EQ *pa0006-pernr
                     AND subty EQ *pa0006-subty
                     AND anssa EQ *pa0006-anssa
                     AND endda = *pa0006-endda.

      INSERT pa0006 FROM *pa0006.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        return-type = 'E'.
        return-message = 'Error Occured when Insert'.
        APPEND return.
        EXIT.
      ENDIF.
    ENDIF.

    COMMIT WORK.

    return-type = 'S'.
    return-message = 'Success!'.
    APPEND return.

  ENDDO.

ENDFUNCTION.
