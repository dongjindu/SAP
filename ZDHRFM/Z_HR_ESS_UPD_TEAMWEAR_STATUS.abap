FUNCTION z_hr_ess_upd_teamwear_status.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(DATUM) TYPE  DATUM DEFAULT SY-DATUM
*"  TABLES
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
  TABLES *pa0019.

  DATA $pernr TYPE persno.

  SELECT SINGLE pernr INTO $pernr FROM pa0019 WHERE pernr EQ
 employee_number.

  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'Invalid TM#'.
    APPEND return.
  ELSE.

    SELECT SINGLE * FROM pa0019 INTO *pa0019
        WHERE pernr EQ employee_number
        AND termn <= datum
        AND bvmrk EQ ' '
        AND SUBTY EQ 'ZT'.

    IF sy-subrc EQ 0.

      SELECT SINGLE * FROM pa0001 WHERE pernr EQ employee_number
                             AND endda EQ '99991231'.

      IF sy-subrc EQ 0.

        IF pa0001-btrtl EQ '0003'.

          IF pa0001-kostl EQ 'MXPX10' OR
             pa0001-kostl EQ 'MXPX20' OR
             pa0001-kostl EQ 'MXPX30' OR
             pa0001-kostl EQ '0000055003'.

            CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
                 EXPORTING
                      months  = '24'
                      olddate = *pa0019-begda
                 IMPORTING
                      newdate = *pa0019-begda.

          ELSE.
            CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
                 EXPORTING
                      months  = '18'
                      olddate = *pa0019-begda
                 IMPORTING
                      newdate = *pa0019-begda.


          ENDIF.

        ELSE.
          CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
               EXPORTING
                    months  = '24'
                    olddate = *pa0019-begda
               IMPORTING
                    newdate = *pa0019-begda.

        ENDIF.

        CALL FUNCTION 'CALCULATE_DATE'
             EXPORTING
                  days        = '14'
                  months      = '0'
                  start_date  = *pa0019-begda
             IMPORTING
                  result_date = *pa0019-mndat.

      ENDIF.

      UPDATE pa0019 SET bvmrk = '2'
           WHERE pernr EQ employee_number
             AND termn <= datum
             AND bvmrk EQ ' '
             AND SUBTY EQ 'ZT'.

      *pa0019-TERMN = *pa0019-begda.
      *pa0019-endda = *pa0019-begda.
      *pa0019-aedtm = sy-datum.
      *pa0019-uname = sy-uname.

      insert pa0019 from *pa0019.

      IF sy-subrc EQ 0.
        COMMIT WORK.
        return-type = 'S'.
        return-message = 'Success!'.
        APPEND return.
      ELSE.
        return-type = 'S'.
        return-message = datum.
        .
        APPEND return.

      ENDIF.

    ELSE.
    ENDIF.

  ENDIF.

ENDFUNCTION.
