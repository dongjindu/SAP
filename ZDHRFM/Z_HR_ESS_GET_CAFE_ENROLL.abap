FUNCTION z_hr_ess_get_cafe_enroll.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) TYPE  BAPI7004-PERNR
*"  EXPORTING
*"     VALUE(PERNR) TYPE  BAPI7004-PERNR
*"     VALUE(CURRENT_STATUS) TYPE  PS9100-ELIGI
*"     VALUE(CURRENT_STATUS_TEXT) TYPE  CHAR40
*"  TABLES
*"      ENROLLMENT_HISTORY STRUCTURE  ZPD9100
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  DATA: lt_9100           LIKE TABLE OF p9100 WITH HEADER LINE
      , ztext             TYPE ztext
      .

* Check if a current PA9100 record exists?.
  CLEAR p9100.
  SELECT SINGLE * FROM pa9100
   WHERE pernr = employee_number
     AND ( begda <= sy-datum  AND endda >= sy-datum ).

  pernr               = employee_number.
  IF sy-subrc NE 0.
    current_status      = 'NE'.
    current_status_text = 'Not Enrolled'.
  ELSE.
    current_status      = pa9100-eligi.
    SELECT SINGLE ztext INTO current_status_text
      FROM zthr_eligi_check
     WHERE eligi = pa9100-eligi.
  ENDIF.

* HR History read
  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
*      TCLAS                 = 'A'
      pernr                 = employee_number
      infty                 = '9100'
*      BEGDA                 = '18000101'
*      ENDDA                 = '99991231'
*      BYPASS_BUFFER         = ' '
*      LEGACY_MODE           = ' '
*    IMPORTING
*      SUBRC                 =
     TABLES
       infty_tab             = lt_9100
     EXCEPTIONS
       infty_not_found       = 1
       OTHERS                = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK lt_9100[] IS NOT INITIAL.
  SORT lt_9100 BY endda DESCENDING.

  LOOP AT lt_9100.
    MOVE-CORRESPONDING lt_9100 TO enrollment_history.
    CASE lt_9100-eligi.
      WHEN 'NE'.
        enrollment_history-eligi_text = 'Not Enrolled'.
      WHEN OTHERS.
        SELECT SINGLE ztext INTO enrollment_history-eligi_text
          FROM zthr_eligi_check
         WHERE eligi = lt_9100-eligi.
    ENDCASE.
    APPEND enrollment_history.
    CLEAR: enrollment_history.
  ENDLOOP.

ENDFUNCTION.
